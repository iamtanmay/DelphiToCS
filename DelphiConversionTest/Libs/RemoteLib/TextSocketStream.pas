unit TextSocketStream;


{ -----------------------------------------------------------------------------------------------------------------------
  Copyright © 2009 Zinsser Analytic GmbH - All rights reserved.
  Author       : Payman Kamali (pk)
  Description  :
  -----------------------------------------------------------------------------------------------------------------------
  Revision History:
  date     op  method                                track-no  improvement/change
  -------- --  ------------------------------------  --------- ----------------------------------------------------------
  18.06.09 pk                                        TN4585.3  Initial Revision
  06.07.09 pk  ReadBytes                             TN4585.4  Was too slow, now optimized using Sock.ReceiveLength
  09.09.09 pk  ReadBytes                             TN4777    calls Buffer.GrowToSize to ensure enough memory in buffer
  21.09.09 pk  DoRead                                TN4777    New
  23.09.09 pk                                        TN4793    TWinSocketStreamExt.BufferToStr/BufferFromStr, and fStringType moved to Bytebuffer
  19.10.10 pk                                        TN5305    changes needed for CoreClient/Server
  19.02.12 pk                                        TN5809     Bug fixes to CoreServer classes for V8
  21.12.12 ts                                        TN6059    Anpassungen, um CoreClient und ZAProxy parallel laufen zu lassen (BayerSpritzstraﬂe)
  ----------------------------------------------------------------------------------------------------------------------- }
interface


uses
    RemoteClasses,
    ScktComp;

type

    TTextSocketStreamStringType = string;

    TTextSocketStream = class(TWinSocketStreamExt)
    private
        function DoReadBytes(const aPos: integer; const aMaxBytes: integer): integer;
        function DetermineReceiveLength(): integer;
    protected
        fEndStr: TTextSocketStreamStringType;
        fWaitForDataTimeoutInMSecs: cardinal;
        fSock: TCustomWinSocket;
        function ReadBytes(const aMaxBytes: integer; out oNumBytesRead: integer): TReadSockResponse;
        function ReadSock: TReadSockResponse;
        function IsMessageComplete(const aExpectedLength: integer): boolean;
        procedure WriteSock(aCount: integer);
    public
        constructor Create(aSocket: TCustomWinSocket; aTimeOut: integer; aWaitForDataTimeout: cardinal;
            const aEndStr: string; const aBufferDefaultSize, aBufferMaxSize: integer);
        destructor Destroy(); override;
        function ReceiveFirstMessageToBuffer(out oType: integer; out oName: string)
            : TReadSockResponse; override;
        function ReceiveToBuffer(): TReadSockResponse; override;
        procedure SendFromBuffer(); override;

        procedure SendResponseFromBuffer(aResponseType: TResponseType); override;
        function ReceiveResponseFromBuffer(): TResponseType; override;
        procedure SendQuit(); override;

        function IsQuitReceived(): boolean; override;
        procedure Close(); override;
    end;


implementation


uses
    SysUtils;

{ TTextSocketStream }
constructor TTextSocketStream.Create(aSocket: TCustomWinSocket; aTimeOut: integer;
    aWaitForDataTimeout: cardinal; const aEndStr: string; const aBufferDefaultSize, aBufferMaxSize: integer);
begin
    inherited Create(aSocket, aTimeout, aBufferDefaultSize, aBufferMaxSize);
    fSock := aSocket;

    fWaitForDataTimeoutInMSecs := aWaitForDataTimeout;
    fEndStr := aEndStr;
end;

destructor TTextSocketStream.Destroy();
begin
    inherited;
end;

function TTextSocketStream.IsMessageComplete(const aExpectedLength: integer): boolean;
var
    xStr: TTextSocketStreamStringType;
    xStartByteIndex: integer;
begin
    xStartByteIndex := SizeOf(fBuffer.Position) + (aExpectedLength) - self.Buffer.StringLengthToByteLength
        (Length(fEndStr));
    self.Buffer.CopyPartToStr(xStr, xStartByteIndex, Length(fEndStr));
    result := WideSameText(xStr, fEndStr);
end;

function TTextSocketStream.DoReadBytes(const aPos: integer; const aMaxBytes: integer): integer;
var
    xReceiveLen: integer;
    xNumBytesToRead: integer;
begin
    result := 0;
    try
        xReceiveLen := fSock.ReceiveLength;
        if xReceiveLen = 0 then
        begin
            // it is possible for WaitForData to return true but for receivelength to return 0 when the connection
            // has been closed by the server
            // gmRemoteWriteLog( 'Receive Length = 0' );
            EXIT;
        end;

        xNumBytesToRead := aMaxBytes;
        if xReceiveLen < aMaxBytes then
        begin
            xNumBytesToRead := xReceiveLen;
        end;

        fBuffer.GrowToSize(aPos + xNumBytesToRead);
        result := read(fBuffer.Buffer[aPos], xNumBytesToRead);

        if result = 0 then
        begin
            gmRemoteWriteLog('No data available');
            EXIT;
        end;

        fBuffer.Seek(spContinue, result);
    except
        on e: exception do
            raise Exception.Create('Could not read: ' + e.Message);
    end;
end;

function TTextSocketStream.DetermineReceiveLength(): integer;
begin
    result := 0;
    fBuffer.Seek(spBegin);
    if not WaitForData(0) then
        EXIT;
    if not(DoReadBytes(fBuffer.Position, SizeOf(fBuffer.Position)) > 0) then
        EXIT;
    fBuffer.Seek(spBegin);
    result := fBuffer.ReadInt;
end;

function TTextSocketStream.ReadBytes(const aMaxBytes: integer; out oNumBytesRead: integer): TReadSockResponse;
var
    xCurrentNumBytesRead, xExpectedRemainingNumBytes, xReceiveLen: integer;
    xIsMessageComplete: boolean;
    i: integer;
begin
    if not WaitForData(fWaitForDataTimeoutInMSecs) then
    begin
        result := rsrTimeout;
        EXIT;
    end;

    fBuffer.Seek(spBegin, 0);

    xReceiveLen := DetermineReceiveLength();
    if (xReceiveLen = 0) then
    begin
        result := RemoteClasses.TReadSockResponse.rsrTimeout;
        EXIT;
    end;

    result := RemoteClasses.TReadSockResponse.rsrOK;

    xExpectedRemainingNumBytes := xReceiveLen;

    oNumBytesRead := 0;
    while true do
    begin
        if not fSock.Connected then
        begin
            gmRemoteWriteLog('Read: connection was closed by the client');
            EXIT;
        end;

        xCurrentNumBytesRead := DoReadBytes(fBuffer.Position, xExpectedRemainingNumBytes);

        oNumBytesRead := oNumBytesRead + xCurrentNumBytesRead;
        xExpectedRemainingNumBytes := xExpectedRemainingNumBytes - xCurrentNumBytesRead;

        if not WaitForData(0) then
        begin
            if (xExpectedRemainingNumBytes = 0) then
            begin
                xIsMessageComplete := IsMessageComplete(xReceiveLen);
                if xIsMessageComplete then
                    EXIT;
            end;
            if fBuffer.Buffer[fBuffer.Position - 1] = Byte(#3) then  //EndStr
                EXIT;
            if fBuffer.Position = 4 then
            begin
                for i := 0 to 3 do
                begin
                    if fBuffer.Buffer[i] = Byte(#3) then  //EndStr
                        EXIT;
                end;
            end;
            if not WaitForData(fWaitForDataTimeoutInMSecs) then
            begin
                if oNumBytesRead = 0 then
                begin
                    gmRemoteWriteLog('Timeout waiting for data');
                    EXIT;
                end;
                raise Exception.Create('Read: data was only partly read.  No end string found.');
            end;
        end;

        if xCurrentNumBytesRead = 0 then
        begin
            raise Exception.Create(Format('Read: %d bytes of data were excepected but no data could be read',
                [xExpectedRemainingNumBytes]));
        end;

        if (xExpectedRemainingNumBytes = 0) then
        begin
            xIsMessageComplete := IsMessageComplete(xReceiveLen);
            if (xIsMessageComplete) then
                EXIT;
        end;

        if (xExpectedRemainingNumBytes <= 0) then
        begin
            raise Exception.Create('Read: data was only partly read.  No end string found.');
        end;

        if (aMaxBytes >= 0) and (oNumBytesRead = aMaxBytes) then
        begin
            EXIT;
        end;
    end;

end;

function TTextSocketStream.ReadSock(): TReadSockResponse;
begin
    result := ReadBytes(-1, fNumBytesRead);
end;

procedure TTextSocketStream.Close;
begin
    fSock.Close();
end;

function TTextSocketStream.IsQuitReceived: boolean;
begin
    result := false;
end;

function TTextSocketStream.ReceiveFirstMessageToBuffer(out oType: integer; out oName: string)
    : TReadSockResponse;
begin
    oType := 0;
    oName := '';
    result := rsrOK;
end;

function TTextSocketStream.ReceiveResponseFromBuffer: TResponseType;
var
    xReadResponse: TReadSockResponse;
begin
    result := rtNack;
    xReadResponse := ReceiveToBuffer();
    if xReadResponse <> rsrOK then
        EXIT;
    result := rtAck;
end;

function TTextSocketStream.ReceiveToBuffer: TReadSockResponse;
begin
    result := ReadSock();
end;

procedure TTextSocketStream.WriteSock(aCount: integer);
begin
    // gmRemoteWriteLog( Format( '%s:%d - Number of bytes written %d', [ self.fSock.RemoteHost, self.fSock.RemotePort, aCount ] ) );
    self.Write(fBuffer.Buffer[0], aCount);
end;

procedure TTextSocketStream.SendFromBuffer;
begin
    WriteSock(fBuffer.Position);
    FillChar(fBuffer.Buffer[0], Length(fBuffer.Buffer) * sizeof(fBuffer.Buffer[0]), 0);
end;

procedure TTextSocketStream.SendQuit;
begin
end;

procedure TTextSocketStream.SendResponseFromBuffer(aResponseType: TResponseType);
begin
    SendFromBuffer();
end;


end.
