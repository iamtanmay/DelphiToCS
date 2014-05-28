{ --------------------------------------------------------------------------------------------------
  Copyright © 2007 Zinsser Analytic GmbH - All rights reserved.
  Author       : Payman Kamali (pk)
  Description  : General Classes used in TCP\IP communication
  --------------------------------------------------------------------------------------------------
  Revision History:
  date     op  method                       track-no improvement/change
  -------- --  ---------------------------  -------- -----------------------------------------------
  31.05.07 pk                               TN3713   initial revision
  11.01.08 pk TWinSocketStreamExt           TN3797   Various changes to make more robust
  09.06.09 pk                               TN4585.2 various changes
  18.06.09 pk                               TN4585.3 various changes
  23.06.09 pk TWinSocketStreamExt           TN4620   New fStringType
  09.09.09 pk TByteBuffer                   TN4777   Is now able to grow in size via function GrowToSize
  21.09.09 pk TWinSocketStreamExt           TN4777   New CreateBuffer
  23.09.09 pk TWinSocketStreamExt           TN4793   BufferToStr/BufferFromStr, and fStringType moved to Bytebuffer
  19.10.10 pk                               TN5305   changes needed for CoreClient/Server
  21.12.12 ts                               TN6059   Anpassungen, um CoreClient und ZAProxy parallel laufen zu lassen (BayerSpritzstraße)
  -------------------------------------------------------------------------------------------------- }

unit RemoteClasses;


interface


uses
    Windows,
    Messages,
    SysUtils,
    Classes,
    scktcomp,
    GeneralTypes;

const
    INT_NETMSG_NACK = -1;
    INT_NETMSG_ACK = 1;

    INT_NETMSGID_QUIT = 0;

    INT_WAITFORDATA_TIMEOUT_MSECS = 3000;
    INT_READBYTES_WAITFORDATA_TIMEOUT_MSECS = 1000;

    cByteBufferSize = 32768;

type

    TRemoteWriteLogFunc = procedure(const aLogText: string);

    TByteArray = array of byte;
    TSeekBufferType = (spContinue, spBegin, spBeginParams, spBeginRespond);
    TResponseType = (rtAck, rtNack);

    TWinSocketStreamStringType = (sstDefault, sstAnsi);

    TByteBuffer = class
    private
        fBuffer: TByteArray;
        fDefaultSize: integer;
        fMaxSize: integer;
        fPosition: integer;
        fStringType: TWinSocketStreamStringType;
        procedure Resize(const aSize: integer);
        procedure ResizeDefault;
        procedure ResizeToRequiredSizeWhenAddingSize(const aAddedSize: integer);
    public
        constructor Create(const aDefaultSize, aMaxSize: integer);
        procedure Seek(aSeekType: TSeekBufferType; aOffset: integer = 0);
        function ReadLength(): integer;
        function ReadChar(): char;
        function ReadInt(): integer;
        function ReadBool(): boolean;
        function ReadFloat: double;
        function ReadStr(): string;
        function ReadIntArray: TIntArray;
        procedure WriteLength(const aValue: integer);
        procedure WriteByte(const aValue: byte);
        procedure WriteChar(const aValue: char);
        procedure WriteInt(const aValue: integer);
        procedure WriteBool(const aValue: boolean);
        procedure WriteStr(const aValue: string);
        procedure WriteFloat(const aValue: double);
        procedure WriteIntArray(const aValue: TIntArray);
        procedure GrowToSize(const aRequiredSize: integer);
        function StringLengthToByteLength(const aStringLength: integer): integer;
        function ByteLengthToStringLength(const aByteLength: integer): integer;
        function CopyPartFromStr(const aValue: string; const aStartByteIndex: integer): integer; virtual;
        procedure CopyPartToStr(out oValue: string; const aStartByteIndex: integer;
            const aStringLength: integer); virtual;
        procedure CopyFromStr(const aValue: string); overload;
        procedure CopyToStr(out oValue: string); overload;
        property Buffer: TByteArray read fBuffer write fBuffer;
        property Position: integer read fPosition;
        property StringType: TWinSocketStreamStringType read fStringType write fStringType;
    end;

    TReadSockResponse = (rsrOK, rsrTimeout, rsrError);

    TWinSocketStreamExt = class(TWinSocketStream)
    protected
        fBuffer: TByteBuffer;
        fNumBytesRead: integer;

        function CreateBuffer(const aDefaultSize, aMaxSize: integer): TByteBuffer; virtual;
    public
        constructor Create(aSocket: TCustomWinSocket; aTimeOut: Longint;
            const aBufferDefaultSize, aBufferMaxSize: integer);
        destructor Destroy(); override;

        function ReceiveFirstMessageToBuffer(out oType: integer; out oName: string): TReadSockResponse;
            virtual; abstract;
        function ReceiveToBuffer(): TReadSockResponse; virtual; abstract;
        procedure SendFromBuffer(); virtual; abstract;

        procedure SendResponseFromBuffer(aResponseType: TResponseType); virtual; abstract;
        function ReceiveResponseFromBuffer(): TResponseType; virtual; abstract;
        procedure SendQuit(); virtual; abstract;

        function IsQuitReceived(): boolean; virtual; abstract;
        procedure Close(); virtual; abstract;
        property Buffer: TByteBuffer read fBuffer;
    end;

    TWinSocketByteStream = class(TWinSocketStreamExt)
    private
        fSock: TCustomWinSocket;
        fWaitForDataTimeoutInMSecs: cardinal;
        function ReadSock(): TReadSockResponse;
        procedure WriteSock(aCount: integer);
        function ReadBytes(const aMaxBytes: integer): integer;
    public
        constructor Create(aSocket: TCustomWinSocket; aTimeOut: integer; aWaitForDataTimeout: cardinal);
        function ReceiveFirstMessageToBuffer(out oType: integer; out oName: string)
            : TReadSockResponse; override;
        function ReceiveToBuffer(): TReadSockResponse; override;
        procedure SendFromBuffer(); override;

        procedure SendResponseFromBuffer(aResponseType: TResponseType); override;
        function ReceiveResponseFromBuffer(): TResponseType; override;
        procedure SendQuit(); override;

        function IsQuitReceived(): boolean; override;
        procedure Close(); override;
        class function IsQuitMsgID(aMsgID: integer): boolean;

    end;

procedure gmSetRemoteWriteLogFunc(aFunc: TRemoteWriteLogFunc);
procedure gmRemoteWriteLog(const aLogText: string);


implementation


var
    uRemoteWriteLogFunc: TRemoteWriteLogFunc;

procedure gmSetRemoteWriteLogFunc(aFunc: TRemoteWriteLogFunc);
begin
    uRemoteWriteLogFunc := aFunc;
end;

procedure gmRemoteWriteLog(const aLogText: string);
begin
    if Assigned(uRemoteWriteLogFunc) then
        uRemoteWriteLogFunc(aLogText);
end;

constructor TWinSocketStreamExt.Create(aSocket: TCustomWinSocket; aTimeOut: Longint;
    const aBufferDefaultSize, aBufferMaxSize: integer);
begin
    inherited Create(aSocket, aTimeOut);
    fBuffer := CreateBuffer(aBufferDefaultSize, aBufferMaxSize);
end;

destructor TWinSocketStreamExt.Destroy();
begin
    fBuffer.Free;
    inherited;
end;

function TWinSocketStreamExt.CreateBuffer(const aDefaultSize, aMaxSize: integer): TByteBuffer;
begin
    result := TByteBuffer.Create(aDefaultSize, aMaxSize);
end;

constructor TWinSocketByteStream.Create(aSocket: TCustomWinSocket; aTimeOut: integer;
    aWaitForDataTimeout: cardinal);
begin
    inherited Create(aSocket, aTimeout, cByteBufferSize, cByteBufferSize);
    fSock := aSocket;

    fWaitForDataTimeoutInMSecs := aWaitForDataTimeout;
end;

function TWinSocketByteStream.ReadBytes(const aMaxBytes: integer): integer;
var
    xNextByte: byte;
    i: integer;
begin
    result := -1;
    i := 0;
    while true do
    begin
        xNextByte := 0;
        if not WaitForData(INT_READBYTES_WAITFORDATA_TIMEOUT_MSECS) then
        begin
            gmRemoteWriteLog('Server Timeout waiting for client data');
            EXIT;
        end;

        if not fSock.Connected then
        begin
            gmRemoteWriteLog('Read: connection was closed by the client');
            EXIT;
        end;

        try
            if read(xNextByte, SizeOf(xNextByte)) = 0 then
                EXIT;
        except
            on e: exception do
                raise Exception.Create('Could not read: ' + e.Message);
        end;

        fBuffer.WriteByte(xNextByte);
        Inc(i);
        if (aMaxBytes >= 0) and ((i) = aMaxBytes) then
            BREAK;

    end;
    result := i;
    if (aMaxBytes >= 0) and ((result) <> aMaxBytes) then
        raise Exception.CreateFmt('ReadBytes: Expected bytes[%d], bytes read [%d]', [aMaxBytes, i]);
end;

function TWinSocketByteStream.ReadSock(): TReadSockResponse;
var
    xNumExpectedBytes: integer;
begin
    fNumBytesRead := 0;

    if not WaitForData(fWaitForDataTimeoutInMSecs) then
    begin
        gmRemoteWriteLog('Timeout waiting for data');
        result := rsrTimeout;
        EXIT;
    end;

    fBuffer.Seek(spBegin, 0);

    result := rsrError;

    // read length byte
    fNumBytesRead := ReadBytes(SizeOf(integer));
    if fNumBytesRead = -1 then
    begin
        gmRemoteWriteLog('Message Length could not be read');
        EXIT;
    end;
    xNumExpectedBytes := fBuffer.ReadInt();

    // read data bytes
    fNumBytesRead := ReadBytes(xNumExpectedBytes - fNumBytesRead);
    if fNumBytesRead = -1 then
    begin
        gmRemoteWriteLog('Message Data could not be read');
        EXIT;
    end;

    result := rsrOK;
end;

function TWinSocketByteStream.ReceiveFirstMessageToBuffer(out oType: integer; out oName: string)
    : TReadSockResponse;
begin
    result := self.ReceiveToBuffer();
    if result = rsrOK then
    begin
        oType := self.Buffer.ReadInt();
        oName := self.Buffer.ReadStr();
    end;
end;

function TWinSocketByteStream.ReceiveToBuffer(): TReadSockResponse;
begin
    result := ReadSock();
end;

function TWinSocketByteStream.ReceiveResponseFromBuffer(): TResponseType;
var
    xAnswer: integer;
    xReadResponse: TReadSockResponse;
begin
    result := rtNack;
    xReadResponse := ReceiveToBuffer();
    if xReadResponse <> rsrOK then
        EXIT;
    fBuffer.Seek(spBeginParams, 0);
    xAnswer := fBuffer.ReadInt();
    if xAnswer = INT_NETMSG_ACK then
        result := rtAck;

end;

procedure TWinSocketByteStream.WriteSock(aCount: integer);
begin
    self.Write(fBuffer.Buffer[0], aCount);
end;

procedure TWinSocketByteStream.SendFromBuffer();
var
    xLength: integer;
begin
    xLength := fBuffer.Position;
    fBuffer.WriteLength(xLength);
    WriteSock(xLength);
    FillChar(fBuffer.Buffer[0], Length(fBuffer.Buffer) * sizeof(fBuffer.Buffer[0]), 0);
end;

procedure TWinSocketByteStream.SendResponseFromBuffer(aResponseType: TResponseType);
var
    xSavePosition: integer;
    xAnswer: integer;
begin
    if aResponseType = rtAck then
        xAnswer := INT_NETMSG_ACK
    else
        xAnswer := INT_NETMSG_NACK;

    xSavePosition := fBuffer.Position;
    fBuffer.Seek(spBeginParams, 0);
    fBuffer.WriteInt(xAnswer);

    fBuffer.Seek(spBegin, xSavePosition);
    SendFromBuffer();

end;

class function TWinSocketByteStream.IsQuitMsgID(aMsgID: integer): boolean;
begin
    result := (aMsgID = INT_NETMSGID_QUIT);
end;

function TWinSocketByteStream.IsQuitReceived(): boolean;
var
    xSavePosition: integer;
begin
    xSavePosition := fBuffer.Position;
    fBuffer.Seek(spBeginParams, 0);
    result := (fBuffer.ReadInt() = INT_NETMSGID_QUIT);
    fBuffer.Seek(spBegin, xSavePosition);
end;

procedure TWinSocketByteStream.SendQuit();
begin
    fBuffer.Seek(spBeginParams, 0);
    fBuffer.WriteInt(INT_NETMSGID_QUIT);
    SendFromBuffer();
end;

procedure TWinSocketByteStream.Close;
begin
    fSock.Close();
end;

{ TByteBuffer }

constructor TByteBuffer.Create(const aDefaultSize, aMaxSize: integer);
begin
    inherited Create();
    fDefaultSize := aDefaultSize;
    fMaxSize := aMaxSize;
    fStringType := sstDefault;
    ResizeDefault();
end;

function TByteBuffer.StringLengthToByteLength(const aStringLength: integer): integer;
begin
    if fStringType = sstDefault then
        result := aStringLength * SizeOf(char)
    else
        result := aStringLength * SizeOf(ansichar);
end;

function TByteBuffer.ByteLengthToStringLength(const aByteLength: integer): integer;
begin
    if fStringType = sstDefault then
        result := aByteLength div SizeOf(char)
    else
        result := aByteLength div SizeOf(ansichar);
end;

function TByteBuffer.CopyPartFromStr(const aValue: string; const aStartByteIndex: integer): integer;
var
    xNumBytes: integer;
    xLen: integer;
    xAnsiString: ansistring;
begin
    xLen := Length(aValue);

    xNumBytes := self.StringLengthToByteLength(xLen);
    result := xNumBytes;

    GrowToSize(fPosition + xNumBytes);
    ASSERT(aStartByteIndex >= 0);
    if fStringType = sstDefault then
        Move(aValue[1], fBuffer[aStartByteIndex], xNumBytes)
    else
    begin
        xAnsiString := AnsiString(aValue);
        Move(xAnsiString[1], fBuffer[aStartByteIndex], xNumBytes)
    end;

    Seek(spBegin, xNumBytes);
end;

procedure TByteBuffer.CopyPartToStr(out oValue: string; const aStartByteIndex: integer;
    const aStringLength: integer);
var
    xLen: integer;
    xValue: string;
    xAnsiValue: ansiString;
begin
    xLen := aStringLength;

    if fStringType = sstDefault then
    begin
        SetLength(xValue, xLen);
        ASSERT(aStartByteIndex >= 0);
        Move(fBuffer[aStartByteIndex], xValue[1], StringLengthToByteLength(xLen));
        oValue := xValue;
    end
    else
    begin
        SetLength(xAnsiValue, xLen);
        ASSERT(aStartByteIndex >= 0);
        Move(fBuffer[aStartByteIndex], xAnsiValue[1], StringLengthToByteLength(xLen));
        oValue := string(xAnsiValue);
    end;

end;

procedure TByteBuffer.CopyToStr(out oValue: string);
var
    xLength: integer;
begin
    if fStringType = sstDefault then
    begin
        Seek(spBegin);
        xLength := ReadInt();
        xLength := ByteLengthToStringLength(xLength);
        CopyPartToStr(oValue, fPosition, xLength);
    end
    else
        CopyPartToStr(oValue, 0, fPosition);
end;

procedure TByteBuffer.CopyFromStr(const aValue: string);
var
    xLength: integer;
begin
    if fStringType = sstDefault then
    begin
        // leave space for length
        Seek(spBegin);
        self.WriteInt(0);

        // write string to buffer
        xLength := CopyPartFromStr(aValue, fPosition);

        // write length at begin
        Seek(spBegin);
        self.WriteInt(xLength);

        // move cursor to end
        Seek(spContinue, xLength);
    end
    else
        CopyPartFromStr(aValue, 0);
end;

procedure TByteBuffer.Resize(const aSize: integer);
begin
    if Length(fBuffer) = aSize then
        EXIT;

    ASSERT(aSize <= fMaxSize, Format('ByteBuffer new size %d would exceed maximum size %d',
        [aSize, fMaxSize]));
    SetLength(fBuffer, aSize);
end;

procedure TByteBuffer.GrowToSize(const aRequiredSize: integer);
begin
    if (aRequiredSize) > Length(fBuffer) then
        Resize(aRequiredSize);
end;

procedure TByteBuffer.ResizeToRequiredSizeWhenAddingSize(const aAddedSize: integer);
var
    xRequiredSize: integer;
begin
    xRequiredSize := fPosition + aAddedSize;
    GrowToSize(xRequiredSize);
end;

procedure TByteBuffer.ResizeDefault();
begin
    Resize(fDefaultSize);
end;

function TByteBuffer.ReadLength(): integer;
begin
    Move(fBuffer[0], result, SizeOf(fPosition));
    Seek(spContinue, SizeOf(fPosition));
end;

function TByteBuffer.ReadChar: char;
begin
    Move(fBuffer[fPosition], result, SizeOf(result));
    Seek(spContinue, SizeOf(result));
end;

function TByteBuffer.ReadInt: integer;
begin
    Move(fBuffer[fPosition], result, SizeOf(result));
    Seek(spContinue, SizeOf(result));
end;

function TByteBuffer.ReadBool: boolean;
begin
    Move(fBuffer[fPosition], result, SizeOf(result));
    Seek(spContinue, SizeOf(result));
end;

function TByteBuffer.ReadFloat: double;
begin
    Move(fBuffer[fPosition], result, SizeOf(result));
    Seek(spContinue, SizeOf(result));
end;

function TByteBuffer.ReadStr: string;
var
    xLen: integer;
    xStr: string;
begin
    xLen := ReadInt;
    SetLength(xStr, xLen);
    Move(fBuffer[fPosition], xStr[1], Length(xStr));
    Seek(spContinue, Length(xStr));
    result := xStr;
end;

procedure TByteBuffer.Seek(aSeekType: TSeekBufferType; aOffset: integer = 0);
begin
    if aSeekType = spContinue then
        fPosition := fPosition + aOffset
    else if aSeekType = spBegin then
        fPosition := aOffset
    else if aSeekType = spBeginParams then
        fPosition := SizeOf(fPosition) + aOffset
    else if aSeekType = spBeginRespond then
        fPosition := SizeOf(fPosition) + SizeOf(integer) + aOffset;
end;

procedure TByteBuffer.WriteByte(const aValue: byte);
begin
    ResizeToRequiredSizeWhenAddingSize(SizeOf(aValue));
    Move(aValue, fBuffer[fPosition], SizeOf(aValue));
    Seek(spContinue, SizeOf(aValue));
end;

procedure TByteBuffer.WriteChar(const aValue: char);
begin
    ResizeToRequiredSizeWhenAddingSize(SizeOf(aValue));
    Move(aValue, fBuffer[fPosition], SizeOf(aValue));
    Seek(spContinue, SizeOf(aValue));
end;

procedure TByteBuffer.WriteInt(const aValue: integer);
begin
    ResizeToRequiredSizeWhenAddingSize(SizeOf(aValue));
    Move(aValue, fBuffer[fPosition], SizeOf(aValue));
    Seek(spContinue, SizeOf(aValue));
end;

procedure TByteBuffer.WriteBool(const aValue: boolean);
begin
    ResizeToRequiredSizeWhenAddingSize(SizeOf(aValue));
    Move(aValue, fBuffer[fPosition], SizeOf(aValue));
    Seek(spContinue, SizeOf(aValue));
end;

procedure TByteBuffer.WriteStr(const aValue: string);
begin
    WriteInt(Length(aValue));
    ResizeToRequiredSizeWhenAddingSize(SizeOf(aValue));
    Move(aValue[1], fBuffer[fPosition], Length(aValue));
    Seek(spContinue, Length(aValue));
end;

procedure TByteBuffer.WriteFloat(const aValue: double);
begin
    ResizeToRequiredSizeWhenAddingSize(SizeOf(aValue));
    Move(aValue, fBuffer[fPosition], SizeOf(aValue));
    Seek(spContinue, SizeOf(aValue));
end;

procedure TByteBuffer.WriteLength(const aValue: integer);
begin
    ResizeToRequiredSizeWhenAddingSize(SizeOf(aValue));
    Move(aValue, fBuffer[0], SizeOf(aValue));
end;

function TByteBuffer.ReadIntArray: TIntArray;
var
    xLen: integer;
    x: integer;
    xArray: TIntArray;
begin
    xLen := ReadInt();
    SetLength(xArray, xLen);
    for x := 0 to xLen - 1 do
    begin
        xArray[x] := ReadInt();
    end;
    result := xArray;
end;

procedure TByteBuffer.WriteIntArray(const aValue: TIntArray);
var
    x: integer;
    xLen: integer;
begin
    xLen := Length(aValue);
    WriteInt(xLen);
    for x := 0 to xLen - 1 do
        WriteInt(aValue[x]);
end;


end.
