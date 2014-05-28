{ -----------------------------------------------------------------------------------------------------------------------
  Copyright © 2009 Zinsser Analytic GmbH - All rights reserved.
  Author       : Payman Kamali (pk)
  Description  :
  -----------------------------------------------------------------------------------------------------------------------
  Revision History:
  date     op  method                                track-no  improvement/change
  -------- --  ------------------------------------  --------- ----------------------------------------------------------
  09.06.09 pk                                        TN4585.2    Initial Revision
  18.06.09 pk                                        TN4585.3    Various changes
  06.07.09 pk  TXMLSocketStream.Create               TN4585.4    Pass buffer size to inherited create
  30.07.09 pk                                        TN4585.5    Various Changes
  24.08.09 pk  Activate                              TN4735.3    First Activatewithout route then RootNodeFromStr
  09.09.09 pk  TSimpleStringSocketStream             TN4777      new Defaultsize parameter for SocketStream.ByteBuffer
  04.02.10 pk                                        TN4972      Changes for Restart
  19.10.10 pk                                        TN5305      changes needed for CoreClient/Server
  ----------------------------------------------------------------------------------------------------------------------- }

unit XMLClientServer;


interface


uses
    RemoteClasses,
    RemoteClient,
    RemoteServer,
    RemoteFunctionClasses,
    RemoteFunctionClientServer,
    TextSocketStream,
    ScktComp,
    XMLReaderWriter,
    Streamable;

type

    TXMLSocketStream = class(TTextSocketStream)
    public
        constructor Create(aSocket: TCustomWinSocket; aTimeOut: integer; aWaitForDataTimeout: cardinal);
    end;

    TXMLRemoteFunctionStreamer = class(TRemoteFunctionStreamer)
    private
        fXMLReaderWriter: TXMLReaderWriter;
        function StreamableToStr(const aItem: TCustomStreamable): TRemoteStringType;
        function StrToStreamable<T: TCustomStreamable, constructor>(const aStr: TRemoteStringType): T;
    public
        constructor Create();
        function EnvelopeToStr(const aEnvelope: TEnvelope): TRemoteStringType; override;
        function StrToEnvelope(const aStr: TRemoteStringType): TEnvelope; override;
    end;

    TTCPIPXMLRemoteFunctionCaller = class(TRemoteFunctionCaller)
    public
        constructor Create(const aSocketStream: TWinSocketStreamExt);
    end;

    TTCPIPXMLRemoteFunctionReceiver = class(TRemoteFunctionReceiver)
    public
        constructor Create(const aSocketStream: TWinSocketStreamExt);
    end;

    TXMLMsgClient = class(TRemoteFunctionMsgClient)
    protected
        function CreateSocketStream(): TWinSocketStreamExt; override;
        function CreateRemoteFunctionCaller(): TRemoteFunctionCaller; override;
    end;

    TXMLMsgInterpreter = class(TMsgInterpreter)
    protected
        function CreateSocketStream(): TWinSocketStreamExt; override;
    end;

    TXMLMsgHandler = class(TRemoteFunctionMsgHandler)
    protected
        function CreateRemoteFunctionReceiver(): TRemoteFunctionReceiver; override;
    end;


implementation


uses
    SysUtils,
    RemoteMessageConstants;

const
    cEndStr: TTextSocketStreamStringType = '</Root>' + #$D + #$A;
    cByteBufferDefaultSize = 262144;
    cByteBufferMaxSize = cByteBufferDefaultSize * 16;

    { TXMLSocketStream }
constructor TXMLSocketStream.Create(aSocket: TCustomWinSocket; aTimeOut: integer;
    aWaitForDataTimeout: cardinal);
begin
    inherited Create(aSocket, aTimeout, aWaitForDataTimeout, cEndStr, cByteBufferSize, cByteBufferMaxSize);
end;

{ TXMLRemoteFunctionStreamer }

constructor TXMLRemoteFunctionStreamer.Create;
begin
    inherited Create();
    fXMLReaderWriter := TXMLReaderWriter.Create('', 0);
end;

function TXMLRemoteFunctionStreamer.StreamableToStr(const aItem: TCustomStreamable): TRemoteStringType;
begin
    fXMLReaderWriter.Activate();
    try
        fXMLReaderWriter.AssignObjectToRootNodeDirect(aItem);
        result := fXMLReaderWriter.RootNodeToStr();
    finally
        fXMLReaderWriter.DisActivate();
    end;
end;

function TXMLRemoteFunctionStreamer.StrToStreamable<T>(const aStr: TRemoteStringType): T;
begin
    try
        fXMLReaderWriter.ActivateWithoutRoot();
        try
            fXMLReaderWriter.RootNodeFromStr(aStr);
            result := default (T);
            result := fXMLReaderWriter.CreateObjectFromRootNode<T>();
        finally
            fXMLReaderWriter.DisActivate();
        end;

    except
        on E: Exception do
        begin
            raise Exception.Create(E.Message + ' ' + #13#10 + Copy(aStr, 1, 2000));
        end;
    end;
end;

function TXMLRemoteFunctionStreamer.EnvelopeToStr(const aEnvelope: TEnvelope): TRemoteStringType;
begin
    result := StreamableToStr(aEnvelope);
end;

function TXMLRemoteFunctionStreamer.StrToEnvelope(const aStr: TRemoteStringType): TEnvelope;
begin
    result := StrToStreamable<TEnvelope>(aStr);
    ASSERT(Assigned(result));
end;

{ TXMLMsgClient }

function TXMLMsgClient.CreateRemoteFunctionCaller(): TRemoteFunctionCaller;
begin
    result := TTCPIPXMLRemoteFunctionCaller.Create(self.Stream);
end;

function TXMLMsgClient.CreateSocketStream: TWinSocketStreamExt;
begin
    result := TXMLSocketStream.Create(self.Socket, 5000, INT_CLIENT_WAITFORDATA_TIMEOUT_MSECS);
end;

{ TXMLMsgInterpreter }

function TXMLMsgInterpreter.CreateSocketStream: TWinSocketStreamExt;
begin
    result := TXMLSocketStream.Create(fSocket, 2000, INT_SERVER_WAITFORDATA_TIMEOUT_MSECS);
    // result :=  TWinSocketByteStream.Create( fSocket, 2000, INT_SERVER_WAITFORDATA_TIMEOUT_MSECS   );
end;

function TXMLMsgHandler.CreateRemoteFunctionReceiver(): TRemoteFunctionReceiver;
begin
    result := TTCPIPXMLRemoteFunctionReceiver.Create(fInterpreter.Stream);
end;

{ TTCPIPXMLRemoteFunctionCaller }

constructor TTCPIPXMLRemoteFunctionCaller.Create(const aSocketStream: TWinSocketStreamExt);
begin
    inherited Create(TTCPIPRemoteFunctionTransporter.Create(aSocketStream,
        TXMLRemoteFunctionStreamer.Create()));
end;

{ TTCPIPXMLRemoteFunctionReceiver }

constructor TTCPIPXMLRemoteFunctionReceiver.Create(const aSocketStream: TWinSocketStreamExt);
begin
    inherited Create(TTCPIPRemoteFunctionTransporter.Create(aSocketStream,
        TXMLRemoteFunctionStreamer.Create()));
end;


end.
