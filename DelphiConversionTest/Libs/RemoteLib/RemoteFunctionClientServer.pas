{ -----------------------------------------------------------------------------------------------------------------------
  Copyright © 2009 Zinsser Analytic GmbH - All rights reserved.
  Author       : Payman Kamali (pk)
  Description  :
  -----------------------------------------------------------------------------------------------------------------------
  Revision History:
  date     op  method                                track-no  improvement/change
  -------- --  ------------------------------------  --------- ----------------------------------------------------------
  18.06.09 pk                                        TN4585.3  Initial Revision
  13.07.09 pk  TRemoteFunctionMsgHandler             TN4585.4  New: DoOnHandleCall
  23.09.09 pk                                        TN4793    TWinSocketStreamExt.BufferToStr/BufferFromStr, and fStringType moved to Bytebuffer
  19.10.10 pk                                        TN5305    changes needed for CoreClient/Server
  ----------------------------------------------------------------------------------------------------------------------- }

unit RemoteFunctionClientServer;


interface


uses
    RemoteClasses,
    RemoteClient,
    RemoteServer,
    RemoteFunctionClasses;

type
    TTCPIPRemoteFunctionTransporter = class(TRemoteFunctionTransporter)
    private
        procedure LoadEnvelopeToBuffer(const aEnvelope: TEnvelope);
        function UnLoadEnvelopeFromBuffer: TEnvelope;
    protected
        fSocketStream: TWinSocketStreamExt;
        fRemoteFunctionStreamer: TRemoteFunctionStreamer;
    public
        constructor Create(const aSocketStream: TWinSocketStreamExt;
            const aRemoteFunctionStreamer: TRemoteFunctionStreamer);
        procedure SendCall(const aEnvelope: TEnvelope); override;
        procedure SendResult(const aEnvelope: TEnvelope); override;
        function ReceiveCall(): TEnvelope; override;
        function ReceiveResult(): TEnvelope; override;
        function Wait(): boolean; override;
    end;

    TRemoteFunctionMsgClient = class(TMsgClient)
    private
        fOnWriteLog: TOnRemoteWriteLogCallback;
        procedure SetOnWriteLog(const aValue: TOnRemoteWriteLogCallback);
    protected
        fRemoteFunctionCaller: TRemoteFunctionCaller;
        procedure SendQuit; override;
        function CreateRemoteFunctionCaller(): TRemoteFunctionCaller; virtual; abstract;
    public
        constructor Create(const aHost: string; aPort: integer);
        destructor Destroy(); override;
        property RemoteFunctionCaller: TRemoteFunctionCaller read fRemoteFunctionCaller;
        property OnWriteLog: TOnRemoteWriteLogCallback read fOnWriteLog write SetOnWriteLog;
    end;

    TRemoteFunctionMsgHandler = class(TMsgHandler)
    private
        fOnWriteLog: TOnRemoteWriteLogCallback;
        procedure SetOnWriteLog(const aValue: TOnRemoteWriteLogCallback);
        procedure DoOnHandleCall(const aSender: TObject);
    protected
        fErrorNumber: integer;
        fRemoteFunctionReceiver: TRemoteFunctionReceiver;
        procedure HandleNext(); override;
        function CreateRemoteFunctionReceiver(): TRemoteFunctionReceiver; virtual; abstract;
        procedure DoHandleCall(); virtual;
        procedure DoHandleCloseConnectionCall(); virtual;
        function GetClientID(): string;
    public
        constructor Create(aInterpreter: TMsgInterpreter);
        destructor Destroy(); override;
        property ClientID: string read GetClientID;
        property OnWriteLog: TOnRemoteWriteLogCallback read fOnWriteLog write SetOnWriteLog;
    end;


implementation


uses
    SysUtils,
    RemoteMessageConstants;

{ TRemoteFunctionMsgClient }
constructor TRemoteFunctionMsgClient.Create(const aHost: string; aPort: integer);
begin
    inherited Create(aHost, aPort);
    fRemoteFunctionCaller := CreateRemoteFunctionCaller();
end;

destructor TRemoteFunctionMsgClient.Destroy;
begin
    fRemoteFunctionCaller.Free;
    inherited;
end;

procedure TRemoteFunctionMsgClient.SendQuit();
begin
    fRemoteFunctionCaller.CallBoolFunc(cCoreCallIDCloseConnection);
end;

procedure TRemoteFunctionMsgClient.SetOnWriteLog(const aValue: TOnRemoteWriteLogCallback);
begin
    fOnWriteLog := aValue;
    if Assigned(fRemoteFunctionCaller) then
        fRemoteFunctionCaller.OnWriteLog := fOnWriteLog;
end;
{ TRemoteFunctionMsgHandler }

constructor TRemoteFunctionMsgHandler.Create(aInterpreter: TMsgInterpreter);
begin
    inherited Create(aInterpreter);
    fErrorNumber := 0;
    fRemoteFunctionReceiver := CreateRemoteFunctionReceiver();
    fRemoteFunctionReceiver.HandleCallCallback := self.DoOnHandleCall;
end;

destructor TRemoteFunctionMsgHandler.Destroy;
begin
    fRemoteFunctionReceiver.Free;
    inherited;
end;

procedure TRemoteFunctionMsgHandler.DoHandleCloseConnectionCall;
begin

end;

procedure TRemoteFunctionMsgHandler.DoOnHandleCall(const aSender: TObject);
begin
    DoHandleCall();
end;

procedure TRemoteFunctionMsgHandler.DoHandleCall();
begin
    if fRemoteFunctionReceiver.CallNameMatches(cCoreCallIDCheckConnection) then
    begin
        fRemoteFunctionReceiver.ResultParams.Add(true);
    end
    else if fRemoteFunctionReceiver.CallNameMatches(cCoreCallIDCloseConnection) then
    begin
        fQuit := true;
        DoHandleCloseConnectionCall();
        fRemoteFunctionReceiver.ResultParams.Add(true);
    end
end;

procedure TRemoteFunctionMsgHandler.HandleNext;
begin
    fRemoteFunctionReceiver.HandleCall();
end;

procedure TRemoteFunctionMsgHandler.SetOnWriteLog(const aValue: TOnRemoteWriteLogCallback);
begin
    fOnWriteLog := aValue;
    if Assigned(fRemoteFunctionReceiver) then
        fRemoteFunctionReceiver.OnWriteLog := fOnWriteLog;
end;

function TRemoteFunctionMsgHandler.GetClientID: string;
begin
    result := Format('%s:%d', [fInterpreter.Socket.RemoteAddress, fInterpreter.Socket.RemotePort]);
end;

{ TTCPIPRemoteFunctionTransporter }
constructor TTCPIPRemoteFunctionTransporter.Create(const aSocketStream: TWinSocketStreamExt;
    const aRemoteFunctionStreamer: TRemoteFunctionStreamer);
begin
    inherited Create();
    fSocketStream := aSocketStream;
    fRemoteFunctionStreamer := aRemoteFunctionStreamer;
end;

procedure TTCPIPRemoteFunctionTransporter.LoadEnvelopeToBuffer(const aEnvelope: TEnvelope);
var
    xStr: TRemoteStringType;
begin
    xStr := fRemoteFunctionStreamer.EnvelopeToStr(aEnvelope);
    Log(xStr);
    fSocketStream.Buffer.CopyFromStr(xStr);
end;

procedure TTCPIPRemoteFunctionTransporter.SendCall(const aEnvelope: TEnvelope);
begin
    LoadEnvelopeToBuffer(aEnvelope);
    fSocketStream.SendFromBuffer();
end;

procedure TTCPIPRemoteFunctionTransporter.SendResult(const aEnvelope: TEnvelope);
begin
    LoadEnvelopeToBuffer(aEnvelope);
end;

function TTCPIPRemoteFunctionTransporter.UnLoadEnvelopeFromBuffer(): TEnvelope;
var
    xStr: TRemoteStringType;
begin
    fSocketStream.Buffer.CopyToStr(xStr);
    result := fRemoteFunctionStreamer.StrToEnvelope(xStr);
    Log(xStr);
end;

function TTCPIPRemoteFunctionTransporter.ReceiveCall(): TEnvelope;
begin
    result := UnLoadEnvelopeFromBuffer();
end;

function TTCPIPRemoteFunctionTransporter.ReceiveResult(): TEnvelope;
begin
    result := UnLoadEnvelopeFromBuffer();
end;

function TTCPIPRemoteFunctionTransporter.Wait: boolean;
begin
    result := fSocketStream.ReceiveResponseFromBuffer() <> rtNack;
end;


end.
