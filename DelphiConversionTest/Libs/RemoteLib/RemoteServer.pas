{ --------------------------------------------------------------------------------------------------
  Copyright © 2007 Zinsser Analytic GmbH - All rights reserved.
  Author       : Payman Kamali (pk)
  Description  : Classes used only on server side of TCP\IP communication
  --------------------------------------------------------------------------------------------------
  Revision History:
  date     op  method                       track-no improvement/change
  -------- --  ---------------------------  -------- -----------------------------------------------
  31.05.07 pk                               TN3713  initial revision
  04.02.08 pk  ClientExecute                TN3713  self.Terminate called on thread otherwise clientexecute is called again
  04.11.09 pk                               TN4843  Replace TObjectList, TStringList, TStrings with Generics Lists and TStringArray
  09.02.10 pk                               TN4973  Thread.Resume replaced by Thread.Start
  19.10.10 pk                               TN5305  changes needed for CoreClient/Server
  10.04.13 wl                               TN6045   uses Generics.Collections
  -------------------------------------------------------------------------------------------------- }

unit RemoteServer;


interface


uses
    SysUtils,
    scktcomp,
    SyncObjs,
    Classes,
    Generics.Collections,
    RemoteClasses,
    ThreadClasses;

const
    INT_SERVER_WAITFORDATA_TIMEOUT_MSECS = 3000;

type

    TFirstMesssageParams = record
        ServerType: integer;
        ServerName: string;
    end;

    TNextMessageParams = record
        MsgType: integer;
        MsgParams: string;
    end;

    TMsgInterpreter = class
    protected
        fStream: TWinSocketStreamExt;
        fSocket: TServerClientWinSocket;
        function CreateSocketStream(): TWinSocketStreamExt; virtual;
    public
        constructor Create(const aSocket: TServerClientWinSocket);
        destructor Destroy(); override;
        function InterpretFirstMessage(): TFirstMesssageParams;
        property Stream: TWinSocketStreamExt read fStream;
        property Socket: TServerClientWinSocket read fSocket;
    end;

    TMsgHandler = class
    protected
        fInterpreter: TMsgInterpreter;
        fQuit: boolean;
        procedure HandleNext(); virtual;
        function IsQuitReceived: boolean; virtual;
    public
        constructor Create(aInterpreter: TMsgInterpreter);
        procedure Execute();
        procedure Quit();
    end;

    TCreateMsgHandlerCallBack = function(aInterpreter: TMsgInterpreter; aParams: TFirstMesssageParams)
        : TMsgHandler of object;
    TCreateMsgInterpreterCallBack = function(const aSocket: TServerClientWinSocket)
        : TMsgInterpreter of object;

    TMsgServer = class(TServerSocket)
    protected
        fTimeout: integer;
        fOnCreateMsgHandler: TCreateMsgHandlerCallBack;
        fOnCreateMsgInterpreter: TCreateMsgInterpreterCallBack;
        fCriticalSection: TCriticalSection;
        fClientThreads: TObjectList<TThread>;
        procedure DoOnServerSocketGetThread(Sender: TObject; ClientSocket: TServerClientWinSocket;
            var SocketThread: TServerClientThread);
        procedure DoOnThreadTerminate(aSender: TObject);
    public
        constructor Create(aPort: integer); reintroduce;
        destructor Destroy; override;
        // procedure DoOnAccept( Sender: TObject; Socket: TCustomWinSocket );
        // procedure DoOnClientRead(  Sender: TObject; Socket: TCustomWinSocket );
        property OnCreateMsgHandler: TCreateMsgHandlerCallBack read fOnCreateMsgHandler
            write fOnCreateMsgHandler;
        property OnCreateMsgInterpreter: TCreateMsgInterpreterCallBack read fOnCreateMsgInterpreter
            write fOnCreateMsgInterpreter;
    end;

    { TServerClientThreadExt }
    TServerClientThreadExt = class(TServerClientThread)
    private
        procedure WaitTillTerminate;
        function CreateMsgInterpreter: TMsgInterpreter;
    protected
        fOnCreateMsgHandler: TCreateMsgHandlerCallBack;
        fOnCreateMsgInterpreter: TCreateMsgInterpreterCallBack;
        fInterpreter: TMsgInterpreter;
        fHandler: TMsgHandler;
        fWaitTerminateLock: TSimpleEvent;
        fDoOnTerminate: TNotifyEvent;
    public
        constructor Create(CreateSuspended: Boolean; ASocket: TServerClientWinSocket);
        destructor Destroy; override;
        procedure ClientExecute; override;
        procedure Quit();
        property Terminated;
        property DoOnTerminate: TNotifyEvent read fDoOnTerminate write fDoOnTerminate;
        property OnCreateMsgHandler: TCreateMsgHandlerCallBack read fOnCreateMsgHandler
            write fOnCreateMsgHandler;
        property OnCreateMsgInterpreter: TCreateMsgInterpreterCallBack read fOnCreateMsgInterpreter
            write fOnCreateMsgInterpreter;
    end;

var
    gMsgServer: TMsgServer = nil;


implementation


uses
    Windows;

constructor TMsgServer.Create(aPort: integer);
begin
    inherited Create(nil);
    self.Port := aPort;

    // self.OnAccept := DoOnAccept;
    // self.OnClientRead := DoOnClientRead;
    // self.ReportLevel := 1;
    // self.Timeout := 5000;
    self.ServerType := stThreadBlocking;
    // self.ServerType := stNonBlocking;
    self.OnGetThread := DoOnServerSocketGetThread;
    fTimeout := 5000;
    fCriticalSection := TCriticalSection.Create();
    fClientThreads := TObjectList<TThread>.Create(false);
end;

destructor TMsgServer.Destroy();
var
    x: integer;
    xFirstThread: TServerClientThreadExt;
begin
    fCriticalSection.Acquire();
    try
        for x := fClientThreads.Count - 1 downto 0 do
        begin
            (fClientThreads[x] as TServerClientThreadExt).Quit();
        end;
    finally
        fCriticalSection.Release;
    end;

    xFirstThread := nil;
    while true do
    begin
        fCriticalSection.Acquire();
        try
            if fClientThreads.Count = 0 then
                BREAK;
            xFirstThread := (fClientThreads[0] as TServerClientThreadExt);
        finally
            fCriticalSection.Release();
        end;
        xFirstThread.WaitTillTerminate();
    end;

    fClientThreads.Free;
    fCriticalSection.Free;

    inherited;
end;

procedure TMsgServer.DoOnServerSocketGetThread(Sender: TObject; ClientSocket: TServerClientWinSocket;
    var SocketThread: TServerClientThread);
const
    BOOL_CREATE_SUSPENDED = true;
begin
    // Create a new thread for connection
    SocketThread := TServerClientThreadExt.Create(BOOL_CREATE_SUSPENDED, ClientSocket);

    fCriticalSection.Acquire();
    try
        fClientThreads.Add(SocketThread);
    finally
        fCriticalSection.Release();
    end;

    (SocketThread as TServerClientThreadExt).OnCreateMsgHandler := fOnCreateMsgHandler;
    (SocketThread as TServerClientThreadExt).OnCreateMsgInterpreter := fOnCreateMsgInterpreter;
    (SocketThread as TServerClientThreadExt).DoOnTerminate := DoOnThreadTerminate;
    SocketThread.Start;
end;

procedure TMsgServer.DoOnThreadTerminate(aSender: TObject);
begin
    fCriticalSection.Acquire();
    try
        fClientThreads.Remove(aSender as TThread);
    finally
        fCriticalSection.Release();
    end;
end;

{
  procedure TMsgInterpreter.InterpretWinMessage( aBuffer : TByteBuffer; aNumBytes : integer );
  var
  xHandle, xMessage, xWParam, xLParam : integer;
  begin
  xHandle := aBuffer.ReadInt();
  xMessage := aBuffer.ReadInt();
  xWParam := aBuffer.ReadInt();
  xLParam := aBuffer.ReadInt();
  PostMessage( xHandle, xMessage, xWParam, xLParam );
  end;
}

constructor TMsgInterpreter.Create(const aSocket: TServerClientWinSocket);
begin
    inherited Create();
    fSocket := aSocket;
    fStream := CreateSocketStream();
end;

destructor TMsgInterpreter.Destroy();
begin
    fStream.Free;
    inherited;
end;

function TMsgInterpreter.CreateSocketStream(): TWinSocketStreamExt;
begin
    result := TWinSocketByteStream.Create(fSocket, 2000, INT_SERVER_WAITFORDATA_TIMEOUT_MSECS);
end;

function TMsgInterpreter.InterpretFirstMessage(): TFirstMesssageParams;
var
    xResponseType: TResponseType;
    xReadSockResponse: TReadSockResponse;
begin
    xResponseType := rtNack;
    xReadSockResponse := fStream.ReceiveFirstMessageToBuffer(result.ServerType, result.ServerName);
    if xReadSockResponse = rsrOK then
    begin
        xResponseType := rtAck;
    end;

    fStream.SendResponseFromBuffer(xResponseType);
end;

{ TMsgHandler }

constructor TMsgHandler.Create(aInterpreter: TMsgInterpreter);
begin
    inherited Create();
    fInterpreter := aInterpreter;
    fQuit := false;
end;

function TMsgHandler.IsQuitReceived: boolean;
begin
    result := fInterpreter.Stream.IsQuitReceived();
end;

procedure TMsgHandler.Execute;
var
    xReadSockResponse: TReadSockResponse;
    xResponseType: TResponseType;
begin
    while true do
    begin
        if fQuit then
        begin
            fInterpreter.Stream.Close();
            EXIT;
        end;
        xResponseType := rtNack;
        xReadSockResponse := fInterpreter.Stream.ReceiveToBuffer();

        if xReadSockResponse = rsrTimeout then
        begin
            CONTINUE;
        end;

        if xReadSockResponse = rsrOK then
        begin
            try
                if IsQuitReceived() then
                begin
                    EXIT;
                end;
                HandleNext();
            except
                on E: Exception do
                    gmRemoteWriteLog(Format('Exception occured in remote MsgHandler: %s', [E.Message]));
            end;
            xResponseType := rtAck;
        end;
        fInterpreter.Stream.SendResponseFromBuffer(xResponseType);
    end;
end;

procedure TMsgHandler.HandleNext();
begin

end;

procedure TMsgHandler.Quit;
begin
    fQuit := true;
end;

{ TServerClientThreadExt }
constructor TServerClientThreadExt.Create(CreateSuspended: Boolean; ASocket: TServerClientWinSocket);
begin
    inherited Create(CreateSuspended, ASocket);
    fWaitTerminateLock := TSimpleEvent.Create();
    fHandler := nil;
    fInterpreter := nil;
end;

destructor TServerClientThreadExt.Destroy();
begin
    fInterpreter.Free;
    fWaitTerminateLock.Free;
    inherited;
end;

function TServerClientThreadExt.CreateMsgInterpreter(): TMsgInterpreter;
begin
    result := fOnCreateMsgInterpreter(ClientSocket);
end;

procedure TServerClientThreadExt.ClientExecute;
var
    xFirstMessageParams: TFirstMesssageParams;
begin
    try
        try
            fInterpreter := CreateMsgInterpreter();
            xFirstMessageParams := fInterpreter.InterpretFirstMessage();
            fHandler := fOnCreateMsgHandler(fInterpreter, xFirstMessageParams);
            try
                fHandler.Execute();
            finally
                fHandler.Free;
                fHandler := nil;
            end;

        except
            on E: Exception do
            begin
                gmRemoteWriteLog('TServerClientThreadExt.ClientExecute - ' + e.Message);
                try
                    ClientSocket.Close;
                except
                    on e: Exception do
                        gmRemoteWriteLog('TServerClientThreadExt.ClientExecute CloseSocket - ' + e.Message);
                end;
            end;
        end;
    finally
        // 04.02.08 pk It is very important that we call Terminate. otherwise the thread never ends and clientexecute is called again
        Terminate();

        fWaitTerminateLock.SetEvent;
        if Assigned(fDoOnTerminate) then
            fDoOnTerminate(self);
    end;

end;

procedure TServerClientThreadExt.Quit;
begin
    if Assigned(fHandler) then
        fHandler.Quit();
end;

procedure TServerClientThreadExt.WaitTillTerminate();
begin
    fWaitTerminateLock.WaitFor(1000);
end;


end.
