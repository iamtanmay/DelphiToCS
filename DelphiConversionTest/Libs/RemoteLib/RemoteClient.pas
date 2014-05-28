{ --------------------------------------------------------------------------------------------------
  Copyright © 2007 Zinsser Analytic GmbH - All rights reserved.
  Author       : Payman Kamali (pk)
  Description  : Classes used only on client side of TCP\IP communication
  --------------------------------------------------------------------------------------------------
  Revision History:
  date     op  method                       track-no improvement/change
  -------- --  ---------------------------  -------- -----------------------------------------------
  31.05.07 pk                               TN3713  initial revision
  04.02.08 pk  TMsgClient.Close             TN3713  error handling
  04.02.08 pk  TMsgClient.CloseClient       TN3713  CloseClient instead of Close
  09.06.09 pk                               TN3585.2 Various Changes
  21.09.09 pk  CloseClient                  TN4777   set Active:= false even on exception
  -------------------------------------------------------------------------------------------------- }

unit RemoteClient;


interface


uses
    Windows,
    Messages,
    SysUtils,
    Classes,
    scktcomp,
    RemoteClasses;

const
    INT_CLIENT_WAITFORDATA_TIMEOUT_MSECS = 15000;

type
    TMsgClient = class(TClientSocket)
    protected
        fStream: TWinSocketStreamExt;
        function CreateSocketStream(): TWinSocketStreamExt; virtual;
        procedure SendQuit(); virtual;
    public
        constructor Create(const aHost: string; aPort: integer); reintroduce;
        destructor Destroy(); override;
        procedure CloseClient();
        procedure DoOnConnect(Sender: TObject; Socket: TCustomWinSocket);
        property Stream: TWinSocketStreamExt read fStream;
    end;


implementation


uses
    Dialogs;

constructor TMsgClient.Create(const aHost: string; aPort: integer);
begin
    inherited Create(nil);

    // self.Host := aHost;
    self.Address := aHost;
    self.Port := aPort;

    // self.ClientType := ctNonBlocking;
    // self.ReportLevel := 1;
    // self.Timeout := 5000;
    self.ClientType := ctBlocking;
    self.OnConnect := DoOnConnect;
    fStream := CreateSocketStream();
end;

destructor TMsgClient.Destroy();
begin

    fStream.Free;
    inherited;
end;

function TMsgClient.CreateSocketStream(): TWinSocketStreamExt;
begin
    result := TWinSocketByteStream.Create(self.Socket, 5000, INT_CLIENT_WAITFORDATA_TIMEOUT_MSECS);
end;

procedure TMsgClient.DoOnConnect(Sender: TObject; Socket: TCustomWinSocket);
begin
    gmRemoteWriteLog(Format('Connected to: server %s, port %d', [self.Address, self.Port]));
end;

procedure TMsgClient.SendQuit();
begin
    self.Stream.SendQuit();
end;

procedure TMsgClient.CloseClient();
begin
    if not self.Active then
        EXIT;
    try
        SendQuit();
    except
        on e: Exception do
            gmRemoteWriteLog(Format('could not close port - %s', [e.Message]));
    end;

    self.Active := false;
end;

{
  procedure TMsgClient.MakeWinMessage( aBuffer : TByteBuffer;
  aWinHandle, aWinMessage, aWinWParam, aWinLParam : integer );
  begin
  aBuffer.WriteInt( INT_MESSAGE_TYPE_WIN );
  aBuffer.WriteInt( aWinHandle );
  aBuffer.WriteInt( aWinMessage );
  aBuffer.WriteInt( aWinWParam );
  aBuffer.WriteInt( aWinLParam );
  end;
}


end.
