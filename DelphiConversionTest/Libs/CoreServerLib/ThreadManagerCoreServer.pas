unit ThreadManagerCoreServer;


{ -----------------------------------------------------------------------------------------------------------------------
  Copyright © 2009 Zinsser Analytic GmbH - All rights reserved.
  Author       : Payman Kamali (pk)
  Description  :
  -----------------------------------------------------------------------------------------------------------------------
  Revision History:
  date     op  method                                track-no  improvement/change
  -------- --  ------------------------------------  --------- ----------------------------------------------------------
  09.06.09 pk                                        TN4585.2    Initial Revision
  ----------------------------------------------------------------------------------------------------------------------- }
interface


uses
    ThrdManExt;

type
    TThreadManagerCoreServer = class(TThreadManagerRun)
    protected
        procedure Initialize(); override;
    end;


implementation


uses
    ServerCoreServerCalls;

{ TThreadManagerCoreServer }

procedure TThreadManagerCoreServer.Initialize;
begin
    inherited;
    TServerCoreServerCalls.Instance.InitCallHandlerThread();
end;


end.
