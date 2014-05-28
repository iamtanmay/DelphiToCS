unit ExecHandlerMinimal;
{ --------------------------------------------------------------------------------------------------
  Copyright © 2008 Zinsser Analytic GmbH - All rights reserved.
  Author       : Wolfgang Lyncke (wl)
  Description  :
  --------------------------------------------------------------------------------------------------
  Revision History:
  date     op  method                       track-no  improvement/change
  -------- --  ---------------------------  --------  ----------------------------------------------
  03.07.08 wl                                         TN4157
  06.11.08 pk  SetThread                    TN4280    removed
  -------------------------------------------------------------------------------------------------- }


interface


uses
    Classes,
    GeneralTypes,
    Executable;

type
    TExecHandlerMinimal = class(TExecutable)
    protected
        fIntfExecutable: IExecutable;
        fCompleted: boolean;
        procedure OnSetThread(); virtual;
        procedure Initialize(); virtual;
        procedure Finalize(); virtual;
        function IsExecutable(): boolean; virtual;
    public
        constructor Create(aIntfExecutable: IExecutable);
        procedure Execute(); override;
        procedure InterruptStart(); virtual;
        procedure InterruptFinish(); virtual;
        procedure ErrorSet(); virtual;
    end;


implementation


constructor TExecHandlerMinimal.Create(aIntfExecutable: IExecutable);
begin
    inherited Create;
    fIntfExecutable := aIntfExecutable;
end;

procedure TExecHandlerMinimal.Initialize();
begin
end;

function TExecHandlerMinimal.IsExecutable(): boolean;
begin
    result := true;
end;

procedure TExecHandlerMinimal.Execute();
begin
    fCompleted := false;
    try
        Initialize();
        if IsExecutable() then
            fIntfExecutable.Execute();

        fCompleted := true // set completed to true if there were no exceptions
    finally
        Finalize();
    end;

end;

procedure TExecHandlerMinimal.Finalize();
begin
end;

procedure TExecHandlerMinimal.InterruptStart();
begin
    // if Assigned( fThread ) then
    // fThread.Suspend;
end;

procedure TExecHandlerMinimal.InterruptFinish();
begin
    // if Assigned( fThread ) then
    // fThread.Resume;
end;

procedure TExecHandlerMinimal.ErrorSet();
begin
end;

procedure TExecHandlerMinimal.OnSetThread();
begin
end;


end.
