{ -----------------------------------------------------------------------------------------------------------------------
  Copyright © 2008 Zinsser Analytic GmbH - All rights reserved.
  Author       : Payman Kamali (pk)
  Description  :
  -----------------------------------------------------------------------------------------------------------------------
  Revision History:
  date     op  method                                track-no  improvement/change
  -------- --  ------------------------------------  --------- ----------------------------------------------------------
  19.09.08 pk  MustPutBackTool                        TN4215   as property instead of field
  20.09.11 wl  HasCalliOpt                            TN5694   entfernt
  ----------------------------------------------------------------------------------------------------------------------- }

unit Action;


interface


uses
    GeneralTypes,
    Executable;

type
    TAction = class(TExecutable)
    protected
        function GetMustPutBackTool: boolean; virtual;
    public
        constructor Create();
        procedure ExecFirst(); virtual;
        procedure ExecLast(); virtual;
        // IExecutable
        procedure Execute(); override;

        property MustPutBackTool: boolean read GetMustPutBackTool;
    end;


implementation


constructor TAction.Create();
begin
    inherited Create;
end;

procedure TAction.ExecFirst();
begin
end;

procedure TAction.ExecLast();
begin
end;

procedure TAction.Execute();
begin
    ExecFirst();
    ExecLast();
end;

function TAction.GetMustPutBackTool: boolean;
begin
    result := false;
end;


end.
