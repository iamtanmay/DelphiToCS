unit ProcExec;


{ -----------------------------------------------------------------------------------------------------------------------
  Copyright © 2009 Zinsser Analytic GmbH - All rights reserved.
  Author       : Payman Kamali (pk)
  Description  :
  -----------------------------------------------------------------------------------------------------------------------
  Revision History:
  date     op  method                                track-no  improvement/change
  -------- --  ------------------------------------  --------- ----------------------------------------------------------
  30.07.09 pk                                         TN4585.5    Initial Revision
  ----------------------------------------------------------------------------------------------------------------------- }
interface


uses
    Windows,
    Variants,
    SysUtils;

type
    TProcExec = class
    private
        fProcessName: string;
        fStartupinfo: TStartupinfo;
        fProcessInfo: TProcessInformation;
    public
        constructor Create(const aProcessName: string);
        destructor Destroy(); override;
        function ExecProcess(): boolean;
        procedure WaitFor();
        function GetExitCode: DWord;
        class function IsErrorExitCode(const aExitCode: DWord): boolean; static;
    end;


implementation


uses
    ErrorManager;

constructor TProcExec.Create(const aProcessName: string);
begin
    inherited Create();
    fProcessName := aProcessName;
    // StartupInfo-record füllen
    FillChar(fStartupinfo, SizeOf(fStartupinfo), 0);
    fStartupinfo.cb := SizeOf(TStartupinfo);
end;

destructor TProcExec.Destroy();
begin
    CloseHandle(fProcessInfo.hProcess);
    inherited;
end;

function TProcExec.ExecProcess(): boolean;
begin
    result := CreateProcess(nil, PChar(fProcessName), nil, nil, false, NORMAL_PRIORITY_CLASS, nil, nil,
        fStartupinfo, fProcessInfo);
end;

procedure TProcExec.WaitFor();
begin

    // auf Ende des Prozesses warten
    while (WaitForSingleObject(fProcessInfo.hProcess, 200) <> WAIT_OBJECT_0) do
    begin
        if gErrorManager.IsGlobalErr() then
            EXIT;
    end;
end;

function TProcExec.GetExitCode: DWord;
begin
    GetExitCodeProcess(fProcessinfo.hProcess, result);
end;

class function TProcExec.IsErrorExitCode(const aExitCode: DWord): boolean;
begin
    result := aExitCode <> 0;
end;


end.
