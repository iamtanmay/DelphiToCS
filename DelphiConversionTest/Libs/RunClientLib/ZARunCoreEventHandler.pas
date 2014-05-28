{ -----------------------------------------------------------------------------------------------------------------------
  Copyright © 2009 Zinsser Analytic GmbH - All rights reserved.
  Author       : Payman Kamali (pk)
  Description  :
  -----------------------------------------------------------------------------------------------------------------------
  Revision History:
  date     op  method                                track-no  improvement/change
  -------- --  ------------------------------------  --------- ----------------------------------------------------------
  09.06.09 pk                                        TN4585.2    Initial Revision
  18.06.09 pk  ProcessFinished                       TN4585.2    New aIsError Parameter
  08.09.09 pk                                        TN4753      uses ErrorMessage replaced by ErrorInfo
  12.10.09 pk                                        TN4812      Status event functions moved here from RunClientMain
  28.05.10 wl                                        TN5116      uses EdExtern
  04.06.10 wl                                        TN5116    uses geändert
  02.02.11 wl  MessageBox                            TN5466   Parameter geändert
  21.03.11 wl  AskRunStart_PromptModal               TN5508   neue Parameter
  14.12.11 wl  ProcessFinished                       TN5765   MethDone-Dialog durch MessageBox ersetzt
  20.04.12 wl  StopPromptModal                       TN5946   entfernt
  29.05.12 wl  LogTextDisplay                        TN5904   mit ThreadID und DisplayType als Parameter
  ----------------------------------------------------------------------------------------------------------------------- }

unit ZARunCoreEventHandler;


interface


uses
    GeneralTypes,
    LogManager,
    CoreEventHandler;

type
    TZARunCoreEventHandler = class(TCoreEventHandler)
    protected
        // controller events
        function ErrorBox(const aErrorID: integer; const aErrorInfo: TObject): integer; override;
        function MessageBox(const aText: string; const aCaption: string = ''; aButtons: integer = 0;
            aIcon: integer = 0; aDefaultButton: integer = 0): integer; override;
        function AskRunStartPromptModal(const aMethodName: string; const aIsSimChangeAllowed: boolean;
            var vIsSim: boolean; var vSimulationAskWeight: boolean; var vSimulationSpeed_Percent: integer)
            : integer; override;
        function ProcessStarted(const aSourceDataName: string; const aProcessID: string): boolean; override;
        procedure ProcessFinished(const aProcessID: string; const aIsError: boolean); override;
        // procedure ProcessPaused(const aProcessID: string); override;

        // status events
        procedure RunInfoInsert(const aDisplayID: string; const aGroupNames: TStringArray;
            const aKey, aText: string; aInfoGroupBehaviour: integer); override;
        procedure LoadDisplayComponentToMain(const aDisplayComponentName: string;
            const aContextID: string); override;
        procedure LogTextDisplay(const aLogText: string; aThreadID: integer;
            aDisplayType: TDisplayLogInfoType); override;
        procedure LinkRunIDToProcess(const aRunID: string); override;
    end;


implementation


uses
    RunDialogsManager,
    ErrorInfo,
    AppTypes,
    EdExtern;

{ TZARunCoreEventHandler }

function TZARunCoreEventHandler.MessageBox(const aText, aCaption: string;
    aButtons, aIcon, aDefaultButton: integer): integer;
begin
    result := TRunDialogsManager.Instance.MessageBox_Show(aText, aCaption, aButtons, aIcon, aDefaultButton);
end;

function TZARunCoreEventHandler.ProcessStarted(const aSourceDataName, aProcessID: string): boolean;
begin
    TRunDialogsManager.Instance.DisplayComponents_RunEnable(true);
    result := true;
end;

procedure TZARunCoreEventHandler.ProcessFinished(const aProcessID: string; const aIsError: boolean);
begin
    if not aIsError then
    begin
        MessageBox('Method Completed');
    end;

    TRunDialogsManager.Instance.DisplayComponents_RunEnable(false);
    TRunDialogsManager.Instance.UnloadAnyLoadedDisplayComponents();
end;

{
  procedure TZARunCoreEventHandler.ProcessPaused(const aProcessID: string);
  begin
  TRunDialogsManager.Instance.SetRunMainState(rmsPaused);
  end;
}
function TZARunCoreEventHandler.ErrorBox(const aErrorID: integer; const aErrorInfo: TObject): integer;
var
    xErrorInfo: TErrorInfo;
begin
    xErrorInfo := aErrorInfo as TErrorInfo;
    try
        result := TRunDialogsManager.Instance.ErrBox_PromptModal(xErrorInfo);
    finally
        xErrorInfo.Free;
    end;
end;

procedure TZARunCoreEventHandler.RunInfoInsert(const aDisplayID: string; const aGroupNames: TStringArray;
    const aKey, aText: string; aInfoGroupBehaviour: integer);
begin
    TRunDialogsManager.Instance.GroupDisplay_InsertInfo(aDisplayID, aGroupNames, aKey, aText,
        TInfoGroupBehaviour(aInfoGroupBehaviour));
end;

procedure TZARunCoreEventHandler.LinkRunIDToProcess(const aRunID: string);
begin
    TEdExtern.Instance.RegisterControllerIfNew(aRunID);
end;

procedure TZARunCoreEventHandler.LoadDisplayComponentToMain(const aDisplayComponentName, aContextID: string);
begin
    TRunDialogsManager.Instance.LoadDisplayComponentToMain(aDisplayComponentName, aContextID);
end;

procedure TZARunCoreEventHandler.LogTextDisplay(const aLogText: string; aThreadID: integer;
    aDisplayType: TDisplayLogInfoType);
begin
    TRunDialogsManager.Instance.LogText_Display(aLogText, aThreadID, aDisplayType);
end;

function TZARunCoreEventHandler.AskRunStartPromptModal(const aMethodName: string;
    const aIsSimChangeAllowed: boolean; var vIsSim: boolean; var vSimulationAskWeight: boolean;
    var vSimulationSpeed_Percent: integer): integer;
begin
    result := TRunDialogsManager.Instance.AskRunStart_PromptModal(aMethodName, aIsSimChangeAllowed, vIsSim,
        vSimulationAskWeight, vSimulationSpeed_Percent);
end;


end.
