{ -----------------------------------------------------------------------------------------------------------------------
  Copyright © 2009 Zinsser Analytic GmbH - All rights reserved.
  Author       : Payman Kamali (pk)
  Description  :
  -----------------------------------------------------------------------------------------------------------------------
  Revision History:
  date     op  method                                track-no  improvement/change
  -------- --  ------------------------------------  --------- ----------------------------------------------------------
  09.06.09 pk                                        TN4585.2    Initial Revision
  30.07.09 pk                                        TN4585.5    Various Changes
  31.07.09 ts  GroupDisplay_InsertInfo               TN4666      New InfoGroupBehavior instead of aHideGroup
  13.08.09 wl  ToolErr_PromptInput                   TN4712      ein Parameter entfernt
  24.08.09 pk  ErrBox_PromptModal                    TN4735.4    Log the error on the server side
  08.09.09 pk                                        TN4753      uses ErrorMessage replaced by ErrorInfo
  12.10.09 pk  GroupDisplay_InsertInfo               TN4812      now implemented
  12.10.09 pk  ErrBox_PromptModal                    TN4812      implemented with more parmeters
  04.11.09 pk                               	    TN4843   	Replace TObjectList, TStringList, TStrings with Generics Lists and TStringArray
  20.07.10 pk                                        TN5201   ChangeTime function now with Progress parameter
  17.08.10 wl  MessageBox                            TN5112   Parameter neu aufgeteilt
  02.02.11 wl  SelectItemBox                         TN5466   neu
  21.03.11 wl  AskRunStart_PromptModal               TN5508   neue Parameter
  26.11.11 wl  BitmapMessageBox                      TN5750   neu: MessageBox mit Bitmap
  14.12.11 wl                                        TN5765   MethDone-Dialog durch MessageBox ersetzt
  20.04.12 wl  Stop_PromptModal                      TN5946   entfernt
  29.05.12 wl  LogText_Display                       TN5904    mit ThreadID und DisplayType als Parameter
  12.11.12 wl                                        TN6008   Log-Funktion angepasst
  30.07.13 wl                                        TN6160   an TSystemEvents angepasst
  ----------------------------------------------------------------------------------------------------------------------- }

unit GUIManagerCoreServer;


interface


uses
    Classes,
    ThreadUtils,
    GUIManager,
    GeneralTypes,
    LogManager,
    GUIManagerRun,
    AppTypes;

type
    TGUIManagerCoreServer = class(TGUIManagerRun)
    public
        procedure LogText_Display(const aLogText: string; aThreadID: integer;
            aDisplayType: TDisplayLogInfoType); override;

        function MessageBox(const aText: string; const aCaption: string = ''; aButtons: integer = 0;
            aIcon: integer = 0; aDefaultButton: integer = 0): integer; override;
        function BitmapMessageBox(const aBitmapName, aText, aCaption: string; aButtons: integer)
            : Integer; override;
        function SelectItemBox(const aItems: TStringArray; const aText, aCaption, aButtonText: string)
            : string; override;

        function Barcode_PromptInput(var vText: string; aCaption, aLabelCaption, aNoRackBtnCaption: string)
            : integer; override;
        function ErrBox_PromptModal(aPErrorInfo: integer): integer; override;
        procedure InterruptSignalStart(); override;
        // function Stop_PromptModal: integer; override;
        function ToolErr_PromptInput(aMessage: string): integer; override;
        function RunAbortInfo_PromptModal(const aActionStatus: TStringArray): integer; override;

        function RackPlaceList_PromptModal(const aRunName, aLayoutName, aRackTypes, aCaptionStr: string;
            aAbortAllowed, aMarkRacks: boolean): integer; override;

        procedure Delay_ChangeTime(const aID: string; aNewTime: string; const aProgress: integer); override;
        procedure Delay_InsertInfo(const aID: string; aTime, aDescription: string); override;
        procedure Delay_DeleteInfo(const aID: string); override;
        function Delay_IsCancelled(const aID: string): boolean; override;

        procedure GroupDisplay_InsertInfo(const aDisplayID: string; const aGroupNames: TStringArray;
            const aKey, aText: string; aInfoGroupBehaviour: TInfoGroupBehaviour); override;
        procedure GroupDisplay_StartNewRun(const aMethodName: string); override;

        function AskRunStart_PromptModal(const aMethodName: string; const aIsSimChangeAllowed: boolean;
            var vIsSim: boolean; var vSimulationAskWeight: boolean; var vSimulationSpeed_Percent: integer)
            : integer; override;

        procedure LoadDisplayComponentToMain(const aDisplayComponentName: string;
            const aContextID: string); override;
    end;


implementation


uses
    Windows,
    SysUtils,
    ErrorMessageFactory,
    ErrorInfo,
    CoreServerEventManager,
    Streamable;

function TGUIManagerCoreServer.MessageBox(const aText: string; const aCaption: string = '';
    aButtons: integer = 0; aIcon: integer = 0; aDefaultButton: integer = 0): integer;
var
    xLogString: string;
begin
    result := TCoreServerEventManager.Instance.MessageBox(aText, aCaption, aButtons, aIcon,
        aDefaultButton, ID_OK);
    xLogString := Format('Message: %s (%s) - Result: %d', [aText, aCaption, result]);
    gLogManager.Log(xLogString, true);
end;

procedure TGUIManagerCoreServer.LogText_Display(const aLogText: string; aThreadID: integer;
    aDisplayType: TDisplayLogInfoType);
begin
    TCoreServerEventManager.Instance.TransmitWriteLogEvent(aLogText, aThreadID, Integer(aDisplayType));
end;

function TGUIManagerCoreServer.ErrBox_PromptModal(aPErrorInfo: integer): integer;
var
    xErrorInfo: TErrorInfo;
    xErrorID: integer;
begin
    xErrorID := 1; // xErrorInfo.ErrorID, every error-type should have its own unique id;
    xErrorInfo := nil;
    try
        xErrorInfo := TErrorInfo(aPErrorInfo);
        TErrorMessageFactory.WriteErrorLog(xErrorInfo);
    except
    end;
    result := TCoreServerEventManager.Instance.ErrorBox(xErrorID, xErrorInfo, idAbort);
end;

function TGUIManagerCoreServer.SelectItemBox(const aItems: TStringArray;
    const aText, aCaption, aButtonText: string): string;
begin

end;

procedure TGUIManagerCoreServer.InterruptSignalStart();
begin
end;

function TGUIManagerCoreServer.ToolErr_PromptInput(aMessage: string): integer;
begin
    result := 0;
end;

{
  function TGUIManagerCoreServer.Stop_PromptModal(): integer;
  begin
  result := TCoreServerEventManager.Instance.StopPromptModal(ID_NO);
  end;
}
function TGUIManagerCoreServer.Barcode_PromptInput(var vText: string;
    aCaption, aLabelCaption, aNoRackBtnCaption: string): integer;
begin
    result := 0;
end;

function TGUIManagerCoreServer.BitmapMessageBox(const aBitmapName, aText, aCaption: string;
    aButtons: integer): Integer;
begin
    result := 0;
end;

function TGUIManagerCoreServer.RunAbortInfo_PromptModal(const aActionStatus: TStringArray): integer;
begin
    result := 0;
end;

function TGUIManagerCoreServer.RackPlaceList_PromptModal(const aRunName, aLayoutName, aRackTypes,
    aCaptionStr: string; aAbortAllowed, aMarkRacks: boolean): integer;
begin
    result := 0;
end;

procedure TGUIManagerCoreServer.Delay_ChangeTime(const aID: string; aNewTime: string;
    const aProgress: integer);
begin

end;

procedure TGUIManagerCoreServer.Delay_DeleteInfo(const aID: string);
begin

end;

procedure TGUIManagerCoreServer.Delay_InsertInfo(const aID: string; aTime, aDescription: string);
begin

end;

function TGUIManagerCoreServer.Delay_IsCancelled(const aID: string): boolean;
begin
    result := false;
end;

procedure TGUIManagerCoreServer.GroupDisplay_InsertInfo(const aDisplayID: string;
    const aGroupNames: TStringArray; const aKey, aText: string; aInfoGroupBehaviour: TInfoGroupBehaviour);
begin
    TCoreServerEventManager.Instance.TransmitRunInfoInsert(aDisplayID, aGroupNames, aKey, aText,
        integer(aInfoGroupBehaviour));
end;

procedure TGUIManagerCoreServer.GroupDisplay_StartNewRun(const aMethodName: string);
begin

end;

function TGUIManagerCoreServer.AskRunStart_PromptModal(const aMethodName: string;
    const aIsSimChangeAllowed: boolean; var vIsSim: boolean; var vSimulationAskWeight: boolean;
    var vSimulationSpeed_Percent: integer): integer;
begin
    result := TCoreServerEventManager.Instance.AskRunStartPromptModal(aMethodName, aIsSimChangeAllowed,
        vIsSim, vSimulationAskWeight, vSimulationSpeed_Percent, idOK);
end;

procedure TGUIManagerCoreServer.LoadDisplayComponentToMain(const aDisplayComponentName: string;
    const aContextID: string);
begin
    TCoreServerEventManager.Instance.TransmitLoadDisplayComponentEvent(aDisplayComponentName, aContextID);
end;


end.
