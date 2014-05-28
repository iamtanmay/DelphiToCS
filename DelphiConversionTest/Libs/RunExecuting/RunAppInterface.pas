{ --------------------------------------------------------------------------------------------------
  Copyright © 2002 Zinsser Analytic GmbH - All rights reserved.
  Author       : Wolfgang Lyncke (wl)
  Description  : workbench object with specisl methods for Removable Redi & Calli
  --------------------------------------------------------------------------------------------------
  Revision History:
  date     op  method                       track-no improvement/change
  -------- --  ---------------------------  -------- -----------------------------------------------
  11.12.02 wl                               TN1345   initial version
  12.12.02 wl  gmGetTubeOrigin              TN1345   aus DBRack hierher verschoben
  12.12.02 wl  gmWriteTubeOrigin            TN1345   aus DBRack hierher verschoben
  27.12.02 wl                               TN1293.5 uses und WinlissyIniAccess geändert
  12.03.03 wl                               TN1293.5 uses posTools
  17.06.03 wl  gmWriteTubeOrigin            TN1501   --> PosinfoDataAdaptor
  26.06.03 wl  gmGetTubeOrigin              TN1501   benutzt PosinfoDataAdaptor-Methoden
  24.07.03 wl                               TN1501   Anpassung der Funktionen durch Abschaffung von gBalances
  05.08.03 wl  gmGetTubeOrigin              TN1536   --> PosinfoDataAdaptor
  02.09.03 wl  FBalanceRackType             TN1559   Char-Array durch string ersetzt
  09.09.03 wl  GetBalancePos                TN1581.5 --> ObjWorkb
  16.09.03 tbh LoadCarriersAndRacks         TN1594   kein Laden des Layouts bei Abbruch des Slave-Mode
  10.10.03 wl  SetBalanceType               TN1559   durch Umbau entstandenen Bug beseitigt
  18.12.03 wl                               TN1672   uses geändert
  17.02.04 pk  SetBalanceType               TN1749   SetTypeName new parameter
  20.04.04 wl  TWorkbenchExt.LoadCarriersAndRacks TN1788   TPRRackManager.Create ohne Parameter
  10.05.04 pk  CheckBalances                TN1889   use TXRackPosition instead of TRackIDPosition
  10.05.04 pk  SetupBalancePositions        TN1889.1 uses FindRackByName
  10.05.04 wl  SetBalanceType               TN1788   SetTubeData entfernt (Methode bewirkt nichts, wenn mit Parameter false aufgerufen)
  08.11.04 wl  CheckBalances                TN2213   Aufruf von GetBalancePos geändert
  07.12.04 wl                               TN2246.4 TWorkbenchExt umbenannt in TRunWorkbench
  07.12.04 wl                               TN2246.4 unit ObjWorkbExt umbenannt in RunWorkbench
  06.01.04 wl  TRunMain,gRunMain            TN2246.4  gemeinsame Basisklasse für Main-Fenster von Sampler und ZARunner
  06.01.05 wl  TSystemEvents                TN2246.4  von Objects hierher verschoben
  25.01.05 tbh TRunWorkbench.BalanceBlocked TN2293    liest ob auf der Waage ein Rack ist, dass Tür blockiert
  09.01.05 wl  TRunWorkbench.BalanceBlocked TN2293.1  durch Assigned-Abfrage wird Access Violation vermieden
  11.03.05 pk  gRunMain, TRunMain           TN2339.2  New : Create GUIManager
  17.03.05 pk  TRunCallStack                TN2352.1  New
  29.03.05 pk  evEndMethodOrScript          TN2362    CloseZipArchive not called
  07.04.05 pk  TRunMain                     TN2375.1  New functions : code from  TMainForm.FormCreate
  19.04.05 pk  TRunMain                     TN2393    Session functions from TMainForm
  22.04.05 pk  SessionSchedule              TN2393    New : reschedule sesssion
  07.06.05 pk  ToolsParserConversion        TN2449    New : call parser converter
  11.07.05 wl  TRunAppInterface             TN2498.1  neu, abgeleitet von TAppInterface (DllLoading.pas), ersetzt App_..-Funktionen aus LisGlobe
  16.09.05 wl                               TN2574    Umbau der ZARunner-Oberfläche
  10.10.05 wl  GetCurrentMethodOrSessionName TN2637.2  = GetCurrentMethodName als public class funtion
  08.11.05 wl                               TN2745    rmmUseScript entfernt (und alles was damit zu tun hat)
  14.11.05 wl  TRunWorkbench.SetBalanceType               TN2775    aufgeräumt
  14.11.05 wl  TRunWorkbench.SetupBalancePositions        TN2775    entfernt (wird jetzt nicht mehr gebraucht)
  24.11.05 pk                                TN2805    objsampl replaced by ZARunnerObjects
  30.11.05 wl  TRunWorkbench.PRRackManager   TN2818    entfernt
  22.12.05 pk                                TN2875    Various changes for UserProtection
  26.01.06 pk evTerminateUserInterrupt       TN2904    New param aIsError - to avoid checking GlobalErr.  Checking GlobalErr caused ZP02 to hang
  21.03.06 wl TRunAppInterface.ModalDialogIsOpened/-Closed  TN2967.1  neu: Um User-Protection-Abfrage kurzfristig abzuschalten
  21.03.06 wl TSystemEvents.evModalDialogShow/-Close        TN2967.1  neu: Um User-Protection-Abfrage kurzfristig abzuschalten
  15.05.06 pk TRunMain.SessionCreate         TN3081    Layoutname no longer needed
  01.12.06 pk TSystemEvents                  TN3441    System event functions ApplicationRequeust, ErrorMessage replaced by InterruptStart/Finish
  11.12.06 pk TSessionBuilderRun             TN3462    New
  15.12.06 pk EnableControlsDuringRun        TN3462    call globals.EnableFormControls
  15.12.06 pk TRunMain.SessionCreate         TN3462    set cursor to hourglass
  15.12.06 pk TRunMain.SessionBuild          TN3462    call EnableControlsDuringRun
  15.12.06 pk TRunMain.SessionStart          TN3462    call EnableControlsDuringRun
  03.01.07 pk TSystemEvents.Create           TN3479    call FindStateSignal and pass statesignal to TSystemEvents object
  29.01.07 pk TSessionBuilderRun.LoadLayout  TN3527    call InstReadLayoutNameForSession to get layoutname
  12.03.07 pk evStartMethodOrScript          TN3631    Start SoftwareProtection
  06.08.07 pk TSessionBuilderRun.LoadLayout  TN3818    Delete old run layout
  30.08.07 pk BCEqualToOther                 TN3840.1  from TWorkbench
  12.11.07 pk TSystemEvents.Create           TN3864    call new FindSoftwareProtectionDevice functions
  09.01.08 wl EndSlaveMode                   TN3972    entfernt
  20.06.08 pk TRunWorkbench                  TN4139    Code moved to ZARunnerObjects
  03.07.08 wl                                         TN4157
  11.07.08 wl InitUserProtection                      TN4164   von ObjModulA hierher
  30.07.08 pk LoadLayout                     TN4139    calls global.LoadRunLayout
  20.09.08 pk GenerateMultipipSeq            TN4139   disabled for now
  17.11.08 pk                                TN4280   ThreadAPI instead of ThreadManagerRun
  19.05.09 wl GetMaxTipVolume                TN4571   wieder implementiert
  31.08.09 pk GetDefinedValue                TN4753   code from DllLoading
  04.11.09 pk                                TN4843   Replace TObjectList, TStringList, TStrings with Generics Lists and TStringArray
  13.04.10 wl                               TN5044   uses StringUtilities
  12.08.10 wl  GetMaxTipVolume               TN5227   an TipSystem angepasst
  02.03.11 wl                                TN5491    zahlreiche Änderungen für neues DllInterface Version 7
  20.09.11 wl                                TN5723   an Änderungen angepasst
  28.10.11 wl  GetDefinedValue               TN5725   bei STR_DLLINT_DEFVALUE_IDENT_PAINTRACKPOS wird nur noch in der Highlight-Farbe gefärbt
  03.11.11 wl                                TN5725   CalcVolumeOfRackPos TRack statt TLayout
  15.12.11 wl  GetRackPos,PaintRackPos,GetCurrentVol  TN5767   neu: ersetzen GetDefinedValue
  30.07.13 wl                                TN6160   an TSystemEvents angepasst
  15.08.13 wl                                TN6223   uses geändert
  -------------------------------------------------------------------------------------------------- }

unit RunAppInterface;


interface


uses
    DllCall;

type
    TRunAppInterface = class(TAppInterface)
    public
        function GetRackPos(const aParams: TArray<string>): string; override;
        function PaintRackPos(const aParams: TArray<string>): string; override;
        function GetCurrentVol(const aParams: TArray<string>): string; override;
        function GetCurrentMethodName(): string; override;
        function GetCurrentRunName(): string; override;
        procedure ModalDialogIsOpened(); override;
        procedure ModalDialogIsClosed(); override;
        class function GetCurrentMethodOrSessionName(): string;
    end;


implementation


uses
    SysUtils,
    RackWell,
    AppSettings,
    ThrdMan,
    ThreadAPI,
    SystemEvents,
    GeneralTypes,
    StringUtilities,
    RackTypes,
    Rack,
    Layout,
    LayoutManager;

{ TRunDllLoading }

function TRunAppInterface.GetCurrentMethodName: string;
begin
    result := TRunAppInterface.GetCurrentMethodOrSessionName();
end;

class function TRunAppInterface.GetCurrentMethodOrSessionName(): string;
begin
    result := TThreadAPI.GetCurrentSourceDataName;
end;

function TRunAppInterface.GetCurrentRunName: string;
begin
    result := TThreadAPI.GetCurrentExeDataPathName;
end;

function TRunAppInterface.GetCurrentVol(const aParams: TArray<string>): string;
const
    STR_DELIM = ',';
var
    xRackPos: TXRackPosition;
    xVolume: double;
begin
    if not Assigned(TLayoutManager.Instance.CurrentLayout) then
        EXIT;
    xRackPos := TLayoutManager.Instance.CurrentLayout.FindXRackPos(aParams[1], StrToInt(aParams[2]));
    if not Assigned(xRackPos.Rack) then
        EXIT;
    xVolume := xRackPos.Rack.CalcVolumeOfRackPos(xRackPos.Pos);
    result := Format('%s' + STR_DELIM + '%d' + STR_DELIM + '%s' + STR_DELIM + '%d',
        [xRackPos.Rack.Name, xRackPos.Pos, '', Round(xVolume)]);
end;

procedure TRunAppInterface.ModalDialogIsOpened();
begin
    TSystemEvents.Instance.ModalDllMessageStart();
end;

function TRunAppInterface.PaintRackPos(const aParams: TArray<string>): string;
const
    STR_DELIM = ',';
var
    xRackPos: TXRackPosition;
begin
    if not Assigned(TLayoutManager.Instance.CurrentLayout) then
        EXIT;
    xRackPos := TLayoutManager.Instance.CurrentLayout.FindXRackPos(aParams[1], StrToInt(aParams[2]));
    if not Assigned(xRackPos.Rack) then
        EXIT;
    xRackPos.Rack.PaintTubePos(xRackPos.Pos, TRackWellDisplayType.Highlight);
    result := Format('%s' + STR_DELIM + '%d', [xRackPos.Rack.Name, xRackPos.Pos]);
end;

procedure TRunAppInterface.ModalDialogIsClosed();
begin
    TSystemEvents.Instance.ModalDllMessageFinish();
end;

function TRunAppInterface.GetRackPos(const aParams: TArray<string>): string;
const
    STR_DELIM = ',';
var
    xRackPos: TXRackPosition;
begin
    if not Assigned(TLayoutManager.Instance.CurrentLayout) then
        EXIT;
    xRackPos := TLayoutManager.Instance.CurrentLayout.FindXRackPos(aParams[1], StrToInt(aParams[2]));
    if not Assigned(xRackPos.Rack) then
        EXIT;
    result := Format('%s' + STR_DELIM + '%d', [xRackPos.Rack.Name, xRackPos.Pos]);
end;


end.
