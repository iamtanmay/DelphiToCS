{ --------------------------------------------------------------------------------------------------
  Ebene 5 (WinLissy)
  --------------------------------------------------------------------------------------------------
  spezifische (aus SAMINTF geerbte) Objekte für Projekt WinLissy
  --------------------------------------------------------------------------------------------------
  Datum    op  function/procedure           Änderung / Neuerung
  -------- --  ---------------------------  --------------------------------------------------------
  06.01.98 mo  TSamplerRack.sMouseUp        keine Kalkulation der letzten Matrixposition wenn DilRack
  TSamplerRack.sMouseMove       dito
  01.07.98 wl  TSamplerRack.sMouseUp        Actions 'MoveRack und 'ReadBarcode werden gleichbehandelt
  TSamplerCarrier.sMouseUp      dito
  13.07.98 wl                               uses main
  TSamplerCarrier.sMouseUp     PMethodname durch MethEditForm.medMethodname ersetzt
  21.07.98 wl                               uses Utility
  TSamplerRack.sMouseUp        'ReadE' hinzugefügt
  TSamplerCarrier.sMouseUp      dito
  30.07.98 wl                               MainForm.PLayoutName statt med..
  31.07.98 wl  TSamplerCarrier.sMouseUp     StringGrid1.DefaultRowHeight muß mind. 18 sein
  15.09.98 wl  TSamplerCarrier.Create       sMouseUp wird auch bei TSamplerCarrier.OnMouseUp aufgerufen
  TSamplerCarrier.sMouseUp     Sender ist nicht mehr eindeutig, deshalb Sender durch Matrix ersetzt
  16.09.98 wl  TSamplerCarrier.sMouseUp     global.LayoutName statt MainForm.PLayoutName
  TSamplerWorkbench.Load       ersetzt LoadLayout
  TSamplerWorkbench.CreateRack Erzeugt ein TSamplerRack
  TSamplerWorkbench.CreateCarrier  Erzeugt einen TSamplerCarrier
  24.09.98 wl  TSamplerCarrier.sMouseUp     Bezug auf MethEditForm nur wenn offen
  25.09.98 wl  alle Funktionen              MethEditFormOpen>0 durch MethEditForm<>nil ersetzt
  28.09.98 wl  TSamplerWorkbench.Load       Aufruf jetzt wie bei ObjWorkb
  01.10.98 wl                               neu: TSamplerGlobals = TMethodSampler (aus globals)
  05.10.98 wl  TSamplerGlobals.RunCreate     Abfrage auf ShowRunForm statt ShowRunFormOpen
  08.10.98 wl  TSamplerGlobals.NameAllowed   override statt virtual
  TSamplerCarrier.SelectCell   gelöscht (keine Funktion)
  20.10.98 wl  TSamplerGlobals.RunCreate     Workbench wird immer geladen
  TSamplerGlobals.NameAllowed   Abfrage auf FUseScript
  21.10.98 wl  TSamplerGlobals.AddScriptLayout   Fügt eine neue Layout-Script-Beziehung zur Script-Tabelle hinzu
  TSamplerGlobals.SendDDERunStatus  aus TEscThread: Sendet Run-Status an DDE
  02.11.98 wl  TSamplerRack.SetSlotWithoutDBChange  ersetzt SetSlot im Edit-Fenster: Layout.DB wird nicht mehr geändert
  03.11.98 wl  TSamplerRack.SetSlotWithoutDBChange  in try-except-Block (für Racknamen, die nicht im Layout existieren)
  06.11.98 wl  TSamplerGlobals.ChangeEditLabel   Ändert Menüpunkte in Main
  10.11.98 wl  TSamplerCarrier.sMouseUp          ruft SelectStackerSlot (TCarrier) auf
  11.11.98 wl                                    neu TSamplerThrMan
  20.11.98 wl  TSamplerGlobals.RunNameAllowed    FExistingMethod (Name geändert)
  09.12.98 wl  TSamplerCarrier.sMouseUp          wirre Numerierung des Slots endlich beseitigt
  16.12.98 wl  TSamplerRack.sMouseUp,sMouseMove  Rackname: Hint durch FName ersetzt
  13.01.99 mo  TSamplerGlobals.RunCreate         Erzeugen des runs in try...finally block gesetzt
  15.01.99 wl  TSamplerCarrier.sMouseUp          SelectStackerSlot als TWorkbench-Funktion
  28.01.99 wl  TSetupRack.Create                 ShowHint:=true entfernt
  TSetupCarrier.Create              dito
  01.02.99 wl  TSamplerThrMan.StopSamplerThread  LogPanel wird nach RUN wieder klein
  StartSamplerThread                von TSamplerThrMan nach TSamplerGlobals
  25.02.99 wl  TSetupRack.sMouseUp               SourceRack wird bei Action "Photo" eingetragen
  03.03.99 mo  TSamplerUser                      neue Instanz von TUser aus \Tools\UserObj.pas
  TSamplerUser.SetAccess            regelt den Userzugriff
  04.02.99 wl  TSamplerWorkbench.ResetAllRacks   setzt alle Racks auf Ursprungsposition
  TSamplerRack.ResetSlot            setzt Rack auf Ursprungsposition
  26.03.99 wl  TSamplerGlobals.RunCreate         nur bei Methoden wird das Layout neu geladen
  TSamplerGlobals.StartSamplerThread übergibt jetzt auch [mdInitAtEnd]
  19.05.99 wl  TSamplerThrMan.ThreadIsStarted/Stopped   benutzt TBasicThread
  TSamplerGlobals.RunCreate         neuer Aufruf von DeleteRunLayout
  29.07.99 wl  TSamplerThrMan                    TMoveThread statt TBasicThread
  12.08.99 wz                                    --> Strings in Ressourcen
  10.09.99 wl  TSamplerGlobals.StartSamplerThread  ResinDispense-Aufruf gelöscht
  08.10.99 wl  TSamplerCarrier.sMouseUp          Erweitert um Action 'ChkBC'
  11.10.99 wl  TSamplerCarrier.sMouseUp          'ChkB' und 'ChkE' statt 'ChkBC'
  12.10.99 wl  TSamplerCarrier.sMouseUp          Beim Editieren des Runs kann man neue Slots wählen
  13.10.99 wl  TSamplerGlobals.ChangeEditLabel   mit mnuMethodRunTable und mnuScriptRunTable
  TSamplerUser.SetAccess            mit mnuMethodRunTable und mnuScriptRunTable
  02.11.99 wl  TSamplerGlobals.AskClose          beim Start einer abgearbeiteten Methode erscheint editierbares Fenster
  16.11.99 wl  TSamplerGlobals.MethodAction      ruft einen TMethodActionThread auf  (SamThrd)
  24.11.99 mo  alle Funktionen                   Feldnamen geändert (sinnlos)
  25.04.00 mo  RunNameAllowed                    dmSampler.DBMETHOD.CancelRange eingebaut ( 2.Methode im Script wurde nicht gefunden )
  28.04.00 wl                                    uses ScheUtil
  09.05.00 wl  TSamplerGlobals                   aufgelöst --> ObjSampl (war zu kompliziert)
  31.05.00 wl  TSamplerRack.SetSlotWithoutDBChange,ResetSlot  --> ObjSampl (TGlobalRack)
  31.05.00 wl  TSamplerWorkbench.ResetAllRacks   --> ObjSampl (TGlobalWorkbench)
  31.05.00 wl  TSamplerGlobals.ChangeEditLabel   aus ObjSampl: Bezug auf Main und dbMTools hier
  31.05.00 wl  TSamplerGlobals.MethodRunCreate   aus dbMTools: VORÜBERGEHEND hier, soll aber in ObjSampl kommen
  05.06.00 wl  TSamplerGlobals.MethodRunCreate   --> ScheUtil
  05.06.00 wl  TSamplerGlobals.ResetVirtualRackMove  überschreibbare funktion (mit Abfrage auf MethEditForm)
  06.06.00 wl  TSamplerGlobals.UpdateTimeSched   aus ObjSampl genommen, weil Editor zunächst ohne ScheTime auskommt
  09.06.00 wl  TSamplerThrMan.UserInterrupt      Abbruch auch wenn ScheduleForm offen, aber gerade kein exclusiver Thread läuft
  20.06.00 wl  TSamplerWorkbench.Load            wenn Disp-Tips im Layout, wird Menu item 'Fill disp...' angezeigt
  21.06.00 wl  TSamplerGlobals.ChangeEditLabel,TSamplerUser.SetAccess   mit mnuScriptSaveAs
  21.06.00 wl  TSamplerGlobals.ChangeEditLabel   mit sbBuild
  18.07.00 wl  TSamplerGlobals.ChangeEditLabel   sbStart und sbEdit sind nur aktiv, wenn Script/Methode geladen
  21.07.00 wl  TSamplerCarrier.Create/sMouseUp   --> ObjSampl (TGlobalCarrier)
  01.09.00 wl  TEditorGlobals.CreateWorkbench   überschreibt Methode für LayOForm
  02.12.00 tbh TSamplerGlobals.ChangeEditLabel  Label für CherryPicking ergänzt
  28.08.01 tbh TSamplerThrMan.UserInterrupt     TN1021 Drücken des Stop-Buttons während Sampler-Run unterbricht Vortexerabfrage
  28.08.01 tbh TSamplerCarrier.sMouseUp         TN1008 Delimiter für MoveRack-Option Slot per Ini einstellbar
  05.09.01 tbh TSamplerThrMan.UserInterrupt     TN1031 Vortexer-Abfrage-Thread durch neue Funktionen gestoppt/fortgesetzt
  17.10.01 mo                                   TN1067 Änderungen für SIAS
  05.11.01 mo  ResetVirtualRackMove             TN1088 ResetVirtualRackMove nicht im Script ausführen
  26.04.02 tbh TSamplerThrMan.UserInterrupt     TN1052.1 SlaveModus wird beendet
  03.05.02 tbh TSamplerThrMan.UserInterrupt     TN1052.1 Aufrufnamen geändert
  01.07.02 pk                                   Changed resource numbers 55500-55521 => 55800-55821
  13.09.02 wl                                   TN1283 Merge mit SIAS
  10.10.02 wl                                   TN1293.2 Ini Access - uses geändert
  21.10.02 tbh TSamplerGlobals.ChangeEditLabel  TN1303 Editierfunktionen werden nur freigeschaltet wenn user Administrator ist
  12.12.02 wl  TSamplerWorkbench                TN1345 jetzt von TWorkbenchExt abgeleitet
  27.12.02 wl                                   TN1293.5 uses und WinlissyIniAccess geändert
  04.01.03 wl  gmUserSetAccess                  TN1334.1 einfacher Ersatz für SamplerUser
  31.01.03 wl  gmUserSetAccess                  TN1334.1 --> Main
  31.01.03 wl  TSamplerGlobals.ChangeEditLabel  TN1334.1 Abfrage der User-Rechte verbessert
  11.02.03 wl                                   TN1345 XAP-Compiler-Direktiven entfernt
  26.02.03 wl  TSamplerThrMan.UserInterrupt     TN1334.3 User Interrupt, Continue und Abort wird geloggt
  27.02.03 wl  TSamplerThrMan.ThreadIsStopped   TN1332.2 beim Beenden des (Method-)Run-Threads wird der ProcessLog geschlossen
  21.03.03 wl  TSamplerThrMan.ThreadIsStopped   TN1332.2 beim Beenden des (Method-)Run-Threads wird der ZipArchiv geschlossen
  31.03.03 mo  TSamplerThrMan.ThreadIsStopped   TN1383.8 Aufruf TGlobal.GlobalEvents
  31.03.03 mo  TSamplerThrMan.UserInterrupt     TN1383.8 Aufruf TGlobal.GlobalEvents
  07.04.03 wl  TGlobalEvents                    TN1383.8 von ObjSampl hierher verschoben
  10.04.03 wl  TSamplerThrMan.evCloseErrorMessage TN1332.4 böser Bug beseitigt
  14.04.03 tbh TGlobalEvents.evApplicationRequest TN1383.8 ruft jetzt UserProtection.ProtectionOff auf
  14.04.03 tbh TSamplerThrMan.UserInterrupt       TN1383.6-8 Event nur wenn Applikation läuft
  09.05.03 wl  TSamplerWorkbench                TN1490 verwaltet Menu items zum Füllen der Diti-Racks
  02.06.03 wl                                   TN1485.4 TAppSettings.IsSias ersetzt die ifdefs
  24.07.03 wl  TSamplerThrMan.GetCurrentRunName TN1515 bekommt Run-Namen von TRunThread.GetCurrentRunName
  24.07.03 tbh TSamplerThrMan.CurrentRunName    TN1515 jetzt property
  28.05.03 mo  TSamplerWorkbench.Load           TN1490 Füllen von Diti Racks funktioniert auch wenn nur 1 DitiTyp vorhanden ist
  12.09.03 wl  TSampler.SlotChosen (sMouseUp)   TN1581.4 Anpassung an Änderungen von TCarrier
  01.10.03 wl  TGlobalEvents.evShowErrorMessage TN1610   komplettes Backup vor dem Anzeigen des Fehlerfensters
  22.10.03 wl  TGlobalEvents.evShowErrorMessage TN1610   komplettes Backup --> RoboticInterfaceZP02
  30.10.03 wl  TSamplerRack                     TN1641 --> TGlobalRack (ObjSampl)
  30.10.03 wl  TSamplerCarrier                  TN1641 --> TGlobalCarrier (ObjSampl)
  30.10.03 wl  TSamplerWorkbench.CreateRack     TN1641 --> TGlobalWorkbench.CreateRack
  30.10.03 wl  TSamplerWorkbench.CreateCarrier  TN1641 --> TGlobalWorkbench.CreateCarrier
  31.10.03 wl  TSamplerGlobals.ResetVirtualRackMove  TN1641   --> TGlobals.ResetVirtualRackMove
  08.12.03 pk  TSamplerThrMan.UserInterrupt     TN1697 uses new ThrMan functions to SendDDEMessage and suspend/resume animation thread
  04.02.04 pk  TSamplerThrMan                   TN1719 ThreadIsStarted/Stopped changed to accept TThread instead of TRunThread
  17.02.04 pk  TSamplerGlobals.CreateWorkbench  TN1749 ChangeVisible called after Workbench is created
  23.02.04 pk                                    TN1719 GlobalEvents renamed to SystemEvents and inherits from SystemEventsMinimal
  26.02.04 wl                                    TN1574   uses SamLiqH entfernt
  19.04.04 wl  TSamplerThrMan.CurrentRunName    TN1788   entfernt (wurde nicht benutzt)
  28.06.04 pk                                   TN2009.7 UserInterrupt uses CreateVortRegThread from DevicesVortex
  30.09.04 wl                                   TN2157   Anpassung an geänderte SystemState-Bezeichnungen
  02.11.04 mo  TSamplerThrMan.UserInterrupt     TN2212 Aufruf von .evTerminateUserInterrupt korrigiert
  11.11.04 wl  TSamplerGlobals.ChooseCarrierSlot TN2213   von ObjSampl hierher verschoben
  11.11.04 wl  TSamplerRack.MenuPopup,EditTubeContent,CreatePopup  TN2213  von ObjSampl hierher verschoben
  11.11.04 wl  TSamplerWorkbench.CreateRack      TN2213    von ObjSampl hierher verschoben
  07.12.04 wl                                    TN2246.4  uses ObjectsRun statt ObjWorkbExt
  06.01.05 wl  TSystemEvents                     TN2246.4  --> ObjectsRun
  06.01.05 wl  TSamplerThrMan                    TN2246.4  als TThreadManagerRun --> ThrdManExt
  06.01.05 wl  TSamplerGlobals.ChooseCarrierSlot TN2246.4  entfernt
  06.01.05 wl  TZARunnerRunMain                  TN2246.4  neu: abgeleitet von TRunMain
  06.01.05 wl                                    TN2213    entspricht Objects.pas
  17.01.05 wl                                    TN2246.4  uses ObjectsShowRun
  28.02.05 pk   ChangeEditLabel                  TN2315    set properties for Session controls
  11.03.05 pk  TZARunnerRunMain.Create           TN2339.2  New : calls inherited
  07.04.05 pk  EnableScheduler                   TN2375.1  Removed
  19.04.05 pk  CreateThreadManager               TN2392    No longer create TThreadManagerExt
  22.04.05 pk  ChangeEditLabel                   TN2393    enable mnuSessionReschedule
  22.08.05 wl                                    TN2558.8  uses ScheUtil entfernt, ScheUtil-Methoden werden mit TDMScript. oder global. aufgerufen
  16.09.05 wl                                    TN2574    Umbau der ZARunner-Oberfläche
  07.11.05 pk  ChangeEditLabel                   TN2737    Build button not Enabled for Session
  08.11.05 pk  CloseEditForms                    TN2744    Close all run forms
  08.11.05 wl  TZARunnerGlobals.UpdateSessionState  TN2745   --> ObjSampl
  24.11.05 pk  MASSIVE CHANGES                   TN2805    ObjSampl functions divided among ZARunnerObjects and ZADesignObjects
  30.11.05 wl  NameAllowed                       TN2815    --> ZADesignMain
  22.12.05 pk  TZARunnerRunMain.CreateThreadManager TN2875    CreateThreadManager has fewer params
  22.12.05 pk  TZARunnerRunMain.ThreadStopped;   TN2875    evEndMethodOrScript no longer called
  06.04.06 pk  TZARunnerGlobals.RunCreate        TN3001    New parameter : current priority fur scheduled thread
  11.04.06 pk  MethodLayoutNameAllowed           TN3001    Caused access violations when two threads called function at same time
  15.05.06 pk  RunNameAllowed                    TN3081    do not use global instance of MethodDataAdaptor
  15.05.06 pk  TZARunnerGlobals.RunCreate        TN3097    pass nil as new build parameter
  05.07.06 pk  TZARunnerGlobals.RunCreate        TN3181    names of parameters changed (i.e Paint changed to IsDesignTimeBuild)
  19.09.06 pk  GuiRunCreate                      TN3228    calls checkbeforebuild
  26.09.06 wl  StartMethodRun                    TN3326    ruft gmExecuteRunStartWithName auf
  11.12.06 pk  RunCreate                         TN3462    New param aNamePriorityList
  11.12.06 pk  ChangeEditLabel                   TN3462    Session Build is enabled again
  15.12.06 pk  EnableFormControls                TN3462    New, code taken from changeeditlabel
  19.12.06 wl  TZARunnerGlobals.EnableFormControls  TN3409    User management implementiert
  29.01.07 pk  ChangeGlobalName                  TN3527    read layoutname using ReadLayoutNameForSession
  20.02.07 wl  TZARunnerGlobals.RunCreate        TN3016    ParserType old entfernt
  30.08.07 pk                                    TN3840.1  use TLayoutDataAdaptorExt
  03.09.07 pk  RunNameAllowed                    TN3847    uses TMethodDataAdaptorExt
  09.11.07 pk                                    TN3922    references to Dataset changed to DataProvider
  09.01.08 wl  EndSlaveMode                      TN3972    entfernt
  20.06.08 pk                                    TN4139    Layout Objects: Various changes
  27.06.08 pk                                    TN4139    GetBalancePos moved from Layout to ZARunnerLayout
  02.07.08 pk  TZARunnerLayout.ChangeRackID      TN4139    code from TRack
  03.07.08 wl                                    TN4157
  09.07.08 pk                                    TN4139    ShowPositioninfo parameters changed
  16.07.08 pk  RunCreate                         TN4139    changes needed for reading racks from runlayout
  21.07.08 pk  UpdateCarrierRunType              TN4179    New
  30.07.08 pk  LoadLayout, LoadRunLayout         TN4139    New
  31.07.08 pk  UpdateRackRunData                 TN4139    New: Code from TLayout
  31.07.08 pk  UpdateRackRunType                 TN4193    New
  01.08.08 pk  RunCreate                         TN4193    do NOT load runlayout. It will be loaded before starting run
  09.12.08 pk                                    TN4279    uses changed
  14.04.09 pk  CanSimulateInRealMode             TN4512    New
  17.06.09 wl                                    TN4612   uses ObjSampl entfernt
  10.08.09 wl                                    TN4702   Strings werden jetzt direkt geladen
  31.08.09 pk  Create                            TN4753   gmReadGlobasIniFile removed
  28.09.09 pk  FGlobalEdit, FGlobalLabel         TN4753   removed
  09.04.10 pk                                    TN5042   functions that were in RunMain now accessible only via GUIManager
  06.05.10 wl                                    TN5086   SimBeforeStart entfernt
  27.07.10 wl                                    TN5123   GetArchiveRunStep entfernt
  28.11.10 wl  TZARunnerGlobals.ReadRunIniFile   TN5355   Delay-Settings werden hier nicht mehr gelesen
  17.11.11 wl  EditRunSlot                       TN5729   entfernt
  14.12.11 wl  ReloadRunLayoutAndDeletePosinfo   TN5765   von LayoutDataAdaptorExt hierher
  14.12.11 wl                                    TN5765   Session entfernt
  27.12.11 wl                                    TN5768   viel unnötiges entfernt
  10.01.12 ts  ChangeGlobalName                  TN5776   wenn kein Methodenname angegeben, dann wird aLayout trotzdem geladen
  07.08.12 wl  RunNameAllowed                    TN5946   entfernt
  07.08.12 wl  ChangeValues                      TN5946   keine Anzeige des temporären Methodennamens
  20.06.13 ts                                    TN6146   TLayoutDataAdaptorExt.cTemporaryMethodName anstatt TMethodDataAdaptorExt.cTemporaryMethodName
  30.08.13 wl                                    TN6236   uses geändert
  -------------------------------------------------------------------------------------------------- }

unit RunGlobals;


interface


uses
    LayoutDataAdaptorExt;

type
    TRunGlobals = class
    private
        class var uInstance: TRunGlobals;
        function ChangeValues(const aNewMethod, aNewLayout: string; aReload: boolean): boolean;
        procedure NoLayout;
    protected
        // Grundeinstellungen
        FInitAtMethodEnd, fCanSimulateInRealMode: boolean;

        // globaler Methoden- und Layoutname
        FLayoutName: string;
        fMethodName: string;

        procedure ReadRunIniFile();
    public
        constructor Create();
        class procedure CreateInstance;
        class procedure DestroyInstance;

        function ChangeGlobalName(const aNewMethod, aNewLayout: string;
            aNewAllowed, aReload: boolean): boolean;
        function ReloadValues: boolean;

        property LayoutName: string read FLayoutName;
        property PMethodName: string read fMethodName;

        property InitAtMethodEnd: Boolean read FInitAtMethodEnd;
        property CanSimulateInRealMode: boolean read fCanSimulateInRealMode;
        class property Instance: TRunGlobals read uInstance;
    end;


implementation


uses
    Windows,
    SysUtils,
    MethodSettingsDataAdaptor,
    MethodDataAdaptorExt,
    LayoutDataAdaptor,
    LayoutManager,
    GUIManager,
    GeneralTypes,
    CommonTypes,
    AppSettings,
    GUIManagerRun;

{ TRunGlobals }

constructor TRunGlobals.Create();
begin
    inherited Create();
    ReadRunIniFile();
end;

class procedure TRunGlobals.CreateInstance;
begin
    uInstance := TRunGlobals.Create;
end;

class procedure TRunGlobals.DestroyInstance;
begin
    FreeAndNil(uInstance);
end;

function TRunGlobals.ReloadValues: boolean;
begin
    result := ChangeValues(fMethodName, FLayoutName, true);
end;

function TRunGlobals.ChangeValues(const aNewMethod, aNewLayout: string; aReload: boolean): boolean;
begin
    result := false;
    if (FLayoutName <> aNewLayout) or (fMethodName <> aNewMethod) or (aReload) then
    begin
        try
            // Wenn das Layout geladen werden kann, werden die neuen Daten gesetzt
            FLayoutName := aNewLayout;
            fMethodName := aNewMethod;
            if (fMethodName = TLayoutDataAdaptorExt.cTemporaryMethodName) then
                // Sonderfall: temporären Methodennamen nicht anzeigen
                TGUIManagerRun.Instance.SourceLabel_Change('', '')
            else
                TGUIManagerRun.Instance.SourceLabel_Change(TLanguageString.Read('Method:', 'Methode:'),
                    fMethodName);
            result := true;
        except
            NoLayout;
        end;
    end
    else
        result := true;
end;

procedure TRunGlobals.NoLayout;
begin
    FLayoutName := '';
    fMethodName := '';
    TGUIManagerRun.Instance.SourceLabel_Change('...', '');
end;

function TRunGlobals.ChangeGlobalName(const aNewMethod, aNewLayout: string;
    aNewAllowed, aReload: boolean): boolean;
var
    xDA: TMethodSettingsDataAdaptor;
    xRec: TMethodSettingsRec;
begin
    result := false;
    if (aNewMethod = '') then
    begin
        ChangeValues('', aNewLayout, aReload);
        EXIT;
    end;

    xDA := TMethodSettingsDataAdaptor.Create;
    try
        xRec := xDA.ReadRec(aNewMethod);
    finally
        FreeAndNil(xDA);
    end;

    if (xRec.MethodName <> '') then
    begin // Methode gefunden
        result := ChangeValues(aNewMethod, xRec.LayoutName, aReload);
    end;
end;

procedure TRunGlobals.ReadRunIniFile;
var
    xIniAccess: IWinLissyIniAccess;
begin
    inherited;
    xIniAccess := gCommonDll.CreateAppIni;
    FInitAtMethodEnd := xIniAccess.ReadBool('Display', 'InitAtMethodEnd');
    fCanSimulateInRealMode := xIniAccess.ReadBool('Display', 'CanSimulateInRealMode');
end;


end.
