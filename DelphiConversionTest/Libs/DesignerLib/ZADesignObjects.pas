unit ZADesignObjects;
{ --------------------------------------------------------------------------------------------------
  EDITOR (Ebene 5)
  --------------------------------------------------------------------------------------------------
  Globale Variablen, Funktionen und Prozeduren für den Editor
  --------------------------------------------------------------------------------------------------
  Datum    op  function/procedure     Änderung / Neuerung
  -------- --  -------------------    --------------------------------------------------------------
  31.05.00 wl                         als EdObject in EDITOR
  05.07.00 wl                         Änderungen durch Abspaltung von EdScrip
  19.07.00 wl  TEditorGlobals.ChangeEditLabel   Änderungen durch geändertes Layout
  19.07.00 wl  TEditorWorkbench.Load            Beschreibt Titelleiste von Layout-Fenster
  21.07.00 wl  TEditorWorkbench.CreateCarrier   TGlobalCarrier statt TEditorCarrier
  10.08.00 wl  TEditorGlobals.ChangeEditLabel   kein Reset-Button mehr
  01.09.00 wl  TEditorGlobals.ChangeEditLabel   neue Buttons in Main, ScheEdit statt EdScrip
  01.09.00 wl  TEditorGlobals.CheckLastScript   vorgesehen: Treeview-Item muß noch gelöscht werden
  01.09.00 wl  TEditorGlobals.CreateWorkbench   überschreibt Methode für LayOForm
  02.09.02 pk  TEditorGlobals.ChangeEditLabel   Took out Layout.Show
  29.10.03 wl  TEditorGlobals.ChangeEditLabel   TN1641 mit allen neuen Menüpunkten und User Management
  28.04.04 wl  TMinimalFakeInterface            TN1788   von MinimalInterface hierher verschoben
  28.04.04 wl  TCommunicationFakeManager        TN1788   von Communication hierher verschoben
  28.04.04 wl  TEditorPip/GrpArmDevice          TN1788   neu: auch im Editor muß es Arm-Instanzen geben (sonst geht nichts)
  28.04.04 wl  TMinimalFakeInterface.ReadZP02CfgIni          TN1788.9 liest primitive Daten aus SiasCfg.ini, um Build Run auch sinnvoll für ZP02 auszuführen
  04.05.04 wl  GetTubeHandlingPossible                       TN1788   bisher: GipperArm true - PipArm false
  10.05.04 wl  TEditorModules.CreateTipManager               TN1788   an Änderungen von TCustomArmDevice angepasst
  10.05.04 wl  TMinimalFakeInterface.PArm_TipsHave2Dilutors  TN1788   gibt es an einem Tip 2 Dilutoren?
  10.05.04 wl  TMinimalFakeInterface.ReadZP02CfgIni          TN1788.9 Aufruf ohne Paramter
  08.06.04 wl  TEditorPip/GrpArmDevice           TN1963  an Änderungen von TRobotArmDevice angepasst
  08.06.04 wl  TEditorModules.AddGripperArm      TN1963    neu: Erzeugen von GripperArmDevice
  08.06.04 pk  TEditorGlobals.CheckLastScript    TN1974.0  no result needed
  08.06.04 wl  TEditorModules.AddGripperArm      TN1963    Fehler beim aufruf von AddArm korrigiert
  15.06.04 wl  TMinimalFakeInterface.GetHYOffset,HROffset,HXMax,HXOffset  TN1963 entfernt
  07.07.04 wl  TEditorModules.AddGripperArm      TN1963    erneute Anpassung an geänderten Create-Aufruf
  06.08.04 wl  TEditorGlobals.ChangeEditLabel    TN2008.1  erzeugt TMDIMethEditForm und TMDIScheduleEditForm
  06.08.04 wl  TEditorGlobals.ChangeEditLabel    TN2008.2  ruft EdMain-Methoden auf, um MethEdit, ScheEdit zu erzeugen
  17.08.04 wl  TEditorRack                       TN1958.2  Menu zeigt auch "Edit Tube Content"
  19.08.04 wl  TEditorMethEditForm.AddAction     TN2008.3  ruft das Property-Fenster für eine Action auf
  10.09.04 wl  TEditorModules                    TN2123    Erzeugen der Arme: Parameter geändert
  24.09.04 wl  TEditorGlobals.ChooseRack                 TN2008.2  Auswahl eines Racks für ActionProps oder Methodeneditor
  24.09.04 wl  TEditorGlobals.ChooseCarrierSlot          TN2008.2  Auswahl eines Slots für ActionProps oder Methodeneditor
  24.09.04 wl  TEditorGlobals.UpdateActionProps          TN2008.2  Parameter-Fenster wird upgedated
  30.09.04 wl                                            TN2157   Anpassung an Änderungen
  03.11.04 wl  TEditorGlobals.ChooseRackPositions        TN2008.2  Auswahl von Rackpositionen für ActionProps oder Methodeneditor
  11.11.04 wl  TEditorGlobals.ChooseCarrierSlot  TN2213   von ObjSampl hierher verschoben
  11.11.04 wl  TEditorRack.MenuPopup,EditTubeContent,CreatePopup  TN2213  von ObjSampl hierher verschoben
  03.12.04 wl                                    TN2008.2  alles bezüglich Script entfernt
  13.12.04 wl  TEditorGlobals.ChangeEditLabel    TN2254.4  hinzugefügt: frmEdMain.mnuMethodSaveAs.Enabled:=false
  21.12.04 wl                                    TN2247.1  durch XP-Manifest bedingte  Änderungen
  22.12.04 wl  TMinimalFakeInterface,..          TN2246.4  -> ObjectsEdit
  11.03.05 pk  TZADesignEditMain                 TN2339.2  New
  15.06.05 wl                                    TN2465    uses dbTools
  22.06.05 wl                                    TN2440    uses MethodEditor
  05.08.05 wl  TZADesignLinkAll.TalkReSchedule   TN2501.1  ruft frmEdMain.ReSchedule auf
  10.08.05 wl                                    TN2501.1  EdExplorer in ViewAllItems umbenannt
  22.08.05 wl                                    TN2558.8  uses ScheUtil entfernt, ScheUtil-Methoden werden mit TDMScript. oder global. aufgerufen
  24.08.05 wl  UpdateSessionState                TN2558.8  von ObjectsShowRun hierher
  25.08.05 wl  TZADesignLinkAll                  TN2558.8  ersetzt TEdMainLnkAll (unit LnkEditorMainLinkAll)
  25.08.05 wl  TZADesignFrmScheduleChart         TN2558.8  ersetzt TSchedChartEdit (unit ImplSchedChartEditor)
  05.09.05 wl                                    TN2541.4  uses ViewReagentsOfLayout
  07.09.05 wl                                    TN2558.9  Setzen von ParentBackground -> Rack,Carrier,ObjWorkb,ObjArm
  08.09.05 wl  AddMethodIcon                     TN2595    an Änderungen von AddChildAsFirstOrLast angepasst
  16.09.05 wl  TZADesignRack.TubeContent         TN2574    von ObjSampl hierher
  05.10.05 wl  TZADesignLinkAll.EditMethodComments  TN2575  ruft Wizard mit MethodCommentPage auf
  05.10.05 wl  TZADesignLinkAll.SaveMethodComments  TN2575  koordiniert das Update der Daten und der Anzeige nach dem Ändern der Kommentare
  10.10.05 wl  TZADesignLinkAll.CheckBeforeBuild    TN2637.1 öffnet MethodBuildPage
  07.11.05 pk  TZADesignLinkAll                     TN2737   Changes for Dynamic Scheduling
  08.11.05 wl  TZADesignGlobals.UpdateSessionState  TN2745   --> ObjSampl
  09.11.05 pk  TZADesignLinkAll                     TN2737   Changes to SchedChart functions
  24.11.05 pk  MASSIVE CHANGES                   TN2805    ObjSampl functions divided among ZARunnerObjects and ZADesignObjects
  05.01.06 pk  TZADesignLinkAll.EditMethodComments TN2873  When cancel is pressed the comment is not saved
  20.01.06 pk  TEditorGlobals.ChooseRackPositions  TN2891  also pass first and last pos as integer
  04.03.06 wl  TZADesignRack.EditTubeContent       TN2954   geänderter Aufruf
  06.04.06 pk  TZADesignGlobals.Build              TN3001   Pass 0 as priority
  09.05.06 pk  TZADesignFrmScheduleChart.EditElement TN3091 Open run table for group
  15.05.06 pk  TZADesignGlobals.Build                TN3097 New param: NamePriorityResultList
  19.05.06 wl  TZADesignLinkAll.ChangeAttribute      TN3109   Ändert ein Methoden-Attribut
  13.06.06 pk  TZADesignFrmScheduleChart.FormLiveOpen TN3150  ViewModes := [osBottom]
  05.07.06 pk  Build                               TN3181   names of parameters changed (i.e Paint changed to IsDesignTimeBuild)
  19.09.06 pk  CheckBeforeBuild                    TN3228   moved to TGlobals
  27.11.06 wl                                      TN3411   uses MethPrn entfernt
  20.02.07 wl  TZADesignGlobals.Build              TN3016    ParserType old entfernt
  06.03.07 wl  TZADesignEditMain                   TN3620   entfernt
  20.03.07 pk  TZADesignRack.sMouseMove            TN3579   The method property grid is no longer update on mousemove because it disrupted the selection
  07.01.08 pk  TZADesignWorkbench.CreateArm        TN3864   New
  20.06.08 pk  TZADesignWorkbench                  TN4139   Layout objects moved to ZADesignLayout
  03.07.08 wl                                         TN4157
  08.09.08 pk  TZADesignGlobals                    TN4215  Various changes.  Build replaced by Compile
  17.09.08 wl                                      TN4224   uses TipsetExt entfernt
  09.12.08 pk                                      TN4279   uses changed
  16.01.09 wl                                      TN4362   an Änderungen in TViewItem angepasst
  10.06.09 pk  Compile                             TN4600   TCompilerMessageList changed to TCompilerResult
  16.06.09 wl                                      TN4606   alle Bezüge auf ntRunTable und RunEditor entfernt
  17.06.09 wl                                      TN4612   uses ObjSampl entfernt
  03.08.09 wl                                      TN4702   uses MethodBuildPage entfernt
  10.08.09 wl                                      TN4702   Strings werden jetzt direkt geladen
  21.08.09 wl                                      TN4702   uses RessourceLoader entfernt
  27.08.09 pk  CreateLayoutManager                 TN4753   removed
  04.11.09 pk                               	  TN4843   Replace TObjectList, TStringList, TStrings with Generics Lists and TStringArray
  06.05.10 wl  TZADesignGlobals                    TN5052   --> EditingWorkflow
  07.05.10 wl                                      TN5052   UseScheduler boolean statt byte
  28.05.10 wl                                      TN5116   uses geändert
  09.06.10 wl  TZADesignLinkAll                    TN5116   ChangeComments --> MethodEditor
  14.12.11 wl                                      TN5765   ohne Session
  -------------------------------------------------------------------------------------------------- }


interface


uses
    Forms,
    Controls,
    StdCtrls,
    Rack,
    Carrier,
    LayoutManager,
    LnkCommonLinkAll,
    AppTypes,
    SchedMakerClasses,
    UFrmScheduleChart,
    MethodSettings,
    TipSystem,
    MethodCompile,
    GeneralTypes;

type
    TZADesignLinkAll = class(TCommonLinkAll)
    private
        class function SchedChartCreate(aParent: TWinControl; aSession: TSchedMakerSession): TForm;
    public
        class function SchedChartInstance(aParent: TWinControl; aSession: TSchedMakerSession;
            aNewInstance: boolean): TForm;
        class procedure TalkSchedChartClose; override;
        class procedure SchedChartShow(aInstance: TForm; aLiveInfoMode: boolean);
        class function ChangeAttribute(const aMethodName: string;
            aAttributeType: TMethodSetAttribute): boolean;
        //
        procedure UpdateEdMainTreeMethod();
    end;

    TZADesignFrmScheduleChart = class(TFrmScheduleChart)
    protected
        procedure SetFormStyle(); override;
    public
        constructor Create(aSession: TSchedMakerSession); reintroduce; virtual;
        destructor Destroy; override;
        //
        procedure FormOpen(); override;
        procedure EditElement(aChartID: integer); override;
        procedure FormLiveOpen();
    end;


implementation


uses
    SysUtils,
    ZADesignMain,
    AppSettings,
    ImplMainSchedChart,
    ViewItem;

{ TZADesignLinkAll }

procedure TZADesignLinkAll.UpdateEdMainTreeMethod();
begin
    frmEdMain.UpdateTV([ntMethod]);
end;

class function TZADesignLinkAll.SchedChartCreate(aParent: TWinControl; aSession: TSchedMakerSession): TForm;
begin
    result := nil;
    if not TAppSettings.UseScheduler then
        Exit;
    result := TZADesignFrmScheduleChart.Create(aSession);
    result.ManualDock(aParent);
    result.Show;
end;

class function TZADesignLinkAll.SchedChartInstance(aParent: TWinControl; aSession: TSchedMakerSession;
    aNewInstance: boolean): TForm;
begin
    result := nil;
    if not TAppSettings.UseScheduler then
        Exit;

    if aNewInstance then
    begin
        result := SchedChartCreate(aParent, aSession);
    end
    else
    begin
        if not Assigned(frmScheduleChart) then
            frmScheduleChart := SchedChartCreate(aParent, aSession) as TfrmScheduleChart;
        result := frmScheduleChart;
    end;
end;

class procedure TZADesignLinkAll.TalkSchedChartClose;
begin
    if not TAppSettings.UseScheduler then
        Exit;
    if frmScheduleChart = nil then
        Exit;
    frmScheduleChart.FormFinish();
end;

class procedure TZADesignLinkAll.SchedChartShow(aInstance: TForm; aLiveInfoMode: boolean);
begin
    if aLiveInfoMode then
        (aInstance as TZADesignFrmScheduleChart)
            .FormLiveOpen else (aInstance as TZADesignFrmScheduleChart).FormOpen;
end;

class function TZADesignLinkAll.ChangeAttribute(const aMethodName: string;
    aAttributeType: TMethodSetAttribute): boolean;
var
    xAttributes: TMethodSetAttributes;
begin
    xAttributes := TMethodSettings.GetAttributes(aMethodName);
    if (aAttributeType in xAttributes) then
        xAttributes := xAttributes - [aAttributeType]
    else
        xAttributes := xAttributes + [aAttributeType];

    result := (aAttributeType in xAttributes);
    TMethodSettings.SetAttributes(aMethodName, xAttributes);

    frmEdMain.UpdateAttributes(aMethodName, xAttributes);
end;

{ TZADesignFrmScheduleChart }

constructor TZADesignFrmScheduleChart.Create(aSession: TSchedMakerSession);
begin
    inherited Create(aSession);
    fElementEditAllowed := true;
    // frmScheduleChart := self;
end;

destructor TZADesignFrmScheduleChart.Destroy;
begin
    frmScheduleChart := nil;

    inherited;
end;

procedure TZADesignFrmScheduleChart.FormLiveOpen();
begin
    ViewModes := [osBottom];
    self.DrawMode := dmLiveInfo;
    self.Draw();
end;

procedure TZADesignFrmScheduleChart.FormOpen();
begin
    ViewModes := [osBottom];
    self.DrawMode := dmDesign;
    self.Draw();
end;

procedure TZADesignFrmScheduleChart.SetFormStyle();
begin
    self.FormStyle := fsStayOnTop;
end;

procedure TZADesignFrmScheduleChart.EditElement(aChartID: integer);
// const
// BOOL_SHOWRUN_AUTO_FREE_DATASET = true;
// var
// xElement : TSchedMakerRunStep;
// xName : string;
begin
    // xElement := fMainSchedChart.GetElementData( aChartID ) as TSchedMakerRunStep;
    { TODO -oPK -cAction package : }
    // xName := Format( '%s;%s;%d;%s', [ xElement.RunStep.Data.RunName, xElement.RunStep.Data.GroupID, xElement.RunStep.Data.Priority, xElement.RunStep.Description ] );
    // TViewItemsWorkflow.OpenEditForm( xName, ntGroupTable );
end;


end.
