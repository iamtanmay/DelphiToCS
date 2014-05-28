{ --------------------------------------------------------------------------------------------------
  LISSY LAYOUTER
  --------------------------------------------------------------------------------------------------
  Hauptfenster
  --------------------------------------------------------------------------------------------------
  Datum    op  function/procedure     Änderung / Neuerung
  -------- --  ---------------------  --------------------------------------------------------------
  20.11.97 mo  Hardlock               Hardlockabfrage eingebunden
  26.11.97 mo                         Hardlockabfrage über IFDEF.. Hardlock
  17.06.98 wl                         Menü: Movement, MovePlates in Tools zusammengefaßt
  Init1 entfernt
  RepairDatabaseIndex1Click  Ndxrebu wird erst zur Laufzeit erzeugt
  16.07.98 wl  FormCreate             global-Objekt wird erzeugt
  21.07.98 wl  FormCreate             HardlockInit --> SamIntf
  22.07.98 wl                         global.Create entfernt (und uses ObjSampl)
  18.09.98 wl  Laden2Click            Aufruf geändert: SamplerCarrierA[C].LoadRack
  SaveLayOut             Geändert (entspr. Objects): SamplerCarrierA[C]....
  02.11.98 wl  FormCreate             InitSamGlobals statt InitIntf
  05.01.99 wl  mnuFileDelete          stark vereinfacht
  CloseAllChilds         beim Schließen der Fenster wird SLayout=nil gesetzt
  07.01.99 wl  CloseAllChilds         beinhaltet jetzt die AskForSave (aus LayOSet)
  11.02.99 wl  FormClose              Beim Schießen des Programms wird nach Save gefragt
  09.03.99 wl  alle Methoden          Hardlock-Abfragen durch User-Funktionen ersetzt
  mnuToolsSimulationClick  wie in WinLissy
  16.03.99 wl  mnuFileSaveAsClick,mnuFileNewClick   Name wird auf gMaxNameLen.LayoutName gekürzt
  29.04.99 wl  FormShow               kein zwanghaftes Initialisieren mehr am Anfang
  mnuToolsMovePlatesClick  zu Beginn wird nur noch initialisiert, wenn GlobalErr<>ERR_NONE
  mnuToolsInitClick      --> gmInit (globals)
  18.05.99 wl  FormCreate             SetGlobalErr( 0;
  18.08.99 wz                         --> Strings in Ressourcen
  14.02.00 wl  FormCreate             LogText() durch gmLogText() ersetzt (für Delphi 5)
  neu: Popup-Menü für Explorer-Ansicht, zum Teil funktionsfähig
  15.02.00 wl  NewRack,NewCarrier     Definieren neuer Racks und Carrier wieder möglich
  23.02.00 wl  pmnuSaveAsClick        'Speichern unter...' für Layouts, Rack und Carriers wieder möglich
  pmnuDeleteClick        Löschen von Layouts, Rack und Carriers wieder möglich
  RCRefresh              Nach Änderungen wird Rack/Carrier neu geladen
  16.03.00 wl                         TV.ReadOnly gesetzt: Namen sollen nicht verändert werden
  16.03.00 wl  rbMoveClick            MoveRack-Layout: Reihenfolge geändert -> keine Exceptions mehr
  21.03.00 mo                         TV.DragMode auf dmAutomatic
  03.04.00 wl  rbMoveClick            Move-Layout wird jedesmal gelöscht (Fehlerquelle)
  NewRack,NewCarrier     namenlose Items werden nicht mehr erzeugt
  15.05.00 wl                         geänderte Ansicht: Layout oben / Rack oder Carrier unten
  OpenRack/OpenCarrier   Rack oder Carrier werden neu geladen
  rbRCEditClick          Layout wird gespeichert, der Edit-Mode aber nicht verlassen
  15.05.00 wl                         neu: Oberfläche mit Buttons (auch Edit Tip Configuration)
  15.05.00 wl                         Schöne Icons für Treeview
  16.05.00 wl  NewRack,SaveAsClick,.. Neue Treeview-Items haben jetzt auch das richtige Icon
  16.05.00 wl  sbLTestClick           Bei jedem Laden des Layouts werden die Motorpositionen upgedated
  17.05.00 wl  mnuTest                neu Test-Menü für alle Test-Fenster (neu: Dll-Testfenster)
  22.05.00 wl                         TLayouterThread statt TMoveCommands
  24.05.00 wl  CloseWB                ruft WB.ApplicationEnd auf, um evtl. geänderte Border-/Zoom-Werte zu speichern
  25.05.00 wl  StopBtnClick           Stop-Button für User Interrupt
  25.05.00 wl  mnuToolsInitClick      ruft TMoveThread statt TLayouterThread auf
  26.05.00 wl  mnuTestReadBCClick     WinLissy-Funktion, jetzt als Test im Layouter
  26.05.00 wl  FormCreate             gLiquids werden jetzt auch hier erzeugt
  26.05.00 wl  OpenLayout             Test-Layout wird mit Run-Namen erzeugt
  31.05.00 wl  FormShow               GetPositions wird beim Start noch nicht aufgerufen (Machine-Error)
  05.06.00 wl  FormCreate             Aufruf von InitWinLissyProjects für Seriennummer
  07.06.00 wl  OpenLayout             GetPositions nicht mehr nötig
  20.06.00 wl                         neues Panel für Waagen-Anzeige & Waage-Grundfunktionen
  25.07.00 wl  pmnuVirtualCarrierClick  automatisches Erstellen eines virtuellen Carriers (Höhe=0)
  10.08.00 wl                         --> String in Ressourcen
  06.09.00 wl                         TreeView mit Tag 52350 ausgestattet
  13.09.00 wl  pmnu1Popup             FRightSelected eingeführt für verbesserte Funktion der rechten Maustaste in der TreeView
  13.09.00 wl  pmnuDeleteClick        FRightSelected eingeführt für verbesserte Funktion der rechten Maustaste in der TreeView
  13.09.00 wl  pmnuNewClick           FRightSelected eingeführt für verbesserte Funktion der rechten Maustaste in der TreeView
  13.09.00 wl  pmnuSaveAsClick        FRightSelected eingeführt für verbesserte Funktion der rechten Maustaste in der TreeView
  17.10.01 mo                         TN1067 Änderungen für SIAS
  29.10.01 mo  FormShow               Tree View wird bei Start der Applikation expanded angezeigt
  12.02.02 tbh OpenRack / OpenCarrier TN1180 Text für Hint angepasst
  06.08.02 tbh pmnuDeleteClick        TN1272 Run-Layouts werden auch gelöscht
  13.09.02 wl                         TN1283 Merge mit SIAS
  25.09.02 wl                         TN1292    Bugfix: fraEvaluate wieder visible
  26.09.02 wl                         TN1283 Kein fester Inifile Name mehr
  10.10.02 wl                               TN1293.2 Ini Access - uses geändert
  16.10.02 wl  FormCreate                   TN1293.2 AppSettings werden gesetzt
  18.10.02 wl  FormCreate             TN1293.1 Aufruf von InitSamGlobals1 --> Projekt-Quelltext
  18.10.02 wl                         TN1293.1 benutzt TAppSettings
  23.10.02 wl  UserLogout             TN1293.1 verwendet TWinlissyIniAccess
  23.10.02 wl  mnuExportClick         TN1293.1 verwendet TWinlissyIniAccess
  25.10.02 wl                         TN1293.1 verwendet TAppSettings.DataPath
  12.12.02 wl  FormCreate             TN1345 Initialisierung -> InitIntf
  12.12.02 wl  mnuExportClick         TN1345 vorübergehend deaktiviert
  17.12.02 tbh pmnuDeleteClick        TN1272 Run-Layouts werden auch wirklich gelöscht
  27.12.02 wl                               TN1293.5 uses und WinlissyIniAccess geändert
  30.12.02 wl  mnuToolsSettings             TN1293.3 neuer Meüpunkt für Settings.db
  04.01.03 wl                         TN1334.1 alle User & Licence-Methoden: gCommonDll statt user.dll
  15.01.03 wl  mnuToolsSettingsClick  TN1293.5 neuer Aufruf
  07.02.03 wl  mnuExportClick         TN1345   gelöscht
  07.02.03 wl  FormShow               TN1334.3 Data-Adaptoren werden initialisiert
  07.02.03 wl  UserSetAccess          TN1334.3 aus LayGlobe hierher
  07.02.03 wl  pmnuDeleteClick        TN1334.3 benutzt Methoden aus Data Adaptoren
  07.02.03 wl  pmnuSaveAsClick        TN1334.3 benutzt Methoden aus Data Adaptoren
  07.02.03 wl  gmVirtualCarrier       TN1334.3 --> ComponentTypeDataAdaptor
  11.02.03 wl  FormShow               TN1334.3 Data-Adaptoren werden mit Instance-function initialisiert
  11.02.03 wl  FormClose              TN1334.3 gCommonDll.Free
  19.02.03 wl  FormShow               TN1334.3 TSetupThreadManager statt TThreadManager
  19.02.03 wl                         TN1334.3 uses EditRack/EditCarr statt SiasEditRack/SiasEditCarr
  16.04.03 wl  FormCreate             TN1332.4 benutzt StopStartupLogging
  02.06.03 wl                         TN1485.4 Zusammenfassing von SiasMoveCtrl und MoveCtrl
  03.06.03 wl  FormKeyUp              TN1485.4 KeyPreview = true / bei Tastendruck wird Funktion aufgerufen
  04.06.03 wl  FormCreate             TN1493.12 Menüpunkt Simulation bei ZP02 unsichtbar
  05.06.03 wl                         TN1499   Layouter öffnet im Vollbild Modus
  18.06.03 wl                         TN1538   File-Menu jetzt wieder sichtbar
  03.07.03 wl                         TN1501.9 neuer Aufruf von TestForm
  14.07.03 wl  mnuToolsSimulation     TN1535.1 entfernt - Simulation kann nicht mehr zurückgesetzt werden
  17.07.03 wl  FormCreate             TN1536   geändertes InitIntf
  23.07.03 wl                         TN1536   neue Aufrufe für Balance-Funktionen
  02.09.03 wl  OpenRack,OpenCarrier   TN1559   Char-Array durch string ersetzt
  12.09.03 wl  ListBox1               TN1581.12 als LogDisplay für den Layouter
  12.09.03 wl  pmnuEditClick,TVDblClick TN1581.10 Edit Rack- und Carrier-Type mit Doppelklick und über Popup-Menu
  12.09.03 wl                         TN1581.4  Anpassung an Änderungen in TSetupWorkbench (LObjEdit)
  17.09.03 wl  CloseWB                TN1571   result falsch bei leeren Layout
  18.09.03 wl                         TN1581.11 "Check Racks" als CheckBox
  18.09.03 wl                         TN1581    Menu item "Movement" entfernt
  22.09.03 pk                         TN1556.2  Balance.DoTara function renamed to StartTare
  07.10.03 wl  OpenRack               TN1581    Rack ist immer visible
  18.12.03 wl                               TN1672   uses geändert
  30.01.04 wl  mnuCommandsClick       TN1722   neuer Menüpunkt "Machine commands ..."
  17.02.04 pk  OpenRack, OpenCarrier  TN1749   New parameter for SetTypeName. Form is set to visible at end of function
  19.02.04 pk  FormClose              TN1753   Calls ShutDownIntf
  23.02.04 pk                         TN1719   gGlobalEvents renamed to gSystemEvents
  23.02.04 pk  FormClose              TN1753   Explicitly destroy balance digit
  25.02.04 pk  FormClose              TN1753   Dont call gCommonDll.Free
  05.03.04 pk  TVDblClick, TVClick    TN1775.1 When machine is busy do not allow mode change (leads to crash)
  08.03.04 pk                         TN1753   ApplicationShutdown, ApplicationStartup
  09.03.04 pk                         TN1779   sbLMove changed to sbLRackMove. Various changes in CloseWB, OpenLayout, sbLRackMoveClick, sbLEditClick, sblTestClick
  09.03.04 pk                         TN1779   CreateAndLoadWorkbench, ChangeMode, ChangeLayoutName, ChangeButtonMode, etc.
  18.03.04 wl                         TN1825    bei allen User-Aktionen wird vorher gCommManager.LayouterResetGlobalErr ausgeführt
  19.03.04 wl  mnuTestReadBC          TN1831    entfernt
  05.04.04 wl  FormShow               TN1788    Robot.HasGripper durch gGrpArm.ArmExists ersetzt
  03.05.04 wl                         TN1788    uses Threads entfernt
  08.06.04 wl  FormShow               TN1963   PlateMove-Panel wird gezeigt wenn GripperArm existiert
  02.08.04 pk  UpdateTVSetup          TN2068   Use TStringList to keep track of names instead of static array
  02.08.04 pk  UpdateTVSetup          TN2068   Bug corrected
  16.08.04 wl  FormShow               TN2096   Auch bei best. XP-Anzeigeoptionen erscheinen Buttons nicht mehr überlappend
  16.02.05 wl  FormCreate             TN2269    statt gLiquids wird hier gDeviceMain erzeugt ( beinhaltet TLiquids.Create )
  11.03.05 pk  FormCreate             TN2339.2  Create gGUIManager
  21.06.05 pk  CreateAndLoadWorkbench TN2464.3  Call fraEvaluate1.TipUsedChanged to set tips on ArmPanel
  11.07.05 wl  ApplicationStartup      TN2498.1  für InitIntf wird eine Instanz von TAppInterface erzeugt und übergeben
  26.09.05 wl  FormCreate              TN2633    XP-Manifest eingefügt, für manche Elemente: ParentBackground := false
  05.10.05 wl  ApplicationStartup      TN2575    gGUIManager wird als erstes erzeugt
  17.11.05 wl  cbGripperArm            TN2771    neu: Auswahlbox für Gripperarm, nur sichtbar wenn 2 Gripperarme zur Auswahl sind
  28.11.05 wl  mnuTestRelaisClick      TN2812    fraEvaluate1.GetPositions von TestForm hierher
  30.11.05 wl  RepairDatabaseIndex1Click   TN2517   --> SettingEditor
  22.12.05 pk  ApplicationStartup      TN2875    TheadMan.Create has fewer params
  05.01.06 pk  FormShow                TN2877   TipManager find arm functions replaced by gModules find arm functions
  03.04.06 thr                         TN3012   Waagen-Buttons entfernt
  17.07.06 pk  cbGripperArms           TN3203   Removed
  18.07.06 pk                          TN3204   Increased height of Bottom Panel
  03.08.06 pk                          TN3244   Increased height of Bottom Panel
  19.09.06 pk  ApplicationStartup      TN3227.1 gDeviceMain changed to gSysLiqManager
  01.12.06 pk  ApplicationStartup      TN3441   cretae thread manager before GUIManager and ActionModules
  01.12.06 pk  StopBtn                 TN3441   cancel property set to false. Avoid user interrupt being done twice
  07.12.06 pk  ApplicationStartup      TN3455   create resource manager
  26.01.07 pk  ApplicationStartup      TN3525.3 create ErrorManager
  26.01.07 pk  ApplicationShutDown     TN3525.4 destroy everything that was created in startup
  31.01.07 wl                          TN3532   DBRack.CARRIER wird zu self.CARRIER (leider)
  27.02.07 pk  ApplicationStartup      TN325.2  log application title
  01.03.07 wl  FormShow                TN2611   wenn kein Arm existiert, wird MoveCtrl, Move to z-Travel, Edit Tips nicht gezeigt
  06.03.07 wl  SetSpeeds               TN3622   neu: Aufruf von gModules.ReduceMotorSpeeds
  01.08.07 wl  DisplayLogText          TN3811.2 überschreibt OnDisplayLogText in LogManager
  30.08.07 pk                          TN3840.1 uses TLayoutDataAdaptor instead of dmRack
  09.11.07 pk                          TN3923    class references to TErrorMessageFactory changed to gErrorMessageFactory
  07.01.08 pk                          TN3922   Save and Delete Rack/Carrier functions reprogrammed
  09.01.08 wl                          TN3972    Aboutbox wird anders aufgerufen
  29.01.08 pk                          TN3864   Menu Commands, Serial Port Test set Visible to false for Now
  30.01.08 pk                          TN3864   TestForm added again
  18.02.08 wl  TMain.UpdateTVSetup     TN4009x   ASSERT durch richtige Fehlermeldung ersetzt
  24.04.08 wl                          TN4073   some changes for new tip type items
  20.06.08 pk                          TN4139   Various changes
  24.06.08 wl                          TN4143   uses geändert
  27.06.08 pk  OpenRack, OpenCarrier   TN4139   SetTypeName changed to SetType
  02.07.08 pk  pmnuEditClick           TN4139   also edit workspace type
  03.07.08 wl                                         TN4157
  16.07.08 wl                          TN4164    Arm-Movement-Panel geändert
  17.07.08 wl                          TN4164    TestForm wird ständig angezeigt
  17.07.08 pk  CreateAndLoadWorkbench  TN4139   call PrepareRunData
  17.07.08 pk  FormClose               TN4139   FreeAndNil removed
  31.07.08 pk  CreateAndLoadWorkbench  TN4139   no longer loads RunLayout
  26.08.08 wl  TMain.InitControls      TN4164   Devices in TestForm werden aktualisiert
  08.09.08 pk  ApplicationStartup      TN4215   Call ThreadManager.SetInstance
  18.09.08 wl                          TN4224   optische Änderungen
  20.09.08 wl  CurrentArmPanel         TN4224   Welcher Arm-Tab ist gerade im Vordergrund
  23.09.08 wl  mnuToolsInitClick       TN4237   Init wieder aktiviert
  07.11.08 pk                          TN4280   ThreadManager class name changed
  15.12.08 wl  Zauberbutton1           TN2633   der Zauberbutton beseitigt die Darstellungsfehler
  17.12.08 pk  ApplicationStartup      TN4372   explicitly call ActionModule.Connect
  11.03.09 pk                          TN4457   Changes to Rack/CarrierType preview panel
  13.03.09 pk  InitControls            TN4465   remember the previous selected arm and set focus to it
  17.04.09 pk  fViewLayoutForm         TN4532   New form with navigation buttons
  09.06.09 pk  ApplicationStartup      TN4585.1 Call TRunDialogsManager.CreateInstance
  16.06.09 wl  ApplicationStartup      TN4609   ruft TActionModules.ConnectModulesSimple ohne Thread auf, vorher gab es bei Fehlern Absturz
  16.06.09 wl  ApplicationShutdown     TN4609   ruft jetzt TActionModules.DisconnectModulesSimple auf
  16.07.09 pk  InitControls            TN4662   Remember the previously used tip
  22.07.09 pk  UpdateTVSetup           TN4670   set the topnode to the firstnode, collapse the tiptypes
  22.07.09 pk                          TN4671   New Rack Icon
  04.08.09 ts  UpdateTVSetup           TN4569   LayoutNamen aus LAYOUTWORKSPACES anstatt aus LAYOUT -> Layouts ohne Racks/Carrier möglich, kein Datenmüll
  05.08.09 ts  EmptyPanel1             TN4333   pnMoveRacks/pnlEdit.Visible->false, damit bei SaveLayoutAs auch ZHeights,Save,CheckRacks weg sind
  05.08.09 ts  FormShow                TN4670   + TV.FullExpand aus FormShow raus, da es bei TVInit schon aufgerufen wird
  10.08.09 wl                          TN4702   Strings werden jetzt direkt geladen
  21.08.09 wl  fStringLoader           TN4702   fStringLoader lädt Strings für Dialog-Elemente
  26.08.09 pk                          TN4753   Some code/components moved to LayouterMain.pas
  28.09.09 pk                          TN4753   LayoutManager.Unload changed to .UnregisterCurrentLayout
  04.11.09 pk                          TN4843   Replace TObjectList, TStringList, TStrings with Generics Lists and TStringArray
  03.11.09 pk  RCWEdit                 TN4840   check if layout is not empty before casting it
  05.11.09 pk                          TN4830   For SaveAs, Add, and Create Virtual Carrier make sure cache of currentlayout is refreshed
  12.11.09 pk                          TN4862   XPManifest component removed
  19.11.09 pk  Close                   TN4892   call destroydefaultscenegraphics instead of destroying layoutmanager
  13.04.10 wl                          TN5044   uses geändert
  23.04.10 wl                          TN5070   uses geändert
  30.04.10 wl                          TN5070   bei Traysy-Lizenz keine Carrier anzeigen
  03.05.10 wl  mnuEditDefaultTipType   TN5052   bei OneTipTypeMode wird statt mehrerer TipTypes nur der Menüpunkt "Default .." angezeigt
  03.05.10 wl  mnuEditDefaultWorkspace TN5052   bei OneWorkspaceMode wird statt mehrerer TipTypes nur der Menüpunkt "Default Workspace" angezeigt
  05.05.10 ts  pmnuDeleteClick         TN5082   Layout-Anzeige wird nur gelöscht, wenn bei Sicherheitsabfrage JA gedrückt wurde und die Namen von angezeigtem und zu löschendem Layout übereinstimmen
  06.05.10 ts  NewLayout               TN5083   Workspace und Default Tiptype wird bei Erstellen von neuem Layout automatisch eingefügt
  07.05.10 wl                          TN5052    Display-Optionen aus TAppSettings statt IniAccess
  20.05.10 wl                          TN5117   neue Schriftart "Segoe UI", Schriftgröße 9
  28.05.10 wl                          TN5116   uses geändert
  11.06.10 pk  RefreshArmPositions     TN5138   GetPositions changed to ReadPositions
  18.01.11 wl  FormKeyUp               TN5176   reimplemented
  08.02.11 wl                          TN5475   benutzt TLayoutDataAdaptor.InstDeleteRun
  10.02.11 wl                          TN5475   Zugriffe auf ..DataAdaptor.Instance geändert
  21.07.11 wl                          TN5614   Edit Settings entfernt
  25.07.11 wl                          TN5614   neue Icons
  14.03.12 wl  cTestRunName            TN5831   von LayoutDataAdaptorExt hierher
  28.06.12 wl  UpdateArmPositions      TN5527   neu
  19.04.13 wl                          TN6106   an LayoutDataAdaptorExt angepasst
  22.08.13 wl                          TN6233.1 Bugfix: TMoveCtrlInitAction wurde immer 2 mal erzeugt
  10.12.13 wl  MoveLayout              TN6325   "Change xy-values of layout" wieder aktiviert
  -------------------------------------------------------------------------------------------------- }

unit LayMain;


interface


uses
    Windows,
    Classes,
    Controls,
    ExtCtrls,
    Buttons,
    Menus,
    ComCtrls,
    StdCtrls,
    Forms,
    ImgList,
    ArmMoveControl,
    LayoutElement,
    SceneGraphics,
    IntfArmDevice,
    TestForm,
    ViewLayout,
    LogManager,
    StringLoader,
    GeneralTypes;

type
    TDLLTestStringLoader = class(TTextListStringLoader)
    protected
        procedure AddAllItems(); override;
    end;

    TLayoutMode = (lmUnknown, lmTest, lmEdit, lmRackMove);
    TLoadWorkbenchOption = (lwChangeMode, lwChangeName);
    TLayoutElementTypeEnum = (letRack, letCarrier, letWorkspace);
    TLoadWorkbenchOptions = set of TLoadWorkbenchOption;

    TMain = class(TForm)
        StatusBar1: TStatusBar;
        Splitter1: TSplitter;
        ili1: TImageList;
        pmnu1: TPopupMenu;
        pmnuNew: TMenuItem;
        N6: TMenuItem;
        N7: TMenuItem;
        pmnuDelete: TMenuItem;
        pmnuSaveAs: TMenuItem;
        Panel3: TPanel;
        Panel1: TPanel;
        Panel5: TPanel;
        pnButtons: TPanel;
        Panel8: TPanel;
        lblMethod: TLabel;
        edLayout: TEdit;
        pnLEdit: TPanel;
        sbEditTips: TSpeedButton;
        sbLSave: TSpeedButton;
        sbZHeights: TSpeedButton;
        pnLayout: TPanel;
        sbLTest: TSpeedButton;
        sbLEdit: TSpeedButton;
        sbLRackMove: TSpeedButton;
        N2: TMenuItem;
        StopBtn: TBitBtn;
        pmnuVirtualCarrier: TMenuItem;
        ListBox1: TMemo;
        Splitter3: TSplitter;
        pmnuEdit: TMenuItem;
        pnMoveRacks: TPanel;
        cbCheckRacks: TCheckBox;
        PageControl1: TPageControl;
        Panel2: TPanel;
        TV: TTreeView;
        Splitter5: TSplitter;
        Splitter2: TSplitter;
        Zauberbutton1: TSpeedButton;
        pnLeftBottom: TPanel;
        Panel6: TPanel;
        lblRC: TLabel;
        edRC: TEdit;
        btnRCWEdit: TButton;
        Panel2x: TPanel;
        procedure FormCreate(Sender: TObject);
        procedure pmnu1Popup(Sender: TObject);
        procedure pmnuDeleteClick(Sender: TObject);
        procedure pmnuNewClick(Sender: TObject);
        procedure pmnuSaveAsClick(Sender: TObject);
        procedure pmnuVirtualCarrierClick(Sender: TObject);
        procedure sbLSaveClick(Sender: TObject);
        procedure sbLTestClick(Sender: TObject);
        procedure sbLEditClick(Sender: TObject);
        procedure sbLRackMoveClick(Sender: TObject);
        procedure sbZHeightsClick(Sender: TObject);
        procedure sbEditTipsClick(Sender: TObject);
        procedure StopBtnClick(Sender: TObject);
        procedure sbLTubeClick(Sender: TObject);

        procedure FormKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
        procedure TVDblClick(Sender: TObject);
        procedure pmnuEditClick(Sender: TObject);
        procedure PageControl1DockOver(Sender: TObject; Source: TDragDockObject; X, Y: Integer;
            State: TDragState; var Accept: Boolean);
        procedure PageControl1GetSiteInfo(Sender: TObject; DockClient: TControl; var InfluenceRect: TRect;
            MousePos: TPoint; var CanDock: Boolean);
        procedure PageControl1UnDock(Sender: TObject; Client: TControl; NewTarget: TWinControl;
            var Allow: Boolean);
        procedure TVChange(Sender: TObject; Node: TTreeNode);
        procedure btnRCWEditClick(Sender: TObject);

    private const
        cTestRunName = '_Layouter_TestRun';
    private
        FLayoutName: string;
        FRightSelected: TTreeNode;
        fLastMode: TLayoutMode;
        fSceneGraphics: TSceneGraphics;
        fCurrentLayoutElementType: TLayoutElement;
        fTestForm: TfrmTest;
        fViewLayoutForm: TfrmViewLayout;
        fStringLoader: TDLLTestStringLoader;
        fLayoutNode, fWorkspaceNode, fRackNode, fCarrierNode, fTipTypeNode: TTreeNode;
        fXYMoveMenuItem: TMenuItem;
    public
        procedure FileDelete();
        procedure FileSave();
        procedure FileNew();
        procedure EditZHeights();
        procedure MoveLayout();
        procedure ToolsInit();
        procedure HelpAbout();
        procedure ToolsZTravel();
        procedure UserLogon();
        procedure UserChangePW();
        procedure TestDLL();
        procedure EditDefaultWorkspace();
        procedure EditDefaultTipType();

        procedure CloseForm();
        procedure UpdateTVSetup;
        procedure TVInit();
        function EmptyPanel1: boolean;
        function CloseWB: boolean;
        procedure OpenLayout(iLName: string);
        procedure DeleteLayout(iLName: string);
        procedure NewTipType;
        procedure NewLayout;
        procedure NewCarrier;
        procedure NewRack;
        procedure NewWorkspace();
        procedure OpenWorkspace(const aTypeName: string);
        procedure OpenCarrier(const aTypeName: string);
        procedure OpenRack(const aTypeName: string);
        procedure UserSetAccess;
        function InMotion(): boolean;
        procedure AddNamesToNode(aType: string; aParentNode: TTreeNode; aImageIndex: integer;
            const aNames: TStringArray);
        function CreateAndLoadWorkbench(aMode: TLayoutMode; aLayoutName: string;
            aOptions: TLoadWorkbenchOptions): boolean;
        procedure ChangeLayoutName(aNewName: string);
        procedure ChangeMode(aNewMode: TLayoutMode; aChangeButtonMode: boolean);
        procedure ChangeButtonMode(aNewMode: TLayoutMode);
        procedure ResetButtonModeToLastMode();
        function ModeAllowed(aMode: TLayoutMode): boolean;
        function ModeToStr(aMode: TLayoutMode): string;
        procedure DoLayoutUnload(aSender: TObject; var vCancel: boolean);
        function AddNode(aParentNode: TTreeNode; const aName: string; aImageIndex: integer): TTreeNode;
        procedure OnInitEndAction(aSender: TObject);
        procedure InitControls();
        procedure CloseDockClient(aForm: TForm);
        procedure RCWEdit(aType: TLayoutElementTypeEnum; aName: string);
        procedure ChangeCurrentLayoutElement(aLayoutElement: TLayoutElement);
        function GetCurrentArmPanel: TfrmArmMoveControl;
        function GetCurrentArm: IArmDevice;
        function GetCurrentTip: integer;
        procedure EnableEditLayoutControls(const aEnable: boolean);

        procedure ApplicationStartup();
        procedure ApplicationShutdown();
        procedure DisplayLogText(const aLogText: string; aThreadID: integer;
            aDisplayType: TDisplayLogInfoType);
        procedure UpdateArmPositions;
        procedure ReadAndUpdateArmPositions;
        //
        property CurrentArmPanel: TfrmArmMoveControl read GetCurrentArmPanel;
        property CurrentArm: IArmDevice read GetCurrentArm;
        property ViewLayoutForm: TfrmViewLayout read fViewLayoutForm;

        property LayoutNode: TTreeNode read fLayoutNode;
        property WorkspaceNode: TTreeNode read fWorkspaceNode;
        property RackNode: TTreeNode read fRackNode;
        property CarrierNode: TTreeNode read fCarrierNode;
        property TipTypeNode: TTreeNode read fTipTypeNode;
        property XYMoveMenuItem: TMenuItem read fXYMoveMenuItem write fXYMoveMenuItem;
    end;

var
    Main: TMain;


implementation


uses
    SysUtils,
    Dialogs,
    ErrorManager,
    About,
    EditRack,
    EditCarr,
    ObjModul,
    ThrdMan,
    Rack,
    Carrier,
    LayGlobe,
    SamStart,
    LObjMove,
    EditingLayoutElements,
    CommonTypes,
    EditArm,
    DLLTest,
    DialogUtils,
    AppSettings,
    WorkspaceEditor,
    ViewItem,
    LayoutDataAdaptor,
    WorkspaceDataAdaptor,
    RackDataAdaptor,
    CarrierDataAdaptor,
    LayoutWorkspaceDataAdaptor,
    PipDeviceManager,
    LayoutDataAdaptorExt,
    ControlUtils,
    LayoutManager,
    LayouterLayoutManager,
    Workspace,
    ActionHandlerLow,
    LayoutElementGraphicsDriverTypeManager,
    TipTypeEditor,
    PeripheryManager,
    WorkspaceDevicesDataAdaptor;

{$R *.DFM}

const
    cImageIndexLayout = 1;
    cImageIndexWorkspace = 2;
    cImageIndexCarrier = 3;
    cImageIndexRack = 4;
    cImageIndexTiptype = 1;

    { TDLLTestStringLoader }

procedure TDLLTestStringLoader.AddAllItems;
begin
    AddSingle(100, '&File', '&Datei');
    AddDouble(50430, '&Test', 'Test pipetting positions', '&Test', 'Testen der Pipettierpositionen');
    AddDouble(50440, '&Edit', 'Edit Layout', '&Bearbeiten', 'Ändern der Layoutdaten');
    AddDouble(50450, '&Racks', 'Test rack movement', '&Racks', 'Testen des Rackgreifens');
    AddDouble(50460, '&Save', 'Save Layout', '&Speichern', 'Layout speichern');
    AddDouble(50470, '&Z-Heights', 'Define Workbench Heights', '&Z-Höhen',
        'Höhen auf der Arbeitsfläche definieren');
    AddDouble(50480, '&Tips', 'Edit tip-configuration', '&Nadeln', 'Spitzen-Konfiguration bearbeiten');
    AddSingle(50540, 'Test &DLL Functions', '&DLL-Funktionen testen');
    AddSingle(50550, '&Serial Port Test', '&Serielle Schnittstelle testen');
    // 50850
    AddSingle(52030, 'Check Racks', 'Racks überprüfen');
    AddSingle(52070, '&Tools', 'E&xtras');
    AddSingle(52080, '&User', '&Benutzer');
    AddSingle(52090, '&Help', '&Hilfe');
    AddSingle(52100, '&New Layout', '&Neues Layout');
    AddSingle(52120, '&Save Layout', 'Layout &speichern');
    AddSingle(52150, '&Exit Program', 'Programm &beenden');
    AddSingle(52180, '&Initialize', '&Initialisieren');
    AddSingle(52190, 'Goto &Z - Travel', 'Gehe zu &Z-Travel');
    AddSingle(52240, '&Logon', '&Anmelden');
    AddSingle(52250, 'Change &password', '&Passwort ändern');
    AddSingle(52260, '&About', '&Info');
    AddSingle(52270, 'Setup', 'Setup');
    AddSingle(52300, 'Edit Workbench Heights', 'Höhen der Arbeitsfläche bearbeiten');
    AddSingle(52310, 'Change XY-values of layout', 'XY-Werte des Layouts ändern');
    AddSingle(52320, '&New', '&Neu');
    AddSingle(52350, 'All Items;Layouts;Workspace Types;Carrier Types;Rack Types',
        'Alle Einträge;Layouts;Workspace Typen;Carrier Typen;Rack Typen');
    AddSingle(52370, 'Create virtual carrier', 'Virtuellen Carrier erzeugen');
    // 52440
end;

{ TMain }

procedure TMain.FormCreate(Sender: TObject);
begin
    TControlUtils.ResetFontForWinXP(self);
    StopBtn.Visible := false;
    StopBtn.Width := 180;

    fStringLoader := TDLLTestStringLoader.Create;
    fStringLoader.LoadLanguage(self);

    // TResLoader.LoadLanguage(fraEvaluate1);
    fLastMode := lmUnknown;

    // LogDisplay Initialisieren
    ListBox1.Lines.Clear;
    gLogManager.OnDisplayLogText := DisplayLogText;

    self.ChangeCurrentLayoutElement(nil);
    pnButtons.ParentBackground := false;
    // pnRC.ParentBackground := false;
    // fraEvaluate1.ParentBackground := false;
end;

procedure SetSpeeds();
var
    xIniAccess: IWinLissyIniAccess;
    // xSpeedFactor   : integer;
begin
    xIniAccess := gCommonDll.CreateRobotIni;
    { xSpeedFactor := } xIniAccess.ReadInteger('Module', 'SetupSpeedFactor'); // Default: 100%
    { TODO : not implemented yet }
    // gModules.ReduceMotorSpeeds( xSpeedFactor );
end;

// --------------------------------------------------------------------------------------------------
procedure TMain.ApplicationStartup();
// --------------------------------------------------------------------------------------------------
var
    xGripperArmNames: TStringArray;
begin

    fViewLayoutForm := TfrmViewLayout.Create(self);
    fViewLayoutForm.Parent := Panel1;
    fViewLayoutForm.Visible := true;

    TLayoutManager.SetInstance(TLayouterLayoutManager.Create(Main.ViewLayoutForm.DrawPanel));

    fViewLayoutForm.SceneGraphics := TLayoutManager.Instance.DefaultSceneGraphics;

    TLayoutManager.Instance.OnUnloadCurrent := self.DoLayoutUnload;

    fSceneGraphics := TSceneGraphics.Create(Panel2x);
    fSceneGraphics.Visible := true;

    if (gPipDeviceManager.Count = 0) then
    begin
        sbEditTips.Visible := false;
    end;

    gLogManager.Log(Application.Title + ': ' + TAppSettings.Version + ', ' + TAppSettings.Build, true);

    UserSetAccess;
    TVInit;

    // BalPanel.Visible := TActionModules.StartupBalanceDisplay( BalPanel );

    // decide whether rackmove controls should be visible
    xGripperArmNames := gModules.FindArmNames(true, false);

    if Length(xGripperArmNames) > 0 then
    begin
        sbLRackMove.Visible := true;
        pnLayout.Width := sbLTest.Width * 3 + 10;
    end
    else
    begin
        sbLRackMove.Visible := false;
        pnLayout.Width := sbLTest.Width * 2 + 10;
    end;

    pnLEdit.Left := pnLayout.Left + pnLayout.Width;

    fTestForm := TfrmTest.Create(self);
    fTestForm.ManualDock(PageControl1);
    fTestForm.Show();
end;

// --------------------------------------------------------------------------------------------------
procedure TMain.CloseForm();
// --------------------------------------------------------------------------------------------------
begin
    if not TLayoutManager.Instance.UnregisterCurrentLayout() then
        EXIT;
    TLayoutManager.Instance.DestroyDefaultSceneGraphics();

    ChangeCurrentLayoutElement(nil);
    if (sbLRackMove.Visible) then
        TLayoutDataAdaptor.InstDeleteRun(cTestRunName);
    fSceneGraphics.Free;
end;

// --------------------------------------------------------------------------------------------------
procedure TMain.ApplicationShutdown();
// --------------------------------------------------------------------------------------------------
begin
end;

// ==================================================================================================
// Menu Items
// --------------------------------------------------------------------------------------------------
procedure TMain.FileDelete();
// --------------------------------------------------------------------------------------------------
begin
    DeleteLayout(TLayoutManager.Instance.CurrentLayout.LayoutName);
end;

// --------------------------------------------------------------------------------------------------
procedure TMain.FileSave();
// --------------------------------------------------------------------------------------------------
begin
    (TLayoutManager.Instance.CurrentLayout as TSetupLayout).SaveAll;
end;

// --------------------------------------------------------------------------------------------------
procedure TMain.FileNew();
// --------------------------------------------------------------------------------------------------
begin
    NewLayout;
end;

procedure TMain.EditDefaultTipType;
begin
    TLayouterWorkflow.OpenFirstItem(ntTipType);
end;

procedure TMain.EditDefaultWorkspace;
begin
    TLayouterWorkflow.OpenFirstItem(ntWorkspace);
end;

procedure TMain.EditZHeights();
begin
    (TLayoutManager.Instance.CurrentLayout as TSetupLayout).EditZHeights;
end;

procedure TMain.MoveLayout();
begin
    (TLayoutManager.Instance.CurrentLayout as TSetupLayout).MoveLayout;
end;

procedure TMain.ToolsInit();
var
    xAction: TMoveCtrlInitAction;
begin
    if not ThrMan.SamThreadRunning(true) then
    begin
        xAction := TMoveCtrlInitAction.Create();
        xAction.OnEndAction := self.OnInitEndAction;
        gmCreateBasicActionHandlerThread(xAction, []);
    end;
end;

procedure TMain.OnInitEndAction(aSender: TObject);
begin
    ReadAndUpdateArmPositions;
end;

// --------------------------------------------------------------------------------------------------
procedure TMain.HelpAbout();
// --------------------------------------------------------------------------------------------------
var
    xAboutBox: TAboutBox;
begin
    xAboutBox := TAboutBox.Create(nil);
    try
        xAboutBox.ShowModal;
    finally
        xAboutBox.Free;
    end;
end;

// --------------------------------------------------------------------------------------------------
procedure TMain.ToolsZTravel();
// --------------------------------------------------------------------------------------------------
begin
    // fraEvaluate1.btnZTravelClick(nil);
end;

// --------------------------------------------------------------------------------------------------
procedure TMain.UserLogon();
// --------------------------------------------------------------------------------------------------
begin
    if not ThrMan.SamThreadRunning(false) then
    begin
        if TLayoutManager.Instance.UnregisterCurrentLayout() then
        begin
            gCommonDll.ChangeCurrentUser;
            UserSetAccess;
        end;
    end;
end;

// --------------------------------------------------------------------------------------------------
procedure TMain.UserChangePW();
// --------------------------------------------------------------------------------------------------
var
    xUser: IUser;
begin
    xUser := gCommonDll.CurrentUser;
    xUser.ChangePassword;
end;

// ==================================================================================================
// allgemeine Main-Methoden
// --------------------------------------------------------------------------------------------------
procedure TMain.DeleteLayout(iLName: string);
// --------------------------------------------------------------------------------------------------
begin
end;

// --------------------------------------------------------------------------------------------------
function TMain.EmptyPanel1: boolean;
// --------------------------------------------------------------------------------------------------
begin
    result := TLayoutManager.Instance.UnregisterCurrentLayout();
    if result then
    begin
        pnLayout.Visible := false;
        pnlEdit.Visible := false;
        pnMoveRacks.Visible := false;
        edLayout.Text := '';
    end;
end;

// --------------------------------------------------------------------------------------------------
function TMain.CloseWB: boolean;
// --------------------------------------------------------------------------------------------------
var
    xLayoutEmpty: boolean;
    x: integer;
begin

    xLayoutEmpty := false;
    result := false;

    if (TLayoutManager.Instance.CurrentLayout is TSetupLayout) and
        (not(TLayoutManager.Instance.CurrentLayout as TSetupLayout).AskForSave(false, xLayoutEmpty)) then
    begin
        ResetButtonModeToLastMode();
        // delete Listview item if layout is empty
        if xLayoutEmpty then
        begin
            for x := 0 to tv.Items.Count - 1 do
                if (tv.Items[x].Text = TLayoutManager.Instance.CurrentLayout.LayoutName) and
                    (tv.Items[x].Parent = LayoutNode) then
                begin
                    tv.Items[x].Delete;
                    break;
                end;
        end;
        Exit;
    end;

    result := true;
    if (TLayoutManager.Instance.CurrentLayout <> nil) then
    begin

        // TLayoutManager.Instance.CurrentLayout.Free;
        TLayoutDataAdaptor.InstDeleteRun(cTestRunName);
    end;
end;

procedure TMain.DoLayoutUnload(aSender: TObject; var vCancel: boolean);
begin
    vCancel := not CloseWB();
end;

// --------------------------------------------------------------------------------------------------
procedure TMain.NewLayout;
// --------------------------------------------------------------------------------------------------
var
    xNewName, xWorkspace, xTipType: string;
    NewNode: TTreeNode;
begin
    xNewName := TDialogUtils.InputBox(TLanguageString.Read('Type name of new layout.',
        'Bitte geben Sie einen Namen für das neue Layout ein.'), TLanguageString.Read('New Layout',
        'Neues Layout'), '');
    if (xNewName = '') then
        Exit;

    xNewName := Copy(xNewName, 1, TLayoutDataAdaptor.MaxNameLen);
    if TLayoutDataAdaptorExt.Instance.NameExists(xNewName) or (not EmptyPanel1) then
    begin
        TDialogUtils.MessageBox(TLanguageString.Read('Layout {0} already exists!',
            'Layout {0} existiert bereits!', [xNewName]), '', 0);
        Exit;
    end;

    NewNode := AddNode(LayoutNode, xNewName, cImageIndexLayout);
    NewNode.MakeVisible;

    self.CreateAndLoadWorkbench(lmEdit, xNewName, [lwChangeMode, lwChangeName]);
    (TLayoutManager.Instance.Instance.CurrentLayout as TSetupLayout).AddNewLayout(xNewName, xWorkspace,
        xTipType);

    if (xWorkspace = '') or (xTipType = '') then
    begin
        TLayoutDataAdaptorExt.Instance.DeleteName(xNewName);
        self.EmptyPanel1;
        LayoutNode.GetLastChild.Delete;
        EXIT;
    end;
    (TLayoutManager.Instance.Instance.CurrentLayout as TSetupLayout).AddNewWorkspace(xWorkspace, true);
    self.FileSave;
    self.CreateAndLoadWorkbench(lmEdit, xNewName, [lwChangeMode, lwChangeName]);
    (TLayoutManager.Instance.Instance.CurrentLayout as TSetupLayout).SetDefaultTipset(xTipType);
end;

// --------------------------------------------------------------------------------------------------
procedure TMain.OpenLayout(iLName: string);
// --------------------------------------------------------------------------------------------------
begin
    if (EmptyPanel1()) then
    begin
        self.CreateAndLoadWorkbench(lmTest, iLName, [lwChangeMode, lwChangeName]);
    end;
end;

// --------------------------------------------------------------------------------------------------
procedure TMain.OpenWorkspace(const aTypeName: string);
// --------------------------------------------------------------------------------------------------
var
    xRec: TWorkspaceRec;
    xWDDA: TWorkspaceDevicesDataAdaptor;
    xDevices: TWorkspaceDevicesRecArray;
    xWorkspace: TWorkspace;
    xDA: TWorkspaceDataAdaptor;
begin
    xDA := TWorkspaceDataAdaptor.Create;
    try
        if not xDA.ReadRecByName(aTypeName, xRec) then
            EXIT;
    finally
        FreeAndNil(xDA);
    end;

    xWDDA := TWorkspaceDevicesDataAdaptor.Create;
    try
        xWDDA.ReadByWorkspaceID(xRec.ID, xDevices);
    finally
        FreeAndNil(xWDDA);
    end;

    xWorkspace := TWorkspace.Create();
    xWorkspace.InitGraphics();
    xWorkspace.SetType(xRec, xDevices);
    self.ChangeCurrentLayoutElement(xWorkspace);
end;

// --------------------------------------------------------------------------------------------------
procedure TMain.OpenRack(const aTypeName: string);
// --------------------------------------------------------------------------------------------------
var
    xRec: TRackRec;
    xRack: TRack;
    xDA: TRackDataAdaptor;
begin
    xDA := TRackDataAdaptor.Create;
    try
        if not xDA.ReadRack(aTypeName, xRec) then
            EXIT;
    finally
        FreeAndNil(xDA);
    end;

    xRack := TRack.CreateRackTypeView(xRec);
    self.ChangeCurrentLayoutElement(xRack);
end;

// --------------------------------------------------------------------------------------------------
procedure TMain.OpenCarrier(const aTypeName: string);
// --------------------------------------------------------------------------------------------------
var
    xRec: TCarrierRec;
    xCarrier: TCarrier;
begin
    if not TCarrierDataAdaptor.ReadCarrier(aTypeName, xRec) then
        EXIT;

    xCarrier := TCarrier.CreateCarrierTypeView(xRec);
    self.ChangeCurrentLayoutElement(xCarrier);
end;

// --------------------------------------------------------------------------------------------------
procedure TMain.NewTipType;
// --------------------------------------------------------------------------------------------------
var
    xName: string;
begin
    xName := TfrmEdTipType.NewTipType();
    if (xName = '') then
        EXIT;

    TV.Selected := TV.Items.AddChild(TipTypeNode, xName);
    TV.Selected.ImageIndex := 0;
    TV.Selected.SelectedIndex := 0;
end;

procedure TMain.NewWorkspace;
var
    xNewName: string;
begin
    xNewName := '';
    if TfrmWorkspaceEditor.InstanceShowToAdd(xNewName) and (xNewName <> '') then
    begin
        if not TLayoutManager.Instance.IsCurrentLayoutEmpty then
            TLayoutManager.Instance.CurrentLayout.TypeDataCache.WorkspaceTypeDataCache.Refresh;
        TV.Selected := TV.Items.AddChild(WorkspaceNode, xNewName);
        TV.Selected.ImageIndex := WorkspaceNode.ImageIndex;
        TV.Selected.SelectedIndex := WorkspaceNode.SelectedIndex;
        TVChange(nil, TV.Selected);
    end
end;

// --------------------------------------------------------------------------------------------------
procedure TMain.NewCarrier;
// --------------------------------------------------------------------------------------------------
var
    xNewName: string;
begin
    xNewName := '';
    if TfrmEdCarr.InstanceShowToAdd(xNewName) and (xNewName <> '') then
    begin
        if not TLayoutManager.Instance.IsCurrentLayoutEmpty then
            TLayoutManager.Instance.CurrentLayout.TypeDataCache.CarrierTypeDataCache.Refresh;
        TV.Selected := TV.Items.AddChild(CarrierNode, xNewName);
        TV.Selected.ImageIndex := CarrierNode.ImageIndex;
        TV.Selected.SelectedIndex := CarrierNode.SelectedIndex;
        TVChange(nil, TV.Selected);
    end
end;

// --------------------------------------------------------------------------------------------------
procedure TMain.NewRack;
// --------------------------------------------------------------------------------------------------
var
    xNewName: string;
begin
    xNewName := '';
    if TfrmEdRack.InstanceShowToAdd(xNewName) and (xNewName <> '') then
    begin
        if not TLayoutManager.Instance.IsCurrentLayoutEmpty then
            TLayoutManager.Instance.CurrentLayout.TypeDataCache.RackTypeDataCache.Refresh;

        TV.Selected := TV.Items.AddChild(RackNode, xNewName);
        TV.Selected.ImageIndex := RackNode.ImageIndex;
        TV.Selected.SelectedIndex := RackNode.SelectedIndex;
        TVChange(nil, TV.Selected);
    end
end;

function TMain.AddNode(aParentNode: TTreeNode; const aName: string; aImageIndex: integer): TTreeNode;
begin
    result := TV.Items.AddChild(aParentNode, aName);
    result.ImageIndex := aImageIndex;
    result.SelectedIndex := result.ImageIndex;
end;

procedure TMain.AddNamesToNode(aType: string; aParentNode: TTreeNode; aImageIndex: integer;
    const aNames: TStringArray);
var
    x: integer;
begin
    for x := 0 to Length(aNames) - 1 do
    begin
        if (aNames[x] = '') then
        begin
            ShowMessage(Format('A %s without name exists in the database. Please delete it!', [aType]));
            CONTINUE;
        end;
        AddNode(aParentNode, aNames[x], aImageIndex);
    end;
end;

procedure TMain.UpdateTVSetup;
var
    xNames: TStringArray;
begin
    fLayoutNode := TV.Items[1];
    fWorkspaceNode := TV.Items[2];
    fCarrierNode := TV.Items[3];
    fRackNode := TV.Items[4];
    fTipTypeNode := TV.Items[5];

    xNames := TLayoutWorkspaceDataAdaptor.InstReadAllNames();
    AddNamesToNode('Layout', fLayoutNode, cImageIndexLayout, xNames);

    if not TAppSettings.IsOneWorkspaceMode then
    begin
        xNames := TWorkspaceDataAdaptor.InstReadAllNames();
        AddNamesToNode('Workspace', fWorkspaceNode, cImageIndexWorkspace, xNames);
    end
    else
    begin
        fWorkspaceNode.Delete;
        fWorkspaceNode := nil;
    end;

    if TAppSettings.UseCarriers then
    begin
        xNames := TCarrierDataAdaptor.InstReadAllNames();
        AddNamesToNode('Carrier', fCarrierNode, cImageIndexCarrier, xNames);
    end
    else
    begin
        fCarrierNode.Delete;
        fCarrierNode := nil;
    end;

    xNames := TRackDataAdaptor.InstReadAllNames();
    AddNamesToNode('Rack', fRackNode, cImageIndexRack, xNames);

    if not TAppSettings.IsOneTipTypeMode then
    begin
        xNames := TfrmEdTipType.GetAllTypes();
        AddNamesToNode('Tip Type', fTipTypeNode, 0, xNames);
    end
    else
    begin
        fTipTypeNode.Delete;
        fTipTypeNode := nil;
    end;
end;

procedure TMain.TVInit();
begin
    UpdateTVSetup;
    TV.FullExpand;

    // Tip types are not edited that often, collapse at startup
    if Assigned(fTipTypeNode) then
        fTipTypeNode.Collapse(true);

    // Focus First Node
    TV.TopItem := TV.Items[0];
end;

// --------------------------------------------------------------------------------------------------
procedure TMain.TVDblClick(Sender: TObject);
// --------------------------------------------------------------------------------------------------
begin
    if InMotion then
        Exit;
    if (TV.Selected.Parent = nil) then
        EXIT; // wichtig für OneWorkspaceMode bzw. OneTipTypMode, da in den Modi WorkspaceNode bzw. TipTypNode
    // auch nil sind und versucht wird ein Edit-Fenster zu öffnen (Assertion failure)

    // ---------------------------------------------------------------------------------- Layout öffnen
    if (TV.Selected.Parent = LayoutNode) and (TV.Selected.Text <> FLayoutName) then
        OpenLayout(TV.Selected.Text);
    // ------------------------------------------------------------------------------ Workspacetyp öffnen
    if (TV.Selected.Parent = WorkspaceNode) then
    begin
        RCWEdit(letWorkspace, TV.Selected.Text);
    end;
    // ------------------------------------------------------------------------------ Carriertyp öffnen
    if (TV.Selected.Parent = CarrierNode) then
    begin
        RCWEdit(letCarrier, TV.Selected.Text);
    end;

    // --------------------------------------------------------------------------------- Racktyp öffnen
    if (TV.Selected.Parent = RackNode) then
    begin
        RCWEdit(letRack, TV.Selected.Text);
    end;

    // TipType
    if (TV.Selected.Parent = TipTypeNode) then
        TfrmEdTipType.EditTipType(TV.Selected.Text);
end;

// --------------------------------------------------------------------------------------------------
procedure TMain.pmnuEditClick(Sender: TObject);
// --------------------------------------------------------------------------------------------------
begin
    if (FRightSelected.Parent = WorkspaceNode) then
        RCWEdit(letWorkspace, FRightSelected.Text);
    if (FRightSelected.Parent = CarrierNode) then
        RCWEdit(letCarrier, FRightSelected.Text);
    if (FRightSelected.Parent = RackNode) then
        RCWEdit(letRack, FRightSelected.Text);
end;

// --------------------------------------------------------------------------------------------------
procedure TMain.RCWEdit(aType: TLayoutElementTypeEnum; aName: string);
// --------------------------------------------------------------------------------------------------
var
    xDummy: boolean;
begin
    if (not TLayoutManager.Instance.IsCurrentLayoutEmpty) and
        (TLayoutManager.Instance.CurrentLayout is TSetupLayout) then
        if not(TLayoutManager.Instance.CurrentLayout as TSetupLayout).AskForSave(true, xDummy) then
            exit;

    if (aType = letWorkspace) then
        if TfrmWorkspaceEditor.InstanceShowToEdit(aName) then
            OpenWorkspace(aName)
        else
            exit;

    if (aType = letCarrier) then
        if TfrmEdCarr.InstanceShowToEdit(aName) then
            OpenCarrier(aName)
        else
            exit;

    if (aType = letRack) then
        if TfrmEdRack.InstanceShowToEdit(aName) then
            OpenRack(aName)
        else
            exit;

    if (sbLTest.Down) then
        sbLTestClick(nil);
    if (sbLEdit.Down) then
        sbLEditClick(nil);
    if (sbLRackMove.Down) then
        sbLRackMoveClick(nil);
end;

// --------------------------------------------------------------------------------------------------
procedure TMain.pmnu1Popup(Sender: TObject);
// --------------------------------------------------------------------------------------------------
var
    i: integer;
begin
    FRightSelected := TV.Selected;
    for i := 0 to pmnu1.Items.count - 1 do
        pmnu1.Items.Items[i].Visible := false;
    // check user level
    if not gCommonDll.CurrentUser.HasLevel(usrSystemAdmin) then
        exit;
    // ---------------------------------------------------------- Layout oder LayoutNode ist ausgewählt
    if (FRightSelected = LayoutNode) or (FRightSelected.Parent = LayoutNode) or
        (FRightSelected = WorkspaceNode) or (FRightSelected.Parent = WorkspaceNode) or
        (FRightSelected = RackNode) or (FRightSelected.Parent = RackNode) or (FRightSelected = CarrierNode) or
        (FRightSelected.Parent = CarrierNode) or (FRightSelected = TipTypeNode) or
        (FRightSelected.Parent = TipTypeNode) then
    begin
        pmnuNew.Visible := true;
    end;
    // ----------------------------------------------------------- Layout, Rack, Carrier ist ausgewählt
    if (FRightSelected.Parent = LayoutNode) or (FRightSelected.Parent = WorkspaceNode) or
        (FRightSelected.Parent = RackNode) or (FRightSelected.Parent = CarrierNode) or
        (FRightSelected.Parent = TipTypeNode) then
    begin
        N6.Visible := true;
        pmnuDelete.Visible := true;
        pmnuDelete.Caption := TLanguageString.Read('Delete {0}', '{0} löschen', [FRightSelected.Text]);
        pmnuSaveAs.Visible := true;
        pmnuSaveAs.Caption := TLanguageString.Read('Save {0} as ...', '{0} speichern unter ...',
            [FRightSelected.Text]);
    end;
    // ----------------------------------------------------------- Layout, Rack, Carrier ist ausgewählt
    if (FRightSelected.Parent = WorkspaceNode) or (FRightSelected.Parent = RackNode) or
        (FRightSelected.Parent = CarrierNode) or (FRightSelected.Parent = TipTypeNode) then
    begin
        pmnuEdit.Visible := true;
        pmnuEdit.Caption := TLanguageString.Read('Edit {0}', '{0} bearbeiten', [FRightSelected.Text]);
    end;

    // ---------------------------------------------------------------------------- Rack ist ausgewählt
    if (FRightSelected.Parent = RackNode) then
    begin
        N7.Visible := true;
        pmnuVirtualCarrier.Visible := true;
    end;
end;

// --------------------------------------------------------------------------------------------------
procedure TMain.pmnuDeleteClick(Sender: TObject);
// --------------------------------------------------------------------------------------------------
var
    CheckErr: string;
    xTypeID: integer;
    xIsTypeUsed: boolean;
    xLWDA: TLayoutWorkspaceDataAdaptor;
    xWDA: TWorkspaceDataAdaptor;
begin
    if (FRightSelected.Text = '') then
        exit;
    if (FRightSelected.Parent = LayoutNode) then
    begin
        if TLayoutDataAdaptorExt.Instance.DeleteNameWithConfirm(FRightSelected.Text) then
        begin
            if (FRightSelected.Text = self.edLayout.Text) then
                (EmptyPanel1);
            FRightSelected.Delete;
        end;
        exit;
    end;

    if (FRightSelected.Parent = WorkspaceNode) then
    begin
        xWDA := TWorkspaceDataAdaptor.Create;
        try
            if not xWDA.ReadIDByName(FRightSelected.Text, xTypeID) then
                EXIT;
        finally
            FreeAndNil(xWDA);
        end;

        xLWDA := TLayoutWorkspaceDataAdaptor.Create;
        try
            xIsTypeUsed := xLWDA.WorkspaceTypeIDExists(xTypeID);
        finally
            FreeAndNil(xLWDA);
        end;

        if (xIsTypeUsed) then
        begin
            Application.MessageBox(PChar('Deleting impossible: Affected one or more Layouts'),
                'Delete Workspace Type', 0);
            EXIT;
        end;

        gmDeleteWorkspace(fRightSelected.Text);
        ChangeCurrentLayoutElement(nil);
        FRightSelected.Delete;
        exit;
    end;

    if (FRightSelected.Parent = CarrierNode) then
    begin
        CheckErr := TLayoutDataAdaptorExt.Instance.CarrierTypeIsUsed(FRightSelected.Text);
        if (CheckErr <> '') then
        begin
            Application.MessageBox(PChar('Deleting impossible: Affected Layouts: ' + CheckErr),
                'Delete Carrier Type', 0);
            exit;
        end;
        if gmDeleteCarrier(FRightSelected.Text) then
        begin
            ChangeCurrentLayoutElement(nil);
            FRightSelected.Delete;
        end;
        exit;
    end;

    if (FRightSelected.Parent = RackNode) then
    begin
        CheckErr := TLayoutDataAdaptorExt.Instance.RackTypeIsUsed(FRightSelected.Text);
        if (CheckErr <> '') then
        begin
            Application.MessageBox(PChar('Deleting impossible: Affected Layouts: ' + CheckErr),
                'Delete Rack Type', 0);
            exit;
        end;
        if gmDeleteRack(FRightSelected.Text) then
        begin
            ChangeCurrentLayoutElement(nil);
            FRightSelected.Delete;
        end;
        exit;
    end;

    // TipType
    if (FRightSelected.Parent = TipTypeNode) then
    begin
        if TfrmEdTipType.DeleteTipType(FRightSelected.Text) then
            FRightSelected.Delete;
        EXIT;
    end;

end;

// --------------------------------------------------------------------------------------------------
procedure TMain.pmnuNewClick(Sender: TObject);
// --------------------------------------------------------------------------------------------------
begin
    if (FRightSelected = LayoutNode) or (FRightSelected.Parent = LayoutNode) then
        if (EmptyPanel1) then
            NewLayout;

    if (FRightSelected = WorkspaceNode) or (FRightSelected.Parent = WorkspaceNode) then
        NewWorkspace;

    if (FRightSelected = CarrierNode) or (FRightSelected.Parent = CarrierNode) then
        NewCarrier;

    if (FRightSelected = RackNode) or (FRightSelected.Parent = RackNode) then
        NewRack;

    if (FRightSelected = TipTypeNode) or (FRightSelected.Parent = TipTypeNode) then
        NewTipType;
end;

// --------------------------------------------------------------------------------------------------
procedure TMain.pmnuSaveAsClick(Sender: TObject);
// --------------------------------------------------------------------------------------------------
var
    xNewName: string;
begin
    if (FRightSelected.Text = '') then
        exit;
    if (FRightSelected.Parent = LayoutNode) and (EmptyPanel1) then
    begin
        xNewName := TLayoutDataAdaptorExt.Instance.SaveAs(FRightSelected.Text);
        if (xNewName <> '') then
        begin
            TV.Selected := AddNode(LayoutNode, xNewName, cImageIndexLayout);
            TVChange(nil, TV.Selected);
        end;
        EXIT;
    end;

    if (FRightSelected.Parent = WorkspaceNode) then
    begin
        xNewName := gmSaveWorkspaceAs(FRightSelected.Text);
        if (xNewName <> '') then
        begin
            if not TLayoutManager.Instance.IsCurrentLayoutEmpty then
                TLayoutManager.Instance.CurrentLayout.TypeDataCache.WorkspaceTypeDataCache.Refresh;
            TV.Selected := AddNode(WorkspaceNode, xNewName, cImageIndexWorkspace);
            TVChange(nil, TV.Selected);
        end;
        EXIT;
    end;

    if (FRightSelected.Parent = CarrierNode) then
    begin
        xNewName := gmSaveCarrierAs(FRightSelected.Text);
        if (xNewName <> '') then
        begin
            if not TLayoutManager.Instance.IsCurrentLayoutEmpty then
                TLayoutManager.Instance.CurrentLayout.TypeDataCache.CarrierTypeDataCache.Refresh;
            TV.Selected := AddNode(CarrierNode, xNewName, cImageIndexCarrier);
            TVChange(nil, TV.Selected);
        end;
        EXIT;
    end;
    if (FRightSelected.Parent = RackNode) then
    begin
        xNewName := gmSaveRackAs(FRightSelected.Text);
        if (xNewName <> '') then
        begin
            if not TLayoutManager.Instance.IsCurrentLayoutEmpty then
                TLayoutManager.Instance.CurrentLayout.TypeDataCache.RackTypeDataCache.Refresh;
            TV.Selected := AddNode(RackNode, xNewName, cImageIndexRack);
            TVChange(nil, TV.Selected);
        end;
        EXIT;
    end;

    // TipType
    if (FRightSelected.Parent = TipTypeNode) then
    begin
        xNewName := FRightSelected.Text;
        if not TfrmEdTipType.SaveAsTipType(xNewName) then
            EXIT;
        TV.Selected := AddNode(TipTypeNode, xNewName, cImageIndexTipType);
        TVChange(nil, TV.Selected);
    end;
end;

// --------------------------------------------------------------------------------------------------
procedure TMain.sbLSaveClick(Sender: TObject);
// --------------------------------------------------------------------------------------------------
begin
    FileSave();
end;

// --------------------------------------------------------------------------------------------------
procedure TMain.sbLTestClick(Sender: TObject);
// --------------------------------------------------------------------------------------------------
begin
    OpenLayout(FLayoutName);
end;

// --------------------------------------------------------------------------------------------------
procedure TMain.sbLEditClick(Sender: TObject);
// --------------------------------------------------------------------------------------------------
begin
    if TLayoutManager.Instance.UnregisterCurrentLayout() then
    begin

        self.CreateAndLoadWorkbench(lmEdit, FLayoutName, [lwChangeMode]);
    end;
end;

// --------------------------------------------------------------------------------------------------
procedure TMain.sbLRackMoveClick(Sender: TObject);
// --------------------------------------------------------------------------------------------------
begin
    if TLayoutManager.Instance.UnregisterCurrentLayout() then
    begin
        self.CreateAndLoadWorkbench(lmRackMove, fLayoutName, [lwChangeMode]);
    end;
end;

// --------------------------------------------------------------------------------------------------
procedure TMain.sbZHeightsClick(Sender: TObject);
// --------------------------------------------------------------------------------------------------
begin
    (TLayoutManager.Instance.CurrentLayout as TSetupLayout).EditZHeights;
end;

// --------------------------------------------------------------------------------------------------
procedure TMain.sbEditTipsClick(Sender: TObject);
// --------------------------------------------------------------------------------------------------
var
    frmEdArm: TfrmEdArm;
begin
    frmEdArm := TfrmEdArm.Create(nil);
    try
        frmEdArm.ShowModal;
    finally
        frmEdArm.Free;
    end;
end;

// --------------------------------------------------------------------------------------------------
procedure TMain.TestDLL();
// --------------------------------------------------------------------------------------------------
var
    frmDLLTest: TfrmDLLTest;
begin
    gErrorManager.LayouterResetGlobalErr;

    frmDLLTest := TfrmDLLTest.Create(nil);
    try
        frmDLLTest.ShowModal;
    finally
        frmDLLTest.Free;
    end;
end;

// --------------------------------------------------------------------------------------------------
procedure TMain.StopBtnClick(Sender: TObject);
// --------------------------------------------------------------------------------------------------
begin
    ThrMan.UserInterrupt;
end;

// --------------------------------------------------------------------------------------------------
procedure TMain.sbLTubeClick(Sender: TObject);
// --------------------------------------------------------------------------------------------------
begin
    // Layout mit der Möglichkeit, Tubes zu bewegen
end;

procedure TMain.pmnuVirtualCarrierClick(Sender: TObject);
var
    xRackRec: TRackRec;
    xRackDA: TRackDataAdaptor;
    xCarrierName: string;
begin
    if (FRightSelected.Parent <> RackNode) then
        EXIT;

    xRackDA := TRackDataAdaptor.Create();
    try
        if not xRackDA.ReadRack(FRightSelected.Text, xRackRec) then
            EXIT;

        xCarrierName := FRightSelected.Text;
        if TfrmEdCarr.InstanceShowToAddFlatCarrier(xCarrierName, xRackRec.X_mm, xRackRec.Y_mm) then
        begin
            if not TLayoutManager.Instance.IsCurrentLayoutEmpty then
                TLayoutManager.Instance.CurrentLayout.TypeDataCache.CarrierTypeDataCache.Refresh;

            TV.Selected := TV.Items.AddChild(CarrierNode, xCarrierName);
            TV.Selected.ImageIndex := cImageIndexCarrier;
            TV.Selected.SelectedIndex := cImageIndexCarrier;
            TVChange(nil, TV.Selected);
        end;
    finally
        xRackDA.Free;
    end;
end;

procedure TMain.UserSetAccess;
var
    xUser: IUser;
begin
    xUser := gCommonDll.CurrentUser;
    StatusBar1.Panels[0].Text := xUser.Name;

    // Guest Access
    sbLEdit.Enabled := false;
    sbLRackMove.Enabled := false;
    StatusBar1.Panels[1].Text := TLanguageString.
        Read('Guest Access: Load Layout, view data, no editing, only simulation mode',
        'Guest Access: Load Layout, view data, no editing, only simulation mode');

    // System Access
    if xUser.HasLevel(usrSystem) then
    begin
        StatusBar1.Panels[1].Text := TLanguageString.Read('System User Access: Test with machine, no editing',
            'System User Access: Test with machine, no editing');
        sbLRackMove.Enabled := true;
    end;

    // System Administrator access
    if xUser.HasLevel(usrSystemAdmin) then
    begin
        StatusBar1.Panels[1].Text := TLanguageString.
            Read('System Administrator Access: Test with machine, change Rack, Carrier and Layout data',
            'System Administrator Access: Test with machine, change Rack, Carrier and Layout data');
        sbLEdit.Enabled := true;
    end;
end;

procedure TMain.FormKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
var
    xArmMoveControl: TfrmArmMoveControl;
begin
    xArmMoveControl := self.GetCurrentArmPanel;
    if not Assigned(xArmMoveControl) then
        EXIT;

    xArmMoveControl.KeyHandling(Key, Shift);
end;

function TMain.InMotion(): boolean;
begin
    result := ThrMan.SamThreadRunning(true);
end;

procedure TMain.EnableEditLayoutControls(const aEnable: boolean);
begin
    pnLEdit.Visible := aEnable;
    fXYMoveMenuItem.enabled := aEnable;
end;

// --------------------------------------------------------------------------------------------------
function TMain.CreateAndLoadWorkbench(aMode: TLayoutMode; aLayoutName: string;
    aOptions: TLoadWorkbenchOptions): boolean;
// --------------------------------------------------------------------------------------------------
var
    xLayoutManagerMode: TLayoutManagerMode;
    xLoadOK: boolean;
    // xRunName : string;
begin

    EnableEditLayoutControls(false);

    result := false;
    if not ModeAllowed(aMode) then
    begin
        ShowMessage(format('Mode %s is not allowed', [ModeToStr(aMode)]));
        ResetButtonModeToLastMode();
        Exit;
    end;

    // xRunName := '';
    xLayoutManagerMode := lmmNone;
    case aMode of
        lmTest:
            begin
                xLayoutManagerMode := lmmTest; { xRunName := cTestRunName; } end;
        lmEdit:
            begin
                xLayoutManagerMode := lmmEdit;
            end;
        lmRackMove:
            begin
                xLayoutManagerMode := lmmRack; { xRunName := cTestRunName; } end;
    end;

    (TLayoutManager.Instance as TLayouterLayoutManager).ChangeLayoutMode(xLayoutManagerMode);
    // if xRunName <> '' then begin
    // TLayoutDataAdaptorExt.PrepareRunData( xRunName, aLayoutName );
    // end;

    TLayoutManager.Instance.RegisterLayout('', aLayoutName);
    xLoadOK := TLayoutManager.Instance.Load();
    if not xLoadOK then
    begin
        ShowMessage(format('Layout %s could not be loaded', [aLayoutName]));
        ResetButtonModeToLastMode();
        Exit;
    end;
    if lwChangeMode in aOptions then
        ChangeMode(aMode, true);
    if lwChangeName in aOptions then
        ChangeLayoutName(aLayoutName);

    if aMode = lmEdit then
    begin
        EnableEditLayoutControls(true);
    end;

    self.InitControls();

    // We have to change the color of the used tips on the arm panel
    // fraEvaluate1.InitTips();
    result := true;

end;

// --------------------------------------------------------------------------------------------------
procedure TMain.ChangeMode(aNewMode: TLayoutMode; aChangeButtonMode: boolean);
// --------------------------------------------------------------------------------------------------
begin
    fLastMode := aNewMode;
    if aChangeButtonMode then
        ChangeButtonMode(aNewMode);
end;

// --------------------------------------------------------------------------------------------------
procedure TMain.ChangeButtonMode(aNewMode: TLayoutMode);
// --------------------------------------------------------------------------------------------------
begin
    sbLTest.Down := (aNewMode = lmTest);
    sbLEdit.Down := (aNewMode = lmEdit);
    sbLRackMove.Down := (aNewMode = lmRackMove);
end;

// --------------------------------------------------------------------------------------------------
procedure TMain.ResetButtonModeToLastMode();
// --------------------------------------------------------------------------------------------------
begin
    ChangeButtonMode(fLastMode);
end;

// --------------------------------------------------------------------------------------------------
function TMain.ModeAllowed(aMode: TLayoutMode): boolean;
// --------------------------------------------------------------------------------------------------
begin
    result := false;
    case aMode of
        lmTest:
            result := sbLTest.Enabled;
        lmEdit:
            result := sbLEdit.Enabled;
        lmRackMove:
            result := sbLRackMove.Enabled;
    end;
end;

// --------------------------------------------------------------------------------------------------
function TMain.ModeToStr(aMode: TLayoutMode): string;
// --------------------------------------------------------------------------------------------------
begin
    result := 'UNKNOWN';
    case aMode of
        lmTest:
            result := 'TEST';
        lmEdit:
            result := 'EDIT';
        lmRackMove:
            result := 'RACKMOVE';
    end;
end;

// --------------------------------------------------------------------------------------------------
procedure TMain.ChangeLayoutName(aNewName: string);
// --------------------------------------------------------------------------------------------------
begin
    FLayoutName := aNewName;
    edLayout.Text := aNewName;
    pnLayout.Visible := true;
end;

procedure TMain.DisplayLogText(const aLogText: string; aThreadID: integer; aDisplayType: TDisplayLogInfoType);
begin
    ListBox1.Lines.Add(aLogText);
end;

procedure TMain.ReadAndUpdateArmPositions();
var
    x: integer;
begin
    for x := PageControl1.DockClientCount - 1 downto 1 do
    begin
        if (PageControl1.DockClients[x] is TfrmArmMoveControl) then
        begin
            (PageControl1.DockClients[x] as TfrmArmMoveControl).ReadPositions();
        end;
    end;
end;

procedure TMain.UpdateArmPositions();
var
    x: integer;
begin
    for x := PageControl1.DockClientCount - 1 downto 1 do
    begin
        if (PageControl1.DockClients[x] is TfrmArmMoveControl) then
        begin
            (PageControl1.DockClients[x] as TfrmArmMoveControl).GetPositions();
        end;
    end;
end;

procedure TMain.PageControl1DockOver(Sender: TObject; Source: TDragDockObject; X, Y: Integer;
    State: TDragState; var Accept: Boolean);
begin
    Accept := (Source.Control is TfrmArmMoveControl) or (Source.Control is TfrmTest);

    if (Source.Control.HostDockSite = PageControl1) then
        Accept := false;

end;

procedure TMain.PageControl1GetSiteInfo(Sender: TObject; DockClient: TControl; var InfluenceRect: TRect;
    MousePos: TPoint; var CanDock: Boolean);
begin
    CanDock := (DockClient is TfrmArmMoveControl) or (DockClient is TfrmTest);

end;

procedure TMain.PageControl1UnDock(Sender: TObject; Client: TControl; NewTarget: TWinControl;
    var Allow: Boolean);
begin
    Allow := false; // man darf einfach keine Edit-Fenster aus dem Tab-Control lösen!
end;

procedure TMain.TVChange(Sender: TObject; Node: TTreeNode);
begin
    if InMotion then
        Exit;

    if (TV.Selected.Parent = WorkspaceNode) then
    begin
        OpenWorkspace(TV.Selected.Text);
    end
    else if (TV.Selected.Parent = CarrierNode) then
    begin
        OpenCarrier(TV.Selected.Text);
    end
    else if (TV.Selected.Parent = RackNode) then
    begin
        OpenRack(TV.Selected.Text);
    end
    else
        ChangeCurrentLayoutElement(nil);
end;

procedure TMain.ChangeCurrentLayoutElement(aLayoutElement: TLayoutElement);
begin
    FreeAndNil(fCurrentLayoutElementType);
    lblRC.Caption := '...';
    edRC.Text := '';
    if Assigned(fSceneGraphics) then
        fSceneGraphics.SceneChanged();

    if not Assigned(aLayoutElement) then
        EXIT;

    fCurrentLayoutElementType := aLayoutElement;
    fCurrentLayoutElementType.AssignGraphicsParent(fSceneGraphics);
    fCurrentLayoutElementType.Visible := true;
    self.fSceneGraphics.SceneChanged();

    if (fCurrentLayoutElementType is TWorkspace) then
    begin
        lblRC.Caption := 'Workspace Type:';
        edRC.Text := (fCurrentLayoutElementType as TWorkspace).WorkspaceTypeName;
    end;
    if (fCurrentLayoutElementType is TRack) then
    begin
        lblRC.Caption := 'Rack Type:';
        edRC.Text := (fCurrentLayoutElementType as TRack).TypeName;
    end;
    if (fCurrentLayoutElementType is TCarrier) then
    begin
        lblRC.Caption := 'Carrier Type:';
        edRC.Text := (fCurrentLayoutElementType as TCarrier).TypeName;
    end;

    try
        TV.SetFocus;
    except
    end;
end;

procedure TMain.btnRCWEditClick(Sender: TObject);
begin
    if (fCurrentLayoutElementType is TWorkspace) then
    begin
        self.RCWEdit(letWorkspace, (fCurrentLayoutElementType as TWorkspace).WorkspaceTypeName);
    end;
    if (fCurrentLayoutElementType is TRack) then
    begin
        self.RCWEdit(letRack, (fCurrentLayoutElementType as TRack).TypeName);
    end;
    if (fCurrentLayoutElementType is TCarrier) then
    begin
        self.RCWEdit(letCarrier, (fCurrentLayoutElementType as TCarrier).TypeName);
    end;
end;

function TMain.GetCurrentArmPanel: TfrmArmMoveControl;
begin
    result := nil;

    // im Page-Control ändert sich die ActivePage
    if not Assigned(PageControl1.ActivePage) then
        EXIT;
    if (PageControl1.DockClientCount <= PageControl1.ActivePageIndex) then
        EXIT;

    if (PageControl1.DockClients[PageControl1.ActivePageIndex] is TfrmArmMoveControl) then
    begin
        result := PageControl1.DockClients[PageControl1.ActivePageIndex] as TfrmArmMoveControl;
    end;
end;

function TMain.GetCurrentArm: IArmDevice;
var
    xMoveControl: TfrmArmMoveControl;
begin
    result := nil;
    xMoveControl := self.CurrentArmPanel;
    if not Assigned(xMoveControl) then
        EXIT;

    result := xMoveControl.CurrentArm;
end;

procedure TMain.CloseDockClient(aForm: TForm);
begin
    if not Assigned(aForm) then
        EXIT;

    // close the form
    aForm.Close;
    // WICHTIG!!! nicht löschen
    Application.ProcessMessages; // Damit wird das geschlossene Fenster zerstört, wenn caFree gesetzt!
    // WICHTIG!!! nicht löschen
end;

function TMain.GetCurrentTip: integer;
var
    xMoveControl: TfrmArmMoveControl;
begin
    result := 0;
    xMoveControl := self.CurrentArmPanel;
    if not Assigned(xMoveControl) then
        EXIT;

    result := xMoveControl.TipUsed;
end;

procedure TMain.InitControls();
var
    x, xSearchIndex: integer;
    xArm, xSelectedArm: IArmDevice;
    xSelectedTip: integer;
    xArmControlForm: TfrmArmMoveControl;
begin
    // save currently selected arm
    xSelectedArm := GetCurrentArm();
    xSelectedTip := GetCurrentTip();

    // alle Controls schließen
    for x := PageControl1.DockClientCount - 1 downto 1 do
    begin
        if (PageControl1.DockClients[x] is TForm) then
        begin
            self.CloseDockClient(PageControl1.DockClients[x] as TForm);
        end;
    end;

    xSearchIndex := 0;
    TPeripheryManager.Instance.RefreshLayoutDeviceList;

    fTestForm.RefreshDevicesView();

    while TPeripheryManager.Instance.FindNextModule(IArmDevice, xSearchIndex, xArm) do
    begin

        xArmControlForm := TfrmArmMoveControl.Create(self);
        xArmControlForm.InitControls(xArm);
        xArmControlForm.ManualDock(PageControl1);
        xArmControlForm.Show();
    end;

    // reselect previously selected arm
    if Assigned(xSelectedArm) then
    begin
        for x := PageControl1.DockClientCount - 1 downto 1 do
        begin
            if (PageControl1.DockClients[x] is TfrmArmMoveControl) then
            begin
                if xSelectedArm = (PageControl1.DockClients[x] as TfrmArmMoveControl).CurrentArm then
                begin
                    PageControl1.ActivePageIndex := x;
                    (PageControl1.DockClients[x] as TfrmArmMoveControl).TipUsed := xSelectedTip;
                end;
            end;
        end;
    end;

end;


end.
