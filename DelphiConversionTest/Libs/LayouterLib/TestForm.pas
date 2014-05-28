{ --------------------------------------------------------------------------------------------------
  Ebene 2 (Sam-Interface)
  --------------------------------------------------------------------------------------------------
  Über dieses Fenster können Devices getestet werden
  --------------------------------------------------------------------------------------------------
  Datum    op  function/procedure     Änderung / Neuerung
  -------- --  -------------------    --------------------------------------------------------------
  13.04.00 wl                         neue unit
  14.04.00 wl                         erweitert um Tango-Funktionen SetTemp und TangoIst
  10.05.00 wl  Button8Click           Tango-Temperatur mit Kommastelle
  26.05.00 wl                         uses geändert
  20.06.00 wl                         mit Balance-Funktionen
  14.07.00 wl                         gModules-Aufrufe z.T. als (gModules as TActionModules)
  02.08.00 wl                         Komplettrenovierung & Integration der Vortexer
  07.08.00 wl  FormCreate             neu: Redi-Switches in Treeview
  07.08.00 wl  TVClick                Redi-Module werden wie normale Switches behandelt
  08.08.00 wl  FormShow,FormClose     LogDisplay wird hierher umgelenkt
  08.08.00 wl                         neu: Testen des Var-Redi-Motors
  09.08.00 wl                         ListView statt TreeView (noch übersichtlicher!)
  10.08.00 wl                         --> String in Ressourcen
  17.10.01 mo                         TN1067 Änderungen für SIAS
  28.12.01 tbh Button2Click           TN1051 Aufruf von VolMotorSetVolume angepasst
  16.01.02 tbh Button2Click           TN1141 es wird Step statt Volumen eingestellt
  10.10.02 wl                               TN1293.2 Ini Access - uses geändert
  12.12.02 wl                         TN1345 für Sias aktiviert
  27.12.02 wl                               TN1293.5 uses und WinlissyIniAccess geändert
  28.01.03 wl                               TN1293.5 geänderte gModules-Methodenaufrufe
  03.07.03 wl                         TN1501.9  mit Decapper komplett überarbeitet
  04.07.03 wl                         TN1501.1 Änderungen für Decapper-Device
  08.07.03 wl                         TN1501.9 Bessere Aufteilung der Oberfläche
  08.07.03 wl                         TN1501.9 Button für mehrfaches Decapping
  10.07.03 wl  TTestThread            TN1501.9 alle komplexen Aktionen können im Thread ablaufen
  21.07.03 wl  btnInitClick           TN1501.9 neu: mit Init-Button
  23.07.03 wl                         TN1501.9 neue Aufrufe für Balance-Funktionen
  30.07.03 wl                         TN1501.3 jetzt mit Sensor-Device!
  31.07.03 wl  TVClick                TN1536   jetzt auch für IKA-Shaker
  03.08.03 wl                         TN1501.9 Alle Schnittstellen werden angezeigt
  04.08.03 wl                         TN1501.3 Decapper-Funktionen mit aUseCapSensor-Parameter
  19.08.03 wl  TTestThread            TN1501.3 Decapper-Funktionen wieder ohne Parameter
  22.09.03 pk                         TN1556.2  Balance.DoTara function renamed to StartTare
  08.10.03 wl  btnBCReaderInitClick   TN1501.9 neu: Button zum Init der BC-Reader
  19.11.03 wl  btnShortReadClick      TN1668.1 BCReader_ShortRead aufgeteilt in TriggerOn und GetBarcode
  20.11.03 wl  TVClick                TN1677   neu: Abfrage des aktuellen Standes bei Switch-Devices
  18.12.03 wl                         TN1672   uses geändert
  04.02.04 pk                         TN1719   uses MessageHandling
  08.06.04 wl  btnZTravelClick        TN1963   benutzt gmAllArmsMoveZTravel
  29.06.04 pk  TTestThread            TN2009.9 Converted to TModuleTestAction
  30.09.04 wl                         TN2157   neuer Label für Protocol
  03.02.05 wl                         TN2269   bentzt auch x-Wege-Ventil
  16.02.05 wl                         TN2269   jetzt mit Unterstützung von TXWayValveDevice und TPipPumpDevice
  17.03.05 pk                         TN2352.1 Uses ActionHandlerLow
  23.03.05 pk                         TN2361   All groups put inside of PageControl
  23.03.05 pk  btnBCReaderInitClick   TN2361   Init single BCReader instead of all BCReaders
  14.10.05 wl                         TN2670   geänderte Funktionsaufrufe fur Thermostat-Devices
  28.11.05 wl                         TN2812   Unterscheidet zwischen einfachen und Doübleswitch (mit HasDefaultSwitch)
  28.11.05 wl                         TN2812   fraEvaluate1.GetPositions -> LayMain
  15.12.05 wl                         TN2633   benutzt TValueListEditor zur Anzeige: Keine Darstellungsprobleme mehr
  03.04.06 thr                        TN3007   Zusätzliche Parameter für StartTare und StartWeight
  03.04.06 pk                         TN2997   new parameter : aInitID for Init methods
  10.04.06 pk  BtnInitClick           TN3031   InitAction has new parameter aFullInit
  07.09.06 pk  btnThermoInitClick     TN3291   New
  19.09.06 pk  TVClick                TN3227.1 gDeviceMain changed to gSysLiqManager
  15.11.06 pk  TVClick                TN3423   Bug fixed: access violation when pippump was clicked on
  15.11.06 pk  btnInitPipPump         TN3422   Init, Asp, Dsp test functions for pippumps
  27.11.06 wl  Button2Click           TN3362    an Änderungen von TIntfVarRedi angepasst
  03.01.07 pk                         TN3479   New tab for StateSignal
  18.01.07 wl                         TN3507   Vortexer-Fiunktionen angepasst
  29.02.07 wl                         TN3574   neuer Button für TThermoDevice.GetTargetTemp
  02.03.07 pk                         TN3613   VarRediMotor Calls TVarRediMotorDevice functions instead of interface functions
  06.03.07 wl  btnInitClick           TN3620    Kein ResetModules vor Init
  08.06.07 pk  btnShakerInitClick     TN3725   New
  01.08.07 wl  DisplayLogText         TN3811.2 überschreibt OnDisplayLogText in LogManager
  07.09.07 pk  fPrevDisplayLogText    TN3811.2 save the previous reference to avoid using laymain
  30.01.08 pk                         TN3864   various changes
  31.01.08 pk                         TN4003   Änderungen für Thermo,Shaker-Device
  19.02.08 wl                         TN4009    uses IntfThermostatDevice, IntSwitchDriver
  17.03.08 wl                         TN4043    uses IntfMixerDevice
  17.04.08 wl                         TN3726    phMeter/Thermometer reactivated
  17.04.08 wl                         TN4072    new: Memory & Trigger device
  06.05.08 wl  FillInfoTree           TN4068    1.Version: Device-Daten werden angezeigt, mehr aber nicht
  07.05.08 wl  FillInfoTree           TN4068    die Daten alle Untermodule werden angezeigt
  08.05.08 pk  ReadSwitchState        TN4102    New. SourceCode from TVClick. Now also called after switch is initialized.
  11.07.08 wl                         TN4164   TActionModules wird ohne "gModules as" aufgerufen
  17.07.08 wl                         TN4164   Format geändert
  26.08.08 wl  RefreshDevicesView     TN4164    lädt nur die LayoutDevices
  08.09.08 pk                         TN4215   use Action instaed of MinimalAction
  10.09.08 wl  ReadSwitchState        TN4220   wenn State gelesen wird, soll nicht gleichzeitig geschaltet werden
  15.09.08 wl  rbAllDevices           TN4220   zeigt jetzt Layout devices ODER alle Devices an
  24.11.08 pk  WaitForThread          TN4280   new. Instead of TThread.WaitFor
  24.06.09 ts  Button13Click          TN4624   Thermostat-Test-Fenster: TargetTemp wird jetzt angezeigt + SpinEdit2 MaxValue von 200-->2000
  21.08.09 wl  fStringLoader          TN4702   fStringLoader lädt Strings für Dialog-Elemente
  03.09.09 wl  cxTreeList1            TN4800   Anpassungen an Treelist Version 5 (nicht kompatibel)
  19.10.09 pk  WaitForThread          TN4824   Removed - caused deadlock
  20.10.09 pk  TBalanceInitTestAction TN4827   New. Also TDecapperTestAction extracted from TModuleTestAction
  11.11.09 pk  RefreshDevicesView     TN4856   uses settingsdevicemanager instead of devicemanager
  16.11.09 ts  PipPumpAsp/DispClick   TN4871   if xIntf.GetExecIntf = nil then EXIT
  18.11.09 pk  RefreshDevicesView     TN4877   clear treeview before refreshing
  19.11.09 ts  PipPumpAsp/DispClick   TN4885   StrToFloat (to enter also nl), insert label for units in PipPump-Testform
  17.03.10 wl                         TN5031   uses GenericTree
  13.04.10 wl                         TN5044   uses geändert
  20.05.10 wl                         TN5117   neue Schriftart "Segoe UI", Schriftgröße 9
  14.12.10 wl  btnPipPumpAspClick     TN5411   DecimalSeparator wird nicht mehr umgeschrieben
  04.10.11 wl                         TN5574   PHMeter-Aufruf: Neue Parameter
  14.10.11 ts  btnAskPHClick          TN5712   an PHMeter angepasst
  08.02.12 wl  btnPipPumpTurnClick    TN5790   Ventil drehen ist jetzt möglich
  10.04.12 wl  cxTreeList1            TN5852   NativeStyle = true
  26.07.13 wl                         TN6160   an IStateSignalDevice-Änderungen angepasst
  21.08.13 wl                         TN6231   uses geändert
  11.09.13 wl                         TN6249   IPipPumpDevice.Pick: neuer Parameter aInWashOrWaste
  -------------------------------------------------------------------------------------------------- }

unit TestForm;


interface


uses
    Forms,
    Controls,
    Classes,
    ComCtrls,
    StdCtrls,
    ExtCtrls,
    Spin,
    ImgList,
    Grids,
    cxGraphics,
    cxCustomData,
    cxStyles,
    cxTL,
    cxControls,
    cxInplaceContainer,
    cxTextEdit,
    cxLookAndFeels,
    cxLookAndFeelPainters,
    Action,
    IntfDevice,
    ModuleSettings,
    ThreadClasses,
    StringLoader;

type
    TTestFormStringLoader = class(TTextListStringLoader)
    protected
        procedure AddAllItems(); override;
    end;

    TModuleTestAction = class(TAction)
    protected
        FCurrentModule: IDevice;
        fControl: TControl;
        procedure DoExecute(); virtual; abstract;
    public
        constructor Create(const aCurrentModule: IDevice);
        procedure ExecFirst(); override;
        property Control: TControl read fControl write fControl;
    end;

    TDevTestType = (ttDecapper_Cap, ttDecapper_Decap, ttDecapper_DecapNCapNRead, ttDecapper_DecapNRead,
        ttDecapper_Read, ttDecapper_DecapNCap);

    TDecapperTestAction = class(TModuleTestAction)
    private
        FTestType: TDevTestType;
        procedure Decapper_Cap;
        procedure Decapper_Decap;
        procedure Decapper_Read;
        procedure Decapper_DecapNRead;
        procedure Decapper_DecapNCap(aNoOfTurns: integer);
        procedure Decapper_DecapNCapNRead(aNoOfTurns: integer);
        constructor Create(aTestType: TDevTestType; const aCurrentModule: IDevice);
    protected
        procedure DoExecute(); override;
    end;

    TBalanceInitTestAction = class(TModuleTestAction)
    protected
        procedure DoExecute(); override;
    end;

    TBalanceTareTestAction = class(TModuleTestAction)
    protected
        procedure DoExecute(); override;
    end;

    TBalanceWeighTestAction = class(TModuleTestAction)
    private
        fRackID: string;
        fPos: integer;
        fSubstID: string;
    protected
        procedure DoExecute(); override;
        constructor Create(const aCurrentModule: IDevice; const aRackID: string; const aPos: integer;
            const aSubstID: string);
    end;

    TfrmTest = class(TForm)
        pnlTopLeft: TPanel;
        tv: TTreeView;
        pnlDeviceTabs: TPanel;
        ImageList1: TImageList;
        pgctlDevices: TPageControl;
        shtBalance: TTabSheet;
        shtSwitch: TTabSheet;
        shtShaker: TTabSheet;
        shtThermostat: TTabSheet;
        grbBalance: TGroupBox;
        Label1: TLabel;
        Label2: TLabel;
        Label3: TLabel;
        Label5: TLabel;
        btnBalInit: TButton;
        btnOpenDoor: TButton;
        btnBalTara: TButton;
        btnStoreWeight: TButton;
        Edit1: TEdit;
        Edit2: TEdit;
        Edit3: TEdit;
        rgrSwitch: TRadioGroup;
        grbCATShaker: TGroupBox;
        Label7: TLabel;
        Label8: TLabel;
        Label9: TLabel;
        rgFixation: TRadioGroup;
        edSpeed: TEdit;
        edOnPulse: TEdit;
        edOffPulse: TEdit;
        rgShaker: TRadioGroup;
        grbThermostat: TGroupBox;
        Label4: TLabel;
        Label6: TLabel;
        SpinEdit2: TSpinEdit;
        Button8: TButton;
        Button7: TButton;
        SpinEdit1: TSpinEdit;
        shtREDI: TTabSheet;
        shtDecapper: TTabSheet;
        shtBCReader: TTabSheet;
        shtSensor: TTabSheet;
        grbRediMotor: TGroupBox;
        Label11: TLabel;
        Label12: TLabel;
        Label13: TLabel;
        Button1: TButton;
        Button2: TButton;
        spinVol: TSpinEdit;
        spinMinVol: TSpinEdit;
        spinMaxVol: TSpinEdit;
        grbDecapper: TGroupBox;
        Button3: TButton;
        Button4: TButton;
        Button5: TButton;
        Button6: TButton;
        edTubeBC: TEdit;
        Button10: TButton;
        Button11: TButton;
        Edit4: TEdit;
        Button9: TButton;
        grbBCReader: TGroupBox;
        Button12: TButton;
        Button14: TButton;
        edBarcode: TEdit;
        btnShortRead: TButton;
        btnBCReaderInitAll: TButton;
        grbSensor: TGroupBox;
        Button15: TButton;
        stSensor: TStaticText;
        shtXWayValve: TTabSheet;
        shtPipPump: TTabSheet;
        grbXWayValve: TGroupBox;
        btnXWayReadPos: TButton;
        rgInPorts: TRadioGroup;
        grbDilutor: TGroupBox;
        Label10: TLabel;
        stDilutorIn: TStaticText;
        btnBCReaderInit: TButton;
        btnSwitchInitRelay: TButton;
        btnThermoInit: TButton;
        btnInitPipPump: TButton;
        btnPipPumpAsp: TButton;
        edPipPumpAspVol: TEdit;
        edPipPumpDspVol: TEdit;
        btnPipPumpDsp: TButton;
        shtStateSignal: TTabSheet;
        rdoStateSignal: TRadioGroup;
        edsStateSignalActiveSwitch: TStaticText;
        edsStateSignalReadySwitch: TStaticText;
        edsStateSignalErrorSwitch: TStaticText;
        Label14: TLabel;
        stCurrentTemp: TStaticText;
        Button13: TButton;
        shtPHMeter: TTabSheet;
        grbPHMeter: TGroupBox;
        btnAskPH: TButton;
        stPHMeter: TStaticText;
        shtThermometer: TTabSheet;
        grbThermometer: TGroupBox;
        btnAskTemp: TButton;
        stThermometer: TStaticText;
        btnShakerInit: TButton;
        shtMemory: TTabSheet;
        grbMemory: TGroupBox;
        btnWriteValue: TButton;
        stGetValue: TStaticText;
        btnReadValue: TButton;
        edSetValue: TEdit;
        shtTrigger: TTabSheet;
        grbTrigger: TGroupBox;
        btnTrigger: TButton;
        cxTreeList1: TcxTreeList;
        cxTreeList1cxTreeListColumn1: TcxTreeListColumn;
        cxTreeList1cxTreeListColumn2: TcxTreeListColumn;
        Splitter2: TSplitter;
        Panel1: TPanel;
        rbLayoutDevices: TRadioButton;
        rbAllDevices: TRadioButton;
        shtLocationBasedMotionDevice: TTabSheet;
        Button16: TButton;
        stTargetTemp: TStaticText;
        Label15: TLabel;
        lblUnitAsp: TLabel;
        lblUnitDsp: TLabel;
        btnPipPumpTurnvalve: TButton;
        rbSyrSys: TRadioButton;
        rbSyrTip: TRadioButton;
        edsStateSignalMessageSwitch: TStaticText;
        edsStateSignalTestSwitch: TStaticText;
        procedure FormCreate(Sender: TObject);
        procedure Button7Click(Sender: TObject);
        procedure Button8Click(Sender: TObject);
        procedure btnBalInitClick(Sender: TObject);
        procedure btnOpenDoorClick(Sender: TObject);
        procedure btnBalTaraClick(Sender: TObject);
        procedure btnStoreWeightClick(Sender: TObject);
        procedure TVClick(Sender: TObject);
        procedure rgFixationClick(Sender: TObject);
        procedure rgShakerClick(Sender: TObject);
        procedure rgrSwitchClick(Sender: TObject);
        procedure Button1Click(Sender: TObject);
        procedure Button2Click(Sender: TObject);
        procedure Button3Click(Sender: TObject);
        procedure Button5Click(Sender: TObject);
        procedure Button4Click(Sender: TObject);
        procedure Button6Click(Sender: TObject);
        procedure Button9Click(Sender: TObject);
        procedure Button10Click(Sender: TObject);
        procedure Button11Click(Sender: TObject);
        procedure Button12Click(Sender: TObject);
        procedure Button14Click(Sender: TObject);
        procedure btnShortReadClick(Sender: TObject);
        procedure Button15Click(Sender: TObject);
        procedure tvGetSelectedIndex(Sender: TObject; Node: TTreeNode);
        procedure btnXWayReadPosClick(Sender: TObject);
        procedure rgInPortsClick(Sender: TObject);
        procedure btnBCReaderInitClick(Sender: TObject);
        procedure btnSwitchInitRelayClick(Sender: TObject);
        procedure btnThermoInitClick(Sender: TObject);
        procedure btnInitPipPumpClick(Sender: TObject);
        procedure btnPipPumpAspClick(Sender: TObject);
        procedure btnPipPumpDspClick(Sender: TObject);
        procedure rdoStateSignalClick(Sender: TObject);
        procedure Button13Click(Sender: TObject);
        procedure btnAskPHClick(Sender: TObject);
        procedure btnAskTempClick(Sender: TObject);
        procedure btnShakerInitClick(Sender: TObject);
        procedure btnTriggerClick(Sender: TObject);
        procedure btnWriteValueClick(Sender: TObject);
        procedure btnReadValueClick(Sender: TObject);
        procedure rbAllDevicesClick(Sender: TObject);
        procedure rbLayoutDevicesClick(Sender: TObject);
        procedure Button16Click(Sender: TObject);
        procedure FormDestroy(Sender: TObject);
        procedure btnPipPumpTurnvalveClick(Sender: TObject);
    private
        FCurrentModule: IDevice;
        fXYZPositionsChanged: boolean;
        fSwitchLocked: boolean;
        fStringLoader: TTestFormStringLoader;
        procedure HideAllDeviceTabs();
        procedure FillInfoTree();
        procedure FillSettingsToNode(aNode: TcxTreeListNode; aSettings: TModuleSettingList);
        procedure ReadSwitchState;
        function CreateTestThread(const aAction: TModuleTestAction): TSysHandleID;
    public
        procedure RefreshDevicesView();
        property XYZPositionsChanged: boolean read fXYZPositionsChanged;
    end;


implementation


{$R *.DFM}

uses
    SysUtils,
    Dialogs,
    ErrorManager,
    GenericTree,
    GeneralTypes,
    GUIManager,
    ActionHandlerLow,
    ObjModul,
    DeviceInitHandling,
    Device,
    Liquids,
    DeviceManager,
    DeviceSettingsManager,
    LiquidManager,
    INtfSwitchDriver,
    IntfThermostatDevice,
    IntfBalanceDevice,
    IntfMixerDevice,
    IntfSwitchDevice,
    IntfSensorDevice,
    IntfDecapperDevice,
    IntfBCReaderDevice,
    IntfVarRediMotorDevice,
    IntfXWayValveDevice,
    IntfPipPumpDevice,
    IntfStateSignalDevice,
    IntfTriggerDevice,
    IntfMemoryDevice,
    IntfPHMeterDevice,
    IntfThermometerDevice,
    Module,
    Driver,
    IntfPipPumpDriver,
    DriverManager,
    DriverSettingList,
    DeviceSettingList,
    IntfMotionDevice,
    Connection,
    ConnectionManager,
    ConnectionSettingList,
    PeripheryManager,
    TeachControl,
    DialogUtils,
    SystemEvents,
    ControlUtils;

var
    gNoOfTurns: integer;
    gBCString: string;

    { TTestFormStringLoader }

procedure TTestFormStringLoader.AddAllItems;
begin
    AddSingle(34000, 'Module test', 'Modultest');
    AddSingle(34040, 'Min. Step:', 'Min. Schritt:');
    AddSingle(34050, 'Max. Step:', 'Max. Schritt:');
    AddSingle(34060, 'Motor Step', 'Schritt:');
    AddSingle(34070, 'Set Step', 'Auf Schritt fahren');
    AddSingle(34080, 'Temperature:', 'Temperatur:');
    AddSingle(34090, 'Get current temp.', 'Temp. abfragen');
    AddSingle(34100, 'Set target temp.', 'Zieltemp. einstellen');
    AddSingle(34110, 'Speed:', 'Geschwindigkeit:');
    AddSingle(34120, 'On Pulse [sec]:', 'On Pulse [sec]:');
    AddSingle(34130, 'Off Pulse [sec]:', 'Off Pulse [sec]:');
    AddDouble(34140, 'Shaker', 'Off;On', 'Schüttler', 'Aus;An');
    AddDouble(34150, 'Fixation', 'Off;On', 'Fixierung', 'Aus;An');
    AddSingle(34160, 'Open Door', 'Tür öffnen');
    AddSingle(34170, 'Do Weight', 'Einwaage');
    AddSingle(34180, 'Substance ID', 'Substanz-ID');
    AddSingle(34200, 'Get target temp.', 'Zieltemp. abfragen');
end;

{ TfrmTest }

procedure TfrmTest.RefreshDevicesView();
var
    x: integer;
    xTree: TStringTree;
begin
    tv.Items.Clear;
    xTree := TStringTree.Create();
    try
        if (rbAllDevices.Checked) then
            TDeviceSettingsManager.Instance.ShowDeviceHirarchy(gDeviceManager.Modules.GetNames, xTree)
        else
            TPeripheryManager.Instance.ShowLayoutDevices(xTree);

        TControlUtils.AddTreeNodeListToTreeNodes(xTree, tv.Items);
    finally
        FreeAndNil(xTree);
    end;

    for x := 0 to tv.Items.Count - 1 do
    begin
        tv.Items[x].ImageIndex := 1;
        tv.Items[x].SelectedIndex := 1;
    end;

    TSystemEvents.Instance.LayouterConnect();
end;

procedure TfrmTest.FormCreate(Sender: TObject);
begin
    TControlUtils.ResetFontForWinXP(self);
    fStringLoader := TTestFormStringLoader.Create;
    fStringLoader.LoadLanguage(self);

    fXYZPositionsChanged := false;

    HideAllDeviceTabs();
    fSwitchLocked := false;
end;

procedure TfrmTest.FormDestroy(Sender: TObject);
begin
    fStringLoader.Free;
end;

procedure TfrmTest.HideAllDeviceTabs();
var
    x: integer;
begin
    for x := 0 to pgctlDevices.PageCount - 1 do
    begin
        pgctlDevices.Pages[x].TabVisible := false;
    end;
end;

// --------------------------------------------------------------------------------------------------
procedure TfrmTest.Button7Click(Sender: TObject);
// --------------------------------------------------------------------------------------------------
var
    xIntf: IThermostatDevice;
begin
    if not Supports(FCurrentModule, IThermostatDevice, xIntf) then
        EXIT;
    xIntf.SetTargetTemp(SpinEdit2.Value + (SpinEdit1.Value / 10));
end;

procedure TfrmTest.Button13Click(Sender: TObject);
var
    xIntf: IThermostatDevice;
begin
    if not Supports(FCurrentModule, IThermostatDevice, xIntf) then
        EXIT;
    stTargetTemp.Caption := Format('%.1f', [xIntf.GetTargetTemp()]);
end;

// --------------------------------------------------------------------------------------------------
procedure TfrmTest.Button8Click(Sender: TObject);
// --------------------------------------------------------------------------------------------------
var
    xIntf: IThermostatDevice;
begin
    if not Supports(FCurrentModule, IThermostatDevice, xIntf) then
        EXIT;
    stCurrentTemp.Caption := Format('%.1f', [xIntf.ReadActualTemp()]);
end;

// --------------------------------------------------------------------------------------------------
procedure TfrmTest.btnBalInitClick(Sender: TObject);
// --------------------------------------------------------------------------------------------------
begin
    if not Supports(FCurrentModule, IBalanceDevice) then
        EXIT;
    CreateTestThread(TBalanceInitTestAction.Create(FCurrentModule));
end;

// --------------------------------------------------------------------------------------------------
procedure TfrmTest.btnOpenDoorClick(Sender: TObject);
// --------------------------------------------------------------------------------------------------
var
    xIntf: IBalanceDevice;
begin
    if (gErrorManager.IsGlobalErr) then
        EXIT;
    if not Supports(FCurrentModule, IBalanceDevice, xIntf) then
        EXIT;
    xIntf.OpenDoor();
end;

// --------------------------------------------------------------------------------------------------
procedure TfrmTest.btnBalTaraClick(Sender: TObject);
// --------------------------------------------------------------------------------------------------
begin
    if not Supports(FCurrentModule, IBalanceDevice) then
        EXIT;
    CreateTestThread(TBalanceTareTestAction.Create(FCurrentModule));
end;

// --------------------------------------------------------------------------------------------------
procedure TfrmTest.btnStoreWeightClick(Sender: TObject);
begin
    if not Supports(FCurrentModule, IBalanceDevice) then
        EXIT;
    CreateTestThread(TBalanceWeighTestAction.Create(FCurrentModule, Edit1.Text, StrToInt(Edit2.Text),
        Edit3.Text));
end;

procedure TfrmTest.ReadSwitchState();
var
    xIntfSwitch: ISwitchDevice;
begin
    fSwitchLocked := true;
    try
        if Supports(FCurrentModule, ISwitchDevice, xIntfSwitch) then
        begin
            if xIntfSwitch.HasDefaultPort() then
            begin
                case xIntfSwitch.SwitchState of
                    dswsOff:
                        rgrSwitch.ItemIndex := 0;
                    dswsDefault:
                        rgrSwitch.ItemIndex := 1;
                    dswsOn:
                        rgrSwitch.ItemIndex := 2;
                    dswsBothOn:
                        rgrSwitch.ItemIndex := 3;
                    else
                        rgrSwitch.ItemIndex := -1;
                end;
            end
            else
            begin
                case xIntfSwitch.SwitchState of
                    dswsOff:
                        rgrSwitch.ItemIndex := 0;
                    dswsOn:
                        rgrSwitch.ItemIndex := 1;
                    else
                        rgrSwitch.ItemIndex := -1;
                end;
            end;
        end;
    finally
        fSwitchLocked := false;
    end;
end;

// --------------------------------------------------------------------------------------------------
procedure TfrmTest.TVClick(Sender: TObject);
// --------------------------------------------------------------------------------------------------
var
    x: integer;
    xLiquidIdent, xLiquidName: string;
    xSysLiq: TSystemLiquid;
    xIntfSwitch: ISwitchDevice;
    xIntfMixer: IMixerDevice;
    xIntfXWayValve: IXWayValveDevice;
    xIntfPipPump: IPipPumpDevice;
    xIntfStateSignal: IStateSignalDevice;
begin
    HideAllDeviceTabs();

    if (tv.Selected = nil) then
        exit;

    if (tv.Selected.ImageIndex = 0) then
        exit;
    if not gModules.Find_ByName(tv.Selected.Text, IDevice, fCurrentModule) then
        EXIT;

    FillInfoTree();

    // ------------------------------------------------------------------------- Test-Groupbox öffnen
    if Supports(FCurrentModule, ILocationBasedMotionDevice) then
    begin
        shtLocationBasedMotionDevice.TabVisible := true;
        shtLocationBasedMotionDevice.Caption := FCurrentModule.Name;
    end;
    if Supports(FCurrentModule, IThermostatDevice) then
    begin
        shtThermostat.TabVisible := true;
        grbThermostat.Caption := FCurrentModule.Name;
    end;
    if Supports(FCurrentModule, ISwitchDevice, xIntfSwitch) then
    begin
        shtSwitch.TabVisible := true;
        rgrSwitch.Caption := FCurrentModule.Name;
        rgrSwitch.Items.Clear;
        if xIntfSwitch.HasDefaultPort() then
        begin
            rgrSwitch.Items.Add('Off');
            rgrSwitch.Items.Add('Default');
            rgrSwitch.Items.Add('On');
            rgrSwitch.Items.Add('Both On');
        end
        else
        begin
            rgrSwitch.Items.Add('Off');
            rgrSwitch.Items.Add('On');
        end;
        ReadSwitchState();
    end;
    if Supports(FCurrentModule, ISensorDevice) then
    begin
        shtSensor.TabVisible := true;
        grbSensor.Caption := FCurrentModule.Name;
        stSensor.Caption := '';
    end;
    if Supports(FCurrentModule, IBalanceDevice) then
    begin
        shtBalance.TabVisible := true;
        grbBalance.Caption := FCurrentModule.Name;
    end;
    if Supports(FCurrentModule, IDecapperDevice) then
    begin
        shtDecapper.TabVisible := true;
        grbDecapper.Caption := FCurrentModule.Name;
    end;
    if Supports(FCurrentModule, IBCReaderDevice) then
    begin
        shtBCReader.TabVisible := true;
        grbBCReader.Caption := FCurrentModule.Name;
    end;
    if Supports(FCurrentModule, IVarRediMotorDevice) then
    begin
        shtRedi.TabVisible := true;
        grbRediMotor.Caption := FCurrentModule.Name;
    end;
    if Supports(FCurrentModule, IMixerDevice, xIntfMixer) then
    begin
        shtShaker.TabVisible := true;
        grbCATShaker.Caption := FCurrentModule.Name;
        edSpeed.Text := IntToStr(xIntfMixer.Speed);
        edOnPulse.Text := IntToStr(xIntfMixer.OnPulse);
        edOffPulse.Text := IntToStr(xIntfMixer.OffPulse);
        rgShaker.ItemIndex := -1;
        rgFixation.ItemIndex := -1;
    end;
    if Supports(FCurrentModule, IXWayValveDevice, xIntfXWayValve) then
    begin
        shtXWayValve.TabVisible := true;
        grbXWayValve.Caption := FCurrentModule.Name;
        rgInPorts.Items.Clear;
        for x := 0 to xIntfXWayValve.InputPortCount - 1 do
        begin
            xLiquidIdent := xIntfXWayValve.InputPortName[x];
            xSysLiq := gSysLiqManager.FindCurrentLiquid(xLiquidIdent);
            xLiquidName := xSysLiq.Diluent;
            rgInPorts.Items.Add(xLiquidIdent + ' ... ' + xLiquidName);
        end;
        btnXWayReadPosClick(nil);
    end;
    if Supports(FCurrentModule, IPipPumpDevice, xIntfPipPump) then
    begin
        shtPipPump.TabVisible := true;
        grbDilutor.Caption := FCurrentModule.Name;
        xSysLiq := gSysLiqManager.PumpGetCurrentSysLiquid(xIntfPipPump);
        if Assigned(xSysLiq) then
            xLiquidName := xSysLiq.Diluent
        else
            ShowMessage('No system liquid connected - check settings');
        xLiquidIdent := xIntfPipPump.InputPortName;
        stDilutorIn.Caption := xLiquidIdent + ' ... ' + xLiquidName;
    end;
    if Supports(FCurrentModule, IStateSignalDevice, xIntfStateSignal) then
    begin
        shtStateSignal.TabVisible := true;
        edsStateSignalActiveSwitch.Caption := xIntfStateSignal.GetSwitchNameForState(sStateActive);
        edsStateSignalReadySwitch.Caption := xIntfStateSignal.GetSwitchNameForState(sStateReady);
        edsStateSignalErrorSwitch.Caption := xIntfStateSignal.GetSwitchNameForState(sStateInterruptError);
        edsStateSignalMessageSwitch.Caption := xIntfStateSignal.GetSwitchNameForState(sStateInterruptMessage);
        edsStateSignalTestSwitch.Caption := xIntfStateSignal.GetSwitchNameForState(sStateTest);
    end;
    if Supports(FCurrentModule, IPHMeterDevice) then
    begin
        shtPHMeter.TabVisible := true;
        grbPHMeter.Caption := FCurrentModule.Name;
        stPHMeter.Caption := '';
    end;
    if Supports(FCurrentModule, IThermometerDevice) then
    begin
        shtThermometer.TabVisible := true;
        grbThermometer.Caption := FCurrentModule.Name;
        stThermometer.Caption := '';
    end;
    if Supports(FCurrentModule, IMemoryDevice) then
    begin
        shtMemory.TabVisible := true;
        grbMemory.Caption := FCurrentModule.Name;
        stGetValue.Caption := '';
        edSetValue.Text := '';
    end;
    if Supports(FCurrentModule, ITriggerDevice) then
    begin
        shtTrigger.TabVisible := true;
        grbTrigger.Caption := FCurrentModule.Name;
    end;
end;

procedure TfrmTest.Button15Click(Sender: TObject);
var
    xIntf: ISensorDevice;
begin
    stSensor.Caption := '';
    if not Supports(FCurrentModule, ISensorDevice, xIntf) then
        EXIT;
    if xIntf.ValueIsDefault then
        stSensor.Caption := 'Default'
    else
        stSensor.Caption := 'Not default!!';

end;

procedure TfrmTest.btnSwitchInitRelayClick(Sender: TObject);
var
    xIntf: ISwitchDevice;
begin
    if not Supports(FCurrentModule, ISwitchDevice, xIntf) then
        EXIT;
    xIntf.Init(TDeviceInitHandling.GenerateInitID());
    ReadSwitchState();
end;

// --------------------------------------------------------------------------------------------------
procedure TfrmTest.rgrSwitchClick(Sender: TObject);
// --------------------------------------------------------------------------------------------------
var
    xIntf: ISwitchDevice;
begin
    if fSwitchLocked then
        EXIT;

    if not Supports(FCurrentModule, ISwitchDevice, xIntf) then
        EXIT;
    if xIntf.HasDefaultPort() then
    begin
        case (rgrSwitch.ItemIndex) of
            0:
                xIntf.SwitchOff(false);
            1:
                xIntf.SwitchToDefault(false);
            2:
                xIntf.SwitchOn(false);
            3:
                xIntf.SwitchBothOn(false);
        end;
    end
    else
    begin
        case (rgrSwitch.ItemIndex) of
            0:
                xIntf.SwitchOff(false);
            1:
                xIntf.SwitchOn(false);
        end;
    end;
end;

// --------------------------------------------------------------------------------------------------
procedure TfrmTest.rgFixationClick(Sender: TObject);
// --------------------------------------------------------------------------------------------------
var
    xIntf: IMixerDevice;
begin
    if not Supports(FCurrentModule, IMixerDevice, xIntf) then
        EXIT;
    xIntf.Fixation(rgFixation.ItemIndex = 1);
end;

// --------------------------------------------------------------------------------------------------
procedure TfrmTest.rgShakerClick(Sender: TObject);
// --------------------------------------------------------------------------------------------------
var
    xIntf: IMixerDevice;
begin
    if not Supports(FCurrentModule, IMixerDevice, xIntf) then
        EXIT;
    if (rgShaker.ItemIndex = 1) then
        xIntf.SetPulseSpeed(StrToInt(edSpeed.Text), StrToInt(edONPulse.Text), StrToInt(edOFFPulse.Text),
            vswIfSpeedIsNull)
    else
        xIntf.SetPulseSpeed(0, 0, 0, vswIfSpeedIsNull);
end;

// --------------------------------------------------------------------------------------------------
procedure TfrmTest.Button1Click(Sender: TObject);
// --------------------------------------------------------------------------------------------------
var
    xIntf: IVarRediMotorDevice;
begin
    if not Supports(FCurrentModule, IVarRediMotorDevice, xIntf) then
        EXIT;
    xIntf.Init(TDeviceInitHandling.GenerateInitID());
    xIntf.SetMinMaxVols(spinMinVol.Value, spinMaxVol.Value);
end;

// --------------------------------------------------------------------------------------------------
procedure TfrmTest.Button2Click(Sender: TObject);
// --------------------------------------------------------------------------------------------------
var
    xVarixMotorSteps: integer;
    xIntf: IVarRediMotorDevice;
begin
    if not Supports(FCurrentModule, IVarRediMotorDevice, xIntf) then
        EXIT;
    xVarixMotorSteps := spinVol.Value;
    xIntf.SetVol(240 / 250 * spinVol.Value, xVarixMotorSteps);
end;

function TfrmTest.CreateTestThread(const aAction: TModuleTestAction): TSysHandleID;
begin
    aAction.Control := self.Parent;
    result := gmCreateBasicActionHandlerThread(aAction, []);
end;

procedure TfrmTest.Button3Click(Sender: TObject);
begin
    if not Supports(FCurrentModule, IDecapperDevice) then
        EXIT;
    CreateTestThread(TDecapperTestAction.Create(ttDecapper_Decap, FCurrentModule));
end;

procedure TfrmTest.Button5Click(Sender: TObject);
begin
    if not Supports(FCurrentModule, IDecapperDevice) then
        EXIT;
    CreateTestThread(TDecapperTestAction.Create(ttDecapper_Cap, FCurrentModule));
end;

procedure TfrmTest.Button4Click(Sender: TObject);
begin
    if not Supports(FCurrentModule, IDecapperDevice) then
        EXIT;
    gBCString := '';
    edTubeBC.Text := '';
    CreateTestThread(TDecapperTestAction.Create(ttDecapper_Read, FCurrentModule));
    edTubeBC.Text := gBCString;
end;

procedure TfrmTest.Button6Click(Sender: TObject);
begin
    gBCString := '';
    edTubeBC.Text := '';
    if not Supports(FCurrentModule, IDecapperDevice) then
        EXIT;
    CreateTestThread(TDecapperTestAction.Create(ttDecapper_DecapNRead, FCurrentModule));
    edTubeBC.Text := gBCString;
end;

procedure TfrmTest.Button9Click(Sender: TObject);
begin
    gBCString := '';
    edTubeBC.Text := '';
    if not Supports(FCurrentModule, IDecapperDevice) then
        EXIT;
    gNoOfTurns := StrToInt(Edit4.text);
    CreateTestThread(TDecapperTestAction.Create(ttDecapper_DecapNCapNRead, FCurrentModule));
    edTubeBC.Text := gBCString;
end;

procedure TfrmTest.Button10Click(Sender: TObject);
var
    xIntf: IDecapperDevice;
begin
    if not Supports(FCurrentModule, IDecapperDevice, xIntf) then
        EXIT;
    xIntf.GoToBasicState;
end;

procedure TfrmTest.Button11Click(Sender: TObject);
begin
    if not Supports(FCurrentModule, IDecapperDevice) then
        EXIT;
    gNoOfTurns := StrToInt(Edit4.text);
    CreateTestThread(TDecapperTestAction.Create(ttDecapper_DecapNCap, FCurrentModule));
end;

procedure TfrmTest.Button12Click(Sender: TObject);
var
    xIntf: IBCReaderDevice;
begin
    edBarcode.Text := '';
    if not Supports(FCurrentModule, IBCReaderDevice, xIntf) then
        EXIT;
    edBarcode.Text := xIntf.LongRead;
end;

procedure TfrmTest.Button14Click(Sender: TObject);
var
    xBC: string;
    xIntf: IBCReaderDevice;
begin
    if not Supports(FCurrentModule, IBCReaderDevice, xIntf) then
        EXIT;
    xBC := edBarcode.Text;
    xIntf.ResetWithRead(xBC);
end;

procedure TfrmTest.btnShortReadClick(Sender: TObject);
var
    xIntf: IBCReaderDevice;
begin
    edBarcode.Text := '';
    if not Supports(FCurrentModule, IBCReaderDevice, xIntf) then
        EXIT;
    xIntf.TriggerOn;
    edBarcode.Text := xIntf.GetBarcode;
end;

var
    uDoNotTurnValve: boolean = false;

procedure TfrmTest.rgInPortsClick(Sender: TObject);
var
    xIntf: IXWayValveDevice;
begin
    if not Supports(FCurrentModule, IXWayValveDevice, xIntf) then
        EXIT;

    if (uDoNotTurnValve) then
        EXIT;
    if (rgInPorts.ItemIndex < 0) then
        EXIT;

    Screen.Cursor := crHourglass;
    rgInPorts.Enabled := false;
    xIntf.SimpleTurnValve(rgInPorts.ItemIndex);
    Screen.Cursor := crDefault;
    rgInPorts.Enabled := true;
end;

procedure TfrmTest.btnXWayReadPosClick(Sender: TObject);
var
    xIntf: IXWayValveDevice;
begin
    uDoNotTurnValve := true;
    rgInPorts.ItemIndex := -1;
    if not Supports(FCurrentModule, IXWayValveDevice, xIntf) then
        EXIT;

    Screen.Cursor := crHourglass;
    rgInPorts.Enabled := false;
    rgInPorts.ItemIndex := xIntf.GetCurrentPosition(true);
    Screen.Cursor := crDefault;
    rgInPorts.Enabled := true;

    uDoNotTurnValve := false;
end;

{ TModuleTestAction }

constructor TModuleTestAction.Create(const aCurrentModule: IDevice);
begin
    inherited Create();
    FCurrentModule := aCurrentModule;
    fControl := nil;
end;

procedure TModuleTestAction.ExecFirst();
begin
    gGUIManager.SetControlProp(fControl, cpEnabled, false);
    try
        DoExecute();
    finally
        gGUIManager.SetControlProp(fControl, cpEnabled, true);
    end;
end;

{ TDecapperTestAction }

constructor TDecapperTestAction.Create(aTestType: TDevTestType; const aCurrentModule: IDevice);
begin
    inherited Create(aCurrentModule);
    FTestType := aTestType;
end;

procedure TDecapperTestAction.Decapper_Cap;
var
    xIntf: IDecapperDevice;
begin
    if not Supports(FCurrentModule, IDecapperDevice, xIntf) then
        EXIT;

    xIntf.GoToBasicState;
    gGUIManager.MessageBox('Jetzt Tube hinstellen', '', 0);
    xIntf.TubeIntake;
    gGUIManager.MessageBox('Jetzt Tube loslassen', '', 0);
    xIntf.PutCapOn;
    gGUIManager.MessageBox('Jetzt Tube festhalten', '', 0);
    xIntf.TubeRelease;
    gGUIManager.MessageBox('Jetzt Tube nehmen', '', 0);
    xIntf.GoToBasicState;
end;

procedure TDecapperTestAction.Decapper_Decap;
var
    xIntf: IDecapperDevice;
begin
    if not Supports(FCurrentModule, IDecapperDevice, xIntf) then
        EXIT;
    xIntf.PrepareForDecap;
    gGUIManager.MessageBox('Jetzt Tube hinstellen', '', 0);
    xIntf.TubeIntake;
    gGUIManager.MessageBox('Jetzt Tube loslassen', '', 0);
    xIntf.TakeCapOff(true);
    gGUIManager.MessageBox('Jetzt Tube festhalten', '', 0);
    xIntf.TubeRelease;
    gGUIManager.MessageBox('Jetzt Tube nehmen', '', 0);
end;

procedure TDecapperTestAction.Decapper_DecapNCap(aNoOfTurns: integer);
var
    x: integer;
    xIntf: IDecapperDevice;
begin
    if not Supports(FCurrentModule, IDecapperDevice, xIntf) then
        EXIT;
    xIntf.PrepareForDecap;
    gGUIManager.MessageBox('Jetzt hinstellen', '', 0);
    xIntf.TubeIntake;

    gGUIManager.MessageBox('Jetzt loslassen', '', 0);
    x := 0;
    while (not gErrorManager.IsGlobalErr) and (x < gNoOfTurns) do
    begin
        xIntf.TakeCapOff(false);
        xIntf.PutCapOn;
        inc(x);
    end;
    gGUIManager.MessageBox('Jetzt Tube festhalten', '', 0);
    xIntf.TubeRelease;
    gGUIManager.MessageBox('Jetzt Tube nehmen', '', 0);
    xIntf.GoToBasicState;
end;

procedure TDecapperTestAction.Decapper_DecapNCapNRead(aNoOfTurns: integer);
var
    x: integer;
    xIntf: IDecapperDevice;
begin
    if not Supports(FCurrentModule, IDecapperDevice, xIntf) then
        EXIT;
    xIntf.PrepareForDecap;
    gGUIManager.MessageBox('Jetzt hinstellen', '', 0);
    xIntf.TubeIntake;
    gGUIManager.MessageBox('Jetzt loslassen', '', 0);
    x := 0;
    while (not gErrorManager.IsGlobalErr) and (x < gNoOfTurns) do
    begin
        gBCString := xIntf.TakeCapOffAndRead(false);
        xIntf.PutCapOn;
        inc(x);
    end;
    gGUIManager.MessageBox('Jetzt Tube festhalten', '', 0);
    xIntf.TubeRelease;
    gGUIManager.MessageBox('Jetzt Tube nehmen', '', 0);
    xIntf.GoToBasicState;
end;

procedure TDecapperTestAction.Decapper_DecapNRead;
var
    xIntf: IDecapperDevice;
begin
    if not Supports(FCurrentModule, IDecapperDevice, xIntf) then
        EXIT;
    xIntf.PrepareForDecap;
    gGUIManager.MessageBox('Jetzt Tube hinstellen', '', 0);
    xIntf.TubeIntake;
    gGUIManager.MessageBox('Jetzt Tube loslassen', '', 0);
    gBCString := xIntf.TakeCapOffAndRead(true);
    gGUIManager.MessageBox('Jetzt Tube festhalten', '', 0);
    xIntf.TubeRelease;
    gGUIManager.MessageBox('Jetzt Tube nehmen', '', 0);

end;

procedure TDecapperTestAction.Decapper_Read;
var
    xIntf: IDecapperDevice;
begin
    if not Supports(FCurrentModule, IDecapperDevice, xIntf) then
        EXIT;
    xIntf.GoToBasicState;
    gGUIManager.MessageBox('Jetzt Tube hinstellen', '', 0);
    xIntf.TubeIntake;
    gGUIManager.MessageBox('Jetzt Tube loslassen', '', 0);
    gBCString := xIntf.ReadTubeID;
    gGUIManager.MessageBox('Jetzt Tube festhalten', '', 0);
    xIntf.TubeRelease;
    gGUIManager.MessageBox('Jetzt Tube nehmen', '', 0);
end;

procedure TDecapperTestAction.DoExecute;
begin
    case (FTestType) of
        ttDecapper_Cap:
            Decapper_Cap;
        ttDecapper_Decap:
            Decapper_Decap;
        ttDecapper_Read:
            Decapper_Read;
        ttDecapper_DecapNRead:
            Decapper_DecapNRead;
        ttDecapper_DecapNCap:
            Decapper_DecapNCap(gNoOfTurns);
        ttDecapper_DecapNCapNRead:
            Decapper_DecapNCapNRead(gNoOfTurns);
    end;
end;

procedure TBalanceInitTestAction.DoExecute;
var
    xIntf: IBalanceDevice;
begin
    if (gErrorManager.IsGlobalErr) then
        EXIT;
    if not Supports(FCurrentModule, IBalanceDevice, xIntf) then
        EXIT;
    xIntf.Init(TDeviceInitHandling.GenerateInitID());
    xIntf.WaitForBalanceThread();
end;

procedure TBalanceTareTestAction.DoExecute;
var
    xIntf: IBalanceDevice;
begin
    if (gErrorManager.IsGlobalErr) then
        EXIT;
    if not Supports(FCurrentModule, IBalanceDevice, xIntf) then
        EXIT;
    xIntf.StartTare(0, 0);
    xIntf.WaitForBalanceThread();
end;

constructor TBalanceWeighTestAction.Create(const aCurrentModule: IDevice; const aRackID: string;
    const aPos: integer; const aSubstID: string);
begin
    inherited Create(aCurrentModule);
    fRackID := aRackID;
    fPos := aPos;
    fSubstID := aSubstID;
end;

procedure TBalanceWeighTestAction.DoExecute;
var
    xIntf: IBalanceDevice;
begin
    if (gErrorManager.IsGlobalErr) then
        EXIT;
    if not Supports(FCurrentModule, IBalanceDevice, xIntf) then
        EXIT;
    xIntf.StartWeight(fRackID, fPos, fSubstID, 0, 0, 0);

    xIntf.WaitForBalanceThread();
end;

procedure TfrmTest.tvGetSelectedIndex(Sender: TObject; Node: TTreeNode);
begin
    // TVClick( Sender );
end;

procedure TfrmTest.btnBCReaderInitClick(Sender: TObject);
var
    xIntf: IBCReaderDevice;
begin
    if (gErrorManager.IsGlobalErr) then
        EXIT;
    if not Supports(FCurrentModule, IBCReaderDevice, xIntf) then
        EXIT;
    xIntf.Init(TDeviceInitHandling.GenerateInitID);
end;

procedure TfrmTest.btnThermoInitClick(Sender: TObject);
var
    xIntf: IThermostatDevice;
begin
    if (gErrorManager.IsGlobalErr) then
        EXIT;
    if not Supports(FCurrentModule, IThermostatDevice, xIntf) then
        EXIT;
    xIntf.Init(TDeviceInitHandling.GenerateInitID());
end;

procedure TfrmTest.btnInitPipPumpClick(Sender: TObject);
var
    xIntf: IPipPumpDevice;
begin
    if not Supports(FCurrentModule, IPipPumpDevice, xIntf) then
        EXIT;
    xIntf.Init(TDeviceInitHandling.GenerateInitID());
    if xIntf.GetExecIntf <> nil then
        xIntf.GetExecIntf.Execute();

end;

procedure TfrmTest.btnPipPumpAspClick(Sender: TObject);
var
    xIntf: IPipPumpDevice;
begin
    if not Supports(FCurrentModule, IPipPumpDevice, xIntf) then
        EXIT;

    xIntf.Pick(StrToFloatDef(edPipPumpAspVol.Text, 0, TFormatUtils.GetSettingsEnglishUS), 0, false, true);

    if xIntf.GetExecIntf <> nil then
        xIntf.GetExecIntf.Execute();
end;

procedure TfrmTest.btnPipPumpDspClick(Sender: TObject);
var
    xIntf: IPipPumpDevice;
begin
    if not Supports(FCurrentModule, IPipPumpDevice, xIntf) then
        EXIT;

    xIntf.Disp(StrToFloatDef(edPipPumpDspVol.Text, 0, TFormatUtils.GetSettingsEnglishUS), 0, 0, false, true);

    if xIntf.GetExecIntf <> nil then
        xIntf.GetExecIntf.Execute();
end;

procedure TfrmTest.rdoStateSignalClick(Sender: TObject);
var
    xIntf: IStateSignalDevice;
begin
    if not Supports(FCurrentModule, IStateSignalDevice, xIntf) then
        EXIT;

    case rdoStateSignal.ItemIndex of
        0:
            xIntf.ChangeSignal(sStateActive);
        1:
            xIntf.ChangeSignal(sStateReady);
        2:
            xIntf.ChangeSignal(sStateInterruptError);
        3:
            xIntf.ChangeSignal(sStateInterruptMessage);
        4:
            xIntf.ChangeSignal(sStateTest);
    end;
end;

procedure TfrmTest.btnAskPHClick(Sender: TObject);
var
    xIntf: IPHMeterDevice;
begin
    if not Supports(FCurrentModule, IPHMeterDevice, xIntf) then
        EXIT;

    stPHMeter.Caption := FloatToStr(xIntf.GetPHValue(false, false, 0, 0, 0, false));
end;

procedure TfrmTest.btnAskTempClick(Sender: TObject);
begin
    // if not ( fCurrentModule is TThermometerDevice ) then EXIT;
    // stThermometer.Caption := FloatToStr( ( fCurrentModule as TThermometerDevice ).GetTemp() );
end;

procedure TfrmTest.btnShakerInitClick(Sender: TObject);
var
    xIntf: IMixerDevice;
begin
    if not Supports(FCurrentModule, IMixerDevice, xIntf) then
        EXIT;
    xIntf.Init(TDeviceInitHandling.GenerateInitID());
end;

procedure TfrmTest.btnTriggerClick(Sender: TObject);
var
    xIntf: ITriggerDevice;
begin
    if not Supports(FCurrentModule, ITriggerDevice, xIntf) then
        EXIT;
    xIntf.StartTrigger();
end;

procedure TfrmTest.btnWriteValueClick(Sender: TObject);
var
    xIntf: IMemoryDevice;
begin
    if not Supports(FCurrentModule, IMemoryDevice, xIntf) then
        EXIT;
    xIntf.WriteValueAsString(edSetValue.Text);
end;

procedure TfrmTest.btnReadValueClick(Sender: TObject);
var
    xIntf: IMemoryDevice;
begin
    if not Supports(FCurrentModule, IMemoryDevice, xIntf) then
        EXIT;
    stGetValue.Caption := xIntf.ReadValueAsString();
end;

procedure TfrmTest.FillSettingsToNode(aNode: TcxTreeListNode; aSettings: TModuleSettingList);
var
    x: integer;
    xChildNode: TcxTreeListNode;
    xModuleFound: boolean;
    xModule: IModule;
begin
    for x := 0 to aSettings.Count - 1 do
    begin

        xChildNode := aNode.AddChild();
        xChildNode.Texts[0] := aSettings[x].SettingName;
        xChildNode.Texts[1] := aSettings[x].Value;

        // Search for modules
        xModuleFound := false;
        if (aSettings[x] is TMSDevice) then
        begin
            if aSettings[x].Value <> '' then
                xModuleFound := gDeviceManager.FindModule(true, aSettings[x].Value,
                    (aSettings[x] as TMSDevice).ModuleID, xModule);
        end
        else if (aSettings[x] is TMSDriver) then
        begin
            if aSettings[x].Value <> '' then
                xModuleFound := gDriverManager.FindModule(true, aSettings[x].Value,
                    (aSettings[x] as TMSDriver).ModuleID, xModule);
        end
        else if (aSettings[x] is TMSConnection) then
        begin
            if aSettings[x].Value <> '' then
            begin
                xModuleFound := gConnectionManager.FindModule(true, aSettings[x].Value,
                    (aSettings[x] as TMSConnection).ModuleID, xModule);
            end;
        end;

        // call this method recursive if module has been found
        if (xModuleFound) then
        begin
            self.FillSettingsToNode(xChildNode, xModule.ModuleSettings);
        end;
    end;
    aNode.Expand(false);
end;

procedure TfrmTest.FillInfoTree();
var
    xNode: TcxTreeListNode;
begin
    self.cxTreeList1.Clear;
    xNode := self.cxTreeList1.Add();
    xNode.Texts[0] := 'DeviceName';
    xNode.Texts[1] := fCurrentModule.Name;

    FillSettingsToNode(xNode, fCurrentModule.ModuleSettings);
end;

procedure TfrmTest.rbAllDevicesClick(Sender: TObject);
begin
    RefreshDevicesView();
end;

procedure TfrmTest.rbLayoutDevicesClick(Sender: TObject);
begin
    RefreshDevicesView();
end;

procedure TfrmTest.Button16Click(Sender: TObject);
var
    xIntf: ILocationBasedMotionDevice;
    xForm: TfrmTeachControl;
begin
    if not Supports(FCurrentModule, ILocationBasedMotionDevice, xIntf) then
        EXIT;

    xForm := TfrmTeachControl.Create(nil);
    try
        xForm.MotionDevice := xIntf;
        xForm.ShowModal;
    finally
        xForm.Free;
    end;
end;

procedure TfrmTest.btnPipPumpTurnvalveClick(Sender: TObject);
var
    xIntf: IPipPumpDevice;
    xVPos: TValvePos;
begin
    if not Supports(FCurrentModule, IPipPumpDevice, xIntf) then
        EXIT;

    if rbSyrSys.Checked then
        xVPos := V_SYR_SYS
    else
        xVPos := V_SYR_TIP;

    xIntf.TurnValve(xVPos);

    if xIntf.GetExecIntf <> nil then
        xIntf.GetExecIntf.Execute();
end;


end.
