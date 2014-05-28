{ --------------------------------------------------------------------------------------------------
  Ebene 2b (Liquid Handling)
  --------------------------------------------------------------------------------------------------
  Flush Formular
  --------------------------------------------------------------------------------------------------
  Datum     op  function/procedure     Änderung / Neuerung
  --------  --  -------------------    -------------------------------------------------------------
  21.11.97  dl  bitbtnok               peripump eingebaut mit checkbox zu aktivieren
  08.12.97  hc  formcreate             schleife für 6wayvalve von 1 bis 6 statt 0..5
  BitBtnOkClick          dito
  06.01.98  mo                         Neu : uses Main, FlushThr;
  formcreate             Flush über Threadaufruf neue Unit \Samintf\FlushThr
  BitBtnOkClick          dito
  05.02.98  dl  BitBtnOkClick          GlobalErr Abfrage in dilcnt schleife
  09.02.98  mo  BitBtnOkClick          TCheckBox(Components[dilcnt]).Color := clBtnFace; (-1 bei dilcnt entfernt)
  schleife für 6wayvalve von 1 bis 6 statt 0..NoOfDiluents
  10.02.98  hc  BitBtnOkClick          Warteschleife nach Flush aufruf
  formcreate             Warteschleife nach Flush aufruf
  23.06.98 wl                          uses SamWash (uses globals entfernt)
  09.07.98 wl   BitBtnOkClick          global.SamplerThread  statt  MainForm.SamplerThread
  FormCreate               dito
  22.07.98 wl                          uses dbTools statt uses Main
  24.07.98 wl   BitBtnCancelClick      Aufruf: global.UserInterrupt
  FormCreate             global.StartFlushThread
  BitBtnOkClick           dito
  29.07.98 wl  FormCreate,FormClose,BitBtnOkClick  global.SamThreadRunning statt SamplerActive
  11.08.98 wl                          uses ObjSampl statt dbTools
  08.10.98 wl  FormCreate              TFlushThread ersetzt durch TActionThread ('Init')
  BitBtnOkClick           TFlushThread ersetzt durch TActionThread ('Flush')
  CheckPeriPump steht am Anfang auf JA
  04.11.98 wl  BitBtnOkClick           OptionStr entweder 'Peri' oder 'NoPeri'
  BitBtnOkClick           bei GlobalErrorPtr<>0 wird immer das Fenster geschlossen
  FormCreate              CheckPeriPump.Checked abh. von UsePeriAtWash
  18.11.98 wl  FormCreate              Init-Thread ersetzt durch mdInitFirst
  01.02.99 wl  BitBtnOkClick           geänderter TFlush-Thread-Aufruf
  Stop-Button über Close-Button
  02.02.99 wl  FormClose               Close-Button hat keine Auswirkung, wenn Thread läuft
  08.06.99 wl  FormCreate              an SamWash angepaßt, undefinierte Liquids nicht sichtbar
  29.07.99 wl  FormCreate              Cycles := gFlushCycles
  17.08.99 wz                          ---> String in Ressourcen
  26.05.00 wl                         uses geändert
  30.05.00 wl                          gLiquids.UsePeriAtWash -> gUsePeriAtWash
  17.07.00 wl  TFlushThread            aus BasicLTh hierher verschoben
  25.07.00 wl  FormCreate              benutzt FindComponent (ist sicherer)
  15.11.00 mo  ExecFirst               Aufruf ExecuteAction Parameter geändert
  10.09.01 tbh BitBtnOkClick           TN1027 Volumen kann jetzt auch direkt übernommen werden
  10.09.01 tbh cbSetVolumeClick        TN1027 neu: statt Zyklen kann auch direkt das Spülvolumen definiert werden
  12.09.01 tbh Formular                TN1027 Ressourcen nachgetragen
  17.10.01 mo                          TN1067 Für SIAS überarbeitet
  18.07.02 mo                          TN1109 Die Systemflüssigkeiten werden dynamisch verwaltet
  09.09.02 mo  FormCreate              TN1109 Die erste Systemflüssigkeit ist immer ausgewählt
  09.10.02 wl                               TN1293.1 divided into rcRobotics and rcCommon
  12.12.02 wl  FormCreate              TN1345 Robot. statt Sam.
  27.12.02 wl                         TN1293.5 uses und WinlissyIniAccess geändert
  04.03.03 wl  FormCreate             TN1345 ifdef SIAS entfernt
  25.05.04 wl  TFlushThread.ExecFirst TN1716   Anzeige beim Spülen (grün) wird immmer synchronisiert
  24.06.04 pk  TFlushThread           TN2009.1 Converted to TDialogFlushAction
  28.06.04 pk                         TN2009.7  BasicLTh removed
  29.06.04 pk                         TN2009.8 Uses ActionLow
  16.02.05 wl                                TN2269   wenn Peripumpe gewählt ist, sind die System-liquids nicht zu sehen
  16.02.05 wl  TDialogFlushAction.ExecFirst  TN2269   wenn Peripumpe gewählt ist, werden keine System-liquids geschaltet!
  28.02.05 pk                         TN2314.1 uses ActionHandler
  15.04.05 wl  TDialogFlushAction.FlushDiluent  TN2379   Aufruf TFlushAction.Create mit neuen Parametern
  18.04.05 wl  cdAddDilutors                    TN2380.3 die Add.Dilutors werden auch gespült (wenn Checkbox gesetzt)
  25.05.05 wl  TdlgFlushSystem.FormCreate       TN2427   Alle Dialog-Einstellungen werden geladen (Appdata.tmp)
  25.05.05 wl  TdlgFlushSystem.FormClose        TN2427   Alle Dialog-Einstellungen werden gespeichert
  15.06.05 pk  TdlgFlushSystem                  TN2464   Choose Pipette arm from drop down
  21.06.05 pk                                   TN2464   Window made smaller
  23.06.05 wl  TdlgFlushSystem.FormCreate       TN2464   Dropdown-Box für Pipettierarm ist nur sichtbar, wenn es mehrere Arme gibt
  23.06.05 wl                                   TN2464   Buttons wieder wie vorher
  23.06.05 wl  TdlgFlushSystem.BitBtnOkClick    TN2463   UsePeriPump kann nicht mehr gesetzt sein, wenn keine Peripump vorhanden
  17.11.05 wl  TdlgFlushSystem.FormCreate       TN2771   AddDilutors werden zur Zeit nut vom 1.GripperArm gelesen
  05.01.06 pk                                   TN2877   TipManager find arm functions replaced by gModules find arm functions
  26.01.06 pk  BitBtnCancel.Cancel := false     TN2903   The cancel property caused the button to be pressed when the escape key was pressed
  24.04.06 pk                                   TN3067   New : Select tipmap using TipMapSelect frame
  23.05.06 pk                                   TN3112   Various changes
  18.09.06 pk                                   TN3227.1 function name GetDiluentIndices... changed to GetLiquidIndices..
  08.03.07 wl  TdlgFlushSystem.FormCreate       TN3620   gModules.FindPeriPump statt ModuleExist(PeriPump)
  13.03.07 pk                                   TN3633   Changes due to changes made in TipMapSelect
  17.07.07 pk                                   TN3752   There are now check boxes for choosing which channels should be used in flush
  09.01.08 wl                                   TN3972   beim Erzeugen von Actions wird ActionType nicht mehr übergeben
  14.04.08 wl                                   TN4060   uses DialogUtils
  20.06.08 pk                                   TN4139   uses changed
  03.07.08 wl                                   TN4157
  20.09.08 pk                                   TN4215   various changes
  23.09.08 wl  FlushDiluent                     TN4238   Aufruf von gmExecuteFlush mit DilIndex (0-basiert) statt mit Diluent (1-basiert)
  10.08.09 wl                                   TN4702   GetMethDlg wird nicht mehr erzeugt
  21.08.09 wl  fStringLoader                    TN4702   fStringLoader lädt Strings für Dialog-Elemente
  28.08.09 pk                                   TN4753   uses Liquids
  31.08.09 pk                                   TN4753   Changes due to TipMapSelect
  26.10.09 wl                                   TN4831    IConfigurationSet replaces ILocalIniFile
  04.11.09 pk                                   TN4843 Replace TObjectList, TStringList, TStrings with Generics Lists and TStringArray
  24.11.09 ts  cmbArmNameChange                 TN4901    if executed, full tipmap will be used (all tips are selected by default)
  13.04.10 wl                                   TN5044   uses geändert
  20.05.10 wl                                   TN5117   neue Schriftart "Segoe UI", Schriftgröße 9
  27.05.10 wl                                   TN5116   TDialogFlushAction --> in eigene unit
  27.05.10 wl                                   TN5116   Zugriffe auf PeripheryManagement ersetzt
  21.06.10 wl                                   TN5160   Position = poScreenCenter
  20.08.10 wl                                   TN5242   PipDevice und Tipmap werden jetzt gespeichert
  10.02.11 wl                                   TN5475   TfraTipMapSelect ist TForm statt TFrame
  20.04.12 wl  ShowDialog                       TN5946   Flush wird erst ausgeführt, wenn das Fenster zu ist
  10.08.12 wl                                   TN5947   gUsePeriAtWash durch true ersetzt
  14.11.12 wl                                   TN6014   Wenn beim Aufstarten nichts angekreuzt ist, wird die erste CheckBox angekreuzt
  04.12.12 wl                                   TN6044   überlagerte Fensterelemente verbessert
  24.04.13 wl                                   TN6137   TLiquids.Instance statt gLiquids
  03.09.13 wl  cbDryAfterWash                   TN6238   neue Checkbox
  18.09.13 wl                                   TN6252.3 Zugriffe auf Devices & EdExtern durch Events ersetzt
  18.09.13 wl                                   TN6252.3 für 2-Kanal-Geräte funktionsfähig gemacht
  30.09.13 wl                                   TN6260   Jetzt können Cycles UND Volume definiert werden
  -------------------------------------------------------------------------------------------------- }

unit dlgflush;


interface


uses
    Forms,
    Controls,
    Classes,
    ExtCtrls,
    StdCtrls,
    Spin,
    TipMapSelect,
    StringLoader,
    AppTypes;

type
    TFlushDialogPeriPumpExistsEvent = function(): boolean of object;
    TFlushDialogGetPipDeviceNamesEvent = function(): TArray<string> of object;
    TFlushDialogGetSystemLiquidNamesEvent = function(): TArray<string> of object;
    TFlushDialogHasMultiPumpsEvent = function(const aPipDeviceName: string): boolean of object;
    TFlushDialogGetLiquidIndicesEvent = function(const aPipDeviceName: string; aTips: TIPMAP;
        aPumpIndex1, aPumpIndex2: integer): TArray<integer> of object;
    TFlushDialogGetTipCountEvent = function(const aPipDeviceName: string): integer of object;
    TFlushDialogStartFlushMethodEvent = procedure(const aPipDeviceName: string; aTipMap: TIPMAP;
        aAddDilutorMap: TIPMAP; const aDiluentNames: TArray<string>; aVolume, aCycles: integer;
        aUseCh1, aUseCh2: boolean; aUsePeriPump, aUseAddDilutors: boolean; aDryAfterWash: boolean) of object;

    TFlushStringLoader = class(TTextListStringLoader)
    protected
        procedure AddAllItems(); override;
    end;

    TdlgFlushSystem = class(TForm)
        GroupBox1: TGroupBox;
        btnCheckAll: TButton;
        GroupBox4: TGroupBox;
        rbUseSyrVol: TRadioButton;
        rbDefineVolume: TRadioButton;
        spnCycles: TSpinEdit;
        spnVolume: TSpinEdit;
        grbOptions: TGroupBox;
        CheckPeriPump: TCheckBox;
        GroupBox2: TGroupBox;
        grbChannels: TGroupBox;
        chkUseCh1: TCheckBox;
        chkUseCh2: TCheckBox;
        Panel1: TPanel;
        Label1: TLabel;
        cmbArmName: TComboBox;
        Panel3: TPanel;
        btnStart: TButton;
        btnCancel: TButton;
        cbDryAfterWash: TCheckBox;
        Label2: TLabel;

        procedure FormCreate(Sender: TObject);
        procedure cbSetVolumeClick(Sender: TObject);
        procedure btnCheckAllClick(Sender: TObject);
        procedure CheckPeriPumpClick(Sender: TObject);
        function GetTipMap(): TIPMAP;
        procedure cmbArmNameChange(Sender: TObject);
        procedure FormDestroy(Sender: TObject);
        procedure CheckUseChannelClick(Sender: TObject);
    private const
        STR_FLUSH_INI_SECTION = 'FlushDialog';
        STR_FLUSH_INI_CYCLES = 'Cycles';
        STR_FLUSH_INI_VOLUME = 'Volume';
        STR_FLUSH_INI_PERIPUMP = 'Peripump';
        STR_FLUSH_INI_USECH1 = 'UseCh1';
        STR_FLUSH_INI_USECH2 = 'UseCh2';
        STR_FLUSH_INI_PIPDEVICE = 'PipDevice';
        STR_FLUSH_INI_TIPMAP = 'Tipmap';
        STR_FLUSH_INI_DRY = 'DryAfterWash';
    private
        FFlushChecks: TArray<TCheckBox>;
        fStringLoader: TFlushStringLoader;
        fLastPipDeviceName: string;
        fLastTipmap: integer;
        fraTipMapSelect1: TfraTipMapSelect;
        fPrepared: boolean;

        fOnPeriPumpExists: TFlushDialogPeriPumpExistsEvent;
        fOnGetPipDeviceNames: TFlushDialogGetPipDeviceNamesEvent;
        fOnGetSystemLiquidNames: TFlushDialogGetSystemLiquidNamesEvent;
        fOnHasMultiPumps: TFlushDialogHasMultiPumpsEvent;
        fOnGetLiquidIndices: TFlushDialogGetLiquidIndicesEvent;
        fOnGetTipCount: TFlushDialogGetTipCountEvent;

        procedure InitPipDevice(const aPipDeviceName: string; aTipmap: integer);
        procedure TipMapBoxChanged(aObject: TObject);
        procedure CheckNextLiqIfNothingIsChecked;
        procedure FlushCheckBoxChanged(aObject: TObject);
        function GetFlushChecks(out oUsePeriPump: boolean; out oDiluentNames: TArray<string>)
            : TArray<boolean>;

        procedure ShowTipMap(const aPipDeviceName: string; const aTipCount: integer; const aTipMap: TIPMAP);

        function PeriPumpExists(): boolean;
        function GetPipDeviceNames(): TArray<string>;
        function HasMultiPumps(const aPipDeviceName: string): boolean;
        function GetLiquidIndices(const aPipDeviceName: string; aTips: TIPMAP;
            aPumpIndex1, aPumpIndex2: integer): TArray<integer>;
        function GetTipCount(const aPipDeviceName: string): integer;
    public
        procedure PrepareDialog;
        class procedure ShowDialog(aOnPeriPumpExists: TFlushDialogPeriPumpExistsEvent;
            aOnGetPipDeviceNames: TFlushDialogGetPipDeviceNamesEvent;
            aOnGetSystemLiquidNames: TFlushDialogGetSystemLiquidNamesEvent;
            aOnHasMultiPumps: TFlushDialogHasMultiPumpsEvent;
            aOnGetLiquidIndices: TFlushDialogGetLiquidIndicesEvent;
            aOnGetTipCount: TFlushDialogGetTipCountEvent;
            aOnStartFlushMethod: TFlushDialogStartFlushMethodEvent);
    end;


implementation


{$R *.DFM}

uses
    SysUtils,
    ControlUtils,
    Generics.Collections,
    GeneralTypes,
    AppSettings,
    ConfigurationFile,
    TipMapUtils;

{ TFlushStringLoader }

procedure TFlushStringLoader.AddAllItems;
begin
    AddSingle(31020, 'Flush System', 'System spülen');
    AddSingle(31030, 'Diluent', 'Lösemittel');
    AddSingle(31040, 'Flush cycles:', 'Spülzyklen:');
    AddSingle(31050, 'Peripump', 'Peripumpe');
    AddSingle(31051, 'Dry after wash', 'Trocknen');
    AddSingle(31150, 'Amount', 'Menge');
    AddSingle(31160, 'Syringe volume [µL]', 'Spritzenvolumen [µL]');
    AddSingle(31161, 'Defined volume: [µL]', 'Definiertes Volumen: [µL]');
    AddSingle(31170, 'Pipetting tool syringes', 'Pumpen der Pipettiertools');
    AddSingle(31180, 'Options', 'Optionen');
    AddSingle(31220, 'Channel', 'Kanal');
    AddSingle(31230, 'Use channel 1', 'Kanal 1 benutzen');
    AddSingle(31240, 'Use channel 2', 'Kanal 2 benutzen');
    AddSingle(32340, '&Start', '&Start');
    AddSingle(32350, '&Cancel', '&Abbrechen');
end;

{ TdlgFlushSystem }

procedure TdlgFlushSystem.FormCreate(Sender: TObject);
begin
    TControlUtils.ResetFontForWinXP(self);

    fStringLoader := TFlushStringLoader.Create;
    fStringLoader.LoadLanguage(self);
    fPrepared := false;
end;

procedure TdlgFlushSystem.PrepareDialog;
var
    cnt: integer;
    xLocalIniFile: IConfigurationSet;
    xSystemLiquids: TArray<string>;
    xNames: TArray<string>;
begin
    // Einstellungen laden
    xLocalIniFile := TConfigurationFile.Create(TAppsettings.LocalIniFileName);
    xLocalIniFile.Open(true);
    try
        xSystemLiquids := fOnGetSystemLiquidNames();
        SetLength(FFlushChecks, Length(xSystemLiquids));
        for cnt := 0 to high(FFlushChecks) do
        begin
            FFlushChecks[cnt] := TCheckBox.Create(self.GroupBox1);
            FFlushChecks[cnt].Parent := self.GroupBox1;
            FFlushChecks[cnt].caption := xSystemLiquids[cnt];
            FFlushChecks[cnt].left := 28;
            FFlushChecks[cnt].top := 30 + (16 * (cnt + 1));
            FFlushChecks[cnt].visible := true;
            FFlushChecks[cnt].Checked := xLocalIniFile.ReadBool(STR_FLUSH_INI_SECTION,
                'Diluent' + IntToStr(cnt + 1), (cnt = 0));
            if FFlushChecks[cnt].top > self.GroupBox1.Height - 28 then
                self.height := self.height + 16;
        end;
        for cnt := 0 to high(FFlushChecks) do
            FFlushChecks[cnt].OnClick := FlushCheckBoxChanged;
        CheckNextLiqIfNothingIsChecked;
        FlushCheckBoxChanged(nil);

        spnCycles.Value := xLocalIniFile.ReadInteger(STR_FLUSH_INI_SECTION, STR_FLUSH_INI_CYCLES, 1);
        spnVolume.Value := xLocalIniFile.ReadInteger(STR_FLUSH_INI_SECTION, STR_FLUSH_INI_VOLUME, 0);
        CheckPeriPump.Checked := xLocalIniFile.ReadBool(STR_FLUSH_INI_SECTION, STR_FLUSH_INI_PERIPUMP, true);
        cbDryAfterWash.Checked := xLocalIniFile.ReadBool(STR_FLUSH_INI_SECTION, STR_FLUSH_INI_DRY, true);
        rbDefineVolume.Checked := spnVolume.Value > 0;
        chkUseCh1.Checked := xLocalIniFile.ReadBool(STR_FLUSH_INI_SECTION, STR_FLUSH_INI_USECH1, true);
        chkUseCh2.Checked := xLocalIniFile.ReadBool(STR_FLUSH_INI_SECTION, STR_FLUSH_INI_USECH2, true);
        fLastPipDeviceName := xLocalIniFile.ReadString(STR_FLUSH_INI_SECTION, STR_FLUSH_INI_PIPDEVICE, '');
        fLastTipmap := xLocalIniFile.ReadInteger(STR_FLUSH_INI_SECTION, STR_FLUSH_INI_TIPMAP, 0);
    finally
        xLocalIniFile.Close;
    end;

    CheckPeriPump.Visible := self.PeriPumpExists();
    CheckPeriPumpClick(nil);

    fraTipMapSelect1 := TfraTipMapSelect.Create(nil);
    fraTipMapSelect1.Parent := self.GroupBox2;
    fraTipMapSelect1.Visible := true;
    fraTipMapSelect1.Align := TAlign.alNone;
    fraTipMapSelect1.Top := 65;
    fraTipMapSelect1.Left := 2;

    self.fraTipMapSelect1.OnTipMapChanged := TipMapBoxChanged;
    self.fraTipMapSelect1.Init();

    xNames := self.GetPipDeviceNames();
    TControlUtils.AddValuesToComboBox(xNames, cmbArmName, true);
    InitPipDevice(fLastPipDevicename, fLastTipmap);

    fPrepared := true;
end;

procedure TdlgFlushSystem.FormDestroy(Sender: TObject);
begin
    FreeAndNil(fraTipMapSelect1);
    FreeAndNil(fStringLoader);
end;

procedure TdlgFlushSystem.TipMapBoxChanged(aObject: TObject);
var
    xList: TArray<integer>;
    x: integer;
    xPumpIndex1, xPumpIndex2: integer;
begin
    for x := 0 to high(FFlushChecks) do
    begin
        FFlushChecks[x].Enabled := false;
    end;
    xPumpIndex1 := -1;
    xPumpIndex2 := -1;
    if self.chkUseCh1.Checked then
        xPumpIndex1 := 0;
    if self.chkUseCh2.Checked then
        xPumpIndex2 := 1;

    xList := self.GetLiquidIndices(self.fraTipMapSelect1.PipDeviceName, self.fraTipMapSelect1.GetTipMap(),
        xPumpIndex1, xPumpIndex2);

    for x := 0 to high(xList) do
    begin
        FFlushChecks[xList[x]].Enabled := true;
    end;
end;

procedure TdlgFlushSystem.FlushCheckBoxChanged(aObject: TObject);
var
    xUsePeriPump: boolean;
    xDiluentNames: TArray<string>;
begin
    GetFlushChecks(xUsePeriPump, xDiluentNames);
    btnStart.Enabled := Length(xDiluentNames) > 0;
end;

procedure TdlgFlushSystem.CheckNextLiqIfNothingIsChecked;
var
    xUsePeriPump: boolean;
    xDiluentNames: TArray<string>;
    x: integer;
begin
    GetFlushChecks(xUsePeriPump, xDiluentNames);
    if (Length(xDiluentNames) > 0) then
        EXIT;

    // die nächstbeste CheckBox aktivieren
    for x := 0 to high(FFlushChecks) do
    begin
        if (FFlushChecks[x].Enabled) then
        begin
            FFlushChecks[x].Checked := true;
            EXIT;
        end;
    end;
end;

function TdlgFlushSystem.GetFlushChecks(out oUsePeriPump: boolean; out oDiluentNames: TArray<string>)
    : TArray<boolean>;
var
    xDiluentList: TList<string>;
    x: integer;
begin
    oUsePeriPump := self.CheckPeriPump.Visible and self.CheckPeriPump.Checked;

    SetLength(result, Length(self.fFlushChecks));
    xDiluentList := TList<string>.Create;
    try
        for x := 0 to high(self.fFlushChecks) do
        begin
            result[x] := false;
            if (self.fFlushChecks[x].Enabled and self.fFlushChecks[x].Checked) then
            begin
                result[x] := true;
                if (not oUsePeripump) then // bei Peripump nicht jedes SystemLiquid - das geht nicht!
                    xDiluentList.Add(self.fFlushChecks[x].Caption);
            end;
        end;

        if (oUsePeripump) then // bei Peripump nur ein SystemLiquid
            xDiluentList.Add(self.fFlushChecks[0].Caption);

        oDiluentNames := xDiluentList.ToArray;
    finally
        FreeAndNil(xDiluentList);
    end;
end;

function TdlgFlushSystem.GetLiquidIndices(const aPipDeviceName: string; aTips: TIPMAP;
    aPumpIndex1, aPumpIndex2: integer): TArray<integer>;
begin
    if Assigned(fOnGetLiquidIndices) then
        EXIT(fOnGetLiquidIndices(aPipDeviceName, aTips, aPumpIndex1, aPumpIndex2));

    EXIT(nil);
end;

function TdlgFlushSystem.GetPipDeviceNames: TArray<string>;
begin
    if Assigned(fOnGetPipDeviceNames) then
        EXIT(fOnGetPipDeviceNames());

    EXIT(nil);
end;

class procedure TdlgFlushSystem.ShowDialog(aOnPeriPumpExists: TFlushDialogPeriPumpExistsEvent;
    aOnGetPipDeviceNames: TFlushDialogGetPipDeviceNamesEvent;
    aOnGetSystemLiquidNames: TFlushDialogGetSystemLiquidNamesEvent;
    aOnHasMultiPumps: TFlushDialogHasMultiPumpsEvent; aOnGetLiquidIndices: TFlushDialogGetLiquidIndicesEvent;
    aOnGetTipCount: TFlushDialogGetTipCountEvent; aOnStartFlushMethod: TFlushDialogStartFlushMethodEvent);
var
    xFlushDialog: TdlgFlushSystem;
    xVolume: integer;
    xCycles: integer;
    xUsePeriPump, xDryAfterWash: boolean;
    xTipMap: TIPMAP;
    xUseCh1, xUseCh2: boolean;
    xPipDeviceName: string;
    xLocalIniFile: IConfigurationSet;
    xDiluentNames: TArray<string>;
    x: integer;
    xFlushChecks: TArray<boolean>;
begin
    xFlushDialog := TdlgFlushSystem.Create(nil);
    try
        xFlushDialog.fOnPeriPumpExists := aOnPeriPumpExists;
        xFlushDialog.fOnGetPipDeviceNames := aOnGetPipDeviceNames;
        xFlushDialog.fOnGetSystemLiquidNames := aOnGetSystemLiquidNames;
        xFlushDialog.fOnHasMultiPumps := aOnHasMultiPumps;
        xFlushDialog.fOnGetLiquidIndices := aOnGetLiquidIndices;
        xFlushDialog.fOnGetTipCount := aOnGetTipCount;
        xFlushDialog.PrepareDialog;

        if (xFlushDialog.ShowModal = mrCancel) then
            EXIT;

        if xFlushDialog.rbDefineVolume.Checked then
            xVolume := xFlushDialog.spnVolume.Value
        else
            xVolume := 0;
        xCycles := xFlushDialog.spnCycles.Value;
        xUseCh1 := xFlushDialog.chkUseCh1.Checked;
        xUseCh2 := xFlushDialog.chkUseCh2.Checked;
        xPipDeviceName := xFlushDialog.fraTipMapSelect1.PipDeviceName;
        xTipMap := xFlushDialog.GetTipMap();
        xDryAfterWash := xFlushDialog.cbDryAfterWash.Checked;

        xFlushChecks := xFlushDialog.GetFlushChecks(xUsePeriPump, xDiluentNames);
    finally
        FreeAndNil(xFlushDialog)
    end;

    // Daten speichern
    xLocalIniFile := TConfigurationFile.Create(TAppsettings.LocalIniFileName);
    xLocalIniFile.Open(false);
    try
        xLocalIniFile.WriteInteger(STR_FLUSH_INI_SECTION, STR_FLUSH_INI_CYCLES, xCycles);
        xLocalIniFile.WriteInteger(STR_FLUSH_INI_SECTION, STR_FLUSH_INI_VOLUME, xVolume);
        xLocalIniFile.WriteBool(STR_FLUSH_INI_SECTION, STR_FLUSH_INI_PERIPUMP, xUsePeriPump);
        xLocalIniFile.WriteBool(STR_FLUSH_INI_SECTION, STR_FLUSH_INI_USECH1, xUseCh1);
        xLocalIniFile.WriteBool(STR_FLUSH_INI_SECTION, STR_FLUSH_INI_USECH2, xUseCh2);
        xLocalIniFile.WriteString(STR_FLUSH_INI_SECTION, STR_FLUSH_INI_PIPDEVICE, xPipDeviceName);
        xLocalIniFile.WriteInteger(STR_FLUSH_INI_SECTION, STR_FLUSH_INI_TIPMAP, xTipMap);
        xLocalIniFile.WriteBool(STR_FLUSH_INI_SECTION, STR_FLUSH_INI_DRY, xDryAfterWash);

        for x := 0 to high(xFlushChecks) do
            xLocalIniFile.WriteBool(STR_FLUSH_INI_SECTION, 'Diluent' + IntToStr(x + 1), xFlushChecks[x]);
    finally
        xLocalIniFile.Close;
    end;

    // Flush starten
    aOnStartFlushMethod(xPipDeviceName, xTipMap, 0, xDiluentNames, xVolume, xCycles, xUseCh1, xUseCh2,
        xUsePeriPump, false, xDryAfterWash);
end;

procedure TdlgFlushSystem.ShowTipMap(const aPipDeviceName: string; const aTipCount: integer;
    const aTipMap: TIPMAP);
begin
    self.fraTipMapSelect1.SetTipMap(aPipDeviceName, aTipCount, aTipMap);

    grbChannels.Visible := self.HasMultiPumps(aPipDeviceName);

    if not grbChannels.Visible then
    begin
        chkUseCh1.Checked := true;
        chkUseCh2.Checked := false;
    end;
end;

procedure TdlgFlushSystem.InitPipDevice(const aPipDeviceName: string; aTipmap: integer);
var
    xTipCount, x, xTipMap: integer;
begin
    for x := 0 to cmbArmName.Items.Count - 1 do
    begin
        if cmbArmName.Items[x] = aPipDeviceName then
        begin
            // PipDevice und Tipmap werden geladen
            cmbArmName.Text := aPipDeviceName;
            xTipCount := self.GetTipCount(aPipDeviceName);
            xTipMap := TTipMapUtils.FullTipMap(xTipCount) and aTipmap;
            ShowTipMap(aPipDeviceName, xTipCount, xTipMap);
            EXIT;
        end;
    end;
    if cmbArmName.Items.Count > 0 then
    begin
        cmbArmName.ItemIndex := 0;
        cmbArmNameChange(nil);
    end;
end;

function TdlgFlushSystem.PeriPumpExists: boolean;
begin
    if Assigned(fOnPeriPumpExists) then
        EXIT(fOnPeriPumpExists());

    EXIT(false);
end;

procedure TdlgFlushSystem.CheckUseChannelClick(Sender: TObject);
begin
    if not fPrepared then
        EXIT;

    TipMapBoxChanged(Sender);
end;

procedure TdlgFlushSystem.cmbArmNameChange(Sender: TObject);
var
    xTipMap: TIPMAP;
    xTipCount: integer;
begin
    xTipCount := self.GetTipCount(cmbArmName.Text);
    xTipMap := TTipMapUtils.FullTipMap(xTipCount);
    ShowTipMap(cmbArmName.Text, xTipCount, xTipMap);
end;

function TdlgFlushSystem.GetTipCount(const aPipDeviceName: string): integer;
begin
    if Assigned(fOnGetTipCount) then
        EXIT(fOnGetTipCount(aPipDeviceName));

    EXIT(0);
end;

function TdlgFlushSystem.GetTipMap(): TIPMAP;
begin
    result := self.fraTipMapSelect1.GetTipMap();
end;

function TdlgFlushSystem.HasMultiPumps(const aPipDeviceName: string): boolean;
begin
    if Assigned(fOnHasMultiPumps) then
        EXIT(fOnHasMultiPumps(aPipDeviceName));

    EXIT(false);
end;

procedure TdlgFlushSystem.btnCheckAllClick(Sender: TObject);
var
    x: integer;
begin
    for x := 0 to high(fFlushChecks) do
    begin
        FFlushChecks[x].Checked := true;
    end;
end;

procedure TdlgFlushSystem.CheckPeriPumpClick(Sender: TObject);
begin
    GroupBox1.Visible := not(CheckPeriPump.Checked and CheckPeriPump.Visible);
end;

procedure TdlgFlushSystem.cbSetVolumeClick(Sender: TObject);
begin
    spnVolume.Visible := rbDefineVolume.Checked;
end;


end.
