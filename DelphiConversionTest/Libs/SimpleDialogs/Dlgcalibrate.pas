{ --------------------------------------------------------------------------------------------------
  Ebene 2b (Liquid Handling)
  --------------------------------------------------------------------------------------------------
  Calibration Formular
  --------------------------------------------------------------------------------------------------
  Datum     op  function/procedure     Änderung / Neuerung
  --------  --  -------------------    -------------------------------------------------------------
  27.01.14  tp                         TN6341   Initializiert
  -------------------------------------------------------------------------------------------------- }

unit Dlgcalibrate;


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
    TFlushDialogGetSystemLiquidNamesEvent = function(): TArray<string> of object;
    TCalibrateDialogStartCalibrateMethodEvent = procedure(aGUILoadPreset, aGUIPresetName, aGUISystemLiq,
        aGUIDensity, aGUICVTolerance, aGUIMaxErrorPercent, aGUICalibrateWithSysLiq, aGUICountTries,
        aGUICountStep, aGUIManualFill, aGUIBalanceWait, aGUIBalancePrecision, aGUITipArray, aGUILHP: string)
        of object;

    TFlushStringLoader = class(TTextListStringLoader)
    protected
        procedure AddAllItems(); override;
    end;

    TdlgCalibSystem = class(TForm)
        GroupBox1: TGroupBox;
        GroupBox4: TGroupBox;
        Panel3: TPanel;
        btnStart: TButton;
        btnCancel: TButton;
        Label2: TLabel;
        GroupBox3: TGroupBox;
        Panel2: TPanel;
        Label4: TLabel;
        Label5: TLabel;
        Label3: TLabel;
        Label6: TLabel;
        Label7: TLabel;
        spnAveragingSteps: TSpinEdit;
        ProcedureName: TComboBox;
        spnMaxTries: TSpinEdit;
        spnMaxIncorrectness: TSpinEdit;
        spnMaxCV: TSpinEdit;
        spnDensity: TSpinEdit;
        CustomProcedureName: TEdit;
        cbUseSystemLiquid: TCheckBox;
        SystemLiquid: TComboBox;
        Label1: TLabel;
        Label8: TLabel;
        Label9: TLabel;
        Label10: TLabel;
        Label11: TLabel;
        LHP: TEdit;
        spnBalancePrecision: TSpinEdit;
        spnBalanceWait: TSpinEdit;
        spnManualFill: TSpinEdit;
        TipArray: TEdit;

        procedure FormCreate(Sender: TObject);
        procedure FormDestroy(Sender: TObject);
        procedure ProcedureNameChange(Sender: TObject);
        procedure SystemLiquidChange(Sender: TObject);
        procedure Start(Sender: TObject);
        procedure SetSystemLiquid(aSelection: string);
        procedure CustomProcedureNameChange(Sender: TObject);
    private const
        STR_FLUSH_INI_SECTION = 'Calibrate';
        STR_FLUSH_INI_CYCLES = 'Cycles';
        STR_FLUSH_INI_VOLUME = 'Volume';
        STR_FLUSH_INI_PERIPUMP = 'Peripump';
        STR_FLUSH_INI_USECH1 = 'UseCh1';
        STR_FLUSH_INI_USECH2 = 'UseCh2';
        STR_FLUSH_INI_PIPDEVICE = 'PipDevice';
        STR_FLUSH_INI_TIPMAP = 'Tipmap';
        STR_FLUSH_INI_DRY = 'DryAfterWash';
    public
        // Checkboxes for System Liquids
        fSysLiqChecks: TArray<TCheckBox>;

        // Arrays to store GUI data for each preset
        // Preset names
        fGUIPresetName: TArray<string>;
        // System Liquid choices of presets
        fGUISystemLiq: TArray<string>;
        // Dichte/Density of the calibrating liquid
        fGUIDensity: TArray<double>;
        // Maximum standard deviation after which to break calibration
        fGUICVTolerance: TArray<double>;
        // Maximum error in measurement (percent) after which to break calibration
        fGUIMaxErrorPercent: TArray<double>;
        // Choice of calibrating with System liquid for each preset
        fGUICalibrateWithSysLiq: TArray<boolean>;
        // Number of tries to correct the factor for each preset
        fGUICountTries: TArray<integer>;
        // Number of averaging steps for each try for each preset
        fGUICountStep: TArray<integer>;
        // The precision of the Balance for each preset
        fGUIBalancePrecision: TArray<integer>;
        // The waiting time for the Balance for each preset
        fGUIBalanceWait: TArray<integer>;
        // The volume for Manual Fill for each preset
        fGUIManualFill: TArray<integer>;
        // The Liquid Handling Parameter to use for each preset
        fGUILHP: TArray<string>;
        // The Tips to use for each preset
        fGUITipArray: TArray<string>;

        // Number of records
        fRecordCount: integer;
        // Load preset flag
        fGUILoadPreset: boolean;
        // CustomPreset name
        fGUICustomPreset: string;
        // Current Preset name
        fGUICurrPresetName: string;

        fAveragingSteps: integer;
        fMaxTries: integer;
        fMaxIncorrectness: double;
        fMaxCV: double;
        fDensity: double;
        fUseSystemLiquid: boolean;
        fSystemLiq: string;

        fManualFill: integer;
        fBalanceWait: integer;
        fBalancePrecision: integer;
        fTipArray: string;
        fLHP: string;

        fStringLoader: TFlushStringLoader;
        fPrepared: boolean;

        fOnGetSystemLiquidNames: TFlushDialogGetSystemLiquidNamesEvent;
    public
        procedure PrepareDialog;
        class procedure ShowDialog(aOnGetSystemLiquidNames: TFlushDialogGetSystemLiquidNamesEvent;
            aOnStartCalibrateMethod: TCalibrateDialogStartCalibrateMethodEvent);
    end;


implementation


{$R *.DFM}

uses
    Math,
    SysUtils,
    ControlUtils,
    Generics.Collections,
    GeneralTypes,
    ErrorInfo,
    ErrorMessageFactory,
    ErrorManager,
    AppSettings,
    ConfigurationFile,
    QueryDataAdaptor,
    StreamableDatasetClasses,
    TipMapUtils;

{ TFlushStringLoader }

procedure TFlushStringLoader.AddAllItems;
begin
    // Window title
    AddSingle(31020, 'Calibration', 'Kalibrierung');

    // Procedure panel title
    AddSingle(31180, 'Procedure', 'Procedure');
    // Options panel title
    AddSingle(31170, 'Options', 'Optionen');
    // System liquid panel title
    AddSingle(31030, 'System liquid', 'Lösemittel');

    // Field names for Procedure panel
    AddSingle(31220, 'Custom', 'Custom');

    // Field names for Options panel
    AddSingle(31040, 'Averaging steps', 'Anzahl der Schritte');
    AddSingle(31050, 'Max tries', 'Max Versuche');
    AddSingle(31150, 'Density', 'Dichte');
    AddSingle(31160, 'Max Error', 'Max Error (%)');
    AddSingle(31161, 'Max CV', 'Max CV');
    AddSingle(31051, 'Use System Liquid', 'Mit Lösemittel kalibrieren');
    AddSingle(31152, 'Tips', 'Tips');
    AddSingle(31153, 'Manual Fill Volume', 'Manual Fill Volume');
    AddSingle(31154, 'Balance Wait', 'Balance Wait');
    AddSingle(31155, 'Balance Precision', 'Balance Precision');
    AddSingle(31156, 'Liquid Handling Param', 'Liquid Handling Param');

    // Start and Cancel buttons
    AddSingle(32340, '&Start', '&Start');
    AddSingle(32350, '&Cancel', '&Abbrechen');

    // Unused
    AddSingle(31230, 'Use channel 1', 'Kanal 1 benutzen');
    AddSingle(31240, 'Use channel 2', 'Kanal 2 benutzen');
end;

{ TdlgCalibSystem }

procedure TdlgCalibSystem.FormCreate(Sender: TObject);
begin
    TControlUtils.ResetFontForWinXP(self);

    fStringLoader := TFlushStringLoader.Create;
    fStringLoader.LoadLanguage(self);
    fPrepared := false;
end;

procedure TdlgCalibSystem.FormDestroy(Sender: TObject);
begin
    FreeAndNil(fStringLoader);
end;

procedure TdlgCalibSystem.PrepareDialog;
var
    x: integer;
    xStrLen: integer;

    xLocalIniFile: IConfigurationSet;
    xCALIBPresetData: TQueryDataAdaptor;
    xError: string;
begin
    fGUILoadPreset := true;

    // Einstellungen laden
    xLocalIniFile := TConfigurationFile.Create(TAppsettings.LocalIniFileName);
    xLocalIniFile.Open(true);

    // Open CALIBPresets to read Preset data
    xCALIBPresetData := TQueryDataAdaptor.Create('CALIBPresets');

    try
        // Get all system liquids
        fGUISystemLiq := fOnGetSystemLiquidNames();
        TControlUtils.AddValuesToComboBox(fGUISystemLiq, SystemLiquid, true);

        // Read all the records
        xCALIBPresetData.SelectAndOpenAll(true);
        // Number of records in the Preset table
        fRecordCount := xCALIBPresetData.DataProvider.RecordCount;

        // Initialize internal arrays according to number of records
        SetLength(fGUIPresetName, fRecordCount);
        SetLength(fGUISystemLiq, fRecordCount);
        SetLength(fGUIDensity, fRecordCount);
        SetLength(fGUICVTolerance, fRecordCount);
        SetLength(fGUIMaxErrorPercent, fRecordCount);
        SetLength(fGUICalibrateWithSysLiq, fRecordCount);
        SetLength(fGUICountTries, fRecordCount);
        SetLength(fGUICountStep, fRecordCount);
        SetLength(fGUIBalancePrecision, fRecordCount);
        SetLength(fGUIBalanceWait, fRecordCount);
        SetLength(fGUIManualFill, fRecordCount);
        SetLength(fGUILHP, fRecordCount);
        SetLength(fGUITipArray, fRecordCount);

        // Default values
        spnDensity.Value := 1;
        spnMaxCV.Value := 5;
        spnMaxIncorrectness.Value := 5;
        cbUseSystemLiquid.Checked := false;
        spnMaxTries.Value := 1;
        spnAveragingSteps.Value := 1;

        fAveragingSteps := spnAveragingSteps.Value;
        fMaxTries := spnMaxTries.Value;
        fMaxIncorrectness := spnMaxIncorrectness.Value;
        fMaxCV := spnMaxCV.Value;
        fDensity := spnDensity.Value;
        fUseSystemLiquid := cbUseSystemLiquid.Checked;
        fSystemLiq := 'SYS01';

        spnBalancePrecision.Value := 2;
        spnBalanceWait.Value := 1;
        spnManualFill.Value := 1000;
        LHP.Text := 'Default';
        TipArray.Text := '1';

        fManualFill := spnManualFill.Value;
        fBalanceWait := spnBalanceWait.Value;
        fBalancePrecision := spnBalancePrecision.Value;
        fTipArray := TipArray.Text;
        fLHP := LHP.Text;

        // Read all the records and store in internal arrays
        if fRecordCount > 0 then
        begin
            x := 0;
            while not xCALIBPresetData.DataProvider.Eof do
            begin
                if (xCALIBPresetData.DataProvider.Fields.Count = 13) then
                begin
                    fGUIPresetName[x] := xCALIBPresetData.DataProvider.Fields[0].CopyData().AsStr;
                    fGUISystemLiq[x] := xCALIBPresetData.DataProvider.Fields[1].CopyData().AsStr;
                    fGUIDensity[x] := xCALIBPresetData.DataProvider.Fields[2].CopyData().AsFloat;
                    fGUICVTolerance[x] := xCALIBPresetData.DataProvider.Fields[5].CopyData().AsFloat;
                    fGUIMaxErrorPercent[x] := xCALIBPresetData.DataProvider.Fields[6].CopyData().AsFloat;
                    fGUICalibrateWithSysLiq[x] := xCALIBPresetData.DataProvider.Fields[7].CopyData().AsBool;
                    fGUICountStep[x] := xCALIBPresetData.DataProvider.Fields[3].CopyData().AsInt;
                    fGUICountTries[x] := xCALIBPresetData.DataProvider.Fields[4].CopyData().AsInt;

                    fGUIBalancePrecision[x] :=
                        Round(Log10(xCALIBPresetData.DataProvider.Fields[10].CopyData().AsInt));
                    fGUIBalanceWait[x] := xCALIBPresetData.DataProvider.Fields[9].CopyData().AsInt;
                    fGUIManualFill[x] := xCALIBPresetData.DataProvider.Fields[8].CopyData().AsInt;
                    fGUILHP[x] := xCALIBPresetData.DataProvider.Fields[12].CopyData().AsStr;
                    fGUITipArray[x] := xCALIBPresetData.DataProvider.Fields[11].CopyData().AsStr;
                    xStrLen := Length(fGUITipArray[x]);
                    fGUITipArray[x] := Copy(fGUITipArray[x], 3, xStrLen - 3);
                end
                // In case the fields for a record are incomplete, fill with default values
                else
                begin
                    xError := 'Incomplete Preset data found. Reverting to default values.';
                    fGUIPresetName[x] := 'Default';
                    fGUISystemLiq[x] := 'Incomplete_Preset_record';
                    fGUIDensity[x] := 1;
                    fGUICVTolerance[x] := 5;
                    fGUIMaxErrorPercent[x] := 5;
                    fGUICalibrateWithSysLiq[x] := false;
                    fGUICountStep[x] := 1;
                    fGUICountTries[x] := 1;

                    fGUIBalancePrecision[x] := 2;
                    fGUIBalanceWait[x] := 1;
                    fGUIManualFill[x] := 1000;
                    fGUILHP[x] := 'Default';
                    fGUITipArray[x] := '1';
                end;

                if (xError <> '') then
                begin
                    gErrorMessageFactory.ErrBoxSimple(xError, 'Preset data error', eibAbort);
                end;

                x := x + 1;
                xCALIBPresetData.DataProvider.Next;
            end;

            TControlUtils.AddValuesToComboBox(fGUIPresetName, ProcedureName, true);
            ProcedureName.ItemIndex := 0;

            // Set fGUILoadPreset to true and current Preset is set to ProcedureName[0]
            fGUILoadPreset := true;
            fGUICurrPresetName := fGUIPresetName[0];

            spnDensity.Value := Round(fGUIDensity[0]);
            spnMaxCV.Value := Round(fGUICVTolerance[0]);
            spnMaxIncorrectness.Value := Round(fGUIMaxErrorPercent[0]);
            cbUseSystemLiquid.Checked := fGUICalibrateWithSysLiq[0];
            spnAveragingSteps.Value := fGUICountStep[0];
            spnMaxTries.Value := fGUICountTries[0];
            SetSystemLiquid(fGUISystemLiq[0]);

            spnBalancePrecision.Value := Round(fGUIBalancePrecision[0]);
            spnBalanceWait.Value := Round(fGUIBalanceWait[0]);
            spnManualFill.Value := Round(fGUIManualFill[0]);
            LHP.Text := fGUILHP[0];
            TipArray.Text := fGUITipArray[0];

            fManualFill := spnManualFill.Value;
            fBalanceWait := spnBalanceWait.Value;
            fBalancePrecision := spnBalancePrecision.Value;
            fTipArray := TipArray.Text;
            fLHP := LHP.Text;

            fAveragingSteps := spnAveragingSteps.Value;
            fMaxTries := spnMaxTries.Value;
            fMaxIncorrectness := spnMaxIncorrectness.Value;
            fMaxCV := spnMaxCV.Value;
            fDensity := spnDensity.Value;
            fUseSystemLiquid := cbUseSystemLiquid.Checked;
            fSystemLiq := SystemLiquid.SelText;
        end
    finally
        xCALIBPresetData.Free;
        xLocalIniFile.Close;
    end;
    fGUILoadPreset := true;
end;

procedure TdlgCalibSystem.CustomProcedureNameChange(Sender: TObject);
begin
    fAveragingSteps := spnAveragingSteps.Value;
    fMaxTries := spnMaxTries.Value;
    fMaxIncorrectness := spnMaxIncorrectness.Value;
    fMaxCV := spnMaxCV.Value;
    fDensity := spnDensity.Value;
    fUseSystemLiquid := cbUseSystemLiquid.Checked;
    fSystemLiq := SystemLiquid.Text;
    // fGUISystemLiq[ProcedureName.ItemIndex];

    fManualFill := spnManualFill.Value;
    fBalanceWait := spnBalanceWait.Value;
    fBalancePrecision := spnBalancePrecision.Value;
    fTipArray := TipArray.Text;
    fLHP := LHP.Text;

    // Set the new selection as the chosen Preset
    fGUILoadPreset := false;
    fGUICurrPresetName := CustomProcedureName.Text;
end;

procedure TdlgCalibSystem.Start(Sender: TObject);
begin
    fAveragingSteps := spnAveragingSteps.Value;
    fMaxTries := spnMaxTries.Value;
    fMaxIncorrectness := spnMaxIncorrectness.Value;
    fMaxCV := spnMaxCV.Value;
    fDensity := spnDensity.Value;
    fUseSystemLiquid := cbUseSystemLiquid.Checked;
    fSystemLiq := SystemLiquid.Text;

    fManualFill := spnManualFill.Value;
    fBalanceWait := spnBalanceWait.Value;
    fBalancePrecision := spnBalancePrecision.Value;
    fTipArray := TipArray.Text;
    fLHP := LHP.Text;

    if CustomProcedureName.Text = 'Default' then
        fGUILoadPreset := false;
end;

procedure TdlgCalibSystem.ProcedureNameChange(Sender: TObject);
begin
    spnDensity.Value := Round(fGUIDensity[ProcedureName.ItemIndex]);
    spnMaxCV.Value := Round(fGUICVTolerance[ProcedureName.ItemIndex]);
    spnMaxIncorrectness.Value := Round(fGUIMaxErrorPercent[ProcedureName.ItemIndex]);
    cbUseSystemLiquid.Checked := fGUICalibrateWithSysLiq[ProcedureName.ItemIndex];
    SetSystemLiquid(fGUISystemLiq[ProcedureName.ItemIndex]);
    spnAveragingSteps.Value := fGUICountStep[ProcedureName.ItemIndex];
    spnMaxTries.Value := fGUICountTries[ProcedureName.ItemIndex];

    spnBalancePrecision.Value := Round(fGUIBalancePrecision[ProcedureName.ItemIndex]);
    spnBalanceWait.Value := Round(fGUIBalanceWait[ProcedureName.ItemIndex]);
    spnManualFill.Value := Round(fGUIManualFill[ProcedureName.ItemIndex]);
    LHP.Text := fGUILHP[ProcedureName.ItemIndex];
    TipArray.Text := fGUITipArray[ProcedureName.ItemIndex];

    fManualFill := spnManualFill.Value;
    fBalanceWait := spnBalanceWait.Value;
    fBalancePrecision := spnBalancePrecision.Value;
    fTipArray := TipArray.Text;
    fLHP := LHP.Text;

    fAveragingSteps := spnAveragingSteps.Value;
    fMaxTries := spnMaxTries.Value;
    fMaxIncorrectness := spnMaxIncorrectness.Value;
    fMaxCV := spnMaxCV.Value;
    fDensity := spnDensity.Value;
    fUseSystemLiquid := cbUseSystemLiquid.Checked;
    fSystemLiq := SystemLiquid.SelText;
    // fGUISystemLiq[ProcedureName.ItemIndex];

    // Set the new selection as the chosen Preset
    fGUILoadPreset := true;
    fGUICurrPresetName := ProcedureName.Text;
end;

procedure TdlgCalibSystem.SystemLiquidChange(Sender: TObject);
begin
    fSystemLiq := SystemLiquid.Text;
end;

procedure TdlgCalibSystem.SetSystemLiquid(aSelection: string);
var
    x: integer;
begin
    x := 0;
    while (x < SystemLiquid.DropDownCount) do
    begin
        if SystemLiquid.Items.Strings[x] = aSelection then
        begin
            SystemLiquid.ItemIndex := x;
        end;
        x := x + 1;
    end;
    fSystemLiq := SystemLiquid.Text;;
end;

class procedure TdlgCalibSystem.ShowDialog(aOnGetSystemLiquidNames: TFlushDialogGetSystemLiquidNamesEvent;
    aOnStartCalibrateMethod: TCalibrateDialogStartCalibrateMethodEvent);
var
    xCalibDialog: TdlgCalibSystem;
    xGUILoadPreset: string;
    xGUICurrPresetName: string;
    xGUISystemLiq: string;
    xGUIDensity: double;
    xGUIMaxCV: double;
    xGUIMaxIncorrectness: double;
    xGUIUseSystemLiquid: string;
    xGUIMaxTries: integer;
    xGUIAveragingSteps: integer;

    xManualFill: integer;
    xBalanceWait: integer;
    xBalancePrecision: integer;
    xTipArray: string;
    xLHP: string;
begin
    xCalibDialog := TdlgCalibSystem.Create(nil);
    try
        xCalibDialog.fOnGetSystemLiquidNames := aOnGetSystemLiquidNames;
        xCalibDialog.PrepareDialog;
        if (xCalibDialog.ShowModal = mrCancel) then
            EXIT;

        // Copy values from the Dialog for local use

        if xCalibDialog.fGUILoadPreset = True then
        begin
            xGUILoadPreset := 'True';
        end
        else
        begin
            xGUILoadPreset := 'False';
        end;

        if xCalibDialog.fUseSystemLiquid = True then
        begin
            xGUIUseSystemLiquid := 'True';
        end
        else
        begin
            xGUIUseSystemLiquid := 'False';
        end;

        xGUICurrPresetName := xCalibDialog.fGUICurrPresetName;
        xGUIDensity := xCalibDialog.fDensity;
        xGUIMaxCV := xCalibDialog.fMaxCV;
        xGUIMaxIncorrectness := xCalibDialog.fMaxIncorrectness;
        xGUIMaxTries := xCalibDialog.fMaxTries;
        xGUIAveragingSteps := xCalibDialog.fAveragingSteps;
        xGUISystemLiq := xCalibDialog.fSystemLiq;

        xManualFill := xCalibDialog.fManualFill;
        xBalanceWait := xCalibDialog.fBalanceWait;
        xBalancePrecision := xCalibDialog.fBalancePrecision;
        xTipArray := xCalibDialog.fTipArray;
        xLHP := xCalibDialog.fLHP;
    finally
        FreeAndNil(xCalibDialog);
    end;

    aOnStartCalibrateMethod(xGUILoadPreset, xGUICurrPresetName, xGUISystemLiq, FloatToStr(xGUIDensity),
        FloatToStr(xGUIMaxCV), FloatToStr(xGUIMaxIncorrectness), xGUIUseSystemLiquid, IntToStr(xGUIMaxTries),
        IntToStr(xGUIAveragingSteps), IntToStr(xManualFill), IntToStr(xBalanceWait),
        FloatToStr(Power(10, xBalancePrecision)), xTipArray, xLHP);
end;


end.
