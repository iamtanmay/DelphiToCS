{ --------------------------------------------------------------------------------------------------
  Copyright © 2006 Zinsser Analytic GmbH - All rights reserved.
  Project      : New Editor
  Author       : Wolfgang Lyncke
  Description  : Shows the part of the LiqPar.db that is used for powder handling
  --------------------------------------------------------------------------------------------------
  Revision History:
  date     op  method                       track-no  improvement/change
  -------- --  ---------------------------  --------  ----------------------------------------------
  03.10.06 wl                                TN3317   initial version
  04.10.06 wl                                TN3317   New Edits "Number of aspirations" & "Delay between aspirations"
  04.10.06 wl                                TN3317   New Checkbox "Use Aspiration Shifting"
  05.10.06 wl                                TN3317   verschiedene optische Korrekturen
  19.12.06 wl                                TN3409   class von TDockableEditForm in TViewItemEditForm geändert
  19.01.07 wl  SetParamName                  TN3473   Swich-Device-Liste zeigt jetzt auch Namen mit * statt einer Zahl
  05.02.07 wl  PrintData                     TN3543   Print-Funktion entfernt
  27.04.07 wl  LiqParTableBeforePost         TN3669   GetReason wird hier nicht mehr gebraucht
  27.04.07 wl  LiqParTableBeforeDelete       TN3669   entfernt (wird seit V 7.0 nicht mehr benutzt)
  06.06.07 wl                                TN3680   Feld SysAirAspDelay wird als "Shake Time [sec]" angezeigt
  12.02.08 wl  SetParamName                  TN4009   gModules.ListAllSwitches wieder aktiviert
  20.06.08 pk                                TN4139   uses changed
  03.07.08 wl                                TN4157
  25.09.08 wl                                TN4242   es gibt nur noch WashMethod, nicht mehr WashMacro
  16.01.09 wl                                TN4362   an Änderungen in TViewItem angepasst
  16.02.09 wl  TLiquidParEditorStringLoader  TN4370   neu: dezentrale Resourcen
  31.07.09 wl                                TN3950   Zeigt neue Felder: SampleAspInsertMoveType, DispInsertMoveType
  31.07.09 wl                                TN4018   neu: SingleTip für Move into well
  10.08.09 wl                                TN4702   Strings werden jetzt direkt geladen
  28.08.09 pk                                TN4753   gPipDeviceManager removed, functions now based on DesignModuleSettingFinder
  31.08.09 pk                                TN4753   uses ObjModul removed
  04.11.09 pk                                TN4843   Replace TObjectList, TStringList, TStrings with Generics Lists and TStringArray
  13.04.10 wl                                TN5044   uses geändert
  20.05.10 wl                                TN5117   neue Schriftart "Segoe UI", Schriftgröße 9
  17.06.10 wl  ShowTipNames                  TN5156   nur so viele Checkboxen wie es Tips gibt
  18.06.10 pk                                TN5152.1 TTable component removed and replaced by TDataProvider
  28.09.11 wl  SetParamName                  TN5703   durch MethodDataCache erscheint das Fenster schneller
  --------------------------------------------------------------------------------------------------- }

unit PowderParamEditor;


interface


uses
    Windows,
    StdCtrls,
    Forms,
    DBCtrls,
    DB,
    Mask,
    ExtCtrls,
    ComCtrls,
    Math,
    Spin,
    Buttons,
    SysUtils,
    dialogs,
    Classes,
    Controls,
    Grids,
    DBGrids,
    CommonTypes,
    ViewItemEditForm,
    ViewItem,
    StringLoader,
    DataProvider;

type
    TPowderParEditorStringLoader = class(TTextListStringLoader)
    protected
        procedure AddAllItems(); override;
    end;

    TfrmPowderParEditor = class(TViewItemEditForm)
        DataSource1: TDataSource;
        PageControl1: TPageControl;
        tbsSampleAsp: TTabSheet;
        rgrAspSwitchPos: TDBRadioGroup;
        GroupBox2: TGroupBox;
        GroupBox19: TGroupBox;
        tbsDispense: TTabSheet;
        GroupBox7: TGroupBox;
        EditDispDelay: TDBEdit;
        rgrDspSwitchPos: TDBRadioGroup;
        GroupBox17: TGroupBox;
        Label22: TLabel;
        Label23: TLabel;
        edDispStepVolume: TDBEdit;
        edDispStepDelay: TDBEdit;
        GroupBox18: TGroupBox;
        Panel3: TPanel;
        edDescription: TDBEdit;
        Label14: TLabel;
        GroupBox14: TGroupBox;
        edDispSubmerge: TDBEdit;
        GroupBox11: TGroupBox;
        Label5: TLabel;
        EditSampleAspSubmerge: TDBEdit;
        cbSAspSwitchModule: TDBComboBox;
        cbDispSwitchModule: TDBComboBox;
        tbsTipConfig: TTabSheet;
        grbTips: TGroupBox;
        Label1: TLabel;
        edUsedTips: TDBEdit;
        rbAllTips: TRadioButton;
        rbChooseTips: TRadioButton;
        cbUsedTipType: TDBComboBox;
        lblDispScanSpeed: TLabel;
        edDispScanSpeed: TDBEdit;
        lblSampleAspScanSpeed: TLabel;
        edSampleAspScanSpeed: TDBEdit;
        GroupBox13: TGroupBox;
        DBcbAspSingleRetract: TDBCheckBox;
        Label37: TLabel;
        DBEdit10: TDBEdit;
        Label4: TLabel;
        DBEdit2: TDBEdit;
        Label6: TLabel;
        EditSampleAspErrFlag: TDBCheckBox;
        cbSampleAspNoCalc: TCheckBox;
        pnSampleAspRetractPos: TPanel;
        EditSampleAspRetractPos: TDBEdit;
        Label16: TLabel;
        Label34: TLabel;
        cbDispNoCalc: TCheckBox;
        EditDispErrFlag: TDBCheckBox;
        GroupBox34: TGroupBox;
        Label13: TLabel;
        Label21: TLabel;
        Label42: TLabel;
        DBEdit12: TDBEdit;
        DBCheckBox8: TDBCheckBox;
        DBEdit3: TDBEdit;
        boxDispLevTr: TCheckBox;
        rbNoTracking: TRadioButton;
        rbLevelTracking: TRadioButton;
        rbRetractFromZMax: TRadioButton;
        tbsOptions: TTabSheet;
        gbWash: TGroupBox;
        cbWashAfterDisp: TDBCheckBox;
        pnWashAfterDisp: TPanel;
        cbWashIsForced: TDBCheckBox;
        pnWashMacro: TPanel;
        lbChooseMacro: TLabel;
        GroupBox38: TGroupBox;
        Label20: TLabel;
        Label52: TLabel;
        DBEdit4: TDBEdit;
        cbEmptyVarRedi: TDBCheckBox;
        edSampleAspLiqDet: TDBEdit;
        edDispLiqDet: TDBEdit;
        Label2: TLabel;
        edSampleAspSpitBackCount: TDBEdit;
        gbDelay: TGroupBox;
        Label7: TLabel;
        Label8: TLabel;
        edSpitBackDelay: TDBEdit;
        EditSampleAspDelay: TDBEdit;
        rgUseWashMacro: TDBRadioGroup;
        DBCheckBox1: TDBCheckBox;
        DBCheckBox2: TDBCheckBox;
        Label3: TLabel;
        edSysAirAspDelay: TDBEdit;
        Label9: TLabel;
        cbUsedPipDevice: TDBComboBox;
        tdbWashMacroName: TDBComboBox;
        boxDispLiqDetSingle: TCheckBox;
        DBRadioGroup1: TDBRadioGroup;
        boxSampleAspLiqDetSingle: TCheckBox;
        DBRadioGroup2: TDBRadioGroup;
        procedure edSampleAspLiqDetChange(Sender: TObject);
        procedure boxSampleAspLiqDetMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState;
            X, Y: Integer);
        procedure boxSampleAspGotoZMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState;
            X, Y: Integer);
        procedure boxSampleAspDispErrMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState;
            X, Y: Integer);
        procedure boxDispLiqDetMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState;
            X, Y: Integer);
        procedure rgrAspSwitchPosChange(Sender: TObject);
        procedure rgrDspSwitchPosChange(Sender: TObject);
        procedure boxDispLevTrMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState;
            X, Y: Integer);
        procedure boxDispGotoZMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState;
            X, Y: Integer);
        procedure boxDispDispErrMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState;
            X, Y: Integer);
        procedure edDispLiqDetChange(Sender: TObject);
        procedure boxSampleAspLevTrMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState;
            X, Y: Integer);
        procedure edUsedTipsChange(Sender: TObject);
        procedure rbAllTipsMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
        procedure rbChooseTipsMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState;
            X, Y: Integer);
        procedure cbWashAfterDispClick(Sender: TObject);
        procedure cbWashIsForcedClick(Sender: TObject);
        procedure cbUsedTipTypeChange(Sender: TObject);
        procedure rgUseWashMacroClick(Sender: TObject);
        procedure cbUsedPipDeviceChange(Sender: TObject);
        procedure DataSource1DataChange(Sender: TObject; Field: TField);
        procedure DataSource1UpdateData(Sender: TObject);
    private
        FcbTips: array of TCheckbox;
        fStringLoader: TPowderParEditorStringLoader;
        fDataProvider: TDataProvider;
        procedure SampleAspLiqDetUpdate;
        procedure DispLiqDetUpdate;
        procedure cbTipsMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
        procedure UpdateUsedTips;
        procedure ShowTipNames;
    protected
        procedure SaveData(); override;
        procedure ResetData(); override;
        procedure UnloadData(); override;
        function GetDataName(): string; override;
        function CreateViewItem(const aItemName: string): TViewItem; override;
        //
        procedure SetParamName(aName: string);
    public
        constructor Create(aOwner: TComponent; const aItemName: string;
            aOnSaveStatusChanged: TNotifyEvent); override;
        destructor Destroy; override;
    end;


    // ##################################################################################################


implementation


uses
    Utility2,
    SamGlobe,
    AppSettings,
    MethodDataCache,
    GeneralTypes,
    AppTypes,
    DataAdaptor,
    LiqHDataAdaptor,
    TipTypeDataAdaptor,
    SpecialViewItems,
    GUIManager,
    DesignModuleSettingFinder,
    ControlUtils,
    DatasetDataProvider,
    DataProviderFactory;

{$R *.DFM}
{ TPowderParEditorStringLoader }

procedure TPowderParEditorStringLoader.AddAllItems;
begin
    AddSingle(34990, 'Empty variable powder pipette', 'Variable Pulverpipette entleeren');
    AddDouble(35000, 'Save last changes (%s)?', 'Parameter Set not saved',
        'Sollen die letzten Änderungen (%s) gespeichert werden?',
        'Die Einstellungen wurden nicht gespeichert');
    AddDouble(35010, 'DEFAULT setting can not be deleted!', 'Error',
        'Die DEFAULT-Einstellung kann nicht gelöscht werden!', 'Fehler');
    AddDouble(35020, 'Do you really want to delete setting %s?', 'Warning',
        'Soll wirklich die Einstellung %s gelöscht werden?', 'Warnung');
    AddDouble(35050, 'Dispense system air volume must not be bigger than the aspirate air volume!', 'Error',
        'Das Abgabevolumen der Systemluft darf nicht größer als das Aufnahmevolumen sein!', 'Fehler');

    AddSingle(35060, 'Setting Name:', 'Name:');
    AddSingle(35062, 'Setting Name:', 'Setting Name:');
    AddSingle(35070, 'Description:', 'Beschreibung:');
    AddSingle(35080, 'Insert', 'Einfügen');
    AddSingle(35085, 'Insert new setting', 'Neue Einstellung einfügen');
    AddSingle(35090, 'Copy', 'Kopieren');
    AddSingle(35095, 'Copy all values to new setting', 'Alle Werte in neue Einstellung kopieren');
    AddSingle(35100, 'Report', 'Bericht');
    AddSingle(35105, 'Print setting report', 'Druckerbericht für Einstellungen');
    AddSingle(35110, 'Aspirate Sample', 'Aufnahme Probe');
    AddSingle(35115, 'Aspirate Sample Options', 'Aufnahme Probe Extras');
    AddSingle(35120, 'Aspirate Diluent', 'Aufnahme Systemfl.');
    AddSingle(35125, 'Split Aspiration Volume', 'Aufnahmevolumen aufteilen');
    AddSingle(35130, 'Dispense', 'Abgabe');
    AddSingle(35135, '.. if 2nd volume <', '..wenn 2.Volumen <');
    AddSingle(35140, 'Mix', 'Mixen');
    AddSingle(35145, '% of max.vol.', '% des Maxvol.');
    AddSingle(35150, 'Moving into the well', 'Anfahren der Z-Position');
    AddSingle(35160, 'Options', 'Extras');
    AddSingle(35170, 'Wash station', 'Waschstation');
    AddSingle(35180, 'Wash after dispense', 'Nach Abgabe waschen');
    AddSingle(35190, '> Volume [µL]  x F >', '> Volumen [µL] x F >');
    AddSingle(35200, 'Additional Aspiration', 'Zur Aufnahme');
    AddSingle(35210, 'Waste', 'Abfall');
    AddSingle(35220, 'Waste [% sample vol.]', 'Abfallvol. [% Probevol.]');
    AddSingle(35230, 'Spit Back', 'Spit-Back');
    AddSingle(35250, 'Disable Z-Error!', 'Z-Error abschalten!');
    AddSingle(35260, 'Use all tips (of that type)', 'Alle Spitzen dieses Typs benutzen');
    AddSingle(35270, 'Choose Tips', 'Spitzen wählen');
    AddSingle(35280, 'Multi-Pipetting (Multi-Dispense)', 'Multi-Pipetting (Multi-Dispense)');
    AddSingle(35290, 'Enable Multi Pipetting ', 'Multi-Pipetting aktivieren');
    AddSingle(35300, 'Max. Aspiration Volume:', 'Max. Aufnahmevolumen:');
    AddSingle(35305, 'Min. Aspiration Volume:', 'Min. Aufnahmevolumen:');
    AddSingle(35310, 'Split if Volume Greater than:', 'Aufteilen wenn Vol. >');
    AddSingle(35315, 'Tip Touch', 'Tip Touch');
    AddSingle(35320, 'Delay [sec]', 'Verzögerung [s]');
    AddSingle(35325, 'Waste Gap', 'Abfall Blase');
    AddSingle(35330, 'Waste', 'Abfall');
    AddSingle(35335, 'Air', 'Luft');
    AddSingle(35340, 'with single tips', 'Einzeln detektieren');
    AddSingle(35350, 'Liquid Detection', 'Liquid Detection');
    AddSingle(35360, 'Level Tracking', 'Level Tracking');
    AddSingle(35370, 'In case of error ..', 'Im Fehlerfall ..');
    AddSingle(35380, 'Goto Z-Max', 'Gehe zu Z-Max');
    AddSingle(35390, 'Display Error', 'Fehler anzeigen');
    AddSingle(35400, 'Goto Z-Disp', 'Gehe zu Z-Disp');
    AddSingle(35410, 'No tracking', 'Kein Tracking');
    AddSingle(35420, 'Submerge [mm]:', 'Eintauchen [mm]:');
    AddSingle(35430, 'Retract from Z-Max:', 'von Z-Max zurückziehen:');
    AddSingle(35440, 'Cycles :', 'Zyklen:');
    AddSingle(35450, 'Volume [µL]:', 'Volumen [µL]:');
    AddSingle(35455, 'Volume:', 'Volumen:');
    AddSingle(35460, 'Min. Rest Volume [µL]:', 'Min. Restvolumen [µL]:');
    AddSingle(35470, 'Mix Aspiration Speed [µL/s]:', 'Mix-Aufnahmegeschw. [µL/s]:');
    AddSingle(35480, 'Mix Dispense Speed [µL/s]:', 'Mix-Abgabegeschw. [µL/s]:');
    AddSingle(35490, 'Method :', 'Methode :');
    AddSingle(35500, 'Z-Max Offset [mm]:', 'Z-Max Offset [mm]:');
    AddSingle(35510, 'Before Sample Aspiration ', 'Vor der Aufnahme der Probe');
    AddSingle(35520, 'Method', 'Methode');
    AddSingle(35530, 'After Dispense ', 'Nach der Abgabe');
    AddSingle(35540, 'Mix only on first aspiration', 'Nur bei 1.Aufnahme mixen');
    AddSingle(35550, 'Tip Configuration', 'Spitzen-Konfiguration');
    AddSingle(35560, 'Tip Type:', 'Nadeltyp:');
    AddSingle(35570, 'System Air', 'Systemluft');
    AddSingle(35580, 'Diluent/', 'Lösemittel/');
    AddSingle(35590, 'Sample', 'Probe');
    AddSingle(35600, 'Transport Air', 'Transportluft');
    AddSingle(35610, ' Volume [µL] ', ' Volumen [µL] ');
    AddSingle(35620, 'Calculate', 'Berechnen');
    AddSingle(35630, 'Speed [µL/s] ', 'Geschw. [µL/s] ');
    AddSingle(35640, 'Delay [sec] ', 'Verzögerung [s] ');
    AddDouble(35650, 'Switch Port', 'Switch outside tube;Switch inside tube;Do not switch', 'Port schalten',
        'außerhalb des Röhrchens;innerhalb des Röhrchens;nicht schalten');
    AddSingle(35660, 'Step - Performing', 'Schrittweise Ausführung');
    AddSingle(35670, 'Aspiration Options', 'Optionen');
    AddSingle(35680, 'Single tip retract', 'Nadeln einzeln zurückziehen');
    AddDouble(35690, 'Transport Air Suck Pos.', 'Z-Travel;Z-Scan;Z-Dispense', 'Transportluft-Ansaugpos.',
        'Z-Travel;Z-Scan;Z-Dispense');
    AddSingle(35710, 'Store detected volume', 'Detektiertes Volumen speichern');
    AddSingle(35720, 'Choose Setting:', 'Einstellung auswählen:');
    AddSingle(35730, 'Pipetting Parameter Settings', 'Pipettierparameter');
    AddSingle(35740, 'Sample', 'Probe');
    AddSingle(35750, 'Diluent', 'Lösemittel');
    AddSingle(35760, 'Use Cones Without Disposable Tips', 'Koni ohne Wegwerfspitzen benutzen');
    AddDouble(35770, 'System Air Suck Pos.', 'Wash Station;Z Travel;Z Scan;Z Dispense',
        'Systemluft-Ansaugpos.', 'Waschstation;Z Travel;Z Scan;Z Dispense');
    AddSingle(35780, 'Use Channel 2', 'Kanal 2 benutzen');
    AddSingle(35790, 'After Aspiration', 'Nach Aufnahme');
    AddSingle(35795, 'Before Aspiration', 'Vor Aufnahme');
    AddSingle(35800, 'Wash tips with', 'Waschen mit');
    AddSingle(35810, 'Channel 2', 'Kanal 2');
    AddSingle(35820, 'Wash With Channel 2 [µL]:', 'Waschen mit Kanal 2 [µL]:');
    AddSingle(35830, 'Force Wash Step', 'Zwingend waschen');
    AddSingle(35845, 'Wash method name:', 'Name der Wasch-Methode:');
    AddDouble(35850, 'Kind of washing', 'Default;With a wash method', 'Art des Waschens',
        'Standard;Mit einer Wasch-Methode');
    AddSingle(35860, 'Change tip type', 'Tiptyp ändern');
    AddDouble(35870, 'Scan Mode (Sensitivity)', '1;2;3;4', 'Scan Mode (Sensitivity)', '1;2;3;4');
    AddSingle(35880, 'Retract Speed:', 'Rückzugsgeschw.:');
    AddSingle(35885, 'Scan Speed:', 'Scan-Geschw.:');
    AddSingle(35890, 'Wash Retract Speed:', 'Wasch-Rückzugsgeschw.:');
    AddSingle(35895, 'Insert Speed:', 'Einführgeschw.:');
    AddSingle(35900, 'Retract distance:', 'Rückzugsstrecke:');
    AddSingle(35910, 'Moving out of well', 'Herausfahren aus Well');
    AddSingle(35920, 'Dispense Options', 'Optionen');
    AddSingle(35930, 'Do not use level calculation', 'Keine Füllstandberechnug');
    AddSingle(35940, 'Dry tips after washing', 'Nadeln nach Waschen trocknen');
    AddSingle(35950, 'Use Class', 'Klasse');
    AddSingle(35960, 'Liquid Class', 'Flüssigkeitsklasse');
    AddSingle(35970, 'Class:', 'Klasse:');
    AddSingle(35980, 'F (wash volume factor):', 'F (Waschvolumenfaktor):');
    AddSingle(35985, 'Volume correction curve', 'Volumenkorrektur-Kurve');
    AddSingle(35990, 'Liquid Handling', 'Liquid Handling');
    AddSingle(35995, 'Powder Handling', 'Pulververteilung');
    AddSingle(36000, 'Tip type [%s] is not defined!', 'Tiptyp [%s] is nicht definiert!');
    AddSingle(36010, 'Liquid class [%s] is not defined!', 'Flüssigkeitsklasse [%s] is nicht definiert!');
    AddSingle(36020, 'Volume correction curve [%s] is not defined!',
        'Volumenkorrektur-Kurve [%s] ist nicht definiert!');
    AddSingle(36030, 'Aspiration', 'Aufnahme');
    AddSingle(36035, 'Aspiration', 'Aufnahme');
    AddSingle(36040, 'Number of powder aspirations:', 'Anzahl der Pulveraufnahmen:');
    AddSingle(36045, 'XY position shifting', 'XY-Position variieren');
    AddSingle(36050, 'Between aspirations:', 'Zwischen den Aufnahmen:');
    AddSingle(36055, 'After last aspiration:', 'Nach der letzten Aufnahme:');
    AddSingle(36060, 'Dispense at Z-Max - Z-Offset', 'Abgabe bei Z-Max - Z-Offset');
    AddSingle(36070, 'Mix with air', 'Mit Luft mixen');
    AddSingle(36080, 'Tracking', 'Tracking');
    AddSingle(36090, 'Liquid detection', 'Liquid Detection');
    AddSingle(36100, 'Shake Time [sec]:', 'Schüttelzeit [sec]:');
    AddDouble(36110, 'Kind of z-movement', 'Default;Block 1;Block 2;Block 3', 'Art der Z-Bewegung',
        'Standard;Block 1;Block 2;Block 3');
end;

{ TfrmPowderParEditor }

constructor TfrmPowderParEditor.Create(aOwner: TComponent; const aItemName: string;
    aOnSaveStatusChanged: TNotifyEvent);
begin
    inherited Create(aOwner, aItemName, aOnSaveStatusChanged);

    TControlUtils.ResetFontForWinXP(self);
    fStringLoader := TPowderParEditorStringLoader.Create;
    fStringLoader.LoadLanguage(self);

    fDataProvider := TDataProviderFactory.Instance.CreateDataProvider();
    fDataProvider.SelectAndOpen('SELECT * FROM LIQPARAM WHERE PARAMName = ''' + fViewItem.Name + '''', false);
    ASSERT(fDataProvider is TDatasetDataProvider);
    self.DataSource1.DataSet := (fDataProvider as TDatasetDataProvider).GetRealDataset;
    self.DataSource1.OnDataChange := self.DataSource1DataChange;
    self.DataSource1.OnUpdateData := self.DataSource1UpdateData;

    SetLength(fcbTips, 0);
    SetParamName(fViewItem.Name);
end;

procedure TfrmPowderParEditor.DataSource1DataChange(Sender: TObject; Field: TField);
begin
    ChangeData();
end;

procedure TfrmPowderParEditor.DataSource1UpdateData(Sender: TObject);
var
    xTipTypeData: TTipType;
begin
    // ------------------------------------------ Prüfen, ob Tiptyp existiert
    if not TTipTypeDataAdaptor.TipTypeExists(fDataProvider.FieldByName('UsedTipType').AsString,
        xTipTypeData) then
    begin
        fStringLoader.ResFMsgBox(36000 { Tip type [%s] is not defined! } , MB_ICONSTOP,
            [fDataProvider.FieldByName('UsedTipType').AsString]);
        abort;
    end;
end;

destructor TfrmPowderParEditor.Destroy;
begin
    self.DataSource1.DataSet := nil;
    FreeAndNil(fDataProvider);
    fStringLoader.Free;

    inherited;
end;

function TfrmPowderParEditor.CreateViewItem(const aItemName: string): TViewItem;
begin
    result := TPowderParViewItem.Create(aItemName);
end;

procedure TfrmPowderParEditor.SetParamName(aName: string);
var
    i: integer;
    xNames: TStringArray;
    x: integer;
begin
    // LiqParTable.SetRange([aName],[aName]);

    Caption := GetCaption();

    rgUseWashMacroClick(nil);

    pnSampleAspRetractPos.Visible := rbRetractFromZMax.Checked;
    // --------------------------------------------------------------------------------- Tips löschen
    for i := 0 to high(FcbTips) do
        if (FcbTips[i] <> nil) then
            FcbTips[i].Free;
    // -------------------------------------------------------------------------- CheckBoxen erzeugen
    rbChooseTips.Caption := fStringLoader.GetResString(35270 { Choose Tips } );
    SetLength(fcbTips, MAX_TIPS);
    for i := 0 to MAX_TIPS - 1 do
    begin
        FcbTips[i] := TCheckBox.Create(grbTips);
        FcbTips[i].Parent := grbTips;
        FcbTips[i].Left := 30;
        FcbTips[i].Width := 180;
        FcbTips[i].Top := rbChooseTips.Top + 20 + (i * 18);
        FcbTips[i].OnMouseUp := cbTipsMouseUp;
    end;
    if (gChangeTipTypes) then
    begin
        rbAllTips.Checked := false;
        rbChooseTips.Checked := true;
        rbAllTips.Visible := false;
        rbAllTips.Enabled := false;
    end;

    ShowTipNames;

    xNames := TDesignModuleSettingFinder.ListAllSwitches();
    for x := 0 to Length(xNames) - 1 do
        cbSAspSwitchModule.Items.Add(xNames[x]);

    xNames := TDesignModuleSettingFinder.ListAllSwitches();
    for x := 0 to Length(xNames) - 1 do
        cbDispSwitchModule.Items.Add(xNames[x]);

    // user restrictions:
    DataSource1.AutoEdit := false;

    // all tab sheets disabled
    tbsSampleAsp.Enabled := false;
    tbsDispense.Enabled := false;
    tbsOptions.Enabled := false;
    tbsTipConfig.Enabled := false;

    if gCommonDll.CurrentUser.HasLevel(usrSystemAdmin) then
    begin
        DataSource1.AutoEdit := true;

        // all tab sheets enabled
        tbsSampleAsp.Enabled := true;
        tbsDispense.Enabled := true;
        tbsOptions.Enabled := true;
        tbsTipConfig.Enabled := true;
    end;

    xNames := TTipTypeDataAdaptor.InstGetPowderTipTypeNames();
    for x := 0 to Length(xNames) - 1 do
        cbUsedTipType.Items.Add(xNames[x]);

    xNames := TDesignModuleSettingFinder.GetPipDeviceNames();
    for x := 0 to Length(xNames) - 1 do
        cbUsedPipDevice.Items.Add(xNames[x]);

    xNames := TMethodDataCache.Instance.GetMethodNames();
    for x := 0 to Length(xNames) - 1 do
        tdbWashMacroName.Items.Add(xNames[x]);
end;

// ==================================================================================================
// Used Tips
// --------------------------------------------------------------------------------------------------
procedure TfrmPowderParEditor.ShowTipNames;
var
    xNoOfTips, xMap, i: Integer;
begin
    xNoOfTips := TDesignModuleSettingFinder.ReadTipCount(self.cbUsedPipDevice.Text);
    gmStrToIntTry(xMap, edUsedTips.Text);
    if (xMap = 0) then
        rbAllTips.Checked := true
    else
        rbChooseTips.Checked := true;

    for i := 0 to high(fcbTips) do
    begin

        if not Assigned(FcbTips[i]) then
            break;

        FcbTips[i].Enabled := (xMap <> 0);
        FcbTips[i].Checked := (((XMap shr i) and 1) = 1);
        FcbTips[i].Caption := 'Tip ' + IntToStr(i + 1);
        FcbTips[i].Visible := i < xNoOfTips;

        // if (gChangeTipTypes) and (FcbTips[i].Checked) then
        // FcbTips[i].Caption := FcbTips[i].Caption + ': ' + cbUsedTipType.Text;// .Field.AsString;

        // if (not gChangeTipTypes) then
        // FcbTips[i].Caption := FcbTips[i].Caption + ': ' + gPipDeviceManager.Tips[i].TypeName
    end;
end;

procedure TfrmPowderParEditor.edUsedTipsChange(Sender: TObject);
begin
    ShowTipNames;
end;

// --------------------------------------------------------------------------------------------------
procedure TfrmPowderParEditor.cbTipsMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState;
    X, Y: Integer);
// --------------------------------------------------------------------------------------------------
begin
    UpdateUsedTips;
end;

// --------------------------------------------------------------------------------------------------
procedure TfrmPowderParEditor.rbAllTipsMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState;
    X, Y: Integer);
// --------------------------------------------------------------------------------------------------
var
    i: integer;
begin
    for i := 0 to high(fcbTips) do
        if (FcbTips[i] <> nil) then
        begin
            FcbTips[i].Enabled := false;
            FcbTips[i].Checked := false;
        end;
    UpdateUsedTips;
end;

// --------------------------------------------------------------------------------------------------
procedure TfrmPowderParEditor.rbChooseTipsMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState;
    X, Y: Integer);
// --------------------------------------------------------------------------------------------------
var
    i: integer;
begin
    for i := 0 to high(fcbTips) do
        if (FcbTips[i] <> nil) then
            FcbTips[i].Enabled := true;
    UpdateUsedTips;
end;

// --------------------------------------------------------------------------------------------------
procedure TfrmPowderParEditor.UpdateUsedTips;
// --------------------------------------------------------------------------------------------------
var
    i, Oldmap, Map: integer;
begin
    Map := 0;
    for i := 0 to high(fcbTips) do
        if (FcbTips[i] <> nil) then
        begin
            if (FcbTips[i].Checked) then
                Map := Map + Round(Power(2, i));
        end;
    gmStrToIntTry(OldMap, edUsedTips.Text);
    if (OldMap <> Map) then
    begin
        if Datasource1.DataSet.Active then
            Datasource1.DataSet.Edit;
        edUsedTips.Text := IntToStr(Map);
    end;
end;

// ==================================================================================================
// Liquid Detection: Sample Aspiration
// --------------------------------------------------------------------------------------------------
procedure TfrmPowderParEditor.edSampleAspLiqDetChange(Sender: TObject);
// --------------------------------------------------------------------------------------------------
var
    xMap: Integer;
begin
    gmStrToIntTry(xMap, edSampleAspLiqDet.Text);

    rbNoTracking.Checked := (xMap and INT_LQMODES_TRACKING) = 0;
    rbLevelTracking.Checked := (xMap and INT_LQMODES_TRACKING) <> 0;

    boxSampleAspLiqDetSingle.Checked := (xMap and INT_LQMODES_LQ_SINGLE_TIP) <> 0;
    cbSampleAspNoCalc.Checked := (xMap and INT_LQMODES_NO_CALCULATION) <> 0;

    if (fDataProvider.FieldByName('AspirationMethodRetractZ').AsBoolean) then
        rbRetractFromZMax.Checked := true;
end;

// --------------------------------------------------------------------------------------------------
procedure TfrmPowderParEditor.SampleAspLiqDetUpdate;
// --------------------------------------------------------------------------------------------------
var
    xMap, xTextMap: integer;
begin
    xMap := 0;
    if rbLevelTracking.Checked then
        xMap := xMap + INT_LQMODES_TRACKING;
    if boxSampleAspLiqDetSingle.Checked then
        xMap := xMap + INT_LQMODES_LQ_SINGLE_TIP;
    if cbSampleAspNoCalc.Checked then
        xMap := xMap + INT_LQMODES_NO_CALCULATION;

    if (fDataProvider.FieldByName('AspirationMethodRetractZ').AsBoolean <> rbRetractFromZMax.Checked) then
    begin
        fDataProvider.Edit;
        fDataProvider.FieldByName('AspirationMethodRetractZ').AsBoolean := rbRetractFromZMax.Checked;
        pnSampleAspRetractPos.Visible := rbRetractFromZMax.checked;
    end;

    gmStrToIntTry(xTextMap, edSampleAspLiqDet.Text);
    if (xTextMap <> xMap) then
    begin
        if fDataProvider.Active then
            fDataProvider.Edit;
        edSampleAspLiqDet.Text := IntToStr(xMap);
    end;
end;

// --------------------------------------------------------------------------------------------------
procedure TfrmPowderParEditor.boxSampleAspLiqDetMouseUp(Sender: TObject; Button: TMouseButton;
    Shift: TShiftState; X, Y: Integer);
// --------------------------------------------------------------------------------------------------
begin
    SampleAspLiqDetUpdate;
end;

// --------------------------------------------------------------------------------------------------
procedure TfrmPowderParEditor.boxSampleAspLevTrMouseUp(Sender: TObject; Button: TMouseButton;
    Shift: TShiftState; X, Y: Integer);
// --------------------------------------------------------------------------------------------------
begin
    SampleAspLiqDetUpdate;
end;

// --------------------------------------------------------------------------------------------------
procedure TfrmPowderParEditor.boxSampleAspGotoZMouseUp(Sender: TObject; Button: TMouseButton;
    Shift: TShiftState; X, Y: Integer);
// --------------------------------------------------------------------------------------------------
begin
    SampleAspLiqDetUpdate;
end;

// --------------------------------------------------------------------------------------------------
procedure TfrmPowderParEditor.boxSampleAspDispErrMouseUp(Sender: TObject; Button: TMouseButton;
    Shift: TShiftState; X, Y: Integer);
// --------------------------------------------------------------------------------------------------
begin
    SampleAspLiqDetUpdate;
end;

// ==================================================================================================
// Liquid Detection: Dispense
// --------------------------------------------------------------------------------------------------
procedure TfrmPowderParEditor.edDispLiqDetChange(Sender: TObject);
// --------------------------------------------------------------------------------------------------
var
    xMap: integer;
begin
    gmStrToIntTry(xMap, edDispLiqDet.Text);
    boxDispLevTr.Checked := (xMap and INT_LQMODES_TRACKING) <> 0;
    boxDispLiqDetSingle.Checked := (xMap and INT_LQMODES_LQ_SINGLE_TIP) <> 0;
    cbDispNoCalc.Checked := (xMap and INT_LQMODES_NO_CALCULATION) <> 0;
end;

// --------------------------------------------------------------------------------------------------
procedure TfrmPowderParEditor.DispLiqDetUpdate;
// --------------------------------------------------------------------------------------------------
var
    xMap, xTextMap: integer;
begin
    xMap := 0;
    if boxDispLevTr.Checked then
        xMap := xMap + INT_LQMODES_TRACKING;
    if boxDispLiqDetSingle.Checked then
        xMap := xMap + INT_LQMODES_LQ_SINGLE_TIP;
    if cbDispNoCalc.Checked then
        xMap := xMap + INT_LQMODES_NO_CALCULATION;

    gmStrToIntTry(xTextMap, edDispLiqDet.Text);
    if (xTextMap <> xMap) then
    begin
        if fDataProvider.Active then
            fDataProvider.Edit;
        edDispLiqDet.Text := inttostr(xMap);
    end;
end;

// --------------------------------------------------------------------------------------------------
procedure TfrmPowderParEditor.boxDispLiqDetMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState;
    X, Y: Integer);
// --------------------------------------------------------------------------------------------------
begin
    DispLiqDetUpdate;
end;

// --------------------------------------------------------------------------------------------------
procedure TfrmPowderParEditor.boxDispLevTrMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState;
    X, Y: Integer);
// --------------------------------------------------------------------------------------------------
begin
    DispLiqDetUpdate;
end;

// --------------------------------------------------------------------------------------------------
procedure TfrmPowderParEditor.boxDispGotoZMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState;
    X, Y: Integer);
// --------------------------------------------------------------------------------------------------
begin
    DispLiqDetUpdate;
end;

// --------------------------------------------------------------------------------------------------
procedure TfrmPowderParEditor.boxDispDispErrMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState;
    X, Y: Integer);
// --------------------------------------------------------------------------------------------------
begin
    DispLiqDetUpdate;
end;

// ==================================================================================================
// Switch Position
// --------------------------------------------------------------------------------------------------
procedure TfrmPowderParEditor.rgrAspSwitchPosChange(Sender: TObject);
// --------------------------------------------------------------------------------------------------
begin
    if (rgrAspSwitchPos.Value = '0') then
        cbSAspSwitchModule.visible := false
    else
        cbSAspSwitchModule.visible := true;
end;

// --------------------------------------------------------------------------------------------------
procedure TfrmPowderParEditor.rgrDspSwitchPosChange(Sender: TObject);
// --------------------------------------------------------------------------------------------------
begin
    if (rgrDspSwitchPos.Value = '0') then
        cbDispSwitchModule.visible := false
    else
        cbDispSwitchModule.visible := true;
end;

// --------------------------------------------------------------------------------------------------
procedure TfrmPowderParEditor.cbWashAfterDispClick(Sender: TObject);
// --------------------------------------------------------------------------------------------------
begin
    pnWashMacro.Visible := false;
    pnWashAfterDisp.Visible := cbWashAfterDisp.Checked;
    cbWashIsForced.Visible := cbWashAfterDisp.Checked;
    rgUseWashMacro.Visible := cbWashAfterDisp.Checked;
    if not(cbWashAfterDisp.Checked) then
    begin
        cbWashIsForced.Checked := false;
        pnWashAfterDisp.Visible := false;
    end;
    rgUseWashMacroClick(Sender);
end;

// --------------------------------------------------------------------------------------------------
procedure TfrmPowderParEditor.cbWashIsForcedClick(Sender: TObject);
// --------------------------------------------------------------------------------------------------
begin
    if cbWashIsForced.Checked then
    begin
        cbWashAfterDisp.Checked := true;
        rgUseWashMacro.Visible := true;
    end;
end;

procedure TfrmPowderParEditor.rgUseWashMacroClick(Sender: TObject);
begin
    if (rgUseWashMacro.ItemIndex = 0) then
    begin
        pnWashAfterDisp.Visible := cbWashAfterDisp.Checked;
        pnWashMacro.Visible := false;
    end
    else
    begin
        pnWashAfterDisp.Visible := false;
        pnWashMacro.Visible := cbWashAfterDisp.Checked;
        lbChooseMacro.Caption := fStringLoader.GetResString(35845 { Enter method name: } );
    end;
end;

// --------------------------------------------------------------------------------------------------
procedure TfrmPowderParEditor.cbUsedPipDeviceChange(Sender: TObject);
begin
    ShowTipNames;
end;

procedure TfrmPowderParEditor.cbUsedTipTypeChange(Sender: TObject);
// --------------------------------------------------------------------------------------------------
begin
    ShowTipNames;
end;

// --------------------------------------------------------------------------------------------------
procedure TfrmPowderParEditor.SaveData;
// --------------------------------------------------------------------------------------------------
begin

    if (self.DataSource1.State in [dsEdit, dsInsert]) then
        self.DataSource1.Dataset.Post;
end;

// --------------------------------------------------------------------------------------------------
procedure TfrmPowderParEditor.ResetData;
// --------------------------------------------------------------------------------------------------
begin
    if (self.DataSource1.Dataset.State in [dsEdit, dsInsert]) then
        self.DataSource1.Dataset.Cancel;
end;

// --------------------------------------------------------------------------------------------------
procedure TfrmPowderParEditor.UnloadData;
// --------------------------------------------------------------------------------------------------
begin
    self.DataSource1.OnDataChange := nil;
    self.DataSource1.OnUpdateData := nil;
    self.DataSource1.DataSet.Close();
end;

function TfrmPowderParEditor.GetDataName(): string;
begin
    result := fDataProvider.FieldByName('PARAMNAME').AsString;
end;


end.
