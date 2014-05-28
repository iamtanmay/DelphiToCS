{ --------------------------------------------------------------------------------------------------
  Copyright © 2002 Zinsser Analytic GmbH - All rights reserved.
  Author       : H.C. Enders
  Description  : Editor for wash programs
  --------------------------------------------------------------------------------------------------
  Revision History:
  date     op  method                       track-no improvement/change
  -------- --  ---------------------------  -------- -----------------------------------------------
  31.05.00 wl                         als WashEdit in EDITOR
  05.07.00 wl                         FormStyle = fsMDIChild
  06.07.00 wl  FormCreate             Pick-Liste mit Systemflüssigkeiten wird zu Beginn ausgefüllt
  27.07.00 wl                         Variablen ins Objekt verschoben
  27.07.00 wl  BitBtn3Click           Print-Aufruf verbessert
  08.08.00 wl  FormClose              Washedi:=nil
  14.08.00 wl                         --> String in Ressourcen
  01.09.00 wl  FormCreate,FormDestroy   Fenster-Position wird geladen und gespeichert
  05.09.00 wl  ChangeWPName           Zu Beginn werden Tabsheets gelöscht
  12.09.00 wl  FormClose,ChangeWPName vor dem Schließen des Fensters um dem Laden einer neuen Sequenz: Speichern???
  10.10.02 wl                               TN1293.2 Ini Access - uses geändert
  12.12.02 wl                               TN1345 uses geändert
  20.12.02 wl                               TN1293.5 uses und WinlissyIniAccess geändert
  26.02.04 wl  btnPrintClick                TN1574   Erzeugen von Quick-Report-Fenster in try-except-Block
  26.02.04 wl  SaveValues                   TN1574   benutzt am Schluß FStepCount statt i
  16.03.04 wl  SaveValues                   TN1819   Fehler: es wurde immer ein Schritt zuviel gelöscht
  02.04.04 wl  ChangeWPName                 TN1788   SeADVolume.MaxValue wird nicht mehr gesetzt
  30.04.04 wl  AddStepClick                 TN1862   Fokus wird auf den neuen Schritt gesetzt
  06.08.04 wl                               TN2008.2  FormStyle = fsNormal, Form derivated from TManualDockableForm
  16.02.05 wl                               TN2269    uses DeviceMain
  29.06.05 wl                               TN2444    an Änderungen von DockableForm angepasst
  26.08.05 pk  Create                       TN2566    call GetAllSystemLiquidNames
  03.10.05 wl                               TN2642    Property Visible = false
  08.11.05 wl                               TN2745    WashprogDataAdaptor
  24.11.05 pk  GetDataName                  TN2765    new
  21.03.06 wl  PrintData                    TN2906    Dataset von WASHPRIN wird wieder richtig gesetzt
  19.12.06 wl                               TN3409   class von TDockableEditForm in TViewItemEditForm geändert
  20.02.07 wl  PrintData                    TN3543   Print-Funktion entfällt, wenn Delphi 2006 benutzt wird
  23.07.07 wl  Add/DeleteStep               TN3792    entfernt: Es gibt nur noch einen Step
  23.07.07 wl                               TN3792    Oberfläche komplett überarbeitet
  08.08.07 wl                               TN3802    WashSt_Ch3Off wird wieder aus Settings gelesen
  08.08.07 wl                               TN3802    N2Time aufgeteilt in 2 Felder (vor und nach "Wash needle")
  09.11.07 pk                               TN3922    Dataset changed to DataProvider
  16.01.09 wl                               TN4362   an Änderungen in TViewItem angepasst
  06.07.09 pk                               TN4585.4 DataProvider.RealDataset removed
  10.08.09 wl                               TN4702   Strings werden jetzt direkt geladen
  20.08.09 wl  fStringLoader                TN4702   fStringLoader lädt Strings für Dialog-Elemente
  24.08.09 wl  fStringLoader                TN4702   Bugfix
  28.08.09 pk                               TN4753   Liquids replaced by DesignLiquids
  31.08.09 pk                               TN4753   uses ObjModul removed
  03.09.09 wl  PrintData                    TN4800   Print-Funktion endgültig entfernt
  04.11.09 pk                               TN4843   Replace TObjectList, TStringList, TStrings with Generics Lists and TStringArray
  10.12.09 pk                               TN4928   DataSource component removed. Now based on DataAdaptor ReadWashProg and WriteWashProg functions
  11.12.09 pk                               TN4167   New Radio group: Choose from Z-Scan or Z-Disp height for dispensing
  13.04.10 wl                               TN5044   uses geändert
  20.05.10 wl                               TN5117   neue Schriftart "Segoe UI", Schriftgröße 9
  14.11.12 wl                               TN6018   Neue Bilder eingefügt
  05.12.12 wl                               TN6045   DesignLiquids entfernt
  ------------------------------------------------------------------------------------------------- }

unit WashProgramEditor;


interface


uses
    Windows,
    Messages,
    SysUtils,
    Classes,
    Graphics,
    Controls,
    Forms,
    Dialogs,
    StdCtrls,
    ExtCtrls,
    ComCtrls,
    Buttons,
    AppTypes,
    DockableForm,
    ViewItem,
    WashprogDataAdaptor,
    ViewItemEditForm,
    StringLoader,
    dxGDIPlusClasses;

type
    TWashProgEditorStringLoader = class(TTextListStringLoader)
    protected
        procedure AddAllItems(); override;
    end;

    TfrmWashProgEditor = class(TViewItemEditForm)
        PageControl1: TPageControl;
        TabSheet1: TTabSheet;
        TabSheet2: TTabSheet;
        TabSheet3: TTabSheet;
        TabSheet4: TTabSheet;
        TabSheet5: TTabSheet;
        Image1: TImage;
        Image2: TImage;
        Image3: TImage;
        GroupBox1: TGroupBox;
        Label10: TLabel;
        Shape2: TShape;
        Shape1: TShape;
        Image4: TImage;
        edInnerVolume: TEdit;
        edWashN2Time: TEdit;
        Label17: TLabel;
        GroupBox4: TGroupBox;
        Shape3: TShape;
        Shape4: TShape;
        Label11: TLabel;
        edOuterVolume: TEdit;
        TabSheet6: TTabSheet;
        Image5: TImage;
        GroupBox6: TGroupBox;
        Label14: TLabel;
        Label19: TLabel;
        Label20: TLabel;
        edVolumeCh2: TEdit;
        edDispSpeed: TEdit;
        edAspDelay: TEdit;
        grpClean1: TGroupBox;
        chkSingleRetract: TCheckBox;
        GroupBox7: TGroupBox;
        Label21: TLabel;
        edN2Time1: TEdit;
        GroupBox8: TGroupBox;
        Label5: TLabel;
        edN2Time2: TEdit;
        grpWashNeedle: TGroupBox;
        Label7: TLabel;
        edCleanVolume: TEdit;
        edCleanDispSpeed: TEdit;
        Label16: TLabel;
        edCleanDelay: TEdit;
        Label8: TLabel;
        rdoCleanMode: TRadioGroup;
        rdoDispHeightPos: TRadioGroup;
        GroupBox2: TGroupBox;
        Label3: TLabel;
        Label2: TLabel;
        Label4: TLabel;
        edResinHeight: TEdit;
        edInsertSpeed: TEdit;
        edInsertDelay: TEdit;
        GroupBox3: TGroupBox;
        Label1: TLabel;
        Label13: TLabel;
        cbDiluent: TComboBox;
        chkInertGas: TCheckBox;
    private
        FWPName: string;
        fDataAdaptor: TWashprogDataAdaptor;
        fStringLoader: TWashProgEditorStringLoader;
        procedure DispHeightPosChanged();
        procedure ChangeWPName(const aWPName: string);
        procedure WashProgRecFromGUI(var vRec: TWashProgRec);
        procedure WashProgRecToGUI(const aRec: TWashProgRec);
        procedure SetChangeEvents;
        procedure OnEditChange(Sender: TObject);
        procedure OnCheckBoxChange(Sender: TObject);
        procedure OnRadioGroupChange(Sender: TObject);
        procedure SetFloatValueFromEdit(const aEdit: TEdit; var vValue: double);
        procedure SetIntValueFromEdit(const aEdit: TEdit; var vValue: integer);
    protected
        procedure SaveData(); override;
        procedure ResetData(); override;
        procedure UnloadData(); override;
        function GetDataName(): string; override;
        function CreateViewItem(const aItemName: string): TViewItem; override;
    public
        constructor Create(aOwner: TComponent; const aItemName: string;
            aOnSaveStatusChanged: TNotifyEvent); override;
        destructor Destroy; override;
    end;


implementation


{$R *.DFM}

uses
    SamGlobe,
    Utility2,
    AppSettings,
    Liquids,
    GUIManager,
    SpecialViewItems,
    GeneralTypes,
    DialogUtils,
    ControlUtils;

{ TWashProgEditorStringLoader }

procedure TWashProgEditorStringLoader.AddAllItems;
begin
    AddSingle(54100, '1. Aspirate', '1. Aufsaugen');
    AddSingle(54110, '2. Dispense and aspirate', '2. Abgeben und Aufsaugen');
    AddSingle(54120, '3. Clean with N2', '3. Reinigen mit N2');
    AddSingle(54130, '4. Wash needle', '4. Nadel waschen');
    AddSingle(54140, '5. Clean with N2', '5. Reinigen mit N2');
    AddSingle(54150, '6. Wash', '6. Waschen');
    AddSingle(54160, 'Start Options', 'Startoptionen');
    AddSingle(54170, 'System liquid', 'Systemflüssigkeit');
    AddSingle(54180, 'Inert gas on', 'Inertgas an');
    AddSingle(54190, 'Move to Z-Max minus Resin Height (and aspirate with vacuum)',
        'Gehe zu Z-Max minus Resinhöhe (und aspiriere mit Vakuum)');
    AddSingle(54200, 'Resin Height [mm]', 'Resin Höhe [mm]');
    AddSingle(54210, 'Track speed [Steps/s]', 'Trackgeschwindigkeit [Steps/s]');
    AddSingle(54220, 'Aspiration Time [s]', 'Saugzeit [s]');
    AddSingle(54230, 'Dispense from outer channel (and aspirate with vacuum)',
        'Abgabe aus dem äußeren Kanal (und Aspirieren mit Vakuum)');
    AddSingle(54240, 'Dispense Volume [µl]', 'Abgabevolumen [µl]');
    AddSingle(54250, 'Dispense Speed [Steps/s]', 'Abgabegeschwindigkeit [Steps/s]');
    AddSingle(54260, 'Aspiration Time [s]', 'Saugzeit [s]');
    AddSingle(54280, 'Single retract', 'Nadeln einzeln zurückziehen');
    AddSingle(54290, 'Blow with N2 from inner channel', 'Mit N2 ausblasen (Innerer Kanal)');
    AddSingle(54300, 'N2 Time [s]', 'N2-Zeit [s]');
    AddSingle(54320, 'Volume [µl]', 'Volumen [µL]');
    AddSingle(54330, 'Dispense Speed [Steps/s]', 'Abgabegeschwindigkeit [Steps/s]');
    AddSingle(54340, 'Delay after dispense [s]', 'Warten nach Abgabe [s]');
    AddDouble(54350, '', 'Switch on vacuum to wash needle;Switch off vacuum for precise dispense', '',
        'Vakuum anschalten, um die Nadel zu waschen;Vakumm ausschalten, um genau abzugeben');
    AddDouble(54360, 'Dispense Position', 'Z-Scan;Z-Dispense', 'Abgabeposition', 'Z-Scan;Z-Dispense');
    AddSingle(54400, 'Wash at WASTE-Rack', 'Wash im WASTE-Rack');
    AddSingle(54410, 'Volume inner channel [µl]', 'Volume innerer Kanal [µl]');
    AddSingle(54420, 'N2 Time to wash the channel [s]', 'N2-Zeit zum Waschen des Kanals [s]');
    AddSingle(54430, 'Wash at WASH-Rack', 'Wash im WASH-Rack');
    AddSingle(54440, 'Volume outer channel [µl]', 'Volume äußerer Kanal [µl]');

end;

{ TfrmWashProgEditor }

constructor TfrmWashProgEditor.Create(aOwner: TComponent; const aItemName: string;
    aOnSaveStatusChanged: TNotifyEvent);
var
    xNames: TStringArray;
    x: integer;
begin
    inherited Create(aOwner, aItemName, aOnSaveStatusChanged);

    TControlUtils.ResetFontForWinXP(self);
    fStringLoader := TWashProgEditorStringLoader.Create;
    fStringLoader.LoadLanguage(self);

    // Alle Pick-Listen mit Systemflüssigkeiten vervollständigen
    xNames := TLiquids.Instance.GetAllSystemLiquidNames();
    for x := 0 to Length(xNames) - 1 do
        cbDiluent.Items.Add(xNames[x]);

    fDataAdaptor := TWashprogDataAdaptor.Create;

    // DataSource1.DataSet := fDataAdaptor.DataProvider.RealDataset;
    ChangeWPName(fViewItem.Name);
    SetChangeEvents;
end;

destructor TfrmWashProgEditor.Destroy;
begin
    fDataAdaptor.Free;
    fStringLoader.Free;

    inherited;
end;

procedure TfrmWashProgEditor.DispHeightPosChanged();
begin
    if self.rdoDispHeightPos.ItemIndex = INT_DISPHEIGHTPOS_ZDISP then
    begin
        self.grpClean1.Caption := TLanguageString.Read('Move to Dispense Height', 'Gehe auf Dispense-Höhe');
        self.grpWashNeedle.Caption := TLanguageString.Read('Dispense at Dispense Height',
            'Abgeben auf Dispense-Höhe');
    end
    else
    begin
        self.grpClean1.Caption := TLanguageString.Read('Move to Scan Height', 'Gehe auf Scan-Höhe');
        self.grpWashNeedle.Caption := TLanguageString.
            Read('Dispense at Scan Height', 'Abgeben auf Scan-Höhe');
    end;
end;

procedure TfrmWashProgEditor.OnEditChange(Sender: TObject);
begin
    self.ChangeData;
end;

procedure TfrmWashProgEditor.OnCheckBoxChange(Sender: TObject);
begin
    self.ChangeData;
end;

procedure TfrmWashProgEditor.OnRadioGroupChange(Sender: TObject);
begin
    if Sender = self.rdoDispHeightPos then
    begin
        DispHeightPosChanged();
    end;
    self.ChangeData;
end;

function TfrmWashProgEditor.CreateViewItem(const aItemName: string): TViewItem;
begin
    result := TWashProgViewItem.Create(aItemName);
end;

procedure TfrmWashProgEditor.SetChangeEvents();
var
    x: integer;
    xComponent: TComponent;
begin
    for x := 0 to self.ComponentCount - 1 do
    begin
        xComponent := self.Components[x];
        if xComponent is TEdit then
            (xComponent as TEdit).OnChange := self.OnEditChange else if xComponent is TComboBox then
                (xComponent as TComboBox).OnChange := self.OnEditChange else if xComponent is TCheckBox then
                (xComponent as TCheckBox).OnClick := self.OnCheckBoxChange else if xComponent is TRadioGroup
            then (xComponent as TRadioGroup).OnClick := self.OnRadioGroupChange;
    end;

end;

procedure TfrmWashProgEditor.SetFloatValueFromEdit(const aEdit: TEdit; var vValue: double);
begin
    if aEdit.Text = '' then
        EXIT;
    vValue := StrToFloat(aEdit.Text);
end;

procedure TfrmWashProgEditor.SetIntValueFromEdit(const aEdit: TEdit; var vValue: integer);
begin
    if aEdit.Text = '' then
        EXIT;
    vValue := StrToInt(aEdit.Text);
end;

procedure TfrmWashProgEditor.UnloadData;
begin

end;

procedure TfrmWashProgEditor.WashProgRecToGUI(const aRec: TWashProgRec);
begin
    with aRec do
    begin
        self.cbDiluent.Text := aRec.Diluent;
        self.edResinHeight.Text := FloatToStr(aRec.Resin_Height);
        self.chkInertGas.Checked := aRec.InertGas;
        self.edInsertSpeed.Text := IntToStr(aRec.InsertSpeed);
        self.edInsertDelay.Text := FloatToStr(aRec.InsertDelay);

        self.edVolumeCh2.Text := FloatToStr(aRec.AspDispVol);
        self.edDispSpeed.Text := IntToStr(aRec.AspDispSpeed);
        self.edAspDelay.Text := FloatToStr(aRec.AspDispDelay);

        self.rdoDispHeightPos.ItemIndex := aRec.DispHeightPos;
        self.chkSingleRetract.Checked := aRec.SingleRetract;
        self.edN2Time1.Text := FloatToStr(aRec.N2Time1);

        self.edCleanVolume.Text := FloatToStr(aRec.Clean_Volume);
        self.edCleanDispSpeed.Text := IntToStr(aRec.CleanDispSpeed);
        self.edCleanDelay.Text := FloatToStr(aRec.CleanDelay);
        self.rdoCleanMode.ItemIndex := aRec.CleanMode;

        self.edN2Time2.Text := FloatToStr(aRec.N2Time2);

        self.edInnerVolume.Text := FloatToStr(aRec.InnerWashVolume);
        self.edWashN2Time.Text := FloatToStr(aRec.InnerWashN2Time);
        self.edOuterVolume.Text := FloatToStr(aRec.OuterWashVolume);
    end;
end;

procedure TfrmWashProgEditor.WashProgRecFromGUI(var vRec: TWashProgRec);
begin
    vRec := TWashProgDataAdaptor.MakeDefaultRec();
    vRec.WPName := fWPName;
    vRec.Diluent := self.cbDiluent.Text;

    SetFloatValueFromEdit(self.edResinHeight, vRec.Resin_Height);
    vRec.InertGas := self.chkInertGas.Checked;
    SetIntValueFromEdit(self.edInsertSpeed, vRec.InsertSpeed);
    SetFloatValueFromEdit(self.edInsertDelay, vRec.InsertDelay);

    SetFloatValueFromEdit(self.edVolumeCh2, vRec.AspDispVol);

    SetIntValueFromEdit(self.edDispSpeed, vRec.AspDispSpeed);
    SetFloatValueFromEdit(self.edAspDelay, vRec.AspDispDelay);

    vRec.DispHeightPos := self.rdoDispHeightPos.ItemIndex;
    vRec.SingleRetract := self.chkSingleRetract.Checked;
    SetFloatValueFromEdit(self.edN2Time1, vRec.N2Time1);

    SetFloatValueFromEdit(self.edCleanVolume, vRec.Clean_Volume);
    SetIntValueFromEdit(self.edCleanDispSpeed, vRec.CleanDispSpeed);
    SetFloatValueFromEdit(self.edCleanDelay, vRec.CleanDelay);
    vRec.CleanMode := self.rdoCleanMode.ItemIndex;

    SetFloatValueFromEdit(self.edN2Time2, vRec.N2Time2);

    SetFloatValueFromEdit(self.edInnerVolume, vRec.InnerWashVolume);
    SetFloatValueFromEdit(self.edWashN2Time, vRec.InnerWashN2Time);
    SetFloatValueFromEdit(self.edOuterVolume, vRec.OuterWashVolume);
end;

function TfrmWashProgEditor.GetDataname(): string;
begin
    result := FWPName;
end;

procedure TfrmWashProgEditor.ChangeWPName(const aWPName: string);
var
    xWashProgRec: TWashProgRec;
begin
    FWPName := aWPName;
    Caption := GetCaption();

    if not fDataAdaptor.ReadWashProg(FWPName, xWashProgRec) then
    begin
        xWashProgRec := TWashProgDataAdaptor.MakeDefaultRec();
        xWashProgRec.WPName := FWPName;
    end;
    self.WashProgRecToGUI(xWashProgRec);

    DispHeightPosChanged();

end;

procedure TfrmWashProgEditor.SaveData();
var
    xWashProgRec: TWashProgRec;
begin
    inherited;

    self.WashProgRecFromGUI(xWashProgRec);
    fDataAdaptor.WriteWashProg(xWashProgRec);
end;

procedure TfrmWashProgEditor.ResetData;
begin

end;


end.
