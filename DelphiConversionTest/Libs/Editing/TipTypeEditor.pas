{ ----------------------------------------------------------------------------------------------------------------------
  Copyright © 2008 Zinsser Analytic GmbH - All rights reserved.
  Author       : Wolfgang Lyncke (wl)
  Description  : Editor for TIPTYPES.
  ----------------------------------------------------------------------------------------------------------------------
  Revision History:
  date     op  method                       track-no  improvement/change
  -------- --  ---------------------------  --------  ------------------------------------------------------------------
  22.04.08 wl                               TN4073    initial version
  23.04.08 wl  NewTipType                   TN4073    degugged
  24.04.08 wl                               TN4073    ComboBox for BasicTipType
  24.04.08 wl                               TN4073.1  DoNotInit (nur V7.1.x) wird jetzt auch in TIPTYPES geführt
  02.05.08 wl  DeleteTipType                TN4094    The request did not work
  16.01.09 wl                               TN4362   an Änderungen in TQueryDataAdaptor angepasst
  09.02.09 wl  TTipTypeEditorStringLoader   TN4370    neu: dezentrale Resourcen
  06.07.09 pk                               TN4585.4  reference to RealDataset removed. Must be reimplemented
  13.07.09 pk                               TN4585.4  Reimplemented without using RealDataset
  10.08.09 wl                               TN4702   Strings werden jetzt direkt geladen
  04.11.09 pk                               TN4843   Replace TObjectList, TStringList, TStrings with Generics Lists and TStringArray
  20.05.10 wl                               TN5117   neue Schriftart "Segoe UI", Schriftgröße 9
  21.06.10 wl                               TN5160   Position = poScreenCenter
  20.07.11 wl  SetEditing                   TN5614   User-Verwaltung greift jetzt
  ---------------------------------------------------------------------------------------------------------------------- }

unit TipTypeEditor;


interface


uses
    Windows,
    Messages,
    SysUtils,
    Variants,
    Classes,
    Graphics,
    Controls,
    Forms,
    Dialogs,
    Grids,
    ValEdit,
    StdCtrls,
    ExtCtrls,
    TipTypeDataAdaptor,
    GeneralTypes,
    StringLoader,
    CommonTypes;

type
    TTipTypeEditorStringLoader = class(TTextListStringLoader)
    protected
        procedure AddAllItems(); override;
    end;

    TfrmEdTipType = class(TForm)
        Panel3: TPanel;
        btnOK: TButton;
        Button2: TButton;
        Label1: TLabel;
        Label2: TLabel;
        Label3: TLabel;
        Label4: TLabel;
        Label5: TLabel;
        Label6: TLabel;
        Label7: TLabel;
        Label8: TLabel;
        Label9: TLabel;
        Label11: TLabel;
        Label12: TLabel;
        Label13: TLabel;
        Label14: TLabel;
        Label15: TLabel;
        Label16: TLabel;
        Label17: TLabel;
        Label18: TLabel;
        edTypeName: TEdit;
        edDeviceName: TEdit;
        edToolName: TEdit;
        edMaxVolume: TEdit;
        edMinVolume: TEdit;
        EdZOffset: TEdit;
        edZOffsetRemovable: TEdit;
        edDilutorName: TEdit;
        edXOffset: TEdit;
        edYOffset: TEdit;
        edWashZOffset: TEdit;
        edWasteZOffset: TEdit;
        edDryZOffset: TEdit;
        edFetchTipsMethodName: TEdit;
        edRemoveTipsMethodName: TEdit;
        chkDryAfterFlush: TCheckBox;
        cboBasicTypeName: TComboBox;
        chkDoNotInit: TCheckBox;
        Label10: TLabel;
        procedure btnOKClick(Sender: TObject);
        procedure Button2Click(Sender: TObject);
    private
        fDA: TTipTypeDataAdaptor;
        fStringLoader: TTipTypeEditorStringLoader;
        fTypeName: string;
        procedure TipTypeRecFromGUI(var vRec: TTipType);
        class function AskNameDialog(var vName: string): boolean;
        function SaveChanges: boolean;
        procedure LoadDataToGUI(const aName: string; const aNew: boolean);
        procedure TipTypeRecToGUI(var vRec: TTipType);
        class procedure EditTipTypeIntern(const aName: string; const aNew: boolean);
        procedure SetEditing;
    public
        constructor Create(AOwner: TComponent); override;
        destructor Destroy; override;

        class procedure EditTipType(const aName: string);
        class function NewTipType(): string;
        class function DeleteTipType(const aName: string): boolean;
        class function SaveAsTipType(var vName: string): boolean;
        class function GetAllTypes(): TStringArray;
    end;


implementation


{$R *.dfm}

uses
    UtilLib,
    AppSettings,
    DialogUtils,
    ControlUtils;

{ TTipTypeEditorStringLoader }

procedure TTipTypeEditorStringLoader.AddAllItems;
begin
    AddStandardButtonStrings();

    AddSingle(11, 'Name:', 'Name:');
    AddSingle(12, 'Type Name:', 'Typname:');
    AddSingle(13, 'Device Name (REDI):', 'Device-Name (REDI):');
    AddSingle(14, 'Tool Name:', 'Tool-Name:');
    AddSingle(15, 'Max. Volume [uL]:', 'Max. Volumen [uL]:');
    AddSingle(16, 'Min. Volume [uL]:', 'Min. Volumen [uL]:');
    AddSingle(17, 'Relative Length (Z-Offset) [mm]:', 'Relative Länge (Z-Offset) [mm]:');
    AddSingle(18, 'Removable Tip Z-Offset [mm]:', 'Wechselspitze Z-Offset [mm]:');
    AddSingle(19, 'Dilutor Name:', 'Dilutor Name:');
    AddSingle(20, 'X-Offset [mm]:', 'X-Offset [mm]:');
    AddSingle(21, 'Y-Offset [mm]:', 'Y-Offset [mm]:');
    AddSingle(22, 'Wash Z Offset [mm]:', 'Wash Z-Offset [mm]:');
    AddSingle(23, 'Waste Z Offset [mm]:', 'Waste Z-Offset [mm]:');
    AddSingle(24, 'Dry Z Offset [mm]:', 'Trocknen (Dry)-Z-Offset [mm]:');
    AddSingle(25, 'Dry After Flush:', 'Trocknen nach Flush:');
    AddSingle(26, 'Method to fetch tip(s):', 'Methode zur Spitzenaufnahme:');
    AddSingle(27, 'Method to remove tip(s):', 'Methode zur Spitzenabgabe:');
    AddSingle(28, 'Do not Init:', 'Nicht initialisieren:');
end;

{ TfrmEdTipType }

procedure TfrmEdTipType.btnOKClick(Sender: TObject);
begin
    if (SaveChanges) then
        ModalResult := mrOK;
end;

procedure TfrmEdTipType.Button2Click(Sender: TObject);
begin
    Close();
end;

constructor TfrmEdTipType.Create(AOwner: TComponent);
begin
    inherited;

    TControlUtils.ResetFontForWinXP(self);
    fStringLoader := TTipTypeEditorStringLoader.Create;
    fStringLoader.LoadLanguage(self);

    fDA := TTipTypeDataAdaptor.Create;
    SetEditing;
end;

class function TfrmEdTipType.DeleteTipType(const aName: string): boolean;
var
    xTypeName: string;
    xDA: TTipTypeDataAdaptor;
begin
    result := false;
    if (aName = '') then
        EXIT;

    xTypeName := 'Tip Type';
    if (TDialogUtils.MessageBox(TLanguageString.Read('Do you really want to delete {0} {1}?',
        'Wollen Sie wirklich {0} {1} löschen?', [xTypeName, aName]), TLanguageString.Read('Delete {0}',
        '{0} löschen', [xTypeName]), MB_ICONWARNING + MB_YESNO + MB_DEFBUTTON2) = IDNO) then
        EXIT;

    xDA := TTipTypeDataAdaptor.Create;
    try
        xDA.DeleteName(aName);
        result := true;
    finally
        xDA.Free;
    end;
end;

destructor TfrmEdTipType.Destroy;
begin
    fDA.Free;
    fStringLoader.Free;

    inherited;
end;

procedure TfrmEdTipType.LoadDataToGUI(const aName: string; const aNew: boolean);
var
    xRec: TTipType;
begin
    self.Caption := 'Tip Type: ' + aName;
    self.edTypeName.ReadOnly := true;
    if not aNew then
    begin
        ASSERT(fDA.ReadTipType(aName, xRec))
    end
    else
    begin
        xRec := TTipTypeDataAdaptor.GetEmptyRec();
        xRec.Name := aName;
    end;

    self.TipTypeRecToGUI(xRec);
end;

class procedure TfrmEdTipType.EditTipTypeIntern(const aName: string; const aNew: boolean);
var
    xfrmEdTipType: TfrmEdTipType;
begin
    xfrmEdTipType := TfrmEdTipType.Create(nil);
    try
        xfrmEdTipType.LoadDataToGUI(aName, aNew);
        xfrmEdTipType.ShowModal()
    finally
        xfrmEdTipType.Free;
    end;
end;

class procedure TfrmEdTipType.EditTipType(const aName: string);
begin
    EditTipTypeIntern(aName, false);
end;

class function TfrmEdTipType.NewTipType: string;
var
    xName: string;
begin
    xName := '';
    if not AskNameDialog(xName) then
        EXIT;
    EditTipTypeIntern(xName, true);
    result := xName;

end;

class function TfrmEdTipType.GetAllTypes: TStringArray;
var
    xDA: TTipTypeDataAdaptor;
begin
    xDA := TTipTypeDataAdaptor.Create;
    try
        result := xDA.ReadAllNames();
    finally
        xDA.Free;
    end;
end;

class function TfrmEdTipType.AskNameDialog(var vName: string): boolean;
var
    xTypeName: string;
begin
    result := false;
    xTypeName := 'Tip Type';

    if not TDialogUtils.InputQuery(TLanguageString.Read('Save {0} {1} as ..', '{0} {1} speichern unter ..',
        [xTypeName, vName]), TLanguageString.Read('Save {0} as ..', '{0} speichern unter ..', [xTypeName]),
        vName) then
        EXIT;

    if (vName = '') then
        EXIT;

    result := true;
end;

class function TfrmEdTipType.SaveAsTipType(var vName: string): boolean;
var
    xRec: TTipType;
    xDA: TTipTypeDataAdaptor;
begin
    result := false;
    if (vName = '') then
        EXIT;

    xDA := TTipTypeDataAdaptor.Create;
    try
        xDA.SelectAndOpenByTypeName(vName, true);
        try
            xDA.ReadRecAtCursor(xDA.DataProvider, xRec);
        finally
            xDA.Close;
        end;

        if not AskNameDialog(vName) then
            EXIT;
        if (vName = xRec.Name) then
            EXIT; // gleicher Name darf nicht sein

        xRec.Name := vName;
        xDA.SelectAndOpenByTypeName(vName, false);
        if not xDA.DataProvider.Eof then
            EXIT;
        try
            xDA.WriteRecAtCursor(xDA.DataProvider, xRec, true);
        finally
            xDA.Close;
        end;
        result := true;
    finally
        xDA.Free;
    end;
end;

procedure TfrmEdTipType.TipTypeRecFromGUI(var vRec: TTipType);
var
    xDitiType: string;
begin
    vRec.Name := self.edTypeName.Text;
    vRec.BasicType := TTipTypeDataAdaptor.GetBasicTipTypeFromName(self.cboBasicTypeName.Text, xDitiType);
    vRec.DitiType := xDitiType;

    vRec.DeviceName := self.edDeviceName.Text;
    vRec.ToolName := self.edToolName.Text;
    vRec.MaxVolume := StrToFloatDef(self.edMaxVolume.Text, 0);
    vRec.MinVolume := StrToFloatDef(self.edMinVolume.Text, 0);
    vRec.RelLength_mm := StrToFloatDef(self.edZOffset.Text, 0);
    vRec.DTOffset_mm := StrToFloatDef(self.edZOffsetRemovable.Text, 0);
    vRec.DilutorName := self.edDilutorName.Text;
    vRec.DoNotDryAfterFlush := not self.chkDryAfterFlush.Checked;
    vRec.DoNotInit := self.chkDoNotInit.Checked;
    vRec.XOffset_mm := StrToFloatDef(self.edXOffset.Text, 0);
    vRec.YOffset_mm := StrToFloatDef(self.edYOffset.Text, 0);
    vRec.WashZOffset_mm := StrToFloatDef(self.edWashZOffset.Text, 0);
    vRec.WasteZOffset_mm := StrToFloatDef(self.edWasteZOffset.Text, 0);
    vRec.DryZOffset_mm := StrToFloatDef(self.edDryZOffset.Text, 0);
    vRec.MethodNameGetTip := edFetchTipsMethodName.Text;
    vRec.MethodNamePutTip := edRemoveTipsMethodName.Text;
end;

function TfrmEdTipType.SaveChanges: boolean;
var
    xTypeName: string;
    xRec: TTipType;
begin
    result := false;
    xTypeName := self.edTypeName.Text;
    if (xTypeName = '') then
        EXIT;

    self.TipTypeRecFromGUI(xRec);

    if (fTypeName = '') then
    begin
        if not TAppSettings.ItemConfirmAdd('TipType', xTypeName, '') then
            EXIT;
        fTypeName := xTypeName;
        self.edTypeName.ReadOnly := true;
    end
    else
    begin
        if not TAppSettings.ItemConfirmEdit('TipType', xTypeName) then
            EXIT;
    end;

    fDA.WriteTipType(xRec);

    result := true;
end;

procedure TfrmEdTipType.SetEditing;
var
    aEditing: boolean;
begin
    aEditing := gCommonDll.LevelIsIncluded(gCommonDll.CurrentUser.Level, usrSystemAdmin);

    self.edTypeName.Enabled := aEditing;
    self.cboBasicTypeName.Enabled := aEditing;
    self.edDeviceName.Enabled := aEditing;
    self.edToolName.Enabled := aEditing;
    self.edMaxVolume.Enabled := aEditing;
    self.edMinVolume.Enabled := aEditing;
    self.edZOffset.Enabled := aEditing;
    self.edZOffsetRemovable.Enabled := aEditing;
    self.edDilutorName.Enabled := aEditing;
    self.chkDryAfterFlush.Enabled := aEditing;
    self.chkDoNotInit.Enabled := aEditing;
    self.edXOffset.Enabled := aEditing;
    self.edYOffset.Enabled := aEditing;
    self.edWashZOffset.Enabled := aEditing;
    self.edWasteZOffset.Enabled := aEditing;
    self.edDryZOffset.Enabled := aEditing;
    self.edFetchTipsMethodName.Enabled := aEditing;
    self.edRemoveTipsMethodName.Enabled := aEditing;
    self.btnOK.Visible := aEditing;
end;

procedure TfrmEdTipType.TipTypeRecToGUI(var vRec: TTipType);
begin
    self.edTypeName.Text := vRec.Name;
    self.cboBasicTypeName.Text := TTipTypeDataAdaptor.GetNameFromBasicTipType(vRec.BasicType, vRec.DitiType);
    self.edDeviceName.Text := vRec.DeviceName;
    self.edToolName.Text := vRec.ToolName;
    self.edMaxVolume.Text := FloatToStr(vRec.MaxVolume);
    self.edMinVolume.Text := FloatToStr(vRec.MinVolume);
    self.edZOffset.Text := FloatToStr(vRec.RelLength_mm);
    self.edZOffsetRemovable.Text := FloatToStr(vRec.DTOffset_mm);
    self.edDilutorName.Text := vRec.DilutorName;
    self.chkDryAfterFlush.Checked := not vRec.DoNotDryAfterFlush;
    self.chkDoNotInit.Checked := vRec.DoNotInit;
    self.edXOffset.Text := FloatToStr(vRec.XOffset_mm);
    self.edYOffset.Text := FloatToStr(vRec.YOffset_mm);
    self.edWashZOffset.Text := FloatToStr(vRec.WashZOffset_mm);
    self.edWasteZOffset.Text := FloatToStr(vRec.WasteZOffset_mm);
    self.edDryZOffset.Text := FloatToStr(vRec.DryZOffset_mm);
    self.edFetchTipsMethodName.Text := vRec.MethodNameGetTip;
    self.edRemoveTipsMethodName.Text := vRec.MethodNamePutTip;
end;


end.
