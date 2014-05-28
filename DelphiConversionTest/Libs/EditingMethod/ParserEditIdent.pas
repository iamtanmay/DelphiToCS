{ --------------------------------------------------------------------------------------------------
  Copyright © 2004 Zinsser Analytic GmbH - All rights reserved.
  Author       : Wolfgang Lyncke (wl)
  Description  : GUI for Parser identifier settings
  --------------------------------------------------------------------------------------------------
  Revision History:
  date     op  method                       track-no improvement/change
  -------- --  ---------------------------  -------- -----------------------------------------------
  30.08.04 wl                               TN2097   initial version
  31.08.04 wl                               TN2097   Ressourcen hinzugefügt, Änderungen im Detail
  25.08.05 pk                               TN2546   references to TParserIdentifier changed to TParserStoredIdent
  07.05.07 wl  SetAllValues                 TN3669   Bei OK oder Apply wird nachgefragt
  27.08.07 pk                               TN3788   Reference to New ParserStoredIdentifier.Pas unit
  06.08.08 pk  Create                       TN4165.1 calls ReadIntoCache
  06.08.08 pk  SetAllValues                 TN4165.1 calls WriteFromCache
  10.08.09 wl                               TN4702   Strings werden jetzt direkt geladen
  20.08.09 wl  fStringLoader                TN4702   fStringLoader lädt Strings für Dialog-Elemente
  07.05.10 pk  TfrmParserEditIdent.Create   TN5092   DefaultValue is now an object instead of a string
  20.05.10 wl                               TN5117   neue Schriftart "Segoe UI", Schriftgröße 9
  21.06.10 wl                               TN5160   Position = poScreenCenter
  14.12.12 wl  EditIdent,NewIdent           TN6054   neuer Parameter: aDataCache:TMethodVariablesDataCache
  20.02.13 wl                               TN6055   Verflechtung mit TParserStoredIdent aufgelöst
  22.02.13 wl                               TN6094   Neu: RequestOrder
  22.02.13 wl                               TN6095   Neu: "Value is rack name","Value is rack position"
  22.02.13 wl                               TN6095   Neu: DialogType, DialogCaption, DialogHide
  08.03.13 wl                               TN6095   Liste der DialogTypes erweitert
  11.03.13 wl                               TN6095   Neu: ArrayLengthRefToOrder
  19.04.13 wl                               TN6095   für MultiPageDialog geändert
  14.05.13 wl                               TN6095   DialogCaption entfernt
  10.02.14 ts                               TN6353   Volume,Substance,LiqPar added
  08.04.14 ts                               TN6391   SourceRackName,-Position added
  -------------------------------------------------------------------------------------------------- }

unit ParserEditIdent;


interface


uses
    Forms,
    StdCtrls,
    Controls,
    Classes,
    ExtCtrls,
    MethodVariableTypes,
    StringLoader,
    Spin;

type
    TParserEditIdentStringLoader = class(TTextListStringLoader)
    protected
        procedure AddAllItems(); override;
    end;

    TfrmParserEditIdent = class(TForm)
        Label1: TLabel;
        edMinValue: TEdit;
        edMaxValue: TEdit;
        Label2: TLabel;
        edName: TEdit;
        edDefault: TEdit;
        edDescr: TEdit;
        Panel3: TPanel;
        btnOK: TButton;
        Button2: TButton;
        Label3: TLabel;
        Label4: TLabel;
        Label5: TLabel;
        Label6: TLabel;
        edPickList: TEdit;
        Button1: TButton;
        Label7: TLabel;
        spinRequestOrder: TSpinEdit;
        cbDialogHide: TCheckBox;
        cbDialogFormat: TComboBox;
        Label9: TLabel;
        Label10: TLabel;
        spArrayLengthRefToOrder: TSpinEdit;
        cbArray: TCheckBox;
        procedure Button2Click(Sender: TObject);
        procedure btnOKClick(Sender: TObject);
        procedure Button1Click(Sender: TObject);
    private
        fVariableName: string;
        fIdentData: TMethodVariableData;
        fAnythingChanged: boolean;
        fStringLoader: TParserEditIdentStringLoader;
        function SetAllValues: boolean;
        procedure InitDialogTypes;
        class function DataTypeToItemIndex(const aDataType: TMethodVariableFormatType): integer;
        class function ItemIndexToDataType(const aItemIndex: integer): TMethodVariableFormatType;
    public
        constructor Create(const aVariableName: string; const aIdentData: TMethodVariableData); reintroduce;
        destructor Destroy; override;

        class function EditData(const aMethodName, aVariableName: string;
            var vRec: TMethodVariableData): boolean;
        class function EditDataWithDatabase(const aMethodName, aVariableName: string): boolean;

        property IdentData: TMethodVariableData read fIdentData;
        property AnythingChanged: boolean read fAnythingChanged;
    end;


implementation


{$R *.DFM}

uses
    GeneralTypes,
    Dialogs,
    SysUtils,
    AppSettings,
    MethodVariablesDataAdaptor,
    ControlUtils;

{ TParserEditIdentStringLoader }

procedure TParserEditIdentStringLoader.AddAllItems;
begin
    AddSingle(510, '&OK', '&OK');
    AddSingle(520, '&Cancel', '&Abbrechen');
    AddSingle(590, '&Apply', 'Ü&bernehmen');
    AddSingle(1000, 'Order Index:', 'Order Index:');
    AddSingle(1001, 'Editor:', 'Editor:');
    AddSingle(1002, 'Caption:', 'Überschrift:');
    AddSingle(1003, 'Hide in last page', 'Auf der letzten Seite verbergen');
    AddSingle(41720, 'Name:', 'Name:');
    AddSingle(41730, 'Description:', 'Beschreibung:');
    AddSingle(41740, 'Default value:', 'Standardwert:');
    AddSingle(41750, 'Minimum value:', 'Minimalwert:');
    AddSingle(41760, 'Maximum value:', 'Maximalwert:');
    AddSingle(41770, 'Type:', 'Typ:');
    AddSingle(41780, 'Pick list:', 'Auswahlliste:');
end;

{ TfrmParserEditIdent }

constructor TfrmParserEditIdent.Create(const aVariableName: string; const aIdentData: TMethodVariableData);
begin
    inherited Create(nil);

    TControlUtils.ResetFontForWinXP(self);
    fStringLoader := TParserEditIdentStringLoader.Create;
    fStringLoader.LoadLanguage(self);
    InitDialogTypes;
    fAnythingChanged := false;
    fVariableName := aVariableName;
    fIdentData := aIdentData;

    ASSERT(fVariableName <> '');

    self.Caption := TLanguageString.Read('Edit parameter: {0}', 'Parameter bearbeiten: {0}', [fVariableName]);
    edName.Enabled := false;
    edName.Text := fVariableName;
    spinRequestOrder.Value := fIdentData.RequestOrder;
    edDescr.Text := fIdentData.RequestText;
    edDefault.Text := fIdentData.DefaultValue;
    edMinValue.Text := TMethodVariableUtils.MinMaxValueToStr(fIdentData.MinCheck, fIdentData.MinValue);
    edMaxValue.Text := TMethodVariableUtils.MinMaxValueToStr(fIdentData.MaxCheck, fIdentData.MaxValue);
    cbDialogFormat.ItemIndex := DataTypeToItemIndex(fIdentData.DataType);
    cbArray.Checked := fIdentData.DataIsArray;
    edPickList.Text := fIdentData.PickList;
    cbDialogHide.Checked := fIdentData.DialogHide;
    spArrayLengthRefToOrder.Value := fIdentData.ArrayLengthRefToOrder;
end;

destructor TfrmParserEditIdent.Destroy;
begin
    fStringLoader.Free;
    inherited;
end;

class function TfrmParserEditIdent.EditData(const aMethodName, aVariableName: string;
    var vRec: TMethodVariableData): boolean;
var
    xForm: TfrmParserEditIdent;
begin
    xForm := TfrmParserEditIdent.Create(aVariableName, vRec);
    try
        xForm.ShowModal;
        if (xForm.AnythingChanged) then
        begin
            vRec := xForm.IdentData;
            EXIT(true);
        end
        else
            EXIT(false);
    finally
        FreeAndNil(xForm);
    end;
end;

class function TfrmParserEditIdent.EditDataWithDatabase(const aMethodName, aVariableName: string): boolean;
var
    xDA: TMethodVariablesDataAdaptor;
    xRec: TMethodVariableData;
begin
    xDA := TMethodVariablesDataAdaptor.Create;
    try
        // Daten lesen
        xRec := xDA.ReadVariableRec(aMethodName, aVariableName);

        // Editor zeigen
        result := TfrmParserEditIdent.EditData(aMethodName, aVariableName, xRec);

        // Daten schreiben
        if (result) then
            xDA.WriteVariableRec(aMethodName, aVariableName, xRec);
    finally
        FreeAndNil(xDA);
    end;
end;

procedure TfrmParserEditIdent.InitDialogTypes;
begin
    self.cbDialogFormat.Items.Add(TLanguageString.Read('No Format', 'Kein Format'));
    self.cbDialogFormat.Items.Add(TLanguageString.Read('Text (String)', 'Text (String)'));
    self.cbDialogFormat.Items.Add(TLanguageString.Read('Number (Float)', 'Zahl (Float)'));
    self.cbDialogFormat.Items.Add(TLanguageString.Read('Integer', 'Ganzzahl (Integer)'));
    self.cbDialogFormat.Items.Add(TLanguageString.Read('True/False (Boolean)', 'True/False (Boolean)'));
    self.cbDialogFormat.Items.Add(TLanguageString.Read('Path name', 'Pfadname'));
    self.cbDialogFormat.Items.Add(TLanguageString.Read('File name', 'Dateiname'));
    self.cbDialogFormat.Items.Add(TLanguageString.Read('Rack', 'Rack'));
    self.cbDialogFormat.Items.Add(TLanguageString.Read('(First) rack position', '(Erste) Rackposition'));
    self.cbDialogFormat.Items.Add(TLanguageString.Read('Last rack position', 'Letzte Rackposition'));
    self.cbDialogFormat.Items.Add(TLanguageString.Read('Carrier', 'Carrier'));
    self.cbDialogFormat.Items.Add(TLanguageString.Read('Carrier slot', 'Carrier-Slot'));
    self.cbDialogFormat.Items.Add(TLanguageString.Read('Volume (Pipet)', 'Volume (Pipettieren)'));
    self.cbDialogFormat.Items.Add(TLanguageString.Read('Substance', 'Substanz'));
    self.cbDialogFormat.Items.Add(TLanguageString.Read('Liquid handling parameter',
        'Liquid Handling Parameter'));
    self.cbDialogFormat.Items.Add(TLanguageString.Read('Source Rack', 'Quellrack'));
    self.cbDialogFormat.Items.Add(TLanguageString.Read('(First) source rack position',
        '(Erste) Quellrackposition'));
    self.cbDialogFormat.Items.Add(TLanguageString.Read('Last source rack position',
        'Letzte Quellrackposition'));
end;

class function TfrmParserEditIdent.DataTypeToItemIndex(const aDataType: TMethodVariableFormatType): integer;
begin
    case aDataType of
        mvfNoFormat:
            EXIT(0);
        mvfString:
            EXIT(1);
        mvfFloat:
            EXIT(2);
        mvfInteger:
            EXIT(3);
        mvfBoolean:
            EXIT(4);
        mvfPathName:
            EXIT(5);
        mvfFileName:
            EXIT(6);
        mvfRackName:
            EXIT(7);
        mvfRackPos:
            EXIT(8);
        mvfRackLastPos:
            EXIT(9);
        mvfCarrier:
            EXIT(10);
        mvfCarrierSlot:
            EXIT(11);
        mvfVolume:
            EXIT(12);
        mvfSubstance:
            EXIT(13);
        mvfLiqParam:
            EXIT(14);
        mvfSourceRackName:
            EXIT(15);
        mvfSourceRackPos:
            EXIT(16);
        mvfSourceRackLastPos:
            EXIT(17);
        else
            EXIT(0);
    end;
end;

class function TfrmParserEditIdent.ItemIndexToDataType(const aItemIndex: integer): TMethodVariableFormatType;
begin
    case aItemIndex of
        0:
            EXIT(mvfNoFormat);
        1:
            EXIT(mvfString);
        2:
            EXIT(mvfFloat);
        3:
            EXIT(mvfInteger);
        4:
            EXIT(mvfBoolean);
        5:
            EXIT(mvfPathName);
        6:
            EXIT(mvfFileName);
        7:
            EXIT(mvfRackName);
        8:
            EXIT(mvfRackPos);
        9:
            EXIT(mvfRackLastPos);
        10:
            EXIT(mvfCarrier);
        11:
            EXIT(mvfCarrierSlot);
        12:
            EXIT(mvfVolume);
        13:
            EXIT(mvfSubstance);
        14:
            EXIT(mvfLiqParam);
        15:
            EXIT(mvfSourceRackName);
        16:
            EXIT(mvfSourceRackPos);
        17:
            EXIT(mvfSourceRackLastPos);
        else
            EXIT(mvfNoFormat);
    end;
end;

procedure TfrmParserEditIdent.Button2Click(Sender: TObject);
begin
    Close;
end;

procedure TfrmParserEditIdent.btnOKClick(Sender: TObject);
begin
    if SetAllValues then
        Close;
end;

function TfrmParserEditIdent.SetAllValues: boolean;
begin
    result := false;

    if not TAppSettings.ItemConfirmEdit('Variable', edName.Text) then
        EXIT;

    fIdentData.RequestOrder := spinRequestOrder.Value;
    fIdentData.RequestText := edDescr.Text;
    fIdentData.DefaultValue := edDefault.Text;
    fIdentData.MinValue := TMethodVariableUtils.StrToMinMaxValue(edMinValue.Text, fIdentData.MinCheck);
    fIdentData.MaxValue := TMethodVariableUtils.StrToMinMaxValue(edMaxValue.Text, fIdentData.MaxCheck);
    fIdentData.PickList := edPickList.Text;
    fIdentData.DataType := ItemIndexToDataType(cbDialogFormat.ItemIndex);
    fIdentData.DataIsArray := cbArray.Checked;
    fIdentData.DialogHide := cbDialogHide.Checked;
    fIdentData.ArrayLengthRefToOrder := self.spArrayLengthRefToOrder.Value;

    fAnythingChanged := true;
    result := true;
end;

procedure TfrmParserEditIdent.Button1Click(Sender: TObject);
var
    xOpenDialog: TOpenDialog;
begin
    xOpenDialog := TOpenDialog.Create(nil);
    try
        if Pos(TMethodVariableUtils.cIdentPicklistPath, edPickList.Text) = 1 then
            xOpenDialog.InitialDir := ExtractFileDir(Copy(edPickList.Text, 7, Length(edPickList.Text)));
        xOpenDialog.Execute;
        if (xOpenDialog.FileName = '') then
            EXIT;

        edPickList.Text := TMethodVariableUtils.cIdentPicklistPath + xOpenDialog.FileName;
    finally
        xOpenDialog.Free;
    end;
end;


end.
