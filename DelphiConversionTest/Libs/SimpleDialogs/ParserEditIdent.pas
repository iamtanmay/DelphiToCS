unit ParserEditIdent;
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
  -------------------------------------------------------------------------------------------------- }


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
    ComCtrls,
    ParserIdentifier,
    ParserStoredIdentifier,
    ExtCtrls,
    StringLoader;

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
        btnApply: TButton;
        btnOK: TButton;
        Button2: TButton;
        Label3: TLabel;
        Label4: TLabel;
        Label5: TLabel;
        rgValueFormat: TRadioGroup;
        Label6: TLabel;
        edPickList: TEdit;
        Button1: TButton;
        procedure Button2Click(Sender: TObject);
        procedure btnApplyClick(Sender: TObject);
        procedure btnOKClick(Sender: TObject);
        procedure Button1Click(Sender: TObject);
    private
        fIdent: TParserStoredIdent;
        fAnythingChanged: boolean;
        fStringLoader: TParserEditIdentStringLoader;
        function SetAllValues: boolean;
    public
        constructor Create(aOwner: TComponent; aIdent: TParserStoredIdent); reintroduce;
        destructor Destroy; override;
        //
        class function EditIdent(aIdent: TParserStoredIdent): boolean;
        class function NewIdent: TParserIdentifier;
        //
        property Ident: TParserStoredIdent read fIdent;
        property AnythingChanged: boolean read fAnythingChanged;
    end;


implementation


{$R *.DFM}

uses
    GeneralTypes,
    AppSettings,
    ControlUtils;

{ TParserEditIdentStringLoader }

procedure TParserEditIdentStringLoader.AddAllItems;
begin
    AddSingle(510, '&OK', '&OK');
    AddSingle(520, '&Cancel', '&Abbrechen');
    AddSingle(590, '&Apply', 'Ü&bernehmen');
    AddSingle(41720, 'Name:', 'Name:');
    AddSingle(41730, 'Description:', 'Beschreibung:');
    AddSingle(41740, 'Default value:', 'Standardwert:');
    AddSingle(41750, 'Minimum value:', 'Minimalwert:');
    AddSingle(41760, 'Maximum value:', 'Maximalwert:');
    AddDouble(41770, '', 'No format;Value is a path name;Value is a file name', '',
        'Kein Format;Wert ist ein Pfadname;Wert ist ein Dateiname');
    AddSingle(41780, 'Pick list:', 'Auswahlliste:');
end;

{ TfrmParserEditIdent }

constructor TfrmParserEditIdent.Create(aOwner: TComponent; aIdent: TParserStoredIdent);
begin
    inherited Create(aOwner);

    TControlUtils.ResetFontForWinXP(self);
    fStringLoader := TParserEditIdentStringLoader.Create;
    fStringLoader.LoadLanguage(self);

    fAnythingChanged := false;
    FIdent := aIdent;
    if Assigned(FIdent) then
    begin // Edit
        fIdent.ReaderWriter.ReadIntoCache();
        fIdent.LoadProperties;
        self.Caption := TLanguageString.Read('Edit parameter: {0}', 'Parameter bearbeiten: {0}',
            [FIdent.Name]);
        edName.Enabled := false;
        edName.Text := FIdent.Name;
        edDescr.Text := FIdent.Description;
        edDefault.Text := fIdent.DefValueAsStr;
        edMinValue.Text := fIdent.MinValue;
        edMaxValue.Text := fIdent.MaxValue;
        rgValueFormat.ItemIndex := fIdent.ValueFormat;
        edPickList.Text := fIdent.PickList;
    end
    else
    begin // New
        self.Caption := TLanguageString.Read('Create new identifier', 'Definition eines neuen Parameters');
        edName.Enabled := true;
        edName.Text := '';
        edDescr.Text := '';
    end;
end;

destructor TfrmParserEditIdent.Destroy;
begin
    fStringLoader.Free;
    inherited;
end;

class function TfrmParserEditIdent.EditIdent(aIdent: TParserStoredIdent): boolean;
var
    xfrmParserEditIdent: TfrmParserEditIdent;
begin
    result := false;
    xfrmParserEditIdent := TfrmParserEditIdent.Create(Application, aIdent);
    xfrmParserEditIdent.ShowModal;

    if (xfrmParserEditIdent.AnythingChanged) then
    begin
        aIdent.SaveProperties;
        result := true;
    end;
    xfrmParserEditIdent.Free;
end;

class function TfrmParserEditIdent.NewIdent: TParserIdentifier;
var
    xfrmParserEditIdent: TfrmParserEditIdent;
    xIdent: TParserStoredIdent;
begin
    result := nil;
    xIdent := nil;
    xfrmParserEditIdent := TfrmParserEditIdent.Create(Application, xIdent);
    xfrmParserEditIdent.ShowModal;

    if (xfrmParserEditIdent.AnythingChanged) and Assigned(xIdent) then
    begin
        result := xIdent;
    end;
    xfrmParserEditIdent.Free;
end;

procedure TfrmParserEditIdent.Button2Click(Sender: TObject);
begin
    Close;
end;

procedure TfrmParserEditIdent.btnApplyClick(Sender: TObject);
begin
    SetAllValues;
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

    fIdent.Description := edDescr.Text;
    fIdent.DefValueAsStr := edDefault.Text;
    fIdent.MinValue := edMinValue.Text;
    fIdent.MaxValue := edMaxValue.Text;
    fIdent.ValueFormat := rgValueFormat.ItemIndex;
    fIdent.PickList := edPickList.Text;

    fIdent.SaveProperties;
    fIdent.ReaderWriter.WriteFromCache();
    fAnythingChanged := true;
    result := true;
end;

procedure TfrmParserEditIdent.Button1Click(Sender: TObject);
var
    xOpenDialog: TOpenDialog;
begin
    xOpenDialog := TOpenDialog.Create(nil);
    if Pos(STR_IDENT_PICKLIST_PATH, edPickList.Text) = 1 then
        xOpenDialog.InitialDir := ExtractFileDir(Copy(edPickList.Text, 7, Length(edPickList.Text)));
    xOpenDialog.Execute;
    if (xOpenDialog.FileName = '') then
        EXIT;

    edPickList.Text := STR_IDENT_PICKLIST_PATH + xOpenDialog.FileName;

    xOpenDialog.Free;
end;


end.
