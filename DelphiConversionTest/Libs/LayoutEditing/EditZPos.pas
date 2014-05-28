unit EditZPos;
{ --------------------------------------------------------------------------------------------------
  LISSY LAYOUTER
  --------------------------------------------------------------------------------------------------
  Dialogfenster zum Ändern der Z-Höhen innerhalb eines Layouts
  --------------------------------------------------------------------------------------------------
  Datum    op  function/procedure     Änderung / Neuerung
  -------- --  ---------------------  --------------------------------------------------------------
  25.02.00 wl                         neue Unit
  16.05.00 wl  neuer Aufruf für SetWB..-Befehle im Carrier-Objekt
  26.05.00 wl                         uses geändert
  10.10.02 wl                               TN1293.2 Ini Access - uses geändert
  27.12.02 wl                               TN1293.5 uses und WinlissyIniAccess geändert
  12.03.03 wl                         TN1293.5 uses RessourceLoader
  02.04.04 wl  Button1Click           TN1788   Robor.ZMax heißt jetzt Robot.PArm_GetZMax
  02.08.04 pk  TfrmEdZPos.btnOKClick  TN2068   NewZ is now dynamic array
  27.04.06 pk                         TN2958   Robot.PArm_GetZMax replaced by wb.MaxZ_Steps
  09.11.07 pk                         TN3924   steps changed to mm
  07.01.08 pk  FormShow               TN3971   use gWorkbenchZ instead of ArmBounds
  20.06.08 pk                         TN4139    WB global object replaced by LayoutManager
  10.08.09 wl                         TN4702   Strings werden jetzt direkt geladen
  21.08.09 wl  fStringLoader          TN4702   fStringLoader lädt Strings für Dialog-Elemente
  23.04.10 wl                         TN5070   uses geändert: alles Layouter-spezifische entfernt
  20.05.10 wl                         TN5117   neue Schriftart "Segoe UI", Schriftgröße 9
  21.06.10 wl                               TN5160   Position = poScreenCenter
  20.10.11 ts  btnOKClick             TN5724   NewZ ist float anstelle von integer
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
    ExtCtrls,
    StringLoader;

type
    TEditZPosStringLoader = class(TTextListStringLoader)
    protected
        procedure AddAllItems(); override;
    end;

    TfrmEdZPos = class(TForm)
        Panel3: TPanel;
        btnOK: TButton;
        Button2: TButton;
        Panel1: TPanel;
        Label1: TLabel;
        Edit1: TEdit;
        Button1: TButton;
        Label2: TLabel;
        procedure FormShow(Sender: TObject);
        procedure Button2Click(Sender: TObject);
        procedure btnOKClick(Sender: TObject);
        procedure FormClose(Sender: TObject; var Action: TCloseAction);
        procedure FormCreate(Sender: TObject);
        procedure Button1Click(Sender: TObject);
        procedure FormDestroy(Sender: TObject);
    private
        fStringLoader: TEditZPosStringLoader;
    end;


implementation


{$R *.DFM}

uses
    Utility2,
    SamGlobe,
    AppTypes,
    EditingLayoutElements,
    LayoutManager,
    Carrier,
    GeneralTypes,
    ControlUtils,
    DialogUtils;

{ TEditZPosStringLoader }

procedure TEditZPosStringLoader.AddAllItems;
begin
    AddSingle(510, '&OK', '&OK');
    AddSingle(520, '&Cancel', '&Abbrechen');
    AddSingle(58500, 'Workbench Heights', 'Höhen der Arbeitsfläche');
    AddSingle(58510, 'Maximum Height:', 'Maximale Höhe:');
    AddSingle(58520, 'Set All Values To Maximum', 'Alle Werte auf Maximum stellen');
end;

{ TfrmEdZPos }

procedure TfrmEdZPos.FormShow(Sender: TObject);
var
    i, j: integer;
    ZEdit: TEdit;
    ZLabel: TLabel;
    xCarrier: TCarrier;
begin
    Edit1.Text := FloatToStr(gWorkbenchZ);
    j := 0;
    for i := 0 to TLayoutManager.Instance.CurrentLayout.NoOfCarrier - 1 do
    begin
        xCarrier := TLayoutManager.Instance.CurrentLayout.Carriers[i];
        if (xCarrier.Name = '') then
            CONTINUE;

        ZLabel := TLabel.Create(self);
        ZLabel.Parent := self;
        ZLabel.Name := 'lblZ' + IntToStr(i);
        ZLabel.Caption := xCarrier.Name + ':';
        // ------------------------------------------------------------------------------- Edit-Feld
        ZEdit := TEdit.Create(self);
        ZEdit.Parent := self;
        ZEdit.Name := 'edZ' + IntToStr(i);
        ZEdit.Text := FloatToStr(xCarrier.PosZ);
        // ---------------------------------------------------------------------------- Fenstergröße
        ZLabel.Left := 60 + ((j div 12) * 200);
        ZLabel.Top := 10 + ((j mod 12) * 24);
        ZEdit.Left := 190 + ((j div 12) * 200);
        ZEdit.Width := 55;
        ZEdit.Top := 8 + ((j mod 12) * 24);
        Width := 290 + ((j div 12) * 200);
        if (j < 12) then
            Height := 180 + ((j mod 12) * 24)
        else
            Height := 444;
        inc(j);
    end;

end;

procedure TfrmEdZPos.Button2Click(Sender: TObject);
begin
    Close;
end;

procedure TfrmEdZPos.btnOKClick(Sender: TObject);
var
    i: integer;
    NewZ: array of double;
    ZEdit: TEdit;
    OKFlag: boolean;
    xCarrier: TSetupCarrier;
begin
    OKFlag := true;
    SetLength(NewZ, TLayoutManager.Instance.CurrentLayout.NoOfCarrier);
    for i := 0 to TLayoutManager.Instance.CurrentLayout.NoOfCarrier - 1 do
    begin
        xCarrier := TLayoutManager.Instance.CurrentLayout.Carriers[i] as TSetupCarrier;
        if (xCarrier.Name = '') then
            CONTINUE;
        ZEdit := TEdit(FindComponent('edZ' + IntToStr(i)));
        if (ZEdit = nil) or (not TryStrToFloat(ZEdit.Text, NewZ[i])) then
            OKFlag := false;
    end;
    if not OKFlag then
    begin
        TDialogUtils.MessageBox(TLanguageString.Read('Please check your settings!',
            'Bitte überprüfen Sie Ihre Eingaben!'), '', MB_ICONSTOP);
        exit;
    end;
    for i := 0 to TLayoutManager.Instance.CurrentLayout.NoOfCarrier - 1 do
    begin
        xCarrier := TLayoutManager.Instance.CurrentLayout.Carriers[i] as TSetupCarrier;
        if (xCarrier.Name = '') then
            CONTINUE;
        if (xCarrier.PosZ <> NewZ[i]) then
        begin
            xCarrier.SetWB_Z(NewZ[i]);
            (TLayoutManager.Instance.CurrentLayout as TSetupLayout).notsaved := true;
        end;
    end;
    Close;
end;

procedure TfrmEdZPos.FormClose(Sender: TObject; var Action: TCloseAction);
begin
    Action := caFree;
end;

procedure TfrmEdZPos.FormCreate(Sender: TObject);
begin
    TControlUtils.ResetFontForWinXP(self);
    fStringLoader := TEditZPosStringLoader.Create;
    fStringLoader.LoadLanguage(self);
end;

procedure TfrmEdZPos.FormDestroy(Sender: TObject);
begin
    fStringLoader.Free;
end;

procedure TfrmEdZPos.Button1Click(Sender: TObject);
var
    i: integer;
    ZEdit: TEdit;
begin
    for i := 0 to TLayoutManager.Instance.CurrentLayout.NoOfCarrier - 1 do
    begin
        ZEdit := TEdit(FindComponent('edZ' + IntToStr(i)));
        if (ZEdit <> nil) then
            ZEdit.Text := FloatToStr(gWorkbenchZ);
    end;

end;


end.
