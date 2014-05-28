unit EditArm;
{ --------------------------------------------------------------------------------------------------
  LISSY LAYOUTER
  --------------------------------------------------------------------------------------------------
  Dialogfenster zum Ändern der Tip Konfiguration
  --------------------------------------------------------------------------------------------------
  Datum    op  function/procedure     Änderung / Neuerung
  -------- --  ---------------------  --------------------------------------------------------------
  14.02.00 wl                         neue Unit
  25.02.00 wl  FormCreate             Strings in Ressourcen
  26.05.00 wl                         uses geändert
  10.10.02 wl                               TN1293.2 Ini Access - uses geändert
  16.10.02 wl                               TN1293.2 High(gTipTypes) statt cMaxTipTypes
  27.12.02 wl                               TN1293.5 uses und WinlissyIniAccess geändert
  12.03.03 wl                               TN1293.5 uses posTools
  24.02.04 pk  btnOKClick                   TN1749   Call ChangeVisible
  18.03.04 wl  btnOKClick                   TN1765.1 WB.Arm.SetTips wird nicht mehr durchgeführt
  05.04.04 wl  FormShow                     TN1788   benutzt gTipManager.GetAllTypeNames statt auf gTipTypes zuzugreifen
  05.04.04 wl  FormShow                     TN1788.1 mit gTipManager.TotalTipCount werden nur noch so viele Tips angezeigt wie möglich sind
  26.09.06 wl  btnOKClick                   TN3326   ruft gTipManager.TipTypeExists auf - Benutzung von gTipTypes entfernt
  20.06.08 pk                               TN4139    WB global object replaced by LayoutManager
  03.07.08 wl                               TN4157
  17.09.08 wl                               TN4224   funktioniert jetzt wieder
  16.01.09 wl                                TN4362   an Änderungen in TQueryDataAdaptor angepasst
  10.08.09 wl                                TN4702   Strings werden jetzt direkt geladen
  20.08.09 wl                                TN4702   Strings werden jetzt direkt geladen
  04.11.09 pk                                TN4843   Replace TObjectList, TStringList, TStrings with Generics Lists and TStringArray
  04.15.10 pk  btnOKClick                    TN5050   now calls UpdateTipTypeNames to update all names at once
  23.04.10 wl                                TN5070   uses geändert: alles Layouter-spezifische entfernt
  20.05.10 wl                                TN5117   neue Schriftart "Segoe UI", Schriftgröße 9
  28.05.10 wl                                TN5116   uses geändert
  07.06.10 pk  FormShow                      TN5077   call TipSetDevice.ChangeTipType and TipTypeChanged
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
    ExtCtrls,
    GeneralTypes;

type
    TfrmEdArm = class(TForm)
        Panel3: TPanel;
        btnOK: TButton;
        Button2: TButton;
        procedure FormShow(Sender: TObject);
        procedure Button2Click(Sender: TObject);
        procedure btnOKClick(Sender: TObject);
        procedure FormClose(Sender: TObject; var Action: TCloseAction);
        procedure FormCreate(Sender: TObject);
    private
        function GetAllTypeNames(): TStringArray;
    end;


implementation


uses
    CommonTypes,
    EditingLayoutElements,
    Tipset,
    LayoutManager,
    TipTypeDataAdaptor,
    IntfPipDevice,
    TipsetDataAdaptor,
    GUIManager,
    ControlUtils;

{$R *.DFM}

function TfrmEdArm.GetAllTypeNames(): TStringArray;
begin
    result := TTipTypeDataAdaptor.InstReadAllNames();
end;

procedure TfrmEdArm.FormShow(Sender: TObject);
var
    xDev, xTip, xTotalTip: integer;
    TipCombo: TComboBox;
    TipLabel: TLabel;
    xTipsetTip: TTipsetTip;
    xNames: TStringArray;
begin
    xTotalTip := 0;

    self.Height := 75;
    for xDev := 0 to TLayoutManager.Instance.CurrentLayout.Tipsets.Count - 1 do
    begin
        for xTip := 0 to TLayoutManager.Instance.CurrentLayout.Tipsets[xDev].Tips.Count - 1 do
        begin

            xTipsetTip := TLayoutManager.Instance.CurrentLayout.Tipsets[xDev].Tips[xTip];

            self.Height := self.Height + 28;

            // Tip-Label
            TipLabel := TLabel.Create(self);
            TipLabel.Parent := self;
            TipLabel.Name := 'lblTip' + IntToStr(xTotalTip);
            TipLabel.Caption := Format('%s:  Tip %d', [xTipsetTip.PipDeviceName, xTip + 1]);
            TipLabel.Left := 5;
            TipLabel.Top := 8 + (xTotalTip * 24);

            // Tip-Label
            TipCombo := TComboBox.Create(self);
            TipCombo.Parent := self;
            TipCombo.Name := 'cboTip' + IntToStr(xTotalTip);
            TipCombo.Text := '';
            TipCombo.Left := 160;
            TipCombo.Top := 8 + (xTotalTip * 24);
            TipCombo.Text := xTipsetTip.TypeName;
            xNames := self.GetAllTypeNames();
            TControlUtils.AddValuesToComboBox(xNames, TipCombo, true);
            inc(xTotalTip);
        end;
    end;
end;

procedure TfrmEdArm.Button2Click(Sender: TObject);
begin
    Close;
end;

procedure TfrmEdArm.btnOKClick(Sender: TObject);
var
    xDev, xTip, xTotalTip: integer;
    ComboB: TComboBox;
    xTypeData: TTipType;
    xTipset: TTipsetDevice;
begin
    xTotalTip := 0;

    // Tipnamen überprüfen
    for xDev := 0 to TLayoutManager.Instance.CurrentLayout.Tipsets.Count - 1 do
    begin
        xTipset := TLayoutManager.Instance.CurrentLayout.Tipsets[xDev];
        for xTip := 0 to xTipset.Tips.Count - 1 do
        begin

            ComboB := TCombobox(FindComponent('cboTip' + IntToStr(xTotalTip)));
            Assert(Assigned(ComboB));

            // unbekannte TipTypes suchen
            if (ComboB.Text <> '') and not TTipTypeDataAdaptor.TipTypeExists(ComboB.Text, xTypeData) then
            begin
                gGUIManager.MessageBox(TLanguageString.Read('Tip types [{0}] not known!',
                    'Spitzentypen [{0}] unbekannt!', [ComboB.Text]), TLanguageString.Read('Error', 'Fehler'),
                    MB_ICONSTOP);
                EXIT;
            end;

            xTipset.ChangeTipType(xTip, ComboB.Text);

            inc(xTotalTip);
        end;
        xTipset.TipTypesChanged();
    end;
    TLayoutManager.Instance.CurrentLayout.Visible := true;
    (TLayoutManager.Instance.CurrentLayout as TSetupLayout).notsaved := true;

    self.Close;
end;

procedure TfrmEdArm.FormClose(Sender: TObject; var Action: TCloseAction);
begin
    Action := caFree;
end;

procedure TfrmEdArm.FormCreate(Sender: TObject);
begin
    TControlUtils.ResetFontForWinXP(self);
    self.Caption := TLanguageString.Read('Tip Configuration', 'Pipettenspitzen-Konfiguration');
    self.btnOK.Caption := TLanguageString.Read('&OK', '&OK');
    self.Button2.Caption := TLanguageString.Read('&Cancel', '&Abbrechen');
end;


end.
