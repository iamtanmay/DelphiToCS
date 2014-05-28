{ --------------------------------------------------------------------------------------------------
  BASEUNIT!
  ---------
  Description  : objekt-orientierte Variante von GetEntry (aus SOPHAS)
  --------------------------------------------------------------------------------------------------
  Datum    op  function/procedure     Änderung / Neuerung
  -------- --  -------------------    --------------------------------------------------------------
  29.07.98 wl                         Rausschmiß der Sophas-spezifischen Funktionen --> ScheUtil
  unit --> \DELPHI32\TOOLS\DIALOGS
  BitBtn1Click           Limitierung auf 20 Buchstaben entfernt
  FormActivate           verbessert (entspricht TGetEntryDlg.FormActivate)
  --> TGetEntryDlg und TGetNameDlg sind jetzt in ihrer Funktionalität identisch
  05.08.98 wl  gmGetDBResult          Aufruf, Lesen und Löschen dieses Fensters
  15.09.98 wl  FormActivate           Keine doppelten Einträge mehr möglich
  16.09.98 wl  FormActivate           DefaultName wird angezeigt, wenn er in der Liste steht
  gmGetDBResult          um DefaultName erweiterter Aufruf
  24.09.98 wl  FormActivate           zu Beginn: FSearchRecDB.CancelRange
  01.10.98 wl  gmGetDBResult          --> Utility
  05.10.98 wl  FormActivate           Caption wird gesetzt, auch wenn SearchRecDB=nil
  31.05.99 wl                         ab Version 4.1 im Verzeichnis /SAMINTF/
  Create                 lädt Samintf.res-Ressourcen 510(&OK),520(&Abbrechen)
  18.08.99 wz                         --> Strings in Ressourcen
  10.10.02 wl  gmGetDBResult,gmGetNameResult  1293.3  aus Utility zurück (für Settings-Editor)
  12.03.03 wl                         TN1293.5 uses RessourceLoader
  03.07.03 wl                         TN1501   Compilerhinweise korrigiert
  27.04.05 pk  fSearchRecDB           TN2398   changed from TTable to TDataset
  24.01.08 pk                         TN3997   various changes to allow access with keyboard
  24.06.08 wl  gmGetDBResult          TN4143   entfernt (auch uses dbTables)
  11.08.09 wl                         TN4702   Optik überarbeitet
  14.11.09 wl                         TN4869   uses UtilLib entfernt
  21.07.10 wl                         TN5202   neue Schriftart "Segoe UI", Schriftgröße 9
  23.09.10 pk                         TN5279   Combobox.Style changed to Simple (list view)
  25.07.11 wl                         TN5614   sieht jetzt etwas besser aus
  09.05.12 wl  fCheckIfItemExists     TN5890   neuer Parameter, der Check ist nicht immer gewollt
  18.03.13 wl  ComboBox1KeyDown       TN6045   funktioniert besser als KeyUp (Gefahr, dass Fenster sofort wieder zugeht)
  -------------------------------------------------------------------------------------------------- }

unit GetNames;


interface


uses
    Forms,
    StdCtrls,
    Buttons,
    Controls,
    Classes,
    GeneralTypes,
    ExtCtrls;

type
    TGetNameDlg = class(TForm)
        Panel1: TPanel;
        btnOK: TButton;
        btnCancel: TButton;
        Panel2: TPanel;
        Label1: TLabel;
        ComboBox1: TComboBox;
        procedure FormClose(Sender: TObject; var Action: TCloseAction);
        procedure ComboBox1DblClick(Sender: TObject);
        procedure FormShow(Sender: TObject);
        procedure ComboBox1KeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    private
        fResultText: string;
        fCheckIfItemExists: boolean;
    public
        property ResultText: string read fResultText write fResultText;
        property CheckIfItemExists: boolean read fCheckIfItemExists write fCheckIfItemExists;
        class function ShowDialog(const aItems: TStringArray; const aText, aCaption, aButtonText: string;
            aCheckIfItemExists: boolean): string;
    end;


implementation


uses
    Windows,
    ControlUtils;

{$R *.DFM}
{ TGetNameDlg }

class function TGetNameDlg.ShowDialog(const aItems: TStringArray; const aText, aCaption, aButtonText: string;
    aCheckIfItemExists: boolean): string;
var
    xGetNameDlg: TGetNameDlg;
    i: integer;
begin
    xGetNameDlg := TGetNameDLG.Create(nil);
    try
        xGetNameDlg.CheckIfItemExists := aCheckIfItemExists;
        xGetNameDlg.Caption := aCaption;
        xGetNameDlg.ComboBox1.Clear;
        xGetNameDlg.Label1.Caption := aText;
        xGetNameDlg.btnOK.Caption := aButtonText;
        xGetNameDlg.btnCancel.Caption := TLanguageString.Read('&Cancel', '&Abbrechen');
        TControlUtils.ResetFontForWinXP(xGetNameDlg);

        if Assigned(aItems) then
        begin
            for i := 0 to high(aItems) do
            begin
                xGetNameDlg.ComboBox1.Items.Add(aItems[i]);
            end;
        end;

        if (xGetNameDlg.ComboBox1.Items.Count > 0) then
        begin
            xGetNameDlg.Combobox1.Text := xGetNameDlg.ComboBox1.Items[0];
        end;
        xGetNameDlg.Showmodal;

        result := xGetNameDLG.ResultText;
    finally
        xGetNameDlg.Free;
    end;
end;

procedure TGetNameDlg.ComboBox1DblClick(Sender: TObject);
begin
    self.ModalResult := mrOK;
end;

procedure TGetNameDlg.ComboBox1KeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
    if Key = VK_RETURN then
    begin
        self.ModalResult := mrOK;
    end;
end;

procedure TGetNameDlg.FormClose(Sender: TObject; var Action: TCloseAction);
var
    xIndex: integer;
begin
    fResultText := '';
    if ModalResult = mrOK then
    begin
        if fCheckIfItemExists then
        begin
            // check if item exists (in case it was typed in using keyboard) and has the correct case.
            xIndex := ComboBox1.Items.IndexOf(Combobox1.Text);
            if xIndex < 0 then
            begin
                ModalResult := mrNone;
                Action := caNone;
            end
            else
            begin
                fResultText := ComboBox1.Items[xIndex];
            end;
        end
        else
            fResultText := Combobox1.Text;
    end;
end;

procedure TGetNameDlg.FormShow(Sender: TObject);
begin
    ComboBox1.SetFocus;
end;


end.
