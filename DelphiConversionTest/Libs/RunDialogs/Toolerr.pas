unit ToolErr;
{ --------------------------------------------------------------------------------------------------
  Ebene 1 (Utilities)
  --------------------------------------------------------------------------------------------------
  Auswahl Menü Error Handling (wird benötigt wenn ein Tool im Handler sitzt)
  --------------------------------------------------------------------------------------------------
  Datum    op  function/procedure     Änderung / Neuerung
  -------- --  -------------------    -------------------------------------------------------------
  12.01.98 dl  ToolErrorHandling      visualisiert eine Fehlermeldung und gibt Auswahlmöglichkeiten
  zur Fehlerbehandlung;
  12.01.98 dl  InitSam                Auswahl mit messagebox: init ja oder nein
  12.01.98 dl  OpenHandler            ResetAllModules; Händler öffnen; InitSam;
  12.01.98 dl  BringbackTool          ResetAllModules; tool zurückbringen; InitSam;
  12.01.98 dl  SolveManual            ResetAllModules; MoveCForm Fenster öffnen; InitSam
  09.04.98 mo  SolveManual            deaktiviert
  04.05.98 mo                         uses movecode entfernt
  01.09.99 wl  FormCreate             gmLoadLanguage eingebaut
  08.11.99 wl  alle Funktionen außer FormCreate  --> BasicThr
  16.03.00 wl  FormCreate             nach LoadLanguage wird Default auf 'Bring Back Tool' gesetzt
  12.03.03 wl                         TN1293.5 uses RessourceLoader
  04.02.04 pk                         TN1719   New functions for Thread-Synchronized access
  24.06.04 wl                         TN2007   uses Variants (nur Delphi 6 und 7)
  11.03.05 pk                         TN2339.2 Synchronized Call --> GUIManager
  12.08.09 wl                         TN4712   komplett überarbeitet
  20.05.10 wl                         TN5117   neue Schriftart "Segoe UI", Schriftgröße 9
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
    Buttons,
    ExtCtrls;

type
    TToolErrForm = class(TForm)
        Shape2: TShape;
        Label1: TLabel;
        rbBringBack: TRadioButton;
        rbOpenGripper: TRadioButton;
        Bevel2: TBevel;
        btnOK: TButton;
        btnCancel: TButton;
        Image1: TImage;
        lblTop: TLabel;
        Label3: TLabel;
        procedure FormCreate(Sender: TObject);
    public
        class function ShowDialog(const aMessage: string): integer;
    end;


implementation


{$R *.DFM}

uses
    ControlUtils,
    GeneralTypes,
    AppTypes;

{ TToolErrForm }

class function TToolErrForm.ShowDialog(const aMessage: string): integer;
var
    xForm: TToolErrForm;
    xModalResult: integer;
begin
    xForm := TToolErrForm.Create(nil);
    try
        xForm.lblTop.Caption := aMessage;

        xModalResult := xForm.ShowModal;
        if (xModalResult = mrCancel) then
        begin
            result := cToolErrResultCancel;
        end
        else
        begin
            if (xForm.rbBringBack.Checked) then
                result := cToolErrResultBringBack
            else
                result := cToolErrResultOpenGripper;

        end;
    finally
        xForm.Free;
    end;
end;

procedure TToolErrForm.FormCreate(Sender: TObject);
begin
    TControlUtils.ResetFontForWinXP(self);
    self.Caption := TLanguageString.Read('Tool Error Handling', 'Tool Fehlerbehandlung');
    btnOk.Caption := TLanguageString.Read('Continue', 'Fortsetzen');
    btnCancel.Caption := TLanguageString.Read('Cancel', 'Abbrechen');
    Label1.Caption := TLanguageString.Read('Please choose one of the following error handling types:',
        'Bitte wählen Sie eine der folgenden Fehlerbehandlungen:');
    rbBringBack.Caption := TLanguageString.Read('Bring back the tool', 'Das Tool soll zurückgebracht werden');
    rbOpenGripper.Caption := TLanguageString.Read('Open gripper', 'Greifer öffnen');
    Label3.Caption := TLanguageString.Read('-> The tool will fall down!', '-> Das Tool wird herunterfallen!');
end;


end.
