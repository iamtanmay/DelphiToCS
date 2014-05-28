unit Stop;
{ --------------------------------------------------------------------------------------------------
  Ebene 1 (Utilities)
  --------------------------------------------------------------------------------------------------
  User Interrupt - Dialogfenster
  --------------------------------------------------------------------------------------------------
  Datum    op  function/procedure     Änderung / Neuerung
  -------- --  -------------------    --------------------------------------------------------------
  07.05.98 wl                         Help-Button entfernt!
  31.05.99 wl                         ab Version 4.1 im Verzeichnis /SAMINTF/
  FormCreate             lädt Samintf.res-Ressourcen 10700,10710,560(&Yes),570(&No)
  12.03.03 wl                         TN1293.5 uses RessourceLoader
  02.12.08 pk                         TN4336   New look
  20.08.09 wl  fStringLoader          TN4702   fStringLoader lädt Strings für Dialog-Elemente
  20.05.10 wl                         TN5117   neue Schriftart "Segoe UI", Schriftgröße 9
  -------------------------------------------------------------------------------------------------- }


interface


uses
    SysUtils,
    Windows,
    Messages,
    Classes,
    Graphics,
    Controls,
    StdCtrls,
    ExtCtrls,
    Forms,
    StringLoader;

type
    TStopDialogStringLoader = class(TTextListStringLoader)
    protected
        procedure AddAllItems(); override;
    end;

    TStopDlg = class(TForm)
        Panel1: TPanel;
        Label1: TLabel;
        Panel2: TPanel;
        Button2: TButton;
        Button1: TButton;
        Bevel1: TBevel;
        procedure FormCreate(Sender: TObject);
        procedure FormDestroy(Sender: TObject);
    private
        fStringLoader: TStopDialogStringLoader;
    public
        class function ShowStopDialog(): integer;
    end;


implementation


{$R *.DFM}

uses
    ControlUtils;

{ TStopDialogStringLoader }

procedure TStopDialogStringLoader.AddAllItems;
begin
    AddSingle(560, '&Yes', '&Ja');
    AddSingle(570, '&No', '&Nein');
    AddSingle(10700, 'User interrupt ...', 'Benutzer-Unterbrechung...');
    AddSingle(10710, 'Abort process?', 'Prozess abbrechen?');
end;

{ TStopDlg }

procedure TStopDlg.FormCreate(Sender: TObject);
begin
    TControlUtils.ResetFontForWinXP(self);
    fStringLoader := TStopDialogStringLoader.Create;
    fStringLoader.LoadLanguage(self);
end;

procedure TStopDlg.FormDestroy(Sender: TObject);
begin
    fStringLoader.Free;
end;

class function TStopDlg.ShowStopDialog: integer;
var
    xStopDlg: TStopDlg;
begin
    xStopDlg := TStopDlg.Create(nil);
    try
        result := xStopDlg.ShowModal;
    finally
        xStopDlg.Free;
    end;
end;


end.
