unit EditXY;
{ --------------------------------------------------------------------------------------------------

  --------------------------------------------------------------------------------------------------
  Datum    op  function/procedure     track-no   Änderung / Neuerung
  -------- --  ---------------------  ---------  ---------------------------------------------------
  25.02.00 wl                                    neue Unit
  16.05.00 wl                                    neuer Aufruf für SetWB..-Befehle im Carrier-Objekt
  26.05.00 wl                                    uses geändert
  12.03.03 wl                         TN1293.5   uses RessourceLoader
  09.11.07 pk  btnOKClick             TN3924     steps changed to mm
  20.06.08 pk                         TN4139     WB global object replaced by LayoutManager
  07.07.08 pk                         TN4139     WB_X field changed to PosX
  22.07.08 pk                         TN4139     Form generalized
  21.08.09 wl                         TN4702   Strings werden jetzt direkt geladen
  20.05.10 wl                         TN5117   neue Schriftart "Segoe UI", Schriftgröße 9
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
    ExtCtrls;

type
    TfrmEdXY = class(TForm)
        Panel3: TPanel;
        btnOK: TButton;
        Button2: TButton;
        Label1: TLabel;
        Label2: TLabel;
        edPosX: TEdit;
        edPosY: TEdit;
        procedure Button2Click(Sender: TObject);
        procedure btnOKClick(Sender: TObject);
        procedure FormClose(Sender: TObject; var Action: TCloseAction);
        procedure FormCreate(Sender: TObject);
    private
        function GetPosX: double;
        function GetPosY: double;
        procedure SetPosX(const aValue: double);
        procedure SetPosY(const aValue: double);
    public
        property PosX: double read GetPosX write SetPosX;
        property PosY: double read GetPosY write SetPosY;
    end;


implementation


{$R *.DFM}

uses
    GeneralTypes,
    ControlUtils;

{ TfrmEdXY }

procedure TfrmEdXY.Button2Click(Sender: TObject);
begin
    ModalResult := mrCancel;
end;

procedure TfrmEdXY.btnOKClick(Sender: TObject);
begin
    ModalResult := mrOK;
end;

procedure TfrmEdXY.FormClose(Sender: TObject; var Action: TCloseAction);
begin
    Action := caHide;
end;

procedure TfrmEdXY.FormCreate(Sender: TObject);
begin
    TControlUtils.ResetFontForWinXP(self);
    self.Caption := TLanguageString.Read('Change XY values', 'XY-Werte ändern');
    self.btnOK.Caption := TLanguageString.Read('&OK', '&OK');
    self.Button2.Caption := TLanguageString.Read('&Cancel', '&Abbrechen');
end;

function TfrmEdXY.GetPosX: double;
begin
    result := StrToFloat(self.edPosX.Text);
end;

function TfrmEdXY.GetPosY: double;
begin
    result := StrToFloat(self.edPosY.Text);
end;

procedure TfrmEdXY.SetPosX(const aValue: double);
begin
    self.edPosX.Text := FloatToStr(aValue);
end;

procedure TfrmEdXY.SetPosY(const aValue: double);
begin
    self.edPosY.Text := FloatToStr(aValue);
end;


end.
