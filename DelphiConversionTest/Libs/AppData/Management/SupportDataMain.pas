{ -----------------------------------------------------------------------------------------------------------
  Copyright © 2012 Zinsser Analytic GmbH - All rights reserved.
  Author       : Wolfgang Lyncke (wl)
  Description  : Data collection application for support cases
  -----------------------------------------------------------------------------------------------------------
  Revision History:
  date     op  method                              track-no improvement/change
  -------- --  ----------------------------------  -------- -------------------------------------------------
  02.11.12 wl                                      TN6003   Initial Revision
  27.01.13 wl                                      TN6069   an WinLissy angepasst
  11.02.13 wl                                      TN6078   IniFileName entfernt
  ----------------------------------------------------------------------------------------------------------- }

unit SupportDataMain;


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
    StdCtrls,
    ComCtrls,
    SupportDataUtils;

type
    TfrmSupportDataMain = class(TForm)
        Button1: TButton;
        Button2: TButton;
        DateTimePicker1: TDateTimePicker;
        Label3: TLabel;
        RadioButton1: TRadioButton;
        RadioButton2: TRadioButton;
        Memo1: TMemo;
        ProgressBar1: TProgressBar;
        procedure Button2Click(Sender: TObject);
        procedure FormCreate(Sender: TObject);
        procedure FormDestroy(Sender: TObject);
        procedure Button1Click(Sender: TObject);
    private
        fBackup: TSupportDataBackup;
        function CollectFiles(): string;
        procedure SetProgressBarPosition(aValue: integer);
        procedure AddText(const aText: string);
    end;


implementation


{$R *.dfm}
{ TForm8 }

procedure TfrmSupportDataMain.AddText(const aText: string);
begin
    Memo1.Lines.Add(aText);
end;

procedure TfrmSupportDataMain.Button1Click(Sender: TObject);
begin
    self.CollectFiles();
end;

procedure TfrmSupportDataMain.Button2Click(Sender: TObject);
var
    xAddresses: TArray<Ansistring>;
    xFiles: TArray<string>;
    xMailText: string;
begin
    xAddresses := TSupportDataUtils.StringToStringArray(fBackup.GetAddresses, ';');

    SetLength(xFiles, 1);
    xFiles[0] := self.CollectFiles();

    if RadioButton2.Checked then
        xMailText := 'Attached: Support data'
    else
        xMailText := 'Attached: Data of ' + DateTimeToStr(DateTimePicker1.DateTime);

    TSupportDataUtils.SendMail('Support case', AnsiString(xMailText), 'ZinsserWinlissy', xAddresses, xFiles,
        '', true);
end;

function TfrmSupportDataMain.CollectFiles(): string;
begin
    self.ProgressBar1.Position := 0;
    self.ProgressBar1.Visible := true;
    self.Button1.Enabled := false;
    self.Button2.Enabled := false;
    try
        Memo1.Lines.Clear();

        EXIT(fBackup.CollectFiles(RadioButton2.Checked, DateTimePicker1.DateTime));
    finally
        self.ProgressBar1.Visible := false;
        self.Button1.Enabled := true;
        self.Button2.Enabled := true;
    end;
end;

procedure TfrmSupportDataMain.FormCreate(Sender: TObject);
begin

    fBackup := TSupportDataBackup.Create(self.SetProgressBarPosition, self.AddText);
    self.Button2.Visible := fBackup.UseMail;
    self.Button2.Default := fBackup.UseMail;
    self.Button1.Default := not fBackup.UseMail;
    self.DateTimePicker1.DateTime := now;
end;

procedure TfrmSupportDataMain.FormDestroy(Sender: TObject);
begin
    FreeAndNil(fBackup)
end;

procedure TfrmSupportDataMain.SetProgressBarPosition(aValue: integer);
begin
    self.ProgressBar1.Position := aValue;
end;


end.
