{ ------------------------------------------------------------------------------------------------------------
  BASEUNIT!
  ---------
  Copyright © 2011 Zinsser Analytic GmbH - All rights reserved.
  Author       : Wolfgang Lyncke (wl)
  Description  : MessageBox with Bitmap
  ------------------------------------------------------------------------------------------------------------
  Revision History:
  date     op  method                            track-no improvement/change
  -------- --  --------------------------------  -------- ----------------------------------------------------
  26.11.11 wl                                    TN5750   Initial Revision
  28.03.12 wl  CalculateBorders                  TN5849   Die Größe des Fensters richtet sich nach Bitmap-Größe und Textlänge
  28.03.12 wl  CalculateBorders                  TN5849   Bei Text = '' soll das Memo nicht zu sehen sein
  ------------------------------------------------------------------------------------------------------------ }

unit BitmapMessageBox;


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
    TfrmBitmapMessageBox = class(TForm)
        Shape2: TShape;
        Panel1: TPanel;
        btnOK: TButton;
        btnCancel: TButton;
        Panel2: TPanel;
        Image1: TImage;
        Memo1: TMemo;
    private
        procedure CalculateBorders(aText: string);
        procedure Setup(const aBitmapName, aText: string; const aCaption: string; aButtons: integer);
    public
        class function MessageBox(const aBitmapName, aText: string; const aCaption: string;
            aButtons: integer): Integer;
    end;


implementation


{$R *.DFM}

uses
    MathUtils,
    ControlUtils,
    Windows;

{ TGetNameDlg }

procedure TfrmBitmapMessageBox.CalculateBorders(aText: string);
var
    xTextSize: TSize;
    xClientHeight: integer;
    x: integer;
    xClientWidth: integer;
begin
    xClientWidth := TMathUtils.MaxIntValue([250, self.Image1.Picture.Width]);
    xClientHeight := self.Image1.Height + self.Panel1.Height;

    if (aText <> '') then
    begin
        // Höhe bestimmen
        xTextSize := self.Canvas.TextExtent(aText);
        xClientHeight := xClientHeight + self.Memo1.Margins.Top + self.Memo1.Margins.Bottom +
            (self.Memo1.Lines.Count * (xTextSize.cy));;

        // Breite bestimmen
        for x := 0 to self.Memo1.Lines.Count - 1 do
        begin
            xTextSize := self.Canvas.TextExtent(self.Memo1.Lines[x]);
            xTextSize.cx := xTextSize.cx + +self.Memo1.Margins.Left + self.Memo1.Margins.Right + 20;
            if (xTextSize.cx > xClientWidth) then
                xClientWidth := xTextSize.cx;
        end;
    end;

    self.ClientWidth := xClientWidth;
    self.ClientHeight := xClientHeight;

    // Nicht größer als der Bildschirm
    if self.Width > Screen.Width then
        self.Width := Screen.Width;
    if self.Height > Screen.Height then
        self.Height := Screen.Height;
end;

class function TfrmBitmapMessageBox.MessageBox(const aBitmapName, aText: string; const aCaption: string;
    aButtons: integer): Integer;
var
    xDialog: TfrmBitmapMessageBox;
begin
    xDialog := TfrmBitmapMessageBox.Create(nil);
    try
        xDialog.Setup(aBitmapName, aText, aCaption, aButtons);

        EXIT(xDialog.Showmodal);
    finally
        xDialog.Free;
    end;
end;

procedure TfrmBitmapMessageBox.Setup(const aBitmapName, aText, aCaption: string; aButtons: integer);
begin
    self.Caption := aCaption;

    self.Memo1.Clear;
    self.Memo1.Lines.Add(aText);
    self.Image1.Picture.LoadFromFile(aBitmapName);

    TControlUtils.ResetFontForWinXP(self);

    if (aButtons = MB_YESNO) then
    begin
        self.btnOK.Caption := TLanguageString.Read('Yes', 'Ja');
        self.btnCancel.Caption := TLanguageString.Read('No', 'Nein');
        self.btnOK.ModalResult := mrYes;
        self.btnCancel.ModalResult := mrNo;
    end
    else if (aButtons = MB_RETRYCANCEL) then
    begin
        self.btnOK.Caption := TLanguageString.Read('Retry', 'Wiederholen');
        self.btnCancel.Caption := TLanguageString.Read('Cancel', 'Abbrechen');
        self.btnOK.ModalResult := mrRetry;
        self.btnCancel.ModalResult := mrCancel;
    end
    else
    begin
        self.btnOK.Caption := 'OK';
        self.btnOK.ModalResult := mrOK;
        self.btnCancel.Visible := false;
    end;

    self.CalculateBorders(aText);
end;


end.
