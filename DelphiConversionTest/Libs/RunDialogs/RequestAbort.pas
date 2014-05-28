{ ----------------------------------------------------------------------------------------------------------------------
  Copyright © 2012 Zinsser Analytic GmbH - All rights reserved.
  Author       : Wolfgang Lyncke (wl)
  Description  :
  ----------------------------------------------------------------------------------------------------------------------
  Revision History:
  date     op  method                       track-no improvement/change
  -------- --  ---------------------------  -------- -------------------------------------------------------------------
  09.05.12 wl                               TN5858   Initial Revision
  ---------------------------------------------------------------------------------------------------------------------- }

unit RequestAbort;


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
    StringLoader,
    ImgList;

type
    TRequestAbortStringLoader = class(TTextListStringLoader)
    protected
        procedure AddAllItems(); override;
    end;

    TfrmRequestAbort = class(TForm)
        Panel2: TPanel;
        Button2: TButton;
        Button1: TButton;
        Bevel1: TBevel;
        Button3: TButton;
        ImageList1: TImageList;
        Panel1: TPanel;
        Label1: TLabel;
        procedure FormCreate(Sender: TObject);
        procedure FormDestroy(Sender: TObject);
    private
        fStringLoader: TRequestAbortStringLoader;
    public
        class function ShowStopDialog(): integer;
    end;


implementation


{$R *.DFM}

uses
    ControlUtils;

{ TStopDialogStringLoader }

procedure TRequestAbortStringLoader.AddAllItems;
begin
    AddSingle(560, '&Abort process', 'Prozess abbrechen');
    AddSingle(570, '&Pause process', 'Prozess &anhalten');
    AddSingle(580, '&Continue process', 'Prozess &fortsetzen');
    AddSingle(10700, 'Process interrupt', 'Prozess-Unterbrechung');
    AddSingle(10710, 'You paused the process. What should be done?',
        'Sie haben den Prozess angehalten. Wie wollen Sie fortfahren?');
end;

{ TStopDlg }

procedure TfrmRequestAbort.FormCreate(Sender: TObject);
begin
    TControlUtils.ResetFontForWinXP(self);
    fStringLoader := TRequestAbortStringLoader.Create;
    fStringLoader.LoadLanguage(self);
end;

procedure TfrmRequestAbort.FormDestroy(Sender: TObject);
begin
    fStringLoader.Free;
end;

class function TfrmRequestAbort.ShowStopDialog: integer;
var
    xDlg: TfrmRequestAbort;
begin
    xDlg := TfrmRequestAbort.Create(nil);
    try
        result := xDlg.ShowModal;
    finally
        xDlg.Free;
    end;
end;


end.
