{ ------------------------------------------------------------------------------------------------------------
  Copyright © 2010 Zinsser Analytic GmbH - All rights reserved.
  Author       : Wolfgang Lyncke (wl)
  Description  :
  ------------------------------------------------------------------------------------------------------------
  Revision History:
  date     op  method                            track-no improvement/change
  -------- --  --------------------------------  -------- ----------------------------------------------------
  03.12.10 pk                                    TN5381    Initial Revisions
  14.12.10 pk                                    TN5381    correct ordering of toolbuttons
  02.08.11 wl                                    TN5645   aufgeräumt, damit Enable/Disable der Start-Buttons endlich funktioniert
  11.04.12 wl                                    TN5861   Panel statt PageControl, damit der Reiter "Start Method" verschwindet
  ------------------------------------------------------------------------------------------------------------ }

unit MainRunDevelopment;


interface


uses
    Forms,
    Controls,
    ExtCtrls,
    ComCtrls,
    Classes,
    MainCustomDevelopment,
    MainRun,
    ToolWin;

type
    TfrmMainRunDevelopment = class(TMainCustomDevelopment)
        ToolBar1: TToolBar;
        Panel1: TPanel;
        procedure FormCreate(Sender: TObject);
    private
        ffrmMainRun: TfrmMainRun;
    protected
        procedure DoAddStandardToolBar(const aToolbar: TToolBar; const aPixelSize: integer); override;
        function GetPageControl: TPageControl; override;
    public
        property MainRun: TfrmMainRun read ffrmMainRun;
    end;


implementation


{$R *.dfm}

uses
    GeneralTypes,
    ControlUtils;

{ TfrmMainLayoutDevelopment }

procedure TfrmMainRunDevelopment.FormCreate(Sender: TObject);
begin
    TControlUtils.ResetFontForWinXP(self);
    self.Caption := TLanguageString.Read('Run', 'Run');
    fFrmMainRun := TfrmMainRun.Create(self);
    fFrmMainRun.Align := alClient;
    fFrmMainRun.ManualDock(self.Panel1);
    fFrmMainRun.Visible := true;

    self.AddStandardToolBar(self.ToolBar1);
end;

function TfrmMainRunDevelopment.GetPageControl: TPageControl;
begin
    result := nil;
end;

procedure TfrmMainRunDevelopment.DoAddStandardToolBar(const aToolbar: TToolBar; const aPixelSize: integer);
begin
    fToolbarUtils.CreateStartToolButton(aToolbar, aPixelSize);
    fToolbarUtils.CreateStartLastStartedToolButton(aToolbar, aPixelSize);
    fToolbarUtils.CreateFlushToolButton(aToolbar, aPixelSize);
    fToolbarUtils.CreateInitToolButton(aToolbar, aPixelSize);
end;


end.
