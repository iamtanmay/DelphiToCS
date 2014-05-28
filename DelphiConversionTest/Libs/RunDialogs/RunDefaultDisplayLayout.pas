{ ------------------------------------------------------------------------------------------------------------
  Copyright © 2009 Zinsser Analytic GmbH - All rights reserved.
  Author       : Wolfgang Lyncke (wl), Payman Kamali (pk)
  Description  :
  ------------------------------------------------------------------------------------------------------------
  Revision History:
  date     op  method                       track-no improvement/change
  -------- --  ---------------------------  -------- ---------------------------------------------------------
  06.04.09 pk                                TN4503  Initial Revision
  07.04.09 pk                                TN4503  dont set Stopbutton to visible
  26.10.09 wl                                TN4831   IConfigurationSet replaces ILocalIniFile
  20.05.10 wl                                TN5117   neue Schriftart "Segoe UI", Schriftgröße 9
  21.03.11 wl                                TN5508  SimulationInfo --> Elemente nach AskRunStart verlegt
  17.11.11 wl                                TN5725   PositionInfo-Fenster entfernt
  20.04.12 wl                                TN5858   StopButton-Panel entfernt
  ------------------------------------------------------------------------------------------------------------ }

unit RunDefaultDisplayLayout;


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
    ExtCtrls,
    ComCtrls;

type
    TfrmRunDefaultDisplayLayout = class(TForm)
        pnlMain: TPanel;
        pnlMainTop: TPanel;
        pnlDelayInfo: TPanel;
        LogPanel: TPanel;
        pnlOtherInfo: TPanel;
        pnlLayoutInfo: TPanel;
        pnlRunInfo: TPanel;
        Splitter1: TSplitter;
        Splitter2: TSplitter;
        Splitter3: TSplitter;
        pgctrlOtherInfo: TPageControl;
        procedure FormCreate(Sender: TObject);
        procedure FormDestroy(Sender: TObject);
    private const
        STR_LOCALINI_SECTION = 'ZARunnerPanel';
    private
        fLayoutInfoControl, fDelayInfoControl, fRunInfoControl, fLogInfoControl, fVolumesInfoControl,
            fSimulationInfoControl: TControl;
        procedure UnloadControl(aControl: TControl);
        procedure AddSheet(aControl: TControl; aCaption: string);
        procedure SavePositions();
        procedure LoadPositions();
    public
        procedure Load(const aLayoutInfoControl, aDelayInfoControl, aRunInfoControl, aLogInfoControl,
            aVolumesInfoControl: TControl; aIsVolumesInfoNeeded: boolean);
        procedure Unload();
    end;


implementation


{$R *.dfm}

uses
    CommonTypes,
    AppTypes,
    AppSettings,
    ConfigurationFile,
    ControlUtils;

procedure TfrmRunDefaultDisplayLayout.AddSheet(aControl: TControl; aCaption: string);
var
    xPageControl: TPageControl;
    xTabSheet: TTabSheet;
begin
    xPageControl := self.pgctrlOtherInfo;
    xTabSheet := TTabSheet.Create(xPageControl);
    xTabSheet.PageControl := xPageControl;
    xTabSheet.Caption := aCaption;
    aControl.Parent := xTabSheet;
    xTabSheet.PageIndex := xPageControl.PageCount - 1;
    xTabSheet.TabVisible := true;
    aControl.Visible := true;
end;

procedure TfrmRunDefaultDisplayLayout.Load(const aLayoutInfoControl, aDelayInfoControl, aRunInfoControl,
    aLogInfoControl, aVolumesInfoControl: TControl; aIsVolumesInfoNeeded: boolean);
begin
    fLayoutInfoControl := aLayoutInfoControl;
    aLayoutInfoControl.Parent := self.pnlLayoutInfo;
    aLayoutInfoControl.Visible := true;

    fDelayInfoControl := aDelayInfoControl;
    aDelayInfoControl.Parent := self.pnlDelayInfo;
    aDelayInfoControl.Visible := true;

    fRunInfoControl := aRunInfoControl;
    aRunInfoControl.Parent := self.pnlRunInfo;
    aRunInfoControl.Visible := true;

    fLogInfoControl := aLogInfoControl;
    AddSheet(fLogInfoControl, 'Logging');

    if aIsVolumesInfoNeeded then
    begin
        fVolumesInfoControl := aVolumesInfoControl;
        AddSheet(fVolumesInfoControl, 'Volumes');
    end
    else
    begin
        fVolumesInfoControl := nil;
    end;

    if pgctrlOtherInfo.PageCount > 0 then
        pgctrlOtherInfo.ActivePageIndex := 0;
end;

procedure TfrmRunDefaultDisplayLayout.UnloadControl(aControl: TControl);
begin
    if not Assigned(aControl) then
        EXIT;
    aControl.Parent := nil;
    aControl.Visible := false;
end;

procedure TfrmRunDefaultDisplayLayout.Unload();
begin
    UnloadControl(fSimulationInfoControl);
    UnloadControl(fVolumesInfoControl);
    UnloadControl(fLogInfoControl);
    UnloadControl(fDelayInfoControl);
    UnloadControl(fRunInfoControl);
    UnloadControl(fLayoutInfoControl);
end;

procedure TfrmRunDefaultDisplayLayout.SavePositions();
var
    xLocalIniFile: IConfigurationSet;
begin
    xLocalIniFile := TConfigurationFile.Create(TAppsettings.LocalIniFileName);
    xLocalIniFile.Open(false);
    try
        xLocalIniFile.WriteInteger(STR_LOCALINI_SECTION, 'LogPanelWidth', self.pnlOtherInfo.Width);
        xLocalIniFile.WriteInteger(STR_LOCALINI_SECTION, 'WBPanelWidth', self.pnlLayoutInfo.Width);
        xLocalIniFile.WriteInteger(STR_LOCALINI_SECTION, 'WBPanelHeight', self.pnlMainTop.Height);
    finally
        xLocalIniFile.Close;
    end;
end;

procedure TfrmRunDefaultDisplayLayout.LoadPositions();
var
    xLocalIniFile: IConfigurationSet;
begin
    xLocalIniFile := TConfigurationFile.Create(TAppsettings.LocalIniFileName);
    xLocalIniFile.Open(true);
    try
        self.pnlOtherInfo.Width := xLocalIniFile.ReadInteger(STR_LOCALINI_SECTION, 'LogPanelWidth', 550);
        self.pnlLayoutInfo.Width := xLocalIniFile.ReadInteger(STR_LOCALINI_SECTION, 'WBPanelWidth', 500);
        self.pnlMainTop.Height := xLocalIniFile.ReadInteger(STR_LOCALINI_SECTION, 'WBPanelHeight', 300);
    finally
        xLocalIniFile.Close;
    end;
end;

procedure TfrmRunDefaultDisplayLayout.FormCreate(Sender: TObject);
begin
    TControlUtils.ResetFontForWinXP(self);
    LoadPositions();
end;

procedure TfrmRunDefaultDisplayLayout.FormDestroy(Sender: TObject);
begin
    SavePositions();
end;


end.
