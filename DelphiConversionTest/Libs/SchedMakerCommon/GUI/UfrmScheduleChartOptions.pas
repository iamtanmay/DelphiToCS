unit UfrmScheduleChartOptions;
{ --------------------------------------------------------------------------------------------------
  Copyright © 2005 Zinsser Analytic GmbH - All rights reserved.
  Author       : Payman Kamali (pk)
  Description  :
  --------------------------------------------------------------------------------------------------
  Revision History:
  date     op  method                       track-no improvement/change
  -------- --  ---------------------------  -------- -----------------------------------------------
  18.01.05 pk                               TN2281   Initial Revision
  09.01.08 wl                               TN3972    uses RessourceLoader
  14.04.08 wl                               TN4060   uses DialogUtils
  19.08.09 wl                               TN4702   Strings werden jetzt direkt geladen
  09.04.10 wl  EnableBorderIcons,RemoveBorderIcons  TN5044  von DialogUtils hierher
  20.05.10 wl                                TN5117   neue Schriftart "Segoe UI", Schriftgröße 9
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
    ComCtrls;

type
    TSchedChartFormOptionsRec = record
        AbsTimeMode: boolean;
        AbsTimeDefined: boolean;
        AbsTime: TDateTime;
    end;

    TBorderIconType = (biMax, biMin, biClose);
    TBorderIconTypes = set of TBorderIconType;

    TFrmScheduleChartOptions = class(TForm)
        pnlBottom: TPanel;
        btnCancel: TButton;
        Bevel2: TBevel;
        pnlTabs: TPanel;
        pgctlMain: TPageControl;
        shtImportDefs: TTabSheet;
        rgrpTimeMode: TRadioGroup;
        grpAbsTimeMode: TGroupBox;
        rgrpTimeDefined: TRadioGroup;
        grpTimeDefinedAt: TGroupBox;
        calDate: TMonthCalendar;
        dtpTime: TDateTimePicker;
        Label1: TLabel;
        Label2: TLabel;
        btnOK: TButton;
        procedure FormCreate(Sender: TObject);
        procedure rgrpTimeModeClick(Sender: TObject);
        procedure rgrpTimeDefinedClick(Sender: TObject);

    private
        procedure ResultRecToGUI(const aResultRec: TSchedChartFormOptionsRec);
        procedure ResultRecFromGUI(var aResultRec: TSchedChartFormOptionsRec);

        function AbsTimeModeFromGUI(): boolean;
        procedure AbsTimeModeToGUI(aAbsTimeMode: boolean);
        function AbsTimeDefinedFromGUI(): boolean;
        procedure AbsTimeDefinedToGUI(AbsTimeDefined: boolean);
        function TimeDefinedAtFromGUI(): TDateTime;
        procedure TimeDefinedAtToGUI(aDateTime: TDateTime);

        function GetTimeFromGUI(): TDateTime;
        function GetDateFromGUI(): TDateTime;

        procedure EnableTimeDefinedAt(aValue: boolean);
        procedure EnableAbsTimeMode(aValue: boolean);
    public
        class function ModalOpen(var aResultRec: TSchedChartFormOptionsRec): TModalResult;
        class procedure EnableBorderIcons(aForm: TForm; aBorderIcons: TBorderIconTypes; aValue: boolean);
        class procedure RemoveBorderIcons(aForm: TForm; aBorderIcons: TBorderIconTypes);
    end;


implementation


{$R *.DFM}

uses
    ControlUtils;

const
    INT_PAGECONTROL_INDEX_IMPORTDEFS = 0;
    INT_PAGECONTROL_INDEX_IMPORTFILEDEFS = 1;

    INT_TIMEMODE_RELATIVE = 0;
    INT_TIMEMODE_ABSOLUTE = 1;

    INT_ABSTIMEDEFINED_NOW = 0;
    INT_ABSTIMEDEFINED_AT = 1;

procedure TFrmScheduleChartOptions.FormCreate(Sender: TObject);
begin
    TControlUtils.ResetFontForWinXP(self);
    TfrmScheduleChartOptions.RemoveBorderIcons(self, [biClose]);
    pgctlMain.ActivePageIndex := INT_PAGECONTROL_INDEX_IMPORTDEFS;
end;

procedure TFrmScheduleChartOptions.ResultRecToGUI(const aResultRec: TSchedChartFormOptionsRec);
var
    xAbsTimeDefined: boolean;
begin
    AbsTimeModeToGUI(aResultRec.AbsTimeMode);
    xAbsTimeDefined := aResultRec.AbsTimeMode and aResultRec.AbsTimeDefined;
    AbsTimeDefinedToGUI(xAbsTimeDefined);
    if not xAbsTimeDefined then
        EXIT;
    TimeDefinedAtToGUI(aResultRec.AbsTime);
end;

procedure TFrmScheduleChartOptions.ResultRecFromGUI(var aResultRec: TSchedChartFormOptionsRec);
begin
    aResultRec.AbsTimeMode := AbsTimeModeFromGUI();
    if not aResultRec.AbsTimeMode then
        EXIT;

    aResultRec.AbsTimeDefined := AbsTimeDefinedFromGUI();
    if not aResultRec.AbsTimeDefined then
        EXIT;

    aResultRec.AbsTime := TimeDefinedAtFromGUI();
end;

function TFrmScheduleChartOptions.AbsTimeModeFromGUI(): boolean;
begin
    result := self.rgrpTimeMode.ItemIndex = INT_TIMEMODE_ABSOLUTE;
end;

procedure TFrmScheduleChartOptions.AbsTimeModeToGUI(aAbsTimeMode: boolean);
begin
    if aAbsTimeMode then
        self.rgrpTimeMode.ItemIndex := INT_TIMEMODE_ABSOLUTE
    else
        self.rgrpTimeMode.ItemIndex := INT_TIMEMODE_RELATIVE;
end;

function TFrmScheduleChartOptions.AbsTimeDefinedFromGUI(): boolean;
begin
    result := self.rgrpTimeDefined.ItemIndex = INT_ABSTIMEDEFINED_AT;
end;

procedure TFrmScheduleChartOptions.AbsTimeDefinedToGUI(AbsTimeDefined: boolean);
begin
    if AbsTimeDefined then
        self.rgrpTimeDefined.ItemIndex := INT_ABSTIMEDEFINED_AT
    else
        self.rgrpTimeDefined.ItemIndex := INT_ABSTIMEDEFINED_NOW;
end;

function TfrmScheduleChartOptions.GetTimeFromGUI(): TDateTime;
begin
    result := Frac(self.dtpTime.Time);
end;

function TfrmScheduleChartOptions.GetDateFromGUI(): TDateTime;
begin
    result := Int(self.calDate.Date);
end;

function TfrmScheduleChartOptions.TimeDefinedAtFromGUI(): TDateTime;
begin
    result := GetDateFromGUI + GetTimeFromGUI();
end;

procedure TfrmScheduleChartOptions.TimeDefinedAtToGUI(aDateTime: TDateTime);
begin
    self.calDate.Date := Int(aDateTime);
    self.dtpTime.Time := Frac(aDateTime);
end;

class function TFrmScheduleChartOptions.ModalOpen(var aResultRec: TSchedChartFormOptionsRec): TModalResult;
var
    xForm: TFrmScheduleChartOptions;
begin
    xForm := TFrmScheduleChartOptions.Create(nil);
    try
        xForm.ResultRecToGUI(aResultRec);
        result := xForm.ShowModal;
        if result <> mrOK then
            EXIT;
        xForm.ResultRecFromGUI(aResultRec);
    finally
        xForm.Free;
    end;
end;

procedure TFrmScheduleChartOptions.EnableTimeDefinedAt(aValue: boolean);
begin
    TControlUtils.SetGroupEnabled(self.grpTimeDefinedAt, aValue);
end;

procedure TFrmScheduleChartOptions.EnableAbsTimeMode(aValue: boolean);
begin
    TControlUtils.SetGroupEnabled(self.grpAbsTimeMode, aValue);
end;

procedure TFrmScheduleChartOptions.rgrpTimeModeClick(Sender: TObject);
begin
    EnableAbsTimeMode(AbsTimeModeFromGUI());
end;

procedure TFrmScheduleChartOptions.rgrpTimeDefinedClick(Sender: TObject);
begin
    EnableTimeDefinedAt(AbsTimeDefinedFromGUI());
end;

class procedure TfrmScheduleChartOptions.EnableBorderIcons(aForm: TForm; aBorderIcons: TBorderIconTypes;
    aValue: boolean);
var
    hMenuHandle: HMENU;
    xFlags: cardinal;
    procedure Enable(aID: longint; aFlags: cardinal);
    begin
        EnableMenuItem(hMenuHandle, aID, aFlags);
    end;

begin
    hMenuHandle := GetSystemMenu(aForm.Handle, False);
    if (hMenuHandle <> 0) then
    begin
        if aValue then
            xFlags := MF_ENABLED
        else
            xFlags := MF_GRAYED;
        if biMax in aBorderIcons then
            Enable(SC_MAXIMIZE, xFlags);
        if biClose in aBorderIcons then
            Enable(SC_CLOSE, xFlags);
        if biMin in aBorderIcons then
            Enable(SC_MINIMIZE, xFlags);
    end;
end;

class procedure TfrmScheduleChartOptions.RemoveBorderIcons(aForm: TForm; aBorderIcons: TBorderIconTypes);
begin
    EnableBorderIcons(aForm, aBorderIcons, false);
end;


end.
