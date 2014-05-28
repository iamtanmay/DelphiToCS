unit ufrmScheduleChart;
{ --------------------------------------------------------------------------------------------------
  Copyright © 2002 Zinsser Analytic GmbH - All rights reserved.
  Author       : Payman Kamali (pk)
  Description  :
  --------------------------------------------------------------------------------------------------
  Revision History:
  date     op  method                       track-no improvement/change
  -------- --  ---------------------------  -------- -----------------------------------------------
  14.06.02 pk                                        Created
  17.06.02 pk  FormCreate                            Cursor names changed
  24.06.02 pk  ShowInfo                              memobox changed to listview
  26.06.02 pk  ShowInfo                              listbox changed to listview
  12.07.02 pk  Create                                Form inherits from TDockableForm
  08.08.02 pk  Redraw                                Exits before drawing if Script not Scheduled
  02.09.02 pk  FOpenStyles                           Keep track of open styles
  02.09.02 pk  Redraw                                aBlnLive argument added
  02.09.02 pk  RedrawActiveScript                    aBlnLive argument added
  02.09.02 pk                                        View3D feature added
  30.01.02 mo                                TN1416.1 Methoden TDockableForm ausgeklammert
  12.03.03 wl                                TN1293.5 uses posTools
  14.03.03 wl                                TN1416.1 TDockableForm replaced by TForm
  08.12.03 pk  Redraw                        TN1697.1 accepts Elapsedtime parameter
  21.07.04 pk                                TN2049   various changes
  18.01.05 pk  Zoom                          TN2281   Removed
  18.01.05 pk                                TN2281   New : DrawMode, TimeMode, Options menu
  18.01.05 pk  Draw                          TN2281   Set .StartDate
  02.03.05 pk                                TN2328   TMainSchedChart based on TSchedMakerSession
  19.04.05 pk                                TN2389   Changes for EditElement
  21.04.05 pk  ShowInfo                      TN2396   Show element resource IDs
  25.08.05 wl                                TN2558.8  toter Quelltext entfernt
  07.11.05 pk                                TN2737   Various changes for Dynamic Scheduler
  18.04.06 pk  ShowElementInfo               TN3048   Show shared id and resources
  06.06.06 pk  ShowElementInfo               TN3131   do not add current selected ID, it will be added automatically in for loop
  06.06.06 pk  GetSelectedSyncListIndex      TN3131   return -1 if no element is selected
  09.01.08 wl                                TN3972    uses RessourceLoader
  14.04.08 wl                                TN4060   uses DialogUtils
  10.08.09 wl                               TN4702   Strings werden jetzt direkt geladen
  26.10.09 wl  Destroy                       TN4831   groß geschrieben wg. Compiler-Warning
  09.04.10 wl                                TN5044   EnableBorderIcons: von DialogUtils nach UfrmScheduleChartOptions
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
    ArrowCha,
    TeEngine,
    Series,
    GanttCh,
    ExtCtrls,
    TeeProcs,
    Chart,
    StdCtrls,
    Menus,
    Buttons,
    syncobjs,
    ComCtrls,
    AppTypes,
    UtilLib,
    ImplMainSchedChart,
    UfrmScheduleChartOptions,
    SchedMakerClasses;

const
    crCustInfo = 5;
    crCustZoom = 6;

type
    TFormEvent = procedure(aForm: TForm) of object;
    TRescheduleEvent = procedure(aName: string) of object;
    TFuncEditEvent = procedure(aName: string; aID: integer) of object;
    TViewMode = (osTop, osBottom, osTools);
    TViewModes = set of TViewMode;

    TfrmScheduleChart = class(TForm)
        pnlForm: TPanel;
        pnlChartBorder: TPanel;
        pnlChart: TPanel;
        pnlTop: TPanel;
        pnlBottom: TPanel;
        pnlElement: TPanel;
        pnlTools1: TPanel;
        Bevel4: TBevel;
        pnlTitle: TPanel;
        lblTitle: TLabel;
        Panel1: TPanel;
        pnlSynch: TPanel;
        Panel3: TPanel;
        lblParallelElements: TLabel;
        Panel2: TPanel;
        edSynchWatchID: TEdit;
        lviewSynch: TListView;
        pnlElementBottom: TPanel;
        lviewInfo: TListView;
        pnlElementTop: TPanel;
        Label1: TLabel;
        btnEditElement: TSpeedButton;
        btnShowPrev: TSpeedButton;
        btnShowNext: TSpeedButton;
        chkView3D: TCheckBox;
        lblName: TLabel;
        Panel5: TPanel;
        PopupMenu1: TPopupMenu;
        mniTop: TMenuItem;
        mniBottom: TMenuItem;
        mniDrawMode: TMenuItem;
        mniLiveMode: TMenuItem;
        mniInfoMode: TMenuItem;
        mniDesignMode: TMenuItem;
        mniViewMode: TMenuItem;
        mniPrint: TMenuItem;
        mniOptions: TMenuItem;
        stName: TStaticText;

        procedure FormCreate(Sender: TObject);
        procedure btnEditElementClick(Sender: TObject);
        procedure btnShowNextClick(Sender: TObject);
        procedure btnShowPrevClick(Sender: TObject);
        procedure edScriptNameEnter(Sender: TObject);
        procedure FormKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
        procedure Exit1Click(Sender: TObject);
        procedure lviewSynchKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
        procedure FormClose(Sender: TObject; var Action: TCloseAction); overload;
        procedure FormDestroy(Sender: TObject);
        procedure chkView3DClick(Sender: TObject);
        procedure mniBottomClick(Sender: TObject);
        procedure mniTopClick(Sender: TObject);
        procedure mniLiveModeClick(Sender: TObject);
        procedure mniInfoModeClick(Sender: TObject);
        procedure mniDesignModeClick(Sender: TObject);
        procedure mniPrintClick(Sender: TObject);
        procedure edSynchWatchChange(Sender: TObject);
        procedure mniOptionsClick(Sender: TObject);
        procedure lviewSynchClick(Sender: TObject);
    private
        FOnFormClose: TFormEvent;
        FOnFormOpen: TFormEvent;
        FOnReschedule: TRescheduleEvent;
        FOnOpenFuncEditForm: TFuncEditEvent;
        fViewModes: TViewModes;
        FCriticalSection: TCriticalSection;

        procedure ClearInfoDisplays;
        procedure ClearSynchDisplays;
        procedure SelectElement(aElementID: integer; aUpdateParallelList: boolean);
        procedure ShowElementInfo(aElementID: integer; aUpdateParallelList: boolean);
        procedure ShowNeighborElementInfo(intDirection: integer);
        procedure OnChartClickSeries(Sender: TCustomChart; Series: TChartSeries; ValueIndex: Integer;
            Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
        procedure DoOnMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
        procedure SchedChartOptionsFromGUI(var aRec: TSchedChartFormOptionsRec);
        procedure SchedChartOptionsToGUI(const aRec: TSchedChartFormOptionsRec);
        procedure SetSession(aSession: TSchedMakerSession);
    protected
        fMainSchedChart: TMainSchedChart;
        fElementEditAllowed: boolean;

        procedure RedrawActiveDatasource(aElapsedTime: cardinal = 0);
        procedure Zoom();
        property MainSchedChart: TMainSchedChart read FMainSchedChart;
        procedure MenuChangeDrawMode(aMenuItem: TMenuItem; aDrawMode: TSchedChartDrawMode);
        procedure SetFormStyle(); virtual;
        procedure ViewBottomChange(aValue: boolean);
        procedure ViewToolsChange(aValue: boolean);
        procedure ViewTopChange(aValue: boolean);
        procedure ViewModeChange(aViewMode: TViewMode; aValue: boolean);
        procedure SetModes(aViewModes: TViewModes);
        procedure SetMode(aViewMode: TViewMode; aValue: boolean);
        procedure SetDrawMode(aValue: TSchedChartDrawMode);
        function GetDrawMode(): TSchedChartDrawMode;
        procedure SetTimeMode(aTimeMode: TMainSchedChartTimeMode);
        function GetTimeMode(): TMainSchedChartTimeMode;
        procedure EditElement(aChartID: integer); virtual;
        procedure AddSyncElement(aPriority: integer; const aAction: string; const aRes: string;
            aSharedID: integer);
        function GetSelectedSyncListIndex(): integer;
        procedure SyncListSelectedChanged();
        procedure RefreshSessionName();
    public
        constructor Create(aSession: TSchedMakerSession); reintroduce; virtual;
        destructor Destroy(); override;
        procedure Draw();
        procedure Redraw(aElapsedTime: cardinal = 0);
        procedure FormOpen(); virtual;
        procedure FormFinish; virtual;
        procedure SetWindowState(aState: TWindowState);

        procedure EnableClose(aValue: boolean);

        // Events
        property OnFormClose: TFormEvent read FOnFormClose write FOnFormClose;
        property OnFormOpen: TFormEvent read FOnFormOpen write FOnFormOpen;
        property OnReschedule: TRescheduleEvent read FOnReschedule write FOnReschedule;
        property OnOpenFuncEditForm: TFuncEditEvent read FOnOpenFuncEditForm;
        property ViewModes: TViewModes read fViewModes write SetModes;
        property DrawMode: TSchedChartDrawMode read GetDrawMode write SetDrawMode;
        property TimeMode: TMainSchedChartTimeMode read GetTimeMode write SetTimeMode;
        property Session: TSchedMakerSession write SetSession;
    end;

var
    frmScheduleChart: TfrmScheduleChart;


implementation


uses
    GeneralTypes,
    ControlUtils;

{$R *.DFM}

const
    INT_FIRST_SYNC_INDEX = 0;

    // --------------------------------------------------------------------------------------------------
constructor TfrmScheduleChart.Create(aSession: TSchedMakerSession);
// --------------------------------------------------------------------------------------------------
begin
    inherited Create(Application);
    // Important! let the Application be the owner. Otherwise on Application Exit it wont be freed
    TControlUtils.ResetFontForWinXP(self);
    SetFormStyle();
    FMainSchedChart := TMainSchedChart.Create(aSession);
    RefreshSessionName();
    ViewModes := [osTop, osBottom, osTools];
    DrawMode := dmDesign;
    FCriticalSection := TCriticalSection.Create();
    TimeMode := mtmRel;
    fElementEditAllowed := false;
end;

// ------------------------------------------------------------------------------
destructor TfrmScheduleChart.Destroy();
// ------------------------------------------------------------------------------
begin
    FreeAndNil(FCriticalSection);
    FreeAndNil(FMainSchedChart);
    frmScheduleChart := nil;
    inherited;
end;

procedure TfrmScheduleChart.SetSession(aSession: TSchedMakerSession);
begin
    fMainSchedChart.Session := aSession;
    RefreshSessionName();
end;

// ------------------------------------------------------------------------------
procedure TfrmScheduleChart.FormCreate(Sender: TObject);
// ------------------------------------------------------------------------------
begin
    Screen.Cursors[crCustInfo] := LoadCursor(HInstance, MAKEINTRESOURCE('INFO'));
    Screen.Cursors[crCustZoom] := LoadCursor(HInstance, MAKEINTRESOURCE('ZOOM'));

    // TCommonLinkAll.ListenLoadFormPosition(self,true);
    // self.Visible:=false;

end;

procedure TfrmScheduleChart.FormOpen();
begin
end;

// ------------------------------------------------------------------------------
procedure TfrmScheduleChart.FormClose(Sender: TObject; var Action: TCloseAction);
// ------------------------------------------------------------------------------
begin
    Action := caFree;
end;

// ------------------------------------------------------------------------------
procedure TfrmScheduleChart.FormDestroy(Sender: TObject);
// ------------------------------------------------------------------------------
begin
    // TCommonLinkAll.ListenSaveFormPosition(self);
end;

// ------------------------------------------------------------------------------
procedure TfrmScheduleChart.FormFinish;
// ------------------------------------------------------------------------------
begin
    if self.Visible then
        self.Close;
end;

// --------------------------------------------------------------------------------------------------
procedure TfrmScheduleChart.SetFormStyle();
// --------------------------------------------------------------------------------------------------
begin
end;

// --------------------------------------------------------------------------------------------------
procedure TfrmScheduleChart.EnableClose(aValue: boolean);
// --------------------------------------------------------------------------------------------------
begin
    TfrmScheduleChartOptions.EnableBorderIcons(self, [biClose], aValue);
end;

// --------------------------------------------------------------------------------------------------
procedure TfrmScheduleChart.SetMode(aViewMode: TViewMode; aValue: boolean);
// --------------------------------------------------------------------------------------------------
begin
    case aViewMode of
        osTop:
            ViewTopChange(aValue);
        osBottom:
            ViewBottomChange(aValue);
        osTools:
            ViewToolsChange(aValue);
    end;

end;

// ------------------------------------------------------------------------------
procedure TfrmScheduleChart.SetModes(aViewModes: TViewModes);
// ------------------------------------------------------------------------------
var
    i: integer;
    xMode: TViewMode;
begin
    for i := 0 to Integer( high(TViewMode)) do
    begin
        xMode := TViewMode(i);
        if xMode in (fViewModes - aViewModes) then
            SetMode(xMode, false)
        else if xMode in (aViewModes - fViewModes) then
            SetMode(xMode, true)
    end;

end;

// --------------------------------------------------------------------------------------------------
procedure TfrmScheduleChart.ViewModeChange(aViewMode: TViewMode; aValue: boolean);
// --------------------------------------------------------------------------------------------------
begin
    if aValue then
        Include(fViewModes, aViewMode)
    else
        Exclude(fViewModes, aViewMode);
end;

// --------------------------------------------------------------------------------------------------
procedure TfrmScheduleChart.ViewTopChange(aValue: boolean);
// --------------------------------------------------------------------------------------------------
begin

    mniTop.Checked := aValue;
    self.pnlTop.Visible := aValue;
    ViewModeChange(osTop, aValue);
end;

// --------------------------------------------------------------------------------------------------
procedure TfrmScheduleChart.ViewBottomChange(aValue: boolean);
// --------------------------------------------------------------------------------------------------
begin
    if aValue then
    begin
        ClearInfoDisplays();
        ClearSynchDisplays();
    end;
    mniBottom.Checked := aValue;
    self.pnlBottom.Visible := aValue;
    ViewModeChange(osBottom, aValue);
end;

// --------------------------------------------------------------------------------------------------
procedure TfrmScheduleChart.ViewToolsChange(aValue: boolean);
// --------------------------------------------------------------------------------------------------
begin
    self.pnlTools1.Visible := aValue;
    ViewModeChange(osTools, aValue);
end;

// --------------------------------------------------------------------------------------------------
procedure TfrmScheduleChart.SetDrawMode(aValue: TSchedChartDrawMode);
// --------------------------------------------------------------------------------------------------
begin

    case aValue of
        dmDesign:
            mniDesignMode.Checked := true;
        dmLive:
            mniLiveMode.Checked := true;
        dmLiveInfo:
            mniInfoMode.Checked := true;
    end;
    ViewBottomChange(aValue <> dmLive);
    mniOptions.Visible := not(aValue in [dmLive, dmLiveInfo]);
    fMainSchedChart.DrawMode := aValue;
end;

// --------------------------------------------------------------------------------------------------
function TfrmScheduleChart.GetDrawMode(): TSchedChartDrawMode;
// --------------------------------------------------------------------------------------------------
begin
    result := fMainSchedChart.DrawMode
end;

// --------------------------------------------------------------------------------------------------
procedure TfrmScheduleChart.SetWindowState(aState: TWindowState);
// --------------------------------------------------------------------------------------------------
begin
    self.WindowState := aState;
end;

// --------------------------------------------------------------------------------------------------
procedure TfrmScheduleChart.Draw();
// --------------------------------------------------------------------------------------------------
begin
    fMainSchedChart.StartDate := Now();
    ReDraw();
    FMainSchedChart.OnChartClickSeries := OnChartClickSeries;
    FMainSchedChart.SchedChart.OnMouseDown := DoOnMouseDown;
    Zoom();
end;

// --------------------------------------------------------------------------------------------------
procedure TfrmScheduleChart.Redraw(aElapsedTime: cardinal = 0);
// --------------------------------------------------------------------------------------------------
begin
    FCriticalSection.Acquire();
    try
        FMainSchedChart.DrawChart(self.pnlChart, DrawMode, aElapsedTime);
        self.Show;
    finally
        FCriticalSection.Release();
    end;
end;

// ------------------------------------------------------------------------------
procedure TfrmScheduleChart.RedrawActiveDatasource(aElapsedTime: cardinal = 0);
// ------------------------------------------------------------------------------
begin
    ReDraw(aElapsedTime);
end;

// ------------------------------------------------------------------------------
procedure TfrmScheduleChart.ClearInfoDisplays;
// ------------------------------------------------------------------------------
begin
    lviewInfo.Items.Clear;
end;

// ------------------------------------------------------------------------------
procedure TfrmScheduleChart.ClearSynchDisplays;
// ------------------------------------------------------------------------------
begin
    lviewSynch.Items.Clear; // lstSynch.Items.Clear;
    edSynchWatchID.Text := '-1';
end;

procedure TfrmScheduleChart.SelectElement(aElementID: integer; aUpdateParallelList: boolean);
begin
    if FMainSchedChart.VisualSelectElement(aElementID) = 0 then
    begin
        TControlUtils.SetControlMemberProperty(self.pnlElementTop, cpEnabled, false);
        // if fDatasourceType <> sdsScript then self.btnOpenUFuncSel.Enabled := false;
        // SetGroupEnabled(self.grpInfoButtons,false);
        ClearInfoDisplays();
        ClearSynchDisplays();
        Exit;
    end;
    ShowElementInfo(aElementID, aUpdateParallelList);
end;

procedure TfrmScheduleChart.AddSyncElement(aPriority: integer; const aAction: string; const aRes: string;
    aSharedID: integer);
begin
    with lviewSynch.Items.Add do
    begin
        Caption := IntToStr(aPriority);
        SubItems.Add(aAction);
        SubItems.Add(aRes);
        SubItems.Add(IntToStr(aSharedID))
    end;
end;

// --------------------------------------------------------------------------------------------------
procedure TfrmScheduleChart.ShowElementInfo(aElementID: integer; aUpdateParallelList: boolean);
// --------------------------------------------------------------------------------------------------
var
    strFormat: string;
    intTime, intTemp: integer;
    i, intChartID, intSynchCount: integer;
    xElementInfo, xSynchElementInfo: TElementInfoRec;
begin

    if not(osBottom in fViewModes) then
        Exit;

    TControlUtils.SetControlMemberProperty(self.pnlElementTop, cpEnabled, true);
    self.btnEditElement.Enabled := fElementEditAllowed and (self.DrawMode in [dmLiveInfo, dmDesign]);

    // SetGroupEnabled(self.grpInfoButtons,true);

    xElementInfo := FMainSchedChart.GetElementInfo(aElementID, true);

    lviewInfo.Items.Clear;
    with lviewInfo.Items.Add do
    begin
        Caption := TLanguageString.Read('Priority No.', 'Priorität');
        SubItems.Add(IntToStr(xElementInfo.LevelID));
    end;
    with lviewInfo.Items.Add do
    begin
        Caption := TLanguageString.Read('Action Name', 'Aktionsname');
        SubItems.Add(xElementInfo.Text);
    end;
    strFormat := '[%d(s)]';
    with lviewInfo.Items.Add do
    begin
        intTime := xElementInfo.StartTime;
        intTemp := intTime;
        Caption := TLanguageString.Read('Start Time', 'Start Zeit');
        SubItems.Add(Format(strFormat, [intTime]));
        SubItems.Add(FMainSchedChart.SchedChart.GetValueLabel(intTime));
    end;
    with lviewInfo.Items.Add do
    begin
        intTime := xElementInfo.EndTime;
        Caption := TLanguageString.Read('End Time', 'End Zeit');
        SubItems.Add(Format(strFormat, [intTime]));
        SubItems.Add(FMainSchedChart.SchedChart.GetValueLabel(intTime));
    end;

    with lviewInfo.Items.Add do
    begin
        intTime := intTime - intTemp;
        Caption := TLanguageString.Read('Duration', 'Dauer');
        SubItems.Add(Format(strFormat, [intTime]));
    end;
    with lviewInfo.Items.Add do
    begin
        intTime := xElementInfo.MinTime;
        intTemp := intTime;
        Caption := TLanguageString.Read('Minimum Time', 'Min. Zeit');
        SubItems.Add(Format(strFormat, [intTime]));
    end;
    with lviewInfo.Items.Add do
    begin
        intTime := xElementInfo.MaxTime - intTemp;
        Caption := TLanguageString.Read('Extended By', 'Verlängert um');
        SubItems.Add(Format(strFormat, [intTime]));
    end;
    with lviewInfo.Items.Add do
    begin
        Caption := TLanguageString.Read('Resource ID', 'Ressourcen-ID');
        SubItems.Add(xElementInfo.Res);
    end;
    with lviewInfo.Items.Add do
    begin
        Caption := TLanguageString.Read('Shared ID', 'Shared-ID');
        SubItems.Add(IntToStr(xElementInfo.SharedID));
    end;

    if aUpdateParallelList then
    begin
        lviewSynch.Items.Clear;
        edSynchWatchID.Text := IntToStr(aElementID);
        intSynchCount := FMainSchedChart.StoreSynchElements(aElementID);
        for i := 0 to intSynchCount - 1 do
        begin
            intChartID := FMainSchedChart.GetSynchElementChartID(i);
            xSynchElementInfo := FMainSchedChart.GetElementInfo(intChartID, false);

            AddSyncElement(xSynchElementInfo.LevelID, xSynchElementInfo.Text, xSynchElementInfo.Res,
                xSynchElementInfo.SharedID);
        end;
    end;
end;

// --------------------------------------------------------------------------------------------------
procedure TfrmScheduleChart.ShowNeighborElementInfo(intDirection: integer);
// --------------------------------------------------------------------------------------------------
var

    intNeighborID, intSelectedID: integer;
begin
    intSelectedID := FMainSchedChart.GetSelectedElementChartID();
    if intSelectedID = -1 then
        Exit;
    intNeighborID := FMainSchedChart.NeighborChartID(intSelectedID, intDirection);
    if intNeighborID > -1 then
        SelectElement(intNeighborID, true);
end;

function TfrmScheduleChart.GetSelectedSyncListIndex(): integer;
begin
    result := -1;
    if not Assigned(lviewSynch.Selected) then
        EXIT;
    result := lviewSynch.Selected.Index;
end;

// --------------------------------------------------------------------------------------------------
procedure TfrmScheduleChart.OnChartClickSeries(Sender: TCustomChart; Series: TChartSeries;
    ValueIndex: Integer; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
// --------------------------------------------------------------------------------------------------

begin
    if Button <> mbLeft then
        EXIT;
    SelectElement(ValueIndex, true);
end;

procedure TfrmScheduleChart.EditElement(aChartID: integer);
begin
end;

// --------------------------------------------------------------------------------------------------
procedure TfrmScheduleChart.btnEditElementClick(Sender: TObject);
// --------------------------------------------------------------------------------------------------
var
    intSelectedID: integer;
begin
    intSelectedID := FMainSchedChart.GetSelectedElementChartID;
    if intSelectedID = -1 then
        Exit;
    EditElement(intSelectedID);

    // TCommonLinkAll.ListenOpenUFuncSelectForm( fMainSchedChart.DatasourceName,FMainSchedChart.GetElementData(intSelectedID));
end;

// --------------------------------------------------------------------------------------------------
procedure TfrmScheduleChart.FormKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
// --------------------------------------------------------------------------------------------------
begin
    if ssAlt in Shift then
    begin
        case Key of
            37:
                ShowNeighborElementInfo(-1);
            39:
                ShowNeighborElementInfo(1);
        end;
    end;
end;

// --------------------------------------------------------------------------------------------------
procedure TfrmScheduleChart.btnShowNextClick(Sender: TObject);
// --------------------------------------------------------------------------------------------------
begin
    ShowNeighborElementInfo(1);
end;

// --------------------------------------------------------------------------------------------------
procedure TfrmScheduleChart.btnShowPrevClick(Sender: TObject);
// --------------------------------------------------------------------------------------------------
begin
    ShowNeighborElementInfo(-1);
end;

// --------------------------------------------------------------------------------------------------
procedure TfrmScheduleChart.edScriptNameEnter(Sender: TObject);
// --------------------------------------------------------------------------------------------------
begin
    pnlTop.SetFocus;
end;

// --------------------------------------------------------------------------------------------------
procedure TfrmScheduleChart.Zoom();
// --------------------------------------------------------------------------------------------------
begin
    FMainSchedChart.Zoom(100);
end;

// --------------------------------------------------------------------------------------------------
procedure TfrmScheduleChart.Exit1Click(Sender: TObject);
// --------------------------------------------------------------------------------------------------
begin
    self.Hide;
end;

// --------------------------------------------------------------------------------------------------
procedure TfrmScheduleChart.SyncListSelectedChanged();
// --------------------------------------------------------------------------------------------------
var
    xIndex: integer;
    xSelectedID, xSyncSelectedID: integer;
begin
    xIndex := GetSelectedSyncListIndex();
    if xIndex < 0 then
        EXIT;
    xSelectedID := fMainSchedChart.GetSelectedElementChartID();
    xSyncSelectedID := FMainSchedChart.GetSynchElementChartID(xIndex);
    if xSelectedID = xSyncSelectedID then
        EXIT;
    SelectElement(xSyncSelectedID, false);
end;

// --------------------------------------------------------------------------------------------------
procedure TfrmScheduleChart.lviewSynchKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
// --------------------------------------------------------------------------------------------------

begin
    case Key of
        38, 40:
            SyncListSelectedChanged();
    end;
end;

// --------------------------------------------------------------------------------------------------
procedure TfrmScheduleChart.chkView3DClick(Sender: TObject);
// --------------------------------------------------------------------------------------------------
begin
    FMainSchedChart.View3D := chkView3D.Checked;
end;

// --------------------------------------------------------------------------------------------------
procedure TfrmScheduleChart.mniBottomClick(Sender: TObject);
// --------------------------------------------------------------------------------------------------
begin
    ViewBottomChange(not mniBottom.Checked);
end;

// --------------------------------------------------------------------------------------------------
procedure TfrmScheduleChart.mniTopClick(Sender: TObject);
// --------------------------------------------------------------------------------------------------
begin
    ViewTopChange(not mniTop.Checked);
end;

// --------------------------------------------------------------------------------------------------
procedure TfrmScheduleChart.RefreshSessionName();
// --------------------------------------------------------------------------------------------------
begin
    // self.Caption :=  Format( '%s - %s', [ PosTools.TResLoader.GetResString( 59810 ), fMainSchedChart.DatasourceName ] );
    stName.Caption := fMainSchedChart.Session.SessionName;
end;

// --------------------------------------------------------------------------------------------------
procedure TfrmScheduleChart.MenuChangeDrawMode(aMenuItem: TMenuItem; aDrawMode: TSchedChartDrawMode);
// --------------------------------------------------------------------------------------------------
begin
    with aMenuItem do
    begin
        if Checked then
            EXIT;
        Checked := true;
    end;
    SetDrawMode(aDrawMode);
    RedrawActiveDatasource();
    if fMainSchedChart.SelectedID > -1 then
        ShowElementInfo(fMainSchedChart.SelectedID, true);
end;

// --------------------------------------------------------------------------------------------------
procedure TfrmScheduleChart.mniLiveModeClick(Sender: TObject);
// --------------------------------------------------------------------------------------------------
begin
    MenuChangeDrawMode(Sender as TMenuItem, dmLive);
end;

// --------------------------------------------------------------------------------------------------
procedure TfrmScheduleChart.mniInfoModeClick(Sender: TObject);
// --------------------------------------------------------------------------------------------------
begin
    MenuChangeDrawMode(Sender as TMenuItem, dmLiveInfo);
end;

// --------------------------------------------------------------------------------------------------
procedure TfrmScheduleChart.mniDesignModeClick(Sender: TObject);
// --------------------------------------------------------------------------------------------------
begin
    MenuChangeDrawMode(Sender as TMenuItem, dmDesign);
end;

// --------------------------------------------------------------------------------------------------
procedure TfrmScheduleChart.DoOnMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState;
    X, Y: Integer);
// --------------------------------------------------------------------------------------------------
begin
    self.PopupMenu1.AutoPopup := not((ssShift in Shift) and (ssRight in Shift));
end;

// --------------------------------------------------------------------------------------------------
procedure TfrmScheduleChart.mniPrintClick(Sender: TObject);
// --------------------------------------------------------------------------------------------------
begin
    fMainSchedChart.Print();
end;

// --------------------------------------------------------------------------------------------------
procedure TfrmScheduleChart.SetTimeMode(aTimeMode: TMainSchedChartTimeMode);
// --------------------------------------------------------------------------------------------------
begin
    fMainSchedChart.TimeMode := aTimeMode;
end;

// --------------------------------------------------------------------------------------------------
function TfrmScheduleChart.GetTimeMode(): TMainSchedChartTimeMode;
// --------------------------------------------------------------------------------------------------
begin
    result := fMainSchedChart.TimeMode;
end;

// --------------------------------------------------------------------------------------------------
procedure TfrmScheduleChart.edSynchWatchChange(Sender: TObject);
// --------------------------------------------------------------------------------------------------
var
    intID: integer;
begin
    intID := StrToInt(edSynchWatchID.Text);
    if intID = -1 then
        Exit;
    SelectElement(intID, false);
end;

// --------------------------------------------------------------------------------------------------
procedure TfrmScheduleChart.SchedChartOptionsFromGUI(var aRec: TSchedChartFormOptionsRec);
// --------------------------------------------------------------------------------------------------
begin
    aRec.AbsTimeMode := fMainSchedChart.TimeMode = mtmAbs;

    aRec.AbsTimeDefined := aRec.AbsTimeMode and fMainSchedChart.AbsTimeDefined;
    if aRec.AbsTimeDefined then
        aRec.AbsTime := fMainSchedChart.AbsoluteStartDate;
end;

// --------------------------------------------------------------------------------------------------
procedure TfrmScheduleChart.SchedChartOptionsToGUI(const aRec: TSchedChartFormOptionsRec);
// --------------------------------------------------------------------------------------------------
begin
    if not aRec.AbsTimeMode then
    begin
        fMainSchedChart.TimeMode := mtmRel;
        EXIT;
    end;

    fMainSchedChart.TimeMode := mtmabs;

    fMainSchedChart.AbsTimeDefined := aRec.AbsTimeDefined;

    if not aRec.AbsTimeDefined then
        EXIT;

    fMainSchedChart.AbsoluteStartDate := aRec.AbsTime;

end;

// --------------------------------------------------------------------------------------------------
procedure TfrmScheduleChart.mniOptionsClick(Sender: TObject);
// --------------------------------------------------------------------------------------------------
var
    xRec: TSchedChartFormOptionsRec;
begin
    SchedChartOptionsFromGUI(xRec);
    if TFrmScheduleChartOptions.ModalOpen(xRec) <> mrOK then
        EXIT;
    SchedChartOptionsToGUI(xRec);
    RedrawActiveDatasource();
end;

procedure TfrmScheduleChart.lviewSynchClick(Sender: TObject);
begin
    SyncListSelectedChanged();
end;


end.
