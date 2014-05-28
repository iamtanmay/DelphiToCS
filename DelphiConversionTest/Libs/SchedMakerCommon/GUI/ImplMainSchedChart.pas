unit ImplMainSchedChart;
{ --------------------------------------------------------------------------------------------------
  Main unit for ScheduleChart
  --------------------------------------------------------------------------------------------------
  Datum    op  function/procedure     track-no   Änderung / Neuerung
  -------- --  -------------------    --------   --------------------------------------------------------------
  14.06.02 pk  Created
  25.06.02 pk  Destructor                        override directive added
  14.07.02 pk  Destroy                           Free Adapter and Chart objects
  08.08.02 pk                                    OnChartClickSeries,  OnChartMouseMove properties added
  02.09.02 pk  Zoom                              aBlnIsPixelPos argument added
  02.09.02 pk  View3D                            New property.
  08.12.03 pk  SchedChart                        New property
  21.07.04 pk  GetElementInfo          TN2049    New
  18.01.05 pk                          TN2281    fDrawMode, fTimeMode, fAbsoluteStartDate, fAbsTimeDefined
  27.01.05 pk  GetElementColor         TN2281.3  Uses state flag to determine color
  27.01.05 pk  DoDrawChart             TN2281.3  GetBarTimeInSecs required new boolean arg
  02.03.05 pk                          TN2328    TMainSchedChart based on TSchedMakerSession instead of data adaptor
  29.03.05 pk  AddProcess              TN2365    use Element.Description instead of Element.Action
  19.04.05 pk  DoDrawChart             TN2389    Call RefreshVisualSelect to Re-color the selected element after recreateing elements
  19.04.05 pk                          TN2389    Chart Element DataID (variant) field changed to Data ( TObject ) field
  21.04.05 pk  GetElementInfo          TN2395    Return ResID
  11.07.05 pk                          TN2737    Dynamic Scheduling - Various changes
  14.11.05 pk                          TN2758    Action State type changed from integer to TExecActionState
  18.04.06 pk  GetElementInfo          TN3048    Show shared id and resources
  18.04.06 pk  GetElementColor         TN3048    Show color
  06.06.06 pk  StoreSynchElements      TN3131    Add given ID as first element of list
  29.01.07 pk  FindMaxSchedEnd         TN3527    Use SchedSession.SessionDuration
  29.01.07 pk  GetBarTimeInSecs        TN3527    Changed to show mintime instead of maxtime for actions that have not been completed
  29.01.07 pk  RedrawActiveElements    TN3527    called in redrawarrow, to change the finishvalue of all actions that are running
  03.08.07 wl                          TN3811.2 uses IntMap statt ObjStructures
  07.08.07 wl                          TN3811.3  TPosinfoDataAdaptor.Create statt Instance()
  09.01.08 wl                          TN3972    uses RessourceLoader
  10.08.09 wl                          TN4702    Strings werden jetzt direkt geladen
  04.11.09 pk                          TN4843    Replace TObjectList, TStringList, TStrings with Generics Lists and TStringArray
  10.03.11 wl                          TN5499   an geänderten ResSchemeDataAdaptor angepasst
  -------------------------------------------------------------------------------------------------- }


interface


uses
    Classes,
    controls,
    Graphics,
    Chart,
    ListClasses,
    Apptypes,
    ObjSchedChart,
    SchedMakerClasses,
    SchedMakerTypes;

const
    CLR_FINISHED_ACTION: TColor = clGray;
    CLR_WORKING_ACTION: TColor = clBlack;

type
    TElementInfoRec = record
        LevelID: integer;
        Text: string;
        StartTime: integer;
        EndTime: integer;
        MinTime: integer;
        MaxTime: integer;
        Res: string;
        SharedID: integer;
    end;

    TSchedChartDrawMode = (dmDesign, dmLive, dmLiveInfo, dmPrint);
    TMainSchedChartTimeMode = (mtmRel, mtmAbs);

    TMainSchedChart = class
    protected
        FSchedChart: TSchedChart;
        // FSchedDataAdaptor: TSchedChartDataAdaptor;
        FSynchList: TStringValueList;
        fDrawMode: TSchedChartDrawMode;
        fTimeMode: TMainSchedChartTimeMode;
        fAbsoluteStartDate: TDateTime;
        fAbsTimeDefined: boolean;
        fStartDate: TDateTime;
        fSession: TSchedMakerSession;
        fActiveElements: TIntegerKeyObjectValueList;

        procedure SetSession(aSession: TSchedMakerSession);

        procedure SetOnChartClickSeries(aEventHandler: TChartClickSeries);
        procedure SetOnChartMouseMove(aEventHandler: TMouseMoveEvent);
        function DoDrawChart(aDrawMode: TSchedChartDrawMode; aElapsedTime: cardinal = 0): string;

        function GetSelectedID(): integer;
        function GetElementColor(aMode: TSchedChartDrawMode; aColor: TColor;
            aState: TExecActionState): TColor;
        function FindMaxSchedEnd(aIsLive: boolean): double;
        procedure DoOnHorizontalAxisTitleEvent(aScale: TChartTimeScale; var aTitle: string);
        procedure SetTimeMode(aTimeMode: TMainSchedChartTimeMode);
        function GetTimeMode(): TMainSchedChartTimeMode;
        procedure SetAbsoluteStartDate(aAbsoluteStartDate: TDateTime);
        function GetAbsoluteStartDate(): TDateTime;

        procedure AddProcess(aSchedMakerProcess: TSchedMakerProcess; aDrawMode: TSchedChartDrawMode;
            aElapsedTime: cardinal; aLive: boolean);
        function ConvertClockTimeToSecs(aTime: cardinal): cardinal;
        function GetBarTimeInSecs(aIsStartTime: boolean; aLive: boolean; aElapsedTime: cardinal;
            aSchedSessionElement: TSchedMakerExecAction): integer;
        function GetSessionElementFromChartElement(aElement: TChartElement): TSchedMakerExecAction;

        procedure RedrawActiveElements(aElapsedTime: cardinal; aLive: boolean);
    public
        constructor Create(aSession: TSchedMakerSession);
        destructor Destroy; override;

        procedure DrawChart(aOwner: TComponent; aMode: TSchedChartDrawMode; aElapsedTime: cardinal = 0);
        procedure RedrawChart(aMode: TSchedChartDrawMode; aElapsedTime: cardinal = 0);
        procedure RedrawArrow(aElapsedTime: cardinal; aLive: boolean);

        function StoreSynchElements(aIntChartID: integer): integer;
        function GetSynchElementChartID(aIntIndex: integer): integer;

        procedure Zoom(aFactor: integer);
        procedure UndoZoom;

        function GetElementByChartID(aIntIndex: integer): TChartElement;
        function GetElementData(aIntIndex: integer): TObject;
        function GetResourceInfo(aResID: integer): string;
        function GetElementInfo(aIntIndex: integer; aReadMinMax: boolean): TElementInfoRec;
        procedure SetElementMinTime(aIntIndex: integer; aIntTime: integer);
        procedure SetElementMaxTime(aIntIndex: integer; aIntTime: integer);

        function VisualSelectElement(aIntSelectedNew: integer): integer;
        function GetSelectedElementChartID: integer;
        function NeighborChartID(aIntChartID: integer; aIntIDOffset: integer): integer;

        procedure SetCursor(aCursor: TCursor);
        function GetView3D: boolean;
        procedure SetView3D(aBlnVal: boolean);
        procedure Print();
        procedure DoOnGetElementMarkText(aValueIndex: LongInt; var aMarkText: string);

        // property    DataAdapter : TSchedChartDataAdaptor read FSchedDataAdaptor;
        property OnChartClickSeries: TChartClickSeries write SetOnChartClickSeries;
        property OnChartMouseMove: TMouseMoveEvent write SetOnChartMouseMove;
        property View3D: boolean read GetView3D write SetView3D;
        property SchedChart: TSchedChart read FSchedChart;
        property DrawMode: TSchedChartDrawMode read fDrawMode write fDrawMode;
        property TimeMode: TMainSchedChartTimeMode read GetTimeMode write SetTimeMode;
        property AbsoluteStartDate: TDateTime read GetAbsoluteStartDate write SetAbsoluteStartDate;
        property AbsTimeDefined: boolean read fAbsTimeDefined write fAbsTimeDefined;
        property StartDate: TDateTime read fStartDate write fStartDate;
        property SelectedID: integer read GetSelectedID;
        property Session: TSchedMakerSession read fSession write SetSession;
    end;


implementation


uses
    Sysutils,
    ResSchemeDataAdaptor,
    GeneralTypes;

// ------------------------------------------------------------------------------------------------------------
constructor TMainSchedChart.Create(aSession: TSchedMakerSession);
// ------------------------------------------------------------------------------------------------------------
begin
    inherited Create;
    self.Session := aSession;
    FSynchList := TStringValueList.Create;
    fActiveElements := TIntegerKeyObjectValueList.Create;
end;

// ------------------------------------------------------------------------------------------------------------
destructor TMainSchedChart.Destroy;
// ------------------------------------------------------------------------------------------------------------
begin
    // FSchedDataAdaptor.Free;
    fActiveElements.Free;
    FSchedChart.Free;
    FSynchList.Free;
    inherited Destroy;
end;

procedure TMainSchedChart.SetSession(aSession: TSchedMakerSession);
begin
    fSession := aSession;

end;

// ------------------------------------------------------------------------------------------------------------
function TMainSchedChart.ConvertClockTimeToSecs(aTime: cardinal): cardinal;
// ------------------------------------------------------------------------------------------------------------
begin
    result := Round(aTime / 100);
end;

// ------------------------------------------------------------------------------------------------------------
function TMainSchedChart.GetBarTimeInSecs(aIsStartTime: boolean; aLive: boolean; aElapsedTime: cardinal;
    aSchedSessionElement: TSchedMakerExecAction): integer;
// ------------------------------------------------------------------------------------------------------------
var
    xTime: cardinal;
begin
    if aIsStartTime then
    begin
        if aLive then
        begin
            case aSchedSessionElement.StateFlag of
                easClear:
                    result := ConvertClockTimeToSecs(aSchedSessionElement.SchedStart);
                easStarted, easFinished:
                    result := ConvertClockTimeToSecs(aSchedSessionElement.RealStart);
                else
                    result := 0;
            end;
        end
        else
        begin
            result := ConvertClockTimeToSecs(aSchedSessionElement.SchedStart);
        end;
    end
    else
    begin
        if aLive then
        begin
            // show start + mintime as finishvalue.  If elapsed time is greater than this value show elapsedtime as finishvalue
            xTime := aSchedSessionElement.SchedStart + aSchedSessionElement.MinTime;
            case aSchedSessionElement.StateFlag of
                easClear:
                    result := ConvertClockTimeToSecs(xTime);
                easStarted:
                    begin
                        if aElapsedTime > xTime then
                            xTime := aElapsedTime;
                        result := ConvertClockTimeToSecs(xTime);
                    end;
                easFinished:
                    result := ConvertClockTimeToSecs(aSchedSessionElement.RealEnd);
                else
                    result := 0;
            end;
        end
        else
        begin
            result := ConvertClockTimeToSecs(aSchedSessionElement.SchedEnd);
        end;
    end;
end;

// ------------------------------------------------------------------------------------------------------------
function TMainSchedChart.GetElementColor(aMode: TSchedChartDrawMode; aColor: TColor;
    aState: TExecActionState): TColor;
// ------------------------------------------------------------------------------------------------------------
begin
    result := aColor;

    if aMode in [dmLive, dmLiveInfo] then
    begin

        if (aState = easFinished) then
            result := CLR_FINISHED_ACTION
        else if (aState = easStarted) then
            result := CLR_WORKING_ACTION;
    end
    else if aMode = dmPrint then
    begin
        result := clWhite;
        EXIT;
    end;
    if result <> clNone then
        EXIT;
    result := clBlue;
end;

// ------------------------------------------------------------------------------------------------------------
function TMainSchedChart.FindMaxSchedEnd(aIsLive: boolean): double;
// ------------------------------------------------------------------------------------------------------------
begin
    result := ConvertClockTimeToSecs(fSession.SchedSession.GetSessionDuration());
end;

procedure TMainSchedChart.RedrawActiveElements(aElapsedTime: cardinal; aLive: boolean);
var
    x: integer;
    xChartID: integer;
    xElement: TSchedMakerExecAction;
    xChartElement: TChartElement;
begin
    for x := 0 to fActiveElements.Count - 1 do
    begin
        xChartID := fActiveElements[x];
        xChartElement := FSchedChart[xChartID];
        xElement := GetSessionElementFromChartElement(xChartElement);
        xChartElement.FinishValue := GetBarTimeInSecs(false, aLive, aElapsedTime, xElement);;
    end;
end;

procedure TMainSchedChart.RedrawArrow(aElapsedTime: cardinal; aLive: boolean);
begin
    fSchedChart.ReDrawArrowSeries(ConvertClockTimeToSecs(aElapsedTime), aLive);
    RedrawActiveElements(aElapsedTime, aLive);
end;

procedure TMainSchedChart.AddProcess(aSchedMakerProcess: TSchedMakerProcess; aDrawMode: TSchedChartDrawMode;
    aElapsedTime: cardinal; aLive: boolean);
var
    i: integer;
    clrBarColor: TColor;
    xElement: TSchedMakerExecAction;
    xSchedStart, xSchedEnd: double;
    xChartID: integer;
begin

    for i := 0 to aSchedMakerProcess.Count - 1 do
    begin
        xElement := (aSchedMakerProcess[i] as TSchedMakerAction).ExecAction;
        clrBarColor := self.GetElementColor(aDrawMode, xElement.Color, xElement.StateFlag);

        xSchedStart := GetBarTimeInSecs(true, aLive, aElapsedTime, xElement);
        xSchedEnd := GetBarTimeInSecs(false, aLive, aElapsedTime, xElement);

        xChartID := FSchedChart.AddBar(xElement, aSchedMakerProcess.ProcessID, xSchedStart, xSchedEnd,
            xElement.Description, clrBarColor);
        if aLive and (xElement.StateFlag = easStarted) then
            fActiveElements.Add(xChartID);

        if ((aDrawMode in [dmLive]) and (xElement.StateFlag = easStarted)) or (aDrawMode = dmPrint) then
        begin
            FSchedChart.MarkTextList.Add(IntToStr(xChartID));
        end;
    end;
end;

// ------------------------------------------------------------------------------------------------------------
function TMainSchedChart.DoDrawChart(aDrawMode: TSchedChartDrawMode; aElapsedTime: cardinal = 0): string;
// ------------------------------------------------------------------------------------------------------------
var
    i: integer;
    xMaxSchedEnd: double;
    xLive: boolean;

begin
    try
        if (TimeMode = mtmAbs) and (not AbsTimeDefined) then
            AbsoluteStartDate := fStartDate;

        FSchedChart.MarkTextList.Clear;
        xLive := aDrawMode in [dmLive, dmLiveInfo];

        xMaxSchedEnd := FindMaxSchedEnd(xLive);
        fSchedChart.SetAutoTimeScale(xMaxSchedEnd);
        fActiveElements.Clear;
        for i := 0 to fSession.Count - 1 do
        begin
            AddProcess(fSession[i], aDrawMode, aElapsedTime, xLive);
        end;

        if xLive then
        begin
            RedrawArrow(aElapsedTime, xLive);
        end;
        FSchedChart.ReZoom();

        fSchedChart.RefreshVisualSelect();

    except
        on E: Exception do
            raise Exception.Create('TMainSchedChart.DrawChart -> Error');
    end;
end;

// ------------------------------------------------------------------------------------------------------------
procedure TMainSchedChart.DoOnHorizontalAxisTitleEvent(aScale: TChartTimeScale; var aTitle: string);
// ------------------------------------------------------------------------------------------------------------
var
    xCaption: string;
begin
    case aScale of
        tsSecs:
            xCaption := TLanguageString.Read('Seconds', 'Sekunden');
        tsMins:
            xCaption := TLanguageString.Read('Minutes', 'Minuten');
        tsHours:
            xCaption := TLanguageString.Read('Hours', 'Stunden');
        tsDays:
            xCaption := TLanguageString.Read('Days', 'Tage');
    end;
    aTitle := TLanguageString.Read('Time ({0})', 'Zeit ({0})', [xCaption]);
end;

// ------------------------------------------------------------------------------------------------------------
procedure TMainSchedChart.RedrawChart(aMode: TSchedChartDrawMode; aElapsedTime: cardinal = 0);
// ------------------------------------------------------------------------------------------------------------
begin
    if fSchedChart = nil then
        EXIT;
    FSchedChart.ClearChart;
    DoDrawChart(aMode, aElapsedTime);
end;

// ------------------------------------------------------------------------------------------------------------
procedure TMainSchedChart.DrawChart(aOwner: TComponent; aMode: TSchedChartDrawMode;
    aElapsedTime: cardinal = 0);
// ------------------------------------------------------------------------------------------------------------
begin
    // if fDatasourceName <> aDatasourceName then begin
    // fDatasourceName := aDatasourceName;
    // DoOnDatasourceNameChanged();
    // end;

    if FSchedChart = nil then
    begin
        FSchedChart := TSchedChart.Create(aOwner);
        fSchedChart.OnHorizontalAxisTitleEvent := DoOnHorizontalAxisTitleEvent;
        fSchedChart.OnGetElementMarkText := DoOnGetElementMarkText;
        fSchedChart.VerticalAxisTitle := TLanguageString.Read('Processes', 'Prozesse');
        SetTimeMode(fTimeMode);
    end
    else
        FSchedChart.ClearChart;

    DoDrawChart(aMode, aElapsedTime);
end;

// ------------------------------------------------------------------------------------------------------------
function TMainSchedChart.StoreSynchElements(aIntChartID: integer): integer;
// ------------------------------------------------------------------------------------------------------------
const
    STR_ID_FORMAT = '%.4d';
begin
    FSynchList.Clear;
    fSynchList.Add(IntToStr(aIntChartID));
    FSchedChart.GetSynchElements(FSynchList, aIntChartID, STR_ID_FORMAT);
    result := fSynchList.Count;
end;

// ------------------------------------------------------------------------------------------------------------
function TMainSchedChart.GetSynchElementChartID(aIntIndex: integer): integer;
// ------------------------------------------------------------------------------------------------------------
begin
    result := StrToInt(FSynchList[aIntIndex]);
end;

// ------------------------------------------------------------------------------------------------------------
procedure TMainSchedChart.Zoom(aFactor: integer);
// ------------------------------------------------------------------------------------------------------------
begin
    // FSchedChart.RelativeZoom(intX, intFactor, aBlnIsPixelPos);
    FSchedChart.Zoom(aFactor);
    FSchedChart.Resize();
end;

// ------------------------------------------------------------------------------------------------------------
procedure TMainSchedChart.UndoZoom;
// ------------------------------------------------------------------------------------------------------------
begin
    // FSchedChart
    // FSchedChart.AbsoluteZoom( 100 );
end;

// ------------------------------------------------------------------------------------------------------------
function TMainSchedChart.GetElementByChartID(aIntIndex: integer): TChartElement;
// ------------------------------------------------------------------------------------------------------------
begin
    result := FSchedChart[aIntIndex];
end;

// ------------------------------------------------------------------------------------------------------------
function TMainSchedChart.GetElementData(aIntIndex: integer): TObject;
// ------------------------------------------------------------------------------------------------------------
begin
    result := FSchedChart[aIntIndex].Data;
end;

// ------------------------------------------------------------------------------------------------------------
function TMainSchedChart.GetSessionElementFromChartElement(aElement: TChartElement): TSchedMakerExecAction;
// ------------------------------------------------------------------------------------------------------------
begin
    result := aElement.Data as TSchedMakerExecAction;
end;

function TMainSchedChart.GetResourceInfo(aResID: integer): string;
var
    xRecs: TArray<TResSchemeRec>;
    i: integer;
    xDelim: string;
    xDA: TResSchemeDataAdaptor;
begin
    result := IntToStr(aResID) + ':';
    xDelim := '';

    xDA := TResSchemeDataAdaptor.Create;
    try
        xRecs := xDA.ReadSchemeByID(aResID);
    finally
        xDA.Free;
    end;

    for i := 0 to high(xRecs) do
    begin
        if i = 1 then
            xDelim := ',';
        result := result + xDelim + xRecs[i].ResID;
    end;
end;

// ------------------------------------------------------------------------------------------------------------
function TMainSchedChart.GetElementInfo(aIntIndex: integer; aReadMinMax: boolean): TElementInfoRec;
// ------------------------------------------------------------------------------------------------------------
var
    xElement: TChartElement;
    xSessionElement: TSchedMakerExecAction;
begin
    xElement := FSchedChart[aIntIndex];
    xSessionElement := GetSessionElementFromChartElement(xElement);
    if aReadMinMax then
    begin
        result.MinTime := ConvertClockTimeToSecs(xSessionElement.MinTime);
        result.MaxTime := ConvertClockTimeToSecs(xSessionElement.MaxTime);
    end;
    with result do
    begin
        LevelID := xElement.LevelID;
        Text := xElement.Text;
        StartTime := Trunc(xElement.StartValue);
        EndTime := Trunc(xElement.FinishValue);
        Res := GetResourceInfo(xSessionElement.ResID);
        SharedID := xSessionElement.SharedID;
    end;
end;

procedure TMainSchedChart.Print();
begin
    RedrawChart(dmPrint);
    try
        fSchedChart.Print;
    finally
        RedrawChart(fDrawMode);
    end;
end;

// ------------------------------------------------------------------------------------------------------------
procedure TMainSchedChart.SetElementMinTime(aIntIndex: integer; aIntTime: integer);
// ------------------------------------------------------------------------------------------------------------
begin
    // FSchedDataAdaptor.SetElementMinTime( FSchedChart[aIntIndex].DataID, aIntTime);
end;

// ------------------------------------------------------------------------------------------------------------
procedure TMainSchedChart.SetElementMaxTime(aIntIndex: integer; aIntTime: integer);
// ------------------------------------------------------------------------------------------------------------
begin
    // FSchedDataAdaptor.SetElementMaxTime( FSchedChart[aIntIndex].DataID, aIntTime);
end;

// ------------------------------------------------------------------------------------------------------------
function TMainSchedChart.VisualSelectElement(aIntSelectedNew: integer): integer;
// ------------------------------------------------------------------------------------------------------------
begin
    result := FSchedChart.VisualSelectElement(aIntSelectedNew);
end;

// ------------------------------------------------------------------------------------------------------------
function TMainSchedChart.GetSelectedElementChartID: integer;
// ------------------------------------------------------------------------------------------------------------
begin
    result := FSchedChart.GetSelectedElementChartID();
end;

// ------------------------------------------------------------------------------------------------------------
function TMainSchedChart.NeighborChartID(aIntChartID: integer; aIntIDOffset: integer): integer;
// ------------------------------------------------------------------------------------------------------------
begin
    result := FSchedChart.NeighborChartID(aIntChartID, aIntIDOffset)
end;

// ------------------------------------------------------------------------------------------------------------
procedure TMainSchedChart.SetOnChartClickSeries(aEventHandler: TChartClickSeries);
// ------------------------------------------------------------------------------------------------------------
begin
    FSchedChart.OnClickSeries := aEventHandler;
end;

// ------------------------------------------------------------------------------------------------------------
procedure TMainSchedChart.SetOnChartMouseMove(aEventHandler: TMouseMoveEvent);
// ------------------------------------------------------------------------------------------------------------
begin
    FSchedChart.OnMouseMove := aEventHandler;
end;

// ------------------------------------------------------------------------------------------------------------
procedure TMainSchedChart.SetCursor(aCursor: TCursor);
// ------------------------------------------------------------------------------------------------------------
begin
    FSchedChart.Cursor := aCursor;
end;

function TMainSchedChart.GetView3D: boolean;
begin
    result := FSchedChart.View3D;
end;

procedure TMainSchedChart.SetView3D(aBlnVal: boolean);
begin
    FSchedChart.View3D := aBlnVal;
end;

procedure TMainSchedChart.SetTimeMode(aTimeMode: TMainSchedChartTimeMode);
begin
    fTimeMode := aTimeMode;
    if Assigned(fSchedChart) then
        FSchedChart.TimeMode := TChartTimeMode(fTimeMode);
end;

function TMainSchedChart.GetTimeMode(): TMainSchedChartTimeMode;
begin
    if Assigned(fSchedChart) then
        fTimeMode := TMainSchedChartTimeMode(fSchedChart.TimeMode);

    result := fTimeMode;
    // result := TMainSchedChartTimeMode( fSchedChart.TimeMode );
end;

procedure TMainSchedChart.SetAbsoluteStartDate(aAbsoluteStartDate: TDateTime);
begin
    fAbsoluteStartDate := aAbsoluteStartDate;

    if Assigned(fSchedChart) then
        FSchedChart.AbsoluteStartDate := fAbsoluteStartDate;

end;

function TMainSchedChart.GetAbsoluteStartDate(): TDateTime;
begin
    if Assigned(fSchedChart) then
        fAbsoluteStartDate := fSchedChart.AbsoluteStartDate;

    result := fAbsoluteStartDate;
end;

procedure TMainSchedChart.DoOnGetElementMarkText(aValueIndex: LongInt; var aMarkText: string);
var
    xElementInfo: TElementInfoRec;
begin
    xElementInfo := GetElementInfo(aValueIndex, false);
    aMarkText := Format('%s'#$D'%s'#$D'%s',
        [xElementInfo.Text, fSchedChart.GetValueLabel(xElementInfo.StartTime),
        fSchedChart.GetValueLabel(xElementInfo.EndTime)])

end;

function TMainSchedChart.GetSelectedID(): integer;
begin
    result := fSchedChart.SelectedID;
end;


end.
