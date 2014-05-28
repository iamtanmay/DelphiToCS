unit ObjSchedChart;


{ --------------------------------------------------------------------------------------------------
  TSchedChart, TChartElement, TChartElements, TChartElementLists objects for Scheduler Chart
  --------------------------------------------------------------------------------------------------
  date     op  method                            track-no    improvement/change
  -------- --  --------------------------------- --------    -----------------------------------------------
  14.06.02 pk                                                Created
  25.06.02 pk                                                Destructor override directive added to all objects
  02.09.02 pk                                                Integer values changed to double to increase accuracy
  02.09.02 pk                                                Use of INT_FACTOR to switch between Milliseconds and Seconds
  02.09.02 pk                                                MarkTextList added to increase flexibility of turning marktext on and off at runtime
  02.09.02 pk                                                View 3D property added
  08.08.03 pk                                                Documentation
  08.12.03 pk   AbsoluteZoom                     TN1697      New: Uses the percent given to do absolute zoom to that percent
  21.07.04 pk                                    TN2049      DataId changed from integer to variant
  21.07.04 pk   Zoom                             TN2049      Multiply by inverse of DblPercent
  18.01.05 pk                                    TN2281      fTimeMode, fAbsoluteStartDate
  18.01.05 pk                                    TN2281      New : fStartDate
  18.01.05 pk   SetHorizontalAxisTimeScale       TN2281      Also depends on TimeMode
  19.04.05 pk                                    TN2389      TChartElement DataID (variant) field changed to Data ( TObject ) field
  19.04.05 pk   RefreshVisualSelect              TN2389      New : Refresh the color of the selected element
  17.11.05 pk   TSchedChart.Create               TN2778      fHorizontalAxis.AxisValues format changed to '0' to avoid exception when
  using StrToFloat in  to convert #.##0,## to a float  in DoOnGetAxisLabel
  29.01.07 pk   SetFinishValue                   TN3527      New
  29.01.07 pk   fIsUserZoomed                    TN3526      Avoid rezooming when ReZoom function is called if the user has explicitly zoomed
  04.11.09 pk                              	TN4843   Replace TObjectList, TStringList, TStrings with Generics Lists and TStringArray
  14.12.10 wl  EncodeTo...                   TN5411   von UtilLib hierher (mit TimeSpan)
  --------------------------------------------------------------------------------------------------
}
interface


uses
    SysUtils,
    Classes,
    Graphics,
    Controls,
    ArrowCha,
    TeEngine,
    Series,
    GanttCh,
    ExtCtrls,
    TeeProcs,
    Chart,
    ListClasses;

const

    COLOR_ELEMENT_SELECTED: TColor = clWhite;
    INT_BOTTOMAXIS_MIN_INCREMENT = 5000;

    INT_SECS_PER_MIN = 60;
    INT_SECS_PER_HOUR = INT_SECS_PER_MIN * 60;
    INT_SECS_PER_DAY = INT_SECS_PER_HOUR * 24;

    CLR_ARROW_SERIES_1 = clRed;
    CLR_ARROW_SERIES_2 = clYellow;

    INT_MAXTIMEALLOWED_UNDEFINED = 0;

type

    TChartElement = class
    private

        FSeries: TGanttSeries;
        FChartID: integer;
        FData: TObject;
        FLevelID: integer;
        FListID: integer;
        FDirty: boolean;

        constructor Create(aSeries: TGanttSeries; aIntChartID: integer; aData: TObject;
            aIntLevelID: integer = 0; aIntListID: integer = 0);
        function GetStartValue: double;
        function GetFinishValue: double;
        procedure SetFinishValue(aValue: double);
        function GetValueColor: TColor;
        procedure SetValueColor(aColor: TColor);
        function GetText: string;

    public
        property Data: TObject read FData;
        property LevelID: integer read FLevelID;
        property ChartID: integer read FChartID;
        property Dirty: boolean read FDirty write FDirty;
        property Level: integer read FLevelID;
        property StartValue: double read GetStartValue;
        property FinishValue: double read GetFinishValue write SetFinishValue;
        property ValueColor: TColor read GetValueColor write SetValueColor;
        property Text: string read GetText;
    end;

    TChartElementList = class
    private
        FTrackList: TList;
        FSeries: TGanttSeries;
        FList: TList;
        FLevel: integer;

        function AddElement(aChartID: integer; aData: TObject): integer;
        procedure SetElement(aIntIndex: integer; aElement: TChartElement);
        function GetElement(aIntIndex: integer): TChartElement;
        function GetCount: integer;
        procedure DeleteAll();

    public
        constructor Create(aSeries: TGanttSeries; aTrackList: TList; aIntLevel: integer);
        destructor Destroy; override;

        property Elements[index: integer]: TChartElement read GetElement write SetElement;
        property Count: integer read GetCount;

    end;

    TChartElementLists = class
    private
        FSeries: TGanttSeries;
        FList: TStringList;
        FTrackList: TList;

        function AddList(aIntLevel: integer): integer;
        procedure SetElement(index: integer; ElementList: TChartElementList);
        function GetElement(index: integer): TChartElementList;
        function GetCount: integer;
        procedure DeleteAll();
    public
        constructor Create(aSeries: TGanttSeries; aTrackList: TList);
        destructor Destroy; override;

        function GetIndexByLevel(aIntLevel: integer): integer;
        property Count: integer read GetCount;
        property Elements[index: integer]: TChartElementList read GetElement write SetElement;
        property TrackList: TList read FTrackList;

    end;

    TChartTimeScale = (tsSecs, tsMins, tsHours, tsDays);
    TChartTimeMode = (tsRelative, tsAbsolute);

    THorizontalAxisTitleEvent = procedure(aScale: TChartTimeScale; var aTitle: string) of object;
    TGetElementMarkText = procedure(aValueIndex: Longint; var aMarkText: string) of object;

    TSchedChart = class // (TChart)
    private

        FChart: TChart;
        FLevels: integer;
        FGanttSeries: TGanttSeries;
        FArrowSeries: TArrowSeries;
        FElementLists: TChartElementLists; // TList;
        FTrackList: TList; // list of all chartelements created, regardless of which level
        FSelectedID: integer;
        FSelectedColor: TColor;
        FMarkTextList: TStringList;
        fZoomFactor: double;
        fIsUserZoomed: boolean; // if the user has manually zoomed, don't allow rezoom
        fTimeScale: TChartTimeScale;
        fTimeMode: TChartTimeMode;
        fAbsoluteStartDate: TDateTime;
        fArrowSeriesColor: TColor;
        fMaxTimeAllowed: double;
        fAllowScroll: boolean;
        fVerticalAxis: TChartAxis;
        fHorizontalAxis: TChartAxis;

        fOnMouseDownEvent: TMouseEvent;
        fOnMouseMoveEvent: TMouseMoveEvent;
        fOnHorizontalAxisTitleEvent: THorizontalAxisTitleEvent;
        fOnGetElementMarkText: TGetElementMarkText;

        function ElementByChartId(aChartID: integer): TChartElement;
        procedure SetOnClickSeries(aEventHandler: TChartClickSeries);
        procedure SeriesOnGetMarkText(Sender: TChartSeries; ValueIndex: Longint; var MarkText: string);
        procedure SetCursor(aCursor: TCursor);
        function GetCursor: TCursor;
        function GetView3D: boolean;
        procedure SetView3D(aBlnVal: boolean);
        procedure SetHorizontalAxisMinMax(aDblMin, aDblMax: double);
        procedure SetVerticalAxisMinMax(aDblMin, aDblMax: double);
        // function    ConvertPixelPosToAxisCoord(aChartAxis:TChartAxis; aDblPixelPos:double):double;

        function GetVerticalAxisTitle: string;
        procedure SetVerticalAxisTitle(aTitle: string);
        procedure CreateGanttSeries(var aGanttPointer: TGanttSeries);
        procedure CreateArrowSeries(var aArrowSeriesPointer: TArrowSeries);
        procedure SetTimeScaleCaption(aScale: TChartTimeScale);
        procedure SetHorizontalAxisTimeScale();
        function HorizontalAxisValueAllowed(aValue: double): boolean;
        function GetHorizAxisType(): THorizAxis;
        function GetVertAxisType(): TVertAxis;

        procedure DoOnGetAxisLabel(Sender: TChartAxis; Series: TChartSeries; ValueIndex: LongInt;
            var LabelText: string);
        procedure DoOnUndoZoom(aObject: TObject);
        procedure DoOnZoom(aObject: TObject);
        procedure DoOnAllowScroll(Sender: TChartAxis; var Amin, AMax: Double; var AllowScroll: Boolean);
        procedure DoOnChartResize(Sender: TObject);
        procedure DoOnMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
        procedure DoOnMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
        // procedure   DestroyGanttSeries(var aGanttPointer:TGanttSeries);
    public
        constructor Create(aOwner: TComponent);
        destructor Destroy; override;

        function AddBar(aData: TObject; aIntLevel: integer; aDblStart, aDblFinish: double;
            aStrLabel: string = ''; aClrColor: TColor = clNone): integer;
        procedure ReDrawArrowSeries(aDblStartVal: double; aFlipColor: boolean = false);
        function ChartIDByLocation(aIntLevelID, aIntIndex: integer): integer;
        function NeighborChartID(aIntChartID: integer; aIntIDOffset: integer): integer;
        procedure AbsoluteZoom(aPercent: double);
        procedure Zoom(aPercent: double);
        procedure ReZoom();
        procedure Resize();

        function VisualSelectElement(aIntSelectedNew: integer): integer;
        procedure RefreshVisualSelect();
        procedure SelectElement(aSelectChartID: integer);
        function GetSelectedElementChartID: integer;
        procedure GetSynchElements(lstResult: TStringValueList; aIntChartID: integer; aIDFormat: string);
        // procedure   GetDirty(resultList:TStringValueList; aStrDelimiter:string);
        procedure ClearChart;
        procedure SetTimeScale(aScale: TChartTimeScale);
        procedure SetTimeMode(aMode: TChartTimeMode);
        function GetMaxTimeAllowedFromTimeScale(aScale: TChartTimeScale): double;
        procedure SetAutoTimeScale(aEndTimeInSecs: double);
        function GetValueLabel(aValueInSecs: double): string;

        procedure Print();

        property VerticalAxisTitle: string read GetVerticalAxisTitle write SetVerticalAxisTitle;
        property Elements[intChartID: integer]: TChartElement read ElementByChartID; default;
        property Cursor: TCursor read GetCursor write SetCursor;
        property OnClickSeries: TChartClickSeries write SetOnClickSeries;
        property OnMouseMove: TMouseMoveEvent write fOnMouseMoveEvent;
        property OnMouseDown: TMouseEvent write fOnMouseDownEvent;
        property MarkTextList: TStringList read FMarkTextList;
        property View3D: boolean read GetView3D write SetView3D;
        property ZoomFactor: double read fZoomFactor write fZoomFactor;
        property OnHorizontalAxisTitleEvent: THorizontalAxisTitleEvent read fOnHorizontalAxisTitleEvent
            write fOnHorizontalAxisTitleEvent;
        property AbsoluteStartDate: TDateTime read fAbsoluteStartDate write fAbsoluteStartDate;
        property TimeScale: TChartTimeScale read fTimeScale write SetTimeScale;
        property TimeMode: TChartTimeMode read fTimeMode write SetTimeMode;
        property OnGetElementMarkText: TGetElementMarkText read fOnGetElementMarkText
            write fOnGetElementMarkText;
        property SelectedID: integer read fSelectedID;
    end;


implementation


uses
    Math,
    Forms,
    UtilLib,
    TimeSpan;

const
    INT_HORIZONTAL_AXIS_SCALE_DEFAULT = tsSecs;
    INT_VERTICAL_AXIS_BUFFER = 1;

    // ******************************************* TChartElement **************************************************
    // ------------------------------------------------------------------------------------------------------------
constructor TChartElement.Create(aSeries: TGanttSeries; aIntChartID: integer; aData: TObject;
    aIntLevelID: integer = 0; aIntListID: integer = 0);
// ------------------------------------------------------------------------------------------------------------
// aSeries must be the GanttSeries object, and aIntChartID is the index of the GanttBar in the GanttSeries object
// aIntDataID is an ID that has helps associate this object externally (for example, a DB record Primary Key )
// aIntLevelID is the ID of the TChartElementlist in which the TChartElement is stored.
// aIntListID is the index of the element within the TChartElementList
// ------------------------------------------------------------------------------------------------------------
begin
    FSeries := aSeries;
    FChartID := aIntChartID;
    FData := aData;
    FLevelID := aIntLevelID;
    FListID := aIntListID;
    FDirty := false;
end;

// ------------------------------------------------------------------------------------------------------------
function TChartElement.GetStartValue: double;
// ------------------------------------------------------------------------------------------------------------
begin
    result := FSeries.StartValues[FChartID];
end;

// ------------------------------------------------------------------------------------------------------------
function TChartElement.GetFinishValue: double;
// ------------------------------------------------------------------------------------------------------------
begin
    result := FSeries.EndValues[FChartID];
end;

procedure TChartElement.SetFinishValue(aValue: double);
begin
    FSeries.EndValues[FChartID] := aValue;
end;

// ------------------------------------------------------------------------------------------------------------
function TChartElement.GetValueColor: TColor;
// ------------------------------------------------------------------------------------------------------------
begin
    result := FSeries.ValueColor[FChartID];
end;

// ------------------------------------------------------------------------------------------------------------
procedure TChartElement.SetValueColor(aColor: TColor);
// ------------------------------------------------------------------------------------------------------------
begin
    FSeries.ValueColor[FChartID] := aColor;
end;

// ------------------------------------------------------------------------------------------------------------
function TChartElement.GetText: string;
// ------------------------------------------------------------------------------------------------------------
begin
    result := FSeries.XLabel[FChartID];
end;

// **************************************** TChartElementList *************************************************
// ------------------------------------------------------------------------------------------------------------
constructor TChartElementList.Create(aSeries: TGanttSeries; aTrackList: TList; aIntLevel: integer);
// ------------------------------------------------------------------------------------------------------------
begin
    inherited Create;
    FSeries := aSeries;
    FTrackList := aTrackList;
    FList := TList.Create;
    FLevel := aIntLevel;
end;

// ------------------------------------------------------------------------------------------------------------
destructor TChartElementList.Destroy;
// ------------------------------------------------------------------------------------------------------------
begin
    self.DeleteAll();
    FList.Free;
    inherited Destroy;
end;

// ------------------------------------------------------------------------------------------------------------
procedure TChartElementList.DeleteAll();
// ------------------------------------------------------------------------------------------------------------
var
    i: integer;
begin
    for i := 0 to FList.Count - 1 do
    begin
        TObject(FList[i]).Free;
    end;
end;

// ------------------------------------------------------------------------------------------------------------
function TChartElementList.AddElement(aChartID: integer; aData: TObject): integer;
// ------------------------------------------------------------------------------------------------------------
// Create a new element and add it to this elementlist
// also add it to the tracklist
// ------------------------------------------------------------------------------------------------------------
var
    xElement: TChartElement;
begin
    xElement := TChartElement.Create(FSeries, aChartId, aData, FLevel, FList.Count);
    result := FList.Add(xElement);
    FTrackList.Add(xElement);
end;

// ------------------------------------------------------------------------------------------------------------
procedure TChartElementList.SetElement(aIntIndex: integer; aElement: TChartElement);
// ------------------------------------------------------------------------------------------------------------
begin
    AddElement(aElement.ChartID, aElement.Data)
end;

// ------------------------------------------------------------------------------------------------------------
function TChartElementList.GetElement(aIntIndex: integer): TChartElement;
// ------------------------------------------------------------------------------------------------------------
begin
    result := FList[aintIndex];
end;

// ------------------------------------------------------------------------------------------------------------
function TChartElementList.GetCount: integer;
// ------------------------------------------------------------------------------------------------------------
begin
    result := FList.Count;
end;

// *************************************** TChartElementLists *************************************************
// ------------------------------------------------------------------------------------------------------------
constructor TChartElementLists.Create(aSeries: TGanttSeries; aTrackList: TList);
// ------------------------------------------------------------------------------------------------------------
begin
    inherited Create;
    FSeries := aSeries;
    FList := TStringList.Create;
    FTrackList := aTrackList;

end;

// ------------------------------------------------------------------------------------------------------------
destructor TChartElementLists.Destroy;
// ------------------------------------------------------------------------------------------------------------
begin
    self.DeleteAll();
    FList.Free;
    // FTrackList.Free;
    inherited Destroy;
end;

// ------------------------------------------------------------------------------------------------------------
procedure TChartElementLists.DeleteAll();
// ------------------------------------------------------------------------------------------------------------
// Free each elementlist object.
// Clear the FList which held the pointers to the elementlist objects.
// ------------------------------------------------------------------------------------------------------------
var
    i: integer;
begin
    for i := 0 to FList.Count - 1 do
    begin
        TChartElementList(FList.Objects[i]).Free;
    end;
    FList.Clear;
end;

// ------------------------------------------------------------------------------------------------------------
function TChartElementLists.AddList(aIntLevel: integer): integer;
// ------------------------------------------------------------------------------------------------------------
// Create a new elementlist and add it to the Flist with the key 'aIntLevel'
// ------------------------------------------------------------------------------------------------------------
begin
    // FSeries.Pointer.VertSize:=
    result := FList.AddObject(IntToStr(aIntLevel), TChartElementList.Create(FSeries, FTrackList, aIntLevel));
end;

// ------------------------------------------------------------------------------------------------------------
procedure TChartElementLists.SetElement(index: integer; ElementList: TChartElementList);
// ------------------------------------------------------------------------------------------------------------
begin
    // This method must make a copy of ElementList...this is incorrectly programmed
    FList.Objects[index] := ElementList;
end;

// ------------------------------------------------------------------------------------------------------------
function TChartElementLists.GetElement(index: integer): TChartElementList;
// ------------------------------------------------------------------------------------------------------------
begin
    result := FList.Objects[index] as TChartElementList;
end;

// ------------------------------------------------------------------------------------------------------------
function TChartElementLists.GetCount: integer;
// ------------------------------------------------------------------------------------------------------------
// Return the number of ElementList objects in the FList
// ------------------------------------------------------------------------------------------------------------
begin
    result := FList.Count;
end;

// ------------------------------------------------------------------------------------------------------------
function TChartElementLists.GetIndexByLevel(aIntLevel: integer): integer;
// ------------------------------------------------------------------------------------------------------------
// Given the key 'aIntLevel' return the index of the ElementList within the FList
// ------------------------------------------------------------------------------------------------------------
begin
    result := FList.IndexOf(IntToStr(aIntLevel));
end;

// ********************************************** TSchedChart *************************************************
// This object has a ChartElementsLists object, which is a list of ChartElementList objects.
// Each ChartElementList object in turn is a list of ChartElements that belong to a certain level
// The Tracklist, however, is a list of every ChartElement objects in the ChartElementList objects
// A GanttSeries is used from the TChart object. A GanttSeries is a series of horizontal bars,
// where each bar has a .
// In TChart implementation all the elements (Gantt bars) are stored one after the other in a long
// list and the only thing that distinguishes the bars in one level from another level is the Y value
// of the bar which allows for the bar to be drawn in a row higher up or in a row lower down.
// The TSchedChart implementation makes it possible to store the bars in separate TChartElementList objects
// where each list represents a different Y value ( a different row ). This in turn makes it possible to
// refer to a certain bar ( element ) in a certain row without having to search through the entire TChart
// list of bars (elements).
// ------------------------------------------------------------------------------------------------------------
constructor TSchedChart.Create(aOwner: TComponent);
// ------------------------------------------------------------------------------------------------------------
var
    titleAxis: TChartAxisTitle;
begin
    // create chart
    FChart := TChart.Create(aOwner);
    FChart.Parent := TWinControl(aOwner);

    // Vertical Axis:
    fVerticalAxis := Fchart.LeftAxis;

    titleAxis := TChartAxisTitle.Create(FChart);
    titleAxis.Angle := 90;

    with fVerticalAxis do
    begin
        Title := titleAxis;
        Inverted := true;
        AxisValuesFormat := '#0'; // :=false;
        Visible := true;
        Labels := true;
        LabelStyle := talValue;
        Automatic := false;
        Minimum := 0;
        Increment := 1;
    end;

    // Horizontal Axis
    fChart.BottomAxis.Visible := false;

    fHorizontalAxis := FChart.TopAxis;
    titleAxis := TChartAxisTitle.Create(FChart);
    with fHorizontalAxis do
    begin
        AxisValuesFormat := '0';
        Title := titleAxis;
        Automatic := true;
        LabelsOnAxis := true;
        Visible := true;
    end;

    // Gantt Series ( bars )
    self.CreateGanttSeries(FGanttSeries);
    FChart.AddSeries(FGanttSeries);

    // internal variables
    FLevels := 0;
    FSelectedID := -1;
    FTrackList := TList.Create;
    FElementLists := TChartElementLists.Create(FGanttSeries, FTrackList);
    FMarkTextList := TStringList.Create;
    FMarkTextList.Sorted := true;
    fZoomFactor := 100;
    fMaxTimeAllowed := INT_MAXTIMEALLOWED_UNDEFINED;
    // alignment, bevels, borders
    FChart.Align := alClient;
    FChart.BevelInner := bvNone;
    FChart.BevelOuter := bvNone;
    FChart.BorderStyle := bsNone;

    // Margins:
    FChart.MarginLeft := 1;
    FChart.MarginBottom := 4;

    FChart.Legend.Visible := false;
    FChart.View3d := false;
    FChart.AxisVisible := true;

    // Arrow Series
    CreateArrowSeries(fArrowSeries);
    FChart.AddSeries(FArrowSeries);
    // FArrowSeries.AddArrow(0,FVerticalAxis.Maximum, 0 ,fVerticalAxis.Minimum,'',clRed);

    // Zooming, Scrolling/Panning
    FChart.AllowPanning := pmHorizontal;
    FChart.OnAllowScroll := DoOnAllowScroll; // panning
    fChart.OnZoom := self.DoOnZoom;
    FChart.OnUndoZoom := self.DoOnUndoZoom;

    FChart.OnGetAxisLabel := self.DoOnGetAxisLabel;
    FChart.OnResize := self.DoOnChartResize;

    FChart.OnMouseMove := DoOnMouseMove;
    FChart.OnMouseDown := DoOnMouseDown;

    FChart.Visible := true;

end;

// ------------------------------------------------------------------------------------------------------------
destructor TSchedChart.Destroy;
// ------------------------------------------------------------------------------------------------------------
begin
    FElementLists.Free;
    FTrackList.Free;
    FMarkTextList.Free;
    inherited Destroy;
end;

// ------------------------------------------------------------------------------------------------------------
procedure TSchedChart.ClearChart;
// ------------------------------------------------------------------------------------------------------------
// Removes all elements from both the TSchedChart implementation and the TChart implementation
// The result is a TChart and TSchedChart with 0 elements
// ------------------------------------------------------------------------------------------------------------
begin
    FTrackList.clear;
    FElementLists.DeleteAll();
    FArrowSeries.Clear;
    FGanttSeries.Clear;
    FMarkTextList.Clear;
    // self.DestroyGanttSeries(FGanttSeries);
    // self.CreateGanttSeries(FGanttSeries);
end;

function TSchedChart.GetHorizAxisType(): THorizAxis;
begin
    if fHorizontalAxis = fChart.TopAxis then
        result := aTopAxis
    else
        result := aBottomAxis;
end;

function TSchedChart.GetVertAxisType(): TVertAxis;
begin
    if fVerticalAxis = fChart.LeftAxis then
        result := aLeftAxis
    else
        result := aRightAxis;
end;

// ------------------------------------------------------------------------------------------------------------
procedure TSchedChart.CreateGanttSeries(var aGanttPointer: TGanttSeries);
// ------------------------------------------------------------------------------------------------------------
begin

    aGanttPointer := TGanttSeries.Create(FChart);
    with aGanttPointer do
    begin
        Pointer.Visible := true;
        Pointer.VertSize := 20;
        Clear;
        StartValues.Order := loNone;
        EndValues.Order := loNone;
        NextTask.Order := loNone;
        ColorEachPoint := True;
        OnGetMarkText := SeriesOnGetMarkText;
        Marks.ArrowLength := 20;
        Marks.Visible := true;
        HorizAxis := GetHorizAxisType;
        VertAxis := GetVertAxisType;

        XValues.DateTime := false;
        YValues.DateTime := false;
        ShowInLegend := false;
    end;
end;

// ------------------------------------------------------------------------------------------------------------
procedure TSchedChart.CreateArrowSeries(var aArrowSeriesPointer: TArrowSeries);
// ------------------------------------------------------------------------------------------------------------
begin
    aArrowSeriesPointer := TArrowSeries.Create(FChart);

    with aArrowSeriesPointer do
    begin
        HorizAxis := GetHorizAxisType;

        XValues.DateTime := false;
        YValues.DateTime := false;
    end;
end;

{
  //------------------------------------------------------------------------------------------------------------
  procedure TSchedChart.DestroyGanttSeries(var aGanttPointer:TGanttSeries);
  //------------------------------------------------------------------------------------------------------------
  begin
  FreeAndNil(aGanttPointer);
  end;
}
// ------------------------------------------------------------------------------------------------------------
function TSchedChart.AddBar(aData: TObject; aIntLevel: integer; aDblStart, aDblFinish: double;
    aStrLabel: string; aClrColor: TColor): integer;
// ------------------------------------------------------------------------------------------------------------
// aDblStart, aDblFinish are in seconds
// Add a new Bar to the TGanttSeries object and store the ChartID of the element in a TChartElementList
// ------------------------------------------------------------------------------------------------------------
var
    intElementListIndex: integer;
    elementList: TChartElementList;
begin

    if aClrColor = clNone then
        aClrColor := clBlack;

    aDblStart := aDblStart;
    aDblFinish := aDblFinish;

    result := FGanttSeries.AddGanttColor(aDblStart, aDblFinish, aIntLevel, aStrLabel, aClrColor);

    intElementListIndex := FElementLists.GetIndexByLevel(aIntLevel);

    // if the current level does not exist yet, add a new element list
    if intElementListIndex = -1 then
    begin
        intElementListIndex := FElementLists.AddList(aIntLevel);
    end;

    elementList := FElementLists.Elements[intElementListIndex];
    elementList.AddElement(result, aData);

end;

procedure TSchedChart.SetTimeScaleCaption(aScale: TChartTimeScale);
var
    xCaption: string;
begin
    xCaption := 'Time';
    if Assigned(fOnHorizontalAxisTitleEvent) then
        fOnHorizontalAxisTitleEvent(aScale, xCaption);
    FHorizontalAxis.Title.Caption := xCaption;
end;

procedure TSchedChart.SetHorizontalAxisTimeScale();
begin
    SetTimeScaleCaption(fTimeScale);

    if (fTimeMode = tsAbsolute) or (fTimeScale = tsDays) then
    begin
        fChart.MarginTop := 12;
        fHorizontalAxis.LabelsAngle := 90;
    end
    else
    begin
        fChart.MarginTop := 4;
        fHorizontalAxis.LabelsAngle := 0;
    end;
end;

procedure TSchedChart.SetTimeMode(aMode: TChartTimeMode);
begin
    fTimeMode := aMode;
    SetHorizontalAxisTimeScale();
end;

procedure TSchedChart.SetTimeScale(aScale: TChartTimeScale);
begin
    fTimeScale := aScale;
    SetHorizontalAxisTimeScale();
    fMaxTimeAllowed := GetMaxTimeAllowedFromTimeScale(aScale);
end;

function TSchedChart.GetMaxTimeAllowedFromTimeScale(aScale: TChartTimeScale): double;
begin
    case aScale of
        tsSecs:
            result := INT_SECS_PER_MIN;
        tsMins:
            result := INT_SECS_PER_HOUR;
        tsHours:
            result := INT_SECS_PER_DAY;
        else
            result := MAXDOUBLE;
    end;
end;

procedure TSchedChart.SetAutoTimeScale(aEndTimeInSecs: double);
var
    xScale: TChartTimeScale;
begin
    if aEndTimeInSecs <= INT_SECS_PER_MIN then
        xScale := tsSecs
    else if aEndTimeInSecs <= INT_SECS_PER_HOUR then
        xScale := tsMins
    else if aEndTimeInSecs <= INT_SECS_PER_DAY then
        xScale := tsHours
    else
        xScale := tsDays;

    self.SetTimeScale(xScale);
end;

// ------------------------------------------------------------------------------------------------------------
function TSchedChart.ChartIDByLocation(aIntLevelID, aIntIndex: integer): integer;
// ------------------------------------------------------------------------------------------------------------
// Get the TChart chartId of the element that at the index 'aIntIndex' of the
// level (Y value) 'aIntLevelID'
// ------------------------------------------------------------------------------------------------------------
var
    intElementListIndex: integer;
begin
    result := -1;
    intElementListIndex := FElementLists.GetIndexByLevel(aIntLevelID);
    if intElementListIndex = -1 then
        Exit;
    result := FElementLists.Elements[intElementListIndex].Elements[aIntIndex].ChartID;
end;

// ------------------------------------------------------------------------------------------------------------
function TSchedChart.NeighborChartID(aIntChartID: integer; aIntIDOffset: integer): integer;
// ------------------------------------------------------------------------------------------------------------
// Return the ChartID of the element that is 'aIntIDOffSet' elements to the right or to the left of
// the element with the ChartID aIntChartID.
// An element to the right needs a negative aIntIDOffset.  An element to the right needs a positive
// aIntIDOffSet
// ------------------------------------------------------------------------------------------------------------
var
    list: TChartElementList;
    element: TChartElement;
    intElementIndex, intElementListIndex, intLevel, intNeighborIndex: integer;

begin
    result := -1;
    element := ElementByChartId(aIntChartID);
    intLevel := element.FLevelID;
    intElementListIndex := FElementLists.GetIndexByLevel(intLevel);
    list := FElementLists.Elements[intElementListIndex];
    intElementIndex := element.FListID;
    intNeighborIndex := intElementIndex + aIntIDOffset;
    if (0 <= intNeighborIndex) and (intNeighborIndex < list.Count) then
    begin
        result := list.Elements[intNeighborIndex].FChartID;
    end;

end;

// ------------------------------------------------------------------------------------------------------------
procedure TSchedChart.Resize();
// ------------------------------------------------------------------------------------------------------------
begin
    DoOnChartResize(fChart);
end;

// ------------------------------------------------------------------------------------------------------------
procedure TSchedChart.SetHorizontalAxisMinMax(aDblMin, aDblMax: double);
// ------------------------------------------------------------------------------------------------------------
const
    INT_NUM_INCREMENTS = 10;
    INT_MIN_INCREMENT_VALUE = 1;
var
    xDif, xBuf: double;
begin

    xDif := (aDblMax - aDblMin);
    xBuf := xDif / 300;
    FHorizontalAxis.SetMinMax(aDblMin - xBuf, aDblMax + xBuf);
    FHorizontalAxis.Increment := Max(INT_MIN_INCREMENT_VALUE, (xDif) / INT_NUM_INCREMENTS);

end;

procedure TSchedChart.SetVerticalAxisMinMax(aDblMin, aDblMax: double);
begin
    FVerticalAxis.SetMinMax(aDblMin - INT_VERTICAL_AXIS_BUFFER, aDblMax + INT_VERTICAL_AXIS_BUFFER);
end;

// ------------------------------------------------------------------------------------------------------------
procedure TSchedChart.AbsoluteZoom(aPercent: double);
// ------------------------------------------------------------------------------------------------------------
var
    xMin, xMax: double;
begin

    // UndoZoom();
    // Zoom( FGanttSeries.MinXValue, FGanttSeries.MaxXValue, aDblX, aDblPercent, aBlnIsPixelPos );
    xMin := FGanttSeries.MinXValue * (100 / aPercent);
    xMax := FGanttSeries.MaxXValue * (100 / aPercent);
    SetHorizontalAxisMinMax(xMin, xMax);

    SetVerticalAxisMinMax(1, FElementLists.Count);
end;

// ------------------------------------------------------------------------------------------------------------
procedure TSchedChart.Zoom(aPercent: double);
// ------------------------------------------------------------------------------------------------------------
begin
    fZoomFactor := aPercent;
    ReZoom();
end;

// ------------------------------------------------------------------------------------------------------------
procedure TSchedChart.ReZoom();
// ------------------------------------------------------------------------------------------------------------
begin
    if fIsUserZoomed then
        EXIT;
    AbsoluteZoom(fZoomFactor);
end;

procedure TSchedChart.SelectElement(aSelectChartID: integer);
var
    xElementToSelect: TChartElement;

begin
    xElementToSelect := ElementByChartId(aSelectChartID);
    // if current element is not selected then select it
    FSelectedColor := xElementToSelect.ValueColor;
    xElementToSelect.ValueColor := COLOR_ELEMENT_SELECTED;
    FSelectedID := aSelectChartID;

    ReDrawArrowSeries(xElementToSelect.StartValue);
end;

procedure TSchedChart.RefreshVisualSelect();
begin
    if fSelectedID = -1 then
        EXIT;
    SelectElement(fSelectedID);
end;

// ------------------------------------------------------------------------------------------------------------
function TSchedChart.VisualSelectElement(aIntSelectedNew: integer): integer;
// ------------------------------------------------------------------------------------------------------------
// Change the colour of the element represented by the chartId aIntSelectedNew so that it looks like
// it has been selected.  Restore the true colour of the previously selected element if one was selected
// Zoom in on the object to make it the focal point of the Chart
// ------------------------------------------------------------------------------------------------------------
var
    elemPrev: TChartElement;
    elemNew: TChartElement;

begin
    result := 1;

    // if other element had been selected, then restore its color
    if (FSelectedID <> -1) then
    begin
        // if current element is already selected, restore its color
        if (aIntSelectedNew = FSelectedID) then
        begin
            elemNew := ElementByChartId(aIntSelectedNew);
            elemNew.ValueColor := FSelectedColor;
            FSelectedID := -1;
            result := 0;
            Exit;
        end;

        elemPrev := ElementByChartId(FSelectedID);
        elemPrev.ValueColor := FSelectedColor;
    end;

    SelectElement(aIntSelectedNew)

end;

// ------------------------------------------------------------------------------------------------------------
function TSchedChart.GetSelectedElementChartID: integer;
// ------------------------------------------------------------------------------------------------------------
begin
    result := FSelectedID;
end;

// ------------------------------------------------------------------------------------------------------------
procedure TSchedChart.GetSynchElements(lstResult: TStringValueList; aIntChartID: integer; aIDFormat: string);
// ------------------------------------------------------------------------------------------------------------
// Get all the elements that are running synchronosly with the element
// given by aIntChartID, and put them in a list...
// ------------------------------------------------------------------------------------------------------------
var
    i, j: integer;
    ElementList: TChartElementList;
    MainElement, CurElement: TChartElement;
    intCurChartID: integer;
    dblMainStart, dblMainFinish, dblCurStart, dblCurFinish: double;
    // pIntCurChartID:PInteger;
    procedure AddID(aID: integer);
    begin
        lstResult.Add(Format(aIDFormat, [aID]));
    end;

begin
    MainElement := ElementByChartId(aIntChartID);
    dblMainStart := MainElement.StartValue;
    dblMainFinish := MainElement.FinishValue;

    for i := 0 to FElementLists.Count - 1 do
    begin
        ElementList := FElementLists.Elements[i];
        if ElementList.FLevel = MainElement.Level then
            CONTINUE;
        for j := 0 to ElementList.Count - 1 do
        begin
            CurElement := ElementList.Elements[j];
            intCurChartID := CurElement.FChartID;
            dblCurStart := CurElement.StartValue;
            if dblCurStart >= dblMainFinish then
                Break;
            dblCurFinish := CurElement.FinishValue;

            if dblCurFinish > dblMainStart then
            begin
                AddID(intCurChartID);
            end;
        end;
    end;
end;

{
  //------------------------------------------------------------------------------------------------------------
  procedure TSchedChart.GetDirty(resultList:TStringValueList; aStrDelimiter:string);
  //------------------------------------------------------------------------------------------------------------
  var
  i:integer;
  element:TChartElement;
  strTemp:string;
  varTemp:variant;
  begin
  for i:=0 to FTrackList.Count-1 do begin
  element:= ElementByChartID(i);
  if not element.Dirty then CONTINUE;
  //intNextChartID:= element.FChartID;
  strTemp:= IntToStr(element.FDataID) + aStrDelimiter;
  varTemp:= element.StartValue;
  strTemp:= strTemp+ IntToStr(varTemp) + aStrDelimiter;
  varTemp:= element.FinishValue;
  strTemp:= strTemp+ IntToStr(varTemp);
  resultList.Add(strTemp);

  end;
  end;
}

// ------------------------------------------------------------------------------------------------------------
function TSchedChart.ElementByChartId(aChartID: integer): TChartElement;
// ------------------------------------------------------------------------------------------------------------
begin
    result := FTrackList[aChartID];
end;

// ------------------------------------------------------------------------------------------------------------
procedure TSchedChart.SetOnClickSeries(aEventHandler: TChartClickSeries);
// ------------------------------------------------------------------------------------------------------------
begin
    FChart.OnClickSeries := aEventHandler;
end;

// ------------------------------------------------------------------------------------------------------------
procedure TSchedChart.SeriesOnGetMarkText(Sender: TChartSeries; ValueIndex: Longint; var MarkText: string);
// ------------------------------------------------------------------------------------------------------------
// When the chart is repainted, this event will be called.
// The purpose it to only show the text of the bar that is currently selected and to temporarily
// set the text of those not selected to an emtpy string.
// ------------------------------------------------------------------------------------------------------------
begin
    // if gMarksShowAll then Exit;
    // ((FSelectedID=-1) or

    if (ValueIndex <> FSelectedID) and (FMarkTextList.IndexOf(IntToStr(ValueIndex)) = -1) then
    begin
        MarkText := '';
    end
    else
    begin
        if Assigned(fOnGetElementMarkText) then
            fOnGetElementMarkText(ValueIndex, MarkText);
    end;
end;

// ------------------------------------------------------------------------------------------------------------
procedure TSchedChart.SetCursor(aCursor: TCursor);
// ------------------------------------------------------------------------------------------------------------
begin
    FGanttSeries.Cursor := aCursor;
end;

// ------------------------------------------------------------------------------------------------------------
function TSchedChart.GetCursor: TCursor;
// ------------------------------------------------------------------------------------------------------------
begin
    result := FGanttSeries.Cursor;
end;

// ------------------------------------------------------------------------------------------------------------
function TSchedChart.GetView3D: boolean;
// ------------------------------------------------------------------------------------------------------------
begin
    result := FChart.View3D;
end;

// ------------------------------------------------------------------------------------------------------------
procedure TSchedChart.SetView3D(aBlnVal: boolean);
// ------------------------------------------------------------------------------------------------------------
begin
    FChart.View3D := aBlnVal;
end;

procedure TSchedChart.Print();
begin
    fChart.PrintLandscape();
end;

{
  //------------------------------------------------------------------------------------------------------------
  function TSchedChart.ConvertPixelPosToAxisCoord(aChartAxis:TChartAxis; aDblPixelPos:double):double;
  //------------------------------------------------------------------------------------------------------------
  var dblAxisWidth, dblPixelWidth, dblPosAsPercent:double;
  begin
  with aChartAxis do begin
  dblAxisWidth:= Maximum-Minimum;
  dblPixelWidth:=IEndPos-IStartPos;
  dblPosAsPercent:= (aDblPixelPos-IStartPos)/dblPixelWidth;
  result:= (dblAxisWidth*dblPosAsPercent + Minimum);
  end;
  end;
}
// ------------------------------------------------------------------------------------------------------------
procedure TSchedChart.ReDrawArrowSeries(aDblStartVal: double; aFlipColor: boolean = false);
// ------------------------------------------------------------------------------------------------------------
begin
    if aFlipColor then
    begin
        if fArrowSeriesColor = CLR_ARROW_SERIES_1 then
            fArrowSeriesColor := CLR_ARROW_SERIES_2
        else
            fArrowSeriesColor := CLR_ARROW_SERIES_1;
    end
    else
        fArrowSeriesColor := CLR_ARROW_SERIES_1;

    FArrowSeries.Clear;
    FArrowSeries.AddArrow(aDblStartVal, FVerticalAxis.Maximum, aDblStartVal, FVerticalAxis.Minimum, '',
        fArrowSeriesColor);
end;

function TSchedChart.GetVerticalAxisTitle: string;
begin
    result := FVerticalAxis.Title.Caption;
end;

procedure TSchedChart.SetVerticalAxisTitle(aTitle: string);
begin
    FVerticalAxis.Title.Caption := aTitle;
end;

// ------------------------------------------------------------------------------------------------------------
function TSchedChart.HorizontalAxisValueAllowed(aValue: double): boolean;
// ------------------------------------------------------------------------------------------------------------
begin
    result := (Abs(aValue) < fMaxTimeAllowed);
end;

function EncodeToDays(aTotalSecs: double): string;
var
    xTimeSpan: TTimeSpan;
begin
    xTimeSpan := TTimeSpan.FromSeconds(aTotalSecs);
    result := Format('%d:%.2d:%.2d:%.2d', [xTimeSpan.TotalDays, xTimeSpan.Hours, xTimeSpan.Minutes,
        xTimeSpan.Seconds]);
end;

function EncodeToHours(aTotalSecs: cardinal): string;
var
    xTimeSpan: TTimeSpan;
begin
    xTimeSpan := TTimeSpan.FromSeconds(aTotalSecs);
    result := Format('%.2d:%.2d:%.2d', [xTimeSpan.TotalHours, xTimeSpan.Minutes, xTimeSpan.Seconds]);
end;

function EncodeToMins(aTotalSecs: cardinal): string;
var
    xTimeSpan: TTimeSpan;
begin
    xTimeSpan := TTimeSpan.FromSeconds(aTotalSecs);
    result := Format('%.2d:%.2d', [xTimeSpan.Minutes, xTimeSpan.Seconds]);
end;

function EncodeToSecs(aTotalSecs: cardinal): string;
var
    xTimeSpan: TTimeSpan;
begin
    xTimeSpan := TTimeSpan.FromSeconds(aTotalSecs);
    result := Format('%.2d', [xTimeSpan.Seconds]);
end;

// ------------------------------------------------------------------------------------------------------------
function TSchedChart.GetValueLabel(aValueInSecs: double): string;
// ------------------------------------------------------------------------------------------------------------
var
    xMinusSign: boolean;
    xSecs: cardinal;
    xDateTime: TDateTime;
begin
    if fTimeMode = tsAbsolute then
    begin
        xDateTime := (fAbsoluteStartDate + (aValueInSecs / SecsPerDay));
        case fTimeScale of
            tsSecs:
                result := FormatDateTime('hh:nn:ss', xDateTime);
            tsMins:
                result := FormatDateTime('hh:nn:ss', xDateTime);
            tsHours:
                result := FormatDateTime('hh:nn:ss', xDateTime);
            tsDays:
                result := FormatDateTime('dd mmm hh:nn:ss', xDateTime);
        end;
    end
    else
    begin
        xMinusSign := aValueInSecs < 0;
        xSecs := Abs(Round(aValueInSecs));

        case fTimeScale of
            tsSecs:
                result := EncodeToSecs(xSecs);
            tsMins:
                result := EncodeToMins(xSecs);
            tsHours:
                result := EncodeToHours(xSecs);
            tsDays:
                result := EncodeToDays(xSecs);
        end;
        if xMinusSign then
            result := '-' + result;
    end;

end;

// ------------------------------------------------------------------------------------------------------------
procedure TSchedChart.DoOnGetAxisLabel(Sender: TChartAxis; Series: TChartSeries; ValueIndex: LongInt;
    var LabelText: string);
// ------------------------------------------------------------------------------------------------------------
var
    xSecsAsDbl: double;
begin
    if Sender <> fHorizontalAxis then
        EXIT;
    xSecsAsDbl := StrToFloat(LabelText);
    if not HorizontalAxisValueAllowed(xSecsAsDbl) then
    begin
        LabelText := '';
        EXIT;
    end;
    LabelText := GetValueLabel(xSecsAsDbl);

end;

// ------------------------------------------------------------------------------------------------------------
procedure TSchedChart.DoOnChartResize(Sender: TObject);
// ------------------------------------------------------------------------------------------------------------
var
    intAxisHeight: integer;
    dblAxisMaxVal: double;
    intSize: integer;
begin
    FChart.Update;
    if FVerticalAxis.Maximum < 1 then
        Exit;

    intAxisHeight := FVerticalAxis.IEndPos - FVerticalAxis.IStartPos;
    dblAxisMaxVal := FVerticalAxis.Maximum; // atleast 1
    intSize := trunc(0.4 * intAxisHeight / dblAxisMaxVal);
    intSize := Max(1, intSize); // atleast 1;
    intSize := Min(20, intSize); // atmost 20;
    FGanttSeries.Pointer.VertSize := intSize;
end;

procedure TSchedChart.DoOnZoom(aObject: TObject);
begin
    fIsUserZoomed := true;
end;

// ------------------------------------------------------------------------------------------------------------
procedure TSchedChart.DoOnUndoZoom(aObject: TObject);
// ------------------------------------------------------------------------------------------------------------
begin
    fIsUserZoomed := false;
    ReZoom();
end;

// ------------------------------------------------------------------------------------------------------------
procedure TSchedChart.DoOnAllowScroll(Sender: TChartAxis; var Amin, AMax: Double; var AllowScroll: Boolean);
// ------------------------------------------------------------------------------------------------------------
begin

    if (Sender <> FHorizontalAxis) or (fMaxTimeAllowed = INT_MAXTIMEALLOWED_UNDEFINED) then
        EXIT;

    if (not fAllowScroll) or ((not HorizontalAxisValueAllowed(aMin)) or
        (not HorizontalAxisValueAllowed(aMax))) then
    begin
        AllowScroll := false;
    end;
end;

// ------------------------------------------------------------------------------------------------------------
procedure TSchedChart.DoOnMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
// ------------------------------------------------------------------------------------------------------------
begin
    fAllowScroll := (ssShift in Shift) and (ssRight in Shift);
    if not fAllowScroll then
        EXIT;

end;

// ------------------------------------------------------------------------------------------------------------
procedure TSchedChart.DoOnMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
// ------------------------------------------------------------------------------------------------------------
begin

    if Assigned(fOnMouseDownEvent) then
        fOnMouseDownEvent(sender, Button, shift, x, y);
end;


end.
