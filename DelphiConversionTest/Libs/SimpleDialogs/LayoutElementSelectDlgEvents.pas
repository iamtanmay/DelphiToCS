{ -----------------------------------------------------------------------------------------------------------
  Copyright © 2013 Zinsser Analytic GmbH - All rights reserved.
  Author       : Wolfgang Lyncke (wl)
  Description  :
  -----------------------------------------------------------------------------------------------------------
  Revision History:
  date     op  method                              track-no improvement/change
  -------- --  ----------------------------------  -------- -------------------------------------------------
  09.04.13 wl                                      TN6095   Initial Revision
  08.04.14 ts                                      TN6391   PositionArray for CherryPicking added
  ----------------------------------------------------------------------------------------------------------- }

unit LayoutElementSelectDlgEvents;


interface


uses
    Controls,
    SceneGraphics,
    Rack,
    RackWell,
    Carrier,
    LayoutElementCallbackTypes,
    PopupMenuInfo,
    CarrierSlot,
    CustomLayoutManager,
    Layout;

type
    TLastSelectedType = (lastNone, lastMethodEditor, lastProperties);

    TSelectMode = (Columnwise, Square);

    TChooseRackEvent = procedure(aRackName: string) of object;
    TChooseRackPosEvent = function(const aRackName: string; aPosInt, aMaxRows: integer;
        const aParamIndexOffset: integer; aSetFocus: boolean): string of object;
    TChooseCarrierSlotEvent = procedure(aCarrierName: string; aSlotNr: integer) of object;

    TLayoutElementSelectDlgEvents = class
    private
        fOnChooseRack: TChooseRackEvent;
        fOnChooseRackFirstPos: TChooseRackPosEvent;
        fOnChooseRackLastPos: TChooseRackPosEvent;
        fOnChooseCarrierSlot: TChooseCarrierSlotEvent;
        fLastSelected: TLastSelectedType;
        procedure SetLastSelected(const Value: TLastSelectedType);
    public
        constructor Create();
        //
        procedure ChooseRack(aRackName: string);
        function ChooseRackFirstPos(const aRackName: string; aPosInt, aMaxRows: integer;
            const aParamIndexOffset: integer; aSetFocus: boolean): string;
        function ChooseRackLastPos(const aRackName: string; aPosInt, aMaxRows: integer;
            const aParamIndexOffset: integer; aSetFocus: boolean): string;
        procedure ChooseCarrierSlot(aCarrierName: string; aSlotNr: integer);

        function CanChooseRack: boolean;
        function CanChooseCarrierSlot: boolean;

        property OnChooseRack: TChooseRackEvent read fOnChooseRack write fOnChooseRack;
        property OnChooseRackFirstPos: TChooseRackPosEvent read fOnChooseRackFirstPos
            write fOnChooseRackFirstPos;
        property OnChooseRackLastPos: TChooseRackPosEvent read fOnChooseRackLastPos
            write fOnChooseRackLastPos;
        property OnChooseCarrierSlot: TChooseCarrierSlotEvent read fOnChooseCarrierSlot
            write fOnChooseCarrierSlot;
        property LastSelected: TLastSelectedType read fLastSelected write SetLastSelected;
    end;

    TLayoutElementSelectDlgGetEvents = function(): TLayoutElementSelectDlgEvents of object;

    TLayoutElementSelectDlgRackWell = class(TRackWell)
    private
        fOnGetEvents: TLayoutElementSelectDlgGetEvents;
        procedure DoMouseDown(aSender: TObject; aButton: TGraphicsMouseButton; aShift: TGraphicsShiftState;
            aX, aY: double);
        procedure DoMouseUp(aSender: TObject; aButton: TGraphicsMouseButton; aShift: TGraphicsShiftState;
            aX, aY: double);
        procedure DoMouseMove(Sender: TObject; Shift: TGraphicsShiftState; aX, aY: double);
        procedure DoPopup(aSender: TObject; aX, aY: double; var vPopupMenuInfos: TPopupMenuInfoList);
        procedure DoMouseDragOver(aSender, aSource: TObject; aX, aY: double; aState: TGraphicsDragState;
            var vAccept: boolean);
    protected
        procedure DoInitGraphics; override;
    public
        property OnGetEvents: TLayoutElementSelectDlgGetEvents read fOnGetEvents write fOnGetEvents;
    end;

    TLayoutElementSelectDlgRack = class(TRack)
    private
        fMultiSelectFirstWell: TRackWell;
        fMultiSelectLastWell: TRackWell;
        fOnGetEvents: TLayoutElementSelectDlgGetEvents;
        fPositionArray: TArray<integer>;
        procedure DoPopup(aSender: TObject; aX, aY: double; var vPopupMenuInfos: TPopupMenuInfoList);
        procedure DoMouseUp(aSender: TObject; aButton: TGraphicsMouseButton; aShift: TGraphicsShiftState;
            aX, aY: double);
        procedure DoMouseDown(aSender: TObject; aButton: TGraphicsMouseButton; aShift: TGraphicsShiftState;
            aX, aY: double);
        procedure MultiSelectionDone();
    protected
        procedure DoInitGraphics; override;
        function DoCreateRackWell(aWellNr: integer): TRackWell; override;
    public
        constructor Create();
        procedure PaintAllPositions(const aFirstPosStr, aLastPosStr: string); overload;
        procedure PaintAllPositions(const aPositionArray: TArray<integer>); overload;
        procedure GetSelectedPositions(aFirstWell, aLastWell: TRackWell;
            out oFirstPosInt, oLastPosInt: integer);
        property MultiSelectFirstWell: TRackWell read fMultiSelectFirstWell write fMultiSelectFirstWell;
        property MultiSelectLastWell: TRackWell read fMultiSelectLastWell write fMultiSelectLastWell;
        property OnGetEvents: TLayoutElementSelectDlgGetEvents read fOnGetEvents write fOnGetEvents;
        property PositionArray: TArray<integer>read fPositionArray write fPositionArray;
    end;

    TLayoutElementSelectDlgCarrierSlot = class(TCarrierSlot)
    private
        fOnGetEvents: TLayoutElementSelectDlgGetEvents;
        procedure DoMouseDown(aSender: TObject; aButton: TGraphicsMouseButton; aShift: TGraphicsShiftState;
            aX, aY: double);
    protected
        procedure DoInitGraphics; override;
    public
        property OnGetEvents: TLayoutElementSelectDlgGetEvents read fOnGetEvents write fOnGetEvents;
    end;

    TLayoutElementSelectDlgCarrier = class(TCarrier)
    private
        fOnGetEvents: TLayoutElementSelectDlgGetEvents;
    protected
        function DoCreateCarrierSlot(): TCarrierSlot; override;
    public
        property OnGetEvents: TLayoutElementSelectDlgGetEvents read fOnGetEvents write fOnGetEvents;
    end;

    TLayoutElementSelectDlgLayout = class(TLayout)
    private
        fOnGetEvents: TLayoutElementSelectDlgGetEvents;
    protected
        function DoCreateRack: TRack; override;
        function DoCreateCarrier: TCarrier; override;
    public
        property OnGetEvents: TLayoutElementSelectDlgGetEvents read fOnGetEvents write fOnGetEvents;
    end;

    TLayoutElementSelectDlgLayoutManager = class(TCustomLayoutManager)
    private
        fOnGetEvents: TLayoutElementSelectDlgGetEvents;
    protected
        function DoCreateLayout(const aRunName, aLayoutName: string): TLayout; override;
        function GetTempSettingsSectionName: string; override;
    public
    public
        constructor Create(aBackgroundGraphicsParent: TWinControl;
            aOnGetEvents: TLayoutElementSelectDlgGetEvents);
    end;


implementation


uses
    SysUtils,
    Windows,
    MathUtils,
    GeneralTypes,
    DialogUtils,
    ControlUtils,
    MethodGUIParsing,
    Generics.Collections;

{ TLayoutElementSelectDlgEvents }

constructor TLayoutElementSelectDlgEvents.Create();
begin
    inherited Create();

    fLastSelected := lastNone;
end;

function TLayoutElementSelectDlgEvents.CanChooseCarrierSlot: boolean;
begin
    EXIT(Assigned(fOnChooseCarrierSlot));
end;

function TLayoutElementSelectDlgEvents.CanChooseRack: boolean;
begin
    EXIT(Assigned(fOnChooseRack));
end;

procedure TLayoutElementSelectDlgEvents.ChooseCarrierSlot(aCarrierName: string; aSlotNr: integer);
begin
    if Assigned(fOnChooseCarrierSlot) then
        fOnChooseCarrierSlot(aCarrierName, aSlotNr);
end;

procedure TLayoutElementSelectDlgEvents.ChooseRack(aRackName: string);
begin
    if Assigned(fOnChooseRack) then
        fOnChooseRack(aRackName);
end;

function TLayoutElementSelectDlgEvents.ChooseRackFirstPos(const aRackName: string; aPosInt, aMaxRows: integer;
    const aParamIndexOffset: integer; aSetFocus: boolean): string;
begin
    result := fOnChooseRackFirstPos(aRackName, aPosInt, aMaxRows, aParamIndexOffset, aSetFocus);
end;

function TLayoutElementSelectDlgEvents.ChooseRackLastPos(const aRackName: string; aPosInt, aMaxRows: integer;
    const aParamIndexOffset: integer; aSetFocus: boolean): string;
begin
    result := fOnChooseRackLastPos(aRackName, aPosInt, aMaxRows, aParamIndexOffset, aSetFocus);
end;

procedure TLayoutElementSelectDlgEvents.SetLastSelected(const Value: TLastSelectedType);
begin
    fLastSelected := Value;
end;

{ TLayoutElementSelectDlgRackWell }

procedure TLayoutElementSelectDlgRackWell.DoInitGraphics;
begin
    inherited;
    self.Graphics.Callbacks.PopupCallback := DoPopup;
    self.Graphics.Callbacks.MouseDownCallback := DoMouseDown;
    self.Graphics.Callbacks.MouseUpCallback := DoMouseUp;
    self.Graphics.Callbacks.MouseMoveCallback := DoMouseMove;
    self.Graphics.Callbacks.MouseDragOverCallback := DoMouseDragOver;
end;

procedure TLayoutElementSelectDlgRackWell.DoMouseDragOver(aSender, aSource: TObject; aX, aY: double;
    aState: TGraphicsDragState; var vAccept: boolean);
begin
    if aState = gdsDragMove then
    begin
        Sleep(100);
    end;
end;

procedure TLayoutElementSelectDlgRackWell.DoPopup(aSender: TObject; aX, aY: double;
    var vPopupMenuInfos: TPopupMenuInfoList);
begin
    (self.Rack as TRack).Graphics.Callbacks.PopupCallback((self.Rack as TRack), aX, aY, vPopupMenuInfos);
end;

procedure TLayoutElementSelectDlgRackWell.DoMouseDown(aSender: TObject; aButton: TGraphicsMouseButton;
    aShift: TGraphicsShiftState; aX, aY: double);
var
    xRack: TLayoutElementSelectDlgRack;
    xFirstPos, xLastPos: string;
    xFirstPosInt, xLastPosInt: integer;
    xPositionArray, xTempArray: TArray<integer>;
    xDone: boolean;
    i, j, xTemp: integer;
begin
    if aButton <> gmbLeft then
        EXIT;

    xRack := self.Rack as TLayoutElementSelectDlgRack;
    xRack.MultiSelectFirstWell := self;
    xRack.GetSelectedPositions(self, self, xFirstPosInt, xLastPosInt);
    xFirstPos := fOnGetEvents().ChooseRackFirstPos(xRack.Name, xFirstPosInt, xRack.Structure.Rows, 1, false);
    xLastPos := xFirstPos;
    xPositionArray := xRack.PositionArray;
    if GetKeyState(VK_CONTROL) < 0 then
    begin
        if Length(xPositionArray) > 0 then
        begin
            xTempArray := xPositionArray;
            SetLength(xPositionArray, Length(xPositionArray) + 1);
        end
        else
            SetLength(xPositionArray, 1);
        xDone := false;
        for i := low(xTempArray) to high(xTempArray) do
        begin
            if xTempArray[i] = xFirstPosInt then
            begin
                xDone := true;
                BREAK;
            end;
        end;
        if xDone then
        begin
            j := 0;
            SetLength(xPositionArray, Length(xTempArray) - 1);
            for i := 0 to high(xTempArray) do
            begin
                if xFirstPosInt <> xTempArray[i] then
                begin
                    xPositionArray[j] := xTempArray[i];
                    Inc(j);
                end;
            end;
        end
        else
            xPositionArray[Length(xPositionArray) - 1] := xFirstPosInt;

        xRack.PaintAllPositions(xPositionArray);
        xDone := false;
        while not xDone do
        begin
            xDone := true;
            for i := low(xPositionArray) to high(xPositionArray) - 1 do
            begin
                if xPositionArray[i] > xPositionArray[i + 1] then
                begin
                    xTemp := xPositionArray[i];
                    xPositionArray[i] := xPositionArray[i + 1];
                    xPositionArray[i + 1] := xTemp;
                    xDone := False;
                end;
            end;
        end;
    end
    else
    begin
        SetLength(xPositionArray, 0);
        xPositionArray := TMethodGUIParser.GetPositionArray(xFirstPos, xLastPos, xRack.Structure.Rows,
            xRack.Structure.Cols, 0);
        xRack.PaintAllPositions(xPositionArray);
        // xRack.PaintAllPositions(xFirstPos, xLastPos);
    end;
    xRack.PositionArray := xPositionArray;
end;

procedure TLayoutElementSelectDlgRackWell.DoMouseUp(aSender: TObject; aButton: TGraphicsMouseButton;
    aShift: TGraphicsShiftState; aX, aY: double);
var
    xRack: TLayoutElementSelectDlgRack;
    i, xFirstPos, xLastPos: integer;
    xPositionArray, xTempPositionArray: TArray<integer>;
begin
    xRack := self.Rack as TLayoutElementSelectDlgRack;
    if not Assigned(xRack.MultiSelectFirstWell) then
        EXIT;

    if GetKeyState(VK_CONTROL) < 0 then
    begin
        xRack.MultiSelectLastWell := xRack.MultiSelectFirstWell;
        xRack.GetSelectedPositions(xRack.fMultiSelectFirstWell, xRack.fMultiSelectLastWell, xFirstPos,
            xLastPos);
        xRack.MultiSelectionDone();
    end
    else
    begin
        xRack.MultiSelectLastWell := self;
        xRack.GetSelectedPositions(xRack.fMultiSelectFirstWell, xRack.fMultiSelectLastWell, xFirstPos,
            xLastPos);
        xRack.MultiSelectionDone();
    end;

    if xFirstPos <> xLastPos then
    begin
        if Length(xRack.PositionArray) > 0 then
        begin
            xPositionArray := TMethodGUIParser.GetPositionArray(IntToStr(xFirstPos), IntToStr(xLastPos),
                xRack.Structure.Rows, xRack.Structure.Cols, 0);
            xTempPositionArray := xRack.PositionArray;
            SetLength(xTempPositionArray, Length(xRack.PositionArray) + Length(xPositionArray));
            for i := Length(xRack.PositionArray) to Length(xRack.PositionArray) +
                Length(xPositionArray) - 1 do
                xTempPositionArray[i] := xPositionArray[i];
            xRack.PositionArray := xTempPositionArray;
        end
        else
            xRack.PositionArray := TMethodGUIParser.GetPositionArray(IntToStr(xFirstPos), IntToStr(xLastPos),
                xRack.Structure.Rows, xRack.Structure.Cols, 0);
    end;
end;

procedure TLayoutElementSelectDlgRackWell.DoMouseMove(Sender: TObject; Shift: TGraphicsShiftState;
    aX, aY: double);
var
    xRack: TLayoutElementSelectDlgRack;
    xFirstPos, xLastPos: string;
    xFirstPosInt, xLastPosInt: integer;
begin
    if not(gssLeft in Shift) or (GetKeyState(VK_CONTROL) < 0) then
        EXIT;

    xRack := self.Rack as TLayoutElementSelectDlgRack;

    if not Assigned(xRack.MultiSelectFirstWell) then
        EXIT;

    if Assigned(xRack.MultiSelectLastWell) then
    begin
        if self.WellNr = xRack.MultiSelectLastWell.WellNr then
            EXIT;
    end;

    xRack.MultiSelectLastWell := self;

    xRack.GetSelectedPositions(xRack.MultiSelectFirstWell, self, xFirstPosInt, xLastPosInt);
    xFirstPos := fOnGetEvents().ChooseRackFirstPos(xRack.Name, xFirstPosInt, xRack.Structure.Rows, 1, false);
    xLastPos := fOnGetEvents().ChooseRackLastPos(xRack.Name, xLastPosInt, xRack.Structure.Rows, 2, false);
    xRack.PaintAllPositions(xFirstPos, xLastPos);
end;

{ TLayoutElementSelectDlgRack }

constructor TLayoutElementSelectDlgRack.Create();
begin
    inherited Create();
end;

procedure TLayoutElementSelectDlgRack.DoInitGraphics;
begin
    inherited;

    self.Graphics.Callbacks.PopupCallback := self.DoPopup;
    self.Graphics.Callbacks.MouseUpCallback := self.DoMouseUp;

    if fOnGetEvents().CanChooseRack then
        self.Graphics.Callbacks.MouseDownCallback := DoMouseDown;
end;

procedure TLayoutElementSelectDlgRack.DoMouseDown(aSender: TObject; aButton: TGraphicsMouseButton;
    aShift: TGraphicsShiftState; aX, aY: double);
begin
    fOnGetEvents().ChooseRack(self.Name);
end;

procedure TLayoutElementSelectDlgRack.GetSelectedPositions(aFirstWell, aLastWell: TRackWell;
    out oFirstPosInt, oLastPosInt: integer);
var
    xPos1, xPos2: integer;
begin
    xPos1 := aFirstWell.WellNr;
    xPos2 := aLastWell.WellNr;
    oFirstPosInt := TMathUtils.MinIntValue([xPos1, xPos2]);
    oLastPosInt := TMathUtils.MaxIntValue([xPos1, xPos2]);
end;

procedure TLayoutElementSelectDlgRack.MultiSelectionDone();
var
    xRack: TLayoutElementSelectDlgRack;
    xFirstPosInt, xLastPosInt: integer;
begin
    xRack := self as TLayoutElementSelectDlgRack;
    if (not Assigned(xRack.MultiSelectFirstWell)) or (not Assigned(xRack.MultiSelectFirstWell)) then
        EXIT;

    xRack.GetSelectedPositions(xRack.MultiSelectFirstWell, xRack.MultiSelectLastWell, xFirstPosInt,
        xLastPosInt);
    fOnGetEvents().ChooseRackLastPos(xRack.Name, xLastPosInt, self.Structure.Rows, 2, true);
    xRack.MultiSelectFirstWell := nil;
end;

procedure TLayoutElementSelectDlgRack.PaintAllPositions(const aPositionArray: TArray<integer>);
var
    x: integer;
begin
    self.ClearAllTubePaint();

    for x := 0 to high(aPositionArray) do
        self.PaintTubePos(aPositionArray[x], TRackWellDisplayType.Highlight);
end;

procedure TLayoutElementSelectDlgRack.PaintAllPositions(const aFirstPosStr, aLastPosStr: string);
var
    xPosArray: TArray<integer>;
    x: integer;
begin
    self.ClearAllTubePaint();

    xPosArray := TMethodGUIParser.GetPositionArray(aFirstPosStr, aLastPosStr, self.Structure.Rows,
        self.Structure.Cols, 0);

    for x := 0 to high(xPosArray) do
        self.PaintTubePos(xPosArray[x], TRackWellDisplayType.Highlight);
end;

procedure TLayoutElementSelectDlgRack.DoMouseUp(aSender: TObject; aButton: TGraphicsMouseButton;
    aShift: TGraphicsShiftState; aX, aY: double);
begin
    MultiSelectionDone();
end;

procedure TLayoutElementSelectDlgRack.DoPopup(aSender: TObject; aX, aY: double;
    var vPopupMenuInfos: TPopupMenuInfoList);
begin
    vPopupMenuInfos := self.Graphics.PopupMenuInfos;
end;

function TLayoutElementSelectDlgRack.DoCreateRackWell(aWellNr: integer): TRackWell;
var
    xWell: TLayoutElementSelectDlgRackWell;
begin
    xWell := TLayoutElementSelectDlgRackWell.Create(aWellNr);
    xWell.OnGetEvents := fOnGetEvents;
    EXIT(xWell);
end;

{ TLayoutElementSelectDlgCarrierSlot }

procedure TLayoutElementSelectDlgCarrierSlot.DoInitGraphics;
begin
    inherited;
    if fOnGetEvents().CanChooseCarrierSlot then
        self.Graphics.Callbacks.MouseDownCallback := DoMouseDown;
end;

procedure TLayoutElementSelectDlgCarrierSlot.DoMouseDown(aSender: TObject; aButton: TGraphicsMouseButton;
    aShift: TGraphicsShiftState; aX, aY: double);
begin
    if not Assigned(self.Rack) then
        fOnGetEvents().ChooseCarrierSlot((self.Carrier as TCarrier).Name, self.SlotNr)
end;

{ TLayoutElementSelectDlgCarrier }

function TLayoutElementSelectDlgCarrier.DoCreateCarrierSlot: TCarrierSlot;
var
    xCarrierSlot: TLayoutElementSelectDlgCarrierSlot;
begin
    xCarrierSlot := TLayoutElementSelectDlgCarrierSlot.Create();
    xCarrierSlot.OnGetEvents := fOnGetEvents;
    EXIT(xCarrierSlot);
end;

{ TLayoutElementSelectDlgLayout }

function TLayoutElementSelectDlgLayout.DoCreateRack: TRack;
var
    xRack: TLayoutElementSelectDlgRack;
begin
    xRack := TLayoutElementSelectDlgRack.Create();
    xRack.OnGetEvents := fOnGetEvents;
    EXIT(xRack);
end;

function TLayoutElementSelectDlgLayout.DoCreateCarrier(): TCarrier;
var
    xCarrier: TLayoutElementSelectDlgCarrier;
begin
    xCarrier := TLayoutElementSelectDlgCarrier.Create();
    xCarrier.OnGetEvents := fOnGetEvents;
    EXIT(xCarrier);
end;

{ TLayoutElementSelectDlgLayoutManager }

constructor TLayoutElementSelectDlgLayoutManager.Create(aBackgroundGraphicsParent: TWinControl;
    aOnGetEvents: TLayoutElementSelectDlgGetEvents);
begin
    inherited Create(aBackgroundGraphicsParent);
    fOnGetEvents := aOnGetEvents;
end;

function TLayoutElementSelectDlgLayoutManager.DoCreateLayout(const aRunName, aLayoutName: string): TLayout;
var
    xLayout: TLayoutElementSelectDlgLayout;
begin
    xLayout := TLayoutElementSelectDlgLayout.Create(aLayoutName, aRunName);
    xLayout.OnGetEvents := fOnGetEvents;
    EXIT(xLayout);
end;

function TLayoutElementSelectDlgLayoutManager.GetTempSettingsSectionName: string;
begin
    result := 'LayoutElementSelectDlg';
end;


end.
