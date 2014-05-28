{ --------------------------------------------------------------------------------------------------
  Copyright © 2003 Zinsser Analytic GmbH - All rights reserved.
  Author       : Wolfgang Lyncke (wl)
  Description  : class that uses the workbench for creating XY-moves with corridors and barriers
  --------------------------------------------------------------------------------------------------
  Revision History:
  date     op  method                       track-no improvement/change
  -------- --  ---------------------------  -------- -----------------------------------------------
  15.12.03 wl                               TN1672   initial version
  16.12.03 wl                               TN1672   weiter ausgebaut
  19.12.03 wl  TRangeEmbeddedRectangle      TN1672   --> GeometricClasses
  16.01.04 wl  TXYRouteFinder               TN1672   1. lauffähige Version - für Hindernisse in X bereits getestet
  20.01.04 wl                               TN1672   beim Testen überarbeitet
  06.02.04 wl  TXYRouteFinder.CreateRouteRel TN1672  Berechnung der Route korrigiert
  02.03.04 wl  TXYRouteFinder.Create        TN1672   new prameters XMin, YMin
  02.03.04 wl  TXYRouteFinder.ResetCurrentRange TN1672   XMin and YMin used for calculation
  26.10.04 wl  TXYRouteFinder.Create        TN1906   Min/Max-Werte werden überprüft - der kleinere Wert ist immer Min
  07.04.06 pk  AnyRelevantBarrier           TN3028   A move inside of a barrier does not count as a relevant barrier move
  31.01.07 wl                               TN3532   benutzt TCarrier-property SlotGroupType statt SlotGroup.TYP
  20.06.08 pk                               TN4139   TMinimalCarrier changed to TCarrierBounds
  13.01.09 wl  TXYRoute                     TN4386   uses default property
  13.04.10 wl                               TN5044   uses geändert
  27.02.13 wl                               TN6045   uses Generics.Collections
  -------------------------------------------------------------------------------------------------- }

unit XYRouteFinder;


interface


uses
    Generics.Collections,
    CommonTypes,
    GeometricClasses;

type
    TXYRoute = class
    protected
        FList: TObjectList<TGeom2D_Position>;
    private
        function GetCount: integer;
        function GetItem(aIndex: Integer): TGeom2D_Position;
        function Add(aItem: TGeom2D_Position): Integer;
    public
        // Constructor/Destructor
        constructor Create(aCurrentX, aCurrentY: TPosMM);
        destructor Destroy; override;
        // Public Methods
        function AddAbsMove(aAbsX, aAbsY: TPosMM): Integer;
        function AddRelMove(aRelX, aRelY: TPosMM): Integer;
        // Properties
        property Count: integer read GetCount;
        property this[index: Integer]: TGeom2D_Position read GetItem; default;
    end;

    TXYRouteFinder = class
    private
        FXMin, FYMin: TPosMM;
        FXMax, FYMax: TPosMM;
        procedure ResetCurrentRange(aArm: TGeom2D_RangedRectangle; aArmRange: TXYRangeData);
        procedure CreateRouteRel(aXYRoute: TXYRoute; aArm: TGeom2D_RangedRectangle; aArmRange: TXYRangeData;
            aRelDestX, aRelDestY: TPosMM; aUseCorridor: boolean);
        function RectContainsPoint(const aRect: TRectangleData; aX, aY: TPosMM): boolean;
    public
        // Constructor/Destructor
        constructor Create(aXMin, aYMin, aXMax, aYMax: TPosMM);
        // Public Methods
        procedure CreateRoute(aXYRoute: TXYRoute; aArmRange: TXYRangeData; aDestX, aDestY: TPosMM;
            aUseCorridor: boolean);
        function AnyRelevantItem(aArmRange: TXYRangeData; aCurrentX, aCurrentY, aDestX, aDestY: TPosMM;
            aUseCorridor: boolean): boolean;
        function AnyRelevantBarrier(const aWay: TRectangleData;
            aCurrentX, aCurrentY, aDestX, aDestY: TPosMM): boolean;
        function AnyRelevantCorridor(aWay: TRectangleData): boolean;
    end;


implementation


uses
    MathUtils,
    SysUtils,
    AppTypes,
    Carrier,
    LayoutManager,
    ErrorManager,
    LogManager;

{ TXYRoute }

constructor TXYRoute.Create(aCurrentX, aCurrentY: TPosMM);
begin
    inherited Create;

    FList := TObjectList<TGeom2D_Position>.Create;
    FList.OwnsObjects := true;

    Add(TGeom2D_Position.Create(aCurrentX, aCurrentY)); // Add current position as first position
end;

destructor TXYRoute.Destroy;
begin
    FList.Free;

    inherited;
end;

function TXYRoute.Add(aItem: TGeom2D_Position): Integer;
begin
    result := -1;
    if Assigned(aItem) then
        result := FList.Add(aItem);
end;

function TXYRoute.GetCount: integer;
begin
    result := FList.Count;
end;

function TXYRoute.GetItem(aIndex: Integer): TGeom2D_Position;
begin
    result := FList.Items[aIndex] as TGeom2D_Position;
end;

function TXYRoute.AddAbsMove(aAbsX, aAbsY: TPosMM): Integer;
begin
    if (Count = 0) then
        result := Add(TGeom2D_Position.Create(aAbsX, aAbsY)) // Add current position as first position
    else
        result := AddRelMove(aAbsX - self[GetCount - 1].X, aAbsY - self[GetCount - 1].Y);
end;

function TXYRoute.AddRelMove(aRelX, aRelY: TPosMM): Integer;
begin
    result := -1;

    if (Count > 0) and ((aRelX <> 0) or (aRelY <> 0)) then
    begin

        // if the last movement had the same direction do not add a new step
        if (Count > 1) then
        begin

            if (aRelY = 0) and (self[GetCount - 1].Y - self[GetCount - 2].Y = 0) then
            begin
                self[GetCount - 1].X := self[GetCount - 1].X + aRelX; // only change x-value
                result := GetCount - 1;
                exit;
            end;

            if (aRelX = 0) and (self[GetCount - 1].X - self[GetCount - 2].X = 0) then
            begin
                self[GetCount - 1].Y := self[GetCount - 1].Y + aRelY; // only change y-value
                result := GetCount - 1;
                exit;
            end;
        end;

        result := Add(TGeom2D_Position.Create(self[GetCount - 1].X + aRelX, self[GetCount - 1].Y + aRelY));
        // Add new step
    end;
end;

{ TXYRouteFinder }

constructor TXYRouteFinder.Create(aXMin, aYMin, aXMax, aYMax: TPosMM);
begin
    inherited Create;

    FXMin := TMathUtils.MinDoubleValue([aXMin, aXMax]);
    FXMax := TMathUtils.MaxDoubleValue([aXMin, aXMax]);
    FYMin := TMathUtils.MinDoubleValue([aYMin, aYMax]);
    FYMax := TMathUtils.MaxDoubleValue([aYMin, aYMax]);
end;

procedure TXYRouteFinder.ResetCurrentRange(aArm: TGeom2D_RangedRectangle; aArmRange: TXYRangeData);
var
    x: integer;
    xCarrier: TCarrier;
begin
    // zunächt die gesamte befahrbare Arbeitsfläche!
    aArm.ResetRange(FXMin - aArmRange.XRel1, FYMin - aArmRange.YRel1, FXMax + aArmRange.XRel2,
        FYMax + aArmRange.YRel2);

    for x := 0 to TLayoutManager.Instance.CurrentLayout.NoOfCarrier - 1 do
    begin
        xCarrier := TLayoutManager.Instance.CurrentLayout.Carriers[x];
        if (xCarrier.SlotGroupType = sgtBarrier) then
        begin
            try
                // Barrier von Range abziehen, wenn er im Weg steht
                aArm.CutRangeWithCarrier(xCarrier.RectData);
            except
                on ERectangleOverlap do
                begin
                    gErrorManager.SetGlobalErr(ERR_USER, Format('Movement blocked by (barrier) carrier %s!',
                        [xCarrier.Name]), true);
                    exit;
                end;
            end;
        end;
    end;
end;

procedure TXYRouteFinder.CreateRoute(aXYRoute: TXYRoute; aArmRange: TXYRangeData; aDestX, aDestY: TPosMM;
    aUseCorridor: boolean);
var
    xArm: TGeom2D_RangedRectangle;
    xCurrentX, xCurrentY, xRelDestX, xRelDestY: TPosMM;
begin
    if (aXYRoute.Count = 0) then
        exit;
    xCurrentX := aXYRoute[0].X;
    xCurrentY := aXYRoute[0].Y;

    xArm := TGeom2D_RangedRectangle.CreateRect(xCurrentX - aArmRange.XRel1, xCurrentY - aArmRange.YRel1,
        aArmRange.XRel1 + aArmRange.XRel2, aArmRange.YRel1 + aArmRange.YRel2);
    try
        xRelDestX := aDestX - xCurrentX;
        xRelDestY := aDestY - xCurrentY;
        CreateRouteRel(aXYRoute, xArm, aArmRange, xRelDestX, xRelDestY, aUseCorridor);
    finally
        xArm.Free;
    end;
end;

procedure TXYRouteFinder.CreateRouteRel(aXYRoute: TXYRoute; aArm: TGeom2D_RangedRectangle;
    aArmRange: TXYRangeData; aRelDestX, aRelDestY: TPosMM; aUseCorridor: boolean);
var
    xArmTry1, xArmTry2: TGeom2D_RangedRectangle;
    xRest, xRestXTry1, xRestYTry1, xRestXTry2, xRestYTry2: TPosMM;
    xNoOfMoves: integer;
    xCarrierBounds: TCarrierBounds;

    function MoveInX(aRelMove: TPosMM; var aRest: TPosMM): TCarrierBounds; // Rückgabewert: Rest
    begin
        ResetCurrentRange(aArm, aArmRange);
        result := aArm.MoveInRangeX(aRelMove, aRest);

        if (aRest <> aRelMove) then
        begin
            aRelDestX := aRelDestX - aRelMove + aRest;
            aXYRoute.AddRelMove(aRelMove - aRest, 0);
        end;
    end;

    function MoveInY(aRelMove: TPosMM; var aRest: TPosMM): TCarrierBounds; // Rückgabewert: Rest
    begin
        ResetCurrentRange(aArm, aArmRange);
        result := aArm.MoveInRangeY(aRelMove, aRest);

        if (aRest <> aRelMove) then
        begin
            aRelDestY := aRelDestY - aRelMove + aRest;
            aXYRoute.AddRelMove(0, aRelMove - aRest);
        end;
    end;

begin
    repeat
        if (aRelDestX = 0) and (aRelDestY = 0) then
            break; // Ziel erreicht!!!

        xNoOfMoves := aXYRoute.Count;

        if (aRelDestX <> 0) then
        begin
            // auf gut Glück in x-Richtung
            xCarrierBounds := MoveInX(aRelDestX, xRest);

            if (xRest <> 0) and Assigned(xCarrierBounds) then
            begin // 2 weitere Schritte notwendig (erst Y, dann X)

                // 1. Versuch: Nach oben!
                xArmTry1 := TGeom2D_RangedRectangle.CreateByCopy(aArm);
                ResetCurrentRange(xArmTry1, aArmRange);
                xArmTry1.MoveInRangeY(xCarrierBounds.Rect.YPos - xArmTry1.Rect.YPos2, xRestYTry1);
                if (xRestYTry1 = 0) then
                begin
                    ResetCurrentRange(xArmTry1, aArmRange);
                    xArmTry1.MoveInRangeX(aRelDestX, xRestXTry1);
                end;

                // 2. Versuch: Nach unten!
                xArmTry2 := TGeom2D_RangedRectangle.CreateByCopy(aArm);
                ResetCurrentRange(xArmTry2, aArmRange);
                xArmTry2.MoveInRangeY(xCarrierBounds.Rect.YPos2 - xArmTry2.Rect.YPos, xRestYTry2);
                if (xRestYTry2 = 0) then
                begin
                    ResetCurrentRange(xArmTry2, aArmRange);
                    xArmTry2.MoveInRangeX(aRelDestX, xRestXTry2);
                end;

                // Welch Richtung ist besser?
                if (xRestYTry1 <> 0) and (xRestYTry2 <> 0) then
                begin
                    gErrorManager.SetGlobalErr(ERR_USER, 'Y-Movement: Es geht nicht weiter', true);
                end
                else
                begin
                    if (xRestYTry1 = 0) and ((xRestYTry2 <> 0) // 2. Try failed
                        or (Abs(xRestXTry1) <= Abs(xRestXTry2))) then
                    begin // 1.Versuch ist dichter dran

                        // 1.Weg durchführen
                        MoveInY(xCarrierBounds.Rect.YPos - aArm.Rect.YPos2, xRest);
                        if (xRest = 0) then
                            MoveInX(aRelDestX, xRest)
                        else
                            gErrorManager.SetGlobalErr(ERR_USER, 'XY-Movement: Darf nicht sein!(1)', true);
                    end
                    else
                    begin
                        // 2.Weg durchführen
                        MoveInY(xCarrierBounds.Rect.YPos2 - aArm.Rect.YPos, xRest);
                        if (xRest = 0) then
                            MoveInX(aRelDestX, xRest)
                        else
                            gErrorManager.SetGlobalErr(ERR_USER, 'XY-Movement: Darf nicht sein!(2)', true);
                    end;
                end;

                xArmTry1.Free;
                xArmTry2.Free;
            end;
        end;

        if (aRelDestY <> 0) then
        begin
            // auf gut Glück in y-Richtung
            xCarrierBounds := MoveInY(aRelDestY, xRest);

            if (xRest <> 0) and Assigned(xCarrierBounds) then
            begin // 2 weitere Schritte notwendig (erst X, dann Y)

                // 1. Versuch: Nach rechts!
                xArmTry1 := TGeom2D_RangedRectangle.CreateByCopy(aArm);
                ResetCurrentRange(xArmTry1, aArmRange);
                xArmTry1.MoveInRangeX(xCarrierBounds.Rect.XPos - xArmTry1.Rect.XPos2, xRestXTry1);
                if (xRestXTry1 = 0) then
                begin
                    ResetCurrentRange(xArmTry1, aArmRange);
                    xArmTry1.MoveInRangeY(aRelDestY, xRestYTry1);
                end;

                // 2. Versuch: Nach links!
                xArmTry2 := TGeom2D_RangedRectangle.CreateByCopy(aArm);
                ResetCurrentRange(xArmTry2, aArmRange);
                xArmTry2.MoveInRangeX(xCarrierBounds.Rect.XPos2 - xArmTry2.Rect.XPos, xRestXTry2);
                if (xRestXTry2 = 0) then
                begin
                    ResetCurrentRange(xArmTry2, aArmRange);
                    xArmTry2.MoveInRangeY(aRelDestY, xRestYTry2);
                end;

                // Welch Richtung ist besser?
                if (xRestXTry1 <> 0) and (xRestXTry2 <> 0) then
                begin
                    gErrorManager.SetGlobalErr(ERR_USER, 'X-Movement: Es geht nicht weiter', true);
                end
                else
                begin
                    if (xRestXTry1 = 0) and ((xRestXTry2 <> 0) // 2. Try failed
                        or (Abs(xRestYTry1) <= Abs(xRestYTry2))) then
                    begin // 1.Versuch ist dichter dran

                        // 1.Weg durchführen
                        MoveInX(xCarrierBounds.Rect.XPos - aArm.Rect.XPos2, xRest);
                        if (xRest = 0) then
                            MoveInY(aRelDestY, xRest)
                        else
                            gErrorManager.SetGlobalErr(ERR_USER, 'XY-Movement: Darf nicht sein(3)!', true);
                    end
                    else
                    begin
                        // 2.Weg durchführen
                        MoveInX(xCarrierBounds.Rect.XPos2 - aArm.Rect.XPos, xRest);
                        if (xRest = 0) then
                            MoveInY(aRelDestY, xRest)
                        else
                            gErrorManager.SetGlobalErr(ERR_USER, 'XY-Movement: Darf nicht sein(4)!', true);
                    end;
                end;

                xArmTry1.Free;
                xArmTry2.Free;
            end;
        end;

        if (xNoOfMoves = aXYRoute.Count) and (not gErrorManager.IsGlobalErr) then
            // keine neuen Schritte hinzugekommen
            gErrorManager.SetGlobalErr(ERR_USER, 'XY-Movement: Barrier Deadlock', true);

    until (gErrorManager.IsGlobalErr);
end;

function TXYRouteFinder.RectContainsPoint(const aRect: TRectangleData; aX, aY: TPosMM): boolean;
begin
    result := ((aRect.XPos <= aX) and (aX <= aRect.XPos2)) and ((aRect.YPos <= aY) and (aY <= aRect.YPos2));
end;

function TXYRouteFinder.AnyRelevantBarrier(const aWay: TRectangleData;
    aCurrentX, aCurrentY, aDestX, aDestY: TPosMM): boolean;
var
    x: integer;
    xCarrRect: TRectangleData;
begin
    result := false;
    // Find Barriers

    for x := 0 to TLayoutManager.Instance.CurrentLayout.NoOfCarrier - 1 do
    begin
        if (TLayoutManager.Instance.CurrentLayout.Carriers[x].SlotGroupType <> sgtBarrier) then
            CONTINUE;

        xCarrRect := TLayoutManager.Instance.CurrentLayout.Carriers[x].RectData;
        // if we are moving into or out of a barrier, Skip it
        if RectContainsPoint(xCarrRect, aCurrentX, aCurrentY) or
            RectContainsPoint(xCarrRect, aDestX, aDestY) then
            CONTINUE;

        // ist das Hindenis relevant für die Bewegung?
        if (xCarrRect.XPos2 > aWay.XPos) and (xCarrRect.XPos < aWay.XPos2) and (xCarrRect.YPos2 > aWay.YPos)
            and (xCarrRect.YPos < aWay.YPos2) then
        begin
            result := true;
            exit;
        end;

    end;

end;

function TXYRouteFinder.AnyRelevantCorridor(aWay: TRectangleData): boolean;
var
    x: integer;
    xCarrRect: TRectangleData;
begin
    result := false;
    // Find Barriers

    for x := 0 to TLayoutManager.Instance.CurrentLayout.NoOfCarrier - 1 do
    begin
        if (TLayoutManager.Instance.CurrentLayout.Carriers[x].SlotGroupType = sgtCorridor) then
        begin

            // ist das Hindenis relevant für die Bewegung?
            xCarrRect := TLayoutManager.Instance.CurrentLayout.Carriers[x].RectData;
            if (xCarrRect.XPos2 > aWay.XPos) and (xCarrRect.XPos < aWay.XPos2) then
            begin
                // in Y kann der Korridor auch außerhalb des Bereichs liegen
                result := true;
                exit;
            end;

        end;
    end;

end;

function TXYRouteFinder.AnyRelevantItem(aArmRange: TXYRangeData; aCurrentX, aCurrentY, aDestX, aDestY: TPosMM;
    aUseCorridor: boolean): boolean;
var
    xCurrent, xDest, xOuterRect: TGeom2D_Rectangle;
begin
    xCurrent := TGeom2D_Rectangle.Create(aCurrentX - aArmRange.XRel1, aCurrentY - aArmRange.YRel1,
        aArmRange.XRel1 + aArmRange.XRel2, aArmRange.YRel1 + aArmRange.YRel2);
    try
        xDest := TGeom2D_Rectangle.Create(aDestX - aArmRange.XRel1, aDestY - aArmRange.YRel1,
            aArmRange.XRel1 + aArmRange.XRel2, aArmRange.YRel1 + aArmRange.YRel2);
        try
            xOuterRect := TGeom2D_Rectangle.CreateNPos([xCurrent.Rect.XPos, xCurrent.Rect.XPos2,
                xDest.Rect.XPos, xDest.Rect.XPos2], [xCurrent.Rect.YPos, xCurrent.Rect.YPos2, xDest.Rect.YPos,
                xDest.Rect.YPos2]);
            try
                result := AnyRelevantBarrier(xOuterRect.Rect, aCurrentX, aCurrentY, aDestX, aDestY);
                if (not result) then
                    result := AnyRelevantCorridor(xOuterRect.Rect);
            finally
                xOuterRect.Free;
            end;
        finally
            xDest.Free;
        end;
    finally
        xCurrent.Free;
    end;
end;


end.
