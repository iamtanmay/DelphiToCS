unit GeometricClasses;
{ --------------------------------------------------------------------------------------------------
  Copyright © 2003 Zinsser Analytic GmbH - All rights reserved.
  Author       : Wolfgang Lyncke (wl)
  Description  : simple and complex geometric classes
  --------------------------------------------------------------------------------------------------
  Revision History:
  date     op  method                            track-no improvement/change
  -------- --  ---------------------------       -------- -----------------------------------------------
  18.12.03 wl                                    TN1672   initial version
  19.12.03 wl  Cut-Funktionen                    TN1672   neu angelegt aber noch nicht funktiosfähig
  16.01.04 wl                                    TN1672   alle Klassen überarbeitet
  12.12.06 wl  TGeom2D_Position.TurnXY           TN3467   Drehung wie bei Vektor möglich
  12.12.06 wl  TGeom2D_Position.AddValues2D      TN3467   neu: Vektoraddition
  22.02.06 pk  TGeom2D_RangedRectangle.Destroy   TN3583   New
  28.11.07 wl  TGeomDistance.CreateAbsPos        TN3949   Parameter getauscht, um Compiler-Fehler zu vermeiden
  28.11.07 wl  TGeom2D_Rectangle.CreateAbsPos    TN3949   entfernt
  20.06.08 pk                                    TN4139   TMinimalCarrier changed to TCarrierBounds. No longer related to TCarrier
  -------------------------------------------------------------------------------------------------- }


interface


uses
    CommonTypes,
    Sysutils;

type
    ENegativeValueNotAllowed = Exception;

    TDirectionValType = (dvtX, dvtY, dvtZ);


    // Position in 1-dimensionalen Koordinatensystem
    // <---|--------------------------------|------------------------------------------------------>
    // 0                               Pos

    TGeomPosition = class
        FDirection: TDirectionValType;
        FPos: TPosMM;
    private
        function GetDirectionAsString: string;
    protected
        procedure SetValueExceptNegative(var aDest: TPosMM; aSource: TPosMM);
    public
        // Constructor/Destructor
        constructor Create(aDirection: TDirectionValType; aPos: TPosMM);
        // Properties
        property Direction: TDirectionValType read FDirection;
        property DirectionAsString: string read GetDirectionAsString;
        property Pos: TPosMM read FPos write FPos;
    end;

    // Streckenabschnitt in 1-dimensionalen Koordinatensystem
    // <---|--------------------------------|<===============>|------------------------------------>
    // 0                               Pos     Size      Pos2

    TGeomDistance = class(TGeomPosition)
    private
        FSize: TPosMM;
        function GetPos2: TPosMM;
    public
        // Constructor/Destructor
        constructor Create(aPos: TPosMM; aDirection: TDirectionValType; aSize: TPosMM);
        constructor CreateAbsPos(aAbsPos1, aAbsPos2: TPosMM; aDirection: TDirectionValType);
        constructor CreateNPos(aDirection: TDirectionValType; const aPositions: array of TPosMM);
        //
        procedure SetSize(const Value: TPosMM); // kann Exception verursachen!!
        // Properties
        property Size: TPosMM read FSize;
        property Pos2: TPosMM read GetPos2; // absolut
    end;

    // Streckenabschnitt mit zusätzlichem Bereich in beide Richtungen
    // <---|---------|<====================>|<===============>|<===============>|------------------>
    // 0     RangePos1    DownRange    Pos     Size      Pos2  UpRange   RangePos2

    TGeomRangedDistance = class(TGeomDistance)
    private
        FUpRange: TPosMM;
        FDownRange: TPosMM;
        procedure SetUpRange(const Value: TPosMM);
        procedure SetDownRange(const Value: TPosMM);
        function GetRangePos1: TPosMM;
        function GetRangePos2: TPosMM;
    public
        // Constructor/Destructor
        constructor Create(aDirection: TDirectionValType; aPos, aSize, aUpRange, aDownRange: TPosMM);
        //
        function ExtendRange(aAbsPos: TPosMM): boolean;
        function CutDownRange(aAbsPos: TPosMM): boolean;
        function CutUpRange(aAbsPos: TPosMM): boolean;
        function MoveInRange(aRelMove: TPosMM): TPosMM; // Rückgabewert: Rest
        // Properties
        property UpRange: TPosMM read FUpRange write SetUpRange;
        property DownRange: TPosMM read FDownRange write SetDownRange;
        property RangePos1: TPosMM read GetRangePos1;
        property RangePos2: TPosMM read GetRangePos2;
    end;

    // ------------------------------------------------------------------------------------------------
    TGeom2D_Position = class
    private
        FX: TGeomPosition;
        FY: TGeomPosition;
        procedure SetX(const Value: TPosMM);
        procedure SetY(const Value: TPosMM);
        function GetX: TPosMM;
        function GetY: TPosMM;
    public
        // Constructor/Destructor
        constructor Create(aX, aY: TPosMM);
        destructor Destroy; override;
        //
        procedure SetValues2D(aX, aY: TPosMM);
        procedure AddValues2D(aX, aY: TPosMM);
        procedure TurnXY(aRotation_Bog: extended); // Rotation im Bogenmaß
        // Properties
        property X: TPosMM read GetX write SetX;
        property Y: TPosMM read GetY write SetY;
    end;

    TGeom3D_Position = class(TGeom2D_Position)
    private
        FZ: TGeomPosition;
        function GetZ: TPosMM;
        procedure SetZ(const Value: TPosMM);
    public
        // Constructor/Destructor
        constructor Create(aX, aY, aZ: TPosMM);
        destructor Destroy; override;
        //
        procedure SetValues3D(aX, aY, aZ: TPosMM);
        procedure AddValues3D(aX, aY, aZ: TPosMM);
        //
        property Z: TPosMM read GetZ write SetZ;
    end;

    // ------------------------------------------------------------------------------------------------
    TRectangleData = record
        XPos: TPosMM;
        YPos: TPosMM;
        XPos2: TPosMM; // Absolut-Position
        YPos2: TPosMM;
    end;

    ERectangleOverlap = Exception;

    TGeom2D_Rectangle = class
    private
        FX: TGeomDistance;
        FY: TGeomDistance;
        function GetRectangle: TRectangleData;
    public
        // Constructor/Destructor
        constructor Create(aXPos, aYPos, aXSize, aYSize: TPosMM);
        constructor CreateNPos(const aXPositions, aYPositions: array of TPosMM);
        destructor Destroy; override;
        // public methods
        procedure SetPosition2D(aX, aY: TPosMM);
        // Properties
        property Rect: TRectangleData read GetRectangle;
    end;

    TCarrierBounds = class
    protected
        fValid: boolean;
        fRect: TRectangleData;
        procedure SetRect(const aRect: TRectangleData);
    public
        constructor Create();
        procedure ResetData();
        property Rect: TRectangleData read fRect write SetRect;
        property Valid: boolean read fValid;
    end;

    TCutWithRectType = (ctXCutDown, ctXCutUp, ctYCutDown, ctYCutUp);
    TCutWithRectTypes = set of TCutWithRectType;

    TGeom2D_RangedRectangle = class
    private
        FX: TGeomRangedDistance;
        FY: TGeomRangedDistance;

        FCarrXDown, FCarrXUp, FCarrYDown, FCarrYUp: TCarrierBounds;
        //
        function GetRectangle: TRectangleData;
        function GetRangeRect: TRectangleData;
        procedure SetRangeData(aRangeData: TXYRangeData);
        function GetRangeData: TXYRangeData;
        procedure SetRange(aXRel1, aXRel2, aYRel1, aYRel2: TPosMM);
    public
        // Constructor/Destructor
        constructor CreateRect(aPosX, aPosY, aXSize, aYSize: TPosMM);
        constructor Create(aPosX, aPosY, aSizeX, aSizeY, aXRel1, aYRel1, aXRel2, aYRel2: TPosMM);
        constructor CreateRData(aPosX, aPosY, aXSize, aYSize: TPosMM; aArmRange: TXYRangeData);
        constructor CreateByCopy(aTwin: TGeom2D_RangedRectangle);
        destructor Destroy(); override;
        // Public Methods
        procedure ExtendRangeXY(aAbsX, aAbsY: TPosMM);
        procedure CutRangeWithCarrier(const aRect: TRectangleData);
        procedure ResetRange(aAbsX1, aAbsY1, aAbsX2, aAbsY2: TPosMM);
        function MoveInRangeX(aRelMove: TPosMM; var aRest: TPosMM): TCarrierBounds; // Rückgabewert: Rest
        function MoveInRangeY(aRelMove: TPosMM; var aRest: TPosMM): TCarrierBounds; // Rückgabewert: Rest
        // Properties
        property Rect: TRectangleData read GetRectangle;
        property RangeRect: TRectangleData read GetRangeRect;
        property RangeData: TXYRangeData read GetRangeData write SetRangeData;
    end;


    // ##################################################################################################


implementation


uses
    Math;

{ TGeomPosition }

constructor TGeomPosition.Create(aDirection: TDirectionValType; aPos: TPosMM);
begin
    inherited Create;

    FDirection := aDirection;
    FPos := aPos;
end;

function TGeomPosition.GetDirectionAsString: string;
begin
    case (FDirection) of
        dvtX:
            result := 'X';
        dvtY:
            result := 'Y';
        dvtZ:
            result := 'Z';
    end;
end;

procedure TGeomPosition.SetValueExceptNegative(var aDest: TPosMM; aSource: TPosMM);
begin
    if (aSource < 0) then
    begin
        raise ENegativeValueNotAllowed.CreateFmt('%s-Value %d not allowed', [GetDirectionAsString, aSource]);
        exit;
    end;

    aDest := aSource;
end;

{ TGeomDistance }

constructor TGeomDistance.Create(aPos: TPosMM; aDirection: TDirectionValType; aSize: TPosMM);
begin
    CreateAbsPos(aPos, aPos + aSize, aDirection);
end;

constructor TGeomDistance.CreateAbsPos(aAbsPos1, aAbsPos2: TPosMM; aDirection: TDirectionValType);
begin
    inherited Create(aDirection, MinValue([aAbsPos1, aAbsPos2]));

    FSize := MaxValue([aAbsPos1, aAbsPos2]) - FPos; // So wird Size immer positiv!
end;

constructor TGeomDistance.CreateNPos(aDirection: TDirectionValType; const aPositions: array of TPosMM);
begin
    inherited Create(aDirection, MinValue(aPositions));

    FSize := MaxValue(aPositions) - FPos; // So wird Size immer positiv!
end;

function TGeomDistance.GetPos2: TPosMM;
begin
    result := FPos + FSize;
end;

procedure TGeomDistance.SetSize(const Value: TPosMM);
begin
    SetValueExceptNegative(FSize, Value); // Achtung: Negative Werte führen zu EXCEPTION!!!
end;

{ TGeomRangedDistance }

constructor TGeomRangedDistance.Create(aDirection: TDirectionValType;
    aPos, aSize, aUpRange, aDownRange: TPosMM);
begin
    inherited Create(aPos, aDirection, aSize);

    SetUpRange(aUpRange);
    SetDownRange(aDownRange);
end;

function TGeomRangedDistance.GetRangePos1: TPosMM;
begin
    result := FPos - FDownRange;
end;

function TGeomRangedDistance.GetRangePos2: TPosMM;
begin
    result := GetPos2 + FUpRange;
end;

procedure TGeomRangedDistance.SetDownRange(const Value: TPosMM);
begin
    FDownRange := Abs(Value); // Achtung: nimmt immer Absolutwert!
end;

procedure TGeomRangedDistance.SetUpRange(const Value: TPosMM);
begin
    FUpRange := Abs(Value); // Achtung: nimmt immer Absolutwert!
end;

function TGeomRangedDistance.ExtendRange(aAbsPos: TPosMM): boolean;
begin
    result := false;

    // Extend UpRange
    if (aAbsPos > GetRangePos2) then
    begin
        FUpRange := FUpRange + aAbsPos - GetRangePos2;
        result := true;
    end;
    // Extent DownRange
    if (aAbsPos < GetRangePos1) then
    begin
        FDownRange := FDownRange + GetRangePos1 - aAbsPos;
        result := true;
    end;
end;

function TGeomRangedDistance.CutDownRange(aAbsPos: TPosMM): boolean;
begin
    result := false;
    if (aAbsPos <= FPos) and (FPos - FDownRange < aAbsPos) then
    begin
        FDownRange := FPos - aAbsPos;
        result := true;
    end;
end;

function TGeomRangedDistance.CutUpRange(aAbsPos: TPosMM): boolean;
begin
    result := false;
    if (aAbsPos >= GetPos2) and (GetPos2 + FUpRange > aAbsPos) then
    begin
        FUpRange := aAbsPos - GetPos2;
        result := true;
    end;
end;

function TGeomRangedDistance.MoveInRange(aRelMove: TPosMM): TPosMM;
begin
    result := 0;
    if (aRelMove = 0) then
        exit;

    if (aRelMove > 0) then
    begin
        if (aRelMove > FUpRange) then
        begin
            result := aRelMove - FUpRange; // Rest, der nicht gefahren werden kann
            aRelMove := FUpRange;
        end;
        FPos := FPos + aRelMove;
        FUpRange := FUpRange - aRelMove;
        FDownRange := FDownRange + aRelMove;
    end;

    if (aRelMove < 0) then
    begin
        if (Abs(aRelMove) > FDownRange) then
        begin
            result := aRelMove + FDownRange; // Rest, der nicht gefahren werden kann
            aRelMove := -FDownRange;
        end;
        FPos := FPos + aRelMove;
        FUpRange := FUpRange - aRelMove;
        FDownRange := FDownRange + aRelMove;
    end;
end;

{ TGeom2D_Position }

constructor TGeom2D_Position.Create(aX, aY: TPosMM);
begin
    inherited Create;

    FX := TGeomPosition.Create(dvtX, aX);
    FY := TGeomPosition.Create(dvtY, aY);
end;

destructor TGeom2D_Position.Destroy;
begin
    FX.Free;
    FY.Free;

    inherited;
end;

function TGeom2D_Position.GetX: TPosMM;
begin
    result := FX.Pos;
end;

function TGeom2D_Position.GetY: TPosMM;
begin
    result := FY.Pos;
end;

procedure TGeom2D_Position.SetValues2D(aX, aY: TPosMM);
begin
    SetX(aX);
    SetY(aY);
end;

procedure TGeom2D_Position.SetX(const Value: TPosMM);
begin
    FX.Pos := Value;
end;

procedure TGeom2D_Position.SetY(const Value: TPosMM);
begin
    FY.Pos := Value;
end;

procedure TGeom2D_Position.AddValues2D(aX, aY: TPosMM);
begin
    // Vektoraddition
    FX.Pos := FX.Pos + aX;
    FY.Pos := FY.Pos + aY;
end;

procedure TGeom2D_Position.TurnXY(aRotation_Bog: extended);
var
    xXPos, xYPos: TPosMM;
begin
    // Wir drehen die 2D-Position um ihrer Ursprung (0,0)
    xXPos := Cos(aRotation_Bog) * FX.Pos - Sin(aRotation_Bog) * FY.Pos;
    xYPos := Sin(aRotation_Bog) * FX.Pos + Cos(aRotation_Bog) * FY.Pos;

    FX.Pos := xXPos;
    FY.Pos := xYPos;
end;

{ TGeom3D_Position }

constructor TGeom3D_Position.Create(aX, aY, aZ: TPosMM);
begin
    inherited Create(aX, aY);

    FZ := TGeomPosition.Create(dvtZ, aZ);
end;

destructor TGeom3D_Position.Destroy;
begin
    FZ.Free;

    inherited;
end;

procedure TGeom3D_Position.SetValues3D(aX, aY, aZ: TPosMM);
begin
    SetValues2D(aX, aY);
    SetZ(aZ);
end;

procedure TGeom3D_Position.SetZ(const Value: TPosMM);
begin
    FZ.Pos := Value;
end;

function TGeom3D_Position.GetZ: TPosMM;
begin
    result := FZ.Pos;
end;

procedure TGeom3D_Position.AddValues3D(aX, aY, aZ: TPosMM);
begin
    // Vektoraddition
    self.AddValues2D(aX, aY);
    FZ.Pos := FZ.Pos + aZ;
end;

{ TGeom2D_Rectangle }

constructor TGeom2D_Rectangle.Create(aXPos, aYPos, aXSize, aYSize: TPosMM);
begin
    inherited Create;

    FX := TGeomDistance.Create(aXPos, dvtX, aXSize);
    FY := TGeomDistance.Create(aYPos, dvtY, aYSize);
end;

constructor TGeom2D_Rectangle.CreateNPos(const aXPositions, aYPositions: array of TPosMM);
begin
    inherited Create;

    FX := TGeomDistance.CreateNPos(dvtX, aXPositions);
    FY := TGeomDistance.CreateNPos(dvtY, aYPositions);
end;

destructor TGeom2D_Rectangle.Destroy;
begin
    FX.Free;
    FY.Free;

    inherited;
end;

procedure TGeom2D_Rectangle.SetPosition2D(aX, aY: TPosMM);
begin
    FX.Pos := aX;
    FY.Pos := aY;
end;

function TGeom2D_Rectangle.GetRectangle: TRectangleData;
begin
    result.XPos := FX.Pos;
    result.XPos2 := FX.Pos2;
    result.YPos := FY.Pos;
    result.YPos2 := FY.Pos2;
end;

{ TGeom2D_RangedRectangle }

constructor TGeom2D_RangedRectangle.Create(aPosX, aPosY, aSizeX, aSizeY, aXRel1, aYRel1, aXRel2,
    aYRel2: TPosMM);
begin
    inherited Create;
    FCarrXDown := TCarrierBounds.Create;
    FCarrXUp := TCarrierBounds.Create;
    FCarrYDown := TCarrierBounds.Create;
    FCarrYUp := TCarrierBounds.Create;

    FX := TGeomRangedDistance.Create(dvtX, aPosX, aSizeX, aXRel1, aXRel2);
    FY := TGeomRangedDistance.Create(dvtY, aPosY, aSizeY, aYRel1, aYRel2);
end;

constructor TGeom2D_RangedRectangle.CreateRData(aPosX, aPosY, aXSize, aYSize: TPosMM;
    aArmRange: TXYRangeData);
begin
    Create(aPosX, aPosY, aXSize, aYSize, aArmRange.XRel1, aArmRange.YRel1, aArmRange.XRel2, aArmRange.YRel2);
end;

constructor TGeom2D_RangedRectangle.CreateRect(aPosX, aPosY, aXSize, aYSize: TPosMM);
begin
    Create(aPosX, aPosY, aXSize, aYSize, 0, 0, 0, 0);
end;

constructor TGeom2D_RangedRectangle.CreateByCopy(aTwin: TGeom2D_RangedRectangle);
begin
    CreateRData(aTwin.Rect.XPos, aTwin.Rect.YPos, aTwin.FX.Size, aTwin.FY.Size, aTwin.RangeData);
end;

destructor TGeom2D_RangedRectangle.Destroy();
begin
    fX.Free;
    fY.Free;
    FCarrXDown.Free;
    FCarrXUp.Free;
    FCarrYDown.Free;
    FCarrYUp.Free;
    inherited;
end;

procedure TGeom2D_RangedRectangle.SetRange(aXRel1, aXRel2, aYRel1, aYRel2: TPosMM);
begin
    FX.DownRange := aXRel1;
    FX.UpRange := aXRel2;
    FY.DownRange := aYRel1;
    FY.UpRange := aYRel2;
end;

function TGeom2D_RangedRectangle.GetRangeData: TXYRangeData;
begin
    result.XRel1 := FX.DownRange;
    result.XRel2 := FX.UpRange;
    result.YRel1 := FY.DownRange;
    result.YRel2 := FX.UpRange;
end;

procedure TGeom2D_RangedRectangle.SetRangeData(aRangeData: TXYRangeData);
begin
    SetRange(aRangeData.XRel1, aRangeData.XRel2, aRangeData.YRel1, aRangeData.YRel2)
end;

procedure TGeom2D_RangedRectangle.ExtendRangeXY(aAbsX, aAbsY: TPosMM);
begin
    FX.ExtendRange(aAbsX);
    FY.ExtendRange(aAbsY);
end;

function TGeom2D_RangedRectangle.GetRangeRect: TRectangleData;
begin
    result.XPos := FX.GetRangePos1;
    result.XPos2 := FX.GetRangePos2;
    result.YPos := FY.GetRangePos1;
    result.YPos2 := FY.GetRangePos2;
end;

procedure TGeom2D_RangedRectangle.CutRangeWithCarrier(const aRect: TRectangleData);
begin
    // Überlappen die Rechtecke?
    if (aRect.YPos < FY.Pos2) and (aRect.YPos2 > FY.Pos) and (aRect.XPos < FX.Pos2) and
        (aRect.XPos2 > FX.Pos) then
    begin
        raise ERectangleOverlap.Create('Rectangles are overlapping!');
        exit;
    end;

    // Hindernisse für die X-Bewegung:
    if (aRect.YPos < FY.Pos2) and (aRect.YPos2 > FY.Pos) then
    begin

        if FX.CutDownRange(aRect.XPos2) then
            FCarrXDown.Rect := aRect;
        if FX.CutUpRange(aRect.XPos) then
            FCarrXUp.Rect := aRect;
    end;

    // Hindernisse für die Y-Bewegung:
    if (aRect.XPos < FX.Pos2) and (aRect.XPos2 > FX.Pos) then
    begin

        if FY.CutDownRange(aRect.YPos2) then
            FCarrYDown.Rect := aRect;
        if FY.CutUpRange(aRect.YPos) then
            FCarrYUp.Rect := aRect;
    end;
end;

procedure TGeom2D_RangedRectangle.ResetRange(aAbsX1, aAbsY1, aAbsX2, aAbsY2: TPosMM);
begin
    SetRange(0, 0, 0, 0);
    ExtendRangeXY(aAbsX1, aAbsY1);
    ExtendRangeXY(aAbsX2, aAbsY2);

    FCarrXDown.ResetData();
    FCarrXUp.ResetData();
    FCarrYDown.ResetData();
    FCarrYUp.ResetData();
end;

function TGeom2D_RangedRectangle.MoveInRangeX(aRelMove: TPosMM; var aRest: TPosMM): TCarrierBounds;
begin
    aRest := FX.MoveInRange(aRelMove);
    result := nil;
    if (aRelMove < 0) and FCarrXDown.Valid then
        result := FCarrXDown;

    if (aRelMove > 0) and FCarrXUp.Valid then
        result := FCarrXUp;
end;

function TGeom2D_RangedRectangle.MoveInRangeY(aRelMove: TPosMM; var aRest: TPosMM): TCarrierBounds;
begin
    aRest := FY.MoveInRange(aRelMove);
    result := nil;

    if (aRelMove < 0) and FCarrYDown.Valid then
        result := FCarrYDown;

    if (aRelMove > 0) and FCarrYUp.Valid then
        result := FCarrYUp;
end;

function TGeom2D_RangedRectangle.GetRectangle: TRectangleData;
begin
    result.XPos := FX.Pos;
    result.XPos2 := FX.Pos2;
    result.YPos := FY.Pos;
    result.YPos2 := FY.Pos2;
end;

{ TCarrierBounds }

constructor TCarrierBounds.Create;
begin
    inherited;
    ResetData();
end;

procedure TCarrierBounds.ResetData;
begin
    fValid := false;
    fRect.XPos := 0;
    fRect.YPos := 0;
    fRect.XPos2 := 0;
    fRect.YPos2 := 0;
end;

procedure TCarrierBounds.SetRect(const aRect: TRectangleData);
begin
    fValid := true;
    fRect := aRect;
end;


end.
