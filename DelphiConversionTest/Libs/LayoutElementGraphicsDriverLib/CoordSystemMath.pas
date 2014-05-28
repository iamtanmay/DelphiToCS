unit CoordSystemMath;


{ --------------------------------------------------------------------------------------------------
  Copyright © 2008 Zinsser Analytic GmbH - All rights reserved.
  Author       : Payman Kamali (pk)
  Description  : Classes and math functions for Coordinate System Relation
  --------------------------------------------------------------------------------------------------
  Revision History:
  date     op  method                       track-no improvement/change
  -------- --  ---------------------------  -------- -----------------------------------------------
  23.06.08 pk                                TN4139  Initial Revision
  02.07.08 pk  Assign                        TN4139  also copies Zoom
  07.07.08 pk  fTranslateSizeZ               TN4139  New
  19.09.08 pk  IncreaseZHeight               TN4215  New
  11.03.09 pk  CalcTransformMatrix           TN4457  ReflectX implemented
  -------------------------------------------------------------------------------------------------- }
interface


uses
    Types,
    MatrixMath;

type

    TCoordSystemRotateType = double;

    TCoordSystemRelation = class
    private
        fTranslateX: double;
        fTranslateY: double;
        fTranslateZ: double;
        fTranslateSizeZ: double;
        fReflectX: boolean;
        fReflectY: boolean;
        fReflectZ: boolean;
        fRotateX: TCoordSystemRotateType;
        fRotateY: TCoordSystemRotateType;
        fRotateZ: TCoordSystemRotateType;
        fRotateZOffsetX, fRotateZOffsetY: double;
        fZoom: double;
        function GetTotalTranslateZ(): double;
    public
        constructor Create();
        procedure Assign(aOtherCoordSystemRelation: TCoordSystemRelation);
        procedure RotateZAround(aRotateZ: TCoordSystemRotateType; aOffsetX, aOffsetY: double);
        property TranslateX: double read fTranslateX write fTranslateX;
        property TranslateY: double read fTranslateY write fTranslateY;
        property TranslateZ: double read fTranslateZ write fTranslateZ;
        property TranslateSizeZ: double read fTranslateSizeZ write fTranslateSizeZ;
        property ReflectX: boolean read fReflectX write fReflectX;
        property ReflectY: boolean read fReflectY write fReflectY;
        property ReflectZ: boolean read fReflectZ write fReflectZ;
        property RotateX: TCoordSystemRotateType read fRotateX write fRotateX;
        property RotateY: TCoordSystemRotateType read fRotateY write fRotateY;
        property RotateZ: TCoordSystemRotateType read fRotateZ write fRotateZ;
        property RotateZOffsetX: double read fRotateZOffsetX write fRotateZOffsetX;
        property RotateZOffsetY: double read fRotateZOffsetY write fRotateZOffsetY;
        property Zoom: double read fZoom write fZoom;
        property TotalTranslateZ: double read GetTotalTranslateZ;
    end;

    TMatrixHandle = class
    private
        fMatrix: TMatrix4d;
    public
        constructor Create();
        property Matrix: TMatrix4d read fMatrix write fMatrix;
    end;

    TZHeightCompareType = (zhcGreater, zhcLess, zhcEqual);

    TCoordCalculator = class
    private
        fCoordSystemRelation: TCoordSystemRelation;
        fParentCoordCalculator: TCoordCalculator;
        fMatrixHandle: TMatrixHandle;
        fInvMatrixHandle: TMatrixHandle;
        fRecalcParent: boolean;
        fRecalcInvParent: boolean;
        function CalcMultMatrix(const aLocalMatrix, aParentMatrix: TMatrix4d): TMatrix4d;
        function GetParentMatrix: TMatrix4d;
        function GetInvParentMatrix(): TMatrix4d;
        class function FindRect(aP1, aP2: TPoint4d; aScaleFactor: double;
            out oReverseX, oReverseY: boolean): TRect;
        class function FindXYZ(aP1, aP2: TPoint4d; out oReverseX, oReverseY, oReverseZ: boolean): TPoint4d;
        function IsZoomed: boolean;
    public
        constructor Create();
        destructor Destroy; override;
        function CalcTransformMatrix(): TMatrix4d;
        function CalcInvTransformMatrix: TMatrix4d;
        procedure CalcCoordMatrix();
        procedure CalcInvCoordMatrix();
        function TransformPoint(aPoint: TPoint4d): TPoint4d;
        class function CalcTransformedRect(aSizeX, aSizeY: double; const aMatrix: TMatrix4d;
            aScaleFactor: double; out oReverseX, oReverseY: boolean): TRect;
        class function CalcTransformedPoint(const aMatrix: TMatrix4d): TPoint4d;
        class function ReOrderValues(var vValue1, vValue2: double): boolean;
        class function IncreaseZHeight(aCurrentValue, aMagnitude: double): double;
        class function DecreaseZHeight(aCurrentValue, aMagnitude: double): double;
        class function CompareZHeight(aHeight1, aHeight2: double): TZHeightCompareType;
        property MatrixHandle: TMatrixHandle read fMatrixHandle;
        property InvMatrixHandle: TMatrixHandle read fInvMatrixHandle;
        property CoordSystem: TCoordSystemRelation read fCoordSystemRelation;
        property ParentCoordCalculator: TCoordCalculator read fParentCoordCalculator
            write fParentCoordCalculator;
        property RecalcParent: boolean read fRecalcParent write fRecalcParent;
        property RecalcInvParent: boolean read fRecalcInvParent write fRecalcInvParent;
    end;


implementation


const
    cZoomNone = 1.0;

    cUpDirectionIsPositive = true;
    // could also be false this should be a variable like in versions before 7.3.0

procedure TCoordSystemRelation.Assign(aOtherCoordSystemRelation: TCoordSystemRelation);
begin
    fTranslateX := aOtherCoordSystemRelation.TranslateX;
    fTranslateY := aOtherCoordSystemRelation.TranslateY;
    fTranslateZ := aOtherCoordSystemRelation.TranslateZ;
    fTranslateSizeZ := aOtherCoordSystemRelation.TranslateSizeZ;
    fReflectX := aOtherCoordSystemRelation.ReflectX;
    fReflectY := aOtherCoordSystemRelation.ReflectY;
    fReflectZ := aOtherCoordSystemRelation.ReflectZ;
    fRotateX := aOtherCoordSystemRelation.RotateX;
    fRotateY := aOtherCoordSystemRelation.RotateY;
    fRotateZ := aOtherCoordSystemRelation.RotateZ;
    fRotateZOffsetX := aOtherCoordSystemRelation.RotateZOffsetX;
    fRotateZOffsetY := aOtherCoordSystemRelation.RotateZOffsetY;
    fZoom := aOtherCoordSystemRelation.Zoom;
end;

constructor TCoordSystemRelation.Create();
begin
    inherited Create();
    fTranslateX := 0;
    fTranslateY := 0;
    fTranslateZ := 0;
    fTranslateSizeZ := 0;
    fReflectX := false;
    fReflectY := false;
    fReflectZ := false;
    fRotateX := 0;
    fRotateY := 0;
    fRotateZ := 0;
    fRotateZOffsetX := 0;
    fRotateZOffsetY := 0;
    fZoom := cZoomNone;
end;

function TCoordSystemRelation.GetTotalTranslateZ(): double;
begin
    result := fTranslateZ + fTranslateSizeZ;
end;

{ TMatrixHandle }

constructor TMatrixHandle.Create;
begin
    inherited Create();
    fMatrix := GetIdentMatrix();
end;

{ TCoordCalculator }

constructor TCoordCalculator.Create;
begin
    inherited Create();
    fMatrixHandle := TMatrixHandle.Create();
    fInvMatrixHandle := TMatrixHandle.Create();
    fCoordSystemRelation := TCoordSystemRelation.Create();
    fParentCoordCalculator := nil;
    fRecalcParent := true;
    fRecalcInvParent := true;
end;

destructor TCoordCalculator.Destroy;
begin
    fMatrixHandle.Free;
    fInvMatrixHandle.Free;
    fCoordSystemRelation.Free;
    inherited;
end;

function TCoordCalculator.IsZoomed(): boolean;
begin
    result := (fCoordSystemRelation.fZoom > cZoomNone) or (fCoordSystemRelation.fZoom < cZoomNone);
end;

function TCoordCalculator.CalcTransformMatrix(): TMatrix4d;
var
    xML: TMatrix4d;
begin

    xML := GetIdentMatrix();

    if self.IsZoomed() then
    begin
        xML := GetMulMatrix(xML, CreateScaleMatrix(fCoordSystemRelation.fZoom, fCoordSystemRelation.fZoom,
            fCoordSystemRelation.fZoom));
    end;

    if (fCoordSystemRelation.TranslateX <> 0) or (fCoordSystemRelation.TranslateY <> 0) or
        (fCoordSystemRelation.TotalTranslateZ <> 0) then
    begin
        xML := GetMulMatrix(xML, CreateTranslationMatrix(MakePoint4d(fCoordSystemRelation.TranslateX,
            fCoordSystemRelation.TranslateY, fCoordSystemRelation.TotalTranslateZ)));
    end;

    if fCoordSystemRelation.RotateZ <> 0 then
    begin
        xML := GetMulMatrix(xML, CreateRotationMatrixZ(fCoordSystemRelation.RotateZ * Pi / 180));
        xML := GetMulMatrix(xML, CreateTranslationMatrix(MakePoint4d(fCoordSystemRelation.RotateZOffsetX,
            fCoordSystemRelation.RotateZOffsetY, 0)));
    end;

    if fCoordSystemRelation.ReflectY then
    begin
        xML := GetMulMatrix(xML, CreateReflectionMatrixX());
    end;
    if fCoordSystemRelation.ReflectX then
    begin
        xML := GetMulMatrix(xML, CreateReflectionMatrixY());
    end;
    result := xML;

end;

function TCoordCalculator.CalcInvTransformMatrix(): TMatrix4d;
var
    xML: TMatrix4d;
begin

    xML := GetIdentMatrix();
    if fCoordSystemRelation.ReflectY then
    begin
        xML := GetMulMatrix(xML, CreateReflectionMatrixX());
    end;

    if fCoordSystemRelation.RotateZ <> 0 then
    begin
        xML := GetMulMatrix(xML, CreateTranslationMatrix(MakePoint4d(-fCoordSystemRelation.RotateZOffsetX,
            -fCoordSystemRelation.RotateZOffsetY, 0)));
        xML := GetMulMatrix(xML, CreateRotationMatrixZ(-fCoordSystemRelation.RotateZ * Pi / 180));
    end;

    if (fCoordSystemRelation.TranslateX <> 0) or (fCoordSystemRelation.TranslateY <> 0) or
        (fCoordSystemRelation.TotalTranslateZ <> 0) then
    begin
        xML := GetMulMatrix(xML, CreateTranslationMatrix(MakePoint4d(-fCoordSystemRelation.TranslateX,
            -fCoordSystemRelation.TranslateY, -fCoordSystemRelation.TotalTranslateZ)));
    end;

    if self.IsZoomed() then
    begin
        xML := GetMulMatrix(xML, CreateScaleMatrix(1.0 / fCoordSystemRelation.fZoom,
            1.0 / fCoordSystemRelation.fZoom, 1.0 / fCoordSystemRelation.fZoom));
    end;

    result := xML;

end;

function TCoordCalculator.GetParentMatrix(): TMatrix4d;
begin
    if fRecalcParent then
    begin
        fParentCoordCalculator.CalcCoordMatrix();
    end;
    result := fParentCoordCalculator.MatrixHandle.Matrix;
end;

function TCoordCalculator.GetInvParentMatrix(): TMatrix4d;
begin
    if fRecalcInvParent then
    begin
        fParentCoordCalculator.CalcInvCoordMatrix();
    end;
    result := fParentCoordCalculator.InvMatrixHandle.Matrix;
end;

procedure TCoordCalculator.CalcCoordMatrix();
var
    xLocalMatrix, xParentMatrix: TMatrix4d;

begin
    xLocalMatrix := CalcTransformMatrix();
    if Assigned(fParentCoordCalculator) then
    begin
        xParentMatrix := GetParentMatrix();
        fMatrixHandle.Matrix := CalcMultMatrix(xLocalMatrix, xParentMatrix);
    end
    else
    begin
        fMatrixHandle.Matrix := xLocalMatrix;
    end;
end;

procedure TCoordCalculator.CalcInvCoordMatrix();
var
    xLocalMatrix, xParentMatrix: TMatrix4d;

begin
    xLocalMatrix := CalcInvTransformMatrix();
    if Assigned(fParentCoordCalculator) then
    begin
        xParentMatrix := GetInvParentMatrix();
        fInvMatrixHandle.Matrix := CalcMultMatrix(xParentMatrix, xLocalMatrix);
    end
    else
    begin
        fInvMatrixHandle.Matrix := xLocalMatrix;
    end;
end;

function TCoordCalculator.CalcMultMatrix(const aLocalMatrix, aParentMatrix: TMatrix4d): TMatrix4d;
begin
    result := GetMulMatrix(aParentMatrix, aLocalMatrix);
end;

class function TCoordCalculator.FindRect(aP1, aP2: TPoint4d; aScaleFactor: double;
    out oReverseX, oReverseY: boolean): TRect;
begin
    result := Rect(Round(aP1.x * aScaleFactor), Round(aP1.y * aScaleFactor), Round(aP2.x * aScaleFactor),
        Round(aP2.y * aScaleFactor));
    oReverseX := aP1.x > aP2.x;
    oReverseY := aP1.y > aP2.y;

    if oReverseX then
    begin
        result.Left := Round(aP2.x * aScaleFactor);
        result.Right := Round(aP1.x * aScaleFactor);
    end;

    if oReverseY then
    begin
        result.Top := Round(aP2.y * aScaleFactor);
        result.Bottom := Round(aP1.y * aScaleFactor);
    end;
end;

class function TCoordCalculator.CalcTransformedRect(aSizeX, aSizeY: double; const aMatrix: TMatrix4d;
    aScaleFactor: double; out oReverseX, oReverseY: boolean): TRect;
var
    xP1, xP2: TPoint4d;
    xT1, xT2: TPoint4d;
begin

    xP1 := MakePoint4d(0, 0, 0);
    xP2 := MakePoint4d(0 + aSizeX, 0 + aSizeY, 0);

    xT1 := MatrixMulPoint(xP1, aMatrix);
    xT2 := MatrixMulPoint(xP2, aMatrix);

    result := FindRect(xT1, xT2, aScaleFactor, oReverseX, oReverseY);

end;

class function TCoordCalculator.ReOrderValues(var vValue1, vValue2: double): boolean;
var
    xTemp: double;
begin
    result := vValue1 < vValue2;
    if result then
    begin
        xTemp := vValue1;
        vValue1 := vValue2;
        vValue2 := xTemp;
    end;
end;

class function TCoordCalculator.FindXYZ(aP1, aP2: TPoint4d; out oReverseX, oReverseY, oReverseZ: boolean)
    : TPoint4d;
begin

    result.x := (aP1.x);
    result.y := (aP1.y);
    result.z := (aP1.z);

    oReverseX := aP1.x > aP2.x;
    oReverseY := aP1.y > aP2.y;
    oReverseZ := aP1.z > aP2.z;

    if oReverseX then
    begin
        result.x := (aP2.x);
    end;

    if oReverseY then
    begin
        result.y := (aP2.y);
    end;

    if oReverseZ then
    begin
        result.z := (aP2.z);
    end;
end;

class function TCoordCalculator.CalcTransformedPoint(const aMatrix: TMatrix4d): TPoint4d;
var
    xT1, xT2: TPoint4d;
    xReverseX, xReverseY, xReverseZ: boolean;
begin
    xT1 := MatrixMulPoint(MakePoint4d(0, 0, 0), aMatrix);
    xT2 := MatrixMulPoint(MakePoint4d(1, 1, 1), aMatrix);
    result := FindXYZ(xT1, xT2, xReverseX, xReverseY, xReverseZ);
end;

procedure TCoordSystemRelation.RotateZAround(aRotateZ: TCoordSystemRotateType; aOffsetX, aOffsetY: double);
begin
    fRotateZ := aRotateZ;
    fRotateZOffsetX := aOffsetX;
    fRotateZOffsetY := aOffsetY;
end;

function TCoordCalculator.TransformPoint(aPoint: TPoint4d): TPoint4d;
begin
    result := MatrixMulPoint(aPoint, self.fMatrixHandle.Matrix);
end;

class function TCoordCalculator.CompareZHeight(aHeight1, aHeight2: double): TZHeightCompareType;
begin
    result := zhcEqual;

    if aHeight1 > aHeight2 then
    begin
        if cUpDirectionIsPositive then
            result := zhcGreater
        else
            result := zhcLess
    end
    else if aHeight1 < aHeight2 then
    begin
        if cUpDirectionIsPositive then
            result := zhcLess
        else
            result := zhcGreater
    end;

end;

class function TCoordCalculator.DecreaseZHeight(aCurrentValue, aMagnitude: double): double;
begin
    if cUpDirectionIsPositive then
        result := aCurrentValue - aMagnitude
    else
        result := aCurrentValue + aMagnitude
end;

class function TCoordCalculator.IncreaseZHeight(aCurrentValue, aMagnitude: double): double;
begin
    if cUpDirectionIsPositive then
        result := aCurrentValue + aMagnitude
    else
        result := aCurrentValue - aMagnitude
end;


end.
