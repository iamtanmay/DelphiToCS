{ --------------------------------------------------------------------------------------------------
  Copyright © 2008 Zinsser Analytic GmbH - All rights reserved.
  Author       : Payman Kamali (pk)
  Description  :
  --------------------------------------------------------------------------------------------------
  Revision History:
  date     op  method                       track-no improvement/change
  -------- --  ---------------------------  -------- -----------------------------------------------
  23.06.08 pk                                TN4139  Initial Revision
  11.03.09 pk  CreateReflectionMatrixY       TN4457  New
  24.04.12 wl                                TN5875   Angle-Werte als double statt single; führte zu erheblichen Rundungsfehlern
  -------------------------------------------------------------------------------------------------- }

unit MatrixMath;


interface


uses
    Windows;

type
    GLDouble = double;
    GLInt = integer;
    GLuByte = Byte;
    GLuInt = cardinal;

const
    _X = 0;
    _Y = 1;
    _Z = 2;
    _W = 3;

type
    TPoint4d = record
        x, y, z, w: GLDouble;
    end;

    TVector4d = record
        x, y, z, w: GLDouble;
    end;

    TMatrix4d = array [0 .. 3, 0 .. 3] of GLDouble;
    PMatrix4d = ^TMatrix4d;
    TMatrixd = array [0 .. 3] of TVector4d;

    TRectangle = record
        Left, Top, Width, Height: integer;
    end;

    TPoint4dArray = array of TPoint4d;

function CalcSpace(aFirst, aLast: double; aNumSlots: integer; aSlotSize: double): double;

function PointRotateAroundX(v: TPoint4d; alpha: double): TPoint4d;
function PointRotateAroundY(v: TPoint4d; alpha: double): TPoint4d;
function PointRotateAroundZ(v: TPoint4d; alpha: double): TPoint4d;
function GetDotProd4d(v1, v2: TVector4d): double;
function GetCrossProdVector4d(v1, v2: TVector4d): TVector4d;
function MakePoint4d(x, y, z: GLDouble; w: GLDouble = 1): TPoint4d;
function MakeVector4d(x, y, z: GLDouble; w: GLDouble = 0): TVector4d;
function NormalizeVector4d(v: TVector4d): TVector4d;
function GetPerpVector4d(p1, p2, p3: TPoint4d): TVector4d;

function GetDifVector4d(p1, p2: TPoint4d): TVector4d;
function ScalarMultVector(s: double; v: TVector4d): TVector4d;
function VectorAddition(v1, v2: TVector4d): TVector4d;
function GetAddVectorToPoint(v: TVector4d; p: TPoint4d): TPoint4d;
function VectorAngleCosine4d(v1, v2: TVector4d): double;
function GetMatrixd(const src: TMatrixd): TMatrixd;
function CreateRotationMatrix(const anAxis: TVector4d; angle: double): TMatrix4d;
function CreateInvTranslationMatrix(const P: TPoint4d): TMatrix4d;
function CreateTranslationMatrix(const P: TPoint4d): TMatrix4d;
function MatrixMulPoint(const p: TPoint4d; m: TMatrix4d): TPoint4d;
function MatrixMulVector(const v: TVector4d; m: TMatrix4d): TVector4d;
function GetMulMatrix(aM1, aM2: TMatrix4d): TMatrix4d;
function CreateRotationMatrixX(const aAngle: double): TMatrix4d;
function CreateRotationMatrixZ(const aAngle: double): TMatrix4d;
function CreateReflectionMatrixX(): TMatrix4d;
function CreateReflectionMatrixY(): TMatrix4d;
function GetTransposeMatrix(aM1: TMatrix4d): TMatrix4d;
function GetMagnitude(v: TVector4d): double;
function GetNormal4d(p1, p2, p3: TPoint4d; aNormalize: boolean = true): TVector4d;
function VectorToPoint(v: TVector4d): TPoint4d;
function PointToVector(p: TPoint4d): TVector4d;
function GetOffsetPoint(p: TPoint4d; aX, aY, aZ: double): TPoint4d;
function GetIdentMatrix(): TMatrix4d;
function CreateScaleMatrix(aX, aY, aZ: double): TMatrix4d;
function InterpPointAlongVector4d(p: TPoint4d; v: TVector4d; t: double): TPoint4d;


implementation


uses
    Math;

const
    MTX_IDENTITY: TMatrix4d = ((1, 0, 0, 0), (0, 1, 0, 0), (0, 0, 1, 0), (0, 0, 0, 1));

function CalcSpace(aFirst, aLast: double; aNumSlots: integer; aSlotSize: double): double;
begin
    if aNumSlots = 1 then
        result := aLast - aFirst
    else
        result := (aLast - aFirst - ((aNumSlots - 1) * aSlotSize)) / (aNumSlots - 1);
end;

function InterpPointAlongVector4d(p: TPoint4d; v: TVector4d; t: double): TPoint4d;
begin
    result := MakePoint4d(p.x + v.x * t, p.y + v.y * t, p.z + v.z * t);
end;

function MakeVector4d(x, y, z: GLDouble; w: GLDouble = 0): TVector4d;
begin
    result.x := x;
    result.y := y;
    result.z := z;
    result.w := w;
end;

function MakePoint4d(x, y, z: GLDouble; w: GLDouble = 1): TPoint4d;
begin
    result.x := x;
    result.y := y;
    result.z := z;
    result.w := w;
end;

function MakeMatrix4d(v1, v2, v3, v4: TVector4d): TMatrixd;
begin
    result[0] := v1;
    result[1] := v2;
    result[2] := v3;
    result[3] := v4;
end;

// direction of this vector is P1 <- P2
function GetDifVector4d(p1, p2: TPoint4d): TVector4d;
begin
    result.x := p1.x - p2.x;
    result.y := p1.y - p2.y;
    result.z := p1.z - p2.z;
    result.w := 0;
end;

function ScalarMultVector(s: double; v: TVector4d): TVector4d;
begin
    result.x := v.x * s;
    result.y := v.y * s;
    result.z := v.z * s;
    result.w := 0;
end;

function VectorAddition(v1, v2: TVector4d): TVector4d;
begin
    result.x := v1.x + v2.x;
    result.y := v1.y + v2.y;
    result.z := v1.z + v2.z;
    result.w := 0;
end;

function GetOffsetPoint(p: TPoint4d; aX, aY, aZ: double): TPoint4d;
begin
    result := MakePoint4d(p.x + aX, p.y + aY, p.z + aZ);
end;

function GetAddVectorToPoint(v: TVector4d; p: TPoint4d): TPoint4d;
begin
    result := MakePoint4d(v.x + p.x, v.y + p.y, v.z + p.z);
end;

function GetDotProd4d(v1, v2: TVector4d): double;
begin
    result := v1.x * v2.x + v1.y * v2.y + v1.z * v2.z;

end;

function GetCrossProdVector4d(v1, v2: TVector4d): TVector4d;
begin
    result.x := v1.y * v2.z - v2.y * v1.z;
    result.y := v2.x * v1.z - v1.x * v2.z;
    result.z := v1.x * v2.y - v2.x * v1.y;
    result.w := 0;
end;

function GetMagnitude(v: TVector4d): double;
begin
    result := Norm([v.x, v.y, v.z]);
end;

function NormalizeVector4d(v: TVector4d): TVector4d;
var
    xMagnitude: double;
begin
    xMagnitude := GetMagnitude(v);
    result.x := v.x / xMagnitude;
    result.y := v.y / xMagnitude;
    result.z := v.z / xMagnitude;
    result.w := 0;
end;

function GetPerpVector4d(p1, p2, p3: TPoint4d): TVector4d;
begin
    result := GetCrossProdVector4d(GetDifVector4d(p1, p2), GetDifVector4d(p2, p3));
end;

function GetNormal4d(p1, p2, p3: TPoint4d; aNormalize: boolean = true): TVector4d;
begin
    result := GetPerpVector4d(p1, p2, p3);
    if aNormalize then
        result := NormalizeVector4d(result);
end;

function VectorAngleCosine4d(v1, v2: TVector4d): double;
begin
    result := GetDotProd4d(v1, v2) / (GetMagnitude(v1) * GetMagnitude(v2));
end;

procedure SinCos(alpha: double; var s, c: double);
begin
    c := Cos(alpha);
    s := Sin(alpha);
end;

function PointRotateAroundX(v: TPoint4d; alpha: double): TPoint4d;
var
    c, s: double;
begin
    SinCos(alpha, s, c);
    result := MakePoint4d(v.x, c * v.y + s * v.z, c * v.z - s * v.y);
end;

function PointRotateAroundY(v: TPoint4d; alpha: double): TPoint4d;
var
    c, s: double;
begin
    SinCos(alpha, s, c);
    result := MakePoint4d(c * v.x - s * v.z, v.y, c * v.z + s * v.x);
end;

function PointRotateAroundZ(v: TPoint4d; alpha: double): TPoint4d;
var
    c, s: double;
begin
    SinCos(alpha, s, c);
    result := MakePoint4d(c * v.x + s * v.y, c * v.y - s * v.x, v.z);
end;

function MakeTranslatef(x, y, z: GLDouble): TMatrixd;
begin
    result := MakeMatrix4d(MakeVector4d(1, 0, 0), MakeVector4d(0, 1, 0), MakeVector4d(0, 0, 1),
        MakeVector4d(x, y, z, 1));
end;

function ModelToView(): TMatrixd;
begin
    result := MakeMatrix4d(MakeVector4d(1, 0, 0), MakeVector4d(0, 1, 0), MakeVector4d(0, 0, -1),
        MakeVector4d(0, 0, 0, 1));
end;

{
  function ConvTColorToTColor4i( aColor: TColor; aAlpha: GLuByte = 255 ): TColor4i;
  begin
  result.r :=  ( aColor and $FF );              // Lowest Byte = R
  result.g :=  ( ( aColor shr 8 ) and $FF );    // Second-Lowest Byte = G
  result.b :=  ( (  aColor shr 16 ) and $FF );  // Third-Lowest Byte = B
  result.a := aAlpha;
  end;

  function ConvTColorToTColor4f( aColor: TColor; aAlpha: double = 1 ): TColor4f;
  begin
  result.r :=  ( aColor and $FF ) / 255;              // Lowest Byte = R
  result.g :=  ( ( aColor shr 8 ) and $FF ) / 255;    // Second-Lowest Byte = G
  result.b :=  ( (  aColor shr 16 ) and $FF ) / 255;  // Third-Lowest Byte = B
  result.a := aAlpha;
  end;
}
{
  SetLength(result, aBitmap.Width);
  for I := Low(result) to High(result) do begin
  SetLength(result[I], aBitMap.Height);
  for J := Low(result[I]) to High(result[I]) do
  result[I,J] := ConvTColorToTColor4i( aBitMap.Canvas.Pixels[ i, j ] );
  end; }

{
  function ColorAlpha( aColor : TColor; aAlpha: double = 1 ) : TColorAlpha;
  begin
  with result do begin
  Color := aColor;
  Alpha := aAlpha;
  end;
  end;
}

function GetMatrixd(const src: TMatrixd): TMatrixd;
var
    i: Integer;
begin

    for i := _X to _W do
    begin
        result[i].x := src[i].x;
        result[i].y := src[i].y;
        result[i].z := src[i].z;
        result[i].w := src[i].w;
    end;

end;

function GetMulMatrix(aM1, aM2: TMatrix4d): TMatrix4d;
var
    xRow, xCol, i: integer;
    xVal: double;
begin

    for xRow := _X to _W do
    begin
        for xCol := _X to _W do
        begin
            xVal := 0;
            for i := 0 to 3 do
            begin
                xVal := xVal + aM1[i, xRow] * aM2[xCol, i];
            end;
            result[xCol, xRow] := xVal;
        end;
    end;

end;

// CreateRotationMatrix
//
function CreateRotationMatrix(const anAxis: TVector4d; angle: double): TMatrix4d;
var
    axis: TVector4d;
    cosine, sine, one_minus_cosine: double;
begin
    SinCos(Angle, Sine, Cosine);
    one_minus_cosine := 1 - cosine;
    axis := NormalizeVector4d(anAxis);

    Result[_X, _X] := (one_minus_cosine * Sqr(Axis.x)) + Cosine;
    Result[_X, _Y] := (one_minus_cosine * Axis.x * Axis.y) - (Axis.z * Sine);
    Result[_X, _Z] := (one_minus_cosine * Axis.z * Axis.x) + (Axis.y * Sine);
    Result[_X, _W] := 0;

    Result[_Y, _X] := (one_minus_cosine * Axis.x * Axis.y) + (Axis.z * Sine);
    Result[_Y, _Y] := (one_minus_cosine * Sqr(Axis.y)) + Cosine;
    Result[_Y, _Z] := (one_minus_cosine * Axis.y * Axis.z) - (Axis.x * Sine);
    Result[_Y, _W] := 0;

    Result[_Z, _X] := (one_minus_cosine * Axis.z * Axis.x) - (Axis.y * Sine);
    Result[_Z, _Y] := (one_minus_cosine * Axis.y * Axis.z) + (Axis.x * Sine);
    Result[_Z, _Z] := (one_minus_cosine * Sqr(Axis.z)) + Cosine;
    Result[_Z, _W] := 0;

    Result[_W, _X] := 0;
    Result[_W, _Y] := 0;
    Result[_W, _Z] := 0;
    Result[_W, _W] := 1;
end;

function CreateTranslationMatrix(const P: TPoint4d): TMatrix4d;
begin
    result := MTX_IDENTITY;
    result[_W, _X] := P.x;
    result[_W, _Y] := P.y;
    result[_W, _Z] := P.z;
end;

function CreateInvTranslationMatrix(const P: TPoint4d): TMatrix4d;
begin
    result := MTX_IDENTITY;
    result[_X, _W] := P.x;
    result[_Y, _W] := P.y;
    result[_Z, _W] := P.z;
end;

function VectorToPoint(v: TVector4d): TPoint4d;
begin
    result := MakePoint4d(v.x, v.y, v.z);
end;

function PointToVector(p: TPoint4d): TVector4d;
begin
    result := MakeVector4d(p.x, p.y, p.z);
end;

function MatrixMulPoint(const p: TPoint4d; m: TMatrix4d): TPoint4d;
begin
    result.x := m[_X, _X] * p.x + m[_Y, _X] * p.y + m[_Z, _X] * p.z + m[_W, _X] * p.w;
    result.y := m[_X, _Y] * p.x + m[_Y, _Y] * p.y + m[_Z, _Y] * p.z + m[_W, _Y] * p.w;
    result.z := m[_X, _Z] * p.x + m[_Y, _Z] * p.y + m[_Z, _Z] * p.z + m[_W, _Z] * p.w;
    result.w := m[_X, _W] * p.x + m[_Y, _W] * p.y + m[_Z, _W] * p.z + m[_W, _W] * p.w;
end;

function MatrixMulVector(const v: TVector4d; m: TMatrix4d): TVector4d;
begin
    result := PointToVector(MatrixMulPoint(VectorToPoint(v), m));
end;

function CreateRotationMatrixX(const aAngle: double): TMatrix4d;
var
    s, c: double;
begin
    SinCos(aAngle, s, c);
    Result := MTX_IDENTITY;
    Result[_X, _X] := 1;
    Result[_Y, _Y] := c;
    Result[_Y, _Z] := s;
    Result[_Z, _Y] := -s;
    Result[_Z, _Z] := c;
    Result[_W, _W] := 1;
end;

function CreateRotationMatrixY(const aAngle: double): TMatrix4d;
var
    s, c: double;
begin
    SinCos(aAngle, s, c);
    Result := MTX_IDENTITY;
    Result[_X, _X] := c;
    Result[_X, _Z] := -s;
    Result[_Y, _Y] := 1;
    Result[_Z, _X] := s;
    Result[_Z, _Z] := c;
    Result[_W, _W] := 1;
end;

function CreateRotationMatrixZ(const aAngle: double): TMatrix4d;
var
    s, c: double;
begin
    SinCos(aAngle, s, c);
    Result := MTX_IDENTITY;
    Result[_X, _X] := c;
    Result[_X, _Y] := s;
    Result[_Y, _X] := -s;
    Result[_Y, _Y] := c;
    Result[_Z, _Z] := 1;
    Result[_W, _W] := 1;
end;

function CreateReflectionMatrixX(): TMatrix4d;
// a reflection in X-Axis
begin
    Result := MTX_IDENTITY;
    Result[_Y, _Y] := -1;
end;

function CreateReflectionMatrixY(): TMatrix4d;
// a reflection in Y-Axis
begin
    Result := MTX_IDENTITY;
    Result[_X, _X] := -1;
end;

function GetIdentMatrix(): TMatrix4d;
begin
    Result := MTX_IDENTITY;
end;

function CreateScaleMatrix(aX, aY, aZ: double): TMatrix4d;
begin
    Result := MTX_IDENTITY;
    Result[_X, _X] := aX;
    Result[_Y, _Y] := aY;
    Result[_Z, _Z] := aZ;
end;

function GetTransposeMatrix(aM1: TMatrix4d): TMatrix4d;
var
    xRow, xCol: integer;
begin
    for xRow := _X to _W do
    begin
        for xCol := _X to _W do
        begin
            result[xRow, xCol] := aM1[xCol, xRow];
        end;
    end;
end;


end.
