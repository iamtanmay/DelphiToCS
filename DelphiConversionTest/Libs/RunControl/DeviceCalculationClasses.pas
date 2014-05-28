unit DeviceCalculationClasses;

{ --------------------------------------------------------------------------------------------------
  Copyright © 2013 Zinsser Analytic GmbH - All rights reserved.
  Author       : Thomas Schubert (ts)
  Description  : Functions and objects for calculation in devices
  --------------------------------------------------------------------------------------------------
  Revision History:
  date     op    method                      track-no improvement/change
  -------- ---  ---------------------------  -------- ----------------------------------------------
  14.02.13 ts                                TN6087   TFifo for Balances, PHMeter for InternalCalc (STDEV)
  -------------------------------------------------------------------------------------------------- }


interface


uses
    Classes;

type

    TFifo = class
    private
        fValueArray: array of extended;
        fNumValues: integer;
        fCurrentNumValues: integer;
        fCurrentIndex: integer;
        fMaxIndex: integer;
        fDeviation, fMaxValue, fMinValue: extended;
        procedure SetNumValues(const Value: integer);
        function CalcMean: extended;
        function CalcStandardDev(aMean: extended): extended;
    public
        constructor Create(aNumValues: integer; aDeviation: extended);
        destructor Destroy; override;
        procedure AddValue(aValue: extended);
        procedure Reset;
        procedure CalculateValues(out oStable: Boolean; out oValue: extended; out oStdDevAvailable: boolean;
            out oStdDev: extended);
        property MaxValue: extended read fMaxValue write fMaxValue;
        property MinValue: extended read fMinValue write fMinValue;
    end;


implementation


{ TFifo }

procedure TFifo.AddValue(aValue: extended);
begin
    if fCurrentNumValues < fNumValues then
        inc(fCurrentNumValues);
    inc(fCurrentIndex);
    if fCurrentIndex > fMaxIndex then
        fCurrentindex := 0;
    fValueArray[fCurrentIndex] := aValue;
end;

procedure TFifo.Reset;
var
    i: integer;
begin
    for I := 0 to fMaxIndex do
        fValueArray[i] := low(Integer);
    fCurrentNumValues := 0;
    fCurrentIndex := -1;
end;

function TFifo.CalcMean(): extended;
var
    xSum: extended;
    xValue: extended;
    x: integer;
begin
    result := 0;
    fMaxValue := low(Integer);
    fMinValue := high(Integer);
    if fCurrentNumValues <= 0 then
        EXIT;

    xSum := 0;
    for x := 0 to fCurrentNumValues - 1 do
    begin
        xValue := fValueArray[x];
        xSum := xSum + xValue;
        if xValue > fMaxValue then
            fMaxValue := xValue
        else if xValue < fMinValue then
            fMinValue := xValue;
    end;
    result := xSum / fCurrentNumValues;
end;

function TFifo.CalcStandardDev(aMean: extended): extended;
var
    xSumOfSquaredDifs: extended;
    x: integer;
    xValue: extended;
begin
    if fCurrentNumValues <= 1 then
    begin
        result := 0;
        EXIT;
    end;

    xSumOfSquaredDifs := 0;
    for x := 0 to fCurrentNumValues - 1 do
    begin
        xValue := fValueArray[x];
        xSumOfSquaredDifs := xSumOfSquaredDifs + Sqr(xValue - aMean);
    end;

    result := Sqrt(xSumOfSquaredDifs / (fCurrentNumValues - 1));
end;

procedure TFifo.CalculateValues(out oStable: Boolean; out oValue: extended; out oStdDevAvailable: boolean;
    out oStdDev: extended);
begin
    oStable := false;

    oStdDev := 0;
    oStdDevAvailable := false;

    oValue := CalcMean();

    if fCurrentNumValues = fNumValues then
    begin
        oStdDevAvailable := true;
        oStdDev := CalcStandardDev(oValue);
        oStable := oStdDev <= fDeviation;
    end;

end;

constructor TFifo.create(aNumValues: integer; aDeviation: extended);
begin
    SetNumValues(aNumValues);
    fDeviation := aDeviation;
    reset;
end;

destructor TFifo.destroy;
begin
    SetLength(fValueArray, 0);
    inherited;
end;

procedure TFifo.SetNumValues(const Value: integer);
begin
    if Value <> fNumValues then
    begin
        fNumValues := Value;
        fMaxIndex := Value - 1;
        SetLength(fValueArray, fNumValues);
    end;
end;


end.
