unit ClockClass;


interface


{ ARR_DAYS_CONVERT : array [ 0 .. Integer( High( TClockTimeUnit ) ), 0 .. Integer( High( TClockTimeUnit ) ) ] of double =
  (
  ( (1                                                            ),    (INT_DAYS_TO_THOUSANDTH_OF_SEC / INT_DAYS_TO_HUNDREDTH_OF_SEC), (INT_DAYS_TO_THOUSANDTH_OF_SEC / INT_DAYS_TO_TENTH_OF_SEC), (INT_DAYS_TO_THOUSANDTH_OF_SEC / INT_DAYS_TO_SEC) ),
  ( (INT_DAYS_TO_HUNDREDTH_OF_SEC  / INT_DAYS_TO_THOUSANDTH_OF_SEC),    (1                                                           ), (INT_DAYS_TO_HUNDREDTH_OF_SEC  / INT_DAYS_TO_TENTH_OF_SEC), (INT_DAYS_TO_HUNDREDTH_OF_SEC  / INT_DAYS_TO_SEC) ),
  ( (INT_DAYS_TO_TENTH_OF_SEC      / INT_DAYS_TO_THOUSANDTH_OF_SEC),    (INT_DAYS_TO_TENTH_OF_SEC      / INT_DAYS_TO_HUNDREDTH_OF_SEC), (1                                                       ), (INT_DAYS_TO_TENTH_OF_SEC      / INT_DAYS_TO_SEC) ),
  ( (INT_DAYS_TO_SEC               / INT_DAYS_TO_THOUSANDTH_OF_SEC),    (INT_DAYS_TO_SEC               / INT_DAYS_TO_HUNDREDTH_OF_SEC), (INT_DAYS_TO_SEC               / INT_DAYS_TO_TENTH_OF_SEC), (1                                              ) )
  );
}

type

    TClockTimeUnit = (tuThousandth, tuHundredth, tuTenth, tuSec);
    // Clock

    TClockState = (csStopped, csStarted, csPaused);
    TClockTimeType = cardinal;

    TClock = class
    protected
        fStartedAt, fLastPausedAt: TDateTime;
        fOffSet: TClockTimeType;
        fClockState: TClockState;
        fPausedDuration: TClockTimeType;
        fTimeUnit: TClockTimeUnit;
        function ElapsedTimeAt(aTime: TDateTime): cardinal;
        class function DaysMultiplier(aTimeUnit: TClockTimeUnit): cardinal;
        class function DaysToTimeUnit(aDays: Extended; aTimeUnit: TClockTimeUnit): TClockTimeType; overload;
        class function DaysToTimeUnit(aDays: Extended; aMultiplier: cardinal): TClockTimeType; overload;
        class function ElapsedTimeBetween(aTime1, aTime2: TDateTime; aTimeUnit: TClockTimeUnit): cardinal;
        class function ConvertTime(aFromTimeUnit, aToTimeUnit: TClockTimeUnit; aTime: TClockTimeType)
            : TClockTimeType;

    public
        constructor Create(aTimeUnit: TClockTimeUnit);
        procedure Reset();
        procedure Start(aOffSet: cardinal);
        function Pause(): TClockTimeType;
        procedure Unpause();
        function ElapsedTime(): TClockTimeType;
        function ElapsedTimeAs(aAsTimeUnit: TClockTimeUnit): TClockTimeType;
        function ConvertToClockTimeUnit(aTime: TClockTimeType; aTimeUnit: TClockTimeUnit): TClockTimeType;
        function ConvertFromClockTimeUnit(aTime: TClockTimeType; aTimeUnit: TClockTimeUnit): TClockTimeType;
        property Offset: cardinal read fOffSet write fOffSet;
        property ClockState: TClockState read fClockState;
    end;


implementation


uses
    SysUtils;

constructor TClock.Create(aTimeUnit: TClockTimeUnit);
begin
    inherited Create;
    fTimeUnit := aTimeUnit;
    Reset();
end;

procedure TClock.Reset();
begin
    fClockState := csStopped;
    fStartedAt := 0;
    fLastPausedAt := 0;
end;

class function TClock.DaysMultiplier(aTimeUnit: TClockTimeUnit): cardinal;
begin
    result := 1;
    case aTimeUnit of
        tuThousandth:
            result := MSECSPERDAY;
        tuHundredth:
            result := Round(MSECSPERDAY / 10);
        tuTenth:
            result := Round(MSECSPERDAY / 100);
        tuSec:
            result := Round(MSECSPERDAY / 1000);
    end;
end;

class function TClock.DaysToTimeUnit(aDays: Extended; aMultiplier: cardinal): TClockTimeType;
begin
    result := Round(aDays * aMultiplier);
end;

class function TClock.DaysToTimeUnit(aDays: Extended; aTimeUnit: TClockTimeUnit): TClockTimeType;
var
    xMultiplier: cardinal;
begin
    xMultiplier := DaysMultiplier(aTimeUnit);
    result := DaysToTimeUnit(aDays, xMultiplier);
end;

class function TClock.ElapsedTimeBetween(aTime1, aTime2: TDateTime; aTimeUnit: TClockTimeUnit)
    : TClockTimeType;
begin
    result := DaysToTimeUnit(aTime1 - aTime2, aTimeUnit);
end;

procedure TClock.Start(aOffSet: TClockTimeType);
begin
    fOffSet := aOffSet;
    fStartedAt := Now();
    fLastPausedAt := fStartedAt;
    fClockState := csStarted;
    fPausedDuration := 0;
end;

function TClock.Pause(): TClockTimeType;
begin
    fLastPausedAt := Now();
    result := ElapsedTimeAt(fLastPausedAt);
    fClockState := csPaused;
end;

procedure TClock.Unpause();
begin
    if fClockState <> csPaused then
        EXIT;
    fClockState := csStopped;
    if fLastPausedAt = 0 then
        EXIT;
    fPausedDuration := fPausedDuration + ElapsedTimeBetween(Now(), fLastPausedAt, fTimeUnit);
    fClockState := csStarted;
end;

function TClock.ElapsedTimeAt(aTime: TDateTime): TClockTimeType;
begin
    result := fOffSet;
    if fClockState in [csStarted, csPaused] then
        result := result + ElapsedTimeBetween(aTime, fStartedAt, fTimeUnit) - fPausedDuration;
end;

function TClock.ElapsedTime(): TClockTimeType;
var
    xCurrentTime: TDateTime;
begin
    xCurrentTime := 0;
    if fClockState = csStarted then
        xCurrentTime := Now()
    else if fLastPausedAt <> 0 then
        xCurrentTime := fLastPausedAt;

    result := ElapsedTimeAt(xCurrentTime);
end;

class function TClock.ConvertTime(aFromTimeUnit, aToTimeUnit: TClockTimeUnit; aTime: TClockTimeType)
    : TClockTimeType;
var
    xMultiplier: double;
begin
    xMultiplier := 1; // ARR_DAYS_CONVERT[ Integer( aToTimeUnit ), Integer( aFromTimeUnit ) ];
    result := Round(aTime * xMultiplier);
end;

function TClock.ConvertToClockTimeUnit(aTime: TClockTimeType; aTimeUnit: TClockTimeUnit): TClockTimeType;
begin
    result := ConvertTime(aTimeUnit, fTimeUnit, aTime);
end;

function TClock.ConvertFromClockTimeUnit(aTime: TClockTimeType; aTimeUnit: TClockTimeUnit): TClockTimeType;
begin
    result := ConvertTime(fTimeUnit, aTimeUnit, aTime);
end;

function TClock.ElapsedTimeAs(aAsTimeUnit: TClockTimeUnit): TClockTimeType;
begin
    result := ConvertFromClockTimeUnit(ElapsedTime(), aAsTimeUnit);
end;


end.
