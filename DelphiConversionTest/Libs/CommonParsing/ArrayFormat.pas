{ -----------------------------------------------------------------------------------------------------------
  Copyright © 2013 Zinsser Analytic GmbH - All rights reserved.
  Author       : Wolfgang Lyncke (wl)
  Description  : Das Format A() muss teilweise auch auﬂerhalb des Parsers umgewandelt werden
  -----------------------------------------------------------------------------------------------------------
  Revision History:
  date     op  method                              track-no improvement/change
  -------- --  ----------------------------------  -------- -------------------------------------------------
  12.08.13 wl                                      TN6214   Initial Revision
  ----------------------------------------------------------------------------------------------------------- }

unit ArrayFormat;


interface


type
    TArrayFormat = record
    private const
        cDelimiter = ',';
        cArrayFormatStart = 'A(';
        cArrayFormatEnd = ')';
    private
        class function GetDelimiter: string; static;
    public
        class function StringIsArrayFormat(const aValue: string): boolean; static;
        class function ArrayFormatToStringArray(const aValue: string): TArray<string>; static;
        class function StringArrayToArrayFormat(const aValues: TArray<string>): string; static;
        class function GetValueText(const aArrayFormat: string): string; static;
        class property Delimiter: string read GetDelimiter;
    end;


implementation


uses
    ArrayUtils;

{ TArrayFormat }

class function TArrayFormat.ArrayFormatToStringArray(const aValue: string): TArray<string>;
begin
    EXIT(TArrayUtils.StringToStringArray(GetValueText(aValue), cDelimiter));
end;

class function TArrayFormat.StringIsArrayFormat(const aValue: string): boolean;
begin
    EXIT((Length(aValue) >= 3) and (Copy(aValue, 1, 2) = cArrayFormatStart) and
        (Copy(aValue, Length(aValue), 1) = cArrayFormatEnd));
end;

class function TArrayFormat.GetDelimiter: string;
begin
    EXIT(cDelimiter);
end;

class function TArrayFormat.GetValueText(const aArrayFormat: string): string;
begin
    EXIT(Copy(aArrayFormat, 3, Length(aArrayFormat) - 3));
end;

class function TArrayFormat.StringArrayToArrayFormat(const aValues: TArray<string>): string;
var
    x: integer;
begin
    result := TArrayFormat.cArrayFormatStart;
    for x := 0 to high(aValues) do
    begin
        if x > 0 then
            result := result + TArrayFormat.cDelimiter;
        result := result + aValues[x];
    end;
    result := result + TArrayFormat.cArrayFormatEnd;
end;


end.
