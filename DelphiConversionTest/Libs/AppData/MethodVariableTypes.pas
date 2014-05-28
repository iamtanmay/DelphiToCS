{ -----------------------------------------------------------------------------------------------------------
  Copyright © 2013 Zinsser Analytic GmbH - All rights reserved.
  Author       : Wolfgang Lyncke (wl)
  Description  :
  -----------------------------------------------------------------------------------------------------------
  Revision History:
  date     op  method                              track-no improvement/change
  -------- --  ----------------------------------  -------- -------------------------------------------------
  20.02.13 wl                                      TN6055   Initial Revision
  22.02.13 wl                                      TN6094   Neu: RequestOrder
  22.02.13 wl                                      TN6095   Neu: DialogType, DialogCaption, DialogHide
  08.03.13 wl  TMethodVariableFormatType           TN6095   neu
  11.03.13 wl                                      TN6095   Neu: ArrayLengthRefToOrder
  21.03.13 wl  GetCompareOrder                     TN6045   neu
  19.04.13 wl                                      TN6095   neues Feld DataIsArray
  14.05.13 wl                                      TN6095   DialogCaption entfernt
  10.02.14 ts                                      TN6353   mvfVolume,mvfSubstance,mvfLiqParam added
  08.04.14 ts                                      TN6391   mvfSourceRackName,-Position added
  ----------------------------------------------------------------------------------------------------------- }

unit MethodVariableTypes;


interface


type
    // kompletter Record (für DataAdaptor)
    TMethodVariableRec = record
        MethodName: string;
        VariableName: string;
        RequestOrder: integer;
        RequestText: string;
        DefaultValue: string;
        MinCheck: boolean;
        MinValue: double;
        MaxCheck: boolean;
        MaxValue: double;
        PickList: string;
        DataType: integer;
        DataIsArray: boolean;
        DialogHide: boolean;
        ArrayLengthRefToOrder: integer;
    end;

    TMethodVariableFormatType = (mvfNoFormat = 0, mvfString = 1, mvfFloat = 2, mvfInteger = 3, mvfBoolean = 4,
        mvfPathName = 11, mvfFileName = 12, mvfRackName = 21, mvfRackPos = 22, mvfRackLastPos = 23,
        mvfVolume = 24, mvfSubstance = 25, mvfLiqParam = 26, mvfSourceRackName = 27, mvfSourceRackPos = 28,
        mvfSourceRackLastPos = 29, mvfCarrier = 31, mvfCarrierSlot = 32);

    // Datansatz für TParserStoredIdent
    TMethodVariableData = record
        RequestOrder: integer;
        RequestText: string;
        DefaultValue: string;
        MinCheck: boolean;
        MinValue: double;
        MaxCheck: boolean;
        MaxValue: double;
        PickList: string;
        DataType: TMethodVariableFormatType;
        DataIsArray: boolean;
        DialogHide: boolean;
        ArrayLengthRefToOrder: integer;
    end;

    TMethodVariableUtils = record
    public const
        cIdentPicklistPath = '[PATH]';
        cIdentPicklistSQL = 'SELECT ';
    public
        class function GetMethodVariableFormatType(Value: integer): TMethodVariableFormatType; static;
        class function MinMaxValueToStr(aIsDefined: boolean; aValue: double): string; static;
        class function StrToMinMaxValue(const aText: string; out oIsDefined: boolean): double; static;
        class function GetEmptyRec(): TMethodVariableData; static;
        class function CreatePickList(aSourceString: string): TArray<string>; static;
        class function MethodVariableRecToData(const aRec: TMethodVariableRec): TMethodVariableData; static;
        class function MethodVariableDataToRec(const aMethodName, aVariableName: string;
            const aRec: TMethodVariableData): TMethodVariableRec; static;
        class function GetCompareOrder(const aRequestOrder, aStandardOrder: integer): integer; static;
    end;

    TMethodVarPageRec = record
        MethodName: string;
        Page: integer;
        FirstOrderIndex: integer;
        LastOrderIndex: integer;
        Caption: string;
    end;


implementation


uses
    SysUtils,
    GeneralTypes,
    DataProvider,
    DataProviderFactory,
    StringUtilities,
    FileUtilities;

{ TMethodVariableUtils }

class function TMethodVariableUtils.GetEmptyRec(): TMethodVariableData;
begin
    result.RequestOrder := 0;
    result.RequestText := '';
    result.DefaultValue := '';
    result.MinCheck := false;
    result.MinValue := 0;
    result.MaxCheck := false;
    result.MaxValue := 0;
    result.PickList := '';
    result.DataType := mvfNoFormat;
    result.DataIsArray := false;
    result.DialogHide := false;
    result.ArrayLengthRefToOrder := 0;
end;

class function TMethodVariableUtils.GetMethodVariableFormatType(Value: integer): TMethodVariableFormatType;
begin
    { TODO : Prüfen, ob der Wert auch imBereich liegt }
    EXIT(TMethodVariableFormatType(Value));
end;

class function TMethodVariableUtils.MethodVariableDataToRec(const aMethodName, aVariableName: string;
    const aRec: TMethodVariableData): TMethodVariableRec;
begin
    result.MethodName := aMethodName;
    result.VariableName := aVariableName;
    result.RequestOrder := aRec.RequestOrder;
    result.RequestText := aRec.RequestText;
    result.DefaultValue := aRec.DefaultValue;
    result.MinCheck := aRec.MinCheck;
    result.MinValue := aRec.MinValue;
    result.MaxCheck := aRec.MaxCheck;
    result.MaxValue := aRec.MaxValue;
    result.PickList := aRec.PickList;
    result.DataType := integer(aRec.DataType);
    result.DataIsArray := aRec.DataIsArray;
    result.DialogHide := aRec.DialogHide;
    result.ArrayLengthRefToOrder := aRec.ArrayLengthRefToOrder;
end;

class function TMethodVariableUtils.MethodVariableRecToData(const aRec: TMethodVariableRec)
    : TMethodVariableData;
begin
    result.RequestOrder := aRec.RequestOrder;
    result.RequestText := aRec.RequestText;
    result.DefaultValue := aRec.DefaultValue;
    result.MinCheck := aRec.MinCheck;
    result.MinValue := aRec.MinValue;
    result.MaxCheck := aRec.MaxCheck;
    result.MaxValue := aRec.MaxValue;
    result.PickList := aRec.PickList;
    result.DataType := GetMethodVariableFormatType(aRec.DataType);
    result.DataIsArray := aRec.DataIsArray;
    result.DialogHide := aRec.DialogHide;
    result.ArrayLengthRefToOrder := aRec.ArrayLengthRefToOrder;
end;

class function TMethodVariableUtils.MinMaxValueToStr(aIsDefined: boolean; aValue: double): string;
begin
    if (aIsDefined) then
        EXIT(FloatToStr(aValue, TFormatUtils.GetSettingsEnglishUS))
    else
        EXIT('');
end;

class function TMethodVariableUtils.StrToMinMaxValue(const aText: string; out oIsDefined: boolean): double;
begin
    if (aText <> '') then
    begin
        if TryStrToFloat(aText, result, TFormatUtils.GetSettingsEnglishUS) then
        begin
            oIsDefined := true;
            EXIT;
        end;
    end;

    oIsDefined := false;
    EXIT(0);
end;

class function TMethodVariableUtils.CreatePickList(aSourceString: string): TArray<string>;
var
    x: integer;
    xQuery: TDataProvider;
begin
    try
        // 4 Möglichkeiten:
        if (Pos(TMethodVariableUtils.cIdentPicklistPath, Uppercase(aSourceString)) = 1) then
        begin

            // a) Zeilen aus Datei lesen
            result := TFileUtilities.TakeAllStringsFromFile(Copy(aSourceString, 7, Length(aSourceString)));

            // b) SQL-Datei
            if (Length(result) > 0) and (Pos(TMethodVariableUtils.cIdentPicklistSQL, UpperCase(result[0])
                ) = 1) then
            begin
                for x := 0 to Length(result) - 1 do
                    aSourceString := result[x] + ' ';
                SetLength(result, 0);
            end
            else
                EXIT; // a) Zeilen sind schon in der Picklist
        end;

        // b/c) SQl-String auswerten
        if (Pos(TMethodVariableUtils.cIdentPicklistSQL, UpperCase(aSourceString)) = 1) then
        begin
            xQuery := TDataProviderFactory.Instance.CreateDataProvider();
            try
                xQuery.SelectAndOpen(aSourceString, true);
                try
                    SetLength(result, xQuery.RecordCount);
                    x := 0;
                    while not xQuery.EOF do
                    begin
                        result[x] := xQuery.Fields[0].AsString;
                        Inc(x);
                        xQuery.Next;
                    end;
                finally
                    xQuery.Close;
                end;
            finally
                FreeAndNil(xQuery);
            end;

            EXIT;
        end;

        // d) durch Komma getrennte strings
        result := TStringUtilities.StringToStringArray(aSourceString, ',');

    except
        SetLength(result, 1);
        result[0] := 'Error reading list items';
    end;
end;

class function TMethodVariableUtils.GetCompareOrder(const aRequestOrder, aStandardOrder: integer): integer;
const
    cStandardOrderAddValue = 1000000;
begin
    if (aRequestOrder <= 0) then
    begin
        // wir wollen sicher gehen, dass Einträge ohne explizite Order-Angabe hinten angezeigt werden
        EXIT(aStandardOrder + cStandardOrderAddValue);
    end;

    EXIT(aRequestOrder);
end;


end.
