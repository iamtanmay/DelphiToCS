{ --------------------------------------------------------------------------------------------------
  Copyright © 2007 Zinsser Analytic GmbH - All rights reserved.
  Author       : pk
  Description  :
  --------------------------------------------------------------------------------------------------
  Revision History:
  date     op  method                       track-no improvement/change
  -------- --  ---------------------------  -------- -----------------------------------------------
  27.08.07 pk                               TN3788   Code moved here from ParserIdentifier.pas
  06.08.08 pk  ReadWriter                   TN4165.1 New property
  06.11.08 pk                               TN4279   uses ParserIdentDataType
  26.05.09 pk  TParserStoredIdent.EditValue TN4348   add ofNoChangeDir to xOpenDialog.Options so that current system directory is not changed
  10.08.09 wl                               TN4702   Strings werden jetzt direkt geladen
  04.11.09 pk                               TN4843   Replace TObjectList, TStringList, TStrings with Generics Lists and TStringArray
  09.02.10 pk                               TN4973   use TDialogUtils.SelectDirectory instead of FileCtrl.SelectDirectory
  11.03.10 ts  CreatePickList               TN5023   Except-Block -> Länge von result muss 1 sein, sonst Access Violation
  13.04.10 wl                               TN5044   uses FileUtilities
  07.05.10 pk  fDefValue                    TN5092   Now Object instead of a simple string
  18.06.10 pk  CreatePickList               TN5152.1 uses DataProvider instead of TQuery
  01.03.12 wl                               TN5822   uses geändert
  14.12.12 wl  IsNonInputIdent              TN6054   _!-(Run-)Variablen sind jetzt globale non-input-Variablen
  14.12.12 wl                               TN6054   ReaderWriter ist kein Member mehr
  20.02.13 wl                               TN6055   Zugriff auf DataCache entfernt
  20.02.13 wl                               TN6055   Struktur überarbeitet
  22.02.13 wl  RequestOrder                 TN6094   neu: Kann durch den Anwender gesetzt werden
  22.02.13 wl  StandardOrder                TN6094   bisherige Reihenfolge (nach dem Auftauchen in der Methode)
  22.02.13 wl  CompareOrder                 TN6094   kombiniert RequestOrder & StandardOrder für IComparer
  22.02.13 wl  TParserStoredIdentComparer   TN6094   neu: Kann für Listen mit TParserStoredIdent verwendet werden
  21.03.13 wl  GetCompareOrder              TN6045   --> TMethodVariableUtils
  22.04.13 wl                               TN6095   für MultiPageDialog geändert
  14.05.13 wl                               TN6095   DialogCaption entfernt
  12.08.13 wl  CheckValue                   TN6214   Bei "Is Array" wird auf korrektes Array-Format geprüft
  12.08.13 wl  CreateArgOrArrayArgByValue   TN6214   für Arrays wird ein TArrayArg erzeugt
  10.02.14 ts                               TN6353   Volume,Substance,LiqPar added
  08.04.14 ts                               TN6391   SourceRackName,-Position added
  -------------------------------------------------------------------------------------------------- }

unit ParserStoredIdentifier;


interface


uses
    SysUtils,
    Classes,
    Generics.Defaults,
    Generics.Collections,
    MethodVariableTypes,
    ParserIdentifier,
    EditableParameter,
    ParserIdentDataType;

type
    TParserStoredIdent = class(TEditableParameter)
    private
        fData: TMethodVariableData;
        fStandardOrder: integer;
        fLinkedIdent: TParserIdentifier;

        class procedure CheckEmpty(const aDescription, aValue: string);
        procedure CheckSingleValue(const aDescription, aValue: string; const aPickList: TArray<string>);
        procedure CheckMinMaxValue(const aDescription: string; aValue: TArg);
        class procedure CheckPickList(const aDescription, aValue: string; const aPickList: TArray<string>);
        class function GetNumberValue(const aValue: TArg; out oValue: double): boolean; static;
        function GetMaxValueAsStr: string;
        function GetMinValueAsStr: string;
        function GetName: string;
        procedure SetData(const Value: TMethodVariableData);
        function GetCompareOrder: integer;
        function GetSimpleDescription: string;
    protected
        function GetDescription: string; override;
        function GetColumnWidth: integer; override;
        function GetColumnDescription: string; override;
        function GetDialogType: TEditableParameterDataType; override;
    public
        constructor Create(const aLinkedIdent: TParserIdentifier; const aData: TMethodVariableData;
            aStandardOrder: integer);
        destructor Destroy; override;

        // abgeleitet von IEditableParameter
        function GetPickList(): TArray<string>; override;
        procedure CheckValue(); override;
        function HasEditFunction: boolean; override;

        procedure SetValueToLinkedValue;
        procedure SetValueToDefaultIfUndefined;

        class function ValueToStr(const aAttrValue: TArg): string; static;
        function CreateArgOrArrayArgByValue(const aValue: string): TArg;

        procedure WriteValueToLinkedValue;

        property MinValueAsStr: string read GetMinValueAsStr;
        property MaxValueAsStr: string read GetMaxValueAsStr;

        property Data: TMethodVariableData read fData write SetData;
        property name: string read GetName;
        property LinkedIdent: TParserIdentifier read fLinkedIdent;
        property StandardOrder: integer read fStandardOrder;
        property CompareOrder: integer read GetCompareOrder;
    end;

    TParserStoredIdentComparer = class(TInterfacedObject, IComparer<TParserStoredIdent>)
        function Compare(const Left, Right: TParserStoredIdent): Integer;
    end;


implementation


uses
    Windows,
    Dialogs,
    GeneralTypes,
    ArrayFormat,
    ArrayUtils,
    TypeMapTranslator,
    DialogUtils;

{ TParserStoredIdent }

constructor TParserStoredIdent.Create(const aLinkedIdent: TParserIdentifier; const aData: TMethodVariableData;
    aStandardOrder: integer);
begin
    inherited Create();

    fLinkedIdent := aLinkedIdent;
    fValue := '';
    fStandardOrder := aStandardOrder;
    fData := aData;
end;

destructor TParserStoredIdent.Destroy;
begin
    inherited;
end;

procedure TParserStoredIdent.SetData(const Value: TMethodVariableData);
begin
    fData := Value;
end;

function TParserStoredIdent.GetPickList(): TArray<string>;
begin
    result := TMethodVariableUtils.CreatePickList(fData.PickList);
end;

function TParserStoredIdent.HasEditFunction: boolean;
begin
    EXIT(fData.DataType >= TMethodVariableFormatType.mvfPathName);
end;

function TParserStoredIdent.GetMaxValueAsStr: string;
begin
    if (fData.MaxCheck) then
        EXIT(FloatToStr(fData.MaxValue, TFormatUtils.GetSettingsEnglishUS))
    else
        EXIT('');
end;

function TParserStoredIdent.GetMinValueAsStr: string;
begin
    if (fData.MinCheck) then
        EXIT(FloatToStr(fData.MinValue, TFormatUtils.GetSettingsEnglishUS))
    else
        EXIT('');
end;

function TParserStoredIdent.GetDialogType: TEditableParameterDataType;
begin
    case self.Data.DataType of
        mvfRackName:
            EXIT(scdRackName);
        mvfSourceRackName:
            EXIT(scdSourceRackName);
        mvfRackPos:
            EXIT(scdPosition);
        mvfSourceRackPos:
            EXIT(scdSourcePosition);
        mvfVolume:
            EXIT(scdVolume);
        mvfSubstance:
            EXIT(scdSubstance);
        mvfLiqParam:
            EXIT(scdLiqParam);
        else
            EXIT(scdOther);
    end;
end;

function TParserStoredIdent.GetSimpleDescription: string;
begin
    // Define Label text
    if (fData.RequestText <> '') then
        EXIT(fData.RequestText)
    else
        EXIT(TLanguageString.Read('Parameter: {0}', 'Parameter: {0}', [self.Name]));
end;

function TParserStoredIdent.GetDescription: string;
begin
    result := GetSimpleDescription();

    if (fData.MinCheck) or (fData.MaxCheck) then
    begin
        result := result + '   [' + TMethodVariableUtils.MinMaxValueToStr(fData.MinCheck, fData.MinValue) +
            ' .. ' + TMethodVariableUtils.MinMaxValueToStr(fData.MaxCheck, fData.MaxValue) + ']';
    end;
end;

function TParserStoredIdent.GetColumnDescription: string;
begin
    EXIT(self.Description); // ist bisher gleich, könnte aber auch anders sein
end;

function TParserStoredIdent.GetColumnWidth: integer;
begin
    EXIT(120); // Denkbar, das variabel zu gestalten
end;

function TParserStoredIdent.GetCompareOrder: integer;
begin
    EXIT(TMethodVariableUtils.GetCompareOrder(fData.RequestOrder, fStandardOrder));
end;

procedure TParserStoredIdent.SetValueToLinkedValue;
begin
    if Assigned(fLinkedIdent.Value) then
        fValue := ValueToStr(fLinkedIdent.Value)
    else
        fValue := '';
end;

procedure TParserStoredIdent.SetValueToDefaultIfUndefined();
begin
    if (fValue = '') then
        fValue := fData.DefaultValue;
end;

function TParserStoredIdent.GetName: string;
begin
    EXIT(fLinkedIdent.Name);
end;

class function TParserStoredIdent.GetNumberValue(const aValue: TArg; out oValue: double): boolean;
begin
    result := false;
    if aValue is TDblArg then
    begin
        result := true;
        oValue := (aValue as TDblArg).AsFloat;
    end
    else if aValue is TIntArg then
    begin
        result := true;
        oValue := (aValue as TIntArg).AsInt;
    end;
end;

procedure TParserStoredIdent.CheckMinMaxValue(const aDescription: string; aValue: TArg);
var
    xNumberValue: double;
begin
    if not GetNumberValue(aValue, xNumberValue) then
        EXIT;

    if (fData.MinCheck) then
    begin
        if (xNumberValue < fData.MinValue) then
            raise ESetEditableParameterException.Create(aDescription + TLanguageString.
                Read(': Value [{0}] is smaller than minimum value [{1}].',
                ': Der Wert [{0}] ist kleiner als der Minimalwert [{1}].', [xNumberValue, fData.MinValue]));
    end;

    if (fData.MaxCheck) then
    begin
        if (xNumberValue > fData.MaxValue) then
            raise ESetEditableParameterException.Create(aDescription + TLanguageString.
                Read(': Value [{0}] is bigger than maximum value [{1}].',
                ': Der Wert [{0}] ist größer als der Maximalwert [{1}].', [xNumberValue, fData.MaxValue]));
    end;
end;

class procedure TParserStoredIdent.CheckPickList(const aDescription, aValue: string;
    const aPickList: TArray<string>);
var
    x: integer;
begin
    if Length(aPickList) <= 0 then
        EXIT;

    for x := 0 to high(aPickList) do
    begin
        if (aPickList[x] = aValue) then
            EXIT;
    end;

    raise ESetEditableParameterException.Create(aDescription + TLanguageString.
        Read(': Value [{0}] is not member of the pick list.',
        ': Der Wert [{0}] ist nicht in der Auswahlliste enthalten.', [aValue]));
end;

class procedure TParserStoredIdent.CheckEmpty(const aDescription, aValue: string);
begin
    if (aValue = '') then
        raise ESetEditableParameterException.Create(aDescription + TLanguageString.Read(': Value is empty.',
            ': Der Wert ist leer.'));
end;

procedure TParserStoredIdent.WriteValueToLinkedValue;
begin
    fLinkedIdent.Value := CreateArgOrArrayArgByValue(fValue);
end;

procedure TParserStoredIdent.CheckValue();
var
    xPickList: TArray<string>;
    xValues: TArray<string>;
    x: integer;
    xDescription, xDescription2: string;
begin
    xDescription := self.GetSimpleDescription();
    CheckEmpty(xDescription, fValue);
    xPickList := self.GetPickList();

    // Sonderfall Array
    if (fData.DataIsArray) then
    begin
        if not TArrayFormat.StringIsArrayFormat(fValue) then
            raise ESetEditableParameterException.Create(xDescription + TLanguageString.
                Read(': Value has no correct array format.', ': Der Wert hat kein korrektes Array-Format.'));

        xValues := TArrayFormat.ArrayFormatToStringArray(fValue);
        for x := 0 to high(xValues) do
        begin
            xDescription2 := xDescription + '[' + IntToStr(x + 1) + ']';

            CheckEmpty(xDescription2, xValues[x]);
            CheckSingleValue(xDescription2, xValues[x], xPickList);
        end;
        EXIT;
    end;

    CheckSingleValue(xDescription, fValue, xPickList);
end;

procedure TParserStoredIdent.CheckSingleValue(const aDescription, aValue: string;
    const aPickList: TArray<string>);
var
    xValue: TArg;
begin
    CheckPickList(aDescription, aValue, aPickList);

    // Typ-Umwandlung und Typ-spezifische Checks
    xValue := CreateArgOrArrayArgByValue(aValue);
    try
        CheckMinMaxValue(aDescription, xValue);
    finally
        FreeAndNil(xValue);
    end;
end;

class function TParserStoredIdent.ValueToStr(const aAttrValue: TArg): string;
begin
    if aAttrValue = nil then
        EXIT('');

    EXIT(TArgUtils.DataValueToStr(aAttrValue));
end;

function TParserStoredIdent.CreateArgOrArrayArgByValue(const aValue: string): TArg;
var
    xValues: TArray<string>;
begin
    if aValue = '' then
        EXIT(nil);

    // Sonderfall Arrays: A() wird hier ohne "echtes" Parsing ausgewertet
    if (fData.DataIsArray and TArrayFormat.StringIsArrayFormat(aValue)) then
    begin
        xValues := TArrayFormat.ArrayFormatToStringArray(aValue);
        EXIT(TArgUtils.CreateArrayArgByValue(xValues));
    end;

    EXIT(TArgUtils.CreateArgByValue(aValue));
end;

{ TParserStoredIdentComparer }

function TParserStoredIdentComparer.Compare(const Left, Right: TParserStoredIdent): Integer;
begin
    if Left.CompareOrder < Right.CompareOrder then
        EXIT(-1);
    if Left.CompareOrder > Right.CompareOrder then
        EXIT(1);

    // Wenn Order gleich ist: Zuerst niedrigeres RequestFormat
    if Left.Data.DataType < Right.Data.DataType then
        EXIT(-1);
    if Left.Data.DataType > Right.Data.DataType then
        EXIT(1);

    EXIT(0);
end;


end.
