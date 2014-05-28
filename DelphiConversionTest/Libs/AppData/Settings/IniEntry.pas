{ --------------------------------------------------------------------------------------------------
  Copyright © 2002 Zinsser Analytic GmbH - All rights reserved.
  Author       : Wolfgang Lyncke (wl)
  Project      : ZACommon.dll
  Description  : The standerd types (boolean, string, integer, float) of ini file entries defined
  as classes.
  --------------------------------------------------------------------------------------------------
  Revision History:
  Date     op  Method                       Track-no Improvement/Change
  -------- --  ---------------------------  -------- -----------------------------------------------
  24.09.02 wl                               TN1293.1 initial version
  10.10.02 wl  GetIdent, GetSection         TN1293.2 new
  11.10.02 wl  TFloatEntry.SetValue         TN1293.2 Decimal punkt wird ersetzt
  15.10.02 wl                               TN1293.1 TFloatEntry debuggt
  21.10.02 wl  GetCompleteIdent             TN1293.1 entfernt
  21.10.02 wl  Value-Set-Funktionen         TN1293.1 Bugfix
  20.12.02 wl  TIniEntryImpl                TN1293.5 abgeleitet von komlett abstrakter Klasse TIniEntry, definiert in CommonTypes
  20.12.02 wl                               TN1293.5 neu: Zugriff auf TResLoader
  02.01.03 wl  GetValueFrom.. methods       TN1293.5 defined as class methods
  08.01.03 wl  TBoolEntry.SetValue          TN1293.5 value = '' -> false
  08.01.03 wl  TFloatEntry.SetValue         TN1293.5 value = '' -> 0
  13.01.03 tbh INT_INTEGERENTRY_DEFAULTs    TN1402 Vorgaben auf Integer-Grenzen gesetzt
  14.01.03 wl  TIniEntry                    TN1293.5 Namen geändert TIniTypeImpl -> TIniType, TIniType -> IIniType
  16.01.03 wl  TIniEntry                    TN1293.5 FValueExists von TiniEntry nach TSimpleIniEntry
  16.01.03 wl  TSimpleIniEntry              TN1293.5 neu: FParentDescrRes (für Sub-Entries)
  10.02.03 wl  TIniEntry.Create             TN1295.3 FIdent & FSection werden auf maximale Länge gekürzt
  31.08.07 wl                               TN3811.4 ist nicht mehr von IIniEntry abgeleitet
  06.08.09 wl                               TN4702   Strings werden direkt geladen
  09.02.10 pk                               TN4973   UnitName changed to ValueUnitName to avoid overwriting TObject.UnitName
  27.10.10 wl  TPasswordStringEntry         TN5300   Sting-Eintrag mit der Eigenschaft ValueHidden = true
  23.11.10 wl  fValueHidden                 TN5300   ValueHidden kann für alle gesetzt werden und macht TPasswordStringEntry überflüssig
  -------------------------------------------------------------------------------------------------- }

unit IniEntry;


interface


uses
    SysUtils,
    CommonTypes;

type
    EInvalidIniValue = class(Exception);

    TIniEntry = class
    private
        FSection: string;
        FIdent: string;
        fValueHidden: boolean;
    protected
        FDescription: string;

        // Abstract methods:
        procedure SetValue(aValue: string); virtual; abstract;
        function GetValue: string; virtual; abstract;
        function GetDefaultValue: string; virtual; abstract;
        function GetValueUnitName: string; virtual; abstract;
        function GetValueExists: boolean; virtual; abstract;
        procedure SetValueExists(aValueExists: boolean); virtual; abstract;
        // Protected methods:
        procedure CheckValueLength(aValue: string); virtual;
        function GetSection: string; virtual;
        function GetIdent: string; virtual;
        function GetDescription: string; virtual;
    protected
        constructor Create(const aSection, aIdent, aDescription: string);
    public
        // Properties
        property Section: string read GetSection;
        property Ident: string read GetIdent;
        property DefaultValue: string read GetDefaultValue;
        property Value: string read GetValue write SetValue;
        property Description: string read GetDescription;
        property ValueUnitName: string read GetValueUnitName;
        property ValueExists: boolean read GetValueExists write SetValueExists;
        property ValueHidden: boolean read fValueHidden write fValueHidden;
    end;

    TSingleIniEntry = class(TIniEntry)
    private
        FParentDescription: string;
        FCurrentValue: string;
        FValueUnitName: string;
        FDefaultValue: string;
        FDefaultSet: boolean; // internal: if FDefaultValue is set (initialized)
        FValueSet: boolean; // internal: if FCurrentValue is set (initialized)
        FValueExists: boolean; // this value is set from outside: If the entry is set in the ini file
    protected
        procedure SetValue(aValue: string); override; // may raise EInvalidIniValue!
        function GetValue: string; override;
        function GetDefaultValue: string; override;
        function GetValueUnitName: string; override;
        function GetValueExists: boolean; override;
        procedure SetValueExists(aValueExists: boolean); override;
    protected
        // Constructor/Destructor (protected!)
        constructor Create(const aSection, aIdent, aDefault, aUnitName, aDescription: string);
        constructor CreateWithoutDefault(aSection, aIdent: string);
    public
        function GetExtDescription: string;
        // properties
        property ParentDescription: string read FParentDescription write FParentDescription;
    end;

    TStringEntry = class(TSingleIniEntry)
    public
        // Constructor/Destructor
        constructor Create(const aSection, aIdent, aDefault, aUnitName, aDescription: string); overload;
        constructor Create(const aSection, aIdent, aDefault, aDescription: string;
            aValueHidden: boolean); overload;
        constructor CreateWithoutDefault(aSection, aIdent: string);
    end;

    TBoolEntry = class(TSingleIniEntry)
    private
        class function GetValueFromBool(aBoolValue: boolean): string;
        procedure SetBoolValue(aBoolValue: boolean);
        function GetBoolValue: boolean;
    protected
        procedure SetValue(aValue: string); override; // may raise EInvalidIniValue!
    public
        // Constructor/Destructor
        constructor Create(const aSection, aIdent: string; aDefault: boolean;
            const aUnitName, aDescription: string);
        constructor CreateWithoutDefault(aSection, aIdent: string);
        // Properties
        property BoolValue: boolean read GetBoolValue write SetBoolValue;
    end;

    TIntegerEntry = class(TSingleIniEntry)
    private
        FCheckRange: boolean;
        FMinValue, FMaxValue: Longint;
        class function GetValueFromInteger(aIntegerValue: Longint): string;
        procedure SetIntegerValue(aIntegerValue: Longint);
        function GetIntegerValue: Longint;
    protected
        procedure SetValue(aValue: string); override; // may raise EInvalidIniValue!
    public
        // Constructor/Destructor
        constructor CreateWithoutDefault(aSection, aIdent: string);
        constructor Create(aSection, aIdent: string; aDefault: Longint;
            aUnitName, aDescription: string); overload;
        constructor Create(aSection, aIdent: string; aDefault: Longint; aUnitName, aDescription: string;
            aMinValue, aMaxValue: Longint); overload;
        // Properties
        property IntegerValue: Longint read GetIntegerValue write SetIntegerValue;
    end;

    TFloatEntry = class(TSingleIniEntry)
    private
        FCheckRange: boolean;
        FMinValue, FMaxValue: double;
        class function GetValueFromFloat(aFloatValue: double): string;
        procedure SetFloatValue(aFloatValue: double);
        function GetFloatValue: double;
    protected
        procedure SetValue(aValue: string); override; // may raise EInvalidIniValue!
    public
        // Constructor/Destructor
        constructor CreateWithoutDefault(aSection, aIdent: string);
        constructor Create(aSection, aIdent: string; aDefault: double;
            aUnitName, aDescription: string); overload;
        constructor Create(aSection, aIdent: string; aDefault: double; aUnitName, aDescription: string;
            aMinValue, aMaxValue: double); overload;
        // Properties
        property FloatValue: double read GetFloatValue write SetFloatValue;
    end;


implementation


uses
    GeneralTypes,
    IniDataWrapper;

{ TIniEntry }

constructor TIniEntry.Create(const aSection, aIdent, aDescription: string);
begin
    FIdent := Copy(aIdent, 1, TIniDataWrapper.GetFieldLength_Ident);
    FSection := Copy(aSection, 1, TIniDataWrapper.GetFieldLength_Section);
    FDescription := aDescription;
end;

function TIniEntry.GetSection: string;
begin
    result := FSection;
end;

function TIniEntry.GetIdent: string;
begin
    result := FIdent;
end;

procedure TIniEntry.CheckValueLength(aValue: string);
begin
    // Check length by using the constant value of field "Value" in table "Inifiles.db"
    if (Length(aValue) > TIniDataWrapper.GetFieldLength_Value) then
        raise EInvalidIniValue.Create(TLanguageString.Read('Identifier [{0}] {1}: Value string is too long!',
            'Identifizierer [{0}] {1}: Zeichenkette ist zu lang!', [FSection, FIdent]));
end;

function TIniEntry.GetDescription: string;
begin
    result := FDescription;
end;

{ TSingleIniEntry }

constructor TSingleIniEntry.Create(const aSection, aIdent, aDefault, aUnitName, aDescription: string);
begin
    inherited Create(aSection, aIdent, aDescription);

    FValueUnitName := aUnitName;

    // so lange nichts anderes gesetzt, gilt der Default
    SetValue(aDefault);
    FDefaultValue := FCurrentValue;
    FDefaultSet := true;
    FValueSet := true;
    FParentDescription := '';
end;

constructor TSingleIniEntry.CreateWithoutDefault(aSection, aIdent: string);
begin
    inherited Create(aSection, aIdent, '');

    FValueUnitName := '';

    FDefaultSet := false;
    FValueSet := false;
end;

procedure TSingleIniEntry.SetValue(aValue: string);
begin
    CheckValueLength(aValue);
    FCurrentValue := aValue;
    FValueSet := true;
end;

function TSingleIniEntry.GetValue: string;
begin
    // Check if FCurrentValue has been set
    if (not FValueSet) then
        raise EInvalidIniValue.Create(TLanguageString.Read('Identifier [{0}] {1}: Value is not set!',
            'Identifizierer [{0}] {1}: Wert ist nicht gesetzt!', [FSection, FIdent]));
    result := FCurrentValue;
end;

function TSingleIniEntry.GetDefaultValue: string;
begin
    // Check if FDefaultValue has been set
    if (not FDefaultSet) then
        raise EInvalidIniValue.Create(TLanguageString.Read('Identifier [{0}] {1}: Default value is not set!',
            'Identifizierer [{0}] {1}: Vorgabewert nicht gesetzt!', [FSection, FIdent]));
    result := FDefaultValue;
end;

function TSingleIniEntry.GetValueUnitName: string;
begin
    result := FValueUnitName;
end;

function TSingleIniEntry.GetExtDescription: string;
begin
    result := GetDescription;

    if (FParentDescription <> '') then
        result := FParentDescription + ': ' + result;
end;

function TSingleIniEntry.GetValueExists: boolean;
begin
    result := FValueExists;
end;

procedure TSingleIniEntry.SetValueExists(aValueExists: boolean);
begin
    FValueExists := aValueExists;
end;

{ TStringEntry }

constructor TStringEntry.Create(const aSection, aIdent, aDefault, aUnitName, aDescription: string);
begin
    inherited Create(aSection, aIdent, aDefault, aUnitName, aDescription);
end;

constructor TStringEntry.Create(const aSection, aIdent, aDefault, aDescription: string;
    aValueHidden: boolean);
begin
    inherited Create(aSection, aIdent, aDefault, '', aDescription);
    self.ValueHidden := aValueHidden;
end;

constructor TStringEntry.CreateWithoutDefault(aSection, aIdent: string);
begin
    inherited CreateWithoutDefault(aSection, aIdent);
end;

{ TBoolEntry }

const
    STR_BOOLENTRY_FALSE = '0';
    STR_BOOLENTRY_TRUE = '1';

constructor TBoolEntry.Create(const aSection, aIdent: string; aDefault: boolean;
    const aUnitName, aDescription: string);
begin
    inherited Create(aSection, aIdent, GetValueFromBool(aDefault), aUnitName, aDescription);
end;

constructor TBoolEntry.CreateWithoutDefault(aSection, aIdent: string);
begin
    inherited CreateWithoutDefault(aSection, aIdent);
end;

class function TBoolEntry.GetValueFromBool(aBoolValue: boolean): string;
begin
    if (aBoolValue) then
        result := STR_BOOLENTRY_TRUE
    else
        result := STR_BOOLENTRY_FALSE;
end;

procedure TBoolEntry.SetValue(aValue: string);
begin
    // if aValue = '' then value is set to false
    if (aValue = '') then
        aValue := STR_BOOLENTRY_FALSE;

    if (aValue = STR_BOOLENTRY_FALSE) or (aValue = STR_BOOLENTRY_TRUE) then
        inherited SetValue(aValue)
    else
        raise EInvalidIniValue.Create(TLanguageString.Read('Identifier [{0}] {1}: Value is no boolean value!',
            'Identifizierer [{0}] {1}: Wert ist kein Boolescher Wert!', [FSection, FIdent]));
end;

procedure TBoolEntry.SetBoolValue(aBoolValue: boolean);
begin
    SetValue(GetValueFromBool(aBoolValue));
end;

function TBoolEntry.GetBoolValue: boolean;
begin
    if (FCurrentValue = STR_BOOLENTRY_TRUE) then
        result := true
    else
        result := false;
end;

{ TIntegerEntry }

const
    INT_INTEGERENTRY_DEFAULT_MIN = -2147483647;

const
    INT_INTEGERENTRY_DEFAULT_MAX = 2147483646;

constructor TIntegerEntry.Create(aSection, aIdent: string; aDefault: Longint; aUnitName, aDescription: string;
    aMinValue, aMaxValue: Longint);
begin
    FMinValue := aMinValue;
    FMaxValue := aMaxValue;
    FCheckRange := true;
    inherited Create(aSection, aIdent, GetValueFromInteger(aDefault), aUnitName, aDescription);
end;

constructor TIntegerEntry.Create(aSection, aIdent: string; aDefault: Longint;
    aUnitName, aDescription: string);
begin
    FMinValue := INT_INTEGERENTRY_DEFAULT_MIN;
    FMaxValue := INT_INTEGERENTRY_DEFAULT_MAX;
    FCheckRange := true;
    inherited Create(aSection, aIdent, GetValueFromInteger(aDefault), aUnitName, aDescription);
end;

constructor TIntegerEntry.CreateWithoutDefault(aSection, aIdent: string);
begin
    FMinValue := INT_INTEGERENTRY_DEFAULT_MIN;
    FMaxValue := INT_INTEGERENTRY_DEFAULT_MAX;
    FCheckRange := true;
    inherited CreateWithoutDefault(aSection, aIdent);
end;

class function TIntegerEntry.GetValueFromInteger(aIntegerValue: Longint): string;
begin
    result := IntToStr(aIntegerValue);
end;

procedure TIntegerEntry.SetValue(aValue: string);
var
    xIntResult, xError: integer;
begin
    // if aValue = '' then value is set to 0
    if (aValue = '') then
        aValue := '0';

    // check if it is really an integer value
    val(aValue, xIntResult, xError);
    if (xError <> 0) then
        raise EInvalidIniValue.Create(TLanguageString.Read('Identifier [{0}] {1}: Value is no integer value!',
            'Identifizierer [{0}] {1}: Wert ist kein Integerwert!', [FSection, FIdent]));

    // check range
    if (FCheckRange) then
    begin
        if (xIntResult < FMinValue) then
            raise EInvalidIniValue.Create(TLanguageString.
                Read('Identifier [{0}] {1}: Value is smaller than minimum value!',
                'Identifizierer [{0}] {1}: Wert ist kleiner als der Minimalwert!', [FSection, FIdent]));
        if (xIntResult > FMaxValue) then
            raise EInvalidIniValue.Create(TLanguageString.
                Read('Identifier [{0}] {1}: Value is bigger than maximum value!',
                'Identifizierer [{0}] {1}: Wert ist größer als der Maximalwert!', [FSection, FIdent]));
    end;

    inherited SetValue(aValue)
end;

procedure TIntegerEntry.SetIntegerValue(aIntegerValue: Longint);
begin
    SetValue(GetValueFromInteger(aIntegerValue));
end;

function TIntegerEntry.GetIntegerValue: Longint;
begin
    result := StrToInt(FCurrentValue);
end;

{ TFloatEntry }

constructor TFloatEntry.Create(aSection, aIdent: string; aDefault: double; aUnitName, aDescription: string;
    aMinValue, aMaxValue: double);
begin
    FMinValue := aMinValue;
    FMaxValue := aMaxValue;
    FCheckRange := true;
    inherited Create(aSection, aIdent, GetValueFromFloat(aDefault), aUnitName, aDescription);
end;

constructor TFloatEntry.Create(aSection, aIdent: string; aDefault: double; aUnitName, aDescription: string);
begin
    FCheckRange := false;
    inherited Create(aSection, aIdent, GetValueFromFloat(aDefault), aUnitName, aDescription);
end;

constructor TFloatEntry.CreateWithoutDefault(aSection, aIdent: string);
begin
    inherited CreateWithoutDefault(aSection, aIdent);
end;

class function TFloatEntry.GetValueFromFloat(aFloatValue: double): string;
begin
    result := FloatToStr(aFloatValue); // TBD_WL testen! Könnte problematisch sein.
end;

procedure TFloatEntry.SetFloatValue(aFloatValue: double);
begin
    SetValue(GetValueFromFloat(aFloatValue))
end;

function TFloatEntry.GetFloatValue: double;
var
    xError: integer;
begin
    // check if it is really an double value
    val(FCurrentValue, result, xError);
    if (xError <> 0) then
        raise EInvalidIniValue.Create(TLanguageString.Read('Identifier [{0}] {1}: Value is no float value!',
            'Identifizierer [{0}] {1}: Wert ist kein Fließkommawert!', [FSection, FIdent]));
end;

procedure TFloatEntry.SetValue(aValue: string);
var
    xDoubleResult: double;
    xSeparator: integer;
    xError: integer;
begin
    // if aValue = '' then value is set to 0
    if (aValue = '') then
        aValue := '0';

    // replace country specific separator with '.'
    xSeparator := pos(',', aValue);
    if (xSeparator > 0) then
        aValue[xSeparator] := '.';

    // StringReplace(aValue, ',', '.', []);  // funktioniert nicht (?)

    // check if it is really an double value
    val(aValue, xDoubleResult, xError);
    if (xError <> 0) then
        raise EInvalidIniValue.Create(TLanguageString.Read('Identifier [{0}] {1}: Value is no float value!',
            'Identifizierer [{0}] {1}: Wert ist kein Fließkommawert!', [FSection, FIdent]));

    // check range
    if (FCheckRange) then
    begin
        if (xDoubleResult < FMinValue) then
            raise EInvalidIniValue.Create(TLanguageString.
                Read('Ini entry [{0}] {1}: Value is smaller than minimum value!',
                'INI-Eintrag [{0}] {1}: Wert ist kleiner als der Minimalwert!', [FSection, FIdent]));
        if (xDoubleResult > FMaxValue) then
            raise EInvalidIniValue.Create(TLanguageString.
                Read('Ini entry [{0}] {1}: Value is bigger than maximum value!',
                'INI-Eintrag [{0}] {1}: Wert ist größer als der Maximalwert!', [FSection, FIdent]));
    end;

    inherited SetValue(aValue)
end;


end.
