{ -----------------------------------------------------------------------------------------------------------------------
  Copyright © 2008 Zinsser Analytic GmbH - All rights reserved.
  Author       : Payman Kamali (pk)
  Description  :
  -----------------------------------------------------------------------------------------------------------------------
  Revision History:
  date     op  method                       track-no improvement/change
  -------- --  ---------------------------  -------- --------------------------------------------------------------------
  24.06.08 pk                               TN4148   uses UpdateManagerCommonTypes
  11.04.11 wl                               TN5549   uses Fileutilities
  27.02.13 wl                               TN6045   uses geändert
  ---------------------------------------------------------------------------------------------------------------------- }

unit TableUpdateDataMap;


interface


uses
    Generics.Collections;

type
    TTableUpdateDataMap = class

    end;

    TTableUpdateCopyMatchingDataMap = class(TTableUpdateDataMap)
    protected
        fExceptionFields: TList<string>;
    public
        constructor Create(const aExceptionFields: array of string);
        property ExceptionFields: TList<string>read fExceptionFields;
    end;

    TTableUpdateCopyFieldDataMap = class(TTableUpdateDataMap)
    protected
        fSourceField, fDestField: string;
    public
        constructor Create(aSourceField: string; aDestField: string);
        property SourceField: string read fSourceField;
        property DestField: string read fDestField;
    end;

    TTableUpdateDataMapFunc = procedure(const aSourceValsArray: variant; var vDestVal: variant) of object;

    TTableUpdateCopyFieldWithFuncDataMap = class(TTableUpdateDataMap)
    protected
        fDataMapFunc: TTableUpdateDataMapFunc;
        fSourceFields: TArray<string>;
        fDestField: string;
    public
        constructor Create(aSourceFields: TArray<string>; aDestField: string;
            aDataMapFunc: TTableUpdateDataMapFunc);
        property DataMapFunc: TTableUpdateDataMapFunc read fDataMapFunc;
        property SourceFields: TArray<string>read fSourceFields;
        property DestField: string read fDestField;
    end;

    TTableUpdateConstFieldDataMap = class(TTableUpdateDataMap)
    protected
        fValue: variant;
        fDestField: string;
        fUpdateOnlyIfEmpty: boolean;
    public
        constructor Create(const aValue: variant; aDestField: string; aUpdateOnlyIfEmpty: boolean);
        property Value: variant read fValue;
        property DestField: string read fDestField;
        property UpdateOnlyIfEmpty: boolean read fUpdateOnlyIfEmpty;
    end;


implementation


{ TTableUpdateCopyMatchingDataMap }

constructor TTableUpdateCopyMatchingDataMap.Create(const aExceptionFields: array of string);
var
    x: integer;
begin
    inherited Create();
    fExceptionFields := TList<string>.Create;
    for x := 0 to high(aExceptionFields) do
        fExceptionFields.Add(aExceptionFields[x]);
end;

{ TTableUpdateCopyFieldDataMap }

constructor TTableUpdateCopyFieldDataMap.Create(aSourceField, aDestField: string);
begin
    inherited Create();
    fSourceField := aSourceField;
    fDestField := aDestField;
end;

{ TTableUpdateCopyFieldWithFuncDataMap }

constructor TTableUpdateCopyFieldWithFuncDataMap.Create(aSourceFields: TArray<string>; aDestField: string;
    aDataMapFunc: TTableUpdateDataMapFunc);
begin
    inherited Create();
    fDestField := aDestField;
    fSourceFields := aSourceFields;
    fDataMapFunc := aDataMapFunc;
end;

{ TTableUpdateConstFieldDataMap }

constructor TTableUpdateConstFieldDataMap.Create(const aValue: variant; aDestField: string;
    aUpdateOnlyIfEmpty: boolean);
begin
    inherited Create();
    fValue := aValue;
    fDestField := aDestField;
    fUpdateOnlyIfEmpty := aUpdateOnlyIfEmpty;
end;


end.
