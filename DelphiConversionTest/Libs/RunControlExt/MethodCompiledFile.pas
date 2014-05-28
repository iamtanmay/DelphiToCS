{ ----------------------------------------------------------------------------------------------------------------------
  Copyright © 2008 Zinsser Analytic GmbH - All rights reserved.
  Author       : Payman Kamali (pk)
  Description  :
  ----------------------------------------------------------------------------------------------------------------------
  Revision History:
  date     op  method                                track-no  improvement/change
  -------- --  ------------------------------------- --------  ---------------------------------------------------------
  06.11.08 pk                                        TN4280    Initial Revision
  10.11.08 pk                                        TN4280    uses TThreadAPI
  10.11.08 pk  SetIdent                              TN4279    now with logging
  10.11.08 pk  TMethodEvalTable                      TN4279    some functions made public
  08.01.09 pk  AllocateResource                      TN4279    change @ to Pointer
  17.02.09 pk                                        TN4232    Various changes for Run Trace
  25.02.09 pk  FindOrCreateIDent                     TN4279    made public
  19.08.09 wl                                        TN4227   es werden immer englische Einstellungen benutzt
  07.05.10 pk                                        TN5092    various changes for new Parser Array Type
  17.05.10 pk                                        TN5092    changes for Restart for new Array Type
  07.06.10 pk                                        TN5077    uses changed
  15.11.10 pk                                        TN5340    Changes to prevent memory leak
  29.06.11 wl  SetIdentifierValue                    TN5618   Der ArrayIndex wird über den Namen der Variable bestimmt
  19.09.11 wl  SetIdentifierValue                    TN5694   Fehlermeldung korrigiert
  01.03.12 wl  SetIdentifierArrayFromStrArray        TN5820   Erzeugt Array-Variable mit allen Werten
  01.03.12 wl  SetIdentifierValueArrayLength         TN5820   schreibt eine Zeile ins Log
  01.03.12 wl                                        TN5822   uses geändert
  02.03.12 wl  SetIdentVal...                        TN5822   neue Set-Funktionen für alle Variablentypen
  05.07.12 wl                                        TN5917   Funktionen von TParserIdentValueUtils nach TArgUtils verschoben
  27.03.13 wl                                        TN6045   TArrayArg-Änderungen
  25.06.13 wl                                        TN6178   uses Identifier
  15.08.13 wl                                        TN6223   uses geändert
  01.10.13 wl  SetIdentValDefineByVariant            TN6264   neu
  10.01.14 ts  SetArrayIdentValue/SetIdentValue      TN6337   Only copy object and write to trace if tracemanager is enabled
  ---------------------------------------------------------------------------------------------------------------------- }

unit MethodCompiledFile;


interface


uses
    MethBuildDataset,
    ThreadClasses,
    Streamable,
    Identifier,
    RunStepBuilder,
    MemoryClasses,
    ParserEvalTable,
    ParserIdentDataType,
    ParserIdentifier;

type

    TMethodProgramCode = class(TProgramCode)
    private
        fCompiledCode: TMethodCompiledCode;
        fOwnsCode: boolean;
    public
        constructor Create();
        destructor Destroy(); override;
        procedure SetCompiledCode(const aCompiledCode: TMethodCompiledCode; const aOwnsCode: boolean);
        property CompiledCode: TMethodCompiledCode read fCompiledCode;
    end;

    TMethodCompiledFile = class(TCompiledFile)
    private
        fOwnsCode: boolean;
        fCompiledCode: TMethodCompiledCode;
    public
        constructor Create();
        destructor Destroy(); override;
        property CompiledCode: TMethodCompiledCode read fCompiledCode;
        property OwnsCode: boolean read fOwnsCode write fOwnsCode;
    end;

    { TODO -owl : TMethodEvalTable ist ein nutzloses Methoden-Sammelsurium, das weg muss! }
    TMethodEvalTable = class
    private
        class procedure SetIdentValue(const aIdent: TIdentifier; const aValue: TArg);
        class procedure SetArrayIdentValue(const aIdent: TIdentifier; const aArrayIndex: integer;
            const aValue: TArg);
        class procedure WriteIdentToTrace(aIdent: TIdentifier; const aOldValue: TStreamableItem);

        function GetIdentValue(const aIdent: TIdentifier; const aArrayIndex: integer): TArg;

        class function GetLocalIdentList(): TIdentifierList;
        class function GetGlobalIdentList(): TThreadSafeIdentifierList;
        class function GetCallArgs(): TIdentifierList;
        class procedure LogIdent(const aIdent: TIdentifier; const aArrayIndex: integer);
        class procedure CheckArrayBounds(const aIdent: TIdentifier; const aArrayIndex: integer);
        class function GetIdentNameWithIndex(const aIdentName: string; const aArrayIndex: integer): string;

        class procedure SetIdentValRawWithIndex(const aIdentName: string; const aArrayIndex: integer;
            const aAttrValue: TArg);
        class procedure SetIdentValAndFreeArg(const aIdentName: string; const aAttrValue: TArg);
        class procedure SetIdentValAndFreeWithIndex(const aIdentName: string; const aArrayIndex: integer;
            const aAttrValue: TArg);
    public
        class function FindOrCreateIdent(const aIdentName: string): TIdentifier;
        class function FindIdent(const aIdentName: string): TIdentifier;
        class function AllocateReference(const aObj: TObject): integer;
        class procedure DeallocateReference(const aID: integer);
        class function FindObjectByReference(const aID: integer): TObject;
        class procedure ReferenceChanged(const aID: integer);

        class procedure SetIdentValRaw(const aIdentName: string; const aValue: TArg);

        class procedure SetIdentValDefineType(const aIdentName: string; const aValue: string);
        class procedure SetIdentValDefineByVariant(const aIdentName: string; const aValue: variant);
        class procedure SetIdentValAsStr(const aIdentName: string; const aValue: string);
        class procedure SetIdentValAsInt(const aIdentName: string; const aValue: integer);
        class procedure SetIdentValAsFloat(const aIdentName: string; const aValue: double);
        class procedure SetIdentValAsBool(const aIdentName: string; const aValue: boolean);
        class procedure SetIdentValAsRef(const aIdentName: string; const aValue: integer);

        class procedure SetIdentValArrayDefineType(const aIdentName: string; const aValues: TArray<string>);
        class procedure SetIdentValArrayAsFloat(const aIdentName: string; const aValues: TArray<double>);
        class procedure SetIdentValArrayAsStr(const aIdentName: string; const aValues: TArray<string>);
        class procedure SetIdentValArrayAsInt(const aIdentName: string; const aValues: TArray<integer>);
        class procedure SetIdentValArrayAsBool(const aIdentName: string; const aValues: TArray<boolean>);

        procedure GetIdentifierValue(const aIdentName: string; const aArrayIndex: integer; out oValue: TArg);
        class procedure SetIdentifierValueArrayLength(const aIdentName: string; const aArrayLength: integer);
    end;

    TMethodAddressSpace = class(TAddressSpace)
    private
        fEvalTable: TMethodEvalTable;
        function GetProgramCode(): TMethodProgramCode;
    protected
        procedure CreateProgramCode(); override;
        procedure CopyCodeIntoAddressSpace(const aFile: TCompiledFile);
    public
        constructor Create();
        destructor Destroy(); override;
        procedure Load(const aFile: TCompiledFile); override;
        property ProgramCode: TMethodProgramCode read GetProgramCode;
    end;


implementation


uses
    GeneralTypes,
    ThrdMan,
    ThreadAPI,
    SysUtils,
    LogManager,
    RunTraceManager,
    CallStackDataCache,
    HeapDataCache,
    ReferenceIdentValueMemory,
    ParserToken,
    ParserTokenizer,
    MethBuildField,
    TypeMapTranslator;

{ TMethodProgramCode }

constructor TMethodProgramCode.Create;
begin
    inherited Create();
    fCompiledCode := nil;
    fOwnsCode := false;
end;

destructor TMethodProgramCode.Destroy;
begin
    if fOwnsCode then
    begin
        FreeAndnil(fCompiledCode);
    end;

    inherited;
end;

procedure TMethodProgramCode.SetCompiledCode(const aCompiledCode: TMethodCompiledCode;
    const aOwnsCode: boolean);
begin
    fCompiledCode := aCompiledCode;
    fOwnsCode := aOwnsCode;
end;

{ TMethodCompiledFile }

constructor TMethodCompiledFile.Create();
begin
    inherited Create();
    fCompiledCode := TMethodCompiledCode.Create();
    fOwnsCode := true;
end;

destructor TMethodCompiledFile.Destroy();
begin
    if fOwnsCode then
    begin
        FreeAndNil(fCompiledCode);
    end;

    inherited;
end;

{ TMethodAddressSpace }

constructor TMethodAddressSpace.Create;
begin
    inherited Create();
    fEvalTable := TMethodEvalTable.Create();
end;

destructor TMethodAddressSpace.Destroy();
begin
    FreeAndNil(fEvalTable);
    inherited;
end;

procedure TMethodAddressSpace.CreateProgramCode();
begin
    fProgramCode := TMethodProgramCode.Create();
end;

function TMethodAddressSpace.GetProgramCode: TMethodProgramCode;
begin
    result := (fProgramCode as TMethodProgramCode);
end;

procedure TMethodAddressSpace.CopyCodeIntoAddressSpace(const aFile: TCompiledFile);
begin
    // this is a hack!  actually we should be copying the CompiledCode instead of stealing it from the aFile object
    self.ProgramCode.SetCompiledCode((aFile as TMethodCompiledFile).CompiledCode, true);
    (aFile as TMethodCompiledFile).OwnsCode := false;
end;

procedure TMethodAddressSpace.Load(const aFile: TCompiledFile);
begin
    CopyCodeIntoAddressSpace(aFile);

    self.ProgramCode.CompiledCode.EvalTable.SetIdentifierValue := fEvalTable.SetIdentValRaw;
    self.ProgramCode.CompiledCode.EvalTable.GetIdentifierValue := fEvalTable.GetIdentifierValue;
    self.ProgramCode.CompiledCode.EvalTable.SetArrayLength := fEvalTable.SetIdentifierValueArrayLength;
end;

{ TMethodEvalTable }

class function TMethodEvalTable.GetGlobalIdentList: TThreadSafeIdentifierList;
var
    xAddressSpace: TAddressSpace;
begin
    xAddressSpace := TThreadAPI.GetCurrentProcess.AddressSpace;
    result := xAddressSpace.Heap;
end;

class function TMethodEvalTable.GetCallArgs: TIdentifierList;
begin
    result := TThreadAPI.GetCurrentThreadImage.CallStack.CurrentFrame.CallArgs;
end;

class function TMethodEvalTable.GetLocalIdentList: TIdentifierList;
begin
    result := TThreadAPI.GetCurrentThreadImage.CallStack.CurrentFrame.LocalVars;
end;

class procedure TMethodEvalTable.LogIdent(const aIdent: TIdentifier; const aArrayIndex: integer);
begin
    gLogManager.Log(TParserIdentValueUtils.IdentToLogStr(aIdent, aArrayIndex), false);
end;

class procedure TMethodEvalTable.CheckArrayBounds(const aIdent: TIdentifier; const aArrayIndex: integer);
begin
    if aArrayIndex = TArrayArg.UndefinedBoundsIndex then
        EXIT;
    if not(aIdent.Value is TArrayArg) then
    begin
        raise Exception.Create(TTypeSafeFormat.Format('Identifier {0} is not an array', [aIdent.Key]));
    end;
    if aArrayIndex > (aIdent.Value as TArrayArg).MaxUserIndex then
        raise Exception.Create
            (TTypeSafeFormat.Format
            ('ArrayIndex {0} is greater than the maximum allowed index {1} for Identifier {2}',
            [aArrayIndex, (aIdent.Value as TArrayArg).MaxUserIndex, aIdent.Key]));
    if aArrayIndex < (aIdent.Value as TArrayArg).MinUserIndex then
        raise Exception.Create
            (TTypeSafeFormat.Format
            ('ArrayIndex {0} is less than the minimum allowed index {1} for Identifier {2}',
            [aArrayIndex, (aIdent.Value as TArrayArg).MinUserIndex, aIdent.Key]));
end;

function TMethodEvalTable.GetIdentValue(const aIdent: TIdentifier; const aArrayIndex: integer): TArg;
begin
    CheckArrayBounds(aIdent, aArrayIndex);
    result := TParserIdentValueUtils.IdentValueToAttrValue(aIdent.Value, aArrayIndex);
end;

class function TMethodEvalTable.FindIdent(const aIdentName: string): TIdentifier;
begin
    result := self.GetLocalIdentList.FindIdentByName(aIdentName);
    if Assigned(result) then
        EXIT;
    result := self.GetCallArgs.FindIdentByName(aIdentName);
    if Assigned(result) then
        EXIT;
    result := self.GetGlobalIdentList.FindIdentByName(aIdentName);
end;

class procedure TMethodEvalTable.WriteIdentToTrace(aIdent: TIdentifier; const aOldValue: TStreamableItem);
var
    xFound: boolean;
    xIsGlobalIdent: boolean;
begin
    xFound := false;
    xIsGlobalIdent := false;
    if Assigned(GetLocalIdentList.FindIdentByName(aIdent.Key)) then
    begin
        TRunTraceManager.Instance.AddLocalIdentToTrace(aIdent);
        xFound := true;
    end
    else if Assigned(GetGlobalIdentList.FindIdentByName(aIdent.Key)) then
    begin
        TRunTraceManager.Instance.AddGlobalIdentToTrace(aIdent);
        xFound := true;
        xIsGlobalIdent := true;
    end;

    if xFound then
    begin
        TRunTraceManager.Instance.AddIdentValueChangedRunEffect(aIdent, aOldValue, xIsGlobalIdent);
    end;
end;

class procedure TMethodEvalTable.SetArrayIdentValue(const aIdent: TIdentifier; const aArrayIndex: integer;
    const aValue: TArg);
var
    xOldValue, xIdentValue: TStreamableItem;
begin
    if TRunTraceManager.Instance.IsEnabled then
        xOldValue := TObjectCopy<TStreamableItem>.Copy(aIdent.Value);
    try
        xIdentValue := TParserIdentValueUtils.AttrValueToIdentValue(aValue);
        CheckArrayBounds(aIdent, aArrayIndex);
        (aIdent.Value as TArrayArg).SetItemByUserIndex(aArrayIndex, xIdentValue);

        LogIdent(aIdent, aArrayIndex);
        if TRunTraceManager.Instance.IsEnabled then
            WriteIdentToTrace(aIdent, xOldValue);

    finally
        if TRunTraceManager.Instance.IsEnabled then
            FreeAndNil(xOldValue);
    end;
end;

class procedure TMethodEvalTable.SetIdentValue(const aIdent: TIdentifier; const aValue: TArg);
var
    xOldValue, xIdentValue: TStreamableItem;
begin
    if TRunTraceManager.Instance.IsEnabled then
        xOldValue := TObjectCopy<TStreamableItem>.Copy(aIdent.Value);
    try
        xIdentValue := TParserIdentValueUtils.AttrValueToIdentValue(aValue);
        aIdent.Value.Free;
        aIdent.Value := xIdentValue;

        LogIdent(aIdent, TArrayArg.UndefinedBoundsIndex);
        if TRunTraceManager.Instance.IsEnabled then
            WriteIdentToTrace(aIdent, xOldValue);
    finally
        if TRunTraceManager.Instance.IsEnabled then
            FreeAndNil(xOldValue);
    end;
end;

class function TMethodEvalTable.FindOrCreateIdent(const aIdentName: string): TIdentifier;
begin
    result := FindIdent(aIdentName);

    if not Assigned(result) then
    begin
        result := TIdentifier.Create(aIdentName, false);

        if (TParserIdentifier.GetIdentType(aIdentName) = itGlobal) then
            self.GetGlobalIdentList.Add(result)
        else
            self.GetLocalIdentList.Add(result);
    end;
end;

class procedure TMethodEvalTable.SetIdentValArrayAsBool(const aIdentName: string;
    const aValues: TArray<boolean>);
var
    x: integer;
begin
    // Länge des Arrays festlegen
    SetIdentifierValueArrayLength(aIdentName, Length(aValues));

    // Variablen-Werte schreiben
    for x := 0 to high(aValues) do
        SetIdentValAndFreeWithIndex(aIdentName, x + 1, TBoolArg.Create(aValues[x]));
end;

class procedure TMethodEvalTable.SetIdentValArrayAsFloat(const aIdentName: string;
    const aValues: TArray<double>);
var
    x: integer;
begin
    // Länge des Arrays festlegen
    SetIdentifierValueArrayLength(aIdentName, Length(aValues));

    // Variablen-Werte schreiben
    for x := 0 to high(aValues) do
        SetIdentValAndFreeWithIndex(aIdentName, x + 1, TDblArg.Create(aValues[x]));
end;

class procedure TMethodEvalTable.SetIdentValArrayAsInt(const aIdentName: string;
    const aValues: TArray<integer>);
var
    x: integer;
begin
    // Länge des Arrays festlegen
    SetIdentifierValueArrayLength(aIdentName, Length(aValues));

    // Variablen-Werte schreiben
    for x := 0 to high(aValues) do
        SetIdentValAndFreeWithIndex(aIdentName, x + 1, TIntArg.Create(aValues[x]));
end;

class procedure TMethodEvalTable.SetIdentValArrayAsStr(const aIdentName: string;
    const aValues: TArray<string>);
var
    x: integer;
begin
    // Länge des Arrays festlegen
    SetIdentifierValueArrayLength(aIdentName, Length(aValues));

    // Variablen-Werte schreiben
    for x := 0 to high(aValues) do
        SetIdentValAndFreeWithIndex(aIdentName, x + 1, TStrArg.Create(aValues[x]));
end;

class procedure TMethodEvalTable.SetIdentValArrayDefineType(const aIdentName: string;
    const aValues: TArray<string>);
var
    x: integer;
    xDataType: TParserIdentDataType;
begin
    // Länge des Arrays festlegen
    SetIdentifierValueArrayLength(aIdentName, Length(aValues));

    // DataType für alle Werte festlegen (soll für alle gleich sein)
    xDataType := TArgUtils.DefineTypeByValueArray(aValues);

    // Variablen-Werte schreiben
    for x := 0 to high(aValues) do
    begin
        SetIdentValAndFreeWithIndex(aIdentName, x + 1, TArgUtils.CreateArgByType(aValues[x], xDataType));
    end;
end;

class procedure TMethodEvalTable.SetIdentValAsBool(const aIdentName: string; const aValue: boolean);
begin
    SetIdentValAndFreeArg(aIdentName, TBoolArg.Create(aValue));
end;

class procedure TMethodEvalTable.SetIdentValAsFloat(const aIdentName: string; const aValue: double);
begin
    SetIdentValAndFreeArg(aIdentName, TDblArg.Create(aValue));
end;

class procedure TMethodEvalTable.SetIdentValAsInt(const aIdentName: string; const aValue: integer);
begin
    SetIdentValAndFreeArg(aIdentName, TIntArg.Create(aValue));
end;

class procedure TMethodEvalTable.SetIdentValAsRef(const aIdentName: string; const aValue: integer);
begin
    SetIdentValAndFreeArg(aIdentName, TReferenceArg.Create(aValue));
end;

class procedure TMethodEvalTable.SetIdentValAsStr(const aIdentName, aValue: string);
begin
    SetIdentValAndFreeArg(aIdentName, TStrArg.Create(aValue));
end;

class procedure TMethodEvalTable.SetIdentValDefineType(const aIdentName: string; const aValue: string);
begin
    SetIdentValAndFreeArg(aIdentName, TArgUtils.CreateArgByValue(aValue));
end;

class procedure TMethodEvalTable.SetIdentValDefineByVariant(const aIdentName: string; const aValue: variant);
begin
    SetIdentValAndFreeArg(aIdentName, TArgUtils.CreateArgByVariantValue(aValue, true));
end;

class procedure TMethodEvalTable.SetIdentValAndFreeArg(const aIdentName: string; const aAttrValue: TArg);
begin
    try
        SetIdentValRaw(aIdentName, aAttrValue);
    finally
        aAttrValue.Free;
    end;
end;

class procedure TMethodEvalTable.SetIdentValAndFreeWithIndex(const aIdentName: string;
    const aArrayIndex: integer; const aAttrValue: TArg);
begin
    try
        SetIdentValRawWithIndex(aIdentName, aArrayIndex, aAttrValue);
    finally
        aAttrValue.Free;
    end;
end;

class function TMethodEvalTable.GetIdentNameWithIndex(const aIdentName: string;
    const aArrayIndex: integer): string;
begin
    // einfacher Trick: An den Namen wird in Klammmern der Index gehängt
    EXIT(aIdentName + '[' + IntToStr(aArrayIndex) + ']');
end;

class procedure TMethodEvalTable.SetIdentValRawWithIndex(const aIdentName: string; const aArrayIndex: integer;
    const aAttrValue: TArg);
begin
    self.SetIdentValRaw(GetIdentNameWithIndex(aIdentName, aArrayIndex), aAttrValue);
end;

class procedure TMethodEvalTable.SetIdentValRaw(const aIdentName: string; const aValue: TArg);
var
    xIdent: TIdentifier;
    xTokens: TTokenList;
    xArrayIndex: integer;
begin
    if aIdentName = '' then
        EXIT;

    xTokens := TParserTokenizer.TokenizeStatic(aIdentName);
    try
        if (xTokens.Count = 2) and (xTokens[0].TokenType = TTokenType.ttVar) then
        begin
            // Normale Variable ohne Index
            xIdent := FindOrCreateIdent(aIdentName);
            SetIdentValue(xIdent, aValue);
        end
        else if (xTokens.Count = 5) and (xTokens[0].TokenType = TTokenType.ttArrayVar) and
            (xTokens[1].TokenType = TTokenType.ttSquareBracketOpen) and
            (xTokens[3].TokenType = TTokenType.ttSquareBracketClose) then
        begin
            // Array-Variable: Name wird nur aus dem ersten Token genommen
            xIdent := FindOrCreateIdent(xTokens[0].Text);

            // ArrayIndex must be an integer
            if not TryStrToInt(xTokens[2].Text, xArrayIndex) then
                raise Exception.Create
                    (TTypeSafeFormat.Format('The value {0} is not a valid array index for the identifier {1}',
                    [xTokens[2].Text, xTokens[0].Text]));

            SetArrayIdentValue(xIdent, xArrayIndex, aValue);
        end
        else
            raise Exception.Create('Wrong Tokens for Variable');
    finally
        xTokens.Free;
    end;

end;

procedure TMethodEvalTable.GetIdentifierValue(const aIdentName: string; const aArrayIndex: integer;
    out oValue: TArg);
var
    xIdent: TIdentifier;
begin
    xIdent := FindIdent(aIdentName);
    if not Assigned(xIdent) then
        raise Exception.CreateFmt('Identifier %s not initialized', [aIdentName]);

    oValue := self.GetIdentValue(xIdent, aArrayIndex);
end;

class procedure TMethodEvalTable.SetIdentifierValueArrayLength(const aIdentName: string;
    const aArrayLength: integer);
var
    xIdent: TIdentifier;
begin
    xIdent := FindOrCreateIdent(aIdentName);
    if not Assigned(xIdent.Value) then
        xIdent.Value := TArrayArg.Create();

    if not(xIdent.Value is TArrayArg) then
        raise Exception.CreateFmt('Identifier %s is not an Array', [aIdentName]);

    (xIdent.Value as TArrayArg).ArrayLength := aArrayLength;

    gLogManager.Log('Set Array Length: ' + aIdentName + ' to ' + IntToStr(aArrayLength), false);
end;

class function TMethodEvalTable.AllocateReference(const aObj: TObject): integer;
var
    xReferenceIdentValueMemory: TReferenceIdentValueMemory;
begin
    result := TReferenceIdentValueMemoryManager.Instance.Add(aObj);
    xReferenceIdentValueMemory := TReferenceIdentValueMemoryManager.Instance.Find(result);
    TRunTraceManager.Instance.AddReferenceIdentValueMemory(xReferenceIdentValueMemory);
end;

class procedure TMethodEvalTable.DeallocateReference(const aID: integer);
var
    xReferenceIdentValueMemory: TReferenceIdentValueMemory;
begin
    xReferenceIdentValueMemory := TReferenceIdentValueMemoryManager.Instance.Find(aID);
    TRunTraceManager.Instance.RemoveReferenceIdentValueMemory(xReferenceIdentValueMemory);

    TReferenceIdentValueMemoryManager.Instance.Remove(aID);
end;

class function TMethodEvalTable.FindObjectByReference(const aID: integer): TObject;
var
    xReferenceIdentValueMemory: TReferenceIdentValueMemory;
begin
    xReferenceIdentValueMemory := TReferenceIdentValueMemoryManager.Instance.Find(aID);
    result := xReferenceIdentValueMemory.Obj;
end;

class procedure TMethodEvalTable.ReferenceChanged(const aID: integer);
var
    xReferenceIdentValueMemory: TReferenceIdentValueMemory;
begin
    xReferenceIdentValueMemory := TReferenceIdentValueMemoryManager.Instance.Find(aID);
    TRunTraceManager.Instance.ReferenceIdentValueMemoryChanged(xReferenceIdentValueMemory);
end;


end.
