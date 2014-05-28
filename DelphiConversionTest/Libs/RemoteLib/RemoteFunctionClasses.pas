unit RemoteFunctionClasses;


{ -----------------------------------------------------------------------------------------------------------------------
  Copyright © 2009 Zinsser Analytic GmbH - All rights reserved.
  Author       : Payman Kamali (pk)
  Description  :
  -----------------------------------------------------------------------------------------------------------------------
  Revision History:
  date     op  method                                track-no  improvement/change
  -------- --  ------------------------------------  --------- ----------------------------------------------------------
  09.06.09 pk                                        TN4585.2    Initial Revision
  18.06.09 pk                                        TN4585.3    Various
  23.06.09 pk                                        TN4620      MethodInfo directive removed
  23.06.09 pk  AddParams                             TN4620      unicodestring also implemented
  06.07.09 pk                                        TN4585.4    Variant, DateTime, Object params implemented
  13.07.09 pk  TRemoteFunctionError                  TN4585.4    New
  13.07.09 pk  TRemoteFunctionCall                   TN4585.4    ObjectID
  30.07.09 pk                                        TN4585.5    Various Changes
  12.10.09 pk  TRemoteCall, TRemoteResult            TN4812      New
  12.10.09 pk  TRemoteFunctionCaller                 TN4812      New CallFunc using generics
  04.02.10 pk                                        TN4972      Changes for Restart
  19.10.10 pk                                        TN5305      changes needed for CoreClient/Server
  19.02.12 pk                                        TN5809       Bug fixes to CoreServer classes for V8
  ----------------------------------------------------------------------------------------------------------------------- }
interface


uses
    GeneralTypes,
    Streamable;

const
    cRemoteFunctionCallObjectIDNone = 0;

type
    TRemoteStringType = string;

    TRemoteMessageBody = class(TCustomStreamable)
    end;

    TEnvelope = class(TStreamable)
    private
        fBody: TRemoteMessageBody;
    public
        constructor Create(); override;
    published
        property Body: TRemoteMessageBody read fBody write fBody;
    end;

    TRemoteFunctionParams = class(TStreamableObjectList)
    private
        function AddIntern(const aValue: TStreamableItem): TStreamableItem;
        function GetItemAt(aIndex: integer): TStreamableItem;
    public
        procedure Add(const aValue: string); overload;
        procedure Add(const aValue: integer); overload;
        procedure Add(const aValue: cardinal); overload;
        procedure Add(const aValue: boolean); overload;
        procedure Add(const aValue: single); overload;
        procedure Add(const aValue: double); overload;
        procedure Add(const aValue: extended); overload;
        procedure Add(const aValue: TIntArray); overload;
        procedure Add(const aValue: TStringArray); overload;
        procedure Add(const aValue: TObject); overload;
        procedure Add(const aValue: TDateTime); overload;
        procedure Add(const aValue: variant); overload;
        function AddParam(const aValue: TStreamableItem): TStreamableItem;
        procedure AddParams(const aValues: array of const );
        property Items[aIndex: integer]: TStreamableItem read GetItemAt; default;
    end;

    TRemoteCall = class(TStreamableItem)
    protected
        function GetFunctionName(): string;
    published
        property FunctionName: string read GetFunctionName;
    end;

    TRemoteResult = class(TStreamableItem)

    end;

    TRemoteFunctionError = class(TCustomStreamable)
    private
        fErrorText: string;
    public
        constructor Create(); override;
    published
        property ErrorText: string read fErrorText write fErrorText;
    end;

    TRemoteFunctionResult = class(TRemoteMessageBody)
    private
        fParams: TRemoteFunctionParams;
        fError: TRemoteFunctionError;
        function GetFirstParam: TStreamableItem;
        function GetAsBool: boolean;
        function GetAsCard: cardinal;
        function GetAsFloat: extended;
        function GetAsInt: integer;
        function GetAsStr: string;
        procedure SetAsBool(const aValue: boolean);
        procedure SetAsCard(const aValue: cardinal);
        procedure SetAsFloat(const aValue: extended);
        procedure SetAsInt(const aValue: integer);
        procedure SetAsStr(const aValue: string);
    public
        constructor Create(); override;
        property FirstParam: TStreamableItem read GetFirstParam;
        property AsInt: integer read GetAsInt write SetAsInt;
        property AsCard: cardinal read GetAsCard write SetAsCard;
        property AsStr: string read GetAsStr write SetAsStr;
        property AsBool: boolean read GetAsBool write SetAsBool;
        property AsFloat: extended read GetAsFloat write SetAsFloat;
    published
        property Parameters: TRemoteFunctionParams read fParams write fParams;
        property Error: TRemoteFunctionError read fError write fError;
    end;

    TRemoteFunctionCall = class(TRemoteMessageBody)
    private
        fObjectID: integer;
        fObjectMethod: string;
        fParams: TRemoteFunctionParams;
        function GetHasObjectID: boolean;
    public
        constructor Create(); override;
        property HasObjectID: boolean read GetHasObjectID;
    published
        property ObjectID: integer read fObjectID write fObjectID;
        property ObjectMethod: string read fObjectMethod write fObjectMethod;
        property Parameters: TRemoteFunctionParams read fParams write fParams;

    end;

    TOnRemoteWriteLogCallback = procedure(const aText: string) of object;

    TRemoteFunctionStreamer = class
    public
        function EnvelopeToStr(const aFunctionResult: TEnvelope): TRemoteStringType; virtual; abstract;
        function StrToEnvelope(const aStr: TRemoteStringType): TEnvelope; virtual; abstract;
    end;

    TRemoteFunctionTransporter = class
    private
        fOnWriteLog: TOnRemoteWriteLogCallback;
    protected
        procedure Log(const aText: string);
    public
        procedure SendCall(const aEnvelope: TEnvelope); virtual; abstract;
        function ReceiveCall(): TEnvelope; virtual; abstract;
        procedure SendResult(const aEnvelope: TEnvelope); virtual; abstract;
        function ReceiveResult(): TEnvelope; virtual; abstract;
        function Wait(): boolean; virtual; abstract;
        property OnWriteLog: TOnRemoteWriteLogCallback read fOnWriteLog write fOnWriteLog;
    end;

    TRemoteFunctionCaller = class
    private
        fOnWriteLog: TOnRemoteWriteLogCallback;
        fRemoteFunctionStreamer: TRemoteFunctionStreamer;
        fRemoteFunctionCall: TRemoteFunctionCall;
        fRemoteFunctionResult: TRemoteFunctionResult;
        fTransporter: TRemoteFunctionTransporter;
        fCallEnvelope: TEnvelope;
        fResultEnvelope: TEnvelope;
        procedure CheckResponse();
        procedure DecodeResult();
        procedure AddParams(const aParamValues: array of const );
        procedure AssignOutParams(const aOutParamPointers: array of pointer;
            const aOutParamValues: array of const );
        procedure SetOnWriteLog(const aValue: TOnRemoteWriteLogCallback);
    protected
        function GetParams: TRemoteFunctionParams; virtual;
        function GetFuncResult: TRemoteFunctionResult; virtual;

    public

        constructor Create(const aTransporter: TRemoteFunctionTransporter);
        destructor Destroy(); override;
        procedure Log(const aText: string);
        procedure PrepareCall(const aObjectID: integer; const aCallName: string); overload; virtual;
        procedure PrepareCall(const aCallName: string); overload; virtual;
        procedure ExecuteCall(); virtual;

        procedure Call(const aObjectID: integer; const aCallName: string; const aParamValues: array of const;
            const aOutParamPointers: array of pointer; const aOutParamValues: array of const );
            overload; virtual;
        procedure Call(const aCallName: string; const aParamValues: array of const;
            const aOutParamPointers: array of pointer; const aOutParamValues: array of const );
            overload; virtual;

        function CallIntFunc(const aCallName: string; const aParamValues: array of const ): integer;
            overload; virtual;
        function CallIntFunc(const aCallName: string): integer; overload; virtual;
        function CallBoolFunc(const aCallName: string; const aParamValues: array of const ): boolean;
            overload; virtual;
        function CallBoolFunc(const aCallName: string): boolean; overload; virtual;

        procedure CallProc(const aCallName: string); overload; virtual;
        procedure CallProc(const aCallName: string; const aParamValues: array of const ); overload; virtual;

        function CallIntFunc(const aObjectID: integer; const aCallName: string;
            const aParamValues: array of const ): integer; overload; virtual;
        function CallIntFunc(const aObjectID: integer; const aCallName: string): integer; overload; virtual;
        function CallBoolFunc(const aObjectID: integer; const aCallName: string;
            const aParamValues: array of const ): boolean; overload; virtual;
        function CallBoolFunc(const aObjectID: integer; const aCallName: string): boolean; overload; virtual;

        procedure CallProc(const aObjectID: integer; const aCallName: string); overload; virtual;
        procedure CallProc(const aObjectID: integer; const aCallName: string;
            const aParamValues: array of const ); overload; virtual;

        function CallFunc<T: TRemoteResult>(const aObjectID: integer; const aCall: TRemoteCall): T; overload;
        function CallFunc<T: TRemoteResult>(const aCall: TRemoteCall): T; overload;

        property Params: TRemoteFunctionParams read GetParams;
        property FuncResult: TRemoteFunctionResult read GetFuncResult;
        property OnWriteLog: TOnRemoteWriteLogCallback read fOnWriteLog write SetOnWriteLog;
    end;

    TRemoteFunctionHandleCallCallback = procedure(const aSender: TObject) of object;

    TRemoteFunctionReceiver = class
    private
        fOnWriteLog: TOnRemoteWriteLogCallback;
        fRemoteFunctionCall: TRemoteFunctionCall;
        fTransporter: TRemoteFunctionTransporter;
        fRemoteFunctionResult: TRemoteFunctionResult;
        fCallEnvelope: TEnvelope;
        fResultEnvelope: TEnvelope;
        fHandleCallCallback: TRemoteFunctionHandleCallCallback;
        function GetCallName: string;
        function GetCallHasObjectID: boolean;
        function GetCallObjectID: integer;
        procedure DoHandleCall;
        procedure ResetError;
        procedure SetError(const aErrorText: string);
        procedure SetOnWriteLog(const aValue: TOnRemoteWriteLogCallback);
        function GetAsRemoteCall(): TRemoteCall;
    protected
        procedure DecodeCall(); virtual;
        procedure FinishCall(); virtual;
        function GetCallParams: TRemoteFunctionParams; virtual;
        function GetRemoteFunctionResultParams: TRemoteFunctionParams; virtual;
    public
        constructor Create(const aTransporter: TRemoteFunctionTransporter);
        procedure Log(const aText: string);
        procedure HandleCall(); virtual;
        procedure AssignRemoteResult(const aRemoteResult: TRemoteResult);
        function CallNameMatches(const aCallName: string): boolean; virtual;
        property CallName: string read GetCallName;
        property CallHasObjectID: boolean read GetCallHasObjectID;
        property CallObjectID: integer read GetCallObjectID;
        property CallParams: TRemoteFunctionParams read GetCallParams;
        property AsRemoteCall: TRemoteCall read GetAsRemoteCall;
        property ResultParams: TRemoteFunctionParams read GetRemoteFunctionResultParams;
        property OnWriteLog: TOnRemoteWriteLogCallback read fOnWriteLog write SetOnWriteLog;
        property HandleCallCallback: TRemoteFunctionHandleCallCallback read fHandleCallCallback
            write fHandleCallCallback;
    end;


implementation


uses
    SysUtils,
    Windows,
    Classes,
    TypeRegistry;

{ TEnvelope }
constructor TEnvelope.Create();
begin
    inherited Create();
end;

{ TRemoteFunctionCall }
constructor TRemoteFunctionCall.Create();
begin
    inherited Create();
    fParams := TRemoteFunctionParams.Create();
end;

function TRemoteFunctionCall.GetHasObjectID: boolean;
begin
    result := fObjectID <> cRemoteFunctionCallObjectIDNone;
end;

{ TRemoteFunctionParams }

procedure TRemoteFunctionParams.Add(const aValue: integer);
begin
    self.AddIntern(TIntStreamableItem.Create()).AsInt := aValue;
end;

procedure TRemoteFunctionParams.Add(const aValue: string);
begin
    self.AddIntern(TStrStreamableItem.Create()).AsStr := aValue;
end;

procedure TRemoteFunctionParams.Add(const aValue: single);
begin
    self.AddIntern(TFloatStreamableItem.Create()).AsFloat := aValue;
end;

procedure TRemoteFunctionParams.Add(const aValue: double);
begin
    self.AddIntern(TFloatStreamableItem.Create()).AsFloat := aValue;
end;

procedure TRemoteFunctionParams.Add(const aValue: extended);
begin
    self.AddIntern(TFloatStreamableItem.Create()).AsFloat := aValue;
end;

procedure TRemoteFunctionParams.Add(const aValue: boolean);
begin
    self.AddIntern(TBoolStreamableItem.Create()).AsBool := aValue;
end;

procedure TRemoteFunctionParams.Add(const aValue: TObject);
begin
    self.AddIntern(TObjStreamableItem.Create()).AsObj := aValue;
end;

procedure TRemoteFunctionParams.Add(const aValue: TDateTime);
begin
    self.AddIntern(TDateTimeStreamableItem.Create()).AsDateTime := aValue;
end;

procedure TRemoteFunctionParams.Add(const aValue: variant);
begin
    self.AddIntern(TVariantStreamableItem.Create()).AsVar := aValue;
end;

procedure TRemoteFunctionParams.Add(const aValue: cardinal);
begin
    self.AddIntern(TCardStreamableItem.Create()).AsCard := aValue;
end;

procedure TRemoteFunctionParams.Add(const aValue: TIntArray);
begin
    self.AddIntern(TStreamableIntList.Create()).AsIntArray := aValue;
end;

procedure TRemoteFunctionParams.Add(const aValue: TStringArray);
begin
    self.AddIntern(TStreamableStringList.Create()).AsStrArray := aValue;
end;

function TRemoteFunctionParams.AddIntern(const aValue: TStreamableItem): TStreamableItem;
begin
    inherited Add(aValue);
    result := aValue;
end;

function TRemoteFunctionParams.AddParam(const aValue: TStreamableItem): TStreamableItem;
begin
    result := AddIntern(aValue);
end;

function TRemoteFunctionParams.GetItemAt(aIndex: integer): TStreamableItem;
begin
    result := inherited Items[aIndex] as TStreamableItem;
end;

procedure TRemoteFunctionParams.AddParams(const aValues: array of const );
var
    x: integer;
begin
    for x := 0 to high(aValues) do
        with aValues[x] do
            case VType of
                vtObject:
                    Add(VObject);
                vtInteger:
                    Add(VInteger);
                vtBoolean:
                    Add(VBoolean);
                vtExtended:
                    Add(VExtended^);
                vtString:
                    Add(string(VString^));
                vtAnsiString:
                    Add(string(VAnsiString));
                vtUnicodeString:
                    Add(string(VUnicodeString));
                vtVariant:
                    Add(VVariant^);
                else
                    ASSERT(false, 'TRemoteFunctionCaller.AddParams: Type Not Implemented');
            end;
end;

{ TRemoteFunctionResult }

constructor TRemoteFunctionResult.Create;
begin
    inherited Create();
    fParams := TRemoteFunctionParams.Create();
    fError := TRemoteFunctionError.Create();
end;

function TRemoteFunctionResult.GetAsBool: boolean;
begin
    result := self.FirstParam.AsBool;
end;

function TRemoteFunctionResult.GetAsCard: cardinal;
begin
    result := self.FirstParam.AsCard;
end;

function TRemoteFunctionResult.GetAsFloat: extended;
begin
    result := self.FirstParam.AsFloat;
end;

function TRemoteFunctionResult.GetAsInt: integer;
begin
    result := self.FirstParam.AsInt;
end;

function TRemoteFunctionResult.GetAsStr: string;
begin
    result := self.FirstParam.AsStr;
end;

function TRemoteFunctionResult.GetFirstParam: TStreamableItem;
begin
    result := fParams[0];
end;

procedure TRemoteFunctionResult.SetAsBool(const aValue: boolean);
begin
    self.Parameters.Add(aValue);
end;

procedure TRemoteFunctionResult.SetAsCard(const aValue: cardinal);
begin
    self.Parameters.Add(aValue);
end;

procedure TRemoteFunctionResult.SetAsFloat(const aValue: extended);
begin
    self.Parameters.Add(aValue);
end;

procedure TRemoteFunctionResult.SetAsInt(const aValue: integer);
begin
    self.Parameters.Add(aValue);
end;

procedure TRemoteFunctionResult.SetAsStr(const aValue: string);
begin
    self.Parameters.Add(aValue);
end;

{ TRemoteFunctionTransporter }
procedure TRemoteFunctionTransporter.Log(const aText: string);
begin
    if not Assigned(self.OnWriteLog) then
        EXIT;
    // self.OnWriteLog( IntToStr( GetTickCount() ) );
    self.OnWriteLog(aText);
end;

{ TRemoteFunctionCaller }
constructor TRemoteFunctionCaller.Create(const aTransporter: TRemoteFunctionTransporter);
begin
    inherited Create();
    fTransporter := aTransporter;
    fCallEnvelope := TEnvelope.Create();
    fRemoteFunctionCall := TRemoteFunctionCall.Create();
    fCallEnvelope.Body := fRemoteFunctionCall;
    fResultEnvelope := nil;
end;

destructor TRemoteFunctionCaller.Destroy();
begin
    fRemoteFunctionStreamer.Free;
    fRemoteFunctionCall.Free;
    fCallEnvelope.Free;
    inherited;
end;

procedure TRemoteFunctionCaller.PrepareCall(const aObjectID: integer; const aCallName: string);
begin
    fRemoteFunctionCall.Parameters.Clear();
    fRemoteFunctionCall.ObjectID := aObjectID;
    fRemoteFunctionCall.ObjectMethod := aCallName;
end;

procedure TRemoteFunctionCaller.PrepareCall(const aCallName: string);
begin
    PrepareCall(cRemoteFunctionCallObjectIDNone, aCallName);
end;

procedure TRemoteFunctionCaller.CheckResponse();
begin
    if not fTransporter.Wait() then
        raise Exception.Create('No answer received from Server');
end;

procedure TRemoteFunctionCaller.DecodeResult;
begin
    FreeAndNil(fResultEnvelope);
    fResultEnvelope := fTransporter.ReceiveResult();
    if not Assigned(fResultEnvelope) then
        raise Exception.Create('DecodeResult: No result');

    if not Assigned(fResultEnvelope.Body) then
    begin
        raise Exception.Create('DecodeResult: No result body');
    end;

    if not(fResultEnvelope.Body is TRemoteFunctionResult) then
    begin
        raise Exception.CreateFmt('DecodeResult: Unexpected result body class [%s]',
            [fResultEnvelope.Body.ClassName]);
    end;
    fRemoteFunctionResult := (fResultEnvelope.Body as TRemoteFunctionResult);
    if fRemoteFunctionResult.Error.ErrorText <> '' then
        raise Exception.Create(fRemoteFunctionResult.Error.ErrorText);

end;

procedure TRemoteFunctionCaller.ExecuteCall();
begin
    try
        fTransporter.SendCall(fCallEnvelope);

        self.CheckResponse();
        DecodeResult();
    except
        on e: exception do
        begin
            raise Exception.CreateFmt('%s - %s', [fRemoteFunctionCall.ObjectMethod, e.Message]);
        end;
    end;
end;

procedure TRemoteFunctionCaller.SetOnWriteLog(const aValue: TOnRemoteWriteLogCallback);
begin
    fOnWriteLog := aValue;
    if Assigned(fTransporter) then
        fTransporter.OnWriteLog := fOnWriteLog;
end;

function TRemoteFunctionCaller.GetParams: TRemoteFunctionParams;
begin
    result := fRemoteFunctionCall.Parameters;
end;

procedure TRemoteFunctionCaller.AddParams(const aParamValues: array of const );
begin
    self.Params.AddParams(aParamValues);
end;

procedure TRemoteFunctionCaller.AssignOutParams(const aOutParamPointers: array of pointer;
    const aOutParamValues: array of const );
var
    x: integer;
var
    xFunctionResultParam: TStreamableItem;
begin
    ASSERT(Length(aOutParamValues) = Length(aOutParamPointers));
    ASSERT((fRemoteFunctionResult.Parameters.Count) = Length(aOutParamPointers));

    for x := 0 to high(aOutParamPointers) do
    begin
        xFunctionResultParam := fRemoteFunctionResult.Parameters[x];
        with aOutParamValues[x] do
            case VType of
                vtInteger:
                    PInteger(aOutParamPointers[x])^ := xFunctionResultParam.AsInt;
                vtBoolean:
                    PBoolean(aOutParamPointers[x])^ := xFunctionResultParam.AsBool;
                vtExtended:
                    PDouble(aOutParamPointers[x])^ := xFunctionResultParam.AsFloat;
                vtString, vtAnsiString, vtUnicodeString:
                    PString(aOutParamPointers[x])^ := xFunctionResultParam.AsStr;
                vtObject:
                    Pointer(aOutParamPointers[x]^) := xFunctionResultParam.AsObj;
                vtVariant:
                    PVariant(aOutParamPointers[x])^ := xFunctionResultParam.AsVar;
                else
                    ASSERT(false, 'TRemoteFunctionCaller.AssignOutParams: Type Not Implemented');
            end;
    end;

end;

procedure TRemoteFunctionCaller.Call(const aObjectID: integer; const aCallName: string;
    const aParamValues: array of const; const aOutParamPointers: array of pointer;
    const aOutParamValues: array of const );
begin
    PrepareCall(aObjectID, aCallName);
    AddParams(aParamValues);
    ExecuteCall;
    AssignOutParams(aOutParamPointers, aOutParamValues);
end;

procedure TRemoteFunctionCaller.Call(const aCallName: string; const aParamValues: array of const;
    const aOutParamPointers: array of pointer; const aOutParamValues: array of const );
begin
    Call(cRemoteFunctionCallObjectIDNone, aCallName, aParamValues, aOutParamPointers, aOutParamValues);
end;

function TRemoteFunctionCaller.CallIntFunc(const aCallName: string;
    const aParamValues: array of const ): integer;
begin
    Call(aCallName, aParamValues, [@result], [result]);
end;

function TRemoteFunctionCaller.CallIntFunc(const aCallName: string): integer;
begin
    Call(aCallName, [], [@result], [result]);
end;

function TRemoteFunctionCaller.CallBoolFunc(const aCallName: string;
    const aParamValues: array of const ): boolean;
begin
    Call(aCallName, aParamValues, [@result], [result]);
end;

function TRemoteFunctionCaller.CallBoolFunc(const aCallName: string): boolean;
begin
    Call(aCallName, [], [@result], [result]);
end;

procedure TRemoteFunctionCaller.CallProc(const aCallName: string; const aParamValues: array of const );
begin
    Call(aCallName, aParamValues, [], []);
end;

procedure TRemoteFunctionCaller.CallProc(const aCallName: string);
begin
    CallProc(aCallName, []);
end;

function TRemoteFunctionCaller.CallIntFunc(const aObjectID: integer; const aCallName: string;
    const aParamValues: array of const ): integer;
begin
    Call(aObjectID, aCallName, aParamValues, [@result], [result]);
end;

function TRemoteFunctionCaller.CallIntFunc(const aObjectID: integer; const aCallName: string): integer;
begin
    Call(aObjectID, aCallName, [], [@result], [result]);
end;

function TRemoteFunctionCaller.CallBoolFunc(const aObjectID: integer; const aCallName: string;
    const aParamValues: array of const ): boolean;
begin
    Call(aObjectID, aCallName, aParamValues, [@result], [result]);
end;

function TRemoteFunctionCaller.CallBoolFunc(const aObjectID: integer; const aCallName: string): boolean;
begin
    Call(aObjectID, aCallName, [], [@result], [result]);
end;

function TRemoteFunctionCaller.CallFunc<T>(const aObjectID: integer; const aCall: TRemoteCall): T;
begin
    PrepareCall(aObjectID, aCall.FunctionName);
    self.Params.AddIntern(aCall);
    ExecuteCall;
    // result := nil;
    if fRemoteFunctionResult.Parameters.Count <= 0 then
        EXIT;
    result := T(fRemoteFunctionResult.Parameters[0]);
end;

function TRemoteFunctionCaller.CallFunc<T>(const aCall: TRemoteCall): T;
begin
    result := CallFunc<T>(cRemoteFunctionCallObjectIDNone, aCall);
end;

procedure TRemoteFunctionCaller.CallProc(const aObjectID: integer; const aCallName: string;
    const aParamValues: array of const );
begin
    Call(aObjectID, aCallName, aParamValues, [], []);
end;

procedure TRemoteFunctionCaller.CallProc(const aObjectID: integer; const aCallName: string);
begin
    CallProc(aObjectID, aCallName, []);
end;

function TRemoteFunctionCaller.GetFuncResult: TRemoteFunctionResult;
begin
    result := fRemoteFunctionResult;
end;

procedure TRemoteFunctionCaller.Log(const aText: string);
begin
    if Assigned(self.OnWriteLog) then
        self.OnWriteLog(aText);
end;

{ TRemoteFunctionReceiver }

procedure TRemoteFunctionReceiver.AssignRemoteResult(const aRemoteResult: TRemoteResult);
begin
    fRemoteFunctionResult.Parameters.AddParam(aRemoteResult);
end;

function TRemoteFunctionReceiver.CallNameMatches(const aCallName: string): boolean;
begin
    result := SameText(aCallName, self.CallName);
end;

constructor TRemoteFunctionReceiver.Create(const aTransporter: TRemoteFunctionTransporter);
begin
    inherited Create();
    fTransporter := aTransporter;
    fCallEnvelope := nil;
    fRemoteFunctionCall := nil;
    fResultEnvelope := TEnvelope.Create();
    fRemoteFunctionResult := TRemoteFunctionResult.Create();
    fResultEnvelope.Body := fRemoteFunctionResult;
end;

procedure TRemoteFunctionReceiver.Log(const aText: string);
begin
    if Assigned(self.OnWriteLog) then
        self.OnWriteLog(aText);
end;

procedure TRemoteFunctionReceiver.SetOnWriteLog(const aValue: TOnRemoteWriteLogCallback);
begin
    fOnWriteLog := aValue;
    if Assigned(fTransporter) then
        fTransporter.OnWriteLog := fOnWriteLog;
end;

procedure TRemoteFunctionReceiver.SetError(const aErrorText: string);
begin
    fRemoteFunctionResult.Error.ErrorText := aErrorText;
end;

procedure TRemoteFunctionReceiver.ResetError();
begin
    SetError('');
end;

procedure TRemoteFunctionReceiver.DecodeCall;
begin
    ResetError();
    fRemoteFunctionResult.Parameters.Clear();

    FreeAndNil(fCallEnvelope);
    fCallEnvelope := fTransporter.ReceiveCall();
    ASSERT(Assigned(fCallEnvelope) and (fCallEnvelope.Body is TRemoteFunctionCall));
    fRemoteFunctionCall := fCallEnvelope.Body as TRemoteFunctionCall;
end;

function TRemoteFunctionReceiver.GetAsRemoteCall(): TRemoteCall;
begin
    result := nil;
    if (fRemoteFunctionCall.Parameters.Count = 0) or
        (not(fRemoteFunctionCall.Parameters[0] is TRemoteCall)) then
        EXIT;
    result := fRemoteFunctionCall.Parameters[0] as TRemoteCall;

end;

procedure TRemoteFunctionReceiver.FinishCall;
begin
    fTransporter.SendResult(fResultEnvelope);
end;

function TRemoteFunctionReceiver.GetCallHasObjectID: boolean;
begin
    result := fRemoteFunctionCall.HasObjectID;
end;

function TRemoteFunctionReceiver.GetCallName: string;
begin
    result := fRemoteFunctionCall.ObjectMethod;
end;

function TRemoteFunctionReceiver.GetCallObjectID: integer;
begin
    result := fRemoteFunctionCall.ObjectID;
end;

function TRemoteFunctionReceiver.GetCallParams: TRemoteFunctionParams;
begin
    result := fRemoteFunctionCall.Parameters;
end;

function TRemoteFunctionReceiver.GetRemoteFunctionResultParams: TRemoteFunctionParams;
begin
    result := fRemoteFunctionResult.Parameters;
end;

procedure TRemoteFunctionReceiver.DoHandleCall();
begin
    if Assigned(fHandleCallCallback) then
        fHandleCallCallback(self);
end;

procedure TRemoteFunctionReceiver.HandleCall;
begin
    DecodeCall();
    try
        try
            DoHandleCall();
        except
            on E: Exception do
            begin
                SetError(E.Message);
            end;
        end;
    finally
        FinishCall();
    end;
end;

{ TRemoteFunctionError }

constructor TRemoteFunctionError.Create;
begin
    inherited Create();
    fErrorText := '';
end;

{ TRemoteCall }

function TRemoteCall.GetFunctionName: string;
begin
    result := self.ClassName;
end;


initialization


TTypeRegistry.InstanceRegisterTypes([TRemoteFunctionParams, TRemoteFunctionCall, TRemoteFunctionResult,
    TRemoteFunctionError, TEnvelope]);


end.
