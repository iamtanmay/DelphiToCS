{ -----------------------------------------------------------------------------------------------------------------------
  Copyright © 2009 Zinsser Analytic GmbH - All rights reserved.
  Author       : Payman Kamali (pk)
  Description  :
  -----------------------------------------------------------------------------------------------------------------------
  Revision History:
  date     op  method                                track-no  improvement/change
  -------- --  ------------------------------------  --------- ----------------------------------------------------------
  18.06.09 pk                                        TN4585.3  Initial Revision
  23.06.09 pk  TSimpleStringSocketStream             TN4620    StringType set to Ansi
  06.07.09 pk  TSimpleStringSocketStream.Create      TN4585.4  Pass buffer size to inherited create
  13.07.09 pk                                        TN4585.4  TDblStreamableItem changed to TFloatStreamableItem
  30.07.09 pk  StrToFunctionParams                   TN4585.5  AddParam instead of Add
  09.09.09 pk  TSimpleStringSocketStream             TN4777    new Defaultsize parameter for SocketStream.ByteBuffer
  23.09.09 pk                                        TN4793    TWinSocketStreamExt.BufferToStr/BufferFromStr, and fStringType moved to Bytebuffer
  04.02.10 pk                                        TN4972    Changes for Restart
  19.10.10 pk                                        TN5305    changes needed for CoreClient/Server
  18.12.12 ts  TSimpleStringSocketStream.Create      TN6059    StringType Ansi doesn´t work / nur von Proxy genutzt
  21.12.12 ts                                        TN6059.1  StringType Ansi wieder aktiviert
  ----------------------------------------------------------------------------------------------------------------------- }

unit SimpleStringClientServer;


interface


uses
    Classes,
    ScktComp,
    RemoteFunctionClasses,
    RemoteClasses,
    RemoteFunctionClientServer,
    RemoteClient,
    Streamable,
    TextSocketStream;

type

    TSimpleStringSocketStream = class(TTextSocketStream)
    public
        constructor Create(aSocket: TCustomWinSocket; aTimeOut: integer; aWaitForDataTimeout: cardinal);
    end;

    TSimpleStringRemoteSocketStreamType = TSimpleStringSocketStream;

    TSimpleStringRemoteFunctionStreamer = class(TRemoteFunctionStreamer)
    private
        function TryStrToFloatVal(const aStr: TRemoteStringType; out oValue: double): boolean;
        function BoolValToStr(const aValue: boolean): TRemoteStringType;
        function SplitStr(const aStr, aDelim: string; out oStr1, oStr2: string): boolean;
        function FunctionCallToStr(const aFunctionCall: TRemoteFunctionCall): TRemoteStringType;
        function FunctionParamsToStr(const aFunctionParams: TRemoteFunctionParams): TRemoteStringType;
        function StrToBody(const aStr: TRemoteStringType): TRemoteMessageBody;
        function StrToFunctionCall(const aStr: TRemoteStringType): TRemoteFunctionCall;
        procedure StrToFunctionParams(const aStr: TRemoteStringType;
            const aFunctionParams: TRemoteFunctionParams);
        function StrToFunctionResult(const aStr: TRemoteStringType): TRemoteFunctionResult;
        function ParamToStr(const aFunctionParam: TStreamableItem): TRemoteStringType;
        function FunctionResultToStr(const aFunctionResult: TRemoteFunctionResult): TRemoteStringType;
        function StrToParam(const aStr: TRemoteStringType; const aIsText: boolean): TStreamableItem;
    public
        constructor Create();
        function EnvelopeToStr(const aEnvelope: TEnvelope): TRemoteStringType; override;
        function StrToEnvelope(const aStr: TRemoteStringType): TEnvelope; override;
    end;

    TTCPIPSimpleStringRemoteFunctionCaller = class(TRemoteFunctionCaller)
    public
        constructor Create(const aSocketStream: TWinSocketStreamExt);
    end;

    TTCPIPSimpleStringRemoteFunctionReceiver = class(TRemoteFunctionReceiver)
    public
        constructor Create(const aSocketStream: TWinSocketStreamExt);
    end;

    TSimpleStringMsgClient = class(TRemoteFunctionMsgClient)
    protected
        function CreateSocketStream(): TWinSocketStreamExt; override;
        function CreateRemoteFunctionCaller(): TRemoteFunctionCaller; override;
    end;

    TSimpleStringRemoteFunctionMsgHandler = class(TRemoteFunctionMsgHandler)
    protected
        function CreateRemoteFunctionReceiver: TRemoteFunctionReceiver; override;
    end;


implementation


uses
    SysUtils,
    RemoteMessageConstants;

const
    cFunctionParamsBeginDelim = '(';
    cFunctionParamsEndDelim = ')';
    cParamsSeparatorDelim = ',';
    cQuoteChar = '"';
    cBoolTrue = 'true';
    cBoolFalse = 'false';
    cDecimalDelim = '.';

    cEndStr: TTextSocketStreamStringType = #3;

    cByteBufferSize = 32768;

    { TSimpleStringRemoteFunctionStreamer }

constructor TSimpleStringRemoteFunctionStreamer.Create;
begin
    inherited Create();
end;

function TSimpleStringRemoteFunctionStreamer.SplitStr(const aStr, aDelim: string;
    out oStr1, oStr2: string): boolean;
var
    xLen, xPos: integer;
begin
    result := false;
    xLen := Length(aStr);

    oStr1 := aStr;
    oStr2 := '';
    xPos := Pos(aDelim, aStr);
    if xPos <= 0 then
        Exit;

    oStr1 := Copy(aStr, 1, xPos - 1);
    oStr2 := Copy(aStr, xPos + 1, xLen);

    result := true;

end;

function TSimpleStringRemoteFunctionStreamer.BoolValToStr(const aValue: boolean): TRemoteStringType;
begin
    if aValue then
        result := cBoolTrue
    else
        result := cBoolFalse;
end;

function TSimpleStringRemoteFunctionStreamer.ParamToStr(const aFunctionParam: TStreamableItem)
    : TRemoteStringType;
begin
    result := '';
    if not Assigned(aFunctionParam) then
        EXIT;
    if (aFunctionParam is TStrStreamableItem) then
        result := Format('%s%s%s', [cQuoteChar, aFunctionParam.AsStr, cQuoteChar])
    else if aFunctionParam is TFloatStreamableItem then
        result := Format('%d%s%d', [Trunc(aFunctionParam.AsFloat), cDecimalDelim,
            Frac(aFunctionParam.AsFloat)])
    else if aFunctionParam is TIntStreamableItem then
        result := IntToStr(aFunctionParam.AsInt)
    else if aFunctionParam is TBoolStreamableItem then
        result := BoolValToStr(aFunctionParam.AsBool)

end;

function TSimpleStringRemoteFunctionStreamer.FunctionParamsToStr(const aFunctionParams: TRemoteFunctionParams)
    : TRemoteStringType;
var
    x: integer;
begin
    result := '';
    for x := 0 to aFunctionParams.Count - 1 do
    begin
        if result <> '' then
            result := result + cParamsSeparatorDelim;
        result := result + ParamToStr(aFunctionParams[x]);
    end;

end;

function TSimpleStringRemoteFunctionStreamer.FunctionCallToStr(const aFunctionCall: TRemoteFunctionCall)
    : TRemoteStringType;
var
    xParams: string;
begin
    xParams := FunctionParamsToStr(aFunctionCall.Parameters);
    result := aFunctionCall.ObjectMethod + cFunctionParamsBeginDelim + xParams + cFunctionParamsEndDelim;
end;

function TSimpleStringRemoteFunctionStreamer.FunctionResultToStr(const aFunctionResult: TRemoteFunctionResult)
    : TRemoteStringType;
var
    xParams: string;
begin
    xParams := FunctionParamsToStr(aFunctionResult.Parameters);
    result := xParams;
end;

function TSimpleStringRemoteFunctionStreamer.EnvelopeToStr(const aEnvelope: TEnvelope): TRemoteStringType;
begin
    if aEnvelope.Body is TRemoteFunctionCall then
    begin
        result := FunctionCallToStr(aEnvelope.Body as TRemoteFunctionCall);
    end
    else if aEnvelope.Body is TRemoteFunctionResult then
    begin
        result := FunctionResultToStr(aEnvelope.Body as TRemoteFunctionResult);
    end;

    result := result + cEndStr;
end;

function TSimpleStringRemoteFunctionStreamer.TryStrToFloatVal(const aStr: TRemoteStringType;
    out oValue: double): boolean;
var
    xFormatSettings: TFormatSettings;
begin
    xFormatSettings.DecimalSeparator := cDecimalDelim;
    result := TryStrToFloat(aStr, oValue, xFormatSettings);
end;

function TSimpleStringRemoteFunctionStreamer.StrToParam(const aStr: TRemoteStringType; const aIsText: boolean)
    : TStreamableItem;
var
    xValDbl: double;
    xValInt: integer;
begin
    result := nil;

    if aIsText then
    begin
        result := TStrStreamableItem.Create(aStr);
    end
    else
    begin
        if SameText(aStr, cBoolFalse) then
        begin
            result := TBoolStreamableItem.Create(false);
        end
        else if SameText(aStr, cBoolTrue) then
        begin
            result := TBoolStreamableItem.Create(true);
        end
        else if TryStrToInt(aStr, xValInt) then
        begin
            result := TIntStreamableItem.Create(xValInt);
        end
        else if TryStrToFloatVal(aStr, xValDbl) then
        begin
            result := TFloatStreamableItem.Create(xValDbl);
        end
    end;

    if result = nil then
        raise Exception.CreateFmt('StrToParam: Value %s could not be converted', [aStr]);
end;

procedure TSimpleStringRemoteFunctionStreamer.StrToFunctionParams(const aStr: TRemoteStringType;
    const aFunctionParams: TRemoteFunctionParams);
var
    xPos: integer;
    xAccum: string;
    xQuoteOpen: boolean;
    xLen: integer;
    xParam: TStreamableItem;
    xTextIdentFound: boolean;

    procedure IdentComplete();
    begin
        if not xTextIdentFound then
            xAccum := Trim(xAccum);

        xParam := StrToParam(xAccum, xTextIdentFound);
        aFunctionParams.AddParam(xParam);
        xAccum := '';
        xTextIdentFound := false;
    end;

begin
    xLen := Length(aStr);

    if xLen = 0 then
        EXIT;

    xAccum := '';
    xQuoteOpen := false;
    xTextIdentFound := false;
    xPos := 1;

    while true do
    begin
        if xPos > xLen then
        begin
            ASSERT(not xQuoteOpen, 'Open quote without close quote');
            IdentComplete();
            EXIT;
        end;

        if aStr[xPos] = cParamsSeparatorDelim then
        begin
            ASSERT(not xQuoteOpen, 'Open quote without close quote');
            IdentComplete();
        end
        else if aStr[xPos] = cQuoteChar then
        begin
            if xQuoteOpen then
            begin
                xTextIdentFound := true;
            end;
            xQuoteOpen := not xQuoteOpen;
        end
        else
        begin
            if xTextIdentFound then
            begin
                ASSERT(aStr[xPos] = ' ', 'Invalid characters found after close quote');
            end
            else
            begin
                xAccum := xAccum + aStr[xPos];
            end;
        end;

        xPos := xPos + 1;
    end;
end;

function TSimpleStringRemoteFunctionStreamer.StrToFunctionResult(const aStr: TRemoteStringType)
    : TRemoteFunctionResult;
begin
    result := TRemoteFunctionResult.Create();
    StrToFunctionParams(aStr, result.Parameters);
end;

function TSimpleStringRemoteFunctionStreamer.StrToFunctionCall(const aStr: TRemoteStringType)
    : TRemoteFunctionCall;
var
    xTempStr, xParamsStr: string;
    xFunctionName: string;
    xDummy: string;
begin
    result := nil;
    if not SplitStr(aStr, cFunctionParamsBeginDelim, xFunctionName, xTempStr) then
        EXIT;
    xFunctionName := Trim(xFunctionName);
    if xFunctionName = '' then
        EXIT;

    if not SplitStr(xTempStr, cFunctionParamsEndDelim, xParamsStr, xDummy) then
        EXIT;

    result := TRemoteFunctionCall.Create();
    result.ObjectMethod := xFunctionName;
    StrToFunctionParams(xParamsStr, result.Parameters);
end;

function TSimpleStringRemoteFunctionStreamer.StrToBody(const aStr: TRemoteStringType): TRemoteMessageBody;

begin
    result := StrToFunctionCall(aStr);
    if Assigned(result) then
        EXIT;

    result := StrToFunctionResult(aStr);
end;

function TSimpleStringRemoteFunctionStreamer.StrToEnvelope(const aStr: TRemoteStringType): TEnvelope;
var
    xStr, xDummy: string;
begin
    if not SplitStr(aStr, cEndStr, xStr, xDummy) then
    begin
        raise Exception.CreateFmt('End delimiter %s not found in %s', [cEndStr, aStr]);
    end;

    result := TEnvelope.Create();
    result.Body := StrToBody(xStr);
end;

{ TSimpleStringSocketStream }
constructor TSimpleStringSocketStream.Create(aSocket: TCustomWinSocket; aTimeOut: integer;
    aWaitForDataTimeout: cardinal);
begin
    inherited Create(aSocket, aTimeout, aWaitForDataTimeout, cEndStr, cByteBufferSize, cByteBufferSize);
    self.Buffer.StringType := sstAnsi;
end;

{ TTCPIPSimpleStringRemoteFunctionCaller }

constructor TTCPIPSimpleStringRemoteFunctionCaller.Create(const aSocketStream: TWinSocketStreamExt);
begin
    inherited Create(TTCPIPRemoteFunctionTransporter.Create(aSocketStream,
        TSimpleStringRemoteFunctionStreamer.Create()));
end;

{ TTCPIPSimpleStringRemoteFunctionReceiver }

constructor TTCPIPSimpleStringRemoteFunctionReceiver.Create(const aSocketStream: TWinSocketStreamExt);
begin
    inherited Create(TTCPIPRemoteFunctionTransporter.Create(aSocketStream,
        TSimpleStringRemoteFunctionStreamer.Create()));
end;

function TSimpleStringRemoteFunctionMsgHandler.CreateRemoteFunctionReceiver: TRemoteFunctionReceiver;
begin
    result := TTCPIPSimpleStringRemoteFunctionReceiver.Create(fInterpreter.Stream);
end;

{ TSimpleStringMsgClient }

function TSimpleStringMsgClient.CreateSocketStream: TWinSocketStreamExt;
begin
    result := TSimpleStringSocketStream.Create(self.Socket, 5000, INT_CLIENT_WAITFORDATA_TIMEOUT_MSECS);
end;

function TSimpleStringMsgClient.CreateRemoteFunctionCaller(): TRemoteFunctionCaller;
begin
    result := TTCPIPSimpleStringRemoteFunctionCaller.Create(self.Stream);
end;


end.
