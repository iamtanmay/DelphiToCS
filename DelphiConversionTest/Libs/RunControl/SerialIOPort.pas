{ ----------------------------------------------------------------------------------------------------------------------
  Copyright © 2011 Zinsser Analytic GmbH - All rights reserved.
  Author       : Wolfgang Lyncke (wl)
  Description  :
  ----------------------------------------------------------------------------------------------------------------------
  Revision History:
  date     op  method                       track-no improvement/change
  -------- --  ---------------------------  -------- -------------------------------------------------------------------
  12.04.11 wl  TSerialIOPort                TN5491.1  von Altlasten bereinigtes TSerialPortExtend2
  12.04.11 wl                               TN5491.1  ISerialPort wieder entfernt
  15.12.11 wl                               TN5767   uses geändert
  10.04.13 wl                               TN5767   uses geändert
  ---------------------------------------------------------------------------------------------------------------------- }

unit SerialIOPort;


interface


uses
    SysUtils,
    Classes,
    Supercom;

type
    ESerialPortError = class(Exception);

    TSerialIOPort = class
    strict private
    const
        kACK = #6;
        kNAK = #21;
        kENQ = #5;
        kSYN = #22;
        kDC2 = #18;
        kDC4 = #20;
        kCR = #13;
        kLF = #10;
        kNUL = #0;
        kETX = #3;
        kSTX = #2;
        kEOT = #4;
        kRECEIVE_WAIT = 100;
    strict private
        fActive: Boolean;
        fBaud: integer;
        fBeginStr: string;
        fBuffSize: cardinal;
        fComPort: Integer;
        fDataBits: Byte;
        fEndChar: string;
        fEventMask: cardinal;
        fFlowContr: integer;
        fName: string;
        fNoOfChar: integer;
        fOutText: string;
        fParity: Char;
        fSleepTime: integer;
        fStopBits: Byte;
        fTimeout: cardinal;
        fEventChar: char;
        FComInt: TComInt;
        FStartRead: TSYSTIME;
        FStopRead: TSYSTIME;
        fErrTimeOut: boolean;
        fErrReachedEnd: boolean;
        fRaiseExceptionOnTimeout: boolean;
        FSleepTimeBeforeReceive: word;
        function GetActive: Boolean;
        function GetBaud: integer;
        function GetBeginStr: string;
        function GetBuffSize: cardinal;
        function GetComPort: Integer;
        function GetDataBits: Byte;
        function GetEndChar: string;
        function GetEventMask: cardinal;
        function GetFlowContr: integer;
        function GetName: string;
        function GetNoOfChar: integer;
        function GetOutText: string;
        function GetParity: Char;
        function GetSleepTime: integer;
        function GetStopBits: Byte;
        function GetTimeout: cardinal;
        procedure SetBaud(const Value: integer);
        procedure SetBeginStr(const Value: string);
        procedure SetBuffSize(const Value: cardinal);
        procedure SetComPort(const Value: Integer);
        procedure SetDataBits(const Value: Byte);
        procedure SetEndChar(const Value: string);
        procedure SetEventMask(const Value: cardinal);
        procedure SetFlowContr(const Value: integer);
        procedure SetName(const Value: string);
        procedure SetNoOfChar(const Value: integer);
        procedure SetParity(const Value: Char);
        procedure SetSleepTime(const Value: integer);
        procedure SetStopBits(const Value: Byte);
        procedure SetTimeout(const Value: cardinal);
        procedure SetOutText(const Value: string);
        procedure ComPortInitIntern();
        function IsComError(out oErrorMessage: string): boolean;
        function GetRaiseExceptionOnTimeout: boolean;
        function GetSleepTimeBeforeReceive: integer;
        procedure SetRaiseExceptionOnTimeout(const Value: boolean);
        procedure SetSleepTimeBeforeReceive(const Value: integer);

        function HasBeginAndEnd(aText: string): boolean;
        function ComReadEnd(aInstr: string): boolean;
        function GetInText: string;
    public
        constructor Create;

        procedure ComPortInit;
        function ComInitWithMsg: boolean;
        procedure ComPortClose();
        procedure ClearInputBuffer();
        function RemoveBeginAndEnd(aText: string): string;
        function InputAvailable(): boolean;
        function SendAndReceive(aText: string): string;

        procedure ReadyToSendOn();
        procedure ReadyToSendOff();
        procedure WriteElapsedTimeToStopRead();

        property name: string read GetName write SetName;
        property Active: Boolean read GetActive;
        property ComPort: integer read GetComPort write SetComPort;
        property Baud: integer read GetBaud write SetBaud;
        property DataBits: Byte read GetDataBits write SetDataBits;
        property StopBits: Byte read GetStopBits write SetStopBits;
        property Parity: Char read GetParity write SetParity;
        property FlowContr: integer read GetFlowContr write SetFlowContr;
        property EventMask: cardinal read GetEventMask write SetEventMask;
        property BuffSize: cardinal read GetBuffSize write SetBuffSize;
        property TimeOut: cardinal read GetTimeout write SetTimeout;
        property SleepTime: integer read GetSleepTime write SetSleepTime;
        property BeginStr: string read GetBeginStr write SetBeginStr;
        property EndChar: string read GetEndChar write SetEndChar;
        property NoOfChar: integer read GetNoOfChar write SetNoOfChar;
        property InText: string read GetInText;
        property OutText: string read GetOutText write SetOutText;

        property SleepTimeBeforeReceive: integer read GetSleepTimeBeforeReceive
            write SetSleepTimeBeforeReceive;
        property RaiseExceptionOnTimeout: boolean read GetRaiseExceptionOnTimeout
            write SetRaiseExceptionOnTimeout;
    end;


implementation


uses
    Windows,
    Forms;

{ TSerialIOPort }

constructor TSerialIOPort.Create;
begin
    inherited create;

    FActive := false;
    FEndChar := #0;
    FFlowContr := $00;
    FBuffSize := 255;
    fEventChar := #0;
    fEventMask := EV_RXCHAR or EV_CTS or EV_DSR or EV_RLSD or EV_BREAK or EV_ERR or EV_TXEMPTY;

    FSleepTimeBeforeReceive := 100;
    fErrTimeOut := false;
    fErrReachedEnd := false;
    fRaiseExceptionOnTimeout := true;
    self.NoOfChar := 0;
end;

function TSerialIOPort.IsComError(out oErrorMessage: string): boolean;
begin
    oErrorMessage := '';
    case FComInt.ComResult of
        scOk:
            begin
            end;
        scInvPort:
            oErrorMessage := 'Invalid Commport.';
        scNoPMInit:
            oErrorMessage := 'Protected mode initialisation failed.';
        scInvBaud:
            oErrorMessage := Format('Invalid baudrate [%d].', [FBaud]);
        scNoUserBreak:
            oErrorMessage := 'No user break.';
        scNoPMReset:
            oErrorMessage := 'Protected mode reset failed.';
        else
            oErrorMessage := 'Undefined error.';
    end;
    result := oErrorMessage <> '';
end;

procedure TSerialIOPort.ComPortInitIntern();
// --------------------------------------------------------------------------------------------------
// ComPort-Init: Löst im Fehlerfall eine Exception aus
// --------------------------------------------------------------------------------------------------
var
    xErrorMessage: string;
begin
    if FComPort <= 0 then
        EXIT;
    FComInt.ComInit(FComPort - 1);
    fComInt.ComGetSetEventMask(fEventMask, fEventChar);
    FComInt.ComSetState(FBaud, FDataBits, FStopBits, FParity, FFlowContr);
    if IsComError(xErrorMessage) then
        raise ESerialPortError.CreateFmt('Commport [%d] Error: %s', [fComPort, xErrorMessage]);
    FComInt.ComBufClear(DIR_INC);
    FComInt.ComBufClear(DIR_OUT);
    FActive := true;
end;

procedure TSerialIOPort.ComPortInit();
const
    INT_NUM_INIT_TRIES = 2;
var
    x: integer;
begin
    for x := 1 to INT_NUM_INIT_TRIES do
    begin
        try
            ComPortInitIntern();
            EXIT;
        except
            if x = INT_NUM_INIT_TRIES then
                raise;
        end;
    end;
end;

function TSerialIOPort.ComInitWithMsg: boolean;
// --------------------------------------------------------------------------------------------------
// ComPort-Init: Fängt Exceptions im Fehlerfall ab und zeigt sie als MessageBox
// --------------------------------------------------------------------------------------------------
begin
    result := false;
    if (fComPort <= 0) then
        EXIT;

    try
        ComportInit();
        result := true;
    except
        on e: ESerialPortError do
            Application.MessageBox(PChar(E.Message), 'Fatal Error', MB_ICONSTOP);
        on e: Exception do
            Application.MessageBox(PChar(Format('Commport [%d] Error: %s', [fComPort, e.Message])),
                'Fatal Error', MB_ICONSTOP);
    end;

end;

procedure TSerialIOPort.ComPortClose();
begin
    fComInt.ComReset();
    fActive := false;
end;

function TSerialIOPort.GetActive: Boolean;
begin
    EXIT(fActive);
end;

function TSerialIOPort.GetBaud: integer;
begin
    EXIT(fBaud);
end;

function TSerialIOPort.GetBeginStr: string;
begin
    EXIT(fBeginStr);
end;

function TSerialIOPort.GetBuffSize: cardinal;
begin
    EXIT(fBuffSize);
end;

function TSerialIOPort.GetComPort: Integer;
begin
    EXIT(fComPort);
end;

function TSerialIOPort.GetDataBits: Byte;
begin
    EXIT(fDataBits);
end;

function TSerialIOPort.GetEndChar: string;
begin
    EXIT(fEndChar);
end;

function TSerialIOPort.GetEventMask: cardinal;
begin
    EXIT(fEventMask);
end;

function TSerialIOPort.GetFlowContr: integer;
begin
    EXIT(fFlowContr);
end;

function TSerialIOPort.GetName: string;
begin
    EXIT(fName);
end;

function TSerialIOPort.GetNoOfChar: integer;
begin
    EXIT(fNoOfChar);
end;

function TSerialIOPort.GetOutText: string;
begin
    EXIT(fOutText);
end;

function TSerialIOPort.GetParity: Char;
begin
    EXIT(fParity);
end;

function TSerialIOPort.GetSleepTime: integer;
begin
    EXIT(fSleepTime);
end;

function TSerialIOPort.GetStopBits: Byte;
begin
    EXIT(fStopBits);
end;

function TSerialIOPort.GetTimeout: cardinal;
begin
    EXIT(fTimeout);
end;

procedure TSerialIOPort.SetBaud(const Value: integer);
begin
    fBaud := Value;
end;

procedure TSerialIOPort.SetBeginStr(const Value: string);
begin
    fBeginStr := Value;
end;

procedure TSerialIOPort.SetBuffSize(const Value: cardinal);
begin
    fBuffSize := Value;
end;

procedure TSerialIOPort.SetComPort(const Value: Integer);
begin
    fComPort := Value;
end;

procedure TSerialIOPort.SetDataBits(const Value: Byte);
begin
    fDataBits := Value;
end;

procedure TSerialIOPort.SetEndChar(const Value: string);
begin
    fEndChar := Value;
end;

procedure TSerialIOPort.SetEventMask(const Value: cardinal);
begin
    fEventMask := Value;
end;

procedure TSerialIOPort.SetFlowContr(const Value: integer);
begin
    fFlowContr := Value;
end;

procedure TSerialIOPort.SetName(const Value: string);
begin
    fName := Value;
end;

procedure TSerialIOPort.SetNoOfChar(const Value: integer);
begin
    fNoOfChar := Value;
end;

procedure TSerialIOPort.SetOutText(const Value: string);
const
    ERROR = 'Buffer overflow ! ! Port:%d';
var
    ch: Char;
    i: Integer;
begin
    if (FActive = TRUE) and (OutText <> '') then
    begin
        if FComInt.ComBufFull(DIR_OUT) = FALSE then
        begin
            i := 1;
            ch := OutText[i];
            while (i <= Length(OutText)) do
            begin
                if (FComInt.ComWrite(ch) = TRUE) then
                begin
                    inc(i);
                    ch := OutText[i];
                end
                else
                    raise ESerialPortError.CreateFmt(ERROR, [FComPort]);
            end;
        end

        else
            raise ESerialPortError.CreateFmt(ERROR, [FComPort]);
    end;
end;

procedure TSerialIOPort.SetParity(const Value: Char);
begin
    fParity := Value;
end;

procedure TSerialIOPort.SetSleepTime(const Value: integer);
begin
    fSleepTime := Value;
end;

procedure TSerialIOPort.SetStopBits(const Value: Byte);
begin
    fStopBits := Value;
end;

procedure TSerialIOPort.SetTimeout(const Value: cardinal);
begin
    fTimeout := Value;
end;

procedure TSerialIOPort.ClearInputBuffer();
// --------------------------------------------------------------------------------------------------
// Clears the input buffer.
// --------------------------------------------------------------------------------------------------
var
    xCount: integer;
begin
    xCount := 0;
    repeat
        FComInt.ComBufClear(DIR_INC);
        inc(xCount);
    until (not InputAvailable) or (xCount > 2);
    ASSERT((xCount < 3), 'TSerialIOPort.ClearInputBuffer: Input Buffer Not Cleared');
end;

procedure TSerialIOPort.ReadyToSendOff;
begin
    FComInt.ComRTSOff;
end;

procedure TSerialIOPort.ReadyToSendOn;
begin
    FComInt.ComRTSOn;
end;

function TSerialIOPort.RemoveBeginAndEnd(aText: string): string;
// --------------------------------------------------------------------------------------------------
// Remove the begin and end characters from the string aText and
// return the new string as the result
// --------------------------------------------------------------------------------------------------
var
    xLenBegin, xLenEnd, xLenRemainingText: integer;
    xPosBegin, xPosEnd: integer;
begin
    xPosBegin := Pos(fBeginStr, aText);
    if xPosBegin = 1 then
    begin
        xLenBegin := Length(fBeginStr);
        Delete(aText, 1, xLenBegin);
    end;

    xPosEnd := Pos(fEndChar, aText);
    xLenRemainingText := Length(aText);
    if xPosEnd = xLenRemainingText then
    begin
        xLenEnd := Length(fEndChar);
        Delete(aText, xPosEnd, xLenEnd);
    end;

    result := aText;
end;

function TSerialIOPort.InputAvailable(): boolean;
// --------------------------------------------------------------------------------------------------
// Return true if the Serial Objects Input Buffer is not empty
// --------------------------------------------------------------------------------------------------
begin
    result := (FComInt.ComBufCount(DIR_INC) > 0);
end;

function TSerialIOPort.ComReadEnd(aInstr: string): boolean;
begin
    if (FStopRead > self.Timeout * 1000) then
    begin
        fErrTimeOut := true;
        if fRaiseExceptionOnTimeout then
            raise ESerialPortError.CreateFmt('ComError: Timeout after %d msec! Port:%d, Timeout set to %d',
                [FStopRead, self.ComPort, self.TimeOut]);

        result := true;
        EXIT;
    end;

    result := HasBeginAndEnd(aInstr);

    if (not result) and ((self.NoOfChar > 0) and (Length(aInstr) >= self.NoOfChar)) then
    begin
        fErrReachedEnd := true;
        result := true;
    end;
end;

function TSerialIOPort.HasBeginAndEnd(aText: string): boolean;
var
    xLength: integer;
    xBeginOk, xEndOK: boolean;
    xTextEnd: string;
begin
    xLength := Length(aText);
    xBeginOk := ((self.BeginStr = '') or (Pos(self.BeginStr, aText) = 1));
    xTextEnd := Copy(aText, xLength - Length(self.EndChar) + 1, Length(self.EndChar));
    xEndOk := (xTextEnd = self.EndChar) and ((self.NoOfChar <= 0) or (xLength = self.NoOfChar));

    result := (xBeginOk and xEndOk);
end;

function TSerialIOPort.SendAndReceive(aText: string): string;
// --------------------------------------------------------------------------------------------------
// Instead of a looping several times calling InText, just call InText once
// --------------------------------------------------------------------------------------------------
begin
    fErrTimeOut := false;
    fErrReachedEnd := false;

    // Send the text to the port
    self.OutText := aText;

    // Wait a bit until the device processes the text that was sent
    Sleep(FSleepTimeBeforeReceive);

    // Receive the response text from the port
    result := self.InText;
end;

procedure TSerialIOPort.SetRaiseExceptionOnTimeout(const Value: boolean);
begin
    fRaiseExceptionOnTimeout := Value;
end;

procedure TSerialIOPort.SetSleepTimeBeforeReceive(const Value: integer);
begin
    fSleepTimeBeforeReceive := Value;
end;

function TSerialIOPort.GetInText: string;
// --------------------------------------------------------------------------------------------------
// Very similar to the original GetInText, except that the guard line
// 'if FComInt.ComBufCount( DIR_INC)' is removed so that any time GetInText is
// called, the only way out of the loop depends on the result of the ComReadEnd function
// --------------------------------------------------------------------------------------------------
var
    xCh: Char;
    xInstr: string;
begin
    result := '';
    if self.Active = false then
        exit;

    xCh := kNUL;
    xInstr := '';

    // record the start time of this attempt
    FStartRead := FComInt.ComGetSysTime;

    while true do
    begin

        while (FComInt.ComRead(xCh) = true) do
            xInstr := xInstr + xCh;

        // record the time so far
        FStopRead := FComInt.ComGetElapsedTime(FStartRead);

        if ComReadEnd(xInStr) then
            System.BREAK;

        // sleep before reading again
        if self.SleepTime > 0 then
            Sleep(self.SleepTime);

    end;

    result := xInstr;
end;

function TSerialIOPort.GetRaiseExceptionOnTimeout: boolean;
begin
    EXIT(fRaiseExceptionOnTimeout);
end;

function TSerialIOPort.GetSleepTimeBeforeReceive: integer;
begin
    EXIT(SleepTimeBeforeReceive);
end;

procedure TSerialIOPort.WriteElapsedTimeToStopRead;
begin
    fStopRead := FComInt.ComGetElapsedTime(FStartRead);
end;


end.
