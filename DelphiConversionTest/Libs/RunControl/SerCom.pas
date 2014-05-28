unit SerCom;
{ --------------------------------------------------------------------------------------------------
  Copyright © 2003 Zinsser Analytic GmbH - All rights reserved.
  Author       : Michael Ott (mo)
  Description  : Serial Port Communication Objects
  --------------------------------------------------------------------------------------------------
  date     op  Version  track-no improvement/change
  -------- --  -------  -------- -----------------------------------------------
  17.09.99 mo                    neu erstellt
  09.02.00 wl                    Reset: Funktion ComInt.ComReset hinzugefügt
  10.02.00 wl                    ComInitWithMsg: Erweiterte ComPortInit-Funktion: Exceptions werden abgefangen
  SendAndReceive: Funktion zum Senden und Empfangen mit Wartezeit dazwischen
  17.02.00 wl                    Reset: wieder entfernt
  21.03.00 wl                    TSerialPort: FActive von protected in public
  30.03.00 wl                    TSerialPort: neu: FName
  29.08.01 mo                    GetLineIn: Auswertung von FSleepTime geändert
  10.10.01 pk                    TSerialPort.SetLineOut: neu: Fehlermeldung bei Buffer overflow
  10.10.01 pk                    TSerialportExtend1: neues erweitertes Objekt mit Checksum Prüfung
  10.10.01 pk                         diverse utilies für Objektnachfahren von TSercom
  19.10.01 mo                    TSerialPort: neu WriteLog()
  23.10.01 mo                    TSerialPort.SetLineOut: korrigiert - letztes Zeichen wurde nicht geschrieben
  14.12.01 mo  			        Merge mit Änderungen PK Graffinity
  29.12.01 pk                    TSerialPortExtend1.SendAndReceive: Änderung
  26.09.02 wl           TN1294   WriteLogFile: ungenutzte Funktion - deaktiviert für Delphi 6
  11.12.02 wl           TN1345   TFuncParam in TSerFuncParam umbenannt
  09.04.03 wl           TN1345   alle Log/File-Funktionen entfernt, da sie das Compliant-DLL-Konzept unterlaufen
  03.07.03 wl           TN1501.1 property Name is now writable
  21.07.03 pk           TN1519   TSerialPortExtend2 : Simplified Send And Receive Logic
  23.07.03 pk           TN1519   TSerialPortExtend2 : ClearInputBuffer().  New. Clears the in buffer.
  23.07.03 pk           TN1525   ClearInputBuffer moved to TSerialPort class
  23.07.03 pk           TN1525   Constants moved from implementation section to interface section
  24.07.03 pk           TN1525   RemoveBeginAndEnd moved to TSerialPort class
  24.07.03 pk           TN1525   RemoveBeginAndEnd corrected
  25.07.03 pk           TN1525   RemoveBeginAndEnd corrected again
  25.07.03 pk           TN1525   InputAvailable corrected
  03.08.03 wl           TN1536   ComInt jetzt als property (nicht schön, aber für Tango und CAT Vortexer nötig)
  03.09.03 pk           TN1569   InputAvailable moved to TSerialPort
  03.09.03 pk           TN1569   Assertion in ClearInputBuffer
  23.06.04 tbh          TN2003   TExtSerialPortExtend2: neu
  21.07.04 tbh          TN2033   TSerialPortExtend2: new property: SleepTimeBeforeReceive
  29.11.04 pk           TN2249   New TSerialPort.ComPortClose -  closes com port
  13.04.06 thr          TN3044   ClearInputBuffer wird mehrfach wiederholt
  26.06.07 pk           TN3742   New: TSerialPort.ComportInitIntern : Try Init two times
  09.07.07 pk           TN3763   Requires new SuperCom.pas (115 kb)
  09.07.07 pk           TN3764   TSerialPortExtend2: fNoOfChar is now tested in ComReadEnd and HasBeginAndEnd
  09.07.07 pk           TN3764   TSerialPort.ComPortInitIntern: call SetComEventMask
  26.11.07 pk           TN3940   WARNING: fEndChar: changed from Char to String because required ending could be a string and not just a char
  26.11.07 pk           TN3940   TSerialPortExtend2.GetLineIn: Sleep only done if ComReadEnd is false
  06.05.08 pk           TN4098   Types of StartRead and StopRead changed to TSystemTime (DWORD)
  09.02.10 pk           TN4973   TSerialPort published section removed to avoid compiler warning
  18.09.13 wl           TN6045   TSerialPortExtend1 ist nur noch ein record
  -------------------------------------------------------------------------------------------------- }


interface


uses
    SysUtils,
    Supercom;

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

type
    ESerialPortError = class(Exception);

    TSerialPort = class
    protected
        FStartRead: TSYSTIME;
        FStopRead: TSYSTIME;
        FComInt: TComInt;
        FName: string;
        FComPort: Integer;
        FBaud: Longint;
        FDataBits: Byte;
        FStopBits: Byte;
        FParity: Char;
        FFlowContr: Longint;
        fEventMask: cardinal;
        fEventChar: char;
        FBuffSize: Word;
        FOutText: string;
        FTimeout: Word;
        FSleepTime: Word;
        FBeginStr: string;
        FEndChar: string;
        FNoOfChar: Word;
        function GetLineIn: string; virtual;
        function ComReadEnd(instr: string): boolean; virtual;
        procedure SetLineOut(OutText: string);
        procedure ComPortInitIntern();
        function IsComError(out oErrorMessage: string): boolean;
    public
        FActive: Boolean;
        procedure ComPortInit;
        function ComInitWithMsg: boolean;
        procedure ComPortClose();
        function SendAndReceive(iSendStr: string; iWaitTime: integer): string; virtual;
        procedure ClearInputBuffer();
        function RemoveBeginAndEnd(aText: string): string; virtual;
        function InputAvailable(): boolean; virtual;

        property ComInt: TComInt read FComInt;
        // --------------------------- CONSTRUCTOR
        constructor Create;

        property name: string read FName write FName;
        property Active: Boolean read FActive default False;
        property ComPort: Integer read FComPort write FComPort;
        property Baud: Longint read FBaud write FBaud;
        property DataBits: Byte read FDataBits write FDataBits default 8;
        property StopBits: Byte read FStopBits write FStopBits default 1;
        property Parity: Char read FParity write FParity default 'N';
        property FlowContr: Longint read FFlowContr write FFlowContr default $00;
        property EventMask: cardinal read fEventMask write fEventMask;
        property BuffSize: Word read FBuffSize write FBuffsize default 255;
        property TimeOut: Word read FTimeout write FTimeout default 1;
        property SleepTime: Word read FSleepTime write FSleepTime default 10;
        property BeginStr: string read FBeginStr write FBeginStr;
        property EndChar: string read FEndChar write FEndChar;
        property NoOfChar: Word read FNoOfChar write FNoOfChar default 0;
        property InText: string read GetLineIn;
        property OutText: string read FOutText write SetLineOut;
    end;

    // ----------------------------------------------------------------------------TSerialPortExtend1
    TSerialPortExtend1 = record
    strict private
        class function isNumber(strNumber: string): integer; static;
        class function HexToDecimal(strValue: string): byte; static;
    public
        class function StrToHex(strText: string): string; static;
        class function HexToString(strText: string): string; static;
    end;

    // ----------------------------------------------------------------------------TSerialPortExtend2
    TSerialPortExtend2 = class(TSerialPort)
    protected
        fErrTimeOut: boolean;
        fErrReachedEnd: boolean;
        fRaiseExceptionOnTimeout: boolean;
        FSleepTimeBeforeReceive: word;
        function HasBeginAndEnd(aText: string): boolean; virtual;
        function ComReadEnd(aInstr: string): boolean; override;
        function GetLineIn: string; override;
    public
        constructor Create;
        function SendAndReceive(aText: string): string; reintroduce;
        property SleepTimeBeforeReceive: word read FSleepTimeBeforeReceive write FSleepTimeBeforeReceive;
        property RaiseExceptionOnTimeout: boolean read fRaiseExceptionOnTimeout
            write fRaiseExceptionOnTimeout;
    end;


implementation


uses
    Windows,
    Forms;

{ TSerialPort }

constructor TSerialPort.Create;
begin
    inherited create;
    FActive := false;
    FEndChar := #0;
    FFlowContr := $00;
    FBuffSize := 255;
    fEventChar := #0;
    fEventMask := EV_RXCHAR or EV_CTS or EV_DSR or EV_RLSD or EV_BREAK or EV_ERR or EV_TXEMPTY;
end;

function TSerialPort.IsComError(out oErrorMessage: string): boolean;
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

// --------------------------------------------------------------------------------------------------
procedure TSerialPort.ComPortInitIntern();
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

procedure TSerialPort.ComPortInit();
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

// --------------------------------------------------------------------------------------------------
function TSerialPort.ComInitWithMsg: boolean;
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

// --------------------------------------------------------------------------------------------------
procedure TSerialPort.ComPortClose();
// --------------------------------------------------------------------------------------------------
begin
    fComInt.ComReset();
    fActive := false;
end;

// ------------------------------------------------------------------------------
function TSerialPort.ComReadEnd(instr: string): boolean;
// ------------------------------------------------------------------------------
var
    intLength: integer;
begin
    result := false;
    intLength := length(instr);
    // if character limit is set check length of string against limit
    if (FNoOfChar > 0) and (intLength >= FNoOfChar) then
        result := true;
    // if FEndChar is set, check last char string agains FEndChar
    if (FEndChar <> '') and (instr[intLength] = FEndChar) then
        result := true;
    if (FStopRead > FTimeout * 1000) then
    begin
        raise ESerialPortError.CreateFmt('ComError: Timeout after %d msec! Port:%d', [FStopRead, FComPort]);
    end;
end;

// --------------------------------------------------------------------------------------------------
function TSerialPort.SendAndReceive(iSendStr: string; iWaitTime: integer): string;
// --------------------------------------------------------------------------------------------------
const
    kRECEIVE_WAIT = 100;
var
    tmpStr: string;
    i: integer;
begin
    OutText := iSendStr;
    tmpStr := '';
    i := 0;
    repeat
        Sleep(kRECEIVE_WAIT);
        tmpStr := tmpStr + InText;
        inc(i);
    until (Pos(FEndChar, tmpStr) <> 0) or (i > 10 * iWaitTime);
    result := tmpStr;
end;

// ------------------------------------------------------------------------------
function TSerialPort.GetLineIn: string;
// ------------------------------------------------------------------------------
var
    ch: Char;
    instr: string;
begin
    Result := '';
    if FActive = false then
        exit;
    { begin
      raise ESerialPortError.CreateFmt('ComError: Input Port not Active ! Port:%d',[FComPort]);
      end; }
    ch := #0;
    instr := '';
    // if buffer not empty begin
    if FComInt.ComBufCount(DIR_INC) > 0 then
    begin
        // record the start time of this attempt
        FstartRead := FComInt.ComGetSysTime;
        if FSleepTime > 0 then
            sleep(FSleepTime);
        repeat
            while (FComInt.ComRead(ch) = true) do
                instr := instr + ch;
            if FSleepTime > 0 then
                sleep(FSleepTime);
            FStopRead := FComInt.ComGetElapsedTime(FstartRead);
        until ComReadEnd(instr);
    end;
    Result := instr;
end;

// ------------------------------------------------------------------------------
procedure TSerialPort.SetLineOut(OutText: string);
// ------------------------------------------------------------------------------
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
    // else raise ESerialPortError.CreateFmt('ComError: Output Port not Active ! Port:%d',[FComPort]);
end;

// --------------------------------------------------------------------------------------------------
procedure TSerialPort.ClearInputBuffer();
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
    ASSERT((xCount < 3), 'TSerialPort.ClearInputBuffer: Input Buffer Not Cleared');
end;

// --------------------------------------------------------------------------------------------------
function TSerialPort.RemoveBeginAndEnd(aText: string): string;
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

// --------------------------------------------------------------------------------------------------
function TSerialPort.InputAvailable(): boolean;
// --------------------------------------------------------------------------------------------------
// Return true if the Serial Objects Input Buffer is not empty
// --------------------------------------------------------------------------------------------------
begin
    result := (FComInt.ComBufCount(DIR_INC) > 0);
end;

{ TSerialPortExtend1 }

// ------------------------------------------------------------------------------------
class function TSerialPortExtend1.HexToString(strText: string): string;
// ------------------------------------------------------------------------------------
// takes in a bunch of Hex values separated by space, converts
// each Hex value in turn to a decimal value using the HexToDecimal
// function and in turn gets the character associated with that
// decimal value using the chr Delphi function.
// ------------------------------------------------------------------------------------
const
    kDelimiter = ' ';
var
    strCurrent: string;
    len: integer;
    intPos: integer;
begin
    len := length(strText);
    intPos := pos(kDelimiter, strText);
    while intPos > 0 do
    begin
        strCurrent := copy(strText, 1, intPos - 1);
        result := result + chr(HexToDecimal(strCurrent));
        strText := copy(strText, intPos + 1, len);
        intPos := pos(kDelimiter, strText);
    end;
    result := result + chr(HexToDecimal(strText));
end;

// ------------------------------------------------------------------------------------
class function TSerialPortExtend1.HexToDecimal(strValue: string): byte;
// ------------------------------------------------------------------------------------
// converts a string containing a hex value to a string containing
// the equivalent decimal (ordianl value) value
// ------------------------------------------------------------------------------------
var
    i: integer;
    len: integer;
    temp: integer;
    intCurrent: integer;
    outcome: integer;

begin
    len := length(strValue);
    temp := 0;
    for i := 1 to len do
    begin
        outcome := isNumber(strValue[i]);
        if outcome = 2 then
            intCurrent := StrToInt(strValue[i])
        else if outcome = 1 then
            intCurrent := Ord(strValue[i]) - 55
        else
            intCurrent := 0;
        temp := temp * 16 + intCurrent;
    end;

    result := byte(temp);
end;

class function TSerialPortExtend1.StrToHex(strText: string): string;
const
    kDelimiter = ' ';
var
    len: integer;
    i: integer;
begin
    strText := TrimRight(strText);
    len := length(strText);
    for i := 1 to len do
    begin
        result := result + ' ' + IntToHex(ord(strText[i]), 2);
    end;
    result := TrimLeft(result);
end;

// --------------------------------------------------------------------------------------------------
class function TSerialPortExtend1.isNumber(strNumber: string): integer;
// --------------------------------------------------------------------------------------------------
// 2 is returned if strNumber is a number made up of integers
// 1 is returned if strNumber is a hex string (made of A-F or 0-9)
// 0 is returned if strNumber has atleast one char that doesn't fit into the above criteria
// --------------------------------------------------------------------------------------------------
var
    len: longint;
    i: integer;
    ordVal: integer;
    ord1, ord2, ord3, ord4: integer;
begin
    result := 2;
    ord1 := ord('0');
    ord2 := ord('9');
    ord3 := ord('A');
    ord4 := ord('F');

    strNumber := UpperCase(strNumber);

    len := length(strNumber);
    for i := 1 to len do
    begin
        ordVal := ord(strNumber[i]);
        if result = 2 then // integer
            if ((ordVal < ord1) or (ordVal > ord2)) then
                result := 1;
        if result = 1 then // hex
            if (ordVal < ord1) or ((ordVal > ord2) and (ordVal < ord3)) or (ordVal > ord4) then
            begin
                result := 0;
                System.Break;
            end;

    end;

end;

{ TSerialPortExtend2 }

constructor TSerialPortExtend2.Create;
begin
    inherited;
    FSleepTimeBeforeReceive := 100;
    fErrTimeOut := false;
    fErrReachedEnd := false;
    fRaiseExceptionOnTimeout := true;
    fNoOfChar := 0;
end;

// --------------------------------------------------------------------------------------------------
function TSerialPortExtend2.ComReadEnd(aInstr: string): boolean;
// --------------------------------------------------------------------------------------------------
begin
    if (FStopRead > FTimeout * 1000) then
    begin
        fErrTimeOut := true;
        if fRaiseExceptionOnTimeout then
            raise ESerialPortError.CreateFmt('ComError: Timeout after %d msec! Port:%d, Timeout set to %d',
                [FStopRead, FComPort, fTimeOut]);

        result := true;
        EXIT;
    end;

    result := HasBeginAndEnd(aInstr);

    if (not result) and ((fNoOfChar > 0) and (Length(aInstr) >= fNoOfChar)) then
    begin
        fErrReachedEnd := true;
        result := true;
    end;
end;

// --------------------------------------------------------------------------------------------------
function TSerialPortExtend2.HasBeginAndEnd(aText: string): boolean;
// --------------------------------------------------------------------------------------------------
var
    xLength: integer;
    xBeginOk, xEndOK: boolean;
    xTextEnd: string;
begin
    xLength := Length(aText);
    xBeginOk := ((FBeginStr = '') or (Pos(FBeginStr, aText) = 1));
    xTextEnd := Copy(aText, xLength - Length(fEndChar) + 1, Length(fEndChar));
    xEndOk := (xTextEnd = fEndChar) and ((fNoOfChar <= 0) or (xLength = fNoOfChar));

    result := (xBeginOk and xEndOk);
end;

// --------------------------------------------------------------------------------------------------
function TSerialPortExtend2.SendAndReceive(aText: string): string;
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

// --------------------------------------------------------------------------------------------------
function TSerialPortExtend2.GetLineIn: string;
// --------------------------------------------------------------------------------------------------
// Very similar to the original GetLineIn, except that the guard line
// 'if FComInt.ComBufCount( DIR_INC)' is removed so that any time GetLineIn is
// called, the only way out of the loop depends on the result of the ComReadEnd function
// --------------------------------------------------------------------------------------------------
var
    xCh: Char;
    xInstr: string;
begin
    result := '';
    if FActive = false then
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
        if FSleepTime > 0 then
            Sleep(FSleepTime);

    end;

    result := xInstr;
end;


end.
