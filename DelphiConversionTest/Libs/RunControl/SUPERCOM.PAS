{ **************************************************** }
{ *             SuperCom 6.0b                        * }
{ *                                                  * }
{ * Windows NT                                       * }
{ * Copyright (c) 1991,2006 by ADONTEC Ltd.          * }
{ **************************************************** }

{ Dont forget to define _OS_NT_ globaly, menu:option compiler }

{$IFDEF WIN32}
{$IFNDEF _OS_NT_}
{$DEFINE _OS_NT_}
{$ENDIF}
{$ENDIF}
{ ----- Delphi 32bit and the SuperCom API

  PLEASE READ THIS

  The SuperCom API supports an OOP API (Object Oriented Programming) and a non OOP API.
  Both APIs are defined in supercom.pas, protocol.pas and terminal.pas.

  NON OOP API
  ------------
  Users of the non OOP API must precede an underscore "_" in front of the function
  name described in the manual.

  e.g.   use  _ComInit(Com);  instead of  ComInit(Com);



  OOP API
  ------------
  Users of the OOP API have to omit the parameter Com in the function calls.

  e.g. Var MyCOM:TTransport;
  MyCOM.ComInit(COM_2);
  :
  MyCom.ComDTROn;  // instead of  ComDTROn(Com);

  The following objects are defined in supercom.pas: TComInt, TDataLink, TTransport, TModem.

  The following objects are defined in protocol.pas: TProtocol.

  The following objects are defined in terminal.pas: TTerminal.

  ---- }

{$I SC.INC}       // set alignment for SuperCom structures
// don'change it nor remove it

unit SuperCom;


interface


{$IFOPT A+}
    SuperCom structures are better of
if compiled with ALING OFF $A -
{$ENDIF}
    uses Windows;

    const
        DLL_Path = 'SUPERCOM.DLL'; { Name of SuperCom DLL }

{$DEFINE _DO_255_COMMS_}
    const
        COM_1 = 0;
        COM_2 = 1;
        COM_3 = 2;
        COM_4 = 3;
        COM_5 = 4;
        COM_6 = 5;
        COM_7 = 6;
        COM_8 = 7;
        COM_9 = 8;
        COM_10 = 9;
        COM_11 = 10;
        COM_12 = 11;
        COM_13 = 12;
        COM_14 = 13;
        COM_15 = 14;
        COM_16 = 15;
        COM_17 = 16;
        COM_18 = 17;
        COM_19 = 18;
        COM_20 = 19;
        COM_21 = 20;
        COM_22 = 21;
        COM_23 = 22;
        COM_24 = 23;
        COM_25 = 24;
        COM_26 = 25;
        COM_27 = 26;
        COM_28 = 27;
        COM_29 = 28;
        COM_30 = 29;
        COM_31 = 30;
        COM_32 = 31;
        COM_33 = 32;
        COM_34 = 33;
        COM_35 = 34;
        COM_36 = 35;

{$IFDEF _DO_255_COMMS_}
        COM_255 = 254;
        MAX_COM = 255;
        LAST_COM = COM_255;
{$ELSE}
        MAX_COM = 36;
        LAST_COM = COM_36;
{$ENDIF}
        MAX_ISR = 14;

        LAST_RS232_COM = COM_10; { RS232 Part 1 (Version 2 compatibel) }
        FIRST_RS232_COM2 = COM_33; { RS232 Part 2 }
        LAST_RS232_COM2 = COM_36; { RS232 Part 2 }
        LAST_DIGI_COM = COM_32; { DigiCHANNEL PC/X }
        LAST_MULTI_COM = COM_8; { AST, ARNET }
        LAST_STAR_COM = COM_32; { StarGate or Hostess }

        NULL = 0; { C/C++ like NIL }

{$IFDEF _OS_NT_}
        (* Time constants in ms *)
        SEC0_05 = 50; (* 50 ms *)
        SEC0_1 = 100; (* 100 ms *)
        SEC0_2 = 200;
        SEC0_3 = 300;
        SEC0_4 = 400;
        SEC0_5 = 500; (* 1 sec *)
        SEC_1 = 1000;
        SEC_2 = 2000;
        SEC_3 = 3000;
        SEC_4 = 4000;
        SEC_5 = 5000;
        SEC_10 = 10000;
        SEC_20 = 20000;
        SEC_30 = 30000;
        SEC_40 = 40000;
        SEC_50 = 50000;
        SEC_60 = 60000; (* 1 min *)
{$ELSE}
        Sec0_05 = 1; { 50 ms }
        Sec0_1 = 2; { 100 ms }
        Sec0_3 = 6; { 300 ms }
        Sec0_5 = 9; { 500 ms }
        Sec_1 = 18; { 1 sec }
        Sec_2 = 37; { 2 sec }
        Sec_3 = 55; { 3 sec }
        Sec_4 = 73; { 4 sec }
        Sec_5 = 91; { 5 sec }
        Sec_10 = 182; { 10 sec }
        Sec_20 = 364; { 20 sec }
        Sec_30 = 546; { 30 sec }
        Sec_60 = 1092; { 1 min }
{$ENDIF}
        SOH = #01;
        STX = #02;
        BREAK = #03;
        EOT = #04;
        ACK = #06;
        BEL = #07;
        BS = #08;
        TAB = #09;
        LF = #10;
        FF = #12;
        CR = #13;
        DC1 = #17;
        DC3 = #19;
        NAK = #21;
        CAN = #24;
        ESC = #27;
        XON = #17;
        XOFF = #19;

        { Function Result Codes, ComResult }

        scOk = 0; { OK }
        scInvPort = 1; { Invalid Port, Port not connected }
        scNoUserBreak = 2; { UserBreak-Future not installed }
        scNoPMInit = 10; { Cannot Init. Protected Mode Data }
        scNoPMReset = 11; { Cannot Reset Protected Mode Data }
        scInvBaud = 12; { Baudrate not supported by driver }
        scInvFunc = 13; { Invalid function. }

        IER_ENABLE_RX = $01; { ISR Mode }
        IER_ENABLE_TX = $02;
        IER_ENABLE_ERROR = $04;
        IER_ENABLE_MODEM = $08;
        IER_ENABLE_ALL = $0F; { Default }

        SIGNAL_DTR = $01; { Com Drive Signals }
        SIGNAL_RTS = $02;
        SIGNAL_OUT2 = $08;
        SIGNAL_DTR_OUT2 = $09;
        SIGNAL_RTS_OUT2 = $0A;
        SIGNAL_DTR_RTS = $03;
        SIGNAL_DTR_RTS_OUT2 = $0B;
        SIGNAL_LOOP = $10;

        SIGNAL_NONE = $00; { Flow Check Signals }
        SIGNAL_CTS = $10;
        SIGNAL_DSR = $20;
        SIGNAL_DSR_CTS = $30;
        SIGNAL_XON = $80;
        SIGNAL_DSR_CTS_XON = $B0;

        RX_READY = $01; { Bits Of LineStatusReg }
        OVERRUN_ERROR = $02;
        PARITY_ERROR = $04;
        FRAMING_ERROR = $08;
        BREAK_DETECT = $10;
        TX_READY = $20;

        DELTA_CTS = $01; { Bits Of ModemStatusReg }
        DELTA_DSR = $02;
        DELTA_RI = $04;
        DELTA_DCD = $08;
        CLEAR_TO_SEND = $10;
        DATA_SET_READY = $20;
        RING_INDICATOR = $40;
        DATA_CARRIER_DETECT = $80;

        { On Init and after Reset signals }
        AR_DEFAULT = $00; { leave as found }
        AR_ENABLE_DTR = $01; { enable the DTR }
        AR_ENABLE_RTS = $02; { enable the RTS }
        AR_DISABLE_DTR = $04; { disable the DTR }
        AR_DISABLE_RTS = $08; { disable the RTS }

        brkChain = 0;
        brkDiscard = 1;
        brkNow = 2;

        MAXMODEMRESULT = 80;

        HAYES_INIT_STRING = 'ATE0L1M0Q0V1&C1&D2&S1S0=0';

        { result of ComPortPresent }
        PORT_UNAVAILABLE = 0;
        PORT_AVAILABLE_READY = 1;
        PORT_AVAILABLE_BUSY = 2;
        { additional result of ComPortPresentEx }
        PORT_AVAILABLE = 3;

        INVALID_PORT_ID = $FFFFFFFF;

{$IFDEF _OS_NT_}
        EV_RXCHAR = $0001; { Any Character received }
        EV_RXFLAG = $0002; { Received event character }
        EV_TXEMPTY = $0004; { Transmit Queue Empty }
        EV_CTS = $0008; { CTS changed state }
        EV_DSR = $0010; { DSR changed state }
        EV_DCD = $0020; { DCD changed state }
        EV_RLSD = $0020; { RLSD changed state }
        EV_BREAK = $0040; { BREAK received }
        EV_ERR = $0080; { Line status error occurred }
        EV_RI = $0100; { Ring signal detected }
        EV_RING = $0100; { Ring signal detected }
        EV_RX80FULL = $0400; { Receive buffer is 80 percent full }

        { SuperCom generated events. Has to be enabled manually. }
        { For more information see description of function ComGetSetEventMaskEx }

        EV_TXCACHE_EMPTY = $01000000; { SuperCom internal TX buffer empty }
        EV_RXCOUNT = $02000000; { Amount of data reached the threshold }

        { Call ComGetSetEventMaskEx to set }
        EV_CLOSE = $04000000; { SuperCom ComReset has been called }
        { At entry of ComReset, port may }
        { still be open }
        EV_TRIGGER = $08000000; { The specified message is received }

        EV_3964 = $80000000; { A 3964 protocol event }

        { includes all SuperCom supported events }
        EV_USER = EV_TXCACHE_EMPTY or EV_RXCOUNT or EV_CLOSE or EV_TRIGGER;

        { The next available event value }
        EV_NEXTRES = $10000000;

        ZEROEVENTMASK = $FFFFFFFF; { EventMask value to query the actual event }
        { mask using ComGetSetEventMask }
        STDEVENTMASK = EV_RXCHAR or EV_TXEMPTY or EV_CTS or EV_DSR or EV_RLSD or EV_BREAK or EV_ERR or
            EV_RING or EV_RX80FULL;

        RS485EVENTMASK = EV_RXCHAR or EV_TXEMPTY or EV_ERR or EV_RX80FULL;

        // includes almost any available event
        COMPLETEEVENTMASK = STDEVENTMASK or EV_USER;

        // reduced one in case of file transfer with low events profile
        XFILEEVENTMASK = EV_DSR or EV_RLSD or EV_ERR;

{$ENDIF}
        { bits 0..31 of protocol status word }
        INV_PROT_STATUS = $FFFFFFFF;

        PROTOCOL_STATUS_NULL = $FFFF; { only get status }
        PROTOCOL_STATUS_GET = $FFFF; { only get status }
        PROTOCOL_STATUS_STOP = $0001; { stop protocol }
        PROTOCOL_STATUS_RUNNING = $0002; { protocol is still running }
        PROTOCOL_STATUS_VALUE = $0004; { }
        PROTOCOL_STATUS_DELAY = $0008; { }

        { IRQ Lines }
        IRQ0 = 0;
        IRQ1 = 1;
        IRQ2 = 2;
        IRQ3 = 3;
        IRQ4 = 4;
        IRQ5 = 5;
        IRQ6 = 6;
        IRQ7 = 7;
        IRQ8 = 8;
        IRQ9 = 9;
        IRQ10 = 10;
        IRQ11 = 11;
        IRQ12 = 12;
        IRQ13 = 13;
        IRQ14 = 14;
        IRQ15 = 15;

        { Vectors }
        VEC1 = IRQ4 + 8; { Vector COM1/IRQ4 }
        VEC2 = IRQ3 + 8; { Vector COM2/IRQ3 }

        { FIFO Reg  bits 0 to 3 }
        FIFO_ENABLE = $01;
        FIFO_CLEAR_RX = $02;
        FIFO_CLEAR_TX = $04;
        FIFO_CLEAR_RX_TX = $06;

        { 16550 FIFO Threshold }
        { FIFO Reg  bits 4 to 7 }
        THRESHOLD01 = $00;
        THRESHOLD04 = $40;
        THRESHOLD08 = $80;
        THRESHOLD14 = $C0;

        evCHG_MODEM_STATUS = $01; { Events for ComGetEvent }
        evTHREMPTY = $02;
        evRXRDY = $04;
{$IFDEF _OS_NT_}
        evEXCEPTION = $08;
{$ELSE}
        evEXCEPTION = $06;
{$ENDIF}
        evCHAR_RECEIVED = $0001;
        evLAST_CHAR_SENT = $0004;
        evCTS_CHANGED = $0008;
        evDSR_CHANGED = $0010;
        evDCD_CHANGED = $0020;
        evBREAK_DETECTED = $0040;
        evERROR_OCCURED = $0080;
        evRI_DETECTED = $0100;
        evRI_CHANGED = $0200;

        { Reasons in AH within UserRXProc }
        REASON_RXREADY = $00;
        REASON_TXEMPTY = $01;
        REASON_MSTATUS = $02;
        REASON_ERROR = $03;
        REASON_USER = $20; { user events schould be > REASON_USER }

        { ComRS485 Action }
        RS485DISABLE = 0;
        RS485ENABLE = 1;
        RS485STATUS = 2;

        flRS485 = 1; { special flag }

        { supported protocols }
        PROTOCOL_ASCII = 0;
        PROTOCOL_XMODEM = 1;
        PROTOCOL_XMODEMCRC = 2;
        PROTOCOL_YMODEM = 3;
        PROTOCOL_YMODEMBATCH = 4;
        PROTOCOL_ZMODEM = 5;

        // pseudo protocols, variations
        PROTOCOL_ZMODEM_SAFE = 6;
        PROTOCOL_3964 = 7;
        PROTOCOL_YMODEM_G = 8;

        PROTOCOL_KERMIT = 9;

        PROTOCOL_USER = 64; { user protocols: PROTOCOL_USER + 1 etc. }

{$IFDEF _OS_NT_}
        SC_MAXPATH = 512;

        _MAX_PATH = 260;
        _MAX_DRIVE = 3;
        _MAX_DIR = 256;
        _MAX_FNAME = 256;
        _MAX_EXT = 256;
{$ELSE}
        SC_MAXPATH = 128;

        _MAX_PATH = 80;
        _MAX_DRIVE = 3;
        _MAX_DIR = 66;
        _MAX_FNAME = 9;
        _MAX_EXT = 5;
{$ENDIF}
        { SuperCom error codes. Other codes are the DOS error codes. }
        ERRTIMEOUT = (-9999);
        ERRCANCELED = (-9998);
        ERRBADLINE = (-9997);
        ERRGOTCAN = (-9996);
        ERRNOHEADER = (-9995);
        ERRBADSTRUCT = (-9994);
        ERRNOMEMORY = (-9993);

        { definitions used inside the file info function }
        acTXING = 0; { transmitting package }
        acRXING = 1; { receiving package }
        acUSERBREAK = 2; { Ctrl-Break }
        acSYN = 3; { Synchronize }
        acRETRY = 4; { Retry }
        acFILENAME = 5; { receiving Filename }
        acRXFIN = 6; { succesfully rx }
        acTXFIN = 7; { succesfully tx }
        acFILEDATA = 8; { on tx request file data }
        acBUFFERSIZE = 9; { Y/ZMODEM RXer requests buffer size }
        acQUEUEFIN = 10; { TFQ, queue empty }

        { Connect action codes }
        acCONNECT_INFO = 32; { info from RS_Connect }
        acCONNECT_OK = 33; { from RS_Connect CONNECT succeed }
        acCONNECT_FAILED = 34; { from RS_Connect CONNECT failed }

        { additional connect info codes delivered to ModemInfoProc }
        acCONNECT_INIT = 35; { info from RS_ConnectEx, ... sending initstring ... }
        acCONNECT_DIALING = 36; { from RS_ConnectEx, ... dialing ... }
        acCONNECT_CONNECTING = 37; { from RS_ConnectEx, ... connecting ... }
        acCONNECT_TAPI_RING = 38; { from RS_ConnectEx, ... TAPI Ringing ... }

        acUSER = 128; { user actions: acUSER + 1 etc. }

        { TAPI  additions }

        INV_TAPI_DEVICE = $FFFFFFFF;

        { return codes from RS_ConnectEx }
        CONNECT_CONNECTED = 0; { RS_ConnectEx connected successfuly }
        CONNECT_USERBREAK = 1; { RS_ConnectEx stopped by user }
        CONNECT_FAILED = 2; { RS_ConnectEx failed to connect }

        { RS_ConnectEx if run threaded }
        CONNECT_THREAD_STARTED = 3; { RS_ConnectEx will run threaded, result will send to InfoProc }
        CONNECT_THREAD_FAILED = 4; { RS_ConnectEx failed to start the thread, normally not happen }

        { return codes for RS_ConnectStatus }
        CONNECT_STATE_IDLE = 0;
        CONNECT_STATE_INIT = 1;
        CONNECT_STATE_DIAL = 2;
        CONNECT_STATE_CONNECTING = 3;
        CONNECT_STATE_CONNECTED = 4;

        CONNECT_INFO_PERIOD = SEC_1; { RS_Connect, info period }

        WAITACK = SEC_10; { Waiting period for Acknowledge }
        WAITONECHAR = SEC_1; { Waiting period for one char }
        WAITHEADER = SEC_5; { Waiting period for Header }
        WAITSTARTHEADER = SEC_60; { Waiting period for ZRINIT Header }

        { Protocol Info }

        MAXRETRYS = 5; { times }
        NAKDELAY = 1; { 1 ms }

        { return codes used in Info functions }
        infBREAK = 0; { Break protocol }
        infCONTINUE = 1; { continue protocol }
        infZRECOVERY = 2; { start ZMODEM crash recovery }
        infCONTSESSION = 3; { continue ZMODEM session (TXFile) }
        { Info proc returns infCONTSESSION }
        { after acTXFIN if more files are to }
        { be sent in the same ZMODEM session }

        (* These defines are used by ComInit. *)
        (* If you want to change the length of the buffers *)
        (* change these constants and build the library again *)
        BUFFERSIZE_RX = 8192; (* RX queue buffer len *)
        BUFFERSIZE_TX = 4096; (* TX queue buffer len *)

        SC_SN_LEN = 128;

    type
{$IFDEF _OS_NT_}
        TSCFSIZE = Int64;

        TDWORD = DWORD;
        TSYSTIME = TDWORD;
        PSYSTIME = ^TSYSTIME;
{$ELSE}
        TDWORD = LongInt;
        TSYSTIME = WORD;
        PSYSTIME = ^TSYSTIME;
{$ENDIF}
        LPSTRING = PCHAR;

        LPBYTE = ^BYTE;

        TCOMMID = BYTE;

    type
        TFDATE = record
            Year: Word; { e.g. 1994 }
            Month: Word; { 1..12 }
            Day: Word; { 1..31 }
            Hour: Word; { 0..23 }
            Min: Word; { 0..59 }
            Sec: Word; { 0..59 }
        end;

    type
        TFEXTDATA = record
            dwZMFileOptions: DWORD; { additionally file options }
            SN: array [0 .. SC_SN_LEN - 1] of Char; { serial number sent by ZMODEM sender }
            dwMode: DWORD; { file mode, usual 0 }
            dwTotalFiles: DWORD; { amount of files expected }
            TotalFileSize: TSCFSIZE; { amount of bytes expected, informational only }
            dwRes1: DWORD;
            pRes1: Pointer;
        end;

    type
        TFDATA = record
            FileName: array [0 .. SC_MAXPATH - 1] of Char; { file name }
            Protocol: Word; { type of protocol }
            FileSize: LONGINT; { file length }
            FileDate: TFDATE; { file date }
            ProtFlags: TDWORD; { flags for ZMODEM etc. }
            Flags2: TDWORD; { flags currently not used }

            ExtData: TFEXTDATA; { ZMODEM optional fields. May be provided or not }
            { If protocol module sets bit FLAG_ZMODEM_EXTDATA }
            { ExtData is supported }
        end;

        PFDATA = ^TFDATA;

    type
        TBufDir = (Dir_Inc, Dir_Out);
        TXBuffer = array [1 .. 128] of Char;

        TBuffer = record
            BufPtr: Pointer;
            BufLen: Word;
            BufCount: Word;
            WrPtr: Word;
            RdPtr: Word;
        end;

{$IFDEF _OS_NT_}

        SHORT = -32768 .. 32767;

        TDCBINFO = TDCB;

        TSaveState = record
            ComInfo: TDCBINFO;
            CommTimeouts: TCOMMTIMEOUTS;
            EventMask: TDWORD;
        end;

        PSaveState = ^TSaveState;

        TComData = record
            hCom: THANDLE;
            fXON: BOOLEAN;
            _DTR: BYTE;
            _RTS: BYTE;
            FlowControl: BYTE;
            UserCount: WORD;
            Result: WORD;
            ErrorState: BYTE;
            ModemDelta: BYTE;
            Res: BYTE;
            Flags: BYTE;
            nTimerId: TDWORD;
            Process: THANDLE;
            SaveState: TSaveState;
            Userdata: array [0 .. 15] of Byte;
        end;
{$ELSE}

        TSaveState = record
            BaudLo: Byte;
            BaudHi: Byte;
            LCRegData: Byte;
            MCRegData: Byte;
            IERegData: Byte;
            Spare: Byte;
        end;

        TComData = record
            XCom: Word;
            IOReg: Word;
            IEReg: Word;
            IIReg: Word;
            LCReg: Word;
            MCReg: Word;
            LSReg: Word;
            MSReg: Word;
            ErrorState: Byte;
            ModemState: Byte;

            Vec: Pointer;
            LineErrProc: Pointer;
            InBuffer: TBuffer;
            OutBuffer: TBuffer;
            IRQ: Byte;
            IRQVec: Byte;

            Baud: LongInt;
            DataBits: Byte;
            StopBits: Byte;
            Parity: Char;

            Mode: Byte;
            XON: Boolean;
            DoXON: Boolean;
            XONLimit: Word;
            XOFFLimit: Word;
            Queue: Byte;
            FlowControl: Byte;

            XONChar: Char;
            XOFFChar: Char;
            Event: Byte;
            Flags: Byte;
            Result: Word;

            SaveState: TSaveState;

            UserMXProc: Pointer;
            UserRXProc: Pointer;
            UserTXProc: Pointer;
            Userdata: array [0 .. 15] of Byte;
        end;
{$ENDIF}

        PComData = ^TComData;

    type
        TSCRPOS = record
            X: SHORT;
            Y: SHORT;
        end;

        PSCRPOS = ^TSCRPOS;

{$IFDEF _OS_NT_}
        { Communication types }

    type
        TRS232PARAM = record { type to be used with ComGetSetConfig-lpParam }
            wFlags: Word; // bit 0..31, set the corresponding bit,
            ComType: Byte; // the ComType  COMTYPE_RS232..TAPI
            SignalState: Byte; // AR_ENABLE_DTR,..., AR_DISABLE_RTS
        end;

    type
        PRS232PARAM = ^TRS232PARAM;

    type
        TRS232INFO = record { type to be used with ComGetInfo-lpParam }
            hCom: THANDLE; { the connection handle, e.g. ComGetInfo after ComInt/ComSetState }
            ComType: Byte; { Communication type }
        end;

    type
        PRS232INFO = ^TRS232INFO;

        { Communication types }

    const
        INVALID_COMTYPE = $FF;

    const
        COMTYPE_RS232 = 0;
    const
        COMTYPE_RS422 = COMTYPE_RS232;
    const
        COMTYPE_RS485 = 1;
    const
        COMTYPE_TAPI = 2;

    const
        COMTYPE_CAPI = 10;
    const
        COMTYPE_ISDN = COMTYPE_CAPI;

    const
        COMTYPE_WINSOCK = 20;
    const
        COMTYPE_WINSOCK_SERVER = (COMTYPE_WINSOCK + 1);
    const
        COMTYPE_WINSOCK_CLIENT = (COMTYPE_WINSOCK + 2);

        // TRS232PARAM.SignalState defines the DTR and RTS signal state when port is opened
        // does the same as calling ComSetDefaultConfig
    const
        PARAM_RS232_SIGNALSTATE = $0001;

    const
        EXTENSIONSIZE = 20;

    const
        PARAM_SET_COMTYPE = $0100;
    const
        PARAM_GET_COMTYPE = $0200;

    const
        PARAM_EXT_STRUCT = $8000; { can be used to extending the structure }

    const
        WAITLC_OPT_CHECK_INC = $00000001;
    const
        WAITLC_OPT_CLEAR_OUT = $00000002;

    const
        WAITLC_RES_OUT_OK = 0; { all data sent as expected }
    const
        WAITLC_RES_OUT_TIMEOUT = 1; { function timed out. wait >  maxWait }
    const
        WAITLC_RES_INC_DATA = 2; { data received }
    const
        WAITLC_RES_OUT_STOPPED = 3; { transmitter has stopped, for some reason }
    const
        WAITLC_RES_USERBREAK = 4; { user break }

        { ProtFlags:
          Following protocol flags which can be set FData.ProtFlags.
          The following Flags can be set by the application usually after the event acSYN
          is received in the FileInfoProc }

        {
          ==============================
          ZMODEM Protocol Defaults:
          ==============================
          -Receiver uses 1024 byte buffer
          -Receiver support CRC32
          -Receiver supports overlapped I/O
          -Crash recovery is supported


          Following flags can be set after acSYN is received

          Values: = $00000000 - = $000000FF reserved for ZMODEM

        }
    const
        FLAG_ZMODEM_BITS = $000000FF;

    const
        FLAG_ZMODEM_FDX = $00000001; { CANFDX   = $01 }
    const
        FLAG_ZMODEM_OVIO = $00000002; { CANOVIO  = $02 }
    const
        FLAG_ZMODEM_CRC32 = $00000020; { CANFC32  = $20 }

    const
        FLAG_ZMODEM_LNAI = $00000040; { Leave name as is e.g. mixed character, do not convert to lowercase }

    const
        FLAG_ZMODEM_EXTDATA = $00000080; { Support extended FData structure (e.g. SN, no of files etc.) }

        { Defines for the ZMODEM File Options  ZF0/ZF1 }

        { Bits to be ored in FData.ExtData.dwZMFileOptions }
        { Accept a path within the file name or not (default NOT) }
        { Yes if bit set. }
    const
        FO_HONOR_DIR = $00000010;

        { Honor file options (ZF1) received with ZFILE ? (default NO) }
        { Yes if bit set. }
    const
        FO_HONOR_OPTIONS = $00000020;

        { The File Options below may be set through SUPERCOM.INI or after acFILENAME / acFILEDATA }

        { ZMODEM Conversion Options (ZF0) }
    const
        FO_ZM_RESUME = $00000040;

        { Mask used to get the transfered file option bits }
    const
        FO_ZM_ZF1_MASK = $0000008F;

        { ZMODEM Management Options (ZF1) }
    const
        FO_ZM_SKIP_IF_ABSENT = $00000080;

        { One of the values below (ZF1) may be added to FData.dwZMFileOptions }
    const
        FO_ZM_RX_NEWER_OR_LONGER = 1;
    const
        FO_ZM_RX_IF_CRC = 2;
    const
        FO_ZM_RX_APPEND = 3;
    const
        FO_ZM_RX_REPLACE = 4;
    const
        FO_ZM_RX_IF_DIFFERENT = 5;
    const
        FO_ZM_RX_IF_ABSENT = 6;
    const
        FO_ZM_RX_NEWER = 7;
    const
        FO_ZM_RX_CHANGE_NAME = 8;

        {
          ==============================
          YMODEM Protocol defaults:
          ==============================

          Values = $00000100 - = $00000F00  reserved for YMODEM
        }

    const
        FLAG_YMODEM_BITS = $00000F00;

    const
        FLAG_YMODEM_USE_G = $00000100; { YMODEM_BATCH 'G' Option }
        { , set this bit on receiver site at acSYN to activate }

    const
        FLAG_YMODEM_LNAI = $00000200; { Leave name as is e.g. mixed character, do not convert to lowercase }

    const
        FLAG_SET_SYN_TIMEOUT = $00000800; { change acSYN timeout value, not implemented yet }

        {
          ==============================
          ASCII Protocol Defaults:
          ==============================

          -Data is send or received without any translation.

          -The receiver will receive data until CTRL-Z is received.

          -The sender will transmit all data in the file including CTRL-Z characters.



          Following flags can be set after acSYN is received


          TXer skips the char CR, LF, EOF (26) or NULL (0) and
          RXer ignores the specific character if one of the following bit is set
        }
    const
        FLAG_ASCII_BITS = $00FFF000;

        { RXer and TXer will strip data, if bit is set. }

        { Strip CR (char(13)) }
    const
        FLAG_ASCII_STRIP_CR = $00001000;
        { Strip LF (char(10)) }
    const
        FLAG_ASCII_STRIP_LF = $00002000;
        { Strip EOF (char(26)) }
    const
        FLAG_ASCII_STRIP_EOF = $00004000;
        { Strip 0 (char(0)) }
    const
        FLAG_ASCII_STRIP_NULL = $00008000;

        { Send LF after CR }
    const
        FLAG_ASCII_ADD_LF_AFTER_CR = $00010000;
        { Send CR before LF }
    const
        FLAG_ASCII_ADD_CR_BEFORE_LF = $00020000;

        { Dont' stop on EOF (CTRL-Z, char(26)).
          RXer will continue receiving even after an EOF.
          RXer will close the file only after a standard Timeout
          Note: The standard Timeout value can be altered with FLAG_ASCII_TIMEOUT. }
    const
        FLAG_ASCII_RX_CONTINUE_ON_EOF = $00040000;

        { Stop on EOF (CTRL-Z, char(26)).
          TXer will stop sending if it encounters an EOF }
    const
        FLAG_ASCII_TX_STOP_ON_EOF = $00080000;

        { RXer will use the Timeout value set in FData.Flags2.
          The RXer will stop and close the file after the timeout occurs. }
    const
        FLAG_ASCII_TIMEOUT = $00100000;

    const
        FLAG_ASCII_RES1 = $002000000;
    const
        FLAG_ASCII_RES2 = $004000000;
    const
        FLAG_ASCII_RES3 = $008000000;

        {
          ==============================
          DATA FILTER option bits
          =============================
          These bits are also used in conjunction with a Telnet link

          Values = $01000000 - = $FF000000

        }

    const
        FLAG_DATA_FILTER_BITS = $FF000000;

        { Receiver: Strip the char(0) if after char(13),  CR NULL  -->  CR }
    const
        FLAG_DATA_FILTER_RX_STRIP_NULL = $01000000;

        { Transmitter: Strip the char(0) if after char(13),  CR NULL  -->  CR }
    const
        FLAG_DATA_FILTER_TX_STRIP_NULL = $02000000;

        { Transmitter: Add the char(0) after char(13),  "CR" --> CR NULL,
          but only if not followed by a = $A, "CR LF" -> "CR LF"
        }
    const
        FLAG_DATA_FILTER_TX_ADD_NULL = $04000000;

        { Transmitter: double the FF char(255)  "FF" --> "FF FF" }
    const
        FLAG_DATA_FILTER_TX_DOUBLE_FF = $08000000;

        { Receiver: strip one FF char(255)  "FF FF" --> "FF" }
    const
        FLAG_DATA_FILTER_RX_STRIP_FF = $10000000;

    const
        FLAG_DATA_FILTER_RES1 = $20000000;
    const
        FLAG_DATA_FILTER_RES2 = $40000000;
    const
        FLAG_DATA_FILTER_RES3 = $80000000;

    const
        DF_FILE = 0;
    const
        DF_GLOBAL = 1;
    const
        DF_DISABLE = 2;

        { --   Data Monitor Procs -- }

    type
        LPXDATAPROC = function(Com: BYTE; lpBuffer: PCHAR; dwCount: DWORD): DWORD; stdcall;

        {
          dwFlags currently DIR_INC, DIR_OUT. More may follow.
        }
    type
        LPDATAMONITORPROC = function(Com: BYTE; dwFlags: DWORD; lpBuffer: PCHAR; dwCount: DWORD)
            : DWORD; stdcall;

    type
        LPRESETPROC = procedure(Com: Byte); stdcall;

        { structure used by ComGetSetProcs }

    type
        TDATAMONPROCS = record
            dwFlags: DWORD;
            pRXDataProc: LPXDATAPROC;
            pTXDataProc: LPXDATAPROC;
            pMonitorProc: LPDATAMONITORPROC;
            pResetProc: LPRESETPROC; { reserved for internal use }
        end;

    type
        LPDATAMONPROCS = ^TDATAMONPROCS;

        { Bit constants to be used with dwFlags
          NOTE:
          PROC_PARAM_GET... only returns the current proc address
          PROC_PARAM_SET... sets the new proc address and returns the previous one

        }
    const
        PROC_PARAM_GET_RXDATAPROC = $00000001;
    const
        PROC_PARAM_SET_RXDATAPROC = $00000002;
    const
        PROC_PARAM_GET_TXDATAPROC = $00000004;
    const
        PROC_PARAM_SET_TXDATAPROC = $00000008;
    const
        PROC_PARAM_GET_MONITORPROC = $00000010;
    const
        PROC_PARAM_SET_MONITORPROC = $00000020;

    const
        PROC_PARAM_INSERT_RESETPROC = $00000040;
    const
        PROC_PARAM_DEL_RESETPROC = $00000080;

        { Structure TEVENTDATA
          Structure definition for the parameter lpData in ComGetSetEventMaskEx .
          Not used fields should be set to 0
        }
    type
        TEVENTDATA = record
            dwFlags: DWORD; { n/a = reserved for future release }
            dwXThreshold: DWORD; { fire the event EV_RXCOUNT if reached this threshold }
            pBuffer: POINTER; { fire the event EV_TRIGGER if message received }
            dwBufferLen: DWORD; { amount of bytes in pBuffer }
            dwRes1: DWORD; { n/a }
            dwRes2: DWORD; { n/a }
            cEventChar: Char; { the byte used to report the event EV_RXFLAG }
        end;

    type
        LPEVENTDATA = ^TEVENTDATA;

        { values for ComGetThreadId-wThread }
    const
        SC_EVENT_THREAD = 0; { the thread that handles mainly the events and the reads }
    const
        SC_WRITE_THREAD = 1; { thread that handle mainly the data write }
    const
        SC_READ_THREAD = 2; { a pseudo thread at the moment }

        { SuperCom specific thread priorities, used in parameter Priority }
    const
        PRIORITY_VERY_LOW = 0;
    const
        PRIORITY_LOW = 1;
    const
        PRIORITY_MEDIUM = 2;
    const
        PRIORITY_HIGH = 3;
    const
        PRIORITY_VERY_HIGH = 4;
    const
        PRIORITY_ULTRA_HIGH = 5;
    const
        PRIORITY_DEFAULT = 15;

        // ComPortPresentEx: Struct to provide additional port present data
    type
        TCOMPRESENTDATAEX = record
            dwFlags: DWORD;
            dwVal1: DWORD;
            dwVal2: DWORD;
            pVal: PCHAR;
        end;

        PCOMPRESENTDATAEX = ^TCOMPRESENTDATAEX;

    const
        PORT_GET_NAMES_ONLY = $00000001;
        // quick run, no BUSY check,
        // just returns COMName and AVAILABLE or UNAVAILABLE

        LEN_COMNAME = 64; // COMxxxx

{$ENDIF}
        { --- CAPI --- }

    type
        TCAPIPARAMS = record
            wFlags: WORD; { bit 0..31, set the corresponding bit, }
            { if one of the following parameters are specified }
            ComType: BYTE; { see definition "Communication types" in supercom.h }
            Res0: Byte; // <<

            szOwnExtension: array [0 .. EXTENSIONSIZE - 1] of Char; { can be empty, or the users extension }
            dwController: DWORD; { usually 1, selects the installed ISDN board (1..127) }
            wCIP_Value: WORD; { default 2=digital data }
            Res1: WORD; // <<
            dwCIP_Mask: DWORD; { default 4=digital data }
            dwINFO_Mask: DWORD; { info mask, default charging }
            wLLCBufSize: WORD;
            wLLCCount: WORD;
            wLLCPos: WORD;
            Res2: WORD; // <<
            pLLCBuffer: ^BYTE;
            L1_Reason: WORD;
            B3_Reason: WORD;
            // extended structure , wFlags bit 0x80000 must be set to read extended struct
            dwFlagsEx: DWORD;
            B1_Protocol: WORD;
            B2_Protocol: WORD;
            B3_Protocol: WORD;
            wRes0: WORD;
            wRes1: WORD;
            wRes2: WORD;
            dwRes0: DWORD;
            dwRes1: DWORD;
            dwRes2: DWORD;
        end;

    type
        PCAPIPARAMS = ^TCAPIPARAMS;

        { Bit constants to be used with wFlags }

    const
        CAPI_PARAM_EXT = $0001;
        CAPI_PARAM_CTRL = $0002;
        CAPI_PARAM_CIPVAL = $0004;
        CAPI_PARAM_CIPMASK = $0008;

        CAPI_PARAM_INFOMASK = $0010;
        CAPI_PARAM_GETREASONS = $0020;
        CAPI_PARAM_GET_EXT = $0040;
        CAPI_PARAM_GET_CTRL = $0080;

        CAPI_PARAM_SET_COMTYPE = PARAM_SET_COMTYPE;
        CAPI_PARAM_GET_COMTYPE = PARAM_GET_COMTYPE;
        CAPI_PARAM_GET_CIPVAL = $0400;
        CAPI_PARAM_GET_CIPMASK = $0800;

        CAPI_PARAM_GETLLCDATA = $1000;
        CAPI_PARAM_PUTLLCDATA = $2000;
        CAPI_PARAM_RES4000 = $4000;
        CAPI_PARAM_EXTENDED = PARAM_EXT_STRUCT;

        { set on connect request }

    type
        TCAPIINFO = record
            szCalled: array [0 .. EXTENSIONSIZE - 1] of Char; { who gets called, destination }
            szCalling: array [0 .. EXTENSIONSIZE - 1] of Char; { who is calling, origin }
        end;

    type
        PCAPIINFO = ^TCAPIINFO;

        { Additional Events }

    const
        EV_ISDN_INFO = $8000;

        { SuperCom CAPI Status }
        SC_STATUS_IDLE = 0;
        SC_STATUS_CONNECTING = 1;
        SC_STATUS_DISCONNECTING = 2;
        SC_STATUS_CONNECTED_L1 = 3; { Physical Connection }
        SC_STATUS_CONNECTING_B3 = 4;
        SC_STATUS_DISCONNECTING_B3 = 5;
        SC_STATUS_CONNECTED_B3 = 6; { Virtual Connection }

        SC_STATUS_INV = $FF;

        { Reject Reason }
        CAPI_RR_ACCEPT = 0; { accept the call }
        CAPI_RR_IGNORE = 1; { ignore the call }
        CAPI_RR_NORMAL = 2; { reject call, normal call clearing }
        CAPI_RR_BUSY = 3; { reject call, user busy }
        CAPI_RR_UNAVAILABLE = 4; { reject call, channel not available }
        CAPI_RR_FACILITY = 5; { reject call, facility rejected }
        CAPI_RR_CHANNEL = 6; { reject call, channel unacceptable }
        CAPI_RR_INCOMPATIBLE = 7; { reject call, incompatible destination }
        CAPI_RR_OUT_OF_ORDER = 8; { reject call, destination out of order }

        { Info Mask, LISTEN REQ }
        CAPI_INFOMASK_CAUSE = $0001;
        CAPI_INFOMASK_DATETIME = $0002;
        CAPI_INFOMASK_DISPLAY = $0004;
        CAPI_INFOMASK_UTU_INFO = $0008; { user to user signaling }
        CAPI_INFOMASK_CHARGING = $0040;
        CAPI_INFOMASK_ALL = $007F;

        { CIP Mask, LISTEN REQ }
        { CIPMask, Bits defining type of incoming calls }
        { 0 = disables the signalling of incoming calls }
        CAPI_CIPMASK_NONE = $00000000;
        CAPI_CIPMASK_ANY = $00000001;
        CAPI_SPEECH = $00000002;
        CAPI_CIPMASK_SPEECH = $00000002;
        CAPI_CIPMASK_DIGITAL_DATA = $00000004; { default }
        CAPI_CIPMASK_DIGITAL_DATA2 = $00000008;
        CAPI_CIPMASK_3_AUDIO = $00000010; { 3.1 kHz Audio }
        CAPI_CIPMASK_7_AUDIO = $00000020; { 7.0 kHz Audio }
        CAPI_CIPMASK_PACKET_MODE = $00000080;
        CAPI_CIPMASK_56KBITS = $00000100; { 56KBITS }
        CAPI_CIPMASK_TELEPHONY = $00010000;
        CAPI_CIPMASK_ALL = $1FFF03FF;

        { CIPValue, CONNECT REQ }
        { CIPValue, Values defining type of outgoing call }
        CAPI_CIPVAL_SPEECH = 1;
        CAPI_CIPVAL_DIGITAL_DATA = 2; { default }
        CAPI_CIPVAL_DIGITAL_DATA2 = 3;
        CAPI_CIPVAL_3_AUDIO = 4; { 3.1 kHz Audio }
        CAPI_CIPVAL_7_AUDIO = 5; { 7.0 kHz Audio }
        CAPI_CIPVAL_PACKET_MODE = 7;
        CAPI_CIPVAL_56KBITS = 8; { 56KBITS }
        CAPI_CIPVAL_TELEPHONY = 16;

        { Protocol Selection }
        B1_0_HDLC = 0;
        B1_1_TRANSPARENT = 1;
        B1_2_V110_ASSYNC = 2;
        B1_3_V110_SYNC = 3;
        B1_4_T30 = 4;
        B1_5_HDLC_INVERTED = 5;
        B1_6_HDLCR = 6;

        B2_0_X75 = 0;
        B2_1_TRANSPARENT = 1;
        B2_2_SDLC = 2;
        B2_3_LAPD = 3;
        B2_4_T30 = 4;
        B2_5_PPP = 5;
        B2_6_TRANSPARENT_NO_CRC = 6;

        B3_0_TRANSPARENT = 0;
        B3_1_T90NL = 1;
        B3_2_ISO8208 = 2;
        B3_3_X25_DCE = 3;
        B3_4_T30 = 4;

        { CAPI Commands }
        CAPI_ALERT = 1;
        CAPI_CONNECT = 2;
        CAPI_CONNECT_ACT = 3;
        CAPI_DISCONNECT = 4;
        CAPI_LISTEN = 5;
        CAPI_INFO = 8;

        CAPI_SELECT_B_PROTOCOL = $41;
        CAPI_FACILITY = $80;
        CAPI_CONNECT_B3 = $82;
        CAPI_CONNECT_B3_ACT = $83;
        CAPI_DISCONNECT_B3 = $84;
        CAPI_DATA_B3 = $86;
        CAPI_CONNECT_B3_T90_ACT = $88;

        { CAPI Sub Commands }
        CAPI_REQUEST = $80;
        CAPI_CONFIRM = $81;
        CAPI_INDICATION = $82;
        CAPI_RESPONSE = $83;

        { CAPI return codes }
        CAPI_OK = $0000; { OK }
        CAPI_OK_1 = $0001; { NCPI not Supported, msg processed = OK }
        CAPI_OK_2 = $0002; { flags not Supported, msg processed = OK }
        CAPI_OK_3 = $0003; { alert allready sent by other app, msg processed = OK }

        CAPI_TOO_MANY = $1001;
        CAPI_BUFFER_FULL = $1103;
        CAPI_QUEUE_OVERFLOW = $1105;
        CAPI_BUSY = $1107;

        CAPI_BUSY_STATE = $2001;

        {

          /*----- New SuperCom functions */

          #define ComSetCAPIParams(a,b) ComGetSetConfig(a,b) /* to be called bevore ComInit */
          #define ComGetCAPIInfo(a,b)   ComGetInfo(a,b) /* call after EV_RING */


        }

    function CIP_TO_MASK(BIT_TO_SET: Byte): DWORD;

    { --- END CAPI --- }

    { --- SOCKET --- }

    const
        MAX_IP_LEN = 64;

    type
        TSOCKETPARAMS = record
            wFlags: WORD;
            { bit 0..15, set the corresponding bit (SOCKET_PARAM_....), if one of the following parameters are specified }
            ComType: BYTE; { see definition "Communication types" in supercom.pas }
            MaxServerConnections: Byte;
            wPort: Word;
            szIP: array [0 .. MAX_IP_LEN - 1] of Char;
        end;

    type
        PSOCKETPARAMS = ^TSOCKETPARAMS;

        { Bit constants to be used with wFlags }

    const
        SOCKET_PARAM_SET_IP = $0001;
    const
        SOCKET_PARAM_SET_PORT = $0002;
    const
        SOCKET_PARAM_GET_IP = $0004;
    const
        SOCKET_PARAM_GET_PORT = $0008;

    const
        SOCKET_PARAM_SET_MAX_CON = $0010;
    const
        SOCKET_PARAM_GET_MAX_CON = $0020;
    const
        SOCKET_PARAM_RES0040 = $0040;
    const
        SOCKET_PARAM_RES0080 = $0080;

    const
        SOCKET_PARAM_SET_COMTYPE = PARAM_SET_COMTYPE;
    const
        SOCKET_PARAM_GET_COMTYPE = PARAM_GET_COMTYPE;
    const
        SOCKET_PARAM_RES0400 = $0400;
    const
        SOCKET_PARAM_RES0800 = $0800;

    const
        SOCKET_PARAM_RES1000 = $1000;
    const
        SOCKET_PARAM_RES2000 = $2000;
    const
        SOCKET_PARAM_RES4000 = $4000;
    const
        SOCKET_PARAM_EXTENDED = PARAM_EXT_STRUCT;

    type
        TSOCKETINFO = record
            hCom: THANDLE; { the connection handle }
            wPort: WORD; { the used port number }
            szIP: array [0 .. MAX_IP_LEN - 1] of Char; { the used IP }
        end;

    type
        PSOCKETINFO = ^TSOCKETINFO;

        { Reject Reason }
    const
        SOCKET_RR_ACCEPT = 0;
        SOCKET_RR_BUSY = 3;
        SOCKET_RR_UNAVAILABLE = 4;
        SOCKET_RR_INCOMPATIBLE = 7;
        SOCKET_RR_OUT_OF_ORDER = 8;

        { --- END SOCKET --- }

        { WaitStringCompareFlags }
    const
        WSCF_SMART = 0;
        { If no lower case char is in the supplied buffer
          the data compared in upper case else if at least
          one lower case char is in the supplied buffer,
          the data compared as is }
        WSCF_BINARY = $00000001;
        { compare data as binary (as is), no case checked }
        WSCF_IGNORECASE = $00000002;
        { compare chars in upper case mode }
        WSCF_IGNORE_MULTISTRING = $00000004;
        { Ignore the delimiter char '|', buffer does not include
          a multi buffer/string }

        WSCF_TRIGGER_ADD = $00010000;
        // Add one as a trigger to get reported (EV_TRIGGER)
        WSCF_TRIGGER_DEL = $00020000;
        // Remove a trigger, it will not get reported (EV_TRIGGER)
        WSCF_TRIGGER_CLEAR = $00040000;
        // Remove a trigger, it will not get reported (EV_TRIGGER)
        WSCF_TRIGGER_GET = $00080000;
        // Get the one that is just reported (EV_TRIGGER)

        // Defines for ComDelegate, parameter dwType
    const
        SCOM_ONCOMM = $0001; // specifies a delegate for COMM events
        SCOM_ONCONNECT = $0002; // specifies a delegate for connect events
        SCOM_ONFILE = $0004; // specifies a delegate for file tranfer events
        SCOM_ONRXCHARS = $0008; // specifies a delegate for filter procs: RXDataproc
        SCOM_ONTXCHARS = $0010; // specifies a delegate for filter procs: TXDataproc
        SCOM_ONMONITOR = $0020; // specifies a delegate for filter procs: MonitorProc

    type
        TInfoProc = function(Com: Byte; Action: WORD; Data: TDWORD; FileDate: PFDATA): LongInt; stdcall;

    type
        PUSEREVENTPROC = function(Com: Byte; SComEvent: TDWORD): TDWORD; stdcall;

    type
        TComInt = object

            _Com: Byte;

            function GetCom: Byte; virtual;
            function ComGetVersion: Word; virtual;
            constructor ComInit(ComPort: Byte);
            constructor ComInithCom(ComPort: Byte; hCom: THANDLE);
            function ComGethCom: THANDLE; virtual;
            function ComGetSetEventProc(EventProc: PUSEREVENTPROC): PUSEREVENTPROC; virtual;
            procedure ComSetBufSize(RxBufferSize: TDWORD; TxBufferSize: TDWORD); virtual;
            function ComResult: Word; virtual;

            procedure ComSaveState; virtual;
            procedure ComSaveStateEx(var SaveState: TSAVESTATE); virtual;
            procedure ComRestoreState(var SaveState: TSAVESTATE); virtual;
            procedure ComXCHGState(var CurState: TSAVESTATE; var NewState: TSAVESTATE); virtual;

            procedure ComSetState(NewBaud: LongInt; NewDataBits: Byte; NewStopBits: Byte; NewParity: Char;
                NewFlow: Byte); virtual;
            procedure ComSetStateEx(NewBaud: LongInt; NewDataBits: Byte; NewStopBits: Byte; NewParity: Char;
                NewFlow: Byte; SignalState: Byte); virtual;
            procedure ComSetBaudRate(NewBaud: LongInt); virtual;
            function ComGetBaudRate: LongInt; virtual;
            function ComGetMaxBaudRate: LongInt; virtual;

            procedure ComReset; virtual;
            procedure ComResetEx(SignalState: Byte); virtual;

            procedure ComSetXONXOFFChars(NewXONChar, NewXOFFChar: Char); virtual;
            function ComGetXONXOFFChars: Word; virtual;
            procedure ComSetXONState(State: Boolean); virtual;

            function ComGetEvent: Byte; virtual;
            function ComGetEventEx: Word; virtual;

            function ComPeekByte(Bofs: Byte): Byte; virtual;
            function ComPeekWord(Bofs: Byte): Word; virtual;
            function ComPeekLong(Bofs: Byte): TDWORD; virtual;

            procedure ComPokeByte(Bofs: Byte; Data: Byte); virtual;
            procedure ComPokeWord(Bofs: Byte; Data: Word); virtual;
            procedure ComPokeLong(Bofs: Byte; Data: TDWORD); virtual;

            function ComRead(var RCh: Char): Boolean; virtual;
            function ComWrite(SCh: Char): Boolean; virtual;
            function ComReadEx(RCh: PChar; Count: TDWord): TDWord; virtual;
            function ComWriteEx(SCh: PChar; Count: TDWord): TDWord; virtual;
            function ComKickOut(SCh: Char): Boolean; virtual;
            function ComKickOutXOFF: Boolean; virtual;
            function ComKickOutXON: Boolean; virtual;
            function ComBufSize(BufDir: TBufDir): TDWORD; virtual;
            function ComBufCount(BufDir: TBufDir): TDWORD; virtual;
            function ComBufFull(BufDir: TBufDir): Boolean; virtual;
            function ComBufEmpty(BufDir: TBufDir): Boolean; virtual;
            procedure ComBufClear(BufDir: TBufDir); virtual;

            procedure ComSetFlow(Flow: Byte); virtual;
            function ComGetFlow: Byte; virtual;

            procedure ComEnableTX; virtual;
            function ComUngetChar(SCh: Char): Boolean; virtual;
            function ComSpyChar(var RCh: Char): Boolean; virtual;
            { ##18.12.1999

              NOTE: Parameter SpyPos is
              in SuperCom 32bit data relative e.g. 0 to BufCount-1
              in SuperCom 16bit buffer absolute e.g. 0 to BufSize

              The following is true for the SuperCom 32 bit version only.
              ComSWBufCount returns only the amount of data stored in the SuperComs buffer.
              Any data stored in a possible hardware buffer cannot be modified.
            }

            function ComSpyCharEx(var RCh: Char; BufDir: TBufDir; SpyPos: Word): Boolean; virtual;
            function ComSwapChar(BufDir: TBufDir; SpyPos: Word; NewCh: Char): Char; virtual;
            function ComSWBufCount(BufDir: TBufDir): DWord; virtual;

            function ComGetSysTime: TSYSTIME; virtual;
            function ComGetElapsedTime(Start: TSYSTIME): TSYSTIME; virtual;

            function ComModemState: Byte; virtual;
            function ComLineState: Byte; virtual;
            function ComXONState: Boolean; virtual;

            procedure ComDTROn; virtual;
            procedure ComRTSOn; virtual;
            procedure ComDTROff; virtual;
            procedure ComRTSOff; virtual;

            procedure ComBreakOn; virtual;
            procedure ComBreakOff; virtual;

            function ComGetMCReg: Byte; virtual;
            procedure ComPutMCReg(Val: Byte); virtual;

            procedure ComSetSCResult(Res: Word); virtual;

            function ComFIFOEnabled: Boolean; virtual;
            function ComDisableFIFO: Boolean; virtual;
            function ComEnableFIFO(Threshold: Byte): Boolean; virtual;

            function ComGetIRQVec(IRQ: Byte): Byte; virtual;
{$IFDEF VER100}
            function ComValid(Com: Byte): Boolean; virtual;
{$ELSE}
            function ComValid(Com: Byte): Boolean; overload; virtual;
            function ComValid: Boolean; overload; virtual;
{$ENDIF}
            function ComRS485(Action: Byte): Boolean; virtual;

            { ##02.03.1998 }
            procedure ComSetCOMName(szNewCOMName: PCHAR); virtual;
            procedure ComSetRIQuard(State: Boolean); virtual;
            procedure ComSetRIGuard(State: Boolean); virtual;
            function ComGetSetEventMask(dwNewEventMask: TDWORD; cEventChar: Char): TDWORD; virtual;
            function ComHWBufSize(BufDir: TBufDir): TDWORD; virtual;
            function ComHWBufCount(BufDir: TBufDir): TDWORD; virtual;
            function ComGetProcess: THANDLE; virtual;

            function ComPortPresent(Com: Byte): WORD; virtual;
            function ComPortPresentEx(dwPortIdx: DWORD; dwComType: DWORD; lpszCOMName: PCHAR;
                pExtData: POINTER): DWORD; virtual;

            procedure ComEnterCritSec; virtual;
            procedure ComLeaveCritSec; virtual;
            procedure ComEnterCritPortSec; virtual;
            procedure ComLeaveCritPortSec; virtual;
            procedure ComOpen(InBuf: POINTER; InLen: Word; OutBuf: POINTER; OutLen: Word); virtual;

            function ComStateDTR: Boolean; virtual;
            function ComStateRTS: Boolean; virtual;

            function ComStateCTS: Boolean; virtual;
            function ComStateDSR: Boolean; virtual;
            function ComStateDCD: Boolean; virtual;
            function ComStateRI: Boolean; virtual;

            function ComStateDeltaCTS: Boolean; virtual;
            function ComStateDeltaDSR: Boolean; virtual;
            function ComStateDeltaDCD: Boolean; virtual;
            function ComStateDeltaRI: Boolean; virtual;

            function ComGetSetConfig(var lpParam): Boolean; virtual;
            function ComGetSetConfigEx(var lpParam; lpParam2: POINTER): DWORD; virtual;
            function ComGetInfo(var lpInfo): Boolean; virtual;
            function ComGetVersionEx(ComType: BYTE): Word; virtual;
            function ComGetSetProcs(var lpProcs: TDATAMONPROCS): BOOLEAN; virtual;
            function ComGetSetEventMaskEx(dwNewEventMask: DWORD; cEventChar: Char; var lpData: TEVENTDATA)
                : DWORD; virtual;

            function ComSetDefaultConfig(SignalState: Byte): Boolean; virtual;

            function ComSetDefaultConfigEx(var lpBaud: DWORD; var lpDataBits: Byte; var lpStopBits: Byte;
                var lpParity: Char; var lpFlow: Byte): Boolean; virtual;

            function ComGetPriorityValues(Priority: Byte; var lpdwThreadPriority: DWORD;
                var lpdwThreadClass: DWORD): Boolean; virtual;
            function ComGetSetPriority(hThisThread: THANDLE; var lpPriority: Byte): Boolean; virtual;
            function ComCallDataMonitorProc(lpDataMonitorProc: LPDATAMONITORPROC; Com: BYTE; dwFlags: DWORD;
                lpBuffer: PCHAR; // Var lpBuffer;
                dwCount: DWORD): DWORD; virtual;
            function ComCallXDataProc(lpXDataProc: LPXDATAPROC; Com: BYTE; lpBuffer: PCHAR; // Var lpBuffer;
                dwCount: DWORD): DWORD; virtual;
            procedure ComCallResetProc(lpResetProc: LPRESETPROC; Com: Byte); virtual;
            function ComCallUserEventProc(lpEventProc: PUSEREVENTPROC; Com: BYTE; dwSComEvent: DWORD)
                : DWORD; virtual;
            function ComFilterInit(dwNewFlags: DWORD; dwAction: DWORD): DWORD; virtual;
            function ComFilterFlush(BufDir: TBufDir): BOOLEAN; virtual;
            function ComFilterRX(RCh: Char): Boolean; virtual;
            function ComFilterTX(var lpBuffer): Word; virtual;
            procedure ComSetDCB(var lpDCB); virtual;
            procedure ComGetDCB(var lpDCB); virtual;
            function ComGetCharTime: DWORD; virtual;
            function ComDelegate(dwType: DWORD; hWnd: DWORD; pUserEventFunction: POINTER): POINTER; virtual;
        end;

    type
        TDataLink = object(TComInt)

            constructor ComInit(ComPort: Byte);
            constructor ComInithCom(ComPort: Byte; hCom: THANDLE);
            function RS_Global_Yield(hWnd: Word): LongInt; virtual;
            procedure RS_SetYieldState(State: Boolean); virtual;
            function RS_GetYieldState: Boolean; virtual;

            procedure RS_TXOnLine(SCh: Char); virtual;
            procedure RS_RXOnLine(var RCh: Char); virtual;

            function RS_TXInTime(SCh: Char; WaitTime: TSYSTIME): Boolean; virtual;
            function RS_RXInTime(var RCh: Char; WaitTime: TSYSTIME): Boolean; virtual;

            function RS_ErrorState: Byte; virtual;
            function RS_ModemState: Byte; virtual;
            function RS_LineState: Byte; virtual;

            procedure RS_Break; virtual;

            function RS_XOFFLimit: Word; virtual;
            function RS_XONLimit: Word; virtual;

            procedure RS_DisableUserBreak; virtual;
            procedure RS_EnableUserBreak; virtual;
            function RS_UserBreak: Boolean; virtual;
            procedure RS_UserBreakOption(Option: Integer); virtual;
            procedure RS_UserBreakPressed; virtual;

            procedure RS_Delay(WaitTime: TSYSTIME); virtual;

            procedure RS_SetXONLimit(XONLimit: Word); virtual;
            procedure RS_SetXOFFLimit(XOFFLimit: Word); virtual;

            function RS_GetXONState: Boolean; virtual;
            procedure RS_SetXONState(State: Boolean); virtual;
            procedure RS_TXCtlChar(CtlChar: Char; minWait, maxWait: TSYSTIME); virtual;
        end;

    type
        TTransport = object(TDataLink)

            constructor ComInit(ComPort: Byte);
            constructor ComInithCom(ComPort: Byte; hCom: THANDLE);
            procedure RS_TXPacket(var Packet; Len: Word; var Result: Word); virtual;
            procedure RS_RXPacket(var Packet; Len: Word; var Result: Word); virtual;

            procedure RS_TXQPacket(var Packet; Len: Word; var Result: Word); virtual;
            procedure RS_RXQPacket(var Packet; Len: Word; var Result: Word); virtual;

            procedure RS_TXPInTime(var Packet; Len: Word; OneChDelay: TSYSTIME; var Result: Word); virtual;
            procedure RS_RXPInTime(var Packet; Len: Word; OneChDelay: TSYSTIME; var Result: Word); virtual;

            function RS_GetChkSum(var Packet; Len: Word): Byte; virtual;
            function RS_GetBCC(var Packet; Len: Word): Byte; virtual;
            procedure Do_CRC(var CRC: SHORT; Ch: Char); virtual;
            procedure RS_GetCRC_CCITT(var Packet; Len: Word; var CRC: SHORT); virtual;

            procedure RS_GetCRC_CCITT32(var Packet; Len: Word; var CRC: LongInt); virtual;
            procedure Do_CRC32(var CRC: LongInt; Ch: Char); virtual;

            function RS_GetTimeZone: LongInt; virtual;
            procedure RS_SetTimeZone(TimeZone: LongInt); virtual;

            procedure RS_FileDateToDos(DateInSec: LongInt; var Year: Word; var Month: Word; var Day: Word;
                var Hour: Word; var Min: Word; var Sec: Word); virtual;
            function RS_FileDateToUnix(Year, Month, Day, Hour, Min, Sec: Word): LongInt; virtual;

            function RS_GetDataPacket(lpRetBuffer: PCHAR; var lpdwRetBufLen: TDWord; lpStartString: PCHAR;
                dwStartLen: TDWord; lpEndString: PCHAR; dwEndLen: TDWord; dwMaxChars: TDWord;
                pTimeOut: PSYSTIME): DWORD; virtual;

            function RS_WaitString(lpRetBuffer: PCHAR; var lpdwRetLen: TDWord; lpDataToWait: PCHAR;
                dwDataLen: TDWord; pTimeOut: PSYSTIME; dwFlags: DWORD): DWORD; virtual;
            { Function RS_WaitString(
              lpRetBuffer:PCHAR;
              lpdwRetLen:LPDWord;
              lpDataToWait:PCHAR;
              dwDataLen:TDWord;
              pTimeOut:PSYSTIME;
              dwFlags:DWORD):DWORD;   VIRTUAL; }

            function RS_WaitOutLineClear(Timeout: TSYSTIME; dwOptions: DWORD): DWORD; virtual;
        end;

    type
        TModem = object(TTransport)
            constructor ComInit(ComPort: Byte);
            constructor ComInithCom(ComPort: Byte; hCom: THANDLE);
            function RS_Carrier: Boolean; virtual;
            procedure RS_Dial(Number: PCHAR); virtual;
            procedure RS_HangUp; virtual;
            procedure RS_PickUp; virtual;
            function RS_Ringing: Boolean; virtual;

            procedure RSH_HangUp; virtual;
            procedure RSH_PickUp; virtual;
            procedure RSH_ModemInit(InitString: PCHAR); virtual;

            procedure RSH_ModemWaitOK; virtual;

            function RS_Connect(ConnectDelay: TSYSTIME; CheckModemResult: Boolean; ModemResult: PCHAR;
                InfoProc: TInfoProc): Boolean; virtual;

            function RS_ConnectEx(ConnectDelay: TSYSTIME; bCheckModemResult: Boolean; lpszModemResult: PCHAR;
                lpfInfoProc: TInfoProc; lpszInitString: PCHAR; lpszDestAddress: PCHAR; bRunThreaded: Boolean)
                : DWORD; virtual;

            function RS_ConnectState: DWORD; virtual;

            function RS_TAPIGetDeviceName(dwDeviceIndex: DWORD; lpszDevName: PCHAR; var dwBufferLen: DWORD)
                : DWORD; virtual;

            function RS_TAPIGetDialString(dwDeviceIndex: DWORD; lpszDialBufferString: PCHAR;
                var lpdwBufferLen: DWORD): DWORD; virtual;

            procedure RS_HangUpEx; virtual;
        end;

        { DLL exports }
    function _ComGetVersion: Word; stdcall;
    procedure _ComInit(Com: Byte); stdcall;
    procedure _ComInithCom(Com: Byte; hCom: THANDLE); stdcall;
    function _ComGethCom(Com: Byte): THANDLE; stdcall;
    function _ComGetSetEventProc(Com: Byte; EventProc: PUSEREVENTPROC): PUSEREVENTPROC; stdcall;
    procedure _ComSetBufSize(Com: Byte; RxBufferSize: TDWORD; TxBufferSize: TDWORD); stdcall;
    function _ComResult(Com: Byte): Word; stdcall;
    procedure _ComSaveState(Com: Byte); stdcall;
    procedure _ComRestoreState(Com: Byte; var SaveState: TSAVESTATE); stdcall;
    procedure _ComSaveStateEx(Com: Byte; var SaveState: TSAVESTATE); stdcall;
    procedure _ComXCHGState(Com: Byte; var CurState: TSAVESTATE; var NewState: TSAVESTATE); stdcall;

    procedure _ComSetState(Com: Byte; NewBaud: LongInt; NewDataBits: Byte; NewStopBits: Byte; NewParity: Char;
        NewFlow: Byte); stdcall;

    procedure _ComSetStateEx(Com: Byte; NewBaud: LongInt; NewDataBits: Byte; NewStopBits: Byte;
        NewParity: Char; NewFlow: Byte; SignalState: Byte); stdcall;

    procedure _ComSetBaudRate(Com: Byte; NewBaud: LongInt); stdcall;
    function _ComGetBaudRate(Com: Byte): LongInt; stdcall;
    function _ComGetMaxBaudRate(Com: Byte): LongInt; stdcall;
    procedure _ComReset(Com: Byte); stdcall;
    procedure _ComResetEx(Com: Byte; SignalState: Byte); stdcall;

    procedure _ComSetXONXOFFChars(Com: Byte; NewXONChar, NewXOFFChar: Char); stdcall;
    function _ComGetXONXOFFChars(Com: Byte): Word; stdcall;
    procedure _ComSetXONState(Com: Byte; State: Boolean); stdcall;

    function _ComGetEvent(Com: Byte): Byte; stdcall;
    function _ComGetEventEx(Com: Byte): Word; stdcall;

    function _ComPeekByte(Com: Byte; Bofs: Byte): Byte; stdcall;
    function _ComPeekWord(Com: Byte; Bofs: Byte): Word; stdcall;
    function _ComPeekLong(Com: Byte; Bofs: Byte): TDWORD; stdcall;

    procedure _ComPokeByte(Com: Byte; Bofs: Byte; Data: Byte); stdcall;
    procedure _ComPokeWord(Com: Byte; Bofs: Byte; Data: Word); stdcall;
    procedure _ComPokeLong(Com: Byte; Bofs: Byte; Data: TDWORD); stdcall;

    function _ComRead(Com: Byte; var RCh: Char): Boolean; stdcall;
    function _ComWrite(Com: Byte; SCh: Char): Boolean; stdcall;

    function _ComReadEx(Com: Byte; RCh: PChar; Count: TDWord): TDWord; stdcall;
    function _ComWriteEx(Com: Byte; SCh: PChar; Count: TDWord): TDWord; stdcall;

    function _ComKickOut(Com: Byte; SCh: Char): Boolean; stdcall;
    function _ComKickOutXOFF(Com: Byte): Boolean; stdcall;
    function _ComKickOutXON(Com: Byte): Boolean; stdcall;
    function _ComBufSize(Com: Byte; BufDir: TBufDir): TDWORD; stdcall;
    function _ComBufCount(Com: Byte; BufDir: TBufDir): TDWORD; stdcall;
    function _ComGetSysTime: TSYSTIME; stdcall;
    function _ComGetElapsedTime(Start: TSYSTIME): TSYSTIME; stdcall;
    function _ComBufFull(Com: Byte; BufDir: TBufDir): Boolean; stdcall;
    function _ComBufEmpty(Com: Byte; BufDir: TBufDir): Boolean; stdcall;
    procedure _ComBufClear(Com: Byte; BufDir: TBufDir); stdcall;
    procedure _ComSetFlow(Com: Byte; Flow: Byte); stdcall;
    function _ComGetFlow(Com: Byte): Byte; stdcall;
    procedure _ComEnableTX(Com: Byte); stdcall;
    function _ComSpyChar(Com: Byte; var RCh: Char): Boolean; stdcall;
    function _ComUngetChar(Com: Byte; SCh: Char): Boolean; stdcall;

    function _ComSpyCharEx(Com: Byte; var RCh: Char; BufDir: TBufDir; SpyPos: Word): Boolean; stdcall;
    function _ComSwapChar(Com: Byte; BufDir: TBufDir; SpyPos: Word; NewCh: Char): Char; stdcall;

    function _ComModemState(Com: Byte): Byte; stdcall;
    function _ComLineState(Com: Byte): Byte; stdcall;
    function _ComXONState(Com: Byte): Boolean; stdcall;

    procedure _ComDTROn(Com: Byte); stdcall;
    procedure _ComRTSOn(Com: Byte); stdcall;
    procedure _ComDTROff(Com: Byte); stdcall;
    procedure _ComRTSOff(Com: Byte); stdcall;

    procedure _ComBreakOn(Com: Byte); stdcall;
    procedure _ComBreakOff(Com: Byte); stdcall;

    function _ComGetMCReg(Com: Byte): Byte; stdcall;
    procedure _ComPutMCReg(Com: Byte; Val: Byte); stdcall;

    procedure _ComSetSCResult(Com: Byte; Res: Word); stdcall;

    function _ComFIFOEnabled(Com: Byte): Boolean; stdcall;
    function _ComDisableFIFO(Com: Byte): Boolean; stdcall;
    function _ComEnableFIFO(Com, Threshold: Byte): Boolean; stdcall;
    function _ComGetIRQVec(IRQ: Byte): Byte; stdcall;

    function _ComValid(Com: Byte): Boolean; stdcall;
    function _ComRS485(Com: Byte; Action: Byte): Boolean; stdcall;

    function __GetUserCount(Com: Byte): Word; stdcall;

    procedure _ComSetCOMName(szNewCOMName: PCHAR); stdcall;
    procedure _ComSetRIQuard(Com: Byte; State: Boolean); stdcall;
    procedure _ComSetRIGuard(Com: Byte; State: Boolean); stdcall;
    function _ComGetSetEventMask(Com: Byte; dwNewEventMask: TDWORD; cEventChar: Char): TDWORD; stdcall;
    function _ComHWBufSize(Com: Byte; BufDir: TBufDir): TDWORD; stdcall;
    function _ComHWBufCount(Com: Byte; BufDir: TBufDir): TDWORD; stdcall;
    function _ComGetProcess(Com: Byte): THANDLE; stdcall;

    function _ComPortPresent(Com: Byte): Word; stdcall;
    function _ComPortPresentEx(dwPortIdx: DWORD; dwComType: DWORD; lpszCOMName: PCHAR; pExtData: POINTER)
        : DWORD; stdcall;

    procedure _ComEnterCritSec; stdcall;
    procedure _ComLeaveCritSec; stdcall;
    procedure _ComEnterCritPortSec(Com: Byte); stdcall;
    procedure _ComLeaveCritPortSec(Com: Byte); stdcall;

    procedure _ComOpen(Com: Byte; InBuf: POINTER; InLen: Word; OutBuf: POINTER; OutLen: Word); stdcall;

    function _ComStateDTR(Com: Byte): Boolean; stdcall;
    function _ComStateRTS(Com: Byte): Boolean; stdcall;

    function _ComStateCTS(Com: Byte): Boolean; stdcall;
    function _ComStateDSR(Com: Byte): Boolean; stdcall;
    function _ComStateDCD(Com: Byte): Boolean; stdcall;
    function _ComStateRI(Com: Byte): Boolean; stdcall;

    function _ComStateDeltaCTS(Com: Byte): Boolean; stdcall;
    function _ComStateDeltaDSR(Com: Byte): Boolean; stdcall;
    function _ComStateDeltaDCD(Com: Byte): Boolean; stdcall;
    function _ComStateDeltaRI(Com: Byte): Boolean; stdcall;

    function _ComGetSetConfig(Com: Byte; var lpParam): Boolean; stdcall;
    function _ComGetSetConfigEx(Com: Byte; var lpParam; lpParam2: POINTER): DWORD; stdcall;

    function _ComGetInfo(Com: Byte; var lpInfo): Boolean; stdcall;

    function _ComSetDefaultConfig(Com: Byte; SignalState: Byte): Boolean; stdcall;
    function _ComSetDefaultConfigEx(Com: Byte; var lpBaud: DWORD; var lpDataBits: Byte; var lpStopBits: Byte;
        var lpParity: Char; var lpFlow: Byte): Boolean; stdcall;

    { #### }

    function _ComGetVersionEx(ComType: BYTE): Word; stdcall;

    function _ComGetSetProcs(Com: Byte; var lpProcs: TDATAMONPROCS): BOOLEAN; stdcall;

    function _ComGetSetEventMaskEx(Com: Byte; dwNewEventMask: DWORD; cEventChar: Char; var lpData: TEVENTDATA)
        : DWORD; stdcall;

    function _RS_WaitString(Com: BYTE; lpRetBuffer: PCHAR; var lpdwRetLen: DWord; lpDataToWait: PCHAR;
        dwDataLen: TDWord; pTimeOut: PSYSTIME; dwFlags: DWORD): DWORD; stdcall;

    function _RS_ConnectEx(Com: Byte; ConnectDelay: TSYSTIME; bCheckModemResult: Boolean;
        lpszModemResult: PCHAR; lpfInfoProc: TInfoProc; lpszInitString: PCHAR; lpszDestAddress: PCHAR;
        bRunThreaded: Boolean): DWORD; stdcall;

    function _RS_ConnectState(Com: Byte): DWORD; stdcall;

    function _RS_TAPIGetDeviceName(Com: BYTE; dwDeviceIndex: DWORD; lpszDevName: PCHAR;
        var dwBufferLen: DWORD): DWORD; stdcall;

    function _RS_TAPIGetDialString(Com: Byte; dwDeviceIndex: DWORD; lpszDialBufferString: PCHAR;
        var lpdwBufferLen: DWORD): DWORD; stdcall;

    procedure _RS_HangUpEx(Com: BYTE); stdcall;

    { ComGetThreadId: returns the thread handle or INVTHREADID, if thread does not exist }
    { The name ComGetThreadId is misused, it means ComGetThreadHandle }
    function _ComGetThreadId(Com: Byte; wThread: Word): DWORD; stdcall;

    function _ComGetPriorityValues(Priority: Byte; var lpdwThreadPriority: DWORD; var lpdwThreadClass: DWORD)
        : Boolean; stdcall;

    function _ComGetSetPriority(Com: Byte; hThisThread: THANDLE; var lpPriority: Byte): Boolean; stdcall;

    { Function _ComSetThreadPriority ; stdcall; }

    function _ComCallDataMonitorProc(lpDataMonitorProc: LPDATAMONITORPROC; Com: BYTE; dwFlags: DWORD;
        lpBuffer: PCHAR; dwCount: DWORD): DWORD; stdcall;

    function _ComCallXDataProc(lpXDataProc: LPXDATAPROC; Com: BYTE; lpBuffer: PCHAR; dwCount: DWORD)
        : DWORD; stdcall;

    procedure _ComCallResetProc(lpResetProc: LPRESETPROC; Com: Byte); stdcall; { internal use }

    function _ComCallUserEventProc(lpEventProc: PUSEREVENTPROC; Com: BYTE; dwSComEvent: DWORD)
        : DWORD; stdcall;

    function _ComFilterInit(Com: BYTE; dwNewFlags: DWORD; dwAction: DWORD): DWORD; stdcall;
    function _ComFilterFlush(Com: BYTE; BufDir: TBufDir): BOOLEAN; stdcall;

    function _ComFilterRX(Com: BYTE; RCh: Char): Boolean; stdcall;
    function _ComFilterTX(Com: BYTE; var lpBuffer): Word; stdcall;

    procedure _ComGetDCB(Com: BYTE; var lpDCB); stdcall;
    procedure _ComSetDCB(Com: BYTE; var lpDCB); stdcall;

    function _RS_GetDataPacket(Com: BYTE; lpRetBuffer: PCHAR; var lpdwRetBufLen: TDWord; lpStartString: PCHAR;
        dwStartLen: TDWord; lpEndString: PCHAR; dwEndLen: TDWord; dwMaxChars: TDWord; pTimeOut: PSYSTIME)
        : DWORD; stdcall;

    { ### }

    { **************************************************** }
    { *                DataLink                          * }
    { *                                                  * }
    { **************************************************** }

    function _RS_Global_Yield(Wnd: HWND): TDWORD; stdcall;
    function _RS_GetYieldState: Boolean; stdcall;
    procedure _RS_SetYieldState(State: Boolean); stdcall;

    procedure _RS_TXOnLine(Com: Byte; SCh: Char); stdcall;
    procedure _RS_RXOnLine(Com: Byte; var RCh: Char); stdcall;

    function _RS_TXInTime(Com: Byte; SCh: Char; WaitTime: TSYSTIME): Boolean; stdcall;
    function _RS_RXInTime(Com: Byte; var RCh: Char; WaitTime: TSYSTIME): Boolean; stdcall;

    function _RS_ErrorState(Com: Byte): Byte; stdcall;
    function _RS_ModemState(Com: Byte): Byte; stdcall;
    function _RS_LineState(Com: Byte): Byte; stdcall;

    procedure _RS_Break(Com: Byte); stdcall;

    function _RS_XOFFLimit(Com: Byte): Word; stdcall;
    function _RS_XONLimit(Com: Byte): Word; stdcall;

    function _RS_UserBreak: Boolean; stdcall;
    procedure _RS_DisableUserBreak; stdcall;
    procedure _RS_EnableUserBreak; stdcall;
    procedure _RS_UserBreakOption(Option: Integer); stdcall;
    procedure _RS_UserBreakPressed; stdcall;

    procedure _RS_Delay(WaitTime: TSYSTIME); stdcall;

    procedure _RS_SetXONLimit(Com: Byte; XONLimit: Word); stdcall;
    procedure _RS_SetXOFFLimit(Com: Byte; XOFFLimit: Word); stdcall;

    function _RS_GetXONState(Com: Byte): Boolean; stdcall;
    procedure _RS_SetXONState(Com: Byte; State: Boolean); stdcall;

    procedure _RS_TXCtlChar(Com: Byte; CtlChar: Char; minWait, maxWait: TSYSTIME); stdcall;

    { **************************************************** }
    { *                TransPort                         * }
    { *                                                  * }
    { **************************************************** }

    procedure _RS_TXPacket(Com: Byte; var Packet; Len: Word; var Result: Word); stdcall;
    procedure _RS_RXPacket(Com: Byte; var Packet; Len: Word; var Result: Word); stdcall;

    procedure _RS_TXQPacket(Com: Byte; var Packet; Len: Word; var Result: Word); stdcall;
    procedure _RS_RXQPacket(Com: Byte; var Packet; Len: Word; var Result: Word); stdcall;

    procedure _RS_TXPInTime(Com: Byte; var Packet; Len: Word; OneChDelay: TSYSTIME;
        var Result: Word); stdcall;
    procedure _RS_RXPInTime(Com: Byte; var Packet; Len: Word; OneChDelay: TSYSTIME;
        var Result: Word); stdcall;

    function _RS_GetChkSum(var Packet; Len: Word): Byte; stdcall;
    function _RS_GetBCC(var Packet; Len: Word): Byte; stdcall;
    procedure _Do_CRC(var CRC: SHORT; Ch: Char); stdcall;
    procedure _RS_GetCRC_CCITT(var Packet; Len: Word; var CRC: SHORT); stdcall;

    procedure _RS_GetCRC_CCITT32(var Packet; Len: Word; var CRC: LongInt); stdcall;
    procedure _Do_CRC32(var CRC: LongInt; Ch: Char); stdcall;

    function _RS_GetTimeZone: LongInt; stdcall;
    procedure _RS_SetTimeZone(TimeZone: LongInt); stdcall;

    procedure _RS_FileDateToDos(DateInSec: LongInt; var Year: Word; var Month: Word; var Day: Word;
        var Hour: Word; var Min: Word; var Sec: Word); stdcall;
    function _RS_FileDateToUnix(Year, Month, Day, Hour, Min, Sec: Word): LongInt; stdcall;
    function _RS_WaitOutLineClear(Com: Byte; Timeout: TSYSTIME; dwOptions: DWORD): DWORD; stdcall;

    function _ComGetCharTime(Com: Byte): DWORD; stdcall;
    function _ComDelegate(Com: Byte; dwType: DWORD; hWnd: DWORD; pUserEventFunction: POINTER)
        : POINTER; stdcall;

    { **************************************************** }
    { *                  Modem                           * }
    { *                                                  * }
    { **************************************************** }

    function _RS_Carrier(Com: Byte): Boolean; stdcall;
    procedure _RS_Dial(Com: Byte; Number: PCHAR); stdcall;
    procedure _RS_HangUp(Com: Byte); stdcall;
    procedure _RS_PickUp(Com: Byte); stdcall;
    function _RS_Ringing(Com: Byte): Boolean; stdcall;

    procedure _RSH_HangUp(Com: Byte); stdcall;
    procedure _RSH_PickUp(Com: Byte); stdcall;

    procedure _RSH_ModemInit(Com: Byte; InitString: PCHAR); stdcall;
    procedure _RSH_ModemWaitOK(Com: Byte); stdcall;
    function _RS_Connect(Com: Byte; ConnectDelay: TSYSTIME; CheckModemResult: Boolean; ModemResult: PCHAR;
        InfoProc: TInfoProc): Boolean; stdcall;

    { Helper function }

    { Some helpers to analyse the result of RS_GetDataPacket }

    { SCOM_DP_FAILED   results true if function failed }
    function SCOM_DP_FAILED(dwRes: DWORD): Boolean;
    { SCOM_DP_OK   results true if function succeeded }
    function SCOM_DP_OK(dwRes: DWORD): Boolean;
    { If one start and one end string used the result will be Hex 00010001 (if succeed)
      If a multi sting buffer were used in start and/or end string, which one failed
      will be returned by one of the following macros }
    function SCOM_DP_END_WHICH(dwRes: DWORD): DWORD;
    function SCOM_DP_START_WHICH(dwRes: DWORD): DWORD;

    function IsSet(dwBits: DWORD; dwFlags: DWORD): Boolean;
    function NotSet(dwBits: DWORD; dwFlags: DWORD): Boolean;
    function SetBits(var dwBits: DWORD; dwFlags: DWORD): DWORD;
    function ClearBits(var dwBits: DWORD; dwFlags: DWORD): DWORD;


implementation


procedure _ComInit; stdcall; external DLL_Path name 'ComInit';
function _ComGetBaudRate; stdcall; external DLL_Path name 'ComGetBaudRate';
function _ComGetMaxBaudRate; stdcall; external DLL_Path name 'ComGetMaxBaudRate';
procedure _ComSetState; stdcall; external DLL_Path name 'ComSetState';
procedure _ComSetStateEx; stdcall; external DLL_Path name 'ComSetStateEx';
procedure _ComReset; stdcall; external DLL_Path name 'ComReset';
procedure _ComResetEx; stdcall; external DLL_Path name 'ComResetEx';
function _ComResult; stdcall; external DLL_Path name 'ComResult';
function _ComGetEvent; stdcall; external DLL_Path name 'ComGetEvent';
function _ComGetEventEx; stdcall; external DLL_Path name 'ComGetEventEx';
procedure _ComSetXONXOFFChars; stdcall; external DLL_Path name 'ComSetXONXOFFChars';
function _ComGetXONXOFFChars; stdcall; external DLL_Path name 'ComGetXONXOFFChars';
function _ComPeekByte; stdcall; external DLL_Path name 'ComPeekByte';
function _ComPeekWord; stdcall; external DLL_Path name 'ComPeekWord';
function _ComPeekLong; stdcall; external DLL_Path name 'ComPeekLong';
procedure _ComPokeByte; stdcall; external DLL_Path name 'ComPokeByte';
procedure _ComPokeWord; stdcall; external DLL_Path name 'ComPokeWord';
procedure _ComPokeLong; stdcall; external DLL_Path name 'ComPokeLong';
function _ComRead; stdcall; external DLL_Path name 'ComRead';
function _ComWrite; stdcall; external DLL_Path name 'ComWrite';
function _ComReadEx; stdcall; external DLL_Path name 'ComReadEx';
function _ComWriteEx; stdcall; external DLL_Path name 'ComWriteEx';
function _ComKickOut; stdcall; external DLL_Path name 'ComKickOut';
function _ComBufSize; stdcall; external DLL_Path name 'ComBufSize';
function _ComBufCount; stdcall; external DLL_Path name 'ComBufCount';
procedure _ComBufClear; stdcall; external DLL_Path name 'ComBufClear';
function _ComBufFull; stdcall; external DLL_Path name 'ComBufFull';
function _ComBufEmpty; stdcall; external DLL_Path name 'ComBufEmpty';
procedure _ComSetFlow; stdcall; external DLL_Path name 'ComSetFlow';
function _ComGetFlow; stdcall; external DLL_Path name 'ComGetFlow';
function _ComModemState; stdcall; external DLL_Path name 'ComModemState';
function _ComLineState; stdcall; external DLL_Path name 'ComLineState';
function _ComXONState; stdcall; external DLL_Path name 'ComXONState';
procedure _ComSetXONState; stdcall; external DLL_Path name 'ComSetXONState';
procedure _ComDTROn; stdcall; external DLL_Path name 'ComDTROn';
procedure _ComRTSOn; stdcall; external DLL_Path name 'ComRTSOn';
procedure _ComDTROff; stdcall; external DLL_Path name 'ComDTROff';
procedure _ComRTSOff; stdcall; external DLL_Path name 'ComRTSOff';
procedure _ComBreakOn; stdcall; external DLL_Path name 'ComBreakOn';
procedure _ComBreakOff; stdcall; external DLL_Path name 'ComBreakOff';
function _ComGetMCReg; stdcall; external DLL_Path name 'ComGetMCReg';
procedure _ComPutMCReg; stdcall; external DLL_Path name 'ComPutMCReg';
function _ComFIFOEnabled; stdcall; external DLL_Path name 'ComFIFOEnabled';
function _ComEnableFIFO; stdcall; external DLL_Path name 'ComEnableFIFO';
function _ComDisableFIFO; stdcall; external DLL_Path name 'ComDisableFIFO';
procedure _ComSaveState; stdcall; external DLL_Path name 'ComSaveState';
procedure _ComRestoreState; stdcall; external DLL_Path name 'ComRestoreState';
procedure _ComXCHGState; stdcall; external DLL_Path name 'ComXCHGState';
function _ComValid; stdcall; external DLL_Path name 'ComValid';
procedure _ComSetSCResult; stdcall; external DLL_Path name 'ComSetSCResult';
function _ComGetSysTime; stdcall; external DLL_Path name 'ComGetSysTime';
function _ComGetElapsedTime; stdcall; external DLL_Path name 'ComGetElapsedTime';
function _ComGethCom; stdcall; external DLL_Path name 'ComGethCom';
function _RS_GetXONState; stdcall; external DLL_Path name 'RS_GetXONState';
procedure _RS_SetXONState; stdcall; external DLL_Path name 'RS_SetXONState';
procedure _RS_TXOnLine; stdcall; external DLL_Path name 'RS_TXOnLine';
procedure _RS_RXOnLine; stdcall; external DLL_Path name 'RS_RXOnLine';
function _RS_TXInTime; stdcall; external DLL_Path name 'RS_TXInTime';
function _RS_RXInTime; stdcall; external DLL_Path name 'RS_RXInTime';
function _RS_ErrorState; stdcall; external DLL_Path name 'RS_ErrorState';
function _RS_ModemState; stdcall; external DLL_Path name 'RS_ModemState';
function _RS_LineState; stdcall; external DLL_Path name 'RS_LineState';
procedure _RS_Break; stdcall; external DLL_Path name 'RS_Break';
procedure _RS_DisableUserBreak; stdcall; external DLL_Path name 'RS_DisableUserBreak';
procedure _RS_EnableUserBreak; stdcall; external DLL_Path name 'RS_EnableUserBreak';
function _RS_UserBreak; stdcall; external DLL_Path name 'RS_UserBreak';
procedure _RS_UserBreakPressed; stdcall; external DLL_Path name 'RS_UserBreakPressed';
procedure _RS_Delay; stdcall; external DLL_Path name 'RS_Delay';
function _RS_Global_Yield; stdcall; external DLL_Path name 'RS_Global_Yield';
function _RS_GetYieldState; stdcall; external DLL_Path name 'RS_GetYieldState';
procedure _RS_SetYieldState; stdcall; external DLL_Path name 'RS_SetYieldState';
function _RS_XOFFLimit; stdcall; external DLL_Path name 'RS_XOFFLimit';
function _RS_XONLimit; stdcall; external DLL_Path name 'RS_XONLimit';
procedure _RS_TXPacket; stdcall; external DLL_Path name 'RS_TXPacket';
procedure _RS_RXPacket; stdcall; external DLL_Path name 'RS_RXPacket';
procedure _RS_TXPInTime; stdcall; external DLL_Path name 'RS_TXPInTime';
procedure _RS_RXPInTime; stdcall; external DLL_Path name 'RS_RXPInTime';
procedure _RS_UserBreakOption; stdcall; external DLL_Path name 'RS_UserBreakOption';
procedure _RS_SetXONLimit; stdcall; external DLL_Path name 'RS_SetXONLimit';
procedure _RS_SetXOFFLimit; stdcall; external DLL_Path name 'RS_SetXOFFLimit';
procedure _RS_TXQPacket; stdcall; external DLL_Path name 'RS_TXQPacket';
procedure _RS_RXQPacket; stdcall; external DLL_Path name 'RS_RXQPacket';
function _RS_Carrier; stdcall; external DLL_Path name 'RS_Carrier';
procedure _RS_Dial; stdcall; external DLL_Path name 'RS_Dial';
procedure _RS_HangUp; stdcall; external DLL_Path name 'RS_HangUp';
procedure _RS_PickUp; stdcall; external DLL_Path name 'RS_PickUp';
function _RS_Ringing; stdcall; external DLL_Path name 'RS_Ringing';
procedure _RSH_HangUp; stdcall; external DLL_Path name 'RSH_HangUp';
procedure _RSH_PickUp; stdcall; external DLL_Path name 'RSH_PickUp';
function _RS_Connect; stdcall; external DLL_Path name 'RS_Connect';
procedure _RSH_ModemInit; stdcall; external DLL_Path name 'RSH_ModemInit';
procedure _RSH_ModemWaitOK; stdcall; external DLL_Path name 'RSH_ModemWaitOK';
procedure _ComSaveStateEx; stdcall; external DLL_Path name 'ComSaveStateEx';
procedure _ComSetBaudRate; stdcall; external DLL_Path name 'ComSetBaudRate';
procedure _ComEnableTX; stdcall; external DLL_Path name 'ComEnableTX';
function _ComKickOutXOFF; stdcall; external DLL_Path name 'ComKickOutXOFF';
function _ComKickOutXON; stdcall; external DLL_Path name 'ComKickOutXON';
procedure _RS_TXCtlChar; stdcall; external DLL_Path name 'RS_TXCtlChar';
function _RS_GetChkSum; stdcall; external DLL_Path name 'RS_GetChkSum';
function _RS_GetBCC; stdcall; external DLL_Path name 'RS_GetBCC';
procedure _RS_GetCRC_CCITT; stdcall; external DLL_Path name 'RS_GetCRC_CCITT';
function _ComRS485; stdcall; external DLL_Path name 'ComRS485';
function _ComGetVersion; stdcall; external DLL_Path name 'ComGetVersion';
function __GetUserCount; stdcall; external DLL_Path name 'GetUserCount';
function _ComGetIRQVec; stdcall; external DLL_Path name 'ComGetIRQVec';
function _RS_GetTimeZone; stdcall; external DLL_Path name 'RS_GetTimeZone';
procedure _RS_SetTimeZone; stdcall; external DLL_Path name 'RS_SetTimeZone';
procedure _Do_CRC; stdcall; external DLL_Path name 'Do_CRC';
function _RS_FileDateToUnix; stdcall; external DLL_Path name 'RS_FileDateToUnix';
procedure _RS_FileDateToDos; stdcall; external DLL_Path name 'RS_FileDateToDos';
procedure _Do_CRC32; stdcall; external DLL_Path name 'Do_CRC32';
procedure _RS_GetCRC_CCITT32; stdcall; external DLL_Path name 'RS_GetCRC_CCITT32';
procedure _ComSetBufSize; stdcall; external DLL_Path name 'ComSetBufSize';
procedure _ComInithCom; stdcall; external DLL_Path name 'ComInithCom';
function _ComSpyChar; stdcall; external DLL_Path name 'ComSpyChar';
function _ComUngetChar; stdcall; external DLL_Path name 'ComUngetChar';

function _ComSpyCharEx; stdcall; external DLL_Path name 'ComSpyCharEx';
function _ComSwapChar; stdcall; external DLL_Path name 'ComSwapChar';

function _ComGetSetEventProc; stdcall; external DLL_Path name 'ComGetSetEventProc';

procedure _ComSetCOMName; stdcall; external DLL_Path name 'ComSetCOMName';
procedure _ComSetRIQuard; stdcall; external DLL_Path name 'ComSetRIQuard';
procedure _ComSetRIGuard; stdcall; external DLL_Path name 'ComSetRIQuard';
function _ComGetSetEventMask; stdcall; external DLL_Path name 'ComGetSetEventMask';
function _ComHWBufSize; stdcall; external DLL_Path name 'ComHWBufSize';
function _ComHWBufCount; stdcall; external DLL_Path name 'ComHWBufCount';
function _ComGetProcess; stdcall; external DLL_Path name 'ComGetProcess';

function _ComPortPresent; stdcall; external DLL_Path name 'ComPortPresent';
function _ComPortPresentEx; stdcall; external DLL_Path name 'ComPortPresentEx';

procedure _ComEnterCritSec; stdcall; external DLL_Path name 'ComEnterCritSec';
procedure _ComLeaveCritSec; stdcall; external DLL_Path name 'ComLeaveCritSec';
procedure _ComEnterCritPortSec; stdcall; external DLL_Path name 'ComEnterCritPortSec';
procedure _ComLeaveCritPortSec; stdcall; external DLL_Path name 'ComLeaveCritPortSec';

procedure _ComOpen; stdcall; external DLL_Path name 'ComOpen';

function _ComStateDTR; stdcall; external DLL_Path name 'ComStateDTR';
function _ComStateRTS; stdcall; external DLL_Path name 'ComStateRTS';

function _ComStateCTS; stdcall; external DLL_Path name 'ComStateCTS';
function _ComStateDSR; stdcall; external DLL_Path name 'ComStateDSR';
function _ComStateDCD; stdcall; external DLL_Path name 'ComStateDCD';
function _ComStateRI; stdcall; external DLL_Path name 'ComStateRI';

function _ComStateDeltaCTS; stdcall; external DLL_Path name 'ComStateDeltaCTS';
function _ComStateDeltaDSR; stdcall; external DLL_Path name 'ComStateDeltaDSR';
function _ComStateDeltaDCD; stdcall; external DLL_Path name 'ComStateDeltaDCD';
function _ComStateDeltaRI; stdcall; external DLL_Path name 'ComStateDeltaRI';

function _ComGetSetConfigEx; stdcall; external DLL_Path name 'ComGetSetConfigEx';
function _ComGetInfo; stdcall; external DLL_Path name 'ComGetInfo';

function _ComSetDefaultConfig; stdcall; external DLL_Path name 'ComSetDefaultConfig';
function _ComSetDefaultConfigEx; stdcall; external DLL_Path name 'ComSetDefaultConfigEx';

{ ##### }
function _RS_WaitOutLineClear; stdcall; external DLL_Path name 'RS_WaitOutLineClear';
function _ComGetCharTime; stdcall; external DLL_Path name 'ComGetCharTime';
function _ComDelegate; stdcall; external DLL_Path name 'ComDelegate';

function _ComGetThreadId; stdcall; external DLL_Path name 'ComGetThreadId';

function _ComGetVersionEx; stdcall; external DLL_Path name 'ComGetVersionEx';
function _ComGetSetProcs; stdcall; external DLL_Path name 'ComGetSetProcs';
function _ComGetSetEventMaskEx; stdcall; external DLL_Path name 'ComGetSetEventMaskEx';

function _RS_WaitString; stdcall; external DLL_Path name 'RS_WaitString';
function _RS_ConnectEx; stdcall; external DLL_Path name 'RS_ConnectEx';
function _RS_ConnectState; stdcall; external DLL_Path name 'RS_ConnectState';

function _RS_TAPIGetDeviceName; stdcall; external DLL_Path name 'RS_TAPIGetDeviceName';
function _RS_TAPIGetDialString; stdcall; external DLL_Path name 'RS_TAPIGetDialString';
procedure _RS_HangUpEx; stdcall; external DLL_Path name 'RS_HangUpEx';

function _ComGetPriorityValues; stdcall; external DLL_Path name 'ComGetPriorityValues';
function _ComGetSetPriority; stdcall; external DLL_Path name 'ComGetSetPriority';
{ Function _ComSetThreadPriority ; stdcall; EXTERNAL DLL_Path name 'ComSetThreadPriority'; }

function _ComCallDataMonitorProc; stdcall; external DLL_Path name 'ComCallDataMonitorProc';
function _ComCallXDataProc; stdcall; external DLL_Path name 'ComCallXDataProc';
procedure _ComCallResetProc; stdcall; external DLL_Path name 'ComCallResetProc';
function _ComCallUserEventProc; stdcall; external DLL_Path name 'ComCallUserEventProc';

function _ComFilterInit; stdcall; external DLL_Path name 'ComFilterInit';
function _ComFilterFlush; stdcall; external DLL_Path name 'ComFilterFlush';

function _ComFilterRX; stdcall; external DLL_Path name 'ComFilterRX';
function _ComFilterTX; stdcall; external DLL_Path name 'ComFilterTX';

procedure _ComGetDCB; stdcall; external DLL_Path name 'ComGetDCB';
procedure _ComSetDCB; stdcall; external DLL_Path name 'ComSetDCB';

function _RS_GetDataPacket; stdcall; external DLL_Path name 'RS_GetDataPacket';
{ #### }

function TComInt.GetCom: Byte;
begin
    GetCom := _Com;
end;

function TComInt.ComGetVersion: Word;
begin
    ComGetVersion := _ComGetVersion;
end;

constructor TComInt.ComInit(ComPort: Byte);
begin
    _Com := ComPort;
    _ComInit(_Com);
end;

constructor TComInt.ComInithCom(ComPort: Byte; hCom: THANDLE);
begin
    _Com := ComPort;
    _ComInithCom(_Com, hCom);
end;

function TComInt.ComGethCom: THANDLE;
begin
    ComGethCom := _ComGethCom(_Com);
end;

function TComInt.ComGetSetEventProc(EventProc: PUSEREVENTPROC): PUSEREVENTPROC;
begin
    ComGetSetEventProc := _ComGetSetEventProc(_Com, EventProc);
end;

procedure TComInt.ComSetBufSize(RxBufferSize: TDWORD; TxBufferSize: TDWORD);
begin
    _ComSetBufSize(_Com, RxBufferSize, TxBufferSize);
end;

function TComInt.ComGetSysTime: TSYSTIME;
begin
    ComGetSysTime := _ComGetSysTime;
end;

function TComInt.ComGetElapsedTime(Start: TSYSTIME): TSYSTIME;
begin
    ComGetElapsedTime := _ComGetElapsedTime(Start);
end;

function TComInt.ComBufSize(BufDir: TBufDir): TDWORD;
begin
    ComBufSize := _ComBufSize(_Com, TBufDir(BufDir));
end;

function TComInt.ComBufCount(BufDir: TBufDir): TDWORD;
begin
    ComBufCount := _ComBufCount(_Com, TBufDir(BufDir));
end;

{$IFDEF VER100}

function TComInt.ComValid(Com: Byte): Boolean;
{$ELSE}

function TComInt.ComValid(Com: Byte): Boolean;
{$ENDIF}
begin
    ComValid := _ComValid(Com);
end;
{$IFDEF VER100}
{$ELSE}

function TComInt.ComValid: Boolean;
begin
    ComValid := _ComValid(_Com);
end;
{$ENDIF}

function TComInt.ComGetEventEx: Word;
begin
    ComGetEventEx := _ComGetEventEx(_Com);
end;

function TComInt.ComGetEvent: Byte;
begin
    ComGetEvent := _ComGetEvent(_Com);
end;

procedure TComInt.ComSetXONXOFFChars(NewXONChar, NewXOFFChar: Char);
begin
    _ComSetXONXOFFChars(_Com, NewXONChar, NewXOFFChar);
end;

function TComInt.ComGetXONXOFFChars: Word;
begin
    ComGetXONXOFFChars := _ComGetXONXOFFChars(_Com);
end;

procedure TComInt.ComSetXONState(State: Boolean);
begin
    _ComSetXONState(_Com, State);
end;

procedure TComInt.ComSetSCResult(Res: WORD);
begin
    _ComSetSCResult(_Com, Res);
end;

function TComInt.ComResult: WORD;
begin
    ComResult := _ComResult(_Com);
end;

procedure TComInt.ComSaveState;
begin
    _ComSaveState(_Com);
end;

procedure TComInt.ComRestoreState(var SaveState: TSAVESTATE);
begin
    _ComRestoreState(_Com, TSaveState(SaveState));
end;

procedure TComInt.ComXCHGState(var CurState: TSAVESTATE; var NewState: TSAVESTATE);
begin
    _ComXCHGState(_Com, TSaveState(CurState), TSaveState(NewState));
end;

procedure TComInt.ComSaveStateEx(var SaveState: TSAVESTATE);
begin
    _ComSaveStateEx(_Com, TSaveState(SaveState));
end;

procedure TComInt.ComEnableTX;
begin
    _ComEnableTX(_Com);
end;

function TComInt.ComSpyChar(var RCh: Char): Boolean;
begin
    ComSpyChar := _ComSpyChar(_Com, RCh);
end;

function TComInt.ComUngetChar(SCh: Char): Boolean;
begin
    ComUngetChar := _ComUngetChar(_Com, SCh);
end;

function TComInt.ComSpyCharEx(var RCh: Char; BufDir: TBufDir; SpyPos: Word): Boolean;
begin
    ComSpyCharEx := _ComSpyCharEx(_Com, RCh, BufDir, SpyPos);
end;

function TComInt.ComSwapChar(BufDir: TBufDir; SpyPos: Word; NewCh: Char): Char;
begin
    ComSwapChar := _ComSwapChar(_Com, BufDir, SpyPos, NewCh);
end;

function TComInt.ComSWBufCount(BufDir: TBufDir): DWord;
begin
    ComSWBufCount := (_ComBufCount(_Com, BufDir) - _ComHWBufCount(_Com, BufDir));
end;

procedure TComInt.ComSetFlow(Flow: Byte);
begin
    _ComSetFlow(_Com, Flow);
end;

function TComInt.ComGetFlow: Byte;
begin
    ComGetFlow := _ComGetFlow(_Com);
end;

procedure TComInt.ComSetState(NewBaud: LongInt; NewDataBits: Byte; NewStopBits: Byte; NewParity: Char;
    NewFlow: Byte);
begin
    _ComSetState(_Com, NewBaud, NewDataBits, NewStopBits, NewParity, NewFlow);
end;

procedure TComInt.ComSetStateEx(NewBaud: LongInt; NewDataBits: Byte; NewStopBits: Byte; NewParity: Char;
    NewFlow: Byte; SignalState: Byte);
begin
    _ComSetStateEx(_Com, NewBaud, NewDataBits, NewStopBits, NewParity, NewFlow, SignalState);
end;

procedure TComInt.ComSetBaudRate(NewBaud: LongInt);
begin
    _ComSetBaudRate(_Com, NewBaud);
end;

function TComInt.ComGetBaudRate: LongInt;
begin
    ComGetBaudRate := _ComGetBaudRate(_Com);
end;

function TComInt.ComGetMaxBaudRate: LongInt;
begin
    ComGetMaxBaudRate := _ComGetMaxBaudRate(_Com);
end;

procedure TComInt.ComReset;
begin
    _ComReset(_Com);
end;

procedure TComInt.ComResetEx(SignalState: Byte);
begin
    _ComResetEx(_Com, SignalState);
end;

function TComInt.ComPeekByte(Bofs: Byte): Byte;
begin
    ComPeekByte := _ComPeekByte(_Com, Bofs);
end;

function TComInt.ComPeekWord(Bofs: Byte): Word;
begin
    ComPeekWord := _ComPeekWord(_Com, Bofs);
end;

function TComInt.ComPeekLong(Bofs: Byte): TDWORD;
begin
    ComPeekLong := _ComPeekLong(_Com, Bofs);
end;

procedure TComInt.ComPokeByte(Bofs: Byte; Data: Byte);
begin
    _ComPokeByte(_Com, Bofs, Data);
end;

procedure TComInt.ComPokeWord(Bofs: Byte; Data: Word);
begin
    _ComPokeWord(_Com, Bofs, Data);
end;

procedure TComInt.ComPokeLong(Bofs: Byte; Data: TDWORD);
begin
    _ComPokeLong(_Com, Bofs, Data);
end;

function TComInt.ComRead(var RCh: Char): Boolean;
begin
    ComRead := _ComRead(_Com, RCh);
end;

function TComInt.ComWrite(SCh: Char): Boolean;
begin
    ComWrite := _ComWrite(_Com, SCh);
end;

function TComInt.ComReadEx(RCh: PChar; Count: TDWord): TDWord;
begin
    ComReadEx := _ComReadEx(_Com, RCh, Count);
end;

function TComInt.ComWriteEx(SCh: PChar; Count: TDWord): TDWord;
begin
    ComWriteEx := _ComWriteEx(_Com, SCh, Count);
end;

function TComInt.ComKickOut(SCh: Char): Boolean;
begin
    ComKickOut := _ComKickOut(_Com, SCh);
end;

function TComInt.ComKickOutXOFF: Boolean;
begin
    ComKickOutXOFF := _ComKickOutXOFF(_Com);
end;

function TComInt.ComKickOutXON: Boolean;
begin
    ComKickOutXON := _ComKickOutXON(_Com);
end;

procedure TComInt.ComBufClear(BufDir: TBufDir);
begin
    _ComBufClear(_Com, TBufDir(BufDir));
end;

function TComInt.ComBufFull(BufDir: TBufDir): Boolean;
begin
    ComBufFull := _ComBufFull(_Com, TBufDir(BufDir));
end;

function TComInt.ComBufEmpty(BufDir: TBufDir): Boolean;
begin
    ComBufEmpty := _ComBufEmpty(_Com, TBufDir(BufDir));
end;

function TComInt.ComModemState: Byte;
begin
    ComModemState := _ComModemState(_Com);
end;

function TComInt.ComLineState: Byte;
begin
    ComLineState := _ComLineState(_Com);
end;

function TComInt.ComXONState: Boolean;
begin
    ComXONState := _ComXONState(_Com);
end;

procedure TComInt.ComDTROn;
begin
    _ComDTROn(_Com);
end;

procedure TComInt.ComRTSOn;
begin
    _ComRTSOn(_Com);
end;

procedure TComInt.ComDTROff;
begin
    _ComDTROff(_Com);
end;

procedure TComInt.ComRTSOff;
begin
    _ComRTSOff(_Com);
end;

procedure TComInt.ComBreakOn;
begin
    _ComBreakOn(_Com);
end;

procedure TComInt.ComBreakOff;
begin
    _ComBreakOff(_Com);
end;

function TComInt.ComGetMCReg: Byte;
begin
    ComGetMCReg := _ComGetMCReg(_Com);
end;

procedure TComInt.ComPutMCReg(Val: Byte);
begin
    _ComPutMCReg(_Com, Val);
end;

function TComInt.ComFIFOEnabled: Boolean;
begin
    ComFIFOEnabled := _ComFIFOEnabled(_Com);
end;

function TComInt.ComDisableFIFO: Boolean;
begin
    ComDisableFIFO := _ComDisableFIFO(_Com);
end;

function TComInt.ComEnableFIFO(Threshold: Byte): Boolean;
begin
    ComEnableFIFO := _ComEnableFIFO(_Com, Threshold);
end;

function TComInt.ComRS485(Action: Byte): Boolean;
begin
    ComRS485 := _ComRS485(_Com, Action);
end;

function TComInt.ComGetIRQVec(IRQ: Byte): Byte; { IRQ = 0..15 }
begin
    ComGetIRQVec := _ComGetIRQVec(IRQ);
end;

function TComInt.ComGetSetEventMask(dwNewEventMask: TDWORD; cEventChar: Char): TDWORD;
begin
    ComGetSetEventMask := _ComGetSetEventMask(_Com, dwNewEventMask, cEventChar);
end;

procedure TComInt.ComSetCOMName(szNewCOMName: PCHAR);
begin
    _ComSetCOMName(szNewCOMName);
end;

procedure TComInt.ComSetRIQuard(State: Boolean);
begin
    _ComSetRIQuard(_Com, State);
end;

procedure TComInt.ComSetRIGuard(State: Boolean);
begin
    _ComSetRIGuard(_Com, State);
end;

function TComInt.ComHWBufSize(BufDir: TBufDir): TDWORD;
begin
    ComHWBufSize := _ComHWBufSize(_Com, BufDir);
end;

function TComInt.ComHWBufCount(BufDir: TBufDir): TDWORD;
begin
    ComHWBufCount := _ComHWBufCount(_Com, BufDir);
end;

function TComInt.ComGetProcess: THANDLE;
begin
    ComGetProcess := _ComGetProcess(_Com);
end;

function TComInt.ComPortPresent(Com: Byte): Word;
begin
    ComPortPresent := _ComPortPresent(Com);
end;

function TComInt.ComPortPresentEx(dwPortIdx: DWORD; dwComType: DWORD; lpszCOMName: PCHAR;
    pExtData: POINTER): DWORD;
begin
    ComPortPresentEx := _ComPortPresentEx(dwPortIdx, dwComType, lpszCOMName, pExtData);
end;

procedure TComInt.ComEnterCritSec;
begin
    _ComEnterCritSec;
end;

procedure TComInt.ComLeaveCritSec;
begin
    _ComLeaveCritSec;
end;

procedure TComInt.ComEnterCritPortSec;
begin
    _ComEnterCritPortSec(_Com);
end;

procedure TComInt.ComLeaveCritPortSec;
begin
    _ComEnterCritPortSec(_Com);
end;

procedure TComInt.ComOpen(InBuf: POINTER; InLen: Word; OutBuf: POINTER; OutLen: Word);
begin
    _ComOpen(_Com, InBuf, InLen, OutBuf, OutLen);
end;

function TComInt.ComStateDTR: Boolean;
begin
    ComStateDTR := _ComStateDTR(_Com);
end;

function TComInt.ComStateRTS: Boolean;
begin
    ComStateRTS := _ComStateRTS(_Com);
end;

function TComInt.ComStateCTS: Boolean;
begin
    ComStateCTS := _ComStateCTS(_Com);
end;

function TComInt.ComStateDSR: Boolean;
begin
    ComStateDSR := _ComStateDSR(_Com);
end;

function TComInt.ComStateDCD: Boolean;
begin
    ComStateDCD := _ComStateDCD(_Com);
end;

function TComInt.ComStateRI: Boolean;
begin
    ComStateRI := _ComStateRI(_Com);
end;

function TComInt.ComStateDeltaCTS: Boolean;
begin
    ComStateDeltaCTS := _ComStateDeltaCTS(_Com);
end;

function TComInt.ComStateDeltaDSR: Boolean;
begin
    ComStateDeltaDSR := _ComStateDeltaDSR(_Com);
end;

function TComInt.ComStateDeltaDCD: Boolean;
begin
    ComStateDeltaDCD := _ComStateDeltaDCD(_Com);
end;

function TComInt.ComStateDeltaRI: Boolean;
begin
    ComStateDeltaRI := _ComStateDeltaRI(_Com);
end;

function _ComGetSetConfig(Com: Byte; var lpParam): Boolean; stdcall;
begin
    if (0 = _ComGetSetConfigEx(Com, lpParam, nil)) then
        _ComGetSetConfig := FALSE
    else
        _ComGetSetConfig := TRUE;
end;

function TComInt.ComGetSetConfig(var lpParam): Boolean;
begin
    ComGetSetConfig := _ComGetSetConfig(_Com, lpParam);
end;

function TComInt.ComGetSetConfigEx(var lpParam; lpParam2: POINTER): DWORD;
begin
    ComGetSetConfigEx := _ComGetSetConfigEx(_Com, lpParam, lpParam2);
end;

function TComInt.ComGetInfo(var lpInfo): Boolean;
begin
    ComGetInfo := _ComGetInfo(_Com, lpInfo);
end;

function TComInt.ComSetDefaultConfig(SignalState: Byte): Boolean;
begin
    ComSetDefaultConfig := _ComSetDefaultConfig(_Com, SignalState);
end;

function TComInt.ComSetDefaultConfigEx(var lpBaud: DWORD; var lpDataBits: Byte; var lpStopBits: Byte;
    var lpParity: Char; var lpFlow: Byte): Boolean;
begin
    ComSetDefaultConfigEx := _ComSetDefaultConfigEx(_Com, lpBaud, lpDataBits, lpStopBits, lpParity, lpFlow);
end;

{ #### }
function TComInt.ComGetVersionEx(ComType: BYTE): Word;
begin
    ComGetVersionEx := _ComGetVersionEx(ComType);
end;

function TComInt.ComGetSetProcs(var lpProcs: TDATAMONPROCS): BOOLEAN;
begin
    ComGetSetProcs := _ComGetSetProcs(_Com, lpProcs);
end;

function TComInt.ComGetSetEventMaskEx(dwNewEventMask: DWORD; cEventChar: Char; var lpData: TEVENTDATA): DWORD;

begin
    ComGetSetEventMaskEx := _ComGetSetEventMaskEx(_Com, dwNewEventMask, cEventChar, lpData);
end;

function TComInt.ComGetPriorityValues(Priority: Byte; var lpdwThreadPriority: DWORD;
    var lpdwThreadClass: DWORD): Boolean;

begin
    ComGetPriorityValues := _ComGetPriorityValues(Priority, lpdwThreadPriority, lpdwThreadClass);
end;

function TComInt.ComGetSetPriority(hThisThread: THANDLE; var lpPriority: Byte): Boolean;
begin
    ComGetSetPriority := _ComGetSetPriority(_Com, hThisThread, lpPriority);
end;

function TComInt.ComCallDataMonitorProc(lpDataMonitorProc: LPDATAMONITORPROC; Com: BYTE; dwFlags: DWORD;
    lpBuffer: PCHAR; // Var lpBuffer;
    dwCount: DWORD): DWORD;
begin
    ComCallDataMonitorProc := _ComCallDataMonitorProc(lpDataMonitorProc, Com, dwFlags, lpBuffer, dwCount);
end;

function TComInt.ComCallXDataProc(lpXDataProc: LPXDATAPROC; Com: BYTE; lpBuffer: PCHAR; // Var lpBuffer;
    dwCount: DWORD): DWORD;
begin
    ComCallXDataProc := _ComCallXDataProc(lpXDataProc, Com, lpBuffer, dwCount);
end;

procedure TComInt.ComCallResetProc(lpResetProc: LPRESETPROC; Com: Byte);
begin
    _ComCallResetProc(lpResetProc, Com);
end;

function TComInt.ComCallUserEventProc(lpEventProc: PUSEREVENTPROC; Com: BYTE; dwSComEvent: DWORD): DWORD;
begin
    ComCallUserEventProc := _ComCallUserEventProc(lpEventProc, Com, dwSComEvent);
end;

function TComInt.ComFilterInit(dwNewFlags: DWORD; dwAction: DWORD): DWORD;
begin
    ComFilterInit := _ComFilterInit(_Com, dwNewFlags, dwAction);
end;

function TComInt.ComFilterFlush(BufDir: TBufDir): BOOLEAN;
begin
    ComFilterFlush := _ComFilterFlush(_Com, BufDir);
end;

function TComInt.ComFilterRX(RCh: Char): Boolean;
begin
    ComFilterRX := _ComFilterRX(_Com, RCh);
end;

function TComInt.ComFilterTX(var lpBuffer): Word;
begin
    ComFilterTX := _ComFilterTX(_Com, lpBuffer);
end;

procedure TComInt.ComGetDCB(var lpDCB);
begin
    _ComGetDCB(_Com, lpDCB);
end;

procedure TComInt.ComSetDCB(var lpDCB);
begin
    _ComSetDCB(_Com, lpDCB);
end;

function TComInt.ComGetCharTime: DWORD;
begin
    ComGetCharTime := _ComGetCharTime(_Com);
end;

function TComInt.ComDelegate(dwType: DWORD; hWnd: DWORD; pUserEventFunction: POINTER): POINTER;
begin
    ComDelegate := _ComDelegate(_Com, dwType, hWnd, pUserEventFunction);
end;

{ ----------------------------------------------------------- }

constructor TDataLink.ComInit(ComPort: Byte);
begin
    TComInt.ComInit(ComPort);
end;

constructor TDataLink.ComInithCom(ComPort: Byte; hCom: THANDLE);
begin
    TComInt.ComInithCom(ComPort, hCom);
end;

function TDataLink.RS_Global_Yield(hWnd: Word): LongInt;
begin
    RS_Global_Yield := _RS_Global_Yield(hWnd);
end;

procedure TDataLink.RS_SetYieldState(State: Boolean);
begin
    _RS_SetYieldState(State);
end;

function TDataLink.RS_GetYieldState: Boolean;
begin
    RS_GetYieldState := _RS_GetYieldState;
end;

procedure TDataLink.RS_TXOnLine(SCh: Char);
begin
    _RS_TXOnLine(_Com, SCh);
end;

procedure TDataLink.RS_RXOnLine(var RCh: Char);
begin
    _RS_RXOnLine(_Com, RCh);
end;

function TDataLink.RS_TXInTime(SCh: Char; WaitTime: TSYSTIME): Boolean;
begin
    RS_TXInTime := _RS_TXInTime(_Com, SCh, WaitTime);
end;

function TDataLink.RS_RXInTime(var RCh: Char; WaitTime: TSYSTIME): Boolean;
begin
    RS_RXInTime := _RS_RXInTime(_Com, RCh, WaitTime);
end;

function TDataLink.RS_ErrorState: Byte;
begin
    RS_ErrorState := _RS_ErrorState(_Com);
end;

function TDataLink.RS_ModemState: Byte;
begin
    RS_ModemState := _RS_ModemState(_Com);
end;

function TDataLink.RS_LineState: Byte;
begin
    RS_LineState := _RS_LineState(_Com);
end;

procedure TDataLink.RS_Break;
begin
    _RS_Break(_Com);
end;

function TDataLink.RS_XOFFLimit: Word;
begin
    RS_XOFFLimit := _RS_XOFFLimit(_Com);
end;

function TDataLink.RS_XONLimit: Word;
begin
    RS_XONLimit := _RS_XONLimit(_Com);
end;

procedure TDataLink.RS_DisableUserBreak;
begin
    _RS_DisableUserBreak;
end;

procedure TDataLink.RS_EnableUserBreak;
begin
    _RS_EnableUserBreak;
end;

function TDataLink.RS_UserBreak: Boolean;
begin
    RS_UserBreak := _RS_UserBreak;
end;

procedure TDataLink.RS_UserBreakOption(Option: Integer);
begin
    _RS_UserBreakOption(Option);
end;

procedure TDataLink.RS_UserBreakPressed;
begin
    _RS_UserBreakPressed;
end;

procedure TDataLink.RS_Delay(WaitTime: TSYSTIME);
begin
    _RS_Delay(WaitTime);
end;

procedure TDataLink.RS_SetXONLimit(XONLimit: Word);
begin
    _RS_SetXONLimit(_Com, XONLimit);
end;

procedure TDataLink.RS_SetXOFFLimit(XOFFLimit: Word);
begin
    _RS_SetXOFFLimit(_Com, XOFFLimit);
end;

function TDataLink.RS_GetXONState: Boolean;
begin
    RS_GetXONState := _RS_GetXONState(_Com);
end;

procedure TDataLink.RS_SetXONState(State: Boolean);
begin
    _RS_SetXONState(_Com, State);
end;

procedure TDataLink.RS_TXCtlChar(CtlChar: Char; minWait, maxWait: TSYSTIME);
begin
    _RS_TXCtlChar(_Com, CtlChar, minWait, maxWait);
end;

{ ----------------------------------------------------------- }

constructor TTransport.ComInit(ComPort: Byte);
begin
    TComInt.ComInit(ComPort);
end;

constructor TTransport.ComInithCom(ComPort: Byte; hCom: THANDLE);
begin
    TComInt.ComInithCom(ComPort, hCom);
end;

procedure TTransport.RS_TXPacket(var Packet; Len: Word; var Result: Word);
begin
    _RS_TXPacket(_Com, Packet, Len, Result);
end;

procedure TTransport.RS_RXPacket(var Packet; Len: Word; var Result: Word);
begin
    _RS_RXPacket(_Com, Packet, Len, Result);
end;

procedure TTransport.RS_TXQPacket(var Packet; Len: Word; var Result: Word);
begin
    _RS_TXQPacket(_Com, Packet, Len, Result);
end;

procedure TTransport.RS_RXQPacket(var Packet; Len: Word; var Result: Word);
begin
    _RS_RXQPacket(_Com, Packet, Len, Result);
end;

procedure TTransport.RS_TXPInTime(var Packet; Len: Word; OneChDelay: TSYSTIME; var Result: Word);
begin
    _RS_TXPInTime(_Com, Packet, Len, OneChDelay, Result);
end;

procedure TTransport.RS_RXPInTime(var Packet; Len: Word; OneChDelay: TSYSTIME; var Result: Word);
begin
    _RS_RXPInTime(_Com, Packet, Len, OneChDelay, Result);
end;

function TTransport.RS_GetChkSum(var Packet; Len: Word): Byte;
begin
    RS_GetChkSum := _RS_GetChkSum(Packet, Len);
end;

function TTransport.RS_GetBCC(var Packet; Len: Word): Byte;
begin
    RS_GetBCC := _RS_GetBCC(Packet, Len);
end;

procedure TTransport.Do_CRC(var CRC: SHORT; Ch: Char);
begin
    _Do_CRC(CRC, Ch);
end;

procedure TTransport.RS_GetCRC_CCITT(var Packet; Len: Word; var CRC: SHORT);
begin
    _RS_GetCRC_CCITT(Packet, Len, CRC);
end;

procedure TTransport.RS_GetCRC_CCITT32(var Packet; Len: Word; var CRC: LongInt);
begin
    _RS_GetCRC_CCITT32(Packet, Len, CRC);
end;

procedure TTransport.Do_CRC32(var CRC: LongInt; Ch: Char);
begin
    _Do_CRC32(CRC, Ch);
end;

function TTransport.RS_GetTimeZone: LongInt;
begin
    RS_GetTimeZone := _RS_GetTimeZone;
end;

procedure TTransport.RS_SetTimeZone(TimeZone: LongInt);
begin
    _RS_SetTimeZone(TimeZone);
end;

procedure TTransport.RS_FileDateToDos(DateInSec: LongInt; var Year: Word; var Month: Word; var Day: Word;
    var Hour: Word; var Min: Word; var Sec: Word);
begin
    _RS_FileDateToDos(DateInSec, Year, Month, Day, Hour, Min, Sec);
end;

function TTransport.RS_FileDateToUnix(Year, Month, Day, Hour, Min, Sec: Word): LongInt;
begin
    RS_FileDateToUnix := _RS_FileDateToUnix(Year, Month, Day, Hour, Min, Sec);
end;

{
  Function TTransport.RS_WaitString(
  lpRetBuffer:PCHAR;
  lpdwRetLen:LPDWord;
  lpDataToWait:PCHAR;
  dwDataLen:TDWord;
  pTimeOut:PSYSTIME;
  dwFlags:DWORD):DWORD;
}
function TTransport.RS_WaitString(lpRetBuffer: PCHAR; var lpdwRetLen: TDWord; lpDataToWait: PCHAR;
    dwDataLen: TDWord; pTimeOut: PSYSTIME; dwFlags: DWORD): DWORD;
begin
    RS_WaitString := _RS_WaitString(_Com, lpRetBuffer, lpdwRetLen, lpDataToWait, dwDataLen, pTimeOut,
        dwFlags);
end;

function TTransport.RS_GetDataPacket(lpRetBuffer: PCHAR; var lpdwRetBufLen: TDWord; lpStartString: PCHAR;
    dwStartLen: TDWord; lpEndString: PCHAR; dwEndLen: TDWord; dwMaxChars: TDWord; pTimeOut: PSYSTIME): DWORD;
begin
    RS_GetDataPacket := _RS_GetDataPacket(_Com, lpRetBuffer, lpdwRetBufLen, lpStartString, dwStartLen,
        lpEndString, dwEndLen, dwMaxChars, pTimeOut);
end;

function TTransport.RS_WaitOutLineClear(Timeout: TSYSTIME; dwOptions: DWORD): DWORD;
begin
    RS_WaitOutLineClear := _RS_WaitOutLineClear(_Com, Timeout, dwOptions);
end;

{ ----------------------------------------------------------- }

constructor TModem.ComInit(ComPort: Byte);
begin
    TComInt.ComInit(ComPort);
end;

constructor TModem.ComInithCom(ComPort: Byte; hCom: THANDLE);
begin
    TComInt.ComInithCom(ComPort, hCom);
end;

function TModem.RS_Carrier: Boolean;
begin
    RS_Carrier := _RS_Carrier(_Com);
end;

function TModem.RS_Ringing: Boolean;
begin
    RS_Ringing := _RS_Ringing(_Com);
end;

procedure TModem.RS_HangUp;
begin
    _RS_HangUp(_Com);
end;

procedure TModem.RS_PickUp;
begin
    _RS_PickUp(_Com);
end;

procedure TModem.RS_Dial(Number: PCHAR);
begin
    _RS_Dial(_Com, Number);
end;

procedure TModem.RSH_HangUp;
begin
    _RSH_HangUp(_Com);
end;

procedure TModem.RSH_PickUp;
begin
    _RSH_PickUp(_Com);
end;

procedure TModem.RSH_ModemInit(InitString: PCHAR);
begin
    _RSH_ModemInit(_Com, InitString);
end;

procedure TModem.RSH_ModemWaitOK;
begin
    _RSH_ModemWaitOK(_Com);
end;

function TModem.RS_Connect(ConnectDelay: TSYSTIME; CheckModemResult: Boolean; ModemResult: PCHAR;
    InfoProc: TInfoProc): Boolean;
begin
    RS_Connect := _RS_Connect(_Com, ConnectDelay, CheckModemResult, ModemResult, TInfoProc(InfoProc));
end;

{ #### }
function TModem.RS_ConnectEx(ConnectDelay: TSYSTIME; bCheckModemResult: Boolean; lpszModemResult: PCHAR;
    lpfInfoProc: TInfoProc; lpszInitString: PCHAR; lpszDestAddress: PCHAR; bRunThreaded: Boolean): DWORD;
begin
    RS_ConnectEx := _RS_ConnectEx(_Com, ConnectDelay, bCheckModemResult, lpszModemResult, lpfInfoProc,
        lpszInitString, lpszDestAddress, bRunThreaded);
end;

function TModem.RS_ConnectState: DWORD;
begin
    RS_ConnectState := _RS_ConnectState(_Com);
end;

function TModem.RS_TAPIGetDeviceName(dwDeviceIndex: DWORD; lpszDevName: PCHAR; var dwBufferLen: DWORD): DWORD;
begin
    RS_TAPIGetDeviceName := _RS_TAPIGetDeviceName(_Com, dwDeviceIndex, lpszDevName, dwBufferLen);
end;

function TModem.RS_TAPIGetDialString(dwDeviceIndex: DWORD; lpszDialBufferString: PCHAR;
    var lpdwBufferLen: DWORD): DWORD;
begin
    RS_TAPIGetDialString := _RS_TAPIGetDialString(_Com, dwDeviceIndex, lpszDialBufferString, lpdwBufferLen);
end;

procedure TModem.RS_HangUpEx;
begin
    _RS_HangUpEx(_Com);
end;

function CIP_TO_MASK(BIT_TO_SET: Byte): DWORD;
begin
    CIP_TO_MASK := (1 shl BIT_TO_SET);
end;

{ Some helpers to analyse the result of RS_GetDataPacket }

{ SCOM_DP_FAILED   results true if function failed }
function SCOM_DP_FAILED(dwRes: DWORD): Boolean;
begin
    if ((LoWord(dwRes) = 0) or (HiWord(dwRes) = 0)) then
        SCOM_DP_FAILED := TRUE
    else
        SCOM_DP_FAILED := FALSE;
end;

{ SCOM_DP_OK   results true if function succeeded }
function SCOM_DP_OK(dwRes: DWORD): Boolean;
begin
    SCOM_DP_OK := not SCOM_DP_FAILED(dwRes);
end;

{ If one start and one end string used the result will be Hex 00010001 (if succeed)
  If a multi sting buffer were used in start and/or end string, which one failed
  will be returned by one of the following macros }
function SCOM_DP_START_WHICH(dwRes: DWORD): DWORD;
begin
    SCOM_DP_START_WHICH := LoWord(dwRes);
end;

function SCOM_DP_END_WHICH(dwRes: DWORD): DWORD;
begin
    SCOM_DP_END_WHICH := HiWord(dwRes);
end;

function IsSet(dwBits: DWORD; dwFlags: DWORD): Boolean;
begin
    if (dwBits and dwFlags) = dwFlags then
        IsSet := TRUE
    else
        IsSet := FALSE;
end;

function NotSet(dwBits: DWORD; dwFlags: DWORD): Boolean;
begin
    NotSet := not IsSet(dwBits, dwFlags);
end;

function SetBits(var dwBits: DWORD; dwFlags: DWORD): DWORD;
begin
    dwBits := dwBits or dwFlags;
    SetBits := dwBits;
end;

function ClearBits(var dwBits: DWORD; dwFlags: DWORD): DWORD;
begin
    dwBits := dwBits and (not dwFlags);
    ClearBits := dwBits;
end;

{ No Init }


end.
