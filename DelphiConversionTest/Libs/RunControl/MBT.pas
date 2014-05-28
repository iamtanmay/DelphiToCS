unit MBT;
{ ----------------------------------------------------------------------------------------------------------------------
  Copyright © 2008 Zinsser Analytic GmbH - All rights reserved.
  Author       : Thomas Schubert (ts)
  Description  : Communication for WagoSPS
  ----------------------------------------------------------------------------------------------------------------------
  Revision History:
  date     op  method                            track-no improvement/change
  -------- --  -------------------------------   -------- --------------------------------------------------------------
  26.11.08 wl  LoadMBTLibrary                    TN4300   MBT.dll wird dynamisch geladen
  19.11.09 ts  LoadMBTLibrary                    TN4890   function instead of procedure, false if error occurs
  02.03.09 ts  LoadMBTLibrary                    TN4999   wenn MBT.dll schon geladen ist, dann muss Rückgabewert true sein
  ---------------------------------------------------------------------------------------------------------------------- }

{ -----------------------------------------------------------------------------
  ' WAGO Kontakttechnik GmbH       |                                           |
  ' Hansastr. 27                   |  Technical Support                        |
  ' D-32423 Minden                 |                                           |
  ' Tel.: +49(0)571 / 887 - 0      |  Tel.: +49(0)571 / 887 - 555              |
  ' Fax.: +49(0)571 / 887 - 169    |  Fax.: +49(0)571 / 887 - 8555             |
  ' Mail: info@wago.com            |  Mail: support@wago.com                   |
  ' www : http://www.wago.com      |                                           |
  '-----------------------------------------------------------------------------
  '              Copyright (C) WAGO 2001 - All Rights Reserved                 |
  '-----------------------------------------------------------------------------
  '  Filename    :
  '  Version     : 1.00
  '  Date        : 12-Maerz-2002
  '-----------------------------------------------------------------------------
  '  Description :
  '
  '-----------------------------------------------------------------------------
  '  Required    :
  '-----------------------------------------------------------------------------
  ' |Date    |Who |Ver  |Changes
  '-----------------------------------------------------------------------------
  '  12.03.02 CM   1.00  Init
  '----------------------------------------------------------------------------- }


interface


uses
    Windows;

const

    { -----------------------------------------------------------------------------
      Values are 32 bit values layed out as follows:

      3 3 2 2 2 2 2 2 2 2 2 2 1 1 1 1 1 1 1 1 1 1
      1 0 9 8 7 6 5 4 3 2 1 0 9 8 7 6 5 4 3 2 1 0 9 8 7 6 5 4 3 2 1 0
      +---+-+-+-----------------------+-------------------------------+
      |Sev|C|R|     Facility          |               Code            |
      +---+-+-+-----------------------+-------------------------------+

      where

      Sev - is the severity code

      00 - Success
      01 - Informational
      10 - Warning
      11 - Error

      C - is the Customer code flag

      R - is a reserved bit

      Facility - is the facility code

      Code - is the facility's status code

      ERROR 0xEF01....

      S_OK = 0
      ----------------------------------------------------------------------------- }

    MBT_ERROR_PREFIX = $EF010000;
    MBT_THREAD_CREATION_ERROR = MBT_ERROR_PREFIX + $00;
    MBT_EXIT_TIMEOUT_ERROR = MBT_ERROR_PREFIX + $01;
    MBT_UNKNOWN_THREAD_EXIT_ERROR = MBT_ERROR_PREFIX + $02;
    MBT_UNAVAILABLE_CLOCK_ERROR = MBT_ERROR_PREFIX + $03;
    MBT_NO_ENTRY_ADDABLE_ERROR = MBT_ERROR_PREFIX + $04;
    MBT_NO_JOB_ADDABLE_ERROR = MBT_ERROR_PREFIX + $05;
    MBT_HANDLE_INVALID_ERROR = MBT_ERROR_PREFIX + $06;
    MBT_CLOSE_FLAG_SET_ERROR = MBT_ERROR_PREFIX + $07;
    MBT_SOCKET_TIMEOUT_ERROR = MBT_ERROR_PREFIX + $08;
    MBT_WRONG_RESPONSE_FC_ERROR = MBT_ERROR_PREFIX + $09;
    MBT_RESPONSE_FALSE_LENGTH_ERROR = MBT_ERROR_PREFIX + $0A;
    MBT_EXIT_ERROR = MBT_ERROR_PREFIX + $0B;

    MBT_EXCEPTION_PREFIX = $EF01F000;
    MBT_ILLEGAL_FUNCTION = MBT_EXCEPTION_PREFIX + $01;
    MBT_ILLEGAL_DATA_ADDRESS = MBT_EXCEPTION_PREFIX + $02;
    MBT_ILLEGAL_DATA_VALUE = MBT_EXCEPTION_PREFIX + $03;
    MBT_ILLEGAL_RESPONSE_LENGTH = MBT_EXCEPTION_PREFIX + $04;
    MBT_ACKNOWLEDGE = MBT_EXCEPTION_PREFIX + $05;
    MBT_SLAVE_DEVICE_BUSY = MBT_EXCEPTION_PREFIX + $06;
    MBT_NEGATIVE_ACKNOWLEDGE = MBT_EXCEPTION_PREFIX + $07;
    MBT_MEMORY_PARITY_ERROR = MBT_EXCEPTION_PREFIX + $08;
    MBT_GATEWAY_PATH_UNAVAILABLE = MBT_EXCEPTION_PREFIX + $0A;
    MBT_GATEWAY_TARGET_DEVICE_FAILED_TO_RESPOND = MBT_EXCEPTION_PREFIX + $0B;
    { ----------------------------------------------------------------------------- }

    // Tablebytes
    MODBUSTCP_TABLE_OUTPUT_REGISTER = 4;
    MODBUSTCP_TABLE_INPUT_REGISTER = 3;
    MODBUSTCP_TABLE_OUTPUT_COIL = 0;
    MODBUSTCP_TABLE_INPUT_COIL = 1;
    MODBUSTCP_TABLE_EXCEPTION_STATUS = 7;

    { ------------------------------------------------------------------------------
      Type Definitions
      ------------------------------------------------------------------------------- }
type
    LPBYTE = array of Byte;

    MBTReadCompleted = procedure(hSocket: THandle; // socket handle
        callbackContext: Longword; // callback context, handed over at the call
        errorCode: LongInt; // result of the read operation
        tableType: Byte; // type of MODBUS/TCP tables(MODBUSTCP_TABLE_xxx)
        dataStartAddress: Word; // start address of the registers or coils to be read
        numRead: Word; // number of the registers or coils to be read
        numBytes: Word; // number of the bytes to be read
        pReadBuffer: LPBYTE // memory section with the data to be written
        );

    MBTWriteCompleted = procedure(hSocket: THandle; // socket handle
        callbackContext: Longword; // callback context, handed over at the call
        errorCode: LongInt; // result of the write operation
        tableType: Byte; // type of MODBUS/TCP tables(MODBUSTCP_TABLE_xxx)
        // output registers or output coils
        dataStartAddress: Word; // start address of the registers or coils to be written
        numWrite: Word; // number of the registers or coils to be written
        pWriteBuffer: LPBYTE // memory section with the data to be written
        );

var
    { -------------------------------------------------------------------------------
      initializes the MODBUS/TCP library
      ------------------------------------------------------------------------------- }
    MBTInit: function: LongInt; stdcall;

    { -------------------------------------------------------------------------------
      terminates the MODBUS/TCP library
      ------------------------------------------------------------------------------- }
    MBTExit: function: LongInt; stdcall;

    { -------------------------------------------------------------------------------
      creates a socket and connects it to the given device port
      ------------------------------------------------------------------------------- }
    MBTConnect: function(szHostAddress: PAnsiChar; // TCP/IP address of device
        port: Word; // TCP port in device for communication
        useTCPorUDP: Boolean; // TRUE - TCP; FALSE - UDP
        requestTimeout: LongWord; // maximal time for managing an I/O request (ms)
        var hSocket: THandle // handle of the connected socket
        ): LongInt; stdcall;

    { -------------------------------------------------------------------------------
      aborts the connection to a device and releases the socket
      ------------------------------------------------------------------------------- }
    MBTDisconnect: function(hSocket: THandle // handle of the connected socket
        ): LongInt; stdcall;

    { -------------------------------------------------------------------------------
      read from a connected socket
      ------------------------------------------------------------------------------- }
    MBTReadRegisters: function(hSocket: THandle; // handle of the connected socket
        tableType: Byte; // Modbus/TCP Tabellen Typ (MODBUSTCP_TABLE_xxx)
        // (here: input register or output register
        dataStartAddress: Word; // start address of the registers to be read
        numWords: Word; // number of the registers to be read
        pReadBuffer: LPBYTE; // memory section from which the data are read
        // (NULL at asynchronous call)
        fpReadCompletedCallback: MBTReadCompleted; // C-callback function, called after termination
        // of asynchronous reading (NULL at synchronous
        // call)
        callbackContext: Longword // context, handed over to the asynchronous
        // (callback function (0 at synchronous call)
        ): LongInt; stdcall;

    { -------------------------------------------------------------------------------
      write to a connected socket
      ------------------------------------------------------------------------------- }
    MBTWriteRegisters: function(hSocket: THandle; // handle of the connected socket
        dataStartAddress: Word; // start address of the registers to be written
        numWords: Word; // number of the registers to be written
        pWriteBuffer: LPBYTE; // memory section from which the data are written
        // (NULL at asynchronous call)
        fpWriteCompletedCallback: MBTWriteCompleted; // C-callback function, called after termination
        // of asynchronous writing (NULL at synchronous
        // call)
        callbackContext: Longword // context, handed over to the asynchronous
        // (callback function (0 at synchronous call)
        ): LongInt; stdcall;

    { -------------------------------------------------------------------------------
      read from a connected socket
      ------------------------------------------------------------------------------- }
    MBTReadCoils: function(hSocket: THandle; // handle of the connected socket
        tableType: Byte; // Modbus/TCP Tabellen Typ (MODBUSTCP_TABLE_xxx)
        // (here: input coil or output coil
        dataStartAddress: Word; // start address of the coils to be read
        numBits: Word; // number of the coils to be read
        pReadBuffer: LPBYTE; // memory section from which the data are read
        // (NULL at asynchronous call)
        fpReadCompletedCallback: MBTReadCompleted; // C-callback function, called after termination
        // of asynchronous reading (NULL at synchronous
        // call)
        callbackContext: Longword // context, handed over to the asynchronous
        // (callback function (0 at synchronous call)
        ): LongInt; stdcall;

    { -------------------------------------------------------------------------------
      write to a connected socket
      ------------------------------------------------------------------------------- }
    MBTWriteCoils: function(hSocket: THandle; // handle of the connected socket
        dataStartAddress: Word; // start address of the coils to be written
        numBits: Word; // number of the coils to be written
        pWriteBuffer: LPBYTE; // memory section from which the data are written
        // (NULL at asynchronous call)
        fpWriteCompletedCallback: MBTWriteCompleted; // C-callback function, called after termination
        // of asynchronous writing (NULL at synchronous
        // call)
        callbackContext: Longword // context, handed over to the asynchronous
        // (callback function (0 at synchronous call)
        ): LongInt; stdcall;

    { -------------------------------------------------------------------------------
      read from a connected socket
      ------------------------------------------------------------------------------- }
    MBTReadExceptionStatus: function(hSocket: THandle; // handle of the connected socket
        pExceptionStatus: LPBYTE; // memory section from which the data are read
        // (NULL at asynchronous call)
        fpReadCompletedCallback: MBTReadCompleted; // C-callback function, called after termination
        // of asynchronous reading (NULL at synchronous
        // call)
        callbackContext: Longword // context, handed over to the asynchronous
        // (callback function (0 at synchronous call)
        ): LongInt; stdcall;

    { -------------------------------------------------------------------------------
      swaps the bytes in a word
      ------------------------------------------------------------------------------- }
    MBTSwapWord: function(const wData: Word): Word; stdcall;

    { -------------------------------------------------------------------------------
      swaps the words in a double word
      ------------------------------------------------------------------------------- }
    MBTSwapDWord: function(const dwData: Longword): Longword; stdcall;

function LoadMBTLibrary(): boolean;
procedure UnloadMBTLibrary();


implementation


uses
    SysUtils;

var
    gDllHandle: Integer = 0;

function LoadMBTLibrary(): boolean;
begin
    if (gDllHandle <> 0) then
    begin
        result := true;
        EXIT;
    end;
    gDllHandle := LoadLibrary('MBT.DLL');
    if (gDllHandle = 0) then
        raise Exception.Create('Could not load MBT.dll');

    @MBTInit := GetProcAddress(gDllHandle, 'MBTInit');
    @MBTExit := GetProcAddress(gDllHandle, 'MBTExit');
    @MBTConnect := GetProcAddress(gDllHandle, 'MBTConnect');
    @MBTDisconnect := GetProcAddress(gDllHandle, 'MBTDisconnect');
    @MBTReadRegisters := GetProcAddress(gDllHandle, 'MBTReadRegisters');
    @MBTWriteRegisters := GetProcAddress(gDllHandle, 'MBTWriteRegisters');
    @MBTReadCoils := GetProcAddress(gDllHandle, 'MBTReadCoils');
    @MBTWriteCoils := GetProcAddress(gDllHandle, 'MBTWriteCoils');
    @MBTReadExceptionStatus := GetProcAddress(gDllHandle, 'MBTReadExceptionStatus');
    @MBTSwapWord := GetProcAddress(gDllHandle, 'MBTSwapWord');
    @MBTSwapDWord := GetProcAddress(gDllHandle, 'MBTSwapDWord');

    if (@MBTInit = nil) or (@MBTExit = nil) or (@MBTConnect = nil) or (@MBTDisconnect = nil) or
        (@MBTReadRegisters = nil) or (@MBTWriteRegisters = nil) or (@MBTReadCoils = nil) or
        (@MBTWriteCoils = nil) or (@MBTReadExceptionStatus = nil) or (@MBTSwapWord = nil) or
        (@MBTSwapDWord = nil) then
        raise Exception.Create('Wrong Version of MBT.dll');

    result := true;
end;

procedure UnloadMBTLibrary();
begin
    if (gDllHandle = 0) then
        EXIT;

    // evtl. noch irgendwas machen
end;


end.
