{ --------------------------------------------------------------------------------------------------
  Copyright © 2003 Zinsser Analytic GmbH - All rights reserved.
  Author       : Wolfgang Lyncke (wl)
  Description  : Interface that provides application functions to all dynamic loaded DLL's
  --------------------------------------------------------------------------------------------------
  Revision History:
  date     op  Version  track-no improvement/change
  -------- --  -------  -------- -----------------------------------------------
  28.03.03 wl       1   TN1293.7 initial version
  16.04.03 tbh      2   TN1468   neue Funktionen ReadStringDLL / ReadIntDLL
  18.07.03 tbh      3   TN1515   neue Funktionen GetCurrentScriptName/GetCurrentMethodName/GetCurrentRunName
  29.07.03 wl       4   TN1515   komplett von PChar auf WideString umgestellt
  06.11.03 pk       5   TN1649, TN1649.1 DLLInterface extended for GlobalErr and SimMode
  07.06.04 pk       6   TN1964.0 All functions are use c calling convention
  07.06.04 pk       6   TN1964.0 Return types of functions changed from Widestring to TInterfaceStr
  22.06.04 pk       6   TN1964.0 InterfaceStrToStr
  20.03.06 wl       6   TN2967.1  Namen für GetDefinedValue sind hier als const definiert
  22.03.06 wl       6   TN2989.1  neu: 'PAINTRACKPOS'
  27.04.07 wl       6   TN3669    neu: const für ItemConfirmDelete,Add,Edit
  07.05.07 wl       6   TN3669    neu: USERISADMIN
  02.03.11 wl       7   TN5491    komplett überarbeitet, neu: IDataProvider (für BDE oder TurboDB)
  12.04.11 wl       7   TN5491.1  neu: ISerialPort
  13.04.11 wl       7   TN5491   neues Konzept: statt IDataProvider wird ein Pointer übergeben
  28.06.11 wl       8   TN5613   passend zu Version 8 (damit man nicht durcheinanderkommt)
  28.06.11 wl       8   TN5613   neu: 10 Dummy-Methoden
  29.06.11 wl       8   TN5613   DPExecSQL: result = RowsAffected
  -------------------------------------------------------------------------------------------------- }

unit DLLInterface;


interface


uses
    SysUtils;

const
    INT_DLLINTERFACE_VERSION = 8; // muß bei jeder Änderung hochgezählt werden

    STR_DLLINT_DEFVALUE_IDENT_GETRACKPOS = 'GETRACKPOS';
    STR_DLLINT_DEFVALUE_IDENT_MODALDIALOGOPENED = 'MODALDIALOGOPENED';
    STR_DLLINT_DEFVALUE_IDENT_MODALDIALOGCLOSED = 'MODALDIALOGCLOSED';
    STR_DLLINT_DEFVALUE_IDENT_GETCURRENTVOLUME = 'GETCURRENTVOLUME';
    STR_DLLINT_DEFVALUE_IDENT_PAINTRACKPOS = 'PAINTRACKPOS';

type
    // TInterfaceStr = PWideString;

    PDllDataProvider = Pointer;

    TDLLInterface = packed record
        SetGlobalErr: procedure(aValue: integer; aReason: WideString); cdecl;
        GetGlobalErr: function: integer; cdecl;
        IsGlobalErr: function: boolean; cdecl;
        IsSimulationMode: function: boolean; cdecl;
        ApplicationSleep: procedure(aMSec: cardinal); cdecl;
        Log: procedure(aLogText: WideString; aLogType: Longint; aDisplay: boolean); cdecl;
        ReadStringApp: function(aSection, aIdent: WideString): PWideString; cdecl;
        ReadStringRobot: function(aSection, aIdent: WideString): PWideString; cdecl;
        ReadStringDLL: function(aSection, aIdent: WideString): PWideString; cdecl;
        ReadStringOther: function(aArea, aSection, aIdent, aDefault: WideString): PWideString; cdecl;
        ReadIntApp: function(aSection, aIdent: WideString): Longint; cdecl;
        ReadIntRobot: function(aSection, aIdent: WideString): Longint; cdecl;
        ReadIntDLL: function(aSection, aIdent: WideString): Longint; cdecl;
        ReadIntOther: function(aArea, aSection, aIdent: WideString; aDefault: Longint): Longint; cdecl;
        GetAliasName: function: PWideString; cdecl;
        GetPathFromAlias: function(aAlias: WideString): PWideString; cdecl;
        GetDataPath: function: PWideString; cdecl;
        GetDefinedValue: function(aParameter: WideString): PWideString; cdecl;
        GetCurrentMethodName: function: PWideString; cdecl;
        GetCurrentRunName: function: PWideString; cdecl;
        CallOtherDllFunction: function(aDllName, aDllFunction, aDllParams: WideString): PWideString; cdecl;
        UserIsAdmin: function: boolean; cdecl;
        UserConfirmAdd: function(const aItemTypeName, aItemName, aCopiedFrom: WideString): boolean; cdecl;
        UserConfirmEdit: function(const aItemTypeName, aItemName: WideString): boolean; cdecl;
        UserConfirmDelete: function(const aItemTypeName, aItemName: WideString): boolean; cdecl;
        LocalIniReadString: function(aSection, aIdent, aDefault: WideString): PWideString; cdecl;
        LocalIniWriteString: procedure(aSection, aIdent, aValue: WideString); cdecl;

        // DataProvider methoden
        CreateMainDataProvider: function(): PDllDataProvider; cdecl;
        CreateDataProvider: function(aAlias: WideString): PDllDataProvider; cdecl;
        DestroyDataProvider: procedure(aDP: PDllDataProvider); cdecl;
        DPGetActive: function(aDP: PDllDataProvider): boolean; cdecl;
        DPGetBof: function(aDP: PDllDataProvider): boolean; cdecl;
        DPGetEof: function(aDP: PDllDataProvider): boolean; cdecl;
        DPGetFieldCount: function(aDP: PDllDataProvider): Integer; cdecl;
        DPGetRecordCount: function(aDP: PDllDataProvider): Integer; cdecl;
        DPGetRecNo: function(aDP: PDllDataProvider): integer; cdecl;
        DPSelectAndOpen: procedure(aDP: PDllDataProvider; const aSQL: WideString; aReadOnly: boolean); cdecl;
        DPExecSQL: function(aDP: PDllDataProvider; const aSQL: WideString): integer; cdecl;
        DPAppend: procedure(aDP: PDllDataProvider); cdecl;
        DPClose: procedure(aDP: PDllDataProvider); cdecl;
        DPDelete: procedure(aDP: PDllDataProvider); cdecl;
        DPEdit: procedure(aDP: PDllDataProvider); cdecl;
        DPFirst: procedure(aDP: PDllDataProvider); cdecl;
        DPInsert: procedure(aDP: PDllDataProvider); cdecl;
        DPIsEmpty: function(aDP: PDllDataProvider): Boolean; cdecl;
        DPLast: procedure(aDP: PDllDataProvider); cdecl;
        DPNext: procedure(aDP: PDllDataProvider); cdecl;
        DPPost: procedure(aDP: PDllDataProvider); cdecl;
        DPPrior: procedure(aDP: PDllDataProvider); cdecl;
        DPMoveBy: procedure(aDP: PDllDataProvider; aOffset: integer); cdecl;
        // DPGetFieldNames: function(aDP: PDllDataProvider): TArray<WideString>; cdecl;
        DPRefresh: procedure(aDP: PDllDataProvider); cdecl;
        DPGetFieldByNameAsString: function(aDP: PDllDataProvider; const aFieldName: WideString)
            : PWideString; cdecl;
        DPSetFieldByNameAsString: procedure(aDP: PDllDataProvider; const aFieldName: WideString;
            const aValue: WideString); cdecl;
        DPGetFieldByNameAsInt: function(aDP: PDllDataProvider; const aFieldName: WideString): integer; cdecl;
        DPSetFieldByNameAsInt: procedure(aDP: PDllDataProvider; const aFieldName: WideString;
            aValue: integer); cdecl;
        DPGetFieldByNameAsFloat: function(aDP: PDllDataProvider; const aFieldName: WideString): double; cdecl;
        DPSetFieldByNameAsFloat: procedure(aDP: PDllDataProvider; const aFieldName: WideString;
            aValue: double); cdecl;
        DPGetFieldByNameAsBoolean: function(aDP: PDllDataProvider; const aFieldName: WideString)
            : boolean; cdecl;
        DPSetFieldByNameAsBoolean: procedure(aDP: PDllDataProvider; const aFieldName: WideString;
            aValue: boolean); cdecl;
        DPGetFieldByNameAsDateTime: function(aDP: PDllDataProvider; const aFieldName: WideString)
            : TDateTime; cdecl;
        DPSetFieldByNameAsDateTime: procedure(aDP: PDllDataProvider; const aFieldName: WideString;
            aValue: TDateTime); cdecl;
        DPGetFieldAsString: function(aDP: PDllDataProvider; const aIndex: integer): PWideString; cdecl;
        DPSetFieldAsString: procedure(aDP: PDllDataProvider; const aIndex: integer;
            const aValue: WideString); cdecl;
        DPGetFieldAsInt: function(aDP: PDllDataProvider; const aIndex: integer): integer; cdecl;
        DPSetFieldAsInt: procedure(aDP: PDllDataProvider; const aIndex: integer; aValue: integer); cdecl;
        DPGetFieldAsFloat: function(aDP: PDllDataProvider; const aIndex: integer): double; cdecl;
        DPSetFieldAsFloat: procedure(aDP: PDllDataProvider; const aIndex: integer; aValue: double); cdecl;
        DPGetFieldAsBoolean: function(aDP: PDllDataProvider; const aIndex: integer): boolean; cdecl;
        DPSetFieldAsBoolean: procedure(aDP: PDllDataProvider; const aIndex: integer; aValue: boolean); cdecl;
        DPGetFieldAsDateTime: function(aDP: PDllDataProvider; const aIndex: integer): TDateTime; cdecl;
        DPSetFieldAsDateTime: procedure(aDP: PDllDataProvider; const aIndex: integer;
            aValue: TDateTime); cdecl;

        // Dummy-Methoden:
        // Diese können durch echte Methoden ersetzt werden, ohne dass sich die Größe dieser Struktur ändert.
        Dummy0: function: integer; cdecl;
        Dummy1: function: integer; cdecl;
        Dummy2: function: integer; cdecl;
        Dummy3: function: integer; cdecl;
        Dummy4: function: integer; cdecl;
        Dummy5: function: integer; cdecl;
        Dummy6: function: integer; cdecl;
        Dummy7: function: integer; cdecl;
        Dummy8: function: integer; cdecl;
        Dummy9: function: integer; cdecl;
    end;

    PDLLInterface = ^TDLLInterface;


implementation



end.
