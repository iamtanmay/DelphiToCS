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
  15.12.11 wl       6   TN5767   von Version 7.4.5 hierher kopiert
  -------------------------------------------------------------------------------------------------- }

unit DLLInterfaceV6;

interface


const
    INT_DLLINTERFACE_VERSION = 6; // muß bei jeder Änderung hochgezählt werden

    STR_DLLINT_DEFVALUE_IDENT_GETTIPMAXVOL = 'GETTIPMAXVOL';
    STR_DLLINT_DEFVALUE_IDENT_GETRACKPOS = 'GETRACKPOS';
    STR_DLLINT_DEFVALUE_IDENT_PREPARERUN = 'PREPARERUN';
    STR_DLLINT_DEFVALUE_IDENT_DLLFU = 'DLLFU';
    STR_DLLINT_DEFVALUE_IDENT_GENERATEMULTIPIPSEQ = 'GENERATEMULTIPIPSEQ';
    STR_DLLINT_DEFVALUE_IDENT_DATAPATH = 'DATAPATH';
    STR_DLLINT_DEFVALUE_IDENT_MODALDIALOGOPENED = 'MODALDIALOGOPENED';
    STR_DLLINT_DEFVALUE_IDENT_MODALDIALOGCLOSED = 'MODALDIALOGCLOSED';
    STR_DLLINT_DEFVALUE_IDENT_GETCURRENTVOLUME = 'GETCURRENTVOLUME';
    STR_DLLINT_DEFVALUE_IDENT_PAINTRACKPOS = 'PAINTRACKPOS';
    STR_DLLINT_DEFVALUE_IDENT_CONFIRMADD = 'CONFIRMADD';
    STR_DLLINT_DEFVALUE_IDENT_CONFIRMEDIT = 'CONFIRMEDIT';
    STR_DLLINT_DEFVALUE_IDENT_CONFIRMDELETE = 'CONFIRMDELETE';
    STR_DLLINT_DEFVALUE_IDENT_USERISADMIN = 'USERISADMIN';

type
    PDLLInterface = ^TDLLInterface;
    TInterfaceStr = PWideString;

    TDLLInterface = record
        SetGlobalErr: procedure(aValue: integer; aReason: string); cdecl;
        GetGlobalErr: function: integer; cdecl;
        IsGlobalErr: function: boolean; cdecl;
        IsSimulationMode: function: boolean; cdecl;
        ApplicationSleep: procedure(aMSec: cardinal); cdecl;
        Log: procedure(aLogText: WideString; aLogType: Longint; aDisplay: boolean); cdecl;
        ReadStringApp: function(aSection, aIdent: WideString): TInterfaceStr; cdecl;
        ReadStringRobot: function(aSection, aIdent: WideString): TInterfaceStr; cdecl;
        ReadStringDLL: function(aSection, aIdent: WideString): TInterfaceStr; cdecl;
        ReadStringOther: function(aIniName, aSection, aIdent, aDefault: WideString): TInterfaceStr; cdecl;
        ReadIntApp: function(aSection, aIdent: WideString): Longint; cdecl;
        ReadIntRobot: function(aSection, aIdent: WideString): Longint; cdecl;
        ReadIntDLL: function(aSection, aIdent: WideString): Longint; cdecl;
        ReadIntOther: function(aIniName, aSection, aIdent: WideString; aDefault: Longint): Longint; cdecl;
        GetAlias: function: TInterfaceStr; cdecl;
        GetRunAlias: function: TInterfaceStr; cdecl;
        GetDefinedValue: function(aParameter: WideString): TInterfaceStr; cdecl;
        // individuell zu definierende Funktion
        GetCurrentScriptName: function: TInterfaceStr; cdecl;
        GetCurrentMethodName: function: TInterfaceStr; cdecl;
        GetCurrentRunName: function: TInterfaceStr; cdecl;
    end;

function StrToInterfaceStr(aStr: string): TInterfaceStr;
function InterfaceStrToStr(aStr: TInterfaceStr): string;



implementation


function StrToInterfaceStr(aStr: string): TInterfaceStr;
begin
    result := PWideString(WideString(aStr));
end;

function InterfaceStrToStr(aStr: TInterfaceStr): string;
begin
    result := WideString(aStr);
end;


end.
