unit SchedMakerTypes;
{ --------------------------------------------------------------------------------------------------
  Copyright © 2005 Zinsser Analytic GmbH - All rights reserved.
  Author       : Payman Kamali (pk)
  Description  : Types used by Scheduler units
  --------------------------------------------------------------------------------------------------
  Revision History:
  date     op  method                       track-no improvement/change
  -------- --  ---------------------------  -------- -----------------------------------------------
  16.02.05 pk                                TN2315    New
  04.03.05 pk                                TN2301.1  uses RunStep removed
  29.03.05 pk                                TN2365    New property : Description
  06.04.05 pk  FlushExeState                 TN2373    New
  19.04.05 pk  CheckExeExists                TN2390   New
  07.11.05 pk                                TN2737   Various changes for Dynamic Scheduling
  14.11.05 pk                                TN2758   Various changes for Dynamic Scheduling
  18.04.06 pk  GetColor                      TN3048    Show color
  29.01.07 pk                                TN3527   Integer changed to cardinal since schedtimes based on 1/100th of a second instead of second
  -------------------------------------------------------------------------------------------------- }


interface


uses
    Contnrs,
    Graphics;

const
    STR_DEFAULT_SHARED_ID = '0';
    DEFAULT_MIN_TIME = 1;

type
    TEnScheduleMode = (smNormal, smRunTime, smReScheduleLive);

    TExecActionState = (easClear, easStarted, easFinished, easFailed);

    TSchedMakerExecAction = class
    protected
        function GetDescription(): string; virtual; abstract;
        function GetActionName(): string; virtual; abstract;
        function GetColor(): TColor; virtual; abstract;
        function GetMinTime(): cardinal; virtual; abstract;
        procedure SetMinTime(aMinTime: cardinal); virtual; abstract;
        function GetMaxTime(): cardinal; virtual; abstract;
        procedure SetMaxTime(aMaxTime: cardinal); virtual; abstract;
        function GetStateFlag(): TExecActionState; virtual; abstract;
        procedure SetStateFlag(aStateFlag: TExecActionState); virtual; abstract;
        function GetSharedID(): integer; virtual; abstract;
        function GetIsShareParent(): boolean; virtual; abstract;
        function GetResID(): integer; virtual; abstract;
        procedure SetResID(aResID: integer); virtual; abstract;
        function GetSchedStart(): cardinal; virtual; abstract;
        procedure SetSchedStart(aSchedStart: cardinal); virtual; abstract;
        function GetSchedEnd(): cardinal; virtual; abstract;
        procedure SetSchedEnd(aSchedEnd: cardinal); virtual; abstract;
        function GetRealStart(): cardinal; virtual; abstract;
        procedure SetRealStart(aRealStart: cardinal); virtual; abstract;
        function GetRealEnd(): cardinal; virtual; abstract;
        procedure SetRealEnd(aRealEnd: cardinal); virtual; abstract;

    public

        procedure DetermineTimes(aElapsedTime: cardinal; aStateChangePossible: boolean;
            out oMinTime, oMaxTime: cardinal; out oSchedStart, oSchedEnd: integer;
            out oState: TExecActionState); virtual; abstract;

        procedure FlushExeState(const aExeName: string); virtual; abstract;

        property Description: string read GetDescription;
        property ActionName: string read GetActionName;
        property Color: TColor read GetColor;
        property MinTime: cardinal read GetMinTime write SetMinTime;
        property MaxTime: cardinal read GetMaxTime write SetMaxTime;
        property StateFlag: TExecActionState read GetStateFlag write SetStateFlag;
        property SharedID: integer read GetSharedID;
        property IsShareParent: boolean read GetIsShareParent;
        property ResID: integer read GetResID write SetResID;
        property SchedStart: cardinal read GetSchedStart write SetSchedStart;
        property SchedEnd: cardinal read GetSchedEnd write SetSchedEnd;
        property RealStart: cardinal read GetRealStart write SetRealStart;
        property RealEnd: cardinal read GetRealEnd write SetRealEnd;

    end;

    TSchedMakerExecActionList = class(TObjectList)
    end;

    TSchedMakerReaderWriter = class
    public
        procedure ReadExecActionList(const aDatasourceName: string; const aPriority: integer;
            aList: TSchedMakerExecActionList); virtual; abstract;
        procedure RereadExecActionList(const aDatasourceName: string; const aPriority: integer;
            aList: TSchedMakerExecActionList); virtual; abstract;
        procedure WriteExecActionList(const aTarget: string; aExecActionList: TSchedMakerExecActionList;
            aAppend: boolean); virtual; abstract;
        procedure CheckExeExists(const aExeName: string); virtual; abstract;
    end;


implementation


end.
