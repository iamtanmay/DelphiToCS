{ --------------------------------------------------------------------------------------------------
  Copyright  2008 Zinsser Analytic GmbH - All rights reserved.
  Author       : Payman Kamali (pk)
  Description  : Device that can store a string (or other) value
  --------------------------------------------------------------------------------------------------
  Revision History:
  date     op  method                       track-no improvement/change
  -------- --  ---------------------------  -------- -----------------------------------------------
  29.01.08 pk                                          initial version
  27.11.08 wl  ACTION_TYPE                  TN3950    neu: AT_BLOCK_MOVE1, -2 und -3
  19.10.09 pk  IMotorDevice.Init            TN4820    New InitID parameter
  30.11.09 pk  TMoveMotorDoneNotifyEvent    TN4915    New
  27.01.11 wl  GetExecIntf                  TN5357   Rückgabewert: IModuleMotorExecute
  09.03.11 wl  IMotorUsage                  TN5496   ist jetzt Teil des Drivers
  10.03.11 wl  ReadPosIsThreadSafe          TN5496   können verschiedene Threads auf die ReadPos-Funktion zugreifen?
  -------------------------------------------------------------------------------------------------- }

unit IntfMotorDriver;


interface


uses
    Module,
    Driver,
    IntfModuleExecute,
    CommonTypes;

type
    ACTION_TYPE = (AT_MOVE_ABS, AT_BLOCK_MOVE1, AT_BLOCK_MOVE2, AT_BLOCK_MOVE3, AT_SCAN_MOVE, AT_MOVE_REL,
        // das gehört hier nicht rein!!
        AT_VCLOSE_MOVE);

    TMoveMotorNotifyEvent = procedure(aUsedMotor: IModule; aDest: double; aIsInitMove: boolean) of object;
    TMoveMotorDoneNotifyEvent = procedure(aUsedMotor: IModule; aDest: double) of object;

    IMotorUsage = interface
        ['{64AFCA34-AB6E-4F24-89EA-D7BBEF30F471}']
        procedure ClearPending();
        procedure ClearUsedByID();
        procedure RegisterPendingMove(const aUsedByID: cardinal; const aDestPos: TPosMM;
            const aIsInitMove: boolean);
        procedure PendingMoveDone(const aPos: TPosMM);
        function GetCurrentPos: TPosMM;
        function GetLastUsedByID: cardinal;
        function GetPendingDestPos: TPosMM;
        function GetIsMovePending: boolean;
        function GetIsInitMovePending: boolean;
        function GetIsNonInitMovePending: boolean;
        function IsLastUsedBy(const aUsedByID: cardinal): boolean;
        function IsUsageAllowed(const aUsedByID: cardinal): boolean;
        function WaitFor(): boolean;
        property CurrentPos: TPosMM read GetCurrentPos;
        property LastUsedByID: cardinal read GetLastUsedByID;
        property PendingDestPos: TPosMM read GetPendingDestPos;
        property IsMovePending: boolean read GetIsMovePending;
        property IsInitMovePending: boolean read GetIsInitMovePending;
        property IsNonInitMovePending: boolean read GetIsNonInitMovePending;
    end;

    // Motor
    IMotorDriver = interface(IDriver)
        ['{175A6A0D-04C3-45C6-A22C-E8BA425A1681}']
        function GetCurrent_Steps(aRead: boolean): integer;
        procedure Move(aMode: ACTION_TYPE; aDest, aSpeed, aRamp: integer);
        function DetectionDone(var aDetected: boolean; var aPosition: integer): boolean;
        function HasError(): boolean;
        procedure DisableError();
        procedure ResetDisabledError();
        procedure SetScanMode(aScanMode: integer);
        procedure SetNextWaiting(aDisable: boolean);
        procedure DisableInitCheck(aDisable: boolean);
        procedure Init(aInitID: TDevInitID);
        function IsStepMotor(): boolean;
        function IsInLB(): boolean;
        function GetMotorID: integer;
        function GetSpeed(): integer;
        procedure SetSpeed(aSpeed: integer);
        function GetRamp(): integer;
        function GetInitSpeed(): integer;
        function GetExecIntf(): IModuleMotorExecute;
        property MotorID: integer read GetMotorID;
        property Speed: integer read GetSpeed write SetSpeed;
        property Ramp: integer read GetRamp;
        property InitSpeed: integer read GetInitSpeed;
        function GetMotorUsage: IMotorUsage;
        procedure SetMotorUsage(aValue: IMotorUsage);
        property MotorUsage: IMotorUsage read GetMotorUsage write SetMotorUsage;
        function ReadPosIsThreadSafe: boolean;
    end;


implementation


end.
