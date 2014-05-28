{ --------------------------------------------------------------------------------------------------
  Copyright © 2005 Zinsser Analytic GmbH - All rights reserved.
  Author       : Payman Kamali (pk)
  Description  : General Interface for SyringePump, GearPump (, PeriPump, EDOS, CatPump )
  --------------------------------------------------------------------------------------------------
  Revision History:
  date     op  method                       track-no improvement/change
  -------- --  ---------------------------  -------- -----------------------------------------------
  16.08.05 pk                                TN2550  New
  08.02.06 thr PipPump_Init                  TN2925  additional parameter InitID
  19.04.06 wl  PipPump_PickSteps/DispSteps     TN3051   abgeschafft, es werden immer Steps übergeben
  19.04.06 wl  PipPump_GetMaxVolSteps          TN3051    diese Angabe wird jetzt im Interface gespeichert
  19.04.06 wl  PipPump_GetVolumeMustBeRounded  TN3051    soll der VolSteps-Wert gerunded werden, oder sind Kommastellen OK (z.B. EDOS)
  19.04.06 wl  IIntfPipPump                    TN3051    benutzt extended für alle Vol, Speed und VolSteps
  19.04.06 wl  PipPump_GetVersion              TN3051    diese Schnittstelle muß versioniert werden
  12.09.06 thr TIntfDllPipPump                 TN3272   Pick, Disp und DispAll mit Wait-Parameter, neue Function Wait
  16.08.10 wl  StandBy                         TN5231   angelegt (wird von PipPump04 genutzt)
  10.05.11 wl  TVPosArray                      TN5893   --> SamplerDllTypes
  19.11.12 ts  GetCurrentVolume                TN6019   Neu um komplettes Volumen abzugeben (ViscTool)
  11.09.13 wl  Pick,Disp,DispAll               TN6249   neuer Parameter aInWashOrWaste
  27.11.13 ts                                  TN6314   if valve is already at destination pos, it hasn´t to be moved again, V_UNDEFINED implemented
  -------------------------------------------------------------------------------------------------- }

unit IntfPipPumpDriver;


interface


uses
    IntfModuleExecute,
    Driver;

type
    TValvePos = (V_SYR_TIP, V_PER_TIP, V_SYR_SYS, V_PER_SYS, V_UNDEFINED);

    IPipPumpDriver = interface(IDriver)
        ['{0BC17907-CFDB-48A8-B5E4-359EB4BE600C}']
        function GetAspRamp: integer;
        function GetAspSpeed: integer;
        function GetDispRamp: integer;
        function GetDispSpeed: integer;
        function GetMaxSteps: integer;
        function GetMaxVolume: integer;
        function GetCurrentVolume: integer;
        function GetVolumeMustBeRounded(): boolean;
        procedure Init(aInitID: TDateTime);
        procedure StandBy(const aStandBy: boolean);
        procedure Pick(aVolSteps, aSpeed, aRamp: extended; aWait: boolean; aInWashOrWaste: boolean);
        procedure Disp(aVolSteps, aSpeed, aRamp: extended; aWait: boolean; aInWashOrWaste: boolean);
        procedure TurnValve(aNewValvePos: TValvePos);
        procedure DispAll(aWait: boolean; aInWashOrWaste: boolean);
        function Wait(): boolean;
        function GetExecIntf(): IModuleExecute;
        property AspSpeed: integer read GetAspSpeed;
        property AspRamp: integer read GetAspRamp;
        property DispSpeed: integer read GetDispSpeed;
        property DispRamp: integer read GetDispRamp;
        property MaxVolume: integer read GetMaxVolume;
        property CurrentVolume: integer read GetCurrentVolume;
        property MaxSteps: integer read GetMaxSteps;
    end;


implementation


end.
