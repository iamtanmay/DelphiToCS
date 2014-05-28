{ --------------------------------------------------------------------------------------------------
  Copyright © 2003 Zinsser Analytic GmbH - All rights reserved.
  Author       : Wolfgang Lyncke (wl)
  Description  : all kinds of liquid pumps (Syringe pumps, peristaltic pumps, ..)
  --------------------------------------------------------------------------------------------------
  Revision History:
  date     op  method                       track-no improvement/change
  -------- --  ---------------------------  -------- -----------------------------------------------
  15.10.03 wl                               TN1624   initial version
  05.11.03 wl  CreateAsPerifill             TN1624   erzeugt Perifill-Interface statt PipPump
  05.04.04 wl  AspSpeed,DispSpeed,MaxVolume TN1788   neue Properties
  05.04.04 wl  Create,CreateAsPerifill      TN1788   TipIndex wird jetzt übergeben und nicht aus Namen ausgelesen
  30.04.04 wl  TPipPumpDevice.GetBitName    TN2157   wenn Bit = -1 wird nichts angezeigt (nur für Testfenster)
  16.02.05 wl  TPipPumpDevice.Create        TN2269   über 'SysInput' wird der Name des angeschlossenen Device oder der Systemflüssigkeit gelesen
  16.02.05 wl  TPipPumpDevice.Disp          TN2269   kann das Volumen zu gLiquids.WasteLiquid hinzurechnen
  16.02.05 wl  TPipPumpDevice.Pick          TN2269   bei Ventilstellung V_SYR_SYS wird das Volumen von der aktuellen Systemfl. abgezogen
  16.02.05 wl  TPipPumpDevice.GetCurrentSysLiquid  TN2269   findet die aktuell angeschlossene Systemflüssigkeit
  15.04.05 wl  TPipPumpDeviceArray          TN2380.1 wird in der Funktion TGripperArmDevice.FlushAddSyringes benutzt
  19.04.05 wl  TPipPumpDevice.GetCurrentAspSpeed     TN2380.1  Funktion von DevicesTips hierher verschoben
  20.04.05 wl  TPipPumpDevice.Pick                   TN2380.1  ruft selbstständig GetCurrentAspSpeed auf wenn Speed = 0
  20.06.05 tbh TPipPumpDevice                        TN2385    new: fMaxMotorStep (of dilutor motor)
  20.06.05 tbh TPipPumpDevice.PickSteps              TN2385    new: for aspiration of a certain number of dilutor motor steps
  20.06.05 tbh TPipPumpDevice.DispSteps              TN2385    new: for dispense of a certain number of dilutor motor steps
  20.06.05 tbh TPipPumpDevice.GetMotorStepsForVolume TN2385    new: calculates the required dilutor motor steps for a volume
  22.06.05 tbh TPipPumpDevice.PickSteps              TN2385    Aufruf für Interface angepasst
  22.06.05 tbh TPipPumpDevice.DispSteps              TN2385    Aufruf für Interface angepasst
  16.08.05 wl  TPipPumpDeviceFactory                 TN2550    erzeugt TPipPumpDevice und IPipPumpInterface
  16.08.05 wl  TPipPumpDevice.Create                 TN2550    liest 'DllName'
  16.08.05 pk                                        TN2550    uses InterfacePipPump
  14.10.05 wl  TPipPumpDeviceFactory.CreateIntfAsPipPump TN2670   benutzt TDevice.GetSamplerProtocol
  03.02.06 thr CreateIntfAsPipPump          TN2896  Bitnr (Port) wird mit übergeben
  08.02.06 thr InitSyringe                  TN2925  additional parameter InitID
  14.03.06 thr GetMotorStepsForVolume       TN2963  Division geschützt
  31.03.06 pk  TPipPumpDevice.Create        TN2958  AspSpeed, Ramp, etc now read from settings
  31.03.06 pk  CreateIntfAsPipPump          TN2958  Now passes AspSpeed, Ramp, etc to AddPipPump
  03.04.06 pk  Init                         TN2997  Init now has parameter aInitID
  19.04.06 wl                                        TN3051    benutzt extended für alle Vol, Speed und VolSteps
  19.04.06 wl  TPipPumpDevice.Create                 TN3051    Default für MaxMotorStep ist 0, nicht mehr 2000 (muß jetzt immer angegeben werden)
  19.04.06 wl  TPipPumpDevice.GetMaxVolSteps         TN3051    diese Angabe wird jetzt im Interface gespeichert
  19.04.06 wl  TPipPumpDevice.GetRoundedVolume       TN3051    Der weggerundete Rest wird gespeichert und darf nicht -0.5 unter bzw. 0.5 überschreiten
  25.04.06 pk  GetMaxVolume_ul                       TN2958    Return max volume even if not active
  08.05.06 pk  TPipPumpDevice.Create                 TN3051    if GetMaxVolume returns 0 dont divide by 0
  23.05.06 pk  LoadDevice                            TN3113    New : Needed for passing execution id to interface
  12.09.06 thr                                       TN3272   Anpassung wegen geändertem Pippump-Interface
  18.09.06 pk                                        TN3227.1 remove references to SystemLiquid, where possible
  23.10.06 wl  WaitFor                               TN3375   ruft PipPump_Wait auf, das bisher nur von CADI.DLL unterstützt wird
  05.12.06 wl  CreateIntfAsPipPump                   TN3243   erweitert um Name und DeviceType
  07.03.07 wl  TPeriPumpDevice                       TN3620   deaktiviert, --> DevicesOther
  08.03.07 wl  LoadDevice                            TN3620   Abfrage auf IsSias entfernt
  29.01.08 wl                                        TN3980   uses geändert
  19.05.08 pk                                        TN4115   New: InitPipPump
  15.12.05 ts  NoAspBeforeDispDil                    TN4947   new
  12.08.10 wl  TPipPumpDeviceArray                   TN5227   von PipDevice hierher
  05.04.11 wl  TPipPumpDeviceArray                   TN5535   = TArray<IPipPumpDevice>
  08.08.12 wl  GetPeriPump                           TN5947   neu: Zeigt an, ob eine Verbindung zu einer PeriPumpe besteht
  19.11.12 ts  GetCurrentVolume                      TN6019   Neu um komplettes Volumen abzugeben (ViscTool)
  15.02.13 wl  fVolumes                              TN5914   von TipSystem hierher
  11.09.13 wl  Pick,PickSteps,DispAll                TN6249   neuer Parameter aInWashOrWaste
  11.09.13 wl  Disp,DispSteps                        TN6249   Parameter umbenannt: aAddToWaste -> aInWashOrWaste
  -------------------------------------------------------------------------------------------------- }

unit IntfPipPumpDevice;


interface


uses
    Generics.Collections,
    AppTypes,
    IntfModuleExecute,
    IntfDevice,
    IntfPeriPumpDevice,
    Liquids,
    VolumeInfo,
    IntfPipPumpDriver;

type
    IPipPumpDevice = interface(IDevice)
        ['{13696A9F-0B00-4DBA-9320-ED5E180B3FFF}']
        function GetName: string;
        function GetInputPortName(): string;
        function GetTipIndex: integer;
        function GetMaxVolume_ul: extended;
        function GetCurrentVolume_ul: extended;
        function GetAspSpeed: integer;
        function GetDispSpeed: integer;
        function GetMaxVolumeSteps: extended;
        function GetCurrentSysLiquid: TSystemLiquid;
        function GetActive(): boolean;
        function GetNoAspBeforeDispDil: boolean;
        function GetPeriPump: IPeriPumpDevice;

        procedure Pick(aVolume: extended; aSpeed: extended; aInWashOrWaste: boolean; aWait: boolean);
        procedure Disp(aVolume, aSpeed, aRamp: extended; aInWashOrWaste: boolean; aWait: boolean);
        procedure PickSteps(aVolSteps, aRealVolume: extended; aSpeed: extended; aInWashOrWaste: boolean;
            aWait: boolean);
        procedure DispSteps(aVolSteps, aRealVolume, aSpeed, aRamp: extended; aInWashOrWaste: boolean;
            aWait: boolean);
        procedure DispAll(aInWashOrWaste: boolean; aWait: boolean);
        procedure TurnValve(aNewValvePos: TValvePos);
        function GetCurrentAspSpeed: extended;
        function VolumeULToVolumeSteps(aVolumeUL: extended): extended;
        procedure WaitFor();
        function GetExecIntf(): IModuleExecute;
        procedure InitPipPump(aInitID: TDateTime; aExec: EXEC_MODE);

        function GetVolumes: TObjectList<TVolumeInfo>;
        property Volumes: TObjectList<TVolumeInfo>read GetVolumes;

        property TipIndex: integer read GetTipIndex;
        property MaxVolume_ul: extended read GetMaxVolume_ul;
        property CurrentVolume_ul: extended read GetCurrentVolume_ul;
        property MaxMotorStep: extended read GetMaxVolumeSteps;
        property AspSpeed: integer read GetAspSpeed;
        property DispSpeed: integer read GetDispSpeed;
        property InputPortName: string read GetInputPortName;
        property Active: boolean read GetActive;
        property name: string read GetName;
        property NoAspBeforeDispDil: boolean read GetNoAspBeforeDispDil;
        property PeriPump: IPeriPumpDevice read GetPeriPump;
    end;

    TPipPumpDeviceArray = TArray<IPipPumpDevice>;


implementation


end.
