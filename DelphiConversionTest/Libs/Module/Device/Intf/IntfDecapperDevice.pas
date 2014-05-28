unit IntfDecapperDevice;
{ --------------------------------------------------------------------------------------------------
  Copyright © 2008 Zinsser Analytic GmbH - All rights reserved.
  Author       : Wolfgang Lyncke (wl)
  Description  :
  --------------------------------------------------------------------------------------------------
  Revision History:
  date     op  method                       track-no  improvement/change
  -------- --  ---------------------------  --------  ----------------------------------------------
  11.09.08 wl  CapIntake,CapRelease          TN4173   Neuer Parameter WithCap
  -------------------------------------------------------------------------------------------------- }


interface


uses
    IntfDevice,
    IntfSensorDevice;

type
    ITurntableAndBCReaderDevice = interface(IDevice)
        ['{A5133C96-982F-4121-A09E-4CDBE972B47A}']
        function ReadTubeID: string;
        function HasTubeReader: boolean;
        function GetTubeSensor: ISensorDevice;
        function GetUseTubeSensor: boolean;
        procedure SetUseTubeSensor(const Value: boolean);
        function GetTubePosRackName: string;
        function GetTubePosition: integer;

        property TubePosRackName: string read GetTubePosRackName;
        property TubePosition: integer read GetTubePosition;
        property UseTubeSensor: boolean read GetUseTubeSensor write SetUseTubeSensor;
        property TubeSensor: ISensorDevice read GetTubeSensor;
    end;

    TDecapperAction = (acnCapping, acnDecapping);

    IDecapperDevice = interface(ITurntableAndBCReaderDevice)
        ['{2D5FF999-2D6D-4CA2-B1B7-4B0FF331ED53}']
        procedure InitDecapper(aCapInDatabase: boolean);
        procedure TubeIntake;
        procedure TubeRelease;
        function GoToBasicState: boolean;
        procedure PrepareForDecap;
        procedure TakeCapOff(aCapToWaste: boolean);
        procedure PutCapOn;
        function TakeCapOffAndRead(aCapToWaste: boolean): string;
        procedure CapRelease(aWithCap: boolean);
        procedure CapIntake(aWithCap: boolean);
        function CapCheck(aExpectCapInGripper: boolean; aDependsFromInternCapWaste: boolean = false): integer;
        function ActionPossible(aAction: TDecapperAction): boolean;
        function GetCurrentTubeRotation: integer;
        function GetTubeDropBeforeIntake: boolean;
        function GetCapDropBeforeIntake: boolean;
        function GetUseCapSensor: boolean;
        procedure SetUseCapSensor(const Value: boolean);
        function GetCapPosRackName: string;
        function GetCapPosition: integer;

        property CurrentTubeRotation: integer read GetCurrentTubeRotation;
        property TubeDropBeforeIntake: boolean read GetTubeDropBeforeIntake;
        property CapDropBeforeIntake: boolean read GetCapDropBeforeIntake;
        property UseCapSensor: boolean read GetUseCapSensor write SetUseCapSensor;
        property CapPosRackName: string read GetCapPosRackName;
        property CapPosition: integer read GetCapPosition;
    end;


implementation


end.
