{ --------------------------------------------------------------------------------------------------
  Copyright © 2007 Zinsser Analytic GmbH - All rights reserved.
  Author       : Wolfgang Lyncke (wl), Payman Kamali (pk)
  Description  : devices for robotic arms (gripper and pipetting arm)
  --------------------------------------------------------------------------------------------------
  Revision History:
  date     op  method                       track-no improvement/change
  -------- --  ---------------------------  -------- -----------------------------------------------
  05.09.07 pk                                         Initial Revision
  25.04.08 wl  Execute                      TN4051    entfernt
  03.06.08 pk  GetArmGroupID                TN4133    New
  12.09.09 wl                               TN4740   TipFloat-/TPosMM-/TipInteger-/MPOSArray durch TDoubleArray/TIntArray ersetzt
  27.08.10 wl  HandlerXYIndependetFromRotation  TN5250   entfernt
  26.01.11 wl  MoveZTravelIfNoXYMove        TN5448    entfernt
  04.04.11 wl  ArmGroupID                   TN5525    entfernt
  09.04.11 wl  Rack-/TubeBCReaderDevice     TN5545    neu
  02.02.11 wl  StorePreviousXYMoveInfo      TN5791   geänderte Parameter
  23.05.13 wl  TArmPositionMemory           TN6153   neu: enthält Get- und Set-Funktionen für PreviousPosition
  23.05.13 wl  IArmDevice.PositionMemory    TN6153   ersetzt RackMoveManager und PreviousZStep
  -------------------------------------------------------------------------------------------------- }

unit IntfArmDevice;


interface


uses
    GeneralTypes,
    AppTypes,
    CommonTypes,
    IntfDevice,
    IntfPipDevice,
    IntfGripDevice,
    IntfMotionDevice,
    IntfZTravelManager,
    IntfContainerBCReaderDevice;

type
    TCustomArmPositionInfo = class
    end;

    TSetArmPositionEvent = procedure(aValue: TCustomArmPositionInfo) of object;
    TGetArmPositionEvent = function(): TCustomArmPositionInfo of object;

    // Hilfsklasse, um Events definieren zu können
    TArmPositionMemory = class
    private
        fPreviousPosition: TCustomArmPositionInfo;
        fOnSetPreviousPosition: TSetArmPositionEvent;
        fOnGetPreviousPosition: TGetArmPositionEvent;
        function GetPreviousPosition: TCustomArmPositionInfo;
        procedure SetPreviousPosition(aValue: TCustomArmPositionInfo);
    public
        constructor Create;
        destructor Destroy; override;
        property OnSetPreviousPosition: TSetArmPositionEvent read fOnSetPreviousPosition;
        property OnGetPreviousPosition: TGetArmPositionEvent read fOnGetPreviousPosition;
    end;

    IArmDevice = interface(IDevice)
        ['{F19D9BB2-0BC7-4E47-A4F7-7FAFDBD00BC9}']
        function GetColor: integer;
        function GetGripDevice: IGripDevice;
        function GetInitXBeforeY: boolean;
        function GetMotionDevice: IMotionDevice;
        function GetPipDevice: IPipDevice;
        function GetXYRange: TXYRangeData;
        function GetZTravelManager: IZTravelManager;
        function GetRackBCReaderDevice: IRackBCReaderDevice;
        function GetTubeBCReaderDevice: ITubeBCReaderDevice;
        function GetPositionMemory: TArmPositionMemory;

        property PipDevice: IPipDevice read GetPipDevice;
        property GripDevice: IGripDevice read GetGripDevice;
        property MotionDevice: IMotionDevice read GetMotionDevice;
        property RackBCReaderDevice: IRackBCReaderDevice read GetRackBCReaderDevice;
        property TubeBCReaderDevice: ITubeBCReaderDevice read GetTubeBCReaderDevice;

        property ZTravelManager: IZTravelManager read GetZTravelManager;
        property PositionMemory: TArmPositionMemory read GetPositionMemory;
        property XYRange: TXYRangeData read GetXYRange;
        property Color: integer read GetColor;
        property InitXBeforeY: boolean read GetInitXBeforeY;
    end;


implementation


uses
    SysUtils;

{ TArmPositionMemory }

constructor TArmPositionMemory.Create;
begin
    inherited Create;

    fOnSetPreviousPosition := SetPreviousPosition;
    fOnGetPreviousPosition := GetPreviousPosition;
end;

destructor TArmPositionMemory.Destroy;
begin
    FreeAndNil(fPreviousPosition); // ist Owner dieses Objekts

    inherited;
end;

function TArmPositionMemory.GetPreviousPosition: TCustomArmPositionInfo;
begin
    result := fPreviousPosition;
end;

procedure TArmPositionMemory.SetPreviousPosition(aValue: TCustomArmPositionInfo);
begin
    FreeAndNil(fPreviousPosition); // ist Owner dieses Objekts
    fPreviousPosition := aValue;
end;


end.
