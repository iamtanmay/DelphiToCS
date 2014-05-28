unit IntfMotionDevice;
{ --------------------------------------------------------------------------------------------------
  Copyright © 2007 Zinsser Analytic GmbH - All rights reserved.
  Author       : Payman Kamali (pk)
  Description  :
  --------------------------------------------------------------------------------------------------
  Revision History:
  date     op  method                       track-no improvement/change
  -------- --  ---------------------------  -------- -----------------------------------------------
  05.09.07 pk                                         Initial Revision
  09.11.07 pk  GetZBounds                   TN3924    Steps to mm
  29.01.08 wl                               TN3980    uses geändert
  19.09.08 pk  MoveThruPath, DeviceMoveY    TN4215    New
  10.12.08 wl  MoveTo/FromGripPosition      TN4353    neu: für Tube- und Rack-Handling
  07.04.09 ts  MoveThruPath                 TN4509    added UseTrack to move the robot without track (default = true)
  12.09.09 wl                               TN4740   MPOS durch integer ersetzt
  12.09.09 wl                               TN4740   default-Werte aus interface entfernt
  -------------------------------------------------------------------------------------------------- }


interface


uses
    CommonTypes,
    AppTypes,
    IntfLocationBasedAllAxesDriver,
    Driver,
    IntfDevice,
    IntfMotorDriver;

const
    INT_FIRST_POSITION = 1;

type
    TMotorDimension = (mdX, mdY, mdZ);

    TMovePlateType = (mpPut, mpGet);

    TOnMotionExecute = procedure(aSender: TObject) of object;

    IMotionDevice = interface(IDevice)
        ['{F79B9BD0-F512-45F8-9EDC-0006321FCC9A}']
        procedure GetXYBounds(out oXMinBound, oXMaxBound, oYMinBound, oYMaxBound: TPosMM);
        procedure GetZBounds(out oZMaxBound: TPosMM);
    end;

    ILocationBasedMotionDevice = interface(IMotionDevice)
        ['{42A6B1E8-7E4A-4A85-A8D2-4E1FC71EE3FF}']
        // neu:
        procedure DeviceMoveToPos(aLocationName: string; aPosition: integer);
        function DeviceGetApproachLocationExists(const aLocationName: string): boolean;
        procedure DeviceApproachPath(aPathname: string; aPositions: TMovoThruPathPos; aRewind: boolean);
        procedure DeviceMoveToTubePos(aRackName: string; aPosition: integer);
        procedure DeviceMoveZ(aDist: double);
        procedure DeviceMoveY(aDist: double);
        procedure DeviceMoveX(aDist: double);
        procedure DeviceMoveToCarrierSlot(aCarrierName: string; aSlot: integer);
        procedure MoveThruPath(const aPathname: string; aPositions: TMovoThruPathPos; aRewind: boolean;
            aUseTrack: boolean);
        procedure MoveInDimension(aMotorDimension: TMotorDimension; aPos: TPosMM; aExec: EXEC_MODE;
            aMode: ACTION_TYPE; aSpeed: integer; aRamp: integer; aClampAbsValue: boolean);
        procedure MoveToGripPosition(const aRackname: string; aPosition: integer; aDistX, aDistZ: double;
            aDoXBeforeZ: boolean);
        procedure MoveFromGripPosition(const aRackname: string; aPosition: integer; aDistX, aDistZ: double;
            aDoXBeforeZ: boolean);
    end;


implementation


end.
