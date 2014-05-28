{ --------------------------------------------------------------------------------------------------
  Copyright  2008 Zinsser Analytic GmbH - All rights reserved.
  Author       : Payman Kamali (pk)
  Description  : Device that can store a string (or other) value
  --------------------------------------------------------------------------------------------------
  Revision History:
  date     op  method                       track-no improvement/change
  -------- --  ---------------------------  -------- -----------------------------------------------
  29.01.08 pk                                        initial version
  10.07.08 hd                                TN4163  aInitID parameter added in the Init method
  10.12.08 wl  MoveTo/FromGripPosition       TN4353    neu: für Tube- und Rack-Handling
  20.01.09 wl  TLocationDetailsRec           TN4358    neu
  03.02.09 wl  IAllAxesDriver                TN4411  X-Offset und Z-Offset können trotzdem noch benutzt werden
  07.04.09 ts  IAllAxesDriver
  ILocationBasedAllAxesDriver   TN4509  added UseTrack to move the robot without track (default = true)
  -------------------------------------------------------------------------------------------------- }

unit IntfLocationBasedAllAxesDriver;


interface


uses
    Driver;

type
    TMovoThruPathPos = (pposAll, pposFirstPosOnly, pposLastPosOnly, pposAllWithoutFirstPos,
        pposAllWithoutLastPos);

    //
    TLocationDetailsRec = record
        index: integer;
        MoveType: string;
        InterpolationType: integer;
        J1: double;
        J2: double;
        J3: double;
        J4: double;
        J5: double;
        J6: double;
        J7: double;
        XOffset: double;
        ZOffset: double;
    end;

    TLocationDetailsRecs = array of TLocationDetailsRec;

    TLocationRec = record
        Valid: boolean;
        name: string;
        Speed: double;
        Acceleration: double;
        Deceleration: double;
        Simultaneuos: boolean;
        Details: TLocationDetailsRecs;
    end;

    IAllAxesDriver = interface(IDriver)
        ['{2907C0C4-0AC2-47DC-BB2B-6845701DD324}']
        procedure Init(aInitID: TDevInitID);
        procedure Reset();
        procedure MoveThruPath(aLocation: TLocationRec; aPositions: TMovoThruPathPos; aRewind: boolean;
            aUseTrack: boolean = true); // added UseTrack to move the robot without track (default = true)
        procedure MoveToPos(aLocation: TLocationRec; aPosition: integer);
        procedure MoveToGripPosition(aLocation: TLocationRec; aPosition: integer; aDistX, aDistZ: double;
            aDoXBeforeZ: boolean);
        procedure MoveFromGripPosition(aLocation: TLocationRec; aPosition: integer; aDistX, aDistZ: double;
            aDoXBeforeZ: boolean);
        procedure MoveZ(aDist: double);
        procedure MoveX(aDist: double);
        procedure MoveY(aDist: double);
    end;

    // z.B. CRS: Positionen werden intern verwaltet
    ILocationBasedAllAxesDriver = interface(IDriver)
        ['{D82BFD0B-DAAC-40E8-95F8-B5AF68A649DE}']
        procedure Init(aInitID: TDevInitID);
        procedure Reset();
        procedure MoveThruPath(aPathname: string; aPositions: TMovoThruPathPos; aRewind: boolean;
            aUseTrack: boolean = true); // added UseTrack to move the robot without track (default = true)
        procedure MoveToPos(aRackname: string; aPosition: integer);
        procedure MoveZ(aDist: double);
        procedure MoveX(aDist: double);
        procedure MoveY(aDist: double);
        function GetLocationSize(aLocationName: string): integer;
        procedure MoveToGripPosition(const aRackname: string; aPosition: integer; aDistX, aDistZ: double;
            aDoXBeforeZ: boolean);
        procedure MoveFromGripPosition(const aRackname: string; aPosition: integer; aDistX, aDistZ: double;
            aDoXBeforeZ: boolean);
    end;


implementation


end.
