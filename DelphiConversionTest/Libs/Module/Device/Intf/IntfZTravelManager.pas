{ --------------------------------------------------------------------------------------------------
  Copyright © 2007 Zinsser Analytic GmbH - All rights reserved.
  Author       : Payman Kamali (pk)
  Description  :
  --------------------------------------------------------------------------------------------------
  Revision History:
  date     op  method                       track-no improvement/change
  -------- --  ---------------------------  -------- -----------------------------------------------
  05.09.07 pk                                         Initial Revision
  09.11.07 pk                               TN3924    Steps to mm
  29.02.08 wl  IGripZTravelManager          TN4032    New function SetToRackMove
  12.09.09 wl                               TN4740   TipFloat-/TPosMM-/TipInteger-/MPOSArray durch TDoubleArray/TIntArray ersetzt
  02.02.11 wl  IRackMoveManager             TN5791   verwendet TArray<TRackPosition> statt Rack und TIntArray
  10.09.12 ts  IGripZTravelManager          TN5977   SetToRackMove - new: RackMoveZTravelOffset
  23.05.13 wl  IRackMoveManager             TN6153   entfernt
  -------------------------------------------------------------------------------------------------- }

unit IntfZTravelManager;


interface


uses
    CommonTypes;

type
    IZTravelManager = interface(IInterface)
        ['{27438767-0D24-4965-807A-AAEAF104288F}']
        function Calculate(): TPosMM;
    end;

    IGripZTravelManager = interface(IZTravelManager)
        ['{BDCB637A-7235-41C4-86D1-76DF10AAA81A}']
        procedure Reset();
        procedure SetToTubeMove(aTubeZ: TPosMM);
        procedure SetToRackMove(aRackMoveZTravelOffset: TPosMM);
        procedure SetToTool;
        procedure SetToPipTool(aTipTypeRelLength_mm: TPosMM);
    end;


implementation


end.
