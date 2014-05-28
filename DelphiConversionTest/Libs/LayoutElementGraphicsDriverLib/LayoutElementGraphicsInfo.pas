unit LayoutElementGraphicsInfo;
{ --------------------------------------------------------------------------------------------------
  Copyright © 2008 Zinsser Analytic GmbH - All rights reserved.
  Author       : Payman Kamali (pk)
  Description  :
  --------------------------------------------------------------------------------------------------
  Revision History:
  date     op  method                       track-no improvement/change
  -------- --  ---------------------------  -------- -----------------------------------------------
  23.06.08 pk                                TN4139  Initial Revision
  10.03.09 pk                                TN4457  Changes needed for reimplementing Stacker Buttons
  17.04.09 pk  IsSizeVirtual                 TN4532  New
  28.10.10 wl  TRackGraphicsInfo             TN5312  TubeDispColor entfernt
  -------------------------------------------------------------------------------------------------- }


interface


type
    TBounds = record
        X1, X2, Y1, Y2, Z1, Z2: double;
    end;

    TLayoutElementGraphicsInfo = class
        name: string;
        SizeX, SizeY, SizeZ: double;
        IsSizeVirtual: boolean;
    end;

    TLayoutGraphicsInfo = class(TLayoutElementGraphicsInfo)

    end;

    TWorkspaceGraphicsInfo = class(TLayoutElementGraphicsInfo)
    end;

    TCarrierSlotShape = (cssRect, cssCircle);

    TCarrierGraphicsInfo = class(TLayoutElementGraphicsInfo)
        Rows: integer;
        Cols: integer;
        Levels: integer;

        SlotSizeX: double;
        SlotSizeY: double;
        SlotSizeZ: double;

        SlotFirstX: double;
        SlotFirstY: double;
        SlotFirstZ: double; // bisher: SlotZ_Last_mm

        SlotLastX: double;
        SlotLastY: double;
        SlotLastZ: double; // bisher: SlotZ_First_mm

        SlotZ_Calculate: boolean;
        SlotShape: TCarrierSlotShape;

        SlotRotation_Degree: double;
        Stackerbuttons: integer;
    end;

    TCarrierSlotGraphicsInfo = class(TLayoutElementGraphicsInfo)
    end;

    TRackShape = (rsRect, rsCircle);
    TRackWellShape = (rwsRect, rwsCircle);

    TRackGraphicsInfo = class(TLayoutElementGraphicsInfo)

        Rows, Cols: integer;

        TubePosX_First, TubePosY_First, TubePosX_Last, TubePosY_Last: double;

        TubeSizeZ: double;
        WellZOffset: double;
        RackShape: TRackShape;
        SlantDeg: double;

        IsMultiTipRack: boolean;
        IsTubeRectangle: boolean;
    end;

    TRackWellGraphicsInfo = class(TLayoutElementGraphicsInfo)
        WellShape: TRackWellShape;
    end;

    TRubberBandGraphicsInfo = class(TLayoutElementGraphicsInfo)
        PosX: double;
        PosY: double;
    end;

    TBasicControlGraphicsInfo = class(TLayoutElementGraphicsInfo);

    TPanelControlGraphicsInfo = class(TBasicControlGraphicsInfo)
    end;

    TButtonControlGraphicsInfo = class(TBasicControlGraphicsInfo)
    end;


implementation


end.
