{ --------------------------------------------------------------------------------------------------
  Copyright © 2008 Zinsser Analytic GmbH - All rights reserved.
  Author       : Payman Kamali (pk)
  Description  : Classes and math functions for Coordinate System Relation
  --------------------------------------------------------------------------------------------------
  Revision History:
  date     op  method                       track-no improvement/change
  -------- --  ---------------------------  -------- -----------------------------------------------
  23.06.08 pk                                TN4139  Initial Revision
  20.09.11 wl  AddWell,ChangeTubeColor       TN5723   entfernt
  -------------------------------------------------------------------------------------------------- }

unit RackGraphics;


interface


uses
    LayoutElementGraphics,
    LayoutElementGraphicsInfo,
    RackWellGraphics,
    IntfLayoutElementGraphicsDriver;

type

    TRackGraphics = class(TLayoutElementGraphics)
    protected
        function GetGraphicsDriverID: TLayoutElementGraphicsDriverID; override;
        function GetGraphicsInfo: TRackGraphicsInfo;
        procedure DoInitGraphicsInfo; override;
    public const
        cRackColorDefault = $808080; // clgray;
    public
        constructor Create();
        destructor Destroy(); override;
        procedure ChangeRackColor(aColor: integer);
        property GraphicsInfo: TRackGraphicsInfo read GetGraphicsInfo;
    end;


implementation


constructor TRackGraphics.Create();
begin
    inherited;
    Color := cRackColorDefault;
end;

destructor TRackGraphics.Destroy;
begin
    inherited;
end;

procedure TRackGraphics.ChangeRackColor(aColor: integer);
begin
    Color := aColor;
end;

function TRackGraphics.GetGraphicsDriverID: TLayoutElementGraphicsDriverID;
begin
    result := IRackGraphicsDriver;
end;

function TRackGraphics.GetGraphicsInfo: TRackGraphicsInfo;
begin
    result := fGraphicsInfo as TRackGraphicsInfo;
end;

procedure TRackGraphics.DoInitGraphicsInfo;
begin
    fGraphicsInfo := TRackGraphicsInfo.Create();
end;


end.
