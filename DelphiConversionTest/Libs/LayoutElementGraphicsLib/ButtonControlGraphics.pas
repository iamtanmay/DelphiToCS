unit ButtonControlGraphics;


{ --------------------------------------------------------------------------------------------------
  Copyright © 2008 Zinsser Analytic GmbH - All rights reserved.
  Author       : Payman Kamali (pk)
  Description  : Classes and math functions for Coordinate System Relation
  --------------------------------------------------------------------------------------------------
  Revision History:
  date     op  method                       track-no improvement/change
  -------- --  ---------------------------  -------- -----------------------------------------------
  23.06.08 pk                                TN4139  Initial Revision
  -------------------------------------------------------------------------------------------------- }
interface


uses
    LayoutElementGraphics,
    LayoutElementGraphicsInfo,
    BasicControlGraphics,
    IntfLayoutElementGraphicsDriver;

type

    TButtonControlGraphics = class(TBasicControlGraphics)
    protected
        function GetGraphicsDriverID: TLayoutElementGraphicsDriverID; override;
        function GetGraphicsInfo: TButtonControlGraphicsInfo;
        procedure DoInitGraphicsInfo; override;
    public
        constructor Create();
        destructor Destroy(); override;
        procedure ChangeButtonControlColor(aColor: integer);
        property GraphicsInfo: TButtonControlGraphicsInfo read GetGraphicsInfo;
    end;

const
    cButtonControlColorDefault = $808080; // clgray;


implementation


constructor TButtonControlGraphics.Create();
begin
    inherited;
    Color := cButtonControlColorDefault;
end;

destructor TButtonControlGraphics.Destroy;
begin
    inherited;
end;

procedure TButtonControlGraphics.ChangeButtonControlColor(aColor: integer);
begin
    Color := aColor;
end;

function TButtonControlGraphics.GetGraphicsDriverID: TLayoutElementGraphicsDriverID;
begin
    result := IButtonControlGraphicsDriver;
end;

function TButtonControlGraphics.GetGraphicsInfo: TButtonControlGraphicsInfo;
begin
    result := fGraphicsInfo as TButtonControlGraphicsInfo;
end;

procedure TButtonControlGraphics.DoInitGraphicsInfo;
begin
    fGraphicsInfo := TButtonControlGraphicsInfo.Create();
end;


end.
