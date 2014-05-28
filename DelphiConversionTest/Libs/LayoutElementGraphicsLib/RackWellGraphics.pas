{ --------------------------------------------------------------------------------------------------
  Copyright © 2008 Zinsser Analytic GmbH - All rights reserved.
  Author       : Payman Kamali (pk)
  Description  : Classes and math functions for Coordinate System Relation
  --------------------------------------------------------------------------------------------------
  Revision History:
  date     op  method                       track-no improvement/change
  -------- --  ---------------------------  -------- -----------------------------------------------
  23.06.08 pk                                TN4139  Initial Revision
  02.07.08 pk  ChangeColorByCode             TN4139  New
  07.07.08 pk  DoUpdateGraphicsInfo          TN4139  New: needed for udpating wellshape
  10.03.09 pk                                TN4457  Changes needed for reimplementing Stacker Buttons
  21.07.10 pk  ChangeColorByCode             TN5066  Changed to Changecolor, code moved to Rack.pas
  20.09.11 wl                                TN5723   aufgeräumt
  20.09.11 wl  Highlighted                   TN5723   neu
  28.10.11 wl  ChangeColor                   TN5725   entfernt
  -------------------------------------------------------------------------------------------------- }

unit RackWellGraphics;


interface


uses
    LayoutElementGraphics,
    LayoutElementGraphicsInfo,
    IntfLayoutElementGraphicsDriver,
    LayoutElementCallBackTypes;

type
    TRackWellGraphics = class(TLayoutElementGraphics)
    private
        fHighlighted: boolean;
        function GetGraphicsInfo: TRackWellGraphicsInfo;
        procedure SetHighlighted(const Value: boolean);
    protected
        function GetGraphicsDriverID: TLayoutElementGraphicsDriverID; override;
        procedure DoInitGraphicsInfo(); override;
        procedure DoUpdateGraphicsInfo; override;
    public
        constructor Create();
        destructor Destroy(); override;
        property GraphicsInfo: TRackWellGraphicsInfo read GetGraphicsInfo;
        property Highlighted: boolean read fHighlighted write SetHighlighted;
    end;


implementation


uses
    Graphics;

{ TRackWellGraphics }

constructor TRackWellGraphics.Create();
begin
    inherited Create();
    self.BorderType := gbtSunken;
    fHighlighted := false;
end;

destructor TRackWellGraphics.Destroy;
begin
    inherited;
end;

function TRackWellGraphics.GetGraphicsDriverID: TLayoutElementGraphicsDriverID;
begin
    result := IRackWellGraphicsDriver;
end;

function TRackWellGraphics.GetGraphicsInfo: TRackWellGraphicsInfo;
begin
    result := fGraphicsInfo as TRackWellGraphicsInfo;
end;

procedure TRackWellGraphics.SetHighlighted(const Value: boolean);
begin
    fHighlighted := Value;
    if fHighlighted then
        self.BorderType := gbtRaised
    else
        self.BorderType := gbtSunken;
end;

procedure TRackWellGraphics.DoInitGraphicsInfo;
begin
    fGraphicsInfo := TRackWellGraphicsInfo.Create();
end;

procedure TRackWellGraphics.DoUpdateGraphicsInfo;
begin
    inherited;
    if not Assigned(fGraphicsDriver) then
        EXIT;
    with fGraphicsDriver.GraphicsInfo as TRackWellGraphicsInfo do
    begin
        WellShape := self.GraphicsInfo.WellShape;
    end;
end;


end.
