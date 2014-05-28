{ --------------------------------------------------------------------------------------------------
  Copyright © 2008 Zinsser Analytic GmbH - All rights reserved.
  Author       : Payman Kamali (pk)
  Description  : Classes and math functions for Coordinate System Relation
  --------------------------------------------------------------------------------------------------
  Revision History:
  date     op  method                       track-no improvement/change
  -------- --  ---------------------------  -------- -----------------------------------------------
  23.06.08 pk                                TN4139  Initial Revision
  10.03.09 pk                                TN4457  Changes needed for reimplementing Stacker Buttons
  10.04.13 wl                                TN6045   uses geändert
  -------------------------------------------------------------------------------------------------- }

unit CarrierSlotGraphics;


interface


uses
    LayoutElementGraphicsInfo,
    LayoutElementGraphics,
    IntfLayoutElementGraphicsDriver,
    LayoutElementCallBackTypes;

type
    TCarrierSlotGraphics = class(TLayoutElementGraphics)
    protected
        function GetGraphicsDriverID: TLayoutElementGraphicsDriverID; override;
        function GetGraphicsInfo: TCarrierSlotGraphicsInfo;
        procedure DoInitGraphicsInfo; override;
    public
        constructor Create();
        destructor Destroy; override;
        property GraphicsInfo: TCarrierSlotGraphicsInfo read GetGraphicsInfo;
    end;


implementation


uses
    Graphics;

{ TCarrierSlotGraphics }
constructor TCarrierSlotGraphics.Create;
begin
    inherited Create();
    self.Color := clSilver;
    self.BorderType := gbtSunken;
end;

destructor TCarrierSlotGraphics.Destroy();
begin
    inherited;
end;

function TCarrierSlotGraphics.GetGraphicsDriverID: TLayoutElementGraphicsDriverID;
begin
    result := ICarrierSlotGraphicsDriver;
end;

procedure TCarrierSlotGraphics.DoInitGraphicsInfo();
begin
    fGraphicsInfo := TCarrierSlotGraphicsInfo.Create();
end;

function TCarrierSlotGraphics.GetGraphicsInfo: TCarrierSlotGraphicsInfo;
begin
    result := fGraphicsInfo as TCarrierSlotGraphicsInfo;
end;


end.
