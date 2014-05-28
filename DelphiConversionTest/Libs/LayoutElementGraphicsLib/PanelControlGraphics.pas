unit PanelControlGraphics;


{ --------------------------------------------------------------------------------------------------
  Copyright © 2009 Zinsser Analytic GmbH - All rights reserved.
  Author       : Payman Kamali (pk)
  Description  :
  --------------------------------------------------------------------------------------------------
  Revision History:
  date     op  method                       track-no improvement/change
  -------- --  ---------------------------  -------- -----------------------------------------------

  -------------------------------------------------------------------------------------------------- }
interface


uses
    LayoutElementGraphics,
    LayoutElementGraphicsInfo,
    BasicControlGraphics,
    IntfLayoutElementGraphicsDriver;

type

    TPanelControlGraphics = class(TBasicControlGraphics)
    protected
        function GetGraphicsDriverID: TLayoutElementGraphicsDriverID; override;
        function GetGraphicsInfo: TPanelControlGraphicsInfo;
        procedure DoInitGraphicsInfo; override;
    public
        constructor Create();
        destructor Destroy(); override;
        procedure ChangePanelControlColor(aColor: integer);
        property GraphicsInfo: TPanelControlGraphicsInfo read GetGraphicsInfo;
    end;

const
    cPanelControlColorDefault = $808080; // clgray;


implementation


constructor TPanelControlGraphics.Create();
begin
    inherited;
    Color := cPanelControlColorDefault;
end;

destructor TPanelControlGraphics.Destroy;
begin
    inherited;
end;

procedure TPanelControlGraphics.ChangePanelControlColor(aColor: integer);
begin
    Color := aColor;
end;

function TPanelControlGraphics.GetGraphicsDriverID: TLayoutElementGraphicsDriverID;
begin
    result := IPanelControlGraphicsDriver;
end;

function TPanelControlGraphics.GetGraphicsInfo: TPanelControlGraphicsInfo;
begin
    result := fGraphicsInfo as TPanelControlGraphicsInfo;
end;

procedure TPanelControlGraphics.DoInitGraphicsInfo;
begin
    fGraphicsInfo := TPanelControlGraphicsInfo.Create();
end;


end.
