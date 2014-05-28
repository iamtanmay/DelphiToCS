{ --------------------------------------------------------------------------------------------------
  Copyright © 2008 Zinsser Analytic GmbH - All rights reserved.
  Author       : Payman Kamali (pk)
  Description  : Classes and math functions for Coordinate System Relation
  --------------------------------------------------------------------------------------------------
  Revision History:
  date     op  method                       track-no improvement/change
  -------- --  ---------------------------  -------- -----------------------------------------------
  23.06.08 pk                                TN4139  Initial Revision
  19.08.09 wl                                TN4702   Strings werden jetzt direkt geladen
  04.11.09 pk                                TN4843  Replace TObjectList, TStringList, TStrings with Generics Lists and TStringArray
  21.03.13 wl                                TN6045   uses Generics.Collections
  -------------------------------------------------------------------------------------------------- }

unit WorkspaceGraphics;


interface


uses
    Generics.Collections,
    LayoutElementGraphicsInfo,
    CarrierGraphics,
    LayoutElementGraphics,
    IntfLayoutElementGraphicsDriver;

type
    {
      TArmBoundGraphics = class( TPanel )
      protected
      fArmBoundInfo : TArmBoundInfo;
      fArmBoundGraphics : TArmBoundGraphics;
      public
      constructor Create( aArmBoundInfo : TArmBoundInfo ); reintroduce;
      destructor Destroy(); override;
      procedure UpdateBounds( aOffset : TWBBorder; aZoom: single );
      end;
    }
    TWorkspaceGraphics = class(TLayoutElementGraphics)
    private
        fCarriers: TObjectList<TCarrierGraphics>;
        function GetGraphicsInfo: TWorkspaceGraphicsInfo;
    protected
        function GetGraphicsDriverID: TLayoutElementGraphicsDriverID; override;
        procedure DoInitGraphicsInfo(); override;
    public
        constructor Create();
        destructor Destroy; override;
        procedure AddCarrier(aCarrierGraphics: TCarrierGraphics);
        property GraphicsInfo: TWorkspaceGraphicsInfo read GetGraphicsInfo;
    end;


implementation


uses
    SysUtils;

{ TWorkspaceGraphics }

constructor TWorkspaceGraphics.Create();
begin
    inherited Create();
    fCarriers := TObjectList<TCarrierGraphics>.Create(false);
end;

destructor TWorkspaceGraphics.Destroy();
begin
    FreeAndNil(fCarriers);
    inherited;
end;

function TWorkspaceGraphics.GetGraphicsDriverID: TLayoutElementGraphicsDriverID;
begin
    result := IWorkspaceGraphicsDriver;
end;

procedure TWorkspaceGraphics.AddCarrier(aCarrierGraphics: TCarrierGraphics);
begin
    fCarriers.Add(aCarrierGraphics);
    aCarrierGraphics.Parent := self;
end;

function TWorkspaceGraphics.GetGraphicsInfo: TWorkspaceGraphicsInfo;
begin
    result := fGraphicsInfo as TWorkspaceGraphicsInfo;
end;

procedure TWorkspaceGraphics.DoInitGraphicsInfo;
begin
    fGraphicsInfo := TWorkspaceGraphicsInfo.Create();
end;


end.
