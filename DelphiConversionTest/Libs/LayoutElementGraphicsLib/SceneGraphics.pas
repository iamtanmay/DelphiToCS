unit SceneGraphics;


{ --------------------------------------------------------------------------------------------------
  Copyright © 2008 Zinsser Analytic GmbH - All rights reserved.
  Author       : Payman Kamali (pk)
  Description  : Classes and math functions for Coordinate System Relation
  --------------------------------------------------------------------------------------------------
  Revision History:
  date     op  method                       track-no improvement/change
  -------- --  ---------------------------  -------- -----------------------------------------------
  23.06.08 pk                                TN4139  Initial Revision
  16.07.08 pk  SceneChanged                  TN4139  Removed
  04.08.08 pk  PanX, PanY                    TN4139  New
  17.04.09 pk  SetCameraMode, FitToScreen    TN4532  Now public property
  -------------------------------------------------------------------------------------------------- }
interface


uses
    LayoutElementGraphics,
    LayoutElementGraphicsInfo,
    RackWellGraphics,
    IntfSceneGraphics,
    IntfLayoutElementGraphicsDriver;

type

    TSceneGraphics = class(TLayoutElementGraphics)
    private
        fPanX, fPanY: double;
        fZoom: double;
        fBackgroundGraphicsParent: TObject;
        function GetSceneGraphicsDriver: ISceneGraphicsDriver;
        procedure AssignDriverBackgroundParent();
        function GetZoom: double;
        procedure SetZoom(const aValue: double);
        procedure SetPanX(const aValue: double);
        function GetPanX(): double;
        procedure SetPanY(const aValue: double);
        function GetPanY(): double;
    protected
        function GetGraphicsDriverID: TLayoutElementGraphicsDriverID; override;
        procedure DoInitGraphicsDriver(); override;
        procedure AssignDriverParent(); override;
        procedure DoInitGraphicsInfo; override;
    public
        constructor Create(aBackgroundGraphicsParent: TObject);
        destructor Destroy(); override;
        procedure FitToScreen();
        procedure SetCameraMode(const aMode: TSceneGraphicsCameraMode);
        property PanX: double read GetPanX write SetPanX;
        property PanY: double read GetPanY write SetPanY;
        property Zoom: double read GetZoom write SetZoom;
    end;


implementation


uses
    SysUtils;

constructor TSceneGraphics.Create(aBackgroundGraphicsParent: TObject);
begin
    inherited Create();
    fBackgroundGraphicsParent := aBackgroundGraphicsParent;
    fColor := $0099A8AC;
    fZoom := 1.0;
    fPanX := 0;
    fPanY := 0;
end;

destructor TSceneGraphics.Destroy;
begin
    self.Visible := false;
    self.SceneChanged();
    fBackgroundGraphicsParent := nil;
    AssignDriverBackgroundParent();
    inherited;
end;

function TSceneGraphics.GetGraphicsDriverID: TLayoutElementGraphicsDriverID;
begin
    result := ISceneGraphicsDriver;
end;

function TSceneGraphics.GetSceneGraphicsDriver(): ISceneGraphicsDriver;
var
    xSceneGraphicsDriver: ISceneGraphicsDriver;
begin
    result := nil;
    if Supports(fGraphicsDriver, ILayoutElementGraphicsDriver, xSceneGraphicsDriver) then
        result := xSceneGraphicsDriver;

end;

procedure TSceneGraphics.AssignDriverBackgroundParent();
var
    xSceneGraphicsDriver: ISceneGraphicsDriver;
begin
    if Supports(fGraphicsDriver, ILayoutElementGraphicsDriver, xSceneGraphicsDriver) then
        xSceneGraphicsDriver.SetBackgroundParent(fBackgroundGraphicsParent);
end;

procedure TSceneGraphics.DoInitGraphicsDriver();
begin
    inherited;
    AssignDriverBackgroundParent();
    self.Zoom := fZoom;
    self.PanX := fPanX;
    self.PanY := fPanY;
end;

procedure TSceneGraphics.AssignDriverParent;
begin
end;

procedure TSceneGraphics.DoInitGraphicsInfo();
begin
    fGraphicsInfo := TLayoutElementGraphicsInfo.Create();
    fGraphicsInfo.SizeX := 10000;
    fGraphicsInfo.SizeY := 10000;
    fGraphicsInfo.SizeZ := 10;
    fGraphicsInfo.IsSizeVirtual := true;
end;

function TSceneGraphics.GetZoom: double;
var
    xSceneGraphicsDriver: ISceneGraphicsDriver;
begin
    xSceneGraphicsDriver := GetSceneGraphicsDriver();
    if Assigned(xSceneGraphicsDriver) then
    begin
        fZoom := xSceneGraphicsDriver.Zoom;
    end;

    result := fZoom;
end;

procedure TSceneGraphics.SetZoom(const aValue: double);
var
    xSceneGraphicsDriver: ISceneGraphicsDriver;
begin
    fZoom := aValue;
    xSceneGraphicsDriver := GetSceneGraphicsDriver();
    if Assigned(xSceneGraphicsDriver) then
        xSceneGraphicsDriver.Zoom := fZoom;
end;

function TSceneGraphics.GetPanX: double;
var
    xSceneGraphicsDriver: ISceneGraphicsDriver;
begin
    xSceneGraphicsDriver := GetSceneGraphicsDriver();
    if Assigned(xSceneGraphicsDriver) then
    begin
        fPanX := xSceneGraphicsDriver.PanX;
    end;

    result := fPanX;
end;

function TSceneGraphics.GetPanY: double;
var
    xSceneGraphicsDriver: ISceneGraphicsDriver;
begin
    xSceneGraphicsDriver := GetSceneGraphicsDriver();
    if Assigned(xSceneGraphicsDriver) then
    begin
        fPanY := xSceneGraphicsDriver.PanY;
    end;

    result := fPanY;
end;

procedure TSceneGraphics.SetPanX(const aValue: double);
var
    xSceneGraphicsDriver: ISceneGraphicsDriver;
begin
    fPanX := aValue;
    xSceneGraphicsDriver := GetSceneGraphicsDriver();
    if Assigned(xSceneGraphicsDriver) then
        xSceneGraphicsDriver.PanX := fPanX;
end;

procedure TSceneGraphics.SetPanY(const aValue: double);
var
    xSceneGraphicsDriver: ISceneGraphicsDriver;
begin
    fPanY := aValue;
    xSceneGraphicsDriver := GetSceneGraphicsDriver();
    if Assigned(xSceneGraphicsDriver) then
        xSceneGraphicsDriver.PanY := fPanY;
end;

procedure TSceneGraphics.FitToScreen();
var
    xSceneGraphicsDriver: ISceneGraphicsDriver;
begin
    xSceneGraphicsDriver := GetSceneGraphicsDriver();
    if Assigned(xSceneGraphicsDriver) then
        xSceneGraphicsDriver.FitToScreen();
end;

procedure TSceneGraphics.SetCameraMode(const aMode: TSceneGraphicsCameraMode);
var
    xSceneGraphicsDriver: ISceneGraphicsDriver;
begin
    xSceneGraphicsDriver := GetSceneGraphicsDriver();
    if Assigned(xSceneGraphicsDriver) then
        xSceneGraphicsDriver.SetCameraMode(aMode);
end;


end.
