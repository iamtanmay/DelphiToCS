{ -----------------------------------------------------------------------------------------------------------------------
  Copyright © 2008 Zinsser Analytic GmbH - All rights reserved.
  Author       : Payman Kamali (pk)
  Description  : Classes and math functions for Coordinate System Relation
  -----------------------------------------------------------------------------------------------------------------------
  Revision History:
  date     op  method                       track-no improvement/change
  -------- --  ---------------------------  -------- --------------------------------------------------------------------
  23.06.08 pk                                TN4139  Initial Revision
  07.07.08 pk  ArmCount, UseAllArms          TN4139  new properties
  16.07.08 wl  DeviceNames, UseAllDevices    TN4164   replaces ArmCount, UseAllArms
  09.07.09 pk  SetType                       TN4644  World relation is now used to calculate physical positions
  04.11.09 pk                                TN4843  Replace TObjectList, TStringList, TStrings with Generics Lists and TStringArray
  04.15.10 pk  MakeHintText                  TN5050  adds text from fParentMakeHintTextCallback to Hint
  27.03.13 wl                                TN6045   uses geändert
  ----------------------------------------------------------------------------------------------------------------------- }

unit Workspace;


interface


uses
    Generics.Collections,
    LayoutElement,
    WorkspaceGraphics,
    WorkspaceDataAdaptor,
    CoordSystemMath,
    WorkspaceDevicesDataAdaptor;

type
    TWorkspace = class(TLayoutElement)
    private
        function GetGraphics: TWorkspaceGraphics;
        function GetCurrentViewCoordSystem: TCoordSystemRelation;
        procedure SetUseCustomView(const aValue: boolean);
        procedure SetUseWorldView(const aValue: boolean);
        procedure UpdateGraphicsInfo;
        function GetUseAllDevices: boolean;
    protected
        fLayout: TObject;
        fID: integer;
        fIsLinked: boolean;
        fWorkspaceTypeName: string;
        fWorkspaceTypeID: integer;
        fUseCustomView: boolean;
        fUseWorldView: boolean;

        fCustomViewCoordSystem, fDefaultViewCoordSystem, fWorldCoordSystem: TCoordSystemRelation;
        fTypeRec: TWorkspaceRec;
        fDeviceNames: TList<string>;
        fParentMakeHintTextCallback: TParentMakeHintTextCallback;

        procedure DoInitGraphics(); override;
        procedure ViewChanged();
        function MakeHintText: string; override;
    public
        constructor Create();
        destructor Destroy(); override;

        procedure SetType(const aTypeRec: TWorkspaceRec; const aDevicesRecs: TWorkspaceDevicesRecArray);
        procedure ChangeView(aUseCustomView, aUseWorldView: boolean);
        class procedure AssignCoordSystemRelationFromRec(aCoordSystemRelation: TCoordSystemRelation;
            const aRec: TCoordSystemRelationRec);
        class procedure AssignCoordSystemRelationToRec(aCoordSystemRelation: TCoordSystemRelation;
            var vRec: TCoordSystemRelationRec);

        property ID: integer read fID write fID;
        property WorkspaceTypeID: integer read fWorkspaceTypeID;
        property WorkspaceTypeName: string read fWorkspaceTypeName write fWorkspaceTypeName;
        property Layout: TObject read fLayout write fLayout;
        property IsLinked: boolean read fIsLinked write fIsLinked;
        property Graphics: TWorkspaceGraphics read GetGraphics;
        property UseCustomView: boolean read fUseCustomView write SetUseCustomView;
        property UseWorldView: boolean read fUseWorldView write SetUseWorldView;
        property CustomViewCoordSystem: TCoordSystemRelation read fCustomViewCoordSystem;
        property DefaultViewCoordSystem: TCoordSystemRelation read fDefaultViewCoordSystem;
        property WorldCoordSystem: TCoordSystemRelation read fWorldCoordSystem;
        property CurrentViewCoordSystem: TCoordSystemRelation read GetCurrentViewCoordSystem;
        property UseAllDevices: boolean read GetUseAllDevices;
        property DeviceNames: TList<string>read fDeviceNames;
        property ParentMakeHintTextCallback: TParentMakeHintTextCallback read fParentMakeHintTextCallback
            write fParentMakeHintTextCallback;
    end;


implementation


uses
    SysUtils;

{ TWorkspace }

constructor TWorkspace.Create();
begin
    inherited Create();

    fDeviceNames := TList<string>.Create;
    fID := 0;
    fCustomViewCoordSystem := TCoordSystemRelation.Create();
    fDefaultViewCoordSystem := TCoordSystemRelation.Create();
    fWorldCoordSystem := TCoordSystemRelation.Create();
    fParentMakeHintTextCallback := nil;
    // fArmBoundGroup := TArmBoundGroup.Create();
end;

destructor TWorkspace.Destroy();
begin
    fParentMakeHintTextCallback := nil;
    fCustomViewCoordSystem.Free;
    fDefaultViewCoordSystem.Free;
    fWorldCoordSystem.Free;
    fDeviceNames.Free;

    inherited;
end;

procedure TWorkspace.DoInitGraphics;
begin
    fGraphics := TWorkspaceGraphics.Create();
end;

function TWorkspace.GetGraphics: TWorkspaceGraphics;
begin
    result := fGraphics as TWorkspaceGraphics;
end;

function TWorkspace.MakeHintText(): string;
begin
    AddHintLine(Format('Workspace: [%s]', [fName]), result);
    AddHintLine(Format('Type: %s', [fWorkspaceTypeName]), result);
    if Assigned(fParentMakeHintTextCallback) then
        AddHintLine(fParentMakeHintTextCallback(self), result);
end;

class procedure TWorkspace.AssignCoordSystemRelationFromRec(aCoordSystemRelation: TCoordSystemRelation;
    const aRec: TCoordSystemRelationRec);
begin
    with aCoordSystemRelation do
    begin
        TranslateX := aRec.TranslateX;
        TranslateY := aRec.TranslateY;
        TranslateZ := aRec.TranslateZ;
        ReflectX := aRec.ReflectX;
        ReflectY := aRec.ReflectY;
        ReflectZ := aRec.ReflectZ;
        RotateX := aRec.RotateX;
        RotateY := aRec.RotateY;
        RotateZ := aRec.RotateZ;
    end;
end;

class procedure TWorkspace.AssignCoordSystemRelationToRec(aCoordSystemRelation: TCoordSystemRelation;
    var vRec: TCoordSystemRelationRec);
begin
    with aCoordSystemRelation do
    begin
        vRec.TranslateX := TranslateX;
        vRec.TranslateY := TranslateY;
        vRec.TranslateZ := TranslateZ;
        vRec.ReflectX := ReflectX;
        vRec.ReflectY := ReflectY;
        vRec.ReflectZ := ReflectZ;
        vRec.RotateX := RotateX;
        vRec.RotateY := RotateY;
        vRec.RotateZ := RotateZ;
    end;
end;

function TWorkspace.GetCurrentViewCoordSystem(): TCoordSystemRelation;
begin
    if fUseCustomView then
        result := fCustomViewCoordSystem
    else
    begin
        if fUseWorldView then
            result := fWorldCoordSystem
        else
            result := fDefaultViewCoordSystem;
    end;
end;

procedure TWorkspace.ViewChanged;
var
    xViewCoordSystemRelation: TCoordSystemRelation;
begin
    xViewCoordSystemRelation := GetCurrentViewCoordSystem();
    self.Graphics.CoordSystem.Assign(xViewCoordSystemRelation);
end;

procedure TWorkspace.ChangeView(aUseCustomView, aUseWorldView: boolean);
begin
    fUseCustomView := aUseCustomView;
    fUseWorldView := aUseWorldView;
    ViewChanged();
end;

procedure TWorkspace.SetUseCustomView(const aValue: boolean);
begin
    if fUseCustomView = aValue then
        EXIT;
    fUseCustomView := aValue;
    ViewChanged();
end;

procedure TWorkspace.SetUseWorldView(const aValue: boolean);
begin
    if fUseWorldView = aValue then
        EXIT;
    fUseWorldView := aValue;
    ViewChanged();
end;

procedure TWorkspace.UpdateGraphicsInfo();
begin
    self.Graphics.GraphicsInfo.Name := fName;
    self.Graphics.GraphicsInfo.SizeX := fTypeRec.X;
    self.Graphics.GraphicsInfo.SizeY := fTypeRec.Y;
    self.Graphics.GraphicsInfo.SizeZ := fTypeRec.Z;
    self.Graphics.Color := fTypeRec.Color;
    self.Graphics.UpdateGraphicsInfo();
end;

procedure TWorkspace.SetType(const aTypeRec: TWorkspaceRec; const aDevicesRecs: TWorkspaceDevicesRecArray);
var
    x: integer;
begin
    fTypeRec := aTypeRec;

    fDeviceNames.Clear;
    for x := 0 to high(aDevicesRecs) do
    begin
        fDeviceNames.Add(aDevicesRecs[x].DeviceName);
    end;

    fWorkspaceTypeID := aTypeRec.ID;
    fWorkspaceTypeName := aTypeRec.Name;

    if aTypeRec.ViewRelation.Valid then
        AssignCoordSystemRelationFromRec(fDefaultViewCoordSystem, aTypeRec.ViewRelation);

    if aTypeRec.WorldRelation.Valid then
    begin
        AssignCoordSystemRelationFromRec(fWorldCoordSystem, aTypeRec.WorldRelation);
        // use the worldcoordsystem for calculating physical workspace/carrier/rack positions which will be used by motors
        self.CoordCalculator.CoordSystem.Assign(fWorldCoordSystem);
    end;

    fUseWorldView := not aTypeRec.ViewRelation.Valid;

    self.ViewChanged();
    UpdateGraphicsInfo();
end;

function TWorkspace.GetUseAllDevices: boolean;
begin
    result := fTypeRec.UseAllDevices;
end;


end.
