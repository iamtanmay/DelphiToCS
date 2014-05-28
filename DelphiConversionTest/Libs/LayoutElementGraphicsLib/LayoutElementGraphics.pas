unit LayoutElementGraphics;
{ --------------------------------------------------------------------------------------------------
  Copyright © 2008 Zinsser Analytic GmbH - All rights reserved.
  Author       : Payman Kamali (pk)
  Description  : Classes and math functions for Coordinate System Relation
  --------------------------------------------------------------------------------------------------
  Revision History:
  date     op  method                       track-no improvement/change
  -------- --  ---------------------------  -------- -----------------------------------------------
  23.06.08 pk                                TN4139  Initial Revision
  07.07.08 pk  GetName                       TN4139  New
  07.07.08 pk  SetParent                     TN4139  call AssignDriverParent
  16.07.08 pk  SceneChanged                  TN4139  New
  31.07.08 pk  Cursor                        TN4139  New
  10.03.09 pk                                TN4457  Changes needed for reimplementing Stacker Buttons
  13.03.09 pk                                TN4463  Changes needed for reimplementing carrier drag move
  13.03.09 pk                                TN4466  Changes need for implementing rack carrier slot change via drag and drop
  17.04.09 pk  DoUpdateGraphicsInfo          TN4532  New: IsSizeVirtual
  19.05.09 wl  GetLeft, GetTop               TN4466   entfernt
  12.08.09 pk  FinlaizeGraphicsDriver        TN4713  New
  29.10.10 pk  Invalidate                    TN5268  New
  15.11.10 pk                                TN5340  Changes to prevent memory leak
  17.11.10 wl  Destroy                       TN5354  FreeAndNil( fPopupMenuInfos ) wieder auskommentiert
  -------------------------------------------------------------------------------------------------- }


interface


uses

    MatrixMath,
    CoordSystemMath,
    IntfLayoutElementGraphicsDriver,
    LayoutElementCallbackTypes,
    LayoutElementGraphicsInfo,
    PopupMenuInfo;

type
    TLayoutElementGraphics = class
    private
        function GetName: string;
        procedure SetName(const aName: string);
        procedure SetParent(const aValue: TLayoutElementGraphics);
        procedure SetColor(const aValue: integer);
        procedure SetHint(const aValue: string);
        // function GetLeft: integer;
        // function GetTop: integer;
        procedure CreatePopupMenu;
        function GetCoordSystem: TCoordSystemRelation;
        procedure SetBorderType(const aValue: TGraphicsBorderType);
        procedure SetCaption(const aValue: string);
        procedure FinalizeGraphicsDriver();
    protected
        fName: string;
        fCoordCalculator: TCoordCalculator;
        fGraphicsInfo: TLayoutElementGraphicsInfo;
        fCallbacks: TLayoutElementGraphicsCallbacks;
        fGraphicsDriver: ILayoutElementGraphicsDriver;
        fTag: TObject;
        fParent: TLayoutElementGraphics;
        fVisible: boolean;
        fColor: integer;
        fHint: string;
        fCursor: TGraphicsMouseCursor;
        fPopupMenuInfos: TPopupMenuInfoList;
        fInitialized: boolean;
        fBorderType: TGraphicsBorderType;
        fCaption: string;
        procedure DoInitGraphicsDriver(); virtual;
        procedure DoInitGraphicsInfo(); virtual;
        procedure DoUpdateGraphicsInfo(); virtual;
        procedure AssignDriverParent(); virtual;
        function GetGraphicsDriverID(): TLayoutElementGraphicsDriverID; virtual;
        procedure SetVisible(aVisible: boolean); virtual;
        function GetPosX: double;
        function GetPosY: double;
        function GetPosZ: double;
        procedure SetPosX(aValue: double);
        procedure SetPosY(aValue: double);
        procedure SetPosZ(aValue: double);
        function GetBounds(): TBounds;
        function GetCursor(): TGraphicsMouseCursor;
        procedure SetCursor(const aValue: TGraphicsMouseCursor);
        procedure Invalidate(const aErase: boolean);
        procedure DoSceneChanged(); virtual;
        procedure DoPopup(aSender: TObject; aX, aY: double; var vPopupMenuInfos: TPopupMenuInfoList);
        procedure DoShowHint(aSender: TObject; var vShow: boolean; var vHintText: string);
        procedure DoMouseClick(aSender: TObject);
        procedure DoMouseDblClick(aSender: TObject);
        procedure DoMouseDown(aSender: TObject; aButton: TGraphicsMouseButton; aShift: TGraphicsShiftState;
            aX, aY: double);
        procedure DoMouseDragDrop(aSender, aSource: TObject; aX, aY: double);
        procedure DoMouseMove(aSender: TObject; aShift: TGraphicsShiftState; aX, aY: double);
        procedure DoMouseUp(aSender: TObject; aButton: TGraphicsMouseButton; aShift: TGraphicsShiftState;
            aX, aY: double);
        procedure DoMouseDragOver(aSender, aSource: TObject; aX, aY: double; aState: TGraphicsDragState;
            var vAccept: boolean);
        procedure DoPosChanged(aSender: TObject; const aX, aY, aZ: double);
        procedure DoIsDragable(aSender: TObject; var vIsDragable: boolean);
        property GraphicsDriver: ILayoutElementGraphicsDriver read fGraphicsDriver;

    public
        constructor Create();
        destructor Destroy(); override;
        procedure InitGraphicsDriver();
        procedure UpdateGraphicsInfo;
        procedure AddPopupMenuItem(aItem: TPopupMenuInfoItem);
        function SceneToClient(aX, aY, aZ: double): TPoint4d;
        function ClientToScene(aX, aY, aZ: double): TPoint4d;
        function ClientToScreen(aX, aY, aZ: double): TPoint4d;
        procedure SceneChanged();
        procedure PosChangeDragStart(const aRefX, aRefY, aRefZ: double);
        property name: string read GetName write SetName;
        property Parent: TLayoutElementGraphics read fParent write SetParent;
        property CoordSystem: TCoordSystemRelation read GetCoordSystem;
        property Visible: boolean read fVisible write SetVisible;
        property Tag: TObject read fTag write fTag;
        property Color: integer read fColor write SetColor;
        property Hint: string read fHint write SetHint;
        property Bounds: TBounds read GetBounds;
        // property Left : integer read GetLeft;
        // property Top : integer read GetTop;
        property Callbacks: TLayoutElementGraphicsCallbacks read fCallbacks;
        property PopupMenuInfos: TPopupMenuInfoList read fPopupMenuInfos;
        property PosX: double read GetPosX write SetPosX;
        property PosY: double read GetPosY write SetPosY;
        property PosZ: double read GetPosZ write SetPosZ;
        property Cursor: TGraphicsMouseCursor read GetCursor write SetCursor;
        property BorderType: TGraphicsBorderType read fBorderType write SetBorderType;
        property Caption: string read fCaption write SetCaption;
    end;


implementation


uses
    SysUtils,
    LayoutElementGraphicsDriverTypeManager;

{ TLayoutElementGraphics }

constructor TLayoutElementGraphics.Create;
begin
    inherited Create();
    fInitialized := false;
    fParent := nil;
    fPopupMenuInfos := TPopupMenuInfoList.Create();
    fGraphicsDriver := nil;
    fGraphicsInfo := nil;
    fCursor := cGraphicsMouseCursorDefault;
    fCoordCalculator := TCoordCalculator.Create();
    fCoordCalculator.RecalcParent := false;
    fBorderType := gbtRaised;
    fCaption := '';
    fCallbacks := TLayoutElementGraphicsCallbacks.Create();
    DoInitGraphicsInfo();
end;

destructor TLayoutElementGraphics.Destroy;
begin
    // FreeAndNil( fPopupMenuInfos );
    // fPopupMenuInfos.Free;
    FreeAndNil(fGraphicsInfo);
    FinalizeGraphicsDriver();

    // set to nil to free driver
    fGraphicsDriver := nil;

    fCoordCalculator.Free;
    fCallbacks.Free;
    inherited;
end;

function TLayoutElementGraphics.GetGraphicsDriverID: TLayoutElementGraphicsDriverID;
begin
    result := ILayoutElementGraphicsDriver;
end;

procedure TLayoutElementGraphics.DoInitGraphicsDriver;
begin
    fGraphicsDriver := TLayoutElementGraphicsDriverTypeManager.Instance.CreateGraphicsDriver
        (self.GetGraphicsDriverID());
end;

procedure TLayoutElementGraphics.InitGraphicsDriver();
begin
    if fInitialized then
        EXIT;
    DoInitGraphicsDriver();
    if not Assigned(fGraphicsDriver) then
        EXIT;
    fGraphicsDriver.Name := self.Name;
    fGraphicsDriver.Tag := self;
    fGraphicsDriver.CoordCalculator := fCoordCalculator;
    fGraphicsDriver.Callbacks.MouseClickCallback := DoMouseClick;
    fGraphicsDriver.Callbacks.MouseDblClickCallback := DoMouseDblClick;
    fGraphicsDriver.Callbacks.MouseMoveCallback := DoMouseMove;
    fGraphicsDriver.Callbacks.MouseDownCallback := DoMouseDown;
    fGraphicsDriver.Callbacks.MouseUpCallback := DoMouseUp;
    fGraphicsDriver.Callbacks.MouseDragOverCallback := DoMouseDragOver;
    fGraphicsDriver.Callbacks.MouseDragDropCallback := DoMouseDragDrop;
    fGraphicsDriver.Callbacks.IsDragableCallback := DoIsDragable;
    fGraphicsDriver.Callbacks.PopupCallback := DoPopup;
    fGraphicsDriver.Callbacks.ShowHintCallback := DoShowHint;
    fGraphicsDriver.Callbacks.PosChangedCallback := DoPosChanged;

    fGraphicsDriver.Color := fColor;
    fGraphicsDriver.Cursor := fCursor;
    fGraphicsDriver.BorderType := fBorderType;
    fGraphicsDriver.Caption := fCaption;
    AssignDriverParent();

    CreatePopupMenu();

    DoUpdateGraphicsInfo();

    fGraphicsDriver.Visible := fVisible;

    fInitialized := true;

end;

procedure TLayoutElementGraphics.FinalizeGraphicsDriver;
begin
    if Assigned(fGraphicsDriver) then
    begin
        fGraphicsDriver.Visible := false;
        fGraphicsDriver.Parent := nil;
        fGraphicsDriver.Finalize;
    end;

end;

procedure TLayoutElementGraphics.AssignDriverParent();
begin
    if not Assigned(fGraphicsDriver) then
        EXIT;

    if Assigned(fParent) then
        fGraphicsDriver.Parent := fParent.GraphicsDriver
    else
        fGraphicsDriver.Parent := nil;
end;

procedure TLayoutElementGraphics.SetParent(const aValue: TLayoutElementGraphics);
begin
    fParent := aValue;
    if fInitialized then
        AssignDriverParent();
end;

procedure TLayoutElementGraphics.CreatePopupMenu();
begin
end;

procedure TLayoutElementGraphics.SetVisible(aVisible: boolean);
begin
    fVisible := aVisible;

    if aVisible then
        InitGraphicsDriver();

    if Assigned(fGraphicsDriver) then
        fGraphicsDriver.Visible := aVisible;
end;

procedure TLayoutElementGraphics.SetColor(const aValue: integer);
begin
    fColor := aValue;
    if Assigned(fGraphicsDriver) then
        fGraphicsDriver.Color := aValue;
end;

procedure TLayoutElementGraphics.SetHint(const aValue: string);
begin
    fHint := aValue;
end;

{
  function TLayoutElementGraphics.GetLeft: integer;
  begin
  result := fGraphicsDriver.Left;
  end;

  function TLayoutElementGraphics.GetTop: integer;
  begin
  result := fGraphicsDriver.Top;
  end;
}
procedure TLayoutElementGraphics.AddPopupMenuItem(aItem: TPopupMenuInfoItem);
begin
    fPopupMenuInfos.Add(aItem);
end;

function TLayoutElementGraphics.GetCoordSystem: TCoordSystemRelation;
begin
    result := fCoordCalculator.CoordSystem;
end;

procedure TLayoutElementGraphics.DoPopup(aSender: TObject; aX, aY: double;
    var vPopupMenuInfos: TPopupMenuInfoList);
begin
    vPopupMenuInfos := fPopupMenuInfos;
    if Assigned(fCallbacks.PopupCallback) then
    begin
        fCallbacks.PopupCallback(self, aX, aY, vPopupMenuInfos);
    end;
end;

procedure TLayoutElementGraphics.DoShowHint(aSender: TObject; var vShow: boolean; var vHintText: string);
begin
    if Assigned(fCallbacks.ShowHintCallBack) then
    begin
        fCallbacks.ShowHintCallBack(self, vShow, vHintText)
    end
    else
    begin
        vHintText := fHint;
        vShow := vHintText <> '';
    end;

end;

procedure TLayoutElementGraphics.DoMouseClick(aSender: TObject);
begin
    if Assigned(fCallbacks.MouseClickCallback) then
        fCallbacks.MouseClickCallback(self);
end;

procedure TLayoutElementGraphics.DoMouseDblClick(aSender: TObject);
begin
    if Assigned(fCallbacks.MouseDblClickCallback) then
        fCallbacks.MouseDblClickCallback(self);
end;

procedure TLayoutElementGraphics.DoMouseMove(aSender: TObject; aShift: TGraphicsShiftState; aX, aY: double);
begin
    if Assigned(fCallbacks.MouseMoveCallback) then
        fCallbacks.MouseMoveCallback(self, aShift, aX, aY);
end;

procedure TLayoutElementGraphics.DoMouseDown(aSender: TObject; aButton: TGraphicsMouseButton;
    aShift: TGraphicsShiftState; aX, aY: double);
begin
    if Assigned(fCallbacks.MouseDownCallback) then
        fCallbacks.MouseDownCallback(self, aButton, aShift, aX, aY);
end;

procedure TLayoutElementGraphics.DoMouseUp(aSender: TObject; aButton: TGraphicsMouseButton;
    aShift: TGraphicsShiftState; aX, aY: double);
begin
    if Assigned(fCallbacks.MouseUpCallback) then
        fCallbacks.MouseUpCallback(self, aButton, aShift, aX, aY);
end;

procedure TLayoutElementGraphics.DoMouseDragOver(aSender, aSource: TObject; aX, aY: double;
    aState: TGraphicsDragState; var vAccept: boolean);
begin
    if Assigned(fCallbacks.MouseDragOverCallback) then
    begin
        fCallbacks.MouseDragOverCallback(self, aSource, aX, aY, aState, vAccept);
    end;
end;

procedure TLayoutElementGraphics.DoMouseDragDrop(aSender, aSource: TObject; aX, aY: double);
begin
    if Assigned(fCallbacks.MouseDragDropCallback) then
    begin
        fCallbacks.MouseDragDropCallback(self, aSource, aX, aY);
    end;
end;

procedure TLayoutElementGraphics.DoPosChanged(aSender: TObject; const aX, aY, aZ: double);
begin
    if Assigned(fCallbacks.PosChangedCallback) then
    begin
        fCallbacks.PosChangedCallback(self, aX, aY, aZ);
    end;
end;

procedure TLayoutElementGraphics.DoIsDragable(aSender: TObject; var vIsDragable: boolean);
begin
    if Assigned(fCallbacks.IsDragableCallback) then
    begin
        fCallbacks.IsDragableCallback(self, vIsDragable);
    end;

end;

function TLayoutElementGraphics.SceneToClient(aX, aY, aZ: double): TPoint4d;
begin
    if Assigned(fGraphicsDriver) then
        result := fGraphicsDriver.SceneToClient(aX, aY, aZ)
    else
        result := MakePoint4d(aX, aY, aZ);
end;

function TLayoutElementGraphics.ClientToScene(aX, aY, aZ: double): TPoint4d;
begin
    if Assigned(fGraphicsDriver) then
        result := fGraphicsDriver.ClientToScene(aX, aY, aZ)
    else
        result := MakePoint4d(aX, aY, aZ);
end;

function TLayoutElementGraphics.ClientToScreen(aX, aY, aZ: double): TPoint4d;
begin
    if Assigned(fGraphicsDriver) then
        result := fGraphicsDriver.ClientToScreen(aX, aY, aZ)
    else
        result := MakePoint4d(aX, aY, aZ);
end;

procedure TLayoutElementGraphics.DoInitGraphicsInfo();
begin
    fGraphicsInfo := TLayoutElementGraphicsInfo.Create();
end;

procedure TLayoutElementGraphics.Invalidate(const aErase: boolean);
// aErase : true - means that the graphics are being moved or resized and the background has to be redrawn - this is very CPU intensive
// : false - means that the graphics are being redrawn in the same place they were before - this is done fast because it does not affect other graphics
begin
    if not Assigned(fGraphicsDriver) then
        EXIT;
    fGraphicsDriver.Invalidate(aErase);
end;

procedure TLayoutElementGraphics.DoUpdateGraphicsInfo;
begin
    if not Assigned(fGraphicsDriver) then
        EXIT;
    fGraphicsDriver.GraphicsInfo.Name := fGraphicsInfo.Name;
    fGraphicsDriver.GraphicsInfo.SizeX := fGraphicsInfo.SizeX;
    fGraphicsDriver.GraphicsInfo.SizeY := fGraphicsInfo.SizeY;
    fGraphicsDriver.GraphicsInfo.SizeZ := fGraphicsInfo.SizeZ;
    fGraphicsDriver.GraphicsInfo.IsSizeVirtual := fGraphicsInfo.IsSizeVirtual;
end;

procedure TLayoutElementGraphics.UpdateGraphicsInfo;
begin
    DoUpdateGraphicsInfo();
    self.Invalidate(true);
end;

function TLayoutElementGraphics.GetPosX: double;
begin
    result := fCoordCalculator.CoordSystem.TranslateX;
end;

function TLayoutElementGraphics.GetPosY: double;
begin
    result := fCoordCalculator.CoordSystem.TranslateY;
end;

function TLayoutElementGraphics.GetPosZ: double;
begin
    result := fCoordCalculator.CoordSystem.TranslateZ;
end;

procedure TLayoutElementGraphics.SetPosX(aValue: double);
begin
    fCoordCalculator.CoordSystem.TranslateX := aValue;
    self.Invalidate(true);
end;

procedure TLayoutElementGraphics.SetPosY(aValue: double);
begin
    fCoordCalculator.CoordSystem.TranslateY := aValue;
    self.Invalidate(true);
end;

procedure TLayoutElementGraphics.SetPosZ(aValue: double);
begin
    fCoordCalculator.CoordSystem.TranslateZ := aValue;
    self.Invalidate(true);
end;

function TLayoutElementGraphics.GetName: string;
begin
    result := fName;
end;

procedure TLayoutElementGraphics.SetName(const aName: string);
begin
    fName := aName;
end;

procedure TLayoutElementGraphics.SceneChanged;
begin
    DoSceneChanged();
end;

procedure TLayoutElementGraphics.DoSceneChanged;
begin
    if Assigned(fGraphicsDriver) then
        fGraphicsDriver.SceneChanged();
end;

function TLayoutElementGraphics.GetCursor: TGraphicsMouseCursor;
begin
    result := fCursor;
end;

procedure TLayoutElementGraphics.SetCursor(const aValue: TGraphicsMouseCursor);
begin
    fCursor := aValue;
    if Assigned(fGraphicsDriver) then
        fGraphicsDriver.Cursor := aValue;
end;

function TLayoutElementGraphics.GetBounds: TBounds;
begin
    result := fGraphicsDriver.Bounds;
end;

procedure TLayoutElementGraphics.SetBorderType(const aValue: TGraphicsBorderType);
begin
    fBorderType := aValue;
    if Assigned(fGraphicsDriver) then
        fGraphicsDriver.BorderType := aValue
end;

procedure TLayoutElementGraphics.SetCaption(const aValue: string);
begin
    fCaption := aValue;
    if Assigned(fGraphicsDriver) then
        fGraphicsDriver.Caption := aValue
end;

procedure TLayoutElementGraphics.PosChangeDragStart(const aRefX, aRefY, aRefZ: double);
begin
    if Assigned(fGraphicsDriver) then
        fGraphicsDriver.PosChangeDragStart(aRefX, aRefY, aRefZ);
end;


end.
