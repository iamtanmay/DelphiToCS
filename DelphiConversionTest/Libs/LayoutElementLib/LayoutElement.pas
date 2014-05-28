unit LayoutElement;
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
  07.07.08 pk  AddHinLine                    TN4139  New
  16.07.08 pk  SceneChanged                  TN4139  New
  10.03.09 pk                                TN4457  Changes needed for reimplementing Stacker Buttons
  23.07.10 wl  TLayoutElementChangeSizeEvent TN5205   neu: wird für unsichtbaren Carrier benutzt
  -------------------------------------------------------------------------------------------------- }


interface


uses
    LayoutElementGraphics,
    CoordSystemMath,
    LayoutElementGraphicsInfo;

type
    TParentMakeHintTextCallback = function(aSender: TObject): string of object;

    TLayoutElementChangeSizeEvent = procedure(aSender: TObject; aSizeX, aSizeY, aSizeZ: double) of object;

    TLayoutElement = class
    private
        procedure SetName(const aValue: string);
        function GetBounds: TBounds;
    protected
        fName: string;
        fGraphics: TLayoutElementGraphics;
        fGraphicsParent: TLayoutElementGraphics;
        fCoordCalculator: TCoordCalculator;
        fUseGraphics: boolean;
        fGraphicsInitialized: boolean;
        fVisible: boolean;
        class procedure AddHintSubLine(const aText: string; var vHintText: string);
        class procedure AddHintLine(const aText: string; var vHintText: string);
        function MakeHintText(): string; virtual;
        procedure DoInitGraphics(); virtual;
        procedure DoShowHint(aSender: TObject; var vShow: boolean; var vHintText: string);
        procedure SetVisible(aVisible: boolean); virtual;

    public
        constructor Create;
        destructor Destroy; override;
        procedure SceneChanged();
        procedure InitGraphics();
        procedure AssignGraphicsParent(aGraphicsParent: TLayoutElementGraphics);
        property CoordCalculator: TCoordCalculator read fCoordCalculator;
        property Graphics: TLayoutElementGraphics read fGraphics;
        property UseGraphics: boolean read fUseGraphics write fUseGraphics;
        property name: string read fName write SetName;
        property Visible: boolean read fVisible write SetVisible;
        property Bounds: TBounds read GetBounds;
    end;


implementation


{ TLayoutElement }

procedure TLayoutElement.AssignGraphicsParent(aGraphicsParent: TLayoutElementGraphics);
begin
    fGraphicsParent := aGraphicsParent;
    if Assigned(fGraphics) then
        fGraphics.Parent := aGraphicsParent;
end;

procedure TLayoutElement.SetVisible(aVisible: boolean);
begin
    fVisible := aVisible;

    if aVisible then
        InitGraphics();

    if Assigned(fGraphics) then
    begin
        fGraphics.Visible := fVisible;
    end;
end;

constructor TLayoutElement.Create();
begin
    inherited;
    fGraphics := nil;
    fUseGraphics := true;
    fGraphicsInitialized := false;
    fVisible := false;
    fCoordCalculator := TCoordCalculator.Create();
end;

destructor TLayoutElement.Destroy();
begin
    fCoordCalculator.Free;
    fGraphics.Free;
    inherited;
end;

procedure TLayoutElement.DoInitGraphics;
begin
    //
end;

procedure TLayoutElement.DoShowHint(aSender: TObject; var vShow: boolean; var vHintText: string);
begin
    vHintText := MakeHintText();
    vShow := vHintText <> '';
end;

procedure TLayoutElement.InitGraphics();
begin
    // initialize only if GraphicsParent is available
    if fGraphicsInitialized then
        EXIT;
    if not fUseGraphics then
        EXIT;
    DoInitGraphics();
    fGraphics.Name := self.Name;
    fGraphics.Tag := self;
    fGraphics.Callbacks.ShowHintCallBack := self.DoShowHint;
    if Assigned(fGraphicsParent) then
        fGraphics.Parent := fGraphicsParent;
    fGraphicsInitialized := true;
end;

function TLayoutElement.MakeHintText: string;
begin
    result := '';
end;

class procedure TLayoutElement.AddHintLine(const aText: string; var vHintText: string);
begin
    if vHintText <> '' then
        vHintText := vHintText + #13#10;

    vHintText := vHintText + aText;
end;

class procedure TLayoutElement.AddHintSubLine(const aText: string; var vHintText: string);
begin
    if vHintText <> '' then
        vHintText := vHintText + #13#10;

    vHintText := vHintText + '    ' + aText;
end;

procedure TLayoutElement.SetName(const aValue: string);
begin
    fName := aValue;
    if Assigned(fGraphics) then
        fGraphics.Name := self.Name;
end;

procedure TLayoutElement.SceneChanged;
begin
    if Assigned(fGraphics) then
        fGraphics.SceneChanged();
end;

function TLayoutElement.GetBounds: TBounds;
begin
    if Assigned(fGraphics) then
        result := fGraphics.Bounds;
end;


end.
