{ --------------------------------------------------------------------------------------------------
  Copyright © 2008 Zinsser Analytic GmbH - All rights reserved.
  Author       : Payman Kamali (pk)
  Description  : Classes and math functions for Coordinate System Relation
  --------------------------------------------------------------------------------------------------
  Revision History:
  date     op  method                       track-no improvement/change
  -------- --  ---------------------------  -------- -----------------------------------------------
  23.06.08 pk                                TN4139  Initial Revision
  09.07.08 pk  Create                        TN4139  Slots Object list is not owner
  10.03.09 pk                                TN4457  Changes needed for reimplementing Stacker Buttons
  11.03.09 pk  SetPanelPosition              TN4457  Positions changed
  12.03.09 pk  CreateLevelButtons            TN4457  Don't create levelpanel if lbtinvisible
  10.08.09 wl                                TN4702   Strings werden jetzt direkt geladen
  04.11.09 pk                                TN4843  Replace TObjectList, TStringList, TStrings with Generics Lists and TStringArray
  21.03.13 wl                                TN6045   uses Generics.Collections
  -------------------------------------------------------------------------------------------------- }

unit CarrierGraphics;


interface


uses
    Generics.Collections,
    Classes,
    LayoutElementGraphicsInfo,
    LayoutElementGraphics,
    MatrixMath,
    CarrierSlotGraphics,
    PanelControlGraphics,
    ButtonControlGraphics,
    IntfLayoutElementGraphicsDriver,
    LayoutElementCallbackTypes;

type
    TButtonOrientation = (lbtFront, lbtBack, lbtLeft, lbtRight, lbtInvisible);

    TVisibleFloorChangedCallback = procedure(const aFloor: integer) of object;

    TFloorButtonControlGraphics = class(TButtonControlGraphics)
    private
        fFloorIndex: integer;
    public
        property FloorIndex: integer read fFloorIndex write fFloorIndex;
    end;

    TCarrierGraphics = class(TLayoutElementGraphics)
    private
        fLevelPanel: TPanelControlGraphics;
        fButtons: TObjectList<TButtonControlGraphics>;
        fSlots: TObjectList<TCarrierSlotGraphics>;
        fShowOverviewCallback: TMouseClickCallback;
        fVisibleFloorChangedCallback: TVisibleFloorChangedCallback;
        function GetButtonOrientation: TButtonOrientation;
        procedure SetPanelPosition;
        procedure SetButtonDownForFloor(aFloor: integer);
    protected
        function GetGraphicsDriverID(): TLayoutElementGraphicsDriverID; override;
        procedure DoInitGraphicsInfo(); override;
        function GetGraphicsInfo(): TCarrierGraphicsInfo;
        procedure DoUpdateGraphicsInfo; override;
        procedure DoOnVisibleFloorChanged(aSender: TObject);
        procedure DoOnShowOverview(aSender: TObject);
        procedure SetLevelPanelVisible(aVisible: boolean);
        procedure SetVisible(aVisible: boolean); override;
    public
        constructor Create();
        destructor Destroy; override;
        procedure CreateLevelButtons;
        procedure SetVisibleFloor(aFloor: integer);
        // procedure SetVisibleFloorButton(aButton: TSpeedButton);
        procedure AddSlot(aSlotGraphics: TCarrierSlotGraphics);
        property GraphicsInfo: TCarrierGraphicsInfo read GetGraphicsInfo;
        property ShowOverviewCallback: TMouseClickCallback read fShowOverviewCallback
            write fShowOverviewCallback;
        property VisibleFloorChangedCallback: TVisibleFloorChangedCallback read fVisibleFloorChangedCallback
            write fVisibleFloorChangedCallback;
    end;


implementation


uses
    SysUtils,
    GeneralTypes;

{ TCarrierGraphics }

constructor TCarrierGraphics.Create();
begin
    inherited Create();
    fSlots := TObjectList<TCarrierSlotGraphics>.Create(false);

    self.Color := $00C4C4C4;
    // BevelInner  := bvRaised;
    fLevelPanel := nil;
    fButtons := TObjectList<TButtonControlGraphics>.Create(true);
end;

destructor TCarrierGraphics.Destroy();
begin
    FreeAndNil(fButtons);
    FreeAndNil(fLevelPanel);
    FreeAndNil(fSlots);
    inherited;
end;

procedure TCarrierGraphics.SetVisible(aVisible: boolean);
begin
    inherited;
    SetLevelPanelVisible(aVisible);
end;

procedure TCarrierGraphics.SetLevelPanelVisible(aVisible: boolean);
var
    x: integer;
begin
    if Assigned(fLevelPanel) then
    begin
        fLevelPanel.Visible := aVisible;
        for x := 0 to fButtons.Count - 1 do
        begin
            fButtons[x].Visible := aVisible;
        end;

        if self.Visible and fLevelPanel.Visible then
            self.SetPanelPosition();
    end;
end;

procedure TCarrierGraphics.SetButtonDownForFloor(aFloor: integer);
var
    x: integer;
begin
    for x := 0 to fButtons.Count - 1 do
    begin
        if not(fButtons[x] is TFloorButtonControlGraphics) then
            CONTINUE;
        if (fButtons[x] as TFloorButtonControlGraphics).FloorIndex = aFloor then
            (fButtons[x] as TFloorButtonControlGraphics).BorderType :=
                gbtSunken else (fButtons[x] as TFloorButtonControlGraphics).BorderType := gbtRaised;
    end;
end;

procedure TCarrierGraphics.SetVisibleFloor(aFloor: integer);
begin
    SetButtonDownForFloor(aFloor);
end;

procedure TCarrierGraphics.AddSlot(aSlotGraphics: TCarrierSlotGraphics);
begin
    aSlotGraphics.Parent := self;
    fSlots.Add(aSlotGraphics);
end;

function TCarrierGraphics.GetGraphicsDriverID: TLayoutElementGraphicsDriverID;
begin
    result := ICarrierGraphicsDriver;
end;

function TCarrierGraphics.GetGraphicsInfo: TCarrierGraphicsInfo;
begin
    result := fGraphicsInfo as TCarrierGraphicsInfo;
end;

procedure TCarrierGraphics.DoInitGraphicsInfo;
begin
    fGraphicsInfo := TCarrierGraphicsInfo.Create();
end;

procedure TCarrierGraphics.DoUpdateGraphicsInfo;
begin
    inherited;
end;

function TCarrierGraphics.GetButtonOrientation(): TButtonOrientation;
const
    ENUM_LEVELBTN_ORIENTATION_DEFAULT = lbtFront;
begin
    case self.GraphicsInfo.StackerButtons of
        0:
            result := lbtFront;
        1:
            result := lbtBack;
        2:
            result := lbtLeft;
        3:
            result := lbtRight;
        4:
            result := lbtInvisible;
        else
            result := ENUM_LEVELBTN_ORIENTATION_DEFAULT;
    end;
end;

procedure TCarrierGraphics.SetPanelPosition;
var
    xCarrierSizeX, xCarrierSizeY: double;
begin
    xCarrierSizeX := self.GraphicsInfo.SizeX;
    xCarrierSizeY := self.GraphicsInfo.SizeY;

    if (fLevelPanel <> nil) then
    begin
        // dimensions are relative to (0,0) being at the top-left of the carrier
        case self.GetButtonOrientation() of
            lbtFront:
                fLevelPanel.SetBounds(0, xCarrierSizeY, 0, fLevelPanel.SizeX, fLevelPanel.SizeY, 0);
            lbtBack:
                fLevelPanel.SetBounds(0, -fLevelPanel.SizeY, 0, fLevelPanel.SizeX, fLevelPanel.SizeY, 0);
            lbtLeft:
                fLevelPanel.SetBounds(0 - fLevelPanel.SizeX, 0, 0, fLevelPanel.SizeX, fLevelPanel.SizeY, 0);
            lbtRight:
                fLevelPanel.SetBounds(0 + xCarrierSizeX, 0, 0, fLevelPanel.SizeX, fLevelPanel.SizeY, 0);
        end;
    end;
end;

procedure TCarrierGraphics.CreateLevelButtons;
const
    cMinButtonHeight = 16;
    cFirstButtonHeight = cMinButtonHeight - 2;
    cMinButtonWidth = 16;
    cFirstButtonWidth = cMinButtonWidth - 2;
    STR_LEVELBTN_NAME = 'LevelButton__';
    cButtonPosZ = 5;
    // this is needed for mousclick events, someway to tell the scene object that button is on top of panel
var
    i: integer;
    xBtnHeight, xBtnWidth, xFirstBtnWidth, xFirstBtnHeight: double;
    xButton: TFloorButtonControlGraphics;
    xFirstButton: TButtonControlGraphics;
    xOrientation: TButtonOrientation;
    xCarrierSizeX, xCarrierSizeY: double;
    xLevels: integer;
begin
    xLevels := self.GraphicsInfo.Levels;
    if xLevels <= 1 then
        EXIT;
    fButtons.Clear();
    FreeAndNil(fLevelPanel);

    xOrientation := self.GetButtonOrientation();
    if xOrientation = lbtInvisible then
        Exit;

    fLevelPanel := TPanelControlGraphics.Create();
    // fLevelPanel.CoordSystem.ReflectY := true; // we need to do reflect the buttons because usually the carrier is reflected in Y, so we reflect to be in relation to workbench

    fLevelPanel.Parent := self;
    fLevelPanel.BorderType := gbtNone;

    xCarrierSizeX := self.GraphicsInfo.SizeX;
    xCarrierSizeY := self.GraphicsInfo.SizeY;

    if (xOrientation = lbtFront) or (xOrientation = lbtBack) then
    begin
        fLevelPanel.SizeX := xCarrierSizeX;
        fLevelPanel.SizeY := cMinButtonHeight;
        xBtnHeight := cMinButtonHeight;
        xBtnWidth := Round(xCarrierSizeX - cFirstButtonWidth) div xLevels;
        xFirstBtnWidth := cFirstButtonWidth;
        xFirstBtnHeight := cMinButtonHeight;
    end
    else
    begin
        fLevelPanel.SizeX := cMinButtonWidth;
        fLevelPanel.SizeY := xCarrierSizeY;
        xBtnWidth := cMinButtonWidth;
        xBtnHeight := Round(xCarrierSizeY - cMinButtonHeight) div xLevels;
        xFirstBtnWidth := cMinButtonWidth;
        xFirstBtnHeight := cFirstButtonHeight;
    end;

    // First Button (Overview)
    xFirstButton := TButtonControlGraphics.Create();
    xFirstButton.Parent := fLevelPanel;
    xFirstButton.Caption := '#';
    xFirstButton.Hint := TLanguageString.Read('Choose a stacker level from overview',
        'Wähle eine Stacker-Ebene aus der Übersicht');
    xFirstButton.SetBounds(0, 0, cButtonPosZ, xFirstBtnWidth, xFirstBtnHeight, 0);

    // xButton.Flat := true;
    xFirstButton.Callbacks.MouseClickCallback := DoOnShowOverview;
    FButtons.Add(xFirstButton);

    if (xBtnWidth > 0) and (xBtnHeight > 0) then
        for i := 0 to xLevels - 1 do
        begin
            xButton := TFloorButtonControlGraphics.Create();
            xButton.Parent := fLevelPanel;
            // xButton.GroupIndex := 99;
            xButton.Name := STR_LEVELBTN_NAME + IntToStr(i + 1);
            xButton.Caption := IntToStr(i + 1);
            xButton.Hint := TLanguageString.Read('Show stacker level {0}', 'Zeige Stacker-Ebene {0}',
                [i + 1]);
            xButton.FloorIndex := i;

            // xButton.ShowHint := true;
            // if (i=0) then xButton.Down := true;
            // xButton.Flat := true;

            xButton.Callbacks.MouseClickCallback := DoOnVisibleFloorChanged;
            FButtons.Add(xButton);

            if (xOrientation = lbtFront) or (xOrientation = lbtBack) then
            begin
                xButton.SetBounds(xFirstBtnWidth + (i * xBtnWidth), 0, cButtonPosZ, xBtnWidth, xBtnHeight, 0);
            end
            else
            begin
                xButton.SetBounds(0, xFirstBtnHeight + (i * xBtnHeight), cButtonPosZ, xBtnWidth,
                    xBtnHeight, 0);
            end;
        end;

end;

procedure TCarrierGraphics.DoOnShowOverview(aSender: TObject);
begin
    if Assigned(fShowOverviewCallback) then
        fShowOverviewCallback(aSender);
end;

procedure TCarrierGraphics.DoOnVisibleFloorChanged(aSender: TObject);
begin
    if Assigned(fVisibleFloorChangedCallback) then
        fVisibleFloorChangedCallback((aSender as TFloorButtonControlGraphics).FloorIndex);
end;


end.
