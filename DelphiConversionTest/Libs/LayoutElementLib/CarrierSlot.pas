unit CarrierSlot;
{ --------------------------------------------------------------------------------------------------
  Copyright © 2008 Zinsser Analytic GmbH - All rights reserved.
  Author       : Payman Kamali (pk)
  Description  :
  --------------------------------------------------------------------------------------------------
  Revision History:
  date     op  method                       track-no improvement/change
  -------- --  ---------------------------  -------- -----------------------------------------------
  23.06.08 pk                                TN4139  Initial Revision
  02.07.08 pk  fPosZ                         TN4139  New
  07.07.08 pk  fRack                         TN4139  changed from TObject to TRack
  16.07.08 pk  UpdateStackHeight,etc         TN4139  new functions needed for calculating stack heights based on prev level
  19.09.08 pk  CalcStackHeight               TN4139  uses function IncreaseStackHeight
  16.03.09 pk                                TN4463  Made more flexible
  16.03.09 pk  SlotHeightChanged             TN4472  New
  04.11.09 pk                                TN4843  Replace TObjectList, TStringList, TStrings with Generics Lists and TStringArray
  25.11.09 pk  RemoveRack                    TN4905  change z position of next level slot only if fCalcStackHeight is true
  23.07.10 wl  MakeHintText                  TN5205   kein Text bei unsichtbaren Carrier
  10.04.13 wl                                TN6045   uses Generics.Collections
  16.04.13 pp  properties SlotSize...        TN6131   new: DiscretePositions
  -------------------------------------------------------------------------------------------------- }


interface


uses
    CommonTypes,
    AppTypes,
    CarrierSlotGraphics,
    CoordSystemMath,
    LayoutElement,
    Rack;

type
    TCarrierGetTypeName = function: string of object;

    TCarrierSlot = class(TLayoutElement)
    private
        fCarrier: TObject;
        fCalcStackHeight: boolean;
        fNextLevelSlot: TCarrierSlot;
        fPreviousLevelSlot: TCarrierSlot;
        fRack: TRack;
        fSizeX, fSizeY, fSizeZ: TPosMM;
        fPosX, fPosY, fPosZ: TPosMM;
        fSlotNr: integer;
        fParentMakeHintTextCallback: TParentMakeHintTextCallback;
        fCarrierGetTypeName: TCarrierGetTypeName;
        procedure UpdateGraphicsInfo;
        function GetGraphics: TCarrierSlotGraphics;
        function DoMakeHintTextForChild(aSender: TObject): string;
        procedure SetPosX(aPosX: TPosMM);
        procedure SetPosY(aPosY: TPosMM);
        procedure SetPosZ(aPosZ: TPosMM);
        procedure SetSizeZ(const aValue: TPosMM);
        function CalcStackHeight: TPosMM;
        procedure SetPreviousLevelSlot(const aValue: TCarrierSlot);
        procedure UpdateNextLevelStackHeight(const aValue: TPosMM);
        procedure SlotHeightChanged();
    protected
        procedure DoInitGraphics(); override;
        function MakeHintText: string; override;
        property PreviousLevelSlot: TCarrierSlot read fPreviousLevelSlot write SetPreviousLevelSlot;
        property SizeZ: TPosMM read fSizeZ write SetSizeZ;
    public
        constructor Create();
        destructor Destroy(); override;

        procedure UpdateStackHeight(const aValue: TPosMM);
        procedure SetUpSlot(aSlotNr: integer; aSizeX, aSizeY, aPosX, aPosY, aPosZ: TPosMM;
            aCalcStackHeight: boolean; aPreviousLevelSlot: TCarrierSlot);
        function IsStackingAllowed(): boolean;
        procedure PutRack(aRack: TRack; aRotation: TRotationValue);
        procedure RemoveRack();

        property Graphics: TCarrierSlotGraphics read GetGraphics;
        property Carrier: TObject read fCarrier write fCarrier;
        property CarrierGetTypeName: TCarrierGetTypeName read fCarrierGetTypeName write fCarrierGetTypeName;
        property SlotNr: integer read fSlotNr;
        property Rack: TRack read fRack;
        property PosX: TPosMM read fPosX write SetPosX;
        property PosY: TPosMM read fPosY write SetPosY;
        property PosZ: TPosMM read fPosZ write SetPosZ;
        property NextLevelSlot: TCarrierSlot read fNextLevelSlot write fNextLevelSlot;
        property ParentMakeHintTextCallback: TParentMakeHintTextCallback read fParentMakeHintTextCallback
            write fParentMakeHintTextCallback;
        property SlotSizeX: TPosMM read fSizeX;
        property SlotSizeY: TPosMM read fSizeY;
        property SlotSizeZ: TPosMM read fSizeZ;
    end;


implementation


uses
    SysUtils,
    LayoutElementGraphicsInfo,
    MatrixMath,
    CarrierDataAdaptor;

{ TCarrierSlot }

constructor TCarrierSlot.Create;
begin
    inherited Create();
    fCarrier := nil;
    fRack := nil;
    fSlotNr := 0;
end;

destructor TCarrierSlot.Destroy;
begin
    inherited;
end;

procedure TCarrierSlot.UpdateGraphicsInfo();
begin
    with self.Graphics.GraphicsInfo do
    begin
        name := fName;
        SizeX := fSizeX;
        SizeY := fSizeY;
    end;
    self.Graphics.UpdateGraphicsInfo();
end;

procedure TCarrierSlot.SetUpSlot(aSlotNr: integer; aSizeX, aSizeY, aPosX, aPosY, aPosZ: TPosMM;
    aCalcStackHeight: boolean; aPreviousLevelSlot: TCarrierSlot);
begin
    fSlotNr := aSlotNr;
    fSizeX := aSizeX;
    fSizeY := aSizeY;
    self.SizeZ := 0;
    self.PosX := aPosX;
    self.PosY := aPosY;
    self.PosZ := aPosZ;
    fCalcStackHeight := aCalcStackHeight;
    self.PreviousLevelSlot := aPreviousLevelSlot;
    UpdateGraphicsInfo();
end;

procedure TCarrierSlot.DoInitGraphics;
begin
    fGraphics := TCarrierSlotGraphics.Create();
end;

function TCarrierSlot.GetGraphics: TCarrierSlotGraphics;
begin
    result := fGraphics as TCarrierSlotGraphics;
end;

function TCarrierSlot.CalcStackHeight(): TPosMM;
begin
    result := self.CoordCalculator.IncreaseZHeight(self.PosZ, self.Rack.StackHeightZ);
end;

procedure TCarrierSlot.UpdateStackHeight(const aValue: TPosMM);
begin
    self.PosZ := aValue;
    SlotHeightChanged();
end;

procedure TCarrierSlot.UpdateNextLevelStackHeight(const aValue: TPosMM);
begin
    if Assigned(fNextLevelSlot) then
        fNextLevelSlot.UpdateStackHeight(aValue);
end;

procedure TCarrierSlot.SlotHeightChanged();
begin
    if fCalcStackHeight and Assigned(self.Rack) then
        UpdateNextLevelStackHeight(self.CalcStackHeight());
end;

procedure TCarrierSlot.PutRack(aRack: TRack; aRotation: TRotationValue);
begin
    fRack := aRack;

    fRack.CoordCalculator.ParentCoordCalculator := self.CoordCalculator;
    fRack.ParentMakeHintTextCallback := self.DoMakeHintTextForChild;
    fRack.AssignGraphicsParent(self.Graphics);

    fRack.ChangeSlot(self, aRotation);

    // 19.09.08 pk this is NOT really a good implementation for StackHeights. We should make it so the
    // stack height is dynamically calculated
    SlotHeightChanged();
end;

function TCarrierSlot.DoMakeHintTextForChild(aSender: TObject): string;
begin
    result := self.MakeHintText();
end;

procedure TCarrierSlot.RemoveRack;
begin
    fRack.ChangeSlot(nil, Rotation_0);
    fRack.CoordCalculator.ParentCoordCalculator := nil;
    fRack.ParentMakeHintTextCallback := nil;
    fRack.AssignGraphicsParent(nil);
    fRack := nil;

    if fCalcStackHeight then
        UpdateNextLevelStackHeight(self.PosZ);
end;

function TCarrierSlot.MakeHintText: string;
begin
    result := '';
    if Assigned(self.CarrierGetTypeName) and
        (self.CarrierGetTypeName() <> TCarrierDataAdaptor.InvisibleCarrierName) then
    begin
        AddHintLine('Slot', result);
        AddHintSubLine(Format('Slot: %d', [fSlotNr]), result);
    end;
    if Assigned(fParentMakeHintTextCallback) then
        AddHintLine(fParentMakeHintTextCallback(self), result);

end;

procedure TCarrierSlot.SetPosX(aPosX: TPosMM);
begin
    fPosX := aPosX;
    fCoordCalculator.CoordSystem.TranslateX := fPosX;
    self.Graphics.PosX := fPosX;
end;

procedure TCarrierSlot.SetPosY(aPosY: TPosMM);
begin
    fPosY := aPosY;
    fCoordCalculator.CoordSystem.TranslateY := fPosY;
    self.Graphics.PosY := fPosY;
end;

procedure TCarrierSlot.SetPosZ(aPosZ: TPosMM);
begin
    fPosZ := aPosZ;
    fCoordCalculator.CoordSystem.TranslateZ := fPosZ;
    self.Graphics.PosZ := fPosZ;
end;

procedure TCarrierSlot.SetSizeZ(const aValue: TPosMM);
begin
    fSizeZ := aValue;
    self.CoordCalculator.CoordSystem.TranslateSizeZ := aValue;
    with self.Graphics.GraphicsInfo do
    begin
        SizeZ := aValue;
    end;
end;

procedure TCarrierSlot.SetPreviousLevelSlot(const aValue: TCarrierSlot);
begin
    fPreviousLevelSlot := aValue;
    if Assigned(fPreviousLevelSlot) then
        fPreviousLevelSlot.NextLevelSlot := self;
end;

function TCarrierSlot.IsStackingAllowed(): boolean;
begin
    // if we have to calculate stackheights make sure the previouslevel has a rack
    result := (not fCalcStackHeight) or (not Assigned(fPreviousLevelSlot)) or
        (Assigned(fPreviousLevelSlot.Rack));
end;


end.
