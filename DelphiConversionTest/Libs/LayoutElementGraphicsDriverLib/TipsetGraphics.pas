unit TipsetGraphics;


{ --------------------------------------------------------------------------------------------------
  Copyright © 2008 Zinsser Analytic GmbH - All rights reserved.
  Author       : Payman Kamali (pk)
  Description  :
  --------------------------------------------------------------------------------------------------
  Revision History:
  date     op  method                       track-no improvement/change
  -------- --  ---------------------------  -------- -----------------------------------------------
  23.06.08 pk                                TN4139  Initial Revision
  09.07.08 pk  fPipDeviceName                TN4157   New
  -------------------------------------------------------------------------------------------------- }
interface


uses
    Controls,
    ExtCtrls;

type
    TTipsetTipGraphics = class(TShape)
    protected
        fTipWidth, fTipHeight, fArmTipIndex: integer;
        fPipDeviceName: string;
    public
        constructor Create(); reintroduce;
        procedure SetTipSelected(aIsSelected: boolean);
        procedure SetGraphicsParent(aGraphicsParent: TObject);
        procedure SetGraphicsInfo(aTipWidth, aTipHeight: integer; const aPipDeviceName: string;
            aArmTipIndex: integer);
    end;

    TTipsetGraphics = class(TPanel)
    private
        procedure DetermineBounds;
    protected
        fTipWidth, fTipHeight, fRows, fColumns: integer;
    public
        constructor Create(); reintroduce;
        procedure SetGraphicsParent(aGraphicsParent: TObject);
        procedure SetGraphicsInfo(aTipWidth, aTipHeight, aRows, aColumns: integer);
    end;


implementation


uses
    Graphics;

const
    cWidthSpace = 5;
    cHeightSpace = 5;

    { TTipsetTipGraphics }
constructor TTipsetTipGraphics.Create();
begin
    inherited Create(nil);
    self.Visible := false;

end;

procedure TTipsetTipGraphics.SetGraphicsParent(aGraphicsParent: TObject);
begin
    if aGraphicsParent is TWinControl then
        self.Parent := aGraphicsParent as TWinControl;
end;

procedure TTipsetTipGraphics.SetTipSelected(aIsSelected: boolean);
var
    xColor: TColor;
begin
    if aIsSelected then
        xColor := clRed
    else
        xColor := clWhite;

    self.Brush.Color := xColor;
end;

procedure TTipsetTipGraphics.SetGraphicsInfo(aTipWidth, aTipHeight: integer; const aPipDeviceName: string;
    aArmTipIndex: integer);
begin
    fPipDeviceName := aPipDeviceName;
    fArmTipIndex := aArmTipIndex;
    fTipWidth := aTipWidth;
    fTipHeight := aTipHeight;

    self.Width := aTipWidth;
    self.Height := aTipHeight;
    { TODO -oPK : ArmIndex changed to PipDeviceName }
    // self.Left := (FArmIndex * 20) + 3;
    self.Top := (FArmTipIndex * 10) + 5;
end;

{ TTipsetGraphics }
constructor TTipsetGraphics.Create();

begin
    inherited Create(nil);
    Visible := false;
    Caption := '';

    Width := 0;
    Height := 0;

    Color := clAqua;
    ShowHint := true;

    self.Color := clSkyBlue;
    self.ParentBackground := false;
end;

procedure TTipsetGraphics.SetGraphicsParent(aGraphicsParent: TObject);
begin
    if aGraphicsParent is TWinControl then
    begin
        self.Parent := aGraphicsParent as TWinControl;
    end;
end;

procedure TTipsetGraphics.DetermineBounds();

begin
    self.Width := fColumns * (fTipWidth) + (fColumns + 1) * cWidthSpace;
    self.Height := fRows * (fTipHeight) + (fRows + 1) * cHeightSpace;

    // draw in bottom right corner of parent
    if Assigned(self.Parent) then
    begin
        self.Left := self.Parent.Width - self.Width;
        self.Top := self.Parent.Height - self.Height;
    end;

end;

procedure TTipsetGraphics.SetGraphicsInfo(aTipWidth, aTipHeight, aRows, aColumns: integer);
begin
    fRows := aRows;
    fColumns := aColumns;
    fTipWidth := aTipWidth;
    fTipHeight := aTipHeight;
    DetermineBounds();
end;


end.
