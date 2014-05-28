{ -----------------------------------------------------------------------------------------------------------
  Copyright © 2011 Zinsser Analytic GmbH - All rights reserved.
  Author       : Wolfgang Lyncke (wl)
  Description  :
  -----------------------------------------------------------------------------------------------------------
  Revision History:
  date     op  method                              track-no improvement/change
  -------- --  ----------------------------------  -------- -------------------------------------------------
  19.10.11 wl                                      TN5723   Initial Revision
  20.10.11 wl  CalculateAmountColorAndID           TN5723   neu
  28.10.11 wl  CalculateColor                      TN5725   neu
  03.11.11 wl  CalculateAmountColorAndID           TN5725   wieder entfernt
  ----------------------------------------------------------------------------------------------------------- }

unit ColorUtilities;


interface


uses
    Graphics;

type
    TColorAndVol = record
    public
        Color: TColor;
        Amount: extended;
    end;

    TColorUtilities = record
    private
        class function RGB2TColor(R, G, B: Byte): Integer; static;
        class procedure TColor2RGB(Color: TColor; var R, G, B: Byte); static;
    public const
        cColorNothing = 0;
    public
        class function AdditiveMix(aData: TArray<TColorAndVol>): TColor; static;
    end;


implementation


uses
    SysUtils,
    Windows,
    Math;

{ TColorUtilities }

class function TColorUtilities.RGB2TColor(R, G, B: Byte): Integer;
begin
    // convert hexa-decimal values to RGB
    Result := R or (G shl 8) or (B shl 16);
end;

class procedure TColorUtilities.TColor2RGB(Color: TColor; var R, G, B: Byte);
begin
    // convert hexa-decimal values to RGB
    if Color shr 24 = $FF then
        Color := GetSysColor(Color and $FF)
    else if Color shr 24 > $02 then
        Color := 0;
    R := Color;
    G := (Color shr 8);
    B := (Color shr 16);
end;

class function TColorUtilities.AdditiveMix(aData: TArray<TColorAndVol>): TColor;
var
    xRB, xGB, xBB: byte;
    xR, xG, xB: extended;
    xReduced: extended;
    xReducedAll: extended;
    x: integer;
begin
    if Length(aData) = 0 then
        EXIT(0);

    xR := 0;
    xG := 0;
    xB := 0;
    xReducedAll := 0;
    for x := 0 to high(aData) do
    begin
        if (aData[x].Color = cColorNothing) then
            CONTINUE; // Farbe 0 = nicht dazu zählen. Für schwarz muss clBlack verwendet werden

        if (aData[x].Amount <= 0) then
            CONTINUE; // Asp-Schritte nicht dazu zählen

        xReduced := Power(2, Log10(aData[x].Amount)); // damit werden kleine Anteile verstärkt

        TColor2RGB(aData[x].Color, xRB, xGB, xBB);

        xR := xR + (xRB * xReduced);
        xG := xG + (xGB * xReduced);
        xB := xB + (xBB * xReduced);
        xReducedAll := xReducedAll + xReduced;
    end;

    if xReducedAll = 0 then
        EXIT(0); // sonst Division By 0

    result := RGB2TColor(Round(xR / xReducedAll), Round(xG / xReducedAll), Round(xB / xReducedAll));
    if result = 0 then
        result := clBlack;
end;


end.
