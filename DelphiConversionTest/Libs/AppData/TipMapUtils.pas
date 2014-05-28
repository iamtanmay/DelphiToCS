{ --------------------------------------------------------------------------------------------------
  Copyright © 2007 Zinsser Analytic GmbH - All rights reserved.
  Author       : Payman Kamali (pk)
  Description  :
  --------------------------------------------------------------------------------------------------
  Revision History:
  date     op  method                       track-no improvement/change
  -------- --  ---------------------------  -------- -----------------------------------------------
  05.09.07 pk                                         Initial Revision
  09.11.07 pk  gmPosMMArrText               TN3924    New
  18.02.08 wl  gmGetNoOfTipsFromTipMap      TN4009    result ist jetzt integer
  24.06.08 wl  gmGetNullArray               TN4143    new
  12.08.08 wl  gmSArrText                   TN4170.1  benutzt Systemvariable ListSeparator statt immer nur Komma
  19.09.08 pk  gmTipMapToStr                TN4215    New
  31.08.09 wl  gmStrArrText                 TN4740    --> TArrayUtils.ArrayToBracketText
  12.09.09 wl                               TN4740   enthält nur noch Funktionen für TIPMAP, Array-Funktionen --> ArrayUtils
  15.12.09 pk  GetFirstSelectedTipIndex     TN4943   New
  -------------------------------------------------------------------------------------------------- }

unit TipMapUtils;


interface


uses
    AppTypes,
    GeneralTypes;

type
    TTipMapUtils = class
    public
        class function GetNoOfTipsFromTipMap(aTipMap: TIPMAP): integer;
        class function GetNoOfUnusedTips(aTipMap: TIPMAP; aTipCount: integer): integer;
        class function GetNoOfUsedTips(aTipMap: TIPMAP; aTipCount: integer): integer;
        class function EmptyTipMap(): TIPMAP;
        class procedure SelectTip(var vTips: TIPMAP; aIndex: integer);
        class procedure UnselectTip(var vTips: TIPMAP; aIndex: integer);
        class function FullTipMap(aTipCount: integer): TIPMAP;
        class function TipMapToStr(aTipMap: TIPMAP): string;
        class function TipSelected(aTips: TIPMAP; aIndex: integer): boolean;
        class function GetFirstSelectedTipIndex(aTips: TIPMAP; aTipCount: integer): integer;
        class function GetFirstUnselectedTipIndex(aTips, aAllowedTips: TIPMAP; aTipCount: integer): integer;
        class function MaxLiquid(aTipMap: TIPMAP; const aVol: TDoubleArray): double;
    end;

    // zu aufwendig: lasse ich erst mal
function gmEmptyTipMap(): TIPMAP;
function gmTipSelected(aTips: TIPMAP; aIndex: integer): boolean;
procedure gmSelectTip(var aTips: TIPMAP; aIndex: integer);


implementation


uses
    SysUtils,
    CommonTypes;

{ TTipmapUtils }

class function TTipmapUtils.GetNoOfTipsFromTipMap(aTipMap: TIPMAP): integer;
var
    i: integer;
    xNoOfTips: word;
begin
    xNoOfTips := 0;
    for i := 0 to MAX_TIPS - 1 do
        if (aTipMap and (1 shl i)) > 0 then
            xNoOfTips := xNoOfTips + 1;
    result := xNoOfTips;
end;

class function TTipMapUtils.GetNoOfUnusedTips(aTipMap: TIPMAP; aTipCount: integer): integer;
var
    x: integer;
begin
    result := 0;
    for x := 0 to aTipCount - 1 do
        if (((aTipMap shr x) and 1) <> 1) then
            inc(result);
end;

// TipCnt: calculates no of tips used in a tip map
class function TTipMapUtils.GetNoOfUsedTips(aTipMap: TIPMAP; aTipCount: integer): integer;
var
    x: integer;
begin
    result := 0;
    for x := 0 to aTipCount - 1 do
        if (((aTipMap shr x) and 1) = 1) then
            inc(result);
end;

class function TTipmapUtils.TipMapToStr(aTipMap: TIPMAP): string;
var
    x: integer;
begin
    result := '';
    for x := 0 to MAX_TIPS - 1 do
    begin
        if not gmTipSelected(aTipMap, x) then
            CONTINUE;
        if result <> '' then
            result := result + ', ';
        result := result + IntToStr(x + 1);
    end;
end;

class procedure TTipMapUtils.UnselectTip(var vTips: TIPMAP; aIndex: integer);
begin
    vTips := vTips and (not(1 shl aIndex));
end;

class function TTipMapUtils.TipSelected(aTips: TIPMAP; aIndex: integer): boolean;
begin
    result := (((aTips shr aIndex) and 1) = 1);
end;

class procedure TTipMapUtils.SelectTip(var vTips: TIPMAP; aIndex: integer);
begin
    vTips := vTips or (1 shl aIndex);
end;

class function TTipMapUtils.EmptyTipMap(): TIPMAP;
begin
    result := 0;
end;

class function TTipmapUtils.FullTipMap(aTipCount: integer): TIPMAP;
var
    x: integer;
begin
    result := TTipMapUtils.EmptyTipMap();
    for x := 0 to aTipCount - 1 do
        SelectTip(result, x);
end;

class function TTipMapUtils.GetFirstSelectedTipIndex(aTips: TIPMAP; aTipCount: integer): integer;
// returns the first tipindex in aTips which is selected
var
    x: integer;
begin
    result := -1;
    for x := 0 to aTipCount - 1 do
    begin
        if TipSelected(aTips, x) then
        begin
            result := x;
            EXIT;
        end;
    end;
end;

class function TTipMapUtils.GetFirstUnselectedTipIndex(aTips, aAllowedTips: TIPMAP;
    aTipCount: integer): integer;
// returns the first tipindex in aTips which NOT selected, using the aAllowedTips as a mask of possible tips.
var
    x: integer;
begin
    result := -1;
    for x := 0 to aTipCount - 1 do
    begin
        if not TipSelected(aAllowedTips, x) then
            CONTINUE;

        if not TipSelected(aTips, x) then
        begin
            result := x;
            EXIT;
        end;
    end;
end;

// from SamHigh.pas:
class function TTipmapUtils.MaxLiquid(aTipMap: TIPMAP; const aVol: TDoubleArray): double;
var
    x: integer;
begin
    result := 0;

    for x := 0 to high(aVol) do
    begin
        if TipSelected(aTipMap, x) and (aVol[x] > result) then
            result := aVol[x];
    end;
end;


// erst mal gelassen:

function gmEmptyTipMap(): TIPMAP;
begin
    result := 0;
end;

function gmTipSelected(aTips: TIPMAP; aIndex: integer): boolean;
begin
    result := (((aTips shr aIndex) and 1) = 1);
end;

procedure gmSelectTip(var aTips: TIPMAP; aIndex: integer);
begin
    aTips := aTips or (1 shl aIndex);
end;


end.
