{ --------------------------------------------------------------------------------------------------
  Copyright © 2004 Zinsser Analytic GmbH - All rights reserved.
  Author       : Wolfgang Lyncke (wl)
  Description  : simple structures that include TRack and belonging utility functions
  --------------------------------------------------------------------------------------------------
  Revision History:
  date     op  method                       track-no improvement/change
  -------- --  ---------------------------  -------- -----------------------------------------------
  15.04.04 wl                               TN1788   initial version
  10.05.04 pk                               TN1889.1 New: TXRackPosition, TXRackPositions, TXTubeParameters: reference TRack instead of rackname(string)
  15.05.04 pk  TDilRackPositions            TN1889.1 DilRackType : system, manual, normal
  24.05.04 wl  gmXRackPosToRackIDPos        TN1949   keine Exception wenn Rack = nil
  17.06.04 pk  TDilRackType                 TN1995   new field : Diluent
  08.11.04 wl  TXRackPosition               TN2213   RecCnt entfernt
  08.11.04 wl  TXTubeParameters             TN2213   entfernt
  15.10.05 wl  TRackDefinitions             TN2672   neue Klasse für Rackdefinitionen
  15.10.05 wl  TRackDefinitions             TN2672   kapselt die Bezeichner STR_RACKNAME_WASH, STR_RACKNAME_WASTE, ..
  09.11.05 wl  TPipSingleStep,TPipStepArray TN2728   aus AppTypes: enthält jetzt Rack und RackPos-Array
  09.11.05 wl  gmGetEmptyPipSingleStep      TN2728   von DevicesArms hierher: Rack wird auch auf nil gesetzt
  14.11.05 wl  TXRackPositionArray          TN2775   neu
  04.03.06 wl  gmGetRackPositionString      TN2954   ezeugt Positionsangaben: A1, B1, ...
  12.09.09 wl  TDiTiPositions               TN4740   --> DiTiObserver
  12.09.09 wl  TXRackPositions              TN4740   enthält TIntArray statt statisches TipIntegerArray
  12.09.09 wl  TXRackPositionUtils          TN4740   enthält Methoden für TXRackPositions
  17.11.11 wl                               TN5725   RecCnt entfernt
  02.02.12 wl  TXRackPositions              TN5791   abgeschafft (Kombination 1 Rack : n Positionen wird nicht mehr verwendet)
  02.02.12 wl  TXRackPositionUtils          TN5791   bietet viele neue Funktionen für TArray<TXRackPosition>
  02.02.12 wl  TXRackPositionUtils.PaintTubePositions      TN5791   von TRack hierher
  06.03.12 wl  TXRackPositionUtils.PaintTubePositions      TN5791   zeichnete falsche Positionen
  29.03.12 wl  gmMakeRackArray                             TN5791   erstellt ein Array desselben Racks
  23.05.13 wl  ExtractInternMoveData                       TN6153   entfernt
  -------------------------------------------------------------------------------------------------- }

unit RackTypes;


interface


uses
    Rack,
    CommonTypes,
    AppTypes,
    RackWell,
    GeneralTypes;

type
    TXRackPosition = record
        Rack: TRack;
        Pos: integer;
    end;

    TXRackPositionUtils = class
    private
        class function RackPosToText(const aRackPositions: TArray<TXRackPosition>): string;
        class function RackRacksToText(const aRackPositions: TArray<TXRackPosition>): string;
    public
        class function GetEmptyXRackPositions(aTipCount: integer): TArray<TXRackPosition>;
        class function RackPositionsToBracketText(const aRackPositions: TArray<TXRackPosition>): string;
        class procedure PaintTubePositions(const aRackPositions: TArray<TXRackPosition>;
            const aColorType: TRackWellDisplayType);
        class function ExtractRacksFromArray(aRP: TArray<TXRackPosition>): TArray<TRack>;
        class function ExtractSingleRackFromArray(const aRP: TArray<TXRackPosition>): TRack; overload;
        class function ExtractSingleRackFromArray(const aRacks: TArray<TRack>): TRack; overload;
        class function ExtractSingleRackNameFromArray(aRP: TArray<TXRackPosition>): string;
        class function ExtractPositionsFromArray(aRP: TArray<TXRackPosition>): TArray<integer>;
        class function XRackPosToRackPosArray(aRackPositions: TArray<TXRackPosition>): TArray<TRackPosition>;
    end;

function gmXRackPosToRackIDPos(aXRackPos: TXRackPosition): TRackIDPosition;
function gmRackIDPosToRackPos(RackIDPos: TRackIDPosition): TRackPosition;
function gmMakeRackPos(aRackName: string; aRackPos: integer): TRackPosition;
function gmMakeRackIDPos(aRackName: string; aRackPos: integer; aRackID: string): TRackIDPosition;
function gmMakeDefRackIDPos(): TRackIDPosition;
function gmMakeXRackPos(aRack: TRack; aPos: integer): TXRackPosition;
function gmMakeRackArray(const aRack: TRack; aCount: integer): TArray<TRack>;

// Das sollte eigentlich in Rack.pas
function gmGetRackPositionString(aPosition, aNoOfRows: integer): string;


// Konventionen für Layout-Objekte

type
    TSpecialRackType = (srtWash, srtWaste, srtDry, srtRediwash);

    // neue Klasse, díe z.B. Namenskonventionen für Racks beinhaltet
    TRackDefinitions = class
    public
        class function GetSpecialRackNameGeneral(aRackType: TSpecialRackType): string;
        class function GetSpecialRackNameForArm(aRackType: TSpecialRackType;
            const aUsedArmName: string): string;
        class function RackIsSpecialRack(aRackType: TSpecialRackType;
            const aRackName, aUsedArmName: string): boolean;
        class function RackIsWashOrWaste(const aRackName, aUsedArmName: string): boolean;
        class function RackIsWashOrRediWash(const aRackName, aUsedArmName: string): boolean;
        class function RackIsGeneralSpecialRack(const aRackName: string): boolean;
    end;


implementation


uses
    SysUtils,
    ArrayUtils;

function gmGetRackPositionString(aPosition, aNoOfRows: integer): string;
begin
    result := CHR(pred(aPosition) mod aNoOfRows + 65) + InttoStr(pred(aPosition) div aNoOfRows + 1);
end;

function gmXRackPosToRackIDPos(aXRackPos: TXRackPosition): TRackIDPosition;
begin
    result.Pos := aXRackPos.Pos;

    if Assigned(aXRackPos.Rack) then
    begin
        result.Rack := aXRackPos.Rack.Name;
        result.RackID := aXRackPos.Rack.RackID;
    end
    else
    begin
        result.Rack := '';
        result.RackID := '';
    end;
end;

function gmMakexRackPos(aRack: TRack; aPos: integer): TXRackPosition;
begin
    result.Rack := aRack;
    result.Pos := aPos;
end;

function gmRackIDPosToRackPos(RackIDPos: TRackIDPosition): TRackPosition;
begin
    result.Rack := RackIDPos.Rack;
    result.Pos := RackIDPos.Pos;
end;

function gmMakeRackPos(aRackName: string; aRackPos: integer): TRackPosition;
begin
    result.Rack := aRackName;
    result.Pos := aRackPos;
end;

function gmMakeRackIDPos(aRackName: string; aRackPos: integer; aRackID: string): TRackIDPosition;
begin
    result.Rack := aRackName;
    result.Pos := aRackPos;
    result.RackID := aRackID;
end;

function gmMakeDefRackIDPos(): TRackIDPosition;
begin
    result := gmMakeRackIDPos('', 0, '');
end;

function gmMakeRackArray(const aRack: TRack; aCount: integer): TArray<TRack>;
var
    x: integer;
begin
    SetLength(result, aCount);
    for x := 0 to high(result) do
        result[x] := aRack;
end;

{ TRackDefinitions }

const
    STR_RACKNAME_REDIWASH = 'RWASH';
    STR_RACKNAME_DRY = 'DRY';
    STR_RACKNAME_WASH = 'WASH';
    STR_RACKNAME_WASTE = 'WASTE';

class function TRackDefinitions.GetSpecialRackNameGeneral(aRackType: TSpecialRackType): string;
begin
    case (aRackType) of
        srtWash:
            result := STR_RACKNAME_WASH;
        srtWaste:
            result := STR_RACKNAME_WASTE;
        srtDry:
            result := STR_RACKNAME_DRY;
        srtRediwash:
            result := STR_RACKNAME_REDIWASH;
    end;
end;

class function TRackDefinitions.GetSpecialRackNameForArm(aRackType: TSpecialRackType;
    const aUsedArmName: string): string;
begin
    result := GetSpecialRackNameGeneral(aRackType) + '_' + aUsedArmName;
end;

class function TRackDefinitions.RackIsSpecialRack(aRackType: TSpecialRackType;
    const aRackName, aUsedArmName: string): boolean;
begin
    result := (aRackName = GetSpecialRackNameGeneral(aRackType)) or
        (aRackName = GetSpecialRackNameForArm(aRackType, aUsedArmName));
end;

class function TRackDefinitions.RackIsWashOrWaste(const aRackName, aUsedArmName: string): boolean;
begin
    result := TRackDefinitions.RackIsSpecialRack(srtWash, aRackName, aUsedArmName) or
        TRackDefinitions.RackIsSpecialRack(srtWaste, aRackName, aUsedArmName);
end;

class function TRackDefinitions.RackIsWashOrRediWash(const aRackName, aUsedArmName: string): boolean;
begin
    result := TRackDefinitions.RackIsSpecialRack(srtWash, aRackName, aUsedArmName) or
        TRackDefinitions.RackIsSpecialRack(srtRediWash, aRackName, aUsedArmName);
end;

class function TRackDefinitions.RackIsGeneralSpecialRack(const aRackName: string): boolean;
begin
    // wird im Layouter verwendet
    result := (aRackname = STR_RACKNAME_WASH) or (aRackname = STR_RACKNAME_WASTE) or
        (aRackname = STR_RACKNAME_DRY) or (aRackname = STR_RACKNAME_REDIWASH);
end;

{ TXRackPositionUtils }

class function TXRackPositionUtils.ExtractPositionsFromArray(aRP: TArray<TXRackPosition>): TArray<integer>;
var
    x: integer;
begin
    SetLength(result, Length(aRP));
    for x := 0 to high(aRP) do
    begin
        result[x] := aRP[x].Pos;
    end;
end;

class function TXRackPositionUtils.ExtractRacksFromArray(aRP: TArray<TXRackPosition>): TArray<TRack>;
var
    x: integer;
begin
    SetLength(result, Length(aRP));
    for x := 0 to high(aRP) do
    begin
        result[x] := aRP[x].Rack;
    end;
end;

class function TXRackPositionUtils.ExtractSingleRackFromArray(const aRP: TArray<TXRackPosition>): TRack;
var
    xLastRack: TRack;
    x: integer;
begin
    xLastRack := nil;

    // gibt es geanau ein Rack, das angefahren wird?
    for x := 0 to high(aRP) do
    begin
        if Assigned(aRP[x].Rack) then
        begin
            if Assigned(xLastRack) and (xLastRack <> aRP[x].Rack) then
                EXIT(nil); // Es gibt mehrere Racks: Rückgabewert = nil

            xLastRack := aRP[x].Rack;
        end;
    end;
    EXIT(xLastRack);
end;

class function TXRackPositionUtils.ExtractSingleRackFromArray(const aRacks: TArray<TRack>): TRack;
var
    xLastRack: TRack;
    x: integer;
begin
    xLastRack := nil;

    // gibt es geanau ein Rack, das angefahren wird?
    for x := 0 to high(aRacks) do
    begin
        if Assigned(aRacks[x]) then
        begin
            if Assigned(xLastRack) and (xLastRack <> aRacks[x]) then
                EXIT(nil); // Es gibt mehrere Racks: Rückgabewert = nil

            xLastRack := aRacks[x];
        end;
    end;
    EXIT(xLastRack);
end;

class function TXRackPositionUtils.ExtractSingleRackNameFromArray(aRP: TArray<TXRackPosition>): string;
var
    xRack: TRack;
begin
    xRack := ExtractSingleRackFromArray(aRP);
    if Assigned(xRack) then
        EXIT(xRack.name)
    else
        EXIT('');
end;

class function TXRackPositionUtils.GetEmptyXRackPositions(aTipCount: integer): TArray<TXRackPosition>;
var
    x: integer;
begin
    SetLength(result, aTipCount);
    for x := 0 to high(result) do
    begin
        result[x].Rack := nil;
        result[x].Pos := 0;
    end;
end;

class function TXRackPositionUtils.RackPositionsToBracketText(const aRackPositions
    : TArray<TXRackPosition>): string;
begin
    result := 'Rack-[' + RackRacksToText(aRackPositions) + '] Pos-[' + RackPosToText(aRackPositions) + ']';
end;

class function TXRackPositionUtils.RackPosToText(const aRackPositions: TArray<TXRackPosition>): string;
var
    xDelim: string;
    x: integer;
begin
    result := '';
    xDelim := '';
    for x := 0 to high(aRackPositions) do
    begin
        if (x = 1) then
            xDelim := TFormatUtils.CurrentListSeparator;
        result := result + xDelim + IntToStr(aRackPositions[x].Pos);
    end;
end;

class function TXRackPositionUtils.RackRacksToText(const aRackPositions: TArray<TXRackPosition>): string;
var
    xDelim: string;
    xCurrentRackName, xLastRackName: string;
    x: integer;
    xDifferentRacks: boolean;
begin
    result := '';
    xDelim := '';
    xLastRackName := '';
    xDifferentRacks := false;
    for x := 0 to high(aRackPositions) do
    begin
        if Assigned(aRackPositions[x].Rack) then
            xCurrentRackName := aRackPositions[x].Rack.name
        else
            xCurrentRackName := '';

        if (xCurrentRackName <> '') then
        begin
            if (xLastRackName <> '') and (xLastRackName <> xCurrentRackName) then
                xDifferentRacks := true;
            // es werden verschiedene Racks benutzt

            xLastRackName := xCurrentRackName;
        end;

        if (x = 1) then
            xDelim := TFormatUtils.CurrentListSeparator;
        result := result + xDelim + xCurrentRackName;
    end;

    if not xDifferentRacks then
        EXIT(xLastRackName); // wie früher: einfach den einen Racknamen ausgeben
end;

class function TXRackPositionUtils.XRackPosToRackPosArray(aRackPositions: TArray<TXRackPosition>)
    : TArray<TRackPosition>;
var
    x: integer;
begin
    SetLength(result, Length(aRackPositions));
    for x := 0 to Length(aRackPositions) - 1 do
    begin
        result[x].Pos := aRackPositions[x].Pos;

        if Assigned(aRackPositions[x].Rack) then
            result[x].Rack := aRackPositions[x].Rack.Name
        else
            result[x].Rack := '';
    end;
end;

class procedure TXRackPositionUtils.PaintTubePositions(const aRackPositions: TArray<TXRackPosition>;
    const aColorType: TRackWellDisplayType);
var
    x: integer;
begin
    for x := 0 to Length(aRackPositions) - 1 do
    begin
        if not Assigned(aRackPositions[x].Rack) then
            CONTINUE;

        aRackPositions[x].Rack.PaintTubePos(aRackPositions[x].Pos, aColorType);
    end;
end;


end.
