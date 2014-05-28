{ --------------------------------------------------------------------------------------------------
  Copyright © 2008 Zinsser Analytic GmbH - All rights reserved.
  Author       : Payman Kamali (pk)
  Description  : Classes and math functions for Coordinate System Relation
  --------------------------------------------------------------------------------------------------
  Revision History:
  date     op  method                       track-no improvement/change
  -------- --  ---------------------------  -------- -----------------------------------------------
  23.06.08 pk                                TN4139  Initial Revision
  02.07.08 pk  fPosZ                         TN4139  New
  07.07.08 pk  fSizeZ                        TN4139  New
  04.11.09 pk                                TN4843  Replace TObjectList, TStringList, TStrings with Generics Lists and TStringArray
  20.09.11 wl  fVolumeData                   TN5723   speichert alle Volumen mit Amount, ID, Farbe
  20.09.11 wl  ShowVolumeData                TN5723   berechnet die (Misch-)Farbe der Position und zeigt sie
  28.10.11 wl  TRackWellDisplayType          TN5725   Standard-Farben für bestimmte Vorgänge
  28.10.11 wl                                TN5725   Farbdarstellung überarbeitet
  03.11.11 wl  TWellVolumePart               TN5725   neu
  03.11.11 wl  AddVolume                     TN5725   alle Volumen-Zugaben und -Wegnahmen werden jetzt hier gespeichert
  17.11.11 wl  StorageSubstID                TN5725   nach StaorageSubstID kann jetzt gesucht werden
  27.11.11 wl  fStorageDeadVolume            TN5730   MinVolume aus SubstanceSet wird als Totvolumen verwendet
  09.12.11 wl  StorageMaxVolume              TN5761   neu: Wird zwar noch nicht verwendet, aber gespeichert
  26.01.12 wl  AddAspVolume                  TN5785   Berechnungsfehler korrigiert
  15.02.12 wl  MakeHintText                  TN5804   Prozentangaben nur zeigen, wenn Volumen > 0
  10.09.12 wl  TDeadVolume,TUseDeadVolume    TN5979   neu
  04.12.12 pp  Declaration                   TN6007   2 neue property hinzugefügt (PosX, PosY)
  30.01.13 wl  fStorageData                  TN6076   neu als read/write-property
  30.01.13 wl  ResetStorageData              TN6076   setzt Storage-Werte zurück
  04.02.13 wl  MakeHintText                  TN6081   Bestndteile nicht nur in Prozent, sondern auch absolut
  11.02.13 wl  StorageData                   TN6078   macht StorageData von außen beschreibbar
  11.02.13 wl  MakeHintText                  TN6078   mehrere kleine Verbessrungen
  -------------------------------------------------------------------------------------------------- }

unit RackWell;


interface


uses
    Generics.Collections,
    Graphics,
    ColorUtilities,
    AppTypes,
    CommonTypes,
    RackWellGraphics,
    CoordSystemMath,
    LayoutElement,
    LayoutElementGraphicsInfo;

type
    TUseDeadVolume = (NoDeadVolume, MinVol1, MinVol2, MinVol1And2);

    TDeadVolume = record
    public
        class function Calculate(const aMinVolume1, aMinVolume2: double; aUseType: TUseDeadVolume)
            : double; static;
    end;

    TWellVolumePart = class
    private
        fID: string;
        fAmount: double;
        fColor: TColor;
    public
        property ID: string read fID;
        property Amount: double read fAmount write fAmount;
        property Color: TColor read fColor;
        constructor Create(aID: string; aAmount: double; aColor: TColor);
    end;

    TRackWellDisplayType = (default, TubeMove, Disposables, Highlight, Other);

    TRackMakeHintTextCallback = function(aSender: TObject): string of object;

    TRackWellStorageData = record
        IsStorage: boolean;
        SubstID: string;
        SubstColor: integer;
        MinVolume1: double;
        MinVolume2: double;
        MaxVolume: double;
        SetName: string;
    end;

    TRackWell = class(TLayoutElement)
    strict private
        fRack: TObject;
        fSizeX, fSizeY, fSizeZ: TPosMM;
        fPosX, fPosY, fPosZ: TPosMM;
        fWellNr: integer;
        fRow: integer;
        fCol: integer;
        fRackColor: integer;
        fDefaultColor: integer;
        fWellShape: TRackWellShape;
        fRackMakeHintTextCallback: TRackMakeHintTextCallback;
        fVolumeData: TObjectList<TWellVolumePart>;
        fDisplayType: TRackWellDisplayType;
        fStorageData: TRackWellStorageData;

        function GetGraphics: TRackWellGraphics;
        function DisplayTypeToColor(const aColorType: TRackWellDisplayType): integer;

        procedure SetAndPaintGraphisColor(const aColor: integer);
        function GetTotalVolume: double;
        function AddAspVolume(aAmount: double; const aSubstID: string; aColor: integer)
            : TArray<TSubstIDAndVol>;
        procedure AddDispVolume(aAmount: double; aSubstID: string; aColor: integer);
        function GetColorArray(): TArray<TColorAndVol>;
    protected
        procedure DoInitGraphics(); override;
        function MakeHintText: string; override;
    public const
        cSubstIDCalculated = 'Calculated by liquid detection';
    public
        constructor Create(aWellNr: integer);
        destructor Destroy(); override;

        procedure SetUpWell(aRow, aCol: integer; aSizeX, aSizeY, aSizeZ, aPosX, aPosY, aPosZ: TPosMM;
            aColor: integer; aWellShape: TRackWellShape);

        procedure ShowVolumeData(aStandardColor: integer);
        procedure SetAndPaintDisplayType(const aColorType: TRackWellDisplayType);
        function AddVolume(aAmount: double; const aSubstID: string; aColor: integer; aStandardColor: integer;
            aUseTubeIDIfNoSubstIDExists: boolean): TArray<TSubstIDAndVol>;
        procedure DeleteVolumes();
        procedure ResetStorageData();
        class function GetResetStorageData(): TRackWellStorageData;

        property TotalVolume: double read GetTotalVolume;
        property Graphics: TRackWellGraphics read GetGraphics;
        property Rack: TObject read fRack write fRack;
        property WellNr: integer read fWellNr;
        property SizeX: TPosMM read fSizeX;
        property SizeY: TPosMM read fSizeY;
        property PosX: TPosMM read fPosX;
        property PosY: TPosMM read fPosY;
        property PosZ: TPosMM read fPosZ;
        property RackMakeHintTextCallback: TRackMakeHintTextCallback read fRackMakeHintTextCallback
            write fRackMakeHintTextCallback;
        property VolumeData: TObjectList<TWellVolumePart>read fVolumeData;
        property DisplayType: TRackWellDisplayType read fDisplayType;
        property StorageData: TRackWellStorageData read fStorageData write fStorageData;
    end;


implementation


uses
    SysUtils,
    SamGlobe;

{ TDeadVolume }

class function TDeadVolume.Calculate(const aMinVolume1, aMinVolume2: double;
    aUseType: TUseDeadVolume): double;
begin
    case aUseType of
        TUseDeadVolume.NoDeadVolume:
            EXIT(0);
        TUseDeadVolume.MinVol1:
            EXIT(aMinVolume1);
        TUseDeadVolume.MinVol2:
            EXIT(aMinVolume2);
        TUseDeadVolume.MinVol1And2:
            EXIT(aMinVolume1 + aMinVolume2);
        else
            raise Exception.Create('Undefined dead volume');
    end;
end;

{ TWellVolumePart }

constructor TWellVolumePart.Create(aID: string; aAmount: double; aColor: TColor);
begin
    inherited Create;
    fID := aID;
    fAmount := aAmount;
    fColor := aColor;
end;

{ TRackWell }

constructor TRackWell.Create(aWellNr: integer);
begin
    inherited Create();

    fWellNr := aWellNr;
    fDisplayType := TRackWellDisplayType.default;
    fVolumeData := TObjectList<TWellVolumePart>.Create;
    ResetStorageData();
end;

procedure TRackWell.DeleteVolumes;
begin
    fVolumeData.Clear;
end;

destructor TRackWell.Destroy;
begin
    FreeAndNil(fVolumeData);
    inherited;
end;

function TRackWell.AddAspVolume(aAmount: double; const aSubstID: string; aColor: integer)
    : TArray<TSubstIDAndVol>;
var
    x: integer;
    xTotalVolume, xPartVolume: double;
    xData: TWellVolumePart;
begin
    if fVolumeData.Count = 0 then
    begin
        // Neuen Eintrag hinzufügen
        xData := TWellVolumePart.Create(aSubstID, aAmount, aColor);
        fVolumeData.Add(xData);

        // entnommenes Volumen zurückgeben
        SetLength(result, 1);
        result[0].SubstID := aSubstID;
        result[0].Volume := -aAmount; // aAmount ist negativ - result ist positiv
        EXIT;
    end;

    if fVolumeData.Count = 1 then
    begin
        fVolumeData[0].Amount := fVolumeData[0].Amount + aAmount;

        // entnommenes Volumen zurückgeben
        SetLength(result, 1);
        result[0].SubstID := fVolumeData[0].ID;
        result[0].Volume := -aAmount; // aAmount ist negativ - result ist positiv
        EXIT;
    end;

    // Mehrere Einträge: Von allen den gleichen Anteil abziehen
    xTotalVolume := self.GetTotalVolume;
    SetLength(result, fVolumeData.Count);
    for x := 0 to fVolumeData.Count - 1 do
    begin
        // aAmount ist negativ - xPartVolume ist positiv
        xPartVolume := -(aAmount * fVolumeData[x].Amount / xTotalVolume);
        fVolumeData[x].Amount := fVolumeData[x].Amount - xPartVolume;

        // entnommenes Volumen zurückgeben
        result[x].Volume := xPartVolume;
        result[x].SubstID := fVolumeData[x].ID;
    end;
end;

procedure TRackWell.AddDispVolume(aAmount: double; aSubstID: string; aColor: integer);
var
    x: integer;
    xData: TWellVolumePart;
begin
    if (aSubstID = cSubstIDCalculated) then
    begin
        // Volumen neu kalkuliert: Von allen den gleichen Anteil abziehen
        AddAspVolume(aAmount, aSubstID, aColor);
        EXIT;
    end;

    for x := 0 to fVolumeData.Count - 1 do
    begin
        if fVolumeData[x].ID = aSubstID then
        begin
            // Subst-ID gefunden: Volumen zu bestehenden hinzufügen
            fVolumeData[x].Amount := fVolumeData[x].Amount + aAmount;
            EXIT;
        end;
    end;

    // Subst-ID nicht gefunden: Neuen Eintrag hinzufügen
    xData := TWellVolumePart.Create(aSubstID, aAmount, aColor);
    fVolumeData.Add(xData);
end;

function TRackWell.AddVolume(aAmount: double; const aSubstID: string; aColor, aStandardColor: integer;
    aUseTubeIDIfNoSubstIDExists: boolean): TArray<TSubstIDAndVol>;
begin
    { TODO : Wenn Tube ID im well ist, könnte man auch UseTubeIDIfNoSubstID wieder einbauen }
    // if (aUseTubeIDIfNoSubstIDExists) and (xSubstID = '') then
    // xSubstId := xTubeID;

    result := nil;

    if aAmount < 0 then
    begin
        result := AddAspVolume(aAmount, aSubstID, aColor);
    end;
    if aAmount > 0 then
    begin
        AddDispVolume(aAmount, aSubstID, aColor);
    end;

    // Farbe anzeigen
    self.ShowVolumeData(aStandardColor);
end;

procedure TRackWell.SetAndPaintDisplayType(const aColorType: TRackWellDisplayType);
var
    xColor: integer;
begin
    if (self.DisplayType = aColorType) then
        EXIT;

    // Wenn Farben auf 0 gesetzt wurden: nicht zeichnen
    xColor := DisplayTypeToColor(aColorType);
    if (xColor = 0) and (aColorType <> TRackWellDisplayType.default) then
        EXIT;

    fDisplayType := aColorType;
    SetAndPaintGraphisColor(xColor);
end;

procedure TRackWell.SetAndPaintGraphisColor(const aColor: integer);
begin
    if (self.Graphics.Color <> aColor) then
    begin
        self.Graphics.Color := aColor;
        self.SceneChanged();
    end;
end;

procedure TRackWell.ResetStorageData();
begin
    fStorageData := GetResetStorageData();
end;

function TRackWell.DisplayTypeToColor(const aColorType: TRackWellDisplayType): integer;
begin
    case aColorType of
        TRackWellDisplayType.TubeMove:
            EXIT(gTubeColors.TubeMove); // oder lime
        TRackWellDisplayType.Other:
            EXIT(gTubeColors.Other);

        TRackWellDisplayType.Disposables:
            EXIT(clPurple);
        TRackWellDisplayType.Highlight:
            EXIT(clRed);

        else
            EXIT(fDefaultColor);
    end;
end;

procedure TRackWell.SetUpWell(aRow, aCol: integer; aSizeX, aSizeY, aSizeZ, aPosX, aPosY, aPosZ: TPosMM;
    aColor: integer; aWellShape: TRackWellShape);
begin
    fRow := aRow;
    fCol := aCol;
    fSizeX := aSizeX;
    fSizeY := aSizeY;
    fSizeZ := aSizeZ;
    fPosX := aPosX;
    fPosY := aPosY;
    fPosZ := aPosZ;
    fRackColor := aColor;
    fDefaultColor := aColor;
    fWellShape := aWellShape;
    fCoordCalculator.CoordSystem.TranslateX := fPosX;
    fCoordCalculator.CoordSystem.TranslateY := fPosY;
    fCoordCalculator.CoordSystem.TranslateZ := fPosZ;

    // Update Graphics Info
    self.Graphics.Color := DisplayTypeToColor(fDisplayType);
    with self.Graphics.GraphicsInfo do
    begin
        name := fName;
        SizeX := fSizeX;
        SizeY := fSizeY;
        SizeZ := fSizeZ;
        WellShape := fWellShape;
    end;
    self.Graphics.PosX := fPosX;
    self.Graphics.PosY := fPosY;
    self.Graphics.PosZ := fPosZ;

    self.Graphics.UpdateGraphicsInfo();
end;

function TRackWell.GetColorArray(): TArray<TColorAndVol>;
var
    x: integer;
begin
    SetLength(result, fVolumeData.Count);
    for x := 0 to fVolumeData.Count - 1 do
    begin
        result[x].Color := fVolumeData[x].Color;
        result[x].Amount := fVolumeData[x].Amount;
    end;
end;

procedure TRackWell.ShowVolumeData(aStandardColor: integer);
var
    xColor: integer;
begin
    if (fStorageData.SubstColor > 0) then
        xColor := fStorageData.SubstColor // Storage-Positionen sollen immer in dieser Farbe erscheinen
    else
        xColor := TColorUtilities.AdditiveMix(GetColorArray());

    // Farbe 0: nicht schwarz, sondern gar keine Farbe
    if (xColor = 0) then
    begin
        if (aStandardColor = 0) then
            fDefaultColor := fRackColor // Farbe des Racks
        else
            fDefaultColor := aStandardColor // Aspirate- oder Dispense-Standardfarbe
    end
    else
        fDefaultColor := xColor; // berechnete Farbe

    SetAndPaintGraphisColor(fDefaultColor);
end;

procedure TRackWell.DoInitGraphics;
begin
    fGraphics := TRackWellGraphics.Create();
end;

function TRackWell.GetGraphics: TRackWellGraphics;
begin
    result := fGraphics as TRackWellGraphics;
end;

class function TRackWell.GetResetStorageData: TRackWellStorageData;
begin
    result.IsStorage := false;
    result.SubstID := '';
    result.SubstColor := 0;
    result.MinVolume1 := 0;
    result.MinVolume2 := 0;
    result.MaxVolume := 0;
    result.SetName := '';
end;

function TRackWell.GetTotalVolume: double;
var
    x: integer;
begin
    result := 0.0;
    for x := 0 to fVolumeData.Count - 1 do
    begin
        result := result + fVolumeData[x].Amount;
    end;
end;

function TRackWell.MakeHintText: string;
var
    x: integer;
    xTotalVolume: double;
begin
    result := '';
    AddHintLine('Well', result);
    AddHintSubLine('Pos: ' + IntToStr(fWellNr), result);

    if (fVolumeData.Count > 0) then
    begin
        xTotalVolume := self.GetTotalVolume;

        if (xTotalVolume > 0) then
        begin
            if (fVolumeData.Count > 1) then
            begin
                // Volumenangaben runden auf ein tausendstel Mikroliter
                AddHintSubLine('Total volume: ' + FloatToStr(Round(xTotalVolume * 1000) / 1000) +
                    ' µL', result);

                for x := 0 to fVolumeData.Count - 1 do
                    AddHintSubLine(' ' + FloatToStr(Round(fVolumeData[x].Amount * 1000) / 1000) + ' µL ' +
                        fVolumeData[x].ID + ' (' +
                        FloatToStr(Round(fVolumeData[x].Amount / self.GetTotalVolume * 10000) / 100) +
                        ' %)', result);
            end
            else if (fVolumeData.Count = 1) then
            begin
                // Volumen und Subst-ID in einer Zeile
                AddHintSubLine('Volume: ' + FloatToStr(Round(xTotalVolume * 1000) / 1000) + ' µL ' +
                    fVolumeData[0].ID, result);
            end;
        end;
    end;
    if (fStorageData.IsStorage) then
    begin
        if (fStorageData.SubstID <> '') then
            AddHintSubLine('Storage/tracking for ' + fStorageData.SubstID, result)
        else
            AddHintSubLine('Storage/tracking position', result);

        if (fStorageData.MinVolume1 > 0) then
            AddHintSubLine(' Min.volume: ' + FloatToStr(Round(fStorageData.MinVolume1 * 1000) /
                1000), result);
        if (fStorageData.MinVolume2 > 0) then
            AddHintSubLine(' Min.volume 2: ' + FloatToStr(Round(fStorageData.MinVolume2 * 1000) /
                1000), result);
        if (fStorageData.SetName <> '') then
            AddHintSubLine(' Substance set: ' + fStorageData.SetName, result);
    end;

    if Assigned(fRackMakeHintTextCallback) then
        AddHintLine(fRackMakeHintTextCallback(self), result);
end;


end.
