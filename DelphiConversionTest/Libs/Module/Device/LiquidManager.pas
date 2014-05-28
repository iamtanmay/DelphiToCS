{ --------------------------------------------------------------------------------------------------
  Copyright © 2005 Zinsser Analytic GmbH - All rights reserved.
  Author       : Wolfgang Lyncke (wl)
  Description  : TSystemLiquidManager : this object links pump devices and system liquids together
  --------------------------------------------------------------------------------------------------
  Revision History:
  date     op  method                       track-no improvement/change
  -------- --  ---------------------------  -------- -----------------------------------------------
  16.02.05 wl                               TN2269   initial revision
  16.02.05 wl  GetGlobalSystemLiquid        TN2269   ersetzt gActSystemLiq, ist aber schlecht und sollte nur im Notfall benutzt werden
  16.02.05 wl  ChangeLiquid                 TN2269   ersetzt gLiquuids.ChangeLiquid - kann so viele Valves schalten wie nötig
  16.02.05 wl  Create                       TN2269   erzeugt gLiquids-Objekt
  23.05.06 pk  GetDiluentIndicesForPipDevice TN3112  New : return indices of all liquids connected to given pumps
  30.05.06 pk  GetDiluentIndicesForPipDevice TN3121  Do not raise exception if sixwayvalve device is not found
  18.09.06 pk                               TN3227.1 DeviceMain changed to SystemLiquidManager
  03.12.06 wl                               TN3243   alle ErrBox-Aufrufe durch ErrBoxSimple oder ErrBoxSerialIntf ersetzt
  08.03.07 wl                               TN3620   uses geändert
  03.07.08 wl                                         TN4157
  10.11.08 pk  ChangeLiquid                 TN4307   Show error box
  18.12.08 wl  IsLiquidReachableFromTips    TN4185   wenn Dilutor nicht existiert, gibt es auch keine Fehlermeldung
  10.08.09 wl                               TN4702   Strings werden jetzt direkt geladen
  27.08.09 pk                               TN4753   references to gErrorManager, gErrorMessageFactory removed
  27.08.09 pk  ChangeLiquid                 TN4753   doesn't work
  28.08.09 pk                               TN4753   Code moved to TSystemLiquidManagerExt
  09.06.10 pk                               TN5116   new TSystemLiquidRouteFinder, TSystemLiquidSettingsFinder
  11.06.10 pk  GetPositionsToDestLiq        TN5116   InputPortNames[x] istead of InputPortName[x]
  18.06.12 wl  TSystemLiquidManager         TN5899   Create und Destroy entfernt
  24.04.13 wl  En-/DecodeSystemLiquidIdent  TN6137   von Liquids hierher
  18.09.13 wl  PipDeviceGetLiquidIndices    TN6252.3 neu für Flush-Dialog
  25.09.13 wl  ChangeLiquid                 TN6258   Rückgabewert ist jetzt TIPMAP
  08.11.13 wl                               TN6298   div. Korrekturen, damit der Default-SystemInputPort wieder funktioniert
  -------------------------------------------------------------------------------------------------- }

unit LiquidManager;


interface


uses
    Generics.Collections,
    AppTypes,
    IntfPipDevice,
    IntfPipPumpDevice,
    IntfXWayValveDevice,
    Liquids;

// here are some definitions and notes on the System Liquid Management:
// Definitions:
// InputPortName: this can be either the name of an XWayValve or a LiquidIdent.
// LiquidIdent  : this has the format SYS##. Example SYS05
// SysLiq       : SysLiq is a more modern name for "Diluent"
// SysLiqIndex  : this index can be used to get a TSystemLiquid from the gLiquids array.
// It can be derived from a LiquidIdent by using DecodeSystemLiquidIdent( LiquidIdent )
// SysLiqName   : this is the name of the liquid such as H20. In the TSystemLiquid object it is referred to as Diluent

// Notes        : Do not put any system liquid functions directly into the pump device.  The pumpdevice should not know anything
// about xwayvalves or system liquids

type
    TDeviceMainMode = (dfmReal, dfmSimulated, dfmFake);

    TXWayValvePos = record
        DeviceName: string;
        Position: integer;
    end;

    TSystemLiquidSettingsFinder = class abstract
    public
        function GetXWayValveInputPortNames(const aXWayValveDeviceName: string): TArray<string>;
            virtual; abstract;
        function XWayValveExists(const aXWayValveDeviceName: string): boolean; virtual; abstract;
        function GetPipPumpDeviceName(const aPipDeviceName: string; const aTipIndex: integer): string;
            virtual; abstract;
        function PipDeviceTipCount(const aPipDeviceName: string): integer; virtual; abstract;
        function PipPumpExists(const aPipPumpDeviceName: string): boolean; virtual; abstract;
    end;

    TSystemLiquidRouteFinder = class
    private const
        STR_SYSTEMLIQUID_PREFIX = 'SYS';
    private
        fSettingsFinder: TSystemLiquidSettingsFinder;

        procedure GetPositionsToDestLiq(const aXWayValveName: string; var aPositions: TArray<TXWayValvePos>;
            aDestLiqIndex: integer);
        class procedure AddPositionToXWayValve(const aXWayValveName: string;
            var aPositions: TArray<TXWayValvePos>; aPosition: integer);
        class procedure CheckRecursionsForXWayValve(const aXWayValveName: string;
            const aPositions: TArray<TXWayValvePos>);
        class procedure RemoveLastPositionFromXWayValve(var aPositions: TArray<TXWayValvePos>);
    public
        class function EncodeSystemLiquidIdent(aIndex: integer): string;
        class function DecodeSystemLiquidIdent(aIdent: string): integer;
    public
        constructor Create(const aSettingsFinder: TSystemLiquidSettingsFinder);
        destructor Destroy(); override;

        function GetXWayValvePositions(const aInputPortName: string; aSysLiqIndex: integer)
            : TArray<TXWayValvePos>;
        procedure GetReachableLiquidIdents(aXWayValveName: string; aLiquidIdents: TList<string>);
        function XWayValveExists(const aXWayValveDeviceName: string): boolean;
    end;

    TSystemLiquidManager = class abstract
    public
        function FindCurrentLiquid(aInputPortName: string): TSystemLiquid; virtual; abstract;

        function ChangeLiquid(aPipDevice: IPipDevice; aTips: TIPMAP; aSysLiqIndex: integer): TIPMAP;
            virtual; abstract;
        function PumpGetCurrentSysIndex(aPipPump: IPipPumpDevice): integer; virtual; abstract;
        function PumpGetCurrentSysName(aPipPump: IPipPumpDevice): string; virtual; abstract;
        function PumpGetCurrentSysLiquid(aPipPump: IPipPumpDevice): TSystemLiquid; virtual; abstract;
        function IsLiquidReachableFromTips(aPipDevice: IPipDevice; aTips: TIPMAP; aSysLiqIndex: integer;
            out oErrorText: string): boolean; virtual; abstract;
        function PipDeviceGetLiquidIndices(aPipDevice: IPipDevice; aTips: TIPMAP;
            aPumpIndex1, aPumpIndex2: integer): TArray<integer>; virtual; abstract;
    end;

var
    gSysLiqManager: TSystemLiquidManager;


implementation


uses
    SysUtils,
    GeneralTypes,
    TipMapUtils,
    Device;

{ TSystemLiquidRouteFinder }

constructor TSystemLiquidRouteFinder.Create(const aSettingsFinder: TSystemLiquidSettingsFinder);
begin
    inherited Create();
    fSettingsFinder := aSettingsFinder;
end;

destructor TSystemLiquidRouteFinder.Destroy;
begin
    FreeAndNil(fSettingsFinder);
    inherited;
end;

procedure TSystemLiquidRouteFinder.GetPositionsToDestLiq(const aXWayValveName: string;
    var aPositions: TArray<TXWayValvePos>; aDestLiqIndex: integer);
var
    x, xSysLiqIndex, xHigh: integer;
    xInputPortName: string;
    xInputPortNames: TArray<string>;
begin
    self.CheckRecursionsForXWayValve(aXWayValveName, aPositions);

    xInputPortNames := fSettingsFinder.GetXWayValveInputPortNames(aXWayValveName);

    for x := 0 to Length(xInputPortNames) - 1 do
    begin

        xInputPortName := xInputPortNames[x];

        // ist es ein System liquid?
        xSysLiqIndex := DecodeSystemLiquidIdent(xInputPortName);

        // SysLiq gefunden
        if (xSysLiqIndex = aDestLiqIndex) then
        begin
            self.AddPositionToXWayValve(aXWayValveName, aPositions, x);
            EXIT; // Liquid gefunden -> raus
        end;

        // nicht das richtige SystemLiquid
        if (xSysLiqIndex >= 0) then
            CONTINUE;

        if not fSettingsFinder.XWayValveExists(xInputPortName) then
            raise Exception.CreateFmt('Device %s not found!', [xInputPortName]); // Es ist kein Device

        // im nächsten Device nach der Systemflüssigkeit suchen
        self.AddPositionToXWayValve(xInputPortName, aPositions, x);
        xHigh := high(aPositions);
        GetPositionsToDestLiq(aXWayValveName, aPositions, aDestLiqIndex);
        if (xHigh >= high(aPositions)) then
            RemoveLastPositionFromXWayValve(aPositions); // Es wurde nichts gefunden!
    end;
end;

function TSystemLiquidRouteFinder.GetXWayValvePositions(const aInputPortName: string; aSysLiqIndex: integer)
    : TArray<TXWayValvePos>;
var
    xSysLiqIndex: integer;
begin
    result := nil;
    xSysLiqIndex := DecodeSystemLiquidIdent(aInputPortName);

    if (xSysLiqIndex < 0) then
    begin
        // LiquidIdent kann der Name eines Device sein: suche danach!
        if not fSettingsFinder.XWayValveExists(aInputPortName) then
            raise Exception.CreateFmt('Device %s not found!', [aInputPortName]);
        GetPositionsToDestLiq(aInputPortName, result, aSysLiqIndex);
    end;
end;

procedure TSystemLiquidRouteFinder.GetReachableLiquidIdents(aXWayValveName: string;
    aLiquidIdents: TList<string>);
// this procedure adds all LiquidIdents which are reachable via this XWayvalve to the list aIdentNames
// Note: a LiquidIdent has the Syntax SYS##. For example: SYS05
var
    x: integer;
    xInputPortName: string;
    xInputPortNames: TArray<string>;
begin
    xInputPortNames := fSettingsFinder.GetXWayValveInputPortNames(aXWayValveName);
    for x := 0 to Length(xInputPortNames) - 1 do
    begin
        xInputPortName := xInputPortNames[x];

        if fSettingsFinder.XWayValveExists(xInputPortName) then
        begin
            // if inputportname is a device, make a recursive call
            GetReachableLiquidIdents(xInputPortName, aLiquidIdents);
        end
        else
        begin
            // if the device is not a device, we assume that it is an liquidident
            aLiquidIdents.Add(xInputPortName);
        end;
    end;
end;

class procedure TSystemLiquidRouteFinder.AddPositionToXWayValve(const aXWayValveName: string;
    var aPositions: TArray<TXWayValvePos>; aPosition: integer);
var
    xHigh: integer;
begin
    xHigh := high(aPositions);
    SetLength(aPositions, xHigh + 2); // einen mehr
    aPositions[xHigh + 1].DeviceName := aXWayValveName;
    aPositions[xHigh + 1].Position := aPosition;
end;

class procedure TSystemLiquidRouteFinder.RemoveLastPositionFromXWayValve
    (var aPositions: TArray<TXWayValvePos>);
var
    xHigh: integer;
begin
    xHigh := high(aPositions);
    SetLength(aPositions, xHigh); // einen weniger
end;

function TSystemLiquidRouteFinder.XWayValveExists(const aXWayValveDeviceName: string): boolean;
begin
    EXIT(fSettingsFinder.XWayValveExists(aXWayValveDeviceName));
end;

class procedure TSystemLiquidRouteFinder.CheckRecursionsForXWayValve(const aXWayValveName: string;
    const aPositions: TArray<TXWayValvePos>);
var
    x: integer;
begin
    for x := 0 to high(aPositions) do
    begin
        if (aPositions[x].DeviceName = aXWayValveName) then
            Exception.CreateFmt('Device %s has recursive input port structure!', [aXWayValveName]);
        // Es ist kein Device
    end;
end;

class function TSystemLiquidRouteFinder.DecodeSystemLiquidIdent(aIdent: string): integer;
var
    xIndex, xErr: integer;
begin
    result := -1;
    if (Pos(STR_SYSTEMLIQUID_PREFIX, aIdent) <> 1) then
        EXIT;
    Val(Copy(aIdent, 4, 2), xIndex, xErr);
    result := xIndex - 1;
end;

class function TSystemLiquidRouteFinder.EncodeSystemLiquidIdent(aIndex: integer): string;
begin
    result := Format('%s%.2d', [STR_SYSTEMLIQUID_PREFIX, (aIndex + 1)]);
end;


end.
