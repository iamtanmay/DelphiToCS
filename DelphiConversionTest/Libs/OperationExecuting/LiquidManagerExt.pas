{ --------------------------------------------------------------------------------------------------
  Copyright © 2009 Zinsser Analytic GmbH - All rights reserved.
  Author       : Wolfgang Lyncke (wl), Payman Kamali (pk)
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
  28.08.09 pk                               TN4753   code from TSystemLiquidManager
  08.09.09 pk                               TN4753   uses ErrorMessage replaced by ErrorInfo
  12.09.09 wl                               TN4740   TipFloat-/TPosMM-/TipInteger-/MPOSArray durch TDoubleArray/TIntArray ersetzt
  09.06.10 pk                               TN5116   new TRunSystemLiquidRouteFinder
  18.06.12 wl  Create                       TN5899   TLiquids-Instanz ist kein Parameter mehr
  11.02.13 ts  IsLiquidReachableFromTips    TN6082   Beachtet jetzt alle Pumpen an Tip
  24.04.13 wl  GetLiquidFromIdent           TN6137   von Liquids hierher
  18.09.13 wl  PipDeviceGetLiquidIndices    TN6252.3 neu für Flush-Dialog
  25.09.13 wl  ChangeLiquid                 TN6258   Result (Tipmap): Alle Tips, bei denen sich die Systemflüssigkeit geändert hat
  25.09.13 wl  ChangeLiquid                 TN6255   Fehlermeldung nur, wenn System liquid mit keiner Pumpe erreichbar ist (auch wenn nur mit Channel 2)
  25.09.13 wl  ChangeLiquid                 TN6259   Verbessertes Logging
  27.09.13 wl  PipDeviceGetLiquidIndices    TN6258.1 Bugfix
  08.11.13 wl                               TN6298   div. Korrekturen, damit der Default-SystemInputPort wieder funktioniert
  -------------------------------------------------------------------------------------------------- }

unit LiquidManagerExt;


interface


uses
    Generics.Collections,
    AppTypes,
    IntfPipDevice,
    IntfPipPumpDevice,
    IntfXWayValveDevice,
    Liquids,
    LiquidManager,
    TipSystem;

type
    TRunSystemLiquidSettingsFinder = class(TSystemLiquidSettingsFinder)
    public
        function GetXWayValveInputPortNames(const aXWayValveDeviceName: string): TArray<string>; override;
        function XWayValveExists(const aXWayValveDeviceName: string): boolean; override;
        function GetPipPumpDeviceName(const aPipDeviceName: string; const aTipIndex: integer)
            : string; override;
        function PipDeviceTipCount(const aPipDeviceName: string): integer; override;
        function PipPumpExists(const aPipPumpDeviceName: string): boolean; override;
    end;

    TSystemLiquidManagerExt = class(TSystemLiquidManager)
    private
        fRouteFinder: TSystemLiquidRouteFinder;
        procedure ChangeSysLiquid(const aInputPortName: string; aSysLiqIndex: integer);
        function FindValveDevice(const aName: string): IXWayValveDevice;
        function GetLiquidFromIdent(aIdent: string): TSystemLiquid;
        function GetCurrentSystemLiquidsText(aPipDevice: IPipDevice): string;
        function GetCurrentTipSystemLiquidsText(aTipSystem: TTipSystem): string;
        procedure PumpGetReachableLiquidIdents(const aPipPumpDeviceName: string;
            aLiquidIdents: TList<string>);
        function PumpGetInputPortName(aPipPump: IPipPumpDevice): string;
    public
        //
        constructor Create(const aSettingsFinder: TSystemLiquidSettingsFinder);
        destructor Destroy(); override;

        function FindCurrentLiquid(aInputPortName: string): TSystemLiquid; override;
        function PumpGetCurrentSysIndex(aPipPump: IPipPumpDevice): integer; override;
        function PumpGetCurrentSysName(aPipPump: IPipPumpDevice): string; override;
        function PumpGetCurrentSysLiquid(aPipPump: IPipPumpDevice): TSystemLiquid; override;
        function ChangeLiquid(aPipDevice: IPipDevice; aTips: TIPMAP; aSysLiqIndex: integer): TIPMAP; override;
        function IsLiquidReachableFromTips(aPipDevice: IPipDevice; aTips: TIPMAP; aSysLiqIndex: integer;
            out oErrorText: string): boolean; override;
        function PipDeviceGetLiquidIndices(aPipDevice: IPipDevice; aTips: TIPMAP;
            aPumpIndex1, aPumpIndex2: integer): TArray<integer>; override;
    end;


implementation


uses
    SysUtils,
    TipMapUtils,
    Device,
    ArrayUtils,
    ErrorInfo,
    ErrorManager,
    ErrorMessageFactory,
    LogManager,
    GeneralTypes,
    DeviceManager,
    PipDeviceManager;

{ TSystemLiquidManagerExt }

constructor TSystemLiquidManagerExt.Create(const aSettingsFinder: TSystemLiquidSettingsFinder);
begin
    inherited Create;
    fRouteFinder := TSystemLiquidRouteFinder.Create(aSettingsFinder);
end;

destructor TSystemLiquidManagerExt.Destroy;
begin
    FreeAndNil(fRouteFinder);
    inherited;
end;

function TSystemLiquidManagerExt.GetLiquidFromIdent(aIdent: string): TSystemLiquid;
var
    xIndex: integer;
begin
    xIndex := TSystemLiquidRouteFinder.DecodeSystemLiquidIdent(aIdent);

    if (xIndex = -1) then
    begin
        result := nil;
        EXIT;
    end;

    result := TLiquids.Instance.SystemLiquids[xIndex];
end;

function TSystemLiquidManagerExt.PumpGetInputPortName(aPipPump: IPipPumpDevice): string;
const
    STR_NAME_SIXWAYVALVE_1 = 'SixWayValve1';
begin
    result := aPipPump.InputPortName;
    if (result <> '') then
        EXIT;

    // Aus Kompatibilitätsgründen: Wenn ein 6-Wege-Ventil mit diesem Namen existiert, braucht es nicht als
    // Input Port mit angegeben werden
    if fRouteFinder.XWayValveExists(STR_NAME_SIXWAYVALVE_1) then
        EXIT(STR_NAME_SIXWAYVALVE_1);

    // Normalfall, wenn nichts angegeben wurde: Verwende System Liquid 1
    result := fRouteFinder.EncodeSystemLiquidIdent(0);
end;

procedure TSystemLiquidManagerExt.PumpGetReachableLiquidIdents(const aPipPumpDeviceName: string;
    aLiquidIdents: TList<string>);
// For Pump aPipPump get the idents of all of the sysliquids which can be reached (directly or via one or more sixwayvalves)
// by this pump
var
    xPipPump: IPipPumpDevice;
    xInputPortName: string;
begin
    if not gDeviceManager.FindModule(true, aPipPumpDeviceName, IPipPumpDevice, xPipPump) then
        EXIT;

    xInputPortName := PumpGetInputPortName(xPipPump);

    // portname is either the name of a sixwayvalve devie OR a LiquidIdent Name
    if fRouteFinder.XWayValveExists(xInputPortName) then
        fRouteFinder.GetReachableLiquidIdents(xInputPortName, aLiquidIdents)
    else
        aLiquidIdents.Add(xInputPortName);
end;

function TSystemLiquidManagerExt.FindValveDevice(const aName: string): IXWayValveDevice;
begin
    gDeviceManager.FindModule(false, aName, IXWayValveDevice, result);
end;

function TSystemLiquidManagerExt.FindCurrentLiquid(aInputPortName: string): TSystemLiquid;
var
    x: integer;
    xDevice: IXWayValveDevice;
    xDevNames: TList<string>;
begin
    xDevNames := TList<string>.Create;
    try
        result := GetLiquidFromIdent(aInputPortName);
        while not Assigned(result) do
        begin
            // bei rekursiver Struktur aussteigen (sobald ein Device zum zweiten Mal auftaucht)
            for x := 0 to xDevNames.Count - 1 do
            begin
                if (xDevNames[x] = aInputPortName) then
                    EXIT;
            end;

            xDevNames.Add(aInputPortName);

            // LiquidIdent kann der Name eines Device sein: suche danach!
            xDevice := self.FindValveDevice(aInputPortName);
            if not Assigned(xDevice) then
                BREAK;

            aInputPortName := xDevice.GetCurrentInputPortName();
            result := GetLiquidFromIdent(aInputPortName);
        end;
    finally
        FreeAndNil(xDevNames);
    end;
end;

function TSystemLiquidManagerExt.PumpGetCurrentSysLiquid(aPipPump: IPipPumpDevice): TSystemLiquid;
begin
    result := FindCurrentLiquid(PumpGetInputPortName(aPipPump));
end;

function TSystemLiquidManagerExt.PumpGetCurrentSysIndex(aPipPump: IPipPumpDevice): integer;
var
    xLiquid: TSystemLiquid;
begin
    result := -1;
    xLiquid := PumpGetCurrentSysLiquid(aPipPump);
    if not Assigned(xLiquid) then
        EXIT;
    result := xLiquid.DilIndex;
end;

function TSystemLiquidManagerExt.PumpGetCurrentSysName(aPipPump: IPipPumpDevice): string;
var
    xLiquid: TSystemLiquid;
begin
    result := '';
    xLiquid := PumpGetCurrentSysLiquid(aPipPump);
    if not Assigned(xLiquid) then
        EXIT;
    result := xLiquid.Diluent;
end;

procedure TSystemLiquidManagerExt.ChangeSysLiquid(const aInputPortName: string; aSysLiqIndex: integer);
var
    xValvePosArr: TArray<TXWayValvePos>;
    x: integer;
    xDevice: IXWayValveDevice;
begin
    xValvePosArr := fRouteFinder.GetXWayValvePositions(aInputPortName, aSysLiqIndex);

    // Alle Ventile auf die angegebene Position stellen
    for x := 0 to high(xValvePosArr) do
    begin
        xDevice := self.FindValveDevice(xValvePosArr[x].DeviceName);
        Assert(Assigned(xDevice), 'Device ' + xValvePosArr[x].DeviceName + ' does not exist!');
        xDevice.TurnValve(xValvePosArr[x].Position);
    end;
end;

function TSystemLiquidManagerExt.IsLiquidReachableFromTips(aPipDevice: IPipDevice; aTips: TIPMAP;
    aSysLiqIndex: integer; out oErrorText: string): boolean;
// returns true if each tip in aTips can reach the liquid: aSysLiqIndex
// otherwise setglobalerr and return false
var
    x, i: integer;
    xList: TList<string>;
    xLiquid: TSystemLiquid;
    xLiquidIdentName: string;
begin
    oErrorText := '';
    result := true;
    xLiquid := TLiquids.Instance.SystemLiquids[aSysLiqIndex];
    xLiquidIdentName := TSystemLiquidRouteFinder.EncodeSystemLiquidIdent(aSysLiqIndex);

    xList := TList<string>.Create();
    try
        for x := 0 to aPipDevice.TipCount - 1 do
        begin
            if not TTipMapUtils.TipSelected(aTips, x) then
                CONTINUE;

            // wenn keine PipPump existiert, wird auch nicht getestet
            if not Assigned(aPipDevice.Tips[x].GetPipPump(0)) then
                CONTINUE;

            xList.Clear;

            // hier wird für alle Pumpen eines Tips zusammen getestet:
            for i := 0 to aPipDevice.Tips[x].NumberOfPumps - 1 do
            begin
                self.PumpGetReachableLiquidIdents(aPipDevice.Tips[x].GetPipPump(i).Name, xList);
            end;

            // Einen Fehler gibt es nur, wenn es mit KEINER Pumpe erreichbar ist
            if xList.IndexOf(xLiquidIdentName) < 0 then
            begin
                result := false;
                oErrorText := Format('Device %s, Tip %d is NOT connected to system liquid %s',
                    [aPipDevice.Name, x + 1, xLiquid.Diluent]);
                EXIT;
            end;
        end;

    finally
        FreeAndNil(xList);
    end;
end;

function TSystemLiquidManagerExt.GetCurrentTipSystemLiquidsText(aTipSystem: TTipSystem): string;
var
    x: integer;
    xSystemLiqNames: TList<string>;
begin
    if (aTipSystem.NumberOfPumps = 0) then
        EXIT('');

    if (aTipSystem.NumberOfPumps = 1) then
        EXIT(PumpGetCurrentSysName(aTipSystem.GetPipPump(0)));

    xSystemLiqNames := TList<string>.Create;
    try
        for x := 0 to aTipSystem.NumberOfPumps - 1 do
        begin
            xSystemLiqNames.Add(PumpGetCurrentSysName(aTipSystem.GetPipPump(x)));
        end;

        EXIT(TArrayUtils.ArrayToBracketText(xSystemLiqNames.ToArray));
    finally
        FreeAndNil(xSystemLiqNames);
    end;
end;

function TSystemLiquidManagerExt.GetCurrentSystemLiquidsText(aPipDevice: IPipDevice): string;
var
    x: integer;
    xSystemLiqNames: TList<string>;
begin
    xSystemLiqNames := TList<string>.Create;
    try
        for x := 0 to aPipDevice.TipCount - 1 do
        begin
            xSystemLiqNames.Add(GetCurrentTipSystemLiquidsText(aPipDevice.Tips[x]));
        end;

        EXIT(TArrayUtils.ArrayToBracketText(xSystemLiqNames.ToArray));
    finally
        FreeAndNil(xSystemLiqNames);
    end;
end;

function TSystemLiquidManagerExt.ChangeLiquid(aPipDevice: IPipDevice; aTips: TIPMAP;
    aSysLiqIndex: integer): TIPMAP;
var
    x, xPIndex: integer;
    xPipPump: IPipPumpDevice;
    xSystemLiqIndexBeforeDict: TDictionary<IPipPumpDevice, integer>;
    xSystemLiqIndexAfter: integer;
    xLiquidReachable: boolean;
begin
    result := 0;

    if not Assigned(TLiquids.Instance.SystemLiquids[aSysLiqIndex]) then
    begin
        raise Exception.Create('System liquid ' + IntToStr(aSysLiqIndex + 1) + ' is undefined!');
    end;

    xSystemLiqIndexBeforeDict := TDictionary<IPipPumpDevice, integer>.Create;
    try
        // Die aktuellen Liquids der Pumpen speichern
        for x := 0 to aPipDevice.TipCount - 1 do
        begin
            for xPIndex := 0 to aPipDevice.Tips[x].NumberOfPumps - 1 do
            begin
                xPipPump := aPipDevice.Tips[x].GetPipPump(xPIndex);
                xSystemLiqIndexBeforeDict.Add(xPipPump, PumpGetCurrentSysIndex(xPipPump));
            end;
        end;

        for x := 0 to aPipDevice.TipCount - 1 do
        begin
            xLiquidReachable := false;

            // Sonderfall: Keine Pumpe: Nicht beschweren, wenn Liquid nicht erreicht werden kann
            if (aPipDevice.Tips[x].NumberOfPumps = 0) then
                xLiquidReachable := true;

            // Alle Pumpen durchgehen:
            for xPIndex := 0 to aPipDevice.Tips[x].NumberOfPumps - 1 do
            begin
                xPipPump := aPipDevice.Tips[x].GetPipPump(xPIndex);

                if TTipMapUtils.TipSelected(aTips, x) then
                begin
                    if (PumpGetCurrentSysIndex(xPipPump) <> aSysLiqIndex) then
                    begin
                        // Ventile/6-Wege-Ventil physikalisch ändern
                        ChangeSysLiquid(PumpGetInputPortName(xPipPump), aSysLiqIndex);
                    end;
                end;
                xSystemLiqIndexAfter := PumpGetCurrentSysIndex(xPipPump);

                // wenn das System liquid mit irgendeiner Pumpe erreichbar ist - OK, sonst gibt es eine Exception
                if (xSystemLiqIndexAfter = aSysLiqIndex) then
                    xLiquidReachable := true;

                // alle Tips, bei denen sich etwas geändert hat, werden zurückgegeben (zum anschließenden Spülen)
                if (xSystemLiqIndexBeforeDict[xPipPump] <> xSystemLiqIndexAfter) then
                    TTipmapUtils.SelectTip(result, x);
            end;

            if (TTipMapUtils.TipSelected(aTips, x) and (not xLiquidReachable)) then
                raise Exception.Create('System Liquid ' + TLiquids.Instance.SystemLiquids[aSysLiqIndex]
                    .Diluent + ' is not reachable with tip ' + IntToStr(x + 1));
        end;
    finally
        FreeAndNil(xSystemLiqIndexBeforeDict);
    end;

    gLogManager.Log('Current System Liquids ' + GetCurrentSystemLiquidsText(aPipDevice), (result > 0));
end;

function TSystemLiquidManagerExt.PipDeviceGetLiquidIndices(aPipDevice: IPipDevice; aTips: TIPMAP;
    aPumpIndex1, aPumpIndex2: integer): TArray<integer>;
var
    x, xPump: integer;
    xList: TList<string>;
    xSysLiqIndex: integer;
begin
    if not Assigned(aPipDevice) then
        EXIT(nil);

    xList := TList<string>.Create();
    try
        for x := 0 to aPipDevice.TipCount - 1 do
        begin
            if not TTipMapUtils.TipSelected(aTips, x) then
                CONTINUE;

            for xPump := 0 to aPipDevice.Tips[x].NumberOfPumps - 1 do
            begin
                if (xPump = aPumpIndex1) or (xPump = aPumpIndex2) then
                    self.PumpGetReachableLiquidIdents(aPipDevice.Tips[x].GetPipPump(xPump).name, xList);
            end;
        end;

        xList.Sort(TCustomStringComparer.Create);

        SetLength(result, xList.Count);
        for x := 0 to xList.Count - 1 do
        begin
            xSysLiqIndex := fRouteFinder.DecodeSystemLiquidIdent(xList[x]);
            if xSysLiqIndex < 0 then
                CONTINUE;
            result[x] := xSysLiqIndex;
        end;
    finally
        FreeAndNil(xList);
    end;
end;

{ TRunSystemLiquidSettingsFinder }

function TRunSystemLiquidSettingsFinder.GetPipPumpDeviceName(const aPipDeviceName: string;
    const aTipIndex: integer): string;
var
    xPipDevice: IPipDevice;
begin
    result := '';

    xPipDevice := gPipDeviceManager.FindPipDevice_ByName(aPipDeviceName);
    if not Assigned(xPipDevice) then
        EXIT;
    if not xPipDevice.TipCount <= aTipIndex then
        EXIT;

    result := xPipDevice.Tips[aTipIndex].GetPipPump(0).Name;
end;

function TRunSystemLiquidSettingsFinder.GetXWayValveInputPortNames(const aXWayValveDeviceName: string)
    : TArray<string>;
var
    xIntf: IXWayValveDevice;
    x: integer;
begin
    gDeviceManager.FindModule(true, aXWayValveDeviceName, IXWayValveDevice, xIntf);
    SetLength(result, xIntf.InputPortCount);
    for x := 0 to Length(result) - 1 do
        result[x] := xIntf.InputPortName[x];

end;

function TRunSystemLiquidSettingsFinder.XWayValveExists(const aXWayValveDeviceName: string): boolean;
var
    xIntf: IXWayValveDevice;
begin
    result := gDeviceManager.FindModule(false, aXWayValveDeviceName, IXWayValveDevice, xIntf);
end;

function TRunSystemLiquidSettingsFinder.PipDeviceTipCount(const aPipDeviceName: string): integer;
var
    xPipDevice: IPipDevice;
begin
    result := 0;

    xPipDevice := gPipDeviceManager.FindPipDevice_ByName(aPipDeviceName);
    if not Assigned(xPipDevice) then
        EXIT;
    result := xPipDevice.TipCount;
end;

function TRunSystemLiquidSettingsFinder.PipPumpExists(const aPipPumpDeviceName: string): boolean;
var
    xIntf: IPipPumpDevice;
begin
    result := gDeviceManager.FindModule(false, aPipPumpDeviceName, IPipPumpDevice, xIntf);
end;


end.
