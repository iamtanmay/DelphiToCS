{ --------------------------------------------------------------------------------------------------
  Copyright © 2002 Zinsser Analytic GmbH - All rights reserved.
  Author       : Wolfgang Lyncke (wl)
  Description  : TDevice is an abstract base class for all devices.
  Device classes should encapsulate 'Drivers' like RoboticInterface, SerCom, ...
  The device class structure should become more important in forthcoming versions!
  --------------------------------------------------------------------------------------------------
  Revision History:
  date     op  method                       track-no improvement/change
  -------- --  ---------------------------  -------- -----------------------------------------------
  09.11.07 pk                               TN3864   Various Changes
  07.02.08 wl  Reset                        TN4009   entfernt
  26.05.08 wl  AreaName                         TN4119   removed
  11.06.08 wl  Acquire                          TN4143   result = true
  26.08.08 wl  Independent                  TN4164   neue Property
  14.04.09 pk  StandBy                      TN4524   New
  04.11.09 pk                               TN4843   Replace TObjectList, TStringList, TStrings with Generics Lists and TStringArray
  27.03.13 wl                               TN6045   uses geändert
  08.11.13 wl                               TN6298   SIXWAYWALVE_1 entfernt
  -------------------------------------------------------------------------------------------------- }

unit Device;


interface


uses
    GeneralTypes,
    SysUtils,
    Classes,
    Module,
    ModuleTypeInfo,
    CommonTypes,
    Driver,
    ModuleSettings,
    DeviceSettingList,
    IntfDevice;

type
    TDeviceTypeInfo = class(TModuleTypeInfo)
    end;

    TDevice = class(TModule, IDevice)
    private const
        INT_DEFAULT_RESOURCE_LOCK_COUNT = 1;
    private
        fIndependent: boolean;
    protected
        function GetPortAdrBit: string; virtual;
        function GetProtocolName: string; virtual;
        function GetIndependent: boolean;
        procedure SetIndependent(const Value: boolean);
    public
        constructor Create(const aName: string); override;

        procedure WriteLogText(aLevel, aNumber: integer); virtual;
        function GetText_FirstLog(aLevel, aNumber: integer): string; virtual;

        // resource methods
        function Acquire(aTimeout: cardinal): boolean;
        procedure Release();

        function GetDependentDeviceNames(): TStringArray;

        // Actions!
        procedure Init(aInitID: TDevInitID); virtual;
        procedure StandBy(const aStandBy: boolean); virtual;

        // properties
        property PortAdrBit: string read GetPortAdrBit;
        property ProtocolName: string read GetProtocolName;
        property Independent: boolean read GetIndependent write SetIndependent;
    end;


implementation


uses
    Graphics,
    Generics.Collections;

{ TDevice }

constructor TDevice.Create(const aName: string);
begin
    inherited Create(aName);
end;

function TDevice.GetPortAdrBit: string;
begin
    result := '';
end;

procedure TDevice.WriteLogText(aLevel, aNumber: integer);
begin
    // gmLogText(GetText_FirstLog(aLevel, aNumber), nil);
end;

function TDevice.GetText_FirstLog(aLevel, aNumber: integer): string;
var
    x: integer;
begin
    result := '';

    if (aLevel = 0) then
    begin
        result := 'Devices: ';
        exit;
    end;

    for x := 1 to aLevel do
        result := result + '--';

    { TODO -oPK -cDEVICES : GetTypeNameFromType }
    // result := Format('%s Device %d >%s<: Type %s, %s',
    // [result, aNumber+1, FName, GetTypeNameFromType(FDType), GetPortAdrBit]);
end;

function TDevice.GetProtocolName: string;
begin
    result := '';
end;

procedure TDevice.Init(aInitID: TDevInitID);
begin
    // do nothing
end;

function TDevice.Acquire(aTimeout: cardinal): boolean;
begin
    result := true;
end;

procedure TDevice.Release();
begin
    // fResource.Release();
end;

function TDevice.GetDependentDeviceNames(): TStringArray;
var
    x: integer;
    xList: TList<string>;
begin
    xList := TList<string>.Create();
    try
        for x := 0 to fSettings.Count - 1 do
        begin

            if (fSettings[x] is TMSDevice) and (fSettings[x].Value <> '') then
            begin
                xList.Add(fSettings[x].Value);
            end;
        end;
        result := xList.ToArray();
    finally
        xList.Free;
    end;

end;

function TDevice.GetIndependent: boolean;
begin
    result := fIndependent;
end;

procedure TDevice.SetIndependent(const Value: boolean);
begin
    fIndependent := Value;
end;

procedure TDevice.StandBy(const aStandBy: boolean);
begin

end;


end.
