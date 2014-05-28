{ -----------------------------------------------------------------------------------------------------------
  Copyright © 2012 Zinsser Analytic GmbH - All rights reserved.
  Author       : Wolfgang Lyncke (wl)
  Description  :
  -----------------------------------------------------------------------------------------------------------
  Revision History:
  date     op  method                              track-no improvement/change
  -------- --  ----------------------------------  -------- -------------------------------------------------
  27.02.13 wl                                      TN6066   Initial Revision
  ----------------------------------------------------------------------------------------------------------- }

unit DevicesConflictManager;


interface


uses
    Generics.Collections,
    IntfArmDevice,
    XTrackConflictManager;

type
    TDevicesConflictManager = class
    strict private
        fXTrackConflictManagers: TObjectList<TXTrackConflictManager>;
        class var uInstance: TDevicesConflictManager ;
        procedure AddArm(aArmDevice: IArmDevice);
        function FindOrAddConflictManager(aTrackID: integer): TXTrackConflictManager;
        procedure InitConflictUnits(aArmDevices: TArray<IArmDevice>);
        procedure ClearMotorUsageUsedByIDAll();
    public
        constructor Create(aArmDevices: TArray<IArmDevice>);
        destructor Destroy(); override;
        procedure UnRegisterUseXMotorAll();
        procedure SetXConflictResolveMode(aMode: TXConflictResolveMode);
        class procedure CreateInstance(); static;
        class procedure DestroyInstance(); static;
        class property Instance: TDevicesConflictManager read uInstance;
    end;


implementation


uses
    SysUtils,  DeviceManager,
    ObjModul,
    IntfMotorDevice;

{ TDevicesConflictManager }

constructor TDevicesConflictManager.Create(aArmDevices: TArray<IArmDevice>);
begin
    inherited Create;

    fXTrackConflictManagers := TObjectList<TXTrackConflictManager>.Create(true);
    InitConflictUnits(aArmDevices);
end;

class procedure TDevicesConflictManager.CreateInstance;
begin
    uInstance := TDevicesConflictManager.Create(gDeviceManager.FindModules<IArmDevice>);
end;

destructor TDevicesConflictManager.Destroy;
begin
    FreeAndNil(fXTrackConflictManagers);

    inherited;
end;

class procedure TDevicesConflictManager.DestroyInstance;
begin
   FreeAndNil(uInstance);
end;

procedure TDevicesConflictManager.InitConflictUnits(aArmDevices: TArray<IArmDevice>);
var
    x: integer;
begin
    // aus den Arm-Devices die Conflict-Units erzeugen
    for x := 0 to high(aArmDevices) do
        AddArm(aArmDevices[x]);

    // ConflictUnits initialisieren
    for x := 0 to fXTrackConflictManagers.Count - 1 do
        fXTrackConflictManagers[x].InitConflictUnits();
end;

procedure TDevicesConflictManager.AddArm(aArmDevice: IArmDevice);
var
    xXMotor: IXMotorDevice;
    xConflictManager: TXTrackConflictManager;
begin
    // No X-Motor: No Conflict management
    if not TModuleFinder.GetXMotor(aArmDevice, xXMotor) then
        EXIT;

    // No X-Motor-Driver: No Conflict management
    if not Assigned(xXMotor.Driver) then
        EXIT;

    // Track-ID < 0: No Conflict management
    if (xXMotor.TrackID < 0) then
        EXIT;

    // ConflictManager finden
    xConflictManager := FindOrAddConflictManager(xXMotor.TrackID);
    xConflictManager.AddArm(aArmDevice);
end;

function TDevicesConflictManager.FindOrAddConflictManager(aTrackID: integer): TXTrackConflictManager;
var
    x: integer;
    xNewConflictManager: TXTrackConflictManager;
begin
    // gibt es schon einen ConflictManager mit dieser Track-ID?
    for x := 0 to fXTrackConflictManagers.Count - 1 do
    begin
        if (fXTrackConflictManagers[x].TrackID = aTrackID) then
        begin
            EXIT(fXTrackConflictManagers[x]);
        end;
    end;

    xNewConflictManager := TXTrackConflictManager.Create(aTrackID);
    fXTrackConflictManagers.Add(xNewConflictManager);
    EXIT(xNewConflictManager);
end;

procedure TDevicesConflictManager.SetXConflictResolveMode(aMode: TXConflictResolveMode);
var
    x: integer;
begin
    for x := 0 to fXTrackConflictManagers.Count - 1 do
        fXTrackConflictManagers[x].SetXConflictResolveMode(aMode);

    ClearMotorUsageUsedByIDAll();
end;

procedure TDevicesConflictManager.ClearMotorUsageUsedByIDAll();
var
    x: integer;
begin
    for x := 0 to fXTrackConflictManagers.Count - 1 do
        fXTrackConflictManagers[x].ClearMotorUsageUsedByIDAll();
end;

procedure TDevicesConflictManager.UnRegisterUseXMotorAll;
var
    x: integer;
begin
    for x := 0 to fXTrackConflictManagers.Count - 1 do
        fXTrackConflictManagers[x].UnRegisterUseXMotorAll;
end;


end.
