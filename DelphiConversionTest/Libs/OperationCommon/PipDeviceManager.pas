{ --------------------------------------------------------------------------------------------------
  Copyright © 2004 Zinsser Analytic GmbH - All rights reserved.
  Author       : Wolfgang Lyncke (wl)
  Description  : This objects is the link between the tip configuration (loaded with the Layout) and
  the arm device configuration (permanent). The used arm and the used tips are set
  here.
  --------------------------------------------------------------------------------------------------
  Revision History:
  date     op  method                       track-no improvement/change
  -------- --  ---------------------------  -------- -----------------------------------------------
  19.03.04 wl                               TN1788   initial version
  19.03.04 wl  GetArmIndex                  TN1788   ermittelt ArmIndex und ArmTipIndex aus einer allgemeinen Tip-Nummer
  05.04.04 wl  Create,CreateSimple          TN1788   "Simple" für Editor, wo die Arm-Devices nicht initialisiert sind
  05.04.04 wl                               TN1788   Unterscheidung von TotalTipIndex (alle Arme) und ArmTipIndex
  05.04.04 wl  GetTotalTipsFromArmTips      TN1788   Umrechnung von ArmTipIndex zu TotalTipIndex
  05.04.04 wl  GetArm_ByTip                 TN1788   man erhält den richtigen Arm aus dem "TotalTipIndex
  05.04.04 wl  FindArmToUse                 TN1788   ruft für jeden Arm SetUseTips auf
  05.04.04 wl  GetAllTypeNames              TN1788   Kapselung für gTipTypes
  20.04.04 wl  ClearTipStatus_AllArms       TN1788   entspricht ehm. gmClearTipStatus
  20.04.04 wl  SetTipPanel                  TN1788   übergibt TArmPanel an Arm-Device
  28.04.04 wl                               TN1788   benutzt TCustomArmDevice
  04.05.04 wl  GetTubeHandlingArm           TN1788   Rückgabewert: GipperArm
  08.06.04 wl  TArmList                     TN1963   Beschränkung auf 2 Arme aufgehoben (Lissy & CRS möglich)
  08.06.04 wl  AddArm                       TN1963   Arme werden einzeln hinzugefügt
  09.06.04 wl  GetTubeHandlingArm           TN1963   Bugfix
  10.08.04 wl  TArmList.GetItem             TN2091   Methode gibt bei falschen Index nil zurück
  13.10.04 wl  TipNameIsRedi                TN2151   neu: für erweitertes LiqPar-Fenster
  13.01.05 wl  GetLiquid/PowderTipTypeNames TN2246.4 neu: für neues LiqPar-Fenster (ZADesign)
  03.06.05 wl  FirstPipArmTipCount          TN2436   Ersatz für Sam.nTips (nur Arm 1)
  03.06.05 wl  TotalPipArmTipCount          TN2436   Ersatz für Sam.nTips (Arm 1 & 2)
  15.06.05 pk  GetArmBySpecificArmIndex     TN2464   New : Get pip arm or grip arm by local arm index
  15.06.05 pk  GetUseTips_Total             TN2464   returns a global tipmap of all currently selected tips on all arms
  21.06.05 pk  fUsedArmIndex                TN2464.3 Removed : Catastrophical when two arms used simultaneously
  21.06.05 pk  fArmPanel                    TN2464.3 New
  21.06.05 pk  GetArmTipsFromTotoalTips     TN2464.3 Now works for all arms
  21.06.05 pk  SetUseTips                   TN2464.3 Also sets fArmPanel properties
  24.06.05 pk  InitTipType                  TN2475   New : some code from ObjArm and some code from arm.InitTipType - passes TotalTipIndex to arm
  17.11.05 wl  GetGripperArm                TN2771   berücksichtigt, dass es mehrere Gripper geben kann
  30.11.05 wl                               TN2818   RemRediTip entfernt
  05.01.06 pk                               TN2877   find arm functions --> ObjModulA
  25.01.06 pk  GetFirstPipArmTipCount       TN2900   do not raise assertion if no piparms are found
  23.05.06 pk  GetTotalTipFromArmTips       TN3112   New: change local tip map to a total tip map
  23.09.06 wl                               TN3326   komplett von gTipTypes auf TTipTypeDataAdaptor umgestellt
  25.09.06 wl  TipTypeExists                TN3326   Funktion korrigiert
  27.02.07 pk  GetHasPipDeviceWithMultiPumpsTN3525   New:  to figure out if second channel exists
  08.03.07 wl                               TN3620   uses geändert
  13.03.07 pk  GetArmTipsFromTotalTips      TN3633   New
  20.06.08 pk                               TN4139   fArmPanel removed for now
  03.07.08 wl                                TN4157
  11.07.08 wl  SetOnShowUseTipsEvent         TN4164   verknüpft die Events mit Tipset, Tipset selbst ist hier nicht bekannt
  17.09.08 wl  InitTipType                   TN4224   entfernt
  16.01.09 wl                                TN4362   an Änderungen in TQueryDataAdaptor angepasst
  28.08.09 pk  TipNameIsRedi                 TN4753   code moved to TipTypeDataAdaptor
  28.08.09 pk  GetAllTypeNames               TN4753   removed
  04.11.09 pk                                TN4843   Replace TObjectList, TStringList, TStrings with Generics Lists and TStringArray
  27.03.13 wl                                TN6045   uses Generics.Collections
  08.11.13 wl                                TN6298   überflüssiges entfernt
  -------------------------------------------------------------------------------------------------- }

unit PipDeviceManager;


interface


uses
    Generics.Collections,
    GeneralTypes,
    CommonTypes,
    AppTypes,
    TipSystem,
    IntfPipDevice;

type
    TPipDeviceManager = class
    private
        fPipDeviceList: TList<IPipDevice>;
        function GetPipDevice(aIndex: Integer): IPipDevice;
        function GetPipDeviceCount: integer;
        function GetTotalTipCount: integer;
    public
        constructor Create;
        destructor Destroy; override;

        procedure AddArm(aArm: IPipDevice);
        function GetTipNames(aTipTypes: TBasicTipTypes): TStringArray;
        function GetLiquidTipTypeNames(): TStringArray;
        function GetPowderTipTypeNames(): TStringArray;
        function GetRediDeviceList(): TStringArray;
        function GetTipTypeFromName(aTypeName: string): TBasicTipType;
        function TipNameIsRedi(aTypeName: string): boolean;
        function GetPipDeviceNames(): TStringArray;
        function FindPipDeviceIndex(aPipDevice: IPipDevice): integer;
        function FindPipDevice_ByName(aPipDeviceName: string): IPipDevice;
        procedure SetOnShowUseTipsEvent(const aShowUseTipsEvent: TShowUseTipsEvent);

        property this[index: Integer]: IPipDevice read GetPipDevice; default;
        property Count: integer read GetPipDeviceCount;
        property TotalTipCount: integer read GetTotalTipCount;
    end;

var
    gPipDeviceManager: TPipDeviceManager = nil;


implementation


uses
    SysUtils,
    Math,
    TipMapUtils,
    SamGlobe,
    TipTypeDataAdaptor;

{ TPipDeviceManager }

constructor TPipDeviceManager.Create;
begin
    inherited Create;

    fPipDeviceList := TList<IPipDevice>.Create;
end;

destructor TPipDeviceManager.Destroy;
begin
    fPipDeviceList.Free;

    inherited;
end;

procedure TPipDeviceManager.AddArm(aArm: IPipDevice);
begin
    if not Assigned(aArm) then
        exit;
    fPipDeviceList.Add(aArm);
end;

function TPipDeviceManager.GetPipDevice(aIndex: Integer): IPipDevice;
begin
    result := nil;
    if (aIndex >= 0) and (aIndex < fPipDeviceList.Count) then
        result := fPipDeviceList[aIndex];
end;

function TPipDeviceManager.FindPipDeviceIndex(aPipDevice: IPipDevice): integer;
var
    x: integer;
begin
    result := -1;
    for x := 0 to fPipDeviceList.Count - 1 do
    begin
        if (fPipDeviceList[x] <> aPipDevice) then
        begin
            result := x;
            EXIT;
        end;
    end;
end;

function TPipDeviceManager.GetTotalTipCount: integer;
var
    x: integer;
begin
    result := 0;
    for x := 0 to fPipDeviceList.Count - 1 do
    begin
        result := result + fPipDeviceList[x].TipCount;
    end;
end;

function TPipDeviceManager.GetTipNames(aTipTypes: TBasicTipTypes): TStringArray;
var
    x: integer;
    xList: TList<string>;
begin
    xList := TList<string>.Create();
    try
        for x := 0 to fPipDeviceList.Count - 1 do
            xList.AddRange(fPipDeviceList[x].GetTipNames(aTipTypes));

        result := xList.ToArray;
    finally
        FreeAndNil(xList);
    end;
end;

function TPipDeviceManager.GetPipDeviceCount: integer;
begin
    result := fPipDeviceList.Count;
end;

function TPipDeviceManager.GetTipTypeFromName(aTypeName: string): TBasicTipType;
begin
    result := TTipTypeDataAdaptor.GetTipTypeFromName(aTypeName);
end;

function TPipDeviceManager.TipNameIsRedi(aTypeName: string): boolean;
begin
    result := TTipTypeDataAdaptor.TipNameIsRedi(aTypeName);
end;

function TPipDeviceManager.GetLiquidTipTypeNames(): TStringArray;
begin
    result := TTipTypeDataAdaptor.InstGetLiquidTipTypeNames();
end;

function TPipDeviceManager.GetPowderTipTypeNames(): TStringArray;
begin
    result := TTipTypeDataAdaptor.InstGetPowderTipTypeNames();
end;

function TPipDeviceManager.GetRediDeviceList(): TStringArray;
var
    xDataAdaptor: TTipTypeDataAdaptor;
begin
    xDataAdaptor := TTipTypeDataAdaptor.Create();
    try
        result := xDataAdaptor.GetRediDeviceList();
    finally
        xDataAdaptor.Free;
    end;
end;

function TPipDeviceManager.GetPipDeviceNames(): TStringArray;
var
    x: integer;
begin
    SetLength(result, fPipDeviceList.Count);
    for x := 0 to fPipDeviceList.Count - 1 do
    begin
        result[x] := fPipDeviceList[x].Name;
    end;
end;

function TPipDeviceManager.FindPipDevice_ByName(aPipDeviceName: string): IPipDevice;
var
    x: integer;
begin
    result := nil;
    for x := 0 to fPipDeviceList.Count - 1 do
    begin
        if (fPipDeviceList[x].Name = aPipDeviceName) then
        begin
            result := fPipDeviceList[x];
            EXIT;
        end;
    end;
end;

procedure TPipDeviceManager.SetOnShowUseTipsEvent(const aShowUseTipsEvent: TShowUseTipsEvent);
var
    x: integer;
begin
    // Event setzen
    for x := 0 to fPipDeviceList.Count - 1 do
    begin
        fPipDeviceList[x].OnShowUseTips := aShowUseTipsEvent;
    end;
end;


end.
