{ --------------------------------------------------------------------------------------------------
  Copyright © 2007 Zinsser Analytic GmbH - All rights reserved.
  Author       : Wolfgang Lyncke (wl)
  Description  : Inherits instances for DataAdaptorCommon library
  --------------------------------------------------------------------------------------------------
  Revision History:
  date     op  method                       track-no improvement/change
  -------- --  ---------------------------  -------- -----------------------------------------------
  AppInstanceAppDataAddaptor:
  31.08.07 wl                               TN3811.4 initial version
  03.09.07 wl  Create                       TN3811.4  neuer Parameter Purpose
  12.09.07 wl  CheckDataForUpdate           TN3811.4  neu dabei: LiqH-, Method-, RunDataAdaptor
  09.11.07 pk  CheckDataForUpdate           TN3921    UpdateManager removed
  27.11.07 wl  SystemDataPassword           TN3908    neue Property
  22.01.08 wl                               TN3972    uses DatabaseProvider
  14.04.08 wl  Purpose                      TN4060    from AppInstanceLogging
  11.06.08 wl  CheckDataForUpdate           TN4143    result = false
  24.06.08 wl                               TN4143    uses geändert
  25.09.08 pk  Create                       TN3960    Patch/UnpatchBDE called explicitly here
  13.07.09 pk  DefineRunAlias               TN4585.4
  07.08.09 wl                               TN4702   Strings werden jetzt direkt geladen
  21.09.09 pk  Destroy                      TN4753   call TAppInstanceLogging.DestroyInstance instead of TAppInstanceLogging.Instance.Free
  04.11.09 pk                               TN4843 Replace TObjectList, TStringList, TStrings with Generics Lists and TStringArray
  16.12.09 pk                               TN4933   TDatabaseProvider functions no longer class functions
  20.05.10 wl ConnectionType                TN5116   neue Property
  17.06.10 pk                               TN5152.1 Create DataProvider TypeDictionaries
  18.06.10 pk                               TN5152.1 PatchBDE and UnPatchBDE moved to BDEDatabaseProviderManager
  AppInstanceAppData:
  21.07.10 pk                               TN5203   Unit renamed.
  04.08.10 pk                               TN5218   Changes for Plugin database packages
  19.10.10 pk                               TN5305   changes needed for CoreClient/Server
  09.09.11 wl                               TN5672   Auskommentiertes entfernt
  27.01.13 wl  GetDataPath                  TN6069   von AppInstanceIniAdaptor hierher
  -------------------------------------------------------------------------------------------------- }

unit AppInstanceAppData;


interface


uses
    CommonTypes;

type
    TAppInstanceAppData = class
    private
        fPurpose: TAppPurpose;
        fConnectionType: TMachineConnectionType;

        class var uInstDataAdaptorCommon: TAppInstanceAppData; // Single Instance
        class function GetInstance(): TAppInstanceAppData; static;
        constructor Create(const aPurpose: TAppPurpose; const aConnectionType: TMachineConnectionType);
    public
        destructor Destroy(); override;
        class function CreateInstance(const aPurpose: TAppPurpose;
            const aConnectionType: TMachineConnectionType): TAppInstanceAppData;
        class procedure DestroyInstance();

        class function GetDataPath(): string;

        property Purpose: TAppPurpose read fPurpose;
        property ConnectionType: TMachineConnectionType read fConnectionType;
        class property Instance: TAppInstanceAppData read GetInstance;
    end;


implementation


uses
    SysUtils,
    AppInstanceLogging,
    FileUtilities;

{ TAppInstanceAppData }

class function TAppInstanceAppData.CreateInstance(const aPurpose: TAppPurpose;
    const aConnectionType: TMachineConnectionType): TAppInstanceAppData;
begin
    // create instance if instance does not exist
    if not Assigned(uInstDataAdaptorCommon) then
        uInstDataAdaptorCommon := TAppInstanceAppData.Create(aPurpose, aConnectionType);

    // return instance
    result := uInstDataAdaptorCommon;
end;

class function TAppInstanceAppData.GetDataPath: string;
const
    cAppDataPath = 'DATA\';
begin
    EXIT(TFileUtilities.GetApplicationDataPath() + cAppDataPath);
end;

class function TAppInstanceAppData.GetInstance: TAppInstanceAppData;
begin
    result := uInstDataAdaptorCommon;
end;

constructor TAppInstanceAppData.Create(const aPurpose: TAppPurpose;
    const aConnectionType: TMachineConnectionType);
begin
    inherited Create();

    fPurpose := aPurpose;
    fConnectionType := aConnectionType;

    // gLogManager muss so früh wie möglich existieren
    TAppInstanceLogging.CreateInstance(aPurpose <> appSimple);
end;

destructor TAppInstanceAppData.Destroy;
begin
    TAppInstanceLogging.DestroyInstance();

    inherited;
end;

class procedure TAppInstanceAppData.DestroyInstance;
begin
    FreeAndNil(uInstDataAdaptorCommon);
end;


end.
