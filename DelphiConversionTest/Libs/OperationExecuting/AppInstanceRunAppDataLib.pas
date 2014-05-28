{ -----------------------------------------------------------------------------------------------------------------------
  Copyright © 2010 Zinsser Analytic GmbH - All rights reserved.
  Author       : Payman Kamali (pk)
  Description  :
  -----------------------------------------------------------------------------------------------------------------------
  Revision History:
  date     op  method                                track-no  improvement/change
  -------- --  ------------------------------------  --------- ----------------------------------------------------------
  19.10.10 pk                                        TN5305     Initial revision
  11.09.11 wl  fAppInstanceAppDataLocal              TN5672   Instanz wird hier verwaltet
  11.09.11 wl  fAppInstanceRunCommon                 TN5672   Instanz wird hier verwaltet
  ----------------------------------------------------------------------------------------------------------------------- }

unit AppInstanceRunAppDataLib;


interface


uses
    AppInstanceAppDataLocalLib,
    AppInstanceRunCommon,
    CommonTypes;

type
    TAppInstanceRunAppDataLib = class
    private
        fAppInstanceAppDataLocal: TAppInstanceAppDataLocal;
        fAppInstanceRunCommon: TAppInstanceRunCommon;
        class var uInstance: TAppInstanceRunAppDataLib; // Single Instance
        class function GetInstance(): TAppInstanceRunAppDataLib; static;

        constructor Create(aPurpose: TAppPurpose; const aAppHInstance: LongWord);
    public
        destructor Destroy(); override;
        class procedure CreateInstance(aPurpose: TAppPurpose; const aAppHInstance: LongWord);
        class procedure DestroyInstance();

        class property Instance: TAppInstanceRunAppDataLib read GetInstance;
    end;


implementation


uses
    SysUtils;

{ TAppInstanceRunAppDataLib }

class procedure TAppInstanceRunAppDataLib.CreateInstance(aPurpose: TAppPurpose;
    const aAppHInstance: LongWord);
begin
    // create instance if instance does not exist
    if not Assigned(uInstance) then
        uInstance := TAppInstanceRunAppDataLib.Create(aPurpose, aAppHInstance);
end;

class function TAppInstanceRunAppDataLib.GetInstance: TAppInstanceRunAppDataLib;
begin
    result := uInstance;
end;

constructor TAppInstanceRunAppDataLib.Create(aPurpose: TAppPurpose; const aAppHInstance: LongWord);
begin
    inherited Create();
    fAppInstanceAppDataLocal := TAppInstanceAppDataLocal.Create(aPurpose, mctDirectConnection, aAppHInstance);
    fAppInstanceRunCommon := TAppInstanceRunCommon.Create();
end;

destructor TAppInstanceRunAppDataLib.Destroy;
begin
    FreeAndNil(fAppInstanceRunCommon);
    FreeAndNil(fAppInstanceAppDataLocal);
    inherited;
end;

class procedure TAppInstanceRunAppDataLib.DestroyInstance;
begin
    FreeAndNil(uInstance);
end;


end.
