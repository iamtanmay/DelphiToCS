{ -----------------------------------------------------------------------------------------------------------------------
  Copyright © 2010 Zinsser Analytic GmbH - All rights reserved.
  Author       : Payman Kamali (pk)
  Description  :
  -----------------------------------------------------------------------------------------------------------------------
  Revision History:
  date     op  method                                track-no  improvement/change
  -------- --  ------------------------------------  --------- ----------------------------------------------------------
  21.07.10 pk                                        TN5203    initial revision
  ----------------------------------------------------------------------------------------------------------------------- }

unit AppInstanceAppDataAdaptor;


interface


uses
    ListClasses,
    CommonTypes,
    AppInstanceLogging,
    AppTypes,
    DataProvider,
    DatabaseProvider;

type
    TAppInstanceAppDataAdaptor = class
    private
        class var uInstDataAdaptorCommon: TAppInstanceAppDataAdaptor; // Single Instance
        class function GetInstance(): TAppInstanceAppDataAdaptor; static;
        constructor Create();
    public
        destructor Destroy(); override;
        class function CreateInstance(): TAppInstanceAppDataAdaptor;
        class procedure DestroyInstance();
        class property Instance: TAppInstanceAppDataAdaptor read GetInstance;
    end;


implementation


uses
    SysUtils,
    MethodDataAdaptor;

{ TAppInstanceAppDataAdaptor }

class function TAppInstanceAppDataAdaptor.CreateInstance(): TAppInstanceAppDataAdaptor;
begin
    // create instance if instance does not exist
    if not Assigned(uInstDataAdaptorCommon) then
        uInstDataAdaptorCommon := TAppInstanceAppDataAdaptor.Create();

    // return instance
    result := uInstDataAdaptorCommon;
end;

class function TAppInstanceAppDataAdaptor.GetInstance: TAppInstanceAppDataAdaptor;
begin
    result := uInstDataAdaptorCommon;
end;

class procedure TAppInstanceAppDataAdaptor.DestroyInstance;
begin
    FreeAndNil(uInstDataAdaptorCommon);
end;

constructor TAppInstanceAppDataAdaptor.Create();
begin
    inherited Create();
    TMethodDataAdaptor.CreateInstance();
end;

destructor TAppInstanceAppDataAdaptor.Destroy;
begin
    TMethodDataAdaptor.DestroyInstance();
    inherited;
end;


end.
