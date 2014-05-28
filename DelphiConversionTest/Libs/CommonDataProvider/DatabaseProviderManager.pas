{ ----------------------------------------------------------------------------------------------------------------------
  Copyright © 2011 Zinsser Analytic GmbH - All rights reserved.
  Author       : Wolfgang Lyncke (wl)
  Description  : Verwaltung der TTDBDatabase-Instanzen
  ----------------------------------------------------------------------------------------------------------------------
  Revision History:
  date     op  method                                track-no improvement/change
  -------- --  ------------------------------------  -------- -------------------------------------------------------------------
  19.09.11 wl  CreateDatabaseProvider                TN5672   Neuer Parameter: aOwnsConnectionParams
  ---------------------------------------------------------------------------------------------------------------------- }

unit DatabaseProviderManager;


interface


uses
    GeneralTypes,
    DataProvider,
    DatabaseProvider,
    DataConnectionParams,
    DataConnectionParamEditor;

type
    TDatabaseProviderManager = class
    protected
        procedure DoPrepare(); virtual; abstract;
        procedure DoUnPrepare(); virtual; abstract;
        function DoCreateDatabaseProvider(const aConnectionParams: TDataConnectionParams;
            aOwnsConnectionParams: boolean): TDatabaseProvider; virtual; abstract;
        function DoCreateDataProvider(const aConnectionParams: TDataConnectionParams): TDataProvider;
            virtual; abstract;
        function DoGetSupportedTypeNames(): TStringArray; virtual; abstract;
        function DoCreateDataConnectionParams(const aTypeName: string): TDataConnectionParams;
            virtual; abstract;
        function DoCreateDataConnectionParamsEditor(const aTypeName: string): TDataConnectionParamsEditor;
            virtual; abstract;
    public
        procedure Prepare();
        procedure UnPrepare();
        procedure PrepareDatabase(const aConnectionParams: TDataConnectionParams); virtual;
        procedure UnPrepareDatabase(const aConnectionParams: TDataConnectionParams); virtual;
        function GetSupportedTypeNames(): TStringArray;
        function CreateDatabaseProvider(const aConnectionParams: TDataConnectionParams;
            aOwnsConnectionParams: boolean): TDatabaseProvider;
        function CreateDataProvider(const aConnectionParams: TDataConnectionParams): TDataProvider;
        function CreateDataConnectionParams(const aTypeName: string): TDataConnectionParams;
        function CreateDataConnectionParamsEditor(const aTypeName: string): TDataConnectionParamsEditor;
    end;


implementation


{ TDatabaseProviderManager }

function TDatabaseProviderManager.CreateDatabaseProvider(const aConnectionParams: TDataConnectionParams;
    aOwnsConnectionParams: boolean): TDatabaseProvider;
begin
    result := DoCreateDatabaseProvider(aConnectionParams, aOwnsConnectionParams);
end;

function TDatabaseProviderManager.CreateDataConnectionParams(const aTypeName: string): TDataConnectionParams;
begin
    result := DoCreateDataConnectionParams(aTypeName);
end;

function TDatabaseProviderManager.CreateDataConnectionParamsEditor(const aTypeName: string)
    : TDataConnectionParamsEditor;
begin
    result := DoCreateDataConnectionParamsEditor(aTypeName);
end;

function TDatabaseProviderManager.CreateDataProvider(const aConnectionParams: TDataConnectionParams)
    : TDataProvider;
begin
    result := DoCreateDataProvider(aConnectionParams);
end;

function TDatabaseProviderManager.GetSupportedTypeNames: TStringArray;
begin
    result := DoGetSupportedTypeNames();
end;

procedure TDatabaseProviderManager.Prepare;
begin
    DoPrepare;
end;

procedure TDatabaseProviderManager.UnPrepare;
begin
    DoUnPrepare;
end;

procedure TDatabaseProviderManager.PrepareDatabase(const aConnectionParams: TDataConnectionParams);
begin

end;

procedure TDatabaseProviderManager.UnPrepareDatabase(const aConnectionParams: TDataConnectionParams);
begin

end;


end.
