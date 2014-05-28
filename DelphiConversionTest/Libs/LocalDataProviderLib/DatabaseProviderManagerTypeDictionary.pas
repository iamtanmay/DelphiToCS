{ -----------------------------------------------------------------------------------------------------------------------
  Copyright © 2010 Zinsser Analytic GmbH - All rights reserved.
  Author       : Payman Kamali (pk)
  Description  :
  -----------------------------------------------------------------------------------------------------------------------
  Revision History:
  date     op  method                                track-no  improvement/change
  -------- --  ------------------------------------  --------- ----------------------------------------------------------
  04.08.10 pk                                        TN5218    Changes for Plugin database packages
  11.09.11 wl  fLoadFromPlugins                      TN5672   Laden von Plugins kann auch abgeschaltet werden
  ----------------------------------------------------------------------------------------------------------------------- }

unit DatabaseProviderManagerTypeDictionary;


interface


uses
    TypeDictionary,
    TypeInfo;

type
    TDatabaseProviderManagerTypeDictionary = class(TTypeDictionary)
    private
        fLoadFromPlugins: boolean;
    protected
        procedure InitTypeInfoList; override;
        function IsValidType(aTypeInfo: TTypeInfo): boolean; override;
    public
        constructor Create(aLoadFromPlugins: boolean);
        function ReadAllNames(): TArray<string>;
    end;


implementation


uses
    PluginLoader,
    DatabaseProviderManagerTypeInfo;

{ TDatabaseProviderManagerTypeDictionary }

function TDatabaseProviderManagerTypeDictionary.IsValidType(aTypeInfo: TTypeInfo): boolean;
begin
    result := aTypeInfo is TDatabaseProviderManagerTypeInfo;
end;

constructor TDatabaseProviderManagerTypeDictionary.Create(aLoadFromPlugins: boolean);
begin
    fLoadFromPlugins := aLoadFromPlugins;
    inherited Create('DatabaseProviderManager Type Dictionary');
end;

procedure TDatabaseProviderManagerTypeDictionary.InitTypeInfoList;
begin
    if fLoadFromPlugins then
        TPluginLoader.LoadAllTypes(self);
end;

function TDatabaseProviderManagerTypeDictionary.ReadAllNames(): TArray<string>;
begin
    result := self.ReadTypeNames(true);
end;


end.
