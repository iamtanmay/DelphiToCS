{ ----------------------------------------------------------------------------------------------------------------------
  Copyright  2008 Zinsser Analytic GmbH - All rights reserved.
  Author       : Wolfgang Lyncke (wl)
  Description  : Inherits functionality to load dynamic packages with type info lists
  ----------------------------------------------------------------------------------------------------------------------
  Revision History:
  date     op  method                       track-no improvement/change
  -------- --  ---------------------------  -------- -------------------------------------------------------------------
  14.04.08 wl                               TN4060    initial version
  21.05.08 wl  gGlobalTypeDictionary        TN4119    stores all type infos that have been loaded from plugings
  13.10.08 pk  LoadGlobalTypeDictionary     TN4272.1  LogFT changed to LogF - faster
  28.05.09 wl  LoadGlobalTypeDictionary     TN4576    Es wird nur nach normalen Dateien gesucht (normal, readonly, hidden und archive)
  06.07.09 pk  SetPluginPath                TN4585.4  New
  02.09.09 pk                               TN4753    New uRefCount, CreateInstance, DestroyInstance
  17.05.10 wl                               TN5111    benutzt FileUtilities
  17.06.10 pk                               TN5152.1  No logging because logging is not available at this point
  04.08.10 pk                               TN5218    Changes for Plugin database packages
  11.09.11 wl  DestroyInstance              TN5672   Instanzen müssen IMMER mit FreeAndNil zerstört werden!
  ---------------------------------------------------------------------------------------------------------------------- }

unit PluginLoader;


interface


uses
    Forms,
    SysUtils,
    LibLoader,
    TypeInfo,
    TypeDictionary;

type
    TPluginLoader = class
    private const
        cDatabasePluginDir = 'Database';
    private
        fGlobalTypeDictionary: TTypeDictionary;
        procedure GetTypes(const aTypeDictionary: TTypeDictionary);
        class var uInstance: TPluginLoader;
        class var uRefCount: integer;
        class var uPluginPath: string;
        constructor Create();
        class property RefCount: integer read uRefCount write uRefCount;
        function LoadGlobalTypeDictionary(aTypeDictionary: TTypeDictionary; const aPath: string): integer;
        function GetRegularPluginPath(): string;
        function GetDatabasePluginPath(): string;
    public
        destructor Destroy(); override;
        procedure LoadDatabasePlugins();
        procedure LoadRegularPlugins();

        class procedure CreateInstance();
        class procedure DestroyInstance();
        class function Instance(): TPluginLoader;
        class procedure LoadAllTypes(const aTypeDictionary: TTypeDictionary);
        class procedure SetPluginPath(const aPluginPath: string);
    end;


implementation


uses
    FileUtilities,
    GeneralTypes;

{ TPluginLoader }

constructor TPluginLoader.Create();
begin
    inherited;
    TLibLoader.CreateInstance();
    fGlobalTypeDictionary := TTypeDictionary.Create('Plugin Types');
end;

destructor TPluginLoader.Destroy;
begin
    FreeAndNil(fGlobalTypeDictionary);
    TLibLoader.DestroyInstance();
    inherited;
end;

procedure TPluginLoader.GetTypes(const aTypeDictionary: TTypeDictionary);
begin
    aTypeDictionary.AddTypes(fGlobalTypeDictionary.TypeInfos);
end;

class function TPluginLoader.Instance: TPluginLoader;
begin
    result := uInstance;
end;

function TPluginLoader.LoadGlobalTypeDictionary(aTypeDictionary: TTypeDictionary;
    const aPath: string): integer;
var
    x: integer;
    xFileName: string;
    xTypeInfoList: TTypeInfoList;
    xFiles: TStringArray;
begin
    result := 0;

    // oPath := xLibDir;

    // gLogManager.Log( 'Load Plugins Search Path ' + oPath + '*.BPL', false );

    xFiles := TFileUtilities.GetListOfFiles(aPath, 'BPL');

    for x := 0 to high(xFiles) do
    begin

        xFileName := TFileUtilities.IncludeTrailingPathDelimiter(aPath) + xFiles[x];

        xTypeInfoList := nil;
        // gLogManager.Log( 'Load Plugin ' + xFileName + ' - START', false );
        TLibLoader.Instance.LoadLibFunction(xFileName, 'GETMODULETYPES', xTypeInfoList);
        if not Assigned(xTypeInfoList) then
            CONTINUE;

        fGlobalTypeDictionary.AddTypes(xTypeInfoList);
        // gLogManager.Log( 'Load Plugin ' + xFileName + ' - END', false );
        inc(result);
    end;
end;

function TPluginLoader.GetRegularPluginPath(): string;
begin
    result := TFileUtilities.IncludeTrailingPathDelimiter(uPluginPath);
end;

function TPluginLoader.GetDatabasePluginPath(): string;
begin
    result := TFileUtilities.IncludeTrailingPathDelimiter(GetRegularPluginPath) + cDatabasePluginDir;
end;

procedure TPluginLoader.LoadRegularPlugins();
var
    xPath: string;
begin
    xPath := GetRegularPluginPath();
    LoadGlobalTypeDictionary(fGlobalTypeDictionary, xPath);
    // gLogManager.LogF( 'Plugin path %s: %d Plugins loaded', [ xPath, xNoOfFiles ], true );
end;

procedure TPluginLoader.LoadDatabasePlugins();
var
    xPath: string;
begin
    xPath := GetDatabasePluginPath();
    LoadGlobalTypeDictionary(fGlobalTypeDictionary, xPath);
end;

class procedure TPluginLoader.CreateInstance();
begin
    if not Assigned(uInstance) then
        uInstance := TPluginLoader.Create();

    Inc(uRefCount);
end;

class procedure TPluginLoader.DestroyInstance;
begin
    if uRefCount = 1 then
        FreeAndNil(uInstance);

    Dec(uRefCount);
end;

class procedure TPluginLoader.LoadAllTypes(const aTypeDictionary: TTypeDictionary);

begin
    ASSERT(Assigned(uInstance));
    uInstance.GetTypes(aTypeDictionary);

end;

class procedure TPluginLoader.SetPluginPath(const aPluginPath: string);
begin
    uPluginPath := aPluginPath;
end;


initialization


TPluginLoader.RefCount := 0;
TPluginLoader.SetPluginPath(IncludeTrailingPathDelimiter(TFileUtilities.GetRealApplicationPath() +
    'Lib\Plugins'));


end.
