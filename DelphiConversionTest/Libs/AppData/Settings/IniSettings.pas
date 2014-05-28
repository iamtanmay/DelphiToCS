{ --------------------------------------------------------------------------------------------------
  Copyright © 2003 Zinsser Analytic GmbH - All rights reserved.
  Author       : Wolfgang Lyncke (wl)
  Project      : ZACommon.dll
  Description  : Class that knows about the ini access type and the different settings areas
  --------------------------------------------------------------------------------------------------
  Revision History:
  date     op  method                       track-no improvement/change
  -------- --  ---------------------------  -------- -----------------------------------------------
  14.01.03 wl                               TN1293.5 initial version
  23.01.03 wl  PrepareIniAccess             TN1293.6 Konvertierung funktioniert jetzt
  05.02.03 wl  PrepareIniAccess             TN1293.6 Konvertierung aufgeräumt
  06.02.03 wl  PrepareIniAccess             TN1334.3 Anpassung an ConvertAndSave (User = nil)
  11.02.03 wl  GetAreaFromAnyName           TN1334.3 neu: wird auch außerhalb des IniDataAdaptors benötigt
  07.03.03 tbh ConvertTubeToolSection       TN1443   neue Schüttelparameter ergänzt
  16.04.03 tbh CreateIndividualArea         TN1468   neue Area 'DLL' eingefügt
  16.04.03 tbh CreateDLLIni                 TN1468   neu
  03.06.03 wl  PrepareIniAccess             TN1485.1 Platform wird hier definiert (auch vor Konvertierung)
  25.07.03 tbh PrepareIniAccess             TN1448   Aufrufe für Updatefunktionen korrigiert
  25.07.03 tbh ConvertTubeToolSection       TN1448   ToolBringBackVOffset ergänzt / Defaults angepasst
  25.07.03 tbh UpdateTubeToolData           TN1448   Prüfung ergänzt
  28.04.04 wl  CreateZP02CfgIni             TN1788.9 kann primitive Daten aus SiasCfg.ini lesen
  10.05.04 wl  CreateIndividualArea         TN1788.9 CFG02-Daten werden im Settingseditor richtig angezeigt
  18.05.04 wl  ConvertTubeToolSection       TN1873   Werte für Z-Drop, V-Drop und V-Open werden jetzt nicht mehr vertauscht
  16.09.04 wl  ConvertTubeToolSection       TN2138   Konvertierung für alle Tube-Daten, außer "TakeTubes"
  16.08.05 wl  CreateMethodIni              TN2558.4  aDeleteAllAllowed wird auf true gesetzt
  31.03.06 pk  ConvertTubeToolSection       TN3009   ToolData fields changed to TPosMM
  31.08.07 wl                               TN3811.4 uses IniDataWrapper
  31.08.07 wl  CreateSettingsTable          TN3811.4 erzeugt Settings-Tabelle mit dem Update-Manager
  09.11.07 pk  ConvertTubeToolSection       TN3924   Steps changed to mm
  09.11.07 pk  CreateSettingsTable          TN3922   removed for now
  11.06.08 wl                               TN4143   TZP01CfgIniAccess, TZP02CfgIniAccess entfernt
  11.06.08 wl                               TN4143   alle Konevertier-Funktionen entfernt
  06.08.08 pk                               TN4165.1 various changes for using SettingsDataCache
  07.08.09 wl                               TN4702   Strings werden jetzt direkt geladen
  04.11.09 pk                               TN4843   Replace TObjectList, TStringList, TStrings with Generics Lists and TStringArray
  15.11.10 pk                               TN5340   Changes to prevent memory leak
  -------------------------------------------------------------------------------------------------- }

unit IniSettings;


interface


uses
    GeneralTypes,
    WinlissyIniAccess,
    SimpleIniAccess,
    CommonTypes,
    IniAccess,
    IniDataWrapper,
    SettingsDataAdaptor,
    SettingsDataCache;

type
    TBasicIniSettings = class
    protected
        fDataPath: string;
        fAccessType: TIniAccessType;
        fDataAdaptor: TSettingsDataAdaptor;
        fDataCache: TSettingsDataCache;
        function CreateIniDataWrapper(aAreaName: string; aAccessType: TIniAccessType): TIniDataWrapper;
        function CreateInitDBDataWrapper(const aAreaName: string): TIniDataWrapper;
    public
        constructor Create(const aDataPath: string);
        destructor Destroy(); override;
        function CreateSimpleIni(aAreaName: string): TSimpleIniAccess;
        property DataCache: TSettingsDataCache read fDataCache;
    end;

    TIniSettings = class(TBasicIniSettings)
    public
        // constructor
        constructor Create(const aDataPath: string);
        // public methods
        function GetAreaNames(): TStringArray;
        function CreateIndividualArea(aAreaName: string; aAccessType: TIniAccessType): TIniAccess;
        // function CreateSimpleIni(aAreaName: string): TSimpleIniAccess;
        function CreateAppIni: TWinlissyIniAccess;
        function CreateRobotIni: TWinlissyIniAccess;
        function CreateDLLIni: IWinlissyIniAccess;
        function GetAreaFromAnyName(aAnyName: string): string;
        // properties
        property AccessType: TIniAccessType read FAccessType;
    end;


implementation


uses
    Windows,
    IniFiles,
    SysUtils,
    DataAdaptor,
    LogManager;

constructor TBasicIniSettings.Create(const aDataPath: string);
begin
    inherited Create;
    fDataPath := aDataPath;

    // ab Version 5.8: Umstellung des Ini-Access
    fAccessType := iatDatabaseAccess;

    fDataAdaptor := TSettingsDataAdaptor.Create();
    fDataCache := TSettingsDataCache.Create(fDataAdaptor);
end;

destructor TBasicIniSettings.Destroy;
begin
    FreeAndNil(fDataCache);
    FreeAndNil(fDataAdaptor);
    inherited;
end;

function TBasicIniSettings.CreateInitDBDataWrapper(const aAreaName: string): TIniDataWrapper;
begin
    result := TIniDBDataWrapper.Create(fDataCache.FindOrCreateArea(aAreaName));

end;

function TBasicIniSettings.CreateIniDataWrapper(aAreaName: string; aAccessType: TIniAccessType)
    : TIniDataWrapper;
begin
    if (aAccessType = iatDatabaseAccess) then
        result := CreateInitDBDataWrapper(aAreaName)
    else
        result := TIniFileDataWrapper.Create(aAreaName, FDataPath);
end;

function TBasicIniSettings.CreateSimpleIni(aAreaName: string): TSimpleIniAccess;
begin
    result := TSimpleIniAccess.Create(CreateIniDataWrapper(aAreaName, FAccessType));
end;

{ TIniSettings }

constructor TIniSettings.Create(const aDataPath: string);
begin
    inherited Create(aDataPath);
end;

function TIniSettings.GetAreaNames(): TStringArray;
var
    xDA: TSettingsDataAdaptor;
begin
    if (FAccessType = iatDatabaseAccess) then
    begin
        xDA := TSettingsDataAdaptor.Create();
        try
            result := xDA.GetAreaNames();
        finally
            FreeAndNil(xDA);
        end;
    end
    else
    begin
        result := TIniFileDataWrapper.GetAreaNames(FDataPath);
    end;
end;

function TIniSettings.CreateAppIni: TWinlissyIniAccess;
begin
    result := TZPAppIniAccess.Create(CreateIniDataWrapper(STR_SETTINGS_AREA_APP, FAccessType));
end;

function TIniSettings.CreateRobotIni: TWinlissyIniAccess;
begin
    result := TZPRobotIniAccess.Create(CreateIniDataWrapper(STR_SETTINGS_AREA_ROBOT, FAccessType));
end;

function TIniSettings.CreateDLLIni: IWinlissyIniAccess;
begin
    result := TZPDLLIniAccess.Create(CreateIniDataWrapper(STR_SETTINGS_AREA_DLL, FAccessType));
end;

function TIniSettings.CreateIndividualArea(aAreaName: string; aAccessType: TIniAccessType): TIniAccess;
// used by settings editor
var
    xIniDataWrapper: TIniDataWrapper;
begin
    xIniDataWrapper := CreateIniDataWrapper(aAreaName, aAccessType);

    if (xIniDataWrapper.Area = STR_SETTINGS_AREA_ROBOT) then
        result := TZPRobotIniAccess.Create(xIniDataWrapper)
    else if (xIniDataWrapper.Area = STR_SETTINGS_AREA_APP) then
        result := TZPAppIniAccess.Create(xIniDataWrapper)
    else if (xIniDataWrapper.Area = STR_SETTINGS_AREA_DLL) then
        result := TZPDLLIniAccess.Create(xIniDataWrapper)
    else
        result := TSimpleIniAccess.Create(xIniDataWrapper)
end;

function TIniSettings.GetAreaFromAnyName(aAnyName: string): string;
begin
    result := TIniDataWrapper.GetAreaFromAnyName(aAnyName);
end;


end.
