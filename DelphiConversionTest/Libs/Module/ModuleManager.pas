{ --------------------------------------------------------------------------------------------------
  Copyright © 2007 Zinsser Analytic GmbH - All rights reserved.
  Author       : Payman Kamali (pk)
  Description  :
  --------------------------------------------------------------------------------------------------
  Revision History:
  date     op  method                       track-no improvement/change
  -------- --  ---------------------------  -------- -----------------------------------------------
  05.09.07 pk                                         Initial Revision
  09.11.07 pk                                TN3864   FindModuleExt vSearchindex := x + 1 instead of x
  07.01.08 pk                                TN3864   NEw DestroyModules, PrepareModules, UnprepareModules, DisconnectModules
  07.02.08 wl  TModuleManager                TN4009   Fehler werden geloggt und in eine Liste geschrieben
  14.04.08 wl  GetModuleAreaName,GetSettingAlias  TN4060   normal function instead of class function
  26.05.08 wl  CreateModuleSettings          TN4119   uses new TModuleTypeInfo-method
  26.05.08 wl  CreateModuleByTypeName        TN4119   uses new TModuleTypeInfo-method
  11.07.08 wl  ModuleTypeExists,FindFirst    TN4164   neue Funktionen zum Suchen von Modulen
  16.07.08 wl  TModuleList.FindModuleByName  TN4164   von ModuleManager nach ModuleList verschoben
  16.07.08 wl  TModuleList.FindNextModule    TN4164   von ModuleManager nach ModuleList verschoben
  26.08.08 wl  TModuleManager.PrepareModules TN4164   ist jetzt virtual;
  26.08.08 wl  TModuleList.Remove            TN4164   neu
  13.10.08 pk  ReadModuleType                TN4272.2 now abstract
  17.12.08 pk                                TN4374   Some Code moved to ModuleSettingsManager
  11.08.09 wl  ReadModuleNames               TN4702    TStringArray statt TStringList
  04.11.09 pk                                TN4843   Replace TObjectList, TStringList, TStrings with Generics Lists and TStringArray
  11.11.09 pk  GetNames                      TN4856   New
  18.02.10 pk                                TN4985.1 Connect/DisconnectModules: Logging
  15.11.10 pk                                TN5340   Changes to prevent memory leak
  19.11.10 wl  Activate-,DeactivateModules   TN5358  neu
  04.04.11 wl  FindModules                   TN5525  neue coole Funktion erzeugt generisches Array
  16.08.11 ts  FindModuleByName              TN5660  wenn kein Motor angegeben ist, trotzdem speichern, damit vollständiges Motor-Array erzeugt wird (wichtig,falls bsp. nur Tip 1,2,4 genutzt werden)
  27.03.13 wl                                TN6045   uses geändert
  -------------------------------------------------------------------------------------------------- }

unit ModuleManager;


interface


uses
    Module,
    ModuleTypeInfo,
    TypeInfo,
    Generics.Collections,
    ModuleSettings,
    ModuleSettingsManager;

type
    TModuleList = class
    protected
        fList: TList<IModule>; // Man sollte mal TObjectList<TModule> testen
    private
        function GetCount: integer;
        function GetModule(aIndex: integer): IModule;
        procedure SetModule(aIndex: integer; const aValue: IModule);
    public
        // constructor/destructor
        constructor Create;
        destructor Destroy; override;
        // Public Methods
        function Add(const aModule: IModule): Integer;
        function IndexOf(const aModuleName: string): integer;
        procedure Clear();
        function FindModuleByName(aMustFind: boolean; const aModuleName: string; aExpectedModuleID: TModuleID;
            out oIntf): boolean;
        function FindNextModule(aExpectedModuleID: TModuleID; var vSearchIndex: integer; out oIntf): boolean;
        function Remove(const aModule: IModule): integer;
        function GetNames(): TArray<string>;
        function FindModules<T: IModule>: TArray<T>;
        // Properties
        property Count: integer read GetCount;
        property this[aIndex: Integer]: IModule read GetModule write SetModule; default;
    end;

    TModuleManager = class
    protected
        fModules: TModuleList;
        fModuleSettingsManager: TModuleSettingsManager;
        function ReadModuleNames(): TArray<string>;
        function GetModuleType(const aModuleTypeName: string): TModuleTypeInfo;
        procedure LogAndAddError(const aErrors: TList<string>; const aCurrentError: string);
        function GetModuleAreaName(): string;
    public
        constructor Create(const aModuleSettingsManager: TModuleSettingsManager);
        destructor Destroy(); override;
        function CreateModuleByTypeName(const aModuleName: string; const aModuleTypeName: string): IModule;
        procedure CreateModules(const aErrors: TList<string>);
        function CreateModule(const aErrors: TList<string>; const aModuleName: string): IModule;
        procedure DestroyModules;
        procedure PrepareModules(const aErrors: TList<string>); virtual;
        procedure UnPrepareModules(const aErrors: TList<string>);
        procedure ConnectModules(const aErrors: TList<string>);
        procedure DisconnectModules(const aErrors: TList<string>);
        procedure ActivateModules(const aErrors: TList<string>);
        procedure DeactivateModules(const aErrors: TList<string>);
        function FindModuleExt(aExpectedModuleID: TModuleID; var vSearchIndex: integer; out oIntf): boolean;
        function FindModule(aMustFind: boolean; const aModuleName: string; aExpectedModuleID: TModuleID;
            out oIntf): boolean;
        function FindModules<T: IModule>(): TArray<T>;
        function FindFirst(aExpectedModuleID: TModuleID; out oIntf): boolean;
        function ModuleTypeExists(aExpectedModuleID: TModuleID): boolean;
        property Modules: TModuleList read fModules;
    end;


implementation


uses
    SysUtils,
    LogManager,
    TypInfo;

{ TModuleList }

constructor TModuleList.Create;
begin
    inherited Create;
    fList := TList<IModule>.Create;
end;

destructor TModuleList.Destroy;
begin
    FreeAndNil(fList);
    inherited Destroy;
end;

function TModuleList.GetCount: integer;
begin
    result := FList.Count;
end;

function TModuleList.GetModule(aIndex: integer): IModule;
begin
    result := fList[aIndex] as IModule;
end;

function TModuleList.GetNames: TArray<string>;
var
    x: integer;
begin
    SetLength(result, fList.Count);
    for x := 0 to fList.Count - 1 do
        result[x] := self[x].Name;
end;

procedure TModuleList.SetModule(aIndex: integer; const aValue: IModule);
begin
    fList[aIndex] := aValue;
end;

function TModuleList.Add(const aModule: IModule): Integer;
begin
    result := FList.Add(aModule);
end;

function TModuleList.IndexOf(const aModuleName: string): integer;
var
    x: integer;
begin
    result := -1;
    for x := 0 to fList.Count - 1 do
    begin
        if SameText(self[x].Name, aModuleName) then
        begin
            result := x;
            EXIT;
        end;
    end;
end;

procedure TModuleList.Clear;
begin
    fList.Clear;
end;

function TModuleList.FindModuleByName(aMustFind: boolean; const aModuleName: string;
    aExpectedModuleID: TModuleID; out oIntf): boolean;
var
    xIndex: integer;
begin
    result := false;
    if aModuleName = 'NIL' then
    begin
        result := true;
        EXIT;
    end;
    xIndex := self.IndexOf(aModuleName);
    if xIndex < 0 then
    begin
        if aMustFind then
            raise Exception.CreateFmt('Module %s was not found', [aModuleName]);
        EXIT;
    end;

    result := Supports(fList[xIndex], aExpectedModuleID, oIntf);
    if not result then
    begin
        if aMustFind then
            raise Exception.CreateFmt('Module %s was found but, had incorrect interface', [aModuleName]);
        EXIT;
    end;
end;

function TModuleList.FindNextModule(aExpectedModuleID: TModuleID; var vSearchIndex: integer;
    out oIntf): boolean;
var
    x: integer;
begin
    result := false;
    for x := vSearchIndex to fList.Count - 1 do
    begin
        if Supports(fList[x], aExpectedModuleID, oIntf) then
        begin
            result := true;
            // Note: we return x + 1 because we want the next call to FindModuleExt to start searching at the next index
            vSearchIndex := x + 1;
            EXIT;
        end;
    end;
end;

function TModuleList.Remove(const aModule: IModule): integer;
begin
    result := fList.Remove(aModule);
end;

function TModuleList.FindModules<T>: TArray<T>;
var
    x: integer;
    aExpectedModuleID: TGUID;
    xList: TList<T>;
    xCurrentModule: T;
begin
    aExpectedModuleID := TypInfo.GetTypeData(System.TypeInfo(T))^.Guid;

    xList := TList<T>.Create;
    try
        for x := 0 to fList.Count - 1 do
        begin
            if Supports(fList[x], aExpectedModuleID, xCurrentModule) then
            begin
                xList.Add(xCurrentModule);
            end;
        end;
        result := xList.ToArray;
    finally
        FreeAndNil(xList);
    end;
end;

{ TModuleManager }

constructor TModuleManager.Create(const aModuleSettingsManager: TModuleSettingsManager);
begin
    inherited Create;
    fModuleSettingsManager := aModuleSettingsManager;
    fModules := TModuleList.Create();
end;

destructor TModuleManager.Destroy;
begin
    FreeAndNil(fModules);
    inherited;
end;

function TModuleManager.GetModuleAreaName: string;
begin
    result := fModuleSettingsManager.GetAreaName;
end;

function TModuleManager.GetModuleType(const aModuleTypeName: string): TModuleTypeInfo;
begin
    result := fModuleSettingsManager.GetModuleTypeInfo(aModuleTypeName);
end;

function TModuleManager.ReadModuleNames(): TArray<string>;
begin
    result := fModuleSettingsManager.ReadModuleNames();
end;

function TModuleManager.CreateModuleByTypeName(const aModuleName: string;
    const aModuleTypeName: string): IModule;

var
    xModuleType: TModuleTypeInfo;
begin
    result := nil;
    xModuleType := GetModuleType(aModuleTypeName);
    if not Assigned(xModuleType) then
        EXIT;
    result := xModuleType.CreateModule(self.GetModuleAreaName(), aModuleName);
end;

function TModuleManager.CreateModule(const aErrors: TList<string>; const aModuleName: string): IModule;
var
    xTypeName: string;
begin
    try
        xTypeName := fModuleSettingsManager.ReadModuleType(aModuleName);
        result := CreateModuleByTypeName(aModuleName, xTypeName);
    except
        on e: exception do
        begin
            LogAndAddError(aErrors, 'Failed loading module:' + aModuleName);
        end;
    end;
end;

procedure TModuleManager.CreateModules(const aErrors: TList<string>);
var
    xNames: TArray<string>;
    x: integer;
    xModule: IModule;
begin
    xNames := ReadModuleNames();
    for x := 0 to high(xNames) do
    begin
        xModule := CreateModule(aErrors, xNames[x]);
        if not Assigned(xModule) then
            CONTINUE;
        fModules.Add(xModule);
    end;
end;

procedure TModuleManager.DestroyModules;
var
    x: integer;
begin
    for x := 0 to fModules.Count - 1 do
    begin
        fModules[x] := nil;
    end;
end;

procedure TModuleManager.PrepareModules(const aErrors: TList<string>);
var
    x: integer;
begin
    for x := 0 to fModules.Count - 1 do
    begin
        try
            fModules[x].Prepare;
        except
            on e: exception do
            begin
                LogAndAddError(aErrors, 'Failed preparing module:' + fModules[x].Name + '-->' + e.Message);
            end;
        end;
    end;
end;

procedure TModuleManager.UnPrepareModules(const aErrors: TList<string>);
var
    x: integer;
begin
    for x := 0 to fModules.Count - 1 do
    begin
        try
            fModules[x].UnPrepare;
        except
            on e: exception do
            begin
                LogAndAddError(aErrors, 'Failed Unpreparing module:' + fModules[x].Name + '-->' + e.Message);
            end;
        end;
    end;
end;

procedure TModuleManager.ActivateModules(const aErrors: TList<string>);
var
    x: integer;
begin
    for x := 0 to fModules.Count - 1 do
    begin
        try
            fModules[x].Activate();
        except
            on e: exception do
            begin
                LogAndAddError(aErrors, 'Failed Activating module:' + fModules[x].Name + '-->' + e.Message);
            end;
        end;
    end;
end;

procedure TModuleManager.DeactivateModules(const aErrors: TList<string>);
var
    x: integer;
begin
    for x := 0 to fModules.Count - 1 do
    begin
        try
            fModules[x].Deactivate();
        except
            on e: exception do
            begin
                LogAndAddError(aErrors, 'Failed Deactivating module:' + fModules[x].Name + '-->' + e.Message);
            end;
        end;
    end;
end;

procedure TModuleManager.ConnectModules(const aErrors: TList<string>);
var
    x: integer;
    xModule: IModule;
begin
    for x := 0 to fModules.Count - 1 do
    begin
        try
            xModule := fModules[x];

            gLogManager.LogF('Connect %s - START', [xModule.Name], false);
            xModule.Connect();
            gLogManager.LogF('Connect %s - FINISH', [xModule.Name], false);
        except
            on e: exception do
            begin
                LogAndAddError(aErrors, 'Failed Connecting module:' + xModule.Name + '-->' + e.Message);
            end;
        end;
    end;
end;

procedure TModuleManager.DisconnectModules(const aErrors: TList<string>);
var
    x: integer;
    xModule: IModule;
begin
    for x := 0 to fModules.Count - 1 do
    begin
        try
            xModule := fModules[x];
            gLogManager.LogF('Disconnect %s - START', [xModule.Name], false);
            xModule.Disconnect;
            gLogManager.LogF('Disconnect %s - FINISH', [xModule.Name], false);
        except
            on e: exception do
            begin
                LogAndAddError(aErrors, 'Failed Disconnecting module:' + xModule.Name + '-->' + e.Message);
            end;
        end;
    end;
end;

function TModuleManager.FindModule(aMustFind: boolean; const aModuleName: string;
    aExpectedModuleID: TModuleID; out oIntf): boolean;
begin
    result := fModules.FindModuleByName(aMustFind, aModuleName, aExpectedModuleID, oIntf)
end;

function TModuleManager.FindModuleExt(aExpectedModuleID: TModuleID; var vSearchIndex: integer;
    out oIntf): boolean;
begin
    result := fModules.FindNextModule(aExpectedModuleID, vSearchIndex, oIntf);
end;

function TModuleManager.FindModules<T>: TArray<T>;
begin
    result := fModules.FindModules<T>;
end;

function TModuleManager.FindFirst(aExpectedModuleID: TModuleID; out oIntf): boolean;
var
    xSearchIndex: integer;
begin
    xSearchIndex := 0;
    // vereinfachte Form von FindModuleExt
    result := fModules.FindNextModule(aExpectedModuleID, xSearchIndex, oIntf);
end;

procedure TModuleManager.LogAndAddError(const aErrors: TList<string>; const aCurrentError: string);
begin
    gLogManager.Log(aCurrentError, false);
    aErrors.Add(aCurrentError);
end;

function TModuleManager.ModuleTypeExists(aExpectedModuleID: TModuleID): boolean;
var
    xIntf: IModule;
begin
    result := self.FindFirst(aExpectedModuleID, xIntf);
end;


end.
