{ --------------------------------------------------------------------------------------------------
  Copyright © 2008 Zinsser Analytic GmbH - All rights reserved.
  Author       : Payman Kamali (pk)
  Description  : Data cache for liquid handling parameters
  --------------------------------------------------------------------------------------------------
  Revision History:
  date     op  method                       track-no improvement/change
  -------- --  ---------------------------  -------- -----------------------------------------------
  07.10.08 pk                                TN4265   Initial Revision
  04.11.09 pk                                TN4843 Replace TObjectList, TStringList, TStrings with Generics Lists and TStringArray
  11.04.13 wl                                TN6045   uses geändert
  30.08.13 wl                                TN6236   unit --> EditingMethod
  30.08.13 wl  fMethods                      TN6236   enthält jetzt alle MethodSettings
  28.11.13 ts  ReadBuildingBlockNames        TN6318   TemporaryMethod rausgefiltert
  04.04.14 ts                                TN6388   Methodenicons werden wieder aktualisiert bei Refresh
  -------------------------------------------------------------------------------------------------- }

unit MethodDataCache;


interface


uses
    Generics.Collections,
    MethodSettingsDataAdaptor;

type
    TMethodDataCache = class
    private
        fDA: TMethodSettingsDataAdaptor;
        fMethods: TList<TMethodSettingsRec>;
        fMethodsLastRead: TDateTime;
        class var uInstance: TMethodDataCache;
    public
        constructor Create();
        destructor Destroy(); override;
        procedure RefreshMethodNames();
        procedure AddMethodName(const aRec: TMethodSettingsRec);
        procedure DeleteMethodName(const aName: string);
        function GetMethodIndex(const aName: string): integer;
        function GetMethodNames(): TArray<string>;
        function GetMethodSettings(const aName: string): TMethodSettingsRec;
        function ReadBuildingBlockNames(): TArray<string>;
        function ReadMethodNames(): TArray<string>;

        property Methods: TList<TMethodSettingsRec>read fMethods;

        class function CreateInstance(): TMethodDataCache;
        class procedure DestroyInstance();
        class property Instance: TMethodDataCache read uInstance;
    end;


implementation


uses
    SysUtils,
    LayoutDataAdaptorExt;

{ TMethodDataCache }

constructor TMethodDataCache.Create;
begin
    inherited Create();
    fDA := TMethodSettingsDataAdaptor.Create();
    fMethods := TList<TMethodSettingsRec>.Create();
    fMethodsLastRead := 0;
    RefreshMethodNames();
end;

destructor TMethodDataCache.Destroy;
begin
    FreeAndNil(fMethods);
    FreeAndNil(fDA);
    inherited;
end;

class function TMethodDataCache.CreateInstance(): TMethodDataCache;
begin
    // create instance if instance does not exist
    if not Assigned(uInstance) then
        uInstance := TMethodDataCache.Create();

    // return instance
    result := uInstance;
end;

class procedure TMethodDataCache.DestroyInstance;
begin
    FreeAndNil(uInstance);
end;

function TMethodDataCache.GetMethodIndex(const aName: string): integer;
var
    x: integer;
begin
    for x := 0 to fMethods.Count - 1 do
    begin
        if (fMethods[x].MethodName = aName) then
            EXIT(x);
    end;
    EXIT(-1);
end;

function TMethodDataCache.GetMethodNames(): TArray<string>;
var
    x: integer;
begin
    SetLength(result, fMethods.Count);
    for x := 0 to fMethods.Count - 1 do
        result[x] := fMethods[x].MethodName;
end;

function TMethodDataCache.GetMethodSettings(const aName: string): TMethodSettingsRec;
var
    xIndex: integer;
begin
    xIndex := GetMethodIndex(aName);
    if xIndex < 0 then
        EXIT(TMethodSettingsDataAdaptor.GetEmptyRec);
    EXIT(fMethods[xIndex]);
end;

function TMethodDataCache.ReadBuildingBlockNames: TArray<string>;
var
    x: integer;
    xNames: TList<string>;
begin
    xNames := TList<string>.Create;
    try
        for x := 0 to fMethods.Count - 1 do
        begin
            if fMethods[x].Startable and
                (fMethods[x].MethodName <> TLayoutDataAdaptorExt.cTemporaryMethodName) then
                xNames.Add(fMethods[x].MethodName);
        end;
        EXIT(xNames.ToArray);
    finally
        FreeAndNil(xNames);
    end;
end;

function TMethodDataCache.ReadMethodNames: TArray<string>;
var
    x: integer;
begin
    SetLength(result, fMethods.Count);
    for x := 0 to fMethods.Count - 1 do
    begin
        if fMethods[x].Startable then
            result[x] := fMethods[x].MethodName;
    end;
end;

procedure TMethodDataCache.RefreshMethodNames;
var
    xDA: TMethodSettingsDataAdaptor;
    xRecs: TArray<TMethodSettingsRec>;
begin
    xDA := TMethodSettingsDataAdaptor.Create;
    try
        xRecs := xDA.ReadAllRecs;
    finally
        FreeAndNil(xDA);
    end;

    fMethods.Clear();
    fMethods.AddRange(xRecs);
end;

procedure TMethodDataCache.AddMethodName(const aRec: TMethodSettingsRec);
begin
    fMethods.Add(aRec);
end;

procedure TMethodDataCache.DeleteMethodName(const aName: string);
var
    xIndex: integer;
begin
    xIndex := GetMethodIndex(aName);
    if xIndex < 0 then
        EXIT;
    fMethods.Delete(xIndex);
end;


end.
