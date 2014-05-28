{ -----------------------------------------------------------------------------------------------------------------------
  Copyright © 2009 Zinsser Analytic GmbH - All rights reserved.
  Author       : Payman Kamali (pk)
  Description  :
  -----------------------------------------------------------------------------------------------------------------------
  Revision History:
  date     op  method                                track-no improvement/change
  -------- --  ---------------------------           -------- --------------------------------------------------------------------
  06.04.09 pk                                        TN4503  Initial Revision
  24.07.09 pk  FindDisplayComponentByDisplayIDExt    TN4675  New
  24.07.09 pk  CreateDisplayComponentByTypeName      TN4675  Add Component to fList
  27.08.09 pk                                        TN4753  Design and Run TypInfos seperated
  27.08.09 pk  LoadComponentToWindow                 TN4753  moved here from componentloader
  04.11.09 pk                                        TN4843   Replace TObjectList, TStringList, TStrings with Generics Lists and TStringArray
  13.11.12 wl                                        TN6015   überarbeitet für DisplayComponentsDataAdaptor
  23.11.12 wl                                        TN6015.1 Es gibt keine Instanz mehr von TDisplayComponentSettingsManager
  29.11.12 wl                                        TN6015.2 DesignDisplayComponents abgeschafft (Es gibt nur noch RunDisplayComponents)
  ----------------------------------------------------------------------------------------------------------------------- }

unit DisplayComponentManager;


interface


uses
    Generics.Collections,
    DisplayComponentIntf,
    DisplayComponentTypeInfo,
    DisplayComponentSettings;

type
    TDisplayComponentList = class
    protected
        fList: TList<IDisplayComponent>;
    private
        function GetCount: integer;
        function GetDisplayComponent(aIndex: integer): IDisplayComponent;
        procedure SetDisplayComponent(aIndex: integer; const aValue: IDisplayComponent);
    public
        // constructor/destructor
        constructor Create;
        destructor Destroy; override;
        // Public Methods
        function Add(aDisplayComponent: IDisplayComponent): Integer;
        function IndexOf(const aDisplayComponentName: string): integer;
        procedure Clear();
        function FindDisplayComponentByName(aMustFind: boolean; const aDisplayComponentName: string;
            aExpectedDisplayComponentID: TDisplayComponentID; out oIntf): boolean;
        function FindNextDisplayComponent(aExpectedDisplayComponentID: TDisplayComponentID;
            var vSearchIndex: integer; out oIntf): boolean;
        function Remove(aDisplayComponent: IDisplayComponent): integer;
        // Properties
        property Count: integer read GetCount;
        property DisplayComponents[aIndex: Integer]: IDisplayComponent read GetDisplayComponent
            write SetDisplayComponent; default;
    end;

    TDisplayComponentManager = class
    private
        fDisplayComponents: TDisplayComponentList;
        class var uInstance: TDisplayComponentManager;
        class function GetDisplayComponentType(const aDisplayComponentTypeName: string)
            : TDisplayComponentTypeInfo;
        class procedure LogAndAddError(aErrors: TList<string>; const aCurrentError: string);
        function DoLoadComponentToWindow(const aComponentName, aContextID: string;
            const aIsDesignMode: boolean; const aDisplayHandle: TDisplayHandle; aErrors: TList<string>)
            : IDisplayComponent;
    public
        constructor Create();
        destructor Destroy(); override;
        function CreateDisplayComponentByTypeName(const aDisplayComponentName: string;
            const aDisplayComponentTypeName: string): IDisplayComponent;
        function CreateDisplayComponent(aErrors: TList<string>; const aDisplayComponentName: string)
            : IDisplayComponent;

        procedure ClearDisplayComponents;

        function FindDisplayComponentExt(aExpectedDisplayComponentID: TDisplayComponentID;
            var vSearchIndex: integer; out oIntf): boolean;
        function FindDisplayComponent(aMustFind: boolean; const aDisplayComponentName: string;
            aExpectedDisplayComponentID: TDisplayComponentID; out oIntf): boolean;
        function FindFirst(aExpectedDisplayComponentID: TDisplayComponentID; out oIntf): boolean;

        function FindDisplayComponentByDisplayIDExt(aExpectedDisplayComponentID: TDisplayComponentID;
            const aDisplayID: string; var vSearchIndex: integer; out oIntf): boolean;
        function DisplayComponentTypeExists(aExpectedDisplayComponentID: TDisplayComponentID): boolean;

        function LoadComponentToWindow(const aComponentName, aContextID: string; const aIsDesignMode: boolean;
            const aDisplayHandle: TDisplayHandle; aErrors: TList<string>): IDisplayComponent;

        property DisplayComponents: TDisplayComponentList read fDisplayComponents;

        class procedure CreateInstance();
        class procedure DestroyInstance;
        class property Instance: TDisplayComponentManager read uInstance;
    end;


implementation


uses
    SysUtils,
    DisplayComponentSettingsManager,
    DisplayComponentsDataAdaptor,
    DisplayComponentTypeDictionary,
    LogManager;

{ TDisplayComponentList }

constructor TDisplayComponentList.Create;
begin
    inherited Create;
    fList := TList<IDisplayComponent>.Create;
end;

destructor TDisplayComponentList.Destroy;
begin
    fList.Free;
    inherited Destroy;
end;

function TDisplayComponentList.GetCount: integer;
begin
    result := FList.Count;
end;

function TDisplayComponentList.GetDisplayComponent(aIndex: integer): IDisplayComponent;
begin
    result := fList[aIndex];
end;

procedure TDisplayComponentList.SetDisplayComponent(aIndex: integer; const aValue: IDisplayComponent);
begin
    fList[aIndex] := aValue;
end;

function TDisplayComponentList.Add(aDisplayComponent: IDisplayComponent): Integer;
begin
    result := FList.Add(aDisplayComponent);
end;

function TDisplayComponentList.IndexOf(const aDisplayComponentName: string): integer;
var
    x: integer;
begin
    result := -1;
    for x := 0 to fList.Count - 1 do
    begin
        if SameText(self[x].Name, aDisplayComponentName) then
        begin
            result := x;
            EXIT;
        end;
    end;
end;

procedure TDisplayComponentList.Clear;
begin
    fList.Clear;
end;

function TDisplayComponentList.FindDisplayComponentByName(aMustFind: boolean;
    const aDisplayComponentName: string; aExpectedDisplayComponentID: TDisplayComponentID; out oIntf)
    : boolean;
var
    xIndex: integer;
begin
    result := false;
    xIndex := self.IndexOf(aDisplayComponentName);
    if xIndex < 0 then
    begin
        if aMustFind then
            raise Exception.CreateFmt('DisplayComponent %s was not found', [aDisplayComponentName]);
        EXIT;
    end;

    result := Supports(fList[xIndex], aExpectedDisplayComponentID, oIntf);
    if not result then
    begin
        if aMustFind then
            raise Exception.CreateFmt('DisplayComponent %s was found but, had incorrect interface',
                [aDisplayComponentName]);
        EXIT;
    end;
end;

function TDisplayComponentList.FindNextDisplayComponent(aExpectedDisplayComponentID: TDisplayComponentID;
    var vSearchIndex: integer; out oIntf): boolean;
var
    x: integer;
begin
    result := false;
    for x := vSearchIndex to fList.Count - 1 do
    begin
        if Supports(fList[x], aExpectedDisplayComponentID, oIntf) then
        begin
            result := true;
            // Note: we return x + 1 because we want the next call to FindDisplayComponentExt to start searching at the next index
            vSearchIndex := x + 1;
            EXIT;
        end;
    end;
end;

function TDisplayComponentList.Remove(aDisplayComponent: IDisplayComponent): integer;
begin
    result := fList.Remove(aDisplayComponent);
end;

{ TDisplayComponentManager }

class procedure TDisplayComponentManager.CreateInstance();
begin
    if Assigned(uInstance) then
        EXIT;
    uInstance := TDisplayComponentManager.Create();
end;

class procedure TDisplayComponentManager.DestroyInstance();
begin
    uInstance.Free;
end;

constructor TDisplayComponentManager.Create();
begin
    inherited Create;
    fDisplayComponents := TDisplayComponentList.Create();
end;

destructor TDisplayComponentManager.Destroy;
begin
    fDisplayComponents.Free;
    inherited;
end;

class function TDisplayComponentManager.GetDisplayComponentType(const aDisplayComponentTypeName: string)
    : TDisplayComponentTypeInfo;
begin
    result := TRunDisplayComponentTypeDictionary.Instance.GetTypeFromTypeName(aDisplayComponentTypeName)
        as TDisplayComponentTypeInfo;
end;

function TDisplayComponentManager.CreateDisplayComponentByTypeName(const aDisplayComponentName: string;
    const aDisplayComponentTypeName: string): IDisplayComponent;
var
    xDisplayComponentType: TDisplayComponentTypeInfo;
    xDisplayComponentSettingsType: TDisplayComponentSettingsTypeInfo;
    xSettings: TDisplayComponentSettingList;
begin
    result := nil;
    xDisplayComponentType := GetDisplayComponentType(aDisplayComponentTypeName);
    if not Assigned(xDisplayComponentType) then
        EXIT;

    xDisplayComponentSettingsType := TDisplayComponentSettingsManager.GetDisplayComponentSettingsTypeInfo
        (aDisplayComponentTypeName);
    xSettings := xDisplayComponentSettingsType.CreateDisplayComponentSettings(aDisplayComponentName);
    result := xDisplayComponentType.CreateDisplayComponent(aDisplayComponentName, xSettings);
    fDisplayComponents.Add(result);
end;

function TDisplayComponentManager.CreateDisplayComponent(aErrors: TList<string>;
    const aDisplayComponentName: string): IDisplayComponent;
var
    xTypeName: string;
begin
    try
        xTypeName := TDisplayComponentsDataAdaptor.ReadDisplayComponentType(aDisplayComponentName);
        result := CreateDisplayComponentByTypeName(aDisplayComponentName, xTypeName);
    except
        on e: exception do
        begin
            LogAndAddError(aErrors, 'Failed loading DisplayComponent:' + aDisplayComponentName);
        end;
    end;
end;

procedure TDisplayComponentManager.ClearDisplayComponents;
var
    x: integer;
begin
    for x := 0 to fDisplayComponents.Count - 1 do
    begin
        fDisplayComponents[x] := nil;
    end;
    fDisplayComponents.Clear;
end;

function TDisplayComponentManager.FindDisplayComponent(aMustFind: boolean;
    const aDisplayComponentName: string; aExpectedDisplayComponentID: TDisplayComponentID; out oIntf)
    : boolean;
begin
    result := fDisplayComponents.FindDisplayComponentByName(aMustFind, aDisplayComponentName,
        aExpectedDisplayComponentID, oIntf)
end;

function TDisplayComponentManager.FindDisplayComponentExt(aExpectedDisplayComponentID: TDisplayComponentID;
    var vSearchIndex: integer; out oIntf): boolean;
begin
    result := fDisplayComponents.FindNextDisplayComponent(aExpectedDisplayComponentID, vSearchIndex, oIntf);
end;

function TDisplayComponentManager.FindFirst(aExpectedDisplayComponentID: TDisplayComponentID;
    out oIntf): boolean;
var
    xSearchIndex: integer;
begin
    xSearchIndex := 0;
    // vereinfachte Form von FindDisplayComponentExt
    result := fDisplayComponents.FindNextDisplayComponent(aExpectedDisplayComponentID, xSearchIndex, oIntf);
end;

class procedure TDisplayComponentManager.LogAndAddError(aErrors: TList<string>; const aCurrentError: string);
begin
    gLogManager.Log(aCurrentError, false);
    aErrors.Add(aCurrentError);
end;

function TDisplayComponentManager.DisplayComponentTypeExists(aExpectedDisplayComponentID
    : TDisplayComponentID): boolean;
var
    xIntf: IDisplayComponent;
begin
    result := self.FindFirst(aExpectedDisplayComponentID, xIntf);
end;

function TDisplayComponentManager.FindDisplayComponentByDisplayIDExt(aExpectedDisplayComponentID
    : TDisplayComponentID; const aDisplayID: string; var vSearchIndex: integer; out oIntf): boolean;

begin
    result := false;
    while true do
    begin
        if not self.FindDisplayComponentExt(aExpectedDisplayComponentID, vSearchIndex, oIntf) then
            EXIT;
        if SameText(IDisplayComponent(oIntf).DisplayID, aDisplayID) then
            BREAK;
    end;
    result := true;
end;

function TDisplayComponentManager.DoLoadComponentToWindow(const aComponentName: string;
    const aContextID: string; const aIsDesignMode: boolean; const aDisplayHandle: TDisplayHandle;
    aErrors: TList<string>): IDisplayComponent;
var
    xDisplayComponent, xChildDisplayComponent: IDisplayComponent;
    x: integer;
    xChildComponentNames: TArray<string>;
begin
    try
        xDisplayComponent := CreateDisplayComponent(aErrors, aComponentName);
        result := xDisplayComponent;
        xDisplayComponent.SetComponentMode(aIsDesignMode);
        xDisplayComponent.ContextID := aContextID;
        xDisplayComponent.Load();
        xDisplayComponent.SetParentDisplayHandle(aDisplayHandle);
        xDisplayComponent.Visible := true;

        xChildComponentNames := xDisplayComponent.ChildComponentNames;
        for x := 0 to high(xChildComponentNames) do
        begin
            xChildDisplayComponent := DoLoadComponentToWindow(xChildComponentNames[x], aContextID,
                aIsDesignMode, xDisplayComponent.DisplayHandle, aErrors);
            xDisplayComponent.AddChildComponent(xChildDisplayComponent);
        end;
    except
        on E: Exception do
        begin
            raise Exception.CreateFmt('%s - %s', [aComponentName, E.Message]);
        end;
    end;
end;

function TDisplayComponentManager.LoadComponentToWindow(const aComponentName: string;
    const aContextID: string; const aIsDesignMode: boolean; const aDisplayHandle: TDisplayHandle;
    aErrors: TList<string>): IDisplayComponent;
begin
    result := DoLoadComponentToWindow(aComponentName, aContextID, aIsDesignMode, aDisplayHandle, aErrors);
end;


end.
