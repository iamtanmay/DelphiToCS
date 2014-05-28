{ --------------------------------------------------------------------------------------------------
  Copyright © 2008 Zinsser Analytic GmbH - All rights reserved.
  Author       : Payman Kamali (pk)
  Description  :
  --------------------------------------------------------------------------------------------------
  Revision History:
  date     op  method                       track-no improvement/change
  -------- --  ---------------------------  -------- -----------------------------------------------
  23.06.08 pk                                TN4139  Initial Revision
  09.07.08 pk  UnLoadFromMemory              TN4139  calls scenechanged
  16.07.08 pk  LoadCurrent                   TN4139  no longer loads runlayout
  30.07.08 pk  RegisterCurrent               TN4139  new
  06.08.08 pk  ForcedLoad                    TN4139  new
  27.07.09 pk  GetSceneGraphicsForLayout     TN4604  New
  26.08.09 pk  uLayoutManagerInstance        TN4753  New
  02.09.09 pk                                TN4753  changes needed for better cleanup
  28.09.09 pk                                TN4753  references to fSceneGraphics removed
  04.11.09 pk                                TN4843  Replace TObjectList, TStringList, TStrings with Generics Lists and TStringArray
  04.08.10 pk  GetTempSettingsSectionName    TN5043  New: a different settings section name for each layoutmanager
  04.08.10 pk  DoDestroySceneGraphcis        TN5043  New
  23.04.10 wl                                TN5070  neue Funktionen aus TLayoutManager überschrieben
  30.04.10 wl  EvaluationStart               TN5070  Rack als neuer Parameter
  07.06.10 wl                                TN5116   Layout editieren vorbereiten
  26.07.11 wl                                TN5614   Zugriff auf ViewReagentsOfLayout geändert
  22.06.12 wl                                TN5924   ViewReagentsOfLayout ist nicht mehr sichtbar
  13.03.13 wl                                TN5960   uses geändert
  09.04.13 wl  GetSceneGraphicsForLayout     TN6095   jetzt public
  11.04.13 wl                                TN6045   verwendet TObjectDictionary
  -------------------------------------------------------------------------------------------------- }

unit ZADesignLayoutManager;


interface


uses
    Generics.Collections,
    ExtCtrls,
    Controls,
    Layout,
    CustomLayoutManager,
    SceneGraphics,
    Carrier,
    LayoutDisplay,
    Rack,
    LayoutEditingFunctionProvider,
    EditingLayoutElements;

type
    TLayoutList = class
    private
        fList: TDictionary<string, TLayout>;
        function MakeListKey(const aLayoutName: string; const aMethodName: string): string;
        function GetCount: integer;
    public
        constructor Create();
        destructor Destroy(); override;
        function FindLayout(const aLayoutName: string; const aMethodName: string): TLayout;
        procedure RemoveLayout(const aLayoutName: string; const aMethodName: string);
        procedure Add(aLayout: TLayout; const aMethodName: string);
        property Count: integer read GetCount;
        function GetAllLayoutNames: TArray<string>;
    end;

    TZADesignLayoutManager = class sealed(TCustomLayoutManager)
    private
        fLayouts: TLayoutList;
        fSceneGraphicsList: TDictionary<string, TSceneGraphics>;
        fLayoutDisplay: ILayoutDisplay;
        class var uInstance: TZADesignLayoutManager;
    protected
        function GetTempSettingsSectionName: string; override;
        function DoCreateLayout(const aRunName, aLayoutName: string): TLayout; override;
        procedure SetCaption(const aRunName, aLayoutName: string); override;
        procedure ClearCurrent(); override;
        function LoadCurrent(): boolean; override;
        function DoRegisterLayout(const aRunName, aLayoutName: string): TLayout; override;
        procedure DoDestroyLayout(const aLayout: TLayout); override;
        function AddSceneForLayout(const aLayoutName: string): TSceneGraphics;
        procedure RemoveSceneForLayout(const aLayoutName: string);
    public
        constructor Create(aBackgroundGraphicsParent: TWincontrol);
        destructor Destroy(); override;

        function GetSceneGraphicsForLayout(const aLayout: TLayout): TSceneGraphics; override;
        function ForcedLoad(): boolean;
        procedure ChangeLayoutMode(aDisplay: ILayoutDisplay);
        function UnLoadFromMemory(const aLayoutName: string): boolean;
        procedure UnLoadAllFromMemory();

        class procedure CreateInstance();
        class procedure DestroyInstance();
        class property Instance: TZADesignLayoutManager read uInstance;
    end;


implementation


uses
    SysUtils,
    cxTL,
    ZADesignLayout,
    LogManager,
    MethodTypes;

{ TLayoutList }

constructor TLayoutList.Create();
begin
    inherited Create();
    fList := TDictionary<string, TLayout>.Create();
end;

destructor TLayoutList.Destroy;
var
    xPair: TPair<string, TLayout>;
begin
    for xPair in fList do
        xPair.Value.Free;

    fList.Free;
    inherited;
end;

function TLayoutList.GetAllLayoutNames: TArray<string>;
var
    xPair: TPair<string, TLayout>;
    xList: TList<string>;
begin
    xList := TList<string>.Create;
    try
        for xPair in fList do
            xList.Add(xPair.Value.Name);

        EXIT(xList.ToArray);
    finally
        FreeAndNil(xList);
    end;
end;

function TLayoutList.GetCount: integer;
begin
    result := fList.Count;
end;

function TLayoutList.MakeListKey(const aLayoutName: string; const aMethodName: string): string;
var
    xMethodName: string;
begin
    ASSERT(aLayoutName <> '', 'Layout Name cannot be empty');
    if aMethodName = '' then
        xMethodName := '~NORUN'
    else
        xMethodName := aMethodName;

    result := Format('%s_%s', [aLayoutName, xMethodName]);
end;

function TLayoutList.FindLayout(const aLayoutName: string; const aMethodName: string): TLayout;
var
    xListKey: string;
begin
    xListKey := MakeListKey(aLayoutName, aMethodName);
    if fList.ContainsKey(xListKey) then
        EXIT(fList[xListKey])
    else
        EXIT(nil);
end;

procedure TLayoutList.Add(aLayout: TLayout; const aMethodName: string);
var
    xListKey: string;
begin
    xListKey := MakeListKey(aLayout.LayoutName, aMethodName);
    fList.Add(xListKey, aLayout);
end;

procedure TLayoutList.RemoveLayout(const aLayoutName: string; const aMethodName: string);
var
    xListKey: string;
begin
    xListKey := MakeListKey(aLayoutName, aMethodName);
    if fList.ContainsKey(xListKey) then
        fList.Remove(xListKey);
end;

{ TZADesignLayoutManager }

class procedure TZADesignLayoutManager.CreateInstance();
begin
    uInstance := TZADesignLayoutManager.Create(nil);
end;

class procedure TZADesignLayoutManager.DestroyInstance();
begin
    uInstance.Free;
end;

constructor TZADesignLayoutManager.Create(aBackgroundGraphicsParent: TWincontrol);
begin
    inherited Create(aBackgroundGraphicsParent);

    fLayouts := TLayoutList.Create();
    fSceneGraphicsList := TDictionary<string, TSceneGraphics>.Create();
end;

destructor TZADesignLayoutManager.Destroy();
begin
    fSceneGraphicsList.Free;
    fLayouts.Free;
    inherited;
end;

function TZADesignLayoutManager.GetTempSettingsSectionName: string;
begin
    result := 'Layout_Designer';
end;

function TZADesignLayoutManager.AddSceneForLayout(const aLayoutName: string): TSceneGraphics;
var
    xBackgroundGraphicsParent: TWincontrol;
begin
    fLayoutDisplay.PrepareAddLayout(aLayoutName, xBackgroundGraphicsParent);
    result := self.CreateSceneGraphics(xBackgroundGraphicsParent);
    fSceneGraphicsList.Add(aLayoutName, result);
end;

procedure TZADesignLayoutManager.RemoveSceneForLayout(const aLayoutName: string);
var
    xSceneGraphics: TSceneGraphics;
begin
    xSceneGraphics := fSceneGraphicsList[aLayoutName];
    DoDestroySceneGraphics(xSceneGraphics);
    fSceneGraphicsList.Remove(aLayoutName);
    fLayoutDisplay.FinalizeRemoveLayout(aLayoutName);
end;

function TZADesignLayoutManager.GetSceneGraphicsForLayout(const aLayout: TLayout): TSceneGraphics;
begin
    if fSceneGraphicsList.ContainsKey(aLayout.Name) then
        EXIT(fSceneGraphicsList[aLayout.Name]);

    EXIT(AddSceneForLayout(aLayout.Name));
end;

function TZADesignLayoutManager.DoCreateLayout(const aRunName, aLayoutName: string): TLayout;
begin
    result := TZADesignLayout.Create(aLayoutName, aRunName)
end;

function TZADesignLayoutManager.DoRegisterLayout(const aRunName, aLayoutName: string): TLayout;
var
    xSceneGraphics: TSceneGraphics;
begin
    result := fLayouts.FindLayout(aLayoutName, aRunName);

    if not Assigned(result) then
    begin
        result := CreateLayout(aRunName, aLayoutName);
        fLayouts.Add(result, aRunName);
    end;

    xSceneGraphics := GetSceneGraphicsForLayout(result);
    fLayoutDisplay.LayoutAdded(aLayoutName, xSceneGraphics);
end;

procedure TZADesignLayoutManager.DoDestroyLayout(const aLayout: TLayout);
begin
    fLayouts.RemoveLayout(aLayout.Name, '');
    inherited;
end;

function TZADesignLayoutManager.UnLoadFromMemory(const aLayoutName: string): boolean;
var
    xLayout: TLayout;

begin
    result := true;
    // if the method doesn't have a layout dont need unload
    if aLayoutName = '' then
        EXIT;

    xLayout := fLayouts.FindLayout(aLayoutName, '');
    ASSERT(Assigned(xLayout), 'Layout object not found in list');

    fLayoutDisplay.LayoutRemoved(aLayoutName);
    self.UnregisterLayout(xLayout);

    RemoveSceneForLayout(aLayoutName);
end;

procedure TZADesignLayoutManager.UnLoadAllFromMemory;
var
    xLayoutNames: TArray<string>;
    x: integer;
begin
    xLayoutNames := fLayouts.GetAllLayoutNames();
    for x := high(xLayoutNames) downto 0 do
        UnLoadFromMemory(xLayoutNames[x]);
end;

function TZADesignLayoutManager.LoadCurrent(): boolean;
begin
    result := true;
    if not Assigned(fCurrentLayout) then
        EXIT;

    if fCurrentLayout.IsLoaded then
    begin
        ShowLayout(fCurrentLayout);
        EXIT;
    end;
    result := self.DoLoad();
end;

procedure TZADesignLayoutManager.ChangeLayoutMode(aDisplay: ILayoutDisplay);
begin
    fLayoutDisplay := aDisplay;
end;

procedure TZADesignLayoutManager.ClearCurrent();
begin
    SetCaption('', '');
    HideLayout(fCurrentLayout);
    fCurrentLayout := nil;
end;

procedure TZADesignLayoutManager.SetCaption(const aRunName, aLayoutName: string);
begin
    fLayoutDisplay.SetCaption(aRunName, aLayoutName);
end;

function TZADesignLayoutManager.ForcedLoad: boolean;
begin
    result := DoUnloadLayout(fCurrentLayout);
    if not result then
        EXIT;
    result := DoLoad();
end;


end.
