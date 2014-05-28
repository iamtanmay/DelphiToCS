{ ------------------------------------------------------------------------------------------------------------
  Copyright © 2010 Zinsser Analytic GmbH - All rights reserved.
  Author       : Wolfgang Lyncke (wl)
  Description  :
  ------------------------------------------------------------------------------------------------------------
  Revision History:
  date     op  method                            track-no improvement/change
  -------- --  --------------------------------  -------- ----------------------------------------------------
  07.06.10 wl                                    TN5116   enthält alle Methoden von TLayoutManager (bis auf Instance)
  07.06.10 pk  SyncRegisterLayout                TN5077   New
  21.07.10 pk  SceneChanged                      TN5066   now also works in designer
  05.01.11 wl  WriteCameraDimensions             TN5411   benutzt immer USSettings: Punkt statt Komma
  13.03.12 wl                                    TN5798   aufgeräumt
  09.04.13 wl  GetSceneGraphicsForLayout         TN6095   jetzt public
  10.12.13 wl  gCarrierReverseY                  TN6326   entfernt
  ------------------------------------------------------------------------------------------------------------ }

unit CustomLayoutManager;


interface


uses
    Controls,
    Layout,
    ThreadUtils,
    SceneGraphics;

type
    TLayoutCameraDimensions = record
        Zoom: double;
        PanX: double;
        PanY: double;
    end;

    TLayoutManagerUnloadCurrentCallback = procedure(aSender: TObject; var vCancel: boolean) of object;

    TCustomLayoutManager = class
    private
        fCurrentCameraDimensions: TLayoutCameraDimensions;
        fOnUnloadCurrent: TLayoutManagerUnloadCurrentCallback;

        function GetCurrentLayout: TLayout;
        function GetIsCurrentLayoutEmpty: boolean;
        procedure ReadSettings();
        procedure ReadCameraDimensions();
        procedure WriteCameraDimensions();
    protected
        fDefaultSceneGraphics: TSceneGraphics;
        fCurrentLayout: TLayout;
        function VCL(const aArgs: TMessageArg): TMessageResult;
        function GetTempSettingsSectionName(): string; virtual;
        function DoCreateLayout(const aRunName, aLayoutName: string): TLayout; virtual;
        function DoRegisterLayout(const aRunName, aLayoutName: string): TLayout; virtual;
        function DoCreateSceneGraphics(const aBackgroundGraphicsParent: TWincontrol): TSceneGraphics; virtual;
        procedure DoDestroySceneGraphics(var vSceneGraphics: TSceneGraphics); virtual;
        procedure SetCaption(const aRunName, aLayoutName: string); virtual;
        function LoadCurrent(): boolean; virtual;
        function UnloadLayout(const aLayout: TLayout): boolean; virtual;
        procedure DoAfterLoaded(); virtual;
        procedure ShowLayout(const aLayout: TLayout);
        procedure HideLayout(const aLayout: TLayout);
        procedure ClearCurrent(); virtual;
        function CreateLayout(const aRunName, aLayoutName: string): TLayout;
        procedure DestroyLayout(const aLayout: TLayout);
        procedure DoDestroyLayout(const aLayout: TLayout); virtual;
        function DoLoad: boolean;
        function DoLoadRun: boolean;
        function DoUnloadLayout(const aLayout: TLayout): boolean;
        function UnregisterLayout(const aLayout: TLayout): boolean;
        function CreateSceneGraphics(const aBackgroundGraphicsParent: TWincontrol): TSceneGraphics;
    public
        constructor Create(aBackgroundGraphicsParent: TWincontrol);
        destructor Destroy(); override;

        function GetSceneGraphicsForLayout(const aLayout: TLayout): TSceneGraphics; virtual;
        function UnregisterCurrentLayout(): boolean;
        procedure CreateDefaultSceneGraphics(aBackgroundGraphicsParent: TWincontrol);
        procedure DestroyDefaultSceneGraphics();
        procedure RegisterLayout(const aRunName, aLayoutName: string);
        procedure SceneChanged();
        function Load(): boolean;
        function LoadRun(): boolean;
        function SyncLoad(): boolean;
        function SyncLoadRun(): boolean;
        function SyncLayoutLoad(aRunName, aLayoutName: string): boolean;
        procedure SyncRegisterLayout(const aRunName, aLayoutName: string);
        procedure SyncUnRegisterCurrentLayout();
        procedure StartMethodRun(const aMethodName: string); virtual;
        function BCEqualToOther(const aRackName, aRackID: string): string; virtual;

        procedure SelectBoundShape(const aBoundName: string); virtual;

        property CurrentLayout: TLayout read GetCurrentLayout;
        property IsCurrentLayoutEmpty: boolean read GetIsCurrentLayoutEmpty;

        property DefaultSceneGraphics: TSceneGraphics read fDefaultSceneGraphics;
        property OnUnloadCurrent: TLayoutManagerUnloadCurrentCallback read fOnUnloadCurrent
            write fOnUnloadCurrent;
    end;


implementation


uses
    SysUtils,
    Variants,
    SamGlobe,
    AppSettings,
    LogManager,
    LayoutDataAdaptorExt,
    GUIManager,
    CommonTypes,
    LayoutElementGraphicsDriverTypeManager,
    ConfigurationFile,
    GeneralTypes;

type
    TLayoutVCL = (vclLoad, vclLoadRun, vclRegister, vclRegisterAndLoadRun, vclUnRegisterCurrent);

    { TCustomLayoutManager }

function TCustomLayoutManager.GetTempSettingsSectionName(): string;
begin
    result := 'Layout';
end;

procedure TCustomLayoutManager.ReadCameraDimensions();
var
    xIniAccess: IConfigurationSet;
    xSectionName: string;
begin
    xIniAccess := TConfigurationFile.Create(TAppsettings.LocalIniFileName);
    xIniAccess.Open(true);
    try
        xSectionName := GetTempSettingsSectionName();
        fCurrentCameraDimensions.Zoom := xIniAccess.ReadFloat(xSectionName, 'Zoom', 1.0,
            TFormatUtils.GetSettingsEnglishUS);
        fCurrentCameraDimensions.PanX := xIniAccess.ReadFloat(xSectionName, 'PanX', 0,
            TFormatUtils.GetSettingsEnglishUS);
        fCurrentCameraDimensions.PanY := xIniAccess.ReadFloat(xSectionName, 'PanY', 0,
            TFormatUtils.GetSettingsEnglishUS);
    finally
        xIniAccess.Close;
    end;
end;

procedure TCustomLayoutManager.WriteCameraDimensions();
var
    xIniAccess: IConfigurationSet;
    xSectionName: string;
begin
    xIniAccess := TConfigurationFile.Create(TAppsettings.LocalIniFileName);
    xIniAccess.Open(false);
    try
        xSectionName := GetTempSettingsSectionName();
        xIniAccess.WriteFloat(xSectionName, 'Zoom', fCurrentCameraDimensions.Zoom,
            TFormatUtils.GetSettingsEnglishUS);
        xIniAccess.WriteFloat(xSectionName, 'PanX', fCurrentCameraDimensions.PanX,
            TFormatUtils.GetSettingsEnglishUS);
        xIniAccess.WriteFloat(xSectionName, 'PanY', fCurrentCameraDimensions.PanY,
            TFormatUtils.GetSettingsEnglishUS);
    finally
        xIniAccess.Close;
    end;
end;

function TCustomLayoutManager.DoCreateSceneGraphics(const aBackgroundGraphicsParent: TWincontrol)
    : TSceneGraphics;
begin
    result := TSceneGraphics.Create(aBackgroundGraphicsParent);
    result.Zoom := fCurrentCameraDimensions.Zoom;
    result.PanX := fCurrentCameraDimensions.PanX;
    result.PanY := fCurrentCameraDimensions.PanY;
end;

procedure TCustomLayoutManager.DoDestroySceneGraphics(var vSceneGraphics: TSceneGraphics);
begin
    if not Assigned(vSceneGraphics) then
        EXIT;

    fCurrentCameraDimensions.Zoom := vSceneGraphics.Zoom;
    fCurrentCameraDimensions.PanX := vSceneGraphics.PanX;
    fCurrentCameraDimensions.PanY := vSceneGraphics.PanY;
    FreeAndNil(vSceneGraphics);
end;

procedure TCustomLayoutManager.ReadSettings();
begin
    ReadCameraDimensions();
end;

constructor TCustomLayoutManager.Create(aBackgroundGraphicsParent: TWincontrol);
begin
    inherited Create();
    fDefaultSceneGraphics := nil;
    ReadSettings();

    if Assigned(aBackgroundGraphicsParent) then
    begin
        CreateDefaultSceneGraphics(aBackgroundGraphicsParent);
    end;

    fCurrentLayout := nil;
end;

destructor TCustomLayoutManager.Destroy();
begin
    // call unload just in case a layout is still loaded
    self.UnregisterCurrentLayout();
    DestroyDefaultSceneGraphics();
    WriteCameraDimensions();
    inherited;
end;

procedure TCustomLayoutManager.CreateDefaultSceneGraphics(aBackgroundGraphicsParent: TWincontrol);
begin
    fDefaultSceneGraphics := CreateSceneGraphics(aBackgroundGraphicsParent);
end;

procedure TCustomLayoutManager.DestroyDefaultSceneGraphics();
begin
    self.DoDestroySceneGraphics(fDefaultSceneGraphics);
end;

procedure TCustomLayoutManager.SelectBoundShape(const aBoundName: string);
begin
    // dummy
end;

function TCustomLayoutManager.VCL(const aArgs: TMessageArg): TMessageResult;
var
    xType: TLayoutVCL;
begin
    xType := aArgs[0];
    case xType of
        vclLoad:
            result := Load();
        vclLoadRun:
            result := LoadRun();
        vclRegister:
            begin
                result := true;
                self.RegisterLayout(aArgs[1], aArgs[2]);
            end;
        vclRegisterAndLoadRun:
            begin
                self.RegisterLayout(aArgs[1], aArgs[2]);
                result := LoadRun();
            end;
        vclUnRegisterCurrent:
            self.UnregisterCurrentLayout();
    end;
end;

procedure TCustomLayoutManager.SceneChanged();
var
    xSceneGraphics: TSceneGraphics;
begin
    if not Assigned(fCurrentLayout) then
        EXIT;
    xSceneGraphics := GetSceneGraphicsForLayout(fCurrentLayout);
    if not Assigned(xSceneGraphics) then
        EXIT;
    xSceneGraphics.SceneChanged;
end;

procedure TCustomLayoutManager.HideLayout(const aLayout: TLayout);
var
    xSceneGraphics: TSceneGraphics;
begin
    if not Assigned(aLayout) then
        EXIT;
    xSceneGraphics := GetSceneGraphicsForLayout(aLayout);
    if not Assigned(xSceneGraphics) then
        EXIT;
    aLayout.Hide;

    if aLayout = fCurrentLayout then
        SceneChanged();
end;

procedure TCustomLayoutManager.ShowLayout(const aLayout: TLayout);
var
    xSceneGraphics: TSceneGraphics;
begin
    if not Assigned(aLayout) then
        EXIT;
    xSceneGraphics := GetSceneGraphicsForLayout(aLayout);
    if not Assigned(xSceneGraphics) then
        EXIT;
    xSceneGraphics.Visible := true;
    aLayout.Show;

    if aLayout = fCurrentLayout then
        SceneChanged();
end;

function TCustomLayoutManager.GetSceneGraphicsForLayout(const aLayout: TLayout): TSceneGraphics;
begin
    result := fDefaultSceneGraphics;
end;

function TCustomLayoutManager.CreateLayout(const aRunName, aLayoutName: string): TLayout;
begin
    result := DoCreateLayout(aRunName, aLayoutName);
    result.InitGraphics();
    result.Graphics.Parent := GetSceneGraphicsForLayout(result);
end;

function TCustomLayoutManager.DoCreateLayout(const aRunName, aLayoutName: string): TLayout;
begin
    result := TLayout.Create(aLayoutName, aRunName);
end;

function TCustomLayoutManager.DoRegisterLayout(const aRunName, aLayoutName: string): TLayout;
begin
    result := CreateLayout(aRunName, aLayoutName);
end;

procedure TCustomLayoutManager.SetCaption(const aRunName, aLayoutName: string);
begin
end;

function TCustomLayoutManager.DoUnloadLayout(const aLayout: TLayout): boolean;
begin
    result := true;
    if not Assigned(aLayout) then
        EXIT;
    HideLayout(aLayout);
    aLayout.Unload();

    if aLayout = fCurrentLayout then
        self.SceneChanged();
end;

function TCustomLayoutManager.UnloadLayout(const aLayout: TLayout): boolean;
var
    xCancel: boolean;
begin
    result := true;
    if not Assigned(aLayout) then
        EXIT;

    if (aLayout = fCurrentLayout) and Assigned(fOnUnloadCurrent) then
    begin
        xCancel := false;
        fOnUnloadCurrent(self, xCancel);
        if xCancel then
            EXIT;
    end;

    result := DoUnloadLayout(aLayout);

end;

function TCustomLayoutManager.LoadCurrent(): boolean;
begin
    if not Assigned(fCurrentLayout) then
    begin
        result := true;
        EXIT;
    end;
    result := DoLoad();
end;

procedure TCustomLayoutManager.DoDestroyLayout(const aLayout: TLayout);
begin
    aLayout.Free;
end;

procedure TCustomLayoutManager.DestroyLayout(const aLayout: TLayout);
begin
    if aLayout = fCurrentLayout then
        fCurrentLayout := nil;

    aLayout.Graphics.Parent := nil;
    DoDestroyLayout(aLayout);
end;

function TCustomLayoutManager.UnregisterLayout(const aLayout: TLayout): boolean;
begin
    result := true;

    if not Assigned(aLayout) then
        EXIT;

    if aLayout = fCurrentLayout then
        SetCaption('', '');

    if not UnloadLayout(aLayout) then
        EXIT;

    DestroyLayout(aLayout);
end;

procedure TCustomLayoutManager.ClearCurrent();
begin
    UnregisterLayout(fCurrentLayout);
end;

procedure TCustomLayoutManager.RegisterLayout(const aRunName, aLayoutName: string);
begin
    gLogManager.Log('Create Layout: ' + aLayoutName, false);
    ClearCurrent();
    if aLayoutName = '' then
        EXIT;
    fCurrentLayout := DoRegisterLayout(aRunName, aLayoutName);
end;

function TCustomLayoutManager.DoLoad: boolean;
begin
    fCurrentLayout.Unload();
    gLogManager.Log('Load Layout: ' + fCurrentLayout.LayoutName, false);
    result := fCurrentLayout.Load();
    DoAfterLoaded();
    ShowLayout(fCurrentLayout);
end;

function TCustomLayoutManager.DoLoadRun: boolean;
begin
    // fCurrentLayout.Unload();
    result := fCurrentLayout.LoadRun();
    DoAfterLoaded();
    ShowLayout(fCurrentLayout);
end;

function TCustomLayoutManager.Load: boolean;
var
    xError: string;
begin
    result := false;

    try
        result := LoadCurrent();
    except
        on E: Exception do
        begin
            xError := Format('Layout %s could not be loaded. %s', [fCurrentLayout.LayoutName, E.Message]);
            gGUIManager.MessageBox(xError, '', 0);
        end;
    end;
end;

function TCustomLayoutManager.LoadRun: boolean;
begin
    gLogManager.Log('Load Run Layout: ' + fCurrentLayout.LayoutName, false);

    result := DoLoadRun();
end;

function TCustomLayoutManager.UnregisterCurrentLayout(): boolean;
begin
    result := self.UnregisterLayout(fCurrentLayout);
end;

function TCustomLayoutManager.SyncLoad(): boolean;
begin
    result := ThreadUtils.gmMessageAndWait(self.VCL, VarArrayOf([vclLoad]));
end;

function TCustomLayoutManager.SyncLoadRun(): boolean;
begin
    result := ThreadUtils.gmMessageAndWait(self.VCL, VarArrayOf([vclLoadRun]));
end;

function TCustomLayoutManager.SyncLayoutLoad(aRunName, aLayoutName: string): boolean;
begin
    result := ThreadUtils.gmMessageAndWait(self.VCL,
        VarArrayOf([vclRegisterAndLoadRun, aRunName, aLayoutName]));
end;

procedure TCustomLayoutManager.SyncRegisterLayout(const aRunName, aLayoutName: string);
begin
    ThreadUtils.gmMessageAndWait(self.VCL, VarArrayOf([vclRegister, aRunName, aLayoutName]));
end;

procedure TCustomLayoutManager.SyncUnRegisterCurrentLayout();
begin
    ThreadUtils.gmMessageAndWait(self.VCL, VarArrayOf([vclUnRegisterCurrent]));
end;

procedure TCustomLayoutManager.StartMethodRun(const aMethodName: string);
begin
    // Dummy
end;

function TCustomLayoutManager.BCEqualToOther(const aRackName, aRackID: string): string;
begin
    // Dummy
    result := '';
end;

procedure TCustomLayoutManager.DoAfterLoaded;
begin
    //
end;

function TCustomLayoutManager.GetCurrentLayout: TLayout;
begin
    if self.IsCurrentLayoutEmpty then
        raise Exception.Create('No layout');

    result := fCurrentLayout;
end;

function TCustomLayoutManager.GetIsCurrentLayoutEmpty: boolean;
begin
    result := not Assigned(fCurrentLayout);
end;

function TCustomLayoutManager.CreateSceneGraphics(const aBackgroundGraphicsParent: TWincontrol)
    : TSceneGraphics;
begin
    result := DoCreateSceneGraphics(aBackgroundGraphicsParent);
    result.Visible := true;
end;


end.
