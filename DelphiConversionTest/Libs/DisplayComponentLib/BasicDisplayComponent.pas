{ -----------------------------------------------------------------------------------------------------------------------
  Copyright © 2009 Zinsser Analytic GmbH - All rights reserved.
  Author       : Payman Kamali (pk)
  Description  :
  -----------------------------------------------------------------------------------------------------------------------
  Revision History:
  date     op  method                                track-no improvement/change
  -------- --  -----------------------------------   -------- --------------------------------------------------------------------
  06.04.09 pk                                        TN4503  Initial Revision
  24.07.09 pk  TBasicDisplayComponentSettingList     TN4675  New: DisplayID
  24.07.09 pk  AddCustomSettings                     TN4675  New
  13.11.09 ts  Load()                                TN4785  fControl can be nil
  04.12.09 pk  fDisplayID                            TN4919  removed, was already in TDisplayComponent
  15.01.10 pk  Unload                                TN4958  DoFinalizeControlProperties done before setting visible to false, and parent to nil
  04.09.10 pk                                        TN5042  New Enabled property
  13.11.12 wl                                        TN6015   überarbeitet für DisplayComponentsDataAdaptor
  29.11.12 wl                                        TN6015.2 DesignDisplayComponents abgeschafft (Es gibt nur noch RunDisplayComponents)
  10.04.13 wl                                        TN6045   uses Generics.Collections
  ----------------------------------------------------------------------------------------------------------------------- }

unit BasicDisplayComponent;


interface


uses
    Controls,
    Generics.Collections,
    DisplayComponent,
    DisplayComponentIntf,
    DisplayComponentSettings,
    DisplayComponentTypeInfo;

type
    TBasicDisplayComponentSettingList = class(TDisplayComponentSettingList)
    strict private
        fDisplayID: string;
        fHeight: integer;
        fWidth: integer;
    strict protected
        fAlign: string;
        fTop: integer;
        fLeft: integer;
        function GetCanHaveChildren(): boolean; override;
        function GetShowAlignSetting(): boolean; virtual;
        function GetShowSizeSettings(): boolean; virtual;
        function GetShowPosSettings(): boolean; virtual;
        function GetShowDisplayIDSetting(): boolean; virtual;
        procedure AddCustomSettings(); virtual;
        procedure AddSettings(); override;
        procedure LoadSettings(); override;
    public
        constructor Create(const aSection, aTypeName: string); override;
        property ShowAlignSetting: boolean read GetShowAlignSetting;
        property ShowSizeSettings: boolean read GetShowSizeSettings;
        property ShowPosSettings: boolean read GetShowPosSettings;
        property ShowDisplayIDSetting: boolean read GetShowDisplayIDSetting;
        property DisplayID: string read fDisplayID;
        property Align: string read fAlign;
        property Height: integer read fHeight;
        property Width: integer read fWidth;
        property Top: integer read fTop;
        property Left: integer read fLeft;
    end;

    TDisplayControl = TControl;

    TBasicDisplayComponent = class(TDisplayComponent, IDisplayComponent)
    strict private
        fHeight: integer;
        fWidth: integer;
        fTop: integer;
        fLeft: integer;
        fChildComponents: TList<IDisplayComponent>;
        fChildComponentsSetting: TDSChildComponents;
        fOwnsControl: boolean;
    strict protected
        fAlign: TAlign;
        fCanHaveChildren: boolean;
        fControl: TDisplayControl;
        function GetSettings(): TBasicDisplayComponentSettingList;
        procedure SetParentDisplayHandle(const aValue: TDisplayHandle); override;
        function GetDisplayHandle(): TDisplayHandle; override;
        function DoCreateDisplayControl: TDisplayControl; virtual;
        function GetChildComponentNames: TArray<string>; override;
        function GetVisible: boolean; override;
        procedure SetVisible(const aValue: boolean); override;
        function GetEnabled: boolean; override;
        procedure SetEnabled(const aValue: boolean); override;
        procedure DoLoadSettings(); virtual;
        procedure DoInitControlProperties(); virtual;
        procedure DoFinalizeControlProperties(); virtual;
        procedure ExecuteSimpleEvent(const aEventName: string);
        function GetCanHaveChildren(): boolean; virtual;
        function GetShowAlignSetting(): boolean; virtual;
        function GetShowSizeSettings(): boolean; virtual;
        function GetShowPosSettings(): boolean; virtual;
        function GetShowDisplayIDSetting(): boolean; virtual;
        property CanHaveChildren: boolean read GetCanHaveChildren;
        property ShowAlignSetting: boolean read GetShowAlignSetting;
        property ShowSizeSettings: boolean read GetShowSizeSettings;
        property ShowPosSettings: boolean read GetShowPosSettings;
        property ShowDisplayIDSetting: boolean read GetShowDisplayIDSetting;
        class function StrToAlign(const aValue: string): TAlign;
    public
        constructor Create(const aName: string); override;
        destructor Destroy(); override;
        procedure Load(); override;
        procedure UnLoad(); override;
        procedure AddChildComponent(const aValue: IDisplayComponent); override;
        property Settings: TBasicDisplayComponentSettingList read GetSettings;
    end;


implementation


uses
    SysUtils,
    DisplayComponentEventManager;

{ TBasicDisplayComponentSettingList }

constructor TBasicDisplayComponentSettingList.Create(const aSection, aTypeName: string);
begin
    inherited;
    fAlign := TDisplayComponentSetting.cSettingAlignNone;
    fHeight := 0;
    fWidth := 0;
    fTop := 0;
    fLeft := 0;
    fDisplayID := '';
end;

function TBasicDisplayComponentSettingList.GetCanHaveChildren: boolean;
begin
    result := false;
end;

procedure TBasicDisplayComponentSettingList.AddCustomSettings;
begin

end;

procedure TBasicDisplayComponentSettingList.AddSettings;
begin
    inherited;
    if self.ShowDisplayIDSetting then
        self.AddStr('DisplayID', '');

    if self.ShowAlignSetting then
        self.AddAlign('Align', '', TDisplayComponentSetting.cSettingAlignNone);

    if self.ShowSizeSettings then
    begin
        self.AddInt('Height', 10);
        self.AddInt('Width', 10);
    end;

    if self.ShowPosSettings then
    begin
        self.AddInt('Top', 0);
        self.AddInt('Left', 0);
    end;

    AddCustomSettings();

    if self.CanHaveChildren then
        self.Add(TDSChildComponents.Create());
end;

procedure TBasicDisplayComponentSettingList.LoadSettings;
begin
    inherited;

    if self.ShowDisplayIDSetting then
        fDisplayID := Find('DisplayID').AsStr;

    if self.ShowAlignSetting then
        fAlign := Find('Align').AsStr;

    if self.ShowSizeSettings then
    begin
        fHeight := Find('Height').AsInt;
        fWidth := Find('Width').AsInt;
    end;

    if self.ShowPosSettings then
    begin
        fTop := Find('Top').AsInt;
        fLeft := Find('Left').AsInt;
    end;
end;

function TBasicDisplayComponentSettingList.GetShowAlignSetting: boolean;
begin
    result := true;
end;

function TBasicDisplayComponentSettingList.GetShowPosSettings: boolean;
begin
    result := true;
end;

function TBasicDisplayComponentSettingList.GetShowSizeSettings: boolean;
begin
    result := true;
end;

function TBasicDisplayComponentSettingList.GetShowDisplayIDSetting: boolean;
begin
    result := false;
end;

{ TBasicDisplayComponent }

constructor TBasicDisplayComponent.Create(const aName: string);
begin
    inherited;
    fChildComponents := TList<IDisplayComponent>.Create();
    fOwnsControl := true;
    fControl := nil;

end;

destructor TBasicDisplayComponent.Destroy;
begin
    fChildComponents.Free;
    if fOwnsControl then
        fControl.Free;
    inherited;
end;

function TBasicDisplayComponent.DoCreateDisplayControl: TDisplayControl;
begin
    result := nil;
end;

function TBasicDisplayComponent.GetChildComponentNames: TArray<string>;
begin
    SetLength(result, 0);
    if self.CanHaveChildren then
        result := fChildComponentsSetting.GetAllChildNames();
end;

function TBasicDisplayComponent.GetDisplayHandle: TDisplayHandle;
begin
    ASSERT(fControl is TDisplayHandle, 'Control is not WinControl');
    result := fControl as TDisplayHandle;

end;

function TBasicDisplayComponent.GetSettings: TBasicDisplayComponentSettingList;
begin
    result := fSettings as TBasicDisplayComponentSettingList;
end;

procedure TBasicDisplayComponent.Load();
begin
    inherited;

    DoLoadSettings;

    if self.CanHaveChildren then
        fChildComponentsSetting := self.Settings.Find(TDisplayComponentSetting.cSettingNameChildComponents)
            as TDSChildComponents;

    fControl := DoCreateDisplayControl();
    if fControl = nil then
        EXIT;
    fControl.Name := self.Name;
    DoInitControlProperties();
end;

procedure TBasicDisplayComponent.Unload();
var
    x: integer;
begin
    for x := fChildComponents.Count - 1 downto 0 do
    begin
        try
            fChildComponents[x].UnLoad;
        except
            on E: Exception do
                raise Exception.CreateFmt('%s - %s', [fChildComponents[x].Name, E.Message]);
        end;
    end;
    DoFinalizeControlProperties();
    self.Visible := false;
    SetParentDisplayHandle(nil);
    inherited;
end;

procedure TBasicDisplayComponent.SetParentDisplayHandle(const aValue: TDisplayHandle);
begin
    if not Assigned(fControl) then
        EXIT;
    fControl.Parent := aValue;
end;

function TBasicDisplayComponent.GetVisible: boolean;
begin
    result := fControl.Visible;
end;

procedure TBasicDisplayComponent.SetVisible(const aValue: boolean);
begin
    if not Assigned(fControl) then
        EXIT;
    fControl.Visible := aValue;
end;

function TBasicDisplayComponent.GetEnabled: boolean;
begin
    result := false;
    if not Assigned(fControl) then
        EXIT;
    result := fControl.Enabled;
end;

procedure TBasicDisplayComponent.SetEnabled(const aValue: boolean);
begin
    if not Assigned(fControl) then
        EXIT;
    fControl.Enabled := aValue;
end;

procedure TBasicDisplayComponent.DoInitControlProperties();
begin
    inherited;
    fControl.Height := fHeight;
    fControl.Width := fWidth;
    fControl.Left := fLeft;
    fControl.Top := fTop;
    fControl.Align := fAlign;
end;

procedure TBasicDisplayComponent.AddChildComponent(const aValue: IDisplayComponent);
begin
    if self.CanHaveChildren then
        fChildComponents.Add(aValue);
end;

class function TBasicDisplayComponent.StrToAlign(const aValue: string): TAlign;
begin
    if SameText(aValue, TDisplayComponentSetting.cSettingAlignClient) then
        result := alClient
    else if SameText(aValue, TDisplayComponentSetting.cSettingAlignTop) then
        result := alTop
    else if SameText(aValue, TDisplayComponentSetting.cSettingAlignBottom) then
        result := alBottom
    else if SameText(aValue, TDisplayComponentSetting.cSettingAlignLeft) then
        result := alLeft
    else if SameText(aValue, TDisplayComponentSetting.cSettingAlignRight) then
        result := alRight
    else
        result := alNone;
end;

procedure TBasicDisplayComponent.DoLoadSettings;
begin
    fDisplayID := self.Settings.DisplayID;

    fAlign := StrToAlign(self.Settings.Align);

    fHeight := self.Settings.Height;
    fWidth := self.Settings.Width;

    fTop := self.Settings.Top;
    fLeft := self.Settings.Left;
end;

procedure TBasicDisplayComponent.ExecuteSimpleEvent(const aEventName: string);
begin
    if TDisplayComponentEventManager.Instance = nil then
        EXIT;
    if not Assigned(TDisplayComponentEventManager.Instance.OnSimpleEvent) then
        EXIT;
    TDisplayComponentEventManager.Instance.OnSimpleEvent(self, aEventName);
end;

procedure TBasicDisplayComponent.DoFinalizeControlProperties;
begin

end;

function TBasicDisplayComponent.GetCanHaveChildren: boolean;
begin
    result := self.Settings.CanHaveChildren;
end;

function TBasicDisplayComponent.GetShowAlignSetting: boolean;
begin
    result := self.Settings.ShowAlignSetting;
end;

function TBasicDisplayComponent.GetShowPosSettings: boolean;
begin
    result := self.Settings.ShowPosSettings;
end;

function TBasicDisplayComponent.GetShowSizeSettings: boolean;
begin
    result := self.Settings.ShowSizeSettings;
end;

function TBasicDisplayComponent.GetShowDisplayIDSetting: boolean;
begin
    result := self.Settings.ShowDisplayIDSetting;
end;


end.
