{ -----------------------------------------------------------------------------------------------------------------------
  Copyright © 2009 Zinsser Analytic GmbH - All rights reserved.
  Author       : Payman Kamali (pk)
  Description  :
  -----------------------------------------------------------------------------------------------------------------------
  Revision History:
  date     op  method                       track-no improvement/change
  -------- --  ---------------------------  -------- --------------------------------------------------------------------
  06.04.09 pk                                TN4503  Initial Revision
  24.07.09 pk  AddCustomSettings             TN4675  renamed from AddSettings
  31.07.09 ts  DoClickEvent                  TN4700  wenn kein ClickEventName eingetragen ist, soll nichts passieren
  27.08.09 pk                                TN4753  Design and Run TypInfos seperated
  04.09.10 pk  TButtonDisplayComponent       TN5042  now implements IButtonDisplayComponent
  29.11.12 wl                                TN6015.2 DesignDisplayComponents abgeschafft (Es gibt nur noch RunDisplayComponents)
  ----------------------------------------------------------------------------------------------------------------------- }

unit ButtonDisplayComponent;


interface


uses
    Buttons,
    DisplayComponentIntf,
    DisplayComponentSettings,
    DisplayComponentTypeInfo,
    BasicDisplayComponent;

type
    TButtonDisplayComponentSettingList = class(TBasicDisplayComponentSettingList)
    private
        fCaption: string;
        fPictureFileName: string;
        fClickEventName: string;
    protected
        procedure AddCustomSettings(); override;
        procedure LoadSettings(); override;
        function GetShowClickEventNameSetting(): boolean; virtual;
    public
        property Caption: string read fCaption;
        property ClickEventName: string read fClickEventName;
        property PictureFileName: string read fPictureFileName;
        property ShowClickEventNameSetting: boolean read GetShowClickEventNameSetting;
    end;

    TButtonDisplayComponent = class(TBasicDisplayComponent, IButtonDisplayComponent)
    private
        function GetSettings(): TButtonDisplayComponentSettingList;
    protected
        fCaption: string;
        fPictureFileName: string;
        fClickEventName: string;
        function DoCreateDisplayControl: TDisplayControl; override;
        procedure DoInitControlProperties(); override;
        function GetControl: TSpeedButton;
        property Control: TSpeedButton read GetControl;
        procedure DoLoadSettings(); override;
        procedure DoClickEvent(aSender: TObject); virtual;
    public
        constructor Create(const aName: string); override;
        destructor Destroy(); override;
        property Settings: TButtonDisplayComponentSettingList read GetSettings;
    end;

    TRunButtonDisplayComponentTypeInfo = class(TRunDisplayComponentTypeInfo)
    public
        constructor Create(const aLibName, aLibVersion: string);
    end;

    TButtonDisplayComponentSettingsTypeInfo = class(TDisplayComponentSettingsTypeInfo)
    public
        constructor Create(const aLibName, aLibVersion: string);
    end;


implementation


const
    cTypeVersionButton = '1.0.0';
    cTypeNameButton = 'Button';

    { TButtonDisplayComponent }

constructor TButtonDisplayComponent.Create(const aName: string);
begin
    inherited;
    fCanHaveChildren := false;
end;

destructor TButtonDisplayComponent.Destroy;
begin
    inherited;
end;

function TButtonDisplayComponent.GetSettings: TButtonDisplayComponentSettingList;
begin
    result := fSettings as TButtonDisplayComponentSettingList;
end;

procedure TButtonDisplayComponent.DoLoadSettings;
begin
    inherited;
    fCaption := self.Settings.Caption;
    fPictureFileName := self.Settings.PictureFileName;
    fClickEventName := self.Settings.ClickEventName;
end;

procedure TButtonDisplayComponent.DoClickEvent(aSender: TObject);
begin
    if fClickEventName = '' then
        EXIT;
    self.ExecuteSimpleEvent(fClickEventName);
end;

function TButtonDisplayComponent.DoCreateDisplayControl: TDisplayControl;
begin
    result := TSpeedButton.Create(nil);
end;

procedure TButtonDisplayComponent.DoInitControlProperties;
begin
    inherited;
    self.Control.Caption := fCaption;
    if fPictureFileName <> '' then
        self.Control.Glyph.LoadFromFile(fPictureFileName);

    if self.Settings.ShowClickEventNameSetting then
        self.Control.OnClick := DoClickEvent;
end;

function TButtonDisplayComponent.GetControl: TSpeedButton;
begin
    result := fControl as TSpeedButton;
end;

{ TButtonDisplayComponentSettingList }

procedure TButtonDisplayComponentSettingList.AddCustomSettings;
begin
    inherited;
    self.AddStr('Caption', '');
    self.AddStr('PictureFileName', '');
    if self.ShowClickEventNameSetting then
        self.AddEvent('ClickEvent', '');
end;

function TButtonDisplayComponentSettingList.GetShowClickEventNameSetting: boolean;
begin
    result := true;
end;

procedure TButtonDisplayComponentSettingList.LoadSettings;
begin
    inherited;
    fCaption := self.Find('Caption').AsStr;
    fPictureFileName := self.Find('PictureFileName').AsStr;
    fClickEventName := '';
    if self.ShowClickEventNameSetting then
        fClickEventName := self.Find('ClickEvent').AsStr;
end;

{ TRunButtonDisplayComponentTypeInfo }

constructor TRunButtonDisplayComponentTypeInfo.Create(const aLibName, aLibVersion: string);
begin
    inherited Create(cTypeNameButton, cTypeVersionButton, aLibName, aLibVersion, TButtonDisplayComponent);
end;

{ TButtonDisplayComponentSettingsTypeInfo }

constructor TButtonDisplayComponentSettingsTypeInfo.Create(const aLibName, aLibVersion: string);
begin
    inherited Create(cTypeNameButton, cTypeVersionButton, aLibName, aLibVersion, TButtonDisplayComponent,
        TButtonDisplayComponentSettingList);
end;


end.
