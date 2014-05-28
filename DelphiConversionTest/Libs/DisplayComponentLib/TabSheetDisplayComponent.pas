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
  27.08.09 pk                                TN4753  Design and Run TypInfos seperated
  29.11.12 wl                                TN6015.2 DesignDisplayComponents abgeschafft (Es gibt nur noch RunDisplayComponents)
  ----------------------------------------------------------------------------------------------------------------------- }

unit TabSheetDisplayComponent;


interface


uses
    ComCtrls,
    DisplayComponentIntf,
    DisplayComponentSettings,
    DisplayComponentTypeInfo,
    BasicDisplayComponent;

type
    TTabSheetDisplayComponentSettingList = class(TBasicDisplayComponentSettingList)
    private
        fPageNumber: integer;
        fCaption: string;
    protected
        procedure AddCustomSettings(); override;
        procedure LoadSettings(); override;
        function GetCanHaveChildren(): boolean; override;
        function GetShowAlignSetting(): boolean; override;
        function GetShowSizeSettings(): boolean; override;
    public
        property PageNumber: integer read fPageNumber;
        property Caption: string read fCaption;
    end;

    TTabSheetDisplayComponent = class(TBasicDisplayComponent, IDisplayComponent)
    private
        fPageNumber: integer;
        fCaption: string;
        function GetSettings(): TTabSheetDisplayComponentSettingList;
    protected
        function DoCreateDisplayControl: TDisplayControl; override;
        procedure DoInitControlProperties(); override;
        function GetControl: TTabSheet;
        property Control: TTabSheet read GetControl;
        procedure DoLoadSettings(); override;
        function GetVisible: boolean; override;
        procedure SetVisible(const aValue: boolean); override;
        procedure SetParentDisplayHandle(const aValue: TDisplayHandle); override;
    public
        constructor Create(const aName: string); override;
        destructor Destroy(); override;
        property Settings: TTabSheetDisplayComponentSettingList read GetSettings;
    end;

    TRunTabSheetDisplayComponentTypeInfo = class(TRunDisplayComponentTypeInfo)
    public
        constructor Create(const aLibName, aLibVersion: string);
    end;

    TTabSheetDisplayComponentSettingsTypeInfo = class(TDisplayComponentSettingsTypeInfo)
    public
        constructor Create(const aLibName, aLibVersion: string);
    end;


implementation


uses
    Controls;

const
    cTypeVersionTabSheet = '1.0.0';
    cTypeNameTabSheet = 'TabSheet';

    { TTabSheetDisplayComponent }

constructor TTabSheetDisplayComponent.Create(const aName: string);
begin
    inherited;
end;

destructor TTabSheetDisplayComponent.Destroy;
begin
    inherited;
end;

function TTabSheetDisplayComponent.GetSettings: TTabSheetDisplayComponentSettingList;
begin
    result := fSettings as TTabSheetDisplayComponentSettingList;
end;

procedure TTabSheetDisplayComponent.DoLoadSettings;
begin
    inherited;
    fAlign := alClient;
    fPageNumber := self.Settings.PageNumber;
    fCaption := self.Settings.Caption;
end;

function TTabSheetDisplayComponent.DoCreateDisplayControl: TDisplayControl;
begin
    result := TTabSheet.Create(nil);
end;

procedure TTabSheetDisplayComponent.DoInitControlProperties;
begin
    inherited;
    self.Control.PageIndex := fPageNumber - 1;
    self.Control.Caption := fCaption;
end;

function TTabSheetDisplayComponent.GetControl: TTabSheet;
begin
    result := fControl as TTabSheet;
end;

function TTabSheetDisplayComponent.GetVisible: boolean;
begin
    result := self.Control.TabVisible;
end;

procedure TTabSheetDisplayComponent.SetVisible(const aValue: boolean);
begin

    self.Control.Visible := aValue;
    self.Control.TabVisible := aValue;

end;

procedure TTabSheetDisplayComponent.SetParentDisplayHandle(const aValue: TDisplayHandle);
var
    xPageControl: TPageControl;
begin
    inherited;
    xPageControl := nil;
    if Assigned(aValue) then
    begin
        ASSERT(aValue is TPageControl, 'Parent component is not of type TabHost');
        xPageControl := aValue as TPageControl;
        // if we dont set the activepageindex, the pagecontrol shows the wrong tab caption
        xPageControl.ActivePageIndex := self.Control.PageIndex;
    end;

    try
        self.Control.PageControl := xPageControl;
    except
    end;
end;

{ TTabSheetDisplayComponentSettingList }

procedure TTabSheetDisplayComponentSettingList.AddCustomSettings;
begin
    inherited;
    self.AddInt('PageNumber', 1);
    self.AddStr('Caption', '');

end;

procedure TTabSheetDisplayComponentSettingList.LoadSettings;
begin
    inherited;
    fPageNumber := self.Find('PageNumber').AsInt;
    if fPageNumber < 1 then
        fPageNumber := 1;
    fCaption := self.Find('Caption').AsStr;

end;

function TTabSheetDisplayComponentSettingList.GetCanHaveChildren: boolean;
begin
    result := true;
end;

function TTabSheetDisplayComponentSettingList.GetShowAlignSetting: boolean;
begin
    result := false;
end;

function TTabSheetDisplayComponentSettingList.GetShowSizeSettings: boolean;
begin
    result := false;
end;

{ TRunTabSheetDisplayComponentTypeInfo }

constructor TRunTabSheetDisplayComponentTypeInfo.Create(const aLibName, aLibVersion: string);
begin
    inherited Create(cTypeNameTabSheet, cTypeVersionTabSheet, aLibName, aLibVersion,
        TTabSheetDisplayComponent);
end;

{ TTabSheetDisplayComponentSettingsTypeInfo }

constructor TTabSheetDisplayComponentSettingsTypeInfo.Create(const aLibName, aLibVersion: string);
begin
    inherited Create(cTypeNameTabSheet, cTypeVersionTabSheet, aLibName, aLibVersion,
        TTabSheetDisplayComponent, TTabSheetDisplayComponentSettingList);
end;


end.
