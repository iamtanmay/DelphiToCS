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

unit TabHostDisplayComponent;


interface


uses
    ComCtrls,
    DisplayComponentIntf,
    DisplayComponentSettings,
    DisplayComponentTypeInfo,
    BasicDisplayComponent;

type
    TTabHostDisplayComponentSettingList = class(TBasicDisplayComponentSettingList)
    protected
        procedure AddCustomSettings(); override;
        procedure LoadSettings(); override;
        function GetCanHaveChildren(): boolean; override;
    end;

    TTabHostDisplayComponent = class(TBasicDisplayComponent, IDisplayComponent)
    private
        function GetSettings(): TTabHostDisplayComponentSettingList;
    protected
        function DoCreateDisplayControl: TDisplayControl; override;
        procedure DoInitControlProperties(); override;
        function GetControl: TPageControl;
        procedure DoLoadSettings(); override;
        property Control: TPageControl read GetControl;
    public
        constructor Create(const aName: string); override;
        destructor Destroy(); override;
        property Settings: TTabHostDisplayComponentSettingList read GetSettings;
    end;

    TRunTabHostDisplayComponentTypeInfo = class(TRunDisplayComponentTypeInfo)
    public
        constructor Create(const aLibName, aLibVersion: string);
    end;

    TTabHostDisplayComponentSettingsTypeInfo = class(TDisplayComponentSettingsTypeInfo)
    public
        constructor Create(const aLibName, aLibVersion: string);
    end;


implementation


const
    cTypeVersionTabHost = '1.0.0';
    cTypeNameTabHost = 'TabHost';

    { TTabHostDisplayComponent }

constructor TTabHostDisplayComponent.Create(const aName: string);
begin
    inherited;
end;

destructor TTabHostDisplayComponent.Destroy;
begin
    inherited;
end;

function TTabHostDisplayComponent.GetSettings: TTabHostDisplayComponentSettingList;
begin
    result := fSettings as TTabHostDisplayComponentSettingList;
end;

procedure TTabHostDisplayComponent.DoLoadSettings;
begin
    inherited;
end;

function TTabHostDisplayComponent.DoCreateDisplayControl: TDisplayControl;
begin
    result := TPageControl.Create(nil);
end;

procedure TTabHostDisplayComponent.DoInitControlProperties;
begin
    inherited;
end;

function TTabHostDisplayComponent.GetControl: TPageControl;
begin
    result := fControl as TPageControl;
end;

{ TTabHostDisplayComponentSettingList }

procedure TTabHostDisplayComponentSettingList.AddCustomSettings;
begin
    inherited;
    // self.AddStr( 'Caption', '' );
end;

function TTabHostDisplayComponentSettingList.GetCanHaveChildren: boolean;
begin
    result := true;
end;

procedure TTabHostDisplayComponentSettingList.LoadSettings;
begin
    inherited;
end;

{ TRunTabHostDisplayComponentTypeInfo }

constructor TRunTabHostDisplayComponentTypeInfo.Create(const aLibName, aLibVersion: string);
begin
    inherited Create(cTypeNameTabHost, cTypeVersionTabHost, aLibName, aLibVersion, TTabHostDisplayComponent);
end;

{ TTabHostDisplayComponentSettingsTypeInfo }

constructor TTabHostDisplayComponentSettingsTypeInfo.Create(const aLibName, aLibVersion: string);
begin
    inherited Create(cTypeNameTabHost, cTypeVersionTabHost, aLibName, aLibVersion, TTabHostDisplayComponent,
        TTabHostDisplayComponentSettingList);
end;


end.
