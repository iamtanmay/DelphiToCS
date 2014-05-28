{ -----------------------------------------------------------------------------------------------------------------------
  Copyright © 2009 Zinsser Analytic GmbH - All rights reserved.
  Author       : Payman Kamali (pk)
  Description  :
  -----------------------------------------------------------------------------------------------------------------------
  Revision History:
  date     op  method                       track-no improvement/change
  -------- --  ---------------------------  -------- --------------------------------------------------------------------
  06.04.09 pk                                TN4503  Initial Revision
  27.08.09 pk                                TN4753  Design and Run TypInfos seperated
  29.11.12 wl                                TN6015.2 DesignDisplayComponents abgeschafft (Es gibt nur noch RunDisplayComponents)
  02.09.13 wl  TPanelDisplayComponent.DoCreateDisplayControl    TN6015.2   Darstellung flat
  ----------------------------------------------------------------------------------------------------------------------- }

unit PanelDisplayComponent;


interface


uses
    ExtCtrls,
    DisplayComponentIntf,
    DisplayComponentSettings,
    DisplayComponentTypeInfo,
    BasicDisplayComponent;

type
    TPanelDisplayComponentSettingList = class(TBasicDisplayComponentSettingList)
    protected
        function GetCanHaveChildren(): boolean; override;
    end;

    TPanelDisplayComponent = class(TBasicDisplayComponent, IDisplayComponent)
    private
        function GetSettings(): TPanelDisplayComponentSettingList;
    protected
        fCaption: string;
        function DoCreateDisplayControl: TDisplayControl; override;
        procedure DoInitControlProperties(); override;
        function GetControl: TPanel;
        property Control: TPanel read GetControl;
        procedure DoLoadSettings(); override;
    public
        constructor Create(const aName: string); override;
        destructor Destroy(); override;
        property Settings: TPanelDisplayComponentSettingList read GetSettings;
    end;

    TPanelDisplayComponentTypeInfoConst = record
    public const
        cTypeVersion = '1.0.0';
        cTypeName = 'Panel';
    end;

    TRunPanelDisplayComponentTypeInfo = class(TRunDisplayComponentTypeInfo)
    public
        constructor Create(const aLibName, aLibVersion: string);
    end;

    TPanelDisplayComponentSettingsTypeInfo = class(TDisplayComponentSettingsTypeInfo)
    public
        constructor Create(const aLibName, aLibVersion: string);
    end;


implementation


uses
    Controls;

{ TPanelDisplayComponent }

constructor TPanelDisplayComponent.Create(const aName: string);
begin
    inherited;
end;

destructor TPanelDisplayComponent.Destroy;
begin
    inherited;
end;

function TPanelDisplayComponent.GetSettings: TPanelDisplayComponentSettingList;
begin
    result := fSettings as TPanelDisplayComponentSettingList;
end;

procedure TPanelDisplayComponent.DoLoadSettings;
begin
    inherited;
    fCaption := self.Name;
end;

function TPanelDisplayComponent.DoCreateDisplayControl: TDisplayControl;
var
    xPanel: TPanel;
begin
    xPanel := TPanel.Create(nil);
    xPanel.BevelOuter := TBevelCut.bvNone;
    EXIT(xPanel);
end;

procedure TPanelDisplayComponent.DoInitControlProperties;
begin
    inherited;

    if self.IsDesignMode then
    begin
        self.Control.Caption := fCaption;
    end
    else
    begin
        self.Control.Caption := '';
    end;
end;

function TPanelDisplayComponent.GetControl: TPanel;
begin
    result := fControl as TPanel;
end;

{ TPanelDisplayComponentSettingList }

function TPanelDisplayComponentSettingList.GetCanHaveChildren: boolean;
begin
    result := true;
end;

{ TRunPanelDisplayComponentTypeInfo }

constructor TRunPanelDisplayComponentTypeInfo.Create(const aLibName, aLibVersion: string);
begin
    inherited Create(TPanelDisplayComponentTypeInfoConst.cTypeName,
        TPanelDisplayComponentTypeInfoConst.cTypeVersion, aLibName, aLibVersion, TPanelDisplayComponent);
end;

{ TPanelDisplayComponentSettingsTypeInfo }

constructor TPanelDisplayComponentSettingsTypeInfo.Create(const aLibName, aLibVersion: string);
begin
    inherited Create(TPanelDisplayComponentTypeInfoConst.cTypeName,
        TPanelDisplayComponentTypeInfoConst.cTypeVersion, aLibName, aLibVersion, TPanelDisplayComponent,
        TPanelDisplayComponentSettingList);
end;


end.
