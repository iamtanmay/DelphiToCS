{ -----------------------------------------------------------------------------------------------------------------------
  Copyright © 2009 Zinsser Analytic GmbH - All rights reserved.
  Author       : Payman Kamali (pk)
  Description  :
  -----------------------------------------------------------------------------------------------------------------------
  Revision History:
  date     op  method                       track-no improvement/change
  -------- --  ---------------------------  -------- --------------------------------------------------------------------
  06.04.09 pk                                TN4503  Initial Revision
  24.07.09 pk  AddSettings                   TN4675  removed
  27.08.09 pk                                TN4753  Design and Run TypInfos seperated
  29.11.12 wl                                TN6015.2 DesignDisplayComponents abgeschafft (Es gibt nur noch RunDisplayComponents)
  ----------------------------------------------------------------------------------------------------------------------- }

unit SplitterDisplayComponent;


interface


uses
    ExtCtrls,
    DisplayComponentIntf,
    DisplayComponentSettings,
    DisplayComponentTypeInfo,
    BasicDisplayComponent;

type
    TSplitterDisplayComponentSettingList = class(TBasicDisplayComponentSettingList)
    protected
        procedure LoadSettings(); override;
    end;

    TSplitterDisplayComponent = class(TBasicDisplayComponent, IDisplayComponent)
    private
        function GetSettings(): TSplitterDisplayComponentSettingList;
    protected
        function DoCreateDisplayControl: TDisplayControl; override;
        procedure DoInitControlProperties(); override;
        function GetControl: TSplitter;
        property Control: TSplitter read GetControl;
        procedure DoLoadSettings(); override;
    public
        constructor Create(const aName: string); override;
        destructor Destroy(); override;
        property Settings: TSplitterDisplayComponentSettingList read GetSettings;
    end;

    TRunSplitterDisplayComponentTypeInfo = class(TRunDisplayComponentTypeInfo)
    public
        constructor Create(const aLibName, aLibVersion: string);
    end;

    TSplitterDisplayComponentSettingsTypeInfo = class(TDisplayComponentSettingsTypeInfo)
    public
        constructor Create(const aLibName, aLibVersion: string);
    end;


implementation


const
    cTypeVersionSplitter = '1.0.0';
    cTypeNameSplitter = 'Splitter';

    { TSplitterDisplayComponent }

constructor TSplitterDisplayComponent.Create(const aName: string);
begin
    inherited;
end;

destructor TSplitterDisplayComponent.Destroy;
begin
    inherited;
end;

function TSplitterDisplayComponent.GetSettings: TSplitterDisplayComponentSettingList;
begin
    result := fSettings as TSplitterDisplayComponentSettingList;
end;

procedure TSplitterDisplayComponent.DoLoadSettings;
begin
    inherited;
end;

function TSplitterDisplayComponent.DoCreateDisplayControl: TDisplayControl;
begin
    result := TSplitter.Create(nil);
end;

procedure TSplitterDisplayComponent.DoInitControlProperties;
begin
    inherited;
end;

function TSplitterDisplayComponent.GetControl: TSplitter;
begin
    result := fControl as TSplitter;
end;

{ TSplitterDisplayComponentSettingList }

procedure TSplitterDisplayComponentSettingList.LoadSettings;
begin
    inherited;
end;

{ TRunSplitterDisplayComponentTypeInfo }

constructor TRunSplitterDisplayComponentTypeInfo.Create(const aLibName, aLibVersion: string);
begin
    inherited Create(cTypeNameSplitter, cTypeVersionSplitter, aLibName, aLibVersion,
        TSplitterDisplayComponent);
end;

{ TSplitterDisplayComponentSettingsTypeInfo }

constructor TSplitterDisplayComponentSettingsTypeInfo.Create(const aLibName, aLibVersion: string);
begin
    inherited Create(cTypeNameSplitter, cTypeVersionSplitter, aLibName, aLibVersion,
        TSplitterDisplayComponent, TSplitterDisplayComponentSettingList);
end;


end.
