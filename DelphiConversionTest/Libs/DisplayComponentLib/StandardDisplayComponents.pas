{ -----------------------------------------------------------------------------------------------------------------------
  Copyright © 2009 Zinsser Analytic GmbH - All rights reserved.
  Author       : Payman Kamali (pk)
  Description  :
  -----------------------------------------------------------------------------------------------------------------------
  Revision History:
  date     op  method                            track-no improvement/change
  -------- --  ---------------------------       -------- --------------------------------------------------------------------
  06.04.09 pk                                    TN4503  Initial Revision
  07.04.09 pk  TMainPanelDisplayComponent        TN4503  implements IMainDisplayComponent
  24.07.09 pk  TRunInfoPanelDisplayComponent     TN4675  InsertInfo
  24.07.09 pk  TRunInfoPanelDisplayComponentSettingList  TN4676  New ColumnWidth settings
  31.07.09 ts  TRunInfoPanelDisplayComponent     TN4666  InfoGroupBehaviour instead of HideGroup
  27.08.09 pk                                    TN4753  Design and Run TypInfos seperated
  04.09.10 pk  TStopButtonDisplayComponent       TN5042  now implements IStopButtonDisplayComponent
  17.11.11 wl                                    TN5725   PositionInfo-Fenster entfernt
  13.11.12 wl                                        TN6015   überarbeitet für DisplayComponentsDataAdaptor
  29.11.12 wl                                        TN6015.2 DesignDisplayComponents abgeschafft (Es gibt nur noch RunDisplayComponents)
  28.02.13 wl  TRunInfoPanelDisplayComponent         TN6096   neu: DateColumnVisible
  ----------------------------------------------------------------------------------------------------------------------- }

unit StandardDisplayComponents;


interface


uses
    DisplayComponentIntf,
    DisplayComponent,
    DisplayComponentTypeInfo,
    PanelDisplayComponent,
    ButtonDisplayComponent,
    AppTypes;

type
    // ----------------------------------------------------------------------------------------------- Stop Button
    TStopButtonDisplayComponentSettingList = class(TButtonDisplayComponentSettingList)
    protected
        function GetShowClickEventNameSetting(): boolean; override;
    end;

    TStopButtonDisplayComponent = class(TButtonDisplayComponent, IStopButtonDisplayComponent)
    private
        function GetSettings(): TStopButtonDisplayComponentSettingList;
    public
        constructor Create(const aName: string); override;
        destructor Destroy(); override;
        property Settings: TStopButtonDisplayComponentSettingList read GetSettings;
    end;

    TStopButtonDisplayComponentSettingsTypeInfo = class(TDisplayComponentSettingsTypeInfo)
    public
        constructor Create(const aLibName, aLibVersion: string);
    end;

    // ----------------------------------------------------------------------------------------------- Main
    TMainPanelDisplayComponentSettingList = class(TPanelDisplayComponentSettingList)
    protected
        procedure LoadSettings(); override;
        function GetShowAlignSetting(): boolean; override;
        function GetShowSizeSettings(): boolean; override;
        function GetShowPosSettings(): boolean; override;
    end;

    TMainPanelDisplayComponent = class(TPanelDisplayComponent, IMainDisplayComponent)
    private
        function GetSettings(): TMainPanelDisplayComponentSettingList;
    public
        constructor Create(const aName: string); override;
        destructor Destroy(); override;
        property Settings: TMainPanelDisplayComponentSettingList read GetSettings;
    end;

    TMainPanelDisplayComponentSettingsTypeInfo = class(TDisplayComponentSettingsTypeInfo)
    public
        constructor Create(const aLibName, aLibVersion: string);
    end;

    // ----------------------------------------------------------------------------------------------- DelayInfo
    TDelayInfoPanelDisplayComponentSettingList = class(TPanelDisplayComponentSettingList)
    protected
        function GetCanHaveChildren(): boolean; override;
    end;

    TDelayInfoPanelDisplayComponent = class(TPanelDisplayComponent)
    private
        function GetSettings(): TDelayInfoPanelDisplayComponentSettingList;
    public
        constructor Create(const aName: string); override;
        destructor Destroy(); override;
        property Settings: TDelayInfoPanelDisplayComponentSettingList read GetSettings;
    end;

    TDelayInfoPanelDisplayComponentSettingsTypeInfo = class(TDisplayComponentSettingsTypeInfo)
    public
        constructor Create(const aLibName, aLibVersion: string);
    end;

    // ----------------------------------------------------------------------------------------------- LayoutInfo
    TLayoutInfoPanelDisplayComponentSettingList = class(TPanelDisplayComponentSettingList)
    protected
        function GetCanHaveChildren(): boolean; override;
    end;

    TLayoutInfoPanelDisplayComponent = class(TPanelDisplayComponent)
    private
        function GetSettings(): TLayoutInfoPanelDisplayComponentSettingList;
    public
        constructor Create(const aName: string); override;
        destructor Destroy(); override;
        property Settings: TLayoutInfoPanelDisplayComponentSettingList read GetSettings;
    end;

    TLayoutInfoPanelDisplayComponentSettingsTypeInfo = class(TDisplayComponentSettingsTypeInfo)
    public
        constructor Create(const aLibName, aLibVersion: string);
    end;
    // ----------------------------------------------------------------------------------------------- RunInfo

    TRunInfoPanelDisplayComponentSettingList = class(TPanelDisplayComponentSettingList)
    protected
        fIdentifierColumnWidth: integer;
        fEventColumnWidth: integer;
        fDateColumnWidth: integer;
        fDateColumnVisible: boolean;
        procedure AddCustomSettings(); override;
        procedure LoadSettings(); override;
        function GetCanHaveChildren(): boolean; override;
        function GetShowDisplayIDSetting(): boolean; override;
    public
        property IdentifierColumnWidth: integer read fIdentifierColumnWidth;
        property EventColumnWidth: integer read fEventColumnWidth;
        property DateColumnWidth: integer read fDateColumnWidth;
        property DateColumnVisible: boolean read fDateColumnVisible;
    end;

    TRunInfoPanelDisplayComponent = class(TPanelDisplayComponent, IRunInfoPanelDisplayComponent)
    private
        function GetSettings(): TRunInfoPanelDisplayComponentSettingList;
    public
        constructor Create(const aName: string); override;
        procedure InsertInfo(const aGroupNames: TArray<string>; const aKey, aText: string;
            aInfoGroupBehaviour: TInfoGroupBehaviour); virtual; abstract;
        destructor Destroy(); override;
        property Settings: TRunInfoPanelDisplayComponentSettingList read GetSettings;
    end;

    TRunInfoPanelDisplayComponentSettingsTypeInfo = class(TDisplayComponentSettingsTypeInfo)
    public
        constructor Create(const aLibName, aLibVersion: string);
    end;

    // ----------------------------------------------------------------------------------------------- LogInfo
    TLogInfoPanelDisplayComponentSettingList = class(TPanelDisplayComponentSettingList)
    protected
        function GetCanHaveChildren(): boolean; override;
    end;

    TLogInfoPanelDisplayComponent = class(TPanelDisplayComponent)
    private
        function GetSettings(): TLogInfoPanelDisplayComponentSettingList;
    public
        constructor Create(const aName: string); override;
        destructor Destroy(); override;
        property Settings: TLogInfoPanelDisplayComponentSettingList read GetSettings;
    end;

    TLogInfoPanelDisplayComponentSettingsTypeInfo = class(TDisplayComponentSettingsTypeInfo)
    public
        constructor Create(const aLibName, aLibVersion: string);
    end;

    // ----------------------------------------------------------------------------------------------- VolumesInfo
    TVolumesInfoPanelDisplayComponentSettingList = class(TPanelDisplayComponentSettingList)
    protected
        function GetCanHaveChildren(): boolean; override;
    end;

    TVolumesInfoPanelDisplayComponent = class(TPanelDisplayComponent)
    private
        function GetSettings(): TVolumesInfoPanelDisplayComponentSettingList;
    public
        constructor Create(const aName: string); override;
        destructor Destroy(); override;
        property Settings: TVolumesInfoPanelDisplayComponentSettingList read GetSettings;
    end;

    TVolumesInfoPanelDisplayComponentSettingsTypeInfo = class(TDisplayComponentSettingsTypeInfo)
    public
        constructor Create(const aLibName, aLibVersion: string);
    end;

    // ----------------------------------------------------------------------------------------------- SimulationInfo
    TSimulationInfoPanelDisplayComponentSettingList = class(TPanelDisplayComponentSettingList)
    protected
        function GetCanHaveChildren(): boolean; override;
    end;

    TSimulationInfoPanelDisplayComponent = class(TPanelDisplayComponent)
    private
        function GetSettings(): TSimulationInfoPanelDisplayComponentSettingList;
    public
        constructor Create(const aName: string); override;
        destructor Destroy(); override;
        property Settings: TSimulationInfoPanelDisplayComponentSettingList read GetSettings;
    end;

    TSimulationInfoPanelDisplayComponentSettingsTypeInfo = class(TDisplayComponentSettingsTypeInfo)
    public
        constructor Create(const aLibName, aLibVersion: string);
    end;


implementation


uses
    TypeInfo,
    DisplayComponentTypeDictionary,
    DisplayComponentSettings;


// ------------------------------------------------------------------------------------------------ Stop Button
{ TStopButtonDisplayComponentSettingList }

function TStopButtonDisplayComponentSettingList.GetShowClickEventNameSetting: boolean;
begin
    result := false;
end;

{ TStopButtonDisplayComponent }

constructor TStopButtonDisplayComponent.Create(const aName: string);
begin
    inherited;
end;

destructor TStopButtonDisplayComponent.Destroy;
begin
    inherited;
end;

function TStopButtonDisplayComponent.GetSettings: TStopButtonDisplayComponentSettingList;
begin
    result := fSettings as TStopButtonDisplayComponentSettingList;
end;

{ TStopButtonDisplayComponentTypeInfo }

constructor TStopButtonDisplayComponentSettingsTypeInfo.Create(const aLibName, aLibVersion: string);
const
    cStepTypeVersionStopButton = '1.0.0';
    cStepTypeNameStopButton = 'StopButton';
begin
    inherited Create(cStepTypeNameStopButton, cStepTypeVersionStopButton, aLibName, aLibVersion,
        TStopButtonDisplayComponent, TStopButtonDisplayComponentSettingList);
end;


// ------------------------------------------------------------------------------------------------ Main Panel

{ TMainPanelDisplayComponent }

constructor TMainPanelDisplayComponent.Create(const aName: string);
begin
    inherited;
end;

destructor TMainPanelDisplayComponent.Destroy;
begin
    inherited;
end;

function TMainPanelDisplayComponent.GetSettings: TMainPanelDisplayComponentSettingList;
begin
    result := fSettings as TMainPanelDisplayComponentSettingList;
end;

{ TMainPanelDisplayComponentSettingList }
function TMainPanelDisplayComponentSettingList.GetShowAlignSetting: boolean;
begin
    result := false;
end;

function TMainPanelDisplayComponentSettingList.GetShowPosSettings: boolean;
begin
    result := false;
end;

function TMainPanelDisplayComponentSettingList.GetShowSizeSettings: boolean;
begin
    result := false;
end;

procedure TMainPanelDisplayComponentSettingList.LoadSettings;
begin
    inherited;
    fAlign := TDisplayComponentSetting.cSettingAlignClient;
    fTop := 0;
    fLeft := 0;
end;

{ TMainPanelDisplayComponentSettingsTypeInfo }

constructor TMainPanelDisplayComponentSettingsTypeInfo.Create(const aLibName, aLibVersion: string);
const
    cStepTypeVersionMainPanel = '1.0.0';
    cStepTypeNameMainPanel = 'MainPanel';
begin
    inherited Create(cStepTypeNameMainPanel, cStepTypeVersionMainPanel, aLibName, aLibVersion,
        TMainPanelDisplayComponent, TMainPanelDisplayComponentSettingList);
end;

// ------------------------------------------------------------------------------------------------ DelayInfo Panel
{ TDelayInfoPanelDisplayComponentSettingList }

function TDelayInfoPanelDisplayComponentSettingList.GetCanHaveChildren: boolean;
begin
    result := false;
end;

{ TDelayInfoPanelDisplayComponent }

constructor TDelayInfoPanelDisplayComponent.Create(const aName: string);
begin
    inherited;
end;

destructor TDelayInfoPanelDisplayComponent.Destroy;
begin
    inherited;
end;

function TDelayInfoPanelDisplayComponent.GetSettings: TDelayInfoPanelDisplayComponentSettingList;
begin
    result := fSettings as TDelayInfoPanelDisplayComponentSettingList;
end;

{ TDelayInfoPanelDisplayComponentSettingsTypeInfo }
constructor TDelayInfoPanelDisplayComponentSettingsTypeInfo.Create(const aLibName, aLibVersion: string);
const
    cStepTypeVersionDelayInfoPanel = '1.0.0';
    cStepTypeNameDelayInfoPanel = 'DelayInfoPanel';
begin
    inherited Create(cStepTypeNameDelayInfoPanel, cStepTypeVersionDelayInfoPanel, aLibName, aLibVersion,
        TDelayInfoPanelDisplayComponent, TDelayInfoPanelDisplayComponentSettingList);
end;
// ------------------------------------------------------------------------------------------------ LayoutInfo Panel
{ TLayoutInfoPanelDisplayComponentSettingList }

function TLayoutInfoPanelDisplayComponentSettingList.GetCanHaveChildren: boolean;
begin
    result := false;
end;

{ TLayoutInfoPanelDisplayComponent }

constructor TLayoutInfoPanelDisplayComponent.Create(const aName: string);
begin
    inherited;
end;

destructor TLayoutInfoPanelDisplayComponent.Destroy;
begin
    inherited;
end;

function TLayoutInfoPanelDisplayComponent.GetSettings: TLayoutInfoPanelDisplayComponentSettingList;
begin
    result := fSettings as TLayoutInfoPanelDisplayComponentSettingList;
end;

{ TLayoutInfoPanelDisplayComponentSettingsTypeInfo }

constructor TLayoutInfoPanelDisplayComponentSettingsTypeInfo.Create(const aLibName, aLibVersion: string);
const
    cStepTypeVersionLayoutInfoPanel = '1.0.0';
    cStepTypeNameLayoutInfoPanel = 'LayoutInfoPanel';
begin
    inherited Create(cStepTypeNameLayoutInfoPanel, cStepTypeVersionLayoutInfoPanel, aLibName, aLibVersion,
        TLayoutInfoPanelDisplayComponent, TLayoutInfoPanelDisplayComponentSettingList);
end;
// ------------------------------------------------------------------------------------------------ RunInfo Panel
{ TRunInfoPanelDisplayComponentSettingList }

function TRunInfoPanelDisplayComponentSettingList.GetCanHaveChildren: boolean;
begin
    result := false;
end;

function TRunInfoPanelDisplayComponentSettingList.GetShowDisplayIDSetting: boolean;
begin
    result := true;
end;

procedure TRunInfoPanelDisplayComponentSettingList.AddCustomSettings;
begin
    inherited;
    self.AddInt('IdentifierColumnWidth', -1);
    self.AddInt('EventColumnWidth', -1);
    self.AddInt('DateColumnWidth', -1);
    self.AddBool('DateColumnVisible', true);
end;

procedure TRunInfoPanelDisplayComponentSettingList.LoadSettings;
begin
    inherited;
    fIdentifierColumnWidth := self.Find('IdentifierColumnWidth').AsInt;
    fEventColumnWidth := self.Find('EventColumnWidth').AsInt;
    fDateColumnWidth := self.Find('DateColumnWidth').AsInt;
    fDateColumnVisible := self.Find('DateColumnVisible').AsBool;
end;

{ TRunInfoPanelDisplayComponent }

constructor TRunInfoPanelDisplayComponent.Create(const aName: string);
begin
    inherited;
end;

destructor TRunInfoPanelDisplayComponent.Destroy;
begin
    inherited;
end;

function TRunInfoPanelDisplayComponent.GetSettings: TRunInfoPanelDisplayComponentSettingList;
begin
    result := fSettings as TRunInfoPanelDisplayComponentSettingList;
end;

{ TRunInfoPanelDisplayComponentSettingsTypeInfo }

constructor TRunInfoPanelDisplayComponentSettingsTypeInfo.Create(const aLibName, aLibVersion: string);
const
    cStepTypeVersionRunInfoPanel = '1.0.0';
    cStepTypeNameRunInfoPanel = 'RunInfoPanel';
begin
    inherited Create(cStepTypeNameRunInfoPanel, cStepTypeVersionRunInfoPanel, aLibName, aLibVersion,
        TRunInfoPanelDisplayComponent, TRunInfoPanelDisplayComponentSettingList);
end;
// ------------------------------------------------------------------------------------------------ LogInfo Panel
{ TLogInfoPanelDisplayComponentSettingList }

function TLogInfoPanelDisplayComponentSettingList.GetCanHaveChildren: boolean;
begin
    result := false;
end;

{ TLogInfoPanelDisplayComponent }

constructor TLogInfoPanelDisplayComponent.Create(const aName: string);
begin
    inherited;
end;

destructor TLogInfoPanelDisplayComponent.Destroy;
begin
    inherited;
end;

function TLogInfoPanelDisplayComponent.GetSettings: TLogInfoPanelDisplayComponentSettingList;
begin
    result := fSettings as TLogInfoPanelDisplayComponentSettingList;
end;

{ TLogInfoPanelDisplayComponentSettingsTypeInfo }

constructor TLogInfoPanelDisplayComponentSettingsTypeInfo.Create(const aLibName, aLibVersion: string);
const
    cStepTypeVersionLogInfoPanel = '1.0.0';
    cStepTypeNameLogInfoPanel = 'LogInfoPanel';
begin
    inherited Create(cStepTypeNameLogInfoPanel, cStepTypeVersionLogInfoPanel, aLibName, aLibVersion,
        TLogInfoPanelDisplayComponent, TLogInfoPanelDisplayComponentSettingList);
end;

// ------------------------------------------------------------------------------------------------ VolumesInfo Panel
{ TVolumesInfoPanelDisplayComponentSettingList }

function TVolumesInfoPanelDisplayComponentSettingList.GetCanHaveChildren: boolean;
begin
    result := false;
end;

{ TVolumesInfoPanelDisplayComponent }

constructor TVolumesInfoPanelDisplayComponent.Create(const aName: string);
begin
    inherited;
end;

destructor TVolumesInfoPanelDisplayComponent.Destroy;
begin
    inherited;
end;

function TVolumesInfoPanelDisplayComponent.GetSettings: TVolumesInfoPanelDisplayComponentSettingList;
begin
    result := fSettings as TVolumesInfoPanelDisplayComponentSettingList;
end;

{ TVolumesInfoPanelDisplayComponentSettingsTypeInfo }

constructor TVolumesInfoPanelDisplayComponentSettingsTypeInfo.Create(const aLibName, aLibVersion: string);
const
    cStepTypeVersionVolumesInfoPanel = '1.0.0';
    cStepTypeNameVolumesInfoPanel = 'VolumesInfoPanel';
begin
    inherited Create(cStepTypeNameVolumesInfoPanel, cStepTypeVersionVolumesInfoPanel, aLibName, aLibVersion,
        TVolumesInfoPanelDisplayComponent, TVolumesInfoPanelDisplayComponentSettingList);
end;
// ------------------------------------------------------------------------------------------------ SimulationInfo Panel
{ TSimulationInfoPanelDisplayComponentSettingList }

function TSimulationInfoPanelDisplayComponentSettingList.GetCanHaveChildren: boolean;
begin
    result := false;
end;

{ TSimulationInfoPanelDisplayComponent }

constructor TSimulationInfoPanelDisplayComponent.Create(const aName: string);
begin
    inherited;
end;

destructor TSimulationInfoPanelDisplayComponent.Destroy;
begin
    inherited;
end;

function TSimulationInfoPanelDisplayComponent.GetSettings: TSimulationInfoPanelDisplayComponentSettingList;
begin
    result := fSettings as TSimulationInfoPanelDisplayComponentSettingList;
end;

{ TSimulationInfoPanelDisplayComponentSettingsTypeInfo }

constructor TSimulationInfoPanelDisplayComponentSettingsTypeInfo.Create(const aLibName, aLibVersion: string);
const
    cStepTypeVersionSimulationInfoPanel = '1.0.0';
    cStepTypeNameSimulationInfoPanel = 'SimulationInfoPanel';
begin
    inherited Create(cStepTypeNameSimulationInfoPanel, cStepTypeVersionSimulationInfoPanel, aLibName,
        aLibVersion, TSimulationInfoPanelDisplayComponent, TSimulationInfoPanelDisplayComponentSettingList);
end;


end.
