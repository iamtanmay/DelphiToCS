{ -----------------------------------------------------------------------------------------------------------------------
  Copyright © 2009 Zinsser Analytic GmbH - All rights reserved.
  Author       : Payman Kamali (pk)
  Description  :
  -----------------------------------------------------------------------------------------------------------------------
  Revision History:
  date     op  method                                track-no    improvement/change
  -------- --  ------------------------------------  ----------  --------------------------------------------------------------------
  06.04.09 pk                                        TN4503      Initial Revision
  24.07.09 pk  TRunRunInfoPanelDisplayComponent      TN4675      If displayID = '' create a new runinformation form
  24.07.09 pk  TRunRunInfoPanelDisplayComponent      TN4676      New ColumnWidthSettings
  31.07.09 ts  TRunRunInfoPanelDisplayComponent      TN4666      InfoGroupBehaviour instead of HideGroup
  27.08.09 pk                                        TN4753      various changes
  04.09.10 pk  TRunStopButtonDisplayComponent        TN5042      set enabled property to false by default
  11.05.10 pk  TRunLogInfoPanelDisplayComponent      TN4958      Clear log lines
  15.11.10 pk                                        TN5340      Changes to prevent memory leak-
  21.03.11 wl                                        TN5508   SimulationInfo --> Elemente nach AskRunStart verlegt
  17.11.11 wl                                        TN5725   PositionInfo-Fenster entfernt
  28.02.13 wl  TRunInfoPanelDisplayComponent         TN6096   neu: DateColumnVisible
  ----------------------------------------------------------------------------------------------------------------------- }

unit RunStandardDisplayComponents;


interface


uses
    DisplayComponentIntf,
    DisplayComponentTypeInfo,
    BasicDisplayComponent,
    StandardDisplayComponents,
    RunInformation,
    AppTypes,
    TypeInfo;

type
    // --------------------------------------------------------------------------------------------- Stop Button
    TRunStopButtonDisplayComponent = class(TStopButtonDisplayComponent)
    protected
        procedure DoInitControlProperties(); override;
        procedure DoFinalizeControlProperties(); override;
    end;

    TRunStopButtonDisplayComponentTypeInfo = class(TRunDisplayComponentTypeInfo)
    public
        constructor Create(const aLibName, aLibVersion: string);
    end;

    // --------------------------------------------------------------------------------------------- Main
    TRunMainPanelDisplayComponent = class(TMainPanelDisplayComponent)
    protected
        // function DoCreateDisplayControl : TDisplayControl; override;
        procedure DoInitControlProperties(); override;
    end;

    TRunMainPanelDisplayComponentTypeInfo = class(TRunDisplayComponentTypeInfo)
    public
        constructor Create(const aLibName, aLibVersion: string);
    end;

    // --------------------------------------------------------------------------------------------- DelayInfo
    TRunDelayInfoPanelDisplayComponent = class(TDelayInfoPanelDisplayComponent)
    protected
        procedure DoInitControlProperties(); override;
        procedure DoFinalizeControlProperties(); override;
    end;

    TRunDelayInfoPanelDisplayComponentTypeInfo = class(TRunDisplayComponentTypeInfo)
    public
        constructor Create(const aLibName, aLibVersion: string);
    end;

    // --------------------------------------------------------------------------------------------- LayoutInfo
    TRunLayoutInfoPanelDisplayComponent = class(TLayoutInfoPanelDisplayComponent)
    protected
        procedure DoInitControlProperties(); override;
        procedure DoFinalizeControlProperties(); override;
    end;

    TRunLayoutInfoPanelDisplayComponentTypeInfo = class(TRunDisplayComponentTypeInfo)
    public
        constructor Create(const aLibName, aLibVersion: string);
    end;

    // --------------------------------------------------------------------------------------------- RunInfo
    TRunRunInfoPanelDisplayComponent = class(TRunInfoPanelDisplayComponent, IRunInfoPanelDisplayComponent)
    private
        fRunInfoControl: TfrmRunInformation;
        fIsOwner: boolean;
    protected
        procedure SetVisible(const aValue: boolean); override;
        procedure DoInitControlProperties(); override;
        procedure DoFinalizeControlProperties(); override;
    public
        procedure InsertInfo(const aGroupNames: TArray<string>; const aKey, aText: string;
            aInfoGroupBehaviour: TInfoGroupBehaviour); override;
    end;

    TRunRunInfoPanelDisplayComponentTypeInfo = class(TRunDisplayComponentTypeInfo)
    public
        constructor Create(const aLibName, aLibVersion: string);
    end;

    // --------------------------------------------------------------------------------------------- LogInfo
    TRunLogInfoPanelDisplayComponent = class(TLogInfoPanelDisplayComponent)
    protected
        procedure DoInitControlProperties(); override;
        procedure DoFinalizeControlProperties(); override;
    end;

    TRunLogInfoPanelDisplayComponentTypeInfo = class(TRunDisplayComponentTypeInfo)
    public
        constructor Create(const aLibName, aLibVersion: string);
    end;

    // --------------------------------------------------------------------------------------------- VolumesInfo
    TRunVolumesInfoPanelDisplayComponent = class(TVolumesInfoPanelDisplayComponent)
    protected
        procedure DoInitControlProperties(); override;
        procedure DoFinalizeControlProperties(); override;
    end;

    TRunVolumesInfoPanelDisplayComponentTypeInfo = class(TRunDisplayComponentTypeInfo)
    public
        constructor Create(const aLibName, aLibVersion: string);
    end;

    TRunStandardDisplayComponents = class
    strict private
        fTypeInfoList: TTypeInfoList;
        class var uInstance: TRunStandardDisplayComponents;
        constructor Create();
        procedure AddStandardTypeInfos;
    public
        destructor Destroy(); override;
        class property Instance: TRunStandardDisplayComponents read uInstance;
        class procedure CreateInstance();
        class procedure DestroyInstance();
    end;


implementation


uses
    SysUtils,
    Controls,
    DisplayComponentTypeDictionary,
    RunMainControls,
    LogInfo;

{ TRunStandardDisplayComponents }

procedure TRunStandardDisplayComponents.AddStandardTypeInfos;
const
    cLibNameDisplayComponent = 'RunDialogs.bpl';
    cLibVersionDisplayComponent = '1.0.0';
begin
    fTypeInfoList.Add(TRunStopButtonDisplayComponentTypeInfo.Create(cLibNameDisplayComponent,
        cLibVersionDisplayComponent));
    fTypeInfoList.Add(TRunMainPanelDisplayComponentTypeInfo.Create(cLibNameDisplayComponent,
        cLibVersionDisplayComponent));
    fTypeInfoList.Add(TRunDelayInfoPanelDisplayComponentTypeInfo.Create(cLibNameDisplayComponent,
        cLibVersionDisplayComponent));
    fTypeInfoList.Add(TRunLayoutInfoPanelDisplayComponentTypeInfo.Create(cLibNameDisplayComponent,
        cLibVersionDisplayComponent));
    fTypeInfoList.Add(TRunRunInfoPanelDisplayComponentTypeInfo.Create(cLibNameDisplayComponent,
        cLibVersionDisplayComponent));
    fTypeInfoList.Add(TRunLogInfoPanelDisplayComponentTypeInfo.Create(cLibNameDisplayComponent,
        cLibVersionDisplayComponent));
    fTypeInfoList.Add(TRunVolumesInfoPanelDisplayComponentTypeInfo.Create(cLibNameDisplayComponent,
        cLibVersionDisplayComponent));
    TRunDisplayComponentTypeDictionary.Instance.AddTypes(fTypeInfoList);
end;

class procedure TRunStandardDisplayComponents.CreateInstance();
begin
    if Assigned(uInstance) then
        EXIT;
    uInstance := TRunStandardDisplayComponents.Create();
end;

class procedure TRunStandardDisplayComponents.DestroyInstance;
begin
    FreeAndNil(uInstance);
end;

constructor TRunStandardDisplayComponents.Create;
begin
    inherited Create();
    fTypeInfoList := TTypeInfoList.Create(true);
    self.AddStandardTypeInfos();
end;

destructor TRunStandardDisplayComponents.Destroy;
begin
    FreeAndNil(fTypeInfoList);
    inherited;
end;


// --------------------------------------------------------------------------------------------- Stop Button
{ TRunStopButtonDisplayComponentTypeInfo }

constructor TRunStopButtonDisplayComponentTypeInfo.Create(const aLibName, aLibVersion: string);
const
    cStepTypeVersionStopButton = '1.0.0';
    cStepTypeNameStopButton = 'StopButton';
begin
    inherited Create(cStepTypeNameStopButton, cStepTypeVersionStopButton, aLibName, aLibVersion,
        TRunStopButtonDisplayComponent);
end;

{ TRunStopButtonDisplayComponent }

procedure TRunStopButtonDisplayComponent.DoInitControlProperties;
begin
    inherited;
    self.Control.OnClick := TRunMainControls.Instance.OnStopButtonClick;
    self.Control.Enabled := false;
end;

procedure TRunStopButtonDisplayComponent.DoFinalizeControlProperties;
begin
    inherited;
end;

// --------------------------------------------------------------------------------------------- Main
{ TRunMainPanelDisplayComponentTypeInfo }

constructor TRunMainPanelDisplayComponentTypeInfo.Create(const aLibName, aLibVersion: string);
const
    cStepTypeVersionMainPanel = '1.0.0';
    cStepTypeNameMainPanel = 'MainPanel';
begin
    inherited Create(cStepTypeNameMainPanel, cStepTypeVersionMainPanel, aLibName, aLibVersion,
        TRunMainPanelDisplayComponent);
end;

procedure TRunMainPanelDisplayComponent.DoInitControlProperties;
begin
    inherited;
end;

// --------------------------------------------------------------------------------------------- DelayInfo
{ TRunDelayInfoPanelDisplayComponentTypeInfo }

constructor TRunDelayInfoPanelDisplayComponentTypeInfo.Create(const aLibName, aLibVersion: string);
const
    cStepTypeVersionDelayInfoPanel = '1.0.0';
    cStepTypeNameDelayInfoPanel = 'DelayInfoPanel';
begin
    inherited Create(cStepTypeNameDelayInfoPanel, cStepTypeVersionDelayInfoPanel, aLibName, aLibVersion,
        TRunDelayInfoPanelDisplayComponent);
end;

{ TRunDelayInfoPanelDisplayComponent }

procedure TRunDelayInfoPanelDisplayComponent.DoInitControlProperties;
begin
    inherited;
    TRunMainControls.Instance.DelayInfoControl.Parent := self.GetDisplayHandle();
    TRunMainControls.Instance.DelayInfoControl.Visible := true;
end;

procedure TRunDelayInfoPanelDisplayComponent.DoFinalizeControlProperties;
begin
    TRunMainControls.Instance.DelayInfoControl.Visible := false;
    TRunMainControls.Instance.DelayInfoControl.Parent := nil;
    inherited;
end;


// --------------------------------------------------------------------------------------------- LayoutInfo
{ TRunLayoutInfoPanelDisplayComponentTypeInfo }

constructor TRunLayoutInfoPanelDisplayComponentTypeInfo.Create(const aLibName, aLibVersion: string);
const
    cStepTypeVersionLayoutInfoPanel = '1.0.0';
    cStepTypeNameLayoutInfoPanel = 'LayoutInfoPanel';
begin
    inherited Create(cStepTypeNameLayoutInfoPanel, cStepTypeVersionLayoutInfoPanel, aLibName, aLibVersion,
        TRunLayoutInfoPanelDisplayComponent);
end;

{ TRunLayoutInfoPanelDisplayComponent }

procedure TRunLayoutInfoPanelDisplayComponent.DoInitControlProperties;
begin
    inherited;
    TRunMainControls.Instance.LayoutInfoControl.Parent := self.GetDisplayHandle();
    TRunMainControls.Instance.LayoutInfoControl.Visible := true;
end;

procedure TRunLayoutInfoPanelDisplayComponent.DoFinalizeControlProperties;
begin
    TRunMainControls.Instance.LayoutInfoControl.Visible := false;
    TRunMainControls.Instance.LayoutInfoControl.Parent := nil;
    inherited;
end;


// --------------------------------------------------------------------------------------------- RunInfo
{ TRunRunInfoPanelDisplayComponentTypeInfo }

constructor TRunRunInfoPanelDisplayComponentTypeInfo.Create(const aLibName, aLibVersion: string);
const
    cStepTypeVersionRunInfoPanel = '1.0.0';
    cStepTypeNameRunInfoPanel = 'RunInfoPanel';
begin
    inherited Create(cStepTypeNameRunInfoPanel, cStepTypeVersionRunInfoPanel, aLibName, aLibVersion,
        TRunRunInfoPanelDisplayComponent);
end;

{ TRunRunInfoPanelDisplayComponent }

procedure TRunRunInfoPanelDisplayComponent.DoInitControlProperties;
begin
    inherited;
    fIsOwner := false;

    if self.DisplayID = '' then
    begin
        fRunInfoControl := TRunMainControls.Instance.RunInfoControl as TfrmRunInformation;
    end
    else
    begin
        fRunInfoControl := TfrmRunInformation.Create(nil);
        fRunInfoControl.Align := alClient;
        fIsOwner := true;
    end;
    fRunInfoControl.Parent := self.GetDisplayHandle();
    fRunInfoControl.SetColumnWidths(self.Settings.IdentifierColumnWidth, self.Settings.EventColumnWidth,
        self.Settings.DateColumnWidth, self.Settings.DateColumnVisible);
end;

procedure TRunRunInfoPanelDisplayComponent.DoFinalizeControlProperties;
begin
    fRunInfoControl.Visible := false;
    fRunInfoControl.Parent := nil;

    if fIsOwner then
    begin
        fRunInfoControl.Free;
    end;

    inherited;
end;

procedure TRunRunInfoPanelDisplayComponent.InsertInfo(const aGroupNames: TArray<string>;
    const aKey, aText: string; aInfoGroupBehaviour: TInfoGroupBehaviour);
begin
    fRunInfoControl.InsertInfo(aGroupNames, aKey, aText, aInfoGroupBehaviour);
end;

procedure TRunRunInfoPanelDisplayComponent.SetVisible(const aValue: boolean);
begin
    inherited;
    fRunInfoControl.Visible := aValue;
end;

// --------------------------------------------------------------------------------------------- LogInfo
{ TRunLogInfoPanelDisplayComponentTypeInfo }

constructor TRunLogInfoPanelDisplayComponentTypeInfo.Create(const aLibName, aLibVersion: string);
const
    cStepTypeVersionLogInfoPanel = '1.0.0';
    cStepTypeNameLogInfoPanel = 'LogInfoPanel';
begin
    inherited Create(cStepTypeNameLogInfoPanel, cStepTypeVersionLogInfoPanel, aLibName, aLibVersion,
        TRunLogInfoPanelDisplayComponent);
end;

{ TRunLogInfoPanelDisplayComponent }

procedure TRunLogInfoPanelDisplayComponent.DoInitControlProperties;
begin
    inherited;
    TRunMainControls.Instance.LogInfoControl.Parent := self.GetDisplayHandle();
    TRunMainControls.Instance.LogInfoControl.Visible := true;
end;

procedure TRunLogInfoPanelDisplayComponent.DoFinalizeControlProperties;
var
    xControl: TControl;
begin
    xControl := TRunMainControls.Instance.LogInfoControl;
    // 15.01.10 pk HACK! clear the memo,  because the next time we load it, it will load all of the old log lines - takes up lots of time
    // and sometimes even causes an AV
    if xControl is TfrmLogInfo then
        (xControl as TfrmLogInfo).MainLogDisplay.Clear;
    xControl.Visible := false;
    xControl.Parent := nil;
    inherited;
end;

// --------------------------------------------------------------------------------------------- Volumes Info
{ TRunVolumesInfoPanelDisplayComponentTypeInfo }

constructor TRunVolumesInfoPanelDisplayComponentTypeInfo.Create(const aLibName, aLibVersion: string);
const
    cStepTypeVersionVolumesInfoPanel = '1.0.0';
    cStepTypeNameVolumesInfoPanel = 'VolumesInfoPanel';
begin
    inherited Create(cStepTypeNameVolumesInfoPanel, cStepTypeVersionVolumesInfoPanel, aLibName, aLibVersion,
        TRunVolumesInfoPanelDisplayComponent);
end;

{ TRunVolumesInfoPanelDisplayComponent }

procedure TRunVolumesInfoPanelDisplayComponent.DoInitControlProperties;
begin
    inherited;
    TRunMainControls.Instance.VolumesInfoControl.Parent := self.GetDisplayHandle();
    TRunMainControls.Instance.VolumesInfoControl.Visible := true;
end;

procedure TRunVolumesInfoPanelDisplayComponent.DoFinalizeControlProperties;
begin
    TRunMainControls.Instance.VolumesInfoControl.Visible := false;
    TRunMainControls.Instance.VolumesInfoControl.Parent := nil;
    inherited;
end;


end.
