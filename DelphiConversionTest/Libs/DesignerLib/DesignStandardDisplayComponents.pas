{ -----------------------------------------------------------------------------------------------------------------------
  Copyright © 2009 Zinsser Analytic GmbH - All rights reserved.
  Author       : Payman Kamali (pk)
  Description  :
  -----------------------------------------------------------------------------------------------------------------------
  Revision History:
  date     op  method                       track-no improvement/change
  -------- --  ---------------------------  -------- --------------------------------------------------------------------
  06.04.09 pk                                TN4503  Initial Revision
  27.08.09 pk                                TN4753  various changes
  17.11.11 wl                                TN5725   PositionInfo-Fenster entfernt
  ----------------------------------------------------------------------------------------------------------------------- }

unit DesignStandardDisplayComponents;


interface


uses
    ExtCtrls,
    Controls,
    DisplayComponentTypeInfo,
    StandardDisplayComponents;

type

    TDesignStopButtonDisplayComponent = class(TStopButtonDisplayComponent)
    protected
        procedure DoInitControlProperties(); override;
    end;

    TDesignStopButtonDisplayComponentTypeInfo = class(TDesignDisplayComponentTypeInfo)
    public
        constructor Create(const aLibName, aLibVersion: string);
    end;

    TDesignMainPanelDisplayComponent = class(TMainPanelDisplayComponent)
    protected
        procedure DoInitControlProperties(); override;
    end;

    TDesignMainPanelDisplayComponentTypeInfo = class(TDesignDisplayComponentTypeInfo)
    public
        constructor Create(const aLibName, aLibVersion: string);
    end;

    TDesignDelayInfoPanelDisplayComponent = class(TDelayInfoPanelDisplayComponent)
    protected
        procedure DoInitControlProperties(); override;
    end;

    TDesignDelayInfoPanelDisplayComponentTypeInfo = class(TDesignDisplayComponentTypeInfo)
    public
        constructor Create(const aLibName, aLibVersion: string);
    end;

    TDesignLayoutInfoPanelDisplayComponent = class(TLayoutInfoPanelDisplayComponent)
    protected
        procedure DoInitControlProperties(); override;
    end;

    TDesignLayoutInfoPanelDisplayComponentTypeInfo = class(TDesignDisplayComponentTypeInfo)
    public
        constructor Create(const aLibName, aLibVersion: string);
    end;

    TDesignRunInfoPanelDisplayComponent = class(TRunInfoPanelDisplayComponent)
    protected
        procedure DoInitControlProperties(); override;
    end;

    TDesignRunInfoPanelDisplayComponentTypeInfo = class(TDesignDisplayComponentTypeInfo)
    public
        constructor Create(const aLibName, aLibVersion: string);
    end;

    // ---------------------------------------------------------------------------------------------------------- LogInfo
    TDesignLogInfoPanelDisplayComponent = class(TLogInfoPanelDisplayComponent)
    protected
        procedure DoInitControlProperties(); override;
    end;

    TDesignLogInfoPanelDisplayComponentTypeInfo = class(TDesignDisplayComponentTypeInfo)
    public
        constructor Create(const aLibName, aLibVersion: string);
    end;

    // ---------------------------------------------------------------------------------------------------------- VolumesInfo
    TDesignVolumesInfoPanelDisplayComponent = class(TVolumesInfoPanelDisplayComponent)
    protected
        procedure DoInitControlProperties(); override;
    end;

    TDesignVolumesInfoPanelDisplayComponentTypeInfo = class(TDesignDisplayComponentTypeInfo)
    public
        constructor Create(const aLibName, aLibVersion: string);
    end;

    // ---------------------------------------------------------------------------------------------------------- SimulationInfo
    TDesignSimulationInfoPanelDisplayComponent = class(TSimulationInfoPanelDisplayComponent)
    protected
        procedure DoInitControlProperties(); override;
    end;

    TDesignSimulationInfoPanelDisplayComponentTypeInfo = class(TDesignDisplayComponentTypeInfo)
    public
        constructor Create(const aLibName, aLibVersion: string);
    end;

    TDesignStandardDisplayComponents = class
    protected
        fImages: TObject;
        procedure LoadImageTo(const aImageIndex: integer; const aTargetControl: TWinControl);
        constructor Create();
    public
        destructor Destroy(); override;
        class procedure AddStandardTypeInfos;
        class function Instance(): TDesignStandardDisplayComponents;
        class procedure CreateInstance();
        class procedure DestroyInstance();
    end;


implementation


uses
    SysUtils,
    TypeInfo,
    DisplayComponentTypeDictionary,
    DesignStandardDisplayComponentImages;

const
    cLibNameDisplayComponent = 'Design.exe';
    cLibVersionDisplayComponent = '1.0.0';

    cImageIndexMainPanel = 0;
    cImageIndexDelayInfoPanel = 1;
    cImageIndexLayoutInfoPanel = 2;
    cImageIndexRunInfoPanel = 3;
    cImageIndexStopButton = 4;

var
    uInstance: TDesignStandardDisplayComponents = nil;

    { TDesignStandardDisplayComponents }

class procedure TDesignStandardDisplayComponents.AddStandardTypeInfos;
var
    xTypeInfoList: TTypeInfoList;
begin
    xTypeInfoList := TTypeInfoList.Create(false);
    try
        xTypeInfoList.Add(TDesignStopButtonDisplayComponentTypeInfo.Create(cLibNameDisplayComponent,
            cLibVersionDisplayComponent));
        xTypeInfoList.Add(TDesignMainPanelDisplayComponentTypeInfo.Create(cLibNameDisplayComponent,
            cLibVersionDisplayComponent));
        xTypeInfoList.Add(TDesignDelayInfoPanelDisplayComponentTypeInfo.Create(cLibNameDisplayComponent,
            cLibVersionDisplayComponent));
        xTypeInfoList.Add(TDesignLayoutInfoPanelDisplayComponentTypeInfo.Create(cLibNameDisplayComponent,
            cLibVersionDisplayComponent));
        xTypeInfoList.Add(TDesignRunInfoPanelDisplayComponentTypeInfo.Create(cLibNameDisplayComponent,
            cLibVersionDisplayComponent));
        xTypeInfoList.Add(TDesignLogInfoPanelDisplayComponentTypeInfo.Create(cLibNameDisplayComponent,
            cLibVersionDisplayComponent));
        xTypeInfoList.Add(TDesignVolumesInfoPanelDisplayComponentTypeInfo.Create(cLibNameDisplayComponent,
            cLibVersionDisplayComponent));
        xTypeInfoList.Add(TDesignSimulationInfoPanelDisplayComponentTypeInfo.Create(cLibNameDisplayComponent,
            cLibVersionDisplayComponent));
        TDesignDisplayComponentTypeDictionary.Instance.AddTypes(xTypeInfoList);
    finally
        xTypeInfoList.Free;
    end;
end;

class function TDesignStandardDisplayComponents.Instance(): TDesignStandardDisplayComponents;
begin
    result := uInstance;
end;

class procedure TDesignStandardDisplayComponents.CreateInstance();
begin
    if Assigned(uInstance) then
        EXIT;
    uInstance := TDesignStandardDisplayComponents.Create();
    uInstance.AddStandardTypeInfos();
end;

constructor TDesignStandardDisplayComponents.Create;
begin
    inherited Create();
    fImages := TFrmDesignStandardDisplayComponentImages.Create(nil);
end;

destructor TDesignStandardDisplayComponents.Destroy;
begin
    fImages.Free;
    inherited;
end;

procedure TDesignStandardDisplayComponents.LoadImageTo(const aImageIndex: integer;
    const aTargetControl: TWinControl);
var
    xSourceImage: TImage;
    xImage: TImage;
begin
    inherited;
    xImage := TImage.Create(aTargetControl);
    xImage.Parent := aTargetControl;
    xImage.Align := alClient;
    xImage.Visible := true;

    xSourceImage := (fImages as TFrmDesignStandardDisplayComponentImages)
        .FindComponent('Image' + IntToStr(aImageIndex + 1)) as TImage;
    if not Assigned(xSourceImage) then
        EXIT;
    xImage.Picture.Assign(xSourceImage.Picture);
end;

class procedure TDesignStandardDisplayComponents.DestroyInstance;
begin

end;

{ TDesignStopButtonDisplayComponentTypeInfo }

constructor TDesignStopButtonDisplayComponentTypeInfo.Create(const aLibName, aLibVersion: string);
const
    cStepTypeVersionStopButton = '1.0.0';
    cStepTypeNameStopButton = 'StopButton';
begin
    inherited Create(cStepTypeNameStopButton, cStepTypeVersionStopButton, aLibName, aLibVersion,
        TDesignStopButtonDisplayComponent);
end;

{ TDesignStopButtonDisplayComponent }

procedure TDesignStopButtonDisplayComponent.DoInitControlProperties;
begin
    inherited;
    // TDesignStandardDisplayComponents.Instance.LoadImageTo( cImageIndexStopButton, self.Control );
end;

{ TDesignMainPanelDisplayComponentTypeInfo }

constructor TDesignMainPanelDisplayComponentTypeInfo.Create(const aLibName, aLibVersion: string);
const
    cStepTypeVersionMainPanel = '1.0.0';
    cStepTypeNameMainPanel = 'MainPanel';
begin
    inherited Create(cStepTypeNameMainPanel, cStepTypeVersionMainPanel, aLibName, aLibVersion,
        TDesignMainPanelDisplayComponent);
end;

{ TDesignMainPanelDisplayComponent }

procedure TDesignMainPanelDisplayComponent.DoInitControlProperties;
begin
    inherited;
    TDesignStandardDisplayComponents.Instance.LoadImageTo(cImageIndexMainPanel, self.Control);
end;

{ TDesignDelayInfoPanelDisplayComponentTypeInfo }

constructor TDesignDelayInfoPanelDisplayComponentTypeInfo.Create(const aLibName, aLibVersion: string);
const
    cStepTypeVersionDelayInfoPanel = '1.0.0';
    cStepTypeNameDelayInfoPanel = 'DelayInfoPanel';
begin
    inherited Create(cStepTypeNameDelayInfoPanel, cStepTypeVersionDelayInfoPanel, aLibName, aLibVersion,
        TDesignDelayInfoPanelDisplayComponent);
end;

{ TDesignDelayInfoPanelDisplayComponent }

procedure TDesignDelayInfoPanelDisplayComponent.DoInitControlProperties;
begin
    inherited;
    TDesignStandardDisplayComponents.Instance.LoadImageTo(cImageIndexDelayInfoPanel, self.Control);
end;

{ TDesignLayoutInfoPanelDisplayComponentTypeInfo }

constructor TDesignLayoutInfoPanelDisplayComponentTypeInfo.Create(const aLibName, aLibVersion: string);
const
    cStepTypeVersionLayoutInfoPanel = '1.0.0';
    cStepTypeNameLayoutInfoPanel = 'LayoutInfoPanel';
begin
    inherited Create(cStepTypeNameLayoutInfoPanel, cStepTypeVersionLayoutInfoPanel, aLibName, aLibVersion,
        TDesignLayoutInfoPanelDisplayComponent);
end;

{ TDesignLayoutInfoPanelDisplayComponent }

procedure TDesignLayoutInfoPanelDisplayComponent.DoInitControlProperties;
begin
    inherited;
    TDesignStandardDisplayComponents.Instance.LoadImageTo(cImageIndexLayoutInfoPanel, self.Control);
end;

{ TDesignRunInfoPanelDisplayComponentTypeInfo }

constructor TDesignRunInfoPanelDisplayComponentTypeInfo.Create(const aLibName, aLibVersion: string);
const
    cStepTypeVersionRunInfoPanel = '1.0.0';
    cStepTypeNameRunInfoPanel = 'RunInfoPanel';
begin
    inherited Create(cStepTypeNameRunInfoPanel, cStepTypeVersionRunInfoPanel, aLibName, aLibVersion,
        TDesignRunInfoPanelDisplayComponent);
end;

{ TDesignRunInfoPanelDisplayComponent }

procedure TDesignRunInfoPanelDisplayComponent.DoInitControlProperties;
begin
    inherited;
    TDesignStandardDisplayComponents.Instance.LoadImageTo(cImageIndexRunInfoPanel, self.Control);
end;

// ---------------------------------------------------------------------------------------------------------- LogInfo

{ TDesignLogInfoPanelDisplayComponentTypeInfo }

constructor TDesignLogInfoPanelDisplayComponentTypeInfo.Create(const aLibName, aLibVersion: string);
const
    cStepTypeVersionLogInfoPanel = '1.0.0';
    cStepTypeNameLogInfoPanel = 'LogInfoPanel';
begin
    inherited Create(cStepTypeNameLogInfoPanel, cStepTypeVersionLogInfoPanel, aLibName, aLibVersion,
        TDesignLogInfoPanelDisplayComponent);
end;

{ TDesignLogInfoPanelDisplayComponent }

procedure TDesignLogInfoPanelDisplayComponent.DoInitControlProperties;
begin
    inherited;
    // TDesignStandardDisplayComponents.Instance.LoadImageTo( cImageIndexLogInfoPanel, self.Control );
end;

// ---------------------------------------------------------------------------------------------------------- VolumesInfo

{ TDesignVolumesInfoPanelDisplayComponentTypeInfo }

constructor TDesignVolumesInfoPanelDisplayComponentTypeInfo.Create(const aLibName, aLibVersion: string);
const
    cStepTypeVersionVolumesInfoPanel = '1.0.0';
    cStepTypeNameVolumesInfoPanel = 'VolumesInfoPanel';
begin
    inherited Create(cStepTypeNameVolumesInfoPanel, cStepTypeVersionVolumesInfoPanel, aLibName, aLibVersion,
        TDesignVolumesInfoPanelDisplayComponent);
end;

{ TDesignVolumesInfoPanelDisplayComponent }

procedure TDesignVolumesInfoPanelDisplayComponent.DoInitControlProperties;
begin
    inherited;
    // TDesignStandardDisplayComponents.Instance.LoadImageTo( cImageIndexVolumesInfoPanel, self.Control );
end;

// ---------------------------------------------------------------------------------------------------------- SimulationInfo

{ TDesignSimulationInfoPanelDisplayComponentTypeInfo }

constructor TDesignSimulationInfoPanelDisplayComponentTypeInfo.Create(const aLibName, aLibVersion: string);
const
    cStepTypeVersionSimulationInfoPanel = '1.0.0';
    cStepTypeNameSimulationInfoPanel = 'SimulationInfoPanel';
begin
    inherited Create(cStepTypeNameSimulationInfoPanel, cStepTypeVersionSimulationInfoPanel, aLibName,
        aLibVersion, TDesignSimulationInfoPanelDisplayComponent);
end;

{ TDesignSimulationInfoPanelDisplayComponent }

procedure TDesignSimulationInfoPanelDisplayComponent.DoInitControlProperties;
begin
    inherited;
    // TDesignStandardDisplayComponents.Instance.LoadImageTo( cImageIndexSimulationInfoPanel, self.Control );
end;


end.
