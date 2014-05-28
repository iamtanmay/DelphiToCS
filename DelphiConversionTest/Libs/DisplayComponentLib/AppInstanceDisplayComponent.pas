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
  24.00.09 pk                                TN4753  call TDisplayComponentsDataCache.Instance.ReadIntoCache
  28.09.09 pk  uRefCount                     TN4753  New reference counting
  25.05.11 ts  SQLTableDisplayComponent      TN5590  new: SQLTableDisplayComponent
  11.09.11 wl                                        TN5672   Instance entfernt
  17.11.11 wl                                TN5725   PositionInfo-Fenster entfernt
  13.11.12 wl                                TN6015   überarbeitet für DisplayComponentsDataAdaptor
  23.11.12 wl  Instance                      TN6015.1 Es gibt keine Instanz mehr von TDisplayComponentSettingsManager
  29.11.12 wl                                TN6015.2 DesignDisplayComponents abgeschafft (Es gibt nur noch RunDisplayComponents)
  29.11.12 wl  InitSettingsTypeInfos         TN6015.2 SimulationInfoPanel gibt es nicht - entfernt
  ----------------------------------------------------------------------------------------------------------------------- }

unit AppInstanceDisplayComponent;


interface


type
    TAppInstanceDisplayComponent = class
    private const
        cLibNameDisplayComponent = 'DisplayComponentLib.bpl';
        cLibVersionDisplayComponent = '1.0.0';
    private
        procedure InitRunTypeInfos();
        procedure InitSettingsTypeInfos;
    public
        constructor Create();
        destructor Destroy(); override;
    end;


implementation


uses
    TypeInfo,
    DisplayComponentTypeDictionary,
    DisplayComponentManager,
    DisplayComponentEventManager,
    PanelDisplayComponent,
    ButtonDisplayComponent,
    SplitterDisplayComponent,
    TabHostDisplayComponent,
    TabSheetDisplayComponent,
    StandardDisplayComponents,
    SQLTableDisplayComponent;

{ TAppInstanceDisplayComponent }

constructor TAppInstanceDisplayComponent.Create;
begin
    inherited Create();

    TRunDisplayComponentTypeDictionary.CreateInstance();
    InitRunTypeInfos();

    TDisplayComponentSettingsTypeDictionary.CreateInstance();
    InitSettingsTypeInfos();

    TDisplayComponentManager.CreateInstance();

    TDisplayComponentEventManager.CreateInstance();
end;

destructor TAppInstanceDisplayComponent.Destroy;
begin
    TDisplayComponentEventManager.DestroyInstance();
    TDisplayComponentManager.DestroyInstance();
    TDisplayComponentSettingsTypeDictionary.DestroyInstance();
    TRunDisplayComponentTypeDictionary.DestroyInstance;

    inherited;
end;

procedure TAppInstanceDisplayComponent.InitRunTypeInfos();
var
    xStaticTypeInfos: TTypeInfoList;
begin
    xStaticTypeInfos := TTypeInfoList.Create(false);
    try
        xStaticTypeInfos.Add(TRunPanelDisplayComponentTypeInfo.Create(cLibNameDisplayComponent,
            cLibVersionDisplayComponent));
        xStaticTypeInfos.Add(TRunButtonDisplayComponentTypeInfo.Create(cLibNameDisplayComponent,
            cLibVersionDisplayComponent));
        xStaticTypeInfos.Add(TRunSplitterDisplayComponentTypeInfo.Create(cLibNameDisplayComponent,
            cLibVersionDisplayComponent));
        xStaticTypeInfos.Add(TRunTabHostDisplayComponentTypeInfo.Create(cLibNameDisplayComponent,
            cLibVersionDisplayComponent));
        xStaticTypeInfos.Add(TRunTabSheetDisplayComponentTypeInfo.Create(cLibNameDisplayComponent,
            cLibVersionDisplayComponent));
        xStaticTypeInfos.Add(TRunSQLTableDisplayComponentTypeInfo.Create(cLibNameDisplayComponent,
            cLibVersionDisplayComponent));
        TRunDisplayComponentTypeDictionary.Instance.AddTypes(xStaticTypeInfos);
    finally
        xStaticTypeInfos.Free;
    end;

end;

procedure TAppInstanceDisplayComponent.InitSettingsTypeInfos();
var
    xStaticTypeInfos: TTypeInfoList;
begin
    xStaticTypeInfos := TTypeInfoList.Create(false);
    try
        xStaticTypeInfos.Add(TPanelDisplayComponentSettingsTypeInfo.Create(cLibNameDisplayComponent,
            cLibVersionDisplayComponent));
        xStaticTypeInfos.Add(TButtonDisplayComponentSettingsTypeInfo.Create(cLibNameDisplayComponent,
            cLibVersionDisplayComponent));
        xStaticTypeInfos.Add(TSplitterDisplayComponentSettingsTypeInfo.Create(cLibNameDisplayComponent,
            cLibVersionDisplayComponent));
        xStaticTypeInfos.Add(TTabHostDisplayComponentSettingsTypeInfo.Create(cLibNameDisplayComponent,
            cLibVersionDisplayComponent));
        xStaticTypeInfos.Add(TTabSheetDisplayComponentSettingsTypeInfo.Create(cLibNameDisplayComponent,
            cLibVersionDisplayComponent));
        xStaticTypeInfos.Add(TStopButtonDisplayComponentSettingsTypeInfo.Create(cLibNameDisplayComponent,
            cLibVersionDisplayComponent));
        xStaticTypeInfos.Add(TMainPanelDisplayComponentSettingsTypeInfo.Create(cLibNameDisplayComponent,
            cLibVersionDisplayComponent));
        xStaticTypeInfos.Add(TDelayInfoPanelDisplayComponentSettingsTypeInfo.Create(cLibNameDisplayComponent,
            cLibVersionDisplayComponent));
        xStaticTypeInfos.Add(TLayoutInfoPanelDisplayComponentSettingsTypeInfo.Create(cLibNameDisplayComponent,
            cLibVersionDisplayComponent));
        xStaticTypeInfos.Add(TRunInfoPanelDisplayComponentSettingsTypeInfo.Create(cLibNameDisplayComponent,
            cLibVersionDisplayComponent));
        xStaticTypeInfos.Add(TLogInfoPanelDisplayComponentSettingsTypeInfo.Create(cLibNameDisplayComponent,
            cLibVersionDisplayComponent));
        xStaticTypeInfos.Add(TVolumesInfoPanelDisplayComponentSettingsTypeInfo.Create
            (cLibNameDisplayComponent, cLibVersionDisplayComponent));
        // xStaticTypeInfos.Add(TSimulationInfoPanelDisplayComponentSettingsTypeInfo.Create
        // (cLibNameDisplayComponent, cLibVersionDisplayComponent));
        xStaticTypeInfos.Add(TSQLTableDisplayComponentSettingsTypeInfo.Create(cLibNameDisplayComponent,
            cLibVersionDisplayComponent));
        TDisplayComponentSettingsTypeDictionary.Instance.AddTypes(xStaticTypeInfos);

    finally
        xStaticTypeInfos.Free;
    end;
end;


end.
