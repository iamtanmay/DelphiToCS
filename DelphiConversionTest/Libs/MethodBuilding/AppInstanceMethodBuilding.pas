{ -----------------------------------------------------------------------------------------------------------------------
  Copyright © 2009 Zinsser Analytic GmbH - All rights reserved.
  Author       : Payman Kamali (pk)
  Description  :
  -----------------------------------------------------------------------------------------------------------------------
  Revision History:
  date     op  method                                track-no  improvement/change
  -------- --  ------------------------------------  --------- ----------------------------------------------------------
  27.08.09 pk                                        TN4753    create instances of RunStepInfoLib and RunStepLib
  28.09.09 pk  uRefCount                             TN4753    New reference counting
  11.09.11 wl  DestroyInstance                       TN5672   Instanzen müssen IMMER mit FreeAndNil zerstört werden!
  09.08.12 wl                                        TN5946   zusammengefasst mit AppInstanceRunStepInfoLib und AppInstanceRunStepLib
  09.08.12 wl                                        TN5946   neu: FLUSH, VSLEN, STORE, INIT
  20.09.12 wl                                        TN5982   neu: FOR
  25.06.13 wl                                        TN6178   neu: RETURN
  ----------------------------------------------------------------------------------------------------------------------- }

unit AppInstanceMethodBuilding;


interface


uses
    TypeInfo;

type
    TAppInstanceMethodBuilding = class
    private
        fStaticTypeInfos: TTypeInfoList;
        class var uInstance: TAppInstanceMethodBuilding;
        class var uRefCount: integer;
        procedure InitTypeInfos();
    private
        constructor Create();
        class property RefCount: integer read uRefCount write uRefCount;
    public
        destructor Destroy(); override;
        class procedure CreateInstance();
        class procedure DestroyInstance();
        class function Instance(): TAppInstanceMethodBuilding;
    end;


implementation


uses
    SysUtils,
    PluginLoader,
    RunStepInfoTypeDictionary,
    RunStepTypeDictionary,
    RunStepBuilderTypeDictionary,

    AddMethodRunStep,
    BlankRunStep,
    DatasetRunStep,
    DatasetRunStepInfo,
    DatasetRunStepBuilder,
    RestartSetMarkRunStep,
    FlushRunStep,
    InitRunStep,
    ParamStoreRunStepBuilder,
    VariableSetLengthRunStep,
    ReturnRunStep,
    WhileRunStep,
    ForRunStep,
    IfRunStep;

{ TAppInstanceMethodBuilding }

constructor TAppInstanceMethodBuilding.Create;
begin
    inherited Create();
    fStaticTypeInfos := TTypeInfoList.Create();
    InitTypeInfos();

    TRunStepInfoTypeDictionary.CreateInstance();
    TRunStepInfoTypeDictionary.Instance.AddTypes(fStaticTypeInfos);
    TRunStepInfoTypeDictionary.Instance.RegisterRunStepInfoTypeInfoClasses();

    TRunStepTypeDictionary.CreateInstance();
    TRunStepTypeDictionary.Instance.AddTypes(fStaticTypeInfos);
    TRunStepTypeDictionary.Instance.RegisterRunStepClasses;

    TRunStepBuilderTypeDictionary.CreateInstance();
    TRunStepBuilderTypeDictionary.Instance.AddTypes(fStaticTypeInfos);
    // TRunStepBuilderTypeDictionary.Instance.RegisterRunStepBuilderClasses();
end;

destructor TAppInstanceMethodBuilding.Destroy;
begin
    TRunStepBuilderTypeDictionary.DestroyInstance();
    TRunStepTypeDictionary.DestroyInstance();
    TRunStepInfoTypeDictionary.DestroyInstance();

    fStaticTypeInfos.Free;
    inherited;
end;

procedure TAppInstanceMethodBuilding.InitTypeInfos();
const
    cLibName = 'StaticRunSteps.bpl';
    cLibVersion = '1.0.0';
begin
    fStaticTypeInfos := TTypeInfoList.Create(true);

    fStaticTypeInfos.Add(TAddMethodRunStepInfoTypeInfo.Create(cLibName, cLibVersion));
    fStaticTypeInfos.Add(TAddMethodRunStepTypeInfo.Create(cLibName, cLibVersion));
    fStaticTypeInfos.Add(TAddMethodRunStepBuilderTypeInfo.Create(cLibName, cLibVersion));

    fStaticTypeInfos.Add(TDatasetOpenRunStepInfoTypeInfo.Create(cLibName, cLibVersion));
    fStaticTypeInfos.Add(TDatasetOpenRunStepTypeInfo.Create(cLibName, cLibVersion));
    fStaticTypeInfos.Add(TDatasetOpenRunStepBuilderTypeInfo.Create(cLibName, cLibVersion));

    fStaticTypeInfos.Add(TDatasetCloseRunStepInfoTypeInfo.Create(cLibName, cLibVersion));
    fStaticTypeInfos.Add(TDatasetCloseRunStepTypeInfo.Create(cLibName, cLibVersion));
    fStaticTypeInfos.Add(TDatasetCloseRunStepBuilderTypeInfo.Create(cLibName, cLibVersion));

    fStaticTypeInfos.Add(TDatasetCursorMoveRunStepInfoTypeInfo.Create(cLibName, cLibVersion));
    fStaticTypeInfos.Add(TDatasetCursorMoveRunStepTypeInfo.Create(cLibName, cLibVersion));
    fStaticTypeInfos.Add(TDatasetCursorMoveRunStepBuilderTypeInfo.Create(cLibName, cLibVersion));

    fStaticTypeInfos.Add(TDatasetReadRunStepInfoTypeInfo.Create(cLibName, cLibVersion));
    fStaticTypeInfos.Add(TDatasetReadRunStepTypeInfo.Create(cLibName, cLibVersion));
    fStaticTypeInfos.Add(TDatasetReadRunStepBuilderTypeInfo.Create(cLibName, cLibVersion));

    fStaticTypeInfos.Add(TRestartSetMarkRunStepInfoTypeInfo.Create(cLibName, cLibVersion));
    fStaticTypeInfos.Add(TRestartSetMarkRunStepTypeInfo.Create(cLibName, cLibVersion));
    fStaticTypeInfos.Add(TRestartSetMarkRunStepBuilderTypeInfo.Create(cLibName, cLibVersion));

    fStaticTypeInfos.Add(TWhileRunStepInfoTypeInfo.Create(cLibName, cLibVersion));
    fStaticTypeInfos.Add(TWhileRunStepTypeInfo.Create(cLibName, cLibVersion));
    fStaticTypeInfos.Add(TWhileRunStepBuilderTypeInfo.Create(cLibName, cLibVersion));

    fStaticTypeInfos.Add(TEndWhileRunStepInfoTypeInfo.Create(cLibName, cLibVersion));
    fStaticTypeInfos.Add(TEndWhileRunStepTypeInfo.Create(cLibName, cLibVersion));
    fStaticTypeInfos.Add(TEndWhileRunStepBuilderTypeInfo.Create(cLibName, cLibVersion));

    fStaticTypeInfos.Add(TForRunStepInfoTypeInfo.Create(cLibName, cLibVersion));
    fStaticTypeInfos.Add(TForRunStepTypeInfo.Create(cLibName, cLibVersion));
    fStaticTypeInfos.Add(TForRunStepBuilderTypeInfo.Create(cLibName, cLibVersion));

    fStaticTypeInfos.Add(TEndForRunStepInfoTypeInfo.Create(cLibName, cLibVersion));
    fStaticTypeInfos.Add(TEndForRunStepTypeInfo.Create(cLibName, cLibVersion));
    fStaticTypeInfos.Add(TEndForRunStepBuilderTypeInfo.Create(cLibName, cLibVersion));

    fStaticTypeInfos.Add(TIfRunStepInfoTypeInfo.Create(cLibName, cLibVersion));
    fStaticTypeInfos.Add(TIfRunStepTypeInfo.Create(cLibName, cLibVersion));
    fStaticTypeInfos.Add(TIfRunStepBuilderTypeInfo.Create(cLibName, cLibVersion));

    fStaticTypeInfos.Add(TEndIfRunStepInfoTypeInfo.Create(cLibName, cLibVersion));
    fStaticTypeInfos.Add(TEndIfRunStepTypeInfo.Create(cLibName, cLibVersion));
    fStaticTypeInfos.Add(TEndIfRunStepBuilderTypeInfo.Create(cLibName, cLibVersion));

    fStaticTypeInfos.Add(TBlankRunStepInfoTypeInfo.Create(cLibName, cLibVersion));
    fStaticTypeInfos.Add(TBlankRunStepTypeInfo.Create(cLibName, cLibVersion));
    fStaticTypeInfos.Add(TBlankRunStepBuilderTypeInfo.Create(cLibName, cLibVersion));

    fStaticTypeInfos.Add(TFlushRunStepInfoTypeInfo.Create(cLibName, cLibVersion));
    fStaticTypeInfos.Add(TFlushRunStepTypeInfo.Create(cLibName, cLibVersion));
    fStaticTypeInfos.Add(TFlushRunStepBuilderTypeInfo.Create(cLibName, cLibVersion));

    fStaticTypeInfos.Add(TInitRunStepInfoTypeInfo.Create(cLibName, cLibVersion));
    fStaticTypeInfos.Add(TInitRunStepTypeInfo.Create(cLibName, cLibVersion));
    fStaticTypeInfos.Add(TInitRunStepBuilderTypeInfo.Create(cLibName, cLibVersion));

    fStaticTypeInfos.Add(TParamStoreRunStepInfoTypeInfo.Create(cLibName, cLibVersion));
    fStaticTypeInfos.Add(TParamStoreRunStepTypeInfo.Create(cLibName, cLibVersion));
    fStaticTypeInfos.Add(TParamStoreRunStepBuilderTypeInfo.Create(cLibName, cLibVersion));

    fStaticTypeInfos.Add(TVariableSetLengthRunStepInfoTypeInfo.Create(cLibName, cLibVersion));
    fStaticTypeInfos.Add(TVariableSetLengthRunStepTypeInfo.Create(cLibName, cLibVersion));
    fStaticTypeInfos.Add(TVariableSetLengthRunStepBuilderTypeInfo.Create(cLibName, cLibVersion));

    fStaticTypeInfos.Add(TReturnRunStepInfoTypeInfo.Create(cLibName, cLibVersion));
    fStaticTypeInfos.Add(TReturnRunStepTypeInfo.Create(cLibName, cLibVersion));
    fStaticTypeInfos.Add(TReturnRunStepBuilderTypeInfo.Create(cLibName, cLibVersion));
end;

class procedure TAppInstanceMethodBuilding.CreateInstance();
begin
    if not Assigned(uInstance) then
        uInstance := TAppInstanceMethodBuilding.Create();

    Inc(uRefCount);
end;

class function TAppInstanceMethodBuilding.Instance(): TAppInstanceMethodBuilding;
begin
    result := uInstance;
end;

class procedure TAppInstanceMethodBuilding.DestroyInstance;
begin
    if uRefCount = 1 then
        FreeAndNil(uInstance);

    Dec(uRefCount);
end;


initialization


TAppInstanceMethodBuilding.RefCount := 0;


end.
