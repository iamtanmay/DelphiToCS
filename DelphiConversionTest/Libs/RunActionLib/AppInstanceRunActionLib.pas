{ -----------------------------------------------------------------------------------------------------------------------
  Copyright © 2008 Zinsser Analytic GmbH - All rights reserved.
  Author       : Payman Kamali (pk)
  Description  :
  -----------------------------------------------------------------------------------------------------------------------
  Revision History:
  date     op  method                                track-no  improvement/change
  -------- --  ------------------------------------  --------- ----------------------------------------------------------
  09.12.08 pk  fStaticTypeInfos                      TN4279      New
  17.02.09 pk  InitTypeInfos                         TN4232      Add DatasetRunActions
  23.06.09 pk  Creat                                 TN4538      create TAppInstanceRunControlExt
  04.02.10 pk                                        TN4972      Changes for Restart
  19.05.10 pk  InitTypeInfos                         TN5113      Add RestartSetMarkRunAction
  22.06.10 pk  InitTypeInfos                         TN5088      New While, If Actions
  22.06.10 pk  InitTypeInfos                         TN5088      BuildCalc Action removed
  09.08.12 wl                                        TN5946   neu: FLUSH, VSLEN, STORE, INIT
  20.09.12 wl                                        TN5982   neu: FOR
  25.06.13 wl                                        TN6178   neu: RETURN
  ----------------------------------------------------------------------------------------------------------------------- }

unit AppInstanceRunActionLib;


interface


uses
    TypeInfo;

type
    TAppInstanceRunActionLib = class
    private
        fStaticTypeInfos: TTypeInfoList;
        constructor Create();
        procedure InitTypeInfos;
    public
        destructor Destroy(); override;
        class function CreateInstance(): TAppInstanceRunActionLib;
        class procedure DestroyInstance();
        class function Instance(): TAppInstanceRunActionLib;
    end;


implementation


uses
    RunActionTypeDictionary,
    RunStepTranslatorTypeDictionary,
    AppInstanceRunControlExt,
    AddMethodRunAction,
    DatasetRunAction,
    RestartSetMarkRunAction,
    IfRunAction,
    ReturnRunAction,
    InitRunAction,
    FlushRunAction,
    ParamStoreRunAction,
    VariableSetLengthRunAction,
    ForRunAction,
    WhileRunAction;

var
    uInstRunActionLib: TAppInstanceRunActionLib;

const
    cLibNameRunAction = 'RunActionLib.bpl';
    cLibVersionRunAction = '1.0.0';

    { TAppInstanceRunActionLib }

constructor TAppInstanceRunActionLib.Create;
begin
    inherited Create();
    TAppInstanceRunControlExt.CreateInstance();

    fStaticTypeInfos := TTypeInfoList.Create();
    InitTypeInfos();

    TRunActionTypeDictionary.CreateInstance();
    TRunActionTypeDictionary.Instance.AddTypes(fStaticTypeInfos);

    TRunStepTranslatorTypeDictionary.CreateInstance();

end;

destructor TAppInstanceRunActionLib.Destroy;
begin
    TRunStepTranslatorTypeDictionary.DestroyInstance();

    TRunActionTypeDictionary.DestroyInstance();
    fStaticTypeInfos.Free;

    TAppInstanceRunControlExt.DestroyInstance();
    inherited;
end;

procedure TAppInstanceRunActionLib.InitTypeInfos();
begin
    fStaticTypeInfos.Add(TAddMethodRunActionTypeInfo.Create(cLibNameRunAction, cLibVersionRunAction));
    fStaticTypeInfos.Add(TDatasetOpenRunActionTypeInfo.Create(cLibNameRunAction, cLibVersionRunAction));
    fStaticTypeInfos.Add(TDatasetCloseRunActionTypeInfo.Create(cLibNameRunAction, cLibVersionRunAction));
    fStaticTypeInfos.Add(TDatasetReadRunActionTypeInfo.Create(cLibNameRunAction, cLibVersionRunAction));
    fStaticTypeInfos.Add(TDatasetCursorMoveRunActionTypeInfo.Create(cLibNameRunAction, cLibVersionRunAction));
    fStaticTypeInfos.Add(TRestartSetMarkRunActionTypeInfo.Create(cLibNameRunAction, cLibVersionRunAction));
    fStaticTypeInfos.Add(TWhileRunActionTypeInfo.Create(cLibNameRunAction, cLibVersionRunAction));
    fStaticTypeInfos.Add(TEndWhileRunActionTypeInfo.Create(cLibNameRunAction, cLibVersionRunAction));
    fStaticTypeInfos.Add(TForRunActionTypeInfo.Create(cLibNameRunAction, cLibVersionRunAction));
    fStaticTypeInfos.Add(TEndForRunActionTypeInfo.Create(cLibNameRunAction, cLibVersionRunAction));
    fStaticTypeInfos.Add(TIfRunActionTypeInfo.Create(cLibNameRunAction, cLibVersionRunAction));
    fStaticTypeInfos.Add(TEndIfRunActionTypeInfo.Create(cLibNameRunAction, cLibVersionRunAction));
    fStaticTypeInfos.Add(TFlushRunActionTypeInfo.Create(cLibNameRunAction, cLibVersionRunAction));
    fStaticTypeInfos.Add(TParamStoreRunActionTypeInfo.Create(cLibNameRunAction, cLibVersionRunAction));
    fStaticTypeInfos.Add(TVariableSetLengthRunActionTypeInfo.Create(cLibNameRunAction, cLibVersionRunAction));
    fStaticTypeInfos.Add(TInitRunActionTypeInfo.Create(cLibNameRunAction, cLibVersionRunAction));
    fStaticTypeInfos.Add(TReturnRunActionTypeInfo.Create(cLibNameRunAction, cLibVersionRunAction));
end;

class function TAppInstanceRunActionLib.CreateInstance(): TAppInstanceRunActionLib;
begin
    // create instance if instance does not exist
    if not Assigned(uInstRunActionLib) then
        uInstRunActionLib := TAppInstanceRunActionLib.Create();

    // return instance
    result := uInstRunActionLib;
end;

class function TAppInstanceRunActionLib.Instance(): TAppInstanceRunActionLib;
begin
    result := uInstRunActionLib;
end;

class procedure TAppInstanceRunActionLib.DestroyInstance;
begin
    uInstRunActionLib.Free;
end;


end.
