{ -----------------------------------------------------------------------------------------------------------
  Copyright © 2013 Zinsser Analytic GmbH - All rights reserved.
  Author       : Wolfgang Lyncke (wl)
  Description  :
  -----------------------------------------------------------------------------------------------------------
  Revision History:
  date     op  method                              track-no improvement/change
  -------- --  ----------------------------------  -------- -------------------------------------------------
  21.02.13 wl                                      TN6045   Initial Revision
  06.03.13 wl  OnGetPickList, OnOpenFunction       TN6103   Parameter geändert
  13.03.13 wl                                      TN5960   TViewItemsWorkflow.Instance statt statischer Methode
  ----------------------------------------------------------------------------------------------------------- }

unit DesignerEditFunctions;


interface


uses
    Layout,
    EditFunctions,
    GeneralTypes,
    CustomSetting,
    ModuleSettings;

type
    TDesignerEditFunctions = class(TEditFunctions)
    private
        function GetParamValue(aSetting: TObject): string;
    protected
        function IsCurrentLayoutEmpty(): boolean; override;
        function GetCurrentLayoutName(): string; override;
        function GetCurrentLayout(): TLayout; override;
    public
        constructor Create();
        procedure LiqParam_OpenValue(aSetting: TObject); override;
        procedure MethodName_OpenValue(aSetting: TObject); override;
        procedure SequenceName_OpenValue(aSetting: TObject); override;
        procedure SQLTermName_OpenValue(aSetting: TObject); override;
        procedure WashProgram_OpenValue(aSetting: TObject); override;
    end;


implementation


uses
    Controls,
    SysUtils,
    Carrier,
    CustomLeafSettings,
    CustomEditFunctionParams,
    ViewItem,
    ViewItemsWorkflow,
    ZADesignLayoutManager;

{ TDesignerEditFunctions }

constructor TDesignerEditFunctions.Create;
begin
    inherited Create(false);
end;

function TDesignerEditFunctions.GetCurrentLayout: TLayout;
begin
    EXIT(TZADesignLayoutManager.Instance.CurrentLayout);
end;

function TDesignerEditFunctions.GetCurrentLayoutName: string;
begin
    EXIT(TZADesignLayoutManager.Instance.CurrentLayout.Name);
end;

function TDesignerEditFunctions.IsCurrentLayoutEmpty: boolean;
begin
    EXIT(TZADesignLayoutManager.Instance.IsCurrentLayoutEmpty);
end;

function TDesignerEditFunctions.GetParamValue(aSetting: TObject): string;
begin
    ASSERT(aSetting is TCustomSetting);
    EXIT((aSetting as TCustomSetting).Value);
end;

procedure TDesignerEditFunctions.LiqParam_OpenValue(aSetting: TObject);
begin
    TViewItemsWorkflow.Instance.OpenPipetteParameter(GetParamValue(aSetting));
end;

procedure TDesignerEditFunctions.MethodName_OpenValue(aSetting: TObject);
begin
    TViewItemsWorkflow.Instance.OpenEditForm(GetParamValue(aSetting), ntMethod, true)
end;

procedure TDesignerEditFunctions.SequenceName_OpenValue(aSetting: TObject);
begin
    TViewItemsWorkflow.Instance.OpenEditForm(GetParamValue(aSetting), ntSequence, true)
end;

procedure TDesignerEditFunctions.SQLTermName_OpenValue(aSetting: TObject);
begin
    TViewItemsWorkflow.Instance.OpenEditForm(GetParamValue(aSetting), ntSQLTerm, true)
end;

procedure TDesignerEditFunctions.WashProgram_OpenValue(aSetting: TObject);
begin
    TViewItemsWorkflow.Instance.OpenEditForm(GetParamValue(aSetting), ntWashProg, true)
end;


end.
