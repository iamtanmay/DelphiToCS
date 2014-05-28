unit EditingLayoutManager;
{ ------------------------------------------------------------------------------------------------------------
  Copyright © 2010 Zinsser Analytic GmbH - All rights reserved.
  Author       : Wolfgang Lyncke (wl)
  Description  :
  ------------------------------------------------------------------------------------------------------------
  Revision History:
  date     op  method                            track-no improvement/change
  -------- --  --------------------------------  -------- ----------------------------------------------------
  07.06.10 wl                                    TN5116   initial revision
  09.12.10 wl  MainNodeIs..                      TN5257   neu für schnelles Insert von racks
  ------------------------------------------------------------------------------------------------------------ }


interface


uses
    Carrier,
    Rack,
    Layout,
    LayoutEditingFunctionProvider,
    EditingLayoutElements,
    CustomLayoutManager,
    Controls;

type
    TDesignerEditingFunctionProvider = class(TLayoutEditingFunctionProvider)
    protected
        function GetNotSaved: boolean; override;
        procedure SetNotSaved(const Value: boolean); override;
        function GetCurrentLayout: TLayout; override;
        function GetCurrentSetupLayout: TSetupLayout; override;
    public
        function DragSourceIsNode(aSource: TObject): boolean; override;
        function DragSourceIsCarrierNode(aSource: TObject; out oCarrierName: string): boolean; override;
        function DragSourceIsLayoutNode(aSource: TObject; out oLayoutName: string): boolean; override;
        function DragSourceIsRackNode(aSource: TObject; out oRackName: string): boolean; override;
        function DragSourceIsWorkspaceNode(aSource: TObject; out oWorkspaceName: string): boolean; override;
        function MainNodeIsCarrierNode(out oCarrierName: string): boolean; override;
        function MainNodeIsRackNode(out oRackName: string): boolean; override;
        function MainNodeIsWorkspaceNode(out oWorkspaceName: string): boolean; override;
        function EvaluationStart(aCarrier: TCarrier; aRack: TRack): TCarrierEvaluation; override;
        procedure SceneChanged(); override;
        procedure Load(); override;
    end;

    TEditingLayoutManager = class sealed(TCustomLayoutManager)
    private
        class var uInstance: TEditingLayoutManager;
    protected
        function DoCreateLayout(const aRunName, aLayoutName: string): TLayout; override;
        function GetTempSettingsSectionName: string; override;
    public
        constructor Create(aBackgroundGraphicsParent: TWinControl);
        class procedure CreateInstance();
        class procedure DestroyInstance();
        class property Instance: TEditingLayoutManager read uInstance;
    end;


implementation


uses
    SysUtils,
    cxTL,
    MethodTypes,
    ViewItemsWorkflow;

{ TDesignerEditingFunctionProvider }

function TDesignerEditingFunctionProvider.GetCurrentLayout: TLayout;
begin
    result := TEditingLayoutManager.Instance.CurrentLayout;
end;

function TDesignerEditingFunctionProvider.GetCurrentSetupLayout: TSetupLayout;
begin
    result := TEditingLayoutManager.Instance.CurrentLayout as TSetupLayout;
end;

function TDesignerEditingFunctionProvider.GetNotSaved: boolean;
begin
    result := false;
end;

procedure TDesignerEditingFunctionProvider.Load;
begin
    TEditingLayoutManager.Instance.Load();
end;

procedure TDesignerEditingFunctionProvider.SceneChanged;
begin
    TEditingLayoutManager.Instance.SceneChanged();
end;

procedure TDesignerEditingFunctionProvider.SetNotSaved(const Value: boolean);
begin

end;

function TDesignerEditingFunctionProvider.MainNodeIsCarrierNode(out oCarrierName: string): boolean;
begin
    result := false;
    oCarrierName := '';
end;

function TDesignerEditingFunctionProvider.MainNodeIsRackNode(out oRackName: string): boolean;
begin
    result := false;
    oRackName := '';
end;

function TDesignerEditingFunctionProvider.MainNodeIsWorkspaceNode(out oWorkspaceName: string): boolean;
begin
    result := false;
    oWorkspaceName := '';
end;

function TDesignerEditingFunctionProvider.DragSourceIsCarrierNode(aSource: TObject;
    out oCarrierName: string): boolean;
begin
    if (aSource is TcxTreeList) and Assigned((aSource as TcxTreeList).DragNode) and
        ((aSource as TcxTreeList).DragNode.StateIndex = INT_IM_INDEX_CARRIER) then
    begin
        result := true;
        oCarrierName := (aSource as TcxTreeList).DragNode.Texts[0];
    end
    else
        result := false;
end;

function TDesignerEditingFunctionProvider.DragSourceIsLayoutNode(aSource: TObject;
    out oLayoutName: string): boolean;
begin
    if (aSource is TcxTreeList) and Assigned((aSource as TcxTreeList).DragNode) and
        ((aSource as TcxTreeList).DragNode.StateIndex = INT_IM_INDEX_LAYOUT) then
    begin
        result := true;
        oLayoutName := (aSource as TcxTreeList).DragNode.Texts[0];
    end
    else
        result := false;
end;

function TDesignerEditingFunctionProvider.DragSourceIsNode(aSource: TObject): boolean;
begin
    result := (aSource is TcxTreeList) and Assigned((aSource as TcxTreeList).DragNode);
end;

function TDesignerEditingFunctionProvider.DragSourceIsRackNode(aSource: TObject;
    out oRackName: string): boolean;
begin
    if (aSource is TcxTreeList) and Assigned((aSource as TcxTreeList).DragNode) and
        ((aSource as TcxTreeList).DragNode.StateIndex = INT_IM_INDEX_RACK) then
    begin
        result := true;
        oRackName := (aSource as TcxTreeList).DragNode.Texts[0];
    end
    else
        result := false;
end;

function TDesignerEditingFunctionProvider.DragSourceIsWorkspaceNode(aSource: TObject;
    out oWorkspaceName: string): boolean;
begin
    if (aSource is TcxTreeList) and Assigned((aSource as TcxTreeList).DragNode) and
        ((aSource as TcxTreeList).DragNode.StateIndex = INT_IM_INDEX_WORKSPACE) then
    begin
        result := true;
        oWorkspaceName := (aSource as TcxTreeList).DragNode.Texts[0];
    end
    else
        result := false;
end;

function TDesignerEditingFunctionProvider.EvaluationStart(aCarrier: TCarrier; aRack: TRack)
    : TCarrierEvaluation;
begin
    result := nil;
end;

{ TEditingLayoutManager }

constructor TEditingLayoutManager.Create(aBackgroundGraphicsParent: TWincontrol);
begin
    inherited Create(aBackgroundGraphicsParent);
    TLayoutEditingFunctionProvider.SetInstance(TDesignerEditingFunctionProvider.Create);
end;

class procedure TEditingLayoutManager.DestroyInstance;
begin
    FreeAndNil(uInstance);
end;

class procedure TEditingLayoutManager.CreateInstance();
begin
    uInstance := TEditingLayoutManager.Create(nil);
end;

function TEditingLayoutManager.DoCreateLayout(const aRunName, aLayoutName: string): TLayout;
begin
    result := TSetupLayout.Create(aLayoutName, aRunName);
end;

function TEditingLayoutManager.GetTempSettingsSectionName: string;
begin
    result := 'Layout_Editing';
end;


end.
