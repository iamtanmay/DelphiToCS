unit LayouterLayoutManager;
{ --------------------------------------------------------------------------------------------------
  Copyright © 2008 Zinsser Analytic GmbH - All rights reserved.
  Author       : Payman Kamali (pk)
  Description  : Layout Manager for the Layouter.exe
  --------------------------------------------------------------------------------------------------
  Revision History:
  date     op  method                       track-no improvement/change
  -------- --  ---------------------------  -------- -----------------------------------------------
  23.06.08 pk                                TN4139  Initial Revision
  04.08.10 pk  GetTempSettingsSectionName    TN5043  New: a different settings section name for each layoutmanager
  23.04.10 wl  TCarrierEvaluation            TN5070  neu, um Carrier Evaluation-Fenster zu kapseln
  23.04.10 wl  DragSourceIs..                TN5070  für DragDrop-Aktionen im Layout-Editor
  30.04.10 wl  EvaluationStart               TN5070  Rack als neuer Parameter
  07.06.10 wl                                TN5116   Layout editieren vorbereiten
  09.12.10 wl  MainNodeIs..                  TN5257   neu für schnelles Insert von racks
  -------------------------------------------------------------------------------------------------- }


interface


uses
    Controls,
    LayoutManager,
    Layout,
    Carrier,
    Rack,
    EditingLayoutElements,
    LayoutEditingFunctionProvider;

type
    TLayoutManagerMode = (lmmNone, lmmTest, lmmRack, lmmEdit);

    TLayouterFunctionProvider = class(TLayoutEditingFunctionProvider)
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

    TLayouterLayoutManager = class(TLayoutManager)
    private
        fMode: TLayoutManagerMode;
    protected
        function DoCreateLayout(const aRunName, aLayoutName: string): TLayout; override;
        procedure SetCaption(const aRunName, aLayoutName: string); override;
        function GetTempSettingsSectionName: string; override;
    public
        constructor Create(aBackgroundGraphicsParent: TWinControl);

        procedure ChangeLayoutMode(aMode: TLayoutManagerMode);
    end;


implementation


uses
    ComCtrls,
    LObjMove,
    LObjTest,
    LayGlobe,
    LayMain,
    LayEvalu;

{ TLayouterLayoutManager }

procedure TLayouterLayoutManager.ChangeLayoutMode(aMode: TLayoutManagerMode);
begin
    fMode := aMode;
end;

constructor TLayouterLayoutManager.Create(aBackgroundGraphicsParent: TWinControl);
begin
    inherited Create(aBackgroundGraphicsParent);
    TLayoutEditingFunctionProvider.SetInstance(TLayouterFunctionProvider.Create);
end;

function TLayouterLayoutManager.DoCreateLayout(const aRunName, aLayoutName: string): TLayout;
begin
    result := nil;
    if fMode = lmmTest then
        result := TTestLayout.Create(aLayoutName, aRunName)
    else if fMode = lmmRack then
        result := TMoveLayout.Create(aLayoutName, aRunName)
    else if fMode = lmmEdit then
        result := TSetupLayout.Create(aLayoutName, aRunName)

end;

function TLayouterLayoutManager.GetTempSettingsSectionName: string;
begin
    result := 'Layout_Layouter';
end;

procedure TLayouterLayoutManager.SetCaption(const aRunName, aLayoutName: string);
begin
    inherited;
end;

{ TLayouterFunctionProvider }

function TLayouterFunctionProvider.DragSourceIsCarrierNode(aSource: TObject;
    out oCarrierName: string): boolean;
begin
    result := ((aSource as TTreeView).Selected.Parent = Main.CarrierNode);
    oCarrierName := (aSource as TTreeView).Selected.Text;
end;

function TLayouterFunctionProvider.DragSourceIsLayoutNode(aSource: TObject; out oLayoutName: string): boolean;
begin
    result := ((aSource as TTreeView).Selected.Parent = Main.LayoutNode);
    oLayoutName := (aSource as TTreeView).Selected.Text;
end;

function TLayouterFunctionProvider.DragSourceIsNode(aSource: TObject): boolean;
begin
    result := (aSource is TTreeView);
end;

function TLayouterFunctionProvider.DragSourceIsRackNode(aSource: TObject; out oRackName: string): boolean;
begin
    result := ((aSource as TTreeView).Selected.Parent = Main.RackNode);
    oRackName := (aSource as TTreeView).Selected.Text;
end;

function TLayouterFunctionProvider.DragSourceIsWorkspaceNode(aSource: TObject;
    out oWorkspaceName: string): boolean;
begin
    result := ((aSource as TTreeView).Selected.Parent = Main.WorkspaceNode);
    oWorkspaceName := (aSource as TTreeView).Selected.Text;
end;

function TLayouterFunctionProvider.EvaluationStart(aCarrier: TCarrier; aRack: TRack): TCarrierEvaluation;
begin
    result := TCarrierEvaluationWithMachine.Create;
    result.EvaluationStart(aCarrier, aRack);
end;

function TLayouterFunctionProvider.GetCurrentLayout: TLayout;
begin
    result := TLayoutManager.Instance.CurrentLayout;
end;

function TLayouterFunctionProvider.GetCurrentSetupLayout: TSetupLayout;
begin
    result := TLayoutManager.Instance.CurrentLayout as TSetupLayout;
end;

function TLayouterFunctionProvider.GetNotSaved: boolean;
begin
    result := Main.sbLSave.Enabled;
end;

procedure TLayouterFunctionProvider.Load;
begin
    TLayoutManager.Instance.Load();
end;

function TLayouterFunctionProvider.MainNodeIsCarrierNode(out oCarrierName: string): boolean;
begin
    result := Main.TV.Selected.Parent = Main.CarrierNode;
    oCarrierName := main.TV.Selected.Text;
end;

function TLayouterFunctionProvider.MainNodeIsRackNode(out oRackName: string): boolean;
begin
    result := Main.TV.Selected.Parent = Main.RackNode;
    oRackName := main.TV.Selected.Text;
end;

function TLayouterFunctionProvider.MainNodeIsWorkspaceNode(out oWorkspaceName: string): boolean;
begin
    result := Main.TV.Selected.Parent = Main.WorkspaceNode;
    oWorkspaceName := main.TV.Selected.Text;
end;

procedure TLayouterFunctionProvider.SceneChanged;
begin
    TLayoutManager.Instance.SceneChanged();
end;

procedure TLayouterFunctionProvider.SetNotSaved(const Value: boolean);
begin
    Main.sbLSave.Enabled := Value;
end;


end.
