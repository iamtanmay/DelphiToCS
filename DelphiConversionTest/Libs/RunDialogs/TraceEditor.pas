{ -----------------------------------------------------------------------------------------------------------------------
  Copyright © 2009 Zinsser Analytic GmbH - All rights reserved.
  Author       : Payman Kamali (pk)
  Description  :
  -----------------------------------------------------------------------------------------------------------------------
  Revision History:
  date     op  method                                track-no  improvement/change
  -------- --  ------------------------------------  --------- ----------------------------------------------------------
  17.02.09 pk                                        TN4232    Initial Revision
  20.02.09 pk                                        TN4232    if IsInEvent do not display
  24.02.09 pk                                        TN4232    ActionID is now int64, Actions is now TObjectList
  03.09.09 wl  cxTreeList1                           TN4800    Anpassungen an Treelist Version 5 (nicht kompatibel)
  04.11.09 pk                               	        TN4843    Replace TObjectList, TStringList, TStrings with Generics Lists and TStringArray
  24.11.09 pk  cxTreeList1CustomDrawDataCell         TN4800    Set the brushstyle to clear
  04.02.10 pk                                        TN4972    Changes for Restart
  04.15.10 pk  DisplayTrace                          TN5050    ActionID field set to -1 for last node
  23.04.10 pk                                        TN5072    Changes to TMultiListDataCacheIterator
  19.05.10 pk                                        TN5113    Changes needed for RESTA action
  19.05.10 pk  InstanceDisplayTrace                  TN5114    New Param oContinueAtDescription
  20.05.10 wl                                        TN5117   neue Schriftart "Segoe UI", Schriftgröße 9
  09.06.10 pk                                        TN5077    NextAction Node removed
  09.06.10 pk                                        TN5077    AV when RunStep is nil corrected
  21.06.10 wl                                        TN5160   Position = poScreenCenter
  26.10.10 pk                                        TN5297   Changes for ActionData segment concept
  29.10.10 pk                                        TN5279   handle case when 0 actions are found
  09.08.12 wl  GetIsSubMethodStep                    TN5946   verwendet TSubMethodRunStep
  27.03.13 wl                                        TN6045   uses geändert
  ----------------------------------------------------------------------------------------------------------------------- }

unit TraceEditor;


interface


uses
    Windows,
    Messages,
    SysUtils,
    Variants,
    Classes,
    Graphics,
    Controls,
    Forms,
    Dialogs,
    cxGraphics,
    cxCustomData,
    cxStyles,
    cxTL,
    cxTextEdit,
    cxInplaceContainer,
    cxControls,
    ExtCtrls,
    StdCtrls,
    Menus,
    Generics.Collections,
    cxLookAndFeels,
    cxLookAndFeelPainters,
    cxMemo,
    TraceManager,
    ActionData,
    ActionDataCache,
    ActionIDDataCache,
    RunStep;

type
    TfrmTraceEditor = class(TForm)
        pnlMiddle: TPanel;
        cxTreeList1: TcxTreeList;
        colStepName: TcxTreeListColumn;
        colStepDescription: TcxTreeListColumn;
        Panel2: TPanel;
        Bevel1: TBevel;
        Button2: TButton;
        btnOK: TButton;
        pnlTop: TPanel;
        colSourceLine: TcxTreeListColumn;
        pnlMiddleBottom: TPanel;
        colSourceName: TcxTreeListColumn;
        pmnuOptions: TPopupMenu;
        mnuContinueAtThisLine: TMenuItem;
        Label2: TLabel;
        colStartTime: TcxTreeListColumn;
        colFinishTime: TcxTreeListColumn;
        colActionData: TcxTreeListColumn;
        AdvancedView1: TMenuItem;
        colActionSegmentIndex: TcxTreeListColumn;
        procedure mnuContinueAtThisLineClick(Sender: TObject);
        procedure pmnuOptionsPopup(Sender: TObject);
        procedure cxTreeList1CustomDrawDataCell(Sender: TcxCustomTreeList; ACanvas: TcxCanvas;
            AViewInfo: TcxTreeListEditCellViewInfo; var ADone: Boolean);
        procedure cxTreeList1DblClick(Sender: TObject);
        procedure AdvancedView1Click(Sender: TObject);
        procedure FormCreate(Sender: TObject);
        procedure btnOKClick(Sender: TObject);
    private

        fSourceDataName: string;
        fActionsIterator: TActionListDataCacheIterator;
        fContinueAtLine: integer;
        fRestartOnlyAtMarkers: boolean;

    const
        cColIndexStepName = 0;
        cColIndexStepDescription = 1;
        cColIndexSourceMethod = 2;
        cColIndexSourceLine = 3;
        cColIndexStartTime = 4;
        cColIndexFinishTime = 5;
        cColIndexActionID = 6;
        cColIndexActionSegmentIndex = 7;

        procedure SelectLastLineAsContinueAtLine();
        procedure SelectContinueAtLine(const aRecIndex: integer);
        procedure SetContinueAtLine(aIndex: integer);
        procedure GetContinueAtInfo(out oContinueAtID: TActionID; out oContinueAtSegmentIndex: integer;
            out oContinueAtDescription: string);
        procedure ChangeView(const aAdvancedView: boolean);
        function AddChildNode(const aParentNode: TcxTreeListNode; const aActionData: TActionData;
            const aActionSegmentIndex: integer; const aStepName, aDescription: string;
            const aStartTime, aFinishTime: string): TcxTreeListNode;
        function GetIsSubMethodStep(const aActionData: TActionData): boolean;
        function GetActionDataFromNode(const aNode: TcxTreeListNode): TActionData;
        procedure AddChildNodesFromActionData(const aDisplayOnlyMarkers: boolean;
            const aParentNode: TcxTreeListNode; const aActionData: TActionData);
    public
        { Public declarations }
        procedure Init(const aSourceDataName: string; const aActionsIterator: TActionListDataCacheIterator;
            const aRestartOnlyAtMarkers: boolean);

        procedure DisplayTrace(const aDisplayOnlyMarkers: boolean);

        class function InstanceDisplayTrace(const aSourceDataName: string;
            const aActionsIterator: TActionListDataCacheIterator; const aRestartOnlyAtMarkers: boolean;
            out oContinueAtID: TActionID; out oContinueAtSegmentIndex: integer;
            out oContinueAtDescription: string): TModalResult;

    end;

    TNodeStack = class(TStack<TcxTreeListNode>);

var
    frmTraceEditor: TfrmTraceEditor;


implementation


{$R *.dfm}

uses
    ControlUtils,
    SubMethodRunStep,
    RestartSetMarkRunStep,
    GUIManager;

{ TfrmTraceEditor }

procedure TfrmTraceEditor.cxTreeList1DblClick(Sender: TObject);
begin
    if self.cxTreeList1.FocusedNode = nil then
        EXIT;

    self.SetContinueAtLine(self.cxTreeList1.FocusedNode.AbsoluteIndex);
end;

procedure TfrmTraceEditor.Init(const aSourceDataName: string;
    const aActionsIterator: TActionListDataCacheIterator; const aRestartOnlyAtMarkers: boolean);
begin
    fSourceDataName := aSourceDataName;
    fActionsIterator := aActionsIterator;
    fRestartOnlyAtMarkers := aRestartOnlyAtMarkers;
    fContinueAtLine := -1;
    self.btnOK.Enabled := false;
    self.ChangeView(self.AdvancedView1.Checked);
end;

function TfrmTraceEditor.AddChildNode(const aParentNode: TcxTreeListNode; const aActionData: TActionData;
    const aActionSegmentIndex: integer; const aStepName, aDescription: string;
    const aStartTime, aFinishTime: string): TcxTreeListNode;
var
    xCurrentNode: TcxTreeListNode;
begin
    xCurrentNode := aParentNode.AddChildFirst;
    xCurrentNode.Data := aActionData;
    // xCurrentNode.Values[cColIndexActionID]           := LongInt(aActionData);
    xCurrentNode.Values[cColIndexActionSegmentIndex] := aActionSegmentIndex;
    xCurrentNode.Values[cColIndexStepName] := aStepName;
    xCurrentNode.Values[cColIndexStepDescription] := aDescription; // xRunStep.Description;
    xCurrentNode.Values[cColIndexStartTime] := aStartTime;
    xCurrentNode.Values[cColIndexFinishTime] := aFinishTime;

    xCurrentNode.Values[cColIndexSourceMethod] := aActionData.Address.LabelName;
    xCurrentNode.Values[cColIndexSourceLine] := IntToStr(aActionData.Address.RelativeAddress);

    aParentNode.Expand(false);
    result := xCurrentNode;
end;

function TfrmTraceEditor.GetIsSubMethodStep(const aActionData: TActionData): boolean;
var
    xRunStep: TRunStep;
begin
    result := false;
    // becarefull, it is possible that xRunStep is nil (if this is the last step)
    if aActionData is TSingleSegmentActionData then
    begin

        if Assigned((aActionData as TSingleSegmentActionData).ActionSegment) then
        begin
            xRunStep := (aActionData as TSingleSegmentActionData).ActionSegment.RunStep;
            result := (xRunStep is TSubMethodRunStep);
        end;
    end;
end;

function TfrmTraceEditor.GetActionDataFromNode(const aNode: TcxTreeListNode): TActionData;
var
    xObject: TObject;
begin
    result := nil;
    if aNode.Data = nil then
        EXIT;
    xObject := aNode.Data;
    if not(xObject is TActionData) then
        EXIT;

    result := xObject as TActionData;
end;

procedure TfrmTraceEditor.AddChildNodesFromActionData(const aDisplayOnlyMarkers: boolean;
    const aParentNode: TcxTreeListNode; const aActionData: TActionData);
var
    xRunStep: TRunStep;
    xCurrentStepMainNode: TcxTreeListNode;
    xActionSegment: TActionRunEffectSegment;
    x: integer;
    xActionSegmentIndex: integer;
    xDisplayCurrentAction: boolean;
begin
    xCurrentStepMainNode := aParentNode;
    if aActionData is TMultiSegmentActionData then
    begin
        // if no segments were recorded just add a line to indicate that the prep for this step was started and must be undone
        xCurrentStepMainNode := AddChildNode(aParentNode, aActionData, -1, aActionData.StepName, '', '', '');
    end;
    // else begin
    for x := aActionData.ActionSegmentCount - 1 downto 0 do
    begin

        xActionSegment := aActionData.ActionSegments[x];
        xRunStep := xActionSegment.RunStep;
        xDisplayCurrentAction := (not aDisplayOnlyMarkers) or
            ((xRunStep is TSubMethodRunStep) or (xRunStep is TRestartSetMarkRunStep));

        if xDisplayCurrentAction then
        begin
            xActionSegmentIndex := -1;
            if aActionData.IsUndoablePerSegment then
            begin
                xActionSegmentIndex := x;
            end;
            AddChildNode(xCurrentStepMainNode, aActionData, xActionSegmentIndex, xRunStep.StepName,
                xRunStep.Description, xActionSegment.StartTime, xActionSegment.FinishTime);
        end;
    end;
end;

procedure TfrmTraceEditor.DisplayTrace(const aDisplayOnlyMarkers: boolean);
var
    xActionData: TActionData;

    xCurrentNode, xCurrentParentNode, xTempNode: TcxTreeListNode;
    xSubMethodName: string;
    xOldCursor: integer;
    xStack: TNodeStack;
    xExpand: boolean;
begin
    xOldCursor := gGUIManager.SetCursor(crHourGlass);
    try
        xStack := TNodeStack.Create();
        try
            xCurrentParentNode := nil;
            xCurrentNode := nil;

            while fActionsIterator.MoveNext do
            begin

                xActionData := fActionsIterator.Current;

                // if xActionData.Undone or xActionData.IsInEvent then begin
                if xActionData.IsInEvent then
                begin
                    CONTINUE;
                end;

                if GetIsSubMethodStep(xActionData) then
                begin
                    xExpand := true;
                    xTempNode := nil;
                    if xStack.Count > 0 then
                    begin
                        xTempNode := xStack.Peek;
                        if Assigned(xTempNode) then
                        begin
                            xSubMethodName := xTempNode.Values[cColIndexStepName];
                            if SameText(xActionData.Address.LabelName, xSubMethodName) then
                            begin
                                xExpand := false;
                                xStack.Pop;
                            end
                            else
                            begin
                                xTempNode := nil;
                            end;
                        end;
                    end;

                    if not Assigned(xTempNode) then
                    begin
                        xTempNode := TcxTreeListNode.Create(self.cxTreeList1);
                        xTempNode.Values[cColIndexStepName] := xActionData.Address.LabelName;
                    end;

                    if Assigned(xCurrentParentNode) then
                    begin

                        self.cxTreeList1.AddNode(xCurrentParentNode, xTempNode, nil, tlamAddChildFirst);
                        if xExpand then
                            xCurrentParentNode.Expand(false)
                        else
                            xCurrentParentNode.Collapse(false);
                    end;

                    xCurrentParentNode := xTempNode;
                    CONTINUE;
                end;

                if Assigned(xCurrentParentNode) then
                begin

                    xSubMethodName := xCurrentParentNode.Values[cColIndexStepName];
                    if not SameText(xActionData.Address.LabelName, xSubMethodName) then
                    begin
                        xStack.Push(xCurrentParentNode);
                        xCurrentParentNode := nil;
                    end;
                end;

                if not Assigned(xCurrentParentNode) then
                begin
                    xCurrentParentNode := TcxTreeListNode.Create(self.cxTreeList1);
                    xCurrentParentNode.Values[cColIndexStepName] := xActionData.Address.LabelName;
                end;

                AddChildNodesFromActionData(aDisplayOnlyMarkers, xCurrentParentNode, xActionData);

            end;

            if Assigned(xCurrentParentNode) then
            begin
                self.cxTreeList1.AddNode(xCurrentParentNode, self.cxTreeList1.Root, nil, tlamAddChildFirst);
                xCurrentParentNode.Expand(false);
            end;

            self.cxTreeList1.TopVisibleNode := xCurrentNode;

        finally
            FreeAndNil(xStack);
        end;

    finally
        gGUIManager.SetCursor(xOldCursor);
    end;
end;

procedure TfrmTraceEditor.FormCreate(Sender: TObject);
begin
    TControlUtils.ResetFontForWinXP(self);
end;

procedure TfrmTraceEditor.SelectContinueAtLine(const aRecIndex: integer);
begin
    SetContinueAtLine(aRecIndex);
end;

procedure TfrmTraceEditor.SelectLastLineAsContinueAtLine();
begin
    SelectContinueAtLine(self.cxTreeList1.AbsoluteCount - 1);
end;

procedure TfrmTraceEditor.SetContinueAtLine(aIndex: integer);
var
    xNode: TcxTreeListNode;
    xActionData: TActionData;
    xActionSegment: TActionRunEffectSegment;
    xActionSegmentIndex: integer;
begin
    if aIndex < 0 then
        EXIT;

    xNode := self.cxTreeList1.AbsoluteItems[aIndex];
    if fRestartOnlyAtMarkers then
    begin
        if (aIndex >= (self.cxTreeList1.AbsoluteCount - 1)) then
            EXIT;
        if (xNode.Values[cColIndexStepName] <> cActionNameRestartSetMark) then
            EXIT;
    end;

    xActionData := self.GetActionDataFromNode(xNode);
    xActionSegmentIndex := xNode.Values[cColIndexActionSegmentIndex];
    if Assigned(xActionData) and (xActionSegmentIndex >= 0) and (xActionData.IsUndoablePerSegment) then
    begin
        xActionSegment := xActionData.ActionSegments[xActionSegmentIndex];
        if not xActionSegment.RunStep.RestartAtStepAllowed then
            EXIT;
    end;

    fContinueAtLine := aIndex;
    self.btnOK.Enabled := true;
    self.cxTreeList1.Repaint;
end;

procedure TfrmTraceEditor.mnuContinueAtThisLineClick(Sender: TObject);
begin
    self.SetContinueAtLine(self.cxTreeList1.FocusedNode.AbsoluteIndex);
end;

procedure TfrmTraceEditor.AdvancedView1Click(Sender: TObject);
begin
    ChangeView(self.AdvancedView1.Checked);
end;

procedure TfrmTraceEditor.btnOKClick(Sender: TObject);
begin
    if fContinueAtLine = -1 then
        self.ModalResult := mrNone;

end;

procedure TfrmTraceEditor.ChangeView(const aAdvancedView: boolean);
begin
    // self.cxTreeList1.Columns[ cColIndexStepName ].Visible       := aAdvancedView;
    self.cxTreeList1.Columns[cColIndexSourceMethod].Visible := aAdvancedView;
    self.cxTreeList1.Columns[cColIndexSourceLine].Visible := aAdvancedView;
end;

procedure TfrmTraceEditor.cxTreeList1CustomDrawDataCell(Sender: TcxCustomTreeList; ACanvas: TcxCanvas;
    AViewInfo: TcxTreeListEditCellViewInfo; var ADone: Boolean);
begin
    if not Assigned(aViewInfo.Node) then
        EXIT;

    if not aViewInfo.Node.Selected then
        aCanvas.Brush.Style := bsClear;

    if aViewInfo.Node.Values[cColIndexStepName] = cActionNameRestartSetMark then
    begin
        aCanvas.Brush.Style := bsSolid;
        aCanvas.Brush.Color := clSkyBlue;
        aCanvas.Font.Color := clBlack;
        if aViewInfo.Node.Focused then
        begin
            aCanvas.Font.Color := clBlack;
        end;
    end;

    if aViewInfo.Node.AbsoluteIndex = fContinueAtLine then
    begin
        aCanvas.Brush.Style := bsSolid;
        aCanvas.Brush.Color := clMoneyGreen;
        aCanvas.Font.Color := clBlack;
        if aViewInfo.Node.Focused then
        begin
            aCanvas.Font.Color := clBlue;
        end;
        // aCanvas.Font.Color := clDkG;
    end;
end;

procedure TfrmTraceEditor.GetContinueAtInfo(out oContinueAtID: TActionID;
    out oContinueAtSegmentIndex: integer; out oContinueAtDescription: string);
var
    xContinueAtNode: TcxTreeListNode;
begin
    ASSERT((fContinueAtLine >= 0) and (fContinueAtLine < (self.cxTreeList1.AbsoluteCount)));

    xContinueAtNode := self.cxTreeList1.AbsoluteItems[fContinueAtLine];
    oContinueAtID := self.GetActionDataFromNode(xContinueAtNode).ActionID;
    oContinueAtSegmentIndex := xContinueAtNode.Values[cColIndexActionSegmentIndex];
    oContinueAtDescription := xContinueAtNode.Values[cColIndexStepDescription];

end;

class function TfrmTraceEditor.InstanceDisplayTrace(const aSourceDataName: string;
    const aActionsIterator: TActionListDataCacheIterator; const aRestartOnlyAtMarkers: boolean;
    out oContinueAtID: TActionID; out oContinueAtSegmentIndex: integer; out oContinueAtDescription: string)
    : TModalResult;
var
    xEditor: TfrmTraceEditor;
begin
    xEditor := TfrmTraceEditor.Create(nil);
    try
        oContinueAtID := 0;
        oContinueAtDescription := '';
        oContinueAtSegmentIndex := -1;

        xEditor.Init(aSourceDataName, aActionsIterator, aRestartOnlyAtMarkers);
        xEditor.DisplayTrace(aRestartOnlyAtMarkers);
        xEditor.SelectLastLineAsContinueAtLine();
        result := xEditor.ShowModal;
        if result = mrOK then
        begin
            xEditor.GetContinueAtInfo(oContinueAtID, oContinueAtSegmentIndex, oContinueAtDescription);
        end;
    finally
        xEditor.Free;
    end;
end;

procedure TfrmTraceEditor.pmnuOptionsPopup(Sender: TObject);
begin
    self.mnuContinueAtThisLine.Enabled := false;
    if self.cxTreeList1.FocusedNode = nil then
        EXIT;
    self.mnuContinueAtThisLine.Enabled := self.cxTreeList1.FocusedNode.AbsoluteIndex <> 0;
end;


end.
