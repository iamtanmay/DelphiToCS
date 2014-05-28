{ --------------------------------------------------------------------------------------------------
  Copyright © 2006 Zinsser Analytic GmbH - All rights reserved.
  Project      : New Editor
  Author       : Payman Kamali
  Description  : Shows a tree of all submethod calls of a method
  --------------------------------------------------------------------------------------------------
  Revision History:
  date     op  method                       track-no  improvement/change
  -------- --  ---------------------------  --------  ----------------------------------------------
  24.07.06 pk                               TN3213    initial version
  17.04.07 wl  MethodStepHasSubMethod       TN3547    benutzt TSubItemMethodStep, Memory Leak entfernt
  26.07.07 pk  MethodStepHasSubMethod       TN3805    uses NameParam field
  09.01.08 wl                               TN3972    diverse Änderungen
  02.09.08 pk                               TN4215   GetIconIndex function moved to TRunStepInfoFactory
  20.09.08 pk                               TN4215   BugFixes
  16.01.09 wl                               TN4362   an Änderungen in TViewItem angepasst
  20.08.09 wl  fStringLoader                TN4702   fStringLoader lädt Strings für Dialog-Elemente
  06.05.10 wl                               TN5052   uses EditingWorkflow
  06.05.10 wl                               TN5087   überflüssige Spalten entfernt
  20.05.10 wl                               TN5117   neue Schriftart "Segoe UI", Schriftgröße 9
  23.09.10 pk  MethodStepHasSubMethod       TN5089   Changes in TRunStepBuilderTypeDictionary
  29.09.10 pk                               TN5283   Short and Long comment combined, unneeded MethodEditor columns removed
  05.03.11 wl                               TN5472   RelatedItemParam ersetzt NameParam
  26.07.11 wl                               TN5614   ist kein TFullDockableForm mehr, Instance entfernt
  11.12.12 wl                               TN6045   uses MethodEditDataHelper
  13.03.13 wl                               TN5960   TViewItemsWorkflow.Instance statt statischer Methode
  04.04.13 wl  MethodStepHasSubMethod       TN6124   funktioniert jetzt wieder
  15.08.13 wl                               TN6223   uses geändert
  30.08.13 wl                               TN6236   an MethodDataAdaptor angepasst
  -------------------------------------------------------------------------------------------------- }

unit ViewHierarchy;


interface


uses
    Forms,
    Menus,
    Classes,
    Controls,
    cxGraphics,
    cxCustomData,
    cxStyles,
    cxTL,
    cxTextEdit,
    cxInplaceContainer,
    cxControls,
    cxLookAndFeels,
    cxLookAndFeelPainters,

    MethodDataAdaptor,
    MethodStep,
    MethodStepDataFields,
    StringLoader;

type
    TViewHierarchyStringLoader = class(TTextListStringLoader)
    protected
        procedure AddAllItems(); override;
    end;

    TfrmViewHierarchy = class(TForm)
        cxTreeList1: TcxTreeList;
        TVcxTreeListColumn1: TcxTreeListColumn;
        TVcxTreeListColumn2: TcxTreeListColumn;
        PopupMenu1: TPopupMenu;
        pmnuExpandAll: TMenuItem;
        pmnuCollapseAll: TMenuItem;
        procedure FormCreate(Sender: TObject);
        procedure FormDestroy(Sender: TObject);
        procedure cxTreeList1DblClick(Sender: TObject);
        procedure pmnuExpandAllClick(Sender: TObject);
        procedure pmnuCollapseAllClick(Sender: TObject);
        procedure FormClose(Sender: TObject; var Action: TCloseAction);
    private
        fParamDataLink: TMethodStepDataLink;
        fStringLoader: TViewHierarchyStringLoader;
        //
        function CreateMethodStepParamDataLink: TMethodStepDataLink;
        procedure MethodStepParam_OnReadData(aSender: TObject; aDataID: integer; var vValue: string);
        class function MethodStepHasSubMethod(const aCurrentRec: TMethodRec;
            aParamDataLink: TMethodStepDataLink; out oMethodName, oOptions, oAction: string): boolean;
    public
        procedure LoadMethod(const aName, aOptions, aAction: string; aParentNode: TcxTreeListNode);
    end;


implementation


{$R *.dfm}

uses
    SysUtils,
    MethodStepSettings,
    RunStepBuilderTypeDictionary,
    MethodEditDataHelper,
    ViewItem,
    CustomSetting,
    CustomLeafSettings,
    RunStepInfoFactory,
    ViewItemsWorkflow,
    ControlUtils;

{ TViewHierarchyStringLoader }

procedure TViewHierarchyStringLoader.AddAllItems;
begin
    AddSingle(44200, 'Method Hierarchy', 'Methodenhierarchie');
    AddSingle(44210, 'Expand all', 'Alle einblenden');
    AddSingle(44220, 'Collapse all', 'Alle ausblenden');
    AddSingle(44230, 'Methods', 'Methoden');
    AddSingle(44240, 'Options', 'Optionen');
end;

{ TfrmViewHierarchy }

function TfrmViewHierarchy.CreateMethodStepParamDataLink: TMethodStepDataLink;
begin
    result := TMethodStepDataLink.Create();
    result.Option := INT_METHOD_COL_SUMMARY;
    result.Comment := INT_METHOD_COL_COMMENT;
    result.OnReadData := MethodStepParam_OnReadData;
    result.OnWriteData := nil;
end;

procedure TfrmViewHierarchy.MethodStepParam_OnReadData(aSender: TObject; aDataID: integer;
    var vValue: string);
begin
end;

class function TfrmViewHierarchy.MethodStepHasSubMethod(const aCurrentRec: TMethodRec;
    aParamDataLink: TMethodStepDataLink; out oMethodName, oOptions, oAction: string): boolean;
var
    xCurrentStep: TMethodStep;
    xNameParam: TCustomSetting;
    xAction: string;
begin
    result := false;

    xAction := aCurrentRec.Action;
    if (xAction = 'ADDM') or (xAction = 'THRST') then // nicht schön, macht es aber schneller
    begin
        xCurrentStep := TRunStepBuilderTypeDictionary.Instance.CreateMethodStep(xAction, aParamDataLink,
            TNotifyEvent(nil));
        try
            if not Assigned(xCurrentStep) then
                EXIT;

            xNameParam := xCurrentStep.RelatedItemParam;
            if not(xNameParam is TCustomSetting_MethodName) then
                EXIT;

            xCurrentStep.M_Options := aCurrentRec.Options;

            // xCurrentStep.ReadData();
            oMethodName := xNameParam.Value;
            oOptions := xCurrentStep.M_Options;
            oAction := aCurrentRec.Action;
            result := true;
        finally
            xCurrentStep.Free;
        end;
    end;
end;

procedure TfrmViewHierarchy.LoadMethod(const aName, aOptions, aAction: string; aParentNode: TcxTreeListNode);
var
    xDataAdaptor: TMethodDataAdaptor;
    xMethodName: string;
    xNode: TcxTreeListNode;
    xIsMainNode: boolean;
    xOptions: string;
    xAction: string;
    xCurrentRec: TMethodRec;
begin
    if (aName = '') then
        EXIT;

    xIsMainNode := aParentNode = nil;
    if xIsMainNode then
    begin
        cxTreeList1.Clear;
        xNode := cxTreeList1.Add
    end
    else
        xNode := aParentNode.AddChild;

    xDataAdaptor := TMethodDataAdaptor.Create();
    try
        xDataAdaptor.SelectAndOpenMethod(aName, false, true);
        try
            if xDataAdaptor.DataProvider.Eof then
                EXIT;

            while not xDataAdaptor.DataProvider.Eof do
            begin
                xCurrentRec := xDataAdaptor.ReadMethodrecFromDataSet(xDataAdaptor.DataProvider);

                if MethodStepHasSubMethod(xCurrentRec, fParamDataLink, xMethodName, xOptions, xAction) then
                    LoadMethod(xMethodName, xOptions, xAction, xNode);

                xDataAdaptor.DataProvider.Next;
            end;
        finally
            xDataAdaptor.Close();
        end;

        xNode.ImageIndex := TRunStepInfoFactory.GetIconIndex(aAction);
        xNode.SelectedIndex := xNode.ImageIndex;
        xNode.Texts[0] := aName;
        xNode.Texts[1] := aOptions;
    finally
        xDataAdaptor.Free;
    end;

    if xIsMainNode then
        xNode.Expand(false);
end;

procedure TfrmViewHierarchy.FormClose(Sender: TObject; var Action: TCloseAction);
begin
    Action := caFree;
end;

procedure TfrmViewHierarchy.FormCreate(Sender: TObject);
begin
    inherited;
    TControlUtils.ResetFontForWinXP(self);
    fStringLoader := TViewHierarchyStringLoader.Create;
    fStringLoader.LoadLanguage(self);

    fParamDataLink := CreateMethodStepParamDataLink();
end;

procedure TfrmViewHierarchy.FormDestroy(Sender: TObject);
begin
    fStringLoader.Free;

    FreeAndNil(fParamDataLink);

    inherited;
end;

procedure TfrmViewHierarchy.cxTreeList1DblClick(Sender: TObject);
begin

    TViewItemsWorkflow.Instance.OpenEditForm(cxTreeList1.FocusedNode.Texts[0], ntMethod);
end;

procedure TfrmViewHierarchy.pmnuExpandAllClick(Sender: TObject);
begin
    cxTreeList1.FocusedNode.Expand(true);
end;

procedure TfrmViewHierarchy.pmnuCollapseAllClick(Sender: TObject);
begin
    cxTreeList1.FocusedNode.Collapse(true);
end;


end.
