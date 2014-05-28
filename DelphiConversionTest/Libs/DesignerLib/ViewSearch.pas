{ --------------------------------------------------------------------------------------------------
  Copyright © 2006 Zinsser Analytic GmbH - All rights reserved.
  Project      : New Editor
  Author       : Payman Kamali
  Description  : Shows a tree of all methods which contain the given text
  Each method node contains subnodes of instances of the given text
  --------------------------------------------------------------------------------------------------
  Revision History:
  date     op  method                       track-no  improvement/change
  -------- --  ---------------------------  --------  ----------------------------------------------
  26.07.06 pk                                TN3218   initial version
  19.12.06 wl                                TN3409   cast auf TDockableEditForm entfernt
  24.07.07 pk  AddNodesFromRec               TN3801   A search for a single char caused endless loop: xPos := xPos + xSearchTextLen.  - 1 removed.
  09.01.08 wl  FindInMethods                 TN3972   sucht nur noch in Action- und Options-Feld
  02.09.08 pk                                TN4125   GetIconIndex function moved to TRunStepInfoFactory
  16.01.09 wl                                TN4362   an Änderungen in TViewItem angepasst
  16.06.09 wl  FindInMethods                 TN4605   benutzt ReadAllNames statt ReadAllmethodNames
  10.08.09 wl                                TN4702   Strings werden jetzt direkt geladen
  20.08.09 wl                                TN4702   Strings werden jetzt direkt geladen
  03.09.09 wl  cxTreeList1                   TN4800   Anpassungen an Treelist Version 5 (nicht kompatibel)
  10.05.10 ts  cxTreeList1DblClick           TN5099   wenn Fenster leer ist, dann soll nichts passieren (sonst AV)
  20.05.10 wl                                TN5117   neue Schriftart "Segoe UI", Schriftgröße 9
  29.09.10 pk                                TN5283   Short and Long comment combined, unneeded MethodEditor columns removed
  30.09.10 pk                                TN5287   Search in methods reimplemented
  26.07.11 wl                                TN5614   ist kein TFullDockableForm mehr, Instance entfernt
  26.08.11 wl  TSQLTermSearchData            TN5670   Neu: Jetzt wird auch in SQL-Statements gesucht
  10.12.12 wl                                TN6045   verwendet ActionImages
  11.12.12 wl                                TN6045   uses MethodEditDataHelper
  13.03.13 wl                                TN5960   TViewItemsWorkflow.Instance statt statischer Methode
  -------------------------------------------------------------------------------------------------- }

unit ViewSearch;


interface


uses
    Forms,
    Classes,
    Controls,
    Generics.Collections,
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
    SQLTermsDataAdaptor,
    MethodEditDataHelper;

type
    TSearchData = class abstract
    private
        fRowIndex: integer;
        fColIndex: integer;
        fSettingKey: TArray<string>;
        fPos: integer;
        fBefore: string;
        fSearch: string;
        fAfter: string;
    strict protected
        function GetMainText: string; virtual; abstract;
        function GetActionText: string; virtual; abstract;
    public
        constructor Create();
        procedure SetTexts(aText: string; aPos, aSearchTextLen: integer);
        property RowIndex: integer read fRowIndex write fRowIndex;
        property ColIndex: integer read fColIndex write fColIndex;
        property SettingKey: TArray<string>read fSettingKey write fSettingKey;
        property Pos: integer read fPos write fPos;
        property Before: string read fBefore write fBefore;
        property Search: string read fSearch write fSearch;
        property After: string read fAfter write fAfter;
        property MainText: string read GetMainText;
        property ActionText: string read GetActionText;
    end;

    TMethodSearchData = class(TSearchData)
    private
        fRec: TMethodRec;
    strict protected
        function GetMainText: string; override;
        function GetActionText: string; override;
    public
        constructor Create(const aMethodRec: TMethodRec);
    end;

    TSQLTermSearchData = class(TSearchData)
    private
        fRec: TSQLTermRec;
    strict protected
        function GetMainText: string; override;
        function GetActionText: string; override;
    public
        constructor Create(const aRec: TSQLTermRec);
    end;

    TfrmViewSearch = class(TForm)
        cxTreeList1: TcxTreeList;
        TVcxTreeListColumn1: TcxTreeListColumn;
        TVcxTreeListColumn2: TcxTreeListColumn;
        TVcxTreeListColumn3: TcxTreeListColumn;
        procedure FormCreate(Sender: TObject);
        procedure FormDestroy(Sender: TObject);
        procedure cxTreeList1DblClick(Sender: TObject);
        procedure cxTreeList1CustomDrawDataCell(Sender: TcxCustomTreeList; ACanvas: TcxCanvas;
            AViewInfo: TcxTreeListEditCellViewInfo; var ADone: Boolean);
        procedure FormClose(Sender: TObject; var Action: TCloseAction);
    private
        fSearchText: string;
        fSearchDataList: TObjectList<TSearchData>;
        fMethodEditDataHelper: TMethodEditDataHelper;
        procedure AddNodesFromMethodRec(var vParentNode: TcxTreeListNode; const aMethodRec: TMethodRec;
            aMethodRow: integer);
        procedure AddNodesFromSQLTermRec(var vParentNode: TcxTreeListNode; const aRec: TSQLTermRec);
        procedure AddNode(var vParentNode: TcxTreeListNode; aSearchData: TSearchData;
            const aMethodRow: integer; const aText: string; const aFoundPos: integer;
            const aKeyArray: TArray<string>; aImageIndex1, aImageIndex2: integer);
        procedure FindInMethods();
        procedure FindInSqlTerms();
    private const
        INT_COL_METHOD = 0;
        INT_COL_LINE = 1;
        INT_COL_OPTION = 2;
    public
        procedure SearchFor(const aSearchText: string);
    end;


implementation


{$R *.dfm}

uses
    Windows,
    Graphics,
    SysUtils,
    StrUtils,
    GeneralTypes,
    ControlUtils,
    MethodStep,
    CustomSetting,
    MethodTypes,
    ViewItem,
    RunStepInfoFactory,
    ViewItemsWorkflow,
    ViewItemEditForm;

{ TSearchData }

constructor TSearchData.Create();
begin
    inherited Create;

    fPos := 0;
    fRowIndex := -1;
    fColIndex := -1;
    fBefore := '';
    fSearch := '';
    fAfter := '';
    SetLength(fSettingKey, 0);
end;

procedure TSearchData.SetTexts(aText: string; aPos: integer; aSearchTextLen: integer);
begin
    fPos := aPos;
    fBefore := Copy(aText, 1, aPos - 1);
    fSearch := Copy(aText, aPos, aSearchTextLen);
    fAfter := Copy(aText, aPos + aSearchTextLen, Length(aText));
end;

{ TMethodSearchData }

constructor TMethodSearchData.Create(const aMethodRec: TMethodRec);
begin
    inherited Create;

    fRec := aMethodRec;
end;

function TMethodSearchData.GetActionText: string;
begin
    EXIT(fRec.Action);
end;

function TMethodSearchData.GetMainText: string;
begin
    EXIT(fRec.name);
end;

{ TSQLTermSearchData }

constructor TSQLTermSearchData.Create(const aRec: TSQLTermRec);
begin
    inherited Create;

    fRec := aRec;
end;

function TSQLTermSearchData.GetActionText: string;
begin
    EXIT(fRec.Term);
end;

function TSQLTermSearchData.GetMainText: string;
begin
    EXIT(fRec.name);
end;

{ TfrmViewSearch }

procedure TfrmViewSearch.FormClose(Sender: TObject; var Action: TCloseAction);
begin
    Action := caFree;
end;

procedure TfrmViewSearch.FormCreate(Sender: TObject);
begin
    inherited;
    TControlUtils.ResetFontForWinXP(self);
    self.Caption := TLanguageString.Read('Search in Methods', 'Suchen in Methoden');

    fSearchDataList := TObjectList<TSearchData>.Create;
    fMethodEditDataHelper := TMethodEditDataHelper.Create(nil);

    self.TVcxTreeListColumn1.Caption.Text := TLanguageString.Read('Method', 'Methode');
    self.TVcxTreeListColumn2.Caption.Text := TLanguageString.Read('Line', 'Zeile');

    cxTreeList1.Images := TViewItemsWorkflow.Instance.ActionImages.ActionImages16;
end;

procedure TfrmViewSearch.FormDestroy(Sender: TObject);
begin
    FreeAndNil(fMethodEditDataHelper);
    FreeAndNil(fSearchDataList);
    inherited;
end;

procedure TfrmViewSearch.SearchFor(const aSearchText: string);
begin
    self.cxTreeList1.Clear;
    fSearchDataList.Clear;

    self.Caption := TLanguageString.Read('Search: ' + aSearchText, 'Suche: ' + aSearchText);
    fSearchText := aSearchText;

    self.FindInMethods();
    self.FindInSqlTerms();
end;

procedure TfrmViewSearch.FindInMethods();
var
    xDataAdaptor: TMethodDataAdaptor;
    i, j: integer;
    xMethodNode: TcxTreeListNode;
    xMethodRecs: TMethodRecArray;
    xMethodNames: TArray<string>;
begin
    xDataAdaptor := TMethodDataAdaptor.Create();
    try
        xMethodNames := xDataAdaptor.ReadAllNames();

        for i := 0 to high(xMethodNames) do
        begin
            xMethodRecs := xDataAdaptor.ReadMethod(xMethodNames[i]);
            xMethodNode := nil;
            for j := 0 to high(xMethodRecs) do
            begin
                AddNodesFromMethodRec(xMethodNode, xMethodRecs[j], j);
            end;
        end;
    finally
        xDataAdaptor.Free;
    end;
end;

procedure TfrmViewSearch.FindInSqlTerms();
var
    xDataAdaptor: TSQLTermsDataAdaptor;
    i: integer;
    xSQLNode: TcxTreeListNode;
    xRec: TSQLTermRec;
    xAllNames: TArray<string>;
begin
    xDataAdaptor := TSQLTermsDataAdaptor.Create();
    try
        xAllNames := xDataAdaptor.ReadAllNames();

        for i := 0 to high(xAllNames) do
        begin
            if xDataAdaptor.ReadSQLTermData(xAllNames[i], xRec) then
            begin
                xSQLNode := nil;
                AddNodesFromSQLTermRec(xSQLNode, xRec);
            end;
        end;
    finally
        xDataAdaptor.Free;
    end;
end;

procedure TfrmViewSearch.AddNode(var vParentNode: TcxTreeListNode; aSearchData: TSearchData;
    const aMethodRow: integer; const aText: string; const aFoundPos: integer; const aKeyArray: TArray<string>;
    aImageIndex1, aImageIndex2: integer);
var
    xKeyArray: TArray<string>;
    xChildNode: TcxTreeListNode;
begin
    fSearchDataList.Add(aSearchData);
    aSearchData.SetTexts(aText, aFoundPos, Length(fSearchText));
    SetLength(xKeyArray, 0);
    aSearchData.SettingKey := aKeyArray;
    aSearchData.RowIndex := aMethodRow;
    // xSearchData.ColIndex := aMethodCol;

    if not Assigned(vParentNode) then
    begin
        vParentNode := self.cxTreeList1.Add;
        vParentNode.Texts[INT_COL_METHOD] := aSearchData.MainText;
        vParentNode.ImageIndex := aImageIndex1;
        vParentNode.SelectedIndex := aImageIndex1;
        vParentNode.Data := nil;
    end;

    xChildNode := vParentNode.AddChild();
    xChildNode.Texts[INT_COL_METHOD] := aSearchData.ActionText;
    xChildNode.Texts[INT_COL_LINE] := IntToStr(aMethodRow + 1);
    xChildNode.Texts[INT_COL_OPTION] := aText;
    xChildNode.ImageIndex := aImageIndex2;
    xChildNode.SelectedIndex := aImageIndex2;

    xChildNode.Data := aSearchData;
end;

procedure TfrmViewSearch.AddNodesFromMethodRec(var vParentNode: TcxTreeListNode; const aMethodRec: TMethodRec;
    aMethodRow: integer);
var
    xUpperSearchText: string;
    xMethodStep: TMethodStep;
    xSearchResult: TCustomSettingSearchResult;
    xSearchResults: TCustomSettingSearchResultList;
    xText: string;
    xKeyArray: TArray<string>;
    xPos: integer;
begin
    xUpperSearchText := Uppercase(fSearchText);

    xMethodStep := self.fMethodEditDataHelper.CreateMethodStepFromMethodRec(aMethodRec, false);
    if not Assigned(xMethodStep) then
        EXIT;

    // search in Action name
    xPos := PosEx(xUpperSearchText, UpperCase(xMethodStep.ActionName));
    if xPos > 0 then
    begin
        SetLength(xKeyArray, 0);
        AddNode(vParentNode, TMethodSearchData.Create(aMethodRec), aMethodRow, xMethodStep.ActionName, xPos,
            xKeyArray, INT_IM_INDEX_METHOD, TRunStepInfoFactory.GetIconIndex(xMethodStep.ActionName));
    end;

    // search in Options
    xSearchResults := TCustomSettingSearchResultList.Create();
    try
        xMethodStep.FindText(xUpperSearchText, xSearchResults);

        for xSearchResult in xSearchResults do
        begin
            xText := xSearchResult.Setting.Value;
            SetLength(xKeyArray, 0);
            xSearchResult.Setting.GetKeyArray(xKeyArray);
            AddNode(vParentNode, TMethodSearchData.Create(aMethodRec), aMethodRow, xText,
                xSearchResult.FoundPos, xKeyArray, INT_IM_INDEX_METHOD,
                TRunStepInfoFactory.GetIconIndex(xMethodStep.ActionName));
        end;
    finally
        FreeAndNil(xSearchResults);
    end;
end;

procedure TfrmViewSearch.AddNodesFromSQLTermRec(var vParentNode: TcxTreeListNode; const aRec: TSQLTermRec);
var
    xUpperSearchText: string;
    xKeyArray: TArray<string>;
    xPos: integer;
begin
    xUpperSearchText := Uppercase(fSearchText);

    // search in SQL
    xPos := PosEx(xUpperSearchText, UpperCase(aRec.Term));
    if xPos > 0 then
    begin
        SetLength(xKeyArray, 0);
        AddNode(vParentNode, TSQLTermSearchData.Create(aRec), 0, aRec.name, xPos, xKeyArray,
            INT_IM_INDEX_SQLTERM, INT_IM_INDEX_SQLTERM);
    end;
end;

procedure TfrmViewSearch.cxTreeList1DblClick(Sender: TObject);
var
    xNode: TcxTreeListNode;
    xItemName: string;
    xData: TSearchData;
    xViewType: TViewItemType;
    xCurrentEditor: TForm;
begin
    xNode := cxTreeList1.FocusedNode;
    if xNode = nil then
        EXIT;

    xData := nil;

    if xNode.Data = nil then
        xItemName := xNode.Texts[INT_COL_METHOD]
    else
    begin
        xData := TSearchData(xNode.Data);
        xItemName := xData.MainText;
    end;

    if (xNode.ImageIndex = INT_IM_INDEX_SQLTERM) then
        xViewType := ntSQLTerm
    else
        xViewType := ntMethod;

    xCurrentEditor := TViewItemsWorkflow.Instance.OpenEditForm(xItemName, xViewType);

    if (xCurrentEditor is TViewItemEditForm) and Assigned(xData) then
        (xCurrentEditor as TViewItemEditForm).EditSetFocusToTextPart(xData.RowIndex, xData.SettingKey,
            xData.Pos, Length(fSearchText));
end;

procedure TfrmViewSearch.cxTreeList1CustomDrawDataCell(Sender: TcxCustomTreeList; ACanvas: TcxCanvas;
    AViewInfo: TcxTreeListEditCellViewInfo; var ADone: Boolean);
var
    xCol: integer;
    xRect: TRect;
    xLeftPos: integer;
    xData: TSearchData;

    procedure AddText(const aText: string);
    begin
        if aText = '' then
            EXIT;
        aCanvas.DrawText(aText, Rect(xRect.Left + xLeftPos, xRect.Top + 2, xRect.Right, xRect.Bottom), 0);
        Inc(xLeftPos, aCanvas.TextWidth(aText));
    end;

begin
    aCanvas.Font.Color := clblack;
    xCol := aViewInfo.Column.ItemIndex;

    if xCol <> INT_COL_OPTION then
        Exit;
    xRect := aViewInfo.VisibleRect;
    ACanvas.FillRect(xRect);

    xLeftPos := 2;
    if aViewInfo.Node.Data = nil then
        EXIT;
    xData := (aViewInfo.Node.Data);
    if xData = nil then
        EXIT;

    AddText(xData.Before);
    aCanvas.Font.Color := clRed;
    AddText(xData.Search);
    aCanvas.Font.Color := clBlack;
    AddText(xData.After);

    aDone := true;
end;


end.
