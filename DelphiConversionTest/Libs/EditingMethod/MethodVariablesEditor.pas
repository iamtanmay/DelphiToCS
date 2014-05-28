{ -----------------------------------------------------------------------------------------------------------
  Copyright © 2013 Zinsser Analytic GmbH - All rights reserved.
  Author       : Wolfgang Lyncke (wl)
  Description  :
  -----------------------------------------------------------------------------------------------------------
  Revision History:
  date     op  method                              track-no improvement/change
  -------- --  ----------------------------------  -------- -------------------------------------------------
  22.04.13 wl                                      TN6095   Initial Revision
  14.05.13 wl                                      TN6095   verwendet MethodVarPagesDataAdaptor
  09.08.13 wl                                      TN6095   Detailverbesserungen
  ----------------------------------------------------------------------------------------------------------- }

unit MethodVariablesEditor;


interface


uses
    Classes,
    Controls,
    Generics.Collections,
    Forms,
    cxStyles,
    cxCustomData,
    cxGraphics,
    cxFilter,
    cxData,
    cxDataStorage,
    cxEdit,
    cxEditRepositoryItems,
    Menus,
    cxGridLevel,
    cxGridCustomTableView,
    cxGridTableView,
    cxClasses,
    cxControls,
    cxGridCustomView,
    cxGrid,
    StdCtrls,
    ExtCtrls,
    cxLookAndFeels,
    cxLookAndFeelPainters,
    cxTL,
    cxTextEdit,
    cxInplaceContainer,
    StringLoader,
    MethodVariableTypes,
    ParserStoredIdentifier;

type
    TMethodVariablesStringLoader = class(TTextListStringLoader)
    protected
        procedure AddAllItems(); override;
    end;

    TfrmMethodVariablesEditor = class(TForm)
        pnlBottom: TPanel;
        btnOK: TButton;
        btnCancel: TButton;
        PopupMenu1: TPopupMenu;
        pmnuEdit: TMenuItem;
        Splitter1: TSplitter;
        pnLeft: TPanel;
        cxGrid1: TcxGrid;
        cxGrid1TableView1: TcxGridTableView;
        cxGrid1TableView1Column1: TcxGridColumn;
        cxGrid1TableView1Column2: TcxGridColumn;
        cxGrid1TableView1Column3: TcxGridColumn;
        cxGrid1TableView1Column4: TcxGridColumn;
        cxGrid1Level1: TcxGridLevel;
        Splitter2: TSplitter;
        Panel1: TPanel;
        cxGrid2: TcxGrid;
        cxGrid2TableView1: TcxGridTableView;
        cxGridColumn2: TcxGridColumn;
        cxGridColumn3: TcxGridColumn;
        cxGridColumn4: TcxGridColumn;
        cxGrid2Level1: TcxGridLevel;
        Panel2: TPanel;
        Label1: TLabel;
        PopupMenu2: TPopupMenu;
        pmnuAddPage: TMenuItem;
        Panel3: TPanel;
        cxTreeList1: TcxTreeList;
        TVcxTreeListColumn1: TcxTreeListColumn;
        Panel4: TPanel;
        Button1: TButton;
        DeletePage1: TMenuItem;
        procedure FormCreate(Sender: TObject);
        procedure pmnuEditClick(Sender: TObject);
        procedure cxGrid1TableView1DblClick(Sender: TObject);
        procedure FormDestroy(Sender: TObject);
        procedure pmnuAddPageClick(Sender: TObject);
        procedure Button1Click(Sender: TObject);
        procedure DeletePage1Click(Sender: TObject);
    private const
        cColumn1Key = 0;
        cColumn1Order = 1;
        cColumn1RefToOrder = 2;
        cColumn1DialogHide = 3;

        cColumn2FirstIndex = 0;
        cColumn2LastIndex = 1;
        cColumn2Caption = 2;
    private
        fIdents: TObjectList<TParserStoredIdent>;
        fParameterDataChanged: boolean;
        fMethodName: string;
        fStringLoader: TMethodVariablesStringLoader;
        function GetFocusedParam(out oParam: TParserStoredIdent): boolean;
        procedure SetIdentData(aIndex: integer; aIdent: TParserStoredIdent);
        procedure SetPageData(aIndex: integer; aPage: TMethodVarPageRec);
        procedure RefreshIdents();
        procedure RefreshTree();
        procedure FullUpdate();
        function GetPageInfos: TArray<TMethodVarPageRec>;
    public
        property Idents: TObjectList<TParserStoredIdent>read fIdents;
        property CurrentMethodName: string read fMethodName;
        property PageInfos: TArray<TMethodVarPageRec>read GetPageInfos;
        procedure SetIdents(const aMethodName: string; aIdents: TObjectList<TParserStoredIdent>;
            const aPageInfos: TArray<TMethodVarPageRec>);
    end;


implementation


{$R *.dfm}

uses
    Dialogs,
    SysUtils,
    Variants,
    ControlUtils,
    GeneralTypes,
    CommonTypes,
    AppSettings,
    ParserEditIdent,
    MethodGUIParsing,
    MethodTypes;

{ TParserEditIdentStringLoader }

procedure TMethodVariablesStringLoader.AddAllItems;
begin
    AddSingle(250, 'Method Variable Input Definition', 'Eingabe-Definition der Variablen');
    AddSingle(510, '&OK', '&OK');
    AddSingle(520, '&Cancel', '&Abbrechen');
end;

{ TfrmMethodVariablesEditor }

procedure TfrmMethodVariablesEditor.FormCreate(Sender: TObject);
begin
    TControlUtils.ResetFontForWinXP(self);
    fStringLoader := TMethodVariablesStringLoader.Create;
    fStringLoader.LoadLanguage(self);

    fParameterDataChanged := false;
end;

procedure TfrmMethodVariablesEditor.FormDestroy(Sender: TObject);
begin
    fStringLoader.Free;
end;

procedure TfrmMethodVariablesEditor.pmnuAddPageClick(Sender: TObject);
begin
    cxGrid2TableView1.DataController.AppendRecord;
end;

procedure TfrmMethodVariablesEditor.pmnuEditClick(Sender: TObject);
var
    xParameter: TParserStoredIdent;
    xRec: TMethodVariableData;
begin
    if not GetFocusedParam(xParameter) then
        EXIT;

    xRec := xParameter.Data;
    if TfrmParserEditIdent.EditData(fMethodName, xParameter.Name, xRec) then
    begin
        xParameter.Data := xRec;

        // Neu sortieren, weil sich die Reihenfolge geändert haben könnte
        self.FullUpdate;

        fParameterDataChanged := true;
    end;
end;

function TfrmMethodVariablesEditor.GetFocusedParam(out oParam: TParserStoredIdent): boolean;
var
    xRecIndex: integer;
begin
    xRecIndex := cxGrid1TableView1.Controller.FocusedRowIndex;
    if xRecIndex = -1 then
        EXIT(false);

    oParam := fIdents[xRecIndex];

    EXIT(true);
end;

function TfrmMethodVariablesEditor.GetPageInfos: TArray<TMethodVarPageRec>;
var
    x: integer;
    xRec: TMethodVarPageRec;
    xList: TList<TMethodVarPageRec>;
begin
    xList := TList<TMethodVarPageRec>.Create;
    try
        for x := 0 to cxGrid2TableView1.DataController.RecordCount - 1 do
        begin
            xRec.MethodName := fMethodName;
            xRec.Page := x + 1;
            if (cxGrid2TableView1.DataController.Values[x, cColumn2FirstIndex] <> null) then
                xRec.FirstOrderIndex := cxGrid2TableView1.DataController.Values[x, cColumn2FirstIndex]
            else
                xRec.FirstOrderIndex := 0;
            if (cxGrid2TableView1.DataController.Values[x, cColumn2LastIndex] <> null) then
                xRec.LastOrderIndex := cxGrid2TableView1.DataController.Values[x, cColumn2LastIndex]
            else
                xRec.LastOrderIndex := 0;
            if (cxGrid2TableView1.DataController.Values[x, cColumn2Caption] <> null) then
                xRec.Caption := cxGrid2TableView1.DataController.Values[x, cColumn2Caption]
            else
                xRec.Caption := '';

            if (xRec.FirstOrderIndex > 0) then
                xList.Add(xRec);
        end;

        EXIT(xList.ToArray);
    finally
        FreeAndNil(xList);
    end;
end;

procedure TfrmMethodVariablesEditor.Button1Click(Sender: TObject);
begin
    RefreshTree;
end;

procedure TfrmMethodVariablesEditor.cxGrid1TableView1DblClick(Sender: TObject);
begin
    pmnuEditClick(Sender);
end;

procedure TfrmMethodVariablesEditor.DeletePage1Click(Sender: TObject);
var
    x, xRecIndex: integer;
begin
    if cxGrid2TableView1.Controller.SelectedRecordCount = 0 then
        EXIT;

    for x := cxGrid2TableView1.Controller.SelectedRecordCount - 1 downto 0 do
    begin
        xRecIndex := cxGrid2TableView1.Controller.SelectedRecords[x].RecordIndex;
        if (xRecIndex < 0) then
            CONTINUE;

        cxGrid2TableView1.DataController.DeleteRecord(xRecIndex);
        cxGrid2TableView1.DataController.UpdateData;
    end;
    cxGrid2TableView1.Controller.ClearSelection();
end;

procedure TfrmMethodVariablesEditor.FullUpdate;
begin
    RefreshIdents;
    RefreshTree;
end;

procedure TfrmMethodVariablesEditor.SetIdentData(aIndex: integer; aIdent: TParserStoredIdent);
begin
    cxGrid1TableView1.DataController.Values[aIndex, cColumn1Key] := aIdent.name;
    cxGrid1TableView1.DataController.Values[aIndex, cColumn1Order] := aIdent.Data.RequestOrder;
    cxGrid1TableView1.DataController.Values[aIndex, cColumn1RefToOrder] := aIdent.Data.ArrayLengthRefToOrder;
    cxGrid1TableView1.DataController.Values[aIndex, cColumn1DialogHide] := aIdent.Data.DialogHide;
end;

procedure TfrmMethodVariablesEditor.SetPageData(aIndex: integer; aPage: TMethodVarPageRec);
begin
    cxGrid2TableView1.DataController.Values[aIndex, cColumn2FirstIndex] := aPage.FirstOrderIndex;
    cxGrid2TableView1.DataController.Values[aIndex, cColumn2LastIndex] := aPage.LastOrderIndex;
    cxGrid2TableView1.DataController.Values[aIndex, cColumn2Caption] := aPage.Caption;
end;

procedure TfrmMethodVariablesEditor.RefreshTree;
var
    x, xPage: integer;
    xMainNode, xPageNode, xChildNode: TcxTreeListNode;
    xPageInfos: TArray<TMethodVarPageRec>;
begin
    // Tree auf der rechten Seite neu aufbauen
    cxTreeList1.Clear;
    xMainNode := self.cxTreeList1.Add;
    xMainNode.Texts[0] := 'Pages';

    // Page-Informationen anzeigen
    xPageInfos := GetPageInfos;
    for xPage := 0 to high(xPageInfos) do
    begin
        xPageNode := self.cxTreeList1.AddChild(xMainNode);
        xPageNode.Texts[0] := 'Page ' + IntToStr(xPageInfos[xPage].Page);

        for x := 0 to fIdents.Count - 1 do
        begin
            if (fIdents[x].Data.RequestOrder = xPageInfos[xPage].FirstOrderIndex) or
                ((xPageInfos[xPage].LastOrderIndex > xPageInfos[xPage].FirstOrderIndex) and
                (fIdents[x].Data.RequestOrder > xPageInfos[xPage].FirstOrderIndex) and
                (fIdents[x].Data.RequestOrder <= xPageInfos[xPage].LastOrderIndex)) then
            begin
                xChildNode := self.cxTreeList1.AddChild(xPageNode);
                xChildNode.Texts[0] := fIdents[x].name;
            end;
        end;
    end;

    // Last page
    xPageNode := nil;
    for x := 0 to fIdents.Count - 1 do
    begin
        if not fIdents[x].Data.DialogHide then
        begin
            if not Assigned(xPageNode) then
            begin
                xPageNode := self.cxTreeList1.AddChild(xMainNode);
                xPageNode.Texts[0] := 'Last page';
            end;
            xChildNode := self.cxTreeList1.AddChild(xPageNode);
            xChildNode.Texts[0] := fIdents[x].name;
        end;
    end;

    xMainNode.Expand(true);
end;

procedure TfrmMethodVariablesEditor.RefreshIdents;
var
    x: integer;
    xRecIndex: integer;
begin
    fIdents.Sort(TParserStoredIdentComparer.Create);

    // Vorhandene Zeilen löschen
    for x := cxGrid1TableView1.DataController.RecordCount - 1 downto 0 do
    begin
        cxGrid1TableView1.DataController.DeleteRecord(x);
    end;

    for x := 0 to fIdents.Count - 1 do
    begin
        xRecIndex := cxGrid1TableView1.DataController.AppendRecord;
        SetIdentData(xRecIndex, fIdents[x]);
    end;
end;

procedure TfrmMethodVariablesEditor.SetIdents(const aMethodName: string;
    aIdents: TObjectList<TParserStoredIdent>; const aPageInfos: TArray<TMethodVarPageRec>);
var
    x: integer;
    xRecIndex: integer;
begin
    fMethodName := aMethodName;
    Caption := fMethodName + ': ' + TLanguageString.
        read('Define variable input', 'Variablen-Eingabe festlegen');

    fIdents := aIdents;
    RefreshIdents;

    // Pages aktualisieren
    for x := 0 to high(aPageInfos) do
    begin
        xRecIndex := cxGrid2TableView1.DataController.AppendRecord;
        SetPageData(xRecIndex, aPageInfos[x]);
    end;

    RefreshTree;
end;


end.
