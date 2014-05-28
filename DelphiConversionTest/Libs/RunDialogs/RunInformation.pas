{ ------------------------------------------------------------------------------------------------------------
  Copyright © 2008 Zinsser Analytic GmbH - All rights reserved.
  Author       : Wolfgang Lyncke (wl)
  Description  : Run Information Frame
  ------------------------------------------------------------------------------------------------------------
  Revision History:
  date     op  method                                track-no  improvement/change
  -------- --  ------------------------------------- --------  -----------------------------------------------
  17.11.08 wl                                        TN4311    initial version
  08.12.08 wl                                        TN4311    jetzt mit TreeList
  12.12.08 pk                                        TN4311    fraRunInformation changed to FrmRunInformation
  18.12.08 pk                                        TN4311    New Value column
  18.12.08 pk                                        TN4311    GroupName is now a StringArray - represents a hierarchy of groupnames
  08.01.09 wl  FormCreate                            TN4311    Überschriften werden aus Ressourcen geladen
  23.04.09 wl  InsertInfoToTreeView                  TN4540    der letzte Eintrag ist immer sichtbar
  30.06.09 wl                                        TN4635    Spaltenbreite und -höhe passen sich jetzt automatisch an
  24.07.09 pk  SetColumnWidths                       TN4676    New
  31.07.09 ts  InsertInfo/ -ToTreeView               TN4666    InfoGroupBehaviour instead of HideGroup, if igbNoChange -> no Expand/Collapse of node
  10.08.09 wl                                        TN4702   Strings werden jetzt direkt geladen
  24.08.09 wl  InsertInfoToTreeView                  TN4733   MakeVisible wird immer durchgeführt
  03.09.09 wl  cxTreeList1                           TN4800   Anpassungen an Treelist Version 5 (nicht kompatibel)
  04.11.09 pk                               	    TN4843   Replace TObjectList, TStringList, TStrings with Generics Lists and TStringArray
  20.05.10 wl                                    TN5117   neue Schriftart "Segoe UI", Schriftgröße 9
  28.02.13 wl  SetColumnWidths                   TN6096   neu: DateColumnVisible
  10.04.13 wl                                    TN6045   uses geändert
  ------------------------------------------------------------------------------------------------------------ }

unit RunInformation;


interface


uses
    Forms,
    Classes,
    Controls,
    cxControls,
    cxGraphics,
    cxCustomData,
    cxStyles,
    cxTL,
    cxInplaceContainer,
    cxTextEdit,
    cxLookAndFeels,
    cxLookAndFeelPainters,
    AppTypes;

type
    TfrmRunInformation = class(TForm)
        cxTreeList1: TcxTreeList;
        cxTreeList1cxTreeListColumn1: TcxTreeListColumn;
        cxTreeList1cxTreeListColumn2: TcxTreeListColumn;
        cxTreeList1cxTreeListColumn3: TcxTreeListColumn;
        procedure FormCreate(Sender: TObject);
    private const
        cColIndexName = 0;
        cColIndexValue = 1;
        cColIndexDateTime = 2;
    private
        class var uInstance: TfrmRunInformation;
        procedure InsertInfoToTreeView(const aGroupNames: TArray<string>; const aKey, aText: string;
            aInfoGroupBehaviour: TInfoGroupBehaviour);
        function GetRunItem(): TcxTreeListNode;
        function FindNodeByGroupNameHierarchyRecursive(const aGroupNameHierarchy: TArray<string>;
            aNode: TcxTreeListNode; aCurrentLevel: integer; aAddNodeIfMissing: boolean): TcxTreeListNode;
    public
        procedure InsertInfo(const aGroupNames: TArray<string>; const aKey, aText: string;
            aInfoGroupBehaviour: TInfoGroupBehaviour);
        procedure StartNewRun(aMethodName: string);
        procedure SetColumnWidths(const aIdentWidth: integer; const aEventWidth: integer;
            const aDateWidth: integer; const aDateVisible: boolean);
        //
        class procedure SetInstance(const aValue: TfrmRunInformation);
        class function Instance(): TfrmRunInformation;
    end;


implementation


{$R *.dfm}

uses
    SysUtils,
    Generics.Collections,
    GeneralTypes,
    ControlUtils;

{ TfrmRunInformation }

class procedure TfrmRunInformation.SetInstance(const aValue: TfrmRunInformation);
begin
    uInstance := aValue;
end;

class function TfrmRunInformation.Instance: TfrmRunInformation;
begin
    result := uInstance;
end;

procedure TfrmRunInformation.InsertInfo(const aGroupNames: TArray<string>; const aKey, aText: string;
    aInfoGroupBehaviour: TInfoGroupBehaviour);
begin

    self.InsertInfoToTreeView(aGroupNames, aKey, aText, aInfoGroupBehaviour);
end;

function TfrmRunInformation.GetRunItem(): TcxTreeListNode;
begin
    result := cxTreeList1.Items[0];
end;

function TfrmRunInformation.FindNodeByGroupNameHierarchyRecursive(const aGroupNameHierarchy: TArray<string>;
    aNode: TcxTreeListNode; aCurrentLevel: integer; aAddNodeIfMissing: boolean): TcxTreeListNode;
var
    x: integer;
    xGroupName: string;
    xChildNode: TcxTreeListNode;
begin
    result := nil;
    if not Assigned(aNode) then
        EXIT;
    ASSERT((aCurrentLevel > 0) and (aCurrentLevel <= Length(aGroupNameHierarchy)));

    xGroupName := aGroupNameHierarchy[aCurrentLevel - 1];
    if not SameText(aNode.Texts[cColIndexName], xGroupName) then
        EXIT;

    if (aCurrentLevel = Length(aGroupNameHierarchy)) then
    begin
        result := aNode;
        EXIT;
    end;

    for x := 0 to aNode.Count - 1 do
    begin
        xChildNode := aNode.Items[x];
        result := FindNodeByGroupNameHierarchyRecursive(aGroupNameHierarchy, xChildNode, aCurrentLevel + 1,
            aAddNodeIfMissing);
        if Assigned(result) then
            EXIT;
    end;

    if aAddNodeIfMissing then
    begin
        xChildNode := aNode.AddChild();
        xChildNode.Parent.Expand(false);
        xChildNode.Texts[cColIndexName] := aGroupNameHierarchy[(aCurrentLevel - 1) + 1];
        result := FindNodeByGroupNameHierarchyRecursive(aGroupNameHierarchy, xChildNode, aCurrentLevel + 1,
            aAddNodeIfMissing);
    end;
end;

procedure TfrmRunInformation.InsertInfoToTreeView(const aGroupNames: TArray<string>;
    const aKey, aText: string; aInfoGroupBehaviour: TInfoGroupBehaviour);
var
    xGroupNode, xFirstNode, xChildNode, xNode: TcxTreeListNode;
    xGroupNameHierarchy: TList<string>;
    xKey: string;
    x: integer;
begin
    xFirstNode := self.GetRunItem;
    xGroupNameHierarchy := TList<string>.Create();
    try
        xGroupNameHierarchy.Add(xFirstNode.Texts[cColIndexName]);
        xGroupNameHierarchy.AddRange(aGroupNames);
        xGroupNode := FindNodeByGroupNameHierarchyRecursive(xGroupNameHierarchy.ToArray, xFirstNode, 1, true);
    finally
        FreeAndNil(xGroupNameHierarchy);
    end;

    ASSERT(Assigned(xGroupNode));
    xNode := nil;
    if aKey <> '' then
    begin
        for x := 0 to xGroupNode.Count - 1 do
        begin
            xChildNode := xGroupNode.Items[x];
            if SameText(xChildNode.Texts[cColIndexName], aKey) then
            begin
                xNode := xChildNode;
                BREAK;
            end;
        end;
    end;

    if not Assigned(xNode) then
    begin
        xNode := xGroupNode.AddChild;
        xKey := aKey;
        if xKey = '' then
            xKey := Format('%d', [xGroupNode.Count]);
        xNode.Texts[cColIndexName] := xKey;
    end;
    xNode.Texts[cColIndexValue] := aText;
    xNode.Texts[cColIndexDateTime] := DateTimeToStr(SysUtils.Now);
    // scroll control until the node appears
    xNode.MakeVisible();

    if (aInfoGroupBehaviour = igbShow) then
        xGroupNode.Expand(true);
    if (aInfoGroupBehaviour = igbHide) then
        xGroupNode.Collapse(true);
end;

procedure TfrmRunInformation.StartNewRun(aMethodName: string);
var
    xNode: TcxTreeListNode;
begin
    self.cxTreeList1.Clear;
    xNode := self.cxTreeList1.Add();
    xNode.Texts[cColIndexName] := aMethodName;
end;

procedure TfrmRunInformation.FormCreate(Sender: TObject);
begin
    TControlUtils.ResetFontForWinXP(self);
    self.cxTreeList1cxTreeListColumn1.Caption.Text := TLanguageString.Read('Identifier', 'Identifizierer');
    self.cxTreeList1cxTreeListColumn2.Caption.Text := TLanguageString.Read('Event', 'Ereignis');
    self.cxTreeList1cxTreeListColumn3.Caption.Text := TLanguageString.Read('Date', 'Zeitpunkt');
    StartNewRun('');
end;

procedure TfrmRunInformation.SetColumnWidths(const aIdentWidth, aEventWidth, aDateWidth: integer;
    const aDateVisible: boolean);
begin
    cxTreeList1cxTreeListColumn3.Visible := aDateVisible;

    if aIdentWidth >= 0 then
    begin
        cxTreeList1cxTreeListColumn1.MinWidth := aIdentWidth;
        cxTreeList1cxTreeListColumn1.Width := aIdentWidth;
    end;

    if aEventWidth >= 0 then
    begin
        cxTreeList1cxTreeListColumn2.MinWidth := aEventWidth;
        cxTreeList1cxTreeListColumn2.Width := aEventWidth;
    end;

    if aDateWidth >= 0 then
    begin
        cxTreeList1cxTreeListColumn3.MinWidth := aDateWidth;
        cxTreeList1cxTreeListColumn3.Width := aDateWidth;
    end;
end;


end.
