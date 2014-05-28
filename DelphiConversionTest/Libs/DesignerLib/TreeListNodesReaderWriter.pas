unit TreeListNodesReaderWriter;
{ ----------------------------------------------------------------------------------------------------------------------
  Copyright  2010 Zinsser Analytic GmbH - All rights reserved.
  Author       : Wolfgang Lyncke (wl)
  Description  : Lesen und Schreiben einer XML-Datei für TreeList-Nodes
  ----------------------------------------------------------------------------------------------------------------------
  Revision History:
  date     op  method                       track-no improvement/change
  -------- --  ---------------------------  -------- -------------------------------------------------------------------
  29.07.10 wl                               TN5204   initial revision
  20.08.10 wl  fOnCheckNodeData             TN5223   Funktion zum Prüfen der Einträge
  ---------------------------------------------------------------------------------------------------------------------- }


interface


uses
    cxTL,
    ListClasses,
    Streamable,
    XMLReaderWriter;

type
    TTreeListNodeData = class(TStreamable)
    private
        fText1: string;
        fStateIndex: integer;
        fNodes: TStreamableObjectList;
    public
        constructor Create(); reintroduce;
        function GetNodeDataAt(aIndex: Integer): TTreeListNodeData;
    published
        property Text1: string read fText1 write fText1;
        property StateIndex: integer read fStateIndex write fStateIndex;
        property Nodes: TStreamableObjectList read fNodes write fNodes;
    end;

    TTreeListNodeAfterReadEvent = procedure(aSender: TObject; aNode: TcxTreeListNode) of object;
    TTreeListNodeDataCheckEvent = function(aSender: TObject; aNodeData: TTreeListNodeData): boolean of object;

    TTreeListNodesReaderWriter = class
    private
        fOnSetNodeAfterRead: TTreeListNodeAfterReadEvent;
        fOnCheckNodeData: TTreeListNodeDataCheckEvent;
        procedure ReadChildNodeData(aNode: TcxTreeListNode; aNodeData: TTreeListNodeData);
        class procedure WriteNodeData(aNode: TcxTreeListNode; aNodeData: TTreeListNodeData);
    public
        procedure read(const aPath: string; aRootNode: TcxTreeListNode);
        procedure write(const aPath: string; aRootNode: TcxTreeListNode);
        property OnSetNodeAfterRead: TTreeListNodeAfterReadEvent read fOnSetNodeAfterRead
            write fOnSetNodeAfterRead;
        property OnCheckNodeData: TTreeListNodeDataCheckEvent read fOnCheckNodeData write fOnCheckNodeData;
    end;


implementation


uses
    SysUtils;

{ TTTreeListNodeData }

constructor TTreeListNodeData.Create;
begin
    inherited Create();
    fNodes := TStreamableObjectList.Create();
end;

function TTreeListNodeData.GetNodeDataAt(aIndex: Integer): TTreeListNodeData;
begin
    result := fNodes[aIndex] as TTreeListNodeData;
end;

{ TXMLReaderWriterTest }

procedure TTreeListNodesReaderWriter.Read(const aPath: string; aRootNode: TcxTreeListNode);
var
    xXMLReaderWriter: TXMLReaderWriter;
    xNodeData: TTreeListNodeData;
begin
    xXMLReaderWriter := TXMLReaderWriter.Create(aPath, 0);
    try
        xXMLReaderWriter.ReadFromFile();
        xNodeData := xXMLReaderWriter.CreateObjectFromRootNode<TTreeListNodeData>();

        aRootNode.DeleteChildren;
        ReadChildNodeData(aRootNode, xNodeData);

    finally
        FreeAndNil(xXMLReaderWriter);
    end;
end;

procedure TTreeListNodesReaderWriter.ReadChildNodeData(aNode: TcxTreeListNode; aNodeData: TTreeListNodeData);
var
    x: integer;
    xChildNode: TcxTreeListNode;
    xChildNodeData: TTreeListNodeData;
begin
    if not Assigned(aNodeData) then
        EXIT;

    for x := 0 to aNodeData.Nodes.Count - 1 do
    begin

        // gespeicherte Daten lesen und prüfen
        xChildNodeData := aNodeData.GetNodeDataAt(x);
        if Assigned(self.fOnCheckNodeData) and not fOnCheckNodeData(self, xChildNodeData) then
            CONTINUE;

        // add new child to TreeList-Node
        xChildNode := aNode.AddChild;
        xChildNode.Texts[0] := xChildNodeData.Text1;
        xChildNode.StateIndex := xChildNodeData.StateIndex;
        if Assigned(fOnSetNodeAfterRead) then
            fOnSetNodeAfterRead(self, xChildNode);

        ReadChildNodeData(xChildNode, xChildNodeData);
    end;
end;

procedure TTreeListNodesReaderWriter.Write(const aPath: string; aRootNode: TcxTreeListNode);
var
    xXMLReaderWriter: TXMLReaderWriter;
    xNodeData: TTreeListNodeData;
begin
    xXMLReaderWriter := TXMLReaderWriter.Create(aPath, 0);
    try
        xNodeData := TTreeListNodeData.Create();
        WriteNodeData(aRootNode, xNodeData);

        xXMLReaderWriter.DataChanged();
        xXMLReaderWriter.Activate();
        xXMLReaderWriter.AddObjectToRootNode(xNodeData);
        xXMLReaderWriter.WriteToFile();
    finally
        FreeAndNil(xXMLReaderWriter);
    end;
end;

class procedure TTreeListNodesReaderWriter.WriteNodeData(aNode: TcxTreeListNode;
    aNodeData: TTreeListNodeData);
var
    xChildNode: TcxTreeListNode;
    xChildNodeData: TTreeListNodeData;
begin
    aNodeData.Text1 := aNode.Texts[0];
    aNodeData.StateIndex := aNode.StateIndex;

    xChildNode := aNode.getFirstChild;
    while Assigned(xChildNode) do
    begin

        if xChildNode.Visible then
        begin
            xChildNodeData := TTreeListNodeData.Create();
            aNodeData.Nodes.Add(xChildNodeData);
            WriteNodeData(xChildNode, xChildNodeData);
        end;

        xChildNode := aNode.GetNextChild(xChildNode);
    end;
end;


end.
