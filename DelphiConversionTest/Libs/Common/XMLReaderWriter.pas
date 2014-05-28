{ -----------------------------------------------------------------------------------------------------------------------
  Copyright © 2009 Zinsser Analytic GmbH - All rights reserved.
  Author       : Payman Kamali (pk)
  Description  :
  -----------------------------------------------------------------------------------------------------------------------
  Revision History:
  date     op  method                                track-no  improvement/change
  -------- --  ------------------------------------  --------- ----------------------------------------------------------
  17.02.09 pk                                        TN4232    Initial Revision
  24.02.09 pk  TProcessDetailsItem                   TN4232    New ActionIDDataCache
  24.02.09 pk                                        TN4232    supports int64
  24.02.09 pk                                        TN4232    TStreamable changed to TCustomStreamable
  25.02.09 pk  ReadIdent                             TN4232    also reads string for enumerated types
  04.03.09 pk  ReadSet,WriteSet                      TN4232    New
  08.06.09 pk  RootNodeToStr                         TN4585.2  New
  08.06.09 pk  XMLStrToStrings                       TN4585.2  New
  23.06.09 pk                                        TN4620    Changes for new delphi string type
  13.07.09 pk  WriteAnsiString                       TN4585.4  New
  13.07.09 pk  WriteFloat                            TN4585.4  replaces WriteDbl
  24.08.09 pk  Activate                              TN4735.3  Now supports German characters (ÜÖÄ)
  12.10.09 pk                                        TN4800    changes for Delphi 2010
  25.11.09 pk                                        TN4898    FlushToFile renamed to DataChanged, new FlushToFile forces flush
  04.02.10 pk                                        TN4972    massive changes for Restart, some code moved to TypeMap, TypeMapTranslator
  13.04.10 wl                                        TN5044   uses FileUtilities
  23.04.10 pk                                        TN4972    destroy TypeMapTranslators
  07.06.10 pk  TXMLReaderWriter.SetPathName          TN5077    New
  13.09.10 pk  SetPathName                           TN5218    Now possible to set a path with an explicit extension
  19.10.10 pk                                        TN5305    changes needed for CoreClient/Server
  08.09.11 wl  CreateObjectFromRootNode              TN5672   Speicherleck geschlossen
  ----------------------------------------------------------------------------------------------------------------------- }

unit XMLReaderWriter;


interface


uses
    Classes,
    XMLIntf,
    XMLDoc,
    TypInfo,
    TypeMap,
    Streamable,
    TypeMapTranslator;

type
    TXMLWriter = class
    private
        procedure WriteValueType(const aValue: string; const aNode: IXMLNode);
        procedure WriteValueTypeName(const aValueTypeName: string; const aNode: IXMLNode);
        procedure WriteTypeAndValue(const aMapItem: TMapItem; const aNode: IXMLNode);
        procedure WriteObjectIntern(const aTypeMap: TTypeMap; const aNode: IXMLNode);
        procedure WriteProperty(const aMapItem: TMapItem; const aNode: IXMLNode);
        procedure WriteProperties(const aTypeMap: TTypeMap; const aNode: IXMLNode);
    public
        constructor Create();
        procedure AddTypeMapToNode(const aTypeMap: TTypeMap; const aNode: IXMLNode);
    end;

    TXMLReader = class
    private
        function IsObjectValueType(const aValueType: string): boolean;
        procedure ReadProperty(const aTypeMap: TTypeMap; const aNode: IXMLNode);
        procedure ReadProperties(const aTypeMap: TTypeMap; const aNode: IXMLNode);
        procedure ReadObjectIntern(Instance: TTypeMap; const aNode: IXMLNode);
        procedure ReadValueTypeAndValue(const aMapItem: TMapItem; const aNode: IXMLNode);
        function ReadValueType(const aNode: IXMLNode): string;
        function ReadNodeValue(const aNode: IXMLNode; out oValue: variant): boolean;
        function ReadValueTypeName(const aNode: IXMLNode): string;
    public
        constructor Create();
        function CreateTypeMapFromNode(const aNode: IXMLNode): TTypeMap;
    end;

    TXMLReaderWriter = class
    private
        fXMLDoc: IXMLDocument;
        fXMLWriter: TXMLWriter;
        fXMLReader: TXMLReader;
        fObjectFromTypeMapTranslator: TObjectFromTypeMapTranslator;
        fObjectToTypeMapTranslator: TObjectToTypeMapTranslator;
        fAutoFlushWriteBufferCycle: integer;
        fDirtyCount: integer;
        fPathName: string;
        fObject: TObject;
        function GetRootNode: IXMLNode;
        procedure XMLNodeToStrings(const aNode: IXMLNode; aStrings: TStrings; aLevel: integer);
        procedure FlushToFile;
        // procedure AddObjectToNode( const aObject : TCustomStreamable; const aTypeMap : TTypeMap );
        procedure ObjectToXMLNodes(const aObject: TObject);
    public
        constructor Create(const aPathName: string; const aAutoFlushWriteBufferCycle: integer); overload;
        constructor Create(const aPathName: string); overload;
        destructor Destroy(); override;
        procedure ActivateWithoutRoot();
        procedure Activate();
        procedure DisActivate();
        procedure AssignObjectToRootNodeDirect(const aObject: TObject);
        procedure AddObjectToRootNode(const aObject: TObject);
        function CreateObjectFromRootNode<T: class, constructor>(): T;

        // procedure AddObjectToItemsTypeMap( const aObject : TCustomStreamable; const aTypeMap : TTypeMap );
        // procedure RemoveObjectFromItemsTypeMap( const aObject : TCustomStreamable; const aTypeMap : TTypeMap );
        // procedure SetObjectToTypeMap( const aObject : TCustomStreamable; const aTypeMap : TTypeMap);
        procedure DataChanged();
        procedure WriteToFile();
        function ReadFromFile(): boolean;
        function RootNodeToStr(): string;
        procedure RootNodeFromStr(const aValue: string);

        procedure RootNodeToStrings(aStrings: TStrings);
        procedure XMLStrToStrings(const aXMLStr: string; aStrings: TStrings);

        procedure SetPathName(const aValue: string);
        property RootNode: IXMLNode read GetRootNode;
    end;


implementation


uses
    SysUtils,
    Variants,
    ActiveX,
    FileUtilities;

const
    cAttributeType = 'Type';
    cAttributeClassName = 'ClassName';

constructor TXMLWriter.Create();
begin
    inherited Create();
end;

procedure TXMLWriter.WriteValueType(const aValue: string; const aNode: IXMLNode);
begin
    if aValue = '' then
        EXIT;
    aNode.Attributes[cAttributeType] := aValue;
end;

procedure TXMLWriter.WriteValueTypeName(const aValueTypeName: string; const aNode: IXMLNode);
begin
    if aValueTypeName = '' then
        EXIT;
    aNode.Attributes[cAttributeClassName] := aValueTypeName;
end;

procedure TXMLWriter.WriteTypeAndValue(const aMapItem: TMapItem; const aNode: IXMLNode);
begin
    self.WriteValueType(aMapItem.ValueType, aNode);
    self.WriteValueTypeName(aMapItem.ValueTypeName, aNode);
    if not aMapItem.IsValueEmpty then
        aNode.NodeValue := aMapItem.Value;
end;

procedure TXMLWriter.WriteProperty(const aMapItem: TMapItem; const aNode: IXMLNode);
var
    xNode: IXMLNode;
    xNodeName: string;
begin
    xNodeName := aMapItem.Name;
    xNode := aNode.AddChild(xNodeName);

    if aMapItem is TTypeMap then
        WriteObjectIntern(aMapItem as TTypeMap, xNode)
    else
        WriteTypeAndValue(aMapItem, xNode);

end;

procedure TXMLWRiter.WriteProperties(const aTypeMap: TTypeMap; const aNode: IXMLNode);
var
    x: Integer;
begin
    for x := 0 to aTypeMap.Count - 1 do
    begin
        WriteProperty(aTypeMap[x], aNode);
    end;
end;

procedure TXMLWriter.WriteObjectIntern(const aTypeMap: TTypeMap; const aNode: IXMLNode);
begin
    WriteTypeAndValue(aTypeMap, aNode);
    WriteProperties(aTypeMap, aNode);
end;

procedure TXMLWriter.AddTypeMapToNode(const aTypeMap: TTypeMap; const aNode: IXMLNode);
var
    xNode: IXMLNode;
begin
    if not Assigned(aTypeMap) then
        EXIT;

    xNode := aNode.AddChild(aTypeMap.Name);
    WriteObjectIntern(aTypeMap, xNode);
end;

{ TXMLReader }

constructor TXMLReader.Create();
begin
    inherited Create();
end;

function TXMLReader.IsObjectValueType(const aValueType: string): boolean;
begin
    // Hack!
    result := SameText(aValueType, 'Object') or SameText(aValueType, 'List');
end;

procedure TXMLReader.ReadValueTypeAndValue(const aMapItem: TMapItem; const aNode: IXMLNode);
var
    xNodeValue: variant;
begin
    aMapItem.ValueType := ReadValueType(aNode);;
    aMapItem.ValueTypeName := ReadValueTypeName(aNode);
    if ReadNodeValue(aNode, xNodeValue) then
        aMapItem.Value := xNodeValue;
end;

procedure TXMLReader.ReadProperty(const aTypeMap: TTypeMap; const aNode: IXMLNode);
var
    xPropName: string;
    xValueType: string;
    xField: TMapItem;

    procedure HandleException(E: Exception);
    var
        xName: string;
    begin
        xName := xPropName;
        raise Exception.Create('Error: ' + xName + ' - ' + E.Message);
    end;

begin
    try
        xPropName := aNode.NodeName;
        xValueType := ReadValueType(aNode);

        if IsObjectValueType(xValueType) then
        begin
            xField := aTypeMap.AddTypeMap(xPropName);
            ReadObjectIntern(xField as TTypeMap, aNode);
        end
        else
        begin
            xField := aTypeMap.AddItem(xPropName);
            ReadValueTypeAndValue(xField, aNode)
        end;

    except
        on E: Exception do
            HandleException(E);
    end;
end;

procedure TXMLReader.ReadProperties(const aTypeMap: TTypeMap; const aNode: IXMLNode);
var
    x: integer;
begin
    for x := 0 to aNode.ChildNodes.Count - 1 do
    begin
        ReadProperty(aTypeMap, aNode.ChildNodes[x])
    end;
end;

function TXMLReader.ReadValueTypeName(const aNode: IXMLNode): string;
begin
    result := '';
    if not aNode.HasAttribute(cAttributeClassName) then
        EXIT;

    result := aNode.Attributes[cAttributeClassName];
end;

function TXMLReader.ReadValueType(const aNode: IXMLNode): string;
begin
    result := '';
    if not aNode.HasAttribute(cAttributeType) then
        EXIT;
    result := aNode.Attributes[cAttributeType];
end;

function TXMLReader.ReadNodeValue(const aNode: IXMLNode; out oValue: variant): boolean;
begin
    result := aNode.IsTextElement;
    if not result then
        EXIT;

    oValue := aNode.NodeValue;
end;

procedure TXMLReader.ReadObjectIntern(Instance: TTypeMap; const aNode: IXMLNode);
begin
    ReadValueTypeAndValue(Instance, aNode);
    ReadProperties(Instance, aNode);
end;

function TXMLReader.CreateTypeMapFromNode(const aNode: IXMLNode): TTypeMap;

begin
    result := TTypeMap.Create(aNode.NodeName);
    ReadObjectIntern(result, aNode);
end;

{ TXMLReaderWriter }

constructor TXMLReaderWriter.Create(const aPathName: string; const aAutoFlushWriteBufferCycle: integer);
begin
    inherited Create();
    fXMLDoc := TXMLDocument.Create(nil);
    fXMLWriter := TXMLWriter.Create();
    fXMLReader := TXMLReader.Create();

    fObjectFromTypeMapTranslator := TObjectFromTypeMapTranslator.Create;
    fObjectToTypeMapTranslator := TObjectToTypeMapTranslator.Create;

    SetPathName(aPathName);
    fAutoFlushWriteBufferCycle := aAutoFlushWriteBufferCycle;
    fDirtyCount := 0;
    fObject := nil;
end;

constructor TXMLReaderWriter.Create(const aPathName: string);
begin
    Create(aPathName, 1);
end;

destructor TXMLReaderWriter.Destroy;
begin
    DisActivate();
    fXMLDoc := nil;
    fObjectToTypeMapTranslator.Free;
    fObjectFromTypeMapTranslator.Free;

    fXMLWriter.Free;
    fXMLReader.Free;

    inherited;
end;

procedure TXMLReaderWriter.ActivateWithoutRoot();
begin
    if fXMLDoc.Active then
        EXIT;
    Coinitialize(nil);
    fXMLDoc.Active := true;
end;

procedure TXMLReaderWriter.Activate();
begin
    fXMLDoc.XML.Text := '';
    ActivateWithoutRoot();
    fXMLDoc.Version := '1.0';
    fXMLDoc.Encoding := 'ISO-8859-1';
    fXMLDoc.DocumentElement := fXMLDoc.CreateNode('Root');
end;

procedure TXMLReaderWriter.Disactivate();
begin
    if not fXMLDoc.Active then
        EXIT;
    // fXMLDoc.DocumentElement := nil;
    fXMLDoc.Active := false;
    CoUninitialize();
end;

procedure TXMLReaderWriter.AssignObjectToRootNodeDirect(const aObject: TObject);
begin
    ObjectToXMLNodes(aObject);
end;

procedure TXMLReaderWriter.AddObjectToRootNode(const aObject: TObject);
begin
    fObject := aObject;
    self.DataChanged();
end;

procedure TXMLReaderWriter.ObjectToXMLNodes(const aObject: TObject);
var
    xTypeMap: TTypeMap;
begin
    if Assigned(fXMLDoc.DocumentElement) then
        fXMLDoc.DocumentElement.ChildNodes.Clear;

    xTypeMap := fObjectToTypeMapTranslator.CreateTypeMap(aObject);
    try
        fXMLWriter.AddTypeMapToNode(xTypeMap, fXMLDoc.DocumentElement);
    finally
        FreeAndNil(xTypeMap);
    end;
end;

function TXMLReaderWriter.CreateObjectFromRootNode<T>(): T;
var
    xTypeMap: TTypeMap;
begin
    result := default (T);
    try
        if not Assigned(fXMLDoc.DocumentElement) then
            EXIT;
        if fXMLDoc.DocumentElement.ChildNodes.Count = 0 then
            EXIT;

        xTypeMap := fXMLReader.CreateTypeMapFromNode(fXMLDoc.DocumentElement.ChildNodes[0]);
        try
            if (not Assigned(xTypeMap)) then
                EXIT;
            result := fObjectFromTypeMapTranslator.CreateObject<T>(xTypeMap);
        finally
            FreeAndNil(xTypeMap);
        end;
    finally
        fObject := result;
    end;
end;

procedure TXMLReaderWriter.FlushToFile;
var
    xPath: string;
begin
    xPath := ExtractFilePath(fPathName);
    if not TFileUtilities.FileExists(fPathName) then
        TFileUtilities.ForceDirectories(xPath);

    ObjectToXMLNodes(fObject);

    fXMLDoc.SaveToFile(fPathName);
    fDirtyCount := 0;
end;

procedure TXMLReaderWriter.DataChanged;
begin
    Inc(fDirtyCount);
    if (fAutoFlushWriteBufferCycle <= 0) then
        EXIT;
    if (fDirtyCount < fAutoFlushWriteBufferCycle) then
        EXIT;
    FlushToFile();
end;

procedure TXMLReaderWriter.WriteToFile;
begin
    if fDirtyCount = 0 then
        EXIT;
    FlushToFile();
end;

function TXMLReaderWriter.ReadFromFile: boolean;
begin
    result := false;
    Activate();
    if not TFileUtilities.FileExists(fPathName) then
        EXIT;
    fXMLDoc.LoadFromFile(fPathName);
    result := true;
end;

function TXMLReaderWriter.RootNodeToStr: string;
begin
    fXMLDoc.SaveToXML(result);
end;

procedure TXMLReaderWriter.RootNodeFromStr(const aValue: string);
begin
    fXMLDoc.LoadFromXML(aValue);
end;

function TXMLReaderWriter.GetRootNode: IXMLNode;
begin
    result := fXMLDoc.DocumentElement.ChildNodes[0];
end;

procedure TXMLReaderWriter.XMLNodeToStrings(const aNode: IXMLNode; aStrings: TStrings; aLevel: integer);
var
    i, j: integer;
    xAttributes: string;
    xChildNode: IXMLNode;
    xTabStr: string;
const
    cXMLIndentStr = '        ';

    function GetTabStr(const aLevel: integer): string;
    var
        x: integer;
    begin
        result := '';
        for x := 1 to aLevel do
            result := result + cXMLIndentStr;
    end;

begin
    xTabStr := GetTabStr(aLevel);
    with aNode do
    begin
        xAttributes := '';
        for j := 0 to AttributeNodes.Count - 1 do
        begin
            xAttributes := Format('%s %s="%s"', [xAttributes, AttributeNodes[j].NodeName,
                AttributeNodes[j].NodeValue]);
        end;

        if aNode.IsTextElement then
        begin
            aStrings.Add(Format('%s<%s%s>%s</%s>', [xTabStr, aNode.NodeName, xAttributes, aNode.NodeValue,
                aNode.NodeName]));
        end
        else
        begin
            aStrings.Add(Format('%s<%s%s>', [xTabStr, aNode.NodeName, xAttributes]));
            for i := 0 to (ChildNodes.Count - 1) do
            begin
                xChildNode := ChildNodes[i];
                XMLNodeToStrings(xChildNode, aStrings, aLevel + 1);
            end;
            aStrings.Add(Format('%s</%s>', [xTabStr, aNode.NodeName]));
        end;
    end;
end;

procedure TXMLReaderWriter.RootNodeToStrings(aStrings: TStrings);
begin
    XMLNodeToStrings(self.RootNode, aStrings, 0);
end;

procedure TXMLReaderWriter.SetPathName(const aValue: string);
begin
    fPathName := '';

    if aValue = '' then
        EXIT;

    fPathName := aValue;

    // if doesn't have an extension, then automatically xml
    if TFileUtilities.ExtractFileExtension(fPathName) = '' then
        fPathName := fPathName + '.xml';
end;

procedure TXMLReaderWriter.XMLStrToStrings(const aXMLStr: string; aStrings: TStrings);
begin
    self.ActivateWithoutRoot();
    self.RootNodeFromStr(aXMLStr);
    try
        RootNodeToStrings(aStrings);
    finally
        self.Disactivate;
    end;
end;


end.
