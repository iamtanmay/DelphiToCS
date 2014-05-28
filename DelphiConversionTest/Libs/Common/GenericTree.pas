{ -----------------------------------------------------------------------------------------------------------------------
  Copyright © 2010 Zinsser Analytic GmbH - All rights reserved.
  Author       : Payman Kamali (pk)
  Description  :
  -----------------------------------------------------------------------------------------------------------------------
  Revision History:
  date     op  method                                track-no  improvement/change
  -------- --  ------------------------------------  --------- ----------------------------------------------------------
  17.03.10 wl  TGenericTree...                       TN5031    von ListClasses getrennt
  27.03.13 wl                                        TN6045   uses geändert
  ----------------------------------------------------------------------------------------------------------------------- }

unit GenericTree;


interface


uses
    Generics.Collections,
    Generics.Defaults,
    GeneralTypes;

type
    TGenericTreeNode<T> = class;

    TGenericTreeNodeListEnumerator<T> = class(TEnumerator < TGenericTreeNode < T >> )
    private
        fList: TObjectList<TGenericTreeNode<T>>;
        fIndex: Integer;
    protected
        function DoGetCurrent: TGenericTreeNode<T>; override;
        function DoMoveNext: Boolean; override;
    public
        constructor Create(const aList: TObjectList < TGenericTreeNode < T >> );
    end;

    TGenericTreeNodeList<T> = class(TEnumerable < TGenericTreeNode < T >> )
    private
        fList: TObjectList<TGenericTreeNode<T>>;
    protected
        function DoGetEnumerator: TEnumerator<TGenericTreeNode<T>>; override;
    public
        constructor Create();
        destructor Destroy(); override;
        procedure Add(const aNode: TGenericTreeNode<T>);
        procedure Clear();

    end;

    TGenericTreeNode<T> = class
    private
        fNodeValue: T;
        fNodes: TGenericTreeNodeList<T>;
    public
        constructor Create();
        destructor Destroy(); override;
        property Nodes: TGenericTreeNodeList<T>read fNodes;
        property NodeValue: T read fNodeValue write fNodeValue;
    end;

    TGenericTree<T> = class
    private
        fNodes: TGenericTreeNodeList<T>;
    public
        constructor Create();
        destructor Destroy(); override;
        property Nodes: TGenericTreeNodeList<T>read fNodes;
    end;

    TStringTreeNode = TGenericTreeNode<string>;
    TStringTreeNodeList = TGenericTreeNodeList<string>;
    TStringTree = TGenericTree<string>;


implementation


uses
    SysUtils;

{ TGenericTreeNode<T> }

constructor TGenericTreeNode<T>.Create;
begin
    inherited Create();
    fNodeValue := default (T);
    fNodes := TGenericTreeNodeList<T>.Create();
end;

destructor TGenericTreeNode<T>.Destroy;
begin
    FreeAndNil(fNodes);
    inherited;
end;

{ TGenericTreeNodeList<T> }

procedure TGenericTreeNodeList<T>.Add(const aNode: TGenericTreeNode<T>);
begin
    fList.Add(aNode);
end;

procedure TGenericTreeNodeList<T>.Clear;
var
    xNode: TGenericTreeNode<T>;
begin
    for xNode in fList do
        xNode.Nodes.Clear;

    fList.Clear;
end;

constructor TGenericTreeNodeList<T>.Create;
begin
    inherited Create();
    fList := TObjectList < TGenericTreeNode < T >>.Create(true);
end;

destructor TGenericTreeNodeList<T>.Destroy;
begin
    FreeAndNil(fList);
    inherited;
end;

function TGenericTreeNodeList<T>.DoGetEnumerator: TEnumerator<TGenericTreeNode<T>>;
begin
    result := TGenericTreeNodeListEnumerator<T>.Create(fList);
end;

{ TGenericTreeNodeListEnumerator<T> }

constructor TGenericTreeNodeListEnumerator<T>.Create(const aList: TObjectList < TGenericTreeNode < T >> );
begin
    inherited Create;
    fList := aList;
    fIndex := -1;
end;

function TGenericTreeNodeListEnumerator<T>.DoGetCurrent: TGenericTreeNode<T>;
begin
    result := fList[fIndex];
end;

function TGenericTreeNodeListEnumerator<T>.DoMoveNext: Boolean;
begin
    if fIndex >= fList.Count then
        Exit(False);

    Inc(fIndex);
    result := fIndex < fList.Count;
end;

{ TGenericTree<T> }

constructor TGenericTree<T>.Create;
begin
    inherited Create();
    fNodes := TGenericTreeNodeList<T>.Create();
end;

destructor TGenericTree<T>.Destroy;
begin
    FreeAndNil(fNodes);
    inherited;
end;


end.
