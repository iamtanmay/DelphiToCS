unit LayoutEditingViewItem;
{ ------------------------------------------------------------------------------------------------------------
  Copyright  2010 Zinsser Analytic GmbH - All rights reserved.
  Author       : Wolfgang Lyncke (wl)
  Description  :
  ------------------------------------------------------------------------------------------------------------
  Revision History:
  date     op  method                            track-no improvement/change
  -------- --  --------------------------------  -------- ----------------------------------------------------
  03.05.10 wl                                    TN5052   initial revision
  10.05.10 wl                                    TN5052   internal changes
  12.06.12 ts  TTipTypeViewItem.CreateNewName    TN5909   new, to add new tiptypes in ZADesigner
  ------------------------------------------------------------------------------------------------------------ }


interface


uses
    QueryDataAdaptor,
    GeneralTypes,
    ViewItem;

type
    TRackViewItem = class(TViewItem)
    protected
        function GetItemType(): TViewItemType; override;
        function GetTypeCaption(): string; override;
    public
        function ReadAllNames: TStringArray; override;
    end;

    TCarrierViewItem = class(TViewItem)
    protected
        function GetItemType(): TViewItemType; override;
        function GetTypeCaption(): string; override;
    public
        function ReadAllNames: TStringArray; override;
    end;

    TWorkspaceViewItem = class(TViewItem)
    protected
        function GetItemType(): TViewItemType; override;
        function GetTypeCaption(): string; override;
    public
        function ReadAllNames: TStringArray; override;
    end;

    TTipTypeViewItem = class(TDatasetViewItem)
    protected
        function GetItemType(): TViewItemType; override;
        function GetTypeCaption(): string; override;
        function CreateDataAdaptor(): TQueryDataAdaptor; override;
    public
        procedure CreateNewName(const aNewName: string); override;
    end;


implementation


uses
    SysUtils,
    TipTypeDataAdaptor,
    RackDataAdaptor,
    CarrierDataAdaptor,
    WorkspaceDataAdaptor,
    CommonTypes;

{ TRackViewItem }

function TRackViewItem.GetItemType: TViewItemType;
begin
    result := ntRack;
end;

function TRackViewItem.GetTypeCaption: string;
begin
    result := 'Rack';
end;

function TRackViewItem.ReadAllNames(): TStringArray;
var
    xDA: TRackDataAdaptor;
begin
    xDA := TRackDataAdaptor.Create;
    try
        result := xDA.ReadAllNames();
    finally
        FreeAndNil(xDA);
    end;
end;

{ TCarrierViewItem }

function TCarrierViewItem.GetItemType: TViewItemType;
begin
    result := ntCarrier;
end;

function TCarrierViewItem.GetTypeCaption: string;
begin
    result := 'Carrier';
end;

function TCarrierViewItem.ReadAllNames(): TStringArray;
var
    xDA: TCarrierDataAdaptor;
begin
    xDA := TCarrierDataAdaptor.Create;
    try
        result := xDA.ReadAllNames();
    finally
        FreeAndNil(xDA);
    end;
end;

{ TWorkspaceViewItem }

function TWorkspaceViewItem.GetItemType: TViewItemType;
begin
    result := ntWorkspace;
end;

function TWorkspaceViewItem.GetTypeCaption: string;
begin
    result := 'Workspace';
end;

function TWorkspaceViewItem.ReadAllNames(): TStringArray;
var
    xDA: TWorkspaceDataAdaptor;
begin
    xDA := TWorkspaceDataAdaptor.Create;
    try
        result := xDA.ReadAllNames();
    finally
        FreeAndNil(xDA);
    end;
end;

{ TTipTypeViewItem }

function TTipTypeViewItem.CreateDataAdaptor: TQueryDataAdaptor;
begin
    result := TTipTypeDataAdaptor.Create
end;

procedure TTipTypeViewItem.CreateNewName(const aNewName: string);
var
    xDataAdaptor: TTipTypeDataAdaptor;
    xNewTipType: TTipType;
begin
    xDataAdaptor := CreateDataAdaptor() as TTipTypeDataAdaptor;
    try
        xNewTipType := xDataAdaptor.GetEmptyRec;
        xNewTipType.name := aNewName;
        xDataAdaptor.WriteTipType(xNewTipType);
    finally
        xDataAdaptor.Free;
    end;
end;

function TTipTypeViewItem.GetItemType: TViewItemType;
begin
    result := ntTipType;
end;

function TTipTypeViewItem.GetTypeCaption: string;
begin
    result := 'Tip Type';
end;


end.
