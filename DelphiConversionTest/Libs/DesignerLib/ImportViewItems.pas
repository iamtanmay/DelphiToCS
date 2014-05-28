{ --------------------------------------------------------------------------------------------------
  Copyright © 2011 Zinsser Analytic GmbH - All rights reserved.
  Author       : Wolfgang Lyncke (wl)
  Description  :
  --------------------------------------------------------------------------------------------------
  Revision History:
  date     op  method                       track-no improvement/change
  -------- --  ---------------------------  -------- -----------------------------------------------
  23.02.11 wl                               TN5486   initial revision
  01.07.11 wl                               TN5619   komplett überarbeitet
  17.04.13 wl  TImportDefViewItem.SaveAs    TN6106   an TViewItem angepasst
  -------------------------------------------------------------------------------------------------- }

unit ImportViewItems;


interface


uses
    Classes,
    DockableForm,
    QueryDataAdaptor,
    GeneralTypes,
    ViewItem;

type
    TImportFileDefViewItem = class(TDatasetViewItem)
    protected
        function GetItemType(): TViewItemType; override;
        function GetTypeCaption(): string; override;
        function CreateDataAdaptor(): TQueryDataAdaptor; override;
        procedure DeleteNamePhysically(); override;
    public
        function CreateEditForm(aOwner: TComponent; aOnSaveStatusChanged: TNotifyEvent)
            : TDockableEditForm; override;
        procedure CreateNewName(const aNewName: string); override;
        procedure SaveAs(const aSourceName, aTargetName: string); override;
    end;

    TImportDefViewItem = class(TDatasetViewItem)
    protected
        procedure DeleteNamePhysically(); override;
        function CreateDataAdaptor(): TQueryDataAdaptor; override;
    public
        procedure SaveAs(const aSourceName, aTargetName: string); override;
    end;

    TVarImportDefViewItem = class(TImportDefViewItem)
    protected
        function GetItemType(): TViewItemType; override;
        function GetTypeCaption(): string; override;
    public
        function CreateEditForm(aOwner: TComponent; aOnSaveStatusChanged: TNotifyEvent)
            : TDockableEditForm; override;
        procedure CreateNewName(const aNewName: string); override;
        function ReadAllNames: TStringArray; override;
    end;

    TTableImportDefViewItem = class(TImportDefViewItem)
    protected
        function GetItemType(): TViewItemType; override;
        function GetTypeCaption(): string; override;
    public
        function CreateEditForm(aOwner: TComponent; aOnSaveStatusChanged: TNotifyEvent)
            : TDockableEditForm; override;
        procedure CreateNewName(const aNewName: string); override;
        function ReadAllNames: TStringArray; override;
    end;


implementation


uses
    SysUtils,
    ImportFileDefDataAdaptor,
    ImportDataTypesDataAdaptor,
    ImportDataAdaptor,
    ImportDefEditor,
    ImportFileDefEditor;

{ TImportFileDefViewItem }

function TImportFileDefViewItem.CreateDataAdaptor: TQueryDataAdaptor;
begin
    EXIT(TImportFileDefDataAdaptor.Create);
end;

function TImportFileDefViewItem.CreateEditForm(aOwner: TComponent; aOnSaveStatusChanged: TNotifyEvent)
    : TDockableEditForm;
begin
    result := TfrmImportFileDefEditor.Create(aOwner, self.Name, aOnSaveStatusChanged);
end;

procedure TImportFileDefViewItem.CreateNewName(const aNewName: string);
var
    xDataAdaptor: TImportFileDefDataAdaptor;
begin
    xDataAdaptor := TImportFileDefDataAdaptor.Create;
    try
        xDataAdaptor.AppendName(aNewName);
    finally
        xDataAdaptor.Free;
    end;
end;

procedure TImportFileDefViewItem.DeleteNamePhysically;
begin
    // Delete from IMPDATATYPES
    TImportDataTypesDataAdaptor.DeleteImportDef(self.Name);

    // Delete from IMPFILE
    inherited;
end;

function TImportFileDefViewItem.GetItemType: TViewItemType;
begin
    result := ntImportFileDef;
end;

function TImportFileDefViewItem.GetTypeCaption: string;
begin
    result := 'Import File';
end;

procedure TImportFileDefViewItem.SaveAs(const aSourceName, aTargetName: string);
begin
    // SaveAs for IMPFILE
    inherited;

    // SaveAs for IMPDATATYPES
    TImportDataTypesDataAdaptor.CopyImportDef(aSourceName, aTargetName);
end;

{ TImportDefViewItem }

procedure TImportDefViewItem.DeleteNamePhysically;
var
    xDA: TImportDefDataAdaptor;
begin
    // kein inherited

    xDA := TImportDefDataAdaptor.Create;
    try
        xDA.DeleteName(self.Name);
        xDA.ColDefsDataAdaptor.DeleteName(self.Name);
    finally
        xDA.Free;
    end;
end;

procedure TImportDefViewItem.SaveAs(const aSourceName, aTargetName: string);
var
    xDA: TImportDefDataAdaptor;
begin
    // kein inherited

    xDA := TImportDefDataAdaptor.Create;
    try
        DoSaveAs(xDA, aSourceName, aTargetName, self.TypeCaption);
        DoSaveAs(xDA.ColDefsDataAdaptor, aSourceName, aTargetName, self.TypeCaption);
    finally
        FreeAndNil(xDA);
    end;
end;

function TImportDefViewItem.CreateDataAdaptor: TQueryDataAdaptor;
begin
    EXIT(TImportDefDataAdaptor.Create);
end;

{ TVarImportDefViewItem }

function TVarImportDefViewItem.CreateEditForm(aOwner: TComponent; aOnSaveStatusChanged: TNotifyEvent)
    : TDockableEditForm;
begin
    result := TfrmImportDefEditor.Create(aOwner, self.Name, aOnSaveStatusChanged, idmMethVarImport);
end;

procedure TVarImportDefViewItem.CreateNewName(const aNewName: string);
var
    xDataAdaptor: TImportDefDataAdaptor;
begin
    xDataAdaptor := TImportDefDataAdaptor.Create;
    try
        xDataAdaptor.AppendName(aNewName, INT_DEF_MODE_METHVARIMPORT);
    finally
        xDataAdaptor.Free;
    end;
end;

function TVarImportDefViewItem.GetItemType: TViewItemType;
begin
    result := ntVarImportDef;
end;

function TVarImportDefViewItem.GetTypeCaption: string;
begin
    result := 'Variable Import';
end;

function TVarImportDefViewItem.ReadAllNames(): TStringArray;
var
    xDA: TImportDefDataAdaptor;
begin
    xDA := TImportDefDataAdaptor.Create;
    try
        result := xDA.ReadAllDefNames(INT_DEF_MODE_METHVARIMPORT);
    finally
        FreeAndNil(xDA);
    end;
end;

{ TTableImportDefViewItem }

function TTableImportDefViewItem.CreateEditForm(aOwner: TComponent; aOnSaveStatusChanged: TNotifyEvent)
    : TDockableEditForm;
begin
    result := TfrmImportDefEditor.Create(aOwner, self.Name, aOnSaveStatusChanged, idmTableImport);
end;

procedure TTableImportDefViewItem.CreateNewName(const aNewName: string);
var
    xDataAdaptor: TImportDefDataAdaptor;
begin
    xDataAdaptor := TImportDefDataAdaptor.Create;
    try
        xDataAdaptor.AppendName(aNewName, INT_DEF_MODE_TABLEIMPORT);
    finally
        xDataAdaptor.Free;
    end;
end;

function TTableImportDefViewItem.GetItemType: TViewItemType;
begin
    result := ntTableImportDef;
end;

function TTableImportDefViewItem.GetTypeCaption: string;
begin
    result := 'Table Import';
end;

function TTableImportDefViewItem.ReadAllNames(): TStringArray;
var
    xDA: TImportDefDataAdaptor;
begin
    xDA := TImportDefDataAdaptor.Create;
    try
        result := xDA.ReadAllDefNames(INT_DEF_MODE_TABLEIMPORT);
    finally
        FreeAndNil(xDA);
    end;
end;


end.
