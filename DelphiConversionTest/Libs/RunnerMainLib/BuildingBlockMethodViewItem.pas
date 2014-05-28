{ -----------------------------------------------------------------------------------------------------------
  Copyright © 2012 Zinsser Analytic GmbH - All rights reserved.
  Author       : Wolfgang Lyncke (wl)
  Description  :
  -----------------------------------------------------------------------------------------------------------
  Revision History:
  date     op  method                              track-no improvement/change
  -------- --  ----------------------------------  -------- -------------------------------------------------
  10.12.12 wl                                      TN6045   Initial Revision
  18.03.13 wl                                      TN6045   AskNewName: Layout wird gleich mit abgefragt
  24.05.13 wl  ReadAllNamesForTree                 TN6159   Zeigt nur Startable-Methoden
  30.08.13 wl  CreateDefaultSettings               TN6236   neu
  30.08.13 wl                                      TN6237   verwendet TLayoutWorkspaceDataAdaptor.InstReadAllNames statt LayoutDataAdaptor
  ----------------------------------------------------------------------------------------------------------- }

unit BuildingBlockMethodViewItem;


interface


uses
    Classes,
    DockableForm,
    MethodSettingsDataAdaptor,
    MethodViewItem;

type
    TBuildingBlockMethodViewItem = class(TMethodViewItem)
    private
        fTempLayoutName: string;
    protected
        function CreateDefaultSettings(const aName: string): TMethodSettingsRec; override;
    public
        constructor Create(const aItemName: string);
        function AskNewName(var vNewName: string): boolean; override;
        function CreateEditForm(aOwner: TComponent; aOnSaveStatusChanged: TNotifyEvent)
            : TDockableEditForm; override;
        function ReadAllNamesForTree: TArray<string>; override;
    end;


implementation


uses
    Generics.Collections,
    SysUtils,
    DialogUtils,
    MethodDataCache,
    LayoutWorkspaceDataAdaptor,
    MethodDataAdaptorExt,
    BuildingBlockEditor;

function TBuildingBlockMethodViewItem.CreateDefaultSettings(const aName: string): TMethodSettingsRec;
begin
    result := TMethodSettingsDataAdaptor.GetEmptyRec;
    result.MethodName := aName;
    result.Startable := true;
    result.LayoutName := fTempLayoutName;
    result.EditInRunner := true;
end;

function TBuildingBlockMethodViewItem.AskNewName(var vNewName: string): boolean;
var
    xLayoutNames: TArray<string>;
begin
    result := inherited;
    if not result then
        EXIT;

    xLayoutNames := TLayoutWorkspaceDataAdaptor.InstReadAllNames();

    if Length(xLayoutNames) = 0 then
        fTempLayoutName := ''
    else if Length(xLayoutNames) = 1 then
        fTempLayoutName := xLayoutNames[0]
    else
    begin // Layout abfragen
        fTempLayoutName := TDialogUtils.SelectItemBox(xLayoutNames, 'Select layout for new method:',
            'New method');
        if (fTempLayoutName = '') then
            result := false;
    end;
end;

constructor TBuildingBlockMethodViewItem.Create(const aItemName: string);
begin
    inherited Create(aItemName, true);
end;

function TBuildingBlockMethodViewItem.CreateEditForm(aOwner: TComponent; aOnSaveStatusChanged: TNotifyEvent)
    : TDockableEditForm;
var
    xForm: TfrmBuildingBlockEditor;
begin
    xForm := TfrmBuildingBlockEditor.Create(nil);
    try
        xForm.LoadMethod(self);
        xForm.ShowModal;
    finally
        FreeAndNil(xForm);
    end;
    EXIT(nil);
end;

function TBuildingBlockMethodViewItem.ReadAllNamesForTree: TArray<string>;
begin
    EXIT(TMethodDataCache.Instance.ReadBuildingBlockNames);
end;


end.
