{ -----------------------------------------------------------------------------------------------------------
  Copyright © 2012 Zinsser Analytic GmbH - All rights reserved.
  Author       : Wolfgang Lyncke (wl)
  Description  :
  -----------------------------------------------------------------------------------------------------------
  Revision History:
  date     op  method                              track-no improvement/change
  -------- --  ----------------------------------  -------- -------------------------------------------------
  10.12.12 wl                                      TN6045   Initial Revision
  14.12.12 wl  TVariableViewItem                   TN6054   entfernt
  13.03.13 wl                                      TN5960   ist jetzt von TViewItemsWorkflow abgeleitet
  14.03.13 wl                                      TN5960   Erweiterungen, um im Runner, die richtigen Menüpunkte zu zeigen
  18.03.13 wl                                      TN6045   benutzt BuildingBlockMethodViewItem
  30.08.13 wl  OpenEditFormIntern                  TN6236   Abfrage auf OpenIsAllowed eingebaut
  03.04.14 ts  CreateAnyViewItem                   TN6387   new ntMethodEditableInRunner
  04.04.14 tp  Create, OpenEditFormIntern          TN6375   aBuildingBlockEditorMode: integer overload fuer neue BuildingBlock Editor
  ----------------------------------------------------------------------------------------------------------- }

unit ViewItemWorkflowRunner;


interface


uses
    DockableForm,
    ViewItemsWorkflow,
    ViewItemEditForm,
    ActionImages,
    ViewItem;

type
    TViewItemsWorkflowRunner = class(TViewItemsWorkflow)
    protected
        fBuildingBlockEditorMode: integer;
        function OpenEditFormIntern(aViewItem: TViewItem; aItemMustExist: boolean = false)
            : TDockableEditForm; override;
    public
        constructor Create(aActionImages: TfrmActionImages; aBuildingBlockEditorMode: integer);

        function CreateAnyViewItem(aViewType: TViewItemType; const aName: string): TViewItem; override;
    end;


implementation


uses
    Forms,
    ShellApi,
    Windows,
    SysUtils,
    CommonTypes,
    ZARunnerMain,
    BuildingBlockMethodViewItem,
    MethodViewItem,
    EdExtern,
    AppSettings;

{ TViewItemsWorkflowRunner }

constructor TViewItemsWorkflowRunner.Create(aActionImages: TfrmActionImages;
    aBuildingBlockEditorMode: integer);
var
    xEditAllowed: boolean;
begin
    fBuildingBlockEditorMode := aBuildingBlockEditorMode;
    if (aBuildingBlockEditorMode > 0) then
    begin
        xEditAllowed := true;
    end
    else
    begin
        xEditAllowed := false;
    end;
    inherited Create(aActionImages, xEditAllowed, dceStart);
end;

function TViewItemsWorkflowRunner.CreateAnyViewItem(aViewType: TViewItemType; const aName: string): TViewItem;
begin
    result := nil;
    case (aViewType) of
        ntFolder:
            result := TFolderViewItem.Create(aName);
        ntMethod, ntMethodEditableInRunner:
            result := TBuildingBlockMethodViewItem.Create(aName);
        ntFavFolder:
            result := TFolderViewItem.Create(aName);
        else
            ASSERT(false, 'Node does not have a corresponding ViewItem');
    end;
end;

function TViewItemsWorkflowRunner.OpenEditFormIntern(aViewItem: TViewItem; aItemMustExist: boolean)
    : TDockableEditForm;
var
    xCommandString: string;
    xParameters: string;
begin
    if (aViewItem.Name = '') or not OpenIsAllowed(aViewItem) then
        EXIT(nil);

    // testen ob es dieses Item gibt
    if (aItemMustExist) and (not aViewItem.ItemNameExists(true, true)) then
        EXIT(nil);

    if (fBuildingBlockEditorMode = 2) then
    begin
        xCommandString := ExtractFilePath(Application.ExeName) + 'ZinsserSharp.exe';
        xParameters := 'openbuildingblockeditor ' + aViewItem.name;
        ShellExecute(0, 'open', PChar(xCommandString), PChar(xParameters), nil, SW_SHOWNORMAL);
        EXIT(nil);
    end
    else
    begin
        EXIT(aViewItem.CreateEditForm(Application, nil));
    end;
end;


end.
