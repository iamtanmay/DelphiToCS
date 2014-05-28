{ ------------------------------------------------------------------------------------------------------------
  Copyright  2009 Zinsser Analytic GmbH - All rights reserved.
  Author       : Wolfgang Lyncke (wl)
  Description  : Base class for items of a special kind that are stored, have a name and can be created,
  deleted and copied
  ------------------------------------------------------------------------------------------------------------
  Revision History:
  date     op  method                            track-no improvement/change
  -------- --  --------------------------------  -------- ----------------------------------------------------
  16.01.09 wl                                    TN4362   Base class extracted from ViewItems.pas
  06.04.09 pk                                    TN4503   New: ntDisplayComponent
  16.06.09 wl                                    TN4606    ntRunTable, ntGroupTable entfernt
  11.08.09 wl                                    TN4702   Strings werden jetzt direkt geladen
  11.08.09 wl  ReadAllNames                      TN4702   TStringArray statt TStringList
  03.05.10 wl  TFolderViewItem                   TN5052   von SpecialViewItems hierher
  09.06.10 wl                                    TN5116   neu: ntFavFolder
  23.02.11 wl                                    TN5486   neu: ImportViewItems
  01.07.11 wl  TDatasetViewItem.DoSaveAs         TN5619   Exception statt nur einer Meldung und dann weitermachen
  14.12.11 wl                                    TN5765   ohne Session
  03.02.12 wl                                    TN5792   neue Namen: SubstanceSet,Substance statt ReagentRack,Reagent
  14.12.12 wl                                    TN6054   ntVariable entfernt
  14.03.13 ts  TViewItemType                     TN6092   new Icons für Startable methods (ntStartableMethod)
  14.03.13 wl                                    TN5960   Erweiterungen, um im Runner, die richtigen Menüpunkte zu zeigen
  27.03.13 pp  DeleteWithoutAsking               TN5975   Eintrag ohne Abfrage löschen
  17.04.13 wl  FileImport,FileExport             TN6106   neu
  24.05.13 wl  ReadAllNamesForTree               TN6159   speziell für ViewAllItems: Kann Teilmengen anzeigen
  30.08.13 wl  OpenIsAllowed                     TN6236   neu
  30.08.13 wl  FileImport-,FileExportIsAllowed   TN6236   getrennt
  03.04.14 ts                                    TN6387   new ntMethodEditableInRunner
  ------------------------------------------------------------------------------------------------------------ }

unit ViewItem;


interface


uses
    Classes,
    AppTypes,
    QueryDataAdaptor,
    DockableForm,
    GeneralTypes;

type
    TViewItemType = (ntAll, ntFolder, ntUnknown, ntAction, ntRack, ntCarrier, ntWorkspace, ntMethod,
        ntSequence, ntWashProg, ntLayout, ntSubstance, ntSubstanceSet, ntLiquidPar, ntPowderPar, ntSQLTerm,
        ntTipType, ntFavFolder, ntVarImportDef, ntTableImportDef, ntImportFileDef, ntDevice, ntDriver,
        ntConnection, ntDisplayComponent, ntMethodEditableInRunner);

    TViewItemTypes = set of TViewItemType;

    TViewItemDataRec = record
        name: string;
        ItemType: TViewItemType;
    end;

    TViewItemDblClickEventType = (dceOpen, dceStart);

    TViewItem = class
    private
        fItemName: string;
        function GetFullCaption(): string;
        function AskNameDialog(var vName: string): boolean;
    protected
        function GetItemType(): TViewItemType; virtual; abstract;
        function GetTypeCaption(): string; virtual; abstract;
        function ItemNameExistsIntern(var vName: string; aCaseSensitive, aGoodResult: boolean)
            : boolean; virtual;
        procedure DeleteNamePhysically(); virtual;
        procedure DeleteNameFromMemory(); virtual;
        function ConfirmDeleteName(): boolean; virtual;
        function GetFileExportExtension: string; virtual;
    public
        constructor Create(const aItemName: string);

        function AddInfos: TArray<string>; virtual;
        function ReadAllNames: TStringArray; virtual;
        function ReadAllNamesForTree: TStringArray; virtual;
        function ItemNameExists(aCaseSensitive: boolean; aGoodResult: boolean): boolean;
        procedure AddName(const aName: string); virtual;
        function AskNewName(var vNewName: string): boolean; virtual;
        function AskSaveAsName(var vName: string): boolean;
        function CheckNewName(aName: string): boolean; virtual;
        procedure SaveAs(const aSourceName, aTargetName: string); virtual;
        procedure CreateNewName(const aNewName: string); virtual;
        function DeleteWithoutAsking(): boolean;
        function DeleteName(): boolean;
        function SaveAsIsAllowed(): boolean; virtual;
        function NewIsAllowed(): boolean; virtual;
        function DeleteIsAllowed(): boolean; virtual;
        function CreateEditForm(aOwner: TComponent; aOnSaveStatusChanged: TNotifyEvent)
            : TDockableEditForm; virtual;
        function GetFullCaptionOfName(const aName: string): string;
        function IsStartable(): boolean; virtual;
        function IsEditableInRunner(): boolean; virtual;
        function GetIndividualIconIndex(): integer; virtual;
        function FileExportIsAllowed(): boolean; virtual;
        function FileImportIsAllowed(): boolean; virtual;
        procedure FileExport(); virtual;
        procedure FileImport(const aFileName: string); virtual;
        function OpenIsAllowed(): boolean; virtual;
        function GetVisibilityAttribute(): TVisibilityAttribute; virtual;

        property name: string read fItemName write fItemName;
        property FullCaption: string read GetFullCaption;
        property TypeCaption: string read GetTypeCaption;
        property ItemType: TViewItemType read GetItemType;
        property FileExportExtension: string read GetFileExportExtension;
    end;

    TFolderViewItem = class(TViewItem)
    protected
        function GetItemType(): TViewItemType; override;
        function GetTypeCaption(): string; override;
    end;

    TDatasetViewItem = class(TViewItem)
    protected
        function CreateDataAdaptor(): TQueryDataAdaptor; virtual;
        procedure DeleteNamePhysically(); override;
        class procedure DoSaveAs(aDataManager: TCustomDataManager; const aSourceName, aTargetName: string;
            const aTypeCaption: string);
    public
        procedure SaveAs(const aSourceName, aTargetName: string); override;
        class function GetTableName(): string; virtual;
        function ReadAllNames: TStringArray; override;
        function SaveAsIsAllowed(): boolean; override;
        function NewIsAllowed(): boolean; override;
        function DeleteIsAllowed(): boolean; override;
    end;


implementation


uses
    Windows,
    SysUtils,
    AppSettings,
    GUIManager,
    CommonTypes;

{ TViewItem }

constructor TViewItem.Create(const aItemName: string);
begin
    inherited Create();
    fItemName := aItemName;
end;

function TViewItem.CreateEditForm(aOwner: TComponent; aOnSaveStatusChanged: TNotifyEvent): TDockableEditForm;
begin
    result := nil;
end;

function TViewItem.GetFileExportExtension: string;
begin
    EXIT('');
end;

function TViewItem.GetFullCaption(): string;
begin
    result := self.GetFullCaptionOfName(fItemName);
end;

function TViewItem.GetFullCaptionOfName(const aName: string): string;
begin
    result := Format('%s: %s', [self.TypeCaption, aName]);
end;

function TViewItem.GetIndividualIconIndex(): integer;
begin
    // sollte nur für bestimmte Items überhaupt benutzt werden
    EXIT(-1);
end;

function TViewItem.GetVisibilityAttribute: TVisibilityAttribute;
begin
    EXIT(meaDefault);
end;

function TViewItem.CheckNewName(aName: string): boolean;
begin
    result := true;
end;

function TViewItem.ReadAllNames(): TStringArray;
begin
    // nichts hinzufügen
    EXIT(nil);
end;

function TViewItem.ReadAllNamesForTree: TStringArray;
begin
    // ist normalerweise mit ReadAllNames identisch
    EXIT(self.ReadAllNames());
end;

function TViewItem.AddInfos: TArray<string>;
begin
    // was über diesen Item in der Info angezeigt werden soll
    SetLength(result, 1);
    result[0] := self.FullCaption;
end;

procedure TViewItem.AddName(const aName: string);
begin
    //
end;

function TViewItem.IsStartable: boolean;
begin
    EXIT(false);
end;

function TViewItem.IsEditableInRunner: boolean;
begin
    EXIT(false);
end;

function TViewItem.ItemNameExists(aCaseSensitive, aGoodResult: boolean): boolean;
var
    xName: string;
begin
    xName := self.Name;
    result := self.ItemNameExistsIntern(xName, aCaseSensitive, aGoodResult);
end;

function TViewItem.ItemNameExistsIntern(var vName: string; aCaseSensitive, aGoodResult: boolean): boolean;
begin
    result := aGoodResult; // Wenn keine Prüfung existiert, soll es einfach weitergehen
end;

procedure TViewItem.SaveAs(const aSourceName, aTargetName: string);
begin
    // Dummy
end;

function TViewItem.AskNameDialog(var vName: string): boolean;
var
    xTypeName: string;
begin
    result := false;
    xTypeName := self.TypeCaption;

    if not gGUIManager.InputQuery(TLanguageString.Read('Save {0} {1} as ..', '{0} {1} speichern unter ..',
        [xTypeName, vName]), TLanguageString.Read('Save {0} as ..', '{0} speichern unter ..', [xTypeName]),
        vName) then
        EXIT;

    if (vName = '') then
        EXIT;

    result := true;
end;

function TViewItem.AskNewName(var vNewName: string): boolean;
begin
    result := AskNameDialog(vNewName);
end;

function TViewItem.AskSaveAsName(var vName: string): boolean;
var
    xNewName: string;
begin
    result := false;
    xNewName := vName;
    if not AskNameDialog(xNewName) then
        EXIT;
    if (vName = xNewName) then
        EXIT; // gleicher Name darf nicht sein
    vName := xNewName;
    result := true;
end;

procedure TViewItem.CreateNewName(const aNewName: string);
begin
    // create new dataset (if necessary)
end;

function TViewItem.DeleteIsAllowed: boolean;
begin
    result := false;
end;

function TViewItem.SaveAsIsAllowed(): boolean;
begin
    result := false;
end;

function TViewItem.NewIsAllowed: boolean;
begin
    result := false;
end;

function TViewItem.OpenIsAllowed: boolean;
begin
    EXIT(true);
end;

function TViewItem.DeleteName(): boolean;
begin
    result := false;

    // Name muss existieren
    if not ItemNameExists(true, true) then
        EXIT;

    // Nachfragen beim User
    if not self.ConfirmDeleteName() then
        EXIT;
    DeleteNameFromMemory();
    DeleteNamePhysically();

    result := true;
end;

function TViewItem.DeleteWithoutAsking(): boolean;
begin
    result := false;

    // Name muss existieren
    if not ItemNameExists(true, true) then
        EXIT;

    DeleteNameFromMemory();
    DeleteNamePhysically();

    result := true;
end;

procedure TViewItem.FileExport;
begin
    //
end;

function TViewItem.FileExportIsAllowed: boolean;
begin
    EXIT(false);
end;

function TViewItem.FileImportIsAllowed: boolean;
begin
    EXIT(false);
end;

procedure TViewItem.FileImport(const aFileName: string);
begin

end;

function TViewItem.ConfirmDeleteName(): boolean;
begin
    result := TAppSettings.ItemConfirmDelete(self.TypeCaption, self.Name);
end;

procedure TViewItem.DeleteNameFromMemory();
begin

end;

procedure TViewItem.DeleteNamePhysically;
begin

end;

{ TFolderViewItem }

function TFolderViewItem.GetItemType: TViewItemType;
begin
    result := ntFolder;
end;

function TFolderViewItem.GetTypeCaption: string;
begin
    result := 'Folder';
end;

{ TDatasetViewItem }

function TDatasetViewItem.CreateDataAdaptor: TQueryDataAdaptor;
begin
    result := nil;
end;

procedure TDatasetViewItem.SaveAs(const aSourceName, aTargetName: string);
var
    xDA: TQueryDataAdaptor;
begin
    xDA := CreateDataAdaptor();
    if Assigned(xDA) then
    begin
        try
            DoSaveAs(xDA, aSourceName, aTargetName, self.TypeCaption);
        finally
            xDA.Free;
        end;
    end;
end;

class procedure TDatasetViewItem.DoSaveAs(aDataManager: TCustomDataManager;
    const aSourceName, aTargetName: string; const aTypeCaption: string);
begin
    if aDataManager.NameExists(aTargetName) then
        raise Exception.Create(TLanguageString.Read('The {0} {1} already exist!"',
            '{0} {1} existiert bereits!', [aTypeCaption, aTargetName]));

    aDataManager.SaveNameAs(aSourceName, aTargetName);
end;

class function TDatasetViewItem.GetTableName(): string;
begin
    result := '';
end;

function TDatasetViewItem.NewIsAllowed: boolean;
begin
    result := true;
end;

function TDatasetViewItem.DeleteIsAllowed: boolean;
begin
    result := true;
end;

function TDatasetViewItem.SaveAsIsAllowed: boolean;
begin
    result := true;
end;

function TDatasetViewItem.ReadAllNames(): TStringArray;
var
    xDA: TQueryDataAdaptor;
begin
    xDA := CreateDataAdaptor();
    if not Assigned(xDA) then
        EXIT;

    try
        result := xDA.ReadAllNames();
    finally
        xDA.Free;
    end;
end;

procedure TDatasetViewItem.DeleteNamePhysically();
var
    xDA: TQueryDataAdaptor;
begin
    xDA := CreateDataAdaptor();
    if Assigned(xDA) then
    begin
        try
            xDA.DeleteName(self.Name);
        finally
            xDA.Free;
        end;
    end;
end;


end.
