{ ------------------------------------------------------------------------------------------------------------
  Copyright  2009 Zinsser Analytic GmbH - All rights reserved.
  Author       : Wolfgang Lyncke (wl)
  Description  :
  ------------------------------------------------------------------------------------------------------------
  Revision History:
  date     op  method                            track-no improvement/change
  -------- --  --------------------------------  -------- ----------------------------------------------------
  16.01.09 wl                                    TN4362   TMethodViewItem extracted from ViewItems.pas
  16.06.09 wl                                    TN4605   uses MethodSettingsDataAdaptor
  04.08.09 ts  SaveAs                            TN4703   Uppercase entfernt
  06.08.09 wl                                    TN4702   Strings nicht mehr über RessourceLoader
  06.05.10 wl                                    TN5052   uses EditingWorkflow
  28.05.10 wl                                    TN5116   uses geändert
  09.06.10 wl  ChangeCommentsAllowwed            TN5116   entfernt
  07.08.12 wl  ReadAllNames                      TN5946   versteckt den temporären Methodennamen
  10.12.12 wl  CreateEditForm                    TN6045   --> TDesignerMethodViewItem
  20.02.13 wl  Delete,SaveAs                     TN6055   Kopiert und löscht auch MethodVariables
  13.03.13 wl                                    TN5960  HiddenMethodsAreShown --> ViewItemsWorkflow
  14.03.13 wl                                    TN5960   Erweiterungen, um im Runner, die richtigen Menüpunkte zu zeigen
  17.04.13 wl  SaveAs,DeleteNameFromMemory       TN6106   vereinfacht durch MethodDataManager
  17.04.13 wl  FileImport,FileExport             TN6106   neu
  20.06.13 ts                                    TN6146   TLayoutDataAdaptorExt.cTemporaryMethodName anstatt TMethodDataAdaptorExt.cTemporaryMethodName
  30.08.13 wl  ... IsAllowed                     TN6236   Anzeige der Menüpunkte in Abhängigkeit von User-Rechten verbessert
  29.11.13 wl  AddName                           TN6319   Neue Settings werden nur geschrieben, wenn keine existieren
  03.04.14 ts                                    TN6387   new ntMethodEditableInRunner
  ------------------------------------------------------------------------------------------------------------ }

unit MethodViewItem;


interface


uses
    Classes,
    QueryDataAdaptor,
    ViewItem,
    AppTypes,
    MethodSettingsDataAdaptor,
    DockableForm;

type
    TMethodViewItem = class abstract(TDatasetViewItem)
    private
        fIsRunner: boolean;
        class function GetMethodAttributeByUserLevel(): TVisibilityAttribute;
    protected
        function GetItemType(): TViewItemType; override;
        function GetTypeCaption(): string; override;
        function CreateDataAdaptor(): TQueryDataAdaptor; override;
        procedure DeleteNamePhysically(); override;
        procedure DeleteNameFromMemory(); override;
        function ItemNameExistsIntern(var vName: string; aCaseSensitive, aGoodResult: boolean)
            : boolean; override;
        function GetFileExportExtension: string; override;
        function CreateDefaultSettings(const aName: string): TMethodSettingsRec; virtual; abstract;
    public
        constructor Create(const aItemName: string; aIsRunner: boolean);

        class function GetTableName(): string; override;
        function CheckNewName(aName: string): boolean; override;
        procedure SaveAs(const aSourceName, aTargetName: string); override;
        function GetVisibilityAttribute(): TVisibilityAttribute; override;
        procedure AddName(const aName: string); override;
        function AddInfos: TArray<string>; override;
        function SaveAsIsAllowed(): boolean; override;
        function DeleteIsAllowed(): boolean; override;
        function ReadAllNames: TArray<string>; override;
        function IsStartable(): boolean; override;
        function IsEditableInRunner(): boolean; override;
        function GetIndividualIconIndex(): integer; override;
        function FileExportIsAllowed(): boolean; override;
        function FileImportIsAllowed(): boolean; override;
        procedure FileExport(); override;
        procedure FileImport(const aFileName: string); override;
        function OpenIsAllowed(): boolean; override;
    end;


implementation


uses
    SysUtils,
    Generics.Collections,
    MethodDataCache,
    AppSettings,
    MethodDataAdaptorExt,
    CommonTypes,
    ViewItemsWorkflow,
    MethodTypes,
    GeneralTypes,
    MethodDataManager,
    MethodDataAdaptor,
    LayoutDataAdaptor,
    MethodVariablesDataAdaptor,
    LayoutDataAdaptorExt;

{ TMethodViewItem }

constructor TMethodViewItem.Create(const aItemName: string; aIsRunner: boolean);
begin
    inherited Create(aItemName);
    fIsRunner := aIsRunner;
end;

function TMethodViewItem.GetTypeCaption(): string;
begin
    result := TLanguageString.Read('Method', 'Methode');
end;

class function TMethodViewItem.GetTableName(): string;
begin
    result := 'MethodSettings';
end;

procedure TMethodViewItem.DeleteNameFromMemory();
var
    xLayoutDA: TLayoutDataAdaptor;
    xDataManager: TMethodDataManager;
begin
    // Löschen wird durchgeführt
    xDataManager := TMethodDataManager.Create;
    try
        xDataManager.DeleteName(self.Name);
    finally
        FreeAndNil(xDataManager);
    end;

    // Löschen des zugehörigen Run-Layout
    xLayoutDA := TLayoutDataAdaptor.Create;
    try
        xLayoutDA.DeleteRun(self.Name);
    finally
        FreeAndNil(xLayoutDA);
    end;
end;

procedure TMethodViewItem.DeleteNamePhysically();
begin
    TMethodDataCache.Instance.DeleteMethodName(self.Name);
end;

procedure TMethodViewItem.FileExport;
var
    xDataManager: TMethodDataManager;
begin
    xDataManager := TMethodDataManager.Create;
    try
        xDataManager.ExportItem(self.Name);
    finally
        FreeAndNil(xDataManager);
    end;
end;

function TMethodViewItem.FileImportIsAllowed: boolean;
begin
    EXIT(true);
end;

function TMethodViewItem.FileExportIsAllowed: boolean;
begin
    EXIT(GetVisibilityAttribute() <> meaHidden);
end;

procedure TMethodViewItem.FileImport(const aFileName: string);
var
    xDataManager: TMethodDataManager;
begin
    xDataManager := TMethodDataManager.Create;
    try
        xDataManager.ImportItem(self.Name, aFileName);
    finally
        FreeAndNil(xDataManager);
    end;
end;

function TMethodViewItem.CheckNewName(aName: string): boolean;
begin
    result := TMethodDataAdaptorExt.CheckNewName(aName);
end;

function TMethodViewItem.AddInfos: TArray<string>;
var
    xRec: TMethodSettingsRec;
begin
    SetLength(result, 3);
    result[0] := self.FullCaption;

    xRec := TMethodDataCache.Instance.GetMethodSettings(self.Name);
    result[1] := TMethodSettingsDataAdaptor.GetStartableText(xRec);
    result[2] := xRec.Comment;
end;

procedure TMethodViewItem.AddName(const aName: string);
var
    xRec: TMethodSettingsRec;
    xDA: TMethodSettingsDataAdaptor;
begin
    xDA := TMethodSettingsDataAdaptor.Create;
    try
        xRec := xDA.ReadRec(aName);

        if (xRec.MethodName = '') then
        begin // Settings für diese Methode existieren nicht
            xRec := self.CreateDefaultSettings(aName);
            xDA.WriteRec(xRec);
        end;
    finally
        FreeAndNil(xDA);
    end;

    TMethodDataCache.Instance.AddMethodName(xRec);
end;

procedure TMethodViewItem.SaveAs(const aSourceName, aTargetName: string);
var
    xDA: TMethodDataManager;
begin
    xDA := TMethodDataManager.Create();
    try
        // Alle zugehörigen Tabellen kopieren
        DoSaveAs(xDA, aSourceName, aTargetName, self.TypeCaption);
    finally
        FreeAndNil(xDA);
    end;
end;

function TMethodViewItem.DeleteIsAllowed: boolean;
begin
    EXIT(GetVisibilityAttribute() = meaDefault);
end;

function TMethodViewItem.SaveAsIsAllowed: boolean;
var
    xAttr: TVisibilityAttribute;
begin
    if not gCommonDll.LevelIsIncluded(gCommonDll.CurrentUser.Level, usrSystemAdmin) then
        EXIT(false);

    xAttr := GetVisibilityAttribute();
    EXIT(xAttr in [meaDefault, meaReadOnly]);
end;

class function TMethodViewItem.GetMethodAttributeByUserLevel(): TVisibilityAttribute;
begin
    if gCommonDll.LevelIsIncluded(gCommonDll.CurrentUser.Level, usrSystemAdmin) then
        EXIT(meaDefault)
    else
        EXIT(meaReadOnly);
end;

function TMethodViewItem.GetVisibilityAttribute(): TVisibilityAttribute;
var
    xRec: TMethodSettingsRec;
begin
    if TViewItemsWorkflow.Instance.HiddenMethodsAreShown then
        EXIT(GetMethodAttributeByUserLevel());

    xRec := TMethodDataCache.Instance.GetMethodSettings(self.name);
    if (xRec.Attributes = TMethodSettingsDataAdaptor.cAttributeHidden) or
        (fIsRunner and not xRec.EditInRunner) then
    begin
        EXIT(meaHidden);
    end
    else if (xRec.Attributes = TMethodSettingsDataAdaptor.cAttributeReadOnly) then
    begin
        EXIT(meaReadOnly);
    end
    else
        EXIT(GetMethodAttributeByUserLevel());
end;

function TMethodViewItem.GetFileExportExtension: string;
begin
    EXIT(TMethodDataManager.cMethodFileExtension);
end;

function TMethodViewItem.GetIndividualIconIndex(): integer;
begin
    if IsEditableInRunner then
        EXIT(cImageIndexMethodEditableInRunner);
    if IsStartable() then
        EXIT(INT_IM_INDEX_STARTABLEMETHOD)
    else
        EXIT(INT_IM_INDEX_METHOD);
end;

function TMethodViewItem.GetItemType: TViewItemType;
begin
    result := ntMethod;
end;

function TMethodViewItem.IsStartable: boolean;
begin
    EXIT(TMethodDataCache.Instance.GetMethodSettings(self.name).Startable);
end;

function TMethodViewItem.IsEditableInRunner: boolean;
begin
    EXIT(TMethodDataCache.Instance.GetMethodSettings(self.name).EditInRunner);
end;

function TMethodViewItem.ItemNameExistsIntern(var vName: string; aCaseSensitive: boolean;
    aGoodResult: boolean): boolean;
var
    xDA: TMethodSettingsDataAdaptor;
begin
    xDA := TMethodSettingsDataAdaptor.Create;
    try
        EXIT(xDA.MethodExistsIntern(vName, aCaseSensitive));
    finally
        FreeAndNil(xDA);
    end;
end;

function TMethodViewItem.OpenIsAllowed: boolean;
begin
    EXIT(GetVisibilityAttribute() <> meaHidden);
end;

function TMethodViewItem.ReadAllNames: TArray<string>;
var
    x: integer;
    xAllNames: TArray<string>;
    xNewList: TList<string>;
begin
    xAllNames := inherited ReadAllNames;
    xNewList := TList<string>.Create;
    try
        for x := 0 to high(xAllNames) do
        begin
            if (xAllNames[x] <> TLayoutDataAdaptorExt.cTemporaryMethodName) then
                xNewList.Add(xAllNames[x]);
        end;

        EXIT(xNewList.ToArray);
    finally
        FreeAndNil(xNewList);
    end;
end;

function TMethodViewItem.CreateDataAdaptor: TQueryDataAdaptor;
begin
    result := TMethodSettingsDataAdaptor.Create();
end;


end.
