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
  20.06.13 ts                                    TN6146   TLayoutDataAdaptorExt.cTemporaryMethodName anstatt TMethodDataAdaptorExt.cTemporaryMethodName
  ------------------------------------------------------------------------------------------------------------ }

unit MethodViewItem;


interface


uses
    Classes,
    QueryDataAdaptor,
    ViewItem,
    AppTypes,
    GeneralTypes,
    DockableForm;

type
    TMethodViewItem = class(TDatasetViewItem)
    protected
        function GetItemType(): TViewItemType; override;
        function GetTypeCaption(): string; override;
        function CreateDataAdaptor(): TQueryDataAdaptor; override;
        procedure DeleteNamePhysically(); override;
        procedure DeleteNameFromMemory(); override;
        function ItemNameExistsIntern(var vName: string; aCaseSensitive, aGoodResult: boolean)
            : boolean; override;
    public
        class function GetTableName(): string; override;
        function CheckNewName(aName: string): boolean; override;
        procedure SaveAs(const aSourceName, aTargetName: string); override;
        class function GetMethodEditorAttribute(const aMethodName: string): TMethodEditorAttribute;
        procedure AddName(const aName: string); override;
        procedure AddInfos(aInfos: TStrings); override;
        function SaveAsIsAllowed(): boolean; override;
        function DeleteIsAllowed(): boolean; override;
        function ReadAllNames: TStringArray; override;
        function CreateEditForm(aOwner: TComponent; aOnSaveStatusChanged: TNotifyEvent)
            : TDockableEditForm; override;
    end;


implementation


uses
    SysUtils,
    Generics.Collections,
    MethodEditor,
    MethodDataCache,
    AppSettings,
    MethodDataAdaptorExt,
    CommonTypes,
    MethodSettings,
    MethodDataAdaptor,
    MethodSettingsDataAdaptor,
    MethodDataUtilities,
    // Muss weg:
    ZADesignMain,
    LayoutDataAdaptorExt;

{ TMethodViewItem }

function TMethodViewItem.GetTypeCaption(): string;
begin
    result := TLanguageString.Read('Method', 'Methode');
end;

class function TMethodViewItem.GetTableName(): string;
begin
    result := 'Method';
end;

procedure TMethodViewItem.DeleteNameFromMemory();
begin
    TMethodDataUtilities.MethodDelete(self.Name);
end;

procedure TMethodViewItem.DeleteNamePhysically();
begin
    TMethodDataCache.Instance.DeleteMethodName(self.Name);
end;

function TMethodViewItem.CheckNewName(aName: string): boolean;
begin
    result := TMethodDataAdaptorExt.CheckNewName(aName);
end;

procedure TMethodViewItem.AddInfos(aInfos: TStrings);
var
    xCommentShort, xCommentLong: string;
begin
    inherited;

    TMethodSettings.LoadComments(self.Name, xCommentShort, xCommentLong);
    aInfos.Add(xCommentShort);
    aInfos.Add(xCommentLong);
end;

procedure TMethodViewItem.AddName(const aName: string);
begin
    TMethodDataCache.Instance.AddMethodName(aName);
end;

procedure TMethodViewItem.SaveAs(const aSourceName, aTargetName: string);
var
    xDA: TMethodSettingsDataAdaptor;
begin
    inherited SaveAs(aSourceName, aTargetName);

    xDA := TMethodSettingsDataAdaptor.Create();
    if Assigned(xDA) then
    begin
        try
            DoSaveAs(xDA, aSourceName, aTargetName);
        finally
            xDA.Free;
        end;
    end;
    // QryTools.gmRecSaveAsTarget( , 'METHODSETTINGS', GetTypeCaption(), 0 );
end;

function TMethodViewItem.DeleteIsAllowed: boolean;
begin
    result := false;
    if (TMethodViewItem.GetMethodEditorAttribute(self.Name) = meaDefault) and
        gCommonDll.LevelIsIncluded(gCommonDll.CurrentUser.Level, usrSystemAdmin) then
    begin
        result := true;
        EXIT;
    end;
end;

function TMethodViewItem.SaveAsIsAllowed: boolean;
var
    xAttr: TMethodEditorAttribute;
begin
    result := false;

    xAttr := TMethodViewItem.GetMethodEditorAttribute(self.Name);
    if (xAttr in [meaDefault, meaReadOnlyChangeView]) then
    begin
        result := true;
        EXIT;
    end;
end;

class function TMethodViewItem.GetMethodEditorAttribute(const aMethodName: string): TMethodEditorAttribute;
var
    xAttributes: TMethodSetAttributes;
begin
    if gCommonDll.LevelIsIncluded(gCommonDll.CurrentUser.Level, usrSystemAdmin) then
        result := meaDefault
    else
        result := meaReadOnlyFull;

    if frmEdMain.HiddenMethodsAreShown then
        EXIT;

    xAttributes := TMethodSettings.GetAttributes(aMethodName);
    if (msaHidden in xAttributes) then
    begin
        result := meaHidden
    end
    else if (msaReadOnly in xAttributes) and (result <> meaReadOnlyFull) then
    begin
        result := meaReadOnlyChangeView;
    end;
end;

function TMethodViewItem.GetItemType: TViewItemType;
begin
    result := ntMethod;
end;

function TMethodViewItem.ItemNameExistsIntern(var vName: string; aCaseSensitive: boolean;
    aGoodResult: boolean): boolean;
begin
    result := TMethodDataAdaptor.Instance.MethodExistsIntern(vName, aCaseSensitive)
end;

function TMethodViewItem.ReadAllNames: TStringArray;
var
    x: integer;
    xAllNames: TStringArray;
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
    result := TMethodDataAdaptor.Create();
end;

function TMethodViewItem.CreateEditForm(aOwner: TComponent; aOnSaveStatusChanged: TNotifyEvent)
    : TDockableEditForm;
begin
    result := TfrmMethodEditor.Create(aOwner, self.Name, aOnSaveStatusChanged);
end;


end.
