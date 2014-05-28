{ --------------------------------------------------------------------------------------------------
  Copyright © 2003 Zinsser Analytic GmbH - All rights reserved.
  Author       : Wolfgang Lyncke (wl)
  Project      : Layouter
  Description  : List View especiallly for editing Settings.db
  --------------------------------------------------------------------------------------------------
  Revision History:
  Date     op  Method                       Track-no Improvement/Change
  -------- --  ---------------------------  -------- -----------------------------------------------
  15.01.03 wl                               TN1293.3 initial version
  16.01.03 wl                               TN1293.3 Strings in Ressourcen
  16.01.03 wl  AskForSave                   TN1293.3 fragt ob gespeichert werden soll
  16.01.03 wl  lvColumnClick                TN1293.3 Sortierung der Einträge
  17.01.03 wl  lvDragDrop, lvDragOver       TN1293.3 Drag/Drop-Funktion für ListItems integriert
  31.01.03 wl  PopupMenu1Popup              TN1293.3 stürzt nicht mehr ab wenn man irgendwo hin klickt
  05.02.03 wl                               TN1293.3 stark überarbeitet; neu: Remove all values, Convert-funktion
  06.02.03 wl  EnableEditFunction           TN1334.3 Zusatzfunktion zu Create: User wird übergeben, das ListView editierbar
  16.08.05 wl  mnuRemoveAreaClick           TN2558.4 IIniAccess.DeleteAllValues - ohne Parameter
  31.08.07 wl                               TN3811.4 uses IniEntry & IniAccess
  04.09.07 wl                               TN3811.4 Bug entfernt
  11.06.08 wl  fZinsserPlatform             TN4143   entfernt
  13.01.09 wl  CreatePopup                  TN4312   statisches Array entfernt
  06.08.09 wl  TSettingsMainStringLoader    TN4702   Strings werden direkt geladen
  09.02.10 pk                               TN4973   UnitName changed to ValueUnitName to avoid overwriting TObject.UnitName
  27.10.10 wl                               TN5300   Eigenschaft ValueHidden wird bei der Anzeige mit Sternchen angezeigt
  09.05.12 wl  fDataCache                   TN5889   neu, wird für ConvertAndSave benötigt
  -------------------------------------------------------------------------------------------------- }

unit IniAreaListView;


interface


uses
    Classes,
    Controls,
    ComCtrls,
    CommonTypes,
    IniAccess,
    SettingsDataCache,
    IniEntry;

type
    TIniAreaListView = class(TListView)
    private const
        INT_INIAREA_NOENTRY = 0;
        INT_INIAREA_ENTRY_EXISTS = 2;
        INT_INIAREA_ENTRY_NOT_EXISTS = 1;
        INT_INIAREA_ENTRY_EDITED = 3;
        INT_INIAREA_ENTRY_REMOVED = 4;
        cHiddenValue = '*****';
    private
        FAliasName, FDataPath: string;
        FColumnToSort: Integer;
        FUnsaved: boolean;
        FIniAccess: TIniAccess;
        FCurrentUser: IUser;
        fDataCache: TSettingsDataCache;
        procedure CreatePopup;
        procedure PopupMenu1Popup(Sender: TObject);
        procedure mnuEditValueClick(Sender: TObject);
        procedure mnuRemoveValueClick(Sender: TObject);
        procedure mnuRemoveAreaClick(Sender: TObject);
        procedure mnuSaveAllValuesClick(Sender: TObject);
        procedure mnuConvertValuesClick(Sender: TObject);
        procedure mnuAddValueClick(Sender: TObject);
        procedure Reload;
        function AddEntry(aIniEntry: TIniEntry): TListItem;
        function FindEntry(aIniEntry: TIniEntry): TListItem;
        procedure lvColumnClick(Sender: TObject; Column: TListColumn);
        procedure lvCompare(Sender: TObject; Item1, Item2: TListItem; Data: Integer; var Compare: Integer);
        procedure lvDragOver(Sender, Source: TObject; X, Y: Integer; State: TDragState; var Accept: Boolean);
        procedure lvDragDrop(Sender, Source: TObject; X, Y: Integer);
        procedure lvKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
        function GetArea: string;
    public
        // constructor / destructor
        constructor Create(aOwner: TComponent); override;
        destructor Destroy; override;

        function AskForSave(var aLastAreaEmpty: boolean): boolean;
        function SetIniAccess(aIniAccess: TIniAccess; var aLastAreaEmpty: boolean): boolean;
        function GetIniEntry(aListItem: TListItem): TIniEntry;
        procedure AddForeignIniEntry(aForeignEntry: TIniEntry);
        procedure EnableConvertFunction(aAliasName, aDataPath: string);
        procedure EnableEditFunction(aCurrentUser: IUser; aDataCache: TSettingsDataCache; aDragging: boolean);
        property Area: string read GetArea;
    end;


implementation


uses
    Sysutils,
    Forms,
    Menus,
    Dialogs,
    Windows,
    DialogUtils,
    GeneralTypes;

{ TIniAreaListView }

constructor TIniAreaListView.Create(aOwner: TComponent);
var
    xListColumn: TListColumn;
begin
    inherited Create(aOwner);

    RowSelect := true;
    ViewStyle := vsReport;
    FUnsaved := false;
    FAliasName := '';
    FDataPath := '';

    OnColumnClick := lvColumnClick;
    OnCompare := lvCompare;

    xListColumn := Columns.Add;
    xListColumn.Caption := TLanguageString.Read('Section', 'Sektion');
    xListColumn.Width := 110;
    xListColumn := Columns.Add;
    xListColumn.Caption := TLanguageString.Read('Identifier', 'Identifizierer');
    xListColumn.Width := 110;
    xListColumn := Columns.Add;
    xListColumn.Caption := TLanguageString.Read('Value', 'Wert');
    xListColumn.Width := 100;
    xListColumn := Columns.Add;
    xListColumn.Caption := TLanguageString.Read('Default value', 'Vorgabewert');
    xListColumn.Width := 100;
    xListColumn := Columns.Add;
    xListColumn.Caption := TLanguageString.Read('Unit', 'Einheit');
    xListColumn.Width := 70;
    xListColumn := Columns.Add;
    xListColumn.Caption := TLanguageString.Read('Description', 'Beschreibung');
    xListColumn.Width := 400;
end;

procedure TIniAreaListView.EnableEditFunction(aCurrentUser: IUser; aDataCache: TSettingsDataCache;
    aDragging: boolean);
begin
    FCurrentUser := aCurrentUser;
    fDataCache := aDataCache;

    if (FCurrentUser = nil) or (not FCurrentUser.HasLevel(usrSystemAdmin)) then
        exit;

    OnDblClick := mnuEditValueClick;
    OnDragOver := lvDragOver;
    OnDragDrop := lvDragDrop;
    OnKeyDown := lvKeyDown;
    CreatePopup;

    if (aDragging) then
        DragMode := dmAutomatic;
end;

destructor TIniAreaListView.Destroy;
begin
    FIniAccess.Free;

    inherited Destroy;
end;

procedure TIniAreaListView.lvKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
    if (Key = VK_DELETE) and (Selected <> nil) then
        mnuRemoveValueClick(Sender);
end;

function TIniAreaListView.AskForSave(var aLastAreaEmpty: boolean): boolean;
var
    xButton: integer;
begin
    if (FIniAccess <> nil) then
        aLastAreaEmpty := FIniAccess.AreaEmpty;
    result := true;

    if (FUnsaved) then
    begin
        aLastAreaEmpty := false;
        xButton := TDialogUtils.MessageBox(TLanguageString.Read('Save changes in Settings Area {0}?',
            'Änderungen in Einstellbereich {0} speichern?', [FIniAccess.FileName]),
            TLanguageString.Read('Edit Settings', 'Einstellungen bearbeiten'),
            MB_ICONQUESTION + MB_YESNOCANCEL);

        if (xButton = IDYES) then
        begin
            if FIniAccess.SaveAllValues(FCurrentUser) then
                aLastAreaEmpty := FIniAccess.AreaEmpty
            else
                result := false;
        end;

        if (xButton = IDCANCEL) then
            result := false;
    end;
end;

function TIniAreaListView.SetIniAccess(aIniAccess: TIniAccess; var aLastAreaEmpty: boolean): boolean;
begin
    result := false;
    if (aIniAccess <> nil) then
    begin
        if (AskForSave(aLastAreaEmpty)) then
        begin
            FIniAccess.Free;
            FIniAccess := aIniAccess;
            result := true;
            Reload;
        end;
    end;
end;

procedure TIniAreaListView.CreatePopup;
var
    xMenuItems: array of TMenuItem;
begin
    SetLength(xMenuItems, 8);
    xMenuItems[0] := NewItem(TLanguageString.Read('&Edit value', 'Wert &bearbeiten'), 0, false, true,
        mnuEditValueClick, 0, 'mnuEditValue');
    xMenuItems[1] := NewItem(TLanguageString.Read('&Remove value', 'Wert &entfernen'), 0, false, true,
        mnuRemoveValueClick, 0, 'mnuRemoveValue');
    xMenuItems[2] := NewItem('-', 0, false, true, nil, 0, 'mnuN1');
    xMenuItems[3] := NewItem(TLanguageString.Read('&Add value', 'Wert &hinzufügen'), 0, false, true,
        mnuAddValueClick, 0, 'mnuAddValue');
    xMenuItems[4] := NewItem(TLanguageString.Read('&Save all values', 'Alle Werte &speichern'), 0, false,
        true, mnuSaveAllValuesClick, 0, 'mnuSaveAllValues');
    xMenuItems[5] := NewItem(TLanguageString.Read('Convert settings area to ini file',
        'Einstellbereich in eine INI-Datei konvertieren'), 0, false, true, mnuConvertValuesClick, 0,
        'mnuConvertValues');
    xMenuItems[6] := NewItem('-', 0, false, true, nil, 0, 'mnuN2');
    xMenuItems[7] := NewItem(TLanguageString.Read('&Remove all(!) values', 'Alle (!) Werte &entfernen'), 0,
        false, true, mnuRemoveAreaClick, 0, 'mnuRemoveArea');
    self.PopupMenu := NewPopupMenu(self, 'PopupMenu1', paRight, true, xMenuItems);
    self.PopupMenu.OnPopup := self.PopupMenu1Popup;
end;

procedure TIniAreaListView.mnuSaveAllValuesClick(Sender: TObject);
begin
    if FIniAccess.SaveAllValues(FCurrentUser) then
        Reload;
end;

procedure TIniAreaListView.mnuEditValueClick(Sender: TObject);
var
    xIniEntry: TIniEntry;
begin
    if (Selected = nil) then
        exit;

    xIniEntry := GetIniEntry(Selected);

    // darf eigentlich nicht passieren !!
    if (xIniEntry = nil) then
    begin
        TDialogUtils.MessageBox(TLanguageString.Read('Identifier [{0}] {1} does not exist!',
            'Identifizierer [{0}] {1} existiert nicht!', [Selected.Caption, Selected.SubItems[0]]),
            TLanguageString.Read('Edit Value', 'Wert bearbeiten'), 16);
        exit;
    end;

    if (FIniAccess.EditDialog(xIniEntry)) then
    begin
        if (Selected.SubItems[1] <> xIniEntry.Value) then
        begin
            if (xIniEntry.ValueHidden) and (xIniEntry.Value <> '') then
                Selected.SubItems[1] := cHiddenValue
            else
                Selected.SubItems[1] := xIniEntry.Value;
            Selected.SubItemImages[1] := INT_INIAREA_ENTRY_EDITED;
            FUnsaved := true;
        end;
    end;
end;

function TIniAreaListView.GetIniEntry(aListItem: TListItem): TIniEntry;
begin
    result := FIniAccess.GetItemByIdent(aListItem.Caption, aListItem.SubItems[0], false);
end;

procedure TIniAreaListView.mnuRemoveValueClick(Sender: TObject);
var
    xButton: integer;
begin
    if (Selected = nil) then
        exit;

    xButton := TDialogUtils.MessageBox(TLanguageString.
        Read('Do you really want to delete [{0}] {1} (value: {2})?',
        'Möchten Sie wirklich [{0}] {1} (Wert: {2}) löschen?', [Selected.Caption, Selected.SubItems[0],
        Selected.SubItems[1]]), TLanguageString.Read('Remove Value', 'Wert entfernen'),
        MB_ICONQUESTION + MB_YESNO + MB_DEFBUTTON2);

    if (xButton = IDYES) then
    begin
        if FIniAccess.DeleteKey(Selected.Caption, Selected.SubItems[0], false) then
        begin
            Selected.SubItems[1] := '';
            Selected.SubItemImages[1] := INT_INIAREA_ENTRY_NOT_EXISTS;
            FUnsaved := true;
        end;
    end;
end;

procedure TIniAreaListView.mnuRemoveAreaClick(Sender: TObject);
var
    x: integer;
begin
    if (FIniAccess = nil) then
        exit;
    if (FIniAccess.AreaEmpty) then
        exit;

    if FIniAccess.DeleteAllValues() then
    begin
        for x := 0 to Items.Count - 1 do
        begin
            Items[x].SubItems[1] := '';
            Items[x].SubItemImages[1] := INT_INIAREA_ENTRY_NOT_EXISTS;
        end;
        FUnsaved := true;
    end;
end;

procedure TIniAreaListView.mnuAddValueClick(Sender: TObject);
var
    xIniEntry: TIniEntry;
    xListItem: TListItem;
begin
    xIniEntry := FIniAccess.AddDialog;
    if (xIniEntry <> nil) then
    begin

        // the entry could be in the list if it has been removed before
        xListItem := FindEntry(xIniEntry);

        if (xListItem = nil) then
            xListItem := AddEntry(xIniEntry);

        // show the new item
        xListItem.MakeVisible(false);
        xListItem.Selected := true;

        xListItem.SubItemImages[1] := INT_INIAREA_ENTRY_EDITED;
        FUnsaved := true;
    end;
end;

procedure TIniAreaListView.PopupMenu1Popup(Sender: TObject);
begin
    PopupMenu.Items[0].Visible := false;
    PopupMenu.Items[1].Visible := false;
    PopupMenu.Items[5].Visible := false;
    PopupMenu.Items[6].Visible := false;
    PopupMenu.Items[7].Visible := false; // remove all items

    if (Selected <> nil) then
    begin
        PopupMenu.Items[0].Visible := true;
        case (Selected.SubItemImages[1]) of
            INT_INIAREA_ENTRY_EXISTS:
                PopupMenu.Items[1].Visible := true; // Items[1]: 'Remove Value'
            INT_INIAREA_ENTRY_EDITED:
                PopupMenu.Items[1].Visible := true;
            else
        end;
    end;

    PopupMenu.Items[5].Visible := true; // // Items[5]: Convert area

    if (FIniAccess <> nil) and (not FIniAccess.IsCheckingAccess) then
    begin
        PopupMenu.Items[6].Visible := true;
        PopupMenu.Items[7].Visible := true; // // Items[7]: Remove all items
    end;
end;

function TIniAreaListView.AddEntry(aIniEntry: TIniEntry): TListItem;
begin
    result := Items.Add;
    result.Caption := aIniEntry.Section;
    result.SubItems.Add(aIniEntry.Ident);
    if (not aIniEntry.ValueExists) then
    begin
        result.SubItems.Add('');
        result.SubItemImages[1] := INT_INIAREA_ENTRY_NOT_EXISTS;
    end
    else
    begin
        if (aIniEntry.ValueHidden) then
            result.SubItems.Add('*****')
        else
            result.SubItems.Add(aIniEntry.Value);
        result.SubItemImages[1] := INT_INIAREA_ENTRY_EXISTS;
    end;
    result.SubItems.Add(aIniEntry.DefaultValue);
    result.SubItems.Add(aIniEntry.ValueUnitName);
    result.SubItems.Add(aIniEntry.Description);
end;

function TIniAreaListView.FindEntry(aIniEntry: TIniEntry): TListItem;
var
    x: integer;
begin
    result := nil;

    for x := 0 to Items.Count - 1 do
    begin
        if (Items[x].Caption = aIniEntry.Section) and (Items[x].SubItems[0] = aIniEntry.Ident) then
            result := Items[x];
    end;
end;

procedure TIniAreaListView.Reload;
var
    x: integer;
begin
    FUnsaved := false;

    // Made invisible to speed up the loading procedure
    Visible := false;

    // exit if IniAccess is not set
    if (FIniAccess = nil) then
        exit;

    if (PopupMenu <> nil) then
        if (FIniAccess.AccessType = iatDatabaseAccess) then
            PopupMenu.Items[5].Caption := TLanguageString.Read('Convert settings area to ini file',
                'Einstellbereich in eine INI-Datei konvertieren')
        else
            PopupMenu.Items[5].Caption := TLanguageString.Read('Convert ini file to settings area',
                'INI-Datei in einen Einstellbereich konvertieren');

    // Read all known values from the ini file
    FIniAccess.LoadAllValues;

    // Write values in the list view
    Items.Clear;
    for x := 0 to FIniAccess.Count - 1 do
        AddEntry(FIniAccess[x]);

    // alpha sort
    lvColumnClick(self, Columns.Items[0]);

    Visible := true;
end;

procedure TIniAreaListView.lvColumnClick(Sender: TObject; Column: TListColumn);
begin
    FColumnToSort := Column.Index;
    (Sender as TCustomListView).AlphaSort;
end;

// Die Ereignisbehandlungsroutine OnCompare veranlaßt die Listenansicht, die Sortierung anhand der
// markierten Spalte vorzunehmen:

procedure TIniAreaListView.lvCompare(Sender: TObject; Item1, Item2: TListItem; Data: Integer;
    var Compare: Integer);
var
    ix: Integer;
begin
    if FColumnToSort = 0 then
    begin
        Compare := CompareText(Item1.Caption, Item2.Caption)
    end
    else
    begin
        ix := FColumnToSort - 1;
        Compare := CompareText(Item1.SubItems[ix], Item2.SubItems[ix]);
    end;

end;

// Hinweis
// Die Ereignisbehandlungsroutine OnCompare verwendet die globale Funktion CompareText.
// Eventuell möchten Sie die sortierung unter Berücksichtigung der Groß-/Kleinschreibung
// oder der Ländereinstellungen vornehmen. Verwenden Sie dafür AnsiCompareText, CompareStr oder AnsiCompareStr.

procedure TIniAreaListView.lvDragOver(Sender, Source: TObject; X, Y: Integer; State: TDragState;
    var Accept: Boolean);
begin
    Accept := false;
    if (Sender is TIniAreaListView) and (Source is TIniAreaListView) and (Sender <> Source) then
        Accept := true;
end;

procedure TIniAreaListView.lvDragDrop(Sender, Source: TObject; X, Y: Integer);
var
    xIniEntry: TIniEntry;
begin
    if (Sender is TIniAreaListView) and (Source is TIniAreaListView) and (Sender <> Source) then
    begin

        // Get Ini Entry from other list view
        xIniEntry := (Source as TIniAreaListView)
            .GetIniEntry((Source as TIniAreaListView).Selected as TListItem);

        // Add IniEntry to List view
        (Sender as TIniAreaListView).AddForeignIniEntry(xIniEntry);
    end;
end;

procedure TIniAreaListView.AddForeignIniEntry(aForeignEntry: TIniEntry);
var
    x: integer;
    xFound: boolean;
    xIniEntry: TIniEntry;
    xListItem: TListItem;
begin
    xIniEntry := FIniAccess.AddForeignIniEntry(aForeignEntry);
    if (xIniEntry <> nil) then
    begin
        FUnsaved := true;

        // seek item that represents the ini entry
        xFound := false;
        for x := 0 to Items.Count - 1 do
            if (Items[x].Caption = xIniEntry.Section) and (Items[x].SubItems[0] = xIniEntry.Ident) then
            begin
                // show the new item
                Items[x].MakeVisible(false);
                Items[x].Selected := true;

                Items[x].SubItems[1] := aForeignEntry.Value;
                Items[x].SubItemImages[1] := INT_INIAREA_ENTRY_EDITED;
                xFound := true;
            end;

        // if item does not exist, add a new one
        if (not xFound) then
        begin
            xListItem := AddEntry(xIniEntry);

            // show the new item
            xListItem.MakeVisible(false);
            xListItem.Selected := true;

            xListItem.SubItemImages[1] := INT_INIAREA_ENTRY_EDITED;
        end;
    end;
end;

function TIniAreaListView.GetArea: string;
begin
    result := '';
    if (FIniAccess <> nil) then
        result := FIniAccess.Area;
end;

procedure TIniAreaListView.EnableConvertFunction(aAliasName, aDataPath: string);
begin
    FAliasName := aAliasName;
    FDataPath := aDataPath;
end;

procedure TIniAreaListView.mnuConvertValuesClick(Sender: TObject);
var
    xLastAreaEmpty: boolean;
begin
    if (AskForSave(xLastAreaEmpty)) and (not xLastAreaEmpty) then
        FIniAccess.ConvertAndSave(FCurrentUser, fDataCache, FDataPath);
end;


end.
