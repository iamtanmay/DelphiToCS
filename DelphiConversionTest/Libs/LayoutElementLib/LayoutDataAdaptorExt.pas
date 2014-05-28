{ --------------------------------------------------------------------------------------------------
  Copyright © 2003 Zinsser Analytic GmbH - All rights reserved.
  Author       : Wolfgang Lyncke (wl)
  Description  : Combination of Layout Data Adaptor with access to Tipset-,LayoutWorkspace-,MethodDataAdaptor
  --------------------------------------------------------------------------------------------------
  Revision History:
  date     op  method                       track-no improvement/change
  -------- --  ---------------------------  -------- -----------------------------------------------
  07.02.03 wl                               TN1334.3 initial version
  11.02.03 wl                               TN1334.3 neu: Instance-function (ist jetzt ein Singleton-Object)
  11.02.03 wl  GetReason                    TN1334.3 --> DataAdaptor
  05.06.03 wl                               TN1476   Tabellenname für Tipset.db korrigiert
  03.03.04 wl  Delete                       TN1784   exit after message "Layout %s can not be deleted"
  09.03.04 pk                               TN1634   New: ObtainRunLayouts, ValidRunLayoutExistsInList
  09.03.04 pk  Delete                       TN1634.1   Avoids deletion only if "Valid" run layout exists.
  09.03.04 pk  Delete                       TN1634.2   Deletes zombie Run tables.
  16.03.04 wl  Delete                       TN1634.2   zweimal xRunLayoutList.Free muss nicht sein (führt zu Schutzverletzungen)
  21.07.04 pk                               TN2049    New: SelectDistinctLayout, SelectDistinctRunLayout
  02.08.04 pk  WriteRunLayout               TN2068   New
  04.03.05 pk                               TN2330.1 call TAppSettings.GetReason instead of TDataAdaptor.GetReason
  24.11.05 pk                               TN2805   DeleteRunLayout from DBRack
  27.04.07 wl  Delete                       TN3669   benutzt TAppSettings.ItemConfirmDeleteName
  04.05.07 wl  SaveAs                       TN3669   benutzt TAppSettings.ItemConfirmAdd
  07.08.07 wl                               TN3811.3 benutzt Delete,SaveAs von TDatasetUtils
  30.08.07 pk                               TN3840.1 Massive Changes: inherits from TLayoutDataAdaptor
  02.10.07 wl                               TN3811.5  uses dbTables entfernt: TDBDataset durch TDataset ersetzt
  09.11.07 pk                               TN3922   Dataset changed to DataProvider
  07.01.08 pk  DeleteRunLayout              TN3922   Delete RunDB only if it exists
  02.06.08 pk  DeleteRunLayout              TN4130   Delete Posinfo before RunLayout
  27.06.08 pk  DeleteAllLayoutRelated       TN4139   New: also deletes LayoutWorkspace
  02.07.08 pk  SaveAs                       TN4139   Also save layoutworkspace
  16.07.08 pk                               TN4139  changes needed for reading racks from runlayout
  21.07.08 pk  CopyCarriersToRunLayoutForLayout      TN4179   New
  02.09.08 pk  DeleteRunLayout              TN4215  Deleting of Run removed
  23.09.08 wl                               TN4236   Vereinigung mit ..Fieldnames
  16.01.09 wl                               TN4362   an Änderungen in TQueryDataAdaptor angepasst
  05.08.09 ts  DeleteName                   TN4569   Layoutnamen aus LAYOUTWORKSPACE-table müssen dort gelöscht werden, wenn sie keine Carrier/Racks haben
  05.08.09 ts  SaveAs                       TN4569   SaveAs -> aName muss nicht in LAYOUT-table vorhanden sein, da Layoutnamen aus LAYOUTWORKSPACE kommen
  10.08.09 wl                               TN4702   Strings werden jetzt direkt geladen
  04.11.09 pk                               TN4843   Replace TObjectList, TStringList, TStrings with Generics Lists and TStringArray
  17.06.10 pk                               TN5152.1 Change SQLs so that they also work for TurboDB (dbl quoatations to single)
  05.11.10 wl  SaveAs                       TN5333   SQL changed
  10.02.11 wl                               TN5475   Zugriffe auf ..DataAdaptor.Instance geändert
  14.12.11 wl  DeleteRunLayout,PrepareRunData  TN5765   --> RunGlobals
  27.12.11 wl  ReloadRunLayoutAndDeletePosinfo TN5768   von RunGlobals hierher verschoben
  13.03.12 wl  ReloadRunLayoutAndDeletePosinfo TN5798   zerteilt in DeleteRunLayoutAndPosinfo & WriteRunLayoutIfNecessary
  14.03.12 wl  DeleteNameWithConfirm           TN5831   Wenn es noch Methoden gibt, die das Layout verwenden, wird abgebrochen
  19.04.13 wl  DeleteName                      TN6106   kein Rückgabewert
  20.06.13 ts  WriteRunLayoutIfNecessary       TN6146   cTemporaryMethodName wird immer gelöscht, sonst evtl. kein Layout bei Init/Flush
  30.08.13 wl  DeleteNameWithConfirm           TN6236   verwendet TMethodSettingsDataAdaptor
  -------------------------------------------------------------------------------------------------- }

unit LayoutDataAdaptorExt;


interface


uses
    DataProvider,
    LayoutDataAdaptor;

type
    TLayoutDataAdaptorExt = class(TLayoutDataAdaptor)
    private
        class var uInstance: TLayoutDataAdaptorExt;
        function ContainerTypeIsUsed(const aTypeName: string; aIsRackType: boolean): string;
        procedure DeleteLayoutWorkspaces(const aLayout: string);
        class procedure WriteRunLayoutForLayout(aDA: TLayoutDataAdaptorExt;
            const aRunName, aLayoutName: string);
    public const
        cTemporaryMethodName = '~TemporaryMethod~';
    public
        constructor Create();
        destructor Destroy; override;
        // instance function (for singleton class)
        class function Instance(): TLayoutDataAdaptorExt;

        function DeleteNameWithConfirm(const aName: string): boolean;
        procedure DeleteName(const aName: string); override;
        function SaveAs(const aName: string): string;
        procedure DeleteAllLayoutRelated(const aLayout: string);
        function CarrierTypeIsUsed(const aTypeName: string): string;
        function RackTypeIsUsed(const aTypeName: string): string;

        class function NewDataset(aSQL: string; aReadOnly: boolean): TDataProvider;

        class procedure DeleteRunLayoutAndPosinfo(const aRunName: string);
        class procedure WriteRunLayoutIfNecessary(const aRunName, aLayoutName: string);
    end;


implementation


uses
    Windows,
    SysUtils,
    Generics.Collections,
    GeneralTypes,
    ArrayUtils,
    DialogUtils,
    AppSettings,
    TipsetDataAdaptor,
    MethodSettingsDataAdaptor,
    LayoutWorkspaceDataAdaptor;

{ TLayoutDataAdaptorExt }

class function TLayoutDataAdaptorExt.Instance(): TLayoutDataAdaptorExt;
begin
    // create instance if instance does not exist
    if (uInstance = nil) then
        uInstance := TLayoutDataAdaptorExt.Create();

    // return instance
    result := uInstance;
end;

constructor TLayoutDataAdaptorExt.Create();
begin
    inherited Create();
end;

destructor TLayoutDataAdaptorExt.Destroy;
begin
    inherited;
end;

procedure TLayoutDataAdaptorExt.DeleteLayoutWorkspaces(const aLayout: string);
var
    xDA: TLayoutWorkspaceDataAdaptor;
begin
    xDA := TLayoutWorkspaceDataAdaptor.Create();
    try
        xDA.DeleteLayoutWorkspaceByLayoutID(aLayout);
    finally
        xDA.Free;
    end;
end;

procedure TLayoutDataAdaptorExt.DeleteAllLayoutRelated(const aLayout: string);
var
    xTipsetDA: TTipsetDataAdaptor;
begin
    xTipsetDA := TTipsetDataAdaptor.Create;
    try
        xTipsetDA.DeleteTipset(aLayout);
    finally
        FreeAndNil(xTipsetDA);
    end;
    DeleteLayoutWorkspaces(aLayout);

    inherited DeleteName(aLayout);
end;

procedure TLayoutDataAdaptorExt.DeleteName(const aName: string);
begin
    if not self.NameExists(aName) then
    begin
        DeleteLayoutWorkspaces(aName);
        EXIT();
    end;

    // delete anything in layout table with Name= "aName" ( i.e. basic layout + all run layouts )
    DeleteAllLayoutRelated(aName);
end;

function TLayoutDataAdaptorExt.DeleteNameWithConfirm(const aName: string): boolean;
var
    xMethodSettingsDA: TMethodSettingsDataAdaptor;
    xMethods: TArray<TMethodSettingsRec>;
    xMethodsWithLayout: TList<string>;
    xText: string;
    x: integer;
begin
    xMethodsWithLayout := TList<string>.Create;
    try
        xMethodSettingsDA := TMethodSettingsDataAdaptor.Create;
        try
            xMethods := xMethodSettingsDA.ReadStartables();
            for x := 0 to high(xMethods) do
            begin
                if (xMethods[x].LayoutName = aName) then
                    xMethodsWithLayout.Add(xMethods[x].MethodName);
            end;
        finally
            FreeAndNil(xMethodSettingsDA);
        end;

        if (xMethodsWithLayout.Count > 0) then
        begin // atleast one valid runlayout exists
            xText := TLanguageString.Read('Layout {0} is still used by these methods: ',
                'Layout {0} wird noch von diesen Methoden verwendet: ', [aName]) +
                TArrayUtils.ArrayToText(xMethodsWithLayout.ToArray);
            TDialogUtils.MessageBox(xText, TLanguageString.Read('Delete Layout', 'Layout löschen'),
                MB_ICONSTOP);
            EXIT(false);
        end;
    finally
        FreeAndNil(xMethodsWithLayout);
    end;

    if not TAppSettings.ItemConfirmDelete(STR_LAYOUT_TBL, aName) then
        EXIT(false);

    DeleteName(aName);
    EXIT(true);
end;

function TLayoutDataAdaptorExt.SaveAs(const aName: string): string;
var
    xNewName: string;
    xIDPairArray: TLayoutWorkspaceIDPairArray;
    x: integer;
    xLWDA: TLayoutWorkspaceDataAdaptor;
    xTipsetDA: TTipsetDataAdaptor;
begin
    result := '';
    // if not self.NameExists( aName ) then EXIT;

    xNewName := TDialogUtils.InputBox(TLanguageString.Read('Insert a new layout name!',
        'Bitte geben Sie einen neuen Layout-Namen ein!'), TLanguageString.Read('Insert layout name',
        'Bitte geben Sie einen Layout-Namen ein.'), '');

    xNewName := Copy(xNewName, 1, self.MaxNameLen);

    if (xNewName = '') then
        EXIT;

    if self.NameExists(xNewName) then
    begin
        TDialogUtils.MessageBox(TLanguageString.Read('{0} already exists. Action can not be continued!',
            '{0} existiert bereits. Die Aktion kann nicht fortgesetzt werden!', [xNewName]),
            TLanguageString.Read('Confirmation', 'Bestätigung'), 16);
    end
    else
    begin
        if TAppSettings.ItemConfirmAdd('Layout', xNewName, aName) then
        begin

            xTipsetDA := TTipsetDataAdaptor.Create;
            try
                xTipsetDA.SaveNameAs(aName, xNewName);
            finally
                FreeAndNil(xTipsetDA);
            end;

            // save layout as ..
            inherited SaveNameAs(aName, xNewName);

            xLWDA := TLayoutWorkspaceDataAdaptor.Create;
            try
                xLWDA.SaveLayoutIDAs(aName, xNewName, xIDPairArray);
            finally
                FreeAndNil(xLWDA);
            end;

            for x := 0 to high(xIDPairArray) do
            begin
                self.ExecSQLFmt('UPDATE ' + STR_LAYOUT_TBL + ' SET ' + STR_LAYOUT_FLD_WORKSPACEID + ' = %d' +
                    ' WHERE ' + STR_LAYOUT_FLD_NAME + ' = ''%s'' AND ' + STR_LAYOUT_FLD_WORKSPACEID + ' = %d',
                    [xIDPairArray[x].NewID, xNewName, xIDPairArray[x].OldID]);
            end;

            result := xNewName;
        end;
    end;
end;

class function TLayoutDataAdaptorExt.NewDataset(aSQL: string; aReadOnly: boolean): TDataProvider;
var
    aDataAdaptor: TLayoutDataAdaptorExt;
begin
    aDataAdaptor := TLayoutDataAdaptorExt.Create();
    if aSQL = '' then
        aDataAdaptor.SelectAndOpenAll(aReadOnly)
    else
        aDataAdaptor.SelectAndOpen(aSQL, aReadOnly);

    result := aDataAdaptor.DataProvider;
    aDataAdaptor.FreeQuery := false;
    aDataAdaptor.Free;

end;

function TLayoutDataAdaptorExt.ContainerTypeIsUsed(const aTypeName: string; aIsRackType: boolean): string;

var
    xPrevName: string;
    xLayoutFound: boolean;
    xRecs: TLayoutRecArray;
    x: integer;
    xRunName, xLayoutName: string;
begin
    result := '';
    xLayoutFound := false;
    if (aTypeName = '') then
        exit;

    if aIsRackType then
        self.ReadRackTypeLayoutNames(aTypeName, xRecs)
    else
        self.ReadCarrierTypeLayoutNames(aTypeName, xRecs);

    xPrevName := '';
    for x := 0 to high(xRecs) do
    begin
        xRunName := xRecs[x].Run;
        xLayoutName := xRecs[x].Layout;
        if (xRunName = '') and (xLayoutName <> xPrevName) then
        begin
            result := result + ' ' + xLayoutName;
            xPrevName := xLayoutName;
            xLayoutFound := true;
        end;
        if (not xLayoutFound) and (xRunName <> '') and (xRunName <> xPrevName) then
        begin
            result := result + ' ' + xLayoutName + ' (Run ' + xRunName + ')';
            xPrevName := xRunName;
        end;
    end;
end;

function TLayoutDataAdaptorExt.RackTypeIsUsed(const aTypeName: string): string;
begin
    result := ContainerTypeIsUsed(aTypeName, true);
end;

function TLayoutDataAdaptorExt.CarrierTypeIsUsed(const aTypeName: string): string;
begin
    result := ContainerTypeIsUsed(aTypeName, false);
end;

class procedure TLayoutDataAdaptorExt.WriteRunLayoutForLayout(aDA: TLayoutDataAdaptorExt;
    const aRunName, aLayoutName: string);
var
    xCarrierRecs: TLayoutCarrierRecArray;
    xRackRecs: TLayoutRackRecArray;
begin
    // copy Layout "","aLayoutName" to run layout "aRunName","aLayoutName"
    try
        aDA.ReadCarrierRecsByLayout(aLayoutName, xCarrierRecs);

        aDA.AddCarrierRecsToRun(aRunName, xCarrierRecs);

        aDA.ReadRackRecsByLayout(aLayoutName, xRackRecs);

        aDA.AddRackRecsToRun(aRunName, xRackRecs);
    except
        on e: Exception do
        begin
            raise Exception.CreateFmt('Could not create run layout for Run: %s, Layout: %s',
                [aRunName, aLayoutName]);
        end
    end;
end;

class procedure TLayoutDataAdaptorExt.DeleteRunLayoutAndPosinfo(const aRunName: string);
var
    xDA: TLayoutDataAdaptorExt;
begin
    if (aRunName = '') then
        EXIT;

    xDA := TLayoutDataAdaptorExt.Create;
    try
        // Posinfo-Daten löschen
        xDA.ExecSQLFmt('DELETE FROM Posinfo WHERE Rackid IN (SELECT Rackid FROM Layout' + ' WHERE Run=''%s'''
            + ' AND Rackid IS NOT NULL)', [aRunName]);

        // Run-Layout löschen
        xDA.DeleteRun(aRunName);
    finally
        FreeAndNil(xDA);
    end;
end;

class procedure TLayoutDataAdaptorExt.WriteRunLayoutIfNecessary(const aRunName, aLayoutName: string);
var
    xDA: TLayoutDataAdaptorExt;
    xLinkedLayoutRecs: TLayoutLinkRecArray;
    x: integer;
begin
    xDA := TLayoutDataAdaptorExt.Create;
    try
        // neues Run-Layout erzeugen
        if (aLayoutName <> '') and (aRunName <> '') then
        begin
            if aRunName = cTemporaryMethodName then
                xDA.DeleteRun(aRunName);
            if not xDA.RunNameExists(aRunName) then
            begin
                xDA.ReadLinkedLayoutRecs(xLinkedLayoutRecs);

                // neues Run-Layout erzeugen
                WriteRunLayoutForLayout(xDA, aRunName, aLayoutName);

                // neues Run-Layout für LinkedLayouts erzeugen
                for x := 0 to high(xLinkedLayoutRecs) do
                begin
                    if SameText(aLayoutName, xLinkedLayoutRecs[x].Layout) then
                        WriteRunLayoutForLayout(xDA, aRunName, xLinkedLayoutRecs[x].LinkedLayout);
                end;
            end;
        end;
    finally
        FreeAndNil(xDA);
    end;
end;


end.
