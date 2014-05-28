{ --------------------------------------------------------------------------------------------------
  Copyright © 2007 Zinsser Analytic GmbH - All rights reserved.
  Author       : pk
  Description  : Layout Data Adaptor encapsulates the access to Layout.db
  --------------------------------------------------------------------------------------------------
  Revision History:
  date     op  method                       track-no improvement/change
  -------- --  ---------------------------  -------- -----------------------------------------------
  30.08.07 pk                               TN3840.1  initial version. Old LayoutDataAdaptor.pas renamed to LayoutDataAdaptorExt
  02.10.07 wl                               TN3811.5 benutzt self.Query statt fQuery
  09.11.07 pk                               TN3921   Changes for updatemanager
  09.11.07 pk                               TN3924   Carr_X/Y/Z Steps to mm
  20.06.08 pk                               TN4139   Various changes
  16.07.08 pk                               TN4139   changes needed for reading racks from runlayout
  21.07.08 pk                               TN4179   Various changes
  31.07.08 pk  UpdateRackRunType            TN4193   New
  20.09.08 wl  ReadRecsByTypeAndCarrXY      TN4227   funktioniert jetzt auch bei deutscher Zeichensetzung
  23.09.08 wl                               TN4236   Vereinigung mit ..Fieldnames
  29.09.08 wl  INT_LAYOUT_FLDLEN_RUN        TN4242   'RUN' hat jetzt 50 chars
  16.01.09 wl                               TN4362   an Änderungen in TQueryDataAdaptor angepasst
  19.05.09 wl                               TN4362   GetTableName entfernt
  17.08.09 wl  ReadRecsByTypeAndCarrXY      TN4227   Funktion ist jetzt thread-safe, an SysUtils.DecimalSeparartor wird nicht mehr gedreht!
  04.11.09 pk                               TN4843   Replace TObjectList, TStringList, TStrings with Generics Lists and TStringArray
  19.11.09 pk  WriteSlot                    TN4585.4 write rotation field as float instead of integer
  17.02.10 ts  ReadRecsByTypeAndCarrXY      TN4981   STR_LAYOUT_FLD_CARR_X_COMPARE_FMT mit BETWEEN zum vergleichen von Gleitkommazahlen angepasst
  17.06.10 pk                               TN5152.1  Change SQLs so that they also work for TurboDB (dbl quoatations to single)
  19.11.10 pk  SetFieldToValueOrNull        TN5334    For Run and Rackname field set the values to null if string is empty
  14.12.10 wl                               TN5411    TFormatUtils statt TValueConverter
  08.02.11 wl  Instance                     TN5475   entfernt
  08.02.11 wl  InstDeleteRun                TN5475   neu: DeleteRun mit neuer Instanz
  07.03.12 wl  SelectAndOpenCarriers..      TN5825   immer: Order by Carriername
  07.03.12 wl  SelectAndOpenRacks..         TN5825   immer: Order by Rackname
  13.03.12 wl  ReadCarrierRecsByRun         TN5798   entfernt
  03.05.12 wl                               TN5884   Bei RUN und RACKNAME wird nicht nur auf Null sondern auch auf leeren String getestet
  27.03.13 wl                               TN6045   uses geändert
  19.04.13 wl  DeleteName                   TN6106   kein Rückgabewert
  -------------------------------------------------------------------------------------------------- }

unit LayoutDataAdaptor;


interface


uses
    ListClasses,
    CommonTypes,
    DataProvider,
    QueryDataAdaptor,
    GeneralTypes;

type
    TLayoutRec = record
        Run: string;
        Layout: string;
        RackName: string;
        CarrierName: string;
        WorkspaceID: integer;
        RackID: string;
        Slot: integer;
        RackType: string;
        Carr_X, Carr_Y, Carr_Z: double;
        CarrierType: string;
    end;

    TLayoutRecArray = array of TLayoutRec;

    TLayoutRackRec = record
        Run: string;
        Layout: string;
        RackName: string;
        RackType: string;
        RackID: string;
        CarrierName: string;
        Slot: integer;
        Rotation: double;
    end;

    TLayoutRackRecArray = array of TLayoutRackRec;

    TLayoutCarrierRec = record
        Run: string;
        Layout: string;
        CarrierName: string;
        WorkspaceID: integer;
        Carr_X, Carr_Y, Carr_Z: double;
        CarrierType: string;
    end;

    TLayoutCarrierRecArray = array of TLayoutCarrierRec;

    TLayoutLinkRec = record
        Layout: string;
        LinkedLayout: string;
    end;

    TLayoutLinkRecArray = array of TLayoutLinkRec;

    TLayoutDataAdaptor = class(TQueryDataAdaptor)
    protected const
        STR_LAYOUT_TBL = 'LAYOUT';
        STR_LAYOUT_FLD_RUN = 'RUN';
        STR_LAYOUT_FLD_NAME = 'NAME';
        STR_LAYOUT_FLD_WORKSPACEID = 'WORKSPACEID';
        STR_LAYOUT_FLD_RACKNAME = 'RACKNAME';
        STR_LAYOUT_FLD_CARRIERNAME = 'CARRIERNAME';
        STR_LAYOUT_FLD_RACKID = 'RACKID';
        STR_LAYOUT_FLD_SLOT = 'SLOT';
        STR_LAYOUT_FLD_RACKTYPE = 'RACKTYP';
        STR_LAYOUT_FLD_CARR_X = 'CARR_X';
        STR_LAYOUT_FLD_CARR_Y = 'CARR_Y';
        STR_LAYOUT_FLD_CARR_Z = 'CARR_Z';
        STR_LAYOUT_FLD_CARRIERTYPE = 'CARRIER';
        STR_LAYOUT_FLD_LINKEDLAYOUT = 'LINKEDLAYOUT';
    private const
        // Das Feld 'CARR_X' wird zur Speicherung des Rotation-Wertes benutzt!!!
        STR_LAYOUT_FLD_RACK_ROTATION = STR_LAYOUT_FLD_CARR_X;
        INT_LAYOUT_FLDLEN_NAME = 20;
        INT_LAYOUT_FLDLEN_RACKNAME = 30;
        INT_LAYOUT_FLDLEN_CARRIERNAME = 20;
        INT_LAYOUT_FLDLEN_RACKID = 20;
        cLinkIDNone = 0;

        STR_LAYOUT_FLD_LAYOUT_COMPARE_FMT = 'UPPER(' + STR_LAYOUT_FLD_NAME + ') = UPPER(''%s'')';
        STR_LAYOUT_FLD_RUN_COMPARE_FMT = 'UPPER(' + STR_LAYOUT_FLD_RUN + ') = UPPER(''%s'')';
        STR_LAYOUT_FLD_RACKNAME_COMPARE_FMT = 'UPPER(' + STR_LAYOUT_FLD_RACKNAME + ') = UPPER(''%s'')';
        STR_LAYOUT_FLD_CARRIERNAME_COMPARE_FMT = 'UPPER(' + STR_LAYOUT_FLD_CARRIERNAME + ') = UPPER(''%s'')';
        STR_LAYOUT_FLD_RACKID_COMPARE_FMT = 'UPPER(' + STR_LAYOUT_FLD_RACKID + ') = UPPER(''%s'')';
        STR_LAYOUT_FLD_RACKTYPE_COMPARE_FMT = 'UPPER(' + STR_LAYOUT_FLD_RACKTYPE + ') = UPPER(''%s'')';
        STR_LAYOUT_FLD_CARRIERTYPE_COMPARE_FMT = 'UPPER(' + STR_LAYOUT_FLD_CARRIERTYPE + ') = UPPER(''%s'')';
        STR_LAYOUT_FLD_CARR_X_COMPARE_FMT = STR_LAYOUT_FLD_CARR_X + ' BETWEEN %f - 0.01 AND %f + 0.01';
        STR_LAYOUT_FLD_CARR_Y_COMPARE_FMT = STR_LAYOUT_FLD_CARR_Y + ' BETWEEN %f - 0.01 AND %f + 0.01';
        STR_LAYOUT_FLD_LINKEDLAYOUT_COMPARE_FMT = 'UPPER(' + STR_LAYOUT_FLD_LINKEDLAYOUT +
            ') = UPPER(''%s'')';
        STR_SQL_WHERE_RUN_NULL = '(' + STR_LAYOUT_FLD_RUN + ' IS NULL OR ' + STR_LAYOUT_FLD_RUN + ' = '''')';
        STR_SQL_WHERE_LAYOUT = ' WHERE ' + STR_LAYOUT_FLD_LAYOUT_COMPARE_FMT;
        STR_SQL_FROM = ' FROM ' + STR_LAYOUT_TBL;
        STR_SQL_DELETE_FMT = 'DELETE ' + STR_SQL_FROM + STR_SQL_WHERE_LAYOUT;
        STR_SQL_SELECT_ALL = 'SELECT * ' + STR_SQL_FROM;
        STR_SQL_SELECT_LAYOUT_FMT = STR_SQL_SELECT_ALL + STR_SQL_WHERE_LAYOUT;
        STR_SQL_SELECT_ALL_RUN_NULL = STR_SQL_SELECT_ALL + ' WHERE ' + STR_SQL_WHERE_RUN_NULL;
        STR_SQL_DELETE_RUN_FMT = 'DELETE ' + STR_SQL_FROM + ' WHERE ' + STR_LAYOUT_FLD_RUN_COMPARE_FMT;
        STR_SQL_RACKNAME_NOT_NULL = '(' + STR_LAYOUT_FLD_RACKNAME + ' IS NOT NULL AND ' +
            STR_LAYOUT_FLD_RACKNAME + ' <> '''')';
        STR_SQL_RACKNAME_NULL = '(' + STR_LAYOUT_FLD_RACKNAME + ' IS NULL OR ' + STR_LAYOUT_FLD_RACKNAME +
            ' = '''')';
        STR_SQL_LINKEDLAYOUT_NULL = STR_LAYOUT_FLD_LINKEDLAYOUT + ' IS NULL ';
        STR_SQL_SELECT_LINKEDLAYOUT = STR_SQL_SELECT_ALL + ' WHERE ' + STR_LAYOUT_FLD_LINKEDLAYOUT +
            ' IS NOT NULL AND ' + STR_SQL_WHERE_RUN_NULL;
    private
        function MakeRunNameAndLayoutSQL(const aRunName, aLayout: string): string;
        procedure SelectAndOpenRackName(const aRunName, aLayout, aRackName: string; aReadOnly: boolean);
        procedure SelectAndOpenCarrierName(const aRunName, aLayout, aCarrierName: string; aReadOnly: boolean);
        procedure SelectAndOpenRunLayout(const aRunName, aLayoutName: string; aReadOnly: boolean);
        function MakeDefaultLayoutRec(): TLayoutRec;
        function MakeRunNameWhereSQL(const aRunName: string): string;
        function MakeRunNameSQL(const aRunName: string): string;
        function MakeLayoutWhereSQL(const aLayoutName: string): string;
        function MakeLayoutSQL(const aLayoutName: string): string;
        class procedure SetFieldToValueOrNull(const aDataset: TDataProvider; const aFieldName: string;
            const aValue: string);
        class procedure SetRunField(const aDataset: TDataProvider; const aValue: string);
        class procedure SetRackNameField(const aDataset: TDataProvider; const aValue: string);
    protected
        function GetNameField(): string; override;
        function GetKeyFields(): TArray<string>; override;
        function GetNameValues(const aName: string): TArray<variant>; override;
    public
        constructor Create();

        procedure SelectAndOpenAll(aReadOnly: boolean);

        procedure SelectAndOpenName(const aName: string; aReadOnly: boolean);
        procedure SelectAndOpenBasicLayout(const aLayoutName: string; aReadOnly: boolean);
        procedure DeleteName(const aName: string); override;
        procedure ReadNameRecs(const aName: string; var aRecs: TLayoutRecArray);
        procedure WriteRecs(const aRecs: TLayoutRecArray);
        procedure SelectAndOpenDistinctRunLayout(aRunName: string = '');
        function GetLayoutNameForRun(const aRunName: string): string;
        function RunLayoutNameExists(const aRunName, aLayoutName: string): boolean;
        function RunNameExists(const aRunName: string): boolean;
        procedure DeleteRun(const aRunName: string);

        // carrier functions
        procedure SelectAndOpenCarriersByLayout(const aLayout: string; aReadOnly: boolean);
        procedure SelectAndOpenCarriers(aReadOnly: boolean);
        function CarrierExists(const aRunName, aLayout: string; const aCarrierName: string): boolean;
        procedure AddCarrier(const aLayout: string; aWorkspaceID: integer;
            const aCarrierTypeName, aCarrierName: string; aCarr_X, aCarr_Y, aCarr_Z: double);
        procedure ReadCarrierRecsByLayout(const aLayout: string; var vRecs: TLayoutCarrierRecArray);
        procedure ReadCarrierRecs(var vRecs: TLayoutCarrierRecArray);

        procedure ReadRecsByTypeAndCarrXY(const aCarrierType: string; aCarrX, aCarrY: double;
            var vRecs: TLayoutRecArray);
        procedure ReadCarrierTypeLayoutNames(const aCarrierType: string; var vRecs: TLayoutRecArray);
        procedure WriteCarrXYForCarrier(const aLayout, aCarrierName: string; aCarrX, aCarrY: double);
        procedure UpdateCarrierRunType(const aRunName, aCarrierName, aNewCarrierType: string);
        procedure UpdateRackRunType(const aRunName, aRackName, aNewRackType: string);
        procedure AddCarrierRecsToRun(const aRunName: string; const aCarrierRecs: TLayoutCarrierRecArray);
        // rack functions
        procedure SelectAndOpenRacksByLayout(const aLayout: string; aReadOnly: boolean);
        procedure SelectAndOpenRacksByRun(const aRunName: string; aReadOnly: boolean);
        procedure SelectAndOpenRacks(aReadOnly: boolean);
        function RackExists(const aRunName, aLayout, aRackName: string): boolean;
        procedure AddRack(const aLayout: string; const aRackTypeName, aRackName, aRackID,
            aCarrierName: string; aSlot, aRotation: integer);
        procedure WriteSlot(const aRunName, aLayout, aRackName, aCarrierName: string; const aSlot: integer;
            const aRotation: double);
        procedure WriteRackID(const aRunName, aLayout, aRackName, aRackID: string; out oOldRackID: string);
        procedure ReadRackInfo(const aRunName, aLayout, aRackName: string;
            out oCarrierName, oRackID, oRackType: string; out oSlot: integer; out oRotation: double);
        function ReadRackID(const aRunName, aLayout, aRackName: string; out oRackID: string): boolean;
        function ReadRackType(const aRunName, aLayout, aRackName: string; out oRackType: string): boolean;
        procedure ReadRackRecsByRun(const aRunName: string; var vRecs: TLayoutRackRecArray);
        procedure ReadRackRecsByLayout(const aLayout: string; var vRecs: TLayoutRackRecArray);
        procedure ReadRackRecs(var vRecs: TLayoutRackRecArray);
        procedure ReadRackTypeLayoutNames(const aRackType: string; var vRecs: TLayoutRecArray);
        function BCEqualToOther(const aRunName, aLayout, aRackName, aRackID: string): string;

        procedure AddRackRecsToRun(const aRunName: string; const aRackRecs: TLayoutRackRecArray);

        // LinkedLayout functions
        procedure ReadLinkedLayoutRecs(var vRecs: TLayoutLinkRecArray);
        function LinkedLayoutExists(const aRunName, aLayout, aLinkedLayout: string): boolean;
        procedure SelectAndOpenLinkedLayout(const aRunName, aLayout, aLinkedLayout: string;
            aReadOnly: boolean);
        procedure AddLinkedLayout(const aLayout, aLinkedLayout: string);

        // field lengths
        class function MaxCarrierNameLen: integer;
        class function MaxNameLen: integer;
        class function MaxRackIDLen: integer;
        class function MaxRackNameLen: integer;

        class procedure ReadCarrierRecAtCursor(aDataset: TDataProvider; var vRec: TLayoutCarrierRec);
        class procedure ReadCarrierRecsAtCursor(aDataset: TDataProvider; var vRecs: TLayoutCarrierRecArray);
        class procedure WriteCarrierRecAtCursor(aDataset: TDataProvider; const aRec: TLayoutCarrierRec;
            aAppend: boolean);

        class procedure ReadRackRecAtCursor(aDataset: TDataProvider; var vRec: TLayoutRackRec);
        class procedure ReadRackRecsAtCursor(aDataset: TDataProvider; var vRecs: TLayoutRackRecArray);
        class procedure WriteRackRecAtCursor(aDataset: TDataProvider; const aRec: TLayoutRackRec;
            aAppend: boolean);

        class procedure ReadRecAtCursor(aDataset: TDataProvider; var vRec: TLayoutRec);
        class procedure ReadRecsAtCursor(aDataset: TDataProvider; var vRecs: TLayoutRecArray);

        class procedure WriteRecsAtCursor(aDataset: TDataProvider; const aRecs: TLayoutRecArray);
        class procedure WriteRecAtCursor(aDataset: TDataProvider; const aRec: TLayoutRec; aAppend: boolean);

        class function InstReadAllNames(): TStringArray;
        class function InstGetLayoutNameForRun(const aRunName: string): string;
        class procedure InstDeleteRun(const aRunName: string); static;
    end;


implementation


uses
    Sysutils,
    Variants,
    UtilLib;

class function TLayoutDataAdaptor.MaxNameLen: integer;
begin
    result := INT_LAYOUT_FLDLEN_NAME;
end;

class function TLayoutDataAdaptor.MaxCarrierNameLen: integer;
begin
    result := INT_LAYOUT_FLDLEN_CARRIERNAME;
end;

class function TLayoutDataAdaptor.MaxRackNameLen: integer;
begin
    result := INT_LAYOUT_FLDLEN_RACKNAME;
end;

class function TLayoutDataAdaptor.MaxRackIDLen: integer;
begin
    result := INT_LAYOUT_FLDLEN_RACKID;
end;

function TLayoutDataAdaptor.GetNameField(): string;
begin
    result := STR_LAYOUT_FLD_NAME;
end;

procedure TLayoutDataAdaptor.DeleteName(const aName: string);
begin
    self.ExecSQL(Format(STR_SQL_DELETE_FMT, [aName]));
end;

procedure TLayoutDataAdaptor.SelectAndOpenAll(aReadOnly: boolean);
begin
    SelectAndOpen(STR_SQL_SELECT_ALL, aReadOnly)
end;

procedure TLayoutDataAdaptor.SelectAndOpenName(const aName: string; aReadOnly: boolean);
begin
    ASSERT(aName <> '', 'Layout name is empty');
    SelectAndOpen(Format(STR_SQL_SELECT_LAYOUT_FMT, [aName]), aReadOnly);
end;

function TLayoutDataAdaptor.MakeRunNameWhereSQL(const aRunName: string): string;
begin
    if aRunName <> '' then
        result := Format(STR_LAYOUT_FLD_RUN_COMPARE_FMT, [aRunName])
    else
        result := STR_SQL_WHERE_RUN_NULL;
end;

function TLayoutDataAdaptor.MakeRunNameSQL(const aRunName: string): string;
var
    xWhere: string;
begin
    result := STR_SQL_SELECT_ALL;
    if aRunName <> '' then
        xWhere := MakeRunNameWhereSQL(aRunName);

    result := result + ' WHERE ' + xWhere;
end;

function TLayoutDataAdaptor.MakeLayoutWhereSQL(const aLayoutName: string): string;
begin
    result := Format(STR_LAYOUT_FLD_LAYOUT_COMPARE_FMT, [aLayoutName]);
end;

function TLayoutDataAdaptor.MakeLayoutSQL(const aLayoutName: string): string;
begin
    result := STR_SQL_SELECT_ALL + ' WHERE ' + MakeLayoutWhereSQL(aLayoutName) + ' AND ' +
        STR_SQL_WHERE_RUN_NULL;
end;

function TLayoutDataAdaptor.MakeRunNameAndLayoutSQL(const aRunName, aLayout: string): string;
var
    xWhere: string;
begin
    result := STR_SQL_SELECT_ALL;
    if aRunName <> '' then
        xWhere := MakeRunNameWhereSQL(aRunName)
    else
        xWhere := MakeLayoutWhereSQL(aLayout);

    result := result + ' WHERE ' + xWhere;
end;

procedure TLayoutDataAdaptor.SelectAndOpenBasicLayout(const aLayoutName: string; aReadOnly: boolean);
begin
    ASSERT(aLayoutName <> '', 'Layout name is empty');
    SelectAndOpen(MakeRunNameAndLayoutSQL('', aLayoutName), aReadOnly);
end;

procedure TLayoutDataAdaptor.SelectAndOpenRunLayout(const aRunName, aLayoutName: string; aReadOnly: boolean);
begin
    SelectAndOpen(MakeRunNameAndLayoutSQL(aRunName, aLayoutName), aReadOnly);
end;

function TLayoutDataAdaptor.RunLayoutNameExists(const aRunName, aLayoutName: string): boolean;
begin
    SelectAndOpenRunLayout(aRunName, aLayoutName, true);
    try
        result := not DataProvider.IsEmpty();
    finally
        Close();
    end;
end;

function TLayoutDataAdaptor.RunNameExists(const aRunName: string): boolean;
begin
    SelectAndOpen(STR_SQL_SELECT_ALL + ' WHERE ' + Format(STR_LAYOUT_FLD_RUN_COMPARE_FMT, [aRunName]), true);
    try
        result := not DataProvider.IsEmpty();
    finally
        Close();
    end;
end;

procedure TLayoutDataAdaptor.WriteRecs(const aRecs: TLayoutRecArray);
begin
    SelectAndOpenAll(false);
    try
        WriteRecsAtCursor(self.DataProvider, aRecs);
    finally
        Close();
    end;
end;

procedure TLayoutDataAdaptor.ReadNameRecs(const aName: string; var aRecs: TLayoutRecArray);
begin
    SelectAndOpenName(aName, true);
    try
        ReadRecsAtCursor(self.DataProvider, aRecs);
    finally
        Close();
    end;
end;

procedure TLayoutDataAdaptor.SelectAndOpenDistinctRunLayout(aRunName: string = '');
var
    xSQLWhere, xSQL: string;
begin
    xSQLWhere := '';
    if aRunName <> '' then
    begin
        xSQLWhere := Format('%s = ''%s''', [STR_LAYOUT_FLD_RUN, aRunName]);
    end;
    if xSQLWhere <> '' then
        xSQLWhere := 'WHERE ' + xSQLWhere;

    xSQL := Format('SELECT DISTINCT %s, %s FROM ''%s'' %s', [STR_LAYOUT_FLD_RUN, STR_LAYOUT_FLD_NAME,
        STR_LAYOUT_TBL, xSQLWhere]);
    SelectAndOpen(xSQL, true);
end;

function TLayoutDataAdaptor.GetLayoutNameForRun(const aRunName: string): string;
begin
    result := '';
    if (aRunName = '') then
        EXIT;
    SelectAndOpenDistinctRunLayout(aRunName);
    try
        if self.DataProvider.Eof then
            EXIT;
        result := ReadName();
    finally
        Close();
    end;
end;

function TLayoutDataAdaptor.GetKeyFields(): TArray<string>;
begin
    result := self.FieldKeyArrayOf([STR_LAYOUT_FLD_RUN, STR_LAYOUT_FLD_NAME]);
end;

function TLayoutDataAdaptor.GetNameValues(const aName: string): TArray<variant>;
begin
    result := FieldValArrayOf([Null, aName]);
end;

procedure TLayoutDataAdaptor.DeleteRun(const aRunName: string);
begin
    self.ExecSQL(Format(STR_SQL_DELETE_RUN_FMT, [aRunName]));
end;

class function TLayoutDataAdaptor.InstReadAllNames(): TStringArray;
var
    xDA: TLayoutDataAdaptor;
begin
    xDA := TLayoutDataAdaptor.Create;
    try
        result := xDA.ReadAllNames();
    finally
        FreeAndNil(xDA);
    end;
end;

class procedure TLayoutDataAdaptor.ReadRecAtCursor(aDataset: TDataProvider; var vRec: TLayoutRec);
begin
    ASSERT(aDataSet.Active, 'Dataset not active');
    with vRec do
    begin
        Run := aDataset.FieldByName(STR_LAYOUT_FLD_RUN).AsString;
        Layout := aDataset.FieldByName(STR_LAYOUT_FLD_NAME).AsString;
        WorkspaceID := aDataset.FieldByName(STR_LAYOUT_FLD_WORKSPACEID).AsInteger;
        RackName := aDataset.FieldByName(STR_LAYOUT_FLD_RACKNAME).AsString;
        CarrierName := aDataset.FieldByName(STR_LAYOUT_FLD_CARRIERNAME).AsString;
        RackID := aDataset.FieldByName(STR_LAYOUT_FLD_RACKID).AsString;
        Slot := aDataset.FieldByName(STR_LAYOUT_FLD_SLOT).AsInteger;
        RackType := aDataset.FieldByName(STR_LAYOUT_FLD_RACKTYPE).AsString;
        Carr_X := aDataset.FieldByName(STR_LAYOUT_FLD_CARR_X).AsFloat;
        Carr_Y := aDataset.FieldByName(STR_LAYOUT_FLD_CARR_Y).AsFloat;
        Carr_Z := aDataset.FieldByName(STR_LAYOUT_FLD_CARR_Z).AsFloat;
        CarrierType := aDataset.FieldByName(STR_LAYOUT_FLD_CARRIERTYPE).AsString;
    end;
end;

class procedure TLayoutDataAdaptor.ReadRecsAtCursor(aDataset: TDataProvider; var vRecs: TLayoutRecArray);
var
    i: integer;
begin
    i := 0;
    SetLength(vRecs, aDataset.RecordCount);
    while not aDataset.Eof do
    begin
        ReadRecAtCursor(aDataset, vRecs[i]);
        aDataset.Next;
        Inc(i);
    end;

end;

class procedure TLayoutDataAdaptor.ReadRackRecAtCursor(aDataset: TDataProvider; var vRec: TLayoutRackRec);
begin
    ASSERT(aDataSet.Active, 'Dataset not active');
    with vRec do
    begin
        Run := aDataset.FieldByName(STR_LAYOUT_FLD_RUN).AsString;
        Layout := aDataset.FieldByName(STR_LAYOUT_FLD_NAME).AsString;
        RackName := aDataset.FieldByName(STR_LAYOUT_FLD_RACKNAME).AsString;
        RackType := aDataset.FieldByName(STR_LAYOUT_FLD_RACKTYPE).AsString;
        RackID := aDataset.FieldByName(STR_LAYOUT_FLD_RACKID).AsString;
        CarrierName := aDataset.FieldByName(STR_LAYOUT_FLD_CARRIERNAME).AsString;
        Slot := aDataset.FieldByName(STR_LAYOUT_FLD_SLOT).AsInteger;
        Rotation := aDataset.FieldByName(STR_LAYOUT_FLD_RACK_ROTATION).AsFloat;
    end;
end;

class procedure TLayoutDataAdaptor.SetFieldToValueOrNull(const aDataset: TDataProvider;
    const aFieldName: string; const aValue: string);
begin
    if aValue = '' then
        aDataset.FieldByName(aFieldName).IsNull := true
    else
        aDataset.FieldByName(aFieldName).AsString := aValue;
end;

class procedure TLayoutDataAdaptor.SetRunField(const aDataset: TDataProvider; const aValue: string);
begin
    SetFieldToValueOrNull(aDataset, STR_LAYOUT_FLD_RUN, aValue);
end;

class procedure TLayoutDataAdaptor.SetRackNameField(const aDataset: TDataProvider; const aValue: string);
begin
    SetFieldToValueOrNull(aDataset, STR_LAYOUT_FLD_RACKNAME, aValue);
end;

class procedure TLayoutDataAdaptor.WriteRackRecAtCursor(aDataset: TDataProvider; const aRec: TLayoutRackRec;
    aAppend: boolean);
begin
    if aAppend then
        aDataset.Append
    else
        aDataset.Edit;

    ASSERT(aDataSet.Active, 'Dataset not active');
    with aRec do
    begin
        SetRunField(aDataset, Run);
        aDataset.FieldByName(STR_LAYOUT_FLD_NAME).AsString := Layout;
        SetRackNameField(aDataset, RackName);
        aDataset.FieldByName(STR_LAYOUT_FLD_RACKTYPE).AsString := RackType;
        aDataset.FieldByName(STR_LAYOUT_FLD_RACKID).AsString := RackID;
        aDataset.FieldByName(STR_LAYOUT_FLD_CARRIERNAME).AsString := CarrierName;
        aDataset.FieldByName(STR_LAYOUT_FLD_SLOT).AsInteger := Slot;
        aDataset.FieldByName(STR_LAYOUT_FLD_RACK_ROTATION).AsFloat := Rotation;
    end;

    aDataset.Post;
end;

class procedure TLayoutDataAdaptor.ReadRackRecsAtCursor(aDataset: TDataProvider;
    var vRecs: TLayoutRackRecArray);
var
    i: integer;
begin
    i := 0;
    SetLength(vRecs, aDataset.RecordCount);
    while not aDataset.Eof do
    begin
        ReadRackRecAtCursor(aDataset, vRecs[i]);
        aDataset.Next;
        Inc(i);
    end;
end;

class procedure TLayoutDataAdaptor.ReadCarrierRecAtCursor(aDataset: TDataProvider;
    var vRec: TLayoutCarrierRec);
begin
    ASSERT(aDataSet.Active, 'Dataset not active');
    with vRec do
    begin
        Run := aDataset.FieldByName(STR_LAYOUT_FLD_RUN).AsString;
        Layout := aDataset.FieldByName(STR_LAYOUT_FLD_NAME).AsString;
        WorkspaceID := aDataset.FieldByName(STR_LAYOUT_FLD_WORKSPACEID).AsInteger;
        CarrierName := aDataset.FieldByName(STR_LAYOUT_FLD_CARRIERNAME).AsString;
        Carr_X := aDataset.FieldByName(STR_LAYOUT_FLD_CARR_X).AsFloat;
        Carr_Y := aDataset.FieldByName(STR_LAYOUT_FLD_CARR_Y).AsFloat;
        Carr_Z := aDataset.FieldByName(STR_LAYOUT_FLD_CARR_Z).AsFloat;
        CarrierType := aDataset.FieldByName(STR_LAYOUT_FLD_CARRIERTYPE).AsString;
    end;
end;

class procedure TLayoutDataAdaptor.WriteCarrierRecAtCursor(aDataset: TDataProvider;
    const aRec: TLayoutCarrierRec; aAppend: boolean);
begin
    ASSERT(aDataSet.Active, 'Dataset not active');
    if aAppend then
        aDataset.Append
    else
        aDataset.Edit;

    with aRec do
    begin
        SetRunField(aDataset, Run);
        aDataset.FieldByName(STR_LAYOUT_FLD_NAME).AsString := Layout;
        aDataset.FieldByName(STR_LAYOUT_FLD_WORKSPACEID).AsInteger := WorkspaceID;
        aDataset.FieldByName(STR_LAYOUT_FLD_CARRIERNAME).AsString := CarrierName;
        aDataset.FieldByName(STR_LAYOUT_FLD_CARR_X).AsFloat := Carr_X;
        aDataset.FieldByName(STR_LAYOUT_FLD_CARR_Y).AsFloat := Carr_Y;
        aDataset.FieldByName(STR_LAYOUT_FLD_CARR_Z).AsFloat := Carr_Z;
        aDataset.FieldByName(STR_LAYOUT_FLD_CARRIERTYPE).AsString := CarrierType;
    end;

    aDataset.Post;
end;

class procedure TLayoutDataAdaptor.ReadCarrierRecsAtCursor(aDataset: TDataProvider;
    var vRecs: TLayoutCarrierRecArray);
var
    i: integer;
begin
    i := 0;
    SetLength(vRecs, aDataset.RecordCount);
    while not aDataset.Eof do
    begin
        ReadCarrierRecAtCursor(aDataset, vRecs[i]);
        aDataset.Next;
        Inc(i);
    end;
end;

class procedure TLayoutDataAdaptor.WriteRecAtCursor(aDataset: TDataProvider; const aRec: TLayoutRec;
    aAppend: boolean);
begin
    ASSERT(aDataSet.Active, 'Dataset not active');
    if aAppend then
        aDataset.Append
    else
        aDataset.Edit;

    with aRec do
    begin
        SetRunField(aDataset, Run);
        aDataset.FieldByName(STR_LAYOUT_FLD_NAME).AsString := aRec.Layout;
        aDataset.FieldByName(STR_LAYOUT_FLD_WORKSPACEID).AsInteger := aRec.WorkspaceID;
        SetRackNameField(aDataset, RackName);
        aDataset.FieldByName(STR_LAYOUT_FLD_CARRIERNAME).AsString := aRec.CarrierName;
        aDataset.FieldByName(STR_LAYOUT_FLD_RACKID).AsString := aRec.RackID;
        aDataset.FieldByName(STR_LAYOUT_FLD_SLOT).AsInteger := aRec.Slot;
        aDataset.FieldByName(STR_LAYOUT_FLD_RACKTYPE).AsString := aRec.RackType;
        aDataset.FieldByName(STR_LAYOUT_FLD_CARR_X).AsFloat := aRec.Carr_X;
        aDataset.FieldByName(STR_LAYOUT_FLD_CARR_Y).AsFloat := aRec.Carr_Y;
        aDataset.FieldByName(STR_LAYOUT_FLD_CARR_Z).AsFloat := aRec.Carr_Z;
        aDataset.FieldByName(STR_LAYOUT_FLD_CARRIERTYPE).AsString := aRec.CarrierType;
    end;
    aDataset.Post;
end;

class procedure TLayoutDataAdaptor.WriteRecsAtCursor(aDataset: TDataProvider; const aRecs: TLayoutRecArray);
var
    i: integer;
begin
    for i := 0 to high(aRecs) do
    begin
        WriteRecAtCursor(aDataset, aRecs[i], true);
    end;
end;

function TLayoutDataAdaptor.MakeDefaultLayoutRec(): TLayoutRec;
begin
    with result do
    begin
        Run := '';
        Layout := '';
        WorkspaceID := 0;
        RackName := '';
        CarrierName := '';
        RackID := '';
        Slot := 0;
        RackType := '';
        Carr_X := 0;
        Carr_Y := 0;
        Carr_Z := 0;
        CarrierType := '';
    end;
end;

procedure TLayoutDataAdaptor.AddCarrier(const aLayout: string; aWorkspaceID: integer;
    const aCarrierTypeName, aCarrierName: string; aCarr_X, aCarr_Y, aCarr_Z: double);
var
    xRec: TLayoutRec;
begin
    self.SelectAndOpenAll(false);
    try
        xRec := MakeDefaultLayoutRec();
        xRec.Layout := aLayout;
        xRec.WorkspaceID := aWorkspaceID;
        xRec.CarrierName := aCarrierName;
        xRec.Carr_X := aCarr_X;
        xRec.Carr_Y := aCarr_Y;
        xRec.Carr_Z := aCarr_Z;
        xRec.CarrierType := aCarrierTypeName;

        self.WriteRecAtCursor(self.DataProvider, xRec, true);
    finally
        Close();
    end;
end;

procedure TLayoutDataAdaptor.AddCarrierRecsToRun(const aRunName: string;
    const aCarrierRecs: TLayoutCarrierRecArray);
var
    x: integer;
begin
    self.SelectAndOpenAll(false);
    try
        for x := 0 to high(aCarrierRecs) do
        begin
            aCarrierRecs[x].Run := aRunName;
            self.WriteCarrierRecAtCursor(self.DataProvider, aCarrierRecs[x], true);
        end;
    finally
        Close();
    end;
end;

procedure TLayoutDataAdaptor.SelectAndOpenCarrierName(const aRunName, aLayout, aCarrierName: string;
    aReadOnly: boolean);
begin
    SelectAndOpen(MakeRunNameAndLayoutSQL(aRunName, aLayout) + ' AND ' + STR_LAYOUT_FLD_CARRIERTYPE +
        ' IS NOT NULL' + ' AND ' + STR_LAYOUT_FLD_CARRIERTYPE + ' <>'''' ' + ' AND ' +
        STR_SQL_LINKEDLAYOUT_NULL + ' AND ' + Format(STR_LAYOUT_FLD_CARRIERNAME_COMPARE_FMT, [aCarrierName]),
        aReadOnly);
end;

function TLayoutDataAdaptor.CarrierExists(const aRunName, aLayout, aCarrierName: string): boolean;
begin
    SelectAndOpenCarrierName(aRunName, aLayout, aCarrierName, true);
    try
        result := not self.DataProvider.IsEmpty;
    finally
        Close();
    end;
end;

constructor TLayoutDataAdaptor.Create;
begin
    inherited Create(STR_LAYOUT_TBL);
end;

procedure TLayoutDataAdaptor.AddRack(const aLayout: string; const aRackTypeName, aRackName, aRackID,
    aCarrierName: string; aSlot, aRotation: integer);
var
    xRec: TLayoutRec;
begin
    self.SelectAndOpenAll(false);
    try
        xRec := MakeDefaultLayoutRec();
        xRec.Layout := aLayout;
        xRec.RackName := aRackName;
        xRec.CarrierName := aCarrierName;
        xRec.RackID := aRackID;
        xRec.Slot := aSlot;
        xRec.RackType := aRackTypeName;
        // Das Feld 'CARR_X' wird zur Speicherung des Rotation-Wertes benutzt!!!
        xRec.Carr_X := aRotation;

        self.WriteRecAtCursor(self.DataProvider, xRec, true);
    finally
        Close();
    end;
end;

procedure TLayoutDataAdaptor.AddRackRecsToRun(const aRunName: string; const aRackRecs: TLayoutRackRecArray);
var
    x: integer;
begin
    self.SelectAndOpenAll(false);
    try
        for x := 0 to high(aRackRecs) do
        begin
            aRackRecs[x].Run := aRunName;
            self.WriteRackRecAtCursor(self.DataProvider, aRackRecs[x], true);
        end;
    finally
        Close();
    end;
end;

procedure TLayoutDataAdaptor.SelectAndOpenRackName(const aRunName, aLayout, aRackName: string;
    aReadOnly: boolean);
begin
    SelectAndOpen(MakeRunNameAndLayoutSQL(aRunName, aLayout) + ' AND ' +
        Format(STR_LAYOUT_FLD_RACKNAME_COMPARE_FMT, [aRackName]), aReadOnly);
end;

function TLayoutDataAdaptor.RackExists(const aRunName, aLayout, aRackName: string): boolean;
begin
    SelectAndOpenRackName(aRunName, aLayout, aRackName, true);
    try
        result := not self.DataProvider.IsEmpty;
    finally
        Close();
    end;
end;

procedure TLayoutDataAdaptor.WriteSlot(const aRunName, aLayout, aRackName, aCarrierName: string;
    const aSlot: integer; const aRotation: double);
begin
    SelectAndOpenRackName(aRunName, aLayout, aRackName, false);
    try
        if self.DataProvider.IsEmpty then
            EXIT;
        self.DataProvider.Edit;
        self.DataProvider.FieldbyName(STR_LAYOUT_FLD_CARRIERNAME).AsString := aCarrierName;
        self.DataProvider.FieldbyName(STR_LAYOUT_FLD_SLOT).AsInteger := aSlot;
        self.DataProvider.FieldbyName(STR_LAYOUT_FLD_RACK_ROTATION).AsFloat := aRotation;
        self.DataProvider.Post;
    finally
        Close();
    end;
end;

procedure TLayoutDataAdaptor.WriteRackID(const aRunName, aLayout, aRackName, aRackID: string;
    out oOldRackID: string);
begin
    oOldRackID := '';
    SelectAndOpenRackName(aRunName, aLayout, aRackName, false);
    try
        if self.DataProvider.IsEmpty then
            EXIT;
        self.DataProvider.Edit;
        oOldRackID := self.DataProvider.FieldbyName(STR_LAYOUT_FLD_RACKID).AsString;
        self.DataProvider.FieldbyName(STR_LAYOUT_FLD_RACKID).AsString := aRackID;
        self.DataProvider.Post;
    finally
        Close();
    end;
end;

procedure TLayoutDataAdaptor.ReadRackInfo(const aRunName, aLayout, aRackName: string;
    out oCarrierName, oRackID, oRackType: string; out oSlot: integer; out oRotation: double);
begin
    SelectAndOpenRackName(aRunName, aLayout, aRackName, true);
    try
        if self.DataProvider.IsEmpty then
            EXIT;
        oCarrierName := self.DataProvider.FieldbyName(STR_LAYOUT_FLD_CARRIERNAME).AsString;
        oSlot := self.DataProvider.FieldbyName(STR_LAYOUT_FLD_SLOT).AsInteger;
        oRotation := self.DataProvider.FieldbyName(STR_LAYOUT_FLD_RACK_ROTATION).AsFloat;
        oRackID := self.DataProvider.FieldbyName(STR_LAYOUT_FLD_RACKID).AsString;
        oRackType := self.DataProvider.FieldbyName(STR_LAYOUT_FLD_RACKTYPE).AsString;
    finally
        Close();
    end;
end;

function TLayoutDataAdaptor.ReadRackID(const aRunName, aLayout, aRackName: string;
    out oRackID: string): boolean;
begin
    SelectAndOpenRackName(aRunName, aLayout, aRackName, true);
    try
        oRackID := '';
        result := not self.DataProvider.IsEmpty;
        if not result then
            EXIT;
        oRackID := self.DataProvider.FieldbyName(STR_LAYOUT_FLD_RACKID).AsString;
    finally
        Close();
    end;
end;

function TLayoutDataAdaptor.ReadRackType(const aRunName, aLayout, aRackName: string;
    out oRackType: string): boolean;
begin
    SelectAndOpenRackName(aRunName, aLayout, aRackName, true);
    try
        oRackType := '';
        result := not self.DataProvider.IsEmpty;
        if not result then
            EXIT;
        oRackType := self.DataProvider.FieldbyName(STR_LAYOUT_FLD_RACKTYPE).AsString;
    finally
        Close();
    end;
end;

procedure TLayoutDataAdaptor.SelectAndOpenCarriersByLayout(const aLayout: string; aReadOnly: boolean);
begin
    SelectAndOpen(MakeLayoutSQL(aLayout) + ' AND ' + STR_SQL_RACKNAME_NULL + ' AND ' +
        STR_SQL_LINKEDLAYOUT_NULL + ' ORDER BY CARRIERNAME', aReadOnly);
end;

procedure TLayoutDataAdaptor.SelectAndOpenCarriers(aReadOnly: boolean);
begin
    SelectAndOpen(STR_SQL_SELECT_ALL_RUN_NULL + ' AND ' + STR_SQL_RACKNAME_NULL + ' AND ' +
        STR_SQL_LINKEDLAYOUT_NULL + ' ORDER BY CARRIERNAME', aReadOnly);
end;

procedure TLayoutDataAdaptor.ReadCarrierRecsByLayout(const aLayout: string;
    var vRecs: TLayoutCarrierRecArray);
begin
    SelectAndOpenCarriersByLayout(aLayout, true);
    try
        ReadCarrierRecsAtCursor(self.DataProvider, vRecs);
    finally
        Close();
    end;
end;

procedure TLayoutDataAdaptor.ReadCarrierRecs(var vRecs: TLayoutCarrierRecArray);
begin
    // NOTE: The carriername/type/xyz pos information is containted only in the basic layout and NOT in the run layouts

    SelectAndOpenCarriers(true);
    try
        ReadCarrierRecsAtCursor(self.DataProvider, vRecs);
    finally
        Close();
    end;
end;

procedure TLayoutDataAdaptor.UpdateCarrierRunType(const aRunName, aCarrierName, aNewCarrierType: string);
begin
    SelectAndOpenCarrierName(aRunName, '', aCarrierName, false);
    try
        if self.DataProvider.IsEmpty then
            EXIT;
        self.DataProvider.Edit;
        self.DataProvider.FieldbyName(STR_LAYOUT_FLD_CARRIERTYPE).AsString := aNewCarrierType;
        self.DataProvider.Post;
    finally
        Close();
    end;
end;

procedure TLayoutDataAdaptor.UpdateRackRunType(const aRunName, aRackName, aNewRackType: string);
begin
    SelectAndOpenRackName(aRunName, '', aRackName, false);
    try
        if self.DataProvider.IsEmpty then
            EXIT;
        self.DataProvider.Edit;
        self.DataProvider.FieldbyName(STR_LAYOUT_FLD_RACKTYPE).AsString := aNewRackType;
        self.DataProvider.Post;
    finally
        Close();
    end;
end;

procedure TLayoutDataAdaptor.SelectAndOpenRacksByLayout(const aLayout: string; aReadOnly: boolean);
begin
    SelectAndOpen(MakeLayoutSQL(aLayout) + ' AND ' + STR_SQL_RACKNAME_NOT_NULL + ' ORDER BY RACKNAME',
        aReadOnly);
end;

procedure TLayoutDataAdaptor.SelectAndOpenRacksByRun(const aRunName: string; aReadOnly: boolean);
begin
    SelectAndOpen(MakeRunNameSQL(aRunName) + ' AND ' + STR_SQL_RACKNAME_NOT_NULL + ' ORDER BY RACKNAME',
        aReadOnly);
end;

procedure TLayoutDataAdaptor.SelectAndOpenRacks(aReadOnly: boolean);
begin
    SelectAndOpen(STR_SQL_SELECT_ALL_RUN_NULL + ' AND ' + STR_SQL_RACKNAME_NOT_NULL + ' ORDER BY RACKNAME',
        aReadOnly);
end;

procedure TLayoutDataAdaptor.ReadRackRecsByLayout(const aLayout: string; var vRecs: TLayoutRackRecArray);
begin
    // 'SELECT * FROM LAYOUT WHERE Layout = aLayout AND Run = aRunName AND RakcName IS NOT NULL'
    SelectAndOpenRacksByLayout(aLayout, true);
    try
        ReadRackRecsAtCursor(self.DataProvider, vRecs);
    finally
        Close();
    end;
end;

procedure TLayoutDataAdaptor.ReadRackRecsByRun(const aRunName: string; var vRecs: TLayoutRackRecArray);
begin
    SelectAndOpenRacksByRun(aRunName, true);
    try
        ReadRackRecsAtCursor(self.DataProvider, vRecs);
    finally
        Close();
    end;
end;

procedure TLayoutDataAdaptor.ReadRackRecs(var vRecs: TLayoutRackRecArray);
begin
    SelectAndOpenRacks(true);
    try
        ReadRackRecsAtCursor(self.DataProvider, vRecs);
    finally
        Close();
    end;
end;

function TLayoutDataAdaptor.BCEqualToOther(const aRunName, aLayout, aRackName, aRackID: string): string;
begin
    result := '';

    if (aRackID = '') or (aRackID = 'BCERROR') or (aRackID = 'NOCODE') then
        EXIT;

    SelectAndOpen(MakeRunNameAndLayoutSQL(aRunName, aLayout) + ' AND ' +
        Format(STR_LAYOUT_FLD_RACKID_COMPARE_FMT, [aRackID]) + ' AND NOT ' +
        Format(STR_LAYOUT_FLD_RACKNAME_COMPARE_FMT, [aRackName]), true);
    try
        if self.DataProvider.IsEmpty then
            EXIT;

        result := self.DataProvider.FieldByName('RACKNAME').AsString;
    finally
        Close();
    end;
end;

procedure TLayoutDataAdaptor.ReadRackTypeLayoutNames(const aRackType: string; var vRecs: TLayoutRecArray);
begin
    SelectAndOpen(STR_SQL_SELECT_ALL + ' WHERE ' + Format(STR_LAYOUT_FLD_RACKTYPE_COMPARE_FMT,
        [aRackType]), true);
    try
        self.ReadRecsAtCursor(self.DataProvider, vRecs);
    finally
        Close();
    end;
end;

procedure TLayoutDataAdaptor.ReadCarrierTypeLayoutNames(const aCarrierType: string;
    var vRecs: TLayoutRecArray);
begin
    SelectAndOpen(STR_SQL_SELECT_ALL + ' WHERE ' + Format(STR_LAYOUT_FLD_CARRIERTYPE_COMPARE_FMT,
        [aCarrierType]), true);
    try
        self.ReadRecsAtCursor(self.DataProvider, vRecs);
    finally
        Close();
    end;
end;

procedure TLayoutDataAdaptor.ReadRecsByTypeAndCarrXY(const aCarrierType: string; aCarrX, aCarrY: double;
    var vRecs: TLayoutRecArray);
var
    xSettings: TFormatSettings;
begin
    xSettings := TFormatUtils.GetSettingsEnglishUS;
    SelectAndOpen(STR_SQL_SELECT_ALL + ' WHERE ' + Format(STR_LAYOUT_FLD_CARRIERTYPE_COMPARE_FMT,
        [aCarrierType]) + ' AND ' + Format(STR_LAYOUT_FLD_CARR_X_COMPARE_FMT, [aCarrX, aCarrX], xSettings) +
        ' AND ' + Format(STR_LAYOUT_FLD_CARR_Y_COMPARE_FMT, [aCarrY, aCarrY], xSettings), true);

    try
        self.ReadRecsAtCursor(self.DataProvider, vRecs);
    finally
        Close();
    end;
end;

procedure TLayoutDataAdaptor.WriteCarrXYForCarrier(const aLayout, aCarrierName: string;
    aCarrX, aCarrY: double);
begin
    SelectAndOpenCarrierName('', aLayout, aCarrierName, false);
    try
        if self.DataProvider.IsEmpty then
            EXIT;
        self.DataProvider.Edit;
        self.DataProvider.FieldByName(STR_LAYOUT_FLD_CARR_X).AsFloat := aCarrX;
        self.DataProvider.FieldByName(STR_LAYOUT_FLD_CARR_Y).AsFloat := aCarrY;
        self.DataProvider.Post;
    finally
        Close();
    end;
end;

procedure TLayoutDataAdaptor.ReadLinkedLayoutRecs(var vRecs: TLayoutLinkRecArray);
var
    x: integer;
begin
    SelectAndOpen(STR_SQL_SELECT_LINKEDLAYOUT, true);
    try
        SetLength(vRecs, self.DataProvider.RecordCount);
        x := 0;
        while not self.DataProvider.Eof do
        begin
            vRecs[x].Layout := self.DataProvider.FieldByName(STR_LAYOUT_FLD_NAME).AsString;
            vRecs[x].LinkedLayout := self.DataProvider.FieldByName(STR_LAYOUT_FLD_LINKEDLAYOUT).AsString;
            self.DataProvider.Next;
            Inc(x);
        end;
    finally
        Close();
    end;

end;

procedure TLayoutDataAdaptor.SelectAndOpenLinkedLayout(const aRunName, aLayout, aLinkedLayout: string;
    aReadOnly: boolean);
begin
    SelectAndOpen(MakeRunNameAndLayoutSQL(aRunName, aLayout) + ' AND ' +
        Format(STR_LAYOUT_FLD_LINKEDLAYOUT_COMPARE_FMT, [aLinkedLayout]), aReadOnly);
end;

function TLayoutDataAdaptor.LinkedLayoutExists(const aRunName, aLayout, aLinkedLayout: string): boolean;
begin
    SelectAndOpenLinkedLayout(aRunName, aLayout, aLinkedLayout, true);
    try
        result := not self.DataProvider.IsEmpty;
    finally
        Close();
    end;
end;

procedure TLayoutDataAdaptor.AddLinkedLayout(const aLayout, aLinkedLayout: string);
begin
    self.SelectAndOpenAll(false);
    try
        self.DataProvider.Append;
        self.DataProvider.FieldByName(STR_LAYOUT_FLD_NAME).AsString := aLayout;
        self.DataProvider.FieldByName(STR_LAYOUT_FLD_LINKEDLAYOUT).AsString := aLinkedLayout;
        self.DataProvider.Post;
    finally
        Close();
    end;
end;

class procedure TLayoutDataAdaptor.InstDeleteRun(const aRunName: string);
var
    xDA: TLayoutDataAdaptor;
begin
    xDA := TLayoutDataAdaptor.Create;
    try
        xDA.DeleteRun(aRunName);
    finally
        FreeAndNil(xDA);
    end;
end;

class function TLayoutDataAdaptor.InstGetLayoutNameForRun(const aRunName: string): string;
var
    xDA: TLayoutDataAdaptor;
begin
    result := '';
    if aRunName = '' then
        EXIT;
    xDA := TLayoutDataAdaptor.Create();
    try
        result := xDA.GetLayoutNameForRun(aRunName)
    finally
        FreeAndNil(xDA);
    end;
end;


end.
