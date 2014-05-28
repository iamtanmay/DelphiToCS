{ --------------------------------------------------------------------------------------------------
  Copyright © 2008 Zinsser Analytic GmbH - All rights reserved.
  Author       : Payman Kamali (pk)
  Description  : Data adaptor for LayoutWorkspace.db
  --------------------------------------------------------------------------------------------------
  Revision History:
  date     op  method                       track-no improvement/change
  -------- --  ---------------------------  -------- -----------------------------------------------
  23.06.08 pk                                TN4139  Initial Revision
  02.07.08 pk  GetNameMaxLen                 TN4139  New
  23.09.08 wl                                TN4236   Vereinigung mit ..Fieldnames
  16.01.09 wl                                TN4362   an Änderungen in TQueryDataAdaptor angepasst
  04.08.09 ts  GetNameField                  TN4569  new: für LayoutNamen
  04.11.09 pk                                TN4843 Replace TObjectList, TStringList, TStrings with Generics Lists and TStringArray
  17.06.10 pk                                TN5152.1  Change SQLs so that they also work for TurboDB (dbl quoatations to single)
  08.02.11 wl  Instance                      TN5475   entfernt
  -------------------------------------------------------------------------------------------------- }

unit LayoutWorkspaceDataAdaptor;


interface


uses
    Classes,
    DataProvider,
    QueryDataAdaptor,
    WorkspaceDataAdaptor,
    GeneralTypes,
    ListClasses;

type
    TLayoutWorkspaceRec = record
        ID: integer;
        LayoutID: string;
        name: string;
        WorkspaceTypeID: integer;
        ViewRelation: TCoordSystemRelationRec;
    end;

    TLayoutWorkspaceRecArray = array of TLayoutWorkspaceRec;

    TLayoutWorkspaceIDPair = record
        OldID, NewID: integer;
    end;

    TLayoutWorkspaceIDPairArray = array of TLayoutWorkspaceIDPair;

    TLayoutWorkspaceDataAdaptor = class(TQueryDataAdaptor)
    private const
        STR_LAYOUTWORKSPACE_TABLE = 'LAYOUTWORKSPACE';

    const
        STR_LAYOUTWORKSPACE_FLD_ID = 'ID';

    const
        STR_LAYOUTWORKSPACE_FLD_LAYOUTID = 'LAYOUTID';

    const
        STR_LAYOUTWORKSPACE_FLD_NAME = 'NAME';

    const
        STR_LAYOUTWORKSPACE_FLD_WORKSPACETYPEID = 'WORKSPACETYPEID';

    const
        STR_LAYOUTWORKSPACE_FLD_VIEWRELATION = 'VIEWRELATION';

    const
        INT_LAYOUTWORKSPACE_FLDLEN_NAME = 50;

    const
        cLinkIDNone = 0;

    const
        STR_SQL_WHERE_LAYOUTID = ' WHERE ' + STR_LAYOUTWORKSPACE_FLD_LAYOUTID + ' =''%s''';

    const
        STR_SQL_WHERE_LAYOUTID_AND_NAME = STR_SQL_WHERE_LAYOUTID + ' AND ' + STR_LAYOUTWORKSPACE_FLD_NAME +
            ' =''%s''';

    const
        STR_SQL_WHERE_ID = ' WHERE ' + STR_LAYOUTWORKSPACE_FLD_ID + ' =%d';

    const
        STR_SQL_WHERE_WORKSPACETYPEID = ' WHERE ' + STR_LAYOUTWORKSPACE_FLD_WORKSPACETYPEID + ' =%d';

    const
        STR_SQL_FROM = ' FROM ' + STR_LAYOUTWORKSPACE_TABLE;

    const
        STR_SQL_DELETE_BYLAYOUTID_FMT = 'DELETE ' + STR_SQL_FROM + STR_SQL_WHERE_LAYOUTID;

    const
        STR_SQL_SELECT_BYLAYOUTID_FMT = 'SELECT * ' + STR_SQL_FROM + STR_SQL_WHERE_LAYOUTID;

    const
        STR_SQL_SELECT_BYLAYOUTID_AND_NAME_FMT = 'SELECT * ' + STR_SQL_FROM + STR_SQL_WHERE_LAYOUTID_AND_NAME;

    const
        STR_SQL_SELECT_BYWORKSPACIDTYPEID_FMT = 'SELECT * ' + STR_SQL_FROM + STR_SQL_WHERE_WORKSPACETYPEID;

    const
        STR_SQL_SELECT_ALL = 'SELECT * ' + STR_SQL_FROM;

        procedure SelectAndOpenAll(aReadOnly: boolean);
        procedure SelectAndOpenByLayoutID(const aLayoutID: string; aReadOnly: boolean);
        procedure SelectAndOpenByLayoutIDAndName(const aLayoutID, aName: string; aReadOnly: boolean);
        procedure SelectAndOpenByWorkspaceTypeID(const aWorkspaceTypeID: integer; aReadOnly: boolean);

        class procedure WriteRecAtCursor(aDataset: TDataProvider; const aRec: TLayoutWorkspaceRec;
            aAppend: boolean);
        class procedure WriteRecsAtCursor(aDataset: TDataProvider; const aRecs: TLayoutWorkspaceRecArray);
        class procedure ReadRecIDAtCursor(aDataset: TDataProvider; var vID: integer);
        class function GetNextAvailID(aExistingIDList: TIntegerKeyObjectValueList): integer;
    protected
        function GetNameField(): string; override; // für Layoutnamen
    public
        constructor Create();

        procedure ReadRecsByLayoutID(const aLayoutID: string; var vRecs: TLayoutWorkspaceRecArray);
        procedure ReadRecIDsByLayoutID(const aLayoutID: string; var vIDs: TIntArray);
        procedure ReadAllRecs(var vRecs: TLayoutWorkspaceRecArray);
        procedure ReadAllRecIDs(var vIDs: TIntArray);
        // procedure DeleteLayoutWorkspace(const aLayoutWorkspace: string);
        procedure WriteRecs(const aRecs: TLayoutWorkspaceRecArray);
        function WorkspaceNameExists(const aLayoutID: string; const aName: string): boolean;
        procedure DeleteLayoutWorkspaceByLayoutID(const aLayoutID: string);
        procedure AddWorkspace(aID: integer; const aLayoutID: string; aName: string; aTypeID: integer;
            aViewRelation: TCoordSystemRelationRec);
        function WorkspaceTypeIDExists(aWorkspaceTypeID: integer): boolean;
        procedure SaveLayoutIDAs(const aLayoutID, aNewLayoutID: string;
            var vIDPairArray: TLayoutWorkspaceIDPairArray);

        class function IsLinked(aLinkID: integer): boolean;
        class procedure ReadRecAtCursor(aDataset: TDataProvider; var vRec: TLayoutWorkspaceRec);
        class procedure ReadRecIDsAtCursor(aDataset: TDataProvider; var vIDs: TIntArray);
        class procedure ReadRecsAtCursor(aDataset: TDataProvider; var vRecs: TLayoutWorkspaceRecArray);
        class function MakeDefaultLayoutWorkspaceRec(): TLayoutWorkspaceRec;
        class function GetNameMaxLen: integer;
        class function InstReadAllNames(): TStringArray;
    end;


implementation


uses
    SysUtils;

{ TLayoutWorkspaceDataAdaptor }

constructor TLayoutWorkspaceDataAdaptor.Create();
begin
    inherited Create(STR_LAYOUTWORKSPACE_TABLE);
end;

procedure TLayoutWorkspaceDataAdaptor.DeleteLayoutWorkspaceByLayoutID(const aLayoutID: string);
begin
    self.ExecSQL(Format(STR_SQL_DELETE_BYLAYOUTID_FMT, [aLayoutID]));
end;

procedure TLayoutWorkspaceDataAdaptor.SelectAndOpenAll(aReadOnly: boolean);
begin
    SelectAndOpen(STR_SQL_SELECT_ALL, aReadOnly)
end;

procedure TLayoutWorkspaceDataAdaptor.SelectAndOpenByLayoutID(const aLayoutID: string; aReadOnly: boolean);
begin
    ASSERT(aLayoutID <> '', 'LayoutWorkspace LayoutID is empty');
    SelectAndOpen(Format(STR_SQL_SELECT_BYLAYOUTID_FMT, [aLayoutID]), aReadOnly);
end;

procedure TLayoutWorkspaceDataAdaptor.SelectAndOpenByLayoutIDAndName(const aLayoutID, aName: string;
    aReadOnly: boolean);
begin
    ASSERT(aName <> '', 'LayoutWorkspace Name is empty');
    ASSERT(aLayoutID <> '', 'LayoutWorkspace LayoutID is empty');
    SelectAndOpen(Format(STR_SQL_SELECT_BYLAYOUTID_AND_NAME_FMT, [aLayoutID, aName]), aReadOnly);
end;

procedure TLayoutWorkspaceDataAdaptor.SelectAndOpenByWorkspaceTypeID(const aWorkspaceTypeID: integer;
    aReadOnly: boolean);
begin
    SelectAndOpen(Format(STR_SQL_SELECT_BYWORKSPACIDTYPEID_FMT, [aWorkspaceTypeID]), aReadOnly);
end;

class function TLayoutWorkspaceDataAdaptor.MakeDefaultLayoutWorkspaceRec(): TLayoutWorkspaceRec;
begin
    with result do
    begin
        ID := 0;
        name := '';
        LayoutID := '';
        WorkspaceTypeID := 0;
        ViewRelation := TWorkspaceUtils.MakeDefCoordSystemRelationRec();
    end;
end;

class function TLayoutWorkspaceDataAdaptor.InstReadAllNames: TStringArray;
var
    xDA: TLayoutWorkspaceDataAdaptor;
begin
    xDA := TLayoutWorkspaceDataAdaptor.Create;
    try
        result := xDA.ReadAllNames();
    finally
        FreeAndNil(xDA);
    end;
end;

class function TLayoutWorkspaceDataAdaptor.IsLinked(aLinkID: integer): boolean;
begin
    result := aLinkID <> cLinkIDNone;
end;

procedure TLayoutWorkspaceDataAdaptor.AddWorkspace(aID: integer; const aLayoutID: string; aName: string;
    aTypeID: integer; aViewRelation: TCoordSystemRelationRec);
var
    xRec: TLayoutWorkspaceRec;
begin
    self.SelectAndOpenAll(false);
    try
        xRec := MakeDefaultLayoutWorkspaceRec();
        xRec.ID := aID;
        xRec.LayoutID := aLayoutID;
        xRec.ViewRelation := aViewRelation;
        xRec.Name := aName;
        xRec.WorkspaceTypeID := aTypeID;

        self.WriteRecAtCursor(self.DataProvider, xRec, true);
    finally
        Close();
    end;
end;

class procedure TLayoutWorkspaceDataAdaptor.WriteRecAtCursor(aDataset: TDataProvider;
    const aRec: TLayoutWorkspaceRec; aAppend: boolean);
begin
    ASSERT(aDataSet.Active, 'Dataset not active');
    if aAppend then
        aDataset.Append
    else
        aDataset.Edit;

    with aRec do
    begin
        aDataset.FieldByName(STR_LAYOUTWORKSPACE_FLD_ID).AsInteger := ID;
        aDataset.FieldByName(STR_LAYOUTWORKSPACE_FLD_LAYOUTID).AsString := LayoutID;
        aDataset.FieldByName(STR_LAYOUTWORKSPACE_FLD_NAME).AsString := name;
        aDataset.FieldByName(STR_LAYOUTWORKSPACE_FLD_WORKSPACETYPEID).AsInteger := WorkspaceTypeID;
        aDataset.FieldByName(STR_LAYOUTWORKSPACE_FLD_VIEWRELATION).AsString :=
            TWorkspaceUtils.CoordSystemRelationRecToStr(ViewRelation);
    end;
    aDataset.Post;
end;

class procedure TLayoutWorkspaceDataAdaptor.WriteRecsAtCursor(aDataset: TDataProvider;
    const aRecs: TLayoutWorkspaceRecArray);
var
    i: integer;
begin
    for i := 0 to high(aRecs) do
    begin
        WriteRecAtCursor(aDataset, aRecs[i], true);
    end;
end;

procedure TLayoutWorkspaceDataAdaptor.WriteRecs(const aRecs: TLayoutWorkspaceRecArray);
begin
    SelectAndOpenAll(false);
    try
        WriteRecsAtCursor(self.DataProvider, aRecs);
    finally
        Close();
    end;
end;

procedure TLayoutWorkspaceDataAdaptor.ReadRecsByLayoutID(const aLayoutID: string;
    var vRecs: TLayoutWorkspaceRecArray);
begin
    SelectAndOpenByLayoutID(aLayoutID, true);
    try
        ReadRecsAtCursor(self.DataProvider, vRecs);
    finally
        Close();
    end;
end;

procedure TLayoutWorkspaceDataAdaptor.ReadRecIDsByLayoutID(const aLayoutID: string; var vIDs: TIntArray);
begin
    SelectAndOpenByLayoutID(aLayoutID, true);
    try
        ReadRecIDsAtCursor(self.DataProvider, vIDs);
    finally
        Close();
    end;
end;

procedure TLayoutWorkspaceDataAdaptor.ReadAllRecs(var vRecs: TLayoutWorkspaceRecArray);
begin
    SelectAndOpenAll(true);
    try
        ReadRecsAtCursor(self.DataProvider, vRecs);
    finally
        Close();
    end;
end;

class procedure TLayoutWorkspaceDataAdaptor.ReadRecIDAtCursor(aDataset: TDataProvider; var vID: integer);
begin
    vID := aDataset.FieldByName(STR_LAYOUTWORKSPACE_FLD_ID).AsInteger;
end;

class procedure TLayoutWorkspaceDataAdaptor.ReadRecAtCursor(aDataset: TDataProvider;
    var vRec: TLayoutWorkspaceRec);
begin
    ASSERT(aDataSet.Active, 'Dataset not active');
    with vRec do
    begin
        ReadRecIDAtCursor(aDataset, vRec.ID);
        LayoutID := aDataset.FieldByName(STR_LAYOUTWORKSPACE_FLD_LAYOUTID).AsString;
        name := aDataset.FieldByName(STR_LAYOUTWORKSPACE_FLD_NAME).AsString;
        WorkspaceTypeID := aDataset.FieldByName(STR_LAYOUTWORKSPACE_FLD_WORKSPACETYPEID).AsInteger;
        ViewRelation := TWorkspaceUtils.StrToCoordSystemRelationRec
            (aDataset.FieldByName(STR_LAYOUTWORKSPACE_FLD_VIEWRELATION).AsString);
    end;
end;

class procedure TLayoutWorkspaceDataAdaptor.ReadRecsAtCursor(aDataset: TDataProvider;
    var vRecs: TLayoutWorkspaceRecArray);
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

class procedure TLayoutWorkspaceDataAdaptor.ReadRecIDsAtCursor(aDataset: TDataProvider; var vIDs: TIntArray);
var
    i: integer;
begin
    i := 0;
    SetLength(vIDs, aDataset.RecordCount);
    while not aDataset.Eof do
    begin
        ReadRecIDAtCursor(aDataset, vIDs[i]);
        aDataset.Next;
        Inc(i);
    end;
end;

procedure TLayoutWorkspaceDataAdaptor.ReadAllRecIDs(var vIDs: TIntArray);
begin
    SelectAndOpenAll(true);
    try
        self.ReadRecIDsAtCursor(self.DataProvider, vIDS);
    finally
        Close();
    end;
end;

function TLayoutWorkspaceDataAdaptor.WorkspaceNameExists(const aLayoutID, aName: string): boolean;
begin
    SelectAndOpenByLayoutIDAndName(aName, aLayoutID, true);
    try
        result := not self.DataProvider.IsEmpty;
    finally
        Close();
    end;
end;

function TLayoutWorkspaceDataAdaptor.WorkspaceTypeIDExists(aWorkspaceTypeID: integer): boolean;
begin
    SelectAndOpenByWorkspaceTypeID(aWorkspaceTypeID, true);
    try
        result := not self.DataProvider.IsEmpty;
    finally
        Close();
    end;
end;

class function TLayoutWorkspaceDataAdaptor.GetNextAvailID(aExistingIDList
    : TIntegerKeyObjectValueList): integer;
begin
    // find new ID
    result := 1;
    while true do
    begin
        if aExistingIDList.IndexOf(result) < 0 then
            EXIT;
        Inc(result);
    end;
end;

procedure TLayoutWorkspaceDataAdaptor.SaveLayoutIDAs(const aLayoutID, aNewLayoutID: string;
    var vIDPairArray: TLayoutWorkspaceIDPairArray);
var
    xOldID, xNewID: integer;
    xAllExistingIDs, xIDs: TIntArray;
    xExistingIDList: TIntegerKeyObjectValueList;
    x: integer;
begin
    xExistingIDList := TIntegerKeyObjectValueList.Create;

    ReadAllRecIDs(xAllExistingIDs);
    for x := 0 to high(xAllExistingIDs) do
        xExistingIDList.Add(xAllExistingIDs[x]);

    ReadRecIDsByLayoutID(aLayoutID, xIDs);
    SetLength(vIDPairArray, Length(xIDs));

    for x := 0 to high(xIDs) do
    begin
        xNewID := GetNextAvailID(xExistingIDList);
        xOldID := xIDs[x];
        vIDPairArray[x].NewID := xNewID;
        vIDPairArray[x].OldID := xOldID;
        self.SaveRecordsAs(self.FieldKeyArrayOf([STR_LAYOUTWORKSPACE_FLD_ID, STR_LAYOUTWORKSPACE_FLD_LAYOUTID]
            ), self.FieldValArrayOf([xOldID, aLayoutID]), self.FieldValArrayOf([xNewID, aNewLayoutID]));
        xExistingIDList.Add(xNewID);
    end;

    FreeAndNil(xExistingIDList);
end;

class function TLayoutWorkspaceDataAdaptor.GetNameMaxLen: integer;
begin
    result := INT_LAYOUTWORKSPACE_FLDLEN_NAME;
end;

function TLayoutWorkspaceDataAdaptor.GetNameField: string;
begin
    result := STR_LAYOUTWORKSPACE_FLD_LAYOUTID;
end;


end.
