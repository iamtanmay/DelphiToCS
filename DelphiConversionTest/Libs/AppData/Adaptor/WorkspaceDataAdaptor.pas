{ --------------------------------------------------------------------------------------------------
  Copyright © 2008 Zinsser Analytic GmbH - All rights reserved.
  Author       : Payman Kamali (pk)
  Description  : Data Adaptor for the Workspace Table
  --------------------------------------------------------------------------------------------------
  Revision History:
  date     op  method                       track-no improvement/change
  -------- --  ---------------------------  -------- -----------------------------------------------
  23.06.08 pk                                TN4139  Initial Revision
  27.06.08 pk  FindNextAvailID               TN4139   New
  02.07.08 pk  SaveWorkspaceAs               TN4139   New
  16.07.08 wl                                TN4164   Field names are defined here
  16.07.08 wl                                TN4164   UsedDevices instead of UsedArms
  16.01.09 wl                                TN4362   an Änderungen in TQueryDataAdaptor angepasst
  09.07.09 pk                                TN4585.4 DataProvider.Locate removed, functionality replaced by SelectAndOpen
  04.11.09 pk                                TN4843   Replace TObjectList, TStringList, TStrings with Generics Lists and TStringArray
  13.04.10 wl                                TN5044   uses StringUtilities
  17.06.10 pk                                TN5152.1 Change SQLs so that they also work for TurboDB (dbl quoatations to single)
  08.02.11 wl  Instance                      TN5475   entfernt
  27.03.13 wl                                TN6045   uses geändert
  -------------------------------------------------------------------------------------------------- }

unit WorkspaceDataAdaptor;


interface


uses
    DataProvider,
    QueryDataAdaptor,
    ListClasses,
    GeneralTypes;

type
    TCoordSystemRelationRec = record
        Valid: boolean;
        TranslateX: double;
        TranslateY: double;
        TranslateZ: double;
        ReflectX: boolean;
        ReflectY: boolean;
        ReflectZ: boolean;
        RotateX: double;
        RotateY: double;
        RotateZ: double;
    end;

    TWorkspaceRec = record
        ID: integer;
        name: string;
        UseAllDevices: boolean;
        WorldRelation: TCoordSystemRelationRec;
        ViewRelation: TCoordSystemRelationRec;
        X, Y, Z: double;
        Color: integer;
    end;

    TWorkspaceRecArray = array of TWorkspaceRec;

    TWorkspaceUtils = record
    public const
        cCoordSystemRelationParamCount = 9;

    const
        cCoordSystemRelationDelim = ',';
        class function MakeCoordSystemRelationRec(aValid: boolean;
            aTranslateX, aTranslateY, aTranslateZ: double; aReflectX, aReflectY, aReflectZ: boolean;
            aRotateX, aRotateY, aRotateZ: double): TCoordSystemRelationRec; static;

        class function MakeDefCoordSystemRelationRec(): TCoordSystemRelationRec; static;
        class function StrToCoordSystemRelationRec(const aValue: string): TCoordSystemRelationRec; static;
        class function CoordSystemRelationRecToStr(const aRec: TCoordSystemRelationRec): string; static;
    end;

    TWorkspaceDataAdaptor = class(TQueryDataAdaptor)
    private const
        STR_WORKSPACE_TABLE = 'WORKSPACE';
        STR_WORKSPACE_FLD_ID = 'ID';
        STR_WORKSPACE_FLD_NAME = 'NAME';
        STR_WORKSPACE_FLD_USEALLDEVICES = 'USEALLDEVICES';
        STR_WORKSPACE_FLD_WORLDRELATION = 'WORLDRELATION';
        STR_WORKSPACE_FLD_VIEWRELATION = 'VIEWRELATION';
        STR_WORKSPACE_FLD_X = 'X_MM';
        STR_WORKSPACE_FLD_Y = 'Y_MM';
        STR_WORKSPACE_FLD_Z = 'Z_MM';
        STR_WORKSPACE_FLD_COLOR = 'COLOR';
        INT_WORKSPACE_FLDLEN_NAME = 50;
        STR_SQL_WHERE_ID = ' WHERE ' + STR_WORKSPACE_FLD_ID + ' =%d';
        STR_SQL_WHERE_NAME = ' WHERE ' + STR_WORKSPACE_FLD_NAME + ' =''%s''';
        STR_SQL_FROM = ' FROM ' + STR_WORKSPACE_TABLE;
        STR_SQL_SELECT_ALL = 'SELECT * ' + STR_SQL_FROM;
        STR_SQL_SELECT_BYID_FMT = 'SELECT * ' + STR_SQL_FROM + STR_SQL_WHERE_ID;
        STR_SQL_SELECT_BYNAME_FMT = 'SELECT * ' + STR_SQL_FROM + STR_SQL_WHERE_NAME;
        STR_SQL_DELETY_BYID_FMT = 'DELETE ' + STR_SQL_FROM + STR_SQL_WHERE_ID;
        cIDDefault = 0;
    private
        class procedure ReadRecFromDataset(aDataset: TDataProvider; var vRec: TWorkspaceRec);
        class procedure WriteRecToDataset(aDataset: TDataProvider; const aRec: TWorkspaceRec;
            aAppend: boolean);
        procedure SelectAndOpenByName(const aName: string; aReadOnly: boolean);
        procedure SelectAndOpenByID(aID: integer; aReadOnly: boolean);
        class function GetNextAvailID(aExistingIDList: TIntegerKeyObjectValueList): integer;
    protected
        function GetNameField: string; override;
        function GetKeyFields: TArray<string>; override;
    public
        constructor Create();

        procedure SaveWorkspaceAs(const aOldName, aNewName: string);
        function ReadIDByName(const aName: string; out oID: integer): boolean;
        function ReadRecByID(aID: integer; var vRec: TWorkspaceRec): boolean;
        function ReadRecByName(const aName: string; var vRec: TWorkspaceRec): boolean;
        function ReadAllIDs(aIDs: TIntegerKeyObjectValueList): boolean;
        function IDExists(aID: integer): boolean;
        procedure WriteRec(const aRec: TWorkspaceRec);
        procedure ReadRecs(var vRecs: TWorkspaceRecArray);
        procedure WriteRecs(aRecs: TWorkspaceRecArray);
        procedure DeleteByID(aID: integer);
        function FindNextAvailID: integer;
        class function InstNameExists(const aName: string): boolean;
        class function InstReadAllNames(): TStringArray;
        class function GetIDDefault(): integer;
        class function GetFieldLengthName(): integer;
    end;


implementation


uses
    SysUtils,
    StringUtilities;

{ TWorkspaceUtils }

class function TWorkspaceUtils.MakeCoordSystemRelationRec(aValid: boolean;
    aTranslateX, aTranslateY, aTranslateZ: double; aReflectX, aReflectY, aReflectZ: boolean;
    aRotateX, aRotateY, aRotateZ: double): TCoordSystemRelationRec;
begin
    with result do
    begin
        Valid := aValid;
        TranslateX := aTranslateX;
        TranslateY := aTranslateY;
        TranslateZ := aTranslateZ;
        ReflectX := aReflectX;
        ReflectY := aReflectY;
        ReflectZ := aReflectZ;
        RotateX := aRotateX;
        RotateY := aRotateY;
        RotateZ := aRotateZ;
    end;
end;

class function TWorkspaceUtils.MakeDefCoordSystemRelationRec(): TCoordSystemRelationRec;
begin
    result := MakeCoordSystemRelationRec(false, 0, 0, 0, false, false, false, 0, 0, 0);
end;

class function TWorkspaceUtils.StrToCoordSystemRelationRec(const aValue: string): TCoordSystemRelationRec;
var
    xValues: TStringArray;
begin
    result := MakeDefCoordSystemRelationRec();
    if aValue = '' then
        EXIT;

    xValues := TStringUtilities.StringToStringArray(aValue, cCoordSystemRelationDelim);
    if Length(xValues) <> cCoordSystemRelationParamCount then
        EXIT;
    result := MakeCoordSystemRelationRec(true, StrToFloatDef(xValues[0], result.TranslateX),
        StrToFloatDef(xValues[1], result.TranslateY), StrToFloatDef(xValues[2], result.TranslateZ),
        StrToBoolDef(xValues[3], result.ReflectX), StrToBoolDef(xValues[4], result.ReflectY),
        StrToBoolDef(xValues[5], result.ReflectZ), StrToFloatDef(xValues[6], result.RotateX),
        StrToFloatDef(xValues[7], result.RotateY), StrToFloatDef(xValues[8], result.RotateZ));
end;

class function TWorkspaceUtils.CoordSystemRelationRecToStr(const aRec: TCoordSystemRelationRec): string;
var
    xValues: TStringArray;
begin
    result := '';
    if not aRec.Valid then
        EXIT;
    SetLength(xValues, cCoordSystemRelationParamCount);
    xValues[0] := FloatToStr(aRec.TranslateX);
    xValues[1] := FloatToStr(aRec.TranslateY);
    xValues[2] := FloatToStr(aRec.TranslateZ);
    xValues[3] := BoolToStr(aRec.ReflectX);
    xValues[4] := BoolToStr(aRec.ReflectY);
    xValues[5] := BoolToStr(aRec.ReflectZ);
    xValues[6] := FloatToStr(aRec.RotateX);
    xValues[7] := FloatToStr(aRec.RotateY);
    xValues[8] := FloatToStr(aRec.RotateZ);
    result := TStringUtilities.StringArrayToString(xValues, cCoordSystemRelationDelim);
end;

{ TWorkspaceDataAdaptor }

constructor TWorkspaceDataAdaptor.Create();
begin
    inherited Create(STR_WORKSPACE_TABLE);
end;

procedure TWorkspaceDataAdaptor.SelectAndOpenByName(const aName: string; aReadOnly: boolean);
begin
    SelectAndOpen(Format(STR_SQL_SELECT_BYNAME_FMT, [aName]), aReadOnly);
end;

procedure TWorkspaceDataAdaptor.SelectAndOpenByID(aID: integer; aReadOnly: boolean);
begin
    SelectAndOpen(Format(STR_SQL_SELECT_BYID_FMT, [aID]), aReadOnly);
end;

function TWorkspaceDataAdaptor.ReadIDByName(const aName: string; out oID: integer): boolean;
begin
    SelectAndOpenByName(aName, true);
    try
        result := not self.DataProvider.IsEmpty;
        if not result then
            EXIT;
        oID := self.DataProvider.FieldByName(STR_WORKSPACE_FLD_ID).AsInteger;
    finally
        Close();
    end;
end;

function TWorkspaceDataAdaptor.ReadRecByID(aID: integer; var vRec: TWorkspaceRec): boolean;
begin
    SelectAndOpenByID(aID, true);
    try
        result := not self.DataProvider.IsEmpty;
        if not result then
            EXIT;
        ReadRecFromDataset(self.DataProvider, vRec);
    finally
        Close();
    end;
end;

function TWorkspaceDataAdaptor.ReadRecByName(const aName: string; var vRec: TWorkspaceRec): boolean;
begin
    SelectAndOpenByName(aName, true);
    try
        result := not self.DataProvider.IsEmpty;
        if not result then
            EXIT;
        ReadRecFromDataset(self.DataProvider, vRec);
    finally
        Close();
    end;
end;

function TWorkspaceDataAdaptor.IDExists(aID: integer): boolean;
begin
    SelectAndOpenByID(aID, true);
    try
        result := not self.DataProvider.IsEmpty;
    finally
        Close();
    end;
end;

class function TWorkspaceDataAdaptor.InstNameExists(const aName: string): boolean;
var
    xDA: TWorkspaceDataAdaptor;
begin
    xDA := TWorkspaceDataAdaptor.Create;
    try
        result := xDA.NameExists(aName);
    finally
        FreeAndNil(xDA);
    end;
end;

class function TWorkspaceDataAdaptor.InstReadAllNames: TStringArray;
var
    xDA: TWorkspaceDataAdaptor;
begin
    xDA := TWorkspaceDataAdaptor.Create;
    try
        result := xDA.ReadAllNames();
    finally
        FreeAndNil(xDA);
    end;
end;

procedure TWorkspaceDataAdaptor.ReadRecs(var vRecs: TWorkspaceRecArray);
var
    i: integer;
begin
    self.SelectAndOpen(STR_SQL_SELECT_ALL, true);
    try
        SetLength(vRecs, self.DataProvider.RecordCount);
        i := 0;
        while not self.DataProvider.Eof do
        begin
            ReadRecFromDataset(self.DataProvider, vRecs[i]);
            self.DataProvider.Next;
            Inc(i);
        end;
    finally
        self.Close();
    end;
end;

procedure TWorkspaceDataAdaptor.WriteRecs(aRecs: TWorkspaceRecArray);
var
    i: integer;
begin
    self.SelectAndOpen(STR_SQL_SELECT_ALL, false);
    try
        for i := 0 to high(aRecs) do
        begin
            WriteRecToDataset(self.DataProvider, aRecs[i], true);
        end;
    finally
        self.Close();
    end;
end;

function TWorkspaceDataAdaptor.GetNameField(): string;
begin
    result := STR_WORKSPACE_FLD_NAME;
end;

class procedure TWorkspaceDataAdaptor.ReadRecFromDataset(aDataset: TDataProvider; var vRec: TWorkspaceRec);
begin
    with aDataset, vRec do
    begin
        ID := FieldByName(STR_WORKSPACE_FLD_ID).AsInteger;
        name := FieldByName(STR_WORKSPACE_FLD_NAME).AsString;
        UseAllDevices := FieldByName(STR_WORKSPACE_FLD_USEALLDEVICES).AsBoolean;
        WorldRelation := TWorkspaceUtils.StrToCoordSystemRelationRec
            (FieldByName(STR_WORKSPACE_FLD_WORLDRELATION).AsString);
        ViewRelation := TWorkspaceUtils.StrToCoordSystemRelationRec
            (FieldByName(STR_WORKSPACE_FLD_VIEWRELATION).AsString);
        X := FieldByName(STR_WORKSPACE_FLD_X).AsFloat;
        Y := FieldByName(STR_WORKSPACE_FLD_Y).AsFloat;
        Z := FieldByName(STR_WORKSPACE_FLD_Z).AsFloat;
        Color := FieldByName(STR_WORKSPACE_FLD_COLOR).AsInteger;
    end;
end;

class procedure TWorkspaceDataAdaptor.WriteRecToDataset(aDataset: TDataProvider; const aRec: TWorkspaceRec;
    aAppend: boolean);
begin
    if aAppend then
        aDataset.Append
    else
        aDataset.Edit;

    with aDataset, aRec do
    begin
        FieldByName(STR_WORKSPACE_FLD_ID).AsInteger := ID;
        FieldByName(STR_WORKSPACE_FLD_NAME).AsString := name;
        FieldByName(STR_WORKSPACE_FLD_USEALLDEVICES).AsBoolean := UseAllDevices;
        FieldByName(STR_WORKSPACE_FLD_WORLDRELATION).AsString := TWorkspaceUtils.CoordSystemRelationRecToStr
            (WorldRelation);
        FieldByName(STR_WORKSPACE_FLD_VIEWRELATION).AsString := TWorkspaceUtils.CoordSystemRelationRecToStr
            (ViewRelation);
        FieldByName(STR_WORKSPACE_FLD_X).AsFloat := X;
        FieldByName(STR_WORKSPACE_FLD_Y).AsFloat := Y;
        FieldByName(STR_WORKSPACE_FLD_Z).AsFloat := Z;
        FieldByName(STR_WORKSPACE_FLD_COLOR).AsInteger := Color;
    end;
    aDataset.Post;
end;

function TWorkspaceDataAdaptor.ReadAllIDs(aIDs: TIntegerKeyObjectValueList): boolean;
begin
    self.SelectAndOpen(STR_SQL_SELECT_ALL, true);
    try
        result := self.DataProvider.RecordCount > 0;
        while not self.DataProvider.Eof do
        begin
            aIDs.Add(self.DataProvider.FieldByName(STR_WORKSPACE_FLD_ID).AsInteger);
            self.DataProvider.Next;
        end;
    finally
        self.Close();
    end;
end;

class function TWorkspaceDataAdaptor.GetNextAvailID(aExistingIDList: TIntegerKeyObjectValueList): integer;
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

function TWorkspaceDataAdaptor.FindNextAvailID(): integer;
var
    xExistingIDs: TIntegerKeyObjectValueList;
begin
    xExistingIDs := TIntegerKeyObjectValueList.Create();
    try
        ReadAllIDs(xExistingIDs);
        result := GetNextAvailID(xExistingIDs);

    finally
        FreeAndNil(xExistingIDs);
    end;
end;

procedure TWorkspaceDataAdaptor.WriteRec(const aRec: TWorkspaceRec);
var
    xAppend: boolean;
begin
    self.SelectAndOpenByID(aRec.ID, false);
    try
        xAppend := self.DataProvider.IsEmpty;
        self.WriteRecToDataset(self.DataProvider, aRec, xAppend);
    finally
        self.Close();
    end;
end;

class function TWorkspaceDataAdaptor.GetIDDefault: integer;
begin
    result := cIDDefault;
end;

procedure TWorkspaceDataAdaptor.DeleteByID(aID: integer);
begin
    self.ExecSQLFmt(STR_SQL_DELETY_BYID_FMT, [aID]);
end;

function TWorkspaceDataAdaptor.GetKeyFields(): TArray<string>;
begin
    result := self.FieldKeyArrayOf([STR_WORKSPACE_FLD_NAME]);
end;

procedure TWorkspaceDataAdaptor.SaveWorkspaceAs(const aOldName, aNewName: string);
var
    xOldID, xNewID: integer;
begin
    ASSERT(ReadIDByName(aOldName, xOldID));
    xNewID := FindNextAvailID();
    self.SaveRecordsAs(FieldKeyArrayOf([STR_WORKSPACE_FLD_ID, STR_WORKSPACE_FLD_NAME]),
        FieldValArrayOf([xOldID, aOldName]), FieldValArrayOf([xNewID, aNewName]));
end;

class function TWorkspaceDataAdaptor.GetFieldLengthName: integer;
begin
    result := INT_WORKSPACE_FLDLEN_NAME;
end;


end.
