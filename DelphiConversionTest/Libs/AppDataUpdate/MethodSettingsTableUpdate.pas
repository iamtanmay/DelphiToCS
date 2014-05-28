{ --------------------------------------------------------------------------------------------------
  Copyright © 2008 Zinsser Analytic GmbH - All rights reserved.
  Author       : Wolfgang Lyncke (wl)
  Description  :
  --------------------------------------------------------------------------------------------------
  Revision History:
  date     op  method                                 track-no improvement/change
  -------- --  -------------------------------------  -------- -------------------------------------
  29.09.08 wl  TMethodSettingsTableStructDefV2        TN4242   Feldnamen geändert
  29.09.08 wl  TMethodSettingsTableStructDefV2        TN4242   MethodName = 50 chars
  13.03.09 ts  TMethodSettingsTableUpdateV2_1         TN4464   "Startable"-option = 1 if Layout is not NULL
  16.03.09 ts  TMethodSettingsTableUpdateV2_2         TN4471   MethodNames in METHODSETTINGS aus METHOD-Tabelle
  15.02.11 pk                               	     TN4780   changes needed to make UpdateManager compatible with TurboDB
  09.06.11 wl  TMethodSettingsTableUpdateV1_5         TN5597   Falls eine Methode mehrere eingetragene Layouts hat, kommt es nicht mehr zu Key Violation
  10.06.13 wl  TMethodSettingsTableUpdateV1_5         TN6171   Akzeptiert auch Methodennamen, die nicht gleich geschieben sind
  30.08.13 wl  TMethodSettingsTableUpdateV3           TN6236   komplett neue Struktur für MethodSettings
  24.09.13 wl  TMethodSettingsTableUpdateV3           TN6236   Bugfix
  -------------------------------------------------------------------------------------------------- }

unit MethodSettingsTableUpdate;


interface


uses
    Update,
    DataProvider,
    TableStructDef,
    TableUpdate;

type
    // table structure definitions
    TMethodSettingsTableStructDefV0 = class(TTableStructDef)
    protected
        procedure DoDefineStruct(); override;
    public const
        cTableName = 'METHODSETTINGS';
    end;

    TMethodSettingsTableStructDefV1 = class(TMethodSettingsTableStructDefV0)
    protected
        procedure DoDefineStruct(); override;
    end;

    TMethodSettingsTableStructDefV2 = class(TMethodSettingsTableStructDefV0)
    protected
        procedure DoDefineStruct(); override;
    public const
        cFieldNameMethodName = 'METHODNAME';
        cFieldNameSectionName = 'SECTIONNAME';
        cFieldNameIdentName = 'IDENTNAME';
        cFieldNameIdentValue = 'IDENTVALUE';
    end;

    TMethodSettingsTableStructDefV3 = class(TMethodSettingsTableStructDefV0)
    protected
        procedure DoDefineStruct(); override;
    public const
        cFieldNameMethodName = 'MethodName';
        cFieldNameLayoutName = 'LayoutName';
        cFieldNameAttributes = 'Attributes';
        cFieldNameComment = 'Comment';
        cFieldNameIsBuildingBlock = 'IsBuildingBlock';
        cFieldNameStartable = 'Startable';
        cFieldNameInitAtStart = 'InitAtStart';
        cFieldNameDeleteRunDataAtStart = 'DeleteRunDataAtStart';
        cFieldNameDisplayComponentName = 'DisplayComponentName';
        cFieldNameImageFileName = 'ImageFileName';
        cFieldNameRestartEventName = 'RestartEventName';
        cFieldNameEditInRunner = 'EditInRunner';
    end;

    TMethodSettingsRecV3 = record
        MethodName: string;
        LayoutName: string;
        Attributes: integer;
        CommentShort: string;
        CommentLong: string;
        IsBuildingBlock: boolean;
        Startable: boolean;
        InitAtStart: boolean;
        DeleteRunDataAtStart: boolean;
        DisplayComponentName: string;
        ImageFileName: string;
        RestartEventName: string;
    end;

    TMethodSettingsTableUpdateV1 = class(TTableUpdate)
    public
        constructor Create(aUpdateNumber: TUpdateID);
    end;

    TMethodSettingsTableUpdateV1_5 = class(TTableUpdate)
    private
        procedure MethodSettingsUpdateV1_5CustomFunc(aSender: TObject);
    protected
        function GetUpdateDescription: string; override;
    public
        constructor Create(aUpdateNumber: TUpdateID);
    end;

    TMethodSettingsTableUpdateV2 = class(TTableUpdate)
    public
        constructor Create(aUpdateNumber: TUpdateID);
    end;

    TMethodSettingsTableUpdateV2_1 = class(TTableUpdate)
    private
        procedure MethodSettingsUpdateV2_1CustomFunc(aSender: TObject);
    protected
        function GetUpdateDescription: string; override;
    public
        constructor Create(aUpdateNumber: TUpdateID);
    end;

    TMethodSettingsTableUpdateV2_2 = class(TTableUpdate)
    private
        procedure MethodSettingsUpdateV2_2CustomFunc(aSender: TObject);
    protected
        function GetUpdateDescription: string; override;
    public
        constructor Create(aUpdateNumber: TUpdateID);
    end;

    TMethodSettingsTableUpdateV3 = class(TTableUpdate)
    private
        function GetEmptyRecord(const aMethodName: string): TMethodSettingsRecV3;
        procedure AppendRecord(aDP: TDataProvider; aRec: TMethodSettingsRecV3);
        procedure MethodSettingsUpdateV3CustomFunc(aSender: TObject);
    public
        constructor Create(aUpdateNumber: TUpdateID);
    end;


implementation


uses
    SysUtils,
    TableChangeAdaptor;

const
    INT_REVISION_1 = 1;
    INT_REVISION_2 = 2;
    INT_REVISION_3 = 3;
    INT_MINORREVISION_1 = 1;
    INT_MINORREVISION_2 = 2;
    INT_MINORREVISION_5 = 5;

    { TMethodSettingsTableStructDefV0 }

procedure TMethodSettingsTableStructDefV0.DoDefineStruct;
begin
    inherited;
    fName := cTableName;
end;

{ TMethodSettingsTableStructDefV1 }

procedure TMethodSettingsTableStructDefV1.DoDefineStruct();
const
    cMethodSettingFieldNameArea = 'AREA';
    cMethodSettingFieldNameSection = 'SECTION';
    cMethodSettingFieldNameIdent = 'IDENT';
    cMethodSettingFieldNameValue = 'VALUE';
    cMethodSettingFieldLengthArea = 40;
    cMethodSettingFieldLengthSection = 40;
    cMethodSettingFieldLengthIdent = 60;
    cMethodSettingFieldLengthValue = 200;
begin
    inherited;

    AddField(cMethodSettingFieldNameArea, tftString, cMethodSettingFieldLengthArea);
    AddField(cMethodSettingFieldNameSection, tftString, cMethodSettingFieldLengthSection);
    AddField(cMethodSettingFieldNameIdent, tftString, cMethodSettingFieldLengthIdent);
    AddField(cMethodSettingFieldNameValue, tftString, cMethodSettingFieldLengthValue);

    AddIndex(cMethodSettingFieldNameArea + ';' + cMethodSettingFieldNameSection + ';' +
        cMethodSettingFieldNameIdent);
end;

{ TMethodSettingsTableStructDefV2 }

procedure TMethodSettingsTableStructDefV2.DoDefineStruct();
const
    cMethodSettingFieldLengthMethodName = 50;
    cMethodSettingFieldLengthSectionName = 40;
    cMethodSettingFieldLengthIdentName = 60;
    cMethodSettingFieldLengthIdentValue = 200;
begin
    inherited;

    // abgeleitet von VO, nicht V1, d.h. Felder werden von Grund auf neu definiert

    self.AddField(cFieldNameMethodName, tftString, cMethodSettingFieldLengthMethodName);
    self.AddField(cFieldNameSectionName, tftString, cMethodSettingFieldLengthSectionName);
    self.AddField(cFieldNameIdentName, tftString, cMethodSettingFieldLengthIdentName);
    self.AddField(cFieldNameIdentValue, tftString, cMethodSettingFieldLengthIdentValue);

    AddIndex(cFieldNameMethodName + ';' + cFieldNameSectionName + ';' + cFieldNameIdentName);
end;

{ TMethodSettingsTableStructDefV3 }

procedure TMethodSettingsTableStructDefV3.DoDefineStruct();
const
    cFieldLengthMethodName = 50;
    cFieldLengthLayoutName = 20;
    cFieldLengthDisplayComponentName = 80;
begin
    inherited;

    // abgeleitet von VO, nicht V2, d.h. Felder werden von Grund auf neu definiert

    self.AddField(cFieldNameMethodName, tftString, cFieldLengthMethodName);
    self.AddField(cFieldNameLayoutName, tftString, cFieldLengthLayoutName);
    self.AddField(cFieldNameAttributes, tftInteger, 0);
    self.AddField(cFieldNameComment, tftMemo, 0);
    self.AddField(cFieldNameIsBuildingBlock, tftBoolean, 0);
    self.AddField(cFieldNameStartable, tftBoolean, 0);
    self.AddField(cFieldNameInitAtStart, tftBoolean, 0);
    self.AddField(cFieldNameDeleteRunDataAtStart, tftBoolean, 0);
    self.AddField(cFieldNameDisplayComponentName, tftString, cFieldLengthDisplayComponentName);
    self.AddField(cFieldNameImageFileName, tftMemo, 0);
    self.AddField(cFieldNameRestartEventName, tftString, cFieldLengthMethodName); // = LengthMethodName
    self.AddField(cFieldNameEditInRunner, tftBoolean, 0);

    AddIndex(cFieldNameMethodName);
end;

{ TMethodSettingsTableUpdateV1 }

constructor TMethodSettingsTableUpdateV1.Create(aUpdateNumber: TUpdateID);
begin
    inherited Create(aUpdateNumber, TMethodSettingsTableStructDefV1, INT_REVISION_1);

    self.AlterStructure(TMethodSettingsTableStructDefV1);
    CopyMatchingFields([]);
end;

{ TMethodSettingsTableUpdateV1_5 }

constructor TMethodSettingsTableUpdateV1_5.Create(aUpdateNumber: TUpdateID);
begin
    inherited Create(aUpdateNumber, TMethodSettingsTableStructDefV1, INT_REVISION_1, INT_MINORREVISION_5);

    self.AlterStructure(TMethodSettingsTableStructDefV1);
    CopyMatchingFields([]);

    self.CustomDataFunc(MethodSettingsUpdateV1_5CustomFunc);
end;

function TMethodSettingsTableUpdateV1_5.GetUpdateDescription: string;
begin
    result := 'New Setting "Layout" will be added';
end;

procedure TMethodSettingsTableUpdateV1_5.MethodSettingsUpdateV1_5CustomFunc(aSender: TObject);
const
    STR_METHOD_V0_FLD_NAME = 'NAME';
    STR_METHOD_V0_FLD_LAYOUT = 'LAYOUT';
var
    xDestDP: TDataProvider;
    xSourceMethodDP: TDataProvider;
    xLastName: string;
begin
    xDestDP := fTableChangeAdaptor.CreateDestDataProvider();

    // pk 13.02.11 this update actually belongs in TMethodSettingsUpdate.  What is id doing in here? this is completely wrong!!
    // All changes must be done in a DestDataProvider so this is totally using the wrong concept dont do this!!!
    xDestDP.SelectAndOpenAll(TMethodSettingsTableStructDefV0.cTableName, false);
    try
        xSourceMethodDP := fTableChangeAdaptor.CreateSourceDataProvider();
        try
            xSourceMethodDP.SelectAndOpen('SELECT DISTINCT Name, Layout FROM Method', true);
            try
                xLastName := '';
                while not xSourceMethodDP.Eof do
                begin
                    if not SameText(xSourceMethodDP.FieldByName(STR_METHOD_V0_FLD_NAME).AsString,
                        xLastName) then
                    begin // wenn ein Methodenname mehrere Layouts hat, übergehen wir das

                        xLastName := xSourceMethodDP.FieldByName(STR_METHOD_V0_FLD_NAME).AsString;
                        xDestDP.Append;
                        xDestDP.FieldByName('AREA').AsString := xLastName;
                        xDestDP.FieldByName('SECTION').AsString := 'LAYOUT';
                        xDestDP.FieldByName('IDENT').AsString := 'LayoutName';
                        xDestDP.FieldByName('VALUE').AsString :=
                            xSourceMethodDP.FieldByName(STR_METHOD_V0_FLD_LAYOUT).AsString;
                        xDestDP.Post;
                    end;
                    xSourceMethodDP.Next;
                end;
            finally
                xSourceMethodDP.Close();
            end;
        finally
            xSourceMethodDP.Free;
        end;
    finally
        xDestDP.Free;
    end;

end;

{ TMethodSettingsTableUpdateV2 }

constructor TMethodSettingsTableUpdateV2.Create(aUpdateNumber: TUpdateID);
begin
    inherited Create(aUpdateNumber, TMethodSettingsTableStructDefV2, INT_REVISION_2);

    self.AlterStructure(TMethodSettingsTableStructDefV2);
    self.CopyField('AREA', 'METHODNAME');
    self.CopyField('SECTION', 'SECTIONNAME');
    self.CopyField('IDENT', 'IDENTNAME');
    self.CopyField('VALUE', 'IDENTVALUE');
end;

{ TMethodSettingsTableUpdateV2_1 }

constructor TMethodSettingsTableUpdateV2_1.Create(aUpdateNumber: TUpdateID);
begin
    inherited Create(aUpdateNumber, TMethodSettingsTableStructDefV2, INT_REVISION_2, INT_MINORREVISION_1);
    self.AlterStructure(TMethodSettingsTableStructDefV2);
    CopyMatchingFields([]);

    self.CustomDataFunc(MethodSettingsUpdateV2_1CustomFunc);
end;

function TMethodSettingsTableUpdateV2_1.GetUpdateDescription: string;
begin
    result := 'New Setting "Startable method" will be added if a LayoutName for a method exists';
end;

procedure TMethodSettingsTableUpdateV2_1.MethodSettingsUpdateV2_1CustomFunc(aSender: TObject);
var
    xDP, xSourceDP: TDataProvider;
    xMethodName: string;
begin
    xSourceDP := fTableChangeAdaptor.CreateSourceDataProvider();
    try
        xSourceDP.SelectAndOpen('SELECT * FROM Methodsettings m WHERE ' + TSQL.FieldEq('m', 'IDENTNAME',
            'LayoutName') + ' AND ' + 'IDENTVALUE is not NULL', true);

        xDP := fTableChangeAdaptor.CreateDestDataProvider();
        try
            xDP.SelectAndOpenAll('Methodsettings', false);

            while not xSourceDP.Eof do
            begin
                xMethodName := xSourceDP.FieldByName('METHODNAME').AsString;
                xDP.Append;
                xDP.FieldByName('METHODNAME').AsString := xMethodName;
                xDP.FieldByName('SECTIONNAME').AsString := 'StartableOptions';
                xDP.FieldByName('IDENTNAME').AsString := 'Startable';
                xDP.FieldByName('IDENTVALUE').AsString := '1';
                xDP.Post;
                xSourceDP.Next;
            end;

        finally
            xDP.Free;
        end;
    finally
        xSourceDP.Free;

    end;
end;

{ TMethodSettingsTableUpdateV2_2 }

constructor TMethodSettingsTableUpdateV2_2.Create(aUpdateNumber: TUpdateID);
begin
    inherited Create(aUpdateNumber, TMethodSettingsTableStructDefV2, INT_REVISION_2, INT_MINORREVISION_2);
    self.AlterStructure(TMethodSettingsTableStructDefV2);
    CopyMatchingFields([]);

    self.CustomDataFunc(MethodSettingsUpdateV2_2CustomFunc);
end;

function TMethodSettingsTableUpdateV2_2.GetUpdateDescription: string;
begin
    result := 'Methodnames in METHODSETTINGS will be renamed to be the same as in METHOD';
end;

procedure TMethodSettingsTableUpdateV2_2.MethodSettingsUpdateV2_2CustomFunc(aSender: TObject);
var
    xDPDest: TDataProvider;
    xDPSource: TDataProvider;
    xMethodName: string;
    i: integer;
begin
    xDPDest := fTableChangeAdaptor.CreateDestDataProvider();
    try
        xDPSource := fTableChangeAdaptor.CreateSourceDataProvider();
        try
            xDPSource.SelectAndOpen('Select * from METHOD m WHERE ' + TSQL.FieldEq('m', 'SEQ', '10'), true);

            while not xDPSource.Eof do
            begin
                xMethodName := xDPSource.FieldByName('NAME').AsString;
                xDPDest.SelectAndOpen('Select * from Methodsettings m where ' + TSQL.FieldSameText('m',
                    'METHODNAME', xMethodName), false);
                for i := 0 to xDPDest.RecordCount - 1 do
                begin
                    xDPDest.Edit;
                    xDPDest.FieldByName('METHODNAME').AsString := xMethodName;
                    xDPDest.Post;
                    xDPDest.Next;
                end;
                xDPDest.Close;
                xDPSource.Next;
            end;
        finally
            FreeAndNil(xDPSource);
        end;
    finally
        FreeAndNil(xDPDest);
    end;
end;

{ TMethodSettingsTableUpdateV3 }

constructor TMethodSettingsTableUpdateV3.Create(aUpdateNumber: TUpdateID);
begin
    inherited Create(aUpdateNumber, TMethodSettingsTableStructDefV2, INT_REVISION_3);
    self.AlterStructure(TMethodSettingsTableStructDefV3);

    self.CustomDataFunc(MethodSettingsUpdateV3CustomFunc);
end;

function TMethodSettingsTableUpdateV3.GetEmptyRecord(const aMethodName: string): TMethodSettingsRecV3;
begin
    result.MethodName := aMethodName;
    result.LayoutName := '';
    result.CommentShort := '';
    result.CommentLong := '';
    result.Attributes := 0;
    result.IsBuildingBlock := false;
    result.Startable := false;
    result.InitAtStart := true;
    result.DeleteRunDataAtStart := true;
    result.DisplayComponentName := '';
    result.ImageFileName := '';
    result.RestartEventName := '';
end;

procedure TMethodSettingsTableUpdateV3.AppendRecord(aDP: TDataProvider; aRec: TMethodSettingsRecV3);
var
    xComment: string;
begin
    if aRec.MethodName = '' then
        EXIT;

    aDP.Append;
    aDP.FieldByName(TMethodSettingsTableStructDefV3.cFieldNameMethodName).AsString := aRec.MethodName;
    aDP.FieldByName(TMethodSettingsTableStructDefV3.cFieldNameLayoutName).AsString := aRec.LayoutName;
    aDP.FieldByName(TMethodSettingsTableStructDefV3.cFieldNameAttributes).AsInteger := aRec.Attributes;
    xComment := aRec.CommentShort;
    if (aRec.CommentLong <> '') then
    begin
        if (aRec.CommentShort <> '') then
            xComment := xComment + #13#10 + aRec.CommentLong
        else
            xComment := aRec.CommentLong;
    end;
    aDP.FieldByName(TMethodSettingsTableStructDefV3.cFieldNameComment).AsString := xComment;
    aDP.FieldByName(TMethodSettingsTableStructDefV3.cFieldNameIsBuildingBlock).AsBoolean :=
        aRec.IsBuildingBlock;
    aDP.FieldByName(TMethodSettingsTableStructDefV3.cFieldNameStartable).AsBoolean := aRec.Startable;
    aDP.FieldByName(TMethodSettingsTableStructDefV3.cFieldNameInitAtStart).AsBoolean := aRec.InitAtStart;
    aDP.FieldByName(TMethodSettingsTableStructDefV3.cFieldNameDeleteRunDataAtStart).AsBoolean :=
        aRec.DeleteRunDataAtStart;
    aDP.FieldByName(TMethodSettingsTableStructDefV3.cFieldNameDisplayComponentName).AsString :=
        aRec.DisplayComponentName;
    aDP.FieldByName(TMethodSettingsTableStructDefV3.cFieldNameImageFileName).AsString := aRec.ImageFileName;
    aDP.FieldByName(TMethodSettingsTableStructDefV3.cFieldNameRestartEventName).AsString :=
        aRec.RestartEventName;
    aDP.FieldByName(TMethodSettingsTableStructDefV3.cFieldNameEditInRunner).AsBoolean := false;
    aDP.Post;
end;

procedure TMethodSettingsTableUpdateV3.MethodSettingsUpdateV3CustomFunc(aSender: TObject);
var
    xDPDest: TDataProvider;
    xDPSource: TDataProvider;
    xMethod, xLastMethod: string;
    xSection, xIdent, xValue: string;
    xRec: TMethodSettingsRecV3;
begin
    xDPDest := fTableChangeAdaptor.CreateDestDataProvider();
    try
        xDPSource := fTableChangeAdaptor.CreateSourceDataProvider();
        try
            xDPSource.SelectAndOpen('Select * from METHODSETTINGS order by ' +
                TMethodSettingsTableStructDefV2.cFieldNameMethodName, true);

            xDPDest.SelectAndOpen('Select * from Methodsettings', false);

            xLastMethod := '';
            xRec := GetEmptyRecord('');
            while not xDPSource.Eof do
            begin
                // Source-Record lesen
                xMethod := xDPSource.FieldByName
                    (TMethodSettingsTableStructDefV2.cFieldNameMethodName).AsString;
                xSection := xDPSource.FieldByName
                    (TMethodSettingsTableStructDefV2.cFieldNameSectionName).AsString;
                xIdent := xDPSource.FieldByName(TMethodSettingsTableStructDefV2.cFieldNameIdentName).AsString;
                xValue := xDPSource.FieldByName(TMethodSettingsTableStructDefV2.cFieldNameIdentValue)
                    .AsString;

                // Daten speichern
                if (Uppercase(xMethod) <> Uppercase(xLastMethod)) then
                begin
                    AppendRecord(xDPDest, xRec);
                    xLastMethod := xMethod;
                    xRec := GetEmptyRecord(xMethod);
                end;

                // Werte in Dest-Record übertragen
                if (xSection = 'LAYOUT') and (xIdent = 'LayoutName') then
                    xRec.LayoutName := xValue;
                if (xSection = 'Attributes') and (xIdent = 'Attributes') then
                    xRec.Attributes := StrToIntDef(xValue, 0);
                if xRec.Attributes = 3 then
                    xRec.Attributes := 2; // ReadOnly+Hidden = Hidden
                if (xSection = 'Comment') and (xIdent = 'Short') then
                    xRec.CommentShort := xValue;
                if (xSection = 'Comment') and (xIdent = 'Long') then
                    xRec.CommentLong := xValue;
                if (xSection = 'BuildingBlock') and (xIdent = 'IsBuildingBlock') and (xValue = '1') then
                    xRec.IsBuildingBlock := true;
                if (xSection = 'StartableOptions') and (xIdent = 'Startable') and (xValue = '1') then
                    xRec.Startable := true;
                if (xSection = 'InitOptions') and (xIdent = 'Init') and (xValue = '0') then
                    xRec.InitAtStart := false;
                if (xSection = 'StartableOptions') and (xIdent = 'DeleteRunData') and (xValue = '0') then
                    xRec.DeleteRunDataAtStart := false;
                if (xSection = 'DisplayOptions') and (xIdent = 'DisplayComponent') then
                    xRec.DisplayComponentName := xValue;
                if (xSection = 'BuildingBlock') and (xIdent = 'ImageFileName') then
                    xRec.ImageFileName := xValue;
                if (xSection = 'DisplayOptions') and (xIdent = 'RestartEvent') then
                    xRec.RestartEventName := xValue;

                xDPSource.Next;
            end;
            AppendRecord(xDPDest, xRec);

            xDPDest.Close;
            xDPSource.Close;
        finally
            FreeAndNil(xDPSource);
        end;
    finally
        FreeAndNil(xDPDest);
    end;
end;


end.
