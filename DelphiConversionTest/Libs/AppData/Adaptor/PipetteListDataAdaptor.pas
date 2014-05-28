{ -----------------------------------------------------------------------------------------------------------
  Copyright © 2011 Zinsser Analytic GmbH - All rights reserved.
  Author       : Wolfgang Lyncke (wl)
  Description  :
  -----------------------------------------------------------------------------------------------------------
  Revision History:
  date     op  method                              track-no improvement/change
  -------- --  ----------------------------------  -------- -------------------------------------------------
  03.11.11 wl                                      TN5730   Initial Revision
  21.11.11 wl                                      TN5730   erweitert für Volumenkontrolle
  22.11.11 wl                                      TN5730   PIPDEVICE statt USEDPIPDEVICE
  30.11.11 wl                                      TN5755   neue Felder: RemarkText, RemarkNumber
  27.12.11 wl                                      TN5773   neue Felder: DESTVOLWASTE, DILVOLWASTE
  05.07.12 wl                                      TN5931   neue Felder: RunSourcePosX, ...
  05.07.12 wl                                      TN5927   DILNAME ist jetzt ein string-Feld
  12.06.13 wl                                      TN6172   neue Felder für den aktuellen Status
  10.12.13 ts  SelectAndOpenSortedList             TN6324   with Filter as parameter
  ----------------------------------------------------------------------------------------------------------- }

unit PipetteListDataAdaptor;


interface


uses
    GeneralTypes,
    DataProvider,
    QueryDataAdaptor;

type
    TPipetteListRec = record
        Valid: boolean;
        ListName: string;
        Seq: integer;
        DilAspDone: boolean;
        SampleAspDone: boolean;
        DispDone: boolean;
        RemarkText: string;
        RemarkNumber: integer;
        UsedPipDevice: string;
        UsedTips: integer;
        UsedTipType: string;
        TipBlock: integer;
        Tip: integer;
        LiqParam: string;
        SourceRack: string;
        SourcePos: integer;
        SourceVol: double;
        DestRack: string;
        DestPos: integer;
        DestVol: double;
        DestVolWaste: double;
        DiluentName: string;
        DiluentRack: string;
        DiluentPos: integer;
        DiluentVol: double;
        DiluentVolWaste: double;
        RunMissingSourceVol: double;
        RunMissingDilVol: double;
        RunReplacedSourceID: string;
        RunReplacedDestID: string;
        RunReplacedDilID: string;
        RunSourcePosX: double;
        RunSourcePosY: double;
        RunDestPosX: double;
        RunDestPosY: double;
        RunDilPosX: double;
        RunDilPosY: double;
        StartTime: TDateTime;
        EndTime: TDateTime;
    end;

    TPipetteListStatus = (plsStarted, plsDilAspDone, plsSampleAspDone, plsDispDone, plsEnded,
        plsDispDoneAndEnded);

    TPipetteListDataAdaptor = class(TQueryDataAdaptor)
    private const
        cTableName = 'PipetteList';
        cFieldNameListName = 'Name';
        cFieldNameSeq = 'Seq';
        cFieldNameDilAspDone = 'DilAspDone';
        cFieldNameSampleAspDone = 'SampleAspDone';
        cFieldNameDispDone = 'DispDone';
        cFieldNameRemarkText1 = 'RemarkText1';
        cFieldNameRemarkNumber1 = 'RemarkNumber1';
        cFieldNameUsedPipDevice = 'PIPDEVICE';
        cFieldNameUsedTips = 'USEDTIPS';
        cFieldNameUsedTipType = 'USEDTIPTYPE';
        cFieldNameTipBlock = 'TipBlock';
        cFieldNameTip = 'Tip';
        cFieldNameLiqParam = 'LiqParam';
        cFieldNameSourceRack = 'SourceRack';
        cFieldNameSourcePos = 'SourcePos';
        cFieldNameSourceVol = 'SourceVol';
        cFieldNameDestRack = 'DestRack';
        cFieldNameDestPos = 'DestPos';
        cFieldNameDestVol = 'DestVol';
        cFieldNameDestVolWaste = 'DestVolWaste';
        cFieldNameDiluentName = 'DILNAME';
        cFieldNameDiluentRack = 'DILRACK';
        cFieldNameDiluentPos = 'DILPOS';
        cFieldNameDiluentVol = 'DILVOL';
        cFieldNameDiluentVolWaste = 'DilVolWaste';
        cFieldNameRunMissingSourceVol = 'RunMissingSourceVol';
        cFieldNameRunMissingDilVol = 'RunMissingDilVol';
        cFieldNameRunReplacedSourceID = 'RunReplacedSourceID';
        cFieldNameRunReplacedDestID = 'RunReplacedDestID';
        cFieldNameRunReplacedDilID = 'RunReplacedDilID';
        cFieldNameRunSourcePosX = 'RunSourcePosX';
        cFieldNameRunSourcePosY = 'RunSourcePosY';
        cFieldNameRunDestPosX = 'RunDestPosX';
        cFieldNameRunDestPosY = 'RunDestPosY';
        cFieldNameRunDilPosX = 'RunDilPosX';
        cFieldNameRunDilPosY = 'RunDilPosY';
        cFieldNameStartTime = 'StartTime';
        cFieldNameEndTime = 'EndTime';

        cStandardOrder = 'SEQ';
    private
        class procedure WriteRecAtCursor(aDataset: TDataProvider; const aRec: TPipetteListRec;
            aAppend: boolean);
        class procedure WriteRecsAtCursor(aDataset: TDataProvider; const aRecs: TArray<TPipetteListRec>);

    protected
        function GetNameField: string; override;
    public
        constructor Create();

        procedure SelectAndOpenSortedList(const aName: string; aOrderBy: string; aFilter: string;
            aReadOnly: boolean);
        class function ReadRecAtCursor(aDataset: TDataProvider): TPipetteListRec;
        class function ReadRecsAtCursor(aDataset: TDataProvider): TArray<TPipetteListRec>;
        function ReadPipetteList(const aPipetteList: string; const aOrderBy: string; const aFilter: string)
            : TArray<TPipetteListRec>;
        function GetHighestSeq(const aName: string): integer;
        procedure WriteRecs(const aRecs: TArray<TPipetteListRec>);
        procedure WriteRec(const aRec: TPipetteListRec);
        class function InstReadAllNames(): TStringArray;
        function ReadRec(const aName: string; out oRec: TPipetteListRec): boolean;
        procedure UpdateSingleStatus(const aName: string; aSeq: integer; aType: TPipetteListStatus);
        procedure UpdateStatus(const aName: string; aSeq: TArray<integer>; aType: TPipetteListStatus);
    end;


implementation


uses
    SysUtils;

{ TPipetteListDataAdaptor }

constructor TPipetteListDataAdaptor.Create;
begin
    inherited Create(cTableName);
end;

function TPipetteListDataAdaptor.GetHighestSeq(const aName: string): integer;
begin
    self.SelectAndOpenSortedList(aName, 'SEQ', '', true);
    try
        if self.DataProvider.Eof then
        begin
            EXIT(0);
        end
        else
        begin
            self.DataProvider.Last;
            EXIT(self.DataProvider.FieldByName(cFieldNameSeq).AsInteger);
        end;
    finally
        Close();
    end;
end;

function TPipetteListDataAdaptor.GetNameField(): string;
begin
    result := cFieldNameListName;
end;

procedure TPipetteListDataAdaptor.WriteRec(const aRec: TPipetteListRec);
begin
    SelectAndOpenAll(false);
    try
        WriteRecAtCursor(self.DataProvider, aRec, true);
    finally
        Close();
    end;
end;

class procedure TPipetteListDataAdaptor.WriteRecAtCursor(aDataset: TDataProvider; const aRec: TPipetteListRec;
    aAppend: boolean);
begin
    ASSERT(aDataSet.Active, 'Dataset not active');

    if not aRec.Valid then
        EXIT;

    if aAppend then
        aDataset.Append
    else
        aDataset.Edit;

    aDataset.FieldByName(cFieldNameListName).AsString := aRec.ListName;
    aDataset.FieldByName(cFieldNameSeq).AsInteger := aRec.Seq;
    aDataset.FieldByName(cFieldNameDilAspDone).AsBoolean := aRec.DilAspDone;
    aDataset.FieldByName(cFieldNameSampleAspDone).AsBoolean := aRec.SampleAspDone;
    aDataset.FieldByName(cFieldNameDispDone).AsBoolean := aRec.DispDone;
    aDataset.FieldByName(cFieldNameRemarkText1).AsString := aRec.RemarkText;
    aDataset.FieldByName(cFieldNameRemarkNumber1).AsInteger := aRec.RemarkNumber;
    aDataset.FieldByName(cFieldNameUsedPipDevice).AsString := aRec.UsedPipDevice;
    aDataset.FieldByName(cFieldNameUsedTips).AsInteger := aRec.UsedTips;
    aDataset.FieldByName(cFieldNameUsedTipType).AsString := aRec.UsedTipType;
    aDataset.FieldByName(cFieldNameTipBlock).AsInteger := aRec.TipBlock;
    aDataset.FieldByName(cFieldNameTip).AsInteger := aRec.Tip;
    aDataset.FieldByName(cFieldNameLiqParam).AsString := aRec.LiqParam;
    aDataset.FieldByName(cFieldNameSourceRack).AsString := aRec.SourceRack;
    aDataset.FieldByName(cFieldNameSourcePos).AsInteger := aRec.SourcePos;
    aDataset.FieldByName(cFieldNameSourceVol).AsFloat := aRec.SourceVol;
    aDataset.FieldByName(cFieldNameDestRack).AsString := aRec.DestRack;
    aDataset.FieldByName(cFieldNameDestPos).AsInteger := aRec.DestPos;
    aDataset.FieldByName(cFieldNameDestVol).AsFloat := aRec.DestVol;
    aDataset.FieldByName(cFieldNameDestVolWaste).AsFloat := aRec.DestVolWaste;
    aDataset.FieldByName(cFieldNameDiluentName).AsString := aRec.DiluentName;
    aDataset.FieldByName(cFieldNameDiluentRack).AsString := aRec.DiluentRack;
    aDataset.FieldByName(cFieldNameDiluentPos).AsInteger := aRec.DiluentPos;
    aDataset.FieldByName(cFieldNameDiluentVol).AsFloat := aRec.DiluentVol;
    aDataset.FieldByName(cFieldNameDiluentVolWaste).AsFloat := aRec.DiluentVolWaste;

    aDataset.FieldByName(cFieldNameRunMissingSourceVol).AsFloat := aRec.RunMissingSourceVol;
    aDataset.FieldByName(cFieldNameRunMissingDilVol).AsFloat := aRec.RunMissingDilVol;
    aDataset.FieldByName(cFieldNameRunReplacedSourceID).AsString := aRec.RunReplacedSourceID;
    aDataset.FieldByName(cFieldNameRunReplacedDestID).AsString := aRec.RunReplacedDestID;
    aDataset.FieldByName(cFieldNameRunReplacedDilID).AsString := aRec.RunReplacedDilID;
    aDataset.FieldByName(cFieldNameRunSourcePosX).AsFloat := aRec.RunSourcePosX;
    aDataset.FieldByName(cFieldNameRunSourcePosY).AsFloat := aRec.RunSourcePosY;
    aDataset.FieldByName(cFieldNameRunDestPosX).AsFloat := aRec.RunDestPosX;
    aDataset.FieldByName(cFieldNameRunDestPosY).AsFloat := aRec.RunDestPosY;
    aDataset.FieldByName(cFieldNameRunDilPosX).AsFloat := aRec.RunDilPosX;
    aDataset.FieldByName(cFieldNameRunDilPosY).AsFloat := aRec.RunDilPosY;

    // StartTime und EndTime sollen null bleiben
    // aDataset.FieldByName(cFieldNameStartTime).AsDateTime := aRec.StartTime;
    // aDataset.FieldByName(cFieldNameEndTime).AsDateTime := aRec.EndTime;

    aDataset.Post;
end;

class procedure TPipetteListDataAdaptor.WriteRecsAtCursor(aDataset: TDataProvider;
    const aRecs: TArray<TPipetteListRec>);
var
    i: integer;
begin
    for i := 0 to high(aRecs) do
    begin
        WriteRecAtCursor(aDataset, aRecs[i], true);
    end;
end;

procedure TPipetteListDataAdaptor.WriteRecs(const aRecs: TArray<TPipetteListRec>);
begin
    SelectAndOpenAll(false);
    try
        WriteRecsAtCursor(self.DataProvider, aRecs);
    finally
        Close();
    end;
end;

function TPipetteListDataAdaptor.ReadPipetteList(const aPipetteList: string; const aOrderBy: string;
    const aFilter: string): TArray<TPipetteListRec>;
begin
    self.SelectAndOpenSortedList(aPipetteList, aOrderBy, aFilter, true);
    try
        result := ReadRecsAtCursor(self.DataProvider);
    finally
        Close();
    end;
end;

class function TPipetteListDataAdaptor.ReadRecAtCursor(aDataset: TDataProvider): TPipetteListRec;
begin
    ASSERT(aDataSet.Active, 'Dataset not active');

    result.Valid := true;
    result.ListName := aDataset.FieldByName(cFieldNameListName).AsString;
    result.Seq := aDataset.FieldByName(cFieldNameSeq).AsInteger;
    result.DilAspDone := aDataset.FieldByName(cFieldNameDilAspDone).AsBoolean;
    result.SampleAspDone := aDataset.FieldByName(cFieldNameSampleAspDone).AsBoolean;
    result.DispDone := aDataset.FieldByName(cFieldNameDispDone).AsBoolean;
    result.RemarkText := aDataset.FieldByName(cFieldNameRemarkText1).AsString;
    result.RemarkNumber := aDataset.FieldByName(cFieldNameRemarkNumber1).AsInteger;
    result.UsedPipDevice := aDataset.FieldByName(cFieldNameUsedPipDevice).AsString;
    result.UsedTips := aDataset.FieldByName(cFieldNameUsedTips).AsInteger;
    result.UsedTipType := aDataset.FieldByName(cFieldNameUsedTipType).AsString;
    result.TipBlock := aDataset.FieldByName(cFieldNameTipBlock).AsInteger;
    result.Tip := aDataset.FieldByName(cFieldNameTip).AsInteger;
    result.LiqParam := aDataset.FieldByName(cFieldNameLiqParam).AsString;
    result.SourceRack := aDataset.FieldByName(cFieldNameSourceRack).AsString;
    result.SourcePos := aDataset.FieldByName(cFieldNameSourcePos).AsInteger;
    result.SourceVol := aDataset.FieldByName(cFieldNameSourceVol).AsFloat;
    result.DestRack := aDataset.FieldByName(cFieldNameDestRack).AsString;
    result.DestPos := aDataset.FieldByName(cFieldNameDestPos).AsInteger;
    result.DestVol := aDataset.FieldByName(cFieldNameDestVol).AsFloat;
    result.DestVolWaste := aDataset.FieldByName(cFieldNameDestVolWaste).AsFloat;
    result.DiluentName := aDataset.FieldByName(cFieldNameDiluentName).AsString;
    result.DiluentRack := aDataset.FieldByName(cFieldNameDiluentRack).AsString;
    result.DiluentPos := aDataset.FieldByName(cFieldNameDiluentPos).AsInteger;
    result.DiluentVol := aDataset.FieldByName(cFieldNameDiluentVol).AsFloat;
    result.DiluentVolWaste := aDataset.FieldByName(cFieldNameDiluentVolWaste).AsFloat;

    result.RunMissingSourceVol := aDataset.FieldByName(cFieldNameRunMissingSourceVol).AsFloat;
    result.RunMissingDilVol := aDataset.FieldByName(cFieldNameRunMissingDilVol).AsFloat;
    result.RunReplacedSourceID := aDataset.FieldByName(cFieldNameRunReplacedSourceID).AsString;
    result.RunReplacedDestID := aDataset.FieldByName(cFieldNameRunReplacedDestID).AsString;
    result.RunReplacedDilID := aDataset.FieldByName(cFieldNameRunReplacedDilID).AsString;
    result.RunSourcePosX := aDataset.FieldByName(cFieldNameRunSourcePosX).AsFloat;
    result.RunSourcePosY := aDataset.FieldByName(cFieldNameRunSourcePosY).AsFloat;
    result.RunDestPosX := aDataset.FieldByName(cFieldNameRunDestPosX).AsFloat;
    result.RunDestPosY := aDataset.FieldByName(cFieldNameRunDestPosY).AsFloat;
    result.RunDilPosX := aDataset.FieldByName(cFieldNameRunDilPosX).AsFloat;
    result.RunDilPosY := aDataset.FieldByName(cFieldNameRunDilPosY).AsFloat;
    result.StartTime := aDataset.FieldByName(cFieldNameStartTime).AsDateTime;
    result.EndTime := aDataset.FieldByName(cFieldNameEndTime).AsDateTime;
end;

class function TPipetteListDataAdaptor.ReadRecsAtCursor(aDataset: TDataProvider): TArray<TPipetteListRec>;
var
    i: integer;
begin
    i := 0;
    SetLength(result, aDataset.RecordCount);
    while not aDataset.Eof do
    begin
        result[i] := ReadRecAtCursor(aDataset);
        aDataset.Next;
        Inc(i);
    end;
end;

procedure TPipetteListDataAdaptor.SelectAndOpenSortedList(const aName: string; aOrderBy: string;
    aFilter: string; aReadOnly: boolean);
var
    xSQL: string;
begin
    xSQL := 'select * from PipetteList where name = ''' + aName + '''';
    if aFilter <> '' then
        xSQL := xSQL + ' and ' + aFilter;
    if aOrderBy = '' then
        aOrderBy := cStandardOrder;
    xSQL := xSQL + ' order by ' + aOrderBy;
    self.SelectAndOpen(xSQL, aReadOnly);
end;

procedure TPipetteListDataAdaptor.UpdateSingleStatus(const aName: string; aSeq: integer;
    aType: TPipetteListStatus);
const
    cSQLEnded = 'EndTime = CombineDateTime(TODAY,NOW)';
    cSQLDispDone = 'DispDone = TRUE';
var
    xSQL: string;
begin
    if (aName = '') or (aSeq <= 0) then
        EXIT;

    case (aType) of
        plsStarted:
            xSQL := 'StartTime = CombineDateTime(TODAY,NOW)';
        plsDilAspDone:
            xSQL := 'DilAspDone = TRUE';
        plsSampleAspDone:
            xSQL := 'SampleAspDone = TRUE';
        plsDispDone:
            xSQL := cSQLDispDone;
        plsEnded:
            xSQL := cSQLEnded;
        plsDispDoneAndEnded:
            xSQL := cSQLDispDone + ',' + cSQLEnded;
        else
            raise Exception.Create('Undefined Pipette status');
    end;
    self.ExecSQL('Update PipetteList set ' + xSQL + ' where ' + cFieldNameListName + '=''' + aName + ''' and '
        + cFieldNameSeq + '=' + IntToStr(aSeq));
end;

procedure TPipetteListDataAdaptor.UpdateStatus(const aName: string; aSeq: TArray<integer>;
    aType: TPipetteListStatus);
var
    x: integer;
begin
    for x := 0 to high(aSeq) do
        self.UpdateSingleStatus(aName, aSeq[x], aType);
end;

class function TPipetteListDataAdaptor.InstReadAllNames(): TStringArray;
var
    xDA: TPipetteListDataAdaptor;
begin
    xDA := TPipetteListDataAdaptor.Create;
    try
        result := xDA.ReadAllNames();
    finally
        FreeAndNil(xDA);
    end;
end;

function TPipetteListDataAdaptor.ReadRec(const aName: string; out oRec: TPipetteListRec): boolean;
begin
    self.SelectAndOpenByName(aName, true);
    try
        result := not self.DataProvider.Eof;
        oRec := self.ReadRecAtCursor(self.DataProvider);
    finally
        Close();
    end;
end;


end.
