{ --------------------------------------------------------------------------------------------------
  Copyright © 2006 Zinsser Analytic GmbH - All rights reserved.
  Author       : Wolfgang Lyncke (wl)
  Description  : Data Adaptor for TIPTYPES.DB
  --------------------------------------------------------------------------------------------------
  Revision History:
  date     op  method                       track-no  improvement/change
  -------- --  ---------------------------  --------  ----------------------------------------------
  23.09.06 wl                               TN3326    initial version
  04.10.06 wl  STR_TIPTYPES_FLD_DONOTINIT   TN3342    DoNotInit (nur V7.1.x) wird jetzt auch in TIPTYPES geführt
  05.02.07 pk                               TN3544    Changes for updatemanager
  16.02.07 pk  UpdateVersion1               TN3581    now accepts DestTableDir parameter
  02.10.07 wl                               TN3811.5  benutzt self.Query statt fQuery
  09.11.07 pk                                TN3921   Changes for updatemanager
  09.11.07 pk                                TN3922   Dataset changed to DataProvider
  24.04.08 wl   Delete                       TN4073    new function for Tip type editor
  03.07.08 wl                                TN4157
  23.09.08 wl                                TN4236   Vereinigung mit TipTypeFieldnames
  16.01.09 wl                                TN4362   an Änderungen in TQueryDataAdaptor angepasst
  13.07.09 pk                                TN4585.4 DataProvider.Locate removed, functionality replaced by SelectAndOpen
  27.08.09 pk   InstGetLiquidTipTypeNames    TN4753   New
  28.08.09 pk   TipNameIsRedi                TN4753   from PipDevice
  28.08.09 pk   InstGetAllTipTypeNames       TN4753   New
  04.11.09 pk                                TN4843   Replace TObjectList, TStringList, TStrings with Generics Lists and TStringArray
  10.05.10 wl   ReadDiTiTipTypeNames         TN5105   neu für FILLD Action
  17.06.10 pk                                TN5152.1 Change SQLs so that they also work for TurboDB (dbl quoatations to single)
  27.03.13 wl                                TN6045   uses geändert
  -------------------------------------------------------------------------------------------------- }

unit TipTypeDataAdaptor;


interface


uses
    GeneralTypes,
    DataProvider,
    QueryDataAdaptor,
    CommonTypes;

// TTipType ist in CommonTypes definiert

type
    TTipTypeDataAdaptor = class(TQueryDataAdaptor)
    private const
        STR_TIPTYPES_TBL = 'TIPTYPES';
        STR_TIPTYPES_FLD_TYPENAME = 'TYPENAME';
        STR_TIPTYPES_FLD_BASICTIPTYPE = 'BASICTIPTYPE';
        STR_TIPTYPES_FLD_TOOLNAME = 'TOOLNAME';
        STR_TIPTYPES_FLD_REDIDEVICENAME = 'REDIDEVICENAME';
        STR_TIPTYPES_FLD_DILUTORNAME = 'DILUTORNAME';
        STR_TIPTYPES_FLD_MAXVOLUME = 'MAXVOLUME';
        STR_TIPTYPES_FLD_MINVOLUME = 'MINVOLUME';
        STR_TIPTYPES_FLD_XOFFSET = 'XOFFSET';
        STR_TIPTYPES_FLD_YOFFSET = 'YOFFSET';
        STR_TIPTYPES_FLD_ZOFFSET = 'ZOFFSET';
        STR_TIPTYPES_FLD_ZOFFSETREMOVABLE = 'ZOFFSETREMOVABLE';
        STR_TIPTYPES_FLD_ZOFFSETWASH = 'ZOFFSETWASH';
        STR_TIPTYPES_FLD_ZOFFSETWASTE = 'ZOFFSETWASTE';
        STR_TIPTYPES_FLD_ZOFFSETDRY = 'ZOFFSETDRY';
        STR_TIPTYPES_FLD_DRYAFTERFLUSH = 'DRYAFTERFLUSH';
        STR_TIPTYPES_FLD_METHODNAMEGETTIP = 'METHODNAMEGETTIP';
        STR_TIPTYPES_FLD_METHODNAMEPUTTIP = 'METHODNAMEPUTTIP';
        STR_TIPTYPES_FLD_DONOTINIT = 'DONOTINIT';
        STR_TIPTYPES_INDEX_FIELDS = STR_TIPTYPES_FLD_TYPENAME;
        STR_SQL_FROM = ' FROM ' + STR_TIPTYPES_TBL;
        STR_TIPTYPES_SELECT = 'SELECT * ' + STR_SQL_FROM;
        STR_TIPTYPES_SELECT_DISTINCT = 'SELECT DISTINCT ' + STR_TIPTYPES_FLD_TYPENAME + ' FROM ' +
            STR_TIPTYPES_TBL;
        STR_SQL_WHERE_TYPENAME = ' WHERE ' + STR_TIPTYPES_FLD_TYPENAME + ' = ''%s''';
        STR_SQL_DELETE_FMT = 'DELETE ' + STR_SQL_FROM + STR_SQL_WHERE_TYPENAME;
        STR_SQL_SELECT_TIPTYPE_FMT = 'SELECT * ' + STR_SQL_FROM + STR_SQL_WHERE_TYPENAME;
        STR_TIPTYPEENTRY_BTT_DITI = 'DITI';
        STR_TIPTYPEENTRY_BTT_DEFAULT = 'DEFAULT';
        STR_TIPTYPEENTRY_BTT_REDI = 'REDI';
    private
        procedure SelectAndOpenTipType(const aName: string; aReadOnly: boolean);
    protected
        function GetNameField(): string; override;
    public
        constructor Create();

        class procedure WriteRecAtCursor(aDataset: TDataProvider; const aRec: TTipType; aAppend: boolean);
        class function ReadRecAtCursor(aDataset: TDataProvider; var vRec: TTipType): boolean;
        class function GetEmptyRec(): TTipType;
        class function GetNameFromBasicTipType(aBasicTipType: TBasicTipType; const aDitiType: string): string;
        class function GetBasicTipTypeFromName(const aBasicTipTypeName: string; out oDitiType: string)
            : TBasicTipType; overload;
        class function GetBasicTipTypeFromName(const aBasicTipTypeName: string): TBasicTipType; overload;
        //
        class function TipTypeIsRedi(aTipType: TBasicTipType): boolean;

        procedure SelectAndOpenByTypeName(aTipTypeName: string; aReadOnly: boolean);
        procedure WriteTipType(const aRec: TTipType);
        function ReadTipType(const aName: string; var vRec: TTipType): boolean;

        function GetLiquidTipTypeList(): TStringArray;
        function GetPowderTipTypeList(): TStringArray;
        function GetRediDeviceList(): TStringArray;
        class function TipTypeExists(aTypeName: string; out oTypeData: TTipType): boolean;
        class function InstGetLiquidTipTypeNames(): TStringArray;
        class function InstGetPowderTipTypeNames(): TStringArray;
        class function InstReadAllNames(): TStringArray;
        class function GetTipTypeFromName(const aTypeName: string): TBasicTipType;
        class function TipNameIsRedi(const aTypeName: string): boolean;
        function ReadDiTiTipTypeNames: TStringArray;
    end;


implementation


uses
    Generics.Collections,
    SysUtils,
    AppTypes;

{ TTipTypeDataAdaptor }

constructor TTipTypeDataAdaptor.Create;
begin
    inherited Create(STR_TIPTYPES_TBL);
end;

function TTipTypeDataAdaptor.GetNameField: string;
begin
    result := STR_TIPTYPES_FLD_TYPENAME;
end;

procedure TTipTypeDataAdaptor.SelectAndOpenTipType(const aName: string; aReadOnly: boolean);
begin
    ASSERT(aName <> '', 'TipType name is empty');
    SelectAndOpen(Format(STR_SQL_SELECT_TIPTYPE_FMT, [aName]), aReadOnly);
end;

class function TTipTypeDataAdaptor.GetNameFromBasicTipType(aBasicTipType: TBasicTipType;
    const aDitiType: string): string;
begin
    case (aBasicTipType) of
        DispTip:
            begin
                result := STR_TIPTYPEENTRY_BTT_DITI;
                if (aDitiType <> '') then
                    result := result + aDitiType;
            end;
        DefaultTip:
            result := STR_TIPTYPEENTRY_BTT_DEFAULT;
        RediTip:
            result := STR_TIPTYPEENTRY_BTT_REDI;
        else
            result := '';
    end;
end;

class function TTipTypeDataAdaptor.GetBasicTipTypeFromName(const aBasicTipTypeName: string;
    out oDitiType: string): TBasicTipType;
begin
    result := NoTip;
    oDitiType := '';

    if (Pos(STR_TIPTYPEENTRY_BTT_DITI, Uppercase(aBasicTipTypeName)) = 1) then
    begin
        result := DispTip;
        oDitiType := Copy(aBasicTipTypeName, Length(STR_TIPTYPEENTRY_BTT_DITI) + 1,
            Length(aBasicTipTypeName));
    end;
    if (Uppercase(aBasicTipTypeName) = STR_TIPTYPEENTRY_BTT_DEFAULT) then
        result := DefaultTip;
    if (Uppercase(aBasicTipTypeName) = STR_TIPTYPEENTRY_BTT_REDI) then
        result := RediTip;
end;

class function TTipTypeDataAdaptor.GetBasicTipTypeFromName(const aBasicTipTypeName: string): TBasicTipType;
var
    xDitiType: string;
begin
    result := GetBasicTipTypeFromName(aBasicTipTypeName, xDitiType);
end;

procedure TTipTypeDataAdaptor.SelectAndOpenByTypeName(aTipTypeName: string; aReadOnly: boolean);
begin
    self.SelectAndOpen('SELECT * FROM ' + STR_TIPTYPES_TBL + ' WHERE ' + STR_TIPTYPES_FLD_TYPENAME + '=''' +
        aTipTypeName + '''', aReadOnly);
end;

class function TTipTypeDataAdaptor.ReadRecAtCursor(aDataset: TDataProvider; var vRec: TTipType): boolean;
var
    xDitiType: string;
begin
    result := not aDataset.Eof;

    // Parameter-Übergabe
    vRec.Name := aDataset.FieldByName(STR_TIPTYPES_FLD_TYPENAME).AsString;
    vRec.BasicType := GetBasicTipTypeFromName(aDataset.FieldByName(STR_TIPTYPES_FLD_BASICTIPTYPE).AsString,
        xDitiType);
    vRec.DitiType := xDitiType;
    vRec.ToolName := aDataset.FieldByName(STR_TIPTYPES_FLD_TOOLNAME).AsString;
    vRec.DeviceName := aDataset.FieldByName(STR_TIPTYPES_FLD_REDIDEVICENAME).AsString;
    vRec.DilutorName := aDataset.FieldByName(STR_TIPTYPES_FLD_DILUTORNAME).AsString;
    vRec.MaxVolume := aDataset.FieldByName(STR_TIPTYPES_FLD_MAXVOLUME).AsFloat;
    vRec.MinVolume := aDataset.FieldByName(STR_TIPTYPES_FLD_MINVOLUME).AsFloat;
    vRec.XOffset_mm := aDataset.FieldByName(STR_TIPTYPES_FLD_XOFFSET).AsFloat;
    vRec.YOffset_mm := aDataset.FieldByName(STR_TIPTYPES_FLD_YOFFSET).AsFloat;
    vRec.RelLength_mm := aDataset.FieldByName(STR_TIPTYPES_FLD_ZOFFSET).AsFloat;
    vRec.DTOffset_mm := aDataset.FieldByName(STR_TIPTYPES_FLD_ZOFFSETREMOVABLE).AsFloat;
    vRec.WashZOffset_mm := aDataset.FieldByName(STR_TIPTYPES_FLD_ZOFFSETWASH).AsFloat;
    vRec.WasteZOffset_mm := aDataset.FieldByName(STR_TIPTYPES_FLD_ZOFFSETWASTE).AsFloat;
    vRec.DryZOffset_mm := aDataset.FieldByName(STR_TIPTYPES_FLD_ZOFFSETDRY).AsFloat;
    vRec.DoNotDryAfterFlush := not aDataset.FieldByName(STR_TIPTYPES_FLD_DRYAFTERFLUSH).AsBoolean;
    vRec.MethodNameGetTip := aDataset.FieldByName(STR_TIPTYPES_FLD_METHODNAMEGETTIP).AsString;
    vRec.MethodNamePutTip := aDataset.FieldByName(STR_TIPTYPES_FLD_METHODNAMEPUTTIP).AsString;
    vRec.DoNotInit := aDataset.FieldByName(STR_TIPTYPES_FLD_DONOTINIT).AsBoolean;
end;

class procedure TTipTypeDataAdaptor.WriteRecAtCursor(aDataset: TDataProvider; const aRec: TTipType;
    aAppend: boolean);
begin
    ASSERT(aDataSet.Active, 'Dataset not active');
    if aAppend then
        aDataset.Append
    else
        aDataset.Edit;

    aDataset.FieldByName(STR_TIPTYPES_FLD_TYPENAME).AsString := aRec.Name;
    aDataset.FieldByName(STR_TIPTYPES_FLD_BASICTIPTYPE).AsString :=
        GetNameFromBasicTipType(aRec.BasicType, aRec.DitiType);
    aDataset.FieldByName(STR_TIPTYPES_FLD_TOOLNAME).AsString := aRec.ToolName;
    aDataset.FieldByName(STR_TIPTYPES_FLD_REDIDEVICENAME).AsString := aRec.DeviceName;
    aDataset.FieldByName(STR_TIPTYPES_FLD_DILUTORNAME).AsString := aRec.DilutorName;
    aDataset.FieldByName(STR_TIPTYPES_FLD_MAXVOLUME).AsFloat := aRec.MaxVolume;
    aDataset.FieldByName(STR_TIPTYPES_FLD_MINVOLUME).AsFloat := aRec.MinVolume;
    aDataset.FieldByName(STR_TIPTYPES_FLD_XOFFSET).AsFloat := aRec.XOffset_mm;
    aDataset.FieldByName(STR_TIPTYPES_FLD_YOFFSET).AsFloat := aRec.YOffset_mm;
    aDataset.FieldByName(STR_TIPTYPES_FLD_ZOFFSET).AsFloat := aRec.RelLength_mm;
    aDataset.FieldByName(STR_TIPTYPES_FLD_ZOFFSETREMOVABLE).AsFloat := aRec.DTOffset_mm;
    aDataset.FieldByName(STR_TIPTYPES_FLD_ZOFFSETWASH).AsFloat := aRec.WashZOffset_mm;
    aDataset.FieldByName(STR_TIPTYPES_FLD_ZOFFSETWASTE).AsFloat := aRec.WasteZOffset_mm;
    aDataset.FieldByName(STR_TIPTYPES_FLD_ZOFFSETDRY).AsFloat := aRec.DryZOffset_mm;
    aDataset.FieldByName(STR_TIPTYPES_FLD_DRYAFTERFLUSH).AsBoolean := not aRec.DoNotDryAfterFlush;
    aDataset.FieldByName(STR_TIPTYPES_FLD_METHODNAMEGETTIP).AsString := aRec.MethodNameGetTip;
    aDataset.FieldByName(STR_TIPTYPES_FLD_METHODNAMEPUTTIP).AsString := aRec.MethodNamePutTip;
    aDataset.FieldByName(STR_TIPTYPES_FLD_DONOTINIT).AsBoolean := aRec.DoNotInit;

    aDataset.Post;
end;

class function TTipTypeDataAdaptor.GetEmptyRec(): TTipType;
begin
    result.Name := '';
    result.BasicType := NoTip;
    result.DeviceName := '';
    result.ToolName := '';
    result.MaxVolume := 0;
    result.MinVolume := 0;
    result.RelLength_mm := 0;
    result.DTOffset_mm := 0;
    result.DilutorName := '';
    result.DitiType := '';
    result.XOffset_mm := 0;
    result.YOffset_mm := 0;
    result.WashZOffset_mm := 0;
    result.WasteZOffset_mm := 0;
    result.DryZOffset_mm := 0;
    result.DoNotDryAfterFlush := false;
    result.MethodNameGetTip := '';
    result.MethodNamePutTip := '';
    result.DoNotInit := false;
end;

procedure TTipTypeDataAdaptor.WriteTipType(const aRec: TTipType);
var
    xAppend: boolean;
begin
    self.SelectAndOpenTipType(aRec.Name, false);
    try
        xAppend := self.DataProvider.IsEmpty;
        self.WriteRecAtCursor(self.DataProvider, aRec, xAppend);
    finally
        self.Close();
    end;
end;

function TTipTypeDataAdaptor.ReadTipType(const aName: string; var vRec: TTipType): boolean;
begin
    SelectAndOpenTipType(aName, true);
    try
        result := not self.DataProvider.IsEmpty();
        if not result then
            EXIT;
        ReadRecAtCursor(self.DataProvider, vRec);
    finally
        Close();
    end;
end;

class function TTipTypeDataAdaptor.TipTypeIsRedi(aTipType: TBasicTipType): boolean;
begin
    result := (aTipType in [RediTip, VarRediTip]);
end;

class function TTipTypeDataAdaptor.GetTipTypeFromName(const aTypeName: string): TBasicTipType;
var
    xTypeData: TTipType;
begin
    result := NoTip;
    if TTipTypeDataAdaptor.TipTypeExists(aTypeName, xTypeData) then
        result := xTypeData.BasicType;
end;

class function TTipTypeDataAdaptor.TipNameIsRedi(const aTypeName: string): boolean;
begin
    result := TipTypeIsRedi(GetTipTypeFromName(aTypeName));
end;

function TTipTypeDataAdaptor.GetLiquidTipTypeList(): TStringArray;
var
    xList: TList<string>;
begin
    xList := TList<string>.Create();
    try
        self.SelectAndOpenAll(true);
        while not self.DataProvider.EOF do
        begin
            if not TipTypeIsRedi
                (GetBasicTipTypeFromName(self.DataProvider.FieldByName(STR_TIPTYPES_FLD_BASICTIPTYPE)
                .AsString)) then
                xList.Add(self.DataProvider.Fields[0].AsString);
            self.DataProvider.Next;
        end;

        result := xList.ToArray;
    finally
        FreeAndNil(xList);
    end;
end;

function TTipTypeDataAdaptor.GetPowderTipTypeList(): TStringArray;
var
    xList: TList<string>;
begin
    xList := TList<string>.Create();
    try

        self.SelectAndOpenAll(true);
        while not self.DataProvider.EOF do
        begin
            if TipTypeIsRedi
                (GetBasicTipTypeFromName(self.DataProvider.FieldByName(STR_TIPTYPES_FLD_BASICTIPTYPE)
                .AsString)) then
                xList.Add(self.DataProvider.Fields[0].AsString);
            self.DataProvider.Next;
        end;

        result := xList.ToArray;
    finally
        FreeAndNil(xList);
    end;
end;

function TTipTypeDataAdaptor.ReadDiTiTipTypeNames: TStringArray;
var
    xTipTypeRec: TTipType;
    xList: TList<string>;
begin
    xList := TList<string>.Create();
    try
        self.SelectAndOpenAll(true);
        while not self.DataProvider.EOF do
        begin
            self.ReadRecAtCursor(self.DataProvider, xTipTypeRec);
            if (xTipTypeRec.BasicType = DispTip) then
                xList.Add(xTipTypeRec.DitiType);
            self.DataProvider.Next;
        end;

        result := xList.ToArray;
    finally
        FreeAndNil(xList);
    end;
end;

function TTipTypeDataAdaptor.GetRediDeviceList(): TStringArray;
var
    xTipTypeRec: TTipType;
    xList: TList<string>;
begin
    xList := TList<string>.Create();
    try
        self.SelectAndOpenAll(true);
        while not self.DataProvider.EOF do
        begin
            self.ReadRecAtCursor(self.DataProvider, xTipTypeRec);
            if TipTypeIsRedi(xTipTypeRec.BasicType) then
                xList.Add(xTipTypeRec.DeviceName);
            self.DataProvider.Next;
        end;
        result := xList.ToArray;
    finally
        FreeAndNil(xList);
    end;
end;

class function TTipTypeDataAdaptor.TipTypeExists(aTypeName: string; out oTypeData: TTipType): boolean;
var
    xDataAdaptor: TTipTypeDataAdaptor;
begin
    oTypeData := TTipTypeDataAdaptor.GetEmptyRec();

    xDataAdaptor := TTipTypeDataAdaptor.Create();
    try
        xDataAdaptor.SelectAndOpenByTypeName(aTypeName, true);
        result := xDataAdaptor.ReadRecAtCursor(xDataAdaptor.DataProvider, oTypeData);
        xDataAdaptor.Close;
    finally
        FreeAndNil(xDataAdaptor);
    end;
end;

class function TTipTypeDataAdaptor.InstGetLiquidTipTypeNames(): TStringArray;
var
    xDataAdaptor: TTipTypeDataAdaptor;
begin
    xDataAdaptor := TTipTypeDataAdaptor.Create();
    try
        result := xDataAdaptor.GetLiquidTipTypeList();
    finally
        FreeAndNil(xDataAdaptor);
    end;
end;

class function TTipTypeDataAdaptor.InstGetPowderTipTypeNames(): TStringArray;
var
    xDataAdaptor: TTipTypeDataAdaptor;
begin
    xDataAdaptor := TTipTypeDataAdaptor.Create();
    try
        result := xDataAdaptor.GetPowderTipTypeList();
    finally
        FreeAndNil(xDataAdaptor);
    end;
end;

class function TTipTypeDataAdaptor.InstReadAllNames(): TStringArray;
var
    xDataAdaptor: TTipTypeDataAdaptor;
begin
    xDataAdaptor := TTipTypeDataAdaptor.Create();
    try
        result := xDataAdaptor.ReadAllNames();
    finally
        FreeAndNil(xDataAdaptor);
    end;
end;


end.
