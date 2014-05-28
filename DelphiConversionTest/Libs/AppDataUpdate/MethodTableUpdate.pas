{ --------------------------------------------------------------------------------------------------
  Copyright © 2008 Zinsser Analytic GmbH - All rights reserved.
  Author       : Wolfgang Lyncke (wl)
  Description  :
  --------------------------------------------------------------------------------------------------
  Revision History:
  date     op  method                       track-no  improvement/change
  -------- --  ---------------------------  --------  ----------------------------------------------
  07.01.08 wl                               TN3972    Felder radikal zusammengestrichen
  24.06.08 pk                               TN4148    FieldNames unit removed
  01.07.08 wl  TMethodTableStructDefV3      TN4127    'INACTIVE' heißt jetzt 'LINEDISABLED'
  03.07.08 wl  TCommonGUIParser             TN4157    Tool to parse options
  03.07.08 wl  TMethodTableUpdateV3         TN4157    inserts USEDPIPDEVICE Key into options
  20.09.08 pk  TMethodTableUpdateV3_1       TN4215    New Update RUN Action to RUNST
  25.09.08 wl  TMethodTableStructDefV4      TN4242.1  MethodName hat jetzt 50 chars
  25.09.08 wl  TMethodTableUpdateV4.ReplaceDllCallEventParams      TN4242  Ändert die Event-Parameter (immer als RUNST)
  25.09.08 wl  TMethodTableUpdateV4.ChangeCommaActionToRunst       TN4242  Ändert COMMA actions in RUNST actions
  25.09.08 wl  TMethodTableUpdateV4.AddCommandMacrosAsDummyMethods TN4242  Macht aus Command macros Untermethoden
  07.10.08 pk  TMethodTableUpdateV4.ReplaceDllCallEventParams      TN4242  Bug fixed
  02.12.08 pk  TMethodTableUpdateV4_1                              TN4337  Convert RUNST to ADDM
  19.02.09 pk  TMethodTableUpdateV4_2                              TN4232  Convert Dataxxx Calc lines to Dataset Actions
  09.06.09 pk  TMethodTableUpdateV2                                TN3979  New
  31.05.10 wl  TMethodTableUpdateV4_2                              TN5120  hebt die Beschränkung der Action-Namen auf und ändert Name der TraySyXPipet Action
  22.06.10 pk  TMethodTableUpdateV4_4                              TN5088  Convert CALC to STORE, WHILE, IF, etc
  29.09.10 pk  TMethodTableUpdateV4_5                              TN5283  Short and Long method step comments combined
  29.09.10 pk  TMethodTableUpdateV4_4                              TN5088  Bug fixed
  30.09.10 pk  TMethodTableUpdateV4_6                              TN5287  Remark Text Property now has a key
  15.02.11 pk                               	     		          TN4780   changes needed to make UpdateManager compatible with TurboDB
  19.02.11 wl  TMethodTableUpdateV2.ChangeOldStyleOptions          TN5480    initial revision
  21.02.11 wl  TMethodTableUpdateV2.ChangeOldStyleOptions          TN5455   neu: Port-Actions
  21.02.11 wl  TMethodTableUpdateV2_1                              TN5455   trägt bei Flush-Action das Device ein
  09.06.11 wl  TMethodTableUpdateV2.UpdateSeqField                 TN5597   Falls eine Methode mehrere gleiche SEQ-Nummern hat, kommt es nicht mehr zu Key Violation
  09.06.11 wl  TMethodTableUpdateV4_4.ConvertCalcToStore           TN5596   Trim() eingefügt
  30.06.11 wl  TMethodTableUpdateV4_7                              TN5618   STORE-Action: ArrayIndex als Parameter fällt weg; neue Schreibweise: Variable[ArrayIndex]
  29.11.11 wl  TMethodTableUpdateV4_5.IsOddComment                 TN5753   entfernt die geistreichen Autokommentare
  29.11.11 wl  TMethodTableUpdateV4_4                              TN5753   IF,WHILE,.. werden jetzt auch konvertiert, wenn es nicht groß geschrieben ist
  29.11.11 wl  TMethodTableUpdateV2.MethodUpdateV2CustomFunc       TN5753   DILUENT wird nur bei PIPET und FLUSH eingetragen
  29.11.11 wl  TMethodTableUpdateV4_6.MethodUpdateCustomFunc       TN5753   REMAR wird durch BLANK ersetzt, wenn es keinen Unterstrich enthält
  19.12.11 wl  TMethodTableUpdateV2                                TN5771   liest auch Import-Optionen
  19.12.11 wl  TMethodTableUpdateV4_3                              TN5771   erweitert um Import-Parameter-Konvertierung
  14.02.12 wl  TMethodTableUpdateV2                                TN5443   Bei ADDM wird AddMethodActionOptionsFromStrToStr aufgerufen
  30.09.13 wl  TMethodTableUpdateV4_8                              TN6260   eventuell vorhandene Volumen löschen, wenn Cycles > 0
  01.10.13 wl  TMethodTableUpdateV4_5.IsOddCommentShort/Long       TN6265   Jetzt werden auch deutsche "Odd-Kommentare" ausgefiltert
  21.10.13 wl  TMethodTableUpdateV4_9                              TN6276   Geänderte Multi-Pipetting-Parameter
  -------------------------------------------------------------------------------------------------- }

unit MethodTableUpdate;


interface


uses
    Generics.Collections,
    Classes,
    Update,
    TableStructDef,
    TableUpdate,
    SettingsTableUpdate,
    TableChangeAdaptor;

const
    cMethodNamePrefixExCommand = 'CMD__';
    cMethodNamePrefixExDllCall = 'DLL__';

type
    TStringArray = array of string;

    // Tool to parse options
    TCommonGUIParser = class
    strict private
        class procedure CommonOptionsStrToList(aList: TStrings; aStr: string);
        class procedure OptionsStrToList(aList: TStrings; aStr: string);
        class function ListToOptionStr(aList: TStrings): string;
        class function ValueContainsDelimiter(aValue: string): boolean;
        class function ConvertValueToSublistValue(aValue: string): string;
        class function ConcatOptions(const aPart1, aPart2: string): string;
    public
        class function SplitStr(const aOriginal: string; const aDelimiter: string; out oPart1, oPart2: string;
            aMatchCase: boolean): boolean;
        class procedure CommentsFromStr(const aCommentString: string;
            out oCommentShort, oCommentLong: string);
        class function FindValuesForKeysInOptions(aOptions: string; out oValues: TStringArray;
            const aKeys: array of string): boolean;
        class function AddKeysAndValuesToOptions(aOptions: string;
            const aKeys, aValues: array of string): string;
    end;

    // table structure definitions
    TMethodTableStructDefV0 = class(TTableStructDef)
    protected
        procedure DoDefineStruct(); override;
    end;

    TMethodTableStructDefV1 = class(TMethodTableStructDefV0)
    protected
        procedure DoDefineStruct(); override;
    end;

    TMethodTableStructDefV2 = class(TMethodTableStructDefV1)
    protected
        procedure DoDefineStruct(); override;
    end;

    TMethodTableStructDefV3 = class(TMethodTableStructDefV2)
    protected
        procedure DoDefineStruct(); override;
    end;

    TMethodTableStructDefV4 = class(TMethodTableStructDefV3)
    protected
        procedure DoDefineStruct(); override;
    end;

    // table updates
    TMethodTableUpdateV1 = class(TTableUpdate)
    private
        class procedure UpdateSeqField(const aSourceVal: variant; var vDestVal: variant);
        class function FormatSeq(aSeqUnformatted: string): string;
        class function FormStr(Str: string; FillCh: Char; Len: Byte; LeftRight: Word): string;
    public
        constructor Create(aUpdateNumber: TUpdateID);
    end;

    TMethodTableUpdateV2 = class(TTableUpdate)
    private
        fAutoSeq: integer;
        procedure UpdateSeqField(const aSourceVal: variant; var vDestVal: variant);
        procedure AddStringToOptions(var vOptionsText: string; const aKey, aValue: string);
        procedure ChangeOldStyleActionName(var vActionName: string; const aDilRack: string);
        function ChangeOldStyleOptions(var vActionName: string; const aOptions: string): string;
        procedure MethodUpdateV2CustomFunc(aSender: TObject);
    public
        constructor Create(aUpdateNumber: TUpdateID);
    end;

    TMethodTableUpdateV2_1 = class(TTableUpdate)
    private
        function GetFirstPipDevice(): string;
        procedure AddPipDeviceToFlushOptions(var vOptions: string; const aPipDevice: string);
        procedure MethodUpdateV2_1CustomFunc(aSender: TObject);
    public
        constructor Create(aUpdateNumber: TUpdateID);
    end;

    TMethodTableUpdateV3 = class(TTableUpdate)
    private
        procedure MethodUpdateV3CustomFunc(aSender: TObject);
        procedure ReplaceTotalTipMap(aPipDevLoader: TSettingsTablePipDeviceLoader);
    public
        constructor Create(aUpdateNumber: TUpdateID);
    end;

    TMethodTableUpdateV3_1 = class(TTableUpdate)
    private
        procedure MethodUpdateV3_1CustomFunc(aSender: TObject);
    protected
        function GetUpdateDescription: string; override;
    public
        constructor Create(aUpdateNumber: TUpdateID);
    end;

    TMethodTableUpdateV4 = class(TTableUpdate)
    private
        procedure AddMethodLine(const aMethodName, aAction, aOptions: string; aSeq: integer);
        procedure AddCommandMacrosAsDummyMethods();
        procedure ReplaceDllCallEventParams();
        procedure ChangeCommaActionToRunst();
        procedure MethodUpdateV4CustomFunc(aSender: TObject);
    protected
        function GetUpdateDescription: string; override;
    public
        constructor Create(aUpdateNumber: TUpdateID);
    end;

    TV4MethodLine = class
    public
        name: string;
        Seq: integer;
        Action: string;
        Options: string;
        Comment: string;
        LineDisabled: boolean;
    end;

    TV4MethodUtils = class
    public
        class procedure ReadMethodLines(aAdaptor: TTableChangeAdaptor; const aMethodName: string;
            const aList: TObjectList<TV4MethodLine>);
        class procedure WriteMethodLines(aAdaptor: TTableChangeAdaptor; const aMethodName: string;
            const aList: TObjectList<TV4MethodLine>);
        class procedure GetMethodNames(aAdaptor: TTableChangeAdaptor; const aSQLWhere: string;
            aList: TStringList);
    end;

    TMethodTableUpdateV4_1 = class(TTableUpdate)
    private
        procedure MethodUpdateV4_1CustomFunc(aSender: TObject);
        function BeginsWithText(const aText, aSubText: string): boolean;
        function FindRepeatCondition(const aSubMethodName: string): string;
    protected
        function GetUpdateDescription: string; override;
    public
        constructor Create(aUpdateNumber: TUpdateID);
    end;

    TMethodTableUpdateV4_2 = class(TTableUpdate)
    private
        function FindMatchingEndWhileIndex(const aMethLines: TObjectList<TV4MethodLine>;
            const aCurIndex: integer): integer;
        procedure ParseParams(const aText: string; const aDelimiter: string; var vValues: TStringArray);
        function ParseObjectIdentName(const aOptionsText: string; out oObjectIdentName: string): boolean;
        function FindCalcFuncBeginAndEndPos(const aFuncName, aOptionsText: string;
            aIsPosBeginAtOpenBracket: boolean; out oPosBegin, oPosEnd: integer): boolean;
        procedure ParseCalcFuncParams(const aFuncName, aOptionsText: string; var vValues: TStringArray);
        function CreateDSOpenLine(const aObjectIdentName, aDefName, aFilter, aRecCountIdentName: string)
            : TV4MethodLine;
        function CreateDSReadLine(const aObjectValue: string): TV4MethodLine;
        function CreateDSCursorMoveLine(const aObjectValue, aIsRelative, aMoveOffset,
            aRecNumIdentName: string): TV4MethodLine;
        function CreateDSCloseLine(const aObjectValue: string): TV4MethodLine;
        procedure ConvertDataOpenLine(const aMethLines: TObjectList<TV4MethodLine>; var vCurIndex: integer;
            var vMethodChanged: boolean);
        procedure ConvertDataReadLine(const aMethLines: TObjectList<TV4MethodLine>; var vCurIndex: integer;
            var vMethodChanged: boolean);
        procedure ConvertDataCursorLine(const aMethLines: TObjectList<TV4MethodLine>; var vCurIndex: integer;
            var vMethodChanged: boolean);
        procedure ConvertDataLoopLines(const aMethLines: TObjectList<TV4MethodLine>; var vCurIndex: integer;
            var vMethodChanged: boolean);
        procedure MethodUpdateV4_2CustomFunc(aSender: TObject);
    protected
        function GetUpdateDescription: string; override;
    public
        constructor Create(aUpdateNumber: TUpdateID);
    end;

    TMethodTableUpdateV4_3 = class(TTableUpdate)
    private
        function GetVarNames(const aImpDefName: string): TArray<string>;
        procedure MethodUpdateV4_3CustomFunc1;
        procedure MethodUpdateV4_3CustomFunc2;
        procedure MethodUpdateV4_3CustomFunc(aSender: TObject);
    protected
        function GetUpdateDescription: string; override;
    public
        constructor Create(aUpdateNumber: TUpdateID);
    end;

    TMethodTableUpdateV4_4 = class(TTableUpdate)
    private
        procedure MethodUpdateV4_4CustomFunc(aSender: TObject);
        function ConvertCalcToStore(const aOldOptions: string; out oNewOptions: string;
            out oNewAction: string): boolean;
        function ConvertCalcToEndWhile(const aOldOptions: string; out oNewOptions: string;
            out oNewAction: string): boolean;
        function ConvertCalcToEndIf(const aOldOptions: string; out oNewOptions: string;
            out oNewAction: string): boolean;
        function ConvertCalcToWhile(const aOldOptions: string; out oNewOptions: string;
            out oNewAction: string): boolean;
        function ConvertCalcToIf(const aOldOptions: string; out oNewOptions: string;
            out oNewAction: string): boolean;
        procedure ConvertCalcToRemark(const aOldOptions: string; out oNewOptions: string;
            out oNewAction: string);
        procedure ConvertCalcAction(const aOldOptions: string; out oNewOptions: string;
            out oNewAction: string);
    public
        constructor Create(aUpdateNumber: TUpdateID);
    end;

    TMethodTableUpdateV4_5 = class(TTableUpdate)
    private
        function IsOddCommentLong(const aComment: string): boolean;
        function IsOddCommentShort(const aComment: string): boolean;
        procedure MethodUpdateV4_5CustomFunc(aSender: TObject);
    public
        constructor Create(aUpdateNumber: TUpdateID);
    end;

    TMethodTableUpdateV4_6 = class(TTableUpdate)
    private
        procedure MethodUpdateCustomFunc(aSender: TObject);
    public
        constructor Create(aUpdateNumber: TUpdateID);
    end;

    TMethodTableUpdateV4_7 = class(TTableUpdate)
    private
        procedure MethodUpdateCustomFunc(aSender: TObject);
    public
        constructor Create(aUpdateNumber: TUpdateID);
    end;

    TMethodTableUpdateV4_8 = class(TTableUpdate)
    private
        procedure MethodUpdateCustomFunc(aSender: TObject);
    public
        constructor Create(aUpdateNumber: TUpdateID);
    end;

    TMethodTableUpdateV4_9 = class(TTableUpdate)
    private
        procedure MethodUpdateCustomFunc(aSender: TObject);
    public
        constructor Create(aUpdateNumber: TUpdateID);
    end;


implementation


uses
    SysUtils,
    StrUtils,
    DataProvider,
    MethodGUIParserConverter;

const
    INT_REVISION_1 = 1;
    INT_REVISION_2 = 2;
    INT_REVISION_3 = 3;
    INT_REVISION_4 = 4;
    INT_MINORREVISION_1 = 1;
    INT_MINORREVISION_2 = 2;
    INT_MINORREVISION_3 = 3;
    INT_MINORREVISION_4 = 4;
    INT_MINORREVISION_5 = 5;
    INT_MINORREVISION_6 = 6;
    INT_MINORREVISION_7 = 7;
    INT_MINORREVISION_8 = 8;
    INT_MINORREVISION_9 = 9;

    STR_METHOD_V0_TBL = 'METHOD';

    STR_METHOD_V0_FLD_NAME = 'NAME';
    STR_METHOD_V0_FLD_LAYOUT = 'LAYOUT';
    STR_METHOD_V0_FLD_SEQ = 'SEQ';
    STR_METHOD_V0_FLD_DILUENT = 'DILUENT';
    STR_METHOD_V0_FLD_SOURCERACK = 'SOURCERACK';
    STR_METHOD_V0_FLD_DESTRACK = 'DESTRACK';
    STR_METHOD_V0_FLD_SOURCEFIRSTPOS = 'SOURCEFIRSTPOS';
    STR_METHOD_V0_FLD_SOURCELASTPOS = 'SOURCELASTPOS';
    STR_METHOD_V0_FLD_DESTFIRSTPOS = 'DESTFIRSTPOS';
    STR_METHOD_V0_FLD_DESTLASTPOS = 'DESTLASTPOS';
    STR_METHOD_V0_FLD_DESTVOL = 'DESTVOL';
    STR_METHOD_V0_FLD_DILVOL = 'DILVOL';
    STR_METHOD_V0_FLD_DILRACK = 'DILRACK';
    STR_METHOD_V0_FLD_DILPOS = 'DILPOS';
    STR_METHOD_V0_FLD_LIQPARA = 'LIQPARA';
    STR_METHOD_V0_FLD_ACTION = 'ACTION';
    STR_METHOD_V0_FLD_REMARK = 'REMARK';
    STR_METHOD_V0_FLD_REMARKEXTEND = 'REMARKEXTEND';
    STR_METHOD_V0_FLD_SCHEDMIN = 'SCHEDMIN';
    STR_METHOD_V0_FLD_SCHEDMAX = 'SCHEDMAX';
    STR_METHOD_V0_FLD_SCHEDSHAREDID = 'SCHEDSHAREDID';
    STR_METHOD_V0_FLD_PRIORITY = 'PRIORITY';
    STR_METHOD_V0_FLD_RESID = 'RESID';
    STR_METHOD_V0_FLD_ITERATE = 'ITERATE';
    STR_METHOD_V0_FLD_COMMENT = 'COMMENT';

    INT_METHOD_V0_FLDLEN_NAME = 20;
    INT_METHOD_V0_FLDLEN_ACTION = 20;

    STR_METHOD_V3_FLD_AOPTIONS = 'AOPTIONS';
    STR_METHOD_V3_FLD_LINEDISABLED = 'LINEDISABLED';
    STR_METHOD_V3_INDEX_FIELDS = STR_METHOD_V0_FLD_NAME + ';' + STR_METHOD_V0_FLD_SEQ;

    cV0_ActionNameCalc = 'CALC';
    cV0_ActionNameRunst = 'RUNST';
    cV0_ActionNameRun = 'RUN';
    cV0_ACtionNameAddM = 'ADDM';

    cV0_SelectALL = 'SELECT * FROM Method';
    cV0_SelectMethod = 'SELECT * FROM Method WHERE Name = ''%s''';
    cV0_DeleteMethod = 'DELETE FROM Method WHERE Name = ''%s''';

    cV3_OptionKeyRunstName = cV0_ActionNameRunst + 'NAME';
    cV3_OptionKeyRunstParams = cV0_ActionNameRunst + 'PARAMS';
    cV3_OptionKeyRunstRepeat = cV0_ActionNameRunst + 'REPEAT';

    cV0_OptionKeyAddMName = cV0_ACtionNameAddM + 'NAME';
    cV0_OptionKeyAddMParams = cV0_ACtionNameAddM + 'PARAMS';

    STR_COMMENT_IDENT_LONG = 'CMTLONG';
    STR_COMMENT_IDENT_SHORT = 'CMTSHORT';

    { TMethodTableStructDefV0 }

procedure TMethodTableStructDefV0.DoDefineStruct;
begin
    inherited;
    fName := 'METHOD';
end;

{ TMethodTableStructDefV1 }

procedure TMethodTableStructDefV1.DoDefineStruct();
begin
    inherited;
    self.AddField('NAME', tftString, 20);
    self.AddField('LAYOUT', tftString, 20);
    self.AddField('SEQ', tftString, 10);
    self.AddField('DILUENT', tftString, 10);
    self.AddField('SOURCERACK', tftString, 30);
    self.AddField('DESTRACK', tftString, 30);
    self.AddField('SOURCEFIRSTPOS', tftString, 8);
    self.AddField('SOURCELASTPOS', tftString, 8);
    self.AddField('DESTFIRSTPOS', tftString, 8);
    self.AddField('DESTLASTPOS', tftString, 8);
    self.AddField('DESTVOL', tftString, 10);
    self.AddField('DILVOL', tftString, 10);
    self.AddField('DILRACK', tftString, 30);
    self.AddField('DILPOS', tftString, 8);
    self.AddField('LIQPARA', tftString, 20);
    self.AddField('ACTION', tftString, 20);
    self.AddField('REMARK', tftString, 255);
    self.AddField('REMARKEXTEND', tftMemo, 0);
    self.AddField('SCHEDMIN', tftString, 10);
    self.AddField('SCHEDMAX', tftString, 10);
    self.AddField('PRIORITY', tftString, 10);
    self.AddField('RESID', tftString, 10);
    self.AddField('SCHEDSHAREDID', tftString, 10);
    self.AddField('ITERATE', tftString, 10);
    self.AddField('COMMENT', tftMemo, 0);
    self.AddIndex('NAME;LAYOUT;SEQ;DILUENT;SOURCERACK;DESTRACK;SOURCEFIRSTPOS;SOURCELASTPOS');
end;

constructor TMethodTableUpdateV1.Create(aUpdateNumber: TUpdateID);
begin
    inherited Create(aUpdateNumber, TMethodTableStructDefV0, INT_REVISION_1);
    AlterStructure(TMethodTableStructDefV1);
    CopyMatchingFields([]);
    self.CopyFieldWithFunc('SEQ', 'SEQ', UpdateSeqField);
end;

class function TMethodTableUpdateV1.FormStr(Str: string; FillCh: Char; Len: Byte; LeftRight: Word): string;
var
    i: integer;
begin
    if LeftRight = 0 then
        for i := Length(Str) to Len do
            Str := Str + FillCh;
    if LeftRight = 1 then
        for i := Length(Str) to Len - 1 do
            Str := FillCh + Str;
    FormStr := COPY(Str, 1, Len);
end;

class function TMethodTableUpdateV1.FormatSeq(aSeqUnformatted: string): string;
begin
    result := FormStr(aSeqUnformatted, '0', 7, 1);
end;

class procedure TMethodTableUpdateV1.UpdateSeqField(const aSourceVal: variant; var vDestVal: variant);
begin
    vDestVal := FormatSeq(aSourceVal);
end;

procedure TMethodTableStructDefV2.DoDefineStruct();
begin
    inherited;
    self.DelField('LAYOUT');
    self.DelField('SEQ');
    self.DelField('DILUENT');
    self.DelField('SOURCERACK');
    self.DelField('DESTRACK');
    self.DelField('SOURCEFIRSTPOS');
    self.DelField('SOURCELASTPOS');
    self.DelField('DESTFIRSTPOS');
    self.DelField('DESTLASTPOS');
    self.DelField('DESTVOL');
    self.DelField('DILVOL');
    self.DelField('DILRACK');
    self.DelField('DILPOS');
    self.DelField('LIQPARA');
    self.DelField('REMARK');
    self.DelField('REMARKEXTEND');
    self.DelField('SCHEDMIN');
    self.DelField('SCHEDMAX');
    self.DelField('PRIORITY');
    self.DelField('RESID');
    self.DelField('SCHEDSHAREDID');
    self.DelField('ITERATE');

    self.AddFieldAt('SEQ', tftInteger, 0, 1);
    self.AddFieldAt('AOPTIONS', tftMemo, 240, 3);

    self.AddIndex('NAME;SEQ');
end;

{ TMethodTableStructDefV3 }

procedure TMethodTableStructDefV3.DoDefineStruct();
begin
    inherited;
    self.AddField('LINEDISABLED', tftBoolean, 0);
end;

{ TMethodTableStructDefV4 }

procedure TMethodTableStructDefV4.DoDefineStruct;
begin
    inherited;
    self.ResizeField('NAME', 50);
end;

{ TCommonGUIParser }

class procedure TCommonGUIParser.OptionsStrToList(aList: TStrings; aStr: string);
begin
    CommonOptionsStrToList(aList, aStr);
end;

class function TCommonGUIParser.SplitStr(const aOriginal, aDelimiter: string; out oPart1, oPart2: string;
    aMatchCase: boolean): boolean;
var
    xPos: integer;
begin
    oPart1 := '';
    oPart2 := '';

    if aMatchCase then
        xPos := Pos(aDelimiter, aOriginal)
    else
        xPos := Pos(UpperCase(aDelimiter), UpperCase(aOriginal));

    result := xPos > 0;
    if not result then
        EXIT;
    oPart1 := Copy(aOriginal, 1, xPos - 1);
    oPart2 := Copy(aOriginal, xPos + Length(aDelimiter), Length(aOriginal));

end;

const
    STR_DELIM_KEYVALUE = '=';
    STR_DELIM_SEPARATOR = ',';
    STR_DELIM_SUBLIST_BEGIN = '{';
    STR_DELIM_SUBLIST_END = '}';

class procedure TCommonGUIParser.CommonOptionsStrToList(aList: TStrings; aStr: string);
var
    i: integer;
    xKey: string;
    xValue: string;
    xSubListFound: integer;
    xChar, xNextChar: char;
    xLen: integer;
    xAccum: string;
    xKeylessValueCount: integer;

    procedure AddValue();
    begin
        xValue := xAccum;
        xAccum := '';
        // if xKey = '' then
        // raise Exception.CreateFmt( 'Value %s is not associated with a proper key', [xValue] );
        if xKey = '' then
        begin
            Inc(xKeylessValueCount);
            xKey := IntToStr(xKeylessValueCount); // create a unique, fake key
        end;
        aList.Values[xKey] := xValue;
        xKey := '';
    end;

begin

    xSubListFound := 0;
    xKeylessValueCount := 0;
    i := 0;
    xLen := Length(aStr);
    if xLen = 0 then
        EXIT;

    while true do
    begin
        Inc(i);
        if i > xLen then
            Break;
        xChar := aStr[i];
        xNextChar := #0;
        if i + 1 <= xLen then
            xNextChar := aStr[i + 1];
        if (xChar = STR_DELIM_SUBLIST_BEGIN) then
        begin
            if (xSubListFound > 0) then
                xAccum := xAccum + xChar;
            inc(xSubListFound);
        end
        else if (xChar = STR_DELIM_SUBLIST_END) then
        begin
            if (xSubListFound = 0) then
                raise Exception.CreateFmt('Close braket without open bracket %s', [aStr]);
            dec(xSubListFound);
            if (xSubListFound > 0) then
                xAccum := xAccum + xChar;
        end
        else if (xSubListFound > 0) then
        begin
            xAccum := xAccum + xChar;
        end
        else if (xChar = STR_DELIM_SEPARATOR) then
        begin
            AddValue();
        end
        // avoid cases of : <=, >=, ==
        else if (xNextChar = STR_DELIM_KEYVALUE) and
            ((xChar = STR_DELIM_KEYVALUE) or (xChar = ':') or (xChar = '<') or (xChar = '>')) then
        begin
            xAccum := xAccum + xChar + xNextChar;
            Inc(i);
        end
        else if xChar = STR_DELIM_KEYVALUE then
        begin
            xKey := xAccum;
            xAccum := '';
        end
        else
        begin
            xAccum := xAccum + xChar;
        end;

        if i = xLen then
        begin
            AddValue();
            BREAK;
        end
    end;

    if (xSubListFound > 0) then
        raise Exception.CreateFmt('Open bracket without close bracket %s', [aStr]);
end;

class function TCommonGUIParser.FindValuesForKeysInOptions(aOptions: string; out oValues: TStringArray;
    const aKeys: array of string): boolean;
var
    xList: TStrings;
    i: integer;
    xKey, xValue: string;
begin
    SetLength(oValues, high(aKeys) + 1);

    result := false;
    xList := TStringList.Create;
    try
        OptionsStrToList(xList, aOptions);
        for i := 0 to high(aKeys) do
        begin
            xValue := '';
            xKey := aKeys[i];
            if (xList.IndexOfName(xKey) > -1) then
                xValue := xList.Values[xKey];
            if (xValue <> '') then
                result := true; // result = true: mindestens ein Parameter ist gesetzt
            oValues[i] := xValue;
        end;
    finally
        xList.Free;
    end;
end;

class function TCommonGUIParser.ValueContainsDelimiter(aValue: string): boolean;
var
    i: integer;
    xSubListFound: integer;
    xChar, xNextChar: char;
    xLen: integer;
begin
    result := false;
    xSubListFound := 0;
    i := 0;
    xLen := Length(aValue);
    if xLen = 0 then
        EXIT;

    while true do
    begin
        Inc(i);
        if i > xLen then
            BREAK;
        xChar := aValue[i];
        xNextChar := #0;
        if i + 1 <= xLen then
            xNextChar := aValue[i + 1];

        if (xChar = STR_DELIM_SUBLIST_BEGIN) then
            inc(xSubListFound)
        else if (xChar = STR_DELIM_SUBLIST_END) then
        begin
            if (xSubListFound = 0) then
                raise Exception.CreateFmt('Close braket without open bracket %s', [aValue]);
            dec(xSubListFound)
        end
        else if (xSubListFound > 0) then
        begin
            // nicht innerhalb der Klammern testen!
        end
        // avoid cases of : <=, >=, ==
        else if (xNextChar = STR_DELIM_KEYVALUE) and
            ((xChar = STR_DELIM_KEYVALUE) or (xChar = ':') or (xChar = '<') or (xChar = '>')) then
        begin
            Inc(i);
        end
        else if (xChar = STR_DELIM_SEPARATOR) then
        begin
            result := true;
            BREAK;
        end
        else if (xChar = STR_DELIM_KEYVALUE) then
        begin
            result := true;
            BREAK;
        end;

    end;

    if (xSubListFound > 0) then
        raise Exception.CreateFmt('Open bracket without close bracket %s', [aValue]);
end;

class function TCommonGUIParser.ConcatOptions(const aPart1, aPart2: string): string;
begin
    if aPart1 = '' then
        result := aPart2
    else if aPart2 = '' then
        result := aPart1
    else
        result := aPart1 + STR_DELIM_SEPARATOR + aPart2;
end;

class function TCommonGUIParser.ListToOptionStr(aList: TStrings): string;
var
    i: integer;
    xKey, xValue: string;
    xKeyAndDelim: string;
    xNewOption: string;
    xDummy: integer;
begin
    result := '';
    for i := 0 to aList.Count - 1 do
    begin
        xKey := aList.Names[i];
        // if an integer then it was a Keyless Value so omit the key and the keyvalue delimiter
        if TryStrToInt(xKey, xDummy) then
            xKeyAndDelim := ''
        else
            xKeyAndDelim := xKey + STR_DELIM_KEYVALUE;
        xValue := aList.Values[xKey];
        if ValueContainsDelimiter(xValue) then
            xValue := ConvertValueToSublistValue(xValue);
        xNewOption := xKeyAndDelim + xValue;
        result := ConcatOptions(result, xNewOption);
    end;
end;

class function TCommonGUIParser.ConvertValueToSublistValue(aValue: string): string;
begin
    result := STR_DELIM_SUBLIST_BEGIN + aValue + STR_DELIM_SUBLIST_END;
end;

class function TCommonGUIParser.AddKeysAndValuesToOptions(aOptions: string;
    const aKeys, aValues: array of string): string;
var
    xList: TStrings;
    i: integer;
    xValue: string;
begin
    result := aOptions;
    xList := TStringList.Create;
    try
        OptionsStrToList(xList, aOptions);
        for i := 0 to high(aKeys) do
        begin
            xValue := aValues[i];
            if (xValue = '') then
                CONTINUE;

            xList.Values[aKeys[i]] := xValue;
        end;
        result := ListToOptionStr(xList);
    finally
        xList.Free;
    end;
end;

class procedure TCommonGUIParser.CommentsFromStr(const aCommentString: string;
    out oCommentShort, oCommentLong: string);
var
    xValues: TStringArray;
begin
    if not FindValuesForKeysInOptions(aCommentString, xValues,
        [STR_COMMENT_IDENT_SHORT, STR_COMMENT_IDENT_LONG]) then
    begin

        oCommentShort := '';
        oCommentLong := '';
        EXIT;
    end;

    oCommentShort := xValues[0];
    oCommentLong := xValues[1];
end;

{ TMethodTableUpdateV2 }

const
    cMethOptionKeySourceRack = 'SOURCERACK';
    cMethOptionKeySourceFirstPos = 'SOURCEFIRSTPOS';
    cMethOptionKeySourceLastPos = 'SOURCELASTPOS';
    cMethOptionKeyDestRack = 'DESTRACK';
    cMethOptionKeyDestFirstPos = 'DESTFIRSTPOS';
    cMethOptionKeyDestLastPos = 'DESTLASTPOS';
    cMethOptionKeyDestVol = 'DESTVOL';
    cMethOptionKeyDiluent = 'DILUENT';
    cMethOptionKeyDilRack = 'DILRACK';
    cMethOptionKeyDilPos = 'DILPOS';
    cMethOptionKeyDilVol = 'DILVOL';
    cMethOptionKeyLiqPara = 'LIQPARA';
    cMethOptionKeyDspTransAir = 'DSPTRANSAIR';

constructor TMethodTableUpdateV2.Create(aUpdateNumber: TUpdateID);
begin
    inherited Create(aUpdateNumber, TMethodTableStructDefV1, INT_REVISION_2);
    fAutoSeq := 1;
    AlterStructure(TMethodTableStructDefV2);
    CopyMatchingFields([]);
    self.CopyFieldWithFunc('SEQ', 'SEQ', UpdateSeqField);

    self.CustomDataFunc(MethodUpdateV2CustomFunc);
end;

procedure TMethodTableUpdateV2.UpdateSeqField(const aSourceVal: variant; var vDestVal: variant);
begin
    // Der Inhalt von SEQ ist egal. Wichtig ist, die Reihenfolge zu erhalten und sicherzustellen, dass
    // es nicht zu "Key violation" kommt.
    vDestVal := fAutoSeq;
    inc(fAutoSeq);
end;

procedure TMethodTableUpdateV2.ChangeOldStyleActionName(var vActionName: string; const aDilRack: string);
const
    STR_ACTION_NAME_PIPETTE_OLD = '';
    STR_ACTION_NAME_MESSAGE_OLD = 'MSGB';
    STR_RACKNAME_MANUAL = 'MANUAL';
begin
    vActionName := TMethodGUIParser.ActionComparableName(vActionName);

    // alte Actionnamen abschaffen
    if (vActionName = STR_ACTION_NAME_PIPETTE_OLD) then
        vActionName := STR_ACTION_NAME_PIPETTE;
    if (vActionName = STR_ACTION_NAME_MESSAGE_OLD) then
        vActionName := STR_ACTION_NAME_MESSAGE;
    if (aDilRack = STR_RACKNAME_MANUAL) then
        vActionName := STR_ACTION_NAME_MANUALFILL;
end;

function TMethodTableUpdateV2.ChangeOldStyleOptions(var vActionName: string; const aOptions: string): string;
var
    xImportOptions: TGenericImportOptions;
begin
    if (aOptions = '') then
        EXIT;

    // alte Options umschreiben
    if (vActionName = STR_ACTION_NAME_SQLUPDATE) then
    begin
        result := TMethodGUIParser.SQLUpdateActionOptionsFromStrToStr(aOptions);
    end
    else if (vActionName = STR_ACTION_NAME_SQLWRITEFILE) then
    begin
        result := TMethodGUIParser.SQLWriteActionOptionsFromStrToStr(aOptions);
    end
    else if (vActionName = STR_ACTION_NAME_MOVETUBE) or (vActionName = STR_ACTION_NAME_READTUBE) or
        (vActionName = STR_ACTION_NAME_READANDWEIGHTTUBE) or (vActionName = STR_ACTION_NAME_WEIGHTTUBE) or
        (vActionName = STR_ACTION_NAME_FREEBALANCE) then
    begin
        result := TMethodGUIParser.TubeActionOptionsFromStrToStr(aOptions);
    end
    else if vActionName = STR_ACTION_NAME_DEVICEACTION then
    begin
        result := TMethodGUIParser.DeviceActionOptionsFromStrToStr(aOptions);
    end
    else if vActionName = STR_ACTION_NAME_FLUSH then
    begin
        result := TMethodGUIParser.FlushActionOptionsFromStrToStr(aOptions);
    end
    else if vActionName = STR_ACTION_NAME_PARAMSTORE then
    begin
        result := TMethodGUIParser.ParamStoreActionOptionsFromStrToStr(aOptions);
    end
    else if vActionName = STR_ACTION_NAME_CALLDLLFUNCTION then
    begin
        result := TMethodGUIParser.CallDllFuncActionOptionsFromStrToStr(aOptions);
    end
    else if vActionName = STR_ACTION_NAME_DELAY then
    begin
        result := TMethodGUIParser.DelayActionOptionsFromStrToStr(aOptions);
    end
    else if vActionName = STR_ACTION_NAME_VORTEXERTEMPSET then
    begin
        result := TMethodGUIParser.VortexerTempSetActionOptionsFromStrToStr(aOptions);
    end
    else if vActionName = STR_ACTION_NAME_VORTEXERTEMPCHECK then
    begin
        result := TMethodGUIParser.VortexerTempCheckActionOptionsFromStrToStr(aOptions);
    end
    else if vActionName = STR_ACTION_NAME_VORTEXERSPEED then
    begin
        result := TMethodGUIParser.VortexerSpeedActionOptionsFromStrToStr(aOptions);
    end
    else if vActionName = STR_ACTION_NAME_VORTEXERFIXATION then
    begin
        vActionName := STR_ACTION_NAME_DEVICEACTION;
        result := TMethodGUIParser.VortexerFixationToDeviceActionOptionsFromStrToStr(aOptions);
    end
    else if vActionName = STR_ACTION_NAME_FILECOPY then
    begin
        result := TMethodGUIParser.FileCopyActionOptionsFromStrToStr(aOptions);
    end
    else if vActionName = STR_ACTION_NAME_TIMERSET then
    begin
        result := TMethodGUIParser.TimerSetActionOptionsFromStrToStr(aOptions);
    end
    else if vActionName = STR_ACTION_NAME_TIMERWAIT then
    begin
        result := TMethodGUIParser.TimerWaitActionOptionsFromStrToStr(aOptions);
    end
    else if (vActionName = STR_ACTION_NAME_RACKMOVE) or (vActionName = STR_ACTION_NAME_READB) or
        (vActionName = STR_ACTION_NAME_READE) or (vActionName = STR_ACTION_NAME_CHECKB) or
        (vActionName = STR_ACTION_NAME_CHECKE) then
    begin
        result := TMethodGUIParser.RackActionOptionsFromStrToStr(aOptions);
    end
    else if (vActionName = STR_ACTION_NAME_MESSAGE) then
    begin
        result := TMethodGUIParser.MessageActionOptionsFromStrToStr(aOptions);
    end
    else if vActionName = STR_ACTION_NAME_DELAY then
    begin
        result := TMethodGUIParser.DelayActionOptionsFromStrToStr(aOptions);
    end
    else if (vActionName = STR_ACTION_NAME_PIPETTE) or (vActionName = STR_ACTION_NAME_ASPIRATE) or
        (vActionName = STR_ACTION_NAME_DISPENSE) then
    begin
        result := TMethodGUIParser.PipetteActionOptionsFromStrToStr(aOptions);
    end
    else if (vActionName = STR_ACTION_NAME_PORT) then
    begin
        vActionName := STR_ACTION_NAME_DEVICEACTION;
        result := TMethodGUIParser.DeviceActionOptionsToStr
            (TMethodGUIParser.DeviceActionOptionsFromPortActionStr(aOptions));
    end
    else if (vActionName = STR_ACTION_NAME_BUILDSUBMETHOD) then
    begin
        result := TMethodGUIParser.AddMethodActionOptionsFromStrToStr(aOptions);
    end
    else
        result := aOptions;

    xImportOptions := TMethodGUIParser.GenericImportOptionsFromStr(aOptions);
    if (xImportOptions.DefName <> '') then
        result := TMethodGUIParser.GenericImportOptionsAddToStr(result, xImportOptions);

    // &-Zeichen entfernen
    result := TMethodGUIParser.RemoveConcatAtBeginAndEnd(result);

end;

procedure TMethodTableUpdateV2.AddStringToOptions(var vOptionsText: string; const aKey, aValue: string);
begin
    if (aValue = '') then
        EXIT;

    if vOptionsText <> '' then
        vOptionsText := vOptionsText + ',';
    vOptionsText := vOptionsText + aKey + '=' + aValue;
end;

procedure TMethodTableUpdateV2.MethodUpdateV2CustomFunc(aSender: TObject);
var
    xDestDP: TDataProvider;
    xSourceDP: TDataProvider;
    xOptionsText: string;
    xActionName: string;
    xDilRack: string;

    procedure AddFieldToOptions(const aFieldName: string; const aKey: string);
    begin
        // if not xSourceDP.FieldByName( aFieldName ).IsNull then begin
        AddStringToOptions(xOptionsText, aKey, xSourceDP.FieldByName(aFieldName).AsString);
        // end;
    end;

begin
    xDestDP := fTableChangeAdaptor.CreateDestDataProvider();
    xDestDP.SelectAndOpen('SELECT * FROM Method', false);
    try
        xSourceDP := fTableChangeAdaptor.CreateSourceDataProvider();
        try
            xSourceDP.SelectAndOpen('SELECT * FROM Method', true);
            try
                while not xSourceDP.Eof do
                begin
                    xActionName := xSourceDP.FieldByName(STR_METHOD_V0_FLD_ACTION).AsString;
                    xOptionsText := xSourceDP.FieldByName(STR_METHOD_V0_FLD_REMARK).AsString +
                        xSourceDP.FieldByName(STR_METHOD_V0_FLD_REMARKEXTEND).AsString;
                    xDilRack := xSourceDP.FieldByName(STR_METHOD_V0_FLD_DILRACK).AsString;

                    ChangeOldStyleActionName(xActionName, xDilRack);
                    xOptionsText := ChangeOldStyleOptions(xActionName, xOptionsText);

                    AddFieldToOptions(STR_METHOD_V0_FLD_SOURCERACK, cMethOptionKeySourceRack);
                    AddFieldToOptions(STR_METHOD_V0_FLD_SOURCEFIRSTPOS, cMethOptionKeySourceFirstPos);
                    AddFieldToOptions(STR_METHOD_V0_FLD_SOURCELASTPOS, cMethOptionKeySourceLastPos);
                    AddFieldToOptions(STR_METHOD_V0_FLD_DESTRACK, cMethOptionKeyDestRack);
                    AddFieldToOptions(STR_METHOD_V0_FLD_DESTFIRSTPOS, cMethOptionKeyDestFirstPos);
                    AddFieldToOptions(STR_METHOD_V0_FLD_DESTLASTPOS, cMethOptionKeyDestLastPos);
                    AddFieldToOptions(STR_METHOD_V0_FLD_DESTVOL, cMethOptionKeyDestVol);

                    // Diluent wurde früher von immer mit SYSTEM beschrieben, nur PIPET und FLUSH nutzen diese Info
                    if (xActionName = STR_ACTION_NAME_PIPETTE) or (xActionName = STR_ACTION_NAME_FLUSH) then
                        AddFieldToOptions(STR_METHOD_V0_FLD_DILUENT, cMethOptionKeyDiluent);

                    AddFieldToOptions(STR_METHOD_V0_FLD_DILVOL, cMethOptionKeyDilVol);
                    AddFieldToOptions(STR_METHOD_V0_FLD_DILRACK, cMethOptionKeyDilRack);
                    AddFieldToOptions(STR_METHOD_V0_FLD_DILPOS, cMethOptionKeyDilPos);
                    AddFieldToOptions(STR_METHOD_V0_FLD_LIQPARA, cMethOptionKeyLiqPara);

                    xDestDP.Edit;
                    xDestDP.FieldByName(STR_METHOD_V0_FLD_ACTION).AsString := xActionName;
                    xDestDP.FieldByName(STR_METHOD_V3_FLD_AOPTIONS).AsString := xOptionsText;
                    xDestDP.Post;
                    xDestDP.Next;

                    xSourceDP.Next;
                end;
            finally
                xSourceDP.Close();
            end;
        finally
            xSourceDP.Free;
        end;

    finally
        xDestDP.Free;
    end;
end;

{ TMethodTableUpdateV2_1 }

constructor TMethodTableUpdateV2_1.Create(aUpdateNumber: TUpdateID);
begin
    inherited Create(aUpdateNumber, TMethodTableStructDefV2, INT_REVISION_2, INT_MINORREVISION_1);
    AlterStructure(TMethodTableStructDefV2);
    CopyMatchingFields([]);

    self.CustomDataFunc(MethodUpdateV2_1CustomFunc);
end;

function TMethodTableUpdateV2_1.GetFirstPipDevice(): string;
var
    xSourceDP: TDataProvider;
begin
    xSourceDP := fTableChangeAdaptor.CreateSourceDataProvider();
    try
        // neuer Wert, der für das Layout-Update V2 benötigt wird
        xSourceDP.SelectAndOpen('select * from settings where settings."AREA" = ''ROBOT''' +
            ' and settings."SECTION" = ''TempConversion'' and settings."IDENT" = ''FlushPipDevice''', true);
        result := xSourceDP.FieldByName('VALUE').AsString;
        xSourceDP.Close;
    finally
        xSourceDP.Free;
    end;
end;

procedure TMethodTableUpdateV2_1.AddPipDeviceToFlushOptions(var vOptions: string; const aPipDevice: string);
const
    cPipDevKey = 'FLUSHPIPDEV';
begin
    if POS(cPipDevKey, vOptions) > 1 then
        EXIT; // PipDevice ist eingetragen

    if (vOptions <> '') then
        vOptions := vOptions + ',';

    vOptions := vOptions + cPipDevKey + '=' + aPipDevice;

end;

procedure TMethodTableUpdateV2_1.MethodUpdateV2_1CustomFunc(aSender: TObject);
var
    xDestDP: TDataProvider;
    xActionName, xOptionsText, xFirstPipDevice: string;
begin
    xFirstPipDevice := GetFirstPipDevice();

    xDestDP := fTableChangeAdaptor.CreateDestDataProvider();
    try
        xDestDP.SelectAndOpen('SELECT * FROM Method', false);
        try
            while not xDestDP.Eof do
            begin
                xActionName := xDestDP.FieldByName(STR_METHOD_V0_FLD_ACTION).AsString;

                if (xFirstPipDevice <> '') and (xActionName = 'FLUSH') then
                begin
                    xOptionsText := xDestDP.FieldByName(STR_METHOD_V3_FLD_AOPTIONS).AsString;
                    AddPipDeviceToFlushOptions(xOptionsText, xFirstPipDevice);
                    xDestDP.Edit;
                    xDestDP.FieldByName(STR_METHOD_V3_FLD_AOPTIONS).AsString := xOptionsText;
                    xDestDP.Post;
                end;

                xDestDP.Next;
            end;
        finally
            xDestDP.Close;
        end;
    finally
        xDestDP.Free;
    end;
end;

{ TMethodTableUpdateV3 }

constructor TMethodTableUpdateV3.Create(aUpdateNumber: TUpdateID);
begin
    inherited Create(aUpdateNumber, TMethodTableStructDefV2, INT_REVISION_3);
    AlterStructure(TMethodTableStructDefV3);
    CopyMatchingFields([]);

    self.CustomDataFunc(MethodUpdateV3CustomFunc);
end;

procedure TMethodTableUpdateV3.ReplaceTotalTipMap(aPipDevLoader: TSettingsTablePipDeviceLoader);
const
    STR_OPTION_KEY_GENBUILD_USEDPIPDEVICE = 'USEDPIPDEVICE';
    STR_OPTION_KEY_GENBUILD_USEDTIPS = 'USEDTIPS';
var
    xDP: TDataProvider;
    xOptionsText, xPipDeviceName: string;
    xUsedTips: integer;
    xValues: TStringArray;
begin
    xDP := fTableChangeAdaptor.CreateDestDataProvider();
    try
        xDP.SelectAndOpen('SELECT * FROM Method', false);
        try
            while not xDP.Eof do
            begin

                xOptionsText := xDP.FieldByName(STR_METHOD_V3_FLD_AOPTIONS).AsString;
                if TCommonGUIParser.FindValuesForKeysInOptions(xOptionsText, xValues,
                    [STR_OPTION_KEY_GENBUILD_USEDTIPS]) then
                begin
                    xUsedTips := StrToIntDef(xValues[0], 0);
                    if (xUsedTips <> 0) then
                    begin
                        xUsedTips := aPipDevLoader.GetArmTipmapFromTotalTipmap(xUsedTips, xPipDeviceName);
                        xOptionsText := TCommonGUIParser.AddKeysAndValuesToOptions(xOptionsText,
                            [STR_OPTION_KEY_GENBUILD_USEDPIPDEVICE, STR_OPTION_KEY_GENBUILD_USEDTIPS],
                            [xPipDeviceName, IntToStr(xUsedTips)]);

                        xDP.Edit;
                        xDP.FieldByName(STR_METHOD_V3_FLD_AOPTIONS).AsString := xOptionsText;
                        xDP.Post;
                    end;
                end;

                xDP.Next;
            end;
        finally
            xDP.Close();
        end;
    finally
        xDP.Free;
    end;
end;

procedure TMethodTableUpdateV3.MethodUpdateV3CustomFunc(aSender: TObject);
var
    xPipDevLoader: TSettingsTablePipDeviceLoader;
begin
    xPipDevLoader := TSettingsTablePipDeviceLoader.Create(fTableChangeAdaptor);
    try
        self.ReplaceTotalTipMap(xPipDevLoader);
    finally
        xPipDevLoader.Free;
    end;
end;

{ TMethodTableUpdateV3_1 }

constructor TMethodTableUpdateV3_1.Create(aUpdateNumber: TUpdateID);
begin
    inherited Create(aUpdateNumber, TMethodTableStructDefV3, INT_REVISION_3, INT_MINORREVISION_1);
    self.AlterStructure(TMethodTableStructDefV3);
    CopyMatchingFields([]);

    self.CustomDataFunc(MethodUpdateV3_1CustomFunc);
end;

function TMethodTableUpdateV3_1.GetUpdateDescription: string;
begin
    result := 'Convert RUN action to RUNST action';
end;

procedure TMethodTableUpdateV3_1.MethodUpdateV3_1CustomFunc(aSender: TObject);
const

    STR_OPTION_KEY_RUNLOAD_RUNNAME = cV0_ActionNameRun + 'NAME';
    STR_OPTION_KEY_RUNLOAD_PARAMS = cV0_ActionNameRun + 'PARAMS';
    STR_OPTION_KEY_RUNLOAD_REPEAT = cV0_ActionNameRun + 'REPEAT';
    STR_OPTION_KEY_RUNLOAD_REPEATNUMLINES = cV0_ActionNameRun + 'REPEATNUMLINES';

var
    xDP: TDataProvider;
    xOptionsText: string;
    xValues: TStringArray;
    xSubMethodName: string;
    xParams: string;
    xRunRepeatText: string;
    xActionName: string;
begin
    xDP := fTableChangeAdaptor.CreateDestDataProvider();
    try
        xDP.SelectAndOpen('SELECT * FROM Method', false);
        try
            while not xDP.Eof do
            begin
                xActionName := xDP.FieldByName(STR_METHOD_V0_FLD_ACTION).AsString;
                xOptionsText := xDP.FieldByName(STR_METHOD_V3_FLD_AOPTIONS).AsString;
                if SameText(xActionName, cV0_ActionNameRun) then
                begin
                    TCommonGUIParser.FindValuesForKeysInOptions(xOptionsText, xValues,
                        [STR_OPTION_KEY_RUNLOAD_RUNNAME, STR_OPTION_KEY_RUNLOAD_PARAMS,
                        STR_OPTION_KEY_RUNLOAD_REPEAT]);

                    xSubMethodName := xValues[0];
                    xParams := xValues[1];
                    xRunRepeatText := xValues[2];

                    xActionName := cV0_ActionNameRunst;
                    xOptionsText := TCommonGUIParser.AddKeysAndValuesToOptions('',
                        [cV3_OptionKeyRunstName, cV3_OptionKeyRunstParams, cV3_OptionKeyRunstRepeat],
                        [xSubMethodName, xParams, xRunRepeatText]);
                    xDP.Edit;
                    xDP.FieldByName(STR_METHOD_V0_FLD_ACTION).AsString := xActionName;
                    xDP.FieldByName(STR_METHOD_V3_FLD_AOPTIONS).AsString := xOptionsText;
                    xDP.Post;
                end;
                xDP.Next;
            end;
        finally
            xDP.Close();
        end;
    finally
        xDP.Free;
    end;
end;

{ TMethodTableUpdateV4 }

constructor TMethodTableUpdateV4.Create(aUpdateNumber: TUpdateID);
begin
    inherited Create(aUpdateNumber, TMethodTableStructDefV3, INT_REVISION_4);

    self.AlterStructure(TMethodTableStructDefV4);
    CopyMatchingFields([]);

    self.CustomDataFunc(MethodUpdateV4CustomFunc);
end;

function TMethodTableUpdateV4.GetUpdateDescription: string;
begin
    result := 'Change Method name length to 50, Replace Command Macros by Methods and change Event parameters for Dll Calls and command macros';
end;

procedure TMethodTableUpdateV4.MethodUpdateV4CustomFunc(aSender: TObject);
begin
    AddCommandMacrosAsDummyMethods();
    ReplaceDllCallEventParams();
    ChangeCommaActionToRunst();
end;

function StringArrayOf(const aArr: array of string): TStringArray;
var
    i: integer;
begin
    SetLength(result, high(aArr) + 1);
    for i := 0 to high(aArr) do
    begin
        result[i] := aArr[i];
    end;
end;

procedure TMethodTableUpdateV4.ReplaceDllCallEventParams();
const
    STR_OPTION_KEY_PIP_PREFIX = 'PIP';
    STR_OPTION_KEY_PIP_EVBEFOREASP = STR_OPTION_KEY_PIP_PREFIX + 'EVBASP';
    STR_OPTION_KEY_PIP_EVBEFOREPICKLQ = STR_OPTION_KEY_PIP_PREFIX + 'EVBPICKLQ';
    STR_OPTION_KEY_PIP_EVAFTERPICKLQ = STR_OPTION_KEY_PIP_PREFIX + 'EVAPICKLQ';
    STR_OPTION_KEY_PIP_EVAFTERASP = STR_OPTION_KEY_PIP_PREFIX + 'EVAASP';
    STR_OPTION_KEY_PIP_EVBEFOREDISP = STR_OPTION_KEY_PIP_PREFIX + 'EVBDISP';
    STR_OPTION_KEY_PIP_EVBEFOREDISPLQ = STR_OPTION_KEY_PIP_PREFIX + 'EVBDISPLQ';
    STR_OPTION_KEY_PIP_EVAFTERDISPLQ = STR_OPTION_KEY_PIP_PREFIX + 'EVADISPLQ';
    STR_OPTION_KEY_PIP_EVAFTERDISP = STR_OPTION_KEY_PIP_PREFIX + 'EVADISP';

    STR_OPTION_KEY_TUBE_PREFIX = 'TUBE';
    STR_OPTION_KEY_TUBE_BEFOREGET = STR_OPTION_KEY_TUBE_PREFIX + 'BGET';
    STR_OPTION_KEY_TUBE_AFTERGET = STR_OPTION_KEY_TUBE_PREFIX + 'AGET';
    STR_OPTION_KEY_TUBE_BEFOREPUT = STR_OPTION_KEY_TUBE_PREFIX + 'BPUT';
    STR_OPTION_KEY_TUBE_AFTERPUT = STR_OPTION_KEY_TUBE_PREFIX + 'APUT';
    STR_OPTION_KEY_TUBEBC_BEFOREREAD = STR_OPTION_KEY_TUBE_PREFIX + 'BREADBC';
    STR_OPTION_KEY_TUBEBC_AFTERREAD = STR_OPTION_KEY_TUBE_PREFIX + 'AREADBC';

    STR_OPTION_KEY_RACK_PREFIX = 'RACK';
    STR_OPTION_KEY_RACK_BEFOREGET = STR_OPTION_KEY_RACK_PREFIX + 'BGET';
    STR_OPTION_KEY_RACK_AFTERGET = STR_OPTION_KEY_RACK_PREFIX + 'AGET';
    STR_OPTION_KEY_RACK_BEFOREPUT = STR_OPTION_KEY_RACK_PREFIX + 'BPUT';
    STR_OPTION_KEY_RACK_AFTERPUT = STR_OPTION_KEY_RACK_PREFIX + 'APUT';
    STR_OPTION_KEY_RACKBC_BEFOREREAD = STR_OPTION_KEY_RACK_PREFIX + 'BREADBC';
    STR_OPTION_KEY_RACKBC_AFTERREAD = STR_OPTION_KEY_RACK_PREFIX + 'AREADBC';

    STR_OPTION_SUBKEY_CALLDLL_DLLNAME = 'NAME';
    STR_OPTION_SUBKEY_CALLDLL_DLLFUNCTION = 'FUNC';
    STR_OPTION_SUBKEY_CALLDLL_PARAMETER = 'PARAM';
    STR_OPTION_SUBKEY_CALLDLL_STORENAME = 'STORENAME';
var
    xDP: TDataProvider;
    x, xDllCallIndex: integer;
    xOptionsText, xNewMethodName: string;
    xValues, xDllValues, xAllEventKeys: TStringArray;
    xEventText: string;
begin
    xAllEventKeys := StringArrayOf([STR_OPTION_KEY_TUBE_BEFOREGET, STR_OPTION_KEY_TUBE_AFTERGET,
        STR_OPTION_KEY_TUBE_BEFOREPUT, STR_OPTION_KEY_TUBE_AFTERPUT, STR_OPTION_KEY_TUBEBC_BEFOREREAD,
        STR_OPTION_KEY_TUBEBC_AFTERREAD, STR_OPTION_KEY_RACK_BEFOREGET, STR_OPTION_KEY_RACK_AFTERGET,
        STR_OPTION_KEY_RACK_BEFOREPUT, STR_OPTION_KEY_RACK_AFTERPUT, STR_OPTION_KEY_RACKBC_BEFOREREAD,
        STR_OPTION_KEY_RACKBC_AFTERREAD, STR_OPTION_KEY_PIP_EVBEFOREASP, STR_OPTION_KEY_PIP_EVBEFOREPICKLQ,
        STR_OPTION_KEY_PIP_EVAFTERPICKLQ, STR_OPTION_KEY_PIP_EVAFTERASP, STR_OPTION_KEY_PIP_EVBEFOREDISP,
        STR_OPTION_KEY_PIP_EVBEFOREDISPLQ, STR_OPTION_KEY_PIP_EVAFTERDISPLQ, STR_OPTION_KEY_PIP_EVAFTERDISP]);

    xDP := fTableChangeAdaptor.CreateDestDataProvider();
    try
        xDP.SelectAndOpen('SELECT * FROM Method', false);
        try
            xDllCallIndex := 1;
            while not xDP.Eof do
            begin
                xOptionsText := xDP.FieldByName(STR_METHOD_V3_FLD_AOPTIONS).AsString;
                if TCommonGUIParser.FindValuesForKeysInOptions(xOptionsText, xValues, xAllEventKeys) then
                begin

                    for x := 0 to high(xValues) do
                    begin

                        // ursprüngliche Event-Paramer lesen (wie DLLFU-Action)
                        TCommonGUIParser.FindValuesForKeysInOptions(xValues[x], xDllValues,
                            [STR_OPTION_SUBKEY_CALLDLL_DLLNAME, STR_OPTION_SUBKEY_CALLDLL_DLLFUNCTION,
                            STR_OPTION_SUBKEY_CALLDLL_PARAMETER, STR_OPTION_SUBKEY_CALLDLL_STORENAME]);

                        if (Uppercase(xDllValues[0]) = 'COMMAND') then
                        begin // Event soll Command-Macro aufrufen

                            // Event-Parameter: Aufruf der Methode, die aus dem Command-Makro gemacht wurde
                            xNewMethodName := cMethodNamePrefixExCommand + xDllValues[2];
                            xEventText := 'RUNSTNAME=' + xNewMethodName;
                        end
                        else if (Uppercase(xDllValues[0]) = 'RUNST') then
                        begin // Event soll Methode aufrufen

                            xEventText := xDllValues[2]; // Parameter-Feld = Event-Parameter
                        end
                        else if (xDllValues[0] <> '') then
                        begin // Event soll Dll-Funktion aufrufen

                            // Neue Methode mit einer Zeile erzeugen
                            xNewMethodName :=
                                Copy(cMethodNamePrefixExDllCall + xDllValues[0] + IntToStr(xDllCallIndex) +
                                '_' + xDllValues[1], 1, 50);
                            AddMethodLine(xNewMethodName, 'DLLFU', 'DLLCALL={' + xValues[x] + '}', 10);
                            inc(xDllCallIndex);

                            { TODO -owl : Für jeden Aufruf wird eine neue Methode erzeugt - besser: Gleiche Aufrufe auf eine Methode lenken }

                            { TODO -owl : Variablen-Übergaben werden so nicht übernommen }

                            // Aufruf dieser Methode
                            xEventText := 'RUNSTNAME=' + xNewMethodName;
                        end
                        else
                        begin
                            xEventText := '';
                        end;

                        xValues[x] := xEventText;
                    end;

                    xOptionsText := TCommonGUIParser.AddKeysAndValuesToOptions(xOptionsText,
                        xAllEventKeys, xValues);

                    xDP.Edit;
                    xDP.FieldByName(STR_METHOD_V3_FLD_AOPTIONS).AsString := xOptionsText;
                    xDP.Post;
                end;
                xDP.Next;
            end;
        finally
            xDP.Close();
        end;
    finally
        xDP.Free;
    end;
end;

procedure TMethodTableUpdateV4.AddMethodLine(const aMethodName, aAction, aOptions: string; aSeq: integer);
var
    xDP: TDataProvider;
begin
    xDP := fTableChangeAdaptor.CreateDestDataProvider();
    try
        xDP.SelectAndOpen('SELECT * FROM Method', false);
        try
            xDP.Append;
            xDP.FieldByName(STR_METHOD_V0_FLD_NAME).AsString := aMethodName;
            xDP.FieldByName(STR_METHOD_V0_FLD_SEQ).AsInteger := aSeq;
            xDP.FieldByName(STR_METHOD_V0_FLD_ACTION).AsString := aAction;
            xDP.FieldByName(STR_METHOD_V3_FLD_AOPTIONS).AsString := aOptions;
            xDP.Post;
        finally
            xDP.Close();
        end;
    finally
        xDP.Free;
    end;
end;

procedure TMethodTableUpdateV4.AddCommandMacrosAsDummyMethods();
var
    xCommandDP: TDataProvider;
    xOptionsText, xLastName, xCurrentName: string;
    xSeq: integer;
    xExecuteStr: string;
begin
    xCommandDP := fTableChangeAdaptor.CreateSourceDataProvider();
    try
        xCommandDP.SelectAndOpen('SELECT * FROM COMMAND', false);
        try
            xSeq := 10;
            xLastName := '';
            while not xCommandDP.Eof do
            begin
                if xCommandDP.FieldByName('Execute').AsBoolean then
                    xExecuteStr := 'Execute'
                else
                    xExecuteStr := '      ';

                xOptionsText := 'Adr=' + xCommandDP.FieldByName('Adr').AsString + ', Cmd=' +
                    xCommandDP.FieldByName('Command').AsString + ', ' + xExecuteStr + ', Flag=' +
                    xCommandDP.FieldByName('Flag').AsString + ', ' + xCommandDP.FieldByName('Remark')
                    .AsString;
                xCurrentName := xCommandDP.FieldByName('MAKRONAME').AsString;

                if (xLastName = xCurrentName) then
                    xSeq := xSeq + 10
                else
                    xSeq := 10;

                // erstellt Dummy-Eintrag in Method.db, der alle Infos aus dem Command Makro enthält
                AddMethodLine(cMethodNamePrefixExCommand + xCurrentName, 'REMAR', xOptionsText, xSeq);

                xLastName := xCurrentName;
                xCommandDP.Next;
            end;
        finally
            xCommandDP.Close();
        end;
    finally
        xCommandDP.Free;
    end;
end;

procedure TMethodTableUpdateV4.ChangeCommaActionToRunst();
const
    STR_ACTION_NAME_COMMAND = 'COMMA';
    STR_OPTION_KEY_COMMAND_NAME = STR_ACTION_NAME_COMMAND + 'NAME';

    cActionNameRunst = 'RUNST';
    cOptionKeyRunstName = cActionNameRunst + 'NAME';
    cOptionKeyRunstParams = cActionNameRunst + 'PARAMS';
var
    xDP: TDataProvider;
    xOptionsText: string;
    xValues: TStringArray;
    xActionName: string;
begin
    xDP := fTableChangeAdaptor.CreateDestDataProvider();
    try
        xDP.SelectAndOpen('SELECT * FROM Method', false);
        try
            while not xDP.Eof do
            begin
                xActionName := xDP.FieldByName(STR_METHOD_V0_FLD_ACTION).AsString;
                xOptionsText := xDP.FieldByName(STR_METHOD_V3_FLD_AOPTIONS).AsString;
                if SameText(xActionName, STR_ACTION_NAME_COMMAND) then
                begin
                    if TCommonGUIParser.FindValuesForKeysInOptions(xOptionsText, xValues,
                        [STR_OPTION_KEY_COMMAND_NAME]) then
                        xOptionsText := cOptionKeyRunstName + '=' + cMethodNamePrefixExCommand + xValues[0]
                    else
                        xOptionsText := cOptionKeyRunstName + '=' + cMethodNamePrefixExCommand + xOptionsText;

                    xActionName := cActionNameRunst;

                    xDP.Edit;
                    xDP.FieldByName(STR_METHOD_V0_FLD_ACTION).AsString := xActionName;
                    xDP.FieldByName(STR_METHOD_V3_FLD_AOPTIONS).AsString := xOptionsText;
                    xDP.Post;
                end;
                xDP.Next;
            end;
        finally
            xDP.Close();
        end;
    finally
        xDP.Free;
    end;
end;

class procedure TV4MethodUtils.ReadMethodLines(aAdaptor: TTableChangeAdaptor; const aMethodName: string;
    const aList: TObjectList<TV4MethodLine>);
var
    xDP: TDataProvider;
    xMethodLine: TV4MethodLine;
begin
    xDP := aAdaptor.CreateDestDataProvider();
    try
        xDP.SelectAndOpen('SELECT * FROM Method WHERE ' + TSQL.FieldEq('Method', 'Name', aMethodName), true);
        try
            while not xDP.Eof do
            begin
                xMethodLine := TV4MethodLine.Create();
                xMethodLine.Name := aMethodName;
                xMethodLine.Seq := 0;
                xMethodLine.Action := xDP.FieldByName(STR_METHOD_V0_FLD_ACTION).AsString;;
                xMethodLine.Options := xDP.FieldByName(STR_METHOD_V3_FLD_AOPTIONS).AsString;
                xMethodLine.Comment := xDP.FieldByName(STR_METHOD_V0_FLD_COMMENT).AsString;
                xMethodLine.LineDisabled := xDP.FieldByName(STR_METHOD_V3_FLD_LINEDISABLED).AsBoolean;
                aList.Add(xMethodLine);
                xDP.Next;
            end;
        finally
            xDP.Close();
        end;
    finally
        xDP.Free;
    end;
end;

class procedure TV4MethodUtils.WriteMethodLines(aAdaptor: TTableChangeAdaptor; const aMethodName: string;
    const aList: TObjectList<TV4MethodLine>);
var
    xDP: TDataProvider;
    xMethodLine: TV4MethodLine;
    x: integer;
begin
    xDP := aAdaptor.CreateDestDataProvider();
    try
        xDP.ExecSQL(Format(cV0_DeleteMethod, [aMethodName]));
        xDP.SelectAndOpen(cV0_SelectALL, false);
        try
            for x := 0 to aList.Count - 1 do
            begin
                xMethodLine := aList[x] as TV4MethodLine;
                xDP.Append();
                xDP.FieldByName(STR_METHOD_V0_FLD_NAME).AsString := aMethodName;
                xDP.FieldByName(STR_METHOD_V0_FLD_SEQ).AsInteger := (x + 1) * 10;
                xDP.FieldByName(STR_METHOD_V0_FLD_ACTION).AsString := xMethodLine.Action;
                xDP.FieldByName(STR_METHOD_V3_FLD_AOPTIONS).AsString := xMethodLine.Options;
                xDP.FieldByName(STR_METHOD_V0_FLD_COMMENT).AsString := xMethodLine.Comment;
                xDP.FieldByName(STR_METHOD_V3_FLD_LINEDISABLED).AsBoolean := xMethodLine.LineDisabled;
                xDP.Post;
            end;
        finally
            xDP.Close();
        end;
    finally
        xDP.Free;
    end;
end;

class procedure TV4MethodUtils.GetMethodNames(aAdaptor: TTableChangeAdaptor; const aSQLWhere: string;
    aList: TStringList);
var
    xDP: TDataProvider;
    xLastName: string;
begin
    xDP := aAdaptor.CreateDestDataProvider();
    try
        xDP.SelectAndOpen('SELECT Name FROM Method' + aSQLWhere + ' order by name', false);
        try
            xLastName := '';
            while not xDP.Eof do
            begin
                if not SameText(xLastName, xDP.FieldByName(STR_METHOD_V0_FLD_NAME).AsString) then
                begin
                    xLastName := xDP.FieldByName(STR_METHOD_V0_FLD_NAME).AsString;
                    aList.Add(xDP.FieldByName(STR_METHOD_V0_FLD_NAME).AsString);
                end;
                xDP.Next;
            end;
        finally
            xDP.Close();
        end;
    finally
        xDP.Free;
    end;
end;

{ TMethodTableUpdateV4_1 }

constructor TMethodTableUpdateV4_1.Create(aUpdateNumber: TUpdateID);
begin
    inherited Create(aUpdateNumber, TMethodTableStructDefV4, INT_REVISION_4, INT_MINORREVISION_1);
    self.AlterStructure(TMethodTableStructDefV4);
    CopyMatchingFields([]);

    self.CustomDataFunc(MethodUpdateV4_1CustomFunc);
end;

function TMethodTableUpdateV4_1.GetUpdateDescription: string;
begin
    result := 'Convert RUNST to ADDM action';
end;

function TMethodTableUpdateV4_1.BeginsWithText(const aText, aSubText: string): boolean;
begin
    result := Pos(Uppercase(aSubText), UpperCase(aText)) = 1;
end;

function TMethodTableUpdateV4_1.FindRepeatCondition(const aSubMethodName: string): string;
const
    cV0_IfToken = 'IF';
    cV0_EndIfToken = 'ENDIF';
var
    xDP: TDataProvider;
    xNestedIFCount: integer;
    xActionName: string;
    xOptionsText: string;
begin
    result := '';

    xDP := fTableChangeAdaptor.CreateDestDataProvider();
    try
        xDP.SelectAndOpen(Format(cV0_SelectMethod, [aSubMethodName]), true);
        try
            // go to the last line in method
            xDP.Last;
            // if the last line is not an ENDIF, this method could not have been used for runrepeat
            xActionName := xDP.FieldByName(STR_METHOD_V0_FLD_ACTION).AsString;
            if not SameText(xActionName, cV0_ActionNameCalc) then
                EXIT;
            xOptionsText := xDP.FieldByName(STR_METHOD_V3_FLD_AOPTIONS).AsString;
            if not BeginsWithText(xOptionsText, cV0_EndIfToken) then
                EXIT;
            xDP.Prior;

            xNestedIFCount := 1;
            while not xDP.Bof do
            begin
                xActionName := xDP.FieldByName(STR_METHOD_V0_FLD_ACTION).AsString;
                if SameText(xActionName, cV0_ActionNameCalc) then
                begin
                    xOptionsText := xDP.FieldByName(STR_METHOD_V3_FLD_AOPTIONS).AsString;
                    if BeginsWithText(xOptionsText, cV0_EndIfToken) then
                    begin
                        Inc(xNestedIFCount);
                    end
                    else if BeginsWithText(xOptionsText, cV0_IfToken) then
                    begin
                        Dec(xNestedIFCount);
                        // here we have found the IF that matches the ENDIF for the last line
                        if xNestedIFCount = 0 then
                        begin
                            result := Trim(Copy(Trim(xOptionsText), 3, Length(xOptionsText)));
                            EXIT;
                        end;
                    end;

                end;
                xDP.Prior;
            end;
        finally
            xDP.Close();
        end;
    finally
        xDP.Free;
    end;
end;

procedure TMethodTableUpdateV4_1.MethodUpdateV4_1CustomFunc(aSender: TObject);
const
    cV0_WhileToken = 'WHILE';
    cV0_EndWhileToken = 'ENDWHILE';
var
    xDP: TDataProvider;
    xParams: string;
    xActionName: string;
    xList: TStringList;
    i: integer;
    xCurIndex: integer;
    xMethodName: string;
    xMethLine: TV4MethodLine;
    xCalcMethLine: TV4MethodLine;
    xValues: TStringArray;
    xOptionsText: string;
    xMethLines: TObjectList<TV4MethodLine>;
    xRepeatCondition: string;
    xMethodChanged: boolean;
begin

    xList := TStringList.Create();
    TV4MethodUtils.GetMethodNames(fTableChangeAdaptor, ' WHERE ' + TSQL.FieldSameText('Method', 'Action',
        'RUNST'), xList);

    xMethLines := TObjectList<TV4MethodLine>.Create(true);
    try
        // change runrepeat to loop
        for i := 0 to xList.Count - 1 do
        begin
            xMethodName := xList[i];
            xMethLines.Clear();
            TV4MethodUtils.ReadMethodLines(fTableChangeAdaptor, xMethodName, xMethLines);
            xMethodChanged := false;
            xCurIndex := 0;
            while xCurIndex < xMethLines.Count do
            begin
                xMethLine := xMethLines[xCurIndex] as TV4MethodLine;
                if SameText(xMethLine.Action, cV0_ActionNameRunst) then
                begin
                    if (Pos(cV3_OptionKeyRunstRepeat, xMethLine.Options) <> 0) then
                    begin
                        TCommonGUIParser.FindValuesForKeysInOptions(xMethLine.Options, xValues,
                            [cV3_OptionKeyRunstName, cV3_OptionKeyRunstParams, cV3_OptionKeyRunstRepeat]);
                        if SameText(Trim(xValues[2]), 'YES') then
                        begin
                            xRepeatCondition := FindRepeatCondition(xValues[0]);

                            // insert while
                            xCalcMethLine := TV4MethodLine.Create();
                            xCalcMethLine.Name := xMethodName;
                            xCalcMethLine.Action := cV0_ActionNameCalc;
                            xCalcMethLine.Options := cV0_WhileToken + ' ' + xRepeatCondition;
                            xMethLines.Insert(xCurIndex, xCalcMethLine);
                            Inc(xCurIndex);

                            // insert endwhile
                            xCalcMethLine := TV4MethodLine.Create();
                            xCalcMethLine.Name := xMethodName;
                            xCalcMethLine.Action := cV0_ActionNameCalc;
                            xCalcMethLine.Options := cV0_EndWhileToken;
                            xMethLines.Insert(xCurIndex + 1, xCalcMethLine);
                            Inc(xCurIndex);

                            xMethodChanged := true;
                        end;
                    end;
                end;

                Inc(xCurIndex);

            end;

            if xMethodChanged then
                TV4MethodUtils.WriteMethodLines(fTableChangeAdaptor, xMethodName, xMethLines);

        end;
    finally
        xMethLines.Free;
    end;

    xDP := fTableChangeAdaptor.CreateDestDataProvider();
    try
        // change RUNST to ADDM
        xDP.SelectAndOpen('SELECT * FROM Method', false);
        try
            while not xDP.Eof do
            begin
                xActionName := xDP.FieldByName(STR_METHOD_V0_FLD_ACTION).AsString;
                xOptionsText := xDP.FieldByName(STR_METHOD_V3_FLD_AOPTIONS).AsString;
                if SameText(xActionName, cV0_ActionNameRunst) then
                begin
                    TCommonGUIParser.FindValuesForKeysInOptions(xOptionsText, xValues,
                        [cV3_OptionKeyRunstName, cV3_OptionKeyRunstParams]);
                    xMethodName := xValues[0];
                    xParams := xValues[1];

                    xOptionsText := TCommonGUIParser.AddKeysAndValuesToOptions('',
                        [cV0_OptionKeyAddMName, cV0_OptionKeyAddMParams], [xMethodName, xParams]);
                    xDP.Edit;
                    xDP.FieldByName(STR_METHOD_V0_FLD_ACTION).AsString := cV0_ActionNameAddM;
                    xDP.FieldByName(STR_METHOD_V3_FLD_AOPTIONS).AsString := xOptionsText;
                    xDP.Post;
                end;
                xDP.Next;
            end;
        finally
            xDP.Close();
        end;
    finally
        xDP.Free;
    end;

end;

const
    cConditionAndToken = '&&';
    cConditionOrToken = '||';
    cMethOptionKeyPrefix = 'DS';

    cMethOptionKeyDefName = cMethOptionKeyPrefix + 'DEFNAME';
    cMethOptionKeyFilter = cMethOptionKeyPrefix + 'FILTER';
    cMethOptionKeyDatasetObjectIdentName = cMethOptionKeyPrefix + 'OBJIDENTNAME';
    cMethOptionKeyRecordCountIdentName = cMethOptionKeyPrefix + 'RECCOUNTIDENTNAME';

    cMethOptionKeyDatasetObject = cMethOptionKeyPrefix + 'OBJECT';

    cMethOptionKeyIsRelativeMove = cMethOptionKeyPrefix + 'MOVETYPE';
    cMethOptionKeyMoveOffset = cMethOptionKeyPrefix + 'MOVEOFFSET';
    cMethOptionKeyRecordNumIdentName = cMethOptionKeyPrefix + 'RECNUMIDENTNAME';

    cActionNameDatasetOpen = 'DSOPN';
    cActionNameDatasetClose = 'DSCLS';
    cActionNameDatasetCursorMove = 'DSCUR';
    cActionNameDatasetRead = 'DSREA';

    cDataTotalLinesSuffix = 'TotalLines';
    cDataLineCountSuffix = 'LineCnt';

    cCalcFuncDataOpen = 'DATAOPEN';
    cCalcFuncDataNotEof = 'DATANOTEOF';
    cCalcFuncDataRead = 'DATAREAD';
    cCalcFuncDataCursor = 'DATACURSOR';

    { TMethodTableUpdateV4_2 }

constructor TMethodTableUpdateV4_2.Create(aUpdateNumber: TUpdateID);
begin
    inherited Create(aUpdateNumber, TMethodTableStructDefV4, INT_REVISION_4, INT_MINORREVISION_2);
    self.AlterStructure(TMethodTableStructDefV4);
    CopyMatchingFields([]);

    self.CustomDataFunc(MethodUpdateV4_2CustomFunc);
end;

function TMethodTableUpdateV4_2.GetUpdateDescription: string;
begin
    result := 'Convert Data loops';
end;

procedure TMethodTableUpdateV4_2.ParseParams(const aText: string; const aDelimiter: string;
    var vValues: TStringArray);
var
    xCount: integer;
    xPosDelim, xPosDelimNext: integer;
begin
    xPosDelim := 1;
    xCount := 0;
    SetLength(vValues, 0);
    while true do
    begin
        xPosDelimNext := PosEx(aDelimiter, aText, xPosDelim);
        if xPosDelimNext <= 0 then
        begin
            xPosDelimNext := Length(aText) + 1;
        end;
        SetLength(vValues, xCount + 1);
        vValues[xCount] := Trim(Copy(aText, xPosDelim, xPosDelimNext - xPosDelim));
        Inc(xCount);
        xPosDelim := xPosDelimNext + 1;
        if xPosDelim > Length(aText) then
        begin
            BREAK;
        end;
    end;
end;

function TMethodTableUpdateV4_2.FindCalcFuncBeginAndEndPos(const aFuncName, aOptionsText: string;
    aIsPosBeginAtOpenBracket: boolean; out oPosBegin, oPosEnd: integer): boolean;

var
    xBeginText, xEndText: string;
begin
    result := false;
    oPosBegin := 0;
    oPosEnd := 0;

    xBeginText := aFuncName + '(';
    xEndText := ')';
    oPosBegin := Pos(xBeginText, aOptionsText);
    if oPosBegin <= 0 then
        EXIT;
    if aIsPosBeginAtOpenBracket then
        oPosBegin := oPosBegin + Length(xBeginText);
    oPosEnd := PosEx(xEndText, aOptionsText, oPosBegin);
    if oPosEnd <= 0 then
        EXIT;
    result := true;
end;

procedure TMethodTableUpdateV4_2.ParseCalcFuncParams(const aFuncName, aOptionsText: string;
    var vValues: TStringArray);
var
    xParamsText: string;
    xPosBegin, xPosEnd: integer;
begin
    if not FindCalcFuncBeginAndEndPos(aFuncName, aOptionsText, true, xPosBegin, xPosEnd) then
        EXIT;

    xParamsText := Copy(aOptionsText, xPosBegin, xPosEnd - xPosBegin);
    ParseParams(xParamsText, ',', vValues);
end;

function TMethodTableUpdateV4_2.CreateDSOpenLine(const aObjectIdentName, aDefName, aFilter,
    aRecCountIdentName: string): TV4MethodLine;
begin
    result := TV4MethodLine.Create();
    result.Action := cActionNameDatasetOpen;
    result.Options := TCommonGUIParser.AddKeysAndValuesToOptions('', [cMethOptionKeyDatasetObjectIdentName,
        cMethOptionKeyDefName, cMethOptionKeyFilter, cMethOptionKeyRecordCountIdentName],
        [aObjectIdentName, aDefName, aFilter, aRecCountIdentName]);
end;

function TMethodTableUpdateV4_2.CreateDSReadLine(const aObjectValue: string): TV4MethodLine;
begin
    result := TV4MethodLine.Create();
    result.Action := cActionNameDatasetRead;
    result.Options := TCommonGUIParser.AddKeysAndValuesToOptions('', [cMethOptionKeyDatasetObject],
        [aObjectValue]);
end;

function TMethodTableUpdateV4_2.CreateDSCloseLine(const aObjectValue: string): TV4MethodLine;
begin
    result := TV4MethodLine.Create();
    result.Action := cActionNameDatasetClose;
    result.Options := TCommonGUIParser.AddKeysAndValuesToOptions('', [cMethOptionKeyDatasetObject],
        [aObjectValue]);
end;

function TMethodTableUpdateV4_2.CreateDSCursorMoveLine(const aObjectValue, aIsRelative, aMoveOffset,
    aRecNumIdentName: string): TV4MethodLine;
var
    xIsRelative: string;
begin
    if SameText(aIsRelative, '1') then
        xIsRelative := 'NO'
    else
        xIsRelative := 'YES';

    result := TV4MethodLine.Create();
    result.Action := cActionNameDatasetCursorMove;
    result.Options := TCommonGUIParser.AddKeysAndValuesToOptions('',
        [cMethOptionKeyDatasetObject, cMethOptionKeyIsRelativeMove, cMethOptionKeyMoveOffset,
        cMethOptionKeyRecordNumIdentName], [aObjectValue, xIsRelative, aMoveOffset, aRecNumIdentName]);
end;

function TMethodTableUpdateV4_2.ParseObjectIdentName(const aOptionsText: string;
    out oObjectIdentName: string): boolean;
var
    xPos: integer;
begin
    oObjectIdentName := '';
    xPos := Pos(':=', aOptionsText);
    result := xPos > 0;
    if not result then
        EXIT;
    oObjectIdentName := Trim(Copy(aOptionsText, 1, xPos - 1));
end;

procedure TMethodTableUpdateV4_2.ConvertDataOpenLine(const aMethLines: TObjectList<TV4MethodLine>;
    var vCurIndex: integer; var vMethodChanged: boolean);
var
    xOldMethLine, xNewMethLine: TV4MethodLine;
    xValues: TStringArray;
    xObjectIdentName: string;
begin
    xOldMethLine := aMethLines[vCurIndex] as TV4MethodLine;
    if not ParseObjectIdentName(xOldMethLine.Options, xObjectIdentName) then
        EXIT;

    ParseCalcFuncParams(cCalcFuncDataOpen, xOldMethLine.Options, xValues);
    if Length(xValues) <> 2 then
        EXIT;

    xNewMethLine := CreateDSOpenLine(xObjectIdentName, xValues[0], xValues[1],
        xObjectIdentName + cDataTotalLinesSuffix);
    xNewMethLine.Name := xOldMethLine.Name;

    aMethLines[vCurIndex] := xNewMethLine;
    vMethodChanged := true;
end;

procedure TMethodTableUpdateV4_2.ConvertDataReadLine(const aMethLines: TObjectList<TV4MethodLine>;
    var vCurIndex: integer; var vMethodChanged: boolean);
var
    xOldMethLine, xNewMethLine: TV4MethodLine;
    xValues: TStringArray;
begin
    xOldMethLine := aMethLines[vCurIndex] as TV4MethodLine;
    ParseCalcFuncParams(cCalcFuncDataRead, xOldMethLine.Options, xValues);
    if Length(xValues) <> 1 then
        EXIT;

    xNewMethLine := CreateDSReadLine(xValues[0]);
    xNewMethLine.Name := xOldMethLine.Name;

    aMethLines[vCurIndex] := xNewMethLine;
    vMethodChanged := true;
end;

procedure TMethodTableUpdateV4_2.ConvertDataCursorLine(const aMethLines: TObjectList<TV4MethodLine>;
    var vCurIndex: integer; var vMethodChanged: boolean);
var
    xOldMethLine, xNewMethLine: TV4MethodLine;
    xValues: TStringArray;
begin
    xOldMethLine := aMethLines[vCurIndex] as TV4MethodLine;
    ParseCalcFuncParams(cCalcFuncDataCursor, xOldMethLine.Options, xValues);
    if Length(xValues) <> 3 then
        EXIT;

    xNewMethLine := CreateDSCursorMoveLine(xValues[0], xValues[1], xValues[2],
        xValues[0] + cDataLineCountSuffix);
    xNewMethLine.Name := xOldMethLine.Name;

    aMethLines[vCurIndex] := xNewMethLine;
    vMethodChanged := true;
end;

function TMethodTableUpdateV4_2.FindMatchingEndWhileIndex(const aMethLines: TObjectList<TV4MethodLine>;
    const aCurIndex: integer): integer;
const
    cWhileToken = 'WHILE';
    cEndWhileToken = 'ENDWHILE';
var
    x: integer;
    xLoopCount: integer;
    xMethLine: TV4MethodLine;
begin
    result := -1;
    xLoopCount := 0;
    for x := aCurIndex + 1 to aMethLines.Count - 1 do
    begin
        xMethLine := aMethLines[x] as TV4MethodLine;

        if Pos(cEndWhileToken, UpperCase(xMethLine.Options)) > 0 then
        begin
            if xLoopCount = 0 then
            begin
                result := x;
                EXIT;
            end;
            Dec(xLoopCount);
        end
        else if Pos(cWhileToken, UpperCase(xMethLine.Options)) > 0 then
        begin
            Inc(xLoopCount);
        end;
    end;
end;

procedure TMethodTableUpdateV4_2.ConvertDataLoopLines(const aMethLines: TObjectList<TV4MethodLine>;
    var vCurIndex: integer; var vMethodChanged: boolean);
var
    xOldMethLine, xNewWhileMethLine, xInitLoopCountIdentLine, xDataCloseMethLine: TV4MethodLine;
    xValues: TStringArray;
    xObjectIdentName: string;
    xOptionsText: string;
    xNewWhileOptionsText: string;
    xEndWhileIndex: integer;
    xMethodName: string;
    xLoopCountIdentName: string;
    xPosBegin, xPosEnd: integer;
begin
    xOldMethLine := aMethLines[vCurIndex] as TV4MethodLine;
    xMethodName := xOldMethLine.Name;

    xOptionsText := xOldMethLine.Options;

    ParseCalcFuncParams(cCalcFuncDataNotEOF, xOptionsText, xValues);
    if Length(xValues) <> 1 then
        EXIT;
    xObjectIdentName := xValues[0];
    xLoopCountIdentName := xObjectIdentName + cDataLineCountSuffix;

    xNewWhileOptionsText := Format('( %s <= %s )',
        [xLoopCountIdentName, xObjectIdentName + cDataTotalLinesSuffix]);

    if not FindCalcFuncBeginAndEndPos(cCalcFuncDataNotEOF, xOptionsText, false, xPosBegin, xPosEnd) then
        EXIT;
    xNewWhileOptionsText := Copy(xOptionsText, 1, xPosBegin - 1) + xNewWhileOptionsText +
        Copy(xOptionsText, xPosEnd + 1, Length(xOptionsText));

    vMethodChanged := true;

    xNewWhileMethLine := TV4MethodLine.Create();
    xNewWhileMethLine.Action := cV0_ActionNameCalc;
    xNewWhileMethLine.Options := xNewWhileOptionsText;
    xNewWhileMethLine.Name := xMethodName;

    aMethLines[vCurIndex] := xNewWhileMethLine;

    // insert loop count ident initialization (eg _$0DataLineCnt := 1 )
    xInitLoopCountIdentLine := TV4MethodLine.Create();
    xInitLoopCountIdentLine.Action := cV0_ActionNameCalc;
    xInitLoopCountIdentLine.Options := Format('%s := 1', [xLoopCountIdentName]);
    xInitLoopCountIdentLine.Name := xMethodName;
    aMethLines.Insert(vCurIndex, xInitLoopCountIdentLine);
    // increment line count
    Inc(vCurIndex);

    // insert dataclose action after endwhile
    xEndWhileIndex := FindMatchingEndWhileIndex(aMethLines, vCurIndex);
    if xEndWhileIndex < 0 then
        EXIT;
    xDataCloseMethLine := CreateDSCloseLine(xObjectIdentName);
    xDataCloseMethLine.Name := xMethodName;
    aMethLines.Insert(xEndWhileIndex + 1, xDataCloseMethLine);

end;

procedure TMethodTableUpdateV4_2.MethodUpdateV4_2CustomFunc(aSender: TObject);

var
    xList: TStringList;
    i: integer;
    xCurIndex: integer;
    xMethodName: string;
    xMethLine: TV4MethodLine;
    xMethLines: TObjectList<TV4MethodLine>;
    xMethodChanged: boolean;
begin
    xList := TStringList.Create();
    TV4MethodUtils.GetMethodNames(fTableChangeAdaptor, '', xList);

    xMethLines := TObjectList<TV4MethodLine>.Create(true);
    try
        // change runrepeat to loop
        for i := 0 to xList.Count - 1 do
        begin
            xMethodName := xList[i];
            xMethLines.Clear();
            TV4MethodUtils.ReadMethodLines(fTableChangeAdaptor, xMethodName, xMethLines);
            xMethodChanged := false;
            xCurIndex := 0;
            while xCurIndex < xMethLines.Count do
            begin
                xMethLine := xMethLines[xCurIndex] as TV4MethodLine;
                if SameText(xMethLine.Action, cV0_ActionNameCalc) then
                begin
                    if (Pos(cCalcFuncDataOpen, xMethLine.Options) <> 0) then
                    begin
                        ConvertDataOpenLine(xMethLines, xCurIndex, xMethodChanged);
                    end
                    else if (Pos(cCalcFuncDataRead, xMethLine.Options) <> 0) then
                    begin
                        ConvertDataReadLine(xMethLines, xCurIndex, xMethodChanged);
                    end
                    else if (Pos(cCalcFuncDataCursor, xMethLine.Options) <> 0) then
                    begin
                        ConvertDataCursorLine(xMethLines, xCurIndex, xMethodChanged);
                    end
                    else if (Pos(cCalcFuncDataNotEof, xMethLine.Options) <> 0) then
                    begin
                        ConvertDataLoopLines(xMethLines, xCurIndex, xMethodChanged);
                    end;
                end;

                Inc(xCurIndex);

            end;

            if xMethodChanged then
                TV4MethodUtils.WriteMethodLines(fTableChangeAdaptor, xMethodName, xMethLines);

        end;
    finally
        xMethLines.Free;
    end;
end;

{ TMethodTableUpdateV4_3 }

constructor TMethodTableUpdateV4_3.Create(aUpdateNumber: TUpdateID);
begin
    inherited Create(aUpdateNumber, TMethodTableStructDefV4, INT_REVISION_4, INT_MINORREVISION_3);
    self.AlterStructure(TMethodTableStructDefV4);
    CopyMatchingFields([]);

    self.CustomDataFunc(MethodUpdateV4_3CustomFunc);
end;

function TMethodTableUpdateV4_3.GetUpdateDescription: string;
begin
    result := 'Replace old import';
end;

procedure TMethodTableUpdateV4_3.MethodUpdateV4_3CustomFunc1;
var
    xDP: TDataProvider;
    xAction: string;
begin
    xDP := fTableChangeAdaptor.CreateDestDataProvider();
    try
        xDP.SelectAndOpen('SELECT * FROM Method', false);
        try
            while not xDP.Eof do
            begin
                xAction := xDP.FieldByName(STR_METHOD_V0_FLD_ACTION).AsString;

                // auf 5 Buchstaben kürzen, um ältere Namesverlängerungen abzuschneiden
                if Length(xAction) > 5 then
                    xAction := Copy(xAction, 1, 5);

                // alle bisherigen Action-Namen einheitlich groß schreiben
                // (klein geschriebene Actions werden aber trotzdem zugelassen)
                xAction := UpperCase(xAction);

                // TRYSY-Action umbenennen (mit SameText, obwohl in diesem Fall nicht unbedigt notwendig)
                if SameText(xAction, 'TRYSY') then
                    xAction := 'TraySyXPipet';

                xDP.Edit;
                xDP.FieldByName(STR_METHOD_V0_FLD_ACTION).AsString := xAction;
                xDP.Post;
                xDP.Next;
            end;
        finally
            xDP.Close();
        end;
    finally
        xDP.Free;
    end;
end;

procedure TMethodTableUpdateV4_3.MethodUpdateV4_3CustomFunc2;
const
    cMethOptionKeyPrefix = 'IMPARR';
    cMethOptionKeyDefName = cMethOptionKeyPrefix + 'NAME';
    cMethOptionKeyFilter = cMethOptionKeyPrefix + 'FILTER';
    cMethOptionKeyOrderBy = cMethOptionKeyPrefix + 'ORDERBY';
    cMethOptionKeySourceFileName = cMethOptionKeyPrefix + 'SOURCE';
    cMethOptionKeyRecordCountIdentName = cMethOptionKeyPrefix + 'STORE';
    cNoOfVarName = '_$0NoOfLnesQ'; // etwas ungewöhnliche Namen, um Namensgleichheit zu vermeiden
    cIndexVarName = '_$0ImpIndxQ';
var
    xList: TStringList;
    i: integer;
    xCurIndex: integer;
    xMethodName: string;
    xMethLine: TV4MethodLine;
    xNewMethLine: TV4MethodLine;
    xMethLines: TObjectList<TV4MethodLine>;
    xMethodChanged: boolean;
    xImportOptions: TGenericImportOptions;
    xVarNames: TArray<string>;
    x: integer;
begin
    xList := TStringList.Create();
    TV4MethodUtils.GetMethodNames(fTableChangeAdaptor, '', xList);

    xMethLines := TObjectList<TV4MethodLine>.Create(true);
    try
        // change Import parameters to ImpArray action or loop
        for i := 0 to xList.Count - 1 do
        begin
            xMethodName := xList[i];
            xMethLines.Clear();
            TV4MethodUtils.ReadMethodLines(fTableChangeAdaptor, xMethodName, xMethLines);
            xMethodChanged := false;
            xCurIndex := 0;
            while xCurIndex < xMethLines.Count do
            begin
                xMethLine := xMethLines[xCurIndex] as TV4MethodLine;
                xImportOptions := TMethodGUIParser.GenericImportOptionsFromStr(xMethLine.Options);
                if (xImportOptions.DefName <> '') then
                begin
                    xVarNames := GetVarNames(xImportOptions.DefName);

                    // insert ImpArray
                    xNewMethLine := TV4MethodLine.Create();
                    xNewMethLine.Name := xMethodName;
                    xNewMethLine.Action := 'ImpArray';
                    xNewMethLine.Options := TCommonGUIParser.AddKeysAndValuesToOptions('',
                        [cMethOptionKeyDefName, cMethOptionKeyFilter, cMethOptionKeySourceFileName,
                        cMethOptionKeyRecordCountIdentName], [xImportOptions.DefName, xImportOptions.Filter,
                        xImportOptions.SourceFileName, cNoOfVarName]);
                    xMethLines.Insert(xCurIndex, xNewMethLine);
                    Inc(xCurIndex);

                    if SameText(xImportOptions.AllRecs, 'YES') then
                    begin

                        // insert Store
                        xNewMethLine := TV4MethodLine.Create();
                        xNewMethLine.Name := xMethodName;
                        xNewMethLine.Action := 'STORE';
                        xNewMethLine.Options := TCommonGUIParser.AddKeysAndValuesToOptions('',
                            ['STOREKEY', 'STOREVALUE'], [cIndexVarName, '1']);
                        xMethLines.Insert(xCurIndex, xNewMethLine);
                        Inc(xCurIndex);

                        // insert while
                        xNewMethLine := TV4MethodLine.Create();
                        xNewMethLine.Name := xMethodName;
                        xNewMethLine.Action := 'WHILE';
                        xNewMethLine.Options := TCommonGUIParser.AddKeysAndValuesToOptions('', ['CONDITION'],
                            ['(' + cIndexVarName + ' <= ' + cNoOfVarName + ')']);
                        xMethLines.Insert(xCurIndex, xNewMethLine);
                        Inc(xCurIndex);

                        // Varaiblen ändern zu Array-Variablen
                        for x := 0 to high(xVarNames) do
                        begin
                            xMethLine.Options := StringReplace(xMethLine.Options, xVarNames[x],
                                TImportColDefUtils.ReplaceGlobalVarName(xVarNames[x]) + '[' + cIndexVarName +
                                ']', [rfReplaceAll, rfIgnoreCase]);
                        end;

                        // insert next
                        xNewMethLine := TV4MethodLine.Create();
                        xNewMethLine.Name := xMethodName;
                        xNewMethLine.Action := 'STORE';
                        xNewMethLine.Options := TCommonGUIParser.AddKeysAndValuesToOptions('',
                            ['STOREKEY', 'STOREVALUE'], [cIndexVarName, cIndexVarName + ' + 1']);
                        xMethLines.Insert(xCurIndex + 1, xNewMethLine);
                        Inc(xCurIndex);

                        // insert endwhile
                        xNewMethLine := TV4MethodLine.Create();
                        xNewMethLine.Name := xMethodName;
                        xNewMethLine.Action := 'WHEND';
                        xNewMethLine.Options := '';
                        xMethLines.Insert(xCurIndex + 1, xNewMethLine);
                        Inc(xCurIndex);
                    end
                    else
                    begin
                        // Varaiblen ändern zu Array-Variablen
                        for x := 0 to high(xVarNames) do
                        begin
                            xMethLine.Options := StringReplace(xMethLine.Options, xVarNames[x],
                                TImportColDefUtils.ReplaceGlobalVarName(xVarNames[x]) + '[0]',
                                [rfReplaceAll, rfIgnoreCase]);
                        end;
                    end;
                    xMethodChanged := true;
                end;

                Inc(xCurIndex);
            end;

            if xMethodChanged then
                TV4MethodUtils.WriteMethodLines(fTableChangeAdaptor, xMethodName, xMethLines);

        end;
    finally
        xMethLines.Free;
    end;
end;

function TMethodTableUpdateV4_3.GetVarNames(const aImpDefName: string): TArray<string>;
var
    xDP: TDataProvider;
    x: integer;
begin
    xDP := fTableChangeAdaptor.CreateSourceDataProvider();
    try
        xDP.SelectAndOpen('SELECT * FROM ImpCols where IMPORTDEFNAME = ''' + aImpDefName + '''', false);
        try
            SetLength(result, xDP.RecordCount);
            x := 0;
            while not xDP.Eof do
            begin
                try
                    result[x] := xDP.FieldByName('TARGETCOL').AsString;
                    inc(x);
                finally
                    xDP.Next;
                end;
            end;
        finally
            xDP.Close();
        end;
    finally
        xDP.Free;
    end;
end;

procedure TMethodTableUpdateV4_3.MethodUpdateV4_3CustomFunc(aSender: TObject);
begin
    MethodUpdateV4_3CustomFunc1;
    MethodUpdateV4_3CustomFunc2;
end;

// Umbenennungen von Action-Namen immer mit SAMETEXT(), da Kleinschreibung immer noch erlaubt ist!

constructor TMethodTableUpdateV4_4.Create(aUpdateNumber: TUpdateID);
begin
    inherited Create(aUpdateNumber, TMethodTableStructDefV4, INT_REVISION_4, INT_MINORREVISION_5);
    self.AlterStructure(TMethodTableStructDefV4);
    CopyMatchingFields([]);

    self.CustomDataFunc(MethodUpdateV4_4CustomFunc);
end;

function TMethodTableUpdateV4_4.ConvertCalcToStore(const aOldOptions: string; out oNewOptions: string;
    out oNewAction: string): boolean;
const
    cAssignmentToken = ':=';
var
    xKey, xValue: string;
begin
    oNewOptions := '';
    oNewAction := '';
    // check for assignment :=
    result := TCommonGUIParser.SplitStr(aOldOptions, cAssignmentToken, xKey, xValue, true);
    if not result then
        EXIT;

    oNewAction := 'STORE';
    xKey := Trim(xKey); // wichtig: keine Leerzeichen vorne oder hinten!
    xValue := Trim(xValue);
    oNewOptions := TCommonGUIParser.AddKeysAndValuesToOptions('', ['STOREKEY', 'STOREVALUE'], [xKey, xValue]);
end;

function TMethodTableUpdateV4_4.ConvertCalcToWhile(const aOldOptions: string; out oNewOptions: string;
    out oNewAction: string): boolean;
const
    cWhileToken = 'WHILE';
var
    xDummy, xCondition: string;
begin
    oNewOptions := '';
    oNewAction := '';

    result := TCommonGUIParser.SplitStr(aOldOptions, cWhileToken, xDummy, xCondition, false);
    if not result then
        EXIT;

    oNewAction := 'WHILE';
    oNewOptions := TCommonGUIParser.AddKeysAndValuesToOptions('', ['CONDITION'], [xCondition]);
    // Format( 'CONDITION=%s', [ xCondition ] );
end;

function TMethodTableUpdateV4_4.ConvertCalcToEndWhile(const aOldOptions: string; out oNewOptions: string;
    out oNewAction: string): boolean;
const
    cEndWhileToken = 'ENDWHILE';
var
    xDummy1, xDummy2: string;
begin
    oNewOptions := '';
    oNewAction := '';

    result := TCommonGUIParser.SplitStr(aOldOptions, cEndWhileToken, xDummy1, xDummy2, false);
    if not result then
        EXIT;

    oNewAction := 'WHEND';
    oNewOptions := '';
end;

function TMethodTableUpdateV4_4.ConvertCalcToEndIf(const aOldOptions: string; out oNewOptions: string;
    out oNewAction: string): boolean;
const
    cEndIfToken = 'ENDIF';
var
    xDummy1, xDummy2: string;
begin
    oNewOptions := '';
    oNewAction := '';

    result := TCommonGUIParser.SplitStr(aOldOptions, cEndIfToken, xDummy1, xDummy2, false);
    if not result then
        EXIT;

    oNewAction := 'IFEND';
    oNewOptions := '';
end;

function TMethodTableUpdateV4_4.ConvertCalcToIf(const aOldOptions: string; out oNewOptions: string;
    out oNewAction: string): boolean;
const
    cIfToken = 'IF';
var
    xDummy, xCondition: string;
begin
    oNewOptions := '';
    oNewAction := '';

    result := TCommonGUIParser.SplitStr(aOldOptions, cIfToken, xDummy, xCondition, false);
    if not result then
        EXIT;

    oNewAction := 'IF';
    oNewOptions := TCommonGUIParser.AddKeysAndValuesToOptions('', ['CONDITION'], [xCondition]);
    // Format( 'CONDITION=%s', [ xCondition ] );
end;

procedure TMethodTableUpdateV4_4.ConvertCalcToRemark(const aOldOptions: string; out oNewOptions: string;
    out oNewAction: string);
begin
    oNewOptions := aOldOptions;
    oNewAction := 'REMAR';
end;

procedure TMethodTableUpdateV4_4.ConvertCalcAction(const aOldOptions: string; out oNewOptions: string;
    out oNewAction: string);
begin
    oNewOptions := '';
    oNewAction := '';

    if ConvertCalcToStore(aOldOptions, oNewOptions, oNewAction) then
        EXIT;
    if ConvertCalcToEndIf(aOldOptions, oNewOptions, oNewAction) then
        EXIT;
    if ConvertCalcToEndWhile(aOldOptions, oNewOptions, oNewAction) then
        EXIT;
    if ConvertCalcToIf(aOldOptions, oNewOptions, oNewAction) then
        EXIT;
    if ConvertCalcToWhile(aOldOptions, oNewOptions, oNewAction) then
        EXIT;

    // we dont know what this is.  Just make it into a remark.
    // Sometimes the CALC action was misused to decalare the order of variables being asked from the user.  So we can use a remark here
    ConvertCalcToRemark(aOldOptions, oNewOptions, oNewAction);
end;

procedure TMethodTableUpdateV4_4.MethodUpdateV4_4CustomFunc(aSender: TObject);
var
    xDP: TDataProvider;
    xOldAction, xNewAction, xOldOptions, xNewOptions: string;
begin
    xDP := fTableChangeAdaptor.CreateDestDataProvider();
    try
        xDP.SelectAndOpen('SELECT * FROM Method', false);
        try
            while not xDP.Eof do
            begin
                try
                    xOldAction := xDP.FieldByName(STR_METHOD_V0_FLD_ACTION).AsString;
                    if not SameText(xOldAction, 'CALC') then
                        CONTINUE;
                    xOldOptions := xDP.FieldByName(STR_METHOD_V3_FLD_AOPTIONS).AsString;
                    ConvertCalcAction(xOldOptions, xNewOptions, xNewAction);

                    xDP.Edit;
                    xDP.FieldByName(STR_METHOD_V0_FLD_ACTION).AsString := xNewAction;
                    xDP.FieldByName(STR_METHOD_V3_FLD_AOPTIONS).AsString := xNewOptions;
                    xDP.Post;
                finally
                    xDP.Next;
                end;
            end;
        finally
            xDP.Close();
        end;
    finally
        xDP.Free;
    end;
end;

{ TMethodTableUpdateV4_5 }

constructor TMethodTableUpdateV4_5.Create(aUpdateNumber: TUpdateID);
begin
    inherited Create(aUpdateNumber, TMethodTableStructDefV4, INT_REVISION_4, INT_MINORREVISION_5);
    self.AlterStructure(TMethodTableStructDefV4);
    CopyMatchingFields([]);

    self.CustomDataFunc(MethodUpdateV4_5CustomFunc);
end;

function TMethodTableUpdateV4_5.IsOddCommentLong(const aComment: string): boolean;
var
    xLong: string;
begin
    if (aComment = '') then
        EXIT(false);

    xLong := Copy(aComment, 1, 26);

    if (xLong = 'This action adds an existi') or (xLong = 'This action aspirates liqu') or
        (xLong = 'This step contains calcula') or (xLong = 'This action checks the tem') or
        (xLong = 'This action runs a command') or (xLong = 'This action copies a file.') or
        (xLong = 'This action writes the val') or (xLong = 'This action stops the run ') or
        (xLong = 'This action switches a dev') or (xLong = 'This action dispenses liqu') or
        (xLong = 'This action calls a DLL fu') or (xLong = 'This action fills the disp') or
        (xLong = 'This action moves the tips') or (xLong = 'This action gets a tool fr') or
        (xLong = 'This action gets tips from') or (xLong = 'This action imports ... .') or
        (xLong = 'This action initializes al') or (xLong = 'This action stores a manua') or
        (xLong = 'This action displays a mes') or (xLong = 'This action makes a single') or
        (xLong = 'This action moves all Z-mo') or (xLong = 'This action moves a rack t') or
        (xLong = 'This action moves a tube w') or (xLong = 'This action pipettes a vol') or
        (xLong = 'This action moves a tube a') or (xLong = 'This is just a comment in ') or
        (xLong = 'This action puts back a to') or (xLong = 'This action defines the te') or
        (xLong = 'This action sets the vorte') or (xLong = 'This action defines the sp') or
        (xLong = 'This action carries out a ') or (xLong = 'This action write an expor') or
        (xLong = 'This action starts a timer') or (xLong = 'This action starts a run.') or
        (xLong = 'This action stores a metho') or (xLong = 'This action tares the bala') or
        (xLong = 'This action moves a rack o') or (xLong = 'This action waits until th') or
        (xLong = 'This action weighs a compl') or (xLong = 'This action moves a tube a') or
        (xLong = 'This action moves one or m') or (xLong = 'This action moves one or m') or
        (xLong = 'This action pipettes a vol') or (xLong = 'Number of values') or
        (xLong = 'This action loads a run.') then
        EXIT(true);

    if (xLong = 'Diese Aktion zeigt ein Mel') or (xLong = 'Diese Aktion hält den Lauf') or
        (xLong = 'Diese Aktion ruft eine DLL') or (xLong = 'Diese Aktion bewegt die Na') or
        (xLong = 'Diese Aktion führt eine In') or (xLong = 'Diese Aktion führt ein Com') or
        (xLong = 'Diese Aktion bewegt alle Z') or (xLong = 'Diese Aktion füllt die Ein') or
        (xLong = 'Diese Aktion führt einen W') or (xLong = 'Diese Aktion speichert ein') or
        (xLong = 'Diese Aktion löscht einen ') or (xLong = 'Diese Aktion führt ein SQL') or
        (xLong = 'Diese Aktion schreibt eine') or (xLong = 'Diese Aktion importiert ..') or
        (xLong = 'Diese Aktion schaltet ein ') or (xLong = 'Diese Aktion schaltet ein ') or
        (xLong = 'Diese Aktion schaltet alle') or (xLong = 'Diese Aktion prüft die Tem') or
        (xLong = 'Diese Aktion definiert die') or (xLong = 'Diese Aktion schaltet die ') or
        (xLong = 'Diese Aktion kopiert eine ') or (xLong = 'Diese Aktion tariert die W') or
        (xLong = 'Diese Aktion startet das E') or (xLong = 'Dies ist nur ein Kommentar') or
        (xLong = 'Diese Aktion wartet auf ei') or (xLong = 'Diese Aktion startet einen') or
        (xLong = 'Diese Aktion zeigt eine Me') or (xLong = 'Diese Aktion bringt ein Gr') or
        (xLong = 'Diese Aktion aktualisiert ') or (xLong = 'Diese Aktion erzeugt einen') or
        (xLong = 'Diese Aktion bewegt ein Ra') or (xLong = 'Diese Aktion liest den Bar') or
        (xLong = 'Diese Aktion speichert ein') or (xLong = 'Diese Aktion löscht einen ') or
        (xLong = 'Diese Aktion lädt einen Ru') or (xLong = 'Diese Aktion startet einen') or
        (xLong = 'Diese Aktion startet ein W') or (xLong = 'Diese Aktion fügt eine exi') or
        (xLong = 'Dieser Schritt enthält Rec') or (xLong = 'Diese Aktion schaltet eine') or
        (xLong = 'Diese Aktion bewegt ein Rö') or (xLong = 'Diese Aktion nimmt die Röh') or
        (xLong = 'Diese Aktion pipettiert ei') or (xLong = 'Diese Aktion wiegt ein kom') or
        (xLong = 'Diese Aktion speichert Kap') or (xLong = 'Diese Aktion bewegt ein Ra') or
        (xLong = 'Diese Aktion speichert ein') or (xLong = 'Diese Aktion nimmt Flüssig') or
        (xLong = 'Diese Aktion speichert gib') or (xLong = 'Diese Aktion wartet bis di') or
        (xLong = 'Diese Aktion nimmt ein Gre') or (xLong = 'Diese Aktion nimmt Wechsel') or
        (xLong = 'Diese Aktion stellt Wechse') or (xLong = 'Diese Aktion bewegt eine o') or
        (xLong = 'Diese Aktion ändert den Sp') or (xLong = 'Diese Aktion schreibt die ') or
        (xLong = 'Auf das Ende der Einwaage ') or (xLong = 'Diese Aktion führt eine ei') or
        (xLong = 'Diese Aktion fragt einen S') then
        EXIT(true);

    EXIT(false);
end;

function TMethodTableUpdateV4_5.IsOddCommentShort(const aComment: string): boolean;
begin
    if (aComment = '') then
        EXIT(false);

    if (aComment = 'Add method') or (aComment = 'Aspirate sample') or (aComment = 'Calculation') or
        (aComment = 'Check Temperature') or (aComment = 'Command Macro') or (aComment = 'Copy file') or
        (aComment = 'Define Variables with SQL') or (aComment = 'Delay') or (aComment = 'Device action') or
        (aComment = 'Dispense') or (aComment = 'DLL Call') or (aComment = 'Fill disposable tip racks') then
        EXIT(true);
    if (aComment = 'Fill rack with caps') or (aComment = 'Flush') or (aComment = 'Get gripper tool') or
        (aComment = 'Get tips') or (aComment = 'Import into table') or (aComment = 'Init') or
        (aComment = 'Initialize device') or (aComment = 'Load run') or (aComment = 'Manual Fill') or
        (aComment = 'Message box') or (aComment = 'Message with beeper') or (aComment = 'Motor movement') or
        (aComment = 'Move to z-Travel') then
        EXIT(true);
    if (aComment = 'Move Rack') or (aComment = 'Move Tube') or (aComment = 'Pipetting action') or
        (aComment = 'Read and weigh tube') or (aComment = 'Read rack barcode') or
        (aComment = 'Read Tube barcode') or (aComment = 'Refresh Layout') or (aComment = 'Remark') or
        (aComment = 'Return gripper tool') or (aComment = 'Set Temperature') or
        (aComment = 'Set Vortexer Fixation') or (aComment = 'Set Vortexer Speed') or (aComment = 'SQL Update')
        or (aComment = 'SQL Write') then
        EXIT(true);
    if (aComment = 'Start Timer') or (aComment = 'Start run') or (aComment = 'Store method parameter') or
        (aComment = 'Tare Balance') or (aComment = 'Virtual rack movement') or (aComment = 'Wait For Balance')
        or (aComment = 'Wait for timer') or (aComment = 'Wash program') or (aComment = 'Weigh rack position')
        or (aComment = 'Weigh tube') or (aComment = 'XY movement to rack position') or
        (aComment = 'Z movement at current position') then
        EXIT(true);

    if (aComment = 'Methode hinzufügen') or (aComment = 'Probenaufnahme') or (aComment = 'Rechnung') or
        (aComment = 'Temperatur überprüfen') or (aComment = 'Command-Makro') or (aComment = 'Datei kopieren')
        or (aComment = 'Variablen definieren mit SQL') or (aComment = 'Wartezeit') or
        (aComment = 'Device-Aktion') or (aComment = 'Abgabe') or (aComment = 'DLL-Aufruf') or
        (aComment = 'Einwegspitzen-Racks füllen') then
        EXIT(true);
    if (aComment = 'Rack mit Kappen füllen') or (aComment = 'Spülen') or (aComment = 'Greifer-Tool aufnehmen')
        or (aComment = 'Sptizen aufnehmen') or (aComment = 'Import in eine Tabelle') or
        (aComment = 'Initialisieren') or (aComment = 'Device initialisieren') or (aComment = 'Run laden') or
        (aComment = 'Manuelles Füllen') or (aComment = 'Meldung') or
        (aComment = 'Meldung mit akustischen Signal') or (aComment = 'Motor-Bewegung') or
        (aComment = 'Zu Z-Travel bewegen') then
        EXIT(true);
    if (aComment = 'Rack bewegen') or (aComment = 'Tube bewegen') or (aComment = 'Pipettieraktion') or
        (aComment = 'Tube lesen und wiegen') or (aComment = 'Rack-Barcode lesen') or
        (aComment = 'Tube-Barcode lesen') or (aComment = 'Layout aktualisieren') or (aComment = 'Bemerkung')
        or (aComment = 'Greifer-Tool zurückbringen') or (aComment = 'Temperatur einstellen') or
        (aComment = 'Vortexer-Fixierung schalten') or (aComment = 'Vortexer-Geschwindigkeit festlegen') or
        (aComment = 'SQL Update') or (aComment = 'SQL Write') then
        EXIT(true);
    if (aComment = 'Timer starten') or (aComment = 'Run starten') or
        (aComment = 'Methoden-Parameter speichern') or (aComment = 'Waage tarieren') or
        (aComment = 'Virtuelle Rackbewegung') or (aComment = 'Warten auf die Waage') or
        (aComment = 'Warte auf den Timer') or (aComment = 'Waschprogramm') or
        (aComment = 'Rackposition wiegen') or (aComment = 'Tube wiegen') or
        (aComment = 'XY-Bewegung zu einer Rackposition') or
        (aComment = 'Z-Bewegung über aktueller Rackposition') then
        EXIT(true);

    EXIT(false);
end;

procedure TMethodTableUpdateV4_5.MethodUpdateV4_5CustomFunc(aSender: TObject);
var
    xDP: TDataProvider;
    xOldComments, xNewComment: string;
    xCommentShort, xCommentLong: string;
begin
    xDP := fTableChangeAdaptor.CreateDestDataProvider();
    try
        xDP.SelectAndOpen('SELECT * FROM Method', false);
        try
            while not xDP.Eof do
            begin
                try
                    xOldComments := xDP.FieldByName(STR_METHOD_V0_FLD_COMMENT).AsString;
                    TCommonGUIParser.CommentsFromStr(xOldComments, xCommentShort, xCommentLong);

                    if self.IsOddCommentLong(xCommentLong) then
                        xCommentLong := '';
                    if self.IsOddCommentShort(xCommentShort) then
                        xCommentShort := '';

                    xNewComment := xCommentShort;
                    if xCommentLong <> '' then
                    begin
                        if xNewComment <> '' then
                            xNewComment := xNewComment + #13#10;

                        xNewComment := xNewComment + xCommentLong;
                    end;

                    xDP.Edit;
                    xDP.FieldByName(STR_METHOD_V0_FLD_COMMENT).AsString := xNewComment;
                    xDP.Post;
                finally
                    xDP.Next;
                end;
            end;
        finally
            xDP.Close();
        end;
    finally
        xDP.Free;
    end;
end;

{ TMethodTableUpdateV4_6 }

constructor TMethodTableUpdateV4_6.Create(aUpdateNumber: TUpdateID);
begin
    inherited Create(aUpdateNumber, TMethodTableStructDefV4, INT_REVISION_4, INT_MINORREVISION_6);
    self.AlterStructure(TMethodTableStructDefV4);
    CopyMatchingFields([]);

    self.CustomDataFunc(MethodUpdateCustomFunc);
end;

procedure TMethodTableUpdateV4_6.MethodUpdateCustomFunc(aSender: TObject);
var
    xDP: TDataProvider;
    xAction, xOptions, xComment: string;
begin
    xDP := fTableChangeAdaptor.CreateDestDataProvider();
    try
        xDP.SelectAndOpen('SELECT * FROM Method', false);
        try
            while not xDP.Eof do
            begin
                try
                    xAction := xDP.FieldByName(STR_METHOD_V0_FLD_ACTION).AsString;
                    xComment := xDP.FieldByName(STR_METHOD_V0_FLD_COMMENT).AsString;
                    xOptions := xDP.FieldByName(STR_METHOD_V3_FLD_AOPTIONS).AsString;
                    if not SameText(xAction, 'REMAR') then
                        CONTINUE;

                    if (POS('_', xOptions) > 0) or (xComment <> '') then
                    begin
                        // REMARK-Action mit Variablen oder Kommentar mit REMTEXT versehen
                        xOptions := TCommonGUIParser.AddKeysAndValuesToOptions('', ['REMTEXT'], [xOptions]);
                    end
                    else
                    begin
                        // REMARK-Action ist unnütz, wenn keine Variablen drin vorkommen
                        xAction := 'BLANK';
                        xComment := xOptions;
                        xOptions := '';
                    end;

                    xDP.Edit;
                    xDP.FieldByName(STR_METHOD_V0_FLD_ACTION).AsString := xAction;
                    xDP.FieldByName(STR_METHOD_V3_FLD_AOPTIONS).AsString := xOptions;
                    xDP.FieldByName(STR_METHOD_V0_FLD_COMMENT).AsString := xComment;
                    xDP.Post;
                finally
                    xDP.Next;
                end;
            end;
        finally
            xDP.Close();
        end;
    finally
        xDP.Free;
    end;
end;

{ TMethodTableUpdateV4_7 }

constructor TMethodTableUpdateV4_7.Create(aUpdateNumber: TUpdateID);
begin
    inherited Create(aUpdateNumber, TMethodTableStructDefV4, INT_REVISION_4, INT_MINORREVISION_7);
    self.AlterStructure(TMethodTableStructDefV4);
    CopyMatchingFields([]);

    self.CustomDataFunc(MethodUpdateCustomFunc);
end;

procedure TMethodTableUpdateV4_7.MethodUpdateCustomFunc(aSender: TObject);
const
    cMethOptionKeyPrefix = 'STORE';
    cMethOptionKeyStoreKey = cMethOptionKeyPrefix + 'KEY';
    cMethOptionKeyStoreValue = cMethOptionKeyPrefix + 'VALUE';
    cMethOptionKeyArrayIndex = cMethOptionKeyPrefix + 'ARRAYINDEX';
var
    xDP: TDataProvider;
    xAction, xOptionsText: string;
    xValues: TStringArray;
begin
    xDP := fTableChangeAdaptor.CreateDestDataProvider();
    try
        xDP.SelectAndOpen('SELECT * FROM Method', false);
        try
            while not xDP.Eof do
            begin
                try
                    // Suchen nach Store-Action
                    xAction := xDP.FieldByName(STR_METHOD_V0_FLD_ACTION).AsString;
                    if not SameText(xAction, 'STORE') then
                        CONTINUE;

                    // es gibt einen ArrayIndex
                    xOptionsText := xDP.FieldByName(STR_METHOD_V3_FLD_AOPTIONS).AsString;
                    if not TCommonGUIParser.FindValuesForKeysInOptions(xOptionsText, xValues,
                        [cMethOptionKeyArrayIndex]) then
                        CONTINUE;

                    TCommonGUIParser.FindValuesForKeysInOptions(xOptionsText, xValues,
                        [cMethOptionKeyStoreKey, cMethOptionKeyStoreValue, cMethOptionKeyArrayIndex]);

                    // Neue Schreibweise: Variable[ArrayIndex]
                    xOptionsText := TCommonGUIParser.AddKeysAndValuesToOptions('',
                        [cMethOptionKeyStoreKey, cMethOptionKeyStoreValue],
                        [xValues[0] + '[' + xValues[2] + ']', xValues[1]]);

                    xDP.Edit;
                    xDP.FieldByName(STR_METHOD_V3_FLD_AOPTIONS).AsString := xOptionsText;
                    xDP.Post;
                finally
                    xDP.Next;
                end;
            end;
        finally
            xDP.Close();
        end;
    finally
        xDP.Free;
    end;
end;

{ TMethodTableUpdateV4_8 }

constructor TMethodTableUpdateV4_8.Create(aUpdateNumber: TUpdateID);
begin
    inherited Create(aUpdateNumber, TMethodTableStructDefV4, INT_REVISION_4, INT_MINORREVISION_8);
    self.AlterStructure(TMethodTableStructDefV4);
    CopyMatchingFields([]);

    self.CustomDataFunc(MethodUpdateCustomFunc);
end;

procedure TMethodTableUpdateV4_8.MethodUpdateCustomFunc(aSender: TObject);
const
    cMethOptionKeyPrefix = 'FLUSH';
    cMethOptionKeyPipDevice = cMethOptionKeyPrefix + 'PIPDEV';
    cMethOptionKeyTipMap = cMethOptionKeyPrefix + 'TIPMAP';
    cMethOptionKeyDiluent = 'DILUENT';
    cMethOptionKeyCycles = cMethOptionKeyPrefix + 'CYCLES';
    cMethOptionKeyVolume = cMethOptionKeyPrefix + 'VOLUME';
    cMethOptionKeyPumpNumber = cMethOptionKeyPrefix + 'CHAN';
    cMethOptionKeyUseAllPumps = cMethOptionKeyPrefix + 'USEALL';
    cMethOptionKeyUsePeripump = cMethOptionKeyPrefix + 'PERI';
    cMethOptionKeyDryAfterWash = cMethOptionKeyPrefix + 'DRY';
var
    xDP: TDataProvider;
    xAction, xOptionsText: string;
    xValues: TStringArray;
begin
    xDP := fTableChangeAdaptor.CreateDestDataProvider();
    try
        xDP.SelectAndOpen('SELECT * FROM Method', false);
        try
            while not xDP.Eof do
            begin
                try
                    // Suchen nach FLUSH-Action
                    xAction := xDP.FieldByName(STR_METHOD_V0_FLD_ACTION).AsString;
                    if not SameText(xAction, 'FLUSH') then
                        CONTINUE;

                    xOptionsText := xDP.FieldByName(STR_METHOD_V3_FLD_AOPTIONS).AsString;
                    TCommonGUIParser.FindValuesForKeysInOptions(xOptionsText, xValues,
                        [cMethOptionKeyPipDevice, cMethOptionKeyTipMap, cMethOptionKeyDiluent,
                        cMethOptionKeyCycles, cMethOptionKeyVolume, cMethOptionKeyPumpNumber,
                        cMethOptionKeyUseAllPumps, cMethOptionKeyUsePeripump, cMethOptionKeyDryAfterWash]);

                    if ((xValues[3] <> '') and (xValues[3] <> '0')) and (xValues[4] <> '') then
                    begin
                        xValues[4] := '';
                        // Wenn Flush-Cycles <> 0, dann entferne die Volumen-Angabe

                        xOptionsText := TCommonGUIParser.AddKeysAndValuesToOptions('',
                            [cMethOptionKeyPipDevice, cMethOptionKeyTipMap, cMethOptionKeyDiluent,
                            cMethOptionKeyCycles, cMethOptionKeyVolume, cMethOptionKeyPumpNumber,
                            cMethOptionKeyUseAllPumps, cMethOptionKeyUsePeripump,
                            cMethOptionKeyDryAfterWash], xValues);

                        xDP.Edit;
                        xDP.FieldByName(STR_METHOD_V3_FLD_AOPTIONS).AsString := xOptionsText;
                        xDP.Post;
                    end;
                finally
                    xDP.Next;
                end;
            end;
        finally
            xDP.Close();
        end;
    finally
        xDP.Free;
    end;
end;

{ TMethodTableUpdateV4_9 }

constructor TMethodTableUpdateV4_9.Create(aUpdateNumber: TUpdateID);
begin
    inherited Create(aUpdateNumber, TMethodTableStructDefV4, INT_REVISION_4, INT_MINORREVISION_9);
    self.AlterStructure(TMethodTableStructDefV4);
    CopyMatchingFields([]);

    self.CustomDataFunc(MethodUpdateCustomFunc);
end;

procedure TMethodTableUpdateV4_9.MethodUpdateCustomFunc(aSender: TObject);
const
    cMultiPipStrategyNone = 'None';
    cMultiPipStrategyLiquid = 'Liquid';
    cMultiPipDestBasic = 'Destination_Basic';

    cMethOptionKeyMultiPip = 'MultiPip';
    cMethOptionKeyMultiPipStrategy = 'MultiPipStrategy';
    cMethOptionKeyUtilizeAllTipsEqually = 'UtilizeAllTipsEqually';
    cMethOptionKeyMultiPipGroupCriteria = 'MultiPipGroupCriteria';
    cMethOptionKeyMultiPipOrderCriteria = 'MultiPipOrderCriteria';

    cMethOptionKeyMultiPipEnabled = 'Enabled';
    cMethOptionKeyMultiPipTipScattering = 'TipScattering';
    cMethOptionKeyMultiPipStepShift = 'StepShift';
    cMethOptionKeyMultiPipSourceCriteria = 'SourceCriteria';
    cMethOptionKeyMultiPipDestCriteria = 'DestCriteria';

    cTipScatteringNone = 'None';
    cTipScatteringStandard = 'Standard';

    cStepShiftNoShift = 'NoShift';
    cStepShiftReduceSourceSteps = 'ReduceSourceSteps';
var
    xDP: TDataProvider;
    xAction, xOptionsText: string;
    xAllValues, xOldValues, xNewValues: TStringArray;
begin
    xDP := fTableChangeAdaptor.CreateDestDataProvider();
    try
        xDP.SelectAndOpen('SELECT * FROM Method', false);
        try
            while not xDP.Eof do
            begin
                try
                    // Suchen nach FLUSH-Action
                    xAction := xDP.FieldByName(STR_METHOD_V0_FLD_ACTION).AsString;
                    if not(SameText(xAction, 'PIPBB') or SameText(xAction, 'PListRework')) then
                        CONTINUE;

                    xOptionsText := xDP.FieldByName(STR_METHOD_V3_FLD_AOPTIONS).AsString;
                    TCommonGUIParser.FindValuesForKeysInOptions(xOptionsText, xAllValues,
                        [cMethOptionKeyMultiPip]);
                    TCommonGUIParser.FindValuesForKeysInOptions(xAllValues[0], xOldValues,
                        [cMethOptionKeyMultiPipStrategy, cMethOptionKeyUtilizeAllTipsEqually,
                        cMethOptionKeyMultiPipGroupCriteria, cMethOptionKeyMultiPipOrderCriteria]);

                    SetLength(xNewValues, 5);
                    xNewValues[0] := '';
                    xNewValues[1] := '';
                    xNewValues[2] := '';
                    xNewValues[3] := xOldValues[2];
                    xNewValues[4] := xOldValues[3];
                    if (xOldValues[0] = cMultiPipDestBasic) then
                    begin
                        xNewValues[0] := 'YES';
                        xNewValues[1] := cTipScatteringStandard;
                    end;
                    if (xOldValues[0] = cMultiPipStrategyLiquid) then
                        xNewValues[0] := 'YES';
                    if (xOldValues[1] = 'YES') then
                        xNewValues[2] := 'ReduceSourceSteps';

                    xAllValues[0] := TCommonGUIParser.AddKeysAndValuesToOptions('',
                        [cMethOptionKeyMultiPipEnabled, cMethOptionKeyMultiPipTipScattering,
                        cMethOptionKeyMultiPipStepShift, cMethOptionKeyMultiPipSourceCriteria,
                        cMethOptionKeyMultiPipDestCriteria], xNewValues);

                    xOptionsText := TCommonGUIParser.AddKeysAndValuesToOptions(xOptionsText,
                        [cMethOptionKeyMultiPip], xAllValues);

                    xDP.Edit;
                    xDP.FieldByName(STR_METHOD_V3_FLD_AOPTIONS).AsString := xOptionsText;
                    xDP.Post;
                finally
                    xDP.Next;
                end;
            end;
        finally
            xDP.Close();
        end;
    finally
        xDP.Free;
    end;
end;


end.
