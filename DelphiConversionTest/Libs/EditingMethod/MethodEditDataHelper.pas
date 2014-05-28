{ -----------------------------------------------------------------------------------------------------------
  Copyright © 2012 Zinsser Analytic GmbH - All rights reserved.
  Author       : Wolfgang Lyncke (wl)
  Description  :
  -----------------------------------------------------------------------------------------------------------
  Revision History:
  date     op  method                              track-no improvement/change
  -------- --  ----------------------------------  -------- -------------------------------------------------
  10.12.12 wl                                      TN6045   Initial Revision
  11.12.12 wl                                      TN6045   enthält den Großteil von MethodEditorDataAdaptor
  27.02.13 wl                                      TN6045   uses Generics.Collections
  ----------------------------------------------------------------------------------------------------------- }

unit MethodEditDataHelper;


interface


uses
    Classes,
    SysUtils,
    ClipboardData,
    Generics.Collections,
    MethodStep,
    MethodDataAdaptor;

const
    INT_METHOD_COL_SUMMARY = 0;
    INT_METHOD_COL_COMMENT = 1;

    INT_NUMBEROFCOLUMNS = 2; // muss unbedingt mit Spaltenanzahl übereinstimmen

type
    EMethodLineSave = class(Exception);

    TMethodEditLine = class
    private
        fValue: TMethodStep;
        fParent: TMethodEditLine;
    public
        property Value: TMethodStep read fValue write fValue;
        property Parent: TMethodEditLine read fParent write fParent;
    end;

    TMethodEditLineList = class(TObjectList<TMethodEditLine>)
    public
        procedure InsertAfterLine(const aMethodEditLine, aInsertAfterMethodEditLine: TMethodEditLine);
    end;

    TMethodEditGroupBeginLine = class(TMethodEditLine)
    private
        fLines: TMethodEditLineList;
        fExpanded: boolean;
        procedure AdoptLine(const aMethodEditLine: TMethodEditLine);
        function GetEndLine: TMethodEditLine;
    public
        constructor Create();
        destructor Destroy(); override;
        procedure ClearLines();
        procedure RemoveLine(const aMethodEditLine: TMethodEditLine);
        procedure AddLine(const aMethodEditLine: TMethodEditLine);
        procedure InsertAfterLine(const aMethodEditLine, aInsertAfterMethodEditLine: TMethodEditLine);
        procedure MoveLinesToGroup(const aMethodEditGroupBeginLine: TMethodEditGroupBeginLine;
            const aInsertLinesAfter: TMethodEditLine);
        property Expanded: boolean read fExpanded write fExpanded;
        property EndLine: TMethodEditLine read GetEndLine;
    end;

    TMethodEditGroupEndLine = class(TMethodEditLine)

    end;

    TMethodEditDataHelper = class
    public
        fOnAddEditFunctions: TNotifyEvent;
        constructor Create(const aOnAddEditFunctions: TNotifyEvent);
        function CreateMethodStep(const aActionName: string): TMethodStep;
        function CreateMethodStepFromMethodRec(const aMethodRec: TMethodRec; const aCreateIfInactive: boolean)
            : TMethodStep;
        function MakeMethodRecFromMethodStep(const aMethodStep: TMethodStep; const aMethodName: string;
            const aSeq: integer): TMethodRec;
        procedure ReadMethod(aMethodName: string; aMethodStepList: TObjectList<TMethodStep>;
            aReadInactiveRecs: boolean); overload;
        function MethodStepToStrArray(const aMethodStep: TMethodStep): TArray<string>;
        function MethodStepFromStrArray(const aArray: TArray<string>): TMethodStep;
    end;

    TClipboardLoadFromStream = procedure(const aMethodSteps: TObjectList<TMethodStep>;
        const aFirstRecordIndex: integer) of object;
    TClipboardSaveToStream = procedure(const aMethodSteps: TObjectList<TMethodStep>) of object;

    TClipboardDataMethodSteps = class(TClipboardData)
    private const
        cNumMethodStepFields = 4;
        STR_CLIPBOARDTEXT_END = #10 + #10 + 'END';
    private
        fFirstRecordIndex: integer;
        fMethodEditDataAdaptor: TMethodEditDataHelper;
        fOnLoadFromStream: TClipboardLoadFromStream;
        fOnSaveToStream: TClipboardSaveToStream;
    protected
        procedure LoadFromStream(Stream: TStream); override;
        procedure SaveToStream(Stream: TStream); override;
    public
        constructor Create(const aMethodEditDataAdaptor: TMethodEditDataHelper;
            const aFirstRecordIndex: integer = 0);
        property OnLoadFromStream: TClipboardLoadFromStream read fOnLoadFromStream write fOnLoadFromStream;
        property OnSaveToStream: TClipboardSaveToStream read fOnSaveToStream write fOnSaveToStream;
    end;


implementation


uses
    Windows,
    RunStepBuilderTypeDictionary;

var
    global_ClipboardFormat_MethodSteps: integer;

const
    ZINSSERMETHODSTEPS_CLIPBOARD = 'ZinsserWinLissyMethodSteps';

    { TClipboardDataMethodSteps }

constructor TClipboardDataMethodSteps.Create(const aMethodEditDataAdaptor: TMethodEditDataHelper;
    const aFirstRecordIndex: integer = 0);
begin
    inherited Create(global_ClipboardFormat_MethodSteps);
    fFirstRecordIndex := aFirstRecordIndex;
    fMethodEditDataAdaptor := aMethodEditDataAdaptor;

end;

procedure TClipboardDataMethodSteps.SaveToStream(Stream: TStream);
var
    x, i: integer;
    xText: string;
    xMethodStepList: TObjectList<TMethodStep>;
    xArray: TArray<string>;
begin
    if not Assigned(self.fOnSaveToStream) then
        EXIT;
    try
        xMethodStepList := TObjectList<TMethodStep>.Create();
        try
            xMethodStepList.OwnsObjects := false;
            fOnSaveToStream(xMethodStepList);

            if xMethodStepList.Count = 0 then
                EXIT;
            SetLength(xArray, cNumMethodStepFields);
            for i := 0 to xMethodStepList.Count - 1 do
            begin
                xArray := fMethodEditDataAdaptor.MethodStepToStrArray(xMethodStepList[i]);
                for x := 0 to Length(xArray) - 1 do
                begin
                    xText := xArray[x];
                    self.WriteAnsiString(Stream, ansistring(xText));
                end;

            end;

        finally
            FreeAndNil(xMethodStepList);
        end;
    finally
        self.WriteAnsiString(Stream, STR_CLIPBOARDTEXT_END);
    end;
end;

procedure TClipboardDataMethodSteps.LoadFromStream(Stream: TStream);
var
    x: integer;
    xText: string;
    xMethodStep: TMethodStep;
    xMethodSteps: TObjectList<TMethodStep>;
    xArray: TArray<string>;
begin
    try
        xMethodSteps := TObjectList<TMethodStep>.Create();
        try
            xMethodSteps.OwnsObjects := false;

            SetLength(xArray, cNumMethodStepFields);
            while (true) do
            begin

                for x := 0 to cNumMethodStepFields - 1 do
                begin
                    xText := string(ReadAnsiString(Stream));
                    if (xText = STR_CLIPBOARDTEXT_END) then
                        BREAK;
                    xArray[x] := xText;
                end;

                if (xText = STR_CLIPBOARDTEXT_END) then
                    BREAK;

                xMethodStep := fMethodEditDataAdaptor.MethodStepFromStrArray(xArray);
                xMethodSteps.Add(xMethodStep);

            end;

            self.fOnLoadFromStream(xMethodSteps, fFirstRecordIndex);

        finally
            FreeAndNil(xMethodSteps);
        end;
    except

    end;
end;

{ TMethodEditLineList }

procedure TMethodEditLineList.InsertAfterLine(const aMethodEditLine, aInsertAfterMethodEditLine
    : TMethodEditLine);
var
    xIndex: integer;
begin
    xIndex := 0;
    if Assigned(aInsertAfterMethodEditLine) then
    begin
        xIndex := self.IndexOf(aInsertAfterMethodEditLine) + 1;
    end;

    self.Insert(xIndex, aMethodEditLine);
end;

{ TMethodEditGroupBeginLine }

procedure TMethodEditGroupBeginLine.RemoveLine(const aMethodEditLine: TMethodEditLine);
begin
    if aMethodEditLine is TMethodEditGroupBeginLine then
    begin
        // Add my child to my parent's child list after myself
        (aMethodEditLine as TMethodEditGroupBeginLine).MoveLinesToGroup(self, aMethodEditLine);
    end;

    fLines.Remove(aMethodEditLine);
end;

procedure TMethodEditGroupBeginLine.AdoptLine(const aMethodEditLine: TMethodEditLine);
var
    xOldParent: TMethodEditLine;
begin
    xOldParent := aMethodEditLine.Parent;
    if xOldParent = self then
        EXIT;

    if Assigned(xOldParent) then
    begin
        (xOldParent as TMethodEditGroupBeginLine).RemoveLine(aMethodEditLine);
    end;

    aMethodEditLine.Parent := self;
end;

procedure TMethodEditGroupBeginLine.AddLine(const aMethodEditLine: TMethodEditLine);
begin
    AdoptLine(aMethodEditLine);
    fLines.Add(aMethodEditLine);
end;

procedure TMethodEditGroupBeginLine.MoveLinesToGroup(const aMethodEditGroupBeginLine
    : TMethodEditGroupBeginLine; const aInsertLinesAfter: TMethodEditLine);
var
    x: integer;
    xLine: TMethodEditLine;
begin
    for x := fLines.Count - 1 downto 0 do
    begin
        xLine := fLines[x];
        if xLine is TMethodEditGroupEndLine then
            BREAK;

        RemoveLine(xLine);
        aMethodEditGroupBeginLine.InsertAfterLine(xLine, aInsertLinesAfter);
    end;

end;

procedure TMethodEditGroupBeginLine.InsertAfterLine(const aMethodEditLine, aInsertAfterMethodEditLine
    : TMethodEditLine);
begin
    AdoptLine(aMethodEditLine);
    fLines.InsertAfterLine(aMethodEditLine, aInsertAfterMethodEditLine);
end;

procedure TMethodEditGroupBeginLine.ClearLines;
begin
    fLines.Clear();
end;

constructor TMethodEditGroupBeginLine.Create;
begin
    inherited Create();
    fLines := TMethodEditLineList.Create(false);
    fExpanded := true;
end;

destructor TMethodEditGroupBeginLine.Destroy;
begin
    FreeAndNil(fLines);
    inherited;
end;

function TMethodEditGroupBeginLine.GetEndLine: TMethodEditLine;
begin
    result := nil;
    if fLines.Count <= 0 then
        EXIT;
    result := fLines[fLines.Count - 1];
end;

{ TMethodEditDataHelper }

constructor TMethodEditDataHelper.Create(const aOnAddEditFunctions: TNotifyEvent);
begin
    inherited Create();
    fOnAddEditFunctions := aOnAddEditFunctions;
end;

function TMethodEditDataHelper.CreateMethodStep(const aActionName: string): TMethodStep;
begin
    result := TRunStepBuilderTypeDictionary.Instance.CreateMethodStep(aActionName, '', '', false, true,
        fOnAddEditFunctions);
end;

function TMethodEditDataHelper.CreateMethodStepFromMethodRec(const aMethodRec: TMethodRec;
    const aCreateIfInactive: boolean): TMethodStep;
begin
    result := TRunStepBuilderTypeDictionary.Instance.CreateMethodStepFromMethodRec(aMethodRec,
        aCreateIfInactive, fOnAddEditFunctions);
end;

function TMethodEditDataHelper.MakeMethodRecFromMethodStep(const aMethodStep: TMethodStep;
    const aMethodName: string; const aSeq: integer): TMethodRec;
begin
    result := aMethodStep.AsMethodRec(aMethodName, aSeq);
end;

function TMethodEditDataHelper.MethodStepFromStrArray(const aArray: TArray<string>): TMethodStep;
var
    xMethodRec: TMethodRec;
begin
    xMethodRec.Valid := true;
    xMethodRec.Action := aArray[0];
    xMethodRec.Options := aArray[1];
    xMethodRec.Comment := aArray[2];
    xMethodRec.Inactive := aArray[3] = '1';
    result := CreateMethodStepFromMethodRec(xMethodRec, true);
end;

function TMethodEditDataHelper.MethodStepToStrArray(const aMethodStep: TMethodStep): TArray<string>;
var
    xMethodRec: TMethodRec;
begin
    xMethodRec := MakeMethodRecFromMethodStep(aMethodStep, '', 0);
    SetLength(result, TClipboardDataMethodSteps.cNumMethodStepFields);
    result[0] := xMethodRec.Action;
    result[1] := xMethodRec.Options;
    result[2] := xMethodRec.Comment;
    if xMethodRec.Inactive then
        result[3] := '1'
    else
        result[3] := '0'
end;

procedure TMethodEditDataHelper.ReadMethod(aMethodName: string; aMethodStepList: TObjectList<TMethodStep>;
    aReadInactiveRecs: boolean);
var
    xDA: TMethodDataAdaptor;
    xMethodRec: TMethodRec;
    xMethodStep: TMethodStep;
begin
    xDA := TMethodDataAdaptor.Create();
    try
        xDA.SelectAndOpenMethod(aMethodName, false, true);
        try
            while not xDA.DataProvider.Eof do
            begin
                xMethodRec := xDA.ReadMethodrecFromDataSet(xDA.DataProvider);
                xDA.DataProvider.Next;

                xMethodStep := CreateMethodStepFromMethodRec(xMethodRec, aReadInactiveRecs);
                if not Assigned(xMethodStep) then
                    CONTINUE;

                aMethodStepList.Add(xMethodStep);

            end;
        finally
            xDA.Close();
        end;
    finally
        xDA.Free;
    end;
end;


initialization


global_ClipboardFormat_MethodSteps := RegisterClipboardFormat(ZINSSERMETHODSTEPS_CLIPBOARD);


end.
