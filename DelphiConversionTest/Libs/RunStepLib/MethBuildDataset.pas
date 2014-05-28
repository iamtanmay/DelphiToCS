unit MethBuildDataset;
{ --------------------------------------------------------------------------------------------------
  Copyright © 2005 Zinsser Analytic GmbH - All rights reserved.
  Author       : pk
  Description  :
  --------------------------------------------------------------------------------------------------
  Revision History:
  date     op  method                       track-no improvement/change
  -------- --  ---------------------------  -------- -----------------------------------------------
  07.06.05 pk                               TN2449 Initial revision
  24.06.05 pk  SetFromMethodRec             TN2449 Do not parse Method Name and Seq fields
  15.08.05 pk  StrToField                   TN2560 call Tokenize function with less parameters
  15.08.05 pk  StrToField                   TN2560 better exception message when parsing fails
  25.03.06 pk  TMethBuildDataset            TN3001 New field fMethodImport
  25.03.06 pk  DoImport                     TN3001 New
  06.04.06 pk  DoImport                     TN3001 Now has new name modifier
  06.04.06 pk                               TN3024 New field SchedSharedID
  06.04.06 pk                               TN3032 New field Iterate
  18.09.06 pk  NextLine                     TN3304 call fOnCheckExternalError do check for errors
  21.02.07 pk  StrToField                   TN3583 Various changes to ParseTree
  21.02.07 pk  uFieldDefs                   TN3583 free in finalization section of unit
  24.07.07 pk  TMethBuildCompositeField     TN3785 New
  24.07.07 pk  MultiStrToField              TN3785 New: Dont parse entire options string, just Value strings
  24.07.07 pk  TMethBuildField              TN3785 MethodName
  31.07.07 pk  SetFromMethodRec             TN3785 Dont parse Calc options as MultiString
  27.08.07 pk  TMethodBuildExecute          TN3788 Name changed from TMethodBuildAssign
  27.08.07 pk  ParserResult.Category        TN3788 removed. Category is now determined from EvalNode class type
  09.11.07 pk                               TN3922    Dataset changed to DataProvider
  08.01.08 wl  TMethBuildDataset            TN3972    benutzt RunStepBuilderList statt Datenbankzugriff
  06.05.08 wl  ReadMethodSteps              TN4074    Reads method without Inactive records
  02.09.08 pk  InitControlFlow              TN4215 parameter removed
  08.09.08 pk                               TN4215 most of code moved to MethodCompile
  19.09.08 pk  ReadMethodSteps              TN4215 moved to MethodCompile
  23.09.08 wl                               TN4236   Verweis auf MethodFieldnames entfernt
  06.10.08 pk  DoImport                     TN4259   removed
  06.11.08 pk  TMethodCompiledCode          TN4279  New
  17.02.09 pk  TMethodCompiledCode          TN4232  New: FindLineByAddress
  04.11.09 pk                               TN4843  Replace TObjectList, TStringList, TStrings with Generics Lists and TStringArray
  15.11.10 pk                               TN5340  Changes to prevent memory leak
  22.04.13 wl                               TN6045   uses Generics.Collections
  -------------------------------------------------------------------------------------------------- }


interface


uses
    Generics.Collections,
    MemoryClasses,
    ParserSymbolTable,
    RunStepBuilder,
    ParserEvalTable;

type
    TMethBuildDataset = class
    private
        fSteps: TObjectList<TRunStepBuilder>;
        fMethodName: string;
        fIdentList: TParserIdentifierList;
        fLineCursor: integer;
        function GetCurrentLine(): TRunStepBuilder;
        function GetIsEOF(): boolean;
    public
        constructor Create(const aMethodName: string);
        destructor Destroy(); override;
        procedure ResetCursor();
        procedure IncrementCursor();
        procedure First();
        procedure MoveCursorTo(const aLineIndex: integer);

        property IdentList: TParserIdentifierList read fIdentList;
        property MName: string read fMethodName;
        property Steps: TObjectList<TRunStepBuilder>read fSteps;
        property CurrentLine: TRunStepBuilder read GetCurrentLine;
        property IsEOF: boolean read GetIsEOF;
    end;

    TMethodCompiledCode = class
    private
        fCode: TObjectList<TMethBuildDataset>;
        fEvalTable: TParserEvalTable;
        function FindDatasetByMethodName(const aMethodName: string): TMethBuildDataset;
    public
        constructor Create();
        destructor Destroy(); override;
        function FindLineByAddress(const aAddress: TRelativeMemAddress): TRunStepByMethodStepBuilder;
        function IsBOFAddress(const aAddress: TRelativeMemAddress): boolean;
        function IsEOFAddress(const aAddress: TRelativeMemAddress): boolean;
        property Code: TObjectList<TMethBuildDataset>read fCode;
        property EvalTable: TParserEvalTable read fEvalTable;
    end;


implementation


uses
    SysUtils;

{ TMethBuildDataset }

constructor TMethBuildDataset.Create(const aMethodName: string);
begin
    inherited Create;

    fMethodName := aMethodName;
    fIdentList := TParserIdentifierList.Create(aMethodName);
    ResetCursor();

    fSteps := TObjectList<TRunStepBuilder>.Create;
end;

destructor TMethBuildDataset.Destroy();
begin
    fIdentList.Free;
    fSteps.Free;

    inherited;
end;

procedure TMethBuildDataset.ResetCursor();
begin
    fLineCursor := -1;
end;

procedure TMethBuildDataset.First();
begin
    ResetCursor();
end;

procedure TMethBuildDataset.IncrementCursor;
begin
    Inc(fLineCursor);
end;

function TMethBuildDataset.GetCurrentLine: TRunStepBuilder;
begin
    result := self.Steps[fLineCursor];
end;

function TMethBuildDataset.GetIsEOF: boolean;
begin
    result := fLineCursor >= self.Steps.Count;
end;

procedure TMethBuildDataset.MoveCursorTo(const aLineIndex: integer);
begin
    fLineCursor := aLineIndex;
end;

{ TMethodCompiledCode }

constructor TMethodCompiledCode.Create;
begin
    inherited Create();
    fCode := TObjectList<TMethBuildDataset>.Create();
    fEvalTable := TParserEvalTable.Create();
end;

destructor TMethodCompiledCode.Destroy;
begin
    fEvalTable.Free;
    fCode.Free;
    inherited;
end;

function TMethodCompiledCode.IsEOFAddress(const aAddress: TRelativeMemAddress): boolean;
var
    xMethBuildDataset: TMethBuildDataset;
begin
    xMethBuildDataset := FindDatasetByMethodName(aAddress.LabelName);
    result := aAddress.RelativeAddress >= xMethBuildDataset.Steps.Count;
end;

function TMethodCompiledCode.IsBOFAddress(const aAddress: TRelativeMemAddress): boolean;
begin
    result := aAddress.RelativeAddress = TRelativeMemAddress.BOFAddress;
end;

function TMethodCompiledCode.FindLineByAddress(const aAddress: TRelativeMemAddress)
    : TRunStepByMethodStepBuilder;
var
    xMethBuildDataset: TMethBuildDataset;
begin
    xMethBuildDataset := FindDatasetByMethodName(aAddress.LabelName);
    if not Assigned(xMethBuildDataset) then
        raise Exception.CreateFmt('Method "%s" not found', [aAddress.LabelName]);
    result := xMethBuildDataset.Steps[aAddress.RelativeAddress] as TRunStepByMethodStepBuilder;
end;

function TMethodCompiledCode.FindDatasetByMethodName(const aMethodName: string): TMethBuildDataset;
var
    x: integer;
begin
    for x := 0 to fCode.Count - 1 do
    begin
        if (fCode[x].MName = aMethodName) then
            EXIT(fCode[x]);
    end;

    EXIT(nil);
end;


end.
