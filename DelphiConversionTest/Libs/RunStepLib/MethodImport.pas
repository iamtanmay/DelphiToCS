{ --------------------------------------------------------------------------------------------------
  Copyright © 2004 Zinsser Analytic GmbH - All rights reserved.
  Author       : Payman Kamali (pk)
  Description  : Various functions that convert a method containing variables and import options to a method
  where the variables are replaced by imported values and the import options are removed
  --------------------------------------------------------------------------------------------------
  Revision History:
  date     op  method                       track-no improvement/change
  -------- --  ---------------------------  -------- -----------------------------------------------
  27.04.04 pk                               TN1880   initial version
  27.04.05 pk  DeleteImportMethod           TN1880   New
  05.05.04 pk  ExpandMethodRow              TN1880   gmGetRackPositionNumber not used anymore
  12.05.04 pk                               TN1880   STR_IMPORT_VARNAME_SOURCEFILERECS changed from _RECS to _#RECS
  13.05.04 pk  ExpandMethod                 TN1880   Fixed Bug: only imported when Action=''
  13.05.04 pk  ExpandMethodRow              TN1880   Allow firstPos to be a variable even if lastpos is _#Recs
  13.05.04 pk  ExpandMethodRow              TN1880   Allow firstPos to be INT_RACKPOS_RACKNAME_IS_TUBEID
  17.05.04 pk  ExpandMethodRow              TN1880   Bug fixed
  17.05.04 pk  ExpandMethodRow              TN1880   Another bug fixed
  17.05.04 pk  ResolveVariables             TN1880   Defname should not be case-sensitive
  18.05.04 pk  IsAllowedAction              TN1941   New: true if the action name given is allowed to contain Import Options
  01.06.04 pk  RemoveImportOptions          TN1941   Don't remove import options for certain actions
  03.06.04 pk  ResolveVariables             TN1968   Correclty handle filters
  08.06.04 pk                               TN1909.0 Target method name no longer assumed to be BUILD and is passed to every function as a argument
  07.07.04 pk  ResolveVariables             TN2023   incorrect method name was used
  07.07.04 pk  ImportMethod                 TN2023   init method name
  03.08.04 pk  ResolveVariables             TN2066   Add the _#RECS coldef to the coldefs array
  03.08.04 pk  ExpandMethodRow              TN2065   if TargetRack/SourceRack is empty number of positions = 1
  08.09.04 pk  ImportUsingImportDef         TN2126   New parameter : Import Filter
  04.11.04 wl                               TN2213   uses MethodTypes
  04.11.04 wl  MethodHasImportOptions,ExpandMethod,ResolveVariables  TN2213  TMethodGUIParser.ImportIntoTableActionRec1FromStr statt .ParseImportKeysInOptions
  16.11.04 pk  RemoveImportOptions          TN2190   Only write to options field if options contains import options
  24.11.04 pk  GetMethodName                TN2244   Always gives a constant
  24.11.04 pk  ImportMethod                 TN2244   if exception occurs delete import method
  02.02.05 pk  GetMethodName                TN2305   Adds _ to given methodname
  07.06.05 pk  GetMethodName                TN2451   Bug : returned name with incorrect length
  07.06.05 pk  ExpandMethodRow              TN2451   if  xSourceNumPositions = 1 then handle as special case
  19.09.05 pk  ExpandMethodRow              TN2609   the calculation of xCurrentSourceFirstPos changed to  = i mod xSourceNumPositions
  03.11.05 wl  ExpandMethodRow              TN2722   wenn aImportOptions.AllRecs = true, werden alle Records (xNumRecs) importiert
  03.11.05 wl  TotalRecsInSource,ChangeDataset TN2723 benutzen ReadImportDefinition statt TImportDefDataAdaptor.QuickReadDef
  03.11.05 wl  ReadImportDefinition         TN2723   wenn OtherFilePathName <> '', wird der Dateiname aus ImportFileDef ersetzt
  01.12.05 pk  ExpandMethodRow              TN2820   use xTargetNumPositions instead of numrecs
  01.12.05 pk  TotalRecsInSource            TN2820   pass username and password to obtaindataset
  06.12.05 wl  ResolveVariables             TN2835   if the record cursor is set to EOF the record in the method table will be deleted
  08.12.05 pk  GetBuildMethodName           TN2845   Bug : gave result := original method name if name was 20 chars long.  This caused the original method to be deleted
  12.12.05 pk                               TN2847   IDataset instead of TDataset
  19.01.06 pk  ResolveVariables             TN2889   MakeColDefRec more params
  03.02.06 wl  ResolveVariables             TN2927   CurrentRecNo wird mitgezählt
  03.02.06 wl  ResolveVariables             TN2927   Fehler bei Delete beseitigt, der bei 2. Import mit CursorMove > A1 auftritt
  03.02.06 wl  ParseNextRecordNumber        TN2927   CursorMove wird in absolute Position umgerechnet!
  03.02.06 wl  ParseNextRecordNumber        TN2298   LAST springt zum letzten Datensatz
  20.03.06 wl  GetBuildMethodName           TN2983   Kompatibilität zu Ver.<= 7.0.1 hergestellt: Name wird weniger gekürzt
  25.03.06 pk                               TN3001   Various changes to avoid using global instance of dataadaptors - crash when multithreading
  06.04.06 pk  GetBuildMethodName           TN3001   Use name modifier to make new method name
  18.04.06 pk  ImportUsingImportDef         TN3001   new sessionname parameter
  19.06.06 wl                               TN3159   statt des direkten Zugriffs auf REMARK wird ReadRemark/WriteRemark benutzt
  07.07.06 wl  ExpandMethodRow              TN3190   Beim Kopieren wurde REMARKEXTEND bisher nicht berücksichtigt
  09.10.06 wl  ResolveVariables             TN3352   statt TImports.ImportRow wird self.ImportRow aufgerufen
  09.10.06 wl  ImportRow                    TN3352   vom ImportClasses hierher kopiert
  09.10.06 wl  ImportColumnDef              TN3352   vom ImportClasses hierher kopiert, berücksichtigt REMARK/REMARKEXTEND
  09.10.06 wl  DetermineTargetFieldIndex    TN3352   vom ImportClasses hierher kopiert, berücksichtigt REMARK/REMARKEXTEND
  09.10.06 wl  AssignSourceValueToTargetRemarkField   TN3352   neue Methode speziell für die Felder REMARK/REMARKEXTEND
  28.11.06 wl  ImportUsingImportDef         TN3397   neuer Parameter SourceFileName überschreibt File-Definition
  07.12.06 wl  IsAllowedAction              TN3456   benutzt TCommonGUIParser.ActionHasGenericMethodImportOption
  14.10.07 wl  ResolveVariables             TN3566   benutzt TBasicDataset, Free am Schluss
  14.10.07 wl  TotalRecsInSource            TN3566   benutzt TBasicDataset, Free am Schluss
  07.08.07 wl                               TN3811.3 uses DatasetUtils
  02.10.07 wl                               TN3811.5  uses dbTables entfernt: TDBDataset durch TDataset ersetzt
  09.11.07 pk                               TN3922    Dataset changed to DataProvider
  09.01.08 wl                               TN3972    DataProvider changed to TRunStepBuilderList (massive changes)
  04.04.08 wl                               TN4058    uses geändert
  17.04.08 wl                               TN4067    TBasicDataset replaced by TImportQuery
  17.04.08 wl                               TN4067    several changes to make the import work again
  02.09.08 pk                               TN4215    reference to gRunStepBuilderTypeDictionary changed
  19.09.08 pk MethodHasImportOptions        TN4215    Check for nil steps
  23.09.08 wl                               TN4236    Verweis auf MethodFieldnames entfernt
  06.10.08 pk                               TN4259    TMethodImport removed. Functionality replaced by import loops
  16.01.09 wl                                TN4362   an Änderungen in TQueryDataAdaptor angepasst
  25.06.10 wl  ImportUsingImportDef         TN5161    OrderBy ermöglich zusätzliche Sortierung
  30.08.13 wl  ImportFormatMethodSeq        TN6236   Zugriff auf TMethodDataAdaptor geändert
  -------------------------------------------------------------------------------------------------- }

unit MethodImport;


interface


uses
    ImportDataAdaptor;

type
    TGeneralImport = class
    private
        class procedure ImportFormatMethodSeq(aImportDef: TImportDefRec);
    public
        class function ImportUsingImportDef(const aImportDefName, aFilter, aOrderBy, aSourceFileName: string;
            aSessionName: string = ''): integer; overload;
    end;


implementation


uses
    SysUtils,
    ImportFileDefDataAdaptor,
    ImportColDefDataAdaptor,
    ImportClasses,
    MethodDataAdaptor,
    ImportDataProvider,
    AppTypes,
    SQLParser;

{ TGeneralImport }

class procedure TGeneralImport.ImportFormatMethodSeq(aImportDef: TImportDefRec);
var
    xDatabaseName, xTableName: string;
begin
    // if method table then format the SEQ field
    TAliasTablePath.ParadoxParseDatabaseNameAndTableName(aImportDef.TargetName, xDatabaseName, xTableName);
    if SameText(xTableName, STR_METHOD_TBL) then
        Exit;
end;

class function TGeneralImport.ImportUsingImportDef(const aImportDefName, aFilter, aOrderBy,
    aSourceFileName: string; aSessionName: string): integer;
var
    xImportDef: TImportDefRec;
    xDataAdaptor: TImportDefDataAdaptor;
begin
    result := 0;
    if aImportDefName = '' then
        EXIT;
    xDataAdaptor := TImportDefDataAdaptor.Create;
    try
        xDataAdaptor.IncludeColDefs := true;
        xDataAdaptor.IncludeSourceFileDef := true;
        xImportDef := xDataAdaptor.ReadDef(aImportDefName);
    finally
        xDataAdaptor.Free;
    end;

    if (aSourceFileName <> '') then
    begin
        xImportDef.SourceFileDef.PathName := aSourceFileName;
    end;
    if (aFilter <> '') then
    begin
        xImportDef.SourceFileDef.Filter := TSQLParser.ConcatFilters(xImportDef.SourceFileDef.Filter, aFilter);
    end;
    if (aOrderBy <> '') then
    begin
        xImportDef.SourceFileDef.OrderBy := TSQLParser.ConcatOrders(xImportDef.SourceFileDef.OrderBy,
            aOrderBy);
    end;

    result := TImports.Import(xImportDef, aSessionName);
    ImportFormatMethodSeq(xImportDef);
end;


end.
