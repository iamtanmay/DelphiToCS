{ --------------------------------------------------------------------------------------------------
  Copyright © 2008 Zinsser Analytic GmbH - All rights reserved.
  Author       : Wolfgang Lyncke (wl)
  Description  : Wrapper for Database functions
  --------------------------------------------------------------------------------------------------
  Revision History:
  date     op  method                       track-no improvement/change
  -------- --  ---------------------------  -------- -----------------------------------------------
  21.01.08 wl                               TN3972    initial version
  08.09.08 pk  OpenSession                  TN4215    New
  17.11.08 wl  StrToOem                     TN4312    für Delphi 2009 auskommentiert
  04.11.09 pk                               TN4843   Replace TObjectList, TStringList, TStrings with Generics Lists and TStringArray
  16.12.09 pk                               TN4933   code moved to TBDEDatabaseProvider
  17.06.10 pk                               TN5152.1 CreateNewAlias Removed
  17.06.10 pk TDatabaseProviderManager      TN5152.1 New
  04.08.10 pk                               TN5218   Changes for Plugin database packages
  13.09.10 pk                               TN5218   Changes for Plugin database packages
  15.02.11 pk                               TN4780   TableExists, DeleteTable new
  -------------------------------------------------------------------------------------------------- }

unit DatabaseProvider;


interface


uses
    GeneralTypes,
    StreamableDatasetClasses,
    DataConnectionParams;

type
    TDatabasePasswordEvent = procedure(Sender: TObject; var Continue: Boolean) of object;

    TDatabaseProvider = class
    public
        function GetAllTableNames(): TStringArray; virtual; abstract;
        procedure AddSystemPassword(const aPassword: string); virtual; abstract;
        procedure SetOnPassword(aOnPassword: TDatabasePasswordEvent); virtual; abstract;
        function SetAllTablesPassword(aAliasName, aPassword: string): string; virtual; abstract;
        procedure OpenSession(const aSessionName: string); virtual; abstract;
        function ReadTableDef(const aTableName: string): TStreamableTableDef; virtual; abstract;
        function CreateTable(const aTableDef: TStreamableTableDef): boolean; virtual; abstract;
        function ReadDatabaseDef(): TStreamableDatabaseDef; virtual; abstract;
        function CreateDatabase(const aDatabaseDef: TStreamableDatabaseDef): boolean; virtual; abstract;
        procedure DeleteTable(const aTableName: string); virtual; abstract;
        function TableExists(const aTableName: string): boolean; virtual; abstract;

    end;


implementation


end.
