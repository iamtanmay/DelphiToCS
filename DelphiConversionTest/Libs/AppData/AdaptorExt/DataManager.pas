{ -----------------------------------------------------------------------------------------------------------
  Copyright © 2013 Zinsser Analytic GmbH - All rights reserved.
  Author       : Wolfgang Lyncke (wl)
  Description  :
  -----------------------------------------------------------------------------------------------------------
  Revision History:
  date     op  method                              track-no improvement/change
  -------- --  ----------------------------------  -------- -------------------------------------------------
  17.04.13 wl                                      TN6106   Initial Revision
  19.04.13 wl                                      TN6106   von TCustomDataManager abgeleitet
  ----------------------------------------------------------------------------------------------------------- }

unit DataManager;


interface


uses
    Generics.Collections,
    QueryDataAdaptor;

type
    TDataManager = class(TCustomDataManager)
    private
        fFileExtension: string;
        fDataAdaptors: TObjectList<TQueryDataAdaptor>;
    public
        constructor Create(const aFileExtension: string);
        destructor Destroy; override;
        procedure ExportItem(const aItemName: string);
        procedure ImportItem(const aItemName: string; const aFileName: string);

        procedure SaveNameAs(const aOldName, aNewName: string); override;
        procedure DeleteName(const aName: string); override;
        function NameExists(const aName: string): boolean; override;
        property DataAdaptors: TObjectList<TQueryDataAdaptor>read fDataAdaptors;
    end;


implementation


uses
    SysUtils,
    AppSettings,
    FileExport;

{ TDataManager }

constructor TDataManager.Create(const aFileExtension: string);
begin
    inherited Create;
    fDataAdaptors := TObjectList<TQueryDataAdaptor>.Create;
    fFileExtension := aFileExtension;
end;

destructor TDataManager.Destroy;
begin
    FreeAndNil(fDataAdaptors);
    inherited;
end;

procedure TDataManager.ExportItem(const aItemName: string);
var
    xContainer: TTableInfoContainerList;
    x: integer;
begin
    xContainer := TTableInfoContainerList.Create;
    try
        for x := 0 to fDataAdaptors.Count - 1 do
            xContainer.Add(TStreamableTableDataTransferer.CreateRecordsFromDataProvider(aItemName,
                fDataAdaptors[x]));

        TXMLDBReaderWriter.WriteTableData(TAppSettings.DataPath, fFileExtension, aItemName, xContainer);
    finally
        FreeAndNil(xContainer);
    end;
end;

procedure TDataManager.ImportItem(const aItemName, aFileName: string);
var
    xContainer: TTableInfoContainerList;
    x: integer;
begin
    xContainer := TXMLDBReaderWriter.CreateTableData(aFileName);
    try
        ASSERT(xContainer.Count = fDataAdaptors.Count);

        for x := 0 to fDataAdaptors.Count - 1 do
            TStreamableTableDataTransferer.WriteRecordsToDataProvider(aItemName, xContainer[x],
                fDataAdaptors[x]);
    finally
        FreeAndNil(xContainer);
    end;
end;

procedure TDataManager.DeleteName(const aName: string);
var
    x: integer;
begin
    for x := 0 to fDataAdaptors.Count - 1 do
        fDataAdaptors[x].DeleteName(aName);
end;

function TDataManager.NameExists(const aName: string): boolean;
var
    x: integer;
begin
    for x := 0 to fDataAdaptors.Count - 1 do
    begin
        if fDataAdaptors[x].NameExists(aName) then
            EXIT(true);
    end;

    EXIT(false);
end;

procedure TDataManager.SaveNameAs(const aOldName, aNewName: string);
var
    x: integer;
begin
    for x := 0 to fDataAdaptors.Count - 1 do
        fDataAdaptors[x].SaveNameAs(aOldName, aNewName);
end;


end.
