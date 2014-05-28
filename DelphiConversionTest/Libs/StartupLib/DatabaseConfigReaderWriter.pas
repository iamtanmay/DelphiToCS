{ -----------------------------------------------------------------------------------------------------------------------
  Copyright © 2010 Zinsser Analytic GmbH - All rights reserved.
  Author       : Payman Kamali (pk)
  Description  :
  -----------------------------------------------------------------------------------------------------------------------
  Revision History:
  date     op  method                                track-no  improvement/change
  -------- --  ------------------------------------  --------- ----------------------------------------------------------
  04.08.10 pk                                        TN5218     Initial revision
  05.11.10 wl                                        TN5218     Dateipfad jetzt absolut
  21.03.11 pk  Create                                TN5483     New overloaded Create with aRootPath used by ZAInstaller
  ----------------------------------------------------------------------------------------------------------------------- }

unit DatabaseConfigReaderWriter;


interface


uses
    GeneralTypes,
    DatabaseConfig,
    XMLReaderWriter,
    TypeMapTranslator;

type
    TDatabaseConfigGroupReaderWriter = class
    private
        fXMLReaderWriter: TXMLReaderWriter;

    const
        cDatabaseConfigFileName = 'ZADBConfig';
    public
        constructor Create(); overload;
        constructor Create(const aRootPath: string); overload;
        destructor Destroy(); override;
        function read(): TDatabaseConfigGroup;
        procedure write(const aValue: TDatabaseConfigGroup);
        class function GetDatabaseAliasNames(): TStringArray;
        class function ReadDatabaseConfigGroup: TDatabaseConfigGroup;
        class procedure WriteDatabaseConfigGroup(const aValue: TDatabaseConfigGroup);
    end;


implementation


uses
    SysUtils,
    FileUtilities;

constructor TDatabaseConfigGroupReaderWriter.Create(const aRootPath: string);
begin
    inherited Create();
    fXMLReaderWriter := TXMLReaderWriter.Create(TFileUtilities.ConcatPaths(aRootPath,
        cDatabaseConfigFileName), 0);
end;

constructor TDatabaseConfigGroupReaderWriter.Create();
begin
    Create(TFileUtilities.GetRealApplicationPath());
end;

destructor TDatabaseConfigGroupReaderWriter.Destroy();
begin
    FreeAndNil(fXMLReaderWriter);
    inherited;
end;

class function TDatabaseConfigGroupReaderWriter.GetDatabaseAliasNames: TStringArray;
var
    xDatabaseConfigGroup: TDatabaseConfigGroup;
begin
    xDatabaseConfigGroup := ReadDatabaseConfigGroup();
    try
        result := xDatabaseConfigGroup.GetDatabaseAliasNames();
    finally
        FreeAndNil(xDatabaseConfigGroup)
    end;
end;

function TDatabaseConfigGroupReaderWriter.Read(): TDatabaseConfigGroup;
begin
    fXMLReaderWriter.ReadFromFile;
    result := fXMLReaderWriter.CreateObjectFromRootNode<TDatabaseConfigGroup>();
    if not Assigned(result) then
    begin
        result := TDatabaseConfigGroup.Create();
    end;
end;

class function TDatabaseConfigGroupReaderWriter.ReadDatabaseConfigGroup: TDatabaseConfigGroup;
var
    xReaderWriter: TDatabaseConfigGroupReaderWriter;
begin
    xReaderWriter := TDatabaseConfigGroupReaderWriter.Create();
    try
        result := xReaderWriter.Read();
    finally
        FreeAndNil(xReaderWriter);
    end;
end;

class procedure TDatabaseConfigGroupReaderWriter.WriteDatabaseConfigGroup(const aValue: TDatabaseConfigGroup);
var
    xReaderWriter: TDatabaseConfigGroupReaderWriter;
begin
    xReaderWriter := TDatabaseConfigGroupReaderWriter.Create();
    try
        xReaderWriter.Write(aValue);
    finally
        FreeAndNil(xReaderWriter);
    end;
end;

procedure TDatabaseConfigGroupReaderWriter.Write(const aValue: TDatabaseConfigGroup);
begin
    fXMLReaderWriter.DataChanged();
    fXMLReaderWriter.Activate();
    fXMLReaderWriter.AddObjectToRootNode(aValue);
    fXMLReaderWriter.WriteToFile;
end;


end.
