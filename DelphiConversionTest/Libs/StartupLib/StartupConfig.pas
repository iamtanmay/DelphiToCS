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
  21.03.11 pk  TStartupConfigReaderWriter.Create     TN5483     New overloaded Create with aRootPath used by ZAInstaller
  08.09.11 wl  TStartupConfigReaderWriter.Read/Write TN5672   StartupConfig wird nicht mehr lokal gespeichert
  08.09.11 wl  TStartupConfigReaderWriter            TN5672   soll nur noch temporär verwendet werden
  ----------------------------------------------------------------------------------------------------------------------- }

unit StartupConfig;


interface


uses
    Streamable,
    XMLReaderWriter;

type
    TStartupConfig = class(TStreamable)
    private
        fDataLocation: string;
        fMainDBAlias: string;
    published
        property DataLocation: string read fDataLocation write fDataLocation;
        property MainDBAlias: string read fMainDBAlias write fMainDBAlias;

    end;

    TStartupConfigReaderWriter = class
    private const
        cStartupConfigFileName = 'ZAStartup';
    private
        fXMLReaderWriter: TXMLReaderWriter;
    public
        constructor Create(const aRootPath: string);
        destructor Destroy(); override;

        function read(): TStartupConfig;
        procedure write(aStartupConfig: TStartupConfig);

        class function CreateStartupConfig: TStartupConfig;
        class procedure WriteStartupConfig(aStartupConfig: TStartupConfig);
    end;


implementation


uses
    SysUtils,
    TypeMapTranslator,
    FileUtilities;

constructor TStartupConfigReaderWriter.Create(const aRootPath: string);
begin
    inherited Create();
    fXMLReaderWriter := TXMLReaderWriter.Create(TFileUtilities.ConcatPaths(aRootPath,
        cStartupConfigFileName), 0);
end;

destructor TStartupConfigReaderWriter.Destroy();
begin
    FreeAndNil(fXMLReaderWriter);
    inherited;
end;

function TStartupConfigReaderWriter.Read(): TStartupConfig;
begin
    fXMLReaderWriter.ReadFromFile;
    result := fXMLReaderWriter.CreateObjectFromRootNode<TStartupConfig>();
    if not Assigned(result) then
    begin
        result := TStartupConfig.Create();
    end;
end;

procedure TStartupConfigReaderWriter.Write(aStartupConfig: TStartupConfig);
begin
    fXMLReaderWriter.Activate();
    fXMLReaderWriter.AddObjectToRootNode(aStartupConfig);
    fXMLReaderWriter.WriteToFile;
end;

class procedure TStartupConfigReaderWriter.WriteStartupConfig(aStartupConfig: TStartupConfig);
var
    xReaderWriter: TStartupConfigReaderWriter;
begin
    xReaderWriter := TStartupConfigReaderWriter.Create(TFileUtilities.GetRealApplicationPath());
    try
        xReaderWriter.Write(aStartupConfig);
    finally
        FreeAndNil(xReaderWriter);
    end;
end;

class function TStartupConfigReaderWriter.CreateStartupConfig: TStartupConfig;
var
    xReaderWriter: TStartupConfigReaderWriter;
begin
    xReaderWriter := TStartupConfigReaderWriter.Create(TFileUtilities.GetRealApplicationPath());
    try
        EXIT(xReaderWriter.Read());
    finally
        FreeAndNil(xReaderWriter);
    end;
end;


end.
