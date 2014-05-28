{ -----------------------------------------------------------------------------------------------------------------------
  Copyright © 2010 Zinsser Analytic GmbH - All rights reserved.
  Author       : Wolfgang Lyncke (wl)
  Description  :
  -----------------------------------------------------------------------------------------------------------------------
  Revision History:
  date     op  method                                track-no  improvement/change
  -------- --  ------------------------------------  --------- ----------------------------------------------------------
  02.11.10 wl                                        TN5112     Initial revision
  04.11.10 wl  cLicenseConfigFileName                TN5112     Pfad ist immer ProgramData/..
  12.11.10 wl                                        TN5112     Pfad ist immer Program Files
  ----------------------------------------------------------------------------------------------------------------------- }

unit LicenseConfig;


interface


uses
    Streamable,
    XMLReaderWriter;

type
    TLicenseConfig = class(TStreamable)
    private
        fLicenseKey: string;
    published
        property LicenseKey: string read fLicenseKey write fLicenseKey;
    end;

    TLicenseConfigReaderWriter = class
    private
        fLicenseConfig: TLicenseConfig;
        fXMLReaderWriter: TXMLReaderWriter;

    const
        cLicenseConfigFileName = 'ZALicense';
    public
        constructor Create();
        destructor Destroy(); override;
        procedure read();
        procedure write();
        property LicenseConfig: TLicenseConfig read fLicenseConfig;
        class function ReadLicenseConfig: TLicenseConfig;
    end;


implementation


uses
    SysUtils,
    TypeMapTranslator,
    FileUtilities;

constructor TLicenseConfigReaderWriter.Create();
begin
    inherited Create();
    fXMLReaderWriter := TXMLReaderWriter.Create(TFileUtilities.GetRealApplicationPath() +
        cLicenseConfigFileName, 0);
    fLicenseConfig := TLicenseConfig.Create();
end;

destructor TLicenseConfigReaderWriter.Destroy();
begin
    FreeAndNil(fLicenseConfig);
    FreeAndNil(fXMLReaderWriter);
    inherited;
end;

procedure TLicenseConfigReaderWriter.Read();
begin
    fXMLReaderWriter.ReadFromFile;
    fLicenseConfig := fXMLReaderWriter.CreateObjectFromRootNode<TLicenseConfig>();
    if not Assigned(fLicenseConfig) then
    begin
        fLicenseConfig := TLicenseConfig.Create();
    end;

end;

procedure TLicenseConfigReaderWriter.Write;
begin
    fXMLReaderWriter.Activate();
    fXMLReaderWriter.AddObjectToRootNode(fLicenseConfig);
    fXMLReaderWriter.WriteToFile;
end;

class function TLicenseConfigReaderWriter.ReadLicenseConfig: TLicenseConfig;
var
    xReaderWriter: TLicenseConfigReaderWriter;
begin
    xReaderWriter := TLicenseConfigReaderWriter.Create();
    try
        xReaderWriter.Read();
        result := TObjectCopy<TLicenseConfig>.Copy(xReaderWriter.LicenseConfig);
    finally
        FreeAndNil(xReaderWriter);
    end;
end;


end.
