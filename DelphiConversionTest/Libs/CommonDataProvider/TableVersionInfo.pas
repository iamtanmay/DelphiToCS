{ -----------------------------------------------------------------------------------------------------------
  Copyright © 2013 Zinsser Analytic GmbH - All rights reserved.
  Author       : Wolfgang Lyncke (wl)
  Description  :
  -----------------------------------------------------------------------------------------------------------
  Revision History:
  date     op  method                              track-no improvement/change
  -------- --  ----------------------------------  -------- -------------------------------------------------
  17.04.13 wl                                      TN6106   Initial Revision
  ----------------------------------------------------------------------------------------------------------- }

unit TableVersionInfo;


interface


type
    TTableRevisionNumberCompareResult = (rncFirstIsNewer, rncFirstIsOlder, rncEqual);

    TTableRevisionNumber = record
    public
        Major: integer;
        Minor: integer;
        class function VersionToStr(aVersion: TTableRevisionNumber): string; static;
    end;

    TTableVersionInfoAdaptor = class
    private const
        INT_MINORVERSION_NONE = 0;
        INT_MAJORVERSION_NONE = 0;
        STR_IDENT_VERSION = 'VERSION';
        STR_IDENT_MAJORREVISIONNUMBER = 'REVISION';
        STR_IDENT_MINORREVISIONNUMBER = 'MINORREVISION';
    private
        fTableName: string;
    public
        constructor Create(const aTableName: string);
        function ReadVersion(const aPath: string): TTableRevisionNumber;
        procedure WriteVersion(const aPath: string; aVersion: TTableRevisionNumber);

        class function IsVersionMissing(aVersion: TTableRevisionNumber): boolean;
        class function CompareVersions(aFirstV, aSecondV: TTableRevisionNumber)
            : TTableRevisionNumberCompareResult;
        class function TableRevisionNumberFromInt(aMajorVersion, aMinorVersion: integer)
            : TTableRevisionNumber;
    end;


implementation


uses
    IniFiles,
    SysUtils;

{ TTableRevisionNumber }

class function TTableRevisionNumber.VersionToStr(aVersion: TTableRevisionNumber): string;
begin
    // the value returned here should also be a valid suffix of a filename... dont use characters that cannot be used in filenames
    result := Format('%d_%d', [aVersion.Major, aVersion.Minor]);
end;

{ TTableVersionInfoAdaptor }

constructor TTableVersionInfoAdaptor.Create(const aTableName: string);
begin
    inherited Create();
    fTableName := aTableName;
end;

class function TTableVersionInfoAdaptor.CompareVersions(aFirstV, aSecondV: TTableRevisionNumber)
    : TTableRevisionNumberCompareResult;
begin
    if (aFirstV.Major > aSecondV.Major) then
        result := rncFirstIsNewer
    else if (aFirstV.Major < aSecondV.Major) then
        result := rncFirstIsOlder
    else
    begin
        if (aFirstV.Minor > aSecondV.Minor) then
            result := rncFirstIsNewer
        else if (aFirstV.Minor < aSecondV.Minor) then
            result := rncFirstIsOlder
        else
            result := rncEqual;
    end;

end;

class function TTableVersionInfoAdaptor.IsVersionMissing(aVersion: TTableRevisionNumber): boolean;
begin
    result := aVersion.Major = INT_MAJORVERSION_NONE;
end;

function TTableVersionInfoAdaptor.ReadVersion(const aPath: string): TTableRevisionNumber;
var
    xIniFile: TIniFile;
    xTablePath: string;
begin
    result.Major := INT_MAJORVERSION_NONE;
    result.Minor := INT_MINORVERSION_NONE;

    xTablePath := aPath;
    if (not FileExists(xTablePath + '.VR')) then
        EXIT;

    xIniFile := TIniFile.Create(xTablePath + '.VR');
    try
        result.Major := xIniFile.ReadInteger(STR_IDENT_VERSION, STR_IDENT_MAJORREVISIONNUMBER, result.Major);
        result.Minor := xIniFile.ReadInteger(STR_IDENT_VERSION, STR_IDENT_MINORREVISIONNUMBER, result.Minor);
    finally
        xIniFile.Free;
    end;
end;

procedure TTableVersionInfoAdaptor.WriteVersion(const aPath: string; aVersion: TTableRevisionNumber);
var
    xIniFile: TIniFile;
begin
    xIniFile := TIniFile.Create(aPath + '.VR');
    try
        xIniFile.WriteInteger(STR_IDENT_VERSION, STR_IDENT_MAJORREVISIONNUMBER, aVersion.Major);
        xIniFile.WriteInteger(STR_IDENT_VERSION, STR_IDENT_MINORREVISIONNUMBER, aVersion.Minor);
        xIniFile.UpdateFile;
    finally
        xIniFile.Free;
    end;
end;

class function TTableVersionInfoAdaptor.TableRevisionNumberFromInt(aMajorVersion, aMinorVersion: integer)
    : TTableRevisionNumber;
begin
    result.Major := aMajorVersion;
    result.Minor := aMinorVersion;
end;


end.
