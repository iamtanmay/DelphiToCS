{ ----------------------------------------------------------------------------------------------------------------------
  Copyright  2008 Zinsser Analytic GmbH - All rights reserved.
  Author       : Wolfgang Lyncke (wl)
  Description  :
  ----------------------------------------------------------------------------------------------------------------------
  Revision History:
  date     op  method                       track-no improvement/change
  -------- --  ---------------------------  -------- -------------------------------------------------------------------
  14.04.08 wl                               TN4060    initial version
  21.05.08 wl  GetLogText                   TN4119    new for Logging
  13.10.08 pk  GetLogText                   TN4272.1  LogFT changed to LogF - faster
  04.11.09 pk                               TN4843    Replace TObjectList, TStringList, TStrings with Generics Lists and TStringArray
  19.04.10 wl  GetLogText                   TN5044    ganz ohne Format-Befehl: genauso schnell aber besser übersetzbar
  22.09.11 wl                               TN5696   TTypeInfoList = class(TObjectList<TTypeInfo>)
  ---------------------------------------------------------------------------------------------------------------------- }

unit TypeInfo;


interface


uses
    Generics.Collections;

type
    TTypeInfo = class
    private
        fTypeName: string;
        fTypeInfoVersion: string;
        fLibName: string;
        fLibVersion: string;
    protected
        function GetLogText: string; virtual;
    public
        constructor Create(const aTypeName, aTypeInfoVersion, aLibName, aLibVersion: string);

        property LogText: string read GetLogText;
        property TypeName: string read fTypeName write fTypeName;
        property TypeInfoVersion: string read fTypeInfoVersion write fTypeInfoVersion;
        property LibName: string read fLibName write fLibName;
        property LibVersion: string read fLibVersion write fLibVersion;
    end;

    TTypeInfoList = class(TObjectList<TTypeInfo>)
    end;


implementation


uses
    SysUtils;

{ TTypeInfo }

constructor TTypeInfo.Create(const aTypeName, aTypeInfoVersion, aLibName, aLibVersion: string);
begin
    inherited Create();

    fTypeName := aTypeName;
    fTypeInfoVersion := aTypeInfoVersion;
    fLibName := aLibName;
    fLibVersion := aLibVersion;
end;

function TTypeInfo.GetLogText: string;
begin
    result := fTypeName + ' (Version: ' + fTypeInfoVersion + '), ' + fLibName + ' (Version: ' +
        fLibVersion + ')';
end;


end.
