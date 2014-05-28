{ ----------------------------------------------------------------------------------------------------------------------
  Copyright  2008 Zinsser Analytic GmbH - All rights reserved.
  Author       : Wolfgang Lyncke (wl)
  Description  : Class that manages a list of type info classes
  ----------------------------------------------------------------------------------------------------------------------
  Revision History:
  date     op  method                       track-no improvement/change
  -------- --  ---------------------------  -------- -------------------------------------------------------------------
  14.04.08 wl                               TN4060    initial version
  21.05.08 wl  fDictionaryName              TN4119    new for Logging
  21.05.08 wl  AddTypes                     TN4119    Logs each TypeInfo that is added
  13.10.08 pk  AddTypes                     TN4272.1  LogFT changed to LogF - faster
  06.04.09 pk  GetTypeFromTypeName          TN4503    use SameText utility instead of uppercase
  04.11.09 pk                               TN4843    Replace TObjectList, TStringList, TStrings with Generics Lists and TStringArray
  19.04.10 wl  AddTypes                     TN5044    ganz ohne Format-Befehl: genauso schnell aber besser übersetzbar
  17.06.10 pk                               TN5152.1  No logging because logging is not available at this point
  27.03.13 wl                               TN6045    uses geändert
  29.11.13 ts  ReadTypeNames                TN6316    sorting not case sensitive
  ---------------------------------------------------------------------------------------------------------------------- }

unit TypeDictionary;


interface


uses
    TypeInfo,
    GeneralTypes;

type
    TTypeDictionary = class
    protected
        fTypeInfos: TTypeInfoList;
        fDictionaryName: string;
        procedure InitTypeInfoList(); virtual;
        function IsValidType(aTypeInfo: TTypeInfo): boolean; virtual;
    public
        constructor Create(const aDictionaryName: string);
        destructor Destroy; override;

        procedure AddTypes(aTypeInfoList: TTypeInfoList);
        function GetTypeFromTypeName(const aTypeName: string): TTypeInfo;
        function ReadTypeNames(aSorted: boolean): TStringArray;

        property TypeInfos: TTypeInfoList read fTypeInfos;
    end;


implementation


uses
    SysUtils,
    Generics.Collections,
    UtilLib;

{ TTypeDictionary }

constructor TTypeDictionary.Create(const aDictionaryName: string);
begin
    inherited Create();
    fDictionaryName := aDictionaryName;
    fTypeInfos := TTypeInfoList.Create(false);
    InitTypeInfoList();
end;

destructor TTypeDictionary.Destroy();
begin
    FreeAndNil(fTypeInfos);
    inherited;
end;

procedure TTypeDictionary.InitTypeInfoList();
begin
end;

function TTypeDictionary.GetTypeFromTypeName(const aTypeName: string): TTypeInfo;
var
    xTypeInfo: TTypeInfo;
begin
    result := nil;

    for xTypeInfo in fTypeInfos do
    begin
        if SameText(xTypeInfo.TypeName, aTypeName) then
        begin
            result := xTypeInfo;
            EXIT;
        end;
    end;

end;

function TTypeDictionary.ReadTypeNames(aSorted: boolean): TStringArray;
var
    xTypeInfo: TTypeInfo;
    xTypeNames: TList<string>;
begin
    result := nil;
    xTypeNames := TList<string>.Create;
    try
        for xTypeInfo in fTypeInfos do
        begin
            xTypeNames.Add(xTypeInfo.TypeName);
        end;

        if (aSorted) then
            xTypeNames.Sort(TCustomTextComparer.Create);

        result := xTypeNames.ToArray;

    finally
        FreeAndNil(xTypeNames);
    end;
end;

function TTypeDictionary.IsValidType(aTypeInfo: TTypeInfo): boolean;
begin
    result := true;
end;

procedure TTypeDictionary.AddTypes(aTypeInfoList: TTypeInfoList);
var
    xTypeInfo: TTypeInfo;
begin
    for xTypeInfo in aTypeInfoList do
    begin
        if IsValidType(xTypeInfo) then
        begin
            fTypeInfos.Add(xTypeInfo);
            // if Assigned( TLogManager.Instance ) then
            // TLogManager.Instance.Log( fDictionaryName + ', Type Info Added: ' + xTypeInfo.LogText, false );
        end;
    end;
end;


end.
