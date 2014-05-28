{ -----------------------------------------------------------------------------------------------------------------------
  Copyright © 2009 Zinsser Analytic GmbH - All rights reserved.
  Author       : Payman Kamali (pk)
  Description  :
  -----------------------------------------------------------------------------------------------------------------------
  Revision History:
  date     op  method                       track-no improvement/change
  -------- --  ---------------------------  -------- --------------------------------------------------------------------
  06.04.09 pk                                TN4503  Initial Revision
  27.08.09 pk                                TN4753  various changes
  04.11.09 pk                                TN4843  Replace TObjectList, TStringList, TStrings with Generics Lists and TStringArray
  13.11.12 wl                                TN6015   überarbeitet für DisplayComponentsDataAdaptor
  29.11.12 wl                                TN6015.2 DesignDisplayComponents abgeschafft (Es gibt nur noch RunDisplayComponents)
  ----------------------------------------------------------------------------------------------------------------------- }

unit DisplayComponentTypeDictionary;


interface


uses
    DisplayComponentTypeInfo,
    DisplayComponentSettings,
    TypeInfo,
    TypeDictionary;

type
    TDisplayComponentTypeDictionary = class abstract(TTypeDictionary)
    protected
        function IsValidType(aTypeInfo: TTypeInfo): boolean; override;
    end;

    TRunDisplayComponentTypeDictionary = class(TDisplayComponentTypeDictionary)
    private
        class var uInstance: TRunDisplayComponentTypeDictionary;
    protected
        function IsValidType(aTypeInfo: TTypeInfo): boolean; override;
    public
        class procedure CreateInstance();
        class property Instance: TRunDisplayComponentTypeDictionary read uInstance;
        class procedure DestroyInstance();
    end;

    TDisplayComponentSettingsTypeDictionary = class(TTypeDictionary)
    private
        class var uInstance: TDisplayComponentSettingsTypeDictionary;
    protected
        function IsValidType(aTypeInfo: TTypeInfo): boolean; override;
    public
        function ReadCompatibleDisplayComponentTypeNamesByType(aDisplayComponentID: TDisplayComponentID)
            : TArray<string>;
        class procedure CreateInstance();
        class property Instance: TDisplayComponentSettingsTypeDictionary read uInstance;
        class procedure DestroyInstance();
    end;


implementation


uses
    SysUtils,Generics.Collections;

{ TDisplayComponentTypeDictionary }

function TDisplayComponentTypeDictionary.IsValidType(aTypeInfo: TTypeInfo): boolean;
begin
    result := aTypeInfo is TDisplayComponentTypeInfo;
end;

{ TRunDisplayComponentTypeDictionary }

class procedure TRunDisplayComponentTypeDictionary.CreateInstance();
begin
    if Assigned(uInstance) then
        EXIT;
    uInstance := TRunDisplayComponentTypeDictionary.Create('RunDisplayComponent Type Dictionary');
end;

class procedure TRunDisplayComponentTypeDictionary.DestroyInstance();
begin
    FreeAndNil(uInstance);
end;

function TRunDisplayComponentTypeDictionary.IsValidType(aTypeInfo: TTypeInfo): boolean;
begin
    result := aTypeInfo is TRunDisplayComponentTypeInfo;
end;

{ TDisplayComponentSettingsTypeDictionary }

class procedure TDisplayComponentSettingsTypeDictionary.CreateInstance();
begin
    if Assigned(uInstance) then
        EXIT;
    uInstance := TDisplayComponentSettingsTypeDictionary.Create('DisplayComponent Type Dictionary');
end;

class procedure TDisplayComponentSettingsTypeDictionary.DestroyInstance();
begin
    FreeAndNil(uInstance);
end;

function TDisplayComponentSettingsTypeDictionary.IsValidType(aTypeInfo: TTypeInfo): boolean;
begin
    result := aTypeInfo is TDisplayComponentSettingsTypeInfo;
end;

function TDisplayComponentSettingsTypeDictionary.ReadCompatibleDisplayComponentTypeNamesByType
    (aDisplayComponentID: TDisplayComponentID): TArray<string>;
var
    xDisplayComponentType: TDisplayComponentSettingsTypeInfo;
    xTypeNames: TList<string>;
    xTypeInfo: TTypeInfo;
begin
    result := nil;
    xTypeNames := TList<string>.Create;
    try
        for xTypeInfo in fTypeInfos do
        begin
            xDisplayComponentType := xTypeInfo as TDisplayComponentSettingsTypeInfo;
            if xDisplayComponentType.SupportsDisplayComponent(aDisplayComponentID) then
                xTypeNames.Add(xDisplayComponentType.TypeName);
        end;

        xTypeNames.Sort;
        result := xTypeNames.ToArray;

    finally
        FreeAndNil(xTypeNames);
    end;
end;


end.
