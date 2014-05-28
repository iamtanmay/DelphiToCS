{ --------------------------------------------------------------------------------------------------
  Copyright © 2008 Zinsser Analytic GmbH - All rights reserved.
  Author       : Payman Kamali (pk)
  Description  :
  --------------------------------------------------------------------------------------------------
  Revision History:
  date     op  method                       track-no improvement/change
  -------- --  ---------------------------  -------- -----------------------------------------------
  23.06.08 pk                                TN4139  Initial Revision
  02.09.09 pk  Create                        TN4753  Reads Graphics type from settings
  04.11.09 pk                                TN4843  Replace TObjectList, TStringList, TStrings with Generics Lists and TStringArray
  11.04.14 ts                                TN6396  3D Bug entfernt
  -------------------------------------------------------------------------------------------------- }

unit LayoutElementGraphicsDriverTypeManager;


interface


uses
    IntfLayoutElementGraphicsDriver,
    LayoutElementGraphicsInfo;

type

    TLayoutElementGraphicsDriverTypeManager = class
    private
        procedure SetCurrentGraphicsType(const aValue: string);
    protected
        fCurrentGraphicsType: string;
    public
        constructor Create();
        function CreateGraphicsDriver(aDriverID: TLayoutElementGraphicsDriverID)
            : ILayoutElementGraphicsDriver;
        class procedure CreateInstance;
        class procedure DestroyInstance;
        class function Instance: TLayoutElementGraphicsDriverTypeManager;
        class procedure SetInstance(aLayoutElementGraphicsDriverTypeManager
            : TLayoutElementGraphicsDriverTypeManager);
        property CurrentGraphicsType: string read fCurrentGraphicsType write SetCurrentGraphicsType;
    end;


implementation


uses
    SysUtils,
    TypeInfo,
    LayoutElementGraphicsDriverTypeDictionary,
    PluginLoader,
    CommonTypes,
    AppSettings;

{ TLayoutElementGraphicsDriverTypeManager }

var
    uLayoutElementGraphicsDriverTypeManagerInstance: TLayoutElementGraphicsDriverTypeManager = nil;

class procedure TLayoutElementGraphicsDriverTypeManager.SetInstance(aLayoutElementGraphicsDriverTypeManager
    : TLayoutElementGraphicsDriverTypeManager);
begin
    uLayoutElementGraphicsDriverTypeManagerInstance := aLayoutElementGraphicsDriverTypeManager;
end;

class procedure TLayoutElementGraphicsDriverTypeManager.CreateInstance();
begin
    SetInstance(TLayoutElementGraphicsDriverTypeManager.Create());
    TPluginLoader.LoadAllTypes(gLayoutElementGraphicsDriverTypeDictionary);
end;

class procedure TLayoutElementGraphicsDriverTypeManager.DestroyInstance();
begin
    uLayoutElementGraphicsDriverTypeManagerInstance.Free;
end;

class function TLayoutElementGraphicsDriverTypeManager.Instance(): TLayoutElementGraphicsDriverTypeManager;
begin
    result := uLayoutElementGraphicsDriverTypeManagerInstance;
end;

constructor TLayoutElementGraphicsDriverTypeManager.Create;
var
    xIniAccess: IWinlissyIniAccess;
begin
    inherited Create();

    xIniAccess := gCommonDll.CreateAppIni;
    fCurrentGraphicsType := xIniAccess.ReadString('Graphics', 'GraphicsType');
end;

procedure TLayoutElementGraphicsDriverTypeManager.SetCurrentGraphicsType(const aValue: string);
begin
    fCurrentGraphicsType := aValue;
end;

function TLayoutElementGraphicsDriverTypeManager.CreateGraphicsDriver
    (aDriverID: TLayoutElementGraphicsDriverID): ILayoutElementGraphicsDriver;
var
    xDriver: ILayoutElementGraphicsDriver;
    xTypeInfo: TTypeInfo;
    xLayoutTypeInfo: TLayoutElementGraphicsDriverTypeInfo;
begin
    result := nil;
    for xTypeInfo in gLayoutElementGraphicsDriverTypeDictionary.TypeInfos do
    begin
        xLayoutTypeInfo := xTypeInfo as TLayoutElementGraphicsDriverTypeInfo;
        if not SameText(xLayoutTypeInfo.GraphicsType, fCurrentGraphicsType) then
            CONTINUE;
        if not xLayoutTypeInfo.SupportsDriver(aDriverID) then
            CONTINUE;
        xDriver := xLayoutTypeInfo.CreateDriver();
        if not Supports(xDriver, aDriverID, result) then
            CONTINUE;
        result.SetSettings(xLayoutTypeInfo.CreateDriverSettings());
        BREAK;
    end;
end;


end.
