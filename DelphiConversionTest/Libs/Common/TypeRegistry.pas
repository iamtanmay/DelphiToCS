{ -----------------------------------------------------------------------------------------------------------------------
  Copyright © 2010 Zinsser Analytic GmbH - All rights reserved.
  Author       : Payman Kamali (pk)
  Description  : Register typenames here so that we dont have to encode unit names into the streamable classes
  -----------------------------------------------------------------------------------------------------------------------
  Revision History:
  date     op  method                                track-no  improvement/change
  -------- --  ------------------------------------  --------- ----------------------------------------------------------
  19.10.10 pk                                        TN5305   Initial revision
  29.08.11 wl  class destructor Destroy              TN5672   Memory leak fixed
  11.04.13 wl                                        TN6045   TObjectDictionary statt TDictionary
  ----------------------------------------------------------------------------------------------------------------------- }

unit TypeRegistry;


interface


uses
    Generics.Collections;

type
    TTypeRegistryItem = class
    strict private
        fName: string;
        fUnitName: string;
        function GetFullName: string;
    public
        constructor Create(const aName: string; const aUnitName: string);
        property name: string read fName;
        property FullName: string read GetFullName;
    end;

    TTypeRegistry = class sealed
    strict private
        fList: TObjectDictionary<string, TTypeRegistryItem>;
        class var uInstance: TTypeRegistry; // Single Instance
        class function GetInstance(): TTypeRegistry; static;

        constructor Create();
        class constructor Create();
        class destructor Destroy();
    public
        destructor Destroy(); override;

        procedure RegisterType(const aClass: TClass);
        procedure RegisterTypes(const aClasses: array of TClass);

        function FindFullNameByName(const aName: string; out oFullName: string): boolean;
        class property Instance: TTypeRegistry read GetInstance;
        class procedure InstanceRegisterTypes(const aClasses: array of TClass);
    end;


implementation


uses
    SysUtils;

{ TTypeRegistryItem }

constructor TTypeRegistryItem.Create(const aName, aUnitName: string);
begin
    inherited Create();
    fName := aName;
    fUnitName := aUnitName;
end;

function TTypeRegistryItem.GetFullName: string;
begin
    result := fUnitName + '.' + fName;
end;

{ TTypeRegistry }

constructor TTypeRegistry.Create();
begin
    inherited Create();
    fList := TObjectDictionary<string, TTypeRegistryItem>.Create([doOwnsValues]);
end;

class constructor TTypeRegistry.Create;
begin
    // create instance if instance does not exist
    if not Assigned(uInstance) then
        uInstance := TTypeRegistry.Create();
end;

destructor TTypeRegistry.Destroy;
begin
    FreeAndNil(fList);

    inherited;
end;

class destructor TTypeRegistry.Destroy;
begin
    FreeAndNil(uInstance);
end;

class function TTypeRegistry.GetInstance: TTypeRegistry;
begin
    result := uInstance;
end;

class procedure TTypeRegistry.InstanceRegisterTypes(const aClasses: array of TClass);
begin
    TTypeRegistry.Instance.RegisterTypes(aClasses);
end;

procedure TTypeRegistry.RegisterType(const aClass: TClass);
begin
    fList.Add(aClass.ClassName, TTypeRegistryItem.Create(aClass.ClassName, aClass.UnitName));
end;

procedure TTypeRegistry.RegisterTypes(const aClasses: array of TClass);
var
    x: integer;
begin
    for x := 0 to high(aClasses) do
        RegisterType(aClasses[x]);
end;

function TTypeRegistry.FindFullNameByName(const aName: string; out oFullName: string): boolean;
var
    xRegistryItem: TTypeRegistryItem;
begin
    oFullName := aName;
    result := fList.TryGetValue(aName, xRegistryItem);
    if not result then
        EXIT;

    oFullName := xRegistryItem.FullName;
end;


end.
