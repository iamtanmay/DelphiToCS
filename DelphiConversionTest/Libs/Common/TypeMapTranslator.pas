{ -----------------------------------------------------------------------------------------------------------------------
  Copyright © 2010 Zinsser Analytic GmbH - All rights reserved.
  Author       : Payman Kamali (pk)
  Description  :
  -----------------------------------------------------------------------------------------------------------------------
  Revision History:
  date     op  method                                track-no  improvement/change
  -------- --  ------------------------------------  --------- ----------------------------------------------------------
  04.02.10 pk                                        TN4972    Initial Revision, some code from XMLReaderWriter
  07.05.10 pk  CopyObject                            TN5092    Exit if not assigned
  17.05.10 pk                                        TN5092    Changes needed to allow nil elements in a list
  04.08.10 pk                                        TN5218    better error handling
  13.09.10 pk  CreateInstanceFromTypeMap             TN5218    retry when type not found
  19.10.10 pk                                        TN5305    changes needed for CoreClient/Server
  15.11.10 pk                                        TN5340    Changes to prevent memory leak
  ----------------------------------------------------------------------------------------------------------------------- }

unit TypeMapTranslator;


interface


uses
    TypInfo,
    RTTI,
    Classes,
    Streamable,
    TypeMap;

type

    TObjectToTypeMapTranslator = class
    private
        fCtx: TRttiContext;
        procedure SetFloatToMapItem(const aValue: extended; const aMapItem: TMapItem);
        procedure SetInt64ToMapItem(const aValue: int64; const aMapItem: TMapItem);
        procedure SetIntToMapItem(const aValue: integer; const aMapItem: TMapItem);
        procedure SetVariantToMapItem(const aValue: variant; const aMapItem: TMapItem);
        procedure SetStringToMapItem(const aValue: string; const aMapItem: TMapItem);
        procedure SetUnicodeStringToMapItem(const aValue: unicodestring; const aMapItem: TMapItem);
        procedure SetAnsiStringToMapItem(const aValue: ansistring; const aMapItem: TMapItem);
        procedure SetWideStringToMapItem(const aValue: widestring; const aMapItem: TMapItem);
        procedure SetEnumToMapItem(const aValue: string; const aMapItem: TMapItem);

        function GetObjectTypeName(const aObject: TObject): string;
        procedure SetValueTypeToMapItem(const aValueType: TMapKind; const aMapItem: TMapItem);
        procedure SetValueTypeNameToMapItem(const aValueTypeName: string; const aMapItem: TMapItem);
        procedure SetObjectToTypeMapIntern(const aObject: TObject; const aTypeMap: TTypeMap);
        procedure SetListItemsToTypeMap(const aObject: TObject; const aTypeMap: TTypeMap);
        procedure SetPropertyToTypeMap(const aObject: TObject; const aProperty: TRttiProperty;
            const aTypeMap: TTypeMap);
        procedure SetPropertiesToTypeMap(const aObject: TObject; const aTypeMap: TTypeMap);
        procedure SetObjectToTypeMap(const aObject: TObject; const aTypeMap: TTypeMap);
    public
        constructor Create();
        destructor Destroy(); override;
        function CreateTypeMap(const aObject: TObject): TTypeMap;
    end;

    TObjectFromTypeMapTranslator = class
    private
        fCtx: TRttiContext;
        procedure CreateInstanceFromTypeMap(const aTypeMap: TTypeMap; var vInstance: TObject);
        procedure GetObjectPropFromMapItem(const aMapItem: TMapItem; var vValue: TValue;
            out oIsNewValue: boolean);
        procedure GetPropertyFromTypeMap(const aInstance: TObject; const aProperty: TRttiProperty;
            const aTypeMap: TTypeMap);
        procedure GetPropertyValueFromMapItem(const aInstance: TObject; const aProperty: TRttiProperty;
            const aMapItem: TMapItem);
        function GetIntegerValueFromMapItem(const aMapItem: TMapItem): integer;
        function GetInt64ValueFromMapItem(const aMapItem: TMapItem): int64;
        function GetFloatValueFromMapItem(const aMapItem: TMapItem): extended;
        function GetStringValueFromMapItem(const aMapItem: TMapItem): string;
        function GetAnsiStringValueFromMapItem(const aMapItem: TMapItem): ansistring;
        function GetWideStringValueFromMapItem(const aMapItem: TMapItem): widestring;
        function GetUnicodeStringValueFromMapItem(const aMapItem: TMapItem): unicodestring;
        function GetEnumValueFromMapItem(const aMapItem: TMapItem): string;
        function GetVariantValueFromMapItem(const aMapItem: TMapItem): variant;

        procedure GetPropertiesFromTypeMap(const aInstance: TObject; const aTypeMap: TTypeMap);
        procedure GetObjectFromTypeMap(var vInstance: TObject; const aTypeMap: TTypeMap);
        procedure GetObjectFromTypeMapListItemsProp(const aInstance: TObject; const aTypeMap: TTypeMap);
        function GetValueTypeFromMapItem(const aMapItem: TMapItem): TMapKind;
        function GetValueTypeNameFromMapItem(const aMapItem: TMapItem): string;
    public
        constructor Create();
        destructor Destroy(); override;
        function CreateObject<T: class>(const aTypeMap: TTypeMap): T;
    end;

    TObjectCopy<T: class> = class
    private
        class function CreateFromTypeMap(const aValue: TTypeMap): T;
        class function CopyObject(const aValue: T): T;
    public
        class function Copy(const aValue: T): T;
    end;


implementation


uses
    SysUtils,
    Variants,
    Windows,
    TypeRegistry;

const
    cValueTypeObject = 'Object';
    cValueTypeNil = 'Nil';
    cValueTypeList = 'List';

constructor TObjectToTypeMapTranslator.Create();
begin
    inherited Create();
    fCtx := TRttiContext.Create();
end;

destructor TObjectToTypeMapTranslator.Destroy;
begin
    fCtx.Free;
    inherited;
end;

procedure TObjectToTypeMapTranslator.SetValueTypeToMapItem(const aValueType: TMapKind;
    const aMapItem: TMapItem);
var
    xTypeValueAsStr: string;
begin
    xTypeValueAsStr := '';
    case aValueType of
        makObject:
            xTypeValueAsStr := 'Object';
        makNil:
            xTypeValueAsStr := 'Nil';
        makList:
            xTypeValueAsStr := 'List';
        // vaInt16     : xTypeValueAsStr := 'Int16';
        // vaInt32     : xTypeValueAsStr := 'Int32';
        // vaInt64     : xTypeValueAsStr := 'Int64';
        // vaSingle    : xTypeValueAsStr := 'Single';
        // vaDouble    : xTypeValueAsStr := 'Double';
        // vaExtended  : xTypeValueAsStr := 'Extended';
        // vaString    : xTypeValueAsStr := 'String';
        // vaLString   : xTypeValueAsStr := 'LString';
        // vaWString   : xTypeValueAsStr := 'WString';
        // vaUTF8String: xTypeValueAsStr := 'UString';
        // vaDate      : xTypeValueAsStr := 'Date';
        // vaSet       : xTypeValueAsStr := 'Set';
        // vaFalse     : xTypeValueAsStr := 'False';
        // vaTrue      : xTypeValueAsStr := 'True';
        // vaNull      : xTypeValueAsStr := 'Null';
    end;

    aMapItem.ValueType := xTypeValueAsStr;
end;

procedure TObjectToTypeMapTranslator.SetValueTypeNameToMapItem(const aValueTypeName: string;
    const aMapItem: TMapItem);
begin
    aMapItem.ValueTypeName := aValueTypeName;
end;

procedure TObjectToTypeMapTranslator.SetFloatToMapItem(const aValue: extended; const aMapItem: TMapItem);
begin
    // SetValueTypeToMapItem( vaExtended, aMapItem );
    aMapItem.Value := aValue;
end;

procedure TObjectToTypeMapTranslator.SetIntToMapItem(const aValue: integer; const aMapItem: TMapItem);
begin
    // SetValueTypeToMapItem( vaInt32, aMapItem );
    aMapItem.Value := aValue;
end;

procedure TObjectToTypeMapTranslator.SetInt64ToMapItem(const aValue: int64; const aMapItem: TMapItem);
begin
    // SetValueTypeToMapItem( vaInt64, aMapItem );
    aMapItem.Value := aValue;
end;

procedure TObjectToTypeMapTranslator.SetWideStringToMapItem(const aValue: widestring;
    const aMapItem: TMapItem);
begin
    // SetValueTypeToMapItem( vaWString, aMapItem );
    aMapItem.Value := aValue;
end;

procedure TObjectToTypeMapTranslator.SetAnsiStringToMapItem(const aValue: ansistring;
    const aMapItem: TMapItem);
begin
    // SetValueTypeToMapItem( vaLString, aMapItem );
    aMapItem.Value := aValue;
end;

procedure TObjectToTypeMapTranslator.SetUnicodeStringToMapItem(const aValue: unicodestring;
    const aMapItem: TMapItem);
begin
    // SetValueTypeToMapItem( vaUTF8String, aMapItem );
    aMapItem.Value := aValue;
end;

procedure TObjectToTypeMapTranslator.SetStringToMapItem(const aValue: string; const aMapItem: TMapItem);
begin
    // SetValueTypeToMapItem( vaString, aMapItem );
    aMapItem.Value := aValue;
end;

procedure TObjectToTypeMapTranslator.SetEnumToMapItem(const aValue: string; const aMapItem: TMapItem);
begin
    // SetValueTypeToMapItem( va?, aMapItem );
    aMapItem.Value := aValue;
end;

procedure TObjectToTypeMapTranslator.SetVariantToMapItem(const aValue: variant; const aMapItem: TMapItem);
begin
    aMapItem.Value := aValue;
end;

procedure TObjectToTypeMapTranslator.SetPropertyToTypeMap(const aObject: TObject;
    const aProperty: TRttiProperty; const aTypeMap: TTypeMap);
var
    xPropertyType: TRttiType;
    xTypeKind: TTypeKind;
    xPropValue: TValue;
    xMapItemName: string;
    xMapItem: TMapItem;

    procedure WriteIntProp();
    var
        Value: integer;
    begin
        Value := xPropValue.AsInteger;
        SetIntToMapItem(Value, xMapItem);
    end;

    procedure WriteOrdProp;
    var
        Value: Int64;
    begin

        Value := xPropValue.AsOrdinal;

        case xTypeKind of
            tkSet, tkChar:
                ASSERT('tkSet no implemnted');
            tkEnumeration:
                SetEnumToMapItem(GetEnumName(xPropValue.TypeInfo, Value), xMapItem);
        end;
    end;

    procedure WriteFloatProp;
    var
        Value: Extended;
    begin

        Value := xPropValue.AsExtended;
        SetFloatToMapItem(Value, xMapItem);
    end;

    procedure WriteInt64Prop;
    var
        Value: Int64;
    begin
        Value := xPropValue.AsInt64;
        SetInt64ToMapItem(Value, xMapItem);
    end;

    procedure WriteUnicodeStrProp;
    var
        Value: unicodestring;
    begin
        Value := xPropValue.AsString;
        SetUnicodeStringToMapItem(Value, xMapItem);
    end;

    procedure WriteWideStrProp;
    var
        Value: widestring;
    begin
        Value := xPropValue.AsString;
        SetWideStringToMapItem(Value, xMapItem);
    end;

    procedure WriteAnsiStrProp;
    var
        Value: ansistring;
    begin
        Value := ansistring(xPropValue.AsString);
        SetAnsiStringToMapItem(Value, xMapItem);
    end;
    procedure WriteStrProp;
    var
        Value: string;
    begin
        Value := xPropValue.AsString;
        SetStringToMapItem(Value, xMapItem);
    end;

    procedure WriteVariantProp;
    var
        xValue: Variant;
    begin
        xValue := xPropValue.AsVariant;
        SetVariantToMapItem(xValue, xMapItem);
    end;

    procedure WriteObjectProp();
    var
        xValue: TObject;
    begin
        xValue := xPropValue.AsObject;
        ASSERT(xMapItem is TTypeMap, Format('MapItem %s is not an object', [xMapItem.Name]));
        SetObjectToTypeMap(xValue, xMapItem as TTypeMap);

    end;

begin
    try
        xPropertyType := aProperty.PropertyType;
        if not xPropertyType.IsPublicType then
            EXIT;
        if not aProperty.IsReadable then
            EXIT;

        xTypeKind := xPropertyType.TypeKind;
        xMapItemName := aProperty.Name;

        if xTypeKind = tkClass then
            xMapItem := aTypeMap.AddTypeMap(xMapItemName)
        else
            xMapItem := aTypeMap.AddItem(xMapItemName);

        xPropValue := aProperty.GetValue(aObject);

        case xTypeKind of
            tkInteger:
                WriteIntProp;
            tkChar, tkEnumeration, tkSet:
                WriteOrdProp;
            tkFloat:
                WriteFloatProp;
            tkString:
                WriteStrProp;
            tkLString:
                WriteAnsiStrProp;
            tkWString:
                WriteWideStrProp;
            tkuString:
                WriteUnicodeStrProp;
            tkClass:
                WriteObjectProp;
            tkVariant:
                WriteVariantProp;
            tkInt64:
                WriteInt64Prop;
        end;
    except
        on e: exception do
        begin
            raise Exception.Create(Format('SetPropertyToTypeMap: Error in Property %s -' + #13#10 + '%s',
                [aProperty.Name, e.Message]));
        end;
    end;
end;

procedure TObjectToTypeMapTranslator.SetListItemsToTypeMap(const aObject: TObject; const aTypeMap: TTypeMap);

var
    xListType: TRttiType;
    xGetEnumMeth: TRttiMethod;
    xEnum: TObject;
    xEnumType: TRttiType;

    xEnumNextMeth: TRttiMethod;
    xEnumCurrentProp: TRttiProperty;

begin
    xListType := fCtx.GetType(aObject.ClassInfo);
    xGetEnumMeth := xListType.GetMethod('GetEnumerator');

    if not Assigned(xGetEnumMeth) then
        EXIT;
    xEnum := xGetEnumMeth.Invoke(aObject, []).AsObject;
    try
        xEnumType := fCtx.GetType(xEnum.ClassInfo);
        xEnumCurrentProp := xEnumType.GetProperty('Current');
        xEnumNextMeth := xEnumType.GetMethod('MoveNext');

        while xEnumNextMeth.Invoke(xEnum, []).AsBoolean do
        begin
            SetPropertyToTypeMap(xEnum, xEnumCurrentProp, aTypeMap);
        end;
    finally
        FreeAndNil(xEnum);
    end;

end;

procedure TObjectToTypeMapTranslator.SetPropertiesToTypeMap(const aObject: TObject; const aTypeMap: TTypeMap);
var
    xProps: TArray<TRttiProperty>;
    xProp: TRttiProperty;
    xObjType: TRttiType;

begin
    xObjType := fCtx.GetType(aObject.ClassInfo);

    xProps := xObjType.GetProperties;
    for xProp in xProps do
    begin
        if not(xProp.Visibility in [mvPublished]) then
            CONTINUE;
        SetPropertyToTypeMap(aObject, xProp, aTypeMap);
    end;
    SetListItemsToTypeMap(aObject, aTypeMap);

end;

function TObjectToTypeMapTranslator.GetObjectTypeName(const aObject: TObject): string;
var
    xDummy: string;
begin
    result := aObject.ClassName;
    // if it is in registry dont need unit name
    if TTypeRegistry.Instance.FindFullNameByName(result, xDummy) then
        EXIT;
    result := aObject.UnitName + '.' + result;
end;

procedure TObjectToTypeMapTranslator.SetObjectToTypeMapIntern(const aObject: TObject;
    const aTypeMap: TTypeMap);
var
    xTypeName: string;
begin
    aTypeMap.Clear();

    if not Assigned(aObject) then
    begin
        SetValueTypeToMapItem(makNil, aTypeMap);
        EXIT;
    end;

    SetValueTypeToMapItem(makObject, aTypeMap);
    xTypeName := GetObjectTypeName(aObject);
    SetValueTypeNameToMapItem(xTypeName, aTypeMap);

    if aObject is TObject then
        SetPropertiesToTypeMap(aObject as TObject, aTypeMap);

end;

procedure TObjectToTypeMapTranslator.SetObjectToTypeMap(const aObject: TObject; const aTypeMap: TTypeMap);
begin

    SetObjectToTypeMapIntern(aObject, aTypeMap);
end;

function TObjectToTypeMapTranslator.CreateTypeMap(const aObject: TObject): TTypeMap;
var
    xName: string;
begin
    result := nil;
    if not Assigned(aObject) then
        EXIT;
    xName := aObject.ClassName;

    result := TTypeMap.Create(xName);
    SetObjectToTypeMap(aObject, result);
end;

{ TObjectFromTypeMapTranslator }

constructor TObjectFromTypeMapTranslator.Create();
begin
    inherited Create();
    fCtx := TRttiContext.Create();
end;

destructor TObjectFromTypeMapTranslator.Destroy;
begin
    fCtx.Free;
    inherited;
end;

function TObjectFromTypeMapTranslator.GetIntegerValueFromMapItem(const aMapItem: TMapItem): integer;
begin
    result := 0;
    if aMapItem.IsValueEmpty then
        EXIT;
    result := aMapItem.Value;
end;

function TObjectFromTypeMapTranslator.GetInt64ValueFromMapItem(const aMapItem: TMapItem): int64;
begin
    result := 0;
    if aMapItem.IsValueEmpty then
        EXIT;
    result := aMapItem.Value;
end;

function TObjectFromTypeMapTranslator.GetFloatValueFromMapItem(const aMapItem: TMapItem): extended;
begin
    result := 0.0;
    if aMapItem.IsValueEmpty then
        EXIT;
    result := aMapItem.Value;
end;

function TObjectFromTypeMapTranslator.GetStringValueFromMapItem(const aMapItem: TMapItem): string;
begin
    result := '';
    if aMapItem.IsValueEmpty then
        EXIT;
    result := aMapItem.Value;
end;

function TObjectFromTypeMapTranslator.GetAnsiStringValueFromMapItem(const aMapItem: TMapItem): ansistring;
begin
    result := '';
    if aMapItem.IsValueEmpty then
        EXIT;
    result := ansistring(aMapItem.Value);
end;

function TObjectFromTypeMapTranslator.GetWideStringValueFromMapItem(const aMapItem: TMapItem): widestring;
begin
    result := '';
    if aMapItem.IsValueEmpty then
        EXIT;
    result := aMapItem.Value;
end;

function TObjectFromTypeMapTranslator.GetUnicodeStringValueFromMapItem(const aMapItem: TMapItem)
    : unicodestring;
begin
    result := '';
    if aMapItem.IsValueEmpty then
        EXIT;
    result := aMapItem.Value;
end;

function TObjectFromTypeMapTranslator.GetEnumValueFromMapItem(const aMapItem: TMapItem): string;
begin
    result := '';
    if aMapItem.IsValueEmpty then
        EXIT;
    result := aMapItem.Value;
end;

function TObjectFromTypeMapTranslator.GetVariantValueFromMapItem(const aMapItem: TMapItem): variant;

begin
    VarClear(result);
    if aMapItem.IsValueEmpty then
        EXIT;
    result := aMapItem.Value;
end;

function TObjectFromTypeMapTranslator.GetValueTypeNameFromMapItem(const aMapItem: TMapItem): string;
var
    xFullName: string;
begin
    result := aMapItem.ValueTypeName;

    if not TTypeRegistry.Instance.FindFullNameByName(result, xFullName) then
        EXIT;
    result := xFullName;

end;

function TObjectFromTypeMapTranslator.GetValueTypeFromMapItem(const aMapItem: TMapItem): TMapKind;
var
    xTypeValueAsStr: string;
begin
    result := makNone;
    xTypeValueAsStr := aMapItem.ValueType;
    if SameText(xTypeValueAsStr, cValueTypeObject) then
        result := makObject
    else if SameText(xTypeValueAsStr, cValueTypeList) then
        result := makList
    else if SameText(xTypeValueAsStr, cValueTypeNil) then
        result := makNil
        // else if SameText( xTypeValueAsStr, 'Int16' ) then
        // result := vaInt16
        // else if SameText( xTypeValueAsStr, 'Int32' ) then
        // result := vaInt32
        // else if SameText( xTypeValueAsStr, 'Int64' ) then
        // result := vaInt64
        // else if SameText( xTypeValueAsStr, 'Extended' ) then
        // result := vaExtended
        // else if SameText( xTypeValueAsStr, 'String' ) then
        // result := vaString
        // else if SameText( xTypeValueAsStr, 'WString' ) then
        // result := vaWString
        // else if SameText( xTypeValueAsStr, 'Set' ) then
        // result := vaSet
        // else if SameText( xTypeValueAsStr, 'False' ) then
        // result := vaFalse
        // else if SameText( xTypeValueAsStr, 'True' ) then
        // result := vaTrue

end;

procedure TObjectFromTypeMapTranslator.CreateInstanceFromTypeMap(const aTypeMap: TTypeMap;
    var vInstance: TObject);
var
    xMapKind: TMapKind;
    xValueTypeName: string;
    t: TRttiInstanceType;
    xMethod, xParameterlessConstructor: TRttiMethod;
    xValue: TValue;
    xMethods: TArray<TRttiMethod>;
    xParams: TArray<TRttiParameter>;
    xType: TRttiType;
    x: integer;
const
    cFindTypeNumTries = 10; // we need to retry because of Delphi type library multithreading problems

begin
    if Assigned(vInstance) then
        EXIT;

    xMapKind := GetValueTypeFromMapItem(aTypeMap);
    if xMapKind = makNil then
    begin
        // vInstance := nil;
        EXIT;
    end;

    ASSERT(xMapKind = makObject);
    xValueTypeName := GetValueTypeNameFromMapItem(aTypeMap);

    xType := nil;
    for x := 0 to cFindTypeNumTries do
    begin
        try
            xType := fCtx.FindType(xValueTypeName);
        except
            // xCtx : TRttiContext;
        end;
        if xType is TRttiInstanceType then
            Break;
    end;

    ASSERT(xType is TRttiInstanceType, Format('Type %s not found', [xValueTypeName]));
    t := (xType as TRttiInstanceType);
    try
        xMethods := t.GetMethods('Create');
        xParameterlessConstructor := nil;
        for xMethod in xMethods do
        begin
            xParams := xMethod.GetParameters();
            if Length(xParams) = 0 then
            begin
                xParameterlessConstructor := xMethod;
                BREAK;
            end;
        end;

        ASSERT(Assigned(xParameterlessConstructor));
        ASSERT(xParameterlessConstructor.IsConstructor);
        xValue := xParameterlessConstructor.Invoke(t.MetaclassType, []);
        vInstance := xValue.AsObject as TObject;
    except
        on E: Exception do
            raise Exception.CreateFmt('%s - %s', [xValueTypeName, e.Message]);

    end;
end;

procedure TObjectFromTypeMapTranslator.GetObjectPropFromMapItem(const aMapItem: TMapItem; var vValue: TValue;
    out oIsNewValue: boolean);
var
    xObject: TObject;
    xObjectAsPersist: TObject;

begin
    oIsNewValue := vValue.IsEmpty;
    if oIsNewValue then
        vValue := nil;

    if GetValueTypeFromMapItem(aMapItem) = makNil then
        EXIT;

    if not(aMapItem is TTypeMap) then
        raise Exception.CreateFmt('MapItem %s is not a TypeMap', [aMapItem.Name]);

    xObjectAsPersist := nil;
    if not oIsNewValue then
    begin
        ASSERT(vValue.IsObject);
        xObject := vValue.AsObject;
        ASSERT(xObject is TObject);
        xObjectAsPersist := xObject as TObject;
    end;

    GetObjectFromTypeMap(xObjectAsPersist, aMapItem as TTypeMap);

    if oIsNewValue then
        vValue := xObjectAsPersist; // TValue.Make( xObjectAsPersist, PropType.Handle, xValue );
end;

procedure TObjectFromTypeMapTranslator.GetPropertyValueFromMapItem(const aInstance: TObject;
    const aProperty: TRttiProperty; const aMapItem: TMapItem);
var
    xPropType: TRTTIType;

    xTypeKind: TTypeKind;

    xValue: TValue;
    xIsNewValue: boolean;
begin

    xPropType := aProperty.PropertyType;
    xTypeKind := xPropType.TypeKind;

    xIsNewValue := true;

    // if not aProperty.IsWritable then EXIT;
    case xTypeKind of
        tkClass:
            begin
                xValue := aProperty.GetValue(aInstance);
                GetObjectPropFromMapItem(aMapItem, xValue, xIsNewValue);
            end;
        tkEnumeration:
            xValue := TValue.FromOrdinal(xPropType.Handle, GetEnumValue(xPropType.Handle,
                GetEnumValueFromMapItem(aMapItem)));
        tkInteger:
            xValue := GetIntegerValueFromMapItem(aMapItem);
        tkFloat:
            xValue := GetFloatValueFromMapItem(aMapItem);
        tkString:
            xValue := GetStringValueFromMapItem(aMapItem);
        tkLString:
            xValue := string(GetAnsiStringValueFromMapItem(aMapItem));
        tkuString:
            xValue := GetUnicodeStringValueFromMapItem(aMapItem);
        tkWString:
            xValue := GetWideStringValueFromMapItem(aMapItem);
        tkInt64:
            xValue := GetInt64ValueFromMapItem(aMapItem);
        tkVariant:
            xValue := TValue.FromVariant(GetVariantValueFromMapItem(aMapItem));
        else
            ASSERT(false, 'type not found');
    end;

    if xIsNewValue then
        aProperty.SetValue(aInstance, xValue);

end;

procedure TObjectFromTypeMapTranslator.GetPropertyFromTypeMap(const aInstance: TObject;
    const aProperty: TRttiProperty; const aTypeMap: TTypeMap);
var
    xPropName: string;
    xPropertyType: TRttiType;
    xMapItem: TMapItem;
begin
    try
        if not(aProperty.Visibility in [mvPublished]) then
            EXIT;
        xPropName := aProperty.Name;

        xPropertyType := aProperty.PropertyType;
        if not xPropertyType.IsPublicType then
            EXIT;

        xMapItem := aTypeMap.Find(xPropName);
        if not Assigned(xMapItem) then
            EXIT;

        self.GetPropertyValueFromMapItem(aInstance, aProperty, xMapItem);

    except
        on E: Exception do
            raise Exception.Create(xPropName + ' - ' + E.Message);
    end;
end;

procedure TObjectFromTypeMapTranslator.GetObjectFromTypeMapListItemsProp(const aInstance: TObject;
    const aTypeMap: TTypeMap);
var
    xListType: TRttiType;
    xGetEnumMeth: TRttiMethod;
    xAddMeth: TRttiMethod;
    x: integer;
    xObject: TObject;
    xMapItem: TMapItem;

begin
    xListType := fCtx.GetType(aInstance.ClassInfo);
    xGetEnumMeth := xListType.GetMethod('GetEnumerator');

    if not Assigned(xGetEnumMeth) then
        EXIT;

    xAddMeth := xListType.GetMethod('Add');

    for x := 0 to aTypeMap.Count - 1 do
    begin
        xMapItem := aTypeMap[x];
        xObject := nil;
        ASSERT(xMapItem is TMapItem);
        // NOTE: it should be possible to add nil objects

        if (xMapItem is TTypeMap) then
        begin
            GetObjectFromTypeMap(xObject, xMapItem as TTypeMap);

            if Assigned(xObject) then
                ASSERT(xObject is TCustomStreamable);
        end;
        xAddMeth.Invoke(aInstance, [xObject])
    end;
end;

procedure TObjectFromTypeMapTranslator.GetPropertiesFromTypeMap(const aInstance: TObject;
    const aTypeMap: TTypeMap);
var
    xProps: TArray<TRttiProperty>;
    xProp: TRttiProperty;
    xObjType: TRttiType;

begin

    xObjType := fCtx.GetType(aInstance.ClassInfo);

    xProps := xObjType.GetProperties;
    for xProp in xProps do
    begin
        GetPropertyFromTypeMap(aInstance, xProp, aTypeMap);
    end;
    GetObjectFromTypeMapListItemsProp(aInstance, aTypeMap);
end;

procedure TObjectFromTypeMapTranslator.GetObjectFromTypeMap(var vInstance: TObject; const aTypeMap: TTypeMap);
begin
    CreateInstanceFromTypeMap(aTypeMap, vInstance);
    if not Assigned(vInstance) then
        EXIT;

    // if vInstance is TStreamableObjectList then
    // GetObjectFromTypeMapListItemsProp( vInstance as TStreamableObjectList, aTypeMap );

    GetPropertiesFromTypeMap(vInstance, aTypeMap);
end;

function TObjectFromTypeMapTranslator.CreateObject<T>(const aTypeMap: TTypeMap): T;
var
    xObject: TObject;
begin
    xObject := nil;
    GetObjectFromTypeMap(xObject, aTypeMap);
    result := default (T);
    if not Assigned(xObject) then
        EXIT;

    result := T(xObject);
end;

{ TObjectCopy }
class function TObjectCopy<T>.CreateFromTypeMap(const aValue: TTypeMap): T;
var
    xObjectFromTypeMapTranslator: TObjectFromTypeMapTranslator;
begin
    xObjectFromTypeMapTranslator := TObjectFromTypeMapTranslator.Create;
    try
        result := xObjectFromTypeMapTranslator.CreateObject<T>(aValue);
    finally
        FreeAndNil(xObjectFromTypeMapTranslator);
    end;
end;

class function TObjectCopy<T>.CopyObject(const aValue: T): T;
var
    xObjectToTypeMapTranslator: TObjectToTypeMapTranslator;
    xTypeMap: TTypeMap;
begin
    result := default (T);
    if not Assigned(aValue) then
        EXIT;

    xObjectToTypeMapTranslator := TObjectToTypeMapTranslator.Create();
    try
        xTypeMap := xObjectToTypeMapTranslator.CreateTypeMap(aValue);
        try
            if not Assigned(xTypeMap) then
                EXIT;

            result := CreateFromTypeMap(xTypeMap);
        finally
            FreeAndNil(xTypeMap);
        end;
    finally
        FreeAndNil(xObjectToTypeMapTranslator);
    end;
end;

class function TObjectCopy<T>.Copy(const aValue: T): T;
begin
    result := CopyObject(aValue);
end;


end.
