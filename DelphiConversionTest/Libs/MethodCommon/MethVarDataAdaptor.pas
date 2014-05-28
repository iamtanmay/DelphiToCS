unit MethVarDataAdaptor;
{ --------------------------------------------------------------------------------------------------
  Copyright © 2005 Zinsser Analytic GmbH - All rights reserved.
  Author       : Payman Kamali (pk)
  Description  : Data Adaptor for GLOBAL variables which are stored in settings.db
  --------------------------------------------------------------------------------------------------
  Revision History:
  date     op  method                       track-no improvement/change
  -------- --  ---------------------------  -------- -----------------------------------------------
  25.08.05 pk                                TN2547  Initial Revision - Code extracted from TParserIdentifier
  08.03.07 wl  WriteValue,DeleteValue        TN3475   jeder neue Wert wird in Log geschrieben
  06.08.08 pk  ReadIntoCache, WriteIntoCache TN4165.1 New
  04.11.09 pk                                TN4843   Replace TObjectList, TStringList, TStrings with Generics Lists and TStringArray
  -------------------------------------------------------------------------------------------------- }


interface


uses
    GeneralTypes,
    CommonTypes;

type
    TMethVarDataAdaptor = class
    private
        fIniAccess: IWinLissyIniAccess;
    public
        constructor Create();
        destructor Destroy(); override;
        procedure ReadIntoCache();
        procedure WriteFromCache();
        function ReadValue(aIdentName: string): string;
        procedure DeleteValue(aIdentName: string);
        procedure WriteValue(aIdentName, aValue: string);
        procedure ReadMinMaxValues(aIdentName: string; out oMinValue, oMaxValue: string);
        procedure ReadOtherValues(aIdentName: string; out oValueFormat: integer;
            out oDescription, oDefValue, oPickList: string);
        function ReadDescription(aIdentName: string): string;
        function GetAllExistingVariables(): TStringArray;
        procedure DeleteIdentifier(aIdentName: string);
        procedure SaveProperties(const aIdentName, aDescription, aMinValue, aMaxValue, aDefValue,
            aPickList: string; aFormat: integer);
        class function InstGetAllExistingVariables(): TStringArray;
    end;


    // ##################################################################################################


implementation


uses
    Forms,
    SysUtils,
    LogManager,
    AppSettings,
    ListClasses;

const
    STR_IDENT_DEFVALUE_SUFFIX = 'DefValue';
    STR_IDENT_MINVALUE_SUFFIX = 'MinValue';
    STR_IDENT_MAXVALUE_SUFFIX = 'MaxValue';
    STR_IDENT_PICKLIST_SUFFIX = 'PickList';
    STR_IDENT_FORMAT_SUFFIX = 'ValFormat';
    STR_IDENT_VALUE_SUFFIX = 'Value';

constructor TMethVarDataAdaptor.Create;
begin
    inherited Create();
    fIniAccess := gCommonDll.CreateAppIni;
end;

destructor TMethVarDataAdaptor.Destroy;
begin
    fIniAccess := nil;
    inherited;
end;

function TMethVarDataAdaptor.ReadValue(aIdentName: string): string;
begin
    result := fIniAccess.ReadString(STR_ISEC_METHODPARAMS, aIdentName + STR_IDENT_VALUE_SUFFIX);
end;

procedure TMethVarDataAdaptor.DeleteValue(aIdentName: string);
begin
    if not fIniAccess.ValueExists(STR_ISEC_METHODPARAMS, aIdentName + STR_IDENT_VALUE_SUFFIX) then
        EXIT;

    fIniAccess.DeleteKey(STR_ISEC_METHODPARAMS, aIdentName + STR_IDENT_VALUE_SUFFIX, true);

    gLogManager.LogF('DELETE global %s Variable: %s (%s)', [STR_ISEC_METHODPARAMS, aIdentName,
        Application.Title], false);
end;

procedure TMethVarDataAdaptor.WriteValue(aIdentName, aValue: string);
begin
    fIniAccess.WriteString(STR_ISEC_METHODPARAMS, aIdentName + STR_IDENT_VALUE_SUFFIX, aValue);

    gLogManager.LogF('STORE global %s Variable: %s = %s (%s)', [STR_ISEC_METHODPARAMS, aIdentName, aValue,
        Application.Title], false);
end;

procedure TMethVarDataAdaptor.ReadMinMaxValues(aIdentName: string; out oMinValue, oMaxValue: string);
begin
    oMinValue := fIniAccess.ReadString(STR_ISEC_METHODPARAMS, aIdentName + STR_IDENT_MINVALUE_SUFFIX);
    oMaxValue := fIniAccess.ReadString(STR_ISEC_METHODPARAMS, aIdentName + STR_IDENT_MAXVALUE_SUFFIX);
end;

procedure TMethVarDataAdaptor.ReadOtherValues(aIdentName: string; out oValueFormat: integer;
    out oDescription, oDefValue, oPickList: string);
var
    xValueFormat: string;
    xErr: integer;
begin
    oDescription := fIniAccess.ReadString(STR_ISEC_METHODPARAMS, aIdentName); // Description
    oDefValue := fIniAccess.ReadString(STR_ISEC_METHODPARAMS, aIdentName + STR_IDENT_DEFVALUE_SUFFIX);
    oPickList := fIniAccess.ReadString(STR_ISEC_METHODPARAMS, aIdentName + STR_IDENT_PICKLIST_SUFFIX);
    xValueFormat := fIniAccess.ReadString(STR_ISEC_METHODPARAMS, aIdentName + STR_IDENT_FORMAT_SUFFIX);
    Val(xValueFormat, oValueFormat, xErr);
end;

function TMethVarDataAdaptor.ReadDescription(aIdentName: string): string;
begin
    result := fIniAccess.ReadString(STR_ISEC_METHODPARAMS, aIdentName); // Description
end;

function TMethVarDataAdaptor.GetAllExistingVariables(): TStringArray;
var
    xIdentNames: TStringArray;
    xResultIdentNames: TStringValueList;
    x: integer;
begin
    xIdentNames := fIniAccess.ReadAllowedSection(STR_ISEC_METHODPARAMS, '');

    xResultIdentNames := TStringValueList.Create();
    try
        for x := 0 to high(xIdentNames) do
        begin
            if Pos(STR_IDENT_MINVALUE_SUFFIX, xIdentNames[x]) > 0 then
                CONTINUE;
            if Pos(STR_IDENT_MAXVALUE_SUFFIX, xIdentNames[x]) > 0 then
                CONTINUE;
            if Pos(STR_IDENT_VALUE_SUFFIX, xIdentNames[x]) > 0 then
                CONTINUE;
            if Pos(STR_IDENT_DEFVALUE_SUFFIX, xIdentNames[x]) > 0 then
                CONTINUE;
            if Pos(STR_IDENT_PICKLIST_SUFFIX, xIdentNames[x]) > 0 then
                CONTINUE;
            if Pos(STR_IDENT_FORMAT_SUFFIX, xIdentNames[x]) > 0 then
                CONTINUE;

            xResultIdentNames.Add(xIdentNames[x]);
        end;

        result := xResultIdentNames.ToArray;

    finally
        FreeAndNil(xResultIdentNames);
    end;

    // TBD_WL: Ein Identifier eigentlich auch, wenn z.B. nur ein DEFVALUE gespeichert ist
end;

procedure TMethVarDataAdaptor.DeleteIdentifier(aIdentName: string);
begin
    fIniAccess.DeleteKey(STR_ISEC_METHODPARAMS, aIdentName, true);
    fIniAccess.DeleteKey(STR_ISEC_METHODPARAMS, aIdentName + STR_IDENT_MINVALUE_SUFFIX, true);
    fIniAccess.DeleteKey(STR_ISEC_METHODPARAMS, aIdentName + STR_IDENT_MAXVALUE_SUFFIX, true);
    fIniAccess.DeleteKey(STR_ISEC_METHODPARAMS, aIdentName + STR_IDENT_DEFVALUE_SUFFIX, true);
    fIniAccess.DeleteKey(STR_ISEC_METHODPARAMS, aIdentName + STR_IDENT_PICKLIST_SUFFIX, true);
    fIniAccess.DeleteKey(STR_ISEC_METHODPARAMS, aIdentName + STR_IDENT_FORMAT_SUFFIX, true);
end;

procedure TMethVarDataAdaptor.SaveProperties(const aIdentName, aDescription, aMinValue, aMaxValue, aDefValue,
    aPickList: string; aFormat: integer);
var
    xFormat: string;
begin
    fIniAccess.WriteString(STR_ISEC_METHODPARAMS, aIdentName, aDescription); // Description
    fIniAccess.WriteString(STR_ISEC_METHODPARAMS, aIdentName + STR_IDENT_MINVALUE_SUFFIX, aMinValue);
    fIniAccess.WriteString(STR_ISEC_METHODPARAMS, aIdentName + STR_IDENT_MAXVALUE_SUFFIX, aMaxValue);
    fIniAccess.WriteString(STR_ISEC_METHODPARAMS, aIdentName + STR_IDENT_DEFVALUE_SUFFIX, aDefValue);
    fIniAccess.WriteString(STR_ISEC_METHODPARAMS, aIdentName + STR_IDENT_PICKLIST_SUFFIX, aPickList);
    xFormat := IntToStr(aFormat);
    fIniAccess.WriteString(STR_ISEC_METHODPARAMS, aIdentName + STR_IDENT_FORMAT_SUFFIX, xFormat);
end;

procedure TMethVarDataAdaptor.ReadIntoCache;
begin
    fIniAccess.ReadSectionIntoCache(STR_ISEC_METHODPARAMS);
end;

procedure TMethVarDataAdaptor.WriteFromCache;
begin
    fIniAccess.WriteSectionFromCache(STR_ISEC_METHODPARAMS);
end;

class function TMethVarDataAdaptor.InstGetAllExistingVariables(): TStringArray;
var
    xDA: TMethVarDataAdaptor;
begin
    xDA := TMethVarDataAdaptor.Create();
    try
        xDA.ReadIntoCache();
        result := xDA.GetAllExistingVariables();

    finally
        xDA.Free;
    end;
end;


end.
