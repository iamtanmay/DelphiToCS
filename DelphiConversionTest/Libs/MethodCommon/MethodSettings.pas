{ --------------------------------------------------------------------------------------------------
  Copyright © 2005 Zinsser Analytic GmbH - All rights reserved.
  Author       : Payman Kamali (pk)
  Description  : Method settings
  --------------------------------------------------------------------------------------------------
  Revision History:
  date     op  method                            track-no  improvement/change
  -------- --  ---------------------------       --------  -----------------------------------------
  20.06.05 wl  LoadComments,SaveComments         TN2441    Lädt Methoden-bezogene Kommentare
  20.06.05 wl  Load/SaveTableWidthAndOrder       TN2440    Lädt Anordnung und Breite der Spalten
  16.08.05 wl  DeleteMethodSettings              TN2558.4  Löscht alle Einträge für eine Methode
  06.04.06 pk  Load/SaveInitOption               TN3031    New : loads option for deciding standard init
  19.05.06 wl  Set/GetAttributes                 TN3109    Liest und schreibt in 'Attributes'
  16.01.07 pk  Load/SaveAutoSeq                  TN3481    Load and Save AutoSeq methodsetting
  08.01.08 wl  Load/SaveAutoSeq                  TN3972    entfernt
  08.01.08 wl  Load/SaveLayoutName               TN3972    neu: Layout zur Methode wird hier gespeichert
  06.08.08 pk  TMethodSettingsDataAdaptor        TN4165.1  New: replaces MethodIniAccess
  29.09.08 wl  TMethodSettingsTableStructDefV2   TN4242   Feldnamen geändert
  16.01.09 wl                                    TN4362   an Änderungen in TQueryDataAdaptor angepasst
  13.03.09 ts  TMethodSettingsDataAdaptor        TN4464   new "startable"-option functions and procedures added
  16.03.09 ts                                    TN4471   Upper entfernt, damit Methodname in METHODSETTINGS-Tabelle wie in METHOD-Tab angezeigt wird
  06.04.09 pk  Load/SaveDisplayComponents        TN4503   New
  16.06.09 wl  TMethodSettingsDataAdaptor        TN4605   --> MethodSettingsDataAdaptor
  16.06.09 wl  CleanUpMethodSettings             TN4605   Settings löschen, zu denen keine Methode existiert
  29.10.10 pk  Load/SaveRestartEventName         TN5320   New
  02.02.11 wl  cChooseLayoutAtRuntime            TN5466   neu
  27.12.11 wl  Load/SaveDeleteRunDataOption      TN5768   neu: wenn false, werden vor dem Laden des Layouts keine Daten gelöscht!
  -------------------------------------------------------------------------------------------------- }

unit MethodSettings;


interface


type
    TMethodSetAttribute = (msaReadOnly, msaHidden);
    TMethodSetAttributes = set of TMethodSetAttribute;

    TMethodInitOption = (mioInit, mioNoInit);

    TMethodStartableOption = (msoStartable, msoNotStartable);

    TMethodSettings = class
    private const
        STR_IDENT_METHOD_INIT = 'Init';
        STR_IDENT_METHOD_STARTABLE = 'Startable';
        STR_IDENT_METHOD_DISPLAYCOMPONENT = 'DisplayComponent';
        STR_IDENT_METHOD_RESTARTEVENT = 'RestartEvent';
        STR_IDENT_METHOD_DeleteRunData = 'DeleteRunData';

        STR_INITOPTIONS_NOINIT = 0;
        STR_INITOPTIONS_INIT = 1;

        STR_STARTABLEOPTIONS_STARTABLE = 1;
        STR_STARTABLEOPTIONS_NOTSTARTABLE = 0;
    private
        class function GetAttibutesFromIntValue(const aIntValue: integer): TMethodSetAttributes;
        class function GetIntValueFromAttibutes(const aAttibutes: TMethodSetAttributes): integer;
    public const
        cChooseLayoutAtRuntime = '*';
    public
        class procedure LoadComments(const aMethodName: string; out oCommentShort, oCommentLong: string);
        class procedure SaveComments(const aMethodName, aCommentShort, aCommentLong: string);
        class procedure LoadTableWidthAndOrder(const aMethodName: string; out oOrderText, oWidthText: string);
        class procedure SaveTableWidthAndOrder(const aMethodName, aOrderText, aWidthText: string);
        class procedure SaveInitOption(const aMethodName: string; aValue: TMethodInitOption);
        class function LoadInitOption(const aMethodName: string): TMethodInitOption;
        class procedure SaveDeleteRunDataOption(const aMethodName: string; aValue: boolean);
        class function LoadDeleteRunDataOption(const aMethodName: string): boolean;
        class procedure DeleteMethodSettings(const aMethodName, aReason: string);
        class function GetAttributes(const aMethodName: string): TMethodSetAttributes;
        class procedure SetAttributes(const aMethodName: string; aAttributes: TMethodSetAttributes);
        class function LoadLayoutName(const aMethodName: string): string;
        class procedure SaveLayoutName(const aMethodName, aLayoutName: string);
        class function LoadDisplayComponentName(const aMethodName: string): string;
        class procedure SaveDisplayComponentName(const aMethodName, aValue: string);
        class function LoadRestartEventName(const aMethodName: string): string;
        class procedure SaveRestartEventName(const aMethodName, aValue: string);
        // new startable method settings
        class procedure SaveStartableOption(const aMethodName: string; aValue: TMethodStartableOption);
        class function LoadStartableOption(const aMethodName: string): TMethodStartableOption;

        class procedure CleanUpMethodSettings();
    end;


implementation


uses
    AppTypes,
    MethodSettingsDataAdaptor,
    MethodDataAdaptor,
    SysUtils;

class procedure TMethodSettings.LoadComments(const aMethodName: string;
    out oCommentShort, oCommentLong: string);
var
    xDA: TMethodSettingsDataAdaptor;
begin
    if (aMethodName = '') then
        EXIT;

    xDA := TMethodSettingsDataAdaptor.Create();
    try
        oCommentShort := xDA.ReadString(aMethodName, 'Comment', 'Short', '');
        oCommentLong := xDA.ReadString(aMethodName, 'Comment', 'Long', '');
    finally
        xDA.Free;
    end;
end;

class procedure TMethodSettings.SaveComments(const aMethodName, aCommentShort, aCommentLong: string);
var
    xDA: TMethodSettingsDataAdaptor;
begin
    if (aMethodName = '') then
        EXIT;

    xDA := TMethodSettingsDataAdaptor.Create();
    try
        xDA.WriteString(aMethodName, 'Comment', 'Short', aCommentShort);
        xDA.WriteString(aMethodName, 'Comment', 'Long', aCommentLong);
    finally
        xDA.Free;
    end;
end;

class procedure TMethodSettings.LoadTableWidthAndOrder(const aMethodName: string;
    out oOrderText, oWidthText: string);
var
    xDA: TMethodSettingsDataAdaptor;
begin
    // Text lesen aus appdata.tmp
    xDA := TMethodSettingsDataAdaptor.Create();
    try
        oOrderText := xDA.ReadString(aMethodName, STR_ISEC_METHOD_EDITGRID, STR_IDENT_METHOD_COLORDER, '');
        oWidthText := xDA.ReadString(aMethodName, STR_ISEC_METHOD_EDITGRID, STR_IDENT_METHOD_COLWIDTHS, '');
    finally
        xDA.Free;
    end;
end;

class procedure TMethodSettings.SaveTableWidthAndOrder(const aMethodName, aOrderText, aWidthText: string);
var
    xDA: TMethodSettingsDataAdaptor;
begin
    // Text lesen aus appdata.tmp
    xDA := TMethodSettingsDataAdaptor.Create();
    try
        xDA.WriteString(aMethodName, STR_ISEC_METHOD_EDITGRID, STR_IDENT_METHOD_COLORDER, aOrderText);
        xDA.WriteString(aMethodName, STR_ISEC_METHOD_EDITGRID, STR_IDENT_METHOD_COLWIDTHS, aWidthText);
    finally
        xDA.Free;
    end;
end;

class procedure TMethodSettings.DeleteMethodSettings(const aMethodName, aReason: string);
var
    xDA: TMethodSettingsDataAdaptor;
begin
    // Text lesen aus appdata.tmp
    xDA := TMethodSettingsDataAdaptor.Create();
    try
        xDA.DeleteName(aMethodName);
    finally
        xDA.Free;
    end;
end;

class function TMethodSettings.LoadInitOption(const aMethodName: string): TMethodInitOption;
var
    xDA: TMethodSettingsDataAdaptor;
    xValue: integer;
begin
    xDA := TMethodSettingsDataAdaptor.Create();
    try
        xValue := xDA.ReadInteger(aMethodName, STR_ISEC_METHOD_INITOPTIONS, STR_IDENT_METHOD_INIT,
            STR_INITOPTIONS_INIT);

        result := mioInit;
        if xValue = STR_INITOPTIONS_NOINIT then
            result := mioNoInit;
    finally
        xDA.Free;
    end;
end;

class procedure TMethodSettings.SaveInitOption(const aMethodName: string; aValue: TMethodInitOption);
var
    xDA: TMethodSettingsDataAdaptor;
    xValue: integer;
begin
    xDA := TMethodSettingsDataAdaptor.Create();
    try
        xValue := STR_INITOPTIONS_INIT;
        if aValue = mioNoInit then
            xValue := STR_INITOPTIONS_NOINIT;
        xDA.WriteInteger(aMethodName, STR_ISEC_METHOD_INITOPTIONS, STR_IDENT_METHOD_INIT, xValue);
    finally
        xDA.Free;
    end;
end;

class procedure TMethodSettings.SaveStartableOption(const aMethodName: string;
    aValue: TMethodStartableOption);
var
    xDA: TMethodSettingsDataAdaptor;
    xValue: integer;
begin
    xDA := TMethodSettingsDataAdaptor.Create();
    try
        xValue := STR_STARTABLEOPTIONS_STARTABLE;
        if aValue = msoNotStartable then
            xValue := STR_STARTABLEOPTIONS_NOTSTARTABLE;
        xDA.WriteInteger(aMethodName, STR_ISEC_METHOD_STARTABLEOPTIONS, STR_IDENT_METHOD_STARTABLE, xValue);
    finally
        xDA.Free;
    end;
end;

class function TMethodSettings.LoadStartableOption(const aMethodName: string): TMethodStartableOption;
var
    xDA: TMethodSettingsDataAdaptor;
    xValue: integer;
begin
    xDA := TMethodSettingsDataAdaptor.Create();
    try
        xValue := xDA.ReadInteger(aMethodName, STR_ISEC_METHOD_STARTABLEOPTIONS, STR_IDENT_METHOD_STARTABLE,
            STR_STARTABLEOPTIONS_NOTSTARTABLE);

        result := msoNotStartable;
        if xValue = STR_STARTABLEOPTIONS_STARTABLE then
            result := msoStartable;
    finally
        xDA.Free;
    end;
end;

class function TMethodSettings.GetAttributes(const aMethodName: string): TMethodSetAttributes;
var
    xDA: TMethodSettingsDataAdaptor;
    xIntValue: integer;
begin
    xDA := TMethodSettingsDataAdaptor.Create();
    try
        xIntValue := xDA.ReadInteger(aMethodName, STR_ISEC_METHOD_ATTRIBUTES, STR_IDENT_METHOD_ATTRIBUTES, 0);
        result := GetAttibutesFromIntValue(xIntValue);
    finally
        xDA.Free;
    end;
end;

class function TMethodSettings.GetAttibutesFromIntValue(const aIntValue: integer): TMethodSetAttributes;
var
    i: integer;
begin
    result := [];
    for i := 0 to Integer( high(TMethodSetAttribute)) do
    begin
        if (aIntValue and ARR_POWER_OF_TWO[i + 1]) = ARR_POWER_OF_TWO[i + 1] then
            result := result + [TMethodSetAttribute(i)];
    end;
end;

class function TMethodSettings.GetIntValueFromAttibutes(const aAttibutes: TMethodSetAttributes): integer;
var
    i: integer;
begin
    result := 0;
    for i := 0 to Integer( high(TMethodSetAttribute)) do
    begin
        if TMethodSetAttribute(i) in aAttibutes then
            result := result + ARR_POWER_OF_TWO[i + 1];
    end;
end;

class procedure TMethodSettings.SetAttributes(const aMethodName: string; aAttributes: TMethodSetAttributes);
var
    xDA: TMethodSettingsDataAdaptor;
    xIntValue: integer;
begin
    xIntValue := self.GetIntValueFromAttibutes(aAttributes);

    xDA := TMethodSettingsDataAdaptor.Create();
    try
        xDA.WriteInteger(aMethodName, STR_ISEC_METHOD_ATTRIBUTES, STR_IDENT_METHOD_ATTRIBUTES, xIntValue);
    finally
        xDA.Free;
    end;
end;

class function TMethodSettings.LoadLayoutName(const aMethodName: string): string;
var
    xDA: TMethodSettingsDataAdaptor;
begin
    xDA := TMethodSettingsDataAdaptor.Create();
    try
        result := xDA.ReadString(aMethodName, STR_ISEQ_METHOD_LAYOUT, STR_IDENT_METHOD_LAYOUT, '');
    finally
        xDA.Free;
    end;
end;

class procedure TMethodSettings.SaveLayoutName(const aMethodName, aLayoutName: string);
var
    xDA: TMethodSettingsDataAdaptor;
begin
    xDA := TMethodSettingsDataAdaptor.Create();
    try
        xDA.WriteString(aMethodName, STR_ISEQ_METHOD_LAYOUT, STR_IDENT_METHOD_LAYOUT, aLayoutName);
    finally
        xDA.Free;
    end;
end;

class function TMethodSettings.LoadDeleteRunDataOption(const aMethodName: string): boolean;
var
    xDA: TMethodSettingsDataAdaptor;
begin
    xDA := TMethodSettingsDataAdaptor.Create();
    try
        result := (xDA.ReadInteger(aMethodName, STR_ISEC_METHOD_STARTABLEOPTIONS,
            STR_IDENT_METHOD_DeleteRunData, 1) = 1); // Default: true!
    finally
        xDA.Free;
    end;
end;

class procedure TMethodSettings.SaveDeleteRunDataOption(const aMethodName: string; aValue: boolean);
var
    xDA: TMethodSettingsDataAdaptor;
    xIntVal: integer;
begin
    xDA := TMethodSettingsDataAdaptor.Create();
    try
        if aValue then
            xIntVal := 1
        else
            xIntVal := 0;
        xDA.WriteInteger(aMethodName, STR_ISEC_METHOD_STARTABLEOPTIONS,
            STR_IDENT_METHOD_DeleteRunData, xIntVal);
    finally
        xDA.Free;
    end;
end;

class function TMethodSettings.LoadDisplayComponentName(const aMethodName: string): string;
var
    xDA: TMethodSettingsDataAdaptor;
begin
    xDA := TMethodSettingsDataAdaptor.Create();
    try
        result := xDA.ReadString(aMethodName, STR_ISEQ_METHOD_DISPLAYOPTIONS,
            STR_IDENT_METHOD_DISPLAYCOMPONENT, '');
    finally
        xDA.Free;
    end;
end;

class procedure TMethodSettings.SaveDisplayComponentName(const aMethodName, aValue: string);
var
    xDA: TMethodSettingsDataAdaptor;
begin
    xDA := TMethodSettingsDataAdaptor.Create();
    try
        xDA.WriteString(aMethodName, STR_ISEQ_METHOD_DISPLAYOPTIONS,
            STR_IDENT_METHOD_DISPLAYCOMPONENT, aValue);
    finally
        xDA.Free;
    end;
end;

class function TMethodSettings.LoadRestartEventName(const aMethodName: string): string;
var
    xDA: TMethodSettingsDataAdaptor;
begin
    xDA := TMethodSettingsDataAdaptor.Create();
    try
        result := xDA.ReadString(aMethodName, STR_ISEQ_METHOD_DISPLAYOPTIONS,
            STR_IDENT_METHOD_RESTARTEVENT, '');
    finally
        xDA.Free;
    end;
end;

class procedure TMethodSettings.SaveRestartEventName(const aMethodName, aValue: string);
var
    xDA: TMethodSettingsDataAdaptor;
begin
    xDA := TMethodSettingsDataAdaptor.Create();
    try
        xDA.WriteString(aMethodName, STR_ISEQ_METHOD_DISPLAYOPTIONS, STR_IDENT_METHOD_RESTARTEVENT, aValue);
    finally
        xDA.Free;
    end;
end;

class procedure TMethodSettings.CleanUpMethodSettings;
var
    xMethodDA: TMethodDataAdaptor;
    xMethodNames: TArray<string>;
    xMethodSettingsDA: TMethodSettingsDataAdaptor;
begin
    xMethodDA := TMethodDataAdaptor.Create;
    try
        xMethodNames := xMethodDA.ReadAllNames();
    finally
        xMethodDA.Free;
    end;

    xMethodSettingsDA := TMethodSettingsDataAdaptor.Create;
    try
        xMethodSettingsDA.DeleteUnusedSettings(xMethodNames);
    finally
        xMethodSettingsDA.Free;
    end;
end;


end.
