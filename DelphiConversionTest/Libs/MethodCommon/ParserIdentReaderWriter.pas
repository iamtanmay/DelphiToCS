unit ParserIdentReaderWriter;

{ --------------------------------------------------------------------------------------------------
  Copyright © 2005 Zinsser Analytic GmbH - All rights reserved.
  Author       : Payman Kamali (pk)
  Description  : Wrapper for data adaptors which access Stored variables in Settings.db or RunVar.db
  --------------------------------------------------------------------------------------------------
  Revision History:
  date     op  method                            track-no improvement/change
  -------- --  -------------------------------  -------- -------------------------------------------
  25.08.05 pk                                    TN2547  Initial Revision
  05.05.06 pk  TParserIdentReaderWriterRunVar    Tn3084  Create now requires Priority
  16.05.06 wl  TParserIdentReaderWriterRunVar.ReadValueAndTime  TN3101   neu, nicht in Basisklasse enthalten
  06.08.08 pk                                    TN4165.1 New: ReadIntoCache, WriteFromCache
  04.11.09 pk                                	TN4843   Replace TObjectList, TStringList, TStrings with Generics Lists and TStringArray
  -------------------------------------------------------------------------------------------------- }


interface


uses
    GeneralTypes,
    MethVarDataAdaptor,
    RunVarDataAdaptor;

type
    TParserIdentReaderWriter = class
    public
        procedure ReadIntoCache; virtual;
        procedure WriteFromCache; virtual;
        function ReadValue(const aIdentName: string): string; virtual; abstract;
        procedure DeleteValue(const aIdentName: string); virtual; abstract;
        procedure WriteValue(const aIdentName, aValue: string); virtual; abstract;
        procedure ReadMinMaxValues(const aIdentName: string; out oMinValue, oMaxValue: string);
            virtual; abstract;
        procedure ReadOtherValues(const aIdentName: string; out oValueFormat: integer;
            out oDescription, oDefValue, oPickList: string); virtual; abstract;
        function ReadDescription(const aIdentName: string): string; virtual; abstract;
        function GetAllExistingVariables(): TStringArray; virtual; abstract;
        procedure DeleteIdentifier(const aIdentName: string); virtual; abstract;
        procedure SaveProperties(const aIdentName, aDescription, aMinValue, aMaxValue, aDefValue,
            aPickList: string; aFormat: integer); virtual; abstract;
    end;

    TParserIdentReaderWriterRunVar = class(TParserIdentReaderWriter)
    protected
        fRunVarDataAdaptor: TRunVarDataAdaptor;
    public
        constructor Create(const aRunName: string; aPriority: integer);
        destructor Destroy(); override;
        function ReadValue(const aIdentName: string): string; override;
        procedure DeleteValue(const aIdentName: string); override;
        procedure WriteValue(const aIdentName, aValue: string); override;
        procedure ReadMinMaxValues(const aIdentName: string; out oMinValue, oMaxValue: string); override;
        procedure ReadOtherValues(const aIdentName: string; out oValueFormat: integer;
            out oDescription, oDefValue, oPickList: string); override;
        function ReadDescription(const aIdentName: string): string; override;
        function GetAllExistingVariables(): TStringArray; override;
        procedure DeleteIdentifier(const aIdentName: string); override;
        procedure SaveProperties(const aIdentName, aDescription, aMinValue, aMaxValue, aDefValue,
            aPickList: string; aFormat: integer); override;
        // not virtual:
        function ReadValueAndTime(const aIdentName: string; out oChangeTime: TDateTime): string;
    end;

    TParserIdentReaderWriterSettings = class(TParserIdentReaderWriter)
    private
        fDataAdaptor: TMethVarDataAdaptor;
    public
        constructor Create;
        destructor Destroy; override;
        procedure ReadIntoCache; override;
        procedure WriteFromCache; override;
        function ReadValue(const aIdentName: string): string; override;
        procedure DeleteValue(const aIdentName: string); override;
        procedure WriteValue(const aIdentName, aValue: string); override;
        procedure ReadMinMaxValues(const aIdentName: string; out oMinValue, oMaxValue: string); override;
        procedure ReadOtherValues(const aIdentName: string; out oValueFormat: integer;
            out oDescription, oDefValue, oPickList: string); override;
        function ReadDescription(const aIdentName: string): string; override;
        function GetAllExistingVariables(): TStringArray; override;
        procedure DeleteIdentifier(const aIdentName: string); override;
        procedure SaveProperties(const aIdentName, aDescription, aMinValue, aMaxValue, aDefValue,
            aPickList: string; aFormat: integer); override;

    end;


implementation


{ TParserIdentReaderWriter }

procedure TParserIdentReaderWriter.ReadIntoCache;
begin

end;

procedure TParserIdentReaderWriter.WriteFromCache;
begin

end;

{ TParserIdentReaderWriterSettings }
constructor TParserIdentReaderWriterSettings.Create();
begin
    inherited;
    fDataAdaptor := TMethVarDataAdaptor.Create();
end;

destructor TParserIdentReaderWriterSettings.Destroy();
begin
    fDataAdaptor.Free;
    inherited;
end;

procedure TParserIdentReaderWriterSettings.ReadIntoCache;
begin
    fDataAdaptor.ReadIntoCache();
end;

procedure TParserIdentReaderWriterSettings.WriteFromCache;
begin
    fDataAdaptor.WriteFromCache();
end;

function TParserIdentReaderWriterSettings.ReadValue(const aIdentName: string): string;
begin
    result := fDataAdaptor.ReadValue(aIdentName);
end;

procedure TParserIdentReaderWriterSettings.DeleteValue(const aIdentName: string);
begin
    fDataAdaptor.DeleteValue(aIdentName);
end;

procedure TParserIdentReaderWriterSettings.WriteValue(const aIdentName, aValue: string);
begin
    fDataAdaptor.WriteValue(aIdentName, aValue);
end;

procedure TParserIdentReaderWriterSettings.ReadMinMaxValues(const aIdentName: string;
    out oMinValue, oMaxValue: string);
begin
    fDataAdaptor.ReadMinMaxValues(aIdentName, oMinValue, oMaxValue);
end;

procedure TParserIdentReaderWriterSettings.ReadOtherValues(const aIdentName: string;
    out oValueFormat: integer; out oDescription, oDefValue, oPickList: string);
begin
    fDataAdaptor.ReadOtherValues(aIdentName, oValueFormat, oDescription, oDefValue, oPickList);
end;

function TParserIdentReaderWriterSettings.ReadDescription(const aIdentName: string): string;
begin
    result := fDataAdaptor.ReadDescription(aIdentName);
end;

function TParserIdentReaderWriterSettings.GetAllExistingVariables(): TStringArray;
begin
    result := fDataAdaptor.GetAllExistingVariables();
end;

procedure TParserIdentReaderWriterSettings.DeleteIdentifier(const aIdentName: string);
begin
    fDataAdaptor.DeleteIdentifier(aIdentName);
end;

procedure TParserIdentReaderWriterSettings.SaveProperties(const aIdentName, aDescription, aMinValue,
    aMaxValue, aDefValue, aPickList: string; aFormat: integer);
begin
    fDataAdaptor.SaveProperties(aIdentName, aDescription, aMinValue, aMaxValue, aDefValue, aPickList,
        aFormat);
end;

constructor TParserIdentReaderWriterRunVar.Create(const aRunName: string; aPriority: integer);
begin
    inherited Create;
    fRunVarDataAdaptor := TRunVarDataAdaptor.Create(aRunName, aPriority)
end;

destructor TParserIdentReaderWriterRunVar.Destroy();
begin
    fRunVarDataAdaptor.Free;
end;

function TParserIdentReaderWriterRunVar.ReadValue(const aIdentName: string): string;
begin
    result := fRunVarDataAdaptor.ReadValue(aIdentName);
end;

procedure TParserIdentReaderWriterRunVar.DeleteValue(const aIdentName: string);
begin
    fRunVarDataAdaptor.DeleteValue(aIdentName);
end;

procedure TParserIdentReaderWriterRunVar.WriteValue(const aIdentName, aValue: string);
begin
    fRunVarDataAdaptor.WriteValue(aIdentName, aValue);
end;

procedure TParserIdentReaderWriterRunVar.ReadMinMaxValues(const aIdentName: string;
    out oMinValue, oMaxValue: string);
begin
    fRunVarDataAdaptor.ReadMinMaxValues(aIdentName, oMinValue, oMaxValue);
end;

procedure TParserIdentReaderWriterRunVar.ReadOtherValues(const aIdentName: string; out oValueFormat: integer;
    out oDescription, oDefValue, oPickList: string);
begin
    fRunVarDataAdaptor.ReadOtherValues(aIdentName, oValueFormat, oDescription, oDefValue, oPickList);
end;

function TParserIdentReaderWriterRunVar.ReadDescription(const aIdentName: string): string;
begin
    result := fRunVarDataAdaptor.ReadDescription(aIdentName);
end;

function TParserIdentReaderWriterRunVar.GetAllExistingVariables(): TStringArray;
begin
    result := fRunVarDataAdaptor.ReadAllIdents();
end;

procedure TParserIdentReaderWriterRunVar.DeleteIdentifier(const aIdentName: string);
begin
    fRunVarDataAdaptor.DeleteIdentifier(aIdentName);
end;

procedure TParserIdentReaderWriterRunVar.SaveProperties(const aIdentName, aDescription, aMinValue, aMaxValue,
    aDefValue, aPickList: string; aFormat: integer);
begin
    fRunVarDataAdaptor.WriteProperties(aIdentName, aDescription, aMinValue, aMaxValue, aDefValue,
        aPickList, aFormat);
end;

function TParserIdentReaderWriterRunVar.ReadValueAndTime(const aIdentName: string;
    out oChangeTime: TDateTime): string;
begin
    result := fRunVarDataAdaptor.ReadValueAndTime(aIdentName, oChangeTime);
end;


end.
