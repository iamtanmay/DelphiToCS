unit MethodStepDataFields;
{ --------------------------------------------------------------------------------------------------
  Copyright © 2008 Zinsser Analytic GmbH - All rights reserved.
  Author       : Wolfgang Lyncke (wl)
  Description  :
  --------------------------------------------------------------------------------------------------
  Revision History:
  date     op  method                       track-no  improvement/change
  -------- --  ---------------------------  --------  ----------------------------------------------
  08.01.08 wl                               TN3972    neu: enthält Typen aus der unit MethodStep
  06.05.08 wl  Inactive                     TN4074    new field
  19.08.09 wl                               TN4702   Strings werden jetzt direkt geladen
  04.11.09 pk                               TN4843    Replace TObjectList, TStringList, TStrings with Generics Lists and TStringArray
  07.05.10 wl                               TN5087    erheblich aufgeräumt
  29.09.10 pk                               TN5283    Short and Long method step comments combined
  -------------------------------------------------------------------------------------------------- }


interface


uses
    ListClasses,
    CustomSetting,
    AppTypes,
    MethodDataAdaptor,
    MethodTypes;

type
    TMethodStepReadDataEvent = procedure(aSender: TObject; aDataID: integer; var vValue: string) of object;
    TMethodStepWriteDataEvent = procedure(aSender: TObject; aDataID: integer; const aValue: string) of object;

    TMethodStepDataLink = class
    private
        fOption, fComment, fInactive: integer;
        fOnReadData: TMethodStepReadDataEvent;
        fOnWriteData: TMethodStepWriteDataEvent;
    public
        constructor Create();
        procedure InitWithDefaults();
        //
        property Option: integer read fOption write fOption;
        property Comment: integer read fComment write fComment;
        property Inactive: integer read fInactive write fInactive;
        property OnReadData: TMethodStepReadDataEvent read fOnReadData write fOnReadData;
        property OnWriteData: TMethodStepWriteDataEvent read fOnWriteData write fOnWriteData;
    end;

    TMethodStepFieldReadDataEvent = procedure(aSender: TObject; var vValue: string) of object;
    TMethodStepFieldWriteDataEvent = procedure(aSender: TObject; const aValue: string) of object;

    TMethodStepDataField = class
    private
        fDataID: integer;
        fParam: TCustomSetting;
        fOnReadData: TMethodStepFieldReadDataEvent;
        fOnWriteData: TMethodStepFieldWriteDataEvent;
        function GetValue(): string;
        procedure SetValue(const aValue: string);
        procedure SetParam(aValue: TCustomSetting);
    public
        constructor Create(aDataID: integer; aParam: TCustomSetting);
        procedure ReadData();
        procedure WriteData();
        property Value: string read GetValue write SetValue;
        property DataID: integer read fDataID write fDataID;
        property Param: TCustomSetting read fParam write SetParam;
        property OnReadData: TMethodStepFieldReadDataEvent read fOnReadData write fOnReadData;
        property OnWriteData: TMethodStepFieldWriteDataEvent read fOnWriteData write fOnWriteData;
    end;

    TMethodStepDataFields = class(TIntegerKeyObjectValueList)
    private
        function GetField(aIndex: integer): TMethodStepDataField;
        procedure SetField(aIndex: integer; const Value: TMethodStepDataField);
    public
        constructor Create();
        destructor Destroy(); override;
        function GetValue(aDataID: integer): string;
        procedure SetValue(aDataID: integer; const aValue: string);
        property Fields[aIndex: integer]: TMethodStepDataField read GetField write SetField; default;
    end;


    // ##################################################################################################


implementation


uses
    SysUtils,
    MethodGUIParsing,
    CustomLeafSettings,
    ImportDataAdaptor;

{ TMethodStepDataLink }

constructor TMethodStepDataLink.Create();
begin
    inherited Create;
    self.OnReadData := nil;
    self.OnWriteData := nil;
end;

procedure TMethodStepDataLink.InitWithDefaults;
begin
    fOption := 0;
    fComment := 1;
    fInactive := 2;
end;

{ TMethodStepDataField }

constructor TMethodStepDataField.Create(aDataID: integer; aParam: TCustomSetting);
begin
    inherited Create();
    fDataID := aDataID;
    fParam := aParam;
end;

function TMethodStepDataField.GetValue: string;
begin
    result := fParam.Value;
end;

procedure TMethodStepDataField.ReadData;
var
    xValue: string;
begin
    if Assigned(fOnReadData) then
    begin
        xValue := self.Value;
        fOnReadData(self, xValue);
        self.Value := xValue;
    end;
end;

procedure TMethodStepDataField.SetParam(aValue: TCustomSetting);
begin
    fParam := aValue;
end;

procedure TMethodStepDataField.SetValue(const aValue: string);
begin
    fParam.Value := aValue;
end;

procedure TMethodStepDataField.WriteData;
begin
    if Assigned(fOnWriteData) then
        fOnWriteData(self, self.Value);
end;

{ TMethodStepDataFields }

constructor TMethodStepDataFields.Create();
begin
    inherited Create(true);
end;

destructor TMethodStepDataFields.Destroy();
begin
    inherited;
end;

function TMethodStepDataFields.GetField(aIndex: integer): TMethodStepDataField;
begin
    if (aIndex >= self.Count) or (aIndex < 0) then
        raise Exception.CreateFmt('TMethodStepDataFields.GetField -> index out of bounds %d', [aIndex]);
    result := self.Objects[aIndex] as TMethodStepDataField;
end;

function TMethodStepDataFields.GetValue(aDataID: integer): string;
var
    xIndex: integer;
begin
    result := '';
    xIndex := self.IndexOf(aDataID);
    if xIndex < 0 then
        EXIT;

    result := self.Fields[xIndex].Value;
end;

procedure TMethodStepDataFields.SetField(aIndex: integer; const Value: TMethodStepDataField);
begin
    if (aIndex >= self.Count) or (aIndex < 0) then
        raise Exception.CreateFmt('TMethodStepDataFields.SetField -> index out of bounds [%d]', [aIndex]);
    self.Objects[aIndex] := Value;
end;

procedure TMethodStepDataFields.SetValue(aDataID: integer; const aValue: string);
var
    xIndex: integer;
begin
    xIndex := self.IndexOf(aDataID);
    if xIndex < 0 then
        EXIT;

    self.Fields[xIndex].Value := aValue;
end;


end.
