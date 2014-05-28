{ -----------------------------------------------------------------------------------------------------------
  Copyright © 2013 Zinsser Analytic GmbH - All rights reserved.
  Author       : Wolfgang Lyncke (wl)
  Description  :
  -----------------------------------------------------------------------------------------------------------
  Revision History:
  date     op  method                              track-no improvement/change
  -------- --  ----------------------------------  -------- -------------------------------------------------
  22.04.13 wl                                      TN6095   Initial Revision
  14.05.13 wl                                      TN6095   DialogCaption entfernt
  10.02.14 ts                                      TN6353   scdVolume, scdSubstance, scdLiqParam added
  08.04.14 ts                                      TN6391   scdSourceRackName, scdSourcePosition added
  ----------------------------------------------------------------------------------------------------------- }

unit EditableParameter;


interface


uses
    SysUtils;

type
    ESetEditableParameterException = class(Exception);

    TEditableParameterDataType = (scdRackName, scdSourceRackName, scdSourcePosition, scdPosition, scdVolume,
        scdSubstance, scdLiqParam, scdOther);

    // Basisklasse für TParserStoredIdent, evtl. auch für TCustomSetting
    TEditableParameter = class
    protected
        fValue: string;
        function GetDescription: string; virtual; abstract;
        function GetColumnDescription: string; virtual; abstract;
        function GetColumnWidth: integer; virtual; abstract;
        function GetDialogType: TEditableParameterDataType; virtual; abstract;
    public
        function GetPickList(): TArray<string>; virtual; abstract;
        procedure CheckValue(); virtual; abstract;
        function HasEditFunction: boolean; virtual; abstract;
        property Description: string read GetDescription;
        property Value: string read fValue write fValue;
        property DialogType: TEditableParameterDataType read GetDialogType;
        property ColumnWidth: integer read GetColumnWidth;
        property ColumnDescription: string read GetColumnDescription;
    end;

    TEditableParameterEditData = record
        Enabled: boolean;
        Param: TEditableParameter;
    end;

    TSimpleParameter = class(TEditableParameter)
    private
        fColumnWidth: integer;
        fDataType: TEditableParameterDataType;
        fColumnDescription: string;
        fPickList: TArray<string>;
        fColumnEnabled: boolean;
    protected
        function GetDescription: string; override;
        function GetColumnWidth: integer; override;
        function GetColumnDescription: string; override;
        function GetDialogType: TEditableParameterDataType; override;
    public
        constructor Create(const aValue: string); overload;
        constructor Create(const aValue: string; aColumnEnabled: boolean; const aColumnDescription: string;
            aColumnWidth: integer; aDataType: TEditableParameterDataType;
            const aPickList: TArray<string>); overload;

        procedure CheckValue(); override;
        function GetPickList(): TArray<string>; override;
        function HasEditFunction: boolean; override;
        property ColumnEnabled: boolean read fColumnEnabled;
    end;


implementation


{ TSimpleStringParameter }

procedure TSimpleParameter.CheckValue;
begin
    //
end;

constructor TSimpleParameter.Create(const aValue: string);
begin
    Create(aValue, false, '', 0, scdOther, nil);
end;

constructor TSimpleParameter.Create(const aValue: string; aColumnEnabled: boolean;
    const aColumnDescription: string; aColumnWidth: integer; aDataType: TEditableParameterDataType;
    const aPickList: TArray<string>);
begin
    inherited Create;

    fValue := aValue;
    fColumnEnabled := aColumnEnabled;
    fColumnDescription := aColumnDescription;
    fColumnWidth := aColumnWidth;
    fDataType := aDataType;
    fPickList := aPickList;
end;

function TSimpleParameter.GetColumnDescription: string;
begin
    EXIT(fColumnDescription);
end;

function TSimpleParameter.GetColumnWidth: integer;
begin
    EXIT(fColumnWidth);
end;

function TSimpleParameter.GetDialogType: TEditableParameterDataType;
begin
    EXIT(fDataType);
end;

function TSimpleParameter.GetDescription: string;
begin
    EXIT('');
end;

function TSimpleParameter.GetPickList: TArray<string>;
begin
    EXIT(fPickList);
end;

function TSimpleParameter.HasEditFunction: boolean;
begin
    EXIT(false);
end;


end.
