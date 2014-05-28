{ -----------------------------------------------------------------------------------------------------------
  Copyright © 2013 Zinsser Analytic GmbH - All rights reserved.
  Author       : Wolfgang Lyncke (wl)
  Description  :
  -----------------------------------------------------------------------------------------------------------
  Revision History:
  date     op  method                              track-no improvement/change
  -------- --  ----------------------------------  -------- -------------------------------------------------
  08.03.13 wl                                      TN6095   Initial Revision
  11.03.13 wl                                      TN6095   jetzt Basisklasse für TEditFunctions
  19.04.13 wl                                      TN6095   für MultiPageDialog geändert
  14.05.13 wl                                      TN6095   mit Caption
  12.08.13 wl  CreateRackPosArrayPage              TN6214   neuer Parameter SingleValueAllowed
  10.02.14 ts                                      TN6353   Volume,Substance,LiqPar added
  08.04.14 ts                                      TN6391   SourceRack,-Pos möglich für 2.Layout in Parameterfenster
  ----------------------------------------------------------------------------------------------------------- }

unit BasicEditFunctions;


interface


uses
    Forms,
    Controls,
    Classes,
    RackPosArraySelectDialog,
    Generics.Collections,
    EditableParameter,
    MultiPageDialog,
    ParserStoredIdentifier;

type
    TBasicEditFunctions = class
    private
        class function GetRackPositionArray(const aRackNames, aPositions: string;
            out oRackNames, oPositions: TArray<string>): integer; static;
    protected
        function IsCurrentLayoutEmpty(): boolean; virtual; abstract;
        function GetCurrentLayoutName(): string; virtual; abstract;
    public
        function CreateCarrierMode(aOwner: TComponent; aCheckValues: boolean; const aDialogCaption: string;
            aCarrierParam: TEditableParameter): TMultiDialogPage;
        function CreateCarrierSlotMode(aOwner: TComponent; aCheckValues: boolean;
            const aDialogCaption: string; aCarrierParam, aCarrierSlotParam: TEditableParameter)
            : TMultiDialogPage;
        function CreateRackMode(aOwner: TComponent; aCheckValues: boolean; const aDialogCaption: string;
            aRackParam: TEditableParameter): TMultiDialogPage;
        function CreateSinglePosMode(aOwner: TComponent; aCheckValues: boolean; const aDialogCaption: string;
            aRackParam, aFirstPosParam: TEditableParameter): TMultiDialogPage;
        function CreateSinglePosModeExtended(aOwner: TComponent; aCheckValues: boolean;
            const aDialogCaption: string; aRackParam, aFirstPosParam, aSourceRackParam, aFirstSourcePosParam,
            aVolumeParam, aSubstanceParam, aLiqParamParam: TEditableParameter): TMultiDialogPage;
        function CreateDoublePosMode(aOwner: TComponent; aCheckValues: boolean; const aDialogCaption: string;
            aRackParam, aFirstPosParam, aLastPosParam: TEditableParameter): TMultiDialogPage;
        function CreateDoublePosModeExtended(aOwner: TComponent; aCheckValues: boolean;
            const aDialogCaption: string; aRackParam, aFirstPosParam, aLastPosParam, aSourceRackParam,
            aFirstSourcePosParam, aLastSourcePosParam, aVolumeParam, aSubstanceParam,
            aLiqParamParam: TEditableParameter): TMultiDialogPage;
        function CreateRackPosArrayPage(aOwner: TComponent; aCheckValues: boolean;
            const aData: TArray<TEditableParameterEditData>; aOnGetNoOfPos: TBasicEditFunctionsGetNoOfPos;
            aArrayLengthRefToOrder: integer; aSingleValueAllowed: boolean): TMultiDialogPage;
        class function ShowModalSinglePage(aMainForm: TfrmMultiPageDialog; const aTitle: string;
            aPage: TMultiDialogPage): boolean;

        class function EditPathName(var vValue: string; const aIdentName: string): boolean; static;
        class function EditFileName(var vValue: string): boolean; static;

        class function GetStringArrayOfPos(const aValue: string; const aNoOfPos: integer)
            : TArray<string>; static;
        class function StringArrayToStringValue(const aValues: TArray<string>; aSingleValueAllowed: boolean)
            : string; static;
        class function ValuesAreEqual(const aValues: TArray<string>): boolean; static;
        class function GetRackPositionArrayLength(const aRackNames, aPositions: string): integer; static;
    end;


implementation


uses
    SysUtils,
    Dialogs,
    GeneralTypes,
    ArrayUtils,
    ArrayFormat,
    MethodGUIParsing,
    LayoutElementSelectDialog,
    DialogUtils;

{ TBasicEditFunctions }

class function TBasicEditFunctions.ShowModalSinglePage(aMainForm: TfrmMultiPageDialog; const aTitle: string;
    aPage: TMultiDialogPage): boolean;
begin
    if not Assigned(aPage) then
        EXIT(false);

    aMainForm.Caption := aTitle;

    ASSERT(Assigned(aPage));
    aMainForm.AddPage(aPage);

    EXIT(aMainForm.ShowModal = mrOK);
end;

function TBasicEditFunctions.CreateCarrierMode(aOwner: TComponent; aCheckValues: boolean;
    const aDialogCaption: string; aCarrierParam: TEditableParameter): TMultiDialogPage;
var
    xPage: TfrmLayoutElementSelectDialog;
begin
    if self.IsCurrentLayoutEmpty then
        EXIT(nil);

    xPage := TfrmLayoutElementSelectDialog.Create(aOwner, self.GetCurrentLayoutName, aCheckValues);
    xPage.SetCarrierMode(aDialogCaption, aCarrierParam);
    EXIT(xPage);
end;

function TBasicEditFunctions.CreateCarrierSlotMode(aOwner: TComponent; aCheckValues: boolean;
    const aDialogCaption: string; aCarrierParam, aCarrierSlotParam: TEditableParameter): TMultiDialogPage;
var
    xPage: TfrmLayoutElementSelectDialog;
begin
    if self.IsCurrentLayoutEmpty then
        EXIT(nil);

    xPage := TfrmLayoutElementSelectDialog.Create(aOwner, self.GetCurrentLayoutName, aCheckValues);
    xPage.SetCarrierSlotMode(aDialogCaption, aCarrierParam, aCarrierSlotParam);
    EXIT(xPage);
end;

function TBasicEditFunctions.CreateDoublePosMode(aOwner: TComponent; aCheckValues: boolean;
    const aDialogCaption: string; aRackParam, aFirstPosParam, aLastPosParam: TEditableParameter)
    : TMultiDialogPage;
var
    xPage: TfrmLayoutElementSelectDialog;
begin
    if self.IsCurrentLayoutEmpty then
        EXIT(nil);

    xPage := TfrmLayoutElementSelectDialog.Create(aOwner, self.GetCurrentLayoutName, aCheckValues);
    xPage.SetRackDoublePosMode(aDialogCaption, aRackParam, aFirstPosParam, aLastPosParam);
    EXIT(xPage);
end;

function TBasicEditFunctions.CreateDoublePosModeExtended(aOwner: TComponent; aCheckValues: boolean;
    const aDialogCaption: string; aRackParam, aFirstPosParam, aLastPosParam, aSourceRackParam,
    aFirstSourcePosParam, aLastSourcePosParam, aVolumeParam, aSubstanceParam,
    aLiqParamParam: TEditableParameter): TMultiDialogPage;
var
    xPage: TfrmLayoutElementSelectDialog;
begin
    if self.IsCurrentLayoutEmpty then
        EXIT(nil);
    xPage := TfrmLayoutElementSelectDialog.Create(aOwner, self.GetCurrentLayoutName, aCheckValues);
    xPage.SetRackDoublePosModeExtended(aDialogCaption, aRackParam, aFirstPosParam, aLastPosParam,
        aSourceRackParam, aFirstSourcePosParam, aLastSourcePosParam, aVolumeParam, aSubstanceParam,
        aLiqParamParam);
    EXIT(xPage);
end;

function TBasicEditFunctions.CreateRackMode(aOwner: TComponent; aCheckValues: boolean;
    const aDialogCaption: string; aRackParam: TEditableParameter): TMultiDialogPage;
var
    xPage: TfrmLayoutElementSelectDialog;
begin
    if self.IsCurrentLayoutEmpty then
        EXIT(nil);

    xPage := TfrmLayoutElementSelectDialog.Create(aOwner, self.GetCurrentLayoutName, aCheckValues);
    xPage.SetRackMode(aDialogCaption, aRackParam);
    EXIT(xPage);
end;

function TBasicEditFunctions.CreateSinglePosMode(aOwner: TComponent; aCheckValues: boolean;
    const aDialogCaption: string; aRackParam, aFirstPosParam: TEditableParameter): TMultiDialogPage;
var
    xPage: TfrmLayoutElementSelectDialog;
begin
    if self.IsCurrentLayoutEmpty then
        EXIT(nil);

    xPage := TfrmLayoutElementSelectDialog.Create(aOwner, self.GetCurrentLayoutName, aCheckValues);
    xPage.SetRackSinglePosMode(aDialogCaption, aRackParam, aFirstPosParam);
    EXIT(xPage);
end;

function TBasicEditFunctions.CreateSinglePosModeExtended(aOwner: TComponent; aCheckValues: boolean;
    const aDialogCaption: string; aRackParam, aFirstPosParam, aSourceRackParam, aFirstSourcePosParam,
    aVolumeParam, aSubstanceParam, aLiqParamParam: TEditableParameter): TMultiDialogPage;
var
    xPage: TfrmLayoutElementSelectDialog;
begin
    if self.IsCurrentLayoutEmpty then
        EXIT(nil);

    xPage := TfrmLayoutElementSelectDialog.Create(aOwner, self.GetCurrentLayoutName, aCheckValues);
    xPage.SetRackSinglePosModeExtended(aDialogCaption, aRackParam, aFirstPosParam, aSourceRackParam,
        aFirstSourcePosParam, aVolumeParam, aSubstanceParam, aLiqParamParam);
    EXIT(xPage);
end;

function TBasicEditFunctions.CreateRackPosArrayPage(aOwner: TComponent; aCheckValues: boolean;
    const aData: TArray<TEditableParameterEditData>; aOnGetNoOfPos: TBasicEditFunctionsGetNoOfPos;
    aArrayLengthRefToOrder: integer; aSingleValueAllowed: boolean): TMultiDialogPage;
var
    xPageForm: TfrmRackPosArraySelectDialog;
begin
    xPageForm := TfrmRackPosArraySelectDialog.Create(aOwner, self.GetCurrentLayoutName(), aCheckValues,
        aSingleValueAllowed);
    xPageForm.Prepare(aData, aOnGetNoOfPos, aArrayLengthRefToOrder);

    EXIT(xPageForm);
end;

class function TBasicEditFunctions.EditFileName(var vValue: string): boolean;
var
    xOpenDialog: TOpenDialog;
begin
    xOpenDialog := TOpenDialog.Create(nil);
    try
        xOpenDialog.Options := xOpenDialog.Options + [ofNoChangeDir];
        xOpenDialog.FileName := vValue;
        result := xOpenDialog.Execute;
        if (result) then
            vValue := xOpenDialog.FileName;
    finally
        xOpenDialog.Free;
    end;
end;

class function TBasicEditFunctions.EditPathName(var vValue: string; const aIdentName: string): boolean;
var
    xRoot: string;
begin
    xRoot := vValue; // ??
    EXIT(TDialogUtils.SelectDirectory(TLanguageString.Read('Please choose directory for parameter {0}:',
        'Bitte wählen Sie einen Pfadnamen für Parameter {0}:', [aIdentName]), xRoot, vValue));
end;

class function TBasicEditFunctions.GetRackPositionArrayLength(const aRackNames, aPositions: string): integer;
var
    xRackNames, xPositions: TArray<string>;
begin
    EXIT(GetRackPositionArray(aRackNames, aPositions, xRackNames, xPositions));
end;

class function TBasicEditFunctions.GetRackPositionArray(const aRackNames, aPositions: string;
    out oRackNames, oPositions: TArray<string>): integer;
begin
    if (aRackNames = '') and (aPositions = '') then
    begin
        SetLength(oRackNames, 0);
        SetLength(oPositions, 0);
        EXIT(0);
    end;

    EXIT(TMethodGUIParser.GetRackPositionArray(aRackNames, aPositions, oRackNames, oPositions));
end;

class function TBasicEditFunctions.GetStringArrayOfPos(const aValue: string; const aNoOfPos: integer)
    : TArray<string>;
var
    x: integer;
    xArray: TArray<string>;
begin
    if TArrayFormat.StringIsArrayFormat(aValue) then
    begin
        // das Ergebnis ist ein Array
        xArray := TArrayFormat.ArrayFormatToStringArray(aValue);
        if (Length(xArray) < aNoOfPos) then
        begin // einfach noch was dranhängen
            SetLength(result, aNoOfPos);
            for x := 0 to high(xArray) do
                result[x] := xArray[x];
            for x := ( high(xArray) + 1) to high(result) do
                result[x] := '';
        end
        else
            result := xArray;
    end
    else
    begin
        // Das Ergebnis ist ein Einzelwert
        SetLength(result, aNoOfPos);
        for x := 0 to high(result) do
            result[x] := aValue;
    end;
end;

class function TBasicEditFunctions.StringArrayToStringValue(const aValues: TArray<string>;
    aSingleValueAllowed: boolean): string;
begin
    if (aSingleValueAllowed) then
    begin
        if Length(aValues) = 0 then
            EXIT('');
        if ValuesAreEqual(aValues) then
            EXIT(aValues[0]);
    end;

    EXIT(TArrayFormat.StringArrayToArrayFormat(aValues));
end;

class function TBasicEditFunctions.ValuesAreEqual(const aValues: TArray<string>): boolean;
var
    x: integer;
begin
    if Length(aValues) <= 1 then
        EXIT(true);

    for x := 1 to high(aValues) do
    begin
        if (aValues[x] <> aValues[0]) then
            EXIT(false);
    end;
    EXIT(true);
end;


end.
