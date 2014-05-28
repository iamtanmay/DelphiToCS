{ -----------------------------------------------------------------------------------------------------------
  Copyright © 2013 Zinsser Analytic GmbH - All rights reserved.
  Author       : Wolfgang Lyncke (wl)
  Description  :
  -----------------------------------------------------------------------------------------------------------
  Revision History:
  date     op  method                              track-no improvement/change
  -------- --  ----------------------------------  -------- -------------------------------------------------
  22.04.13 wl                                      TN6095   Initial Revision
  14.05.13 wl                                      TN6095   verwendet MethodVarPagesDataAdaptor
  12.08.13 wl  CreateArrayParameterPage            TN6214   Parameter SingleValueAllowed = false
  10.02.14 ts                                      TN6353   Volume,Substance,LiqPar added
  13.02.14 ts                                      TN6353   Bugfix
  08.04.14 ts                                      TN6391   SourceRackName,-Position hinzu für Anzeige von 2.Layout
  ----------------------------------------------------------------------------------------------------------- }

unit ParserValueRequest;


interface


uses
    Forms,
    Classes,
    Controls,
    Generics.Collections,
    ParserInterface,
    MethodVariableTypes,
    EditableParameter,
    ParserStoredIdentifier,
    BasicEditFunctions,
    MultiPageDialog;

type
    TParserValueRequest = class
    private
        fIdentList: TObjectList<TParserStoredIdent>;
        fPageInfos: TArray<TMethodVarPageRec>;
        fCheckValues: boolean;
        fEditFunctions: TBasicEditFunctions;
        fShowCancelButton: boolean;
        function GetNoOfPos(aArrayLengthRefToOrder: integer): integer;
        class function AllValuesAreArrays(const aIdents: TArray<TParserStoredIdent>;
            out oArrayLengthRefToOrder: integer): boolean;
        class procedure PrepareList(aIdentList: TObjectList<TParserStoredIdent>);
        procedure WriteEditedValues();
        class function FindIdentWithType(const aIdents: TArray<TParserStoredIdent>;
            aType: TMethodVariableFormatType; aIsArray: boolean; aOrderIndex: integer;
            out oIdent: TParserStoredIdent): boolean;
        class function Find2IdentsWithType(const aIdents: TArray<TParserStoredIdent>;
            aType1, aType2: TMethodVariableFormatType; aIsArray: boolean; aOrderIndex: integer;
            out oIdent1, oIdent2: TParserStoredIdent): boolean;
        class function Find3IdentsWithType(const aIdents: TArray<TParserStoredIdent>;
            aType1, aType2, aType3: TMethodVariableFormatType; aIsArray: boolean; aOrderIndex: integer;
            out oIdent1, oIdent2, oIdent3: TParserStoredIdent): boolean;
        class function FindIdentsByOrder(const aIdentList: TList<TParserStoredIdent>; aOrderIndex: integer)
            : TArray<TParserStoredIdent>;
        class function FindArrayIdents(const aAllIdents: TArray<TParserStoredIdent>; aOrderIndex: integer;
            aSort: boolean; var vPosCount: integer): TArray<TParserStoredIdent>;
        function CreateSpecificDialogModal(aOwner: TComponent; aCheckValues: boolean;
            aIdent: TParserStoredIdent): TMultiDialogPage;
        function CreateSpecificDialogPage(aOwner: TComponent; aCheckValues: boolean; aRequestOrder: integer;
            const aDialogCaption: string; const aIdents: TArray<TParserStoredIdent>): TMultiDialogPage;
        function ShowParameterDialogCasted(aIdent: TParserStoredIdent): boolean;
        function ShowParameterDialog(aIdent: TEditableParameter): boolean;
        function CreateArrayParameterPage(aOwner: TComponent; aCheckValues: boolean;
            const aIdents: TArray<TParserStoredIdent>; aRequestOrder, aArrayLengthRefToOrder: integer)
            : TMultiDialogPage;
        function CreateEditorPage(aOwner: TComponent; const aCaption: string;
            const aIdents: TArray<TParserStoredIdent>; const aMethodName: string;
            const aEnterKeyNavigationOnly: boolean): TMultiDialogPage;
        procedure AddEditorPage(const aPageInfo: TMethodVarPageRec; aForm: TfrmMultiPageDialog;
            const aMethodName: string; const aEnterKeyNavigationOnly: boolean);
        procedure AddFinalPage(aForm: TfrmMultiPageDialog; const aMethodName: string;
            const aEnterKeyNavigationOnly: boolean);
    public
        constructor Create(aIdentList: TObjectList<TParserStoredIdent>;
            const aPageInfos: TArray<TMethodVarPageRec>; aCheckValues, aShowCancelButton: boolean;
            aEditFunctions: TBasicEditFunctions);

        function ShowMultiPageModal(const aMethodName: string; const aEnterKeyNavigationOnly: boolean)
            : TModalResult;

        class function ShowModal(const aMethodName: string; aIdentList: TObjectList<TParserStoredIdent>;
            const aPageInfos: TArray<TMethodVarPageRec>; aCheckValues, aShowCancelButton: boolean;
            const aEnterKeyNavigationOnly: boolean; aEditFunctions: TBasicEditFunctions): TModalResult;
    end;


implementation


uses
    Windows,
    SysUtils,
    GeneralTypes,
    RackPosArraySelectDialog;

{ TParserValueRequest }

constructor TParserValueRequest.Create(aIdentList: TObjectList<TParserStoredIdent>;
    const aPageInfos: TArray<TMethodVarPageRec>; aCheckValues, aShowCancelButton: boolean;
    aEditFunctions: TBasicEditFunctions);
begin
    inherited Create;

    fIdentList := aIdentList; // wird nicht in diesem Objekt zerstört!!
    fCheckValues := aCheckValues;
    fShowCancelButton := aShowCancelButton;
    fEditFunctions := aEditFunctions;
    fPageInfos := aPageInfos;
end;

function TParserValueRequest.CreateEditorPage(aOwner: TComponent; const aCaption: string;
    const aIdents: TArray<TParserStoredIdent>; const aMethodName: string;
    const aEnterKeyNavigationOnly: boolean): TMultiDialogPage;
var
    xPage: TParserSetValueForm;
    xEditIdents: TArray<TEditableParameter>;
    x: integer;
begin
    SetLength(xEditIdents, Length(aIdents));
    for x := 0 to high(xEditIdents) do
        xEditIdents[x] := aIdents[x];

    // Normale Parser-Request-Seite erzeugen
    xPage := TParserSetValueForm.Create(aOwner, aMethodName, aEnterKeyNavigationOnly, ShowParameterDialog,
        fCheckValues);
    xPage.AddIdentifiers(xEditIdents);
    EXIT(xPage);
end;

procedure TParserValueRequest.AddEditorPage(const aPageInfo: TMethodVarPageRec; aForm: TfrmMultiPageDialog;
    const aMethodName: string; const aEnterKeyNavigationOnly: boolean);
var
    xFirstIndex, xLastIndex, x: integer;
    xPage: TMultiDialogPage;
    xIdents: TList<TParserStoredIdent>;
begin
    xPage := nil;
    xFirstIndex := aPageInfo.FirstOrderIndex;
    xLastIndex := aPageInfo.LastOrderIndex;

    xIdents := TList<TParserStoredIdent>.Create;
    try
        if (xLastIndex <= xFirstIndex) then
            xIdents.AddRange(FindIdentsByOrder(fIdentList, xFirstIndex))
        else
        begin
            for x := xFirstIndex to xLastIndex do
                xIdents.AddRange(FindIdentsByOrder(fIdentList, x));
        end;

        if (xIdents.Count <= 0) then
            EXIT;

        // Spezielle Seite für Layout und Arrays (möglich, wenn es nur einen Index gibt)
        if (xLastIndex <= xFirstIndex) then
            xPage := CreateSpecificDialogPage(aForm, fCheckValues, xFirstIndex, aPageInfo.Caption,
                xIdents.ToArray);

        // Normale Parser-Request-Seite erzeugen
        if (xPage = nil) then
            xPage := CreateEditorPage(aForm, aPageInfo.Caption, xIdents.ToArray, aMethodName,
                aEnterKeyNavigationOnly);

        // Seite hinzufügen
        aForm.AddPage(xPage);
    finally
        FreeAndNil(xIdents);
    end;
end;

procedure TParserValueRequest.AddFinalPage(aForm: TfrmMultiPageDialog; const aMethodName: string;
    const aEnterKeyNavigationOnly: boolean);
var
    xAnyIdent: boolean;
    x: integer;
    xPage: TParserSetValueForm;
begin
    // Testen, ob überhaupt Variablen gezeigt werden sollen
    xAnyIdent := false;
    for x := 0 to fIdentList.Count - 1 do
    begin
        if not fIdentList[x].Data.DialogHide then
            xAnyIdent := true;
    end;
    if not xAnyIdent then
        EXIT;

    // Main Page: Am Schluß wie gehabt die Gesamtübersicht
    xPage := TParserSetValueForm.Create(aForm, aMethodName, aEnterKeyNavigationOnly, ShowParameterDialog,
        fCheckValues);
    for x := 0 to fIdentList.Count - 1 do
    begin
        if not fIdentList[x].Data.DialogHide then
            xPage.AddIdentifier(fIdentList[x]);
    end;
    aForm.AddPage(xPage);
end;

class function TParserValueRequest.ShowModal(const aMethodName: string;
    aIdentList: TObjectList<TParserStoredIdent>; const aPageInfos: TArray<TMethodVarPageRec>;
    aCheckValues, aShowCancelButton: boolean; const aEnterKeyNavigationOnly: boolean;
    aEditFunctions: TBasicEditFunctions): TModalResult;
var
    xInstance: TParserValueRequest;
begin
    xInstance := TParserValueRequest.Create(aIdentList, aPageInfos, aCheckValues, aShowCancelButton,
        aEditFunctions);
    try
        EXIT(xInstance.ShowMultiPageModal(aMethodName, aEnterKeyNavigationOnly));
    finally
        FreeAndNil(xInstance);
    end;
end;

class procedure TParserValueRequest.PrepareList(aIdentList: TObjectList<TParserStoredIdent>);
var
    x: integer;
begin
    for x := 0 to aIdentList.Count - 1 do
    begin
        // CurrentValue wird entweder LinkedIdend.Value oder Defaultwert
        aIdentList[x].SetValueToLinkedValue();
        aIdentList[x].SetValueToDefaultIfUndefined();
    end;

    // Neu sortieren
    aIdentList.Sort(TParserStoredIdentComparer.Create);
end;

procedure TParserValueRequest.WriteEditedValues();
var
    x: integer;
begin
    // Werte zurück in die verlinkten Identifier schreiben
    for x := 0 to FIdentList.Count - 1 do
    begin
        FIdentList[x].WriteValueToLinkedValue;
    end;
end;

function TParserValueRequest.ShowMultiPageModal(const aMethodName: string;
    const aEnterKeyNavigationOnly: boolean): TModalResult;
var
    xForm: TfrmMultiPageDialog;
    xScreenCursor: TCursor;
    x: integer;
begin
    if (fIdentList.Count <= 0) then
        EXIT(mrOK);

    PrepareList(fIdentList);

    xScreenCursor := Screen.Cursor;
    Screen.Cursor := crDefault;
    try
        xForm := TfrmMultiPageDialog.Create(nil);
        try
            if (not fShowCancelButton) then
            begin // kein Cancel während des Laufs!
                xForm.btnCancel.Visible := false;
                EnableMenuItem(GetSystemMenu(xForm.handle, False), SC_CLOSE, MF_BYCOMMAND or MF_GRAYED);
            end;

            xForm.Caption := TLanguageString.read('Method {0}: Parameters', 'Methode {0}: Parameter',
                [aMethodName]);

            for x := 0 to high(fPageInfos) do
                AddEditorPage(fPageInfos[x], xForm, aMethodName, aEnterKeyNavigationOnly);

            AddFinalPage(xForm, aMethodName, aEnterKeyNavigationOnly);

            if (xForm.PageCount <= 0) then
            begin
                // Sonderfall: Nur versteckte Parameter - Default-Werte werden direkt übernommen
                result := mrOK
            end
            else
            begin
                // Fenster wird normal gezeigt
                result := xForm.ShowModal;
            end;

            if result = mrOK then
                WriteEditedValues();
        finally
            xForm.Free;
        end;
    finally
        Screen.Cursor := xScreenCursor;
    end;
end;

class function TParserValueRequest.FindIdentsByOrder(const aIdentList: TList<TParserStoredIdent>;
    aOrderIndex: integer): TArray<TParserStoredIdent>;
var
    x: integer;
    xTempIdentList: TList<TParserStoredIdent>;
begin
    xTempIdentList := TList<TParserStoredIdent>.Create;
    try
        for x := 0 to aIdentList.Count - 1 do
        begin
            if (aIdentList[x].Data.RequestOrder <> aOrderIndex) then
                CONTINUE;

            xTempIdentList.Add(aIdentList[x]);
        end;

        EXIT(xTempIdentList.ToArray);
    finally
        FreeAndNil(xTempIdentList);
    end;
end;

class function TParserValueRequest.FindIdentWithType(const aIdents: TArray<TParserStoredIdent>;
    aType: TMethodVariableFormatType; aIsArray: boolean; aOrderIndex: integer;
    out oIdent: TParserStoredIdent): boolean;
var
    x: integer;
begin
    for x := 0 to high(aIdents) do
    begin
        if (aIdents[x].Data.RequestOrder <> aOrderIndex) then
            CONTINUE;

        if (aIdents[x].Data.DataType = aType) and (aIdents[x].Data.DataIsArray = aIsArray) then
        begin
            oIdent := aIdents[x];
            EXIT(true);
        end;
    end;
    oIdent := nil;
    EXIT(false);
end;

class function TParserValueRequest.Find2IdentsWithType(const aIdents: TArray<TParserStoredIdent>;
    aType1, aType2: TMethodVariableFormatType; aIsArray: boolean; aOrderIndex: integer;
    out oIdent1, oIdent2: TParserStoredIdent): boolean;
var
    xIdent1, xIdent2: TParserStoredIdent;
begin
    if FindIdentWithType(aIdents, aType1, aIsArray, aOrderIndex, xIdent1) and
        FindIdentWithType(aIdents, aType2, aIsArray, aOrderIndex, xIdent2) then
    begin
        oIdent1 := xIdent1;
        oIdent2 := xIdent2;
        EXIT(true);
    end;

    oIdent1 := nil;
    oIdent2 := nil;
    EXIT(false);
end;

class function TParserValueRequest.Find3IdentsWithType(const aIdents: TArray<TParserStoredIdent>;
    aType1, aType2, aType3: TMethodVariableFormatType; aIsArray: boolean; aOrderIndex: integer;
    out oIdent1, oIdent2, oIdent3: TParserStoredIdent): boolean;
var
    xIdent1, xIdent2, xIdent3: TParserStoredIdent;
begin
    if FindIdentWithType(aIdents, aType1, aIsArray, aOrderIndex, xIdent1) and
        FindIdentWithType(aIdents, aType2, aIsArray, aOrderIndex, xIdent2) and
        FindIdentWithType(aIdents, aType3, aIsArray, aOrderIndex, xIdent3) then
    begin
        oIdent1 := xIdent1;
        oIdent2 := xIdent2;
        oIdent3 := xIdent3;
        EXIT(true);
    end;

    oIdent1 := nil;
    oIdent2 := nil;
    oIdent3 := nil;
    EXIT(false);
end;

class function TParserValueRequest.FindArrayIdents(const aAllIdents: TArray<TParserStoredIdent>;
    aOrderIndex: integer; aSort: boolean; var vPosCount: integer): TArray<TParserStoredIdent>;
var
    x: integer;
    xParams: TList<TParserStoredIdent>;
    xRackNames, xPositions, xPosCountValue: string;
begin
    xParams := TList<TParserStoredIdent>.Create(TParserStoredIdentComparer.Create);
    try
        xRackNames := '';
        xPositions := '';
        xPosCountValue := '';
        for x := 0 to high(aAllIdents) do
        begin
            if (aAllIdents[x].Data.RequestOrder <> aOrderIndex) then
                CONTINUE;

            if aAllIdents[x].Data.DataIsArray then
                xParams.Add(aAllIdents[x]);

            if (aAllIdents[x].Data.DataType = TMethodVariableFormatType.mvfRackName) then
                xRackNames := aAllIdents[x].Value;
            if (aAllIdents[x].Data.DataType = TMethodVariableFormatType.mvfRackPos) then
                xPositions := aAllIdents[x].Value;
            if (aAllIdents[x].Data.DataType = TMethodVariableFormatType.mvfInteger) then
                xPosCountValue := aAllIdents[x].Value;
        end;

        // PosCount bestimmen
        if (vPosCount <= 0) then // Zunächst mit fest definierter Array-Length
            vPosCount := StrToIntDef(xPosCountValue, 0);
        if (vPosCount <= 0) then // dann über die Rack-Positionen
            vPosCount := TBasicEditFunctions.GetRackPositionArrayLength(xRackNames, xPositions);

        if (aSort) then
            xParams.Sort;

        EXIT(xParams.ToArray());
    finally
        FreeAndNil(xParams);
    end;
end;

function TParserValueRequest.CreateSpecificDialogPage(aOwner: TComponent; aCheckValues: boolean;
    aRequestOrder: integer; const aDialogCaption: string; const aIdents: TArray<TParserStoredIdent>)
    : TMultiDialogPage;
var
    xArrayLengthRefToOrder: integer;
    xIdent1, xIdent2, xIdent3, xIdent4, xIdent5, xIdent6, xIdent7, xIdent8, xIdent9: TParserStoredIdent;
begin
    // alle Beteiligten müssen Arrays sein -> sonst normale Parameter-Page zeigen
    if AllValuesAreArrays(aIdents, xArrayLengthRefToOrder) then
        EXIT(CreateArrayParameterPage(aOwner, aCheckValues, aIdents, aRequestOrder, xArrayLengthRefToOrder));

    // Hier muss die Anzahl der Parameter genau stimmen -> sonst normale Parameter-Page zeigen
    if (Length(aIdents) = 4) or (Length(aIdents) = 5) or (Length(aIdents) = 6) or (Length(aIdents) = 7) then
    begin
        // Double Position (First pos and last pos)
        if Find3IdentsWithType(aIdents, mvfRackName, mvfRackPos, mvfRackLastPos, false, aRequestOrder,
            xIdent1, xIdent2, xIdent3) then
        begin
            // Source positions
            Find3IdentsWithType(aIdents, mvfSourceRackName, mvfSourceRackPos, mvfSourceRackLastPos, false,
                aRequestOrder, xIdent7, xIdent8, xIdent9);
            // Volume
            FindIdentWithType(fIdentList.ToArray, mvfVolume, false, aRequestOrder, xIdent4);
            // Substance
            FindIdentWithType(fIdentList.ToArray, mvfSubstance, false, aRequestOrder, xIdent5);
            // LiqParam
            FindIdentWithType(fIdentList.ToArray, mvfLiqParam, false, aRequestOrder, xIdent6);

            if (xIdent4 <> nil) or (xIdent5 <> nil) or (xIdent6 <> nil) or (xIdent7 <> nil) or
                (xIdent8 <> nil) or (xIdent9 <> nil) then
                EXIT(fEditFunctions.CreateDoublePosModeExtended(aOwner, aCheckValues, '', xIdent1, xIdent2,
                    xIdent3, xIdent7, xIdent8, xIdent9, xIdent4, xIdent5, xIdent6))
            else
                EXIT(fEditFunctions.CreateDoublePosMode(aOwner, aCheckValues, aDialogCaption, xIdent1,
                    xIdent2, xIdent3));
        end;
    end
    else if (Length(aIdents) = 3) then
    begin
        // Double Position (First pos and last pos)
        if Find3IdentsWithType(aIdents, mvfRackName, mvfRackPos, mvfRackLastPos, false, aRequestOrder,
            xIdent1, xIdent2, xIdent3) then
            EXIT(fEditFunctions.CreateDoublePosMode(aOwner, aCheckValues, aDialogCaption, xIdent1, xIdent2,
                xIdent3));
    end
    else if (Length(aIdents) = 2) then
    begin
        // Single Position
        if Find2IdentsWithType(aIdents, mvfRackName, mvfRackPos, false, aRequestOrder, xIdent1, xIdent2) then
            EXIT(fEditFunctions.CreateSinglePosMode(aOwner, aCheckValues, aDialogCaption, xIdent1, xIdent2));

        // Slot und Carrier
        if Find2IdentsWithType(aIdents, mvfCarrier, mvfCarrierSlot, false, aRequestOrder, xIdent1,
            xIdent2) then
            EXIT(fEditFunctions.CreateCarrierSlotMode(aOwner, aCheckValues, aDialogCaption, xIdent1, xIdent2))
    end
    else if (Length(aIdents) = 1) then
    begin
        if FindIdentWithType(aIdents, mvfRackName, false, aRequestOrder, xIdent1) then
            EXIT(fEditFunctions.CreateRackMode(aOwner, aCheckValues, aDialogCaption, xIdent1));

        if FindIdentWithType(aIdents, mvfCarrier, false, aRequestOrder, xIdent1) then
            EXIT(fEditFunctions.CreateCarrierMode(aOwner, aCheckValues, aDialogCaption, xIdent1));
    end;

    EXIT(nil);
end;

function TParserValueRequest.ShowParameterDialog(aIdent: TEditableParameter): boolean;
begin
    EXIT(ShowParameterDialogCasted(aIdent as TParserStoredIdent));
end;

function TParserValueRequest.CreateSpecificDialogModal(aOwner: TComponent; aCheckValues: boolean;
    aIdent: TParserStoredIdent): TMultiDialogPage;
var
    xIdent1, xIdent2, xIdent3, xIdent4, xIdent5, xIdent6, xIdent7, xIdent8, xIdent9: TParserStoredIdent;
begin
    if aIdent.Data.DataIsArray then
    begin
        EXIT(CreateArrayParameterPage(aOwner, aCheckValues, fIdentList.ToArray, aIdent.Data.RequestOrder,
            aIdent.Data.ArrayLengthRefToOrder));
    end;

    if (aIdent.Data.DataType = mvfCarrier) then
    begin
        // Slot und Carrier
        if FindIdentWithType(fIdentList.ToArray, mvfCarrierSlot, false, aIdent.Data.RequestOrder,
            xIdent2) then
            EXIT(fEditFunctions.CreateCarrierSlotMode(aOwner, aCheckValues, '', aIdent, xIdent2));

        EXIT(fEditFunctions.CreateCarrierMode(aOwner, aCheckValues, '', aIdent));
    end;

    if (aIdent.Data.DataType = mvfCarrierSlot) then
    begin
        // Slot und Carrier
        if FindIdentWithType(fIdentList.ToArray, mvfCarrier, false, aIdent.Data.RequestOrder, xIdent2) then
            EXIT(fEditFunctions.CreateCarrierSlotMode(aOwner, aCheckValues, '', xIdent2, aIdent));
    end;

    if (aIdent.Data.DataType = mvfRackName) then
    begin
        // Double Position (First pos and last pos)
        if Find2IdentsWithType(fIdentList.ToArray, mvfRackPos, mvfRackLastPos, false,
            aIdent.Data.RequestOrder, xIdent2, xIdent3) then
        begin
            // Volume
            FindIdentWithType(fIdentList.ToArray, mvfVolume, false, aIdent.Data.RequestOrder, xIdent4);
            // Substance
            FindIdentWithType(fIdentList.ToArray, mvfSubstance, false, aIdent.Data.RequestOrder, xIdent5);
            // LiqParam
            FindIdentWithType(fIdentList.ToArray, mvfLiqParam, false, aIdent.Data.RequestOrder, xIdent6);
            // Source positions
            Find3IdentsWithType(fIdentList.ToArray, mvfSourceRackName, mvfSourceRackPos, mvfSourceRackLastPos,
                false, aIdent.Data.RequestOrder, xIdent7, xIdent8, xIdent9);
        end;
        if (xIdent4 <> nil) or (xIdent5 <> nil) or (xIdent6 <> nil) or (xIdent7 <> nil) or (xIdent8 <> nil) or
            (xIdent9 <> nil) then
            EXIT(fEditFunctions.CreateDoublePosModeExtended(aOwner, aCheckValues, '', aIdent, xIdent2,
                xIdent3, xIdent7, xIdent8, xIdent9, xIdent4, xIdent5, xIdent6))
        else
            EXIT(fEditFunctions.CreateDoublePosMode(aOwner, aCheckValues, '', aIdent, xIdent2, xIdent3));

        // Single Position
        if FindIdentWithType(fIdentList.ToArray, mvfRackPos, false, aIdent.Data.RequestOrder, xIdent2) then
            if // Source positions
                Find2IdentsWithType(fIdentList.ToArray, mvfSourceRackName, mvfSourceRackPos, false,
                aIdent.Data.RequestOrder, xIdent7, xIdent8) then
                EXIT(fEditFunctions.CreateDoublePosModeExtended(aOwner, aCheckValues, '', aIdent, xIdent2,
                    xIdent3, xIdent7, xIdent8, xIdent9, xIdent4, xIdent5, xIdent6))
            else
                EXIT(fEditFunctions.CreateSinglePosMode(aOwner, aCheckValues, '', aIdent, xIdent2));

        // nur ein Rack
        EXIT(fEditFunctions.CreateRackMode(aOwner, aCheckValues, '', aIdent));
    end;

    if (aIdent.Data.DataType = mvfRackPos) then
    begin
        // Double Position (First pos and last pos)
        if Find2IdentsWithType(fIdentList.ToArray, mvfRackName, mvfRackLastPos, false,
            aIdent.Data.RequestOrder, xIdent2, xIdent3) then
        begin
            // Volume
            FindIdentWithType(fIdentList.ToArray, mvfVolume, false, aIdent.Data.RequestOrder, xIdent4);
            // Substance
            FindIdentWithType(fIdentList.ToArray, mvfSubstance, false, aIdent.Data.RequestOrder, xIdent5);
            // LiqParam
            FindIdentWithType(fIdentList.ToArray, mvfLiqParam, false, aIdent.Data.RequestOrder, xIdent6);
            // Source positions
            Find3IdentsWithType(fIdentList.ToArray, mvfSourceRackName, mvfSourceRackPos, mvfSourceRackLastPos,
                false, aIdent.Data.RequestOrder, xIdent7, xIdent8, xIdent9);
        end;
        if (xIdent4 <> nil) or (xIdent5 <> nil) or (xIdent6 <> nil) or (xIdent7 <> nil) or (xIdent8 <> nil) or
            (xIdent9 <> nil) then
            EXIT(fEditFunctions.CreateDoublePosModeExtended(aOwner, aCheckValues, '', xIdent2, aIdent,
                xIdent3, xIdent7, xIdent8, xIdent9, xIdent4, xIdent5, xIdent6))
        else
            EXIT(fEditFunctions.CreateDoublePosMode(aOwner, aCheckValues, '', xIdent2, aIdent, xIdent3));

        // Single Position
        if FindIdentWithType(fIdentList.ToArray, mvfRackName, false, aIdent.Data.RequestOrder, xIdent2) then
            EXIT(fEditFunctions.CreateSinglePosMode(aOwner, aCheckValues, '', xIdent2, aIdent));
    end;

    if (aIdent.Data.DataType = mvfRackLastPos) then
    begin
        // Double Position (First pos and last pos)
        if Find2IdentsWithType(fIdentList.ToArray, mvfRackName, mvfRackPos, false, aIdent.Data.RequestOrder,
            xIdent2, xIdent3) then
        begin
            // Volume
            FindIdentWithType(fIdentList.ToArray, mvfVolume, false, aIdent.Data.RequestOrder, xIdent4);
            // Substance
            FindIdentWithType(fIdentList.ToArray, mvfSubstance, false, aIdent.Data.RequestOrder, xIdent5);
            // LiqParam
            FindIdentWithType(fIdentList.ToArray, mvfLiqParam, false, aIdent.Data.RequestOrder, xIdent6);
            // Source positions
            Find3IdentsWithType(fIdentList.ToArray, mvfSourceRackName, mvfSourceRackPos, mvfSourceRackLastPos,
                false, aIdent.Data.RequestOrder, xIdent7, xIdent8, xIdent9);
        end;
        if (xIdent4 <> nil) or (xIdent5 <> nil) or (xIdent6 <> nil) or (xIdent7 <> nil) or (xIdent8 <> nil) or
            (xIdent9 <> nil) then
            EXIT(fEditFunctions.CreateDoublePosModeExtended(aOwner, aCheckValues, '', xIdent2, xIdent3,
                aIdent, xIdent7, xIdent8, xIdent9, xIdent4, xIdent5, xIdent6))
        else
            EXIT(fEditFunctions.CreateDoublePosMode(aOwner, aCheckValues, '', xIdent2, xIdent3, aIdent));
    end;

    if (aIdent.Data.DataType = mvfVolume) or (aIdent.Data.DataType = mvfSubstance) or
        (aIdent.Data.DataType = mvfLiqParam) then
    begin
        // Double Position (First pos and last pos) and Volume
        if Find3IdentsWithType(fIdentList.ToArray, mvfRackName, mvfRackPos, mvfRackLastPos, false,
            aIdent.Data.RequestOrder, xIdent1, xIdent2, xIdent3) then
        begin
            // Volume
            FindIdentWithType(fIdentList.ToArray, mvfVolume, false, aIdent.Data.RequestOrder, xIdent4);
            // Substance
            FindIdentWithType(fIdentList.ToArray, mvfSubstance, false, aIdent.Data.RequestOrder, xIdent5);
            // LiqParam
            FindIdentWithType(fIdentList.ToArray, mvfLiqParam, false, aIdent.Data.RequestOrder, xIdent6);
            // Source positions
            Find3IdentsWithType(fIdentList.ToArray, mvfSourceRackName, mvfSourceRackPos, mvfSourceRackLastPos,
                false, aIdent.Data.RequestOrder, xIdent7, xIdent8, xIdent9);
        end;
        if (xIdent4 <> nil) or (xIdent5 <> nil) or (xIdent6 <> nil) or (xIdent7 <> nil) or (xIdent8 <> nil) or
            (xIdent9 <> nil) then
            EXIT(fEditFunctions.CreateDoublePosModeExtended(aOwner, aCheckValues, '', xIdent1, xIdent2,
                xIdent3, xIdent7, xIdent8, xIdent9, xIdent4, xIdent5, xIdent6));

        // Single Position and Volume
        if Find2IdentsWithType(fIdentList.ToArray, mvfRackName, mvfRackPos, false, aIdent.Data.RequestOrder,
            xIdent1, xIdent2) then
        begin
            // Volume
            FindIdentWithType(fIdentList.ToArray, mvfVolume, false, aIdent.Data.RequestOrder, xIdent4);
            // Substance
            FindIdentWithType(fIdentList.ToArray, mvfSubstance, false, aIdent.Data.RequestOrder, xIdent5);
            // LiqParam
            FindIdentWithType(fIdentList.ToArray, mvfLiqParam, false, aIdent.Data.RequestOrder, xIdent6);
            // Source positions
            Find3IdentsWithType(fIdentList.ToArray, mvfSourceRackName, mvfSourceRackPos, mvfSourceRackLastPos,
                false, aIdent.Data.RequestOrder, xIdent7, xIdent8, xIdent9);
        end;
        if (xIdent4 <> nil) or (xIdent5 <> nil) or (xIdent6 <> nil) or (xIdent7 <> nil) or (xIdent8 <> nil) or
            (xIdent9 <> nil) then
            EXIT(fEditFunctions.CreateDoublePosModeExtended(aOwner, aCheckValues, '', xIdent1, xIdent2,
                xIdent3, xIdent7, xIdent8, xIdent9, xIdent4, xIdent5, xIdent6))
    end;

    EXIT(nil);
end;

function TParserValueRequest.ShowParameterDialogCasted(aIdent: TParserStoredIdent): boolean;
var
    xValue: string;
    xPage: TMultiDialogPage;
    xMainForm: TfrmMultiPageDialog;
begin
    xValue := aIdent.Value;

    if (aIdent.Data.DataType = mvfPathName) then
    begin
        result := TBasicEditFunctions.EditPathName(xValue, aIdent.name);
        if result then
            aIdent.Value := xValue;
        EXIT;
    end;

    if (aIdent.Data.DataType = mvfFileName) then
    begin
        result := TBasicEditFunctions.EditFileName(xValue);
        if result then
            aIdent.Value := xValue;
        EXIT;
    end;

    // SinglePageDialog zeigen
    xMainForm := TfrmMultiPageDialog.Create(nil);
    try
        xPage := CreateSpecificDialogModal(xMainForm, false, aIdent);
        if Assigned(xPage) then
            EXIT(fEditFunctions.ShowModalSinglePage(xMainForm, '', xPage))
        else
            EXIT(false);
    finally
        FreeAndNil(xMainForm);
    end;
end;

class function TParserValueRequest.AllValuesAreArrays(const aIdents: TArray<TParserStoredIdent>;
    out oArrayLengthRefToOrder: integer): boolean;
var
    x: integer;
begin
    oArrayLengthRefToOrder := 0;

    for x := 0 to high(aIdents) do
    begin
        if not aIdents[x].Data.DataIsArray then
            EXIT(false);

        if (oArrayLengthRefToOrder < aIdents[x].Data.ArrayLengthRefToOrder) then
            oArrayLengthRefToOrder := aIdents[x].Data.ArrayLengthRefToOrder;
    end;

    EXIT(true);
end;

function TParserValueRequest.GetNoOfPos(aArrayLengthRefToOrder: integer): integer;
begin
    result := 0;
    if (aArrayLengthRefToOrder > 0) then
        FindArrayIdents(fIdentList.ToArray, aArrayLengthRefToOrder, true, result);
end;

function TParserValueRequest.CreateArrayParameterPage(aOwner: TComponent; aCheckValues: boolean;
    const aIdents: TArray<TParserStoredIdent>; aRequestOrder, aArrayLengthRefToOrder: integer)
    : TMultiDialogPage;
var
    x: integer;
    xEditParams, xReadOnlyParams: TArray<TParserStoredIdent>;
    xAllParams: TArray<TEditableParameterEditData>;
    xFirstEditPos: integer;
    xPosCount: integer;
begin
    xPosCount := 0;
    if (aArrayLengthRefToOrder > 0) then
    begin
        xReadOnlyParams := FindArrayIdents(fIdentList.ToArray, aArrayLengthRefToOrder, true, xPosCount);
    end
    else
        SetLength(xReadOnlyParams, 0);

    // Writable-Parameter suchen
    xEditParams := FindArrayIdents(aIdents, aRequestOrder, true, xPosCount);

    // Beide Arrays zusammenführen
    xFirstEditPos := Length(xReadOnlyParams);
    SetLength(xAllParams, xFirstEditPos + Length(xEditParams));
    for x := 0 to high(xReadOnlyParams) do
    begin
        xAllParams[x].Param := xReadOnlyParams[x];
        xAllParams[x].Enabled := false; // Als ReadOnly kennzeichnen
    end;
    for x := 0 to high(xEditParams) do
    begin
        xAllParams[xFirstEditPos + x].Param := xEditParams[x];
        xAllParams[xFirstEditPos + x].Enabled := true; // Als Writable kennzeichnen
    end;

    // RackPosArray-Dialog zeigen
    EXIT(fEditFunctions.CreateRackPosArrayPage(aOwner, aCheckValues, xAllParams, GetNoOfPos,
        aArrayLengthRefToOrder, false));
end;


end.
