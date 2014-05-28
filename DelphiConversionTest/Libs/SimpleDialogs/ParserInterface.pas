{ --------------------------------------------------------------------------------------------------
  Copyright © 2003 Zinsser Analytic GmbH - All rights reserved.
  Author       : Michael Ott (mo)
  Description  : Ex-SamPars
  --------------------------------------------------------------------------------------------------
  Datum    op  function/procedure     Änderung / Neuerung
  -------- --  -------------------    --------------------------------------------------------------
  11.01.00 mo  GetIdentifierStrValue  neu
  11.01.00 mo  SetIdentifierValue     Wenn Variablenzuordnung im Parser fehlschlägt dann wird VarTyp itString
  11.01.00 mo  ParserCheckIdentifiers Sneu : stringzuweisung an Variable ist möglich
  11.01.00 mo  ParserCompare          neu : Stringvergleich
  06.06.00 mo  ParserCheckloop        IF..ENDIF statement hizugefügt
  21.11.00 mo  FormCreate             Anpassung der Fensterhöhe an Anzahl Zeilen
  29.10.02 mo  FormCreate             Benutzt TSamInifile; Picklist wird unterstützt
  29.10.02 mo  TSamInifile            TN1301 neu
  29.10.02 mo  ParserCheckLoop        TN1301 benutzt TSaminifile
  11.03.03 wl                         TN1293.7 All SamPars-units united, hiding most parts in the implementaition
  11.03.03 wl                         TN1293.7 auf IniAccess umgestellt
  15.12.03 tbh ParserSetParamList     TN1705   Counter und Auslesen der Parameter korrigiert
  06.04.04 pk  SyncCreate             TN1714   Synchronized way of creating the form
  03.03.04 wl  TSamParsStr            TN1786   Beschränkung auf 120 Zeichen aufgehoben
  05.05.04 pk  ParserCalcExpression   TN1900   Reset LastExpression if parser raises an Exception
  12.05.04 pk  ParserCheckIdentifiers TN1914 Allow '_#' prefix for a variable
  25.05.04 pk                         TN1909.1 MASSIVE CHANGES: store/retrieve variables based on variable scope ( global or local )
  08.06.04 pk  TSamIdentEndChars      TN1974.0 Allow string to end with a close parenthesis
  24.06.04 wl                         TN2007   uses Variants (nur Delphi 6 und 7)
  04.08.04 wl  TParserSetValueForm    TN2081   GUI-Änderungen: Strings jetzt auch in deutsch, Eingabeverhalten verbessert
  04.08.04 wl  TParserSetValueForm    TN2081   GUI-Änderungen: TEdit und ComboBox statt TStringGrid
  04.08.04 wl  TParserSetValueForm    TN2081   Bei 'Picklist'<>'' wird eine ComboBox erzeugt, die Werte werden bei OK geprüft
  04.08.04 wl  TParserSetValueForm.CheckMinMaxValue  TN2081   Prüft Minimum und Maximum getrennt, es muß nicht mehr beides gesetzt sein
  05.08.04 pk  ParserCalcExpression   TN2082   calls Parser.FreeExpression befor Parser.ClearVariables
  05.08.04 pk  ParserGetValue         TN2082   when the input string is empty EXIT the function
  30.08.04 wl  TSamIdentifier         TN2097   --> ParserIdentifier.pas
  30.08.04 wl  TSymbolTable,TExtendedParser,TSamIdentList  TN2097   --> ParserWrapper.pas
  30.08.04 wl  AddButton              TN2097   Edit-Button, um Einstellungen zu ändern
  30.08.04 wl  AddButton2             TN2098   Path-Button, um einen Pfad oder eine Datei als Wert auszuwählen
  31.08.04 wl                         TN2097   Ressourcen hinzugefügt, Änderungen im Detail
  08.09.04 pk                         TN2128   Component.Name = Constant + index.  Instead of Constant + IdentName
  09.12.04 wl  TParserSetValueFormMode  TN2247.2  dieser Dialog wird kann jetzt auch beim Action-Parameter editieren aufgerufen werden
  09.12.04 wl                           TN2247.2  diverse Änderungen für neuen Modus
  02.02.05 pk                         TN2304   Method name required in all functions
  07.06.05 pk  ParserInputIdentifierValues TN2449 Calls Parser.GetIdentsNeedingInput
  07.06.05 pk  TParserSetValueForm         TN2449   usage of TParserStoredIdentifier
  29.06.05 wl  AddNewLine                  TN2465.1 Fallunterscheidung für bsmEditMethodParameter (an Parser-Änderungen angepasst)
  05.07.05 pk  ParserInputIdentifierValues TN2494   Never Get Idents which do not need input
  25.08.05 pk                              TN2573   TParserIdentPair.StoredIdent type changed
  25.08.05 pk  TParserSetValueForm         TN2573   Unneeded references to fParser removed
  31.08.06 pk  CheckPickList               TN3276   If the value is a variable do not raise error
  02.08.07 wl                              TN3817   kompletter Umbau: Ab sofort Darstellung in TreeList
  02.08.07 wl                              TN3817   kleine Verbesserungen
  15.08.07 pk  cxTreeList1                 TN3817   OptionsBehaviour.AlwaysShowEditor = true
  15.08.07 pk  ShowForm                    TN3817   Set focus to first value
  15.08.07 pk                              TN3817   Resource string for Edit button
  17.08.07 pk  cxTreeList1DataChanged      TN3817   since AlwaysShowEditor = true the OnEdited event did not fire
  17.08.07 pk  UpdateEditedParameter       TN3817   call endedit before reading value
  27.08.07 pk                              TN3788   Reference to New ParserStoredIdentifier.Pas unit
  12.10.07 wl                              TN3885   Fenster ist Sizable
  12.10.07 wl                              TN3885   Tree ist nicht mehr sichtbar
  12.10.07 wl  FormShow                    TN3885   Größe des Fensters entspricht der Anzahl der Parameter
  12.10.07 wl  cxTreeList1KeyDown          TN3885   Bessere Navigation mit TAB und RETURN
  15.10.07 wl  cxTreeList1DblClick         TN3885   Doppelklick: Markeirten Parameter bearbeiten
  16.01.08 wl                              TN3992   Formstype = fsNormal statt fsStayOnTop (führte zu Crashs)
  17.11.08 wl                              TN4310   neuer Modus: bsmInputParamAction
  26.11.08 wl                              TN4310   bsmInputParamAction: Cancel-Button ist enabled
  07.01.09 pk  ParserInputIdentifierValues TN4380.1 removed
  23.07.08 pk  EdButtonClick               TN4168   Call EndEdit again after setting Texts property
  10.08.09 wl                              TN4702   Strings werden jetzt direkt geladen
  20.08.09 wl  fStringLoader                TN4702   fStringLoader lädt Strings für Dialog-Elemente
  24.08.09 wl                               TN4702   Bugfix
  03.09.09 wl  cxTreeList1                  TN4800   Anpassungen an Treelist Version 5 (nicht kompatibel)
  04.11.09 pk                               TN4843   Replace TObjectList, TStringList, TStrings with Generics Lists and TStringArray
  12.11.09 pk                               TN4800   use EditValueChanged event instead of DataChanged
  18.11.09 pk                               TN4800   use Edited and Exit events instead of EditValueChanged
  01.04.10 pk                               TN5003.2  New EnterKeyNavigationOnly parameter
  04.12.10 pk  FormClose                    TN4998   Call DialogUtils.Messagebox directly instead of Via GUIManager
  07.05.10 pk  TParserSetValueForm.Create   TN5092   Value, DefaultValue is now an object instead of a string
  20.05.10 wl                               TN5117   neue Schriftart "Segoe UI", Schriftgröße 9
  25.07.11 wl  Create                       TN5636   im Falle einer INPUT-Action wird kein Cancel-Button gezeigt
  16.08.11 wl  Create                       TN5659   im Falle einer INPUT-Action wird auch der Schließen Button oben rechts deaktiviert
  08.09.11 wl  FormClose                    TN5684   Schleife aufgeteilt: Erst alles prüfen, dann alles zurückschreiben
  17.11.11 wl  FormShow                     TN5737   Bessere Darstellung bei Auflösung 125%
  01.03.12 wl                               TN5822   TArg statt TAttrValue
  27.03.12 wl  UserMayEditParameters        TN5833   wenn Setting 'EditVariablesInRunner' = 0, dann kein EditParameter im Runner möglich
  14.12.12 wl  ParserInputModal             TN6054   neuer Parameter: aDataCache:TMethodVariablesDataCache
  20.02.13 wl                               TN6055   MethodVariablesDataCache wird nicht mehr benötigt
  22.02.13 wl  FullUpdate                   TN6094   vor dem Aufbau der Anzeige werden die Parameter neu sortiert
  22.04.13 wl                               TN6095   für MultiPageDialog geändert
  08.08.13 wl                               TN6095   Steurung mit TAB und RETURN wiederhergestellt
  09.08.13 wl  cxTreeList1KeyDown           TN6211   mit Shift+Tab geht man zurück (wenn nicht EnterKeyNavigationOnly)
  -------------------------------------------------------------------------------------------------- }

unit ParserInterface;


interface


uses
    Windows,
    Forms,
    Classes,
    ExtCtrls,
    StdCtrls,
    Controls,
    Menus,
    cxStyles,
    cxCustomData,
    cxGraphics,
    cxFilter,
    cxData,
    cxDataStorage,
    cxEdit,
    cxClasses,
    cxControls,
    cxEditRepositoryItems,
    cxExtEditRepositoryItems,
    cxTL,
    cxTextEdit,
    cxInplaceContainer,
    cxDropDownEdit,
    cxButtonEdit,
    cxLookAndFeels,
    cxLookAndFeelPainters,

    StringLoader,
    MultiPageDialog,
    EditableParameter;

type
    TParserInterfaceStringLoader = class(TTextListStringLoader)
    protected
        procedure AddAllItems(); override;
    end;

    TParameterShowDialogEvent = function(aIdent: TEditableParameter): boolean of object;

    TParserSetValueForm = class(TMultiDialogPage)
        Panel2: TPanel;
        lblHeaderTitle: TLabel;
        Label1: TLabel;
        cxEditRepository1: TcxEditRepository;
        cxTreeList1: TcxTreeList;
        cxTreeList1cxTreeListColumn1: TcxTreeListColumn;
        cxTreeList1cxTreeListColumn2: TcxTreeListColumn;
        cxTreeList1cxTreeListColumn3: TcxTreeListColumn;
        cxTreeList1cxTreeListColumn4: TcxTreeListColumn;
        procedure cxTreeList1cxTreeListColumn2GetEditingProperties(Sender: TcxTreeListColumn;
            ANode: TcxTreeListNode; var EditProperties: TcxCustomEditProperties);
        procedure cxTreeList1KeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
        procedure cxTreeList1Edited(Sender: TcxCustomTreeList; AColumn: TcxTreeListColumn);
        procedure cxTreeList1Exit(Sender: TObject);
    private const
        INT_COL_KEY = 0;
        INT_COL_VALUE = 1;
        INT_COL_RepositoryIndex = 3;
    private
        fCheckValues: boolean;
        fEnterKeyNavigationOnly: boolean;
        fStringLoader: TParserInterfaceStringLoader;
        fOnParameterShowDialog: TParameterShowDialogEvent;
        //
        function GetFirstNode(): TcxTreeListNode;
        function AddEditRepositoryItem(aParam: TEditableParameter): integer;
        function GetFocusedNodeAndParam(out oNode: TcxTreeListNode; out oParam: TEditableParameter): boolean;
        function GetParameterFromNode(aNode: TcxTreeListNode): TEditableParameter;
        function GetFocusedParam(out oParam: TEditableParameter): boolean;
        function GetRepIndex(aNode: TcxTreeListNode): integer;
        procedure UpdateEditedParameter(aNode: TcxTreeListNode);
        procedure RefreshNodeValue(aNode: TcxTreeListNode);

        // repository event handlers
        procedure EdButtonClick(aSender: TObject);
        procedure EdButtonClick2(aSender: TObject; aButtonIndex: Integer);
        procedure RefreshPickList(aSender: TObject);
    public
        constructor Create(aOwner: TComponent; const aMethodName: string;
            const aEnterKeyNavigationOnly: boolean; aOnParameterShowDialog: TParameterShowDialogEvent;
            aCheckValues: boolean); reintroduce;
        destructor Destroy; override;

        procedure AddIdentifier(aIdent: TEditableParameter);
        procedure AddIdentifiers(aIdents: TArray<TEditableParameter>);

        procedure FirstSetFocus(); override;
        procedure RefreshPageData(); override;
        function WritePageData(aCheckBefore: boolean): boolean; override;
    end;


implementation


{$R *.DFM}

uses
    SysUtils,
    DialogUtils,
    Utility_DevExpress,
    GeneralTypes,
    MethodVariableTypes,
    ControlUtils;

{ TParserInterfaceStringLoader }

procedure TParserInterfaceStringLoader.AddAllItems;
begin
    AddSingle(510, '&OK', '&OK');
    AddSingle(520, '&Cancel', '&Abbrechen');
    AddSingle(41500, 'Method Parameters', 'Methodenparameter');
    AddSingle(41520, 'Please enter the necessary parameter values',
        'Bitte geben Sie die notwendigen Parameter ein');
end;

{ TParserSetValueForm }

constructor TParserSetValueForm.Create(aOwner: TComponent; const aMethodName: string;
    const aEnterKeyNavigationOnly: boolean; aOnParameterShowDialog: TParameterShowDialogEvent;
    aCheckValues: boolean);
begin
    inherited Create();

    fEnterKeyNavigationOnly := aEnterKeyNavigationOnly;
    fCheckValues := aCheckValues;

    TControlUtils.ResetFontForWinXP(self);
    fOnParameterShowDialog := aOnParameterShowDialog;

    fStringLoader := TParserInterfaceStringLoader.Create;
    fStringLoader.LoadLanguage(self);

    if (aMethodName <> '') then
        lblHeaderTitle.Caption := TLanguageString.Read('Method: ', 'Methode: ') + aMethodName;

    cxEditRepository1.Clear;
    self.cxTreeList1.Clear();
end;

procedure TParserSetValueForm.FirstSetFocus;
begin
    self.NextButtonSetDefault(false);
    self.cxTreeList1.SetFocus;
    self.cxTreeList1.SetFocusedNode(GetFirstNode, []);
    self.cxTreeList1.Select(self.cxTreeList1.FocusedNode);
    self.cxTreeList1.Columns[INT_COL_VALUE].Focused := true;
end;

function TParserSetValueForm.GetFirstNode(): TcxTreeListNode;
begin
    result := cxTreeList1.AbsoluteItems[0];
end;

function TParserSetValueForm.GetRepIndex(aNode: TcxTreeListNode): integer;
var
    xIntText: string;
begin
    result := -1;
    xIntText := aNode.Texts[INT_COL_RepositoryIndex];
    if (xIntText = '') then
        EXIT;

    result := StrToInt(aNode.Texts[INT_COL_RepositoryIndex]);
end;

procedure TParserSetValueForm.AddIdentifier(aIdent: TEditableParameter);
var
    xNode: TcxTreeListNode;
begin
    // Node neu erzeugen
    xNode := self.cxTreeList1.Add();

    // Node und Parameter verlinken
    xNode.Data := aIdent;

    // Node mit Daten füllen
    xNode.Texts[INT_COL_KEY] := aIdent.Description;
    xNode.Texts[INT_COL_VALUE] := aIdent.Value;

    // wichtig: für jeden Parameter wird ein Item in Repository angelegt!
    xNode.Texts[INT_COL_RepositoryIndex] := IntToStr(AddEditRepositoryItem(aIdent));
end;

procedure TParserSetValueForm.AddIdentifiers(aIdents: TArray<TEditableParameter>);
var
    x: integer;
begin
    for x := 0 to high(aIdents) do
        AddIdentifier(aIdents[x]);
end;

procedure TParserSetValueForm.RefreshNodeValue(aNode: TcxTreeListNode);
var
    xParam: TEditableParameter;
begin
    if (aNode = nil) then
        EXIT;

    xParam := GetParameterFromNode(aNode);
    if (xParam = nil) then
        EXIT;

    aNode.Texts[INT_COL_VALUE] := xParam.Value;

    // 23.07.08 pk VERY IMPORTANT to EndEdit mode after we have changed the Texts property, otherwise user does not see changes
    aNode.EndEdit(false);
end;

function TParserSetValueForm.WritePageData(aCheckBefore: boolean): boolean;
var
    x: integer;
    xParameter: TEditableParameter;
    xNode: TcxTreeListNode;
begin
    // Editieren beenden
    if Assigned(cxTreeList1.FocusedNode) then
        UpdateEditedParameter(cxTreeList1.FocusedNode);

    // alle Werte überprüfen
    if aCheckBefore and fCheckValues then
    begin
        xNode := nil;
        try
            for x := 0 to cxTreeList1.AbsoluteCount - 1 do
            begin
                xNode := cxTreeList1.AbsoluteItems[x];
                xParameter := GetParameterFromNode(xNode);
                xParameter.CheckValue(); // hier können Exceptions hochkommen
            end;
        except
            on E: ESetEditableParameterException do
            begin
                TDialogUtils.MessageBox(E.Message, 'Property is not valid', MB_ICONSTOP);
                if Assigned(xNode) then
                    xNode.Focused := true;
                cxTreeList1.Columns[INT_COL_VALUE].Focused := true;
                EXIT(false);
            end
            else
                raise;
        end;
    end;

    EXIT(true);
end;

function TParserSetValueForm.AddEditRepositoryItem(aParam: TEditableParameter): integer;
var
    xStrings: TStringArray;
    xItem: TcxEditRepositoryItem;
begin
    xStrings := aParam.GetPickList();
    // etwas umständlich, aber wie soll es sonst gehen

    if (Length(xStrings) > 0) then
    begin
        if aParam.HasEditFunction() then
        begin
            xItem := cxEditRepository1.CreateItem(TcxEditRepositoryComboButtonItem);
            TcxUtils.AddValuesToComboBoxProps(xStrings, (xItem as TcxEditRepositoryComboButtonItem)
                .Properties);
            (xItem as TcxEditRepositoryComboButtonItem).Properties.OnButtonClick := EdButtonClick;
            result := cxEditRepository1.Count - 1;
        end
        else
        begin
            xItem := cxEditRepository1.CreateItem(TcxEditRepositoryComboBoxItem);
            (xItem as TcxEditRepositoryComboBoxItem).Properties.OnInitPopup := RefreshPickList;
            TcxUtils.AddValuesToComboBoxProps(xStrings, (xItem as TcxEditRepositoryComboBoxItem).Properties);
            result := cxEditRepository1.Count - 1;
        end;
    end
    else
    begin
        if aParam.HasEditFunction() then
        begin
            xItem := cxEditRepository1.CreateItem(TcxEditRepositoryButtonItem);
            (xItem as TcxEditRepositoryButtonItem).Properties.OnButtonClick := EdButtonClick2;
            result := cxEditRepository1.Count - 1;
        end
        else
        begin
            cxEditRepository1.CreateItem(TcxEditRepositoryTextItem);
            result := cxEditRepository1.Count - 1;
        end;
    end;
end;

function TParserSetValueForm.GetParameterFromNode(aNode: TcxTreeListNode): TEditableParameter;
begin
    EXIT(TEditableParameter(aNode.Data));
end;

function TParserSetValueForm.GetFocusedNodeAndParam(out oNode: TcxTreeListNode;
    out oParam: TEditableParameter): boolean;
// returns true if vParam is assigned
begin
    result := false;
    oNode := cxTreeList1.FocusedNode;
    ASSERT(Assigned(oNode), 'FocusedNode is nil');
    oParam := self.GetParameterFromNode(oNode);
    if not Assigned(oParam) then
        EXIT;
    result := true;
end;

procedure TParserSetValueForm.EdButtonClick(aSender: TObject);
var
    xParameter: TEditableParameter;
    xNode: TcxTreeListNode;
begin
    if not GetFocusedNodeAndParam(xNode, xParameter) then
        EXIT;

    // Value des Editierten Parameters aktualisieren
    UpdateEditedParameter(xNode);

    // Dialog zeigen
    if Assigned(fOnParameterShowDialog) then
        fOnParameterShowDialog(xParameter);

    // Der fokussierte Parameter muss als erstes aktualisiert werden, sonst geht der Wert verloren
    RefreshNodeValue(xNode);

    // Alle anderen Werte nachtragen
    RefreshPageData;
end;

procedure TParserSetValueForm.EdButtonClick2(aSender: TObject; aButtonIndex: Integer);
begin
    self.EdButtonClick(aSender);
end;

function TParserSetValueForm.GetFocusedParam(out oParam: TEditableParameter): boolean;
var
    xDummy: TcxTreeListNode;
begin
    result := GetFocusedNodeAndParam(xDummy, oParam);
end;

procedure TParserSetValueForm.UpdateEditedParameter(aNode: TcxTreeListNode);
var
    xParam: TEditableParameter;
begin
    xParam := self.GetParameterFromNode(aNode);
    if not Assigned(xParam) then
        EXIT;

    xParam.Value := aNode.Texts[INT_COL_VALUE];
    aNode.EndEdit(false);
end;

procedure TParserSetValueForm.RefreshPageData;
var
    x: integer;
begin
    for x := 0 to cxTreeList1.AbsoluteCount - 1 do
    begin
        self.RefreshNodeValue(cxTreeList1.AbsoluteItems[x]);
    end;
end;

procedure TParserSetValueForm.RefreshPickList(aSender: TObject);
var
    xParameter: TEditableParameter;
    xList: TStrings;
    x: integer;
    xValues: TStringArray;
begin
    if not GetFocusedParam(xParameter) then
        EXIT;

    xList := nil;
    if aSender is TcxComboBox then
        xList := (aSender as TcxComboBox).Properties.Items;

    if not Assigned(xList) then
        EXIT;

    xList.Clear;
    xValues := xParameter.GetPickList();
    for x := 0 to high(xValues) do
        xList.Add(xValues[x]);

end;

procedure TParserSetValueForm.cxTreeList1cxTreeListColumn2GetEditingProperties(Sender: TcxTreeListColumn;
    ANode: TcxTreeListNode; var EditProperties: TcxCustomEditProperties);
var
    xRepIndex: integer;
begin
    xRepIndex := GetRepIndex(aNode);
    if (xRepIndex < 0) then
        EXIT;

    EditProperties := cxEditRepository1.Items[xRepIndex].Properties;
end;

procedure TParserSetValueForm.cxTreeList1KeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
var
    xNextKey, xEnterKey: word;
begin
    xNextKey := VK_TAB;
    xEnterKey := VK_RETURN;

    if fEnterKeyNavigationOnly then
    begin
        xNextKey := VK_RETURN;
        xEnterKey := 0;
    end;

    if (Key = xEnterKey) then
        DoExitPage(Sender);

    if (Key = xNextKey) then
    begin
        if Assigned(cxTreeList1.FocusedNode) then
        begin
            if (ssShift in Shift) and (Key = VK_TAB) then
            begin // Shift+Tab: Gehe rückwärts
                if (cxTreeList1.FocusedNode.Index > 0) then
                    cxTreeList1.FocusedNode := cxTreeList1.AbsoluteItems[cxTreeList1.FocusedNode.Index - 1]
            end
            else
            begin
                if (cxTreeList1.FocusedNode.Index + 1 < cxTreeList1.AbsoluteCount) then
                    cxTreeList1.FocusedNode := cxTreeList1.AbsoluteItems[cxTreeList1.FocusedNode.Index + 1]
                else
                    DoExitPage(Sender);
            end;
        end;
    end;
end;

destructor TParserSetValueForm.Destroy;
begin
    fStringLoader.Free;
    inherited;
end;

procedure TParserSetValueForm.cxTreeList1Edited(Sender: TcxCustomTreeList; AColumn: TcxTreeListColumn);
begin
    if not Assigned(cxTreeList1.FocusedNode) then
        EXIT;
    UpdateEditedParameter(cxTreeList1.FocusedNode);
end;

procedure TParserSetValueForm.cxTreeList1Exit(Sender: TObject);
begin
    // we need this because edited event does not get called when OK/Cancel button is clicked
    if not Assigned(cxTreeList1.FocusedNode) then
        EXIT;
    UpdateEditedParameter(cxTreeList1.FocusedNode);
end;


end.
