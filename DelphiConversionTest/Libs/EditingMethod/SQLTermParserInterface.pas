{ --------------------------------------------------------------------------------------------------
  Copyright © 2007 Zinsser Analytic GmbH - All rights reserved.
  Author       : Wolfgang Lyncke (wl)
  Description  : User Interface for input of SQL parameter values
  --------------------------------------------------------------------------------------------------
  Revision History:
  date     op  method                       track-no improvement/change
  -------- --  ---------------------------  -------- -----------------------------------------------
  16.04.07 wl                                TN3547  Initial Revision
  16.04.07 wl  gmParseSQLStrAtRun            TN3547  von SQLResult hierher
  07.05.07 wl                                TN3547  unbenutzte Funktionen auskommentiert
  15.10.08 pk  gmEditSQLTermParameters       TN4258  Now returns boolean
  06.07.09 pk                                TN4585.4  Some code moved to SQLTermParser.pas
  10.08.09 wl                                TN4702   Strings werden jetzt direkt geladen
  20.08.09 wl  fStringLoader                 TN4702   fStringLoader lädt Strings für Dialog-Elemente
  04.11.09 pk                                TN4843   Replace TObjectList, TStringList, TStrings with Generics Lists and TStringArray
  16.11.09 pk  gmSQLTermPrepareBeforeExecute TN4843   bug fixed
  13.04.10 wl                                TN5044   uses StringUtilities
  20.05.10 wl                                TN5117   neue Schriftart "Segoe UI", Schriftgröße 9
  21.02.13 wl                                TN6045   uses geändert
  -------------------------------------------------------------------------------------------------- }

unit SQLTermParserInterface;


interface


uses
    Forms,
    Classes,
    ExtCtrls,
    StdCtrls,
    Controls,
    ThreadUtils,
    SQLTermParser,
    StringLoader;

type
    TSQLTermParamsStringLoader = class(TTextListStringLoader)
    protected
        procedure AddAllItems(); override;
    end;

    TfrmSQLTermParams = class(TForm)
        Panel1: TPanel;
        Panel2: TPanel;
        lblHeaderTitle: TLabel;
        Bevel1: TBevel;
        btnOK: TButton;
        btnCancel: TButton;
        Label1: TLabel;
        Panel3: TPanel;
        procedure FormClose(Sender: TObject; var Action: TCloseAction);
        procedure FormShow(Sender: TObject);
    private
        fIdentList: TSQLParserIdentPairList;
        fScopeName: string;
        fStringLoader: TSQLTermParamsStringLoader;
        //
        class procedure CheckEmpty(aText, aIdentName: string);
        class procedure CheckPickList(aComboBox: TComboBox; aIdentName: string);
        class procedure CheckMinMaxValue(aValAsStr: string; aIdent: TSQLParserStoredIdent);
        class function CreateDescrLabel1Text(aIdent: TSQLParserStoredIdent): string;
        class function CreateDescrLabel2Text(aIdent: TSQLParserStoredIdent): string;
        //
        function FindIdentComponent(aIndex: integer; aKind: TSQLParserSetValComponent): TComponent;
        function AddEditControl(aIndex: integer; aIdentName, aPickList, aDefValue: string): TWinControl;
        function AddLabel(aIndex: integer; aIdentName, aCaption: string): TLabel;
        // function AddButton( aIndex: integer; aIdentName: string ): TButton;
        function AddLabel2(aIndex: integer; aIdentName, aCaption: string): TLabel;
        procedure AddNewLine(aIndex: integer; aStoredIdent: TSQLParserStoredIdent;
            aIdent: TSQLParserIdentifier);
        function GetValueFromComboBox(aComboBox: TComboBox; aIdent: TSQLParserStoredIdent): string;
        function GetValueFromEdit(aEdit: TEdit; aIdent: TSQLParserStoredIdent): string;
        // procedure EditButtonClick(Sender: TObject);
        // procedure PathButtonClick(Sender: TObject);
        // procedure FileButtonClick(Sender: TObject);
        function GetNextTop(aIndex: integer): integer;
    public
        constructor Create(const aScopeName: string; aIdentsNeedingInput: TSQLParserIdentPairList);
            reintroduce;
        destructor Destroy; override;
        //
        class function VCL(const aArgs: TMessageArg): TMessageResult;
        class function SyncParserInputModal(const aScopeName: string;
            aIdentsNeedingInput: TSQLParserIdentPairList): TModalResult;
        class function ParserInputModal(const aScopeName: string;
            aIdentsNeedingInput: TSQLParserIdentPairList): TModalResult;

    end;

procedure gmSQLTermPrepareBeforeExecute(const aSQLTermName: string; aSQLStrings: TStrings;
    const aParamsString: string);
function gmEditSQLTermParameters(const aSQLTermName: string; var vParamsString: string): boolean;


implementation


uses
    Variants,
    SysUtils,
    GeneralTypes,
    StringUtilities,
    SQLTermsDataAdaptor,
    ControlUtils;

{$R *.DFM}

const
    // unwichtig:
    STR_COMPONENT_EDIT = 'Contr';
    STR_COMPONENT_LABEL_1 = 'Lbl1_';
    STR_COMPONENT_LABEL_2 = 'Lbl2_';
    STR_COMPONENT_BUTTON_EDIT = 'Btn1_';
    STR_COMPONENT_BUTTON_PATH = 'Btn2_';

    { TSQLTermParamsStringLoader }

procedure TSQLTermParamsStringLoader.AddAllItems;
begin
    AddSingle(510, '&OK', '&OK');
    AddSingle(520, '&Cancel', '&Abbrechen');
    AddSingle(41500, 'SQL Parameters', 'SQL-parameter');
    AddSingle(41520, 'Please enter the necessary parameter values',
        'Bitte geben Sie die notwendigen Parameter ein');
end;

{ TfrmSQLTermParams }

class function TfrmSQLTermParams.ParserInputModal(const aScopeName: string;
    aIdentsNeedingInput: TSQLParserIdentPairList): TModalResult;
var
    xForm: TfrmSQLTermParams;
    xScreenCursor: TCursor;
begin
    xForm := TfrmSQLTermParams.Create(aScopeName, aIdentsNeedingInput);
    xScreenCursor := Screen.Cursor;
    Screen.Cursor := crDefault;
    try
        result := xForm.ShowModal;
    finally
        Screen.Cursor := xScreenCursor;
    end;
end;

constructor TfrmSQLTermParams.Create(const aScopeName: string; aIdentsNeedingInput: TSQLParserIdentPairList);
begin
    inherited Create(Application);

    TControlUtils.ResetFontForWinXP(self);
    fScopeName := aScopeName;
    fIdentList := aIdentsNeedingInput;

    if (fScopeName <> '') then
        lblHeaderTitle.Caption := TLanguageString.Read('Method: ', 'Methode: ') + fScopeName;

    fStringLoader := TSQLTermParamsStringLoader.Create;
    fStringLoader.LoadLanguage(self);
end;

destructor TfrmSQLTermParams.Destroy;
begin
    fStringLoader.Free;
    inherited;
end;

procedure TfrmSQLTermParams.FormShow(Sender: TObject);
var
    x: integer;
    xControl: TComponent;
    xStoredIdent: TSQLParserStoredIdent;
begin
    for x := 0 to fIdentList.Count - 1 do
    begin
        xStoredIdent := fIdentList.Items[x].StoredIdent;
        AddNewLine(x, xStoredIdent, fIdentList.Items[x].Ident);
    end;

    if (FIdentList.Count <= 0) then
        EXIT;

    // set the focus to the first edit control
    xControl := FindIdentComponent(0, pscEdit);
    if (xControl is TWinControl) then
        (xControl as TWinControl).SetFocus;

    if (self.Height < (GetNextTop(FIdentList.Count) + 130)) then
        Height := GetNextTop(FIdentList.Count) + 130;
end;

function TfrmSQLTermParams.AddLabel(aIndex: integer; aIdentName, aCaption: string): TLabel;
begin
    result := TLabel.Create(self);
    result.Parent := self.Panel3;
    result.Name := STR_COMPONENT_LABEL_1 + IntToStr(aIndex);
    result.Left := 2;
    result.Top := GetNextTop(aIndex) + 2;
    result.Caption := aCaption;
    result.AutoSize := false;
    result.Width := 330;
    result.Alignment := taRightJustify;
end;

function TfrmSQLTermParams.AddLabel2(aIndex: integer; aIdentName, aCaption: string): TLabel;
begin
    result := TLabel.Create(self);
    result.Parent := self.Panel3;
    result.Name := STR_COMPONENT_LABEL_2 + IntToStr(aIndex);
    result.Left := 532;
    result.Top := GetNextTop(aIndex) + 2;
    result.Caption := aCaption;
end;

{ function TfrmSQLTermParams.AddButton( aIndex: integer; aIdentName: string ): TButton;
  begin
  result := TButton.Create(self);
  result.Parent := self.Panel3;
  result.Name := STR_COMPONENT_BUTTON_EDIT + IntToStr( aIndex );
  result.SetBounds(610, GetNextTop( aIndex ), 65, 21);
  result.Caption := TResLoader.GetResString(41580{Edit }{ );
  result.OnClick := EditButtonClick;
  end; }

function TfrmSQLTermParams.FindIdentComponent(aIndex: integer; aKind: TSQLParserSetValComponent): TComponent;
var
    xIndexAsStr: string;
begin
    xIndexAsStr := IntToStr(aIndex);
    case (aKind) of
        pscLabel1:
            result := self.FindComponent(STR_COMPONENT_LABEL_1 + xIndexAsStr);
        pscLabel2:
            result := self.FindComponent(STR_COMPONENT_LABEL_2 + xIndexAsStr);
        pscEdit:
            result := self.FindComponent(STR_COMPONENT_EDIT + xIndexAsStr);
        pscButtonEdit:
            result := self.FindComponent(STR_COMPONENT_BUTTON_EDIT + xIndexAsStr);
        pscButtonPath:
            result := self.FindComponent(STR_COMPONENT_BUTTON_PATH + xIndexAsStr);
        else
            result := nil;
    end;
end;

{ procedure TfrmSQLTermParams.EditButtonClick(Sender: TObject);
  var
  xButton: TButton;
  xIdentifier: TSQLParserStoredIdent;
  xComp: TComponent;
  xIndex: integer;
  xIndexAsStr : string;
  begin
  if not (Sender is TButton) then EXIT;

  xButton := Sender as TButton;

  xIndexAsStr :=  CopyTillEnd( xButton.Name, Length( STR_COMPONENT_BUTTON_EDIT ) + 1 );
  xIndex := StrToInt( xIndexAsStr );
  ASSERT( xIndex < fIdentList.Count );

  xIdentifier := fIdentList.Items[ xIndex ].StoredIdent;

  if (TfrmParserEditIdent.EditIdent(xIdentifier)) then begin

  xComp := FindIdentComponent( xIndex, pscLabel1);
  if (xComp is TLabel) then
  (xComp as TLabel).Caption := CreateDescrLabel1Text( xIdentifier );
  xComp := FindIdentComponent( xIndex, pscLabel2);
  if (xComp is TLabel) then
  (xComp as TLabel).Caption := CreateDescrLabel2Text( xIdentifier );

  AddButton2( xIndex, xIdentifier.Name, xIdentifier.ValueFormat);
  AddEditControl( xIndex, xIdentifier.Name, xIdentifier.PickList, xIdentifier.DefValue);
  end;
  end;


  procedure TfrmSQLTermParams.PathButtonClick(Sender: TObject);
  var xDir, xIdentName: string;
  xButton: TButton;
  xComp: TComponent;
  begin
  if not (Sender is TButton) then EXIT;

  xButton := Sender as TButton;
  xIdentName := Copy(xButton.Name, 6, Length(xButton.Name));

  SelectDirectory(TResLoader.GetResFString(41570{choose directory for parameter %s: }{ , [xIdentName]), '', xDir);
  if (xDir = '') then EXIT;

  xComp := self.FindComponent(STR_COMPONENT_EDIT + xIdentName);
  if (xComp is TEdit) then (xComp as TEdit).Text := xDir;
  if (xComp is TComboBox) then (xComp as TComboBox).Text := xDir;
  end;


  procedure TfrmSQLTermParams.FileButtonClick(Sender: TObject);
  var xIdentName: string;
  xButton: TButton;
  xComp: TComponent;
  xOpenDialog: TOpenDialog;
  begin
  if not (Sender is TButton) then EXIT;

  xButton := Sender as TButton;
  xIdentName := Copy(xButton.Name, 6, Length(xButton.Name));

  xOpenDialog := TOpenDialog.Create(nil);
  xOpenDialog.Execute;
  if (xOpenDialog.FileName = '') then EXIT;

  xComp := self.FindComponent(STR_COMPONENT_EDIT + xIdentName);
  if (xComp is TEdit) then (xComp as TEdit).Text := xOpenDialog.FileName;
  if (xComp is TComboBox) then (xComp as TComboBox).Text := xOpenDialog.FileName;

  xOpenDialog.Free;
  end;
}

function TfrmSQLTermParams.AddEditControl(aIndex: integer; aIdentName, aPickList, aDefValue: string)
    : TWinControl;
var
    xComp: TComponent;
    xIndexAsStr: string;
begin
    xIndexAsStr := IntToStr(aIndex);
    xComp := self.FindComponent(STR_COMPONENT_EDIT + xIndexAsStr);
    FreeAndNil(xComp);

    result := TEdit.Create(self);
    result.Parent := self.Panel3;
    result.Name := STR_COMPONENT_EDIT + xIndexAsStr;
    (result as TEdit).Text := aDefValue;

    result.Left := 350;
    result.Top := GetNextTop(aIndex);
    result.Width := 170;
    result.Hint := aIdentName;
    result.ShowHint := true;
    result.TabOrder := aIndex;
end;

procedure TfrmSQLTermParams.AddNewLine(aIndex: integer; aStoredIdent: TSQLParserStoredIdent;
    aIdent: TSQLParserIdentifier);
var
    xValue: string;
begin

    // aIdent.LoadProperties;
    AddLabel(aIndex, aStoredIdent.Name, CreateDescrLabel1Text(aStoredIdent));
    AddLabel2(aIndex, aStoredIdent.Name, CreateDescrLabel2Text(aStoredIdent));

    xValue := '';

    // nur beim Editieren von Submethoden: Wert einsetzen!
    xValue := aIdent.Value;
    if (xValue = '') then
        xValue := aStoredIdent.DefValue;

    AddEditControl(aIndex, aStoredIdent.Name, '', xValue);

    // if gCommonDll.CurrentUser.HasLevel( usrSystemAdmin ) then
    // AddButton( aIndex, aStoredIdent.Name );

    // xIdent.Free;
end;

class procedure TfrmSQLTermParams.CheckMinMaxValue(aValAsStr: string; aIdent: TSQLParserStoredIdent);
var
    MinVal, MaxVal, value: Variant;
    xSettingsMinVal, xSettingsMaxVal: string;
begin
    xSettingsMinVal := ''; // aIdent.MinValue;
    xSettingsMaxVal := ''; // aIdent.MaxValue;

    if (xSettingsMinVal <> '') then
    begin
        try
            value := StrToFloat(aValAsStr);
            MinVal := StrToFloat(xSettingsMinVal);
        except
            value := aValAsStr;
            MinVal := xSettingsMinVal;
        end;
        if (value < MinVal) then
            raise Exception.Create(TLanguageString.
                Read('Parameter {0} value [{1}] is smaller than minimum value [{2}].',
                'Der Wert für Parameter {0} [{1}] ist kleiner als der Minimalwert [{2}].',
                [aIdent.Name, aValAsStr, xSettingsMinVal]));
    end;

    if (xSettingsMaxVal <> '') then
    begin
        try
            value := StrToFloat(aValAsStr);
            MaxVal := StrToFloat(xSettingsMaxVal);
        except
            value := aValAsStr;
            MaxVal := xSettingsMaxVal;
        end;
        if (value > MaxVal) then
            raise Exception.Create(TLanguageString.
                Read('Parameter {0} value [{1}] is bigger than maximum value [{2}].',
                'Der Wert für Parameter {0} [{1}] ist größer als der Maximalwert [{2}].',
                [aIdent.Name, aValAsStr, xSettingsMaxVal]));
    end;
end;

class procedure TfrmSQLTermParams.CheckPickList(aComboBox: TComboBox; aIdentName: string);
var
    x: integer;
begin
    // if TSQLParserIdentifier.GetIdentType( aIdentName ) <> itNone then EXIT;

    for x := 0 to aCombobox.Items.Count - 1 do
        if (aComboBox.Text = aCombobox.Items[x]) then
            EXIT;

    raise Exception.Create(TLanguageString.Read('Parameter {0} value [{1}] is not member of the pick list.',
        'Der Wert für Parameter {0} [{1}] ist nicht in der Auswahlliste enthalten.',
        [aIdentName, aComboBox.Text]));
end;

class procedure TfrmSQLTermParams.CheckEmpty(aText: string; aIdentName: string);
begin
    if (aText = '') then
        raise Exception.Create(TLanguageString.Read('Parameter {0} value is empty.',
            'Der Wert für Parameter {0} ist leer.', [aIdentName]));
end;

function TfrmSQLTermParams.GetValueFromEdit(aEdit: TEdit; aIdent: TSQLParserStoredIdent): string;
begin
    result := '';

    try
        if (aEdit.Text = '') then
            EXIT; // Parameter edit: '' ist erlaubt!
        CheckEmpty(aEdit.Text, aIdent.Name);
        CheckMinMaxValue(aEdit.Text, aIdent);
        result := aEdit.Text;
    except
        on E: Exception do
        begin
            aEdit.SetFocus;
            raise;
        end;
    end;
end;

function TfrmSQLTermParams.GetValueFromComboBox(aComboBox: TComboBox; aIdent: TSQLParserStoredIdent): string;
begin
    result := '';

    try
        if (aComboBox.Text = '') then
            EXIT; // Parameter edit: '' ist erlaubt!
        CheckMinMaxValue(aComboBox.Text, aIdent);
        CheckPickList(aComboBox, aIdent.Name);
        result := aComboBox.Text;
    except
        on E: Exception do
        begin
            aComboBox.SetFocus;
            raise;
        end;
    end;
end;

procedure TfrmSQLTermParams.FormClose(Sender: TObject; var Action: TCloseAction);
var
    x: integer;
    xValue: string;
    xComp: TComponent;
    xStoredIdent: TSQLParserStoredIdent;
begin
    Action := caFree;
    if (self.ModalResult <> mrOK) then
        exit;

    for x := 0 to FIdentList.Count - 1 do
    begin

        xValue := '';

        xStoredIdent := fIdentList.Items[x].StoredIdent;

        xComp := FindIdentComponent(x, pscEdit);

        if (xComp is TEdit) then
            xValue := GetValueFromEdit(xComp as TEdit, xStoredIdent);

        if (xComp is TComboBox) then
            xValue := GetValueFromComboBox(xComp as TComboBox, xStoredIdent);

        // Wert der Variablen zuweisen
        fIdentList.Items[x].Ident.Value := xValue;
    end;
end;

class function TfrmSQLTermParams.SyncParserInputModal(const aScopeName: string;
    aIdentsNeedingInput: TSQLParserIdentPairList): TModalResult;
begin
    result := ThreadUtils.gmMessageAndWait(VCL, VarArrayOf([aScopeName, LongInt(aIdentsNeedingInput)]));
end;

class function TfrmSQLTermParams.VCL(const aArgs: TMessageArg): TMessageResult;
var
    xScopeName: string;
    xIdentsNeedingInput: TSQLParserIdentPairList;
begin
    xScopeName := aArgs[0];
    xIdentsNeedingInput := TSQLParserIdentPairList(LongInt(aArgs[1]));
    result := TfrmSQLTermParams.ParserInputModal(xScopeName, xIdentsNeedingInput);
end;

class function TfrmSQLTermParams.CreateDescrLabel1Text(aIdent: TSQLParserStoredIdent): string;
begin
    // Define Label text
    if (aIdent.Description <> '') then
        result := aIdent.Description
    else
        result := TLanguageString.Read('Parameter: {0}', 'Parameter: {0}', [aIdent.Name]);
end;

class function TfrmSQLTermParams.CreateDescrLabel2Text(aIdent: TSQLParserStoredIdent): string;
begin
    result := '';
    // if (aIdent.MinValue <> '') or (aIdent.MaxValue <> '') then
    // result := '[' + aIdent.MinValue + ' .. ' + aIdent.MaxValue + ']';
end;

function TfrmSQLTermParams.GetNextTop(aIndex: integer): integer;
begin
    result := 6 + (aIndex * 23);
end;

{
  procedure gmParseHardcodeVariables( aIdentsNeedingInput : TSQLParserIdentPairList; const aSQLTerm: string; aSQLArgs: TStrings );
  var
  x: integer;
  xParamName: string;
  xIdent: TSQLParserIdentifier;
  xStoredIdent: TSQLParserStoredIdent;
  begin
  for x := 1 to 3 do begin
  case x of
  1: xParamName := STR_SQLTERM_PARAM_CURRENTRUNNAME;
  2: xParamName := STR_SQLTERM_PARAM_RUNLAYOUTNAME;
  3: xParamName := STR_SQLTERM_PARAM_PRIORITY;
  end;

  if Pos( xParamName, UpperCase( aSQLTerm ) ) = 0 then EXIT;

  xIdent := TSQLParserIdentifier.Create( xParamName );
  xStoredIdent := TSQLParserStoredIdent.Create( xParamName );
  if ( aSQLArgs.Count > x ) then begin
  xIdent.Value := aSQLArgs[ x ];
  xStoredIdent.Value := aSQLArgs[ x ];
  end;
  aIdentsNeedingInput.AddItem( TSQLParserIdentPair.Create( xIdent, xStoredIdent ) );
  end;
  end;
}

function DirectEditSQLTermParameters(const aSQLTermName, aSQLTerm: string; const aSQLArgs: TStringArray;
    aAddHardcodeValues: boolean; out oResult: TStringArray): boolean;
// --------------------------------------------------------------------------------------------------
// zeigt EditFenster und setzt die Variablenwerte
//
// TParserIdentPairList is a list of TParserIdentPair objects
// TParserIdentPair has two fields :
// StoredIdent - Contains properties for an identifier read from the settings.
// These properties can be edited info from the ParserEditIdent Dialog box.
// This object is actually stored in the Parser.fStoredIdents list. DO NOT FREE IT HERE!
// Ident       - Contains a value.  When the TParserSetValueForm is closed Ident.Value should be set
// This object is actually stored in Parser.fSymbolTable. DO NOT FREE IT HERE!
// --------------------------------------------------------------------------------------------------
var
    xModalResult: TModalResult;
    xIdentsNeedingInput: TSQLParserIdentPairList;
    x: integer;
begin
    result := false;
    SetLength(oResult, 0);
    xIdentsNeedingInput := TSQLParserIdentPairList.Create();
    try
        TSQLTermParser.ParseSQLVariables(xIdentsNeedingInput, aSQLTerm, aSQLArgs);

        // tbd: erstmal nicht aktiviert, da ich nicht weiß, wie ich die Werte hinterher ersetzen soll
        // if ( aAddHardcodeValues ) then gmParseHardcodeVariables( xIdentsNeedingInput, aSQLTerm, aSQLArgs );

        if xIdentsNeedingInput.Count = 0 then
            EXIT;

        xModalResult := TfrmSQLTermParams.SyncParserInputModal(aSQLTermName, xIdentsNeedingInput);
        if (xModalResult = mrOK) then
        begin
            result := true;
            SetLength(oResult, xIdentsNeedingInput.Count);
            for x := 0 to xIdentsNeedingInput.Count - 1 do
            begin
                oResult[x] := xIdentsNeedingInput.Items[x].Ident.Value;
            end;
        end;
    finally
        TGarbage.FreeAndNil(true, xIdentsNeedingInput);
    end;
end;

function gmEditSQLTermParameters(const aSQLTermName: string; var vParamsString: string): boolean;
var
    xDA: TSQLTermsDataAdaptor;
    xSQLTermRec: TSQLTermRec;
    xSQLArgs: TStringArray;
    xSQLArgsEdited: TStringArray;
begin
    xDA := TSQLTermsDataAdaptor.Create;
    try
        xDA.ReadSQLTermData(aSQLTermName, xSQLTermRec)
    finally
        xDA.Free;
    end;

    xSQLArgs := TStringUtilities.StringToStringArray(vParamsString, ',');
    result := DirectEditSQLTermParameters(xSQLTermRec.Name, xSQLTermRec.Term, xSQLArgs, false,
        xSQLArgsEdited);
    if not result then
        EXIT;

    vParamsString := TStringUtilities.StringArrayToString(xSQLArgsEdited, ',');

end;

procedure gmSQLTermPrepareBeforeExecute(const aSQLTermName: string; aSQLStrings: TStrings;
    const aParamsString: string);
var
    xSQLArgs: TStringArray;
    xSQLArgsEdited: TStringArray;
    x, xIndex: integer;
begin
    xSQLArgs := TStringUtilities.StringToStringArray(aParamsString, ',');
    if not DirectEditSQLTermParameters(aSQLTermName, aSQLStrings.Text, xSQLArgs, true, xSQLArgsEdited) then
        EXIT;

    // replace placeholders by values
    for xIndex := 0 to aSQLStrings.Count - 1 do
    begin
        for x := 0 to Length(xSQLArgsEdited) - 1 do
        begin
            aSQLStrings[xIndex] := StringReplace(aSQLStrings[xIndex], Format(STR_SQLTERM_PARAM_VARIABLEFORMAT,
                [x]), xSQLArgsEdited[x], [rfReplaceAll, rfIgnoreCase]);
        end;
    end;
end;


end.
