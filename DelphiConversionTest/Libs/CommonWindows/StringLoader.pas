{ ------------------------------------------------------------------------------------------------------------
  Copyright  2009 Zinsser Analytic GmbH - All rights reserved.
  Author       : Wolfgang Lyncke (wl)
  Description  : Replacement for resource loading from rc-Files
  ------------------------------------------------------------------------------------------------------------
  Revision History:
  date     op  method                            track-no improvement/change
  -------- --  --------------------------------  -------- ----------------------------------------------------
  06.02.09 wl                                    TN4370   initial revision
  09.02.09 wl  AddStandardButtonStrings          TN4370   neu für alle Dialoge
  16.02.09 wl  uLanguage                         TN4370   --> GeneralTypes (TLanguageString)
  20.02.09 wl  AddStandardButtonStrings          TN4370   Yes und No entfernt
  25.02.09 pk  TLanguageTextList                 TN4370   inherits from TIntList instead of TObjectList
  10.08.09 wl                                    TN4702   ein paar Funktionen wieder entfernt
  04.11.09 pk                                 	TN4843   Replace TObjectList, TStringList, TStrings with Generics Lists and TStringArray
  30.07.10 wl  TStringLoader.LoadLanguage        TN5209   neu: TToolButton
  03.12.10 pk  LoadLanguage                      TN5381   changed from TScrollingWinControl to TWinControl
  11.04.13 wl  TLanguageTextList                 TN6045   überflüssige Klasse entfernt
  ------------------------------------------------------------------------------------------------------------ }

unit StringLoader;


interface


uses
    Generics.Collections,
    Controls,
    GeneralTypes;

type
    TStringLoader = class
    protected
        procedure GetDoubleResString(aResNr: integer; out oText1, oText2: string); virtual; abstract;
    public
        procedure LoadLanguage(const aApplObject: TWinControl);
        function GetResString(aResNr: integer): string; virtual; abstract;
        function ResMsgBox(aResNr, aMsgOptions: integer): integer;
        function ResInputBox(aResNr: integer; aDefault: string): string;
        function ResInputQuery(aResNr: integer; var vValue: string): boolean;

        // diese Funktionen sollten noch raus
        function GetResFString(aResNr: integer; aArgs: array of const ): string;
        function ResFMsgBox(aResNr, aMsgOptions: integer; aTextArgs: array of const ): integer;
    end;

    TLanguageText = class
    private
        fIdentifier: integer;
    protected
        function GetText(): string; virtual; abstract;
    public
        constructor Create(aIdentifier: integer);
        procedure GetDoubleText(out oText1, oText2: string); virtual; abstract;
        property Text: string read GetText;
        property Identifier: integer read fIdentifier;
    end;

    TSingleLanguageText = class(TLanguageText)
    private
        fString1: TLanguageString;
    protected
        function GetText(): string; override;
    public
        constructor Create(aIdentifier: integer; const aText_EN, aText_DE: string);
        destructor Destroy; override;
        procedure GetDoubleText(out oText1, oText2: string); override;
    end;

    TDoubleLanguageText = class(TLanguageText)
    private
        fString1: TLanguageString;
        fString2: TLanguageString;
    protected
        function GetText(): string; override;
    public
        constructor Create(aIdentifier: integer; const aText1_EN, aText2_EN, aText1_DE, aText2_DE: string);
        destructor Destroy; override;
        procedure GetDoubleText(out oText1, oText2: string); override;
    end;

    TTextListStringLoader = class(TStringLoader)
    private
        fAllTexts: TObjectList<TLanguageText>;
    protected
        procedure GetDoubleResString(aResNr: integer; out oText1, oText2: string); override;
        procedure AddSingle(aIdentifier: integer; const aText_EN, aText_DE: string);
        procedure AddDouble(aIdentifier: integer; const aText1_EN, aText2_EN, aText1_DE, aText2_DE: string);
        procedure AddAllItems(); virtual; abstract;
        procedure AddStandardButtonStrings();
    public
        constructor Create();
        destructor Destroy; override;

        function GetResString(aResNr: integer): string; override;
    end;


implementation


uses
    Windows,
    Forms,
    Classes,
    StdCtrls,
    DBCtrls,
    Menus,
    ComCtrls,
    ExtCtrls,
    SysUtils,
    Buttons,
    DBGrids,
    DialogUtils,
    Utility2;

{ TStringLoader }

function TStringLoader.GetResFString(aResNr: integer; aArgs: array of const ): string;
begin
    result := Format(self.GetResString(aResNr), aArgs);
end;

procedure TStringLoader.LoadLanguage(const aApplObject: TWinControl);
var
    xComponentIndex, i, ResNo, ActChar: integer;
    c: TComponent;
    k: boolean;
    ReadStr: string;
    xText1, xText2: string;
begin
    // Caption laden
    if (aApplObject is TForm) then
        (aApplObject as TForm).Caption := GetResString(aApplObject.tag);

    k := (Pos('   ', GetResString(aApplObject.tag)) > 0);

    for xComponentIndex := 0 to aApplObject.ComponentCount - 1 do
    begin

        c := aApplObject.Components[xComponentIndex];

        if (c is TDBGrid) then
        begin
            for i := 0 to (c as TDBGrid).Columns.Count - 1 do
            begin
                if gmStrToIntTry(ResNo, (c as TDBGrid).Columns[i].Title.Caption) then
                    (c as TDBGrid).Columns[i].Title.Caption := GetResString(ResNo);
            end;
        end
        else if (c is TListView) then
            for i := 0 to (c as TListView).Columns.Count - 1 do
            begin
                if gmStrToIntTry(ResNo, (c as TListView).Columns[i].Caption) then
                    (c as TListView).Columns[i].Caption := GetResString(ResNo);
            end
        else if (c is TImage) then
        begin
            if k then
                (c as TImage).Align := alClient;
        end;

        if (c.Tag = 0) then
            CONTINUE;

        if (c is TLabel) then
        begin
            (c as TLabel).caption := GetResString((c as TLabel).tag);
        end
        else if (c is TCheckbox) then
        begin
            (c as TCheckbox).caption := GetResString((c as TCheckbox).tag);
            if k then
                (c as TCheckBox).Visible := false;
        end
        else if (c is TDBCheckbox) then
        begin
            (c as TDBCheckbox).caption := GetResString((c as TDBCheckbox).tag);
        end
        else if (c is TMenuitem) then
        begin
            (c as TMenuitem).caption := GetResString((c as TMenuitem).tag);
        end
        else if (c is TGroupBox) then
        begin
            (c as TGroupBox).caption := GetResString((c as TGroupBox).tag);
            if k then
                (c as TGroupBox).Visible := false;
        end
        else if (c is TTabSheet) then
        begin
            (c as TTabSheet).caption := GetResString((c as TTabSheet).tag);
        end
        else if (c is TRadioButton) then
        begin
            (c as TRadioButton).caption := GetResString((c as TRadioButton).tag);
        end
        else if (c is TDBRadioGroup) then
        begin
            self.GetDoubleResString((c as TDBRadioGroup).tag, xText1, xText2);
            (c as TDBRadioGroup).caption := xText1;
            if (xText2 <> '') then
            begin
                (c as TDBRadioGroup).Items.Clear;
                ActChar := 1;
                repeat
                    ReadStr := gmReadSubString(ActChar, xText2, ';');
                    if (ReadStr <> '') then
                        (c as TDBRadioGroup).Items.Add(ReadStr);
                until (ReadStr = '');
            end;
        end
        else if (c is TRadioGroup) then
        begin
            self.GetDoubleResString((c as TRadioGroup).tag, xText1, xText2);
            (c as TRadioGroup).caption := xText1;
            if (xText2 <> '') then
            begin
                (c as TRadioGroup).Items.Clear;
                ActChar := 1;
                repeat
                    ReadStr := gmReadSubString(ActChar, xText2, ';');
                    if (ReadStr <> '') then
                        (c as TRadioGroup).Items.Add(ReadStr);
                until (ReadStr = '');
            end;
        end
        else if (c is TSpeedButton) then
        begin
            self.GetDoubleResString((c as TSpeedButton).tag, xText1, xText2);
            (c as TSpeedButton).Caption := xText1;
            (c as TSpeedButton).Hint := xText2;
        end
        else if (c is TToolButton) then
        begin
            self.GetDoubleResString((c as TToolButton).tag, xText1, xText2);
            (c as TToolButton).Caption := xText1;
            (c as TToolButton).Hint := xText2;
        end
        else if (c is TButton) then
        begin
            self.GetDoubleResString((c as TButton).tag, xText1, xText2);
            (c as TButton).Caption := xText1;
            (c as TButton).Hint := xText2;
            if k then
                (c as TButton).Visible := false;
        end
        else if (c is TTreeView) then
        begin
            ActChar := 1;
            for i := 0 to (c as TTreeView).Items.Count - 1 do
            begin
                ReadStr := gmReadSubString(ActChar, GetResString((c as TTreeView).tag), ';');
                if (ReadStr <> '') then
                    (c as TTreeView).Items[i].Text := ReadStr;
            end;
        end;
    end;
end;

function TStringLoader.ResFMsgBox(aResNr, aMsgOptions: integer; aTextArgs: array of const ): integer;
var
    xText1, xText2: string;
begin
    self.GetDoubleResString(aResNr, xText1, xText2);
    result := TDialogUtils.MessageBox(Format(xText1, aTextArgs), xText2, aMsgOptions);
end;

function TStringLoader.ResInputBox(aResNr: integer; aDefault: string): string;
var
    xText1, xText2: string;
begin
    self.GetDoubleResString(aResNr, xText1, xText2);
    result := TDialogUtils.InputBox(xText2, xText1, aDefault);
end;

function TStringLoader.ResInputQuery(aResNr: integer; var vValue: string): boolean;
var
    xText1, xText2: string;
begin
    self.GetDoubleResString(aResNr, xText1, xText2);
    result := TDialogUtils.InputQuery(xText2, xText1, vValue);
end;

function TStringLoader.ResMsgBox(aResNr, aMsgOptions: integer): integer;
var
    xText1, xText2: string;
begin
    self.GetDoubleResString(aResNr, xText1, xText2);
    result := TDialogUtils.MessageBox(xText1, xText2, aMsgOptions);
end;

{ TTextListStringLoader }

procedure TTextListStringLoader.AddDouble(aIdentifier: integer; const aText1_EN, aText2_EN, aText1_DE,
    aText2_DE: string);
begin
    fAllTexts.Add(TDoubleLanguageText.Create(aIdentifier, aText1_EN, aText2_EN, aText1_DE, aText2_DE));
end;

procedure TTextListStringLoader.AddSingle(aIdentifier: integer; const aText_EN, aText_DE: string);
begin
    fAllTexts.Add(TSingleLanguageText.Create(aIdentifier, aText_EN, aText_DE));
end;

procedure TTextListStringLoader.AddStandardButtonStrings;
begin
    AddSingle(510, '&OK', '&OK');
    AddSingle(520, '&Cancel', '&Abbrechen');
    AddSingle(530, '&Abort', '&Abbrechen');
    AddSingle(540, '&Retry', '&Wiederholen');
    AddSingle(550, '&Ignore', '&Ignorieren');
    AddSingle(560, '&Yes', '&Ja');
    AddSingle(570, '&No', '&Nein');
    AddSingle(580, '&Continue', '&Fortsetzen');
    AddSingle(590, '&Apply', 'Ü&bernehmen');
    AddSingle(600, '< Back', '< Zurück');
    AddSingle(610, 'Next >', 'Weiter >');
    AddSingle(620, 'Finish', 'Fertig stellen');
    AddSingle(630, 'Cancel', 'Abbrechen');
end;

constructor TTextListStringLoader.Create();
begin
    inherited Create;
    fAllTexts := TObjectList<TLanguageText>.Create;
    self.AddAllItems();
end;

destructor TTextListStringLoader.Destroy;
begin
    FreeAndNil(fAllTexts);
    inherited;
end;

procedure TTextListStringLoader.GetDoubleResString(aResNr: integer; out oText1, oText2: string);
var
    x: Integer;
begin
    oText1 := '';
    oText2 := '';
    for x := 0 to fAllTexts.Count - 1 do
    begin
        if (fAllTexts[x].Identifier = aResNr) then
        begin
            fAllTexts[x].GetDoubleText(oText1, oText2);
            EXIT;
        end;
    end;
end;

function TTextListStringLoader.GetResString(aResNr: integer): string;
var
    x: Integer;
begin
    result := '';
    for x := 0 to fAllTexts.Count - 1 do
    begin
        if (fAllTexts[x].Identifier = aResNr) then
        begin
            result := fAllTexts[x].Text;
            EXIT;
        end;
    end;
end;

{ TLanguageText }

constructor TLanguageText.Create(aIdentifier: integer);
begin
    inherited Create;
    fIdentifier := aIdentifier;
end;

{ TSingleLanguageText }

constructor TSingleLanguageText.Create(aIdentifier: integer; const aText_EN, aText_DE: string);
begin
    inherited Create(aIdentifier);

    fString1 := TLanguageString.Create(aText_EN, aText_DE);
end;

destructor TSingleLanguageText.Destroy;
begin
    FreeAndNil(fString1);

    inherited;
end;

procedure TSingleLanguageText.GetDoubleText(out oText1, oText2: string);
begin
    oText1 := self.Text;
    oText2 := '';
end;

function TSingleLanguageText.GetText: string;
begin
    result := fString1.Text;
end;

{ TDoubleLanguageText }

constructor TDoubleLanguageText.Create(aIdentifier: integer; const aText1_EN, aText2_EN, aText1_DE,
    aText2_DE: string);
begin
    inherited Create(aIdentifier);

    fString1 := TLanguageString.Create(aText1_EN, aText1_DE);
    fString2 := TLanguageString.Create(aText2_EN, aText2_DE);
end;

destructor TDoubleLanguageText.Destroy;
begin
    FreeAndNil(fString1);
    FreeAndNil(fString2);

    inherited;
end;

procedure TDoubleLanguageText.GetDoubleText(out oText1, oText2: string);
begin
    oText1 := self.Text;
    oText2 := fString2.Text;
end;

function TDoubleLanguageText.GetText: string;
begin
    result := fString1.Text;
end;


end.
