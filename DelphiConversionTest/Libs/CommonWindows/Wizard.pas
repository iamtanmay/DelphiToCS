{ --------------------------------------------------------------------------------------------------
  Copyright © 2003 Zinsser Analytic GmbH - All rights reserved.
  Author       : Wolfgang Lyncke (wl)
  Description  : TWizardFrame: Base class for all wizard pages
  TfrmWizard: "Microsoft Wizard 97"-Form with all necessary functionality
  --------------------------------------------------------------------------------------------------
  Revision History:
  Date     op  Method                       Track-no Improvement/Change
  -------- --  ---------------------------  -------- -----------------------------------------------
  08.01.03 wl                               TN1334.1 initial version
  09.01.03 wl  TWizardPage                  TN1334.1 derived from TPanel
  09.01.03 wl                               TN1334.1 Button captions loaded from Ressource loader
  09.01.03 wl                               TN1334.1 new: constant values for Left, Width, Top
  10.01.03 wl  TWizardPage                  TN1334.1 new: AddLabel, AddDistance, ..
  15.01.03 wl  SetUp, AddComboBox           TN1295.5 neu für IniAddEntry
  16.01.03 wl  TWizardPage.AddStaticText    TN1293.5 neu
  14.02.03 wl  CreateWithBitmap             TN1293.5 Ressource-Nummer für Bitmap kann übergeben werden
  14.02.03 wl  FormCreate                   TN1293.5 Application.Icon wird geladen, wenn kein Bitmap vorhanden
  24.05.04 wl  AddAnyControl                TN1945   fügt ein beliebiges Control hinzu und versetzt FNextTop
  24.05.04 wl  AddSpinEdit                  TN1945   erzeugt ein TSpinControl
  02.12.04 wl  AddCheckBox                  TN2254.2 auch eine CheckBox ist jetzt möglich
  22.12.04 wl                               TN2246.5  Änderungen der Oberfläche für XP-Optik
  08.12.06 wl                               TN3459    TRichEdit durch TMemo ersetzt
  03.08.07 wl  FormCreate                   TN3811.2 gmGetPictureFromIcon statt TPicture-cast
  17.06.09 wl  AddLabel,AddCheckBox         TN4612   jetzt auch ohne RessourceLoader möglich
  03.08.09 wl                               TN4702   Strings werden jetzt direkt geladen
  04.11.09 pk                               TN4843   Replace TObjectList, TStringList, TStrings with Generics Lists and TStringArray
  13.04.10 wl                               TN5044   uses geändert
  21.07.10 wl                               TN5202   neue Schriftart "Segoe UI", Schriftgröße 9
  08.02.11 wl                               TN5474   Oberfläche überarbeitet, Bitmaps mit 64 x 64 möglich
  27.02.13 wl                               TN6045   uses Generics.Collections
  21.03.13 wl  AddButton                    TN6045   neu
  -------------------------------------------------------------------------------------------------- }

unit Wizard;


interface


uses
    SysUtils,
    Windows,
    Messages,
    Classes,
    Graphics,
    Controls,
    StdCtrls,
    ExtCtrls,
    Forms,
    ComCtrls,
    Grids,
    Spin,
    GeneralTypes,
    Generics.Collections;

const
    INT_WIZARD_LEFT_1 = 60; // Label oder anderes
    INT_WIZARD_LEFT_2 = 100; // eingerückte Label
    INT_WIZARD_LEFT_EDIT = 325; // TEdit hinter einem Label

    INT_WIZARD_WIDTH_1 = 565; // Breite von Objekten, ausgehend von LEFT_1, bis zum rechten Rand
    INT_WIZARD_WIDTH_EDIT = 300; // Breite von Objekten, ausgehend von LEFT_EDIT, bis zum rechten Rand

type
    TWizardDistance = (wzdXSmall, wzdSmall, wzdMedium, wzdLarge, wzdXLarge, wzdXXLarge);

    TWizardPage = class(TPanel)
    strict private
    const
        INT_WIZARD_TOP_1 = 24; // Höhe des 1.Objekts

    const
        INT_WIZARD_TOP_DIST_Q = 2; // Höhen-Unterschied zwischen Label und Edit-Feld

    const
        INT_WIZARD_TOP_DIST_XS = 10; // kleiner Abstand

    const
        INT_WIZARD_TOP_DIST_S = 20; // kleiner Abstand

    const
        INT_WIZARD_TOP_DIST_M = 30; // mittlerer Abstand

    const
        INT_WIZARD_TOP_DIST_L = 40; // großer Abstand

    const
        INT_WIZARD_TOP_DIST_XL = 50; // größerer Abstand

    const
        INT_WIZARD_TOP_DIST_XXL = 60; // größerer Abstand
    strict protected
        FNextTop: integer;
        function AddLabel(const aText: string; aLeft: integer): TLabel; overload;
        function AddEdit(aLeft, aWidth: integer): TEdit;
        function AddButton(aLeft, aWidth: integer; const aCaption: string; aOnClick: TNotifyEvent): TButton;
        function AddPasswordEdit(aLeft, aWidth: integer): TEdit;
        function AddStaticText(aLeft, aWidth: integer): TStaticText;
        function AddRadioButton(const aText: string; aLeft, aWidth: integer; aBold: boolean)
            : TRadioButton; overload;
        function AddCheckBox(const aText: string; aLeft, aWidth: integer; aBold: boolean): TCheckBox;
            overload;
        function AddComboBox(aLeft, aWidth: integer): TComboBox;
        function AddSpinEdit(aLeft, aWidth: integer): TSpinEdit;
        procedure AddAnyControl(aControl: TControl; aLeft, aWidth, aHeight: integer);
        procedure AddDistance(aDistance: TWizardDistance);
        function GetSingleTextAsHeader(const aValue: string): TStringArray;
    public
        // constructor
        constructor Create(aOwner: TComponent); override;
        // abstract methods
        function GetHeaderTitle: string; virtual; abstract;
        function GetHeaderText(): TStringArray; virtual; abstract;
        function CheckBeforeNext: boolean; virtual; abstract;
        function GetActiveControl: TWinControl; virtual; abstract;
        procedure SetUp(aPreviousPage: TWizardPage); virtual;
    end;

    TfrmWizard = class(TForm)
        Shape2: TShape;
        Bevel2: TBevel;
        lblHeaderTitle: TLabel;
        Memo1: TMemo;
        Image1: TImage;
        Panel3: TPanel;
        btnCancel: TButton;
        btnNext: TButton;
        btnBack: TButton;
        pnlMain: TPanel;
        procedure FormShow(Sender: TObject);
        procedure btnBackClick(Sender: TObject);
        procedure btnNextClick(Sender: TObject);
        procedure btnCancelClick(Sender: TObject);
        procedure FormCreate(Sender: TObject);
        procedure FormDestroy(Sender: TObject);
    private
        FPageIndex: integer;
        FList: TObjectList<TWizardPage>;
        function GetCount: integer;
        function GetItem(aIndex: Integer): TWizardPage;
        procedure ShowNextPage;
        procedure ShowPreviousPage;
        procedure SetButtons;
    public
        // Public Methods
        function Add(aItem: TWizardPage): Integer; overload;
        // Properties
        property Count: integer read GetCount;
        property Items[index: Integer]: TWizardPage read GetItem;
    end;


implementation


{$R *.DFM}

uses
    UtilLib,
    ControlUtils;

{ TWizardPage }

constructor TWizardPage.Create(aOwner: TComponent);
begin
    inherited Create(aOwner);

    BevelOuter := bvNone;
    Align := alClient;
    Caption := '';
    FNextTop := INT_WIZARD_TOP_1;
end;

function TWizardPage.AddLabel(const aText: string; aLeft: integer): TLabel;
begin
    result := TLabel.Create(self);
    result.Parent := self;
    result.Left := aLeft;
    result.Top := FNextTop + INT_WIZARD_TOP_DIST_Q;
    result.Caption := aText;
end;

function TWizardPage.AddEdit(aLeft, aWidth: integer): TEdit;
begin
    result := TEdit.Create(self);
    result.Parent := self;
    result.Left := aLeft;
    result.Top := FNextTop;
    result.Width := aWidth;
    result.Text := '';
end;

function TWizardPage.AddPasswordEdit(aLeft, aWidth: integer): TEdit;
begin
    result := AddEdit(aLeft, aWidth);
    result.PasswordChar := '*';
end;

function TWizardPage.AddStaticText(aLeft, aWidth: integer): TStaticText;
begin
    result := TStaticText.Create(self);
    result.Parent := self;
    result.BorderStyle := sbsSunken;
    result.AutoSize := false;
    result.SetBounds(aLeft, FNextTop, aWidth, 17);
end;

function TWizardPage.AddRadioButton(const aText: string; aLeft, aWidth: integer; aBold: boolean)
    : TRadioButton;
begin
    result := TRadioButton.Create(self);
    result.Parent := self;
    result.Left := aLeft;
    result.Top := FNextTop;
    result.Width := aWidth;
    if aBold then
        result.Font.Style := [fsBold];
    result.Caption := aText;
end;

function TWizardPage.AddButton(aLeft, aWidth: integer; const aCaption: string;
    aOnClick: TNotifyEvent): TButton;
begin
    result := TButton.Create(self);
    result.Parent := self;
    result.Left := aLeft;
    result.Top := FNextTop - 1;
    result.Width := aWidth;
    result.Caption := aCaption;
    result.OnClick := aOnClick;
end;

function TWizardPage.AddCheckBox(const aText: string; aLeft, aWidth: integer; aBold: boolean): TCheckBox;
begin
    result := TCheckBox.Create(self);
    result.Parent := self;
    result.Left := aLeft;
    result.Top := FNextTop;
    result.Width := aWidth;
    if aBold then
        result.Font.Style := [fsBold];
    result.Caption := aText;
end;

function TWizardPage.AddComboBox(aLeft, aWidth: integer): TComboBox;
begin
    result := TComboBox.Create(self);
    result.Parent := self;
    result.Left := aLeft;
    result.Top := FNextTop;
    result.Width := aWidth;
    result.Text := '';
end;

function TWizardPage.AddSpinEdit(aLeft, aWidth: integer): TSpinEdit;
begin
    result := TSpinEdit.Create(self);
    result.Parent := self;
    result.Left := aLeft;
    result.Top := FNextTop;
    result.Width := aWidth;
end;

procedure TWizardPage.AddAnyControl(aControl: TControl; aLeft, aWidth, aHeight: integer);
begin
    aControl.Parent := self;
    aControl.SetBounds(aLeft, FNextTop, aWidth, aHeight);
    FNextTop := FNextTop + aControl.Height;
end;

procedure TWizardPage.AddDistance(aDistance: TWizardDistance);
begin
    case (aDistance) of
        wzdXSmall:
            FNextTop := FNextTop + INT_WIZARD_TOP_DIST_XS;
        wzdSmall:
            FNextTop := FNextTop + INT_WIZARD_TOP_DIST_S;
        wzdMedium:
            FNextTop := FNextTop + INT_WIZARD_TOP_DIST_M;
        wzdLarge:
            FNextTop := FNextTop + INT_WIZARD_TOP_DIST_L;
        wzdXLarge:
            FNextTop := FNextTop + INT_WIZARD_TOP_DIST_XL;
        wzdXXLarge:
            FNextTop := FNextTop + INT_WIZARD_TOP_DIST_XXL;
    end;
end;

procedure TWizardPage.SetUp(aPreviousPage: TWizardPage);
begin
    // Dummy
end;

function TWizardPage.GetSingleTextAsHeader(const aValue: string): TStringArray;
begin
    SetLength(result, 1);
    result[0] := aValue;
end;

{ TfrmWizard }

procedure TfrmWizard.FormCreate(Sender: TObject);
begin
    FList := TObjectList<TWizardPage>.Create(true);

    FPageIndex := -1;

    btnBack.Caption := TLanguageString.Read('< Back', '< Zurück');
    btnNext.Caption := TLanguageString.Read('Next >', 'Weiter >');
    btnCancel.Caption := TLanguageString.Read('Cancel', 'Abbrechen');
    TControlUtils.ResetFontForWinXP(self);
end;

procedure TfrmWizard.FormDestroy(Sender: TObject);
begin
    FreeAndNil(FList);
end;

procedure TfrmWizard.FormShow(Sender: TObject);
begin
    // if no picture is set, use the application icon
    if (Image1.Picture.Bitmap.Empty) then
        Image1.Picture := gmGetPictureFromIcon(Application.Icon);

    ShowNextPage;
end;

function TfrmWizard.Add(aItem: TWizardPage): Integer;
begin
    aItem.Parent := self.pnlMain;
    aItem.Top := 64;
    aItem.Left := 3;
    aItem.Visible := false;

    result := FList.Add(aItem);
end;

function TfrmWizard.GetCount: integer;
begin
    result := FList.Count;
end;

function TfrmWizard.GetItem(aIndex: Integer): TWizardPage;
begin
    if (aIndex >= 0) and (aIndex < FList.Count) then
        result := FList.Items[aIndex] as TWizardPage
    else
        result := nil;
end;

procedure TfrmWizard.ShowNextPage;
begin
    // if last page: finish & close
    if ((FPageIndex + 1) = FList.Count) then
    begin
        ModalResult := mrOK;
        exit;
    end;

    // show next page
    if ((FPageIndex + 1) < FList.Count) then
    begin
        FPageIndex := FPageIndex + 1;
        SetButtons;
    end;
end;

procedure TfrmWizard.ShowPreviousPage;
begin
    // show previous page
    if ((FPageIndex - 1) >= 0) then
    begin
        FPageIndex := FPageIndex - 1;
        SetButtons;
    end;
end;

procedure TfrmWizard.SetButtons;
var
    xPage: TWizardPage;
    xHeaderValues: TStringArray;
begin
    xPage := GetItem(FPageIndex);
    xPage.SetUp(GetItem(FPageIndex - 1));
    xPage.Visible := true;
    lblHeaderTitle.Caption := xPage.GetHeaderTitle;

    xHeaderValues := xPage.GetHeaderText();
    TControlUtils.AddValuesToMemo(xHeaderValues, Memo1, true);

    if (FPageIndex = FList.Count - 1) then
        btnNext.Caption := TLanguageString.Read('Finish', 'Fertig stellen')
    else
        btnNext.Caption := TLanguageString.Read('Next >', 'Weiter >');

    if (FPageIndex <= 0) then
        btnBack.Enabled := false
    else
        btnBack.Enabled := true;

    if (FPageIndex < 0) then
        btnNext.Enabled := false
    else
        btnNext.Enabled := true;

    ActiveControl := GetItem(FPageIndex).GetActiveControl;
end;

procedure TfrmWizard.btnBackClick(Sender: TObject);
begin
    // hide current page
    GetItem(FPageIndex).Visible := false;
    // show previous page
    ShowPreviousPage;
end;

procedure TfrmWizard.btnNextClick(Sender: TObject);
begin
    if GetItem(FPageIndex).CheckBeforeNext then
    begin
        // hide current page
        GetItem(FPageIndex).Visible := false;
        // show next page
        ShowNextPage;
    end
    else
        ActiveControl := GetItem(FPageIndex).GetActiveControl;
end;

procedure TfrmWizard.btnCancelClick(Sender: TObject);
begin
    Close;
end;


end.
