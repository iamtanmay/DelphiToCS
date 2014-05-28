{ -----------------------------------------------------------------------------------------------------------------------
  Copyright © 2010 Zinsser Analytic GmbH - All rights reserved.
  Author       : Payman Kamali (pk)
  Description  :
  -----------------------------------------------------------------------------------------------------------------------
  Revision History:
  date     op  method                                track-no  improvement/change
  -------- --  ------------------------------------  --------- ----------------------------------------------------------
  07.06.10 pk                                        TN5077     initial revision
  21.06.10 wl                                        TN5160   Position = poScreenCenter
  10.04.13 wl                                        TN6045   uses Generics.Collections
  ----------------------------------------------------------------------------------------------------------------------- }

unit RestartPreparations;


interface


uses
    Windows,
    Messages,
    SysUtils,
    Variants,
    Classes,
    Graphics,
    Controls,
    Forms,
    Dialogs,
    StdCtrls,
    ComCtrls,
    ExtCtrls,
    Generics.Collections,
    RestartPreparationStep,
    cxGraphics,
    cxControls,
    cxLookAndFeels,
    cxLookAndFeelPainters,
    cxCustomData,
    cxStyles,
    cxTL,
    cxDropDownEdit,
    cxInplaceContainer,
    cxTextEdit;

type
    TPageMoveDirection = (pmdNone, pmdForward, pmdBackward);

    TAllowedMode = (imNone, imAll);

    TLoadWizardPageCallback = TNotifyEvent;
    TValidateWizardPageCallback = procedure(const aSender: TObject; out oCancel: boolean) of object;

    TWizardPage = class
    private
        fCaption: string;
        fNextCaption: string;
        fNextClickCallback: TNotifyEvent;
        fBackAllowed: boolean;
        fControl: TTabSheet;
        fAllowedMode: TAllowedMode;
        fLoadCallback: TLoadWizardPageCallback;
        fValidateCallback: TValidateWizardPageCallback;
    public
        constructor Create(const aCaption: string; const aNextCaption: string;
            const aNextClickCallback: TNotifyEvent; const aBackAllowed: boolean;
            const aAllowedMode: TAllowedMode; const aControl: TTabSheet;
            const aLoadCallback: TLoadWizardPageCallback;
            const aValidateCallback: TValidateWizardPageCallback);

        property Caption: string read fCaption;
        property NextCaption: string read fNextCaption write fNextCaption;
        property NextClickCallback: TNotifyEvent read fNextClickCallback;
        property BackAllowed: boolean read fBackAllowed;
        property AllowedMode: TAllowedMode read fAllowedMode;
        property Control: TTabsheet read fControl;
        property LoadCallback: TLoadWizardPageCallback read fLoadCallback;
        property ValidateCallback: TValidateWizardPageCallback read fValidateCallback;
    end;

    TWizardPageList = class(TObjectList<TWizardPage>)
    public
        function FindPageByName(const aPageName: string): TWizardPage;
    end;

    TfrmRestartPreparations = class(TForm)
        Shape1: TShape;
        Panel1: TPanel;
        lblTitle: TLabel;
        edMethodName: TEdit;
        Panel3: TPanel;
        PageControl1: TPageControl;
        tbsWelcome: TTabSheet;
        tbsLayout: TTabSheet;
        pnlBottom: TPanel;
        Shape2: TShape;
        btnCancel: TButton;
        btnNext: TButton;
        btnBack: TButton;
        Label1: TLabel;
        mmoIntro: TMemo;
        tbsCompletion: TTabSheet;
        tlLayout: TcxTreeList;
        cxTreeList1cxTreeListColumn1: TcxTreeListColumn;
        procedure btnBackClick(Sender: TObject);
    private
        fRestartPreparationList: TRestartPreparationStepList;
        fVisualManager: TRestartPreparationsVisualManager;
        fPages: TWizardPageList;
        fActivePageNumber: integer;
        procedure GotoPage(aIndex: integer; aPageDirection: TPageMoveDirection);
        procedure GotoNextPage;
        procedure SetVisualManagerForSteps();
        procedure VisualizeSteps();
        procedure CreatePages();
        procedure btnCloseClick(Sender: TObject);
        procedure btnNextClick(Sender: TObject);
        function IsModeAllowed(const aMode: TAllowedMode): boolean;

    public
        constructor Create(const aMethodName: string;
            const aRestartPreparationList: TRestartPreparationStepList); reintroduce;
        destructor Destroy(); override;
        class function InstanceShowModal(const aMethodName: string;
            const aRestartPreparationList: TRestartPreparationStepList): TModalResult;
    end;

    TRealRestartPreparationsVisualManager = class(TRestartPreparationsVisualManager)
    private
        fControl: TfrmRestartPreparations;
    public
        constructor Create(const aControl: TfrmRestartPreparations);
        procedure AddText(const aText: string); override;
    end;


implementation


{$R *.dfm}

uses
    GeneralTypes;

const

    cPageNameConfirm = 'Confirm';
    cNextCaption = 'Next >';

    { TWizardPage }

constructor TWizardPage.Create(const aCaption: string; const aNextCaption: string;
    const aNextClickCallback: TNotifyEvent; const aBackAllowed: boolean; const aAllowedMode: TAllowedMode;
    const aControl: TTabSheet; const aLoadCallback: TLoadWizardPageCallback;
    const aValidateCallback: TValidateWizardPageCallback);
begin
    inherited Create();
    fCaption := aCaption;
    fNextCaption := aNextCaption;
    fNextClickCallback := aNextClickCallback;
    fBackAllowed := aBackAllowed;
    fAllowedMode := aAllowedMode;
    fControl := aControl;
    fLoadCallback := aLoadCallback;
    fValidateCallback := aValidateCallback;
end;

{ TWizardPageList }

function TWizardPageList.FindPageByName(const aPageName: string): TWizardPage;
var
    xPage: TWizardPage;
begin
    result := nil;
    for xPage in self do
    begin
        if SameText(xPage.Caption, aPageName) then
        begin
            result := xPage;
            EXIT;
        end;
    end;

end;
{ TfrmAskRunStart }

constructor TfrmRestartPreparations.Create(const aMethodName: string;
    const aRestartPreparationList: TRestartPreparationStepList);
var
    x: integer;
begin
    inherited Create(nil);

    self.edMethodName.Text := aMethodName;
    fRestartPreparationList := aRestartPreparationList;
    fVisualManager := TRealRestartPreparationsVisualManager.Create(self);
    SetVisualManagerForSteps();
    VisualizeSteps();

    fPages := TWizardPageList.Create();
    CreatePages();

    for x := 0 to PageControl1.PageCount - 1 do
    begin
        PageControl1.Pages[x].TabVisible := false;
    end;

    // select the first tab
    GotoPage(0, pmdNone);

end;

destructor TfrmRestartPreparations.Destroy;
begin
    FreeAndNil(fPages);
    inherited;
end;

procedure TfrmRestartPreparations.CreatePages();
begin
    fPages := TWizardPageList.Create;
    fPages.Add(TWizardPage.Create('Welcome', cNextCaption, btnNextClick, true, imAll, self.tbsWelcome,
        nil, nil));
    fPages.Add(TWizardPage.Create('Layout', cNextCaption, btnNextClick, true, imAll, self.tbsLayout,
        nil, nil));
    fPages.Add(TWizardPage.Create('Completed', 'Close', btnCloseClick, false, imAll, self.tbsCompletion,
        nil, nil));

end;

procedure TfrmRestartPreparations.VisualizeSteps();
var
    xStep: TRestartPreparationStep;
begin
    for xStep in fRestartPreparationList do
        xStep.Visualize();
end;

procedure TfrmRestartPreparations.SetVisualManagerForSteps();
var
    xStep: TRestartPreparationStep;
begin
    for xStep in fRestartPreparationList do
        xStep.SetVisualManager(fVisualManager);
end;

// procedure TfrmRestartPreparations.GotoPage( aIndex : integer; aPageDirection : TPageMoveDirection );
// var
// xIndex : integer;
// begin
// xIndex := aIndex;
//
// if xIndex < 0 then xIndex := 0;
// if xIndex > self.PageControl1.PageCount - 1 then
// xIndex := self.PageControl1.PageCount - 1;
//
// self.PageControl1.ActivePageIndex := xIndex;
//
// if PageControl1.ActivePageIndex = ( PageControl1.PageCount - 1 ) then
// self.btnNext.Caption := TLanguageString.Read( '&OK', '&OK' )
// else
// self.btnNext.Caption := TLanguageString.Read( '&Next', '&Weiter' ) + ' >';
//
// self.btnBack.Enabled := PageControl1.ActivePageIndex > 0;
// end;

function TfrmRestartPreparations.IsModeAllowed(const aMode: TAllowedMode): boolean;
begin
    result := true;
end;

procedure TfrmRestartPreparations.GotoPage(aIndex: integer; aPageDirection: TPageMoveDirection);
var
    xWizardPage: TWizardPage;
    xCancel: boolean;
    xAllowedPageNumber: integer;
begin
    // before going to next page validate the current page
    if (aPageDirection = pmdForward) and (fActivePageNumber >= 0) and
        (fActivePageNumber <= (fPages.Count - 1)) then
    begin
        xWizardPage := fPages[fActivePageNumber];
        if Assigned(xWizardPage.ValidateCallback) then
        begin
            xWizardPage.ValidateCallback(xWizardPage, xCancel);
            if xCancel then
                EXIT;
        end;
    end;

    xAllowedPageNumber := aIndex;
    xWizardPage := nil;
    while true do
    begin
        xWizardPage := fPages[xAllowedPageNumber];

        if IsModeAllowed(xWizardPage.AllowedMode) then
            BREAK;

        if aPageDirection = pmdForward then
            Inc(xAllowedPageNumber)
        else if aPageDirection = pmdBackward then
            Dec(xAllowedPageNumber);
    end;

    fActivePageNumber := xAllowedPageNumber;

    self.PageControl1.ActivePageIndex := xWizardPage.Control.PageIndex;

    // self.pnlTop.Visible := true;
    // self.lblHeader.Caption := xWizardPage.Caption;
    // lblUpdateDescr.Caption := xWizardPage.Caption;

    self.btnNext.Caption := xWizardPage.NextCaption;
    self.btnNext.OnClick := xWizardPage.NextClickCallback;

    self.btnNext.Visible := Assigned(xWizardPage.NextClickCallback);

    self.btnBack.Visible := (fActivePageNumber > 0) and xWizardPage.BackAllowed;

    if Assigned(xWizardPage.LoadCallback) then
        xWizardPage.LoadCallback(xWizardPage);

end;

procedure TfrmRestartPreparations.GotoNextPage();
begin
    GotoPage(PageControl1.ActivePageIndex + 1, pmdForward);
end;

procedure TfrmRestartPreparations.btnBackClick(Sender: TObject);
begin
    GotoPage(PageControl1.ActivePageIndex - 1, pmdBackward);
end;

procedure TfrmRestartPreparations.btnCloseClick(Sender: TObject);
begin
    self.ModalResult := mrOK;
end;

procedure TfrmRestartPreparations.btnNextClick(Sender: TObject);
begin
    GotoNextPage();
end;

// procedure TfrmRestartPreparations.btnNextClick(Sender: TObject);
// begin
// if PageControl1.ActivePageIndex = ( PageControl1.PageCount - 1 ) then begin
// self.ModalResult := mrOK;
// EXIT;
// end;
// GotoNextPage();
// end;

class function TfrmRestartPreparations.InstanceShowModal(const aMethodName: string;
    const aRestartPreparationList: TRestartPreparationStepList): TModalResult;
var
    xForm: TfrmRestartPreparations;
begin
    xForm := TfrmRestartPreparations.Create(aMethodName, aRestartPreparationList);
    try
        result := xForm.ShowModal;
    finally
        xForm.Free;
    end;
end;
{ TRealRestartPreparationsVisualManager }

procedure TRealRestartPreparationsVisualManager.AddText(const aText: string);
var
    xNode: TcxTreeListNode;
begin
    xNode := fControl.tlLayout.Add;
    xNode.Texts[0] := aText

end;

constructor TRealRestartPreparationsVisualManager.Create(const aControl: TfrmRestartPreparations);
begin
    inherited Create();
    fControl := aControl;
end;


end.
