{ -----------------------------------------------------------------------------------------------------------
  Copyright © 2013 Zinsser Analytic GmbH - All rights reserved.
  Author       : Wolfgang Lyncke (wl)
  Description  :
  -----------------------------------------------------------------------------------------------------------
  Revision History:
  date     op  method                              track-no improvement/change
  -------- --  ----------------------------------  -------- -------------------------------------------------
  22.04.13 wl                                      TN6095   Initial Revision
  08.08.13 wl  OnNextButtonSetFocus,-SetDefault    TN6095   neu für bessere Benutzerführung
  08.08.13 wl  FormDestroy                         TN6095   Zerstörung der Pages verbessert
  ----------------------------------------------------------------------------------------------------------- }

unit MultiPageDialog;


interface


uses
    Classes,
    Graphics,
    Generics.Collections,
    Forms,
    Dialogs,
    StdCtrls,
    ExtCtrls,
    ComCtrls,
    Controls;

type
    TMultiPageDialogSetDefaultEvent = procedure(aSender: TObject; aIsDefault: boolean) of object;

    TMultiDialogPage = class(TForm)
    private
        fOnNextButtonClick: TNotifyEvent;
        fOnNextButtonSetFocus: TNotifyEvent;
        fOnNextButtonSetDefault: TMultiPageDialogSetDefaultEvent;
    protected
        procedure DoExitPage(Sender: TObject);
        procedure NextButtonSetFocus();
        procedure NextButtonSetDefault(aIsDefault: boolean);
    public
        constructor Create; reintroduce;
        procedure RefreshPageData(); virtual; abstract;
        procedure FirstSetFocus(); virtual; abstract;
        function WritePageData(aCheckBefore: boolean): boolean; virtual; abstract;
        property OnNextButtonClick: TNotifyEvent read fOnNextButtonClick write fOnNextButtonClick;
        property OnNextButtonSetFocus: TNotifyEvent read fOnNextButtonSetFocus write fOnNextButtonSetFocus;
        property OnNextButtonSetDefault: TMultiPageDialogSetDefaultEvent read fOnNextButtonSetDefault
            write fOnNextButtonSetDefault;
    end;

    TfrmMultiPageDialog = class(TForm)
        Panel3: TPanel;
        btnCancel: TButton;
        btnNext: TButton;
        btnBack: TButton;
        PageControl1: TPageControl;
        procedure FormShow(Sender: TObject);
        procedure btnNextClick(Sender: TObject);
        procedure btnBackClick(Sender: TObject);
        procedure FormCreate(Sender: TObject);
        procedure FormDestroy(Sender: TObject);
    private
        fPages: TList<TMultiDialogPage>;
        procedure ButtonRefresh();
        function GetPageCount: integer;
        procedure NextButtonSetDefault(aSender: TObject; aIsDefault: boolean);
        procedure NextButtonSetFocus(aSender: TObject);
    public
        procedure AddPage(aPage: TMultiDialogPage);
        property PageCount: integer read GetPageCount;
    end;


implementation


{$R *.dfm}

uses
    SysUtils;

{ TMultiDialogPage }

constructor TMultiDialogPage.Create;
begin
    inherited Create(nil); // Page soll keinen Owner haben
end;

procedure TMultiDialogPage.DoExitPage(Sender: TObject);
begin
    if Assigned(fOnNextButtonClick) then
        fOnNextButtonClick(Sender);
end;

procedure TMultiDialogPage.NextButtonSetDefault(aIsDefault: boolean);
begin
    if Assigned(fOnNextButtonSetDefault) then
        fOnNextButtonSetDefault(self, aIsDefault);
end;

procedure TMultiDialogPage.NextButtonSetFocus;
begin
    if Assigned(fOnNextButtonSetFocus) then
        fOnNextButtonSetFocus(self);
end;

{ TfrmMultiPageDialog }

procedure TfrmMultiPageDialog.FormCreate(Sender: TObject);
begin
    fPages := TList<TMultiDialogPage>.Create;
end;

procedure TfrmMultiPageDialog.FormDestroy(Sender: TObject);
var
    x: integer;
begin
    for x := 0 to fPages.Count - 1 do
        fPages[x].Free; // sicherstellen, dass Pages vor Dialog zerstört werden
    FreeAndNil(fPages);
end;

procedure TfrmMultiPageDialog.AddPage(aPage: TMultiDialogPage);
begin
    aPage.Align := alClient;
    aPage.BorderStyle := bsNone;
    aPage.ManualDock(PageControl1);
    aPage.Visible := true;
    aPage.OnNextButtonClick := btnNextClick;
    aPage.OnNextButtonSetFocus := NextButtonSetFocus;
    aPage.OnNextButtonSetDefault := NextButtonSetDefault;

    fPages.Add(aPage);
end;

procedure TfrmMultiPageDialog.btnBackClick(Sender: TObject);
begin
    // Daten der aktuellen Seite wegschreiben (kein Check, result wird nicht abgefragt)
    fPages[self.PageControl1.ActivePageIndex].WritePageData(false);

    // Eine Seite zurück
    self.PageControl1.ActivePageIndex := self.PageControl1.ActivePageIndex - 1;

    // Button Refresh
    ButtonRefresh;

    // abhängige Daten auffrischen
    fPages[self.PageControl1.ActivePageIndex].RefreshPageData();
end;

procedure TfrmMultiPageDialog.btnNextClick(Sender: TObject);
begin
    // Daten der aktuellen Seite wegschreiben
    if not fPages[self.PageControl1.ActivePageIndex].WritePageData(true) then
        EXIT;

    // Letzte Seite: raus
    if (PageControl1.ActivePageIndex = PageControl1.PageCount - 1) then
    begin
        self.ModalResult := mrOK;
        EXIT;
    end;

    // Eine Seite vor
    self.PageControl1.ActivePageIndex := self.PageControl1.ActivePageIndex + 1;

    // Button Refresh
    ButtonRefresh;

    // abhängige Daten auffrischen
    fPages[self.PageControl1.ActivePageIndex].RefreshPageData();
    fPages[self.PageControl1.ActivePageIndex].FirstSetFocus();
end;

procedure TfrmMultiPageDialog.ButtonRefresh;
begin
    self.btnBack.Enabled := (PageControl1.ActivePageIndex > 0);

    if (PageControl1.ActivePageIndex < PageControl1.PageCount - 1) then
        self.btnNext.Caption := '&Next >'
    else
        self.btnNext.Caption := '&Finish';
end;

procedure TfrmMultiPageDialog.FormShow(Sender: TObject);
var
    // xNeededHeight: integer;
    x: integer;
begin
    { xNeededHeight := self.Height - self.cxTreeList1.Height +
      ((self.cxTreeList1.DefaultIndentSize.cy + 2) * (FIdentList.Count + 2));
      self.Height := xNeededHeight;
      if (self.Height > Screen.Height - 40) then
      self.Height := Screen.Height - 40;
      self.Top := Round((Screen.Height / 2) - (self.Height / 2)) }

    for x := 0 to PageControl1.PageCount - 1 do
    begin
        PageControl1.Pages[x].TabVisible := false;
    end;

    PageControl1.ActivePageIndex := 0;
    fPages[self.PageControl1.ActivePageIndex].FirstSetFocus();

    if (PageControl1.PageCount = 1) then
    begin
        btnNext.Caption := '&OK';
        btnBack.Visible := false;
    end;
end;

function TfrmMultiPageDialog.GetPageCount: integer;
begin
    EXIT(fPages.Count);
end;

procedure TfrmMultiPageDialog.NextButtonSetDefault(aSender: TObject; aIsDefault: boolean);
begin
    self.btnNext.Default := aIsDefault;
end;

procedure TfrmMultiPageDialog.NextButtonSetFocus(aSender: TObject);
begin
    self.btnNext.SetFocus;
end;


end.
