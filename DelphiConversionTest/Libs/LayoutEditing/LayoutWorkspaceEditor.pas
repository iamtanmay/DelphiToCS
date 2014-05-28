unit LayoutWorkspaceEditor;
{ --------------------------------------------------------------------------------------------------
  Copyright © 2008 Zinsser Analytic GmbH - All rights reserved.
  Author       : Payman Kamali (pk)
  Description  : Editor for a LayoutWorkspace
  --------------------------------------------------------------------------------------------------
  Revision History:
  date     op  method                       track-no improvement/change
  -------- --  ---------------------------  -------- -----------------------------------------------
  23.06.08 pk                                TN4139  Initial Revision
  21.08.09 wl  fStringLoader                 TN4702   fStringLoader lädt Strings für Dialog-Elemente
  10.02.11 wl                                TN5475   TfraCoordSystemRelation ist TForm statt TFrame
  -------------------------------------------------------------------------------------------------- }


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
    ExtCtrls,
    CoordSystemRelationSubEditor,
    CoordSystemMath,
    StringLoader;

type
    TLayoutWorkspaceEditorStringLoader = class(TTextListStringLoader)
    protected
        procedure AddAllItems(); override;
    end;

    TfrmLayoutWorkspaceEditor = class(TForm)
        pnlBottom: TPanel;
        btnOK: TButton;
        btnCancel: TButton;
        Panel1: TPanel;
        rdoViewType: TRadioGroup;
        procedure FormClose(Sender: TObject; var Action: TCloseAction);
        procedure FormCreate(Sender: TObject);
        procedure rdoViewTypeClick(Sender: TObject);
        procedure FormDestroy(Sender: TObject);
        procedure FormShow(Sender: TObject);
    private
        fStringLoader: TLayoutWorkspaceEditorStringLoader;
        fCoordSystemRelation: TCoordSystemRelation;
        fUseCustomView: boolean;
        fraCoordSystemRelation1: TfraCoordSystemRelation;
        procedure SetCoordSystemRelation(const aValue: TCoordSystemRelation);
        procedure SetUseCustomView(aValue: boolean);
    public
        class function InstanceShowModal(aCoordSystemRelation: TCoordSystemRelation;
            var vUseCustomView: boolean): TModalResult;
        property CoordSystemRelation: TCoordSystemRelation read fCoordSystemRelation
            write SetCoordSystemRelation;
        property UseCustomView: boolean read fUseCustomView write SetUseCustomView;
    end;


implementation


{$R *.dfm}

const
    cDefaultViewItemIndex = 0;
    cCustomViewItemIndex = 1;

    { TLayoutWorkspaceEditorStringLoader }

procedure TLayoutWorkspaceEditorStringLoader.AddAllItems;
begin
    AddSingle(510, '&OK', '&OK');
    AddSingle(520, '&Cancel', '&Abbrechen');
    AddSingle(3000, 'Workspace relation', 'Workspace-Verhältnis');
    AddDouble(51900, 'Coord. System', 'Default;Custom', 'Coord. System', 'Default;Custom');
end;

{ TfrmLayoutWorkspaceEditor }

procedure TfrmLayoutWorkspaceEditor.FormCreate(Sender: TObject);
begin
    fStringLoader := TLayoutWorkspaceEditorStringLoader.Create;
    fStringLoader.LoadLanguage(self);

    self.UseCustomView := true;

    fraCoordSystemRelation1 := TfraCoordSystemRelation.Create(nil);
end;

procedure TfrmLayoutWorkspaceEditor.FormDestroy(Sender: TObject);
begin
    FreeAndNil(fraCoordSystemRelation1);
    FreeAndNil(fStringLoader);
end;

procedure TfrmLayoutWorkspaceEditor.FormShow(Sender: TObject);
begin
    fraCoordSystemRelation1.Parent := self.Panel1;
    fraCoordSystemRelation1.Visible := true;
    fraCoordSystemRelation1.Left := 124;
    fraCoordSystemRelation1.ChangeEnabled(fUseCustomView);
end;

class function TfrmLayoutWorkspaceEditor.InstanceShowModal(aCoordSystemRelation: TCoordSystemRelation;
    var vUseCustomView: boolean): TModalResult;
var
    xForm: TfrmLayoutWorkspaceEditor;
begin
    xForm := TfrmLayoutWorkspaceEditor.Create(nil);
    try
        xForm.CoordSystemRelation := aCoordSystemRelation;
        xForm.fraCoordSystemRelation1.CoordSystemRelationToGUI();
        xForm.UseCustomView := vUseCustomView;
        result := xForm.ShowModal;
        if result = mrCancel then
            EXIT;
        xForm.fraCoordSystemRelation1.CoordSystemRelationFromGUI();
        vUseCustomView := xForm.UseCustomView;
    finally
        xForm.Free;
    end;
end;

procedure TfrmLayoutWorkspaceEditor.SetCoordSystemRelation(const aValue: TCoordSystemRelation);
begin
    fCoordSystemRelation := aValue;
    self.fraCoordSystemRelation1.CoordSystemRelation := aValue;
end;

procedure TfrmLayoutWorkspaceEditor.FormClose(Sender: TObject; var Action: TCloseAction);
begin
    if self.ModalResult <> mrOK then
        EXIT;

    // if errors cancel close
    if not self.fraCoordSystemRelation1.ValuesOK() then
        Action := caNone;
end;

procedure TfrmLayoutWorkspaceEditor.SetUseCustomView(aValue: boolean);
begin
    fUseCustomView := aValue;
    if aValue then
        self.rdoViewType.ItemIndex := cCustomViewItemIndex
    else
        self.rdoViewType.ItemIndex := cDefaultViewItemIndex
end;

procedure TfrmLayoutWorkspaceEditor.rdoViewTypeClick(Sender: TObject);
begin
    SetUseCustomView(self.rdoViewType.ItemIndex = cCustomViewItemIndex);

    if fraCoordSystemRelation1 <> nil then
        fraCoordSystemRelation1.ChangeEnabled(fUseCustomView);
end;


end.
