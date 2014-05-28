{ ------------------------------------------------------------------------------------------------------------
  Copyright  2009 Zinsser Analytic GmbH - All rights reserved.
  Author       : Payman Kamali (pk)
  Description  :
  ------------------------------------------------------------------------------------------------------------
  Revision History:
  date     op  method                            track-no improvement/change
  -------- --  --------------------------------  -------- ----------------------------------------------------
  16.01.09 wl                                     TN4362   an Änderungen in TViewItem angepasst
  10.06.09 pk                                     TN4600   TCompilerMessageList changed to TCompilerResult
  06.08.09 wl                                     TN4702   Strings nicht mehr über RessourceLoader
  20.05.10 wl                                     TN5117   neue Schriftart "Segoe UI", Schriftgröße 9
  26.07.11 wl                                     TN5614   ist kein TFullDockableForm mehr, Instance entfernt
  13.03.13 wl                                     TN5960   TViewItemsWorkflow.Instance statt statischer Methode
  ------------------------------------------------------------------------------------------------------------ }

unit ViewCompileMessages;


interface


uses
    Forms,
    Menus,
    Classes,
    Controls,
    cxGraphics,
    cxCustomData,
    cxStyles,
    cxTL,
    cxTextEdit,
    cxInplaceContainer,
    cxControls,
    cxLookAndFeels,
    cxLookAndFeelPainters,

    MethodCompile;

type
    TfrmViewCompileMessages = class(TForm)
        cxTreeList1: TcxTreeList;
        TVcxTreeListColumn1: TcxTreeListColumn;
        TVcxTreeListColumn2: TcxTreeListColumn;
        TVcxTreeListColumn3: TcxTreeListColumn;
        pmnuPopup: TPopupMenu;
        mnuClear: TMenuItem;
        procedure FormCreate(Sender: TObject);
        procedure cxTreeList1DblClick(Sender: TObject);
        procedure mnuClearClick(Sender: TObject);
        procedure FormClose(Sender: TObject; var Action: TCloseAction);
    private const
        cColMethodName = 0;
        cColLine = 1;
        cColText = 2;
    private
        fCompilerResult: TCompilerResult;
    public
        constructor Create(aOwner: TComponent); override;
        destructor Destroy; override;
        procedure LoadMessages();
        procedure ClearMessages();
        procedure RedirectCompilerMessage(aCompilerMessage: TCompilerMessage);
        procedure RedirectFirstCompilerMessage();
        property CompilerResult: TCompilerResult read fCompilerResult;
    end;


implementation


{$R *.dfm}

uses
    SysUtils,
    ViewItemEditForm,
    RunStepInfoFactory,
    ViewItem,
    GeneralTypes,
    ViewItemsWorkflow,
    ControlUtils;

{ TfrmViewCompileMessages }

procedure TfrmViewCompileMessages.LoadMessages();
var
    xNode: TcxTreeListNode;
    xCompilerMessage: TCompilerMessage;
    x: integer;
begin
    cxTreeList1.Clear();

    if fCompilerResult.ErrorMessages.Count = 0 then
    begin
        xNode := cxTreeList1.Add;
        xNode.Texts[cColText] := TLanguageString.Read('{0} methods compiled successfully.',
            '{0} Methoden erfolgreich kompiliert.', [fCompilerResult.TotalMethods]);
        xNode.Data := nil;
        xNode.ImageIndex := -1;
        xNode.SelectedIndex := xNode.ImageIndex;
        EXIT;
    end;

    Screen.Cursor := crHourglass;
    try

        for x := 0 to fCompilerResult.ErrorMessages.Count - 1 do
        begin
            xCompilerMessage := fCompilerResult.ErrorMessages[x];

            xNode := cxTreeList1.Add;
            xNode.Texts[cColMethodName] := xCompilerMessage.MName;
            xNode.Texts[cColLine] := IntToStr(xCompilerMessage.LineIndex + 1);
            xNode.Texts[cColText] := xCompilerMessage.Text;
            xNode.ImageIndex := -1; // TRunStepInfoFactory.GetIconIndex( 'ADDM' );
            xNode.SelectedIndex := xNode.ImageIndex;
            xNode.Data := xCompilerMessage;
        end;

        // ???
        // if Assigned(uInstance.Parent) and (uInstance.Parent.Parent is TPageControl) then
        // (uInstance.Parent.Parent as TPageControl).ActivePage := (uInstance.Parent as TTabsheet);
    finally
        Screen.Cursor := crDefault;
    end;
end;

procedure TfrmViewCompileMessages.FormClose(Sender: TObject; var Action: TCloseAction);
begin
    Action := caFree;
end;

procedure TfrmViewCompileMessages.FormCreate(Sender: TObject);
begin
    inherited;

    TControlUtils.ResetFontForWinXP(self);
    self.Caption := TLanguageString.Read('Compiler Messages', 'Kompiler-Meldungen');
    self.TVcxTreeListColumn1.Caption.Text := TLanguageString.Read('Method', 'Methode');
    self.TVcxTreeListColumn2.Caption.Text := TLanguageString.Read('Line', 'Zeile');
end;

constructor TfrmViewCompileMessages.Create(aOwner: TComponent);
begin
    inherited Create(aOwner);
    fCompilerResult := TCompilerResult.Create();
end;

destructor TfrmViewCompileMessages.Destroy();
begin
    fCompilerResult.Free;
    inherited;
end;

procedure TfrmViewCompileMessages.RedirectFirstCompilerMessage();
begin
    if fCompilerResult.ErrorMessages.Count > 0 then
    begin
        RedirectCompilerMessage(fCompilerResult.ErrorMessages[0]);
    end;
end;

procedure TfrmViewCompileMessages.RedirectCompilerMessage(aCompilerMessage: TCompilerMessage);
var
    xCurrentEditor: TForm;
begin
    if not Assigned(aCompilerMessage) then
        EXIT;
    xCurrentEditor := TViewItemsWorkflow.Instance.OpenEditForm(aCompilerMessage.MName, ntMethod);
    if (xCurrentEditor is TViewItemEditForm) then
        (xCurrentEditor as TViewItemEditForm).EditSetFocusToRow(aCompilerMessage.LineIndex);
end;

procedure TfrmViewCompileMessages.cxTreeList1DblClick(Sender: TObject);
var
    xNode: TcxTreeListNode;
    xData: TCompilerMessage;
begin
    xNode := cxTreeList1.FocusedNode;
    if not Assigned(xNode) then
        EXIT;
    xData := TCompilerMessage(xNode.Data);
    RedirectCompilerMessage(xData);

end;

procedure TfrmViewCompileMessages.mnuClearClick(Sender: TObject);
begin
    ClearMessages();
end;

procedure TfrmViewCompileMessages.ClearMessages;
begin
    fCompilerResult.ErrorMessages.Clear;
    cxTreeList1.Clear();
end;


end.
