{ --------------------------------------------------------------------------------------------------
  Copyright © 2007 Zinsser Analytic GmbH - All rights reserved.
  Author       : Payman Kamali (pk)
  Description  : Dialog for entering properties of parser while and if statements
  --------------------------------------------------------------------------------------------------
  Revision History:
  date     op  method                           track-no  improvement/change
  -------- --  -------------------------------  --------- ----------------------------------------------
  27.07.07 pk                                   TN3786    initial version
  01.08.07 pk                                   TN3786    corrected: STR_PARSER_ENDIF
  27.08.07 pk                                   TN3788    New: Dataloop
  18.02.09 pk                                   TN4232    Calc actions changed to Dataset actions
  13.03.09 pk                                   TN4232    Functions now also return Options string
  10.08.09 wl                                   TN4702   Strings werden jetzt direkt geladen
  20.08.09 wl  fStringLoader                    TN4702   fStringLoader lädt Strings für Dialog-Elemente
  20.05.10 wl                                   TN5117   neue Schriftart "Segoe UI", Schriftgröße 9
  22.06.10 pk                                   TN5088   Now works with new While, If, etc actions instead of Calc Action
  08.08.12 wl                                   TN5946   uses geändert
  20.09.12 wl                                   TN5982   alles außer DataLoop entfernt
  21.03.13 wl                                   TN6045   an TCustomSetting-Änderungen angepasst
  -------------------------------------------------------------------------------------------------- }

unit ParserCodeGenerateDlg;


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
    ComCtrls,
    MethodStepDataFields,
    cxGraphics,
    cxCustomData,
    cxStyles,
    cxTL,
    cxDropDownEdit,
    cxTextEdit,
    cxInplaceContainer,
    cxControls,
    StringLoader,
    cxLookAndFeels,
    cxLookAndFeelPainters,
    MethodStep;

type
    TParserCodeGenerateDlgStringLoader = class(TTextListStringLoader)
    protected
        procedure AddAllItems(); override;
    end;

    TfrmParserCodeGenerateDlg = class(TForm)
        Shape2: TShape;
        Bevel2: TBevel;
        mmoHeaderDescription: TMemo;
        Bevel1: TBevel;
        Panel1: TPanel;
        btnNext: TButton;
        btnCancel: TButton;
        btnPreview: TButton;
        Panel2: TPanel;
        pgCtrl: TPageControl;
        shtDataLoop: TTabSheet;
        Label8: TLabel;
        edDataHandleName: TEdit;
        Label9: TLabel;
        Label10: TLabel;
        edDataFilter: TEdit;
        cmbDataDefName: TComboBox;
        cxTreeList1: TcxTreeList;
        cxTreeList1cxTreeListColumn2: TcxTreeListColumn;
        procedure btnPreviewClick(Sender: TObject);
        procedure FormCreate(Sender: TObject);
        procedure FormDestroy(Sender: TObject);
    private
        fStringLoader: TParserCodeGenerateDlgStringLoader;
        fDefaultDataLink: TMethodStepDataLink;

        function GetDataDefName: string;

        function GetDataTotalLinesIdentName(const aObjectIdentName: string): string;
        function GetDataLineCountIdentName(const aObjectIdentName: string): string;

        property DataDefName: string read GetDataDefName;

        function GetDataCondition(): string;
        procedure AddPreviewStr(const aDescription: string);
        procedure AddPreviewStrAndDestroy(var vMethodStep: TMethodStep);
    public
        function ShowDialog(): TModalResult;

        function CreateDataOpenLine(): TMethodStep;
        function CreateDataInitVarLine(): TMethodStep;
        function CreateDataBeginLoopLine(): TMethodStep;
        function CreateDataIncrementLine(): TMethodStep;
        function CreateDataReadLine(): TMethodStep;
        function CreateDataEndLoopLine(): TMethodStep;
        function CreateDataCloseLine(): TMethodStep;
    end;


implementation


{$R *.dfm}

uses
    GeneralTypes,
    MethodTypes,
    ControlUtils,
    ParserWrapperOptimal,
    ParserTokenizer,
    DatasetRunStepInfo,
    DatasetRunStepBuilder,
    ParamStoreRunStepBuilder,
    WhileRunStep;

{ TParserCodeGenerateDlgStringLoader }

procedure TParserCodeGenerateDlgStringLoader.AddAllItems;
begin
    AddSingle(44740, 'Insert Loop', 'Schleife hinzufügen');
    AddSingle(44810, 'Preview', 'Vorschau');
    AddSingle(44820, 'Cancel', 'Abbrechen');
    AddSingle(44860, 'Condition:', 'Bedingung:');
    AddSingle(44960, 'Import definition', 'Importdefinition');
    AddSingle(44970, 'Filter', 'Filter');
    AddSingle(44980, 'Data variable', 'Data-Variable');
end;

{ TfrmParserCodeGenerateDlg }

procedure TfrmParserCodeGenerateDlg.FormCreate(Sender: TObject);
begin
    TControlUtils.ResetFontForWinXP(self);
    fStringLoader := TParserCodeGenerateDlgStringLoader.Create;
    fStringLoader.LoadLanguage(self);

    fDefaultDataLink := TMethodStepDataLink.Create();
    fDefaultDataLink.InitWithDefaults();

    self.Caption := TLanguageString.Read('Insert data Loop', 'Data-Schleife hinzufügen');
    self.mmoHeaderDescription.Lines.Clear;
    self.mmoHeaderDescription.Lines.Add(TLanguageString.Read('Please define the properties of the data loop:',
        'Bitte definieren Sie die Data-Schleifeneigenschaften:'));
end;

procedure TfrmParserCodeGenerateDlg.FormDestroy(Sender: TObject);
begin
    fStringLoader.Free;
end;

function TfrmParserCodeGenerateDlg.ShowDialog(): TModalResult;
begin
    self.cxTreeList1.Clear;
    result := ShowModal();
end;

function TfrmParserCodeGenerateDlg.GetDataDefName: string;
begin
    result := self.cmbDataDefName.Text;
end;

function TfrmParserCodeGenerateDlg.GetDataTotalLinesIdentName(const aObjectIdentName: string): string;
const
    cDataTotalLinesSuffix = 'TotalLines';
begin
    result := aObjectIdentName + cDataTotalLinesSuffix;
end;

function TfrmParserCodeGenerateDlg.GetDataLineCountIdentName(const aObjectIdentName: string): string;
const
    cDataLineCountSuffix = 'LineCnt';
begin
    result := aObjectIdentName + cDataLineCountSuffix;
end;

function TfrmParserCodeGenerateDlg.CreateDataOpenLine(): TMethodStep;

var
    xDatasetObjectIdentName: string;
    xMethodStep: TDatasetOpenMethodStep;
begin
    xDatasetObjectIdentName := edDataHandleName.Text;

    xMethodStep := TDatasetOpenMethodStep.Create('', fDefaultDataLink);
    result := xMethodStep;

    xMethodStep.MainSubOptionSetting.DefName.Value := self.DataDefName;
    xMethodStep.MainSubOptionSetting.Filter.Value := self.edDataFilter.Text;
    xMethodStep.MainSubOptionSetting.DatasetObjectIdentName.Value := xDatasetObjectIdentName;
    xMethodStep.MainSubOptionSetting.RecordCountIdentName.Value :=
        GetDataTotalLinesIdentName(xDatasetObjectIdentName);
end;

function TfrmParserCodeGenerateDlg.CreateDataInitVarLine(): TMethodStep;
var
    xMethodStep: TParamStoreMethodStep;
    xDatasetObjectIdentName: string;
begin
    xDatasetObjectIdentName := edDataHandleName.Text;

    xMethodStep := TParamStoreMethodStep.Create('', fDefaultDataLink, nil);
    result := xMethodStep;
    xMethodStep.MainSubOptionSetting.StoreKey.Value := GetDataLineCountIdentName(xDatasetObjectIdentName);
    xMethodStep.MainSubOptionSetting.StoreValue.Value := IntToStr(1);
end;

function TfrmParserCodeGenerateDlg.GetDataCondition(): string;
var
    xDatasetObjectIdentName: string;
begin
    xDatasetObjectIdentName := edDataHandleName.Text;

    result := Format('%s <= %s', [GetDataLineCountIdentName(xDatasetObjectIdentName),
        GetDataTotalLinesIdentName(xDatasetObjectIdentName)]);
end;

function TfrmParserCodeGenerateDlg.CreateDataBeginLoopLine(): TMethodStep;
var
    xMethodStep: TWhileMethodStep;
begin
    xMethodStep := TWhileMethodStep.Create('', fDefaultDataLink, nil);
    result := xMethodStep;
    xMethodStep.MainSubOptionSetting.Condition.Value := self.GetDataCondition;
end;

function TfrmParserCodeGenerateDlg.CreateDataReadLine(): TMethodStep;
var
    xDatasetObjectIdentName: string;
    xMethodStep: TDatasetReadMethodStep;
begin
    xDatasetObjectIdentName := edDataHandleName.Text;

    xMethodStep := TDatasetReadMethodStep.Create('', fDefaultDataLink);
    result := xMethodStep;
    xMethodStep.MainSubOptionSetting.DatasetObject.Value := xDatasetObjectIdentName;
end;

function TfrmParserCodeGenerateDlg.CreateDataIncrementLine(): TMethodStep;
var
    xDatasetObjectIdentName: string;
    xMethodStep: TDatasetCursorMoveMethodStep;
begin
    xDatasetObjectIdentName := edDataHandleName.Text;

    xMethodStep := TDatasetCursorMoveMethodStep.Create('', fDefaultDataLink);
    result := xMethodStep;
    xMethodStep.MainSubOptionSetting.DatasetObject.Value := xDatasetObjectIdentName;
    xMethodStep.MainSubOptionSetting.IsRelativeMove.Value := STR_YES;
    xMethodStep.MainSubOptionSetting.MoveOffset.Value := IntToStr(1);
    xMethodStep.MainSubOptionSetting.RecordNumIdentName.Value :=
        GetDataLineCountIdentName(xDatasetObjectIdentName);
end;

function TfrmParserCodeGenerateDlg.CreateDataEndLoopLine(): TMethodStep;
begin
    result := TEndWhileMethodStep.Create('', fDefaultDataLink, nil);
end;

function TfrmParserCodeGenerateDlg.CreateDataCloseLine(): TMethodStep;
var
    xDatasetObjectIdentName: string;
    xMethodStep: TDatasetCloseMethodStep;
begin
    xDatasetObjectIdentName := edDataHandleName.Text;

    xMethodStep := TDatasetCloseMethodStep.Create('', fDefaultDataLink);
    result := xMethodStep;
    xMethodStep.MainSubOptionSetting.DatasetObject.Value := xDatasetObjectIdentName;
end;

procedure TfrmParserCodeGenerateDlg.AddPreviewStr(const aDescription: string);
var
    xNode: TcxTreeListNode;
begin
    xNode := self.cxTreeList1.Add;

    xNode.Texts[0] := aDescription;
end;

procedure TfrmParserCodeGenerateDlg.AddPreviewStrAndDestroy(var vMethodStep: TMethodStep);
begin
    AddPreviewStr(vMethodStep.GetSummary());
    FreeAndNil(vMethodStep);
end;

procedure TfrmParserCodeGenerateDlg.btnPreviewClick(Sender: TObject);
var
    xMethodStep: TMethodStep;
begin
    self.cxTreeList1.Clear();

    xMethodStep := CreateDataOpenLine();
    AddPreviewStrAndDestroy(xMethodStep);

    xMethodStep := CreateDataInitVarLine();
    AddPreviewStrAndDestroy(xMethodStep);

    xMethodStep := CreateDataBeginLoopLine();
    AddPreviewStrAndDestroy(xMethodStep);

    xMethodStep := CreateDataReadLine();
    AddPreviewStrAndDestroy(xMethodStep);

    AddPreviewStr('...');

    xMethodStep := CreateDataIncrementLine();
    AddPreviewStrAndDestroy(xMethodStep);

    xMethodStep := CreateDataEndLoopLine();
    AddPreviewStrAndDestroy(xMethodStep);

    xMethodStep := CreateDataCloseLine();
    AddPreviewStrAndDestroy(xMethodStep);
end;


end.
