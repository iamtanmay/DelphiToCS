{ ------------------------------------------------------------------------------------------------------------
  Copyright © 2008 Zinsser Analytic GmbH - All rights reserved.
  Author       : Wolfgang Lyncke (wl)
  Description  : Parameter editor for Run step INPUT
  ------------------------------------------------------------------------------------------------------------
  Revision History:
  date     op  method                                track-no  improvement/change
  -------- --  ------------------------------------- --------  -----------------------------------------------
  17.11.08 wl                                        TN4310    initial version
  25.11.08 wl                                        TN4310    ist jetzt funktionsfähig
  08.12.08 wl                                        TN4310    File und Path waren vertauscht
  10.12.08 pk                                        TN4310    EditType Item "Default" value changed to 0
  10.12.08 pk  pmnuAppendClick                       TN4310    Focus and edit first column
  10.12.08 pk  pmnuAppendClick                       TN4310    set EditType value to default
  20.05.10 wl                                        TN5117   neue Schriftart "Segoe UI", Schriftgröße 9
  21.06.10 wl                               TN5160   Position = poScreenCenter
  14.12.12 wl                                        TN6054   an Änderungen in ParserIdent angepasst
  20.02.13 wl                                        TN6055   uses MethodVariableTypes
  ------------------------------------------------------------------------------------------------------------ }

unit DialogInputRunStepEditor;


interface


uses
    Classes,
    Controls,
    Forms,
    cxStyles,
    cxCustomData,
    cxGraphics,
    cxFilter,
    cxData,
    cxDataStorage,
    cxEdit,
    cxEditRepositoryItems,
    Menus,
    cxGridLevel,
    cxGridCustomTableView,
    cxGridTableView,
    cxClasses,
    cxControls,
    cxGridCustomView,
    cxGrid,
    StdCtrls,
    ExtCtrls,
    cxLookAndFeels,
    cxLookAndFeelPainters;

type
    TfrmDialogInputRunStepEditor = class(TForm)
        pnlBottom: TPanel;
        btnOK: TButton;
        btnCancel: TButton;
        cxGrid1: TcxGrid;
        cxGrid1TableView1: TcxGridTableView;
        cxGrid1TableView1Column1: TcxGridColumn;
        cxGrid1TableView1Column2: TcxGridColumn;
        cxGrid1TableView1Column3: TcxGridColumn;
        cxGrid1TableView1Column4: TcxGridColumn;
        cxGrid1Level1: TcxGridLevel;
        PopupMenu1Grid: TPopupMenu;
        pmnuAppend: TMenuItem;
        N1: TMenuItem;
        pmnuDelete: TMenuItem;
        cxGrid1TableView1Column5: TcxGridColumn;
        cxGrid1TableView1Column6: TcxGridColumn;
        cxGrid1TableView1Column7: TcxGridColumn;
        cxEditRepository1: TcxEditRepository;
        cxEditRepository1ComboBoxItem1: TcxEditRepositoryComboBoxItem;
        cxEditRepository1RadioGroupItem1: TcxEditRepositoryRadioGroupItem;
        procedure pmnuDeleteClick(Sender: TObject);
        procedure pmnuAppendClick(Sender: TObject);
        procedure cxEditRepository1ButtonItem1PropertiesButtonClick(Sender: TObject; AButtonIndex: Integer);
        procedure FormCreate(Sender: TObject);
    private const
        cColumnKey = 0;
        cColumnDescription = 1;
        cColumnDefaultValue = 2;
        cColumnMinValue = 3;
        cColumnMaxValue = 4;
        cColumnEditType = 5;
        cColumnDropdownList = 6;
        cEditTypeDefault = '0';
    private
        function GetValue: string;
        procedure SetValue(const Value: string);
        function GetStringValue(aRecIndex, aColumnIndex: integer): string;
    public
        property Value: string read GetValue write SetValue;
    end;


implementation


{$R *.dfm}

uses
    Dialogs,
    SysUtils,
    Variants,
    ControlUtils,
    MethodVariableTypes,
    MethodGUIParsing,
    MethodTypes;

procedure TfrmDialogInputRunStepEditor.pmnuDeleteClick(Sender: TObject);
var
    x: integer;
begin
    if (cxGrid1TableView1.Controller.SelectedRecordCount < 0) then
        EXIT;

    for x := cxGrid1TableView1.Controller.SelectedRecordCount - 1 downto 0 do
        cxGrid1TableView1.DataController.DeleteRecord(cxGrid1TableView1.Controller.SelectedRecords[x]
            .RecordIndex);

    cxGrid1TableView1.DataController.ClearSelection;
end;

procedure TfrmDialogInputRunStepEditor.pmnuAppendClick(Sender: TObject);
var
    xIndex: integer;
begin
    xIndex := cxGrid1TableView1.DataController.AppendRecord;
    cxGrid1TableView1.DataController.FocusedRecordIndex := xIndex;

    // 10.12.08 pk This is important! atleast one of the paramters has to be set otherwise the whole line is not saved
    // so we set a value for the EditType parameter just to be safe.
    cxGrid1TableView1.DataController.Values[xIndex, cColumnEditType] := cEditTypeDefault;

    // select row
    cxGrid1TableView1.Controller.ClearSelection();
    cxGrid1TableView1.DataController.ChangeRowSelection(xIndex, true);
    // select col
    cxGrid1TableView1.VisibleColumns[cColumnKey].Focused := true;

    // set editing to true to create an Edit control
    cxGrid1TableView1.Controller.FocusedItem.Editing := true;
end;

procedure TfrmDialogInputRunStepEditor.cxEditRepository1ButtonItem1PropertiesButtonClick(Sender: TObject;
    AButtonIndex: Integer);
var
    xText: string;

    xOpenDialog: TOpenDialog;
begin
    xText := self.GetStringValue(AButtonIndex, 6);
    xOpenDialog := TOpenDialog.Create(nil);
    if Pos(TMethodVariableUtils.cIdentPicklistPath, xText) = 1 then
        xOpenDialog.InitialDir := ExtractFileDir(Copy(xText, 7, Length(xText)));
    xOpenDialog.Execute;
    if (xOpenDialog.FileName = '') then
        EXIT;

    cxGrid1TableView1.DataController.Values[AButtonIndex, cColumnDropdownList] :=
        TMethodVariableUtils.cIdentPicklistPath + xOpenDialog.FileName;

    xOpenDialog.Free;
end;

procedure TfrmDialogInputRunStepEditor.FormCreate(Sender: TObject);
begin
    TControlUtils.ResetFontForWinXP(self);
end;

function TfrmDialogInputRunStepEditor.GetStringValue(aRecIndex, aColumnIndex: integer): string;
begin
    result := ''; // Variant-Value ist Null
    if (cxGrid1TableView1.DataController.Values[aRecIndex, aColumnIndex] = Null) then
        EXIT;
    result := cxGrid1TableView1.DataController.Values[aRecIndex, aColumnIndex];
end;

function TfrmDialogInputRunStepEditor.GetValue: string;
var
    xParameters: TDialogInputKeyParamArray;
    x: integer;
begin
    SetLength(xParameters, cxGrid1TableView1.DataController.RecordCount);
    for x := 0 to cxGrid1TableView1.DataController.RecordCount - 1 do
    begin
        xParameters[x].Key := self.GetStringValue(x, cColumnKey);
        xParameters[x].V.Description := self.GetStringValue(x, cColumnDescription);
        xParameters[x].V.DefaultValue := self.GetStringValue(x, cColumnDefaultValue);
        xParameters[x].V.MinValue := self.GetStringValue(x, cColumnMinValue);
        xParameters[x].V.MaxValue := self.GetStringValue(x, cColumnMaxValue);
        xParameters[x].V.EditType := self.GetStringValue(x, cColumnEditType);
        xParameters[x].V.DropdownList := self.GetStringValue(x, cColumnDropdownList);
    end;
    result := TMethodGUIParser.DialogInputKeyParamArrayToStr(xParameters);
end;

procedure TfrmDialogInputRunStepEditor.SetValue(const Value: string);
var
    xParameters: TDialogInputKeyParamArray;
    x: integer;
    xRecIndex: integer;
begin
    xParameters := TMethodGUIParser.DialogInputKeyParamArrayFromStr(Value);
    for x := 0 to high(xParameters) do
    begin
        xRecIndex := cxGrid1TableView1.DataController.AppendRecord;
        cxGrid1TableView1.DataController.Values[xRecIndex, cColumnKey] := xParameters[x].Key;
        cxGrid1TableView1.DataController.Values[xRecIndex, cColumnDescription] :=
            xParameters[x].V.Description;
        cxGrid1TableView1.DataController.Values[xRecIndex, cColumnDefaultValue] :=
            xParameters[x].V.DefaultValue;
        cxGrid1TableView1.DataController.Values[xRecIndex, cColumnMinValue] := xParameters[x].V.MinValue;
        cxGrid1TableView1.DataController.Values[xRecIndex, cColumnMaxValue] := xParameters[x].V.MaxValue;
        cxGrid1TableView1.DataController.Values[xRecIndex, cColumnEditType] := xParameters[x].V.EditType;
        cxGrid1TableView1.DataController.Values[xRecIndex, cColumnDropdownList] :=
            xParameters[x].V.DropdownList;
    end;
end;


end.
