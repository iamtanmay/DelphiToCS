{ --------------------------------------------------------------------------------------------------
  Copyright © 2006 Zinsser Analytic GmbH - All rights reserved.
  Author       : Payman Kamali (pk)
  Description  : A frame for selecting a tipmap. This is already used in :
  1. Flush dialog
  --------------------------------------------------------------------------------------------------
  Revision History:
  date     op  method                       track-no improvement/change
  -------- --  ---------------------------  -------- -----------------------------------------------
  24.04.06 pk                                TN3067   initial version
  23.05.06 pk                                TN3112   Various changes
  13.03.07 pk                                TN3633   Everything is now based on PipDevice instead of ArmDevice
  13.03.07 pk  SetTotalTipIndex              TN3633   New
  24.06.08 wl                                TN4143   uses geändert
  03.07.08 wl                                         TN4157
  19.09.08 pk  stPipDeviceName               TN4215   removed
  06.07.09 pk                                TN4585.4 uses ObjModul removed
  20.08.09 wl                                TN4702   Strings werden jetzt direkt geladen
  28.08.09 pk                                TN4753   references to PipDevice removed
  10.02.11 wl                                TN5475   TfraTipMapSelect ist TForm statt TFrame
  04.12.12 wl                                TN6044   Schrift geändert
  -------------------------------------------------------------------------------------------------- }

unit TipMapSelect;


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
    cxStyles,
    cxCustomData,
    cxGraphics,
    cxFilter,
    cxData,
    cxDataStorage,
    cxEdit,
    DB,
    cxDBData,
    cxExtEditRepositoryItems,
    cxEditRepositoryItems,
    cxGridCustomTableView,
    cxGridTableView,
    cxGridDBTableView,
    StdCtrls,
    cxGridLevel,
    cxClasses,
    cxControls,
    cxGridCustomView,
    cxGrid,
    Menus,
    AppTypes,
    cxLookAndFeels,
    cxLookAndFeelPainters;

type
    TfraTipMapSelect = class(TForm)
        cxTipGridLevel1: TcxGridLevel;
        cxTipGrid: TcxGrid;
        cxEditRepository1: TcxEditRepository;
        cxEditRepository1CheckBoxItem1: TcxEditRepositoryCheckBoxItem;
        cxEditRepository1TextItem1: TcxEditRepositoryTextItem;
        cxTipGridTableView1: TcxGridTableView;
        cxTipGridTableView1Column1: TcxGridColumn;
        cxTipGridTableView1Column2: TcxGridColumn;
        pmnuTips: TPopupMenu;
        SelectAll1: TMenuItem;
        procedure SelectAll1Click(Sender: TObject);
        procedure cxEditRepository1CheckBoxItem1PropertiesEditValueChanged(Sender: TObject);
    private
        fPipDeviceName: string;
        fTipCount: integer;
        fOnTipMapChanged: TNotifyEvent;
        procedure UpdateGrid();
        procedure ClearGrid();
        function GetTipMapFromGUI(): TIPMAP;
        procedure SetTipMapToGUI(aTipMap: TIPMAP);
        procedure TipMapChanged();
    public
        { Public declarations }
        procedure Init();

        function GetTipMap(): TIPMAP;
        procedure SetTipMap(const aPipDeviceName: string; const aTipCount: integer; const aTipMap: TIPMAP);
        property OnTipMapChanged: TNotifyEvent read fOnTipMapChanged write fOnTipMapChanged;
        property PipDeviceName: string read fPipDeviceName;
    end;


implementation


{$R *.dfm}

uses
    TipMapUtils,
    ControlUtils,
    GeneralTypes;

const
    INT_COL_INDEX_TIPSELECTED = 0;
    INT_COL_INDEX_TIPNAME = 1;

procedure TfraTipMapSelect.Init();
begin
    TControlUtils.ResetFontForWinXP(self);

    SelectAll1.Caption := TLanguageString.Read('&Select All', '&Alles markieren');

    fPipDeviceName := '';
    cxTipGridTableView1.Columns[INT_COL_INDEX_TIPSELECTED].Caption := '';
    cxTipGridTableView1.Columns[INT_COL_INDEX_TIPNAME].Caption := 'Tip';
    cxTipGridTableView1.Columns[INT_COL_INDEX_TIPSELECTED].RepositoryItem :=
        self.cxEditRepository1CheckBoxItem1;
    cxTipGridTableView1.Columns[INT_COL_INDEX_TIPNAME].RepositoryItem := self.cxEditRepository1TextItem1;

    // this is important - otherwise the change event is fired BEFORE the datacontroller.values array is updated instead of AFTER
    cxEditRepository1CheckBoxItem1.Properties.ImmediatePost := true;
end;

procedure TfraTipMapSelect.TipMapChanged();
begin
    if Assigned(fOnTipMapChanged) then
        fOnTipMapChanged(self);
end;

function TfraTipMapSelect.GetTipMapFromGUI(): TIPMAP;
var
    x: integer;
    xTipIndex: integer;
begin
    result := gmEmptyTipmap();
    for x := 0 to cxTipGridTableView1.DataController.RecordCount - 1 do
    begin
        if not cxTipGridTableView1.DataController.Values[x, INT_COL_INDEX_TIPSELECTED] then
            CONTINUE;
        xTipIndex := cxTipGridTableView1.DataController.Values[x, INT_COL_INDEX_TIPNAME] - 1;
        gmSelectTip(result, xTipIndex);
    end;
end;

function TfraTipMapSelect.GetTipMap(): TIPMAP;
begin
    result := GetTipMapFromGUI();
end;

procedure TfraTipMapSelect.SetTipMapToGUI(aTipMap: TIPMAP);
var
    x: integer;
begin
    if fPipDeviceName = '' then
        EXIT;
    // Select the check boxes
    for x := 0 to fTipCount - 1 do
    begin
        cxTipGridTableView1.DataController.Values[x, INT_COL_INDEX_TIPSELECTED] := gmTipSelected(aTipMap, x);
    end;
end;

procedure TfraTipMapSelect.SetTipMap(const aPipDeviceName: string; const aTipCount: integer;
    const aTipMap: TIPMAP);
begin
    fPipDeviceName := aPipDeviceName;
    fTipCount := aTipCount;

    UpdateGrid();
    TipMapChanged();

    SetTipMapToGUI(aTipMap);
end;

procedure TfraTipMapSelect.ClearGrid();
var
    x: integer;
begin
    for x := cxTipGridTableView1.DataController.RecordCount - 1 downto 0 do
    begin
        cxTipGridTableView1.DataController.DeleteRecord(x);
    end;
end;

procedure TfraTipMapSelect.UpdateGrid();
var
    x: integer;
begin
    ClearGrid();

    if fPipDeviceName = '' then
        EXIT;

    for x := 0 to fTipCount - 1 do
    begin
        cxTipGridTableView1.DataController.InsertRecord(x);
        cxTipGridTableView1.DataController.Values[x, INT_COL_INDEX_TIPSELECTED] := true;
        cxTipGridTableView1.DataController.Values[x, INT_COL_INDEX_TIPNAME] := x + 1;
    end;
end;

procedure TfraTipMapSelect.SelectAll1Click(Sender: TObject);
var
    x: integer;
begin
    for x := 0 to cxTipGridTableView1.DataController.RecordCount - 1 do
    begin
        cxTipGridTableView1.DataController.Values[x, INT_COL_INDEX_TIPSELECTED] := true;
    end;
end;

procedure TfraTipMapSelect.cxEditRepository1CheckBoxItem1PropertiesEditValueChanged(Sender: TObject);
begin
    TipMapChanged();
end;


end.
