{ --------------------------------------------------------------------------------------------------
  Copyright © 2008 Zinsser Analytic GmbH - All rights reserved.
  Author       : Payman Kamali (pk), Wolfgang Lyncke (wl)
  Description  :
  --------------------------------------------------------------------------------------------------
  Revision History:
  date     op  method                       track-no improvement/change
  -------- --  ---------------------------  -------- -----------------------------------------------
  07.07.08 pk                                TN4139  Initial Revision. Code from TCarrier
  10.03.09 pk                                TN4457  Changes needed for reimplementing Stacker Buttons
  16.03.09 pk                                TN4463  Made more flexible
  06.09.10 wl  StackerGridKeyDown            TN5256   Auswahl ist jetzt auch über RETURN-Taste möglich
  21.09.10 wl  StackerGridKeyDown,-MouseUp   TN5256   Bei gedrückter Alt-Taste keine Auswahl
  10.04.13 wl                                TN6045   uses geändert
  -------------------------------------------------------------------------------------------------- }

unit StackerLevelSelectorGraphics;


interface


uses
    Forms,
    Grids,
    Controls,
    Classes,
    StdCtrls,
    ExtCtrls,
    Buttons,
    LayoutElementCallbackTypes;

type
    // TStackerLevelChosenCallback = procedure( aStackerChosenSlot : integer; const aStackerChosenRack : string ) of object;
    TStackerLevelInfo = record
        name: string;
        SlotNr: integer;
    end;

    TGetStackerLevelInfoArray = function(aSender: TObject): TArray<TStackerLevelInfo> of object;

    TStackerLevelSelectorGraphics = class
    private
        fStackerName: string;
        fChosenRack: string;
        fChosenSlot: integer;
        fOnGetLevelInfoArray: TGetStackerLevelInfoArray;
        fRows, fCols, fFloors: integer;
        fLeft, fTop: integer;
        fDragOverGrid: TMouseDragOverCallback;
        fDragDropGrid: TMouseDragDropCallback;
        fForm: TForm;
        fStackerGrid: TStringGrid;
        procedure PrepareStackerGrid();
        procedure PrepareStackerOverviewDialog;
        procedure StackerGridMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState;
            X, Y: Integer);
        procedure DoDragOverGrid(aSender, aSource: TObject; aX, aY: integer; aState: TDragState;
            var vAccept: boolean);
        procedure DoDragDropGrid(aSender, aSource: TObject; aX, aY: integer);
    protected
        procedure FillGrid();
        procedure StackerGridMouseDown(aSender: TObject; aButton: TMouseButton; aShift: TShiftState;
            aX, aY: Integer); virtual;
        procedure StackerGridKeyDown(aSender: TObject; var aKey: Word; aShift: TShiftState); virtual;
        function ChooseSelected(): boolean;
        function ChooseByMousePos(aX, aY: integer): boolean;
    public
        constructor Create();
        destructor Destroy(); override;
        procedure Hide();
        procedure Show;
        procedure ShowModal;
        procedure SetInfo(const aName: string; aOnGetLevelInfoArray: TGetStackerLevelInfoArray;
            aRows, aCols, aFloors, aLeft, aTop: integer; aDragOverGrid: TMouseDragOverCallback;
            aDragDropGrid: TMouseDragDropCallback);

        property StackerName: string read fStackerName;
        property ChosenSlot: integer read fChosenSlot;
        property ChosenRack: string read fChosenRack;
    end;


implementation


uses
    Windows,
    SysUtils,
    Math,
    Types;

constructor TStackerLevelSelectorGraphics.Create();
begin
    inherited Create();

    fForm := nil;

    // create & initialize stackform window
    fForm := TForm.Create(nil);
    fForm.Visible := false;

    fForm.BorderStyle := bsSizeable;
    fForm.BorderIcons := [biSystemMenu];

    fStackerGrid := TStringGrid.Create(fForm);
    fStackerGrid.Parent := fForm;
    fStackerGrid.Align := alClient;
    fStackerGrid.DefaultColWidth := 160;
    fStackerGrid.DefaultRowHeight := 18;
    fStackerGrid.FixedCols := 0;
    fStackerGrid.FixedRows := 0;
    fStackerGrid.OnMouseUp := StackerGridMouseUp;
    fStackerGrid.OnMouseDown := StackerGridMouseDown;
    fStackerGrid.OnKeyDown := StackerGridKeyDown;
end;

procedure TStackerLevelSelectorGraphics.SetInfo(const aName: string;
    aOnGetLevelInfoArray: TGetStackerLevelInfoArray; aRows, aCols, aFloors, aLeft, aTop: integer;
    aDragOverGrid: TMouseDragOverCallback; aDragDropGrid: TMouseDragDropCallback);
begin
    fOnGetLevelInfoArray := aOnGetLevelInfoArray;
    fStackerName := aName;
    fRows := aRows;
    fCols := aCols;
    fFloors := aFloors;
    fLeft := aLeft;
    fTop := aTop;
    fDragOverGrid := aDragOverGrid;
    fDragDropGrid := aDragDropGrid;

    PrepareStackerOverviewDialog();
end;

destructor TStackerLevelSelectorGraphics.Destroy();
begin
    fForm.Free;
    inherited;
end;

function TStackerLevelSelectorGraphics.ChooseSelected: boolean;
var
    xCol, xRow: integer;
begin
    if not Assigned(fStackerGrid) then
        EXIT(false);

    xCol := fStackerGrid.Selection.Left;
    xRow := fStackerGrid.Selection.Top;

    // Übergabe von (evtl.) Rackname und Slot-Nr
    fChosenRack := (fStackerGrid).Cells[xCol, xRow];
    fChosenSlot := (fStackerGrid).Row * (fStackerGrid).ColCount + xCol + 1;
    EXIT(true);
end;

function TStackerLevelSelectorGraphics.ChooseByMousePos(aX, aY: integer): boolean;
var
    xCol, xRow: integer;
begin
    result := false;
    if not Assigned(fStackerGrid) then
        EXIT;
    fStackerGrid.Mousetocell(aX, aY, xCol, xRow);
    if (xCol > -1) and (xCol < (fStackerGrid).ColCount) and (xRow > -1) and
        (xRow < (fStackerGrid).RowCount) then
    begin

        // Übergabe von (evtl.) Rackname und Slot-Nr
        fChosenRack := (fStackerGrid).Cells[xCol, xRow];
        fChosenSlot := (fStackerGrid).Row * (fStackerGrid).ColCount + xCol + 1;
        result := true;
    end;
end;

procedure TStackerLevelSelectorGraphics.StackerGridKeyDown(aSender: TObject; var aKey: Word;
    aShift: TShiftState);
begin
    if (aKey = VK_RETURN) and (not(ssCtrl in aShift)) and (not(ssAlt in aShift)) then
    begin
        if not ChooseSelected() then
            EXIT;
        self.Hide();
    end;
end;

procedure TStackerLevelSelectorGraphics.StackerGridMouseDown(aSender: TObject; aButton: TMouseButton;
    aShift: TShiftState; aX, aY: Integer);
begin
    // nur bei Edit benötigt
end;

procedure TStackerLevelSelectorGraphics.StackerGridMouseUp(Sender: TObject; Button: TMouseButton;
    Shift: TShiftState; X, Y: Integer);
begin
    if (Sender = fStackerGrid) and (Button = mbLeft) and (not(ssCtrl in Shift)) and (not(ssAlt in Shift)) then
    begin
        if not ChooseByMousePos(X, Y) then
            EXIT;
        self.Hide();

    end;
end;

procedure TStackerLevelSelectorGraphics.PrepareStackerGrid();
var
    xStackerHeight: integer;
begin

    fStackerGrid.ColCount := fRows * fCols;
    fStackerGrid.RowCount := fFloors;
    fStackerGrid.ScrollBars := ssVertical;
    fStackerGrid.Options := [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine];
    fForm.Width := ((fStackerGrid.DefaultColwidth + fStackerGrid.gridlinewidth) * fStackerGrid.ColCount) + 30;
    xStackerHeight := ((fStackerGrid.DefaultRowHeight + fStackerGrid.gridlinewidth) *
        fStackerGrid.RowCount) + 45;
    xStackerHeight := Min(Application.MainForm.ClientHeight - fForm.Top - 10, xStackerHeight);
    fForm.Height := xStackerHeight;
    fStackerGrid.OnDragOver := DoDragOverGrid;
    fStackerGrid.OnDragDrop := DoDragDropGrid;
end;

procedure TStackerLevelSelectorGraphics.FillGrid();
var
    x, xActCol, xActRow, xSlotIndex: integer;
    xLevelInfoArray: TArray<TStackerLevelInfo>;
begin
    if Assigned(fOnGetLevelInfoArray) then
        xLevelInfoArray := fOnGetLevelInfoArray(self)
    else
        xLevelInfoArray := nil;

    // Racks in StringGrid eintragen
    for x := 0 to high(xLevelInfoArray) do
    begin
        xSlotIndex := xLevelInfoArray[x].SlotNr - 1;
        xActCol := xSlotIndex mod fStackerGrid.ColCount;
        xActRow := xSlotIndex div fStackerGrid.ColCount;

        if (xActRow >= 0) and (xActRow < fStackerGrid.RowCount) and (xActCol >= 0) and
            (xActCol < fStackerGrid.ColCount) then
            fStackerGrid.Cells[xActCol, xActRow] := xLevelInfoArray[x].Name;
    end;

end;

procedure TStackerLevelSelectorGraphics.DoDragDropGrid(aSender, aSource: TObject; aX, aY: integer);
begin
    if not Assigned(fDragOverGrid) then
        EXIT;
    fDragDropGrid(aSender, aSource, aX, aY);
end;

procedure TStackerLevelSelectorGraphics.DoDragOverGrid(aSender, aSource: TObject; aX, aY: integer;
    aState: TDragState; var vAccept: boolean);
begin
    if not Assigned(fDragOverGrid) then
        EXIT;
    fDragOverGrid(aSender, aSource, aX, aY, TGraphicsDragState(aState), vAccept);
end;

procedure TStackerLevelSelectorGraphics.PrepareStackerOverviewDialog();
begin

    fForm.Caption := Format('Stacker %s', [fStackerName]);
    // TResLoader.GetResFString(28000{Stacker %s}, [FName]);
    fForm.Left := fLeft; // xOriginPoint.x;
    fForm.Top := fTop; // xOriginPoint.y + fGraphics.SizeY;
    self.PrepareStackerGrid();

    // Racks in StringGrid eintragen
    FillGrid();

    fChosenSlot := 0;
    fChosenRack := '';
end;

procedure TStackerLevelSelectorGraphics.Show();
begin
    fForm.Show();
end;

procedure TStackerLevelSelectorGraphics.ShowModal();
begin
    fForm.ShowModal();
end;

procedure TStackerLevelSelectorGraphics.Hide;
begin
    fForm.Close;
end;


end.
