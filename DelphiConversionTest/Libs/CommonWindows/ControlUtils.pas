{ ------------------------------------------------------------------------------------------------------------
  BASEUNIT!
  ---------
  Copyright © 2010 Zinsser Analytic GmbH - All rights reserved.
  Author       : Wolfgang Lyncke (wl)
  Description  :
  ------------------------------------------------------------------------------------------------------------
  Revision History:
  date     op  method                            track-no improvement/change
  -------- --  --------------------------------  -------- ----------------------------------------------------
  21.07.10 wl                                    TN5202   initial revision
  21.07.10 wl  ResetFontForWinXP                 TN5202   setzt für Windows XP Schriftartz auf Tahoma zurück
  30.09.10 pk  AutoAdjustMemoHeight              TN4989   New
  02.10.12 wl  ResetFontForWinXP                 TN5960   verwendet TCustomForm
  ------------------------------------------------------------------------------------------------------------ }

unit ControlUtils;


interface


uses
    Forms,
    Classes,
    Grids,
    Controls,
    StdCtrls,
    ComCtrls,
    GeneralTypes,
    ListClasses,
    GenericTree;

type
    TEnControlProp = (cpVisible, cpReadOnly, cpEnabled, cpEnabledSpecial, cpColor, cpClose);

    TControlUtils = class // TWinControl Utilities
    private
        class procedure AddValuesToStrings(const aValues: array of string; const aStrings: TStrings;
            const aClear: boolean);
        class function GetValuesFromStrings(const aStrings: TStrings): TStringArray;
        class procedure AddTreeNodeListToTreeNodesRecursive(const aTreeNodeList: TStringTreeNodeList;
            const aTreeNodes: TTreeNodes; const aParentNode: TTreeNode);
    public
        class procedure OptimizeGridColumnSizes(aGrid: TStringGrid; aFirstRow: integer = 0);
        class function SelectComboBoxEntry(aComboBox: TComboBox; aComboBoxEntry: string): string;
        class procedure InsertIntoControlAtCurrentCursorPos(aControl: TCustomEdit; aTextToAdd: string);
        class procedure SetGroupEnabled(aGroupBox: TGroupBox; aValue: boolean);
        class procedure SetControlMemberProperty(aControl: TControl; aProperty: TEnControlProp;
            aValue: variant);
        class procedure SetControlProperty(aControl: TControl; aProperty: TEnControlProp; aValue: variant);
        class procedure AddLinesToMemo(aMemo: TMemo; aLines: array of string); overload;
        class procedure AddLineToMemo(aMemo: TMemo; aLine: string);

        class procedure AddValuesToTabControl(const aValues: array of string; const aTabControl: TTabControl);
        class procedure AddValuesToListBox(const aValues: array of string; const aListBox: TListBox;
            const aClear: boolean);
        class procedure AddValuesToComboBox(const aValues: array of string; const aComboBox: TComboBox;
            const aClear: boolean);
        class procedure AddValuesToMemo(const aValues: array of string; const aMemo: TMemo;
            const aClear: boolean);
        class procedure AddValuesToGridRow(const aValues: array of string; const aGrid: TStringGrid;
            const aRow: integer);

        class procedure AddTreeNodeListToTreeNodes(const aTree: TStringTree; const aTreeNodes: TTreeNodes);
        class function GetValuesFromMemo(const aMemo: TMemo): TStringArray;
        class procedure AutoAdjustMemoHeight(const aMemo: TMemo);
        class procedure ResetFontForWinXP(aForm: TCustomForm);
    end;


implementation


uses
    Windows,
    Graphics,
    SysUtils,
    UtilLib;

{ TControlUtils }

class procedure TControlUtils.OptimizeGridColumnSizes(aGrid: TStringGrid; aFirstRow: integer = 0);
const
    INT_FONT_SIZE = 8;
    INT_FONT_OFFSET = 10;

var
    i, j: integer;
    xLen, xMaxLen: integer;
    xText: string;

begin
    for i := 0 to aGrid.ColCount - 1 do
    begin
        xMaxLen := 0;
        for j := aFirstRow to aGrid.RowCount - 1 do
        begin
            xText := aGrid.Cells[i, j];
            xLen := Length(xText);
            if xLen > xMaxLen then
                xMaxLen := xLen;
        end;
        aGrid.ColWidths[i] := xMaxLen * INT_FONT_SIZE + INT_FONT_OFFSET;
    end;
end;

class procedure TControlutils.AutoAdjustMemoHeight(const aMemo: TMemo);
var
    aDC: hDC;
    R: TRect;
    aCanvas: TCanvas;
begin
    R := aMemo.BoundsRect;
    aMemo.Lines.BeginUpdate;
    try
        aMemo.Text := trim(aMemo.Text);
        aDC := GetWindowDC(aMemo.Handle);
        if aDC <> 0 then
            try
                aCanvas := TCanvas.Create;
                try
                    aCanvas.Handle := aDC;
                    aCanvas.Font.Assign(aMemo.Font);
                    DrawText(aCanvas.Handle, PChar(aMemo.Text), -1, R, DT_NOPREFIX or DT_CALCRECT or
                        DT_WORDBREAK);
                finally
                    aCanvas.Free;
                end;
            finally
                ReleaseDC(aMemo.Handle, aDC);
            end;
    finally
        aMemo.Lines.EndUpdate;
    end;
    aMemo.Height := R.Bottom - R.Top + 6;
end;

class procedure TControlUtils.ResetFontForWinXP(aForm: TCustomForm);
begin
    if TOSInfo.VersionIsVistaOrHigher then
        EXIT;

    // für Windows XP sieht Tahoma einfach besser aus
    aForm.Font.Name := 'Tahoma';
    aForm.Font.Size := 8;
end;

class function TControlUtils.SelectComboBoxEntry(aComboBox: TComboBox; aComboBoxEntry: string): string;
// select the given entry from the combo box.  If the entry does not exists nothing
// will be selected (i.e. itemindex = -1 )
// if the entry is found the result is the sames as the argument aComboBoxEntry
// if no entry found result = ''
// this is useful when you have a combo box with the csDropDownList Style
begin
    aComboBox.ItemIndex := aComboBox.Items.IndexOf(aComboBoxEntry);
    result := aComboBox.Text;
end;

class procedure TControlUtils.InsertIntoControlAtCurrentCursorPos(aControl: TCustomEdit; aTextToAdd: string);
var
    xSelStart: integer;
    xText: string;
begin
    xSelStart := aControl.SelStart;
    xText := aControl.Text;
    Insert(aTextToAdd, xText, xSelStart + 1);
    aControl.Text := xText;
    aControl.SetFocus;
    aControl.SelStart := xSelStart + Length(aTextToAdd);
end;

class procedure TControlUtils.SetGroupEnabled(aGroupBox: TGroupBox; aValue: boolean);
var
    i: integer;
begin
    for i := 0 to aGroupBox.ControlCount - 1 do
    begin
        aGroupBox.Controls[i].Enabled := aValue;
    end;
end;

class procedure TControlUtils.SetControlMemberProperty(aControl: TControl; aProperty: TEnControlProp;
    aValue: variant);
var
    i: integer;
    curCont: TControl;
begin
    if aControl.InheritsFrom(TWinControl) then
    begin
        with aControl as TWinControl do
        begin
            if ControlCount = 0 then
            begin
                SetControlProperty(aControl, aProperty, aValue);
                Exit;
            end;
            for i := 0 to ControlCount - 1 do
            begin
                curCont := TControl(Controls[i]);
                SetControlMemberProperty(curCont, aProperty, aValue);
            end;
        end;
    end;
    SetControlProperty(aControl, aProperty, aValue);
end;

class procedure TControlUtils.SetControlProperty(aControl: TControl; aProperty: TEnControlProp;
    aValue: variant);
begin
    if not aControl.InheritsFrom(TControl) then
        Exit;
    case aProperty of
        cpEnabled:
            begin
                try
                    aControl.Enabled := aValue;
                except
                end;
            end;
        cpEnabledSpecial:
            begin
                try
                    aControl.Enabled := aValue;
                except
                end;

                if (aControl is TComboBox) then
                begin
                    if aValue then
                        (aControl as TComboBox).Color := clWindow else (aControl as TComboBox).Color :=
                            clBtnFace;
                end;
                if (aControl is TEdit) then
                begin
                    if aValue then
                        (aControl as TEdit).Color := clWindow else (aControl as TEdit).Color := clBtnFace;
                end;
            end;
        cpVisible:
            begin
                try
                    aControl.Visible := aValue;
                except
                end;
            end;
        cpColor:
            begin
                try
                    (aControl as TWinControl).Brush.Color := aValue;
                except
                end; // Poo! TControl.Color is not public
            end;
        cpClose:
            begin
                if (aControl is TForm) then
                    (aControl as TForm).Close;
            end;
    end;
end;

class procedure TControlUtils.AddLinesToMemo(aMemo: TMemo; aLines: array of string);
var
    x: integer;
begin
    for x := 0 to high(aLines) do
    begin
        aMemo.Lines.Add(aLines[x]);
    end;
end;

class procedure TControlUtils.AddLineToMemo(aMemo: TMemo; aLine: string);
begin
    aMemo.Lines.Add(aLine);
end;

class procedure TControlUtils.AddTreeNodeListToTreeNodesRecursive(const aTreeNodeList: TStringTreeNodeList;
    const aTreeNodes: TTreeNodes; const aParentNode: TTreeNode);
var
    xNode: TStringTreeNode;
    xChild: TTreeNode;
begin
    for xNode in aTreeNodeList do
    begin
        xChild := aTreeNodes.AddChild(aParentNode, xNode.NodeValue);
        AddTreeNodeListToTreeNodesRecursive(xNode.Nodes, aTreeNodes, xChild);
    end;

end;

class procedure TControlUtils.AddTreeNodeListToTreeNodes(const aTree: TStringTree;
    const aTreeNodes: TTreeNodes);
begin
    AddTreeNodeListToTreeNodesRecursive(aTree.Nodes, aTreeNodes, nil);
end;

class procedure TControlUtils.AddValuesToStrings(const aValues: array of string; const aStrings: TStrings;
    const aClear: boolean);
var
    x: integer;
begin
    if aClear then
        aStrings.Clear;

    for x := 0 to high(aValues) do
        aStrings.Add(aValues[x]);
end;

class function TControlUtils.GetValuesFromMemo(const aMemo: TMemo): TStringArray;
begin
    result := GetValuesFromStrings(aMemo.Lines);
end;

class function TControlUtils.GetValuesFromStrings(const aStrings: TStrings): TStringArray;
var
    x: integer;
begin
    SetLength(result, aStrings.Count);
    for x := 0 to aStrings.Count - 1 do
        result[x] := aStrings[x];

end;

class procedure TControlUtils.AddValuesToGridRow(const aValues: array of string; const aGrid: TStringGrid;
    const aRow: integer);
begin
    AddValuesToStrings(aValues, aGrid.Rows[aRow], false);
end;

class procedure TControlUtils.AddValuesToComboBox(const aValues: array of string; const aComboBox: TComboBox;
    const aClear: boolean);
begin
    AddValuesToStrings(aValues, aComboBox.Items, aClear);
end;

class procedure TControlUtils.AddValuesToListBox(const aValues: array of string; const aListBox: TListBox;
    const aClear: boolean);
begin
    AddValuesToStrings(aValues, aListBox.Items, aClear);
end;

class procedure TControlUtils.AddValuesToTabControl(const aValues: array of string;
    const aTabControl: TTabControl);
begin
    AddValuesToStrings(aValues, aTabControl.Tabs, false);
end;

class procedure TControlUtils.AddValuesToMemo(const aValues: array of string; const aMemo: TMemo;
    const aClear: boolean);
begin
    AddValuesToStrings(aValues, aMemo.Lines, aClear);
end;


end.
