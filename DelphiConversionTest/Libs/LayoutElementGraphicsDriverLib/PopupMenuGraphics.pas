unit PopupMenuGraphics;


{ --------------------------------------------------------------------------------------------------
  Copyright © 2008 Zinsser Analytic GmbH - All rights reserved.
  Author       : Payman Kamali (pk)
  Description  :
  --------------------------------------------------------------------------------------------------
  Revision History:
  date     op  method                       track-no improvement/change
  -------- --  ---------------------------  -------- -----------------------------------------------
  23.06.08 pk                                TN4139  Initial Revision
  02.07.08 pk  Init                          TN4139  Sets visible prop for each menuitem
  02.07.08 pk  Init                          TN4139  Autopopup disabled
  -------------------------------------------------------------------------------------------------- }
interface


uses
    Controls,
    Menus,
    PopupMenuInfo;

type
    TPopupMenuGraphics = class
    private
        fPopupMenuInfo: TPopupMenuInfoList;
        fParent: TObject;
        fPopupMenu: TPopupMenu;
        constructor Create(aPopupMenuInfo: TPopupMenuInfoList; aParent: TObject);
        procedure Init();
    public
        procedure Popup(aX, aY: integer);
        class function CreatePopupInstance(aPopupMenuInfo: TPopupMenuInfoList; aParent: TObject)
            : TPopupMenuGraphics;
    end;


implementation


uses
    ExtCtrls,
    Types;
{ TPopupMenuGraphics }

constructor TPopupMenuGraphics.Create(aPopupMenuInfo: TPopupMenuInfoList; aParent: TObject);
begin
    inherited Create();
    fPopupMenuInfo := aPopupMenuInfo;
    fParent := aParent;
    fPopupMenu := nil;
end;

class function TPopupMenuGraphics.CreatePopupInstance(aPopupMenuInfo: TPopupMenuInfoList; aParent: TObject)
    : TPopupMenuGraphics;
begin
    result := TPopupMenuGraphics.Create(aPopupMenuInfo, aParent);
    result.Init();
end;

procedure TPopupMenuGraphics.Init();
var
    x: integer;
    xItemArray: array of TMenuItem;
    xInfoItem: TPopupMenuInfoItem;
    xParent: TPanel;
begin
    Assert(Assigned(fParent));
    // Implemented for TPanel, The popup property is protected for Twincontrol and only visible through TPanel
    if not(fParent is TPanel) then
        Exit;
    xParent := fParent as TPanel;

    Assert(xParent.HandleAllocated, 'Handle not allocated for parent, cannot create Popup Menu');

    SetLength(xItemArray, fPopupMenuInfo.Count);
    for x := 0 to high(xItemArray) do
    begin
        xInfoItem := fPopupMenuInfo[x];
        xItemArray[x] := NewItem(xInfoItem.Caption, 0, xInfoItem.Checked, xInfoItem.Enabled,
            xInfoItem.OnClick, 0 { HelpContext } , xInfoItem.Name);
        xItemArray[x].Visible := xInfoItem.Visible;
    end;

    try
        xParent.PopupMenu.Free;
    except
    end;
    fPopupMenu := NewPopupMenu(xParent, 'PopupMenu', paLeft, false { AutoPopup } , xItemArray);
    xParent.PopupMenu := fPopupMenu;

end;

procedure TPopupMenuGraphics.Popup(aX, aY: integer);
var
    xParent: TPanel;
    xPoint: TPoint;
begin
    if not(fParent is TPanel) then
        Exit;
    xParent := fParent as TPanel;
    xPoint := Point(aX, aY);
    xPoint := xParent.ClientToScreen(xPoint);
    fPopupMenu.Popup(xPoint.X, xPoint.Y);
end;


end.
