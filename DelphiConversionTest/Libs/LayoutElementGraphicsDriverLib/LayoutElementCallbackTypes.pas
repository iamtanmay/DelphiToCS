unit LayoutElementCallbackTypes;
{ --------------------------------------------------------------------------------------------------
  Copyright © 2008 Zinsser Analytic GmbH - All rights reserved.
  Author       : Payman Kamali (pk)
  Description  :
  --------------------------------------------------------------------------------------------------
  Revision History:
  date     op  method                       track-no improvement/change
  -------- --  ---------------------------  -------- -----------------------------------------------
  23.06.08 pk                                TN4139  Initial Revision
  31.07.08 pk  TGraphicsMouseCursor          TN4139  New
  10.03.09 pk                                TN4457  Changes needed for reimplementing Stacker Buttons
  13.03.09 pk                                TN4463  Changes needed for reimplementing carrier drag move
  13.03.09 pk                                TN4466  Changes need for implementing rack carrier slot change via drag and drop
  17.04.09 pk  cGraphicsMouseCursorUpArrow   TN4532  changed to -10
  03.09.09 wl  TGraphicsShiftState           TN4800  Anpassung an Delphi 2010
  29.11.10 wl                                TN5370  ifdefs entfernt
  -------------------------------------------------------------------------------------------------- }


interface


uses
    PopupMenuInfo;

type
    TGraphicsShiftState = set of (gssShift, gssAlt, gssCtrl, gssLeft, gssRight, gssMiddle, gssDouble,
        gssTouch, gssPen);
    TGraphicsMouseButton = (gmbLeft, gmbRight, gmbMiddle);
    TGraphicsDragState = (gdsDragEnter, gdsDragLeave, gdsDragMove);

    TMouseClickCallback = procedure(aSender: TObject) of object;
    TMouseDblClickCallback = TMouseClickCallback;
    TMouseMoveCallback = procedure(aSender: TObject; aShift: TGraphicsShiftState; aX, aY: double) of object;
    TMouseDownCallback = procedure(aSender: TObject; aButton: TGraphicsMouseButton;
        aShift: TGraphicsShiftState; aX, aY: double) of object;
    TMouseUpCallback = TMouseDownCallback;
    TMouseDragOverCallback = procedure(aSender, aSource: TObject; aX, aY: double; aState: TGraphicsDragState;
        var vAccept: boolean) of object;
    TMouseDragDropCallback = procedure(aSender, aSource: TObject; aX, aY: double) of object;
    TIsDragableCallback = procedure(aSender: TObject; var vIsDragable: boolean) of object;
    TPopupCallback = procedure(aSender: TObject; aX, aY: double; var vPopupMenuInfos: TPopupMenuInfoList)
        of object;
    TShowHintCallback = procedure(aSender: TObject; var vShow: boolean; var vHintText: string) of object;
    TPosChangedCallback = procedure(aSender: TObject; const aX, aY, aZ: double) of object;

    TLayoutElementGraphicsCallbacks = class
    public
        MouseClickCallback: TMouseClickCallback;
        MouseDblClickCallback: TMouseDblClickCallback;
        MouseMoveCallback: TMouseMoveCallback;
        MouseDownCallback: TMouseDownCallback;
        MouseUpCallback: TMouseUpCallback;
        MouseDragOverCallback: TMouseDragOverCallback;
        MouseDragDropCallback: TMouseDragDropCallback;
        IsDragableCallback: TIsDragableCallback;
        PopupCallback: TPopupCallback;
        ShowHintCallBack: TShowHintCallback;
        PosChangedCallback: TPosChangedCallback;
    end;

const
    cGraphicsMouseCursorDefault = 0;
    cGraphicsMouseCursorUpArrow = -10;

type
    TGraphicsMouseCursor = integer;
    TGraphicsBorderType = (gbtNone, gbtRaised, gbtSunken, gbtSingle);


implementation


end.
