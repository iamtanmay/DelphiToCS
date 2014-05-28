{ --------------------------------------------------------------------------------------------------
  Copyright © 2008 Zinsser Analytic GmbH - All rights reserved.
  Author       : Payman Kamali (pk)
  Description  :
  --------------------------------------------------------------------------------------------------
  Revision History:
  date     op  method                       track-no improvement/change
  -------- --  ---------------------------  -------- -----------------------------------------------
  23.06.08 pk                                TN4139  Initial Revision
  02.07.08 pk  fVisible                      TN4139  New
  04.11.09 pk                                TN4843  Replace TObjectList, TStringList, TStrings with Generics Lists and TStringArray
  10.04.13 wl                                TN6045      uses geändert
  -------------------------------------------------------------------------------------------------- }

unit PopupMenuInfo;


interface


uses
    Generics.Collections;

type
    TPopupCallback = procedure(aSender: TObject) of object;

    TPopupMenuInfoItem = class
    private
        fCaption: string;
        fOnClick: TPopupCallback;
        fChecked: boolean;
        fEnabled: boolean;
        fName: string;
        fVisible: boolean;
    public
        constructor Create(const aCaption: string; aChecked, aEnabled: Boolean; aOnClick: TPopupCallback;
            const aName: string);
        property Caption: string read fCaption write fCaption;
        property OnClick: TPopupCallback read fOnClick;
        property Checked: boolean read fChecked;
        property Enabled: boolean read fEnabled write fEnabled;
        property name: string read fName;
        property Visible: boolean read fVisible write fVisible;
    end;

    TPopupMenuInfoList = class(TObjectList<TPopupMenuInfoItem>)
    private
        fActive: boolean;
    public
        procedure AddItem(aItem: TPopupMenuInfoItem);
        property Active: boolean read fActive write fActive;
    end;


implementation


constructor TPopupMenuInfoItem.Create(const aCaption: string; aChecked, aEnabled: Boolean;
    aOnClick: TPopupCallback; const aName: string);
begin
    inherited Create();
    fVisible := true;
    fCaption := aCaption;
    fOnClick := aOnClick;
    fChecked := aChecked;
    fEnabled := aEnabled;
    fName := aName;
end;

procedure TPopupMenuInfoList.AddItem(aItem: TPopupMenuInfoItem);
begin
    Add(aItem);
end;


end.
