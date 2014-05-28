{ --------------------------------------------------------------------------------------------------
  Copyright © 2008 Zinsser Analytic GmbH - All rights reserved.
  Author       : Payman Kamali (pk)
  Description  :
  --------------------------------------------------------------------------------------------------
  Revision History:
  date     op  method                       track-no improvement/change
  -------- --  ---------------------------  -------- -----------------------------------------------
  23.06.08 pk                                TN4139  Initial Revision
  03.07.08 wl                                TN4157
  09.07.08 pk                                TN4139  various changes
  06.08.08 pk  RefreshLayout                 TN4139  new
  02.09.08 pk                                TN4125  Inherit directly from TRack instead of TGlobalRack
  17.09.08 wl                                TN4224   TZADesignLayout = class( TLayoutWithDevices )
  16.01.09 wl                                TN4362   an Änderungen in TViewItem angepasst
  16.06.09 wl                                TN4606   alle Bezüge auf ntRunTable und RunEditor entfernt
  17.06.09 wl                                TN4612   uses ObjSampl entfernt
  10.08.09 wl                                TN4702   Strings werden jetzt direkt geladen
  26.08.09 pk                                TN4753   reference to LayoutManager changed to TZADesignLayoutManager
  31.08.09 pk                                TN4753   uses PeripheryManager removed
  15.12.09 pk  ChooseCarrierSlot             TN4948   calls new EdActionProps.ChooseCarrierSlot function
  15.12.09 pk  TZADesignRack/Well            TN4946   Edit/ShowTubeContents popup menu did not work
  16.12.09 pk  CreatePopup                   TN4946   remove condition for ReactionBlocks (the prefix RB is no longer used)
  21.07.10 pk                                TN5066   MultiSelect implemented
  14.02.11 wl                                TN5468   entrümpelt
  03.11.11 wl  DoCreateRackWell              TN5725   jetzt mit WellNr
  20.02.12 wl                                TN5812   Rack-Menu "Tube content" entfernt
  -------------------------------------------------------------------------------------------------- }

unit ZADesignLayout;


interface


uses
    Layout;

type
    TZADesignLayout = class(TLayout)
    private
        procedure CreatePopup;
        procedure DoRefreshLayout(aSender: TObject);
        procedure RefreshLayout;
    protected
        procedure DoInitGraphics; override;
    end;


implementation


uses
    PopupMenuInfo,
    ZADesignLayoutManager;

{ TZADesignLayout }

procedure TZADesignLayout.DoInitGraphics;
begin
    inherited;
    CreatePopup();
end;

procedure TZADesignLayout.DoRefreshLayout(aSender: TObject);
begin
    RefreshLayout();
end;

procedure TZADesignLayout.RefreshLayout();
begin
    TZADesignLayoutManager.Instance.ForcedLoad();
end;

procedure TZADesignLayout.CreatePopup;
begin
    self.Graphics.AddPopupMenuItem(TPopupMenuInfoItem.Create('Refresh Layout', False, true, DoRefreshLayout,
        'RefreshLayout'));
end;


end.
