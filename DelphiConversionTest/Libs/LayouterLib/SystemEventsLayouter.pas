{ ----------------------------------------------------------------------------------------------------------------------
  Copyright © 2013 Zinsser Analytic GmbH - All rights reserved.
  Author       : Wolfgang Lyncke (wl)
  Description  :
  ----------------------------------------------------------------------------------------------------------------------
  Revision History:
  date     op  method                       track-no improvement/change
  -------- --  ---------------------------  -------- -------------------------------------------------------------------
  30.07.13 wl                               TN6160   initial version
  ---------------------------------------------------------------------------------------------------------------------- }

unit SystemEventsLayouter;


interface


uses
    SystemEvents,
    IntfStateSignalDevice;

type
    TSystemEventsLayouter = class(TSystemEvents)
    public
        procedure LayouterConnect(); override;
        procedure LayouterDisconnect(); override;
    end;


implementation


{ TSystemEventsLayouter }

procedure TSystemEventsLayouter.LayouterConnect;
begin
    self.SetState(sStateTest, 'Layouter: Devices connected');
end;

procedure TSystemEventsLayouter.LayouterDisconnect;
begin
    self.SetState(sStateReady, 'Layouter: Devices disconnected');
end;


end.
