{ -----------------------------------------------------------------------------------------------------------
  Copyright © 2012 Zinsser Analytic GmbH - All rights reserved.
  Author       : Wolfgang Lyncke (wl)
  Description  :
  -----------------------------------------------------------------------------------------------------------
  Revision History:
  date     op  method                              track-no improvement/change
  -------- --  ----------------------------------  -------- -------------------------------------------------
  10.12.12 wl                                      TN6045   Initial Revision
  14.03.13 ts                                      TN6092   new Icons für Startable methods
  12.07.13 ts                                      TN6200   new Icons for RETURN
  03.04.14 ts                                      TN6387   new Icons for Editable in ZARunner
  ----------------------------------------------------------------------------------------------------------- }

unit ActionImages;


interface


uses
    Forms,
    ImgList,
    Classes,
    Controls;

type
    TfrmActionImages = class(TForm)
        ActionImages64: TImageList;
        ActionImages24: TImageList;
        ActionImages16: TImageList;
    end;


implementation


{$R *.dfm}


end.
