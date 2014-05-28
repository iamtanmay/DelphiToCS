unit LayoutDisplay;
{ -----------------------------------------------------------------------------------------------------------------------
  Copyright © 2010 Zinsser Analytic GmbH - All rights reserved.
  Author       : Wolfgang Lyncke (wl)
  Description  :
  -----------------------------------------------------------------------------------------------------------------------
  Revision History:
  date     op  method                                track-no  improvement/change
  -------- --  ------------------------------------  --------- ----------------------------------------------------------
  23.04.10 wl                                        TN5070    Initial Revision
  ----------------------------------------------------------------------------------------------------------------------- }


interface


uses
    SceneGraphics,
    Controls;

type
    ILayoutDisplay = interface
        ['{B2D67D4C-D9D4-45B6-8D28-33327E888D98}']
        procedure PrepareAddLayout(const aLayoutName: string; out oBackground: TWinControl);
        procedure FinalizeRemoveLayout(const aLayoutName: string);
        procedure LayoutAdded(const aLayoutName: string; const aSceneGraphics: TSceneGraphics);
        procedure LayoutRemoved(const aLayoutName: string);
        procedure SetCaption(aRunName, aLayoutName: string);
    end;


implementation


end.
