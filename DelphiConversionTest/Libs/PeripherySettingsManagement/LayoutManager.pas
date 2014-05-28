unit LayoutManager;
{ --------------------------------------------------------------------------------------------------
  Copyright © 2008 Zinsser Analytic GmbH - All rights reserved.
  Author       : Payman Kamali (pk)
  Description  :
  --------------------------------------------------------------------------------------------------
  Revision History:
  date     op  method                       track-no improvement/change
  -------- --  ---------------------------  -------- -----------------------------------------------
  23.06.08 pk                                TN4139  Initial Revision
  02.07.08 pk  SceneChanged                  TN4139  New
  09.07.08 pk  Reload                        TN4139  calls unload and scenechanged
  09.07.08 pk  Destroy                       TN4139  call unload just in case a layout is still loaded
  16.07.08 pk                                TN4139  changes needed for reading racks from runlayout
  30.07.08 pk                                TN4139  various changes
  04.08.08 pk                                TN4139  Read, Write Zoom, PanX, and PanY
  06.08.08 pk  DoUnload                      TN4139  New
  08.09.08 pk  Destroy                       TN4139  ApplicationEnd now called automatically in destroy
  16.03.09 pk  Load                          TN4472  Error message corrected
  06.04.09 pk  IsCurrentLayoutEmpty          TN4521  New
  06.04.09 pk  Create                        TN4503  SceneGraphics.Visible := true moved to ShowCurrent
  17.04.09 pk  SceneGraphics                 TN4532  Now public property
  08.06.09 pk  Create                        TN4585.1 Create SceneGraphics only if Backgroundparent is assigned
  27.07.09 pk  GetSceneGraphicsForLayout     TN4604  New
  10.08.09 wl                                TN4702   Strings werden jetzt direkt geladen
  02.09.09 pk                                TN4753  changes needed for better cleanup in Designer
  28.09.09 pk                                TN4753  Unload changed to UnregisterCurrentLayout
  28.09.09 pk  CreateDefaultSceneGraphics    TN4753  New
  26.10.09 wl  DoCreateSceneGraphics         TN4831   IConfigurationSet replaces TIniFile
  05.11.09 pk  ReadSettings                  TN4851  New: ReadReverseY here.  Was previously done in ReadRobotIni
  04.08.10 pk  GetTempSettingsSectionName    TN5043  New: a different settings section name for each layoutmanager
  04.08.10 pk  DoDestroySceneGraphcis        TN5043  New
  23.04.10 wl  TCarrierEvaluation            TN5070  neu, um Carrier Evaluation-Fenster zu kapseln
  23.04.10 wl  DragSourceIs..                TN5070  für DragDrop-Aktionen im Layout-Editor
  30.04.10 wl  EvaluationStart               TN5070  Rack als neuer Parameter
  02.06.10 wl  alles bis auf Instance        TN5116   --> CustomLayoutManager
  -------------------------------------------------------------------------------------------------- }


interface


uses
    CustomLayoutManager;

type
    TLayoutManager = class(TCustomLayoutManager)
    private
        class var uInstance: TLayoutManager;
    public
        class procedure SetInstance(aLayoutManager: TLayoutManager);
        class procedure DestroyInstance();
        class property Instance: TLayoutManager read uInstance;
    end;


implementation


uses
    SysUtils;

{ TLayoutManager }

class procedure TLayoutManager.SetInstance(aLayoutManager: TLayoutManager);
begin
    uInstance := aLayoutManager;
end;

class procedure TLayoutManager.DestroyInstance();
begin
    FreeAndNil(uInstance);
end;


end.
