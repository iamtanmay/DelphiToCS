unit LnkCommonLinkAll;
{ --------------------------------------------------------------------------------------------------
  Copyright © 2002 Zinsser Analytic GmbH - All rights reserved.
  Author       : Payman Kamali (pk)
  Description  :
  --------------------------------------------------------------------------------------------------
  Revision History:
  date     op  method                       track-no improvement/change
  -------- --  ---------------------------  -------- -----------------------------------------------
  26.07.02 pk                                        Created
  02.09.02 pk  ListenRefreshEditFormGrid             New: Refresh the ScheEdit form grid.
  02.09.02 pk  ListenChangeEditMode                  New: Change mode of ScheEdit form
  14.11.02 wl  ListenOpenUFuncSelectForm    TN1328.1 uses TAppSettings.UsesScheduler
  08.12.03 pk                               TN1697   Makesure ScheduleEditForm is assigned before accessing it
  21.07.04 pk                               TN2049   Various changes
  06.01.05 wl  ListenChangeEditMode,ListenRefreshEditFormGrid  TN2246.4  Bezug auf ScheEdit.pas --> ObjSampl,ObjectsOld
  06.01.05 wl  ListenOpenUFuncSelectForm          TN2246.4  Bezug auf UFuncSel.pas --> ObjSampl,ObjectsOld
  13.01.05 wl  ListenShow/CloseRunForm            TN2246.4  Bezug auf ShowRun.pas --> ObjSampl,ObjectsOld
  27.01.05 pk                               TN2281.0  scope of functions changed to public
  02.03.05 pk                               TN2328    SchedChart Methods changed
  22.03.05 pk  ListenChangeGlobalName       TN2359    Change global name to Session
  02.08.05 wl                               TN2501.1  Funktionen, die ZADesign nicht braucht --> EdObject
  22.08.05 wl                               TN2558.8  uses ScheUtil entfernt, ScheUtil-Methoden werden mit TDMScript. oder global. aufgerufen
  25.08.05 wl                               TN2558.8  toter Quelltext entfernt
  23.09.05 pk  ListenChangeWorkbench        TN2631    removed
  07.11.05 pk  ListenChangeEditForm         TN2737    LayoutName parameter
  08.11.05 wl  TEnGlobalChange              TN2745    gcScript entfernt
  24.11.05 pk                               TN2805    unused functions removed
  09.01.08 wl  ListenLoad/SaveFormPosition  TN3972    Inhalt wird nur im Runner benötigt --> LnkSamplerMainLinkAll
  23.10.09 wl  ListenLoad/SaveFormPosition  TN4831    entfernt
  -------------------------------------------------------------------------------------------------- }


interface


uses
    Forms;

type
    TEnGlobalChange = (gcNone, gcMethod, gcSession);

    TCommonLinkAll = class
    public
        class procedure TalkSchedChartClose; virtual;
    end;


implementation


uses
    uFrmScheduleChart;

class procedure TCommonLinkAll.TalkSchedChartClose;
begin
    if frmScheduleChart = nil then
        Exit;
    frmScheduleChart.Close;
end;


end.
