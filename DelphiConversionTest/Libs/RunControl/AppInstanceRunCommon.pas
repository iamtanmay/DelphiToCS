{ --------------------------------------------------------------------------------------------------
  Copyright © 2007 Zinsser Analytic GmbH - All rights reserved.
  Author       : Wolfgang Lyncke (wl)
  Description  : Inherits instances for Logging library
  --------------------------------------------------------------------------------------------------
  Revision History:
  date     op  method                       track-no improvement/change
  -------- --  ---------------------------  -------- -----------------------------------------------
  31.08.07 wl                               TN3811.4  initial version
  03.09.07 wl  Purpose                      TN3811.4  neue Member-Variable
  17.04.09 pk  TAppInstanceRunCommon.Create TN4532    now with aAppHInstance parameter
  20.05.10 wl                               TN5116   benutzt TAppDataInstancesStarter
  21.07.10 pk  TAppInstanceRunCommon.Destroy TN5203  Free RunFlow and AppInstanceStarter
  19.10.10 pk                               TN5305   changes needed for CoreClient/Server
  11.09.11 wl                               TN5672   Instance entfernt
  20.08.13 wl                               TN6231   an RunFlow angepasst
  -------------------------------------------------------------------------------------------------- }

unit AppInstanceRunCommon;


interface


type
    TAppInstanceRunCommon = class
    public
        constructor Create();
        destructor Destroy(); override;
    end;


implementation


uses
    RunFlow;

{ TAppInstanceRunCommon }

constructor TAppInstanceRunCommon.Create();
begin
    inherited Create;
    TRunFlow.CreateInstance;
end;

destructor TAppInstanceRunCommon.Destroy();
begin
    TRunFlow.DestroyInstance;
    inherited;
end;


end.
