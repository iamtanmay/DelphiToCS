{ -----------------------------------------------------------------------------------------------------------------------
  Copyright © 2009 Zinsser Analytic GmbH - All rights reserved.
  Author       : Payman Kamali (pk)
  Description  :
  -----------------------------------------------------------------------------------------------------------------------
  Revision History:
  date     op  method                                track-no  improvement/change
  -------- --  ------------------------------------  --------- ----------------------------------------------------------
  23.06.09 pk                                         TN4538    InitialRevision
  27.08.09 pk  TAppInstanceRunControlExt              TN4753    create instance of RunDisplayComponentManager
  28.03.12 wl                                         TN5844    RunDisplayComponentManager entfernt (war unnötig)
  ----------------------------------------------------------------------------------------------------------------------- }

unit AppInstanceRunControlExt;


interface


type
    TAppInstanceRunControlExt = class
    private
        class var uInstance: TAppInstanceRunControlExt;
        constructor Create();
    public
        destructor Destroy(); override;
        class procedure CreateInstance();
        class procedure DestroyInstance();
        class property Instance: TAppInstanceRunControlExt read uInstance;
    end;


implementation


uses
    SysUtils,
    RunStepBlockManager;

{ TAppInstanceRunControlExt }

constructor TAppInstanceRunControlExt.Create;
begin
    inherited Create();
    TRunStepBlockManager.CreateInstance();
end;

destructor TAppInstanceRunControlExt.Destroy;
begin
    TRunStepBlockManager.DestroyInstance();
    inherited;
end;

class procedure TAppInstanceRunControlExt.CreateInstance();
begin
    // create instance if instance does not exist
    if not Assigned(uInstance) then
        uInstance := TAppInstanceRunControlExt.Create();
end;

class procedure TAppInstanceRunControlExt.DestroyInstance;
begin
    FreeAndNil(uInstance);
end;


end.
