{ -----------------------------------------------------------------------------------------------------------------------
  Copyright © 2009 Zinsser Analytic GmbH - All rights reserved.
  Author       : Payman Kamali (pk)
  Description  :
  -----------------------------------------------------------------------------------------------------------------------
  Revision History:
  date     op  method                       track-no improvement/change
  -------- --  ---------------------------  -------- --------------------------------------------------------------------
  06.04.09 pk                                TN4503  Initial Revision
  11.09.11 wl  DestroyInstance               TN5672   Instanzen müssen IMMER mit FreeAndNil zerstört werden!
  ----------------------------------------------------------------------------------------------------------------------- }

unit DisplayComponentEventManager;


interface


uses
    DisplayComponentIntf;

type

    TDisplayComponentEventManager = class
    strict private
        fOnSimpleEvent: TDisplayComponentNotifyEvent;
        class var uInstance: TDisplayComponentEventManager;
        constructor Create();
    public
        destructor Destroy(); override;
        class procedure CreateInstance();
        class procedure DestroyInstance();
        class function Instance(): TDisplayComponentEventManager;
        property OnSimpleEvent: TDisplayComponentNotifyEvent read fOnSimpleEvent write fOnSimpleEvent;
    end;


implementation


uses
    SysUtils;

{ TDisplayComponentEventManager }

constructor TDisplayComponentEventManager.Create;
begin
    inherited;
    fOnSimpleEvent := nil;
end;

destructor TDisplayComponentEventManager.Destroy;
begin
    inherited;
end;

class procedure TDisplayComponentEventManager.CreateInstance;
begin
    uInstance := TDisplayComponentEventManager.Create();
end;

class procedure TDisplayComponentEventManager.DestroyInstance;
begin
    FreeAndNil(uInstance);
end;

class function TDisplayComponentEventManager.Instance: TDisplayComponentEventManager;
begin
    result := uInstance;
end;


end.
