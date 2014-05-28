{ --------------------------------------------------------------------------------------------------
  Copyright © 2003 Zinsser Analytic GmbH - All rights reserved.
  Author       : Payman Kamali (pk)
  Description  : The minimal SystemEvents functionality required by the Layouter
  --------------------------------------------------------------------------------------------------
  Revision History:
  date     op  method                       track-no improvement/change
  -------- --  ---------------------------  -------- -----------------------------------------------
  23.02.04 pk                                TN1719   New
  06.01.05 wl                               TN2246.4  enthält nun ALLE Funktionen, die auch TSystemEvents enthält
  26.01.06 pk evTerminateUserInterrupt      TN2904    New param aIsError
  01.12.06 pk TSystemEventsMinimal          TN3441    System event functions ApplicationRequeust, ErrorMessage replaced by InterruptStart/Finish
  07.04.09 pk                                        TN4503    ThreadStarted/Stopped removed
  14.04.09 pk Init                          TN4523   New
  19.11.10 wl  evStartMethodOrScript        TN5358   kein Rückgabewert mehr
  17.10.12 ts                               TN5993   UserProtection als property, damit InterruptMonitor in ExecHandler beendet werden kann
  30.07.13 wl                               TN6160   neue Struktur
  31.07.13 ts  InitDevices                  TN6160.1 auskommentiert, da Probleme mit ZP02
  -------------------------------------------------------------------------------------------------- }

unit SystemEvents;


interface


uses
    IntfStateSignalDevice;

type
    TSystemEvents = class
    private
        fIsInit: boolean;
        fCurrentState: TSystemState;
        fStateSignalDevice: IStateSignalDevice;
        class var uInstance: TSystemEvents;
    protected
        procedure SetState(aState: TSystemState; aMessage: string);
        procedure CallDllFunction(aState: TSystemState; aMessage: string); virtual;
        procedure InitDevices(); virtual;
    public
        constructor Create;

        procedure LayouterConnect(); virtual;
        procedure LayouterDisconnect(); virtual;

        procedure MethodStart(); virtual;
        procedure MethodFinish(); virtual;

        procedure InterruptStart(const aReason: string); virtual;
        procedure InterruptFinish(); virtual;

        procedure ModalMessageStart(); virtual;
        procedure ModalMessageFinish(); virtual;

        procedure ModalDllMessageStart(); virtual;
        procedure ModalDllMessageFinish(); virtual;

        procedure QuitProtectionPoll(); virtual;

        property CurrentState: TSystemState read fCurrentState;
        class procedure CreateInstance(aInstance: TSystemEvents);
        class procedure DestroyInstance();
        class property Instance: TSystemEvents read uInstance;
    end;


implementation


uses
    SysUtils,
    ObjModul,
    ObjModulA;

{ TSystemEvents }

constructor TSystemEvents.Create;
begin
    inherited Create;

    fIsInit := false;
    fCurrentState := sStateUnknown;
    fStateSignalDevice := gModules.FindStateSignal();
end;

class procedure TSystemEvents.CreateInstance(aInstance: TSystemEvents);
begin
    uInstance := aInstance;
end;

class procedure TSystemEvents.DestroyInstance;
begin
    FreeAndNil(uInstance);
end;

procedure TSystemEvents.CallDllFunction(aState: TSystemState; aMessage: string);
begin
    // nichts passiert
end;

procedure TSystemEvents.SetState(aState: TSystemState; aMessage: string);
begin
    if (fCurrentState = aState) then
        EXIT;

    fCurrentState := aState;

    if Assigned(fStateSignalDevice) then
    begin
        fStateSignalDevice.ChangeSignal(fCurrentState);
    end;

    CallDllFunction(fCurrentState, aMessage);
end;

procedure TSystemEvents.InitDevices();
begin
    { if fIsInit then
      EXIT;
      if Assigned(fStateSignalDevice) then
      TActionModules.ConnectDevice(fStateSignalDevice);
      fIsInit := true; }
end;

procedure TSystemEvents.InterruptFinish;
begin

end;

procedure TSystemEvents.InterruptStart(const aReason: string);
begin

end;

procedure TSystemEvents.LayouterConnect;
begin

end;

procedure TSystemEvents.LayouterDisconnect;
begin

end;

procedure TSystemEvents.MethodFinish;
begin

end;

procedure TSystemEvents.MethodStart;
begin

end;

procedure TSystemEvents.ModalDllMessageFinish;
begin

end;

procedure TSystemEvents.ModalDllMessageStart;
begin

end;

procedure TSystemEvents.ModalMessageFinish;
begin

end;

procedure TSystemEvents.ModalMessageStart;
begin

end;

procedure TSystemEvents.QuitProtectionPoll;
begin

end;


end.
