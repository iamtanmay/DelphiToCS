unit AppInstanceRunDialogs;


{ -----------------------------------------------------------------------------------------------------------------------
  Copyright © 2009 Zinsser Analytic GmbH - All rights reserved.
  Author       : Payman Kamali (pk)
  Description  :
  -----------------------------------------------------------------------------------------------------------------------
  Revision History:
  date     op  method                                track-no  improvement/change
  -------- --  ------------------------------------  --------- ----------------------------------------------------------
  27.08.09 pk                                        TN4753      Initial Revision
  ----------------------------------------------------------------------------------------------------------------------- }
interface


uses
    TypeInfo;

type
    TAppInstanceRunDialogs = class
    private
        constructor Create();
    public
        destructor Destroy(); override;
        class function CreateInstance(): TAppInstanceRunDialogs;
        class procedure DestroyInstance();
        class function Instance(): TAppInstanceRunDialogs;
    end;


implementation


uses
    RunDialogsManager,
    RunStandardDisplayComponents;

var
    uInstRunDialogs: TAppInstanceRunDialogs;

const
    cNameRunDialogs = 'RunDialogs.bpl';
    cVersionRunDialogs = '1.0.0';

    { TAppInstanceRunDialogs }

constructor TAppInstanceRunDialogs.Create;
begin
    inherited Create();
    TRunDialogsManager.CreateInstance();
    TRunStandardDisplayComponents.CreateInstance();
end;

destructor TAppInstanceRunDialogs.Destroy;
begin
    TRunStandardDisplayComponents.DestroyInstance();
    TRunDialogsManager.DestroyInstance();
    inherited;
end;

class function TAppInstanceRunDialogs.CreateInstance(): TAppInstanceRunDialogs;
begin
    // create instance if instance does not exist
    if not Assigned(uInstRunDialogs) then
        uInstRunDialogs := TAppInstanceRunDialogs.Create();

    // return instance
    result := uInstRunDialogs;
end;

class function TAppInstanceRunDialogs.Instance(): TAppInstanceRunDialogs;
begin
    result := uInstRunDialogs;
end;

class procedure TAppInstanceRunDialogs.DestroyInstance;
begin
    uInstRunDialogs.Free;
end;


end.
