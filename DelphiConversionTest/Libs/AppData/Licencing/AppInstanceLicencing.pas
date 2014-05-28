{ --------------------------------------------------------------------------------------------------
  Copyright © 2007 Zinsser Analytic GmbH - All rights reserved.
  Author       : Wolfgang Lyncke (wl)
  Description  : Inherits instances for Licencing library
  --------------------------------------------------------------------------------------------------
  Revision History:
  date     op  method                       track-no improvement/change
  -------- --  ---------------------------  -------- -----------------------------------------------
  31.08.07 wl                               TN3811.4 initial version
  17.08.10 wl                               TN5112   Hardlock-Teile entfernt
  12.11.10 wl  Create                       TN5112   an Änderungen von Licence angepasst
  -------------------------------------------------------------------------------------------------- }

unit AppInstanceLicencing;


interface


uses
    CommonTypes,
    Licence;

type
    TAppInstanceLicencing = class
    private
        fLicence: TLicence;
        class var uInstLicencing: TAppInstanceLicencing; // Single Instance
        constructor Create(aAppMode: TAppMode);
    public
        destructor Destroy(); override;
        class function CreateInstance(aAppMode: TAppMode): TAppInstanceLicencing;
        class function Instance(): TAppInstanceLicencing;

        property Licence: TLicence read fLicence;
    end;


implementation


uses
    SysUtils;

{ TAppInstanceLicencing }

class function TAppInstanceLicencing.CreateInstance(aAppMode: TAppMode): TAppInstanceLicencing;
begin
    // create instance if instance does not exist
    if not Assigned(uInstLicencing) then
        uInstLicencing := TAppInstanceLicencing.Create(aAppMode);

    // return instance
    result := uInstLicencing;
end;

class function TAppInstanceLicencing.Instance: TAppInstanceLicencing;
begin
    result := uInstLicencing;
end;

constructor TAppInstanceLicencing.Create(aAppMode: TAppMode);
begin
    inherited Create();

    // Create Licence object with key
    fLicence := TLicence.Create(aAppMode);
end;

destructor TAppInstanceLicencing.Destroy();
begin
    FreeAndNil(fLicence);

    inherited;
end;


end.
