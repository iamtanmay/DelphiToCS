{ --------------------------------------------------------------------------------------------------
  Copyright © 2006 Zinsser Analytic GmbH - All rights reserved.
  Author       : Payman Kamali (pk)
  Description  :
  --------------------------------------------------------------------------------------------------
  Revision History:
  date     op  method                       track-no improvement/change
  -------- --  ---------------------------  -------- -----------------------------------------------
  05.01.06 pk                               TN2877   Initial version
  05.07.06 pk  fVirtualResources            TN3179   New : Resources are read from resource and stored in this list
  05.07.06 pk  AcquireVirtualResource       TN3179   New : do acqire for a virtual resource
  07.12.06 pk                               TN3455   most of code moved to TResourceManagerRun
  17.11.08 pk                               TN4280   various changes
  15.08.13 wl                               TN6223   uses geändert
  -------------------------------------------------------------------------------------------------- }

unit ResourceManager;


interface


uses
    Classes,
    GeneralTypes,
    MessagableExecutable;

type

    IResource = interface
        function Acquire(aTimeout: cardinal): boolean;
        procedure Release();
    end;

    TResource = class
    public
        function Acquire(aTimeout: cardinal): boolean; virtual;
        procedure Release(); virtual;
    end;

    TResourceType = (rtDevice, rtRack, rtCarrier, rtResScheme);

    TResourceManager = class(TMessagableExecutable)
    public
        function CreateResource(aMaxCount: integer): TResource; virtual;
        procedure AcquireResource(const aResourceName: string; const aResourceType: TResourceType); virtual;
        procedure ReleaseResource(const aResourceName: string; const aResourceType: TResourceType); virtual;
        procedure ReleaseAllResources(const aResourceType: TResourceType); virtual;
    end;

var
    gResourceManager: TResourceManager;


implementation


{ TResource }

function TResource.Acquire(aTimeout: cardinal): boolean;
begin
    result := true;
end;

procedure TResource.Release;
begin
end;

{ TResourceManager }
procedure TResourceManager.AcquireResource(const aResourceName: string; const aResourceType: TResourceType);
begin
end;

procedure TResourceManager.ReleaseResource(const aResourceName: string; const aResourceType: TResourceType);
begin
end;

procedure TResourceManager.ReleaseAllResources(const aResourceType: TResourceType);
begin

end;

function TResourceManager.CreateResource(aMaxCount: integer): TResource;
begin
    result := nil;
end;


end.
