{ --------------------------------------------------------------------------------------------------
  Copyright © 2004 Zinsser Analytic GmbH - All rights reserved.
  Author       : Payman Kamali (pk)
  Description  : Used to keep track of points in the software startup sequence which were reached successfully.
  This information can then be used in the software shutdown sequence
  --------------------------------------------------------------------------------------------------
  Revision History:
  date     op  method                       track-no improvement/change
  -------- --  ---------------------------  -------- -----------------------------------------------
  08.03.04 pk                                TN1646  New
  04.11.09 pk                               TN4843 Replace TObjectList, TStringList, TStrings with Generics Lists and TStringArray
  12.04.13 wl                               TN6045   uses Generics.Collections
  -------------------------------------------------------------------------------------------------- }

unit AppServices;


interface


uses
    Generics.Collections;

type
    TAppServiceType = (asAppInit, asMainFormCreate, asMainFormStartup);

    TAppServiceLoading = class
    private
        class var uLoadedServiceList: TList<TAppServiceType>;
    public
        class procedure CreateLoadedServicesList();
        class procedure DestroyLoadedServicesList();
        class procedure ServiceLoaded(aService: TAppServiceType);
        class function ServiceUnloaded(aService: TAppServiceType): boolean;
        class function LoadedServiceListIndexOf(aService: TAppServiceType): integer;
        class function IsServiceLoaded(aService: TAppServiceType): boolean;
    end;


implementation


uses
    SysUtils;

class procedure TAppServiceLoading.CreateLoadedServicesList();
begin
    uLoadedServiceList := TList<TAppServiceType>.Create();
end;

class procedure TAppServiceLoading.DestroyLoadedServicesList();
begin
    FreeAndNil(uLoadedServiceList);
end;

class procedure TAppServiceLoading.ServiceLoaded(aService: TAppServiceType);
begin
    uLoadedServiceList.Add(aService);
end;

class function TAppServiceLoading.ServiceUnloaded(aService: TAppServiceType): boolean;
var
    xIndex: integer;
begin
    result := false;
    xIndex := LoadedServiceListIndexOf(aService);
    if xIndex = -1 then
        Exit;
    uLoadedServiceList.Delete(xIndex);
    result := true;
end;

class function TAppServiceLoading.LoadedServiceListIndexOf(aService: TAppServiceType): integer;
begin
    result := uLoadedServiceList.IndexOf(aService);
end;

class function TAppServiceLoading.IsServiceLoaded(aService: TAppServiceType): boolean;
begin
    result := self.LoadedServiceListIndexOf(aService) > -1;
end;


end.
