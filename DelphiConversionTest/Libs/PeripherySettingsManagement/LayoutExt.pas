unit LayoutExt;
{ ----------------------------------------------------------------------------------------------------------------------
  Copyright © 2010 Zinsser Analytic GmbH - All rights reserved.
  Author       : Wolfgang Lyncke (wl)
  Description  : Layout methods that uses device settings
  ----------------------------------------------------------------------------------------------------------------------
  Revision History:
  date     op  method                       track-no improvement/change
  -------- --  ---------------------------  -------- -------------------------------------------------------------------
  17.06.10 wl                               TN5150   initial revision
  ---------------------------------------------------------------------------------------------------------------------- }


interface


uses
    GeneralTypes,
    Layout,
    Tipset;

type
    TLayoutExt = class(TLayout)
    public
        constructor Create(const aLayoutName, aRunName: string);
        function GetPossibleTipsetDevices(aLayoutDeviceNames: TStringArray; aUseAllDevices: boolean)
            : TTipsetDeviceRecArray;
    end;


implementation


uses
    SysUtils,
    DesignModuleSettingFinder;

{ TLayoutExt }

constructor TLayoutExt.Create(const aLayoutName, aRunName: string);
begin
    inherited;

    self.OnFindTipsetDevices := self.GetPossibleTipsetDevices;
end;

function TLayoutExt.GetPossibleTipsetDevices(aLayoutDeviceNames: TStringArray; aUseAllDevices: boolean)
    : TTipsetDeviceRecArray;
var
    x: integer;
    xPipDevices: TStringArray;
begin
    xPipDevices := TDesignModuleSettingFinder.GetPipDeviceNames(aLayoutDeviceNames, aUseAllDevices);

    SetLength(result, Length(xPipDevices));
    for x := 0 to Length(xPipDevices) - 1 do
    begin
        result[x].DeviceName := xPipDevices[x];
        result[x].NoOfTips := TDesignModuleSettingFinder.ReadTipCount(xPipDevices[x]);
    end;
end;


end.
