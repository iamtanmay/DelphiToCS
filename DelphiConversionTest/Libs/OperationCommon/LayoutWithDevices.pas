unit LayoutWithDevices;
{ ----------------------------------------------------------------------------------------------------------------------
  Copyright  2008 Zinsser Analytic GmbH - All rights reserved.
  Author       : Wolfgang Lyncke (wl)
  Description  : Layout: Overridden methods that uses the device hirarchy
  ----------------------------------------------------------------------------------------------------------------------
  Revision History:
  date     op  method                       track-no improvement/change
  -------- --  ---------------------------  -------- -------------------------------------------------------------------
  17.09.08 wl                               TN4224   initial revision
  17.07.09 pk  FindWorkspaceForArm          TN4661   New
  04.15.10 pk  TipTypeChanged               TN5050   New
  04.15.10 pk  AssignCallbacks              TN5050   New
  07.06.10 wl  FindWorkspaceForArm          TN5116   jetzt als class Methode
  17.06.10 wl  TipTypeChanged               TN5150   --> Layout
  17.06.10 wl  FindWorkspaceForArm          TN5150   --> ArmMoveControl
  17.06.10 wl  GetPossibleTipsetDevices     TN5150   --> LayoutExt
  18.12.12 ts  DoCreateTipsetDevice         TN6013   LayoutDeviceList muss auch ohne StandardInit geladen werden, sonst keine TipTypes
  ---------------------------------------------------------------------------------------------------------------------- }


interface


uses
    LayoutExt,
    Tipset;

type
    // This is really bad design! This class should be removed in C#
    TLayoutWithDevices = class(TLayoutExt)
    protected
        function DoCreateTipsetDevice: TTipsetDevice; override;
    public
        constructor Create(const aLayoutName, aRunName: string);
    end;


implementation


uses
    PeripheryManager;

{ TLayoutWithDevices }

constructor TLayoutWithDevices.Create(const aLayoutName, aRunName: string);
begin
    inherited;

end;

function TLayoutWithDevices.DoCreateTipsetDevice: TTipsetDevice;
begin
    result := inherited DoCreateTipsetDevice();
    TPeripheryManager.Instance.RefreshLayoutDeviceList;
    result.OnInitTipset := TPeripheryManager.Instance.InitTipTypes;
end;


end.
