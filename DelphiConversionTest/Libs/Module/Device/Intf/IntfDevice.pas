unit IntfDevice;
{ ----------------------------------------------------------------------------------------------------------------------
  Copyright  2008 Zinsser Analytic GmbH - All rights reserved.
  Author       : Wolfgang Lyncke (wl)
  Description  :
  ----------------------------------------------------------------------------------------------------------------------
  Revision History:
  date     op  method                       track-no improvement/change
  -------- --  ---------------------------  -------- -------------------------------------------------------------------
  07.02.08 wl  TDevInitID                   TN4009   entfernt (ist bereits in Driver definiert)
  26.08.08 wl  Independent                  TN4164   neue Property
  14.04.09 pk  StandBy                      TN4524   New
  ---------------------------------------------------------------------------------------------------------------------- }


interface


uses
    Module,
    Driver;

type
    IDevice = interface(IModule)
        ['{916C8B76-763B-4F47-AAAB-057EBE8F3161}']
        function GetIndependent: boolean;
        procedure SetIndependent(const Value: boolean);
        property Independent: boolean read GetIndependent write SetIndependent;

        procedure Init(aInitID: TDevInitID);
        procedure StandBy(const aStandBy: boolean);
    end;


implementation


end.
