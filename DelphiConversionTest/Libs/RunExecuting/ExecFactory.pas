{ --------------------------------------------------------------------------------------------------
  Copyright © 2005 Zinsser Analytic GmbH - All rights reserved.
  Author       : Payman Kamali (pk)
  Description  : Creates an IExecutable given an action
  --------------------------------------------------------------------------------------------------
  Revision History:
  date     op  method                       track-no improvement/change
  -------- --  ---------------------------  -------- -----------------------------------------------
  17.03.05 pk                               TN2353   New
  06.04.05 pk                               TN2373   Changes for TMultipleItems/TMultipleItemsItem
  19.04.05 pk  CreateExecFromRunStep        TN2391   if Group readMode is live then create TGroupHandler
  05.01.06 pk                               TN2877   changes because MultipleItems.create has a IResource parameter
  25.03.06 pk                               TN2999   Various changes to allow nested groups to be executed correctly
  02.05.06 pk  CreateExecFromRunStep        TN3081   TGroupedRunStep instead of TGroupRunStep
  04.05.06 pk  CreateRunStartExec           TN3081   TRunStartActionHandler requires CallStack
  08.05.06 pk  CreateMultipleItemsItemFromRunStep TN3087 Replaces CreatePipetteItemFromRunStep
  20.08.07 pk  CreateSpecialExec            TN3830   Removed.
  03.07.08 wl                                         TN4157
  08.09.08 pk                               TN4215   various changes
  06.11.08 pk                               TN4279   various changes
  09.12.08 pk                               TN4279   reference to GroupRunStepInfo removed
  06.04.09 pk                               TN4503   hints, warnings removed
  16.11.10 wl                               TN5351   uses ActionLow entfernt
  14.12.11 wl                               TN5765   uses geändert
  09.08.12 wl                               TN5946   stark vereinfacht
  ------------------------------------------------------------------------------------------------- }

unit ExecFactory;


interface


uses
    RunAction,
    Executable;

type
    TExecFactory = class
    public
        class function CreateExec(const aAction: TRunAction): IExecutable;
    end;


implementation


{ TExecFactory }

class function TExecFactory.CreateExec(const aAction: TRunAction): IExecutable;
begin
    result := aAction;
end;


end.
