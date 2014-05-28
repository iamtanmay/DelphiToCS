unit RunStepAnything;
{ --------------------------------------------------------------------------------------------------
  Copyright © 2008 Zinsser Analytic GmbH - All rights reserved.
  Author       : Wolfgang Lyncke (wl)
  Description  :
  --------------------------------------------------------------------------------------------------
  Revision History:
  date     op  method                       track-no  improvement/change
  -------- --  ---------------------------  --------  ----------------------------------------------
  08.01.08 wl                               TN3972    initial version
  -------------------------------------------------------------------------------------------------- }


interface


uses
    MethodStep;

type
    TRunStepAnything = class(TInterfacedObject)
    end;

    IRunStepBuilder = interface(IInterface)
        ['{A204A525-5A58-4B2D-AA1F-447371E03C33}']

    end;

    IRunStepExecuter = interface(IInterface)
        ['{C07C48AA-4494-4AEF-99F8-3F66FC8C9374}']

    end;

    TRunStepExecuter = class(TRunStepAnything)
    end;


implementation


end.
