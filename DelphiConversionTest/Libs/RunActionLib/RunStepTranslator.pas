unit RunStepTranslator;


{ -----------------------------------------------------------------------------------------------------------------------
  Copyright © 2010 Zinsser Analytic GmbH - All rights reserved.
  Author       : Payman Kamali (pk)
  Description  :
  -----------------------------------------------------------------------------------------------------------------------
  Revision History:
  date     op  method                                track-no  improvement/change
  -------- --  ------------------------------------  --------- ----------------------------------------------------------
  04.02.10 pk                                        TN4972     Initial Revision
  ----------------------------------------------------------------------------------------------------------------------- }
interface


uses
    RunStep;

type

    TRunStepTranslator = class
    protected
        function DoTranslateSteps(const aSourceRunSteps: TRunStepList; const aDestRunSteps: TRunStepList)
            : boolean; virtual; abstract;
    public
        function TranslateSteps(const aSourceRunSteps: TRunStepList;
            const aDestRunSteps: TRunStepList): boolean;
    end;


implementation


{ TRunStepTranslator }

function TRunStepTranslator.TranslateSteps(const aSourceRunSteps, aDestRunSteps: TRunStepList): boolean;
begin
    result := DoTranslateSteps(aSourceRunSteps, aDestRunSteps);
end;


end.
