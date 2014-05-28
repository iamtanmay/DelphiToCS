unit RunStepTypeInfo;


{ -----------------------------------------------------------------------------------------------------------------------
  Copyright © 2008 Zinsser Analytic GmbH - All rights reserved.
  Author       : Payman Kamali (pk)
  Description  :
  -----------------------------------------------------------------------------------------------------------------------
  Revision History:
  date     op  method                                track-no  improvement/change
  -------- --  ------------------------------------  --------- ----------------------------------------------------------
  17.02.09 pk  RegisterRunStepClasses                TN4232    New
  04.02.10 pk                                        TN4972    Changes for Restart
  ----------------------------------------------------------------------------------------------------------------------- }
interface


uses
    RunStep,
    TypeInfo;

type

    TRunStepCreator = class
    protected
        function DoCreateRunStep(): TRunStep; virtual; abstract;
    public
        function CreateRunStep(): TRunStep;
        procedure RegisterRunStepClass();
    end;

    TRunStepTypeInfo = class(TTypeInfo)
    protected
        fRunStepCreator: TRunStepCreator;
        procedure DoCreateRunStepCreator(); virtual; abstract;

    public
        constructor Create(const aTypeName, aTypeInfoVersion, aLibName, aLibVersion: string);
        property RunStepCreator: TRunStepCreator read fRunStepCreator;

    end;


implementation


{ TRunStepTypeInfo }

constructor TRunStepTypeInfo.Create(const aTypeName, aTypeInfoVersion, aLibName, aLibVersion: string);
begin
    inherited Create(aTypeName, aTypeInfoVersion, aLibName, aLibVersion);
    DoCreateRunStepCreator();
end;

function TRunStepCreator.CreateRunStep(): TRunStep;
begin
    result := DoCreateRunStep();
end;

procedure TRunStepCreator.RegisterRunStepClass;
begin
end;


end.
