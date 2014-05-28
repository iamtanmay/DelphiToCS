{ -----------------------------------------------------------------------------------------------------------------------
  Copyright © 2008 Zinsser Analytic GmbH - All rights reserved.
  Author       : Payman Kamali (pk)
  Description  :
  -----------------------------------------------------------------------------------------------------------------------
  Revision History:
  date     op  method                                track-no  improvement/change
  -------- --  ------------------------------------  --------- ----------------------------------------------------------
  14.08.13 wl                                        TN6218   verwendet TRunStepListIterator
  ----------------------------------------------------------------------------------------------------------------------- }

unit RunActionTypeInfo;


interface


uses
    RunAction,
    RunStep,
    TypeInfo;

type

    TRunActionCreator = class
    protected
        function GetIsValidCreatorForRunStep(const aRunStep: TRunStep): boolean; virtual; abstract;
        function DoCreateRunActionFromStep(const aRunStep: TRunStep): TRunAction; virtual;
        function DoCreateRunActionFromStepList(const aRunStepListIterator: TRunStepListIterator)
            : TRunAction; virtual;
    public
        function IsValideCreatorForRunStep(const aRunStep: TRunStep): boolean;
        function CreateRunActionFromStep(const aRunStep: TRunStep): TRunAction;
        function CreateRunActionFromStepList(const aRunStepListIterator: TRunStepListIterator): TRunAction;
    end;

    TRunActionTypeInfo = class(TTypeInfo)
    protected
        fRunActionCreator: TRunActionCreator;
        procedure DoCreateRunActionCreator(); virtual; abstract;
    public
        constructor Create(const aTypeName, aTypeInfoVersion, aLibName, aLibVersion: string);
        property RunActionCreator: TRunActionCreator read fRunActionCreator;
    end;


implementation


{ TRunActionCreator }

constructor TRunActionTypeInfo.Create(const aTypeName, aTypeInfoVersion, aLibName, aLibVersion: string);
begin
    inherited Create(aTypeName, aTypeInfoVersion, aLibName, aLibVersion);
    DoCreateRunActionCreator();
end;

function TRunActionCreator.CreateRunActionFromStep(const aRunStep: TRunStep): TRunAction;
begin
    result := DoCreateRunActionFromStep(aRunStep);
end;

function TRunActionCreator.DoCreateRunActionFromStepList(const aRunStepListIterator: TRunStepListIterator)
    : TRunAction;
var
    xStep: TRunStep;
begin
    ASSERT(not aRunStepListIterator.IsEOF);
    xStep := aRunStepListIterator.CurrentStep;

    result := DoCreateRunActionFromStep(xStep);
    result.RunStep := xStep;

    aRunStepListIterator.MoveNext();
end;

function TRunActionCreator.CreateRunActionFromStepList(const aRunStepListIterator: TRunStepListIterator)
    : TRunAction;
begin
    result := DoCreateRunActionFromStepList(aRunStepListIterator);
end;

function TRunActionCreator.IsValideCreatorForRunStep(const aRunStep: TRunStep): boolean;
begin
    result := GetIsValidCreatorForRunStep(aRunStep);
end;

function TRunActionCreator.DoCreateRunActionFromStep(const aRunStep: TRunStep): TRunAction;
begin
    result := nil;
    ASSERT(false, 'DoCreateRunActionFromStep function not overriden');
end;


end.
