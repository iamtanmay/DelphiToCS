unit RunStepInfoFactory;
{ -----------------------------------------------------------------------------------------------------------------------
  Copyright © 2008 Zinsser Analytic GmbH - All rights reserved.
  Author       : Payman Kamali (pk)
  Description  :
  -----------------------------------------------------------------------------------------------------------------------
  Revision History:
  date     op  method                                track-no  improvement/change
  -------- --  ------------------------------------  --------- ----------------------------------------------------------
  08.09.08 pk                                        TN4215    Initial Revision
  20.09.08 pk  CreateRunStepInfoByTypeName           TN4215    if typeinfo not found create TUnknownRunStepInfo
  ----------------------------------------------------------------------------------------------------------------------- }


interface


uses
    RunStepInfo;

type
    TRunStepInfoFactory = class
    public
        class function CreateRunStepInfoByTypeName(const aTypeName: string): TRunStepInfo;
        class function GetIconIndex(const aActionName: string): integer;
    end;


implementation


uses
    MethodTypes,
    TypeInfo,
    RunStepInfoTypeInfo,
    RunStepInfoTypeDictionary;

class function TRunStepInfoFactory.CreateRunStepInfoByTypeName(const aTypeName: string): TRunStepInfo;
var
    xTypeInfo: TTypeInfo;
begin
    xTypeInfo := TRunStepInfoTypeDictionary.Instance.GetTypeFromTypeName(aTypeName);
    if (xTypeInfo is TRunStepInfoTypeInfo) then
        result := (xTypeInfo as TRunStepInfoTypeInfo).RunStepInfoCreator.CreateRunStepInfo()
    else
        result := TUnknownRunStepInfo.Create();
end;

class function TRunStepInfoFactory.GetIconIndex(const aActionName: string): integer;
var
    xRunStepInfo: TRunStepInfo;
begin
    result := INT_IM_INDEX_ACTION_UNKNOWN;

    xRunStepInfo := self.CreateRunStepInfoByTypeName(aActionName);
    if not Assigned(xRunStepInfo) then
        EXIT;
    try
        result := xRunStepInfo.IconIndex;
    finally
        xRunStepInfo.Free;
    end;
end;


end.
