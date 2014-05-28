{ -----------------------------------------------------------------------------------------------------------
  Copyright © 2013 Zinsser Analytic GmbH - All rights reserved.
  Author       : Wolfgang Lyncke (wl)
  Description  :
  -----------------------------------------------------------------------------------------------------------
  Revision History:
  date     op  method                              track-no improvement/change
  -------- --  ----------------------------------  -------- -------------------------------------------------
  28.06.13 wl                                      TN6191    Initial Revision
  ----------------------------------------------------------------------------------------------------------- }

unit ParserIdentToJSONUtils;


interface


uses
    DBXJSON,
    ParserIdentDataType;

type
    TParserIdentToJSONUtils = record
        class function GetJSONValue(aParam: TArg): TJSONValue; static;
    end;


implementation


{ TParserIdentToJSONUtils }

class function TParserIdentToJSONUtils.GetJSONValue(aParam: TArg): TJSONValue;
var
    x: integer;
    xArray: TJSONArray;
    xCurrentArg: TArg;
begin
    if (aParam is TStrArg) then
        EXIT(TJSONString.Create(aParam.AsStr));

    if (aParam is TIntArg) then
        EXIT(TJSONNumber.Create(aParam.AsInt));

    if (aParam is TDblArg) then
        EXIT(TJSONNumber.Create(aParam.AsFloat));

    if (aParam is TBoolArg) then
    begin
        if aParam.AsBool then
            EXIT(TJSONTrue.Create)
        else
            EXIT(TJSONFalse.Create);
    end;

    if (aParam is TArrayArg) then
    begin
        xArray := TJSONArray.Create;
        for x := TArrayArg.MinUserIndex to (aParam as TArrayArg).MaxUserIndex do
        begin
            xCurrentArg := (aParam as TArrayArg).GetItemByUserIndex(x);
            if (xCurrentArg is TStrArg) then
                xArray.Add(xCurrentArg.AsStr)
            else if (xCurrentArg is TIntArg) then
                xArray.Add(xCurrentArg.AsInt)
            else if (xCurrentArg is TDblArg) then
                xArray.Add(xCurrentArg.AsFloat)
            else if (xCurrentArg is TBoolArg) then
                xArray.Add(xCurrentArg.AsBool);
        end;
        EXIT(xArray);
    end;

    EXIT(TJSONNull.Create);
end;


end.
