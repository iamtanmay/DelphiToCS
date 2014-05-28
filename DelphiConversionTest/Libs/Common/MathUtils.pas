{ -----------------------------------------------------------------------------------------------------------------------
  BASEUNIT!
  ---------
  Copyright © 2009 Zinsser Analytic GmbH - All rights reserved.
  Author       : Wolfgang Lyncke (wl)
  Description  :
  -----------------------------------------------------------------------------------------------------------------------
  Revision History:
  date     op  method                                track-no  improvement/change
  -------- --  ------------------------------------  --------- ----------------------------------------------------------
  14.11.09 wl                                        TN4869    Initial revision
  30.12.11 wl  Max/MinExtValue                       TN5773   neu
  ----------------------------------------------------------------------------------------------------------------------- }

unit MathUtils;


interface


type
    TMathUtils = class
    public
        class function MaxIntValue(const aArray: array of integer): integer;
        class function MaxDoubleValue(const aArray: array of double): double;
        class function MaxExtValue(const aArray: array of extended): extended;

        class function MinIntValue(const aArray: array of integer): integer;
        class function MinDoubleValue(const aArray: array of double): double;
        class function MinExtValue(const aArray: array of extended): extended;
    end;


implementation


uses
    Math;

{ TMathUtils }

class function TMathUtils.MaxDoubleValue(const aArray: array of double): double;
begin
    result := Math.MaxValue(aArray);
end;

class function TMathUtils.MaxExtValue(const aArray: array of extended): extended;
begin
    result := Math.MaxValue(aArray);
end;

class function TMathUtils.MaxIntValue(const aArray: array of integer): integer;
begin
    result := Math.MaxIntValue(aArray);
end;

class function TMathUtils.MinDoubleValue(const aArray: array of double): double;
begin
    result := Math.MinValue(aArray);
end;

class function TMathUtils.MinExtValue(const aArray: array of extended): extended;
begin
    result := Math.MinValue(aArray);
end;

class function TMathUtils.MinIntValue(const aArray: array of integer): integer;
begin
    result := Math.MinIntValue(aArray);
end;


end.
