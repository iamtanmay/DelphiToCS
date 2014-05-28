{ -----------------------------------------------------------------------------------------------------------------------
  Copyright © 2009 Zinsser Analytic GmbH - All rights reserved.
  Author       : Payman Kamali (pk)
  Description  :
  -----------------------------------------------------------------------------------------------------------------------
  Revision History:
  date     op  method                                track-no  improvement/change
  -------- --  ------------------------------------  --------- ----------------------------------------------------------
  27.08.09 pk                                        TN4753     Initial Revision
  18.06.12 wl                                        TN5899   hat keine eigene TLiquids-Instanz mehr
  ----------------------------------------------------------------------------------------------------------------------- }

unit DesignLiquids;


interface


uses
    Liquids;

type
    TDesignLiquids = record
    public
        class procedure CreateInstance; static;
        class procedure DestroyInstance; static;
        class function Instance: TLiquids; static;
    end;


implementation


class procedure TDesignLiquids.CreateInstance();
begin
    // uDesignLiquidsInstance := TLiquids.Create();
end;

class procedure TDesignLiquids.DestroyInstance();
begin
    // uDesignLiquidsInstance.Free;
end;

class function TDesignLiquids.Instance(): TLiquids;
begin
    result := TLiquids.Instance;
end;


end.
