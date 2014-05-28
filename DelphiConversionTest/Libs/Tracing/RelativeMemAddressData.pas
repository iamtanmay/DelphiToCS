unit RelativeMemAddressData;


{ -----------------------------------------------------------------------------------------------------------------------
  Copyright © 2009 Zinsser Analytic GmbH - All rights reserved.
  Author       : Payman Kamali (pk)
  Description  :
  -----------------------------------------------------------------------------------------------------------------------
  Revision History:
  date     op  method                                track-no  improvement/change
  -------- --  ------------------------------------  --------- ----------------------------------------------------------
  04.02.10 pk                                        TN4972    Various Changes
  ----------------------------------------------------------------------------------------------------------------------- }
interface


uses
    Streamable;

type
    TRelativeMemAddressData = class(TStreamable)
    private
        fLabelName: string;
        fRelativeAddress: integer;
    public
        constructor Create(); override;
    published
        property LabelName: string read fLabelName write fLabelName;
        property RelativeAddress: integer read fRelativeAddress write fRelativeAddress;
    end;


implementation


{ TRelativeMemAddressData }

constructor TRelativeMemAddressData.Create;
begin
    inherited Create();
    fLabelName := '';
    fRelativeAddress := -1;
end;


end.
