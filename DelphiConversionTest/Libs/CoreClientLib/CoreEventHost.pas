{ -----------------------------------------------------------------------------------------------------------------------
  Copyright © 2009 Zinsser Analytic GmbH - All rights reserved.
  Author       : Payman Kamali (pk)
  Description  :
  -----------------------------------------------------------------------------------------------------------------------
  Revision History:
  date     op  method                                track-no  improvement/change
  -------- --  ------------------------------------  --------- ----------------------------------------------------------
  09.06.09 pk                                        TN4585.2    Initial Revision
  30.07.09 pk                                        TN4585.5    Various changes
  12.10.09 pk                                        TN4812      Some code moved to CoreEventMessageInfo
  ----------------------------------------------------------------------------------------------------------------------- }

unit CoreEventHost;


interface


uses
    Classes,
    ThreadClasses,
    ThreadUtils,
    GeneralTypes,
    CoreEventMessageInfo;

type

    TCoreEventCallback = procedure(const aMessageInfo: TCoreEventMessageInfo) of object;

    TCoreEventHost = class
    private
        fOnEventHostEventPending: TCoreEventCallback;
    public
        property OnEventHostEventPending: TCoreEventCallback read fOnEventHostEventPending
            write fOnEventHostEventPending;
    end;


implementation


end.
