{ ------------------------------------------------------------------------------------------------------------
  Copyright © 2010 Zinsser Analytic GmbH - All rights reserved.
  Author       : Wolfgang Lyncke (wl)
  Description  :
  ------------------------------------------------------------------------------------------------------------
  Revision History:
  date     op  method                            track-no improvement/change
  -------- --  --------------------------------  -------- ----------------------------------------------------
  28.05.10 wl  DeleteRunLayout                   TN5116   ersetzt gmDeleteRunDB
  10.12.10 pk  DeleteRunLayout                   TN5389   double quotations changed to single quotations
  14.12.11 wl  MethodDelete                      TN5765   Beim Löschen einer Methode wird die Posinfo nicht mehr angerührt
  ------------------------------------------------------------------------------------------------------------ }

unit MethodDataUtilities;


interface


type
    TMethodDataUtilities = record
    public
        class procedure MethodDelete(const aMethodName: string); static;
    end;


implementation


uses
    SysUtils,
    LayoutDataAdaptor,
    MethodDataAdaptor,
    MethodSettings;

{ TMethodDataUtilities }

class procedure TMethodDataUtilities.MethodDelete(const aMethodName: string);
var
    xReason: string;
    xLayoutDA: TLayoutDataAdaptor;
    xMethodDA: TMethodDataAdaptor;
begin
    // Löschen wird durchgeführt
    xMethodDA := TMethodDataAdaptor.Create;
    try
        xMethodDA.DeleteName(aMethodName);
    finally
        FreeAndNil(xMethodDA);
    end;

    // MethodSettings löschen
    TMethodSettings.DeleteMethodSettings(aMethodName, xReason);

    // Löschen des zugehörigen Run-Layout
    xLayoutDA := TLayoutDataAdaptor.Create;
    try
        xLayoutDA.DeleteRun(aMethodName);
    finally
        FreeAndNil(xLayoutDA);
    end;
end;


end.
