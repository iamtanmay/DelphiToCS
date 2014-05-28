{ -----------------------------------------------------------------------------------------------------------------------
  Revision History:
  date     op  method                                track-no  improvement/change
  -------- --  ------------------------------------  --------- ----------------------------------------------------------
  17.08.10 wl                                        TN5112   initial revision
  ----------------------------------------------------------------------------------------------------------------------- }

unit GetMACFromNB30;


interface


uses
    Types;

type
    TCurrentNB30Info = class
        class function GetMACAddress: TByteDynArray;
    end;


implementation


uses
    Windows,
    SysUtils,
    NB30;

{$IFDEF WARNDIRS}{$WARN UNSAFE_CODE OFF}{$ENDIF}

class function TCurrentNB30Info.GetMACAddress: TByteDynArray;
{ Gets MAC address of first ethernet adapter on computer. Returns '' if no
  ethernet adapter found. Based on code at MSDN knowledge base Q118623 article
  at http://support.microsoft.com/kb/q118623/ }
type
    // This type is defined in MSDN sample code, but tests have found this is
    // not needed (on XP Pro) and Adapter can be of type TAdapterStatus. This
    // method uses the type in case other OSs require it
    TAStat = packed record
        Adapt: TAdapterStatus;
        NameBuff: array [0 .. 29] of TNameBuffer;
    end;
var
    Adapter: TAStat; // info about a network adapter
    AdapterList: TLanaEnum; // numbers for current LAN adapters
    Ncb: TNCB; // network control block descriptor
    I: Integer; // loops thru all adapters in list

    // ---------------------------------------------------------------------------
    function NetBiosSucceeded(const RetCode: AnsiChar): Boolean;
    { Checks if call to NetBios API function succeeded.
      @param RetCode Return code of NetBios API call.
      @return True for successful return code, false otherwise.
    }
    begin
        Result := UCHAR(RetCode) = NRC_GOODRET;
    end;
// ---------------------------------------------------------------------------

begin
    // Assume not adapter
    SetLength(Result, 0);
    // Get list of adapters
    FillChar(Ncb, SizeOf(Ncb), 0);
    Ncb.ncb_command := AnsiChar(NCBENUM);
    Ncb.ncb_buffer := PAnsiChar(@AdapterList);
    Ncb.ncb_length := SizeOf(AdapterList);
    if not NetBiosSucceeded(Netbios(@Ncb)) then
        Exit;
    // Get status of each adapter, exiting when first valid one reached
    // MSDN cautions us not to assume lana[0] is valid
    for I := 0 to Pred(Integer(AdapterList.length)) do
    begin
        // reset the adapter
        FillChar(Ncb, SizeOf(Ncb), 0);
        Ncb.ncb_command := AnsiChar(NCBRESET);
        Ncb.ncb_lana_num := AdapterList.lana[I];
        if not NetBiosSucceeded(Netbios(@Ncb)) then
            Exit;
        // get status of adapter
        FillChar(Ncb, SizeOf(Ncb), 0);
        Ncb.ncb_command := AnsiChar(NCBASTAT);
        Ncb.ncb_lana_num := AdapterList.lana[i];
        Ncb.ncb_callname := '*               ';
        Ncb.ncb_buffer := PAnsiChar(@Adapter);
        Ncb.ncb_length := SizeOf(Adapter);
        if NetBiosSucceeded(Netbios(@Ncb)) then
        begin
            // we have a MAC address: return it
            SetLength(result, 6);
            result[0] := Ord(Adapter.Adapt.adapter_address[0]);
            result[1] := Ord(Adapter.Adapt.adapter_address[1]);
            result[2] := Ord(Adapter.Adapt.adapter_address[2]);
            result[3] := Ord(Adapter.Adapt.adapter_address[3]);
            result[4] := Ord(Adapter.Adapt.adapter_address[4]);
            result[5] := Ord(Adapter.Adapt.adapter_address[5]);
            EXIT;
        end;
    end;
end;
{$IFDEF WARNDIRS}{$WARN UNSAFE_CODE ON}{$ENDIF}


end.
