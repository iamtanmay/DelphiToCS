{ -----------------------------------------------------------------------------------------------------------------------
  Revision History:
  date     op  method                                track-no  improvement/change
  -------- --  ------------------------------------  --------- ----------------------------------------------------------
  17.08.10 wl                                        TN5112   initial revision
  29.09.10 wl                                        TN5112   div. Änderungen
  08.11.10 wl  GetChosenMacAddresses                 TN5112   benutzt TStringArray
  10.02.11 wl  GetEthernetAddresses                  TN5476   auch Schnittstellen, die offline sind, werden in die Liste eingetragen
  10.04.13 wl                                        TN6045   uses geändert
  13.12.13 ts  GetEthernetAddresses                  TN6331   WLAN adaptors can also be used for licensing
  ----------------------------------------------------------------------------------------------------------------------- }

unit GetMACFromIFTable;


interface


uses
    classes,
    sysutils,
    Types,
    GeneralTypes;

const
    MAX_INTERFACE_NAME_LEN = $100;
    ERROR_SUCCESS = 0;
    MAXLEN_IFDESCR = $100;
    MAXLEN_PHYSADDR = 8;

    MIB_IF_OPER_STATUS_NON_OPERATIONAL = 0;
    MIB_IF_OPER_STATUS_UNREACHABLE = 1;
    MIB_IF_OPER_STATUS_DISCONNECTED = 2;
    MIB_IF_OPER_STATUS_CONNECTING = 3;
    MIB_IF_OPER_STATUS_CONNECTED = 4;
    MIB_IF_OPER_STATUS_OPERATIONAL = 5;

    MIB_IF_TYPE_OTHER = 1;
    MIB_IF_TYPE_ETHERNET = 6;
    MIB_IF_TYPE_TOKENRING = 9;
    MIB_IF_TYPE_FDDI = 15;
    MIB_IF_TYPE_PPP = 23;
    MIB_IF_TYPE_LOOPBACK = 24;
    MIB_IF_TYPE_SLIP = 28;
    MIB_IF_TYPE_ATM = 37;
    MIB_IF_TYPE_IEEE80211 = 71;
    MIB_IF_TYPE_TUNNEL = 131;
    MIB_IF_TYPE_IEEE1394 = 144;

    MIB_IF_ADMIN_STATUS_UP = 1;
    MIB_IF_ADMIN_STATUS_DOWN = 2;
    MIB_IF_ADMIN_STATUS_TESTING = 3;

type

    MIB_IFROW = record
        wszName: array [0 .. (MAX_INTERFACE_NAME_LEN - 1)] of WideChar;
        dwIndex: LongInt;
        dwType: LongInt;
        dwMtu: LongInt;
        dwSpeed: LongInt;
        dwPhysAddrLen: LongInt;
        bPhysAddr: array [0 .. (MAXLEN_PHYSADDR - 1)] of Byte;
        dwAdminStatus: LongInt;
        dwOperStatus: LongInt;
        dwLastChange: LongInt;
        dwInOctets: LongInt;
        dwInUcastPkts: LongInt;
        dwInNUcastPkts: LongInt;
        dwInDiscards: LongInt;
        dwInErrors: LongInt;
        dwInUnknownProtos: LongInt;
        dwOutOctets: LongInt;
        dwOutUcastPkts: LongInt;
        dwOutNUcastPkts: LongInt;
        dwOutDiscards: LongInt;
        dwOutErrors: LongInt;
        dwOutQLen: LongInt;
        dwDescrLen: LongInt;
        bDescr: array [0 .. (MAXLEN_IFDESCR - 1)] of AnsiChar;
    end;

    TIFTableReader = record
    private
        class procedure GetEthernetAddresses(aFullList, aMACAddresses: TStrings); static;
    public
        class procedure GetFullList(aFullList: TStrings); static;
        class procedure GetMacAddresses(aMACAddresses: TStrings); static;
        class function GetChosenMacAddresses(): TStringArray; static;
        class function AddressIsKnown(const aMACAddress: string): boolean; static;
    end;


implementation


function GetIfTable(pIfTable: Pointer; var pdwSize: LongInt; bOrder: LongInt): LongInt; stdcall;
    external 'IPHLPAPI.DLL';

class function TIFTableReader.AddressIsKnown(const aMACAddress: string): boolean;
var
    xMACAddresses: TStringArray;
    x: integer;
begin
    if (Length(aMACAddress) <> 12) then
        EXIT(false);

    xMACAddresses := TIFTableReader.GetChosenMacAddresses();
    for x := 0 to high(xMACAddresses) do
    begin
        if aMACAddress = xMACAddresses[x] then
            EXIT(true);
    end;
    EXIT(false);
end;

class function TIFTableReader.GetChosenMacAddresses(): TStringArray;
var
    xList, xChosenAdresses: TStringList;
    x: integer;
begin
    xList := TStringList.Create;
    try
        xList.Duplicates := TDuplicates.dupIgnore;
        TIFTableReader.GetMacAddresses(xList);
        xList.Sort;

        xChosenAdresses := TStringList.Create;
        try
            for x := 0 to xList.Count - 1 do
            begin
                if (x = 0) or (xList[x] <> xList[x - 1]) then
                    xChosenAdresses.Add(xList[x]);
            end;

            SetLength(result, xChosenAdresses.Count);
            for x := 0 to xChosenAdresses.Count - 1 do
            begin
                result[x] := xChosenAdresses[x];
            end;
        finally
            FreeAndNil(xChosenAdresses);
        end;
    finally
        FreeAndNil(xList);
    end;
end;

class procedure TIFTableReader.GetEthernetAddresses(aFullList, aMACAddresses: TStrings);
const
    _MAX_ROWS_ = 20;

type
    _IfTable = record
        nRows: LongInt;
        ifRow: array [1 .. _MAX_ROWS_] of MIB_IFROW;
    end;

var
    pIfTable: ^_IfTable;
    TableSize: LongInt;
    tmp: string;
    i, j: Integer;
    ErrCode: LongInt;
begin
    pIfTable := nil;
    // ------------------------------------------------------------
    try
        // -------------------------------------------------------
        // First: just get the buffer size.
        // TableSize returns the size needed.
        TableSize := 0; // Set to zero so the GetIfTabel function
        // won't try to fill the buffer yet,
        // but only return the actual size it needs.
        GetIfTable(pIfTable, TableSize, 1);
        if (TableSize < SizeOf(MIB_IFROW) + Sizeof(LongInt)) then
        begin
            Exit; // less than 1 table entry?!
        end; // if-end.

        // Second:
        // allocate memory for the buffer and retrieve the
        // entire table.
        GetMem(pIfTable, TableSize);
        ErrCode := GetIfTable(pIfTable, TableSize, 1);
        if ErrCode <> ERROR_SUCCESS then
        begin
            Exit; // OK, that did not work.
            // Not enough memory i guess.
        end; // if-end.

        // Read the ETHERNET addresses.
        for i := 1 to pIfTable^.nRows do
            try
                tmp := '';
                for j := 0 to pIfTable^.ifRow[i].dwPhysAddrLen - 1 do
                begin
                    // if j > 0 then tmp := tmp + '-';  keine Striche
                    tmp := tmp + format('%.2x', [pIfTable^.ifRow[i].bPhysAddr[j]]);
                end; // for-end.
                // -------------------------------------
                if Assigned(aFullList) then
                    aFullList.Add(tmp + '  (' + string(AnsiString(pIfTable^.ifRow[i].bDescr)) + ',    ' +
                        pIfTable^.ifRow[i].wszName + ', Type: ' + IntToStr(pIfTable^.ifRow[i].dwType) +
                        ', Status: ' + IntToStr(pIfTable^.ifRow[i].dwOperStatus) + ')');

                if Assigned(aMacAddresses) and (pIfTable^.ifRow[i].dwPhysAddrLen = 6) then
                begin
                    if (pIfTable^.ifRow[i].dwType = MIB_IF_TYPE_ETHERNET) or
                        (pIfTable^.ifRow[i].dwType = MIB_IF_TYPE_IEEE80211) then
                        aMacAddresses.Add(tmp);
                    // if (pIfTable^.ifRow[i].dwType = MIB_IF_TYPE_IEEE80211) and (pIfTable^.ifRow[i].dwOperStatus <> MIB_IF_OPER_STATUS_NON_OPERATIONAL) then
                    // aMacAddresses.Add( tmp );
                end;

            except
                Exit;
            end; // if-try-except-end.

    finally
        if Assigned(pIfTable) then
            FreeMem(pIfTable, TableSize);
    end; // if-try-finally-end.
end;

class procedure TIFTableReader.GetFullList(aFullList: TStrings);
begin
    GetEthernetAddresses(aFullList, nil);
end;

class procedure TIFTableReader.GetMacAddresses(aMACAddresses: TStrings);
begin
    GetEthernetAddresses(nil, aMACAddresses);
end;


end.
