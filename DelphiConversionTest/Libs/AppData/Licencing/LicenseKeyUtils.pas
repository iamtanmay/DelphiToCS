{ -----------------------------------------------------------------------------------------------------------------------
  Copyright © 2010 Zinsser Analytic GmbH - All rights reserved.
  Author       : Wolfgang Lyncke (wl)
  Description  :
  -----------------------------------------------------------------------------------------------------------------------
  Revision History:
  date     op  method                                track-no  improvement/change
  -------- --  ------------------------------------  --------- ----------------------------------------------------------
  17.08.10 wl                                        TN5112   initial revision
  29.09.10 wl                                        TN5112   div. Änderungen
  02.11.10 wl  CheckLicenseKey                       TN5112   Simulation wurde falsch geprüft
  05.11.10 wl  CreateMACArray                        TN5112   erzeugt ByteArray
  08.11.10 wl  CheckMACAddress                       TN5112   funktioniert jetzt auch offline
  12.11.10 wl                                        TN5112   neu: Staff Licence
  17.01.11 wl                                        TN5112   interne Änderungen
  ----------------------------------------------------------------------------------------------------------------------- }

unit LicenseKeyUtils;


interface


uses
    Types;

type
    TLicensePart = (
        // Byte 1
        licRunSystem = 1, licStaffLicense = 2, // Mitarbeiterlizenz
        licSophas = 4, licRedi = 8, licMoss = 16, licSymyx = 32, licCFR21Compliant = 64, licScheduler = 128,
        // Byte 2
        licTraySy = 256, licSami = 512);

    TLicenseRec = record
        Valid: boolean;
        MACAddress: TByteDynArray;
        UseExpiryDate: boolean;
        ExpiryDate: TDateTime;
        Parts: cardinal;
    end;

    TLicenseKeyUtils = record
    private const
        cCRCStartValue = $FFFFFFFF;
        class function GetLengthLicenseKey: integer; static;
        class function GetLengthPCKey: integer; static;
    public
        class function CheckMACAddress(aLicenseMACAddr: TByteDynArray): boolean; static;
        class function GetPCKey: string; static;
        class function CheckLicenseKey(const aKey: string; aRealMode: boolean; out oErrorText: string)
            : boolean; static;
        class function CreateNewLicenseKey(const aLicense: TLicenseRec): string; static;
        class function GetLicenseKeyOptions(const aLicenseKey: string): TLicenseRec; static;
        class procedure StoreLicenseKey(const aKey: string); static;
        class function ReadLicenseKey(): string; static;
        class function LicensePartValid(aAllLicenseParts: integer; aLicensePart: TLicensePart)
            : boolean; static;
        class function CreateMACArray(aByte1, aByte2, aByte3, aByte4, aByte5, aByte6: byte)
            : TByteDynArray; static;
        class function GetMACString(const aAdr: TByteDynArray): string; static;

        class property LengthLicenseKey: integer read GetLengthLicenseKey;
        class property LengthPCKey: integer read GetLengthPCKey;
    end;


implementation


uses
    Windows,
    Classes,
    SysUtils,
    Dialogs,
    Registry,
    GetMACFromIFTable,
    GetMACFromNB30,
    LicenseKeyMaker,
    CRC32,
    BlockCiphers,
    LicenseConfig,
    GeneralTypes;

{ TLicenseKeyUtils }

class function TLicenseKeyUtils.CheckLicenseKey(const aKey: string; aRealMode: boolean;
    out oErrorText: string): boolean;
var
    xLicenseRec: TLicenseRec;
begin
    xLicenseRec := GetLicenseKeyOptions(aKey);

    // Any key?
    if (aKey = '') then
    begin
        oErrorText := 'No License Key found!';
        EXIT(false);
    end;

    // Valid ?
    if not xLicenseRec.Valid then
    begin
        oErrorText := 'License Key is not valid!';
        EXIT(false);
    end;

    // Check MAC Address
    if not CheckMACAddress(xLicenseRec.MACAddress) then
    begin
        oErrorText := 'License Key is not valid for this PC!';
        EXIT(false);
    end;

    // Check Expiry Date
    if xLicenseRec.UseExpiryDate and (xLicenseRec.ExpiryDate < SysUtils.Now) then
    begin
        oErrorText := 'License has expired!';
        EXIT(false);
    end;

    // Check RealMode
    if aRealMode and not LicensePartValid(xLicenseRec.Parts, TLicensePart.licRunSystem) then
    begin
        oErrorText := 'License is only valid for simulation!';
        EXIT(false);
    end;

    oErrorText := '';
    EXIT(true);
end;

class function TLicenseKeyUtils.GetMACString(const aAdr: TByteDynArray): string;
begin
    if Length(aAdr) <> 6 then
        EXIT('');

    Result := Format('%.2x%.2x%.2x%.2x%.2x%.2x', // ohne Striche
        [aAdr[0], aAdr[1], aAdr[2], aAdr[3], aAdr[4], aAdr[5]]);
end;

class function TLicenseKeyUtils.CheckMACAddress(aLicenseMACAddr: TByteDynArray): boolean;
var
    xCurrentMAC, xLicenseMAC: string;
begin
    xLicenseMAC := GetMACString(aLicenseMACAddr);

    // Aktuelle Addresse prüfen
    xCurrentMAC := GetMACString(TCurrentNB30Info.GetMACAddress());
    if xCurrentMAC = xLicenseMAC then
        EXIT(true);

    // in IF-Table eingetragene Addressen prüfen (falls offline)
    if TIFTableReader.AddressIsKnown(xLicenseMAC) then
        EXIT(true);

    EXIT(false);
end;

class function TLicenseKeyUtils.CreateNewLicenseKey(const aLicense: TLicenseRec): string;
var
    xLicenseBytes1: TByteDynArray;
    xKeyPart1, xKeyPart2: int64;
    xYear, xMonth, xDay: Word;
    xCRCValue: cardinal;
    x: integer;
begin
    SetLength(xLicenseBytes1, 12);

    // MAC-Addresse (6 Bytes)
    for x := 0 to 5 do
        xLicenseBytes1[x] := aLicense.MACAddress[x];

    // Expiry Date (2 Bytes)
    if aLicense.UseExpiryDate then
    begin
        DecodeDate(aLicense.ExpiryDate, xYear, xMonth, xDay);
        xLicenseBytes1[6] := Byte((xYear - 2000) * 2 + ((xMonth - 1) div 6));
        // Au weia, wir bekommen ein Jahr 2127-Problem!
        xLicenseBytes1[7] := Byte(((((xMonth - 1) mod 6) + 1) * 32) + xDay);
    end
    else
    begin
        xLicenseBytes1[6] := 255;
        xLicenseBytes1[7] := 255;
    end;

    // LicenseParts (4 Bytes)
    xLicenseBytes1[8] := Byte(aLicense.Parts);
    xLicenseBytes1[9] := Byte(aLicense.Parts shr 8);
    xLicenseBytes1[10] := Byte(aLicense.Parts shr 16);
    xLicenseBytes1[11] := Byte(aLicense.Parts shr 24);

    // Checksum (4 Bytes)
    xCRCValue := cCRCStartValue;
    TCRC32.Calc(xLicenseBytes1, xCRCValue);

    // Encrypted Key erstellen
    xKeyPart1 := TLicenseKeyMaker.BytesToInt64(xLicenseBytes1);
    xKeyPart2 := aLicense.Parts + Int64(xCRCValue) shl 32;
    result := TLicenseKeyMaker.Int64ToEncryptedKey(xKeyPart1) + '-' + TLicenseKeyMaker.Int64ToEncryptedKey
        (xKeyPart2);
end;

class function TLicenseKeyUtils.GetLengthLicenseKey: integer;
begin
    result := 2 * TLicenseKeyMaker.cLengthHalfLicenseKey + 1;
end;

class function TLicenseKeyUtils.GetLengthPCKey: integer;
begin
    result := TLicenseKeyMaker.cLengthPCKey;
end;

class function TLicenseKeyUtils.GetLicenseKeyOptions(const aLicenseKey: string): TLicenseRec;
var
    xLicenseBytes1, xKeyBytes1, xKeyBytes2: TByteDynArray;
    xKeyPart1, xKeyPart2: int64;
    xYear, xMonth, xDay: Word;
    xCRCValue, xReadCRCValue: cardinal;
    x: integer;
begin
    try
        // Key Parts aus Encrypted Key lesen
        xKeyPart1 := TLicenseKeyMaker.EncryptedKeyToInt64
            (Copy(aLicenseKey, 1, TLicenseKeyMaker.cLengthHalfLicenseKey));
        xKeyPart2 := TLicenseKeyMaker.EncryptedKeyToInt64
            (Copy(aLicenseKey, TLicenseKeyMaker.cLengthHalfLicenseKey + 2,
            TLicenseKeyMaker.cLengthHalfLicenseKey));
        xKeyBytes1 := TLicenseKeyMaker.Int64ToBytes(xKeyPart1);
        xKeyBytes2 := TLicenseKeyMaker.Int64ToBytes(xKeyPart2);

        SetLength(xLicenseBytes1, 12);
        for x := 0 to 7 do
            xLicenseBytes1[x] := xKeyBytes1[x];
        for x := 0 to 3 do
            xLicenseBytes1[x + 8] := xKeyBytes2[x];
        xReadCRCValue := Int32(xKeyPart2 shr 32);

        // Checksum prüfen (4 Bytes)
        xCRCValue := cCRCStartValue;
        TCRC32.Calc(xLicenseBytes1, xCRCValue);
        if xCRCValue <> xReadCRCValue then
        begin
            result.Valid := false;
            EXIT;
        end;

        // LicenseParts (4 Bytes)
        result.Parts := Int32(xKeyPart2);

        // Expiry Date (2 Bytes)
        if ((xLicenseBytes1[6] = 255) and (xLicenseBytes1[7] = 255)) then
        begin
            result.UseExpiryDate := false;
            result.ExpiryDate := 0;
        end
        else
        begin
            result.UseExpiryDate := true;
            xYear := Word(xLicenseBytes1[6]) div 2 + 2000;
            xMonth := (Word(xLicenseBytes1[6]) mod 2) * 6 + (Word(xLicenseBytes1[7]) div 32);
            xDay := Word(xLicenseBytes1[7]) mod 32;
            result.ExpiryDate := EncodeDate(xYear, xMonth, xDay);
        end;

        // MAC-Addresse (6 Bytes)
        SetLength(result.MACAddress, 6);
        for x := 0 to 5 do
            result.MACAddress[x] := xLicenseBytes1[x];
    except
        result.Valid := false;
    end;
end;

class function TLicenseKeyUtils.CreateMACArray(aByte1, aByte2, aByte3, aByte4, aByte5, aByte6: byte)
    : TByteDynArray;
begin
    // MAC-Addresse (6 Bytes)
    SetLength(result, 6);
    result[0] := aByte1;
    result[1] := aByte2;
    result[2] := aByte3;
    result[3] := aByte4;
    result[4] := aByte5;
    result[5] := aByte6;
end;

class function TLicenseKeyUtils.GetPCKey: string;
var
    xCurrentAdr: TByteDynArray;
    xRawKey: string;
begin
    xCurrentAdr := TCurrentNB30Info.GetMACAddress();

    if TIFTableReader.AddressIsKnown(GetMACString(xCurrentAdr)) then
    begin
        xRawKey := TLicenseKeyMaker.BytesToKey(xCurrentAdr);
        EXIT(Copy(xRawKey, 1, TLicenseKeyMaker.cLengthPCKey));
    end;

    EXIT('');
end;

class function TLicenseKeyUtils.ReadLicenseKey: string;
var
    fLicenseConfigReaderWriter: TLicenseConfigReaderWriter;
begin
    try
        fLicenseConfigReaderWriter := TLicenseConfigReaderWriter.Create;
        try
            fLicenseConfigReaderWriter.Read();
            result := fLicenseConfigReaderWriter.LicenseConfig.LicenseKey;
        finally
            FreeAndNil(fLicenseConfigReaderWriter);
        end;
    except

    end;
end;

class procedure TLicenseKeyUtils.StoreLicenseKey(const aKey: string);
var
    fLicenseConfigReaderWriter: TLicenseConfigReaderWriter;
begin
    fLicenseConfigReaderWriter := TLicenseConfigReaderWriter.Create;
    try
        fLicenseConfigReaderWriter.LicenseConfig.LicenseKey := aKey;
        fLicenseConfigReaderWriter.Write();
    finally
        FreeAndNil(fLicenseConfigReaderWriter);
    end;
end;

class function TLicenseKeyUtils.LicensePartValid(aAllLicenseParts: integer;
    aLicensePart: TLicensePart): boolean;
begin
    result := (Integer(aLicensePart) and aAllLicenseParts <> 0);
end;


end.
