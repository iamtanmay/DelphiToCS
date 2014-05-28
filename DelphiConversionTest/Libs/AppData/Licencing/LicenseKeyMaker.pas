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
  17.01.11 wl                                        TN5112   interne Änderungen
  18.01.11 wl  cAllowedKeys                          TN5369   neu: Key kann nur noch 32 Zeichen enthalten (nur Großbuchstaben)
  18.01.11 wl  CharToBitmap                          TN5369   auch Kleinschreibung wird akzeptiert
  ----------------------------------------------------------------------------------------------------------------------- }

unit LicenseKeyMaker;


interface


uses
    SysUtils,
    Types;

type
    TLicenseKeyMaker = record
    private const
        cZinsserKey: array [0 .. 7] of byte = (244, 234, 33, 44, 55, 66, 77, 88);

    const
        cAllowedKeys = 'LB7XDJSRETQ9K4WU2PZAGN8MV5FH3Y6C'; // 32 characters = 5Bit
        // 01234567890123456789012345678901
        // 10        20        30

        class function BitmapToChar(aBitmap: int64): char; static;
        class function CharToBitmap(aChar: Char): int64; static;

        class function EightCharsToBitmap(aChar1, aChar2, aChar3, aChar4, aChar5, aChar6, aChar7,
            aChar8: Char): int64; static;
        class procedure BitmapToEightChars(aBitmap: int64; out oChar1, oChar2, oChar3, oChar4, oChar5, oChar6,
            oChar7, oChar8: Char); static;

        class function FiveBytesToBitmap(aByte1, aByte2, aByte3, aByte4, aByte5: Byte): int64; static;
        class procedure BitmapToFiveBytes(aBitmap: int64; out oByte1, oByte2, oByte3, oByte4,
            oByte5: Byte); static;

    public
        class function BytesToKey(const aBytes: array of Byte): string; static;
        class function KeyToBytes(const aKey: string): TByteDynArray; static;

        class function BytesToInt64(const aBytes: array of Byte): Int64; static;
        class function Int64ToBytes(const aKey: Int64): TByteDynArray; static;

        class function Int64ToEncryptedKey(const aValue: Int64): string; static;
        class function EncryptedKeyToInt64(const aKey: string): Int64; static;

    const
        cLengthHalfLicenseKey = 13;

    const
        cLengthPCKey = 10;
    end;


implementation


uses
    BlockCiphers;

{ TLicenseKeyMaker }

class function TLicenseKeyMaker.BitmapToChar(aBitmap: int64): char;
begin
    if (aBitmap > 31) then
        raise Exception.Create('Wrong bitmap value');

    result := cAllowedKeys[aBitmap + 1]
end;

class procedure TLicenseKeyMaker.BitmapToEightChars(aBitmap: int64;
    out oChar1, oChar2, oChar3, oChar4, oChar5, oChar6, oChar7, oChar8: Char);
begin
    oChar1 := BitmapToChar(aBitmap and 31);
    oChar2 := BitmapToChar((aBitmap shr 5) and 31);
    oChar3 := BitmapToChar((aBitmap shr 10) and 31);
    oChar4 := BitmapToChar((aBitmap shr 15) and 31);
    oChar5 := BitmapToChar((aBitmap shr 20) and 31);
    oChar6 := BitmapToChar((aBitmap shr 25) and 31);
    oChar7 := BitmapToChar((aBitmap shr 30) and 31);
    oChar8 := BitmapToChar((aBitmap shr 35) and 31);
end;

class procedure TLicenseKeyMaker.BitmapToFiveBytes(aBitmap: int64;
    out oByte1, oByte2, oByte3, oByte4, oByte5: Byte);
begin
    oByte1 := aBitmap and 255;
    oByte2 := (aBitmap shr 8) and 255;
    oByte3 := (aBitmap shr 16) and 255;
    oByte4 := (aBitmap shr 24) and 255;
    oByte5 := (aBitmap shr 32) and 255;

    // letztes Byte muss leer sein
    ASSERT((aBitmap shr 40) and 255 = 0, 'Last byte not empty');
end;

class function TLicenseKeyMaker.CharToBitmap(aChar: Char): int64;
var
    x: integer;
    xUpChar: char;
begin
    if aChar = ' ' then
        EXIT(0);
    if aChar = '' then
        EXIT(0);

    case aChar of
        'a' .. 'z': // aus Klein- wird Großschreibung
            xUpChar := Char(Word(aChar) xor $0020);
        else
            xUpChar := aChar;
    end;

    for x := 1 to Length(cAllowedKeys) do
    begin
        if (xUpChar = cAllowedKeys[x]) then
            EXIT(x - 1);
    end;

    raise Exception.Create('Character is not known!');
end;

class function TLicenseKeyMaker.EightCharsToBitmap(aChar1, aChar2, aChar3, aChar4, aChar5, aChar6, aChar7,
    aChar8: Char): int64;
begin
    result := CharToBitmap(aChar1) + (CharToBitmap(aChar2) shl 5) + (CharToBitmap(aChar3) shl 10) +
        (CharToBitmap(aChar4) shl 15) + (CharToBitmap(aChar5) shl 20) + (CharToBitmap(aChar6) shl 25) +
        (CharToBitmap(aChar7) shl 30) + (CharToBitmap(aChar8) shl 35);
end;

class function TLicenseKeyMaker.Int64ToBytes(const aKey: Int64): TByteDynArray;
begin
    SetLength(result, 8);
    result[0] := aKey;
    result[1] := aKey shr 8;
    result[2] := aKey shr 16;
    result[3] := aKey shr 24;
    result[4] := aKey shr 32;
    result[5] := aKey shr 40;
    result[6] := aKey shr 48;
    result[7] := aKey shr 56;
end;

class function TLicenseKeyMaker.Int64ToEncryptedKey(const aValue: Int64): string;
var
    xEncryptedBlock: int64;
    xLicenseBytes: TByteDynArray;
    xBC: TBlowfishCipher;
begin
    xBC := TBlowfishCipher.Create(cZinsserKey, 8);
    try
        xEncryptedBlock := xBC.EncryptedBlock(aValue);
    finally
        FreeAndNil(xBC);
    end;

    xLicenseBytes := TLicenseKeyMaker.Int64ToBytes(xEncryptedBlock);
    result := Copy(TLicenseKeyMaker.BytesToKey(xLicenseBytes), 1, cLengthHalfLicenseKey);
    // 11 Zeichen reichen bei 8 Bytes
end;

class function TLicenseKeyMaker.FiveBytesToBitmap(aByte1, aByte2, aByte3, aByte4, aByte5: Byte): int64;
begin
    result := Int64(aByte1) + (Int64(aByte2) shl 8) + (Int64(aByte3) shl 16) + (Int64(aByte4) shl 24) +
        (Int64(aByte5) shl 32);
end;

class function TLicenseKeyMaker.KeyToBytes(const aKey: string): TByteDynArray;
var
    xByte1, xByte2, xByte3, xByte4, xByte5: Byte;
    xBitmap: int64;
    x, xLength: integer;
begin
    SetLength(result, 0);
    for x := 1 to Length(aKey) do
    begin
        if (x mod 8 = 1) then
        begin

            if (x = Length(aKey)) then
            begin
                xBitmap := EightCharsToBitmap(aKey[x], cAllowedKeys[1], cAllowedKeys[1], cAllowedKeys[1],
                    cAllowedKeys[1], cAllowedKeys[1], cAllowedKeys[1], cAllowedKeys[1]);
            end
            else if (x + 1 = Length(aKey)) then
            begin
                xBitmap := EightCharsToBitmap(aKey[x], aKey[x + 1], cAllowedKeys[1], cAllowedKeys[1],
                    cAllowedKeys[1], cAllowedKeys[1], cAllowedKeys[1], cAllowedKeys[1]);
            end
            else if (x + 2 = Length(aKey)) then
            begin
                xBitmap := EightCharsToBitmap(aKey[x], aKey[x + 1], aKey[x + 2], cAllowedKeys[1],
                    cAllowedKeys[1], cAllowedKeys[1], cAllowedKeys[1], cAllowedKeys[1]);
            end
            else if (x + 3 = Length(aKey)) then
            begin
                xBitmap := EightCharsToBitmap(aKey[x], aKey[x + 1], aKey[x + 2], aKey[x + 3], cAllowedKeys[1],
                    cAllowedKeys[1], cAllowedKeys[1], cAllowedKeys[1]);
            end
            else if (x + 4 = Length(aKey)) then
            begin
                xBitmap := EightCharsToBitmap(aKey[x], aKey[x + 1], aKey[x + 2], aKey[x + 3], aKey[x + 4],
                    cAllowedKeys[1], cAllowedKeys[1], cAllowedKeys[1]);
            end
            else if (x + 5 = Length(aKey)) then
            begin
                xBitmap := EightCharsToBitmap(aKey[x], aKey[x + 1], aKey[x + 2], aKey[x + 3], aKey[x + 4],
                    aKey[x + 5], cAllowedKeys[1], cAllowedKeys[1]);
            end
            else if (x + 6 = Length(aKey)) then
            begin
                xBitmap := EightCharsToBitmap(aKey[x], aKey[x + 1], aKey[x + 2], aKey[x + 3], aKey[x + 4],
                    aKey[x + 5], aKey[x + 6], cAllowedKeys[1]);
            end
            else
            begin
                xBitmap := EightCharsToBitmap(aKey[x], aKey[x + 1], aKey[x + 2], aKey[x + 3], aKey[x + 4],
                    aKey[x + 5], aKey[x + 6], aKey[x + 7]);
            end;

            BitmapToFiveBytes(xBitmap, xByte1, xByte2, xByte3, xByte4, xByte5);

            xLength := Length(result); // Bisherige Länge
            SetLength(result, xLength + 5); // Array vergrößern
            result[xLength] := xByte1;
            result[xLength + 1] := xByte2;
            result[xLength + 2] := xByte3;
            result[xLength + 3] := xByte4;
            result[xLength + 4] := xByte5;
        end;
    end;
end;

class function TLicenseKeyMaker.EncryptedKeyToInt64(const aKey: string): Int64;
var
    xEncryptedBlock: int64;
    xLicenseBytes: TByteDynArray;
    xBC: TBlowfishCipher;
begin
    if (aKey = '') then
        EXIT(0);

    xLicenseBytes := TLicenseKeyMaker.KeyToBytes(aKey);
    xEncryptedBlock := TLicenseKeyMaker.BytesToInt64(xLicenseBytes);

    xBC := TBlowfishCipher.Create(cZinsserKey, 8);
    try
        result := xBC.DecryptedBlock(xEncryptedBlock);
    finally
        FreeAndNil(xBC);
    end;
end;

class function TLicenseKeyMaker.BytesToInt64(const aBytes: array of Byte): Int64;
begin
    // wir gehen davon aus, dass es 8 Bytes gibt:
    ASSERT(Length(aBytes) >= 8, 'Not enough data in Byte array');

    result := aBytes[0];
    result := result + Int64(aBytes[1]) shl 8;
    result := result + Int64(aBytes[2]) shl 16;
    result := result + Int64(aBytes[3]) shl 24;
    result := result + Int64(aBytes[4]) shl 32;
    result := result + Int64(aBytes[5]) shl 40;
    result := result + Int64(aBytes[6]) shl 48;
    result := result + Int64(aBytes[7]) shl 56;
end;

class function TLicenseKeyMaker.BytesToKey(const aBytes: array of byte): string;
var
    xChr1, xChr2, xChr3, xChr4, xChr5, xChr6, xChr7, xChr8: Char;
    xBitmap: int64;
    x: integer;
begin
    result := '';
    for x := 0 to high(aBytes) do
    begin
        if (x mod 5 = 0) then
        begin
            if (x = high(aBytes)) then
            begin
                xBitmap := FiveBytesToBitmap(aBytes[x], 0, 0, 0, 0);
            end
            else if (x + 1 = high(aBytes)) then
            begin
                xBitmap := FiveBytesToBitmap(aBytes[x], aBytes[x + 1], 0, 0, 0);
            end
            else if (x + 2 = high(aBytes)) then
            begin
                xBitmap := FiveBytesToBitmap(aBytes[x], aBytes[x + 1], aBytes[x + 2], 0, 0);
            end
            else if (x + 3 = high(aBytes)) then
            begin
                xBitmap := FiveBytesToBitmap(aBytes[x], aBytes[x + 1], aBytes[x + 2], aBytes[x + 3], 0);
            end
            else
            begin
                xBitmap := FiveBytesToBitmap(aBytes[x], aBytes[x + 1], aBytes[x + 2], aBytes[x + 3],
                    aBytes[x + 4]);
            end;

            BitmapToEightChars(xBitmap, xChr1, xChr2, xChr3, xChr4, xChr5, xChr6, xChr7, xChr8);

            result := result + xChr1 + xChr2 + xChr3 + xChr4 + xChr5 + xChr6 + xChr7 + xChr8;
        end;
    end;
end;


end.
