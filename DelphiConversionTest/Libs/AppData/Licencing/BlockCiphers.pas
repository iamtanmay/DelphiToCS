unit BlockCiphers;

{
  Copyright (c) 1998-1999 Dave Shapiro, Professional Software, Inc.
  (daves@cfxc.com) Use and modify freely. Keep this header, please.

  BlockCiphers class hierarchy:


  TBlockCipher
  (abstract)
  |
  |
  T64BitBlockCipher
  (abstract)
  |
  --------------------------------------
  |                                      |
  TDESCipher                         TBlowfishCipher


  How it works: TBlockCipher introduces functionality for transforming blocks
  of plaintext into blocks of ciphertext. Specifically, the abstract methods
  EncryptBlock and DecryptBlock are declared for descendants to fill out.
  EncryptStream and DecryptStream are methods completed by TBlockCipher for
  encrypting and decrypting TStreams.

  It's not typesafe: Making this stuff into a single hierarchy requires some
  unsafe typing. First, a block cipher can use any size block. DES and Blowfish
  use 64-bit blocks, but there are many ciphers that use 128-bit blocks. As
  such, EncryptBlock and DecryptBlock take an untyped parameter, and it is left
  to the user of the class to ensure that they're passing the right size block.

  If the user is not sure what the block size is, they should call the virtual
  class method BlockSize.

  There is a similar type-safety problem with the constructor. Some ciphers
  take a 64-bit value as their key. Some take a range in length. At any rate,
  I thought it would be important (or at least cool) to have a virtual
  constructor in this hierarchy. Again, this means sacrificing type safety. In
  the constructor, you pass the key as an untyped const, and the length of
  the key in bytes. The class trusts that the user is sending consistent
  information. TBlockCipher introduces two virtual class methods, MinKeyLength
  and MaxKeyLength for querying key-length information.

  You may be interested in ensuring that this code is correct. There are many
  test vector suites out there. I've tested all ciphers against a lot of stuff;
  it's all correct. If you do choose to verify this stuff on your own, you
  should be aware of endian problems. Intels are little-endian machines, and
  most other stuff is big-endian. Most test suites assume a big-endian
  architecture. At any rate, you can spend all afternoon swapping bytes, trying
  to get things so they agree exactly with others' results. I've done it. It's
  not fun. (Take a peek at the stuff in the '$DEFINE UseTestVectors' part, in
  the Main unit.) The end results are that these ciphers agree with 'official'
  results up to byte-ordering. (Remember that byte-ordering issues occur at
  all points of the encryption, so don't expect to just swap the resulting
  ciphertext bytes and get exactly what the 'official' test vectors say.)
  Chances are, these ciphers aren't compatible across machines, or even with
  other implementations, without some tweaking, which causes a slight
  performance degredation.
}

(*$J-*)
(*$IFDEF VER80*)
(*$DEFINE PreDelphi4*)
(*$DEFINE PreDelphi3*)
(*$ENDIF*)
(*$IFDEF VER90*)
(*$DEFINE PreDelphi4*)
(*$DEFINE PreDelphi3*)
(*$ENDIF*)
(*$IFDEF VER100*)
(*$DEFINE PreDelphi4*)
(*$ENDIF*)


interface


uses
    SysUtils,
    Classes;

type
    PDWORD = ^DWORD;
    DWORD = (*$IFDEF PreDelphi4*)
        Longint (*$ELSE*)
        Longword (*$ENDIF*)
        ;
(*$IFDEF PreDelphi4*)
    PInt64 = ^Int64;
    Int64 = Comp;
(*$ENDIF*)

    TDoubleDWORD = packed record
        L, R: DWORD;
    end;

    TFourByte = packed record
        B1, B2, B3, B4: Byte;
    end;

    TBlockCipher = class(TObject)
    private
    public
        constructor Create(const Key; const Length: Integer); virtual;
        class function CiphertextLength(const PlaintextLength: Longint): Longint; virtual;
        procedure EncryptStream(const Plaintext, Ciphertext: TStream); virtual;
        procedure DecryptStream(const Ciphertext, Plaintext: TStream); virtual;
        procedure EncryptBlock(var Plaintext); virtual; abstract;
        procedure DecryptBlock(var Ciphertext); virtual; abstract;
        class function MinKeyLength: Integer; virtual; abstract;
        class function MaxKeyLength: Integer; virtual;
        class function BlockSize: Integer; virtual; abstract;
    end;

    TBlockCipherClass = class of TBlockCipher;

    T64BitBlockCipher = class(TBlockCipher)
    public
        function EncryptedBlock(const Plaintext: Int64): Int64; virtual; abstract;
        function DecryptedBlock(const Ciphertext: Int64): Int64; virtual; abstract;
        function EncryptedString(const Plaintext: string): string; virtual;
        function DecryptedString(const Ciphertext: string): string; virtual;
        class function BlockSize: Integer; override;
    end;

    T64BitBlockCipherClass = class of T64BitBlockCipher;

    {
      TDESCipher: Implements the Data Encryption Standard block cipher.

      Current performance figures, for a Pentium II, 400 MHz, 128 MB SDRAM:
      4.2 megabytes/sec encryption/decryption rate.

      Many, many thanks to Bob Lee for lots of help optimizing this thing.
    }

    TDESKeyScheduleRange = 0 .. 15;
    TSixBitArray = array [0 .. 7] of DWORD;
    TDESKeySchedule = array [TDESKeyScheduleRange] of TSixBitArray;

    TDESCipher = class(T64BitBlockCipher)
    private
        FKeySchedule: TDESKeySchedule;
        procedure CreateKeySchedule(const Key: Int64);
    protected
    public
        constructor Create(const Key; const Length: Integer); override;
        destructor Destroy; override;
        procedure EncryptBlock(var Plaintext); override;
        procedure DecryptBlock(var Ciphertext); override;
        function EncryptedBlock(const Plaintext: Int64): Int64; override;
        function DecryptedBlock(const Ciphertext: Int64): Int64; override;
        class function MinKeyLength: Integer; override;
    end;

    {
      TDESCipher: Implements Bruce Schneier's Blowfish block cipher.

      Current performance figures, for a Pentium II, 400 MHz, 128 MB SDRAM:
      6.5 megabytes/sec encryption/decryption rate.
    }

    TBlowfishSBox = array [Byte] of DWORD;
    TBlowfishPArray = array [0 .. 17] of DWORD;

    TBlowfishCipher = class(T64BitBlockCipher)
    private
        FSBox1, FSBox2, FSBox3, FSBox4: TBlowfishSBox;
        FPArray: TBlowfishPArray;
        procedure GenerateSubkeys(const Key; const Length: Integer);
    public
        constructor Create(const Key; const Length: Integer); override;
        destructor Destroy; override;
        procedure EncryptBlock(var Plaintext); override;
        procedure DecryptBlock(var Ciphertext); override;
        function EncryptedBlock(const Plaintext: Int64): Int64; override;
        function DecryptedBlock(const Ciphertext: Int64): Int64; override;
        class function MinKeyLength: Integer; override;
        class function MaxKeyLength: Integer; override;
    end;

    {
      procedure SwapInt64(var N: Int64);
      procedure ReverseInt64(var N: Int64);
    }


implementation


{
  procedure ReverseInt64(var N: Int64);
  var
  A: array [1..8] of Byte absolute N;
  T: Byte;
  begin
  T := A[1]; A[1] := A[8]; A[8] := T;
  T := A[2]; A[2] := A[7]; A[7] := T;
  T := A[3]; A[3] := A[6]; A[6] := T;
  T := A[4]; A[4] := A[5]; A[5] := T;
  end;

  procedure SwapInt64(var N: Int64);
  var
  D: TDoubleDWORD absolute N;
  T: DWORD;
  begin
  T := D.L;
  D.L := D.R;
  D.R := T;
  end;
}


// ------------------------------- TBlockCipher ---------------------------------

constructor TBlockCipher.Create(const Key; const Length: Integer);
begin
    inherited Create;
    if Length < MinKeyLength then
    begin
        raise Exception.CreateFmt('Key must be at least %d bytes long.', [MinKeyLength]);
    end;
end;

const
    BlocksPerBuf = 512;

procedure TBlockCipher.EncryptStream(const Plaintext, Ciphertext: TStream);
var
    Count: Longint;
    ThisBlockSize, BufSize, I, N: Integer;
    Buf: Pointer;
    P: ^Byte;
    LastBuf: Boolean;
begin
    P := nil; // Suppresses superfluous compiler warning.
    Count := 0; // Ditto.
    ThisBlockSize := BlockSize;
    BufSize := ThisBlockSize * BlocksPerBuf;
    GetMem(Buf, BufSize);
    while True do
    begin
        Count := Plaintext.Read(Buf^, BufSize);
        P := Buf;
        LastBuf := Count < BufSize;
        if LastBuf then
            N := Count div ThisBlockSize
        else
            N := BlocksPerBuf;
        for I := 1 to N do
        begin
            EncryptBlock(P^);
            Inc(P, ThisBlockSize);
        end;
        if LastBuf then
            Break;
        Ciphertext.Write(Buf^, Count);
    end;
    // We're at the end of the data in a not-completely-full-buffer (or, in the
    // case of Plaintext.Size mod BufSize = 0, at the beginning of an empty
    // buffer, which is just a special case of the former). Now we use the last
    // byte of the current block to give the number of extra padding bytes.
    // This will be a number from 1..ThisBlockSize. Specifically, if the
    // Plaintext length is an exact multiple of ThisBlockSize, the number of
    // extra padding bytes will be ThisBlockSize, i.e. the entire final block
    // is junk. In any other case, the last block has at least some ciphertext.
    Inc(P, ThisBlockSize - 1);
    P^ := Byte(ThisBlockSize - Count mod ThisBlockSize);
    Inc(Count, P^);
    Dec(P, ThisBlockSize - 1);
    EncryptBlock(P^);
    Ciphertext.Write(Buf^, Count);
    FreeMem(Buf);
end;

procedure TBlockCipher.DecryptStream(const Ciphertext, Plaintext: TStream);
var
    Count: Longint;
    ThisBlockSize, BufSize, I, J: Integer;
    Buf: Pointer;
    P: ^Byte;
begin
    ThisBlockSize := BlockSize;
    Count := Ciphertext.Size - Ciphertext.Position;
    if (Count = 0) or (Count mod ThisBlockSize <> 0) then
    begin
        raise Exception.CreateFmt('Ciphertext length is not a multiple of %d.', [ThisBlockSize]);
    end;
    BufSize := ThisBlockSize * BlocksPerBuf;
    GetMem(Buf, BufSize);
    for I := 1 to Count div BufSize do
    begin
        Ciphertext.Read(Buf^, BufSize);
        P := Buf;
        for J := 1 to BlocksPerBuf do
        begin
            DecryptBlock(P^);
            Inc(P, ThisBlockSize);
        end;
        Plaintext.Write(Buf^, BufSize);
    end;
    Count := Count mod BufSize;
    Ciphertext.Read(Buf^, Count);
    P := Buf;
    for J := 1 to Count div ThisBlockSize do
    begin
        DecryptBlock(P^);
        Inc(P, ThisBlockSize);
    end;
    Dec(P);
    Plaintext.Write(Buf^, Count - P^);
    FreeMem(Buf);
end;

class function TBlockCipher.CiphertextLength(const PlaintextLength: Longint): Longint;
begin
    Result := Succ(PlaintextLength div BlockSize) * BlockSize;
end;

class function TBlockCipher.MaxKeyLength: Integer;
begin
    Result := MinKeyLength;
end;


// ---------------------------- T64BitBlockCipher -------------------------------

function T64BitBlockCipher.EncryptedString(const Plaintext: string): string;
var
    PS, PD: PInt64;
    Source: Int64;
    I: Integer;
    NumBlocks: Longint;
    NumPadBytes: Byte;
begin
    NumBlocks := Length(Plaintext) div SizeOf(Int64);
    NumPadBytes := SizeOf(Int64) - Length(Plaintext) mod SizeOf(Int64);
    SetLength(Result, Succ(NumBlocks) * SizeOf(Int64));
    PS := Pointer(Plaintext);
    PD := Pointer(Result);
    for I := 1 to NumBlocks do
    begin
        PD^ := EncryptedBlock(PS^);
        Inc(PS);
        Inc(PD);
    end;
    {
      Fill in the number of padding bytes. Just write the whole block, and then
      overwrite the beginning bytes with Source.
    }
    FillChar(Source, SizeOf(Source), NumPadBytes);
    {
      What if PS points to the end of the string? Won't dereferencing it cause
      a memory problem? Not really. For one, the string will always have a
      trailing null, so there's always one byte, which avoids an AV. Also,
      since PS^ is passed as an untyped var, the compiler will just pass the
      address without dereferencing.
    }
    Move(PS^, Source, SizeOf(Int64) - NumPadBytes);
    PD^ := EncryptedBlock(Source);
end;

function T64BitBlockCipher.DecryptedString(const Ciphertext: string): string;
var
    Dest: Int64;
    PS, PD: PInt64;
    I: Integer;
    NumCiphertextBytes: Longint;
    NumPadBytes: Byte;
begin
    NumCiphertextBytes := Length(Ciphertext);
    if (NumCiphertextBytes = 0) or (NumCiphertextBytes mod SizeOf(Int64) <> 0) then
    begin
        raise Exception.CreateFmt('Ciphertext is not a multiple of %d bytes.', [SizeOf(Int64)]);
    end;
    { Decrypt last block first. This tells us how many padding bytes there are. }
    PS := Pointer(Ciphertext);
    Inc(PS, Pred(NumCiphertextBytes div SizeOf(Int64)));
    Dest := DecryptedBlock(PS^);
    NumPadBytes := TFourByte(TDoubleDWORD(Dest).R).B4;
    SetLength(Result, NumCiphertextBytes - NumPadBytes);
    { From the last block, move only the non-padding bytes to the end of Result. }
    Move(Dest, Result[NumCiphertextBytes - SizeOf(Int64) + 1], SizeOf(Int64) - NumPadBytes);
    PS := Pointer(Ciphertext);
    PD := Pointer(Result);
    for I := 1 to Length(Result) div SizeOf(Int64) do
    begin
        PD^ := DecryptedBlock(PS^);
        Inc(PS);
        Inc(PD);
    end;
end;

class function T64BitBlockCipher.BlockSize: Integer;
begin
    Result := SizeOf(Int64);
end;


// -------------------------------- TDESCipher ----------------------------------

const
    LowSixBits = $3F;
    LowTwoBits = $03;

function CircularSHL28(const X: DWORD; const Amount: Byte): DWORD;
{
  Pre: Amount < BitsInX.
  Post: Result is an unsigned circular left shift of X by Amount bytes.
}
const
    BitLength = 28;
    { The high nibble needs to be cleared. }
    Mask = not(Pred(1 shl (SizeOf(X) - BitLength)) shl BitLength);
begin
    Result := X shl Amount and Mask or X shr (BitLength - Amount);
end;

type
    TPBox = array [Byte] of DWORD;

const
    PBox1: TPBox = ($00000000, $00004000, $40000000, $40004000, $00000010, $00004010, $40000010, $40004010,
        $00080000, $00084000, $40080000, $40084000, $00080010, $00084010, $40080010, $40084010, $00000002,
        $00004002, $40000002, $40004002, $00000012, $00004012, $40000012, $40004012, $00080002, $00084002,
        $40080002, $40084002, $00080012, $00084012, $40080012, $40084012, $00000200, $00004200, $40000200,
        $40004200, $00000210, $00004210, $40000210, $40004210, $00080200, $00084200, $40080200, $40084200,
        $00080210, $00084210, $40080210, $40084210, $00000202, $00004202, $40000202, $40004202, $00000212,
        $00004212, $40000212, $40004212, $00080202, $00084202, $40080202, $40084202, $00080212, $00084212,
        $40080212, $40084212, $00008000, $0000C000, $40008000, $4000C000, $00008010, $0000C010, $40008010,
        $4000C010, $00088000, $0008C000, $40088000, $4008C000, $00088010, $0008C010, $40088010, $4008C010,
        $00008002, $0000C002, $40008002, $4000C002, $00008012, $0000C012, $40008012, $4000C012, $00088002,
        $0008C002, $40088002, $4008C002, $00088012, $0008C012, $40088012, $4008C012, $00008200, $0000C200,
        $40008200, $4000C200, $00008210, $0000C210, $40008210, $4000C210, $00088200, $0008C200, $40088200,
        $4008C200, $00088210, $0008C210, $40088210, $4008C210, $00008202, $0000C202, $40008202, $4000C202,
        $00008212, $0000C212, $40008212, $4000C212, $00088202, $0008C202, $40088202, $4008C202, $00088212,
        $0008C212, $40088212, $4008C212, $00800000, $00804000, $40800000, $40804000, $00800010, $00804010,
        $40800010, $40804010, $00880000, $00884000, $40880000, $40884000, $00880010, $00884010, $40880010,
        $40884010, $00800002, $00804002, $40800002, $40804002, $00800012, $00804012, $40800012, $40804012,
        $00880002, $00884002, $40880002, $40884002, $00880012, $00884012, $40880012, $40884012, $00800200,
        $00804200, $40800200, $40804200, $00800210, $00804210, $40800210, $40804210, $00880200, $00884200,
        $40880200, $40884200, $00880210, $00884210, $40880210, $40884210, $00800202, $00804202, $40800202,
        $40804202, $00800212, $00804212, $40800212, $40804212, $00880202, $00884202, $40880202, $40884202,
        $00880212, $00884212, $40880212, $40884212, $00808000, $0080C000, $40808000, $4080C000, $00808010,
        $0080C010, $40808010, $4080C010, $00888000, $0088C000, $40888000, $4088C000, $00888010, $0088C010,
        $40888010, $4088C010, $00808002, $0080C002, $40808002, $4080C002, $00808012, $0080C012, $40808012,
        $4080C012, $00888002, $0088C002, $40888002, $4088C002, $00888012, $0088C012, $40888012, $4088C012,
        $00808200, $0080C200, $40808200, $4080C200, $00808210, $0080C210, $40808210, $4080C210, $00888200,
        $0088C200, $40888200, $4088C200, $00888210, $0088C210, $40888210, $4088C210, $00808202, $0080C202,
        $40808202, $4080C202, $00808212, $0080C212, $40808212, $4080C212, $00888202, $0088C202, $40888202,
        $4088C202, $00888212, $0088C212, $40888212, $4088C212);

    PBox2: TPBox = ($00000000, $80000000, $00400000, $80400000, $00001000, $80001000, $00401000, $80401000,
        $00000040, $80000040, $00400040, $80400040, $00001040, $80001040, $00401040, $80401040, $04000000,
        $84000000, $04400000, $84400000, $04001000, $84001000, $04401000, $84401000, $04000040, $84000040,
        $04400040, $84400040, $04001040, $84001040, $04401040, $84401040, $00000004, $80000004, $00400004,
        $80400004, $00001004, $80001004, $00401004, $80401004, $00000044, $80000044, $00400044, $80400044,
        $00001044, $80001044, $00401044, $80401044, $04000004, $84000004, $04400004, $84400004, $04001004,
        $84001004, $04401004, $84401004, $04000044, $84000044, $04400044, $84400044, $04001044, $84001044,
        $04401044, $84401044, $00010000, $80010000, $00410000, $80410000, $00011000, $80011000, $00411000,
        $80411000, $00010040, $80010040, $00410040, $80410040, $00011040, $80011040, $00411040, $80411040,
        $04010000, $84010000, $04410000, $84410000, $04011000, $84011000, $04411000, $84411000, $04010040,
        $84010040, $04410040, $84410040, $04011040, $84011040, $04411040, $84411040, $00010004, $80010004,
        $00410004, $80410004, $00011004, $80011004, $00411004, $80411004, $00010044, $80010044, $00410044,
        $80410044, $00011044, $80011044, $00411044, $80411044, $04010004, $84010004, $04410004, $84410004,
        $04011004, $84011004, $04411004, $84411004, $04010044, $84010044, $04410044, $84410044, $04011044,
        $84011044, $04411044, $84411044, $00000100, $80000100, $00400100, $80400100, $00001100, $80001100,
        $00401100, $80401100, $00000140, $80000140, $00400140, $80400140, $00001140, $80001140, $00401140,
        $80401140, $04000100, $84000100, $04400100, $84400100, $04001100, $84001100, $04401100, $84401100,
        $04000140, $84000140, $04400140, $84400140, $04001140, $84001140, $04401140, $84401140, $00000104,
        $80000104, $00400104, $80400104, $00001104, $80001104, $00401104, $80401104, $00000144, $80000144,
        $00400144, $80400144, $00001144, $80001144, $00401144, $80401144, $04000104, $84000104, $04400104,
        $84400104, $04001104, $84001104, $04401104, $84401104, $04000144, $84000144, $04400144, $84400144,
        $04001144, $84001144, $04401144, $84401144, $00010100, $80010100, $00410100, $80410100, $00011100,
        $80011100, $00411100, $80411100, $00010140, $80010140, $00410140, $80410140, $00011140, $80011140,
        $00411140, $80411140, $04010100, $84010100, $04410100, $84410100, $04011100, $84011100, $04411100,
        $84411100, $04010140, $84010140, $04410140, $84410140, $04011140, $84011140, $04411140, $84411140,
        $00010104, $80010104, $00410104, $80410104, $00011104, $80011104, $00411104, $80411104, $00010144,
        $80010144, $00410144, $80410144, $00011144, $80011144, $00411144, $80411144, $04010104, $84010104,
        $04410104, $84410104, $04011104, $84011104, $04411104, $84411104, $04010144, $84010144, $04410144,
        $84410144, $04011144, $84011144, $04411144, $84411144);

    PBox3: TPBox = ($00000000, $00002000, $00200000, $00202000, $00000008, $00002008, $00200008, $00202008,
        $10000000, $10002000, $10200000, $10202000, $10000008, $10002008, $10200008, $10202008, $20000000,
        $20002000, $20200000, $20202000, $20000008, $20002008, $20200008, $20202008, $30000000, $30002000,
        $30200000, $30202000, $30000008, $30002008, $30200008, $30202008, $00000080, $00002080, $00200080,
        $00202080, $00000088, $00002088, $00200088, $00202088, $10000080, $10002080, $10200080, $10202080,
        $10000088, $10002088, $10200088, $10202088, $20000080, $20002080, $20200080, $20202080, $20000088,
        $20002088, $20200088, $20202088, $30000080, $30002080, $30200080, $30202080, $30000088, $30002088,
        $30200088, $30202088, $00040000, $00042000, $00240000, $00242000, $00040008, $00042008, $00240008,
        $00242008, $10040000, $10042000, $10240000, $10242000, $10040008, $10042008, $10240008, $10242008,
        $20040000, $20042000, $20240000, $20242000, $20040008, $20042008, $20240008, $20242008, $30040000,
        $30042000, $30240000, $30242000, $30040008, $30042008, $30240008, $30242008, $00040080, $00042080,
        $00240080, $00242080, $00040088, $00042088, $00240088, $00242088, $10040080, $10042080, $10240080,
        $10242080, $10040088, $10042088, $10240088, $10242088, $20040080, $20042080, $20240080, $20242080,
        $20040088, $20042088, $20240088, $20242088, $30040080, $30042080, $30240080, $30242080, $30040088,
        $30042088, $30240088, $30242088, $01000000, $01002000, $01200000, $01202000, $01000008, $01002008,
        $01200008, $01202008, $11000000, $11002000, $11200000, $11202000, $11000008, $11002008, $11200008,
        $11202008, $21000000, $21002000, $21200000, $21202000, $21000008, $21002008, $21200008, $21202008,
        $31000000, $31002000, $31200000, $31202000, $31000008, $31002008, $31200008, $31202008, $01000080,
        $01002080, $01200080, $01202080, $01000088, $01002088, $01200088, $01202088, $11000080, $11002080,
        $11200080, $11202080, $11000088, $11002088, $11200088, $11202088, $21000080, $21002080, $21200080,
        $21202080, $21000088, $21002088, $21200088, $21202088, $31000080, $31002080, $31200080, $31202080,
        $31000088, $31002088, $31200088, $31202088, $01040000, $01042000, $01240000, $01242000, $01040008,
        $01042008, $01240008, $01242008, $11040000, $11042000, $11240000, $11242000, $11040008, $11042008,
        $11240008, $11242008, $21040000, $21042000, $21240000, $21242000, $21040008, $21042008, $21240008,
        $21242008, $31040000, $31042000, $31240000, $31242000, $31040008, $31042008, $31240008, $31242008,
        $01040080, $01042080, $01240080, $01242080, $01040088, $01042088, $01240088, $01242088, $11040080,
        $11042080, $11240080, $11242080, $11040088, $11042088, $11240088, $11242088, $21040080, $21042080,
        $21240080, $21242080, $21040088, $21042088, $21240088, $21242088, $31040080, $31042080, $31240080,
        $31242080, $31040088, $31042088, $31240088, $31242088);

    PBox4: TPBox = ($00000000, $00000800, $00020000, $00020800, $00000020, $00000820, $00020020, $00020820,
        $08000000, $08000800, $08020000, $08020800, $08000020, $08000820, $08020020, $08020820, $02000000,
        $02000800, $02020000, $02020800, $02000020, $02000820, $02020020, $02020820, $0A000000, $0A000800,
        $0A020000, $0A020800, $0A000020, $0A000820, $0A020020, $0A020820, $00000400, $00000C00, $00020400,
        $00020C00, $00000420, $00000C20, $00020420, $00020C20, $08000400, $08000C00, $08020400, $08020C00,
        $08000420, $08000C20, $08020420, $08020C20, $02000400, $02000C00, $02020400, $02020C00, $02000420,
        $02000C20, $02020420, $02020C20, $0A000400, $0A000C00, $0A020400, $0A020C00, $0A000420, $0A000C20,
        $0A020420, $0A020C20, $00100000, $00100800, $00120000, $00120800, $00100020, $00100820, $00120020,
        $00120820, $08100000, $08100800, $08120000, $08120800, $08100020, $08100820, $08120020, $08120820,
        $02100000, $02100800, $02120000, $02120800, $02100020, $02100820, $02120020, $02120820, $0A100000,
        $0A100800, $0A120000, $0A120800, $0A100020, $0A100820, $0A120020, $0A120820, $00100400, $00100C00,
        $00120400, $00120C00, $00100420, $00100C20, $00120420, $00120C20, $08100400, $08100C00, $08120400,
        $08120C00, $08100420, $08100C20, $08120420, $08120C20, $02100400, $02100C00, $02120400, $02120C00,
        $02100420, $02100C20, $02120420, $02120C20, $0A100400, $0A100C00, $0A120400, $0A120C00, $0A100420,
        $0A100C20, $0A120420, $0A120C20, $00000001, $00000801, $00020001, $00020801, $00000021, $00000821,
        $00020021, $00020821, $08000001, $08000801, $08020001, $08020801, $08000021, $08000821, $08020021,
        $08020821, $02000001, $02000801, $02020001, $02020801, $02000021, $02000821, $02020021, $02020821,
        $0A000001, $0A000801, $0A020001, $0A020801, $0A000021, $0A000821, $0A020021, $0A020821, $00000401,
        $00000C01, $00020401, $00020C01, $00000421, $00000C21, $00020421, $00020C21, $08000401, $08000C01,
        $08020401, $08020C01, $08000421, $08000C21, $08020421, $08020C21, $02000401, $02000C01, $02020401,
        $02020C01, $02000421, $02000C21, $02020421, $02020C21, $0A000401, $0A000C01, $0A020401, $0A020C01,
        $0A000421, $0A000C21, $0A020421, $0A020C21, $00100001, $00100801, $00120001, $00120801, $00100021,
        $00100821, $00120021, $00120821, $08100001, $08100801, $08120001, $08120801, $08100021, $08100821,
        $08120021, $08120821, $02100001, $02100801, $02120001, $02120801, $02100021, $02100821, $02120021,
        $02120821, $0A100001, $0A100801, $0A120001, $0A120801, $0A100021, $0A100821, $0A120021, $0A120821,
        $00100401, $00100C01, $00120401, $00120C01, $00100421, $00100C21, $00120421, $00120C21, $08100401,
        $08100C01, $08120401, $08120C01, $08100421, $08100C21, $08120421, $08120C21, $02100401, $02100C01,
        $02120401, $02120C01, $02100421, $02100C21, $02120421, $02120C21, $0A100401, $0A100C01, $0A120401,
        $0A120C01, $0A100421, $0A100C21, $0A120421, $0A120C21);

type
    TSBox = array [0 .. 63] of Integer;

const
    SBox1: TSBox = (224, 0, 64, 240, 208, 112, 16, 64, 32, 224, 240, 32, 176, 208, 128, 16, 48, 160, 160, 96,
        96, 192, 192, 176, 80, 144, 144, 80, 0, 48, 112, 128, 64, 240, 16, 192, 224, 128, 128, 32, 208, 64,
        96, 144, 32, 16, 176, 112, 240, 80, 192, 176, 144, 48, 112, 224, 48, 160, 160, 0, 80, 96, 0, 208);

    SBox2: TSBox = (15, 3, 1, 13, 8, 4, 14, 7, 6, 15, 11, 2, 3, 8, 4, 14, 9, 12, 7, 0, 2, 1, 13, 10, 12, 6, 0,
        9, 5, 11, 10, 5, 0, 13, 14, 8, 7, 10, 11, 1, 10, 3, 4, 15, 13, 4, 1, 2, 5, 11, 8, 6, 12, 7, 6, 12, 9,
        0, 3, 5, 2, 14, 15, 9);

    SBox3: TSBox = (160, 208, 0, 112, 144, 0, 224, 144, 96, 48, 48, 64, 240, 96, 80, 160, 16, 32, 208, 128,
        192, 80, 112, 224, 176, 192, 64, 176, 32, 240, 128, 16, 208, 16, 96, 160, 64, 208, 144, 0, 128, 96,
        240, 144, 48, 128, 0, 112, 176, 64, 16, 240, 32, 224, 192, 48, 80, 176, 160, 80, 224, 32, 112, 192);

    SBox4: TSBox = (7, 13, 13, 8, 14, 11, 3, 5, 0, 6, 6, 15, 9, 0, 10, 3, 1, 4, 2, 7, 8, 2, 5, 12, 11, 1, 12,
        10, 4, 14, 15, 9, 10, 3, 6, 15, 9, 0, 0, 6, 12, 10, 11, 1, 7, 13, 13, 8, 15, 9, 1, 4, 3, 5, 14, 11, 5,
        12, 2, 7, 8, 2, 4, 14);

    SBox5: TSBox = (32, 224, 192, 176, 64, 32, 16, 192, 112, 64, 160, 112, 176, 208, 96, 16, 128, 80, 80, 0,
        48, 240, 240, 160, 208, 48, 0, 144, 224, 128, 144, 96, 64, 176, 32, 128, 16, 192, 176, 112, 160, 16,
        208, 224, 112, 32, 128, 208, 240, 96, 144, 240, 192, 0, 80, 144, 96, 160, 48, 64, 0, 80, 224, 48);

    SBox6: TSBox = (12, 10, 1, 15, 10, 4, 15, 2, 9, 7, 2, 12, 6, 9, 8, 5, 0, 6, 13, 1, 3, 13, 4, 14, 14, 0, 7,
        11, 5, 3, 11, 8, 9, 4, 14, 3, 15, 2, 5, 12, 2, 9, 8, 5, 12, 15, 3, 10, 7, 11, 0, 14, 4, 1, 10, 7, 1,
        6, 13, 0, 11, 8, 6, 13);

    SBox7: TSBox = (64, 208, 176, 0, 32, 176, 224, 112, 240, 64, 0, 144, 128, 16, 208, 160, 48, 224, 192, 48,
        144, 80, 112, 192, 80, 32, 160, 240, 96, 128, 16, 96, 16, 96, 64, 176, 176, 208, 208, 128, 192, 16,
        48, 64, 112, 160, 224, 112, 160, 144, 240, 80, 96, 0, 128, 240, 0, 224, 80, 32, 144, 48, 32, 192);

    SBox8: TSBox = (13, 1, 2, 15, 8, 13, 4, 8, 6, 10, 15, 3, 11, 7, 1, 4, 10, 12, 9, 5, 3, 6, 14, 11, 5, 0, 0,
        14, 12, 9, 7, 2, 7, 2, 11, 1, 4, 14, 1, 7, 9, 4, 12, 10, 14, 8, 2, 13, 0, 15, 6, 12, 10, 9, 13, 0, 15,
        3, 3, 5, 5, 6, 8, 11);

function ExpandedSubstitutedAndPermutedDWORD(const D: DWORD; const K: TSixBitArray): DWORD;
var
    X, Y: DWORD;
begin
    { First row takes bits 32, and bits 1 - 5. }
    X := K[0] xor (D shl 5 and $20 or D shr 27 and $1F);
    { Next row takes bits 4 - 9. }
    Y := K[1] xor D shr 23 and LowSixBits;
    Result := PBox1(.SBox1[X] or SBox2[Y].);

    { Next row takes bits 8 - 13. }
    X := K[2] xor D shr 19 and LowSixBits;
    { Next row takes bits 12 - 17. }
    Y := K[3] xor D shr 15 and LowSixBits;
    Result := Result or PBox2(.SBox3[X] or SBox4[Y].);

    { Next row takes bits 16 - 21. }
    X := K[4] xor D shr 11 and LowSixBits;
    { Bits 20 - 25. }
    Y := K[5] xor D shr 7 and LowSixBits;
    Result := Result or PBox3(.SBox5[X] or SBox6[Y].);

    { Bits 24 - 29. }
    X := K[6] xor D shr 3 and LowSixBits;
    { Bits 28 - 32,        bit 1. }
    Y := K[7] xor (D shl 1 and $3E or D shr 31 and $3F);
    Result := Result or PBox4(.SBox7[X] or SBox8[Y].);
end;

function BitSelection(const Block: Int64; const A; const ASize: Integer): DWORD;
var
    I: Integer;
    ShiftAmount: Integer;
    H, L: DWORD;
    PA: ^Integer;
begin
    Result := 0;
    PA := Addr(A);
    H := TDoubleDWORD(Block).R;
    L := TDoubleDWORD(Block).L;
    for I := Pred(ASize) downto 0 do
    begin
        ShiftAmount := PA^;
        if ShiftAmount > 31 then
            Result := Result or H shr (ShiftAmount - 32) and 1 shl I
        else
        begin
            Result := Result or L shr ShiftAmount and 1 shl I;
        end;
        Inc(PA);
    end;
end;

function PC2(const C, D: DWORD): Int64;
const
    MapL: array [0 .. 31] of Byte = (24, 27, 20, 6, 14, 10, 3, 22, 0, 17, 7, 12, 8, 23, 11, 5, 16, 26, 1, 9,
        19, 25, 4, 15, 26, 15, 8, 1, 21, 12, 20, 2);
    MapH: array [0 .. 15] of Byte = (24, 16, 9, 5, 18, 7, 22, 13, 0, 25, 23, 27, 4, 17, 11, 14);
var
    I: Integer;
    ResultHL: TDoubleDWORD absolute Result;
begin
    {
      C and D are 28-bit halves. Thus if bit needed is more than 28, we
      need to go to the high DWORD, namely C. Fortunately, all bits
      greater than 28 occur between Map[1] and Map[24] inclusive, so we can
      optimize this by breaking up the loops.
    }
    Result := 0;
    { First fill in the high 16 bits, which are the low 16 bits of KeyHL.H. }
    for I := high(MapH) downto low(MapH) do
    begin
        ResultHL.R := ResultHL.R or C shr MapH[I] and 1 shl I;
    end;
    { Now fill in the next 8 bits, which are the high 8 bits of KeyHL.L. }
    for I := high(MapL) downto high(MapL) - 7 do
    begin
        ResultHL.L := ResultHL.L or C shr MapL[I] and 1 shl I;
    end;
    { Finally fill in the low 24 bits, which are the low 24 bits of KeyHL.L. }
    for I := high(MapL) - 8 downto low(MapL) do
    begin
        ResultHL.L := ResultHL.L or D shr MapL[I] and 1 shl I;
    end;
end;

constructor TDESCipher.Create(const Key; const Length: Integer);
begin
    inherited;
    CreateKeySchedule(Int64(Key));
end;

destructor TDESCipher.Destroy;
begin
    inherited;
end;

procedure TDESCipher.CreateKeySchedule(const Key: Int64);
type
    THalfArray = array [0 .. 27] of Integer;
const
    V: array [TDESKeyScheduleRange] of Byte = (1, 1, 2, 2, 2, 2, 2, 2, 1, 2, 2, 2, 2, 2, 2, 1);
    PC1C: THalfArray = (7, 15, 23, 31, 39, 47, 55, 63, 6, 14, 22, 30, 38, 46, 54, 62, 5, 13, 21, 29, 37, 45,
        53, 61, 4, 12, 20, 28);
    PC1D: THalfArray = (1, 9, 17, 25, 33, 41, 49, 57, 2, 10, 18, 26, 34, 42, 50, 58, 3, 11, 19, 27, 35, 43,
        51, 59, 36, 44, 52, 60);
var
    C, D: DWORD;
    I, J: Integer;
    {
      I could just use a TDoubleDWORD, but doing this caused an internal error
      C3677 in D2. Using an absolute, ironically, caused this to go away.
      (Normally absolutes create tons of internal errors.)
    }
    K1: Int64;
    K: TDoubleDWORD absolute K1;
begin
    C := BitSelection(Key, PC1C, high(PC1C) - low(PC1C) + 1);
    D := BitSelection(Key, PC1D, high(PC1D) - low(PC1D) + 1);
    J := high(V);
    for I := low(V) to high(V) do
    begin
        C := CircularSHL28(C, V[I]);
        D := CircularSHL28(D, V[I]);
        { Select 48 bits from the concatenation of C and D. (C is the high DWORD.) }
        K1 := PC2(C, D);
        { Pre-calc the six-bit chunks and store them. }
        FKeySchedule[J][0] := K.R shr 10 and LowSixBits;
        FKeySchedule[J][1] := K.R shr 4 and LowSixBits;
        FKeySchedule[J][2] := K.R shl 2 and LowSixBits or K.L shr 30 and LowTwoBits;
        FKeySchedule[J][3] := K.L shr 24 and LowSixBits;
        FKeySchedule[J][4] := K.L shr 18 and LowSixBits;
        FKeySchedule[J][5] := K.L shr 12 and LowSixBits;
        FKeySchedule[J][6] := K.L shr 6 and LowSixBits;
        FKeySchedule[J][7] := K.L and LowSixBits;
        Dec(J);
    end;
end;

type
    TInitialPermutation = array [Byte] of DWORD;

const
    IP: TInitialPermutation = ($00000000, $00000080, $00000008, $00000088, $00008000, $00008080, $00008008,
        $00008088, $00000800, $00000880, $00000808, $00000888, $00008800, $00008880, $00008808, $00008888,
        $00800000, $00800080, $00800008, $00800088, $00808000, $00808080, $00808008, $00808088, $00800800,
        $00800880, $00800808, $00800888, $00808800, $00808880, $00808808, $00808888, $00080000, $00080080,
        $00080008, $00080088, $00088000, $00088080, $00088008, $00088088, $00080800, $00080880, $00080808,
        $00080888, $00088800, $00088880, $00088808, $00088888, $00880000, $00880080, $00880008, $00880088,
        $00888000, $00888080, $00888008, $00888088, $00880800, $00880880, $00880808, $00880888, $00888800,
        $00888880, $00888808, $00888888, $80000000, $80000080, $80000008, $80000088, $80008000, $80008080,
        $80008008, $80008088, $80000800, $80000880, $80000808, $80000888, $80008800, $80008880, $80008808,
        $80008888, $80800000, $80800080, $80800008, $80800088, $80808000, $80808080, $80808008, $80808088,
        $80800800, $80800880, $80800808, $80800888, $80808800, $80808880, $80808808, $80808888, $80080000,
        $80080080, $80080008, $80080088, $80088000, $80088080, $80088008, $80088088, $80080800, $80080880,
        $80080808, $80080888, $80088800, $80088880, $80088808, $80088888, $80880000, $80880080, $80880008,
        $80880088, $80888000, $80888080, $80888008, $80888088, $80880800, $80880880, $80880808, $80880888,
        $80888800, $80888880, $80888808, $80888888, $08000000, $08000080, $08000008, $08000088, $08008000,
        $08008080, $08008008, $08008088, $08000800, $08000880, $08000808, $08000888, $08008800, $08008880,
        $08008808, $08008888, $08800000, $08800080, $08800008, $08800088, $08808000, $08808080, $08808008,
        $08808088, $08800800, $08800880, $08800808, $08800888, $08808800, $08808880, $08808808, $08808888,
        $08080000, $08080080, $08080008, $08080088, $08088000, $08088080, $08088008, $08088088, $08080800,
        $08080880, $08080808, $08080888, $08088800, $08088880, $08088808, $08088888, $08880000, $08880080,
        $08880008, $08880088, $08888000, $08888080, $08888008, $08888088, $08880800, $08880880, $08880808,
        $08880888, $08888800, $08888880, $08888808, $08888888, $88000000, $88000080, $88000008, $88000088,
        $88008000, $88008080, $88008008, $88008088, $88000800, $88000880, $88000808, $88000888, $88008800,
        $88008880, $88008808, $88008888, $88800000, $88800080, $88800008, $88800088, $88808000, $88808080,
        $88808008, $88808088, $88800800, $88800880, $88800808, $88800888, $88808800, $88808880, $88808808,
        $88808888, $88080000, $88080080, $88080008, $88080088, $88088000, $88088080, $88088008, $88088088,
        $88080800, $88080880, $88080808, $88080888, $88088800, $88088880, $88088808, $88088888, $88880000,
        $88880080, $88880008, $88880088, $88888000, $88888080, $88888008, $88888088, $88880800, $88880880,
        $88880808, $88880888, $88888800, $88888880, $88888808, $88888888);

    IPInv: TInitialPermutation = ($00000000, $02000000, $00020000, $02020000, $00000200, $02000200, $00020200,
        $02020200, $00000002, $02000002, $00020002, $02020002, $00000202, $02000202, $00020202, $02020202,
        $01000000, $03000000, $01020000, $03020000, $01000200, $03000200, $01020200, $03020200, $01000002,
        $03000002, $01020002, $03020002, $01000202, $03000202, $01020202, $03020202, $00010000, $02010000,
        $00030000, $02030000, $00010200, $02010200, $00030200, $02030200, $00010002, $02010002, $00030002,
        $02030002, $00010202, $02010202, $00030202, $02030202, $01010000, $03010000, $01030000, $03030000,
        $01010200, $03010200, $01030200, $03030200, $01010002, $03010002, $01030002, $03030002, $01010202,
        $03010202, $01030202, $03030202, $00000100, $02000100, $00020100, $02020100, $00000300, $02000300,
        $00020300, $02020300, $00000102, $02000102, $00020102, $02020102, $00000302, $02000302, $00020302,
        $02020302, $01000100, $03000100, $01020100, $03020100, $01000300, $03000300, $01020300, $03020300,
        $01000102, $03000102, $01020102, $03020102, $01000302, $03000302, $01020302, $03020302, $00010100,
        $02010100, $00030100, $02030100, $00010300, $02010300, $00030300, $02030300, $00010102, $02010102,
        $00030102, $02030102, $00010302, $02010302, $00030302, $02030302, $01010100, $03010100, $01030100,
        $03030100, $01010300, $03010300, $01030300, $03030300, $01010102, $03010102, $01030102, $03030102,
        $01010302, $03010302, $01030302, $03030302, $00000001, $02000001, $00020001, $02020001, $00000201,
        $02000201, $00020201, $02020201, $00000003, $02000003, $00020003, $02020003, $00000203, $02000203,
        $00020203, $02020203, $01000001, $03000001, $01020001, $03020001, $01000201, $03000201, $01020201,
        $03020201, $01000003, $03000003, $01020003, $03020003, $01000203, $03000203, $01020203, $03020203,
        $00010001, $02010001, $00030001, $02030001, $00010201, $02010201, $00030201, $02030201, $00010003,
        $02010003, $00030003, $02030003, $00010203, $02010203, $00030203, $02030203, $01010001, $03010001,
        $01030001, $03030001, $01010201, $03010201, $01030201, $03030201, $01010003, $03010003, $01030003,
        $03030003, $01010203, $03010203, $01030203, $03030203, $00000101, $02000101, $00020101, $02020101,
        $00000301, $02000301, $00020301, $02020301, $00000103, $02000103, $00020103, $02020103, $00000303,
        $02000303, $00020303, $02020303, $01000101, $03000101, $01020101, $03020101, $01000301, $03000301,
        $01020301, $03020301, $01000103, $03000103, $01020103, $03020103, $01000303, $03000303, $01020303,
        $03020303, $00010101, $02010101, $00030101, $02030101, $00010301, $02010301, $00030301, $02030301,
        $00010103, $02010103, $00030103, $02030103, $00010303, $02010303, $00030303, $02030303, $01010101,
        $03010101, $01030101, $03030101, $01010301, $03010301, $01030301, $03030301, $01010103, $03010103,
        $01030103, $03030103, $01010303, $03010303, $01030303, $03030303);

function TDESCipher.EncryptedBlock(const Plaintext: Int64): Int64;
var
    H, L, R, DH, DL: DWORD;
begin
    L := TDoubleDWORD(Plaintext).L;
    H := TDoubleDWORD(Plaintext).R;
    DL := L and $55555555 or H and $55555555 shl 1;
    DH := H and $AAAAAAAA or L and $AAAAAAAA shr 1;
    L := IP[DL shr 24] shr 3 or IP[DL shr 16 and $FF] shr 2 or IP[DL shr 8 and $FF] shr 1 or IP[DL and $FF];
    R := IP[DH shr 24] shr 3 or IP[DH shr 16 and $FF] shr 2 or IP[DH shr 8 and $FF] shr 1 or IP[DH and $FF];

    L := L xor ExpandedSubstitutedAndPermutedDWORD(R, FKeySchedule[15]);
    R := R xor ExpandedSubstitutedAndPermutedDWORD(L, FKeySchedule[14]);
    L := L xor ExpandedSubstitutedAndPermutedDWORD(R, FKeySchedule[13]);
    R := R xor ExpandedSubstitutedAndPermutedDWORD(L, FKeySchedule[12]);
    L := L xor ExpandedSubstitutedAndPermutedDWORD(R, FKeySchedule[11]);
    R := R xor ExpandedSubstitutedAndPermutedDWORD(L, FKeySchedule[10]);
    L := L xor ExpandedSubstitutedAndPermutedDWORD(R, FKeySchedule[9]);
    R := R xor ExpandedSubstitutedAndPermutedDWORD(L, FKeySchedule[8]);
    L := L xor ExpandedSubstitutedAndPermutedDWORD(R, FKeySchedule[7]);
    R := R xor ExpandedSubstitutedAndPermutedDWORD(L, FKeySchedule[6]);
    L := L xor ExpandedSubstitutedAndPermutedDWORD(R, FKeySchedule[5]);
    R := R xor ExpandedSubstitutedAndPermutedDWORD(L, FKeySchedule[4]);
    L := L xor ExpandedSubstitutedAndPermutedDWORD(R, FKeySchedule[3]);
    R := R xor ExpandedSubstitutedAndPermutedDWORD(L, FKeySchedule[2]);
    L := L xor ExpandedSubstitutedAndPermutedDWORD(R, FKeySchedule[1]);
    R := R xor ExpandedSubstitutedAndPermutedDWORD(L, FKeySchedule[0]);

    { Exchange final blocks L16, R16. }
    { Run 'em through the inverse of IP and put 'em back without swapping. }
    DL := L and $0F0F0F0F or R and $0F0F0F0F shl 4;
    DH := R and $F0F0F0F0 or L and $F0F0F0F0 shr 4;
    TDoubleDWORD(Result).R := IPInv[DL shr 24] shl 6 or IPInv[DL shr 16 and $FF] shl 4 or
        IPInv[DL shr 8 and $FF] shl 2 or IPInv[DL and $FF];
    TDoubleDWORD(Result).L := IPInv[DH shr 24] shl 6 or IPInv[DH shr 16 and $FF] shl 4 or
        IPInv[DH shr 8 and $FF] shl 2 or IPInv[DH and $FF];
end; { EncryptedBlock }

procedure TDESCipher.EncryptBlock(var Plaintext);
var
    H, L, R, DH, DL: DWORD;
begin
    L := TDoubleDWORD(Plaintext).L;
    H := TDoubleDWORD(Plaintext).R;
    DL := L and $55555555 or H and $55555555 shl 1;
    DH := H and $AAAAAAAA or L and $AAAAAAAA shr 1;
    L := IP[DL shr 24] shr 3 or IP[DL shr 16 and $FF] shr 2 or IP[DL shr 8 and $FF] shr 1 or IP[DL and $FF];
    R := IP[DH shr 24] shr 3 or IP[DH shr 16 and $FF] shr 2 or IP[DH shr 8 and $FF] shr 1 or IP[DH and $FF];

    L := L xor ExpandedSubstitutedAndPermutedDWORD(R, FKeySchedule[15]);
    R := R xor ExpandedSubstitutedAndPermutedDWORD(L, FKeySchedule[14]);
    L := L xor ExpandedSubstitutedAndPermutedDWORD(R, FKeySchedule[13]);
    R := R xor ExpandedSubstitutedAndPermutedDWORD(L, FKeySchedule[12]);
    L := L xor ExpandedSubstitutedAndPermutedDWORD(R, FKeySchedule[11]);
    R := R xor ExpandedSubstitutedAndPermutedDWORD(L, FKeySchedule[10]);
    L := L xor ExpandedSubstitutedAndPermutedDWORD(R, FKeySchedule[9]);
    R := R xor ExpandedSubstitutedAndPermutedDWORD(L, FKeySchedule[8]);
    L := L xor ExpandedSubstitutedAndPermutedDWORD(R, FKeySchedule[7]);
    R := R xor ExpandedSubstitutedAndPermutedDWORD(L, FKeySchedule[6]);
    L := L xor ExpandedSubstitutedAndPermutedDWORD(R, FKeySchedule[5]);
    R := R xor ExpandedSubstitutedAndPermutedDWORD(L, FKeySchedule[4]);
    L := L xor ExpandedSubstitutedAndPermutedDWORD(R, FKeySchedule[3]);
    R := R xor ExpandedSubstitutedAndPermutedDWORD(L, FKeySchedule[2]);
    L := L xor ExpandedSubstitutedAndPermutedDWORD(R, FKeySchedule[1]);
    R := R xor ExpandedSubstitutedAndPermutedDWORD(L, FKeySchedule[0]);

    { Exchange final blocks L16, R16. }
    { Run 'em through the inverse of IP and put 'em back without swapping. }
    DL := L and $0F0F0F0F or R and $0F0F0F0F shl 4;
    DH := R and $F0F0F0F0 or L and $F0F0F0F0 shr 4;
    TDoubleDWORD(Plaintext).R := IPInv[DL shr 24] shl 6 or IPInv[DL shr 16 and $FF] shl 4 or
        IPInv[DL shr 8 and $FF] shl 2 or IPInv[DL and $FF];
    TDoubleDWORD(Plaintext).L := IPInv[DH shr 24] shl 6 or IPInv[DH shr 16 and $FF] shl 4 or
        IPInv[DH shr 8 and $FF] shl 2 or IPInv[DH and $FF];
end; { EncryptBlock }

function TDESCipher.DecryptedBlock(const Ciphertext: Int64): Int64;
{ See comments for EncryptedBlock. }
var
    H, L, R, DH, DL: DWORD;
begin
    L := TDoubleDWORD(Ciphertext).L;
    H := TDoubleDWORD(Ciphertext).R;
    DL := L and $55555555 or H and $55555555 shl 1;
    DH := H and $AAAAAAAA or L and $AAAAAAAA shr 1;
    L := IP[DL shr 24] shr 3 or IP[DL shr 16 and $FF] shr 2 or IP[DL shr 8 and $FF] shr 1 or IP[DL and $FF];
    R := IP[DH shr 24] shr 3 or IP[DH shr 16 and $FF] shr 2 or IP[DH shr 8 and $FF] shr 1 or IP[DH and $FF];

    L := L xor ExpandedSubstitutedAndPermutedDWORD(R, FKeySchedule[0]);
    R := R xor ExpandedSubstitutedAndPermutedDWORD(L, FKeySchedule[1]);
    L := L xor ExpandedSubstitutedAndPermutedDWORD(R, FKeySchedule[2]);
    R := R xor ExpandedSubstitutedAndPermutedDWORD(L, FKeySchedule[3]);
    L := L xor ExpandedSubstitutedAndPermutedDWORD(R, FKeySchedule[4]);
    R := R xor ExpandedSubstitutedAndPermutedDWORD(L, FKeySchedule[5]);
    L := L xor ExpandedSubstitutedAndPermutedDWORD(R, FKeySchedule[6]);
    R := R xor ExpandedSubstitutedAndPermutedDWORD(L, FKeySchedule[7]);
    L := L xor ExpandedSubstitutedAndPermutedDWORD(R, FKeySchedule[8]);
    R := R xor ExpandedSubstitutedAndPermutedDWORD(L, FKeySchedule[9]);
    L := L xor ExpandedSubstitutedAndPermutedDWORD(R, FKeySchedule[10]);
    R := R xor ExpandedSubstitutedAndPermutedDWORD(L, FKeySchedule[11]);
    L := L xor ExpandedSubstitutedAndPermutedDWORD(R, FKeySchedule[12]);
    R := R xor ExpandedSubstitutedAndPermutedDWORD(L, FKeySchedule[13]);
    L := L xor ExpandedSubstitutedAndPermutedDWORD(R, FKeySchedule[14]);
    R := R xor ExpandedSubstitutedAndPermutedDWORD(L, FKeySchedule[15]);

    DL := L and $0F0F0F0F or R and $0F0F0F0F shl 4;
    DH := R and $F0F0F0F0 or L and $F0F0F0F0 shr 4;
    TDoubleDWORD(Result).R := IPInv[DL shr 24] shl 6 or IPInv[DL shr 16 and $FF] shl 4 or
        IPInv[DL shr 8 and $FF] shl 2 or IPInv[DL and $FF];
    TDoubleDWORD(Result).L := IPInv[DH shr 24] shl 6 or IPInv[DH shr 16 and $FF] shl 4 or
        IPInv[DH shr 8 and $FF] shl 2 or IPInv[DH and $FF];
end; { DecryptedBlock }

procedure TDESCipher.DecryptBlock(var Ciphertext);
var
    H, L, R, DH, DL: DWORD;
begin
    L := TDoubleDWORD(Ciphertext).L;
    H := TDoubleDWORD(Ciphertext).R;
    DL := L and $55555555 or H and $55555555 shl 1;
    DH := H and $AAAAAAAA or L and $AAAAAAAA shr 1;
    L := IP[DL shr 24] shr 3 or IP[DL shr 16 and $FF] shr 2 or IP[DL shr 8 and $FF] shr 1 or IP[DL and $FF];
    R := IP[DH shr 24] shr 3 or IP[DH shr 16 and $FF] shr 2 or IP[DH shr 8 and $FF] shr 1 or IP[DH and $FF];

    L := L xor ExpandedSubstitutedAndPermutedDWORD(R, FKeySchedule[0]);
    R := R xor ExpandedSubstitutedAndPermutedDWORD(L, FKeySchedule[1]);
    L := L xor ExpandedSubstitutedAndPermutedDWORD(R, FKeySchedule[2]);
    R := R xor ExpandedSubstitutedAndPermutedDWORD(L, FKeySchedule[3]);
    L := L xor ExpandedSubstitutedAndPermutedDWORD(R, FKeySchedule[4]);
    R := R xor ExpandedSubstitutedAndPermutedDWORD(L, FKeySchedule[5]);
    L := L xor ExpandedSubstitutedAndPermutedDWORD(R, FKeySchedule[6]);
    R := R xor ExpandedSubstitutedAndPermutedDWORD(L, FKeySchedule[7]);
    L := L xor ExpandedSubstitutedAndPermutedDWORD(R, FKeySchedule[8]);
    R := R xor ExpandedSubstitutedAndPermutedDWORD(L, FKeySchedule[9]);
    L := L xor ExpandedSubstitutedAndPermutedDWORD(R, FKeySchedule[10]);
    R := R xor ExpandedSubstitutedAndPermutedDWORD(L, FKeySchedule[11]);
    L := L xor ExpandedSubstitutedAndPermutedDWORD(R, FKeySchedule[12]);
    R := R xor ExpandedSubstitutedAndPermutedDWORD(L, FKeySchedule[13]);
    L := L xor ExpandedSubstitutedAndPermutedDWORD(R, FKeySchedule[14]);
    R := R xor ExpandedSubstitutedAndPermutedDWORD(L, FKeySchedule[15]);

    DL := L and $0F0F0F0F or R and $0F0F0F0F shl 4;
    DH := R and $F0F0F0F0 or L and $F0F0F0F0 shr 4;
    TDoubleDWORD(Ciphertext).R := IPInv[DL shr 24] shl 6 or IPInv[DL shr 16 and $FF] shl 4 or
        IPInv[DL shr 8 and $FF] shl 2 or IPInv[DL and $FF];
    TDoubleDWORD(Ciphertext).L := IPInv[DH shr 24] shl 6 or IPInv[DH shr 16 and $FF] shl 4 or
        IPInv[DH shr 8 and $FF] shl 2 or IPInv[DH and $FF];
end; { DecryptBlock }

class function TDESCipher.MinKeyLength: Integer;
begin
    Result := 8;
end;


// ----------------------------- TBlowfishCipher --------------------------------

const
    InitialSBox1: TBlowfishSBox = ($D1310BA6, $98DFB5AC, $2FFD72DB, $D01ADFB7, $B8E1AFED, $6A267E96,
        $BA7C9045, $F12C7F99, $24A19947, $B3916CF7, $0801F2E2, $858EFC16, $636920D8, $71574E69, $A458FEA3,
        $F4933D7E, $0D95748F, $728EB658, $718BCD58, $82154AEE, $7B54A41D, $C25A59B5, $9C30D539, $2AF26013,
        $C5D1B023, $286085F0, $CA417918, $B8DB38EF, $8E79DCB0, $603A180E, $6C9E0E8B, $B01E8A3E, $D71577C1,
        $BD314B27, $78AF2FDA, $55605C60, $E65525F3, $AA55AB94, $57489862, $63E81440, $55CA396A, $2AAB10B6,
        $B4CC5C34, $1141E8CE, $A15486AF, $7C72E993, $B3EE1411, $636FBC2A, $2BA9C55D, $741831F6, $CE5C3E16,
        $9B87931E, $AFD6BA33, $6C24CF5C, $7A325381, $28958677, $3B8F4898, $6B4BB9AF, $C4BFE81B, $66282193,
        $61D809CC, $FB21A991, $487CAC60, $5DEC8032, $EF845D5D, $E98575B1, $DC262302, $EB651B88, $23893E81,
        $D396ACC5, $0F6D6FF3, $83F44239, $2E0B4482, $A4842004, $69C8F04A, $9E1F9B5E, $21C66842, $F6E96C9A,
        $670C9C61, $ABD388F0, $6A51A0D2, $D8542F68, $960FA728, $AB5133A3, $6EEF0B6C, $137A3BE4, $BA3BF050,
        $7EFB2A98, $A1F1651D, $39AF0176, $66CA593E, $82430E88, $8CEE8619, $456F9FB4, $7D84A5C3, $3B8B5EBE,
        $E06F75D8, $85C12073, $401A449F, $56C16AA6, $4ED3AA62, $363F7706, $1BFEDF72, $429B023D, $37D0D724,
        $D00A1248, $DB0FEAD3, $49F1C09B, $075372C9, $80991B7B, $25D479D8, $F6E8DEF7, $E3FE501A, $B6794C3B,
        $976CE0BD, $04C006BA, $C1A94FB6, $409F60C4, $5E5C9EC2, $196A2463, $68FB6FAF, $3E6C53B5, $1339B2EB,
        $3B52EC6F, $6DFC511F, $9B30952C, $CC814544, $AF5EBD09, $BEE3D004, $DE334AFD, $660F2807, $192E4BB3,
        $C0CBA857, $45C8740F, $D20B5F39, $B9D3FBDB, $5579C0BD, $1A60320A, $D6A100C6, $402C7279, $679F25FE,
        $FB1FA3CC, $8EA5E9F8, $DB3222F8, $3C7516DF, $FD616B15, $2F501EC8, $AD0552AB, $323DB5FA, $FD238760,
        $53317B48, $3E00DF82, $9E5C57BB, $CA6F8CA0, $1A87562E, $DF1769DB, $D542A8F6, $287EFFC3, $AC6732C6,
        $8C4F5573, $695B27B0, $BBCA58C8, $E1FFA35D, $B8F011A0, $10FA3D98, $FD2183B8, $4AFCB56C, $2DD1D35B,
        $9A53E479, $B6F84565, $D28E49BC, $4BFB9790, $E1DDF2DA, $A4CB7E33, $62FB1341, $CEE4C6E8, $EF20CADA,
        $36774C01, $D07E9EFE, $2BF11FB4, $95DBDA4D, $AE909198, $EAAD8E71, $6B93D5A0, $D08ED1D0, $AFC725E0,
        $8E3C5B2F, $8E7594B7, $8FF6E2FB, $F2122B64, $8888B812, $900DF01C, $4FAD5EA0, $688FC31C, $D1CFF191,
        $B3A8C1AD, $2F2F2218, $BE0E1777, $EA752DFE, $8B021FA1, $E5A0CC0F, $B56F74E8, $18ACF3D6, $CE89E299,
        $B4A84FE0, $FD13E0B7, $7CC43B81, $D2ADA8D9, $165FA266, $80957705, $93CC7314, $211A1477, $E6AD2065,
        $77B5FA86, $C75442F5, $FB9D35CF, $EBCDAF0C, $7B3E89A0, $D6411BD3, $AE1E7E49, $00250E2D, $2071B35E,
        $226800BB, $57B8E0AF, $2464369B, $F009B91E, $5563911D, $59DFA6AA, $78C14389, $D95A537F, $207D5BA2,
        $02E5B9C5, $83260376, $6295CFA9, $11C81968, $4E734A41, $B3472DCA, $7B14A94A, $1B510052, $9A532915,
        $D60F573F, $BC9BC6E4, $2B60A476, $81E67400, $08BA6FB5, $571BE91F, $F296EC6B, $2A0DD915, $B6636521,
        $E7B9F9B6, $FF34052E, $C5855664, $53B02D5D, $A99F8FA1, $08BA4799, $6E85076A);

    InitialSBox2: TBlowfishSBox = ($4B7A70E9, $B5B32944, $DB75092E, $C4192623, $AD6EA6B0, $49A7DF7D,
        $9CEE60B8, $8FEDB266, $ECAA8C71, $699A17FF, $5664526C, $C2B19EE1, $193602A5, $75094C29, $A0591340,
        $E4183A3E, $3F54989A, $5B429D65, $6B8FE4D6, $99F73FD6, $A1D29C07, $EFE830F5, $4D2D38E6, $F0255DC1,
        $4CDD2086, $8470EB26, $6382E9C6, $021ECC5E, $09686B3F, $3EBAEFC9, $3C971814, $6B6A70A1, $687F3584,
        $52A0E286, $B79C5305, $AA500737, $3E07841C, $7FDEAE5C, $8E7D44EC, $5716F2B8, $B03ADA37, $F0500C0D,
        $F01C1F04, $0200B3FF, $AE0CF51A, $3CB574B2, $25837A58, $DC0921BD, $D19113F9, $7CA92FF6, $94324773,
        $22F54701, $3AE5E581, $37C2DADC, $C8B57634, $9AF3DDA7, $A9446146, $0FD0030E, $ECC8C73E, $A4751E41,
        $E238CD99, $3BEA0E2F, $3280BBA1, $183EB331, $4E548B38, $4F6DB908, $6F420D03, $F60A04BF, $2CB81290,
        $24977C79, $5679B072, $BCAF89AF, $DE9A771F, $D9930810, $B38BAE12, $DCCF3F2E, $5512721F, $2E6B7124,
        $501ADDE6, $9F84CD87, $7A584718, $7408DA17, $BC9F9ABC, $E94B7D8C, $EC7AEC3A, $DB851DFA, $63094366,
        $C464C3D2, $EF1C1847, $3215D908, $DD433B37, $24C2BA16, $12A14D43, $2A65C451, $50940002, $133AE4DD,
        $71DFF89E, $10314E55, $81AC77D6, $5F11199B, $043556F1, $D7A3C76B, $3C11183B, $5924A509, $F28FE6ED,
        $97F1FBFA, $9EBABF2C, $1E153C6E, $86E34570, $EAE96FB1, $860E5E0A, $5A3E2AB3, $771FE71C, $4E3D06FA,
        $2965DCB9, $99E71D0F, $803E89D6, $5266C825, $2E4CC978, $9C10B36A, $C6150EBA, $94E2EA78, $A5FC3C53,
        $1E0A2DF4, $F2F74EA7, $361D2B3D, $1939260F, $19C27960, $5223A708, $F71312B6, $EBADFE6E, $EAC31F66,
        $E3BC4595, $A67BC883, $B17F37D1, $018CFF28, $C332DDEF, $BE6C5AA5, $65582185, $68AB9802, $EECEA50F,
        $DB2F953B, $2AEF7DAD, $5B6E2F84, $1521B628, $29076170, $ECDD4775, $619F1510, $13CCA830, $EB61BD96,
        $0334FE1E, $AA0363CF, $B5735C90, $4C70A239, $D59E9E0B, $CBAADE14, $EECC86BC, $60622CA7, $9CAB5CAB,
        $B2F3846E, $648B1EAF, $19BDF0CA, $A02369B9, $655ABB50, $40685A32, $3C2AB4B3, $319EE9D5, $C021B8F7,
        $9B540B19, $875FA099, $95F7997E, $623D7DA8, $F837889A, $97E32D77, $11ED935F, $16681281, $0E358829,
        $C7E61FD6, $96DEDFA1, $7858BA99, $57F584A5, $1B227263, $9B83C3FF, $1AC24696, $CDB30AEB, $532E3054,
        $8FD948E4, $6DBC3128, $58EBF2EF, $34C6FFEA, $FE28ED61, $EE7C3C73, $5D4A14D9, $E864B7E3, $42105D14,
        $203E13E0, $45EEE2B6, $A3AAABEA, $DB6C4F15, $FACB4FD0, $C742F442, $EF6ABBB5, $654F3B1D, $41CD2105,
        $D81E799E, $86854DC7, $E44B476A, $3D816250, $CF62A1F2, $5B8D2646, $FC8883A0, $C1C7B6A3, $7F1524C3,
        $69CB7492, $47848A0B, $5692B285, $095BBF00, $AD19489D, $1462B174, $23820E00, $58428D2A, $0C55F5EA,
        $1DADF43E, $233F7061, $3372F092, $8D937E41, $D65FECF1, $6C223BDB, $7CDE3759, $CBEE7460, $4085F2A7,
        $CE77326E, $A6078084, $19F8509E, $E8EFD855, $61D99735, $A969A7AA, $C50C06C2, $5A04ABFC, $800BCADC,
        $9E447A2E, $C3453484, $FDD56705, $0E1E9EC9, $DB73DBD3, $105588CD, $675FDA79, $E3674340, $C5C43465,
        $713E38D8, $3D28F89E, $F16DFF20, $153E21E7, $8FB03D4A, $E6E39F2B, $DB83ADF7);

    InitialSBox3: TBlowfishSBox = ($E93D5A68, $948140F7, $F64C261C, $94692934, $411520F7, $7602D4F7,
        $BCF46B2E, $D4A20068, $D4082471, $3320F46A, $43B7D4B7, $500061AF, $1E39F62E, $97244546, $14214F74,
        $BF8B8840, $4D95FC1D, $96B591AF, $70F4DDD3, $66A02F45, $BFBC09EC, $03BD9785, $7FAC6DD0, $31CB8504,
        $96EB27B3, $55FD3941, $DA2547E6, $ABCA0A9A, $28507825, $530429F4, $0A2C86DA, $E9B66DFB, $68DC1462,
        $D7486900, $680EC0A4, $27A18DEE, $4F3FFEA2, $E887AD8C, $B58CE006, $7AF4D6B6, $AACE1E7C, $D3375FEC,
        $CE78A399, $406B2A42, $20FE9E35, $D9F385B9, $EE39D7AB, $3B124E8B, $1DC9FAF7, $4B6D1856, $26A36631,
        $EAE397B2, $3A6EFA74, $DD5B4332, $6841E7F7, $CA7820FB, $FB0AF54E, $D8FEB397, $454056AC, $BA489527,
        $55533A3A, $20838D87, $FE6BA9B7, $D096954B, $55A867BC, $A1159A58, $CCA92963, $99E1DB33, $A62A4A56,
        $3F3125F9, $5EF47E1C, $9029317C, $FDF8E802, $04272F70, $80BB155C, $05282CE3, $95C11548, $E4C66D22,
        $48C1133F, $C70F86DC, $07F9C9EE, $41041F0F, $404779A4, $5D886E17, $325F51EB, $D59BC0D1, $F2BCC18F,
        $41113564, $257B7834, $602A9C60, $DFF8E8A3, $1F636C1B, $0E12B4C2, $02E1329E, $AF664FD1, $CAD18115,
        $6B2395E0, $333E92E1, $3B240B62, $EEBEB922, $85B2A20E, $E6BA0D99, $DE720C8C, $2DA2F728, $D0127845,
        $95B794FD, $647D0862, $E7CCF5F0, $5449A36F, $877D48FA, $C39DFD27, $F33E8D1E, $0A476341, $992EFF74,
        $3A6F6EAB, $F4F8FD37, $A812DC60, $A1EBDDF8, $991BE14C, $DB6E6B0D, $C67B5510, $6D672C37, $2765D43B,
        $DCD0E804, $F1290DC7, $CC00FFA3, $B5390F92, $690FED0B, $667B9FFB, $CEDB7D9C, $A091CF0B, $D9155EA3,
        $BB132F88, $515BAD24, $7B9479BF, $763BD6EB, $37392EB3, $CC115979, $8026E297, $F42E312D, $6842ADA7,
        $C66A2B3B, $12754CCC, $782EF11C, $6A124237, $B79251E7, $06A1BBE6, $4BFB6350, $1A6B1018, $11CAEDFA,
        $3D25BDD8, $E2E1C3C9, $44421659, $0A121386, $D90CEC6E, $D5ABEA2A, $64AF674E, $DA86A85F, $BEBFE988,
        $64E4C3FE, $9DBC8057, $F0F7C086, $60787BF8, $6003604D, $D1FD8346, $F6381FB0, $7745AE04, $D736FCCC,
        $83426B33, $F01EAB71, $B0804187, $3C005E5F, $77A057BE, $BDE8AE24, $55464299, $BF582E61, $4E58F48F,
        $F2DDFDA2, $F474EF38, $8789BDC2, $5366F9C3, $C8B38E74, $B475F255, $46FCD9B9, $7AEB2661, $8B1DDF84,
        $846A0E79, $915F95E2, $466E598E, $20B45770, $8CD55591, $C902DE4C, $B90BACE1, $BB8205D0, $11A86248,
        $7574A99E, $B77F19B6, $E0A9DC09, $662D09A1, $C4324633, $E85A1F02, $09F0BE8C, $4A99A025, $1D6EFE10,
        $1AB93D1D, $0BA5A4DF, $A186F20F, $2868F169, $DCB7DA83, $573906FE, $A1E2CE9B, $4FCD7F52, $50115E01,
        $A70683FA, $A002B5C4, $0DE6D027, $9AF88C27, $773F8641, $C3604C06, $61A806B5, $F0177A28, $C0F586E0,
        $006058AA, $30DC7D62, $11E69ED7, $2338EA63, $53C2DD94, $C2C21634, $BBCBEE56, $90BCB6DE, $EBFC7DA1,
        $CE591D76, $6F05E409, $4B7C0188, $39720A3D, $7C927C24, $86E3725F, $724D9DB9, $1AC15BB4, $D39EB8FC,
        $ED545578, $08FCA5B5, $D83D7CD3, $4DAD0FC4, $1E50EF5E, $B161E6F8, $A28514D9, $6C51133C, $6FD5C7E7,
        $56E14EC4, $362ABFCE, $DDC6C837, $D79A3234, $92638212, $670EFA8E, $406000E0);

    InitialSBox4: TBlowfishSBox = ($3A39CE37, $D3FAF5CF, $ABC27737, $5AC52D1B, $5CB0679E, $4FA33742,
        $D3822740, $99BC9BBE, $D5118E9D, $BF0F7315, $D62D1C7E, $C700C47B, $B78C1B6B, $21A19045, $B26EB1BE,
        $6A366EB4, $5748AB2F, $BC946E79, $C6A376D2, $6549C2C8, $530FF8EE, $468DDE7D, $D5730A1D, $4CD04DC6,
        $2939BBDB, $A9BA4650, $AC9526E8, $BE5EE304, $A1FAD5F0, $6A2D519A, $63EF8CE2, $9A86EE22, $C089C2B8,
        $43242EF6, $A51E03AA, $9CF2D0A4, $83C061BA, $9BE96A4D, $8FE51550, $BA645BD6, $2826A2F9, $A73A3AE1,
        $4BA99586, $EF5562E9, $C72FEFD3, $F752F7DA, $3F046F69, $77FA0A59, $80E4A915, $87B08601, $9B09E6AD,
        $3B3EE593, $E990FD5A, $9E34D797, $2CF0B7D9, $022B8B51, $96D5AC3A, $017DA67D, $D1CF3ED6, $7C7D2D28,
        $1F9F25CF, $ADF2B89B, $5AD6B472, $5A88F54C, $E029AC71, $E019A5E6, $47B0ACFD, $ED93FA9B, $E8D3C48D,
        $283B57CC, $F8D56629, $79132E28, $785F0191, $ED756055, $F7960E44, $E3D35E8C, $15056DD4, $88F46DBA,
        $03A16125, $0564F0BD, $C3EB9E15, $3C9057A2, $97271AEC, $A93A072A, $1B3F6D9B, $1E6321F5, $F59C66FB,
        $26DCF319, $7533D928, $B155FDF5, $03563482, $8ABA3CBB, $28517711, $C20AD9F8, $ABCC5167, $CCAD925F,
        $4DE81751, $3830DC8E, $379D5862, $9320F991, $EA7A90C2, $FB3E7BCE, $5121CE64, $774FBE32, $A8B6E37E,
        $C3293D46, $48DE5369, $6413E680, $A2AE0810, $DD6DB224, $69852DFD, $09072166, $B39A460A, $6445C0DD,
        $586CDECF, $1C20C8AE, $5BBEF7DD, $1B588D40, $CCD2017F, $6BB4E3BB, $DDA26A7E, $3A59FF45, $3E350A44,
        $BCB4CDD5, $72EACEA8, $FA6484BB, $8D6612AE, $BF3C6F47, $D29BE463, $542F5D9E, $AEC2771B, $F64E6370,
        $740E0D8D, $E75B1357, $F8721671, $AF537D5D, $4040CB08, $4EB4E2CC, $34D2466A, $0115AF84, $E1B00428,
        $95983A1D, $06B89FB4, $CE6EA048, $6F3F3B82, $3520AB82, $011A1D4B, $277227F8, $611560B1, $E7933FDC,
        $BB3A792B, $344525BD, $A08839E1, $51CE794B, $2F32C9B7, $A01FBAC9, $E01CC87E, $BCC7D1F6, $CF0111C3,
        $A1E8AAC7, $1A908749, $D44FBD9A, $D0DADECB, $D50ADA38, $0339C32A, $C6913667, $8DF9317C, $E0B12B4F,
        $F79E59B7, $43F5BB3A, $F2D519FF, $27D9459C, $BF97222C, $15E6FC2A, $0F91FC71, $9B941525, $FAE59361,
        $CEB69CEB, $C2A86459, $12BAA8D1, $B6C1075E, $E3056A0C, $10D25065, $CB03A442, $E0EC6E0E, $1698DB3B,
        $4C98A0BE, $3278E964, $9F1F9532, $E0D392DF, $D3A0342B, $8971F21E, $1B0A7441, $4BA3348C, $C5BE7120,
        $C37632D8, $DF359F8D, $9B992F2E, $E60B6F47, $0FE3F11D, $E54CDA54, $1EDAD891, $CE6279CF, $CD3E7E6F,
        $1618B166, $FD2C1D05, $848FD2C5, $F6FB2299, $F523F357, $A6327623, $93A83531, $56CCCD02, $ACF08162,
        $5A75EBB5, $6E163697, $88D273CC, $DE966292, $81B949D0, $4C50901B, $71C65614, $E6C6C7BD, $327A140A,
        $45E1D006, $C3F27B9A, $C9AA53FD, $62A80F00, $BB25BFE2, $35BDD2F6, $71126905, $B2040222, $B6CBCF7C,
        $CD769C2B, $53113EC0, $1640E3D3, $38ABBD60, $2547ADF0, $BA38209C, $F746CE76, $77AFA1C5, $20756060,
        $85CBFE4E, $8AE88DD8, $7AAAF9B0, $4CF9AA7E, $1948C25C, $02FB8A8C, $01C36AE4, $D6EBE1F9, $90D4F869,
        $A65CDEA0, $3F09252D, $C208E69F, $B74E6132, $CE77E25B, $578FDFE3, $3AC372E6);

    InitialPArray: TBlowfishPArray = ($243F6A88, $85A308D3, $13198A2E, $03707344, $A4093822, $299F31D0,
        $082EFA98, $EC4E6C89, $452821E6, $38D01377, $BE5466CF, $34E90C6C, $C0AC29B7, $C97C50DD, $3F84D5B5,
        $B5470917, $9216D5D9, $8979FB1B);

constructor TBlowfishCipher.Create(const Key; const Length: Integer);
begin
    inherited;
    GenerateSubkeys(Key, Length);
end;

destructor TBlowfishCipher.Destroy;
begin
    inherited;
end;

procedure TBlowfishCipher.GenerateSubkeys(const Key; const Length: Integer);
var
    PKey: ^Byte;
    I, J, K: Integer;
    P: Int64;
    Data: DWORD;
begin
    FSBox1 := InitialSBox1;
    FSBox2 := InitialSBox2;
    FSBox3 := InitialSBox3;
    FSBox4 := InitialSBox4;
    J := 0;
    PKey := Addr(Key);
    for I := low(FPArray) to high(FPArray) do
    begin
        Data := 0;
        for K := 1 to 4 do
        begin
            Data := Data shl 8 or PKey^;
            J := Succ(J) mod Length;
            if J = 0 then
                PKey := Addr(Key)
            else
                Inc(PKey);
        end;
        FPArray[I] := InitialPArray[I] xor Data;
    end;
    P := 0;
    I := low(FPArray);
    while I <= high(FPArray) do
    begin
        P := EncryptedBlock(P);
        FPArray[I] := TDoubleDWORD(P).L;
        Inc(I);
        FPArray[I] := TDoubleDWORD(P).R;
        Inc(I);
    end;
    J := low(FSBox1);
    while J <= high(FSBox1) do
    begin
        P := EncryptedBlock(P);
        FSBox1[J] := TDoubleDWORD(P).L;
        Inc(J);
        FSBox1[J] := TDoubleDWORD(P).R;
        Inc(J);
    end;
    J := low(FSBox2);
    while J <= high(FSBox2) do
    begin
        P := EncryptedBlock(P);
        FSBox2[J] := TDoubleDWORD(P).L;
        Inc(J);
        FSBox2[J] := TDoubleDWORD(P).R;
        Inc(J);
    end;
    J := low(FSBox3);
    while J <= high(FSBox3) do
    begin
        P := EncryptedBlock(P);
        FSBox3[J] := TDoubleDWORD(P).L;
        Inc(J);
        FSBox3[J] := TDoubleDWORD(P).R;
        Inc(J);
    end;
    J := low(FSBox4);
    while J <= high(FSBox4) do
    begin
        P := EncryptedBlock(P);
        FSBox4[J] := TDoubleDWORD(P).L;
        Inc(J);
        FSBox4[J] := TDoubleDWORD(P).R;
        Inc(J);
    end;
end;

function TBlowfishCipher.EncryptedBlock(const Plaintext: Int64): Int64;
var
    L, R: DWORD;
begin
    L := TDoubleDWORD(Plaintext).L;
    R := TDoubleDWORD(Plaintext).R;
    L := L xor FPArray[0];
    with TFourByte(L) do
    begin
        R := R xor (FSBox1[B4] + FSBox2[B3] xor FSBox3[B2] + FSBox4[B1]);
    end;
    R := R xor FPArray[1];
    with TFourByte(R) do
    begin
        L := L xor (FSBox1[B4] + FSBox2[B3] xor FSBox3[B2] + FSBox4[B1]);
    end;
    L := L xor FPArray[2];
    with TFourByte(L) do
    begin
        R := R xor (FSBox1[B4] + FSBox2[B3] xor FSBox3[B2] + FSBox4[B1]);
    end;
    R := R xor FPArray[3];
    with TFourByte(R) do
    begin
        L := L xor (FSBox1[B4] + FSBox2[B3] xor FSBox3[B2] + FSBox4[B1]);
    end;
    L := L xor FPArray[4];
    with TFourByte(L) do
    begin
        R := R xor (FSBox1[B4] + FSBox2[B3] xor FSBox3[B2] + FSBox4[B1]);
    end;
    R := R xor FPArray[5];
    with TFourByte(R) do
    begin
        L := L xor (FSBox1[B4] + FSBox2[B3] xor FSBox3[B2] + FSBox4[B1]);
    end;
    L := L xor FPArray[6];
    with TFourByte(L) do
    begin
        R := R xor (FSBox1[B4] + FSBox2[B3] xor FSBox3[B2] + FSBox4[B1]);
    end;
    R := R xor FPArray[7];
    with TFourByte(R) do
    begin
        L := L xor (FSBox1[B4] + FSBox2[B3] xor FSBox3[B2] + FSBox4[B1]);
    end;
    L := L xor FPArray[8];
    with TFourByte(L) do
    begin
        R := R xor (FSBox1[B4] + FSBox2[B3] xor FSBox3[B2] + FSBox4[B1]);
    end;
    R := R xor FPArray[9];
    with TFourByte(R) do
    begin
        L := L xor (FSBox1[B4] + FSBox2[B3] xor FSBox3[B2] + FSBox4[B1]);
    end;
    L := L xor FPArray[10];
    with TFourByte(L) do
    begin
        R := R xor (FSBox1[B4] + FSBox2[B3] xor FSBox3[B2] + FSBox4[B1]);
    end;
    R := R xor FPArray[11];
    with TFourByte(R) do
    begin
        L := L xor (FSBox1[B4] + FSBox2[B3] xor FSBox3[B2] + FSBox4[B1]);
    end;
    L := L xor FPArray[12];
    with TFourByte(L) do
    begin
        R := R xor (FSBox1[B4] + FSBox2[B3] xor FSBox3[B2] + FSBox4[B1]);
    end;
    R := R xor FPArray[13];
    with TFourByte(R) do
    begin
        L := L xor (FSBox1[B4] + FSBox2[B3] xor FSBox3[B2] + FSBox4[B1]);
    end;
    L := L xor FPArray[14];
    with TFourByte(L) do
    begin
        R := R xor (FSBox1[B4] + FSBox2[B3] xor FSBox3[B2] + FSBox4[B1]);
    end;
    R := R xor FPArray[15];
    with TFourByte(R) do
    begin
        L := L xor (FSBox1[B4] + FSBox2[B3] xor FSBox3[B2] + FSBox4[B1]);
    end;
    TDoubleDWORD(Result).L := R xor FPArray[17];
    TDoubleDWORD(Result).R := L xor FPArray[16];
end;

function TBlowfishCipher.DecryptedBlock(const Ciphertext: Int64): Int64;
var
    L, R: DWORD;
begin
    L := TDoubleDWORD(Ciphertext).L;
    R := TDoubleDWORD(Ciphertext).R;
    L := L xor FPArray[17];
    with TFourByte(L) do
    begin
        R := R xor (FSBox1[B4] + FSBox2[B3] xor FSBox3[B2] + FSBox4[B1]);
    end;
    R := R xor FPArray[16];
    with TFourByte(R) do
    begin
        L := L xor (FSBox1[B4] + FSBox2[B3] xor FSBox3[B2] + FSBox4[B1]);
    end;
    L := L xor FPArray[15];
    with TFourByte(L) do
    begin
        R := R xor (FSBox1[B4] + FSBox2[B3] xor FSBox3[B2] + FSBox4[B1]);
    end;
    R := R xor FPArray[14];
    with TFourByte(R) do
    begin
        L := L xor (FSBox1[B4] + FSBox2[B3] xor FSBox3[B2] + FSBox4[B1]);
    end;
    L := L xor FPArray[13];
    with TFourByte(L) do
    begin
        R := R xor (FSBox1[B4] + FSBox2[B3] xor FSBox3[B2] + FSBox4[B1]);
    end;
    R := R xor FPArray[12];
    with TFourByte(R) do
    begin
        L := L xor (FSBox1[B4] + FSBox2[B3] xor FSBox3[B2] + FSBox4[B1]);
    end;
    L := L xor FPArray[11];
    with TFourByte(L) do
    begin
        R := R xor (FSBox1[B4] + FSBox2[B3] xor FSBox3[B2] + FSBox4[B1]);
    end;
    R := R xor FPArray[10];
    with TFourByte(R) do
    begin
        L := L xor (FSBox1[B4] + FSBox2[B3] xor FSBox3[B2] + FSBox4[B1]);
    end;
    L := L xor FPArray[9];
    with TFourByte(L) do
    begin
        R := R xor (FSBox1[B4] + FSBox2[B3] xor FSBox3[B2] + FSBox4[B1]);
    end;
    R := R xor FPArray[8];
    with TFourByte(R) do
    begin
        L := L xor (FSBox1[B4] + FSBox2[B3] xor FSBox3[B2] + FSBox4[B1]);
    end;
    L := L xor FPArray[7];
    with TFourByte(L) do
    begin
        R := R xor (FSBox1[B4] + FSBox2[B3] xor FSBox3[B2] + FSBox4[B1]);
    end;
    R := R xor FPArray[6];
    with TFourByte(R) do
    begin
        L := L xor (FSBox1[B4] + FSBox2[B3] xor FSBox3[B2] + FSBox4[B1]);
    end;
    L := L xor FPArray[5];
    with TFourByte(L) do
    begin
        R := R xor (FSBox1[B4] + FSBox2[B3] xor FSBox3[B2] + FSBox4[B1]);
    end;
    R := R xor FPArray[4];
    with TFourByte(R) do
    begin
        L := L xor (FSBox1[B4] + FSBox2[B3] xor FSBox3[B2] + FSBox4[B1]);
    end;
    L := L xor FPArray[3];
    with TFourByte(L) do
    begin
        R := R xor (FSBox1[B4] + FSBox2[B3] xor FSBox3[B2] + FSBox4[B1]);
    end;
    R := R xor FPArray[2];
    with TFourByte(R) do
    begin
        L := L xor (FSBox1[B4] + FSBox2[B3] xor FSBox3[B2] + FSBox4[B1]);
    end;
    TDoubleDWORD(Result).L := R xor FPArray[0];
    TDoubleDWORD(Result).R := L xor FPArray[1];
end;

procedure TBlowfishCipher.EncryptBlock(var Plaintext);
var
    L, R: DWORD;
begin
    L := TDoubleDWORD(Plaintext).L;
    R := TDoubleDWORD(Plaintext).R;
    L := L xor FPArray[0];
    with TFourByte(L) do
    begin
        R := R xor (FSBox1[B4] + FSBox2[B3] xor FSBox3[B2] + FSBox4[B1]);
    end;
    R := R xor FPArray[1];
    with TFourByte(R) do
    begin
        L := L xor (FSBox1[B4] + FSBox2[B3] xor FSBox3[B2] + FSBox4[B1]);
    end;
    L := L xor FPArray[2];
    with TFourByte(L) do
    begin
        R := R xor (FSBox1[B4] + FSBox2[B3] xor FSBox3[B2] + FSBox4[B1]);
    end;
    R := R xor FPArray[3];
    with TFourByte(R) do
    begin
        L := L xor (FSBox1[B4] + FSBox2[B3] xor FSBox3[B2] + FSBox4[B1]);
    end;
    L := L xor FPArray[4];
    with TFourByte(L) do
    begin
        R := R xor (FSBox1[B4] + FSBox2[B3] xor FSBox3[B2] + FSBox4[B1]);
    end;
    R := R xor FPArray[5];
    with TFourByte(R) do
    begin
        L := L xor (FSBox1[B4] + FSBox2[B3] xor FSBox3[B2] + FSBox4[B1]);
    end;
    L := L xor FPArray[6];
    with TFourByte(L) do
    begin
        R := R xor (FSBox1[B4] + FSBox2[B3] xor FSBox3[B2] + FSBox4[B1]);
    end;
    R := R xor FPArray[7];
    with TFourByte(R) do
    begin
        L := L xor (FSBox1[B4] + FSBox2[B3] xor FSBox3[B2] + FSBox4[B1]);
    end;
    L := L xor FPArray[8];
    with TFourByte(L) do
    begin
        R := R xor (FSBox1[B4] + FSBox2[B3] xor FSBox3[B2] + FSBox4[B1]);
    end;
    R := R xor FPArray[9];
    with TFourByte(R) do
    begin
        L := L xor (FSBox1[B4] + FSBox2[B3] xor FSBox3[B2] + FSBox4[B1]);
    end;
    L := L xor FPArray[10];
    with TFourByte(L) do
    begin
        R := R xor (FSBox1[B4] + FSBox2[B3] xor FSBox3[B2] + FSBox4[B1]);
    end;
    R := R xor FPArray[11];
    with TFourByte(R) do
    begin
        L := L xor (FSBox1[B4] + FSBox2[B3] xor FSBox3[B2] + FSBox4[B1]);
    end;
    L := L xor FPArray[12];
    with TFourByte(L) do
    begin
        R := R xor (FSBox1[B4] + FSBox2[B3] xor FSBox3[B2] + FSBox4[B1]);
    end;
    R := R xor FPArray[13];
    with TFourByte(R) do
    begin
        L := L xor (FSBox1[B4] + FSBox2[B3] xor FSBox3[B2] + FSBox4[B1]);
    end;
    L := L xor FPArray[14];
    with TFourByte(L) do
    begin
        R := R xor (FSBox1[B4] + FSBox2[B3] xor FSBox3[B2] + FSBox4[B1]);
    end;
    R := R xor FPArray[15];
    with TFourByte(R) do
    begin
        L := L xor (FSBox1[B4] + FSBox2[B3] xor FSBox3[B2] + FSBox4[B1]);
    end;
    TDoubleDWORD(Plaintext).L := R xor FPArray[17];
    TDoubleDWORD(Plaintext).R := L xor FPArray[16];
end;

procedure TBlowfishCipher.DecryptBlock(var Ciphertext);
var
    L, R: DWORD;
begin
    L := TDoubleDWORD(Ciphertext).L;
    R := TDoubleDWORD(Ciphertext).R;
    L := L xor FPArray[17];
    with TFourByte(L) do
    begin
        R := R xor (FSBox1[B4] + FSBox2[B3] xor FSBox3[B2] + FSBox4[B1]);
    end;
    R := R xor FPArray[16];
    with TFourByte(R) do
    begin
        L := L xor (FSBox1[B4] + FSBox2[B3] xor FSBox3[B2] + FSBox4[B1]);
    end;
    L := L xor FPArray[15];
    with TFourByte(L) do
    begin
        R := R xor (FSBox1[B4] + FSBox2[B3] xor FSBox3[B2] + FSBox4[B1]);
    end;
    R := R xor FPArray[14];
    with TFourByte(R) do
    begin
        L := L xor (FSBox1[B4] + FSBox2[B3] xor FSBox3[B2] + FSBox4[B1]);
    end;
    L := L xor FPArray[13];
    with TFourByte(L) do
    begin
        R := R xor (FSBox1[B4] + FSBox2[B3] xor FSBox3[B2] + FSBox4[B1]);
    end;
    R := R xor FPArray[12];
    with TFourByte(R) do
    begin
        L := L xor (FSBox1[B4] + FSBox2[B3] xor FSBox3[B2] + FSBox4[B1]);
    end;
    L := L xor FPArray[11];
    with TFourByte(L) do
    begin
        R := R xor (FSBox1[B4] + FSBox2[B3] xor FSBox3[B2] + FSBox4[B1]);
    end;
    R := R xor FPArray[10];
    with TFourByte(R) do
    begin
        L := L xor (FSBox1[B4] + FSBox2[B3] xor FSBox3[B2] + FSBox4[B1]);
    end;
    L := L xor FPArray[9];
    with TFourByte(L) do
    begin
        R := R xor (FSBox1[B4] + FSBox2[B3] xor FSBox3[B2] + FSBox4[B1]);
    end;
    R := R xor FPArray[8];
    with TFourByte(R) do
    begin
        L := L xor (FSBox1[B4] + FSBox2[B3] xor FSBox3[B2] + FSBox4[B1]);
    end;
    L := L xor FPArray[7];
    with TFourByte(L) do
    begin
        R := R xor (FSBox1[B4] + FSBox2[B3] xor FSBox3[B2] + FSBox4[B1]);
    end;
    R := R xor FPArray[6];
    with TFourByte(R) do
    begin
        L := L xor (FSBox1[B4] + FSBox2[B3] xor FSBox3[B2] + FSBox4[B1]);
    end;
    L := L xor FPArray[5];
    with TFourByte(L) do
    begin
        R := R xor (FSBox1[B4] + FSBox2[B3] xor FSBox3[B2] + FSBox4[B1]);
    end;
    R := R xor FPArray[4];
    with TFourByte(R) do
    begin
        L := L xor (FSBox1[B4] + FSBox2[B3] xor FSBox3[B2] + FSBox4[B1]);
    end;
    L := L xor FPArray[3];
    with TFourByte(L) do
    begin
        R := R xor (FSBox1[B4] + FSBox2[B3] xor FSBox3[B2] + FSBox4[B1]);
    end;
    R := R xor FPArray[2];
    with TFourByte(R) do
    begin
        L := L xor (FSBox1[B4] + FSBox2[B3] xor FSBox3[B2] + FSBox4[B1]);
    end;
    TDoubleDWORD(Ciphertext).L := R xor FPArray[0];
    TDoubleDWORD(Ciphertext).R := L xor FPArray[1];
end;

class function TBlowfishCipher.MinKeyLength: Integer;
begin
    Result := 8;
end;

class function TBlowfishCipher.MaxKeyLength: Integer;
begin
    Result := 56;
end;


end.
