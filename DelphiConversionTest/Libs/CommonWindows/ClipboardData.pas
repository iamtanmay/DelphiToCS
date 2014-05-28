{ --------------------------------------------------------------------------------------------------
  Copyright © 2005 Zinsser Analytic GmbH - All rights reserved.
  Project      : New Editor
  Author       : Wolfgang Lyncke (wl)
  Description  : Common class for clipboard actions
  --------------------------------------------------------------------------------------------------
  Revision History:
  date     op  method                       track-no  improvement/change
  -------- --  ---------------------------  --------  ----------------------------------------------
  17.06.05 wl                               TN2438    initial version
  04.02.09 wl                               TN4370    PAnsiChar statt PChar
  -------------------------------------------------------------------------------------------------- }

unit ClipboardData;


interface


uses
    Classes;

type
    TClipboardData = class
    private
        fClipboardFormat: integer;
    protected
        class function ReadAnsiString(Stream: TStream): AnsiString;
        class procedure WriteAnsiString(Stream: TStream; str: AnsiString);
        procedure LoadFromStream(Stream: TStream); virtual; abstract;
        procedure SaveToStream(Stream: TStream); virtual; abstract;
    public
        constructor Create(aClipboardFormat: integer);
        //
        procedure LoadFromClipboard();
        procedure SaveToClipboard();
        function ClipboardHasFormat(): boolean;
    end;


    // ##################################################################################################


implementation


uses
    Windows,
    Clipbrd;

constructor TClipboardData.Create(aClipboardFormat: integer);
begin
    inherited Create();

    fClipboardFormat := aClipboardFormat;
end;

class function TClipboardData.ReadAnsiString(Stream: TStream): AnsiString;
var
    Len: Longword;
begin
    Stream.Read(Len, SizeOf(Len));
    SetLength(Result, Len);
    Stream.Read(PAnsiChar(Result)^, Len);
end;

class procedure TClipboardData.WriteAnsiString(Stream: TStream; str: AnsiString);
var
    Len: Longword;
begin
    Len := length(str);
    Stream.Write(Len, SizeOf(Len));
    Stream.Write(PAnsiChar(str)^, Len);
end;

procedure TClipboardData.SaveToClipboard();
var
    Stream: TMemoryStream;
    MemHandle: THandle;
    MemBlock: Pointer;
begin
    Stream := nil;
    try
        // Einen Memory-Stream erzeugen, in den wir alle Daten des Kontaktes ablegen
        Stream := TMemoryStream.Create;

        // Das Zwischenformat in den Stream schreiben
        SaveToStream(Stream);

        // Speicher für die Zwischenablage anfordern
        MemHandle := GlobalAlloc(GMEM_DDESHARE, Stream.Size);
        // Den Speicher fixieren
        MemBlock := GlobalLock(MemHandle);
        try
            // Den Inhalt des Streams in den Speicher kopieren
            Stream.Seek(0, soFromBeginning);
            Stream.Read(MemBlock^, Stream.Size);
        finally
            // Die Fixierung wieder aufheben
            GlobalUnlock(MemHandle);
        end;
        // Den Speicher an die Zwischenablage übergeben und als das
        // richtigen Format kennzeichnen
        Clipboard.Open;
        Clipboard.SetAsHandle(fClipboardFormat, MemHandle);
        Clipboard.Close;
    finally
        Stream.Free;
    end;
end;

procedure TClipboardData.LoadFromClipboard();
var
    Stream: TMemoryStream;
    MemHandle: THandle;
    MemBlock: Pointer;
    ASize: integer;
begin
    Clipboard.Open;
    try
        if Clipboard.HasFormat(fClipboardFormat) then
        begin // Wenn in der Zwischenablage etwas in richtigen Format vorliegt

            // Eine Referenz auf die Daten in der Zwischenablage anfordern
            MemHandle := Clipboard.GetAsHandle(fClipboardFormat);

            if MemHandle <> 0 then
            begin

                // Die Größe (Anzahl der Bytes) feststellen
                ASize := GlobalSize(MemHandle);
                Stream := nil;
                try
                    Stream := TMemoryStream.Create;

                    // Den Inhalt der Zwischenablage fixieren
                    MemBlock := GlobalLock(MemHandle);
                    try
                        // Die Daten in den Stream kopieren
                        Stream.Write(MemBlock^, ASize);
                    finally
                        GlobalUnlock(MemHandle);
                    end;

                    // Den Inhalt des Streams in das Zwischenformat umwandeln
                    Stream.Seek(0, soFromBeginning);
                    LoadFromStream(Stream);
                finally
                    Stream.Free;
                end;
            end;
        end;
    finally
        Clipboard.Close;
    end;
end;

function TClipboardData.ClipboardHasFormat: boolean;
begin
    Clipboard.Open;
    try
        result := Clipboard.HasFormat(fClipboardFormat);
    finally
        Clipboard.Close;
    end;
end;


end.
