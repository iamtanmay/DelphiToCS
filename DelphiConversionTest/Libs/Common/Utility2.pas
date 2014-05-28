{ --------------------------------------------------------------------------------------------------
  Ebene 1 (Utilities)
  --------------------------------------------------------------------------------------------------
  Allgemein n¸tzliche Funktionen f¸r Audat und Sampler-Software
  --------------------------------------------------------------------------------------------------
  Datum    op  function/procedure     ƒnderung / Neuerung
  -------- --  -------------------    --------------------------------------------------------------
  20.11.98 wl                         neue Unit
  gmHexStr,gmReadSubStr  aus Utility hierher verschoben
  FormStr                aus postools hierher verschoben
  gmCreateString         aus Tools(Audat) hierher verschoben
  30.11.98 wl  gmStrToIntTry          aus Utility hierher verschoben
  05.01.99 mo  CheckPath              neu ‹berpr¸ft ob ein Pfad vorhanden ist
  21.01.99 wl  gmDeleteString         aus Utility hierher verschoben
  26.01.99 wl  gmStrToIntTry          im Fehlerfall ist IntResult=0
  28.01.99 wl  gmDelCharFromString    Ertfernt alle Zeichen eines Typs aus einem String
  11.12.99 wl  gmCopyAndRemoveDir     Funktion zum Kopieren und Lˆschen aller Dateien in einem Verzeichnis
  13.12.99 wl  gmCopyAndRemoveDir     jetzt auch nur Kopieren mˆglich
  gmLogText              entspricht der Funktion LogText (SamIntf)
  12.01.99 wl  gmLogText              verbessert: Texte werden gek¸rzt
  gmWriteLog             Schreiben der LogFile-Zeile: FileName und Typ konfigurierbar
  11.04.00 wl  LogText                alte Funktion gelˆscht
  14.04.00 wl  gmTimeInSeconds        neu: aus SOPHAS (ChkTThrd)
  06.09.00 wl  gmTimeInSeconds        Funktion verallgemeinert f¸r alle mˆglichen DateTime-Werte
  12.09.01 mo  gmStrToFloatTry        TN1006 neu: Konvertierung mit , oder .
  22.02.02 tbh gmStrToWordTry         TN1052.2 neu: Konvertierung von String zu Word
  03.07.02 tbh gmTimeStrInDateTime    TN1238 neu: Konvertiert ZeitString zu TDateTime-Typ
  13.09.02 wl                         TN1283 Merge mit SIAS
  16.09.02 mo                         TN1262 ifndef IBTOOLS...endif
  18.09.02 wl                         TN1283 Merge mit SIAS
  12.12.02 wl  Sam                    TN1345 wird hier definiert! (als TMinimalInterface)
  12.12.02 wl  gmWriteLog             TN1345 Inhalt --> TMinimalInterface.WriteLog
  12.03.03 wl  gmLogText,gmLogErrorText,gmWriteLog  TN1293.5 --> posTools
  12.03.03 wl  gmCopyDirectory,gmCopyFiles          TN1293.5 --> posTools
  12.03.03 wl  gmTimeStrInDateTime    TN1293.5 iTime wurde uninitialisiert zu result hinzugef¸gt - korrigiert
  19.03.03 wl  gmReadSubString        TN1293.5 neue Funktion, die auch richtig liest, wenn Delimiter an erster Stelle steht
  19.03.03 wl  gmReadSubStr           TN1293.5 wird aus Kompatibilit‰tsgr¸nden so gelassen, wie sie ist
  24.07.03 wl  gmReadSubStr           TN1501.7 wenn CurrentChar = 0 wird er auf 1 gesetzt - f¸hrte vorher zu Exception (aufgefallen bei TubeTool-Pr¸fung)
  24.07.03 wl  gmReadSubString        TN1501.7 wenn CurrentChar = 0 wird er auf 1 gesetzt - f¸hrte vorher zu Exception (aufgefallen bei TubeTool-Pr¸fung)
  05.08.05 thr SetBit / Checkbit      TN2532  Routinen zur Bitmanipulation
  05.09.07 wl                         TN3850   kleine ƒnderungen zur Vereinfachung der C#-Konvertierung
  13.02.08 wl  FormStr                TN4009   --> qryTools
  18.02.08 wl                         TN4009   alle nicht verwendeten Funktionen entfernt
  -------------------------------------------------------------------------------------------------- }

unit Utility2;


interface


function gmReadSubString(var aCurrentChar: integer; aText, aDelimiter: string): string; // neu!!
function gmReadSubStr(var ActualChar: integer; S, Delimiter: string): string; // alt!
function gmStrToIntTry(var IntResult: integer; Text: string): boolean;
function gmTimeInSeconds(iTime: TDateTime): Int64;

// ==================================================================================================
// ===== Utility Unit: No dependencies! No variables! No side effects! ==============================
// ==================================================================================================

// ##################################################################################################


implementation


uses
    SysUtils;

// --------------------------------------------------------------------------------------------------
function gmReadSubString(var aCurrentChar: integer; aText, aDelimiter: string): string;
// --------------------------------------------------------------------------------------------------
// Achtung! ge‰nderte Funktion! Bei neuen Funktionen sollte diese Funktion bevorzugt werden!
// Problem: Ist nicht 100% kompatibel, da ActPos am Schluﬂ an anderer Stelle steht als bei der alten Funktion
//
// Liest aus string S einen SubString, bis der Delimiter (z.B. ',') erreicht ist.
// Die aktuelle Position (ActualChar) wird hinter den n‰chsten Delimiter verschoben.
// --------------------------------------------------------------------------------------------------
var
    xDelimiterChar: char; // nur der erste Buchstabe wird benutzt
begin
    result := '';
    xDelimiterChar := aDelimiter[1];
    if (aCurrentChar < 1) then
        aCurrentChar := 1; // bei 0 passiert das ungeheure
    while (aCurrentChar <= Length(aText)) and (aText[aCurrentChar] <> xDelimiterChar) do
    begin
        result := result + aText[aCurrentChar];
        inc(aCurrentChar);
    end;
    inc(aCurrentChar);
end;

// --------------------------------------------------------------------------------------------------
function gmReadSubStr(var ActualChar: integer; S, Delimiter: string): string;
// --------------------------------------------------------------------------------------------------
// Alte Funktion!
// Problem: Liest falsch, wenn an erster Stelle nichts steht (String beginnt mit Delimiter)
//
// Liest aus string S einen SubString, bis der Delimiter (z.B. ',') erreicht ist.
// Die aktuelle Position (ActualChar) wird hinter den n‰chsten Delimiter verschoben.
// --------------------------------------------------------------------------------------------------
var
    xDelimiterChar: char; // nur der erste Buchstabe wird benutzt
begin
    result := '';
    xDelimiterChar := Delimiter[1];
    if (ActualChar < 1) then
        ActualChar := 1; // bei 0 passiert das ungeheure
    if ActualChar > length(S) then
        exit;
    repeat
        if S[ActualChar] <> xDelimiterChar then
            result := result + S[ActualChar];
        inc(ActualChar);
    until (S[ActualChar] = xDelimiterChar) or (ActualChar > length(S));
end;

// ------------------------------------------------------------------------------
function gmStrToIntTry(var IntResult: integer; Text: string): boolean;
// ------------------------------------------------------------------------------
var
    d: integer;
begin
    val(Text, IntResult, d);
    if (d = 0) then
        result := true
    else
        result := false;
end;

// --------------------------------------------------------------------------------------------------
function gmTimeInSeconds(iTime: TDateTime): Int64;
// --------------------------------------------------------------------------------------------------
var
    Year, Month, Day: word;
    Hour, Min, Sec, MSec: word;
begin
    DecodeDate(iTime, Year, Month, Day);
    DecodeTime(iTime, Hour, Min, Sec, MSec);
    result := 365 * 24 * 3600 * Year + 30 * 24 * 3600 * Month + 24 * 3600 * Day + Hour * 3600 + Min *
        60 + Sec;
end;


end.
