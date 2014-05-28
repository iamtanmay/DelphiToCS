{ --------------------------------------------------------------------------------------------------
  Allgemeine Schnittstelle zu externen (anwendungsspezifischen) DLLs
  --------------------------------------------------------------------------------------------------
  Datum     op  function/procedure     Änderung / Neuerung
  --------  --  -------------------    -------------------------------------------------------------
  08.12.00  tbh                        neu
  18.12.00  tbh InitExternalDLLs       initialisiert cp-prozeduren nur bei SAMI-Lizenz
  12.01.01  tbh InitExternalDLLs       CPCheckRackID hat anderen Übergabeparameter
  19.01.01  tbh InitExternalDLLs       GetProcAdress wird nach 'nil' abgefragt
  26.03.01  mo  CPCreateSamRunTable    Neuer Parameter
  17.05.01  mo  CPGetNextSRfromStorage Neue Funktion aus IBTOOLS
  17.05.01  mo  SU_GetNextFreeStorageSlot Neue Funktion aus SAMUTIL
  27.11.01  tbh CWInsertWeighData      TN1051 neu: CALLIWEIGH-Funktion zum Schreiben der Wiegedaten
  27.11.01  tbh InitExternalDLLs       TN1051 Funktionen für CALLIWEIGH eingeführt
  28.11.01  tbh CWInsertWeighData      TN1051 Rückgabewert per Call-by-Reference
  30.11.01  tbh InitExternalDLLs       TN1123 DllHandle initialisiert
  04.12.01  tbh CWInsertWeighData      TN1129 jetzt Funktion mit Rückgabewert Boolean
  11.12.01  mo  CWInsertWeighData      TN1051 Parameter erweitert
  28.12.01  tbh CWGetTool              TN1051 neu: CALLIWEIGH-Funktion zur Auswahl des Pipettiertools
  29.12.01  tbh CWInsertWeighData      TN1051 Funktion um Target erweitert
  03.05.02  tbh SU_CopyFile            TN1052.1 Neue Funktion aus SAMUTIL für SlaveModus
  03.05.02  tbh SU_WaitForSlave        TN1052.1 Neue Funktion aus SAMUTIL für SlaveModus
  03.05.02  tbh SU_ExportRacks         TN1052.1 Neue Funktion aus SAMUTIL für SlaveModus
  03.05.02  tbh SU_ImportRacks         TN1052.1 Neue Funktion aus SAMUTIL für SlaveModus
  12.07.02  tbh diverse                TN1243 neue Funktionen für Skript-Archivierung
  09.10.02 wl                          TN1293.1 divided into rcRobotics and rcCommon
  18.10.02 wl                          TN1293.1 benutzt TAppSettings
  05.11.02 wl  alle SU_..-Methoden     TN1329 aus DLL hierher verschoben, benutzen TAppSettings.Alias
  06.11.02 wl                          TN1329 uses FileCtrl
  22.11.02 mo  s2z....                 TN1240 neu: Funktionen aus Sym2Zin.dll
  29.11.02 mo  s2z_CreateRunFromXML    TN1240 neu
  03.12.02 mo  s2z_CreateRunFromXML    TN1240 neuer Parameter
  20.12.02 wl                          TN1293.5 uses und WinlissyIniAccess geändert
  30.12.02 wl  Archiving-Methoden      TN1332  --> Archiving
  21.01.03 mo  InitExternalDLLs        TN1383 loads Hummingbird.dll
  22.01.03 mo  uses                    TN1383 + Dialogs
  12.03.03 wl                          TN1293.5 uses posTools
  09.04.03 wl                          TN1293.5 unbenutzte Variablen entfernt
  24.06.04 wl                          TN2007   FileCtrl-Aufrufe durch Utility-Aufrufe ersetzt
  07.09.05 wl                          TN2585   TLoginModes hierher verschoben
  13.12.05 wl  SU_CopyFile             TN2856   Path & File werden als ein String übergeben, zusätzlicher Parameter DeleteIfFileExists
  15.12.05 wl  SU_CopyFile             TN2856   mit aDeleteSourceFile kann die Funktion auch zum Verschieben oder Löschen von Dateien genutzt werden
  15.12.05 wl  SU_CopyFile             TN2856   Jetzt mit Fehlerbehandlung: RETRY möglich!!!
  17.02.06 wl  InternCopyFile          TN2940   Wenn nur gelöscht werden soll, gibt es keinen Fehler
  02.10.06 wl  InternCopyFile          TN3339   Statt bei fehlendem Zielverzeichnis eine Fehlermeldung zu bringen, wird dieses erzeugt
  03.12.06 wl                          TN3243   alle ErrBox-Aufrufe durch ErrBoxSimple oder ErrBoxSerialIntf ersetzt
  26.01.07 pk  SU_WaitForSlave         TN3525.3 avoid using globalerrptr.  instead use isglobalerr functions
  21.06.07 wl  CWInsertWeighData       TN3709.2 neuer var Parameter: vIsStored
  09.11.07 pk                          TN3923   class references to TErrorMessageFactory changed to gErrorMessageFactory
  09.01.08 wl                          TN3972   Master/Slave-Krempel  entfernt
  04.03.09 pk  SU_CopyFile             TN4451   --> FileCopyRunAction
  10.08.09 wl                          TN4702   Strings werden jetzt direkt geladen
  08.09.09 pk                           TN4753   uses ErrorMessage replaced by ErrorInfo
  13.04.10 wl                           TN5044   uses geändert
  18.06.10 pk   gmQS                    TN5152.1 from QryTolls
  18.06.10 wl                           TN5116   aufgeräumt
  20.10.10 wl                           TN5116   nochmal aufgeräumt
  20.10.11 wl  gmConnectExtDatabases    TN5723   hierher verschoben
  ------------------------------------------------------------------------------------------------- }

unit ExternDllFunc;


interface


uses
    Windows,
    AppTypes,
    ComCtrls,
    Classes,
    Dialogs;

type
    TIDstr = string[40];

    TLoginMode = (CPMode, CWMode);
    TLoginModes = set of TLoginMode;

var
    // ------------------------------------------------------------------------------------ IBTools.Dll
    IBLogin: function(IBUser, IBPassword, IBDatabaseName: PChar; AppHwnd: HWND; LoginModes: TLoginModes)
        : Boolean; // zum Einloggen in Datenbank
    IBConnected: function(LoginModes: TLoginModes): Boolean; // prüft ob mit Datenbank verbunden
    CWInsertWeighData: function(Device, Tool, Substance, Paramset, Run: PChar; var RunID: Integer;
        var ProcessID: Integer; Step: Integer; Weight, Target: Double; var vIsStored: boolean): Boolean;
    CWGetTool: function(Device, SubstID: PChar; ProcessID: integer; TargetWeight: Double;
        var Result: TCWGetToolResult): Boolean;

function InitExternalDLLs: Boolean;
procedure gmConnectExtDatabases;


implementation


uses
    Forms,
    SysUtils,
    Controls,
    SamGlobe,
    GUIManager,
    GeneralTypes;

// --------------------------------------------------------------------------------------------------
function InitExternalDLLs: Boolean;
// --------------------------------------------------------------------------------------------------
var
    DllHandle: Integer;
    AllFuncPresent: Boolean;
    DllName: string;
begin
    result := true;
    DllHandle := 0;
    AllFuncPresent := true;
    if (gWeighLogicDbName <> '') then
    begin // bei CW-Applikation --------------------------------------
        if (DllName = '') then
            DllName := gDbToolDllName;
        if (DLLHandle = 0) then
            DllHandle := LoadLibrary(PChar(gDbToolDllName));
        if (DLLHandle <> 0) then
        begin
            if (GetProcAddress(DllHandle, 'IBLogin') <> nil) then
                @IBLogin := GetProcAddress(DllHandle, 'IBLogin')
            else
                AllFuncPresent := false;
            if (GetProcAddress(DllHandle, 'IBConnected') <> nil) then
                @IBConnected := GetProcAddress(DllHandle, 'IBConnected')
            else
                AllFuncPresent := false;
            if (GetProcAddress(DllHandle, 'CWInsertWeighData') <> nil) then
                @CWInsertWeighData := GetProcAddress(DllHandle, 'CWInsertWeighData')
            else
                AllFuncPresent := false;
            if (GetProcAddress(DllHandle, 'CWGetTool') <> nil) then
                @CWGetTool := GetProcAddress(DllHandle, 'CWGetTool')
            else
                AllFuncPresent := false;
            if not(AllFuncPresent) then
            begin
                gGUIManager.MessageBox(TLanguageString.
                    Read('Wrong Version of {0} ! Contact Zinsser support !',
                    'Falsche Version der {0} ! Kontaktieren Sie den Zinsser Support !', [DllName + '.dll']),
                    TLanguageString.Read('Error', 'Fehler'), MB_ICONSTOP);
                result := false;
            end;
        end;
    end;
    // ---------------------------------------------------------------------------- Hummingbird
    try
        LoadLibrary('Hummingbird.dll');
    except
        on E: Exception do
            MessageDlg('Error in Hummingbird.dll: ' + E.Message, mtInformation, [mbOk], 0);
    end;
end;

// --------------------------------------------------------------------------------------------------
procedure gmConnectExtDatabases;
// --------------------------------------------------------------------------------------------------
begin
    if (gWeighLogicDbName <> '') then
    begin
        if not(IBConnected([CWMode])) then
            IBLogin(PChar(gWeighLogicDbUser), PChar(gWeighLogicDbPassword), PChar(gWeighLogicDbName),
                Application.Handle, [CWMode]);
    end;
end;


end.
