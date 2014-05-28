{ --------------------------------------------------------------------------------------------------
  Ebene 1 (Utilities)
  --------------------------------------------------------------------------------------------------
  universelles Fenster mit Angaben zur Applikation
  --------------------------------------------------------------------------------------------------
  Datum    op  function/procedure     Änderung / Neuerung
  -------- --  -------------------    --------------------------------------------------------------
  04.12.97 dl  FormCreate             Visualisierung der DLL Versions - Nummer
  07.05.98 wl  FormCreate             Einfügen von Application.Icon
  ab sofort im Pfad  \Tools\dialogs
  16.06.98 wl  FormCreate             Application.title wird eingefügt
  20.07.98 wl                         mit WWW-Adresse
  13.08.98 wl                         mit Support-Adresse
  16.03.99 wl  FormCreate             wenn (DLLVersionPtr<>nil) wird nichts angezeigt (für Audat)
  31.05.99 wl                         ab Version 4.1 im Verzeichnis /SAMINTF/
  10.09.99 wl                         Lizenzlabel geändert, Optische Änderungen
  17.12.99 mo                         Das schreckliche gelb in weiss geändert
  04.01.99 mo FormCreate              Oem Picture und Texte eingefügt
  05.06.00 wl  FormCreate             AddTitle entfernt
  14.08.00 mo FormCreate              DLLVersionPtr ist jetzt PChar
  19.01.01 mo FormCreate              erweiterte OEM Anzeige
  18.09.02 wl                         TN1283 Merge mit SIAS
  18.10.02 wl                               TN1293.1 an TAppSettings angepasst
  12.12.02 wl                         TN1345 DLLVersionPtr vorübergehend deaktiviert
  23.01.03 wl  FormCreate             TN1345 DLL-Version wird wieder angezeigt (mit TMinimal Interface)
  12.03.03 wl                         TN1293.5 uses posTools
  25.08.05 wl  FormCreate             TN2558.8 in Delphi 7 muß ParentBackground gesetzt sein
  21.09.06 wl                         TN3325   Fenster in neuer Optik
  21.09.06 wl                         TN3325   Internet-Adresse jetzt interaktiv
  21.09.06 wl                         TN3325   neu: Send Mail to support
  24.11.06 wl  lblFeatures            TN3420   Featurs wie REDI, Sophas, CFR21 werden hier angezeigt
  02.03.07 pk  DLLVersion             TN3612   Removed.  Doesn't make sense for CRS Projects
  09.01.08 wl  var AboutBox           TN3972   entfernt
  17.11.08 wl  gmSendMail             TN4312   für Delphi 2009 auskommentiert
  18.05.10 wl  gmSendMail             TN5111   Error.dat hat richtigen Pfad
  20.05.10 wl                         TN5117   neue Schriftart "Segoe UI", Schriftgröße 9
  27.03.12 wl                         TN5848   Neues Zinsser-Logo
  27.03.12 wl  SendMail               TN5848   funktioniert wieder!!
  -------------------------------------------------------------------------------------------------- }

unit About;


interface


uses
    Forms,
    ExtCtrls,
    Graphics,
    Classes,
    StdCtrls,
    Controls;

type
    TAboutBox = class(TForm)
        Panel1: TPanel;
        Versionlbl: TLabel;
        Copyright: TLabel;
        Label1: TLabel;
        Label2: TLabel;
        Buildlbl: TLabel;
        Lizenzlbl: TLabel;
        Label6: TLabel;
        Panel3: TPanel;
        Button1: TButton;
        Image1: TImage;
        ProductName: TLabel;
        Label3: TLabel;
        Label5: TLabel;
        lblFeatures: TLabel;
        lblSupport: TLabel;
        Label4: TLabel;
        lblOemSupport: TLabel;
        Label7: TLabel;
        imgOem: TImage;
        procedure FormCreate(Sender: TObject);
        procedure Button1Click(Sender: TObject);
        procedure Label3Click(Sender: TObject);
        procedure lblSupportClick(Sender: TObject);
        procedure lblOemSupportClick(Sender: TObject);
    private
        class procedure SendMail(const aSubject, aMailtext, aFromName, aToName: Ansistring;
            const aAttachedFiles: TArray<string>; const aDisplayFileName: Ansistring; aShowDialog: boolean);
        class procedure SendSupportMail(const aFromAdress: string);
    end;


implementation


uses
    WinTypes,
    ShellAPI,
    MAPI,
    SysUtils,
    Dialogs,
    ControlUtils,
    AppSettings,
    FileUtilities;

{$R *.DFM}

procedure TAboutBox.FormCreate(Sender: TObject);
begin
    TControlUtils.ResetFontForWinXP(self);
    // load oem bitmap if file exists
    if FileExists('oem.bmp') then
        imgOem.Picture.Loadfromfile('oem.bmp');

    if (TAppSettings.OEM.OemTitle <> '') then
        self.Caption := 'About ' + TAppSettings.OEM.OemTitle;
    lblOemSupport.Caption := TAppSettings.OEM.support;
    Image1.Picture := TPicture(Application.Icon);
    Versionlbl.Caption := TAppSettings.Version;
    Buildlbl.Caption := TAppSettings.Build;
    Lizenzlbl.Caption := TAppSettings.SerialNo;
    if TAppSettings.OEM.Copyr1 > '' then
    begin
        label1.Caption := TAppSettings.OEM.Copyr1;
        label2.Caption := TAppSettings.OEM.Copyr2;
        label3.Caption := TAppSettings.OEM.www;
        lblSupport.Caption := ''
    end;

    ProductName.Caption := Application.Title;

    lblFeatures.Caption := TAppSettings.GetSpecialFeatures();

    Panel1.ParentBackground := false;
    Button1.Default := true;
end;

procedure TAboutBox.Button1Click(Sender: TObject);
begin
    Close
end;

procedure TAboutBox.Label3Click(Sender: TObject);
begin
    ShellExecute(Application.Handle, 'open', PChar('http://' + Label3.Caption), nil, nil, sw_ShowNormal);
end;

class procedure TAboutBox.SendMail(const aSubject, aMailtext, aFromName, aToName: Ansistring;
            const aAttachedFiles: TArray<string>; const aDisplayFileName: Ansistring; aShowDialog: boolean);
var
    MapiMessage: TMapiMessage;
    MError: Cardinal;
    Empfaenger: array [0 .. 1] of TMapiRecipDesc;
    Absender: TMapiRecipDesc;
    Datei: array [0 .. 1] of TMapiFileDesc;
    x: integer;
begin
    with MapiMessage do
    begin
        ulReserved := 0;

        // Betreff
        lpszSubject := PAnsiChar(aSubject);

        // Body
        lpszNoteText := PAnsiChar(aMailtext);

        lpszMessageType := nil;
        lpszDateReceived := nil;
        lpszConversationID := nil;
        flFlags := 0;

        // Absender festlegen
        Absender.ulReserved := 0;
        Absender.ulRecipClass := MAPI_ORIG;
        Absender.lpszName := PAnsiChar(aFromName);
        Absender.lpszAddress := PAnsiChar(aFromName);
        Absender.ulEIDSize := 0;
        Absender.lpEntryID := nil;
        lpOriginator := @Absender;

        // Empfänger festlegen (Hier: nur 1 Empfänger)
        nRecipCount := 1;

        Empfaenger[0].ulReserved := 0;
        Empfaenger[0].ulRecipClass := MAPI_TO;
        Empfaenger[0].lpszName := PAnsiChar(aToName);
        Empfaenger[0].lpszAddress := PAnsiChar(aToName);
        Empfaenger[0].ulEIDSize := 0;
        Empfaenger[0].lpEntryID := nil;
        lpRecips := @Empfaenger;

        // Dateien anhängen (Hier: nur 1 Datei)
        nFileCount := Length(aAttachedFiles);

        for x := 0 to high(aAttachedFiles) do
        begin
            // Name der Datei auf der Festplatte
            Datei[x].lpszPathName := PAnsiChar(Ansistring(aAttachedFiles[x]));

            // Name, der in der Email angezeigt wird
            Datei[x].lpszFileName := PAnsiChar(Ansistring(ExtractFileName(aAttachedFiles[x])));
            Datei[x].ulReserved := 0;
            Datei[x].flFlags := 0;
            Datei[x].nPosition := Cardinal(-1);
            Datei[x].lpFileType := nil;
        end;

        lpFiles := @Datei;

    end;

    // Senden
    if aShowDialog then
        MError := MapiSendMail(0, Application.Handle, MapiMessage, MAPI_DIALOG or MAPI_LOGON_UI, 0)
    else
        // Wenn kein Dialogfeld angezeigt werden soll:
        MError := MapiSendMail(0, Application.Handle, MapiMessage, 0, 0);

    case MError of
        MAPI_E_AMBIGUOUS_RECIPIENT:
            MessageDlg('Empfänger nicht eindeutig. (Nur möglich, wenn Emailadresse nicht angegeben.)',
                mterror, [mbok], 0);

        MAPI_E_ATTACHMENT_NOT_FOUND:
            MessageDlg('Datei zum Anhängen nicht gefunden', mterror, [mbok], 0);

        MAPI_E_ATTACHMENT_OPEN_FAILURE:
            MessageDlg('Datei zum Anhängen konnte nicht geöffnet werden.', mterror, [mbok], 0);

        MAPI_E_BAD_RECIPTYPE:
            MessageDlg('Empfängertyp nicht MAPI_TO, MAPI_CC oder MAPI_BCC.', mterror, [mbok], 0);

        MAPI_E_FAILURE:
            MessageDlg('Unbekannter Fehler.', mterror, [mbok], 0);

        MAPI_E_INSUFFICIENT_MEMORY:
            MessageDlg('Nicht genug Speicher.', mterror, [mbok], 0);

        MAPI_E_LOGIN_FAILURE:
            MessageDlg('Benutzerlogin (z.B. bei Outlook) fehlgeschlagen.', mterror, [mbok], 0);

        MAPI_E_TEXT_TOO_LARGE:
            MessageDlg('Text zu groß.', mterror, [mbok], 0);

        MAPI_E_TOO_MANY_FILES:
            MessageDlg('Zu viele Dateien zum Anhängen.', mterror, [mbok], 0);

        MAPI_E_TOO_MANY_RECIPIENTS:
            MessageDlg('Zu viele Empfänger angegeben.', mterror, [mbok], 0);

        MAPI_E_UNKNOWN_RECIPIENT:
            MessageDlg('Empfänger nicht in Adressbuch gefunden. ' +
                '(Nur möglich, wenn Emailadresse nicht angegeben.)', mterror, [mbok], 0);

        // MAPI_E_USER_ABORT:
        // MessageDlg('Benutzer hat Senden abgebrochen oder MAPI nicht installiert.',mterror,[mbok],0);

        SUCCESS_SUCCESS:
            MessageDlg('Erfolgreich !!! (Aber Absenden nicht garantiert.)', mtinformation, [mbok], 0);

    end;
end;

class procedure TAboutBox.SendSupportMail(const aFromAdress: string);
var
    xSupportAddress: Ansistring;
    xFiles: TArray<string>;
begin
    xSupportAddress := Ansistring(aFromAdress);

    SetLength(xFiles, 1);
    xFiles[0] := TFileUtilities.GetApplicationDataPath() + 'Error.dat';
    // xFiles[1] := gCommonDll.CompleteZipBackup('DBDataBackup');

    SendMail('Support case', 'Attached: Error.dat', 'ZinsserWinlissy', xSupportAddress, xFiles, '', true);
end;

procedure TAboutBox.lblOemSupportClick(Sender: TObject);
begin
    SendSupportMail(lblOEMSupport.Caption);
end;

procedure TAboutBox.lblSupportClick(Sender: TObject);
begin
    SendSupportMail(lblSupport.Caption);
end;


end.
