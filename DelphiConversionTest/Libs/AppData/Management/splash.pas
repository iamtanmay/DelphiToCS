{ --------------------------------------------------------------------------------------------------
  Ebene 1 (Utilities)
  --------------------------------------------------------------------------------------------------
  universelles Splash-Fenster mit Angaben zur Applikation
  --------------------------------------------------------------------------------------------------
  Datum    op  function/procedure     Änderung / Neuerung
  -------- --  -------------------    --------------------------------------------------------------
  07.05.98 wl  FormCreate             Einfügen von Application.Icon
  ab sofort im Pfad  \Tools\dialogs
  18.05.98 wl                         FormStyle = fsNormal statt fsStayOnTop
  16.06.98 wl  FormCreate             Einfügen von Application.Title
  23.06.98 wl                         Änderungen im Outfit für Version 3.3.0
  29.06.98 mo                         Fonts geändert
  05.11.98 wl                         Fankfurt -> Frankfurt
  31.05.99 wl                         ab Version 4.1 im Verzeichnis /SAMINTF/
  10.09.99 wl  FormCreate             Lizenz-Label entfernt - wurde sowieso nicht angezeigt
  04.01.00 mo  FormCreate             Oem Picture und Texte
  05.06.00 wl  FormCreate             Application.title wird erst in globals geändert
  19.01.01 mo FormCreate              erweiterte OEM Anzeige
  19.01.01 mo FormCreate              lesen der OEM Informationen
  18.09.02 wl                         TN1283 Merge mit SIAS
  18.10.02 wl                               TN1293.1 an TAppSettings angepasst
  03.07.03 wl                         TN1501   Compilerhinweise korrigiert
  22.12.04 wl                         TN2246.5  Änderungen der Oberfläche für XP-Optik
  21.09.06 wl                         TN3325   Fenster in neuer Optik
  17.03.08 wl                         TN4044   nicht mehr StayOnTop !!!
  20.05.10 wl                         TN5117   neue Schriftart "Segoe UI", Schriftgröße 9
  27.03.12 wl                         TN5848   Neues Zinsser-Logo
  -------------------------------------------------------------------------------------------------- }

unit splash;


interface


uses
    Forms,
    StdCtrls,
    ExtCtrls,
    Graphics,
    Controls,
    Classes;

type
    Tsplashform = class(TForm)
        Panel1: TPanel;
        lblTitle: TLabel;
        lblVersion: TLabel;
        Label2: TLabel;
        Image1: TImage;
        lblOem: TLabel;
        Copyright: TLabel;
        Label1: TLabel;
        Label3: TLabel;
        Image2: TImage;
        procedure FormCreate(Sender: TObject);
    end;

var
    splashform: Tsplashform;


implementation


{$R *.DFM}

uses
    SysUtils,
    ControlUtils,
    AppSettings;

procedure Tsplashform.FormCreate(Sender: TObject);
begin
    TControlUtils.ResetFontForWinXP(self);
    // load oem bitmap if file exists
    if FileExists('oem.bmp') then
        Image2.Picture.Loadfromfile('oem.bmp');

    Image1.Picture := TPicture(Application.Icon);
    lblOem.Caption := TAppSettings.OEM.OemTitle;
    lblTitle.Caption := Application.Title;
    lblVersion.Caption := TAppSettings.Version;
    if (TAppSettings.OEM.Copyr1 > '') then
    begin
        label1.Caption := TAppSettings.OEM.Copyr1;
        label3.Caption := TAppSettings.OEM.Copyr2;
        label2.Caption := TAppSettings.OEM.www;
    end;
end;


end.
