{ --------------------------------------------------------------------------------------------------
  Ebene 3 (Run-Tools)
  --------------------------------------------------------------------------------------------------
  Liquid Handling Settings - Formular
  --------------------------------------------------------------------------------------------------
  Datum     op  function/procedure     Änderung / Neuerung
  --------  --  -------------------    -------------------------------------------------------------
  17.11.97  dl  SpeedButton1Click      Copy Funktion implementiert
  Spin Buttons zum Port schalten
  Lookup List für Liquid Parameter Namen
  19.11.97  dl  LDetDilAspParaUpdate   Liquid Detection Parameter für Dispense
  LDetdispParaUpdate     und Dil Asp implementiert
  ...
  25.11.97  dl  liquid Parameter       Umstellung aller Delay Felder auf TFloatField
  09.12.97  dl  Class TLiqParam        jetzt gültig für alle Software Projekte
  (Sampler - SW: SamLiqH.pas) Objekt TSamLiqHPar hat TLiqParam als Vorfahr
  (Sophas - SW): nutzt TLiqParam direkt
  02.02.98  mo  edSPortNrChange        Aufruf von FormActivate
  deEdSPortDilChange      "
  edSDilPortChange        "
  24.03.98  mo                         Im Dialog "Insert" in "Enter" geändert
  = KV Solvay 1.1.0
  02.06.98  wl                         Zahlreiche Änderungen (s.u.)
  Problem: Beim Ändern der Liquid Detection Parameter oder der Switch Ports wird die Tabelle nicht in den Edit-Status gesetzt
  Abhilfe: Vor diesen Änderungen muß ein anderer Parameter geändert werden. Danach sind Änderungen bei den o.g. Parametern möglich.
  = KV 3.2.0
  10.06.98  wl                         überarbeitete Fassung (Problem wurde behoben)
  --------------------------------------------------------------------------------------------------
  Alle Änderungen seit dem 24.03.98:
  Datenbankaktionen:
  -Oberfläche-              Entfernen der Navigator-Buttons <<,<,>,>> und Insert
  Bewegen zwischen den Datensätzen erfogt mit DBGrid1
  sbInsert: Zum Hinzufügen eines Datensatzes
  LiqParTableBeforeInsert   Fragt neuen Namen ab, und stoppt Action wenn es den Namen schon gibt
  LiqParTableAfterInsert    neuer Name wird PARAMNAME
  LiqParTableBeforeDelete   Nachfrage, ob dieser Datensatz wirklich gelöscht werden soll
  sbCopyClick               = SpeedButton1Click --> start vereinfachte Kopierfunktion
  ChangesSave               Fragt nach, ob Änderungen am Datensatz gespeichert werden sollen
  LiqParTableBeforeClose    Aufruf von ChangesSave, bevor die Datenbank geschlossen wird
  DBGrid1Enter              Aufruf von ChangesSave vor dem Bewegen zwischen Datensätzen
  sbInsertClick,sbCopyClick Aufruf von ChangesSave vor dem Einfügen eines Datensatzes
  Drucken:
  -Oberfläche-              sbPrint:  Zum Drucken eines Settings
  sbPrint                   Zeigt die Druckansicht - LiqParPR -
  -Variable-                QRLiqParOpen: Bezeichnet, ob Druckansicht geöffnet ist
  FormCreate                QRLiqParOpen:=false;
  sbPrint                   QRLiqParOpen:=true;
  FormDeactivate            Automatisches Schließen nur, wenn Druckansicht nicht geöffnet ist
  Liquid Detection:
  -Oberfläche-              Änderung und Vereinheitlichung der Namensbezeichnungen aller Liquid Detection-Controls:
  ( Sample Aspiration: boxSampleAspLiqDet,boxSampleAspDispErr,boxSampleAspGotoZ,boxSampleAspLevTr,...)
  (          Dispense: boxDispLiqDet,boxDispDispErr,boxDispGotoZ,boxDispLevTr,...)
  (Diluent Aspiration: boxDilAspLiqDet,boxDilAspDispErr,boxDilAspGotoZ,boxDilAspLevTr,...)
  SampleAspLiqDetUpdate     = LParaUpdate ; Bei Änderung wird der Tabellenstatus auf -EDIT- gesetzt
  DispLiqDetUpdate          = LDetdispParaUpdate ; Bei Änderung wird der Tabellenstatus auf -EDIT- gesetzt
  DilAspLiqDetUpdate        = LDetDilAspParaUpdate ; Bei Änderung wird der Tabellenstatus auf -EDIT- gesetzt
  edSampleAspLiqDetChange   = EditSampleAspLiqDetChange (stark vereinfacht)
  edDispLiqDetChange        = edDBDispLiqDetChange (stark vereinfacht)
  edDilAspLiqDetChange      = edDBDilAspLiqDetChange (stark vereinfacht)
  ...                       Namensänderungen der Funktionen entsprechend der Control-Namen
  MultiPipetting:
  MultiPipettingSet         Zeigt die mpPanel und/oder mpWashPanel
  FormActivate              Auslagern der MultiPipetting-Oberflächeneinstellungen in MultiPipettingSet
  cbMPwashClick,TabSheet6Enter,
  cbMultiPipettClick        Aufruf von MultiPipettingSet
  Switch Positions:
  -Oberfläche-              Ersetzen von SpinEdit1,2,3 (für Switch Ports) durch UpDown-Controls
  (updSPortNr,updSDilPort,updDispSPort)
  FormCreate                Entfernen SpinEdit-Zeilen
  FormActivate              Entfernen SpinEdit-Zeilen
  updSPortNrClick           Ändern des Inhalts von edSPortNr; setzt Datenbankstatus auf EDIT
  updSDilPortClick          Ändern des Inhalts von edSDilPort; setzt Datenbankstatus auf EDIT
  updDispSPortClick         Ändern des Inhalts von edDispSPort; setzt Datenbankstatus auf EDIT
  SpinEdit1Change,SpinEdit2Change,SpinEdit3Change  ---> entfernt
  Sonstiges:
  -Oberfläche-              edParamname: Verändern eines Parameternamens nicht mehr möglich
  edDescription: Editieren der Description möglich
  -Objekt-Deklaration-      var LiqParam : TLiqParam; --> um unabhängig von SamLiqH aufgerufen zu werden
  EditSampleAspMixCyclExit  Funktion entfernt (war vorher bereits deaktiviert)
  DBLookupComboBox1MouseUp,
  deEdSPortDilChange,
  edSPortNrChange,
  DBNavigatorClick,...      entfernt (durch Oberflächenänderungen unnötig geworden)
  10.06.98 wl                           = KV 3.2.0 (Build 980615)
  --------------------------------------------------------------------------------------------------
  Problem: Die Funktion FormClose von QRLiqPar wird beim Schließen des Print-Fensters Übersprungen:
  --> QRLiqParOpen wird nicht zurückgesetzt
  15.06.98 mo                            edDilAspLiqDet,edSampleAspLiqDet,edDispLiqDet visible=false
  22.06.98 mo  MultiPipettingSet         MPwashPanel.visible... ausgeklammert
  cbMPWash. visible = false
  02.07.98 wl                            Rechtschreibfehler korrigiert --> noch als V 3.3.0 eingebucht
  01.10.98 wl                            uses NameDlg entfernt
  05.10.98 wl  FormClose                 LiqParam := nil
  08.10.98 wl  LiqParTableAfterInsert    bessere Default-Werte
  25.01.99 wl                            10 neue Felder für LiqParam.db (Mixen & AirDelay)
  TabSheet Mix um neue Felder erweitert
  TabSheet Aspirate in Sample und Diluent aufgeteilt
  26.01.99 wl                            Verbesserungen an der Oberfläche (Mix,Options)
  Max.Vol.(%) für Mixen ist noch unsichtbar
  LiqParTableBeforePost     Prüfen, ob DispSysAir > AspSysAir
  01.02.99 wl                            neu in Mix: SampleAspMixFirstOnly
  19.04.99  hc                         Neue Parameter hinzugefügt -AspirationMethodRetractZ + RecordDetectionVolume
  08.09.99 wl                            neu: Tip Configuration: Auswählen eines Tipsets
  09.09.99 wl  edUsedTipsChange          Tipset wird richtig angezeigt und gespeichert
  TabSheet 'Z-Positions' aufgeteilt auf Aspirate und Dispense
  26.05.00 wl                         uses geändert
  23.06.00 wl  DMRackCreate              neu in Tip Configuration: UseDispTipWithoutTip
  04.07.00 wl  rgrAspSwitchPosChange,..  Statt PortNumber wird Modulname ausgewählt, Funktion vereinfacht
  04.07.00 wl                            neu: System Air Suck Position  bei Aspirate Sample
  12.07.00 wl     neue Felder für Kanal 2 (Asp.Diluent/Wash At Aspirate/After Dispense) & optische Änderungen
  07.08.00 wl  FormActivate              GetSwitchList: geänderte Parameter
  16.08.00 mo  ...                       neue Checkboxen für Single Tipd detection
  27.09.00 tbh ...                       neue Checkbox für Zwangswaschen
  18.12.00 tbh ...                       neue Checkboxen für Waschen mit Waschmakro
  28.02.01 tbh FormActivate              pnWashMacro ist sichtbar wenn Waschmakro angewählt ist
  28.02.01 tbh DBGrid1CellClick          neu: bei Wechsel von Parameter zu anderem werden Einstellungen aktualisiert
  28.07.01 tbh cbWashAfterDispClick      wenn deaktiviert wird UseWashMacro deaktiviert
  28.07.01 tbh cbWashAfterDispClick      wenn aktiviert wird cbWashIsForced erst sichtbar
  12.11.01 tbh edUsedTipsChange          TN1092 FcbTips sind wenn checked mit UsedTipType initialisiert
  12.11.01 tbh btChangeTipTypeClick      TN1092 schreibt TArm.FTips zu eingestelltem UsedTipType um
  12.11.01 tbh cbUsedTipTypeChange       TN1092 ersetzt btChangeTipTypeClick
  23.11.01 tbh cbUsedTipTypeChange       TN1112 wird nur ausgeführt wenn gChangeTipTypes=true
  23.11.01 tbh edUsedTipsChange          TN1112 wenn gChangeTipTypes=true werden FcbTips mit UsedTipType initialisiert
  23.11.01 tbh FormActivate              TN1112 wenn gChangeTipTypes=true ist rbAllTips nicht mehr angezeigt
  30.01.02 tbh FUsedTipType              TN1164/TN1165 neu: Zwischenablage für TipType wenn ChangeTipType=1
  30.01.02 tbh edUsedTipsChange          TN1164/TN1165 FUsedTipType eingearbeitet
  30.01.02 tbh DBGrid1CellClick          TN1164/TN1165 FUsedTipType eingearbeitet
  11.09.02 mo                            TN1283 Merge mit SIAS
  10.10.02 wl                               TN1293.2 Ini Access - uses geändert
  16.10.02 wl                               TN1293.2 High(gTipTypes) statt cMaxTipTypes
  04.11.02 wl  FormCreate                TN1329 DatabaseName für LiqParTable: TAppSettings.Alias
  12.12.02 wl                            TN1345 Sam.nDilutors ersetzt gmGetDilutorAmount
  20.12.02 wl                               TN1293.5 uses und WinlissyIniAccess geändert
  28.01.03 wl                               TN1293.5 geänderte gModules-Methodenaufrufe
  13.02.03 wl  LiqParTableBeforeDelete   TN1334.3 Löschen von Daten wird geloggt
  13.02.03 wl  LiqParTableBeforePost     TN1334.3 Editieren und hinzufügen von Daten wird geloggt
  05.03.03 tbh FormCreate                TN1430 keine Auswahl nach TipType mehr wenn gChangeTipTypes=true
  02.06.03 wl                            TN1485.4 TAppSettings.IsSias ersetzt die ifdefs
  04.06.03 wl  FormCreate                TN1493.11 Festlegung der Airgap Z-Positionen auch für SIAS sichtbar
  02.10.03 wl                            TN1611    StrToInt durch gmStrToIntTry ersetzt - unterdrückt Meldung "..ist kein gültiger integerwert"
  02.10.03 wl                            TN1580    Alle Beschränkungen für Sias aufgehoben
  26.02.04 wl                            TN1574   ist jetzt ein normales modales Fenster
  26.02.04 wl  sbPrintClick              TN1574   Erzeugen von Quick-Report-Fenster in try-except-Block
  18.03.04 wl                            TN1708   10 neue Felder: (ScanMode,ScanSpeed,RetractSpeed) * 3 + WashRetractSpeed
  05.04.04 wl  FormActivate              TN1788   alle Bezüge auf WB.Arm durch gTipManager ersetzt
  05.04.04 wl  ShowTipNames              TN1788.3 bei "ChangeTipTypes werden die Tip-Namen aus dem aktuellen Layout nicht gezeigt
  19.04.04 wl  ShowTipNames              TN1788   Aufruf nur wenn Assigned(FcbTips)
  30.04.04 wl  sbCopyClick               TN1864   Variant-Array war zu klein -> Access Violation
  10.05.04 wl  FormCreate                TN1788   2-Dilutoren-Einstellungen wieder wie vorher
  14.05.04 wl                            TN1910   Das Ändern aller "Single Tip"-Checkboxen wird jetzt registriert
  17.05.04 wl                            TN1935   Das Ändern aller "Single Tip"-Checkboxen wird jetzt wirklich registriert
  17.06.04 wl                            TN1975   Controls für DispRetractSpeed entfernt
  25.08.04 wl                            TN2105   Scanspeed heißt jetzt Insertspeed, wenn LiqDet abgeschaltet ist
  13.10.04 wl                            TN2151   neuer Parameter: ZRetractDistance
  13.10.04 wl                            TN2178   neu: DisableZErr wird jetzt auch wirklich ausgeführt
  13.10.04 wl                            TN2178   Aufteilung überarbeitet
  19.10.04 wl  FormActivate              TN2185.2 Wenn User <> SystemAdmin -> alle Tab Sheets disabled!
  01.11.04 wl                            TN2198.1 benutzt GetReason aus TDataAdaptor
  12.11.04 tbh LiqParTableBeforePost     TN2219   Prüfung auf korrekten Tiptyp eingefügt
  13.12.04 wl                            TN2254.3 Farben an die Windows XP-Standardansicht angepasst
  --------------------------------------------------------------------------------------------------
  Achtung: alle Änderungen müssen auch in LiqPar.pas nachgezogen werden
  --------------------------------------------------------------------------------------------------
  31.01.05 wl                            TN2297.3 neu: DryAfterWash
  04.03.05 pk                            TN2330.1 call TAppSettings.GetReason instead of TDataAdaptor.GetReason
  09.03.05 wl  FormActivate              TN2340   zeigt jetzt ALLE Swich-Devices!!
  30.05.05 tbh cbLiqClassDispClick       TN2386   neu:
  30.05.05 tbh cbLiqClassAspClick        TN2386   neu:
  30.05.05 tbh cbLiqClassDilClick        TN2386   neu:
  30.05.05 tbh ActivateLiqClassUse       TN2386   neu: aktiviert Anzeigenteile für Liquid-Klassen
  30.05.05 tbh DeactivateLiqClassUse     TN2386   neu: deaktiviert Anzeigenteile für Liquid-Klassen
  30.05.05 tbh ReadLiqClasses            TN2386   neu: liest verfügbare Liquid-Klassen
  30.05.05 tbh IsLiqClassValid           TN2386   neu: prüft ob Liquid-Klasse existiert
  30.05.05 tbh Create                    TN2386   liest die verfügbaren Liquid-Klassen ein
  30.05.05 tbh LiqParTableBeforePost     TN2386   prüft ob Liquid-Klasse existiert
  30.05.05 tbh DBGrid1CellClick          TN2386   entfernt
  31.05.05 tbh                           TN2430   new controls for wash volume factor
  20.06.05 tbh ReadVolcorrCurves         TN2385   new: reads available volume correction curves
  20.06.05 tbh IsVolcorrCurveValid       TN2385   new: checks if volume correction curve name exists
  20.06.05 tbh Create                    TN2385   new: reads available volume correction curves
  20.06.05 tbh LiqParTableBeforePost     TN2385   checks if if volume correction curve name exists
  29.06.05 wl                            TN2444    an Änderungen von DockableForm angepasst
  01.08.05 wl                            TN2506   Bedienung in Bezug auf Liquid-Klassen geändert
  01.08.05 wl  ActivateLiqClassUse       TN2506   Spitback und Channel2-Wash-Werte sind nicht sichtbar, wenn Liquid-Klasse <>''
  31.08.05 thr (de-)ActivateLiqClassUse  TN2587   Wasch-Optionen sind nicht sichtbar, wenn Liquid-Klasse <>''
  23.09.05 pk                            TN2628   Various changes from sampler
  07.11.05 wl  LiqParTableBeforePost     TN2726   Wenn Mix-Tab nicht vorhanden, wird auch nicht geprüft (Access Violation)
  24.11.05 pk  TfrmLiquid\PowderParEditor TN2765  New
  24.11.05 pk  SetParamName              TN2765   fPowderH no longer set in this function
  11.01.06 pk                            TN2871.0 New Tab: Aspirate Sample Options
  11.01.06 pk                            TN2871.0 New fields for Extra Gap (waste gap)
  13.04.06 pk  LiqParTableBeforePost     TN3043   check if EditSysAirDispVol is assigned
  18.04.06 pk  SetParamName              TN3043   Bug: the dispense tab was freed instead of the mix tab
  30.04.06 wl                            TN3053   Neu: AspSplitMinVolPercent (mit Sprachunterstützung)
  24.05.06 wl                            TN3119   Der neue Parameter "DispEmptyVarRedi" wird angezeigt
  06.09.06 wl                            TN3256   Statt Wash macro jetzt auch Wash Method möglich
  06.09.06 wl                            TN3257   Neu: GetTip/PutTip-Methode
  23.09.06 wl  LiqParTableBeforePost     TN3326   ruft gTipManager.TipTypeExists auf - Benutzung von gTipTypes entfernt
  25.09.06 wl                            TN3326   entfernt: GetTip/PutTip-Methode
  03.10.06 wl                            TN3317   unit von PipetteParamEditor in LiquidParamEditor umbenannt
  03.10.06 wl                            TN3317   abgetrennt: PowderParamEditor
  19.12.06 wl                                TN3409   class von TDockableEditForm in TViewItemEditForm geändert
  19.01.07 wl  SetParamName                  TN3473   Swich-Device-Liste zeigt jetzt auch Namen mit * statt einer Zahl
  05.02.07 wl  PrintData                     TN3543   Print-Funktion entfernt
  27.02.07 pk  SetParamName                  TN3525   call HasPipDeviceWithMultiPumps to figure out if second channel exists
  18.04.07 wl                                TN3658   neu: SampleAspMixMinRestVol, DispMixMinRestVol
  18.04.07 wl                                TN3658   alle Mix-Parameter überarbeitet: VolPercent entfernt, neue Mix Modes
  27.04.07 wl  LiqParTableBeforePost         TN3669   GetReason wird hier nicht mehr gebraucht
  27.04.07 wl  LiqParTableBeforeDelete       TN3669   entfernt (wird seit V 7.0 nicht mehr benutzt)
  12.02.08 wl  SetParamName                  TN4009   gModules.ListAllSwitches wieder aktiviert
  11.06.08 wl                                TN4143   ScanModes werden immer gezeigt
  20.06.08 pk                                TN4139   uses changed
  03.07.08 wl                                TN4157
  25.09.08 wl                                TN4242   es gibt nur noch WashMethod, nicht mehr WashMacro
  16.01.09 wl                                TN4362   an Änderungen in TViewItem angepasst
  16.02.09 wl  TLiquidParEditorStringLoader  TN4370   neu: dezentrale Resourcen
  06.05.09 ts  LiqParTableBeforePost         TN4554   Ressourcenaufruf mit fStringLoader
  21.07.09 pk                                TN4667   new: INT_LQMODES_USE_DETECTED_VAL option
  31.07.09 wl                                TN3950   Zeigt neue Felder: SampleAspInsertMoveType, DilAspInsertMoveType, DispInsertMoveType
  31.07.09 wl                                TN4018   SingleTip ist jetzt auch ohne Liquid Detection sichtbar
  10.08.09 wl                                TN4702   Strings werden jetzt direkt geladen
  27.08.09 pk  SetParamName                  TN4753   Not working correclty some code commented out because no access to PipDevice
  28.08.09 pk                                TN4753   gPipDeviceManager removed, functions now based on DesignModuleSettingFinder
  28.08.09 pk  SetParamName                  TN4753   HasPipDeviceWithMultiPumps function still missing
  31.08.09 pk                                TN4753   uses ObjModul removed
  04.11.09 pk                                TN4843   Replace TObjectList, TStringList, TStrings with Generics Lists and TStringArray
  13.04.10 wl                                TN5044   uses geändert
  20.05.10 wl                                TN5117   neue Schriftart "Segoe UI", Schriftgröße 9
  17.06.10 wl  ShowTipNames                  TN5156   nur so viele Checkboxen wie es Tips gibt
  18.06.10 pk                                TN5152.1 TTable component removed and replaced by TDataProvider
  26.08.10 ts                                TN5248   new: MoveToSysAirPosSpeed, Special Movement setting for moving with entered speed to SysAirSuck-position
  22.09.10 wl  rgExtraGapAirPos              TN5275   new: ExtraGapAirPos
  10.12.10 wl  Create                        TN5402   StringLoader wurde zu spät erstellt
  01.06.11 ts  rbChooseTipsMouseUp           TN5593   it is now possible (again) to choose tips
  19.07.11 wl  cbTransAirRetakeAfterDisp     TN5630   Neu: Transportluftblase am Schluß wieder aufnehmen
  27.09.11 wl                                TN5698   LiqClass-Anzeige geändert
  28.09.11 wl  SetParamName                  TN5703   durch MethodDataCache erscheint das Fenster schneller
  02.12.11 wl  TLiqHandlingMixData           TN5758   Neue Felder: 'DispMixUseCalculatedVol', 'SampleAspMixUseCalculatedVol'
  23.02.12 wl                                TN5818   Neue Felder: SampleAspTipTouchScan,-Submerge,DispTipTouchScan,-Submerge = 'SampleAspTipTouchScan';
  10.05.12 wl                                TN5892   Das Feld Ch1PumpNumber wird jetzt endlich angezeigt
  10.08.12 wl  TLiqHandlingRec               TN5947   Neues Feld: WashUsePeripump
  04.09.12 wl  TLiqHandlingRec               TN5972   Neues Feld: SampleAspSpitBackAtAspPos (DilAspSpitBack entfernt)
  26.11.12 wl                                TN6027   Neue Checkboxen für TMixModes.cUseRetract
  02.01.13 wl  TLiqHandlingRec               TN6064   Neues Feld: DispMixAspSubmerge
  07.05.13 ts  TLiqHandlingRec               TN6118   Neues Feld: DispTipTouchScanStoreVol, Detektiertes Volumen kann in Posinfo gespeichert werden
  -------------------------------------------------------------------------------------------------- }

unit LiquidParamEditor;


interface


uses
    Windows,
    StdCtrls,
    Forms,
    DBCtrls,
    DB,
    Mask,
    ExtCtrls,
    ComCtrls,
    Math,
    Spin,
    Buttons,
    SysUtils,
    dialogs,
    Classes,
    Controls,
    Grids,
    DBGrids,
    CommonTypes,
    ViewItemEditForm,
    ViewItem,
    StringLoader,
    GeneralTypes,
    DataProvider;

type
    TLiquidParEditorStringLoader = class(TTextListStringLoader)
    protected
        procedure AddAllItems(); override;
    end;

    TfrmLiquidParEditor = class(TViewItemEditForm)
        DataSource1: TDataSource;
        PageControl1: TPageControl;
        tbsSampleAsp: TTabSheet;
        gbVolume: TGroupBox;
        EditSysAirAspVol: TDBEdit;
        EditTransAirVol: TDBEdit;
        GroupBox1: TGroupBox;
        EditSampleAspSpeed: TDBEdit;
        EditSysAirAspSpeed: TDBEdit;
        EditTransAirSpeed: TDBEdit;
        gbDelay: TGroupBox;
        EditSampleAspDelay: TDBEdit;
        gBCalculate: TGroupBox;
        cbSysAirAspCalc: TDBCheckBox;
        cbTransAirAspCalc: TDBCheckBox;
        DBCheckBox3: TDBCheckBox;
        rgrAspSwitchPos: TDBRadioGroup;
        dbTransAir: TDBRadioGroup;
        GroupBox2: TGroupBox;
        Label24: TLabel;
        Label26: TLabel;
        Label27: TLabel;
        GroupBox19: TGroupBox;
        tbsDispense: TTabSheet;
        GroupBox5: TGroupBox;
        DBText5: TDBText;
        EditSysAirDispVol: TDBEdit;
        GroupBox6: TGroupBox;
        DBText7: TDBText;
        DBText8: TDBText;
        EditDispSpeed: TDBEdit;
        GroupBox7: TGroupBox;
        EditDispDelay: TDBEdit;
        GroupBox10: TGroupBox;
        DBCheckBox5: TDBCheckBox;
        rgrDspSwitchPos: TDBRadioGroup;
        GroupBox17: TGroupBox;
        Label22: TLabel;
        Label23: TLabel;
        edDispStepVolume: TDBEdit;
        edDispStepDelay: TDBEdit;
        GroupBox18: TGroupBox;
        tbsMix: TTabSheet;
        GroupBox3: TGroupBox;
        LblCycles: TLabel;
        LblVolume: TLabel;
        LblSpeed: TLabel;
        GroupBox4: TGroupBox;
        edSampleAspMixCycl: TDBEdit;
        edSampleAspMixVol: TDBEdit;
        edSampleAspMixSpeed: TDBEdit;
        GroupBox8: TGroupBox;
        edDispMixCycl: TDBEdit;
        edDispMixVol: TDBEdit;
        edDispMixSpeed: TDBEdit;
        tbsSampleAspOptions: TTabSheet;
        Panel3: TPanel;
        edDescription: TDBEdit;
        Label14: TLabel;
        GroupBox9: TGroupBox;
        Label2: TLabel;
        Label3: TLabel;
        Label9: TLabel;
        Label10: TLabel;
        Label28: TLabel;
        Label30: TLabel;
        Label31: TLabel;
        edSampleAspMixDispSpeed: TDBEdit;
        edSampleAspMixZOffset: TDBEdit;
        dbSampleAspMixVolPercent: TDBEdit;
        edDispMixDispSpeed: TDBEdit;
        edDispMixZOffset: TDBEdit;
        edDispMixVolPercent: TDBEdit;
        edSysAirAspDelay: TDBEdit;
        edTransAirAspDelay: TDBEdit;
        tbsDilAsp: TTabSheet;
        GroupBox15: TGroupBox;
        Label32: TLabel;
        Label33: TLabel;
        Label35: TLabel;
        GroupBox21: TGroupBox;
        GroupBox22: TGroupBox;
        DBCheckBox10: TDBCheckBox;
        GroupBox23: TGroupBox;
        edDilAspSpeed: TDBEdit;
        GroupBox24: TGroupBox;
        edDilAspDelay: TDBEdit;
        rgrDilAspSwichPos: TDBRadioGroup;
        lblSysAirAspVol: TDBText;
        DBText2: TDBText;
        DBText3: TDBText;
        DBText4: TDBText;
        DBText9: TDBText;
        GroupBox25: TGroupBox;
        Label25: TLabel;
        gbWaste: TGroupBox;
        Label15: TLabel;
        DBCheckBox7: TDBCheckBox;
        GroupBox14: TGroupBox;
        boxDispLiqDet: TCheckBox;
        edDispSubmerge: TDBEdit;
        edDispLiqDet: TDBEdit;
        GroupBox20: TGroupBox;
        EditDilAspSubmerge: TDBEdit;
        boxDilAspLiqDet: TCheckBox;
        edDilAspLiqDet: TDBEdit;
        GroupBox11: TGroupBox;
        Label5: TLabel;
        EditSampleAspSubmerge: TDBEdit;
        boxSampleAspLiqDet: TCheckBox;
        edSampleAspLiqDet: TDBEdit;
        DBRadioGroup3: TDBRadioGroup;
        cbDAspSwitchModule: TDBComboBox;
        cbSAspSwitchModule: TDBComboBox;
        cbDispSwitchModule: TDBComboBox;
        tbsTipConfig: TTabSheet;
        grbTips: TGroupBox;
        Label1: TLabel;
        edUsedTips: TDBEdit;
        rbAllTips: TRadioButton;
        rbChooseTips: TRadioButton;
        cbUsedTipType: TDBComboBox;
        cbUseDipTipsWithoutTips: TDBCheckBox;
        gbAfterAsp: TGroupBox;
        Label18: TLabel;
        lbl1SmpAspCh2: TLabel;
        lbl2SmpAspCh2: TLabel;
        lblDispScanSpeed: TLabel;
        edDispScanSpeed: TDBEdit;
        edDilAspScanSpeed: TDBEdit;
        lblSampleAspScanSpeed: TLabel;
        edSampleAspScanSpeed: TDBEdit;
        GroupBox13: TGroupBox;
        DBcbAspSingleRetract: TDBCheckBox;
        Label37: TLabel;
        DBEdit10: TDBEdit;
        Label4: TLabel;
        DBEdit2: TDBEdit;
        Label6: TLabel;
        EditSampleAspErrFlag: TDBCheckBox;
        cbSampleAspNoCalc: TCheckBox;
        pnSampleAspRetractPos: TPanel;
        EditSampleAspRetractPos: TDBEdit;
        Label16: TLabel;
        pnSampleAspLiqDet: TPanel;
        grbSampleAspScanMode: TDBRadioGroup;
        DBCheckBox9: TDBCheckBox;
        grbSampleAspErrType: TGroupBox;
        boxSampleAspGotoZ: TRadioButton;
        boxSampleAspDispErr: TRadioButton;
        GroupBox31: TGroupBox;
        Label36: TLabel;
        Label39: TLabel;
        Label40: TLabel;
        DBEdit8: TDBEdit;
        DBCheckBox2: TDBCheckBox;
        DBEdit6: TDBEdit;
        boxDilAspLevTr: TCheckBox;
        lblDilAspScanSpeed: TLabel;
        Label41: TLabel;
        cbDilAspNoCalc: TCheckBox;
        pnDilAspLiqDet: TPanel;
        GroupBox32: TGroupBox;
        grbDilAspScanMode: TDBRadioGroup;
        boxDilAspGotoZ: TRadioButton;
        boxDilAspDispErr: TRadioButton;
        EditDilAspErrFlag: TDBCheckBox;
        Label34: TLabel;
        cbDispNoCalc: TCheckBox;
        pnDispLiqDet: TPanel;
        GroupBox33: TGroupBox;
        grbDispScanMode: TDBRadioGroup;
        boxDispGotoZ: TRadioButton;
        boxDispDispErr: TRadioButton;
        EditDispErrFlag: TDBCheckBox;
        GroupBox34: TGroupBox;
        Label13: TLabel;
        Label21: TLabel;
        Label42: TLabel;
        DBEdit12: TDBEdit;
        DBCheckBox8: TDBCheckBox;
        DBEdit3: TDBEdit;
        boxDispLevTr: TCheckBox;
        GroupBox35: TGroupBox;
        cbDilAspChannel2: TDBCheckBox;
        rbNoTracking: TRadioButton;
        rbLevelTracking: TRadioButton;
        rbRetractFromZMax: TRadioButton;
        gbLiqClassAsp: TGroupBox;
        lLiqClassAsp: TLabel;
        gbLiqClassDil: TGroupBox;
        lLiqClassDil: TLabel;
        gbLiqClassDisp: TGroupBox;
        lLiqClassDisp: TLabel;
        Label11: TLabel;
        lbLiqClassName: TDBComboBox;
        stLiqClassNameAsp: TStaticText;
        stLiqClassNameDil: TStaticText;
        stLiqClassNameDisp: TStaticText;
        gbLiqClassSpit: TGroupBox;
        lLiqClassSpit: TLabel;
        stLiqClassNameSpit: TStaticText;
        gbLiqClassWaste: TGroupBox;
        lLiqClassWaste: TLabel;
        stLiqClassWaste: TStaticText;
        GroupBox36: TGroupBox;
        DBChkDispTipTouch: TDBCheckBox;
        pnlTipTouchDisp: TPanel;
        Label47: TLabel;
        DBEdit5: TDBEdit;
        DBRadioGroup5: TDBRadioGroup;
        DBChkDispTipTouchSingle: TDBCheckBox;
        gbAfterAspVol: TGroupBox;
        edSampleAspSpitBackVol: TDBEdit;
        edSmpAspCh2Vol: TDBEdit;
        gbAfterAspCalc: TGroupBox;
        cbSampleAspSpitBackCalc: TDBCheckBox;
        cbSmpAspCh2Calc: TDBCheckBox;
        gbAfterAspSpeed: TGroupBox;
        edSmpAspCh2Speed: TDBEdit;
        edSampleAspSpitBackSpeed: TDBEdit;
        gbAfterAspDelay: TGroupBox;
        edSpitBackDelay: TDBEdit;
        gbBeforeAspVol: TGroupBox;
        edExtraGapAirVol: TDBEdit;
        edExtraGapWasteVol: TDBEdit;
        GroupBox37: TGroupBox;
        DBChkSampleAspTipTouch: TDBCheckBox;
        pnlTipTouchAsp: TPanel;
        Label45: TLabel;
        DBEdSampleAspTipTouchDelay: TDBEdit;
        DBRadioGroup4: TDBRadioGroup;
        DBCheckBox1: TDBCheckBox;
        gbBeforeAsp: TGroupBox;
        Label48: TLabel;
        Label44: TLabel;
        Label46: TLabel;
        lblSampleAspSpitBackX: TLabel;
        edSampleAspSpitBackCount: TDBEdit;
        Label49: TLabel;
        edExtraGapCount: TDBEdit;
        tbsOptions: TTabSheet;
        gbVolCorrCurve: TGroupBox;
        dcbVolCorrCurve: TDBComboBox;
        gbWash: TGroupBox;
        cbWashAfterDisp: TDBCheckBox;
        pnWashAfterDisp: TPanel;
        Label12: TLabel;
        lblWashCh2: TLabel;
        lWashVolFactor: TLabel;
        EditWashVolMax: TDBEdit;
        DBEdit1: TDBEdit;
        edWashCh2: TDBEdit;
        cbDryAfterWash: TDBCheckBox;
        edWashVolFactor: TDBEdit;
        cbWashIsForced: TDBCheckBox;
        pnWashMacro: TPanel;
        lbChooseMacro: TLabel;
        GroupBox16: TGroupBox;
        cbMultiPipett: TDBCheckBox;
        pnMultiPipett: TPanel;
        Label19: TLabel;
        Label8: TLabel;
        Label43: TLabel;
        MPwashPanel: TPanel;
        Label7: TLabel;
        Label17: TLabel;
        DBEdAspMultiMaxVol: TDBEdit;
        cbMPwash: TDBCheckBox;
        DBEdAspMultiMinVol: TDBEdit;
        DBEdAspMultiSplitVol: TDBEdit;
        gbWasteVol: TGroupBox;
        edWasteperCent: TDBEdit;
        edSampleAspWasteVol: TDBEdit;
        Label50: TLabel;
        Label51: TLabel;
        gbLiqClassWash: TGroupBox;
        lLiqClassWash: TLabel;
        stLiqClassNameWash: TStaticText;
        gbBeforeAspSpeed: TGroupBox;
        gbBeforeAspDelay: TGroupBox;
        gbBeforeAspCalc: TGroupBox;
        DBCheckBox4: TDBCheckBox;
        DBCheckBox6: TDBCheckBox;
        edExtraGapWasteSpeed: TDBEdit;
        edExtraGapAirSpeed: TDBEdit;
        dbExtraGapWasteDelay: TDBEdit;
        dbExtraGapAirDelay: TDBEdit;
        GroupBox38: TGroupBox;
        Label20: TLabel;
        Label52: TLabel;
        DBEdit4: TDBEdit;
        rgUseWashMacro: TDBRadioGroup;
        gbDispMixMethod: TGroupBox;
        cbDispMixMethod1: TCheckBox;
        cbDispMixMethod2: TCheckBox;
        edDispMixMethod: TDBEdit;
        cbDispMixMethod3: TCheckBox;
        cbDispMixMethod4: TCheckBox;
        gbSampleAspMixMethod: TGroupBox;
        cbSampleAspMixMethod1: TCheckBox;
        cbSampleAspMixMethod2: TCheckBox;
        edSampleAspMixMethod: TDBEdit;
        Label29: TLabel;
        cbUsedPipDevice: TDBComboBox;
        tdbWashMacroName: TDBComboBox;
        cbSampleAspUseDetectedLevel: TCheckBox;
        cbDispUseDetectedLevel: TCheckBox;
        rgSampleAspInsertMoveType: TDBRadioGroup;
        rgDilAspInsertMoveType: TDBRadioGroup;
        rgDispInsertMoveType: TDBRadioGroup;
        boxSampleAspLiqDetSingle: TCheckBox;
        boxDilAspLiqDetSingle: TCheckBox;
        boxDispLiqDetSingle: TCheckBox;
        GroupBox12: TGroupBox;
        edMoveToSysAirPosSpeed: TDBEdit;
        Label38: TLabel;
        Label53: TLabel;
        rgExtraGapAirPos: TDBRadioGroup;
        cbTransAirRetakeAfterDisp: TDBCheckBox;
        DBCheckBox11: TDBCheckBox;
        DBCheckBox12: TDBCheckBox;
        DBCheckBox13: TDBCheckBox;
        Label54: TLabel;
        DBEdit7: TDBEdit;
        DBChkDispTipTouchScan: TDBCheckBox;
        Label55: TLabel;
        DBEdit9: TDBEdit;
        DBCheckBox14: TDBCheckBox;
        Label56: TLabel;
        DBEdit11: TDBEdit;
        cbWashUsePeripump: TDBCheckBox;
        DBCheckBox16: TDBCheckBox;
        cbDispMixMethod5: TCheckBox;
        cbSampleAspMixMethod5: TCheckBox;
        edDispMixAspSubmerge: TDBEdit;
        Label57: TLabel;
        DBChkDispTipTouchStoreVol: TDBCheckBox;
        procedure edSampleAspLiqDetChange(Sender: TObject);
        procedure boxSampleAspLiqDetMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState;
            X, Y: Integer);
        procedure boxSampleAspGotoZMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState;
            X, Y: Integer);
        procedure boxSampleAspDispErrMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState;
            X, Y: Integer);
        procedure boxDispLiqDetMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState;
            X, Y: Integer);
        procedure rgrAspSwitchPosChange(Sender: TObject);
        procedure rgrDilAspSwichPosChange(Sender: TObject);
        procedure rgrDspSwitchPosChange(Sender: TObject);
        procedure boxSampleAspLiqDetClick(Sender: TObject);
        procedure boxDispLevTrMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState;
            X, Y: Integer);
        procedure boxDispGotoZMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState;
            X, Y: Integer);
        procedure boxDispDispErrMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState;
            X, Y: Integer);
        procedure boxDispLiqDetClick(Sender: TObject);
        procedure edDispLiqDetChange(Sender: TObject);
        procedure edDilAspLiqDetChange(Sender: TObject);
        procedure boxDilAspLiqDetClick(Sender: TObject);
        procedure boxDilAspLiqDetMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState;
            X, Y: Integer);
        procedure boxDilAspLevTrMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState;
            X, Y: Integer);
        procedure boxDilAspGotoZMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState;
            X, Y: Integer);
        procedure boxDilAspDispErrMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState;
            X, Y: Integer);
        procedure boxSampleAspLevTrMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState;
            X, Y: Integer);
        procedure edUsedTipsChange(Sender: TObject);
        procedure rbAllTipsMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
        procedure cbWashAfterDispClick(Sender: TObject);
        procedure cbMultiPipettClick(Sender: TObject);
        procedure cbWashIsForcedClick(Sender: TObject);
        procedure cbUsedTipTypeChange(Sender: TObject);
        procedure lbLiqClassNameChange(Sender: TObject);
        procedure DBChkSampleAspTipTouchClick(Sender: TObject);
        procedure DBChkDispTipTouchClick(Sender: TObject);
        procedure rgUseWashMacroClick(Sender: TObject);
        procedure edSampleAspMixMethodChange(Sender: TObject);
        procedure edDispMixMethodChange(Sender: TObject);
        procedure cbSampleAspMixMethodMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState;
            X, Y: Integer);
        procedure cbDispMixMethodMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState;
            X, Y: Integer);
        procedure cbUsedPipDeviceChange(Sender: TObject);
        procedure DataSource1DataChange(Sender: TObject; Field: TField);
        procedure DataSource1UpdateData(Sender: TObject);
        procedure rbChooseTipsMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState;
            X, Y: Integer);
        procedure DBChkDispTipTouchScanClick(Sender: TObject);
    private
        fShowScanModes: boolean;
        FcbTips: array of TCheckbox;
        fStringLoader: TLiquidParEditorStringLoader;
        fDataProvider: TDataProvider;
        procedure SampleAspLiqDetUpdate;
        procedure DispLiqDetUpdate;
        procedure DilAspLiqDetUpdate;
        procedure cbTipsMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
        procedure UpdateUsedTips;
        procedure ShowTipNames;
        procedure ActivateClassUse(aLiqClassBox, aNormalBox: TControl; aLiqClassText: TStaticText;
            aLiqClassLabel: TLabel);
        procedure ActivateLiqClassUse;
        procedure DeactivateLiqClassUse;
        procedure ReadLiqClasses;
        procedure ReadVolcorrCurves;
        function IsLiqClassValid: boolean;
        function IsVolcorrCurveValid: boolean;
    protected
        procedure SaveData(); override;
        procedure ResetData(); override;
        procedure UnloadData(); override;
        function GetDataName(): string; override;
        function CreateViewItem(const aItemName: string): TViewItem; override;
        //
        procedure SetParamName(aName: string);
    public
        constructor Create(aOwner: TComponent; const aItemName: string;
            aOnSaveStatusChanged: TNotifyEvent); override;
        destructor Destroy; override;
    end;


implementation


{$R *.DFM}

uses
    Utility2,
    SamGlobe,
    AppSettings,
    AppTypes,
    DataAdaptor,
    LiqClassDataAdaptor,
    VolcorrDataAdaptor,
    LiqHDataAdaptor,
    TipTypeDataAdaptor,
    MethodDataCache,
    SpecialViewItems,
    DeviceSettingsManager,
    IntfPipDevice,
    IntfSwitchDevice,
    DesignModuleSettingFinder,
    DialogUtils,
    LiqHTypes,
    ControlUtils,
    IntfPeriPumpDevice,
    DataProviderFactory,
    DatasetDataProvider;

{ TLiquidParEditorStringLoader }

procedure TLiquidParEditorStringLoader.AddAllItems;
begin
    AddSingle(34990, 'Empty variable powder pipette', 'Variable Pulverpipette entleeren');
    AddDouble(35000, 'Save last changes (%s)?', 'Parameter Set not saved',
        'Sollen die letzten Änderungen (%s) gespeichert werden?',
        'Die Einstellungen wurden nicht gespeichert');
    AddDouble(35010, 'DEFAULT setting can not be deleted!', 'Error',
        'Die DEFAULT-Einstellung kann nicht gelöscht werden!', 'Fehler');
    AddDouble(35020, 'Do you really want to delete setting %s?', 'Warning',
        'Soll wirklich die Einstellung %s gelöscht werden?', 'Warnung');
    AddDouble(35050, 'Dispense system air volume must not be bigger than the aspirate air volume!', 'Error',
        'Das Abgabevolumen der Systemluft darf nicht größer als das Aufnahmevolumen sein!', 'Fehler');

    AddSingle(35060, 'Setting Name:', 'Name:');
    AddSingle(35062, 'Setting Name:', 'Setting Name:');
    AddSingle(35070, 'Description:', 'Beschreibung:');
    AddSingle(35080, 'Insert', 'Einfügen');
    AddSingle(35085, 'Insert new setting', 'Neue Einstellung einfügen');
    AddSingle(35090, 'Copy', 'Kopieren');
    AddSingle(35095, 'Copy all values to new setting', 'Alle Werte in neue Einstellung kopieren');
    AddSingle(35100, 'Report', 'Bericht');
    AddSingle(35105, 'Print setting report', 'Druckerbericht für Einstellungen');
    AddSingle(35110, 'Aspirate Sample', 'Aufnahme Probe');
    AddSingle(35115, 'Aspirate Sample Options', 'Aufnahme Probe Extras');
    AddSingle(35120, 'Aspirate Diluent', 'Aufnahme Systemfl.');
    AddSingle(35125, 'Split Aspiration Volume', 'Aufnahmevolumen aufteilen');
    AddSingle(35130, 'Dispense', 'Abgabe');
    AddSingle(35135, '.. if 2nd volume <', '..wenn 2.Volumen <');
    AddSingle(35140, 'Mix', 'Mixen');
    AddSingle(35145, '% of max.vol.', '% des Maxvol.');
    AddSingle(35150, 'Moving into the well', 'Anfahren der Z-Position');
    AddSingle(35160, 'Options', 'Extras');
    AddSingle(35170, 'Wash station', 'Waschstation');
    AddSingle(35180, 'Wash after dispense', 'Nach Abgabe waschen');
    AddSingle(35190, '> Volume [µL]  x F >', '> Volumen [µL] x F >');
    AddSingle(35200, 'Additional Aspiration', 'Zur Aufnahme');
    AddSingle(35210, 'Waste', 'Abfall');
    AddSingle(35220, 'Waste [% sample vol.]', 'Abfallvol. [% Probevol.]');
    AddSingle(35230, 'Spit Back', 'Spit-Back');
    AddSingle(35232, 'Spit Back at aspiration position', 'Spit-Back an der Aspirate-Position');
    AddSingle(35250, 'Disable Z-Error!', 'Z-Error abschalten!');
    AddSingle(35260, 'Use all tips (of that type)', 'Alle Spitzen dieses Typs benutzen');
    AddSingle(35270, 'Choose Tips', 'Spitzen wählen');
    AddSingle(35280, 'Multi-Pipetting (Multi-Dispense)', 'Multi-Pipetting (Multi-Dispense)');
    AddSingle(35290, 'Enable Multi Pipetting ', 'Multi-Pipetting aktivieren');
    AddSingle(35300, 'Max. Aspiration Volume:', 'Max. Aufnahmevolumen:');
    AddSingle(35305, 'Min. Aspiration Volume:', 'Min. Aufnahmevolumen:');
    AddSingle(35310, 'Split if Volume Greater than:', 'Aufteilen wenn Vol. >');
    AddSingle(35315, 'Tip Touch', 'Tip Touch');
    AddSingle(35320, 'Delay [sec]', 'Verzögerung [s]');
    AddSingle(35325, 'Waste Gap', 'Abfall Blase');
    AddSingle(35330, 'Waste', 'Abfall');
    AddSingle(35335, 'Air', 'Luft');
    AddSingle(35340, 'with single tips', 'Einzeln detektieren');
    AddSingle(35350, 'Liquid Detection', 'Liquid Detection');
    AddSingle(35360, 'Level Tracking', 'Level Tracking');
    AddSingle(35370, 'In case of error ..', 'Im Fehlerfall ..');
    AddSingle(35380, 'Goto Z-Max', 'Gehe zu Z-Max');
    AddSingle(35390, 'Display Error', 'Fehler anzeigen');
    AddSingle(35400, 'Goto Z-Disp', 'Gehe zu Z-Disp');
    AddSingle(35410, 'No tracking', 'Kein Tracking');
    AddSingle(35420, 'Submerge [mm]:', 'Eintauchen [mm]:');
    AddSingle(35421, 'Submerge (Asp.) [mm]:', 'Eintauchen (Asp.) [mm]:');
    AddSingle(35425, 'Submerge:', 'Eintauchen:'); // für TipTouch ohne [mm]
    AddSingle(35430, 'Retract from Z-Max:', 'von Z-Max zurückziehen:');
    AddSingle(35440, 'Cycles :', 'Zyklen:');
    AddSingle(35450, 'Volume [µL]:', 'Volumen [µL]:');
    AddSingle(35455, 'Volume:', 'Volumen:');
    AddSingle(35460, 'Min. Rest Volume [µL]:', 'Min. Restvolumen [µL]:');
    AddSingle(35470, 'Mix Aspiration Speed [µL/s]:', 'Aufnahmegeschw. [µL/s]:');
    AddSingle(35480, 'Mix Dispense Speed [µL/s]:', 'Abgabegeschw. [µL/s]:');
    AddSingle(35490, 'Method :', 'Methode :');
    AddSingle(35500, 'Z-Max Offset [mm]:', 'Z-Max Offset [mm]:');
    AddSingle(35510, 'Before Sample Aspiration ', 'Vor der Aufnahme der Probe');
    AddSingle(35520, 'Method', 'Methode');
    AddSingle(35530, 'After Dispense ', 'Nach der Abgabe');
    AddSingle(35540, 'Mix only on first aspiration', 'Nur bei 1.Aufnahme mixen');
    AddSingle(35550, 'Tip Configuration', 'Spitzen-Konfiguration');
    AddSingle(35560, 'Tip Type:', 'Nadeltyp:');
    AddSingle(35570, 'System Air', 'Systemluft');
    AddSingle(35580, 'Diluent/', 'Lösemittel/');
    AddSingle(35590, 'Sample', 'Probe');
    AddSingle(35600, 'Transport Air', 'Transportluft');
    AddSingle(35610, ' Volume [µL] ', ' Volumen [µL] ');
    AddSingle(35620, 'Calculate', 'Berechnen');
    AddSingle(35630, 'Speed [µL/s] ', 'Geschw. [µL/s] ');
    AddSingle(35640, 'Delay [sec] ', 'Verzögerung [s] ');
    AddDouble(35650, 'Switch Port', 'Switch outside tube;Switch inside tube;Do not switch', 'Port schalten',
        'außerhalb des Röhrchens;innerhalb des Röhrchens;nicht schalten');
    AddSingle(35660, 'Step - Performing', 'Schrittweise Ausführung');
    AddSingle(35670, 'Aspiration Options', 'Optionen');
    AddSingle(35680, 'Single tip retract', 'Nadeln einzeln zurückziehen');
    AddDouble(35690, 'Transport Air Suck Pos.', 'Z-Travel;Z-Scan;Z-Dispense', 'Transportluft-Ansaugpos.',
        'Z-Travel;Z-Scan;Z-Dispense');
    AddDouble(35695, 'Waste Air Suck Pos.', 'Z-Travel;Z-Scan;Z-Dispense', 'Abfall-Luft-Ansaugpos.',
        'Z-Travel;Z-Scan;Z-Dispense');
    AddSingle(35710, 'Store detected volume', 'Detektiertes Volumen speichern');
    AddSingle(35720, 'Choose Setting:', 'Einstellung auswählen:');
    AddSingle(35730, 'Pipetting Parameter Settings', 'Pipettierparameter');
    AddSingle(35740, 'Sample', 'Probe');
    AddSingle(35750, 'Diluent', 'Lösemittel');
    AddSingle(35760, 'Use Cones Without Disposable Tips', 'Koni ohne Wegwerfspitzen benutzen');
    AddDouble(35770, 'System Air Suck Pos.', 'Wash Station;Z Travel;Z Scan;Z Dispense',
        'Systemluft-Ansaugpos.', 'Waschstation;Z Travel;Z Scan;Z Dispense');
    AddSingle(35780, 'Use Channel 2', 'Kanal 2 benutzen');
    AddSingle(35790, 'After Aspiration', 'Nach Aufnahme');
    AddSingle(35795, 'Before Aspiration', 'Vor Aufnahme');
    AddSingle(35800, 'Wash tips with', 'Waschen mit');
    AddSingle(35810, 'Channel 2', 'Kanal 2');
    AddSingle(35820, 'Wash With Channel 2 [µL]:', 'Waschen mit Kanal 2 [µL]:');
    AddSingle(35830, 'Force Wash Step', 'Zwingend waschen');
    AddSingle(35845, 'Wash method name:', 'Name der Wasch-Methode:');
    AddDouble(35850, 'Kind of washing', 'Default;With a wash method', 'Art des Waschens',
        'Standard;Mit einer Wasch-Methode');
    AddSingle(35860, 'Change tip type', 'Tiptyp ändern');
    AddDouble(35870, 'Scan Mode (Sensitivity)', '1;2;3;4', 'Scan Mode (Sensitivity)', '1;2;3;4');
    AddSingle(35880, 'Retract Speed:', 'Rückzugsgeschw.:');
    AddSingle(35885, 'Scan Speed:', 'Scan-Geschw.:');
    AddSingle(35890, 'Wash Retract Speed:', 'Wasch-Rückzugsgeschw.:');
    AddSingle(35895, 'Insert Speed:', 'Einführgeschw.:');
    AddSingle(35900, 'Retract distance:', 'Rückzugsstrecke:');
    AddSingle(35910, 'Moving out of well', 'Herausfahren aus Well');
    AddSingle(35920, 'Dispense Options', 'Optionen');
    AddSingle(35930, 'Do not use level calculation', 'Keine Füllstandberechnug');
    AddSingle(35940, 'Dry tips after washing', 'Nadeln nach Waschen trocknen');
    AddSingle(35944, 'Wash with Peri-Pump', 'Mit Peri-Pumpe waschen');
    AddSingle(35950, 'Use Class', 'Klasse');
    AddSingle(35960, 'Liquid Class', 'Flüssigkeitsklasse');
    AddSingle(35970, 'Class:', 'Klasse:');
    AddSingle(35980, 'F (wash volume factor):', 'F (Waschvolumenfaktor):');
    AddSingle(35985, 'Volume correction curve', 'Volumenkorrektur-Kurve');
    AddSingle(35990, 'Liquid Handling', 'Liquid Handling');
    AddSingle(35995, 'Powder Handling', 'Pulververteilung');
    AddSingle(36000, 'Tip type [%s] is not defined!', 'Tiptyp [%s] is nicht definiert!');
    AddSingle(36010, 'Liquid class [%s] is not defined!', 'Flüssigkeitsklasse [%s] is nicht definiert!');
    AddSingle(36020, 'Volume correction curve [%s] is not defined!',
        'Volumenkorrektur-Kurve [%s] ist nicht definiert!');
    AddSingle(36030, 'Aspiration', 'Aufnahme');
    AddSingle(36035, 'Aspiration', 'Aufnahme');
    AddSingle(36040, 'Number of powder aspirations:', 'Anzahl der Pulveraufnahmen:');
    AddSingle(36045, 'XY position shifting', 'XY-Position variieren');
    AddSingle(36050, 'Between aspirations:', 'Zwischen den Aufnahmen:');
    AddSingle(36055, 'After last aspiration:', 'Nach der letzten Aufnahme:');
    AddSingle(36060, 'Dispense at Z-Max - Z-Offset', 'Abgabe bei Z-Max - Z-Offset');
    AddSingle(36070, 'Mix with air', 'Mit Luft mixen');
    AddSingle(36071, 'Use Z retract speed', 'Z-Rückzugsgeschw. verwenden');
    AddSingle(36080, 'Tracking', 'Tracking');
    AddSingle(36090, 'Liquid detection', 'Liquid Detection');
    AddSingle(36100, 'Shake Time [sec]:', 'Schüttelzeit [sec]:');
    AddDouble(36110, 'Kind of z-movement', 'Default;Block 1;Block 2;Block 3', 'Art der Z-Bewegung',
        'Standard;Block 1;Block 2;Block 3');
    AddSingle(36120, 'Use detected level', 'Detektierte Höhe verwenden');
    AddSingle(36130, 'Move to System Air Suck', 'Systemluft-Ansaugposition');
    AddSingle(36140, 'Position Speed:', 'Anfahrgeschw.:');
    AddSingle(36150, 'Special Movement', 'Spezialbewegung');
    AddSingle(36151, 'Retake Transport air between multi disps', 'Transportluft zw. Multi-Disp. aufziehen');
    AddSingle(36152, 'Retake Transport air after (last) dispense',
        'Transportluft nach Disp. wieder aufziehen');
    AddSingle(36160, 'Use calculated volume', 'Berechentes Volumen verwenden');
    AddSingle(36170, 'Pump number:', 'Pumpen-Nummer:');
    AddSingle(36180, 'Store detected vol', 'Volumen speichern');
end;

{ TfrmLiquidParEditor }

constructor TfrmLiquidParEditor.Create(aOwner: TComponent; const aItemName: string;
    aOnSaveStatusChanged: TNotifyEvent);
begin
    inherited Create(aOwner, aItemName, aOnSaveStatusChanged);

    TControlUtils.ResetFontForWinXP(self);
    fStringLoader := TLiquidParEditorStringLoader.Create;
    fStringLoader.LoadLanguage(self);

    fDataProvider := TDataProviderFactory.Instance.CreateDataProvider();
    fDataProvider.SelectAndOpen('SELECT * FROM LIQPARAM WHERE PARAMName = ''' + fViewItem.Name + '''', false);
    ASSERT(fDataProvider is TDatasetDataProvider);
    self.DataSource1.DataSet := (fDataProvider as TDatasetDataProvider).GetRealDataset;
    self.DataSource1.OnDataChange := self.DataSource1DataChange;
    self.DataSource1.OnUpdateData := self.DataSource1UpdateData;

    ReadLiqClasses;
    ReadVolcorrCurves;
    SetLength(fcbTips, 0);
    SetParamName(fViewItem.Name);
    fShowScanModes := true; // sollte eventuell in allgemeinen Settings festgelegt werden!
end;

destructor TfrmLiquidParEditor.Destroy;
begin

    self.DataSource1.DataSet := nil;
    FreeAndNil(fDataProvider);
    fStringLoader.Free;
    inherited;
end;

function TfrmLiquidParEditor.CreateViewItem(const aItemName: string): TViewItem;
begin
    result := TLiquidParViewItem.Create(aItemName);
end;

procedure TfrmLiquidParEditor.SetParamName(aName: string);
var
    i: integer;
    xNames: TStringArray;
    x: integer;
begin

    { TODO -oPK -cgPipDeviceManager : HasPipDeviceWithMultiPumps no longer accessible here }
    // keine zusätzlichen Dilutoren (Kanal 2)
    {
      if not gPipDeviceManager.HasPipDeviceWithMultiPumps then begin
      cbDilAspChannel2.Visible:=False;
      edSmpAspCh2Vol.Visible:=false;
      edSmpAspCh2Speed.Visible:=false;
      cbSmpAspCh2Calc.Visible:=false;
      lbl1SmpAspCh2.Visible:=false;
      lbl2SmpAspCh2.Visible:=false;
      lblWashCh2.Visible:=false;
      edWashCh2.Visible:=false;
      edSpitBackDelay.Top := edSampleAspSpitBackSpeed.Top;
      end;
    }
    Caption := GetCaption();

    pnMultiPipett.visible := cbMultiPipett.Checked;
    rgUseWashMacroClick(nil);
    boxSampleAspLiqDetClick(nil);
    boxDilAspLiqDetClick(nil);
    boxDispLiqDetClick(nil);
    DBChkSampleAspTipTouchClick(nil);
    DBChkDispTipTouchClick(nil);
    DBChkDispTipTouchScanClick(nil);

    pnSampleAspRetractPos.Visible := rbRetractFromZMax.Checked;
    // --------------------------------------------------------------------------------- Tips löschen
    for i := 0 to high(FcbTips) do
        if (FcbTips[i] <> nil) then
            FcbTips[i].Free;
    // -------------------------------------------------------------------------- CheckBoxen erzeugen
    rbChooseTips.Caption := fStringLoader.GetResString(35270 { Choose Tips } );

    { TODO -owl : Muss geändert werden }

    SetLength(fcbTips, MAX_TIPS);
    for i := 0 to MAX_TIPS - 1 do
    begin
        FcbTips[i] := TCheckBox.Create(grbTips);
        FcbTips[i].Parent := grbTips;
        FcbTips[i].Left := 30;
        FcbTips[i].Width := 180;
        FcbTips[i].Top := rbChooseTips.Top + 20 + (i * 18);
        FcbTips[i].OnMouseUp := cbTipsMouseUp;
    end;
    if (gChangeTipTypes) then
    begin
        rbAllTips.Checked := false;
        rbChooseTips.Checked := true;
        rbAllTips.Visible := false;
        rbAllTips.Enabled := false;
    end;

    ShowTipNames;

    if (lbLiqClassName.Field.AsString <> '') then
        ActivateLiqClassUse
    else
        DeactivateLiqClassUse;

    xNames := TDesignModuleSettingFinder.ListAllSwitches();
    for x := 0 to Length(xNames) - 1 do
        cbSAspSwitchModule.Items.Add(xNames[x]);

    xNames := TDesignModuleSettingFinder.ListAllSwitches();
    for x := 0 to Length(xNames) - 1 do
        cbDAspSwitchModule.Items.Add(xNames[x]);

    xNames := TDesignModuleSettingFinder.ListAllSwitches();
    for x := 0 to Length(xNames) - 1 do
        cbDispSwitchModule.Items.Add(xNames[x]);

    // user restrictions:
    DataSource1.AutoEdit := false;

    // all tab sheets disabled
    tbsSampleAsp.Enabled := false;
    tbsDilAsp.Enabled := false;
    tbsDispense.Enabled := false;
    tbsMix.Enabled := false;
    tbsSampleAspOptions.Enabled := false;
    tbsOptions.Enabled := false;
    tbsTipConfig.Enabled := false;

    if gCommonDll.CurrentUser.HasLevel(usrSystemAdmin) then
    begin
        DataSource1.AutoEdit := true;

        // all tab sheets enabled
        tbsSampleAsp.Enabled := true;
        tbsDilAsp.Enabled := true;
        tbsDispense.Enabled := true;
        tbsMix.Enabled := true;
        tbsSampleAspOptions.Enabled := true;
        tbsOptions.Enabled := true;
        tbsTipConfig.Enabled := true;
    end;

    // unterschiedliche Fenstergestaltung für Liquid oder Powder Handling
    xNames := TTipTypeDataAdaptor.InstGetLiquidTipTypeNames();
    for x := 0 to Length(xNames) - 1 do
        cbUsedTipType.Items.Add(xNames[x]);

    xNames := TDesignModuleSettingFinder.GetPipDeviceNames();
    for x := 0 to Length(xNames) - 1 do
        cbUsedPipDevice.Items.Add(xNames[x]);

    xNames := TMethodDataCache.Instance.GetMethodNames();
    for x := 0 to Length(xNames) - 1 do
        tdbWashMacroName.Items.Add(xNames[x]);

    cbWashUsePeripump.Visible := TDesignModuleSettingFinder.DeviceExists(IPeriPumpDevice);
end;

// ==================================================================================================
// Used Tips
// --------------------------------------------------------------------------------------------------
procedure TfrmLiquidParEditor.ShowTipNames;
var
    xNoOfTips, xMap, i: Integer;
begin
    xNoOfTips := TDesignModuleSettingFinder.ReadTipCount(self.cbUsedPipDevice.Text);
    gmStrToIntTry(xMap, edUsedTips.Text);
    if (xMap = 0) then
        rbAllTips.Checked := true
    else
        rbChooseTips.Checked := true;

    for i := 0 to high(fcbTips) do
    begin

        if not Assigned(FcbTips[i]) then
            break;

        FcbTips[i].Enabled := (xMap <> 0);
        FcbTips[i].Checked := (((XMap shr i) and 1) = 1);
        FcbTips[i].Caption := 'Tip ' + IntToStr(i + 1);
        FcbTips[i].Visible := i < xNoOfTips;

        // if (gChangeTipTypes) and (FcbTips[i].Checked) then
        // FcbTips[i].Caption := FcbTips[i].Caption + ': ' + cbUsedTipType.Text;// .Field.AsString;

        // if (not gChangeTipTypes) then
        // FcbTips[i].Caption := FcbTips[i].Caption + ': ' + gPipDeviceManager.Tips[i].TypeName
    end;
end;

procedure TfrmLiquidParEditor.edUsedTipsChange(Sender: TObject);
begin
    if not self.DataSource1.DataSet.Active then
        EXIT;

    ShowTipNames;
end;

// --------------------------------------------------------------------------------------------------
procedure TfrmLiquidParEditor.cbTipsMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState;
    X, Y: Integer);
// --------------------------------------------------------------------------------------------------
begin
    UpdateUsedTips;
end;

// --------------------------------------------------------------------------------------------------
procedure TfrmLiquidParEditor.rbAllTipsMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState;
    X, Y: Integer);
// --------------------------------------------------------------------------------------------------
var
    i: integer;
begin
    for i := 0 to high(fcbTips) do
        if (FcbTips[i] <> nil) then
        begin
            FcbTips[i].Enabled := false;
            FcbTips[i].Checked := false;
        end;
    UpdateUsedTips;
end;

// --------------------------------------------------------------------------------------------------
procedure TfrmLiquidParEditor.rbChooseTipsMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState;
    X, Y: Integer);
// --------------------------------------------------------------------------------------------------
var
    i: integer;
begin
    for i := 0 to high(fcbTips) do
        if (FcbTips[i] <> nil) then
            FcbTips[i].Enabled := true;
    UpdateUsedTips;
end;

// --------------------------------------------------------------------------------------------------
procedure TfrmLiquidParEditor.UpdateUsedTips;
// --------------------------------------------------------------------------------------------------
var
    i, Oldmap, Map: integer;
begin
    Map := 0;
    for i := 0 to high(fcbTips) do
        if (FcbTips[i] <> nil) then
        begin
            if (FcbTips[i].Checked) then
                Map := Map + Round(Power(2, i));
        end;
    gmStrToIntTry(OldMap, edUsedTips.Text);
    if (OldMap <> Map) then
    begin
        if Datasource1.Dataset.Active then
            Datasource1.Dataset.Edit;

        edUsedTips.Text := IntToStr(Map);
    end;
end;

// ==================================================================================================
// Liquid Detection: Sample Aspiration
// --------------------------------------------------------------------------------------------------
procedure TfrmLiquidParEditor.edSampleAspLiqDetChange(Sender: TObject);
// --------------------------------------------------------------------------------------------------
var
    xMap: Integer;
begin
    gmStrToIntTry(xMap, edSampleAspLiqDet.Text);
    boxSampleAspLiqDet.Checked := (xMap and INT_LQMODES_LQ_SCAN) <> 0;
    boxSampleAspDispErr.Checked := (xMap and INT_LQMODES_LQ_DISP_ERROR) <> 0;
    boxSampleAspGotoZ.Checked := (xMap and INT_LQMODES_LQ_ERROR_GOTO) <> 0;

    rbNoTracking.Checked := (xMap and INT_LQMODES_TRACKING) = 0;
    rbLevelTracking.Checked := (xMap and INT_LQMODES_TRACKING) <> 0;

    boxSampleAspLiqDetSingle.Checked := (xMap and INT_LQMODES_LQ_SINGLE_TIP) <> 0;
    cbSampleAspNoCalc.Checked := (xMap and INT_LQMODES_NO_CALCULATION) <> 0;
    cbSampleAspUseDetectedLevel.Checked := (xMap and INT_LQMODES_USE_DETECTED_VAL) <> 0;
    if (Datasource1.Dataset.FieldByName('AspirationMethodRetractZ').AsBoolean) then
        rbRetractFromZMax.Checked := true;
end;

// --------------------------------------------------------------------------------------------------
procedure TfrmLiquidParEditor.SampleAspLiqDetUpdate;
// --------------------------------------------------------------------------------------------------
var
    xMap, xTextMap: integer;
begin
    xMap := 0;
    if boxSampleAspLiqDet.Checked then
        xMap := xMap + INT_LQMODES_LQ_SCAN;
    if boxSampleAspDispErr.Checked then
        xMap := xMap + INT_LQMODES_LQ_DISP_ERROR;
    if boxSampleAspGotoZ.Checked then
        xMap := xMap + INT_LQMODES_LQ_ERROR_GOTO;
    if rbLevelTracking.Checked then
        xMap := xMap + INT_LQMODES_TRACKING;
    if boxSampleAspLiqDetSingle.Checked then
        xMap := xMap + INT_LQMODES_LQ_SINGLE_TIP;
    if cbSampleAspNoCalc.Checked then
        xMap := xMap + INT_LQMODES_NO_CALCULATION;
    if cbSampleAspUseDetectedLevel.Checked then
        xMap := xMap + INT_LQMODES_USE_DETECTED_VAL;

    if (Datasource1.Dataset.FieldByName('AspirationMethodRetractZ').AsBoolean <>
        rbRetractFromZMax.Checked) then
    begin
        Datasource1.Dataset.Edit;
        Datasource1.Dataset.FieldByName('AspirationMethodRetractZ').AsBoolean := rbRetractFromZMax.Checked;
        pnSampleAspRetractPos.Visible := rbRetractFromZMax.checked;
    end;

    gmStrToIntTry(xTextMap, edSampleAspLiqDet.Text);
    if (xTextMap <> xMap) then
    begin
        if Datasource1.Dataset.Active then
            Datasource1.Dataset.Edit;
        edSampleAspLiqDet.Text := IntToStr(xMap);
    end;
end;

// --------------------------------------------------------------------------------------------------
procedure TfrmLiquidParEditor.boxSampleAspLiqDetClick(Sender: TObject);
// --------------------------------------------------------------------------------------------------
begin
    pnSampleAspLiqDet.Visible := boxSampleAspLiqDet.Checked;
    rgSampleAspInsertMoveType.Visible := not boxSampleAspLiqDet.Checked;
    if (boxSampleAspLiqDet.checked) then
        lblSampleAspScanSpeed.Caption := fStringLoader.GetResString(35885 { Scan Speed: } )
    else
        lblSampleAspScanSpeed.Caption := fStringLoader.GetResString(35895 { Insert Speed: } );
    grbSampleAspScanMode.Visible := fShowScanModes;
end;

// --------------------------------------------------------------------------------------------------
procedure TfrmLiquidParEditor.boxSampleAspLiqDetMouseUp(Sender: TObject; Button: TMouseButton;
    Shift: TShiftState; X, Y: Integer);
// --------------------------------------------------------------------------------------------------
begin
    SampleAspLiqDetUpdate;
end;

// --------------------------------------------------------------------------------------------------
procedure TfrmLiquidParEditor.boxSampleAspLevTrMouseUp(Sender: TObject; Button: TMouseButton;
    Shift: TShiftState; X, Y: Integer);
// --------------------------------------------------------------------------------------------------
begin
    SampleAspLiqDetUpdate;
end;

// --------------------------------------------------------------------------------------------------
procedure TfrmLiquidParEditor.boxSampleAspGotoZMouseUp(Sender: TObject; Button: TMouseButton;
    Shift: TShiftState; X, Y: Integer);
// --------------------------------------------------------------------------------------------------
begin
    SampleAspLiqDetUpdate;
end;

// --------------------------------------------------------------------------------------------------
procedure TfrmLiquidParEditor.boxSampleAspDispErrMouseUp(Sender: TObject; Button: TMouseButton;
    Shift: TShiftState; X, Y: Integer);
// --------------------------------------------------------------------------------------------------
begin
    SampleAspLiqDetUpdate;
end;

// ==================================================================================================
// Liquid Detection: Dispense
// --------------------------------------------------------------------------------------------------
procedure TfrmLiquidParEditor.edDispLiqDetChange(Sender: TObject);
// --------------------------------------------------------------------------------------------------
var
    xMap: integer;
begin
    gmStrToIntTry(xMap, edDispLiqDet.Text);
    boxDispLiqDet.Checked := (xMap and INT_LQMODES_LQ_SCAN) <> 0;
    boxDispDispErr.Checked := (xMap and INT_LQMODES_LQ_DISP_ERROR) <> 0;
    boxDispGotoZ.Checked := (xMap and INT_LQMODES_LQ_ERROR_GOTO) <> 0;
    boxDispLevTr.Checked := (xMap and INT_LQMODES_TRACKING) <> 0;
    boxDispLiqDetSingle.Checked := (xMap and INT_LQMODES_LQ_SINGLE_TIP) <> 0;
    cbDispNoCalc.Checked := (xMap and INT_LQMODES_NO_CALCULATION) <> 0;
    cbDispUseDetectedLevel.Checked := (xMap and INT_LQMODES_USE_DETECTED_VAL) <> 0;
end;

// --------------------------------------------------------------------------------------------------
procedure TfrmLiquidParEditor.DispLiqDetUpdate;
// --------------------------------------------------------------------------------------------------
var
    xMap, xTextMap: integer;
begin
    xMap := 0;
    if boxDispLiqDet.Checked then
        xMap := xMap + INT_LQMODES_LQ_SCAN;
    if boxDispDispErr.Checked then
        xMap := xMap + INT_LQMODES_LQ_DISP_ERROR;
    if boxDispGotoZ.Checked then
        xMap := xMap + INT_LQMODES_LQ_ERROR_GOTO;
    if boxDispLevTr.Checked then
        xMap := xMap + INT_LQMODES_TRACKING;
    if boxDispLiqDetSingle.Checked then
        xMap := xMap + INT_LQMODES_LQ_SINGLE_TIP;
    if cbDispNoCalc.Checked then
        xMap := xMap + INT_LQMODES_NO_CALCULATION;
    if cbDispUseDetectedLevel.Checked then
        xMap := xMap + INT_LQMODES_USE_DETECTED_VAL;

    gmStrToIntTry(xTextMap, edDispLiqDet.Text);
    if (xTextMap <> xMap) then
    begin
        if Datasource1.Dataset.Active then
            Datasource1.Dataset.Edit;
        edDispLiqDet.Text := inttostr(xMap);
    end;
end;

// --------------------------------------------------------------------------------------------------
procedure TfrmLiquidParEditor.boxDispLiqDetClick(Sender: TObject);
// --------------------------------------------------------------------------------------------------
begin
    pnDispLiqDet.Visible := boxDispLiqDet.Checked;
    rgDispInsertMoveType.Visible := not boxDispLiqDet.Checked;
    if (boxDispLiqDet.checked) then
        lblDispScanSpeed.Caption := fStringLoader.GetResString(35885 { Scan Speed: } )
    else
        lblDispScanSpeed.Caption := fStringLoader.GetResString(35895 { Insert Speed: } );
    grbDispScanMode.Visible := fShowScanModes;
end;

// --------------------------------------------------------------------------------------------------
procedure TfrmLiquidParEditor.boxDispLiqDetMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState;
    X, Y: Integer);
// --------------------------------------------------------------------------------------------------
begin
    DispLiqDetUpdate;
end;

// --------------------------------------------------------------------------------------------------
procedure TfrmLiquidParEditor.boxDispLevTrMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState;
    X, Y: Integer);
// --------------------------------------------------------------------------------------------------
begin
    DispLiqDetUpdate;
end;

// --------------------------------------------------------------------------------------------------
procedure TfrmLiquidParEditor.boxDispGotoZMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState;
    X, Y: Integer);
// --------------------------------------------------------------------------------------------------
begin
    DispLiqDetUpdate;
end;

// --------------------------------------------------------------------------------------------------
procedure TfrmLiquidParEditor.boxDispDispErrMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState;
    X, Y: Integer);
// --------------------------------------------------------------------------------------------------
begin
    DispLiqDetUpdate;
end;

// ==================================================================================================
// Liquid Detection: Diluent Aspiration
// --------------------------------------------------------------------------------------------------
procedure TfrmLiquidParEditor.edDilAspLiqDetChange(Sender: TObject);
// --------------------------------------------------------------------------------------------------
var
    xMap: integer;
begin
    gmStrToIntTry(xMap, edDilAspLiqDet.Text);
    boxDilAspLiqDet.Checked := (xMap and INT_LQMODES_LQ_SCAN) <> 0;
    boxDilAspDispErr.Checked := (xMap and INT_LQMODES_LQ_DISP_ERROR) <> 0;
    boxDilAspGotoZ.Checked := (xMap and INT_LQMODES_LQ_ERROR_GOTO) <> 0;
    boxDilAspLevTr.Checked := (xMap and INT_LQMODES_TRACKING) <> 0;
    boxDilAspLiqDetSingle.Checked := (xMap and INT_LQMODES_LQ_SINGLE_TIP) <> 0;
    cbDilAspNoCalc.Checked := (xMap and INT_LQMODES_NO_CALCULATION) <> 0;
end;

// --------------------------------------------------------------------------------------------------
procedure TfrmLiquidParEditor.DilAspLiqDetUpdate;
// --------------------------------------------------------------------------------------------------
var
    xMap, xTextMap: integer;
begin
    xMap := 0;
    if boxDilAspLiqDet.Checked then
        xMap := xMap + INT_LQMODES_LQ_SCAN;
    if boxDilAspDispErr.Checked then
        xMap := xMap + INT_LQMODES_LQ_DISP_ERROR;
    if boxDilAspGotoZ.Checked then
        xMap := xMap + INT_LQMODES_LQ_ERROR_GOTO;
    if boxDilAspLevTr.Checked then
        xMap := xMap + INT_LQMODES_TRACKING;
    if boxDilAspLiqDetSingle.Checked then
        xMap := xMap + INT_LQMODES_LQ_SINGLE_TIP;
    if cbDilAspNoCalc.Checked then
        xMap := xMap + INT_LQMODES_NO_CALCULATION;

    gmStrToIntTry(xTextMap, edDilAspLiqDet.Text);
    if (xTextMap <> xMap) then
    begin
        if Datasource1.Dataset.Active then
            Datasource1.Dataset.Edit;
        edDilAspLiqDet.Text := inttostr(xMap);
    end;
end;

// --------------------------------------------------------------------------------------------------
procedure TfrmLiquidParEditor.boxDilAspLiqDetClick(Sender: TObject);
// --------------------------------------------------------------------------------------------------
begin
    pnDilAspLiqDet.Visible := boxDilAspLiqDet.Checked;
    rgDilAspInsertMoveType.Visible := not boxDilAspLiqDet.Checked;
    if (boxDilAspLiqDet.checked) then
        lblDilAspScanSpeed.Caption := fStringLoader.GetResString(35885 { Scan Speed: } )
    else
        lblDilAspScanSpeed.Caption := fStringLoader.GetResString(35895 { Insert Speed: } );
    grbDilAspScanMode.Visible := fShowScanModes;
end;

// --------------------------------------------------------------------------------------------------
procedure TfrmLiquidParEditor.boxDilAspLiqDetMouseUp(Sender: TObject; Button: TMouseButton;
    Shift: TShiftState; X, Y: Integer);
// --------------------------------------------------------------------------------------------------
begin
    DilAspLiqDetUpdate;
end;

// --------------------------------------------------------------------------------------------------
procedure TfrmLiquidParEditor.boxDilAspLevTrMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState;
    X, Y: Integer);
// --------------------------------------------------------------------------------------------------
begin
    DilAspLiqDetUpdate;
end;

// --------------------------------------------------------------------------------------------------
procedure TfrmLiquidParEditor.boxDilAspGotoZMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState;
    X, Y: Integer);
// --------------------------------------------------------------------------------------------------
begin
    DilAspLiqDetUpdate;
end;

// --------------------------------------------------------------------------------------------------
procedure TfrmLiquidParEditor.boxDilAspDispErrMouseUp(Sender: TObject; Button: TMouseButton;
    Shift: TShiftState; X, Y: Integer);
// --------------------------------------------------------------------------------------------------
begin
    DilAspLiqDetUpdate;
end;

// ==================================================================================================
// Switch Position
// --------------------------------------------------------------------------------------------------
procedure TfrmLiquidParEditor.rgrAspSwitchPosChange(Sender: TObject);
// --------------------------------------------------------------------------------------------------
begin
    if (rgrAspSwitchPos.Value = '0') then
        cbSAspSwitchModule.visible := false
    else
        cbSAspSwitchModule.visible := true;
end;

// --------------------------------------------------------------------------------------------------
procedure TfrmLiquidParEditor.rgrDilAspSwichPosChange(Sender: TObject);
// --------------------------------------------------------------------------------------------------
begin
    if (rgrDilAspSwichPos.Value = '0') then
        cbDAspSwitchModule.visible := false
    else
        cbDAspSwitchModule.visible := true;
end;

// --------------------------------------------------------------------------------------------------
procedure TfrmLiquidParEditor.rgrDspSwitchPosChange(Sender: TObject);
// --------------------------------------------------------------------------------------------------
begin
    if (rgrDspSwitchPos.Value = '0') then
        cbDispSwitchModule.visible := false
    else
        cbDispSwitchModule.visible := true;
end;

// --------------------------------------------------------------------------------------------------
procedure TfrmLiquidParEditor.cbWashAfterDispClick(Sender: TObject);
// --------------------------------------------------------------------------------------------------
begin
    pnWashMacro.Visible := false;
    pnWashAfterDisp.Visible := cbWashAfterDisp.Checked;
    cbWashIsForced.Visible := cbWashAfterDisp.Checked;
    rgUseWashMacro.Visible := cbWashAfterDisp.Checked;
    if not(cbWashAfterDisp.Checked) then
    begin
        cbWashIsForced.Checked := false;
        pnWashAfterDisp.Visible := false;
    end;
    rgUseWashMacroClick(Sender);
end;

// --------------------------------------------------------------------------------------------------
procedure TfrmLiquidParEditor.cbMultiPipettClick(Sender: TObject);
// --------------------------------------------------------------------------------------------------
begin
    pnMultiPipett.visible := cbMultiPipett.Checked;
end;

// --------------------------------------------------------------------------------------------------
procedure TfrmLiquidParEditor.cbWashIsForcedClick(Sender: TObject);
// --------------------------------------------------------------------------------------------------
begin
    if cbWashIsForced.Checked then
    begin
        cbWashAfterDisp.Checked := true;
        rgUseWashMacro.Visible := true;
    end;
end;

procedure TfrmLiquidParEditor.rgUseWashMacroClick(Sender: TObject);
begin
    if (rgUseWashMacro.ItemIndex = 0) then
    begin
        pnWashAfterDisp.Visible := cbWashAfterDisp.Checked;
        pnWashMacro.Visible := false;
    end
    else
    begin
        pnWashAfterDisp.Visible := false;
        pnWashMacro.Visible := cbWashAfterDisp.Checked;
        lbChooseMacro.Caption := fStringLoader.GetResString(35845 { Enter method name: } );
    end;
end;

// --------------------------------------------------------------------------------------------------
procedure TfrmLiquidParEditor.cbUsedPipDeviceChange(Sender: TObject);
begin
    ShowTipNames;
end;

procedure TfrmLiquidParEditor.cbUsedTipTypeChange(Sender: TObject);
// --------------------------------------------------------------------------------------------------
begin
    ShowTipNames;
end;

procedure TfrmLiquidParEditor.lbLiqClassNameChange(Sender: TObject);
begin
    if (lbLiqClassName.Text <> '') then
        ActivateLiqClassUse
    else
        DeactivateLiqClassUse;
end;

// --------------------------------------------------------------------------------------------------
procedure TfrmLiquidParEditor.SaveData;
// --------------------------------------------------------------------------------------------------
begin
    if (Datasource1.Dataset.State in [dsEdit]) then
        Datasource1.Dataset.Post;
end;

// --------------------------------------------------------------------------------------------------
procedure TfrmLiquidParEditor.ResetData;
// --------------------------------------------------------------------------------------------------
begin
    if (Datasource1.Dataset.State in [dsEdit]) then
        Datasource1.Dataset.Cancel;
end;

// --------------------------------------------------------------------------------------------------
procedure TfrmLiquidParEditor.UnloadData;
// --------------------------------------------------------------------------------------------------
begin
    self.DataSource1.OnDataChange := nil;
    self.DataSource1.OnUpdateData := nil;
    self.DataSource1.DataSet.Close();
end;

function TfrmLiquidParEditor.GetDataName(): string;
begin
    result := Datasource1.Dataset.FieldByName('PARAMNAME').AsString;
end;

procedure TfrmLiquidParEditor.ActivateClassUse(aLiqClassBox, aNormalBox: TControl; aLiqClassText: TStaticText;
    aLiqClassLabel: TLabel);
begin
    aLiqClassBox.BoundsRect := aNormalBox.BoundsRect;

    aLiqClassText.Caption := lbLiqClassName.Text;
    aLiqClassText.Left := Trunc((aLiqClassBox.Width - aLiqClassText.Width) / 2);
    aLiqClassText.Top := Trunc(1 / 2 * aLiqClassBox.Height);

    aLiqClassLabel.Left := aLiqClassText.Left;
    aLiqClassLabel.Top := Trunc(1 / 4 * aLiqClassBox.Height);

    aLiqClassBox.Visible := true;
    aNormalBox.Visible := false;

end;

// --------------------------------------------------------------------------------------------------
procedure TfrmLiquidParEditor.ActivateLiqClassUse;
// --------------------------------------------------------------------------------------------------
begin
    // --------------------------------------- Tab Aspirate
    gbLiqClassAsp.Left := gbVolume.Left;
    gbLiqClassAsp.Top := gbVolume.Top;
    gbLiqClassAsp.Height := gbVolume.Height;
    gbLiqClassAsp.Width := gbDelay.Width + gbDelay.Left - gbVolume.Left;

    stLiqClassNameAsp.Caption := lbLiqClassName.Text;
    stLiqClassNameAsp.Left := Trunc((gbLiqClassAsp.Width - stLiqClassNameAsp.Width) / 2);
    stLiqClassNameAsp.Top := Trunc(1 / 2 * gbLiqClassAsp.Height);

    lLiqClassAsp.Left := stLiqClassNameAsp.Left;
    lLiqClassAsp.Top := Trunc(1 / 4 * gbLiqClassAsp.Height);

    gbVolume.Visible := false;
    gbCalculate.Visible := false;
    GroupBox1.Visible := false;
    gbDelay.Visible := false;

    gbLiqClassAsp.Visible := true;

    // --------------------------------------- Tab Diluent
    gbLiqClassDil.Left := GroupBox21.Left;
    gbLiqClassDil.Top := GroupBox21.Top;
    gbLiqClassDil.Height := GroupBox21.Height;
    gbLiqClassDil.Width := GroupBox24.Width + GroupBox24.Left - GroupBox21.Left;

    stLiqClassNameDil.Caption := lbLiqClassName.Text;
    stLiqClassNameDil.Left := Trunc((gbLiqClassDil.Width - stLiqClassNameDil.Width) / 2);
    stLiqClassNameDil.Top := Trunc(1 / 2 * gbLiqClassDil.Height);

    lLiqClassDil.Left := stLiqClassNameDil.Left;
    lLiqClassDil.Top := Trunc(1 / 4 * gbLiqClassDil.Height);

    GroupBox21.Visible := false;
    GroupBox22.Visible := false;
    GroupBox23.Visible := false;
    GroupBox24.Visible := false;

    gbLiqClassDil.Visible := true;

    // --------------------------------------- Tab Dispense
    gbLiqClassDisp.Left := GroupBox5.Left;
    gbLiqClassDisp.Top := GroupBox5.Top;
    gbLiqClassDisp.Height := GroupBox5.Height;
    gbLiqClassDisp.Width := GroupBox7.Width + GroupBox7.Left - GroupBox5.Left;

    stLiqClassNameDisp.Caption := lbLiqClassName.Text;
    stLiqClassNameDisp.Left := Trunc((gbLiqClassDisp.Width - stLiqClassNameDisp.Width) / 2);
    stLiqClassNameDisp.Top := Trunc(1 / 2 * gbLiqClassDisp.Height);

    lLiqClassDisp.Left := stLiqClassNameDisp.Left;
    lLiqClassDisp.Top := Trunc(1 / 4 * gbLiqClassDisp.Height);

    GroupBox5.Visible := false;
    GroupBox10.Visible := false;
    GroupBox6.Visible := false;
    GroupBox7.Visible := false;

    gbLiqClassDisp.Visible := true;

    // --------------------------------------- Tab SpitBack
    gbLiqClassSpit.Left := gbAfterAspVol.Left;
    gbLiqClassSpit.Top := gbAfterAspVol.Top;
    gbLiqClassSpit.Height := gbAfterAspVol.Height;
    gbLiqClassSpit.Width := gbAfterAspDelay.Width + gbAfterAspDelay.Left - gbAfterAspVol.Left;

    stLiqClassNameSpit.Caption := lbLiqClassName.Text;
    stLiqClassNameSpit.Left := Trunc((gbLiqClassSpit.Width - stLiqClassNameSpit.Width) / 2);
    stLiqClassNameSpit.Top := Trunc(1 / 2 * gbLiqClassSpit.Height);

    lLiqClassSpit.Left := stLiqClassNameSpit.Left;
    lLiqClassSpit.Top := Trunc(1 / 4 * gbLiqClassSpit.Height);

    gbLiqClassSpit.Visible := true;

    gbAfterAspVol.Visible := false;
    gbAfterAspCalc.Visible := false;
    gbAfterAspSpeed.Visible := false;
    gbAfterAspDelay.Visible := false;

    // --------------------------------------- Wash
    ActivateClassUse(gbLiqClassWash, gbWash, stLiqClassNameWash, lLiqClassWash);

    // --------------------------------------- Tab Waste
    ActivateClassUse(gbLiqClassWaste, gbWasteVol, stLiqClassWaste, lLiqClassWaste);
end;

// --------------------------------------------------------------------------------------------------
procedure TfrmLiquidParEditor.DeactivateLiqClassUse;
// --------------------------------------------------------------------------------------------------
begin
    // --------------------------------------- Tab Aspirate
    gbLiqClassAsp.Visible := false;

    gbVolume.Visible := true;
    gbCalculate.Visible := true;
    GroupBox1.Visible := true;
    gbDelay.Visible := true;

    // --------------------------------------- Tab Diluent
    gbLiqClassDil.Visible := false;

    GroupBox21.Visible := true;
    GroupBox22.Visible := true;
    GroupBox23.Visible := true;
    GroupBox24.Visible := true;

    // --------------------------------------- Tab Dispense
    gbLiqClassDisp.Visible := false;

    GroupBox5.Visible := True;
    GroupBox10.Visible := True;
    GroupBox6.Visible := True;
    GroupBox7.Visible := True;

    // --------------------------------------- SpitBack
    gbLiqClassSpit.Visible := false;

    gbAfterAspVol.Visible := True;
    gbAfterAspCalc.Visible := True;
    gbAfterAspSpeed.Visible := True;
    gbAfterAspDelay.Visible := True;

    // --------------------------------------- Wash
    gbLiqClassWash.Visible := false;
    gbWash.Visible := True;

    // --------------------------------------- Waste
    gbLiqClassWaste.Visible := false;
    gbWasteVol.Visible := true;

end;

// --------------------------------------------------------------------------------------------------
procedure TfrmLiquidParEditor.ReadLiqClasses;
// --------------------------------------------------------------------------------------------------
var
    xLiqClassDataAdaptor: TLiqClassDataAdaptor;
    xNames: TStringArray;
    x: integer;
begin
    xLiqClassDataAdaptor := TLiqClassDataAdaptor.Create;
    try
        xNames := xLiqClassDataAdaptor.ReadAllNames();
        for x := 0 to Length(xNames) - 1 do
            lbLiqClassName.Items.Add(xNames[x]);
    finally
        FreeAndNil(xLiqClassDataAdaptor);
    end;
end;

// --------------------------------------------------------------------------------------------------
function TfrmLiquidParEditor.IsLiqClassValid: boolean;
// --------------------------------------------------------------------------------------------------
var
    xCnt: integer;
begin
    result := false;
    for xCnt := 0 to lbLiqClassName.Items.Count - 1 do
        if (lbLiqClassName.Items.Strings[xCnt] = Datasource1.Dataset.FieldByName('LIQCLASS').AsString) then
            result := true;
end;

// --------------------------------------------------------------------------------------------------
procedure TfrmLiquidParEditor.ReadVolcorrCurves;
// --------------------------------------------------------------------------------------------------
var
    xVolcorrDataAdaptor: TVolcorrDataAdaptor;
    xNames: TStringArray;
    x: integer;
begin
    xVolcorrDataAdaptor := TVolcorrDataAdaptor.Instance;
    xNames := xVolcorrDataAdaptor.ReadCurveNames;
    dcbVolcorrCurve.Items.Clear;
    dcbVolcorrCurve.Items.Add('');
    for x := 0 to Length(xNames) - 1 do
        dcbVolcorrCurve.Items.Add(xNames[x]);

end;

// --------------------------------------------------------------------------------------------------
function TfrmLiquidParEditor.IsVolcorrCurveValid: boolean;
// --------------------------------------------------------------------------------------------------
var
    xCnt: integer;
begin
    result := false;
    for xCnt := 0 to dcbVolcorrCurve.Items.Count - 1 do
        if (dcbVolcorrCurve.Items.Strings[xCnt] = Datasource1.Dataset.FieldByName('VolCorrCurve')
            .AsString) then
            result := true;
end;

procedure TfrmLiquidParEditor.DBChkSampleAspTipTouchClick(Sender: TObject);
begin
    self.pnlTipTouchAsp.Visible := DBChkSampleAspTipTouch.Checked;
end;

procedure TfrmLiquidParEditor.DataSource1DataChange(Sender: TObject; Field: TField);
begin
    self.ChangeData;
end;

procedure TfrmLiquidParEditor.DataSource1UpdateData(Sender: TObject);
var
    AspVol, DispVol: integer;
    xTipTypeData: TTipType;
begin
    // ------------------------------------------ Prüfen, ob DispSysAir > AspSysAir
    gmStrToIntTry(DispVol, EditSysAirDispVol.Text);
    gmStrToIntTry(AspVol, EditSysAirAspVol.Text);
    if (DispVol > AspVol) then
    begin
        fStringLoader.ResMsgBox(35050 { Disp.system air vol. must not be bigger than asp.air vol.! } ,
            MB_ICONSTOP);
        abort;
    end;
    // ------------------------------------------ Prüfen, ob Tiptyp existiert
    if not TTipTypeDataAdaptor.TipTypeExists(Datasource1.Dataset.FieldByName('UsedTipType').AsString,
        xTipTypeData) then
    begin
        fStringLoader.ResFMsgBox(36000 { Tip type [%s] is not defined! } , MB_ICONSTOP,
            [Datasource1.Dataset.FieldByName('UsedTipType').AsString]);
        abort;
    end;
    // ------------------------------------------ Prüfen, ob Liquidklasse existiert
    if (Datasource1.Dataset.FieldByName('LIQCLASS').AsString <> '') and not(IsLiqClassValid) then
    begin
        fStringLoader.ResFMsgBox(36010 { Liquid class [%s] is not defined! } , MB_ICONSTOP,
            [Datasource1.Dataset.FieldByName('LIQCLASS').AsString]);
        abort;
    end;
    // ------------------------------------------ Prüfen, ob Volumenkorrekturkurve existiert
    if (Datasource1.Dataset.FieldByName('LIQCLASS').AsString <> '') and not(IsVolCorrCurveValid) then
    begin
        fStringLoader.ResFMsgBox(36020 { Volume correction curve [%s] is not defined! } , MB_ICONSTOP,
            [Datasource1.Dataset.FieldByName('LIQCLASS').AsString]);
        abort;
    end;
end;

procedure TfrmLiquidParEditor.DBChkDispTipTouchClick(Sender: TObject);
begin
    self.pnlTipTouchDisp.Visible := DBChkDispTipTouch.Checked;
end;

procedure TfrmLiquidParEditor.DBChkDispTipTouchScanClick(Sender: TObject);
begin
    DBChkDispTipTouchStoreVol.Enabled := DBChkDispTipTouchScan.Checked;
end;

procedure TfrmLiquidParEditor.edSampleAspMixMethodChange(Sender: TObject);
var
    xMap: integer;
begin
    gmStrToIntTry(xMap, edSampleAspMixMethod.Text);

    cbSampleAspMixMethod1.Checked := (xMap and TMixModes.cDispenseAtZOffset) <> 0;
    cbSampleAspMixMethod2.Checked := (xMap and TMixModes.cMixWithAir) <> 0;
    cbSampleAspMixMethod5.Checked := (xMap and TMixModes.cUseRetract) <> 0;
end;

procedure TfrmLiquidParEditor.edDispMixMethodChange(Sender: TObject);
var
    xMap: integer;
begin
    gmStrToIntTry(xMap, edDispMixMethod.Text);

    cbDispMixMethod1.Checked := (xMap and TMixModes.cDispenseAtZOffset) <> 0;
    cbDispMixMethod2.Checked := (xMap and TMixModes.cMixWithAir) <> 0;
    cbDispMixMethod3.Checked := (xMap and TMixModes.cAspirateTracking) <> 0;
    cbDispMixMethod4.Checked := (xMap and TMixModes.cAspirateWithScan) <> 0;
    cbDispMixMethod5.Checked := (xMap and TMixModes.cUseRetract) <> 0;
end;

procedure TfrmLiquidParEditor.cbSampleAspMixMethodMouseUp(Sender: TObject; Button: TMouseButton;
    Shift: TShiftState; X, Y: Integer);
var
    xMap, xTextMap: integer;
begin
    xMap := 0;
    if (cbSampleAspMixMethod1.Checked) then
        xMap := xMap + TMixModes.cDispenseAtZOffset;
    if (cbSampleAspMixMethod2.Checked) then
        xMap := xMap + TMixModes.cMixWithAir;
    if (cbSampleAspMixMethod5.Checked) then
        xMap := xMap + TMixModes.cUseRetract;

    gmStrToIntTry(xTextMap, edSampleAspMixMethod.Text);
    if (xTextMap <> xMap) then
    begin
        if Datasource1.Dataset.Active then
            Datasource1.Dataset.Edit;
        edSampleAspMixMethod.Text := IntToStr(xMap);
    end;
end;

procedure TfrmLiquidParEditor.cbDispMixMethodMouseUp(Sender: TObject; Button: TMouseButton;
    Shift: TShiftState; X, Y: Integer);
var
    xMap, xTextMap: integer;
begin
    xMap := 0;
    if (cbDispMixMethod1.Checked) then
        xMap := xMap + TMixModes.cDispenseAtZOffset;
    if (cbDispMixMethod2.Checked) then
        xMap := xMap + TMixModes.cMixWithAir;
    if (cbDispMixMethod3.Checked) then
        xMap := xMap + TMixModes.cAspirateTracking;
    if (cbDispMixMethod4.Checked) then
        xMap := xMap + TMixModes.cAspirateWithScan;
    if (cbDispMixMethod5.Checked) then
        xMap := xMap + TMixModes.cUseRetract;

    gmStrToIntTry(xTextMap, edDispMixMethod.Text);
    if (xTextMap <> xMap) then
    begin
        if Datasource1.Dataset.Active then
            Datasource1.Dataset.Edit;
        edDispMixMethod.Text := IntToStr(xMap);
    end;
end;


end.
