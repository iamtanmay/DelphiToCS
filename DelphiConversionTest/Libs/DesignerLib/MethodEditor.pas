{ ------------------------------------------------------------------------------------
  Methodeneditor
  ------------------------------------------------------------------------------------
  Version  Datum     op  function/procedure     Änderung / Neuerung
  -------  --------  --  -------------------    --------------------------------------
  07.10.97  mo  Saveas1Click           Kopieren einer Methode
  10.10.97  mo  Copy1Click             Kopieren von records
  Funktioniert auch Methodenübergreifend
  11.10.97  mo  Delete1Click           Löschen mehrerer records
  11.10.97  mo  SpeedBtnPrintClick     Ausdruck einer Methode
  18.11.97  mo  CheckBox ShowRun       wenn checked wird rundb angezeigt
  19.11.97  mo  BitBtn1Click           Method.post eingeführt - zur Überprüfung eventueller fehler
  DBGrid1 Spaltenbreite fest in CreateForm gesetzt
  20.11.97  mo  BitBtn1Click           Method.post nur wenn insert oder edit
  8.12.97   dl+HC FormCreate           Array von 6WayVale umgestellt (1..6)
  15.12.97  mo  BitBtn1Click           Aufruf der RunDBAnzeige über TShowRunForm.Create(Nil);
  06.01.98  mo  Form Create            Lösungsmittel in PickList laden : Index -1
  07.08.98  mo  ShowRunForm.Create     Neuer Parameter wird übergeben
  17.02.98  mo  DelBlank(..)           neue Funktion
  Saveas1Click()         Prüfung auf Leerstring bei save as
  01.04.98  wl  FormCreate             Aufruf von ReadLiqParDB
  ReadLiqParDB           Erzeugen einer PickList für LiquidParameters  (unvollendet)
  FormResize             Funktion meiner Meinung nach überflüssig -> gelöscht
  FormClose              Nachfrage, ob Änderungen gespeichert werden sollen,
  erscheint jetzt immer wenn das Fenster verlassen wird
  BitBtn2                2.Close-Button mitsamt Click-Methode gelöscht (s.o.)
  02.04.98  wl  LiquidParameters1Click Menüpunkt zum Aufruf des Liquid-Parameter-Fenster
  05.05.98  wl                         Menü-Bezeichnungen 'Method' und 'Tools'
  03.06.98  wl                            MultiCopy deaktiviert (wg. Fehler)
  =Build 980603
  MultiCopy aktiviert (Fehler in Unit DM_Sampl behoben)
  =Build 980603a
  10.06.98 wl  LiquidParameters1Click     Aufrufe von LiqPar in SamLiqPar geändert
  DBGrid1DblClick             "
  16.06.98 wl  LiquidParameters1Click     Fenstereigenschaften von SamLiqPar in SamLiqH verschoben
  DBGrid1DblClick             "
  23.06.98 wl                             uses SamWash
  01.07.98 wl  DataSourceMethodDataChange jetzt für MoveRack oder ReadBarcode
  09.07.98 wl  btnBuildClick              Aufruf von gmRunCreate mit Methode
  10.07.98 wl  btnBuildClick              Aufruf von ShowRunForm mit neuen Parameter
  SpeedBtnPrintClick         Aufruf mit Methodenname
  mnuMethodDeleteClick         dito
  13.07.98 wl  FormClose                  PMethodName --> medMethodName
  FormCreate,DBMETHODBeforePost,SpeedBtnPrintClick,DBNavigator1Click,Saveas1Click,
  btnBuildClick,mnuMethodDeleteClick       dito
  Saveas1Click               für neue Methode: ChangeMethodName
  15.07.98 wl  FormClose                  DataSourceMethod.DataSet:= nil
  FormCreate                 DataSourceMethod.DataSet:= dmSampler.DBMETHOD
  btnBuildClick              neuer Aufruf von ShowRun
  21.07.98 wl                             uses Utility
  DataSourceMethodDataChange 'ReadE' hinzugefügt
  30.07.98 wl  FormClose                  DeleteLayout hin
  mnuMethodDeleteClick       entfernt
  MainForm.PLayoutName statt medLayoutname
  02.09.98 wl                             gSeqInc (globale Variable) statt SeqInc (public)
  03.09.98 wl  FormClose                  MethRCpyForm wird nut bei Delete-Mode geschlossen
  16.09.98 wl                             gSeqInc --> globals
  alle Funktionen            global.LayoutName/global.PMethodName  statt MainForm. ...
  Saveas1Click               MainForm.ChangeMethodName -> global.ChangeGlobalName
  FormCreate                 Layout.Workbench -> WB
  FormClose,DBNavigator1Click  LoadLayout -> WB.Load
  DataSourceMethodDataChange Aufrufe von R und RackA durch WB.R und WB.RackA ersetzt
  21.09.98 wl  Saveas1Click               bei Cancel kein Exit (Fehlerquelle) sondern normales Beenden der Funktion
  22.09.98 wl  FormCreate                 SpinEdit1 := gSeqInc (nicht umgekehrt)
  25.09.98 wl  FormCreate                 MethEditFormOpen gelöscht
  28.09.98 wl  btnBuildClick              ruft global.RunCreate auf
  FormClose,DBNavigator1Click  kein Löschen des Layouts mehr
  29.09.98 wl  Saveas1Click               Aufruf von global.ChangeGlobalName mir Reload=false
  05.10.98 wl  FormClose                  Abfrage auf ShowRunForm statt ShowRunFormOpen
  02.11.98 wl  DataSourceMethodDataChange SetSlotWithoutDBChange: Layout.DB wird nicht mehr geändert
  06.11.98 wl                             eigenes Menü abgeschafft
  FormCreate,FormClose       änder Einträge im Main-Menü
  10.11.98 wl  DataSourceMethodDataChange ruft gmGetSlotStruct auf
  02.02.99 mo                             "Save AS" Button Hint Korrigiert
  17.02.99 wl  DataSourceMethodDataChange Rackbewegungen --> DM_Sampl
  DBNavigator1Click          WB.Reload --> DM_sampl
  08.03.99 wl  DBNavigator1Click          ResetAllRacks, wenn auf Taste "1.Datensatz" geklickt wird
  FormClose                  ResetAllRacks statt WB.Load
  08.06.99 wl  FormCreate                 Diluents in PickList laden mit gLiquid-properties
  11.08.99 wz                           --> Strings in Ressourcen
  12.08.99 wl                             neu: CoolBar; Felder für Default Values endgültig raus
  12.10.99 wl  sbSaveAsClick              Abfrage nach Speichern vor dem Kopieren (führte zu Fehler)
  11.11.99 mo  sbExcelOle                 neu Datenübergabe nach Excel
  19.11.99 mo  sbExcelOle                 überarbeitet für mehrsprachige Nutzung
  26.11.99 mo  sbExcelOle                 Datenrückgabe aus Excel
  17.12.99 mo  sbSaveAsClick              Prüft richtig ob die Methode schon vorhanden ist
  17.12.99 mo  sbExcelOle                 Es werden keine führenden Nullen im Excel SEQ Feld mehr eingetragen
  17.12.99 mo  sbExcelOle                 Feldnummern in Method wieder wie in Version 4.x
  23.12.99 wl  FormCreate                 keine Dummy-Liquids in die Pick-List laden
  26.05.00 wl                         uses geändert
  05.06.00 wl                             gSeqInc -> gMethSeqInc
  31.08.00 wl  FormCreate,FormClose       Fenster-Position und -Format werden geladen und gespeichert
  19.01.01 mo  DataSourceMethodDataChange Rote Farbe für Kolonnen deaktiviert
  21.09.01 mo  FormCreate                 TN1060 : wenn MethEditActionOnly=1 wird nur Seq,Action,Option wird in Methedit angezeigt
  30.11.01 mo  sbEXcelOle                 TN1106,TN1105 gmOpenExcelApplication wird aufgerufen
  13.09.02 wl                             TN1283 Merge mit SIAS
  18.10.02 wl  FormCreate                 TN1293.1 Actions und Options werden anders gelesen
  31.01.03 wl  FormCreate                 TN1334.1 User-Rechte werden direkt abgefragt
  11.02.03 wl                             TN1345 XAP-Compiler-Direktiven entfernt
  11.02.03 wl  FormCreate                 TN1334.1 sbExcelGet nur für UnlimitedAdmin
  13.02.03 wl  DBGrid1DblClick            TN1334.3 nur wenn User = SystemAdmin
  12.03.03 wl                             TN1293.5 uses info entfernt
  30.10.03 wl  sbSaveAsClick              TN1641   wird ersetzt durch ScheUtil.gmMethodSaveAs
  30.10.03 wl  InitItems                  TN1641   aus LisGlobe.InitProject hierher verschoben
  30.10.03 wl  ChangeMethod               TN1641   neu: damit Methode von außerhalb geändert werden kann (Editor)
  28.01.04 wl  FExcelMan                  TN1414   wird bei FormCreate erzeugt, bei FormClose zerstört
  28.01.04 wl  sbExcelOle                 TN1414   benutzt TExcelOleManager-Methoden
  06.02.04 wl                             TN1414   uses ExcelOleManager
  26.02.04 wl  ShowDependentLiqHandlingForm TN1574 ersetzt die komplette unit SamLiqH.pas, modaler Aufruf von LiqPar
  26.03.04 pk  FormDestroy                TN1836   FExcelMan.Free moved from FormClose to FormDestroy
  27.04.04 pk                             TN1880  New Import Button
  02.07.04 wl  FormCreate                 TN2017   Lädt die Spaltenbreiten aus appini.tmp
  02.07.04 wl  FormClose                  TN2017   Speichert die Spaltenbreiten in appini.tmp
  21.07.04 pk                             TN2049   New fields : Priority, SchedMin, SchedMax
  21.07.04 pk  FormCreate                 TN2049   Hide scheduler fields if the proper settings aren't found
  28.07.04 pk  ChangeMethod               TN2059   Shut off virtual rack move and set it back to its previous state when done
  05.08.04 wl  TMethEditForm              TN2008.1  FormStyle = fsNormal
  06.08.04 wl  TMDIMethEditForm.SetFormStyle  TN2008.1  Form is created with FormStyle = fsMDIChild
  18.08.04 wl  AddAction                  TN2008.1  Dummy-Funktion (wird nur von Editor überschrieben)
  18.08.04 wl                             TN2085   Buttons mit neuen Icons und ohne Schrift (wegen der Breite)
  19.08.04 wl  sbSaveAs                   TN1780   Der SaveAs-Buttons ist wieder da - zusammen mit Print-Button
  27.08.04 pk                             TN2085   Resource for Import Wizard Button changed to 52330
  19.10.04 wl  FormCreate                 TN2185.2 sbSaveAs und sbImport enabled nur für System Admin
  24.11.04 wl  UpdateProps                TN2213   nur für ZADesign.exe: kann Datensatz von außen ändern
  24.11.04 wl  FormShow                   TN2213   nur für ZADesign.exe: Update auf Properties-Fenster
  02.12.04 wl  sbChangeLayout             TN2254.2  neu: ruft global.ChangeLayout auf und definiert neues Layout
  09.12.04 wl                             TN2246.4  benutzt global.CreateRunForm
  21.12.04 pk  DBGrid1CellClick           TN2271    a bug in the DBGrid component prevents the grid from receiving focus
  21.12.04 wl  InitItems                  TN2247.1  Uses MethodGUIParsing to load the "Favourite" actions
  22.12.04 wl                             TN2246.5  Änderungen der Oberfläche für XP-Optik
  13.04.05 wl  SaveData                   TN2248.8  Speichern sollte jetzt endlich richtig funktionieren
  22.04.05 wl  SaveData                   TN2248.8  Seq wird immer um 10 hochgezählt
  06.06.05 wl                             TN2440    Umstellung auf TcxGrid !
  16.06.05 wl  ChangeValuesBeforePost     TN2467    entspricht der gleichnamigen Funktion in DM_Sampl
  16.06.05 wl  EditCopy,EditPaste         TN2438    kopiert entweder den Text in einem Feld oder alle selektierten Spalten
  17.06.05 wl  cbViewTypesChange          TN2440    kann verschiedene hard-codierte Ansichten laden
  17.06.05 wl  sbViewAutoSize             TN2440    passt die Spaltenbreite den Daten an
  17.06.05 wl  sbExcelGetClick            TN2439.1  funktioniert genau wie beim alten Methoden-Editor
  17.06.05 wl  sbExcelPutClick            TN2439.1  funktioniert genau wie beim alten Methoden-Editor
  20.06.05 wl  edCommentLong/Short        TN2441    Zum Editieren Methoden-bezogener Kommentare
  21.06.05 wl                             TN2440    Anordnung der Spalten ist jetzt veränderbar
  21.06.05 wl                             TN2440    Umbenennung MethodDesign.pas in MethodEditor.pas
  22.06.05 wl  mnuDeleteColumnClick       TN2440    löscht die aktuelle Kolonne
  22.06.05 wl  mnuSelectFieldClick        TN2440    öffnet Select field-dialog
  24.06.05 wl                             TN2441    Verbesserungen bei Copy/Paste und DragDrop
  29.06.05 wl  Find/ReplaceTextInMethod   TN2444    neue Funktionen zum Suchen/Ersetzen
  30.06.05 wl  SelectAllData              TN2495    Selektiert alle Reihen
  06.07.05 wl                             TN2495    Excel,Build,Layout,Import-Buttons -> ZADesignMain
  06.07.05 wl  ExcelPut/Get               TN2495    neue Funktionalität für Methoden und Commend-Makro
  06.07.05 wl  CheckLastChange            TN2496    durch das Versetzen des Focus wird auch die aktuelle Änderung immer mit übernommen
  07.07.05 wl  MoveSelectedRecords        TN2495    Verschieben von Zeilen implementiert
  07.07.05 wl  EditCut                    TN2495    Fehler beseitigt: Falsche Zeilen wurden gelöscht
  13.07.05 wl  mnuSaveCurrentViewClick    TN2440.1  speichert die aktuelle Spaltenanordnung
  13.07.05 wl  mnuDeleteTableViewClick    TN2440.1  löscht die Spaltenanordnung, die gerade in cbTableView angezeigt wird
  14.07.05 wl  edCommentChange            TN2502    die Kommentare werden jetzt sofort gespeichert
  14.07.05 wl  UpdateMethodComments       TN2502    die Kommentare werden aktualisiert, wenn sie in ActionList geändert werden
  01.08.05 wl  GetRealActionName          TN2503    IsPipetteOrCalliOrManualFillAction ersetzt ungenaue Abfrage IsPipetteAction
  01.08.05 wl  ReplaceTextInMethod        TN2444    nach dem Ersetzen wird Save-Button gesetzt
  02.08.05 wl  cxGrid1TableView1DataControllerAfterDelete  TN2502    nach dem Löschen einer Zeile wird Save-button gesetzt
  10.08.05 wl                             TN2501.1  EdExplorer in ViewAllItems umbenannt
  22.08.05 wl                             TN2558.8  uses ScheUtil entfernt, ScheUtil-Methoden werden mit TDMScript. oder global. aufgerufen
  31.08.05 wl  GetComments                TN2541.0  --> MethodStepManager
  31.08.05 wl  ActionNameChanged          TN2541.0  Teile nach MethodStepManager.GetComments verschoben
  07.09.05 wl  StartMethod                TN2585    benutzt neuen DDE-Befehl "ZAStartMeth"
  12.09.05 wl  SetRepositoryItems         TN2591    Für jedes Feld der Method.db gibt es ein Repository-Item
  13.09.05 wl  ChangeValuesBeforePost     TN2598    Changes needed for ASP and DSP actions
  03.10.05 wl                             TN2642    Property Visible = false
  03.10.05 wl  GetCurrentLiqParName       TN2439    neu: zum Öffnen des richtigen PipetteParamEditors
  05.10.05 wl  sbChangeCommentsClick      TN2575    neuer Button zum Ändern der Kommentare
  05.10.05 wl  UpdateCommentsAndSubmethodComments  TN2584  ändert Kommentare zu Untermethoden, wenn diese geändert wurden
  07.10.05 wl  BuildMethod()              TN2637.1  BuildWizardPage wird gezeigt, zunächst der Run-Status geprüft
  10.10.05 wl  BuildMethod()              TN2637.2  über DDE wird geprüft, ob die Methode gerade läuft
  20.10.05 wl  AddAction                  TN2659    an neue Actions angepasst
  21.10.05 wl  StartMethod                TN2687    --> ZADesignMain
  21.10.05 wl                             TN2682    Buttons in neuer Anordnung
  07.11.05 pk  Start                      TN2737    Start a method via DDE
  14.11.05 pk  ShowBuildResult            TN2759    New : overrides inherited function and shows a run table
  14.11.05 pk  Build                      TN2759    Instead of BuildMethod. overrides inherited
  14.11.05 pk  fLiqHDataAdaptor           TN2720    New : used in CheckMethodStepBeforePost
  15.11.05 pk  ChangeValuesBeforePost     TN2761    Do not save dest vol for manual fill
  16.11.05 pk  EditAction                 TN2751    Replaces EditCopy, Edit...
  18.11.05 pk  Build                      TN2680    Syntax check removed
  24.11.05 pk  fLayoutName, LoadLayout    TN2767    Load layout when SelectEditor is called
  24.11.05 pk                             TN2805    references to global removed
  05.12.05 pk  MoveSelectedRecords        TN2832    ClearSelection if the user cancels the move
  05.12.05 pk  cxGrid1TableView1MouseDown TN2832    Turn on drag and drop only if first column is selected
  05.12.05 pk  cxGrid1TableView1          TN2832    OptionsBehavior.pullfocusing := true - Allow multiselect by dragging
  06.12.05 pk  cxGrid1TableView1CellDblClick TN2840 Open submethod for ADDM,RUN, etc
  07.12.05 wl  cxGrid1TableView1EditValueChanged  TN2839  Wenn Actionname geändert wird, bleiben Kommentare erhalten
  20.01.06 pk  UpdateProperty             TN2891    Removed. Replaced by MethodStepParam_OnWriteData Event
  20.01.06 pk  DefineCurrentMethodStep    TN2891    triggers MethodStepParam_OnWriteData Event for each field
  25.01.06 pk  cxGrid1TableView1CellDblClick TN2897 Find subparam TMethodStepParameter_MethodParams
  25.01.06 pk  CheckCurrentStep           TN2898    Call frmActionProps StepChanged
  29.03.06 wl  CheckRecord                TN2970    CheckMethodStepBeforePost komplett entfernt
  29.03.06 wl  ChangeValuesBeforePost     TN3002    bei WAIT action darf es einen SourceRack-Eintrag geben
  06.04.06 pk                             TN3024    New field SchedSharedID
  10.04.06 pk                             TN3031    Load and Save value of Use Standard Init
  10.04.06 pk                             TN3032    New field Iterate
  12.04.06 pk  MoveSelectedRecords        TN3040    Check if Focusedrecord is assigned
  18.04.06 wl                             TN3025    Inactive komplett entfernt
  15.05.06 pk  Build                      TN3097    Build has extra param
  19.05.06 wl  LoadMethodData             TN3109    ruft UpdateMethodAttributes auf
  19.05.06 wl  GetMethodEditorAttribute   TN3109    gibt Default, ReadOnly oder Hidden zurück (wenn HiddenMethodsAreShown -> Default)
  19.05.06 wl  WriteAttributeText         TN3109    das Attribut wird in stAttribute geschrieben
  28.06.06 wl  EditExcelPut                     TN3172   benutzt nicht mehr die Method.db
  28.06.06 wl  EditExcelGet                     TN3172   Excel-Import wird nicht mehr sofort gespeichert!
  21.07.06 pk  cxGrid1TableView1CellDblClick    TN3213   Use TSubMethodMethodStep cast to get method name
  21.07.06 pk  DefineCurrentMethodStep          TN3213   use new currentstep.readdata function
  26.07.06 pk  EditSetFocusToTextPart           TN3220   selects the text at the given pos
  26.07.06 pk  FindOrReplaceTextInMethod        TN3220   Can now find more than one instances of sustr in text
  26.07.06 pk  FindOrReplaceTextInMethod        TN3220   calls EditSetFocusToTextPart to highlight the substr
  27.07.06 pk  DoFindTextInMethod               TN3221   New: code taken from old FindOrReplaceTextInMethod
  27.07.06 pk  DoReplaceTextInMethod            TN3221   New: code taken from old FindOrReplaceTextInMethod
  28.07.06 pk                                   TN3221   Select replaced text before doing next find
  28.07.06 pk  SelectEditor                     TN3222   Setfocus to grid, Select the focusedrecord or first record, recheck popupmenu
  03.08.06 pk  DoReplaceTextInMethod            TN3221   We only replace the text if the text we want to find is highlighted
  08.08.06 wl                                   TN3254   Wenn ReadOnly gesetzt ist, kann gescrollt und es können Zeilen markiert werden
  18.08.06 wl  cxGrid1TableView1DragOver,-Drop  TN3257   andere Actions und Methoden können nicht mehr in schreibgeschützte Methoden gezogen werden
  06.09.06 wl                                   TN3287   Liquid Handling Parameter für MASSP wird jetzt auch gelöscht
  12.09.06 wl  ChangeValuesBeforePost           TN3284   nichts tun bei REMARK-Action
  13.09.06 pk  ChangeValuesBeforePost           TN3283.2 Delete unneeded info when nogroup is selected
  13.09.06 pk  ChangeValuesBeforePost           TN3283.1 Delete priority if not schedulable
  13.09.06 pk  SaveData                         TN3283.1 Do NOT set priority to default
  19.09.06 pk  Build                            TN3228   CheckBeforeBuild now in TGlobals
  02.10.06 wl  MethodStepParam_OnGetMaxLength   TN3236   Funktion, die mit TMethodEditorDataAdaptor.GetMaxFieldLength die Feldlänge bestimmt
  24.11.06 wl  Create                           TN3432    wenn GetZARunnerConnection = false, ist Start-Button disabled
  24.11.06 wl                                   TN2692   Save/Start/ExcelGet-Button sind auch sichtbar, wenn disabled
  27.11.06 wl  PrintData                        TN3411   Funktion muß zunächst entfernt werden
  28.11.06 wl  CanClose                         TN3434    wenn self.IsDeleted: keine Nachfrage
  28.11.06 wl  UnloadData                       TN3434    wenn self.IsDeleted: Tabellen-Settings nicht speichern
  19.12.06 wl                                   TN3409   class von TDockableEditForm in TViewItemEditForm geändert
  19.12.06 wl  GetMethodEditorAttribute         TN3409   --> ViewItems
  19.12.06 wl  viele Methoden                   TN3409    User management überarbeitet
  19.12.06 wl                                   TN3409    Buttons mit neuen Disabled-Bitmaps
  21.12.06 wl  UnloadData                       TN3496    Das Properties-Fenster wird geleert und es bleiben keine alten Angaben stehen
  16.01.07 pk                                   TN3481   Load and Save AutoSeq methodsetting
  18.01.07 pk                                   TN3482   Changes for ADSEQ action
  16.04.07 wl  cxGrid1TableView1CellDblClick    TN3547   bei Doppelklick wird jetzt auch SQL-Editor geöffnet
  07.05.07 wl  UpdateMethodAttributes           TN3669   Bei Read-Only-Attribut sind beide Checkboxen disabled
  24.07.07 wl  AddWashProg                      TN3792   Waschprogramme werden jetzt als WASHP Action angefügt
  25.07.07 pk  cxGrid1TableView1MouseDown       TN3772   call checksingleline
  25.07.07 pk  ChangeValuesBeforePost           TN3772   if dispensing SYSTEM set srack and spos to empty
  26.07.07 pk  cxGrid1TableView1CellDblClick    TN3805   Implemented for WashP, Comma, and AdSeq actions
  27.07.07 pk  InsertLoopOrCond                 TN3786   New: generating While and if statements and insert into method
  27.08.07 pk  InsertLoopOrCond                 TN3788   New: generate data loop
  28.11.07 wl                                   TN3919    einige Änderungen, damit Read-Only-methoden wirklich nicht mehr editiert werden können
  09.01.08 wl                                   TN3972   diverse Änderungen aufgrund der neuen Method.db-Struktur
  09.01.08 wl                                   TN3972   nicht mehr vorhanden: Seq; neu: Used Tips, Method name
  09.01.08 wl                                   TN3972   CheckBox AutoSeq und SpinEdit Seq entfernt
  07.02.08 wl                                   TN4009   Column-Moving vorübergehend abgeschaltet
  07.02.08 wl  ChooseRack,ChooseRackPositions   TN4009   entfernt
  07.02.08 wl                                   TN4009   alles außer Comments ist jetzt Read-Only
  14.04.08 wl  AddActionNode                    TN4060   RunStepTypeDictionary uses TStringArray
  06.05.08 wl                                   TN4074   New column Inactive
  06.05.08 wl  pmnuActivate,DeactivateSelectedLinesClick  TN4074   Menu items to activate,deactivate the selected lines
  06.05.08 wl  cxGrid1TableView1CustomDrawCell  TN4074   Inactive lines are drawn in money green
  07.05.08 pk  cxGrid1TableView1CustomDrawCell  TN4074   RecordIndex instead of Index. and line color changed to gray
  20.06.08 pk                                   TN4139   WB global object replaced by LayoutManager
  20.06.08 pk                                   TN4139   uses PosTools removed
  09.07.08 pk  AddActionNode                    TN4009   resource corrected to "Choose Action"
  22.07.08 pk  AddMethod1,2, etc                TN4182   Add at current line position instead of at end of method
  22.07.08 pk  AskActionName                    TN4183   code taken from AddActionNode. Now called only in mnuAddRowClick
  25.07.08 pk                                   TN4188   New: Goto Line
  25.07.08 pk                                   TN4190   New: Show modified status in status bar
  25.07.08 pk                                   TN4190   Changed stAttribute to edAttribute and moved down to status bar
  08.09.08 pk  Build                            TN4215   now only compiles.  Does not write run table
  20.09.08 pk  ChooseCarrierSlot                TN4215   disabled for now
  20.09.08 pk                                   TN4215   ActionName constants changed
  25.09.08 wl                                   TN4242   Command macros endgültig entfernt
  06.10.08 pk  LoadMethodData                   TN4258   M_SRack, etc removed
  15.10.08 pk  AddMethod1                       TN4258   reimplemented
  10.11.08 pk  AddMethod3                       TN4280   ThreadStart instead of RunStart Action
  19.11.08 pk  AddMethod3                       TN4280   ThreadStart removed
  09.12.08 pk                                   TN4280   reference to GroupRunStep removed
  17.12.08 pk  sbSimStart                       TN4372   New simulation button
  16.01.09 wl                                   TN4362   an Änderungen in TViewItem angepasst
  18.02.09 pk  InsertLoopOrCond                 TN4232   CALC lines changed to DSXXX actions
  04.03.09 pk  SetEditing                       TN4454   Editing disabled for Comments... ChangeData was not called, and action properties window was not updated
  13.03.09 ts  EnableDisableButtons/Startable   TN4464   Methode nur noch ausführbar, wenn neues Setting "startable" gesetzt ist
  13.03.09 pk  EnableDisableButtons             TN4464   if method is not startable set visible (instead of enabled) to false
  13.03.09 pk  InsertLoopOrCond                 TN4232   use Options string retruned from CodeGenerator instead of OptionsSummary
  18.03.09 pk  EditSetFocusToTextPart           TN4475   Message "Column %s is not visible" removed
  06.04.09 pk  sbDisplayComponent               TN4503   New
  07.04.09 pk  fDisplayComponentLoadedValue     TN4503   New
  10.06.09 pk  sbBuild                          TN4598   Build changed to Compile
  16.06.09 wl                                   TN4606   uses RunEditor entfernt
  16.06.09 wl  sbShowRun                        TN4606   entfernt
  17.06.09 wl  sbStartOptionsClick              TN4612   zusammengefasst: MethodLayoutPage, Use Init, DisplayComponentWizardPage
  30.06.09 wl                                   TN4635   Spaltenbreite und -höhe passen sich jetzt automatisch an
  11.08.09 wl                                   TN4702   Strings werden jetzt direkt geladen
  11.08.09 wl  sbAppendRecordClick              TN4702   es erscheint wieder das Action-Auswahlfenster
  20.08.09 wl                                   TN4702   frmParserCodeGenerateDlg-Instanz nur noch lokal
  24.08.09 wl  AddMethodParallel                TN4702   vobereitet
  04.11.09 pk                                   TN4843   Replace TObjectList, TStringList, TStrings with Generics Lists and TStringArray
  12.11.09 pk                                   TN4861   check CanSetFocus before calling SetFocus function
  19.02.10 ts  AddRowAfter/Before               TN4988   es können neue Zeilen vor oder nach einer gewählten Zeile eingefügt werden
  19.02.10 ts  MoveSelectedRecords              TN4988   wenn eine Zeile per Mouse ans Ende (leerer Bereich) gezogen wird, dann wird sie auch ans Ende verschoben
  08.03.10 ts                                   TN5013   sbSimStart, sbStart invisible to avoid AccessViolations
  12.04.10 ts  EnableDisableButtons             TN5013   xIsStartable not used any longer
  06.05.10 wl                                   TN5052   uses EditingWorkflow
  06.05.10 wl                                   TN5087   Spalten sind jetzt nicht mehr verschiebbar oder editierbar
  06.05.10 wl                                   TN5087   überflüssige Spalten entfernt
  12.05.10 wl                                   TN5064   Massive changes
  17.05.10 wl  ColumnMode                       TN5064   neu: Unterscheidung zwischen Ein-Spalten-Modus und Zwei-Spalten-Modus
  18.05.10 wl  CustomizeView                    TN5064   man kann jetzt auch alle Zeilen anzeigen
  19.05.10 wl  cxGrid1TableView1CustomDrawCell  TN5064   Ein Buchstabe am Anfang wurde weggeschnitten
  20.05.10 wl                                   TN5116   Startbutton aktiviert
  20.05.10 wl                                   TN5117   neue Schriftart "Segoe UI", Schriftgröße 9
  27.05.10 wl  Start,SimStart                   TN5116   entfernt
  28.05.10 wl                                   TN5116   uses geändert
  31.05.10 wl  AddActionNode                    TN5120   UpperCase für ActionName entfernt
  09.06.10 wl  Edit/SavemethodComments          TN5116   von ZADesignObjects hierher
  17.06.10 wl  sbDeleteRecordClick              TN5129   Focus wird zunächst auf Editor gesetzt, sonst Access Violation
  18.06.10 wl                                   TN5116   Buttons überarbeitet
  22.06.10 pk  InsertLoopOrCond                 TN5088   No works with new While, If, etc actions instead of Calc Action
  13.09.10 pk                                   TN5116   Buttons überarbeitet
  21.09.10 pk                                   TN5089   Various changes to MethodEditor for displaying indented actions
  23.09.10 pk                                   TN4989   Shortcut Ctrl + Ins (Einfg) to insert new line
  23.09.10 pk  cxGrid1TableView1CanSelectRecord TN5089   EndIf/EndWhile can only be selected with If and While
  23.09.10 pk  DeleteSelectedRecords_Intern     TN5089   corrected for Multi-selected lines
  23.09.10 pk  cxGrid1TableView1CustomDrawCell  TN5089   Icon is now drawn transparent
  24.09.10 pk                                   TN5089   Various changes
  29.09.10 pk                                   TN5283   Short and Long Comments Combined and can be edit with new comment editor
  30.09.10 pk                                   TN4989   implementation of "NewItemRow" concept - always keep extra line at end of grid
  30.09.10 pk                                   TN5287   Search in methods reimplemented
  08.10.10 pk                                   TN5295   Various changes
  29.10.10 pk                                   TN5320   New RestartEventName method setting
  19.11.10 pk  LoadViewAndMethodData            TN5320   fRestartEventNameLoadedValue was not loaded
  10.12.10 pk  SetSelectedLinesInactive         TN5404   now actives a line correclty
  10.12.10 pk                                   TN5409   Compile button removed and placed in menu instead
  14.12.10 pk  ShowDialog                       TN5330   now returns modal result
  07.02.11 wl                                   TN5461   diverse Änderungen, neue Buttons
  08.02.11 wl                                   TN5474   neue Icons
  10.02.11 wl                                   TN5474   Wizards zeigen Icons
  14.02.11 wl                                   TN5468   entrümpelt
  05.03.11 wl  cxGrid1TableView1MouseDown       TN5472   Verschieben von Zeilen wieder ermöglichen
  05.03.11 wl  OpenMethodStepRelatedEditor      TN5472   Doppelklick bei Icon: Öffnen der Untermethode, SQL, WaschProgram, Liquid Handling Parameter
  11.03.11 wl                                   TN5500   Buttons und Menüs optimieren
  21.03.11 ts                                   TN5503   EditMethodStepComment -> ChangeData eingefügt,
  21.03.11 ts                                            EditMethodStepProperties -> Kommentar bleibt erhalten
  31.05.11 ts  ShowHintForMethodStep,
  31.05.11 ts  cxGrid1MouseLeave                TN5576   HideHint ersetzt durch Timeout HideAfter, da HideHint nicht immer funktioniert
  31.05.11 ts  cxGrid1MouseLeave                TN5576   keine Abfrage ShowingHint, da auch die nicht korrekt funktioniert
  30.06.11 wl                                   TN5620   Import-Schliefe: Neues Icon
  20.07.11 wl  Create                           TN5614.1 abhängig von 'Display', 'MethodEditorBigImages' werden große Icons geladen
  21.07.11 wl  UpdateMethodAttributes           TN5614   enthält jetzt weitere Controls
  26.07.11 wl  Build                            TN5614   Zugriff auf ViewCompileMessages geändert
  02.08.11 wl  sbStartClick                     TN5645   entfernt
  03.08.11 wl  EnableDisableButtons             TN5645   ruft auch Main.RefreshButtons auf
  27.12.11 wl                                   TN5768   DeleteRunDataOption: wenn false, werden vor dem Laden des Layouts keine Daten gelöscht!
  15.03.12 wl                                   TN5835   sieht jetzt in Windows XP weniger scheußlich aus
  10.05.12 wl  SaveData                         TN5155   Methoden ohne Zeilen werden mit einer BLANK-Zeile gespeichert, damit sie nicht verschwinden
  07.08.12 wl  AskActionNameAndAdd              TN5948   Dialog-Parameter geändert
  08.08.12 wl                                   TN5946   uses geändert
  20.09.12 wl                                   TN5982   neu: FOR-Schleife; Data-Loop und While nicht mehr als Buttons
  14.11.12 wl                                   TN6018   Neues Icon für FOR-Loop
  14.11.12 wl                                   TN6018   Bei For,If und While wird sofort das Eigenschaften-Fenster gezeigt
  23.11.12 wl  Instance                         TN6015.1 Es gibt keine Instanz mehr von TDisplayComponentSettingsManager
  05.12.12 wl                                   TN6045   neu: "Building Block", diese erscheinen im Building Block Editor
  11.12.12 wl                                   TN6045   ist abgeleitet von TMethodEditForm
  21.02.13 wl                                   TN6045   uses EditFunctions entfernt
  08.03.13 wl  sbMethVariables                  TN6095   ruft neuen Editor für Variablen auf
  13.03.13 wl                                   TN5960   TViewItemsWorkflow.Instance statt statischer Methode
  18.03.13 wl                                   TN5960   verweist nicht mehr auf frmEdMain
  21.03.13 wl  EditMethodComments               TN6045   --> von MethodEditForm
  21.03.13 wl                                   TN6115   statt rosa Quadrat wird Store mit seinem Symbol dargestellt
  30.08.13 wl  sbChangeSettingsClick            TN6236   neu: Settings-Fenster statt bisher Layout und Comments getrennt
  03.09.13 wl  ShowMethodStepCommentInColumn    TN6236   Kommentare erscheinen wieder wie vorher
  05.12.13 wl  ResetData                        TN6045.1 wird von FirstLoad aufgerufen
  15.04.14 ts  EditAction                       TN5976   Ctrl+A -> alle Zeilen auswählen
  15.04.14 ts                                   TN6397   Spaltenbreite von Kommentar kann geändert werden
  15.04.14 ts                                   TN6398   Ctrl+Ins = Blank line in method
  -------------------------------------------------------------------------------------------------- }

unit MethodEditor;


interface


uses
    Generics.Collections,
    Forms,
    Controls,
    ImgList,
    Menus,
    StdCtrls,
    Buttons,
    Classes,
    ExtCtrls,
    Windows,
    Dialogs,
    cxGraphics,
    cxCustomData,
    cxStyles,
    cxTextEdit,
    cxControls,
    cxInplaceContainer,
    cxEdit,
    cxEditRepositoryItems,
    cxFilter,
    cxData,
    cxDataStorage,
    cxDBData,
    cxGridCustomTableView,
    cxGridTableView,
    cxGridDBTableView,
    cxGridLevel,
    cxClasses,
    cxGridCustomView,
    cxGrid,
    cxImage,
    cxLookAndFeels,
    cxLookAndFeelPainters,
    //
    DockableForm,
    ViewItemEditForm,
    MethodStep,
    MethodSettingsDataAdaptor,
    ViewItem,
    MethodEditDataHelper,
    MethodEditForm,
    ParserCodeGenerateDlg,
    AppTypes,
    StringLoader;

type
    TLoopOrConditionType = (pcgWhile, pcgIf, pcgFor);

    TfrmMethodEditor = class(TMethodEditForm)
        Panel3: TPanel;
        PopupMenu1: TPopupMenu;
        mnuCutItem: TMenuItem;
        mnuCopyItem: TMenuItem;
        mnuPasteItem: TMenuItem;
        cxGrid1Level1: TcxGridLevel;
        cxGrid1: TcxGrid;
        cxGrid1TableView1: TcxGridTableView;
        cxGrid1TableView1ColumnSummary: TcxGridColumn;
        cxGrid1TableView1ColumnComment: TcxGridColumn;
        mnuHeader: TMenuItem;
        Panel1: TPanel;
        sbAppendRecord: TSpeedButton;
        mnuAddEmptyLine: TMenuItem;
        mnuDeleteLines: TMenuItem;
        sbDeleteRecord: TSpeedButton;
        sbChangeSettings: TSpeedButton;
        mnuInsertWhileLoopSelected: TMenuItem;
        mnuInsertConditionSelected: TMenuItem;
        mnuInsertDataLoopSelected: TMenuItem;
        pmnuDeactivateSelectedLines: TMenuItem;
        pmnuActivateSelectedLines: TMenuItem;
        mnuGotoLine: TMenuItem;
        pnlStatus: TPanel;
        pnlCurLineNumber: TPanel;
        lblCurLineNumber: TLabel;
        edCurLineNumber: TEdit;
        Panel2: TPanel;
        Panel4: TPanel;
        edModifiedStatus: TEdit;
        Panel5: TPanel;
        edAttribute: TEdit;
        Memo1: TMemo;
        ImageList1: TImageList;
        N1: TMenuItem;
        mnuEditComment: TMenuItem;
        BalloonHint1: TBalloonHint;
        sbCheckMethod: TSpeedButton;
        mnuAddActionLine: TMenuItem;
        sbInsertCondition: TSpeedButton;
        sbInsertForLoop: TSpeedButton;
        ImageListWizard: TImageList;
        mnuOpenItem: TMenuItem;
        mnuOpenProperties: TMenuItem;
        N4: TMenuItem;
        mnuInsertForLoopSelected: TMenuItem;
        sbMethVaraibles: TSpeedButton;
        procedure FormShow(Sender: TObject);
        procedure cxGrid1TableView1DragDrop(Sender, Source: TObject; aX, aY: Integer);
        procedure cxGrid1TableView1DragOver(Sender, Source: TObject; X, Y: Integer; State: TDragState;
            var Accept: Boolean);
        procedure cxGrid1TableView1FocusedRecordChanged(Sender: TcxCustomGridTableView;
            APrevFocusedRecord, AFocusedRecord: TcxCustomGridRecord; ANewItemRecordFocusingChanged: Boolean);
        procedure cxGrid1Exit(Sender: TObject);
        procedure cxGrid1TableView1SelectionChanged(Sender: TcxCustomGridTableView);
        procedure cxGrid1TableView1KeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
        procedure cxGrid1TableView1MouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState;
            X, Y: Integer);
        procedure sbAppendRecordClick(Sender: TObject);
        procedure mnuAddEmptyLineClick(Sender: TObject);
        procedure mnuDeleteLinesClick(Sender: TObject);
        procedure sbDeleteRecordClick(Sender: TObject);
        procedure cxGrid1TableView1DataControllerAfterDelete(ADataController: TcxCustomDataController);
        procedure sbChangeSettingsClick(Sender: TObject);
        procedure cxGrid1TableView1CellDblClick(Sender: TcxCustomGridTableView;
            ACellViewInfo: TcxGridTableDataCellViewInfo; AButton: TMouseButton; AShift: TShiftState;
            var AHandled: Boolean);
        procedure PopupMenu1Popup(Sender: TObject);
        procedure mnuInsertWhileLoopSelectedClick(Sender: TObject);
        procedure mnuInsertConditionSelectedClick(Sender: TObject);
        procedure mnuInsertDataLoopSelectedClick(Sender: TObject);
        procedure cxGrid1TableView1CustomDrawCell(Sender: TcxCustomGridTableView; ACanvas: TcxCanvas;
            AViewInfo: TcxGridTableDataCellViewInfo; var ADone: Boolean);
        procedure pmnuDeactivateSelectedLinesClick(Sender: TObject);
        procedure pmnuActivateSelectedLinesClick(Sender: TObject);
        procedure mnuGotoLineClick(Sender: TObject);
        procedure cxGrid1Resize(Sender: TObject);
        procedure cxGrid1TableView1CanSelectRecord(Sender: TcxCustomGridTableView;
            ARecord: TcxCustomGridRecord; var AAllow: Boolean);
        procedure cxGrid1TableView1GetCellHeight(Sender: TcxCustomGridTableView; ARecord: TcxCustomGridRecord;
            AItem: TcxCustomGridTableItem; ACellViewInfo: TcxGridTableDataCellViewInfo; var AHeight: Integer);
        procedure mnuEditCommentClick(Sender: TObject);
        procedure cxGrid1TableView1MouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
        procedure cxGrid1MouseLeave(Sender: TObject);
        procedure sbCheckMethodClick(Sender: TObject);
        procedure mnuAddActionLineClick(Sender: TObject);
        procedure sbInsertConditionClick(Sender: TObject);
        procedure sbInsertForLoopClick(Sender: TObject);
        procedure mnuOpenPropertiesClick(Sender: TObject);
        procedure mnuOpenItemClick(Sender: TObject);
        procedure mnuInsertForLoopSelectedClick(Sender: TObject);
        procedure sbMethVaraiblesClick(Sender: TObject);

    private const
        cIndentWidth = 20;
        cFirstXOffset = 8;
        STR_LOCALINI_SECTION = 'MethodView';
    private
        fActionImages: TImageList;
        FInternUpdate: boolean;
        fStringLoader: TMethodEditorStringLoader;
        fColumnMode: TMethodEditorColumnMode;
        fIsSelecting: boolean;
        fPrevMousePos: TPoint;
        procedure DefineMenuItems(aX, aY: Integer);
        procedure CheckClipboardPaste();
        function GetSavedInsertRecIndex: integer;
        procedure SelectAllData(aSender: TObject);
        procedure FindTextInMethod(aOwner: TObject; const aText: string; aOptions: TFindOptions);
        procedure ReplaceTextInMethod(aOwner: TObject; const aFindText, aReplaceText: string;
            aOptions: TFindOptions);
        procedure DoFindTextInMethod(const aFindText: string; aOptions: TFindOptions);
        procedure DoReplaceTextInMethod(const aFindText, aReplaceText: string; aOptions: TFindOptions);

        procedure ReplaceAllTextInMethod(const aFindText, aReplaceText: string; aOptions: TFindOptions);
        function GetMethodName: string;
        procedure CheckRangeLineLevels(const aFirstIndex, aLastIndex: integer);
        procedure CheckSelectedLineLevels();
        procedure MoveSelectedRecords();
        procedure CopySelectedRecords();
        procedure PasteSelectedRecords(aInsertRecIndex: integer);
        procedure DeleteSelectedRecords(var vFocusIndex: integer);
        procedure DeleteSelectedRecords_Intern(var vFocusIndex: integer);
        procedure EditCut;
        procedure EditCopy;
        procedure EditPaste;
        procedure WriteAttributeText();
        procedure DeterminePopupEnabled();

        procedure InsertLoopOrCond(aDlgType: TLoopOrConditionType);
        procedure InsertDataLoop;
        procedure AskActionNameAndAdd(aRecIndex: integer);
        procedure DefineCurrentMethodStep();
        procedure SetCurLineNumberDisplay(aRecIndex: integer);

        procedure SetColumnMode(const Value: TMethodEditorColumnMode);
        procedure CustomizeView;

        function GetLineStartX(const aMethodLine: TMethodEditLine): integer;
        function GetLineIconX(const aMethodLine: TMethodEditLine): integer;
        function IsXOnLineIcon(const aX: integer; const aMethodLine: TMethodEditLine): boolean;

        procedure EditMethodStepProperties(const aIndex: integer);
        procedure EditMethodStepPropertiesOrComment(const aIndex: integer);
        procedure OpenMethodStepRelatedEditor(const aIndex: integer);
        procedure EditCellProperties(const aRecordIndex: integer; const aColIndex: integer;
            const aEditRelatedProperties: boolean);

        procedure DrawComment(const aCanvas: TcxCanvas; const aMethodLine: TMethodEditLine;
            const aIsSummaryCol: boolean; const aIsSelected: boolean; var vRect: TRect);
        procedure DrawGroupBrackets(const aCanvas: TcxCanvas; const aMethodLine: TMethodEditLine;
            const aRect: TRect);

        procedure DrawIconOrShape(const aCanvas: TcxCanvas; const aMethodLine: TMethodEditLine;
            const aIsSelected: boolean; var vRect: TRect);

        procedure DrawSummaryText(const aCanvas: TcxCanvas; const aMethodLine: TMethodEditLine;
            const aIsSelected: boolean; var vRect: TRect);

        procedure SetSelectedLinesInactive(const aInactive: boolean);

        class function CalculateTextHeight(const aText: string; const aCanvas: TCxCanvas;
            const aPossibleCellWidth: integer): integer;

        function ShowMethodStepCommentInColumn(const aMethodStep: TMethodStep): integer;
        procedure AddMethodStepAndSelect(const aMethodStep: TMethodStep; aIndex: integer);
        procedure ShowHintForMethodStep(const aMethodStep: TMethodStep; const aHint: TBalloonHint);
        procedure LoadLayout();
        function CreateClipboardData(const aFirstRecordIndex: integer = 0): TClipboardDataMethodSteps;
        procedure DoClipboardDataOnLoadFromStream(const aMethodSteps: TObjectList<TMethodStep>;
            const aFirstRecordIndex: integer);
        procedure SavePositions();
        procedure LoadPositions();
    protected
        function GetAttribute: TVisibilityAttribute; override;
        function CreateViewItem(const aItemName: string): TViewItem; override;
        procedure ResetData(); override;
        function CanClose(): boolean; override;
        function CheckLastChange(): boolean; override;
        procedure SaveStatusChanged; override;
        function EditMethodStepModal(const aMethodStep: TMethodStep): boolean; override;
        function GetInsertRecIndex: integer; override;
        procedure SaveData(); override;
    public
        constructor Create(aOwner: TComponent; const aItemName: string; aOnSaveStatusChanged: TNotifyEvent);
            reintroduce; virtual;
        destructor Destroy; override;
        //
        procedure FirstLoad(); override;
        function EditAction(aSender: TObject; aEditAction: TEditActionType): boolean; override;
        procedure SelectEditor(); override;

        procedure AddMethod(const aMethodName: string); overload;
        procedure AddMethod(const aMethodName: string; aRecIndex: integer); overload;
        procedure AddMethodParallel(const aMethodName: string); overload;
        procedure AddMethodParallel(const aMethodName: string; aRecIndex: integer); overload;
        procedure AddWashProg(const aWashProgName: string; aRecIndex: integer);
        procedure UpdateMethodAttributes(); override;
        function Build(): boolean; override;
        procedure EditSetFocusToRow(aRow: integer); override;
        procedure EditSetFocusToTextPart(const aRow: integer; const aSettingKey: TArray<string>;
            const aFocusStart, aFocusLen: integer); override;
        function StartableFromGUI(): boolean;
        //
        property InternUpdate: boolean read FInternUpdate;
        property PMethodName: string read GetMethodName;
        // property LayoutName: string read fLayoutName write SetLayoutName;
        property Attribute: TVisibilityAttribute read GetAttribute;
        property ColumnMode: TMethodEditorColumnMode read fColumnMode write SetColumnMode;
    end;


implementation


uses
    cxTL,
    StrUtils,
    SysUtils,
    IfRunStep,
    ForRunStep,
    WhileRunStep,
    BlankRunStep,
    AddMethodRunStep,
    Graphics,
    AppSettings,
    CommonTypes,
    GUIManager,
    MethodDataAdaptor,
    MethodTypes,
    GeneralTypes,
    EdActionProps,
    DesignerMain,
    DialogUtils,
    RunStepInfoTypeDictionary,
    ImportDataAdaptor,
    ViewItemsWorkflow,
    MethodDataCache,
    CustomSetting,
    CustomLeafSettings,
    UtilLib,
    ZADesignLayout,
    MethodCompile,
    MethodViewItem,
    DesignerMethodViewItem,

    // sollte wieder raus:
    MethodStepDataFields,
    RunStepBuilderTypeDictionary,

    DisplayComponentSettingsManager,
    DisplayComponentIntf,
    Wizard,
    ControlUtils,
    ParamStoreRunStepBuilder,
    MethodStepCommentEditor,
    MethodStepSelectDialog,
    ConfigurationFile;

{$R *.DFM}

var
    uGridColumn: TcxGridColumn;
    uGridRecord: TcxCustomGridRecord;

    { TfrmMethodEditor }

constructor TfrmMethodEditor.Create(aOwner: TComponent; const aItemName: string;
    aOnSaveStatusChanged: TNotifyEvent);
var
    xIniAccess: IWinlissyIniAccess;
begin
    inherited Create(aOwner, aItemName, aOnSaveStatusChanged,
        frmDesignerMain.EditFunctions.Parameter_OnAddEditFunctions);

    TControlUtils.ResetFontForWinXP(self);
    fStringLoader := TMethodEditorStringLoader.Create;
    fStringLoader.LoadLanguage(self);

    SetGridPointer(self.cxGrid1, self.cxGrid1TableView1);

    edAttribute.Text := '';
    FInternUpdate := true;

    fAddingNewRecord := false;
    fHasUndoFunction := true;
    fHasClipboardFunction := true;
    self.OnSelectAll := SelectAllData;
    self.OnFindText := FindTextInMethod;
    self.OnReplaceText := ReplaceTextInMethod;
    // caption MUST be set AFTER loadlanguage otherwise caption=''
    Caption := GetCaption();

    FInternUpdate := false;

    BalloonHint1.Images := TViewItemsWorkflow.Instance.ActionImages.ActionImages64;

    self.cxGrid1Resize(nil);

    self.PopupMenu1.Images := frmDesignerMain.MainImageList;
    self.mnuCutItem.Action := frmDesignerMain.actEditCut;
    self.mnuCopyItem.Action := frmDesignerMain.actEditCopy;
    self.mnuPasteItem.Action := frmDesignerMain.actEditPaste;

    // Versuchsballon Icons 24
    xIniAccess := TAppSettings.CreateAppIni;
    if xIniAccess.ReadBool('Display', 'MethodEditorBigImages') then
        fActionImages := TViewItemsWorkflow.Instance.ActionImages.ActionImages24
    else
        fActionImages := TViewItemsWorkflow.Instance.ActionImages.ActionImages16;
    LoadPositions;
end;

procedure TfrmMethodEditor.SavePositions();
var
    xLocalIniFile: IConfigurationSet;
begin
    if cxGrid1TableView1.Columns[1].Width <> 124 then
    begin
        xLocalIniFile := TConfigurationFile.Create(TAppsettings.LocalIniFileName);
        xLocalIniFile.Open(false);
        try
            xLocalIniFile.WriteInteger(STR_LOCALINI_SECTION, 'CommentColWidth_' + fViewItem.name,
                cxGrid1TableView1.Columns[1].Width);
        finally
            xLocalIniFile.Close;
        end;
    end;
end;

procedure TfrmMethodEditor.LoadPositions();
var
    xLocalIniFile: IConfigurationSet;
begin
    xLocalIniFile := TConfigurationFile.Create(TAppsettings.LocalIniFileName);
    xLocalIniFile.Open(true);
    try
        cxGrid1TableView1.Columns[1].Width := xLocalIniFile.ReadInteger(STR_LOCALINI_SECTION,
            'CommentColWidth_' + fViewItem.name, 124);
    finally
        xLocalIniFile.Close;
    end;
end;

function TfrmMethodEditor.CreateViewItem(const aItemName: string): TViewItem;
begin
    result := TDesignerMethodViewItem.Create(aItemName);
end;

destructor TfrmMethodEditor.Destroy;
begin
    frmDesignerMain.actEditUndo.Enabled := false;
    frmDesignerMain.actEditRedo.Enabled := false;
    frmDesignerMain.actEditCut.Enabled := false;
    frmDesignerMain.actEditCopy.Enabled := false;

    inherited;
end;

procedure TfrmMethodEditor.FirstLoad();
begin
    FInternUpdate := true;

    InsertNewItemRowLine();

    ResetData();

    SetFocusAndSelectRecord(0);

    FInternUpdate := false;
end;

function TfrmMethodEditor.Build(): boolean;
var
    xSavedCursor: integer;
begin
    result := false;
    if not RequestSaveChanges() then
        EXIT;

    frmDesignerMain.actViewCompilerMessagesExecute(nil);

    xSavedCursor := gGUIManager.SetCursor(crHourglass);
    try
        TMethodCompile.InstCompileJustCheck(fViewItem.name,
            frmDesignerMain.MethodDevelopment.CompilerMessagesStart);
    finally
        gGUIManager.SetCursor(xSavedCursor);
    end;

    result := (frmDesignerMain.MethodDevelopment.CompilerMessagesLoad = 0);
end;

procedure TfrmMethodEditor.LoadLayout();
begin
    TViewItemsWorkflow.Instance.LoadLayout(fSettings.LayoutName)
end;

procedure TfrmMethodEditor.SelectEditor();
var
    xRec: integer;
begin
    LoadLayout();

    // Problems occured with the copy/paste popup and mainform edit menu, so we have to select a record and call determinepopupenabled
    if cxGrid1.CanFocus and (not cxGrid1.IsFocused) then
    begin
        cxGrid1.SetFocus;

        xRec := cxGrid1TableView1.DataController.FocusedRecordIndex;
        SetFocusAndSelectRecord(xRec);
    end;

    DeterminePopupEnabled();
end;

procedure TfrmMethodEditor.AskActionNameAndAdd(aRecIndex: integer);
var
    xActions: TArray<string>;
    xModalResult: TModalResult;
    xActionName: string;
begin
    xActions := TViewItemsWorkflow.Instance.ReadAllNames(ntAction);

    xModalResult := TfrmMethodStepSelectDialog.ShowDialog(xActions,
        TViewItemsWorkflow.Instance.ActionImages.ActionImages24,
        TViewItemsWorkflow.Instance.ActionImages.ActionImages64, xActionName);

    case xModalResult of
        mrOK:
            self.AddAction(xActionName, aRecIndex);
        // mrYes: self.AddAction( 'BLANK', aRecIndex );
    end;
end;

procedure TfrmMethodEditor.SetCurLineNumberDisplay(aRecIndex: integer);
var
    xCurLineNumber: integer;
begin
    xCurLineNumber := aRecIndex + 1;
    edCurLineNumber.Text := IntToStr(xCurLineNumber);
end;

procedure TfrmMethodEditor.DefineCurrentMethodStep();
var
    xRecIndex: integer;
begin
    xRecIndex := cxGrid1TableView1.DataController.FocusedRecordIndex;
    SetCurLineNumberDisplay(xRecIndex);
end;

procedure TfrmMethodEditor.FormShow(Sender: TObject);
begin
    if cxGrid1.CanFocus then
        cxGrid1.SetFocus;
end;

function TfrmMethodEditor.CanClose(): boolean;
var
    xAskClose: integer;
begin
    SavePositions;
    result := true;
    if (self.IsDeleted) then
        EXIT;
    if cxGrid1TableView1.DataController.RecordCount > 0 then
        EXIT;

    xAskClose := Application.MessageBox
        (PChar(Format('Method %s contains no data and will be deleted after closing the editor.',
        [fViewItem.name]) + #13#10 + 'Close editor and delete method?'), 'No data',
        MB_OKCANCEL + MB_ICONWARNING);
    result := (xAskClose = ID_OK);
    if not result then
        EXIT;
    TViewItemsWorkflow.Instance.OverviewFormsUpdateItems([ntMethod]);
end;

function TfrmMethodEditor.CheckLastChange(): boolean;
var
    xRecIndex: integer;
begin
    result := true;

    // Editoren, die gerade benutzt werden, schließen!
    xRecIndex := cxGrid1TableView1.DataController.FocusedRecordIndex;
    if (xRecIndex < 0) then
    begin
        EXIT;
    end;

    // TBD_WL: Diese Lösung ist noch nicht ideal, weil der Fokus immer versetzt wird. Nötig wäre
    // dies aber nur, wenn cxGrid1 noch den Fokus hat.
end;

function TfrmMethodEditor.StartableFromGUI: boolean;
begin
    result := fSettings.Startable;
end;

function TfrmMethodEditor.EditMethodStepModal(const aMethodStep: TMethodStep): boolean;
begin
    EXIT(TfrmActionProps.InstanceEditMethodStepModal(aMethodStep,
        TViewItemsWorkflow.Instance.ActionImages.ActionImages64, self.IsWritable) = mrOK);
end;

procedure TfrmMethodEditor.EditMethodStepProperties(const aIndex: integer);
var
    xMethodLine: TMethodEditLine;
    xMethodStep: TMethodStep;
    xComment: string;
begin
    if not MethodEditLineExists(aIndex) then
        EXIT;

    xMethodLine := fMethodEditLines[aIndex];

    xMethodStep := xMethodLine.Value;
    xComment := xMethodStep.M_Comments;
    if xMethodStep is TBlankMethodStep then
        EXIT;

    if TfrmActionProps.InstanceEditMethodStepModal(xMethodStep,
        TViewItemsWorkflow.Instance.ActionImages.ActionImages64, self.IsWritable) = mrOK then
    begin
        xMethodStep.M_Comments := xComment;
        self.ChangeData();
    end;
end;

procedure TfrmMethodEditor.EditMethodStepPropertiesOrComment(const aIndex: integer);
var
    xMethodLine: TMethodEditLine;
    xMethodStep: TMethodStep;
begin
    if not MethodEditLineExists(aIndex) then
        EXIT;

    xMethodLine := fMethodEditLines[aIndex];

    xMethodStep := xMethodLine.Value;
    if xMethodStep is TBlankMethodStep then
    begin
        EditMethodStepComment(aIndex);
    end
    else
    begin
        EditMethodStepProperties(aIndex);
    end;
end;

procedure TfrmMethodEditor.OpenMethodStepRelatedEditor(const aIndex: integer);
var
    xMethodLine: TMethodEditLine;
    xMethodStep: TMethodStep;
    xRelatedItemParam: TCustomSetting;
    xSubItemName: string;
begin
    if not MethodEditLineExists(aIndex) then
        EXIT;

    xMethodLine := fMethodEditLines[aIndex];
    xMethodStep := xMethodLine.Value;

    xRelatedItemParam := xMethodStep.RelatedItemParam;
    if Assigned(xRelatedItemParam) then
    begin
        xSubItemName := xRelatedItemParam.Value;
        if (xSubItemName = '') then
            EXIT;

        xRelatedItemParam.OpenFunction();
    end;
end;

procedure TfrmMethodEditor.mnuAddEmptyLineClick(Sender: TObject);
begin
    if not CheckLastChange() then
        EXIT;
    self.AddAction('BLANK', GetInsertRecIndex());
end;

procedure TfrmMethodEditor.mnuAddActionLineClick(Sender: TObject);
begin
    if not self.IsWritable then
        EXIT;
    if not CheckLastChange() then
        EXIT;
    AskActionNameAndAdd(GetInsertRecIndex());
end;

function GetCommentFromMethod(const aMethodName: string): string;
var
    xDA: TMethodSettingsDataAdaptor;
begin
    xDA := TMethodSettingsDataAdaptor.Create;
    try
        EXIT(xDA.ReadRec(aMethodName).Comment);
    finally
        FreeAndNil(xDA);
    end;
end;

procedure TfrmMethodEditor.AddMethod(const aMethodName: string; aRecIndex: integer);
var
    xMethodStep: TMethodStep;
begin
    xMethodStep := self.CreateMethodStep(cActionAddMethod, aMethodName);
    xMethodStep.M_Comments := GetCommentFromMethod(aMethodName);

    self.AddMethodStepLine(xMethodStep, aRecIndex, true);
    SetFocusAndSelectRecord(aRecIndex);
    ForceGridResize();
end;

procedure TfrmMethodEditor.AddMethod(const aMethodName: string);
begin
    AddMethod(aMethodName, self.GetInsertRecIndex());
end;

procedure TfrmMethodEditor.AddMethodParallel(const aMethodName: string; aRecIndex: integer);
const
    cThreadStartActionName = 'THRST'; // redundant!!
var
    xMethodStep: TMethodStep;
begin
    xMethodStep := self.CreateMethodStep(cThreadStartActionName, aMethodName);
    xMethodStep.M_Comments := GetCommentFromMethod(aMethodName);

    self.AddMethodStepLine(xMethodStep, aRecIndex, true);
    SetFocusAndSelectRecord(aRecIndex);
    ForceGridResize();
end;

procedure TfrmMethodEditor.AddMethodParallel(const aMethodName: string);
begin
    AddMethodParallel(aMethodName, self.GetInsertRecIndex());
end;

procedure TfrmMethodEditor.AddWashProg(const aWashProgName: string; aRecIndex: integer);
const
    cActionNameWashProgram = 'WASHP'; // redundant!!
var
    xRecIndex: integer;
    xMethodStep: TMethodStep;
begin
    if aRecIndex = -1 then
        xRecIndex := self.GetInsertRecIndex()
    else
        xRecIndex := aRecIndex;

    xMethodStep := self.CreateMethodStep(cActionNameWashProgram, aWashProgName);

    self.AddMethodStepLine(xMethodStep, xRecIndex, true);
    SetFocusAndSelectRecord(aRecIndex);
    ForceGridResize();
end;

procedure TfrmMethodEditor.ResetData;
begin
    LoadViewAndMethodData(Memo1);
end;

function TfrmMethodEditor.EditAction(aSender: TObject; aEditAction: TEditActionType): boolean;
var
    xIsEditingSingleCell: boolean;
begin
    xIsEditingSingleCell := cxGrid1TableView1.Controller.IsEditing;

    // We only want to overwrite the actions for Row editing operations
    // We also dont do anything if sender is not a child control of cxGrid1TableView1
    result := (not(xIsEditingSingleCell)) and cxGrid1TableView1.Control.ContainsControl(aSender as TControl);
    if not result then
        EXIT;

    case aEditAction of
        eaCut:
            EditCut;
        eaCopy:
            EditCopy;
        eaPaste:
            EditPaste;
        eaSelectAll:
            SelectAllData(aSender);
    end;
end;

procedure TfrmMethodEditor.EditCut;
var
    xRecIndex: integer;
begin
    CopySelectedRecords();
    if (self.Attribute <> meaDefault) then
        EXIT;

    xRecIndex := cxGrid1TableView1.Controller.FocusedRow.Index;
    DeleteSelectedRecords(xRecIndex);
end;

procedure TfrmMethodEditor.EditCopy;
begin
    CopySelectedRecords();
end;

function TfrmMethodEditor.GetSavedInsertRecIndex(): integer;
begin
    if Assigned(uGridRecord) then
        result := uGridRecord.Index // für Rechtsklick und DragDrop
    else
        result := cxGrid1TableView1.DataController.RecordCount - 1;
end;

function TfrmMethodEditor.GetInsertRecIndex(): integer;
begin
    result := cxGrid1TableView1.DataController.FocusedRecordIndex;
    // für Buttons und Menus außerhalb des Grids
    if result < 0 then
        result := cxGrid1TableView1.DataController.RecordCount - 1;
end;

procedure TfrmMethodEditor.EditPaste;
begin
    PasteSelectedRecords(GetInsertRecIndex());
end;

procedure TfrmMethodEditor.DoClipboardDataOnLoadFromStream(const aMethodSteps: TObjectList<TMethodStep>;
    const aFirstRecordIndex: integer);
var
    x: integer;
    xRecordIndex: integer;
begin
    self.cxGrid1TableView1.Controller.ClearSelection;
    for x := 0 to aMethodSteps.Count - 1 do
    begin
        xRecordIndex := aFirstRecordIndex + x;
        InsertMethodStepAtIndex(aMethodSteps[x], xRecordIndex);
        self.ChangeData();
    end;
end;

function TfrmMethodEditor.CreateClipboardData(const aFirstRecordIndex: integer = 0)
    : TClipboardDataMethodSteps;
begin
    result := TClipboardDataMethodSteps.Create(fMethodEditDataHelper, aFirstRecordIndex);
    result.OnLoadFromStream := DoClipboardDataOnLoadFromStream;
    result.OnSaveToStream := DoClipboardDataOnSaveToStream;
end;

procedure TfrmMethodEditor.CheckClipboardPaste;
var
    xClipboardData: TClipboardDataMethodSteps;
begin
    // Krücke: Nur damit Paste immer enabled ist!
    if (frmDesignerMain.actEditPaste.Enabled) then
        EXIT;
    xClipboardData := CreateClipboardData();
    try
        if xClipboardData.ClipboardHasFormat() then
            frmDesignerMain.actEditPaste.Enabled := true;
    finally
        xClipboardData.Free;
    end;
end;

procedure TfrmMethodEditor.cxGrid1Exit(Sender: TObject);
begin
    self.BalloonHint1.HideHint;
end;

procedure TfrmMethodEditor.cxGrid1MouseLeave(Sender: TObject);
begin
    // if self.BalloonHint1.ShowingHint then
    self.BalloonHint1.HideAfter := 100;
end;

procedure TfrmMethodEditor.SetColumnMode(const Value: TMethodEditorColumnMode);
begin
    if Value = fColumnMode then
        EXIT;

    fColumnMode := Value;

    self.CustomizeView;
end;

procedure TfrmMethodEditor.cxGrid1Resize(Sender: TObject);
begin
    if (cxGrid1.Width > 750) then
    begin
        self.ColumnMode := mcmTwoColumns;
    end;

    if (cxGrid1.Width < 550) then
        self.ColumnMode := mcmOneColumn;
end;

procedure TfrmMethodEditor.CheckRangeLineLevels(const aFirstIndex, aLastIndex: integer);
var
    xIndices: TArray<integer>;
    x: integer;
    xLength: integer;
begin
    if cxGrid1TableView1.Controller.SelectedRecordCount = 0 then
        EXIT;
    xLength := aLastIndex - aFirstIndex + 1;
    SetLength(xIndices, xLength);
    for x := 0 to xLength - 1 do
    begin
        xIndices[x] := aFirstIndex + x;
    end;

    CheckLineLevels(xIndices);
end;

procedure TfrmMethodEditor.CheckSelectedLineLevels();
var
    xIndices: TArray<integer>;
    x: integer;
    xRecIndex: integer;
begin
    if cxGrid1TableView1.Controller.SelectedRecordCount = 0 then
        EXIT;
    SetLength(xIndices, cxGrid1TableView1.Controller.SelectedRecordCount);
    for x := 0 to cxGrid1TableView1.Controller.SelectedRecordCount - 1 do
    begin
        xRecIndex := cxGrid1TableView1.Controller.SelectedRecords[x].RecordIndex;
        xIndices[x] := xRecIndex;
    end;

    CheckLineLevels(xIndices);
end;

procedure TfrmMethodEditor.DeleteSelectedRecords_Intern(var vFocusIndex: integer);
var
    x, xRecIndex: integer;
begin
    if cxGrid1TableView1.Controller.SelectedRecordCount = 0 then
        EXIT;

    try
        for x := cxGrid1TableView1.Controller.SelectedRecordCount - 1 downto 0 do
        begin
            xRecIndex := cxGrid1TableView1.Controller.SelectedRecords[x].RecordIndex;
            if (xRecIndex < 0) then
                CONTINUE;

            if (xRecIndex < vFocusIndex) then
                dec(vFocusIndex); // FocusedRecordIndex verschiebt sich

            DeleteEditLines(xRecIndex, xRecIndex);
        end;
        cxGrid1TableView1.Controller.ClearSelection();

        ForceGridResize();

    finally
        ChangeData();
    end;
end;

procedure TfrmMethodEditor.DeleteSelectedRecords(var vFocusIndex: integer);
begin
    if (cxGrid1TableView1.Controller.SelectedRecordCount <= 0) then
        EXIT;

    CheckSelectedLineLevels();

    gGUIManager.SetCursor(crHourglass);
    fAddingNewRecord := true;
    try
        DeleteSelectedRecords_Intern(vFocusIndex);
        SetFocusAndSelectRecord(vFocusIndex);
    finally
        fAddingNewRecord := false;
        gGUIManager.SetCursor(crDefault);
    end;
end;

procedure TfrmMethodEditor.CopySelectedRecords();
var
    xClipboardData: TClipboardDataMethodSteps;
begin
    if (cxGrid1TableView1.Controller.SelectedRecordCount <= 0) then
        EXIT;
    CheckSelectedLineLevels();

    gGUIManager.SetCursor(crHourglass);
    fAddingNewRecord := true;
    try
        xClipboardData := CreateClipboardData();
        try
            xClipboardData.SaveToClipboard();
        finally
            xClipboardData.Free;
        end;
    finally
        fAddingNewRecord := false;
        gGUIManager.SetCursor(crDefault);
    end;
end;

procedure TfrmMethodEditor.PasteSelectedRecords(aInsertRecIndex: integer);
var
    xClipboardData: TClipboardDataMethodSteps;
begin
    if (self.Attribute <> meaDefault) then
        EXIT;

    gGUIManager.SetCursor(crHourglass);
    fAddingNewRecord := true;
    try
        xClipboardData := CreateClipboardData(aInsertRecIndex);
        // TClipboardDataMethodSteps.Create( cxGrid1TableView1, aInsertRecIndex );
        try
            xClipboardData.LoadFromClipboard();
        finally
            xClipboardData.Free;
        end;
    finally
        gGUIManager.SetCursor(crDefault);
        fAddingNewRecord := false;
    end;
end;

procedure TfrmMethodEditor.MoveSelectedRecords();
var
    xInsertRecIndex: integer;
    xClipboardData: TClipboardDataMethodSteps;
begin
    // prüfen,ob überhaupt etwas verschoben werden kann
    if (not Assigned(cxGrid1TableView1.Controller.FocusedRecord)) or
        (cxGrid1TableView1.Controller.FocusedRecord.Selected and Assigned(uGridRecord)) then
        EXIT; // wenn uGridRecord nil ist, dann wurde in leeren Bereich geklickt
    if Assigned(uGridRecord) then
        // SelectedRecord nur ausgewählt, wenn nicht in leeren Bereich geklickt wurde
        if (cxGrid1TableView1.Controller.SelectedRecordCount <= 0) then
            EXIT;
    if (self.Attribute <> meaDefault) then
        EXIT;

    if TDialogUtils.MessageBox(TLanguageString.Read('Move selected method step?',
        'Gewählten Methodenschritt verschieben?'), TLanguageString.Read('Move line', 'Zeile verschieben'),
        MB_YESNO) = mrNO then
    begin
        cxGrid1TableView1.Controller.ClearSelection();
        EXIT;
    end;

    if Assigned(uGridRecord) then
        xInsertRecIndex := cxGrid1TableView1.Controller.FocusedRecordIndex
    else
        xInsertRecIndex := GetSavedInsertRecIndex();
    // wenn in leeren Bereich geklickt wurde, dann wird RecordCount als InsertIndex verwendet (letzte Position)
    if (cxGrid1TableView1.Controller.SelectedRecordCount <= 0) then
        EXIT;

    // da ich keine Funktion zum Verschieben von Zeilen gefunden habe, benutze ich NOCH die
    // Clipboard-Funktionen Cut und Paste (WL)

    CheckSelectedLineLevels();

    gGUIManager.SetCursor(crHourglass);
    fAddingNewRecord := true;
    try
        // copy
        xClipboardData := CreateClipboardData(); // TClipboardDataMethodSteps.Create( cxGrid1TableView1 );
        try
            xClipboardData.SaveToClipboard();
        finally
            xClipboardData.Free;
        end;

        // delete records
        DeleteSelectedRecords_Intern(xInsertRecIndex);

        // paste
        xClipboardData := CreateClipboardData(xInsertRecIndex);
        try
            xClipboardData.LoadFromClipboard();
        finally
            xClipboardData.Free;
        end;
    finally
        fAddingNewRecord := false;
        gGUIManager.SetCursor(crDefault);
        ChangeData();
    end;
end;

procedure TfrmMethodEditor.cxGrid1TableView1DragDrop(Sender, Source: TObject; aX, aY: Integer);
var
    xHitTest: TcxCustomGridHitTest;
    xRecIndex: integer;
begin
    if not(Sender is TcxGridSite) then
        EXIT;

    xHitTest := (Sender as TcxGridSite).ViewInfo.GetHitTest(aX, aY);
    if (xHitTest is TcxGridRecordHitTest) then
        uGridRecord := (xHitTest as TcxGridRecordHitTest).GridRecord
    else
        uGridRecord := nil;
    xRecIndex := GetSavedInsertRecIndex();

    if (Source is TcxTreeList) // Overview oder Favourites
        and (self.Attribute = meaDefault) and Assigned((Source as TcxTreeList).DragNode) then
    begin

        if (Source as TcxTreeList).DragNode.StateIndex = INT_IM_INDEX_METHOD then
            AddMethod((Source as TcxTreeList).DragNode.Texts[0], xRecIndex);

        if (Source as TcxTreeList).DragNode.StateIndex = INT_IM_INDEX_ACTION then
            AddAction((Source as TcxTreeList).DragNode.Texts[0], xRecIndex);

        if (Source as TcxTreeList).DragNode.StateIndex = INT_IM_INDEX_WASHPROG then
            AddWashProg((Source as TcxTreeList).DragNode.Texts[0], xRecIndex);
    end;

    // interne Verschiebungen (nicht aktiv)
    if (Source is TDragControlObject) and (TDragControlObject(Source).Control is TcxGridSite) and
        ((TDragControlObject(Source).Control as TcxGridSite).GridView is TcxGridTableView) then
    begin
        MoveSelectedRecords;
    end;
end;

procedure TfrmMethodEditor.cxGrid1TableView1DragOver(Sender, Source: TObject; X, Y: Integer;
    State: TDragState; var Accept: Boolean);
begin
    if (Source is TcxTreeList) and Assigned((Source as TcxTreeList).DragNode) then
    begin
        Accept := (self.Attribute = meaDefault) and
            (((Source as TcxTreeList).DragNode.StateIndex <> INT_IM_INDEX_METHOD) or
            ((Source as TcxTreeList).DragNode.StateIndex <> INT_IM_INDEX_ACTION) or
            ((Source as TcxTreeList).DragNode.StateIndex <> INT_IM_INDEX_WASHPROG));
        EXIT;
    end;
end;

procedure TfrmMethodEditor.DeterminePopupEnabled();
var
    xMethodLine: TMethodEditLine;
    xMethodStep: TMethodStep;
    xRelatedItemParam: TCustomSetting;
begin
    frmDesignerMain.actEditCut.Enabled := (cxGrid1TableView1.DataController.GetSelectedCount > 0);
    frmDesignerMain.actEditCopy.Enabled := (cxGrid1TableView1.DataController.GetSelectedCount > 0);

    self.mnuOpenItem.Visible := false;
    if self.cxGrid1TableView1.Controller.FocusedRowIndex <= 0 then
        EXIT;
    if not MethodEditLineExists(self.cxGrid1TableView1.Controller.FocusedRowIndex) then
        EXIT;
    xMethodLine := fMethodEditLines[self.cxGrid1TableView1.Controller.FocusedRowIndex];
    xMethodStep := xMethodLine.Value;
    xRelatedItemParam := xMethodStep.RelatedItemParam;
    if not Assigned(xRelatedItemParam) then
        EXIT;
    if (xRelatedItemParam.Value = '') then
        EXIT;
    self.mnuOpenItem.Caption := TLanguageString.Read('Open ' + xRelatedItemParam.Value,
        xRelatedItemParam.Value + ' bearbeiten');
    self.mnuOpenItem.Visible := true;
end;

procedure TfrmMethodEditor.cxGrid1TableView1SelectionChanged(Sender: TcxCustomGridTableView);
begin
    DeterminePopupEnabled();
end;

procedure TfrmMethodEditor.cxGrid1TableView1FocusedRecordChanged(Sender: TcxCustomGridTableView;
    APrevFocusedRecord, AFocusedRecord: TcxCustomGridRecord; ANewItemRecordFocusingChanged: Boolean);
begin
    if (fAddingNewRecord) then
        EXIT;
    DefineCurrentMethodStep();
end;

procedure TfrmMethodEditor.cxGrid1TableView1KeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
var
    xRecordIndex, xColumnIndex: integer;
begin

    xRecordIndex := self.cxGrid1TableView1.DataController.FocusedRecordIndex;

    if Key = VK_RETURN then
    begin
        xColumnIndex := INT_METHOD_COL_SUMMARY;
        { 28.09.10 pk commented out, confuses user. use ctrl + K to edit comments self.cxGrid1TableView1.Controller.FocusedColumn.Index , }
        EditCellProperties(xRecordIndex, xColumnIndex, ssCtrl in Shift);
    end
    else
    begin

        CheckClipboardPaste();
    end;
end;

procedure TfrmMethodEditor.DefineMenuItems(aX, aY: Integer);
var
    xHitTest: TcxCustomGridHitTest;
begin
    mnuAddEmptyLine.Visible := false;
    mnuAddActionLine.Visible := false;
    mnuDeleteLines.Visible := false;

    // Menüpunkte bestimmen
    xHitTest := cxGrid1TableView1.GetHitTest(aX, aY);

    if (xHitTest is TcxGridColumnHeaderHitTest) then
    begin
        uGridRecord := nil;
        uGridColumn := (xHitTest as TcxGridColumnHeaderHitTest).Column;

    end
    else
    begin
        if (xHitTest is TcxGridRecordHitTest) then
            uGridRecord := (xHitTest as TcxGridRecordHitTest).GridRecord // Paste in welcher Zeile
        else
            uGridRecord := nil;

        uGridColumn := nil;

        if (self.Attribute = meaDefault) then
        begin
            mnuAddEmptyLine.Visible := true;
            mnuAddActionLine.Visible := true;
            mnuDeleteLines.Visible := true;
        end;
    end;
    CheckClipboardPaste();
end;

procedure TfrmMethodEditor.FindTextInMethod(aOwner: TObject; const aText: string; aOptions: TFindOptions);
begin
    aOptions := aOptions - [frReplace];
    DoFindTextInMethod(aText, aOptions);
end;

procedure TfrmMethodEditor.ReplaceTextInMethod(aOwner: TObject; const aFindText, aReplaceText: string;
    aOptions: TFindOptions);
begin
    if (frReplaceAll in aOptions) then
        ReplaceAllTextInMethod(aFindText, aReplaceText, aOptions)
    else
        DoReplaceTextInMethod(aFindText, aReplaceText, aOptions)
end;

procedure TfrmMethodEditor.ReplaceAllTextInMethod(const aFindText, aReplaceText: string;
    aOptions: TFindOptions);
begin
    ShowMessage('noch nicht imlementiert');
end;

procedure TfrmMethodEditor.DoReplaceTextInMethod(const aFindText, aReplaceText: string;
    aOptions: TFindOptions);
begin
    // var
    // xSubStr, xFieldText, xFieldTextLook : string;
    // xRec, xVisCol, xColIndex, xTextPos, xSelLen : integer;
    // begin
    // if ( cxGrid1TableView1.Controller.EditingController.IsEditing )
    // and ( cxGrid1TableView1.Controller.EditingController.Edit is TcxCustomTextEdit ) then begin
    // with ( cxGrid1TableView1.Controller.EditingController.Edit as TcxCustomTextEdit ) do begin
    // xTextPos := SelStart + 1;
    // xSelLen := SelLength;
    // end;
    //
    // if ( frMatchCase in aOptions ) then
    // xSubStr := aFindText
    // else
    // xSubStr := UpperCase( aFindText );
    //
    // xRec := cxGrid1TableView1.Controller.FocusedRecordIndex;
    // xVisCol := cxGrid1TableView1.Controller.FocusedColumnIndex;
    //
    // xColIndex := cxGrid1TableView1.VisibleColumns[ xVisCol ].Tag;
    //
    // ASSERT( false, 'Not implemented' );
    /// /        xFieldText := self.GetStringValue( xRec, xColIndex );
    //
    // if ( frMatchCase in aOptions ) then
    // xFieldTextLook := xFieldText
    // else
    // xFieldTextLook := UpperCase( xFieldText );
    //
    // // We only replace the text if the text we want to find is highlighted
    // if Copy( xFieldTextLook, xTextPos, xSelLen ) = xSubStr then begin
    // xFieldText := Copy( xFieldText, 1, xTextPos-1 ) + aReplaceText + Copy( xFieldText, xTextPos + Length( aFindText ), Length( xFieldText ) );
    // //self.SetStringValue( xRec, xColIndex, xFieldText );
    // ASSERT( false, 'Not implemented' );
    // ChangeData();
    // //EditSetFocusToTextPart( xRec, xColIndex, xTextPos, Length( aReplaceText ) );
    // end;
    // end;
    //
    // DoFindTextInMethod( aFindText, aOptions );
end;

procedure TfrmMethodEditor.DoFindTextInMethod(const aFindText: string; aOptions: TFindOptions);
begin
    // var
    // xSubStr, xSubStrLook, xFieldText, xFieldTextLook : string;
    // xRec, xVisCol, xColIndex, xFirstRec, xFirstVisCol, xLastRec, xLastVisCol, xTextPos, xPrevTextPos, xSelStart, xSelLen: integer;
    // xReversed : boolean;
    // begin
    // xFirstRec := 0;
    // xFirstVisCol := 1;
    // xLastRec := cxGrid1TableView1.DataController.RecordCount-1;
    // xLastVisCol := cxGrid1TableView1.VisibleColumnCount-1;
    //
    // if ( frMatchCase in aOptions ) then
    // xSubStr := aFindText
    // else
    // xSubStr := UpperCase( aFindText );
    //
    // xRec := cxGrid1TableView1.Controller.FocusedRecordIndex;
    // xVisCol := cxGrid1TableView1.Controller.FocusedColumnIndex;
    //
    // while ( xRec >= xFirstRec ) and ( xRec <= xLastRec ) do begin
    // while ( xVisCol >= xFirstVisCol ) and ( xVisCol <= xLastVisCol ) do begin
    //
    // xColIndex := cxGrid1TableView1.VisibleColumns[ xVisCol ].Tag;
    // ASSERT( false, 'Not implemented' );
    // //xFieldText := self.GetStringValue( xRec, xColIndex );
    //
    // if ( frMatchCase in aOptions ) then
    // xFieldTextLook := xFieldText
    // else
    // xFieldTextLook := UpperCase( xFieldText );
    //
    // xPrevTextPos := 1;
    // xReversed := false;
    // xSubStrLook := xSubstr;
    // xSelStart := -1;
    // xSelLen := 0;
    // // get the previous cursor position
    // if  ( cxGrid1TableView1.Controller.FocusedRecordIndex = xRec )
    // and ( cxGrid1TableView1.Controller.FocusedColumn.Tag = xColIndex )
    // and ( cxGrid1TableView1.Controller.EditingController.IsEditing )
    // and ( cxGrid1TableView1.Controller.EditingController.Edit is TcxCustomTextEdit ) then begin
    // with ( cxGrid1TableView1.Controller.EditingController.Edit as TcxCustomTextEdit ) do begin
    // xSelStart := SelStart;
    // xSelLen := SelLength;
    // end;
    // end;
    // if xSelStart >= 0 then begin
    // if ( frDown in aOptions ) then begin
    // xPrevTextPos := ( xSelStart + 1 ) + xSelLen;
    // end
    // else begin
    // xPrevTextPos := Length( xFieldTextLook ) - ( xSelStart - 1 ) + xSelLen - 1;
    // end;
    // end;
    //
    // // look backwards within the text if we are searching up
    // if not ( frDown in aOptions  ) then begin
    // xSubStrLook := ReverseString( xSubstr );
    // xFieldTextLook := ReverseString( xFieldTextLook );
    // xReversed := true;
    // end;
    //
    // xTextPos := PosEx( xSubStrLook, xFieldTextLook, xPrevTextPos );
    //
    // if ( xTextPos > 0 ) then begin
    // cxGrid1TableView1.Controller.FocusedColumnIndex := xVisCol; // Wird manchmal einfach nicht gesetzt!!!
    // cxGrid1TableView1.Controller.FocusedRecordIndex := xRec;
    // cxGrid1TableView1.Controller.ClearSelection();
    // //cxGrid1TableView1.Controller.FocusedRecord.Selected := true;
    // if xReversed then
    // xTextPos := ( Length( xFieldTextLook ) - xTextPos + 1 ) - ( Length( xSubstr ) - 1 );
    //
    // // set focus to the found text
    // //self.EditSetFocusToTextPart( xRec, xColIndex, xTextPos, Length( xSubstr ) );
    // EXIT;
    // end;
    //
    // // Goto the next column
    // if ( frDown in aOptions ) then inc( xVisCol ) else dec( xVisCol );
    // end;
    //
    // // Goto the next row
    // if ( frDown in aOptions ) then begin
    // inc( xRec );
    // xVisCol := xFirstVisCol;
    // end
    // else begin
    // dec( xRec );
    // xVisCol := xLastVisCol;
    // end;
    // end;
    //
    // // EOF reached and nothing found
    // Beep;

end;

procedure TfrmMethodEditor.SelectAllData(aSender: TObject);
begin
    cxGrid1TableView1.Controller.SelectAll;
end;

function TfrmMethodEditor.GetMethodName: string;
begin
    result := fViewItem.name;
end;

procedure TfrmMethodEditor.sbAppendRecordClick(Sender: TObject);
begin
    if not CheckLastChange() then
        EXIT;
    AskActionNameAndAdd(GetInsertRecIndex());
end;

procedure TfrmMethodEditor.mnuDeleteLinesClick(Sender: TObject);
var
    xFocusedRecordIndex: integer;
begin
    if not self.IsWritable then
        EXIT;

    fAddingNewRecord := true;
    try
        if (cxGrid1TableView1.Controller.SelectedRecordCount <= 0) then
            EXIT;
        if (cxGrid1TableView1.Controller.SelectedRecordCount > 1) and
            (TDialogUtils.MessageBox(TLanguageString.Read('Delete selected rows?',
            'Sollen die makierten Zeilen gelöscht werden?'), TLanguageString.Read('Delete selection',
            'Auswahl löschen'), MB_YESNO) <> MRYES) then
            EXIT;

        xFocusedRecordIndex := self.cxGrid1TableView1.DataController.FocusedRecordIndex;
        DeleteSelectedRecords(xFocusedRecordIndex);
    finally
        fAddingNewRecord := false;
    end;
end;

procedure TfrmMethodEditor.mnuEditCommentClick(Sender: TObject);
begin
    self.EditMethodStepComment(self.cxGrid1TableView1.Controller.FocusedRowIndex);
end;

procedure TfrmMethodEditor.sbDeleteRecordClick(Sender: TObject);
begin
    ASSERT(cxGrid1.CanFocus, 'Can not set focus to method editor');
    cxGrid1.SetFocus;
    mnuDeleteLinesClick(Sender);
end;

procedure TfrmMethodEditor.sbInsertConditionClick(Sender: TObject);
begin
    InsertLoopOrCond(pcgIf);
end;

procedure TfrmMethodEditor.sbInsertForLoopClick(Sender: TObject);
begin
    InsertLoopOrCond(pcgFor);
end;

procedure TfrmMethodEditor.sbMethVaraiblesClick(Sender: TObject);
begin
    EditMethodVaraibles;
end;

procedure TfrmMethodEditor.WriteAttributeText();
begin
    edAttribute.Text := GetAttributeText();
end;

procedure TfrmMethodEditor.UpdateMethodAttributes();
var
    xAttribute: TVisibilityAttribute;
begin
    xAttribute := self.GetAttribute;
    self.WriteAttributeText();

    cxGrid1TableView1.OptionsData.Deleting := (xAttribute = meaDefault);
    cxGrid1TableView1.OptionsData.Editing := (xAttribute = meaDefault);
    sbAppendRecord.Visible := (xAttribute = meaDefault);
    sbDeleteRecord.Visible := (xAttribute = meaDefault);
    sbChangeSettings.Visible := (xAttribute = meaDefault);
    sbMethVaraibles.Visible := (xAttribute = meaDefault);

    sbInsertForLoop.Visible := (xAttribute = meaDefault);
    sbInsertCondition.Visible := (xAttribute = meaDefault);
    mnuInsertForLoopSelected.Visible := (xAttribute = meaDefault);
    mnuInsertWhileLoopSelected.Visible := (xAttribute = meaDefault);
    mnuInsertDataLoopSelected.Visible := (xAttribute = meaDefault);
    mnuInsertConditionSelected.Visible := (xAttribute = meaDefault);
    N1.Visible := (xAttribute = meaDefault);
    self.pmnuDeactivateSelectedLines.Visible := (xAttribute = meaDefault);
    self.pmnuActivateSelectedLines.Visible := (xAttribute = meaDefault);

    // nur bei Hidden
    cxGrid1.Visible := (xAttribute in [meaDefault, meaReadOnly]);
end;

function TfrmMethodEditor.GetAttribute: TVisibilityAttribute;
begin
    result := fViewItem.GetVisibilityAttribute();
end;

procedure TfrmMethodEditor.cxGrid1TableView1DataControllerAfterDelete
    (ADataController: TcxCustomDataController);
begin
    ChangeData();
end;

procedure TfrmMethodEditor.sbChangeSettingsClick(Sender: TObject);
begin
    // Editieren!
    EditMethodSettings(fViewItem.name, Memo1, false);
end;

procedure TfrmMethodEditor.sbCheckMethodClick(Sender: TObject);
begin
    self.Build();
end;

procedure TfrmMethodEditor.EditSetFocusToRow(aRow: integer);
begin
    if not MethodEditLineExists(aRow) then
        EXIT;
    // Set Focus to grid: if the grid does not have focus, no editing can occur and no Edit control is created. An edit control
    // is needed for selecting a part of a text.
    if cxGrid1.CanFocus then
        cxGrid1.SetFocus;

    // Set focus to row
    cxGrid1TableView1.DataController.FocusedRecordIndex := aRow;

    cxGrid1TableView1.Controller.ClearSelection();
    cxGrid1TableView1.DataController.ChangeRowSelection(aRow, true);
end;

procedure TfrmMethodEditor.EditSetFocusToTextPart(const aRow: integer; const aSettingKey: TArray<string>;
    const aFocusStart, aFocusLen: integer);
var
    xMethodLine: TMethodEditLine;
    xMethodStep: TMethodStep;
    xFocusOptions: TEdActionPropsFocusOptions;
begin
    if not MethodEditLineExists(aRow) then
        EXIT;

    xMethodLine := fMethodEditLines[aRow];
    xMethodStep := xMethodLine.Value;

    EditSetFocusToRow(aRow);

    xFocusOptions.SettingKey := aSettingKey;
    xFocusOptions.FocusStart := aFocusStart;
    xFocusOptions.FocusLen := aFocusLen;

    if TfrmActionProps.InstanceEditMethodStepSpecialFocusModal(xMethodStep,
        TViewItemsWorkflow.Instance.ActionImages.ActionImages64, self.IsWritable, xFocusOptions) = mrOK then
    begin
        self.ChangeData();
    end;
end;

procedure TfrmMethodEditor.PopupMenu1Popup(Sender: TObject);
begin
    DeterminePopupEnabled();
end;

procedure TfrmMethodEditor.AddMethodStepAndSelect(const aMethodStep: TMethodStep; aIndex: integer);
begin
    AddMethodStepLine(aMethodStep, aIndex, false);
    cxGrid1TableView1.DataController.ChangeRowSelection(aIndex, true);
end;

procedure TfrmMethodEditor.InsertLoopOrCond(aDlgType: TLoopOrConditionType);
var
    xFirstIndex, xLastIndex, xBeginGroupIndex, xEndGroupIndex, xBodyLineCount: integer;
    xSelectedRows: array of integer;
    xDataLink: TMethodStepDataLink;
    xMethodStep1, xMethodStep2: TMethodStep;
begin
    if (cxGrid1TableView1.Controller.SelectedRecordCount > 0) then
    begin
        xFirstIndex := cxGrid1TableView1.Controller.SelectedRecords[0].RecordIndex;
        xLastIndex := cxGrid1TableView1.Controller.SelectedRecords
            [cxGrid1TableView1.Controller.SelectedRecordCount - 1].RecordIndex;

        // we want to insert the line UNDER the last selected line.
        xLastIndex := xLastIndex + 1;
    end
    else
    begin
        xFirstIndex := GetInsertRecIndex();
        xLastIndex := xFirstIndex;
    end;

    CheckRangeLineLevels(xFirstIndex, xLastIndex - 1);

    if not CheckLastChange() then
        EXIT;

    cxGrid1TableView1.Controller.ClearSelection();
    xBodyLineCount := xLastIndex - xFirstIndex;

    case aDlgType of // das sollte noch in den DataAdaptor verschoben werden
        pcgIf:
            begin
                xDataLink := TRunStepBuilderTypeDictionary.Instance.DefaultDataLink;
                xMethodStep1 := TIfMethodStep.Create('', xDataLink, nil);
                xMethodStep2 := TEndIfMethodStep.Create('', xDataLink, nil);
            end;
        pcgWhile:
            begin
                xDataLink := TRunStepBuilderTypeDictionary.Instance.DefaultDataLink;
                xMethodStep1 := TWhileMethodStep.Create('', xDataLink, nil);
                xMethodStep2 := TEndWhileMethodStep.Create('', xDataLink, nil);
            end;
        pcgFor:
            begin
                xDataLink := TRunStepBuilderTypeDictionary.Instance.DefaultDataLink;
                xMethodStep1 := TForMethodStep.Create('', xDataLink, nil);
                xMethodStep2 := TEndForMethodStep.Create('', xDataLink, nil);
            end;
        else
            raise Exception.Create('Unknown loop op condition type');
    end;

    // neue Actions im Editor einfügen
    SetLength(xSelectedRows, 2);
    xBeginGroupIndex := xFirstIndex;
    xEndGroupIndex := xBeginGroupIndex + 1 + xBodyLineCount;
    AddMethodStepAndSelect(xMethodStep1, xBeginGroupIndex);
    MoveLinesToGroup(xBeginGroupIndex, xBeginGroupIndex + 1, xEndGroupIndex - 1);
    AddMethodStepAndSelect(xMethodStep2, xEndGroupIndex);

    // Eigenschaften-Fenster zeigen
    self.EditMethodStepProperties(xFirstIndex);
    ForceGridResize;
end;

procedure TfrmMethodEditor.InsertDataLoop;
var
    xResult: TModalResult;
    xFirstIndex, xLastIndex, xBeginGroupIndex, xEndGroupIndex, xBodyLineCount: integer;
    xSelectedRows: array of integer;
    frmParserCodeGenerateDlg: TfrmParserCodeGenerateDlg;
    xNames: TArray<string>;
    xMethodStep: TMethodStep;
begin
    if (cxGrid1TableView1.Controller.SelectedRecordCount > 0) then
    begin
        xFirstIndex := cxGrid1TableView1.Controller.SelectedRecords[0].RecordIndex;
        xLastIndex := cxGrid1TableView1.Controller.SelectedRecords
            [cxGrid1TableView1.Controller.SelectedRecordCount - 1].RecordIndex;

        // we want to insert the line UNDER the last selected line.
        xLastIndex := xLastIndex + 1;
    end
    else
    begin
        xFirstIndex := GetInsertRecIndex();
        xLastIndex := xFirstIndex;
    end;

    CheckRangeLineLevels(xFirstIndex, xLastIndex - 1);

    frmParserCodeGenerateDlg := TfrmParserCodeGenerateDlg.Create(nil);
    try
        xNames := TImportDefDataAdaptor.ReadAllDefNames(INT_DEF_MODE_METHVARIMPORT);
        TControlUtils.AddValuesToComboBox(xNames, frmParserCodeGenerateDlg.cmbDataDefName, true);

        xResult := frmParserCodeGenerateDlg.ShowDialog();
        if xResult <> mrOK then
            EXIT;

        if not CheckLastChange() then
            EXIT;

        cxGrid1TableView1.Controller.ClearSelection();
        xBodyLineCount := xLastIndex - xFirstIndex;

        SetLength(xSelectedRows, 7);

        xMethodStep := frmParserCodeGenerateDlg.CreateDataOpenLine();
        AddMethodStepAndSelect(xMethodStep, xFirstIndex);

        xMethodStep := frmParserCodeGenerateDlg.CreateDataInitVarLine();
        AddMethodStepAndSelect(xMethodStep, xFirstIndex + 1);

        // Begin
        xBeginGroupIndex := xFirstIndex + 2;
        xEndGroupIndex := xBeginGroupIndex + 2 + xBodyLineCount;
        xMethodStep := frmParserCodeGenerateDlg.CreateDataBeginLoopLine();
        AddMethodStepAndSelect(xMethodStep, xBeginGroupIndex);

        xMethodStep := frmParserCodeGenerateDlg.CreateDataReadLine();
        AddMethodStepAndSelect(xMethodStep, xBeginGroupIndex + 1);

        MoveLinesToGroup(xBeginGroupIndex, xBeginGroupIndex + 1, xEndGroupIndex - 1);

        xMethodStep := frmParserCodeGenerateDlg.CreateDataIncrementLine();
        AddMethodStepAndSelect(xMethodStep, xEndGroupIndex);

        xMethodStep := frmParserCodeGenerateDlg.CreateDataEndLoopLine;
        AddMethodStepAndSelect(xMethodStep, xEndGroupIndex + 1);

        xMethodStep := frmParserCodeGenerateDlg.CreateDataCloseLine();
        AddMethodStepAndSelect(xMethodStep, xEndGroupIndex + 2);

        ForceGridResize;
    finally
        frmParserCodeGenerateDlg.Free;
    end;
end;

procedure TfrmMethodEditor.mnuInsertWhileLoopSelectedClick(Sender: TObject);
begin
    InsertLoopOrCond(pcgWhile);
end;

procedure TfrmMethodEditor.mnuInsertForLoopSelectedClick(Sender: TObject);
begin
    InsertLoopOrCond(pcgFor);
end;

procedure TfrmMethodEditor.mnuOpenItemClick(Sender: TObject);
begin
    self.OpenMethodStepRelatedEditor(self.cxGrid1TableView1.Controller.FocusedRowIndex);
end;

procedure TfrmMethodEditor.mnuOpenPropertiesClick(Sender: TObject);
begin
    self.EditMethodStepPropertiesOrComment(self.cxGrid1TableView1.Controller.FocusedRowIndex);
end;

procedure TfrmMethodEditor.mnuInsertConditionSelectedClick(Sender: TObject);
begin
    InsertLoopOrCond(pcgIf);
end;

procedure TfrmMethodEditor.mnuInsertDataLoopSelectedClick(Sender: TObject);
begin
    InsertDataLoop();
end;

function TfrmMethodEditor.GetLineStartX(const aMethodLine: TMethodEditLine): integer;
var
    xLevel: integer;
begin
    xLevel := GetLineLevel(aMethodLine);
    result := (xLevel) * cIndentWidth;
end;

function TfrmMethodEditor.GetLineIconX(const aMethodLine: TMethodEditLine): integer;
begin
    result := GetLineStartX(aMethodLine) + cIndentWidth;
end;

function TfrmMethodEditor.IsXOnLineIcon(const aX: integer; const aMethodLine: TMethodEditLine): boolean;
var
    xIconX: integer;
begin
    xIconX := GetLineIconX(aMethodLine) + cFirstXOffset;

    result := { ( aX > xIconX ) and } (aX < (xIconX + 16));
end;

procedure TfrmMethodEditor.cxGrid1TableView1MouseDown(Sender: TObject; Button: TMouseButton;
    Shift: TShiftState; X, Y: Integer);
var
    xHitTest: TcxCustomGridHitTest;
    xRecordIndex: integer;
begin
    if (Button = mbRight) then
    begin
        self.DefineMenuItems(X, Y);
    end
    else if (button = mbleft) and (not(ssDouble in Shift)) then
    begin

        if (self.Attribute <> meaDefault) then
            EXIT;

        xHitTest := (Sender as TcxGridSite).ViewInfo.GetHitTest(X, Y);
        if (xHitTest is TcxGridRecordCellHitTest) then
        begin
            xRecordIndex := (xHitTest as TcxGridRecordCellHitTest).GridRecord.RecordIndex;
            if not MethodEditLineExists(xRecordIndex) then
                EXIT;

            // cxGrid1.BeginDrag( false ); This doesn't kill the multi-select, but pullfocusing is not released at the end of the drag.
            (Sender as TcxGridSite).BeginDrag(false); // works. but kills the multiselect
        end
    end;
end;

procedure TfrmMethodEditor.ShowHintForMethodStep(const aMethodStep: TMethodStep; const aHint: TBalloonHint);
var
    xPoint: TPoint;
begin
    aHint.HideHint;
    if not Assigned(aMethodStep) then
        EXIT;
    if aMethodStep.InactiveAsBool then
        EXIT;

    aHint.Title := aMethodStep.SummaryFirstLine;
    aHint.Description := aMethodStep.SummaryParamters;
    aHint.Description := aHint.Description + #13#10 + #13#10 + #13#10 + #13#10;
    aHint.ImageIndex := aMethodStep.StepInfo.IconIndex;
    // xRect := aButtonItem.Bounds;

    xPoint := Mouse.CursorPos;
    // OffsetRect( xRect, xPoint.X, xPoint.Y );
    aHint.HideAfter := -1;

    aHint.ShowHint(xPoint);
end;

procedure TfrmMethodEditor.cxGrid1TableView1MouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
const
    cXTolerance = 3;
    cYTolerance = 3;
var
    xMethodStep: TMethodStep;
    xMethodLine: TMethodEditLine;
    xRecordIndex: integer;
    xHitTest: TcxCustomGridHitTest;
    xPoint: TPoint;
begin
    xPoint.X := X;
    xPoint.Y := Y;

    if (Abs(fPrevMousePos.X - xPoint.X) <= cXTolerance) and
        (Abs(fPrevMousePos.Y - xPoint.Y) <= cYTolerance) then
        EXIT;
    fPrevMousePos := xPoint;
    xHitTest := (Sender as TcxGridSite).ViewInfo.GetHitTest(X, Y);
    if not(xHitTest is TcxGridRecordCellHitTest) then
    begin
        self.BalloonHint1.HideHint;
        EXIT;
    end;

    xRecordIndex := (xHitTest as TcxGridRecordCellHitTest).GridRecord.RecordIndex;

    self.BalloonHint1.HideHint;
    if not MethodEditLineExists(xRecordIndex) then
        EXIT;
    xMethodLine := fMethodEditLines[xRecordIndex];
    if not self.IsXOnLineIcon(X, xMethodLine) then
        EXIT;

    xMethodStep := xMethodLine.Value;

    ShowHintForMethodStep(xMethodStep, self.BalloonHint1);
end;

procedure TfrmMethodEditor.cxGrid1TableView1CanSelectRecord(Sender: TcxCustomGridTableView;
    ARecord: TcxCustomGridRecord; var AAllow: Boolean);
var
    xRecordIndex: integer;
    xMethodLine: TMethodEditLine;
    xGroupMatchingLineIndex: integer;
begin
    AAllow := false;
    xRecordIndex := ARecord.RecordIndex;
    if not MethodEditLineExists(xRecordIndex) then
        EXIT;

    AAllow := true;
    xMethodLine := fMethodEditLines[xRecordIndex];

    if (not fIsSelecting) and (xMethodLine is TMethodEditGroupEndLine) then
    begin
        xGroupMatchingLineIndex := FindLineIndexOf((xMethodLine.Parent));
        AAllow := self.cxGrid1TableView1.DataController.IsRowSelected(xGroupMatchingLineIndex);
        EXIT;
    end;

    if xMethodLine is TMethodEditGroupBeginLine then
    begin
        xGroupMatchingLineIndex := FindLineIndexOf((xMethodLine as TMethodEditGroupBeginLine).EndLine);
        fIsSelecting := true;
        try
            if xGroupMatchingLineIndex >= 0 then
            begin
                self.cxGrid1TableView1.DataController.SelectRows(xGroupMatchingLineIndex,
                    xGroupMatchingLineIndex);
            end;
        finally
            fIsSelecting := false;
        end;
    end;
end;

procedure TfrmMethodEditor.EditCellProperties(const aRecordIndex: integer; const aColIndex: integer;
    const aEditRelatedProperties: boolean);
begin
    if aColIndex = INT_METHOD_COL_SUMMARY then
    begin
        if aEditRelatedProperties then
        begin
            self.OpenMethodStepRelatedEditor(aRecordIndex);
        end
        else
        begin
            self.EditMethodStepPropertiesOrComment(aRecordIndex);
        end;
    end
    else if aColIndex = INT_METHOD_COL_COMMENT then
    begin
        self.EditMethodStepComment(aRecordIndex);
    end;
end;

procedure TfrmMethodEditor.cxGrid1TableView1CellDblClick(Sender: TcxCustomGridTableView;
    ACellViewInfo: TcxGridTableDataCellViewInfo; AButton: TMouseButton; AShift: TShiftState;
    var AHandled: Boolean);
var
    xColIndex: integer;
    xRecordIndex: integer;
    xMethodLine: TMethodEditLine;
begin
    xColIndex := (ACellViewInfo.Item as TcxGridColumn).Index;
    xRecordIndex := aCellViewinfo.GridRecord.RecordIndex;

    if not MethodEditLineExists(xRecordIndex) then
        EXIT;
    xMethodLine := fMethodEditLines[xRecordIndex];

    if self.IsXOnLineIcon(fPrevMousePos.X, xMethodLine) then
        self.OpenMethodStepRelatedEditor(xRecordIndex)
    else
        EditCellProperties(xRecordIndex, xColIndex, ssCtrl in AShift);
end;

class function TfrmMethodEditor.CalculateTextHeight(const aText: string; const aCanvas: TCxCanvas;
    const aPossibleCellWidth: integer): integer;
var
    xRect: TRect;
begin
    xRect := Rect(0, 0, aPossibleCellWidth, 0);
    aCanvas.TextExtent(aText, xRect, cxWordBreak);
    result := xRect.Bottom - xRect.Top;
end;

function TfrmMethodEditor.ShowMethodStepCommentInColumn(const aMethodStep: TMethodStep): integer;
begin
    if (self.ColumnMode = mcmOneColumn) or (aMethodStep is TBlankMethodStep) then
        result := INT_METHOD_COL_SUMMARY
    else
        result := INT_METHOD_COL_COMMENT;
end;

procedure TfrmMethodEditor.cxGrid1TableView1GetCellHeight(Sender: TcxCustomGridTableView;
    ARecord: TcxCustomGridRecord; AItem: TcxCustomGridTableItem; ACellViewInfo: TcxGridTableDataCellViewInfo;
    var AHeight: Integer);
var
    xRecordIndex: integer;
    xMethodLine: TMethodEditLine;
    xMethodStep: TMethodStep;
    xText: string;
    xHeight: integer;
    xShowMethodStepCommentInThisCol: boolean;
begin
    xRecordIndex := ARecord.RecordIndex;
    if xRecordIndex = GetNewItemRowIndex() then
    begin
        AHeight := CalculateTextHeight('X', aCellViewInfo.GridView.Painter.Site.Canvas, aCellViewInfo.Width);
        AHeight := AHeight * 4;
    end;

    if not MethodEditLineExists(xRecordIndex) then
        EXIT;

    xMethodLine := fMethodEditLines[xRecordIndex];
    xMethodStep := xMethodLine.Value;

    AHeight := 0;

    xShowMethodStepCommentInThisCol :=
        (ACellViewInfo.Item.Index = ShowMethodStepCommentInColumn(xMethodStep));

    if xShowMethodStepCommentInThisCol then
    begin
        xText := xMethodStep.M_Comments;
        xHeight := CalculateTextHeight(xText, aCellViewInfo.GridView.Painter.Site.Canvas,
            aCellViewInfo.Width);
        AHeight := AHeight + xHeight;
    end;

    if ACellViewInfo.Item.Index = INT_METHOD_COL_SUMMARY then
    begin
        xText := GetCompleteSummaryText(xMethodStep);
        xHeight := CalculateTextHeight(xText, aCellViewInfo.GridView.Painter.Site.Canvas,
            aCellViewInfo.Width);
        AHeight := AHeight + xHeight;
    end;

    // atleast one line
    if AHeight = 0 then
    begin
        AHeight := CalculateTextHeight('X', aCellViewInfo.GridView.Painter.Site.Canvas, aCellViewInfo.Width);
    end;

    AHeight := AHeight + 2;

    if AHeight < fActionImages.Height then
        AHeight := fActionImages.Height;

end;

procedure TfrmMethodEditor.DrawComment(const aCanvas: TcxCanvas; const aMethodLine: TMethodEditLine;
    const aIsSummaryCol: boolean; const aIsSelected: boolean; var vRect: TRect);
var
    xText: string;
    xSavedColor: TColor;
    xMethodStep: TMethodStep;
begin

    xMethodStep := aMethodLine.Value;
    xText := xMethodStep.M_Comments;
    if xText = '' then
        EXIT;

    xSavedColor := aCanvas.Font.Color;
    try
        if (not aIsSelected) and (not xMethodStep.InactiveAsBool) then
        begin
            aCanvas.Font.Color := clPurple;
        end;

        aCanvas.DrawText(xText, Rect(vRect.Left, vRect.Top, vRect.Right, vRect.Bottom), cxWordBreak);

        vRect.Top := vRect.Top + CalculateTextHeight(xText, aCanvas, vRect.Right - vRect.Left) + 1;
        aCanvas.Font.Style := [];
    finally
        aCanvas.Font.Color := xSavedColor;
    end;
end;

procedure TfrmMethodEditor.DrawGroupBrackets(const aCanvas: TcxCanvas; const aMethodLine: TMethodEditLine;
    const aRect: TRect);
var
    xSavedColor: TColor;
    xColor: TColor;
    xTempRect, xTempRect2: TRect;
    x: integer;
    xLineLevel: integer;
begin
    xLineLevel := GetLineLevel(aMethodLine);

    xSavedColor := aCanvas.Brush.Color;
    try
        for x := 1 to xLineLevel + 1 do
        begin
            xTempRect := aRect;

            if x mod 2 = 0 then
            begin
                xColor := clMoneyGreen;
            end
            else
            begin
                xColor := clSkyBlue;
            end;

            xTempRect.Left := aRect.Left + ((x) * cIndentWidth) + 4;
            xTempRect.Right := xTempRect.Left + 3;

            if x = xLineLevel + 1 then
            begin
                if (aMethodLine is TMethodEditGroupBeginLine) or (aMethodLine is TMethodEditGroupEndLine) then
                begin
                    xTempRect2 := xTempRect;

                    xTempRect2.Left := xTempRect2.Left;
                    xTempRect2.Right := xTempRect2.Left + 16;

                    if (aMethodLine is TMethodEditGroupBeginLine) then
                    begin
                        xTempRect2.Top := xTempRect2.Top + 4;
                        xTempRect.Top := xTempRect2.Top;

                        xTempRect2.Bottom := xTempRect2.Top + 6;
                    end
                    else if (aMethodLine is TMethodEditGroupEndLine) then
                    begin
                        xTempRect2.Top := xTempRect2.Top + 8;

                        xTempRect2.Bottom := xTempRect2.Top + 4;
                        xTempRect.Bottom := xTempRect2.Bottom;
                    end;

                    aCanvas.FillRect(xTempRect2, xColor);
                    aCanvas.FillRect(xTempRect, xColor);
                end;

            end
            else
            begin
                aCanvas.FillRect(xTempRect, xColor);
            end;
        end;

    finally
        aCanvas.Brush.Color := xSavedColor;
    end;
end;

procedure TfrmMethodEditor.DrawIconOrShape(const aCanvas: TcxCanvas; const aMethodLine: TMethodEditLine;
    const aIsSelected: boolean; var vRect: TRect);
var
    xMethodStep: TMethodStep;
begin
    xMethodStep := aMethodLine.Value;

    if not(xMethodStep.InactiveAsBool) then
    begin
        if (not xMethodStep.IsCodeStep) or (xMethodStep is TParamStoreMethodStep) then
        begin
            aCanvas.DrawImage(fActionImages, vRect.Left, vRect.Top, xMethodStep.StepInfo.IconIndex);
        end;
    end;

    vRect.Left := vRect.Left + fActionImages.Width + 5;
end;

procedure TfrmMethodEditor.DrawSummaryText(const aCanvas: TcxCanvas; const aMethodLine: TMethodEditLine;
    const aIsSelected: boolean; var vRect: TRect);
var
    xText: string;
    xSavedColor: TColor;
    xMethodStep: TMethodStep;
    xLeft: integer;
begin

    xMethodStep := aMethodLine.Value;
    xLeft := vRect.Left;
    if not xMethodStep.IsCodeStep then
    begin
        xSavedColor := aCanvas.Font.Color;
        try
            if (not aIsSelected) and (not xMethodStep.InactiveAsBool) then
            begin
                aCanvas.Font.Color := clBlue;
            end;

            if (xMethodStep.InactiveAsBool) then
            begin
                aCanvas.Font.Color := clDkGray;
            end;
            xText := xMethodStep.GetActionCaptionText();
            aCanvas.DrawText(xText, Rect(xLeft, vRect.Top, vRect.Right, vRect.Bottom), cxWordBreak);

            aCanvas.Font.Style := [];
            xLeft := xLeft + aCanvas.TextWidth(xText) + 1;
        finally
            aCanvas.Font.Color := xSavedColor;
        end;
    end;

    xText := xMethodStep.OptionSummary;
    aCanvas.DrawText(xText, Rect(xLeft, vRect.Top, vRect.Right, vRect.Bottom), cxWordBreak);
    vRect.Top := vRect.Top + CalculateTextHeight(xText, aCanvas, vRect.Right - vRect.Left) + 1;
end;

procedure TfrmMethodEditor.cxGrid1TableView1CustomDrawCell(Sender: TcxCustomGridTableView; ACanvas: TcxCanvas;
    AViewInfo: TcxGridTableDataCellViewInfo; var ADone: Boolean);
var
    xRect: TRect;
    xRecordIndex: integer;
    xMethodStep: TMethodStep;
    xMethodLine: TMethodEditLine;
    xTempRect: TRect;
begin
    if not Assigned(aViewInfo.RecordViewInfo) then
        EXIT;

    xRecordIndex := aViewInfo.RecordViewInfo.GridRecord.RecordIndex;

    if xRecordIndex = GetNewItemRowIndex() then
    begin
        if aViewInfo.Item.Index = INT_METHOD_COL_SUMMARY then
        begin
            if not aViewInfo.Selected then
            begin
                aCanvas.Brush.Color := clWhite;
                ACanvas.FillRect(aViewInfo.Bounds);

                xRect := aViewInfo.ContentBounds;
                xRect.Left := xRect.Left + 5;
                aCanvas.Font.Style := [fsBold];
                aCanvas.DrawText('. . .', xRect, cxAlignVCenter);
                aDone := true;
            end;
        end;
        EXIT;
    end;

    if not MethodEditLineExists(xRecordIndex) then
        EXIT;

    xMethodLine := fMethodEditLines[xRecordIndex];
    xMethodStep := xMethodLine.Value;

    // clear old drawings
    if not aViewInfo.Selected then
    begin
        aCanvas.Brush.Color := clWhite;
        if (xMethodStep.InactiveAsBool) then
        begin
            aCanvas.Brush.Color := clLtGray;
        end;
    end;

    if (xMethodStep.InactiveAsBool) then
    begin
        aCanvas.Font.Color := clDkGray;
    end;

    ACanvas.FillRect(aViewInfo.Bounds);

    xRect := aViewInfo.ContentBounds;

    // Kommentarzeile purple
    if aViewInfo.Item.Index = INT_METHOD_COL_COMMENT then
    begin
        if ShowMethodStepCommentInColumn(xMethodStep) = INT_METHOD_COL_COMMENT then
        begin

            xRect.Left := xRect.Left + 2;
            DrawComment(aCanvas, xMethodLine, false, aViewInfo.Selected, xRect);
        end;
    end
    else if aViewInfo.Item.Index = INT_METHOD_COL_SUMMARY then
    begin

        if not(xMethodStep.InactiveAsBool) then
        begin
            DrawGroupBrackets(aCanvas, xMethodLine, xRect);
        end;

        xRect.Left := xRect.Left + self.GetLineIconX(xMethodLine);

        // Comment
        if ShowMethodStepCommentInColumn(xMethodStep) = INT_METHOD_COL_SUMMARY then
        begin
            xTempRect := xRect;
            xTempRect.Left := xTempRect.Left + fActionImages.Width + 5;
            DrawComment(aCanvas, xMethodLine, true, aViewInfo.Selected, xTempRect);
            xRect.Top := xTempRect.Top;
        end;

        // Icon
        DrawIconOrShape(aCanvas, xMethodLine, aViewInfo.Selected, xRect);

        // Summary Text
        DrawSummaryText(aCanvas, xMethodLine, aViewInfo.Selected, xRect);
    end;

    aDone := true;
end;

procedure TfrmMethodEditor.SetSelectedLinesInactive(const aInactive: boolean);
var
    x: integer;
    xRecIndex: integer;
begin
    for x := 0 to cxGrid1TableView1.Controller.SelectedRecordCount - 1 do
    begin
        xRecIndex := cxGrid1TableView1.Controller.SelectedRecords[x].RecordIndex;
        if not MethodEditLineExists(xRecIndex) then
            CONTINUE;

        fMethodEditLines[xRecIndex].Value.InactiveAsBool := aInactive;
        if (x = 0) then
        begin
            self.ChangeData;
            self.cxGrid1TableView1.Invalidate();
        end;
    end;
end;

procedure TfrmMethodEditor.pmnuDeactivateSelectedLinesClick(Sender: TObject);
begin
    SetSelectedLinesInactive(true);
end;

procedure TfrmMethodEditor.pmnuActivateSelectedLinesClick(Sender: TObject);
begin
    SetSelectedLinesInactive(false);
end;

procedure TfrmMethodEditor.mnuGotoLineClick(Sender: TObject);
var
    xLineNumberAsStr: string;
    xLineNumber: integer;
begin
    xLineNumberAsStr := IntToStr(1);

    if not TDialogUtils.InputQuery(TLanguageString.Read('Enter line number', 'Zeilennummer eingeben'),
        TLanguageString.Read('Goto Line', 'Gehe zu Zeile'), xLineNumberAsStr) then
        EXIT;
    if (not TryStrToInt(xLineNumberAsStr, xLineNumber)) or (xLineNumber <= 0) or
        (xLineNumber > self.cxGrid1TableView1.DataController.RecordCount) then
        raise Exception.Create(TLanguageString.Read('The value entered [{0}] is not a valid line number',
            'Die eingegebene Zeilennummer [{0}] ist ungültig', [xLineNumberAsStr]));

    cxGrid1TableView1.DataController.FocusedRecordIndex := xLineNumber - 1;
    cxGrid1TableView1.Controller.ClearSelection();
    cxGrid1TableView1.DataController.ChangeRowSelection
        (cxGrid1TableView1.DataController.FocusedRecordIndex, true);
end;

procedure TfrmMethodEditor.SaveStatusChanged();
begin
    if fSaveStatus = efssDataChanged then
    begin
        self.edModifiedStatus.Text := TLanguageString.Read('Modified', 'Geändert');
    end
    else
    begin
        self.edModifiedStatus.Text := '';
    end;
end;

procedure TfrmMethodEditor.CustomizeView;
begin
    if (fColumnMode = mcmTwoColumns) then
    begin
        cxGrid1TableView1.Columns[INT_METHOD_COL_COMMENT].Visible := true;
        cxGrid1TableView1.Columns[INT_METHOD_COL_SUMMARY].Width := cxGrid1.Width div 2;
    end
    else
    begin
        cxGrid1TableView1.Columns[INT_METHOD_COL_COMMENT].Visible := false;
    end;
end;

procedure TfrmMethodEditor.SaveData;
begin
    gGUIManager.SetCursor(crHourglass);
    try
        SaveDataIntern;
        frmDesignerMain.RefreshButtons();
    finally
        gGUIManager.SetCursor(crDefault);
    end;
end;


end.
