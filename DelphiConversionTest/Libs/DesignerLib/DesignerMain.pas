{ --------------------------------------------------------------------------------------------------
  EDITOR (Ebene 5)
  --------------------------------------------------------------------------------------------------
  Globale Variablen, Funktionen und Prozeduren für den Editor
  --------------------------------------------------------------------------------------------------
  Datum    op  function/procedure     Änderung / Neuerung
  -------- --  -------------------    --------------------------------------------------------------
  31.05.00 wl                         als EdMain in EDITOR
  05.06.00 wl  FormCreate             Aufruf von InitWinLissyProjects für Seriennummer
  21.06.00 wl  mnuScriptSaveAsClick,mnuScriptDeleteClick - überarbeitet
  04.07.00 wl  DBGridScheduleDblClick  nach der Rückkehr aus UFuncsel wird der Focus gesetzt
  05.07.00 wl  UpdateTV               Methoden werden mit aufgelistet
  05.07.00 wl  Layout-Ansicht --> EdLayout  &  Script-Editor --> EdScrip
  12.07.00 mo  OpenSeq                Form schliessen vor erneutem Öffnen
  12.07.00 mo  mnuWPOpenClick         Form schliessen vor erneutem Öffnen
  12.07.00 mo  TVClick                SetFocus auf Layout um Seq Grid in Editiermodus zu bringen
  18.07.00 wl                         neue Menü-Punkte: Import, View Run List, Position Information, ...
  18.07.00 wl                         Neu: Button-Leiste
  19.07.00 wl                         Unterstützung der rechten Maustaste für Scripte fertig
  21.07.00 wl                         Unterstützung der rechten Maustaste für Sequence,Washpr. fertig
  26.07.00 wl                         Menüpunkte Sequence/Washprogram entfernt
  26.07.00 wl                         Verallgemeinerung New-, Delete- und SaveAs-Funktion --> qryTools
  08.08.00 wl  OpenWashpr             Fenster wird auch bei noch nicht vorhandenen Programmen gezeigt
  08.08.00 wl  OpenSeq                es geht auch ohne WB.Layoutname
  10.08.00 wl                         Script-,Reset-Button weg; edMethod --> EdScript
  10.08.00 wl  mnuScriptSelect        komplett raus!
  11.08.00 wl                           --> String in Ressourcen
  01.09.00 wl                         neu: edMethod-Label, Print- und Reset-Button
  01.09.00 wl                         LayoForm ersetzt EdLayout, ScheEdit ersetzt EdScrip
  01.09.00 wl  OpenWashpr,OpenSeq     Aufruf geändert, damit beim Schließen OnDestroy durchlaufen wird
  04.09.00 wl  pmnu1Popup             bestimmt TreeNode "FRightSelected"
  04.09.00 wl  pmnuDeleteClick,pmnuSaveAsClick,pmnuNewClick beziehen sich auf FRightSelected statt TV.Selected
  04.09.00 wl  OpenWashpr,OpenSeq     Funktionen vereinfacht - laufen jetzt besser
  06.09.00 wl  pmnuNewClick           Sequenz- und Wascheditorfenster werden vor Neuaufruf geschlossen
  08.06.01 mo  mnuScriptResetClick    aufruf gmScriptReset Parameter hinzugefügt
  12.06.02 pk                         TN1052 div Änderungen Scheduler
  24.06.02 pk  mnsScriptSaveAsClick   TN1052 prevent the Unsaved Schedule from begin saved
  25.06.02 pk  mnuScriptSaveAsClick   TN1052 Exit without doing anything
  25.06.02 pk  pmnu1Popup             TN1052 Do not show SaveAs
  14.07.02 pk  UpdateTV               New functions for adding children to list
  19.07.02 pk  mnuScriptNewClick      Call AskForNewScriptName function to get Valid Script Name
  19.07.02 pk                         New functions for History Menu.
  10.10.02 wl                               TN1293.2 Ini Access - uses geändert
  18.10.02 wl  FormCreate             TN1293.1 Aufruf von InitSamGlobals1 --> Projekt-Quelltext
  18.10.02 wl                         TN1293.1 benutzt TAppSettings
  14.11.02 wl                         TN1328.1 uses TAppSettings.UsesScheduler
  30.01.03 mo  UpdateTV               TN1416.1  Erster Eintrag wir immer selectiert
  18.02.03 tbh mnuScriptPrintClick    TN1413 Ressource korrigiert
  29.10.03 wl                         TN1641   etliche neue Menüpunkte - für Methoden
  29.10.03 wl  TVClick                TN1641   Methoden haben jetzt gleiche Funktionalität wie Scripte
  30.10.03 wl  UserSetAccess          TN1641   neu: User Management im Editor
  30.10.03 wl  FormShow               TN1641   GetMethod-Dialog ohne Select- und Start-Button
  12.10.03 pk                         TN1641   Schedule menu had been deleted by accident
  24.02.04 pk  FormShow               TN1753   New way of creating the layout form
  26.02.04 wl  mnuScriptPrintClick    TN1574   Erzeugen von Quick-Report-Fenster in try-except-Block
  26.02.04 wl  mnuToolsLiqHClick      TN1574   Geänderter Aufruf von LiqPar
  28.04.04 wl  FormCreate             TN1788   erzeugt TEditorModules
  13.05.04 pk                         TN1917   New: Import Wizard menu
  24.05.04 wl  mnuImportSequencesClick TN1945  neu: Import von Sequenzen
  24.05.04 wl  pmnuNewClick            TN1945  benutzt TSequenceManager
  21.07.04 pk  mnuScheduleDesignClick TN2049   use TEdMainLinkAll instead of edMainLinkAll
  06.08.04 wl                         TN2008.1  erzeugt TMDIMethEditForm und TMDIScheduleEditForm
  06.08.04 wl                         TN2008.2  Form is now Docking Main Form instead of MDIForm
  06.08.04 wl                         TN2008.2  Treeview with all functionality --> EdExplorer
  09.08.04 wl                         TN2008.2  Docking verbessert
  17.08.04 wl  pmnuCloseClick         TN2008.2  Close-Menu: halbgare Lösung: ruft nicht FormClose der Fenster auf
  17.08.04 wl                         TN2008.2  neue Menu items in View menu
  19.08.04 wl  mnuMethodEditClick     TN2008.3  ruft TEditorMethEditForm auf
  19.08.04 wl  mnuMethodSaveAsClick   TN1780    ruft global.MethodSaveAs auf
  19.08.04 wl  mnuMethodBuildClick    TN2102    Der Build-Button tut jetzt dasselbe wie der Build-Button des MethEdit-Fensters
  24.08.04 wl                         TN2008.2  mit Laden und Speichern der DockableForms
  03.12.04 wl                         TN2008.2  alles bezüglich Script entfernt
  03.12.04 wl  mnuToolsLayout         TN2254.2  ruft global.ChangeLayout auf (d.h. kann auch für Methoden neues Layout laden)
  03.12.04 wl  mnuMethodNewClick      TN2254.2  ruft global.CreateNewMethod auf
  09.12.04 wl                         TN2246.4  benutzt global.CreateRunForm
  13.12.04 wl  mnuRunTableClick       TN2254.4  funktioniert jetzt auch für Methoden
  21.12.04 wl                         TN2247.1  Einfügen von XP-Manifest
  22.12.04 wl                         TN2246.4  uses ObjectEdit
  16.02.05 pk  actOpenMethodExecute   TN2315    GetMeth.Call new parameter
  16.02.05 wl  FormCreate             TN2269    statt gLiquids wird hier gDeviceMain erzeugt ( beinhaltet TLiquids.Create )
  11.03.05 pk  FormCreate             TN2339.2  Create gEditMain
  13.04.05 wl  RefreshButtons         TN2248.8  Save-Buttons werden jetzt richtig gesetzt
  28.04.05 wl                         TN2248.8  neu: INT_IM_INDEX_VARIABLE
  22.06.05 wl                         TN2440    uses MethodEditor
  29.06.05 wl                         TN2444    neue Menüpunkte für Find/Replace
  30.06.05 wl                         TN2444    verschiedene Fehler behoben
  04.07.05 wl                         TN2494    alle Menü-Punkte und Buttons übersetzt
  06.07.05 wl                         TN2495    neue Buttons für die View-Menu items
  06.07.05 wl  EditExcelPut/Get       TN2495    funktionieren für Methoden und Commend-Makro
  06.07.05 wl  actOpenMethodExecute   TN2495    "Build"-Button aus GetMethod entfernt
  14.07.05 wl  UpdateMethodComments   TN2502    die Kommentare werden aktualisiert, wenn sie in ActionList geändert werden
  01.08.05 wl                         TN2506    uses LiqHDataAdaptor statt PipetteParamDataAdaptor
  05.08.05 wl  OpenSession            TN2501.1  Sessions können editiert werden
  10.08.05 wl  actNewSessionExecute   TN2501.1  New:Session implementiert
  22.08.05 wl                         TN2558.8  uses ScheUtil entfernt, ScheUtil-Methoden werden mit TDMScript. oder global. aufgerufen
  25.08.05 wl                         TN2558.8  benutzt TZADesignLinkAll statt TEdMainLnkAll (unit LnkEditorMainLinkAll)
  25.08.05 wl  actCompleteMethodImportExecute  TN2451.0  New menu item
  26.08.05 pk  FillRepositoryItems    TN2566    call GetAllSystemLiquidNames
  05.09.05 wl  actViewExternReagentListExecute    TN2541.4  new menu item for sql reagent list
  07.09.05 wl  MayShowExternReagentList           TN2541.4  ReagentList kann nicht mehr gezeigt werden, wenn Tabelle nicht definiert ist
  08.09.05 wl                                     TN2595    an Änderungen von AddChildAsFirstOrLast angepasst
  12.09.05 wl                                     TN2591    Für jedes Feld der Method.db gibt es ein Repository-Item
  12.09.05 wl  RepositoryItemsSetFieldLength      TN2591    bestimmt die Länge der einzugebenden Werte
  13.09.05 wl                                     TN2574    Label: "This is an evalu..." entfernt
  13.09.05 wl                                     TN2574    DDEClient ZARunner statt Sampler
  03.10.05 wl  DockAndLoadForm                    TN2642    wird jetzt von allen Etitoren benutzt
  03.10.05 wl  LiqParaButtonClick                 TN2439    neu: zum Öffnen des PipetteParamEditors
  03.10.05 wl                                     TN2413    'Saveas' button removed
  04.10.05 wl  FindDockClient                     TN2634    wenn ein Tab aktiviert wird, wird RefreshButtons aufgerufen
  05.10.05 wl  UpdateMethodComments               TN2584    neu: ändert Kommentare in allen Method-Editoren
  09.10.05 wl  MethodIsRunning                    TN2637.1  prüft, ob eine bestimmte Methode gerade läuft
  12.10.05 wl  FillRepositoryLiqParItems          TN2595    nur der LiqPar-Teil aus FillRepositoryItems
  12.10.05 wl  actNewLiquidParameterExecute       TN2595    ruft FillRepositoryLiqParItems auf
  12.10.05 wl  actNewPowderParameterExecute       TN2595    ruft FillRepositoryLiqParItems auf
  21.10.05 wl  CloseAllRunEditors                 TN2687    wird vor dem Starten von Methoden aufgerufen
  21.10.05 wl  StartMethod                        TN2687    von methodEditor hierher, ruft CloseAllRunEditors auf
  21.10.05 wl  RefreshButtons                     TN2686    Start-button erscheint auch bei Runeditor
  21.10.05 wl  RefreshButtons                     TN2686    Auch ein Run-Editor ruft ChangeGlobalName auf
  21.10.05 wl  actMethodStartExecute              TN2686    benutzt Start-button erscheint auch bei Runeditor
  07.11.05 pk                                     TN2737    use unit EdExtern for DDE
  07.11.05 pk  StartMethod                        TN2737    Removed.  Call fCurrentEditor.Start instead
  08.11.05 pk  CloseAllRunEditors                 TN2737    New parameter to allow keeping the currenteditor open
  08.11.05 pk  actMethodBuildExecute              TN2744    close all Runner run forms
  08.11.05 wl  FormShow                           TN2745    Öffnen der dMSampler-Tabelle(n) jetzt hier
  14.11.05 pk  actMethodBuildExecute              TN2759    call currenteditor.build instead of methodbuild
  16.11.05 pk  DoEditAction                       TN2751    logic changed to allow for default TAction functionality to work
  24.11.05 pk                                     TN2765    use TViewItemsWorkflow class for open, new, etcs
  24.11.05 pk  RefreshButtons                     TN2767    call selecteditor to change the layout
  24.11.05 pk                                     TN2805    references to global removed
  30.11.05 wl  CreateNewMethod                    TN2815    von ObjSampl hierher
  30.11.05 wl                                     TN2817    View, Show Posinfo entfernt
  04.12.05 pk  cxEditRepositoryLiqParamItem       TN2828    TcxEditRepositoryComboButtonItem instead of Tcx TcxEditRepositoryMRUItem
  20.12.05 wl  CreateNewMethodName                TN2541.0  Ist jetzt public
  21.12.05 wl  CreateNewMethodName                TN2541.0  kann Methoden-Namen übernehmen
  22.12.05 wl  FormCreate                         TN2541.0  TDllLoading wird initialisiert
  04.03.06 wl                                     TN2541.4  Anzeige von Reagents in AllItems, dafür entfällt ViewReagentsOfLayout
  15.03.06 wl  actMethodImportVariables           TN2889.3  Button ist jetzt immer sichtbar
  06.04.06 pk  cxEditRepositorySchedSharedIDItem  TN3024    SchedSharedID
  10.04.06 pk  cxEditRepositoryIterateItem        TN3032    Iterate
  12.04.06 pk  pmnuCloseClick                     TN3041    determine the next activepageindex
  12.04.06 pk  fDeletingPage                      TN3041    when = true skip the refreshbuttons functions if pagecontrol1.onchange is called
  09.05.06 pk  FindDockClient                     TN3091    for dockable forms use the CompareCaption instead of form.caption
  19.05.06 wl  HiddenMethodsAreShown              TN3109    neu: wenn gesetzt, sind auch ReadOnly- und Hidden-Methoden editierbar
  19.05.06 wl  RefreshSaveButtons,RefreshButtons  TN3109    bei ReadOnly- oder Hidden-Methoden sind bestimmte Buttons nicht zu sehen
  19.05.06 wl  UpdateAttributes                   TN3109    Ändert das Attribut bei einem offenen Methodeneditor
  06.06.06 pk  actMethodBuildExecute              TN3135    call close all runforms
  08.06.06 wl  RepositoryItemsSetFieldLength      TN2969    Keine Begranzung mehr für REMARK
  14.07.06 pk  CloseAllRunEditors                 TN3197    the currenteditor should be reloaded - use fDeletingPage
  21.07.06 pk  actViewHierarchy                   TN3213    New
  26.07.06 pk  actViewSearch, actFindinMethods    TN3218    New
  08.08.06 wl  RefreshButtons                     TN3254    actEditReplace wird nur noch angezeigt, wenn möglich
  12.09.06 wl                                     TN3287    neues Symbol für WGWIZ action
  18.09.06 pk                                     TN3227.1  gDeviceMain changed to gSysLiqManager
  03.10.06 wl                                     TN3317    uses LiquidParamEditor, PowderParamEditor
  24.11.06 wl  RefreshButtons                     TN3432    wenn GetZARunnerConnection = false, sind Start-Buttons disabled
  24.11.06 wl  RefreshButtons                     TN3420    Capption := Application.Title
  27.11.06 wl                                     TN3411    entfernt: öffnen von DM_Sampl-Tabellen
  28.11.06 wl  CloseDockClient                    TN3434    aus pmnuCloseClick extrahiert
  28.11.06 wl  CloseAllRunEditors                 TN3434    benutzt CloseDockClient
  08.12.06 pk  FormCreate                         TN3455    Create resourcemanager
  19.12.06 wl  viele Methoden                     TN3409    User management implementiert
  26.01.07 pk  FormDestroy                        TN3525.4  destroy objectes created in formcreate
  26.01.07 pk  FormCreate                         TN3525.3  Create ErrorManager
  02.03.07 pk  FormCreate                         TN3612   reference to global var Sam removed
  06.03.07 wl  FormCreate                         TN3620    statt TZADesignEditMain wird TGUIManager hier erzeugt
  27.04.07 wl  actNewMethodExecute                TN3669    Rückfrage mit ItemConfirmAdd
  01.08.07 wl                                     TN3811.2  InitIntf ohne LogDisplay
  30.08.07 pk                                     TN3840.1  use TLayoutDataAdaptorExt
  03.09.07 wl                                     TN3811.4  uses ObjHistory entfernt
  03.09.07 pk                                     TN3847    uses LiqHDataAdaptorExt
  09.11.07 pk                                     TN3922    references to Dataset changed to DataProvider
  09.01.08 wl   RepositoryItemsSetFieldLength     TN3972    entfernt - ist nicht mehr notwendig
  09.01.08 wl                                     TN3972    uses GetMeth entfernt
  31.01.08 pk                                     TN3864    Menu Items and Icons for Device, Driver, Connection
  20.06.08 pk                                     TN4139    Layout Various changes
  03.07.08 wl                                     TN4157
  09.07.08 pk   FormClose                         TN4139    calls CloseAllPages
  11.07.08 wl                                     TN4164    für neue Nodes im Designer: Workspace, Rack, Carrier, TipType
  11.07.08 wl   FormCreate                        TN4164    TModules heißt jetzt TModuleFinder
  31.07.08 pk   LoadLayout                        TN4139    calls RegisterLayout
  12.08.08 pk   FormCreate						 TN4165.1
  08.09.08 pk                                     TN4215    New CompilerMessages window
  23.09.08 wl                                     TN4236    uses MethodFieldnames entfernt
  23.09.08 wl   actOpenCommandMacro,actNewCommandMacro  TN4236    entfernt
  29.09.08 wl                                     TN4242    an MethodDataAdaptor angepasst
  07.10.08 pk   FormCreate                        TN4265    Create MethodDataCache and LiqHDataCache
  17.12.08 pk                                     TN4372    New Method Simultion Start button
  16.01.09 wl                                     TN4362   an Änderungen in TViewItem angepasst
  13.03.09 ts  actNewMethodExecute/RefreshButtons TN4464    Sichtbarkeit der Buttons im MethodEditor in Abhängigkeit von "startable"/ keine Layoutwahl bei Methode Neu
  13.03.09 pk                                     TN4464    uneeded code removed
  13.03.09 pk  DefineCurrentEditor                TN4467    Calls GetSelectedEditor. This means that fCurrentEditor can also be set to nil if final editor has been closed
  06.04.09 pk                                     TN4503    Changes for Display Components
  10.06.09 pk  EditRunTable Button                TN4599    removed for now
  10.06.09 pk  Build Button                       TN4598    Changed to compile
  16.06.09 wl  FormCreate                         TN4605    Beim Aufstarten wird geprüft, ob es unnötige MethodSettings gibt
  16.06.09 wl                                     TN4606    alle Bezüge auf ntRunTable und RunEditor entfernt
  17.06.09 wl                                     TN4612    actOpenLayout und actDisplayComponent werden zu actStartOptions
  17.06.09 wl                                     TN4612    das Layout wird ohne Runname geladen und bleibt beim Schließen der Methode stehen
  17.06.09 ts  Load/SaveDockableForms             TN4603    New dockable form ViewConnections ( added in View-menu )
  06.07.09 pk                                     TN4585.4  some of EdExtern functions commented out
  10.08.09 wl                                     TN4702   Strings werden jetzt direkt geladen
  25.08.09 wl                                     TN4611   neue Menübezeichnungen: "Define table import" und "Exceute table import"
  25.08.09 wl  fStringLoader                      TN4611   42210 - 42246: neue Menübezeichnungen: "Define table import" und "Exceute table import"
  28.08.09 pk                                     TN4753   LiquidsExt removed from uses
  28.08.09 pk                                     TN4753   PipDeviceManager removed from uses
  31.08.09 pk                                     TN4753   SamStart and gmReadGlobalsIniFile removed
  02.09.09 pk  CloseLayout                        TN4753   New
  02.09.09 pk  CloseAllLayouts                    TN4753   New
  28.09.09 pk                                     TN4753   New Methodname, Layoutname components for Run Tab
  26.10.09 wl                                     TN4831   Form-Position wird nicht mehr geladen und ist immer wsMaximized;
  04.11.09 pk                               	     TN4843   Replace TObjectList, TStringList, TStrings with Generics Lists and TStringArray
  12.11.09 pk                                     TN4845   XPManifest Removed
  23.11.09 pk  RefreshButtons                     TN4896   Call EdActionProps.SaveChanges to End the edit mode.
  13.04.10 wl                                     TN5044   uses geändert
  23.04.10 wl                                     TN5070   für LayoutEditor vorbereitet
  27.04.10 wl  TfrmEdMain.FormDestroy             TN5070   FreeAndNil für EdLayout entfernt
  30.04.10 wl  actViewDefaultTipTypeExecute       TN5052   bei OneTipTypeMode wird statt mehrerer TipTypes nur der Menüpunkt "Default .." angezeigt
  30.04.10 wl  actViewDefaultWorkspaceExecute     TN5052   bei OneWorkspaceMode wird statt mehrerer TipTypes nur der Menüpunkt "Default Workspace" angezeigt
  06.05.10 wl  tbsLayout                          TN5052   neu: Layout Development Page (nur in ZAArchitect sichtbar)
  06.05.10 wl  EditRepository                     TN5087   Items entfernt
  07.05.10 wl                                     TN5052    Display-Optionen aus TAppSettings statt IniAccess
  10.05.10 wl  CurrentEditMode                    TN5052    es gibt 2 Edit-Modi und einen Run-Modus
  20.05.10 wl                                     TN5116   Startbutton aktiviert
  20.05.10 wl                                     TN5117   neue Schriftart "Segoe UI", Schriftgröße 9
  25.05.10 wl  AddMainTabSheet                    TN5116   TabSheets Run, "Layout development" werden dazugeleden
  25.05.10 wl                                     TN5116   Docking --> MainMethodDevelopment
  26.05.10 wl                                     TN5116   ActionMainMenu durch normales Menu ersetzt
  27.05.10 wl                                     TN5116   Menu erweitert, auch DllMenuItems möglich
  31.05.10 wl                                     TN5116   Änderung gGUIManager.StatusBar
  02.06.10 wl  ffrmMainDeviceDevelopment          TN5116   neuer Tab Device-Entwicklung
  04.06.10 wl                                     TN5116   neu: mit großen Buttons
  07.06.10 wl  LoadLayout                         TN5116   je nach Modus wird ReadOnly- oder editierbares Layout geladen
  08.06.10 ts                                     TN5121   Menü komplett auf deutsch
  09.06.10 wl                                     TN5116   neu: DisplayDevelopment (und neue Bitmaps)
  09.06.10 wl                                     TN5116   View favourites entfernt
  18.06.10 wl                                     TN5116   Buttons überarbeitet
  18.06.10 wl                                     TN5116   Moss, Cherry Picking und Run Table-Funktionen entfernt
  18.06.10 wl  FindDockClient                     TN5116   Fenster können jetzt nicht mehr doppelt geöffnet werden
  22.07.10 wl                                     TN5192   neu: init-Button
  29.07.10 wl                                     TN5204   schöne Ordner und Favourite-icons
  30.07.10 wl                                     TN5209   Tool-Buttons zeigen Hints (außer Save und Save All, warum weiß ich nicht)
  13.09.10 pk                                     TN5116   Buttons überarbeitet
  21.09.10 pk                                     TN5047   refrences to frmActionProps removed
  23.09.10 pk  CMDialogKey                        TN4989   Make the current Pagecontrol react to Tab key (for moving to next page)
  23.09.10 pk                                     TN4989   New shortcut Ctrl + M to open method
  24.09.10 pk                                     TN5089   New imagelist for 64 Action Icons
  08.10.10 pk                                     TN5295   Various changes
  11.11.10 wl  actViewConnections                 TN5211   ViewConnections neu implementiert
  19.11.10 pk  actStartCurrentExecute             TN5359   When in run mode, starts last loaded method
  03.12.10 pk  GetCurrentEditModeForm             TN5381   new
  06.12.10 pk                                     TN5382   New Action Icons
  18.01.11 wl                                     TN5434   Layout-Editor wird noch nicht aufgerufen
  07.02.11 wl  Find/Replace                       TN5461   entfernt
  08.02.11 wl                                     TN5474   neue Icons
  10.02.11 wl                                     TN5474   Layout-Fenster nicht mehr dockable
  11.02.11 wl                                     TN5474   ImageList aufgeräumt
  23.02.11 wl                                     TN5486   neue Icons, Import-Design-Tab
  11.03.11 wl                                     TN5467   Paste funktioniert jetzt
  30.06.11 wl                                     TN5620   viele neue Icons
  01.07.11 wl                                     TN5619   alte Import-Menüpunkte entfernt
  20.07.11 wl                                     TN5614.1 neue Icons
  21.07.11 wl                                     TN5614   uses geändert
  26.07.11 wl                                     TN5614   View-Fenster sind keine FullDockableForms mehr
  02.08.11 wl                                     TN5645   Ressourcen geändert, User management verbessert
  03.08.11 wl                                     TN5645   MenuItem StartLastStarted entfernt
  16.08.11 wl                                     TN5670   SearchFor statt FindInMethods
  29.08.11 wl                                     TN5614.2 neue Icons
  14.12.11 wl                                     TN5765   ohne Session
  27.01.12 wl  RefreshButtons                     TN5786   SaveAs wird wieder richtig aktiviert
  14.03.12 wl  DockAndLoadForm                    TN5832  neuer optionaler Parameter EditMode: Der Editor wird in einem best. tab geöffnet
  30.03.12 wl                                     TN5852   neue und getauschte Icons
  20.04.12 wl  FormShow                           TN5858   Initialisierung von RunMainControls vereinfacht
  04.05.12 wl  FormShow                           TN5858   Animation endgültig endfernt
  09.05.12 wl  UserSetAccess                      TN5458   Bei useLiquids = 0 wird kein Flush-Button angezeigt
  09.05.12 wl  actFindInMethodsExecute            TN5890   Die letzten 8 Suchbriffe werden aufgelistet
  22.06.12 wl                                     TN5924   ViewReagentsOfLayout ist nicht mehr enthalten
  08.08.12 wl                                     TN5946   uses geändert
  14.11.12 wl                                     TN6018   Neues Icon für FOR-Loop
  04.12.12 wl  FormShow                           TN5960   SetMainFormControls mit neuen Parameter
  10.12.12 wl  ActionImages16,24,64               TN6045   --> ActionImages
  21.02.13 wl                                     TN6045   uses DesignerEditFunctions
  13.03.13 wl                                     TN5960   TViewItemsWorkflow.Instance statt statischer Methode
  14.03.13 wl                                     TN5960   Erweiterungen, um im Runner, die richtigen Menüpunkte zu zeigen
  30.08.13 wl  UpdateAttributes                   TN6236   entfernt
  18.09.13 wl  actToolsFlushExecute               TN6252.3 Aufruf von TEdExtern.Instance.ShowFlushDialog();
  24.01.14 tp  FormShow()                         TN6341   Dummy Action1 ersetzt fuer Calibrate function im Runner
  -------------------------------------------------------------------------------------------------- }

unit DesignerMain;


interface


uses
    Windows,
    Forms,
    Messages,

    LayoutEditor,
    Utility_DevExpress,
    ViewItem,
    EdLayout,
    DockableForm,
    AppTypes,
    ViewItemsWorkflow,
    MethodSettingsDataAdaptor,
    DesignerEditFunctions,
    ViewItemEditForm,
    StringLoader,
    MainRun,
    MainCustomDevelopment,
    MainLayoutDevelopment,
    MainDeviceDevelopment,
    MainMethodDevelopment,
    MainDisplayDevelopment,
    MainImportDevelopment,
    MainRunDevelopment,
    ActionImages,
    StdActns,
    BandActn,
    ActnList,
    ActnMan,

    ComCtrls,
    Menus,
    StdCtrls,
    Dialogs,
    Controls,
    Classes,
    ImgList,
    cxEditRepositoryItems,
    cxEdit,
    PlatformDefaultStyleActnCtrls,
    ToolWin;

const
    WM_EXTERNALCLOSE = WM_APP + 10;

type
    TZADesignMainStringLoader = class(TTextListStringLoader)
    protected
        procedure AddAllItems(); override;
    end;

    TfrmDesignerMain = class(TForm)
        StatusBar1: TStatusBar;
        ActionManager1: TActionManager;
        actFileExit: TFileExit;
        actNewMethod: TAction;
        actFilePrint: TAction;
        MainImageList: TImageList;
        actOpenMethod: TAction;
        actFileSave: TAction;
        actFileSaveAs: TAction;
        actImportIntoTable: TAction;
        actImportSequence: TAction;
        actStartOpenMethod: TAction;
        actViewProperties: TAction;
        actHelpAbout: TAction;
        CustomizeActionBars1: TCustomizeActionBars;
        actNewWashProgram: TAction;
        actNewSequence: TAction;
        actNewLiquidParameter: TAction;
        actNewPowderParameter: TAction;
        actOpenPowderParameter: TAction;
        actOpenLiquidParameter: TAction;
        actOpenSequence: TAction;
        actOpenWashProgram: TAction;
        actFileSaveAll: TAction;
        actFileDelete: TAction;
        actEditCut: TEditCut;
        actEditCopy: TEditCopy;
        actEditPaste: TEditPaste;
        actEditUndo: TEditUndo;
        actEditRedo: TAction;
        actEditSelectAll: TAction;
        actEditExcelPut: TAction;
        actEditExcelGet: TAction;
        actCompleteMethodImport: TAction;
        actViewHierarchy: TAction;
        actViewSearch: TAction;
        actFindInMethods: TAction;
        actOpenDevice: TAction;
        actOpenDriver: TAction;
        actOpenConnection: TAction;
        actNewDevice: TAction;
        actNewDriver: TAction;
        actNewConnection: TAction;
        actViewCompilerMessages: TAction;
        pgctrlModes: TPageControl;
        actViewDefaultTipType: TAction;
        actViewDefaultWorkspace: TAction;
        cxEditRepository1: TcxEditRepository;
        cxEditRepositoryActionItem: TcxEditRepositoryTextItem;
        MainMenu1: TMainMenu;
        mnuFile: TMenuItem;
        New1: TMenuItem;
        Open1: TMenuItem;
        Method1: TMenuItem;
        Method2: TMenuItem;
        Sequence1: TMenuItem;
        Save1: TMenuItem;
        Save2: TMenuItem;
        Saveas1: TMenuItem;
        Saveall1: TMenuItem;
        mnuEdit: TMenuItem;
        Cut: TMenuItem;
        Copy1: TMenuItem;
        Paste1: TMenuItem;
        N1: TMenuItem;
        SelectAll1: TMenuItem;
        N2: TMenuItem;
        SearchinMethods1: TMenuItem;
        mnuView: TMenuItem;
        Hierarchy1: TMenuItem;
        DefaultTipType1: TMenuItem;
        Searchresults1: TMenuItem;
        DefaultWorkspace1: TMenuItem;
        Compilemessages1: TMenuItem;
        N3: TMenuItem;
        mnuHelp: TMenuItem;
        About1: TMenuItem;
        Method5: TMenuItem;
        Sequence3: TMenuItem;
        Washprogram4: TMenuItem;
        LiquidHandlingParameter3: TMenuItem;
        PowderHandlingParameter3: TMenuItem;
        Device3: TMenuItem;
        Driver3: TMenuItem;
        Connection3: TMenuItem;
        Sequence4: TMenuItem;
        Washprogram5: TMenuItem;
        LiquidHandlingParameter4: TMenuItem;
        PowderHandlingParameter4: TMenuItem;
        Device4: TMenuItem;
        Driver4: TMenuItem;
        Connection4: TMenuItem;
        N4: TMenuItem;
        iport1: TMenuItem;
        Importintotable1: TMenuItem;
        Print1: TMenuItem;
        Delete1: TMenuItem;
        Exit1: TMenuItem;
        N5: TMenuItem;
        N6: TMenuItem;
        mnuTools: TMenuItem;
        N7: TMenuItem;
        actToolsFlush: TAction;
        actToolsInit: TAction;
        Action1: TAction;
        Flush1: TMenuItem;
        Initsystem1: TMenuItem;
        mnuUser: TMenuItem;
        actUserLogon: TAction;
        actUserChangePassword: TAction;
        actStartWithSelect: TAction;
        actStartLastStarted: TAction;
        TabImages: TImageList;
        mnuMethod: TMenuItem;
        mnuStartSelected: TMenuItem;
        mnuUserLogon: TMenuItem;
        mnuUserChangePassword: TMenuItem;
        ImagesButtons: TImageList;
        actViewConnections: TAction;
        Connections1: TMenuItem;
        mnuStartOpenMethod: TMenuItem;
        N8: TMenuItem;
        procedure FormCreate(Sender: TObject);
        procedure FormShow(Sender: TObject);
        procedure FormClose(Sender: TObject; var Action: TCloseAction);
        procedure FormDestroy(Sender: TObject);
        procedure FormActivate(Sender: TObject);

        procedure actOpenMethodExecute(Sender: TObject);
        procedure actNewMethodExecute(Sender: TObject);
        procedure actFilePrintExecute(Sender: TObject);
        procedure actImportIntoTableExecute(Sender: TObject);
        procedure actImportSequenceExecute(Sender: TObject);
        procedure actFileSaveAsExecute(Sender: TObject);
        procedure actStartOpenMethodExecute(Sender: TObject);
        procedure actHelpAboutExecute(Sender: TObject);
        procedure actNewWashProgramExecute(Sender: TObject);
        procedure actNewSequenceExecute(Sender: TObject);
        procedure actNewLiquidParameterExecute(Sender: TObject);
        procedure actNewPowderParameterExecute(Sender: TObject);
        procedure actOpenSequenceExecute(Sender: TObject);
        procedure actOpenWashProgramExecute(Sender: TObject);
        procedure actOpenPowderParameterExecute(Sender: TObject);
        procedure actOpenLiquidParameterExecute(Sender: TObject);
        procedure actFileSaveExecute(Sender: TObject);
        procedure actFileSaveAllExecute(Sender: TObject);
        procedure actEditUndoExecute(Sender: TObject);
        procedure actEditCutExecute(Sender: TObject);
        procedure actEditCopyExecute(Sender: TObject);
        procedure actEditPasteExecute(Sender: TObject);
        procedure actEditRedoExecute(Sender: TObject);
        procedure actEditSelectAllExecute(Sender: TObject);
        procedure actCompleteMethodImportExecute(Sender: TObject);
        procedure actViewHierarchyExecute(Sender: TObject);
        procedure actViewSearchExecute(Sender: TObject);
        procedure actFindInMethodsExecute(Sender: TObject);
        procedure actOpenDeviceExecute(Sender: TObject);
        procedure actOpenDriverExecute(Sender: TObject);
        procedure actOpenConnectionExecute(Sender: TObject);
        procedure actNewDeviceExecute(Sender: TObject);
        procedure actNewDriverExecute(Sender: TObject);
        procedure actNewConnectionExecute(Sender: TObject);
        procedure actViewCompilerMessagesExecute(Sender: TObject);
        procedure actViewDefaultTipTypeExecute(Sender: TObject);
        procedure actViewDefaultWorkspaceExecute(Sender: TObject);
        procedure actToolsFlushExecute(Sender: TObject);
        procedure actToolsInitExecute(Sender: TObject);
        procedure actUserLogonExecute(Sender: TObject);
        procedure actUserChangePasswordExecute(Sender: TObject);
        procedure actStartWithSelectExecute(Sender: TObject);
        procedure actStartLastStartedExecute(Sender: TObject);
        procedure pgctrlModesChange(Sender: TObject);
        procedure mnuUserLogonClick(Sender: TObject);
        procedure mnuUserChangePasswordClick(Sender: TObject);
        procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
        procedure pgctrlModesDrawTab(Control: TCustomTabControl; TabIndex: Integer; const Rect: TRect;
            Active: Boolean);
        procedure actViewConnectionsExecute(Sender: TObject);
    private
        fDllMenuItems: TDLLMenuItems;
        fEditFunctions: TDesignerEditFunctions;
        fStringLoader: TZADesignMainStringLoader;
        fReadOnlyLayoutDisplay: TReadOnlyLayoutDisplay;
        ffrmMainMethodDevelopment: TfrmMainMethodDevelopment;
        ffrmMainDeviceDevelopment: TfrmMainDeviceDevelopment;
        ffrmMainImportDevelopment: TfrmMainImportDevelopment;
        ffrmMainDisplayDevelopment: TfrmMainDisplayDevelopment;
        ffrmMainLayoutDevelopment: TfrmMainLayoutDevelopment;
        ffrmMainRunDevelopment: TfrmMainRunDevelopment;

        fRunTabSheet: TTabSheet;
        fMethodDevelopmentTabSheet: TTabSheet;
        fLayoutDevelopmentTabSheet: TTabSheet;
        fDeviceDevelopmentTabSheet: TTabSheet;
        fImportDevelopmentTabSheet: TTabSheet;
        fDisplayDevelopmentTabSheet: TTabSheet;
        fStarted: boolean;
        fDefaultMethod: string;
        fCurrentEditor: TViewItemEditForm;
        fDeletingPage: boolean;
        fUseLiquids: boolean;
        fExternalClose: boolean;
        fMainDevelopmentExternalFunctions: TMainDevelopmentExternalFunctions;
        //
        procedure UserSetAccess;
        procedure RefreshSaveButtons();
        procedure CreateDllMenuItems();
        procedure ExecuteDLLMenu(aSender: TObject);
        procedure DoEditAction(aSender: TObject; aEditType: TEditActionType);
        procedure ShowAppMode(aMode: TAppEditMode);

        procedure LoadLanguage(aApplObject: TScrollingWinControl);
        function CloseAllPages(): boolean;
        procedure CloseAllLayouts;
        function GetCurrentEditModeForm: TMainCustomDevelopment;
        function GetCurrentEditMode: TAppEditMode;
        function AddMainTabSheet(aForm: TForm): TTabSheet;
        procedure DoRefreshButtons(aSender: TObject);
        procedure DefineCurrentEditor;
        procedure PostClose(aSender: TObject);
        procedure ExternalClose(const aMaxWaitTime: cardinal);
        procedure WMExternalClose(var vMsg: TMessage); message WM_EXTERNALCLOSE;
        procedure CMDialogKey(var msg: TCMDialogKey); message CM_DIALOGKEY;
        procedure DoOnGetStandardToolbarImageList(const aSender: TObject; out oImageList: TCustomImageList);
        procedure DoOnGetStringLoader(const aSender: TObject; out oStringLoader: TStringLoader);
        function GetMainRunForm(): TfrmMainRun;
        procedure SetFileSaveAllEnabled(const aEnabled: boolean);
        procedure SetFileSaveEnabled(const aEnabled: boolean);
        procedure SetStartCurrentEnabled(const aMethodName: string; const aEnabled: boolean);
        property CurrentEditModeForm: TMainCustomDevelopment read GetCurrentEditModeForm;
    public
        cxEditRepositoryLiqParamItem: TcxEditRepositoryComboButtonItem;

        // Non-standard open functions
        procedure OpenTubeEditor(const aRackID: string);
        procedure OpenExistingPipetteParameter(const aName: string);

        function FindDockClient(aCaption: string; aActivate: boolean; out oEditMode: TAppEditMode): TForm;
        procedure AddReagent(aReagentName: string);
        procedure RefreshButtons();
        procedure AfterSaveStatusChanged(Sender: TObject);
        procedure LoadLayout(const aLayoutName: string);
        procedure CloseLayout(const aLayoutName: string);
        procedure DockAndLoadForm(aForm: TDockableEditForm); overload;
        procedure DockAndLoadForm(aForm: TDockableEditForm; aEditMode: TAppEditMode); overload;
        procedure CloseDockClient(aForm: TForm; aEditMode: TAppEditMode);
        procedure SearchFor(const aSearchText: string);

        // Property
        property CurrentEditMode: TAppEditMode read GetCurrentEditMode;
        property CurrentEditor: TViewItemEditForm read fCurrentEditor;
        property EditFunctions: TDesignerEditFunctions read fEditFunctions;
        property MainRun: TfrmMainRun read GetMainRunForm;
        property MethodDevelopment: TfrmMainMethodDevelopment read ffrmMainMethodDevelopment;
    end;

var
    frmDesignerMain: TfrmDesignerMain;


implementation


{$R *.DFM}

uses
    cxTL,
    SysUtils,
    Graphics,
    About,
    SamGlobe,
    Wizard,
    DialogUtils,
    AppSettings,
    ConfigurationFile,
    ViewConnections,
    AppInstanceMethodBuilding,
    SeqenceEditor,
    WashProgramEditor,
    LayoutDisplay,
    Import,
    TubeEditor,
    CompleteMethodImportForm,
    MethodEditor,
    CommonTypes,
    GUIManager,
    SpecialViewItems,
    LayoutManager,
    ZADesignLayoutManager,
    GeneralTypes,
    SeqenceManager,
    LogManager,
    ViewAllItems,
    ControlUtils,
    GetMeth,
    EditingLayoutManager,
    MethodDataAdaptor,
    EdExtern,
    ViewSearch,
    LiqHDataAdaptor,
    TipTypeDataAdaptor,
    RunMainControls,
    DesignerViewItemsWorkflow,
    LayoutElementGraphicsDriverTypeManager,
    AppInstanceDisplayComponent;

{ TZADesignMainStringLoader }

procedure TZADesignMainStringLoader.AddAllItems;
begin
    AddSingle(10000, 'Default Tip Type', 'Standard-Tip-Typ');
    AddSingle(10001, 'Default Workspace', 'Standard-Workspace');
    AddSingle(10002, 'Connections', 'Connections');
    AddDouble(42010, '&Save', 'Save data of the active page', '&Speichern',
        'Speichern|Speichert die Daten der aktuellen Seite');
    AddDouble(42020, 'S&ave as ..', 'Save data of the active page', 'Speichern &unter ..',
        'Speichern unter ..|Speichert die Daten der aktuellen Seite');
    AddDouble(42030, 'Sa&ve all', 'Save data of all pages', '&Alles speichern',
        'Alles speichern|Speichert die Daten aller Seiten');
    AddDouble(42040, '&Print', 'Print data of the active page', '&Drucken',
        'Drucken|Daten der aktuellen Seite drucken');
    AddDouble(42050, '&Delete', 'Delete data of the active page!', '&Löschen',
        'Daten der aktuellen Seite löschen!');
    AddDouble(42060, 'E&xit', 'Exit application', '&Beenden', 'Beenden|Anwendung beenden');
    AddDouble(42070, '&Undo', 'Undo last action', '&Rückgängig', 'Letzte Aktuion rückgängig machen');
    AddDouble(42080, '&Redo', 'Redo last action', '&Wiederherstellen', 'Letzte Aktuion wiederherstellen');
    AddDouble(42090, 'C&ut', 'Cut|Move marked objekt into the clipboard', '&Ausschneiden',
        'Ausschneiden|Markiertes Objekt in die Zwischenablage verschieben');
    AddDouble(42100, '&Copy', 'Copy|Copy marked objekt into the clipboard', '&Kopieren',
        'Kopieren|Markiertes Objekt in die Zwischenablage kopieren');
    AddDouble(42110, '&Paste', 'Paste|Paste content of the clipboard', '&Einfügen',
        'Einfügen|Inhalt der Zwischenablage einfügen');
    AddDouble(42120, '&Find ..', 'Find text', '&Finden...', 'Text finden');
    AddDouble(42130, 'Find &Next', 'Find next', 'Suche wiederholen', 'Nächsten finden');
    AddDouble(42140, '&Replace ..', 'Replace text', '&Ersetzen...', 'Text ersetzen');
    AddDouble(42150, '&Select All', 'Select All', '&Alles markieren', 'Alles markieren');
    AddDouble(42160, '&Export to Excel', 'Export data to Excel', '&Excel-Export',
        'Daten nach Excel exportieren');
    AddDouble(42170, '&Import from Excel', 'Import data from Excel', 'Excel-&Import',
        'Daten von Excel importieren');
    AddSingle(42180, 'Search in Methods', 'Suchen in Methoden');
    AddDouble(42200, '&About', 'About', '&Info', '');
    AddDouble(42210, '&Define table import', 'Define table import', 'Tabellen-Import &definieren',
        'Tabellen-Import definieren');
    AddDouble(42220, 'Execute &table import', 'Execute table import', '&Tabllen-Import durchführen',
        'Tabllen-Import durchführen');
    AddDouble(42230, '&Sequence import', '', '&Sequenzimport', '');
    AddDouble(42240, 'Define &variable import', 'Define variable import', '&Variablen-Import definieren',
        'Variablen-Import definieren');
    AddDouble(42260, '&Start', 'Start method', '&Starten', 'Methode starten');
    AddSingle(42280, 'Run table', 'Run-Tabelle');
    AddDouble(42300, '&Method', 'Create new method', '&Methode', 'Neue Methode erstellen');
    AddDouble(42310, '&Sequence', 'Create new sequence', '&Sequenz', 'Neue Sequenz erstellen');
    AddDouble(42320, '&Wash Program', 'Create new wash program', '&Waschprogramm',
        'Neues Waschprogramm erstellen');
    AddDouble(42340, '&Liquid Handling Parameter', 'Create new liquid handling parameter',
        '&Liquid Handling Parameter', 'Neuen Liquid Handling Parameter erstellen');
    AddDouble(42350, '&Powder Handling Parameter', 'Create new powder handling parameter',
        '&Pulverpipettierparameter', 'Neuen Pulverpipettierparameter erstellen');
    AddDouble(42400, '&Method', 'Open method', '&Methode', 'Methode öffnen');
    AddDouble(42410, '&Sequence', 'Open sequence', '&Sequenz', 'Sequenz öffnen');
    AddDouble(42420, '&Wash Program', '', '&Waschprogramm', '');
    AddDouble(42430, 'Compile', 'Compile Method', 'Kompilieren', 'Methode Kompilieren');
    AddDouble(42440, '&Liquid Handling Parameter', '', '&Liquid Handling Parameter', '');
    AddDouble(42450, '&Powder Handling Parameter', '', '&Pulverpipettierparameter', '');
    AddDouble(42470, '', 'Change start options', '', 'Start-Optionen ändern');
    AddSingle(42490, 'Select', 'Auswählen');
    AddDouble(42510, '&Layout', '', '&Layout', '');
    AddDouble(42520, '&Overview', 'Overview', '&Übersicht', 'Übersicht');
    AddDouble(42530, '&Favourites', 'Favourites', '&Favoriten', 'Favoriten');
    AddDouble(42540, '&Method Action Properties', 'Method Action Properties',
        '&Eigenschaften Methoden-Aktion', 'Eigenschaften Methoden-Aktion');
    AddDouble(42560, '&Hierarchy', '', '&Hierarchie', '');
    AddDouble(42570, '&Search results', '', '&Suchergebnisse', '');
    AddDouble(42580, 'Compiler Messages', 'Compiler Messages', 'Kompiler-Meldungen', 'Kompiler-Meldungen');
    AddSingle(42600, '&File', '&Datei');
    AddSingle(42610, '&New', '&Neu');
    AddSingle(42620, '&Open', 'Ö&ffnen');
    AddSingle(42630, '&Import', '&Import');
    AddSingle(42640, '&Edit', '&Bearbeiten');
    AddSingle(42650, '&Start', '&Starten');
    AddSingle(42660, '&View', '&Ansicht');
    AddSingle(42680, '&User', '&Benutzer');
    AddSingle(42690, '&Help', '&Hilfe');
    AddDouble(42980, '&Flush', '', '&Spülen', '');
    AddSingle(42990, 'Init system', 'System initialisieren');
    AddSingle(43000, '&Reagents of the layout', 'Reagenzien des Layouts');
    AddSingle(43010, 'Change password', 'Kennwort ändern');
    AddSingle(62400, 'Start &method once again', '&Methode noch einmal starten');
    AddSingle(62820, '&Tools', 'E&xtras');
    AddDouble(62850, '&Select and start', 'Select method and start', 'Methode &wählen und starten',
        'Methode wählen und starten');
    AddDouble(62860, 'Start method &once again', 'Start method once again', 'Methode &noch einmal starten',
        'Methode noch einmal starten');
    AddDouble(62870, '&Flush', 'Flush System', 'Spülen', 'System spülen');
    AddDouble(62880, '&Init', 'Init System', 'Initialisieren', 'System initialisieren');
end;

{ TfrmEdMain }

procedure TfrmDesignerMain.FormCreate(Sender: TObject);
var
    xIniAccess: IWinlissyIniAccess;
begin
    fStarted := true;
    fExternalClose := false;
    fDeletingPage := false;
    self.WindowState := wsMaximized;

    TControlUtils.ResetFontForWinXP(self);
    fStringLoader := TZADesignMainStringLoader.Create;
    self.LoadLanguage(self);

    CreateDllMenuItems();

    fEditFunctions := TDesignerEditFunctions.Create;
    TViewItemsWorkflow.SetInstance(TDesignerViewItemsWorkflow.Create(TfrmActionImages.Create(self)));

    // Menu items for Sophas
    actImportSequence.Visible := TAppSettings.IsSophas and
        gCommonDll.LevelIsIncluded(gCommonDll.CurrentUser.Level, usrSystem);
    actNewSequence.Visible := TAppSettings.IsSophas and
        gCommonDll.LevelIsIncluded(gCommonDll.CurrentUser.Level, usrSystemAdmin);
    actNewWashProgram.Visible := TAppSettings.IsSophas and
        gCommonDll.LevelIsIncluded(gCommonDll.CurrentUser.Level, usrSystemAdmin);
    actOpenSequence.Visible := TAppSettings.IsSophas;
    actOpenWashProgram.Visible := TAppSettings.IsSophas;

    // Menu items for Redi
    actNewPowderParameter.Visible := TAppSettings.IsRedi and
        gCommonDll.LevelIsIncluded(gCommonDll.CurrentUser.Level, usrSystemAdmin);
    actOpenPowderParameter.Visible := TAppSettings.IsRedi;

    // Menu items for complete method import (Symyx)
    actCompleteMethodImport.Visible := TAppSettings.UseSymyx() and
        gCommonDll.LevelIsIncluded(gCommonDll.CurrentUser.Level, usrSystem);

    // other "new" items
    self.actNewMethod.Enabled := gCommonDll.LevelIsIncluded(gCommonDll.CurrentUser.Level, usrSystemAdmin);
    self.actNewLiquidParameter.Enabled := gCommonDll.LevelIsIncluded(gCommonDll.CurrentUser.Level,
        usrSystemAdmin);

    fReadOnlyLayoutDisplay := TReadOnlyLayoutDisplay.Create;

    actViewDefaultTipType.Visible := TAppSettings.IsOneTipTypeMode;
    actViewDefaultWorkspace.Visible := TAppSettings.IsOneWorkspaceMode;

    xIniAccess := TAppSettings.CreateAppIni;
    fUseLiquids := xIniAccess.ReadBool('Display', 'UseLiquids');
    fDefaultMethod := xIniAccess.ReadString('Display', 'DefaultMethod');

    fMainDevelopmentExternalFunctions := TMainDevelopmentExternalFunctions.Create();
    fMainDevelopmentExternalFunctions.OnGetStandardToolbarImageList := self.DoOnGetStandardToolbarImageList;
    fMainDevelopmentExternalFunctions.OnGetStringLoader := DoOnGetStringLoader;
    fMainDevelopmentExternalFunctions.OnRefreshButtons := DoRefreshButtons;
    fMainDevelopmentExternalFunctions.OnCloseDockClient := CloseDockClient;

    fMainDevelopmentExternalFunctions.OnSave := self.actFileSave;
    fMainDevelopmentExternalFunctions.OnSaveAll := self.actFileSaveAll;
    fMainDevelopmentExternalFunctions.OnEditCut := self.actEditCut;
    fMainDevelopmentExternalFunctions.OnEditCopy := self.actEditCopy;
    fMainDevelopmentExternalFunctions.OnEditPaste := self.actEditPaste;
    fMainDevelopmentExternalFunctions.OnStart := self.actStartWithSelect;
    fMainDevelopmentExternalFunctions.OnStartLastStarted := self.actStartLastStarted;
    fMainDevelopmentExternalFunctions.OnStartOpenMethod := self.actStartOpenMethod;
    fMainDevelopmentExternalFunctions.OnFlush := self.actToolsFlush;
    fMainDevelopmentExternalFunctions.OnInit := self.actToolsInit;

    // alle TabSheets einblenden:
    ffrmMainMethodDevelopment := TfrmMainMethodDevelopment.Create(self, fMainDevelopmentExternalFunctions);
    fMethodDevelopmentTabSheet := self.AddMainTabSheet(ffrmMainMethodDevelopment);
    fReadOnlyLayoutDisplay.LayoutForm := ffrmMainMethodDevelopment.LayoutForm;

    ffrmMainRunDevelopment := TfrmMainRunDevelopment.Create(self, fMainDevelopmentExternalFunctions);
    fRunTabSheet := self.AddMainTabSheet(ffrmMainRunDevelopment);

    ffrmMainDeviceDevelopment := TfrmMainDeviceDevelopment.Create(self, fMainDevelopmentExternalFunctions);
    fDeviceDevelopmentTabSheet := self.AddMainTabSheet(ffrmMainDeviceDevelopment);

    ffrmMainDisplayDevelopment := TfrmMainDisplayDevelopment.Create(self, fMainDevelopmentExternalFunctions);
    fDisplayDevelopmentTabSheet := self.AddMainTabSheet(ffrmMainDisplayDevelopment);

    ffrmMainLayoutDevelopment := TfrmMainLayoutDevelopment.Create(self, fMainDevelopmentExternalFunctions);
    fLayoutDevelopmentTabSheet := self.AddMainTabSheet(ffrmMainLayoutDevelopment);

    ffrmMainImportDevelopment := TfrmMainImportDevelopment.Create(self, fMainDevelopmentExternalFunctions);
    fImportDevelopmentTabSheet := self.AddMainTabSheet(ffrmMainImportDevelopment);
end;

function TfrmDesignerMain.GetMainRunForm(): TfrmMainRun;
begin
    result := ffrmMainRunDevelopment.MainRun;
end;

function TfrmDesignerMain.GetCurrentEditModeForm: TMainCustomDevelopment;
var
    xCurrentEditMode: TAppEditMode;
begin
    xCurrentEditMode := GetCurrentEditMode();
    case xCurrentEditMode of
        aemMethodDevelopment:
            result := ffrmMainMethodDevelopment;
        aemRun:
            result := ffrmMainRunDevelopment;
        aemDeviceDevelopment:
            result := ffrmMainDeviceDevelopment;
        aemDisplayDevelopment:
            result := ffrmMainDisplayDevelopment;
        aemLayoutDevelopment:
            result := ffrmMainLayoutDevelopment;
        aemImportDevelopment:
            result := ffrmMainImportDevelopment;
        else
            result := nil;
    end;
end;

function TfrmDesignerMain.GetCurrentEditMode: TAppEditMode;
begin
    case self.pgctrlModes.ActivePageIndex of
        0:
            result := aemMethodDevelopment;
        1:
            result := aemRun;
        2:
            result := aemDeviceDevelopment;
        3:
            result := aemDisplayDevelopment;
        4:
            result := aemLayoutDevelopment;
        5:
            result := aemImportDevelopment;
        else
            result := aemUnknown
    end;
end;

procedure TfrmDesignerMain.ShowAppMode(aMode: TAppEditMode);
begin
    case aMode of
        aemMethodDevelopment:
            self.pgctrlModes.ActivePageIndex := fMethodDevelopmentTabSheet.PageIndex;
        aemRun:
            self.pgctrlModes.ActivePageIndex := fRunTabSheet.PageIndex;
        aemDeviceDevelopment:
            self.pgctrlModes.ActivePageIndex := fDeviceDevelopmentTabSheet.PageIndex;
        aemDisplayDevelopment:
            self.pgctrlModes.ActivePageIndex := fDisplayDevelopmentTabSheet.PageIndex;
        aemLayoutDevelopment:
            self.pgctrlModes.ActivePageIndex := fLayoutDevelopmentTabSheet.PageIndex;
        aemImportDevelopment:
            self.pgctrlModes.ActivePageIndex := fImportDevelopmentTabSheet.PageIndex;
    end;
end;

procedure TfrmDesignerMain.DefineCurrentEditor;
begin
    case (self.CurrentEditMode) of
        aemDisplayDevelopment:
            fCurrentEditor := ffrmMainDisplayDevelopment.CurrentEditor;
        aemMethodDevelopment:
            fCurrentEditor := ffrmMainMethodDevelopment.CurrentEditor;
        aemDeviceDevelopment:
            fCurrentEditor := ffrmMainDeviceDevelopment.CurrentEditor;
        aemLayoutDevelopment:
            fCurrentEditor := ffrmMainLayoutDevelopment.CurrentEditor;
        aemImportDevelopment:
            fCurrentEditor := ffrmMainImportDevelopment.CurrentEditor;
        else
            fCurrentEditor := nil;
    end;
end;

function TfrmDesignerMain.AddMainTabSheet(aForm: TForm): TTabSheet;
var
    xPageControl: TPageControl;
begin
    aForm.BorderStyle := bsNone;
    aForm.Align := alClient;
    xPageControl := self.pgctrlModes;
    result := TTabSheet.Create(xPageControl);
    result.PageControl := xPageControl;
    result.Caption := ''; // aForm.Caption;
    aForm.Parent := result;
    result.PageIndex := xPageControl.PageCount - 1;
    result.ImageIndex := result.PageIndex;
    result.TabVisible := true;
    aForm.Visible := true;
end;

procedure TfrmDesignerMain.DoOnGetStandardToolbarImageList(const aSender: TObject;
    out oImageList: TCustomImageList);
begin
    oImageList := self.ImagesButtons;
end;

procedure TfrmDesignerMain.DoOnGetStringLoader(const aSender: TObject; out oStringLoader: TStringLoader);
begin
    oStringLoader := fStringLoader;
end;

procedure TfrmDesignerMain.FormDestroy(Sender: TObject);
begin
    fReadOnlyLayoutDisplay := nil;

    TViewItemsWorkflow.DestroyInstance;
    FreeAndNil(fStringLoader);
end;

procedure TfrmDesignerMain.FormShow(Sender: TObject);
var
    x: integer;
begin
    if fStarted then
    begin
        TEdExtern.Instance.SetOnSafeAppClose(self.PostClose);

        cxEditRepositoryActionItem.Properties.MaxLength := TMethodDataAdaptor.GetMaxActionNameLength;

        // einmal durchklicken, sonst sind die Fenster nicht richtig zu sehen
        for x := self.pgctrlModes.PageCount - 1 downto 0 do
            self.pgctrlModes.ActivePageIndex := x;

        TRunMainControls.Instance.SetMainFormControls(self, fDefaultMethod, fUseLiquids,
            self.actStartWithSelect, self.actStartLastStarted, self.actStartOpenMethod, self.actToolsFlush,
            self.actToolsInit, self.Action1, self.MainRun, mnuFile, mnuTools, mnuUser, mnuMethod, nil, nil);

        TEdExtern.Instance.ResetGlobalName;
    end;

    UserSetAccess;
    fStarted := false;
end;

procedure TfrmDesignerMain.FormActivate(Sender: TObject);
begin
    RefreshButtons();
    Repaint;
end;

procedure TfrmDesignerMain.PostClose(aSender: TObject);
begin
    // Post a close message -> this will result in the Application Main Thread calling MainForm.Close
    PostMessage(self.Handle, WM_CLOSE, 0, 0);
end;

procedure TfrmDesignerMain.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
// Note: This function is always called internally by the TForm class just before closing, and before FormClose
// read TForm.OnCloseQuery for more info
// If CanClose is set to false the form close is cancelled
begin
    CanClose := true;
    if (not fExternalClose) and TEdExtern.Instance.AnyProcessesRunning() then
    begin
        if gGUIManager.MessageBox(TLanguageString.
            Read('One or more processes are active.  Would you like to abort all processes and exit the program?',
            'Ein oder mehrere Prozesse sind aktiv.  Möchten Sie alle Prozesse abbrechen und die Anwendung beenden?'),
            TLanguageString.Read('Processes Active', 'Prozesse Aktiv'), MB_ICONSTOP + MB_OKCANCEL)
            = mrCancel then
        begin
            CanClose := false;
            EXIT;
        end;
    end;
end;

procedure TfrmDesignerMain.ExternalClose(const aMaxWaitTime: cardinal);
begin
    gLogManager.Log
        ('Another application has sent a Safe Exit request. This application will close automatically when all running actions have completed.',
        false);

    // if a Request has not yet been made then make a request to close
    if TEdExtern.Instance.AnyProcessesRunning() then
    begin
        fExternalClose := true;
        // start a thread which ensures a "Safe" close
        TEdExtern.Instance.RequestSafeAppClose(aMaxWaitTime);
    end
    else
    begin
        self.Close();
    end;
end;

procedure TfrmDesignerMain.WMExternalClose(var vMsg: TMessage);
begin
    self.ExternalClose(vMsg.WParam);
end;

function TfrmDesignerMain.CloseAllPages(): boolean;
begin
    result := ffrmMainMethodDevelopment.CloseAllPages and ffrmMainLayoutDevelopment.CloseAllPages and
        ffrmMainDeviceDevelopment.CloseAllPages and ffrmMainImportDevelopment.CloseAllPages and
        ffrmMainDisplayDevelopment.CloseAllPages;
end;

procedure TfrmDesignerMain.CloseLayout(const aLayoutName: string);
begin
    TZADesignLayoutManager.Instance.UnLoadFromMemory(aLayoutName)
end;

procedure TfrmDesignerMain.CloseAllLayouts();
begin
    TZADesignLayoutManager.Instance.UnloadAllFromMemory();
end;

// --------------------------------------------------------------------------------------------------
procedure TfrmDesignerMain.FormClose(Sender: TObject; var Action: TCloseAction);
// --------------------------------------------------------------------------------------------------
begin
    try
        if not CloseAllPages() then
        begin
            Action := caNone;
            EXIT;
        end;

        // close layouts after method editors are closed because methodeditors will try to access layouts
        CloseAllLayouts();

    except
        if Application.MessageBox('Ignore errors and close application?', 'Error closing', MB_OKCANCEL) <>
            ID_OK then
        begin
            Action := caNone;
            EXIT;
        end;
    end;
    // global.CheckLastScript;
    // TZADesignLayoutManager.Instance.ApplicationEnd;

    TLayoutManager.Instance.UnregisterCurrentLayout();
    TLayoutManager.Instance.DestroyDefaultSceneGraphics();
end;

procedure TfrmDesignerMain.DockAndLoadForm(aForm: TDockableEditForm);
begin
    self.DockAndLoadForm(aForm, aemUnknown);
end;

procedure TfrmDesignerMain.DockAndLoadForm(aForm: TDockableEditForm; aEditMode: TAppEditMode);
begin
    if not Assigned(aForm) then
        EXIT;

    if (aEditMode in [aemUnknown, aemRun]) then
        aEditMode := GetCurrentEditMode
    else
        ShowAppMode(aEditMode);

    case aEditMode of
        aemMethodDevelopment:
            ffrmMainMethodDevelopment.DockAndLoadForm(aForm);
        aemDeviceDevelopment:
            ffrmMainDeviceDevelopment.DockAndLoadForm(aForm);
        aemDisplayDevelopment:
            ffrmMainDisplayDevelopment.DockAndLoadForm(aForm);
        aemLayoutDevelopment:
            ffrmMainLayoutDevelopment.DockAndLoadForm(aForm);
        aemImportDevelopment:
            ffrmMainImportDevelopment.DockAndLoadForm(aForm);
        else
            raise Exception.Create('New Editor can not be docked');
    end;

    RefreshButtons();
end;

{ Non-Standard Open Editor functions }

procedure TfrmDesignerMain.OpenTubeEditor(const aRackID: string);
var
    xEditor: TfrmTubeEditor;
    xForm: TForm;
    xEditMode: TAppEditMode;
begin
    if (aRackID = '') then
        EXIT;

    // suchen nach offenen Fenster
    xForm := FindDockClient(TfrmTubeEditor.GetCaption(aRackID), true, xEditMode);
    if (xForm <> nil) then
        EXIT;

    xEditor := TfrmTubeEditor.Create(Application, aRackID, self.AfterSaveStatusChanged);
    DockAndLoadForm(xEditor);
end;

procedure TfrmDesignerMain.pgctrlModesChange(Sender: TObject);
begin
    RefreshButtons(); // im Page-Control ändert sich die ActivePage
end;

procedure TfrmDesignerMain.pgctrlModesDrawTab(Control: TCustomTabControl; TabIndex: Integer;
    const Rect: TRect; Active: Boolean);
var
    xBitmap: TBitmap;
    xTrColor: TColor;
begin
    xBitmap := Graphics.TBitmap.Create;
    try
        if Active then
            TabImages.GetBitmap(TabIndex, xBitmap)
        else
            TabImages.GetBitmap(TabIndex + 6, xBitmap);

        // Bitmap transparent machen:
        xTrColor := xBitmap.Canvas.Pixels[0, xBitmap.Height - 1];
        xBitmap.Transparent := true;
        xBitmap.TransparentMode := tmFixed;
        xBitmap.TransparentColor := xTrColor;

        Control.Canvas.Draw(Rect.Left, Rect.Top, xBitmap);
    finally
        FreeAndNil(xBitmap);
    end;
end;

procedure TfrmDesignerMain.OpenExistingPipetteParameter(const aName: string);
begin
    if (aName = '') then
        EXIT;
    TViewItemsWorkflow.Instance.OpenPipetteParameter(aName);
end;

{ NEW actions }

procedure TfrmDesignerMain.actNewMethodExecute(Sender: TObject);
begin
    TViewItemsWorkflow.Instance.NewEditForm(ntMethod);
end;

procedure TfrmDesignerMain.actNewWashProgramExecute(Sender: TObject);
begin
    TViewItemsWorkflow.Instance.NewEditForm(ntWashProg);
end;

procedure TfrmDesignerMain.actNewSequenceExecute(Sender: TObject);
begin
    TViewItemsWorkflow.Instance.NewEditForm(ntSequence);
end;

procedure TfrmDesignerMain.actNewLiquidParameterExecute(Sender: TObject);
begin
    TViewItemsWorkflow.Instance.NewEditForm(ntLiquidPar);
end;

procedure TfrmDesignerMain.actNewPowderParameterExecute(Sender: TObject);
begin
    TViewItemsWorkflow.Instance.NewEditForm(ntPowderPar);
end;

procedure TfrmDesignerMain.actNewDeviceExecute(Sender: TObject);
begin
    TViewItemsWorkflow.Instance.NewEditForm(ntDevice);
end;

procedure TfrmDesignerMain.actNewDriverExecute(Sender: TObject);
begin
    TViewItemsWorkflow.Instance.NewEditForm(ntDriver);
end;

procedure TfrmDesignerMain.actNewConnectionExecute(Sender: TObject);
begin
    TViewItemsWorkflow.Instance.NewEditForm(ntConnection);
end;

{ OPEN actions }

procedure TfrmDesignerMain.actOpenMethodExecute(Sender: TObject);
begin
    TViewItemsWorkflow.Instance.ChooseAndOpen(ntMethod);
end;

procedure TfrmDesignerMain.actOpenSequenceExecute(Sender: TObject);
begin
    TViewItemsWorkflow.Instance.ChooseAndOpen(ntSequence);
end;

procedure TfrmDesignerMain.actOpenWashProgramExecute(Sender: TObject);
begin
    TViewItemsWorkflow.Instance.ChooseAndOpen(ntWashProg);
end;

procedure TfrmDesignerMain.actOpenPowderParameterExecute(Sender: TObject);
begin
    TViewItemsWorkflow.Instance.ChooseAndOpen(ntPowderPar);
end;

procedure TfrmDesignerMain.actOpenLiquidParameterExecute(Sender: TObject);
begin
    TViewItemsWorkflow.Instance.ChooseAndOpen(ntLiquidPar);
end;

procedure TfrmDesignerMain.actOpenDeviceExecute(Sender: TObject);
begin
    TViewItemsWorkflow.Instance.ChooseAndOpen(ntDevice);
end;

procedure TfrmDesignerMain.actOpenDriverExecute(Sender: TObject);
begin
    TViewItemsWorkflow.Instance.ChooseAndOpen(ntDriver);
end;

procedure TfrmDesignerMain.actOpenConnectionExecute(Sender: TObject);
begin
    TViewItemsWorkflow.Instance.ChooseAndOpen(ntConnection);
end;

{ Other actions }

procedure TfrmDesignerMain.actImportIntoTableExecute(Sender: TObject);
var
    ImportForm: TImportForm;
begin
    ImportForm := TImportForm.Create(nil);
    try
        ImportForm.ShowModal;
    finally
        FreeAndNil(ImportForm);
    end;
end;

procedure TfrmDesignerMain.actImportSequenceExecute(Sender: TObject);
var
    xNewName: string;
begin
    xNewName := TSequenceManager.ImportSequence;
    if (xNewName = '') then
        EXIT;

    TViewItemsWorkflow.Instance.OverviewFormsAddChild(ntSequence, xNewName);
    TViewItemsWorkflow.Instance.OpenEditForm(xNewName, ntSequence);
end;

procedure TfrmDesignerMain.actViewDefaultWorkspaceExecute(Sender: TObject);
begin
    TViewItemsWorkflow.Instance.OpenFirstItem(ntWorkspace);
end;

procedure TfrmDesignerMain.actViewDefaultTipTypeExecute(Sender: TObject);
begin
    TViewItemsWorkflow.Instance.OpenFirstItem(ntTipType);
end;

procedure TfrmDesignerMain.actFileSaveAsExecute(Sender: TObject);
var
    xNodeType: TViewItemType;
begin
    if not Assigned(self.CurrentEditor) then
        EXIT;

    xNodeType := self.CurrentEditor.ViewItemType;
    TViewItemsWorkflow.Instance.SaveAs(self.CurrentEditor.DataName, xNodeType);
end;

procedure TfrmDesignerMain.mnuUserChangePasswordClick(Sender: TObject);
var
    xUser: IUser;
begin
    xUser := gCommonDll.CurrentUser;
    xUser.ChangePassword;
end;

procedure TfrmDesignerMain.mnuUserLogonClick(Sender: TObject);
begin
    // system is not running, edit forms may be open
    gCommonDll.ChangeCurrentUser;
    UserSetAccess;
    TEdExtern.Instance.ReloadValues;
end;

procedure TfrmDesignerMain.actStartLastStartedExecute(Sender: TObject);
begin
    self.ShowAppMode(aemRun);
    TEdExtern.Instance.StartCurrentMethod;
    { case GetCurrentEditMode of
      aemMethodDevelopment:
      self.CurrentEditorStartMethod(false);
      aemRun:
      self.StartMethod(TEdExtern.Instance.CurrentMethodName, false);
      end; }
end;

procedure TfrmDesignerMain.actStartOpenMethodExecute(Sender: TObject);
begin
    if not Assigned(self.CurrentEditor) then
        EXIT;
    if not self.CurrentEditor.RequestSaveChanges() then
        EXIT;
    if not self.CurrentEditor.Build() then
        EXIT;

    // Methode Starten
    self.ShowAppMode(aemRun);
    TEdExtern.Instance.MethodStart(self.CurrentEditor.DataName, false, false);
end;

{
  procedure TfrmEdMain.RunModeStartMethod(aAlwaysSelect: boolean; aSimulate: boolean);
  var
  xSelectName, xStartName: string;
  begin
  if (aAlwaysSelect) or (fDefaultMethod = '') then
  begin
  xSelectName := TEdExtern.Instance.CurrentMethodName;
  if (TGetMethDlg.Call(xSelectName, dtMethod) = actStart) then
  xStartName := xSelectName;
  Application.ProcessMessages;
  end
  else
  xStartName := fDefaultMethod;

  StartMethod(xStartName, aSimulate);
  end;
}
procedure TfrmDesignerMain.actStartWithSelectExecute(Sender: TObject);
begin
    if TEdExtern.Instance.ThreadIsRunning(false) then
        EXIT;

    Application.ProcessMessages; // Nötig, um metheditform.FormClose vollständig abzuschließen

    self.ShowAppMode(aemRun);
    TEdExtern.Instance.StartMethod(true, fDefaultMethod);
    // self.RunModeStartMethod(true, false);
end;

procedure TfrmDesignerMain.actViewHierarchyExecute(Sender: TObject);
begin
    self.ShowAppMode(TAppEditMode.aemMethodDevelopment);
    ffrmMainMethodDevelopment.ShowViewHierarchy;
end;

procedure TfrmDesignerMain.actToolsFlushExecute(Sender: TObject);
begin
    if TEdExtern.Instance.ThreadIsRunning(false) then
        EXIT;

    self.ShowAppMode(aemRun);
    TEdExtern.Instance.ShowFlushDialog();
end;

procedure TfrmDesignerMain.actToolsInitExecute(Sender: TObject);
begin
    if TEdExtern.Instance.ThreadIsRunning(false) then
        EXIT;

    self.ShowAppMode(aemRun);
    TEdExtern.Instance.InitSystem();
end;

procedure TfrmDesignerMain.actUserChangePasswordExecute(Sender: TObject);
var
    xUser: IUser;
begin
    if TEdExtern.Instance.ThreadIsRunning(false) then
        EXIT;

    xUser := gCommonDll.CurrentUser;
    xUser.ChangePassword;
end;

procedure TfrmDesignerMain.actUserLogonExecute(Sender: TObject);
begin
    if TEdExtern.Instance.ThreadIsRunning(false) then
        EXIT;

    // system is not running, edit forms may be open
    gCommonDll.ChangeCurrentUser;
    UserSetAccess;
    TEdExtern.Instance.ReloadValues;
end;

procedure TfrmDesignerMain.actHelpAboutExecute(Sender: TObject);
var
    xAboutBox: TAboutBox;
begin
    xAboutBox := TAboutBox.Create(nil);
    try
        xAboutBox.ShowModal;
    finally
        FreeAndNil(xAboutBox);
    end;
end;

procedure TfrmDesignerMain.SetFileSaveEnabled(const aEnabled: boolean);
begin
    actFileSave.Enabled := aEnabled;
    self.CurrentEditModeForm.SetFileSaveEnabled(aEnabled);
end;

procedure TfrmDesignerMain.SetFileSaveAllEnabled(const aEnabled: boolean);
begin
    actFileSaveAll.Enabled := aEnabled;
    self.CurrentEditModeForm.SetFileSaveAllEnabled(aEnabled);
end;

procedure TfrmDesignerMain.SetStartCurrentEnabled(const aMethodName: string; const aEnabled: boolean);
begin
    if aMethodName = '' then
        self.actStartOpenMethod.Caption := 'Start'
    else
        self.actStartOpenMethod.Caption := TLanguageString.read('Start method: ' + aMethodName,
            'Methode starten: ' + aMethodName);
    TRunMainControls.Instance.EnableControlsByOpenMethod(aEnabled);
end;

procedure TfrmDesignerMain.RefreshButtons();
var
    // xCurrentEditMode: TAppEditMode;
    xStartableMethodName: string;
begin
    if fDeletingPage then
        EXIT;

    // fCurrentEditor bestimmen!
    self.DefineCurrentEditor();

    // xCurrentEditMode := GetCurrentEditMode();
    // Grundeinstelleung
    actFilePrint.Enabled := false;
    actEditSelectAll.Enabled := false;
    SetFileSaveEnabled(false);
    xStartableMethodName := '';

    if Assigned(self.CurrentEditor) then
    begin
        Caption := Application.Title + ' [' + self.CurrentEditor.Caption + ']';

        actFilePrint.Enabled := Assigned(self.CurrentEditor.OnPrint);
        actEditSelectAll.Enabled := Assigned(self.CurrentEditor.OnSelectAll);
        actFileSaveAs.Enabled := TViewItemsWorkflow.Instance.SaveAsIsAllowed(self.CurrentEditor.ViewItem);

        if (self.CurrentEditor is TfrmMethodEditor) and
            ((self.CurrentEditor as TfrmMethodEditor).StartableFromGUI) then
        begin
            xStartableMethodName := (self.CurrentEditor as TfrmMethodEditor).DataName;
        end
    end
    else
        Caption := Application.Title;

    SetStartCurrentEnabled(xStartableMethodName, xStartableMethodName <> '');

    // Enable/Disable Save-Buttons
    RefreshSaveButtons();

    // switch the layout
    if Assigned(self.CurrentEditor) then
        self.CurrentEditor.SelectEditor();
end;

procedure TfrmDesignerMain.RefreshSaveButtons();
begin
    // Grundeinstelleung
    SetFileSaveEnabled(false);
    SetFileSaveAllEnabled(false);
    actEditExcelGet.Enabled := false;
    actEditExcelPut.Enabled := false;

    // save all
    SetFileSaveAllEnabled(ffrmMainMethodDevelopment.DataChanged or ffrmMainLayoutDevelopment.DataChanged or
        ffrmMainDeviceDevelopment.DataChanged or ffrmMainImportDevelopment.DataChanged or
        ffrmMainDisplayDevelopment.DataChanged);

    if not Assigned(self.CurrentEditor) then
        EXIT;

    // Excel-Buttons: auch hier, weil sie sich zwischendurch ändern können!
    if Assigned(self.CurrentEditor.OnExcelPut) and
        not((self.CurrentEditor is TfrmMethodEditor) and ((self.CurrentEditor as TfrmMethodEditor)
        .Attribute = meaHidden)) and gCommonDll.CurrentUser.HasLevel(usrSystemAdmin) then
    begin
        actEditExcelPut.Enabled := true;
        actEditExcelGet.Enabled := self.CurrentEditor.ExcelGetPossible;
    end;

    // save
    SetFileSaveEnabled(self.CurrentEditor.DataChanged);
end;

procedure TfrmDesignerMain.actFileSaveExecute(Sender: TObject);
begin
    if not Assigned(fCurrentEditor) then
        EXIT;

    if fCurrentEditor.DataChanged then
        fCurrentEditor.Save();
end;

procedure TfrmDesignerMain.actFilePrintExecute(Sender: TObject);
begin
    if not Assigned(fCurrentEditor) then
        EXIT;

    fCurrentEditor.Print();
end;

procedure TfrmDesignerMain.actFileSaveAllExecute(Sender: TObject);
begin
    ffrmMainMethodDevelopment.SaveAll;
    ffrmMainLayoutDevelopment.SaveAll;
    ffrmMainDeviceDevelopment.SaveAll;
    ffrmMainImportDevelopment.SaveAll;
    ffrmMainDisplayDevelopment.SaveAll;
end;

procedure TfrmDesignerMain.DoEditAction(aSender: TObject; aEditType: TEditActionType);
// Sender is a TAction
begin
    if Assigned(fCurrentEditor) then
    begin
        if fCurrentEditor.EditAction(self.ActiveControl, aEditType) then
            EXIT;
    end;

    // Here we are assuming that the active control is actually the control for which we want to execute the edit Action
    // we will Execute the action onto the ActiveControl.
    if aSender is TAction then
        self.ActiveControl.ExecuteAction(aSender as TAction);
end;

procedure TfrmDesignerMain.DoRefreshButtons(aSender: TObject);
begin
    RefreshButtons;
end;

procedure TfrmDesignerMain.actEditUndoExecute(Sender: TObject);
begin
    DoEditAction(Sender, eaUndo);
end;

procedure TfrmDesignerMain.actEditRedoExecute(Sender: TObject);
begin
    DoEditAction(Sender, eaRedo);
end;

procedure TfrmDesignerMain.actEditCutExecute(Sender: TObject);
begin
    DoEditAction(Sender, eaCut);
end;

procedure TfrmDesignerMain.actEditCopyExecute(Sender: TObject);
begin
    DoEditAction(Sender, eaCopy);
end;

procedure TfrmDesignerMain.actEditPasteExecute(Sender: TObject);
begin
    DoEditAction(Sender, eaPaste);
end;

procedure TfrmDesignerMain.actEditSelectAllExecute(Sender: TObject);
begin
    DoEditAction(Sender, eaSelectAll);
end;

procedure TfrmDesignerMain.actCompleteMethodImportExecute(Sender: TObject);
begin
    TfrmCompleteMethodImport.ShowAndImport;
end;

procedure TfrmDesignerMain.LoadLayout(const aLayoutName: string);
var
    xEditor: TfrmLayoutEditor;
    xForm: TForm;
    xEditMode: TAppEditMode;
begin
    // result := true;
    if (aLayoutName = '') then
        EXIT;

    if (self.GetCurrentEditMode() = aemLayoutDevelopment) then
    begin
        if (not TEditingLayoutManager.Instance.IsCurrentLayoutEmpty) and
            (aLayoutName = TEditingLayoutManager.Instance.CurrentLayout.Name) then
            EXIT;

        // ---------------------------------------------------------------------------------------------
        // ACHTUNG: Weil der Layout-Editor noch zu fehlerhaft ist, wird er an dieser Stelle
        // noch nicht aufgerufen:
        EXIT;
        // ---------------------------------------------------------------------------------------------

        // suchen nach offenen Fenster
        xForm := FindDockClient(TfrmLayoutEditor.GetCaption(aLayoutName), true, xEditMode);
        if (xForm <> nil) then
            EXIT;

        xEditor := TfrmLayoutEditor.Create(Application, aLayoutName, self.AfterSaveStatusChanged);
        ffrmMainLayoutDevelopment.DockAndLoadForm(xEditor);

        TEditingLayoutManager.Instance.CreateDefaultSceneGraphics(xEditor.pnLayout);
        TEditingLayoutManager.Instance.RegisterLayout('', aLayoutName);
        { result := } TEditingLayoutManager.Instance.Load();
        RefreshButtons;
    end
    else if (self.GetCurrentEditMode() = aemMethodDevelopment) then
    begin
        if (not TZADesignLayoutManager.Instance.IsCurrentLayoutEmpty) and
            (aLayoutName = TZADesignLayoutManager.Instance.CurrentLayout.Name) then
            EXIT;

        TZADesignLayoutManager.Instance.ChangeLayoutMode(fReadOnlyLayoutDisplay);
        TZADesignLayoutManager.Instance.RegisterLayout('', aLayoutName);
        fReadOnlyLayoutDisplay.SetCaption('', aLayoutName);
        { result := } TZADesignLayoutManager.Instance.Load();
    end;
end;

procedure TfrmDesignerMain.AddReagent(aReagentName: string);
begin
    if not(fCurrentEditor is TfrmSequenceEditor) then
        EXIT;
    (fCurrentEditor as TfrmSequenceEditor).AddReagent(aReagentName);
end;

procedure TfrmDesignerMain.AfterSaveStatusChanged(Sender: TObject);
begin
    self.RefreshSaveButtons(); // wenn sich in dem Editor-Fenster selbst etwas ändert
end;

procedure TfrmDesignerMain.LoadLanguage(aApplObject: TScrollingWinControl);
var
    x: integer;
    xComponent: TComponent;
begin
    // Standard-LoadLanguage
    fStringLoader.LoadLanguage(aApplObject);

    for x := 0 to aApplObject.ComponentCount - 1 do
    begin
        xComponent := aApplObject.Components[x];
        if (xComponent.Tag = 0) then
            CONTINUE;

        if (xComponent is TAction) then
        begin
            (xComponent as TAction).Caption := fStringLoader.GetResString(xComponent.Tag);
            (xComponent as TAction).Hint := fStringLoader.GetResString(xComponent.Tag + 5);
        end;
    end;
end;

procedure TfrmDesignerMain.CloseDockClient(aForm: TForm; aEditMode: TAppEditMode);
begin
    if not Assigned(aForm) then
        EXIT;

    // close the form
    fDeletingPage := true;
    // deleting a page causes pagecontrol.onchange to be called, and the first page to be loaded. we dont want this!
    try
        aForm.Close;
        // WICHTIG!!! nicht löschen
        Application.ProcessMessages; // Damit wird das geschlossene Fenster zerstört, wenn caFree gesetzt!
        // WICHTIG!!! nicht löschen
    finally
        fDeletingPage := false;
    end;
end;

function TfrmDesignerMain.FindDockClient(aCaption: string; aActivate: boolean;
    out oEditMode: TAppEditMode): TForm;
begin
    result := ffrmMainMethodDevelopment.FindDockClient(aCaption, aActivate);
    oEditMode := aemMethodDevelopment;
    if not Assigned(result) then
    begin
        result := ffrmMainLayoutDevelopment.FindDockClient(aCaption, aActivate);
        oEditMode := aemLayoutDevelopment;
    end;
    if not Assigned(result) then
    begin
        result := ffrmMainDeviceDevelopment.FindDockClient(aCaption, aActivate);
        oEditMode := aemDeviceDevelopment;
    end;
    if not Assigned(result) then
    begin
        result := ffrmMainImportDevelopment.FindDockClient(aCaption, aActivate);
        oEditMode := aemImportDevelopment;
    end;
    if not Assigned(result) then
    begin
        result := ffrmMainDisplayDevelopment.FindDockClient(aCaption, aActivate);
        oEditMode := aemDisplayDevelopment;
    end;

    if aActivate and (result is TForm) then
    begin
        ShowAppMode(oEditMode);
    end;
end;

procedure TfrmDesignerMain.SearchFor(const aSearchText: string);
begin
    self.ShowAppMode(TAppEditMode.aemMethodDevelopment);
    self.ffrmMainMethodDevelopment.SearchFor(aSearchText);
end;

{ Repository }

procedure TfrmDesignerMain.ExecuteDLLMenu(aSender: TObject);
var
    x: integer;
begin
    if not(aSender is TMenuItem) then
        EXIT;

    for x := 0 to high(fDLLMenuItems) do
    begin
        if (fDLLMenuItems[x].MenuName <> (aSender as TMenuItem).Name) then
            CONTINUE;
        TEdExtern.Instance.ExecuteDllCall(fDLLMenuItems[x].DLLName, fDLLMenuItems[x].DLLFunction,
            fDLLMenuItems[x].Parameter);
    end;
end;

procedure TfrmDesignerMain.CreateDllMenuItems();
var
    xNewItem: TMenuItem;
    x: integer;
begin
    TEdExtern.ReadDllMenuItems(fDllMenuItems);

    // Erzeugen beliebig vieler Menüpunkte zum Aufruf von DLL-Funktionen
    for x := 0 to high(fDLLMenuItems) do
    begin
        xNewItem := TMenuItem.Create(self);
        xNewItem.Caption := fDLLMenuItems[x].MenuCaption;
        xNewItem.Name := fDLLMenuItems[x].MenuName;
        xNewItem.OnClick := ExecuteDLLMenu;
        mnuTools.Add(xNewItem);
    end;
end;

procedure TfrmDesignerMain.UserSetAccess;
var
    xUser: IUser;
begin
    xUser := gCommonDll.CurrentUser;
    StatusBar1.Panels[0].Text := xUser.Name;

    // Guest Access
    StatusBar1.Panels[1].Text := 'Guest Access: Load Layout, view data, no editing, only simulation mode';
    actImportIntoTable.Enabled := false;
    self.actToolsFlush.Visible := false;
    self.actToolsInit.Enabled := false;

    // System Access
    if gCommonDll.LevelIsIncluded(xUser.Level, usrSystem) then
    begin
        StatusBar1.Panels[1].Text := 'System User Access: Start Runs, no editing';
        self.actToolsFlush.Visible := fUseLiquids;
        self.actToolsInit.Enabled := true;
    end;

    // System Administrator access (CFR 21 compliant)
    if gCommonDll.LevelIsIncluded(xUser.Level, usrSystemAdmin) then
    begin
        StatusBar1.Panels[1].Text := 'System Administrator Access: Start runs, change data';
        actImportIntoTable.Enabled := true;
    end;

    // unlimited Administrator access (not CFR 21 compliant)
    if gCommonDll.LevelIsIncluded(xUser.Level, usrUnlimitedAdmin) then
    begin
        StatusBar1.Panels[1].Text := 'Administrator Access: Start runs, change data';
    end;

    // CFR21 Prepare Mode
    if (xUser.CFR21Mode = TCFR21ComplianceMode.ccmPrepared) then
    begin
        StatusBar1.Panels.Add;
        StatusBar1.Panels[2].Text := '--- CFR21 PART 11 PREPARE MODE ---';
    end;

    // CFR21 Compliant Mode
    if (xUser.CFR21Mode = TCFR21ComplianceMode.ccmCompliant) then
    begin
        StatusBar1.Panels.Add;
        StatusBar1.Panels[2].Text := '--- CFR21 Part 11 Compliant ---';
    end;
end;

procedure TfrmDesignerMain.actViewSearchExecute(Sender: TObject);
begin
    self.ShowAppMode(TAppEditMode.aemMethodDevelopment);
    self.ffrmMainMethodDevelopment.ShowViewSearch;
end;

procedure TfrmDesignerMain.actFindInMethodsExecute(Sender: TObject);
const
    cMaxSearchItems = 8;
var
    xLocalIniFile: IConfigurationSet;
    xSearchText: string;
    x, xItemIndex: integer;
    xSearchItems: TArray<string>;
begin
    SetLength(xSearchItems, cMaxSearchItems);

    // Text lesen aus appdata.tmp
    xLocalIniFile := TConfigurationFile.Create(TAppsettings.LocalIniFileName);
    xLocalIniFile.Open(true);
    try
        for x := 0 to cMaxSearchItems - 1 do
        begin
            xSearchItems[x] := xLocalIniFile.ReadString('Search', 'Item' + IntToStr(x + 1), '');
        end;
    finally
        xLocalIniFile.Close;
    end;

    xSearchText := TDialogUtils.SelectItemBox(xSearchItems, TLanguageString.Read('Search for:',
        'Suchen nach:'), TLanguageString.Read('Search in Methods and SQL terms',
        'Suchen in Methoden und SQL-Terms'), '', false);
    if (xSearchText = '') then
        EXIT;

    // xLocalIniFile := TConfigurationFile.Create(TAppsettings.LocalIniFileName);
    xLocalIniFile.Open(false);
    try
        xLocalIniFile.WriteString('Search', 'Item1', xSearchText);
        xItemIndex := 2;
        for x := 0 to cMaxSearchItems - 1 do
        begin
            if not SameText(xSearchItems[x], xSearchText) then
            begin
                xLocalIniFile.WriteString('Search', 'Item' + IntToStr(xItemIndex), xSearchItems[x]);
                inc(xItemIndex);
            end;
        end;
    finally
        xLocalIniFile.Close;
    end;

    self.SearchFor(xSearchText);
end;

procedure TfrmDesignerMain.actViewCompilerMessagesExecute(Sender: TObject);
begin
    self.ShowAppMode(TAppEditMode.aemMethodDevelopment);
    self.ffrmMainMethodDevelopment.ShowViewCompilerMessages;
end;

procedure TfrmDesignerMain.actViewConnectionsExecute(Sender: TObject);
var
    xFrm: TfrmViewConnections;
begin
    xFrm := TfrmViewConnections.Create(nil);
    try
        xFrm.ShowModal;
    finally
        FreeAndNil(xFrm);
    end;
end;

procedure TfrmDesignerMain.CMDialogKey(var msg: TCMDialogKey);
var
    Control: TWinControl;
begin
    with Msg do
    begin
        if (charcode = VK_TAB) and (GetKeyState(VK_CONTROL) < 0) then
        begin
            Control := ActiveControl;
            while Assigned(Control) do
                if Control is TPageControl then
                begin
                    Control.Perform(CM_DIALOGKEY, charcode, keydata);
                    Exit;
                end
                else
                    Control := Control.Parent;
        end;
    end;
    inherited;
end;


end.
