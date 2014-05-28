{ --------------------------------------------------------------------------------------------------
  Copyright © 2003 Zinsser Analytic GmbH - All rights reserved.
  Author       : Wolfgang Lyncke (wl)
  Description  : TTubeHandling: Collection of all possible tube handling methods
  (using Devices, TPosinfoDataAdaptor and TWorkbench)
  according to LiquidHandling, PowderHandling & RackHandling
  --------------------------------------------------------------------------------------------------
  Revision History:
  date     op  method                       track-no improvement/change
  -------- --  ---------------------------  -------- -----------------------------------------------
  18.07.03 wl                               TN1501.7 initial version
  18.07.03 wl  GetTube/PutTube              TN1501.7 aufgeteilt in GetTube1/2 und PutTube1/2 (für Decapping)
  18.07.03 wl  GetTube1                     TN1539   Fehlermeldungen die zum Abbruch des Threads führen, werden am Schluß angezeigt ("Tool-tube diameter ...")
  22.07.03 wl  BalanceTakeTube              TN1501.7 vor dem Zurückbringen wird ein Cap aufgesetzt (wenn vorhanden)
  23.07.03 mo  MoveTubeFrom/ToPOs           TN1524   Move wird vom richtigen xMotor ausgeführt
  24.07.03 wl  GetTube2                     TN1501.7 MoveTubeData wird jetzt richtig gesetzt
  24.07.03 wl  GetTube/PutTube              TN1501.7 nur noch procedure, da Rückgabewert nutzlos (und falsch)
  24.07.03 wl  gmReadTubeBarCode            TN1501.7 Barcode wird jetzt wieder in Posinfo geschrieben
  24.07.03 wl  GetTubeFromDecapper          TN1501.7 von Decap/Cap-Funktion getrennt, weil es nicht immer hintereinander abläuft
  24.07.03 wl  GetTube1                     TN1501.7 Meldung deaktiviert: Current Varispan < Tool-Tube width -> no Tool in Gripper
  24.07.03 wl  RWTubeID                     TN1501.7 Abläufe komplett überarbeitet (1.erfolgreicher Test)
  24.07.03 wl  BalanceGetTube,BalancePutTube TN1536  Ersetzen der Abfrage (gBalances.Count=0) durch (gModules.FindBalance=nil)
  25.07.03 wl  PutTube,GetTube,BalanceTakeTube  TN1501.7 Umstellung von TMoveTubeMode auf TMoveTubeModes (weil mehrere Parameter zusammen kommen können)
  25.07.03 wl  BalanceTakeTube              TN1501.7 [mtmNoStartPosBeforePutTube] - wenn das Tube von der Waage auf den Decapper gestellt wird, d.h. Start-Offsets werden nicht berücksichtigt!!
  25.07.03 wl  RWTubeID                     TN1501.7 [mtmNoStartPosAfterGetTube] - wenn das Tube vom Decapper auf die Waage gestellt wird, d.h. Start-Offsets werden nicht berücksichtigt!!
  25.07.03 wl  DecapperResetAfterCapping    TN1501.7 benutzt neue Decapper-Funktion GoToBasicStateAfterCapping
  25.07.03 wl  PutTubeOnDecapper            TN1501.7 stellt nur das Tube auf den Decapper (für Capping)
  30.07.03 wl  DecapperResetAfterCapping    TN1501.7 benutzt wieder alte GoToBasicState-Methode
  31.07.03 wl  MoveTubeFromPos,-ToPos       TN1501.3 wenn TubeSensor <> nil, erfolgt eine Sensor-Abfrage an der Startposition
  31.07.03 wl  PutTubeOnDecapper,GetTubeFromDecapper  TN1501.3 bei GetTube und PutTube wird TubeSensor übergeben
  31.07.03 wl  DecapTube,gmReadTubeBarCode  TN1501.3 wenn nach Decapping kein Cap im Gripper -> SetGlobalErr
  31.07.03 wl  PutCapOnTube                 TN1501.3 wenn vor dem Capping kein Cap im Gripper -> SetGlobalErr
  03.08.03 wl  PutTubeOnDecapper,GetTubeFromDecapper  TN1501.3 TubeSensor wird nur übergeben, wenn optTubeNoTubeSensor nicht gesetzt ist
  04.08.03 wl  DecapTube,gmReadTubeBarCode  TN1501.3 Option optTubeNoCapSensor wird an Decap-Funktion übergeben
  04.08.03 wl  PutCapOnTube                 TN1501.3 Option optTubeNoCapSensor wird an Cap-Funktion übergeben
  04.08.03 wl  BalancePutTube,BalanceTakeTube  TN1501.10 Tube-Sensor des Weighing-Moduls wird mit übergeben
  04.08.03 wl  BalTakeTubeAndWeigh          TN1501.10 Tube-Sensor des Weighing-Moduls wird mit übergeben
  05.08.03 wl  RWTubeID                     TN1536   benutzt TPosinfoDataAdaptor.GetTubeOrigin
  06.08.03 wl  CheckTubeWithSensor          TN1501.3 verwendet das Error-Fenster mit Abort-Retry-Ignore
  06.08.03 wl  gmReadTubeBarCode,DecapTube  TN1501.3 verwendet das Error-Fenster mit Abort-Retry-Ignore
  07.08.03 wl  RWTubeID                     TN1501.6 wenn ein Tube einen Random-Barcode hat, wird bei erneutem "ReadT" wieder der Barcode gelesen
  19.08.03 wl  DecapTube                    TN1551   Bei Retry wird das Tube wieder hochgenommen und alles wiederholt
  19.08.03 wl  PutCapInCapWaste             TN1549   Nach dem Wegwerfen wird gecheckt, ob Cap noch da ist (Retry möglich)
  19.08.03 wl  FetchNewCap                  TN1549   Nach PutTube/CapIntake wird gecheckt, ob ein Cap da ist (Retry möglich)
  19.08.03 wl  RWTubeID,SetDecapperOptions  TN1501.3 das Deaktivieren der Sensoren passiert jetzt hier
  01.09.03 wl  PutCapOnTube                 TN1565   GetTubeFromDecapper muß auch ausgeführt werden, wenn kein Cap da ist!!
  01.09.03 wl  gmReadTubeBarCode            TN1566   die Variable aIsDecapped wird jetzt nach dem Decappen gesetzt
  01.09.03 wl  Balance-Methoden             TN1556   Balance-Methoden werden nicht mehr über TActionModules sondern direkt aufgerufen
  03.09.03 wl                               TN1568   (für Editor) FindTubeReadingDevice wird für TActionModules aufgerufen
  09.09.03 wl                               TN1581.5 uses ObjWorkbExt entfernt
  17.09.03 wl  GetMoveTubeData              TN1526.1 Vereinfachte Berechnung von result.XDest bei ZP02
  18.09.03 wl  gmRWTubeID                   TN1597   g24hMode jetzt als integer (statt boolean)
  18.09.03 wl                               TN1564   Robot-DisableError-Methoden statt ErrorDisableBitmap
  18.09.03 wl  GetMoveTubeData              TN1526.1 Berechnung von result.YDest wieder wie vorher
  22.09.03 pk  RWTubeID                     TN1556.2 DoTara function name changed to StartTare
  22.09.03 pk  RWTubeID                     TN1593   removed call to global gmBalanceAskTara, replaced it with direct object call.
  22.09.03 pk  RWTubeID                     TN1593   Open door not called when there are no tubes to remove from the balance
  29.09.03 wl  GetZTubePos                  TN1501.4 benutzt Rack_methode GetTubeZPosition_mm
  30.09.03 pk  DecapTube                    TN1605   Decapper without using decapper
  01.10.03 pk  PutTubeOnDecapper            TN1607   If DropBeforeIntake is true then drop off the tube on the turntable before TubeIntake
  01.10.03 pk  GetTubeFromDecapper          TN1607   If DropBeforeIntake is true then ReleaseTube first then pick up the tube from the turntable
  14.10.03 pk  CheckTubeWithSensor          TN1622   Now calls TubeSensor.AskValue. If a none-value is returned an OK status is interpreted
  30.10.03 tbh GetMoveTubeData              TN1635   Berechnung der Koordinaten für Carrier-XStartOffsets korrigiert
  18.11.03 wl  MoveTubeFrom/ToPos           TN1667   Neu: Rotation-Motor wird bewegt wenn HRStart > 0
  19.11.03 wl  MoveTubeFrom/ToPos           TN1674   Bei X-Start und Y-Start vom Startpunkt aus zuerst X, dann Y fahren
  19.11.03 wl  DecapTube,gmReadTubeBarCode  TN1675   bei Decap und CapCheck wird (optTubeDecapToWaste in aTOptions) übergeben
  20.11.03 wl  GetMoveTubeData              TN1677   Rotation-Wert wird durch Decapper-Device bestimmt - statt durch HRStart-Wert (TN1667)
  20.11.03 wl  gmReadTubeBarCode            TN1673   Sonderfunktion ZP02: Tube-Retake, 90° versetzen und nochmal lesen
  24.11.03 wl  GetMoveTubeData              TN1678   für die Berechnung der Y-Position wird DistY_H_Tip1 statt HYOffset verwendet
  10.12.03 wl  GetMoveTubeData              TN1672   CalculatePipSteps mit Y-Array statt mit Varispan
  16.12.03 wl  alle Funktionen              TN1672   Robot-Move-Aufrufe ersetzt durch gPipArm-Methoden
  18.12.03 wl                               TN1672   uses geändert
  19.12.03 wl                               TN1672    _Move durch gGrpArm.Move-Methoden ersetzt
  23.12.03 wl  gmReadTubeBarCode            TN1712   Erweiterung für TN1673: nicht um 90° sondern um gTubeRetakeRotation versetzen
  06.01.04 wl  RWTubeID                     TN1713   Random-Barcode wird nur erzeugt wenn GlobalErr nicht gesetzt ist
  06.01.04 wl  CreateCapEntry               TN1713   erfolgt nur wenn GlobalErr nicht gesetzt ist (Random-Barcode und Cap-Entry erzeugen)
  06.01.04 wl  RWTubeID                     TN1711   Painttubes wird am Schluß nicht auf #0 gesetzt (Tube bleibt grün)
  04.02.04 pk                               TN1719   functions moved into this unit from old Thread units( ExtLiq, BasicLig, etc)
  05.02.04 wl  CheckTubeID                  TN1711   Tube wird zur Abfrage violett und nach erfolgter Eingabe grün gezeichnet
  05.02.04 wl  CheckTubeID                  TN1729.1 bei GetTubeFromDecapper wird "Decapping" übergeben (BC-Lesen und Capping ist nicht möglich)
  05.02.04 wl  CheckTubeID                  TN1739   die Position darf niemals 0 sein - wenn doch, wird der GlobalError gesetzt
  05.02.04 wl  DecapTube, PutCapInCapWaste  TN1729.1 bei FindDecapper wird "Decapping" übergeben
  05.02.04 wl  PutCapOnTube, FetchNewCap    TN1729.1 bei FindDecapper wird "Capping" übergeben
  05.02.04 wl  SetDecapperOptions           TN1729.1 die Sensoren werden für Capper UND Decapper deaktiviert
  05.02.04 wl  TubeBCReaderExists           TN1729.1 bei FindDecapper wird "Decapping" übergeben (BC-Lesen und Capping ist nicht möglich)
  05.02.04 wl  gmReadTubeBarCode            TN1729.1 bei FindDecapper wird "Decapping" übergeben (BC-Lesen und Capping ist nicht möglich)
  05.02.04 wl  RWTubeID                     TN1729.1 bei PutTubeOnDecapper wird "Capping", bei TakeTubeFromDecapper "Decapping" übergeben
  05.02.04 wl  FetchNewCap                  TN1729.3 CapDropBeforeIntake: CapRelease wird erst nach der Tube-Bewegung ausgeführt
  05.02.04 wl  PutCapInCapWaste             TN1729.3 CapDropBeforeIntake: CapIntake wird vor der Tube-Bewegung ausgeführt
  05.02.04 wl  PutCapOnTube                 TN1729.1 bei Rücktransport ohne Capping wird "Decapping" übergeben
  05.02.04 wl  RWTubeID                     TN1739   exit ersetzt - die letzten Zeilen der Funktion müssen durchlaufen werden
  12.02.04 mo  ToolVarispanFactor           TN1743   Kalkulation eines varispan Faktors in % zum Ändern des Tool-open Werts in Abhänigkeit vom Röhrchendurchmesser
  12.02.04 mo  GetTube1                     TN1743   Variable xCloseVarispan korrigiert
  19.02.04 mo  GetTube1,PutTube2            TN1754   Varispan Wert beim Tube Greifen und Absetzen kann als Fixwert aus den Rackdaten gelesen werden
  23.02.04 pk                               TN1719   gGlobalEvents renamed to gSystemEvents
  25.02.04 wl  SetDecapperOptions           TN1764   sets property UseTubeSensor of Weighing Device
  02.03.04 wl  GetMoveTubeData              TN1773   --> TubeHandlingLow
  02.03.04 wl  MoveTubeFrom/ToPos           TN1773   --> TubeHandlingLow
  02.03.04 wl  GetTube1, PutTube2           TN1773   EventList used for tube events
  02.03.04 wl  all functions                TN1773   new parameter: EventList for tube events
  12.03.04 mo  RWTubeID                     TN1808   new : optUseRackidPos
  16.03.04 wl  PutCapOnTube                 TN1823   Das Tube wird jetzt auch runtergenommen, wenn kein Capper vorhanden
  05.04.04 wl                               TN1788   Robot.HasGripper durch gGrpArm.ArmExists ersetzt
  05.04.04 wl                               TN1788   Robot.SetHandlerValues durch gGrpArm.SetZTravel_steps ersetzt
  15.04.04 mo  GetTube1                     TN1855   Berechnung von xCloseVarispan korrigiert
  10.05.04 pk                               TN1889   All function parameters changed to use TXRackPosition or TRack instead of RackName
  15.05.04 pk  gmCheckTubeID                TN1889   Bug: format string caused a crash.
  17.05.04 mo  gmReadTubeBarCode            TN1929   if gTubeRetakeRotation=0 no second ReadT move is done
  08.06.04 wl  alle Funktionen              TN1963   alle Funktionen benutzen aUsedArm (TGripperArmDevice)
  08.06.04 wl  TTubeMoveMode                TN1963   --> DevicesGrpArm
  08.06.04 wl  GetTube1,2, PutTube1,2       TN1963   --> DevicesGrpArm
  08.06.04 wl  ToolVarispanFactor           TN1963   --> DevicesGrpArm (als Variable)
  17.06.04 wl  gmReadWriteTubeID,gmTubeAction  TN1981   von ContainerHandling hierher verschoben
  22.06.04 wl  PutCapIntoCapParkRack        TN1981   Bringt die gerade abgeschraubte Kappe in die nächste freie Cap-Park-Position (Option 1024)
  22.06.04 wl  FindCapInCapParkRack         TN1981   Sucht zunächst Cap mit gleicher Tube-ID, dann die nächste neue Kappe (ohne Tube-ID)
  22.06.04 wl  FindFreePosInCapParkRack     TN1981   Findet die erste "freie" Position in einem Rack (ohne Cap)
  07.07.04 pk  gmReadWriteTubeID            TN2021   Don't ask for manual barcode entry if optUseRackIDPos
  29.07.04 wl  gmReadTubeBarCode            TN2063   Zu Anfang wird EV_BREADB ausgeführt, am Schluß EV_AREADB
  04.08.04 pk  gmReadWriteRackID            TN2080   gmReadWriteRackID takes RackOptions parameter
  16.09.04 wl  gmTubeAction,gmReadWriteRackID  TN2136.1  Access Violation abgefangen, wenn DestRack = nil
  26.10.04 pk  AskTubeIDUntilUnique         TN2195   New: Keep ask for tubeID until unique. called by gmReadWriteTubeID, gmCheckTubeID
  01.11.04 wl  TTubeHandlingStatus          TN2181   neu: beinhaltet alle Informationen für eine Tube-Aktion
  01.11.04 wl  TTubeHandling                TN2181   beinhaltet alle Tube-Handling-Aktionen, die nicht von außen aufgerufen werden
  01.11.04 wl  gmReadWriteTubeID,gmTubeAction,..  TN2181   --> TTubeHandling
  01.11.04 wl  TExtTubeHandlingStatus.WriteStatusText  TN2181  zeigt Infos über Tube- und Cap-Position, löscht Cap-Entry
  03.11.04 wl  TExtTubeHandlingStatus.WriteStatusText  TN2181  noch etwas abgeändert
  03.11.04 wl  TExtTubeHandlingStatus.WriteStatusText  TN2215  Exception bei abgebrochener Decapping-Aktion wird verhindert
  04.11.04 wl                                TN2213   uses MethodTypes
  04.11.04 wl                                TN2213   TTubeActionRunRec ersetzt TTubeParameters
  08.11.04 wl  TTubeHandlingStatusExt.TubeRecCnt  TN2213   RecCnt wird unabhängig von TubePosition gespeichert
  08.11.04 wl  BalancePutTube,BalTakeTubeAndWeigh,RWTubeID  TN2213  RecCnt wird unabhängig von TubePosition behandelt
  08.11.04 wl  TTubeHandlingStatus           TN2213   enthält alle Events als TDllCallExt
  08.11.04 wl  gmGetXTubeParamsByTubeParams  TN2213   entfernt
  08.11.04 wl  GetTube,PutTube               TN2213   Parameter: 2 mal TDllCallExt statt TSamplerEventList
  10.11.04 wl  TTubeActionModes              TN2213   --> AppTypes
  16.11.04 wl  TExtTubeHandlingStatus.MoveFromSourceToDecapper  TN2161   wenn Source-Pos = Tube-Pos des Decappers (Turntable) braucht das Tube nicht bewegt werden
  16.11.04 wl  TExtTubeHandlingStatus.MoveFromDecapperToDest  TN2161   wenn Dest.-Pos = Tube-Pos des Decappers (Turntable) braucht das Tube nicht bewegt werden
  16.11.04 wl  TTubeHandling.RWTubeID        TN2161   berücksichtigt MoveFromSourceToDecapper und MoveFromDecapperToDest
  16.11.04 wl  PutCapOnTube                  TN2161   neuer Paramter: soll das Tube hinterher aus dem Capper genommen werden?
  22.11.04 wl  TDecapperActionExt            TN2161   entfernt
  08.12.04 wl  gmAnyDecappingInOptions       TN2258   uses also new option optTubeJustDecap
  08.12.04 wl  TTubeHandling.RWTubeID        TN2258   if optTubeJustDecap is set, do not put on cap
  08.12.04 wl  TTubeHandling.FetchNewCap     TN2259   if source and dest. cap position are the same, exit (no movement)
  08.12.04 wl                                TN2246.4  uses ObjectsRun statt ObjWorkbExt
  13.12.04 wl  TTubeHandling.TakeTubeFromBalance  TN2264   die in aStatus eingelesenen Events werden an BalanceTakeTube übergeben
  14.12.04 wl  TTubeHandling.RWTubeID             TN2265   wenn Option 4096 (optTubeNoPosinfoChange) gesetzt ist -> Tubedaten in Posinfo nicht verschieben!
  14.12.04 wl  PutCapOnTube                       TN2265.1 neu: nur wenn in der Tube Options auch Capping (64 oder 128) enthalten ist, wird die Kappe aufgesetzt
  15.12.04 wl  TTubeHandling.TakeTubeFromBalance  TN2265.1 für den PutTube-Teil werden die definierten Options benutzt (nicht gMoveTubeFromBalance)
  20.12.04 wl  TExtTubeHandlingStatus.MoveFromDecapperToDest  TN2268  funktioniert jetzt auch für ReadT!
  05.01.05 wl  TTubeHandling.TubeAction           TN2282   bei UsedArm = nil tritt keine Access Violation mehr auf
  05.01.05 wl  TTubeHandling.ReadWriteTubeID      TN2282   bei UsedArm = nil tritt keine Access Violation mehr auf
  11.03.05 pk  AskTubeID                          TN2339.2  call barcode dialog via GUIManager
  27.04.05 pk                                     TN2398    try..finally inserted to make sure DataAdaptor is destroyed
  19.05.05 wl  TTubeHandlingStatus.FindCapInCapPark TN2414  wenn gCapParkStrategy = FILO werden die Rack in umgekehrter Reihenfolge durchsucht
  17.11.05 wl                                     TN2771   alle Funktionen die gGrpArm benutzt haben, haben Parameter aGripperArm
  31.03.06 thr RWTubeID  / BalTakeTubeAndWeigh    TN3007   neur Parameter in StartTare / StartWeight eingefügt
  18.07.06 pk  ReadWriteTubeID                    TN3205    new: tube option 8192 for entering  barcode manually
  07.09.06 pk                                     TN3292   PosinfoDataAdaptor instance removed
  27.10.06 pk  ReadTubeBarcode                    TN3386   Read barcode and move to next position during reading
  31.10.06 pk  gmAskTubeID                        TN3391   SystemEvent calls moved to GUIManager
  07.12.06 wl                                     TN3243    uses SamErr entfernt
  05.01.07 pk  FetchNewCap, MoveCap               TN3498   No events for cap moves
  05.01.07 pk  GetTubeFromDecapper,etc.           TN3498   No events for tube movements which are not from Status.source or to status.dest
  19.01.07 pk  GetTubeFromDecapper                TN3513   GetTube1 had been deleted by accident in changes made for TN3498
  26.01.07 pk  ReadTubeBarCode                    TN3526   No events at TubeRetake
  08.03.07 wl                                     TN3620   uses geändert
  10.07.07 mo  TTubeHandling.ReadWriteTubeID      TN3775  Manuelles BC Lesen liest nur wenn kein BC oder ein Random BC vorhanden ist
  07.08.07 wl                                     TN3811.3  TPosinfoAdapter.Create ohne Parameter
  04.09.07 pk                                     TN3847  some PosinfoDataAdaptor references changed to PosinfoDataAdaptorExt
  09.11.07 pk                                      TN3924  Steps changed to mm
  09.01.08 wl                                     TN3972   uses TubeRunStep
  22.04.08 pk                                     TN4080   use ITubeBCReaderDevice
  20.06.08 pk                                     TN4139    WB global object replaced by LayoutManager
  27.06.08 pk  RWTubeID                           TN4139   GetBalancePos moved from Layout to ZARunnerLayout
  11.07.08 wl                                         TN4164   TActionModules wird ohne "gModules as" aufgerufen
  02.09.08 pk                                     TN4215    DllCallExt changed to DllCall
  11.09.08 wl  FetchNewCap, MoveCapFromDecapper   TN4173   Bei CapIntake und CapRelease wird jetzt mit angegeben, ob eine Kappe aufliegt
  19.09.08 pk  FreeTubeFromBalance                TN4215   extracted From TubeAction
  25.09.08 wl                                     TN4242    TRunstCall ersetzt TDllCall
  13.02.09 wl  RWTubeID                           TN4429    neu: InBetweeen-Event nach dem Lesen, vor dem Zurückbringen
  13.03.09 wl                                     TN4460    Parameter aller GetTube und PutTube-Aufrufe angepasst
  04.11.09 pk                               	 TN4843   Replace TObjectList, TStringList, TStrings with Generics Lists and TStringArray
  19.10.10 wl  GeneralCheckTubeID             TN5288   wird von BCENT-Action verwendet
  19.10.10 wl  TubeAction,ReadWriteTubeID     TN5288   --> TubeRunAction
  28.10.10 wl  TTubeHandling.RWTubeID         TN5312   Verlassene Source-Positionen werden graugrün gefärbt (vorher konnten die Positionen unterschiedlich zurückbleiben)
  23.03.11 wl  TTubeHandling.RWTubeID         TN5515   MoveToZTravel zu Beginn durch gmCombinedArmsMoveToZTravel ersetzt, denn der UsedArm wird auch bei XY-Move hochgefahren
  09.04.11 wl                                 TN5545   aUsedArm.TubeBCReaderDevice statt FindTubeReadingDevice
  20.06.11 ts  ReadTubeBarCode                TN5601   Retake auch bei pneumatischem Turndevice möglich
  19.10.11 wl                                 TN5723   PaintTubes ersetzt
  20.10.11 wl                                 TN5723   TLayout.PaintPosition ersetzt
  03.11.11 wl                                 TN5725   gmCalculatePipSteps jetzt ohne Parameter
  17.11.11 wl                                 TN5729   RecCnt entfernt
  15.12.11 wl                                 TN5767   uses geändert
  28.08.12 ts                                 TN5943   unterschiedliche Z-Geschwindigkeiten für Z-Bewegungen mit Tube
  30.07.13 wl                                 TN6160   uses geändert
  20.08.13 wl                                 TN6231   an Änderungen in ToolHandling angepasst
  21.08.13 wl  gmGetRealBarcodes              TN6231   von ContainerHandling hierher
  -------------------------------------------------------------------------------------------------- }

unit TubeHandling;


interface


uses
    Generics.Collections,
    GeneralTypes,
    CommonTypes,
    AppTypes,
    Rack,
    RackTypes,
    PosinfoDataAdaptor,
    IntfArmDevice,
    OperationTube,
    MethodStepSettingRunStart,
    MotionSystemTube,
    EventManager,
    IntfDecapperDevice;

const
    cTubeWasteRack = 'TubeWaste';
    cTubeWastePos = 1;

type
    TTubeHandlingType = (tatGetFromSource, tatDecapping, tatCapping, tatGetTubeAndPutToDest);
    TTubeHandlingTypes = set of TTubeHandlingType;

    TActionStatus = class
    protected
        procedure AddLine(aLines: TList<string>; aText: string);
    public
        function DetermineStatusText(): TStringArray; virtual; abstract;
    end;

    TTubeHandlingStatus = class(TActionStatus)
    private
        // Status
        fStarted: TTubeHandlingTypes;
        fCompleted: TTubeHandlingTypes;
        fZSpeedTubeUp, fZRampTubeUp, fZSpeedTubeDown, fZRampTubeDown: integer;
        function FindCapInRack(aRack: TRack; aLastOut: boolean): TXRackPosition;
    protected
        fEvBeforeGet, fEvAfterGet, fEvInBetween, fEvBeforePut, fEvAfterPut, fEvBeforeRead,
            fEvAfterRead: TRunstCall;
        fDataAdapter: TPosinfoDataAdaptor;
        fCurrentTubePos: TXRackPosition;
        fCapTypeName: string;
    public
        constructor Create(aCurrentTubePos: TXRackPosition);
        destructor Destroy; override;
        //
        class function AnyDecappingInOptions(aTOptions: TTubeOptions): boolean;
        class function AnyCappingInOptions(aTOptions: TTubeOptions): boolean;
        class function FindDecapper(aCapTypeName: string; aAction: TDecapperAction): IDecapperDevice; static;

        procedure Start(aType: TTubeHandlingType);
        procedure Complete(aType: TTubeHandlingType);
        function DetermineStatusText(): TStringArray; override;
        //
        procedure CreateCapEntry(aOriginPos: TXRackPosition; aCapPos: TRackPosition);
        procedure DeleteCapEntry(aCapPos: TXRackPosition);
        function FindCapForTube: TXRackPosition;
        function FindCapInCapPark: TXRackPosition;
        function FindFreePosInCapPark(aCapTypeName: string): TXRackPosition;
        procedure WriteTubeID(aTubeID: string);
        function ReadTubeID: string;
        //
        property EvBeforeGet: TRunstCall read fEvBeforeGet;
        property EvAfterGet: TRunstCall read fEvAfterGet;
        property EvInBetween: TRunstCall read fEvInBetween;
        property EvBeforePut: TRunstCall read fEvBeforePut;
        property EvAfterPut: TRunstCall read fEvAfterPut;
        property EvBeforeRead: TRunstCall read fEvBeforeRead;
        property EvAfterRead: TRunstCall read fEvAfterRead;
        property TubePos: TXRackPosition read fCurrentTubePos write fCurrentTubePos;
        property CapTypeName: string read fCapTypeName;
        property ZSpeedTubeUp: integer read fZSpeedTubeUp;
        property ZRampTubeUp: integer read fZRampTubeUp;
        property ZSpeedTubeDown: integer read fZSpeedTubeDown;
        property ZRampTubeDown: integer read fZRampTubeDown;
    end;

    TExtTubeHandlingStatus = class(TTubeHandlingStatus)
    private
        // Startparameter (ReadOnly)
        fSource: TXRackPosition;
        fDest: TXRackPosition;
        fToolName: string;
        fTOptions: TTubeOptions;
        fTubeModes: TTubeActionModes;
        fSubstID: string;
    public
        constructor Create(const aGripperArmName: string; const aSRackPos: TRackPosition;
            const aDRackPos: TRackPosition; aToolName: string; aBitOptions: TTubeOptions;
            aEv: TContainerActionRunStarts; aTubeModes: TTubeActionModes; aSubstID: string;
            aZSpeedTubeUp, aZRampTubeUp, aZSpeedTubeDown, aZRampTubeDown: integer);
        //
        function DetermineStatusText(): TStringArray; override;
        procedure DefineStartPosition;
        procedure HandleNotReadTubes;
        function MoveFromSourceToDecapper(): boolean;
        function MoveFromDecapperToDest(): boolean;
        //
        property ToolName: string read fToolName;
        property TOptions: TTubeOptions read fTOptions;
        property D: TXRackPosition read fDest;
        property S: TXRackPosition read fSource;

        property TubeModes: TTubeActionModes read fTubeModes;
        property SubstID: string read fSubstID;
    end;

    TTubeHandling = class
    private
        class procedure ReadTubeBarCode(aStatus: TTubeHandlingStatus; aUsedArm: IArmDevice;
            aTubeOp: TTubeMoveOperation; aWithDecapping: boolean; var aIsDecapped: boolean;
            aTOptions: TTubeOptions; var aTubeOnDecapper: boolean; aBalFreePos: TXRackPosition);
        class procedure TakeTubeFromBalance(aStatus: TExtTubeHandlingStatus; aUsedArm: IArmDevice;
            aTubeOp: TTubeMoveOperation);
        class function IsMoveRequiredDuringBCRead(aStatus: TTubeHandlingStatus; aBalFreePos: TXRackPosition;
            out oDest: TXRackPosition): boolean;
        class function DecapTube(aTubeOp: TTubeMoveOperation; aCapTypeName: string;
            aOriginPos: TXRackPosition; aTOptions: TTubeOptions; var aTubeOnDecapper: boolean;
            aStatus: TTubeHandlingStatus): boolean; static;
        class procedure FetchNewCap(aTubeOp: TTubeMoveOperation; aStatus: TExtTubeHandlingStatus); static;
        class procedure GetTubeFromDecapper(aTubeOp: TTubeMoveOperation; aCapTypeName: string;
            aTOptions: TTubeOptions; aMTModes: TMoveTubeModes; var aTubeOnDecapper: boolean;
            aAction: TDecapperAction; aStatus: TTubeHandlingStatus); static;
        class procedure MoveCapFromDecapper(aTubeOp: TTubeMoveOperation; aCapTypeName: string;
            aTOptions: TTubeOptions; aSourcePos, aDestPos: TXRackPosition; aDecapper: IDecapperDevice;
            aStatus: TTubeHandlingStatus); static;
        class procedure PutCapIntoCapParkRack(aTubeOp: TTubeMoveOperation; aTOptions: TTubeOptions;
            aStatus: TTubeHandlingStatus); static;
        class procedure PutCapIntoCapWaste(aTubeOp: TTubeMoveOperation; aCapTypeName: string;
            aTOptions: TTubeOptions; aStatus: TTubeHandlingStatus); static;
        class procedure PutCapOnTube(aTubeOp: TTubeMoveOperation; aStatus: TTubeHandlingStatus;
            aCapTypeName, aTubeID: string; aTOptions: TTubeOptions; var aTubeOnDecapper: boolean;
            aMTModes: TMoveTubeModes; aGetTubeFromDecapper: boolean); static;
        class procedure PutTubeOnDecapper(aTubeOp: TTubeMoveOperation; aCapTypeName: string;
            aTOptions: TTubeOptions; aMTModes: TMoveTubeModes; var aTubeOnDecapper: boolean;
            aAction: TDecapperAction; aStatus: TTubeHandlingStatus); static;
        class procedure SetDecapperOptions(aTOptions: TTubeOptions; aCapTypeName: string); static;
        class procedure BalanceTakeTube(aStatus: TTubeHandlingStatus; aUsedArm: IArmDevice;
            aTubeOp: TTubeMoveOperation; aBalGetPos, aOriginGetPos: TXRackPosition; aMTModes: TMoveTubeModes;
            aTOptions: TTubeOptions);
        class function RequestTubeID(aStatus: TTubeHandlingStatus): string;
    public
        class function AskTubeIDUntilUnique(aCaption: string; aShowReadAgainBtn: boolean): string;
        class function TubeBCReaderExists(aUsedArm: IArmDevice; aCapTypeName: string): boolean;
        class procedure RWTubeID(aUsedArm: IArmDevice; aStatus: TExtTubeHandlingStatus;
            aTubeOp: TTubeMoveOperation);
        class procedure FreeTubeFromBalance(aStatus: TExtTubeHandlingStatus; aUsedArm: IArmDevice);

        class procedure GeneralBalanceTakeTube(aUsedArm: IArmDevice;
            aBalGetPos, aOriginGetPos: TXRackPosition; aMTModes: TMoveTubeModes; aTOptions: TTubeOptions);
        class function CheckTubeIDLoop(aStatus: TTubeHandlingStatus; aTubeOp: TTubeMoveOperation;
            aUsedArm: IArmDevice; const aToolName: string; aReadAgain: boolean): string;
        class function GeneralCheckTubeID(aPosition: TXRackPosition; aTubesReadAgain: boolean;
            aUsedArm: IArmDevice; const aToolName: string): string;
    end;

    // ist hier schlecht aufgehoben
procedure gmGetRealBarcodes(aTubesReadAgain: boolean; aGripperArm: IArmDevice; const aToolName: string);


implementation


uses
    Windows,
    SysUtils,
    Math,
    Controls,
    ErrorManager,
    LogManager,
    SamGlobe,
    AppSettings, // Common
    ObjModul,
    IntfBalanceDevice,
    IntfWeighingDevice,
    IntfSensorDevice,
    IntfContainerBCReaderDevice,
    LayoutManager, // Workbench
    RackWell,
    PlateHandling,
    GUIManager,
    GUIManagerSetup, // Dialogs
    ToolHandling,
    OperationFactory,
    SamHigh,
    SubstanceLoading,
    ColorUtilities,
    OperationBCReading,
    ErrorMessageFactory,
    ErrorInfo;

const
    STR_BARCODE_READAGAIN = 'READAGAIN';

procedure WriteTubeIDEntry(const aRackPos: TXRackPosition; const aTubeID: string);
var
    xDataAdaptor: TPosinfoDataAdaptor;
begin
    xDataAdaptor := TPosinfoDataAdaptor.Create();
    try
        xDataAdaptor.WriteTubeID(aTubeID, gmXRackPosToRackIDPos(aRackPos));
    finally
        xDataAdaptor.Free;
    end;
end;

procedure MoveCapEntry(const aSourceCapPos, aDestCapPos: TXRackPosition);
var
    xDataAdaptor: TPosinfoDataAdaptor;
begin
    xDataAdaptor := TPosinfoDataAdaptor.Create();
    try
        xDataAdaptor.MoveCapEntry(gmXRackPosToRackIDPos(aSourceCapPos), gmXRackPosToRackIDPos(aDestCapPos));
    finally
        xDataAdaptor.Free;
    end;
end;

// --------------------------------------------------------------------------------------------------
// Balance Tube Handling
// --------------------------------------------------------------------------------------------------
procedure BalancePutTube(aTubeOp: TTubeMoveOperation; aTubePos, aBalPutPos: TXRackPosition;
    aMTModes: TMoveTubeModes; aStatus: TExtTubeHandlingStatus);
var
    xDataAdaptor: TPosinfoDataAdaptor;
    xWeighDevice: IWeighingDevice;
    xBalPutRackIDPos, xTubeRackIDPos: TRackIDPosition;
begin
    xWeighDevice := gModules.FindWeighDevice;
    if (xWeighDevice = nil) then
        exit;
    // ------------------------ Tube auf freie Position fahren (und dessen Ursprungsposition speichern)
    if (gErrorManager.IsGlobalErr) then
        exit;
    aTubeOp.PutTube(aBalPutPos, gMoveTubeToBalance, aMTModes, xWeighDevice.TubeSensor, 0,
        aStatus.fEvBeforePut, aStatus.fEvAfterPut, aStatus.fZSpeedTubeDown, aStatus.fZRampTubeDown);
    xBalPutRackIDPos := gmXRackPosToRackIDPos(aBalPutPos);
    xTubeRackIDPos := gmXRackPosToRackIDPos(aTubePos);
    xDataAdaptor := TPosinfoDataAdaptor.Create();
    try
        xDataAdaptor.WriteTubeOrigin(xBalPutRackIDPos, xTubeRackIDPos);
    finally
        xDataAdaptor.Free;
    end;

end;

procedure BalTakeTubeAndWeigh(aTubeOp: TTubeMoveOperation;
    aBalGetPos, aOriginGetPos, aTubePos: TXRackPosition; aMTModes: TMoveTubeModes; aTOptions: TTubeOptions;
    aSubstID: string; aStatus: TExtTubeHandlingStatus);
var
    xDataAdaptor: TPosinfoDataAdaptor;
    xWeighDevice: IWeighingDevice;
    xTubeRackIDPos, xBalGetRackIDPos: TRackIDPosition;
begin
    xWeighDevice := gModules.FindWeighDevice;
    if (xWeighDevice = nil) then
        exit;
    // ----------------------------------------------------------- anderes Tube aufnehmen und wegfahren
    if (gErrorManager.IsGlobalErr) then
        exit;

    if (aBalGetPos.Pos > 0) then
    begin
        if (aOriginGetPos.Pos > 0) then
        begin
            aTubeOp.GetTube(aBalGetPos, gMoveTubeFromBalance, aMTModes, xWeighDevice.TubeSensor, 0,
                aStatus.fEvBeforeGet, aStatus.fEvAfterGet, aStatus.fZSpeedTubeUp, aStatus.fZRampTubeUp);

            if (xWeighDevice.Balance <> nil) then
            begin
                xTubeRackIDPos := gmXRackPosToRackIDPos(aTubePos);
                xWeighDevice.Balance.StartWeight(xTubeRackIDPos.RackID, xTubeRackIDPos.Pos, 'TARA', 0, 0, 0);
            end;
            xBalGetRackIDPos := gmXRackPosToRackIDPos(aBalGetPos);

            xDataAdaptor := TPosinfoDataAdaptor.Create();
            try
                xDataAdaptor.DeleteTubeOrigin(xBalGetRackIDPos);
            finally
                xDataAdaptor.Free;
            end;

            aTubeOp.PutTube(aOriginGetPos, aTOptions, [], nil, 0, aStatus.fEvBeforePut, aStatus.fEvAfterPut,
                aStatus.fZSpeedTubeDown, aStatus.fZRampTubeDown);

        end
        else
        begin
            gLogManager.Log('Error: No Destination position for tube', true);
            gErrorManager.SetGlobalErr(ERR_MODULE, 'No Destination position for tube');
            exit;
        end;
    end
    else if (xWeighDevice.Balance <> nil) then
    begin
        xTubeRackIDPos := gmXRackPosToRackIDPos(aTubePos);
        xWeighDevice.Balance.StartWeight(xTubeRackIDPos.RackID, xTubeRackIDPos.Pos, aSubstID, 0, 0, 0);
    end;
end;

// --------------------------------------------------------------------------------------------------
// Capping/Decapping
// --------------------------------------------------------------------------------------------------
class function TTubeHandlingStatus.AnyDecappingInOptions(aTOptions: TTubeOptions): boolean;
begin
    result := (optTubeDecapToWaste in aTOptions) // Kappe in Waste werfen
        or (optTubeDecapAndPutBack in aTOptions)
    // Kappe lassen wo sie ist und danach wieder aufsetzen (nur in Verbindung mit Capping)
        or (optTubeDecapToCapPark in aTOptions) // Kappe in Cap-Park-Rack für spätere Verwendung einlagern
        or (optTubeJustDecap in aTOptions); // Kappe lassen wo sie ist
end;

class function TTubeHandlingStatus.AnyCappingInOptions(aTOptions: TTubeOptions): boolean;
begin
    result := (optTubePutOnNewCap in aTOptions) // Kappe aus Cap-Park-Rack nehmen
        or (optTubeDecapAndPutBack in aTOptions);
    // Kappe lassen wo sie ist und danach wieder aufsetzen (nur in Verbindung mit Capping)
end;

class function TTubeHandlingStatus.FindDecapper(aCapTypeName: string; aAction: TDecapperAction)
    : IDecapperDevice;
var
    x: integer;
    xRack: TRack;
begin
    result := nil;
    for x := 0 to TLayoutManager.Instance.CurrentLayout.NoOfRacks - 1 do
    begin
        xRack := TLayoutManager.Instance.CurrentLayout.Racks[x];
        if (aCapTypeName <> '') and (xRack.RackStructure.CapType = aCapTypeName) then
        begin
            result := gModules.FindDecapperDevice(xRack.Name, aAction);
            if Assigned(result) then
                exit;
        end;
    end;
end;

class procedure TTubeHandling.PutTubeOnDecapper(aTubeOp: TTubeMoveOperation; aCapTypeName: string;
    aTOptions: TTubeOptions; aMTModes: TMoveTubeModes; var aTubeOnDecapper: boolean; aAction: TDecapperAction;
    aStatus: TTubeHandlingStatus);
var
    xDecapper: IDecapperDevice;
    xTubeSensor: ISensorDevice;
    xTubePosition: TXRackPosition;
    xEvBeforePut, xEvAfterPut: TRunstCall;
begin
    xDecapper := TTubeHandlingStatus.FindDecapper(aCapTypeName, aAction);

    if (not gErrorManager.IsGlobalErr) and (xDecapper <> nil) and (not aTubeOnDecapper) then
    begin

        xTubeSensor := xDecapper.TubeSensor;
        xTubePosition := TLayoutManager.Instance.CurrentLayout.FindXRackPos(xDecapper.TubePosRackName,
            xDecapper.TubePosition);

        aTubeOp.PutTubeMoveTo(xTubePosition, aTOptions, [], xTubeSensor, xDecapper.CurrentTubeRotation,
            aStatus.fZSpeedTubeDown, aStatus.fZRampTubeDown);
        // hinstellen, aber festhalten!!
        xEvBeforePut := nil;
        xEvAfterPut := nil;
        // the decapper is the dest position. this is the last time we are putting the tube, so do events.
        if not(aStatus as TExtTubeHandlingStatus).MoveFromDecapperToDest() then
        begin
            xEvBeforePut := aStatus.fEvBeforePut;
            xEvAfterPut := aStatus.fEvAfterPut;
        end;

        if xDecapper.TubeDropBeforeIntake then
        begin
            aTubeOp.PutTubeRelease(xTubePosition, aTOptions, [], xTubeSensor, xDecapper.CurrentTubeRotation,
                xEvBeforePut, xEvAfterPut); // Tube loslassen!!
            aTubeOp.PutTubeMoveFrom(xTubePosition, aTOptions, [], xTubeSensor,
                xDecapper.CurrentTubeRotation, 0, 0);
            // Tube loslassen!!
            xDecapper.TubeIntake;
        end
        else
        begin
            xDecapper.TubeIntake;
            aTubeOp.PutTubeRelease(xTubePosition, aTOptions, [], xTubeSensor, xDecapper.CurrentTubeRotation,
                xEvBeforePut, xEvAfterPut); // Tube loslassen!!
            aTubeOp.PutTubeMoveFrom(xTubePosition, aTOptions, [], xTubeSensor,
                xDecapper.CurrentTubeRotation, 0, 0);
            // Tube loslassen!!
        end;

        aTubeOnDecapper := true;
    end;
end;

class procedure TTubeHandling.GetTubeFromDecapper(aTubeOp: TTubeMoveOperation; aCapTypeName: string;
    aTOptions: TTubeOptions; aMTModes: TMoveTubeModes; var aTubeOnDecapper: boolean; aAction: TDecapperAction;
    aStatus: TTubeHandlingStatus);
var
    xDecapper: IDecapperDevice;
    xTubeSensor: ISensorDevice;
    xTubePosition: TXRackPosition;
    xEvBeforeGet, xEvAfterGet: TRunstCall;
begin
    xDecapper := TTubeHandlingStatus.FindDecapper(aCapTypeName, aAction);

    if (not gErrorManager.IsGlobalErr) and (xDecapper <> nil) and (aTubeOnDecapper) then
    begin

        // get tube from decap position
        xTubeSensor := xDecapper.TubeSensor;
        xTubePosition := TLayoutManager.Instance.CurrentLayout.FindXRackPos(xDecapper.TubePosRackName,
            xDecapper.TubePosition);
        xEvBeforeGet := nil;
        xEvAfterGet := nil;
        // the decapper is the source position. this is the first time we are getting the tube, so do events.
        if not(aStatus as TExtTubeHandlingStatus).MoveFromSourceToDecapper() then
        begin
            xEvBeforeGet := aStatus.fEvBeforeGet;
            xEvAfterGet := aStatus.fEvAfterGet;
        end;
        if xDecapper.TubeDropBeforeIntake then
        begin
            xDecapper.TubeRelease;
            aTubeOp.GetTubeMoveTo(xTubePosition, aTOptions, aMTModes, xTubeSensor,
                xDecapper.CurrentTubeRotation, 0, 0); // Tube festhalten!!
            aTubeOp.GetTubeGrip(xTubePosition, aTOptions, aMTModes, xTubeSensor,
                xDecapper.CurrentTubeRotation, xEvBeforeGet, xEvAfterGet); // Tube festhalten!!
        end
        else
        begin
            aTubeOp.GetTubeMoveTo(xTubePosition, aTOptions, aMTModes, xTubeSensor,
                xDecapper.CurrentTubeRotation, 0, 0); // Tube festhalten!!
            aTubeOp.GetTubeGrip(xTubePosition, aTOptions, aMTModes, xTubeSensor,
                xDecapper.CurrentTubeRotation, xEvBeforeGet, xEvAfterGet); // Tube festhalten!!
            xDecapper.TubeRelease;
        end;
        aTubeOp.GetTubeMoveFrom(xTubePosition, aTOptions, aMTModes, xTubeSensor,
            xDecapper.CurrentTubeRotation, aStatus.fZSpeedTubeUp, aStatus.fZRampTubeUp); // Tube wegnehmen

        aTubeOnDecapper := false;
    end;
end;

class function TTubeHandling.DecapTube(aTubeOp: TTubeMoveOperation; aCapTypeName: string;
    aOriginPos: TXRackPosition; aTOptions: TTubeOptions; var aTubeOnDecapper: boolean;
    aStatus: TTubeHandlingStatus): boolean;
var
    xDecapper: IDecapperDevice;
    xButton: integer;
begin
    result := false;
    xDecapper := TTubeHandlingStatus.FindDecapper(aCapTypeName, acnDecapping);
    if (gErrorManager.IsGlobalErr) or (xDecapper = nil) then
        exit;

    aStatus.Start(tatDecapping);

    xButton := mrRetry;
    while (not gErrorManager.IsGlobalErr) and (xButton = mrRetry) do
    begin

        if (not aTubeOnDecapper) then
            xDecapper.PrepareForDecap;

        PutTubeOnDecapper(aTubeOp, aCapTypeName, aTOptions, [], aTubeOnDecapper, acnDecapping, aStatus);

        result := true;
        xDecapper.TakeCapOff(optTubeDecapToWaste in aTOptions);
        xButton := xDecapper.CapCheck(true, (optTubeDecapToWaste in aTOptions));
        // Cap muss im Greifer sein (außer: Kappe in Waste)

        if (xButton = mrRetry) then
        begin // Nur im Fehlerfall mit Retry
            GetTubeFromDecapper(aTubeOp, aCapTypeName, aTOptions, [], aTubeOnDecapper, acnDecapping, aStatus);
        end;
    end;

    aStatus.CreateCapEntry(aOriginPos, gmMakeRackPos(xDecapper.CapPosRackName, xDecapper.CapPosition));
    aStatus.Complete(tatDecapping);
end;

class procedure TTubeHandling.PutCapOnTube(aTubeOp: TTubeMoveOperation; aStatus: TTubeHandlingStatus;
    aCapTypeName: string; aTubeID: string; aTOptions: TTubeOptions; var aTubeOnDecapper: boolean;
    aMTModes: TMoveTubeModes; aGetTubeFromDecapper: boolean);
var
    xDecapper: IDecapperDevice;
    xDataAdapter: TPosinfoDataAdaptor;
    xCapTubeID: string;
    xButton: integer;
begin
    if (gErrorManager.IsGlobalErr) then
        exit;

    xDataAdapter := TPosinfoDataAdaptor.Create();
    try
        xDecapper := TTubeHandlingStatus.FindDecapper(aCapTypeName, acnCapping);

        if (xDecapper <> nil) and TTubeHandlingStatus.AnyCappingInOptions(aTOptions)
        // 14.12.04 nur wenn Capping in den Tube Options enthalten ist!
            and xDataAdapter.CapExists(TLayoutManager.Instance.CurrentLayout.GetRackIDPos
            (gmMakeRackPos(xDecapper.CapPosRackName, xDecapper.CapPosition)), xCapTubeID)
        // check if a cap is on the cap position
        then
        begin

            if (xCapTubeID <> '') and (xCapTubeID <> aTubeID) // Tube ID of cap must fit to tube
            then
                gErrorManager.SetGlobalErr(ERR_USER, 'Tube ID of cap (' + xCapTubeID +
                    ') does not fit to tube ID (' + aTubeID + ')');

            if (not aTubeOnDecapper) then
                xDecapper.GoToBasicState;

            aStatus.Start(tatCapping);
            xButton := mrRetry;
            while (not gErrorManager.IsGlobalErr) and (xButton = mrRetry) do
            begin
                xButton := mrNone;

                PutTubeOnDecapper(aTubeOp, aCapTypeName, aTOptions, aMTModes, aTubeOnDecapper,
                    acnCapping, aStatus);

                xDecapper.PutCapOn;
                aStatus.DeleteCapEntry(TLayoutManager.Instance.CurrentLayout.FindXRackPos
                    (gmMakeRackPos(xDecapper.CapPosRackName, xDecapper.CapPosition)));

                if (not gErrorManager.IsGlobalErr) and (aTubeOnDecapper) and (aGetTubeFromDecapper) then
                begin
                    GetTubeFromDecapper(aTubeOp, aCapTypeName, aTOptions, [], aTubeOnDecapper,
                        acnCapping, aStatus);

                    // Reset Decapper After Capping
                    xDecapper.GoToBasicState;
                    xButton := xDecapper.CapCheck(false); // Cap darf nicht im Greifer sein
                end;
            end;
            aStatus.Complete(tatCapping);
        end
        else
        begin
            // nur Tube aus dem Decapper holen
            xDecapper := TTubeHandlingStatus.FindDecapper(aCapTypeName, acnDecapping);
            if (xDecapper <> nil) and (aTubeOnDecapper) and (aGetTubeFromDecapper) then
                GetTubeFromDecapper(aTubeOp, aCapTypeName, aTOptions, [], aTubeOnDecapper,
                    acnDecapping, aStatus);
        end;
    finally
        xDataAdapter.Free;
    end;
end;

class procedure TTubeHandling.FetchNewCap(aTubeOp: TTubeMoveOperation; aStatus: TExtTubeHandlingStatus);
var
    xNewCapPos, xCapPosition: TXRackPosition;
    xDecapper: IDecapperDevice;
    xButton: integer;
begin
    if (gErrorManager.IsGlobalErr) then
        exit;

    xDecapper := TTubeHandlingStatus.FindDecapper(aStatus.CapTypeName, acnCapping);
    if (xDecapper = nil) or (xDecapper.CapPosition = 0) then
    begin
        gErrorManager.SetGlobalErr(ERR_USER, 'No Decapper cap position defined');
        exit;
    end;

    xNewCapPos := aStatus.FindCapInCapPark();

    if (xNewCapPos.Pos = 0) then
    begin
        gErrorManager.SetGlobalErr(ERR_USER, 'No more caps in cap park rack!');
        exit;
    end;

    if (gErrorManager.IsGlobalErr) then
        exit;
    if (xDecapper = nil) then
        exit;

    xCapPosition := TLayoutManager.Instance.CurrentLayout.FindXRackPos(gmMakeRackPos(xDecapper.CapPosRackName,
        xDecapper.CapPosition));
    if (xCapPosition.Rack = xNewCapPos.Rack) and (xCapPosition.Pos = xNewCapPos.Pos) then
        exit;

    xButton := mrRetry;
    while (not gErrorManager.IsGlobalErr) and (xButton = mrRetry) do
    begin

        if (not xDecapper.CapDropBeforeIntake) then
            xDecapper.CapRelease(false);
        aTubeOp.GetTube(xNewCapPos, aStatus.TOptions, [], nil, 0, nil, nil, aStatus.fZSpeedTubeUp,
            aStatus.fZRampTubeUp);
        aTubeOp.PutTube(xCapPosition, aStatus.TOptions, [], nil, 0, nil, nil, aStatus.fZSpeedTubeDown,
            aStatus.fZRampTubeDown);
        if (xDecapper.CapDropBeforeIntake) then
            xDecapper.CapRelease(false);
        xDecapper.CapIntake(true);
        xButton := xDecapper.CapCheck(true); // Cap muss im Greifer sein
    end;

    if (not gErrorManager.IsGlobalErr) then
        MoveCapEntry(xNewCapPos, xCapPosition);

end;

class procedure TTubeHandling.SetDecapperOptions(aTOptions: TTubeOptions; aCapTypeName: string);
var
    xDecapper: IDecapperDevice;
    xWeigh: IWeighingDevice;
begin
    xDecapper := TTubeHandlingStatus.FindDecapper(aCapTypeName, acnDecapping);
    if (xDecapper <> nil) then
    begin
        xDecapper.UseCapSensor := not(optTubeNoCapSensor in aTOptions);
        xDecapper.UseTubeSensor := not(optTubeNoTubeSensor in aTOptions);
    end;

    xDecapper := TTubeHandlingStatus.FindDecapper(aCapTypeName, acnCapping);
    if (xDecapper <> nil) then
    begin
        xDecapper.UseCapSensor := not(optTubeNoCapSensor in aTOptions);
        xDecapper.UseTubeSensor := not(optTubeNoTubeSensor in aTOptions);
    end;

    xWeigh := gModules.FindWeighDevice;
    if (xWeigh <> nil) then
    begin
        xWeigh.UseTubeSensor := not(optTubeNoTubeSensor in aTOptions);
    end;
end;

class procedure TTubeHandling.MoveCapFromDecapper(aTubeOp: TTubeMoveOperation; aCapTypeName: string;
    aTOptions: TTubeOptions; aSourcePos, aDestPos: TXRackPosition; aDecapper: IDecapperDevice;
    aStatus: TTubeHandlingStatus);
var
    xButton: integer;
begin
    if (aDecapper <> nil) and (aDestPos.Pos <> 0) then
    begin // real movement only if cap-waste position exists
        xButton := mrRetry;
        while (not gErrorManager.IsGlobalErr) and (xButton = mrRetry) do
        begin

            aDecapper.CapRelease(true);
            if (aDecapper.CapDropBeforeIntake) then
                aDecapper.CapIntake(false);
            aTubeOp.GetTube(aSourcePos, aTOptions, [], nil, 0, nil, nil, 0, 0);
            aTubeOp.PutTube(aDestPos, aTOptions, [], nil, 0, nil, nil, 0, 0);
            if (not aDecapper.CapDropBeforeIntake) then
                aDecapper.CapIntake(false);
            xButton := aDecapper.CapCheck(false); // Cap darf nicht im Greifer sein
        end;
    end;

    // move posinfo cap entry (move cap to waste means delete posinfo entry)
    MoveCapEntry(aSourcePos, aDestPos);
end;

class procedure TTubeHandling.PutCapIntoCapWaste(aTubeOp: TTubeMoveOperation; aCapTypeName: string;
    aTOptions: TTubeOptions; aStatus: TTubeHandlingStatus);
var
    xCapWastePos, xCapPosition: TXRackPosition;
    xDecapper: IDecapperDevice;
begin
    if (gErrorManager.IsGlobalErr) then
        exit;

    xCapWastePos := TLayoutManager.Instance.CurrentLayout.FindCapWastePos(aCapTypeName);
    xDecapper := TTubeHandlingStatus.FindDecapper(aCapTypeName, acnDecapping);

    if (xDecapper <> nil) then
        xCapPosition := TLayoutManager.Instance.CurrentLayout.FindXRackPos
            (gmMakeRackPos(xDecapper.CapPosRackName, xDecapper.CapPosition));

    // Cap Movement from Decapper
    MoveCapFromDecapper(aTubeOp, aCapTypeName, aTOptions, xCapPosition, xCapWastePos, xDecapper, aStatus);
end;

class procedure TTubeHandling.PutCapIntoCapParkRack(aTubeOp: TTubeMoveOperation; aTOptions: TTubeOptions;
    aStatus: TTubeHandlingStatus);
var
    xCapParkPos, xCapPosition: TXRackPosition;
    xDecapper: IDecapperDevice;
begin
    if (gErrorManager.IsGlobalErr) then
        exit;

    xDecapper := TTubeHandlingStatus.FindDecapper(aStatus.CapTypeName, acnDecapping);
    if (xDecapper = nil) then
        exit;

    xCapParkPos := aStatus.FindFreePosInCapPark(aStatus.CapTypeName);
    if (xCapParkPos.Pos = 0) then
    begin
        gErrorManager.SetGlobalErr(ERR_USER, 'No free position in cap park rack');
        exit;
    end;

    xCapPosition := TLayoutManager.Instance.CurrentLayout.FindXRackPos(gmMakeRackPos(xDecapper.CapPosRackName,
        xDecapper.CapPosition));

    // Cap Movement from Decapper
    MoveCapFromDecapper(aTubeOp, aStatus.CapTypeName, aTOptions, xCapPosition, xCapParkPos,
        xDecapper, aStatus);
end;

// --------------------------------------------------------------------------------------------------
// Tube barcode reading (with Decapping)
// --------------------------------------------------------------------------------------------------
class function TTubeHandling.TubeBCReaderExists(aUsedArm: IArmDevice; aCapTypeName: string): boolean;
var
    xDecapper: IDecapperDevice;
begin
    result := false;

    // am Decapper angebauter Tubereader
    xDecapper := TTubeHandlingStatus.FindDecapper(aCapTypeName, acnDecapping);
    // BC-Lesen zur Zeit nicht in Verbindung mit Capping
    if (not gErrorManager.IsGlobalErr) and (xDecapper <> nil) and (xDecapper.HasTubeReader) then
        result := true;

    // kann auch vor dem Decapping mit normalen Tubereader gelesen werden
    if (aUsedArm.TubeBCReaderDevice <> nil) then
        result := true;
end;

class function TTubeHandling.IsMoveRequiredDuringBCRead(aStatus: TTubeHandlingStatus;
    aBalFreePos: TXRackPosition; out oDest: TXRackPosition): boolean;
var
    xDecapper: IDecapperDevice;
    xStatus: TExtTubeHandlingStatus;
begin
    result := false;
    if not(aStatus is TExtTubeHandlingStatus) then
        exit;

    xStatus := aStatus as TExtTubeHandlingStatus;
    if not(optTubeMoveToDestDuringBC in xStatus.TOptions) then
        exit;

    if (xStatus.TubePos.Rack.Name <> cTubeWasteRack) and TTubeHandlingStatus.AnyDecappingInOptions
        (xStatus.TOptions) then
    begin
        xDecapper := TTubeHandlingStatus.FindDecapper(xStatus.CapTypeName, acnDecapping);
        if (xDecapper <> nil) then
        begin
            result := true;
            oDest := TLayoutManager.Instance.CurrentLayout.FindXRackPos
                (gmMakeRackPos(xDecapper.TubePosRackName, xDecapper.TubePosition));
        end
    end
    else if (mdtWeighTare in xStatus.TubeModes) then
    begin
        result := true;
        oDest := aBalFreePos;
    end
    else if Assigned(xStatus.D.Rack) and ((xStatus.D.Rack <> xStatus.TubePos.Rack) or
        (xStatus.D.Pos <> xStatus.TubePos.Pos)) then
    begin
        result := true;
        oDest := xStatus.D;
    end;

end;

class procedure TTubeHandling.ReadTubeBarCode(aStatus: TTubeHandlingStatus; aUsedArm: IArmDevice;
    aTubeOp: TTubeMoveOperation; aWithDecapping: boolean; var aIsDecapped: boolean; aTOptions: TTubeOptions;
    var aTubeOnDecapper: boolean; aBalFreePos: TXRackPosition);
var
    xDecapper: IDecapperDevice;
    xBCReadDev: ITubeBCReaderDevice;
    xTubeID: string;
    xButton: integer;
    xBCReadOp: TTubeBCReadingOperation;
    xDestAfterBCRead: TXRackPosition;
begin
    xTubeID := '';
    aIsDecapped := false;
    if (gErrorManager.IsGlobalErr) then
        exit;
    if not TubeBCReaderExists(aUsedArm, aStatus.CapTypeName) then
        exit;

    // Event ausführen: EV_BREADB
    if Assigned(aStatus.EvBeforeRead) then
        aStatus.EvBeforeRead.Execute('before reading tube barcode');

    // am Decapper angebauter Tubereader: Lesen und Decappen zusammen
    xDecapper := TTubeHandlingStatus.FindDecapper(aStatus.CapTypeName, acnDecapping);
    // BC-Lesen zur Zeit nicht in Verbindung mit Capping
    if (xDecapper <> nil) and (xDecapper.HasTubeReader) then
    begin

        if (aWithDecapping) then
        begin // Decappen und lesen
            xButton := mrRetry;
            while (not gErrorManager.IsGlobalErr) and (xButton = mrRetry) do
            begin

                xDecapper.PrepareForDecap;
                PutTubeOnDecapper(aTubeOp, aStatus.CapTypeName, aTOptions, [], aTubeOnDecapper,
                    acnDecapping, aStatus);
                xTubeID := xDecapper.TakeCapOffAndRead(optTubeDecapToWaste in aTOptions);

                xButton := xDecapper.CapCheck(true, (optTubeDecapToWaste in aTOptions));
                // Cap muss im Greifer sein

                if (xButton = mrRetry) then
                begin // Nur im Fehlerfall mit Retry
                    GetTubeFromDecapper(aTubeOp, aStatus.CapTypeName, aTOptions, [], aTubeOnDecapper,
                        acnDecapping, aStatus);
                end;
            end;
            aIsDecapped := true;
        end
        else
        begin // Einfach nur lesen
            xDecapper.GoToBasicState;
            PutTubeOnDecapper(aTubeOp, aStatus.CapTypeName, aTOptions, [], aTubeOnDecapper,
                acnDecapping, aStatus);
            xTubeID := xDecapper.ReadTubeID;
        end;

        if (xTubeID <> '') then
            WriteTubeIDEntry(aStatus.TubePos, xTubeID);

        if (aIsDecapped) then
        begin
            aStatus.CreateCapEntry(aStatus.TubePos, gmMakeRackPos(xDecapper.CapPosRackName,
                xDecapper.CapPosition));
        end;
    end
    else
    begin // Barcodelesen mit normalen Tubereader (auch vor dem Decapping)
        xBCReadDev := aUsedArm.TubeBCReaderDevice;
        if (xBCReadDev <> nil) then
        begin
            if IsMoveRequiredDuringBCRead(aStatus, aBalFreePos, xDestAfterBCRead) then
            begin
                // if the bcreader is mounted on the arm we want to move to the next position and read the barcode on the
                // way to the next position
                xBCReadOp := TOperationFactory.CreateTubeBCReadingAndMoveOp(aUsedArm, xBCReadDev,
                    xDestAfterBCRead);
            end
            else
            begin
                xBCReadOp := TOperationFactory.CreateTubeBCReadingOp(aUsedArm, xBCReadDev);
            end;

            xTubeID := xBCReadOp.ReadTubeBarCode(aUsedArm.GripDevice.Tool, aStatus.TubePos.Rack.TypeName);

            // Sonderfunktion ZP02: Tube-Retake, n° versetzen und nochmal lesen
            if (gTubeRetakeRotation > 0) and (xBCReadDev.HasTurnDevice) and (xTubeID = '')
            { { TODO : Robot.hrmotor and Robot.ModuleExist(Robot.HRMotor.Adr) } then
            begin
                xBCReadDev.TurnDevice.Turn(0);
                aTubeOp.PutTube(aStatus.TubePos, aTOptions, [], nil, 0, nil, nil, aStatus.fZSpeedTubeDown,
                    aStatus.fZRampTubeDown);
                xBCReadDev.TurnDevice.Return;
                aTubeOp.GetTube(aStatus.TubePos, aTOptions, [], nil, gTubeRetakeRotation, nil, nil,
                    aStatus.fZSpeedTubeUp, aStatus.fZRampTubeUp);
                xTubeID := xBCReadOp.ReadTubeBarCode(aUsedArm.GripDevice.Tool, aStatus.TubePos.Rack.TypeName);
            end;
            xBCReadOp.Free;

            if (xTubeID <> '') then
                WriteTubeIDEntry(aStatus.TubePos, xTubeID);
        end;
    end;

    // Event ausführen: EV_AREADB
    if Assigned(aStatus.EvAfterRead) then
        aStatus.EvAfterRead.Execute('after reading tube barcode');

end;

// --------------------------------------------------------------------------------------------------
// High-Level Tube Actions
// --------------------------------------------------------------------------------------------------
class procedure TTubeHandling.RWTubeID(aUsedArm: IArmDevice; aStatus: TExtTubeHandlingStatus;
    aTubeOp: TTubeMoveOperation);
// --------------------------------------------------------------------------------------------------
// Lesen und Speichern von Röhrchenbarcodes
// Read Options: 1 - Tubes werden geschüttelt
// 2 - nicht gelesene Tubes wandern in den Waste
// 4 - Tubes werden kein zweites Mal gewogen
// 8 - Tubes werden im SaveMove-Modus bewegt
// --------------------------------------------------------------------------------------------------
var
    xTubeBC: string;
    BalFreePos, BalOccupiedPos, OriginOccupiedPos: TXRackPosition;
    xDataAdapter: TPosinfoDataAdaptor;
    xIsDecapped: boolean;
    xTubeOnDecapper: boolean;
    xBalanceDev: IBalanceDevice;
begin
    // auf Fehler prüfen (Rack ok, RackBarcode vorhanden, Positionen ok?)
    aStatus.DefineStartPosition;

    if (aStatus.TubePos.Pos > aStatus.TubePos.Rack.TubeCount) then
        exit;
    if (gErrorManager.IsGlobalErr) then
        exit;

    xTubeOnDecapper := false;
    xBalanceDev := gModules.FindBalance;

    xDataAdapter := TPosinfoDataAdaptor.Create();
    // ---------------------------------------------------------------------------------- Tubes lesen

    if not(optUseRackidPos in aStatus.TOptions) then
        gmCombinedArmsMoveToZTravel(aUsedArm); // Nadeln eines kombinierten Pipettierarms hochfahren!

    if (mdtWeighTare in aStatus.TubeModes) then
    begin
        // ---------------------------------------- freie und besetzte Balance-Positionen ermitteln
        TLayoutManager.Instance.CurrentLayout.GetBalancePos(BalFreePos, BalOccupiedPos, OriginOccupiedPos);
        // -------------------------------------------------------------------------- Tara wiegen ?
        if (xBalanceDev <> nil) and (xBalanceDev.AskTara()) then
        begin
            if (not gErrorManager.IsGlobalErr) and (BalOccupiedPos.Pos > 0) then
            begin
                xBalanceDev.OpenDoor;
            end;

            while (not gErrorManager.IsGlobalErr) and (BalOccupiedPos.Pos > 0) do
            begin // --- alle Tubes wegfahren
                BalanceTakeTube(aStatus, aUsedArm, aTubeOp, BalOccupiedPos, OriginOccupiedPos, [],
                    aStatus.TOptions);
                TLayoutManager.Instance.CurrentLayout.GetBalancePos(BalFreePos, BalOccupiedPos,
                    OriginOccupiedPos);
            end;
            xBalanceDev.StartTare(0, 0);
        end;
    end;
    // Optionen setzen für neues Tube!
    SetDecapperOptions(aStatus.TOptions, aStatus.CapTypeName);
    // ----------------------------------------------------------------------------- Tube greifen
    if not(optUseRackidPos in aStatus.TOptions) and aStatus.MoveFromSourceToDecapper() then
        aTubeOp.GetTube(aStatus.TubePos, aStatus.TOptions, [], nil, 0, aStatus.fEvBeforeGet,
            aStatus.fEvAfterGet, aStatus.fZSpeedTubeUp, aStatus.fZRampTubeUp);

    // Tube ist bereits auf dem Decapper
    if not aStatus.MoveFromSourceToDecapper() then
        xTubeOnDecapper := true;
    // ---------------------------------------------------------- Tube lesen und (evtl.) decappen
    xIsDecapped := false;
    xTubeBC := aStatus.ReadTubeID();
    if (mdtReadBC in aStatus.TubeModes) and ((xTubeBC = '') or (xDataAdapter.IsRandomTubeID(xTubeBC))) then
    begin
        if ((gUseRackIDPos) and not(mdtInsertBC in aStatus.TubeModes)) or
            (optUseRackidPos in aStatus.TOptions) then
            aStatus.WriteTubeID(aStatus.TubePos.Rack.RackID + '_' + IntToStr(aStatus.TubePos.Pos))
        else
            ReadTubeBarCode(aStatus, aUsedArm, aTubeOp,
                TTubeHandlingStatus.AnyDecappingInOptions(aStatus.TOptions), xIsDecapped, aStatus.TOptions,
                xTubeOnDecapper, BalFreePos); // Tube BC lesen und evtl. Decappen
    end;

    // Nicht gelesene Tubes behandeln
    aStatus.HandleNotReadTubes;

    // ----------------------------------------------------------- Decapping (ohne Barcode-Lesen)
    if (not xIsDecapped) and (aStatus.TubePos.Rack.Name <> cTubeWasteRack) and
        TTubeHandlingStatus.AnyDecappingInOptions(aStatus.TOptions) then
        xIsDecapped := DecapTube(aTubeOp, aStatus.CapTypeName, aStatus.TubePos, aStatus.TOptions,
            xTubeOnDecapper, aStatus);
    // ----------------------------------------------------------- auf Decapper stellen
    if (not xTubeOnDecapper) and (aStatus.TubePos.Rack.Name <> cTubeWasteRack) and
        (optTubePutOnNewCap in aStatus.TOptions) then
        PutTubeOnDecapper(aTubeOp, aStatus.CapTypeName, aStatus.TOptions, [], xTubeOnDecapper,
            acnCapping, aStatus);
    // ----------------------------------------------------------------- Zielposition umschreiben
    if Assigned(aStatus.D.Rack) and ((aStatus.D.Rack <> aStatus.TubePos.Rack) or
        (aStatus.D.Pos <> aStatus.TubePos.Pos)) then
    begin
        aStatus.TubePos.Rack.PaintTubePos(aStatus.TubePos.Pos, TRackWellDisplayType.TubeMove);
        // alte Position nicht so auffällig einfärben
        if not(optTubeNoPosinfoChange in aStatus.TOptions) then
        begin
            // Posinfo-Entries werden umgeschrieben
            TSubstanceLoading.Instance.MoveTubeAndRefreshColor(xDataAdapter, aStatus.TubePos.Rack,
                aStatus.TubePos.Pos, aStatus.D.Rack, aStatus.D.Pos);
        end;
        aStatus.TubePos := aStatus.D;
    end;

    if Assigned(aStatus.fEvInBetween) then
        aStatus.fEvInBetween.Execute('between get and put tube');

    // ---------------------------------------------------------------- Tube in die Waage stellen
    if (mdtWeighTare in aStatus.TubeModes) and (aStatus.TubePos.Rack.Name <> cTubeWasteRack) then
    begin
        if (xTubeOnDecapper) then
        begin
            GetTubeFromDecapper(aTubeOp, aStatus.CapTypeName, aStatus.TOptions, [mtmNoStartPosAfterGetTube],
                xTubeOnDecapper, acnDecapping, aStatus);
        end;
        if (xBalanceDev <> nil) then
            xBalanceDev.OpenDoor;
        if (BalFreePos.Rack = BalOccupiedPos.Rack) and (BalOccupiedPos.Pos > 0) then
        begin
            BalancePutTube(aTubeOp, aStatus.TubePos, BalFreePos, [mtmNoStartPosAfterPutTube], aStatus);
            BalTakeTubeAndWeigh(aTubeOp, BalOccupiedPos, OriginOccupiedPos, aStatus.TubePos,
                [mtmNoStartPosBeforeGetTube], aStatus.TOptions, aStatus.SubstID, aStatus);
        end
        else
        begin
            BalancePutTube(aTubeOp, aStatus.TubePos, BalFreePos, [], aStatus);
            BalTakeTubeAndWeigh(aTubeOp, BalOccupiedPos, OriginOccupiedPos, aStatus.TubePos, [],
                aStatus.TOptions, aStatus.SubstID, aStatus);
        end;
    end;
    // ------------------------------------------------------------------------ Cap in Cap-Waste
    if (xIsDecapped) and (optTubeDecapToWaste in aStatus.TOptions) and
        (aStatus.TubePos.Rack.Name <> cTubeWasteRack) then
        PutCapIntoCapWaste(aTubeOp, aStatus.CapTypeName, aStatus.TOptions, aStatus);
    // ---------------------------------- Cap in Cap-Park-Rack stellen (und für später aufheben)
    if (xIsDecapped) and (optTubeDecapToCapPark in aStatus.TOptions) and
        (aStatus.TubePos.Rack.Name <> cTubeWasteRack) then
        PutCapIntoCapParkRack(aTubeOp, aStatus.TOptions, aStatus);

    // ab jetzt soll Tube Action nicht mehr wiederholt werden
    aStatus.Start(tatGetTubeAndPutToDest);
    // ------------------------------------------------------------ Cap von Cap-Park-Rack nehmen
    if (optTubePutOnNewCap in aStatus.TOptions) and (aStatus.TubePos.Rack.Name <> cTubeWasteRack) then
        FetchNewCap(aTubeOp, aStatus);

    if not((mdtWeighTare in aStatus.TubeModes) and (aStatus.TubePos.Rack.Name <> cTubeWasteRack)) and
        not(optUseRackidPos in aStatus.TOptions) then
    begin
        // ----------------------------------------(evtl. Capping) und Tube in ZielPosition ablegen
        PutCapOnTube(aTubeOp, aStatus, aStatus.CapTypeName, aStatus.ReadTubeID(), aStatus.TOptions,
            xTubeOnDecapper, [], aStatus.MoveFromDecapperToDest());
        if aStatus.MoveFromDecapperToDest() then
            aTubeOp.PutTube(aStatus.TubePos, aStatus.TOptions, [], nil, 0, aStatus.fEvBeforePut,
                aStatus.fEvAfterPut, aStatus.fZSpeedTubeDown, aStatus.fZRampTubeDown);
        aStatus.Complete(tatGetTubeAndPutToDest);
    end;

    xDataAdapter.Free;
end;

// --------------------------------------------------------------------------------------------------
function gmAskTubeID(aCaption: string; aShowReadAgainBtn: boolean): string;
// --------------------------------------------------------------------------------------------------
// FInsCaption sollte vorher bestimmt werden
// FTubeID wird auf jeden Fall gesetzt
// --------------------------------------------------------------------------------------------------
var
    xModRes: integer;
    xTubeID, xLabelCaption, xNoRackBtnCaption: string;
begin
    xTubeID := '';
    xNoRackBtnCaption := '';

    if aShowReadAgainBtn then
        xNoRackBtnCaption := TLanguageString.Read('Read again', 'Nochmal lesen');

    xLabelCaption := TLanguageString.Read('Please enter ID for Tube: ',
        'Bitte ID für das Röhrchen eingeben:');
    xModRes := (gGUIManager as TGUIManagerSetup).Barcode_PromptInput(xTubeID, aCaption, xLabelCaption,
        xNoRackBtnCaption);
    case xModRes of
        40:
            xTubeID := STR_BARCODE_READAGAIN;
        mrNone:
            xTubeID := '';
        mrAbort:
            begin
                xTubeID := '';
                gErrorManager.SetGlobalErr(ERR_USER);
            end;
    end;
    result := xTubeID;
end;

// --------------------------------------------------------------------------------------------------
class function TTubeHandling.AskTubeIDUntilUnique(aCaption: string; aShowReadAgainBtn: boolean): string;
// --------------------------------------------------------------------------------------------------
// if xCheckUniqueness is true, the gmAskTubeID dialog is called until the global error is set
// or until a unique barcode is entered.
// otherwise if xCheckUniqueness is false, gmAskTubeID is only called once and the barcode entered is NOT
// checked for uniqueness.
// --------------------------------------------------------------------------------------------------
var
    xCheckUniqueness: boolean;
    xPosinfoDA: TPosinfoDataAdaptor;
begin
    xPosinfoDA := TPosinfoDataAdaptor.Create();
    try
        xCheckUniqueness := gTubeBarcodesAreUnique;
        while true do
        begin
            result := gmAskTubeID(aCaption, aShowReadAgainBtn);
            if result <> '' then
            begin
                if result = STR_BARCODE_READAGAIN then
                    exit;
                if (not xCheckUniqueness) or (not xPosinfoDA.TubeIDExists(result)) then
                    BREAK;
                gGUIManager.MessageBox(TLanguageString.Read('Tube ID [{0}] already exists',
                    'Röhrchen-ID [{0}] existiert bereits', [result]), '', MB_ICONSTOP + MB_OK);
            end;
            if (gErrorManager.IsGlobalErr) then
                BREAK;
        end;
    finally
        xPosinfoDA.Free;
    end;
end;

class function TTubeHandling.RequestTubeID(aStatus: TTubeHandlingStatus): string;
var
    xDataAdapter: TPosinfoDataAdaptor;
    xTubeID: string;
    xCaption: string;
    xRackIDPos: TRackIDPosition;
begin
    if (aStatus.TubePos.Pos = 0) then
    begin // aussteigen, wenn Position = 0
        raise Exception.Create('Check Tube ID - No Position');
    end;

    xRackIDPos := gmXRackPosToRackIDPos(aStatus.TubePos);

    if gErrorManager.IsGlobalErr then
        exit;

    xDataAdapter := TPosinfoDataAdaptor.Create();
    try
        xTubeID := xDataAdapter.ReadTubeID(xRackIDPos);

        if (aStatus.TubePos.Rack.Name <> cTubeWasteRack) and
            ((xTubeID = '') or TPosinfoDataAdaptor.IsRandomTubeID(xTubeID)) then
        begin
            aStatus.TubePos.Rack.PaintTubePos(aStatus.TubePos.Pos, TRackWellDisplayType.Highlight);
            xCaption := TLanguageString.Read('Position {0} in rack [{1}]', 'Position {0} in Rack [{1}]',
                [aStatus.TubePos.Pos, aStatus.TubePos.Rack.Name]);
            xTubeID := AskTubeIDUntilUnique(xCaption, false);

            if (xTubeID <> '') then
            begin
                xDataAdapter.WriteTubeID(xTubeID, xRackIDPos);
                aStatus.TubePos.Rack.PaintTubePos(aStatus.TubePos.Pos, TRackWellDisplayType.TubeMove);
            end;
        end;
    finally
        xDataAdapter.Free;
    end;

    result := xTubeID;
end;

class function TTubeHandling.CheckTubeIDLoop(aStatus: TTubeHandlingStatus; aTubeOp: TTubeMoveOperation;
    aUsedArm: IArmDevice; const aToolName: string; aReadAgain: boolean): string;
var
    i: integer;
    xIsDecapped: boolean;
    xDataAdapter: TPosinfoDataAdaptor;
    xTubeOnDecapper: boolean;
    xTubeID: string;
    xCaption: string;
    xRackIDPos: TRackIDPosition;
begin
    i := 0;

    xTubeOnDecapper := false;

    if (aStatus.TubePos.Pos = 0) then
    begin // aussteigen, wenn Position = 0
        raise Exception.Create('Check Tube ID - No Position');
    end;

    xRackIDPos := gmXRackPosToRackIDPos(aStatus.TubePos);

    xDataAdapter := TPosinfoDataAdaptor.Create();
    try
        while (i < 1) and (not gErrorManager.IsGlobalErr) do
        begin

            xTubeID := xDataAdapter.ReadTubeID(xRackIDPos);

            if (aStatus.TubePos.Rack.Name <> cTubeWasteRack) and
                ((xTubeID = '') or TPosinfoDataAdaptor.IsRandomTubeID(xTubeID)) then
            begin
                aStatus.TubePos.Rack.PaintTubePos(aStatus.TubePos.Pos, TRackWellDisplayType.Highlight);
                // lila
                xCaption := TLanguageString.Read('Position {0} in rack [{1}]', 'Position {0} in Rack [{1}]',
                    [aStatus.TubePos.Pos, aStatus.TubePos.Rack.Name]);
                xTubeID := AskTubeIDUntilUnique(xCaption, aReadAgain);

                if (xTubeID = STR_BARCODE_READAGAIN) then
                begin
                    TToolHandling.GetHandlerTool(aUsedArm, aToolName);
                    // ----------------------------------------------------------------------- Tube-Barcode lesen
                    if (not gErrorManager.IsGlobalErr) then
                        aTubeOp.GetTube(aStatus.TubePos, [], [], nil, 0, aStatus.fEvBeforeGet,
                            aStatus.fEvAfterGet, aStatus.fZSpeedTubeUp, aStatus.fZRampTubeUp);
                    if (not gErrorManager.IsGlobalErr) then
                        ReadTubeBarCode(aStatus, aUsedArm, aTubeOp, false, xIsDecapped, [], xTubeOnDecapper,
                            aStatus.TubePos);
                    if (not gErrorManager.IsGlobalErr) then
                        aTubeOp.PutTube(aStatus.TubePos, [], [], nil, 0, aStatus.fEvBeforePut,
                            aStatus.fEvAfterPut, aStatus.fZSpeedTubeDown, aStatus.fZRampTubeDown);
                end
                else if (xTubeID <> '') then
                begin
                    xDataAdapter.WriteTubeID(xTubeID, xRackIDPos);
                    aStatus.TubePos.Rack.PaintTubePos(aStatus.TubePos.Pos, TRackWellDisplayType.TubeMove);
                end;
            end
            else
                inc(i);
        end;

        if (xTubeOnDecapper) then
            GetTubeFromDecapper(aTubeOp, aStatus.TubePos.Rack.RackStructure.CapType, [], [], xTubeOnDecapper,
                acnDecapping, aStatus);
    finally
        xDataAdapter.Free;
    end;

    result := xTubeID;
end;

class procedure TTubeHandling.GeneralBalanceTakeTube(aUsedArm: IArmDevice;
    aBalGetPos, aOriginGetPos: TXRackPosition; aMTModes: TMoveTubeModes; aTOptions: TTubeOptions);
var
    xStatus: TTubeHandlingStatus;
    xTubeOp: TTubeMoveOperation;
begin

    xStatus := TTubeHandlingStatus.Create(aBalGetPos);
    xTubeOp := TOperationFactory.CreateTubeMoveOp(aUsedArm);
    try
        TTubeHandling.BalanceTakeTube(xStatus, aUsedArm, xTubeOp, aBalGetPos, aOriginGetPos, aMTModes,
            aTOptions);
    finally
        xTubeOp.Free;
        xStatus.Free;
    end;
end;

class function TTubeHandling.GeneralCheckTubeID(aPosition: TXRackPosition; aTubesReadAgain: boolean;
    aUsedArm: IArmDevice; const aToolName: string): string;
var
    xStatus: TTubeHandlingStatus;
    xTubeOp: TTubeMoveOperation;
begin

    xStatus := TTubeHandlingStatus.Create(aPosition);
    try
        if aTubesReadAgain then
        begin

            // Tubes nochmal lesen mit Gerät
            xTubeOp := TOperationFactory.CreateTubeMoveOp(aUsedArm);
            try
                result := TTubeHandling.CheckTubeIDLoop(xStatus, xTubeOp, aUsedArm, aToolName, true);
            finally
                FreeAndNil(xTubeOp);
            end;
        end
        else
        begin
            // Barcode manuell eingeben
            result := TTubeHandling.RequestTubeID(xStatus);
        end;

    finally
        FreeAndNil(xStatus);
    end;
end;

class procedure TTubeHandling.BalanceTakeTube(aStatus: TTubeHandlingStatus; aUsedArm: IArmDevice;
    aTubeOp: TTubeMoveOperation; aBalGetPos, aOriginGetPos: TXRackPosition; aMTModes: TMoveTubeModes;
    aTOptions: TTubeOptions);
var
    xDataAdaptor: TPosinfoDataAdaptor;
    xCapTypeName: string;
    xTubeOnDecapper: boolean;
    xWeighDevice: IWeighingDevice;
    xBalGetRackIDPos: TRackIDPosition;
begin

    xWeighDevice := gModules.FindWeighDevice;
    if (xWeighDevice = nil) then
        exit;
    // ----------------------------------------------------------- anderes Tube aufnehmen und wegfahren
    if (gErrorManager.IsGlobalErr) then
        exit;
    if (aBalGetPos.Pos <= 0) then
        exit;

    if (not Assigned(aOriginGetPos.Rack)) or (aOriginGetPos.Pos <= 0) then
    begin
        gLogManager.Log('Error: No Destination position for tube', true);
        gErrorManager.SetGlobalErr(ERR_MODULE, 'No Destination position for tube');
        exit;
    end;

    aTubeOp.GetTube(aBalGetPos, gMoveTubeFromBalance, aMTModes, xWeighDevice.TubeSensor, 0,
        aStatus.fEvBeforeGet, aStatus.fEvAfterGet, aStatus.fZSpeedTubeUp, aStatus.fZRampTubeUp);

    xBalGetRackIDPos := gmXRackPosToRackIDPos(aBalGetPos);
    xDataAdaptor := TPosinfoDataAdaptor.Create();
    try
        xDataAdaptor.DeleteTubeOrigin(xBalGetRackIDPos);
        // ----------------------------------------(evtl. Capping) und Tube in ZielPosition ablegen
        xCapTypeName := aOriginGetPos.Rack.RackStructure.CapType;
        xTubeOnDecapper := false;
        // Wenn das Tube von der Waage auf den Decapper gestellt wird, ist UseStartPos = false, d.h. Start-Offsets werden nicht berücksichtigt!!
        PutCapOnTube(aTubeOp, aStatus, xCapTypeName,
            xDataAdaptor.ReadTubeID(gmXRackPosToRackIDPos(aOriginGetPos)), aTOptions, xTubeOnDecapper,
            [mtmNoStartPosBeforePutTube], true);
        aTubeOp.PutTube(aOriginGetPos, aTOptions, [], nil, 0, aStatus.fEvBeforePut, aStatus.fEvAfterPut,
            aStatus.fZSpeedTubeDown, aStatus.fZRampTubeDown);
    finally
        xDataAdaptor.Free;
    end;
end;

class procedure TTubeHandling.TakeTubeFromBalance(aStatus: TExtTubeHandlingStatus; aUsedArm: IArmDevice;
    aTubeOp: TTubeMoveOperation);
var
    BalFreePos, BalOccupiedPos, OriginOccupiedPos: TXRackPosition;
    xBalanceDev: IBalanceDevice;
begin
    TLayoutManager.Instance.CurrentLayout.GetBalancePos(BalFreePos, BalOccupiedPos, OriginOccupiedPos);
    if ((BalOccupiedPos.Rack <> OriginOccupiedPos.Rack) or (BalOccupiedPos.Pos <> OriginOccupiedPos.Pos)) and
        (not gErrorManager.IsGlobalErr) then
    begin
        xBalanceDev := gModules.FindBalance;
        if (xBalanceDev <> nil) then
            xBalanceDev.OpenDoor;
        BalanceTakeTube(aStatus, aUsedArm, aTubeOp, BalOccupiedPos, OriginOccupiedPos, [], aStatus.TOptions);
    end;
end;

class procedure TTubeHandling.FreeTubeFromBalance(aStatus: TExtTubeHandlingStatus; aUsedArm: IArmDevice);
var
    xTubeOp: TTubeMoveOperation;
begin
    xTubeOp := TOperationFactory.CreateTubeMoveOp(aUsedArm);
    // letztes Tube aus der Waage nehmen
    TakeTubeFromBalance(aStatus, aUsedArm, xTubeOp);
    xTubeOp.Free;
end;

{ TActionStatus }

procedure TActionStatus.AddLine(aLines: TList<string>; aText: string);
begin
    aLines.Add('');
    aLines.Add(aText);
    gLogManager.Log(aText, true);
end;

// --------------------------------------------------------------------------------------------------
{ TTubeHandlingStatus }

constructor TTubeHandlingStatus.Create(aCurrentTubePos: TXRackPosition);
begin
    inherited Create;

    fStarted := [];
    fCompleted := [];
    fDataAdapter := TPosinfoDataAdaptor.Create();

    fCurrentTubePos := aCurrentTubePos;
    fCapTypeName := '';
    if Assigned(fCurrentTubePos.Rack) then
        fCapTypeName := fCurrentTubePos.Rack.RackStructure.CapType;
end;

destructor TTubeHandlingStatus.Destroy;
begin
    fDataAdapter.Free;
    fEvBeforeGet.Free;
    fEvAfterGet.Free;
    fEvInBetween.Free;
    fEvBeforePut.Free;
    fEvAfterPut.Free;
    fEvBeforeRead.Free;
    fEvAfterRead.Free;

    inherited;
end;

procedure TTubeHandlingStatus.Complete(aType: TTubeHandlingType);
begin
    if (gErrorManager.IsGlobalErr) then
        exit;
    fCompleted := fCompleted + fStarted;
end;

procedure TTubeHandlingStatus.Start(aType: TTubeHandlingType);
begin
    if (gErrorManager.IsGlobalErr) then
        exit;
    fStarted := fStarted + [aType];
end;

function TTubeHandlingStatus.DetermineStatusText(): TStringArray;
begin
    SetLength(result, 0);
    // schreibt nichts - wird auch nicht benutzt
end;

function TTubeHandlingStatus.ReadTubeID: string;
var
    xTubeRackIDPos: TRackIDPosition;
begin
    xTubeRackIDPos := gmXRackPosToRackIDPos(fCurrentTubePos);
    result := fDataAdapter.ReadTubeID(xTubeRackIDPos);
end;

procedure TTubeHandlingStatus.WriteTubeID(aTubeID: string);
var
    xTubeRackIDPos: TRackIDPosition;
begin
    xTubeRackIDPos := gmXRackPosToRackIDPos(fCurrentTubePos);
    fDataAdapter.WriteTubeID(aTubeID, xTubeRackIDPos);
end;

procedure TTubeHandlingStatus.CreateCapEntry(aOriginPos: TXRackPosition; aCapPos: TRackPosition);
var
    xTubeID: string;
    xTubePosition, xCapPosition: TRackIDPosition;
begin
    if (gErrorManager.IsGlobalErr) then
        exit;

    xTubePosition := gmXRackPosToRackIDPos(aOriginPos);
    xCapPosition := gmXRackPosToRackIDPos(TLayoutManager.Instance.CurrentLayout.FindXRackPos(aCapPos));

    // read tube ID or create a random barcode
    xTubeID := fDataAdapter.ReadTubeID(xTubePosition);
    if (xTubeID = '') then
        fDataAdapter.WriteTubeID(fDataAdapter.CreateRandomTubeID, xTubePosition);

    // enter cap in posinfo.db
    fDataAdapter.WriteCapEntry(xTubePosition);
    fDataAdapter.MoveCapEntry(xTubePosition, xCapPosition);
end;

procedure TTubeHandlingStatus.DeleteCapEntry(aCapPos: TXRackPosition);
var
    xDummyPos: TRackIDPosition;
begin
    // move cap to '' means delete posinfo entry
    xDummyPos.RackID := '';
    fDataAdapter.MoveCapEntry(gmXRackPosToRackIDPos(aCapPos), xDummyPos);
end;

function TTubeHandlingStatus.FindCapForTube: TXRackPosition;
var
    xTubeID: string;
begin
    result.Pos := 0;

    // Find cap with the same ID than the tube
    xTubeID := self.ReadTubeID;
    if (xTubeID <> '') then
    begin
        result := TSubstanceLoading.Instance.FindCapEntry(TLayoutManager.Instance.CurrentLayout, xTubeID,
            TLayoutManager.Instance.CurrentLayout.LayoutRunName);
    end;
end;

function TTubeHandlingStatus.FindCapInRack(aRack: TRack; aLastOut: boolean): TXRackPosition;
begin
    result.Pos := 0;
    if (not aRack.IsCapParkRack) then
        exit; // rack must be a cap park rack
    if (aRack.RackStructure.CapType <> fCapTypeName) then
        exit; // check cap type

    result.Rack := aRack;
    result.Pos := fDataAdapter.FindFirstNewCap(aRack.RackID, aLastOut);
end;

function TTubeHandlingStatus.FindCapInCapPark: TXRackPosition;
var
    x: integer;
    xLastOut: boolean;
begin
    // 1. Find cap with the same ID than the tube
    result := FindCapForTube;
    if (result.Pos <> 0) then
        exit;

    // Get Strategy from global Variable:
    xLastOut := false;
    if (Length(gCapParkStrategy) > 3) and (gCapParkStrategy[3] = 'L') then
        xLastOut := true;

    // 2. Find a new cap in cap park rack
    if (xLastOut) then
    begin
        for x := TLayoutManager.Instance.CurrentLayout.NoOfRacks - 1 downto 0 do
        begin // Last out: beim letzten Rack anfangen!
            result := FindCapInRack(TLayoutManager.Instance.CurrentLayout.Racks[x], xLastOut);
            if (result.Pos <> 0) then
                exit;
        end;
    end
    else
    begin
        for x := 0 to TLayoutManager.Instance.CurrentLayout.NoOfRacks - 1 do
        begin
            result := FindCapInRack(TLayoutManager.Instance.CurrentLayout.Racks[x], xLastOut);
            if (result.Pos <> 0) then
                exit;
        end;
    end;
end;

function TTubeHandlingStatus.FindFreePosInCapPark(aCapTypeName: string): TXRackPosition;
var
    x: integer;
begin
    result.Pos := 0;

    // Find free position in cap park rack
    for x := 0 to TLayoutManager.Instance.CurrentLayout.NoOfRacks - 1 do
    begin
        if (not TLayoutManager.Instance.CurrentLayout.Racks[x].IsCapParkRack) then
            continue; // rack must be a cap park rack
        if (TLayoutManager.Instance.CurrentLayout.Racks[x].RackStructure.CapType <> aCapTypeName) then
            continue; // check cap type

        result.Rack := TLayoutManager.Instance.CurrentLayout.Racks[x];
        result.Pos := fDataAdapter.FindFirstCapFreePos(result.Rack.RackID, result.Rack.TubeCount);
        if (result.Pos <> 0) then
            BREAK;
    end;
end;

{ TExtTubeHandlingStatus }

constructor TExtTubeHandlingStatus.Create(const aGripperArmName: string; const aSRackPos: TRackPosition;
    const aDRackPos: TRackPosition; aToolName: string; aBitOptions: TTubeOptions;
    aEv: TContainerActionRunStarts; aTubeModes: TTubeActionModes; aSubstID: string;
    aZSpeedTubeUp, aZRampTubeUp, aZSpeedTubeDown, aZRampTubeDown: integer);
begin
    inherited Create(TLayoutManager.Instance.CurrentLayout.FindXRackPos(aSRackPos));

    fToolName := aToolName;
    fTOptions := aBitOptions;
    fSource := TLayoutManager.Instance.CurrentLayout.FindXRackPos(aSRackPos);
    fDest := TLayoutManager.Instance.CurrentLayout.FindXRackPos(aDRackPos);

    fTubeModes := aTubeModes;
    fSubstID := aSubstID;

    fEvBeforeGet := TEventManager.Instance.CreateEventIfNecessary(aEv.BeforeGet);
    fEvAfterGet := TEventManager.Instance.CreateEventIfNecessary(aEv.AfterGet);
    fEvInBetween := TEventManager.Instance.CreateEventIfNecessary(aEv.InBetween);
    fEvBeforePut := TEventManager.Instance.CreateEventIfNecessary(aEv.BeforePut);
    fEvAfterPut := TEventManager.Instance.CreateEventIfNecessary(aEv.AfterPut);
    fEvBeforeRead := TEventManager.Instance.CreateEventIfNecessary(aEv.BCBeforeRead);
    fEvAfterRead := TEventManager.Instance.CreateEventIfNecessary(aEv.BCAfterRead);

    fZRampTubeUp := aZRampTubeUp;
    fZSpeedTubeUp := aZSpeedTubeUp;
    fZRampTubeDown := aZRampTubeDown;
    fZSpeedTubeDown := aZSpeedTubeDown;
    // CurrentPos und CapTypeName werden in DefineStartPosition() überschrieben!
end;

function TExtTubeHandlingStatus.DetermineStatusText(): TStringArray;
var
    xText2, xErrorReason: string;
    xCapPosition: TXRackPosition;
    xDeleteCapEntry: boolean;
    xLines: TList<string>;
begin
    xLines := TList<string>.Create();
    try
        xErrorReason := gErrorManager.GetAndDeleteErrorReason;
        if (xErrorReason <> '') then
            AddLine(xLines, xErrorReason)
        else
            AddLine(xLines, TLanguageString.Read('The run has been interrupted during a tube action',
                'Der Lauf wurde während einer Tube-Aktion unterbrochen'));

        xCapPosition := FindCapForTube;
        xDeleteCapEntry := false;

        xText2 := '';
        if (tatGetTubeAndPutToDest in fStarted) then
        begin // größter Teil des Tube-Handlings bereits abgeschlossen
            if AnyCappingInOptions(fTOptions) then
            begin
                xText2 := TLanguageString.
                    Read('Please close the tube with cap and put it to destination position',
                    'Bitte schrauben Sie das Tube wieder zu und stellen es auf die Zielposition');
                xDeleteCapEntry := true;
            end
            else
                xText2 := TLanguageString.Read('Please put the tube to destination position',
                    'Bitte stellen Sie das Tube auf die Zielposition');

            if Assigned(fDest.Rack) then
                xText2 := xText2 + #13 + '   ' + TLanguageString.
                    Read('Rack >{0}< position >{1}<', 'Rack >{0}< Position >{1}<',
                    [fDest.Rack.Name, fDest.Pos]);
        end
        else
        begin
            if TTubeHandlingStatus.AnyDecappingInOptions(fTOptions) then
            begin
                xText2 := TLanguageString.
                    Read('Please close the tube with cap and put it back to the source position',
                    'Bitte schrauben Sie das Tube wieder zu und stellen es zurück auf die Startposition');
                xDeleteCapEntry := true;
            end
            else
                xText2 := TLanguageString.Read('Please put the tube back to source position',
                    'Bitte stellen Sie das Tube zurück auf die Startposition');

            if Assigned(fSource.Rack) then
                xText2 := xText2 + #13 + '   ' + TLanguageString.
                    Read('Rack >{0}< position >{1}<', 'Rack >{0}< Position >{1}<',
                    [fSource.Rack.Name, fSource.Pos]);
        end;

        AddLine(xLines, xText2);

        if (xDeleteCapEntry) and (xCapPosition.Pos <> 0) and Assigned(xCapPosition.Rack) then
        begin
            AddLine(xLines, TLanguageString.Read('The cap is on rack >{0}< position >{1}<',
                'Die Kappe steht auf Rack >{0}< Position >{1}<', [xCapPosition.Rack.Name, xCapPosition.Pos]));
            DeleteCapEntry(xCapPosition);
        end;

        result := xLines.ToArray();
    finally
        FreeAndNil(xLines);
    end;
end;

procedure TExtTubeHandlingStatus.DefineStartPosition;
begin
    // auf Fehler prüfen (Rack ok, RackBarcode vorhanden, Positionen ok?)
    if (fSource.Pos < 1) then
        fSource.Pos := 1;

    if (gDbToolDllName = '') then
    begin
        if (fDest.Rack = nil) or (fDest.Rack.Name = '') then
            fDest.Rack := fSource.Rack;
        if (fDest.Pos < 1) then
            fDest.Pos := 1;
    end;

    // fCurrentTubePos bestimmen
    fCurrentTubePos := fSource;

    // Cap Type bestimmen
    fCapTypeName := fCurrentTubePos.Rack.RackStructure.CapType;
end;

procedure TExtTubeHandlingStatus.HandleNotReadTubes;
var
    xTubeBC: string;
begin
    xTubeBC := self.ReadTubeID;

    if (xTubeBC <> '') then
        exit;
    if not(mdtReadBC in fTubeModes) then
        exit;

    fCurrentTubePos.Rack.PaintTubePos(fCurrentTubePos.Pos, TRackWellDisplayType.Highlight);

    if (optTubeNotReadIntoWaste in fTOptions) then
    begin

        // Tube in Waste
        fCurrentTubePos.Rack.PaintTubePos(fCurrentTubePos.Pos, TRackWellDisplayType.default);
        fDest := TLayoutManager.Instance.CurrentLayout.FindXRackPos
            (gmMakeRackPos(cTubeWasteRack, cTubeWastePos)); // Zielpos = Wastepos
    end
    else if (g24hMode > 0) and (not gErrorManager.IsGlobalErr) then
    begin

        // Random-ID in PosInfo schreiben (für Source-Pos)
        xTubeBC := fDataAdapter.CreateRandomTubeID;
        self.WriteTubeID(xTubeBC);
    end;
end;

function TExtTubeHandlingStatus.MoveFromDecapperToDest(): boolean;
var
    xCapping, xDecapping: boolean;
    xDecapper: IDecapperDevice;
begin
    result := true;
    xCapping := TTubeHandlingStatus.AnyCappingInOptions(fTOptions);

    if (xCapping) then
    begin
        xDecapper := TTubeHandlingStatus.FindDecapper(fCapTypeName, acnCapping);
    end
    else
    begin
        xDecapping := TTubeHandlingStatus.AnyDecappingInOptions(fTOptions);
        if not(xDecapping) then
            exit;
        if (mdtWeighTare in fTubeModes) then
            exit; // Bei Decapping darf es ReadT/MoveT sein, aber keine Wäge-Actions
        xDecapper := TTubeHandlingStatus.FindDecapper(fCapTypeName, acnDecapping);
    end;
    if (xDecapper = nil) then
        exit;

    // DestPos muß der Decapper.TubePos entsprechen
    if (xDecapper.TubePosRackName <> fDest.Rack.Name) or (xDecapper.TubePosition <> fDest.Pos) then
        exit;

    result := false;
end;

function TExtTubeHandlingStatus.MoveFromSourceToDecapper(): boolean;
var
    xCapping, xDecapping: boolean;
    xDecapper: IDecapperDevice;
begin
    result := true;
    xDecapping := AnyDecappingInOptions(fTOptions);

    if (xDecapping) then
    begin
        xDecapper := FindDecapper(fCapTypeName, acnDecapping);
    end
    else
    begin
        xCapping := AnyCappingInOptions(fTOptions);
        if not(xCapping) then
            exit;
        if (fTubeModes <> []) then
            exit; // bei Capping darf es nur ein MoveTube sein
        xDecapper := FindDecapper(fCapTypeName, acnCapping);
    end;
    if (xDecapper = nil) then
        exit;

    // SourcePos muß der Decapper.TubePos entsprechen
    if (xDecapper.TubePosRackName <> fSource.Rack.Name) or (xDecapper.TubePosition <> fSource.Pos) then
        exit;

    result := false;
end;

// --------------------------------------------------------------------------------------------------
procedure gmGetRealBarcodes(aTubesReadAgain: boolean; aGripperArm: IArmDevice; const aToolName: string);
// --------------------------------------------------------------------------------------------------
// Rückgabewert: wenn FALSE, ist eines der gelesenen Racks nicht vorhanden
// --------------------------------------------------------------------------------------------------
var
    x, xTube: integer;
    xPositions: TRackPositions;
    xRack: TRack;
begin
    // replace virtual rack ID's
    for x := 0 to TLayoutManager.Instance.CurrentLayout.NoOfRacks - 1 do
    begin
        if (gErrorManager.IsGlobalErr) then
            EXIT;
        xRack := TLayoutManager.Instance.CurrentLayout.Racks[x];

        if (Pos(cBCRandom, xRack.RackID) = 1) then
        begin
            gmReadWriteRackID(nil, xRack, [mdShowErrMsg, mdInsertOnly]);
        end;
    end;

    // replace virtual tube ID's
    for x := 0 to TLayoutManager.Instance.CurrentLayout.NoOfRacks - 1 do
    begin
        xRack := TLayoutManager.Instance.CurrentLayout.Racks[x];

        xPositions := xRack.FindRandomTubeBC();
        for xTube := 0 to high(xPositions) do
        begin
            if (gErrorManager.IsGlobalErr) then
                EXIT;
            TTubeHandling.GeneralCheckTubeID(TLayoutManager.Instance.CurrentLayout.FindXRackPos
                (xPositions[xTube].Rack, xPositions[xTube].Pos, true), aTubesReadAgain, aGripperArm,
                aToolName);
        end;
    end;
end;


end.
