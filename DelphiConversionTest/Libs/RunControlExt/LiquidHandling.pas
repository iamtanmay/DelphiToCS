{ --------------------------------------------------------------------------------------------------
  Copyright © 2003 Zinsser Analytic GmbH - All rights reserved.
  Author       : Wolfgang Lyncke (wl)
  Description  : All liquid handling functions including aspirate, dispense and washing
  --------------------------------------------------------------------------------------------------
  Revision History:
  date     op  method                       track-no improvement/change
  -------- --  ---------------------------  -------- -----------------------------------------------
  03.09.03 wl                               TN1526   initial version
  23.09.03 wl  gmAspirate,gmDispense...     TN1580   alle High Level Liquid Handling Routinen übersetzt
  24.09.03 wl  gmAspirate,gmDispense...     TN1580   Logs korrigiert
  29.09.03 wl  gmAspirate,gmDispense...     TN1580   weitere Fehler behoben
  07.10.03 wl  gmFetchTips                  TN1526   DroZOfs wurde und wird nie benutzt
  15.10.03 wl  gmPickLiquid, gmDispLiquid   TN1624   benutzt das TPipPumpDevice
  15.10.03 wl  gmDispPeriPump               TN1624   benutzt das TPipPumpDevice
  17.10.03 wl  gmAspirate,gmDispense        TN1489   Volumenkorrektur jetzt auch bei den neuen Funktionen
  10.11.03 wl  gmAspirate                   TN1656.2 RetractPos: integer statt single (führte zu Fehler)
  12.11.03 wl  gmAspirate,gmDispense        TN1659.1 um mit ZP02 individuelle y-Positionen anzufahren: CalculatePipSteps_WithVarispan ersetzt
  10.12.03 wl  alle Funktionen              TN1672   Robot-Move-Aufrufe ersetzt durch gPipArm-Methoden
  16.12.03 wl  gmDropTips                   TN1672   UseCorridor wird mit übergeben
  19.12.03 wl                               TN1672   MoveSingleZ - Parameter geändert
  04.02.04 pk                               TN1719   functions moved into this unit from old Thread units( ExtLiq, BasicLig, etc)
  05.02.04 wl  gmDropDitiTips               TN1734   Übergangslösung für ZP02, da GetOKTips anders implementiert ist
  05.02.04 wl  gmFetchTips                  TN1734   DiTi-Aufnahme und DiTi-Check anders für ZP02
  05.02.04 wl  gmDropTips                   TN1734   Abwerfen der Spitzen anders für ZP02
  27.02.04 pk  gmFetchTips                  TN1751   Call gmIsZTravel to reset the FLastRackName variable
  02.03.04 wl  gmDropTips, gmFetchTips      TN1773   --> LiquidHandlingLow
  02.03.04 wl  gmFlushSyringe               TN1773   moved here from SamCmd
  02.03.04 wl  gmLiquidDetection            TN1773   moved here from SamCmd
  02.03.04 wl  gmAspirate, gmDispense       TN1773   uses new parameter aEventList for Events (EV_BASP,EV_BPICKLQ,..)
  12.03.04 wl                               TN1812   alle MoveXY-Befehle: statt ZTravel-Wert wird aktuelles Rack übereben
  12.03.04 wl                               TN1812   alle Aufrufe von gmIsZTravel entfernt
  05.04.04 wl  gmAspirate,gmDispense        TN1708   ZScanMode, ZScanSpeed und ZRetractSpeed als Parameter
  05.04.04 wl  gmInitSyringes,gmFlushPeripump,gmAcetoneWash,gmCalcRestVol,gmDry  TN1788  von SamCmd hierher
  05.04.04 wl  alle Funktionen!             TN1788   aUsedArm wird immer als Parameter übergeben und immer benutzt (wenn irgend möglich)
  05.04.04 wl  gmPickLiquid, gmDispLiquid   TN1788   benutzt Tips[].PipPump1/2 statt gModules.FindPipPump
  05.04.04 wl  gmNewStationCoordinates      TN1788   entfernt - Daten werden nicht mehr umgeschrieben, sondern aus Rack-Objekt genommen
  06.04.04 mo  gmDispense                   TN1847   CorrectVol muss mit aAirVol aufgerufen werden
  08.04.04 wl  gmFlushSyringe               TN1788   MAX_TIPS durch aUsedArm.TipCount ersetzt (sonst kracht es)
  20.04.04 wl  gmCalcRestVol,gmFlushSyringe TN1788   noch einmal debuggt
  20.04.04 wl  alle Funktionen              TN1788   TipIntegerArray statt TipWordArray
  20.04.04 wl  gmInitializeSyringes         TN1788   gTipManager.FindArmToUse statt WB.Arm.FindArmToUse
  22.04.04 wl  gmDropDTips                  TN1788   --> LiquidHandlingDiTi
  22.04.04 wl  gmPickLiquid,gmDispLiquid,gmCalcRestVol,gmFlushSyringe   TN1788   --> LiquidHandlingLow
  22.04.04 wl  gmWashing,gmFlushPeripump    TN1788   --> LiquidHandlingLow
  28.04.04 wl  gmInitializeSyringes         TN1788   --> SubstanceHandling
  04.05.04 tbh gmDry                        TN1274   Vakuum der Dry-Station stoppt erst nach Ausfahrt der Nadeln
  04.05.04 tbh gmDry                        TN1704   Dry-Station wird auch ohne Tip1 korrekt angefahren
  05.05.04 wl  gmInitSyringes               TN1788   AddDilutors initialisieren -> gmInitializeSyringes (SubstanceHandling)
  10.05.04 pk  gmDry                        TN1889.1 New overloaded : if without TRack param, then use the default DRY rack.
  11.05.04 wl  gmLiquidWash                 TN1788   Fehler korrigiert
  13.05.04 wl  gmAspirate                   TN1788   Parameter aAspPos wurde nicht benutzt -> entfernt
  13.05.04 wl  gmDispense                   TN1656.6 MixMode 0: es werden jetzt korrekterweise auch xTTips als "Tracking"-Tips übergeben
  14.05.04 wl  gmDispense                   TN1656.7 MixMode 0 und 1: Anfahren der Positionen mit gmMoveAspPos2 statt gmMoveDspPos2, da danach auch aspiriert wird
  14.05.04 wl  gmPickLiq_from_Zmax_with_Retract TN1922.2 durch Abs(RetractPos) wird der Retract-Wert immer nach oben gefahren, da unter Z-Max niemals gefahren werden darf
  17.05.04 wl  gmAspirate,gmDispense        TN1933   aUseVolControl als neuer Parameter
  08.06.04 wl  LoadDilutors1To4             TN1963   aUsedArm.Execute statt _Execute
  23.06.04 tbh gmDry                        TN2005   Dry-Station wird auch ohne Tip1 korrekt angefahren
  29.06.04 wl  gmDoWashMacro                TN1985   ResetLastRack (statt MovetoZTravel), damit nicht das Sourcerack mit Rack-ZTravel angefahren wird
  01.07.04 wl                               TN1963   Anpassung an Änderungen in TPipStep
  08.07.04 wl  gmDispense                   TN1915.1 wenn SingleRetract, keine Transport-Air und weitere Hübe mit Systemfl. notwendig, fahren die Tips nach Dispense nicht auf Z-Travel!
  09.07.04 wl  gmDoWashMacro                TN2040   --> SubstanceHandling
  09.07.04 wl  gmLiquidWash                 TN2040   Aufruf von gmDry von SubstanceHandling hierher
  22.07.04 tbh gmAspirate                   TN1972   SpitBack vor Aufnahme von TransportAirGap
  29.07.04 tbh gmDispLiq, gmPickLiq             TN2058   Volumen und in POSINFO dokumentiertes Volumen getrennt
  29.07.04 tbh gmPickLiq_from_Zmax_with_Retract TN2058   Volumen und in POSINFO dokumentiertes Volumen getrennt
  29.07.04 tbh gmAspirate                       TN2058   Aufrufe angepasst und SpitBack bei Volumenkorrektur korrigiert
  29.07.04 tbh gmDispense                       TN2058   Aufrufe angepasst
  29.07.04 tbh gmAspirate                       TN2058.1 Anpassung Volumen für Spitback korrigiert
  18.08.04 wl  gmDoLiquidWash,GetWashAtWasteVol TN2079   WashAtWasteVol wird aus Area ROBOT gelesen
  25.08.04 wl                               TN2105   ZScanspeed heißt jetzt ZInsertspeed
  01.09.04 pk                               TN2114 gmInitSyringes, gmDispLiq, gmDispense : Use TipCount - 1
  08.09.04 wl                               TN2121   SamHigh.gm-Funktinen statt global_AppIntf.-Funktionen
  08.09.04 wl  gmPickLiq,gmPickLiq_from_Zmax_with_Retract  TN2121   ruft gmStoreAsp auf, mit dem Zusatz asptLiquid
  08.09.04 wl  gmPickAir                    TN2121   global_appIntf.AspLiquid entfernt: Air wird nicht gespeichert
  08.09.04 wl  gmPickSys                    TN2121   jetzt mit Channel2 & WaitAfterPickSys/ gmLogText, ChangeActualVol ergänzt
  11.09.04 wl  gmDropDispTips               TN2123   ermittelt Positionen für DropRack mit CalcSingelStepRack
  11.09.04 wl  gmAspirate,gmDispense        TN2123   geänderte Aufrufe von MoveZ (MposArray statt MPos)
  11.09.04 wl  gmPickLiq,gmDispLiq          TN2123  Berecnungen ohne aUsedArm.UseTip_ZOffset_mm
  11.09.04 wl  gmDoLiquidWash,gmDry,gmInitSyringes,gmAcetoneWash  TN2123 Benutzt aUsedArm.CalcSingleStepRack statt gmMoveToWashOrWasteRack
  13.10.04 wl  gmMoveAspPos2,gmMoveDspPos2,gmDetectLiq,gmGetVol,gmMoveToLowestTip  TN2151 aus SamHigh hierher verschoben
  13.10.04 wl  gmRetractAndGetTransportAir  TN2151   Retract-Vorgang aus Aspirate UND Dispense vereinheitlicht
  13.10.04 wl  gmMoveRetractDistance        TN2151   neu: wenn (RetractDistance > 0) wird eine best. Strecke mit Retract Speed gefahren
  14.10.04 wl                               TN2178   LQModes: Bezeichner mit INT_LQMODES_
  18.10.04 wl  gmAspirate,gmDispense        TN2177   bei MoveXY wird nicht aSyrMap sondern die Syrmap des Einzelschrittes übergeben - dadurch fahren unbenutzte Tips auf global-ZTravel
  08.11.04 wl  gmAspirate,gmDispense        TN2213   Events werden als TExtDllCall übergeben
  01.02.05 wl  gmDoLiquidWash               TN2297.3 DryAfterWash als Parameter
  03.02.05 wl  gmMoveAspPos2,gmMoveDspPos2  TN2297.7 Beschränkung auf TPipArm aufgehoben
  16.02.05 wl  gmDispPeripump               TN2269   --> LiquidHandlingLow
  16.02.05 wl  gmPickSys                    TN2269   Update der Füllstände entfernt
  21.02.05 wl  alle Funktionen              TN1960   gCommManager.ErrBox & AppSleep statt global_AppInterface-Methode
  22.02.05 wl  gmDoLiquidWash               TN2323   für DryAfterWash gibt es 3 Möglichkeiten: ja, nein, nach TipType-Parameter "DryAfterFlush" entscheiden
  04.03.05 pk                               TN2330.1  uses LiqHDataAdaptor
  19.04.05 wl  gmAcetoneWash                TN2380.1  Anpassung an Änderung von TPipPumpDevice.Pick
  27.04.05 pk  gmGetVol                     TN2400    Receive ZTube as parameter instead of calculating it - for Slanted reacks
  27.04.05 pk  gmPickLiq                    TN2400    Receive ZMax, ZTube array.  No internal calculation of ZMax neede - caused incorrect ZMax vals for slanted racks
  27.04.05 pk  gmLiquidDetection            TN2398    use TPosinfoDataadaptor instead of DMRack.Posinfo
  28.04.05 wl  gmNoDispenseVolume           TN2383    Prüft, ob das zu dispensierende Volumen (minus Airgap) > 0 ist
  20.06.05 tbh gmCalcDilutorSteps           TN2385    neu: rechnet für Volumen die benötigten Dilutormotorschritte aus
  20.06.05 tbh gmPickAir                    TN2385    verwendet Volumenkorrektur
  20.06.05 tbh gmPickSys                    TN2385    verwendet Volumenkorrektur
  20.06.05 tbh gmPickLiq,gmPickLiq_from_Zmax_with_Retract  TN2385    verwendet Dilutormotor-Schritte bei Volumenkorrektur
  20.06.05 tbh gmDispLiq                    TN2385    verwendet Dilutormotor-Schritte bei Volumenkorrektur
  20.06.05 tbh gmAspirate                   TN2385    angepasst zur Verwendung Dilutormotor-Schritte bei Volumenkorrektur
  20.06.05 tbh gmDispense                   TN2385    angepasst zur Verwendung Dilutormotor-Schritte bei Volumenkorrektur
  22.06.05 tbh gmPickAir                    TN2385    Volumenkorrektur nur für benötigte Tips
  22.06.05 tbh gmAspirate                   TN2385    Volumenkorrektur nur für benötigte Tips
  22.06.05 tbh gmDispense                   TN2385    Volumenkorrektur nur für benötigte Tips
  22.06.05 tbh gmCalcDilutorSteps           TN2385    Berechnung nur wenn Pumpe existiert
  22.06.05 tbh gmCalcDilutorSteps           TN2464.2  TArm.Tips[x].St_LhPtr kein Pointer mehr
  23.06.05 tbh gmAspirate                   TN2385    Volumenkorrektur nur für existierende Pumpen
  06.07.05 pk  gmAspirate                   TN2492    multiple spit backs
  18.07.05 thr gmMoveAspPos2                TN2411    Bug-Korrektur x<... durch x > ... ersetzt
  18.07.05 thr gmGetZPosByVolCalc           TN2411    Parameter added
  08.08.05 pk  gmTipTouch                   TN2527    New : called in both gmAspirate and gmDispense
  08.08.05 pk  gmAspirate                   TN2534    Spitback now done at retract position instead of zscan
  18.08.05 pk  gmMoveRetractDistance        TN2567    Check if retract position < 0
  18.08.05 pk                               TN2550    uses InterfacePipPump
  22.09.05 pk  gmDoLiquidWash               TN2622    calls gmFindWasteRackForArm
  22.09.05 pk  gmInitSyringes               TN2622    calls gmFindWashRackForArm
  10.10.05 pk  gmDispense                   TN2661    Steps performing bug Vol[x]:=0 removed
  14.10.05 wl  gmDry                        TN2672    Dry-Rack wird mit WB.FindSpecialRack gesucht
  09.11.05 wl                               TN2728    Aufrufe von MoveXY mit geänderten Parametern
  09.11.05 wl  TipTouch,MoveRetractDistance TN2728    --> Operator
  15.11.05 pk  gmFindSpecialDryValve        TN2754    Find Dry valve switch device using rackname
  17.11.05 wl                               TN2771    TOperation statt TOperator
  18.11.05 wl                               TN2764    gmRackGetZPos hat aTip als Parameter
  18.11.05 wl  gmMoveDspPos2                TN2764    SetToMinPosIfIsBeyondMin ersetzt if zPos<0 then zPos:=0
  11.01.06 pk  gmAspirate                   TN2871.0  Now aspirates extra gap
  11.01.06 pk  gmPickExtraGap               TN2871.0  New : Picks Waste and Airgap several times
  11.01.06 pk                               TN2871.0  Various functions : var arrays changed to const arrays
  12.01.06 pk  gmPickExtraGap               TN2871.0  Do only if maxliquid > 0
  08.02.06 thr gmInitSyringes               TN2925    additional parameter InitID
  15.03.06 thr gmdispense                   TN2963    Vorbesetzung xdummyarray1 fehlte -> Fehler in GetMotorStepsForVolume
  25.03.06 pk  gmInitSyringes               TN2997    Pump init moved to DevicesPipette
  18.04.06 pk                               TN2958    Call StorePreviousXYMoveInfo
  19.04.06 wl                               TN3051    benutzt TipFloatArray und extended für alle Vol, Speed und VolSteps
  26.04.06 wl  gmCalcDilutorSteps           TN3051    Errechneter Step-Wert kann nicht größer als MaxMotorStep werden
  24.08.06 wl                               TN3271    bei den Aufrufen von gmCalculatePipSteps wird wieder PaintTubes übergeben
  05.09.06 pk  gmAspirate                   TN3193    new aAspType parameter: distinguish between asp diluent and asp sample
  12.09.06 thr                              TN3272    Anpassung wegen geändertem Pippump-Interface
  15.09.06 pk  MoveRetractDistance          TN3310    now executed just once instead of from 0 to tipcount-1
  19.09.06 pk  gmInitSyringes               TN3315    return waste rack instead of nil
  19.09.06 pk  gmFindDrySwitchDevice        TN3312    New: find dry-related switches depending on the rackname setting of the device
  19.10.06 wl  gmDispLiq                    TN3329    an gmStoreDsp wird aVol übergeben, das auch die abgegebenen Airgaps enthält
  25.10.06 pk                               TN3381    free operation and motion objects
  04.12.06 wl  gmResetLiqError              TN3445    --> SamHigh
  07.12.06 wl                               TN3243    uses SamCmd entfernt
  19.01.07 wl  gmAspirate,gmDispense        TN3473    an gmSwitchModulePort wird jetzt Swich-Name, PipDevice und Tipmap übergeben
  26.01.07 pk                               TN3525.2  Robot.WriteLog changed to gmLogTxt
  14.02.07 wl  gmWashAtWashRack             TN3147    geänderter Aufruf von gmCalculatePipStep, alles in try-finally-Blöcken
  14.02.07 wl  gmAcetoneWash                TN3147    geänderter Aufruf von gmCalculatePipStep, alles in try-finally-Blöcken
  14.02.07 wl  gmDry                        TN3147    geänderter Aufruf von gmCalculatePipStep, alles in try-finally-Blöcken
  21.02.07 wl  gmWashAtWasteRack            TN3590    aus gmDoLiquidWash extrahiert
  21.02.07 wl  gmDoLiquidWash               TN3590    Bei WashAtWaste = -1 wird Waste-Rack nicht angefahren und Spritzen in WASH-Rack entleert
  21.02.07 wl  gmDoLiquidWash               TN3590    WashAtWaste wird nur noch aus ROBOT gelesen (nicht aus ZP01CONFIG)
  21.02.07 wl  gmWashAtWashRack             TN3590    auch im WASH können die Dilutoren geleert werden
  08.03.07 wl  gmDispLiq                    TN3623    benutzt FindPeriPump für PERIVOL
  12.03.07 pk                               TN3628    create TZAxisTravelMotionSystem instead of MotorMotionSystem
  18.04.07 wl  TLiquidStepData              TN3658    LIQPAR_MIXREC heißt jetzt TLiqHandlingMixRec
  18.04.07 wl  gmDoMixing                   TN3658    neue Mixfunktion
  18.04.07 wl  gmAspirate                   TN3658    Mix before Aspirate --> gmDoMixing
  18.04.07 wl  gmDispense                   TN3658    Mix after dispense --> gmDoMixing
  18.04.07 wl  gmReduceMixVol               TN3658    wenn MinRestVol > 0 wird nur so viel Mixvol. benutzt, wie detektiert wurde (- MinRestVol)
  18.04.07 wl  gmDispense                   TN3658    LQMode für das Mixen wird durch aMix.Method geändert (Detection und Tracking)
  07.08.07 wl                               TN3811.3  TPosinfoAdapter.Create ohne Parameter
  30.08.07 pk                               TN3840.2  Uses LiqHTypes
  09.11.07 pk                               TN3924    Steps changed to mm
  27.11.07 wl  gmDoMixing                   TN3898    Hack: wenn 'Mixing', 'DispLessThanAsp' <> 0 wird entsprechend weniger bei Disp abgegeben
  27.11.07 wl  gmPickAir                    TN3901    Angabe von Speed im Log
  27.11.07 wl  gmRetractAndGetTransportAir  TN3901    Transport Air-Aufnahme erscheint im LogDisplay
  27.11.07 wl  gmPickExtraGap               TN3901    Log erweitert
  27.11.07 wl  gmAspirate                   TN3901    Aspitare Sample Vol erscheint erst hier im LogDisplay
  27.11.07 wl  gmAspirate                   TN3897    aExtraGapLastWasteVol entspricht Additional Waste vol., wird in Log extra aufgeführt
  28.11.07 wl  gmDispense                   TN3942    Step Performing: Berechnung überarbeitet
  28.11.07 wl  gmDispense                   TN3942    Kein Step Performing bei Volume Control
  09.01.08 wl  gmDispense                   TN3972    Perivol entfernt
  29.01.08 wl                               TN3980   uses geändert
  24.04.08 wl  gmPumpAspirateActionExecute  TN4050   neu für PUMPA Action
  24.04.08 wl  gmPumpDispenseActionExecute  TN4050   neu für PUMPD Action
  24.04.08 wl  gmGetAspirate/DispenseTrackingWay   TN4050   Berechnung des Tracking-Weges verallgemeinert
  24.04.08 wl  gmPickLiq,gmDispLiq          TN4050   benutzen die neuen Funktionen gmGetAspirate/DispenseTrackingWay
  24.04.08 wl  gmPumpDispenseActionExecute  TN4050   TWay wird mit 0 gefllt, sonst passiert Unsinn
  25.04.08 pk  gmGetTWayZPos                TN4086   calls CalcZPosDifference to do subtraction
  25.04.08 pk  gmGetAspirateTrackingWay     TN4086   calls CalcZPosDifference to do subtraction
  05.05.08 wl  gmDispense                   TN4064    Step Performing: "Event before/after disp liquid" is done at each dispense step
  15.05.08 wl                               TN4100    TXYStepList property changed
  05.06.08 pk  gmCalcDilutorSteps           TN4138    Create new instance instead of using global instance
  20.06.08 pk                               TN4139    WB global object replaced by LayoutManager
  01.07.08 wl  gmPumpAspirateActionExecute  TN4156    xTWay wird zunächst mit Nullen gefüllt
  03.07.08 wl                               TN4157
  16.07.08 pk  gmLiquidDetection            TN4157    ZMotion.SetTipLiqDetError instead of gmSetTipError
  02.09.08 pk                               TN4215    DllCallExt changed to DllCall
  25.09.08 wl                               TN4242    TRunstCall ersetzt TDllCall
  17.12.08 wl  gmDry                        TN4121    exit if tips = 0
  18.12.08 wl  gmPumpAspirate/DispenseActionExecute  TN4318    neuer Parameter: Execute
  18.12.08 wl  gmPick/DispLiqCalculateTrackingWay TN4063   = gmGetAspirate/DispenseTrackingWay
  18.12.08 wl  gmCompleteMixing             TN4063    = gmDoMixing
  18.12.08 wl  gmPickLiq                    TN4063    now it is a function with result = TrackingWay
  18.12.08 wl  gmDispLiqWithoutCalc         TN4063    does the same as gmDispLiq, but TrackingWay is a const parameter
  18.12.08 wl  gmCompleteMixing             TN4063    Change at mix method 0: The tracking way calculted by gmPickLiq is also used at dispense
  14.05.09 ts  gmPumpDispenseActionExecute  TN4556    new: xRackname ( = '' if xRack = nil ,e.g. PUMPD action don´t need rack)
  15.05.09 ts  gmPumpDispenseActionExecute  TN4556    xRackname entfernt, dafür Fehlermeldung anstatt Access violation
  31.07.09 wl  gmCompleteMixing             TN4049   SyrMap als neuer Parameter
  31.07.09 wl  gmGetNextAspMixSyrMap        TN4049   ermittelt TipMap aus vorgegebenen Tips (result anders wenn MixFistOnly aktiv)
  31.07.09 wl  gmAspirate                   TN4049   neu: xMixSyrMap wird mit gmGetNextAspMixSyrMap ermittelt
  31.07.09 wl  gmAspirate                   TN3950   übergibt aZInsertMoveType an MoveAspPos2
  31.07.09 wl  gmDispense                   TN3950   übergibt aZInsertMoveType an MoveDspPos2
  31.07.09 pk  gmAspirate                   TN3950   DisableZErrors now called just before move to aspiration position because for ZP02 disable only disables error for next z-move
  31.07.09 pk  gmDispense                   TN3950   Do same thing for dispense
  31.07.09 wl  gmLiquidDetection            TN4018    geänderter Aufruf von DetectLiquid
  10.08.09 wl                               TN4702   Strings werden jetzt direkt geladen
  12.09.09 wl  TLiquidHandling              TN4740    alle Funktionen werden zu Klassen-Methoden
  12.09.09 wl                               TN4740    TipFloat-/TPosMM-/TipInteger-/MPOSArray durch TDoubleArray/TIntArray ersetzt
  02.10.09 pk  DispLiqWithoutCalc           TN4802    check if channel2 vol > 0  to avoid unneeded logging
  02.10.09 pk  CompleteMixing               TN4802    speed and channel2 args for DispLiqWithoutCalc were in the wrong ordere
  02.10.09 pk  CompleteMixing               TN4802    call MoveZ_WithOffset with positive offset (also changed to positive in MoveZ_WithOffset)
  24.11.09 ts  Aspirate/Dispense            TN4771    xXYSteps created in gmCalculatePipSteps will be freed
  10.12.09 ts                               TN4921    new: GetValvePosFromDispenseType /important for turning valve to different positions while dispensing
  29.01.10 pk  Pick/DispenseActionExecute   TN4966    New aTurnValves boolean parameter, if false dont turn valves
  29.01.10 pk  TurnValveActionExecute       TN4966    New TurnValves
  31.05.10 ts  DispLiqWithoutCalc           TN5101    gmStoreDsp nutzt xSampleVol anstatt aVol, POSINFO-Eintrag ohne Transportluft
  29.06.10 pk                               TN5173    various changes to allow pipetting sample with second pump
  15.07.10 pk  PipDelay                     TN5196    execute removed
  15.07.10 pk  PickSys                      TN5196    WaitAfterPickSys removed
  19.07.10 wl  CompleteMixing               TN5184    MixWithAir ist nicht mehr fest verknüpft mit DispAtZMaxMinusZOffset.Dadurch kann Luft an detektierter position abgegeben werden
  27.07.10 wl  CompleteMixing               TN5184.1  Bugfix für TN5184 "Mix with air"
  27.07.10 ts  PumpDisp/AspActionExecute    TN5012    Änderungen, damit bei PUMPA/D-Actions PipDevice ohne Arm genutzt werden kann
  12.08.10 wl  TurnValve/PumpDisp/AspActionExecute  TN5227    Channel durch PumpIndex ersetzt - damit kann auch Pumpe 3, 4 oder höher benutzt werden
  12.08.10 wl  Aspirate                     TN5227    Aspirate kam mehrfach und verwirrt die Anwender --> LiquidHandlingHigh
  26.08.10 ts  PickSysAirAtSysAirPos        TN5248    new: aMoveToSysAirPosSpeed
  21.09.10 wl  PickSys                      TN5264    Geänderte Begrenzung lässt jetzt Channel2Vol ohne Channel1Vol zu
  22.09.10 pk  CompleteMixing               TN5259    Calculate Trackingway when mixing with air - otherwise AV occurs
  22.09.10 pk  PickSysAirAtSysAirPos        TN5260    Move to ZTravel with normal speed and then use special speed for move to ZScan or ZDisp
  22.09.10 pk  CompleteMixing               TN5263    use PickSysAirAtSysAirPos to pick system air instead of just moving to aZStep.Z.ZTravel all the time
  22.09.10 wl  Aspirate                     TN5274    Reihenfolge geändert: Erst mixen, dann PickExtraGap
  22.09.10 wl  Aspirate                     TN5275    ExtraGapAirPos wird übergeben
  29.11.10 wl                               TN5370    Bugfix (der D2010-Compiler hätte das nicht zulassen dürfen)
  17.02.11 wl  DispLiqWithoutCalc           TN5478    gmStoreDsp nutzt jetzt wieder aVol anstatt xSampleVol (TN5101 wieder rückgängig gemacht)
  23.03.11 wl                               TN5515   MoveToZTravel: Methodenname geändert
  29.03.11 wl                               TN5524   uses OperationPip entfernt
  05.04.11 wl                               TN5535   ExecutePipPumps jetzt überall mit TipMap
  02.09.11 ts  MoveAspPos2/MoveDspPos2      TN5680   wenn ZErrorDisabled, dann Single Execute - wie ZPOSM
  13.09.11 ts                               TN5680   wenn ZErrorDisabled, dann Funktionsweise auch in Retract-function
  28.09.11 wl                               TN5725   gmCalculatePipSteps jetzt ohne Parameter
  28.09.11 wl                               TN5725   verwendet SubstanceLoading fpr AspStore,DspStroe
  31.10.11 wl  Aspirate,Dispense            TN5731   UseVolControl für Aspirate und Dispense ist immer true
  31.10.11 wl  GetVolCorrCurves             TN5731   neu: liest VolCorrCurve aus Liquid Handling Parameter
  31.10.11 wl  PumpAspirate/DispenseActionExecute  TN5731   UseVolCurve als neuer Parameter
  01.11.11 wl  Dispense                     TN5731   Compiler-Warnung entfernt
  03.11.11 wl                               TN5725   verwendet TSubstanceLoading.StoreDisp/StoreAsp
  05.12.11 wl  CompleteMixing               TN5758   Parameter UseCalculatedVol wird an MoveAspPos2 übergeben
  02.02.12 wl  alle Funktionen              TN5791   verwendet TArray<TXRackPosition> statt TRack und Pos-Array
  23.02.12 wl                               TN5818   Neue Felder für TipTouch werden übergeben
  08.03.12 wl  DispLiq                      TN5829   verwendet jetzt immer Volumenkorrektur
  08.03.12 wl  DispLiqWithEvents            TN5829   Step Performing und Volumenkorrektur können jetzt kombiniert werden
  08.03.12 wl  PumpAspirate/DispenseActionExecute  TN5828   Volumen und Speed-Parameter jetzt als Array
  29.03.12 wl  alle Funktionen              TN5791   Restarbeiten Rck-übergreifendes Pipettieren
  10.05.12 wl  PumpDispenseActionExecute    TN5893   TPumpDispenseType wieder abgeschafft
  29.05.12 wl  GetLogDescriptionFromAspType TN5903   neu: Um ähnliche Log-Zeile wie Aspirate-Action zu produzieren
  29.05.12 wl  PumpAspirate/DispenseActionExecute  TN5903   mit Log-Zeilen
  10.08.12 wl  DoLiquidWash                        TN5947 aPumpIndices: Pumpennummer der ersten Pumpe kann bestimmt werden
  10.08.12 wl  PeriPumpDispenseActionExecute       TN5947 neu: für Action PeriDisp
  28.08.12 ts                                      TN5943   unterschiedliche Z-Geschwindigkeiten für Z-Bewegungen mit Tube
  04.09.12 wl  CompleteMixing               TN5971   Bei der Abgabe von Mix-Luft darf es kein Tracking geben
  04.09.12 wl  Aspirate                     TN5972   Bei SpitBackAtAspPos wird vor SpitBack kein Z-Move ausgeführt
  19.11.12 wl  PickLiqCalculateTrackingWay  TN6022   Multi-Tip-Tubes: Tracking wurde bisher immer für alle Tips berechnet
  19.11.12 wl  DispLiqCalculateTrackingWay  TN6022   Multi-Tip-Tubes: Tracking wurde bisher immer für alle Tips berechnet
  26.11.12 wl  PickSysAirAtSysAirPos        TN6027   Neu: Retract Speed, wird verwendet, wenn die Nadeln nur hochgefahren werden
  26.11.12 wl  CompleteMixing               TN6027   wenn TMixModes.cUseRetract gesetzt, wird mit Retract Speed hochgefahren
  02.01.13 wl  Aspirate                     TN6064   SampleAspMixFirstOnly ist jetzt TLiqHandlingRec zugeordnet
  02.01.13 wl  Dispense                     TN6064   Beim Mixen wird neues Feld DispMixAspSubmerge statt DispSubmerge verwendet
  15.02.13 wl                               TN5914   StoreAsp & StoreDsp wird überall für beide Kanäle durchgeführt
  15.02.13 wl  DoLiquidWash                 TN6089   gmResetWashFlags wird hier aufgerufen
  07.05.13 wl  FlushAfterInit               TN6145   Pick/Disp Air wird nur ausgeführt, wenn AirVolAfterInit > 0
  07.05.13 wl  FlushAfterInit               TN6145   Disp Air wird nur für die angegegbenen Tips ausgeführt
  07.05.13 wl  FlushAfterInit               TN6145   Disp Air wird mit Ramp=0 (Default ramping) ausgeführt
  07.05.13 ts  Dispense                     TN6118   DispTipTouchScanStoreVol, Detektiertes Volumen kann in Posinfo gespeichert werden
  23.05.13 wl  Aspirate,Dispense            TN6153   SetPreviosZStep und StorePreviousXYMoveInfo werden nach MoveXY nicht mehr benötigt
  23.05.13 wl                               TN6153   geänderte Parameter bei MoveXY
  23.05.13 wl  PumpAspirate/DispenseActionExecute  TN6153   verwenden TArmPositionInfo
  05.06.13 wl                               TN6164   TArmPositionInfo: Bezeichnung "Previous" entfernt
  -------------------------------------------------------------------------------------------------- }

unit LiquidHandling;


interface


uses
    AppTypes,
    CommonTypes,
    GeneralTypes,
    Rack,
    IntfArmDevice,
    IntfSwitchDevice,
    IntfPipPumpDriver,
    RackTypes,
    LiqHTypes,
    IntfPipDevice,
    MotionSystemPipMove,
    TipSystem,
    MotionSystemTravel,
    EventManager;

type
    TPickAirPosition = (papUndefined, papZTravel, papZScan, papZDisp);
    TDryAfterWashType = (dawUseTipParameter, dawDoDryAfterWash, dawDoNotDryAfterWash);

    TLiquidHandling = class
    private
        class function GetTWayZPos(aZMotion: TZAxisPipMoveMotorMotionSystem; aTipIndex: integer;
            ahRack: TRack; aVol1, aVol2: extended; aZMax, aZTube: TPosMM): TPosMM;
        class function PickLiqCalculateTrackingWay(aPipDevice: IPipDevice;
            aZMotion: TZAxisPipMoveMotorMotionSystem; aTipMap: TIPMAP; const aRacks: TArray<TRack>;
            const aVol: TDoubleArray; aTTips: TIPMAP; var aIsVol: TDoubleArray;
            const aZMax, aZTube: TDoubleArray): TDoubleArray;
        class function DispLiqCalculateTrackingWay(aPipDevice: IPipDevice;
            aZMotion: TZAxisPipMoveMotorMotionSystem; aTipMap: TIPMAP; const aRacks: TArray<TRack>;
            const aVol: TDoubleArray; aTTips: TIPMAP; const aIsVol: TDoubleArray;
            const aZMax, aZTube: TDoubleArray): TDoubleArray;
        class function PickLiq(aPipDevice: IPipDevice; aZMotion: TZAxisPipMoveMotorMotionSystem;
            aTipMap: TIPMAP; const aRP: TArray<TXRackPosition>; const aCh1PumpIndices: TIntArray;
            const aVol: TDoubleArray; const aSpeed, aCh2WashVol, aCh2Speed: TDoubleArray;
            const aVolStepsCh1, aVolStepsCh2: TDoubleArray; aDelay: integer; aTTips: TIPMAP;
            var vIsVol: TDoubleArray; const aZMax, aZTube: TDoubleArray; aUselVolCorrect: boolean;
            aAspType: TAspirateType): TDoubleArray;
        class procedure DispLiq(aPipDevice: IPipDevice; aZMotion: TZAxisPipMoveMotorMotionSystem;
            aTipMap: TIPMAP; const aRP: TArray<TXRackPosition>; const aCh1PumpIndices: TIntArray;
            const aVol, aAirVol, aSpeed, aVolCh2, aSpeedCh2: TDoubleArray;
            const aVolStepsCh1, aVolStepsCh2: TDoubleArray; aDelay: integer; aTTips: integer;
            var vIsVol: TDoubleArray; aRecord_in_Database, aDispCh1and2together: boolean;
            const aZMax, aZTube: TDoubleArray);
        class procedure DispLiqWithEvents(aPipDevice: IPipDevice; aZMotion: TZAxisPipMoveMotorMotionSystem;
            aTipMap: TIPMAP; const aRP: TArray<TXRackPosition>; const aCh1PumpIndices: TIntArray;
            const aVol, aAirVol, aSpeed, aVolCh2, aSpeedCh2: TDoubleArray;
            const aVolStepsCh1, aVolStepsCh2: TDoubleArray; aDelay: integer; aTTips: integer;
            var vIsVol: TDoubleArray; aRecord_in_Database, aDispCh1and2together: boolean;
            const aZMax, aZTube: TDoubleArray; aEvBeforeDispLq, aEvAfterDispLq: TRunstCall;
            aPerformingSteps: double; aPerformDelay: integer);
        class procedure PickLiq_from_Zmax_with_Retract(aPipDevice: IPipDevice;
            aZMotion: TZAxisPipMoveMotorMotionSystem; aTipMap: TIPMAP; const aRP: TArray<TXRackPosition>;
            const aCh1PumpIndices: TIntArray; const aVol, aSpeed, aCh2WashVol, aCh2Speed: TDoubleArray;
            const aVolStepsCh1, aVolStepsCh2: TDoubleArray; aDelay: integer; aTTips: TIPMAP;
            aRetractPos_mm: TPosMM; aUseVolCorrect: boolean; aAspType: TAspirateType);
        class procedure DispLiqWithoutCalc(aPipDevice: IPipDevice; aZMotion: TZAxisPipMoveMotorMotionSystem;
            aTipMap: TIPMAP; const aRP: TArray<TXRackPosition>; const aCh1PumpIndices: TIntArray;
            const aVol, aAirVol, aSpeed, aVolCh2, aSpeedCh2: TDoubleArray;
            const aVolStepsCh1, aVolStepsCh2: TDoubleArray; aDelay: integer; var vIsVol: TDoubleArray;
            aRecord_in_Database, aDispCh1and2together: boolean; aUseVolControl: boolean;
            const aTWay: TDoubleArray);
        class procedure RetractAndGetTransportAir(aPipDevice: IPipDevice;
            aZMotion: TZAxisPipMoveMotorMotionSystem; aZStep: TPipStepZPos; const aCh1PumpIndices: TIntArray;
            const aTransAirVol, aTransAirSpeed: TDoubleArray; aTransAirPos: TPickAirPosition;
            aTransAirDelay: integer; aZRetractSpeed: integer; aZRetractDistance_mm: TPosMM;
            aSingleRetract, aDispense, aUseVolCorrect, aRetractMoveDone, aDisableZErr: boolean);
        class procedure PickSysAirAtSysAirPos(aPipDevice: IPipDevice;
            aZMotion: TZAxisPipMoveMotorMotionSystem; aAirPos: TPickAirPosition; aSyrMap: TIPMAP;
            const aZTravel, aZScan, aZDisp: TDoubleArray; const aCh1PumpIndices: TIntArray;
            const aVol: TDoubleArray; const aSpeed: TDoubleArray; aDelay: integer; aUseVolControl: boolean;
            aZDownToSysAirPosSpeed, aZUpRetractSpeed: integer);
        class procedure PickExtraGap(aPipDevice: IPipDevice; aZMotion: TZAxisPipMoveMotorMotionSystem;
            aZTravelMotion: TZAxisTravelMotionSystem; const aPStep: TPipStepZPosArray; aStep: integer;
            const aRP: TArray<TXRackPosition>; aLiqDet: integer; const aCh1PumpIndices: TIntArray;
            const aVol: TDoubleArray; aSubMerge_mm: TPosMM; var vIsVol: TDoubleArray;
            aZScanMode, aZInsertSpeed, aZInsertMoveType: integer; aSysAirPos: TPickAirPosition;
            const aExtraGapCount: TIntArray; const aExtraGapAirVol: TDoubleArray;
            const aExtraGapAirSpeed: TDoubleArray; aExtraGapAirDelay: integer;
            const aExtraGapWasteVol: TDoubleArray; const aExtraGapWasteSpeed: TDoubleArray;
            aExtraGapWasteDelay: integer; aUseVolControl: boolean);
        class procedure ReduceMixVol(aPipDevice: IPipDevice; aTips: TIPMAP; var vMixVol: TDoubleArray;
            const aMinRestVol: double; const aIsVol: TDoubleArray);
        class procedure CompleteMixing(aPipDevice: IPipDevice; aZMotion: TZAxisPipMoveMotorMotionSystem;
            aZTravelMotion: TZAxisTravelMotionSystem; aMixSyrMap: TIPMAP; var vMix: TLiqHandlingMixRec;
            const aZStep: TPipStepZPos; const aRP: TArray<TXRackPosition>; const aCh1PumpIndices: TIntArray;
            const aVol: TDoubleArray; aAspLQModes: integer; aAspSubMerge_mm: TPosMM;
            aRecordDetectionVolume: boolean; aZScanMode, aZInsertSpeed, aZInsertMoveType: integer;
            aDelay: integer; const aSysAirPos: TPickAirPosition;
            const aZDownToSysAirPosSpeed, aZUpRetractSpeed: integer; var vTrackingTips: TIPMAP;
            var vIsVol: TDoubleArray; aDisableZErr: boolean);
        class function GetNextAspMixSyrMap(aPipDevice: IPipDevice; aSyrMap: TIPMAP;
            const aRP: TArray<TXRackPosition>; const aMix: TLiqHandlingMixRec;
            const aMixFirstOnly: boolean): TIPMAP;
        class function NoDispenseVolume(aPipDevice: IPipDevice;
            const aVolCh1, aAirVolCh1, aVolCh2: TDoubleArray): boolean;
        class procedure WashAtWashRack(aUsedArm: IArmDevice; aPipDevice: IPipDevice;
            aZMotion: TZAxisPipMoveMotorMotionSystem; aTips: TIPMAP; const aVolume, aVolume2: TDoubleArray;
            aUsePeriPump: boolean; aUseCorridor: boolean; aWashRetractSpeed: integer; aEmptyDilutors: boolean;
            const aPumpIndices: TIntArray);
        class procedure WashAtWasteRack(aUsedArm: IArmDevice; aPipDevice: IPipDevice;
            aZMotion: TZAxisPipMoveMotorMotionSystem; aTips: TIPMAP; const aVolume, aVolume2: TDoubleArray;
            aUsePeripump: boolean; aUseCorridor: boolean; aWashAtWaste: integer;
            const aPumpIndices: TIntArray);
        class function GetAspirateTypeFromPumpAspirateType(aPumpAspirateType: TPumpAspirateType)
            : TAspirateType;
        class function GetValvePosFromPumpAspirateType(aPumpAspirateType: TPumpAspirateType): TValvePos;
        class function GetLogDescriptionFromAspType(aAspType: TPumpAspirateType): string;
    public
        class procedure PickSys(aPipDevice: IPipDevice; aZMotion: TZAxisPipMoveMotorMotionSystem;
            aTips: TIPMAP; const aCh1PumpIndices: TIntArray; const aVol, aSpeed: TDoubleArray;
            const aVolCh2, aSpeedCh2: TDoubleArray; const aVolStepsCh1, aVolStepsCh2: TDoubleArray;
            aDelay: integer; aUseVolCorrect: boolean);

        class procedure Aspirate(aUsedArm: IArmDevice; aSyrMap: TIPMAP; const aRP: TArray<TXRackPosition>;
            // array of Rack Positions
            const aCh1PumpIndices: TIntArray; const aSysAir: TDoubleArray; // airgap volumes before aspiration
            const aSysAirSpeed: TDoubleArray; // system Airgap speed
            const aAspVol: TDoubleArray; // aspiration volumes
            const aVolStepsCh1: TDoubleArray; const aVolStepsCh2: TDoubleArray; const aTransAir: TDoubleArray;
            // transport airgap volumes after aspiration
            const aSpeed: TDoubleArray; // aspiration speeds
            const aTransAirSpeed: TDoubleArray; // transport asp speed
            aSysAirPos: TPickAirPosition; aTransAirPos: TPickAirPosition;
            // ZPosition, wo die Luftblase gezogen werden soll
            aDelay: integer; // aspiration delay
            const aSpitBackVol: TDoubleArray; // spit back volumes
            aSpitBackCount: integer; // number of spit backs
            const aSpitBackSpeed: TDoubleArray; // spit back dispense speeds
            aSpitBackAtAspPos: boolean; // spit back position (aspirate or dispense)
            const aCh2WashVol: TDoubleArray; // TipWashVol Channel2
            const aCh2WashSpeed: TDoubleArray; // TipWashSpeed Channel2
            aCh2WashDelay: integer; aSysAirDelay: integer; aTransAirDelay: integer; // dispense delay
            aLiqDet: integer; // How to use the Liquid detector
            aSubMerge_mm: TPosMM; // Subtract mm
            aMix: TLiqHandlingMixRec; const aMixFirstOnly: boolean; // Mixing Parameter
            aSwitchDeviceName: string; // Port to Switch
            aSwitchPlace: TSwitchPlace; // switch above or in Tube
            aSingleRetract: boolean; aRecordDetectionVolume: boolean; // write Detection Volume in Database !
            aAspirationMethodRetractZ: integer; // Aspiration Method for small Volumes
            aSampleAspRetractPos_mm: TPosMM; // Position from Zmax to travel retract
            aDispCh1and2together: boolean; aEvBeforeAsp, aEvBeforePickLq, aEvAfterPickLq,
            aEvAfterAsp: TRunstCall; aDisableZErr: boolean; aZScanMode, aZInsertSpeed, aZInsertMoveType,
            aZRetractSpeed: integer; aZRetractDistance_mm: TPosMM; aTipTouch, aTipTouchScan: boolean;
            aTipTouchDelay: integer; aTipTouchScanMode: integer; aTipTouchSingle: boolean;
            const aTipTouchSubmerge: TDoubleArray; const aExtraGapCount: TIntArray;
            const aExtraGapAirVol: TDoubleArray; const aExtraGapAirSpeed: TDoubleArray;
            aExtraGapAirDelay: integer; aExtraGapAirPos: TPickAirPosition;
            const aExtraGapWasteVol: TDoubleArray; const aExtraGapWasteSpeed: TDoubleArray;
            aExtraGapWasteDelay: integer; const aExtraGapLastWasteVol: TDoubleArray; aAspType: TAspirateType;
            aMoveToSysAirPosSpeed: integer);

        class procedure Dispense(aUsedArm: IArmDevice; aSyrMap: TIPMAP; const aRP: TArray<TXRackPosition>;
            // array of Rack Positions
            const aCh1PumpIndices: TIntArray; const aVol: TDoubleArray; // array of volumes
            const aVolCh2: TDoubleArray; // array of volumes Channel 2
            const aTotalVol: TDoubleArray;
            // array of total volume ( dispense is performed in n steps of Vol to reach TtotalVol )
            const aAirVol: TDoubleArray; // array of volumes of AirGapVolumes (Trans + Sys)
            const aSpeed: TDoubleArray; // array of dispense speeds
            aDelay: integer; // dispense delay
            const aTransAir: TDoubleArray; const aTransAirSpeed: TDoubleArray; aTransAirPos: TPickAirPosition;
            // ZPosition, wo die Luftblase gezogen werden soll
            aMix: TLiqHandlingMixRec; aMixAspSubmerge: double; // Mixing Parameter
            aLiqDet: integer; // LiquidDetector Mode
            aSubMerge_mm: TPosMM; // Subtract steps
            aPerformingSteps: double; aPerformDelay: integer; aSwitchDeviceName: string; // Port to Switch
            aSwitchPlace: TSwitchPlace; // switch position
            aSingleRetract: boolean; aDispCh1and2together: boolean; aEvBeforeDisp: TRunstCall;
            aEvBeforeDispLq: TRunstCall; aEvAfterDispLq: TRunstCall; aEvAfterDisp: TRunstCall;
            aDisableZErr: boolean; aZScanMode: integer; aZInsertSpeed: integer; aZInsertMoveType: integer;
            aZRetractSpeed: integer; aZRetractDistance_mm: TPosMM; aTipTouch, aTipTouchScan: boolean;
            aTipTouchDelay: integer; aTipTouchScanMode: integer; aTipTouchSingle: boolean;
            const aTipTouchSubmerge: TDoubleArray; aTipTouchScanStoreVol: boolean);
        // Default-Werte für Dispense-Aufruf in PeriCal.pas

        class procedure DoLiquidWash(aUsedArm: IArmDevice; const aVolume, aVolume2: TDoubleArray;
            aUsePeripump: boolean; aUseCorridor: boolean; aWashRetractSpeed: integer;
            aDryAfterWash: TDryAfterWashType; const aPumpIndices: TIntArray);

        class procedure AcetoneWash(aUsedArm: IArmDevice; iTips: TIPMAP;
            aZMotion: TZAxisPipMoveMotorMotionSystem);
        class function CalcDilutorSteps(const aPipDevice: IPipDevice; const aUsedTips: integer;
            const aVolArray: TDoubleArray; const aPumpIndices: TIntArray; const aVolCorrCurve: TArray<string>)
            : TDoubleArray;

        class procedure PumpAspirateActionExecute(aPipDevice: IPipDevice; aUsedTips: TIPMAP;
            aPumpIndex: integer; aVolumes, aSpeeds: TArray<double>; const aVolCorrCurve: string;
            aAspType: TPumpAspirateType; aZTracking: boolean; aZSubmerge_mm: extended;
            aTurnValves, aExecute: boolean);
        class procedure PumpDispenseActionExecute(aPipDevice: IPipDevice; aUsedTips: TIPMAP;
            aPumpIndex: integer; aVolumes, aSpeeds: TArray<double>; const aVolCorrCurve: string;
            aZTracking: boolean; aZSubmerge_mm: extended; aTurnValves, aExecute: boolean);
        class procedure PeriPumpDispenseActionExecute(aPipDevice: IPipDevice; aUsedTips: TIPMAP;
            aVolumes: TArray<double>);

        class procedure TurnValveActionExecute(aPipDevice: IPipDevice; aUsedTips: TIPMAP; aVInx: TValvePos;
            aPumpIndex: integer; aExecute: boolean);
        class function GetVolCorrCurves(aPipDevice: IPipDevice): TArray<string>;
    end;


implementation


uses
    Forms,
    Classes,
    SysUtils,
    Math,
    Windows,
    ErrorManager,
    LogManager,
    RunFlow,
    TipMapUtils,
    Layout,
    IntfPipPumpDevice,
    IntfPeriPumpDevice,
    IntfMotorDriver,
    IntfMotorDevice,
    ObjModul,
    SamGlobe,
    LiquidHandlingDiti,
    IntfZTravelManager,
    SamHigh,
    PipDeviceManager,
    LayoutManager,
    Device,
    LiquidHandlingLow,
    AppSettings,
    LiquidManager,
    ArrayUtils,
    SubstanceLoading,
    VolcorrDataAdaptor,
    OperationAxisMove,
    ArmPositionInfo,
    OperationFactory;

{ TLiquidHandling }

class function TLiquidHandling.GetTWayZPos(aZMotion: TZAxisPipMoveMotorMotionSystem; aTipIndex: integer;
    ahRack: TRack; aVol1, aVol2: extended; aZMax, aZTube: TPosMM): TPosMM;
var
    xZPos1, xZPos2: TPosMM;
begin
    xZPos1 := aZMotion.GetRackZPosByVolume(aTipIndex, ahRack, aVol1, aZMax, aZTube);
    xZPos2 := aZMotion.GetRackZPosByVolume(aTipIndex, ahRack, aVol2, aZMax, aZTube);
    result := aZMotion.CalcZPosDifference(aTipIndex, xZPos1, xZPos2);
end;

class function TLiquidHandling.PickLiqCalculateTrackingWay(aPipDevice: IPipDevice;
    aZMotion: TZAxisPipMoveMotorMotionSystem; aTipMap: TIPMAP; const aRacks: TArray<TRack>;
    const aVol: TDoubleArray; aTTips: TIPMAP; var aIsVol: TDoubleArray; const aZMax, aZTube: TDoubleArray)
    : TDoubleArray;
var
    x: integer;
    xv, xRest: TPosMM;
    xSingleRack: TRack;
    xTrackingTips: TIPMAP;
begin
    result := aPipDevice.GetNullDoubleArray;
    xTrackingTips := aTipMap and aTTips;

    // MultiTipRack: all tips in same liquid (Funktioniert nur für ein Rack)
    xSingleRack := TXRackPositionUtils.ExtractSingleRackFromArray(aRacks);
    if Assigned(xSingleRack) and ((xSingleRack.Structure.TubeTyp and RTF_SINGLE_TUBE) = 0) then
    begin
        xv := 0;
        // all in same tube ==> calculate sum of all liquids to take
        for x := 0 to aPipDevice.TipCount - 1 do
        begin
            if (((xTrackingTips shr x) and 1) <> 0) then
                xv := xv + aVol[x];
        end;
        for x := 0 to aPipDevice.TipCount - 1 do
        begin
            if (((xTrackingTips shr x) and 1) <> 0) then
            begin
                xRest := aZMotion.CalcZPosDifference(x, aZMax[x], aZMotion.ReadCurrentPos(x));
                result[x] := GetTWayZPos(aZMotion, x, aRacks[x], aIsVol[x] - xv, aIsVol[x], aZMax[x],
                    aZTube[x]);

                if (result[x] > xRest) then
                    result[x] := xRest;
            end
            else
                result[x] := 0;
        end;
    end;

    // single tip tubes
    for x := 0 to aPipDevice.TipCount - 1 do
    begin
        if (((xTrackingTips shr x) and 1) <> 0) then
        begin
            xRest := aZMotion.CalcZPosDifference(x, aZMax[x], aZMotion.ReadCurrentPos(x));
            result[x] := GetTWayZPos(aZMotion, x, aRacks[x], aIsVol[x] - aVol[x], aIsVol[x], aZMax[x],
                aZTube[x]);

            if (result[x] > xRest) then
                result[x] := xRest;
        end
        else
            result[x] := 0; // no tracking
    end;
end;

class function TLiquidHandling.DispLiqCalculateTrackingWay(aPipDevice: IPipDevice;
    aZMotion: TZAxisPipMoveMotorMotionSystem; aTipMap: TIPMAP; const aRacks: TArray<TRack>;
    const aVol: TDoubleArray; aTTips: TIPMAP; const aIsVol: TDoubleArray; const aZMax, aZTube: TDoubleArray)
    : TDoubleArray;
var
    x: integer;
    xv: TPosMM;
    xSingleRack: TRack;
    xTrackingTips: TIPMAP;
begin
    result := aPipDevice.GetNullDoubleArray;
    xTrackingTips := aTipMap and aTTips;

    // MultiTipRack: all tips in same liquid (Funktioniert nur für ein Rack)
    xSingleRack := TXRackPositionUtils.ExtractSingleRackFromArray(aRacks);
    if Assigned(xSingleRack) and ((xSingleRack.Structure.TubeTyp and RTF_SINGLE_TUBE) = 0) then
    begin
        xv := 0;
        // all in same tube ==> calculate sum of all liquids to take
        for x := 0 to aPipDevice.TipCount - 1 do
        begin
            if (((xTrackingTips shr x) and 1) <> 0) then
                xv := xv + aVol[x];
        end;
        for x := 0 to aPipDevice.TipCount - 1 do
        begin
            if (((xTrackingTips shr x) and 1) <> 0) then
            begin
                result[x] := GetTWayZPos(aZMotion, x, aRacks[x], aIsVol[x], aIsVol[x] + xv, aZMax[x],
                    aZTube[x]);
            end
            else
                result[x] := 0;
        end;
        EXIT;
    end;

    // single tip tubes
    for x := 0 to aPipDevice.TipCount - 1 do
    begin
        if (((xTrackingTips shr x) and 1) <> 0) then
        begin
            result[x] := GetTWayZPos(aZMotion, x, aRacks[x], aIsVol[x], aIsVol[x] + aVol[x], aZMax[x],
                aZTube[x]);
        end
        else
            result[x] := 0; // no tracking
    end;
end;

class function TLiquidHandling.PickLiq(aPipDevice: IPipDevice; aZMotion: TZAxisPipMoveMotorMotionSystem;
    aTipMap: TIPMAP; const aRP: TArray<TXRackPosition>; const aCh1PumpIndices: TIntArray;
    const aVol: TDoubleArray; const aSpeed, aCh2WashVol, aCh2Speed: TDoubleArray;
    const aVolStepsCh1, aVolStepsCh2: TDoubleArray; aDelay: integer; aTTips: TIPMAP; var vIsVol: TDoubleArray;
    const aZMax, aZTube: TDoubleArray; aUselVolCorrect: boolean; aAspType: TAspirateType): TDoubleArray;
var
    x: integer;
begin
    result := aPipDevice.GetNullDoubleArray;

    if (gErrorManager.IsGlobalErr()) then
        EXIT;

    aTipMap := aTipMap and aPipDevice.OKTips;
    aTTips := aTTips and aPipDevice.OKTips;

    result := PickLiqCalculateTrackingWay(aPipDevice, aZMotion, aTipMap,
        TXRackPositionUtils.ExtractRacksFromArray(aRP), aVol, aTTips, vIsVol, aZMax, aZTube);

    TLiquidHandlingLow.PickLiquid(aPipDevice, aZMotion, aTipMap, aCh1PumpIndices, aVol, aSpeed, V_SYR_TIP,
        aCh2WashVol, aCh2Speed, V_SYR_SYS, aVolStepsCh1, aVolStepsCh2, result, true, true, aUselVolCorrect);
    gRunFlow.AppSleep( aDelay);

    for x := 0 to aPipDevice.TipCount - 1 do
    begin
        if TTipMapUtils.TipSelected(aTipMap, x) then
        begin
            TSubstanceLoading.Instance.StoreLiquidAsp(aPipDevice, aRP[x].Rack, aRP[x].Pos, x, aVol[x],
                aAspType, aCh1PumpIndices[x]);
            TSubstanceLoading.Instance.StoreLiquidAsp(aPipDevice, aRP[x].Rack, aRP[x].Pos, x, aCh2WashVol[x],
                aAspType, 1);

            vIsVol[x] := vIsVol[x] - aVol[x]; // update volume for tracking and retract
        end;
    end;
end;

class procedure TLiquidHandling.PickLiq_from_Zmax_with_Retract(aPipDevice: IPipDevice;
    aZMotion: TZAxisPipMoveMotorMotionSystem; aTipMap: TIPMAP; const aRP: TArray<TXRackPosition>;
    const aCh1PumpIndices: TIntArray; const aVol, aSpeed, aCh2WashVol, aCh2Speed: TDoubleArray;
    const aVolStepsCh1, aVolStepsCh2: TDoubleArray; aDelay: integer; aTTips: TIPMAP; aRetractPos_mm: TPosMM;
    aUseVolCorrect: boolean; aAspType: TAspirateType);
var
    xTWay: TDoubleArray;
    x: integer;
begin
    if (gErrorManager.IsGlobalErr()) then
        EXIT;

    aTipMap := aTipMap and aPipDevice.OKTips;

    xTWay := TArrayUtils.GetDefinedDoubleArray(-Abs(aRetractPos_mm), aPipDevice.TipCount);

    gLogManager.LogF('Aspiration from Z-Max with tracking of %f mm', [aRetractPos_mm], false);
    TLiquidHandlingLow.PickLiquid(aPipDevice, aZMotion, aTipMap, aCh1PumpIndices, aVol, aSpeed, V_SYR_TIP,
        aCh2WashVol, aCh2Speed, V_SYR_SYS, aVolStepsCh1, aVolStepsCh2, xTWay, true, true, aUseVolCorrect);
    gRunFlow.AppSleep( aDelay);

    for x := 0 to aPipDevice.TipCount - 1 do
    begin
        if TTipMapUtils.TipSelected(aTipMap, x) then
        begin
            TSubstanceLoading.Instance.StoreLiquidAsp(aPipDevice, aRP[x].Rack, aRP[x].Pos, x, aVol[x],
                aAspType, aCh1PumpIndices[x]);
            TSubstanceLoading.Instance.StoreLiquidAsp(aPipDevice, aRP[x].Rack, aRP[x].Pos, x, aCh2WashVol[x],
                aAspType, 1);
        end;
    end;
end;

class procedure TLiquidHandling.PickSys(aPipDevice: IPipDevice; aZMotion: TZAxisPipMoveMotorMotionSystem;
    aTips: TIPMAP; const aCh1PumpIndices: TIntArray; const aVol, aSpeed: TDoubleArray;
    const aVolCh2, aSpeedCh2: TDoubleArray; const aVolStepsCh1, aVolStepsCh2: TDoubleArray; aDelay: integer;
    aUseVolCorrect: boolean);
const
    cWaitAfterPickSys = true;
    // this used to be: cWaitAfterPickSys := (gWaitAfterPickSys <> 0) when aspirating diluent

var
    xTWay: TDoubleArray;
    x: integer;
    xVol1, xSpeed1, xVol2, xSpeed2: TDoubleArray;
    xCh2LogText: string;
    xCh2HasVolume: boolean;
begin
    if (gErrorManager.IsGlobalErr()) then
        EXIT;

    aTips := aTips and aPipDevice.OKTips;
    if (TTipMapUtils.MaxLiquid(aTips, aVol) <= 0) and (TTipMapUtils.MaxLiquid(aTips, aVolCh2) <= 0) then
        EXIT;

    xCh2LogText := '';
    xCh2HasVolume := false;
    for x := 0 to aPipDevice.TipCount - 1 do
    begin
        if not TTipMapUtils.TipSelected(aTips, x) then
            CONTINUE;
        if aVolCh2[x] > 0 then
        begin
            xCh2HasVolume := true;
            BREAK;
        end;
    end;

    if xCh2HasVolume then
        xCh2LogText := ' Vol(Ch2) - ' + TArrayUtils.ArrayToBracketText(aVolCh2);

    gLogManager.Log('Pick SystemLiq - Vol' + TArrayUtils.ArrayToBracketText(aVol) + xCh2LogText + ' Speed' +
        TArrayUtils.ArrayToBracketText(aSpeed) + ' Delay[' + IntToStr(aDelay) + ']', true);

    xTWay := aPipDevice.GetNullDoubleArray;

    xVol1 := Copy(aVol);
    xSpeed1 := Copy(aSpeed);
    xVol2 := Copy(aVolCh2);
    xSpeed2 := Copy(aSpeedCh2);

    // pick liquid from system
    TLiquidHandlingLow.PickLiquid(aPipDevice, aZMotion, aTips, aCh1PumpIndices, xVol1, xSpeed1, V_SYR_SYS,
        xVol2, xSpeed2, V_SYR_SYS, aVolStepsCh1, aVolStepsCh2, xTWay, true, cWaitAfterPickSys,
        aUseVolCorrect);

    // delay
    gRunFlow.AppSleep( aDelay);

    // update Posinfo
    for x := 0 to aPipDevice.TipCount - 1 do
    begin
        if (((aTips shr x) and 1) <> 0) then
        begin
            TSubstanceLoading.Instance.StoreLiquidAsp(aPipDevice, nil, 0, x, aVol[x], asptSystemLiquid,
                aCh1PumpIndices[x]);
            TSubstanceLoading.Instance.StoreLiquidAsp(aPipDevice, nil, 0, x, aVolCh2[x], asptSystemLiquid, 1);
            // update tipinfo
        end;
    end;
end;

class procedure TLiquidHandling.DispLiqWithoutCalc(aPipDevice: IPipDevice;
    aZMotion: TZAxisPipMoveMotorMotionSystem; aTipMap: TIPMAP; const aRP: TArray<TXRackPosition>;
    const aCh1PumpIndices: TIntArray; const aVol, aAirVol, aSpeed, aVolCh2, aSpeedCh2: TDoubleArray;
    const aVolStepsCh1, aVolStepsCh2: TDoubleArray; aDelay: integer; var vIsVol: TDoubleArray;
    aRecord_in_Database, aDispCh1and2together: boolean; aUseVolControl: boolean; const aTWay: TDoubleArray);
var
    x: integer;
    xSampleVol: TDoubleArray; // ProbenVolumen ohne Airgaps
    xIsChannel2Used: boolean;
begin
    if (gErrorManager.IsGlobalErr()) then
        EXIT;

    xSampleVol := aPipDevice.GetNullDoubleArray;
    for x := 0 to aPipDevice.TipCount - 1 do
    begin
        xSampleVol[x] := aVol[x] + aVolCh2[x] - aAirVol[x];
    end;

    aTipMap := aTipMap and aPipDevice.OKTips;

    xIsChannel2Used := false;
    for x := 0 to aPipDevice.TipCount - 1 do
    begin
        if TTipMapUtils.TipSelected(aTipMap, x) and (aVolCh2[x] > 0) then
        begin
            xIsChannel2Used := true;
            BREAK;
        end;
    end;

    if (not xIsChannel2Used) or (aDispCh1and2together) then
    begin
        TLiquidHandlingLow.DispLiquid(aPipDevice, aZMotion, aTipMap, aCh1PumpIndices, aVol, aSpeed, V_SYR_TIP,
            aVolCh2, aSpeedCh2, V_SYR_TIP, aVolStepsCh1, aVolStepsCh2, aTWay,
            TXRackPositionUtils.ExtractRacksFromArray(aRP), true, true, aUseVolControl);
    end
    else
    begin
        TLiquidHandlingLow.DispLiquid(aPipDevice, aZMotion, aTipMap, aCh1PumpIndices,
            aPipDevice.GetNullDoubleArray, aSpeed, V_SYR_TIP, aVolCh2, aSpeedCh2, V_SYR_TIP,
            aPipDevice.GetNullDoubleArray, aVolStepsCh2, aTWay,
            TXRackPositionUtils.ExtractRacksFromArray(aRP), true, true, aUseVolControl);
        TLiquidHandlingLow.DispLiquid(aPipDevice, aZMotion, aTipMap, aCh1PumpIndices, aVol, aSpeed, V_SYR_TIP,
            aPipDevice.GetNullDoubleArray, aSpeedCh2, V_SYR_TIP, aVolStepsCh1, aPipDevice.GetNullDoubleArray,
            aTWay, TXRackPositionUtils.ExtractRacksFromArray(aRP), true, true, aUseVolControl);
    end;

    gRunFlow.AppSleep(aDelay);

    for x := 0 to aPipDevice.TipCount - 1 do
        if (((aTipMap shr x) and 1) <> 0) and (aRecord_in_Database) then
        begin
            TSubstanceLoading.Instance.StoreLiquidDsp(aPipDevice, aRP[x].Rack, aRP[x].Pos, x, aVol[x],
                dsptLiquid, aCh1PumpIndices[x]);
            if (xIsChannel2Used) then
                TSubstanceLoading.Instance.StoreLiquidDsp(aPipDevice, aRP[x].Rack, aRP[x].Pos, x, aVolCh2[x],
                    dsptLiquid, 1);
            vIsVol[x] := vIsVol[x] + xSampleVol[x]; // update volume for tracking and retract
        end;
end;

class procedure TLiquidHandling.DispLiq(aPipDevice: IPipDevice; aZMotion: TZAxisPipMoveMotorMotionSystem;
    aTipMap: TIPMAP; const aRP: TArray<TXRackPosition>; const aCh1PumpIndices: TIntArray;
    const aVol, aAirVol, aSpeed, aVolCh2, aSpeedCh2: TDoubleArray;
    const aVolStepsCh1, aVolStepsCh2: TDoubleArray; aDelay: integer; aTTips: integer;
    var vIsVol: TDoubleArray; aRecord_in_Database, aDispCh1and2together: boolean;
    const aZMax, aZTube: TDoubleArray);
var
    xTrackingWay: TDoubleArray;
begin
    xTrackingWay := DispLiqCalculateTrackingWay(aPipDevice, aZMotion, aTipMap,
        TXRackPositionUtils.ExtractRacksFromArray(aRP), aVol, aTTips, vIsVol, aZMax, aZTube);

    DispLiqWithoutCalc(aPipDevice, aZMotion, aTipMap, aRP, aCh1PumpIndices, aVol, aAirVol, aSpeed, aVolCh2,
        aSpeedCh2, aVolStepsCh1, aVolStepsCh2, aDelay, vIsVol, aRecord_in_Database, aDispCh1and2together,
        true, xTrackingWay)
end;

class procedure TLiquidHandling.DispLiqWithEvents(aPipDevice: IPipDevice;
    aZMotion: TZAxisPipMoveMotorMotionSystem; aTipMap: TIPMAP; const aRP: TArray<TXRackPosition>;
    const aCh1PumpIndices: TIntArray; const aVol, aAirVol, aSpeed, aVolCh2, aSpeedCh2, aVolStepsCh1,
    aVolStepsCh2: TDoubleArray; aDelay, aTTips: integer; var vIsVol: TDoubleArray;
    aRecord_in_Database, aDispCh1and2together: boolean; const aZMax, aZTube: TDoubleArray;
    aEvBeforeDispLq, aEvAfterDispLq: TRunstCall; aPerformingSteps: double; aPerformDelay: integer);
var
    xSumVol: extended;
    xSingleVol, xSingleVolSteps, xRestVol: TDoubleArray;
    x: integer;
begin
    if (aPerformingSteps <= 1) then
    begin
        if Assigned(aEvBeforeDispLq) then
            aEvBeforeDispLq.Execute('before disp. liquid');

        DispLiq(aPipDevice, aZMotion, aTipMap, aRP, aCh1PumpIndices, aVol, aAirVol, aSpeed, aVolCh2,
            aSpeedCh2, aVolStepsCh1, aVolStepsCh2, aDelay, aTTips, vIsVol, aRecord_in_Database,
            aDispCh1and2together, aZMax, aZTube);

        if Assigned(aEvAfterDispLq) then
            aEvAfterDispLq.Execute('after disp. liquid');
    end
    else
    begin
        xSingleVol := aPipDevice.GetNullDoubleArray;
        xSingleVolSteps := aPipDevice.GetNullDoubleArray;
        xRestVol := aPipDevice.GetNullDoubleArray;
        for x := 0 to aPipDevice.TipCount - 1 do
            xRestVol[x] := aVol[x];

        // In Schritten Dispensieren
        while (not gErrorManager.IsGlobalErr()) do
        begin
            { TODO : AirVol und Ch2Vol werden noch nicht berücksichtigt }
            xSumVol := 0;
            for x := 0 to aPipDevice.TipCount - 1 do
            begin
                xSingleVol[x] := 0;
                xSingleVolSteps[x] := 0;

                if (((aTipMap shr x) and 1) = 0) or (xRestVol[x] <= 0) then
                    CONTINUE;

                xSingleVol[x] := MinValue([aPerformingSteps, xRestVol[x]]);
                xSingleVolSteps[x] := xSingleVol[x] * aVolStepsCh1[x] / aVol[x];

                xRestVol[x] := xRestVol[x] - xSingleVol[x];
                xSumVol := xSumVol + xSingleVol[x];
            end;
            if (xSumVol <= 0) then
                BREAK;

            if Assigned(aEvBeforeDispLq) then
                aEvBeforeDispLq.Execute('before disp. liquid');

            DispLiq(aPipDevice, aZMotion, aTipMap, aRP, aCh1PumpIndices, xSingleVol,
                aPipDevice.GetNullDoubleArray, aSpeed, aVolCh2, aSpeedCh2, xSingleVolSteps, aVolStepsCh2,
                aDelay, aTTips, vIsVol, aRecord_in_Database, aDispCh1and2together, aZMax, aZTube);

            gRunFlow.AppSleep(aPerformDelay);

            if Assigned(aEvAfterDispLq) then
                aEvAfterDispLq.Execute('after disp. liquid');
        end;
    end;
end;

class procedure TLiquidHandling.RetractAndGetTransportAir(aPipDevice: IPipDevice;
    aZMotion: TZAxisPipMoveMotorMotionSystem; aZStep: TPipStepZPos; const aCh1PumpIndices: TIntArray;
    const aTransAirVol, aTransAirSpeed: TDoubleArray; aTransAirPos: TPickAirPosition; aTransAirDelay: integer;
    aZRetractSpeed: integer; aZRetractDistance_mm: TPosMM; aSingleRetract, aDispense, aUseVolCorrect,
    aRetractMoveDone, aDisableZErr: boolean);
var
    x: integer;
    xTransAirZPos: TDoubleArray;
    xUseTransportAir: boolean;
    xSyrMap: TIPMAP;
    xZRetractTipMap: TIPMAP;
begin
    if (gErrorManager.IsGlobalErr()) then
        EXIT;

    xSyrMap := aZStep.SyrMap;
    xUseTransportAir := (TTipMapUtils.MaxLiquid(xSyrMap, aTransAirVol) > 0);
    // Kein Retract wenn bei Systemverteilung weitere Hübe notwendig sind!!!
    xZRetractTipMap := aPipDevice.GetZRetractTips(xSyrMap, aDispense);

    // 1. if RetractDistance_mm is defined: Relative movement for that distance
    if not aRetractMoveDone then
        aRetractMoveDone := aZMotion.MoveRetractDistance(xZRetractTipMap, aZRetractSpeed,
            aZRetractDistance_mm, aSingleRetract, aDispense, aDisableZErr);

    // Dispense: if no transport air is defined - do not retract!
    if (aDispense) and (not xUseTransportAir) then
    begin
        if (aSingleRetract) then // single retract: move up without retract speed
            aZMotion.MoveZ(xSyrMap, aZStep.Z.ZTravel, m_EXECUTE, AT_MOVE_ABS, 0, 0, true);

        EXIT;
    end;

    xTransAirZPos := Copy(aZStep.Z.ZTravel);
    if (xUseTransportAir) then
    begin
        case (aTransAirPos) of
            papZScan:
                xTransAirZPos := Copy(aZStep.Z.ZScan);
            papZDisp:
                xTransAirZPos := Copy(aZStep.Z.ZDisp);
        end;
    end;

    // 2. Move to transport air position

    aZMotion.MoveToTransportAirPos(xZRetractTipMap, aTransAirVol, aZRetractSpeed, aSingleRetract,
        xTransAirZPos, aRetractMoveDone);
    if (gErrorManager.IsGlobalErr()) then
        EXIT;

    // 3. Aspirate second (transport) air gap
    if (not xUseTransportAir) then
        EXIT;
    gLogManager.Log('Aspirate Transport Air - Vol-' + TArrayUtils.ArrayToBracketText(aTransAirVol) + ' Speed-'
        + TArrayUtils.ArrayToBracketText(aTransAirSpeed) + ' Delay-[' + IntToStr(aTransAirDelay) + ']', true);
    TLiquidHandlingLow.PickAir(aPipDevice, aZMotion, xSyrMap, aCh1PumpIndices, aTransAirVol, aTransAirSpeed, aTransAirDelay,
        aUseVolCorrect);
    if (gErrorManager.IsGlobalErr()) then
        EXIT;

    // 4. If Single retract and TransAirPos <> ZTravel - move to ZTravel with retract speed
    if (not aSingleRetract) then
        EXIT;

    for x := 0 to aPipDevice.TipCount - 1 do
    begin
        if (((xZRetractTipMap shr x) and 1) = 0) then
            CONTINUE;

        if (xTransAirZPos[x] = aZStep.Z.ZTravel[x]) then
            CONTINUE;

        if (aRetractMoveDone) then // Retract-Movement wurde bereits ausgeführt: fahre normalen Speed
            aZMotion.MoveSingleZ(x, aZStep.Z.ZTravel[x], m_EXECUTE)
        else
            aZMotion.MoveSingleZ_Retract(x, aZStep.Z.ZTravel[x], m_EXECUTE, aZRetractSpeed, false);
    end;
end;

class procedure TLiquidHandling.PickSysAirAtSysAirPos(aPipDevice: IPipDevice;
    aZMotion: TZAxisPipMoveMotorMotionSystem; aAirPos: TPickAirPosition; aSyrMap: TIPMAP;
    const aZTravel, aZScan, aZDisp: TDoubleArray; const aCh1PumpIndices: TIntArray; const aVol: TDoubleArray;
    const aSpeed: TDoubleArray; aDelay: integer; aUseVolControl: boolean;
    aZDownToSysAirPosSpeed, aZUpRetractSpeed: integer);
var
    xZSpeed: integer;
begin
    if (aZDownToSysAirPosSpeed > 0) and ((aAirPos = papZDisp) or (aAirPos = papZScan)) then
    begin
        // if there is a "MoveToSysAir" speed, then we want to move with standard speed to the Ztravel position
        // and then from Ztravel down to ZScan or ZDisp with the special speed
        aZMotion.MoveZ(aSyrMap, aZTravel, m_EXECUTE, AT_MOVE_ABS, 0);
    end;

    // if there is a retract speed the tips are going up with this speed
    if (aZUpRetractSpeed > 0) and (aZDownToSysAirPosSpeed <= 0) then
        xZSpeed := aZUpRetractSpeed
    else
        xZSpeed := aZDownToSysAirPosSpeed;

    case (aAirPos) of
        papZScan:
            aZMotion.MoveZ(aSyrMap, aZScan, m_EXECUTE, AT_MOVE_ABS, xZSpeed); // Z Scan
        papZDisp:
            aZMotion.MoveZ(aSyrMap, aZDisp, m_EXECUTE, AT_MOVE_ABS, xZSpeed); // S Disp
        else
            aZMotion.MoveZ(aSyrMap, aZTravel, m_EXECUTE, AT_MOVE_ABS, xZSpeed); // Z Travel
    end;

    TLiquidHandlingLow.PickAir(aPipDevice, aZMotion, aSyrMap, aCh1PumpIndices, aVol, aSpeed, aDelay, aUseVolControl); // Pick air
end;

class procedure TLiquidHandling.PickExtraGap(aPipDevice: IPipDevice; aZMotion: TZAxisPipMoveMotorMotionSystem;
    aZTravelMotion: TZAxisTravelMotionSystem; const aPStep: TPipStepZPosArray; aStep: integer;
    const aRP: TArray<TXRackPosition>; aLiqDet: integer; const aCh1PumpIndices: TIntArray;
    const aVol: TDoubleArray; aSubMerge_mm: TPosMM; var vIsVol: TDoubleArray;
    aZScanMode, aZInsertSpeed, aZInsertMoveType: integer; aSysAirPos: TPickAirPosition;
    const aExtraGapCount: TIntArray; const aExtraGapAirVol: TDoubleArray;
    const aExtraGapAirSpeed: TDoubleArray; aExtraGapAirDelay: integer; const aExtraGapWasteVol: TDoubleArray;
    const aExtraGapWasteSpeed: TDoubleArray; aExtraGapWasteDelay: integer; aUseVolControl: boolean);
// Pick Waste and then Pick Air.  Do this from 1 to aExtraGapCount
var
    x: integer;
    xCount: integer;
    xGapMap: TIPMAP;
    xAirVol, xWasteVol: TDoubleArray;
    xTTips: TIPMAP;
begin
    xCount := 1;
    while true do
    begin
        // add each tip which is in the SyrMap and which still needs to take a gap to xGapMap
        xGapMap := 0;
        xAirVol := aPipDevice.GetNullDoubleArray;
        xWasteVol := aPipDevice.GetNullDoubleArray;
        for x := 0 to aPipDevice.TipCount - 1 do
        begin
            if (not TTipMapUtils.TipSelected(aPStep[aStep].SyrMap, x)) or (aExtraGapCount[x] < xCount) then
                CONTINUE;

            TTipMapUtils.SelectTip(xGapMap, x);
            xAirVol[x] := aExtraGapAirVol[x];
            xWasteVol[x] := aExtraGapWasteVol[x];
        end;

        // if no more tips in map, Exit
        if xGapMap = 0 then
            EXIT;

        if TTipMapUtils.MaxLiquid(xGapMap, xWasteVol) > 0 then
        begin
            // pick waste
            gLogManager.LogF('Aspirate Extra Gap Waste %d, Vol-%s, Speed-%s, Delay-%d',
                [xCount, TArrayUtils.ArrayToBracketText(xWasteVol),
                TArrayUtils.ArrayToBracketText(aExtraGapWasteSpeed), aExtraGapWasteDelay], true);
            xTTips := aZMotion.MoveAspPos2(aZTravelMotion, xGapMap, aPipDevice.OKTips, aRP, aLiqDet, aVol,
                aSubMerge_mm, vIsVol, false, false, aZScanMode, aZInsertSpeed, aZInsertMoveType,
                aPStep[aStep].Z.ZScan, aPStep[aStep].Z.ZMax, aPStep[aStep].Z.ZTube, false);

            PickLiq(aPipDevice, aZMotion, xGapMap, aRP, aCh1PumpIndices, xWasteVol, aExtraGapWasteSpeed,
                aPipDevice.GetNullDoubleArray, aPipDevice.GetNullDoubleArray, aPipDevice.GetNullDoubleArray,
                aPipDevice.GetNullDoubleArray, aExtraGapWasteDelay, xTTips, vIsVol, aPStep[aStep].Z.ZMax,
                aPStep[aStep].Z.ZTube, false, asptLiquid);
        end;

        if TTipMapUtils.MaxLiquid(xGapMap, xAirVol) > 0 then
        begin
            // pick air at system air gap position
            gLogManager.LogF('Aspirate Extra Gap Air %d, Vol-%s, Speed-%s, Delay-%d',
                [xCount, TArrayUtils.ArrayToBracketText(xAirVol),
                TArrayUtils.ArrayToBracketText(aExtraGapAirSpeed), aExtraGapAirDelay], true);
            PickSysAirAtSysAirPos(aPipDevice, aZMotion, aSysAirPos, xGapMap, aPStep[aStep].Z.ZTravel,
                aPStep[aStep].Z.ZScan, aPStep[aStep].Z.ZDisp, aCh1PumpIndices, xAirVol, aExtraGapAirSpeed,
                aExtraGapAirDelay, aUseVolControl, 0, 0);
        end;
        Inc(xCount);
    end;
end;

class procedure TLiquidHandling.ReduceMixVol(aPipDevice: IPipDevice; aTips: TIPMAP; var vMixVol: TDoubleArray;
    const aMinRestVol: double; const aIsVol: TDoubleArray);
var
    x: integer;
begin
    if (aMinRestVol <= 0) then
        EXIT; // 0: MinRestVol ist nicht definiert

    for x := 0 to aPipDevice.TipCount - 1 do
    begin

        if not TTipMapUtils.TipSelected(aTips, x) then
            CONTINUE;

        if (vMixVol[x] > aIsVol[x] - aMinRestVol) then
        begin

            vMixVol[x] := aIsVol[x] - aMinRestVol;
            if (vMixVol[x] < 1) then
                vMixVol[x] := 1; // 1 uL muss sein, sonst wird der Ablauf gestört

            gLogManager.LogF('Tip %d: Mix volume reduced to %f', [x + 1, vMixVol[x]], false);
        end;
    end;
end;

class procedure TLiquidHandling.CompleteMixing(aPipDevice: IPipDevice;
    aZMotion: TZAxisPipMoveMotorMotionSystem; aZTravelMotion: TZAxisTravelMotionSystem; aMixSyrMap: TIPMAP;
    var vMix: TLiqHandlingMixRec; const aZStep: TPipStepZPos; const aRP: TArray<TXRackPosition>;
    const aCh1PumpIndices: TIntArray; const aVol: TDoubleArray; aAspLQModes: integer; aAspSubMerge_mm: TPosMM;
    aRecordDetectionVolume: boolean; aZScanMode, aZInsertSpeed, aZInsertMoveType: integer; aDelay: integer;
    const aSysAirPos: TPickAirPosition; const aZDownToSysAirPosSpeed, aZUpRetractSpeed: integer;
    var vTrackingTips: TIPMAP; var vIsVol: TDoubleArray; aDisableZErr: boolean);
var
    x, xCycle, xLessDispVol: integer;
    xMixVol, xMixDVol, xMixASpeed, xMixDSpeed: TDoubleArray;
    xTrackingWay: TDoubleArray;
    xIniAccess: IWinLissyIniAccess;
    xZUpSpeed: integer;
begin
    if (aMixSyrMap <= 0) then
        EXIT; // no mixing -> EXIT

    xMixVol := TArrayUtils.GetDefinedDoubleArray(vMix.Vol, aPipDevice.TipCount);
    xMixDVol := TArrayUtils.GetNullDoubleArray(aPipDevice.TipCount);
    xMixASpeed := TArrayUtils.GetDefinedDoubleArray(vMix.AspSpeed, aPipDevice.TipCount);
    xMixDSpeed := TArrayUtils.GetDefinedDoubleArray(vMix.DispSpeed, aPipDevice.TipCount);
    xTrackingWay := TArrayUtils.GetNullDoubleArray(aPipDevice.TipCount);

    if (vMix.ZOffset_mm < 0) then
        vMix.ZOffset_mm := 0;

    // Start Mixing
    for xCycle := 0 to vMix.Cycles - 1 do
    begin

        // Mix Aspirate
        if ((vMix.Method and TMixModes.cMixWithAir) <> 0) then
        begin
            // Bei der Luftabgabe darf es kein Tracking geben
            vTrackingTips := 0;

            xZUpSpeed := 0;
            if ((vMix.Method and TMixModes.cUseRetract) <> 0) then
                xZUpSpeed := aZUpRetractSpeed; // Retract Speed aus den Aspirate/Dispense parametern verwenden

            // move to z-travel and pick air
            PickSysAirAtSysAirPos(aPipDevice, aZMotion, aSysAirPos, aMixSyrMap, aZStep.Z.ZTravel,
                aZStep.Z.ZScan, aZStep.Z.ZDisp, aCh1PumpIndices, xMixVol, xMixASpeed, 0, false,
                aZDownToSysAirPosSpeed, xZUpSpeed);
        end;

        if (((vMix.Method and TMixModes.cMixWithAir) = 0) and (xCycle = 0))
        // Normal: Fahre nur beim ersten Mal aus Asp-Position
            or (((vMix.Method and TMixModes.cMixWithAir) = 0) and
            ((vMix.Method and TMixModes.cDispenseAtZOffset) <> 0))
        // Disp at z-Offset: fahre jedes mal auf asp-position
            or (((vMix.Method and TMixModes.cMixWithAir) <> 0) and
            ((vMix.Method and TMixModes.cDispenseAtZOffset) = 0))
        // Mix with air, aber nicht Disp at z-Offset: fahre jedes mal auf asp-position
        then
        begin

            vTrackingTips := aZMotion.MoveAspPos2(aZTravelMotion, aMixSyrMap, aPipDevice.OKTips, aRP,
                aAspLQModes, aVol, aAspSubMerge_mm, vIsVol, vMix.UseCalculatedVol, aRecordDetectionVolume,
                aZScanMode, aZInsertSpeed, aZInsertMoveType, aZStep.Z.ZScan, aZStep.Z.ZMax, aZStep.Z.ZTube,
                aDisableZErr);

            // Mix-Volumen evtl. verkleinern
            ReduceMixVol(aPipDevice, aMixSyrMap, xMixVol, vMix.MinRestVol, vIsVol);
        end;

        if ((vMix.Method and TMixModes.cMixWithAir) = 0) then
        begin
            xTrackingWay := PickLiq(aPipDevice, aZMotion, aMixSyrMap, aRP, aCh1PumpIndices, xMixVol,
                xMixASpeed, aPipDevice.GetNullDoubleArray, aPipDevice.GetNullDoubleArray,
                aPipDevice.GetNullDoubleArray, aPipDevice.GetNullDoubleArray, aDelay, vTrackingTips, vIsVol,
                aZStep.Z.ZMax, aZStep.Z.ZTube, false, asptLiquid);
        end;

        // fahre zu z-max - z-offset
        if ((vMix.Method and TMixModes.cDispenseAtZOffset) <> 0) then
        begin
            aZMotion.MoveZ_WithOffset(aMixSyrMap, aZStep.Z.ZMax, vMix.ZOffset_mm, m_EXECUTE);
            vTrackingTips := 0; // in diesem Fall kein Tracking
        end;

        // Dangerous Hack for Bayer France: Please do not use this!
        xIniAccess := gCommonDll.CreateRobotIni;
        xLessDispVol := xIniAccess.ReadInteger('Mixing', 'DispLessThanAsp');
        for x := 0 to aPipDevice.TipCount - 1 do
        begin
            xMixDVol[x] := xMixVol[x];
            if (xLessDispVol > 0) and (xMixVol[x] > 0) then
                xMixDVol[x] := MaxValue([1, xMixDVol[x] - xLessDispVol]);
        end;

        // Mix Dispense
        DispLiqWithoutCalc(aPipDevice, aZMotion, aMixSyrMap, aRP, aCh1PumpIndices, xMixDVol,
            aPipDevice.GetNullDoubleArray, xMixDSpeed, aPipDevice.GetNullDoubleArray,
            aPipDevice.GetNullDoubleArray, aPipDevice.GetNullDoubleArray, aPipDevice.GetNullDoubleArray,
            aDelay, vIsVol, ((vMix.Method and TMixModes.cMixWithAir) = 0), false, false, xTrackingWay);

        gLogManager.LogF('MixMethod: %d - MixCycles %d: %d executed !',
            [vMix.Method, vMix.Cycles, xCycle + 1], false);
    end;
end;

class function TLiquidHandling.GetNextAspMixSyrMap(aPipDevice: IPipDevice; aSyrMap: TIPMAP;
    const aRP: TArray<TXRackPosition>; const aMix: TLiqHandlingMixRec; const aMixFirstOnly: boolean): TIPMAP;
var
    x: integer;
begin
    result := 0;
    if (aMix.Cycles < 1) then
        EXIT;

    // Bestimmen, welche Tips gemixt werden
    for x := 0 to aPipDevice.TipCount - 1 do
    begin

        if not TTipMapUtils.TipSelected(aSyrMap, x) then
            CONTINUE;

        ASSERT(Assigned(aRP[x].Rack), 'Rack undefined');

        // Ist MixAtFirstOnly aktiv und wurde die Probe bereits gemixt?
        if (aMixFirstOnly) and aPipDevice.Tips[x].IsSampleAlreadyMixed(aRP[x].Rack.name, aRP[x].Pos) then
            CONTINUE;

        TTipMapUtils.SelectTip(result, x);
        aPipDevice.Tips[x].SaveLastAspMix(aRP[x].Rack.name, aRP[x].Pos);
    end;
end;

// --------------------------------------------------------------------------------------------------
// Aspirate
// --------------------------------------------------------------------------------------------------
class procedure TLiquidHandling.Aspirate(aUsedArm: IArmDevice; aSyrMap: TIPMAP;
    const aRP: TArray<TXRackPosition>; // array of Rack Positions
    const aCh1PumpIndices: TIntArray; const aSysAir: TDoubleArray; // airgap volumes before aspiration
    const aSysAirSpeed: TDoubleArray; // system Airgap speed
    const aAspVol: TDoubleArray; // aspiration volumes
    const aVolStepsCh1: TDoubleArray; const aVolStepsCh2: TDoubleArray; const aTransAir: TDoubleArray;
    // transport airgap volumes after aspiration
    const aSpeed: TDoubleArray; // aspiration speeds
    const aTransAirSpeed: TDoubleArray; // transport asp speed
    aSysAirPos: TPickAirPosition; aTransAirPos: TPickAirPosition;
    // ZPosition, wo die Luftblase gezogen werden soll
    aDelay: integer; // aspiration delay
    const aSpitBackVol: TDoubleArray; // spit back volumes
    aSpitBackCount: integer; // number of spit backs
    const aSpitBackSpeed: TDoubleArray; // spit back dispense speeds
    aSpitBackAtAspPos: boolean; // spit back position (aspirate or dispense)
    const aCh2WashVol: TDoubleArray; // TipWashVol Channel2
    const aCh2WashSpeed: TDoubleArray; // TipWashSpeed Channel2
    aCh2WashDelay: integer; aSysAirDelay: integer; aTransAirDelay: integer; // dispense delay
    aLiqDet: integer; // How to use the Liquid detector
    aSubMerge_mm: TPosMM; // Subtract mm
    aMix: TLiqHandlingMixRec; const aMixFirstOnly: boolean; // Mixing Parameter
    aSwitchDeviceName: string; // Port to Switch
    aSwitchPlace: TSwitchPlace; // switch above or in Tube
    aSingleRetract: boolean; aRecordDetectionVolume: boolean; // write Detection Volume in Database !
    aAspirationMethodRetractZ: integer; // Aspiration Method for small Volumes
    aSampleAspRetractPos_mm: TPosMM; // Position from Zmax to travel retract
    aDispCh1and2together: boolean; aEvBeforeAsp, aEvBeforePickLq, aEvAfterPickLq, aEvAfterAsp: TRunstCall;
    aDisableZErr: boolean; aZScanMode, aZInsertSpeed, aZInsertMoveType, aZRetractSpeed: integer;
    aZRetractDistance_mm: TPosMM; aTipTouch, aTipTouchScan: boolean; aTipTouchDelay: integer;
    aTipTouchScanMode: integer; aTipTouchSingle: boolean; const aTipTouchSubmerge: TDoubleArray;
    const aExtraGapCount: TIntArray; const aExtraGapAirVol: TDoubleArray;
    const aExtraGapAirSpeed: TDoubleArray; aExtraGapAirDelay: integer; aExtraGapAirPos: TPickAirPosition;
    const aExtraGapWasteVol: TDoubleArray; const aExtraGapWasteSpeed: TDoubleArray;
    aExtraGapWasteDelay: integer; const aExtraGapLastWasteVol: TDoubleArray; aAspType: TAspirateType;
    aMoveToSysAirPosSpeed: integer);
// --------------------------------------------------------------------------------------------------
var
    xStep, x, i: integer;
    xTTips: TIPMAP;
    xIsVol: TDoubleArray;
    xSpitBackSteps: TDoubleArray;
    xRetractMoveDone: boolean;
    xDisabledErrorMap: TIPMAP;
    xXYSteps: TXYStepList;
    xZSteps: TPipStepZPosArray;
    xPipDevice: IPipDevice;
    xXYOp: TXYZTravelAxisPipMoveOperation;
    xZMotion: TZAxisPipMoveMotorMotionSystem;
    xZTravelMotion: TZAxisTravelMotionSystem;
    xAspVolWithWasteVol: TDoubleArray;
    xMixSyrMap: TIPMAP;
begin
    xPipDevice := aUsedArm.PipDevice;

    if Assigned(aEvBeforeAsp) then
        aEvBeforeAsp.Execute('before aspiration');

    xTTips := 0;
    gLogManager.Log(Format('Aspirate Delay=%d TransAirDelay=%d LiqDet=%d Submerge mm=%f MixCycle=%d',
        [aDelay, aTransAirDelay, aLiqDet, aSubMerge_mm, aMix.Cycles]), false);

    if (gErrorManager.IsGlobalErr()) then
        EXIT;

    aSyrMap := aSyrMap and xPipDevice.OKTips;

    xIsVol := xPipDevice.GetNullDoubleArray;
    xSpitBackSteps := xPipDevice.GetNullDoubleArray;
    xAspVolWithWasteVol := xPipDevice.GetNullDoubleArray;
    for x := 0 to xPipDevice.TipCount - 1 do
    begin
        xAspVolWithWasteVol[x] := aAspVol[x] + aExtraGapLastWasteVol[x];
    end;

    xZTravelMotion := TOperationFactory.CreateZAxisTravelMotionSys(aUsedArm.MotionDevice);
    xZMotion := TOperationFactory.CreateZAxisPipMoveMotorMotionSys(aUsedArm.MotionDevice);

    gmCalculatePipSteps(aUsedArm, aSyrMap, aRP, xXYSteps, xZSteps);
    xXYOp := TOperationFactory.CreateXYZTravelPipMoveOp(aUsedArm);
    // Volumenkorrektur
    for x := 0 to xPipDevice.TipCount - 1 do
        if ((aSyrMap and (1 shl x)) > 0) and (xPipDevice.Tips[x].GetPipPump(aCh1PumpIndices[x]) <> nil) then
            xSpitBackSteps[x] := xPipDevice.Tips[x].GetPipPump(aCh1PumpIndices[x])
                .VolumeULToVolumeSteps(aSpitBackVol[x]);

    for xStep := 0 to xXYSteps.Count - 1 do
    begin
        if (xZSteps[xStep].SyrMap = 0) then
            CONTINUE;

        xDisabledErrorMap := aSyrMap;
        xRetractMoveDone := false;
        if (TTipMapUtils.MaxLiquid(xZSteps[xStep].SyrMap, aAspVol) > 0) then
        begin
            // --------------------------------------------------------------------- Move to step position
            xXYOp.MoveXY(xXYSteps[xStep], aRP, xZSteps[xStep], []);

            // Module Port über dem Röhrchen einschalten
            if (aSwitchPlace = spSwitchOutside) then
                gmSwitchModulePort(xPipDevice, xZSteps[xStep].SyrMap, aSwitchDeviceName, true);

            // --------------------------------------------------------------------------- Take system airgap before
            // If aSysAirPos = 0, Airgap has already been taken
            // 10.01.05 pk : Does anyone know why we do NOT check if we have already aspirated an air gap (ActSysAir) ?
            if (TTipMapUtils.MaxLiquid(xZSteps[xStep].SyrMap, aSysAir) > 0) and
                (aSysAirPos <> papUndefined) then
            begin
                PickSysAirAtSysAirPos(xPipDevice, xZMotion, aSysAirPos, xZSteps[xStep].SyrMap,
                    xZSteps[xStep].Z.ZTravel, xZSteps[xStep].Z.ZScan, xZSteps[xStep].Z.ZDisp, aCh1PumpIndices,
                    aSysAir, aSysAirSpeed, aSysAirDelay, true, aMoveToSysAirPosSpeed, 0);

            end;

            // ------------Z move Error für den Aspirate ignorieren--------------------------
            // for ZP02 disable must be done again before each z-move
            if (aDisableZErr) then
                xZMotion.DisableZErrors(xDisabledErrorMap);

            // --------------------------------------------------------------------- Mixing -------------
            xMixSyrMap := GetNextAspMixSyrMap(xPipDevice, xZSteps[xStep].SyrMap, aRP, aMix, aMixFirstOnly);
            CompleteMixing(xPipDevice, xZMotion, xZTravelMotion, xMixSyrMap, aMix, xZSteps[xStep], aRP,
                aCh1PumpIndices, xAspVolWithWasteVol, aLiqDet, aSubMerge_mm, aRecordDetectionVolume,
                aZScanMode, aZInsertSpeed, aZInsertMoveType, aDelay, aSysAirPos, aMoveToSysAirPosSpeed,
                aZRetractSpeed, xTTips, xIsVol, aDisableZErr);

            // -------------------------------------------------------------------------- ExtraGap
            PickExtraGap(xPipDevice, xZMotion, xZTravelMotion, xZSteps, xStep, aRP, aLiqDet, aCh1PumpIndices,
                xAspVolWithWasteVol, aSubMerge_mm, xIsVol, aZScanMode, aZInsertSpeed, aZInsertMoveType,
                aExtraGapAirPos, aExtraGapCount, aExtraGapAirVol, aExtraGapAirSpeed, aExtraGapAirDelay,
                aExtraGapWasteVol, aExtraGapWasteSpeed, aExtraGapWasteDelay, true);

            // --------------------------------------------------------------------- Aspiration Position in Z anfahren
            if (xMixSyrMap < xZSteps[xStep].SyrMap) // einer der Tips wurde nicht gemixt
                or ((aMix.Method and TMixModes.cDispenseAtZOffset) <> 0) then
            begin // Disp at z-Offset: es muss wieder auf asp-position gefahren werden

                xTTips := xZMotion.MoveAspPos2(xZTravelMotion, xZSteps[xStep].SyrMap, xPipDevice.OKTips, aRP,
                    aLiqDet, xAspVolWithWasteVol, aSubMerge_mm, xIsVol, false, aRecordDetectionVolume,
                    aZScanMode, aZInsertSpeed, aZInsertMoveType, xZSteps[xStep].Z.ZScan,
                    xZSteps[xStep].Z.ZMax, xZSteps[xStep].Z.ZTube, aDisableZErr);

                if (aAspirationMethodRetractZ <> 0) then
                begin
                    xTTips := xZSteps[xStep].SyrMap;
                    xZMotion.MoveZ(xZSteps[xStep].SyrMap, xZSteps[xStep].Z.ZMax, m_EXECUTE);
                    gLogManager.Log('Aspirate: AspirationMethodRetractZ (Goto ZMax)', false);
                end;
            end;

            // --------------------------------------------------------------------- Module Port nach dem Heruntergehen anschalten
            if (aSwitchPlace = spSwitchInside) then
                gmSwitchModulePort(xPipDevice, xZSteps[xStep].SyrMap, aSwitchDeviceName, true);

            // --------------------------------------------------------------------- Event before PickLiquid
            if Assigned(aEvBeforePickLq) then
                aEvBeforePickLq.Execute('before pick liquid');

            // --------------------------------------------------------------------- es wird nur das detektierte Volumen verteilt
            if ((aLiqDet and INT_LQMODES_LQ_PIP_ISVOL) <> 0) then
            begin
                for x := 0 to xPipDevice.TipCount - 1 do
                begin
                    gLogManager.LogF('Tip[%d]: Volume %f IsVol %f',
                        [x + 1, xAspVolWithWasteVol[x], xIsVol[x]], false);

                    if (((xZSteps[xStep].SyrMap shr x) and 1) <> 0) and
                        (xIsVol[x] < xAspVolWithWasteVol[x]) then
                    begin
                        gLogManager.LogF('Tip[%d]: Volume %f reduced to %f',
                            [x + 1, xAspVolWithWasteVol[x], xIsVol[x]], false);
                        xAspVolWithWasteVol[x] := xIsVol[x];
                    end;
                end;
            end;

            // --------------------------------------------------------------------- PickLiquid
            if (aAspirationMethodRetractZ = 0) then
            begin
                PickLiq(xPipDevice, xZMotion, xZSteps[xStep].SyrMap, aRP, aCh1PumpIndices,
                    xAspVolWithWasteVol, aSpeed, aCh2WashVol, xPipDevice.GetNullDoubleArray, aVolStepsCh1,
                    aVolStepsCh2, aDelay, xTTips, xIsVol, xZSteps[xStep].Z.ZMax, xZSteps[xStep].Z.ZTube, true,
                    aAspType);
            end
            else
            begin
                PickLiq_from_Zmax_with_Retract(xPipDevice, xZMotion, xZSteps[xStep].SyrMap, aRP,
                    aCh1PumpIndices, xAspVolWithWasteVol, aSpeed, aCh2WashVol, xPipDevice.GetNullDoubleArray,
                    aVolStepsCh1, aVolStepsCh2, aDelay, xZSteps[xStep].SyrMap, aSampleAspRetractPos_mm, true,
                    aAspType);
            end;

            // --------------------------------------------------------------------- Dispense Spit back volume
            xZSteps[xStep].SyrMap := xZSteps[xStep].SyrMap and xPipDevice.OKTips;
            if (TTipMapUtils.MaxLiquid(xZSteps[xStep].SyrMap, aSpitBackVol) > 0) or
                (TTipMapUtils.MaxLiquid(xZSteps[xStep].SyrMap, aCh2WashVol) > 0) then
            begin
                if (not aSpitBackAtAspPos) then
                begin
                    if aZRetractDistance_mm <= 0 then
                    begin
                        // fahre auf Z-Dispense (mit retract speed)
                        for x := 0 to xPipDevice.TipCount - 1 do
                        begin
                            if not TTipMapUtils.TipSelected(xZSteps[xStep].SyrMap, x) then
                                CONTINUE;
                            xZMotion.MoveSingleZ_Retract(x, xZSteps[xStep].Z.ZDisp[x], m_NO_EXEC,
                                aZRetractSpeed, false); // auf Z-Disp fahren
                        end;
                        xZMotion.Execute;
                    end
                    else
                    begin
                        // Fahre auf die Retract position
                        xRetractMoveDone := xZMotion.MoveRetractDistance(xZSteps[xStep].SyrMap,
                            aZRetractSpeed, aZRetractDistance_mm, aSingleRetract, false, aDisableZErr);
                    end;
                end;

                for i := 0 to aSpitBackCount - 1 do
                begin
                    DispLiq(xPipDevice, xZMotion, xZSteps[xStep].SyrMap, aRP, aCh1PumpIndices, aSpitBackVol,
                        xPipDevice.GetNullDoubleArray, aSpitBackSpeed, aCh2WashVol, aCh2WashSpeed,
                        xSpitBackSteps, aVolStepsCh2, aCh2WashDelay, 0, xIsVol, true, aDispCh1and2together,
                        xZSteps[xStep].Z.ZMax, xZSteps[xStep].Z.ZTube);
                    gLogManager.Log(Format('Aspirate: Spitback executed SyrMap %d',
                        [xZSteps[xStep].SyrMap]), false);
                end;
            end; // -------------------------------------------------------------------- Ende SpitBack

            // ------------------------------------------------------------------------- Tip Touch
            if aTipTouch then
            begin
                xZMotion.TipTouch(xZSteps[xStep].SyrMap, xZSteps[xStep].SyrMap, aRP, aTipTouchDelay,
                    aTipTouchScan, aTipTouchScanMode, aTipTouchSingle, aZRetractSpeed, aZRetractDistance_mm,
                    xZSteps[xStep].Z.ZMax, xZSteps[xStep].Z.ZTube, aTipTouchSubmerge, aZInsertSpeed,
                    aSingleRetract, false, xRetractMoveDone, aDisableZErr, false);
                gLogManager.Log(Format('Aspirate: Tiptouch executed SyrMap %d',
                    [xZSteps[xStep].SyrMap]), false);
            end;

            // --------------------------------------------------------------------- Event after PickLiquid
            if Assigned(aEvAfterPickLq) then
                aEvAfterPickLq.Execute('after pick liquid');

            // --------------------------------------------------------------------- Module Port im Tube ausschalten
            if (aSwitchPlace = spSwitchInside) then
                gmSwitchModulePort(xPipDevice, xZSteps[xStep].SyrMap, aSwitchDeviceName, false);
        end;

        // ------------------------------------------------ Retract: Move to transport air position
        RetractAndGetTransportAir(xPipDevice, xZMotion, xZSteps[xStep], aCh1PumpIndices, aTransAir,
            aTransAirSpeed, aTransAirPos, aTransAirDelay, aZRetractSpeed, aZRetractDistance_mm,
            aSingleRetract, false, true, xRetractMoveDone, aDisableZErr);

        if (aDisableZErr) then
            xZMotion.ResetDisabledZErrors(xDisabledErrorMap);

        // ------------------------------------------------------------------------  Module Port ausserhalb des Tubes ausschalten
        if (aSwitchPlace = spSwitchOutside) then
            gmSwitchModulePort(xPipDevice, xZSteps[xStep].SyrMap, aSwitchDeviceName, false);
    end;
    if Assigned(aEvAfterAsp) then
        aEvAfterAsp.Execute('after aspiration');

    FreeAndNil(xXYOp);
    FreeAndNil(xZMotion);
    FreeAndNil(xZTravelMotion);
    FreeAndNil(xXYSteps);
end;

class function TLiquidHandling.NoDispenseVolume(aPipDevice: IPipDevice;
    const aVolCh1, aAirVolCh1, aVolCh2: TDoubleArray): boolean;
var
    xSumVolCh1, xSumVolCh2: extended;
    x: integer;
begin
    xSumVolCh1 := 0;
    xSumVolCh2 := 0;
    for x := 0 to aPipDevice.TipCount - 1 do
    begin
        xSumVolCh1 := xSumVolCh1 + aVolCh1[x] - aAirVolCh1[x];
        xSumVolCh2 := xSumVolCh2 + aVolCh2[x];
        // kein Airgap für Channel 2, da damit bisher nur System aufgezogen werden kann
    end;
    result := (xSumVolCh1 <= 0) and (xSumVolCh2 <= 0);
end;

// --------------------------------------------------------------------------------------------------
// Dispense
// --------------------------------------------------------------------------------------------------
class procedure TLiquidHandling.Dispense(aUsedArm: IArmDevice; aSyrMap: TIPMAP;
    const aRP: TArray<TXRackPosition>; // array of Rack Positions
    const aCh1PumpIndices: TIntArray; const aVol: TDoubleArray; // array of volumes
    const aVolCh2: TDoubleArray; // array of volumes Channel 2
    const aTotalVol: TDoubleArray;
    // array of total volume ( dispense is performed in n steps of Vol to reach TtotalVol )
    const aAirVol: TDoubleArray; // array of volumes of AirGapVolumes (Trans + Sys)
    const aSpeed: TDoubleArray; // array of dispense speeds
    aDelay: integer; // dispense delay
    const aTransAir: TDoubleArray; const aTransAirSpeed: TDoubleArray; aTransAirPos: TPickAirPosition;
    // ZPosition, wo die Luftblase gezogen werden soll
    aMix: TLiqHandlingMixRec; aMixAspSubmerge: double; // Mixing Parameter
    aLiqDet: integer; // LiquidDetector Mode
    aSubMerge_mm: TPosMM; // Subtract steps
    aPerformingSteps: double; aPerformDelay: integer; aSwitchDeviceName: string; // Port to Switch
    aSwitchPlace: TSwitchPlace; // switch position
    aSingleRetract: boolean; aDispCh1and2together: boolean; aEvBeforeDisp: TRunstCall;
    aEvBeforeDispLq: TRunstCall; aEvAfterDispLq: TRunstCall; aEvAfterDisp: TRunstCall; aDisableZErr: boolean;
    aZScanMode: integer; aZInsertSpeed: integer; aZInsertMoveType: integer; aZRetractSpeed: integer;
    aZRetractDistance_mm: TPosMM; aTipTouch, aTipTouchScan: boolean; aTipTouchDelay: integer;
    aTipTouchScanMode: integer; aTipTouchSingle: boolean; const aTipTouchSubmerge: TDoubleArray;
    aTipTouchScanStoreVol: boolean);
// Default-Werte für Dispense-Aufruf in PeriCal.pas
// --------------------------------------------------------------------------------------------------
var
    xStep, x, xMixLQModeAsp: integer;
    xSumSteps: extended;
    xTrackingTips, xVTips: TIPMAP;
    xIsVol, xPerStepVol, xVolWithoutAir: TDoubleArray;
    xSumVol: extended;
    xStepsCh1, xStepsCh2, xStepsVolWithoutAir, xStepsAir: TDoubleArray;
    xRetractMoveDone: boolean;
    xDisabledErrorMap: TIPMAP;
    xXYSteps: TXYStepList;
    xZSteps: TPipStepZPosArray;
    xPipDevice: IPipDevice;
    xXYOp: TXYZTravelAxisPipMoveOperation;
    xZMotion: TZAxisPipMoveMotorMotionSystem;
    xZTravelMotion: TZAxisTravelMotionSystem;
    xVolCorrCurves: TArray<string>;
begin
    if (gErrorManager.IsGlobalErr()) then
        EXIT;
    xPipDevice := aUsedArm.PipDevice;
    if NoDispenseVolume(xPipDevice, aVol, aAirVol, aVolCh2) then
        EXIT;

    xZMotion := TOperationFactory.CreateZAxisPipMoveMotorMotionSys(aUsedArm.MotionDevice);
    xZTravelMotion := TOperationFactory.CreateZAxisTravelMotionSys(aUsedArm.MotionDevice);

    xIsVol := xPipDevice.GetNullDoubleArray;
    xPerStepVol := xPipDevice.GetNullDoubleArray;
    xVolWithoutAir := xPipDevice.GetNullDoubleArray;
    xStepsCh1 := xPipDevice.GetNullDoubleArray;

    // Volumenkorrektur
    for x := 0 to xPipDevice.TipCount - 1 do
    begin
        if (aSyrMap and (1 shl x)) > 0 then
        begin
            xVolWithoutAir[x] := aVol[x] - aAirVol[x];
        end;
    end;

    xVolCorrCurves := TLiquidHandling.GetVolCorrCurves(xPipDevice);
    xStepsVolWithoutAir := CalcDilutorSteps(xPipDevice, aSyrMap, xVolWithoutAir, aCh1PumpIndices,
        xVolCorrCurves);

    xStepsAir := CalcDilutorSteps(xPipDevice, aSyrMap, aAirVol, aCh1PumpIndices, nil);
    for x := 0 to xPipDevice.TipCount - 1 do
        if (aSyrMap and (1 shl x)) > 0 then
            xStepsCh1[x] := xStepsVolWithoutAir[x] + xStepsAir[x];
    xStepsCh2 := CalcDilutorSteps(xPipDevice, aSyrMap, aVolCh2,
        TLiquidChannels.GetDefaultChannel2Array(xPipDevice), xVolCorrCurves);

    if Assigned(aEvBeforeDisp) then
        aEvBeforeDisp.Execute('before dispense');

    // -------------------------------------------------- if Vol+PeriVol = 0 Tip aus Syrmap entfernen
    xVTips := 0;
    for x := 0 to xPipDevice.TipCount - 1 do
    begin
        if (aVol[x] + aVolCh2[x] > 0) then
            xVTips := xVTips or (1 shl x);
    end;
    aSyrMap := aSyrMap and xVTips;

    gmCalculatePipSteps(aUsedArm, aSyrMap, aRP, xXYSteps, xZSteps);

    xXYOp := TOperationFactory.CreateXYZTravelPipMoveOp(aUsedArm);

    for xStep := 0 to xXYSteps.Count - 1 do
    begin
        if (xZSteps[xStep].SyrMap <= 0) then
            CONTINUE;

        xRetractMoveDone := false;

        xZSteps[xStep].SyrMap := xZSteps[xStep].SyrMap and xPipDevice.OKTips;
        xDisabledErrorMap := aSyrMap;
        if (TTipMapUtils.MaxLiquid(xZSteps[xStep].SyrMap, aVol) > 0) or
            (TTipMapUtils.MaxLiquid(xZSteps[xStep].SyrMap, aVolCh2) > 0) then
        begin

            // ------------------------------------------- Move to Dispense Position (Move along Corridor)
            xXYOp.MoveXY(xXYSteps[xStep], aRP, xZSteps[xStep], [xyoUseCorridor]);
            // ----------------------------------------------- ModulePort vor dem Heruntergehen anschalten
            if (aSwitchPlace = spSwitchOutside) then
                gmSwitchModulePort(xPipDevice, xZSteps[xStep].SyrMap, aSwitchDeviceName, true);

            // ------------Z move Error für den Dispense ignorieren--------------------------
            // for ZP02 disable must be done again before each z-move
            if (aDisableZErr) then
                xZMotion.DisableZErrors(xDisabledErrorMap);
            // -------------------------------------------------------------- Auf Dispense Position fahren
            xTrackingTips := xZMotion.MoveDspPos2(xZTravelMotion, xZSteps[xStep].SyrMap, xPipDevice.OKTips,
                aRP, aLiqDet, dapDispAtZDisp, aVol, xIsVol, aSubMerge_mm, aZScanMode, aZInsertSpeed,
                aZInsertMoveType, aZRetractSpeed, xZSteps[xStep].Z.ZScan, xZSteps[xStep].Z.ZDisp,
                xZSteps[xStep].Z.ZMax, xZSteps[xStep].Z.ZTube, aDisableZErr);

            // -------------------------------------------------------------------------- ModulePort nach dem Heruntergehen anschalten
            if (aSwitchPlace = spSwitchInside) then
                gmSwitchModulePort(xPipDevice, xZSteps[xStep].SyrMap, aSwitchDeviceName, true);

            // -------------------------------------------------------------------------- Nur bis zu einem maximalen Volumen auffüllen
            if ((aLiqDet and INT_LQMODES_LQ_PIP_ISVOL) <> 0) then
            begin
                for x := 0 to xPipDevice.TipCount - 1 do
                begin
                    if (((xZSteps[xStep].SyrMap shr x) and 1) <> 0) and
                        (aTotalVol[x] - xIsVol[x] < aVol[x]) then
                    begin
                        gLogManager.LogF('Tip[%d]: Dispense volume %f reduced to %f = Total tube volume %f',
                            [x + 1, aVol[x], aTotalVol[x] - xIsVol[x], aTotalVol[x]], false);
                        aVol[x] := aTotalVol[x] - xIsVol[x];
                    end;
                end;
                PickSys(xPipDevice, xZMotion, xZSteps[xStep].SyrMap, aCh1PumpIndices, aVol, aSpeed,
                    xPipDevice.GetNullDoubleArray, xPipDevice.GetNullDoubleArray,
                    xPipDevice.GetNullDoubleArray, xPipDevice.GetNullDoubleArray, aDelay, false);
                gRunFlow.AppSleep(100); // --------- Das Systemflüssigkeitsvolumen aufnehmen
            end;

            // -------------------------------------------------------------------------- Dispense
            DispLiqWithEvents(xPipDevice, xZMotion, xZSteps[xStep].SyrMap, aRP, aCh1PumpIndices, aVol,
                aAirVol, aSpeed, aVolCh2, aSpeed, xStepsCh1, xStepsCh2, aDelay, xTrackingTips, xIsVol, true,
                aDispCh1and2together, xZSteps[xStep].Z.ZMax, xZSteps[xStep].Z.ZTube, aEvBeforeDispLq,
                aEvAfterDispLq, aPerformingSteps, aPerformDelay);

            // ------------------------------------------------------------------------- Tip Touch
            if aTipTouch then
            begin
                xZMotion.TipTouch(xPipDevice.GetZRetractTips(xZSteps[xStep].SyrMap, true),
                    xPipDevice.GetZRetractTips(xZSteps[xStep].SyrMap, true), aRP, aTipTouchDelay,
                    aTipTouchScan, aTipTouchScanMode, aTipTouchSingle, aZRetractSpeed, aZRetractDistance_mm,
                    xZSteps[xStep].Z.ZMax, xZSteps[xStep].Z.ZTube, aTipTouchSubmerge, aZInsertSpeed,
                    aSingleRetract, true, xRetractMoveDone, aDisableZErr, aTipTouchScanStoreVol);

                gLogManager.Log(Format('Dispense: Tiptouch executed SyrMap %d',
                    [xZSteps[xStep].SyrMap]), false);
            end;

            // -------------------------------------------------------------------------- Mixing -----
            if ((aMix.Method and TMixModes.cAspirateTracking) <> 0) then
                xMixLQModeAsp := aLiqDet or INT_LQMODES_TRACKING
            else
                xMixLQModeAsp := aLiqDet and not INT_LQMODES_TRACKING;

            if ((aMix.Method and TMixModes.cAspirateWithScan) <> 0) then
            begin
                xMixLQModeAsp := xMixLQModeAsp or INT_LQMODES_LQ_SCAN; // Liquid detection
                xMixLQModeAsp := xMixLQModeAsp and not INT_LQMODES_LQ_DISP_ERROR; // Fehler nicht anzeigen
                xMixLQModeAsp := xMixLQModeAsp or INT_LQMODES_LQ_ERROR_GOTO; // immer auf Z-Max gehen
            end
            else
            begin
                xMixLQModeAsp := xMixLQModeAsp and not INT_LQMODES_LQ_SCAN; // kein liquid detection
            end;

            CompleteMixing(xPipDevice, xZMotion, xZTravelMotion, xZSteps[xStep].SyrMap, aMix, xZSteps[xStep],
                aRP, aCh1PumpIndices, aVol, xMixLQModeAsp, aMixAspSubmerge, false, aZScanMode, aZInsertSpeed,
                aZInsertMoveType, aDelay, papZTravel, 0, aZRetractSpeed, xTrackingTips, xIsVol, aDisableZErr);

            // -------------------------------------------------------------------------- Module Port vor dem Herausgehen ausschalten
            if (aSwitchPlace = spSwitchInside) then
                gmSwitchModulePort(xPipDevice, xZSteps[xStep].SyrMap, aSwitchDeviceName, false);

        end;

        // Retract: Move to transport air position
        RetractAndGetTransportAir(xPipDevice, xZMotion, xZSteps[xStep], aCh1PumpIndices, aTransAir,
            aTransAirSpeed, aTransAirPos, 0, aZRetractSpeed, aZRetractDistance_mm, aSingleRetract, true, true,
            xRetractMoveDone, aDisableZErr);

        if (aDisableZErr) then
            xZMotion.ResetDisabledZErrors(xDisabledErrorMap);
        // -------------------------------------------------------------------------- Module Port nach dem Herausgehen ausschalten
        if (aSwitchPlace = spSwitchOutside) then
            gmSwitchModulePort(xPipDevice, xZSteps[xStep].SyrMap, aSwitchDeviceName, false);
    end;
    if Assigned(aEvAfterDisp) then
        aEvAfterDisp.Execute('after dispense');

    FreeAndNil(xXYOp);
    FreeAndNil(xZMotion);
    FreeAndNil(xZTravelMotion);
    FreeAndNil(xXYSteps);
end;

class procedure TLiquidHandling.WashAtWashRack(aUsedArm: IArmDevice; aPipDevice: IPipDevice;
    aZMotion: TZAxisPipMoveMotorMotionSystem; aTips: TIPMAP; const aVolume, aVolume2: TDoubleArray;
    aUsePeripump: boolean; aUseCorridor: boolean; aWashRetractSpeed: integer; aEmptyDilutors: boolean;
    const aPumpIndices: TIntArray);
var
    xCurrentRack: TRack;
    xXYStep: TXYStep;
    xZStep: TPipStepZPos;
    xXYOp: TXYZTravelAxisPipMoveOperation;
    xXYOptions: TMoveXYMovementOptions;
begin
    // Normales Waschen in Wash-Rack
    if (gErrorManager.IsGlobalErr) then
        EXIT;
    xCurrentRack := TLiquidHandlingLow.FindWashRack(aPipDevice);

    xXYStep := gmCalculatePipStep(aUsedArm, aTips, gmGetRackPosArrayForTips(aPipDevice,
        xCurrentRack), xZStep);
    if not Assigned(xXYStep) then
        EXIT;
    try
        xXYOptions := [];
        if aUseCorridor then
            xXYOptions := [xyoUseCorridor];

        xXYOp := TOperationFactory.CreateXYZTravelPipMoveOp(aUsedArm);
        try
            xXYOp.MoveXY(xXYStep, gmGetRackPosArrayForTips(aUsedArm.PipDevice, xCurrentRack), xZStep,
                xXYOptions);
        finally
            FreeAndNil(xXYOp);
        end;

        aZMotion.MoveZ(xZStep.SyrMap, xZStep.Z.ZMax, m_EXECUTE);

        // entleere alle Dilutoren in WASH rack
        if (aEmptyDilutors) then
        begin
            TLiquidHandlingLow.ChangeValvePos1To4(aUsedArm.PipDevice, aTips, V_SYR_TIP);
            TLiquidHandlingLow.EmptyDilutors(aUsedArm.PipDevice, aTips, aPumpIndices);
            TLiquidHandlingLow.EmptyDilutors(aUsedArm.PipDevice, aTips,
                TLiquidChannels.GetDefaultChannel2Array(aUsedArm.PipDevice));
            // für mehr als vier Dilutoren (SOPHAS)
        end;

        TLiquidHandlingLow.FlushSyringeOrPeripump(aPipDevice, aZMotion, xCurrentRack, aVolume, aVolume2,
            aUsePeripump, 0, aPumpIndices);

        // Nadeln auf Z-Travel (mit WashRetractSpeed)
        aZMotion.MoveZ_WashRetract(xZStep.SyrMap, xZStep.Z.ZTravel, aWashRetractSpeed);
    finally
        FreeAndNil(xXYStep);
    end;
end;

class procedure TLiquidHandling.WashAtWasteRack(aUsedArm: IArmDevice; aPipDevice: IPipDevice;
    aZMotion: TZAxisPipMoveMotorMotionSystem; aTips: TIPMAP; const aVolume, aVolume2: TDoubleArray;
    aUsePeripump: boolean; aUseCorridor: boolean; aWashAtWaste: integer; const aPumpIndices: TIntArray);
var
    x: integer;
    xWashAtWasteVol1, xWashAtWasteVol2: TDoubleArray;
    xWasteRack: TRack;
begin
    if gErrorManager.IsGlobalErr() then
        EXIT;

    // fahre zum WASTE-Rack
    xWasteRack := TLiquidHandlingLow.FindWasteRack(aUsedArm.PipDevice);
    TLiquidHandlingLow.MoveToWasteRack(aUsedArm, aTips);

    // entleere alle Dilutoren in den Waste
    TLiquidHandlingLow.ChangeValvePos1To4(aUsedArm.PipDevice, aTips, V_SYR_TIP);
    TLiquidHandlingLow.EmptyDilutors(aUsedArm.PipDevice, aTips, aPumpIndices);
    TLiquidHandlingLow.EmptyDilutors(aUsedArm.PipDevice, aTips,
        TLiquidChannels.GetDefaultChannel2Array(aPipDevice)); // für mehr als vier Dilutoren (SOPHAS)

    // Waschen in Waste-Rack
    if (aWashAtWaste > 0) then
    begin
        if (not gErrorManager.IsGlobalErr) then
        begin
            xWashAtWasteVol1 := aPipDevice.GetNullDoubleArray;
            xWashAtWasteVol2 := aPipDevice.GetNullDoubleArray;
            for x := 0 to aPipDevice.TipCount - 1 do
            begin
                xWashAtWasteVol1[x] := aVolume[x] * aWashAtWaste / 100;
                xWashAtWasteVol2[x] := aVolume2[x] * aWashAtWaste / 100;
            end;
            TLiquidHandlingLow.FlushSyringeOrPeripump(aPipDevice, aZMotion, xWasteRack, xWashAtWasteVol1,
                xWashAtWasteVol2, aUsePeripump, 0, aPumpIndices);
        end;
    end;
end;

class procedure TLiquidHandling.DoLiquidWash(aUsedArm: IArmDevice; const aVolume, aVolume2: TDoubleArray;
    aUsePeripump: boolean; aUseCorridor: boolean; aWashRetractSpeed: integer;
    aDryAfterWash: TDryAfterWashType; const aPumpIndices: TIntArray);
var
    x, xWashAtWaste: integer;
    xTips, xDryTips: TIPMAP;
    xPipDevice: IPipDevice;
    xZMotion: TZAxisPipMoveMotorMotionSystem;
    xZTravelMotion: TZAxisTravelMotionSystem;
    xUseWasteRack: boolean;
    xIniAccess: IWinLissyIniAccess;
begin
    xPipDevice := aUsedArm.PipDevice;

    xTips := 0;
    for x := 0 to xPipDevice.TipCount - 1 do
        if (aVolume[x] > 0) or (aVolume2[x] > 0) then
            TTipMapUtils.SelectTip(xTips, x);
    if (xTips = 0) then
        EXIT;

    // lese WashAtWaste aus Area ROBOT
    xIniAccess := gCommonDll.CreateRobotIni;
    xWashAtWaste := xIniAccess.ReadInteger('Precision', 'WashAtWaste');
    xUseWasteRack := (xWashAtWaste >= 0); // wenn WashAtWaste < 0, wird WASTE-Rack nicht angefahren

    xZMotion := TOperationFactory.CreateZAxisPipMoveMotorMotionSys(aUsedArm.MotionDevice);
    try
        // Waschroutine, die ein Vorwaschen mit Aceton ermöglicht
        if (aVolume[0] = 999) then
            TLiquidHandling.AcetoneWash(aUsedArm, xTips, xZMotion);

        // Empty dilutors and wash at WASTE rack
        if (xUseWasteRack) then
            WashAtWasteRack(aUsedArm, xPipDevice, xZMotion, xTips, aVolume, aVolume2, aUsePeripump,
                aUseCorridor, xWashAtWaste, aPumpIndices);

        // (Empty dilutors and) wash at WASH rack
        WashAtWashRack(aUsedArm, xPipDevice, xZMotion, xTips, aVolume, aVolume2, aUsePeripump, aUseCorridor,
            aWashRetractSpeed, not xUseWasteRack, aPumpIndices);

        // Move to Z-Travel (wahrscheinlich unnötig)
        xZTravelMotion := TOperationFactory.CreateZAxisTravelMotionSys(aUsedArm.MotionDevice);
        try
            xZTravelMotion.MoveToZTravelAllTips(0, 0);
        finally
            FreeAndNil(xZTravelMotion);
        end;

        // Trocknen im Dry-Rack
        if (aDryAfterWash = dawUseTipParameter) then
            xDryTips := xTips and xPipDevice.DryAfterFlushTips
        else if (aDryAfterWash = dawDoDryAfterWash) then
            xDryTips := xTips
        else
            xDryTips := 0;

        TLiquidHandlingLow.Dry(aUsedArm, xDryTips);

        // Tips-Status zurücksetzen
        gmResetWashFlags(aUsedArm.PipDevice, xTips);
    finally
        FreeAndNil(xZMotion);
    end;
end;

class procedure TLiquidHandling.AcetoneWash(aUsedArm: IArmDevice; iTips: TIPMAP;
    aZMotion: TZAxisPipMoveMotorMotionSystem);
var
    iDevN2: ISwitchDevice;
    xPipDevice: IPipDevice;
    xXYStep: TXYStep;
    xZStep: TPipStepZPos;
    xXYOp: TXYZTravelAxisPipMoveOperation;
    xZTravelMotion: TZAxisTravelMotionSystem;
    xQuenchRack: TRack;

    // Bewegt die Dilutoren 1 bis 4 an eine beliebige Position (ohne Ventilschalten)
    procedure LoadDilutors1To4(iTips: TIPMAP; iPosition: integer);
    var
        i: integer;
        xDilutorDev: IPipPumpDevice;
    begin
        if (not gErrorManager.IsGlobalErr) then
            for i := 0 to xPipDevice.TipCount - 1 do
                if (((iTips shr i) and 1) = 1) then
                begin
                    xDilutorDev := xPipDevice.Tips[i].GetPipPump(0);
                    if Assigned(xDilutorDev) then
                        xDilutorDev.Pick(iPosition, 0, true);
                end;
        if (not gErrorManager.IsGlobalErr) then
            xPipDevice.ExecutePipPumps(iTips);
    end;

begin
    xQuenchRack := TLayoutManager.Instance.CurrentLayout.FindRackByName('QUENCHING');
    if not Assigned(xQuenchRack) then
        EXIT;

    xPipDevice := aUsedArm.PipDevice;
    xXYStep := gmCalculatePipStep(aUsedArm, iTips, gmGetRackPosArrayForTips(xPipDevice, xQuenchRack), xZStep);
    if not Assigned(xXYStep) then
        EXIT;

    try
        xXYOp := TOperationFactory.CreateXYZTravelPipMoveOp(aUsedArm);
        try
            xXYOp.MoveXY(xXYStep, gmGetRackPosArrayForTips(aUsedArm.PipDevice, xQuenchRack), xZStep,
                [xyoUseCorridor]);
        finally
            FreeAndNil(xXYOp);
        end;

        if not gModules.Find_ByNameCut('N2CANNEL3', ISwitchDevice, iDevN2) then
            EXIT;

        iDevN2.SwitchOn(false);
        aZMotion.MoveZ(xZStep.SyrMap, xZStep.Z.ZMax, m_EXECUTE);

        // Ventile drehen
        TLiquidHandlingLow.ChangeValvePos1To4(xPipDevice, iTips, V_SYR_TIP);
        TLiquidHandlingLow.EmptyDilutors(xPipDevice, iTips,
            TLiquidChannels.GetDefaultChannel1Array(xPipDevice)); // Rest herausblasen!
        LoadDilutors1To4(iTips, 500); // Ansaugen
        TLiquidHandlingLow.EmptyDilutors(xPipDevice, iTips,
            TLiquidChannels.GetDefaultChannel1Array(xPipDevice)); // Rest herausblasen!
        LoadDilutors1To4(iTips, 500); // Ansaugen
        TLiquidHandlingLow.EmptyDilutors(xPipDevice, iTips,
            TLiquidChannels.GetDefaultChannel1Array(xPipDevice)); // Rest herausblasen!
        (iDevN2 as ISwitchDevice).SwitchOff(false);

        if (gErrorManager.IsGlobalErr) then
            EXIT;

        xZTravelMotion := TOperationFactory.CreateZAxisTravelMotionSys(aUsedArm.MotionDevice);
        try
            xZTravelMotion.MoveToZTravelAllTips(0, 0);
        finally
            FreeAndNil(xZTravelMotion);
        end;
    finally
        FreeAndNil(xXYStep);
    end;
end;

class function TLiquidHandling.CalcDilutorSteps(const aPipDevice: IPipDevice; const aUsedTips: integer;
    const aVolArray: TDoubleArray; const aPumpIndices: TIntArray; const aVolCorrCurve: TArray<string>)
    : TDoubleArray;
var
    x: integer;
    xVolCorrFactor: extended;
    xVolCorrDA: TVolcorrDataAdaptor;
    xPipPumpDevice: IPipPumpDevice;
begin
    result := aPipDevice.GetNullDoubleArray;
    xVolCorrDA := TVolcorrDataAdaptor.Create;
    try
        for x := 0 to aPipDevice.TipCount - 1 do
        begin
            if not TTipMapUtils.TipSelected(aUsedTips, x) then
                CONTINUE;

            if (x <= high(aVolCorrCurve)) and (aVolCorrCurve[x] <> '') then
            begin
                xVolCorrFactor := xVolCorrDA.GetFactorForVol(aVolCorrCurve[x], aPipDevice.Name, x + 1,
                    aVolArray[x]);
            end
            else
            begin
                xVolCorrFactor := 1;
            end;

            xPipPumpDevice := aPipDevice.Tips[x].GetPipPump(aPumpIndices[x]);
            if (xPipPumpDevice <> nil) then
            begin
                result[x] := MinValue([xPipPumpDevice.VolumeULToVolumeSteps(aVolArray[x] * xVolCorrFactor),
                    xPipPumpDevice.MaxMotorStep]);
            end;
        end;
    finally
        FreeAndNil(xVolCorrDA);
    end;
end;

class function TLiquidHandling.GetAspirateTypeFromPumpAspirateType(aPumpAspirateType: TPumpAspirateType)
    : TAspirateType;
begin
    case (aPumpAspirateType) of
        patAir:
            result := asptAir;
        patSystem:
            result := asptSystemLiquid;
        else
            result := asptLiquid;
    end;
end;

class function TLiquidHandling.GetValvePosFromPumpAspirateType(aPumpAspirateType: TPumpAspirateType)
    : TValvePos;
begin
    case (aPumpAspirateType) of
        patAir:
            result := V_SYR_TIP;
        patSystem:
            result := V_SYR_SYS;
        else
            result := V_SYR_TIP;
    end;
end;

class function TLiquidHandling.GetVolCorrCurves(aPipDevice: IPipDevice): TArray<string>;
var
    x: integer;
begin
    SetLength(result, aPipDevice.TipCount);
    for x := 0 to aPipDevice.TipCount - 1 do
    begin
        if not(aPipDevice.Tips[x].St_IsClear) then
            result[x] := aPipDevice.Tips[x].St_LhPtr.VolCorrCurve
        else
            result[x] := '';
    end;

end;

class function TLiquidHandling.GetLogDescriptionFromAspType(aAspType: TPumpAspirateType): string;
begin
    case (aAspType) of
        TPumpAspirateType.patSample:
            EXIT('Aspirate Sample');
        TPumpAspirateType.patSystem:
            EXIT('Aspirate Diluent');
        TPumpAspirateType.patAir:
            EXIT('Pick System Air');
        else
            EXIT('');
    end;
end;

class procedure TLiquidHandling.PumpAspirateActionExecute(aPipDevice: IPipDevice; aUsedTips: TIPMAP;
    aPumpIndex: integer; aVolumes, aSpeeds: TArray<double>; const aVolCorrCurve: string;
    aAspType: TPumpAspirateType; aZTracking: boolean; aZSubmerge_mm: extended;
    aTurnValves, aExecute: boolean);
var
    xUsedArm: IArmDevice;
    xZMotion: TZAxisPipMoveMotorMotionSystem;
    xPreviousArmPosition: TArmPositionInfo;
    xRP: TArray<TXRackPosition>;
    x: integer;
    xIsVol, xVolSteps: TDoubleArray;
    xTWay: TDoubleArray;
    xTrackingTips: TIPMAP;
    xValvePos: TValvePos;
    xCh1PumpIndices: TIntArray;
    xVolCorrCurves: TArray<string>;
    xLogCaption: string;
    xZMaxValues, xZTubeValues: TDoubleArray;
begin
    if (gErrorManager.IsGlobalErr) then
        EXIT;

    xLogCaption := GetLogDescriptionFromAspType(aAspType);
    gLogManager.Log(xLogCaption + ' - START', true);

    if not Assigned(aPipDevice) then
        raise Exception.Create(TLanguageString.
            Read('Pump Aspirate: Tips could not be found. (Tipname: {0} Tipmap {1})',
            'Pump Aspirate: Die Pipettiernadeln konnten nicht gefunden werden. (Tipname: {0} Tipmap {1})',
            ['', aUsedTips]));
    aPipDevice.SetUseTips('', aUsedTips, false);

    // Devices finden:
    xUsedArm := gModules.FindArmByPipDevice(aPipDevice);
    xZMotion := nil;
    xPreviousArmPosition := nil;
    if Assigned(xUsedArm) then
    begin
        xZMotion := TOperationFactory.CreateZAxisPipMoveMotorMotionSys(xUsedArm.MotionDevice);
        xPreviousArmPosition := TArmPositionInfo.GetArmPosition
            (xUsedArm.PositionMemory.OnGetPreviousPosition);
    end;

    xTWay := aPipDevice.GetNullDoubleArray();

    if (aZTracking) then
        xTrackingTips := aUsedTips
    else
        xTrackingTips := 0;

    if Assigned(xPreviousArmPosition) then
    begin
        xRP := xPreviousArmPosition.RP;
        xZMaxValues := xPreviousArmPosition.ZStep.Z.ZMax;
        xZTubeValues := xPreviousArmPosition.ZStep.Z.ZTube;
    end
    else
    begin
        xRP := TXRackPositionUtils.GetEmptyXRackPositions(aPipDevice.TipCount);
        xZMaxValues := aPipDevice.GetNullDoubleArray();
        xZTubeValues := aPipDevice.GetNullDoubleArray();
    end;

    // Execute pump aspirate
    xIsVol := aPipDevice.GetNullDoubleArray();
    if (aZTracking) then
    begin
        for x := 0 to aPipDevice.TipCount - 1 do
        begin
            if not TTipMapUtils.TipSelected(aUsedTips, x) then
                CONTINUE;
            if not Assigned(xRP[x].Rack) then
                raise Exception.Create(TLanguageString.
                    Read('Destination rack from last XYPOS Action not found in layout.',
                    'Ziel-Rack des letzen XYPOS-Befehls ist im Layout nicht vorhanden.'));

            if (xRP[x].Rack.RackId = '') then
                CONTINUE;

            xIsVol[x] := xRP[x].Rack.CalcVolumeOfRackPos(xRP[x].Pos);
            gLogManager.LogF('Substance Volume in RackID %s, Position %d --> %f uL found',
                [xRP[x].Rack.RackId, xRP[x].Pos, xIsVol[x]], false);
        end;

        xTWay := PickLiqCalculateTrackingWay(aPipDevice, xZMotion, aUsedTips,
            TXRackPositionUtils.ExtractRacksFromArray(xRP), aVolumes, xTrackingTips, xIsVol, xZMaxValues,
            xZTubeValues);
    end;

    xValvePos := GetValvePosFromPumpAspirateType(aAspType);
    xCh1PumpIndices := TLiquidChannels.GetPumpIndexArray(aPipDevice, aPumpIndex);

    xVolCorrCurves := TArrayUtils.GetDefinedStringArray(aVolCorrCurve, aPipDevice.TipCount);
    xVolSteps := CalcDilutorSteps(aPipDevice, aUsedTips, aVolumes, xCh1PumpIndices, xVolCorrCurves);
    TLiquidHandlingLow.PickLiquid(aPipDevice, xZMotion, aUsedTips, xCh1PumpIndices, aVolumes, aSpeeds,
        xValvePos, aPipDevice.GetNullDoubleArray(), aPipDevice.GetNullDoubleArray(), xValvePos, xVolSteps,
        aPipDevice.GetNullDoubleArray(), xTWay, aTurnValves, aExecute, true);

    for x := 0 to aPipDevice.TipCount - 1 do
    begin
        if (((aUsedTips shr x) and 1) <> 0) then
        begin
            if not Assigned(xRP[x].Rack) then
                CONTINUE; // Pump used without arm. No Rackdata available! No volume update

            TSubstanceLoading.Instance.StoreLiquidAsp(aPipDevice, xRP[x].Rack, xRP[x].Pos, x, aVolumes[x],
                GetAspirateTypeFromPumpAspirateType(aAspType), xCh1PumpIndices[x]); // Update Tube liquid list
            xIsVol[x] := xIsVol[x] - aVolumes[x]; // update volume for tracking and retract
        end;
    end;
    gLogManager.Log(xLogCaption + ' - END', true);
end;

class procedure TLiquidHandling.PumpDispenseActionExecute(aPipDevice: IPipDevice; aUsedTips: TIPMAP;
    aPumpIndex: integer; aVolumes, aSpeeds: TArray<double>; const aVolCorrCurve: string; aZTracking: boolean;
    aZSubmerge_mm: extended; aTurnValves, aExecute: boolean);
var
    xUsedArm: IArmDevice;
    xZMotion: TZAxisPipMoveMotorMotionSystem;
    xPreviousPos: TArmPositionInfo;
    x: integer;
    xRP: TArray<TXRackPosition>;
    xIsVol, xVolSteps: TDoubleArray;
    xTWay: TDoubleArray;
    xTrackingTips: TIPMAP;
    xValvePos: TValvePos;
    xCh1PumpIndices: TIntArray;
    xVolCorrCurves: TArray<string>;
    xZMaxValues, xZTubeValues: TDoubleArray;
begin
    if (gErrorManager.IsGlobalErr) then
        EXIT;

    gLogManager.Log('Dispense - START', true);

    if not Assigned(aPipDevice) then
        raise Exception.Create(TLanguageString.
            Read('Pump Dispense: Tips could not be found. (Tipname: {0} Tipmap {1})',
            'Pump Dispense: Die Pipettiernadeln konnten nicht gefunden werden. (Tipname: {0} Tipmap {1})',
            ['', aUsedTips]));
    aPipDevice.SetUseTips('', aUsedTips, false);

    // Devices finden:
    xUsedArm := gModules.FindArmByPipDevice(aPipDevice);
    xZMotion := nil;
    xPreviousPos := nil;
    if Assigned(xUsedArm) then
    begin
        xZMotion := TOperationFactory.CreateZAxisPipMoveMotorMotionSys(xUsedArm.MotionDevice);
        xPreviousPos := TArmPositionInfo.GetArmPosition(xUsedArm.PositionMemory.OnGetPreviousPosition);
    end;

    xTWay := aPipDevice.GetNullDoubleArray();

    if (aZTracking) then
        xTrackingTips := aUsedTips
    else
        xTrackingTips := 0;

    if Assigned(xPreviousPos) then
    begin
        xRP := xPreviousPos.RP;
        xZMaxValues := xPreviousPos.ZStep.Z.ZMax;
        xZTubeValues := xPreviousPos.ZStep.Z.ZTube;
    end
    else
    begin
        xRP := TXRackPositionUtils.GetEmptyXRackPositions(aPipDevice.TipCount);
        xZMaxValues := aPipDevice.GetNullDoubleArray();
        xZTubeValues := aPipDevice.GetNullDoubleArray();
    end;

    // Execute pump Dispense
    xIsVol := aPipDevice.GetNullDoubleArray();
    if (aZTracking) then
    begin
        for x := 0 to aPipDevice.TipCount - 1 do
        begin
            if not TTipMapUtils.TipSelected(aUsedTips, x) then
                CONTINUE;
            if not Assigned(xRP[x].Rack) then
                raise Exception.Create(TLanguageString.
                    Read('Destination rack from last XYPOS Action not found in layout.',
                    'Ziel-Rack des letzen XYPOS-Befehls ist im Layout nicht vorhanden.'));

            if (xRP[x].Rack.RackId = '') then
                CONTINUE;

            xIsVol[x] := xRP[x].Rack.CalcVolumeOfRackPos(xRP[x].Pos);
            gLogManager.LogF('Substance Volume in RackID %s, Position %d --> %f uL found',
                [xRP[x].Rack.RackId, xRP[x].Pos, xIsVol[x]], false);
        end;

        xTWay := DispLiqCalculateTrackingWay(aPipDevice, xZMotion, aUsedTips,
            TXRackPositionUtils.ExtractRacksFromArray(xRP), aVolumes, xTrackingTips, xIsVol, xZMaxValues,
            xZTubeValues);
    end;

    // wenn das Ventil geschaltet wird, ist nur Syr-Tip möglich!
    xValvePos := V_SYR_TIP;

    xCh1PumpIndices := TLiquidChannels.GetPumpIndexArray(aPipDevice, aPumpIndex);

    xVolCorrCurves := TArrayUtils.GetDefinedStringArray(aVolCorrCurve, aPipDevice.TipCount);
    xVolSteps := CalcDilutorSteps(aPipDevice, aUsedTips, aVolumes, xCh1PumpIndices, xVolCorrCurves);

    TLiquidHandlingLow.DispLiquid(aPipDevice, xZMotion, aUsedTips, xCh1PumpIndices, aVolumes, aSpeeds,
        xValvePos, aPipDevice.GetNullDoubleArray(), aPipDevice.GetNullDoubleArray(), xValvePos, xVolSteps,
        aPipDevice.GetNullDoubleArray(), xTWay, TXRackPositionUtils.ExtractRacksFromArray(xRP), aTurnValves,
        aExecute, true);

    for x := 0 to aPipDevice.TipCount - 1 do
    begin
        if (((aUsedTips shr x) and 1) <> 0) then
        begin
            if not Assigned(xRP[x].Rack) then
                CONTINUE; // Pump used without arm. No Rackdata available! No volume update

            TSubstanceLoading.Instance.StoreLiquidDsp(aPipDevice, xRP[x].Rack, xRP[x].Pos, x, aVolumes[x],
                dsptLiquid, xCh1PumpIndices[x]);
            // Update Tube liquid list
            xIsVol[x] := xIsVol[x] - aVolumes[x]; // update volume for tracking and retract
        end;
    end;
    gLogManager.Log('Dispense - END', true);
end;

class procedure TLiquidHandling.PeriPumpDispenseActionExecute(aPipDevice: IPipDevice; aUsedTips: TIPMAP;
    aVolumes: TArray<double>);
var
    xUsedArm: IArmDevice;
    xPreviousPos: TArmPositionInfo;
    xRP: TArray<TXRackPosition>;
begin
    if (gErrorManager.IsGlobalErr) then
        EXIT;

    gLogManager.Log('Dispense PeriPump - START', true);

    if not Assigned(aPipDevice) then
        raise Exception.Create(TLanguageString.
            Read('Pump Dispense: Tips could not be found. (Tipname: {0} Tipmap {1})',
            'Pump Dispense: Die Pipettiernadeln konnten nicht gefunden werden. (Tipname: {0} Tipmap {1})',
            ['', aUsedTips]));
    aPipDevice.SetUseTips('', aUsedTips, false);

    // Devices finden:
    xUsedArm := gModules.FindArmByPipDevice(aPipDevice);
    xPreviousPos := nil;
    if Assigned(xUsedArm) then
    begin
        xPreviousPos := TArmPositionInfo.GetArmPosition(xUsedArm.PositionMemory.OnGetPreviousPosition);
    end;

    if Assigned(xPreviousPos) then
        xRP := xPreviousPos.RP
    else
        xRP := TXRackPositionUtils.GetEmptyXRackPositions(aPipDevice.TipCount);

    TLiquidHandlingLow.DispSeveralPeriPumps(aPipDevice, xRP, aVolumes);

    gLogManager.Log('Dispense PeriPump - END', true);
end;

class procedure TLiquidHandling.TurnValveActionExecute(aPipDevice: IPipDevice; aUsedTips: TIPMAP;
    aVInx: TValvePos; aPumpIndex: integer; aExecute: boolean);
var
    xCh1PumpIndices: TIntArray;
begin
    xCh1PumpIndices := TLiquidChannels.GetPumpIndexArray(aPipDevice, aPumpIndex);
    TLiquidHandlingLow.TurnValves(aPipDevice, aUsedTips, aVInx, xCh1PumpIndices, aExecute);
end;


end.
