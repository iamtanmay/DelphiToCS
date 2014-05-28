{ --------------------------------------------------------------------------------------------------
  Ebene 1b (Sam-Globals)
  --------------------------------------------------------------------------------------------------
  Rack - Objekt
  --------------------------------------------------------------------------------------------------
  Datum    op  function/procedure      Änderung / Neuerung
  -------- --  -------------------     -------------------------------------------------------------
  24.11.97 dl  TRack                   public property RackStructure
  04.12.97 dl  GetRackData             ZTube implementiert / Z-Höhe zum Tube Boden (absolut)
  19.01.97 dl  FRackStruct //          verschoben von protected nach public für setup anpassung
  11.02.97 dl  Create                  Global Offset Initialisierung für Redi gelöscht
  (GlobalXOffset  := 0;)
  13.05.98 mo                          Painttubes Farben erweitert
  30.05.98 mo                          FSlotgroups nach public verschoben
  ------------------------------------------------------------------------------------
  16.07.98 wl  GetTubeData             Versetzte Racks werden mit PosY_Offset_mm richtig angesteuert
  SetTubeData             PosX/Y_Offset_mm aus Berechnung entfernt
  == Version 3.3.0 (Branch)
  ------------------------------------------------------------------------------------
  07.07.98 wl  GetSlotGroupData        CarrierType wird bestimmt (erst in KV 3.4.0)
  16.07.98 wl  GetTubeData/SetTubeData Änderungen von KV 3.3.0 (Branch) nachgetragen
  17.07.98 wl  GetTubeData             X- und Y-Position werden anders berechnet, wenn ein AddOffset vorliegt
  Paint                   bei AddOffset unterschiedliche Spaltenbreiten (wirkt sich noch nicht aus)
  31.07.98 wl  GetSlotGroupData        CarrierType entfernt (verursachte Chaos beim GetPlate)
  01.09.98 wl  TRack.SetSlot           Wenn Rack im RunLayout nicht gefunden wird, gibt es keine Exception
  15.09.98 wl  TRack.Paint             Änderungen rückgängig gemacht (entsprechen KV 3.3.0)
  16.09.98 wl  TRack.SetTubeData       BorderWidth eingebaut
  02.11.98 wl  TRack.SetCarrierTypeName  um 'H_ZPut_mm' erweitert
  TRack.GetSlotGroupData    um 'HPutOfs' bzw. 'H_ZPut_mm' erweitert
  18.11.98 wl  TRackGrid.DrawCell      Farbe 'S' unterscheidet sich jetzt von 'Y' -> orange
  16.12.98 wl  TRack.CreateHint        beschreibt den Hint mit Name, ID, Typ
  TRack.SetRName          ruft CreateHint auf
  TRack.SetRackID         ruft CreateHint auf (bei jedem Ändern der RackID)
  FSlotStruct,FRackStruct von public zurück nach protected
  16.12.98 wl                          TRackGrid durch TStringGrid ersetzt
  MatrixDrawCell          ehemals TRackGrid.DrawCell
  TRackGrid.FBorderX,-Y -> FMatrixBorderX,-Y
  14.01.99 wl  SetRName,SetCarrier     jetzt als überschreibbare Methoden (virtual)
  SetSlot                 Änderung vom 01.09.98 rückgängig gemacht
  28.01.99 wl  CreateHint              Hint = Matrix.Hint
  Create                  ShowHint ist immer gesetzt
  03.02.99 wl  Create                  Matrix.Font.Color = clSilver (Schrift nicht sichtbar)
  04.02.99 wl  SetTubeData             WBBorders.Left,.Top statt BorderWidthX,Y
  05.08.99 wl  MatrixDrawCell          zur Delphi 4-Kompatibilität Klammern anders gesetzt
  24.08.99 wl  SetCarrierTypeName      SlotZ_mm entfernt
  SetTubeData             PipettXZero,PipettYZero beim Rack zeichnen entfernt
  14.09.99 wl  SetTypeName             erweitert um:  MOffsetZ_mm, MOffsetUpX_mm, MOffsetUpY_mm
  GetRackData             jetzt mit MOffset-Parametern
  22.09.99 mo  GetTubeData             td.NumLiquid := gmGetNoOfTubeAccess  (StepZähler für Tube )
  24.09.99 wl                          uses dbTools entfernt
  27.09.99 wl  SetTypeName             erweitert: Shift_Radius_mm, Shift_NoOfSteps
  01.12.99 wl                          geänderte Farben
  14.12.99 mo  GetRackData             GlobalZFactor ( für Redi am Handler ) in Z-Berechnung eingefügt
  17.01.00 wl  ChangeRackID            Funktion zum Ändern der RackID und der zugehörigen POSINFO-Einträge
  28.01.00 wl  ChangeRackID            Umschreiben der Posinfo ausgeklammert ( Problem Bayer Insektizide )
  11.02.00 wl  ChangeRackID            Umschreiben wenn 24h-Mode oder gChangePosinfo und alte ID <>0
  05.05.00 wl  ChangeZMax              nötig für Sophas-Threads (Z-Höhe + ResinHeight)
  16.05.00 wl  SetTubeData             kann mit oder ohne Rack-Zeichnen aufgerufen werden (für Layouter)
  25.05.00 wl  CheckTubePos            --> SamCmd (uses Samintf entfernt)
  27.07.00 wl  SetTypeName             Rackdimensionen werden nicht richtig gelesen -> nicht geändert, nur kommentiert
  19.01.01 mo  TRack.MatrixDrawCell    Farbe für Destpositionen einstellbar
  20.02.01 mo  TRack.GetTubeData       Paint in try...except block wegen Windows2000
  28.09.01 tbh TPRRack                 neu: Objekt zum Verwalten von Pickup Redi Tips
  28.09.01 tbh TPRRackManager          neu: Objekt zum Verwalten aller im Layout existierenden TPRRack-Objekten
  16.10.01 mo                          TN1067 Merge mit SIAS Änderungen
  17.10.01 tbh TPRRack/TPRRackManager  TN1050 Objekte nach PrRackDlg.pas verschoben
  23.01.02 tbh TRack.SetTubeData       TN1135 jetzt public
  26.03.02 tbh TRack.GetTubeData       TN1039 Positionen werden nicht mehr gemalt (wegen Windows2000)
  11.09.02 mo                          TN1283 Merge mit SIAS
  26.09.02 wl                          TN1283 Merge mit SIAS
  10.10.02 wl                          TN1293.2 Ini Access - uses geändert
  31.10.02 mo GetTubeData/SetTubeData  TN1322 Rundungsfehler beim Berechnen der Tube Positionen beseitigt.
  15.11.02 mo TRack                    TN1242 neu: properties ReactBlockNo & IsReactBlock
  15.11.02 mo TRack.SetRName           TN1242 Hint enthält ReactionsBlock Nummer
  18.11.02 mo TRack.SetRName           TN1242 Farbzuweisung entfernt
  05.12.02 wl  GetRackData             TN1345 Rackdata-Membernamen geändert
  05.12.02 wl  GetSlotGroupData        TN1345 Slotgroupdata-Membernamen geändert
  10.12.02 wl                          TN1345 TRack wird von TMinimalRack abgeleitet, HRACK fällt weg
  10.12.02 wl  Painte,GetRackData,..   TN1345  ifdef sias  wird durch AppSettings.IsSias ersetzt
  12.12.02 wl  HRACK,HSLOTGRP          TN1345 --> RoboticInterfaceZP01/ZP02
  20.12.02 wl                          TN1293.5 uses und WinlissyIniAccess geändert
  22.01.03 tbh TRack                   TN1172 neue Property PosShifted
  22.01.03 tbh TRack.Create            TN1172 neue Property PosShifted wird initialisiert
  18.02.03 wl  GetRackData             TN1345 ZTube wird auch für Sias genutzt!!
  04.06.03 wl                          TN1485.4 TAppSettings.IsSias ersetzt die ifdefs
  17.06.03 tbh FColor                  TN1503.1 entfernt stattdessen gilt Color-Property des TPanels
  17.06.03 tbh TRack.Create            TN1503.1 Standardfarbe ist jetzt Konstante (INT_RACK_COLOR)
  17.06.03 tbh TRack.SetRackColor      TN1503.1 neu: setzt Farbe einheitlich für alle Rack-Teile
  17.06.03 tbh TRack.SetTypeName       TN1501/TN1503.1/TN1502  RackStruct liest neue Daten ein
  17.06.03 tbh TRack.SetTypeName       TN1503.1 setzt Farbe gemäß Daten
  17.06.03 tbh TRack.GetTubeZPosition  TN1501.4 liefert die TubeZ-Höhe für bestimmte Position
  17.06.03 tbh TRack.SetRackColor      TN1503.1 jetzt als public (wenn aColor=0 wird Farbe zurückgesetzt)
  25.06.03 wl  CheckAndDisplay         TN1501.5 CapPark- oder Decapper-Racks müssen einen definierten Captyp haben
  26.06.03 wl  FillRackWithCaps        TN1501.8 Schreibt für das komplette Rack Deckel-Einträge (und evtl. Random-Tube-ID's)
  26.06.03 wl  FindRandomTubeBC        TN1501.6 Sucht alle Random-Barcodes für das Rack
  27.06.03 wl  IsDecapperRack          TN1501.7 Ein Decapper-Rack heißt 'DECAPPER ..
  17.07.03 wl  IsDecapperRack          TN1501.7 weg: Decapper-Name wird in Devices bestimmt
  17.07.03 wl  IsCapParkRack           TN1501.7 Ein Decapper-Rack heißt 'CAPPARK ..
  17.07.03 wl  IsCapWasteRack          TN1501.7 Ein Decapper-Rack heißt 'CAPWASTE ..
  24.07.03 tbh GetTubeZPosition        TN1501.4 If-Statements korrigiert
  02.09.03 wl  Destroy                 TN1559   Matrix wird mit freigegeben
  02.09.03 wl  SetSlotWithoutDBChange  TN1559   aus ObjSampl hierher
  02.09.03 wl  FCarrier                TN1559   neu: das Rack besitzt eine Referenz auf den Carrier!!
  02.09.03 wl  SetCarrier,SetCarrierTypeName  TN1559   kann alles raus - durch FCarrier
  02.09.03 wl  SetUp, SetUp_NoRead     TN1559   ersetzt SetRName() - .._NoRead ohne Lesen der Layout-Daten (für Layouter)
  12.09.03 wl  GetSlotData             TN1581   wurde gar nicht benutzt -> raus
  12.09.03 wl  SetUp,SetSlot           TN1581.7 neu: Rotation wird aus Layout.DB Feld 'CARR_X_STEPS' (!!!) gelesen
  12.09.03 wl  ResetSlot               TN1581.7 aus ObjSampl hierher
  12.09.03 wl  GetRackData,SetTubeData TN1581.8 ActRow,ActCol,ActFloor werden mit neuen Konvertiermethoden von TCarrier ermittelt
  17.09.03 wl  GetSlotGroupData        TN1526   ungenutzte Parameter aus TSlotGroupStruct entfernt
  17.09.03 wl  GetRackData             TN1526   Plattform-spezifische Parameter werden allgemein
  17.09.03 wl  GetRackData             TN1526.1 Vereinfachte Berechnung von rd.hXTake bei ZP02
  18.09.03 wl  ChangeRackID            TN1597   g24hMode jetzt als integer (statt boolean)
  29.09.03 wl  SetTypeName             TN1526   X_mm, Y_mm, Z_mm werden als float eingelesen (mit Kommastellen)
  29.09.03 wl  GetTubeZPosition_mm     TN1501.4 neue Berechnung für schräge Racks
  07.10.03 wl  SetTubeData             TN1581   Methode jetzt virtuell (für Layouter)
  07.10.03 wl  Create                  TN1581   Rack ist beim Start nicht visible, erstbei SetTubeData
  23.10.03 wl  GetSlotGroupData        TN1631   hXPreStartOfs = H_XPreStart_mm in Steps
  14.11.03 wl  SetTypeName             TN1664   Lesen von H_Direction eingefügt
  14.11.03 wl  SetSlotWithoutDBChange  TN1664   FRotation wird aus "Handler Direction" (Rack) und "Slot Rotation" (Carrier) berechnet
  14.11.03 wl  SetSlotWithoutDBChange  TN1664   Bei Umgreifposition wird die übergebene Rotation verwendet
  18.11.03 wl  GetSlotGroupData        TN1667   hRStartOfs = H_RStart_degree in Steps
  19.11.03 mo  GetSlotGroupData        TN1676   hXTake wird als Abs() wert berechnet
  10.12.03 wl  GetSlotGroupData        TN1672   neu: mit SlotNr (und ohne HRReTakeOfs)
  17.02.04 pk  Paint                   TN1749   Renamed DoPaint.  Calculations in paint caused another Paint to be called
  17.02.04 pk  ChangeVisible           TN1749   By default, controls start as invisible and are set Visible later to reduce flickering
  17.02.04 pk  SetTubeData             TN1749   Visibility of rack no longer changed in this function. See SetCarrierTypeName when aDoPaint=true
  19.02.04 mo  SetTypeName             TN1754   Neue Felder TubeGet.. TubePut..
  12.03.04 wl  GetRackData             TN1812   bereinigt: GlobalZ...Offset nicht benötigt -> enfernt
  12.03.04 wl  GetTubeCount            TN1812   neu: wird oft benötigt
  05.04.04 wl  PaintTubePos            TN1788   neu: ersetzt in vielen Fällen GetRackData (wenn nur die Tubes bunt werden sollen)
  05.04.04 wl  GetSlotGroupData        TN1788   --> DevicesGrpArm
  05.04.04 wl  GetTubeZPosition_mm     TN1788   --> TubeHandlingLow
  05.04.04 wl  GetTubeData_NoPaintTubesTN1788   GlobalX/YOffset werden als Parameter übergeben
  05.04.04 wl  GetRackData_WithOffset  TN1788   GlobalZOffset wird als Parameter übergeben
  05.04.04 wl  GetRackData_WithOffset  TN1788   entfernt: NumTubes,HVOpen,HVClose; sind jetzt SLOTGROUPDATA: hxTake,hyTake,hZTake,hRTake
  06.04.04 pk  GetRackData_WithOffset  TN1848   Rack Z Values were incorrectly calculated.  Bug corrected
  19.04.04 wl                          TN1788   TRack direkt von TPanel abgeleitet (nicht meht TMinimalRack)
  27.04.04 wl  GetTubeData_NoPaintTubes TN1788  Subtraktion von (Sam.YOffset * (Sam.ntips-1) div 2) -> DevicesArms,TubeHandlingLow
  05.05.04 wl  FTubeData               TN1788   Positionen werden nicht mehr mit SetTubeData festgelegt, sondern erst bei GetTubeData ermittelt
  05.05.04 wl  CreateRack/FirstTubePosition  TN1788 allgemeine Berechnung der Position aus SetTubeData/GetRackData entnommen
  05.05.04 wl  GetRackData             TN1788   benutzt GetFirstTubePosition
  05.05.04 wl  CreateTubePosition      TN1788   ersetzt GetTubeData (mit XY-TubeOffset-Berechnung)
  05.05.04 wl  CreateTubePosition      TN1788   enthält jetzt Slope-Berechnung aus ehm. GetTubeZPosition_mm -> könnte jetzt auch zum Pipettieren verwendet werden, wird aber noch nicht
  10.05.04 wl  DrawRackPosition        TN1788   entspricht SetTubeData
  10.05.04 wl  WellSurface_mm2         TN1788   Teil der Berechnung aus gmGetVol (SamHigh)
  10.05.04 wl  CreateHint              TN1908   spezieller Hint für Racks ohne Namen (nicht im Layout)
  10.05.04 wl  CreateHint              TN1908   zeigt jetzt auch das Volumen eines Wells an
  10.05.04 wl  SetTypeName             TN1908   ruft CreateHint auf
  25.05.04 pk  SetSlotWithoutDBChange  TN1865   Call OnCarrierChange if carrier has changed
  25.05.04 pk  Create                  TN1865   Set the parent to the owner and force handle allocations
  25.05.04 pk  OnCarrierChange         TN1865   Updates visibility if the rack's old or new carrier was or is a stacker
  04.06.04 pk  TRack.Create            TN1865   Set visible to false before assigning parent
  08.06.04 pk  SyncSetSlotWithoutDB... TN1974.0 New: Synchronized call to SetSlotWithoutDBChange
  08.06.04 pk  ResetSlot               TN1974.0 uses SyncSetSlotWithoutDBChange
  17.06.04 pk  IsBigReactionBlock      TN1993   New: Checks distance between first and last y
  24.06.04 wl                          TN2007   uses Variants (nur Delphi 6 und 7)
  28.06.04 pk  SyncSetRackColor        TN2009.5 New
  01.07.04 wl  GetRackData_WithOffset  TN1963   XY-Move-Offset -> TPipStep
  30.07.04 pk  RackArray               TN2068   Dynamic
  10.08.04 wl                          TN2088   alle Änderungen bzl. TN2061.1, TN2061.2 rückgängig gemacht (Sorry Payman!)
  11.09.04 wl  GetRackData_WithOffset  TN2123   TPipZPosition: neuer Name für RACKDATA
  13.09.04 wl  CreateTubePosition      TN2123   neu: Berechnung von HighestZPos zur Z-Travel-Berechnung bei schrägen Racks
  27.10.04 wl  GetHRTake_degree        TN2071   Unterscheidung zwischen FRackRotation und HRTake (für Rack-Transport)
  27.10.04 wl  SetRoatation            TN2071   ruft FCarrier.DefineSlotRotation auf, bestimmt nur FRackRotation!
  27.10.04 wl  SetTypeName             TN2071   statt H_Rotation wird H_RTake_degree eingelesen
  27.10.04 wl  Calc_XY_Pos,Calc_Pos_XY TN2071   90° und 270° waren vertauscht - korrigiert
  25.01.05 tbh SetTypeName             TN2293   Auslesen um Feld 'BlockBalanceDoor' erweitert
  27.04.05 pk  ChangeRackID            TN2398   use TPosinfoDataAdaptor
  07.09.05 wl  Create                  TN2558.9 in Delphi 7 muß ParentBackground gesetzt sein
  16.09.05 wl  CreateRackNameAndIDString  TN2574  aus CreateHint ausgelagert
  14.11.05 wl  FOriginalTypeName       TN2775   merkt sich den Type-Namen, der bei SetUp_NoRead gesetzt wird
  14.11.05 wl  ResetTypeName           TN2775   ändert den Racktyp zurück zu FOriginalTypeName
  16.11.05 wl  GetRackData_WithOffset  TN2775   entfernt, wird durch TCustomArmDevice.GetRackZTravel ersetzt
  06.02.06 wl  GetSlotStruct           TN2928   zeigt auch Rotation richtig an
  22.03.06 wl  MatrixDrawCell          TN2989.1 Farbspektrum erweitert
  03.04.06 pk  SetTypeName             TN3009   TubetGetOpen, etc changed from steps to mm
  24.08.06 wl  CreateRackPosition      TN3269    aUseDifferentLevelHeights als Parameter
  07.09.06 pk  ChangeRackID            TN3292   reference to posinfodataadaptor instance removed
  15.09.06 pk  H_VIsUndefined          TN3073   set H_VIsUndefined to true if either VClose or VOpen is null
  07.11.06 wl  SetTypeName             TN3398   neues Feld StackHeightZ_mm wird jetzt auch gelesen
  29.11.06 wl  RackRotation            TN3408   new property for fRackRotation
  12.12.06 wl  CreateTubePosition_Relative  TN3467   entspricht CreateTubePosition (nicht Rotationsabhängig)
  12.12.06 wl  CreateTubePos           TN3467   benutzt CreateTubePosition_Relative und dreht die berechneten Positionen bei Rotation
  12.12.06 wl  DoPaint                 TN3468   Berechnung der dargestellten Positionen jetzt nah an der Wirklichkeit
  12.12.06 wl  DoPaint                 TN3468   Prüfungen stellen sicher, dass Matrix nie außerhalb des Racks liegt
  13.12.06 wl  CreateTubePos           TN3467.1 Wert für oHighestZPos_mm ist jetzt wieder absolut
  30.08.07 pk                          TN3840.1 uses TLayoutDataAdaptor instead of dmRack
  09.11.07 pk                          TN3924    Steps changed to mm
  07.01.08 pk  CreateRotationCorner    TN3971    use AddZHeight to add ZHeights
  20.06.08 pk                          TN4139   inherits from TLayoutElement not TPanel. Graphics moved to fGraphics
  27.06.08 pk  SetTypeName             TN4139   Changed to SetType
  27.06.08 pk  OriginalTypeName        TN4139   New property
  02.07.08 pk                          TN4139   Rack/Tube Position now based on matrix math
  07.07.08 pk                          TN4139   various changes
  09.07.08 pk  Destroy                 TN4139   call fWells.Free
  16.07.08 pk  StackHeightZ            TN4139   new property
  31.07.08 pk  ChangeType              TN4193   New
  29.10.08 wl  TRackStruct             TN4290   von AppTypes hierher
  29.10.08 wl  TRackStruct             TN4290   neu: TubePutOffset_mm
  13.01.09 wl  FindRandomTubeBC        TN4312    TRackPositions ist jetzt dynamisches Array
  11.03.09 pk  CreateRackTypeView      TN4457   New
  05.08.09 ts  CalcWellPosition        TN4689   xZSlopeDiff Berechnung umgedreht, da Koordinaten von unten nach oben angegeben werden
  05.08.09 wl  PaintTubePos            TN4706   durch den Aufruf SceneChanged wird das Rack anschließend neu gezeichnet
  10.08.09 wl                          TN4702   Strings werden jetzt direkt geladen
  04.11.09 pk                          TN4843   Replace TObjectList, TStringList, TStrings with Generics Lists and TStringArray
  14.12.09 pk  CreateRackWell          TN4939   set Rack Property
  13.04.10 wl                          TN5044   uses geändert
  21.07.10 pk  PaintTubePositions      TN5066   New
  23.07.10 wl  TRackStruct             TN5205   Size-Werte heißen jetzt auch SizeX,SizeY,SizeZ
  23.07.10 wl  SetType                 TN5205   setzt auch die Size-Werte eines unsichtbaren Carriers
  28.10.10 wl  SetHighligt             TN5312   true: Rack wird in Highlightfarbe gefärbt, false: Rack bekommt Ursprungsfarbe
  28.10.10 wl  ColorCodeToColor        TN5315   gTubeDispColor wird wieder benutzt
  24.02.11 wl                          TN5431   3 neue Felder: WellsAroundCenter (boolean), CenterX_mm, CenterY_mm
  24.02.11 wl  CalcWellAroundCenter    TN5431   Berechnung der Positionen in Kreisform, wenn WellsAroundCenter = true
  08.03.11 wl  CalcWellAroundCenter    TN5431   Bugfix
  05.05.11 ts                          TN5552   new: TubeGripOffset_mm
  05.09.11 ts  SetWellsVisible         TN5683   new: DoNotPaintTubes
  19.10.11 wl  PaintTubePositions      TN5723   geht jetzt auch direkt mit TColor
  20.09.11 wl  SetPosVolumes           TN5723   übergibt Volumen-Array an RackWell
  20.09.11 wl  HighlightPosition       TN5723   neu
  28.10.11 wl  ColorCodeToColor        TN5725   alte Color-Codes entfernt
  28.10.11 wl  PaintTubePositions      TN5725   Parameter ist jetzt nur noch TRackWellDisplayType
  03.11.11 wl  DoCreateRackWell        TN5725   jetzt mit WellNr
  03.11.11 wl  CalcVolumeOfRackPos     TN5725   von Layout hierher
  17.11.11 wl  GetWellFromWellNr,Wells TN5725   jetzt public
  17.11.11 ts                          TN5702   new: DiscretePositions
  02.02.11 wl  PaintTubePositions      TN5791   --> RackTypes.TXRackPositionUtils
  10.09.12 ts                          TN5977   new: RackMoveZTravelOffset
  28.03.13 wl  WellIsStoragePosition   TN6120   neu
  10.06.13 pp  GetDiscretePosition     TN6167   Anpassungen für DiscreteRacks
  27.11.13 wl  CreateRackNameAndIDString  TN6313   Hint Text für MultiTip Racks
  -------------------------------------------------------------------------------------------------- }

unit Rack;


interface


uses
    Classes,
    Generics.Collections,
    SysUtils,
    GeneralTypes,
    AppTypes,
    CommonTypes,
    Graphics,
    ColorUtilities,
    GeometricClasses,
    ThreadUtils,
    RackWell,
    RackGraphics,
    MatrixMath,
    CoordSystemMath,
    LayoutElement,
    LayoutElementCallbackTypes,
    LayoutElementGraphicsInfo,
    RackDataAdaptor,
    SubstanceSetDataAdaptor,
    DiscreteRackDataAdaptor;

const
    cTubeTypeMultiTipTube = 0;
    cTubeTypeSingle = 1;

type
    // diesen Record sollte man auflösen und die einzelnen Werte zu properties machen
    TRackStruct = record
        name: string;
        TYP: integer;
        SizeX_mm: double;
        SizeY_mm: double;
        SizeZ_mm: double;
        Rows: integer;
        Cols: integer;
        TubeTyp: integer;
        TubeX_mm: double;
        TubeY_mm: double;
        TubeY2_mm: double;
        TubeZ_mm: double;
        TubeGetOpen_mm, TubeGetClose_mm, TubePutOpen_mm, TubePutOffset_mm, TubeGripOffset_mm: TPosMM;
        BorderX_mm: double;
        BorderY_mm: double;
        PosX_First_mm: double;
        PosY_First_mm: double;
        PosX_Last_mm: double;
        PosY_Last_mm: double;
        PosX_Offset_mm: double;
        PosY_Offset_mm: double;
        AddOffsetX_AfterPos: integer;
        AddOffsetX_mm: double;
        AddOffsetY_AfterPos: integer;
        AddOffsetY_mm: double;
        ZTravel_mm: double;
        ZScan_mm: double;
        ZDisp_mm: double;
        ZMax_mm: double;
        H_XTake_mm: TPosMM;
        H_YTake_mm: TPosMM;
        H_ZTake_mm: TPosMM;
        H_RTake_degree: TPosMM;
        H_VOpen_mm: TPosMM;
        H_VClose_mm: TPosMM;
        H_VIsUndefined: boolean;
        MOffsetZ_mm: double;
        MOffsetUpX_mm: double;
        MOffsetUpY_mm: double;
        Shift_Radius_mm: double;
        Shift_NoOfSteps: integer;
        Flags_ZP02: integer;
        CapType: string;
        CapDiameter: double;
        Cap_GripZ_mm: double;
        Color: integer;
        SlopeType: integer;
        Z_LastPos_mm: double;
        BlockBalanceDoor: boolean;
        StackHeigthZ_mm: double;
        WellsAroundCenter: boolean;
        CenterX_mm: double;
        CenterY_mm: double;
        DoNotPaintTubes: boolean;
        DiscretePositions: boolean;
        RackMoveZTravelOffset: TPosMM;
    end;

    TVCLType = (vclSetSlotWithoutDBChange, vclSetRackColor);

    TRack = class(TLayoutElement)
    private
        FPosShifted: boolean;
        FRackRotation: TRotationValue;
        fParentMakeHintTextCallback: TParentMakeHintTextCallback;
        fRackSizeHasChanged: TLayoutElementChangeSizeEvent;
        fHighlight: boolean;
        // function GetSlotStruct: TSlotStruct;
        function GetTubeCount: integer;
        function GetWellSurface_mm2: TPosMM;
        function GetWellMaxVolume_mm3: TPosMM;
        function GetHRTake_degree: TPosMM;
        function GetTubeX_mm(): TPosMM;
        function GetTubeY_mm(): TPosMM;
        procedure InitWells;
        function GetGraphics: TRackGraphics;
        function CalcWellPosition(aRow, aCol: integer): TPoint4d;
        function CalcWellAroundCenter(aRow, aCol: integer): TPoint4d;
        function GetDiscretePosition(aRow, aCol: integer): TPoint4d;
        procedure SetUpWell(aWell: TRackWell; aWellIndex: integer);
        function DoRackMakeHintText(aSender: TObject): string;
        function GetRackWellShape(): TRackWellShape;
        function GetSizeZ: TPosMM;
        procedure SetSizeZ(const aValue: TPosMM);
        function FindHighestWell: TRackWell;
        function GetStackHeightZ: TPosMM;
        procedure SetWellsVisible(aVisible: boolean);
        function GetSizeX: TPosMM;
        function GetSizeY: TPosMM;
        procedure SetHighlight(const aValue: boolean);
        procedure SetRackColor(aColor: integer);
        function MakeWellPoint(aX, aY, aZ: double): TPoint4d;
        // synchronized
        procedure SyncSetRackColor(aColor: integer);
        function VCL(const aArgs: TMessageArg): TMessageResult;
    protected
        FTypeName: string;
        FOriginalTypeName: string; // defined only when SetUp_NoRead is called
        FRackID: string;
        fStruct: TRackStruct;
        fSlot: TObject;
        fIsLinked: boolean;
        fWells: TObjectList<TRackWell>;
        procedure SetRackID(const aRackID: string);
        procedure SetRackRotation(aRotationDegree: TRotationValue);
        procedure UpdateGraphicsInfo();
        procedure DoInitGraphics; override;
        procedure SetVisible(aVisible: boolean); override;

        function MakeHintText: string; override;
        procedure DoAfterCreateRackWell(aRackWell: TRackWell); virtual;
        function DoCreateRackWell(aWellNr: integer): TRackWell; virtual;
        function CreateRackWell(aWellNr: integer): TRackWell;
    public
        // constructor
        constructor Create();
        destructor Destroy; override;

        // public methods
        procedure DrawRackPosition; virtual;
        procedure ChangeSlot(aSlot: TObject; aRotation: TRotationValue);
        procedure ChangeZMax(iZMaxAdd: integer);
        procedure FillRackWithCaps;
        function IsCapParkRack: boolean;
        function IsCapWasteRack: boolean;
        procedure CheckAndDisplay;
        function FindRandomTubeBC(): TRackPositions;
        procedure ClearAllTubePaint();
        function GetWellFromWellNr(aWellNr: integer): TRackWell;

        procedure PaintTubePos(const aPos: integer; const aColorType: TRackWellDisplayType);
        function CalcRackPosition(aOffsetX, aOffsetY, aOffsetZ: TPosMM): TPoint4d;
        function CalcTubePos(aPos: integer): TPoint4d;
        function CreateRackPosition(aOffsetX, aOffsetY, aOffsetZ: TPosMM): TGeom3D_Position;
        function CreateTubePos(aPos: integer): TGeom3D_Position;
        function IsBigReactionBlock(): boolean;
        function CreateRackNameAndIDString(): string;
        function IsMultiTipRack(): boolean;
        function TubeIsRectangle(): boolean;
        procedure SetUp(const aName, aRackID: string; const aRackType: TRackRec);
        procedure ChangeType(const aRackType: TRackRec);
        function CalcRackTopZPos: TPosMM;
        function CalcHighestZPos(): TPosMM;
        procedure SetType(const aRackType: TRackRec); // muss wieder zu private
        procedure HighlightPosition(aPos: integer; aValue: boolean);
        function CalcVolumeOfRackPos(aPos: integer): double;
        function WellIsStoragePosition(aPos: integer; out aSubstanceSetRec: TSubstanceSetRec): boolean;

        class function GetRotationDegree(aRotationValue: TRotationValue): integer;
        class function GetRotationValue(aRotationDegree: integer): TRotationValue;
        class function CreateRackTypeView(const aRackTypeRec: TRackRec): TRack;
        // properties
        property TypeName: string read FTypeName;
        property Structure: TRackStruct read fStruct;
        property RackStructure: TRackStruct read fStruct;
        property RackID: string read FRackID write SetRackID;
        property PosShifted: boolean read FPosShifted write FPosShifted;
        property HRTake_degree: TPosMM read GetHRTake_degree;
        property Slot: TObject read fSlot;
        property TubeCount: integer read GetTubeCount;
        property WellSurface_mm2: TPosMM read GetWellSurface_mm2;
        property WellMaxVolume_mm3: TPosMM read GetWellMaxVolume_mm3;
        property RackRotation: TRotationValue read fRackRotation write SetRackRotation;
        property IsLinked: boolean read fIsLinked write fIsLinked;
        property TubeX_mm: TPosMM read GetTubeX_mm;
        property TubeY_mm: TPosMM read GetTubeY_mm;
        property SizeX: TPosMM read GetSizeX;
        property SizeY: TPosMM read GetSizeY;
        property SizeZ: TPosMM read GetSizeZ write SetSizeZ;
        property StackHeightZ: TPosMM read GetStackHeightZ;
        property Graphics: TRackGraphics read GetGraphics;
        property OrigignalTypeName: string read fOriginalTypeName;
        property RackWellShape: TRackWellShape read GetRackWellShape;
        property ParentMakeHintTextCallback: TParentMakeHintTextCallback read fParentMakeHintTextCallback
            write fParentMakeHintTextCallback;
        property RackSizeHasChanged: TLayoutElementChangeSizeEvent read fRackSizeHasChanged
            write fRackSizeHasChanged;
        property Highlight: boolean read FHighlight write SetHighlight;
        property Wells: TObjectList<TRackWell>read fWells;
    end;


implementation


uses
    Variants,
    Math,
    SamGlobe,
    PosinfoDataAdaptor,
    GUIManager;

{ TRack }

constructor TRack.Create();
begin
    inherited Create();

    FPosShifted := true;
    fWells := TObjectList<TRackWell>.Create(true);
end;

destructor TRack.Destroy;
begin
    fWells.Free;
    inherited;
end;

procedure TRack.SetWellsVisible(aVisible: boolean);
var
    x: integer;
begin
    if self.RackStructure.DoNotPaintTubes then
        EXIT;
    for x := 0 to fWells.Count - 1 do
    begin
        fWells[x].Visible := aVisible;
    end;
end;

procedure TRack.SetVisible(aVisible: boolean);
begin
    inherited;
    SetWellsVisible(aVisible);
end;

procedure TRack.DoAfterCreateRackWell(aRackWell: TRackWell);
begin
end;

function TRack.DoCreateRackWell(aWellNr: integer): TRackWell;
begin
    result := TRackWell.Create(aWellNr);
end;

function TRack.CreateRackWell(aWellNr: integer): TRackWell;
begin
    result := DoCreateRackWell(aWellNr);
    result.Rack := self;
    result.InitGraphics;
    DoAfterCreateRackWell(result);
end;

function TRack.DoRackMakeHintText(aSender: TObject): string;
begin
    result := MakeHintText();
end;

function TRack.MakeWellPoint(aX, aY, aZ: double): TPoint4d;
begin
    // Wir berechnen den Mittelpunkt der Wellposition:
    // Für die Zeichnung brauchen wir aber die linke obere Ecke (Nullpunkt)
    result := MakePoint4d(aX - (self.TubeX_mm / 2), aY - (self.TubeY_mm / 2), aZ);
end;

function TRack.GetDiscretePosition(aRow, aCol: integer): TPoint4d;
var
    xDA: TDiscreteRackDataAdaptor;
    xPoint: TPoint4d;
begin
    xDA := TDiscreteRackDataAdaptor.Create();
    try
        xDA.SelectAndOpenRackPosition(FTypeName, aCol + 1, true);
        xPoint.x := xDA.DataProvider.FieldByNameAsFloat('Pos_X_mm');
        xPoint.y := xDA.DataProvider.FieldByNameAsFloat('Pos_Y_mm');
        xPoint.z := 0;
        xPoint.w := 0;
        xDA.Close;
    finally
        FreeAndNil(xDA);
    end;
    result := xPoint;
end;

function TRack.CalcWellAroundCenter(aRow, aCol: integer): TPoint4d;
var
    xCenterPoint: TPoint4d;
    xRadiusFirst, xRadiusLast, xRadiusCurrent, xRadiusAllPos, xRadiusOnePos: extended;
    xAzimutFirst, xAzimutLast, xAzimutCurrent: extended;
    xAngleAllPos, xAngleOnePos: extended;
begin
    xCenterPoint := MakePoint4d(fStruct.CenterX_mm, fStruct.CenterY_mm, 0);

    // Radius und Winkel aus Mittelpunkt und erster Position berechnen
    xRadiusFirst := Sqrt(Power(fStruct.PosX_First_mm - fStruct.CenterX_mm, 2) +
        Power(fStruct.PosY_First_mm - fStruct.CenterY_mm, 2));
    xRadiusLast := Sqrt(Power(fStruct.PosX_Last_mm - fStruct.CenterX_mm, 2) +
        Power(fStruct.PosY_Last_mm - fStruct.CenterY_mm, 2));
    xAzimutFirst := Math.ArcTan2(fStruct.PosY_First_mm - fStruct.CenterY_mm,
        fStruct.PosX_First_mm - fStruct.CenterX_mm);
    xAzimutLast := Math.ArcTan2(fStruct.PosY_Last_mm - fStruct.CenterY_mm,
        fStruct.PosX_Last_mm - fStruct.CenterX_mm);

    if (xAzimutLast = xAzimutFirst) or (fStruct.Cols <= 1) then
    begin
        // kompletter Kreis
        xAngleAllPos := 2 * System.Pi;
        // Achtung: Letzte Position gibt es nicht, deshalb ein Kreisabsschnitt mehr!!
        xAngleOnePos := xAngleAllPos / fStruct.Cols;
    end
    else
    begin
        // Kreisabschnitt
        xAngleAllPos := xAzimutLast - xAzimutFirst;
        if (xAngleAllPos <= 0) then // Wert muss im Bereich 0 bis 2*pi liegen
            xAngleAllPos := xAngleAllPos + 2 * System.Pi;
        xAngleOnePos := xAngleAllPos / (fStruct.Cols - 1); // Achtung: Cols darf nicht 1 sein
    end;

    xAzimutCurrent := xAngleOnePos * aCol + xAzimutFirst;

    // mehrere Kreise umeinander sind möglich:
    if fStruct.Rows > 1 then
    begin
        xRadiusAllPos := xRadiusLast - xRadiusFirst;
        xRadiusOnePos := xRadiusAllPos / (fStruct.Rows - 1); // Achtung: Rows darf nicht 1 sein
        xRadiusCurrent := xRadiusOnePos * aRow + xRadiusFirst;
    end
    else
    begin
        xRadiusCurrent := xRadiusFirst;
    end;

    // andere Eigenheiten wie Slope, Abstände oder Versatz werden (noch) nicht berücksichtigt!

    result := MakeWellPoint(xRadiusCurrent * Cos(xAzimutCurrent) + fStruct.CenterX_mm,
        xRadiusCurrent * Sin(xAzimutCurrent) + fStruct.CenterY_mm, 0);
end;

function TRack.CalcWellPosition(aRow, aCol: integer): TPoint4d;
var
    xNoOfXOffset, xNoOfYOffset: integer;
    xColDist, // Abstand zwischen den Kolonnen
    xRowDist, // Abstand zwischen den Reihen
    xZSlopeDiff, xDifferenceX, xDifferenceY, xUsableWidthX, xUsableWidthY: TPosMM;
    xAddOffsetX, xAddOffsetY: TPosMM;
begin
    if fStruct.DiscretePositions then
        EXIT(self.GetDiscretePosition(aRow, aCol));
    if fStruct.WellsAroundCenter then
        EXIT(self.CalcWellAroundCenter(aRow, aCol));

    result := MakeWellPoint(fStruct.PosX_First_mm, fStruct.PosY_First_mm, 0);

    xDifferenceX := fStruct.PosX_Last_mm - fStruct.PosX_First_mm;
    xDifferenceY := fStruct.PosY_Last_mm - fStruct.PosY_First_mm;
    xUsableWidthX := xDifferenceX;
    xUsableWidthY := xDifferenceY;
    xAddOffsetY := 0;
    xAddOffsetX := 0;

    // Abstand zwischen den Kolonnen
    if (fStruct.AddOffsetX_AfterPos <> 0) then
    begin
        xNoOfXOffset := (fStruct.Cols - 1) div fStruct.AddOffsetX_AfterPos;
        xUsableWidthX := xUsableWidthX - (fStruct.AddOffsetX_mm * xNoOfXOffset);
        xAddOffsetX := ((aCol) div fStruct.AddOffsetX_AfterPos) * fStruct.AddOffsetX_mm;
    end;
    if (fStruct.Cols = 1) then
        xColDist := 0
    else
        xColDist := xUsableWidthX / (fStruct.Cols - 1);
    result.X := result.X + (aCol * xColDist) + xAddOffsetX;

    // Abstand zwischen den Reihen
    if (fStruct.AddOffsetY_AfterPos <> 0) then
    begin
        xNoOfYOffset := (fStruct.Rows - 1) div fStruct.AddOffsetY_AfterPos;
        xUsableWidthY := xUsableWidthY - (fStruct.AddOffsetY_mm * xNoOfYOffset);
        xAddOffsetY := ((aRow) div fStruct.AddOffsetY_AfterPos) * fStruct.AddOffsetY_mm;
    end;
    if (fStruct.Rows = 1) then
        xRowDist := 0
    else
        xRowDist := xUsableWidthY / (fStruct.Rows - 1);
    result.Y := result.Y + (aRow * xRowDist) + xAddOffsetY;

    // Versetzte Rackpositionen
    if ((aRow mod 2) <> 0) then
        result.X := result.X + fStruct.PosX_Offset_mm;
    if ((aCol mod 2) <> 0) then
        result.Y := result.Y + fStruct.PosY_Offset_mm;

    // Slope: Rack mit unterschiedlichen Höhen
    xZSlopeDiff := fStruct.Z_LastPos_mm - fStruct.SizeZ_mm;

    if (xZSlopeDiff <> 0) and ((fStruct.SlopeType = 1) or (fStruct.SlopeType = 2)) then
    begin

        // Höhenunterschied in X:
        if (fStruct.SlopeType = 1) and (xDifferenceX <> 0) then
            result.Z := result.Z + (xZSlopeDiff * (result.X - fStruct.PosX_First_mm) / (xDifferenceX));

        // Höhenunterschied in Y:
        if (fStruct.SlopeType = 2) and (xDifferenceY <> 0) then
            result.Z := result.Z + (xZSlopeDiff * (result.Y - fStruct.PosY_First_mm) / (xDifferenceY));
    end;
end;

procedure TRack.SetUpWell(aWell: TRackWell; aWellIndex: integer);
var
    xRowIndex, xColIndex: integer;
    xPoint: TPoint4d;
begin
    xRowIndex := aWellIndex mod fStruct.Rows;
    xColIndex := aWellIndex div fStruct.Rows;
    xPoint := CalcWellPosition(xRowIndex, xColIndex);
    aWell.SetUpWell(xRowIndex, xColIndex, self.TubeX_mm, self.TubeY_mm, fStruct.TubeZ_mm, xPoint.x, xPoint.y,
        xPoint.z, fStruct.Color, self.RackWellShape);
end;

procedure TRack.InitWells();
var
    x, xWellNr: integer;
    xWell: TRackWell;
begin
    for x := 0 to self.GetTubeCount - 1 do
    begin
        // Wells erzeugen (zu Beginn) oder fehlende erzeugen (bei ChangeRackType)
        if (x >= fWells.Count) then
        begin
            xWellNr := x + 1;
            xWell := CreateRackWell(xWellNr);
            xWell.Name := self.Name + ' Well ' + IntToStr(xWellNr);
            xWell.RackMakeHintTextCallback := DoRackMakeHintText;
            xWell.Graphics.Parent := self.Graphics;
            xWell.CoordCalculator.ParentCoordCalculator := fCoordCalculator;
            fWells.Add(xWell);
        end;

        // Well-Daten übertragen
        self.SetUpWell(fWells[x], x);
    end;

    SetWellsVisible(self.Visible);
end;

function TRack.GetTubeX_mm(): TPosMM;
begin
    if self.TubeIsRectangle then
        result := fStruct.TubeX_mm
    else
        result := fStruct.TubeY_mm;
end;

function TRack.GetTubeY_mm(): TPosMM;
begin
    result := fStruct.TubeY_mm;
end;

procedure TRack.SetRackRotation(aRotationDegree: TRotationValue);
var
    xTransX, xTransY: double;
    xDegree: double;
begin
    FRackRotation := aRotationDegree;
    xDegree := GetRotationDegree(aRotationDegree);

    xTransX := 0;
    xTransY := 0;
    case FRackRotation of
        rotation_90:
            begin
                xTransY := fStruct.SizeY_mm;
            end;
        rotation_180:
            begin
                xTransX := fStruct.SizeX_mm;
                xTransY := fStruct.SizeY_mm;
            end;
        rotation_270:
            begin
                xTransX := fStruct.SizeX_mm;
            end;
    end;
    self.CoordCalculator.CoordSystem.RotateZAround(xDegree, -xTransX, -xTransY);
    if Assigned(self.Graphics) then
    begin
        self.Graphics.CoordSystem.RotateZAround(xDegree, -xTransX, -xTransY);
    end;
end;

procedure TRack.ChangeSlot(aSlot: TObject; aRotation: TRotationValue);
begin
    fSlot := aSlot;
    self.RackRotation := aRotation;
    DrawRackPosition;
end;

function TRack.GetHRTake_degree: TPosMM;
begin
    // die Greif-Richtung des Racks wird aus "H_RTake_degree" (Rack) und FRackRotation berechnet
    result := fStruct.H_RTake_degree + GetRotationDegree(FRackRotation);
    if (result >= 360) then
        result := result - 360;
end;

procedure TRack.SetUp(const aName, aRackID: string; const aRackType: TRackRec);
begin
    // --------------------------------------------------- Rackname, -ID zuweisen
    self.Name := aName;
    FRackId := aRackID;
    // ------------------------------------------------------ RackDaten bestimmen
    FOriginalTypeName := aRackType.Name;
    SetType(aRackType);
end;

procedure TRack.ChangeType(const aRackType: TRackRec);
begin
    SetType(aRackType);
end;

function TRack.GetRackWellShape(): TRackWellShape;
begin
    if self.TubeIsRectangle() then
    begin
        result := rwsRect
    end
    else
    begin
        result := rwsCircle;
    end;
end;

function TRack.GetWellFromWellNr(aWellNr: integer): TRackWell;
var
    xWellIndex: integer;
begin
    result := nil;
    xWellIndex := aWellNr - 1;
    if (xWellIndex < 0) or (xWellIndex >= self.Wells.Count) then
        EXIT;
    result := fWells[aWellNr - 1];
end;

procedure TRack.UpdateGraphicsInfo();
begin
    with self.Graphics.GraphicsInfo do
    begin
        name := fName;
        SizeX := fStruct.SizeX_mm;
        SizeY := fStruct.SizeY_mm;
        SizeZ := self.SizeZ;
        Rows := fStruct.Rows;
        Cols := fStruct.Cols;

        TubePosX_First := fStruct.PosX_First_mm;
        TubePosY_First := fStruct.PosY_First_mm;
        TubePosX_Last := fStruct.PosX_Last_mm;
        TubePosY_Last := fStruct.PosY_Last_mm;

        IsMultiTipRack := self.IsMultiTipRack;
        IsTubeRectangle := self.TubeIsRectangle;
        TubeSizeZ := fStruct.TubeZ_mm;

        // TubeZoffset + TubeSizeZ = original SizeZ
        if self.TubeIsRectangle() then
        begin
            // TubeSizeZ         := 0;
        end
        else
        begin
            // TubeSizeZ         := SizeZ * 0.90;
            SizeZ := SizeZ * 0.60;
        end;
        WellZOffset := SizeZ * 0.10;
    end;
    self.Graphics.Hint := MakeHintText();
    self.Graphics.UpdateGraphicsInfo();
end;

procedure TRack.SetType(const aRackType: TRackRec);
begin
    FTypeName := aRackType.Name;
    with fStruct do
    begin
        TYP := aRackType.TYP;
        SizeX_mm := aRackType.X_mm;
        SizeY_mm := aRackType.Y_mm;
        self.SizeZ := aRackType.Z_mm;
        Rows := aRackType.Rows;
        Cols := aRackType.Cols;
        TubeTyp := aRackType.TubeTyp;
        TubeX_mm := aRackType.TubeX_mm;
        TubeY_mm := aRackType.TubeY_mm;
        TubeY2_mm := aRackType.TubeY2_mm;
        TubeZ_mm := aRackType.TubeZ_mm;
        TubeGetOpen_mm := aRackType.TubeGetOpen_mm;
        TubeGetClose_mm := aRackType.TubeGetClose_mm;
        TubePutOpen_mm := aRackType.TubePutOpen_mm;
        TubePutOffset_mm := aRackType.TubePutOffset_mm;
        TubeGripOffset_mm := aRackType.TubeGripOffset_mm;
        BorderX_mm := aRackType.BorderX_mm;
        BorderY_mm := aRackType.BorderY_mm;
        PosX_First_mm := aRackType.PosX_First_mm;
        PosY_First_mm := aRackType.PosY_First_mm;
        PosX_Last_mm := aRackType.PosX_Last_mm;
        PosY_Last_mm := aRackType.PosY_Last_mm;
        PosX_Offset_mm := aRackType.PosX_Offset_mm;
        PosY_Offset_mm := aRackType.PosY_Offset_mm;
        AddOffsetX_AfterPos := aRackType.AddOffsetX_AfterPos;
        AddOffsetX_mm := aRackType.AddOffsetX_mm;
        AddOffsetY_AfterPos := aRackType.AddOffsetY_AfterPos;
        AddOffsetY_mm := aRackType.AddOffsetY_mm;
        ZTravel_mm := aRackType.ZTravel_mm;
        ZScan_mm := aRackType.ZScan_mm;
        ZDisp_mm := aRackType.ZDisp_mm;
        ZMax_mm := aRackType.ZMax_mm;
        H_XTake_mm := aRackType.H_XTake_mm;
        H_YTake_mm := aRackType.H_YTake_mm;
        H_ZTake_mm := aRackType.H_ZTake_mm;
        H_RTake_degree := aRackType.H_RTake_degree;
        H_VIsUndefined := aRackType.H_VIsUndefined;
        H_VOpen_mm := aRackType.H_VOpen_mm;
        H_VClose_mm := aRackType.H_VClose_mm;
        MOffsetZ_mm := aRackType.MOffsetZ_mm;
        MOffsetUpX_mm := aRackType.MOffsetUpX_mm;
        MOffsetUpY_mm := aRackType.MOffsetUpY_mm;
        Shift_Radius_mm := aRackType.Shift_Radius_mm;
        Shift_NoOfSteps := aRackType.Shift_NoOfSteps;
        CapType := aRackType.CapType;
        CapDiameter := aRackType.CapDiameter;
        Cap_GripZ_mm := aRackType.Cap_GripZ_mm;
        Z_LastPos_mm := aRackType.Z_LastPos_mm;
        StackHeigthZ_mm := aRackType.StackHeight_mm;
        WellsAroundCenter := aRackType.WellsAroundCenter;
        CenterX_mm := aRackType.CenterX_mm;
        CenterY_mm := aRackType.CenterY_mm;
        SlopeType := aRackType.SlopeType;
        BlockBalanceDoor := aRackType.BlockBalanceDoor;
        Color := aRackType.Color;
        if (Color = 0) then
            Color := TRackGraphics.cRackColorDefault;
        DoNotPaintTubes := aRackType.DoNotPaintTubes;
        DiscretePositions := aRackType.DiscretePositions;
        RackMoveZTravelOffset := aRackType.RackMoveZTravelOffset;
    end;

    InitWells();

    // ------------------------------------------------------------ Rack Zeichnen
    SetRackColor(fStruct.Color);
    UpdateGraphicsInfo();

    if Assigned(fRackSizeHasChanged) then
        fRackSizeHasChanged(self, fStruct.SizeX_mm, fStruct.SizeY_mm, fStruct.SizeZ_mm);
end;

function TRack.CalcRackPosition(aOffsetX, aOffsetY, aOffsetZ: TPosMM): TPoint4d;
begin
    result := MakePoint4d(aOffsetX, aOffsetY, aOffsetZ);
    if not Assigned(fSlot) then
        EXIT;
    fCoordCalculator.CalcCoordMatrix();
    result := fCoordCalculator.TransformPoint(result);
end;

function TRack.CreateRackPosition(aOffsetX, aOffsetY, aOffsetZ: TPosMM): TGeom3D_Position;
var
    xPoint: TPoint4d;
begin
    xPoint := self.CalcRackPosition(aOffsetX, aOffsetY, aOffsetZ);
    result := TGeom3D_Position.Create(xPoint.x, xPoint.y, xPoint.z);
end;

function TRack.CalcRackTopZPos(): TPosMM;
var
    xTemp: TPoint4d;
begin
    xTemp := CalcRackPosition(0, 0, 0);
    result := xTemp.z;
end;

function TRack.FindHighestWell(): TRackWell;
var
    x: integer;
    xWell: TRackWell;
    xPosZ: TPosMM;
begin
    xPosZ := 0;
    result := nil;
    for x := 1 to self.GetTubeCount do
    begin
        xWell := GetWellFromWellNr(x);
        if (not Assigned(result)) or (xWell.PosZ > xPosZ) then
        begin
            result := xWell;
            xPosZ := result.PosZ;
        end;
    end;
end;

function TRack.CalcHighestZPos(): TPosMM;
var
    xWell: TRackWell;
    xWellCenterPoint: TPoint4d;
    xPoint: TPoint4d;
begin
    result := 0;
    xWell := FindHighestWell();
    if not Assigned(xWell) then
        EXIT;
    xWellCenterPoint := MakePoint4d(0, 0, 0);
    xWell.CoordCalculator.CalcCoordMatrix();
    xPoint := xWell.CoordCalculator.TransformPoint(xWellCenterPoint);
    result := xPoint.z;
end;

function TRack.CalcTubePos(aPos: integer): TPoint4d;
var
    xWell: TRackWell;
    xWellCenterPoint: TPoint4d;
begin
    xWell := GetWellFromWellNr(aPos);
    result := MakePoint4d(0, 0, 0);
    if not Assigned(xWell) then
        EXIT;
    xWellCenterPoint := MakePoint4d(xWell.SizeX / 2, xWell.SizeY / 2, 0);
    xWell.CoordCalculator.CalcCoordMatrix();
    result := xWell.CoordCalculator.TransformPoint(xWellCenterPoint);
end;

function TRack.CalcVolumeOfRackPos(aPos: integer): double;
var
    xTotalAmount: double;
    x: integer;
begin
    if (self.RackId = '') then
        EXIT(0);

    if IsMultiTipRack() then
    begin
        xTotalAmount := 0;
        for x := 0 to self.TubeCount - 1 do
            xTotalAmount := xTotalAmount + fWells[x].TotalVolume;
        EXIT(xTotalAmount);
    end
    else
        EXIT(self.GetWellFromWellNr(aPos).TotalVolume);
end;

function TRack.CreateTubePos(aPos: integer): TGeom3D_Position;
var
    xPoint: TPoint4d;
begin
    xPoint := CalcTubePos(aPos);
    result := TGeom3D_Position.Create(xPoint.x, xPoint.y, xPoint.z);
end;

procedure TRack.DrawRackPosition;
begin
    // Positionen zeichnen
end;

procedure TRack.PaintTubePos(const aPos: integer; const aColorType: TRackWellDisplayType);
var
    xWell: TRackWell;
begin
    xWell := self.GetWellFromWellNr(aPos);
    if Assigned(xWell) then
    begin
        xWell.SetAndPaintDisplayType(aColorType);
    end;
end;

procedure TRack.ClearAllTubePaint();
var
    x, xTubeCount: integer;
begin
    xTubeCount := self.GetTubeCount();
    for x := 0 to xTubeCount - 1 do
    begin
        PaintTubePos(x + 1, TRackWellDisplayType.Default);
    end;
end;

procedure TRack.SetRackID(const aRackID: string);
begin
    FRackID := aRackID;
end;

function TRack.CreateRackNameAndIDString(): string;
begin
    result := '';
    if (fName <> '') then
    begin
        AddHintLine('Rack', result);
        AddHintSubLine(Format('Name: [%s]', [fName]), result);

        if (FRackID <> '') then
            AddHintSubLine('Rack ID: ' + fRackID, result);
        if (FTypeName <> '') then
            AddHintSubLine('Type: ' + fTypeName, result);
        if self.IsMultiTipRack() then
            AddHintSubLine('Is Multi-Tip-Rack', result);
    end
    else
        AddHintLine(Format('Rack Name: [%s]', [FTypeName]), result);
end;

function TRack.MakeHintText: string;
begin
    result := CreateRackNameAndIDString();
    if (fStruct.CapType <> '') then
        AddHintSubLine(Format('Cap: %s', [fStruct.CapType]), result);
    if (FRackRotation <> rotation_0) then
        AddHintSubLine(Format('Rack Rotation: %d°', [GetRotationDegree(FRackRotation)]), result);
    AddHintSubLine(Format('Well Max. Vol: %d uL', [Round(self.WellMaxVolume_mm3)]), result);

    if Assigned(fParentMakeHintTextCallback) then
        AddHintLine(fParentMakeHintTextCallback(self), result);
end;

procedure TRack.ChangeZMax(iZMaxAdd: integer);
begin
    // -----"ZMax" um den den Wert "Resinheigt" erhöhen !
    fStruct.ZMax_mm := fStruct.ZMax_mm + iZMaxAdd;
end;

procedure TRack.SetRackColor(aColor: integer);
begin
    self.Graphics.ChangeRackColor(aColor);
end;

procedure TRack.SyncSetRackColor(aColor: integer);
begin
    gmMessageAndWait(VCL, VarArrayOf([vclSetRackColor, aColor]));
end;

procedure TRack.SetHighlight(const aValue: boolean);
var
    xColor: TColor;
begin
    FHighlight := aValue;

    if aValue then
        xColor := $00FF0080
    else
        xColor := fStruct.Color; // Farbe zurücksetzen

    self.SyncSetRackColor(xColor);
    self.SceneChanged;
end;

procedure TRack.FillRackWithCaps;
var
    xDataAdapter: TPosinfoDataAdaptor;
    xTubePos: TRackIDPosition;
    x: integer;
begin
    xDataAdapter := TPosinfoDataAdaptor.Create();
    xTubePos.Rack := FName;
    xTubePos.RackID := FRackID;

    for x := 1 to (fStruct.Cols * fStruct.Rows) do
    begin
        xTubePos.Pos := x;

        // if the rack is no cap-park-rack, create a virtual tube-ID for each pos. that has no real ID
        if (not IsCapParkRack) then
            xDataAdapter.WriteTubeID(xDataAdapter.CreateRandomTubeID, xTubePos);

        // create a cap entry in the posinfo
        xDataAdapter.WriteCapEntry(xTubePos);
    end;
    xDataAdapter.Free;
end;

function TRack.IsCapParkRack: boolean;
begin
    result := false;
    if Pos('CAPPARK', UpperCase(FName)) = 1 then
        result := true;
end;

function TRack.IsCapWasteRack: boolean;
begin
    result := false;
    if Pos('CAPWASTE', UpperCase(FName)) = 1 then
        result := true;
end;

procedure TRack.CheckAndDisplay;
var
    xName: string;
begin
    xName := FName;

    // CapPark-Racks müssen einen definierten Captyp haben
    if IsCapParkRack and (fStruct.CapType = '') then
        gGUIManager.MessageBox(TLanguageString.
            Read('Rack {0} is a cap-park-rack. The cap type has to be defined!',
            'Rack {0} ist ein Deckel-Park-Rack. Der Deckeltyp muß definiert werden!', [xName]),
            TLanguageString.Read('Data Error', 'Datenfehler'), 16);

    if IsCapParkRack and (RackID = '') then
        gGUIManager.MessageBox(TLanguageString.
            Read('Rack {0} is a cap-park-rack. A Rack-ID has to be defined!',
            'Rack {0} ist ein Deckel-Park-Rack. Eine Rack-ID muß definiert werden!', [xName]),
            TLanguageString.Read('Data Error', 'Datenfehler'), 16);

    if IsCapWasteRack and (RackID <> '') then
        gGUIManager.MessageBox(TLanguageString.
            Read('Rack {0} is a cap-waste rack. This rack must not have a Rack-ID!',
            'Rack {0} ist ein Cap-Waste-Rack. Dises rack darf keine Rack-ID haben!', [xName]),
            TLanguageString.Read('Data Error', 'Datenfehler'), 16);

    // if IsDecapperRack and (RackStructure.CapType = '') then
    // gmResFMsgBox(26130{Rack %s is a decapper rack. A cap type has to be defined!}, 16, [xName]);
end;

function TRack.FindRandomTubeBC(): TRackPositions;
var
    xDataAdapter: TPosinfoDataAdaptor;
begin
    if (FRackID <> '') then
    begin
        xDataAdapter := TPosinfoDataAdaptor.Create();
        try
            result := xDataAdapter.FindRandomTubeBC(FName, FRackID);
        finally
            xDataAdapter.Free;
        end;
    end;
end;

function TRack.GetTubeCount: integer;
begin
    result := fStruct.Rows * fStruct.Cols;
end;

function TRack.GetWellSurface_mm2: TPosMM;
begin
    if not self.TubeIsRectangle() then
        // Tube ist rund
        result := pi * (sqr(fStruct.TubeY_mm) / 4)
    else
        result := fStruct.TubeY_mm * fStruct.TubeX_mm;
end;

procedure TRack.HighlightPosition(aPos: integer; aValue: boolean);
var
    xWell: TRackWell;
begin
    xWell := self.GetWellFromWellNr(aPos);
    if Assigned(xWell) and Assigned(xWell.Graphics) then
    begin
        xWell.Graphics.Highlighted := aValue;
    end;
    self.SceneChanged;
end;

function TRack.GetWellMaxVolume_mm3: TPosMM;
begin
    result := fStruct.TubeZ_mm * GetWellSurface_mm2;
end;

function TRack.VCL(const aArgs: TMessageArg): TMessageResult;
var
    xType: TVCLType;
begin
    xType := aArgs[0];
    case xType of
        vclSetRackColor:
            self.SetRackColor(aArgs[1]);
    end;
end;

function TRack.WellIsStoragePosition(aPos: integer; out aSubstanceSetRec: TSubstanceSetRec): boolean;
var
    xIndex: integer;
begin
    xIndex := aPos - 1;

    if not(self.Wells[xIndex].StorageData.IsStorage) then
        EXIT(false);

    aSubstanceSetRec.SetName := self.Wells[xIndex].StorageData.SetName;
    aSubstanceSetRec.RackID := self.RackID;
    aSubstanceSetRec.Pos := self.Wells[xIndex].WellNr;
    aSubstanceSetRec.SubstID := self.Wells[xIndex].StorageData.SubstID;
    aSubstanceSetRec.Amount := self.Wells[xIndex].TotalVolume;
    aSubstanceSetRec.MinVolume1 := self.Wells[xIndex].StorageData.MinVolume1;
    aSubstanceSetRec.MinVolume2 := self.Wells[xIndex].StorageData.MinVolume2;
    aSubstanceSetRec.MaxVolume := self.Wells[xIndex].StorageData.MaxVolume;
    EXIT(true);
end;

function TRack.IsBigReactionBlock(): boolean;
begin
    result := fStruct.PosY_Last_mm - fStruct.PosY_First_mm > 30;
end;

function TRack.IsMultiTipRack(): boolean;
begin
    result := ((fStruct.TubeTyp and cTubeTypeSingle) = 0)
end;

function TRack.TubeIsRectangle(): boolean;
begin
    result := (fStruct.TubeX_mm > 0);
end;

class function TRack.GetRotationDegree(aRotationValue: TRotationValue): integer;
begin
    case aRotationValue of
        rotation_90:
            result := 90;
        rotation_180:
            result := 180;
        rotation_270:
            result := 270;
        else
            result := 0;
    end;
end;

class function TRack.GetRotationValue(aRotationDegree: integer): TRotationValue;
begin
    case aRotationDegree of
        90:
            result := rotation_90;
        180:
            result := rotation_180;
        270:
            result := rotation_270;
        else
            result := rotation_0;
    end;
end;

procedure TRack.DoInitGraphics;
begin
    fGraphics := TRackGraphics.Create();
end;

function TRack.GetGraphics: TRackGraphics;
begin
    result := fGraphics as TRackGraphics;
end;

function TRack.GetSizeX: TPosMM;
begin
    result := fStruct.SizeX_mm;
end;

function TRack.GetSizeY: TPosMM;
begin
    result := fStruct.SizeY_mm;
end;

function TRack.GetSizeZ: TPosMM;
begin
    result := fStruct.SizeZ_mm;
end;

procedure TRack.SetSizeZ(const aValue: TPosMM);
begin
    fStruct.SizeZ_mm := aValue;
    self.CoordCalculator.CoordSystem.TranslateSizeZ := aValue;
end;

function TRack.GetStackHeightZ: TPosMM;
begin
    result := fStruct.StackHeigthZ_mm;
end;

class function TRack.CreateRackTypeView(const aRackTypeRec: TRackRec): TRack;
begin
    result := TRack.Create();
    result.InitGraphics();
    result.SetType(aRackTypeRec);
    result.Graphics.CoordSystem.ReflectY := true;
    result.Graphics.CoordSystem.TranslateY := result.Graphics.CoordSystem.TranslateY +
        result.Graphics.GraphicsInfo.SizeY;
end;


end.
