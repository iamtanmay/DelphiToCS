{ --------------------------------------------------------------------------------------------------
  Copyright © 2008 Zinsser Analytic GmbH - All rights reserved.
  Author       : Wolfgang Lyncke (wl)
  Description  :
  --------------------------------------------------------------------------------------------------
  Revision History:
  date     op  method                       track-no  improvement/change
  -------- --  ---------------------------  --------  ----------------------------------------------
  08.01.08 wl                               TN3972    initial version
  18.02.08 wl  TRunStepBuilderHelper.Create TN4009    fIsSeqAuto wird zu Beginn auf true gesetzt
  20.06.08 pk                               TN4139    WB global object replaced by LayoutManager
  03.07.08 wl                               TN4157
  09.07.08 pk  RegisterGroupID              TN4160    avoid internal exception
  31.07.08 pk                               TN4031    IsRedi now determined only once in CreateLiqHCacheElement and stored in TLiqHCacheElement
  02.09.08 pk                               TN4215    various changes
  06.10.08 pk  RunRecFromMethodStep         TN4258    removed
  06.11.08 pk                               TN4279    uneeded code removed
  10.11.08 pk                               TN4279    fLiqHCache made case-insensitive
  16.01.09 wl                               TN4362   an Änderungen in TQueryDataAdaptor angepasst
  13.07.09 pk                               TN4585.4  FindLiqHParam removed
  10.08.09 wl                               TN4702   Strings werden jetzt direkt geladen
  28.08.09 pk  GetRackPositionNumber        TN4753   No longer uses PipDeviceManager
  31.08.09 pk  Create                       TN4753   Read settings gRunCreatePaint, etc
  04.11.09 pk                               TN4843   Replace TObjectList, TStringList, TStrings with Generics Lists and TStringArray
  04.02.10 pk                               TN4972   Changes for Restart
  17.06.10 wl  TFindRackByNameEvent         TN5150   durch Event wird Zugriff auf LayoutManager ersetzt
  21.07.10 pk  GetActPos                    TN5066   exception removed
  25.01.11 wl  IsRackNameRequired           TN5445   exception removed
  06.04.11 wl  fLiqHCache                   TN5501   wird nur noch immer neu geladen, wenn fReloadLiqHParamsForEachStep gesetzt ist
  14.04.11 wl  GetLiqHElement               TN5501   LiqParams werden nur für den einen Namen neu geladen
  27.09.11 wl  CreateIndividualLiqHRec      TN5698   Ersetzt Liquid Handling Parameter durch LiquidClass-Daten
  20.09.11 wl                               TN5723   Es werden beim Build keine Positionen mehr gefärbt
  17.11.11 wl  GetRackPositionNumber        TN5725   TubeID als Racknamen werden hier in Positionen verwandelt
  17.11.11 wl  Create                       TN5725   gRunCreateRackNameRequired raus (ist jetzt überflüssig)
  21.11.11 wl  FindXRackPosBySubstID        TN5730   von SubstanceLoading hierher
  21.11.11 wl                               TN5730   SplitVolume möglich: Pipettierschritte können aufgeteilt werden, wenn 1.Position nicht ausreicht
  27.11.11 wl                               TN5730   Bugfix
  27.12.11 wl                               TN5773   ein zusätzliches WasteVol wird für die Volumenkontrolle mit verwendet
  30.12.11 wl                               TN5773   das voraussichtliche DestWasteVolumen wird mit berechnet, außerdem gibt es MinSplitVol
  24.01.12 wl  FindRackPosBySubstIDIntern   TN5773.1 Schleifenbedingung war falsch
  06.02.12 wl  FindRackPosBySubstIDIntern   TN5793   Split Volumes funktioniert jetzt wieder
  07.02.12 wl  FindRackPosBySubstIDIntern   TN5793   Weitere Korrekturen bei der Berechnung, keine Exception mehr wenn zu wenig Volumen
  07.02.12 wl  GetWasteVolByLHP             TN5793   aLHRec.SampleAspMultiMaxVol nur berücksichtigen, wenn > 0
  23.03.12 wl  CalcWasteAndAddPos           TN5843   Wenn RackName = '', dann kein Eintrag: Führt zu Exception in GetRackPositionNumber
  23.03.12 wl  GetRackPositionArray         TN5843   Wenn kein Eintrag, dann Fake-Eintrag erstellen und MissingVol = Vol + Waste
  05.07.12 wl  GetXYByRackPosition          TN5931   neue Funktion, um XY-Position zu bestimmen
  10.08.12 wl  CreateIndividualLiqHRec      TN5947.1 abgeschafft: WASHAFTERDISPENSE, WASHISFORCED, USEWASHMACRO, WASHMACRONAME, DRYAFTERWASH
  10.09.12 wl  UseDeadVolume                TN5979   neue Property: Bestimmt, welches Min-Volumen als Totvolumen verwendet wird
  04.02.12 wl  GetWasteVolByLHP             TN6079   The calculation for SampleAspWastePerCent was wrong
  04.02.12 wl  CalculateWaste               TN6080   The calculation of waste volumes can be disabled
  11.02.13 wl                               TN6078   Unit überarbeitet; Teile in neue Unit VolumeTracker verlagert
  13.02.13 wl  GetCurrentThreadID           TN6075   neu
  26.08.13 wl                               TN6236   uses geändert
  -------------------------------------------------------------------------------------------------- }

unit RunStepBuilderHelper;


interface


uses
    AppTypes,
    MethodTypes,
    RackWell,
    VolumeTracker,
    LiqHTypes,
    LiqHDataCache;

type
    TFindRackDataByNameEvent = function(var vRackName: string; out oRackRows, oRackCols: integer;
        out oRotation: TRotationValue): boolean of object;
    TFindXYByRackPosEvent = function(const aRackName: string; const aPos: integer; out aX, aY: double)
        : boolean of object;
    TGetCurrentThreadIDEvent = function: cardinal of object;

    TRunStepBuilderHelper = class
    strict private
        fOnFindRackDataByName: TFindRackDataByNameEvent;
        fOnFindXYByRackPos: TFindXYByRackPosEvent;
        fOnGetCurrentThreadID: TGetCurrentThreadIDEvent;

        // Liquid Handling
        fReloadLiqHParamsForEachStep: boolean;
        fLiqHCache: TLiqHDataCompleteCache;

        // Suchen von Positionen
        fVolumeTracker: TVolumeTracker;

        class function GetRackTypeName(aRackNameType: TMethodRackNameType): string;
        class function GetRackPosTypeName(aRackNameType: TMethodRackNameType): string;
        class function GetActPos(aPosCnt: integer; aFirstCoord, aLastCoord: string;
            aRackRows, aRackCols: integer; aRotation: TRotationValue): integer;

        function InternGetRackPosition(aRackNameType: TMethodRackNameType; const aRackNameOrID: string;
            const aRackFirstPos, aRackLastPos: string; aPosCounter: integer): TRackPosition;
    public
        constructor Create(aIsDesignTimeBuild: boolean; aOnGetCurrentThreadID: TGetCurrentThreadIDEvent;
            aOnFindRackDataByName: TFindRackDataByNameEvent;
            aOnFindRackPosByTubeID: TFindRackPosByTubeIDEvent;
            aOnFindRackPosBySubstID: TFindRackPosBySubstIDEvent;
            aOnFindAllStorageRackPos: TFindAllStorageRackPosEvent; aOnFindXYByRackPos: TFindXYByRackPosEvent);
        destructor Destroy; override;
        //
        procedure ClearUsedStorages;

        function GetRackPositionNumber(aRackNameType: TMethodRackNameType; var vRackName: string;
            const aRackFirstPos, aRackLastPos: string; aPosCounter: integer; const aVol: extended)
            : integer; overload;
        function GetRackPositionNumber(aRackNameType: TMethodRackNameType; var vRackName: string;
            const aRackPos: string; const aVol: extended): integer; overload;
        function GetRackPositionNumber(aRackNameType: TMethodRackNameType; var vRackName: string;
            const aRackFirstPos, aRackLastPos: string; aPosCounter: integer): integer; overload;
        function GetRackPositionNumber(aRackNameType: TMethodRackNameType; var vRackName: string;
            const aRackPos: string): integer; overload;
        function GetRackPositionNumberNoIDReplace(aRackNameType: TMethodRackNameType; var vRackName: string;
            const aRackFirstPos, aRackLastPos: string; aPosCounter: integer): integer; overload;
        function GetRackPositionNumberNoIDReplace(aRackNameType: TMethodRackNameType; var vRackName: string;
            const aRackPos: string): integer; overload;
        function GetLiqHElement(const aName: string): TLiqHCacheElement;
        function CreateIndividualLiqHRec(aCommonRec: TLiqHandlingRec;
            const aAspVol, aDilVol, aDispVol, aSpitbackVol: double): TLiqHandlingRec;
        function GetXYByRackPosition(const aRackName: string; const aPos: integer;
            out oX, oY: double): boolean;
        function GetCurrentThreadID: cardinal;

        property VolumeTracker: TVolumeTracker read fVolumeTracker;
    end;


implementation


uses
    SysUtils,
    GeneralTypes,
    CommonTypes,
    MathUtils,
    AppSettings,
    MethodGUIParsing,
    TipTypeDataAdaptor,
    LiqClassDataAdaptor;

{ TRunStepBuilderHelper }

constructor TRunStepBuilderHelper.Create(aIsDesignTimeBuild: boolean;
    aOnGetCurrentThreadID: TGetCurrentThreadIDEvent; aOnFindRackDataByName: TFindRackDataByNameEvent;
    aOnFindRackPosByTubeID: TFindRackPosByTubeIDEvent; aOnFindRackPosBySubstID: TFindRackPosBySubstIDEvent;
    aOnFindAllStorageRackPos: TFindAllStorageRackPosEvent; aOnFindXYByRackPos: TFindXYByRackPosEvent);
var
    xIniAccess: IWinLissyIniAccess;
begin
    inherited Create;

    fOnFindRackDataByName := aOnFindRackDataByName;
    fOnFindXYByRackPos := aOnFindXYByRackPos;
    fOnGetCurrentThreadID := aOnGetCurrentThreadID;

    fVolumeTracker := TVolumeTracker.Create(aOnFindRackPosByTubeID, aOnFindRackPosBySubstID,
        aOnFindAllStorageRackPos);
    fLiqHCache := TLiqHDataCompleteCache.Create;

    xIniAccess := gCommonDll.CreateAppIni;
    fReloadLiqHParamsForEachStep := xIniAccess.ReadBool('Pipetting', 'ReloadLiqHParamsForEachStep');
end;

destructor TRunStepBuilderHelper.Destroy;
begin
    FreeAndNil(fLiqHCache);
    FreeAndNil(fVolumeTracker);

    inherited;
end;

function TRunStepBuilderHelper.CreateIndividualLiqHRec(aCommonRec: TLiqHandlingRec;
    const aAspVol, aDilVol, aDispVol, aSpitbackVol: double): TLiqHandlingRec;
var
    xLiqClassRec: TLiqClassRecExtern;
    xLiqClassDataAdaptor: TLiqClassDataAdaptor;
    xLiqClassFound: boolean;
begin
    result := aCommonRec;
    if (aCommonRec.LiqClass = '') then
        EXIT;

    // Liquid Handling-Einstellungen ersetzen durch Werte aus LiqClass
    xLiqClassDataAdaptor := TLiqClassDataAdaptor.Create;
    try
        xLiqClassFound := xLiqClassDataAdaptor.GetRecForVol(aCommonRec.LiqClass, aAspVol, aDilVol, aDispVol,
            aSpitbackVol, xLiqClassRec);
        ASSERT(xLiqClassFound, Format('Reading LiqHClass [%s] failed!', [aCommonRec.LiqClass]));

        result.SampleAspSpeed := xLiqClassRec.SampleAspSpeed;
        result.SampleAspDelay := xLiqClassRec.SampleAspDelay;
        result.SampleAspCalc := false;

        result.SampleAspWasteVol := xLiqClassRec.SampleAspWasteVol;
        result.SampleAspWastePerCent := xLiqClassRec.SampleAspWastePerCent;

        result.SysAirAspVol := xLiqClassRec.SysAirAspVol;
        result.SysAirAspSpeed := xLiqClassRec.SysAirSpeed;
        result.SysAirDispVol := xLiqClassRec.SysAirDispVol;
        result.SysAirAspCalc := false;

        result.TransAirVol := xLiqClassRec.TransAirVol;
        result.TransAirSpeed := xLiqClassRec.TransAirSpeed;
        result.TransAirAspDelay := xLiqClassRec.TransAirDelay;
        result.TransAirAspCalc := false;

        result.SampleAspSpitBack := xLiqClassRec.SampleAspSpitBackVol;
        result.SampleAspSpitBackCount := xLiqClassRec.SampleAspSpitBackCount;
        result.SampleAspSpitBackSpeed := xLiqClassRec.SampleAspSpitBackSpeed;
        result.SampleAspCh2WashDelay := xLiqClassRec.SampleAspSpitBackDelay;
        result.SampleAspCh2WashVol := xLiqClassRec.SampleAspCh2SpitBackVol;
        result.SampleAspCh2WashSpeed := xLiqClassRec.SampleAspCh2SpitBackSpeed;
        result.SampleAspSpitBackCalc := false;
        result.SampleAspCh2WashCalc := false;

        result.DilAspSpeed := xLiqClassRec.DilAspSpeed;
        result.DilAspDelay := xLiqClassRec.DilAspDelay;
        result.DilAspCalc := false;

        result.DispSpeed := xLiqClassRec.DispenseSpeed;
        result.DispDelay := xLiqClassRec.DispenseDelay;
        result.DispCalc := false;

        result.Wash.VolMin := xLiqClassRec.WashVolMin;
        result.Wash.VolMax := xLiqClassRec.WashVolMax;
        result.Wash.VolFactor := xLiqClassRec.WashVolFactor;
        result.Wash.VolChannel2 := xLiqClassRec.WashVolChannel2;
    finally
        FreeAndNil(xLiqClassDataAdaptor);
    end;

end;

class function TRunStepBuilderHelper.GetActPos(aPosCnt: integer; aFirstCoord, aLastCoord: string;
    aRackRows, aRackCols: integer; aRotation: TRotationValue): integer;
{
  Errechnen der Aktuellen Rackposition aus einer im Rack gewählten Matrix
  Parameter :
  PosCnt           = Probenzähler in der Matrix
  FirstCoord       = 1. Matrixkoordinate     z.B. B1
  LastCoord        = Letzte Matrixkoordinate z.B.  H11
  RackRows         = Anzahl Reihen im Rack
  RackCols         = Anzahl Kolonnen im Rack
  Result           = Aktuelle Position im Rack als Zahl
}
var
    xFirstCoordType, xLastCoordType: TCoordType;
    xPos: integer;
begin
    xFirstCoordType := TMethodGUIParser.GetCoordType(aFirstCoord);
    xLastCoordType := TMethodGUIParser.GetCoordType(aLastCoord);

    xPos := 0;
    result := xPos;

    if (xFirstCoordType = ctMatrix) and (xLastCoordType = ctMatrix) then
    begin
        xPos := TMethodGUIParser.MatrixCoordCurrentPos(aPosCnt, aFirstCoord, aLastCoord, aRackRows,
            aRackCols, 0);
    end
    else if (xFirstCoordType = ctInteger) and (xLastCoordType = ctInteger) then
    begin
        xPos := TMethodGUIParser.IntegerCoordCurrentPos(aPosCnt, aFirstCoord, aRackRows, 0);
    end;

    if (xPos > aRackRows * aRackCols) then
        EXIT;

    result := xPos;
end;

function TRunStepBuilderHelper.GetCurrentThreadID: cardinal;
begin
    EXIT(fOnGetCurrentThreadID);
end;

class function TRunStepBuilderHelper.GetRackTypeName(aRackNameType: TMethodRackNameType): string;
begin
    case aRackNameType of
        rnSource:
            EXIT(TLanguageString.Read('Source rack', 'Quellrack'));
        rnDestination:
            EXIT(TLanguageString.Read('Destination rack', 'Ziel-Rack'));
        rnDiluent:
            EXIT(TLanguageString.Read('Diluent rack', 'Lösemittel-Rack'));
        else
            EXIT(TLanguageString.Read('Rack', 'Rack'));
    end;
end;

function TRunStepBuilderHelper.GetXYByRackPosition(const aRackName: string; const aPos: integer;
    out oX, oY: double): boolean;
begin
    EXIT(fOnFindXYByRackPos(aRackName, aPos, oX, oY));
end;

function TRunStepBuilderHelper.InternGetRackPosition(aRackNameType: TMethodRackNameType;
    const aRackNameOrID, aRackFirstPos, aRackLastPos: string; aPosCounter: integer): TRackPosition;
var
    xRackRows, xRackCols, xActPos: integer;
    xRackName: string;
    xRotation: TRotationValue;
begin
    // Rack suchen
    xRackName := aRackNameOrID;
    if not fOnFindRackDataByName(xRackName, xRackRows, xRackCols, xRotation) then
    begin
        raise Exception.Create(TLanguageString.Read('{0} {1} not found in layout.',
            '{0} {1} ist im Layout nicht vorhanden.', [GetRackTypeName(aRackNameType), aRackNameOrID]));
    end;

    // Aktuelle Position berechnen
    xActPos := TRunStepBuilderHelper.GetActPos(aPosCounter, aRackFirstPos, aRackLastPos, xRackRows, xRackCols,
        xRotation);
    if (xActPos <= 0) then
        raise Exception.Create(TLanguageString.Read('An invalid {0} was calculated [{1}]',
            'Eine ungültige {0} wurde berechnet [{1}]', [GetRackPosTypeName(aRackNameType), xActPos]));

    result.Rack := xRackName;
    result.Pos := xActPos;
end;

class function TRunStepBuilderHelper.GetRackPosTypeName(aRackNameType: TMethodRackNameType): string;
begin
    case aRackNameType of
        rnSource:
            EXIT(TLanguageString.Read('Source Position', 'Quellposition'));
        rnDestination:
            EXIT(TLanguageString.Read('Destination Position', 'Zielposition'));
        rnDiluent:
            EXIT(TLanguageString.Read('Diluent Position', 'Lösemittel-Position'));
        else
            EXIT(TLanguageString.Read('Position', 'Position'));
    end;
end;

procedure TRunStepBuilderHelper.ClearUsedStorages;
begin
    fVolumeTracker.ClearStoragePositions;
end;

function TRunStepBuilderHelper.GetRackPositionNumber(aRackNameType: TMethodRackNameType;
    var vRackName: string; const aRackFirstPos, aRackLastPos: string; aPosCounter: integer;
    const aVol: extended): integer;
var
    xRackPos: TRackPosition;
    xPosAsInt: integer;
    xPosArr: TArray<TRackPositionWithVolAndWaste>;
    xMissingVolume: extended;
    xLHRec: TLiqHandlingRec;
begin
    // Sonderfall: Position = -1 heißt, dass vRackName als TubeID verwendet wird!
    xPosAsInt := StrToIntDef(aRackFirstPos, 0);
    if (xPosAsInt = INT_RACKPOS_RACKNAME_IS_TUBEID) then
    begin
        xLHRec.Valid := false;
        xPosArr := fVolumeTracker.FindRackPosBySubstOrTubeID(false, vRackName, aVol, 0, 0, aRackNameType,
            false, xMissingVolume, xLHRec);
        if Length(xPosArr) = 0 then
            raise Exception.Create(TLanguageString.Read('{0}: Tube-ID/Substance-ID {1} not found in layout.',
                '{0}: Tube-ID/Substanz-ID {1} ist im Layout nicht vorhanden.',
                [GetRackPosTypeName(aRackNameType), vRackName]));

        vRackName := xPosArr[0].Rack;
        EXIT(xPosArr[0].Pos);
    end
    else
    begin
        xRackPos := InternGetRackPosition(aRackNameType, vRackName, aRackFirstPos, aRackLastPos, aPosCounter);
    end;
    vRackName := xRackPos.Rack;
    EXIT(xRackPos.Pos);
end;

function TRunStepBuilderHelper.GetRackPositionNumber(aRackNameType: TMethodRackNameType;
    var vRackName: string; const aRackPos: string; const aVol: extended): integer;
begin
    EXIT(self.GetRackPositionNumber(aRackNameType, vRackName, aRackPos, aRackPos, 1, aVol));
end;

function TRunStepBuilderHelper.GetRackPositionNumber(aRackNameType: TMethodRackNameType;
    var vRackName: string; const aRackFirstPos, aRackLastPos: string; aPosCounter: integer): integer;
begin
    EXIT(self.GetRackPositionNumber(aRackNameType, vRackName, aRackFirstPos, aRackLastPos, aPosCounter, 0));
end;

function TRunStepBuilderHelper.GetRackPositionNumber(aRackNameType: TMethodRackNameType;
    var vRackName: string; const aRackPos: string): integer;
begin
    EXIT(self.GetRackPositionNumber(aRackNameType, vRackName, aRackPos, aRackPos, 1, 0));
end;

function TRunStepBuilderHelper.GetRackPositionNumberNoIDReplace(aRackNameType: TMethodRackNameType;
    var vRackName: string; const aRackFirstPos, aRackLastPos: string; aPosCounter: integer): integer;
var
    xRackPos: TRackPosition;
    xPosAsInt: integer;
begin
    // Sonderfall: Position = -1 heißt, dass vRackName als TubeID verwendet wird!
    xPosAsInt := StrToIntDef(aRackFirstPos, 0);
    if (xPosAsInt = INT_RACKPOS_RACKNAME_IS_TUBEID) then
    begin
        EXIT(-1);
    end
    else
    begin
        xRackPos := InternGetRackPosition(aRackNameType, vRackName, aRackFirstPos, aRackLastPos, aPosCounter);
    end;
    vRackName := xRackPos.Rack;
    EXIT(xRackPos.Pos);
end;

function TRunStepBuilderHelper.GetRackPositionNumberNoIDReplace(aRackNameType: TMethodRackNameType;
    var vRackName: string; const aRackPos: string): integer;
begin
    EXIT(self.GetRackPositionNumberNoIDReplace(aRackNameType, vRackName, aRackPos, aRackPos, 1));
end;

function TRunStepBuilderHelper.GetLiqHElement(const aName: string): TLiqHCacheElement;
begin
    // wenn es so gewünscht ist: bei jedem Schritt die Liquid-Handling-Daten neu laden (zum Tricksen)
    if fReloadLiqHParamsForEachStep then
        fLiqHCache.ReloadLiqHData(aName);

    result := fLiqHCache.FindByName(aName);
    if not Assigned(result) then
    begin
        raise Exception.Create(TLanguageString.Read('Liquid handling parameter {0} not found!',
            'Liquid Handling Parameter {0} nicht gefunden!', [aName]));
    end;
end;


end.
