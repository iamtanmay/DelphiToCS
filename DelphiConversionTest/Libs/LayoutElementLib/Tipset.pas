{ --------------------------------------------------------------------------------------------------
  Copyright © 2008 Zinsser Analytic GmbH - All rights reserved.
  Author       : Payman Kamali (pk)
  Description  : Classes and math functions for Coordinate System Relation
  --------------------------------------------------------------------------------------------------
  Revision History:
  date     op  method                       track-no improvement/change
  -------- --  ---------------------------  -------- -----------------------------------------------
  ObjArm:
  06.09.99 wl                           neue Unit: TTip, TArm
  07.09.99 wl                           Objekte stark erweitert
  09.09.99 wl  TArm.CreateArm           Numerierung der Tips berichtigt
  10.09.99 wl  TTip.CreateTip           MaxVolume wird bei Vol=0 auf SyrVol gesetzt
  16.09.99 wl  TArm.SetUseTipsByName    alle Tips des Typs ... werden gesetzt
  TArm.GetTipNames         man erhält alle TipType-Namen von best. Typen
  TArm.InitSyringes        Init-Funktion (aus sampler.dll) + Flush aus BasicThr
  20.09.99 wl  TTip.LoadRediDevices     dem Tip werden eigene Devices (Pump,Vacuum,Blower) zugeordnet
  TArm.SetUseTips          Zuordnung geändert
  24.09.99 wl  TArm.InitSyringes        --> BasicThr
  27.09.99 wl  TTip[i].DLT              Nummer des zu diesem Tip gehörenden Dilutors
  29.09.99 wl  TArm.SetUseTips          Verbesserte Tip-Vergabe
  01.10.99 wl  TArm.SetUseTipsByName    entfernt - in erweitertes SetUseTips integriert
  TArm.GetTipNames         korrigiert
  04.10.99 wl  TArm.SetUseTips          aus den gewählten Tips wird der GlobalZOffset bestimmt
  TArm.PickDTips,DropDTips,DropAllDTips  Führt _Get-/_DropTips aus und bestimmt DropTipMap
  08.12.99 mo  CreateTip                Sam.Diluter[FTipNo-1].SyrVol;
  12.01.00 wl  alle Funktionen        LogText() durch gmLogText() ersetzt (für Delphi 5)
  11.02.00 wl  TTip                     jedem Tip kann ein VolMotor:TVarRediMotor zugeordnet werden
  TArm.InitRediMotors      Initialisieren der Redi-Motoren
  TArm.ResetRediMotors     Reset der Redi-Motoren
  TArm.SetVarRediVolumes   Stellt Volumen für alle Redi-Motoren ein
  14.02.00 wl  TArm.LoadTips            Funktion von CreateArm getrennt (für Layouter)
  17.02.00 wl  TArm.ResetRediMotors     überflüssig -> weg
  21.02.00 wl  TArm.LoadTips            SetRange statt FindKey: Tips wurden bisher zum Teil von falschen Layouts geladen
  22.02.00 wl  TArm./TTip.CreateTip     übergibt TipData statt TipName -> Fehler fallen bereits in TArm auf
  22.02.00 wl  TArm.CreateTips          erzeugt alle Tips neu (nach einer Änderung)
  25.02.00 mo  TArm                     neu FNoOfTips = property NoOfTips = Anzahl der benutzbaren Tips
  25.02.00 mo  TArm                     neu FLastTip = property LastTip = Letzter benutzbarer Tip
  25.02.00 mo  TArm.GetTipNumber        neu ermittelt den wirklichen Tip für einen Tipcounter 0..MAX_TIPS
  01.03.00 wl  TArm.SetVarRediVolumes   Hardcore-Einstellung für 5.Nadel am Handler
  21.03.00 mo  TArm.SetUseTips          korrigiert
  03.04.00 mo  TArm.DropAllDTips        korrigiert
  03.04.00 mo  TArm.DropDTips           korrigiert
  04.04.00 mo  TArm.SetUseTips          Berechnung GlobalZOffset korrigiert
  12.04.00 wl                           uses SerObj statt ObjRelay
  26.05.00 wl                         uses geändert
  31.05.00 wl  TArm.DropDTips,PickDTips,DropAllDTips --> BasicLTh (um uses SamIntf zu entfernen)
  06.06.00 wl  TArm.FDispTipUsed        überflüssige Variable gelöscht
  05.07.00 wl  TTip.LoadRediDevices     --> ObjModul
  17.07.00 wl  TArm.InitRediMotors,SetVarRediVolumes  --> BasicLTh (Arm-Objekt soll nicht aktiv auf serielle Objekte zugreifen!)
  25.07.00 wl  TArm.SetUseTips          korrigiert: bei der 2.Pipettierung wird der DTOffset vergessen
  03.08.00 wl                         --> String in Ressourcen  (263..)
  29.08.00 wl  TTip.CreateTip           Daten werden in LogFile geschrieben
  13.09.00 wl  TArm.GetDispTipUsed      result wird mit false initialisiert
  06.06.01 tbh TArm.SetUseTips          TipTypes setzen XOffset, YOffset
  27.10.01 tbh TArm.FUseTipData         TN1050 FUseTipData jetzt public
  12.11.01 tbh TArm.ChangeTip           TN1092 neu: schreibt Tip um falls LiqHandlParameter neuen Tiptyp setzt
  12.11.01 tbh TArm.SetUseTips          TN1092 Tip wird umgeschrieben falls LiqHandlParameter neuen Typ setzt
  15.11.01 tbh TArm.ChangeTip           TN1092 Tip wird für Umschreiben neu erzeugt
  23.11.01 tbh TArm.SetUseTips          TN1112 TArm.ChangeTip wird nur aufgerufen wenn gChangeTipTypes=true
  06.12.01 tbh TTip.CreateTip           TN1137 Redi-Devices werden jetzt auch für RemRediTip und VarrediTips geladen
  07.12.01 tbh TArm.ChangeTip           TN1138 an Tip.Create übergebener Tip korrigiert
  08.12.01 tbh TArm.SetUseTips          TN1144 Parameter OverwriteTipTypes legt fest ob TipTypes überschrieben werden
  28.12.01 tbh TArm                     TN1051 neue Daten/Properties FOriginalTips/TipConfig/TipsToUse für CALLIW
  28.12.01 tbh TArm.SetUseTips          TN1051 X- bzw- YOffset eines TipTypes werden korrekt verrechnet
  05.02.02 tbh TArm.ResetTipTypes       TN1163 neu: setzt Tip-Konfiguration auf urspr. Layout Tipset zurück
  06.02.02 tbh TArm.ActualTips          TN1051 neue Property zum Auslesen der aktuell gesetzten Tips
  03.06.02 tbh TArm.GetBasicTipType     TN1223 neu: liefert BasisTipTyp für angebenen TipTypNamen
  10.10.02 wl                               TN1293.2 Ini Access - uses geändert
  16.10.02 wl                               TN1293.2 High(gTipTypes) statt cMaxTipTypes
  20.12.02 wl                               TN1293.5 uses und WinlissyIniAccess geändert
  27.12.02 wl                               TN1293.5 uses Device: TDevice statt TDevicePart
  28.01.03 wl                               TN1293.5 geänderte gModules-Methodenaufrufe
  05.02.03 wl  TTip.CreateTip               TN1295.5 Exception abgefangen wenn FRediDev.VolMotor = nil
  12.03.03 wl                               TN1293.5 uses posTools
  09.05.03 wl  TArm.GetDispTipUsed      TN1490 entfernt (durch TDitiObserver überflüssig)
  03.07.03 wl                           TN1501 uses DeviceInterfaces
  18.12.03 wl                               TN1672   uses geändert
  17.02.04 pk                           TN1749 All controls set to invisible to start and then changed to visible when needed.
  19.03.04 wl  TArm.TipsToUse,...       TN1788  suboptimale properties entfernt
  19.03.04 wl  TTip.GetHandlerTip       TN1788  erstzt durch gTipManager.GetArmIndex
  05.04.04 wl  TTip.Data,No,DLT,RediDev     TN1788   --> DevicesTips
  05.04.04 wl  TArm.UseTips,UseTipCount,DropTipMap,UseTipData     TN1788   --> DevicesArms
  05.04.04 wl  TArm.SetUseTips              TN1788   umbenannt in FindArmToUse, Inhalt --> TipManager
  05.04.04 wl  TArm.ActualTips,OriginalTips TN1788   sinnloser Mechanismus - entfernt
  05.04.04 wl  TArm,TTip                    TN1788   umbenannt in TArmPanel und TTipPanel (nur noch GUI und Datenbankzugriff)
  05.04.04 wl  MakeHint                     TN1788.4 nach ChangeTipType wird auch der Hint geändert!
  05.04.04 wl  TArmPanel.GetTipNo           TN1788   --> DevicesArms
  20.04.04 wl  TArmPanel.FindArmToUse       TN1788   entfernt (muß über gTipManager aufgerufen werden)
  20.04.04 wl  TArmPanel.ShowUseTips        TN1788   neu: wird von TRobotArmDevice aus aufgerufen
  28.04.04 wl  TTipPanel.Create             TN1788   benutzt TCustomArmDevice
  03.06.05 wl  TArmPanel.LoadTips           TN2436   Sam.nTips ersetzt durch gTipManager.FirstPipArmTipCount
  15.06.05 pk  TTipPanel                    TN2464   fields accessible via properties
  21.06.05 pk  TArmPanel.ShowUseTips        TN2464.3 show the most recently changed tips in Red and all other used tips in gray
  24.06.05 pk  TTipPanel.Create             TN2475   Calls gTipManager.InitTipType instead of Arm.InitTipType
  07.09.05 wl  TArmPanel.Create             TN2558.9 in Delphi 7 muß ParentBackground gesetzt sein
  25.01.06 pk  LoadTips                     TN2900   check if piparm exists before accessing tips
  08.03.07 wl                               TN3620   uses geändert
  ---------------------------------------------------------------------------------------------------
  20.06.08 pk                               TN4139   References to TPanel removed.  Graphics information moved to TipsetGraphics
  23.06.08 pk                                TN4139  Initial Revision
  03.07.08 wl                                TN4157
  09.07.08 pk  fIsLinked                     TN4157  New
  11.07.08 wl  ShowUseTips                  TN4164   Parameter geändert
  17.09.08 wl                               TN4224  Tipset und TipsetExt wieder vereinigt
  10.08.09 wl                               TN4702   Strings werden jetzt direkt geladen
  04.11.09 pk                               TN4843   Replace TObjectList, TStringList, TStrings with Generics Lists and TStringArray
  04.15.10 pk                               TN5050   various changes
  21.04.10 wl  LoadTipTypeNames             TN5069   Die Länge des Arrays entspricht jetzt immer der Anzahl der möglichen Tips
  23.04.10 pk  GetHintText                  TN5050   TipIndex + 1
  07.06.10 pk  LoadTipTypeNames             TN5077   Removed, load now done in TLayout
  17.06.10 wl  OnInitTipset                 TN5150   kann als property gesetzt werden, wird nicht bei Prepare übergeben
  10.04.13 wl                               TN6045   uses geändert
  -------------------------------------------------------------------------------------------------- }

unit Tipset;


interface


uses
    GeneralTypes,
    Generics.Collections,
    AppTypes,
    CommonTypes,
    TipsetDataAdaptor,
    TipsetGraphics;

const
    cTipHeight = 5;
    cTipWidth = 15;

type
    TTipsetInitTipEvent = procedure(const aPipDeviceName: string; const aTipTypeNames: TStringArray)
        of object;

    TTipsetTip = class
    private
        fTypeName: string;
        procedure CreateGraphics;
        procedure SetTypeName(const aValue: string);
    protected
        fPipDeviceName: string;
        fLayoutName: string;
        fTipIndex: integer;
        fTipset: TObject;
        fGraphics: TTipsetTipGraphics;
        procedure DoInitGraphics(); virtual;
    public
        // constructor
        constructor Create();
        //
        procedure Prepare(const aLayoutName, aDeviceName: string; aTipIndex: integer);
        procedure InitGraphics(aGraphicsParent: TObject);
        procedure SetTipSelected(aIsSelected: boolean);
        procedure RefreshTypeName;
        procedure ChangeVisible(aVisible: boolean);
        //
        property TipIndex: integer read fTipIndex;
        property TypeName: string read fTypeName write SetTypeName;
        property Tipset: TObject read fTipset write fTipset;
        property PipDeviceName: string read fPipDeviceName;
    end;

    TTipsetDeviceRec = record
        DeviceName: string;
        NoOfTips: integer;
    end;

    TTipsetDeviceRecArray = array of TTipsetDeviceRec;

    TTipsetDevice = class
    private
        fTips: TObjectList<TTipsetTip>;
        fIsLinked: boolean;
        fDeviceName: string;
        fLayoutName: string;
        fPossibleTipCount: integer;
        fOnInitTipset: TTipsetInitTipEvent;

        procedure DetermineGraphicsBounds;
        procedure CreateGraphics;
        function CreateTip(const aLayoutName, aDeviceName: string; aTipIndex: integer): TTipsetTip;
    protected
        fGraphics: TTipsetGraphics;
        procedure DoInitGraphics(); virtual;
        function DoCreateTip(): TTipsetTip; virtual;
    public
        constructor Create();
        destructor Destroy(); override;
        procedure Prepare(const aLayoutName: string; const aDeviceRec: TTipsetDeviceRec);
        procedure ChangeVisible(aVisible: boolean);
        procedure InitGraphics(aGraphicsParent: TObject);
        procedure TipTypesChanged();
        procedure ChangeTipType(const aTipIndex: integer; const aTipTypeName: string);
        function GetHintText(): string;
        property IsLinked: boolean read fIsLinked write fIsLinked;
        property Tips: TObjectList<TTipsetTip>read fTips;
        property DeviceName: string read fDeviceName;
        property OnInitTipset: TTipsetInitTipEvent read fOnInitTipset write fOnInitTipset;
    end;


implementation


uses
    SysUtils,
    TipTypeDataAdaptor;

{ TTipsetTip }

constructor TTipsetTip.Create();
begin
    inherited Create();
end;

procedure TTipsetTip.Prepare(const aLayoutName, aDeviceName: string; aTipIndex: integer);
begin
    fTipIndex := aTipIndex;
    ASSERT(fTipIndex >= 0, 'TipIndex < 0');
    fLayoutName := aLayoutName;
    fPipDeviceName := aDeviceName;
    fTypeName := '';
end;

procedure TTipsetTip.CreateGraphics();
begin
    fGraphics := TTipsetTipGraphics.Create();
end;

procedure TTipsetTip.DoInitGraphics();
begin
    fGraphics.SetGraphicsInfo(cTipWidth, cTipHeight, fPipDeviceName, fTipIndex);
end;

procedure TTipsetTip.InitGraphics(aGraphicsParent: TObject);
begin
    CreateGraphics();
    fGraphics.SetGraphicsParent(aGraphicsParent);
    DoInitGraphics();
end;

procedure TTipsetTip.SetTipSelected(aIsSelected: boolean);
begin
    fGraphics.SetTipSelected(aIsSelected);
end;

procedure TTipsetTip.ChangeVisible(aVisible: boolean);
begin
    fGraphics.Visible := aVisible;
end;

procedure TTipsetTip.RefreshTypeName;
// var xArm: IPipDevice;
begin
    {
      xArm := gPipDeviceManager.FindPipDevice_ByName( fPipDeviceName );
      if Assigned(xArm) then
      FTypeName := xArm.Tips[FArmTipIndex].TypeName;
    }
end;

procedure TTipsetTip.SetTypeName(const aValue: string);
begin
    fTypeName := aValue;
    // 04.13.10 pk event removed here
end;

{ TTipset }

constructor TTipsetDevice.Create();
begin
    inherited Create();

    fTips := TObjectList<TTipsetTip>.Create(true); // Objekte sollen mit zerstört werden!!
end;

destructor TTipsetDevice.Destroy;
begin
    FreeAndNil(fTips);

    inherited;
end;

procedure TTipsetDevice.TipTypesChanged();
var
    x: integer;
    xTipTypeNames: TStringArray;
begin

    SetLength(xTipTypeNames, fPossibleTipCount);
    for x := 0 to Length(xTipTypeNames) - 1 do
    begin
        xTipTypeNames[x] := fTips[x].TypeName;
    end;

    if Assigned(fOnInitTipset) then
        fOnInitTipset(fDeviceName, xTipTypeNames);
end;

procedure TTipsetDevice.Prepare(const aLayoutName: string; const aDeviceRec: TTipsetDeviceRec);
var
    x: integer;
    xTip: TTipsetTip;
begin
    fDeviceName := aDeviceRec.DeviceName;
    fPossibleTipCount := aDeviceRec.NoOfTips;
    fLayoutName := aLayoutName;

    DetermineGraphicsBounds();

    for x := 0 to fPossibleTipCount - 1 do
    begin
        xTip := CreateTip(fLayoutName, fDeviceName, x);
        fTips.Add(xTip);
    end;
end;

procedure TTipsetDevice.ChangeTipType(const aTipIndex: integer; const aTipTypeName: string);
begin
    if (aTipIndex < 0) or (aTipIndex >= fPossibleTipCount) then
        EXIT;
    fTips[aTipIndex].TypeName := aTipTypeName;

end;

procedure TTipsetDevice.ChangeVisible(aVisible: boolean);
var
    x: integer;
begin
    if not Assigned(fGraphics) then
        EXIT;
    fGraphics.Visible := aVisible;
    for x := 0 to fTips.Count - 1 do
    begin
        try
            fTips[x].ChangeVisible(aVisible)
        except
        end;
    end;
end;

procedure TTipsetDevice.CreateGraphics();
begin
    fGraphics := TTipsetGraphics.Create();
end;

procedure TTipsetDevice.DoInitGraphics();
begin
end;

procedure TTipsetDevice.InitGraphics(aGraphicsParent: TObject);
begin
    CreateGraphics();
    DoInitGraphics();
end;

function TTipsetDevice.CreateTip(const aLayoutName, aDeviceName: string; aTipIndex: integer): TTipsetTip;
begin
    result := DoCreateTip();
    result.Prepare(aLayoutName, aDeviceName, aTipIndex);
    result.Tipset := self;
    result.InitGraphics(fGraphics);
end;

function TTipsetDevice.GetHintText(): string;
const
    cIndent = '    ';
var
    x: integer;
begin
    result := cIndent + fDeviceName + ':';
    for x := 0 to fTips.Count - 1 do
    begin
        if result <> '' then
            result := result + #13#10;
        result := result + cIndent + cIndent + Format('Tip %d: %s',
            [fTips[x].TipIndex + 1, fTips[x].TypeName]);
    end;
end;

procedure TTipsetDevice.DetermineGraphicsBounds();
begin
    fGraphics.SetGraphicsInfo(cTipWidth, cTipHeight, fPossibleTipCount, 1);
end;

function TTipsetDevice.DoCreateTip(): TTipsetTip;
begin
    result := TTipsetTip.Create();
end;


end.
