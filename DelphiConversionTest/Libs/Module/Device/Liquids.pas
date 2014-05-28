{ -----------------------------------------------------------------------------------------------------------------------
  Copyright © 2009 Zinsser Analytic GmbH - All rights reserved.
  Author       : Wolfgang Lyncke (wl), Payman Kamali (pk)
  Description  :
  -----------------------------------------------------------------------------------------------------------------------
  Revision History:
  date     op  method                                track-no  improvement/change
  -------- --  ------------------------------------  --------- ----------------------------------------------------------
  27.08.09 pk                                        TN4753   reference to runflow removed
  27.08.09 pk  ChangeVolume                          TN4753   must be reimplemented
  28.08.09 pk                                        TN4753   code moved from LiquidsBasic here
  28.08.09 pk  gLiquids                              TN4753     instance variable moved here from LiquidManager
  04.11.09 pk                                        TN4843   Replace TObjectList, TStringList, TStrings with Generics Lists and TStringArray
  18.06.12 wl                                        TN5899   Liquids und LiquidsBasic zusammengefasst
  18.06.12 wl  TSystemLiquid.Create                  TN5899   SysLiqMinVol wird hier gelesen
  05.11.12 wl  TLiquids.WriteAllVolumes              TN6006   in der Simulation wird nichts geschrieben
  05.11.12 wl  TLiquid.ChangeVolumeAbsolute          TN6006   in der Simulation wird nichts geschrieben
  24.04.13 wl  TLiquids.FindDiluentIndex             TN6137   ersetzt GetDilNo, kann Exception zeigen, gibt Index statt Nummer zurück
  24.04.13 wl  En-/DecodeSystemLiquidIdent           TN6137   --> LiquidManager
  ----------------------------------------------------------------------------------------------------------------------- }

unit Liquids;


interface


uses
    Classes,
    SyncObjs,
    CommonTypes,
    Generics.Collections;

type
    TLiquid = class abstract
    strict private
        fCriticalSection: TCriticalSection;
        fOnChangeVolume: TNotifyEvent;
        fVolume_uL: double;
        fVolumeReal_uL: double;
        function GetVolume_mL: integer;
    strict protected
        function GetVolumeIdentName: string; virtual; abstract;
        procedure ChangeVolumeRelative(aVolume_uL: double; aIsSimulated: boolean);
    public const
        cIniSectionLiquids = 'Liquids';
    public
        constructor Create();
        destructor Destroy; override;

        class function GetVolumeMlFromUl(aVolume_uL: double): integer;
        procedure WriteVolumeToIni(aIniAccess: IWinlissyIniAccess);
        procedure ChangeVolumeAbsolute(aVolume_uL: double; aIsSimulated: boolean);
        procedure ResetVolumeToRealValue();
        procedure ReadVolumeFromIni(aIniAccess: IWinlissyIniAccess);

        property Volume_mL: integer read GetVolume_mL;
        property Volume_uL: double read fVolume_uL write fVolume_uL;
        property OnChangeVolume: TNotifyEvent read fOnChangeVolume write fOnChangeVolume;
    end;

    TSystemLiquid = class(TLiquid)
    strict private
        FAspSpeed: integer;
        FDiluent: string;
        FPortName: string;
        fMinVolume_mL: integer;
        fDilIndex: integer;
        procedure SetDiluentName(aName: string);
    strict protected
        function GetVolumeIdentName: string; override;
    public
        constructor Create(aPortName: string; aIndex: integer);

        procedure TakeVolume(aVolume_uL: double; aIsSimulated: boolean);

        property DilIndex: integer read fDilIndex;
        property PortName: string read fPortName;
        property AspSpeed: integer read FAspSpeed;
        property Diluent: string read FDiluent;
        property MinVolume_mL: integer read fMinVolume_mL;
    end;

    TWasteLiquid = class(TLiquid)
    strict private
        fMaxVolume_uL: double;
        function GetMaxVolume_mL: integer;
    strict protected
        function GetVolumeIdentName: string; override;
    public
        constructor Create();

        procedure AddVolume(aVolume_uL: double; aIsSimulated: boolean);

        property MaxVolume_mL: integer read GetMaxVolume_mL;
    end;

    TLiquids = class sealed // Singleton Instance => sealed
    strict private
        fWasteLiquid: TWasteLiquid;
        fSystemLiquids: TObjectList<TSystemLiquid>;
        class var uInstance: TLiquids;
        class function GetNumberOfPorts: integer;
    public
        constructor Create;
        destructor Destroy; override;

        class procedure CreateInstance;
        class procedure DestroyInstance;

        function GetAllSystemLiquidNames(): TArray<string>;
        procedure SetEvents(aOnChangeSysVol, aOnChangeWasteVol: TNotifyEvent);
        procedure ResetEvents;
        procedure WriteAllVolumes(aIsSimulated: boolean);
        procedure ResetVolumesToRealValues();
        function FindDiluentIndex(const aDiluentName: string; aMustFind: boolean): integer;
        function FindSystemLiquid(aIndex: integer): TSystemLiquid;

        property SystemLiquids: TObjectList<TSystemLiquid>read fSystemLiquids;
        property WasteLiquid: TWasteLiquid read FWasteLiquid;

        class property Instance: TLiquids read uInstance write uInstance;
    end;


implementation


uses
    SysUtils,
    GeneralTypes,
    AppSettings,
    AppTypes,
    SamGlobe;

{ TLiquid }

constructor TLiquid.Create;
begin
    inherited;
    fCriticalSection := TCriticalSection.Create;
end;

destructor TLiquid.Destroy;
begin
    FreeAndNil(fCriticalSection);
    inherited;
end;

class function TLiquid.GetVolumeMlFromUl(aVolume_uL: double): integer;
begin
    EXIT(Round(aVolume_uL / 1000));
end;

function TLiquid.GetVolume_mL: integer;
begin
    EXIT(GetVolumeMlFromUl(fVolume_uL));
end;

procedure TLiquid.ReadVolumeFromIni(aIniAccess: IWinlissyIniAccess);
begin
    // Waste-Liquid-Daten lesen
    fVolumeReal_uL := 1000 * aIniAccess.ReadInteger(cIniSectionLiquids, self.GetVolumeIdentName);
    fVolume_uL := fVolumeReal_uL;
end;

procedure TLiquid.ResetVolumeToRealValue;
begin
    fCriticalSection.Enter;
    try
        fVolume_uL := fVolumeReal_uL;

        if Assigned(fOnChangeVolume) then
            fOnChangeVolume(self);
    finally
        fCriticalSection.Leave;
    end;
end;

procedure TLiquid.WriteVolumeToIni(aIniAccess: IWinlissyIniAccess);
begin
    aIniAccess.WriteInteger(cIniSectionLiquids, self.GetVolumeIdentName, GetVolumeMlFromUl(fVolumeReal_uL));
end;

procedure TLiquid.ChangeVolumeRelative(aVolume_uL: double; aIsSimulated: boolean);
begin
    fCriticalSection.Enter;
    try
        if (not aIsSimulated) then
            fVolumeReal_uL := fVolumeReal_uL + aVolume_uL;

        fVolume_uL := fVolume_uL + aVolume_uL;

        if Assigned(fOnChangeVolume) then
            fOnChangeVolume(self);
    finally
        fCriticalSection.Leave;
    end;
end;

procedure TLiquid.ChangeVolumeAbsolute(aVolume_uL: double; aIsSimulated: boolean);
var
    xIniAccess: IWinlissyIniAccess;
begin
    fCriticalSection.Enter;
    try
        // Der "echte Wert" wird geändert!
        fVolumeReal_uL := aVolume_uL;

        fVolume_uL := fVolumeReal_uL;

        // Absolute Änderungen werden sofort gespeichert!
        if not aIsSimulated then
        begin
            xIniAccess := gCommonDll.CreateAppIni;
            self.WriteVolumeToIni(xIniAccess);
            xIniAccess.WriteSectionFromCache(cIniSectionLiquids);
        end;

        if Assigned(fOnChangeVolume) then
            fOnChangeVolume(self);
    finally
        fCriticalSection.Leave;
    end;
end;

{ TSystemLiquid }

constructor TSystemLiquid.Create(aPortName: string; aIndex: integer);
var
    xIniAccess: IWinlissyIniAccess;
    xLiquidPort: TLiquidPortData;
begin
    inherited Create;

    fPortName := aPortName;
    fDilIndex := aIndex;

    xIniAccess := gCommonDll.CreateAppIni;
    xLiquidPort := xIniAccess.ReadLiquidPortData(STR_ISEC_SYSLIQVALVE, aPortName);

    SetDiluentName(xLiquidPort.DilName);
    FAspSpeed := xLiquidPort.AspSpeed;

    // define minimum volume for volume control
    fMinVolume_mL := xIniAccess.ReadInteger(cIniSectionLiquids, aPortName + 'MinVol');
    if (fMinVolume_mL = 0) then // allgemeines Minimum lesen
        fMinVolume_mL := xIniAccess.ReadInteger(cIniSectionLiquids, 'SysLiqMinVol');

    ReadVolumeFromIni(xIniAccess);
end;

procedure TSystemLiquid.SetDiluentName(aName: string);
var
    x: integer;
begin
    // dem Kind einen (Dummy-)Namen geben
    if (aName = '') or (aName = '--') then
        aName := '->' + IntToStr(fDilIndex + 1);

    // kürzen auf 20 Buchstaben
    if Length(aName) > 20 then
    begin
        x := (Length(aName)) - 20;
        Delete(aName, 21, x);
    end;

    FDiluent := aName;
end;

function TSystemLiquid.GetVolumeIdentName: string;
begin
    EXIT(fPortName + 'Vol');
end;

procedure TSystemLiquid.TakeVolume(aVolume_uL: double; aIsSimulated: boolean);
begin
    ChangeVolumeRelative(-aVolume_uL, aIsSimulated);
end;

{ TWasteLiquid }

constructor TWasteLiquid.Create();
var
    xIniAccess: IWinlissyIniAccess;
begin
    inherited Create;

    xIniAccess := gCommonDll.CreateAppIni;
    ReadVolumeFromIni(xIniAccess);
    FMaxVolume_uL := 1000 * xIniAccess.ReadInteger(cIniSectionLiquids, 'WasteMaxVol');
end;

function TWasteLiquid.GetMaxVolume_mL: integer;
begin
    EXIT(GetVolumeMlFromUl(fMaxVolume_uL));
end;

function TWasteLiquid.GetVolumeIdentName: string;
begin
    EXIT('WasteVol');
end;

procedure TWasteLiquid.AddVolume(aVolume_uL: double; aIsSimulated: boolean);
begin
    ChangeVolumeRelative(aVolume_uL, aIsSimulated);
end;

{ TLiquids }

constructor TLiquids.Create;
var
    x: integer;
    xNoOfPorts: integer;
begin
    inherited Create;

    FWasteLiquid := TWasteLiquid.Create;

    fSystemLiquids := TObjectList<TSystemLiquid>.Create(true);
    xNoOfPorts := self.GetNumberOfPorts();
    for x := 1 to xNoOfPorts do
        fSystemLiquids.Add(TSystemLiquid.Create('Port' + IntToStr(x), x - 1));
end;

destructor TLiquids.Destroy;
begin
    fWasteLiquid.Free;
    fSystemLiquids.Free;

    inherited;
end;

class procedure TLiquids.DestroyInstance;
begin
    FreeAndNil(uInstance);
end;

class procedure TLiquids.CreateInstance;
begin
    uInstance := TLiquids.Create;
end;

class function TLiquids.GetNumberOfPorts: integer;
const
    cNumberOfUndefinedPorts = 5;
var
    x: integer;
    xIniAccess: IWinlissyIniAccess;
begin
    result := 1; // einen Port gibt es mindestens!

    // höchsten Port ermitteln (es dürfen maximal 5 Lücken davor sein)
    xIniAccess := TAppSettings.CreateAppIni;
    x := 1;
    while (x - result <= cNumberOfUndefinedPorts) do
    begin
        if xIniAccess.ValueExists(STR_ISEC_SYSLIQVALVE, 'Port' + IntToStr(x)) then
            result := x;

        inc(x);
    end;
end;

function TLiquids.FindSystemLiquid(aIndex: integer): TSystemLiquid;
begin
    result := nil;
    if (aIndex < 0) then
        EXIT;

    if (aIndex >= fSystemLiquids.Count) then
    begin
        raise Exception.CreateFmt('System Liquid %d undefined', [aIndex + 1]); // darf nicht sein
        EXIT;
    end;
    result := FSystemLiquids[aIndex];
end;

function TLiquids.FindDiluentIndex(const aDiluentName: string; aMustFind: boolean): integer;
var
    x: integer;
begin
    // Spezialfall: Es gibt nur ein Diluent - dann nimm dieses!
    if (aDiluentName = '') and (fSystemLiquids.Count = 1) then
        EXIT(0);

    for x := 0 to fSystemLiquids.Count - 1 do
        if (aDiluentName = fSystemLiquids[x].Diluent) then
            EXIT(x);

    if (aMustFind) then
        raise Exception.Create(TLanguageString.Read('Diluent {0} unknown.', 'Lösungsmittel {0} unbekannt.',
            [aDiluentName]));

    EXIT(-2); // bleibt negativ, auch wenn es für PipetteList um eine Zahl erhöht wird
end;

function TLiquids.GetAllSystemLiquidNames(): TArray<string>;
var
    x: integer;
    xDiluentName: string;
    xList: TList<string>;
begin
    xList := TList<string>.Create();
    try
        for x := 0 to fSystemLiquids.Count - 1 do
        begin
            xDiluentName := (fSystemLiquids[x] as TSystemLiquid).Diluent;
            if (Copy(xDiluentName, 1, 2) <> '->') then
                xList.Add(xDiluentName);
        end;
        result := xList.ToArray();
    finally
        FreeAndNil(xList);
    end;
end;

procedure TLiquids.SetEvents(aOnChangeSysVol, aOnChangeWasteVol: TNotifyEvent);
var
    x: integer;
begin
    fWasteLiquid.OnChangeVolume := aOnChangeWasteVol;
    for x := 0 to fSystemLiquids.Count - 1 do
        fSystemLiquids[x].OnChangeVolume := aOnChangeSysVol;
end;

procedure TLiquids.WriteAllVolumes(aIsSimulated: boolean);
var
    x: integer;
    xIniAccess: IWinlissyIniAccess;
begin
    if aIsSimulated then
        EXIT;

    xIniAccess := TAppSettings.CreateAppIni;

    fWasteLiquid.WriteVolumeToIni(xIniAccess);
    for x := 0 to fSystemLiquids.Count - 1 do
        fSystemLiquids[x].WriteVolumeToIni(xIniAccess);

    xIniAccess.WriteSectionFromCache(TLiquid.cIniSectionLiquids);
end;

procedure TLiquids.ResetEvents;
var
    x: integer;
begin
    fWasteLiquid.OnChangeVolume := nil;
    for x := 0 to fSystemLiquids.Count - 1 do
        fSystemLiquids[x].OnChangeVolume := nil;
end;

procedure TLiquids.ResetVolumesToRealValues;
var
    x: integer;
begin
    fWasteLiquid.ResetVolumeToRealValue;
    for x := 0 to fSystemLiquids.Count - 1 do
        fSystemLiquids[x].ResetVolumeToRealValue;
end;


end.
