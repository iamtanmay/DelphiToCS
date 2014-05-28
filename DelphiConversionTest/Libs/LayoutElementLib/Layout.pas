{ --------------------------------------------------------------------------------------------------
  Copyright © 2008 Zinsser Analytic GmbH - All rights reserved.
  Author       : Payman Kamali (pk)
  Description  :
  --------------------------------------------------------------------------------------------------
  Revision History:
  date     op  method                       track-no improvement/change
  -------- --  ---------------------------  -------- -----------------------------------------------
  23.06.08 pk                                TN4139  Initial Revision
  27.06.08 pk                                TN4139  GetBalancePos moved from Layout to ZARunnerLayout
  27.06.08 pk  fTypeDataCache                TN4139  New.  Workspace, Carrier and Racktypes now read from cache instead of directly from db
  27.06.08 pk  AddCarrier                    TN4139  use gReverseY setting
  02.07.08 pk                                TN4139  various changes
  07.07.08 pk  LoadTipset                    TN4139  called for each linked layout as well
  09.07.08 pk                                TN4139  various changes
  16.07.08 pk                                TN4139  changes needed for reading racks from runlayout
  16.07.08 wl  GetUsedDevices                TN4164   replaces GetUsedArms
  21.07.08 pk  ChangeCarrierType             TN4179  New
  30.07.08 pk  LoadRun                       TN4139  New
  31.07.08 pk  UpdateRackRunData             TN4139  now only an empty virtual method, code moved
  31.07.08 pk  ChangeRackType                TN4193  New
  02.09.08 pk  GetBalancePos                 TN4215  New
  17.09.08 wl  fTipsets                      TN4224  Tipset-Implementierung überarbeitet
  19.09.08 pk  SetBalanceType                TN4215  As virtual function
  16.03.09 pk  CheckCarriers                 TN4472  New
  04.08.09 ts  TLayout.AddWorkspace          TN4569  fTypeDataCache.Refresh eingefügt, damit neu angelegte Workspaces direkt eingefügt werden können
  10.08.09 wl                                TN4702   Strings werden jetzt direkt geladen
  12.09.09 wl                                TN4740   an Änderungen in DiTiObserver angepasst
  04.11.09 pk                                TN4843  Replace TObjectList, TStringList, TStrings with Generics Lists and TStringArray
  11.11.09 pk                                TN4830  LayoutDataCache made public
  25.11.09 pk  GetCarrierNames               TN4843  used wrong array length
  04.15.10 pk                                TN5050  various changes for showing Tipset in Hint
  07.06.10 pk  StoreRacks/Carriers           TN5077  New
  07.06.10 pk  LoadTipsetDevices             TN5077  Adds tips to TipsetDevice using TipsetCache Items
  07.06.10 pk                                TN5077  LoadGeneral changed to DoLoad, and no longer reads the cache, read must be done explicitly
  07.06.10 wl  Workspaces                    TN5116  neue Property
  17.06.10 wl  OnInitTipType                 TN5150   entfernt
  17.06.10 wl  TipTypeChanged                TN5150   von LayoutWithDevices hierher
  22.06.10 wl  ReloadTipsetDevices           TN5150   führt für alle Tips TipTypeChanged aus
  23.07.10 wl  PutRack                       TN5205   setzt auch die Size-Werte eines unsichtbaren Carriers
  28.10.10 wl  PaintLayoutRack               TN5312   wird nicht benutzt: entfernt
  02.11.10 pk  StoreRacks/CarriersRecursive  TN5307  Find the DataCacheItem using the layout name otherwise the wrong item will be found
  24.02.11 wl                                TN5431   an Änderungen von TRack angepasst
  20.09.11 wl  PaintPosition                 TN5723   entfernt
  28.10.11 wl                                TN5728   an Änderungen von TDiTiObserver angepasst
  03.11.11 wl  CalcVolumeOfRackPos           TN5725   --> Rack
  17.11.11 wl  FindXRackPosByTubeID          TN5725   --> SubstanceLoading
  17.11.11 wl  FindAllStoragePositions       TN5725   neu: Suche nach SubstID nicht mehr in der POSINFO sondern im Layout (im Speicher)
  21.11.11 wl  FindAllStoragePositions       TN5730   gibt Array mit Volumen zurück
  27.11.11 wl  FindAllStoragePositions       TN5730   MinVolume aus SubstanceSet wird als Totvolumen verwendet
  09.12.11 wl  FindSetStoragePositions       TN5761   neu: Alle Storage-Positionen eines Sets suchen
  30.01.12 wl  GetRackNames,GetCarrierNames  TN5789   Namen werden jetzt alphabetisch zurückgegeben
  02.02.11 wl  MakeXRackPositionArray        TN5791   neu
  10.09.12 wl  FindAllStoragePositions       TN5979   neuer Parameter UseDeadVolume
  11.02.13 wl  FindStoragePositionsByID      TN6078   Ex-FindAllStoragePositions: sucht nach allen Storage-Positionen mit gleicher SubstID
  11.02.13 wl  FindStoragePositionsAll       TN6078   neu: sucht nach allen Storage-Positionen überhaupt
  28.03.13 wl  FindSetStoragePositions       TN6120   verwendet TRack.WellIsStoragePosition
  25.04.13 wl  FindSetStoragePositions       TN6120   Bugfix
  06.06.13 wl  ReadToolData                  TN6154   --> ToolHandling
  18.09.13 wl                                TN6045   uses geändert
  -------------------------------------------------------------------------------------------------- }

unit Layout;


interface


uses
    Generics.Collections,
    ListClasses,
    Rack,
    Carrier,
    Workspace,
    AppTypes,
    SubstanceSetDataAdaptor,
    CommonTypes,
    Tipset,
    DitiObserver,
    RackWell,
    RackTypes,
    GeometricClasses,
    CoordSystemMath,
    LayoutDataCache,
    LayoutElementTypeDataCache,
    GeneralTypes,
    LayoutElement,
    LayoutGraphics,
    LayoutDataAdaptor,
    LayoutWorkspaceDataAdaptor,
    WorkspaceDataAdaptor,
    TipsetDataAdaptor;

type
    TFindTipsetDevicesEvent = function(aLayoutDeviceNames: TArray<string>; aUseAllDevices: boolean)
        : TTipsetDeviceRecArray of object;

    TLayoutLink = class
    protected
        fName: string;
    public
        constructor Create();
        property name: string read fName write fName;
    end;

    TLayout = class(TLayoutElement)
    private
        fOnFindTipsetDevices: TFindTipsetDevicesEvent;
        fHintEnabled: boolean;
        function GetGraphics: TLayoutGraphics;
        procedure LoadTipsetDevices(const aLayoutName: string);
        procedure LoadWorkspaces(const aLayoutName: string);
        procedure LoadCarriers(const aLayoutName: string);
        procedure LoadRacks(const aLayoutName: string);
        procedure LoadLayoutLinks(const aLayoutName: string);
        procedure ShowCarrierStackingError(const aCarrierName: string; const aSlotNr: integer);
        procedure CheckCarriers();

        function AddCarrierByRec(const aCarrierRec: TLayoutCarrierRec): TCarrier;
        function AddRackByRec(const aRackRec: TLayoutRackRec; aCheckStackingAllowed: boolean): TRack;
        function AddWorkspaceByRec(const aLayoutWorkspaceRec: TLayoutWorkspaceRec): TWorkspace;

        function CreateWorkspace(): TWorkspace;
        function CreateLayoutLink: TLayoutLink;
        function CreateRack: TRack;
        function CreateCarrier: TCarrier;
        function CreateTipsetDevice(const aLayoutName: string; const aDeviceRec: TTipsetDeviceRec)
            : TTipsetDevice;
        function GetNoOfRacks(): integer;
        function GetNoOfCarriers(): integer;

        function FindCarrierIndexByName(const aName: string): integer;
        function FindWorkspaceIndexByName(const aName: string): integer;
        function GetNextAvailableID(aExistingIDs: TIntegerKeyObjectValueList): integer;
        procedure ClearElements;
        procedure LoadTipsetsRecursive(const aLayoutName: string);
        procedure LoadWorkspacesRecursive(const aLayoutName: string);
        procedure LoadCarriersRecursive(const aLayoutName: string);
        procedure LoadRacksRecursive(const aLayoutName: string);
        function DoLoad(): boolean;
        function DoMakeHintTextForChild(aSender: TObject): string;

        procedure StoreRacksForLayout(const aLayoutName: string);
        procedure StoreRacksRecursive(const aLayoutName: string);
        procedure StoreCarriersForLayout(const aLayoutName: string);
        procedure StoreCarriersRecursive(const aLayoutName: string);
        class function FindWorkspaceByID(aList: TObjectList<TWorkspace>; aID: integer): TWorkspace; static;
        class function FindTipsetByName(aList: TObjectList<TTipsetDevice>; const aDeviceName: string)
            : TTipsetDevice; static;
    protected
        FCarriers: TObjectList<TCarrier>;
        FRacks: TObjectList<TRack>;
        FRunName: string;
        FLayoutName: string; // (wird nur intern benutzt, kein globales Layout)
        fTipsets: TObjectList<TTipsetDevice>;
        fWorkspaces: TObjectList<TWorkspace>;
        fScreenCoordSystemRelation: TCoordSystemRelation;
        fDataCache: TLayoutDataCache;
        fTypeDataCache: TLayoutTypeDataCache;
        fLayoutLinks: TObjectList<TLayoutLink>;
        FDiTiObsvList: TObjectList<TDiTiObserver>;
        fIsLoaded: boolean;
        procedure DoInitGraphics(); override;
        function R(name: string): integer;
        function C(name: string): integer;
        function DoCreateLayoutLink: TLayoutLink; virtual;
        function DoCreateWorkspace(): TWorkspace; virtual;
        function DoCreateCarrier: TCarrier; virtual;
        function DoCreateRack: TRack; virtual;
        function DoCreateTipsetDevice(): TTipsetDevice; virtual;
        procedure SetVisible(aVisible: boolean); override;
        procedure UpdateCarrierRunType(const aCarrierName, aNewCarrierType: string); virtual;
        procedure UpdateRackRunType(const aRackName, aNewRackType: string); virtual;
        procedure UpdateRackRunData(aRack: TRack); virtual;
        procedure LoadElements(const aLayoutName: string);
        procedure TipTypeChanged(const aDeviceName: string; const aTipIndex: integer;
            const aTipTypeName: string);
    public
        constructor Create(const aLayoutName, aRunName: string);
        destructor Destroy(); override;
        function Load(): boolean; virtual;
        function LoadRun(): boolean; virtual;
        function Unload(): boolean; virtual;
        procedure StoreRacks;
        procedure StoreCarriers;
        procedure Show();
        procedure Hide();
        procedure ReloadTipsetDevices();

        function GetRackNoForRackID(aRackID: string): integer;
        function FloorIsVisible(aCarrierName: string; aFloor: integer): boolean;
        procedure UpdateVisibleFloors(aCarrierName: string);
        procedure ChangeVisibleAllRacks(aVisible: boolean);

        function GetDiTiPositions(aTipMap: TipMap; aTipCount: integer; aDitiType: string; aSimulated: boolean;
            var aEmptyRack: TRack): TDiTiPositions;
        function GetDropRack(aDitiType: string; var aDitiTypeExists: boolean): TRack;
        procedure FillDitiRacks(aDitiType: string);
        function CheckSlot(var vSlot: TSlotStruct; aRack: TRack): string;

        function FindCapWastePos(aCapTypeName: string): TXRackPosition;
        function FindFreeSlot(const aCarrierName: string): TSlotStruct;
        function FindFreeDiTiStoreSlot(aCarrierName: string): TSlotStruct;
        procedure GetBalancePos(out oFreePos, oOccupiedPos, oOriginOccupiedPos: TXRackPosition); virtual;

        function FindWorkspaceByName(const aName: string): TWorkspace;
        function FindCarrierByName(const aName: string): TCarrier;
        function FindRackByName(const aName: string): TRack; overload;
        function FindRackByName(const aName: string; aMustFind: boolean): TRack; overload;
        function FindRackByRackID(const aRackID: string): TRack;
        function FindXRackPos(aRackName: string; aPos: integer; aMustFind: boolean = false)
            : TXRackPosition; overload;
        function FindXRackPos(aRackPos: TRackIDPosition; aMustFind: boolean = false): TXRackPosition;
            overload;
        function FindXRackPos(aRackPos: TRackPosition; aMustFind: boolean = false): TXRackPosition; overload;
        function FindSpecialRack(aRackType: TSpecialRackType; aUsedArmName: string;
            aMustFind: boolean): TRack;

        function GetRackIDPos(aRackPos: TRackPosition): TRackIDPosition;

        procedure TakeRack(aRack: TRack);
        procedure PutRack(aRack: TRack; const aCarrierName: string; aSlotNr: integer;
            aRotation: TRotationValue; aCheckStackingAllowed: boolean);
        procedure MoveRack(aRack: TRack; const aCarrierName: string; aSlotNr: integer;
            aRotation: TRotationValue);
        procedure ChangeRackID(aRack: TRack; const aNewID: string); virtual;

        function GetRackNames(): TArray<string>;
        function GetCarrierNames(): TArray<string>;

        function AddLayoutLink(const aLayoutName: string): TLayoutLink;

        function AddWorkspace(aIsLinked: boolean; aID: integer; const aWorkspaceName: string;
            aWorkspaceType: integer; const aViewRelation: TCoordSystemRelationRec): TWorkspace;
        function AddCarrier(aIsLinked: boolean; const aCarrierName, aCarrierType: string;
            aWorkspaceID: integer; aX, aY, aZ: TPosMM): TCarrier;

        function AddRack(aIsLinked: boolean; const aRackName, aRackID, aRackType, aCarrierName: string;
            aSlotNr: integer; aRotation: TRotationValue; aCheckStackingAllowed: boolean = true): TRack;

        procedure RemoveWorkspace(const aWorkspaceName: string);
        procedure RemoveCarrier(const aCarrierName: string);
        procedure RemoveRack(const aRackName: string);

        function GenerateNewWorkspaceID: integer;

        function GetDitiObserver(aDitiType: string; aCreate: boolean): TDitiObserver;
        procedure CreateDitiObservers;

        procedure ChangeCarrierType(const aCarrierName, aNewCarrierType: string; out oOldCarrierType: string);
        procedure ChangeRackType(const aRackName, aNewRackType: string; out oOldRackType: string);
        procedure SetBalanceType(const aSourceRackName: string); virtual;

        class function GetCarrierOfRack(aRack: TRack): TCarrier;
        function GetUsedDevices(out oUseAllDevices: boolean): TArray<string>;
        function FindStoragePositionsByID(const aSubstID: string; aUseDeadVolume: TUseDeadVolume)
            : TArray<TRackIDPositionWithVol>;
        function FindStoragePositionsAll(aUseDeadVolume: TUseDeadVolume): TArray<TRackIDPositionWithVol>;
        function FindSetStoragePositions(const aSetName: string): TArray<TSubstanceSetRec>;
        function MakeXRackPositionArray(aRP: TArray<TRackPosition>): TArray<TXRackPosition>;

        // muss raus:
        property NoOfRacks: integer read GetNoOfRacks;
        property NoOfCarrier: integer read GetNoOfCarriers;

        property Carriers: TObjectList<TCarrier>read fCarriers;
        property Racks: TObjectList<TRack>read fRacks;
        property Workspaces: TObjectList<TWorkspace>read fWorkspaces;

        property LayoutName: string read FLayoutName;
        property LayoutRunName: string read FRunName;
        property Graphics: TLayoutGraphics read GetGraphics;
        property DiTiObsvList: TObjectList<TDiTiObserver>read FDiTiObsvList;
        property IsLoaded: boolean read fIsLoaded;
        property OnFindTipsetDevices: TFindTipsetDevicesEvent read fOnFindTipsetDevices
            write fOnFindTipsetDevices;
        property Tipsets: TObjectList<TTipsetDevice>read fTipsets;
        property TypeDataCache: TLayoutTypeDataCache read fTypeDataCache;
        property DataCache: TLayoutDataCache read fDataCache;
    end;


implementation


uses
    Windows,
    SysUtils,
    LogManager,
    AppSettings,
    SamGlobe,
    UtilLib,
    GUIManager,
    CarrierSlot;

{ TLayoutLink }

constructor TLayoutLink.Create();
begin
    inherited Create();
    fName := '';
end;

{ TLayout }

constructor TLayout.Create(const aLayoutName, aRunName: string);
begin
    inherited Create();
    fName := aLayoutName;
    fLayoutName := aLayoutName;
    fRunName := aRunName;

    fTipsets := TObjectList<TTipsetDevice>.Create(true);

    fLayoutLinks := TObjectList<TLayoutLink>.Create();
    fWorkspaces := TObjectList<TWorkspace>.Create(true);
    fCarriers := TObjectList<TCarrier>.Create();
    fRacks := TObjectList<TRack>.Create(true);

    fDataCache := TLayoutDataCache.Create();
    fTypeDataCache := TLayoutTypeDataCache.Create();

    fDitiObsvList := TObjectList<TDiTiObserver>.Create(true);

    fIsLoaded := false;
end;

destructor TLayout.Destroy();
begin
    FreeAndNil(fDitiObsvList);
    FreeAndNil(fTypeDataCache);
    FreeAndNil(fDataCache);
    FreeAndNil(fRacks);
    FreeAndNil(fCarriers);
    FreeAndNil(fWorkspaces);
    FreeAndNil(fLayoutLinks);
    FreeAndNil(fTipsets);

    inherited;
end;

function TLayout.GetDitiObserver(aDitiType: string; aCreate: boolean): TDitiObserver;
var
    x: integer;
begin
    result := nil;

    // check if diti observer exists
    for x := 0 to FDitiObsvList.Count - 1 do
    begin
        if (FDitiObsvList[x].DitiType = aDitiType) then
        begin
            result := FDitiObsvList[x];
            exit;
        end;
    end;

    // create new diti observer
    if (aCreate) then
    begin
        result := TDitiObserver.Create(aDitiType);
        FDitiObsvList.Add(result);
    end;
end;

procedure TLayout.CreateDitiObservers;
var
    xRackNo, x, x_Pos: integer;
    xDitiType: string;

begin
    // create diti observers for all diti fetch and drop racks in layout
    for xRackNo := 0 to self.NoOfRacks - 1 do
    begin

        // find diti fetch racks
        if (Pos('DT', Uppercase(FRacks[xRackNo].RackID)) = 1) then
        begin // Rack ID must be like 'DT..._...'
            x_Pos := Pos('_', FRacks[xRackNo].RackID);
            if (x_Pos > 0) then
            begin

                // add diti fetch rack to diti observer
                xDitiType := Copy(FRacks[xRackNo].RackID, 3, x_Pos - 3);
                GetDitiObserver(xDitiType, true).AddFetchRack(FRacks[xRackNo]);
            end;
        end;

        // find diti drop racks
        if (Pos('DITIDROP', FRacks[xRackNo].Name) = 1) then
        begin // Name must be like 'DITIDROP...'

            // add diti drop rack to diti observer
            xDitiType := Copy(FRacks[xRackNo].Name, 9, Length(FRacks[xRackNo].Name));
            GetDitiObserver(xDitiType, true).DropRack := FRacks[xRackNo];
        end;
    end;

    // diti observers should check fetch and
    for x := 0 to FDitiObsvList.Count - 1 do
    begin

        // is there any fetch rack?
        if (FDitiObsvList[x].NoOfFetchRacks = 0) then
        begin
            gGUIManager.MessageBox(TLanguageString.
                Read('No disposable tip rack found for type DITI{0}. The rack ID of that rack has to begin with: ""DT{1}_"" !',
                'Es wurde kein Wegwerfspitzenrack für den DiTi-Typ DITi{0} gefunden. Die Rack ID dieses Racks muss mit ""DT{1}_"" beginnen!',
                [FDitiObsvList[x].DitiType, FDitiObsvList[x].DitiType]),
                TLanguageString.Read('No Disposable Tip Rack', 'Kein Wegwerfspitzen-Rack'), MB_ICONSTOP);
            break;
        end;

        // is there a drop rack?
        if not((FDitiObsvList.Count = 1) and (FDitiObsvList[0].DitiType = '')) then
            // bisher: nur 1 diti type namens ''
            if (FDitiObsvList[x].DropRack = nil) then
            begin
                gGUIManager.MessageBox(TLanguageString.
                    Read('No Drop Rack found for type DITI{0}. Name must be DITIDROP{1}',
                    'Es wurde kein Weine Abwurfstation für den Wegwerfspitzen-Typ DITi{0} gefunden. Der Rackname muss DITIDROP{1} lauten!',
                    [FDitiObsvList[x].DitiType, FDitiObsvList[x].DitiType]),
                    TLanguageString.Read('No Drop-Disposable-Tip-Rack',
                    'Keine Abwurfstation für Wegwerfspitzen'), MB_ICONSTOP);
                break;
            end;

        // Show all full positions
        FDitiObsvList[x].PaintFetchRacks;
    end;
end;

function TLayout.R(name: string): integer;

var
    i: integer;
begin
    i := -1;
    repeat
        i := i + 1;
    until ((i = NoOfRacks) or (FRacks[i].Name = '') or (FRacks[i].Name = name));
    result := i;
    if i = NoOfRacks then
        result := -1;
end;

function TLayout.C(name: string): integer;

// This function returns index of first carrier with carriername = "" or carriername = Name
// if you just want to return when carriername = Name lookat at TLayout.FindCarrierIndexByName!

var
    i: integer;
begin
    i := -1;
    repeat
        i := i + 1;
        // PK: is there a reason why the search stops at the first carrier with an Empty Name????????
        // If you can answer this question please put a comment explaining why this is necessary!!!
    until ((i = self.NoOfCarrier) or (FCarriers[i].Name = '') or
        (uppercase(FCarriers[i].Name) = uppercase(name)));
    result := i;
    if i = self.NoOfCarrier then
        result := -1;
end;

function TLayout.GetRackNoForRackID(aRackID: string): integer;

var
    x: integer;
begin
    result := -1;
    if (aRackID = '') then
        EXIT;

    for x := 0 to self.NoOfRacks - 1 do
    begin

        if (aRackID <> FRacks[x].RackID) then
            CONTINUE;

        result := x;
        BREAK;
    end;
end;

procedure TLayout.FillDitiRacks(aDitiType: string);

// Posinfo-Daten eines oder meherer Racks aus dem Layout neu füllen

var
    x: integer;
begin
    for x := 0 to FDiTiObsvList.Count - 1 do
        if (FDitiObsvList[x].DitiType = aDitiType) then
            FDitiObsvList[x].FillFetchRacks;
end;

function TLayout.GetDropRack(aDitiType: string; var aDitiTypeExists: boolean): TRack;
var
    xDitiObserver: TDitiObserver;
begin
    aDitiTypeExists := false;
    result := nil;
    xDitiObserver := GetDitiObserver(aDitiType, false);
    if (xDitiObserver <> nil) then
    begin
        result := xDitiObserver.DropRack;
        aDitiTypeExists := true;
    end;
end;

function TLayout.GetDiTiPositions(aTipMap: TipMap; aTipCount: integer; aDitiType: string; aSimulated: boolean;
    var aEmptyRack: TRack): TDiTiPositions;
var
    x: integer;
begin
    result := TDiTiPositionsUtils.GetEmptyDiTiPositions(aTipCount);

    for x := 0 to FDiTiObsvList.Count - 1 do
        if (FDitiObsvList[x].DitiType = aDitiType) then
        begin
            result := FDitiObsvList[x].GetDiTiPositions(aTipMap, aTipCount, aSimulated, aEmptyRack);
            exit;
        end;
end;

function TLayout.CheckSlot(var vSlot: TSlotStruct; aRack: TRack): string;

// Preconditions: aRack cannot be NULL

var
    xOtherRack: TRack;
    xSlot: TCarrierSlot;
    xCarrier: TCarrier;
begin
    result := '';
    if vSlot.CarrierName = STR_CARRIER_SAME then
    begin // Source-Position = Destination-Position
        ASSERT(Assigned(aRack), 'Rack was not assigned');
        xCarrier := GetCarrierOfRack(aRack);
        xSlot := aRack.Slot as TCarrierSlot;
        vSlot.CarrierName := xCarrier.Name; // ATTENTION : aSlot is changed here!
        vSlot.SlotNr := xSlot.SlotNr;
        vSlot.Rotation := aRack.RackRotation;
    end
    else
    begin
        xCarrier := self.FindCarrierByName(vSlot.CarrierName);
        // ------ Gibt es den Slot überhaupt?
        if Assigned(xCarrier) and (vSlot.SlotNr > 0) and (vSlot.SlotNr <= xCarrier.SlotCount) then
        begin
            // ------------- Ist er durch ein anderes Rack besetzt?
            ASSERT(Assigned(aRack), 'Rack was not assigned');
            xSlot := xCarrier.GetSlotBySlotNr(vSlot.SlotNr);
            if Assigned(xSlot.Rack) and (xSlot.Rack <> aRack) then
            begin
                xOtherRack := xSlot.Rack as TRack;
                result := TLanguageString.Read('Slot {0} on Carrier {1} is occupied by rack [{2}]!',
                    'Slot {0} auf Carrier {1} ist mit Rack [{2}] besetzt!',
                    [vSlot.SlotNr, vSlot.CarrierName, xOtherRack.Name]);
            end;
        end
        else
        begin
            result := TLanguageString.Read('Slot {0} on Carrier {1} is unknown!',
                'Slot {0} auf Carrier {1} ist unbekannt!', [vSlot.SlotNr, vSlot.CarrierName]);
        end;
    end;
end;

{
  procedure TLayout.PaintPosition(aRack: string; aPos: integer; aColor: char);

  begin
  if (R(aRack) > -1) then
  begin
  self.PaintPosition(FRacks[R(aRack)], aPos, aColor);
  end;
  end;

  procedure TLayout.PaintPosition(aRack: TRack; aPos: integer; aColor: char);

  begin
  if (aPos > 0) then
  aRack.PaintTubePos(aPos, aColor);
  end;
}
function TLayout.FindCapWastePos(aCapTypeName: string): TXRackPosition;
var
    i: integer;
begin
    result.Pos := 0;
    result.Rack := nil;

    for i := 0 to self.NoOfRacks - 1 do // search for waste rack with THIS cap type
        if (FRacks[i].IsCapWasteRack) and (FRacks[i].Structure.CapType = aCapTypeName) then
        begin
            result.Rack := FRacks[i];
            result.Pos := 1;
            Exit;
        end;

    for i := 0 to self.NoOfRacks - 1 do // search for waste rack with NO cap type
        if (FRacks[i].IsCapWasteRack) and (FRacks[i].Structure.CapType = '') then
        begin
            result.Rack := FRacks[i];
            result.Pos := 1;
            Exit;
        end;
end;

function TLayout.FindFreeSlot(const aCarrierName: string): TSlotStruct;
var
    xCarrier: TCarrier;
begin
    result.SlotNr := -1;
    xCarrier := FindCarrierByName(aCarrierName);
    if not Assigned(xCarrier) then
        EXIT;
    result := xCarrier.FindFreeSlot();
end;

function TLayout.FindFreeDiTiStoreSlot(aCarrierName: string): TSlotStruct;
var
    x: integer;
begin
    // erst mal in anderen Diti-Storage-Carriern suchen
    for x := 0 to self.NoOfCarrier - 1 do
    begin
        if (aCarrierName <> FCarriers[x].Name) and TCarrier.IsDitiStorage(FCarriers[x].Name) then
        begin
            result := FindFreeSlot(FCarriers[x].Name);
            if (result.SlotNr > 0) then
                exit;
        end;
    end;

    // erst danach im angegebenen Carrier suchen
    if TCarrier.IsDitiStorage(aCarrierName) then
        result := FindFreeSlot(aCarrierName);
end;

procedure TLayout.UpdateVisibleFloors(aCarrierName: string);
var
    xCarrier: TCarrier;
begin
    xCarrier := FindCarrierByName(aCarrierName);
    if not Assigned(xCarrier) then
        EXIT;
    xCarrier.UpdateVisibleFloors();
end;

function TLayout.GetRackIDPos(aRackPos: TRackPosition): TRackIDPosition;
begin
    result.Pos := 0;
    if R(aRackPos.Rack) > -1 then
    begin
        result.Rack := aRackPos.Rack;
        result.Pos := aRackPos.Pos;
        result.RackID := FRacks[R(aRackPos.Rack)].Rackid;
    end;
end;

function TLayout.FindCarrierIndexByName(const aName: string): integer;
var
    i: integer;
    xName: string;
begin
    result := -1;
    xName := UpperCase(aName);
    for i := 0 to self.NoOfCarrier - 1 do
    begin
        if xName = UpperCase(FCarriers[i].Name) then
        begin
            result := i;
            Break;
        end;
    end;
end;

function TLayout.FindWorkspaceIndexByName(const aName: string): integer;
var
    i: integer;
    xName: string;
begin
    result := -1;
    xName := UpperCase(aName);
    for i := 0 to fWorkspaces.Count - 1 do
    begin
        if xName = UpperCase(fWorkspaces[i].Name) then
        begin
            result := i;
            Break;
        end;
    end;
end;

function TLayout.FindWorkspaceByName(const aName: string): TWorkspace;
var
    xIndex: integer;
begin
    result := nil;
    xIndex := FindWorkspaceIndexByName(aName);
    if (xIndex < 0) then
        exit;
    result := fWorkspaces[xIndex];
end;

function TLayout.FindCarrierByName(const aName: string): TCarrier;
var
    xCarrierIndex: integer;
begin
    result := nil;
    xCarrierIndex := FindCarrierIndexByName(aName);
    if (xCarrierIndex < 0) then
        exit;
    result := Carriers[xCarrierIndex];
end;

function TLayout.FindRackByName(const aName: string): TRack;
begin
    result := self.FindRackByName(aName, false);
end;

function TLayout.FindRackByName(const aName: string; aMustFind: boolean): TRack;
var
    xRackIndex: integer;
begin
    result := nil;
    // Normal Case: RackName = aName
    xRackIndex := self.R(aName);

    // Special Case : ID = aName
    // if aName was not a rackname, try to find a rack with the ID = aName
    if xRackIndex < 0 then
    begin
        xRackIndex := self.GetRackNoForRackID(aName);
    end;

    // Error Case :
    // if still no rack was found then raise an error when aMustFind=true
    if (xRackIndex < 0) then
    begin
        if not aMustFind then
            Exit;
        raise Exception.Create(TLanguageString.Read('Rack {0}  is not included in Run-Layout!',
            'Rack {0} ist im Run-Layout nicht enthalten!', [aName]));
    end;

    result := Racks[xRackIndex];

end;

function TLayout.FindXRackPos(aRackName: string; aPos: integer; aMustFind: boolean = false): TXRackPosition;
begin
    result.Pos := aPos;
    result.Rack := FindRackByName(aRackName, aMustFind);
end;

function TLayout.FindXRackPos(aRackPos: TRackIDPosition; aMustFind: boolean = false): TXRackPosition;
begin
    result := self.FindXRackPos(aRackPos.Rack, aRackPos.Pos, aMustFind);
end;

function TLayout.FindXRackPos(aRackPos: TRackPosition; aMustFind: boolean = false): TXRackPosition;
begin
    result := self.FindXRackPos(aRackPos.Rack, aRackPos.Pos, aMustFind);
end;

function TLayout.FindSpecialRack(aRackType: TSpecialRackType; aUsedArmName: string;
    aMustFind: boolean): TRack;
begin
    result := self.FindRackByName(TRackDefinitions.GetSpecialRackNameForArm(aRackType, aUsedArmName), false);
    if Assigned(result) then
        EXIT;
    result := self.FindRackByName(TRackDefinitions.GetSpecialRackNameGeneral(aRackType), aMustFind);
end;

function TLayout.FloorIsVisible(aCarrierName: string; aFloor: integer): boolean;
var
    xCarrier: TCarrier;
begin
    result := false;
    xCarrier := FindCarrierByName(aCarrierName);
    if not Assigned(xCarrier) then
        EXIT;
    result := xCarrier.IsFloorVisible(aFloor);
end;

function TLayout.GetCarrierNames(): TArray<string>;
var
    x: integer;
    xList: TList<string>;
begin
    xList := TList<string>.Create();
    try
        for x := 0 to fCarriers.Count - 1 do
        begin
            xList.Add(FCarriers[x].Name);
        end;
        xList.Sort;
        EXIT(xList.ToArray);
    finally
        FreeAndNil(xList);
    end;
end;

function TLayout.GetRackNames(): TArray<string>;
var
    x: integer;
    xList: TList<string>;
begin
    xList := TList<string>.Create();
    try
        for x := 0 to fRacks.Count - 1 do
        begin
            xList.Add(FRacks[x].Name);
        end;
        xList.Sort;
        EXIT(xList.ToArray);
    finally
        FreeAndNil(xList);
    end;
end;

function TLayout.GetNoOfCarriers: integer;
begin
    result := fCarriers.Count;
end;

function TLayout.GetNoOfRacks: integer;
begin
    result := fRacks.Count;
end;

procedure TLayout.SetVisible(aVisible: boolean);
var
    x: integer;
begin
    inherited;

    for x := 0 to fTipsets.Count - 1 do
    begin
        fTipsets[x].ChangeVisible(aVisible);
    end;

    for x := 0 to fWorkspaces.Count - 1 do
    begin
        fWorkspaces[x].Visible := aVisible;
    end;

    for x := 0 to self.NoOfCarrier - 1 do
    begin
        fCarriers[x].Visible := aVisible;
    end;
    ChangeVisibleAllRacks(aVisible);

end;

procedure TLayout.ChangeVisibleAllRacks(aVisible: boolean);
var
    i: integer;
begin
    for i := 0 to self.NoOfRacks - 1 do
    begin
        FRacks[i].Visible := aVisible;
    end;
end;

procedure TLayout.DoInitGraphics;
begin
    fGraphics := TLayoutGraphics.Create();
end;

function TLayout.GetGraphics: TLayoutGraphics;
begin
    result := fGraphics as TLayoutGraphics;
end;

class function TLayout.GetCarrierOfRack(aRack: TRack): TCarrier;
begin
    result := nil;
    if not Assigned(aRack.Slot) then
        EXIT;
    result := ((aRack.Slot as TCarrierSlot).Carrier as TCarrier);
end;

function TLayout.FindRackByRackID(const aRackID: string): TRack;
var
    xIndex: integer;
begin
    result := nil;
    xIndex := self.GetRackNoForRackID(aRackID);
    if xIndex < 0 then
        EXIT;
    result := self.Racks[xIndex];
end;

function TLayout.DoMakeHintTextForChild(aSender: TObject): string;
var
    x: integer;
begin
    result := '';

    if not fHintEnabled then
        EXIT;

    result := 'Tipsets:';
    for x := 0 to self.Tipsets.Count - 1 do
    begin
        self.AddHintLine(self.Tipsets[x].GetHintText, result);
    end;
end;

procedure TLayout.ChangeRackID(aRack: TRack; const aNewID: string);
begin
    aRack.RackID := aNewID;
end;

procedure TLayout.RemoveCarrier(const aCarrierName: string);
var
    xCarrier: TCarrier;
begin
    xCarrier := self.FindCarrierByName(aCarrierName);
    fCarriers.Remove(xCarrier);
end;

procedure TLayout.RemoveRack(const aRackName: string);
var
    xRack: TRack;
begin
    xRack := self.FindRackByName(aRackName);
    fRacks.Remove(xRack);
end;

procedure TLayout.RemoveWorkspace(const aWorkspaceName: string);
var
    xWorkspace: TWorkspace;
begin
    xWorkspace := self.FindWorkspaceByName(aWorkspaceName);
    fWorkspaces.Remove(xWorkspace);
end;

procedure TLayout.Hide;
begin
    Visible := false;
end;

procedure TLayout.Show;
begin
    Visible := true;
end;

function TLayout.DoCreateTipsetDevice(): TTipsetDevice;
begin
    result := TTipsetDevice.Create();
end;

function TLayout.CreateTipsetDevice(const aLayoutName: string; const aDeviceRec: TTipsetDeviceRec)
    : TTipsetDevice;
begin
    result := DoCreateTipsetDevice();
    if not Assigned(result) then
        EXIT;

    result.InitGraphics(nil);
    result.Prepare(aLayoutName, aDeviceRec);
    result.IsLinked := (aLayoutName <> fLayoutName);
end;

class function TLayout.FindTipsetByName(aList: TObjectList<TTipsetDevice>; const aDeviceName: string)
    : TTipsetDevice;
var
    xTipset: TTipsetDevice;
begin
    result := nil;

    for xTipset in aList do
    begin
        if SameText(xTipset.DeviceName, aDeviceName) then
        begin
            result := xTipset;
            EXIT;
        end;
    end;
end;

procedure TLayout.LoadTipsetDevices(const aLayoutName: string);
var
    xUseAllDevices: boolean;
    xDeviceNames: TArray<string>;
    xDeviceArray: TTipsetDeviceRecArray;
    xTipsetDevice: TTipsetDevice;
    x: integer;
    xTipsetDataCacheItem: TLayoutTipsetDataCacheItem;
begin
    if not Assigned(self.OnFindTipsetDevices) then
        EXIT;

    xDeviceNames := self.GetUsedDevices(xUseAllDevices);
    xDeviceArray := self.OnFindTipsetDevices(xDeviceNames, xUseAllDevices);

    for x := 0 to high(xDeviceArray) do
    begin
        xTipsetDevice := CreateTipsetDevice(aLayoutName, xDeviceArray[x]);
        fTipsets.Add(xTipsetDevice);
    end;

    for xTipsetDataCacheItem in self.DataCache.LayoutTipsetDataCache do
    begin
        if not SameText(xTipsetDataCacheItem.LayoutID, aLayoutName) then
            CONTINUE;
        xTipsetDevice := FindTipsetByName(fTipsets, xTipsetDataCacheItem.PipDeviceName);
        if not Assigned(xTipsetDevice) then
            CONTINUE;
        xTipsetDevice.ChangeTipType(xTipsetDataCacheItem.TipNumber - 1, xTipsetDataCacheItem.TipTypeName);
    end;

    self.ReloadTipsetDevices();
end;

procedure TLayout.ReloadTipsetDevices();
var
    xTipsetDevice: TTipsetDevice;
    x: integer;
begin
    for x := 0 to fTipsets.Count - 1 do
    begin
        xTipsetDevice := fTipsets[x];
        xTipsetDevice.TipTypesChanged();
    end;
end;

function TLayout.CreateRack: TRack;
begin
    result := DoCreateRack();
    result.InitGraphics();
end;

function TLayout.CreateCarrier: TCarrier;
begin
    result := DoCreateCarrier();
    result.InitGraphics();
end;

function TLayout.DoCreateLayoutLink(): TLayoutLink;
begin
    result := TLayoutLink.Create();
end;

function TLayout.DoCreateWorkspace: TWorkspace;
begin
    result := TWorkspace.Create();
end;

function TLayout.DoCreateCarrier: TCarrier;
begin
    result := TCarrier.Create();
end;

function TLayout.DoCreateRack: TRack;
begin
    result := TRack.Create();
end;

function TLayout.CreateWorkspace(): TWorkspace;
begin
    result := DoCreateWorkspace();
    result.InitGraphics();
end;

function TLayout.CreateLayoutLink(): TLayoutLink;
begin
    result := DoCreateLayoutLink();
end;

function TLayout.GetNextAvailableID(aExistingIDs: TIntegerKeyObjectValueList): integer;
begin
    // find new ID
    result := 1;
    while true do
    begin
        if aExistingIDs.IndexOf(result) < 0 then
            EXIT;
        Inc(result);
    end;
end;

function TLayout.GenerateNewWorkspaceID(): integer;
var
    xExistingIDs: TIntegerKeyObjectValueList;
    x: integer;
begin
    xExistingIDs := TIntegerKeyObjectValueList.Create(dupIgnore);
    try
        for x := 0 to fDataCache.LayoutWorkspaceDataCache.Count - 1 do
        begin
            xExistingIDs.Add(fDataCache.LayoutWorkspaceDataCache[x].ToRec.ID);
        end;

        // add workspaces ids in current layout (just in case they are not saved yet - in Layouter)
        for x := 0 to fWorkspaces.Count - 1 do
        begin
            xExistingIDs.Add(fWorkspaces[x].ID);
        end;

        // find new ID
        result := GetNextAvailableID(xExistingIDs);
    finally
        FreeAndNil(xExistingIDs);
    end;
end;

function TLayout.AddWorkspace(aIsLinked: boolean; aID: integer; const aWorkspaceName: string;
    aWorkspaceType: integer; const aViewRelation: TCoordSystemRelationRec): TWorkspace;
var
    xWorkspace: TWorkspace;
    xWorkspaceTypeCacheItem: TWorkspaceTypeDataCacheItem;
begin
    xWorkspace := CreateWorkspace();
    xWorkspace.ID := aID;
    xWorkspace.IsLinked := aIsLinked;

    xWorkspace.Layout := self;
    xWorkspace.Name := aWorkspaceName;
    xWorkspace.ParentMakeHintTextCallback := self.DoMakeHintTextForChild;
    if aViewRelation.Valid then
        xWorkspace.AssignCoordSystemRelationFromRec(xWorkspace.CustomViewCoordSystem, aViewRelation);

    xWorkspace.UseCustomView := aViewRelation.Valid;

    fTypeDataCache.Refresh;
    xWorkspaceTypeCacheItem := fTypeDataCache.WorkspaceTypeDataCache.FindItemByType(aWorkspaceType);
    if not Assigned(xWorkspaceTypeCacheItem) then
        raise Exception.Create(TLanguageString.Read('Workspace type {0} not found',
            'Workspacetyp {0} wurde nicht gefunden', [aWorkspaceType]));

    xWorkspace.SetType(xWorkspaceTypeCacheItem.Rec, xWorkspaceTypeCacheItem.DeviceRecs);

    fWorkspaces.Add(xWorkspace);
    self.Graphics.AddWorkspace(xWorkspace.Graphics);

    result := xWorkspace;
end;

function TLayout.AddWorkspaceByRec(const aLayoutWorkspaceRec: TLayoutWorkspaceRec): TWorkspace;
begin
    result := AddWorkspace(aLayoutWorkspaceRec.LayoutID <> fLayoutName, aLayoutWorkspaceRec.ID,
        aLayoutWorkspaceRec.Name, aLayoutWorkspacerec.WorkspaceTypeID, aLayoutWorkspaceRec.ViewRelation);
end;

procedure TLayout.LoadWorkspaces(const aLayoutName: string);
var
    x: integer;
    xLayoutWorkspaceRec: TLayoutWorkspaceRec;
begin
    for x := 0 to fDataCache.LayoutWorkspaceDataCache.Count - 1 do
    begin
        xLayoutWorkspaceRec := fDataCache.LayoutWorkspaceDataCache[x].ToRec;
        if not SameText(xLayoutWorkspaceRec.LayoutID, aLayoutName) then
            CONTINUE;
        AddWorkspaceByRec(xLayoutWorkspaceRec);
    end;
end;

class function TLayout.FindWorkspaceByID(aList: TObjectList<TWorkspace>; aID: integer): TWorkspace;
var
    x: integer;
    xWorkspace: TWorkspace;
    xID: integer;
begin
    result := nil;
    for x := 0 to aList.Count - 1 do
    begin
        xWorkspace := aList[x];
        xID := xWorkspace.ID;
        if xID = aID then
        begin
            result := xWorkspace;
            EXIT;
        end;
    end;
end;

function TLayout.AddCarrier(aIsLinked: boolean; const aCarrierName, aCarrierType: string;
    aWorkspaceID: integer; aX, aY, aZ: TPosMM): TCarrier;
const
    cCarrierReverseY: boolean = true;
var
    xCarrier: TCarrier;
    xWorkspace: TWorkspace;
    xCarrierTypeCacheItem: TCarrierTypeDataCacheItem;
begin
    xCarrier := CreateCarrier;
    fCarriers.Add(xCarrier);
    xCarrier.IsLinked := aIsLinked;

    xWorkspace := FindWorkspaceByID(fWorkspaces, aWorkspaceID);
    ASSERT(Assigned(xWorkspace), Format('Workspace ID %d not found for carrier %s',
        [aWorkspaceID, aCarrierName]));
    xCarrier.Workspace := xWorkspace;
    xWorkspace.Graphics.AddCarrier(xCarrier.Graphics);
    xCarrier.CoordCalculator.ParentCoordCalculator := xWorkspace.CoordCalculator;

    // Important: for layout graphics the carrier is reflected in y
    xCarrier.CoordCalculator.CoordSystem.ReflectY := cCarrierReverseY;
    if Assigned(xCarrier.Graphics) then
        xCarrier.Graphics.CoordSystem.ReflectY := cCarrierReverseY;

    xCarrierTypeCacheItem := fTypeDataCache.CarrierTypeDataCache.FindItemByType(aCarrierType);

    if not Assigned(xCarrierTypeCacheItem) then
    begin
        raise Exception.Create(TLanguageString.Read('Carrier type {0} not found!',
            'Carriertype {0} wurde nicht gefunden!', [aCarrierType]));
    end;

    xCarrier.SetUp(aCarrierName, xCarrierTypeCacheItem.Rec, aWorkspaceID, aX, aY, aZ);

    result := xCarrier;
    gLogManager.LogF('Load Carrier object %s', [xCarrier.Name], false);
end;

function TLayout.AddCarrierByRec(const aCarrierRec: TLayoutCarrierRec): TCarrier;
begin
    result := AddCarrier(aCarrierRec.Layout <> fLayoutName, aCarrierRec.CarrierName, aCarrierRec.CarrierType,
        aCarrierRec.WorkspaceID, aCarrierRec.Carr_X, aCarrierRec.Carr_Y, aCarrierRec.Carr_Z);
end;

procedure TLayout.LoadCarriers(const aLayoutName: string);
var
    x: integer;
    xLayoutCarrierRec: TLayoutCarrierRec;
begin
    for x := 0 to fDataCache.LayoutCarrierDataCache.Count - 1 do
    begin
        xLayoutCarrierRec := fDataCache.LayoutCarrierDataCache[x].ToRec;
        if not SameText(xLayoutCarrierRec.Layout, aLayoutName) then
            CONTINUE;
        AddCarrierByRec(xLayoutCarrierRec);
    end;
end;

procedure TLayout.ShowCarrierStackingError(const aCarrierName: string; const aSlotNr: integer);
begin
    gGUIManager.MessageBox(TLanguageString.
        Read('Warning: Rack will be placed on slot [{0}] on Carrier [{1}], but previous Level does not contain a rack',
        'Warnung: Das Rack wird auf Slot [{0}] in Carrier {1} gebracht, aber der bisherige Level enthält kein Rack',
        [aSlotNr, aCarrierName]), '', 0);
end;

procedure TLayout.CheckCarriers();
var
    x: integer;
    xCarrier: TCarrier;
    xErrorSlot: TCarrierSlot;
begin
    for x := 0 to self.GetNoOfCarriers - 1 do
    begin
        xCarrier := self.Carriers[x];
        if not xCarrier.CheckSlots(xErrorSlot) then
        begin
            ShowCarrierStackingError(xCarrier.Name, xErrorSlot.SlotNr);
        end;
    end;
end;

procedure TLayout.UpdateCarrierRunType(const aCarrierName, aNewCarrierType: string);
begin
    //
end;

procedure TLayout.ChangeCarrierType(const aCarrierName, aNewCarrierType: string; out oOldCarrierType: string);
var
    xCarrier: TCarrier;
    x: integer;
    xSlot: TCarrierSlot;
    xSlotInfos: TSlotInfoArray;
    xRack: TRack;
    xCarrierTypeCacheItem: TCarrierTypeDataCacheItem;
begin
    oOldCarrierType := '';

    xCarrier := FindCarrierByName(aCarrierName);
    if not Assigned(xCarrier) then
        raise Exception.CreateFmt('Carrier %s not found', [aCarrierName]);

    oOldCarrierType := xCarrier.TypeName;

    xCarrierTypeCacheItem := fTypeDataCache.CarrierTypeDataCache.FindItemByType(aNewCarrierType);

    if not Assigned(xCarrierTypeCacheItem) then
    begin
        raise Exception.Create(TLanguageString.Read('Carrier type {0} not found!',
            'Carriertype {0} wurde nicht gefunden!', [aNewCarrierType]));
    end;

    xSlotInfos := xCarrier.GetSlotInfos();
    xCarrier.ChangeType(xCarrierTypeCacheItem.Rec);

    for x := 0 to high(xSlotInfos) do
    begin
        xSlot := xCarrier.GetSlotBySlotNr(xSlotInfos[x].SlotNr);
        if not Assigned(xSlot) then
            BREAK;
        xRack := FindRackByName(xSlotInfos[x].RackName);
        if not Assigned(xRack) then
            CONTINUE;
        self.PutRack(xRack, xCarrier.Name, xSlot.SlotNr, xRack.RackRotation, true);
    end;
    self.SceneChanged();
    UpdateCarrierRunType(aCarrierName, aNewCarrierType);
end;

procedure TLayout.UpdateRackRunType(const aRackName, aNewRackType: string);
begin
    //
end;

procedure TLayout.ChangeRackType(const aRackName, aNewRackType: string; out oOldRackType: string);
var
    xRack: TRack;
    xRackTypeCacheItem: TRackTypeDataCacheItem;
begin
    oOldRackType := '';

    xRack := FindRackByName(aRackName);
    if not Assigned(xRack) then
        raise Exception.CreateFmt('Rack %s not found', [aRackName]);

    oOldRackType := xRack.TypeName;

    xRackTypeCacheItem := fTypeDataCache.RackTypeDataCache.FindItemByType(aNewRackType);

    if not Assigned(xRackTypeCacheItem) then
    begin
        raise Exception.Create(TLanguageString.Read('Rack type {0} not found!',
            'Racktyp {0} wurde nicht gefunden!', [aNewRackType]));
    end;

    xRack.ChangeType(xRackTypeCacheItem.Rec);

    self.SceneChanged();
    UpdateRackRunType(aRackName, aNewRackType);
end;

procedure TLayout.TakeRack(aRack: TRack);
var
    xSlot: TCarrierSlot;
begin
    if not(aRack.Slot is TCarrierSlot) then
        EXIT;
    xSlot := aRack.Slot as TCarrierSlot;
    xSlot.RemoveRack();
end;

procedure TLayout.PutRack(aRack: TRack; const aCarrierName: string; aSlotNr: integer;
    aRotation: TRotationValue; aCheckStackingAllowed: boolean);
var
    xCarrier: TCarrier;
    xSlot: TCarrierSlot;
    xRotation: TRotationValue;
begin
    xCarrier := self.FindCarrierByName(aCarrierName);
    if not Assigned(xCarrier) then
        raise Exception.CreateFmt('Carrier [%s] not found', [aCarrierName]);

    xSlot := xCarrier.GetSlotBySlotNr(aSlotNr);
    if not Assigned(xSlot) then
        raise Exception.CreateFmt('Slot %d not found in carrier [%s]', [aSlotNr, aCarrierName]);

    xRotation := xCarrier.DefineSlotRotation(aRotation);
    if aCheckStackingAllowed and (not xSlot.IsStackingAllowed) then
    begin
        ShowCarrierStackingError(aCarrierName, aSlotNr);
    end;

    xSlot.PutRack(aRack, xRotation);

    // Invisible Carrier: Größenanpassung durchfüren
    xCarrier.ChangeInvisibleCarrierSize(aRack, aRack.SizeX, aRack.SizeY, aRack.SizeZ);
    // Invisible Carrier: Event für zukünftige Größenanpassungen
    aRack.RackSizeHasChanged := xCarrier.ChangeInvisibleCarrierSize;
end;

procedure TLayout.MoveRack(aRack: TRack; const aCarrierName: string; aSlotNr: integer;
    aRotation: TRotationValue);
var
    xPrevSlot: TCarrierSlot;
    xPrevCarrier: TCarrier;
begin
    if (aRack.Slot is TCarrierSlot) then
    begin
        xPrevSlot := aRack.Slot as TCarrierSlot;
        ASSERT(xPrevSlot.Carrier is TCarrier);
        xPrevCarrier := xPrevSlot.Carrier as TCarrier;
        if SameText(xPrevCarrier.Name, aCarrierName) and (xPrevSlot.SlotNr = aSlotNr) then
        begin
            // No Change
            EXIT;
        end;
        TakeRack(aRack);
    end;

    PutRack(aRack, aCarrierName, aSlotNr, aRotation, true);
    UpdateRackRunData(aRack);
    SceneChanged();
end;

procedure TLayout.UpdateRackRunData(aRack: TRack);
begin
    //
end;

function TLayout.AddRack(aIsLinked: boolean; const aRackName, aRackID, aRackType, aCarrierName: string;
    aSlotNr: integer; aRotation: TRotationValue; aCheckStackingAllowed: boolean): TRack;
var
    xRack: TRack;
    xRackTypeCacheItem: TRackTypeDataCacheItem;
begin
    xRack := CreateRack;
    fRacks.Add(xRack);
    xRack.IsLinked := aIsLinked;

    xRackTypeCacheItem := fTypeDataCache.RackTypeDataCache.FindItemByType(aRackType);
    if not Assigned(xRackTypeCacheItem) then
        raise Exception.Create(TLanguageString.Read('Rack type {0} not found',
            'Racktyp {0} wurde nicht gefunden', [aRackType]));

    xRack.SetUp(aRackName, aRackID, xRackTypeCacheItem.Rec);

    result := xRack;

    xRack.CheckAndDisplay;

    PutRack(xRack, aCarrierName, aSlotNr, aRotation, aCheckStackingAllowed);

    gLogManager.LogF('Load Rack object %s', [xRack.Name], false);

end;

function TLayout.AddRackByRec(const aRackRec: TLayoutRackRec; aCheckStackingAllowed: boolean): TRack;
begin
    result := self.AddRack(aRackRec.Layout <> fLayoutName, aRackRec.RackName, aRackRec.RackID,
        aRackRec.RackType, aRackRec.CarrierName, aRackRec.Slot,
        TRack.GetRotationValue(Trunc(aRackRec.Rotation)), aCheckStackingAllowed);
end;

procedure TLayout.LoadRacks(const aLayoutName: string);
var
    x: integer;
    xLayoutRackRec: TLayoutRackRec;
const
    cCheckStackingAllowedWhenLoadingRacks = false;
    // there is no guarantee that racks appear in the correct slot order.  we will check the stackers later
begin
    for x := 0 to fDataCache.LayoutRackDataCache.Count - 1 do
    begin
        xLayoutRackRec := fDataCache.LayoutRackDataCache[x].ToRec;
        if not SameText(xLayoutRackRec.Layout, aLayoutName) then
            CONTINUE;
        AddRackByRec(xLayoutRackRec, cCheckStackingAllowedWhenLoadingRacks);
    end;
end;

function TLayout.AddLayoutLink(const aLayoutName: string): TLayoutLink;
begin
    result := CreateLayoutLink();
    result.Name := aLayoutName;
    fLayoutLinks.Add(result);
end;

procedure TLayout.LoadLayoutLinks(const aLayoutName: string);
var
    x: integer;
    xLayoutLinkRec: TLayoutLinkRec;
begin
    for x := 0 to fDataCache.LayoutLinkDataCache.Count - 1 do
    begin
        xLayoutLinkRec := fDataCache.LayoutLinkDataCache[x].ToRec;
        if not SameText(xLayoutLinkRec.Layout, aLayoutName) then
            CONTINUE;
        AddLayoutLink(xLayoutLinkRec.LinkedLayout);
    end;
end;

procedure TLayout.LoadTipsetsRecursive(const aLayoutName: string);
var
    x: integer;
    xLayoutLinkRec: TLayoutLinkRec;
begin
    LoadTipsetDevices(aLayoutName);
    for x := 0 to fDataCache.LayoutLinkDataCache.Count - 1 do
    begin
        xLayoutLinkRec := fDataCache.LayoutLinkDataCache[x].ToRec;
        if not SameText(xLayoutLinkRec.Layout, aLayoutName) then
            CONTINUE;
        LoadTipsetsRecursive(xLayoutLinkRec.LinkedLayout);
    end;
end;

procedure TLayout.LoadWorkspacesRecursive(const aLayoutName: string);
var
    x: integer;
    xLayoutLinkRec: TLayoutLinkRec;
begin
    LoadWorkspaces(aLayoutName);
    for x := 0 to fDataCache.LayoutLinkDataCache.Count - 1 do
    begin
        xLayoutLinkRec := fDataCache.LayoutLinkDataCache[x].ToRec;
        if not SameText(xLayoutLinkRec.Layout, aLayoutName) then
            CONTINUE;
        LoadWorkspacesRecursive(xLayoutLinkRec.LinkedLayout);
    end;
end;

procedure TLayout.LoadCarriersRecursive(const aLayoutName: string);
var
    x: integer;
    xLayoutLinkRec: TLayoutLinkRec;
begin
    LoadCarriers(aLayoutName);
    for x := 0 to fDataCache.LayoutLinkDataCache.Count - 1 do
    begin
        xLayoutLinkRec := fDataCache.LayoutLinkDataCache[x].ToRec;
        if not SameText(xLayoutLinkRec.Layout, aLayoutName) then
            CONTINUE;
        LoadCarriersRecursive(xLayoutLinkRec.LinkedLayout);
    end;
end;

procedure TLayout.LoadRacksRecursive(const aLayoutName: string);
var
    x: integer;
    xLayoutLinkRec: TLayoutLinkRec;
begin
    LoadRacks(aLayoutName);
    for x := 0 to fDataCache.LayoutLinkDataCache.Count - 1 do
    begin
        xLayoutLinkRec := fDataCache.LayoutLinkDataCache[x].ToRec;
        if not SameText(xLayoutLinkRec.Layout, aLayoutName) then
            CONTINUE;
        LoadRacksRecursive(xLayoutLinkRec.LinkedLayout);
    end;
end;

procedure TLayout.LoadElements(const aLayoutName: string);
begin
    LoadLayoutLinks(aLayoutName);
    LoadWorkspacesRecursive(aLayoutName);
    LoadCarriersRecursive(aLayoutName);
    LoadRacksRecursive(aLayoutName);
    LoadTipsetsRecursive(aLayoutName);
end;

procedure TLayout.ClearElements();
begin
    fHintEnabled := false;
    fDitiObsvList.Clear();
    fTypeDataCache.ClearCache();
    fDataCache.ClearCache();
    fRacks.Clear();
    fCarriers.Clear();
    fWorkspaces.Clear();
    fLayoutLinks.Clear();
    fTipsets.Clear();
end;

function TLayout.DoLoad(): boolean;
begin
    result := true;
    fHintEnabled := false;

    fTypeDataCache.Refresh();

    LoadElements(fLayoutName);
    CheckCarriers();

    // Disposable Tip Racks anzeigen
    CreateDitiObservers;

    fHintEnabled := true;

    fIsLoaded := true;
end;

function TLayout.Load(): boolean;
begin
    fDataCache.Read();
    result := DoLoad();
end;

function TLayout.LoadRun(): boolean;
begin
    result := DoLoad();
end;

function TLayout.Unload(): boolean;
begin
    ClearElements();
    result := true;
end;

procedure TLayout.StoreRacksForLayout(const aLayoutName: string);
var
    x: integer;
    xRack: TRack;
    xRackDataCacheItem: TLayoutRackDataCacheItem;
begin
    for x := 0 to fRacks.Count - 1 do
    begin
        xRack := fRacks[x];
        xRackDataCacheItem := self.DataCache.LayoutRackDataCache.FindCacheItemByName(aLayoutName, xRack.Name);
        ASSERT(Assigned(xRackDataCacheItem));
        xRackDataCacheItem.RackID := xRack.RackID;
        xRackDataCacheItem.RackType := xRack.TypeName;
        xRackDataCacheItem.CarrierName := ((xRack.Slot as TCarrierSlot).Carrier as TCarrier).Name;
        xRackDataCacheItem.Slot := (xRack.Slot as TCarrierSlot).SlotNr;
        xRackDataCacheItem.Rotation := TRack.GetRotationDegree(xRack.RackRotation);
    end;
end;

procedure TLayout.StoreRacksRecursive(const aLayoutName: string);
var
    x: integer;
    xLayoutLinkRec: TLayoutLinkRec;
begin
    StoreRacksForLayout(aLayoutName);
    for x := 0 to fDataCache.LayoutLinkDataCache.Count - 1 do
    begin
        xLayoutLinkRec := fDataCache.LayoutLinkDataCache[x].ToRec;
        if not SameText(xLayoutLinkRec.Layout, aLayoutName) then
            CONTINUE;
        StoreRacksRecursive(xLayoutLinkRec.LinkedLayout);
    end;
end;

procedure TLayout.StoreCarriersForLayout(const aLayoutName: string);
var
    x: integer;
    xCarrier: TCarrier;
    xCarrierDataCacheItem: TLayoutCarrierDataCacheItem;
begin
    for x := 0 to fCarriers.Count - 1 do
    begin
        xCarrier := fCarriers[x];
        xCarrierDataCacheItem := self.DataCache.LayoutCarrierDataCache.FindCacheItemByName(aLayoutName,
            xCarrier.Name);
        ASSERT(Assigned(xCarrierDataCacheItem));
        xCarrierDataCacheItem.CarrierType := xCarrier.TypeName;
    end;

end;

procedure TLayout.StoreCarriersRecursive(const aLayoutName: string);
var
    x: integer;
    xLayoutLinkRec: TLayoutLinkRec;
begin
    StoreCarriersForLayout(aLayoutName);
    for x := 0 to fDataCache.LayoutLinkDataCache.Count - 1 do
    begin
        xLayoutLinkRec := fDataCache.LayoutLinkDataCache[x].ToRec;
        if not SameText(xLayoutLinkRec.Layout, aLayoutName) then
            CONTINUE;
        StoreCarriersRecursive(xLayoutLinkRec.LinkedLayout);
    end;
end;

procedure TLayout.StoreRacks();
begin
    StoreRacksRecursive(self.Name);
end;

procedure TLayout.StoreCarriers();
begin
    StoreCarriersRecursive(self.Name);
end;

function TLayout.GetUsedDevices(out oUseAllDevices: boolean): TArray<string>;
var
    x, xDevIndex: integer;
    xWorkspace: TWorkspace;
    xDeviceName: string;
    xDevices: TList<string>;
begin
    oUseAllDevices := false;
    result := nil;

    xDevices := TList<string>.Create;
    try
        xDevices.Clear();
        for x := 0 to fWorkspaces.Count - 1 do
        begin
            xWorkspace := fWorkspaces[x];
            oUseAllDevices := xWorkspace.UseAllDevices;
            if oUseAllDevices then
                EXIT;
        end;

        for x := 0 to fWorkspaces.Count - 1 do
        begin
            xWorkspace := fWorkspaces[x];

            for xDevIndex := 0 to xWorkspace.DeviceNames.Count - 1 do
            begin
                xDeviceName := xWorkspace.DeviceNames[xDevIndex];
                if xDevices.IndexOf(xDeviceName) >= 0 then
                    CONTINUE;
                xDevices.Add(xDeviceName);
            end;
        end;

        xDevices.Sort;
        result := xDevices.ToArray;
    finally
        FreeAndNil(xDevices);
    end;
end;

procedure TLayout.GetBalancePos(out oFreePos, oOccupiedPos, oOriginOccupiedPos: TXRackPosition);
begin

end;

procedure TLayout.SetBalanceType(const aSourceRackName: string);
begin
    // dummy
end;

procedure TLayout.TipTypeChanged(const aDeviceName: string; const aTipIndex: integer;
    const aTipTypeName: string);
var
    xTipset: TTipsetDevice;
begin
    xTipset := FindTipsetByName(self.Tipsets, aDeviceName);
    ASSERT(Assigned(xTipset));
    xTipset.Tips[aTipIndex].TypeName := aTipTypeName;
end;

function TLayout.FindSetStoragePositions(const aSetName: string): TArray<TSubstanceSetRec>;
var
    xRack: TRack;
    x, xTubeCount: integer;
    xIsStorage: boolean;
    xRec: TSubstanceSetRec;
begin
    SetLength(result, 0);
    for xRack in fRacks do
    begin
        xTubeCount := xRack.TubeCount;
        for x := 0 to xTubeCount - 1 do
        begin
            xIsStorage := xRack.WellIsStoragePosition(x + 1, xRec);
            if (xIsStorage) and (xRack.Wells[x].StorageData.SetName = aSetName) then
            begin
                SetLength(result, Length(result) + 1);
                result[ high(result)] := xRec;
            end;
        end;
    end;
end;

function TLayout.FindStoragePositionsByID(const aSubstID: string; aUseDeadVolume: TUseDeadVolume)
    : TArray<TRackIDPositionWithVol>;
var
    xRack: TRack;
    x, xTubeCount: integer;
begin
    SetLength(result, 0);
    for xRack in fRacks do
    begin
        xTubeCount := xRack.TubeCount;
        for x := 0 to xTubeCount - 1 do
        begin
            if (xRack.Wells[x].StorageData.IsStorage) and (xRack.Wells[x].StorageData.SubstID = aSubstID) then
            begin
                SetLength(result, Length(result) + 1);
                result[ high(result)].Rack := xRack.name;
                result[ high(result)].RackID := xRack.RackID;
                result[ high(result)].Pos := xRack.Wells[x].WellNr;
                result[ high(result)].Vol := xRack.Wells[x].TotalVolume -
                    TDeadVolume.Calculate(xRack.Wells[x].StorageData.MinVolume1,
                    xRack.Wells[x].StorageData.MinVolume2, aUseDeadVolume);
            end;
        end;
    end;
end;

function TLayout.FindStoragePositionsAll(aUseDeadVolume: TUseDeadVolume): TArray<TRackIDPositionWithVol>;
var
    xRack: TRack;
    x, xTubeCount: integer;
begin
    SetLength(result, 0);
    for xRack in fRacks do
    begin
        xTubeCount := xRack.TubeCount;
        for x := 0 to xTubeCount - 1 do
        begin
            if (xRack.Wells[x].StorageData.IsStorage) then
            begin
                SetLength(result, Length(result) + 1);
                result[ high(result)].Rack := xRack.name;
                result[ high(result)].RackID := xRack.RackID;
                result[ high(result)].Pos := xRack.Wells[x].WellNr;
                result[ high(result)].Vol := xRack.Wells[x].TotalVolume -
                    TDeadVolume.Calculate(xRack.Wells[x].StorageData.MinVolume1,
                    xRack.Wells[x].StorageData.MinVolume2, aUseDeadVolume);
                result[ high(result)].SubstID := xRack.Wells[x].StorageData.SubstID;
            end;
        end;
    end;
end;

function TLayout.MakeXRackPositionArray(aRP: TArray<TRackPosition>): TArray<TXRackPosition>;
var
    x: integer;
begin
    SetLength(result, Length(aRP));
    for x := 0 to high(result) do
    begin
        result[x].Rack := self.FindRackByName(aRP[x].Rack, false);
        result[x].Pos := aRP[x].Pos;
    end;
end;


end.
