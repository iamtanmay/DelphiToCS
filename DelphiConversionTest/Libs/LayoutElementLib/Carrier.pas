{ --------------------------------------------------------------------------------------------------
  Ebene 2 (Sam-Interface)
  --------------------------------------------------------------------------------------------------
  TCarrier - Objekt
  --------------------------------------------------------------------------------------------------
  Datum    op  function/procedure          Änderung / Neuerung
  -------- --  -------------------------   ---------------------------------------------------------
  19.01.98  dl  TCarrier Object public FCarrierName         : string;       für setup
  FCarrierTypeName     : SlotName;     für setup
  property  LCarrierName     : string read FCarrierName
  write FCarrierName;  für setup
  10.11.98 wl  TCarrier.SetCarrierTypeName BorderWidthX,BorderWidthY werden mit eingerechnet
  TCarrier.SelectStackerSlot  ruft Stackerform mit den Carrier-Daten auf
  16.11.98 wl  TCarrier.Paint              doppelte with-Anweisung verursachte Chaos -> entfernt
  18.11.98 wl  TCarrier.SetCarrPosition    aus SetCarrierTypeName -> setzen des Racks an die richtige Position
  WB_X_Steps = FLayout.X_Steps für Setup auch von außen beschreibbar
  WB_Y_Steps,WB_Z_Steps   dito
  16.12.98 wl  TCarrier.SelectStackerSlot  benutzt Property Slot statt FSlotGroup von TRack
  16.12.98 wl                              TCarrierGrid durch TStringGrid ersetzt
  MatrixDrawCell              ehemals TCarrierGrid.DrawCell
  17.12.98 wl                              property LCarrierName entfernt
  property SlotGroup statt FSlotGroup
  07.01.99 wl                              FLayoutName von private nach protected
  14.01.99 wl                      entfernt: FIsShaker,ShakerInUse,SwitchShakerOn/Off,CanSwitch
  entfernt: property LayoutDB
  15.01.99 wl  SelectStackerSlot           --> ObjWorkb
  28.01.99 wl  SetCarrier                  Hint = Matrix.Hint
  Create                      ShowHint ist immer gesetzt
  04.02.99 wl  SetCarrPosition             WBBorders.Left,.Top statt BorderWidthX,Y
  11.02.99 wl  SetCarrierTypeName          FSlotCount wird bestimmt
  24.08.99 wl  SetCarrierTypeName          SlotZ_mm entfernt
  SetCarrPosition             PipettXZero,PipettYZero entfernt
  22.02.00 wl  SetCarrierTypeName          Hint zeigt auch Slot-Anzahl und CarrierTyp
  20.03.00 wl                              mit Level-Buttons (bisher auskommentiert)
  16.05.00 wl  SetWB_X_Steps,SetWB_Y_Steps,SetWB_Z_Steps --> LObjedit (Layouter)
  09.10.02 wl                               TN1293.1 divided into rcRobotics and rcCommon
  20.12.02 wl                               TN1293.5 uses und WinlissyIniAccess geändert
  12.03.03 wl                               TN1293.5 uses posTools
  03.07.03 wl                               TN1501   Compilerhinweise korrigiert
  30.08.03 wl  IsDitiStorage                TN1543   untersucht den Namen auf "DITISTOR"
  02.09.03 wl  SetUp, SetUp_NoRead          TN1559   ersetzt SetCarrier() - .._NoRead ohne Lesen der Layout-Daten (für Layouter)
  02.09.03 wl  Destroy                      TN1559   Matrix wird mit freigegeben
  02.09.03 wl  FLayout                      TN1559   ersetzt durch FWB_X_Steps, FWB_Y_Steps, FWB_Z_Steps
  12.09.03 wl  SetUp                        TN1581   Lesen aus Layout.db --> ObjWorkb
  12.09.03 wl  CreateLevelButtons           TN1581.4  Neues Konzept: Buttons für jeden Level eines Stackers
  12.09.03 wl  BtnOverviewClick             TN1581.4  Zusätzlicher Button: ruft Stacker-Übersicht auf (Stackfrm.pas)
  12.09.03 wl  SetPanelPosition             TN1581.4  Die Buttons können auf allen 4 Seiten eines Carriers liegen (theoretisch auch irgendwo anders)
  12.09.03 wl  SlotChosen                   TN1581.4  virtuelle Funktion, die aufgerufen wird, wenn ein Slot direkt oder über Overview ausgewählt wird
  12.09.03 wl  GetRotation                  TN1581.7  raus (der Carriertyp bestimmt nicht mehr über die Rotation eines Racks)
  12.09.03 wl  GetRow/Column/FloorFromSlot  TN1581.8  Konvertiermethoden: durch Aufhebung der Beschränkungen neue Berechnungen
  16.09.03 wl  CreateLevelButtons           TN1581.4  Buttons noch etwas kleiner
  08.10.03 wl  CreateLevelButtons           TN1581.4  Die Schrift ist bei kleinen Buttons auch kleiner
  17.10.03 wl  BtnOverviewClick             TN1581.15 Das Auswahlfenster für Stacker hier erzeugt -> Stackfrm.pas wird überflüssig!!
  17.10.03 wl  StackerGridMouseUp           TN1581.15 entspricht Stackfrm.StringGrid1MouseUp
  23.10.03 wl  SetCarrierTypeName           TN1631  H_XPreStart_mm wird mit eingelesen
  14.11.03 wl  SetCarrierTypeName           TN1664    Rotation-Werte werden gelesen
  18.11.03 wl  SetCarrierTypeName           TN1667    H_RStart_degree wird gelesen
  15.12.03 wl  SetCarrierTypeName           TN1672    TYP kann Default, Barrier oder Corridor sein
  16.01.04 wl  GetRectangle                 TN1672    gibt Positionswerte in mm zurück
  17.02.04 pk  Paint                        TN1749    Renamed DoPaint.  Calculations in paint caused another Paint to be called
  17.02.04 pk  ChangeVisible                TN1749    By default, controls start as invisible and are set Visible later to reduce flickering
  17.02.04 pk  CreateLevelButtons           TN1750.1  Set caption to '' when level button is too small
  17.02.04 pk  SetVisibleFloor              TN1750.2  New. The logic for setting a floor to visible has been changed
  17.02.04 pk  ShowStackerOverviewDialog    TN1750    Dont show dialog if NumFloors < 2
  03.03.04 pk  ChangeVisible                TN1749    virtual directive
  21.04.04 pk  IsStacker                    TN1865    New
  05.05.04 wl  CreateCarrier/Slot/FirstSlotPosition  TN1788  allgemeine Berechnung von Slots-Positionen von TRack.SetTubeData hierher
  05.05.04 wl  SetCarrPosition              TN1788    benutzt neue Funktion CreateCarrierPosition
  10.07.04 wl  CreateCornerPosition         TN0972    neu: wird für "Evaluate Carrier Position" benutzt
  28.07.04 pk  MatrixDrawCell               TN2061    Calculations cleaned upt to avoid "mysterious" white border
  30.07.04 pk  CarrierArray                 TN2068    Dynamic
  27.10.04 wl  SetCarrierTypeName           TN2071    SlotRotation kann nur diskrete Zustände haben (0,90,180,270)
  27.10.04 wl  DefineSlotRotation           TN2071    prüft einen Rotation-Wert darauf, ob er auf diesem Carrier möglich ist
  11.03.05 wl  SetCarrierTypeName           TN2342    neue TSlotGroupStruct-Werte für Tube-Bewegungen
  07.09.05 wl  Create,CreateLevelButtons    TN2558.9 in Delphi 7 muß ParentBackground gesetzt sein
  06.01.06 wl  CreateSlotPosition           TN2718    ENDLICH!!! Stacker-Höhen werden richtig berechnet wenn gUseCorrectLevelHeights=true
  24.08.06 wl  CreateSlotPosition           TN3269    statt gUseCorrectLevelHeights ist aUseDifferentLevelHeights ein Parameter
  24.08.06 wl  SetCarrierTypeName           TN3269    liest SlotZ_Calculate
  24.08.06 wl  CreateSlotPosition           TN3269    bei SlotZ_Calculate = true wird TWorkbench.GetRackStackHeightsOfLowerRacks_mm zur Höhe gerechnet
  21.11.06 wl  SlotIsBelowOther             TN3412    Erkennt, ob Slots übereinanderliegen
  31.01.07 wl  alle Funktionen              TN3532    TSlotGroup ersetzt: durch neue Properties, TCarrierRec, TRackMoveStruct, TTubeMoveStruct
  31.01.07 wl  SetCarrierTypeName           TN3532    Umstellung von DBRack auf TCarrierDataAdaptor
  31.01.07 wl  GetTubeMoveStruct            TN3532    Jetzt auch mit RStart
  31.01.07 wl  GetRackMoveStruct            TN3532    Jetzt auch mit RStart und RRetake
  14.06.07 wl  GetTubeMoveStruct            TN3728    jetzt mit KeepRotation
  09.11.07 pk  FWB_X/Y/Z                    TN3924    Steps changed to mm
  07.01.08 pk  AddZHeight                   TN3971    New global function. We should later move this function to a more appropriate unit
  20.06.08 pk                               TN4139    inherits from TLayoutElement not TPanel. Graphics moved to fGraphics
  27.06.08 pk  SetTypeName                  TN4139    Changed to SetType
  02.07.08 pk                               TN4139    CarrierPosition now based on matrix math
  07.07.08 pk                               TN4139    Stacker Overview re-implemented in StackerLevelSelectorGraphics
  09.07.08 pk  Destroy                      TN4139    Free Slots
  16.07.08 pk  AddSlot                      TN4139    find the previous level slot and pass it to SetUpSlot
  21.07.08 pk  GetSlotInfos                 TN4179    New
  29.07.08 pk  ShowStackerOverviewDialog    TN4139    call scenechanged
  19.09.08 pk  AddZHeight                   TN4215    call function from CoordCalculator
  10.03.09 pk                               TN4457    Changes needed for reimplementing Stacker Buttons
  11.03.09 pk  CreateCarrierTypeView        TN4457    New
  13.03.09 pk                               TN4463    Changes needed for reimplementing carrier drag move
  16.03.09 pk  SetStackerOverviewDialogInfo TN4463    New
  16.03.09 pk  CheckSlots                   TN4472    New
  11.08.09 wl  CalcSlotPosition             TN4710    Berechnung der Stacker-Höhe korrigiert
  12.08.09 pk  CreateCarrierPosition        TN4714    changed to CalcCarrierPosition
  04.11.09 pk                               TN4843    Replace TObjectList, TStringList, TStrings with Generics Lists and TStringArray
  03.05.10 wl  MakeHintText                 TN5070    kein Text bei Invisible Carrier
  23.07.10 wl  TCarrierRec                  TN5205    Size-Werte heißen jetzt auch SizeX,SizeY,SizeZ
  23.07.10 wl  ChangeInvisibleCarrierSize   TN5205    damit werden die Size-Werte eines unsichtbaren Carriers an die des Racks angepasst
  06.09.10 wl  DoCreateStackerLevelSelectorGraphics   TN5256   Create kann überschrieben werden
  06.09.10 wl  GetStackerLevelInfoArray     TN5256    kann jetzt jederzeit von Stackerauswahlfenster abgefragt werden
  20.09.11 wl  TCarrierList                 TN5723    entfernt
  10.04.13 wl                               TN6045    uses Generics.Collections
  16.04.13 pp  GetCarrierSlots              TN6131    new: DiscretePositions
  -------------------------------------------------------------------------------------------------- }

unit Carrier;


interface


uses
    Windows,
    Generics.Collections,
    SysUtils,
    AppTypes,
    CommonTypes,
    GeometricClasses,
    CarrierDataAdaptor,
    CarrierGraphics,
    CarrierSlot,
    CoordSystemMath,
    MatrixMath,
    LayoutElement,
    StackerLevelSelectorGraphics,
    DiscreteCarrierDataAdaptor;

type
    TSlotGroupType = (sgtDefault, sgtBarrier, sgtCorridor);
    TSlotTurnType = (sttNoTurn, stt180Degree, stt90Degree);

    TRackMoveStruct = record
        HR_XStart_mm: TPosMM;
        HR_YStart_mm: TPosMM;
        HR_ZStart_mm: TPosMM;
        HR_RStart_degree: TPosMM;
        HR_XReTake_mm: TPosMM;
        HR_YReTake_mm: TPosMM;
        HR_ZReTake_mm: TPosMM;
        HR_RReTake_degree: TPosMM;
        HR_XPreStart_mm: TPosMM;
        HR_ZPut_mm: TPosMM;
    end;

    TTubeMoveStruct = record
        HT_XStart_mm: TPosMM;
        HT_YStart_mm: TPosMM;
        HT_ZStart_mm: TPosMM;
        HT_RStart_degree: TPosMM;
        HT_YStartBeforeX: boolean;
        HT_KeepRotation: boolean;
    end;

    TGetScreenPosCallback = procedure(aSender: TObject; out oScreenPos: TGeom3D_Position) of object;

    TSlotInfo = record
        RackName: string;
        SlotNr: integer;
    end;

    TSlotInfoArray = array of TSlotInfo;

    TCarrier = class(TLayoutElement)
    private
        fPosX: TPosMM;
        fPosY: TPosMM;
        fPosZ: TPosMM;

        procedure DoOnVisibleFloor(const aFloor: integer);
        procedure DoOnShowOverview(Sender: TObject);
        function GetSlotCount: integer;
        function GetSlotCountForOneFloor(): integer;
        // procedure StackerGridMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
        function GetWB_X_Right: TPosMM;
        function GetWB_Y_Front: TPosMM;
        function GetSlotGroupType: TSlotGroupType;
        function GetSlotDirection: TRotationValue;
        function GetSlotTurnType: TSlotTurnType;
        function GetSlotGroupCols: integer;
        function GetSlotGroupRows: integer;
        function GetSizeX: TPosMM;
        function GetSizeY: TPosMM;
        procedure UpdateGraphicsInfo;
        procedure AddSlot(aSlotNr: integer);
        procedure AddSlots;
        // function CreateFirstSlotPosition: T3DPoint;
        function CalcSlotPosition(aSlotNr: integer): TPoint4d;
        function GetGraphics: TCarrierGraphics;
        function GetSlotNr(aRow, aColumn, aFloor: integer): integer;
        // function GetRackStackHeightsOfLowerRacks_mm( aSlotNo: integer ): TPosMM;
        function DoMakeHintTextForChild(aSender: TObject): string;
        procedure SetPosX(const aValue: TPosMM);
        procedure SetPosY(const aValue: TPosMM);
        procedure SetPosZ(const aValue: TPosMM);
        function GetSlotGroupFloors: integer;
        function GetSizeZ: TPosMM;
        procedure ClearSlots;
        procedure SetSlotsVisible(aVisible: boolean);
        function GetTypeName: string;
        function GetStackerLevelInfoArray(aSender: TObject): TArray<TStackerLevelInfo>;
        // function IsDiscretePositionTrue: boolean;
        // procedure SetDiscretePosition(const aValue: boolean);
    protected
        fLayout: TObject;
        fWorkspace: TObject;
        FLayoutName: string;
        fTypeName: string;

        fWorkspaceID: integer;
        FVisibleFloor: integer;
        fCarrierRec: TCarrierRec;
        fLayoutWorkspaceID: integer;
        fSlots: TObjectList<TCarrierSlot>;
        fIsLinked: boolean;
        //
        // procedure ChooseSlotByXY(aX, aY: integer);
        procedure SlotChosen(aSlotNr: integer; aRackName: string); virtual;
        // function CreateStackerGrid(aStackerForm: TForm): TStringGrid; virtual;
        function GetRectangle: TRectangleData; // override;

        procedure SetVisibleFloor(aFloor: integer);
        function CreateCarrierSlot: TCarrierSlot;
        function DoCreateStackerLevelSelectorGraphics(): TStackerLevelSelectorGraphics; virtual;
        procedure DoInitGraphics; override;
        function DoCreateCarrierSlot: TCarrierSlot; virtual;
        procedure SetVisible(aVisible: boolean); override;
        function MakeHintText(): string; override;
        procedure DoAfterCreateCarrierSlot(aCarrierSlot: TCarrierSlot); virtual;
        procedure DoOnStackerLevelChosen(aStackerChosenSlot: integer; const aStackerChosenRack: string);
        procedure ShowStackerOverviewDialog;
    public
        // constructor
        constructor Create();
        destructor Destroy; override;
        // public methods
        class function IsDitiStorage(aCarrierName: string): boolean;
        procedure SetUp(const aName: string; const aCarrierType: TCarrierRec; aWorkspaceID: integer;
            aX, aY, aZ: TPosMM);
        procedure SetType(const aCarrierType: TCarrierRec);
        procedure ChangeType(const aCarrierType: TCarrierRec);
        function GetSlotInfos: TSlotInfoArray;
        procedure MakeFloorOfSlotVisible(aSlotNr: integer);
        function GetRowFromSlot(aSlotNr: integer): integer;
        function GetColumnFromSlot(aSlotNr: integer): integer;
        function GetFloorFromSlot(aSlotNr: integer): integer;
        function GetVisibleSlot(aRow, aColumn: integer): integer;
        function IsFloorVisible(aFloor: integer): boolean;
        function IsSlotOnVisibleFloor(aSlotNr: integer): boolean;
        function IsStacker(): boolean;
        // procedure ShowStackerOverviewDialog();
        function CreateCornerPosition(aRight, aFront: boolean): TGeom3D_Position;
        function CalcCarrierPosition: TPoint4d;

        function DefineSlotRotation(aDesiredRotation: TRotationValue): TRotationValue;
        function SlotIsBelowOther(aRequestedSlot, aOtherSlot: integer): boolean;
        procedure GetH_StartValues(out oXStart_mm, oYStart_mm, oZStart_mm, oRStart_degree: TPosMM);
        function GetRackMoveStruct(): TRackMoveStruct;
        function GetTubeMoveStruct(): TTubeMoveStruct;
        function GetSlotBySlotNr(aSlotNr: integer): TCarrierSlot;
        function FindFreeSlot(): TSlotStruct;
        procedure UpdateVisibleFloors();
        function CheckSlots(out oErrorSlot: TCarrierSlot): boolean;
        procedure SetStackerOverviewDialogInfo(const aSelectorGraphics: TStackerLevelSelectorGraphics);
        procedure ChangeInvisibleCarrierSize(aSender: TObject; aSizeX, aSizeY, aSizeZ: double);

        function GetDiscretePosition(const aSlotNr: integer): TPoint4d;

        class function Turn180degree(aRotationValue: TRotationValue): TRotationValue;
        class function Turn90degree(aRotationValue: TRotationValue): TRotationValue;
        class function CreateCarrierTypeView(const aCarrierTypeRec: TCarrierRec): TCarrier;
        class function GetCarrierSlots(const aCarrier: TCarrier): TObjectList<TCarrierSlot>;

        // properties
        property Workspace: TObject read fWorkspace write fWorkspace;
        property TypeName: string read GetTypeName;
        property PosX: TPosMM read fPosX write SetPosX;
        property PosY: TPosMM read fPosY write SetPosY;
        property PosZ: TPosMM read fPosZ write SetPosZ;
        property WB_X_Right: TPosMM read GetWB_X_Right;
        property WB_Y_Front: TPosMM read GetWB_Y_Front;

        property WorkspaceID: integer read fWorkspaceID write fWorkspaceID;
        property IsLinked: boolean read fIsLinked write fIsLinked;
        property SlotCount: integer read GetSlotCount;
        property VisibleFloor: integer read FVisibleFloor;
        property RectData: TRectangleData read GetRectangle;
        //
        property SlotGroupType: TSlotGroupType read GetSlotGroupType;
        property SlotDirection: TRotationValue read GetSlotDirection;
        property SlotTurnType: TSlotTurnType read GetSlotTurnType;
        property SizeX: TPosMM read GetSizeX;
        property SizeY: TPosMM read GetSizeY;
        property SizeZ: TPosMM read GetSizeZ;
        property SlotGroupRows: integer read GetSlotGroupRows;
        property SlotGroupCols: integer read GetSlotGroupCols;
        property SlotGroupFloors: integer read GetSlotGroupFloors;

        property Graphics: TCarrierGraphics read GetGraphics;
    end;

function AddZHeight(const aCurHeight, aAddedHeight: TPosMM): TPosMM;


implementation


uses
    Math,
    Dialogs,
    GeneralTypes,
    SamGlobe,
    AppSettings,
    LogManager,
    Layout,
    Rack,
    LayoutElementGraphicsInfo;

{ TCarrier }

constructor TCarrier.Create();
begin
    inherited Create();

    FLayoutName := '';
    fVisibleFloor := -1;

    fSlots := TObjectList<TCarrierSlot>.Create;
end;

destructor TCarrier.Destroy;
begin
    FreeAndNil(fSlots);
    inherited;
end;

procedure TCarrier.SetSlotsVisible(aVisible: boolean);
var
    x: integer;
begin
    for x := 0 to fSlots.Count - 1 do
    begin
        fSlots[x].Visible := aVisible;
    end;
    UpdateVisibleFloors();
end;

procedure TCarrier.SetVisible(aVisible: boolean);
begin
    inherited;
    SetSlotsVisible(aVisible);
end;

function TCarrier.DoCreateCarrierSlot(): TCarrierSlot;
begin
    result := TCarrierSlot.Create();
end;

function TCarrier.DoMakeHintTextForChild(aSender: TObject): string;
begin
    result := MakeHintText();
end;

function TCarrier.CalcSlotPosition(aSlotNr: integer): TPoint4d;
var
    xFloorDist, xColDist, xRowDist: TPosMM; // Abstände zwischen den Reihen, Kolonnen, Levels
begin
    // Bei true soll er die Position aus einer Tbl lesen
    if fCarrierRec.DiscretePositions then
        EXIT(self.GetDiscretePosition(aSlotNr));

    xFloorDist := 0;
    if (fCarrierRec.Floors > 1) then
        xFloorDist := (fCarrierRec.SlotZ_ReallyFirst_mm - fCarrierRec.SlotZ_ReallyLast_mm) /
            (fCarrierRec.Floors - 1);

    xColDist := 0;
    if (fCarrierRec.Cols > 1) then
        xColDist := (fCarrierRec.SlotX_Last_mm - fCarrierRec.SlotX_First_mm) / (fCarrierRec.Cols - 1);

    xRowDist := 0;
    if (fCarrierRec.Rows > 1) then
        xRowDist := (fCarrierRec.SlotY_Last_mm - fCarrierRec.SlotY_First_mm) / (fCarrierRec.Rows - 1);

    result.X := fCarrierRec.SlotX_First_mm + (self.GetColumnFromSlot(aSlotNr) * xColDist);
    result.Y := fCarrierRec.SlotY_First_mm + (self.GetRowFromSlot(aSlotNr) * xRowDist);
    result.Z := fCarrierRec.SlotZ_ReallyLast_mm + (self.GetFloorFromSlot(aSlotNr) * xFloorDist);
end;

procedure TCarrier.AddSlot(aSlotNr: integer);
var
    xSlot: TCarrierSlot;
    xFloor, xCol, xRow: integer;
    xPrevLevelSlotNr: integer;
    xPrevLevelSlot: TCarrierSlot;
    xPoint: TPoint4d;
begin
//    if fCarrierRec.DiscretePositions = false then
        xPoint := CalcSlotPosition(aSlotNr);

    xSlot := CreateCarrierSlot();
    xSlot.Carrier := self;
    xSlot.CarrierGetTypeName := self.GetTypeName;
    xSlot.ParentMakeHintTextCallback := DoMakeHintTextForChild;

    // Get Prev level slot
    xPrevLevelSlot := nil;
    xFloor := self.GetFloorFromSlot(aSlotNr);
    xRow := self.GetRowFromSlot(aSlotNr);
    xCol := self.GetColumnFromSlot(aSlotNr);
    if xFloor > 0 then
    begin
        xPrevLevelSlotNr := self.GetSlotNr(xRow, xCol, xFloor - 1);
        xPrevLevelSlot := self.GetSlotBySlotNr(xPrevLevelSlotNr);
    end;

    if ((fCarrierRec.DiscretePositions) and (Length(fCarrierRec.DiscretePosRecArray) > 0)) then
        xSlot.SetUpSlot(aSlotNr, fCarrierRec.SlotX_mm, fCarrierRec.SlotY_mm,
            fCarrierRec.DiscretePosRecArray[aSlotNr - 1].X_mm,
            fCarrierRec.DiscretePosRecArray[aSlotNr - 1].Y_mm,
            fCarrierRec.DiscretePosRecArray[aSlotNr - 1].Z_mm,
            fCarrierRec.SlotZ_Calculate, xPrevLevelSlot)
    else
        xSlot.SetUpSlot(aSlotNr, fCarrierRec.SlotX_mm, fCarrierRec.SlotY_mm, xPoint.X, xPoint.Y, xPoint.Z,
            fCarrierRec.SlotZ_Calculate, xPrevLevelSlot);

    fSlots.Add(xSlot);
    self.Graphics.AddSlot(xSlot.Graphics);
    xSlot.CoordCalculator.ParentCoordCalculator := fCoordCalculator;
end;
{
  function TCarrier.CreateFirstSlotPosition: T3DPoint;
  begin
  result := CreateCarrierPosition;
  result.X := result.X + fCarrierRec.SlotX_First_mm;
  result.Y := result.Y + fCarrierRec.SlotY_First_mm;
  result.Z := AddZHeight( result.Z, fCarrierRec.SlotZ_ReallyFirst_mm );
  end;
}

function TCarrier.GetSizeZ(): TPosMM;
begin
    result := fCarrierRec.SizeZ_mm;
end;

procedure TCarrier.ClearSlots();
var
    x: integer;
    xSlot: TCarrierSlot;
begin
    // dont use slotcount here, use fslots!
    for x := 0 to fSlots.Count - 1 do
    begin
        xSlot := fSlots[x];
        if Assigned(xSlot.Rack) then
            xSlot.RemoveRack;
    end;
    fSlots.Clear;
end;

procedure TCarrier.AddSlots();
var
    x: integer;
begin
    ClearSlots();
    for x := 1 to self.SlotCount do
    begin
        AddSlot(x);
    end;
    SetSlotsVisible(self.Visible);
end;

function TCarrier.CheckSlots(out oErrorSlot: TCarrierSlot): boolean;
var
    x: integer;
    xSlot: TCarrierSlot;
begin
    result := true;
    oErrorSlot := nil;

    for x := 0 to fSlots.Count - 1 do
    begin
        xSlot := fSlots[x];
        if not Assigned(xSlot.Rack) then
            CONTINUE;
        if not xSlot.IsStackingAllowed then
        begin
            result := false;
            oErrorSlot := xSlot;
            EXIT;
        end;
    end;
end;

procedure TCarrier.SetPosX(const aValue: TPosMM);
begin
    fPosX := aValue;
    fCoordCalculator.CoordSystem.TranslateX := fPosX;
    self.Graphics.PosX := fPosX;
end;

procedure TCarrier.SetPosY(const aValue: TPosMM);
begin
    fPosY := aValue;
    fCoordCalculator.CoordSystem.TranslateY := fPosY;
    self.Graphics.PosY := fPosY;
end;

procedure TCarrier.SetPosZ(const aValue: TPosMM);
begin
    fPosZ := aValue;
    fCoordCalculator.CoordSystem.TranslateZ := fPosZ;
    self.Graphics.PosZ := fPosZ;
end;

procedure TCarrier.SetUp(const aName: string; const aCarrierType: TCarrierRec; aWorkspaceID: integer;
    aX, aY, aZ: TPosMM);
begin
    self.Name := aName;
    fWorkspaceID := aWorkspaceID;
    self.PosX := aX;
    self.PosY := aY;
    self.PosZ := aZ;
    // ---------------------------------------- Carriertyp bestimmen und zeichnen
    SetType(aCarrierType);
end;

function TCarrier.GetSlotGroupType: TSlotGroupType;
begin
    case fCarrierRec.CarrierType of
        1:
            result := sgtBarrier;
        2:
            result := sgtCorridor;
        else
            result := sgtDefault;
    end;
end;

function TCarrier.GetSlotDirection: TRotationValue;
begin
    case Round(fCarrierRec.SlotRotation_Degree) of
        90:
            result := rotation_90; // zur Zeit nur gerade Ausrichtung möglich!
        180:
            result := rotation_180;
        270:
            result := rotation_270;
        else
            result := rotation_0;
    end;
end;

function TCarrier.GetSlotTurnType: TSlotTurnType;
begin
    case fCarrierRec.H_DirectionTurnType of
        1:
            result := stt180Degree;
        2:
            result := stt90Degree;
        else
            result := sttNoTurn;
    end;
end;

function TCarrier.MakeHintText(): string;
begin
    result := '';
    if self.TypeName = TCarrierDataAdaptor.InvisibleCarrierName then
        EXIT;

    self.AddHintLine('Carrier', result);
    self.AddHintSubLine(Format('Name: [%s]', [fName]), result);
    self.AddHintSubLine(Format('Type: %s', [fTypeName]), result);
    self.AddHintSubLine(Format('Total Slots: %d', [GetSlotCount]), result);
end;

function TCarrier.GetDiscretePosition(const aSlotNr: integer): TPoint4d;
var
    xPosX, xPosY, xPosZ: double;
    xDA: TDiscreteCarrierDataAdaptor;
begin
    xDA := TDiscreteCarrierDataAdaptor.Create();
    try
        xDA.SelectAndOpenCarrierPosition(FTypeName, aSlotNr, true);
        xPosX := xDA.DataProvider.FieldByNameAsFloat('Pos_X_mm');
        xPosY := xDA.DataProvider.FieldByNameAsFloat('Pos_Y_mm');
        xPosZ := xDA.DataProvider.FieldByNameAsFloat('Pos_Z_mm');
        xDA.Close;
    finally
        FreeAndNil(xDA);
    end;
    result := MakePoint4d(xPosX, xPosY, xPosZ);
end;

procedure TCarrier.ChangeInvisibleCarrierSize(aSender: TObject; aSizeX, aSizeY, aSizeZ: double);
begin
    if self.TypeName = TCarrierDataAdaptor.InvisibleCarrierName then
    begin
        fCarrierRec.SizeX_mm := aSizeX;
        fCarrierRec.SizeY_mm := aSizeY;
        UpdateGraphicsInfo();
    end;
end;

procedure TCarrier.UpdateGraphicsInfo();
begin
    with self.Graphics.GraphicsInfo do
    begin
        name := fName;
        SizeX := fCarrierRec.SizeX_mm;
        SizeY := fCarrierRec.SizeY_mm;
        SizeZ := self.SizeZ;
        Rows := fCarrierRec.Rows;
        Cols := fCarrierRec.Cols;
        Levels := fCarrierRec.Floors;
        SlotSizeX := fCarrierRec.SlotX_mm;
        SlotSizeY := fCarrierRec.SlotY_mm;
        SlotFirstX := fCarrierRec.SlotX_First_mm;
        SlotFirstY := fCarrierRec.SlotY_First_mm;
        SlotLastX := fCarrierRec.SlotX_Last_mm;
        SlotLastY := fCarrierRec.SlotY_Last_mm;
        Stackerbuttons := fCarrierRec.Stackerbuttons;
    end;

    self.Graphics.UpdateGraphicsInfo();
end;

procedure TCarrier.SetType(const aCarrierType: TCarrierRec);
begin
    fCarrierRec := aCarrierType;
    self.fCoordCalculator.CoordSystem.TranslateSizeZ := self.SizeZ;
    fTypeName := aCarrierType.Name;
    AddSlots();
    UpdateGraphicsInfo();
    self.Graphics.CreateLevelButtons();
    self.Graphics.VisibleFloorChangedCallback := self.DoOnVisibleFloor;
    self.Graphics.ShowOverviewCallback := self.DoOnShowOverview;
    self.SetVisibleFloor(0);
end;

procedure TCarrier.SlotChosen(aSlotNr: integer; aRackName: string);
begin
    // virtual method "SlotChosen" will be called after choosing a rack/slot from the slot overview
end;

function TCarrier.GetSlotCount: integer;
begin
    result := fCarrierRec.Rows * fCarrierRec.Cols * fCarrierRec.Floors;
end;

class function TCarrier.IsDitiStorage(aCarrierName: string): boolean;
begin
    result := false;
    if POS('DITISTOR', Uppercase(aCarriername)) > 0 then
        result := true;
end;

// --------------------------------------------------------------------------------------------------
// Konvertiermethoden:
// Achtung: Row, Colunm und Floor sind 0-basiert, Slot ist 1-basiert!!
// --------------------------------------------------------------------------------------------------
function TCarrier.GetRowFromSlot(aSlotNr: integer): integer;
begin
    result := 0;
    if (aSlotNr > 0) then
        result := (aSlotNr - 1) mod fCarrierRec.Rows;
end;

class function TCarrier.GetCarrierSlots(const aCarrier: TCarrier): TObjectList<TCarrierSlot>;
var
    xCarrierSlot: TObjectList<TCarrierSlot>;
begin
    xCarrierSlot := aCarrier.fSlots;
    EXIT(xCarrierSlot);
end;

function TCarrier.GetColumnFromSlot(aSlotNr: integer): integer;
begin
    result := 0;
    if (aSlotNr > 0) then
        result := (aSlotNr - 1) mod (fCarrierRec.Rows * fCarrierRec.Cols) div fCarrierRec.Rows;
end;

function TCarrier.GetFloorFromSlot(aSlotNr: integer): integer;
begin
    result := 0;
    if (aSlotNr > 0) then
        result := (aSlotNr - 1) div (fCarrierRec.Rows * fCarrierRec.Cols);
end;

function TCarrier.GetSlotNr(aRow, aColumn, aFloor: integer): integer;
begin
    result := 1 + aRow + (aColumn * fCarrierRec.Rows) + (aFloor * fCarrierRec.Rows * fCarrierRec.Cols);
end;

function TCarrier.GetVisibleSlot(aRow, aColumn: integer): integer;
begin
    result := GetSlotNr(aRow, aColumn, fVisibleFloor);
end;

function TCarrier.GetWB_X_Right: TPosMM;
begin
    result := fPosX + fCarrierRec.SizeX_mm
end;

function TCarrier.GetWB_Y_Front: TPosMM;
begin
    result := fPosY + fCarrierRec.SizeY_mm
end;

function TCarrier.GetRectangle: TRectangleData;
begin
    //
    { TODO -oPK -cWB : Will propbably not work if rack is rotated }
    result.XPos := fPosX;
    result.XPos2 := GetWB_X_Right;
    result.YPos := fPosY;
    result.YPos2 := GetWB_Y_Front;
end;

function TCarrier.IsStacker(): boolean;
begin
    result := (fCarrierRec.floors > 1);
end;

function TCarrier.IsFloorVisible(aFloor: integer): boolean;
begin
    result := (fVisibleFloor = aFloor);
end;

function TCarrier.IsSlotOnVisibleFloor(aSlotNr: integer): boolean;

var
    xFloor: integer;
begin
    xFloor := GetFloorFromSlot(aSlotNr);
    result := IsFloorVisible(xFloor);
end;

function TCarrier.GetStackerLevelInfoArray(aSender: TObject): TArray<TStackerLevelInfo>;
var
    xSlotNr: integer;
    x: integer;
begin
    SetLength(result, self.SlotCount);
    for x := 0 to self.SlotCount - 1 do
    begin
        xSlotNr := x + 1;
        result[x].SlotNr := xSlotNr;
        if Assigned(self.GetSlotBySlotNr(xSlotNr).Rack) then
            result[x].Name := self.GetSlotBySlotNr(xSlotNr).Rack.Name
        else
            result[x].Name := '';
    end;
end;

procedure TCarrier.SetStackerOverviewDialogInfo(const aSelectorGraphics: TStackerLevelSelectorGraphics);
var
    xPoint: TPoint4d;
begin
    if SlotGroupFloors < 2 then
        Exit;

    xPoint := self.Graphics.ClientToScreen(0, self.Graphics.GraphicsInfo.SizeY, 0);
    aSelectorGraphics.SetInfo(fName, self.GetStackerLevelInfoArray, self.SlotGroupRows, self.SlotGroupCols,
        self.SlotGroupFloors, round(xPoint.x), round(xPoint.y), nil, nil);
end;

procedure TCarrier.ShowStackerOverviewDialog();
var
    xChosenSlot: integer;
    xChosenRack: string;
    xSelectorGraphics: TStackerLevelSelectorGraphics;
begin
    if SlotGroupFloors < 2 then
        Exit;
    xSelectorGraphics := DoCreateStackerLevelSelectorGraphics();
    try
        SetStackerOverviewDialogInfo(xSelectorGraphics);
        xSelectorGraphics.ShowModal();
        xChosenSlot := xSelectorGraphics.ChosenSlot;
        xChosenRack := xSelectorGraphics.ChosenRack;
    finally
        xSelectorGraphics.Free;
    end;
    self.DoOnStackerLevelChosen(xChosenSlot, xChosenRack);
    self.SceneChanged();
end;

procedure TCarrier.DoOnShowOverview(Sender: TObject);
begin
    ShowStackerOverviewDialog();
end;

procedure TCarrier.DoOnVisibleFloor(const aFloor: integer);

begin
    SetVisibleFloor(aFloor);
end;

procedure TCarrier.SetVisibleFloor(aFloor: integer);

begin
    if fVisibleFloor = aFloor then
        Exit;
    FVisibleFloor := aFloor;

    self.Graphics.SetVisibleFloor(aFloor);
    UpdateVisibleFloors();
    self.SceneChanged();
end;

procedure TCarrier.MakeFloorOfSlotVisible(aSlotNr: integer);

var
    xFloor: integer;
begin
    // Acthtung: xFloor ist 0-basiert, SlotNr ist 1-basiert!!
    xFloor := GetFloorFromSlot(aSlotNr);
    SetVisibleFloor(xFloor);

end;

procedure TCarrier.DoOnStackerLevelChosen(aStackerChosenSlot: integer; const aStackerChosenRack: string);
begin
    // make floor visible and call virtual method "SlotChosen"
    if (aStackerChosenSlot > 0) and (aStackerChosenSlot <= GetSlotCount) then
    begin
        MakeFloorOfSlotVisible(aStackerChosenSlot);
        SlotChosen(aStackerChosenSlot, aStackerChosenRack)
    end;
end;

function TCarrier.CalcCarrierPosition: TPoint4d;
begin
    self.CoordCalculator.CalcCoordMatrix();
    result := self.CoordCalculator.TransformPoint(MakePoint4d(0, 0, 0));
end;

function TCarrier.CreateCornerPosition(aRight, aFront: boolean): TGeom3D_Position;
var
    xPoint: TPoint4d;
begin
    xPoint := CalcCarrierPosition;
    result := TGeom3D_Position.Create(xPoint.X, xPoint.Y, xPoint.Z);
    if (aRight) then
        result.X := result.X + fCarrierRec.SizeX_mm;
    if (aFront) then
        result.Y := result.Y + fCarrierRec.SizeY_mm;
end;

function TCarrier.DoCreateStackerLevelSelectorGraphics: TStackerLevelSelectorGraphics;
begin
    result := TStackerLevelSelectorGraphics.Create;
end;

function TCarrier.DefineSlotRotation(aDesiredRotation: TRotationValue): TRotationValue;
begin
    case (self.SlotTurnType) of

        // Rack kann um 180° verdreht sein
        stt180Degree:
            begin
                if (aDesiredRotation = Turn180degree(self.SlotDirection)) then
                    result := aDesiredRotation
                else
                    result := self.SlotDirection;
            end;

        // Rack kann um 90° verdreht sein -> alle Richtungen sind möglich
        stt90Degree:
            begin
                result := aDesiredRotation;
            end;

        // keine Rotation möglich
        else
            result := self.SlotDirection;
    end;
end;

procedure TCarrier.GetH_StartValues(out oXStart_mm, oYStart_mm, oZStart_mm, oRStart_degree: TPosMM);
begin
    oXStart_mm := fCarrierRec.H_XStart_mm;
    oYStart_mm := fCarrierRec.H_YStart_mm;
    oZStart_mm := fCarrierRec.H_ZStart_mm;
    oRStart_degree := fCarrierRec.H_RStart_degree;
end;

function TCarrier.GetRackMoveStruct(): TRackMoveStruct;
begin
    GetH_StartValues(result.HR_XStart_mm, result.HR_YStart_mm, result.HR_ZStart_mm, result.HR_RStart_degree);
    result.HR_XReTake_mm := fCarrierRec.H_XReTake_mm;
    result.HR_YReTake_mm := fCarrierRec.H_YReTake_mm;
    result.HR_ZReTake_mm := fCarrierRec.H_ZReTake_mm;
    result.HR_RReTake_degree := fCarrierRec.H_RReTake_degree;
    result.HR_XPreStart_mm := fCarrierRec.H_XPreStart_mm;
    result.HR_ZPut_mm := fCarrierRec.H_ZPut_mm;
end;

function TCarrier.GetTubeMoveStruct(): TTubeMoveStruct;
begin
    if (fCarrierRec.HTube_UseValues) then
    begin
        result.HT_XStart_mm := fCarrierRec.HTube_XStart_mm;
        result.HT_YStart_mm := fCarrierRec.HTube_YStart_mm;
        result.HT_ZStart_mm := fCarrierRec.HTube_ZStart_mm;
        result.HT_RStart_degree := fCarrierRec.HTube_RStart_degree;
        result.HT_YStartBeforeX := fCarrierRec.HTube_YStartBeforeX;
        result.HT_KeepRotation := fCarrierRec.HTube_KeepRotation;
    end
    else
    begin
        GetH_StartValues(result.HT_XStart_mm, result.HT_YStart_mm, result.HT_ZStart_mm,
            result.HT_RStart_degree);
        result.HT_YStartBeforeX := false;
        result.HT_KeepRotation := false;
    end;
end;

function TCarrier.GetTypeName: string;
begin
    result := fTypeName;
end;

function TCarrier.GetSlotCountForOneFloor(): integer;
begin
    result := fCarrierRec.Rows * fCarrierRec.Cols;
end;

function TCarrier.SlotIsBelowOther(aRequestedSlot, aOtherSlot: integer): boolean;
begin
    result := false;

    if (aRequestedSlot < aOtherSlot) // Slot-Nr von Requested muss kleiner sein
        and ((aRequestedSlot mod self.GetSlotCountForOneFloor())
        = (aOtherSlot mod self.GetSlotCountForOneFloor())) then
        result := true;
end;

function TCarrier.GetSlotGroupCols: integer;
begin
    result := fCarrierRec.Cols;
end;

function TCarrier.GetSlotGroupRows: integer;
begin
    result := fCarrierRec.Rows;
end;

function TCarrier.GetSizeX: TPosMM;
begin
    result := fCarrierRec.SizeX_mm;
end;

function TCarrier.GetSizeY: TPosMM;
begin
    result := fCarrierRec.SizeY_mm;
end;

class function TCarrier.Turn180degree(aRotationValue: TRotationValue): TRotationValue;
begin
    case aRotationValue of
        rotation_90:
            result := rotation_270;
        rotation_180:
            result := rotation_0;
        rotation_270:
            result := rotation_90;
        else
            result := rotation_180;
    end;
end;

class function TCarrier.Turn90degree(aRotationValue: TRotationValue): TRotationValue;
begin
    case aRotationValue of
        rotation_90:
            result := rotation_180;
        rotation_180:
            result := rotation_270;
        rotation_270:
            result := rotation_0;
        else
            result := rotation_90;
    end;
end;

function AddZHeight(const aCurHeight, aAddedHeight: TPosMM): TPosMM;
begin
    result := TCoordCalculator.IncreaseZHeight(aCurHeight, aAddedHeight);
end;

function TCarrier.GetSlotBySlotNr(aSlotNr: integer): TCarrierSlot;
begin
    result := nil;
    if (aSlotNr <= 0) or (aSlotNr > self.SlotCount) then
        EXIT;
    result := fSlots[aSlotNr - 1];
end;

procedure TCarrier.DoInitGraphics;
begin
    fGraphics := TCarrierGraphics.Create();
end;

function TCarrier.GetGraphics: TCarrierGraphics;
begin
    result := fGraphics as TCarrierGraphics;
end;

function TCarrier.FindFreeSlot(): TSlotStruct;
var
    x: integer;
    xSlot: TCarrierSlot;
begin
    result.SlotNr := -1;

    // Die Suche erfolgt von unten nach oben, da es sich um einen Stapel handeln könnte
    for x := self.SlotCount downto 1 do
    begin
        xSlot := self.GetSlotBySlotNr(x);
        if not Assigned(xSlot.Rack) then
        begin // no rack found
            result.SlotNr := xSlot.SlotNr;
            result.CarrierName := self.Name;
            EXIT;
        end;
    end;
end;

procedure TCarrier.UpdateVisibleFloors();
var
    xSlot: TCarrierSlot;
    x: integer;
    xVisible: boolean;
begin
    for x := 1 to self.SlotCount do
    begin
        xSlot := self.GetSlotBySlotNr(x);
        xVisible := self.Visible and self.IsSlotOnVisibleFloor(xSlot.SlotNr);
        xSlot.Visible := xVisible;
    end;
end;

procedure TCarrier.DoAfterCreateCarrierSlot(aCarrierSlot: TCarrierSlot);
begin

end;

function TCarrier.CreateCarrierSlot: TCarrierSlot;
begin
    result := DoCreateCarrierSlot();
    result.Carrier := self;
    result.InitGraphics;
    DoAfterCreateCarrierSlot(result);
end;

function TCarrier.GetSlotGroupFloors: integer;
begin
    result := fCarrierRec.Floors;
end;

function TCarrier.GetSlotInfos: TSlotInfoArray;
var
    x: integer;
    xSlot: TCarrierSlot;
begin
    SetLength(result, self.SlotCount);
    for x := 1 to self.SlotCount do
    begin
        xSlot := self.GetSlotBySlotNr(x);
        result[x - 1].SlotNr := xSlot.SlotNr;
        result[x - 1].RackName := '';
        if Assigned(xSlot.Rack) then
            result[x - 1].RackName := xSlot.Rack.Name;
    end;
end;

procedure TCarrier.ChangeType(const aCarrierType: TCarrierRec);
begin
    SetType(aCarrierType);
end;

class function TCarrier.CreateCarrierTypeView(const aCarrierTypeRec: TCarrierRec): TCarrier;
begin
    result := TCarrier.Create();
    result.InitGraphics();
    result.SetType(aCarrierTypeRec);
    result.Graphics.CoordSystem.ReflectY := true;
    result.Graphics.CoordSystem.TranslateY := result.Graphics.CoordSystem.TranslateY +
        result.Graphics.GraphicsInfo.SizeY;
end;


end.
