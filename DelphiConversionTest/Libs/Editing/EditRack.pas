{ --------------------------------------------------------------------------------------------------
  SETUP
  --------------------------------------------------------------------------------------------------
  Rack Definitionsfenster für Setup Programm
  --------------------------------------------------------------------------------------------------
  Datum    op  function/procedure     Änderung / Neuerung
  -------- --  -------------------    --------------------------------------------------------------
  20.07.98 wl                         Type sichtbar gemacht
  24.07.98 wl                         X,Y eingetragen
  25.08.98 wl  PaintRack              Rack laden mit Zoom = 0,8, danach Zoom wieder normal
  13.11.98 wl                         mehr Platz für das Rack und Platz für weitere Parameter
  19.11.98 wl                         erscheint nicht mehr maximiert
  08.01.99 wl                         Tab-Reihenfolge geändert
  DBRACK und DataSource aus LDBModul
  DBCARRIERBeforePost    Racks ohne Namen werden nicht gespeichert
  Rack Type reaktiviert
  09.03.99 wl  CopyRackClick          Funktion stark vereinfacht -> alle Felder werden kopiert
  16.03.99 wl  CopyRackClick          Name wird auf gMaxNameLen.RackType gekürzt
  18.08.99 wz                         --> Strings in Ressourcen
  02.09.99 wl  TubeGeoGroupClick      Überflüssige Zeile gelöscht
  14.09.99 wl                         neu: Move Offset Height, Move Offset Up-X, Up-Y
  14.02.00 wl                         Einfache Oberfläche mit OK/Cancel
  22.02.00 wl                         Delete-, New-, Rename- und Copy-Funktion entfernt
  Löschen, Kopieren, Einfügen --> LayMain
  22.03.00 wl  FormShow               Caption des Fensters angepaßt
  03.04.00 wl  FormShow,TubeGeoGroupClick  Anzeigen und ändern von Recteck/Kreis funktioniert jetzt
  10.08.00 wl                         --> String in Ressourcen
  06.02.03 wl  btnOKClick             TN1334.3 Every 'Post' will be logged (with reason)
  06.02.03 wl  FormShow               TN1334.3 if user is not Admin, 'OK' is not enabled
  12.02.03 wl  FormShow               TN1334.3 if user is not Admin, DataSource is ReadOnly
  19.02.03 wl  FormShow               TN1345   bei Sias-Geräten wird Rotation mit angezeigt
  12.03.03 wl                         TN1293.5 uses posTools
  17.06.03 tbh Formular               TN1501.4/TN1502/TN1503.1  umgebaut und ergänzt
  17.06.03 tbh FormShow               TN1503.1 Farbe wird gesetzt und dargestellt
  17.06.03 tbh TubeGeoGroupClick      TN1502   Außendurchmesser ergänzt / Ressourcen angepasst
  17.06.03 tbh btnOKClick             TN1501.4 Datenprüfung für schräge Racks erweiter
  17.06.03 tbh btRackColorClick       TN1503.1 neu: Öffnet Farbdialog für Racktypfarbe
  23.06.03 wl                         TN1501   Cap-Type auf ReadOnly = false gesetzt
  03.07.03 wl                         TN1501   Compilerhinweise korrigiert
  21.07.03 wl                         TN1502   TabOrder korrigiert
  12.09.03 wl                         TN1581.7 'H_Rotation' in Racks.db abgeschafft
  29.09.03 wl  btnOKClick             TN1501.4 Datenprüfung für schräge Racks entfernt
  30.09.03 wl                         TN1501.4 schräge Racks: neu: SlopeType und Z_LastPos_mm
  13.11.03 wl  btnApplyClick          TN1581.19 neu: Darstellung des Racks mit "Übernehmen"-Button zur sofortigen Ansicht
  13.11.03 wl                         TN1664    Rotation-Feld für ZP02: H_Direction
  20.11.03 wl  btnApplyClick          TN1581.19 Buttons Windows-konform
  17.02.03 pk  DisplayRack            TN1749    New parameter for SetTypeName
  16.03.04 wl                         TN1754.1  Neue Felder TubeGet.. TubePut..
  27.10.04 wl                         TN2071    HRTake_degree statt Rack direction
  25.01.05 tbh Formular               TN2293    Neues Feld "Balance Door" in "Blocks Movement"
  11.03.05 wl  FormShow               TN2293.1  "Balance Door" wird bei neuen Datensatz immer auf false gesetzt
  24.08.06 wl                         TN3269    neues Feld: StackHeightZ_mm
  04.05.07 wl  SaveChanges            TN3669    benutzt TAppSettings.ItemConfirmEdit/Add
  09.11.07 pk                         TN3922    Dataset changed to DataProvider
  09.11.07 pk                         TN3924    SpecialUses Tubhandling changed to mm
  20.06.08 pk                         TN4139    Graphics now drawn to fSceneGraphics
  27.06.08 pk                         TN4139    DB controls removed. Now based on TRackRec
  11.07.08 wl                         TN4164    Struktur geändert, damit auch im Designer verwendbar
  29.10.08 wl                         TN4290    Neues Feld "Z Offset: Get/put tube"
  09.02.09 wl  TRackEditorStringLoader  TN4370  neu: dezentrale Resourcen
  11.03.09 pk  DisplayRack            TN4457    Call CreateRackTypeView
  10.08.09 wl                         TN4702   Strings werden jetzt direkt geladen
  05.11.09 pk  SaveChanges            TN4834    raise exception if type already exists
  13.04.10 wl                         TN5044   uses geändert
  30.04.10 wl                         TN5070    bei Traysy-Lizenz werden die meisten Parameter ausgeblendet
  20.05.10 wl                         TN5117   neue Schriftart "Segoe UI", Schriftgröße 9
  21.06.10 wl                               TN5160   Position = poScreenCenter
  11.11.10 wl                               TN5212   Geänderte Beschriftung: "X: After Column"
  10.02.11 wl                               TN5474   Neues Format
  24.02.11 wl                         TN5431   3 neue Felder: WellsAroundCenter (boolean), CenterX_mm, CenterY_mm
  29.03.11 wl  MakeDefaultRackRec           TN5431   --> RackDataAdaptor
  05.05.11 ts                               TN5552   new: TubeGripOffset_mm
  21.07.11 wl  FormCreate                   TN5614   User-Verwaltung greift jetzt
  05.09.11 ts                               TN5683   new: DoNotPaintTubes
  10.11.11 ts                               TN5702   new: Rack with discrete positions
  10.09.12 ts                               TN5977   new: RackMoveZTravelOffset
  03.12.12 pp  GetDiscretePos               TN6007   new: btnConvertDiscretePos
  29.04.13 pp  SaveChanges                  TN6143   FreeAndNil korrigiert
  10.06.13 pp  ...From/ToGrid Dis(EnableXY) TN6167   Anpassungen für DiscreteRacks
  27.11.13 wl                               TN6313   verwendet  cTubeTypeSingle und cTubeTypeMultiTipTube
  -------------------------------------------------------------------------------------------------- }

unit EditRack;


interface


uses
    Windows,
    Messages,
    Classes,
    Controls,
    Forms,
    StdCtrls,
    ExtCtrls,
    SysUtils,
    ComCtrls,
    Dialogs,
    Rack,
    RackDataAdaptor,
    DiscreteRackDataAdaptor,
    SceneGraphics,
    StringLoader,
    cxGraphics,
    cxControls,
    cxLookAndFeels,
    cxLookAndFeelPainters,
    cxStyles,
    cxCustomData,
    cxFilter,
    cxData,
    cxDataStorage,
    cxEdit,
    Menus,
    cxGridLevel,
    cxGridCustomTableView,
    cxGridTableView,
    cxClasses,
    cxGridCustomView,
    cxGrid;

type
    TRackEditorStringLoader = class(TTextListStringLoader)
    protected
        procedure AddAllItems(); override;
    public const
        cFStringRackType = 51360;
        cStringNewRackType = 51350;
        cStringDiameterInside = 51030;
        cStringDiameterOutside = 51060;
        cStringLength = 51040;
        cStringWidthInside = 51050;
        cStringWidthOutside = 51070;
    end;

    TfrmEdRack = class(TForm)
        ColorDialog1: TColorDialog;
        Panel3: TPanel;
        btnApply: TButton;
        btnOK: TButton;
        btnCancel: TButton;
        Panel2x: TPanel;
        PageControl1: TPageControl;
        TabSheet1: TTabSheet;
        RackTyp: TGroupBox;
        Label11: TLabel;
        lblTypeNo: TLabel;
        edTypeName: TEdit;
        edTypeNo: TEdit;
        gbAbmessungen: TGroupBox;
        Label1: TLabel;
        Label2: TLabel;
        lblSizeZ: TLabel;
        edSizeX: TEdit;
        edSizeY: TEdit;
        edSizeZ: TEdit;
        GroupBox1: TGroupBox;
        Label4: TLabel;
        Label5: TLabel;
        edRows: TEdit;
        edCols: TEdit;
        gbZHeights: TGroupBox;
        ZTravelMM: TLabel;
        ZScanMM: TLabel;
        ZDispMM: TLabel;
        ZMaxMM: TLabel;
        EdZScan: TEdit;
        EdZDisp: TEdit;
        EdZMax: TEdit;
        edZTravel: TEdit;
        TubeGeoGroup: TRadioGroup;
        TubeMeasure: TGroupBox;
        lblTubeX: TLabel;
        lblTubeY: TLabel;
        lblTubeZ: TLabel;
        lblTubeYOut: TLabel;
        edTubeX: TEdit;
        edTubeY: TEdit;
        edTubeZ: TEdit;
        EdTubeYOut: TEdit;
        gbTubepos: TGroupBox;
        PosXFirst: TLabel;
        Label10: TLabel;
        Label7: TLabel;
        Label9: TLabel;
        EdPosXFirst: TEdit;
        EdPosYFirst: TEdit;
        edPosXLast: TEdit;
        EdPosYLast: TEdit;
        GroupBox7: TGroupBox;
        TabSheet2: TTabSheet;
        HandlerPara: TGroupBox;
        HXTake: TLabel;
        HYTake: TLabel;
        HZTake: TLabel;
        EdHXTakeMM: TEdit;
        EdHYTakeMM: TEdit;
        edHZTakeMM: TEdit;
        GroupBox2: TGroupBox;
        HVOpen: TLabel;
        HVClose: TLabel;
        TabSheet3: TTabSheet;
        GroupBox3: TGroupBox;
        Label14: TLabel;
        Label18: TLabel;
        GroupBox9: TGroupBox;
        Label22: TLabel;
        Label19: TLabel;
        Label21: TLabel;
        Label20: TLabel;
        gbSlantedRack: TGroupBox;
        Label23: TLabel;
        TabSheet4: TTabSheet;
        GroupBox4: TGroupBox;
        Label6: TLabel;
        Label8: TLabel;
        Label13: TLabel;
        edMOffsetUpX: TEdit;
        edMOffsetUpY: TEdit;
        edMOffsetZ: TEdit;
        GroupBox5: TGroupBox;
        Label15: TLabel;
        Label16: TLabel;
        GroupBox6: TGroupBox;
        Label28: TLabel;
        Label29: TLabel;
        Label30: TLabel;
        edCapType: TEdit;
        edCapDiameter: TEdit;
        edCapGripHeight: TEdit;
        GroupBox10: TGroupBox;
        Label17: TLabel;
        Label24: TLabel;
        Label25: TLabel;
        Label26: TLabel;
        GroupBox11: TGroupBox;
        cbBalanceDoor: TCheckBox;
        GroupBox12: TGroupBox;
        Label27: TLabel;
        rdoTubeType: TRadioGroup;
        edHRTake: TEdit;
        edVOpen_mm: TEdit;
        edVClose_mm: TEdit;
        edPosX_Offset_mm: TEdit;
        edPosY_Offset_mm: TEdit;
        edAddOffsetX_AfterPos: TEdit;
        edAddOffsetY_AfterPos: TEdit;
        rdoSlopeType: TRadioGroup;
        edZ_LastPos_mm: TEdit;
        edShift_Radius_mm: TEdit;
        edShift_NoOfSteps: TEdit;
        edTubeGetOpen_mm: TEdit;
        edTubeGetClose_mm: TEdit;
        edTubePutOpen_mm: TEdit;
        edStackHeight_mm: TEdit;
        edAddOffsetX: TEdit;
        edAddOffsetY: TEdit;
        Label31: TLabel;
        edTubePutOffset_mm: TEdit;
        btnColor: TButton;
        Shape1: TShape;
        GroupBox8: TGroupBox;
        Label3: TLabel;
        Label12: TLabel;
        edCenterX_mm: TEdit;
        edCenterY_mm: TEdit;
        cbWellsAroundCenter: TCheckBox;
        Label32: TLabel;
        edTubeGripOffset_mm: TEdit;
        cbDoNotPaintTubes: TCheckBox;
        GroupBox13: TGroupBox;
        cbDiscretePositions: TCheckBox;
        TabSheet5: TTabSheet;
        cxGrid1: TcxGrid;
        cxGrid1TableView1: TcxGridTableView;
        cxGrid1TableView1Column1: TcxGridColumn;
        cxGrid1TableView1Column2: TcxGridColumn;
        cxGrid1TableView1Column3: TcxGridColumn;
        cxGrid1Level1: TcxGridLevel;
        GroupBox14: TGroupBox;
        Label33: TLabel;
        edRackZTravelOffset: TEdit;
        btnConvertDiscretePos: TButton;
        procedure FormCreate(Sender: TObject);
        procedure FormClose(Sender: TObject; var Action: TCloseAction);
        procedure TubeGeoGroupClick(Sender: TObject);
        procedure FormShow(Sender: TObject);
        procedure btnCancelClick(Sender: TObject);
        procedure btnOKClick(Sender: TObject);
        procedure btnColorClick(Sender: TObject);
        procedure btnApplyClick(Sender: TObject);
        procedure FormDestroy(Sender: TObject);
        procedure cbDiscretePositionsClick(Sender: TObject);
        procedure DiscretePositionsToGrid(aNumberOfPos: integer; aDiscretePosRecArray: TDiscretePosRecArray);
        procedure DiscretePositionsFromGrid(aNumberOfPos: integer;
            var aDiscretePosRecArray: TDiscretePosRecArray);
        procedure btnConvertRackInDiscretePosClick(Sender: TObject);
    private const
        cTubeGeometryCircle = 0;
        cTubeGeometryRect = 1;
    private
        fTypeName: string;
        fTubeGeometryMode: integer;
        FRack: TRack;
        FApplied: boolean;
        fSceneGraphics: TSceneGraphics;
        fCurrentName: string;
        fStringLoader: TRackEditorStringLoader;
        fRackRec: TRackRec;
        function SaveChanges: boolean;
        procedure DisplayRack;
        procedure RackRecFromGUI(var vRackRec: TRackRec);
        procedure LoadDataToGUI(const aTypeName: string);
        procedure RackRecToGUI(const aRackRec: TRackRec);
        procedure SetTubeGeometryMode(aMode: integer);
        function GetDiscretePos: TDiscretePosRecArray;
        procedure EnableFieldsFirstLastXY;
        // Prozedur soll noch das Aktivieren der Felder First,LastXY übernehmen
        procedure DisableFieldsFirstLastXY;
        // Somit sind die Felder editierbar, sobald man Discrete rausnimmt ohne OK oder APPLY anzuklicken!
    public
        property CurrentName: string read fCurrentName write fCurrentName;

        class function InstanceShowToAdd(out oName: string): boolean;
        class function InstanceShowToEdit(const aName: string): boolean;
    end;


implementation


{$R *.DFM}

uses
    AppSettings,
    CommonTypes,
    GeneralTypes,
    ControlUtils,
    LayoutElementGraphicsDriverTypeManager;

{ TRackEditorStringLoader }

procedure TRackEditorStringLoader.AddAllItems;
begin
    AddStandardButtonStrings();

    AddSingle(50140, 'Name', 'Name');
    AddSingle(50150, 'Type', 'Typ');
    AddSingle(50170, 'Length (X)', 'Länge (X)');
    AddSingle(50180, 'Width (Y)', 'Breite (Y)');
    AddSingle(50190, 'Height (Z)', 'Höhe (Z)');
    AddSingle(50210, 'Rows', 'Reihen');
    AddSingle(50220, 'Columns', 'Spalten');
    AddSingle(50330, 'First Position (X)', 'Erste Position (X)');
    AddSingle(50340, 'First Position (Y)', 'Erste Position (Y)');
    AddSingle(50360, 'Last Position (X)', 'Letzte Position (X)');
    AddSingle(50370, 'Last Position (Y)', 'Letzte Position (Y)');
    AddDouble(51000, 'Save current rack?', 'Save Rack', 'Soll das aktuelle Rack gespeichert werden?',
        'Rack speichern');
    AddDouble(51010, 'Insert new rack type name!', 'New Rack Type',
        'Bitte geben Sie einen neuen Racktyp-Namen ein!', 'Neuer Racktyp');
    AddSingle(cStringDiameterInside, 'Diameter -inside-', 'Durchmesser innen');
    AddSingle(cStringLength, 'Length (X)', 'Länge (X)');
    AddSingle(cStringWidthInside, 'Width -inside- (Y)', 'Breite -innen- (Y)');
    AddSingle(cStringDiameterOutside, 'Diameter -outside-', 'Durchmesser außen');
    AddSingle(cStringWidthOutside, 'Width -outside- (Y)', 'Breite -außen- (Y)');
    AddSingle(51080, 'Insert Rack Name!', 'Bitte geben Sie einen Racknamen ein!');
    AddSingle(51090, 'Rack Type', 'Racktyp');
    AddSingle(51100, 'Rack Dimensions [mm] ', 'Rack Dimensionen [mm] ');
    AddSingle(51110, 'Heights (Z)  [mm] rel. to rack', 'Höhe (Z)  [mm] rel. zum Rack');
    AddSingle(51120, 'Travel ', 'Travel ');
    AddSingle(51130, 'Scan', 'Scan');
    AddSingle(51140, 'Dispense', 'Abgabe');
    AddSingle(51150, 'Maximum', 'Maximum');
    AddDouble(51160, 'Tube - Type', 'SingleTip Tube;Mutli Tip Tube', 'Tube - Typ',
        'Einzel Spitze Tube;Multi Spitzen Tube');
    AddDouble(51170, 'Tube - Geometry', 'Circle;Rectangle', 'Tube - Geometrie', 'Kreis;Rechteck');
    AddSingle(51180, 'Tube - Dimensions [mm]', 'Tube - Dimensionen [mm]');
    AddSingle(51190, 'X - Diameter', 'X-Durchmesser');
    AddSingle(51200, 'Y - Diameter', 'Y-Durchmesser');
    AddSingle(51210, 'Height (Z)', 'Höhe (Z)');
    AddSingle(51220, 'Tube - Positions [mm]', 'Tube - Positionen [mm]');
    AddSingle(51230, 'Staggered rows/colums [mm]', 'Versetzte Reihen/Kolonnen [mm]');
    AddSingle(51240, 'Offset between 2 rows/columns', 'Offset zwischen 2 Reihen/Kolonnen');
    AddSingle(51250, 'X: After Column:', 'X: Nach Spalte:');
    AddSingle(51260, 'Y: After Row:', 'Y: Nach Reihe:');
    AddSingle(51270, 'Handler values - relative to rack', 'Greifer-Werte - relativ zum Rack');
    AddSingle(51280, 'X - Take [mm]:', 'X - Aufnahme [mm]:');
    AddSingle(51290, 'Y - Take [mm]:', 'Y - Aufnahme [mm]:');
    AddSingle(51300, 'Z - Take [mm]:', 'Z - Aufnahme [mm]:');
    AddSingle(51310, 'Gripper [mm]', 'Greifer [mm]');
    AddSingle(51320, 'Max. Open', 'Max. Offen');
    AddSingle(51330, 'Max. Close', 'Max. Geschlossen');
    AddSingle(51340, 'Rack Type Definition', 'Racktyp Definition');
    AddSingle(cStringNewRackType, 'New Rack Type', 'Neuer Racktyp');
    AddSingle(cFStringRackType, 'Rack Type: %s', 'Racktyp: %s');
    AddSingle(51370, 'Create virtual carrier', 'Virtuellen Carrier erzeugen');
    AddSingle(51380, 'Move Offset (Z)', 'Move Offset (Z)');
    AddSingle(51390, 'Move Offset [mm/rel. to posit.]', 'Move Offset [mm/rel. zur Pos.]');
    AddSingle(51400, 'Move up to X', 'in X bewegen');
    AddSingle(51410, 'Move up to Y', 'in Y bewegen');
    AddSingle(51420, 'Aspiration Shifting', 'Aspiration Shifting');
    AddSingle(51430, 'Radius [mm]', 'Radius [mm]');
    AddSingle(51440, 'No Of Positions', 'Zahl der Positionen');
    AddSingle(51450, 'Slanted Rack Height [mm]', 'Höhe (Schräges Rack) [mm]');
    AddDouble(51460, '', 'No Slope;Slope in X;Slope in Y', '', 'Keine Steigung;Steigung in X;Steigung in Y');
    AddSingle(51470, 'Height (Last Pos.)', 'Höhe (Letzte Pos.)');
    AddSingle(51490, 'Basic', 'Basis');
    AddSingle(51495, 'Rack Gripping', 'Rackgreifen');
    AddSingle(51500, 'Special Forms', 'Spezialformen');
    AddSingle(51505, 'Special Uses', 'Spezialanwendungen');
    AddSingle(51510, 'Tube Caps', 'Deckel');
    AddSingle(51520, 'Type', 'Typ');
    AddSingle(51530, 'Diameter [mm]', 'Durchmesser [mm]');
    AddSingle(51540, 'Grip Height [mm]', 'Greifhöhe   [mm]');
    AddSingle(51550, 'Color', 'Farbe');
    AddSingle(51560, 'Change Color', 'Farbe ändern');
    AddSingle(51570, 'X - Offset (2nd row)', 'X - Offset (2. Reihe)');
    AddSingle(51580, 'Y - Offset (2nd column):', 'Y - Offset (2. Kolonne)');
    AddSingle(51590, 'Direction [degree]:', 'Greifrichtung [Grad]:');
    AddSingle(51600, 'Tube Handling [mm] - optional', 'Tube-Transport [mm] - optional');
    AddSingle(51610, 'V-Open (Get Tube)', 'V-Open, Tube nehmen');
    AddSingle(51615, 'V-Close (Get Tube)', 'V-Close, Tube nehmen');
    AddSingle(51620, 'V-Open (Put Tube)', 'V-Open, Tube abstellen');
    AddSingle(51625, 'Z Offset (Put Tube)', 'Z-Offset abstellen');
    AddSingle(51630, 'Blocks Movement', 'Behindert Bewegung');
    AddSingle(51635, 'Z Offset: Grip (Put/Get Tube)', 'Z-Offset Greifen');
    AddSingle(51640, 'Balance Door', 'Waagentür');
    AddSingle(51650, 'Stacking', 'Stapeln');
    AddSingle(51660, 'Stack Height [mm]', 'Stapelhöhe [mm]');
    AddDouble(51800, 'Please enter a reason for your changes:', 'Save changes',
        'Please enter a reason for your changes:', 'Save changes');
    AddDouble(51810, 'The tube height Z is smaller than Z-Max, which is not possible! Save anyway?',
        'Confirmation',
        'Die Tube Höhe Z ist kleiner als Z-Max, was nicht möglich ist! Soll dennoch gespeichert werden?',
        'Bestätigung');
    AddDouble(51820,
        'The top most z position of a slanted rack can not be higher than the total z height of the rack!',
        'Data Error',
        'Die Höchste Position eines schrägen Racks kann nicht höher sein als die Gesammthöhe des Racks!',
        'Datenfehler');
    AddDouble(51830, 'A rack can only be slanted in one direction X or Y!', 'Data Error',
        'Ein Rack darf nur in einer Richtung schräg sein X oder Y!', 'Datenfehler');
    AddSingle(51840, 'Do not paint tubes', 'Tubes nicht zeichnen');
    AddSingle(51850, 'Discrete positions', 'Diskrete Positionen');
    AddSingle(51855, 'Rack with discrete positions', 'Rack mit diskreten Positionen');
    AddSingle(51860, 'Rack movement', 'Rackbewegungen');
    AddSingle(51865, 'Convert rack in discrete pos', 'Wandle Rack in diskrete Pos um');
end;

{ TfrmEdRack }

procedure TfrmEdRack.FormCreate(Sender: TObject);
begin
    TControlUtils.ResetFontForWinXP(self);
    fStringLoader := TRackEditorStringLoader.Create;
    fStringLoader.LoadLanguage(self);

    fSceneGraphics := TSceneGraphics.Create(Panel2x);
    fSceneGraphics.Visible := true;
    fTypeName := '';
    fTubeGeometryMode := cTubeGeometryCircle;

    btnOK.Visible := gCommonDll.LevelIsIncluded(gCommonDll.CurrentUser.Level, usrSystemAdmin);
    btnApply.Visible := gCommonDll.LevelIsIncluded(gCommonDll.CurrentUser.Level, usrSystemAdmin);

    // für Traysy unnötige Dinge ausblenden:
    if TAppSettings.IsTraySy then
    begin
        self.PageControl1.Pages[1].TabVisible := false;
        self.PageControl1.Pages[3].TabVisible := false;

        self.gbSlantedRack.Visible := false;
        self.gbZHeights.Visible := false;
        self.rdoTubeType.Visible := false;
        self.lblSizeZ.Visible := false;
        self.edSizeZ.Visible := false;
        self.lblTubeZ.Visible := false;
        self.edTubeZ.Visible := false;
        self.lblTubeYOut.Visible := false;
        self.edTubeYOut.Visible := false;
        self.lblTypeNo.Visible := false;
        self.edTypeNo.Visible := false;
    end;
end;

procedure TfrmEdRack.FormDestroy(Sender: TObject);
begin
    fSceneGraphics.Free;
    fStringLoader.Free;
end;

procedure TfrmEdRack.FormShow(Sender: TObject);
begin
    PageControl1.ActivePageIndex := 0;
    LoadDataToGUI(fCurrentName);
end;

procedure TfrmEdRack.FormClose(Sender: TObject; var Action: TCloseAction);
begin
    FreeAndNil(FRack);
    if (FApplied) then
        ModalResult := mrOK;
end;

procedure TfrmEdRack.RackRecFromGUI(var vRackRec: TRackRec);
var
    x: integer;
begin
    with vRackRec do
    begin
        name := edTypeName.Text;
        Typ := StrToInt(edTypeNo.Text);
        X_mm := StrToFloat(edSizeX.Text);
        Y_mm := StrToFloat(edSizeY.Text);
        Z_mm := StrToFloat(edSizeZ.Text);
        Rows := StrToInt(edRows.Text);
        Cols := StrToInt(edCols.Text);

        ZTravel_mm := StrToFloat(self.edZTravel.Text);
        ZScan_mm := StrToFloat(self.edZScan.Text);
        ZDisp_mm := StrToFloat(self.edZDisp.Text);
        ZMax_mm := StrToFloat(self.edZMax.Text);

        if self.rdoTubeType.ItemIndex = 0 then
            TubeTyp := cTubeTypeSingle
        else
            TubeTyp := cTubeTypeMultiTipTube;

        TubeX_mm := StrToFloat(edTubeX.Text);
        TubeY_mm := StrToFloat(edTubeY.Text);
        TubeZ_mm := StrToFloat(edTubeZ.Text);
        TubeY2_mm := StrToFloat(edTubeYOut.Text);

        PosX_Offset_mm := StrToFloat(self.edPosX_Offset_mm.Text);
        PosY_Offset_mm := StrToFloat(self.edPosY_Offset_mm.Text);

        Color := self.Shape1.Brush.Color;

        H_XTake_mm := StrToFloat(self.edHXTakeMM.Text);
        H_YTake_mm := StrToFloat(self.edHYTakeMM.Text);
        H_ZTake_mm := StrToFloat(self.edHZTakeMM.Text);
        H_RTake_Degree := StrToFloat(self.edHRTake.Text);
        H_VOpen_mm := StrToFloat(self.edVOpen_mm.Text);
        H_VClose_mm := StrToFloat(self.edVClose_mm.Text);
        H_VIsUndefined := (edVOpen_mm.Text = '') and (edVClose_mm.Text = '');

        AddOffsetX_AfterPos := StrToInt(self.edAddOffsetX_AfterPos.Text);
        AddOffsetX_mm := StrToFloat(self.edAddOffsetX.Text);
        AddOffsetY_AfterPos := StrToInt(self.edAddOffsetY_AfterPos.Text);
        AddOffsetY_mm := StrToFloat(self.edAddOffsetY.Text);
        MOffsetZ_mm := StrToFloat(self.edMOffsetZ.Text);
        MOffsetUpX_mm := StrToFloat(self.edMOffsetUpX.Text);
        MOffsetUpY_mm := StrToFloat(self.edMOffsetUpY.Text);
        Shift_Radius_mm := StrToFloat(self.edShift_Radius_mm.Text);
        Shift_NoOfSteps := StrToInt(self.edShift_NoOfSteps.Text);

        CapType := self.edCapType.Text;
        CapDiameter := StrToFloat(self.edCapDiameter.Text);
        Cap_GripZ_mm := StrTofloat(self.edCapGripHeight.Text);

        Z_LastPos_mm := StrToFloat(edZ_LastPos_mm.Text);
        SlopeType := self.rdoSlopeType.ItemIndex;

        TubeGetOpen_mm := StrToFloat(self.edTubeGetOpen_mm.Text);
        TubeGetClose_mm := StrToFloat(self.edTubeGetClose_mm.Text);
        TubePutOpen_mm := StrToFloat(self.edTubePutOpen_mm.Text);
        TubePutOffset_mm := StrToFloat(self.edTubePutOffset_mm.Text);
        TubeGripOffset_mm := StrToFloat(self.edTubeGripOffset_mm.Text);

        BlockBalanceDoor := self.cbBalanceDoor.Checked;
        StackHeight_mm := StrToFloat(self.edStackHeight_mm.Text);

        WellsAroundCenter := self.cbWellsAroundCenter.Checked;
        CenterX_mm := StrToFloat(edCenterX_mm.Text);
        CenterY_mm := StrToFloat(edCenterY_mm.Text);
        DoNotPaintTubes := self.cbDoNotPaintTubes.Checked;
        RackMoveZTravelOffset := StrToFloat(self.edRackZTravelOffset.Text);
        DiscretePositions := self.cbDiscretePositions.Checked;
    end;

    if cbDiscretePositions.Checked then
    begin
        btnConvertDiscretePos.Enabled := false;
        if cxGrid1TableView1.DataController.RecordCount > vRackRec.Cols then
            for x := cxGrid1TableView1.DataController.RecordCount downto vRackRec.Cols + 1 do
            begin
                cxGrid1TableView1.DataController.DeleteRecord(x - 1);
            end;
        self.PageControl1.Pages[4].TabVisible := true;
        self.DiscretePositionsFromGrid(vRackRec.Cols, vRackRec.DiscretePosRecArray);
    end
    else
    begin
        self.PageControl1.Pages[4].TabVisible := false;
        btnConvertDiscretePos.Enabled := true;
        vRackRec.PosX_First_mm := StrToFloat(self.edPosXFirst.Text);
        vRackRec.PosY_First_mm := StrToFloat(self.edPosYFirst.Text);
        vRackRec.PosX_Last_mm := StrToFloat(self.edPosXLast.Text);
        vRackRec.PosY_Last_mm := StrToFloat(self.edPosYLast.Text);

        fRackRec := vRackRec;
    end;
end;

procedure TfrmEdRack.RackRecToGUI(const aRackRec: TRackRec);
begin
    with aRackRec do
    begin
        edTypeName.Text := name;
        edTypeNo.Text := IntToStr(Typ);
        edSizeX.Text := FloatToStr(X_mm);
        edSizeY.Text := FloatToStr(Y_mm);
        edSizeZ.Text := FloatToStr(Z_mm);
        edRows.Text := IntToStr(Rows);
        edCols.Text := IntToStr(Cols);

        edZTravel.Text := FloatToStr(ZTravel_mm);
        edZScan.Text := FloatToStr(ZScan_mm);
        edZDisp.Text := FloatToStr(ZDisp_mm);
        edZMax.Text := FloatToStr(ZMax_mm);

        if TubeTyp = cTubeTypeSingle then
            rdoTubeType.ItemIndex := 0
        else
            rdoTubeType.ItemIndex := 1;

        if TubeX_mm = 0 then
        begin
            SetTubeGeometryMode(cTubeGeometryCircle)
        end
        else
        begin
            SetTubeGeometryMode(cTubeGeometryRect);
            edTubeX.Text := FloatToStr(TubeX_mm);
        end;

        edTubeY.Text := FloatToStr(TubeY_mm);
        edTubeZ.Text := FloatToStr(TubeZ_mm);
        edTubeYOut.Text := FloatToStr(TubeY2_mm);

        edPosX_Offset_mm.Text := FloatToStr(PosX_Offset_mm);
        edPosY_Offset_mm.Text := FloatToStr(PosY_Offset_mm);

        Shape1.Brush.Color := Color;

        edHXTakeMM.Text := FloatToStr(H_XTake_mm);
        edHYTakeMM.Text := FloatToStr(H_YTake_mm);
        edHZTakeMM.Text := FloatToStr(H_ZTake_mm);
        edHRTake.Text := FloatToStr(H_RTake_Degree);
        edVOpen_mm.Text := FloatToStr(H_VOpen_mm);
        edVClose_mm.Text := FloatToStr(H_VClose_mm);

        edAddOffsetX_AfterPos.Text := IntToStr(AddOffsetX_AfterPos);
        edAddOffsetX.Text := FloatToStr(AddOffsetX_mm);
        edAddOffsetY_AfterPos.Text := IntToStr(AddOffsetY_AfterPos);
        edAddOffsetY.Text := FloatToStr(AddOffsetY_mm);
        edMOffsetZ.Text := FloatToStr(MOffsetZ_mm);
        edMOffsetUpX.Text := FloatToStr(MOffsetUpX_mm);
        edMOffsetUpY.Text := FloatToStr(MOffsetUpY_mm);
        edShift_Radius_mm.Text := FloatToStr(Shift_Radius_mm);
        edShift_NoOfSteps.Text := IntToStr(Shift_NoOfSteps);

        edCapType.Text := CapType;
        edCapDiameter.Text := FloatToStr(CapDiameter);
        edCapGripHeight.Text := FloatToStr(Cap_GripZ_mm);

        edZ_LastPos_mm.Text := FloatToStr(Z_LastPos_mm);
        rdoSlopeType.ItemIndex := SlopeType;

        edTubeGetOpen_mm.Text := FloatToStr(TubeGetOpen_mm);
        edTubeGetClose_mm.Text := FloatToStr(TubeGetClose_mm);
        edTubePutOpen_mm.Text := FloatToStr(TubePutOpen_mm);
        edTubePutOffset_mm.Text := FloatToStr(TubePutOffset_mm);
        edTubeGripOffset_mm.Text := FloatToStr(TubeGripOffset_mm);

        cbBalanceDoor.Checked := BlockBalanceDoor;
        edStackHeight_mm.Text := FloatToStr(StackHeight_mm);

        self.cbWellsAroundCenter.Checked := WellsAroundCenter;
        edCenterX_mm.Text := FloatToStr(CenterX_mm);
        edCenterY_mm.Text := FloatToStr(CenterY_mm);
        cbDoNotPaintTubes.Checked := DoNotPaintTubes;
        self.edRackZTravelOffset.Text := FloatToStr(RackMoveZTravelOffset);
        cbDiscretePositions.Checked := DiscretePositions;
    end;

    if cbDiscretePositions.Checked then
    begin
        btnConvertDiscretePos.Enabled := false;
        self.PageControl1.Pages[4].TabVisible := true;
        self.DiscretePositionsToGrid(aRackRec.Cols, aRackRec.DiscretePosRecArray);
        edPosXFirst.Text := '';
        edPosYFirst.Text := '';
        edPosXLast.Text := '';
        edPosYLast.Text := '';
    end
    else
    begin
        self.PageControl1.Pages[4].TabVisible := false;
        btnConvertDiscretePos.Enabled := true;
        edPosXFirst.Text := FloatToStr(aRackRec.PosX_First_mm);
        edPosYFirst.Text := FloatToStr(aRackRec.PosY_First_mm);
        edPosXLast.Text := FloatToStr(aRackRec.PosX_Last_mm);
        edPosYLast.Text := FloatToStr(aRackRec.PosY_Last_mm);
        fRackRec := aRackRec;
    end;
    self.DisplayRack();
end;

procedure TfrmEdRack.LoadDataToGUI(const aTypeName: string);
var
    xTypeFound: boolean;
    xRackRec: TRackRec;
    xDA: TRackDataAdaptor;
    xRackPositions: TDiscretePosRecArray;
    xDADiscrete: TDiscreteRackDataAdaptor;
    x: integer;
begin
    xTypeFound := false;
    if aTypeName <> '' then
    begin
        xDA := TRackDataAdaptor.Create;
        try
            ASSERT(xDA.ReadRack(aTypeName, xRackRec));
        finally
            FreeAndNil(xDA);
        end;
        xTypeFound := true;
        fTypeName := aTypeName;
        self.Caption := fStringLoader.GetResFString(TRackEditorStringLoader.cFStringRackType, [aTypeName]);
    end;

    if not xTypeFound then
    begin
        self.Caption := fStringLoader.GetResString(TRackEditorStringLoader.cStringNewRackType);
        xRackRec := TRackDataAdaptor.MakeDefaultRackRec();
        xRackRec.Name := aTypeName;
    end;

    if xRackRec.DiscretePositions then
    begin
        xDADiscrete := TDiscreteRackDataAdaptor.Create;
        try
            // Records aus Table auslesen
            xDADiscrete.ReadRecs(aTypeName, xRackPositions);
            xRackRec.DiscretePosRecArray := xRackPositions;
        finally
            FreeAndNil(xDADiscrete);
        end;
        if ( high(xRackPositions) + 1) < xRackRec.Cols then
        begin
            SetLength(xRackRec.DiscretePosRecArray, xRackRec.Cols);
            for x := high(xRackPositions) + 1 to xRackRec.Cols - 1 do
            begin
                xRackRec.DiscretePosRecArray[x] := TDiscreteRackDataAdaptor.MakeDefaultRackRec(x + 1);
            end;
        end;
        self.DiscretePositionsToGrid(xRackRec.Cols, xRackRec.DiscretePosRecArray);
    end;
    edTypeName.ReadOnly := xTypeFound;
    self.RackRecToGUI(xRackRec);
end;

procedure TfrmEdRack.SetTubeGeometryMode(aMode: integer);
begin
    fTubeGeometryMode := aMode;
    if aMode = cTubeGeometryCircle then
    begin
        edTubeX.Text := '0';
        edTubeX.Visible := False;
        lblTubeX.Visible := False;
        lblTubeY.Caption := fStringLoader.GetResString(TRackEditorStringLoader.cStringDiameterInside);
        lblTubeYOut.Caption := fStringLoader.GetResString(TRackEditorStringLoader.cStringDiameterOutside);
    end
    else
    begin
        edTubeX.Visible := true;
        lblTubeX.Visible := true;
        lblTubeX.Caption := fStringLoader.GetResString(TRackEditorStringLoader.cStringLength);
        lblTubeY.Caption := fStringLoader.GetResString(TRackEditorStringLoader.cStringWidthInside);
        lblTubeYOut.Caption := fStringLoader.GetResString(TRackEditorStringLoader.cStringWidthOutside);
    end;
    self.TubeGeoGroup.ItemIndex := aMode;
end;

procedure TfrmEdRack.TubeGeoGroupClick(Sender: TObject);

begin
    SetTubeGeometryMode(TubeGeoGroup.ItemIndex);
end;

procedure TfrmEdRack.btnCancelClick(Sender: TObject);
begin
    Close;
end;

function TfrmEdRack.SaveChanges: boolean;
var
    xTypeName: string;
    xRec: TRackRec;
    xDA: TRackDataAdaptor;
    xDADiscretePos: TDiscreteRackDataAdaptor;
begin
    result := false;
    xTypeName := self.edTypeName.Text;
    if (xTypeName = '') then
        EXIT;

    self.RackRecFromGUI(xRec);

    // Nicht tiefer in Röhrchen fahren als es überhaupt lang ist
    if (xRec.TubeZ_mm < xRec.ZMax_mm) and
        (fStringLoader.ResMsgBox(51810 { The tube height Z is smaller than ZMax..Save anyway? } ,
        MB_ICONQUESTION + MB_YESNO) = IDNo) then
        EXIT;

    xDA := TRackDataAdaptor.Create;
    try
        if (fTypeName = '') then
        begin

            if xDA.NameExists(xTypeName) then
            begin // this is needed in layouter when InstanceShowToAdd
                raise Exception.Create(TLanguageString.
                    Read('{0} already exists. Action can not be continued!',
                    '{0} existiert bereits. Die Aktion kann nicht fortgesetzt werden!', [xTypeName]));
            end;

            if not TAppSettings.ItemConfirmAdd('Rack', xTypeName, '') then
                EXIT;

            fTypeName := xTypeName;
            self.edTypeName.ReadOnly := true;
        end
        else
        begin
            if not TAppSettings.ItemConfirmEdit('Rack', xTypeName) then
                EXIT;
        end;

        xDA.WriteRack(xRec);
    finally
        FreeAndNil(xDA);
    end;

    // Discrete Positions
    xDADiscretePos := TDiscreteRackDataAdaptor.Create;
    try
        xDADiscretePos.WritePositions(xRec.name, xRec.DiscretePosRecArray);
    finally
        FreeAndNil(xDADiscretePos);
    end;

    fCurrentName := xTypeName;
    result := true;
end;

procedure TfrmEdRack.btnOKClick(Sender: TObject);
begin
    if (SaveChanges) then
        ModalResult := mrOK;
end;

procedure TfrmEdRack.btnConvertRackInDiscretePosClick(Sender: TObject);
// Button darf erst nutzbar sein, wenn cbDiscretePositions Unchecked ist
var
    xDiscretePosRecArray: TDiscretePosRecArray;
begin
    DisableFieldsFirstLastXY;
    xDiscretePosRecArray := GetDiscretePos;
    // Schreiben in TabSheet3 (Tabellensicht)
    DiscretePositionsToGrid(Length(xDiscretePosRecArray), xDiscretePosRecArray);
    // Werte aus TabSheet 'Basic' für 'Rows' und 'Cols' anpassen
    edCols.Text := IntToStr(Length(xDiscretePosRecArray));
    edRows.Text := '1';
    // cbDiscretePos Häckchen setzen und Schaltfläche ausgrauen
    cbDiscretePositions.Checked := true;
    btnConvertDiscretePos.Enabled := false;
    // Array freigeben
    xDiscretePosRecArray := nil;
end;

function TfrmEdRack.GetDiscretePos: TDiscretePosRecArray;
var
    xRack: TRack;
    x: Integer;
    xX, xY, xZ: double;
    xStr: string;
begin
    xRack := TRack.CreateRackTypeView(fRackRec);
    try
        for x := 0 to xRack.Wells.Count - 1 do
        begin
            xStr := '';
            // Werte aus xRack entnehmen
            xX := xRack.Wells.Items[x].PosX;
            xY := xRack.Wells.Items[x].PosY;
            xZ := xRack.Wells.Items[x].PosZ;
            // Format anpassen
            xStr := Format('%8.2f', [xX]);
            xX := StrToFloat(xStr);
            xStr := Format('%8.2f', [xY]);
            xY := StrToFloat(xStr);
            xStr := Format('%8.2f', [xZ]);
            xZ := StrToFloat(xStr);
            // Werte in Array speichern
            SetLength(result, x + 1);
            result[x].Position := x + 1;
            result[x].X_mm := xX;
            result[x].Y_mm := xY;
            result[x].Z_mm := xZ;
        end;
    finally
        FreeAndNil(xRack);
    end;
end;

procedure TfrmEdRack.cbDiscretePositionsClick(Sender: TObject);
var
    xRackRec: TRackRec;
begin
    if cbDiscretePositions.Checked then
    begin
        DisableFieldsFirstLastXY;
        btnConvertDiscretePos.Enabled := false;
        self.PageControl1.Pages[4].TabVisible := true;
        self.RackRecFromGUI(xRackRec);
        self.DiscretePositionsToGrid(xRackRec.Cols, xRackRec.DiscretePosRecArray);
    end
    else
    begin
        EnableFieldsFirstLastXY;
        self.PageControl1.Pages[4].TabVisible := false;
        btnConvertDiscretePos.Enabled := true;
    end;

end;

procedure TfrmEdRack.btnApplyClick(Sender: TObject);
begin
    if (SaveChanges) then
    begin
        DisplayRack;
        FApplied := true;
    end;
end;

procedure TfrmEdRack.btnColorClick(Sender: TObject);
begin
    if (ColorDialog1.Execute) then
        Shape1.Brush.Color := ColorDialog1.Color;
end;

procedure TfrmEdRack.DisableFieldsFirstLastXY;
begin
    EdPosXFirst.Enabled := false;
    EdPosYFirst.Enabled := false;
    EdPosXLast.Enabled := false;
    EdPosYLast.Enabled := false;
end;

procedure TfrmEdRack.DiscretePositionsFromGrid(aNumberOfPos: integer;
    var aDiscretePosRecArray: TDiscretePosRecArray);
var
    x: integer;
    xRecIndex: integer;
begin
    xRecIndex := cxGrid1TableView1.DataController.RecordCount;
    if xRecIndex <= 0 then
        EXIT;
    SetLength(aDiscretePosRecArray, xRecIndex);
    for x := 0 to xRecIndex - 1 do
    begin
        aDiscretePosRecArray[x].Position := cxGrid1TableView1.DataController.Values[x, 0];
        aDiscretePosRecArray[x].X_mm := cxGrid1TableView1.DataController.Values[x, 1];
        aDiscretePosRecArray[x].Y_mm := cxGrid1TableView1.DataController.Values[x, 2];
    end;

    if high(aDiscretePosRecArray) < aNumberOfPos - 1 then
    begin
        SetLength(aDiscretePosRecArray, aNumberOfPos);
        for x := xRecIndex to aNumberOfPos - 1 do
        begin
            aDiscretePosRecArray[x] := TDiscreteRackDataAdaptor.MakeDefaultRackRec(x + 1);
            xRecIndex := cxGrid1TableView1.DataController.AppendRecord;
            cxGrid1TableView1.DataController.Values[xRecIndex, 0] := aDiscretePosRecArray[x].Position;
            cxGrid1TableView1.DataController.Values[xRecIndex, 1] := aDiscretePosRecArray[x].X_mm;
            cxGrid1TableView1.DataController.Values[xRecIndex, 2] := aDiscretePosRecArray[x].Y_mm;
        end;
    end;
end;

procedure TfrmEdRack.DiscretePositionsToGrid(aNumberOfPos: integer;
    aDiscretePosRecArray: TDiscretePosRecArray);
var
    x: integer;
    xRecIndex: integer;
begin
    for x := 0 to high(aDiscretePosRecArray) do
    begin
        if cxGrid1TableView1.DataController.RecordCount <= x then
            xRecIndex := cxGrid1TableView1.DataController.AppendRecord
        else
            xRecIndex := x;
        cxGrid1TableView1.DataController.Values[xRecIndex, 0] := aDiscretePosRecArray[x].Position;
        cxGrid1TableView1.DataController.Values[xRecIndex, 1] := aDiscretePosRecArray[x].X_mm;
        cxGrid1TableView1.DataController.Values[xRecIndex, 2] := aDiscretePosRecArray[x].Y_mm;
    end;
    if high(aDiscretePosRecArray) < aNumberOfPos - 1 then
    begin
        for x := high(aDiscretePosRecArray) + 2 to aNumberOfPos do
        begin
            xRecIndex := cxGrid1TableView1.DataController.AppendRecord;
            cxGrid1TableView1.DataController.Values[xRecIndex, 0] := xRecIndex + 1;
            cxGrid1TableView1.DataController.Values[xRecIndex, 1] := 0;
            cxGrid1TableView1.DataController.Values[xRecIndex, 2] := 0;
        end;
    end;
end;

procedure TfrmEdRack.DisplayRack;
var
    xRec: TRackRec;
begin
    // Rack löschen
    FreeAndNil(FRack);
    // Rack neu erzeugen
    self.RackRecFromGUI(xRec);
    fRack := TRack.CreateRackTypeView(xRec);
    fRack.AssignGraphicsParent(fSceneGraphics);
    fRack.Visible := true;
    fSceneGraphics.SceneChanged();
end;

procedure TfrmEdRack.EnableFieldsFirstLastXY;
begin
    EdPosXFirst.Enabled := true;
    EdPosYFirst.Enabled := true;
    EdPosXLast.Enabled := true;
    EdPosYLast.Enabled := true;
end;

class function TfrmEdRack.InstanceShowToAdd(out oName: string): boolean;
var
    xInstance: TfrmEdRack;
begin
    oName := '';
    result := false;
    xInstance := TfrmEdRack.Create(nil);
    try
        xInstance.CurrentName := '';
        if (xInstance.ShowModal = mrOK) then
        begin
            result := true;
            oName := xInstance.CurrentName;
        end;
    finally
        xInstance.Free;
    end;
end;

class function TfrmEdRack.InstanceShowToEdit(const aName: string): boolean;
var
    xInstance: TfrmEdRack;
begin
    result := false;
    xInstance := TfrmEdRack.Create(nil);
    try
        xInstance.CurrentName := aName;
        if (xInstance.ShowModal = mrOK) then
        begin
            result := true;
        end;
    finally
        xInstance.Free;
    end;
end;


end.
