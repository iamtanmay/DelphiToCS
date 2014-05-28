{ --------------------------------------------------------------------------------------------------
  SETUP
  --------------------------------------------------------------------------------------------------
  Carrier Definition Form
  --------------------------------------------------------------------------------------------------
  Datum    op  function/procedure     Änderung / Neuerung
  -------- --  -------------------    --------------------------------------------------------------
  17.06.98 wl  dbEdSlotsExit          Nachfrage geändert
  dbEdColsCarrierExit    neu: Aufbau wie dbEdSlotsExit
  20.07.98 wl                         Type sichtbar
  variablen ins Objekt
  25.08.98 wl  PaintCarrier           Carrier laden mit Zoom = 0,8, danach Zoom wieder normal
  02.09.98 wl  CopyCarrierClick       um 'H_ZPut_mm' erweitert
  17.06.98 wl  dbEdSlotsExit          Exceptions werden abgefangen
  dbEdColsCarrierExit    dito
  13.11.98 wl  CopyCarrierClick       EditForm ersetzt durch InputQuery
  mehr Platz für den Carrier
  19.11.98 wl                         erscheint nicht mehr maximiert
  08.01.99 wl                         DBCARRIER und DataSource aus LDBModul
  DBCARRIERBeforePost    Carrier ohne Namen werden nicht gespeichert
  09.03.99 wl  CopyCarrierClick       Funktion stark vereinfacht -> alle Felder werden kopiert
  16.03.99 wl  CopyCarrierClick       Name wird auf gMaxNameLen.CarrierType gekürzt
  18.08.99 wz                         --> Strings in Ressourcen
  24.08.99 wl                         Edit-Feld für 'SlotZ_mm' entfernt
  14.02.00 wl                         Einfache Oberfläche mit OK/Cancel
  14.02.00 wl                         Einfache Oberfläche mit OK/Cancel
  22.02.00 wl                         Delete-, New-, Rename- und Copy-Funktion entfernt
  Löschen, Kopieren, Einfügen --> LayMain
  22.03.00 wl  FormShow               Caption des Fensters angepaßt
  06.02.03 wl  btnOKClick             TN1334.3 Every 'Post' will be logged (with reason)
  06.02.03 wl  FormShow               TN1334.3 if user is not Admin, 'OK' is not enabled
  12.02.03 wl  FormShow               TN1334.3 if user is not Admin, DataSource is ReadOnly
  19.02.03 wl  FormShow               TN1345 bei Sias-Geräten wird Rotation & Handler Offsets mit angezeigt
  12.03.03 wl                         TN1293.5 uses posTools
  12.09.03 wl  dbEditSlotsExit        TN1581.8 Beschränkung aufgehoben: "Stacker mit mehreren Columns nicht möglich"
  12.09.03 wl                         TN1581.4 neu: Button Orientation für Stacker
  12.09.03 wl                         TN1581.7 alle Rotation-Editfelder unsichtbar
  23.10.03 wl                         TN1631  Neues Feld für 'H_XPreStart_mm'
  13.11.03 wl  btnApplyClick          TN1581.19 neu: Darstellung des Carriers mit "Übernehmen"-Button zur sofortigen Ansicht
  13.11.03 wl                         TN1664    Rotation-Felder für ZP02: SlotRotation_degree und H_DirectionTurnType
  20.11.03 wl  btnApplyClick          TN1581.19 Buttons Windows-konform
  15.12.03 wl                         TN1672    neu: Carrier-Type (Barrier, Corridor)
  16.01.04 wl                         TN1672    Carrier-Type korrigiert
  29.01.04 wl  FormShow               TN1703    Slots wird bei neuen Carriern auf 1 gesetzt
  17.02.04 pk  Button2Click           TN1749    New parameter for SetCarrierTypeName
  02.09.04 pk                         TN2119    Invisible Radio Button fixed
  27.10.04 wl                         TN2071   Rack direction: von Rack transport zu "Other" verschoben (hat nicht nur auf Transport Einfluß)
  11.03.05 wl                         TN2342   neue Werte für Tube-Bewegungen
  24.08.06 wl                         TN3269    neues Feld: SlotZ_Calculate
  24.08.06 wl                         TN3269.1  Label für SlotZ_Last_mm und SlotZ_First_mm vertauscht
  31.01.07 wl                         TN3532    mit den neuen Feldern H_RSTART_DEGREE, H_RRETAKE_DEGREE und HTUBE_RSTART_DEGREE
  31.01.07 wl                         TN3532    benutzt Main.CARRIER statt DBRack.CARRIER
  04.05.07 wl  SaveChanges            TN3669    benutzt TAppSettings.ItemConfirmEdit/Add
  14.06.07 wl                         TN3728    neues Feld HTUBE_KEEPROTATION
  20.06.08 pk                         TN3922    Dataset changed to DataProvider
  20.06.08 pk                         TN4139    Graphics now drawn to fSceneGraphics
  27.06.08 pk                         TN4139    DB controls removed. Now based on TCarrierRec
  11.07.08 wl                         TN4164    Struktur geändert, damit auch im Designer verwendbar
  09.02.09 wl  TCarrierEditorStringLoader  TN4370  neu: dezentrale Resourcen
  11.03.09 pk                         TN4457    Stacker buttons Bottom changed to Front, Top changed To Back
  11.03.09 pk  DisplayCarrier         TN4457    Call CreateCarrierTypeView
  05.11.09 pk  SaveChanges            TN4834    raise exception if type already exists
  30.04.10 wl                         TN5070    MakeDefaultCarrierRec -> TCarrierDataAdaptor
  20.05.10 wl                         TN5117   neue Schriftart "Segoe UI", Schriftgröße 9
  21.06.10 wl                               TN5160   Position = poScreenCenter
  23.07.10 wl  TCarrierRec                  TN5205   Size-Werte heißen jetzt auch SizeX,SizeY,SizeZ
  10.02.11 wl                               TN5474   Neues Format
  21.07.11 wl  FormCreate                   TN5614   User-Verwaltung greift jetzt
  16.04.13 pp  GetDiscretePosition/..ToGrid TN6131   new: DiscretePositions
  -------------------------------------------------------------------------------------------------- }

unit EditCarr;


interface


uses
    Windows,
    Forms,
    StdCtrls,
    Controls,
    Classes,
    ComCtrls,
    ExtCtrls,
    CarrierDataAdaptor,
    Carrier,
    CommonTypes,
    StringLoader,
    SceneGraphics,
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
    cxGridLevel,
    cxGridCustomTableView,
    cxGridTableView,
    cxClasses,
    cxGridCustomView,
    cxGrid,
    DiscreteCarrierDataAdaptor,
    MatrixMath,
    CarrierSlot,
    Generics.Collections;

type
    TCarrierEditorStringLoader = class(TTextListStringLoader)
    protected
        procedure AddAllItems(); override;
    end;

    TfrmEdCarr = class(TForm)
        PageControl1: TPageControl;
        TabSheet1: TTabSheet;
        TabSheet2: TTabSheet;
        TabSheet3: TTabSheet;
        GroupBox5: TGroupBox;
        Label27: TLabel;
        edTypeName: TEdit;
        GroupBox4: TGroupBox;
        Label24: TLabel;
        Label25: TLabel;
        Label26: TLabel;
        edSizeX: TEdit;
        edSizeY: TEdit;
        edSizeZ: TEdit;
        GroupBox2: TGroupBox;
        Label13: TLabel;
        Label15: TLabel;
        edRows: TEdit;
        edCols: TEdit;
        GroupBox1: TGroupBox;
        rdoStackerButtonAlign: TRadioGroup;
        SlotMeasures: TGroupBox;
        Label29: TLabel;
        Label30: TLabel;
        edSlotXmm: TEdit;
        edSlotYmm: TEdit;
        SlotPos: TGroupBox;
        Label18: TLabel;
        Label19: TLabel;
        Label20: TLabel;
        Label21: TLabel;
        edSlotXFirst: TEdit;
        edSlotYFirst: TEdit;
        edSlotXLast: TEdit;
        edSlotYLast: TEdit;
        gbHandler: TGroupBox;
        Label40: TLabel;
        Label32: TLabel;
        edHXRetake: TEdit;
        edHYRetake: TEdit;
        edHZRetake: TEdit;
        Panel2x: TPanel;
        Panel3: TPanel;
        btnApply: TButton;
        btnOK: TButton;
        btnCancel: TButton;
        edFloors: TEdit;
        Label33: TLabel;
        GroupBox3: TGroupBox;
        lblFirstZPos: TLabel;
        edSlotZFirst: TEdit;
        lblLastZPos: TLabel;
        edSlotZLast: TEdit;
        GroupBox6: TGroupBox;
        Label2: TLabel;
        edHXPreStart: TEdit;
        GroupBox7: TGroupBox;
        Label38: TLabel;
        Label39: TLabel;
        edHXStart: TEdit;
        edHYStart: TEdit;
        edHZStart: TEdit;
        GroupBox8: TGroupBox;
        Label1: TLabel;
        edHZPut: TEdit;
        Label37: TLabel;
        TabSheet5: TTabSheet;
        rdoCarrierType: TRadioGroup;
        rdoSlotRotation: TRadioGroup;
        rdoDirectionTurnType: TRadioGroup;
        chkHTubeUseValues: TCheckBox;
        GroupBox9: TGroupBox;
        Label4: TLabel;
        Label5: TLabel;
        edHTubeXStart: TEdit;
        edHTubeYStart: TEdit;
        edHTubeZStart: TEdit;
        Label6: TLabel;
        chkHTubeYStartBeforeX: TCheckBox;
        chkCalcStackHeights: TCheckBox;
        edHRStart: TEdit;
        Label7: TLabel;
        edHRRetake: TEdit;
        edHTubeRStart: TEdit;
        Label8: TLabel;
        Label10: TLabel;
        Label3: TLabel;
        Label9: TLabel;
        chkHTubeKeepRotation: TCheckBox;
        TabSheet4: TTabSheet;
        GroupBox10: TGroupBox;
        cbDiscretePositions: TCheckBox;
        btnConvertCarrierDisPos: TButton;
        TabSheet6: TTabSheet;
        cxGrid1: TcxGrid;
        cxGrid1TableView1: TcxGridTableView;
        cxGrid1TableView1Column1: TcxGridColumn;
        cxGrid1TableView1Column2: TcxGridColumn;
        cxGrid1TableView1Column3: TcxGridColumn;
        cxGrid1Level1: TcxGridLevel;
        cxGrid1TableView1Column4: TcxGridColumn;
        procedure FormClose(Sender: TObject; var Action: TCloseAction);
        procedure FormCreate(Sender: TObject);
        procedure btnOKClick(Sender: TObject);
        procedure btnCancelClick(Sender: TObject);
        procedure btnApplyClick(Sender: TObject);
        procedure chkCalcStackHeightsClick(Sender: TObject);
        procedure FormDestroy(Sender: TObject);
        procedure btnConvertCarrierDisPosClick(Sender: TObject);
        procedure DiscretePositionsToGrid(aNumberOfPos: integer; aDiscretePosRecArray: TDiscretePosRecArray);
        procedure DiscretePositionsFromGrid(aNumberOfPos: integer;
            aDiscretePosRecArray: TDiscretePosRecArray);
        procedure cbDiscretePositionsClick(Sender: TObject);
    private
        fApplied: boolean;
        fCarrier: TCarrier;
        fSceneGraphics: TSceneGraphics;
        fTypeName: string;
        fCurrentName: string;
        fStringLoader: TCarrierEditorStringLoader;
        fCarrierRec: TCarrierRec;
        procedure CarrierRecFromGUI(out oCarrierRec: TCarrierRec);
        procedure CarrierRecToGUI(const aCarrierRec: TCarrierRec; aNameReadOnly: boolean);
        function SlotRotationFromGUI(): integer;
        procedure SlotRotationToGUI(aSlotRotation: double);
        function SaveChanges: boolean;
        procedure DisplayCarrier;
        function GetDiscretePos: TDiscretePosRecArray;
        procedure EnableFieldsFirstLastXY;
        procedure DisableFieldsFirstLastXY;
    public
        property CurrentName: string read fCurrentName;

        class function InstanceShowToAddFlatCarrier(var vName: string; aX, aY: TPosMM): boolean;
        class function InstanceShowToAdd(out oName: string): boolean;
        class function InstanceShowToEdit(const aName: string): boolean;
        class function InstanceShow(const aCarrierRec: TCarrierRec; aNameReadOnly: boolean;
            out oName: string): boolean;
    end;


implementation


uses
    SysUtils,
    AppSettings,
    SamGlobe,
    GeneralTypes,
    ControlUtils;

{$R *.DFM}
{ TCarrierEditorStringLoader }

procedure TCarrierEditorStringLoader.AddAllItems;
begin
    AddStandardButtonStrings();

    AddDouble(50000, 'Save current carrier?', 'Save Carrier', 'Soll der aktuelle Carrier gespeichert werden?',
        'Carrier speichern');
    AddSingle(50001, 'Basic', 'Abmessungen');
    AddDouble(50010, 'Insert new carrier type name!', 'New Carrier Type',
        'Bitte einen neuen Carriertyp-Namen eingeben!', 'Neuer Carriertyp');
    AddSingle(50020, 'Carriertype - Name already exists, insert another name!',
        'Carriertyp: Name existiert bereits. Neuen Namen eingeben!');
    AddSingle(50050, 'Insert Carrier Name!', 'Carrier-Namen eingeben!');
    AddSingle(50070, '&Copy', '&Kopieren');
    AddSingle(50130, 'Carrier Type', 'Carriertyp');
    AddSingle(50140, 'Name', 'Name');
    AddSingle(50150, 'Type', 'Typ');
    AddSingle(50160, 'Carrier Dimensions [mm] ', 'Carrier Dimensionen [mm] ');
    AddSingle(50170, 'Length (X)', 'Länge (X)');
    AddSingle(50180, 'Width (Y)', 'Breite (Y)');
    AddSingle(50190, 'Height (Z)', 'Höhe (Z)');
    AddSingle(50200, 'Positions ', 'Positionen ');
    AddSingle(50210, 'Rows', 'Reihen');
    AddSingle(50220, 'Columns', 'Spalten');
    AddSingle(50229, 'Stacker', 'Stapel');
    AddSingle(50230, 'Number of stacker levels', 'Anzahl der Stapel-Ebenen');
    AddSingle(50240, 'Pre-Start-Position [mm / rel. to start pos.]',
        'Vor-Start-Position [mm / rel. zu Startpos.]');
    AddSingle(50250, 'Start Position [mm / rel. to carrier]', 'Start-Position [mm / rel. zu Carrier]');
    AddSingle(50260, 'Retake [mm / rel. to carrier]', 'Nachgreifen [mm / rel. zu Carrier]');
    AddSingle(50270, 'Put-Plate-Offset [mm / rel. to carrier]', 'Rack abstellen [mm / rel. zu Carrier]');
    AddDouble(50280, 'Default rack direction on slot', '0°;90°;180°;270°', 'Normale Ausrichtung des Racks',
        '0°;90°;180°;270°');
    AddDouble(50290, 'Other possible rack directions', 'No;Rack can be turned 180°;Rack can be turned 90°',
        'Andere mögliche Ausrichtungen des Racks', 'Nein;Drehung um 180° möglich;Drehung um 90° möglich');
    AddSingle(50300, 'Stacker levels', 'Stapel-Ebenen');
    AddSingle(50310, 'Z - Put Rack', 'Z - Rack abstellen');
    AddSingle(50320, 'Slots [mm / rel. to carrier]', 'Slots [mm / Carrier]');
    AddSingle(50330, 'First Position (X)', 'Erste Position (X)');
    AddSingle(50340, 'First Position (Y)', 'Erste Position (Y)');
    AddSingle(50350, 'First Position (Z)', 'Erste Position (Z)');
    AddSingle(50360, 'Last Position (X)', 'Letzte Position (X)');
    AddSingle(50370, 'Last Position (Y)', 'Letzte Position (Y)');
    AddSingle(50380, 'Last Position (Z)', 'Letzte Position (Z)');
    AddSingle(50390, 'New Carrier Type', 'Neuer Carriertyp');
    AddSingle(50400, 'Carrier Type: %s', 'Carriertyp: %s');
    AddDouble(50570, 'Level Buttons (if levels >1)', 'Front;Back;Left;Right;Invisible',
        'Level Buttons (Bei >1 Ebene)', 'Vorne;Hinten;Links;Rechts;Unsichtbar');
    AddSingle(50590, 'X - Pre-Start', 'X - vor Start');
    AddSingle(50600, 'Other', 'Anderes');
    AddDouble(50610, 'Special carriers', 'Default;Barrier for XY-movement', 'Spezial-Carrier',
        'Standard;Hindernis für XY-Bewegungen');
    AddSingle(50620, 'Movement', 'Bewegungen');
    AddSingle(50629, 'Keep rotation value for XY movement', 'Rotation bei XY-Beweg. beibehalten');
    AddSingle(50630, 'Plate movement (and tube movement):', 'Plattenbewegungen (und Tube-Bewegungen):');
    AddSingle(50640, 'Use different values for tube movement:', 'Für Tube-Bewegung andere Werte benutzen');
    AddSingle(50650, 'Move to Y-Start before X-Start', 'Zuerst Y-Start anfahren, dann X-Start');
    AddSingle(50660, 'Calculate height by rack stack heights', 'Höhe aus Stapelhöhe der Racks berechnen');

    // AddDouble( 50430, '&Test', 'Test pipetting positions', '&Test', 'Testen der Pipettierpositionen' );

    { 50440                   ""
      50441                   "&Bearbeiten"
      50445                   "Edit Layout"
      50446                   "Ändern der Layoutdaten"
      50450                   "&Racks"
      50451                   "&Racks"
      50455                   "Test rack movement"
      50456                   "Testen des Rackgreifens"
      50460                   "&Save"
      50461                   "&Speichern"
      50465                   "Save Layout"
      50466                   "Layout speichern"
      50470                   "&Z-Heights"
      50471                   "&Z-Höhen"
      50475                   "Define Workbench Heights"
      50476                   "Höhen auf der Arbeitsfläche definieren"
      50480                   "&Tips"
      50481                   "&Nadeln"
      50485                   "Edit tip-configuration"
      50486                   "Spitzen-Konfiguration bearbeiten"
      50490                   "Please type the shortcut of the Racks you want to read:"
      50491                   "Abkürzung der einzulesenden Racks eingeben:"
      50495                   "Read Barcodes"
      50496                   "Barcodes lesen"
      50500                   "Init Balance"
      50501                   "Waage initialisieren"
      50510                   "Open Door"
      50511                   "Tür öffnen"
      50520                   "Weigh"
      50521                   "Einwaage"
      50530                   "Test &Modules"
      50531                   "&Module testen"
      50540                   "Test &DLL Functions"
      50541                   "&DLL-Funktionen testen"
      50550                   "&Serial Port Test"
      50551                   "&Serielle Schnittstelle testen"
      50560                   "&Read Barcodes"
      50561                   "&Barcodes lesen"

      50800       "Please enter a reason for your changes:"
      50805       "Save changes"

      50870                   "Edit"
      50871                   "Bearbeiten"
      50875                   "Edit %s"
      50876                   "Bearbeite %s"
    }

end;

{ TfrmEdCarr }

procedure TfrmEdCarr.FormCreate(Sender: TObject);
begin
    TControlUtils.ResetFontForWinXP(self);
    fStringLoader := TCarrierEditorStringLoader.Create;
    fStringLoader.LoadLanguage(self);

    fTypeName := '';
    fSceneGraphics := TSceneGraphics.Create(Panel2x);
    fSceneGraphics.Visible := true;

    btnOK.Visible := gCommonDll.LevelIsIncluded(gCommonDll.CurrentUser.Level, usrSystemAdmin);
    btnApply.Visible := gCommonDll.LevelIsIncluded(gCommonDll.CurrentUser.Level, usrSystemAdmin);
end;

procedure TfrmEdCarr.FormDestroy(Sender: TObject);
begin
    fSceneGraphics.Free;
    fStringLoader.Free;
end;

procedure TfrmEdCarr.FormClose(Sender: TObject; var Action: TCloseAction);
begin
    FreeAndNil(FCarrier);
    if (FApplied) then
        ModalResult := mrOK;
end;

procedure TfrmEdCarr.SlotRotationToGUI(aSlotRotation: double);
begin
    if aSlotRotation = 90 then
        rdoSlotRotation.ItemIndex := 1
    else if aSlotRotation = 180 then
        rdoSlotRotation.ItemIndex := 2
    else if aSlotRotation = 270 then
        rdoSlotRotation.ItemIndex := 3
    else
        rdoSlotRotation.ItemIndex := 0;
end;

function TfrmEdCarr.SlotRotationFromGUI(): integer;
begin
    case self.rdoSlotRotation.ItemIndex of
        1:
            result := 90;
        2:
            result := 180;
        3:
            result := 270
        else
            result := 0;
    end;
end;

procedure TfrmEdCarr.CarrierRecFromGUI(out oCarrierRec: TCarrierRec);
var
    x, xNumberOfSlots: integer;
begin
    xNumberOfSlots := 0;
    with oCarrierRec do
    begin
        oCarrierRec.Name := edTypeName.Text;
        oCarrierRec.CarrierType := rdoCarrierType.ItemIndex;
        oCarrierRec.SizeX_mm := StrToFloat(edSizeX.Text);
        oCarrierRec.SizeY_mm := StrToFloat(edSizeY.Text);
        oCarrierRec.SizeZ_mm := StrToFloat(edSizeZ.Text);
        oCarrierRec.Rows := StrToInt(edRows.Text);
        oCarrierRec.Cols := StrToInt(edCols.Text);
        oCarrierRec.Floors := StrToInt(edFloors.Text);
        oCarrierRec.SlotX_mm := StrToFloat(edSlotXmm.Text);
        oCarrierRec.SlotY_mm := StrToFloat(edSlotYmm.Text);

        oCarrierRec.SlotZ_ReallyFirst_mm := StrToFloat(edSlotZFirst.Text);

        oCarrierRec.SlotZ_ReallyLast_mm := StrToFloat(edSlotZLast.Text);
        oCarrierRec.SlotZ_Calculate := chkCalcStackHeights.Checked;

        oCarrierRec.SlotRotation_Degree := SlotRotationFromGUI();
        oCarrierRec.Stackerbuttons := rdoStackerButtonAlign.ItemIndex;

        oCarrierRec.H_XStart_mm := StrToFloat(edHXStart.Text);
        oCarrierRec.H_YStart_mm := StrToFloat(edHYStart.Text);
        oCarrierRec.H_ZStart_mm := StrToFloat(edHZStart.Text);
        oCarrierRec.H_RStart_degree := StrToFloat(edHRStart.Text);
        oCarrierRec.H_XRetake_mm := StrToFloat(edHXRetake.Text);
        oCarrierRec.H_YRetake_mm := StrToFloat(edHYRetake.Text);
        oCarrierRec.H_ZRetake_mm := StrToFloat(edHZRetake.Text);
        oCarrierRec.H_RRetake_degree := StrToFloat(edHRRetake.Text);
        oCarrierRec.H_ZPut_mm := StrToFloat(edHZPut.Text);
        oCarrierRec.H_XPreStart_mm := StrToFloat(edHXPreStart.Text);
        oCarrierRec.H_DirectionTurnType := rdoDirectionTurnType.ItemIndex;

        oCarrierRec.HTube_UseValues := chkHTubeUseValues.Checked;
        oCarrierRec.HTube_XStart_mm := StrToFloat(edHTubeXStart.Text);
        oCarrierRec.HTube_YStart_mm := StrToFloat(edHTubeYStart.Text);
        oCarrierRec.HTube_ZStart_mm := StrToFloat(edHTubeZStart.Text);
        oCarrierRec.HTube_RStart_degree := StrToFloat(edHTubeRStart.Text);
        oCarrierRec.HTube_YStartBeforeX := chkHTubeYStartBeforeX.Checked;
        oCarrierRec.HTube_KeepRotation := chkHTubeKeepRotation.Checked;
        oCarrierRec.DiscretePositions := cbDiscretePositions.Checked;
    end;

    if cbDiscretePositions.Checked then
    begin
        btnConvertCarrierDisPos.Enabled := false;
        // Sind bereits Einträge in der Tabelle vorhanden?
        // Falls ja, dann lade sie ins DiscetePosRecArray
        // Falls nein, übergebe leeres Array
        if cxGrid1TableView1.DataController.RecordCount > 0 then
        begin
            xNumberOfSlots := StrToInt(edRows.Text) * StrToInt(edCols.Text);
            // setze Länge DiscretePosRecArray gleich Anzahl existierender Slots
            SetLength(oCarrierRec.DiscretePosRecArray, xNumberOfSlots);
            // als nächstes müssen die einzelnen Werte aus den Records dem DiscretePosRecArray hinzugefügt werden
            // Weil x bei 0 anfängt zu zählen
            // xNumberOfSlots := xNumberOfSlots - 1;
            for x := 0 to xNumberOfSlots - 1 do
            begin
                // ...GetDisplayText(Record, Spalte);
                oCarrierRec.DiscretePosRecArray[x].Position :=
                    StrToInt(cxGrid1TableView1.DataController.GetDisplayText(x, 0));
                oCarrierRec.DiscretePosRecArray[x].X_mm :=
                    StrToFloat(cxGrid1TableView1.DataController.GetDisplayText(x, 1));
                oCarrierRec.DiscretePosRecArray[x].Y_mm :=
                    StrToFloat(cxGrid1TableView1.DataController.GetDisplayText(x, 2));
                oCarrierRec.DiscretePosRecArray[x].Z_mm :=
                    StrToFloat(cxGrid1TableView1.DataController.GetDisplayText(x, 3));
            end;
        end;
        self.PageControl1.Pages[5].TabVisible := true;
        self.DiscretePositionsFromGrid(xNumberOfSlots, oCarrierRec.DiscretePosRecArray);
    end
    else
    begin
        self.PageControl1.Pages[5].TabVisible := false;
        btnConvertCarrierDisPos.Enabled := true;
        oCarrierRec.SlotX_First_mm := StrToFloat(edSlotXFirst.Text);
        oCarrierRec.SlotY_First_mm := StrToFloat(edSlotYFirst.Text);
        oCarrierRec.SlotX_Last_mm := StrToFloat(edSlotXLast.Text);
        oCarrierRec.SlotY_Last_mm := StrToFloat(edSlotYLast.Text);
        fCarrierRec := oCarrierRec;
    end;
end;

procedure TfrmEdCarr.CarrierRecToGUI(const aCarrierRec: TCarrierRec; aNameReadOnly: boolean);
begin
    if aNameReadOnly then
    begin
        fTypeName := aCarrierRec.Name;
        fCurrentName := aCarrierRec.Name;
        self.Caption := fStringLoader.GetResFString(50400 { Carrier Type: %s } , [aCarrierRec.Name]);
        edTypeName.ReadOnly := true;
    end
    else
    begin
        fCurrentName := '';
        self.Caption := fStringLoader.GetResString(50390 { New Carrier Type } );
        edTypeName.ReadOnly := false;
    end;

    with aCarrierRec do
    begin
        edTypeName.Text := aCarrierRec.Name;
        rdoCarrierType.ItemIndex := aCarrierRec.CarrierType;
        edSizeX.Text := FloatToStr(aCarrierRec.SizeX_mm);
        edSizeY.Text := FloatToStr(aCarrierRec.SizeY_mm);
        edSizeZ.Text := FloatToStr(aCarrierRec.SizeZ_mm);
        edRows.Text := IntToStr(aCarrierRec.Rows);
        edCols.Text := IntToStr(aCarrierRec.Cols);
        edFloors.Text := IntToStr(aCarrierRec.Floors);
        edSlotXmm.Text := FloatToStr(aCarrierRec.SlotX_mm);
        edSlotYmm.Text := FloatToStr(aCarrierRec.SlotY_mm);

        // edSlotXFirst.Text := FloatToStr(aCarrierRec.SlotX_First_mm);    //
        // edSlotYFirst.Text := FloatToStr(aCarrierRec.SlotY_First_mm);    //
        edSlotZFirst.Text := FloatToStr(aCarrierRec.SlotZ_ReallyFirst_mm);
        // edSlotXLast.Text := FloatToStr(aCarrierRec.SlotX_Last_mm);      //
        // edSlotYLast.Text := FloatToStr(aCarrierRec.SlotY_Last_mm);      //
        edSlotZLast.Text := FloatToStr(aCarrierRec.SlotZ_ReallyLast_mm);
        chkCalcStackHeights.Checked := aCarrierRec.SlotZ_Calculate;
        SlotRotationToGUI(aCarrierRec.SlotRotation_Degree);
        rdoStackerButtonAlign.ItemIndex := aCarrierRec.Stackerbuttons;

        edHXStart.Text := FloatToStr(aCarrierRec.H_XStart_mm);
        edHYStart.Text := FloatToStr(aCarrierRec.H_YStart_mm);
        edHZStart.Text := FloatToStr(aCarrierRec.H_ZStart_mm);
        edHRStart.Text := FloatToStr(aCarrierRec.H_RStart_degree);
        edHXRetake.Text := FloatToStr(aCarrierRec.H_XRetake_mm);
        edHYRetake.Text := FloatToStr(aCarrierRec.H_YRetake_mm);
        edHZRetake.Text := FloatToStr(aCarrierRec.H_ZRetake_mm);
        edHRRetake.Text := FloatToStr(aCarrierRec.H_RRetake_degree);
        edHZPut.Text := FloatToStr(aCarrierRec.H_ZPut_mm);
        edHXPreStart.Text := FloatToStr(aCarrierRec.H_XPreStart_mm);
        rdoDirectionTurnType.ItemIndex := aCarrierRec.H_DirectionTurnType;

        chkHTubeUseValues.Checked := aCarrierRec.HTube_UseValues;
        edHTubeXStart.Text := FloatToStr(aCarrierRec.HTube_XStart_mm);
        edHTubeYStart.Text := FloatToStr(aCarrierRec.HTube_YStart_mm);
        edHTubeZStart.Text := FloatToStr(aCarrierRec.HTube_ZStart_mm);
        edHTubeRStart.Text := FloatToStr(aCarrierRec.HTube_RStart_degree);
        chkHTubeYStartBeforeX.Checked := aCarrierRec.HTube_YStartBeforeX;
        chkHTubeKeepRotation.Checked := aCarrierRec.HTube_KeepRotation;
        cbDiscretePositions.Checked := aCarrierRec.DiscretePositions;
        fCarrierRec := aCarrierRec;
    end;

    if cbDiscretePositions.Checked then
    begin
        btnConvertCarrierDisPos.Enabled := false;
        self.PageControl1.Pages[5].TabVisible := true;
        self.DiscretePositionsToGrid(aCarrierRec.Cols * aCarrierRec.Rows, aCarrierRec.DiscretePosRecArray);
        edSlotXFirst.Text := '';
        edSlotYFirst.Text := '';
        edSlotXLast.Text := '';
        edSlotYLast.Text := '';
    end
    else
    begin
        edSlotXFirst.Text := FloatToStr(aCarrierRec.SlotX_First_mm);
        edSlotYFirst.Text := FloatToStr(aCarrierRec.SlotY_First_mm);
        edSlotXLast.Text := FloatToStr(aCarrierRec.SlotX_Last_mm);
        edSlotYLast.Text := FloatToStr(aCarrierRec.SlotY_Last_mm);
        self.PageControl1.Pages[5].TabVisible := false;
        btnConvertCarrierDisPos.Enabled := true;
        fCarrierRec := aCarrierRec;
    end;

    self.DisplayCarrier();
end;

function TfrmEdCarr.SaveChanges: boolean;
var
    xTypeName: string;
    xRec: TCarrierRec;
    xDA: TCarrierDataAdaptor;
    xDADiscretePos: TDiscreteCarrierDataAdaptor;
begin
    result := false;
    xTypeName := self.edTypeName.Text;
    if (xTypeName = '') then
        EXIT;

    self.CarrierRecFromGUI(xRec);

    xDA := TCarrierDataAdaptor.Create;
    try
        if (fTypeName = '') then
        begin
            if xDA.NameExists(xTypeName) then
            begin // this is needed in layouter when InstanceShowToAdd / InstanceShowToAddFlatCarrier
                raise Exception.Create(TLanguageString.
                    Read('{0} already exists. Action can not be continued!',
                    '{0} existiert bereits. Die Aktion kann nicht fortgesetzt werden!', [xTypeName]));
            end;

            if not TAppSettings.ItemConfirmAdd('Carrier', xTypeName, '') then
                EXIT;
            fTypeName := xTypeName;
            self.edTypeName.ReadOnly := true;
        end
        else
        begin
            if not TAppSettings.ItemConfirmEdit('Carrier', xTypeName) then
                EXIT;
        end;

        xDA.WriteCarrier(xRec);
    finally
        FreeAndNil(xDA);
    end;

    // Discrete Positions
    xDADiscretePos := TDiscreteCarrierDataAdaptor.Create;
    try
        xDADiscretePos.WritePositions(xRec.name, xRec.DiscretePosRecArray);
    finally
        FreeAndNil(xDADiscretePos);
    end;

    fCurrentName := xTypeName;
    result := true;
end;

procedure TfrmEdCarr.btnOKClick(Sender: TObject);
begin
    if (SaveChanges) then
        ModalResult := mrOK;
end;

procedure TfrmEdCarr.btnConvertCarrierDisPosClick(Sender: TObject);
var
    xDiscretePosRecArray: TDiscretePosRecArray;
begin
    DisableFieldsFirstLastXY;
    // Anzahl Slots in DiscretePosRecArray
    xDiscretePosRecArray := GetDiscretePos;
    // Schreiben in TabSheet3 (Tabellensicht)
    DiscretePositionsToGrid(Length(xDiscretePosRecArray), xDiscretePosRecArray);
    // cbDiscretePos Häckchen setzen und Schaltfläche(Button) ausgrauen
    cbDiscretePositions.Checked := true;
    btnConvertCarrierDisPos.Enabled := false;
end;

function TfrmEdCarr.GetDiscretePos: TDiscretePosRecArray;
var
    xCarrier: TCarrier;
    x: Integer;
    xX, xY, xZ: double;
    xStr: string;
    xCarrierSlot: TObjectList<TCarrierSlot>;
begin
    xCarrier := TCarrier.CreateCarrierTypeView(fCarrierRec);
    xCarrierSlot := TCarrier.GetCarrierSlots(xCarrier);
    try
        for x := 0 to xCarrier.SlotCount - 1 do
        begin
            xStr := '';
            // Koordinaten von Slot Positionen abfragen
            xX := xCarrierSlot.Items[x].PosX;
            xY := xCarrierSlot.Items[x].PosY;
            xZ := xCarrierSlot.Items[x].PosZ;
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
        FreeAndNil(xCarrier);
    end;
end;

procedure TfrmEdCarr.DiscretePositionsToGrid(aNumberOfPos: integer;
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
        cxGrid1TableView1.DataController.Values[xRecIndex, 3] := aDiscretePosRecArray[x].Z_mm;
    end;
    if high(aDiscretePosRecArray) < aNumberOfPos - 1 then
    begin
        for x := high(aDiscretePosRecArray) + 2 to aNumberOfPos do
        begin
            xRecIndex := cxGrid1TableView1.DataController.AppendRecord;
            cxGrid1TableView1.DataController.Values[xRecIndex, 0] := xRecIndex + 1;
            cxGrid1TableView1.DataController.Values[xRecIndex, 1] := 0;
            cxGrid1TableView1.DataController.Values[xRecIndex, 2] := 0;
            cxGrid1TableView1.DataController.Values[xRecIndex, 3] := 0;
        end;
    end;
end;

procedure TfrmEdCarr.DisableFieldsFirstLastXY;
begin
    edSlotXFirst.Enabled := false;
    edSlotYFirst.Enabled := false;
    edSlotXLast.Enabled := false;
    edSlotYLast.Enabled := false;
end;

procedure TfrmEdCarr.DiscretePositionsFromGrid(aNumberOfPos: integer;
    aDiscretePosRecArray: TDiscretePosRecArray);
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
        aDiscretePosRecArray[x].Z_mm := cxGrid1TableView1.DataController.Values[x, 3];
    end;

    if high(aDiscretePosRecArray) < aNumberOfPos - 1 then
    begin
        SetLength(aDiscretePosRecArray, aNumberOfPos);
        for x := xRecIndex to aNumberOfPos - 1 do
        begin
            aDiscretePosRecArray[x] := TDiscreteCarrierDataAdaptor.MakeDefaultCarrierRec(x + 1);
            xRecIndex := cxGrid1TableView1.DataController.AppendRecord;
            cxGrid1TableView1.DataController.Values[xRecIndex, 0] := aDiscretePosRecArray[x].Position;
            cxGrid1TableView1.DataController.Values[xRecIndex, 1] := aDiscretePosRecArray[x].X_mm;
            cxGrid1TableView1.DataController.Values[xRecIndex, 2] := aDiscretePosRecArray[x].Y_mm;
            cxGrid1TableView1.DataController.Values[xRecIndex, 3] := aDiscretePosRecArray[x].Z_mm;
        end;
    end;
end;

procedure TfrmEdCarr.btnCancelClick(Sender: TObject);
begin
    ModalResult := mrCancel;
end;

procedure TfrmEdCarr.DisplayCarrier;
var
    xRec: TCarrierRec;
begin
    // Carrier löschen
    FreeAndNil(FCarrier);
    // Carrier neu erzeugen
    self.CarrierRecFromGUI(xRec);
    fCarrier := TCarrier.CreateCarrierTypeView(xRec);
    fCarrier.AssignGraphicsParent(fSceneGraphics);

    fCarrier.Visible := true;
    fSceneGraphics.SceneChanged();
end;

procedure TfrmEdCarr.EnableFieldsFirstLastXY;
begin
    edSlotXFirst.Enabled := true;
    edSlotYFirst.Enabled := true;
    edSlotXLast.Enabled := true;
    edSlotYLast.Enabled := true;
end;

procedure TfrmEdCarr.btnApplyClick(Sender: TObject);
begin
    if (SaveChanges) then
    begin
        DisplayCarrier;
        FApplied := true;
    end;
end;

procedure TfrmEdCarr.cbDiscretePositionsClick(Sender: TObject);
var
    xCarrierRec: TCarrierRec;
    x, xNumberOfSlots: integer;
begin
    if cbDiscretePositions.Checked then
    begin
        DisableFieldsFirstLastXY;
        btnConvertCarrierDisPos.Enabled := false;
        self.PageControl1.Pages[5].TabVisible := true;
        self.CarrierRecFromGUI(xCarrierRec);
        xNumberOfSlots := xCarrierRec.Rows * xCarrierRec.Cols;
        // Werte werden erstmal auf Null gesetzt, da Benutzer die Werte festlegen soll
        if not Assigned(xCarrierRec.DiscretePosRecArray) then
        begin
            SetLength(xCarrierRec.DiscretePosRecArray, xNumberOfSlots);
            for x := 0 to xNumberOfSlots - 1 do
            begin
                xCarrierRec.DiscretePosRecArray[x].Position := x + 1;
                xCarrierRec.DiscretePosRecArray[x].X_mm := 0;
                xCarrierRec.DiscretePosRecArray[x].Y_mm := 0;
                xCarrierRec.DiscretePosRecArray[x].Z_mm := 0;
            end;
        end;
        // self.DiscretePositionsToGrid(xCarrierRec.Cols, xCarrierRec.DiscretePosRecArray); vorher
        self.DiscretePositionsToGrid(xNumberOfSlots, xCarrierRec.DiscretePosRecArray);
    end
    else
    begin
        EnableFieldsFirstLastXY;
        self.PageControl1.Pages[5].TabVisible := false;
        btnConvertCarrierDisPos.Enabled := true;
    end;

end;

procedure TfrmEdCarr.chkCalcStackHeightsClick(Sender: TObject);
begin
    lblLastZPos.Visible := not chkCalcStackHeights.Checked;
    edSlotZFirst.Visible := not chkCalcStackHeights.Checked;
end;

class function TfrmEdCarr.InstanceShow(const aCarrierRec: TCarrierRec; aNameReadOnly: boolean;
    out oName: string): boolean;
var
    xInstance: TfrmEdCarr;
begin
    result := false;
    oName := '';
    xInstance := TfrmEdCarr.Create(nil);
    try
        xInstance.CarrierRecToGUI(aCarrierRec, aNameReadOnly);
        xInstance.PageControl1.ActivePageIndex := 0;

        if (xInstance.ShowModal = mrOK) then
        begin
            result := true;
            oName := xInstance.CurrentName;
        end;
    finally
        xInstance.Free;
    end;
end;

class function TfrmEdCarr.InstanceShowToAdd(out oName: string): boolean;
var
    xCarrierRec: TCarrierRec;
begin
    xCarrierRec := TCarrierDataAdaptor.MakeDefaultCarrierRec();
    result := InstanceShow(xCarrierRec, false, oName);
end;

class function TfrmEdCarr.InstanceShowToEdit(const aName: string): boolean;
var
    xCarrierRec: TCarrierRec;
    xDummy: string;
    xDiscreteCarrierDataAdaptor: TDiscreteCarrierDataAdaptor;
begin
    if not TCarrierDataAdaptor.ReadCarrier(aName, xCarrierRec) then
        raise Exception.Create(TLanguageString.Read('Carrier {0} does not exist!',
            'Carrier {0} existiert nicht!', [aName]));

    if xCarrierRec.DiscretePositions then
    begin
        xDiscreteCarrierDataAdaptor := TDiscreteCarrierDataAdaptor.Create;
        try
            // Array füllen
            xDiscreteCarrierDataAdaptor.ReadRecs(aName, xCarrierRec.DiscretePosRecArray);
        finally
            xDiscreteCarrierDataAdaptor.Free;
        end;
    end;

    result := InstanceShow(xCarrierRec, true, xDummy);
end;

class function TfrmEdCarr.InstanceShowToAddFlatCarrier(var vName: string; aX, aY: TPosMM): boolean;
var
    xCarrierRec: TCarrierRec;
begin
    xCarrierRec := TCarrierDataAdaptor.MakeFlatCarrierRec(vName, aX, aY);
    result := InstanceShow(xCarrierRec, false, vName);
end;


end.
