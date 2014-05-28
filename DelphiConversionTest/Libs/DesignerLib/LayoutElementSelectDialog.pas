{ -----------------------------------------------------------------------------------------------------------------------
  Copyright © 2010 Zinsser Analytic GmbH - All rights reserved.
  Author       : Payman Kamali (pk)
  Description  :
  -----------------------------------------------------------------------------------------------------------------------
  Revision History:
  date     op  method                                track-no  improvement/change
  -------- --  ------------------------------------  --------- ----------------------------------------------------------
  06.10.10 pk                                        TN5290     New
  14.02.11 wl                                        TN5468   komplett überarbeitet
  08.07.11 wl  SelectSingleRackPosArrayModal         TN5626   neu, noch nicht fertig
  28.10.11 wl                                        TN5725   an TRack.PaintTubePositions angepasst
  03.11.11 wl  DoCreateRackWell                      TN5725   jetzt mit WellNr
  02.02.11 wl  PaintAllPositions                     TN5791   PaintTubePositions ersetzt
  ----------------------------------------------------------------------------------------------------------------------- }

unit LayoutElementSelectDialog;


interface


uses
    Windows,
    Messages,
    SysUtils,
    Variants,
    Classes,
    Graphics,
    Controls,
    Forms,
    Dialogs,
    StdCtrls,
    ExtCtrls,
    ComCtrls,
    ViewLayout,
    SceneGraphics,
    Rack,
    RackWell,
    Carrier,
    CarrierSlot,
    AppTypes,
    Layout,
    LayoutElementCallbackTypes,
    PopupMenuInfo,
    CustomLayoutManager,
    GeneralTypes;

type
    TLastSelectedType = (lastNone, lastMethodEditor, lastProperties);

    TSelectMode = (Columnwise, Square);

    TChooseRackCallback = procedure(aRackName: string) of object;
    TChooseRackPosCallback = function(const aRackName: string; aPosInt, aMaxRows: integer;
        const aParamIndexOffset: integer; aSetFocus: boolean): string of object;
    TChooseCarrierSlotCallback = procedure(aCarrierName: string; aSlotNr: integer) of object;

    TLayoutElementSelectLayoutDlgEvents = class
    private
        fChooseRackCallback: TChooseRackCallback;
        fChooseRackFirstPosCallback: TChooseRackPosCallback;
        fChooseRackLastPosCallback: TChooseRackPosCallback;
        fChooseCarrierSlotCallback: TChooseCarrierSlotCallback;
        fLastSelected: TLastSelectedType;
        procedure SetLastSelected(const Value: TLastSelectedType);
    public
        constructor Create();
        //
        procedure ChooseRack(aRackName: string);
        function ChooseRackFirstPos(const aRackName: string; aPosInt, aMaxRows: integer;
            const aParamIndexOffset: integer; aSetFocus: boolean): string;
        function ChooseRackLastPos(const aRackName: string; aPosInt, aMaxRows: integer;
            const aParamIndexOffset: integer; aSetFocus: boolean): string;
        procedure ChooseCarrierSlot(aCarrierName: string; aSlotNr: integer);

        class procedure InstanceSetLastSelected(const aValue: TLastSelectedType);
        class function Instance(): TLayoutElementSelectLayoutDlgEvents;
        class procedure CreateInstance();
        class procedure DestroyInstance();

        property ChooseRackCallback: TChooseRackCallback read fChooseRackCallback write fChooseRackCallback;
        property ChooseRackFirstPosCallback: TChooseRackPosCallback read fChooseRackFirstPosCallback
            write fChooseRackFirstPosCallback;
        property ChooseRackLastPosCallback: TChooseRackPosCallback read fChooseRackLastPosCallback
            write fChooseRackLastPosCallback;
        property ChooseCarrierSlotCallback: TChooseCarrierSlotCallback read fChooseCarrierSlotCallback
            write fChooseCarrierSlotCallback;
        property LastSelected: TLastSelectedType read fLastSelected write SetLastSelected;
    end;

    TLayoutElementSelectDlgRackWell = class(TRackWell)
    private
        procedure DoMouseDown(aSender: TObject; aButton: TGraphicsMouseButton; aShift: TGraphicsShiftState;
            aX, aY: double);
        procedure DoMouseUp(aSender: TObject; aButton: TGraphicsMouseButton; aShift: TGraphicsShiftState;
            aX, aY: double);
        procedure DoMouseMove(Sender: TObject; Shift: TGraphicsShiftState; aX, aY: double);
        procedure DoPopup(aSender: TObject; aX, aY: double; var vPopupMenuInfos: TPopupMenuInfoList);
        procedure DoMouseDragOver(aSender, aSource: TObject; aX, aY: double; aState: TGraphicsDragState;
            var vAccept: boolean);
    protected
        procedure DoInitGraphics; override;
    end;

    TLayoutElementSelectDlgRack = class(TRack)
    private
        fMultiSelectFirstWell: TRackWell;
        fMultiSelectLastWell: TRackWell;
        procedure DoPopup(aSender: TObject; aX, aY: double; var vPopupMenuInfos: TPopupMenuInfoList);
        procedure DoMouseUp(aSender: TObject; aButton: TGraphicsMouseButton; aShift: TGraphicsShiftState;
            aX, aY: double);
        procedure DoMouseDown(aSender: TObject; aButton: TGraphicsMouseButton; aShift: TGraphicsShiftState;
            aX, aY: double);
        procedure MultiSelectionDone();
    protected
        procedure DoInitGraphics; override;
        function DoCreateRackWell(aWellNr: integer): TRackWell; override;
    public
        constructor Create();
        procedure PaintAllPositions(const aFirstPosStr, aLastPosStr: string);
        procedure GetSelectedPositions(aFirstWell, aLastWell: TRackWell;
            out oFirstPosInt, oLastPosInt: integer);
        property MultiSelectFirstWell: TRackWell read fMultiSelectFirstWell write fMultiSelectFirstWell;
        property MultiSelectLastWell: TRackWell read fMultiSelectLastWell write fMultiSelectLastWell;
    end;

    TLayoutElementSelectDlgCarrierSlot = class(TCarrierSlot)
    private
        procedure DoMouseDown(aSender: TObject; aButton: TGraphicsMouseButton; aShift: TGraphicsShiftState;
            aX, aY: double);
    protected
        procedure DoInitGraphics; override;
    end;

    TLayoutElementSelectDlgCarrier = class(TCarrier)
    protected
        function DoCreateCarrierSlot(): TCarrierSlot; override;
    end;

    TLayoutElementSelectDlgLayout = class(TLayout)
    protected
        function DoCreateRack: TRack; override;
        function DoCreateCarrier: TCarrier; override;
        procedure DoInitGraphics; override;
    end;

    TLayoutElementSelectDlgLayoutManager = class(TCustomLayoutManager)
    protected
        function DoCreateLayout(const aRunName, aLayoutName: string): TLayout; override;
        function GetTempSettingsSectionName: string; override;
    end;

    TLayoutElementSelectMode = (lsmCarrier, lsmCarrierSlot, lsmRack, lsmRackSinglePos, lsmRackDoublePos,
        lsmSingleRackPosArray);

    TfrmLayoutElementSelectDialog = class(TForm)
        pnlTop: TPanel;
        lblTitle: TLabel;
        pnlBottom: TPanel;
        btnOK: TButton;
        btnCancel: TButton;
        pnlLayout: TPanel;
        PageControl1: TPageControl;
        tbsCarrier: TTabSheet;
        lblCarrierName: TLabel;
        lblCarrierSlot: TLabel;
        cmbCarrierName: TComboBox;
        cmbCarrierSlot: TComboBox;
        tbsRack: TTabSheet;
        lblPositionSeparator: TLabel;
        lblRack: TLabel;
        lblPosition: TLabel;
        edRackFirstPos: TEdit;
        edRackLastPos: TEdit;
        cmbRackName: TComboBox;
        rbInteger: TRadioButton;
        rbMatrix: TRadioButton;
        lblNoOfPos: TLabel;
        edNoOfPos: TEdit;
        btnRefresh: TButton;
        procedure FormCreate(Sender: TObject);
        procedure FormDestroy(Sender: TObject);
        procedure cmbCarrierNameChange(Sender: TObject);
        procedure FormResize(Sender: TObject);
        procedure FormShow(Sender: TObject);
        procedure btnRefreshClick(Sender: TObject);
    private
        fLayoutManager: TLayoutElementSelectDlgLayoutManager;
        fLayoutName: string;
        fViewLayout: TfrmViewLayout;
        fMode: TLayoutElementSelectMode;
        constructor Create(const aLayoutName: string); reintroduce;
        procedure DoOnChooseRack(aRackName: string);
        function DoOnChooseRackFirstPos(const aRackName: string; aPosInt, aMaxRows: integer;
            const aParamIndexOffset: integer; aSetFocus: boolean): string;
        function DoOnChooseRackLastPos(const aRackName: string; aPosInt, aMaxRows: integer;
            const aParamIndexOffset: integer; aSetFocus: boolean): string;
        procedure DoOnChooseCarrierSlot(aCarrierName: string; aSlotNr: integer);
        procedure ChooseCarrierSlotByRackName(const aRackName: string);
        procedure SelectTab();
        procedure FillDropDownLists();
        procedure FitLayoutToScreen();
        procedure ChangeSelectionAfterManualChange();
    public
        destructor Destroy(); override;
        procedure SetMode(const aMode: TLayoutElementSelectMode);
        class function SelectCarrierModal(const aLayoutName: string; var vCarrierName: string): TModalResult;
        class function SelectCarrierSlotModal(const aLayoutName: string; var vCarrierName: string;
            var vCarrierSlot: integer): TModalResult;
        class function SelectRackModal(const aLayoutName: string; var vRackName: string): TModalResult;
        class function SelectRackDoublePosModal(const aLayoutName: string; var vRackName: string;
            var vFirstPos, vLastPos: string; const aTitle: string): TModalResult;
        class function SelectRackSinglePosModal(const aLayoutName: string; var vRackName: string;
            var vPos: string; const aTitle: string): TModalResult;
        class function SelectSingleRackPosArrayModal(const aLayoutName: string; var vRackName: string;
            var vPosArray: TArray<integer>; const aTitle: string): TModalResult;
    end;


implementation


{$R *.dfm}

uses
    MathUtils,
    ControlUtils,
    MethodGUIParsing;

var
    uLayoutEvents: TLayoutElementSelectLayoutDlgEvents;

class function TfrmLayoutElementSelectDialog.SelectCarrierSlotModal(const aLayoutName: string;
    var vCarrierName: string; var vCarrierSlot: integer): TModalResult;
var
    xForm: TfrmLayoutElementSelectDialog;
begin
    xForm := TfrmLayoutElementSelectDialog.Create(aLayoutName);
    try
        xForm.SetMode(lsmCarrierSlot);
        xForm.cmbCarrierName.Text := vCarrierName;
        xForm.cmbCarrierSlot.Text := IntToStr(vCarrierSlot);
        result := xForm.ShowModal;
        if result <> mrOK then
            EXIT;

        vCarrierName := xForm.cmbCarrierName.Text;
        vCarrierSlot := StrToIntDef(xForm.cmbCarrierSlot.Text, 0);
    finally
        FreeAndNil(xForm);
    end;
end;

class function TfrmLayoutElementSelectDialog.SelectCarrierModal(const aLayoutName: string;
    var vCarrierName: string): TModalResult;
var
    xForm: TfrmLayoutElementSelectDialog;
begin
    xForm := TfrmLayoutElementSelectDialog.Create(aLayoutName);
    try
        xForm.SetMode(lsmCarrier);
        xForm.cmbCarrierName.Text := vCarrierName;
        result := xForm.ShowModal;
        if result <> mrOK then
            EXIT;

        vCarrierName := xForm.cmbCarrierName.Text;
    finally
        FreeAndNil(xForm);
    end;
end;

class function TfrmLayoutElementSelectDialog.SelectRackModal(const aLayoutName: string; var vRackName: string)
    : TModalResult;
var
    xForm: TfrmLayoutElementSelectDialog;
begin
    xForm := TfrmLayoutElementSelectDialog.Create(aLayoutName);
    try
        xForm.SetMode(lsmRack);
        xForm.cmbRackName.Text := vRackName;
        result := xForm.ShowModal;
        if result <> mrOK then
            EXIT;

        vRackName := xForm.cmbRackName.Text;
    finally
        FreeAndNil(xForm);
    end;
end;

class function TfrmLayoutElementSelectDialog.SelectRackSinglePosModal(const aLayoutName: string;
    var vRackName: string; var vPos: string; const aTitle: string): TModalResult;
var
    xForm: TfrmLayoutElementSelectDialog;
begin
    xForm := TfrmLayoutElementSelectDialog.Create(aLayoutName);
    try
        xForm.SetMode(lsmRackSinglePos);
        xForm.lblTitle.Caption := TLanguageString.Read('Select ', 'Markiere die ') + aTitle + ':';
        xForm.cmbRackName.Text := vRackName;
        xForm.edRackFirstPos.Text := vPos;
        result := xForm.ShowModal;
        if result <> mrOK then
            EXIT;

        vRackName := xForm.cmbRackName.Text;
        vPos := xForm.edRackFirstPos.Text;
    finally
        FreeAndNil(xForm);
    end;
end;

class function TfrmLayoutElementSelectDialog.SelectSingleRackPosArrayModal(const aLayoutName: string;
    var vRackName: string; var vPosArray: TArray<integer>; const aTitle: string): TModalResult;
var
    xForm: TfrmLayoutElementSelectDialog;
begin
    xForm := TfrmLayoutElementSelectDialog.Create(aLayoutName);
    try
        xForm.SetMode(lsmSingleRackPosArray);
        xForm.lblTitle.Caption := TLanguageString.Read('Select ', 'Markiere die ') + aTitle + ':';
        xForm.cmbRackName.Text := vRackName;
        // xForm.edRackFirstPos.Text := vPos;
        result := xForm.ShowModal;
        if result <> mrOK then
            EXIT;

        vRackName := xForm.cmbRackName.Text;
        // vPos := xForm.edRackFirstPos.Text;
    finally
        FreeAndNil(xForm);
    end;
end;

class function TfrmLayoutElementSelectDialog.SelectRackDoublePosModal(const aLayoutName: string;
    var vRackName: string; var vFirstPos, vLastPos: string; const aTitle: string): TModalResult;
var
    xForm: TfrmLayoutElementSelectDialog;
begin
    xForm := TfrmLayoutElementSelectDialog.Create(aLayoutName);
    try
        xForm.SetMode(lsmRackDoublePos);
        xForm.Caption := aTitle;
        xForm.lblTitle.Caption := TLanguageString.Read('Select ', 'Markiere die ') + aTitle + ':';
        xForm.cmbRackName.Text := vRackName;
        xForm.edRackFirstPos.Text := vFirstPos;
        xForm.edRackLastPos.Text := vLastPos;
        result := xForm.ShowModal;
        if result <> mrOK then
            EXIT;

        vRackName := xForm.cmbRackName.Text;
        vFirstPos := xForm.edRackFirstPos.Text;
        vLastPos := xForm.edRackLastPos.Text;
    finally
        FreeAndNil(xForm);
    end;
end;

procedure TfrmLayoutElementSelectDialog.SetMode(const aMode: TLayoutElementSelectMode);
begin
    fMode := aMode;
    cmbCarrierName.Visible := fMode in [lsmCarrier, lsmCarrierSlot];
    lblCarrierName.Visible := fMode in [lsmCarrier, lsmCarrierSlot];

    cmbCarrierSlot.Visible := fMode = lsmCarrierSlot;
    lblCarrierSlot.Visible := fMode = lsmCarrierSlot;

    cmbRackName.Visible := fMode in [lsmRack, lsmRackSinglePos, lsmRackDoublePos];
    lblRack.Visible := fMode in [lsmRack, lsmRackSinglePos, lsmRackDoublePos];

    edRackFirstPos.Visible := fMode in [lsmRackSinglePos, lsmRackDoublePos];
    edRackLastPos.Visible := fMode in [lsmRackDoublePos];
    lblPosition.Visible := fMode in [lsmRackSinglePos, lsmRackDoublePos];
    lblPositionSeparator.Visible := edRackLastPos.Visible;

    lblNoOfPos.Visible := fMode in [lsmRackDoublePos];
    edNoOfPos.Visible := fMode in [lsmRackDoublePos];
    rbInteger.Visible := fMode in [lsmRackDoublePos];
    rbMatrix.Visible := fMode in [lsmRackDoublePos];

    SelectTab();

    FillDropDownLists();
end;

procedure TfrmLayoutElementSelectDialog.FillDropDownLists();
var
    xNames: TStringArray;
begin
    xNames := fLayoutManager.CurrentLayout.GetCarrierNames;
    TControlUtils.AddValuesToComboBox(xNames, self.cmbCarrierName, true);

    xNames := fLayoutManager.CurrentLayout.GetRackNames;
    TControlUtils.AddValuesToComboBox(xNames, self.cmbRackName, true);
end;

procedure TfrmLayoutElementSelectDialog.SelectTab();
var
    x: integer;
begin
    for x := 0 to self.PageControl1.PageCount - 1 do
    begin
        self.PageControl1.Pages[x].TabVisible := false;
    end;

    if fMode in [lsmCarrier, lsmCarrierSlot] then
        self.PageControl1.ActivePage := self.tbsCarrier
    else if fMode in [lsmRack, lsmRackSinglePos, lsmRackDoublePos] then
        self.PageControl1.ActivePage := self.tbsRack;
end;

procedure TfrmLayoutElementSelectDialog.cmbCarrierNameChange(Sender: TObject);
var
    xCarrier: TCarrier;
    xCarrierSlots: TStringArray;
    x: integer;
begin
    xCarrier := fLayoutManager.CurrentLayout.FindCarrierByName(self.cmbCarrierName.Text);

    SetLength(xCarrierSlots, 0);
    if Assigned(xCarrier) then
    begin

        SetLength(xCarrierSlots, xCarrier.SlotCount);
        for x := 0 to xCarrier.SlotCount - 1 do
            xCarrierSlots[x] := IntToStr(x + 1);
    end;

    TControlUtils.AddValuesToComboBox(xCarrierSlots, self.cmbCarrierSlot, true);
end;

constructor TfrmLayoutElementSelectDialog.Create(const aLayoutName: string);
begin
    inherited Create(nil);
    fLayoutName := aLayoutName;

    TLayoutElementSelectLayoutDlgEvents.CreateInstance;
    uLayoutEvents.Instance.ChooseCarrierSlotCallback := self.DoOnChooseCarrierSlot;
    uLayoutEvents.Instance.ChooseRackCallback := self.DoOnChooseRack;
    uLayoutEvents.Instance.ChooseRackFirstPosCallback := self.DoOnChooseRackFirstPos;
    uLayoutEvents.Instance.ChooseRackLastPosCallback := self.DoOnChooseRackLastPos;

    self.rbMatrix.Caption := TLanguageString.Read('as square', 'als Rechteck');
    self.rbInteger.Caption := TLanguageString.Read('columnwise', 'spaltenweise');
    self.btnRefresh.Caption := TLanguageString.Read('Show', 'Zeigen');
    self.lblNoOfPos.Caption := TLanguageString.Read('Number of pos.', 'Anzahl Positionen');
end;

destructor TfrmLayoutElementSelectDialog.Destroy();
begin
    TLayoutElementSelectLayoutDlgEvents.DestroyInstance();
    inherited;
end;

procedure TfrmLayoutElementSelectDialog.DoOnChooseCarrierSlot(aCarrierName: string; aSlotNr: integer);
begin
    self.cmbCarrierName.Text := aCarrierName;
    self.cmbCarrierSlot.Text := IntToStr(aSlotNr);
end;

procedure TfrmLayoutElementSelectDialog.FormCreate(Sender: TObject);
begin
    fViewLayout := TfrmViewLayout.Create(nil);
    fViewLayout.Parent := self.pnlLayout;
    fViewLayout.Align := alClient;
    fViewLayout.Visible := true;

    fLayoutManager := TLayoutElementSelectDlgLayoutManager.Create(fViewLayout.DrawPanel);
    fViewLayout.SceneGraphics := fLayoutManager.GetSceneGraphicsForLayout(nil);

    fLayoutManager.RegisterLayout('', fLayoutName);

    fLayoutManager.Load();
    FitLayoutToScreen();
end;

procedure TfrmLayoutElementSelectDialog.FormDestroy(Sender: TObject);
begin
    fLayoutManager.UnregisterCurrentLayout();
    fLayoutManager.DestroyDefaultSceneGraphics();
    FreeAndNil(fLayoutManager);
    FreeAndNil(fViewLayout);
end;

procedure TfrmLayoutElementSelectDialog.FitLayoutToScreen();
begin
    fViewLayout.btnFitToScreenClick(fViewLayout.btnFitToScreen);
end;

procedure TfrmLayoutElementSelectDialog.FormResize(Sender: TObject);
begin
    FitLayoutToScreen();
end;

procedure TfrmLayoutElementSelectDialog.FormShow(Sender: TObject);
begin
    ChangeSelectionAfterManualChange();
end;

procedure TfrmLayoutElementSelectDialog.btnRefreshClick(Sender: TObject);
begin
    self.ChangeSelectionAfterManualChange();
end;

procedure TfrmLayoutElementSelectDialog.ChangeSelectionAfterManualChange();
var
    xRack: TRack;
    xLastPos: integer;
    xFirstCoordType, xLastCoordType: TCoordType;
begin
    if (self.fMode = lsmRackDoublePos) then
    begin
        // Select Mode ändern
        xFirstCoordType := TMethodGUIParser.GetCoordType(self.edRackFirstPos.Text);
        xLastCoordType := TMethodGUIParser.GetCoordType(self.edRackLastPos.Text);
        if (xFirstCoordType = ctMatrix) and (xLastCoordType = ctMatrix) then
        begin
            self.rbMatrix.Checked := true;
        end
        else if (xFirstCoordType = ctInteger) and (xLastCoordType = ctInteger) then
        begin
            self.rbInteger.Checked := true;
        end
        else
        begin
            EXIT;
        end;

        // Anzahl der Positionen ermitteln
        xLastPos := TMethodGUIParser.GetLastPos(self.edRackFirstPos.Text, self.edRackLastPos.Text);
        self.edNoOfPos.Text := IntToStr(xLastPos);

        // Rackposiitonen markieren
        xRack := fLayoutManager.CurrentLayout.FindRackByName(self.cmbRackName.Text);
        if (xRack is TLayoutElementSelectDlgRack) then
            (xRack as TLayoutElementSelectDlgRack).PaintAllPositions(self.edRackFirstPos.Text,
                self.edRackLastPos.Text);
    end;

    if (self.fMode = lsmRackSinglePos) then
    begin
        // Select Mode ändern
        self.rbInteger.Checked := true;

        // Rackposiitonen markieren
        xRack := fLayoutManager.CurrentLayout.FindRackByName(self.cmbRackName.Text);
        if (xRack is TLayoutElementSelectDlgRack) then
            (xRack as TLayoutElementSelectDlgRack).PaintAllPositions(self.edRackFirstPos.Text,
                self.edRackFirstPos.Text);
    end;
end;

procedure TfrmLayoutElementSelectDialog.ChooseCarrierSlotByRackName(const aRackName: string);
var
    xRack: TRack;
    xSlot: TCarrierSlot;
    xCarrier: TCarrier;
begin
    if self.fMode in [lsmCarrier, lsmCarrierSlot] then
    begin
        xRack := fLayoutManager.CurrentLayout.FindRackByName(aRackName);
        if not Assigned(xRack.Slot) then
            EXIT;

        xSlot := (xRack.Slot as TCarrierSlot);
        xCarrier := xSlot.Carrier as TCarrier;
        self.DoOnChooseCarrierSlot(xCarrier.Name, xSlot.SlotNr);
    end
end;

procedure TfrmLayoutElementSelectDialog.DoOnChooseRack(aRackName: string);
begin
    if self.fMode in [lsmCarrier, lsmCarrierSlot] then
    begin
        ChooseCarrierSlotByRackName(aRackName);
    end
    else if self.fMode = lsmRack then
    begin
        // only on Rack mode! on position mode dont change the rack text if a position is not selected
        cmbRackName.Text := aRackName;
    end;
end;

function TfrmLayoutElementSelectDialog.DoOnChooseRackFirstPos(const aRackName: string;
    aPosInt, aMaxRows: integer; const aParamIndexOffset: integer; aSetFocus: boolean): string;
begin
    result := '';
    if self.fMode in [lsmCarrier, lsmCarrierSlot] then
    begin
        ChooseCarrierSlotByRackName(aRackName);
    end
    else
    begin
        cmbRackName.Text := aRackName;
        if (self.rbInteger.Checked) then
            result := IntToStr(aPosInt)
        else
            result := TMethodGUIParser.GetCoordString(aMaxRows, aPosInt);
        edRackFirstPos.Text := result;
    end;
end;

function TfrmLayoutElementSelectDialog.DoOnChooseRackLastPos(const aRackName: string;
    aPosInt, aMaxRows: integer; const aParamIndexOffset: integer; aSetFocus: boolean): string;
begin
    if self.fMode in [lsmCarrier, lsmCarrierSlot] then
    begin
        ChooseCarrierSlotByRackName(aRackName);
    end
    else
    begin
        cmbRackName.Text := aRackName;
        if (self.rbInteger.Checked) then
            result := IntToStr(aPosInt)
        else
            result := TMethodGUIParser.GetCoordString(aMaxRows, aPosInt);
        edRackLastPos.Text := result;
        self.edNoOfPos.Text := IntToStr(TMethodGUIParser.GetLastPos(edRackFirstPos.Text, edRackLastPos.Text));
    end;
end;

{ TLayoutElementSelectLayoutDlgEvents }

class function TLayoutElementSelectLayoutDlgEvents.Instance(): TLayoutElementSelectLayoutDlgEvents;
begin
    result := uLayoutEvents;
end;

class procedure TLayoutElementSelectLayoutDlgEvents.CreateInstance();
begin
    uLayoutEvents := TLayoutElementSelectLayoutDlgEvents.Create();
end;

class procedure TLayoutElementSelectLayoutDlgEvents.DestroyInstance();
begin
    FreeAndNil(uLayoutEvents);
end;

class procedure TLayoutElementSelectLayoutDlgEvents.InstanceSetLastSelected(const aValue: TLastSelectedType);
begin
    uLayoutEvents.LastSelected := aValue;
end;

constructor TLayoutElementSelectLayoutDlgEvents.Create();
begin
    inherited Create();

    fLastSelected := lastNone;
end;

procedure TLayoutElementSelectLayoutDlgEvents.ChooseCarrierSlot(aCarrierName: string; aSlotNr: integer);
begin
    self.fChooseCarrierSlotCallback(aCarrierName, aSlotNr);
end;

procedure TLayoutElementSelectLayoutDlgEvents.ChooseRack(aRackName: string);
begin
    self.fChooseRackCallback(aRackName);
end;

function TLayoutElementSelectLayoutDlgEvents.ChooseRackFirstPos(const aRackName: string;
    aPosInt, aMaxRows: integer; const aParamIndexOffset: integer; aSetFocus: boolean): string;
begin
    result := fChooseRackFirstPosCallback(aRackName, aPosInt, aMaxRows, aParamIndexOffset, aSetFocus);
end;

function TLayoutElementSelectLayoutDlgEvents.ChooseRackLastPos(const aRackName: string;
    aPosInt, aMaxRows: integer; const aParamIndexOffset: integer; aSetFocus: boolean): string;
begin
    result := fChooseRackLastPosCallback(aRackName, aPosInt, aMaxRows, aParamIndexOffset, aSetFocus);
end;

procedure TLayoutElementSelectLayoutDlgEvents.SetLastSelected(const Value: TLastSelectedType);
begin
    fLastSelected := Value;
end;

{ TLayoutElementSelectDlgRackWell }

procedure TLayoutElementSelectDlgRackWell.DoInitGraphics;
begin
    inherited;
    self.Graphics.Callbacks.PopupCallback := DoPopup;
    self.Graphics.Callbacks.MouseDownCallback := DoMouseDown;
    self.Graphics.Callbacks.MouseUpCallback := DoMouseUp;
    self.Graphics.Callbacks.MouseMoveCallback := DoMouseMove;
    self.Graphics.Callbacks.MouseDragOverCallback := DoMouseDragOver;
end;

procedure TLayoutElementSelectDlgRackWell.DoMouseDragOver(aSender, aSource: TObject; aX, aY: double;
    aState: TGraphicsDragState; var vAccept: boolean);
begin
    if aState = gdsDragMove then
    begin
        Sleep(100);
    end;
end;

procedure TLayoutElementSelectDlgRackWell.DoPopup(aSender: TObject; aX, aY: double;
    var vPopupMenuInfos: TPopupMenuInfoList);
begin
    (self.Rack as TRack).Graphics.Callbacks.PopupCallback((self.Rack as TRack), aX, aY, vPopupMenuInfos);
end;

procedure TLayoutElementSelectDlgRackWell.DoMouseDown(aSender: TObject; aButton: TGraphicsMouseButton;
    aShift: TGraphicsShiftState; aX, aY: double);
var
    xRack: TLayoutElementSelectDlgRack;
    xFirstPos, xLastPos: string;
    xFirstPosInt, xLastPosInt: integer;
begin
    if aButton <> gmbLeft then
        EXIT;

    xRack := self.Rack as TLayoutElementSelectDlgRack;
    xRack.MultiSelectFirstWell := self;
    xRack.GetSelectedPositions(self, self, xFirstPosInt, xLastPosInt);
    xFirstPos := uLayoutEvents.ChooseRackFirstPos(xRack.Name, xFirstPosInt, xRack.Structure.Rows, 1, false);
    xLastPos := xFirstPos;
    xRack.PaintAllPositions(xFirstPos, xLastPos);
end;

procedure TLayoutElementSelectDlgRackWell.DoMouseUp(aSender: TObject; aButton: TGraphicsMouseButton;
    aShift: TGraphicsShiftState; aX, aY: double);
var
    xRack: TLayoutElementSelectDlgRack;
begin
    xRack := self.Rack as TLayoutElementSelectDlgRack;
    if not Assigned(xRack.MultiSelectFirstWell) then
        EXIT;

    xRack.MultiSelectLastWell := self;

    xRack.MultiSelectionDone();
end;

procedure TLayoutElementSelectDlgRackWell.DoMouseMove(Sender: TObject; Shift: TGraphicsShiftState;
    aX, aY: double);
var
    xRack: TLayoutElementSelectDlgRack;
    xFirstPos, xLastPos: string;
    xFirstPosInt, xLastPosInt: integer;
begin
    if not(gssLeft in Shift) then
        EXIT;

    xRack := self.Rack as TLayoutElementSelectDlgRack;

    if not Assigned(xRack.MultiSelectFirstWell) then
        EXIT;

    if Assigned(xRack.MultiSelectLastWell) then
    begin
        if self.WellNr = xRack.MultiSelectLastWell.WellNr then
            EXIT;
    end;

    xRack.MultiSelectLastWell := self;

    xRack.GetSelectedPositions(xRack.MultiSelectFirstWell, self, xFirstPosInt, xLastPosInt);
    xFirstPos := uLayoutEvents.ChooseRackFirstPos(xRack.Name, xFirstPosInt, xRack.Structure.Rows, 1, false);
    xLastPos := uLayoutEvents.ChooseRackLastPos(xRack.Name, xLastPosInt, xRack.Structure.Rows, 2, false);
    xRack.PaintAllPositions(xFirstPos, xLastPos);
end;

{ TLayoutElementSelectDlgRack }

constructor TLayoutElementSelectDlgRack.Create();
begin
    inherited Create();
end;

procedure TLayoutElementSelectDlgRack.DoInitGraphics;
begin
    inherited;

    self.Graphics.Callbacks.PopupCallback := self.DoPopup;
    self.Graphics.Callbacks.MouseUpCallback := self.DoMouseUp;
    self.Graphics.Callbacks.MouseDownCallback := DoMouseDown;
end;

procedure TLayoutElementSelectDlgRack.DoMouseDown(aSender: TObject; aButton: TGraphicsMouseButton;
    aShift: TGraphicsShiftState; aX, aY: double);
begin
    uLayoutEvents.ChooseRack(self.Name);
end;

procedure TLayoutElementSelectDlgRack.GetSelectedPositions(aFirstWell, aLastWell: TRackWell;
    out oFirstPosInt, oLastPosInt: integer);
var
    xPos1, xPos2: integer;
begin
    xPos1 := aFirstWell.WellNr;
    xPos2 := aLastWell.WellNr;
    oFirstPosInt := TMathUtils.MinIntValue([xPos1, xPos2]);
    oLastPosInt := TMathUtils.MaxIntValue([xPos1, xPos2]);
end;

procedure TLayoutElementSelectDlgRack.MultiSelectionDone();
var
    xRack: TLayoutElementSelectDlgRack;
    xFirstPosInt, xLastPosInt: integer;
begin
    xRack := self as TLayoutElementSelectDlgRack;
    if (not Assigned(xRack.MultiSelectFirstWell)) or (not Assigned(xRack.MultiSelectFirstWell)) then
        EXIT;

    xRack.GetSelectedPositions(xRack.MultiSelectFirstWell, xRack.MultiSelectLastWell, xFirstPosInt,
        xLastPosInt);
    uLayoutEvents.ChooseRackLastPos(xRack.Name, xLastPosInt, self.Structure.Rows, 2, true);
    xRack.MultiSelectFirstWell := nil;
end;

procedure TLayoutElementSelectDlgRack.PaintAllPositions(const aFirstPosStr, aLastPosStr: string);
var
    xPosArray: TIntArray;
    x: integer;
begin
    self.ClearAllTubePaint();

    xPosArray := TMethodGUIParser.GetPositionArray(aFirstPosStr, aLastPosStr, self.Structure.Rows,
        self.Structure.Cols, 0);

    for x := 0 to high(xPosArray) do
        self.PaintTubePos(xPosArray[x], TRackWellDisplayType.Highlight);
end;

procedure TLayoutElementSelectDlgRack.DoMouseUp(aSender: TObject; aButton: TGraphicsMouseButton;
    aShift: TGraphicsShiftState; aX, aY: double);
begin
    MultiSelectionDone();
end;

procedure TLayoutElementSelectDlgRack.DoPopup(aSender: TObject; aX, aY: double;
    var vPopupMenuInfos: TPopupMenuInfoList);
begin
    vPopupMenuInfos := self.Graphics.PopupMenuInfos;
end;

function TLayoutElementSelectDlgRack.DoCreateRackWell(aWellNr: integer): TRackWell;
begin
    result := TLayoutElementSelectDlgRackWell.Create(aWellNr);
end;

function TLayoutElementSelectDlgCarrier.DoCreateCarrierSlot: TCarrierSlot;
begin
    result := TLayoutElementSelectDlgCarrierSlot.Create();
end;

{ TLayoutElementSelectDlgLayout }

function TLayoutElementSelectDlgLayout.DoCreateRack: TRack;
begin
    result := TLayoutElementSelectDlgRack.Create();
end;

function TLayoutElementSelectDlgLayout.DoCreateCarrier(): TCarrier;
begin
    result := TLayoutElementSelectDlgCarrier.Create();
end;

procedure TLayoutElementSelectDlgLayout.DoInitGraphics;
begin
    inherited;
end;

{ TLayoutElementSelectDlgCarrierSlot }

procedure TLayoutElementSelectDlgCarrierSlot.DoInitGraphics;
begin
    inherited;
    self.Graphics.Callbacks.MouseDownCallback := DoMouseDown;
end;

procedure TLayoutElementSelectDlgCarrierSlot.DoMouseDown(aSender: TObject; aButton: TGraphicsMouseButton;
    aShift: TGraphicsShiftState; aX, aY: double);
begin
    if not Assigned(self.Rack) then
        uLayoutEvents.ChooseCarrierSlot((self.Carrier as TCarrier).Name, self.SlotNr)
end;

{ TLayoutElementSelectDlgLayoutManager }

function TLayoutElementSelectDlgLayoutManager.DoCreateLayout(const aRunName, aLayoutName: string): TLayout;
begin
    result := TLayoutElementSelectDlgLayout.Create(aLayoutName, aRunName);
end;

function TLayoutElementSelectDlgLayoutManager.GetTempSettingsSectionName: string;
begin
    result := 'LayoutElementSelectDlg';
end;


end.
