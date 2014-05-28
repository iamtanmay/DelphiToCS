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
  28.02.13 wl  SelectSingleRackPosArrayModal         TN6045   wieder entfernt
  22.04.13 wl                                        TN6095   für MultiPageDialog geändert
  14.05.13 wl                                        TN6095   mit Caption
  08.08.13 wl                                        TN6095   verbesserte Benutzerführung
  20.01.14 ts                                        TN6348   FitLayoutToScreen auskommentiert, sonst wird gespeicherter Zoom nicht verwendet
  10.02.14 ts                                        TN6353   Volume,Substance,LiqPar added
  13.02.14 ts                                        TN6353   Bugfix
  08.04.14 ts                                        TN6391   SourceRackName,-Position hinzu für Anzeige von 2.Layout
  25.04.14 ts                                        TN6400   TN6391 ohne 2.Layout werden zoom Werte nicht korrekt gespeichert
  ----------------------------------------------------------------------------------------------------------------------- }

unit LayoutElementSelectDialog;


interface


uses
    Forms,
    Classes,
    Controls,
    StdCtrls,
    ExtCtrls,
    ComCtrls,

    ViewLayout,
    MultiPageDialog,
    EditableParameter,
    LayoutElementSelectDlgEvents,
    CustomLayoutManager;

type
    TLayoutElementSelectMode = (lsmCarrier, lsmCarrierSlot, lsmRack, lsmRackSinglePos, lsmRackDoublePos,
        lsmRackDoublePosExtended, lsmTwoRackDoublePosExtended, lsmRackSinglePosExtended,
        lsmTwoRackSinglePosExtended);

    TfrmLayoutElementSelectDialog = class(TMultiDialogPage)
        pnlTop: TPanel;
        lblTitle: TLabel;
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
        edVolume: TEdit;
        lblVolume: TLabel;
        lblSubstance: TLabel;
        lblLiqParam: TLabel;
        cmbLiqParam: TComboBox;
        cmbSubstance: TComboBox;
        lblSource: TLabel;
        cmbSourceRackname: TComboBox;
        lblSourcePosition: TLabel;
        edSourceRackFirstPos: TEdit;
        edSourceRackLastPos: TEdit;
        Label7: TLabel;
        btnRefreshSource: TButton;
        lblNoOfSourcePos: TLabel;
        edNoOfSourcePos: TEdit;
        pnlSourceLayout: TPanel;

        procedure cmbCarrierNameChange(Sender: TObject);
        procedure FormResize(Sender: TObject);
        procedure FormShow(Sender: TObject);
        procedure btnRefreshClick(Sender: TObject);
    private
        fLayoutManager, fSourceLayoutManager: TLayoutElementSelectDlgLayoutManager;
        fLayoutName: string;
        fCheckValues: boolean;
        fViewLayout, fViewSourceLayout: TfrmViewLayout;
        fMode: TLayoutElementSelectMode;
        fCarrierParam: TEditableParameter;
        fCarrierSlotParam: TEditableParameter;
        fRackParam, fSourceRackParam: TEditableParameter;
        fFirstPosParam, fFirstSourcePosParam: TEditableParameter;
        fLastPosParam, fLastSourcePosParam: TEditableParameter;
        fVolumeParam: TEditableParameter;
        fSubstanceParam: TEditableParameter;
        fLiqParamParam: TEditableParameter;
        fDialogEvents, fSourceDialogEvents: TLayoutElementSelectDlgEvents;

        procedure DoOnChooseRack(aRackName: string);
        function DoOnChooseRackFirstPos(const aRackName: string; aPosInt, aMaxRows: integer;
            const aParamIndexOffset: integer; aSetFocus: boolean): string;
        function DoOnChooseRackLastPos(const aRackName: string; aPosInt, aMaxRows: integer;
            const aParamIndexOffset: integer; aSetFocus: boolean): string;
        function DoOnChooseSourceRackFirstPos(const aRackName: string; aPosInt, aMaxRows: integer;
            const aParamIndexOffset: integer; aSetFocus: boolean): string;
        function DoOnChooseSourceRackLastPos(const aRackName: string; aPosInt, aMaxRows: integer;
            const aParamIndexOffset: integer; aSetFocus: boolean): string;
        procedure DoOnChooseCarrierSlot(aCarrierName: string; aSlotNr: integer);
        procedure ChooseCarrierSlotByRackName(const aRackName: string);
        procedure SelectTab();
        procedure FillDropDownLists();
        // procedure FitLayoutToScreen();
        procedure ChangeSelectionAfterManualChange();
        procedure SetMode(const aMode: TLayoutElementSelectMode);
    public
        constructor Create(aOwner: TComponent; const aLayoutName: string; aCheckValues: boolean); reintroduce;
        destructor Destroy(); override;

        procedure SetCarrierMode(const aDialogCaption: string; aCarrierParam: TEditableParameter);
        procedure SetCarrierSlotMode(const aDialogCaption: string;
            aCarrierParam, aCarrierSlotParam: TEditableParameter);
        procedure SetRackMode(const aDialogCaption: string; aRackParam: TEditableParameter);
        procedure SetRackSinglePosMode(const aDialogCaption: string;
            aRackParam, aPosParam: TEditableParameter);
        procedure SetRackSinglePosModeExtended(const aDialogCaption: string;
            aRackParam, aPosParam, aSourceRackParam, aSourcePosParam, aVolume, aSubstance,
            aLiqParam: TEditableParameter);
        procedure SetRackDoublePosMode(const aDialogCaption: string;
            aRackParam, aFirstPosParam, aLastPosParam: TEditableParameter);
        procedure SetRackDoublePosModeExtended(const aDialogCaption: string;
            aRackParam, aFirstPosParam, aLastPosParam, aSourceRackParam, aFirstSourcePosParam,
            aLastSourcePosParam, aVolume, aSubstance, aLiqParam: TEditableParameter);

        procedure FirstSetFocus(); override;
        procedure RefreshPageData(); override;
        function WritePageData(aCheckBefore: boolean): boolean; override;
        function GetEvents: TLayoutElementSelectDlgEvents;
        function GetSourceEvents: TLayoutElementSelectDlgEvents;
    end;


implementation


{$R *.dfm}

uses
    SysUtils,
    Windows,

    MathUtils,
    AppTypes,
    GeneralTypes,
    DialogUtils,
    Carrier,
    Rack,
    CarrierSlot,
    ControlUtils,
    MethodGUIParsing,
    SubstanceDataDataAdaptor,
    LiqHDataAdaptor;

{ TfrmLayoutElementSelectDialog }

constructor TfrmLayoutElementSelectDialog.Create(aOwner: TComponent; const aLayoutName: string;
    aCheckValues: boolean);
begin
    inherited Create();

    fLayoutName := aLayoutName;
    fCheckValues := aCheckValues;

    fDialogEvents := TLayoutElementSelectDlgEvents.Create;
    fDialogEvents.OnChooseCarrierSlot := self.DoOnChooseCarrierSlot;
    fDialogEvents.OnChooseRack := self.DoOnChooseRack;
    fDialogEvents.OnChooseRackFirstPos := self.DoOnChooseRackFirstPos;
    fDialogEvents.OnChooseRackLastPos := self.DoOnChooseRackLastPos;

    fSourceDialogEvents := TLayoutElementSelectDlgEvents.Create;
    fSourceDialogEvents.OnChooseRackFirstPos := self.DoOnChooseSourceRackFirstPos;
    fSourceDialogEvents.OnChooseRackLastPos := self.DoOnChooseSourceRackLastPos;

    fViewLayout := TfrmViewLayout.Create(nil);
    fViewLayout.Parent := self.pnlLayout;
    fViewLayout.Align := alClient;
    fViewLayout.Visible := true;

    fViewSourceLayout := TfrmViewLayout.Create(nil);
    fViewSourceLayout.Parent := self.pnlSourceLayout;
    fViewSourceLayout.Align := alClient;
    fViewSourceLayout.Visible := true;

    fLayoutManager := TLayoutElementSelectDlgLayoutManager.Create(fViewLayout.DrawPanel, GetEvents);
    fViewLayout.SceneGraphics := fLayoutManager.GetSceneGraphicsForLayout(nil);

    fLayoutManager.RegisterLayout('', fLayoutName);
    fLayoutManager.Load();

    fSourceLayoutManager := TLayoutElementSelectDlgLayoutManager.Create(fViewSourceLayout.DrawPanel,
        GetSourceEvents);
    fViewSourceLayout.SceneGraphics := fSourceLayoutManager.GetSceneGraphicsForLayout(nil);

    fSourceLayoutManager.RegisterLayout('', fLayoutName);
    fSourceLayoutManager.Load();
    // FitLayoutToScreen();

    self.rbMatrix.Caption := TLanguageString.Read('as square', 'als Rechteck');
    self.rbInteger.Caption := TLanguageString.Read('columnwise', 'spaltenweise');
    self.btnRefresh.Caption := TLanguageString.Read('Show', 'Zeigen');
    self.lblNoOfPos.Caption := TLanguageString.Read('Number of pos.', 'Anzahl Positionen');
end;

destructor TfrmLayoutElementSelectDialog.Destroy();
begin
    fLayoutManager.UnregisterCurrentLayout();
    fLayoutManager.DestroyDefaultSceneGraphics();
    FreeAndNil(fLayoutManager);
    FreeAndNil(fViewLayout);
    FreeAndNil(fDialogEvents);

    if Assigned(fSourceLayoutManager) then
    begin
        fSourceLayoutManager.UnregisterCurrentLayout();
        fSourceLayoutManager.DestroyDefaultSceneGraphics();
        FreeAndNil(fSourceLayoutManager);
    end;
    FreeAndNil(fViewSourceLayout);
    FreeAndNil(fSourceDialogEvents);

    inherited;
end;

procedure TfrmLayoutElementSelectDialog.SetMode(const aMode: TLayoutElementSelectMode);
begin
    fMode := aMode;

    cmbCarrierName.Visible := fMode in [lsmCarrier, lsmCarrierSlot];
    lblCarrierName.Visible := fMode in [lsmCarrier, lsmCarrierSlot];

    cmbCarrierSlot.Visible := fMode = lsmCarrierSlot;
    lblCarrierSlot.Visible := fMode = lsmCarrierSlot;

    cmbRackName.Visible := fMode in [lsmRack, lsmRackSinglePos, lsmRackSinglePosExtended,
        lsmTwoRackSinglePosExtended, lsmRackDoublePos, lsmRackDoublePosExtended, lsmTwoRackDoublePosExtended];
    lblRack.Visible := fMode in [lsmRack, lsmRackSinglePos, lsmRackSinglePosExtended,
        lsmTwoRackSinglePosExtended, lsmRackDoublePos, lsmRackDoublePosExtended, lsmTwoRackDoublePosExtended];

    edRackFirstPos.Visible := fMode in [lsmRackSinglePos, lsmRackSinglePosExtended,
        lsmTwoRackSinglePosExtended, lsmRackDoublePos, lsmRackDoublePosExtended, lsmTwoRackDoublePosExtended];
    edRackLastPos.Visible := fMode in [lsmRackDoublePos, lsmRackDoublePosExtended,
        lsmTwoRackDoublePosExtended];
    lblPosition.Visible := fMode in [lsmRackSinglePos, lsmRackSinglePosExtended, lsmTwoRackSinglePosExtended,
        lsmRackDoublePos, lsmRackDoublePosExtended, lsmTwoRackDoublePosExtended];
    lblPositionSeparator.Visible := edRackLastPos.Visible;

    lblNoOfPos.Visible := fMode in [lsmRackDoublePos, lsmRackDoublePosExtended, lsmTwoRackDoublePosExtended];
    edNoOfPos.Visible := fMode in [lsmRackDoublePos, lsmRackDoublePosExtended, lsmTwoRackDoublePosExtended];
    rbInteger.Visible := fMode in [lsmRackDoublePos, lsmRackDoublePosExtended, lsmTwoRackDoublePosExtended];
    rbMatrix.Visible := fMode in [lsmRackDoublePos, lsmRackDoublePosExtended, lsmTwoRackDoublePosExtended];

    edVolume.Visible := fMode in [lsmRackDoublePosExtended, lsmTwoRackDoublePosExtended,
        lsmRackSinglePosExtended, lsmTwoRackSinglePosExtended];
    lblVolume.Visible := fMode in [lsmRackDoublePosExtended, lsmTwoRackDoublePosExtended,
        lsmRackSinglePosExtended, lsmTwoRackSinglePosExtended];
    cmbSubstance.Visible := fMode in [lsmRackDoublePosExtended, lsmTwoRackDoublePosExtended,
        lsmRackSinglePosExtended, lsmTwoRackSinglePosExtended];
    lblSubstance.Visible := fMode in [lsmRackDoublePosExtended, lsmTwoRackDoublePosExtended,
        lsmRackSinglePosExtended, lsmTwoRackSinglePosExtended];
    cmbLiqParam.Visible := fMode in [lsmRackDoublePosExtended, lsmTwoRackDoublePosExtended,
        lsmRackSinglePosExtended, lsmTwoRackSinglePosExtended];
    lblLiqParam.Visible := fMode in [lsmRackDoublePosExtended, lsmTwoRackDoublePosExtended,
        lsmRackSinglePosExtended, lsmTwoRackSinglePosExtended];

    SelectTab();

    FillDropDownLists();
end;

procedure TfrmLayoutElementSelectDialog.FillDropDownLists();
var
    xNames: TArray<string>;
    xDA: TLiqHDataAdaptor;
begin
    xNames := fLayoutManager.CurrentLayout.GetCarrierNames;
    TControlUtils.AddValuesToComboBox(xNames, self.cmbCarrierName, true);

    xNames := fLayoutManager.CurrentLayout.GetRackNames;
    TControlUtils.AddValuesToComboBox(xNames, self.cmbRackName, true);
    TControlUtils.AddValuesToComboBox(xNames, self.cmbSourceRackname, true);

    xNames := TSubstanceDataDataAdaptor.InstReadAllNames;
    TControlUtils.AddValuesToComboBox(xNames, self.cmbSubstance, true);

    xDA := TLiqHDataAdaptor.Create();
    try
        xNames := xDA.ReadAllNames();
    finally
        FreeAndNil(xDA);
    end;
    TControlUtils.AddValuesToComboBox(xNames, self.cmbLiqParam, true);
end;

procedure TfrmLayoutElementSelectDialog.FirstSetFocus;
begin
    self.NextButtonSetDefault(true);
    if self.fMode in [lsmCarrier, lsmCarrierSlot] then
        self.cmbCarrierName.SetFocus
    else
        self.cmbRackName.SetFocus;
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
    else if fMode in [lsmRack, lsmRackSinglePos, lsmRackSinglePosExtended, lsmTwoRackSinglePosExtended,
        lsmRackDoublePos, lsmRackDoublePosExtended, lsmTwoRackDoublePosExtended] then
        self.PageControl1.ActivePage := self.tbsRack;
end;

procedure TfrmLayoutElementSelectDialog.cmbCarrierNameChange(Sender: TObject);
var
    xCarrier: TCarrier;
    xCarrierSlots: TArray<string>;
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

procedure TfrmLayoutElementSelectDialog.DoOnChooseCarrierSlot(aCarrierName: string; aSlotNr: integer);
begin
    self.cmbCarrierName.Text := aCarrierName;
    self.cmbCarrierSlot.Text := IntToStr(aSlotNr);
end;

{ procedure TfrmLayoutElementSelectDialog.FitLayoutToScreen();
  begin
  fViewLayout.btnFitToScreenClick(fViewLayout.btnFitToScreen);
  end; }

procedure TfrmLayoutElementSelectDialog.FormResize(Sender: TObject);
begin
    // FitLayoutToScreen();
end;

procedure TfrmLayoutElementSelectDialog.FormShow(Sender: TObject);
begin
    ChangeSelectionAfterManualChange();
end;

function TfrmLayoutElementSelectDialog.GetEvents: TLayoutElementSelectDlgEvents;
begin
    EXIT(fDialogEvents);
end;

function TfrmLayoutElementSelectDialog.GetSourceEvents: TLayoutElementSelectDlgEvents;
begin
    EXIT(fSourceDialogEvents);
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
    if (self.fMode = lsmRackDoublePos) or (self.fMode = lsmRackDoublePosExtended) or
        (self.fMode = lsmTwoRackDoublePosExtended) then
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
        if pnlSourceLayout.Visible then
        begin
            // Select Mode ändern
            xFirstCoordType := TMethodGUIParser.GetCoordType(self.edSourceRackFirstPos.Text);
            xLastCoordType := TMethodGUIParser.GetCoordType(self.edSourceRackLastPos.Text);
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
            xLastPos := TMethodGUIParser.GetLastPos(self.edSourceRackFirstPos.Text,
                self.edSourceRackLastPos.Text);
            self.edNoOfSourcePos.Text := IntToStr(xLastPos);

            // Rackposiitonen markieren
            xRack := fSourceLayoutManager.CurrentLayout.FindRackByName(self.cmbSourceRackName.Text);
            if (xRack is TLayoutElementSelectDlgRack) then
                (xRack as TLayoutElementSelectDlgRack).PaintAllPositions(self.edSourceRackFirstPos.Text,
                    self.edSourceRackLastPos.Text);
        end;
    end;

    if (self.fMode = lsmRackSinglePos) or (self.fMode = lsmRackSinglePosExtended) or
        (self.fMode = lsmTwoRackSinglePosExtended) then
    begin
        // Select Mode ändern
        self.rbInteger.Checked := true;

        // Rackposiitonen markieren
        xRack := fLayoutManager.CurrentLayout.FindRackByName(self.cmbRackName.Text);
        if (xRack is TLayoutElementSelectDlgRack) then
            (xRack as TLayoutElementSelectDlgRack).PaintAllPositions(self.edRackFirstPos.Text,
                self.edRackFirstPos.Text);
        if pnlSourceLayout.Visible then
        begin
            // Select Mode ändern
            self.rbInteger.Checked := true;

            // Rackposiitonen markieren
            xRack := fSourceLayoutManager.CurrentLayout.FindRackByName(self.cmbSourceRackName.Text);
            if (xRack is TLayoutElementSelectDlgRack) then
                (xRack as TLayoutElementSelectDlgRack).PaintAllPositions(self.edSourceRackFirstPos.Text,
                    self.edSourceRackFirstPos.Text);
        end;
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

function TfrmLayoutElementSelectDialog.DoOnChooseSourceRackFirstPos(const aRackName: string;
    aPosInt, aMaxRows: integer; const aParamIndexOffset: integer; aSetFocus: boolean): string;
begin
    result := '';
    if self.fMode in [lsmCarrier, lsmCarrierSlot] then
    begin
        ChooseCarrierSlotByRackName(aRackName);
    end
    else
    begin
        cmbSourceRackName.Text := aRackName;
        if (self.rbInteger.Checked) then
            result := IntToStr(aPosInt)
        else
            result := TMethodGUIParser.GetCoordString(aMaxRows, aPosInt);
        edSourceRackFirstPos.Text := result;
    end;
end;

function TfrmLayoutElementSelectDialog.DoOnChooseSourceRackLastPos(const aRackName: string;
    aPosInt, aMaxRows: integer; const aParamIndexOffset: integer; aSetFocus: boolean): string;
begin
    if self.fMode in [lsmCarrier, lsmCarrierSlot] then
    begin
        ChooseCarrierSlotByRackName(aRackName);
    end
    else
    begin
        cmbSourceRackName.Text := aRackName;
        if (self.rbInteger.Checked) then
            result := IntToStr(aPosInt)
        else
            result := TMethodGUIParser.GetCoordString(aMaxRows, aPosInt);
        edSourceRackLastPos.Text := result;
        self.edNoOfSourcePos.Text := IntToStr(TMethodGUIParser.GetLastPos(edSourceRackFirstPos.Text,
            edSourceRackLastPos.Text));
    end;
end;

procedure TfrmLayoutElementSelectDialog.SetCarrierMode(const aDialogCaption: string;
    aCarrierParam: TEditableParameter);
begin
    SetMode(lsmCarrier);
    if (aDialogCaption = '') then
        self.lblTitle.Caption := TLanguageString.Read('Select carrier:', 'Markiere Carrier:')
    else
        self.lblTitle.Caption := aDialogCaption;

    fCarrierParam := aCarrierParam;

    RefreshPageData;
end;

procedure TfrmLayoutElementSelectDialog.SetCarrierSlotMode(const aDialogCaption: string;
    aCarrierParam, aCarrierSlotParam: TEditableParameter);
begin
    SetMode(lsmCarrierSlot);
    if (aDialogCaption = '') then
        self.lblTitle.Caption := TLanguageString.Read('Select carrier slot', 'Markiere den Carrier-Slot')
    else
        self.lblTitle.Caption := aDialogCaption;

    fCarrierParam := aCarrierParam;
    fCarrierSlotParam := aCarrierSlotParam;

    RefreshPageData;
end;

procedure TfrmLayoutElementSelectDialog.SetRackDoublePosMode(const aDialogCaption: string;
    aRackParam, aFirstPosParam, aLastPosParam: TEditableParameter);
begin
    SetMode(lsmRackDoublePos);
    if (aDialogCaption = '') then
        self.lblTitle.Caption := TLanguageString.Read('Select rack positions', 'Markiere die Rack-Positionen')
    else
        self.lblTitle.Caption := aDialogCaption;

    fRackParam := aRackParam;
    fFirstPosParam := aFirstPosParam;
    fLastPosParam := aLastPosParam;

    RefreshPageData;
end;

procedure TfrmLayoutElementSelectDialog.SetRackDoublePosModeExtended(const aDialogCaption: string;
    aRackParam, aFirstPosParam, aLastPosParam, aSourceRackParam, aFirstSourcePosParam, aLastSourcePosParam,
    aVolume, aSubstance, aLiqParam: TEditableParameter);
begin
    if aSourceRackParam <> nil then
        SetMode(lsmTwoRackDoublePosExtended)
    else
        SetMode(lsmRackDoublePosExtended);
    if (aDialogCaption = '') then
        self.lblTitle.Caption := TLanguageString.Read('Select rack positions and Parameters',
            'Markiere die Rack-Positionen')
    else
        self.lblTitle.Caption := aDialogCaption;

    fRackParam := aRackParam;
    fFirstPosParam := aFirstPosParam;
    fLastPosParam := aLastPosParam;
    fVolumeParam := aVolume;
    fSubstanceParam := aSubstance;
    fLiqParamParam := aLiqParam;
    fSourceRackParam := aSourceRackParam;
    fFirstSourcePosParam := aFirstSourcePosParam;
    fLastSourcePosParam := aLastSourcePosParam;
    RefreshPageData;
end;

procedure TfrmLayoutElementSelectDialog.SetRackMode(const aDialogCaption: string;
    aRackParam: TEditableParameter);
begin
    SetMode(lsmRack);
    if (aDialogCaption = '') then
        self.lblTitle.Caption := TLanguageString.Read('Select rack', 'Markiere das Rack')
    else
        self.lblTitle.Caption := aDialogCaption;

    fRackParam := aRackParam;

    RefreshPageData;
end;

procedure TfrmLayoutElementSelectDialog.SetrackSinglePosMode(const aDialogCaption: string;
    aRackParam, aPosParam: TEditableParameter);
begin
    SetMode(lsmRackSinglePos);
    if (aDialogCaption = '') then
        self.lblTitle.Caption := TLanguageString.Read('Select rack position', 'Markiere die Rack-Position')
    else
        self.lblTitle.Caption := aDialogCaption;

    fRackParam := aRackParam;
    fFirstPosParam := aPosParam;

    RefreshPageData;
end;

procedure TfrmLayoutElementSelectDialog.SetRackSinglePosModeExtended(const aDialogCaption: string;
    aRackParam, aPosParam, aSourceRackParam, aSourcePosParam, aVolume, aSubstance,
    aLiqParam: TEditableParameter);
begin
    if aSourceRackParam <> nil then
        SetMode(lsmTwoRackSinglePosExtended)
    else
        SetMode(lsmRackSinglePosExtended);
    if (aDialogCaption = '') then
        self.lblTitle.Caption := TLanguageString.Read('Select rack position', 'Markiere die Rack-Position')
    else
        self.lblTitle.Caption := aDialogCaption;

    fRackParam := aRackParam;
    fFirstPosParam := aPosParam;
    fVolumeParam := aVolume;
    fSubstanceParam := aSubstance;
    fLiqParamParam := aLiqParam;
    fSourceRackParam := aSourceRackParam;
    fFirstSourcePosParam := aSourcePosParam;
    RefreshPageData;
end;

procedure TfrmLayoutElementSelectDialog.RefreshPageData;
begin
    if Assigned(fRackParam) then
        self.cmbRackName.Text := fRackParam.Value;

    if Assigned(fFirstPosParam) then
        self.edRackFirstPos.Text := fFirstPosParam.Value;

    if Assigned(fLastPosParam) then
        self.edRackLastPos.Text := fLastPosParam.Value;

    if Assigned(fCarrierParam) then
        self.cmbCarrierName.Text := fCarrierParam.Value;

    if Assigned(fCarrierSlotParam) then
        self.cmbCarrierSlot.Text := fCarrierSlotParam.Value;

    if Assigned(fVolumeParam) then
        self.edVolume.Text := fVolumeParam.Value
    else
    begin
        edVolume.Visible := false;
        lblVolume.Visible := false;
    end;
    if Assigned(fSubstanceParam) then
        self.cmbSubstance.Text := fSubstanceParam.Value
    else
    begin
        cmbSubstance.Visible := false;
        lblSubstance.Visible := false;
    end;
    if Assigned(fLiqParamParam) then
        self.cmbLiqParam.Text := fLiqParamParam.Value
    else
    begin
        cmbLiqParam.Visible := false;
        lblLiqParam.Visible := false;
    end;

    if not Assigned(fVolumeParam) then
        if not Assigned(fSubstanceParam) then
            if not Assigned(fLiqParamParam) then
                if PageControl1.Height > 80 then
                    PageControl1.Height := PageControl1.Height - 30;

    if not Assigned(fSourceRackParam) then
    begin
        lblRack.Top := 11;
        cmbRackName.Top := 7;
        lblPosition.Top := 11;
        edRackFirstPos.Top := 7;
        edRackLastPos.Top := 7;
        lblNoOfPos.Top := 11;
        edNoOfPos.Top := 7;
        btnRefresh.Top := 7;
        lblSource.Visible := false;
        cmbSourceRackName.Visible := false;
        lblSourcePosition.Visible := false;
        edSourceRackFirstPos.Visible := false;
        edSourceRackLastPos.Visible := false;
        lblNoOfSourcePos.Visible := false;
        edNoOfSourcePos.Visible := false;
        btnRefreshSource.Visible := false;
        lblVolume.Top := 40;
        edVolume.Top := 36;
        lblSubstance.Top := 40;
        cmbSubstance.Top := 36;
        lblLiqParam.Top := 40;
        cmbLiqParam.Top := 36;
        if PageControl1.Height > 30 then
            PageControl1.Height := PageControl1.Height - 30;
        pnlSourceLayout.Visible := false;
        fSourceLayoutManager.UnregisterCurrentLayout();
        fSourceLayoutManager.DestroyDefaultSceneGraphics();
        FreeAndNil(fSourceLayoutManager);
    end
    else
    begin
        self.cmbSourceRackName.Text := fSourceRackParam.Value;
        if Assigned(fFirstSourcePosParam) then
            self.edSourceRackFirstPos.Text := fFirstSourcePosParam.Value;
        if Assigned(fLastSourcePosParam) then
            self.edSourceRackLastPos.Text := fLastSourcePosParam.Value;
        pnlSourceLayout.Align := alLeft;
        pnlSourceLayout.Width := self.Width div 2;
        fViewSourceLayout.Align := alLeft;
        fViewSourceLayout.Width := self.Width div 2;
    end;
end;

function TfrmLayoutElementSelectDialog.WritePageData(aCheckBefore: boolean): boolean;
begin
    // Editierte Werte übernehmen
    if Assigned(fRackParam) then
        fRackParam.Value := self.cmbRackName.Text;
    if Assigned(fFirstPosParam) then
        fFirstPosParam.Value := self.edRackFirstPos.Text;
    if Assigned(fLastPosParam) then
        fLastPosParam.Value := self.edRackLastPos.Text;
    if Assigned(fSourceRackParam) then
        fSourceRackParam.Value := self.cmbSourceRackName.Text;
    if Assigned(fFirstSourcePosParam) then
        fFirstSourcePosParam.Value := self.edSourceRackFirstPos.Text;
    if Assigned(fLastSourcePosParam) then
        fLastSourcePosParam.Value := self.edSourceRackLastPos.Text;
    if Assigned(fCarrierParam) then
        fCarrierParam.Value := self.cmbCarrierName.Text;
    if Assigned(fCarrierSlotParam) then
        fCarrierSlotParam.Value := self.cmbCarrierSlot.Text;
    if Assigned(fVolumeParam) then
        fVolumeParam.Value := self.edVolume.Text;
    if Assigned(fSubstanceParam) then
        fSubstanceParam.Value := self.cmbSubstance.Text;
    if Assigned(fLiqParamParam) then
        fLiqParamParam.Value := self.cmbLiqParam.Text;

    // ResultValue prüfen
    if aCheckBefore and fCheckValues then
    begin
        try
            if Assigned(fRackParam) then
                fRackParam.CheckValue;
            if Assigned(fFirstPosParam) then
                fFirstPosParam.CheckValue;
            if Assigned(fLastPosParam) then
                fLastPosParam.CheckValue;
            if Assigned(fSourceRackParam) then
                fSourceRackParam.CheckValue;
            if Assigned(fFirstSourcePosParam) then
                fFirstSourcePosParam.CheckValue;
            if Assigned(fLastSourcePosParam) then
                fLastSourcePosParam.CheckValue;
            if Assigned(fCarrierParam) then
                fCarrierParam.CheckValue;
            if Assigned(fCarrierSlotParam) then
                fCarrierSlotParam.CheckValue;
            if Assigned(fVolumeParam) then
                fVolumeParam.CheckValue;
            if Assigned(fSubstanceParam) then
                fSubstanceParam.CheckValue;
            if Assigned(fLiqParamParam) then
                fLiqParamParam.CheckValue;
        except
            on E: ESetEditableParameterException do
            begin
                TDialogUtils.MessageBox(E.Message, 'Property is not valid', MB_ICONSTOP);
                EXIT(false);
            end
            else
                raise;
        end;
    end;

    EXIT(true);
end;


end.
