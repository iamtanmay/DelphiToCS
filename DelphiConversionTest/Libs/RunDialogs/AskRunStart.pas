unit AskRunStart;
{ ----------------------------------------------------------------------------------------------------------------------
  Copyright © 2009 Zinsser Analytic GmbH - All rights reserved.
  Author       : Payman Kamali (pk)
  Description  :
  ----------------------------------------------------------------------------------------------------------------------
  Revision History:
  date     op  method                       track-no improvement/change
  -------- --  ---------------------------  -------- -------------------------------------------------------------------
  08.04.09 pk                                TN4503  Initial Revision
  08.04.09 pk                                TN4503  bug fixes, resources moved here
  20.08.09 wl                                TN4702   Strings werden jetzt direkt geladen
  15.04.10 ts  FormShow                      TN5057  new: btnNext will be focused, the user can start methods with keyboard (enter)
  20.05.10 wl                                TN5117   neue Schriftart "Segoe UI", Schriftgröße 9
  21.06.10 wl                                TN5160   Position = poScreenCenter
  21.03.11 wl  cbSimInputWeight              TN5508  Fensterelemente, die nur bei Simulation erscheinen
  14.04.11 wl  lblSimSpeed                   TN5554    SimulationSpeed_Percent wird jetzt auch in Prozent gezeigt
  27.04.11 wl  gbSimulation                  TN5554   neu
  ---------------------------------------------------------------------------------------------------------------------- }


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
    ComCtrls;

type
    TPageMoveDirection = (pmdNone, pmdForward, pmdBackward);

    TfrmAskRunStart = class(TForm)
        Panel1: TPanel;
        lblTitle: TLabel;
        edMethodName: TEdit;
        Panel3: TPanel;
        PageControl1: TPageControl;
        TabSheet1: TTabSheet;
        chkSimulation: TCheckBox;
        TabSheet2: TTabSheet;
        pnlBottom: TPanel;
        Shape2: TShape;
        btnCancel: TButton;
        btnNext: TButton;
        btnBack: TButton;
        Shape1: TShape;
        edSimulationCaption: TEdit;
        mmoWarnings: TMemo;
        gbSimulation: TGroupBox;
        lblSimSpeed: TLabel;
        trbSimulation: TTrackBar;
        cbSimInputWeight: TCheckBox;
        procedure FormCreate(Sender: TObject);
        procedure btnBackClick(Sender: TObject);
        procedure chkSimulationClick(Sender: TObject);
        procedure btnNextClick(Sender: TObject);
        procedure FormShow(Sender: TObject);
        procedure trbSimulationChange(Sender: TObject);
    private
        fAskRacksPlaced: boolean;
        fIsSimChangeAllowed: boolean;
        function GetIsSimulation: boolean;
        procedure GotoPage(aIndex: integer; aPageDirection: TPageMoveDirection);
        procedure SetIsSimulation(const aValue: boolean);
        procedure GotoNextPage;
        function AnyOptionsEnabled: boolean;
        procedure RefreshWarningLines();
        procedure AddWarningLine(const aText: string);
        function GetSimulationAskWeight: boolean;
        function GetSimulationSpeed_Percent: integer;
    public
        constructor Create(const aMethodName: string; const aIsSimChangeAllowed: boolean;
            const aIsSim: boolean; aSimulationAskWeight: boolean; aSimulationSpeed_Percent: integer);
            reintroduce;

        class function InstanceShowModal(const aMethodName: string; const aIsSimChangeAllowed: boolean;
            var vIsSim: boolean; var vSimulationAskWeight: boolean; var vSimulationSpeed_Percent: integer)
            : TModalResult;

        property IsSimulation: boolean read GetIsSimulation write SetIsSimulation;
        property SimulationSpeed_Percent: integer read GetSimulationSpeed_Percent;
        property SimulationAskWeight: boolean read GetSimulationAskWeight;
    end;


implementation


{$R *.dfm}

uses
    GeneralTypes,
    ControlUtils;

{ TfrmAskRunStart }

constructor TfrmAskRunStart.Create(const aMethodName: string; const aIsSimChangeAllowed, aIsSim: boolean;
    aSimulationAskWeight: boolean; aSimulationSpeed_Percent: integer);
begin
    inherited Create(nil);

    self.edMethodName.Text := aMethodName;
    self.IsSimulation := aIsSim;
    fIsSimChangeAllowed := aIsSimChangeAllowed;
    trbSimulation.Hint := TLanguageString.Read('Length of the wait time during simulation',
        'Länge der Wartezeiten während der Simulation');
    cbSimInputWeight.Checked := aSimulationAskWeight;
    trbSimulation.Position := aSimulationSpeed_Percent;
end;

function TfrmAskRunStart.AnyOptionsEnabled(): boolean;
begin
    result := self.chkSimulation.Enabled;
end;

procedure TfrmAskRunStart.GotoPage(aIndex: integer; aPageDirection: TPageMoveDirection);
var
    xIndex: integer;
begin
    xIndex := aIndex;

    if xIndex < 0 then
        xIndex := 0;
    if xIndex > self.PageControl1.PageCount - 1 then
        xIndex := self.PageControl1.PageCount - 1;

    self.PageControl1.ActivePageIndex := xIndex;

    if PageControl1.ActivePageIndex = (PageControl1.PageCount - 1) then
        self.btnNext.Caption := TLanguageString.Read('&OK', '&OK')
    else
        self.btnNext.Caption := TLanguageString.Read('&Next', '&Weiter') + ' >';

    self.btnBack.Enabled := PageControl1.ActivePageIndex > 0;
end;

procedure TfrmAskRunStart.GotoNextPage();
begin
    GotoPage(PageControl1.ActivePageIndex + 1, pmdForward);
end;

procedure TfrmAskRunStart.btnNextClick(Sender: TObject);
begin
    if PageControl1.ActivePageIndex = (PageControl1.PageCount - 1) then
    begin
        self.ModalResult := mrOK;
        EXIT;
    end;
    GotoNextPage();
end;

procedure TfrmAskRunStart.btnBackClick(Sender: TObject);
begin
    GotoPage(PageControl1.ActivePageIndex - 1, pmdBackward);
end;

class function TfrmAskRunStart.InstanceShowModal(const aMethodName: string;
    const aIsSimChangeAllowed: boolean; var vIsSim: boolean; var vSimulationAskWeight: boolean;
    var vSimulationSpeed_Percent: integer): TModalResult;
var
    xForm: TfrmAskRunStart;
begin
    xForm := TfrmAskRunStart.Create(aMethodName, aIsSimChangeAllowed, vIsSim, vSimulationAskWeight,
        vSimulationSpeed_Percent);
    try
        result := xForm.ShowModal;
        vIsSim := xForm.IsSimulation;
        vSimulationAskWeight := xForm.SimulationAskWeight;
        vSimulationSpeed_Percent := xForm.SimulationSpeed_Percent;
    finally
        xForm.Free;
    end;
end;

function TfrmAskRunStart.GetIsSimulation: boolean;
begin
    result := self.chkSimulation.Checked;
end;

function TfrmAskRunStart.GetSimulationAskWeight: boolean;
begin
    result := cbSimInputWeight.Checked;
end;

function TfrmAskRunStart.GetSimulationSpeed_Percent: integer;
begin
    result := trbSimulation.Position;
end;

procedure TfrmAskRunStart.SetIsSimulation(const aValue: boolean);
begin
    self.chkSimulation.Checked := aValue;
    self.edSimulationCaption.Visible := aValue;
    self.gbSimulation.Visible := aValue;

    // when simulation dont need to ask if all racks are placed correctly
    fAskRacksPlaced := not aValue;
    RefreshWarningLines();
end;

procedure TfrmAskRunStart.trbSimulationChange(Sender: TObject);
begin
    lblSimSpeed.Caption := 'Delay length in simulation: ' + IntToStr(self.trbSimulation.Position) + '%';
end;

procedure TfrmAskRunStart.FormCreate(Sender: TObject);
// Hide PageControl Tabs
var
    x: integer;
begin
    TControlUtils.ResetFontForWinXP(self);

    if not fIsSimChangeAllowed then
        chkSimulation.Enabled := false;

    if not AnyOptionsEnabled() then
    begin
        self.TabSheet1.PageControl := nil;
    end;

    if PageControl1.PageCount <= 1 then
    begin
        self.btnBack.Visible := false;
    end;

    self.btnBack.Caption := '< ' + TLanguageString.Read('&Back', '&Zurück');

    self.lblTitle.Caption := TLanguageString.Read('Start Method:', 'Methode Starten:');
    self.btnCancel.Caption := TLanguageString.Read('&Cancel', '&Abbrechen');

    self.Caption := TLanguageString.Read('Start', 'Starten');

    for x := 0 to PageControl1.PageCount - 1 do
    begin
        PageControl1.Pages[x].TabVisible := false;
    end;

    // select the first tab
    GotoPage(0, pmdNone);

    trbSimulationChange(self);
end;

procedure TfrmAskRunStart.FormShow(Sender: TObject);
begin
    self.btnNext.SetFocus;
end;

procedure TfrmAskRunStart.chkSimulationClick(Sender: TObject);
begin
    self.IsSimulation := chkSimulation.Checked;
end;

procedure TfrmAskRunStart.AddWarningLine(const aText: string);
var
    xText: string;
begin
    xText := Format('- %s', [aText]);
    mmoWarnings.Lines.Add(xText);
end;

procedure TfrmAskRunStart.RefreshWarningLines;
var
    xText: string;
begin
    mmoWarnings.Lines.Clear();

    if fAskRacksPlaced then
    begin
        xText := TLanguageString.Read('Are all racks placed correctly?', 'Alle Racks richtig plaziert?');
        AddWarningLine(xText);
    end;

    xText := TLanguageString.Read('Start Method?', 'Methode Starten?');
    AddWarningLine(xText);
end;


end.
