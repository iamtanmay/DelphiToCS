{ -----------------------------------------------------------------------------------------------------------------------
  Copyright © 2009 Zinsser Analytic GmbH - All rights reserved.
  Author       : Wolfgang Lyncke (wl), Payman Kamali (pk)
  Description  :
  -----------------------------------------------------------------------------------------------------------------------
  Revision History:
  date     op  method                       track-no improvement/change
  -------- --  ---------------------------  -------- --------------------------------------------------------------------
  06.04.09 pk                                TN4503  Initial Revision - Code from ZARunnerMain
  09.06.09 pk  TSysLiquidIsOutErrorInfo      TN4585.1 from ErrorInfosExt
  20.08.09 wl                                TN4702   Strings werden jetzt direkt geladen
  28.08.09 pk                                TN4753   uses Liquids
  08.09.09 pk                                TN4753   code disabled for now
  12.10.09 pk                                TN4812   AdditionalInfo is no longer normal TStringList class
  04.02.10 pk                                TN4972   Changes for Restart
  20.05.10 wl                                TN5117   neue Schriftart "Segoe UI", Schriftgröße 9
  18.06.12 wl                                TN5899   alle Funktionen wieder implementiert
  05.11.12 wl  ChangeSimulationMode          TN6006   alle Buttons werden im Simulationsmodus disabled
  24.04.13 wl                                TN6137   TLiquids.Instance statt gLiquids
  ---------------------------------------------------------------------------------------------------------------------- }

unit VolumesInfo;


interface


uses
    Windows,
    Forms,
    Dialogs,
    StdCtrls,
    ExtCtrls,
    Controls,
    Classes;

type
    TfrmVolumesInfo = class(TForm)
        pnVolumes: TPanel;
        lblW: TLabel;
        Label1: TLabel;
        Bevel1: TBevel;
        Label3: TLabel;
        Label4: TLabel;
        lblWasteVol: TStaticText;
        lblMaxWaste: TStaticText;
        btnEmpty: TButton;
        procedure btnEmptyClick(Sender: TObject);
        procedure FormCreate(Sender: TObject);
    private
        procedure CalcLeftAndTop(aIndex: integer; out oTop, oLeft: integer);
        procedure PlaceBevel(aIndex: integer);
        procedure FillClick(Sender: TObject);
        procedure CreateLiquidLabels();
    public
        procedure InitVolumesInfo();
        procedure ChangeSysVol(Sender: TObject);
        procedure ChangeWasteVol(Sender: TObject);
        procedure ChangeSimulationMode(aIsSimulated: boolean);
    end;


implementation


{$R *.dfm}

uses
    Graphics,
    SysUtils,
    CommonTypes,
    AppTypes,
    Liquids,
    SamGlobe,
    AppSettings,
    ControlUtils,
    GUIManager,
    GeneralTypes;

{ TfrmVolumesInfo }

procedure TfrmVolumesInfo.CalcLeftAndTop(aIndex: integer; out oTop, oLeft: integer);
begin
    oLeft := ((aIndex + 2) div 8) * 210;
    oTop := ((aIndex + 2) mod 8) * 24 + 3;
end;

procedure TfrmVolumesInfo.PlaceBevel(aIndex: integer);
var
    xLeft, xTop: integer;
begin
    self.CalcLeftAndTop(aIndex, xTop, xLeft);
    Bevel1.Left := xLeft;
    Bevel1.Top := xTop;
end;

procedure TfrmVolumesInfo.ChangeWasteVol(Sender: TObject);
begin
    // ------------------------------------------------------------------------- Verändern des Labels
    lblWasteVol.Caption := IntToStr(TLiquids.Instance.WasteLiquid.Volume_mL);
    lblWasteVol.Color := clWhite;
    //
    if (TLiquids.Instance.WasteLiquid.Volume_mL > Round(TLiquids.Instance.WasteLiquid.MaxVolume_mL *
        0.9)) then
        self.lblWasteVol.Color := clYellow;
    //
    if (TLiquids.Instance.WasteLiquid.Volume_mL > TLiquids.Instance.WasteLiquid.MaxVolume_mL) then
        self.lblWasteVol.Color := clRed;
end;

procedure TfrmVolumesInfo.CreateLiquidLabels();
begin
end;

procedure TfrmVolumesInfo.FormCreate(Sender: TObject);
var
    NewLabel1, NewLabel2, NewLabel3: TStaticText;
    NewButton: TButton;
    x, xLeft, xTop: integer;
    xLiquid: TSystemLiquid;
begin
    TControlUtils.ResetFontForWinXP(self);

    self.btnEmpty.Caption := TLanguageString.Read('Empty', 'Leeren');
    self.lblW.Caption := TLanguageString.Read('Waste:', 'Abfall:');

    if not gCommonDll.CurrentUser.HasLevel(usrSystem) then
        btnEmpty.Visible := false;

    for x := 0 to TLiquids.Instance.SystemLiquids.Count - 1 do
    begin
        xLiquid := TLiquids.Instance.SystemLiquids[x];

        CalcLeftAndTop(x, xTop, xLeft);
        // ------------------------------------- Diluent-Label
        NewLabel1 := TStaticText.Create(self);
        NewLabel1.Parent := pnVolumes;
        NewLabel1.Name := 'lblSysDil' + IntToStr(x);
        NewLabel1.Caption := xLiquid.Diluent + ':';
        NewLabel1.Left := xLeft + 5;
        NewLabel1.Top := xTop + 4;
        // -------------------------------------- Volumen-Label
        NewLabel2 := TStaticText.Create(self);
        NewLabel2.Parent := pnVolumes;
        NewLabel2.Name := 'lblSysVol' + xLiquid.PortName;
        NewLabel2.Color := clWhite;
        NewLabel2.SetBounds(xLeft + 70, xTop + 4, 50, 13);
        NewLabel2.AutoSize := false;
        NewLabel2.Transparent := false;
        NewLabel2.Alignment := taRightJustify;
        self.ChangeSysVol(xLiquid);
        // ------------------------------------- mL-Label
        NewLabel3 := TStaticText.Create(self);
        NewLabel3.Parent := pnVolumes;
        NewLabel3.Caption := 'mL';
        NewLabel3.Left := xLeft + 125;
        NewLabel3.Top := xTop + 3;
        // ------------------------------------------- Fill-Button
        if gCommonDll.CurrentUser.HasLevel(usrSystem) then
        begin
            NewButton := TButton.Create(self);
            NewButton.Parent := pnVolumes;
            NewButton.Name := 'btnFill' + IntToStr(x);
            NewButton.Caption := TLanguageString.Read('Fill', 'Füllen');
            NewButton.SetBounds(xLeft + 145, xTop, 60, 20);
            NewButton.OnClick := FillClick;
        end;
        // -------------------------------------------------------------------- Waste
        self.ChangeWasteVol(TLiquids.Instance.WasteLiquid);
        lblMaxWaste.Caption := IntToStr(TLiquids.Instance.WasteLiquid.MaxVolume_mL);
    end;
end;

procedure TfrmVolumesInfo.ChangeSimulationMode(aIsSimulated: boolean);
var
    x: integer;
begin
    for x := 0 to self.ComponentCount - 1 do
    begin
        if (self.Components[x] is TButton) then
            (self.Components[x] as TButton).Enabled := not aIsSimulated;
    end;
end;

procedure TfrmVolumesInfo.ChangeSysVol(Sender: TObject);
var
    xVolLbl: TStaticText;
    xLiquid: TSystemLiquid;
begin
    if not(Sender is TSystemLiquid) then
        EXIT;

    xLiquid := Sender as TSystemLiquid;
    self.PlaceBevel(xLiquid.DilIndex);

    // Verändern des Labels
    xVolLbl := TStaticText(self.FindComponent('lblSysVol' + xLiquid.PortName));
    xVolLbl.Caption := IntToStr(xLiquid.Volume_mL);
    xVolLbl.Color := clWhite;

    if (xLiquid.Volume_mL < xLiquid.MinVolume_mL) then
        xVollbl.Color := clYellow;
    if xLiquid.Volume_mL < 0 then
        xVollbl.Color := clRed;
end;

procedure TfrmVolumesInfo.FillClick(Sender: TObject);
var
    xBtnFill: TButton;
    xUseStr: string;
    i, xVolML: integer;
begin
    if not TryStrToInt(gGUIManager.InputBox(TLanguageString.Read('What is this liquid volume (in ml)?',
        'Wie groß ist das Systemflüssigkeits-Volumen [in ml]?'), '', ''), xVolML) then
        EXIT;

    xBtnFill := TButton(Sender);
    xUseStr := xBtnFill.Name;
    Delete(xUseStr, 1, 7);
    i := StrToInt(xUseStr);
    TLiquids.Instance.SystemLiquids[i].ChangeVolumeAbsolute(1000 * xVolML, false);
end;

procedure TfrmVolumesInfo.btnEmptyClick(Sender: TObject);
begin
    if gGUIManager.MessageBox(TLanguageString.Read('Waste Container empty?',
        'Ist der Abfallbehälter geleert?'), TLanguageString.Read('Waste', 'Abfall'),
        MB_ICONWARNING + MB_YESNO + MB_DEFBUTTON2) = IDYES then
        TLiquids.Instance.WasteLiquid.ChangeVolumeAbsolute(0, false);
end;

procedure TfrmVolumesInfo.InitVolumesInfo();
begin
    CreateLiquidLabels();
end;


end.
