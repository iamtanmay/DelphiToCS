unit ViewLayout;
{ -----------------------------------------------------------------------------------------------------------------------
  Copyright © 2009 Zinsser Analytic GmbH - All rights reserved.
  Author       : Payman Kamali (pk)
  Description  :
  -----------------------------------------------------------------------------------------------------------------------
  Revision History:
  date     op  method                                track-no  improvement/change
  -------- --  ------------------------------------  --------- ----------------------------------------------------------
  24.11.08 pk                                        TN4532    New
  27.07.09 pk                                        TN4604    Now DockClient
  28.07.09 wl                                        TN4604    Form-Variable entfernt
  02.09.09 pk  SetSceneGraphics                      TN4753    if value nil then Exit
  20.05.10 wl                                        TN5117   neue Schriftart "Segoe UI", Schriftgröße 9
  21.07.10 pk                                        TN5183    ZoomFactor Combo box no longer has a tabstop
  23.04.12 ts                                        TN5860    Zoom Funktionalität überarbeitet
  08.05.12 ts                                        TN5860    Zoombar align client
  04.06.12 ts                                        TN5907    Max height for zoombar
  ----------------------------------------------------------------------------------------------------------------------- }


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
    ExtCtrls,
    Buttons,
    StdCtrls,
    SceneGraphics,
    IntfSceneGraphics,
    ComCtrls;

type
    TfrmViewLayout = class(TForm)
        pnlNavigation: TPanel;
        pnlDraw: TPanel;
        btnFitToScreen: TSpeedButton;
        tbZoom: TTrackBar;
        sbZoomOut: TSpeedButton;
        sbZoomIn: TSpeedButton;
        procedure btnFitToScreenClick(Sender: TObject);
        procedure FormCreate(Sender: TObject);
        procedure tbZoomChange(Sender: TObject);
        procedure sbZoomOutClick(Sender: TObject);
        procedure sbZoomInClick(Sender: TObject);
    private
        fSceneGraphics: TSceneGraphics;
        fZoomBarPosition: integer;
        fFitToScreen: boolean;
        function GetDrawPanel: TPanel;
        procedure ChangeCameraMode(const aMode: TSceneGraphicsCameraMode);
        procedure SetSceneGraphics(const aValue: TSceneGraphics);
        procedure SetZoomFactorEditFromGraphics;
    public
        property SceneGraphics: TSceneGraphics read fSceneGraphics write SetSceneGraphics;
        property DrawPanel: TPanel read GetDrawPanel;
    end;


implementation


uses
    Math,
    AppTypes,
    ControlUtils,
    AppSettings,
    CommonTypes,
    MatrixMath;

{$R *.dfm}
{ TfrmViewLayout }

function TfrmViewLayout.GetDrawPanel: TPanel;
begin
    result := self.pnlDraw;
end;

procedure TfrmViewLayout.ChangeCameraMode(const aMode: TSceneGraphicsCameraMode);
begin
    fSceneGraphics.SetCameraMode(aMode);
end;

procedure TfrmViewLayout.FormCreate(Sender: TObject);
var
    xIniAccess: IWinLissyIniAccess;
begin
    TControlUtils.ResetFontForWinXP(self);

    xIniAccess := gCommonDll.CreateAppIni;
    fZoomBarPosition := xIniAccess.ReadInteger('Display', 'LayoutZoomBarPosition');
    if fZoomBarPosition = 0 then
    begin
        self.pnlNavigation.Align := alTop;
        self.pnlNavigation.Height := 35;
        self.btnFitToScreen.Top := 1;
        self.btnFitToScreen.Left := 270;
        self.sbZoomIn.Top := 1;
        self.sbZoomIn.Left := 235;
        self.sbZoomOut.Top := 1;
        self.sbZoomOut.Left := 5;
        self.tbZoom.Orientation := trHorizontal;
        self.tbZoom.Left := 35;
        self.tbZoom.Top := 4;
    end
    else
    begin
        if fZoomBarPosition = 1 then
            self.pnlNavigation.Align := alLeft;
        if fZoomBarPosition = 2 then
            self.pnlNavigation.Align := alRight;
        self.pnlNavigation.Width := 32;
        self.btnFitToScreen.Align := alTop;
        self.btnFitToScreen.Left := 1;
        self.sbZoomIn.Align := alTop;
        self.pnlNavigation.Constraints.MaxHeight := 300;
        self.sbZoomIn.Left := 1;
        self.sbZoomOut.Align := alBottom;
        self.sbZoomOut.Left := 1;
        self.tbZoom.Orientation := trVertical;
        self.tbZoom.Align := alClient;
    end;
    fFitToScreen := false;
end;

procedure TfrmViewLayout.btnFitToScreenClick(Sender: TObject);
begin
    fFitToScreen := true;
    fSceneGraphics.FitToScreen();
    SetZoomFactorEditFromGraphics();
    fFitToScreen := false;
end;

procedure TfrmViewLayout.sbZoomInClick(Sender: TObject);
begin
    if fZoomBarPosition = 0 then
        self.tbZoom.Position := self.tbZoom.Position + 5
    else
        self.tbZoom.Position := self.tbZoom.Position - 5;
end;

procedure TfrmViewLayout.sbZoomOutClick(Sender: TObject);
begin
    if fZoomBarPosition = 0 then
        self.tbZoom.Position := self.tbZoom.Position - 5
    else
        self.tbZoom.Position := self.tbZoom.Position + 5;
end;

procedure TfrmViewLayout.SetZoomFactorEditFromGraphics();
begin
    if fZoomBarPosition = 0 then
        self.tbZoom.Position := Round(fSceneGraphics.Zoom * 100)
    else
        self.tbZoom.Position := self.tbZoom.Max + self.tbZoom.Min - Round(fSceneGraphics.Zoom * 100);
end;

procedure TfrmViewLayout.tbZoomChange(Sender: TObject);
begin
    if fFitToScreen then
        EXIT;

    if fZoomBarPosition = 0 then
        fSceneGraphics.Zoom := self.tbZoom.Position / 100
    else
        fSceneGraphics.Zoom := (self.tbZoom.Max + self.tbZoom.Min - self.tbZoom.Position) / 100;

    fSceneGraphics.SceneChanged;
end;

procedure TfrmViewLayout.SetSceneGraphics(const aValue: TSceneGraphics);
begin
    fSceneGraphics := aValue;
    if aValue = nil then
        EXIT;

    SetZoomFactorEditFromGraphics;
    ChangeCameraMode(scmNone);
end;


end.
