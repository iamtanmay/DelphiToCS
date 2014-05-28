unit ViewDisplayComponentPreview;
{ -----------------------------------------------------------------------------------------------------------------------
  Copyright © 2009 Zinsser Analytic GmbH - All rights reserved.
  Author       : Payman Kamali (pk)
  Description  :
  -----------------------------------------------------------------------------------------------------------------------
  Revision History:
  date     op  method                       track-no improvement/change
  -------- --  ---------------------------  -------- --------------------------------------------------------------------
  06.04.09 pk                                TN4503  Initial Revision
  07.04.09 pk                                TN4503  FormDestroy, FormCreate events were not linked
  10.08.09 wl                                TN4702   Strings werden jetzt direkt geladen
  21.08.09 wl                                TN4702   Strings werden jetzt direkt geladen
  27.08.09 pk  fDisplayComponentManager      TN4753  New
  18.03.10 ts                                TN5033  ClearDisplayComponent von Destroy->FormDestroy, sonst AV, da auf fDisplayComponentManager zugegriffen wird
  20.05.10 wl                                TN5117   neue Schriftart "Segoe UI", Schriftgröße 9
  09.06.10 wl                                TN5116   ist jetzt kein DockableForm mehr, sondern Bestandteil von MainDisplayDevelopment
  30.06.11 wl                                TN5620   Hintergrund jetzt weiß
  23.11.12 wl                                TN6015.1 Es gibt keine Instanz mehr von TDisplayComponentSettingsManager
  ----------------------------------------------------------------------------------------------------------------------- }


interface


uses
    Classes,
    Controls,
    Forms,
    ExtCtrls,
    DisplayComponentIntf,
    DisplayComponentManager;

type
    TfrmViewDisplayComponentPreview = class(TForm)
        pnlMain: TPanel;
        procedure FormCreate(Sender: TObject);
        procedure FormDestroy(Sender: TObject);
    private
        fDisplayComponent: IDisplayComponent;
        fDisplayComponentManager: TDisplayComponentManager;
        class var uInstance: TfrmViewDisplayComponentPreview;
        procedure ShowComponent(const aDisplayComponentName: string);
        procedure ClearDisplayComponent;
    public
        destructor Destroy(); override;
        class procedure CreateInstance(aOwner: TComponent);
        class procedure InstanceShowComponent(const aDisplayComponentName: string);
        class procedure DestoryInstance();
        class property Instance: TfrmViewDisplayComponentPreview read uInstance;
    end;


implementation


{$R *.dfm}

uses
    Generics.Collections,
    GeneralTypes,
    ControlUtils;

class procedure TfrmViewDisplayComponentPreview.CreateInstance(aOwner: TComponent);
begin
    if Assigned(uInstance) then
    begin
        uInstance.BringToFront;
        EXIT;
    end;

    uInstance := TfrmViewDisplayComponentPreview.Create(aOwner);
end;

procedure TfrmViewDisplayComponentPreview.FormCreate(Sender: TObject);
begin
    inherited;

    TControlUtils.ResetFontForWinXP(self);
    fDisplayComponentManager := TDisplayComponentManager.Create();
    self.Caption := TLanguageString.Read('Display Component Preview', 'Display-Komponente Vorschau');
end;

procedure TfrmViewDisplayComponentPreview.FormDestroy(Sender: TObject);
begin
    ClearDisplayComponent();
    fDisplayComponentManager.Free;
    uInstance := nil;
    inherited;
end;

procedure TfrmViewDisplayComponentPreview.ClearDisplayComponent();
begin
    fDisplayComponentManager.ClearDisplayComponents();
    if Assigned(fDisplayComponent) then
        fDisplayComponent.UnLoad();

    fDisplayComponent := nil; // free previous component

end;

class procedure TfrmViewDisplayComponentPreview.InstanceShowComponent(const aDisplayComponentName: string);
begin
    if not Assigned(uInstance) then
        EXIT;
    uInstance.ShowComponent(aDisplayComponentName);
end;

procedure TfrmViewDisplayComponentPreview.ShowComponent(const aDisplayComponentName: string);
var
    xErrors: TList<string>;
begin
    ClearDisplayComponent();

    xErrors := TList<string>.Create();
    try
        fDisplayComponent := fDisplayComponentManager.LoadComponentToWindow(aDisplayComponentName, '', true,
            pnlMain, xErrors);
    finally
        xErrors.Free;
    end;

    self.Show();
end;

class procedure TfrmViewDisplayComponentPreview.DestoryInstance;
begin
    uInstance.Free;
end;

destructor TfrmViewDisplayComponentPreview.Destroy;
begin
    inherited;
end;


end.
