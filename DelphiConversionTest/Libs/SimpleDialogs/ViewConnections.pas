unit ViewConnections;
{ ------------------------------------------------------------------------------------------------------------
  Copyright  2009 Zinsser Analytic GmbH - All rights reserved.
  Author       : Thomas Schubert (ts)
  Description  :
  ------------------------------------------------------------------------------------------------------------
  Revision History:
  date     op  method                            track-no improvement/change
  -------- --  --------------------------------  -------- ----------------------------------------------------
  17.06.09 ts                                     TN4603   initial revision
  19.06.09 ts  SaveSettings                       TN4621   Speichern aller Connections nacheinander, ohne xNode.FindByText
  10.08.09 wl                                     TN4702   Strings werden jetzt direkt geladen
  11.08.09 wl  FormActivate                       TN4702   TStringArray statt TStringList
  03.09.09 wl  cxTreeList1                        TN4800   Anpassungen an Treelist Version 5 (nicht kompatibel)
  12.11.09 pk                                     TN4845   XPManifest Removed
  21.04.10 ts  SpeedButton1-/2Click               TN5068   funktioniert wieder
  20.05.10 wl                                     TN5117   neue Schriftart "Segoe UI", Schriftgröße 9
  07.06.10 ts  SpeedButton1-/2Click               TN5134   AV bei nur einer Connection -> Schleife geändert (for->while)
  09.06.10 wl                                     TN5116   ist kein DockableForm mehr
  17.06.10 wl                                     TN5116   uses geändert
  11.11.10 wl                                     TN5211   ist jetzt modal und unabhängig von Designer
  11.11.10 wl  ListView1DblClick                  TN5211   das Öffnen des Editors musste leider weggefallen
  ------------------------------------------------------------------------------------------------------------ }


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
    GeneralTypes,
    cxGraphics,
    cxCustomData,
    cxStyles,
    cxTL,
    cxTextEdit,
    cxControls,
    cxCheckBox,
    Buttons,
    ExtCtrls,
    cxLookAndFeels,
    cxLookAndFeelPainters,
    cxInplaceContainer,
    StdCtrls;

type
    TfrmViewConnections = class(TForm)
        cxTreeList1: TcxTreeList;
        TVcxTreeListColumn1: TcxTreeListColumn;
        TVcxTreeListColumn2: TcxTreeListColumn;
        pnlTop: TPanel;
        Panel1: TPanel;
        btnOK: TButton;
        btnCancel: TButton;
        btnSelectAll: TButton;
        btnDeselectAll: TButton;
        procedure FormDestroy(Sender: TObject);
        procedure FormActivate(Sender: TObject);
        procedure cxTreeList1Editing(Sender: TcxCustomTreeList; AColumn: TcxTreeListColumn;
            var Allow: Boolean);
        procedure btnSelectAllClick(Sender: TObject);
        procedure FormCreate(Sender: TObject);
        procedure btnDeselectAllClick(Sender: TObject);
        procedure btnOKClick(Sender: TObject);
    private const
        cColConnection = 0; // 'Connection';

    const
        cColEnabled = 1; // 'Enabled';
        class var uInstance: TfrmViewConnections;
        procedure SaveSettings();
    public
        class procedure CreateInstance(aOwner: TComponent);
        class property Instance: TfrmViewConnections read uInstance;
    end;


implementation


{$R *.dfm}

uses
    Connection,
    ModuleSettings,
    RunStepInfoFactory,
    ConnectionDataCache,
    ConnectionSettingsManager,
    DialogUtils,
    ControlUtils;

{ TfrmViewConnections }

procedure TfrmViewConnections.FormCreate(Sender: TObject);
begin
    TControlUtils.ResetFontForWinXP(self);
    btnSelectAll.Caption := TLanguageString.Read('Select All', 'Alles auswählen');
    btnDeselectAll.Caption := TLanguageString.Read('Deselect All', 'Auswahl entfernen');

    self.FormActivate(self);
end;

procedure TfrmViewConnections.FormDestroy(Sender: TObject);
begin
    uInstance := nil;
    inherited;
end;

procedure TfrmViewConnections.btnOKClick(Sender: TObject);
begin
    self.SaveSettings();
end;

class procedure TfrmViewConnections.CreateInstance(aOwner: TComponent);
begin
    if Assigned(uInstance) then
    begin
        uInstance.BringToFront;
        EXIT;
    end;

    uInstance := TfrmViewConnections.Create(aOwner);
end;

procedure TfrmViewConnections.FormActivate(Sender: TObject);
var
    xNode: TcxTreeListNode;
    i: integer;
    xModulType: string;
    xSettings: TModuleSettingList;
    xConnectionNames: TStringArray;
begin
    xConnectionNames := TConnectionsDataCache.Instance.ReadAllNames();
    cxTreeList1.Clear();
    Screen.Cursor := crHourglass;
    try
        for i := 0 to high(xConnectionNames) do
        begin
            xNode := cxTreeList1.Add;
            xNode.Texts[cColConnection] := xConnectionNames[i];
            xModulType := TConnectionSettingsManager.Instance.ReadModuleType(xConnectionNames[i]);
            xSettings := TConnectionSettingsManager.Instance.CreateModuleSettings(xConnectionNames[i],
                xModulType);
            xSettings.ReadAll();
            if xSettings.Find('Enabled').AsBool then
                xNode.Texts[cColEnabled] := 'TRUE'
            else
                xNode.Texts[cColEnabled] := 'FALSE'; // IntToStr( xCompilerMessage.LineIndex + 1 );
        end;
    finally
        Screen.Cursor := crDefault;
    end;
end;

procedure TfrmViewConnections.cxTreeList1Editing(Sender: TcxCustomTreeList; AColumn: TcxTreeListColumn;
    var Allow: Boolean);
begin
    if Assigned(cxTreeList1.FocusedNode) then
        cxTreeList1.FocusedNode.EndEdit(false);
end;

procedure TfrmViewConnections.btnSelectAllClick(Sender: TObject);
var
    xNode: TcxTreeListNode;
begin
    self.Activate;
    if Assigned(cxTreeList1.FocusedNode) then
        cxTreeList1.FocusedNode.EndEdit(false);
    xNode := cxTreeList1.TopVisibleNode;
    cxTreeList1.IsEditing;
    cxTreeList1.FocusedNode := xNode;
    while xNode <> nil do
    begin
        xNode.Texts[cColEnabled] := 'TRUE';
        xNode := xNode.GetNextVisible;
    end;
end;

procedure TfrmViewConnections.SaveSettings;
var
    xNode: TcxTreeListNode;
    xModulType: string;
    xSettings: TModuleSettingList;
    i: integer;
begin
    if Assigned(cxTreeList1.FocusedNode) then
        cxTreeList1.FocusedNode.EndEdit(false);
    xNode := cxTreeList1.TopNode;
    for i := 0 to cxTreeList1.Count - 1 do
    begin
        xModulType := TConnectionSettingsManager.Instance.ReadModuleType(xNode.Texts[cColConnection]);
        xSettings := TConnectionSettingsManager.Instance.CreateModuleSettings(xNode.Texts[cColConnection],
            xModulType);
        xSettings.ReadAll;
        if xNode.Texts[cColEnabled] = 'True' then
        begin
            xSettings.Settings[1].Value := 'YES';
        end
        else
        begin
            xSettings.Settings[1].Value := 'NO';
        end;
        if Assigned(xSettings) then
        begin
            xSettings.WriteAll();
            xSettings.WriteFromCache;
        end;
        xNode := xNode.GetNext;
    end;
end;

procedure TfrmViewConnections.btnDeselectAllClick(Sender: TObject);
var
    xNode: TcxTreeListNode;
begin
    if Assigned(cxTreeList1.FocusedNode) then
        cxTreeList1.FocusedNode.EndEdit(false);
    xNode := cxTreeList1.TopVisibleNode;
    cxTreeList1.FocusedNode := xNode;
    while xNode <> nil do
    begin
        xNode.Texts[cColEnabled] := 'FALSE';
        xNode := xNode.GetNextVisible;
    end;
end;


end.
