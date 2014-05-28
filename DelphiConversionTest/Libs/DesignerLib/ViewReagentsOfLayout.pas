{ --------------------------------------------------------------------------------------------------
  Copyright © 2004 Zinsser Analytic GmbH - All rights reserved.
  Project      : New Editor
  Author       : Wolfgang Lyncke (wl)
  Description  : Reagent list for sequence editing
  --------------------------------------------------------------------------------------------------
  Revision History:
  date     op  method                       track-no  improvement/change
  -------- --  ---------------------------  --------  ----------------------------------------------
  17.08.04 wl                               TN1958.1  initial version  (separated from SeqEdit.pas)
  17.08.04 wl                               TN2008.2  jetzt als singleton
  24.08.04 wl                               TN2008.2  mit Laden und Speichern der DockableForms
  24.09.04 wl                               TN2008.2  Laden und Speichern verbessert
  06.05.05 wl                               TN2398  use TPosinfoDataAdaptor instead of DMRack.Posinfo
  03.09.05 wl                               TN2541.4  umbenannt in ViewReagentsOfLayout (vorher: EdReagentList)
  12.10.05 wl                               TN2541.4  kann jetzt mit Drag & Drop bedient werden
  04.03.06 wl                               TN2541.4  Reagenzien aus ReagentList werden hier nicht mehr angezeigt
  20.03.07 pk                               TN3632    First reagent could not be double clicked
  20.03.07 pk                               TN3637    The lvReagents READONLY property is now set to true.
  20.06.08 pk                               TN4139    WB global object replaced by LayoutManager
  14.12.09 pk  DoUpdate                     TN4940    check IsCurrentLayoutEmpty
  14.12.09 pk                               TN4940    use TDataProvider instead of TQuery
  20.05.10 wl                               TN5117   neue Schriftart "Segoe UI", Schriftgröße 9
  17.06.10 pk                               TN5152.1  uses DataProviderFactory
  21.06.10 wl                               TN5160   Position = poScreenCenter
  27.06.11 wl                               TN5611   funktioniert jetzt auch mit TurboDB
  26.07.11 wl                                     TN5614   ist kein TFullDockableForm mehr, Instance entfernt
  -------------------------------------------------------------------------------------------------- }

unit ViewReagentsOfLayout;


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
    ComCtrls,
    StdCtrls,
    ExtCtrls;

type
    TfrmReagentsOfLayout = class(TForm)
        rgAll: TRadioGroup;
        lvReagents: TListView;
        procedure rgAllClick(Sender: TObject);
        procedure lvReagentsDblClick(Sender: TObject);
        procedure FormCreate(Sender: TObject);
        procedure FormClose(Sender: TObject; var Action: TCloseAction);
    private
        procedure DoUpdate;
        class procedure AddReagentsOfAllLayouts(aReagentNames: TListItems);
        class procedure AddReagentsOfLayout(aReagentNames: TListItems; aLayoutName: string);
        //
        function GetCurrentReagent: string;
    public
        procedure UpdateReagents;
        property CurrentReagent: string read GetCurrentReagent;
    end;


implementation


uses
    ZADesignLayoutManager,
    ZADesignMain,
    PosinfoDataAdaptor,
    CommonTypes,
    AppSettings,
    DataProvider,
    DataProviderFactory,
    ControlUtils;

{$R *.dfm}

class procedure TfrmReagentsOfLayout.AddReagentsOfAllLayouts(aReagentNames: TListItems);
var
    xQuery: TDataProvider;
    xItem: TListItem;
begin
    xQuery := TDataProviderFactory.Instance.CreateDataProvider();
    try
        xQuery.SelectAndOpen
            ('SELECT DISTINCT SUBSTID FROM POSINFO WHERE (STEP = 0) AND (SUBSTID <> '''') ORDER BY SUBSTID',
            true);

        while not xQuery.Eof do
        begin
            xItem := aReagentNames.Add;
            xItem.Caption := xQuery.FieldbyName('SUBSTID').ASString;
            xQuery.Next;
        end;
    finally
        xQuery.Free;
    end;
end;

class procedure TfrmReagentsOfLayout.AddReagentsOfLayout(aReagentNames: TListItems; aLayoutName: string);
var
    xQuery: TDataProvider;
    xItem: TListItem;
begin
    xQuery := TDataProviderFactory.Instance.CreateDataProvider();
    try

        xQuery.SelectAndOpen('SELECT DISTINCT SUBSTID FROM LAYOUT as l, POSINFO as p WHERE (l.NAME = ''' +
            aLayoutName +
            ''') and (l.RACKID = p.RACKID) and (STEP = 0) AND (SUBSTID <> '''') ORDER BY SUBSTID', true);

        while not xQuery.Eof do
        begin
            xItem := aReagentNames.Add;
            xItem.Caption := xQuery.FieldbyName('SUBSTID').ASString;
            xQuery.Next;
        end;
    finally
        xQuery.Free;
    end;
end;

procedure TfrmReagentsOfLayout.DoUpdate;
var
    xShowMolWeight: boolean;
begin
    lvReagents.Items.Clear;
    xShowMolWeight := false;

    if (rgAll.ItemIndex = 1) then
    begin
        if TZADesignLayoutManager.Instance.IsCurrentLayoutEmpty then
            exit;
        AddReagentsOfLayout(lvReagents.Items, TZADesignLayoutManager.Instance.CurrentLayout.LayoutName);
    end
    else
    begin
        AddReagentsOfAllLayouts(lvReagents.Items);
    end;

    if (xShowMolWeight) then
        lvReagents.ViewStyle := vsReport
    else
        lvReagents.ViewStyle := vsList;
end;

procedure TfrmReagentsOfLayout.FormClose(Sender: TObject; var Action: TCloseAction);
begin
    Action := caFree;
end;

procedure TfrmReagentsOfLayout.FormCreate(Sender: TObject);
begin
    TControlUtils.ResetFontForWinXP(self);
end;

procedure TfrmReagentsOfLayout.rgAllClick(Sender: TObject);
begin
    DoUpdate;
end;

procedure TfrmReagentsOfLayout.UpdateReagents;
begin
    self.DoUpdate;
end;

procedure TfrmReagentsOfLayout.lvReagentsDblClick(Sender: TObject);
var
    xReagentName: string;
begin
    if (lvReagents.Itemindex >= 0) then
        xReagentName := lvReagents.Items[lvReagents.Itemindex].Caption
    else
        xReagentName := '';

    frmEdMain.AddReagent(xReagentName);
end;

function TfrmReagentsOfLayout.GetCurrentReagent: string;
begin
    result := lvReagents.ItemFocused.Caption;
end;


end.
