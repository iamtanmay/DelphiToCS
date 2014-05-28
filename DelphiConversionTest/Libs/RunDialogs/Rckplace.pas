unit RckPlace;
{ ---------------------------------------------------------------------------------------------------
  Visualisierung der Rack-Platzierung
  ---------------------------------------------------------------------------------------------------
  Datum    op  function/procedure    Änderung / Neuerung
  -------- --  -------------------   ----------------------------------------------------------------
  07.05.98 mo  RackPlaceListViewDblClick   Möglichkeit zum löschen von RackBarcodes
  ---------------------------------------------------------------------------------------------------
  21.10.98 wl  alle Funktionen       nötige Änderungen für Version 4.0
  02.12.00 tbh alle Funktionen       Anpassung an Version 5.3
  07.12.00 tbh ShowRackList          Anzeige von bestimmten Racktypen möglich (SR, DR usw.)
  22.12.00 tbh RackPlaceListViewDblClick  deaktiviert!
  02.01.01 tbh RackPlaceListViewChanging  Anzeige orientiert sich an Carrier nicht mehr an Rack
  26.01.01 tbh MarkwithColor         Neu: übernimmt farbiges markieren der Racks
  26.01.01 tbh ShowRackList          Racks eines Racktypes können jetzt farbig unterlegt angezeigt werden
  29.01.01 tbh MarkwithColor         Alle Racks eines Carriers können farbig markiert werden
  17.05.01 mo  ShowRackList          Numerische Sortierung nach slots
  17.05.01 mo  ShowRackList          Liste bleibt immer enabled ( sonst kann nicht gesscrollt werden )
  10.10.02 wl                               TN1293.2 Ini Access - uses geändert
  27.12.02 wl                         TN1293.5 uses und WinlissyIniAccess geändert
  28.02.03 mo                         TN1348   Ressourcen korrigiert
  17.06.03 tbh                        TN1503.1 alle Farbwechsel für Racks jetzt mit WB.PaintLayoutRack
  02.09.03 wl  MarkwithColor          TN1559  SLOTNAME durch Char-Array ersetzt
  22.06.04 pk  ShowRackListInstance   TN2002  New
  24.06.04 wl                         TN2007   uses Variants (nur Delphi 6 und 7)
  28.06.04 pk  ShowRackListInstance   TN2009.5 do not free RackPlaceList
  08.09.05 wl                         TN2585   uses LayOForm entfernt
  31.01.07 wl  MarkwithColor          TN3532   benutzt TCarrier-property SlotGroupRows/Cols statt SlotGroup.Rows/Cols
  20.06.08 pk                         TN4139    WB global object replaced by LayoutManager
  20.06.08 pk                         TN4139   Rack no longer has typed link to Carrier use TLayout.GetCarrierOfRack
  24.06.08 wl  MarkwithColor          TN4143   Compiler-Fehler entfernt
  21.07.08 pk  ShowRackList           TN4179   calls ReadRackRecsByRun
  25.09.08 pk  InstPromptModal        TN4241   New
  10.08.09 wl                         TN4702   Strings werden jetzt direkt geladen
  08.09.09 pk  RackPlaceListViewDblClick         TN4753  Now creates DataProvider instead of using qryTools
  26.10.09 wl                                    TN4831  TAppSettings.Load/SaveFormPosition entfernt
  20.05.10 wl                                TN5117   neue Schriftart "Segoe UI", Schriftgröße 9
  07.06.10 ts  RackPlaceListViewChanging     TN5124   Layout refresh + StringLoader vervollständigt
  07.06.10 ts  RackPlaceListViewDblClick     TN5124   entfernt + Fenster an Design angepasst
  21.06.10 wl                                TN5160   Position = poScreenCenter
  28.10.10 wl                                TN5314   Buttons neu beschriftet, in Version 7.4.4 eingefügt, benutzt TRack.Highlight
  -------------------------------------------------------------------------------------------------- }


interface


uses
    Windows,
    Classes,
    Graphics,
    Controls,
    Forms,
    StdCtrls,
    Buttons,
    ComCtrls,
    ExtCtrls,
    StringLoader,
    Rack;

type
    TRackPlaceListStringLoader = class(TTextListStringLoader)
    protected
        procedure AddAllItems(); override;
    end;

    TRackPlaceList = class(TForm)
        RackPlaceListView: TListView;
        Panel1: TPanel;
        Panel2: TPanel;
        btnCancel: TButton;
        btnOK: TButton;
        Label1: TLabel;
        procedure FormClose(Sender: TObject; var Action: TCloseAction);
        procedure RackPlaceListViewChanging(Sender: TObject; Item: TListItem; Change: TItemChange;
            var AllowChange: Boolean);
        procedure RackPlaceListViewDblClick(Sender: TObject);
        procedure FormCreate(Sender: TObject);
        procedure FormDestroy(Sender: TObject);
        procedure FormShow(Sender: TObject);
    private
        fStringLoader: TRackPlaceListStringLoader;
        FDoNotPaint: boolean;
        fLastHighlightedRack: TRack;
        procedure UnMarkRack;
    public
        procedure ShowRackList(RunName, LayoutName, RackTypes, CaptionStr: string; AbortAllowed: Boolean);
        procedure MarkwithColor(Item: TListItem);
        class function InstPromptModal(const aRunName, aLayoutName, aRackTypes, aCaptionStr: string;
            aAbortAllowed, aMarkRacks: boolean): TModalResult;
    end;


implementation


{$R *.DFM}

uses
    Variants,
    SysUtils,
    AppTypes,
    SamGlobe,
    GUIManager,
    AppSettings,
    GeneralTypes,
    LayoutManager,
    Carrier,
    CarrierSlot,
    LayoutDataAdaptor,
    LogManager,
    DataProvider,
    ControlUtils;

{ TRackPlaceListStringLoader }

procedure TRackPlaceListStringLoader.AddAllItems;
begin
    AddSingle(510, 'Continue', 'Fortsetzen');
    AddSingle(520, 'Abort', 'Abbrechen');
    AddSingle(49010, 'Please place all racks as listed below.',
        'Bitte plazieren Sie die Racks so, wie unten aufgelistet.');
    AddSingle(49220, 'Carrier name', 'Carrier-Name');
    AddSingle(49230, 'Slot No', 'Slot Nr.');
    AddSingle(49240, 'Rack name', 'Rack-Name');
    AddSingle(49250, 'Racktype', 'Racktyp');
    AddSingle(49260, 'Barcode', 'Barcode');
end;

{ TRackPlaceList }

class function TRackPlaceList.InstPromptModal(const aRunName, aLayoutName, aRackTypes, aCaptionStr: string;
    aAbortAllowed, aMarkRacks: boolean): TModalResult;
var
    xRackPlaceList: TRackPlaceList;
begin
    xRackPlaceList := TRackPlaceList.Create(nil);
    try
        xRackPlaceList.ShowRackList(aRunName, aLayoutName, aRackTypes, aCaptionStr, aAbortAllowed);
        result := xRackPlaceList.ShowModal;
    finally
        xRackPlaceList.Free;
    end;
end;

procedure TRackPlaceList.FormCreate(Sender: TObject);
begin
    TControlUtils.ResetFontForWinXP(self);
    fStringLoader := TRackPlaceListStringLoader.Create;
    fStringLoader.LoadLanguage(self);

    if (RackPlaceListView.enabled = false) then
        RackPlaceListView.enabled := true
end;

procedure TRackPlaceList.FormDestroy(Sender: TObject);
begin
    fStringLoader.Free;
end;

procedure TRackPlaceList.FormClose(Sender: TObject; var Action: TCloseAction);
begin
    UnMarkRack();

    Action := caFree;
end;

procedure TRackPlaceList.FormShow(Sender: TObject);
begin
    self.btnOK.SetFocus;
end;

// --------------------------------------------------------------------------------------------------
procedure TRackPlaceList.RackPlaceListViewChanging(Sender: TObject; Item: TListItem; Change: TItemChange;
    var AllowChange: Boolean);
// --------------------------------------------------------------------------------------------------
begin
    if FDoNotPaint then
        EXIT;

    MarkwithColor(Item);
end;

// --------------------------------------------------------------------------------------------------
procedure TRackPlaceList.ShowRackList(RunName, LayoutName, RackTypes, CaptionStr: string;
    AbortAllowed: Boolean);
// --------------------------------------------------------------------------------------------------
var
    cnt: integer;
    NewItem: TListItem;
    xDA: TLayoutDataAdaptor;
    xRecs: TLayoutRackRecArray;
    x: integer;
begin
    if AbortAllowed then
        btnCancel.visible := true
    else
        btnCancel.visible := false;
    cnt := 0;
    FDoNotPaint := true;
    if CaptionStr <> '' then
        Caption := CaptionStr;
    height := 120;

    gLogManager.Log('ShowRackList ' + RunName, true);

    xDA := TLayoutDataAdaptor.Create();
    try
        xDA.ReadRackRecsByRun(RunName, xRecs);
    finally
        xDA.Free;
    end;

    for x := 0 to high(xRecs) do
    begin
        if (RackTypes = '') or (pos(Uppercase(Copy(xRecs[x].RackName, 0, 2)), RackTypes) > 0) then
        begin
            NewItem := RackPlaceListView.Items.Add;
            RackPlaceListView.Items[Cnt].Caption := xRecs[x].CarrierName + Format('%03d', [xRecs[x].Slot]);
            NewItem.SubItems.Add(xRecs[x].CarrierName);
            NewItem.SubItems.Add(Format('%03d', [xRecs[x].Slot]));
            NewItem.SubItems.Add(xRecs[x].RackName);
            NewItem.SubItems.Add(xRecs[x].RackType);
            NewItem.SubItems.Add(xRecs[x].RackType);
            height := height + 18;
            inc(cnt);
        end;
    end;

    FDoNotPaint := false;
end;

procedure TRackPlaceList.UnMarkRack;
begin
    if self.fLastHighlightedRack <> nil then
    begin
        fLastHighlightedRack.Highlight := false;
        fLastHighlightedRack := nil;
    end;
end;

// --------------------------------------------------------------------------------------------------
procedure TRackPlaceList.RackPlaceListViewDblClick(Sender: TObject);
// --------------------------------------------------------------------------------------------------
// var
// xDataProvider : TDataProvider;
begin
    // if (RackPlaceListView.Selected.SubItems[4]>'')
    // and (copy( RackPlaceListView.Selected.SubItems[2],1,2)<>'DT')
    // then if gGUIManager.MessageBox(TLanguageString.Read( 'Delete barcode of selected item ?', 'Soll der Barcode des markierten Objekts gelöscht werden ?' ),
    // TLanguageString.Read( 'Delete Barcode', 'Barcode löschen' ),mb_YESNO+mb_ICONQUESTION+mb_APPLMODAL) = mrYES
    // then begin
    // xDataProvider := TDataProviderFactory.Instance.CreateDataProvider();
    // try
    // xDataProvider.ExecSQL( 'update LAYOUT set RACKID=null where RUN is not null and RACKID="'+RackPlaceListView.Selected.SubItems[4]+'"');
    // finally
    // xDataProvider.Free;
    // end;
    // RackPlaceListView.Selected.SubItems[4]:='';
    // end;
end;

// --------------------------------------------------------------------------------------------------
procedure TRackPlaceList.MarkwithColor(Item: TListItem);
// --------------------------------------------------------------------------------------------------
var
    x: integer;
    xCarrier: TCarrier;
    xSlot: TCarrierSlot;
    xItemSlotNr: integer;
    xItemCarrierName: string;
    xSlotRack: TRack;
begin
    UnMarkRack();

    xItemCarrierName := item.subitems[0];
    xItemSlotNr := StrToInt(item.subitems[1]);

    for x := 0 to TLayoutManager.Instance.CurrentLayout.NoOfCarrier - 1 do
    begin

        xCarrier := TLayoutManager.Instance.CurrentLayout.Carriers[x];
        if (xCarrier.Name <> xItemCarrierName) then
            CONTINUE;

        xSlot := xCarrier.GetSlotBySlotNr(xItemSlotNr);
        ASSERT(xSlot <> nil);
        xSlotRack := xSlot.Rack;
        if not Assigned(xSlotRack) then
            EXIT;

        xSlotRack.Highlight := true;
        self.fLastHighlightedRack := xSlotRack;
        EXIT;
    end;
end;


end.
