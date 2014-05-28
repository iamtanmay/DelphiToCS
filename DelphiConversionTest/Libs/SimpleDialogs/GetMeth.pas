{ --------------------------------------------------------------------------------------------------
  Ebene 4 (Script- & Methoden-Tools)
  --------------------------------------------------------------------------------------------------
  Auswahlfenster für Methoden und Scripte
  --------------------------------------------------------------------------------------------------
  Datum    op  function/procedure      Änderung / Neuerung
  -------- --  --------------------    -------------------------------------------------------------
  05.05.98 wl                         neu (von GetEntry kopiert)
  15.05.98 mo  ListBox1DblClick       neu öffnet Methodeneditor
  03.06.98 wl  FormActivate           Abfrage von MethEditForm in Main verschoben
  FormClose              action:=caFree
  23.06.98 wl                         uses SamIntf,SamGlobe  (statt globals)
  10.07.98 wl  sbMethodCreateClick    Aufruf mit Angabe der Methode (s.Änderungen dbMTools)
  sbMethodDeleteClick     dito
  sbMethodEditClick       dito
  13.07.98 wl                          jeden Bezug auf PMethodName entfernt
  gibt Funktionalität (Edit,Start,...) als TGetMethAction an Main weiter
  sbSelectClick           Methode wird ausgewählt, Editierfenster aber nicht geladen
  14.07.98 wl  GetMethod               Modal-Aufruf des Fensters als Funktion
  29.07.98 wl  FormActivate            SamplerActive-Abfrage entfernt (bereits in MAIN)
  30.07.98 wl  FormActivate            CancelRange
  28.09.98 wl  sbBuildClick            neu: Build Method
  01.10.98 wl                          Layout wird mit angezeigt
  FormActivate            Unterscheidung zwischen Script und Method
  05.11.98 wl  alle Methoden           Method-spezifische Namen geändert
  sbDeleteClick           Scripte werden jetzt auch gelöscht
  02.02.99 wl  ListView1DblClick       öffnet Methodeneditor
  22.02.99 mo                          ListView1.ReadOnly = true  -> Methoden bezeichnung nicht editierbar
  ListView1.Rowselect = true -> komplette Zeile kann ausgewählt werden
  25.02.99 wl                          Select-Button in "Open" umbenannt (auf vielfachen Wunsch)
  02.03.99 mo ListView1DblClick        edit wird nur ausgeführt wenn sbEdit.visble=true
  09.05.00 wl                          nur Tabellennamen geändert
  26.05.00 wl                          uses geändert
  23.06.00 wl  ListView1DblClick       wenn sbEdit.visble=false wird die Methode ohne Editfenster geöffnet
  28.08.02 mo  GetSelectedItem         TN1217 neu
  28.08.02 mo  ListView1ColumnClick    TN1217 Sortierung kann geändert werden
  31.01.03 wl  SetNewName              TN1334.1 User-Rechte werden direkt abgefragt
  12.03.03 wl                          TN1293.5 uses posTools
  15.02.05 pk                          TN2315   Now displays Sessions
  28.02.05 pk  ListView1DblClick       TN2315   Session : actSelect instead of actEdit
  07.04.05 pk  ReadItems               TN2375.1 ReadAllMethodsAndLayouts
  15.06.05 wl                          TN2465   uses DM_Sampl entfernt
  22.08.05 wl                          TN2558.8  uses ScheUtil entfernt, ScheUtil-Methoden werden mit TDMScript. oder global. aufgerufen
  08.11.05 wl  TGetMethDatasourceType  TN2745    dtScript entfernt (und alles was damit zu tun hat)
  24.11.05 pk  sbDeleteClick           TN2805    removed
  21.01.08 wl                          TN3972    Bug behoben
  08.09.08 pk                          TN4215    Most buttons removed
  20.09.08 pk  ListView1DblClick       TN4215    Starts the method
  03.12.08 pk                          TN4342    Do not load layout names (takes too long)
  03.12.08 pk                          TN3997    various changes to allow access with keyboard
  13.03.09 ts ReadAllMethodsAndLayouts TN4464    es werden nur Methoden mit "startable=1" aus METHODSETTINGS angezeigt/ Layoutanzeige hinzugefügt
  19.05.09 wl                                    unbenutzte Variable entfernt
  16.06.09 wl                          TN4605   uses MethodSettingsDataAdaptor
  10.08.09 wl                          TN4702   GetMethDlg wird nicht mehr erzeugt
  20.05.10 wl                          TN5117   neue Schriftart "Segoe UI", Schriftgröße 9
  08.06.10 ts                          TN5136   Fenster an Design angepasst
  18.06.10 pk                          TN5152.1 uses changed
  21.06.10 wl                               TN5160   Position = poScreenCenter
  14.12.11 wl                               TN5765   ohne Session
  14.03.12 wl                               TN5831   TStartableMethodRec ersetzt TStringPairRec
  30.08.13 wl                               TN6236   verwendet TMethodSettingsDataAdaptor
  28.11.13 ts  ReadItems                    TN6318   TemporaryMethod rausgefiltert
  -------------------------------------------------------------------------------------------------- }

unit getMeth;


interface


uses
    Forms,
    ComCtrls,
    StdCtrls,
    ExtCtrls,
    Classes,
    Controls,
    MethodSettingsDataAdaptor;

type
    TGetMethAction = (actNone, actStart);

    TGetMethDlg = class(TForm)
        Panel1: TPanel;
        Panel4: TPanel;
        ListView1: TListView;
        Panel3: TPanel;
        lblTitle: TLabel;
        Label3: TLabel;
        btnStart: TButton;
        procedure FormActivate(Sender: TObject);
        procedure sbStartSamplerClick(Sender: TObject);

        procedure ListBox1DblClick(Sender: TObject);
        procedure ListView1DblClick(Sender: TObject);
        procedure FormCreate(Sender: TObject);
        procedure ListView1ColumnClick(Sender: TObject; Column: TListColumn);
        procedure ListView1KeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
        procedure ListView1SelectItem(Sender: TObject; Item: TListItem; Selected: Boolean);
    private const
        cLayoutCaption = 'Layout';
    private
        FName: string;
        FAction: TGetMethAction;
        FSortByLayout: Boolean;
        procedure SetNewName;
        procedure SetCaptions(const aTitle, aColTitle, aStart, aStartHint: string);
        class function ReadAllMethodsAndLayouts: TArray<TMethodSettingsRec>;
        procedure ReadItems;
        function GetSelectedItem: string;
    public
        class function Call(var NewName: string): TGetMethAction;
    end;


implementation


{$R *.DFM}

uses
    AppSettings,
    GeneralTypes,
    CommonTypes,
    ControlUtils,
    LayoutDataAdaptorExt;

procedure TGetMethDlg.FormActivate(Sender: TObject);
begin
    ReadItems;
end;

class function TGetMethDlg.Call(var NewName: string): TGetMethAction;
var
    xForm: TGetMethDlg;
begin
    xForm := TGetMethDlg.Create(nil);
    try
        xForm.FName := NewName;
        xForm.FAction := actNone;
        xForm.ShowModal;
        NewName := xForm.FName;
        result := xForm.FAction;
    finally
        xForm.Free;
    end;
end;

procedure TGetMethDlg.sbStartSamplerClick(Sender: TObject);
begin
    FAction := actStart;
    Close;
end;

procedure TGetMethDlg.SetNewName;
begin
    Label3.Caption := FName;

    btnStart.Enabled := false;

    if (FName <> '') then
    begin

        // No system user: no start, no build!
        if gCommonDll.LevelIsIncluded(gCommonDll.CurrentUser.Level, usrSystem) then
        begin
            btnStart.Enabled := true;
        end;
    end;
end;

procedure TGetMethDlg.ListBox1DblClick(Sender: TObject);
begin
    FName := GetSelectedItem;
    SetNewName;
    sbStartSamplerClick(Sender);
end;

procedure TGetMethDlg.ListView1DblClick(Sender: TObject);
begin
    if (ListView1.Selected = nil) then
        exit;

    FAction := actStart;
    Close;
end;

procedure TGetMethDlg.SetCaptions(const aTitle, aColTitle, aStart, aStartHint: string);
begin
    Caption := aTitle;
    lblTitle.Caption := aColTitle + ':';

    if FSortByLayout then
    begin
        ListView1.Columns[1].Caption := aColTitle;
        ListView1.Columns[0].Caption := cLayoutCaption;
    end
    else
    begin
        ListView1.Columns[0].Caption := aColTitle;
        ListView1.Columns[1].Caption := cLayoutCaption;
    end;

    btnStart.Caption := aStart;
    btnStart.Hint := aStartHint;
end;

procedure TGetMethDlg.FormCreate(Sender: TObject);
begin
    TControlUtils.ResetFontForWinXP(self);
    FSortByLayout := false;
end;

class function TGetMethDlg.ReadAllMethodsAndLayouts: TArray<TMethodSettingsRec>;
var
    xDA: TMethodSettingsDataAdaptor;
begin
    xDA := TMethodSettingsDataAdaptor.Create;
    try
        EXIT(xDA.ReadStartables);
    finally
        xDA.Free;
    end;
end;

procedure TGetMethDlg.ReadItems;
var
    i: integer;
    xNamesAndLayouts: TArray<TMethodSettingsRec>;
begin
    ListView1.visible := false;
    ListView1.SortType := stNone;

    // Füllen der Liste
    xNamesAndLayouts := self.ReadAllMethodsAndLayouts();
    SetCaptions(TLanguageString.Read('Select Method', 'Methode auswählen'),
        TLanguageString.Read('Method', 'Methode'), TLanguageString.Read('&Start', '&Start'),
        TLanguageString.Read('Start selected method', 'Markierte Methode starten'));

    ListView1.Items.Clear;
    for i := 0 to high(xNamesAndLayouts) do
    begin
        if xNamesAndLayouts[i].MethodName <> TLayoutDataAdaptorExt.cTemporaryMethodName then
        begin
            ListView1.Items.Add;
            if FSortByLayout then
            begin
                ListView1.Items[i].Caption := xNamesAndLayouts[i].LayoutName;
                ListView1.Items[i].SubItems.Add(xNamesAndLayouts[i].MethodName);
            end
            else
            begin
                ListView1.Items[i].Caption := xNamesAndLayouts[i].MethodName;
                ListView1.Items[i].SubItems.Add(xNamesAndLayouts[i].LayoutName);
            end;
        end;
    end;

    // Suchen nach der aktuellen Methode
    if (ListView1.FindCaption(0, FName, false, false, false) <> nil) then
        FName := '';
    SetNewName;
    if FSortByLayout then
        ListView1.SortType := stText;
    ListView1.visible := true;

    ListView1.SetFocus();
    if ListView1.Items.Count > 0 then
        self.ListView1.ItemIndex := 0;
end;

function TGetMethDlg.GetSelectedItem: string;
begin
    result := ListView1.Selected.Caption;
    if FSortByLayout then
        result := ListView1.Selected.SubItems[0];
end;

procedure TGetMethDlg.ListView1ColumnClick(Sender: TObject; Column: TListColumn);
begin
    if Column.Caption = cLayoutCaption then
        FSortByLayout := true
    else
        FSortByLayout := false;
    ReadItems;
end;

procedure TGetMethDlg.ListView1KeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
    if Key = 13 then
    begin
        ListView1DblClick(self);
    end;
end;

procedure TGetMethDlg.ListView1SelectItem(Sender: TObject; Item: TListItem; Selected: Boolean);
begin
    if ListView1.Selected <> nil then
    begin
        FName := GetSelectedItem;
        SetNewName();
    end;
end;


end.
