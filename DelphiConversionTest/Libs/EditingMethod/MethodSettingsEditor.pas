{ -----------------------------------------------------------------------------------------------------------
  Copyright © 2013 Zinsser Analytic GmbH - All rights reserved.
  Author       : Wolfgang Lyncke (wl)
  Description  :
  -----------------------------------------------------------------------------------------------------------
  Revision History:
  date     op  method                              track-no improvement/change
  -------- --  ----------------------------------  -------- -------------------------------------------------
  30.08.13 wl                                      TN6236   neuer Editor: ersetzt MethodLayoutPage und MethodCommentPage
  30.08.13 wl                                      TN6237   verwendet TLayoutWorkspaceDataAdaptor.InstReadAllNames statt LayoutDataAdaptor
  ----------------------------------------------------------------------------------------------------------- }

unit MethodSettingsEditor;


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
    MethodSettingsDataAdaptor,
    StdCtrls,
    ExtCtrls;

type
    TfrmMethodSettingsEditor = class(TForm)
        PageControl1: TPageControl;
        tbCommon: TTabSheet;
        tbStartOptions: TTabSheet;
        tbBuildingBlockOptions: TTabSheet;
        pnlBottom: TPanel;
        btnOK: TButton;
        btnCancel: TButton;
        cbStartable: TCheckBox;
        pnStartable: TPanel;
        cbUseLayout: TCheckBox;
        rbUseDisplayComponent: TRadioButton;
        rbNoDisplayComponent: TRadioButton;
        cbLayoutName: TComboBox;
        cbChooseLayout: TCheckBox;
        cbUseStandardInit: TCheckBox;
        cbDeleteRunData: TCheckBox;
        cbDisplayComponentName: TComboBox;
        cbRestartEventName: TComboBox;
        Memo1: TMemo;
        lblMethodComment: TLabel;
        tbAttributes: TTabSheet;
        cbIsBuildingBlock: TCheckBox;
        edImageFileName: TEdit;
        lblImageFileName: TLabel;
        btnImageFile: TButton;
        lblRestartEvent: TLabel;
        rbReadOnly: TRadioButton;
        rbHidden: TRadioButton;
        rbDefault: TRadioButton;
        lblLineBreakInfo: TLabel;
        cbEditInRunner: TCheckBox;
        procedure FormCreate(Sender: TObject);
        procedure FormShow(Sender: TObject);
        procedure cbUseLayoutClick(Sender: TObject);
        procedure cbChooseLayoutClick(Sender: TObject);
        procedure rbUseDisplayComponentClick(Sender: TObject);
        procedure rbNoDisplayComponentClick(Sender: TObject);
        procedure cbStartableClick(Sender: TObject);
        procedure Memo1KeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
        procedure FormClose(Sender: TObject; var Action: TCloseAction);
        procedure btnImageFileClick(Sender: TObject);
    private
        fIsRunner: boolean;
        fSettings: TMethodSettingsRec;
        function LoadDisplayComponentNames: TArray<string>;
        function LoadRestartEventNames: TArray<string>;
        function LoadLayoutNames: TArray<string>;
        function CheckSettings(out oErrorText: string): boolean;
        function GetLayoutName: string;
    public
        property IsRunner: boolean read fIsRunner write fIsRunner;
        property Settings: TMethodSettingsRec read fSettings write fSettings;
    end;


implementation


{$R *.dfm}

uses
    AppSettings,
    CommonTypes,
    DisplayComponentIntf,
    ControlUtils,
    LayoutWorkspaceDataAdaptor,
    DisplayComponentSettingsManager,
    ViewItemsWorkflow,
    MethodDataCache,
    GeneralTypes;

{ TfrmMethodSettingsEditor }

procedure TfrmMethodSettingsEditor.btnImageFileClick(Sender: TObject);
var
    xOpenDialog: TOpenDialog;
begin
    xOpenDialog := TOpenDialog.Create(nil);
    try
        xOpenDialog.InitialDir := TAppSettings.DataPath;
        xOpenDialog.FileName := edImageFileName.Text;
        xOpenDialog.Filter := TLanguageString.Read('Bitmap files, preferably 64x64 (*.bmp)|*.bmp|',
            'Bitmapdateien, möglichst 64x64 (*.bmp)|*.bmp|');
        if (xOpenDialog.Execute) then
            edImageFileName.Text := xOpenDialog.FileName;
    finally
        xOpenDialog.Free;
    end;
end;

procedure TfrmMethodSettingsEditor.cbChooseLayoutClick(Sender: TObject);
begin
    cbLayoutName.Visible := cbUseLayout.Checked;
    cbChooseLayout.Visible := not cbUseLayout.Checked;
end;

procedure TfrmMethodSettingsEditor.cbStartableClick(Sender: TObject);
begin
    pnStartable.Visible := cbStartable.Checked;
end;

procedure TfrmMethodSettingsEditor.cbUseLayoutClick(Sender: TObject);
begin
    cbLayoutName.Visible := cbUseLayout.Checked;
    cbChooseLayout.Visible := not cbUseLayout.Checked;
end;

function TfrmMethodSettingsEditor.GetLayoutName: string;
begin
    if (cbUseLayout.Checked) then
        EXIT(cbLayoutName.Text);

    if (cbChooseLayout.Checked) then
        EXIT(TMethodSettingsDataAdaptor.cChooseLayoutAtRuntime);

    EXIT('');
end;

function TfrmMethodSettingsEditor.LoadDisplayComponentNames: TArray<string>;
begin
    EXIT(TDisplayComponentSettingsManager.ReadNamesForType(IMainDisplayComponent));
end;

function TfrmMethodSettingsEditor.LoadLayoutNames: TArray<string>;
begin
    EXIT(TLayoutWorkspaceDataAdaptor.InstReadAllNames());
end;

function TfrmMethodSettingsEditor.LoadRestartEventNames: TArray<string>;
begin
    EXIT(TMethodDataCache.Instance.ReadMethodNames());
end;

function TfrmMethodSettingsEditor.CheckSettings(out oErrorText: string): boolean;
begin
    fSettings.LayoutName := GetLayoutName();
    if rbHidden.Checked then
        fSettings.Attributes := TMethodSettingsDataAdaptor.cAttributeHidden
    else if rbReadOnly.Checked then
        fSettings.Attributes := TMethodSettingsDataAdaptor.cAttributeReadOnly
    else
        fSettings.Attributes := TMethodSettingsDataAdaptor.cAttributeDefault;
    fSettings.Comment := self.Memo1.Text;
    fSettings.IsBuildingBlock := self.cbIsBuildingBlock.Checked;
    fSettings.Startable := self.cbStartable.Checked;
    fSettings.InitAtStart := self.cbUseStandardInit.Checked;
    fSettings.DeleteRunDataAtStart := self.cbDeleteRunData.Checked;
    if (rbUseDisplayComponent.Checked) then
        fSettings.DisplayComponentName := cbDisplayComponentName.Text
    else
        fSettings.DisplayComponentName := '';
    fSettings.ImageFileName := self.edImageFileName.Text;
    fSettings.RestartEventName := self.cbRestartEventName.Text;
    fSettings.EditInRunner := self.cbEditInRunner.Checked;

    EXIT(true);
end;

procedure TfrmMethodSettingsEditor.FormClose(Sender: TObject; var Action: TCloseAction);
var
    xMsgText: string;
begin
    if (self.ModalResult = mrOK) and not CheckSettings(xMsgText) then
    begin
        ShowMessage(xMsgText);
        Action := TCloseAction.caNone;
    end;
end;

procedure TfrmMethodSettingsEditor.FormCreate(Sender: TObject);
begin
    cbUseLayout.Caption := TLanguageString.Read('Load this layout:', 'Dieses Layout laden:');
    cbChooseLayout.Caption := TLanguageString.Read('Choose layout at runtime',
        'Das Layout zur Laufzeit wählen');
    cbUseStandardInit.Caption := TLanguageString.Read('Use standard initialization',
        'Standard-Initialisierung benutzen');
    cbDeleteRunData.Caption := TLanguageString.Read('Delete run layout and posinfo data at begin',
        'Zu Beginn Run-Layout und Posinfo-Daten löschen');
    rbUseDisplayComponent.Caption := TLanguageString.Read('Choose a Display Component',
        'Display-Komponente auswählen');
    rbNoDisplayComponent.Caption := TLanguageString.Read('Use Default Display Component',
        'Default Display-Komponente verwenden');
    lblRestartEvent.Caption := TLanguageString.Read('Use Restart Event:', 'Restart-Ereigniss verwenden:');
    self.lblMethodComment.Caption := TLanguageString.read('Method Comment:', 'Methoden-Kommentar:');
    lblLineBreakInfo.Caption := TLanguageString.Read('Line break: Shift+Return/Alt+Return',
        'Zeilenumbruch: Shift+Return/Alt+Return');
    cbStartable.Caption := TLanguageString.Read('Method can be started', 'Methode kann ausgeführt werden');
    tbCommon.Caption := TLanguageString.Read('Common', 'Allgemein');
    self.tbStartOptions.Caption := TLanguageString.Read('Start Options', 'Start-Optionen');
    tbBuildingBlockOptions.Caption := TLanguageString.
        Read('Building Block Options', 'Building-Block-Optionen');
    self.tbAttributes.Caption := TLanguageString.Read('Attributes', 'Attribute');
    cbIsBuildingBlock.Caption := TLanguageString.Read('Is Building Block', 'Ist ein Building-Block');
    lblImageFileName.Caption := TLanguageString.Read('Image File:', 'Bild-Datei:');
    self.cbEditInRunner.Caption := TLanguageString.Read('Can be edited in ZARunner',
        'Kann in ZARunner bearbeitet werden');
end;

procedure TfrmMethodSettingsEditor.FormShow(Sender: TObject);
var
    xIniAccess: IWinLissyIniAccess;
begin
    self.Caption := TLanguageString.Read('Method Settings: ', 'Methoden-Einstellungen: ') +
        fSettings.MethodName;

    cbStartable.Checked := fSettings.Startable;
    cbStartableClick(Sender);

    cbUseLayout.Checked := (fSettings.LayoutName <> '') and
        (fSettings.LayoutName <> TMethodSettingsDataAdaptor.cChooseLayoutAtRuntime);
    if (fSettings.LayoutName <> TMethodSettingsDataAdaptor.cChooseLayoutAtRuntime) then
        cbLayoutName.Text := fSettings.LayoutName;
    cbChooseLayout.Checked := (fSettings.LayoutName = TMethodSettingsDataAdaptor.cChooseLayoutAtRuntime);
    TControlUtils.AddValuesToComboBox(self.LoadLayoutNames, cbLayoutName, true);

    cbUseStandardInit.Checked := fSettings.InitAtStart;
    cbDeleteRunData.Checked := fSettings.DeleteRunDataAtStart;

    if TAppSettings.UseDisplayComponents then
    begin
        TControlUtils.AddValuesToComboBox(self.LoadDisplayComponentNames, cbDisplayComponentName, true);
    end
    else
    begin
        rbUseDisplayComponent.Visible := false;
        rbNoDisplayComponent.Visible := false;
    end;
    rbUseDisplayComponent.Checked := (fSettings.DisplayComponentName <> '');
    rbNoDisplayComponent.Checked := (fSettings.DisplayComponentName = '');
    cbDisplayComponentName.Text := fSettings.DisplayComponentName;
    rbUseDisplayComponentClick(self);

    cbRestartEventName.Text := fSettings.RestartEventName;

    xIniAccess := TAppSettings.CreateAppIni;
    if (not xIniAccess.ReadBool('Restart', 'Enabled')) then
    begin
        TControlUtils.AddValuesToComboBox(self.LoadRestartEventNames, cbRestartEventName, true);
        lblRestartEvent.Visible := false;
        cbRestartEventName.Visible := false;
    end;

    case (fSettings.Attributes) of
        TMethodSettingsDataAdaptor.cAttributeHidden:
            rbHidden.Checked := true;
        TMethodSettingsDataAdaptor.cAttributeReadOnly:
            rbReadOnly.Checked := true;
        else
            rbDefault.Checked := true;
    end;

    if (not TViewItemsWorkflow.Instance.HiddenMethodsAreShown) then
        self.tbAttributes.TabVisible := false;

    self.Memo1.Text := fSettings.Comment;
    self.cbIsBuildingBlock.Checked := fSettings.IsBuildingBlock;
    self.edImageFileName.Text := fSettings.ImageFileName;
    self.cbEditInRunner.Checked := fSettings.EditInRunner;

    if fIsRunner then
    begin
        cbStartable.Enabled := false;
        self.cbEditInRunner.Visible := false;
    end;
end;

procedure TfrmMethodSettingsEditor.Memo1KeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
var
    xSelStart: integer;
begin
    if (Key = VK_RETURN) and (Shift = []) then
    begin
        self.ModalResult := mrOK;
    end;
    if (Key = VK_RETURN) and (ssAlt in Shift) and not Memo1.ReadOnly then
    begin
        // Alt + Return funktioniert leider nicht von selbst, deshalb muss man nachhelfen
        xSelStart := Memo1.SelStart;
        // Selektierte Zeichen löschen
        Memo1.Text := Copy(Memo1.Text, 1, Memo1.SelStart) + #13 + #10 +
            Copy(Memo1.Text, Memo1.SelStart + Memo1.SelLength + 1);
        Memo1.SelStart := xSelStart + 2;
    end;
end;

procedure TfrmMethodSettingsEditor.rbNoDisplayComponentClick(Sender: TObject);
begin
    cbDisplayComponentName.Visible := rbUseDisplayComponent.Checked and TAppSettings.UseDisplayComponents;
end;

procedure TfrmMethodSettingsEditor.rbUseDisplayComponentClick(Sender: TObject);
begin
    cbDisplayComponentName.Visible := rbUseDisplayComponent.Checked and TAppSettings.UseDisplayComponents;
end;


end.
