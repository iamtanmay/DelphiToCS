{ -----------------------------------------------------------------------------------------------------------------------
  Copyright © 2010 Zinsser Analytic GmbH - All rights reserved.
  Author       : Payman Kamali (pk)
  Description  :
  -----------------------------------------------------------------------------------------------------------------------
  Revision History:
  date     op  method                                track-no  improvement/change
  -------- --  ------------------------------------  --------- ----------------------------------------------------------
  08.10.10 pk                                        TN5295      Initial Revision
  14.12.10 pk  ShowDialog                            TN5330      now returns modal result
  07.02.11 wl  btnAddEmptyStep                       TN5461   neu: Add blank line
  04.03.11 ts  CategorySelectedItemChanged           TN5331   Try..except block, sonst invalid class typecast möglich
  07.03.11 ts  CategorySelectedItemChanged           TN5331   Try..except ersetzt durch if *.selectedItem is TButtonItem + Deutsche Tab-Beschriftung
  10.03.11 wl                                        TN5500   Buttons und Menüs optimieren
  06.06.12 wl  LoadByCategoryItems                   TN5908   Neue Standardkategorien
  07.08.12 wl                                        TN5948   komplett überarbeitet: Jetzt ohne Tabs
  11.12.12 wl                                        TN5948   Eingabe jetzt weniger träge
  11.04.13 wl                                        TN6045   uses geändert
  ----------------------------------------------------------------------------------------------------------------------- }

unit MethodStepSelectDialog;


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
    ComCtrls,
    GeneralTypes,
    ImgList,
    CategoryButtons,
    RunStepInfoTypeInfo,
    RunStepInfo,
    ConfigurationFile,
    Generics.Collections;

type
    TfrmMethodStepSelectDialog = class(TForm)
        pnlTop: TPanel;
        Label1: TLabel;
        BalloonHint1: TBalloonHint;
        cbActionName: TComboBox;
        Panel1: TPanel;
        lstMRU: TListBox;
        Label2: TLabel;
        Panel2: TPanel;
        Bevel1: TBevel;
        btnCancel: TButton;
        btnOK: TButton;
        imgActionImage: TImage;
        mmoDescription: TMemo;
        Panel3: TPanel;
        CategoryButtons1: TCategoryButtons;
        Splitter1: TSplitter;
        procedure FormClose(Sender: TObject; var Action: TCloseAction);
        procedure cbActionNameKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
        procedure CategoryButtons1GetHint(Sender: TObject; const Button: TButtonItem;
            const Category: TButtonCategory; var HintStr: string; var Handled: Boolean);
        procedure CategoryButtons1HotButton(Sender: TObject; const Button: TButtonItem);
        procedure lstMRUDblClick(Sender: TObject);
        procedure lstMRUKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
        procedure lstMRUClick(Sender: TObject);
        procedure CategoryButtons1SelectedItemChange(Sender: TObject; const Button: TButtonItem);
        procedure CategoryButtons1MouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState;
            X, Y: Integer);
        procedure CategoryButtons1KeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
    private const
        cSettingsSectionName = 'MethodStepSelectDlg';
        cSettingsIdentMRUPrefix = 'MRU';
        cMaxCountMRU = 15;
    private
        fImages24, fImages64: TCustomImageList;
        fRunStepInfoList: TList<TRunStepInfo>;
        fMRUHistoryList: TList<string>;
        fAllowedSteps: TList<string>;
        fLoadingItems: boolean;
        function GetRunStepFromList(const aStepName: string): TRunStepInfo;
        function AddCategory(const aCategoryName: string): TButtonCategory;
        procedure AddStepToCategories(const aRunStepInfo: TRunStepInfo);
        procedure AddNewStepNameToMRUList(const aMethodStepName: string);
        procedure ReadMRUSettings(const aIniAccess: IConfigurationSet);
        procedure WriteMRUSettings(const aIniAccess: IConfigurationSet);
        procedure ReadSettings();
        procedure WriteSettings();
        procedure LoadRunStepInfos();
        procedure LoadMRUItems;
        procedure LoadByCategoryItems();
        procedure SetHintForButtonItem(const aButtonItem: TButtonItem; const aHint: TBalloonHint);
        procedure ChangeSelectedStepName(const aStepName: string; aLoadToCombo, aSelectCategoryItem: boolean);
        procedure MRUSelectedItemChanged;
        procedure ComboByNameSelectedItemChanged();
        procedure CategorySelectedItemChanged();
        procedure SelectCategoryItem(const aStepName: string);

        constructor Create(const aItems: TStringArray; const aImages24, aImages64: TCustomImageList);
            reintroduce;
    public
        destructor Destroy(); override;
        class function ShowDialog(const aItems: TStringArray;
            const aSmallImages, aLargeImages: TCustomImageList; out oActionName: string): TModalResult;
    end;


implementation


{$R *.dfm}

uses
    ControlUtils,
    RunStepInfoTypeDictionary,
    AppSettings;

{ TfrmMethodStepSelectDialog }

class function TfrmMethodStepSelectDialog.ShowDialog(const aItems: TStringArray;
    const aSmallImages, aLargeImages: TCustomImageList; out oActionName: string): TModalResult;
var
    xDlg: TfrmMethodStepSelectDialog;
begin
    xDlg := TfrmMethodStepSelectDialog.Create(aItems, aSmallImages, aLargeImages);
    try
        oActionName := '';
        result := xDlg.Showmodal;

        if result = mrOK then
            oActionName := xDlg.cbActionName.Text;
    finally
        FreeAndNil(xDlg);
    end;
end;

constructor TfrmMethodStepSelectDialog.Create(const aItems: TStringArray;
    const aImages24, aImages64: TCustomImageList);
begin
    inherited Create(nil);
    fLoadingItems := true;
    fImages24 := aImages24;
    fImages64 := aImages64;
    self.Caption := TLanguageString.Read('Add new line', 'Neue Zeile hinzufügen');
    fRunStepInfoList := TList<TRunStepInfo>.Create();

    self.cbActionName.Clear;
    self.Label1.Caption := TLanguageString.Read('Action name:', 'Name der Aktion:');
    self.btnOK.Caption := TLanguageString.Read('&OK', '&OK');
    self.btnCancel.Caption := TLanguageString.Read('&Cancel', '&Abbrechen');
    self.Label2.Caption := TLanguageString.Read('Most Recently Used:', 'Zuletzt genutzt:');
    TControlUtils.ResetFontForWinXP(self);

    fMRUHistoryList := TList<string>.Create();

    fAllowedSteps := TList<string>.Create();
    fAllowedSteps.AddRange(TRunStepInfoTypeDictionary.Instance.GetAllActions());

    ReadSettings();

    LoadRunStepInfos();
    LoadMRUItems();
    TControlUtils.AddValuesToComboBox(fAllowedSteps.ToArray, self.cbActionName, true);
    LoadByCategoryItems();
    fLoadingItems := false;
end;

destructor TfrmMethodStepSelectDialog.Destroy();
begin
    FreeAndNil(fMRUHistoryList);
    FreeAndNil(fRunStepInfoList);
    FreeAndNil(fAllowedSteps);
    inherited;
end;

procedure TfrmMethodStepSelectDialog.FormClose(Sender: TObject; var Action: TCloseAction);
begin
    if ModalResult = mrOK then
    begin
        // check if item exists (in case it was typed in using keyboard) and has the correct case.
        if not fAllowedSteps.Contains(cbActionName.Text) then
        begin
            ModalResult := mrNone;
            Action := caNone;
            EXIT;
        end;

        AddNewStepNameToMRUList(cbActionName.Text);
        WriteSettings();
    end;
end;

procedure TfrmMethodStepSelectDialog.AddNewStepNameToMRUList(const aMethodStepName: string);
var
    xIndex: integer;
begin
    xIndex := self.fMRUHistoryList.IndexOf(aMethodStepName);
    if xIndex >= 0 then
    begin
        self.fMRUHistoryList.Move(xIndex, 0);
    end
    else
    begin
        self.fMRUHistoryList.Insert(0, aMethodStepName);
    end;

    if fMRUHistoryList.Count > cMaxCountMRU then
    begin
        self.fMRUHistoryList.Delete(fMRUHistoryList.Count - 1);
    end;

end;

procedure TfrmMethodStepSelectDialog.ReadMRUSettings(const aIniAccess: IConfigurationSet);
var
    x: integer;
    xStepName: string;
begin
    for x := 0 to cMaxCountMRU - 1 do
    begin
        xStepName := aIniAccess.ReadString(cSettingsSectionName,
            Format('%s%.2d', [cSettingsIdentMRUPrefix, x]), '');
        if xStepName = '' then
            BREAK;
        if not fAllowedSteps.Contains(xStepName) then
            CONTINUE;

        fMRUHistoryList.Add(xStepName);
    end;
end;

procedure TfrmMethodStepSelectDialog.WriteMRUSettings(const aIniAccess: IConfigurationSet);
var
    x: integer;
    xStepName: string;
begin
    for x := 0 to fMRUHistoryList.Count - 1 do
    begin
        xStepName := fMRUHistoryList[x];
        aIniAccess.WriteString(cSettingsSectionName, Format('%s%.2d', [cSettingsIdentMRUPrefix, x]),
            xStepName);
    end;
end;

procedure TfrmMethodStepSelectDialog.ReadSettings();
var
    xIniAccess: IConfigurationSet;

begin
    xIniAccess := TConfigurationFile.Create(TAppsettings.LocalIniFileName);
    xIniAccess.Open(true);
    try
        ReadMRUSettings(xIniAccess);
    finally
        xIniAccess.Close;
    end;
end;

procedure TfrmMethodStepSelectDialog.WriteSettings();
var
    xIniAccess: IConfigurationSet;
begin
    xIniAccess := TConfigurationFile.Create(TAppsettings.LocalIniFileName);
    xIniAccess.Open(false);
    try
        WriteMRUSettings(xIniAccess);
    finally
        xIniAccess.Close;
    end;
end;

procedure TfrmMethodStepSelectDialog.SelectCategoryItem(const aStepName: string);
var
    xCategory, xButton: TCollectionItem;
begin
    for xCategory in self.CategoryButtons1.Categories do
    begin
        for xButton in (xCategory as TButtonCategory).Items do
        begin
            if ((xButton as TButtonItem).Caption = aStepName) then
            begin
                (xCategory as TButtonCategory).Collapsed := false;
                (xCategory as TButtonCategory).ScrollIntoView;
                self.CategoryButtons1.SelectedItem := (xButton as TButtonItem);
            end;
        end;
    end;
end;

procedure TfrmMethodStepSelectDialog.SetHintForButtonItem(const aButtonItem: TButtonItem;
    const aHint: TBalloonHint);
var
    xRunStepInfo: TRunStepInfo;
    xPoint: TPoint;
    xRect: TRect;
begin
    aHint.HideHint;
    aHint.Title := '';
    aHint.Description := '';
    aHint.ImageIndex := -1;
    if not Assigned(aButtonItem) then
        EXIT;

    xRunStepInfo := aButtonItem.Data;
    aHint.Title := aButtonItem.Caption;
    aHint.Description := xRunStepInfo.Caption + #13 + xRunStepInfo.Description;
    aHint.ImageIndex := xRunStepInfo.IconIndex;
    xRect := aButtonItem.Bounds;

    xPoint := aButtonItem.CategoryButtons.ClientOrigin;
    OffsetRect(xRect, xPoint.X, xPoint.Y);
    aHint.HideAfter := 3000;
    aHint.ShowHint(xRect);
end;

procedure TfrmMethodStepSelectDialog.CategoryButtons1GetHint(Sender: TObject; const Button: TButtonItem;
    const Category: TButtonCategory; var HintStr: string; var Handled: Boolean);
begin
    HintStr := '';
    Handled := true;
end;

function TfrmMethodStepSelectDialog.GetRunStepFromList(const aStepName: string): TRunStepInfo;
var
    x: integer;
begin
    for x := 0 to fRunStepInfoList.Count - 1 do
    begin
        if (fRunStepInfoList[x].DefaultName = aStepName) then
            EXIT(fRunStepInfoList[x]);
    end;
    EXIT(nil);
end;

procedure TfrmMethodStepSelectDialog.ChangeSelectedStepName(const aStepName: string;
    aLoadToCombo, aSelectCategoryItem: boolean);
var
    xRunStepInfo: TRunStepInfo;
    xSaveColor: TColor;
    xRect: TRect;
begin
    self.mmoDescription.Lines.Clear;
    xSaveColor := self.imgActionImage.Canvas.Brush.Color;
    self.imgActionImage.Canvas.Brush.Color := clWindow;
    xRect.Left := 0;
    xRect.Top := 0;
    xRect.Right := 63;
    xRect.Bottom := 63;
    self.imgActionImage.Canvas.FillRect(xRect);
    // self.imgActionImage.Canvas.Rectangle(0, 0, 63, 63);
    self.imgActionImage.Canvas.Brush.Color := xSaveColor;

    if (fLoadingItems) then
        EXIT;

    xRunStepInfo := GetRunStepFromList(aStepName);
    if not Assigned(xRunStepInfo) then
        EXIT;

    if (aLoadToCombo) then
        self.cbActionName.Text := aStepName;

    if aSelectCategoryItem then
        SelectCategoryItem(aStepName);

    fImages64.Draw(self.imgActionImage.Canvas, 0, 0, xRunStepInfo.IconIndex);
    self.mmoDescription.Lines.Add(xRunStepInfo.Caption);
    if xRunStepInfo.Description <> '' then
        self.mmoDescription.Lines.Add(xRunStepInfo.Description);
end;

procedure TfrmMethodStepSelectDialog.CategoryButtons1HotButton(Sender: TObject; const Button: TButtonItem);
begin
    SetHintForButtonItem(Button, BalloonHint1);
end;

procedure TfrmMethodStepSelectDialog.CategoryButtons1KeyUp(Sender: TObject; var Key: Word;
    Shift: TShiftState);
begin
    if Key = VK_RETURN then
    begin
        self.CategorySelectedItemChanged();
        self.ModalResult := mrOK;
    end;
end;

function TfrmMethodStepSelectDialog.AddCategory(const aCategoryName: string): TButtonCategory;
begin
    result := self.CategoryButtons1.Categories.Add();
    result.Caption := aCategoryName;
    result.Color := clScrollBar;
end;

procedure TfrmMethodStepSelectDialog.AddStepToCategories(const aRunStepInfo: TRunStepInfo);
var
    xCategoryNames: TStringArray;
    x: integer;
    xCategoryIndex: integer;
    xButtonCategory: TButtonCategory;
    xButtonItem: TButtonItem;
    xCategoryName: string;
    xTypeinfo: TRunStepInfoTypeInfo;
begin
    xTypeInfo := TRunStepInfoTypeDictionary.Instance.GetTypeFromTypeName(aRunStepInfo.DefaultName)
        as TRunStepInfoTypeInfo;
    xCategoryNames := xTypeInfo.GetCategoryNames();

    for x := 0 to Length(xCategoryNames) - 1 do
    begin
        xCategoryName := xCategoryNames[x];
        xCategoryIndex := self.CategoryButtons1.Categories.IndexOf(xCategoryName);
        if (xCategoryIndex > -1) then
        begin
            xButtonCategory := self.CategoryButtons1.Categories[xCategoryIndex];
        end
        else
        begin
            // Zusätzliche Kategorien einfügen:
            xButtonCategory := self.AddCategory(xCategoryName);
        end;
        xButtonItem := xButtonCategory.Items.Add;
        xButtonItem.Data := aRunStepInfo;
        xButtonItem.Caption := aRunStepInfo.DefaultName;
        xButtonItem.ImageIndex := aRunStepInfo.IconIndex;
    end;
end;

procedure TfrmMethodStepSelectDialog.LoadByCategoryItems();
var
    x: integer;
    xRunStepInfo: TRunStepInfo;
begin
    self.CategoryButtons1.Images := fImages24;
    self.CategoryButtons1.OnGetHint := self.CategoryButtons1GetHint;
    self.BalloonHint1.Images := fImages64 as TImageList;

    // Add standard categories
    AddCategory(TRunStepInfoTypeInfo.CategoryNameControlFlow);
    AddCategory(TRunStepInfoTypeInfo.CategoryNameFileAndDatabase);
    AddCategory(TRunStepInfoTypeInfo.CategoryNamePipetting);
    AddCategory(TRunStepInfoTypeInfo.CategoryNameStepByStep);
    AddCategory(TRunStepInfoTypeInfo.CategoryNameRack);
    AddCategory(TRunStepInfoTypeInfo.CategoryNameTube);
    AddCategory(TRunStepInfoTypeInfo.CategoryNameDevice);
    AddCategory(TRunStepInfoTypeInfo.CategoryNameDisplay);
    AddCategory(TRunStepInfoTypeInfo.CategoryNameThread);
    AddCategory(TRunStepInfoTypeInfo.CategoryNameLayout);
    AddCategory(TRunStepInfoTypeInfo.CategoryNameBarcode);
    AddCategory(TRunStepInfoTypeInfo.CategoryNameWeighing);

    for xRunStepInfo in fRunStepInfoList do
    begin
        AddStepToCategories(xRunStepInfo);
    end;

    for x := self.CategoryButtons1.Categories.Count - 1 downto 0 do
    begin
        if self.CategoryButtons1.Categories[x].Items.Count = 0 then
        begin
            // remove empty categories in case these actions are hidden
            self.CategoryButtons1.Categories.Delete(x);
            CONTINUE;
        end;
        self.CategoryButtons1.Categories[x].Collapsed := true;
    end;
    {
      if self.CategoryButtons1.Categories.Count > 0 then
      begin
      if self.CategoryButtons1.Categories[0].Items.Count > 0 then
      begin
      self.CategoryButtons1.Categories[0].Collapsed := false;
      self.CategoryButtons1.SelectedItem := self.CategoryButtons1.Categories[0].Items[0];
      end;
      end; }
end;

procedure TfrmMethodStepSelectDialog.LoadMRUItems;
begin
    TControlUtils.AddValuesToListBox(fMRUHistoryList.ToArray, self.lstMRU, true);
    if self.lstMRU.Count > 0 then
    begin
        self.lstMRU.ItemIndex := 0;
        self.MRUSelectedItemChanged();
    end;
end;

procedure TfrmMethodStepSelectDialog.LoadRunStepInfos();
var
    x: integer;
    xTypeInfo: TRunStepInfoTypeInfo;
    xRunStepInfo: TRunStepInfo;
begin
    for x := 0 to fAllowedSteps.Count - 1 do
    begin
        xTypeInfo := TRunStepInfoTypeDictionary.Instance.GetTypeFromTypeName(fAllowedSteps[x])
            as TRunStepInfoTypeInfo;
        xRunStepInfo := xTypeInfo.RunStepInfoCreator.CreateRunStepInfo();
        fRunStepInfoList.Add(xRunStepInfo);
    end;
end;

procedure TfrmMethodStepSelectDialog.MRUSelectedItemChanged();
var
    xStepName: string;
begin
    xStepName := '';
    if lstMRU.ItemIndex >= 0 then
        xStepName := self.lstMRU.Items[self.lstMRU.ItemIndex];

    ChangeSelectedStepName(xStepName, true, true);
end;

procedure TfrmMethodStepSelectDialog.lstMRUClick(Sender: TObject);
begin
    MRUSelectedItemChanged();
end;

procedure TfrmMethodStepSelectDialog.lstMRUDblClick(Sender: TObject);
begin
    MRUSelectedItemChanged();
    self.ModalResult := mrOK;
end;

procedure TfrmMethodStepSelectDialog.lstMRUKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
    if Key = VK_RETURN then
    begin
        MRUSelectedItemChanged();
        self.ModalResult := mrOK;
    end;
end;

procedure TfrmMethodStepSelectDialog.ComboByNameSelectedItemChanged();
var
    xStepName: string;
begin
    xStepName := '';
    if self.cbActionName.ItemIndex >= 0 then
    begin
        xStepName := self.cbActionName.Items[self.cbActionName.ItemIndex];
        ChangeSelectedStepName(xStepName, false, true);
    end;
end;

procedure TfrmMethodStepSelectDialog.cbActionNameKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
    if Key = VK_RETURN then
    begin
        self.ComboByNameSelectedItemChanged();
    end;
end;

procedure TfrmMethodStepSelectDialog.CategorySelectedItemChanged();
var
    xStepName: string;
begin
    xStepName := '';
    if (self.CategoryButtons1.SelectedItem is TButtonItem) then
        if ((self.CategoryButtons1.SelectedItem as TButtonItem).Data <> nil) then
            xStepName := TRunStepInfo((self.CategoryButtons1.SelectedItem as TButtonItem).Data).DefaultName;

    ChangeSelectedStepName(xStepName, true, false);
end;

procedure TfrmMethodStepSelectDialog.CategoryButtons1SelectedItemChange(Sender: TObject;
    const Button: TButtonItem);
begin
    CategorySelectedItemChanged();
end;

procedure TfrmMethodStepSelectDialog.CategoryButtons1MouseDown(Sender: TObject; Button: TMouseButton;
    Shift: TShiftState; X, Y: Integer);
var
    xButtonItem: TButtonItem;
begin
    if not(ssDouble in Shift) then
        EXIT;

    xButtonItem := self.CategoryButtons1.GetButtonAt(X, Y);
    if not Assigned(xButtonItem) then
        EXIT;

    CategorySelectedItemChanged();
    self.ModalResult := mrOK;
end;


end.
