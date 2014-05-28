{ --------------------------------------------------------------------------------------------------
  Copyright © 2008 Zinsser Analytic GmbH - All rights reserved.
  Author       : Payman Kamali (pk)
  Description  : Editor for a Workspace type
  --------------------------------------------------------------------------------------------------
  Revision History:
  date     op  method                       track-no improvement/change
  -------- --  ---------------------------  -------- -----------------------------------------------
  23.06.08 pk                                TN4139  Initial Revision
  27.06.08 pk  MakeDefaultWorkspaceRec       TN4139  UseAllArms set to false
  11.07.08 wl                                TN4164    Struktur geändert, damit auch im Designer verwendbar
  11.07.08 wl                                TN4164    Devices statt nur Arms
  16.07.08 wl                                TN4164    now it works
  26.08.08 wl                                TN4164    zeigt jetzt die Device-Hirarchie
  11.03.09 pk  WorkspaceRecDevicesToGUI      TN4459    Clear the Used list before adding
  31.08.09 pk                                TN4753    Workspace-Devices commented out
  04.11.09 pk                                TN4843    Replace TObjectList, TStringList, TStrings with Generics Lists and TStringArray
  11.11.09 pk                                TN4856    uses settingsmanager instead of devicemanager
  17.03.10 wl                                TN5031   uses GenericTree
  05.05.10 ts  ShowDeviceAsUsedOrUnused      TN5081    Devices werden nur in tvAvailable eingefügt, wenn sie da noch nicht drin stehen
  20.05.10 wl                               TN5117   uses ControlUtils
  10.02.11 wl                               TN5475   TfraCoordSystemRelation ist TForm statt TFrame
  21.07.11 wl  FormCreate                   TN5614   User-Verwaltung greift jetzt
  08.09.11 ts  WorkspaceRecRelationsFromGUI TN5686   vWorldRelationRec.Valid muss true sein, sonst werden Werte nicht gespeichert/geladen
  10.04.13 wl                               TN6045   uses Generics.Collections
  -------------------------------------------------------------------------------------------------- }

unit WorkspaceEditor;


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
    ComCtrls,
    StdCtrls,
    Buttons,
    Workspace,
    WorkspaceDataAdaptor,
    WorkspaceDevicesDataAdaptor,
    GeneralTypes,
    CoordSystemRelationSubEditor,
    CoordSystemMath,
    SceneGraphics,
    IntfDevice;

type
    TfrmWorkspaceEditor = class(TForm)
        PageControl1: TPageControl;
        Panel2x: TPanel;
        TabSheet1: TTabSheet;
        GroupBox7: TGroupBox;
        Shape1: TShape;
        btnColor: TButton;
        RackTyp: TGroupBox;
        Label11: TLabel;
        gbAbmessungen: TGroupBox;
        Label1: TLabel;
        Label2: TLabel;
        Label3: TLabel;
        edTypeName: TEdit;
        edSizeX: TEdit;
        edSizeY: TEdit;
        edSizeZ: TEdit;
        ColorDialog1: TColorDialog;
        Panel3: TPanel;
        btnApply: TButton;
        btnOK: TButton;
        btnCancel: TButton;
        TabSheet2: TTabSheet;
        TabSheet3: TTabSheet;
        TabSheet4: TTabSheet;
        rdoViewType: TRadioGroup;
        grpArmSelection: TGroupBox;
        Label4: TLabel;
        btnAddArm: TSpeedButton;
        btnRemoveArm: TSpeedButton;
        Label5: TLabel;
        rdoArmSelectionMode: TRadioGroup;
        tvAvailableDevices: TTreeView;
        tvUsedDevices: TTreeView;
        procedure btnColorClick(Sender: TObject);
        procedure btnAddArmClick(Sender: TObject);
        procedure btnRemoveArmClick(Sender: TObject);
        procedure FormCreate(Sender: TObject);
        procedure btnOKClick(Sender: TObject);
        procedure btnCancelClick(Sender: TObject);
        procedure btnApplyClick(Sender: TObject);
        procedure rdoViewTypeClick(Sender: TObject);
        procedure FormDestroy(Sender: TObject);
        procedure rdoArmSelectionModeClick(Sender: TObject);
        procedure FormShow(Sender: TObject);
    private
        fWorkspace: TWorkspace;
        fViewCoordSystemRelation, fWorldCoordSystemRelation: TCoordSystemRelation;
        fraViewCoordSystemRelation: TfraCoordSystemRelation;
        fraWorldCoordSystemRelation: TfraCoordSystemRelation;
        fUseCustomView: boolean;
        fUseAllDevices: boolean;
        fTypeID: integer;
        fSceneGraphics: TSceneGraphics;
        fCurrentName: string;
        procedure WorkspaceRecDevicesToGUI(const aDevices: TWorkspaceDevicesRecArray);
        procedure WorkspaceRecToGUI(const aWorkspaceRec: TWorkspaceRec;
            const aWorkspaceDevicesRec: TWorkspaceDevicesRecArray);
        procedure WorkspaceRecRelationsToGUI(const aViewRelationRec, aWorldRelationRec
            : TCoordSystemRelationRec);
        procedure SetUseCustomView(aValue: boolean);
        procedure WorkspaceRecFromGUI(out oWorkspaceRec: TWorkspaceRec;
            out oWorkspaceDevicesRec: TWorkspaceDevicesRecArray);
        procedure WorkspaceRecRelationsFromGUI(var vViewRelationRec, vWorldRelationRec
            : TCoordSystemRelationRec);
        procedure WorkspaceRecDevicesFromGUI(const aWorkspaceID: integer;
            out oDevices: TWorkspaceDevicesRecArray);
        class function MakeDefaultWorkspaceRec(out oDevices: TWorkspaceDevicesRecArray): TWorkspaceRec;
        procedure DisplayWorkspace;
        function SaveChanges: boolean;
        procedure SetDeviceSelectionMode(aUseAllDevices: boolean);
        procedure EnabledAsParent(container: TWinControl);
        procedure LoadDataToGUI(const aTypeName: string);
        procedure ShowDeviceAsUsedOrUnused(const aCurrentDeviceName: string;
            const aDevices: TWorkspaceDevicesRecArray);
    public
        property CurrentName: string read fCurrentName write fCurrentName;

        class function InstanceShowToAdd(out oName: string): boolean;
        class function InstanceShowToEdit(const aName: string): boolean;
    end;


implementation


{$R *.dfm}

uses
    Generics.Collections,
    ControlUtils,
    AppSettings,
    CommonTypes,
    UtilLib,
    DeviceSettingsManager,
    DialogUtils,
    GenericTree;

const
    cDefaultViewItemIndex = 0;
    cCustomViewItemIndex = 1;

    cDeviceSelectionAllItemIndex = 0;
    cDeviceSelectionCustomItemIndex = 1;

class function TfrmWorkspaceEditor.MakeDefaultWorkspaceRec(out oDevices: TWorkspaceDevicesRecArray)
    : TWorkspaceRec;
begin
    result.ID := TWorkspaceDataAdaptor.GetIDDefault();
    result.Name := '';
    result.ViewRelation := TWorkspaceUtils.MakeDefCoordSystemRelationRec();
    result.WorldRelation := TWorkspaceUtils.MakeDefCoordSystemRelationRec();
    result.WorldRelation.Valid := true;
    result.X := 0;
    result.Y := 0;
    result.Z := 0;
    result.UseAllDevices := true;
    result.Color := 0;
    SetLength(oDevices, 0);
end;

procedure TfrmWorkspaceEditor.FormCreate(Sender: TObject);
begin
    fraViewCoordSystemRelation := TfraCoordSystemRelation.Create(nil);
    fraWorldCoordSystemRelation := TfraCoordSystemRelation.Create(nil);

    fViewCoordSystemRelation := TCoordSystemRelation.Create();
    fWorldCoordSystemRelation := TCoordSystemRelation.Create();

    self.fraViewCoordSystemRelation.CoordSystemRelation := fViewCoordSystemRelation;
    self.fraWorldCoordSystemRelation.CoordSystemRelation := fWorldCoordSystemRelation;

    fSceneGraphics := TSceneGraphics.Create(Panel2x);
    fSceneGraphics.Visible := true;

    btnOK.Visible := gCommonDll.LevelIsIncluded(gCommonDll.CurrentUser.Level, usrSystemAdmin);
    btnApply.Visible := gCommonDll.LevelIsIncluded(gCommonDll.CurrentUser.Level, usrSystemAdmin);
end;

procedure TfrmWorkspaceEditor.FormDestroy(Sender: TObject);
begin
    FreeAndNil(fSceneGraphics);
    FreeAndNil(fViewCoordSystemRelation);
    FreeAndNil(fWorldCoordSystemRelation);
    FreeAndNil(fraViewCoordSystemRelation);
    FreeAndNil(fraWorldCoordSystemRelation);
end;

procedure TfrmWorkspaceEditor.FormShow(Sender: TObject);
begin
    PageControl1.ActivePageIndex := 0;
    LoadDataToGUI(fCurrentName);

    fraViewCoordSystemRelation.Parent := TabSheet2;
    fraViewCoordSystemRelation.Visible := true;
    fraViewCoordSystemRelation.Left := 124;

    fraWorldCoordSystemRelation.Parent := TabSheet4;
    fraWorldCoordSystemRelation.Visible := true;
    fraWorldCoordSystemRelation.Left := 2;
end;

procedure TfrmWorkspaceEditor.LoadDataToGUI(const aTypeName: string);
var
    xTypeFound: boolean;
    xWorkspaceRec: TWorkspaceRec;
    xID: integer;
    xDevices: TWorkspaceDevicesRecArray;
    xWDDA: TWorkspaceDevicesDataAdaptor;
    xDA: TWorkspaceDataAdaptor;
begin

    xTypeFound := false;
    if aTypeName <> '' then
    begin
        xDA := TWorkspaceDataAdaptor.Create;
        try
            ASSERT(xDA.ReadIDByName(aTypeName, xID));
            xDA.ReadRecByID(xID, xWorkspaceRec);
        finally
            FreeAndNil(xDA);
        end;

        xWDDA := TWorkspaceDevicesDataAdaptor.Create;
        try
            xWDDA.ReadByWorkspaceID(xID, xDevices);
        finally
            xWDDA.Free;
        end;
        xTypeFound := true;
    end;

    if not xTypeFound then
    begin
        xWorkspaceRec := MakeDefaultWorkspaceRec(xDevices);
        xWorkspaceRec.Name := aTypeName;
        self.Caption := 'Workspace Editor';
    end
    else
    begin
        self.Caption := 'Workspace: ' + aTypeName;
    end;

    self.edTypeName.ReadOnly := xTypeFound;
    self.WorkspaceRecToGUI(xWorkspaceRec, xDevices);
end;

procedure TfrmWorkspaceEditor.btnColorClick(Sender: TObject);
begin
    if (ColorDialog1.Execute) then
        Shape1.Brush.Color := ColorDialog1.Color;
end;

procedure TfrmWorkspaceEditor.btnAddArmClick(Sender: TObject);
var
    xDeviceName: string;
    xTree: TStringTree;
begin
    if not Assigned(tvAvailableDevices.Selected) then
        EXIT;
    if (self.tvAvailableDevices.Selected.Level <> 0) then
        EXIT; // keine Subdevices

    xDeviceName := tvAvailableDevices.Selected.Text;
    if not TDeviceSettingsManager.Instance.ModuleExists(xDeviceName) then
        EXIT;
    if not TDeviceSettingsManager.Instance.IsDeviceIndependent(xDeviceName) then
        EXIT;

    xTree := TStringTree.Create();
    try
        TDeviceSettingsManager.Instance.ShowDeviceWithDependentDevices(xDeviceName, xTree);
        TControlUtils.AddTreeNodeListToTreeNodes(xTree, self.tvUsedDevices.Items);
    finally
        FreeAndNil(xTree);
    end;

    tvAvailableDevices.Selected.Delete();

end;

procedure TfrmWorkspaceEditor.btnRemoveArmClick(Sender: TObject);
var
    xDeviceName: string;
    xTree: TStringTree;
begin
    if not Assigned(tvUsedDevices.Selected) then
        EXIT;
    if (self.tvUsedDevices.Selected.Level <> 0) then
        EXIT; // keine Subdevices

    xDeviceName := tvUsedDevices.Selected.Text;
    if not TDeviceSettingsManager.Instance.ModuleExists(xDeviceName) then
        EXIT;
    if not TDeviceSettingsManager.Instance.IsDeviceIndependent(xDeviceName) then
        EXIT;

    tvUsedDevices.Selected.Delete();

    xTree := TStringTree.Create();
    try
        TDeviceSettingsManager.Instance.ShowDeviceWithDependentDevices(xDeviceName, xTree);
        TControlUtils.AddTreeNodeListToTreeNodes(xTree, self.tvAvailableDevices.Items);
    finally
        FreeAndNil(xTree);
    end;
end;

procedure TfrmWorkspaceEditor.EnabledAsParent(container: TWinControl);
var
    index: integer;
    aControl: TControl;
    isContainer: boolean;
begin
    for index := 0 to -1 + container.ControlCount do
    begin
        aControl := container.Controls[index];

        aControl.Enabled := container.Enabled;

        isContainer := (csAcceptsControls in container.Controls[index].ControlStyle);

        if (isContainer) and (aControl is TWinControl) then
        begin
            // recursive for child controls
            EnabledAsParent(TWinControl(container.Controls[index]));
        end;
    end;
end;

procedure TfrmWorkspaceEditor.SetDeviceSelectionMode(aUseAllDevices: boolean);
begin
    fUseAllDevices := aUseAllDevices;
    self.grpArmSelection.Enabled := not aUseAllDevices;
    EnabledAsParent(self.grpArmSelection);
    if aUseAllDevices then
        self.rdoArmSelectionMode.ItemIndex := cDeviceSelectionAllItemIndex
    else
        self.rdoArmSelectionMode.ItemIndex := cDeviceSelectionCustomItemIndex
end;

procedure TfrmWorkspaceEditor.ShowDeviceAsUsedOrUnused(const aCurrentDeviceName: string;
    const aDevices: TWorkspaceDevicesRecArray);
var
    x: integer;
    xTree: TStringTree;
begin
    xTree := TStringTree.Create();
    try
        for x := 0 to high(aDevices) do
        begin
            if (aCurrentDeviceName = aDevices[x].DeviceName) then
            begin
                TDeviceSettingsManager.Instance.ShowDeviceWithDependentDevices(aCurrentDeviceName, xTree);
                TControlUtils.AddTreeNodeListToTreeNodes(xTree, self.tvUsedDevices.Items);
                EXIT;
            end;
        end;

        TDeviceSettingsManager.Instance.ShowDeviceWithDependentDevices(aCurrentDeviceName, xTree);
        for x := 0 to self.tvAvailableDevices.Items.Count - 1 do
            if aCurrentDeviceName = self.tvAvailableDevices.Items[x].Text then
                EXIT;

        TControlUtils.AddTreeNodeListToTreeNodes(xTree, self.tvAvailableDevices.Items);
    finally
        xTree.Free;
    end;
end;

procedure TfrmWorkspaceEditor.WorkspaceRecDevicesToGUI(const aDevices: TWorkspaceDevicesRecArray);
var
    x: integer;
    xIndependentDeviceNames: TList<string>;
begin
    self.tvUsedDevices.Items.Clear();

    xIndependentDeviceNames := TList<string>.Create();
    try
        TDeviceSettingsManager.Instance.GetAllIndependentDeviceNames(xIndependentDeviceNames);
        for x := 0 to xIndependentDeviceNames.Count - 1 do
        begin
            ShowDeviceAsUsedOrUnused(xIndependentDeviceNames[x], aDevices);
        end;
    finally
        xIndependentDeviceNames.Free;
    end;
end;

procedure TfrmWorkspaceEditor.SetUseCustomView(aValue: boolean);
begin
    fUseCustomView := aValue;
    self.fraViewCoordSystemRelation.ChangeEnabled(fUseCustomView);
    if aValue then
        self.rdoViewType.ItemIndex := cCustomViewItemIndex
    else
        self.rdoViewType.ItemIndex := cDefaultViewItemIndex
end;

procedure TfrmWorkspaceEditor.WorkspaceRecRelationsToGUI(const aViewRelationRec,
    aWorldRelationRec: TCoordSystemRelationRec);
var
    xViewRelationRec: TCoordSystemRelationRec;
    xUseCustomView: boolean;
begin

    // view relation
    xUseCustomView := aViewRelationRec.Valid;
    if xUseCustomView then
    begin
        xViewRelationRec := aViewRelationRec;
    end
    else
    begin
        xViewRelationRec := aWorldRelationRec;
    end;

    TWorkspace.AssignCoordSystemRelationFromRec(fViewCoordSystemRelation, xViewRelationRec);
    self.fraViewCoordSystemRelation.CoordSystemRelationToGUI();
    SetUseCustomView(xUseCustomView);

    // world relation
    TWorkspace.AssignCoordSystemRelationFromRec(fWorldCoordSystemRelation, aWorldRelationRec);
    self.fraWorldCoordSystemRelation.CoordSystemRelationToGUI();

end;

procedure TfrmWorkspaceEditor.WorkspaceRecToGUI(const aWorkspaceRec: TWorkspaceRec;
    const aWorkspaceDevicesRec: TWorkspaceDevicesRecArray);
begin
    fTypeID := aWorkspaceRec.ID;

    WorkspaceRecRelationsToGUI(aWorkspaceRec.ViewRelation, aWorkspaceRec.WorldRelation);
    self.SetDeviceSelectionMode(aWorkspaceRec.UseAllDevices);
    WorkspaceRecDevicesToGUI(aWorkspaceDevicesRec);

    self.edTypeName.Text := aWorkspaceRec.Name;
    self.edSizeX.Text := FloatToStr(aWorkspaceRec.X);
    self.edSizeY.Text := FloatToStr(aWorkspaceRec.Y);
    self.edSizeZ.Text := FloatToStr(aWorkspaceRec.Z);
    self.Shape1.Brush.Color := aWorkspaceRec.Color;

    self.DisplayWorkspace();
end;

procedure TfrmWorkspaceEditor.WorkspaceRecDevicesFromGUI(const aWorkspaceID: integer;
    out oDevices: TWorkspaceDevicesRecArray);
var
    x: integer;
    xDeviceList: TList<string>;
begin
    xDeviceList := TList<string>.Create;
    try
        // Devicenamen aus Treeview lesen
        for x := 0 to self.tvUsedDevices.Items.Count - 1 do
        begin
            if (self.tvUsedDevices.Items[x].Level <> 0) then
                CONTINUE; // keine Subdevices

            xDeviceList.Add(self.tvUsedDevices.Items[x].Text);
        end;

        // In Array schreiben
        SetLength(oDevices, xDeviceList.Count);
        for x := 0 to xDeviceList.Count - 1 do
        begin
            oDevices[x].WorkspaceID := aWorkspaceID;
            oDevices[x].DeviceName := xDeviceList[x];
        end;

    finally
        FreeAndNil(xDeviceList);
    end;
end;

procedure TfrmWorkspaceEditor.WorkspaceRecRelationsFromGUI(var vViewRelationRec,
    vWorldRelationRec: TCoordSystemRelationRec);
begin

    // view relation
    vViewRelationRec.Valid := fUseCustomView;
    if vViewRelationRec.Valid then
    begin
        self.fraViewCoordSystemRelation.CoordSystemRelationFromGUI();
        TWorkspace.AssignCoordSystemRelationToRec(fViewCoordSystemRelation, vViewRelationRec);
    end;

    // world relation
    vWorldRelationRec.Valid := true;
    self.fraWorldCoordSystemRelation.CoordSystemRelationFromGUI();
    TWorkspace.AssignCoordSystemRelationToRec(fWorldCoordSystemRelation, vWorldRelationRec);

end;

procedure TfrmWorkspaceEditor.WorkspaceRecFromGUI(out oWorkspaceRec: TWorkspaceRec;
    out oWorkspaceDevicesRec: TWorkspaceDevicesRecArray);
begin
    oWorkspaceRec.ID := fTypeID;

    WorkspaceRecRelationsFromGUI(oWorkspaceRec.ViewRelation, oWorkspaceRec.WorldRelation);
    oWorkspaceRec.UseAllDevices := self.rdoArmSelectionMode.ItemIndex = cDeviceSelectionAllItemIndex;
    WorkspaceRecDevicesFromGUI(fTypeID, oWorkspaceDevicesRec);

    oWorkspaceRec.Name := self.edTypeName.Text;
    oWorkspaceRec.X := StrToFloat(self.edSizeX.Text);
    oWorkspaceRec.Y := StrToFloat(self.edSizeY.Text);
    oWorkspaceRec.Z := StrToFloat(self.edSizeZ.Text);
    oWorkspaceRec.Color := self.Shape1.Brush.Color;
end;

function TfrmWorkspaceEditor.SaveChanges: boolean;
var
    xTypeName: string;
    xWorkspaceRec: TWorkspaceRec;
    xWorkspaceDevicesRec: TWorkspaceDevicesRecArray;
    xWDDA: TWorkspaceDevicesDataAdaptor;
    xDA: TWorkspaceDataAdaptor;
begin
    result := false;
    xTypeName := self.edTypeName.Text;
    if (xTypeName = '') then
        EXIT;

    self.WorkspaceRecFromGUI(xWorkspaceRec, xWorkspaceDevicesRec);

    xDA := TWorkspaceDataAdaptor.Create;
    try
        if (fTypeID = TWorkspaceDataAdaptor.GetIDDefault) then
        begin
            if not TAppSettings.ItemConfirmAdd('Workspace', xTypeName, '') then
                EXIT;
            fTypeID := xDA.FindNextAvailID();
            xWorkspaceRec.ID := fTypeID;
            self.edTypeName.ReadOnly := true;
        end
        else
        begin
            if not TAppSettings.ItemConfirmEdit('Workspace', xTypeName) then
                EXIT;
        end;

        xDA.WriteRec(xWorkspaceRec);
    finally
        FreeAndNil(xDA);
    end;

    xWDDA := TWorkspaceDevicesDataAdaptor.Create;
    try
        xWDDA.WriteByWorkspaceID(xWorkspaceRec.ID, xWorkspaceDevicesRec);
    finally
        xWDDA.Free;
    end;

    fCurrentName := xTypeName;

    result := true;
end;

procedure TfrmWorkspaceEditor.btnOKClick(Sender: TObject);
begin
    if (SaveChanges) then
        ModalResult := mrOK;
end;

procedure TfrmWorkspaceEditor.btnCancelClick(Sender: TObject);
begin
    ModalResult := mrCancel;
end;

procedure TfrmWorkspaceEditor.DisplayWorkspace();
var
    xWorkspaceRec: TWorkspaceRec;
    xWorkspaceDevicesRec: TWorkspaceDevicesRecArray;
begin

    FreeAndNil(fWorkspace);
    // Carrier neu erzeugen
    fWorkspace := TWorkspace.Create();
    fWorkspace.InitGraphics();
    fWorkspace.AssignGraphicsParent(fSceneGraphics);
    self.WorkspaceRecFromGUI(xWorkspaceRec, xWorkspaceDevicesRec);
    fWorkspace.SetType(xWorkspaceRec, xWorkspaceDevicesRec);
    // fCarrier.Graphics.CoordSystem.TranslateX := 20;
    // fCarrier.Graphics.CoordSystem.TranslateY := 50;
    fWorkspace.Visible := true;
    fSceneGraphics.SceneChanged();
end;

procedure TfrmWorkspaceEditor.btnApplyClick(Sender: TObject);
begin
    if (SaveChanges) then
    begin
        self.LoadDataToGUI(fCurrentName);
        DisplayWorkspace;
        // FApplied := true;
        // btnApply.Enabled := false;
    end;
end;

procedure TfrmWorkspaceEditor.rdoViewTypeClick(Sender: TObject);
begin
    SetUseCustomView(self.rdoViewType.ItemIndex = cCustomViewItemIndex);
end;

procedure TfrmWorkspaceEditor.rdoArmSelectionModeClick(Sender: TObject);
begin
    SetDeviceSelectionMode(self.rdoArmSelectionMode.ItemIndex = cDeviceSelectionAllItemIndex);
end;

class function TfrmWorkspaceEditor.InstanceShowToAdd(out oName: string): boolean;
var
    xInstance: TfrmWorkspaceEditor;
begin
    oName := '';
    result := false;
    xInstance := TfrmWorkspaceEditor.Create(nil);
    try
        xInstance.CurrentName := '';
        if (xInstance.ShowModal = mrOK) then
        begin
            result := true;
            oName := xInstance.CurrentName;
        end;
    finally
        xInstance.Free;
    end;
end;

class function TfrmWorkspaceEditor.InstanceShowToEdit(const aName: string): boolean;
var
    xInstance: TfrmWorkspaceEditor;
begin
    result := false;
    xInstance := TfrmWorkspaceEditor.Create(nil);
    try
        xInstance.CurrentName := aName;
        if (xInstance.ShowModal = mrOK) then
        begin
            result := true;
        end;
    finally
        xInstance.Free;
    end;
end;


end.
