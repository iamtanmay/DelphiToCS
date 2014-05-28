{ ------------------------------------------------------------------------------------------------------------
  Copyright © 2010 Zinsser Analytic GmbH - All rights reserved.
  Author       : Wolfgang Lyncke (wl)
  Description  :
  ------------------------------------------------------------------------------------------------------------
  Revision History:
  date     op  method                                track-no  improvement/change
  -------- --  ------------------------------------  --------- -----------------------------------------------
  06.05.10 wl                                        TN5052    Initial Revision
  07.05.10 wl                                        TN5052    Display-Optionen aus TAppSettings statt IniAccess
  09.06.10 wl                                        TN5116    ViewAllItems ist kein DockableForm mehr
  17.06.10 wl  GetItemTypesByName                    TN5116    TipTypes werden auch bei Methoden-Entwicklung angezeigt
  10.12.10 pk  GetItemTypesByName      		    TN5408    Actions removed from overview for now
  10.02.11 wl  GetItemTypesByName      		    TN5475    Actions werden wieder angezeigt, Layout aber nicht mehr
  11.02.11 wl                                    TN5474   Variablen werden nicht mehr gezeigt
  23.02.11 wl                                    TN5486   neu: ImportViewItems
  01.08.11 wl  GetItemTypesByName                TN5642   SQLTerms werden jetzt auch bei DisplayComponenets mit angezeigt
  14.12.11 wl                                    TN5765   ohne Session
  03.02.12 wl                                    TN5792   neue Namen: SubstanceSet,Substance statt ReagentRack,Reagent
  09.02.12 wl  GetAllowedTypes                   TN5792   Substance-Editor wird auch ohne Sophas-Lizenzgezeigt, wenn UseSubstances = true
  09.05.12 wl  GetAllowedTypes                   TN5458   Bei useLiquids = 0 werden keine Liquid Handling Parameter angezeigt
  ------------------------------------------------------------------------------------------------------------ }

unit ViewItemOverviewManager;


interface


uses
    cxTL,
    StdCtrls,
    Classes,
    Controls,
    Types,
    Generics.Collections,
    CommonTypes,
    ViewItemEditForm,
    ViewItem,
    AppTypes,
    GeneralTypes,
    ViewAllItems;

type
    TViewAllItemsDictionary = class(TDictionary<string, TfrmViewAllItems>);

    TViewItemOverviewManager = class
    private
        fInstances: TViewAllItemsDictionary;

    const
        cMethodDevelopmentName: string = 'MethodDevelopment';

    const
        cLayoutDevelopmentName: string = 'LayoutDevelopment';

    const
        cDeviceDevelopmentName: string = 'DeviceDevelopment';

    const
        cDisplayDevelopmentName: string = 'DisplayDevelopment';

    const
        cImportDevelopmentName: string = 'ImportDevelopment';
        class function GetAllowedTypes(aTypes: TViewItemTypes): TViewItemTypes;
        class function GetFormNameByEditMode(aEditMode: TAppEditMode): string;
        class function GetItemTypesByName(const aFormName: string; out oCaption: string)
            : TViewItemTypes; static;

        function GetInstanceByName(const aFormName: string): TfrmViewAllItems;
    public
        constructor Create();
        destructor Destroy; override;

        function CreateOverviewInstance(aEditMode: TAppEditMode; aNewDockSite: TWinControl): TfrmViewAllItems;

        procedure InstanceUpdateTV(aUpdateModes: TViewItemTypes);
        procedure InstanceAddChildAsFirstOrLast(aNodeType: TViewItemType; const aNodeName: string);
        procedure InstanceDeleteSelectedNode();
        procedure RegisterAllItemsFormDestruction(aForm: TfrmViewAllItems);
    end;


implementation


uses
    Forms,
    AppSettings,
    ViewItemsWorkflow,
    MathUtils;

{ TViewItemOverviewManager }

class function TViewItemOverviewManager.GetItemTypesByName(const aFormName: string; out oCaption: string)
    : TViewItemTypes;
begin
    result := [];
    if aFormName = cMethodDevelopmentName then
    begin
        result := [ntAction, { ntVariable, } ntMethod, ntSequence, ntWashProg, ntSubstance, ntSubstanceSet,
            ntLiquidPar, ntPowderPar, ntSQLTerm, ntTipType];
        oCaption := TLanguageString.Read('Method development', 'Methodenentwicklung');
    end;
    if aFormName = cLayoutDevelopmentName then
    begin
        if TAppSettings.IsDebugMode then
            result := [ntRack, ntCarrier, ntWorkspace, ntLayout, ntTipType]
        else
            result := [ntRack, ntCarrier, ntWorkspace, ntTipType];
        oCaption := TLanguageString.Read('Layout development', 'Layout-Entwicklung');
    end;
    if aFormName = cDeviceDevelopmentName then
    begin
        result := [ntDevice, ntDriver, ntConnection];
        oCaption := TLanguageString.Read('Device development', 'Device-Entwicklung');
    end;
    if aFormName = cImportDevelopmentName then
    begin
        result := [ntVarImportDef, ntTableImportDef, ntImportFileDef];
        oCaption := TLanguageString.Read('Import development', 'Import-Entwicklung');
    end;
    if aFormName = cDisplayDevelopmentName then
    begin
        result := [ntDisplayComponent, ntSQLTerm];
        oCaption := TLanguageString.Read('Display development', 'Display-Entwicklung');
    end;
end;

class function TViewItemOverviewManager.GetAllowedTypes(aTypes: TViewItemTypes): TViewItemTypes;
var
    xUseWashPrograms, xUseSequences, xUseSubstances, xUseSubstanceSets: boolean;
    xUseLiquids: boolean;
    xIniAccess: IWinlissyIniAccess;
begin
    xIniAccess := gCommonDll.CreateAppIni;
    xUseWashPrograms := xIniAccess.ReadBool('Display', 'UseWashPrograms');
    xUseSequences := xIniAccess.ReadBool('Display', 'UseSequences');
    xUseSubstanceSets := xIniAccess.ReadBool('Display', 'UseReagentRacks');
    xUseSubstances := xIniAccess.ReadBool('Display', 'UseSubstances');
    xUseLiquids := xIniAccess.ReadBool('Display', 'UseLiquids');

    result := aTypes;

    if (not TAppSettings.IsRedi) then
    begin
        result := result - [ntPowderPar];
    end;
    if not((TAppSettings.IsSophas) and (xUseSequences)) then
    begin
        result := result - [ntSequence];
    end;
    if not((TAppSettings.IsSophas) and (xUseSubstanceSets)) then
    begin
        result := result - [ntSubstanceSet];
    end;
    if not(xUseSubstances) then
    begin
        result := result - [ntSubstance];
    end;
    if not((TAppSettings.IsSophas) and (xUseWashPrograms)) then
    begin
        result := result - [ntWashProg];
    end;
    if (TAppSettings.IsOneWorkspaceMode) then
    begin
        result := result - [ntWorkspace];
    end;
    if (not TAppSettings.UseCarriers) then
    begin
        result := result - [ntCarrier];
    end;
    if (TAppSettings.IsOneTipTypeMode) then
    begin
        result := result - [ntTipType];
    end;
    if (not TAppSettings.UseDisplayComponents) then
    begin
        result := result - [ntDisplayComponent];
    end;
    if not(xUseLiquids) then
    begin
        result := result - [ntLiquidPar];
    end;
end;

class function TViewItemOverviewManager.GetFormNameByEditMode(aEditMode: TAppEditMode): string;
begin
    case (aEditMode) of
        aemDeviceDevelopment:
            result := cDeviceDevelopmentName;
        aemImportDevelopment:
            result := cImportDevelopmentName;
        aemDisplayDevelopment:
            result := cDisplayDevelopmentName;
        aemMethodDevelopment:
            result := cMethodDevelopmentName;
        aemLayoutDevelopment:
            result := cLayoutDevelopmentName;
        else
            result := 'XXX';
    end;
end;

function TViewItemOverviewManager.GetInstanceByName(const aFormName: string): TfrmViewAllItems;
begin
    if not fInstances.TryGetValue(aFormName, result) then
        result := nil;
end;

function TViewItemOverviewManager.CreateOverviewInstance(aEditMode: TAppEditMode; aNewDockSite: TWinControl)
    : TfrmViewAllItems;
var
    xCaption: string;
    xTypes: TViewItemTypes;
    xFormName: string;
begin
    xFormName := GetFormNameByEditMode(aEditMode);
    xTypes := GetItemTypesByName(xFormName, xCaption);

    result := GetInstanceByName(xFormName);
    if Assigned(result) then
    begin
        EXIT;
    end;

    // neues Fester erzeugen
    result := TfrmViewAllItems.Create(Application, xFormName);
    fInstances.Add(xFormName, result);

    result.CreateTopNodes(GetAllowedTypes(xTypes), xCaption);
    result.UpdateTV([ntAll]);
    result.JustStarted := false;
    TViewItemsWorkflow.ShowInfo(result.cxTreeList1.FocusedNode, result.Memo1);

    result.Parent := aNewDockSite;
    result.Align := alClient;
    result.Visible := true;
end;

constructor TViewItemOverviewManager.Create;
begin
    fInstances := TViewAllItemsDictionary.Create;
end;

destructor TViewItemOverviewManager.Destroy;
begin

    inherited;
end;

procedure TViewItemOverviewManager.InstanceAddChildAsFirstOrLast(aNodeType: TViewItemType;
    const aNodeName: string);
var
    xPair: TPair<string, TfrmViewAllItems>;
begin
    for xPair in fInstances do
    begin
        xPair.Value.AddChildAsLast(aNodeType, aNodeName);
    end;
end;

procedure TViewItemOverviewManager.InstanceDeleteSelectedNode();
var
    xPair: TPair<string, TfrmViewAllItems>;
begin
    for xPair in fInstances do
    begin
        xPair.Value.DeleteSelectedNode();
    end;
end;

procedure TViewItemOverviewManager.InstanceUpdateTV(aUpdateModes: TViewItemTypes);
var
    xPair: TPair<string, TfrmViewAllItems>;
begin
    for xPair in fInstances do
    begin
        xPair.Value.UpdateTV(aUpdateModes);
    end;
end;

procedure TViewItemOverviewManager.RegisterAllItemsFormDestruction(aForm: TfrmViewAllItems);
var
    xPair: TPair<string, TfrmViewAllItems>;
begin
    for xPair in fInstances do
    begin
        if aForm = xPair.Value then
        begin
            fInstances.Remove(xPair.Key);
            EXIT;
        end;
    end;
end;


end.
