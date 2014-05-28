{ ------------------------------------------------------------------------------------------------------------
  Copyright  2009 Zinsser Analytic GmbH - All rights reserved.
  Author       : Wolfgang Lyncke (wl)
  Description  :
  ------------------------------------------------------------------------------------------------------------
  Revision History:
  date     op  method                            track-no improvement/change
  -------- --  --------------------------------  -------- ----------------------------------------------------
  16.01.09 wl                                    TN4362   special View Items extracted from ViewItems.pas
  04.03.09 pk  TSQLTermViewItem                  TN4446   New CreateNewName
  06.04.09 pk                                    TN4503   New: ntDisplayComponent
  16.06.09 wl                                    TN4606   alle Bezüge auf ntRunTable und RunEditor entfernt
  10.08.09 wl                                    TN4702   Strings werden jetzt direkt geladen
  11.08.09 wl  TActionViewItem.ReadAllItems      TN4702   Funktion auch für Actions hinzugefügt
  21.09.09 pk                                    TN4788   New ReadAllNames for TRackViewItem, TCarrierViewItem, TWorkspaceViewItem
  04.11.09 pk                                  	TN4843   Replace TObjectList, TStringList, TStrings with Generics Lists and TStringArray
  13.04.10 wl                                    TN5044   uses geändert
  15.04.10 ts  TActionViewItem.ReadAllNames      TN5054   if action names are added to Setting APPLICATION-ACTIONS-ALLOWED, only these available in ZADesigner
  23.04.10 wl  TLayoutViewItem.CreateEditForm    TN5070   neu für LayoutEditor
  03.05.10 wl  TFolderViewItem                   TN5052   --> ViewItem
  03.05.10 wl  TRack,Carrier,..ViewItem          TN5052   --> LayoutEditingViewItem
  07.05.10 pk  TVariableViewItem.AddInfos        TN5092   CreateWithReaderwritersettings no longer needs type parameter
  14.12.11 wl                                    TN5765   ohne Session
  03.02.12 wl                                    TN5792   neue Namen: SubstanceSet,Substance statt ReagentRack,Reagent
  20.02.12 wl  TSubstanceSetViewItem             TN5812   Grundfunktionalität (New, Delete, SaveAs) eingebaut
  13.11.12 wl  TDisplayComponentViewItem         TN6015   überarbeitet für DisplayComponentsDataAdaptor
  14.11.12 wl  TSubstanceSetViewItem.ReadAllNames TN6015  selbstgestrickte Funktion entfernt
  10.12.12 wl                                    TN6045   uses ZADesignObjects entfernt
  13.12.12 wl  TActionViewItem.ReadAllNames      TN6054   AllowedActions-Mechanismus entfernt
  14.12.12 wl  TVariableViewItem                 TN6054   entfernt
  14.03.13 wl                                    TN5960   Erweiterungen, um im Runner die richtigen Menüpunkte zu zeigen
  11.04.13 wl                                    TN6045   uses geändert
  30.08.13 wl  TActionViewItem.AddInfos          TN6236   angepasst
  30.08.13 wl                                    TN6237   verwendet TLayoutWorkspaceDataAdaptor.InstReadAllNames statt LayoutDataAdaptor
  ------------------------------------------------------------------------------------------------------------ }

unit SpecialViewItems;


interface


uses
    Classes,
    QueryDataAdaptor,
    ViewItem,
    DockableForm;

type
    // View Items

    TActionViewItem = class(TViewItem)
    protected
        function GetItemType(): TViewItemType; override;
        function GetTypeCaption(): string; override;
    public
        function AddInfos: TArray<string>; override;
        function ReadAllNames: TArray<string>; override;
        function GetIndividualIconIndex(): integer; override;
    end;

    TLayoutViewItem = class(TViewItem)
    protected
        function GetItemType(): TViewItemType; override;
        function GetTypeCaption(): string; override;
        procedure DeleteNamePhysically(); override;
    public
        function ReadAllNames: TArray<string>; override;
        function CreateEditForm(aOwner: TComponent; aOnSaveStatusChanged: TNotifyEvent)
            : TDockableEditForm; override;
    end;

    // Database View Items

    TPipetteParamViewItem = class(TDatasetViewItem)
    protected
        function ConfirmDeleteName(): boolean; override;
        procedure DeleteNameFromMemory(); override;
        function ItemNameExistsIntern(var vName: string; aCaseSensitive, aGoodResult: boolean)
            : boolean; override;
    public
        procedure AddName(const aName: string); override;
    end;

    TLiquidParViewItem = class(TPipetteParamViewItem)
    protected
        function GetItemType(): TViewItemType; override;
        function GetTypeCaption(): string; override;
        function CreateDataAdaptor(): TQueryDataAdaptor; override;
    public
        function ReadAllNames: TArray<string>; override;
        procedure CreateNewName(const aNewName: string); override;
        function CreateEditForm(aOwner: TComponent; aOnSaveStatusChanged: TNotifyEvent)
            : TDockableEditForm; override;
    end;

    TPowderParViewItem = class(TPipetteParamViewItem)
    protected
        function GetItemType(): TViewItemType; override;
        function GetTypeCaption(): string; override;
        function CreateDataAdaptor(): TQueryDataAdaptor; override;
    public
        function ReadAllNames: TArray<string>; override;
        procedure CreateNewName(const aNewName: string); override;
        function CreateEditForm(aOwner: TComponent; aOnSaveStatusChanged: TNotifyEvent)
            : TDockableEditForm; override;
    end;

    TSequenceViewItem = class(TDatasetViewItem)
    protected
        function GetItemType(): TViewItemType; override;
        function GetTypeCaption(): string; override;
        function CreateDataAdaptor: TQueryDataAdaptor; override;
    public
        function AskNewName(var vNewName: string): boolean; override;
        function CreateEditForm(aOwner: TComponent; aOnSaveStatusChanged: TNotifyEvent)
            : TDockableEditForm; override;
    end;

    TWashProgViewItem = class(TDatasetViewItem)
    protected
        function GetItemType(): TViewItemType; override;
        function GetTypeCaption(): string; override;
        function CreateDataAdaptor(): TQueryDataAdaptor; override;
    public
        procedure CreateNewName(const aNewName: string); override;
        function CreateEditForm(aOwner: TComponent; aOnSaveStatusChanged: TNotifyEvent)
            : TDockableEditForm; override;
    end;

    TReagentViewItem = class(TDatasetViewItem)
    protected
        function GetItemType(): TViewItemType; override;
        function GetTypeCaption(): string; override;
        function CreateDataAdaptor(): TQueryDataAdaptor; override;
    public
        function ReadAllNames: TArray<string>; override;
        function CreateEditForm(aOwner: TComponent; aOnSaveStatusChanged: TNotifyEvent)
            : TDockableEditForm; override;
    end;

    TSubstanceSetViewItem = class(TDatasetViewItem)
    protected
        function GetItemType(): TViewItemType; override;
        function GetTypeCaption(): string; override;
        function CreateDataAdaptor(): TQueryDataAdaptor; override;
    public
        function CreateEditForm(aOwner: TComponent; aOnSaveStatusChanged: TNotifyEvent)
            : TDockableEditForm; override;
    end;

    TSQLTermViewItem = class(TDatasetViewItem)
    protected
        function GetItemType(): TViewItemType; override;
        function GetTypeCaption(): string; override;
        function ItemNameExistsIntern(var vName: string; aCaseSensitive, aGoodResult: boolean)
            : boolean; override;
        function CreateDataAdaptor(): TQueryDataAdaptor; override;
    public
        function CreateEditForm(aOwner: TComponent; aOnSaveStatusChanged: TNotifyEvent)
            : TDockableEditForm; override;
        procedure CreateNewName(const aNewName: string); override;
    end;

    TDisplayComponentViewItem = class(TDatasetViewItem)
    protected
        function CreateDataAdaptor(): TQueryDataAdaptor; override;
        function GetTypeCaption(): string; override;
        function GetItemType(): TViewItemType; override;
    public
        function CreateEditForm(aOwner: TComponent; aOnSaveStatusChanged: TNotifyEvent)
            : TDockableEditForm; override;
    end;


implementation


uses
    Windows,
    SysUtils,
    GeneralTypes,
    DisplayComponentsDataAdaptor,
    GUIManager,
    RunStepBuilderTypeDictionary,
    SeqenceEditor,
    WashProgramEditor,
    LiquidParamEditor,
    PowderParamEditor,
    SQLTermEditor,
    RunStepInfoFactory,
    LiqHDataCache,
    ReagentEditor,
    TubeEditor,
    LayoutDataAdaptorExt,
    LayoutWorkspaceDataAdaptor,
    SeqenceManager,
    SequenceDataAdaptor,
    SQLTermsDataAdaptor,
    RunStepInfoTypeDictionary,
    WashprogDataAdaptor,
    LiqHDataAdaptor,
    LiqHDataAdaptorExt,
    LayoutEditor,
    DisplayComponentEditor,
    SubstanceSetDataAdaptor,
    SubstanceDataDataAdaptor;

{ TActionViewItem }

function TActionViewItem.AddInfos: TArray<string>;
var
    xCommentShort, xCommentLong: string;
begin
    SetLength(result, 3);
    result[0] := self.FullCaption;

    TRunStepBuilderTypeDictionary.Instance.GetActionDescription(self.Name, xCommentShort, xCommentLong);
    result[1] := xCommentShort;
    result[2] := xCommentLong;
end;

function TActionViewItem.GetIndividualIconIndex: integer;
begin
    EXIT(TRunStepInfoFactory.GetIconIndex(self.name));
end;

function TActionViewItem.GetItemType: TViewItemType;
begin
    result := ntAction;
end;

function TActionViewItem.GetTypeCaption: string;
begin
    result := 'Action';
end;

function TActionViewItem.ReadAllNames: TArray<string>;
begin
    EXIT(TRunStepInfoTypeDictionary.Instance.GetAllActions());
end;

{ TLayoutViewItem }

function TLayoutViewItem.GetTypeCaption(): string;
begin
    result := 'Layout';
end;

function TLayoutViewItem.CreateEditForm(aOwner: TComponent; aOnSaveStatusChanged: TNotifyEvent)
    : TDockableEditForm;
begin
    result := TfrmLayoutEditor.Create(aOwner, self.Name, aOnSaveStatusChanged);
end;

procedure TLayoutViewItem.DeleteNamePhysically();
var
    xDA: TLayoutDataAdaptorExt;
begin
    xDA := TLayoutDataAdaptorExt.Create;
    try
        xDA.DeleteName(self.Name);
    finally
        xDA.Free;
    end;
end;

function TLayoutViewItem.GetItemType: TViewItemType;
begin
    result := ntLayout;
end;

function TLayoutViewItem.ReadAllNames(): TArray<string>;
begin
    EXIT(TLayoutWorkspaceDataAdaptor.InstReadAllNames());
end;

{ TPipetteParamViewItem }

function TPipetteParamViewItem.ConfirmDeleteName(): boolean;
begin
    if (UpperCase(self.Name) = 'DEFAULT') then
    begin
        gGUIManager.MessageBox(TLanguageString.Read('DEFAULT setting can not be deleted!',
            'Die DEFAULT-Einstellung kann nicht gelöscht werden!'), TLanguageString.Read('Error', 'Fehler'),
            MB_ICONSTOP);
        result := false;
        EXIT;
    end;

    result := inherited ConfirmDeleteName();
end;

procedure TPipetteParamViewItem.AddName(const aName: string);
begin
    TLiqHDataCache.Instance.AddLiqHName(aName);
end;

procedure TPipetteParamViewItem.DeleteNameFromMemory();
begin
    TLiqHDataCache.Instance.DeleteLiqHName(self.Name);
end;

function TPipetteParamViewItem.ItemNameExistsIntern(var vName: string; aCaseSensitive: boolean;
    aGoodResult: boolean): boolean;
var
    xDataAdaptor: TLiqHDataAdaptor;
begin
    xDataAdaptor := TLiqHDataAdaptor.Create;
    try
        result := xDataAdaptor.LiqParamNameExists(vName, aCaseSensitive);
    finally
        xDataAdaptor.Free;
    end;
end;

{ TLiquidParViewItem }

function TLiquidParViewItem.GetTypeCaption(): string;
begin
    result := TLanguageString.Read('Liquid handling parameter', 'Liquid Handling-Parameter');
end;

function TLiquidParViewItem.ReadAllNames(): TArray<string>;
var
    xDataAdaptor: TLiquidParDataAdaptor;
begin
    xDataAdaptor := TLiquidParDataAdaptor.Create;
    try
        result := xDataAdaptor.ReadAllNames();
    finally
        xDataAdaptor.Free;
    end;
end;

function TLiquidParViewItem.CreateEditForm(aOwner: TComponent; aOnSaveStatusChanged: TNotifyEvent)
    : TDockableEditForm;
begin
    result := TfrmLiquidParEditor.Create(aOwner, self.Name, aOnSaveStatusChanged);
end;

procedure TLiquidParViewItem.CreateNewName(const aNewName: string);
begin
    TLiquidParDataAdaptor.NewParameter(aNewName);
end;

function TLiquidParViewItem.GetItemType: TViewItemType;
begin
    result := ntLiquidPar;
end;

function TLiquidParViewItem.CreateDataAdaptor: TQueryDataAdaptor;
begin
    result := TLiquidParDataAdaptor.Create();
end;

{ TPowderParViewItem }

function TPowderParViewItem.GetTypeCaption(): string;
begin
    result := TLanguageString.Read('Powder handling parameter', 'Pulverpipettier-Parameter');
end;

function TPowderParViewItem.ReadAllNames(): TArray<string>;
var
    xDataAdaptor: TPowderParDataAdaptor;
begin
    xDataAdaptor := TPowderParDataAdaptor.Create;
    try
        result := xDataAdaptor.ReadAllNames();
    finally
        xDataAdaptor.Free;
    end;
end;

function TPowderParViewItem.CreateDataAdaptor: TQueryDataAdaptor;
begin
    result := TPowderParDataAdaptor.Create();
end;

function TPowderParViewItem.CreateEditForm(aOwner: TComponent; aOnSaveStatusChanged: TNotifyEvent)
    : TDockableEditForm;
begin
    result := TfrmPowderParEditor.Create(aOwner, self.Name, aOnSaveStatusChanged);
end;

procedure TPowderParViewItem.CreateNewName(const aNewName: string);
begin
    TPowderParDataAdaptor.NewParameter(aNewName);
end;

function TPowderParViewItem.GetItemType: TViewItemType;
begin
    result := ntPowderPar;
end;

{ TSequenceViewItem }

function TSequenceViewItem.GetTypeCaption(): string;
begin
    result := TLanguageString.Read('Sequence', 'Sequenz');
end;

function TSequenceViewItem.CreateDataAdaptor: TQueryDataAdaptor;
begin
    result := TSequenceDataAdaptor.Create();
end;

function TSequenceViewItem.AskNewName(var vNewName: string): boolean;
begin
    result := true;
    vNewName := TSequenceManager.NewSequence;
end;

function TSequenceViewItem.CreateEditForm(aOwner: TComponent; aOnSaveStatusChanged: TNotifyEvent)
    : TDockableEditForm;
begin
    result := TfrmSequenceEditor.Create(aOwner, self.Name, aOnSaveStatusChanged);
end;

function TSequenceViewItem.GetItemType: TViewItemType;
begin
    result := ntSequence;
end;

{ TWashProgViewItem }

function TWashProgViewItem.GetTypeCaption(): string;
begin
    result := TLanguageString.Read('Wash program', 'Waschprogram');
end;

function TWashProgViewItem.GetItemType: TViewItemType;
begin
    result := ntWashProg;
end;

function TWashProgViewItem.CreateEditForm(aOwner: TComponent; aOnSaveStatusChanged: TNotifyEvent)
    : TDockableEditForm;
begin
    result := TfrmWashProgEditor.Create(aOwner, self.Name, aOnSaveStatusChanged);
end;

procedure TWashProgViewItem.CreateNewName(const aNewName: string);
var
    xDataAdaptor: TWashProgDataAdaptor;
begin
    xDataAdaptor := TWashProgDataAdaptor.Create;
    try
        xDataAdaptor.SelectAndOpenByName(aNewName, false);
        try
            if (not xDataAdaptor.DataProvider.Eof) then
                EXIT;
            xDataAdaptor.AppendWashProgram(xDataAdaptor.DataProvider, aNewName);
        finally
            xDataAdaptor.Close
        end;
    finally
        xDataAdaptor.Free;
    end;
end;

function TWashProgViewItem.CreateDataAdaptor: TQueryDataAdaptor;
begin
    result := TWashProgDataAdaptor.Create();
end;

{ TReagentViewItem }

function TReagentViewItem.GetTypeCaption(): string;
begin
    result := 'Substance';
end;

function TReagentViewItem.CreateDataAdaptor: TQueryDataAdaptor;
begin
    result := TSubstanceDataDataAdaptor.Create()
end;

function TReagentViewItem.CreateEditForm(aOwner: TComponent; aOnSaveStatusChanged: TNotifyEvent)
    : TDockableEditForm;
begin
    result := TfrmReagentEditor.Create(aOwner, self.Name, aOnSaveStatusChanged);
end;

function TReagentViewItem.GetItemType: TViewItemType;
begin
    result := ntSubstance;
end;

function TReagentViewItem.ReadAllNames(): TArray<string>;
begin
    EXIT(TSubstanceDataDataAdaptor.InstReadAllNames);
end;

{ TSubstanceSetViewItem }

function TSubstanceSetViewItem.GetTypeCaption(): string;
begin
    result := TLanguageString.Read('Reagent', 'Reagenz');
end;

function TSubstanceSetViewItem.CreateDataAdaptor: TQueryDataAdaptor;
begin
    result := TSubstanceSetDataAdaptor.Create()
end;

function TSubstanceSetViewItem.CreateEditForm(aOwner: TComponent; aOnSaveStatusChanged: TNotifyEvent)
    : TDockableEditForm;
begin
    result := TfrmTubeEditor.Create(aOwner, self.Name, aOnSaveStatusChanged);
end;

function TSubstanceSetViewItem.GetItemType: TViewItemType;
begin
    result := ntSubstanceSet;
end;

{ TSQLTermViewItem }

function TSQLTermViewItem.GetTypeCaption(): string;
begin
    result := 'SQL Term';
end;

function TSQLTermViewItem.CreateDataAdaptor: TQueryDataAdaptor;
begin
    result := TSQLTermsDataAdaptor.Create;
end;

function TSQLTermViewItem.CreateEditForm(aOwner: TComponent; aOnSaveStatusChanged: TNotifyEvent)
    : TDockableEditForm;
begin
    result := TfrmSQLTermEditor.Create(aOwner, self.Name, aOnSaveStatusChanged);
end;

function TSQLTermViewItem.GetItemType: TViewItemType;
begin
    result := ntSQLTerm;
end;

function TSQLTermViewItem.ItemNameExistsIntern(var vName: string; aCaseSensitive: boolean;
    aGoodResult: boolean): boolean;
var
    xDataAdaptor: TSQLTermsDataAdaptor;
    xRec: TSQLTermRec;
begin
    xDataAdaptor := TSQLTermsDataAdaptor.Create;
    try
        result := xDataAdaptor.ReadSQLTermData(vName, xRec);
        if (result) then
            vName := xRec.Name;
    finally
        xDataAdaptor.Free;
    end;
end;

procedure TSQLTermViewItem.CreateNewName(const aNewName: string);
var
    xDataAdaptor: TSQLTermsDataAdaptor;
begin
    xDataAdaptor := CreateDataAdaptor() as TSQLTermsDataAdaptor;
    try
        xDataAdaptor.AppendName(aNewName);
    finally
        xDataAdaptor.Free;
    end;
end;

{ TDisplayComponentViewItem }

function TDisplayComponentViewItem.CreateDataAdaptor: TQueryDataAdaptor;
begin
    EXIT(TDisplayComponentsDataAdaptor.Create);
end;

function TDisplayComponentViewItem.GetTypeCaption: string;
begin
    result := TLanguageString.Read('DisplayComponent', 'Display-Komponente');
end;

function TDisplayComponentViewItem.CreateEditForm(aOwner: TComponent; aOnSaveStatusChanged: TNotifyEvent)
    : TDockableEditForm;
begin
    result := TfrmDisplayComponentEditor.Create(aOwner, self.Name, aOnSaveStatusChanged);
end;

function TDisplayComponentViewItem.GetItemType: TViewItemType;
begin
    result := ntDisplayComponent;
end;


end.
