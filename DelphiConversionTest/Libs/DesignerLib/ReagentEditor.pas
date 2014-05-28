{ --------------------------------------------------------------------------------------------------
  Copyright © 2005 Zinsser Analytic GmbH - All rights reserved.
  Author       : Wolfgang Lyncke (wl)
  Description  : Editor for reagent data
  --------------------------------------------------------------------------------------------------
  Revision History:
  date     op  method                       track-no  improvement/change
  -------- --  ---------------------------  --------- ----------------------------------------------
  04.03.06 wl                               TN2541.4  initial version
  19.12.06 wl                               TN3409   class von TDockableEditForm in TViewItemEditForm geändert
  27.08.07 pk                               TN3788   Reference to New ParserStoredIdentifier.Pas unit
  04.04.08 wl                               TN4058    uses geändert
  16.01.09 wl                                TN4362   an Änderungen in TViewItem angepasst
  04.11.09 pk                               TN4843    Replace TObjectList, TStringList, TStrings with Generics Lists and TStringArray
  06.05.10 wl                               TN5052    uses TViewItemsWorkflow.OverviewManager
  20.05.10 wl                                TN5117   neue Schriftart "Segoe UI", Schriftgröße 9
  03.02.12 wl                                TN5792   Baut jetzt auf SubstanceData auf
  13.03.13 wl                                TN5960   TViewItemsWorkflow.Instance statt statischer Methode
  -------------------------------------------------------------------------------------------------- }

unit ReagentEditor;


interface


uses
    Forms,
    StdCtrls,
    ViewItem,
    ViewItemEditForm,
    ExtCtrls,
    Classes,
    Controls,
    Dialogs;

type
    TfrmReagentEditor = class(TViewItemEditForm)
        Label1: TLabel;
        Edit2: TEdit;
        Label2: TLabel;
        Edit3: TEdit;
        Label3: TLabel;
        Label4: TLabel;
        ComboBox1: TComboBox;
        GroupBox7: TGroupBox;
        Shape1: TShape;
        btnColor: TButton;
        ColorDialog1: TColorDialog;
        procedure btnColorClick(Sender: TObject);
        procedure Edit2Change(Sender: TObject);
        procedure Edit3Change(Sender: TObject);
        procedure ComboBox1Change(Sender: TObject);
    private
        fReagentName: string;
        procedure FillLiqHCombo;
    protected
        procedure SaveData(); override;
        procedure ResetData(); override;
        procedure UnloadData(); override;
        function GetDataName(): string; override;
        function CreateViewItem(const aItemName: string): TViewItem; override;
    public
        constructor Create(aOwner: TComponent; const aItemName: string;
            aOnSaveStatusChanged: TNotifyEvent); override;
    end;


implementation


{$R *.dfm}

uses
    SysUtils,
    SubstanceDataDataAdaptor,
    ControlUtils,
    SpecialViewItems,
    LiqHDataAdaptor,
    ViewItemsWorkflow;

{ TfrmReagentEditor }

procedure TfrmReagentEditor.ComboBox1Change(Sender: TObject);
begin
    ChangeData;
end;

constructor TfrmReagentEditor.Create(aOwner: TComponent; const aItemName: string;
    aOnSaveStatusChanged: TNotifyEvent);
begin
    inherited Create(aOwner, aItemName, aOnSaveStatusChanged);

    TControlUtils.ResetFontForWinXP(self);
    fReagentName := fViewItem.Name;
    Label1.Caption := 'Substance ID: ' + fViewItem.Name;
    Caption := GetCaption();
    FillLiqHCombo;

    self.Reset();
end;

function TfrmReagentEditor.CreateViewItem(const aItemName: string): TViewItem;
begin
    result := TReagentViewItem.Create(aItemName);
end;

procedure TfrmReagentEditor.Edit2Change(Sender: TObject);
begin
    ChangeData;
end;

procedure TfrmReagentEditor.Edit3Change(Sender: TObject);
begin
    ChangeData;
end;

procedure TfrmReagentEditor.FillLiqHCombo;
var
    xDA: TLiqHDataAdaptor;
begin
    xDA := TLiqHDataAdaptor.Create;
    try
        TControlUtils.AddValuesToComboBox(xDA.ReadAllNames, ComboBox1, true);
    finally
        FreeAndNil(xDA);
    end;
end;

function TfrmReagentEditor.GetDataName: string;
begin
    result := fReagentName;
end;

procedure TfrmReagentEditor.ResetData;
var
    xDataRec: TSubstanceDataRec;
    xDA: TSubstanceDataDataAdaptor;
begin
    xDA := TSubstanceDataDataAdaptor.Create;
    try
        xDA.ReadRec(fReagentName, xDataRec);
    finally
        FreeAndNil(xDA);
    end;

    self.Edit2.Text := xDataRec.FullName;
    self.Edit3.Text := xDataRec.Description;
    Shape1.Brush.Color := xDataRec.SubstColor;
    self.ComboBox1.Text := xDataRec.LiqParam;
end;

procedure TfrmReagentEditor.SaveData;
var
    xDataRec: TSubstanceDataRec;
    xDA: TSubstanceDataDataAdaptor;
begin
    xDataRec.Valid := true;
    xDataRec.SubstID := fReagentName;
    xDataRec.FullName := self.Edit2.Text;
    xDataRec.Description := self.Edit3.Text;
    xDataRec.SubstColor := Shape1.Brush.Color;
    xDataRec.LiqParam := self.ComboBox1.Text;

    xDA := TSubstanceDataDataAdaptor.Create;
    try
        xDA.WriteRec(xDataRec);
    finally
        FreeAndNil(xDA);
    end;
end;

procedure TfrmReagentEditor.UnloadData;
begin
    TViewItemsWorkflow.Instance.OverviewFormsUpdateItems([ntSubstance]);
end;

procedure TfrmReagentEditor.btnColorClick(Sender: TObject);
begin
    if (ColorDialog1.Execute) then
    begin
        Shape1.Brush.Color := ColorDialog1.Color;
        ChangeData;
    end;
end;


end.
