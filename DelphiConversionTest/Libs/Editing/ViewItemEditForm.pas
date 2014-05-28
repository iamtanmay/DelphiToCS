unit ViewItemEditForm;
{ ------------------------------------------------------------------------------------------------------------
  Copyright  2009 Zinsser Analytic GmbH - All rights reserved.
  Author       : Wolfgang Lyncke (wl)
  Description  : Base class for (dockable) editors that belongs to a special view item
  ------------------------------------------------------------------------------------------------------------
  Revision History:
  date     op  method                            track-no improvement/change
  -------- --  --------------------------------  -------- ----------------------------------------------------
  16.01.09 wl                                    TN4362   Base class extracted from ViewItems.pas
  20.05.10 wl  Start,SimStart                    TN5116   entfernt
  02.06.10 wl  TCloseDockClientEvent             TN5116   neu: für MainDevelopment-Fenster
  09.06.10 wl                                    TN5116   neu: DisplayDevelopment
  23.02.11 wl                                    TN5486   neu: ImportViewItems
  13.03.13 wl  TAppEditMode                      TN5960   --> MainCustomDevelopment
  14.03.13 wl                                    TN5960   Erweiterungen, um im Runner, die richtigen Menüpunkte zu zeigen
  ------------------------------------------------------------------------------------------------------------ }


interface


uses
    Forms,
    Classes,
    ViewItem,
    DockableForm;

type
    TViewItemEditForm = class(TDockableEditForm)
    private
        fOnExcelPut: TNotifyEvent;
        fOnExcelGet: TNotifyEvent;
        fExcelGetPossible: boolean;
        function GetViewItemType: TViewItemType;
    protected
        fViewItem: TViewItem;
        function CreateViewItem(const aItemName: string): TViewItem; virtual; abstract;
        function GetCompareCaption(): string;
        function GetCaption(): string;
        function GetDataName(): string; virtual;
        function GetExcelGetPossible(): boolean;
        function GetExcelPutPossible(): boolean;
        procedure SetExcelGetPossible(const Value: boolean);
        function GetReasonBeforeSave(): boolean; override;
    public
        constructor Create(aOwner: TComponent; const aItemName: string; aOnSaveStatusChanged: TNotifyEvent);
            reintroduce; virtual;
        destructor Destroy(); override;
        //
        procedure EditExcelPut();
        procedure EditExcelGet();
        function Build(): boolean; virtual;
        procedure ShowBuildResult(); virtual;
        //
        property CompareCaption: string read GetCompareCaption;
        property DataName: string read GetDataName;
        property OnExcelGet: TNotifyEvent read fOnExcelGet write fOnExcelGet;
        property OnExcelPut: TNotifyEvent read fOnExcelPut write fOnExcelPut;
        property ExcelPutPossible: boolean read GetExcelPutPossible;
        property ExcelGetPossible: boolean read GetExcelGetPossible write SetExcelGetPossible;
        property ViewItemType: TViewItemType read GetViewItemType;
        property ViewItem: TViewItem read fViewItem;
    end;


implementation


uses
    AppSettings;

{ TViewItemEditForm }

function TViewItemEditForm.Build: boolean;
begin
    // Dummy
    result := false;
end;

constructor TViewItemEditForm.Create(aOwner: TComponent; const aItemName: string;
    aOnSaveStatusChanged: TNotifyEvent);
begin
    inherited Create(aOwner, aOnSaveStatusChanged);

    fViewItem := self.CreateViewItem(aItemName);
    fOnExcelGet := nil;
    fOnExcelPut := nil;
    fExcelGetPossible := false;
end;

destructor TViewItemEditForm.Destroy;
begin
    fViewItem.Free;

    inherited;
end;

procedure TViewItemEditForm.EditExcelGet;
begin
    if not GetExcelGetPossible() then
        EXIT;

    self.OnExcelGet(nil);
end;

procedure TViewItemEditForm.EditExcelPut;
begin
    if not self.GetExcelPutPossible then
        EXIT;

    self.OnExcelPut(nil);
end;

function TViewItemEditForm.GetCaption(): string;
begin
    // fCompareName is not necessarily the same as DataName that's why we need this
    result := fViewItem.GetFullCaptionOfName(DataName);
end;

function TViewItemEditForm.GetCompareCaption(): string;
begin
    // fCompareName is not necessarily the same as DataName that's why we need this
    result := fViewItem.FullCaption;
end;

function TViewItemEditForm.GetDataName(): string;
begin
    // diese Funktion wird meist überschrieben!
    result := fViewItem.Name;
end;

function TViewItemEditForm.GetExcelGetPossible: boolean;
begin
    result := fExcelGetPossible and Assigned(self.OnExcelGet);
end;

function TViewItemEditForm.GetExcelPutPossible: boolean;
begin
    result := Assigned(self.OnExcelPut);
end;

function TViewItemEditForm.GetReasonBeforeSave: boolean;
begin
    result := TAppSettings.ItemConfirmEdit(fViewItem.TypeCaption, fViewItem.Name);
end;

function TViewItemEditForm.GetViewItemType: TViewItemType;
begin
    result := fViewItem.ItemType;
end;

procedure TViewItemEditForm.SetExcelGetPossible(const Value: boolean);
begin
    fExcelGetPossible := Value;
    if Assigned(fOnSaveStatusChanged) then
        fOnSaveStatusChanged(nil);
end;

procedure TViewItemEditForm.ShowBuildResult;
begin
    // Dummy
end;


end.
