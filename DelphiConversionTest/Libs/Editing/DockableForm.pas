{ --------------------------------------------------------------------------------------------------
  Copyright © 2004 Zinsser Analytic GmbH - All rights reserved.
  Project      : New Editor
  Author       : Wolfgang Lyncke (wl)
  Description  : Base classes for Docking
  --------------------------------------------------------------------------------------------------
  Revision History:
  date     op  method                       track-no  improvement/change
  -------- --  ---------------------------  --------  ----------------------------------------------
  06.08.04 wl                               TN2008.2  initial version
  17.08.04 wl                               TN2008.2  FullDockableForm mit Möglichkeit, an einem TabHost anzudocken
  24.08.04 wl                               TN2008.2  mit Laden und Speichern der DockableForms
  24.09.04 wl                               TN2008.2  Laden und Speichern erheblich verbessert
  21.12.04 wl  TSingleFullDockableForm.Create  TN2247.1  method is virtual
  06.01.05 wl  TDockableEditForm               TN2246.6  Basisklasse für alle Editoren, mit Save-Dialog beim Schließen
  29.06.05 wl  TDockableEditForm               TN2444    Änderungen für Find/Replace, arbeitet mit TDockableFormFindNotifyEvent
  30.06.05 wl  EditSelectAll                   TN2495    neu: um alle Daten zu selektieren
  06.07.05 wl  ExcelPut/Get                    TN2495    neue Funktionalität für Methoden und Commend-Makro
  07.11.05 pk  Start                           TN2737    New : starts a method/run/session
  14.11.05 pk  Build, ShowBuildResult          TN2759    New
  16.11.05 pk  EditAction                      TN2751    Replaces EditCopy, Edit...
  24.11.05 pk  DockableEditForm                TN2767    SelectEditor
  24.11.05 pk  DockableEditForm                TN2765    GetDataName, GetCaption
  24.11.05 pk  TViewItem                       TN2765    New
  09.05.06 pk  CompareCaption, fCompareName    TN3091    Save a CompareName and use it for checking if form is already loaded
  26.07.06 pk  EditSetFocusToTextPart          TN3218    New
  28.11.06 wl  TDockableEditForm.IsDeleted     TN3434    wenn self.IsDeleted: keine Nachfrage bei RequestSaveChanges
  19.12.06 wl  TViewItem                       TN3409    --> ViewItems
  19.12.06 wl  TDockableEditForm               TN3409    ExcelPut/Get,Start,fComparableName,Start --> ViewItems.TViewItemEditForm
  27.04.07 wl  RequestSaveChanges              TN3669    neu: In der Basisfunktion ist result immer true
  25.07.08 pk                                  TN4190    SaveStatusChanged
  08.09.08 pk  EditSetFocusToRow               TN4215    New
  07.08.09 wl                                  TN4702   Strings werden jetzt direkt geladen
  26.10.09 wl  TSingleFullDockableForm.Get/Save/DeletePosition  TN4831   IConfigurationSet replaces TIniFile
  30.09.10 pk                                  TN5287   Search in methods reimplemented
  26.07.11 wl  TFullDockableForm               TN5614   entfernt
  -------------------------------------------------------------------------------------------------- }

unit DockableForm;


interface


uses
    SysUtils,
    Classes,
    Controls,
    Forms,
    Dialogs,
    GeneralTypes;

type
    TDockableFormFindNotifyEvent = procedure(aSender: TObject; const aText: string; aOptions: TFindOptions)
        of object;
    TDockableFormReplaceNotifyEvent = procedure(aSender: TObject; const aFindText, aReplaceText: string;
        aOptions: TFindOptions) of object;

    // Dockable form: Is not automatically dockable. Is docked in the client area of the main form
    TDockableForm = class(TForm)
    private
        fOnFindText: TDockableFormFindNotifyEvent;
    public
        constructor Create(aOwner: TComponent); override;
        //
        procedure EditFindText(aSender: TObject; const aText: string; aOptions: TFindOptions);
        property OnFindText: TDockableFormFindNotifyEvent read fOnFindText write fOnFindText;
    end;

    TEditFormSaveStatus = (efssDataUnchanged, efssDataChanged);

    TEditActionType = (eaCopy, eaCut, eaPaste, eaUndo, eaRedo, eaSelectAll);

    // Dockable edit form: Containt logic to save values and ask before closing
    TDockableEditForm = class(TDockableForm)
    private
        fOnPrint: TNotifyEvent;
        fOnSelectAll: TNotifyEvent;
        fOnReplaceText: TDockableFormReplaceNotifyEvent;
        fIsDeleted: boolean;
        //
        procedure FormClose(Sender: TObject; var Action: TCloseAction);
        procedure SetSaveStatus(aValue: TEditFormSaveStatus);
        function GetDataChanged: boolean;
    protected
        fHasUndoFunction: boolean;
        fHasClipboardFunction: boolean;
        fSaveStatus: TEditFormSaveStatus;
        fOnSaveStatusChanged: TNotifyEvent;
        //
        procedure ChangeData();
        procedure SaveData(); virtual; abstract;
        procedure ResetData(); virtual;
        procedure UnloadData(); virtual;
        function CanClose(): boolean; virtual;
        function CheckLastChange(): boolean; virtual;
        function GetReasonBeforeSave(): boolean; virtual;
        procedure SaveStatusChanged(); virtual;
    public
        //
        constructor Create(aOwner: TComponent; aOnSaveStatusChanged: TNotifyEvent); reintroduce; virtual;
        //
        procedure FirstLoad(); virtual;
        procedure SelectEditor(); virtual;
        procedure Save();
        procedure Reset();
        procedure Print();
        function EditAction(aSender: TObject; aEditAction: TEditActionType): boolean; virtual;
        procedure EditReplaceText(aSender: TObject; const aFindText, aReplaceText: string;
            aOptions: TFindOptions);
        procedure EditSelectAll();
        procedure EditSetFocusToRow(aRow: integer); virtual;
        procedure EditSetFocusToTextPart(const aRow: integer; const aSettingKey: TStringArray;
            const aFocusStart, aFocusLen: integer); virtual;
        function RequestSaveChanges(): boolean;
        //
        property DataChanged: boolean read GetDataChanged;
        property HasUndoFunction: boolean read fHasUndoFunction;
        property HasClipboardFunction: boolean read fHasClipboardFunction;
        property OnPrint: TNotifyEvent read fOnPrint write fOnPrint;
        property OnSelectAll: TNotifyEvent read fOnSelectAll write fOnSelectAll;
        property OnSaveStatusChanged: TNotifyEvent read fOnSaveStatusChanged;
        property OnReplaceText: TDockableFormReplaceNotifyEvent read fOnReplaceText write fOnReplaceText;
        property IsDeleted: boolean read fIsDeleted write fIsDeleted;
    end;


implementation


uses
    Windows,
    DialogUtils;

{ TDockableForm }

constructor TDockableForm.Create(aOwner: TComponent);
begin
    inherited;

    self.DragKind := dkDock;
    fOnFindText := nil;
end;

procedure TDockableForm.EditFindText(aSender: TObject; const aText: string; aOptions: TFindOptions);
begin
    if not Assigned(self.OnFindText) then
        EXIT;

    self.OnFindText(aSender, aText, aOptions);
end;

{ TDockableEditForm }

constructor TDockableEditForm.Create(aOwner: TComponent; aOnSaveStatusChanged: TNotifyEvent);
begin
    inherited Create(aOwner);

    self.OnClose := FormClose;
    fOnPrint := nil;
    fOnSelectAll := nil;
    fOnReplaceText := nil;
    fOnSaveStatusChanged := aOnSaveStatusChanged;
    fHasUndoFunction := false;
    fHasClipboardFunction := false;
    fSaveStatus := efssDataUnchanged;
    fIsDeleted := false;
    // if Assigned( fOnSaveStatusChanged ) then fOnSaveStatusChanged( nil );
end;

procedure TDockableEditForm.ResetData;
begin
    // Dummy
end;

function TDockableEditForm.RequestSaveChanges(): boolean;
var
    xMsgBoxResult: integer;
begin
    if (fIsDeleted) then
    begin
        result := true;
        EXIT;
    end;

    result := false;
    if not CheckLastChange() then
        EXIT;

    result := true;
    if (fSaveStatus = efssDataUnchanged) then
        EXIT;

    xMsgBoxResult := TDialogUtils.MessageBox(TLanguageString.Read('{0} has been changed. Save changes?',
        '{0} wurde geändert. Sollen die Änderungen gespeichert werden?', [self.Caption]),
        TLanguageString.Read('Request', 'Rückfrage'), mb_YESNOCANCEL);

    case (xMsgBoxResult) of
        IDYES:
            begin
                if not self.GetReasonBeforeSave() then
                begin
                    result := false;
                    EXIT;
                end;
                SaveData;
                self.SetSaveStatus(efssDataUnchanged);
            end;
        IDNO:
            begin
                Reset;
            end;
        else
            begin
                result := false;
            end;
    end;
end;

function TDockableEditForm.CanClose(): boolean;
begin
    result := true;
end;

procedure TDockableEditForm.FormClose(Sender: TObject; var Action: TCloseAction);
begin
    Action := caNone;

    if RequestSaveChanges() then
    begin
        if not CanClose() then
            EXIT;
        Action := caFree;
        UnloadData();
    end
end;

procedure TDockableEditForm.SaveStatusChanged();
begin
end;

procedure TDockableEditForm.SetSaveStatus(aValue: TEditFormSaveStatus);
begin
    if (aValue = fSaveStatus) then
        EXIT;

    fSaveStatus := aValue;

    SaveStatusChanged();
    if Assigned(fOnSaveStatusChanged) then
        fOnSaveStatusChanged(nil);
end;

procedure TDockableEditForm.ChangeData;
begin
    self.SetSaveStatus(efssDataChanged);
end;

procedure TDockableEditForm.Reset;
begin
    ResetData;
    self.SetSaveStatus(efssDataUnchanged);
end;

procedure TDockableEditForm.Save;
begin
    if not CheckLastChange() then
        EXIT;
    if not self.GetReasonBeforeSave() then
        EXIT;

    SaveData;
    self.SetSaveStatus(efssDataUnchanged);
end;

procedure TDockableEditForm.Print;
begin
    if not Assigned(self.OnPrint) then
        EXIT;

    if RequestSaveChanges() then
        self.OnPrint(nil);
end;

procedure TDockableEditForm.UnloadData;
begin
    // Dummy
end;

function TDockableEditForm.GetDataChanged: boolean;
begin
    result := (fSaveStatus = efssDataChanged);
end;

procedure TDockableEditForm.EditReplaceText(aSender: TObject; const aFindText, aReplaceText: string;
    aOptions: TFindOptions);
begin
    if not Assigned(self.OnReplaceText) then
        EXIT;

    self.OnReplaceText(aSender, aFindText, aReplaceText, aOptions);
end;

procedure TDockableEditForm.FirstLoad;
begin
    // Inhalt laden (nach dem Andocken)
end;

procedure TDockableEditForm.EditSelectAll;
begin
    if not Assigned(self.OnSelectAll) then
        EXIT;

    self.OnSelectAll(nil);
end;

function TDockableEditForm.CheckLastChange(): boolean;
begin
    result := true;
end;

function TDockableEditForm.EditAction(aSender: TObject; aEditAction: TEditActionType): boolean;
begin
    result := false;
end;

procedure TDockableEditForm.EditSetFocusToTextPart(const aRow: integer; const aSettingKey: TStringArray;
    const aFocusStart, aFocusLen: integer);
begin
    // Dummy
end;

procedure TDockableEditForm.EditSetFocusToRow(aRow: integer);
begin
    // Dummy;
end;

procedure TDockableEditForm.SelectEditor;
begin
    // Dummy
end;

function TDockableEditForm.GetReasonBeforeSave(): boolean;
begin
    result := true;
end;


end.
