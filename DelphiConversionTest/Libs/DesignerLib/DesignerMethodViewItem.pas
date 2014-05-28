{ -----------------------------------------------------------------------------------------------------------
  Copyright © 2012 Zinsser Analytic GmbH - All rights reserved.
  Author       : Wolfgang Lyncke (wl)
  Description  :
  -----------------------------------------------------------------------------------------------------------
  Revision History:
  date     op  method                              track-no improvement/change
  -------- --  ----------------------------------  -------- -------------------------------------------------
  10.12.12 wl                                      TN6045   Initial Revision
  13.03.13 wl  Create                              TN5960   Parameter geändert
  30.08.13 wl  CreateDefaultSettings               TN6236   neu
  ----------------------------------------------------------------------------------------------------------- }

unit DesignerMethodViewItem;


interface


uses
    Classes,
    DockableForm,
    MethodSettingsDataAdaptor,
    MethodViewItem;

type
    TDesignerMethodViewItem = class(TMethodViewItem)
    protected
        function CreateDefaultSettings(const aName: string): TMethodSettingsRec; override;
    public
        constructor Create(const aItemName: string);
        function CreateEditForm(aOwner: TComponent; aOnSaveStatusChanged: TNotifyEvent)
            : TDockableEditForm; override;
    end;


implementation


uses
    MethodEditor;

{ TDesignerMethodViewItem }

constructor TDesignerMethodViewItem.Create(const aItemName: string);
begin
    inherited Create(aItemName, false);
end;

function TDesignerMethodViewItem.CreateDefaultSettings(const aName: string): TMethodSettingsRec;
begin
    result := TMethodSettingsDataAdaptor.GetEmptyRec;
    result.MethodName := aName;
end;

function TDesignerMethodViewItem.CreateEditForm(aOwner: TComponent; aOnSaveStatusChanged: TNotifyEvent)
    : TDockableEditForm;
begin
    result := TfrmMethodEditor.Create(aOwner, self.Name, aOnSaveStatusChanged);
end;


end.
