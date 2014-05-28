{ -----------------------------------------------------------------------------------------------------------------------
  Copyright © 2010 Zinsser Analytic GmbH - All rights reserved.
  Author       : Wolfgang Lyncke (wl)
  Description  :
  -----------------------------------------------------------------------------------------------------------------------
  Revision History:
  date     op  method                                track-no  improvement/change
  -------- --  ------------------------------------  --------- ----------------------------------------------------------
  23.04.10 wl                                        TN5070    Initial Revision
  06.05.10 wl  UnloadData                            TN5052    ruft TViewItemsWorkflow.OverviewManager auf
  20.05.10 wl                                        TN5117   neue Schriftart "Segoe UI", Schriftgröße 9
  07.06.10 wl                                        TN5116   Layout wird jetzt schon geladen
  13.03.13 wl                                        TN5960   TViewItemsWorkflow.Instance statt statischer Methode
  ----------------------------------------------------------------------------------------------------------------------- }

unit LayoutEditor;


interface


uses
    Forms,
    Classes,
    Controls,
    ExtCtrls,

    ViewItem,
    ViewItemEditForm,
    ViewLayout;

type
    TfrmLayoutEditor = class(TViewItemEditForm)
        pnLayout: TPanel;
    private
        fLayoutName: string;
        fViewLayoutForm: TfrmViewLayout;
    protected
        procedure SaveData(); override;
        procedure UnloadData(); override;
        function CreateViewItem(const aItemName: string): TViewItem; override;
    public
        constructor Create(aOwner: TComponent; const aItemName: string;
            aOnSaveStatusChanged: TNotifyEvent); override;
        destructor Destroy(); override;
        property ViewLayoutForm: TfrmViewLayout read fViewLayoutForm;

        class function GetCaption(const aLayoutName: string): string;
    end;


implementation


{$R *.dfm}

uses
    SpecialViewItems,
    ViewItemsWorkflow,
    ControlUtils,
    EditingLayoutManager;

{ TfrmLayoutEditor }

constructor TfrmLayoutEditor.Create(aOwner: TComponent; const aItemName: string;
    aOnSaveStatusChanged: TNotifyEvent);
begin
    inherited;
    TControlUtils.ResetFontForWinXP(self);
    fLayoutName := fViewItem.Name;
    self.Caption := GetCaption(fLayoutName);
    fViewLayoutForm := TfrmViewLayout.Create(self);
    fViewLayoutForm.Parent := pnLayout;
    fViewLayoutForm.Visible := true;

    // TLayoutManager.SetInstance( TLayouterLayoutManager.Create( Main.ViewLayoutForm.DrawPanel ) );

    fViewLayoutForm.SceneGraphics := TEditingLayoutManager.Instance.DefaultSceneGraphics;

    { TZADesignLayoutManager.Instance.ChangeLayoutMode( lmmEdit, self );
      TZADesignLayoutManager.Instance.RegisterLayout( '', fLayoutName );
      //result :=
      TZADesignLayoutManager.Instance.Load();
    }
    // oBackground := self.pnLayout;
end;

function TfrmLayoutEditor.CreateViewItem(const aItemName: string): TViewItem;
begin
    result := TLayoutViewItem.Create(aItemName);
end;

destructor TfrmLayoutEditor.Destroy;
begin
    // TEditingLayoutManager.Instance.UnLoadFromMemory( fLayoutName );

    inherited;
end;

class function TfrmLayoutEditor.GetCaption(const aLayoutName: string): string;
begin
    result := 'Layout: ' + aLayoutName;
end;

procedure TfrmLayoutEditor.SaveData;
begin
    inherited;

    // update Layout view
    // TfrmEdLayout.UpdateInstance();
end;

procedure TfrmLayoutEditor.UnloadData;
begin
    TViewItemsWorkflow.Instance.OverviewFormsUpdateItems([ntLayout]);
end;


end.
