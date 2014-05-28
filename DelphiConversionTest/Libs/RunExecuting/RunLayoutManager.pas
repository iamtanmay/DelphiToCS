unit RunLayoutManager;
{ -----------------------------------------------------------------------------------------------------------------------
  Copyright © 2008 Zinsser Analytic GmbH - All rights reserved.
  Author       : Payman Kamali (pk)
  Description  :
  -----------------------------------------------------------------------------------------------------------------------
  Revision History:
  date     op  method                                track-no  improvement/change
  -------- --  ------------------------------------  --------- ----------------------------------------------------------
  03.12.08 pk  DoAfterLoaded                          TN4341    set gRunMain.LayoutName
  17.12.08 pk                                         TN4341    call SyncChangeLayoutName
  20.08.09 wl                                         TN4702     Strings werden jetzt direkt geladen
  04.08.10 pk  GetTempSettingsSectionName             TN5043     New: a different settings section name for each layoutmanager
  04.09.10 pk                                         TN5042     functions that were in RunMain now accessible only via GUIManager
  27.02.13 wl                                         TN6045   uses Generics.Collections
  ----------------------------------------------------------------------------------------------------------------------- }


interface


uses
    Controls,
    Layout,
    LayoutManager;

type
    TRunLayoutManager = class(TLayoutManager)
    protected
        function DoCreateLayout(const aRunName, aLayoutName: string): TLayout; override;
        procedure DoAfterLoaded(); override;
        function GetTempSettingsSectionName(): string; override;
    public
        constructor Create(aBackGroundGraphicsParent: TWinControl);
        destructor Destroy; override;
        function BCEqualToOther(const aRackName, aRackID: string): string; override;
        procedure StartMethodRun(const aMethodName: string); override;
    end;


implementation


uses
    LayoutDataAdaptor,
    RunLayout,
    GUIManagerRun;

{ TRunLayoutManager }

constructor TRunLayoutManager.Create(aBackGroundGraphicsParent: TWinControl);
begin
    inherited Create(aBackGroundGraphicsParent);
end;

destructor TRunLayoutManager.Destroy;
begin
    inherited;
end;

procedure TRunLayoutManager.StartMethodRun(const aMethodName: string);
begin
    { TODO -oPK -cAction Package : }
    // gmExecuteRunStartWithName( aMethodName );
end;

function TRunLayoutManager.BCEqualToOther(const aRackName, aRackID: string): string;
var
    xDA: TLayoutDataAdaptor;
begin
    result := '';
    if not Assigned(self.CurrentLayout) then
        EXIT;
    xDA := TLayoutDataAdaptor.Create();
    try
        result := xDA.BCEqualToOther(self.CurrentLayout.LayoutRunName, self.CurrentLayout.LayoutName,
            aRackName, aRackID);
    finally
        xDA.Free;
    end;
end;

procedure TRunLayoutManager.DoAfterLoaded();
begin
    // delete additional menu items
    TGUIManagerRun.Instance.DitiMenu_Clear();
    if not Assigned(fCurrentLayout) then
        EXIT;
    TGUIManagerRun.Instance.LayoutName_Change(fCurrentLayout.LayoutName);
    // TResLoader.GetResFString(55810{LayoutName: %s ----- Run Name: %s},[fCurrentLayout.LayoutName,fCurrentLayout.LayoutRunName]);

    TGUIManagerRun.Instance.DitiMenu_Load();
end;

function TRunLayoutManager.DoCreateLayout(const aRunName, aLayoutName: string): TLayout;
begin
    result := TRunLayout.Create(aLayoutName, aRunName);
end;

function TRunLayoutManager.GetTempSettingsSectionName: string;
begin
    result := 'Layout_Runner';
end;


end.
