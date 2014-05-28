{ -----------------------------------------------------------------------------------------------------------
  Copyright © 2012 Zinsser Analytic GmbH - All rights reserved.
  Author       : Wolfgang Lyncke (wl)
  Description  :
  -----------------------------------------------------------------------------------------------------------
  Revision History:
  date     op  method                              track-no improvement/change
  -------- --  ----------------------------------  -------- -------------------------------------------------
  10.12.12 wl                                      TN6045   Initial Revision
  22.02.13 wl                                      TN6045   überarbeitet
  06.03.13 wl  OnGetPickList, OnOpenFunction       TN6103   Parameter geändert
  08.03.13 wl                                      TN6045   keine Compilermeldung mehr
  21.03.13 wl                                      TN6045   funktioniert jetzt
  ----------------------------------------------------------------------------------------------------------- }

unit BuildingBlockEditFunctions;


interface


uses
    Layout,
    EditFunctions,
    GeneralTypes,
    CustomSetting,
    ModuleSettings;

type
    TBuildingBlockEditFunctions = class(TEditFunctions)
    private
        fMethodLayoutName: string;
    protected
        function IsCurrentLayoutEmpty(): boolean; override;
        function GetCurrentLayoutName(): string; override;
        function GetCurrentLayout(): TLayout; override;
    public
        constructor Create();
        procedure MethodName_OpenValue(aSetting: TObject); override;
        property MethodLayoutName: string read fMethodLayoutName write fMethodLayoutName;
    end;


implementation


uses
    CustomEditFunctionParams;

{ TEditFunctions }

constructor TBuildingBlockEditFunctions.Create;
begin
    inherited Create(true);
end;

function TBuildingBlockEditFunctions.GetCurrentLayout: TLayout;
begin
    // das könnte krachen!
    EXIT(nil);
end;

function TBuildingBlockEditFunctions.GetCurrentLayoutName: string;
begin
    EXIT(fMethodLayoutName);
end;

function TBuildingBlockEditFunctions.IsCurrentLayoutEmpty: boolean;
begin
    EXIT(false);
end;

procedure TBuildingBlockEditFunctions.MethodName_OpenValue(aSetting: TObject);
begin
    // TViewItemsWorkflow.OpenEditForm(xValue, ntMethod, true)
end;


end.
