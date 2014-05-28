unit LayoutEditingFunctionProvider;
{ ------------------------------------------------------------------------------------------------------------
  Copyright © 2010 Zinsser Analytic GmbH - All rights reserved.
  Author       : Wolfgang Lyncke (wl)
  Description  :
  ------------------------------------------------------------------------------------------------------------
  Revision History:
  date     op  method                            track-no improvement/change
  -------- --  --------------------------------  -------- ----------------------------------------------------
  07.06.10 wl                                    TN5116   initial revision
  09.12.10 wl  MainNodeIs..                      TN5257   neu für schnelles Insert von racks
  ------------------------------------------------------------------------------------------------------------ }


interface


uses
    CommonTypes,
    Rack,
    Carrier,
    Layout,
    EditingLayoutElements;

type
    TLayoutEditingFunctionProvider = class
    private
        class var uInstance: TLayoutEditingFunctionProvider;
    protected
        function GetNotSaved: boolean; virtual; abstract;
        procedure SetNotSaved(const Value: boolean); virtual; abstract;
        function GetCurrentLayout: TLayout; virtual; abstract;
        function GetCurrentSetupLayout: TSetupLayout; virtual; abstract;
    public
        class procedure SetInstance(aLayoutManager: TLayoutEditingFunctionProvider);
        class procedure DestroyInstance();
        class property Instance: TLayoutEditingFunctionProvider read uInstance;

        function DragSourceIsNode(aSource: TObject): boolean; virtual; abstract;
        function DragSourceIsCarrierNode(aSource: TObject; out oCarrierName: string): boolean;
            virtual; abstract;
        function DragSourceIsLayoutNode(aSource: TObject; out oLayoutName: string): boolean; virtual;
            abstract;
        function DragSourceIsRackNode(aSource: TObject; out oRackName: string): boolean; virtual; abstract;
        function DragSourceIsWorkspaceNode(aSource: TObject; out oWorkspaceName: string): boolean;
            virtual; abstract;
        function MainNodeIsCarrierNode(out oCarrierName: string): boolean; virtual; abstract;
        function MainNodeIsRackNode(out oRackName: string): boolean; virtual; abstract;
        function MainNodeIsWorkspaceNode(out oWorkspaceName: string): boolean; virtual; abstract;
        function EvaluationStart(aCarrier: TCarrier; aRack: TRack): TCarrierEvaluation; virtual; abstract;

        procedure SceneChanged(); virtual; abstract;
        procedure Load(); virtual; abstract;

        property CurrentLayout: TLayout read GetCurrentLayout;
        property CurrentSetupLayout: TSetupLayout read GetCurrentSetupLayout;
        property NotSaved: boolean read GetNotSaved write SetNotSaved;
    end;


implementation


uses
    SysUtils;

class procedure TLayoutEditingFunctionProvider.SetInstance(aLayoutManager: TLayoutEditingFunctionProvider);
begin
    uInstance := aLayoutManager;
end;

class procedure TLayoutEditingFunctionProvider.DestroyInstance();
begin
    FreeAndNil(uInstance);
end;


end.
