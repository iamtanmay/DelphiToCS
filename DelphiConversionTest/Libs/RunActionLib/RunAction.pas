{ -----------------------------------------------------------------------------------------------------------------------
  Copyright © 2008 Zinsser Analytic GmbH - All rights reserved.
  Author       : Payman Kamali (pk), Wolfgang Lyncke (wl)
  Description  :
  -----------------------------------------------------------------------------------------------------------------------
  Revision History:
  date     op  method                                track-no  improvement/change
  -------- --  ------------------------------------  --------- ----------------------------------------------------------
  08.09.08 pk                                        TN4215    Initial Revision (code from TAction)
  20.09.08 pk  SetWashManager                        TN4215    removed
  20.09.08 pk  DetermineUniqueRunName                TN4215    removed
  04.02.10 pk                                        TN4972    Changes for Restart
  25.04.13 wl  MustWashBeforeExecute                 TN6139   entfernt
  30.04.13 wl  TRunActionReoperationTask             TN6139.1 neue Klasse, die von einer Action erzeugt, aber später ausgeführt wird
  ----------------------------------------------------------------------------------------------------------------------- }

unit RunAction;


interface


uses
    Action,
    Executable,
    RunStepInfo,
    RunStep;

type
    TRunActionReoperationTask = class
    protected
        fCanBeDeleted: boolean;
    public
        constructor Create;
        procedure Execute(aCurrentRunStep: TRunStep); virtual; abstract;
        property CanBeDeleted: boolean read fCanBeDeleted;
    end;

    // an action that may follow another action in a series of actions would implement this interface
    IMultipleItemsItem = interface
        ['{630DFBD1-4F21-4103-B34C-9886D0D2263D}']
        function CreateReoperationTask: TRunActionReoperationTask;
        function MustBringBackToolBeforeExecute: boolean;
    end;

    IResourcedItem = interface
        ['{18E95E2D-18F1-405B-9825-6A895591FB7F}']
        procedure AcquireResources();
        procedure ReleaseResources();
    end;

    TRunAction = class(TAction, IExecutable, IMultipleItemsItem, IResourcedItem)
    protected
        fRunStep: TRunStep;
        fOwnsRunStep: boolean;
        procedure AcquireResourcesImplicit(); virtual;
        procedure ReleaseResourcesImplicit(); virtual;
        procedure AcquireResourcesExplicit(); virtual;
        procedure ReleaseResourcesExplicit(); virtual;
    public
        constructor Create(const aRunStep: TRunStep);
        destructor Destroy(); override;

        // implement IMultipleItemsItem
        function MustBringBackToolBeforeExecute: boolean;

        // implement IResourcedItem
        procedure AcquireResources();
        procedure ReleaseResources();

        function CreateReoperationTask: TRunActionReoperationTask; virtual;

        property RunStep: TRunStep read fRunStep write fRunStep;
        property OwnsRunStep: boolean read fOwnsRunStep write fOwnsRunStep;
    end;


implementation


uses
    SysUtils,
    ResourceManager,
    ThrdMan;

constructor TRunAction.Create(const aRunStep: TRunStep);
begin
    inherited Create;
    fRunStep := aRunStep;
    fOwnsRunStep := false;
end;

function TRunAction.CreateReoperationTask: TRunActionReoperationTask;
begin
    EXIT(nil); // nichts wird erzeugt
end;

destructor TRunAction.Destroy();
begin
    if fOwnsRunStep then
        fRunStep.Free;
    inherited;
end;

function TRunAction.MustBringBackToolBeforeExecute(): boolean;
begin
    result := self.MustPutBackTool;
end;

procedure TRunAction.AcquireResourcesExplicit;
begin

    /// / 05.01.2010 pk  todo if fRunStep.IsDataValid and ( fRunStep.ResID > 0 ) then
    // gResourceManager.AcquireResource( IntToStr( fRunStep.ResID ), rtResScheme );
end;

procedure TRunAction.AcquireResourcesImplicit;
begin

end;

procedure TRunAction.ReleaseResourcesExplicit;
begin
    // // 05.01.2010 pk  todo if fRunStep.IsDataValid and ( fRunStep.ResID > 0 ) then
    // gResourceManager.ReleaseResource( IntToStr( fRunStep.ResID ), rtResScheme );
end;

procedure TRunAction.ReleaseResourcesImplicit;
begin

end;

procedure TRunAction.AcquireResources();
begin
    AcquireResourcesExplicit();
    AcquireResourcesImplicit();
end;

procedure TRunAction.ReleaseResources();
begin
    ReleaseResourcesImplicit();
    ReleaseResourcesExplicit();
end;

{ TRunActionReoperationTask }

constructor TRunActionReoperationTask.Create;
begin
    inherited;
    fCanBeDeleted := false;
end;


end.
