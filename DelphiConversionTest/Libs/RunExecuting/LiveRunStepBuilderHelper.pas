{ -----------------------------------------------------------------------------------------------------------
  Copyright © 2013 Zinsser Analytic GmbH - All rights reserved.
  Author       : Wolfgang Lyncke (wl)
  Description  :
  -----------------------------------------------------------------------------------------------------------
  Revision History:
  date     op  method                              track-no improvement/change
  -------- --  ----------------------------------  -------- -------------------------------------------------
  13.02.13 wl                                      TN6075   Initial Revision
  15.08.13 wl                                      TN6223   uses geändert
  ----------------------------------------------------------------------------------------------------------- }

unit LiveRunStepBuilderHelper;


interface


uses
    RunStepBuilderHelper,
    AppTypes,
    RackWell;

type
    TLiveRunStepBuilderHelper = class(TRunStepBuilderHelper)
    private
        function DoOnFindRackDataByName(var vRackName: string; out oRackRows, oRackCols: integer;
            out oRotation: TRotationValue): boolean;
        function DoOnFindRackPosByTubeID(const aTubeID: string): TRackPosition;
        function DoOnFindRackPosBySubstID(const aSubstID: string; aUseDeadVolume: TUseDeadVolume)
            : TArray<TRackPositionWithVol>;
        function DoOnFindAllStorageRackPos(aUseDeadVolume: TUseDeadVolume): TArray<TRackIDPositionWithVol>;
        function DoOnFindXYByRackPosition(const aRackName: string; const aPos: integer;
            out oX, oY: double): boolean;
    public
        constructor Create;
    end;


implementation


uses
    ThreadAPI,
    MatrixMath,
    LayoutManager,
    Rack,
    SubstanceLoading,
    RackTypes;

{ TLiveRunStepBuilderHelper }

constructor TLiveRunStepBuilderHelper.Create;
begin
    inherited Create(false, TThreadAPI.GetCurrentThreadID, DoOnFindRackDataByName, DoOnFindRackPosByTubeID,
        DoOnFindRackPosBySubstID, DoOnFindAllStorageRackPos, DoOnFindXYByRackPosition);
end;

function TLiveRunStepBuilderHelper.DoOnFindRackDataByName(var vRackName: string;
    out oRackRows, oRackCols: integer; out oRotation: TRotationValue): boolean;
var
    xRack: TRack;
begin
    if TLayoutManager.Instance.IsCurrentLayoutEmpty then
        EXIT(false);

    xRack := TLayoutManager.Instance.CurrentLayout.FindRackByName(vRackName);
    if Assigned(xRack) then
    begin
        vRackName := xRack.name; // RackID wird zu RackName umgeschrieben
        oRackRows := xRack.Structure.Rows;
        oRackCols := xRack.Structure.Cols;
        oRotation := xRack.RackRotation;
        EXIT(true);
    end;
    EXIT(false);
end;

function TLiveRunStepBuilderHelper.DoOnFindRackPosBySubstID(const aSubstID: string;
    aUseDeadVolume: TUseDeadVolume): TArray<TRackPositionWithVol>;
begin
    if TLayoutManager.Instance.IsCurrentLayoutEmpty then
        EXIT(nil);

    EXIT(TSubstanceLoading.Instance.FindCorrectedStorageVolumesByID(TLayoutManager.Instance.CurrentLayout,
        aSubstID, aUseDeadVolume));
end;

function TLiveRunStepBuilderHelper.DoOnFindAllStorageRackPos(aUseDeadVolume: TUseDeadVolume)
    : TArray<TRackIDPositionWithVol>;
begin
    if TLayoutManager.Instance.IsCurrentLayoutEmpty then
        EXIT(nil);

    EXIT(TSubstanceLoading.Instance.FindCorrectedStorageVolumesAll(TLayoutManager.Instance.CurrentLayout,
        aUseDeadVolume));
end;

function TLiveRunStepBuilderHelper.DoOnFindRackPosByTubeID(const aTubeID: string): TRackPosition;
begin
    if TLayoutManager.Instance.IsCurrentLayoutEmpty then
        EXIT(RackTypes.gmMakeRackPos('', 0));

    EXIT(TSubstanceLoading.Instance.FindRackPosByTubeID(TLayoutManager.Instance.CurrentLayout, aTubeID));
end;

function TLiveRunStepBuilderHelper.DoOnFindXYByRackPosition(const aRackName: string; const aPos: integer;
    out oX, oY: double): boolean;
var
    xPoint: TPoint4d;
    xRack: TRack;
begin
    if TLayoutManager.Instance.IsCurrentLayoutEmpty then
        EXIT(false);

    xRack := TLayoutManager.Instance.CurrentLayout.FindRackByName(aRackName);
    if Assigned(xRack) then
    begin
        xPoint := xRack.CalcTubePos(aPos);
        oX := xPoint.x;
        oY := xPoint.Y;
        EXIT(true);
    end;
    EXIT(false);
end;


end.
