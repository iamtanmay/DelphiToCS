{ -----------------------------------------------------------------------------------------------------------------------
  Copyright © 2009 Zinsser Analytic GmbH - All rights reserved.
  Author       : Payman Kamali (pk)
  Description  :
  -----------------------------------------------------------------------------------------------------------------------
  Revision History:
  date     op  method                                track-no  improvement/change
  -------- --  ------------------------------------  --------- ----------------------------------------------------------
  17.02.09 pk                                        TN4232    Initial Revision
  20.02.09 pk  TActionData                           TN4232    New fIsInEvent
  24.02.09 pk  TActionData                           TN4232    New fStartTime, fEndTime
  24.02.09 pk  TActionListDataCache                  TN4232    wrapper for TMultiListDataCache
  04.11.09 pk                               	        TN4843    Replace TObjectList, TStringList, TStrings with Generics Lists and TStringArray
  25.11.09 pk                                        TN4898    New: Flush function
  04.02.10 pk                                        TN4972    Various Changes
  23.04.10 pk                                        TN5072    Changes to TMultiListDataCacheIterator
  07.06.10 pk  ChangeStarted                         TN5077    New
  26.10.10 pk  Remove                                TN5297    New
  27.10.10 pk  Remove                                TN5297    move cursor backwards
  18.09.13 wl                                        TN6045   uses geändert
  ----------------------------------------------------------------------------------------------------------------------- }

unit ActionDataCache;


interface


uses
    MultiListDataCache,
    ActionData,
    XMLReaderWriter,
    Streamable,
    RelativeMemAddressData,
    RunStep,
    ActionIDDataCache,
    RunEffectData;

type
    TActionListDataCacheIterator = TMultiListDataCacheIterator<TActionData>;

    TActionMultiListDataCache = class(TMultiListDataCache<TActionData>);

    // TActionMultiListDataCache = TMultiListDataCache<TActionData>;
    TActionListDataCache = class
    private const
        cActionsPerFile = 20;
        cAutoFlushBufferCycle = 0;
    private
        fCache: TActionMultiListDataCache;
        fIterator: TActionListDataCacheIterator;
        function GetCurrentActionData: TActionData;
    public
        constructor Create(const aPathName: string);
        destructor Destroy(); override;

        // function Find( const aActionID : integer ) : TActionData;
        procedure Add(const aData: TActionData);
        procedure Remove(const aData: TActionData);
        procedure CurrentActionDataChanged();
        procedure ActionDataChanged();
        procedure ChangedFinished(const aValue: TDateTime);
        procedure ChangedStarted(const aValue: TDateTime; const aRunStep: TRunStep);
        procedure Init();
        procedure Clear();
        function read(): boolean;
        procedure Flush();
        procedure AddRunEffect(const aRunEffectData: TRunEffectData);
        function CreateForwardIterator(): TActionListDataCacheIterator;
        function CreateBackwardIterator(): TActionListDataCacheIterator;
        property CurrentActionData: TActionData read GetCurrentActionData;
    end;


implementation


uses
    SysUtils;

{ TActionListDataCache }

constructor TActionListDataCache.Create(const aPathName: string);
begin
    inherited Create();

    fCache := TActionMultiListDataCache.Create(aPathName, cActionsPerFile, cAutoFlushBufferCycle);
    fIterator := fCache.CreateForwardStartAtLastIterator();

end;

function TActionListDataCache.CreateForwardIterator: TActionListDataCacheIterator;
begin
    result := fCache.CreateForwardIterator();
end;

function TActionListDataCache.CreateBackwardIterator: TActionListDataCacheIterator;
begin
    result := fCache.CreateBackwardIterator();
end;

destructor TActionListDataCache.Destroy;
begin
    fIterator.Free;
    fCache.Free;
    inherited;
end;

procedure TActionListDataCache.Flush;
begin
    fCache.Flush();
end;

function TActionListDataCache.GetCurrentActionData: TActionData;
begin
    result := fIterator.Current;
end;

procedure TActionListDataCache.Init();
begin
    fCache.Init();
end;

procedure TActionListDataCache.Clear();
begin
    fCache.Clear();
end;

procedure TActionListDataCache.Add(const aData: TActionData);
begin
    fCache.Add(aData);
    fIterator.MoveNext;
end;

procedure TActionListDataCache.Remove(const aData: TActionData);
begin
    fCache.Remove(aData);
    fIterator.MovePrevious();
end;

procedure TActionListDataCache.AddRunEffect(const aRunEffectData: TRunEffectData);
var
    xCurrentActionData: TActionData;
begin
    xCurrentActionData := self.CurrentActionData;
    xCurrentActionData.AddRunEffectToCurrentSegment(aRunEffectData);
    // aRunEffectData.ID := IntToStr( fCurrentActionData.RunEffects.Count - 1 );
    CurrentActionDataChanged();
end;

procedure TActionListDataCache.ActionDataChanged();
begin
    fCache.DataChanged();
end;

procedure TActionListDataCache.CurrentActionDataChanged();
begin
    ActionDataChanged();
end;

function TActionListDataCache.Read(): boolean;
begin
    result := fCache.ReadTOC();
    // movenext should move to the last action in the list since fIterator is a CreateForwardStartAtLastIterator
    fIterator.MoveNext;
end;

procedure TActionListDataCache.ChangedFinished(const aValue: TDateTime);
begin
    self.CurrentActionData.CurrentActionSegment.FinishTime := TActionData.DateTimeValueToStr(aValue);
    CurrentActionDataChanged();
end;

procedure TActionListDataCache.ChangedStarted(const aValue: TDateTime; const aRunStep: TRunStep);
begin
    self.CurrentActionData.AddActionSegment();
    self.CurrentActionData.CurrentActionSegment.StartTime := TActionData.DateTimeValueToStr(aValue);
    self.CurrentActionData.CurrentActionSegment.RunStep := aRunStep;
    CurrentActionDataChanged();
end;

//
// initialization
// RegisterClasses( [ TActionData,
// TRunEffectData, TRunEffectListData,
// TRunStep ] );


end.
