{ -----------------------------------------------------------------------------------------------------------------------
  Copyright © 2009 Zinsser Analytic GmbH - All rights reserved.
  Author       : Payman Kamali (pk)
  Description  :
  -----------------------------------------------------------------------------------------------------------------------
  Revision History:
  date     op  method                                track-no  improvement/change
  -------- --  ------------------------------------  --------- ----------------------------------------------------------
  24.02.09 pk                                        TN4232    Initial Revision
  25.11.09 pk                                        TN4898    New: Flush function
  04.02.10 pk                                        TN4972    Various Changes
  15.11.10 pk                                        TN5340    Changes to prevent memory leak
  15.08.13 wl                               TN6223   uses geändert
  ----------------------------------------------------------------------------------------------------------------------- }

unit ActionIDDataCache;


interface


uses
    XMLReaderWriter,
    Streamable,
    TrackingSemaphore,
    RelativeMemAddressData;

type
    TActionID = int64;

    TActionIDData = class(TStreamable)
    private
        fActionID: TActionID;
    public
        constructor Create(const aID: TActionID); reintroduce;
    published
        property ActionID: TActionID read fActionID write fActionID;
    end;

    TActionIDDataCache = class
    private
        fCriticalSection: TTrackingSemaphore;
        fData: TActionIDData;
        fReaderWriter: TXMLReaderWriter;
        procedure DataChanged();
    public
        constructor Create(const aPathName: string);
        destructor Destroy(); override;
        procedure Init();
        procedure Clear;
        function read(): boolean;
        function GenerateNextActionID(): TActionID;
        procedure Flush();
    end;


implementation


uses
    SysUtils,
    Classes,
    ThreadAPI;

const
    cFirstActionID = 0;
    cAutoFlushBufferCycle = 0;

    { TActionIDDataCache }

constructor TActionIDDataCache.Create(const aPathName: string);
begin
    inherited Create();
    fCriticalSection := TThreadAPI.CreateSemaphore(1);
    fData := nil;
    fReaderWriter := TXMLReaderWriter.Create(aPathName, cAutoFlushBufferCycle);
end;

destructor TActionIDDataCache.Destroy;
begin
    FreeAndNil(fData);
    fCriticalSection.Free;
    fReaderWriter.Free;
    inherited;
end;

procedure TActionIDDataCache.Flush;
begin
    fCriticalSection.Enter();
    try
        fReaderWriter.WriteToFile();
    finally
        fCriticalSection.Leave();
    end;
end;

procedure TActionIDDataCache.Init();
begin
    fData := TActionIDData.Create(cFirstActionID);
    fReaderWriter.Activate();
    fReaderWriter.AddObjectToRootNode(fData);
end;

procedure TActionIDDataCache.Clear;
begin
    FreeAndNil(fData);
    fReaderWriter.DisActivate();
end;

function TActionIDDataCache.Read(): boolean;
begin
    result := false;
    if not fReaderWriter.ReadFromFile() then
        EXIT;
    fData := fReaderWriter.CreateObjectFromRootNode<TActionIDData>();
    if not Assigned(fData) then
        EXIT;
    result := true;
end;

procedure TActionIDDataCache.DataChanged();
begin
    fReaderWriter.DataChanged();
    // fReaderWriter.SetObjectToVirtualObject( fData, fData.Tag );
end;

function TActionIDDataCache.GenerateNextActionID(): TActionID;
begin
    fCriticalSection.Enter();
    try
        ASSERT(fData.ActionID < high(fData.ActionID), 'ActionID Overflow');
        fData.ActionID := fData.ActionID + 1;
        result := fData.ActionID;
        DataChanged();
    finally
        fCriticalSection.Leave();
    end;
end;

{ TActionIDData }
constructor TActionIDData.Create(const aID: TActionID);
begin
    inherited Create();
    fActionID := aID;
end;

// initialization
// RegisterClasses( [TActionIDData ] );


end.
