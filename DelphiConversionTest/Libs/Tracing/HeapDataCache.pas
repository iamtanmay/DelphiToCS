{ -----------------------------------------------------------------------------------------------------------------------
  Copyright © 2009 Zinsser Analytic GmbH - All rights reserved.
  Author       : Payman Kamali (pk)
  Description  :
  -----------------------------------------------------------------------------------------------------------------------
  Revision History:
  date     op  method                                track-no  improvement/change
  -------- --  ------------------------------------  --------- ----------------------------------------------------------
  17.02.09 pk                                        TN4232    Initial Revision
  20.02.09 pk                                        TN4232    Changes for multithreaded trace
  25.11.09 pk                                        TN4898    New: Flush function
  04.02.10 pk                                        TN4972    Various Changes
  15.08.13 wl                               TN6223   uses geändert
  ----------------------------------------------------------------------------------------------------------------------- }

unit HeapDataCache;


interface


uses
    Classes,
    XMLReaderWriter,
    IdentItemData,
    TrackingSemaphore;

type
    THeapReaderWriter = class(TXMLReaderWriter)
    end;

    THeapData = class(TIdentListData)
    public
        constructor Create(); override;
    end;

    THeapDataCache = class
    private
        fCriticalSection: TTrackingSemaphore;
        fHeapData: THeapData;
        fReaderWriter: THeapReaderWriter;
    public
        constructor Create(const aPathName: string);
        destructor Destroy(); override;
        function FindIdent(const aIdentName: string): TIdentItemData;
        function FindOrCreateIdent(const aIdentName: string): TIdentItemData;
        procedure IdentChanged(const aIdentItemData: TIdentItemData);
        procedure Init();
        procedure Clear();
        function read(): boolean;
        procedure Flush();
        property HeapData: THeapData read fHeapData;
    end;


implementation


uses
    SysUtils,
    ThreadAPI,
    Streamable;

const
    cAutoFlushBufferCycle = 0;

    { THeapData }
constructor THeapData.Create();
begin
    inherited Create();
end;
{ THeapDataCache }

constructor THeapDataCache.Create(const aPathName: string);
begin
    inherited Create();
    fHeapData := nil;
    fReaderWriter := THeapReaderWriter.Create(aPathName, cAutoFlushBufferCycle);
    fCriticalSection := TThreadAPI.CreateSemaphore(1);
end;

destructor THeapDataCache.Destroy;
begin
    fCriticalSection.Free;
    fReaderWriter.Free;
    fHeapData.Free;
    inherited;
end;

procedure THeapDataCache.Init();
begin
    fHeapData := THeapData.Create();
    fReaderWriter.Activate();
    fReaderWriter.AddObjectToRootNode(fHeapData);
end;

procedure THeapDataCache.Clear();
begin
    fReaderWriter.DisActivate();
end;

function THeapDataCache.FindIdent(const aIdentName: string): TIdentItemData;
begin
    result := fHeapData.FindIdentByName(aIdentName);
end;

function THeapDataCache.FindOrCreateIdent(const aIdentName: string): TIdentItemData;
begin
    fCriticalSection.Enter();
    try
        result := FindIdent(aIdentName);
        if not Assigned(result) then
        begin
            result := TIdentItemData.Create();
            result.IdentName := aIdentName;
            fHeapData.Add(result);
            // result.ID := IntToStr( fHeapData.List.Count - 1 );
            fReaderWriter.DataChanged();
            // fReaderWriter.AddObjectToItemsVirtualObject( result, fHeapData.List.Tag );
        end;
    finally
        fCriticalSection.Leave()
    end;
end;

procedure THeapDataCache.Flush;
begin
    fCriticalSection.Enter();
    try
        fReaderWriter.WriteToFile();
    finally
        fCriticalSection.Leave();
    end;
end;

procedure THeapDataCache.IdentChanged(const aIdentItemData: TIdentItemData);
begin
    fCriticalSection.Enter();
    try
        fReaderWriter.DataChanged();
        // fReaderWriter.SetObjectToVirtualObject( aIdentItemData, aIdentItemData.Tag );
    finally
        fCriticalSection.Leave()
    end;
end;

function THeapDataCache.Read(): boolean;
begin
    result := false;
    if not fReaderWriter.ReadFromFile() then
        EXIT;
    fHeapData := fReaderWriter.CreateObjectFromRootNode<THeapData>();
    if not Assigned(fHeapData) then
        EXIT;
    result := true
end;


// initialization
// RegisterClasses( [ THeapData ] );
end.
