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
  24.02.09 pk  TProgramCounterDataCache              TN4232    --> ProgramCounterDataCache.pas
  25.11.09 pk                                        TN4898    New: Flush function
  04.02.10 pk                                        TN4972    Various Changes
  24.04.13 wl                                        TN6137   uses geändert
  ----------------------------------------------------------------------------------------------------------------------- }

unit CallStackDataCache;


interface


uses
    XMLReaderWriter,
    Streamable,
    IdentItemData,
    RelativeMemAddressData;

type
    TCallStackFrameData = class(TStreamable)
    private
        fReturnAddress: TRelativeMemAddressData;
        fCallArgs: TIdentListData;
        fLocalIdents: TIdentListData;
    public
        constructor Create(); override;
        destructor Destroy; override;
    published
        property ReturnAddress: TRelativeMemAddressData read fReturnAddress write fReturnAddress;
        property CallArgs: TIdentListData read fCallArgs write fCallArgs;
        property LocalIdents: TIdentListData read fLocalIdents write fLocalIdents;
    end;

    TCallStackFrameListData = class(TStreamable)
    private
        fList: TStreamableObjectList;
    public
        constructor Create(); override;
        destructor Destroy(); override;
    published
        property List: TStreamableObjectList read fList write fList;
    end;

    TCallStackData = class(TStreamable)
    private
        fFrames: TCallStackFrameListData;
    public
        constructor Create(); override;
        destructor Destroy(); override;
    published
        property Frames: TCallStackFrameListData read fFrames write fFrames;
    end;

    TCallStackReaderWriter = class(TXMLReaderWriter)
    end;

    TCallStackDataCache = class
    private
        fCallStackData: TCallStackData;
        fCurrentCallStackFrameData: TCallStackFrameData;
        fReaderWriter: TCallStackReaderWriter;
        procedure RefreshCurentCallStackFrameData();
    public
        constructor Create(const aPathName: string);
        destructor Destroy(); override;
        procedure AddCallStackFrameData(const aCallStackFrameData: TCallStackFrameData);
        procedure RemoveLastCallStackFrameData();
        function FindLocalIdent(const aIdentName: string): TIdentItemData;
        function FindOrCreateLocalIdent(const aIdentName: string): TIdentItemData;
        procedure LocalIdentChanged(const aIdentItemData: TIdentItemData);
        procedure Init();
        procedure Clear();
        function read(): boolean;
        procedure Flush();
        property CallStackData: TCallStackData read fCallStackData;
        property CurrentCallStackFrameData: TCallStackFrameData read fCurrentCallStackFrameData;
    end;


implementation


uses
    SysUtils;

const
    cAutoFlushBufferCycle = 0;

constructor TCallStackFrameData.Create;
begin
    inherited;
end;

destructor TCallStackFrameData.Destroy;
begin

    inherited;
end;

{ TCallStackFrameListData }

constructor TCallStackFrameListData.Create;
begin
    inherited;
    fList := TStreamableObjectList.Create();
end;

destructor TCallStackFrameListData.Destroy;
begin
    FreeAndNil(fList);
    inherited;
end;

{ TCallStackData }

constructor TCallStackData.Create;
begin
    inherited Create();
    fFrames := TCallStackFrameListData.Create();
end;

destructor TCallStackData.Destroy;
begin
    FreeAndNil(fFrames);
    inherited;
end;

{ TCallStackDataCache }

constructor TCallStackDataCache.Create(const aPathName: string);
begin
    inherited Create();
    fCallStackData := nil;
    fReaderWriter := TCallStackReaderWriter.Create(aPathName, cAutoFlushBufferCycle);
end;

destructor TCallStackDataCache.Destroy;
begin
    fReaderWriter.Free;
    fCallStackData.Free;
    inherited;
end;

procedure TCallStackDataCache.AddCallStackFrameData(const aCallStackFrameData: TCallStackFrameData);
begin
    fCurrentCallStackFrameData := aCallStackFrameData;
    fCallStackData.Frames.List.Add(fCurrentCallStackFrameData);
    // fCurrentCallStackFrameData.ID := IntToStr( fCallStackData.Frames.List.Count - 1 );

    // fReaderWriter.AddObjectToItemsVirtualObject( fCurrentCallStackFrameData, fCallStackData.Frames.List.Tag );
end;

procedure TCallStackDataCache.RefreshCurentCallStackFrameData();
var
    xCount: integer;
begin
    xCount := fCallStackData.Frames.List.Count;

    if xCount = 0 then
        fCurrentCallStackFrameData := nil
    else
        fCurrentCallStackFrameData := fCallStackData.Frames.List[xCount - 1] as TCallStackFrameData;
end;

procedure TCallStackDataCache.RemoveLastCallStackFrameData;

begin
    // fReaderWriter.RemoveObjectFromItemsVirtualObject( fCurrentCallStackFrameData, fCallStackData.Frames.List.Tag );
    fReaderWriter.DataChanged();
    fCallStackData.Frames.List.Remove(fCurrentCallStackFrameData);
    RefreshCurentCallStackFrameData();
end;

function TCallStackDataCache.FindLocalIdent(const aIdentName: string): TIdentItemData;
begin
    result := fCurrentCallStackFrameData.LocalIdents.FindIdentByName(aIdentName);
end;

function TCallStackDataCache.FindOrCreateLocalIdent(const aIdentName: string): TIdentItemData;
begin
    result := FindLocalIdent(aIdentName);
    if not Assigned(result) then
    begin
        result := TIdentItemData.Create();
        result.IdentName := aIdentName;
        fCurrentCallStackFrameData.LocalIdents.Add(result);
        // result.ID := IntToStr( fCurrentCallStackFrameData.LocalIdents.List.Count - 1 );
        fReaderWriter.DataChanged();
        // fReaderWriter.AddObjectToItemsVirtualObject( result, fCurrentCallStackFrameData.LocalIdents.List.Tag );
    end;
end;

procedure TCallStackDataCache.Flush;
begin
    fReaderWriter.WriteToFile();
end;

procedure TCallStackDataCache.LocalIdentChanged(const aIdentItemData: TIdentItemData);
begin
    // fReaderWriter.SetObjectToVirtualObject( aIdentItemData, aIdentItemData.Tag );
    fReaderWriter.DataChanged();
end;

procedure TCallStackDataCache.Init();
begin
    fCallStackData := TCallStackData.Create();
    fReaderWriter.Activate();
    fReaderWriter.AddObjectToRootNode(fCallStackData);
end;

function TCallStackDataCache.Read(): boolean;
begin
    result := false;
    if not fReaderWriter.ReadFromFile() then
        EXIT;
    fCallStackData := fReaderWriter.CreateObjectFromRootNode<TCallStackData>();
    if not Assigned(fCallStackData) then
        EXIT;

    RefreshCurentCallStackFrameData();
    result := true;
end;

procedure TCallStackDataCache.Clear;
begin
    FreeAndNil(fCallStackData);
    fReaderWriter.DisActivate();
end;

// initialization
// RegisterClasses( [TStreamableObjectList, TRelativeMemAddressData ] );
//
// RegisterClasses( [ TCallStackData, TCallStackFrameListData, TCallStackFrameData ] );


end.
