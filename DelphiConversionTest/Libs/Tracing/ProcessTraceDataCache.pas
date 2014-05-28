unit ProcessTraceDataCache;


{ -----------------------------------------------------------------------------------------------------------------------
  Copyright © 2009 Zinsser Analytic GmbH - All rights reserved.
  Author       : Payman Kamali (pk)
  Description  :
  -----------------------------------------------------------------------------------------------------------------------
  Revision History:
  date     op  method                                track-no  improvement/change
  -------- --  ------------------------------------  --------- ----------------------------------------------------------
  17.02.09 pk                                        TN4232      Initial Revision
  20.02.09 pk                                        TN4232      Changes for multithreaded trace
  24.02.09 pk  TProcessTraceListDataCache            TN4232      Create aPath given directly to XMLReaderWriter.Create
  24.02.09 pk  TProcessTraceListDataCache            TN4232      free fProcessTraces in Read
  25.11.09 pk                                        TN4898      XMLReaderWriter.FlushToFile renamed to DataChanged
  04.02.10 pk                                        TN4972    Various Changes
  13.04.10 wl                                        TN5044   uses geändert
  15.11.10 pk                                        TN5340     Changes to prevent memory leak
  ----------------------------------------------------------------------------------------------------------------------- }
interface


uses
    XMLReaderWriter,
    Streamable;

type
    TProcessTraceData = class(TStreamable)
    private
        fTraceName: string;
    public

        constructor Create(const aTraceName: string); reintroduce;
    published
        property TraceName: string read fTraceName write fTraceName;
    end;

    TProcessTraceListData = class(TStreamable)
    private
        fList: TStreamableObjectList;
    public
        constructor Create(); override;
        destructor Destroy(); override;
    published
        property List: TStreamableObjectList read fList write fList;
    end;

    TProcessTraceListDataCache = class
    private
        fProcessTraces: TProcessTraceListData;
        fReaderWriter: TXMLReaderWriter;
        function GetCount: integer;
        function GetItemAt(aIndex: integer): TProcessTraceData;
    public
        constructor Create(const aPath: string);
        destructor Destroy(); override;
        procedure AddProcessTrace(aProcessTraceData: TProcessTraceData);
        procedure RemoveProcessTrace(const aTraceName: string; const aRemoveFromFile: boolean);
        function FindProcessTrace(const aTraceName: string): TProcessTraceData;
        // function FindProcessTraceBySourceDataName( const aSourceDataName: string ): TProcessTraceData;
        function read(): boolean;
        procedure Init();
        procedure Flush();
        property Count: integer read GetCount;
        property Items[aIndex: integer]: TProcessTraceData read GetItemAt; default;
    end;


implementation


uses
    Classes,
    SysUtils,
    AppSettings;

{ TProcessTraceListData }

constructor TProcessTraceListData.Create;
begin
    inherited;
    fList := TStreamableObjectList.Create();
end;

destructor TProcessTraceListData.Destroy;
begin
    fList.Free;
    inherited;
end;

{ TProcessTraceListDataCache }
constructor TProcessTraceListDataCache.Create(const aPath: string);
begin
    inherited Create();
    fReaderWriter := TXMLReaderWriter.Create(aPath, 1);
    fProcessTraces := nil;
end;

destructor TProcessTraceListDataCache.Destroy;
begin
    FreeAndNil(fProcessTraces);
    fReaderWriter.Free;
    inherited;
end;

function TProcessTraceListDataCache.FindProcessTrace(const aTraceName: string): TProcessTraceData;
var
    x: integer;
begin
    result := nil;
    for x := 0 to fProcessTraces.List.Count - 1 do
    begin
        if SameText((fProcessTraces.List.Items[x] as TProcessTraceData).TraceName, aTraceName) then
        begin
            result := fProcessTraces.List.Items[x] as TProcessTraceData;
            EXIT;
        end;
    end;
end;

procedure TProcessTraceListDataCache.Flush;
begin
    fReaderWriter.WriteToFile();
end;

{
  function TProcessTraceListDataCache.FindProcessTraceBySourceDataName(
  const aSourceDataName: string): TProcessTraceData;
  var
  x : integer;
  begin
  result := nil;
  for x := 0 to fProcessTraces.List.Count - 1 do begin
  if SameText( ( fProcessTraces.List.Items[ x ] as TProcessTraceData ).SourceDataName, aSourceDataName ) then begin
  result := fProcessTraces.List.Items[ x ] as TProcessTraceData;
  EXIT;
  end;
  end;
  end;
}
procedure TProcessTraceListDataCache.AddProcessTrace(aProcessTraceData: TProcessTraceData);
begin
    fProcessTraces.List.Add(aProcessTraceData);
    // aProcessTraceData.ID := IntToStr( fProcessTraces.List.Count - 1 );
    fReaderWriter.DataChanged();
    // fReaderWriter.AddObjectToItemsVirtualObject( aProcessTraceData, fProcessTraces.List.Tag );
end;

procedure TProcessTraceListDataCache.RemoveProcessTrace(const aTraceName: string;
    const aRemoveFromFile: boolean);
var
    xProcessTrace: TProcessTraceData;
begin
    if not aRemoveFromFile then
        EXIT;
    xProcessTrace := FindProcessTrace(aTraceName);
    fProcessTraces.List.Remove(xProcessTrace);
    fReaderWriter.DataChanged();
end;

function TProcessTraceListDataCache.Read: boolean;
begin
    result := false;
    if not fReaderWriter.ReadFromFile() then
        EXIT;
    FreeAndNil(fProcessTraces);

    fProcessTraces := fReaderWriter.CreateObjectFromRootNode<TProcessTraceListData>();
    if not Assigned(fProcessTraces) then
        EXIT;

    result := true;
end;

procedure TProcessTraceListDataCache.Init;
begin
    read();
    if not Assigned(fProcessTraces) then
    begin
        fProcessTraces := TProcessTraceListData.Create();
        fReaderWriter.AddObjectToRootNode(fProcessTraces);
    end;
end;

function TProcessTraceListDataCache.GetCount: integer;
begin
    result := fProcessTraces.List.Count;
end;

function TProcessTraceListDataCache.GetItemAt(aIndex: integer): TProcessTraceData;
begin
    result := fProcessTraces.List[aIndex] as TProcessTraceData;
end;

{ TProcessTraceData }

constructor TProcessTraceData.Create(const aTraceName: string);
begin
    inherited Create();
    fTraceName := aTraceName;
end;


// initialization
// RegisterClasses( [ TProcessTraceData, TProcessTraceListData ] );
end.
