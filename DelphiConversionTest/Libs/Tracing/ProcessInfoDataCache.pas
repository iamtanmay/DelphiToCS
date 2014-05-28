unit ProcessInfoDataCache;


{ -----------------------------------------------------------------------------------------------------------------------
  Copyright © 2009 Zinsser Analytic GmbH - All rights reserved.
  Author       : Payman Kamali (pk)
  Description  :
  -----------------------------------------------------------------------------------------------------------------------
  Revision History:
  date     op  method                                track-no  improvement/change
  -------- --  ------------------------------------  --------- ----------------------------------------------------------
  20.02.09 pk                                        TN4232    Initial Revision
  25.11.09 pk                                        TN4898      XMLReaderWriter.FlushToFile renamed to DataChanged
  04.02.10 pk                                        TN4972    Various Changes
  15.11.10 pk                                        TN5340     Changes to prevent memory leak
  ----------------------------------------------------------------------------------------------------------------------- }
interface


uses
    XMLReaderWriter,
    Streamable,
    GeneralTypes,
    ThreadClasses;

type
    TProcessInfoData = class(TStreamable)
    private
        fTraceName: string;
        fSourceDataName: string;
        fIsCompleted: boolean;
        fIsRestart: boolean;
        fDateCreated: string;
        fDateLastStarted: string;
        fDateLastStopped: string;
        class function DateTimeValueToStr(const aValue: TDateTime): string;
    public
        constructor Create(const aTraceName, aSourceDataName: string; const aIsRestart: boolean;
            const aDateCreated: TDateTime); reintroduce;
        procedure ChangedDateLastStarted(const aValue: TDateTime);
        procedure ChangedDateLastStopped(const aValue: TDateTime);
    published
        property TraceName: string read fTraceName write fTraceName;
        property SourceDataName: string read fSourceDataName write fSourceDataName;
        property IsCompleted: boolean read fIsCompleted write fIsCompleted;
        property IsRestart: boolean read fIsRestart write fIsRestart;
        property DateCreated: string read fDateCreated write fDateCreated;
        property DateLastStarted: string read fDateLastStarted write fDateLastStarted;
        property DateLastStopped: string read fDateLastStopped write fDateLastStopped;
    end;

    TProcessInfoDataCache = class
    private
        fProcessInfoData: TProcessInfoData;
        fReaderWriter: TXMLReaderWriter;
        procedure DataChanged();
    public
        constructor Create(const aPathName: string);
        destructor Destroy(); override;
        procedure Init(const aTraceName, aSourceDataName: string; const aIsRestart: boolean;
            const aDateCreated: TDateTime);
        procedure Clear;
        function read(): boolean;

        procedure ChangedDateLastStarted(const aValue: TDateTime);
        procedure ChangedDateLastStopped(const aValue: TDateTime);
        procedure ChangeIsRestart(const aIsRestart: boolean);
        procedure ChangeIsCompleted(const aIsCompleted: boolean);
        property ProcessInfoData: TProcessInfoData read fProcessInfoData;
    end;


implementation


uses
    SysUtils,
    Classes;

{ TProcessInfoDataCache }

constructor TProcessInfoDataCache.Create(const aPathName: string);
begin
    inherited Create();
    fProcessInfoData := nil;
    fReaderWriter := TXMLReaderWriter.Create(aPathName, 1);
end;

destructor TProcessInfoDataCache.Destroy;
begin
    FreeAndNil(fProcessInfoData);
    fReaderWriter.Free;
    inherited;
end;

procedure TProcessInfoDataCache.Init(const aTraceName, aSourceDataName: string; const aIsRestart: boolean;
    const aDateCreated: TDateTime);
begin
    fProcessInfoData := TProcessInfoData.Create(aTraceName, aSourceDataName, aIsRestart, aDateCreated);
    fReaderWriter.Activate();
    fReaderWriter.AddObjectToRootNode(fProcessInfoData);
end;

procedure TProcessInfoDataCache.Clear;
begin
    FreeAndNil(fProcessInfoData);
    fReaderWriter.DisActivate();
end;

function TProcessInfoDataCache.Read(): boolean;
begin
    result := false;
    if not fReaderWriter.ReadFromFile() then
        EXIT;
    fProcessInfoData := fReaderWriter.CreateObjectFromRootNode<TProcessInfoData>();
    if not Assigned(fProcessInfoData) then
        EXIT;
    result := true;
end;

procedure TProcessInfoDataCache.DataChanged();
begin
    fReaderWriter.DataChanged();
    // fReaderWriter.SetObjectToVirtualObject( fProcessInfoData, fProcessInfoData.Tag );
end;

procedure TProcessInfoDataCache.ChangedDateLastStarted(const aValue: TDateTime);
begin
    fProcessInfoData.ChangedDateLastStarted(aValue);
    DataChanged();
end;

procedure TProcessInfoDataCache.ChangedDateLastStopped(const aValue: TDateTime);
begin
    fProcessInfoData.ChangedDateLastStopped(aValue);
    DataChanged();
end;

procedure TProcessInfoDataCache.ChangeIsRestart(const aIsRestart: boolean);
begin
    fProcessInfoData.IsRestart := aIsRestart;
    DataChanged();
end;

procedure TProcessInfoDataCache.ChangeIsCompleted(const aIsCompleted: boolean);
begin
    fProcessInfoData.IsCompleted := aIsCompleted;
    DataChanged();
end;

{ TProcessInfoData }

class function TProcessInfoData.DateTimeValueToStr(const aValue: TDateTime): string;
const
    cFormatTimeStamp = 'dd.mm.yyyy, hh:nn:ss';
begin
    result := FormatDateTime(cFormatTimeStamp, aValue);
end;

constructor TProcessInfoData.Create(const aTraceName, aSourceDataName: string; const aIsRestart: boolean;
    const aDateCreated: TDateTime);
begin
    inherited Create();
    fTraceName := aTraceName;
    fSourceDataName := aSourceDataName;
    fIsCompleted := false;
    fIsRestart := aIsRestart;
    fDateCreated := DateTimeValueToStr(aDateCreated);
    fDateLastStarted := fDateCreated;
    fDateLastStopped := '';
end;

procedure TProcessInfoData.ChangedDateLastStarted(const aValue: TDateTime);
begin
    fDateLastStarted := DateTimeValueToStr(aValue);
end;

procedure TProcessInfoData.ChangedDateLastStopped(const aValue: TDateTime);
begin
    fDateLastStopped := DateTimeValueToStr(aValue);
end;


initialization


// RegisterClasses( [TProcessInfoData ] );


end.
