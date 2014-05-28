unit ProgramCounterDataCache;


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
  ----------------------------------------------------------------------------------------------------------------------- }
interface


uses
    XMLReaderWriter,
    Streamable,
    RelativeMemAddressData;

type
    TProgramCounterData = class(TRelativeMemAddressData)
    public
        constructor Create(); override;
    end;

    TProgramCounterDataCache = class
    private
        fProgramCounterData: TProgramCounterData;
        fReaderWriter: TXMLReaderWriter;
    public
        constructor Create(const aPathName: string);
        destructor Destroy(); override;
        procedure Init();
        procedure Clear;
        function read(): boolean;
        procedure ChangeProgramCounter(const aLabelName: string; const aRelativeAddress: integer);
        procedure Flush();
        property ProgramCounterData: TProgramCounterData read fProgramCounterData;
    end;


implementation


uses
    SysUtils,
    Classes;

const
    cAutoFlushBufferCycle = 0;

    { TProgramCounterDataCache }

constructor TProgramCounterDataCache.Create(const aPathName: string);
begin
    inherited Create();
    fProgramCounterData := nil;
    fReaderWriter := TXMLReaderWriter.Create(aPathName, cAutoFlushBufferCycle);
end;

destructor TProgramCounterDataCache.Destroy;
begin
    fReaderWriter.Free;
    FreeAndnil(fProgramCounterData);
    inherited;
end;

procedure TProgramCounterDataCache.Flush;
begin
    fReaderWriter.WriteToFile();
end;

procedure TProgramCounterDataCache.Init();
begin
    fProgramCounterData := TProgramCounterData.Create();
    fReaderWriter.Activate();
    fReaderWriter.AddObjectToRootNode(fProgramCounterData);
end;

procedure TProgramCounterDataCache.Clear;
begin
    FreeAndNil(fProgramCounterData);
    fReaderWriter.DisActivate();
end;

function TProgramCounterDataCache.Read(): boolean;
begin
    result := false;
    if not fReaderWriter.ReadFromFile() then
        EXIT;
    fProgramCounterData := fReaderWriter.CreateObjectFromRootNode<TProgramCounterData>();
    if not Assigned(fProgramCounterData) then
        EXIT;
    result := true;
end;

procedure TProgramCounterDataCache.ChangeProgramCounter(const aLabelName: string;
    const aRelativeAddress: integer);
begin
    fProgramCounterData.LabelName := aLabelName;
    fProgramCounterData.RelativeAddress := aRelativeAddress;
    fReaderWriter.DataChanged();
    // fReaderWriter.SetObjectToVirtualObject( fProgramCounterData, fProgramCounterData.Tag );
end;

{ TProgramCounterData }

constructor TProgramCounterData.Create;
begin
    inherited;
end;

// initialization
// RegisterClasses( [TProgramCounterData ] );


end.
