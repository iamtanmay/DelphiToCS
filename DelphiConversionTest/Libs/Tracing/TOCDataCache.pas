unit TOCDataCache;


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
  26.10.10 pk  TTOCListDataCache                     TN5297    Remove
  15.11.10 pk                                        TN5340    Changes to prevent memory leak
  ----------------------------------------------------------------------------------------------------------------------- }
interface


uses
    XMLReaderWriter,
    Streamable,
    GeneralTypes;

type
    TTOCData = class(TStreamable)
    private
        fPathName: string;
    published
        constructor Create(const aPathName: string); reintroduce;
        property PathName: string read fPathName write fPathName;
    end;

    TTOCListData = class(TStreamable)
    private
        fList: TStreamableObjectList;
    public
        constructor Create(); override;
        destructor Destroy(); override;
    published
        property List: TStreamableObjectList read fList write fList;

    end;

    TTOCListDataCache = class
    private
        fTOCs: TTOCListData;
        fReaderWriter: TXMLReaderWriter;
        function GetCount: integer;
        function GetTOCDataAt(aIndex: integer): TTOCData;
    public
        constructor Create(const aPathName: string);
        destructor Destroy(); override;
        procedure Init();
        procedure Clear();
        function read(): boolean;
        function FindTOCByPathName(const aPathName: string): TTOCData;
        procedure AddTOC(aTOCData: TTOCData);
        procedure RemoveTOC(aTOCData: TTOCData);
        property Count: integer read GetCount;
        property TOCs[aIndex: integer]: TTOCData read GetTOCDataAt; default;
    end;


implementation


uses
    SysUtils,
    Classes,
    FileUtilities;

{ TTOCListData }

constructor TTOCListData.Create;
begin
    inherited;
    fList := TStreamableObjectList.Create()
end;

destructor TTOCListData.Destroy;
begin
    FreeAndNil(fList);
    inherited;
end;

{ TTOCListDataCache }
constructor TTOCListDataCache.Create(const aPathName: string);
begin
    inherited Create();
    fReaderWriter := TXMLReaderWriter.Create(aPathName);
    fTOCs := nil;
end;

destructor TTOCListDataCache.Destroy;
begin
    FreeAndNil(fTOCs);
    fReaderWriter.Free;
    inherited;
end;

function TTOCListDataCache.FindTOCByPathName(const aPathName: string): TTOCData;
var
    x: integer;
    xTOC: TTOCData;
begin
    result := nil;
    for x := 0 to fTOCs.List.Count - 1 do
    begin
        xTOC := self[x];
        if SameText(xTOC.PathName, aPathName) then
        begin
            result := xTOC;
            EXIT;
        end;
    end;

end;

procedure TTOCListDataCache.AddTOC(aTOCData: TTOCData);
begin
    fTOCs.List.Add(aTOCData);
    fReaderWriter.DataChanged();
    fReaderWriter.WriteToFile();
end;

procedure TTOCListDataCache.RemoveTOC(aTOCData: TTOCData);
begin
    fTOCs.List.Remove(aTOCData);
    fReaderWriter.DataChanged();
    fReaderWriter.WriteToFile();
end;

function TTOCListDataCache.Read(): boolean;
begin
    result := false;
    if not fReaderWriter.ReadFromFile() then
        EXIT;
    fTOCs := fReaderWriter.CreateObjectFromRootNode<TTOCListData>();

    if not Assigned(fTOCs) then
        EXIT;

    result := true;
end;

procedure TTOCListDataCache.Init;
begin
    read();
    if not Assigned(fTOCs) then
    begin
        fTOCs := TTOCListData.Create();
        fReaderWriter.AddObjectToRootNode(fTOCs);
    end;
end;

function TTOCListDataCache.GetCount: integer;
begin
    result := fTOCs.List.Count;
end;

function TTOCListDataCache.GetTOCDataAt(aIndex: integer): TTOCData;
begin
    result := fTOCs.List[aIndex] as TTOCData;
end;

procedure TTOCListDataCache.Clear;
begin
    FreeAndNil(fTOCs);
    fReaderWriter.DisActivate();
end;

{ TTOCData }

constructor TTOCData.Create(const aPathName: string);
begin
    inherited Create();
    fPathName := aPathName;
end;


// initialization
// RegisterClasses( [ TTOCData, TTOCListData ] );
end.
