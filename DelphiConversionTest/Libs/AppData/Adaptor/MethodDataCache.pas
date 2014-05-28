{ --------------------------------------------------------------------------------------------------
  Copyright © 2008 Zinsser Analytic GmbH - All rights reserved.
  Author       : Payman Kamali (pk)
  Description  : Data cache for liquid handling parameters
  --------------------------------------------------------------------------------------------------
  Revision History:
  date     op  method                       track-no improvement/change
  -------- --  ---------------------------  -------- -----------------------------------------------
  07.10.08 pk                                TN4265   Initial Revision
  04.11.09 pk                                TN4843 Replace TObjectList, TStringList, TStrings with Generics Lists and TStringArray
  -------------------------------------------------------------------------------------------------- }

unit MethodDataCache;


interface


uses
    GeneralTypes,
    ListClasses,
    MethodDataAdaptor;

type

    TMethodDataCache = class
    private
        fDA: TMethodDataAdaptor;
        fMethodNames: TStringKeyList;
        fMethodNamesLastRead: TDateTime;
        procedure RefreshMethodNames();
    public
        constructor Create();
        destructor Destroy(); override;
        procedure AddMethodName(const aName: string);
        procedure DeleteMethodName(const aName: string);
        function GetMethodNames(): TStringArray;

        class function CreateInstance(): TMethodDataCache;
        class procedure DestroyInstance();
        class function Instance(): TMethodDataCache;
    end;


implementation


uses
    SysUtils;

var
    uInstMethodDataCache: TMethodDataCache = nil;

    { TMethodDataCache }

constructor TMethodDataCache.Create;
begin
    inherited Create();
    fDA := TMethodDataAdaptor.Create();
    fMethodNames := TStringKeyList.Create();
    fMethodNamesLastRead := 0;
    RefreshMethodNames();
end;

destructor TMethodDataCache.Destroy;
begin
    FreeAndNil(fMethodNames);
    FreeAndNil(fDA);
    inherited;
end;

class function TMethodDataCache.CreateInstance(): TMethodDataCache;
begin
    // create instance if instance does not exist
    if not Assigned(uInstMethodDataCache) then
        uInstMethodDataCache := TMethodDataCache.Create();

    // return instance
    result := uInstMethodDataCache;
end;

class function TMethodDataCache.Instance(): TMethodDataCache;
begin
    result := uInstMethodDataCache;
end;

class procedure TMethodDataCache.DestroyInstance;
begin
    FreeAndNil(uInstMethodDataCache);
end;

function TMethodDataCache.GetMethodNames(): TStringArray;
begin
    result := fMethodNames.ToArray;
end;

procedure TMethodDataCache.RefreshMethodNames;
var
    xNames: TStringArray;
begin
    fMethodNames.Clear();
    xNames := fDA.ReadAllNames();
    fMethodNames.AddRange(xNames);

end;

procedure TMethodDataCache.AddMethodName(const aName: string);
begin
    fMethodNames.Add(aName);
end;

procedure TMethodDataCache.DeleteMethodName(const aName: string);
var
    xIndex: integer;
begin
    xIndex := fMethodNames.IndexOf(aName);
    if xIndex < 0 then
        EXIT;
    fMethodNames.Delete(xIndex);
end;


end.
