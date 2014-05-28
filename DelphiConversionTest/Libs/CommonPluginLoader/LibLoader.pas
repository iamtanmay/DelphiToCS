{ --------------------------------------------------------------------------------------------------
  Copyright © 2007 Zinsser Analytic GmbH - All rights reserved.
  Author       :
  Description  :
  --------------------------------------------------------------------------------------------------
  Revision History:
  date     op  method                       track-no improvement/change
  -------- --  ---------------------------  -------- -----------------------------------------------
  14.04.08 wl                               TN4009   uses TypeInfo, Name changed to LibLoader
  02.09.09 pk                               TN4753   New uRefCount, CreateInstance, DestroyInstance
  04.11.09 pk                               TN4843   Replace TObjectList, TStringList, TStrings with Generics Lists and TStringArray
  27.03.13 wl                               TN6045   uses geändert
  -------------------------------------------------------------------------------------------------- }

unit LibLoader;


interface


uses
    Generics.Collections,
    Windows,
    TypeInfo;

type
    TLibLoaderFuncParam = array [0 .. 255] of Char;

    TModuleEventFunc = function(var vTypeInfoList: TTypeInfoList): Boolean;

    TLibLoader = class
    private
        fList: TList<string>;
        procedure UnloadLibs();
        constructor Create();
        class var uInstance: TLibLoader;
        class var uRefCount: integer;
        function DoLoadPackage(aLibName: TLibLoaderFuncParam; out oLibHandle: HMODULE): boolean;
        function LoadFunction(aDLLHandle: HMODULE; aDllFunction: TLibLoaderFuncParam;
            var aProcAddress: TFarProc): boolean;
        class property RefCount: integer read uRefCount write uRefCount;
    public
        destructor Destroy(); override;
        class procedure CreateInstance();
        class procedure DestroyInstance();
        class function Instance(): TLibLoader;
        procedure LoadLibFunction(aDLLName, aDLLFunction: string; var vTypeInfoList: TTypeInfoList);
    end;


implementation


uses
    SysUtils;

{ TLibLoader }
class procedure TLibLoader.CreateInstance();
begin
    if not Assigned(uInstance) then
        uInstance := TLibLoader.Create();

    Inc(uRefCount);
end;

class procedure TLibLoader.DestroyInstance;
begin
    if uRefCount = 1 then
        FreeAndNil(uInstance);

    Dec(uRefCount);
end;

class function TLibLoader.Instance(): TLibLoader;
begin
    result := uInstance;
end;

constructor TLibLoader.Create();
begin
    inherited Create();
    fList := TList<string>.Create;

end;

destructor TLibLoader.Destroy;
begin
    UnloadLibs;
    FreeAndNil(fList);
    inherited;
end;

procedure TLibLoader.UnloadLibs();
var
    x: integer;
    xHandle: HMODULE;
    xLibName: string;
begin
    for x := 0 to fList.Count - 1 do
    begin
        xLibName := fList[x];
        if (UpperCase(ExtractFileExt(xLibName)) = '.BPL') then
        begin
            xHandle := GetModuleHandle(PChar(xLibName));
            UnloadPackage(xHandle);
        end;
    end;
end;

function TLibLoader.DoLoadPackage(aLibName: TLibLoaderFuncParam; out oLibHandle: HMODULE): boolean;
begin
    result := false;
    oLibHandle := GetModuleHandle(aLibName);
    if oLibHandle > 0 then
    begin
        result := true;
        EXIT;
    end;

    oLibHandle := LoadPackage(aLibName);
    if (oLibHandle = 0) then
    begin
        Exit;
    end;

    fList.Add(aLibName);
    result := true;
end;

function TLibLoader.LoadFunction(aDLLHandle: HMODULE; aDllFunction: TLibLoaderFuncParam;
    var aProcAddress: TFarProc): boolean;
begin
    result := false;
    aProcAddress := GetProcAddress(aDLLHandle, aDLLFunction);
    if (aProcAddress = nil) then
    begin
        Exit;
    end;
    result := true;
end;

procedure TLibLoader.LoadLibFunction(aDLLName, aDLLFunction: string; var vTypeInfoList: TTypeInfoList);
var
    xDLLHandle: HMODULE;
    xProcAddress: TFarProc;
    xDLLName, xDLLFunction: TLibLoaderFuncParam;
    xFunc: TModuleEventFunc;
begin
    StrPCopy(xDllName, aDLLName);
    StrPCopy(xDLLFunction, aDLLFunction);

    if not DoLoadPackage(xDLLName, xDLLHandle) then
        Exit;
    if not LoadFunction(xDLLHandle, xDLLFunction, xProcAddress) then
        Exit;
    @xFunc := xProcAddress;
    xFunc(vTypeInfoList);
end;


initialization


TLibLoader.RefCount := 0;


finalization


// uLoadedLibs.Free;


end.
