unit RunActionTypeDictionary;


interface


uses
    Classes,
    TypeDictionary,
    TypeInfo,
    Action;

type
    TRunActionTypeDictionary = class(TTypeDictionary)
    protected
        function IsValidType(aTypeInfo: TTypeInfo): boolean; override;
        procedure InitTypeInfoList; override;
    public
        class procedure CreateInstance();
        class function Instance(): TRunActionTypeDictionary;
        class procedure DestroyInstance();
    end;


    // ##################################################################################################


implementation


uses
    RunActionTypeInfo,
    PluginLoader;

var
    uRunActionTypeDictionary: TRunActionTypeDictionary = nil;

class procedure TRunActionTypeDictionary.CreateInstance();
begin
    if Assigned(uRunActionTypeDictionary) then
        EXIT;
    uRunActionTypeDictionary := TRunActionTypeDictionary.Create('RunAction Type Dictionary');
end;

class procedure TRunActionTypeDictionary.DestroyInstance();
begin
    uRunActionTypeDictionary.Free;
end;

class function TRunActionTypeDictionary.Instance(): TRunActionTypeDictionary;
begin
    result := uRunActionTypeDictionary;
end;

function TRunActionTypeDictionary.IsValidType(aTypeInfo: TTypeInfo): boolean;
begin
    result := aTypeInfo is TRunActionTypeInfo;
end;

procedure TRunActionTypeDictionary.InitTypeInfoList;
begin
    TPluginLoader.LoadAllTypes(self);
end;


end.
