unit AppInstanceEditingMethod;


interface


type
    TAppInstanceEditingMethod = class
    private
        class var uInstance: TAppInstanceEditingMethod;
        constructor Create();
    public
        destructor Destroy(); override;
        class procedure CreateInstance();
        class procedure DestroyInstance();
        class property Instance: TAppInstanceEditingMethod read uInstance;
    end;


implementation


uses
    SysUtils,
    AppInstanceMethodBuilding,
    MethodDataCache;

{ TAppInstanceEditingMethod }

constructor TAppInstanceEditingMethod.Create;
begin
    inherited Create();

    TAppInstanceMethodBuilding.CreateInstance();

    TMethodDataCache.CreateInstance();
end;

class procedure TAppInstanceEditingMethod.CreateInstance;
begin
    uInstance := TAppInstanceEditingMethod.Create();
end;

destructor TAppInstanceEditingMethod.Destroy;
begin
    TMethodDataCache.DestroyInstance();

    TAppInstanceMethodBuilding.DestroyInstance();

    inherited;
end;

class procedure TAppInstanceEditingMethod.DestroyInstance;
begin
    FreeAndNil(uInstance)
end;


end.
