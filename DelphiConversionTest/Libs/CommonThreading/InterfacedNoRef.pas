unit InterfacedNoRef;


interface


type
    TInterfacedNoRef = class(TObject, IUnknown)
        function QueryInterface(const IID: TGUID; out Obj): HResult; stdcall;
        function _AddRef: Integer; stdcall;
        function _Release: Integer; stdcall;
    end;


implementation


{ TInterfacedNoRef }

function TInterfacedNoRef.QueryInterface(const IID: TGUID; out Obj): HResult; stdcall;
const
    E_NOINTERFACE = HResult($80004002);
begin
    if GetInterface(IID, Obj) then
        Result := 0
    else
        Result := E_NOINTERFACE;
end;

function TInterfacedNoRef._AddRef: Integer; stdcall;
begin
    result := 0;
end;

function TInterfacedNoRef._Release: Integer; stdcall;
begin
    Result := 0;
end;


end.
