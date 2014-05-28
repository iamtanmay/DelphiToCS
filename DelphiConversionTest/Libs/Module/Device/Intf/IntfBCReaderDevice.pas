unit IntfBCReaderDevice;


interface


uses
    IntfDevice,
    Driver;

type
    IBCReaderDevice = interface(IDevice)
        ['{579AA339-A315-480C-A0CF-75D021927D49}']
        procedure Init(aInitID: TDevInitID);
        procedure Reset();
        procedure TriggerOn();
        function GetBarcode(): string; overload;
        function LongRead(): string; overload;
        procedure ResetWithRead(var vBarcode: string); overload;
        function GetBarcode(const aCodeFilter: string): string; overload;
        function LongRead(const aCodeFilter: string): string; overload;
        procedure ResetWithRead(const aCodeFilter: string; var vBarcode: string); overload;
        function GetCodeFilter: string;
        property CodeFilter: string read GetCodeFilter;
    end;


implementation


end.
