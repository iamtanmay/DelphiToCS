unit IntfLevelDetectionDevice;


interface


uses
    CommonTypes,
    IntfDevice;

type
    ILevelDetectionDevice = interface(IDevice)
        ['{46BA82DD-7293-471A-9EFF-012CAE1D0AEE}']
        function DetectLevel(): TPosMM;
        function GetDetectPos(var vDetectPos: TPosMM): boolean;
    end;


implementation


end.
