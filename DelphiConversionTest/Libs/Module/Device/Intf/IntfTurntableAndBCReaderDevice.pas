unit IntfTurntableAndBCReaderDevice;


interface


uses
    IntfDevice;

type
    ITurntableAndBCReaderDevice = interface(IDevice)
        ['{A5133C96-982F-4121-A09E-4CDBE972B47A}']
        function ReadTubeID: string;
        function HasTubeReader: boolean;
    end;


implementation


end.
