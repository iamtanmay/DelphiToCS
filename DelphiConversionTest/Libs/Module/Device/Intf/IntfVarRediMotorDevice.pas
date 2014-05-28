unit IntfVarRediMotorDevice;


interface


uses
    IntfDevice;

type
    IVarRediMotorDevice = interface(IDevice)
        ['{6466C04B-8D8D-4D95-8501-9478AF3F3C79}']
        procedure SetMinMaxVols(aMinVolume, aMaxVolume: double);
        procedure SetVol(aVolume: double; var vMotorSteps: integer);
    end;


implementation


end.
