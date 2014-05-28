unit IntfWatchDogDevice;


interface


type
    IWatchDogDevice = interface(IInterface)
        ['{C5C96900-5F32-4943-8E0F-A0E28FF1514E}']
        procedure Start();
        procedure Stop();
        procedure Trigger();
        procedure Wait();
    end;


implementation


end.
