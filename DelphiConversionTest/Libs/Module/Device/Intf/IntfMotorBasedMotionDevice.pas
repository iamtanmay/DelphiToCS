{ --------------------------------------------------------------------------------------------------
  Copyright © 2007 Zinsser Analytic GmbH - All rights reserved.
  Author       : Payman Kamali (pk)
  Description  :
  --------------------------------------------------------------------------------------------------
  Revision History:
  date     op  method                       track-no improvement/change
  -------- --  ---------------------------  -------- -----------------------------------------------
  05.09.07 pk                                         Initial Revision
  09.11.07 pk                               TN3924    Steps to mm
  29.01.08 wl                               TN3980   uses geändert
  12.09.09 wl                               TN4740   MPOS durch integer ersetzt
  12.09.09 wl                               TN4740   default-Werte aus interface entfernt
  22.03.11 wl  GetVMotor                    TN5401   neu
  06.04.11 wl  GetVMotor                    TN5401   wieder entfernt
  15.11.11 wl  IXYZRMotorMotionDevice       TN5736   --> MotionSytemTravel
  16.01.13 wl  SetZProps                    TN6068   entfernt
  -------------------------------------------------------------------------------------------------- }

unit IntfMotorBasedMotionDevice;


interface


uses
    IntfMotionDevice,
    IntfMotorDriver,
    IntfMotorDevice,
    IntfMultiZMotorDevice,
    IntfMultiYMotorDevice;

type
    IMotorBasedMotionDevice = interface(IMotionDevice)
        ['{A70B2E74-F892-4D16-B717-8603345184FC}']
        function GetXMotor: IXMotorDevice;
        function GetYMotor: IYMotorDevice;
        function GetZMotor: IZMotorDevice;
        function GetRMotor: IRMotorDevice;
        function GetZMotors: IMultiZMotorDevice;
        function GetYMotors: IMultiYMotorDevice;

        property XMotor: IXMotorDevice read GetXMotor;
        property YMotor: IYMotorDevice read GetYMotor;
        property ZMotor: IZMotorDevice read GetZMotor;
        property RMotor: IRMotorDevice read GetRMotor;
        property YMotors: IMultiYMotorDevice read GetYMotors;
        property ZMotors: IMultiZMotorDevice read GetZMotors;
    end;


implementation


end.
