unit CFR21UserLogPrint;
{--------------------------------------------------------------------------------------------------
 Copyright © 2004 Zinsser Analytic GmbH - All rights reserved.
 Author       : Wolfgang Lyncke (wl)
 Project      : UserManager.exe
 Description  : Print-Form for the Supervisor to look at the user log entries (CFR21 only)
 --------------------------------------------------------------------------------------------------
 Revision History:
 Date     op  Method                       Track-no Improvement/Change
 -------- --  ---------------------------  -------- -----------------------------------------------
 26.10.04 wl                               TN2185.9 initial version
 09.08.07 wl                               TN3827   Create-Funktion --> CFR21UserLogForm
 --------------------------------------------------------------------------------------------------}

interface

uses
  Forms, Classes, Controls, Qrctrls, QuickRpt, ExtCtrls;

type
  TfrmQRLogPrint = class(TForm)
    QuickRep1: TQuickRep;
    DetailBand1: TQRBand;
    ColumnHeaderBand1: TQRBand;
    QRDBText1: TQRDBText;
    QRDBText2: TQRDBText;
    QRDBText3: TQRDBText;
    QRDBText4: TQRDBText;
    QRLabel1: TQRLabel;
    QRLabel2: TQRLabel;
    QRLabel3: TQRLabel;
    QRLabel4: TQRLabel;
    QRLabel5: TQRLabel;
    QRLabel6: TQRLabel;
    QRDBText5: TQRDBText;
    QRDBText6: TQRDBText;
    QRLabel7: TQRLabel;
    QRSysData2: TQRSysData;
    QRLabel8: TQRLabel;
    QRLabel9: TQRLabel;
  end;

implementation

{$R *.DFM}

end.
