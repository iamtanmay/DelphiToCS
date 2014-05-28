{ --------------------------------------------------------------------------------------------------
  Copyright © 2003 Zinsser Analytic GmbH - All rights reserved.
  Author       : Wolfgang Lyncke (wl)
  Description  : Data Adaptor should become the base class for all TDBDataset-based Data Adaptors
  --------------------------------------------------------------------------------------------------
  Revision History:
  date     op  method                       track-no improvement/change
  -------- --  ---------------------------  -------- -----------------------------------------------
  07.02.03 wl                               TN1334.3 initial version
  11.02.03 wl  GetReason                    TN1334.3 jetzt als allgemeine Funktion
  12.03.03 wl                               TN1293.5 alle Methoden als class-Methoden
  27.04.04 pk  SetRange, CancelRange        TN1880   Select and Cancel a range
  27.04.04 pk  CommonSavAs, CommonDelete    TN1880   Now call SetRange and CancelRange
  27.04.04 pk  CopyDatasetRecord            TN1880   Copy a single record from one dataset to another
  01.11.04 wl  GetReason                    TN2198.1 ist jetzt public
  01.11.04 wl  GetReason                    TN2198.1 Reason muss immer mindestens 4 Zeichen lang sein
  04.03.05 pk  GetReason                    TN2330.1 ---> TAppSettings ( to avoid "uses CommonTypes" )
  04.03.05 pk  SetAliases                   TN2330.1 New
  23.06.05 pk                               TN2471.1 remove resourceloader from uses
  11.01.06 pk  InstallTable                 TN2871.1 New functions for update management
  21.09.06 wl  VerifyTable                  TN3326   aFirstInit: Um Basis-Daten einzutragen, aber nur wenn es die Tabelle noch nicht gab
  05.02.07 pk                               TN3544   Changes for updatemanager
  22.02.07 pk  StandardCreateQuery          TN3583   name changed from CreateQuery
  06.08.07 wl  GetUserAlias,-AliasPath      TN3811.3 für CFR21 User Management - wird noch nicht benutzt
  06.08.07 wl  UserGetLevelName             TN3811.3 von UserDataAdaptor hierher
  07.08.07 wl  CopyDatasetRecord            TN3811.3 --> DatasetUtils
  07.08.07 wl  CommonDelete,CommonSaveAs    TN3811.3 --> DatasetUtils
  07.08.07 wl  CreateUpdater,TableName      TN3811.3 --> QueryDataAdaptor
  31.08.07 wl  GetSystemDataPassword        TN3811.4 neu für CFR21
  06.07.09 pk                               TN4585.4    uMainAliasPath moved to DataProvider
  13.07.09 pk                               TN4585.4 GetRunAlias removed
  04.08.10 pk                               TN5218   Changes for Plugin database packages
  19.10.10 pk                               TN5305   changes needed for CoreClient/Server
  -------------------------------------------------------------------------------------------------- }

unit DataAdaptor;


interface


uses
    CommonTypes;

type
    TDataAdaptor = class
    public
        class function UserGetLevelName(aUserLevel: TUserLevel): string;
        class function GetCFR21Mode(): TCFR21ComplianceMode;
    end;


implementation


uses
    DataProviderFactory;

class function TDataAdaptor.UserGetLevelName(aUserLevel: TUserLevel): string;
begin
    case aUserLevel of
        usrGuest:
            result := 'Guest';
        usrSystem:
            result := 'System User';
        usrSystemAdmin:
            result := 'System Administrator';
        usrSupervisor:
            result := 'Supervisor';
        usrUnlimitedAdmin:
            result := 'Administrator';
        else
            result := 'None';
    end;
end;

class function TDataAdaptor.GetCFR21Mode: TCFR21ComplianceMode;
begin
    result := ccmNone;
    if (TDataProviderFactory.Instance.UserDBAlias <> '') then
    begin
        result := ccmPrepared;
        if (TDataProviderFactory.Instance.SystemDataPassword <> '') then
            result := ccmCompliant;
    end;
end;


end.
