{ ----------------------------------------------------------------------------------------------------------------------
  Copyright  2008 Zinsser Analytic GmbH - All rights reserved.
  Author       : Payman Kamali (pk)
  Description  :
  ----------------------------------------------------------------------------------------------------------------------
  Revision History:
  date     op  method                       track-no improvement/change
  -------- --  ---------------------------  -------- -------------------------------------------------------------------
  28.09.07 pk                                        Initial Revision
  13.10.08 pk  CreateDataCache              TN4272.2 New
  28.01.09 wl  AddConnection                TN4243   overload-Methode mit Description
  ---------------------------------------------------------------------------------------------------------------------- }

unit ConnectionSettingList;


interface


uses
    ModuleSettings,
    ModuleDataCache;

type
    TMSConnectionType = class(TMSModuleType)
    end;

    TMSConnection = class(TMSModule)
    end;

    TConnectionSettingList = class(TModuleSettingList)
    protected
        fEnabled: boolean;
        procedure AddSettings(); override;
        procedure LoadSettings(); override;
        function CreateDataCache(): TModuleDataCache; override;
    public
        procedure AddModuleType(const aDefaultValue: string); override;
        procedure AddConnection(const aSettingName: string; aID: TModuleID); overload;
        procedure AddConnection(const aSettingName, aDescription: string; aID: TModuleID); overload;
        property Enabled: boolean read fEnabled;
    end;


implementation


uses
    ConnectionDataCache;

{ TConnectionSettingList }

procedure TConnectionSettingList.AddConnection(const aSettingName, aDescription: string; aID: TModuleID);
begin
    self.Add(TMSConnection.Create(aSettingName, aDescription, '', aID));
end;

procedure TConnectionSettingList.AddConnection(const aSettingName: string; aID: TModuleID);
begin
    self.AddConnection(aSettingName, '', aID);
end;

procedure TConnectionSettingList.AddModuleType(const aDefaultValue: string);
begin
    self.Add(TMSConnectionType.Create(aDefaultValue));
end;

procedure TConnectionSettingList.AddSettings;
begin
    inherited;
    AddBool('Enabled', true);
end;

procedure TConnectionSettingList.LoadSettings;
begin
    fEnabled := Find('Enabled').AsBool;
end;

function TConnectionSettingList.CreateDataCache(): TModuleDataCache;
begin
    result := TConnectionsDataCache.Instance.CreateByModuleName(fSectionName);
end;


end.
