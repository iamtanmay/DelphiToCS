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
  20.01.09 wl  AddDriver                    TN4358    neu
  28.01.09 wl  AddConnection,AddDriver      TN4243   overload-Methode mit Description
  ---------------------------------------------------------------------------------------------------------------------- }

unit DriverSettingList;


interface


uses
    Module,
    ModuleSettings,
    ModuleDataCache;

type
    TMSDriver = class(TMSModule)
    end;

    TMSDriverType = class(TMSModuleType)
    end;

    TDriverSettingList = class(TModuleSettingList)
    protected
        function CreateDataCache: TModuleDataCache; override;
    public
        procedure AddModuleType(const aDefaultValue: string); override;
        procedure AddConnection(const aSettingName: string; aID: TModuleID); overload;
        procedure AddConnection(const aSettingName, aDescription: string; aID: TModuleID); overload;
        procedure AddDriver(const aSettingName: string; aID: TModuleID); overload;
        procedure AddDriver(const aSettingName, aDescription: string; aID: TModuleID); overload;
    end;


implementation


uses
    ConnectionSettingList,
    DriverDataCache;

{ TDriverSettingList }

procedure TDriverSettingList.AddConnection(const aSettingName, aDescription: string; aID: TModuleID);
begin
    self.Add(TMSConnection.Create(aSettingName, aDescription, '', aID));
end;

procedure TDriverSettingList.AddConnection(const aSettingName: string; aID: TModuleID);
begin
    self.AddConnection(aSettingName, '', aID);
end;

procedure TDriverSettingList.AddDriver(const aSettingName, aDescription: string; aID: TModuleID);
begin
    self.Add(TMSDriver.Create(aSettingName, aDescription, '', aID));
end;

procedure TDriverSettingList.AddDriver(const aSettingName: string; aID: TModuleID);
begin
    self.AddDriver(aSettingName, '', aID);
end;

procedure TDriverSettingList.AddModuleType(const aDefaultValue: string);
begin
    self.Add(TMSDriverType.Create(aDefaultValue));
end;

function TDriverSettingList.CreateDataCache(): TModuleDataCache;
begin
    result := TDriversDataCache.Instance.CreateByModuleName(fSectionName);
end;


end.
