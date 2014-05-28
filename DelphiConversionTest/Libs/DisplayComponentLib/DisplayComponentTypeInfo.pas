{ -----------------------------------------------------------------------------------------------------------------------
  Copyright © 2009 Zinsser Analytic GmbH - All rights reserved.
  Author       : Payman Kamali (pk)
  Description  :
  -----------------------------------------------------------------------------------------------------------------------
  Revision History:
  date     op  method                       track-no improvement/change
  -------- --  ---------------------------  -------- --------------------------------------------------------------------
  06.04.09 pk                                TN4503  Initial Revision
  27.08.09 pk                                TN4753  Various Changes
  13.11.12 wl                                TN6015   überarbeitet für DisplayComponentsDataAdaptor
  29.11.12 wl                                TN6015.2 DesignDisplayComponents abgeschafft (Es gibt nur noch RunDisplayComponents)
  ----------------------------------------------------------------------------------------------------------------------- }

unit DisplayComponentTypeInfo;


interface


uses
    DisplayComponentIntf,
    DisplayComponent,
    DisplayComponentSettings,
    TypeInfo;

type
    TDisplayComponentTypeInfo = class(TTypeInfo)
    private
        fDisplayComponentClass: TDisplayComponentClass;
    public
        constructor Create(const aTypeName, aTypeInfoVersion, aLibName, aLibVersion: string;
            aDisplayComponentClass: TDisplayComponentClass);
        function SupportsDisplayComponent(aDisplayComponentID: TDisplayComponentID): boolean;
        function CreateDisplayComponent(const aDisplayComponentName: string;
            const aDisplayComponentSettings: TDisplayComponentSettingList): IDisplayComponent;
    end;

    TRunDisplayComponentTypeInfo = class(TDisplayComponentTypeInfo)
    end;

    TDisplayComponentSettingsTypeInfo = class(TTypeInfo)
    private
        fDisplayComponentClass: TDisplayComponentClass;
        fDisplayComponentSettingsClass: TDisplayComponentSettingsClass;
    public
        constructor Create(const aTypeName, aTypeInfoVersion, aLibName, aLibVersion: string;
            aDisplayComponentClass: TDisplayComponentClass;
            aDisplayComponentSettingsClass: TDisplayComponentSettingsClass);
        function SupportsDisplayComponent(aDisplayComponentID: TDisplayComponentID): boolean;
        function CreateDisplayComponentSettings(const aDisplayComponentName: string)
            : TDisplayComponentSettingList;
    end;


implementation


uses
    SysUtils;

{ TDisplayComponentTypeInfo }

constructor TDisplayComponentTypeInfo.Create(const aTypeName, aTypeInfoVersion, aLibName, aLibVersion: string;
    aDisplayComponentClass: TDisplayComponentClass);
begin
    inherited Create(aTypeName, aTypeInfoVersion, aLibName, aLibVersion);

    fDisplayComponentClass := aDisplayComponentClass;
end;

function TDisplayComponentTypeInfo.CreateDisplayComponent(const aDisplayComponentName: string;
    const aDisplayComponentSettings: TDisplayComponentSettingList): IDisplayComponent;
begin
    result := fDisplayComponentClass.Create(aDisplayComponentName);
    result.DisplayComponentSettings := aDisplayComponentSettings;
end;

function TDisplayComponentTypeInfo.SupportsDisplayComponent(aDisplayComponentID: TDisplayComponentID)
    : boolean;
begin
    result := Supports(fDisplayComponentClass, aDisplayComponentID);
end;

{ TDisplayComponentSettingsTypeInfo }

constructor TDisplayComponentSettingsTypeInfo.Create(const aTypeName, aTypeInfoVersion, aLibName,
    aLibVersion: string; aDisplayComponentClass: TDisplayComponentClass;
    aDisplayComponentSettingsClass: TDisplayComponentSettingsClass);
begin
    inherited Create(aTypeName, aTypeInfoVersion, aLibName, aLibVersion);
    fDisplayComponentClass := aDisplayComponentClass;
    fDisplayComponentSettingsClass := aDisplayComponentSettingsClass;
end;

function TDisplayComponentSettingsTypeInfo.CreateDisplayComponentSettings(const aDisplayComponentName: string)
    : TDisplayComponentSettingList;
begin
    result := fDisplayComponentSettingsClass.Create(aDisplayComponentName, self.TypeName);
end;

function TDisplayComponentSettingsTypeInfo.SupportsDisplayComponent(aDisplayComponentID
    : TDisplayComponentID): boolean;
begin
    result := Supports(fDisplayComponentClass, aDisplayComponentID);
end;


end.
