{ -----------------------------------------------------------------------------------------------------------------------
  Copyright © 2009 Zinsser Analytic GmbH - All rights reserved.
  Author       : Payman Kamali (pk)
  Description  :
  -----------------------------------------------------------------------------------------------------------------------
  Revision History:
  date     op  method                       track-no improvement/change
  -------- --  ---------------------------  -------- --------------------------------------------------------------------
  06.04.09 pk                                TN4503  Initial Revision
  24.07.09 pk  DisplayID                     TN4675  New
  04.09.10 pk                                TN5042  New Enabled property
  29.11.12 wl                                TN6015.2 DesignDisplayComponents abgeschafft (Es gibt nur noch RunDisplayComponents)
  ----------------------------------------------------------------------------------------------------------------------- }

unit DisplayComponent;


interface


uses
    GeneralTypes,
    DisplayComponentSettings,
    DisplayComponentIntf;

type
    TDisplayComponentClass = class of TDisplayComponent;

    TDisplayComponent = class(TInterfacedObject, IDisplayComponent)
    private
        fName: string;
        fContextID: string;
        fLoaded: boolean;
        fIsDesignMode: boolean;
        function GetDisplayComponentSettings(): TDisplayComponentSettingList;
        procedure SetDisplayComponentSettings(aSettings: TDisplayComponentSettingList);
    protected
        fDisplayID: string;
        fSettings: TDisplayComponentSettingList;
        procedure SetParentDisplayHandle(const aValue: TDisplayHandle); virtual;
        function GetDisplayHandle(): TDisplayHandle; virtual;
        function GetChildComponentNames(): TStringArray; virtual;

        function GetName(): string;

        function GetVisible: boolean; virtual;
        procedure SetVisible(const aValue: boolean); virtual;
        function GetContextID(): string;
        procedure SetContextID(const aValue: string);
        function GetDisplayID: string; virtual;
        procedure SetDisplayID(const aValue: string); virtual;
        function GetEnabled: boolean; virtual;
        procedure SetEnabled(const aValue: boolean); virtual;
        // ---------------------------------------------------------------
    public
        constructor Create(const aName: string); virtual;
        destructor Destroy(); override;

        procedure SetComponentMode(const aIsDesignMode: boolean);
        procedure Load(); virtual;
        procedure UnLoad(); virtual;

        procedure AddChildComponent(const aValue: IDisplayComponent); virtual;

        property name: string read GetName;
        property DisplayComponentSettings: TDisplayComponentSettingList read GetDisplayComponentSettings
            write SetDisplayComponentSettings;
        property Visible: boolean read GetVisible write SetVisible;
        property ContextID: string read GetContextID write SetContextID;
        property DisplayID: string read fDisplayID;
        property IsDesignMode: boolean read fIsDesignMode;
    end;


implementation


uses
    SysUtils;

{ TDisplayComponent }

constructor TDisplayComponent.Create(const aName: string);
begin
    inherited Create();
    fName := aName;
    fSettings := nil;
    fLoaded := false;
    fDisplayID := '';
end;

destructor TDisplayComponent.Destroy;
begin
    fSettings.Free;
    inherited;
end;

procedure TDisplayComponent.SetDisplayComponentSettings(aSettings: TDisplayComponentSettingList);
begin
    fSettings := aSettings;
end;

function TDisplayComponent.GetName: string;
begin
    result := fName;
end;

procedure TDisplayComponent.Load();
begin
    if fLoaded then
        EXIT;

    fSettings.Prepare();
    fLoaded := true;
end;

procedure TDisplayComponent.UnLoad;
begin
    fLoaded := false;
end;

function TDisplayComponent.GetDisplayComponentSettings: TDisplayComponentSettingList;
begin
    result := fSettings;
end;

function TDisplayComponent.GetDisplayHandle: TDisplayHandle;
begin
    result := nil;
end;

procedure TDisplayComponent.SetParentDisplayHandle(const aValue: TDisplayHandle);
begin

end;

function TDisplayComponent.GetChildComponentNames: TStringArray;
begin
    SetLength(result, 0);
end;

function TDisplayComponent.GetVisible: boolean;
begin
    result := false;
end;

procedure TDisplayComponent.SetVisible(const aValue: boolean);
begin
end;

procedure TDisplayComponent.AddChildComponent(const aValue: IDisplayComponent);
begin

end;

procedure TDisplayComponent.SetComponentMode(const aIsDesignMode: boolean);
begin
    fIsDesignMode := aIsDesignMode;
end;

function TDisplayComponent.GetContextID: string;
begin
    result := fContextID;
end;

procedure TDisplayComponent.SetContextID(const aValue: string);
begin
    fContextID := aValue;
end;

function TDisplayComponent.GetDisplayID: string;
begin
    result := fDisplayID;
end;

procedure TDisplayComponent.SetDisplayID(const aValue: string);
begin
    fDisplayID := aValue;
end;

function TDisplayComponent.GetEnabled: boolean;
begin
    result := false;
end;

procedure TDisplayComponent.SetEnabled(const aValue: boolean);
begin

end;


end.
