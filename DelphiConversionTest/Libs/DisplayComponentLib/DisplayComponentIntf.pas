{ -----------------------------------------------------------------------------------------------------------------------
  Copyright © 2009 Zinsser Analytic GmbH - All rights reserved.
  Author       : Payman Kamali (pk)
  Description  :
  -----------------------------------------------------------------------------------------------------------------------
  Revision History:
  date     op  method                       track-no improvement/change
  -------- --  ---------------------------  -------- --------------------------------------------------------------------
  06.04.09 pk                                TN4503  Initial Revision
  07.04.09 pk  IMainDisplayComponent         TN4503  New
  24.07.09 pk  IDisplayComponent             TN4675  New: DisplayID
  24.07.09 pk  IRunInfoPanelDisplayComponent TN4675  New
  31.07.09 ts  IRunInfoPanelDisplayComponent TN4666  InfoGroupBehaviour instead of HideGroup
  13.11.09 ts  IShowChartDisplayComponent    TN4785  New
  04.09.10 pk  IDisplayComponent             TN5042  New Enabled property
  25.05.11 ts  ISQLTableDisplayComponent     TN5590  new
  28.08.13 wl  IShowChartDisplayComponent    TN6236   entfernt
  ----------------------------------------------------------------------------------------------------------------------- }

unit DisplayComponentIntf;


interface


uses
    DisplayComponentSettings,
    Controls,
    AppTypes;

type
    TDisplayHandle = TWinControl;

    IDisplayComponent = interface(IInterface)
        ['{EC06DFE1-6B2D-48D5-84CE-7BD292AA4629}']
        procedure SetComponentMode(const aIsDesignMode: boolean);
        procedure Load();
        procedure UnLoad();
        function GetName(): string;
        function GetContextID(): string;
        procedure SetContextID(const aValue: string);
        procedure SetDisplayComponentSettings(aSettings: TDisplayComponentSettingList);
        function GetDisplayComponentSettings(): TDisplayComponentSettingList;
        procedure SetParentDisplayHandle(const aValue: TDisplayHandle);
        function GetDisplayHandle(): TDisplayHandle;
        function GetChildComponentNames: TArray<string>;
        function GetVisible: boolean;
        procedure SetVisible(const aValue: boolean);
        function GetDisplayID: string;
        procedure SetDisplayID(const aValue: string);
        function GetEnabled: boolean;
        procedure SetEnabled(const aValue: boolean);
        procedure AddChildComponent(const aValue: IDisplayComponent);
        property name: string read GetName;
        property DisplayComponentSettings: TDisplayComponentSettingList read GetDisplayComponentSettings
            write SetDisplayComponentSettings;
        property DisplayHandle: TDisplayHandle read GetDisplayHandle;
        property ChildComponentNames: TArray<string>read GetChildComponentNames;
        property Visible: boolean read GetVisible write SetVisible;
        property ContextID: string read GetContextID write SetContextID;
        property DisplayID: string read GetDisplayID write SetDisplayID;
        property Enabled: boolean read GetEnabled write SetEnabled;
    end;

    IMainDisplayComponent = interface(IDisplayComponent)
        ['{51A79826-85DE-411A-B535-6529476B60BC}']
    end;

    IRunInfoPanelDisplayComponent = interface(IDisplayComponent)
        ['{64BBFA2F-BB47-451D-AFEB-76730B1A280C}']
        procedure InsertInfo(const aGroupNames: TArray<string>; const aKey, aText: string;
            aInfoGroupBehaviour: TInfoGroupBehaviour);
    end;

    IButtonDisplayComponent = interface(IDisplayComponent)
        ['{D53010DA-2664-4B67-8AC0-9D465021805B}']
    end;

    IStopButtonDisplayComponent = interface(IButtonDisplayComponent)
        ['{F82297F9-80CE-41C2-886D-1DC54B7DF378}']
    end;

    ISQLTableDisplayComponent = interface(IDisplayComponent)
        ['{CFAD842B-7F39-498E-A108-E1CCDFBBFEA0}']
        procedure DoRefresh;
    end;

    TDisplayComponentNotifyEvent = procedure(const aSender: IDisplayComponent; const aEventName: string)
        of object;


implementation


end.
