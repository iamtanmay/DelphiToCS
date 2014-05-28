{ -----------------------------------------------------------------------------------------------------------------------
  Copyright © 2009 Zinsser Analytic GmbH - All rights reserved.
  Author       : Payman Kamali (pk)
  Description  :
  -----------------------------------------------------------------------------------------------------------------------
  Revision History:
  date     op  method                                track-no  improvement/change
  -------- --  ------------------------------------  --------- ----------------------------------------------------------
  12.10.09 pk                                        TN4812    Initial Revision
  29.05.12 wl  TCoreWriteLogEventMessageInfo         TN5904   mit ThreadID und DisplayType als Parameter
  27.03.13 wl                                        TN6045   uses geändert
  ----------------------------------------------------------------------------------------------------------------------- }

unit CoreEventMessageInfo;


interface


uses
    ThreadClasses,
    LogManager,
    GeneralTypes;

type
    TCoreEventMessageInfo = class(TMessageInfo);

    TCoreControlEventMessageInfo = class(TCoreEventMessageInfo)
    protected
        function GetMessageID(): integer; override;
    end;

    TCoreWriteLogEventMessageInfo = class(TCoreEventMessageInfo)
    private
        fLogText: string;
        fThreadID: integer;
        fDisplayType: TDisplayLogInfoType;
    protected
        function GetMessageID(): integer; override;
    public
        constructor Create(const aLogText: string; aThreadID: integer; aDisplayType: TDisplayLogInfoType);
        property LogText: string read fLogText;
        property DisplayType: TDisplayLogInfoType read fDisplayType;
        property ThreadID: integer read fThreadID;
    end;

    TCoreLoadDisplayComponentEventMessageInfo = class(TCoreEventMessageInfo)
    private
        fDisplayComponentName: string;
        fContextID: string;
    protected
        function GetMessageID: integer; override;
    public
        constructor Create(const aDisplayComponentName, aContextID: string);
        property DisplayComponentName: string read fDisplayComponentName;
        property ContextID: string read fContextID;
    end;

    TCoreLinkRunIDToProcesstEventMessageInfo = class(TCoreEventMessageInfo)
    private
        fRunID: string;
        fProcessID: string;
    protected
        function GetMessageID: integer; override;
    public
        constructor Create(const aRunID, aProcessID: string);
        property RunID: string read fRunID;
        property ProcessID: string read fProcessID;
    end;

    TCoreRunInfoInsertEventMessageInfo = class(TCoreEventMessageInfo)
    private
        fDisplayID: string;
        fGroupNames: TStringArray;
        fKey: string;
        fText: string;
        fInfoGroupBehaviour: integer;
    protected
        function GetMessageID: integer; override;
    public
        constructor Create(const aDisplayID: string; const aGroupNames: TStringArray;
            const aKey, aText: string; aInfoGroupBehaviour: integer);

        property DisplayID: string read fDisplayID write fDisplayID;
        property GroupNames: TStringArray read fGroupNames write fGroupNames;
        property Key: string read fKey write fKey;
        property Text: string read fText write fText;
        property InfoGroupBehaviour: integer read fInfoGroupBehaviour write fInfoGroupBehaviour;
    end;


implementation


const
    cMessageIDCoreControlEvent = 1;
    cMessageIDCoreWriteLogEvent = 10;
    cMessageIDCoreLoadDisplayComponentEvent = 11;
    cMessageIDCoreLinkRunIDToProcessEvent = 12;
    cMessageIDCoreRunInfoInsertEvent = 13;

    { TCoreWriteLogEventMessageInfo }

constructor TCoreWriteLogEventMessageInfo.Create(const aLogText: string; aThreadID: integer;
    aDisplayType: TDisplayLogInfoType);
begin
    inherited Create();
    fLogText := aLogText;
    fDisplayType := aDisplayType;
    fThreadID := aThreadID;
end;

function TCoreWriteLogEventMessageInfo.GetMessageID: integer;
begin
    result := cMessageIDCoreWriteLogEvent;
end;

{ TCoreLoadDisplayComponentEventMessageInfo }

constructor TCoreLoadDisplayComponentEventMessageInfo.Create(const aDisplayComponentName, aContextID: string);
begin
    inherited Create();
    fDisplayComponentName := aDisplayComponentName;
    fContextID := aContextID;
end;

function TCoreLoadDisplayComponentEventMessageInfo.GetMessageID: integer;
begin
    result := cMessageIDCoreLoadDisplayComponentEvent;
end;

{ TCoreLinkRunIDToProcesstEventMessageInfo }

constructor TCoreLinkRunIDToProcesstEventMessageInfo.Create(const aRunID, aProcessID: string);
begin
    inherited Create();
    fRunID := aRunID;
    fProcessID := aProcessID;
end;

function TCoreLinkRunIDToProcesstEventMessageInfo.GetMessageID: integer;
begin
    result := cMessageIDCoreLinkRunIDToProcessEvent;
end;

{ TCoreControlEventMessageInfo }

function TCoreControlEventMessageInfo.GetMessageID: integer;
begin
    result := cMessageIDCoreControlEvent;
end;

{ TCoreRunInfoInsertEventMessageInfo }

constructor TCoreRunInfoInsertEventMessageInfo.Create(const aDisplayID: string;
    const aGroupNames: TStringArray; const aKey, aText: string; aInfoGroupBehaviour: integer);
begin
    inherited Create();
    fDisplayID := aDisplayID;
    fGroupNames := aGroupNames;
    fKey := aKey;
    fText := aText;
    fInfoGroupBehaviour := aInfoGroupBehaviour;

end;

function TCoreRunInfoInsertEventMessageInfo.GetMessageID: integer;
begin
    result := cMessageIDCoreRunInfoInsertEvent;
end;


end.
