{ -----------------------------------------------------------------------------------------------------------------------
  Copyright © 2009 Zinsser Analytic GmbH - All rights reserved.
  Author       : Payman Kamali (pk)
  Description  :
  -----------------------------------------------------------------------------------------------------------------------
  Revision History:
  date     op  method                                track-no  improvement/change
  -------- --  ------------------------------------  --------- ----------------------------------------------------------
  12.10.09 pk                                        TN4812     Initial Revision
  04.02.10 pk                                        TN4972   Changes for Restart
  ----------------------------------------------------------------------------------------------------------------------- }

unit RemoteMessageTypes;


interface


uses
    RemoteFunctionClasses,
    GeneralTypes;

type
    TRemoteCallRunInfoInsertCoreStatusEvent = class(TRemoteCall)
    private
        fDisplayID: string;
        fGroupNames: TStringArray;
        fKey: string;
        fText: string;
        fInfoGroupBehaviour: integer;
    public
        constructor Create(const aDisplayID: string; const aGroupNames: TStringArray;
            const aKey, aText: string; aInfoGroupBehaviour: integer); reintroduce; overload;
        constructor Create(); overload; override;
    published
        property DisplayID: string read fDisplayID write fDisplayID;
        property GroupNames: TStringArray read fGroupNames write fGroupNames;
        property Key: string read fKey write fKey;
        property Text: string read fText write fText;
        property InfoGroupBehaviour: integer read fInfoGroupBehaviour write fInfoGroupBehaviour;
    end;

    TRemoteResultControllerGetErrorBoxAdvanced = class(TRemoteResult)
    private
        fErrorID: integer;
        fErrorInfo: TObject;
        fResultSuccess: boolean;
    public
        constructor Create(const aErrorID: integer; const aErrorInfo: TObject; const aResultSuccess: boolean);
            reintroduce; overload;
        constructor Create(); overload; override;
    published
        property ErrorID: integer read fErrorID write fErrorID;
        property ErrorInfo: TObject read fErrorInfo write fErrorInfo;
        property ResultSuccess: boolean read fResultSuccess write fResultSuccess;
    end;

    TRemoteCallControllerGetErrorBoxAdvanced = class(TRemoteCall);


implementation


uses
    Classes;
{ TRemoteCallRunInfoInsertCoreStatusEvent }

constructor TRemoteCallRunInfoInsertCoreStatusEvent.Create(const aDisplayID: string;
    const aGroupNames: TStringArray; const aKey, aText: string; aInfoGroupBehaviour: integer);
begin
    inherited Create();
    fDisplayID := aDisplayID;
    fGroupNames := aGroupNames;
    fKey := aKey;
    fText := aText;
    fInfoGroupBehaviour := aInfoGroupBehaviour;
end;

constructor TRemoteCallRunInfoInsertCoreStatusEvent.Create();
begin
    Create('', nil, '', '', 0);
end;
{ TRemoteResultControllerGetErrorBoxAdvanced }

constructor TRemoteResultControllerGetErrorBoxAdvanced.Create(const aErrorID: integer;
    const aErrorInfo: TObject; const aResultSuccess: boolean);
begin
    inherited Create();
    fErrorID := aErrorID;
    fErrorInfo := aErrorInfo;
    fResultSuccess := aResultSuccess;
end;

constructor TRemoteResultControllerGetErrorBoxAdvanced.Create();
begin
    Create(0, nil, false);
end;

// initialization
// RegisterClasses( [ TRemoteCallRunInfoInsertCoreStatusEvent ] );
// RegisterClasses( [ TRemoteCallControllerGetErrorBoxAdvanced, TRemoteResultControllerGetErrorBoxAdvanced ] );


end.
