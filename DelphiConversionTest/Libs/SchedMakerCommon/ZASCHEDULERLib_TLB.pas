unit ZASCHEDULERLib_TLB;

// ************************************************************************ //
// WARNING
// -------
// The types declared in this file were generated from data read from a
// Type Library. If this type library is explicitly or indirectly (via
// another type library referring to this type library) re-imported, or the
// 'Refresh' command of the Type Library Editor activated while editing the
// Type Library, the contents of this file will be regenerated and all
// manual modifications will be lost.
// ************************************************************************ //

// PASTLWTR : 1.2
// File generated on 1/4/2007 2:41:43 PM from Type Library described below.

// ************************************************************************  //
// Type Lib: F:\CPlusPlus\Scheduler\ZAScheduler_d.ocx (1)
// LIBID: {EC7E9AEA-4C16-4F1B-BE86-2DB59539731E}
// LCID: 0
// Helpfile: F:\delphi32\test\ZAScheduler.hlp
// HelpString: ZAScheduler ActiveX Control module
// DepndLst:
// (1) v2.0 stdole, (D:\WINNT\system32\STDOLE2.TLB)
// ************************************************************************ //
// *************************************************************************//
// NOTE:
// Items guarded by $IFDEF_LIVE_SERVER_AT_DESIGN_TIME are used by properties
// which return objects that may need to be explicitly created via a function
// call prior to any access via the property. These items have been disabled
// in order to prevent accidental use from within the object inspector. You
// may enable them by defining LIVE_SERVER_AT_DESIGN_TIME or by selectively
// removing them from the $IFDEF blocks. However, such items must still be
// programmatically created via a method of the appropriate CoClass before
// they can be used.
{$TYPEDADDRESS OFF} // Unit must be compiled without type-checked pointers.
{$WARN SYMBOL_PLATFORM OFF}
{$WRITEABLECONST ON}
{$VARPROPSETTER ON}


interface


uses
    Windows,
    ActiveX,
    Classes,
    Graphics,
    OleCtrls,
    OleServer,
    StdVCL,
    Variants;

// *********************************************************************//
// GUIDS declared in the TypeLibrary. Following prefixes are used:
// Type Libraries     : LIBID_xxxx
// CoClasses          : CLASS_xxxx
// DISPInterfaces     : DIID_xxxx
// Non-DISP interfaces: IID_xxxx
// *********************************************************************//
const
    // TypeLibrary Major and minor versions
    ZASCHEDULERLibMajorVersion = 1;
    ZASCHEDULERLibMinorVersion = 0;

    LIBID_ZASCHEDULERLib: TGUID = '{EC7E9AEA-4C16-4F1B-BE86-2DB59539731E}';

    DIID__DZAScheduler: TGUID = '{4DE2F455-519E-4338-BAEE-37BC8BF1E94A}';
    DIID__DZASchedulerEvents: TGUID = '{323D34C4-8C61-4233-A16F-B1C27C85662B}';
    CLASS_ZAScheduler: TGUID = '{C9D7E3A9-56A7-49CB-9306-CD8C8C923AC2}';

type

    // *********************************************************************//
    // Forward declaration of types defined in TypeLibrary
    // *********************************************************************//
    _DZAScheduler = dispinterface;
    _DZASchedulerEvents = dispinterface;

    // *********************************************************************//
    // Declaration of CoClasses defined in Type Library
    // (NOTE: Here we map each CoClass to its Default Interface)
    // *********************************************************************//
    ZAScheduler = _DZAScheduler;

    // *********************************************************************//
    // Declaration of structures, unions and aliases.
    // *********************************************************************//
    PInteger1 = ^Integer; { * }
    PSmallint1 = ^Smallint; { * }
    PWideString1 = ^WideString; { * }

    // *********************************************************************//
    // DispIntf:  _DZAScheduler
    // Flags:     (4112) Hidden Dispatchable
    // GUID:      {4DE2F455-519E-4338-BAEE-37BC8BF1E94A}
    // *********************************************************************//
    _DZAScheduler = dispinterface
        ['{4DE2F455-519E-4338-BAEE-37BC8BF1E94A}']
        property TimeUnit: Smallint dispid 1;
        property TextOutput: WordBool dispid 2;
        property GraphicOutput: WordBool dispid 3;
        property LogOutputFile: WideString dispid 4;
        property CheckLevel: Smallint dispid 5;
        property Flag1: Integer dispid 6;
        property Flag2: Integer dispid 7;
        property Flag3: Integer dispid 8;
        property Flag4: Integer dispid 9;
        property Flag5: Integer dispid 10;
        property Flag6: Integer dispid 11;
        property Flag7: Integer dispid 12;
        property Flag8: Integer dispid 13;
        property Flag9: Integer dispid 14;
        property Flag10: Integer dispid 15;
        function OpenSession(const name: WideString): Integer; dispid 16;
        function CloseSession: Integer; dispid 17;
        function GetSessionDuration(var Duration: Integer): Integer; dispid 18;
        function AddResource(ResourceId: Integer; const name: WideString; ShareLevel: Smallint): Integer;
            dispid 19;
        function GetResourceCount(var Count: Smallint): Integer; dispid 20;
        function GetResourceID(ResourceIndex: Smallint; var ResourceId: Integer): Integer; dispid 21;
        function GetResourceData(ResourceId: Integer; var name: WideString; var ShareLevel: Smallint;
            var Status: Integer): Integer; dispid 22;
        function GetResourceReserveCount(ResourceId: Integer; ProcessId: Integer; var Count: Smallint)
            : Integer; dispid 23;
        function GetResourceReserve(ResourceId: Integer; ProcessId: Integer; index: Smallint;
            var ActionID: Integer; var StartTime: Integer; var Duration: Integer; var ShareLevel: Smallint)
            : Integer; dispid 24;
        function PreReserveResource(ResourceId: Integer; ProcessId: Integer; ShareLevel: Smallint): Integer;
            dispid 25;
        function AddProcess(ProcessId: Integer; const name: WideString; Priority: Smallint; Color: Smallint)
            : Integer; dispid 26;
        function DeleteProcess(ProcessId: Integer): Integer; dispid 27;
        function GetProcessCount(var Count: Smallint): Integer; dispid 28;
        function GetProcessID(ProcessIndex: Smallint; var ProcessId: Integer): Integer; dispid 29;
        function GetProcessData(ProcessId: Integer; var name: WideString; var Priority: Smallint;
            var Color: Smallint): Integer; dispid 30;
        function GetProcessTimes(ProcessId: Integer; var StartTime: Integer; var Duration: Integer;
            var Status: Integer): Integer; dispid 31;
        function AddAction(ActionID: Integer; const name: WideString; ProcessId: Integer; SharedId: Integer;
            MinLength: Integer; MaxLength: Integer; Color: Smallint; Style: Smallint): Integer; dispid 32;
        function AddResourceUse(ActionID: Integer; ResId: Integer; UseLevel: Smallint; ReleaseLevel: Smallint)
            : Integer; dispid 33;
        function GetActionCount(var Count: Smallint): Integer; dispid 34;
        function GetActionID(ActionIndex: Smallint; var ActionID: Integer): Integer; dispid 35;
        function GetActionScheduleTimes(ActionID: Integer; var StartTime: Integer; var Duration: Integer;
            var Status: Integer): Integer; dispid 36;
        function GetActionData(ActionID: Integer; var name: WideString; var ProcessId: Integer;
            var SharedId: Integer; var MinLength: Integer; var MaxLength: Integer; var Color: Smallint;
            var Style: Smallint): Integer; dispid 37;
        function GetActionResourceUseCount(ActionID: Integer; var Count: Smallint): Integer; dispid 38;
        function GetActionResourceUse(ActionID: Integer; index: Smallint; var ResId: Integer;
            var UseLevel: Smallint; var ReleaseLevel: Smallint): Integer; dispid 39;
        function GetProcessActionCount(ProcessId: Integer; var Count: Smallint): Integer; dispid 40;
        function GetProcessActionID(ProcessId: Integer; ActionIndex: Smallint; var ActionID: Integer)
            : Integer; dispid 41;
        function Schedule(StartTime: Integer): Integer; dispid 42;
        function GetErrorText(ErrorNumber: Integer; var ErrorText: WideString): Integer; dispid 43;
        function GetErrorDescription(var ErrorText: WideString): Integer; dispid 44;
        function Display: Integer; dispid 45;
        function AdjustResourceReserve(ActionID: Integer; ResourceId: Integer; UseDifference: Smallint)
            : Integer; dispid 46;
        function ReleaseResource(ResourceId: Integer; ShareLevel: Smallint; ScheduleTime: Integer): Integer;
            dispid 47;
        function GetActionRealTimes(ActionID: Integer; var RealStartTime: Integer; var RealDuration: Integer)
            : Integer; dispid 48;
        function GetErrorCount(var Count: Smallint): Integer; dispid 49;
        function GetError(index: Smallint; var ErrorCode: Integer; var Time: Integer; var ProcessId: Integer;
            var ActionID: Integer; var ResourceId: Integer; var ErrorText: WideString;
            var ErrorDescription: WideString): Integer; dispid 50;
        function ShiftTime: Integer; dispid 51;
        function Stop: Integer; dispid 52;
        function DisplayErrors: Integer; dispid 53;
        function ClearErrors: Integer; dispid 54;
        function SetActionScheduleTimes(ActionID: Integer; StartTime: Integer; Duration: Integer;
            Status: Integer): Integer; dispid 55;
        function AdjustActionData(ActionID: Integer; AdjustType: Integer; Value: Integer;
            var MinTime: Integer; var ShiftTime: Integer): Integer; dispid 56;
        function Shift(MinTime: Integer; ShiftTime: Integer; ExceptAId: Integer): Integer; dispid 57;
        function ChangeActionTimes(ActionID: Integer; MinLength: Integer; MaxLength: Integer): Integer;
            dispid 58;
        procedure AboutBox; dispid - 552;
    end;

    // *********************************************************************//
    // DispIntf:  _DZASchedulerEvents
    // Flags:     (4096) Dispatchable
    // GUID:      {323D34C4-8C61-4233-A16F-B1C27C85662B}
    // *********************************************************************//
    _DZASchedulerEvents = dispinterface
        ['{323D34C4-8C61-4233-A16F-B1C27C85662B}']
    end;

    // *********************************************************************//
    // OLE Control Proxy class declaration
    // Control Name     : TZAScheduler
    // Help String      : ZAScheduler Control
    // Default Interface: _DZAScheduler
    // Def. Intf. DISP? : Yes
    // Event   Interface: _DZASchedulerEvents
    // TypeFlags        : (34) CanCreate Control
    // *********************************************************************//
    TZAScheduler = class(TOleControl)
    private
        FIntf: _DZAScheduler;
        function GetControlInterface: _DZAScheduler;
    protected
        procedure CreateControl;
        procedure InitControlData; override;
    public
        function OpenSession(const name: WideString): Integer;
        function CloseSession: Integer;
        function GetSessionDuration(var Duration: Integer): Integer;
        function AddResource(ResourceId: Integer; const name: WideString; ShareLevel: Smallint): Integer;
        function GetResourceCount(var Count: Smallint): Integer;
        function GetResourceID(ResourceIndex: Smallint; var ResourceId: Integer): Integer;
        function GetResourceData(ResourceId: Integer; var name: WideString; var ShareLevel: Smallint;
            var Status: Integer): Integer;
        function GetResourceReserveCount(ResourceId: Integer; ProcessId: Integer;
            var Count: Smallint): Integer;
        function GetResourceReserve(ResourceId: Integer; ProcessId: Integer; index: Smallint;
            var ActionID: Integer; var StartTime: Integer; var Duration: Integer;
            var ShareLevel: Smallint): Integer;
        function PreReserveResource(ResourceId: Integer; ProcessId: Integer; ShareLevel: Smallint): Integer;
        function AddProcess(ProcessId: Integer; const name: WideString; Priority: Smallint;
            Color: Smallint): Integer;
        function DeleteProcess(ProcessId: Integer): Integer;
        function GetProcessCount(var Count: Smallint): Integer;
        function GetProcessID(ProcessIndex: Smallint; var ProcessId: Integer): Integer;
        function GetProcessData(ProcessId: Integer; var name: WideString; var Priority: Smallint;
            var Color: Smallint): Integer;
        function GetProcessTimes(ProcessId: Integer; var StartTime: Integer; var Duration: Integer;
            var Status: Integer): Integer;
        function AddAction(ActionID: Integer; const name: WideString; ProcessId: Integer; SharedId: Integer;
            MinLength: Integer; MaxLength: Integer; Color: Smallint; Style: Smallint): Integer;
        function AddResourceUse(ActionID: Integer; ResId: Integer; UseLevel: Smallint;
            ReleaseLevel: Smallint): Integer;
        function GetActionCount(var Count: Smallint): Integer;
        function GetActionID(ActionIndex: Smallint; var ActionID: Integer): Integer;
        function GetActionScheduleTimes(ActionID: Integer; var StartTime: Integer; var Duration: Integer;
            var Status: Integer): Integer;
        function GetActionData(ActionID: Integer; var name: WideString; var ProcessId: Integer;
            var SharedId: Integer; var MinLength: Integer; var MaxLength: Integer; var Color: Smallint;
            var Style: Smallint): Integer;
        function GetActionResourceUseCount(ActionID: Integer; var Count: Smallint): Integer;
        function GetActionResourceUse(ActionID: Integer; index: Smallint; var ResId: Integer;
            var UseLevel: Smallint; var ReleaseLevel: Smallint): Integer;
        function GetProcessActionCount(ProcessId: Integer; var Count: Smallint): Integer;
        function GetProcessActionID(ProcessId: Integer; ActionIndex: Smallint; var ActionID: Integer)
            : Integer;
        function Schedule(StartTime: Integer): Integer;
        function GetErrorText(ErrorNumber: Integer; var ErrorText: WideString): Integer;
        function GetErrorDescription(var ErrorText: WideString): Integer;
        function Display: Integer;
        function AdjustResourceReserve(ActionID: Integer; ResourceId: Integer;
            UseDifference: Smallint): Integer;
        function ReleaseResource(ResourceId: Integer; ShareLevel: Smallint; ScheduleTime: Integer): Integer;
        function GetActionRealTimes(ActionID: Integer; var RealStartTime: Integer;
            var RealDuration: Integer): Integer;
        function GetErrorCount(var Count: Smallint): Integer;
        function GetError(index: Smallint; var ErrorCode: Integer; var Time: Integer; var ProcessId: Integer;
            var ActionID: Integer; var ResourceId: Integer; var ErrorText: WideString;
            var ErrorDescription: WideString): Integer;
        function ShiftTime: Integer;
        function Stop: Integer;
        function DisplayErrors: Integer;
        function ClearErrors: Integer;
        function SetActionScheduleTimes(ActionID: Integer; StartTime: Integer; Duration: Integer;
            Status: Integer): Integer;
        function AdjustActionData(ActionID: Integer; AdjustType: Integer; Value: Integer;
            var MinTime: Integer; var ShiftTime: Integer): Integer;
        function Shift(MinTime: Integer; ShiftTime: Integer; ExceptAId: Integer): Integer;
        function ChangeActionTimes(ActionID: Integer; MinLength: Integer; MaxLength: Integer): Integer;
        procedure AboutBox;
        property ControlInterface: _DZAScheduler read GetControlInterface;
        property DefaultInterface: _DZAScheduler read GetControlInterface;
    published
        property Anchors;
        property TabStop;
        property Align;
        property DragCursor;
        property DragMode;
        property ParentShowHint;
        property PopupMenu;
        property ShowHint;
        property TabOrder;
        property Visible;
        property OnDragDrop;
        property OnDragOver;
        property OnEndDrag;
        property OnEnter;
        property OnExit;
        property OnStartDrag;
        property TimeUnit: Smallint index 1 read GetSmallintProp write SetSmallintProp stored False;
        property TextOutput: WordBool index 2 read GetWordBoolProp write SetWordBoolProp stored False;
        property GraphicOutput: WordBool index 3 read GetWordBoolProp write SetWordBoolProp stored False;
        property LogOutputFile: WideString index 4 read GetWideStringProp write SetWideStringProp
            stored False;
        property CheckLevel: Smallint index 5 read GetSmallintProp write SetSmallintProp stored False;
        property Flag1: Integer index 6 read GetIntegerProp write SetIntegerProp stored False;
        property Flag2: Integer index 7 read GetIntegerProp write SetIntegerProp stored False;
        property Flag3: Integer index 8 read GetIntegerProp write SetIntegerProp stored False;
        property Flag4: Integer index 9 read GetIntegerProp write SetIntegerProp stored False;
        property Flag5: Integer index 10 read GetIntegerProp write SetIntegerProp stored False;
        property Flag6: Integer index 11 read GetIntegerProp write SetIntegerProp stored False;
        property Flag7: Integer index 12 read GetIntegerProp write SetIntegerProp stored False;
        property Flag8: Integer index 13 read GetIntegerProp write SetIntegerProp stored False;
        property Flag9: Integer index 14 read GetIntegerProp write SetIntegerProp stored False;
        property Flag10: Integer index 15 read GetIntegerProp write SetIntegerProp stored False;
    end;

procedure register;

resourcestring
    dtlServerPage = 'ActiveX';

    dtlOcxPage = 'ActiveX';


implementation


uses
    ComObj;

procedure TZAScheduler.InitControlData;
const
    CControlData: TControlData2 = (ClassID: '{C9D7E3A9-56A7-49CB-9306-CD8C8C923AC2}'; EventIID: '';
        EventCount: 0; EventDispIDs: nil; LicenseKey: nil (* HR:$80004005 *); Flags: $00000000; Version: 401);
begin
    ControlData := @CControlData;
end;

procedure TZAScheduler.CreateControl;

    procedure DoCreate;
    begin
        FIntf := IUnknown(OleObject) as _DZAScheduler;
    end;

begin
    if FIntf = nil then
        DoCreate;
end;

function TZAScheduler.GetControlInterface: _DZAScheduler;
begin
    CreateControl;
    Result := FIntf;
end;

function TZAScheduler.OpenSession(const name: WideString): Integer;
begin
    Result := DefaultInterface.OpenSession(name);
end;

function TZAScheduler.CloseSession: Integer;
begin
    Result := DefaultInterface.CloseSession;
end;

function TZAScheduler.GetSessionDuration(var Duration: Integer): Integer;
begin
    Result := DefaultInterface.GetSessionDuration(Duration);
end;

function TZAScheduler.AddResource(ResourceId: Integer; const name: WideString; ShareLevel: Smallint): Integer;
begin
    Result := DefaultInterface.AddResource(ResourceId, name, ShareLevel);
end;

function TZAScheduler.GetResourceCount(var Count: Smallint): Integer;
begin
    Result := DefaultInterface.GetResourceCount(Count);
end;

function TZAScheduler.GetResourceID(ResourceIndex: Smallint; var ResourceId: Integer): Integer;
begin
    Result := DefaultInterface.GetResourceID(ResourceIndex, ResourceId);
end;

function TZAScheduler.GetResourceData(ResourceId: Integer; var name: WideString; var ShareLevel: Smallint;
    var Status: Integer): Integer;
begin
    Result := DefaultInterface.GetResourceData(ResourceId, name, ShareLevel, Status);
end;

function TZAScheduler.GetResourceReserveCount(ResourceId: Integer; ProcessId: Integer;
    var Count: Smallint): Integer;
begin
    Result := DefaultInterface.GetResourceReserveCount(ResourceId, ProcessId, Count);
end;

function TZAScheduler.GetResourceReserve(ResourceId: Integer; ProcessId: Integer; index: Smallint;
    var ActionID: Integer; var StartTime: Integer; var Duration: Integer; var ShareLevel: Smallint): Integer;
begin
    Result := DefaultInterface.GetResourceReserve(ResourceId, ProcessId, index, ActionID, StartTime, Duration,
        ShareLevel);
end;

function TZAScheduler.PreReserveResource(ResourceId: Integer; ProcessId: Integer;
    ShareLevel: Smallint): Integer;
begin
    Result := DefaultInterface.PreReserveResource(ResourceId, ProcessId, ShareLevel);
end;

function TZAScheduler.AddProcess(ProcessId: Integer; const name: WideString; Priority: Smallint;
    Color: Smallint): Integer;
begin
    Result := DefaultInterface.AddProcess(ProcessId, name, Priority, Color);
end;

function TZAScheduler.DeleteProcess(ProcessId: Integer): Integer;
begin
    Result := DefaultInterface.DeleteProcess(ProcessId);
end;

function TZAScheduler.GetProcessCount(var Count: Smallint): Integer;
begin
    Result := DefaultInterface.GetProcessCount(Count);
end;

function TZAScheduler.GetProcessID(ProcessIndex: Smallint; var ProcessId: Integer): Integer;
begin
    Result := DefaultInterface.GetProcessID(ProcessIndex, ProcessId);
end;

function TZAScheduler.GetProcessData(ProcessId: Integer; var name: WideString; var Priority: Smallint;
    var Color: Smallint): Integer;
begin
    Result := DefaultInterface.GetProcessData(ProcessId, name, Priority, Color);
end;

function TZAScheduler.GetProcessTimes(ProcessId: Integer; var StartTime: Integer; var Duration: Integer;
    var Status: Integer): Integer;
begin
    Result := DefaultInterface.GetProcessTimes(ProcessId, StartTime, Duration, Status);
end;

function TZAScheduler.AddAction(ActionID: Integer; const name: WideString; ProcessId: Integer;
    SharedId: Integer; MinLength: Integer; MaxLength: Integer; Color: Smallint; Style: Smallint): Integer;
begin
    Result := DefaultInterface.AddAction(ActionID, name, ProcessId, SharedId, MinLength, MaxLength,
        Color, Style);
end;

function TZAScheduler.AddResourceUse(ActionID: Integer; ResId: Integer; UseLevel: Smallint;
    ReleaseLevel: Smallint): Integer;
begin
    Result := DefaultInterface.AddResourceUse(ActionID, ResId, UseLevel, ReleaseLevel);
end;

function TZAScheduler.GetActionCount(var Count: Smallint): Integer;
begin
    Result := DefaultInterface.GetActionCount(Count);
end;

function TZAScheduler.GetActionID(ActionIndex: Smallint; var ActionID: Integer): Integer;
begin
    Result := DefaultInterface.GetActionID(ActionIndex, ActionID);
end;

function TZAScheduler.GetActionScheduleTimes(ActionID: Integer; var StartTime: Integer; var Duration: Integer;
    var Status: Integer): Integer;
begin
    Result := DefaultInterface.GetActionScheduleTimes(ActionID, StartTime, Duration, Status);
end;

function TZAScheduler.GetActionData(ActionID: Integer; var name: WideString; var ProcessId: Integer;
    var SharedId: Integer; var MinLength: Integer; var MaxLength: Integer; var Color: Smallint;
    var Style: Smallint): Integer;
begin
    Result := DefaultInterface.GetActionData(ActionID, name, ProcessId, SharedId, MinLength, MaxLength,
        Color, Style);
end;

function TZAScheduler.GetActionResourceUseCount(ActionID: Integer; var Count: Smallint): Integer;
begin
    Result := DefaultInterface.GetActionResourceUseCount(ActionID, Count);
end;

function TZAScheduler.GetActionResourceUse(ActionID: Integer; index: Smallint; var ResId: Integer;
    var UseLevel: Smallint; var ReleaseLevel: Smallint): Integer;
begin
    Result := DefaultInterface.GetActionResourceUse(ActionID, index, ResId, UseLevel, ReleaseLevel);
end;

function TZAScheduler.GetProcessActionCount(ProcessId: Integer; var Count: Smallint): Integer;
begin
    Result := DefaultInterface.GetProcessActionCount(ProcessId, Count);
end;

function TZAScheduler.GetProcessActionID(ProcessId: Integer; ActionIndex: Smallint;
    var ActionID: Integer): Integer;
begin
    Result := DefaultInterface.GetProcessActionID(ProcessId, ActionIndex, ActionID);
end;

function TZAScheduler.Schedule(StartTime: Integer): Integer;
begin
    Result := DefaultInterface.Schedule(StartTime);
end;

function TZAScheduler.GetErrorText(ErrorNumber: Integer; var ErrorText: WideString): Integer;
begin
    Result := DefaultInterface.GetErrorText(ErrorNumber, ErrorText);
end;

function TZAScheduler.GetErrorDescription(var ErrorText: WideString): Integer;
begin
    Result := DefaultInterface.GetErrorDescription(ErrorText);
end;

function TZAScheduler.Display: Integer;
begin
    Result := DefaultInterface.Display;
end;

function TZAScheduler.AdjustResourceReserve(ActionID: Integer; ResourceId: Integer;
    UseDifference: Smallint): Integer;
begin
    Result := DefaultInterface.AdjustResourceReserve(ActionID, ResourceId, UseDifference);
end;

function TZAScheduler.ReleaseResource(ResourceId: Integer; ShareLevel: Smallint;
    ScheduleTime: Integer): Integer;
begin
    Result := DefaultInterface.ReleaseResource(ResourceId, ShareLevel, ScheduleTime);
end;

function TZAScheduler.GetActionRealTimes(ActionID: Integer; var RealStartTime: Integer;
    var RealDuration: Integer): Integer;
begin
    Result := DefaultInterface.GetActionRealTimes(ActionID, RealStartTime, RealDuration);
end;

function TZAScheduler.GetErrorCount(var Count: Smallint): Integer;
begin
    Result := DefaultInterface.GetErrorCount(Count);
end;

function TZAScheduler.GetError(index: Smallint; var ErrorCode: Integer; var Time: Integer;
    var ProcessId: Integer; var ActionID: Integer; var ResourceId: Integer; var ErrorText: WideString;
    var ErrorDescription: WideString): Integer;
begin
    Result := DefaultInterface.GetError(index, ErrorCode, Time, ProcessId, ActionID, ResourceId, ErrorText,
        ErrorDescription);
end;

function TZAScheduler.ShiftTime: Integer;
begin
    Result := DefaultInterface.ShiftTime;
end;

function TZAScheduler.Stop: Integer;
begin
    Result := DefaultInterface.Stop;
end;

function TZAScheduler.DisplayErrors: Integer;
begin
    Result := DefaultInterface.DisplayErrors;
end;

function TZAScheduler.ClearErrors: Integer;
begin
    Result := DefaultInterface.ClearErrors;
end;

function TZAScheduler.SetActionScheduleTimes(ActionID: Integer; StartTime: Integer; Duration: Integer;
    Status: Integer): Integer;
begin
    Result := DefaultInterface.SetActionScheduleTimes(ActionID, StartTime, Duration, Status);
end;

function TZAScheduler.AdjustActionData(ActionID: Integer; AdjustType: Integer; Value: Integer;
    var MinTime: Integer; var ShiftTime: Integer): Integer;
begin
    Result := DefaultInterface.AdjustActionData(ActionID, AdjustType, Value, MinTime, ShiftTime);
end;

function TZAScheduler.Shift(MinTime: Integer; ShiftTime: Integer; ExceptAId: Integer): Integer;
begin
    Result := DefaultInterface.Shift(MinTime, ShiftTime, ExceptAId);
end;

function TZAScheduler.ChangeActionTimes(ActionID: Integer; MinLength: Integer; MaxLength: Integer): Integer;
begin
    Result := DefaultInterface.ChangeActionTimes(ActionID, MinLength, MaxLength);
end;

procedure TZAScheduler.AboutBox;
begin
    DefaultInterface.AboutBox;
end;

procedure register;
begin
    RegisterComponents(dtlOcxPage, [TZAScheduler]);
end;


end.
