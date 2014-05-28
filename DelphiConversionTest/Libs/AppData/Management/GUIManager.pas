{ --------------------------------------------------------------------------------------------------
  Copyright © 2005 Zinsser Analytic GmbH - All rights reserved.
  Author       : Payman Kamali (pk)
  Description  : Handle calls to Delphi Graphics ( VCL ) functions
  --------------------------------------------------------------------------------------------------
  Revision History:
  date     op  method                            track-no improvement/change
  -------- --  -------------------------------   -------- ------------------------------------------
  11.03.05 pk                                     TN2339.2  Initial Revision
  15.03.05 pk                                     TN2349   MessageBox : do not check error pointer
  06.04.05 pk SetCursor                           TN2374   returns the previous cursor
  15.08.05 wl  TVCLHandling.VCL                   TN2558.1 Text und Caption von InputQuery waren vertauscht: geändert
  15.08.05 wl                                     TN2558.1 intern: uses SamGlobe.GlobalErrBeep --> GUIManagerSetup
  31.10.06 pk StartSysEv, EndSysEv                TN3391   New
  01.11.06 pk AcquireSerialAccess                 TN3391   New. criticalsection.acquire moved to GUIManagerRun
  21.11.06 pk TInteractMessageSysEvType           TN3424   New type imtNone
  21.11.06 pk MessageBox                          TN3424   TInteractMessageSysEvType paramter instead of boolean
  01.12.06 pk                                     TN3441   TInteractMessageSysEvType, StartSysEv, EndSysEv removed. Now handled in InterruptManager
  13.04.07 pk ShowModal                           TN3667   New
  01.08.07 wl                                     TN3811.2 benutzt gLogManager (jetzt in AppCommon)
  06.09.07 wl  TVCLHandling.VCL                   TN3850   benutzt gmInputQuery
  14.04.08 wl                                     TN4060   uses DialogUtils
  24.06.08 wl                                     TN4143   uses AppTypes entfernt
  03.07.08 wl                                     TN4157
  17.12.08 pk MessageBox                          TN4373   Log text changed
  08.06.09 pk InteractiveMessageAndWait           TN4585.1 now public
  05.08.09 wl  MessageBox                         TN4702   uses DialogUtils
  10.08.09 wl  SetStatusBar                       TN4702   von messageHandling hierher
  08.09.09 pk                                     TN4753   No longer inherits from InterfacedNoRef
  20.05.10 wl                                     TN5117   uses ControlUtils
  28.05.10 wl                                     TN5116   fStatusBar als property
  17.08.10 wl  MessageBox                         TN5112   DefaultWerte auch für Caption, Buttons, usw..
  02.02.11 wl  SelectItemBox                      TN5466   neu
  26.11.11 wl  BitmapMessageBox                   TN5750   neu: MessageBox mit Bitmap
  30.07.13 wl  StatusBar                          TN6160   entfernt
  30.07.13 wl  InterruptSignalStart,-End          TN6160   entspricht StartBeep, EndBeep
  30.07.13 wl  InterruptSignalMute                TN6160   neu für "Beeper off"-Button
  -------------------------------------------------------------------------------------------------- }

unit GUIManager;


interface


uses
    Forms,
    Controls,
    ThreadUtils,
    ControlUtils,
    GeneralTypes;

type
    TVCLHandling = class
    private
        class function VCL(const aArgs: TMessageArg): TMessageResult;
        class function VCLShowMessageBox(const aText, aCaption: string;
            aButtons, aIcon, aDefaultButton: integer): integer;
        class function VCLShowBitmapMessageBox(const aBitmapName, aText, aCaption: string;
            aButtons: integer): integer;
    end;

    TGUIManager = class
    protected
        procedure InterruptSignalStart(); virtual;
        procedure InterruptSignalEnd(); virtual;
        procedure AcquireSerialAccess; virtual;
        procedure ReleaseSerialAccess; virtual;
    public
        procedure InterruptSignalMute(); virtual;

        function InteractiveMessageAndWait(aFunc: TFunc; const aArgs: TMessageArg): TMessageResult;
        function NonInteractiveMessageAndWait(aFunc: TFunc; const aArgs: TMessageArg): TMessageResult;

        function InputQuery(const aText, aCaption: string; var vValue: string): boolean;
        function InputBox(const aText, aCaption, aDefault: string): string;
        function SelectItemBox(const aItems: TStringArray; const aText, aCaption: string;
            const aButtonText: string = ''): string; virtual;
        function MessageBox(const aText: string; const aCaption: string = ''; aButtons: integer = 0;
            aIcon: integer = 0; aDefaultButton: Integer = 0): Integer; virtual;
        function BitmapMessageBox(const aBitmapName, aText, aCaption: string; aButtons: integer)
            : Integer; virtual;
        function SetCursor(aCursor: TCursor): integer;
        procedure SetControlProp(aControl: TControl; aProperty: TEnControlProp; aValue: variant);
        function ShowModal(aForm: TForm): TModalResult;
    end;

var
    gGUIManager: TGUIManager;


implementation


uses
    SysUtils,
    Variants,
    LogManager,
    DialogUtils;

type
    TVCL = (vclShowMessage, vclShowBitmapMessage, vclSetCursor, vclSetControlProp, vclInputQuery,
        vclShowModal, vclSelectItemBox);

    { TVCLHandling }

class function TVCLHandling.VCLShowMessageBox(const aText, aCaption: string;
    aButtons, aIcon, aDefaultButton: integer): integer;
var
    xCaption: string;
begin

    if aCaption = '' then
        xCaption := Application.Title
    else
        xCaption := aCaption;

    result := TDialogUtils.MessageBox(aText, xCaption, aButtons, aIcon, aDefaultButton);
end;

class function TVCLHandling.VCLShowBitmapMessageBox(const aBitmapName, aText, aCaption: string;
    aButtons: integer): integer;
var
    xCaption: string;
begin

    if aCaption = '' then
        xCaption := Application.Title
    else
        xCaption := aCaption;

    result := TDialogUtils.BitmapMessageBox(aBitmapName, aText, xCaption, aButtons);
end;

class function TVCLHandling.VCL(const aArgs: TMessageArg): TMessageResult;
const
    INT_VCL_INDEX_TYPE = 0;
    INT_VCL_INDEX_INPUTMSG_TEXT = 1;
    INT_VCL_INDEX_INPUTMSG_CAPTION = 2;
    INT_VCL_INDEX_INPUTMSG_DEFAULT = 3;
var
    xType: TVCL;
    xBool1: boolean;
    xStr1: string;
begin
    xType := aArgs[INT_VCL_INDEX_TYPE];

    case xType of
        vclShowMessage:
            result := VCLShowMessageBox(string(aArgs[1]), string(aArgs[2]), integer(aArgs[3]),
                integer(aArgs[4]), integer(aArgs[5]));
        vclShowBitmapMessage:
            result := VCLShowBitmapMessageBox(string(aArgs[1]), string(aArgs[2]), string(aArgs[3]),
                integer(aArgs[4]));

        vclSelectItemBox:
            begin
                xStr1 := TDialogUtils.SelectItemBox(TStringArray(LongInt(aArgs[1])), aArgs[2], aArgs[3],
                    aArgs[4]);
                result := VarArrayOf([xStr1]);
            end;

        vclInputQuery:
            begin
                xStr1 := aArgs[INT_VCL_INDEX_INPUTMSG_DEFAULT];
                xBool1 := TDialogUtils.InputQuery(aArgs[INT_VCL_INDEX_INPUTMSG_CAPTION],
                    aArgs[INT_VCL_INDEX_INPUTMSG_TEXT], xStr1);
                result := VarArrayOf([xBool1, xStr1]);
            end;

        vclSetCursor:
            begin
                result := Screen.Cursor;
                Screen.Cursor := aArgs[1];
            end;
        vclSetControlProp:
            TControlUtils.SetControlProperty(TControl(LongInt(aArgs[1])), aArgs[2], aArgs[3]);
        vclShowModal:
            result := TForm(LongInt(aArgs[1])).ShowModal;
    end;
end;

{ TGUIManager }

procedure TGUIManager.AcquireSerialAccess();
begin
end;

procedure TGUIManager.ReleaseSerialAccess();
begin
end;

function TGUIManager.InteractiveMessageAndWait(aFunc: TFunc; const aArgs: TMessageArg): TMessageResult;
// no two threads can make a send call at the same time
begin
    AcquireSerialAccess();
    try
        self.InterruptSignalStart();
        result := NonInteractiveMessageAndWait(aFunc, aArgs);
        self.InterruptSignalEnd();
    finally
        ReleaseSerialAccess();
    end;
end;

function TGUIManager.NonInteractiveMessageAndWait(aFunc: TFunc; const aArgs: TMessageArg): TMessageResult;
// parallel threads can send messages, ( although the main thread will handle the messages in series )
begin
    result := gmMessageAndWait(aFunc, aArgs);
end;

function TGUIManager.InputQuery(const aText, aCaption: string; var vValue: string): boolean;
var
    xResult: variant;
begin
    xResult := InteractiveMessageAndWait(TVCLHandling.VCL,
        VarArrayOf([vclInputQuery, aText, aCaption, vValue]));
    result := xResult[0];
    vValue := xResult[1];
end;

function TGUIManager.InputBox(const aText, aCaption, aDefault: string): string;

begin
    result := aDefault;
    self.InputQuery(aText, aCaption, result);
end;

function TGUIManager.MessageBox(const aText: string; const aCaption: string = ''; aButtons: integer = 0;
    aIcon: integer = 0; aDefaultButton: Integer = 0): Integer;
// synchronisierter Aufruf einer Messagebox:
// grundsätzlich wird die Box mit (GlobalError-)Beep unterlegt und im Logfile vermerkt
var
    xLogString: string;
begin
    result := InteractiveMessageAndWait(TVCLHandling.VCL,
        VararrayOf([vclShowMessage, aText, aCaption, aButtons, aIcon, aDefaultButton]));
    // -------------------------------------------------------------------- Inhalt in Logfile schreiben
    xLogString := Format('Message: %s (%s) - Result: %d', [aText, aCaption, result]);
    gLogManager.Log(xLogString, true);
end;

function TGUIManager.BitmapMessageBox(const aBitmapName, aText, aCaption: string; aButtons: integer): Integer;
var
    xLogString: string;
begin
    result := InteractiveMessageAndWait(TVCLHandling.VCL,
        VararrayOf([vclShowBitmapMessage, aBitmapName, aText, aCaption, aButtons]));
    // -------------------------------------------------------------------- Inhalt in Logfile schreiben
    xLogString := Format('Message: %s (%s) - Result: %d', [aText, aCaption, result]);
    gLogManager.Log(xLogString, true);
end;

function TGUIManager.SetCursor(aCursor: TCursor): integer;
begin
    result := NonInteractiveMessageAndWait(TVCLHandling.VCL, VarArrayOf([vclSetCursor, aCursor]));
end;

procedure TGUIManager.SetControlProp(aControl: TControl; aProperty: TEnControlProp; aValue: variant);
begin
    NonInteractiveMessageAndWait(TVCLHandling.VCL, VarArrayOf([vclSetControlProp, LongInt(aControl),
        aProperty, aValue]));
end;

function TGUIManager.ShowModal(aForm: TForm): TModalResult;
begin
    result := InteractiveMessageAndWait(TVCLHandling.VCL, VarArrayOf([vclShowModal, LongInt(aForm)]));
end;

procedure TGUIManager.InterruptSignalEnd;
begin
    // Dummy
end;

procedure TGUIManager.InterruptSignalMute;
begin
    // Dummy
end;

function TGUIManager.SelectItemBox(const aItems: TStringArray;
    const aText, aCaption, aButtonText: string): string;
var
    xResult: variant;
begin
    xResult := InteractiveMessageAndWait(TVCLHandling.VCL,
        VarArrayOf([vclSelectItemBox, LongInt(aItems), aText, aCaption, aButtonText]));
    result := xResult[0];
end;

procedure TGUIManager.InterruptSignalStart;
begin
    // Dummy
end;


end.
