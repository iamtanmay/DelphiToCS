{ --------------------------------------------------------------------------------------------------
  Copyright © 2005 Zinsser Analytic GmbH - All rights reserved.
  Author       : Payman Kamali (pk)
  Description  : Handle calls to Delphi Graphics ( VCL ) functions
  Used by Layouter, Sampler, ZARunner
  --------------------------------------------------------------------------------------------------
  Revision History:
  date     op  method                            track-no improvement/change
  -------- --  -------------------------------   -------- ------------------------------------------
  11.03.05 pk                                     TN2339.2  Initial Revision
  17.03.05 pk                                     TN2339.2  ErrBox_PromptModal do not check for error
  15.08.05 wl                                     TN2558.1  intern: uses SamGlobe.GlobalErrBeep hierher
  04.10.05 pk  ErrBox_PromptModal                 TN2648    New parameters. call ShowErrorBox.
  31.10.06 pk                                     TN3391    InteractiveMessageAndWait with new SystemEventType parameter
  27.11.06 wl  TVCLHandlingGUIManagerSetup.VCL    TN3243    calls TErrorMessageFactory.ShowErrorBox();
  01.12.06 pk                                     TN3441    SystemEventType parameter removed
  07.12.06 pk  Stop_PromptModal                   TN3455    moved here from GUIManagerRun so that it can be used by Layouter
  07.12.06 wl                                     TN3243    uses SamErr entfernt
  14.04.08 wl                                     TN4060    uses ErrorMessage ( GlobalErrBeep )
  02.09.08 pk                                     TN4215    thrman global variable removed
  02.09.08 pk  Barcode_PromptInput                TN4215    new virtual function
  20.09.08 pk  RunAbortInfo_PromptModal           TN4215    new virtual function
  24.09.08 pk  RackPlaceList_PromptModal          TN4241    new virtual function
  25.09.08 pk  Delay_ChangeTime, etc              TN4241    new virtual function
  10.11.08 pk  Delay_...                          TN4280    Priority changed to ID
  17.11.08 wl  GroupDisplay_InsertInfo            TN4311    new virtual function
  18.12.08 pk  GroupDisplay_InsertInfo            TN4311    New Key field.  GroupName is now stringarray
  06.04.09 pk  LoadDisplayComponentToMain         TN4503    New
  08.04.09 pk  AskRunStart_PromptModal            TN4512    New
  08.06.09 pk                                     TN4585.1  Various functions moved to GUIManagerRun
  12.08.09 wl  ToolErr_PromptInput                TN4712    ein Parameter entfernt
  08.09.09 pk  Start/EndBeep                      TN4753    Beep functionality removed for now
  18.02.10 pk  LogText_Display                    TN4985.1  New LogText_Display
  10.01.12 ts  Start/EndBeep                      TN5760    Beep functionality reactivated
  20.04.12 wl  Stop_PromptModal                   TN5858   entfernt
  29.05.12 wl  LogText_Display                    TN5904    mit ThreadID und DisplayType als Parameter
  30.07.13 wl  InterruptSignalStart,-End          TN6160   entspricht StartBeep, EndBeep
  30.07.13 wl  InterruptSignalMute                TN6160   neu für "Beeper off"-Button
  -------------------------------------------------------------------------------------------------- }

unit GUIManagerSetup;


interface


uses
    GUIManager,
    LogManager,
    ThreadClasses;

type
    TGUIManagerSetup = class(TGUIManager)
    protected
        procedure InterruptSignalStart(); override;
        procedure InterruptSignalEnd(); override;
        function Barcode_PromptInputInterrupt(aSender: TObject; aIRArgs: TInterruptRoutineArgs)
            : TInterruptRoutineResult;
        function Barcode_PromptInputGUI(var vText: string; aCaption: string; aLabelCaption: string;
            aNoRackBtnCaption: string): integer; virtual;
    public
        procedure InterruptSignalMute(); override;

        procedure LogText_Display(const aLogText: string; aThreadID: integer;
            aDisplayType: TDisplayLogInfoType); virtual; abstract;

        function Barcode_PromptInput(var vText: string; aCaption: string; aLabelCaption: string;
            aNoRackBtnCaption: string): integer; virtual;

        // ErrBox
        function ErrBox_PromptModal(aPErrorInfo: integer): integer; virtual;
        // ToolErr
        function ToolErr_PromptInput(aMessage: string): integer; virtual;
    end;


implementation


uses
    ThrdMan,
    Variants;

procedure TGUIManagerSetup.InterruptSignalStart();
begin
    ThrMan.InterruptBeepStart();
end;

procedure TGUIManagerSetup.InterruptSignalEnd();
begin
    ThrMan.InterruptBeepEnd();
end;

procedure TGUIManagerSetup.InterruptSignalMute();
begin
    ThrMan.InterruptBeepEnd();
end;

function TGUIManagerSetup.ErrBox_PromptModal(aPErrorInfo: integer): integer;
begin
    result := 0;
end;

function TGUIManagerSetup.ToolErr_PromptInput(aMessage: string): integer;
begin
    result := 0;
end;

function TGUIManagerSetup.Barcode_PromptInputGUI(var vText: string; aCaption: string; aLabelCaption: string;
    aNoRackBtnCaption: string): integer;
begin
    result := 0;
end;

function TGUIManagerSetup.Barcode_PromptInputInterrupt(aSender: TObject; aIRArgs: TInterruptRoutineArgs)
    : TInterruptRoutineResult;
var
    xInt: integer;
    xStr: string;
begin
    xStr := aIRArgs[0];
    xInt := Barcode_PromptInputGUI(xStr, aIRArgs[1], aIRArgs[2], aIRArgs[3]);
    result := VarArrayOf([xInt, xStr]);
end;

function TGUIManagerSetup.Barcode_PromptInput(var vText: string; aCaption: string; aLabelCaption: string;
    aNoRackBtnCaption: string): integer;
var
    xResult: variant;
begin
    ThrMan.RequestInterrupt('Barcode input', Barcode_PromptInputInterrupt,
        VarArrayOf([vText, aCaption, aLabelCaption, aNoRackBtnCaption]), xResult, true, nil);
    result := xResult[0];
    vText := xResult[1];

end;


end.
