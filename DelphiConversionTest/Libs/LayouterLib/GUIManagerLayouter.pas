{ --------------------------------------------------------------------------------------------------
  Copyright © 2009 Zinsser Analytic GmbH - All rights reserved.
  Author       : Payman Kamali (pk)
  Description  : Handle calls to Delphi Graphics ( VCL ) functions
  Used by Layouter
  --------------------------------------------------------------------------------------------------
  Revision History:
  date     op  method                            track-no improvement/change
  -------- --  -------------------------------   -------- ------------------------------------------
  08.06.09 pk                                     TN4585.1  Initial Revision
  12.08.09 wl  ToolErr_PromptInput                TN4712    ein Parameter entfernt
  08.09.09 pk                                     TN4753    uses ErrorMessage replaced by ErrorInfo
  18.02.10 pk  LogText_Display                    TN4985.1  New LogText_Display
  20.04.12 wl  Stop_PromptModal                   TN5858   speziell für den Layouter noch gelassen
  29.05.12 wl  LogText_Display                    TN5904    mit ThreadID und DisplayType als Parameter
  -------------------------------------------------------------------------------------------------- }

unit GUIManagerLayouter;


interface


uses
    GUIManagerSetup,
    LogManager,
    ThreadUtils;

type
    TVCLGUIManagerLayouter = (vclStopPromptModal);

    TVCLHandlingLayouter = class
    private
        class function VCL(const aArgs: TMessageArg): TMessageResult;
    end;

    TGUIManagerLayouter = class(TGUIManagerSetup)
    protected
        function Barcode_PromptInputGUI(var vText: string; aCaption: string; aLabelCaption: string;
            aNoRackBtnCaption: string): integer; override;
    public
        procedure LogText_Display(const aLogText: string; aThreadID: integer;
            aDisplayType: TDisplayLogInfoType); override;

        // ErrBox
        function ErrBox_PromptModal(aPErrorInfo: integer): integer; override;
        // ToolErr
        function ToolErr_PromptInput(aMessage: string): integer; override;
        // StopDlg
        function Stop_PromptModal(): integer;
    end;


implementation


uses
    Variants,
    RunDialogsManager,
    Stop,
    ErrorInfo;

{ TGUIManagerLayouter }

function TGUIManagerLayouter.Barcode_PromptInputGUI(var vText: string; aCaption: string;
    aLabelCaption: string; aNoRackBtnCaption: string): integer;
begin
    result := TRunDialogsManager.Instance.Barcode_PromptInputGUI(vText, aCaption, aLabelCaption,
        aNoRackBtnCaption);
end;

function TGUIManagerLayouter.ErrBox_PromptModal(aPErrorInfo: integer): integer;
begin
    result := TRunDialogsManager.Instance.ErrBox_PromptModal(TErrorInfo(aPErrorInfo));
end;

class function TVCLHandlingLayouter.VCL(const aArgs: TMessageArg): TMessageResult;
var
    xType: TVCLGUIManagerLayouter;
begin
    xType := aArgs[0];
    case xType of
        vclStopPromptModal:
            result := TStopDlg.ShowStopDialog;
    end;
end;

function TGUIManagerLayouter.Stop_PromptModal: integer;
begin
    result := InteractiveMessageAndWait(TVCLHandlingLayouter.VCL, VarArrayOf([vclStopPromptModal]));
end;

function TGUIManagerLayouter.ToolErr_PromptInput(aMessage: string): integer;
begin
    result := TRunDialogsManager.Instance.ToolErr_PromptInput(aMessage);
end;

procedure TGUIManagerLayouter.LogText_Display(const aLogText: string; aThreadID: integer;
    aDisplayType: TDisplayLogInfoType);
begin
    TRunDialogsManager.Instance.LogText_Display(aLogText, aThreadID, aDisplayType);
end;


end.
