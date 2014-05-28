{ -----------------------------------------------------------------------------------------------------------------------
  Copyright © 2010 Zinsser Analytic GmbH - All rights reserved.
  Author       : Payman Kamali (pk)
  Description  :
  -----------------------------------------------------------------------------------------------------------------------
  Revision History:
  date     op  method                                track-no  improvement/change
  -------- --  ------------------------------------- --------  ----------------------------------------------------------
  22.06.10 pk                                        TN5088    Initial revision
  29.09.10 pk                                        TN5284    New BLANK action
  14.04.11 wl                                        TN5288    hat jetzt sinnvolle Kommentare
  08.08.12 wl  TIf.., TWhile.., TBlank..             TN5946   --> IfRunStep, WhileRunStep, BlankRunStep
  25.06.13 wl  TControlFlowReturnRunStepBuilder      TN6178   neu
  ----------------------------------------------------------------------------------------------------------------------- }

unit ControlFlowRunStepBuilder;


interface


uses
    RunStepBuilder;

type
    TControlFlowBlockRunStepBuilder = class(TRunStepByMethodStepBuilder)
    protected
        fRelativeAddress: integer;
    protected
        function DoIsMatchingControlFlow(const aRunStepBuilder: TControlFlowBlockRunStepBuilder): boolean;
            virtual; abstract;
    public
        function IsMatchingControlFlow(const aRunStepBuilder: TControlFlowBlockRunStepBuilder): boolean;
        property RelativeAddress: integer read fRelativeAddress write fRelativeAddress;
    end;

    TControlFlowBlockBeginRunStepBuilder = class(TControlFlowBlockRunStepBuilder)
    protected
        function DoIsMatchingControlFlow(const aRunStepBuilder: TControlFlowBlockRunStepBuilder)
            : boolean; override;
    end;

    TControlFlowBlockEndRunStepBuilder = class(TControlFlowBlockRunStepBuilder)
    protected
        function DoIsMatchingControlFlow(const aRunStepBuilder: TControlFlowBlockRunStepBuilder)
            : boolean; override;
    end;

    TControlFlowReturnRunStepBuilder = class(TControlFlowBlockRunStepBuilder)
    protected
        function DoIsMatchingControlFlow(const aRunStepBuilder: TControlFlowBlockRunStepBuilder)
            : boolean; override;
    end;


implementation


{ TControlFlowBlockRunStepBuilder }

function TControlFlowBlockRunStepBuilder.IsMatchingControlFlow(const aRunStepBuilder
    : TControlFlowBlockRunStepBuilder): boolean;
begin
    result := DoIsMatchingControlFlow(aRunStepBuilder);
end;

{ TControlFlowBlockBeginRunStepBuilder }

function TControlFlowBlockBeginRunStepBuilder.DoIsMatchingControlFlow(const aRunStepBuilder
    : TControlFlowBlockRunStepBuilder): boolean;
begin
    result := aRunStepBuilder is TControlFlowBlockEndRunStepBuilder;
end;

{ TControlFlowBlockEndRunStepBuilder }

function TControlFlowBlockEndRunStepBuilder.DoIsMatchingControlFlow(const aRunStepBuilder
    : TControlFlowBlockRunStepBuilder): boolean;
begin
    result := aRunStepBuilder is TControlFlowBlockBeginRunStepBuilder;
end;

{ TControlFlowReturnRunStepBuilder }

function TControlFlowReturnRunStepBuilder.DoIsMatchingControlFlow(const aRunStepBuilder
    : TControlFlowBlockRunStepBuilder): boolean;
begin
    result := aRunStepBuilder is TControlFlowBlockBeginRunStepBuilder;
end;


end.
