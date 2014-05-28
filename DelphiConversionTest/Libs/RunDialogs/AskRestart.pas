{ ----------------------------------------------------------------------------------------------------------------------
  Copyright © 2010 Zinsser Analytic GmbH - All rights reserved.
  Author       : Payman Kamali (pk)
  Description  :
  ----------------------------------------------------------------------------------------------------------------------
  Revision History:
  date     op  method                       track-no improvement/change
  -------- --  ---------------------------  -------- -------------------------------------------------------------------
  19.05.10 pk                               TN5114   New
  20.05.10 wl                               TN5117   neue Schriftart "Segoe UI", Schriftgröße 9
  09.06.10 pk  DeterminePreviousStepID      TN5077   AV occured when RunStep was null
  21.06.10 wl                               TN5160   Position = poScreenCenter
  26.10.10 pk                               TN5297   Changes for ActionData segment concept
  27.10.10 pk                               TN5297   Panel heights changed
  10.04.13 wl                               TN6045   uses Generics.Collections
  ---------------------------------------------------------------------------------------------------------------------- }

unit AskRestart;


interface


uses
    Windows,
    Messages,
    SysUtils,
    Variants,
    Classes,
    Graphics,
    Controls,
    Forms,
    Dialogs,
    StdCtrls,
    ExtCtrls,
    Generics.Collections,
    ActionIDDataCache,
    ActionDataCache,
    ActionData;

type
    TRestartChoiceVerifyCallback = procedure(aSender: TObject; var vIsVerified: boolean) of object;

    TRestartChoice = class
    private
        fCaption: string;
        fExecuteCallback: TNotifyEvent;
        fVerifyCallback: TRestartChoiceVerifyCallback;
    public
        constructor Create(const aCaption: string; const aExecuteCallback: TNotifyEvent;
            const aVerifyCallback: TRestartChoiceVerifyCallback);
        property Caption: string read fCaption;
        property ExecuteCallback: TNotifyEvent read fExecuteCallback;
        property VerifyCallback: TRestartChoiceVerifyCallback read fVerifyCallback;

    end;

    TFrmAskRestart = class(TForm)
        pnlTop: TPanel;
        Panel2: TPanel;
        Bevel1: TBevel;
        Button2: TButton;
        btnOK: TButton;
        pnlMiddle: TPanel;
        Bevel2: TBevel;
        lblTitle: TLabel;
        edMethodName: TEdit;
        pnlMiddleTop: TPanel;
        mmoRestartDescription: TMemo;
        pnlMiddleBottom: TPanel;
        btnChooseSpecific: TButton;
        mmoSpecificStepDescription: TMemo;
        pnlMiddleMid: TPanel;
        rdgChoices: TRadioGroup;
        procedure rdgChoicesClick(Sender: TObject);
        procedure btnChooseSpecificClick(Sender: TObject);
        procedure btnOKClick(Sender: TObject);
        procedure FormCreate(Sender: TObject);
    private
        { Private declarations }
        fResultContinueAtID: TActionID;
        fResultContinueAtSegmentIndex: integer;

        fSourceDataName: string;
        fDateLastStoppedStr: string;
        fRestartOnlyAtMarks: boolean;
        fPreviousStepID: TActionID;
        fPreviousSegmentIndex: integer;
        fPreviousStepCaption: string;
        fActionListDataCache: TActionListDataCache;
        fChoices: TObjectList<TRestartChoice>;
        fSpecificContinueAtID: integer;
        fIsSpecificContinueAtIDDefined: boolean;
        procedure AddChoice(const aChoice: TRestartChoice);
        procedure AddChoices();
        function GetCurrentChoice(): TRestartChoice;
        procedure ChoiceChanged(const aNewChoice: integer);
        procedure DeterminePreviousStepID();
        procedure ChoiceExecuteRestartAtPreviousStep(aSender: TObject);
        procedure ChoiceExecuteRestartAtNextStep(aSender: TObject);
        procedure ChoiceExecuteRestartAtSpecificStep(aSender: TObject);
        procedure ChoiceExecuteStartNewRun(aSender: TObject);
        procedure ChoiceVerifyRestartAtSpecificStep(aSender: TObject; var vIsVerified: boolean);
        function IsRestartMarkRunStep(const aActionData: TActionData): boolean;
    public
        procedure Init(const aSourceDataName: string; const aDateLastStoppedStr: string;
            const aActionsListDataCache: TActionListDataCache; const aRestartOnlyAtMarks: boolean);
        procedure Display();
        property ResultContinueAtID: TActionID read fResultContinueAtID;
        property ResultContinueAtSegmentIndex: integer read fResultContinueAtSegmentIndex;
        class function InstancePromptModal(const aSourceDataName: string; const aDateLastStoppedStr: string;
            const aActionListDataCache: TActionListDataCache; const aRestartOnlyAtMarks: boolean;
            out oContinueAtID: TActionID; out oContinueAtSegmentIndex: integer): TModalResult;
    end;


implementation


{$R *.dfm}

uses
    RunDialogsManager,
    RunStep,
    RestartSetMarkRunStep,
    ControlUtils;

{ TFrmAskRestart }

procedure TFrmAskRestart.Display;
begin
    self.edMethodName.Text := fSourceDataName;
    self.mmoRestartDescription.Lines.Clear;
    self.mmoRestartDescription.Lines.Add('A previous execution of the method was stopped');
    self.mmoRestartDescription.Lines.Add(Format('at Time: %s', [fDateLastStoppedStr]));
    self.mmoRestartDescription.Lines.Add('');
    self.mmoRestartDescription.Lines.Add('Please choose how you would like to proceed:');

end;

procedure TFrmAskRestart.FormCreate(Sender: TObject);
begin
    TControlUtils.ResetFontForWinXP(self);
end;

procedure TFrmAskRestart.ChoiceExecuteRestartAtPreviousStep(aSender: TObject);
begin
    fResultContinueAtID := fPreviousStepID;
    fResultContinueAtSegmentIndex := fPreviousSegmentIndex;
end;

procedure TFrmAskRestart.ChoiceExecuteRestartAtNextStep(aSender: TObject);
begin
    fResultContinueAtID := -1;
end;

procedure TFrmAskRestart.ChoiceExecuteRestartAtSpecificStep(aSender: TObject);
begin
    self.btnChooseSpecific.Visible := true;
    self.mmoSpecificStepDescription.Visible := true;

    if fIsSpecificContinueAtIDDefined then
        fResultContinueAtID := fSpecificContinueAtID;
end;

procedure TFrmAskRestart.ChoiceExecuteStartNewRun(aSender: TObject);
begin
    fResultContinueAtID := 0;
end;

procedure TFrmAskRestart.ChoiceVerifyRestartAtSpecificStep(aSender: TObject; var vIsVerified: boolean);
begin
    vIsVerified := fIsSpecificContinueAtIDDefined;
end;

procedure TFrmAskRestart.AddChoice(const aChoice: TRestartChoice);
begin
    self.rdgChoices.Items.Add(aChoice.Caption);
    fChoices.Add(aChoice);
end;

procedure TFrmAskRestart.AddChoices();
begin
    if fPreviousStepID > -1 then
    begin
        AddChoice(TRestartChoice.Create(fPreviousStepCaption, ChoiceExecuteRestartAtPreviousStep, nil));
    end;

    if not fRestartOnlyAtMarks then
    begin
        AddChoice(TRestartChoice.Create('Restart at next step', ChoiceExecuteRestartAtNextStep, nil));
    end;

    AddChoice(TRestartChoice.Create('Restart from a specific step (for advanced users only)',
        ChoiceExecuteRestartAtSpecificStep, ChoiceVerifyRestartAtSpecificStep));

    AddChoice(TRestartChoice.Create('Start a fresh run', ChoiceExecuteStartNewRun, nil));
    ChoiceChanged(0);
end;

procedure TFrmAskRestart.btnChooseSpecificClick(Sender: TObject);
var
    xActionsIterator: TActionListDataCacheIterator;
    xContinueAtDescription: string;
    xContinueAtSegmentIndex: integer;
    xContinueAtID: TActionID;
begin
    xActionsIterator := fActionListDataCache.CreateBackwardIterator();
    try
        if TRunDialogsManager.Instance.Trace_PromptModal(fSourceDataName, xActionsIterator,
            fRestartOnlyAtMarks, xContinueAtID, xContinueAtSegmentIndex, xContinueAtDescription) <> mrOK then
            EXIT;
        fIsSpecificContinueAtIDDefined := true;
        fSpecificContinueAtID := xContinueAtID;
        fResultContinueAtID := fSpecificContinueAtID;
        fResultContinueAtSegmentIndex := xContinueAtSegmentIndex;
        mmoSpecificStepDescription.Text := xContinueAtDescription;
    finally
        FreeAndNil(xActionsIterator);
    end;
end;

procedure TFrmAskRestart.btnOKClick(Sender: TObject);
var
    xChoice: TRestartChoice;
    xIsVerified: boolean;
begin
    self.ModalResult := mrNone;
    xIsVerified := true;

    xChoice := GetCurrentChoice();
    if Assigned(xChoice.VerifyCallback) then
        xChoice.VerifyCallback(xChoice, xIsVerified);

    if xIsVerified then
        self.ModalResult := mrOK

end;

function TFrmAskRestart.IsRestartMarkRunStep(const aActionData: TActionData): boolean;
var
    xRunStep: TRunStep;
begin
    result := false;
    if aActionData is TSingleSegmentActionData then
    begin
        if (aActionData as TSingleSegmentActionData).ActionSegment <> nil then
        begin
            xRunStep := (aActionData as TSingleSegmentActionData).ActionSegment.RunStep;
            if xRunStep is TRestartSetMarkRunStep then
            begin
                result := true;
            end;
        end;
    end;
end;

procedure TFrmAskRestart.DeterminePreviousStepID();

var
    xActionData: TActionData;
    xActionsIterator: TActionListDataCacheIterator;

    xActionSegment: TActionRunEffectSegment;
    xActionSegmentIndex, xPreviousActionSegmentIndex: integer;
    xDescription: string;
    x: integer;
begin
    fPreviousStepID := -1;
    fPreviousStepCaption := 'Restart at previous step';
    xActionData := nil;
    xPreviousActionSegmentIndex := -1;

    xActionsIterator := fActionListDataCache.CreateBackwardIterator();
    try

        while xActionsIterator.MoveNext do
        begin

            xActionData := xActionsIterator.Current;

            // if xActionData.Undone or xActionData.IsInEvent then begin
            if xActionData.IsInEvent then
            begin
                CONTINUE;
            end;

            if fRestartOnlyAtMarks then
            begin
                if not IsRestartMarkRunStep(xActionData) then
                    CONTINUE;
            end;

            // if no segments we just assume we start at beginning of action.  But this could be dangerous for Disp actions when dilutors are empty!
            if xActionData.ActionSegmentCount = 0 then
            begin
                xDescription := xActionData.Description;
                BREAK;
            end;

            // Get first restartable index
            xActionSegmentIndex := -1;
            for x := xActionData.ActionSegmentCount - 1 downto 0 do
            begin
                xActionSegment := xActionData.ActionSegments[x];
                if xActionSegment.RunStep.RestartAtStepAllowed then
                begin
                    xActionSegmentIndex := x;
                    BREAK;
                end;
            end;

            if xActionSegmentIndex >= 0 then
            begin
                xActionSegment := xActionData.ActionSegments[xActionSegmentIndex];
                xDescription := xActionSegment.RunStep.Description;
                if xActionData.IsUndoablePerSegment then
                begin
                    xPreviousActionSegmentIndex := xActionSegmentIndex;
                end;
                BREAK;
            end;

        end;

        if not Assigned(xActionData) then
            EXIT;

        fPreviousStepCaption := 'Restart at previous step: ' + #13#10 + xDescription;
        fPreviousSegmentIndex := xPreviousActionSegmentIndex;

        fPreviousStepID := xActionData.ActionID;
    finally
        FreeAndNil(xActionsIterator);
    end;
end;

procedure TFrmAskRestart.Init(const aSourceDataName, aDateLastStoppedStr: string;
    const aActionsListDataCache: TActionListDataCache; const aRestartOnlyAtMarks: boolean);
begin
    fSourceDataName := aSourceDataName;
    fDateLastStoppedStr := aDateLastStoppedStr;
    fActionListDataCache := aActionsListDataCache;
    fRestartOnlyAtMarks := aRestartOnlyAtMarks;
    fChoices := TObjectList<TRestartChoice>.Create();
    DeterminePreviousStepID();
    AddChoices();
    fIsSpecificContinueAtIDDefined := false;
    fSpecificContinueAtID := -2;
end;

class function TFrmAskRestart.InstancePromptModal(const aSourceDataName: string;
    const aDateLastStoppedStr: string; const aActionListDataCache: TActionListDataCache;
    const aRestartOnlyAtMarks: boolean; out oContinueAtID: TActionID; out oContinueAtSegmentIndex: integer)
    : TModalResult;
var
    xDialog: TFrmAskRestart;
begin
    xDialog := TFrmAskRestart.Create(nil);
    try
        oContinueAtID := -1;
        xDialog.Init(aSourceDataName, aDateLastStoppedStr, aActionListDataCache, aRestartOnlyAtMarks);
        xDialog.Display();
        result := xDialog.ShowModal;
        if result = mrOK then
        begin
            oContinueAtID := xDialog.ResultContinueAtID;
            oContinueAtSegmentIndex := xDialog.ResultContinueAtSegmentIndex;
        end;
    finally
        FreeAndNil(xDialog);
    end;
end;

function TFrmAskRestart.GetCurrentChoice(): TRestartChoice;
begin
    result := fChoices[self.rdgChoices.ItemIndex];
end;

procedure TFrmAskRestart.ChoiceChanged(const aNewChoice: integer);
var
    xChoice: TRestartChoice;
begin
    self.btnChooseSpecific.Visible := false;
    self.mmoSpecificStepDescription.Visible := false;

    self.rdgChoices.ItemIndex := aNewChoice;

    xChoice := GetCurrentChoice();
    xChoice.ExecuteCallback(xChoice);
end;

procedure TFrmAskRestart.rdgChoicesClick(Sender: TObject);
begin
    ChoiceChanged(self.rdgChoices.ItemIndex);
end;

{ TRestartChoice }

constructor TRestartChoice.Create(const aCaption: string; const aExecuteCallback: TNotifyEvent;
    const aVerifyCallback: TRestartChoiceVerifyCallback);
begin
    inherited Create();
    fCaption := aCaption;
    fExecuteCallback := aExecuteCallback;
    fVerifyCallback := aVerifyCallback;
end;


end.
