unit ufrmSchedStatus;
{ --------------------------------------------------------------------------------------------------
  Copyright © 2002 Zinsser Analytic GmbH - All rights reserved.
  Author       : Payman Kamali (pk)
  Description  :
  --------------------------------------------------------------------------------------------------
  Revision History:
  date     op  method                       track-no improvement/change
  -------- --  ---------------------------  -------- -----------------------------------------------
  12.03.03 wl                               TN1293.5 uses posTools
  08.12.03 pk                               TN1697   Various changes
  07.11.05 pk InitInternal                  TN2737   Make window shorter and wider
  19.11.08 wl                               TN4312   AddArrayToList ersetzt
  20.05.10 wl                               TN5117   neue Schriftart "Segoe UI", Schriftgröße 9
  -------------------------------------------------------------------------------------------------- }


interface


uses
    Windows,
    Messages,
    SysUtils,
    Classes,
    Graphics,
    Controls,
    Forms,
    Dialogs,
    StdCtrls,
    ExtCtrls,
    Grids,
    syncobjs,
    Buttons,
    ThreadUtils;

const
    COLARR_ROW_COLORS: array [0 .. 9] of TColor = (clWhite, $00FEA75F { blue } , $007CF9FC { yellow } ,
        $006BDD4A { green } , $00D0D044 { Aqua } , $00E856F8 { Fuchsia } , $007373B9 { beige } ,
        $0055AAFF { orange } , $00C693BD { purple } , $008A72FC { red } );

type
    TEnThreadType = (ttSession, ttProcess, ttAction);
    PForm = ^TForm;

    TfrmSchedStatus = class(TForm)
        Timer1: TTimer;
        Panel1: TPanel;
        Panel2: TPanel;
        edTime: TEdit;
        sgStats: TStringGrid;
        procedure FormClose(Sender: TObject; var Action: TCloseAction);
        procedure Timer1Timer(Sender: TObject);
        procedure FormCreate(Sender: TObject);
        procedure sgStatsDrawCell(Sender: TObject; ACol, ARow: Integer; Rect: TRect; State: TGridDrawState);
        procedure Button1Click(Sender: TObject);
    private
        fRowCount: integer;
        FPtr: PForm;
        FTimeSoFar: cardinal;
        FCriticalSection: TCriticalSection;
        function VCL(const aArgs: TMessageArg): TMessageResult;
        procedure SetStatusInternal(aBytColor: byte; aCrdTime: cardinal; aIntThreadID: integer;
            aStrStatus: string);
        procedure InitInternal();
    public
        constructor Create(aPointer: PForm); reintroduce;
        procedure Init();
        procedure SetStatus(aBytColor: byte; aCrdTime: cardinal; aIntThreadID: integer; aStrStatus: string);
        procedure SetTimer(aBlnEnabled: boolean; aDblOffSet: double);
    end;


implementation


{$R *.DFM}

uses
    Variants,
    Math,
    ControlUtils,
    UtilLib;

const
    INT_FIRST_FIXED_ROW_INDEX = 0;

type
    TVCL = (vclInit, vclUpdate);

constructor TfrmSchedStatus.Create(aPointer: PForm);
begin
    inherited Create(nil);
    FPtr := aPointer;
    fRowCount := 0;
end;

procedure TfrmSchedStatus.FormClose(Sender: TObject; var Action: TCloseAction);
begin
    FCriticalSection.Free;
    FPtr^ := nil;
    Action := caFree;
end;

function TfrmSchedStatus.VCL(const aArgs: TMessageArg): TMessageResult;
var
    xType: TVCL;
begin
    xType := aArgs[0];

    case xType of
        vclInit:
            InitInternal();
        vclUpdate:
            SetStatusInternal(aArgs[1], aArgs[2], aArgs[3], aArgs[4]);
    end;

end;

procedure TfrmSchedStatus.SetStatus(aBytColor: byte; aCrdTime: cardinal; aIntThreadID: integer;
    aStrStatus: string);
begin
    ThreadUtils.gmMessageAndGo(self.VCL, VarArrayOf([vclUpdate, aBytColor, integer(aCrdTime), aIntThreadID,
        aStrStatus]));
end;

procedure TfrmSchedStatus.SetStatusInternal(aBytColor: byte; aCrdTime: cardinal; aIntThreadID: integer;
    aStrStatus: string);
var
    xRowIndex: integer;
begin

    try
        Inc(fRowCount);

        sgStats.RowCount := fRowCount + sgStats.FixedRows;
        xRowIndex := (fRowCount - 1) + sgStats.FixedRows;

        sgStats.Rows[xRowIndex].Add(IntToStr(aBytColor));
        sgStats.Rows[xRowIndex].Add(IntToStr(aCrdTime));
        sgStats.Rows[xRowIndex].Add(IntToStr(aIntThreadID));
        sgStats.Rows[xRowIndex].Add(aStrStatus);

        sgStats.TopRow := Max(1, sgStats.RowCount - 30);

    except
        on E: Exception do
            raise Exception.CreateFmt('TfrmSchedStatus.SetStatus -> %s', [e.Message]);
    end;

    // self.memStatus.Lines.Add(format('%d     %s%d: %s',[aCrdTime, Indent(Integer(aThreadType)), aIntThreadID, aStrStatus]));
end;

procedure TfrmSchedStatus.Timer1Timer(Sender: TObject);
begin
    Inc(FTimeSoFar);
    self.edTime.Text := IntToStr(FTimeSoFar);
end;

procedure TfrmSchedStatus.FormCreate(Sender: TObject);
begin
    TControlUtils.ResetFontForWinXP(self);

    FCriticalSection := TCriticalSection.Create();
end;

procedure TfrmSchedStatus.Init();
begin
    ThreadUtils.gmMessageAndWait(self.VCL, VarArrayOf([vclInit]));
end;

procedure TfrmSchedStatus.InitInternal();
begin
    self.Top := Application.MainForm.Top + 5;
    self.Height := Application.MainForm.ClientHeight - 410;
    self.Width := 350;

    self.Left := Application.MainForm.ClientWidth - self.Width - 20;

    FTimeSoFar := 0;

    sgStats.RowCount := sgStats.FixedRows + 1;
    sgStats.Rows[INT_FIRST_FIXED_ROW_INDEX].Add('Color');
    sgStats.Rows[INT_FIRST_FIXED_ROW_INDEX].Add('Time');
    sgStats.Rows[INT_FIRST_FIXED_ROW_INDEX].Add('ThreadID');
    sgStats.Rows[INT_FIRST_FIXED_ROW_INDEX].Add('Status');

    fRowCount := 0;
end;

procedure TfrmSchedStatus.SetTimer(aBlnEnabled: boolean; aDblOffSet: double);
begin
    FTimeSoFar := Round(aDblOffSet / 1000);
    Timer1.Enabled := aBlnEnabled;
end;

procedure TfrmSchedStatus.sgStatsDrawCell(Sender: TObject; ACol, ARow: Integer; Rect: TRect;
    State: TGridDrawState);
var
    intColorIndex: integer;
    xColor: TColor;
begin
    xColor := clBtnFace;
    intColorIndex := StrToIntDef(sgStats.Cells[0, ARow], -1);
    if intColorIndex >= 0 then
        xColor := COLARR_ROW_COLORS[intColorIndex];
    with sgStats.Canvas do
    begin
        Brush.Color := xColor;
        FillRect(Rect);
        TextRect(Rect, Rect.Left + 2, Rect.Top + 2, sgStats.Cells[ACol, ARow]);
        Brush.Color := clBlack;
    end;
end;

procedure TfrmSchedStatus.Button1Click(Sender: TObject);
begin
    sgStats.Repaint();
end;


end.
