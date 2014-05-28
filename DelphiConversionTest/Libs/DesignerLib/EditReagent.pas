{ --------------------------------------------------------------------------------------------------
  Copyright © 2005 Zinsser Analytic GmbH - All rights reserved.
  Author       : Wolfgang Lyncke (wl)
  Description  : Modal Form to insert reagent data
  --------------------------------------------------------------------------------------------------
  Revision History:
  date     op  method                       track-no  improvement/change
  -------- --  ---------------------------  --------- ----------------------------------------------
  03.09.05 wl                               TN2541.4  initial version
  12.10.05 wl  Create                       TN2541.4  enthält Row und Column
  04.03.06 wl                               TN2541.4  Dropdownliste wird mit Reagenzien gefüllt
  04.11.09 pk                               TN4843    Replace TObjectList, TStringList, TStrings with Generics Lists and TStringArray
  20.05.10 wl                               TN5117   neue Schriftart "Segoe UI", Schriftgröße 9
  21.06.10 wl                               TN5160   Position = poScreenCenter
  03.02.12 wl                               TN5792   benutzt SubstanceDataDataAdaptor
  -------------------------------------------------------------------------------------------------- }

unit EditReagent;


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
    StdCtrls;

type
    TEditReagentOption = (edrMultiSelect, edrStartVolume);
    TEditReagentOptions = set of TEditReagentOption;

    TfrmEditReagent = class(TForm)
        Label1: TLabel;
        Label2: TLabel;
        edVolume: TEdit;
        Button1: TButton;
        Button2: TButton;
        Label3: TLabel;
        Label4: TLabel;
        edCol: TEdit;
        edRow: TEdit;
        uL: TLabel;
        cbReagentName: TComboBox;
        procedure Button2Click(Sender: TObject);
        procedure Button1Click(Sender: TObject);
    private
        function GetRow: integer;
        procedure SetRow(const Value: integer);
        function GetCol: integer;
        procedure SetCol(const Value: integer);
        function GetAmount: extended;
        function GetReagentName: string;
        procedure SetAmount(const Value: extended);
        procedure SetReagentName(const Value: string);
    public
        constructor Create(aRow, aCol: integer; aOptions: TEditReagentOptions); reintroduce;
        //
        property Row: integer read GetRow write SetRow;
        property Col: integer read GetCol write SetCol;
        property ReagentName: string read GetReagentName write SetReagentName;
        property Amount: extended read GetAmount write SetAmount;
    end;


implementation


{$R *.dfm}

uses
    SubstanceDataDataAdaptor,
    GeneralTypes,
    ControlUtils;

procedure TfrmEditReagent.Button2Click(Sender: TObject);
begin
    self.Close;
end;

procedure TfrmEditReagent.Button1Click(Sender: TObject);
begin
    self.ModalResult := mrOK;
end;

function TfrmEditReagent.GetRow: integer;
begin
    result := StrToInt(edRow.Text);
end;

procedure TfrmEditReagent.SetRow(const Value: integer);
begin
    edRow.Text := IntToStr(Value);
end;

function TfrmEditReagent.GetCol: integer;
begin
    result := StrToInt(edCol.Text);
end;

procedure TfrmEditReagent.SetCol(const Value: integer);
begin
    edCol.Text := IntToStr(Value);
end;

function TfrmEditReagent.GetAmount: extended;
begin
    result := StrToFloat(edVolume.Text);
end;

function TfrmEditReagent.GetReagentName: string;
begin
    result := cbReagentName.Text;
end;

procedure TfrmEditReagent.SetAmount(const Value: extended);
begin
    edVolume.Text := FloatToStr(Value);
end;

procedure TfrmEditReagent.SetReagentName(const Value: string);
begin
    cbReagentName.Text := Value;
end;

constructor TfrmEditReagent.Create(aRow, aCol: integer; aOptions: TEditReagentOptions);
var
    xNames: TStringArray;
begin
    inherited Create(Application);

    TControlUtils.ResetFontForWinXP(self);
    SetRow(aRow);
    SetCol(aCol);

    if (edrMultiSelect in aOptions) then
    begin
        cbReagentName.Visible := false;
        Label1.Visible := false;
    end
    else
    begin
        xNames := TSubstanceDataDataAdaptor.InstReadAllNames();
        TControlUtils.AddValuesToComboBox(xNames, cbReagentName, true);
    end;

    if (edrStartVolume in aOptions) then
        Label2.Caption := 'Start Volume:'
    else
        Label2.Caption := 'Volume:'
end;


end.
