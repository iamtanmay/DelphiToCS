unit CoordSystemRelationSubEditor;
{ --------------------------------------------------------------------------------------------------
  Copyright © 2008 Zinsser Analytic GmbH - All rights reserved.
  Author       : Payman Kamali (pk)
  Description  : Editor for a Coordinate System Relation
  --------------------------------------------------------------------------------------------------
  Revision History:
  date     op  method                       track-no improvement/change
  -------- --  ---------------------------  -------- -----------------------------------------------
  23.06.08 pk                                TN4139  Initial Revision
  10.08.09 wl                                TN4702   Strings werden jetzt direkt geladen
  10.02.11 wl                                TN5475   TfraCoordSystemRelation ist TForm statt TFrame
  -------------------------------------------------------------------------------------------------- }


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
    CoordSystemMath;

type
    TfraCoordSystemRelation = class(TForm)
        GroupBox1: TGroupBox;
        edTranslateX: TEdit;
        edTranslateY: TEdit;
        edTranslateZ: TEdit;
        Label1: TLabel;
        Label2: TLabel;
        Label3: TLabel;
        GroupBox2: TGroupBox;
        chkReflectX: TCheckBox;
        chkReflectY: TCheckBox;
        chkReflectZ: TCheckBox;
        GroupBox3: TGroupBox;
        Label7: TLabel;
        Label8: TLabel;
        Label9: TLabel;
        edRotateX: TEdit;
        edRotateY: TEdit;
        edRotateZ: TEdit;
    private
        fCoordSystemRelation: TCoordSystemRelation;
        function CheckRotationValue(const aRotationName: string; aRotationValue: double): boolean;
    public
        procedure CoordSystemRelationToGUI();
        procedure CoordSystemRelationFromGUI();
        procedure ChangeEnabled(aValue: boolean);
        function ValuesOK(): boolean;
        property CoordSystemRelation: TCoordSystemRelation read fCoordSystemRelation
            write fCoordSystemRelation;
    end;


implementation


uses
    DialogUtils,
    GeneralTypes;

{$R *.dfm}
{ TfraCoordSystemRelation }

function TfraCoordSystemRelation.CheckRotationValue(const aRotationName: string;
    aRotationValue: double): boolean;
begin
    result := Abs(aRotationValue) < 360;
    if not result then
        TDialogUtils.MessageBox(TLanguageString.
            Read('Rotation value {0} for {1} must be a value between 360 and -360 degrees',
            'Rotation-Wert {0} für {1} muss zwischen 360 und -360 Grad sein', [aRotationValue, aRotationName]
            ), '', MB_OK)
end;

procedure TfraCoordSystemRelation.CoordSystemRelationToGUI;
begin
    edTranslateX.Text := FloatToStr(fCoordSystemRelation.TranslateX);
    edTranslateY.Text := FloatToStr(fCoordSystemRelation.TranslateY);
    edTranslateZ.Text := FloatToStr(fCoordSystemRelation.TranslateZ);

    chkReflectX.Checked := fCoordSystemRelation.ReflectX;
    chkReflectY.Checked := fCoordSystemRelation.ReflectY;
    chkReflectZ.Checked := fCoordSystemRelation.ReflectZ;

    edRotateX.Text := FloatToStr(fCoordSystemRelation.RotateX);
    edRotateY.Text := FloatToStr(fCoordSystemRelation.RotateY);
    edRotateZ.Text := FloatToStr(fCoordSystemRelation.RotateZ);
end;

function TfraCoordSystemRelation.ValuesOK(): boolean;
begin
    result := false;
    if not CheckRotationValue('X', StrToFloat(edRotateX.Text)) then
        EXIT;
    if not CheckRotationValue('Y', StrToFloat(edRotateY.Text)) then
        EXIT;
    if not CheckRotationValue('Z', StrToFloat(edRotateZ.Text)) then
        EXIT;
    result := true;
end;

procedure TfraCoordSystemRelation.CoordSystemRelationFromGUI;
begin

    fCoordSystemRelation.TranslateX := StrToFloat(edTranslateX.Text);
    fCoordSystemRelation.TranslateY := StrToFloat(edTranslateY.Text);
    fCoordSystemRelation.TranslateZ := StrToFloat(edTranslateZ.Text);

    fCoordSystemRelation.ReflectX := chkReflectX.Checked;
    fCoordSystemRelation.ReflectY := chkReflectY.Checked;
    fCoordSystemRelation.ReflectZ := chkReflectZ.Checked;

    fCoordSystemRelation.RotateX := StrToFloat(edRotateX.Text);
    fCoordSystemRelation.RotateY := StrToFloat(edRotateY.Text);
    fCoordSystemRelation.RotateZ := StrToFloat(edRotateZ.Text);
end;

procedure EnabledAsParent(container: TWinControl);
var
    index: integer;
    aControl: TControl;
    isContainer: boolean;
begin
    for index := 0 to -1 + container.ControlCount do
    begin
        aControl := container.Controls[index];

        aControl.Enabled := container.Enabled;

        isContainer := (csAcceptsControls in container.Controls[index].ControlStyle);

        if (isContainer) and (aControl is TWinControl) then
        begin
            // recursive for child controls
            EnabledAsParent(TWinControl(container.Controls[index]));
        end;
    end;
end;

procedure TfraCoordSystemRelation.ChangeEnabled(aValue: boolean);
begin
    self.Enabled := aValue;
    EnabledAsParent(self);
end;


end.
