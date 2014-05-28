{ --------------------------------------------------------------------------------------------------
  Copyright © 2007 Zinsser Analytic GmbH - All rights reserved.
  Author       : Payman Kamali (pk)
  Description  : Data Adaptor for the RACKS.DB
  --------------------------------------------------------------------------------------------------
  Revision History:
  date     op  method                       track-no improvement/change
  -------- --  ---------------------------  -------- -----------------------------------------------
  02.10.07 wl                               TN3811.5 benutzt self.Query statt fQuery
  09.11.07 pk                               TN3921   Changes for updatemanager
  09.11.07 pk                               TN3922   Dataset changed to DataProvider
  09.11.07 pk                               TN3924   Steps changed to mm
  27.06.08 pk  WriteRack                    TN4139   New
  23.09.08 wl                               TN4290   Vereinigung mit RackFieldnames
  29.10.08 wl  TRackRec                     TN4290   neu: TubePutOffset_mm
  29.10.08 wl  WriteRecToDataset            TN4290   TubeGet/Put-Werte werden als integer geschrieben
  16.01.09 wl                               TN4362   an Änderungen in TQueryDataAdaptor angepasst
  09.07.09 pk                               TN4585.4 DataProvider.Locate removed, functionality replaced by SelectAndOpen
  17.06.10 pk                               TN5152.1 Change SQLs so that they also work for TurboDB (dbl quoatations to single)
  08.02.11 wl  Instance                     TN5475   entfernt
  24.02.11 wl                               TN5431   3 neue Felder: WellsAroundCenter (boolean), CenterX_mm, CenterY_mm
  24.02.11 wl  MakeDefaultRackRec           TN5431   Default-Werte für neue Felder
  05.05.11 ts                               TN5552   new: TubeGripOffset_mm
  05.09.11 ts                               TN5683   new: DoNotPaintTubes
  10.11.11 ts                               TN5702   new: DiscretePositions
  10.09.12 ts                               TN5977   new: RackMoveZTravelOffset
  13.11.12 wl  GetKeyFields                 TN6015   kann entfernt werden, wenn Key = NameField
  -------------------------------------------------------------------------------------------------- }

unit RackDataAdaptor;


interface


uses
    Classes,
    GeneralTypes,
    DataProvider,
    QueryDataAdaptor,
    DiscreteRackDataAdaptor;

type
    TRackRec = record
        name: string;
        TYP: integer;
        X_mm, Y_mm, Z_mm: double;
        Rows: integer;
        Cols: integer;
        TubeTyp: integer;
        TubeX_mm: double;
        TubeY_mm: double;
        TubeZ_mm: double;
        BorderX_mm: double;
        BorderY_mm: double;
        PosX_First_mm: double;
        PosY_First_mm: double;
        PosX_Last_mm: double;
        PosY_Last_mm: double;
        PosX_Offset_mm: double;
        PosY_Offset_mm: double;
        ZTravel_mm: double;
        ZScan_mm: double;
        ZDisp_mm: double;
        ZMax_mm: double;
        H_XTake_mm: double;
        H_YTake_mm: double;
        H_ZTake_mm: double;
        H_VOpen_mm: double;
        H_VClose_mm: double;
        H_VIsUndefined: boolean;
        AddOffsetX_AfterPos: integer;
        AddOffsetX_mm: double;
        AddOffsetY_AfterPos: integer;
        AddOffsetY_mm: double;
        MOffsetZ_mm: double;
        MOffsetUpX_mm: double;
        MOffsetUpY_mm: double;
        Shift_Radius_mm: double;
        Shift_NoOfSteps: integer;
        TubeY2_mm: double;
        ZPosX_First_mm: double;
        ZPosX_Last_mm: double;
        ZPosY_First_mm: double;
        ZPosY_Last_mm: double;
        CapType: string;
        CapDiameter: double;
        Cap_GripZ_mm: double;
        Color: integer;
        Z_LastPos_mm: double;
        SlopeType: integer;
        H_Direction: integer;
        TubeGetOpen_mm: double;
        TubeGetClose_mm: double;
        TubePutOpen_mm: double;
        TubePutOffset_mm: double;
        TubeGripOffset_mm: double;
        H_RTake_Degree: double;
        BlockBalanceDoor: boolean;
        StackHeight_mm: double;
        WellsAroundCenter: boolean;
        CenterX_mm: double;
        CenterY_mm: double;
        DoNotPaintTubes: boolean;
        DiscretePositions: boolean;
        DiscretePosRecArray: TDiscretePosRecArray;
        RackMoveZTravelOffset: double;
    end;

    TRackRecArray = array of TRackRec;

    TRackDataAdaptor = class(TQueryDataAdaptor)
    private const
        STR_RACKS_TBL = 'RACKS';
        STR_RACKS_FLD_NAME = 'NAME';
        STR_RACKS_FLD_TYP = 'TYP';
        STR_RACKS_FLD_X_MM = 'X_mm';
        STR_RACKS_FLD_Y_MM = 'Y_mm';
        STR_RACKS_FLD_Z_MM = 'Z_mm';
        STR_RACKS_FLD_ROWS = 'Rows';
        STR_RACKS_FLD_COLS = 'Cols';
        STR_RACKS_FLD_TUBETYP = 'TubeTyp';
        STR_RACKS_FLD_TUBEX_MM = 'TubeX_mm';
        STR_RACKS_FLD_TUBEY_MM = 'TubeY_mm';
        STR_RACKS_FLD_TUBEZ_MM = 'TubeZ_mm';
        STR_RACKS_FLD_BORDERX_MM = 'BorderX_mm';
        STR_RACKS_FLD_BORDERY_MM = 'BorderY_mm';
        STR_RACKS_FLD_POSX_FIRST_MM = 'PosX_First_mm';
        STR_RACKS_FLD_POSY_FIRST_MM = 'PosY_First_mm';
        STR_RACKS_FLD_POSX_LAST_MM = 'PosX_Last_mm';
        STR_RACKS_FLD_POSY_LAST_MM = 'PosY_Last_mm';
        STR_RACKS_FLD_POSX_OFFSET_MM = 'PosX_Offset_mm';
        STR_RACKS_FLD_POSY_OFFSET_MM = 'PosY_Offset_mm';
        STR_RACKS_FLD_ZTRAVEL_MM = 'ZTravel_mm';
        STR_RACKS_FLD_ZSCAN_MM = 'ZScan_mm';
        STR_RACKS_FLD_ZDISP_MM = 'ZDisp_mm';
        STR_RACKS_FLD_ZMAX_MM = 'ZMax_mm';
        STR_RACKS_FLD_H_XTAKE_MM = 'H_XTake_mm';
        STR_RACKS_FLD_H_YTAKE_MM = 'H_YTake_mm';
        STR_RACKS_FLD_H_ZTAKE_MM = 'H_ZTake_mm';
        STR_RACKS_FLD_H_VOPEN_MM = 'H_VOpen_mm';
        STR_RACKS_FLD_H_VCLOSE_MM = 'H_VClose_mm';
        STR_RACKS_FLD_ADDOFFSETX_AFTERPOS = 'AddOffsetX_AfterPos';
        STR_RACKS_FLD_ADDOFFSETX_MM = 'AddOffsetX_mm';
        STR_RACKS_FLD_ADDOFFSETY_AFTERPOS = 'AddOffsetY_AfterPos';
        STR_RACKS_FLD_ADDOFFSETY_MM = 'AddOffsetY_mm';
        STR_RACKS_FLD_MOFFSETZ_MM = 'MOffsetZ_mm';
        STR_RACKS_FLD_MOFFSETUPX_MM = 'MOffsetUpX_mm';
        STR_RACKS_FLD_MOFFSETUPY_MM = 'MOffsetUpY_mm';
        STR_RACKS_FLD_SHIFT_RADIUS_MM = 'Shift_Radius_mm';
        STR_RACKS_FLD_SHIFT_NOOFSTEPS = 'Shift_NoOfSteps';
        STR_RACKS_FLD_TUBEY2_MM = 'TubeY2_mm';
        STR_RACKS_FLD_ZPOSX_FIRST_MM = 'ZPosX_First_mm';
        STR_RACKS_FLD_ZPOSX_LAST_MM = 'ZPosX_Last_mm';
        STR_RACKS_FLD_ZPOSY_FIRST_MM = 'ZPosY_First_mm';
        STR_RACKS_FLD_ZPOSY_LAST_MM = 'ZPosY_Last_mm';
        STR_RACKS_FLD_CAPTYPE = 'CapType';
        STR_RACKS_FLD_CAPDIAMETER = 'CapDiameter';
        STR_RACKS_FLD_CAP_GRIPZ_MM = 'Cap_GripZ_mm';
        STR_RACKS_FLD_COLOR = 'Color';
        STR_RACKS_FLD_Z_LASTPOS_MM = 'Z_LastPos_mm';
        STR_RACKS_FLD_SLOPETYPE = 'SlopeType';
        STR_RACKS_FLD_H_DIRECTION = 'H_Direction';
        STR_RACKS_FLD_TUBEGETOPEN_MM = 'TubeGetOpen_mm';
        STR_RACKS_FLD_TUBEGETCLOSE_MM = 'TubeGetClose_mm';
        STR_RACKS_FLD_TUBEPUTOPEN_MM = 'TubePutOpen_mm';
        STR_RACKS_FLD_TUBEPUTOFFSET_MM = 'TUBEPUTOFFSET_MM';
        STR_RACKS_FLD_TUBEGRIPOFFSET_MM = 'TUBEGRIPOFFSET_MM';
        STR_RACKS_FLD_H_RTAKE_DEGREE = 'H_RTake_Degree';
        STR_RACKS_FLD_BLOCKBALANCEDOOR = 'BlockBalanceDoor';
        STR_RACKS_FLD_STACKHEIGHT_MM = 'StackHeight_mm';
        STR_RACKS_FLD_WellsAroundCenter = 'WellsAroundCenter';
        STR_RACKS_FLD_CenterX_mm = 'CenterX_mm';
        STR_RACKS_FLD_CenterY_mm = 'CenterY_mm';
        STR_RACKS_FLD_DoNotPaintTubes = 'DoNotPaintTubes';
        STR_RACKS_FLD_DiscretePositions = 'DiscretePositions';
        STR_RACKS_FLD_RackMoveZTravelOffset = 'RackMoveZTravelOffset';

        INT_RACKS_FLDLEN_NAME = 20;
        INT_RACKS_FLDLEN_CAPTYPE = 10;

        STR_RACKS_INDEX_FIELDS = STR_RACKS_FLD_NAME;

        INT_MOST_RECENT_VERSION = 1;

        STR_SQL_WHERE_RACK = ' WHERE ' + STR_RACKS_FLD_NAME + ' =''%s''';
        STR_SQL_FROM = ' FROM ' + STR_RACKS_TBL;
        STR_SQL_DELETE_FMT = 'DELETE ' + STR_SQL_FROM + STR_SQL_WHERE_RACK;
        STR_SQL_SELECT_RACKS_FMT = 'SELECT * ' + STR_SQL_FROM + STR_SQL_WHERE_RACK;
        STR_SQL_SELECT_ALL = 'SELECT * ' + STR_SQL_FROM;
    protected
        function GetNameField(): string; override;
    public
        constructor Create();

        procedure SelectAndOpenRack(const aRack: string; aReadOnly: boolean);
        procedure DeleteRack(const aRack: string);
        procedure WriteRecs(const aRecs: TRackRecArray);
        procedure WriteRack(const aRec: TRackRec);
        function ReadRack(const aRack: string; var vRec: TRackRec): boolean;
        procedure ReadRecs(var vRecs: TRackRecArray);
        class procedure ReadRecFromDataset(aDataset: TDataProvider; var vRec: TRackRec);
        class procedure ReadRecsFromDataset(aDataset: TDataProvider; var vRecs: TRackRecArray);
        class procedure WriteRecsToDataset(aDataset: TDataProvider; const aRecs: TRackRecArray);
        class procedure WriteRecToDataset(aDataset: TDataProvider; const aRec: TRackRec; aAppend: boolean);
        class function InstNameExists(const aName: string): boolean;
        class function InstReadAllNames(): TStringArray;

        class function NameFieldLength: integer;
        class function MakeDefaultRackRec(): TRackRec;
    end;


implementation


uses
    Graphics,
    SysUtils;

{ TRackDataAdaptor }

constructor TRackDataAdaptor.Create();
begin
    inherited Create(STR_RACKS_TBL);
end;

function TRackDataAdaptor.GetNameField(): string;
begin
    result := STR_RACKS_FLD_NAME;
end;

class function TRackDataAdaptor.InstNameExists(const aName: string): boolean;
var
    xDA: TRackDataAdaptor;
begin
    xDA := TRackDataAdaptor.Create;
    try
        result := xDA.NameExists(aName);
    finally
        FreeAndNil(xDA);
    end;
end;

class function TRackDataAdaptor.InstReadAllNames: TStringArray;
var
    xDA: TRackDataAdaptor;
begin
    xDA := TRackDataAdaptor.Create;
    try
        result := xDA.ReadAllNames();
    finally
        FreeAndNil(xDA);
    end;
end;

procedure TRackDataAdaptor.DeleteRack(const aRack: string);
begin
    self.ExecSQL(Format(STR_SQL_DELETE_FMT, [aRack]));
end;

procedure TRackDataAdaptor.SelectAndOpenRack(const aRack: string; aReadOnly: boolean);
begin
    ASSERT(aRack <> '', 'Rack name is empty');
    SelectAndOpen(Format(STR_SQL_SELECT_RACKS_FMT, [aRack]), aReadOnly);
end;

procedure TRackDataAdaptor.WriteRecs(const aRecs: TRackRecArray);
begin
    self.SelectAndOpenAll(false);
    try
        WriteRecsToDataset(self.DataProvider, aRecs);
    finally
        Close();
    end;
end;

procedure TRackDataAdaptor.WriteRack(const aRec: TRackRec);
var
    xAppend: boolean;
begin
    self.SelectAndOpenRack(aRec.Name, false);
    try
        xAppend := self.DataProvider.IsEmpty;
        self.WriteRecToDataset(self.DataProvider, aRec, xAppend);
    finally
        self.Close();
    end;
end;

function TRackDataAdaptor.ReadRack(const aRack: string; var vRec: TRackRec): boolean;
begin
    SelectAndOpenRack(aRack, true);
    try
        result := not self.DataProvider.IsEmpty();
        if not result then
            EXIT;
        ReadRecFromDataset(self.DataProvider, vRec);
    finally
        Close();
    end;
end;

class procedure TRackDataAdaptor.ReadRecFromDataset(aDataset: TDataProvider; var vRec: TRackRec);
begin
    ASSERT(aDataSet.Active, 'Dataset not active');
    with vRec do
    begin
        name := aDataset.FieldByName(STR_RACKS_FLD_NAME).AsString;
        TYP := aDataset.FieldByName(STR_RACKS_FLD_TYP).AsInteger;
        X_mm := aDataset.FieldByName(STR_RACKS_FLD_X_MM).AsFloat;
        Y_mm := aDataset.FieldByName(STR_RACKS_FLD_Y_MM).AsFloat;
        Z_mm := aDataset.FieldByName(STR_RACKS_FLD_Z_MM).AsFloat;
        Rows := aDataset.FieldByName(STR_RACKS_FLD_ROWS).AsInteger;
        Cols := aDataset.FieldByName(STR_RACKS_FLD_COLS).AsInteger;
        TubeTyp := aDataset.FieldByName(STR_RACKS_FLD_TUBETYP).AsInteger;
        TubeX_mm := aDataset.FieldByName(STR_RACKS_FLD_TUBEX_MM).AsFloat;
        TubeY_mm := aDataset.FieldByName(STR_RACKS_FLD_TUBEY_MM).AsFloat;
        TubeZ_mm := aDataset.FieldByName(STR_RACKS_FLD_TUBEZ_MM).AsFloat;
        BorderX_mm := aDataset.FieldByName(STR_RACKS_FLD_BORDERX_MM).AsFloat;
        BorderY_mm := aDataset.FieldByName(STR_RACKS_FLD_BORDERY_MM).AsFloat;
        PosX_First_mm := aDataset.FieldByName(STR_RACKS_FLD_POSX_FIRST_MM).AsFloat;
        PosY_First_mm := aDataset.FieldByName(STR_RACKS_FLD_POSY_FIRST_MM).AsFloat;
        PosX_Last_mm := aDataset.FieldByName(STR_RACKS_FLD_POSX_LAST_MM).AsFloat;
        PosY_Last_mm := aDataset.FieldByName(STR_RACKS_FLD_POSY_LAST_MM).AsFloat;
        PosX_Offset_mm := aDataset.FieldByName(STR_RACKS_FLD_POSX_OFFSET_MM).AsFloat;
        PosY_Offset_mm := aDataset.FieldByName(STR_RACKS_FLD_POSY_OFFSET_MM).AsFloat;
        ZTravel_mm := aDataset.FieldByName(STR_RACKS_FLD_ZTRAVEL_MM).AsFloat;
        ZScan_mm := aDataset.FieldByName(STR_RACKS_FLD_ZSCAN_MM).AsFloat;
        ZDisp_mm := aDataset.FieldByName(STR_RACKS_FLD_ZDISP_MM).AsFloat;
        ZMax_mm := aDataset.FieldByName(STR_RACKS_FLD_ZMAX_MM).AsFloat;
        H_XTake_mm := aDataset.FieldByName(STR_RACKS_FLD_H_XTAKE_MM).AsFloat;
        H_YTake_mm := aDataset.FieldByName(STR_RACKS_FLD_H_YTAKE_MM).AsFloat;
        H_ZTake_mm := aDataset.FieldByName(STR_RACKS_FLD_H_ZTAKE_MM).AsFloat;
        H_VOpen_mm := aDataset.FieldByName(STR_RACKS_FLD_H_VOPEN_MM).AsFloat;
        H_VClose_mm := aDataset.FieldByName(STR_RACKS_FLD_H_VCLOSE_MM).AsFloat;
        H_VIsUndefined := aDataset.FieldbyName(STR_RACKS_FLD_H_VOPEN_MM).IsNull or
            aDataset.FieldbyName(STR_RACKS_FLD_H_VCLOSE_MM).IsNull;
        AddOffsetX_AfterPos := aDataset.FieldByName(STR_RACKS_FLD_ADDOFFSETX_AFTERPOS).AsInteger;
        AddOffsetX_mm := aDataset.FieldByName(STR_RACKS_FLD_ADDOFFSETX_MM).AsFloat;
        AddOffsetY_AfterPos := aDataset.FieldByName(STR_RACKS_FLD_ADDOFFSETY_AFTERPOS).AsInteger;
        AddOffsetY_mm := aDataset.FieldByName(STR_RACKS_FLD_ADDOFFSETY_MM).AsFloat;
        MOffsetZ_mm := aDataset.FieldByName(STR_RACKS_FLD_MOFFSETZ_MM).AsFloat;
        MOffsetUpX_mm := aDataset.FieldByName(STR_RACKS_FLD_MOFFSETUPX_MM).AsFloat;
        MOffsetUpY_mm := aDataset.FieldByName(STR_RACKS_FLD_MOFFSETUPY_MM).AsFloat;
        Shift_Radius_mm := aDataset.FieldByName(STR_RACKS_FLD_SHIFT_RADIUS_MM).AsFloat;
        Shift_NoOfSteps := aDataset.FieldByName(STR_RACKS_FLD_SHIFT_NOOFSTEPS).AsInteger;
        TubeY2_mm := aDataset.FieldByName(STR_RACKS_FLD_TUBEY2_MM).AsFloat;
        ZPosX_First_mm := aDataset.FieldByName(STR_RACKS_FLD_ZPOSX_FIRST_MM).AsFloat;
        ZPosX_Last_mm := aDataset.FieldByName(STR_RACKS_FLD_ZPOSX_LAST_MM).AsFloat;
        ZPosY_First_mm := aDataset.FieldByName(STR_RACKS_FLD_ZPOSY_FIRST_MM).AsFloat;
        ZPosY_Last_mm := aDataset.FieldByName(STR_RACKS_FLD_ZPOSY_LAST_MM).AsFloat;
        CapType := aDataset.FieldByName(STR_RACKS_FLD_CAPTYPE).AsString;
        CapDiameter := aDataset.FieldByName(STR_RACKS_FLD_CAPDIAMETER).AsFloat;
        Cap_GripZ_mm := aDataset.FieldByName(STR_RACKS_FLD_CAP_GRIPZ_MM).AsFloat;
        Color := aDataset.FieldByName(STR_RACKS_FLD_COLOR).AsInteger;
        Z_LastPos_mm := aDataset.FieldByName(STR_RACKS_FLD_Z_LASTPOS_MM).AsFloat;
        SlopeType := aDataset.FieldByName(STR_RACKS_FLD_SLOPETYPE).AsInteger;
        H_Direction := aDataset.FieldByName(STR_RACKS_FLD_H_DIRECTION).AsInteger;
        TubeGetOpen_mm := aDataset.FieldByName(STR_RACKS_FLD_TUBEGETOPEN_MM).AsFloat;
        TubeGetClose_mm := aDataset.FieldByName(STR_RACKS_FLD_TUBEGETCLOSE_MM).AsFloat;
        TubePutOpen_mm := aDataset.FieldByName(STR_RACKS_FLD_TUBEPUTOPEN_MM).AsFloat;
        TubePutOffset_mm := aDataset.FieldByName(STR_RACKS_FLD_TUBEPUTOFFSET_MM).AsFloat;
        TubeGripOffset_mm := aDataset.FieldByName(STR_RACKS_FLD_TUBEGRIPOFFSET_MM).AsFloat;
        H_RTake_Degree := aDataset.FieldByName(STR_RACKS_FLD_H_RTAKE_DEGREE).AsFloat;
        BlockBalanceDoor := aDataset.FieldByName(STR_RACKS_FLD_BLOCKBALANCEDOOR).AsBoolean;
        StackHeight_mm := aDataset.FieldByName(STR_RACKS_FLD_STACKHEIGHT_MM).AsFloat;
        WellsAroundCenter := aDataset.FieldByName(STR_RACKS_FLD_WellsAroundCenter).AsBoolean;
        CenterX_mm := aDataset.FieldByName(STR_RACKS_FLD_CenterX_mm).AsFloat;
        CenterY_mm := aDataset.FieldByName(STR_RACKS_FLD_CenterY_mm).AsFloat;
        DoNotPaintTubes := aDataset.FieldByName(STR_RACKS_FLD_DoNotPaintTubes).AsBoolean;
        DiscretePositions := aDataset.FieldByName(STR_RACKS_FLD_DiscretePositions).AsBoolean;
        RackMoveZTravelOffset := aDataset.FieldByName(STR_RACKS_FLD_RackMoveZTravelOffset).AsFloat;
    end;
end;

class procedure TRackDataAdaptor.ReadRecsFromDataset(aDataset: TDataProvider; var vRecs: TRackRecArray);
var
    i: integer;
begin
    i := 0;
    SetLength(vRecs, aDataset.RecordCount);
    while not aDataset.Eof do
    begin
        ReadRecFromDataset(aDataset, vRecs[i]);
        aDataset.Next;
        Inc(i);
    end;
end;

class procedure TRackDataAdaptor.WriteRecToDataset(aDataset: TDataProvider; const aRec: TRackRec;
    aAppend: boolean);
begin
    ASSERT(aDataSet.Active, 'Dataset not active');
    if aAppend then
        aDataset.Append
    else
        aDataset.Edit;

    with aRec do
    begin
        aDataset.FieldByName(STR_RACKS_FLD_NAME).AsString := name;
        aDataset.FieldByName(STR_RACKS_FLD_TYP).AsInteger := TYP;
        aDataset.FieldByName(STR_RACKS_FLD_X_MM).AsFloat := X_mm;
        aDataset.FieldByName(STR_RACKS_FLD_Y_MM).AsFloat := Y_mm;
        aDataset.FieldByName(STR_RACKS_FLD_Z_MM).AsFloat := Z_mm;
        aDataset.FieldByName(STR_RACKS_FLD_ROWS).AsInteger := Rows;
        aDataset.FieldByName(STR_RACKS_FLD_COLS).AsInteger := Cols;
        aDataset.FieldByName(STR_RACKS_FLD_TUBETYP).AsInteger := TubeTyp;
        aDataset.FieldByName(STR_RACKS_FLD_TUBEX_MM).AsFloat := TubeX_mm;
        aDataset.FieldByName(STR_RACKS_FLD_TUBEY_MM).AsFloat := TubeY_mm;
        aDataset.FieldByName(STR_RACKS_FLD_TUBEZ_MM).AsFloat := TubeZ_mm;
        aDataset.FieldByName(STR_RACKS_FLD_BORDERX_MM).AsFloat := BorderX_mm;
        aDataset.FieldByName(STR_RACKS_FLD_BORDERY_MM).AsFloat := BorderY_mm;
        aDataset.FieldByName(STR_RACKS_FLD_POSX_FIRST_MM).AsFloat := PosX_First_mm;
        aDataset.FieldByName(STR_RACKS_FLD_POSY_FIRST_MM).AsFloat := PosY_First_mm;
        aDataset.FieldByName(STR_RACKS_FLD_POSX_LAST_MM).AsFloat := PosX_Last_mm;
        aDataset.FieldByName(STR_RACKS_FLD_POSY_LAST_MM).AsFloat := PosY_Last_mm;
        aDataset.FieldByName(STR_RACKS_FLD_POSX_OFFSET_MM).AsFloat := PosX_Offset_mm;
        aDataset.FieldByName(STR_RACKS_FLD_POSY_OFFSET_MM).AsFloat := PosY_Offset_mm;
        aDataset.FieldByName(STR_RACKS_FLD_ZTRAVEL_MM).AsFloat := ZTravel_mm;
        aDataset.FieldByName(STR_RACKS_FLD_ZSCAN_MM).AsFloat := ZScan_mm;
        aDataset.FieldByName(STR_RACKS_FLD_ZDISP_MM).AsFloat := ZDisp_mm;
        aDataset.FieldByName(STR_RACKS_FLD_ZMAX_MM).AsFloat := ZMax_mm;
        aDataset.FieldByName(STR_RACKS_FLD_H_XTAKE_MM).AsFloat := H_XTake_mm;
        aDataset.FieldByName(STR_RACKS_FLD_H_YTAKE_MM).AsFloat := H_YTake_mm;
        aDataset.FieldByName(STR_RACKS_FLD_H_ZTAKE_MM).AsFloat := H_ZTake_mm;
        aDataset.FieldByName(STR_RACKS_FLD_H_VOPEN_MM).AsFloat := H_VOpen_mm;
        aDataset.FieldByName(STR_RACKS_FLD_H_VCLOSE_MM).AsFloat := H_VClose_mm;
        aDataset.FieldByName(STR_RACKS_FLD_ADDOFFSETX_AFTERPOS).AsInteger := AddOffsetX_AfterPos;
        aDataset.FieldByName(STR_RACKS_FLD_ADDOFFSETX_MM).AsFloat := AddOffsetX_mm;
        aDataset.FieldByName(STR_RACKS_FLD_ADDOFFSETY_AFTERPOS).AsInteger := AddOffsetY_AfterPos;
        aDataset.FieldByName(STR_RACKS_FLD_ADDOFFSETY_MM).AsFloat := AddOffsetY_mm;
        aDataset.FieldByName(STR_RACKS_FLD_MOFFSETZ_MM).AsFloat := MOffsetZ_mm;
        aDataset.FieldByName(STR_RACKS_FLD_MOFFSETUPX_MM).AsFloat := MOffsetUpX_mm;
        aDataset.FieldByName(STR_RACKS_FLD_MOFFSETUPY_MM).AsFloat := MOffsetUpY_mm;
        aDataset.FieldByName(STR_RACKS_FLD_SHIFT_RADIUS_MM).AsFloat := Shift_Radius_mm;
        aDataset.FieldByName(STR_RACKS_FLD_SHIFT_NOOFSTEPS).AsInteger := Shift_NoOfSteps;
        aDataset.FieldByName(STR_RACKS_FLD_TUBEY2_MM).AsFloat := TubeY2_mm;
        aDataset.FieldByName(STR_RACKS_FLD_ZPOSX_FIRST_MM).AsFloat := ZPosX_First_mm;
        aDataset.FieldByName(STR_RACKS_FLD_ZPOSX_LAST_MM).AsFloat := ZPosX_Last_mm;
        aDataset.FieldByName(STR_RACKS_FLD_ZPOSY_FIRST_MM).AsFloat := ZPosY_First_mm;
        aDataset.FieldByName(STR_RACKS_FLD_ZPOSY_LAST_MM).AsFloat := ZPosY_Last_mm;
        aDataset.FieldByName(STR_RACKS_FLD_CAPTYPE).AsString := CapType;
        aDataset.FieldByName(STR_RACKS_FLD_CAPDIAMETER).AsFloat := CapDiameter;
        aDataset.FieldByName(STR_RACKS_FLD_CAP_GRIPZ_MM).AsFloat := Cap_GripZ_mm;
        aDataset.FieldByName(STR_RACKS_FLD_COLOR).AsInteger := Color;
        aDataset.FieldByName(STR_RACKS_FLD_Z_LASTPOS_MM).AsFloat := Z_LastPos_mm;
        aDataset.FieldByName(STR_RACKS_FLD_SLOPETYPE).AsInteger := SlopeType;
        aDataset.FieldByName(STR_RACKS_FLD_H_DIRECTION).AsInteger := H_Direction;
        aDataset.FieldByName(STR_RACKS_FLD_TUBEGETOPEN_MM).AsFloat := TubeGetOpen_mm;
        aDataset.FieldByName(STR_RACKS_FLD_TUBEGETCLOSE_MM).AsFloat := TubeGetClose_mm;
        aDataset.FieldByName(STR_RACKS_FLD_TUBEPUTOPEN_MM).AsFloat := TubePutOpen_mm;
        aDataset.FieldByName(STR_RACKS_FLD_TUBEPUTOFFSET_MM).AsFloat := TubePutOffset_mm;
        aDataset.FieldByName(STR_RACKS_FLD_TUBEGRIPOFFSET_MM).AsFloat := TubeGripOffset_mm;
        aDataset.FieldByName(STR_RACKS_FLD_H_RTAKE_DEGREE).AsFloat := H_RTake_Degree;
        aDataset.FieldByName(STR_RACKS_FLD_BLOCKBALANCEDOOR).AsBoolean := BlockBalanceDoor;
        aDataset.FieldByName(STR_RACKS_FLD_STACKHEIGHT_MM).AsFloat := StackHeight_mm;
        aDataset.FieldByName(STR_RACKS_FLD_WellsAroundCenter).AsBoolean := WellsAroundCenter;
        aDataset.FieldByName(STR_RACKS_FLD_CenterX_mm).AsFloat := CenterX_mm;
        aDataset.FieldByName(STR_RACKS_FLD_CenterY_mm).AsFloat := CenterY_mm;
        aDataset.FieldByName(STR_RACKS_FLD_DoNotPaintTubes).AsBoolean := DoNotPaintTubes;
        aDataset.FieldByName(STR_RACKS_FLD_DiscretePositions).AsBoolean := DiscretePositions;
        aDataset.FieldByName(STR_RACKS_FLD_RackMoveZTravelOffset).AsFloat := RackMoveZTravelOffset;
    end;
    aDataset.Post;
end;

class procedure TRackDataAdaptor.WriteRecsToDataset(aDataset: TDataProvider; const aRecs: TRackRecArray);
var
    i: integer;
begin
    for i := 0 to high(aRecs) do
    begin
        WriteRecToDataset(aDataset, aRecs[i], true);
    end;
end;

procedure TRackDataAdaptor.ReadRecs(var vRecs: TRackRecArray);
begin
    self.SelectAndOpenAll(true);
    try
        ReadRecsFromDataset(self.DataProvider, vRecs);
    finally
        self.Close();
    end;
end;

class function TRackDataAdaptor.NameFieldLength: integer;
begin
    result := INT_RACKS_FLDLEN_NAME;
end;

class function TRackDataAdaptor.MakeDefaultRackRec(): TRackRec;
begin
    result.name := '';
    result.Typ := 0;
    result.X_mm := 0;
    result.Y_mm := 0;
    result.Z_mm := 0;
    result.Rows := 0;
    result.Cols := 0;

    result.ZTravel_mm := 0;
    result.ZScan_mm := 0;
    result.ZDisp_mm := 0;
    result.ZMax_mm := 0;

    result.TubeTyp := 1;

    result.TubeX_mm := 0;
    result.TubeY_mm := 0;
    result.TubeZ_mm := 0;
    result.TubeY2_mm := 0;

    result.PosX_First_mm := 0;
    result.PosY_First_mm := 0;
    result.PosX_Last_mm := 0;
    result.PosY_Last_mm := 0;
    result.PosX_Offset_mm := 0;
    result.PosY_Offset_mm := 0;

    result.Color := clBtnFace;

    result.H_XTake_mm := 0;
    result.H_YTake_mm := 0;
    result.H_ZTake_mm := 0;
    result.H_RTake_Degree := 0;
    result.H_VOpen_mm := 0;
    result.H_VClose_mm := 0;

    result.AddOffsetX_AfterPos := 0;
    result.AddOffsetX_mm := 0;
    result.AddOffsetY_AfterPos := 0;
    result.AddOffsetY_mm := 0;
    result.MOffsetZ_mm := 0;
    result.MOffsetUpX_mm := 0;
    result.MOffsetUpY_mm := 0;
    result.Shift_Radius_mm := 0;
    result.Shift_NoOfSteps := 0;

    result.CapType := '';
    result.CapDiameter := 0;
    result.Cap_GripZ_mm := 0;

    result.Z_LastPos_mm := 0;
    result.SlopeType := 0;

    result.TubeGetOpen_mm := 0;
    result.TubeGetClose_mm := 0;
    result.TubePutOpen_mm := 0;
    result.TubePutOffset_mm := 0;
    result.TubeGripOffset_mm := 0;

    result.BlockBalanceDoor := false;
    result.StackHeight_mm := 0;
    result.WellsAroundCenter := false;
    result.CenterX_mm := 0;
    result.CenterY_mm := 0;
    result.DoNotPaintTubes := false;
    result.DiscretePositions := false;
    result.RackMoveZTravelOffset := 0;
end;


end.
