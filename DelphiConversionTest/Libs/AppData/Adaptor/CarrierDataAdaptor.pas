{ --------------------------------------------------------------------------------------------------
  Copyright © 2006 Zinsser Analytic GmbH - All rights reserved.
  Author       : Wolfgang Lyncke (wl)
  Description  : Data Adaptor for the CARRIER.DB
  --------------------------------------------------------------------------------------------------
  Revision History:
  date     op  method                       track-no improvement/change
  -------- --  ---------------------------  -------- -----------------------------------------------
  05.12.06 wl                                TN3532   Initial Revision
  31.01.07 wl  InstallTable                  TN3532   mit den neuen Feldern H_RRETAKE_DEGREE und HTUBE_RSTART_DEGREE
  05.02.07 pk                                TN3544   Changes for updatemanager
  14.06.07 wl                                TN3728   neues Feld HTUBE_KEEPROTATION
  02.10.07 wl                                TN3811.5 benutzt self.Query statt fQuery
  09.11.07 pk                                TN3921   Field names moved
  07.01.08 pk  GetKeyFields                  TN3922   New
  20.06.08 pk  SelectAndOpenAll              TN4139   New
  27.06.08 pk  ReadCarrier, WriteCarrier     TN4139   New
  23.09.08 wl                                TN4236   Vereinigung mit CarrierFieldnames
  16.01.09 wl                                TN4362   an Änderungen in TQueryDataAdaptor angepasst
  09.07.09 pk                                TN4585.4 DataProvider.Locate removed, functionality replaced by SelectAndOpen
  30.04.10 wl  MakeInvisbleCarrierRec        TN5070   Invisible Carrier 0000000~NoCarrier wird erzeugt
  30.04.10 wl  NameExists                    TN5070   Carrier-Name 0000000~NoCarrier ist reserviert
  30.04.10 wl  ReadRecsFromDataset           TN5070   Invisible Carrier 0000000~NoCarrier wird hinzugefügt
  17.06.10 pk                                TN5152.1  Change SQLs so that they also work for TurboDB (dbl quoatations to single)
  23.07.10 wl  TCarrierRec                   TN5205   Size-Werte heißen jetzt auch SizeX,SizeY,SizeZ
  04.08.10 pk                                TN5218   Changes for Plugin database packages
  08.02.11 wl  Instance                      TN5475   entfernt
  13.11.12 wl  GetKeyFields                  TN6015   kann entfernt werden, wenn Key = NameField
  16.04.13 pp                                TN6131   new: DiscretePositions
  -------------------------------------------------------------------------------------------------- }

unit CarrierDataAdaptor;


interface


uses
    Classes,
    DataProvider,
    QueryDataAdaptor,
    CommonTypes,
    GeneralTypes,
    DiscreteCarrierDataAdaptor;

type
    TCarrierRec = record
        name: string;
        CarrierType: integer;
        SizeX_mm: double;
        SizeY_mm: double;
        SizeZ_mm: double;
        Rows: integer;
        Cols: integer;
        Floors: integer;
        SlotX_mm: double;
        SlotY_mm: double;

        SlotX_First_mm: double;
        SlotY_First_mm: double;
        SlotZ_ReallyLast_mm: double; // bisher: SlotZ_First_mm
        SlotX_Last_mm: double;
        SlotY_Last_mm: double;
        SlotZ_ReallyFirst_mm: double; // bisher: SlotZ_Last_mm
        SlotZ_Calculate: boolean;
        SlotRotation_Degree: double;
        Stackerbuttons: integer;

        H_XStart_mm: double;
        H_YStart_mm: double;
        H_ZStart_mm: double;
        H_RStart_degree: double;
        H_XReTake_mm: double;
        H_YReTake_mm: double;
        H_ZReTake_mm: double;
        H_RReTake_degree: double;
        H_ZPut_mm: double;
        H_XPreStart_mm: double;
        H_DirectionTurnType: integer;

        HTube_UseValues: boolean;
        HTube_XStart_mm: double;
        HTube_YStart_mm: double;
        HTube_ZStart_mm: double;
        HTube_RStart_degree: double;
        HTube_YStartBeforeX: boolean;
        HTube_KeepRotation: boolean;
        DiscretePosRecArray: TDiscretePosRecArray;
        DiscretePositions: boolean;
    end;

    TCarrierRecArray = array of TCarrierRec;

    TCarrierDataAdaptor = class(TQueryDataAdaptor)
    private const
        STR_CARRIER_TBL = 'CARRIER';
        STR_CARRIER_FLD_NAME = 'NAME';
        STR_CARRIER_FLD_TYP = 'TYP';
        STR_CARRIER_FLD_X_MM = 'X_MM';
        STR_CARRIER_FLD_Y_MM = 'Y_MM';
        STR_CARRIER_FLD_Z_MM = 'Z_MM';
        STR_CARRIER_FLD_ROWS = 'ROWS';
        STR_CARRIER_FLD_COLS = 'COLS';
        STR_CARRIER_FLD_SLOTS = 'SLOTS';
        STR_CARRIER_FLD_SLOTX_MM = 'SLOTX_MM';
        STR_CARRIER_FLD_SLOTY_MM = 'SLOTY_MM';
        STR_CARRIER_FLD_SLOTX_FIRST_MM = 'SLOTX_FIRST_MM';
        STR_CARRIER_FLD_SLOTY_FIRST_MM = 'SLOTY_FIRST_MM';
        STR_CARRIER_FLD_SLOTZ_FIRST_MM = 'SLOTZ_FIRST_MM';
        STR_CARRIER_FLD_SLOTX_LAST_MM = 'SLOTX_LAST_MM';
        STR_CARRIER_FLD_SLOTY_LAST_MM = 'SLOTY_LAST_MM';
        STR_CARRIER_FLD_SLOTZ_LAST_MM = 'SLOTZ_LAST_MM';
        STR_CARRIER_FLD_SLOTZ_CALCULATE = 'SLOTZ_CALCULATE';
        STR_CARRIER_FLD_SLOTROTATION_DEGREE = 'SLOTROTATION_DEGREE';
        STR_CARRIER_FLD_STACKERBUTTONS = 'STACKERBUTTONS';
        STR_CARRIER_FLD_H_XSTART_MM = 'H_XSTART_MM';
        STR_CARRIER_FLD_H_YSTART_MM = 'H_YSTART_MM';
        STR_CARRIER_FLD_H_ZSTART_MM = 'H_ZSTART_MM';
        STR_CARRIER_FLD_H_RSTART_DEGREE = 'H_RSTART_DEGREE';
        STR_CARRIER_FLD_H_XRETAKE_MM = 'H_XRETAKE_MM';
        STR_CARRIER_FLD_H_YRETAKE_MM = 'H_YRETAKE_MM';
        STR_CARRIER_FLD_H_ZRETAKE_MM = 'H_ZRETAKE_MM';
        STR_CARRIER_FLD_H_RRETAKE_DEGREE = 'H_RRETAKE_DEGREE';
        STR_CARRIER_FLD_H_ZPUT_MM = 'H_ZPUT_MM';
        STR_CARRIER_FLD_H_XPRESTART_MM = 'H_XPRESTART_MM';
        STR_CARRIER_FLD_H_DIRECTIONTURNTYPE = 'H_DIRECTIONTURNTYPE';
        STR_CARRIER_FLD_HTUBE_USEVALUES = 'HTUBE_USEVALUES';
        STR_CARRIER_FLD_HTUBE_XSTART_MM = 'HTUBE_XSTART_MM';
        STR_CARRIER_FLD_HTUBE_YSTART_MM = 'HTUBE_YSTART_MM';
        STR_CARRIER_FLD_HTUBE_ZSTART_MM = 'HTUBE_ZSTART_MM';
        STR_CARRIER_FLD_HTUBE_RSTART_DEGREE = 'HTUBE_RSTART_DEGREE';
        STR_CARRIER_FLD_HTUBE_YSTARTBEFOREX = 'HTUBE_YSTARTBEFOREX';
        STR_CARRIER_FLD_HTUBE_KEEPROTATION = 'HTUBE_KEEPROTATION';
        STR_CARRIERS_FLD_DISCRETEPOSITIONS = 'DISCRETEPOSITIONS';
        INT_CARRIER_FLDLEN_NAME = 20;
        STR_CARRIER_INDEX_FIELDS = STR_CARRIER_FLD_NAME;
        STR_SQL_WHERE_CARRIER = ' WHERE ' + STR_CARRIER_FLD_NAME + ' =''%s''';
        STR_SQL_FROM = ' FROM ' + STR_CARRIER_TBL;
        STR_SQL_SELECT_ALL = 'SELECT * ' + STR_SQL_FROM;
        STR_SQL_SELECT_CARRIERS_FMT = STR_SQL_SELECT_ALL + STR_SQL_WHERE_CARRIER;
        cDefaultInvisbleCarrierName = '0000000~NoCarrier';
    strict private
        class function GetInvisibleCarrierName: string; static;
        class procedure ReadRecFromDataset(aDataset: TDataProvider; out oCarrierRec: TCarrierRec);
        class procedure ReadRecsFromDataset(aDataset: TDataProvider; out oRecs: TCarrierRecArray);
        class procedure WriteRecToDataset(aDataset: TDataProvider; const aRec: TCarrierRec; aAppend: boolean);
    strict protected
        function GetNameField: string; override;
    public
        constructor Create();

        procedure ReadRecs(var vRecs: TCarrierRecArray);
        procedure SelectAndOpenCarrier(const aCarrier: string; aReadOnly: boolean);
        function NameExists(const aName: string): boolean; override;

        class function ReadCarrier(const aCarrierTypeName: string; var vRec: TCarrierRec): boolean;
        procedure WriteCarrier(const aRec: TCarrierRec);

        class function NameFieldLength: integer;
        class function MakeDefaultCarrierRec(): TCarrierRec;
        class function MakeFlatCarrierRec(const aName: string; aSizeX, aSizeY: TPosMM): TCarrierRec;
        class function MakeInvisbleCarrierRec(): TCarrierRec;
        class function InstReadAllNames(): TStringArray;
        class property InvisibleCarrierName: string read GetInvisibleCarrierName;
    end;


implementation


uses
    Variants,
    SysUtils,
    MathUtils;

{ TCarrierDataAdaptor }

class function TCarrierDataAdaptor.MakeDefaultCarrierRec: TCarrierRec;
begin
    result.Name := '';
    result.CarrierType := 0;
    result.SizeX_mm := 0;
    result.SizeY_mm := 0;
    result.SizeZ_mm := 0;
    result.Rows := 0;
    result.Cols := 0;
    result.Floors := 1;
    result.SlotX_mm := 0;
    result.SlotY_mm := 0;

    result.SlotX_First_mm := 0;
    result.SlotY_First_mm := 0;
    result.SlotZ_ReallyFirst_mm := 0;
    result.SlotX_Last_mm := 0;
    result.SlotY_Last_mm := 0;
    result.SlotZ_ReallyLast_mm := 0;
    result.SlotZ_Calculate := false;

    result.SlotRotation_Degree := 0;
    result.Stackerbuttons := 0;

    result.H_XStart_mm := 0;
    result.H_YStart_mm := 0;
    result.H_ZStart_mm := 0;
    result.H_RStart_degree := 0;
    result.H_XRetake_mm := 0;
    result.H_YRetake_mm := 0;
    result.H_ZRetake_mm := 0;
    result.H_RRetake_degree := 0;
    result.H_ZPut_mm := 0;
    result.H_XPreStart_mm := 0;
    result.H_DirectionTurnType := 0;

    result.HTube_UseValues := false;
    result.HTube_XStart_mm := 0;
    result.HTube_YStart_mm := 0;
    result.HTube_ZStart_mm := 0;
    result.HTube_RStart_degree := 0;
    result.HTube_YStartBeforeX := false;
    result.HTube_KeepRotation := false;
    result.DiscretePositions := false;
end;

class function TCarrierDataAdaptor.MakeFlatCarrierRec(const aName: string; aSizeX, aSizeY: TPosMM)
    : TCarrierRec;
begin
    result := TCarrierDataAdaptor.MakeDefaultCarrierRec();

    result.Name := aName;
    result.SizeX_mm := aSizeX;
    result.SizeY_mm := aSizeY;
    result.Rows := 1;
    result.Cols := 1;
    result.SlotX_mm := aSizeX;
    result.SlotY_mm := aSizeY;
end;

class function TCarrierDataAdaptor.MakeInvisbleCarrierRec: TCarrierRec;
begin
    result := TCarrierDataAdaptor.MakeFlatCarrierRec(cDefaultInvisbleCarrierName, 1, 1);
end;

constructor TCarrierDataAdaptor.Create();
begin
    inherited Create(STR_CARRIER_TBL);
end;

procedure TCarrierDataAdaptor.SelectAndOpenCarrier(const aCarrier: string; aReadOnly: boolean);
begin
    ASSERT(aCarrier <> '', 'Carrier name is empty');
    SelectAndOpen(Format(STR_SQL_SELECT_CARRIERS_FMT, [aCarrier]), aReadOnly);
end;

function TCarrierDataAdaptor.GetNameField(): string;
begin
    result := STR_CARRIER_FLD_NAME;
end;

class function TCarrierDataAdaptor.InstReadAllNames: TStringArray;
var
    xDA: TCarrierDataAdaptor;
begin
    xDA := TCarrierDataAdaptor.Create;
    try
        result := xDA.ReadAllNames();
    finally
        FreeAndNil(xDA);
    end;
end;

class function TCarrierDataAdaptor.GetInvisibleCarrierName: string;
begin
    EXIT(cDefaultInvisbleCarrierName);
end;

class function TCarrierDataAdaptor.ReadCarrier(const aCarrierTypeName: string; var vRec: TCarrierRec)
    : boolean;
var
    xDA: TCarrierDataAdaptor;
begin
    // Sonderfall Invisible Carrier
    if TCarrierDataAdaptor.InvisibleCarrierName = aCarrierTypeName then
    begin
        vRec := TCarrierDataAdaptor.MakeInvisbleCarrierRec();
        result := true;
        EXIT;
    end;

    xDA := TCarrierDataAdaptor.Create;
    try
        xDA.SelectAndOpen(Format(STR_SQL_SELECT_ALL + ' WHERE %s = ''%s''',
            [STR_CARRIER_FLD_NAME, aCarrierTypeName]), true);
        try
            if (xDA.DataProvider.Eof) then
            begin
                result := false;
                EXIT;
            end;
            result := true;

            xDA.ReadRecFromDataset(xDA.DataProvider, vRec);
        finally
            xDA.Close();
        end;
    finally
        FreeAndNil(xDA);
    end;
end;

procedure TCarrierDataAdaptor.WriteCarrier(const aRec: TCarrierRec);
var
    xAppend: boolean;
begin
    self.SelectAndOpenCarrier(aRec.Name, false);
    try
        xAppend := self.DataProvider.IsEmpty;
        self.WriteRecToDataset(self.DataProvider, aRec, xAppend);
    finally
        self.Close();
    end;
end;

class procedure TCarrierDataAdaptor.ReadRecFromDataset(aDataset: TDataProvider; out oCarrierRec: TCarrierRec);
begin
    oCarrierRec.Name := aDataset.FieldByName(STR_CARRIER_FLD_NAME).AsString;
    oCarrierRec.CarrierType := aDataset.FieldByName(STR_CARRIER_FLD_TYP).AsInteger;
    oCarrierRec.SizeX_mm := aDataset.FieldByName(STR_CARRIER_FLD_X_MM).AsFloat;
    oCarrierRec.SizeY_mm := aDataset.FieldByName(STR_CARRIER_FLD_Y_MM).AsFloat;
    oCarrierRec.SizeZ_mm := aDataset.FieldByName(STR_CARRIER_FLD_Z_MM).AsFloat;
    oCarrierRec.Rows := aDataset.FieldByName(STR_CARRIER_FLD_ROWS).AsInteger;
    oCarrierRec.Cols := aDataset.FieldByName(STR_CARRIER_FLD_COLS).AsInteger;
    oCarrierRec.Floors := aDataset.FieldByName(STR_CARRIER_FLD_SLOTS).AsInteger;
    oCarrierRec.SlotX_mm := aDataset.FieldByName(STR_CARRIER_FLD_SLOTX_MM).AsFloat;
    oCarrierRec.SlotY_mm := aDataset.FieldByName(STR_CARRIER_FLD_SLOTY_MM).AsFloat;

    oCarrierRec.SlotX_First_mm := aDataset.FieldByName(STR_CARRIER_FLD_SLOTX_FIRST_MM).AsFloat;
    oCarrierRec.SlotY_First_mm := aDataset.FieldByName(STR_CARRIER_FLD_SLOTY_FIRST_MM).AsFloat;
    oCarrierRec.SlotZ_ReallyLast_mm := aDataset.FieldByName(STR_CARRIER_FLD_SLOTZ_FIRST_MM).AsFloat;
    oCarrierRec.SlotX_Last_mm := aDataset.FieldByName(STR_CARRIER_FLD_SLOTX_LAST_MM).AsFloat;
    oCarrierRec.SlotY_Last_mm := aDataset.FieldByName(STR_CARRIER_FLD_SLOTY_LAST_MM).AsFloat;
    oCarrierRec.SlotZ_ReallyFirst_mm := aDataset.FieldByName(STR_CARRIER_FLD_SLOTZ_LAST_MM).AsFloat;
    oCarrierRec.SlotZ_Calculate := aDataset.FieldByName(STR_CARRIER_FLD_SLOTZ_CALCULATE).AsBoolean;
    oCarrierRec.SlotRotation_Degree := aDataset.FieldByName(STR_CARRIER_FLD_SLOTROTATION_DEGREE).AsFloat;
    oCarrierRec.Stackerbuttons := aDataset.FieldByName(STR_CARRIER_FLD_STACKERBUTTONS).AsInteger;

    oCarrierRec.H_XStart_mm := aDataset.FieldByName(STR_CARRIER_FLD_H_XSTART_MM).AsFloat;
    oCarrierRec.H_YStart_mm := aDataset.FieldByName(STR_CARRIER_FLD_H_YSTART_MM).AsFloat;
    oCarrierRec.H_ZStart_mm := aDataset.FieldByName(STR_CARRIER_FLD_H_ZSTART_MM).AsFloat;
    oCarrierRec.H_RStart_degree := aDataset.FieldByName(STR_CARRIER_FLD_H_RSTART_DEGREE).AsFloat;
    oCarrierRec.H_XRetake_mm := aDataset.FieldByName(STR_CARRIER_FLD_H_XRETAKE_MM).AsFloat;
    oCarrierRec.H_YRetake_mm := aDataset.FieldByName(STR_CARRIER_FLD_H_YRETAKE_MM).AsFloat;
    oCarrierRec.H_ZRetake_mm := aDataset.FieldByName(STR_CARRIER_FLD_H_ZRETAKE_MM).AsFloat;
    oCarrierRec.H_RRetake_degree := aDataset.FieldByName(STR_CARRIER_FLD_H_RRETAKE_DEGREE).AsFloat;
    oCarrierRec.H_ZPut_mm := aDataset.FieldByName(STR_CARRIER_FLD_H_ZPUT_MM).AsFloat;
    oCarrierRec.H_XPreStart_mm := aDataset.FieldByName(STR_CARRIER_FLD_H_XPRESTART_MM).AsFloat;
    oCarrierRec.H_DirectionTurnType := aDataset.FieldByName(STR_CARRIER_FLD_H_DIRECTIONTURNTYPE).AsInteger;

    oCarrierRec.HTube_UseValues := aDataset.FieldByName(STR_CARRIER_FLD_HTUBE_USEVALUES).AsBoolean;
    oCarrierRec.HTube_XStart_mm := aDataset.FieldByName(STR_CARRIER_FLD_HTUBE_XSTART_MM).AsFloat;
    oCarrierRec.HTube_YStart_mm := aDataset.FieldByName(STR_CARRIER_FLD_HTUBE_YSTART_MM).AsFloat;
    oCarrierRec.HTube_ZStart_mm := aDataset.FieldByName(STR_CARRIER_FLD_HTUBE_ZSTART_MM).AsFloat;
    oCarrierRec.HTube_RStart_degree := aDataset.FieldByName(STR_CARRIER_FLD_HTUBE_RSTART_DEGREE).AsFloat;
    oCarrierRec.HTube_YStartBeforeX := aDataset.FieldByName(STR_CARRIER_FLD_HTUBE_YSTARTBEFOREX).AsBoolean;
    oCarrierRec.HTube_KeepRotation := aDataset.FieldByName(STR_CARRIER_FLD_HTUBE_KEEPROTATION).AsBoolean;
    oCarrierRec.DiscretePositions := aDataset.FieldByName(STR_CARRIERS_FLD_DISCRETEPOSITIONS).AsBoolean;
end;

class procedure TCarrierDataAdaptor.WriteRecToDataset(aDataset: TDataProvider; const aRec: TCarrierRec;
    aAppend: boolean);
begin
    ASSERT(aDataSet.Active, 'Dataset not active');

    if TCarrierDataAdaptor.InvisibleCarrierName = aRec.Name then
        raise Exception.Create('This carrier type name is not allowed');

    if aAppend then
        aDataset.Append
    else
        aDataset.Edit;

    with aRec do
    begin
        aDataset.FieldByName(STR_CARRIER_FLD_NAME).AsString := name;
        aDataset.FieldByName(STR_CARRIER_FLD_TYP).AsInteger := CarrierType;
        aDataset.FieldByName(STR_CARRIER_FLD_X_MM).AsFloat := SizeX_mm;
        aDataset.FieldByName(STR_CARRIER_FLD_Y_MM).AsFloat := SizeY_mm;
        aDataset.FieldByName(STR_CARRIER_FLD_Z_MM).AsFloat := SizeZ_mm;
        aDataset.FieldByName(STR_CARRIER_FLD_ROWS).AsInteger := Rows;
        aDataset.FieldByName(STR_CARRIER_FLD_COLS).AsInteger := Cols;
        aDataset.FieldByName(STR_CARRIER_FLD_SLOTS).AsInteger := Floors;
        aDataset.FieldByName(STR_CARRIER_FLD_SLOTX_MM).AsFloat := SlotX_mm;
        aDataset.FieldByName(STR_CARRIER_FLD_SLOTY_MM).AsFloat := SlotY_mm;

        aDataset.FieldByName(STR_CARRIER_FLD_SLOTX_FIRST_MM).AsFloat := SlotX_First_mm;
        aDataset.FieldByName(STR_CARRIER_FLD_SLOTY_FIRST_MM).AsFloat := SlotY_First_mm;
        aDataset.FieldByName(STR_CARRIER_FLD_SLOTZ_FIRST_MM).AsFloat := SlotZ_ReallyLast_mm;
        aDataset.FieldByName(STR_CARRIER_FLD_SLOTX_LAST_MM).AsFloat := SlotX_Last_mm;
        aDataset.FieldByName(STR_CARRIER_FLD_SLOTY_LAST_MM).AsFloat := SlotY_Last_mm;
        aDataset.FieldByName(STR_CARRIER_FLD_SLOTZ_LAST_MM).AsFloat := SlotZ_ReallyFirst_mm;
        aDataset.FieldByName(STR_CARRIER_FLD_SLOTZ_CALCULATE).AsBoolean := SlotZ_Calculate;
        aDataset.FieldByName(STR_CARRIER_FLD_SLOTROTATION_DEGREE).AsFloat := SlotRotation_Degree;
        aDataset.FieldByName(STR_CARRIER_FLD_STACKERBUTTONS).AsInteger := Stackerbuttons;

        aDataset.FieldByName(STR_CARRIER_FLD_H_XSTART_MM).AsFloat := H_XStart_mm;
        aDataset.FieldByName(STR_CARRIER_FLD_H_YSTART_MM).AsFloat := H_YStart_mm;
        aDataset.FieldByName(STR_CARRIER_FLD_H_ZSTART_MM).AsFloat := H_ZStart_mm;
        aDataset.FieldByName(STR_CARRIER_FLD_H_RSTART_DEGREE).AsFloat := H_RStart_degree;
        aDataset.FieldByName(STR_CARRIER_FLD_H_XRETAKE_MM).AsFloat := H_XRetake_mm;
        aDataset.FieldByName(STR_CARRIER_FLD_H_YRETAKE_MM).AsFloat := H_YRetake_mm;
        aDataset.FieldByName(STR_CARRIER_FLD_H_ZRETAKE_MM).AsFloat := H_ZRetake_mm;
        aDataset.FieldByName(STR_CARRIER_FLD_H_RRETAKE_DEGREE).AsFloat := H_RRetake_degree;
        aDataset.FieldByName(STR_CARRIER_FLD_H_ZPUT_MM).AsFloat := H_ZPut_mm;
        aDataset.FieldByName(STR_CARRIER_FLD_H_XPRESTART_MM).AsFloat := H_XPreStart_mm;
        aDataset.FieldByName(STR_CARRIER_FLD_H_DIRECTIONTURNTYPE).AsInteger := H_DirectionTurnType;

        aDataset.FieldByName(STR_CARRIER_FLD_HTUBE_USEVALUES).AsBoolean := HTube_UseValues;
        aDataset.FieldByName(STR_CARRIER_FLD_HTUBE_XSTART_MM).AsFloat := HTube_XStart_mm;
        aDataset.FieldByName(STR_CARRIER_FLD_HTUBE_YSTART_MM).AsFloat := HTube_YStart_mm;
        aDataset.FieldByName(STR_CARRIER_FLD_HTUBE_ZSTART_MM).AsFloat := HTube_ZStart_mm;
        aDataset.FieldByName(STR_CARRIER_FLD_HTUBE_RSTART_DEGREE).AsFloat := HTube_RStart_degree;
        aDataset.FieldByName(STR_CARRIER_FLD_HTUBE_YSTARTBEFOREX).AsBoolean := HTube_YStartBeforeX;
        aDataset.FieldByName(STR_CARRIER_FLD_HTUBE_KEEPROTATION).AsBoolean := HTube_KeepRotation;
        aDataset.FieldByName(STR_CARRIERS_FLD_DISCRETEPOSITIONS).AsBoolean := DiscretePositions;
    end;

    aDataset.Post;
end;

class procedure TCarrierDataAdaptor.ReadRecsFromDataset(aDataset: TDataProvider; out oRecs: TCarrierRecArray);
var
    x: integer;
begin
    x := 0;
    SetLength(oRecs, aDataset.RecordCount + 1);
    while not aDataset.Eof do
    begin
        ReadRecFromDataset(aDataset, oRecs[x]);
        aDataset.Next;
        Inc(x);
    end;

    // Sonderfall Invisible Carrier
    oRecs[aDataset.RecordCount] := TCarrierDataAdaptor.MakeInvisbleCarrierRec();
end;

procedure TCarrierDataAdaptor.ReadRecs(var vRecs: TCarrierRecArray);
begin
    self.SelectAndOpenAll(true);
    try
        ReadRecsFromDataset(self.DataProvider, vRecs);
    finally
        self.Close();
    end;
end;

function TCarrierDataAdaptor.NameExists(const aName: string): boolean;
begin
    result := inherited NameExists(aName);

    if TCarrierDataAdaptor.InvisibleCarrierName = aName then
        result := true;
end;

class function TCarrierDataAdaptor.NameFieldLength: integer;
begin
    result := INT_CARRIER_FLDLEN_NAME;
end;


end.
