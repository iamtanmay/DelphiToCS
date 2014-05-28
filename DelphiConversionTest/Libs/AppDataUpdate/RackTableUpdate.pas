unit RackTableUpdate;
{ --------------------------------------------------------------------------------------------------
  Copyright © 2008 Zinsser Analytic GmbH - All rights reserved.
  Author       : Payman Kamali (pk)
  Description  :
  --------------------------------------------------------------------------------------------------
  Revision History:
  date     op  method                       track-no improvement/change
  -------- --  ---------------------------  -------- -----------------------------------------------
  24.06.08 pk                               TN4148   FieldNames unit removed
  29.10.08 wl  TRacksTableStructDefV3       TN4290   neues Feld 'TUBEPUTOFFSET_MM'
  29.10.08 wl  TRacksTableUpdateV3          TN4290   Update auf neue Struktur
  24.02.11 wl  TRacksTableUpdateV4          TN5431   3 neue Felder: WellsAroundCenter (boolean), CenterX_mm, CenterY_mm
  05.05.11 ts  TRacksTableUpdateV5          TN5552   new: TubeGripOffset_mm
  05.09.11 ts  TRacksTableUpdateV6          TN5683   new: DoNotPaintTubes
  10.11.11 ts  TRacksTableUpdateV7          TN5702   new: DiscretePositions
  10.09.12 ts  TRacksTableUpdateV8          TN5977   new: RackMoveZTravelOffset
  -------------------------------------------------------------------------------------------------- }


interface


uses
    TableStructDef,
    TableUpdate;

const
    INT_RACKS_MAJORREVISION_1 = 1;
    INT_RACKS_MAJORREVISION_2 = 2;
    INT_RACKS_MAJORREVISION_3 = 3;
    INT_RACKS_MAJORREVISION_4 = 4;
    INT_RACKS_MAJORREVISION_5 = 5;
    INT_RACKS_MAJORREVISION_6 = 6;
    INT_RACKS_MAJORREVISION_7 = 7;
    INT_RACKS_MAJORREVISION_8 = 8;

type
    // table structure definitions
    TRacksTableStructDefV0 = class(TTableStructDef)
    protected
        procedure DoDefineStruct(); override;
    end;

    TRacksTableStructDefV1 = class(TRacksTableStructDefV0)
    protected
        procedure DoDefineStruct(); override;
    end;

    TRacksTableStructDefV2 = class(TRacksTableStructDefV1)
    protected
        procedure DoDefineStruct(); override;
    end;

    TRacksTableStructDefV3 = class(TRacksTableStructDefV2)
    protected
        procedure DoDefineStruct(); override;
    end;

    TRacksTableStructDefV4 = class(TRacksTableStructDefV3)
    protected
        procedure DoDefineStruct(); override;
    end;

    TRacksTableStructDefV5 = class(TRacksTableStructDefV4)
    protected
        procedure DoDefineStruct(); override;
    end;

    TRacksTableStructDefV6 = class(TRacksTableStructDefV5)
    protected
        procedure DoDefineStruct(); override;
    end;

    TRacksTableStructDefV7 = class(TRacksTableStructDefV6)
    protected
        procedure DoDefineStruct(); override;
    end;

    TRacksTableStructDefV8 = class(TRacksTableStructDefV7)
    protected
        procedure DoDefineStruct(); override;
    end;

    // table updates
    TRacksTableUpdateV1 = class(TTableUpdate)
    public
        constructor Create(aUpdateID: integer);
    end;

    TRacksTableUpdateV3 = class(TTableUpdate)
    public
        constructor Create(aUpdateID: integer);
    end;

    TRacksTableUpdateV4 = class(TTableUpdate)
    public
        constructor Create(aUpdateID: integer);
    end;

    TRacksTableUpdateV5 = class(TTableUpdate)
    public
        constructor Create(aUpdateID: integer);
    end;

    TRacksTableUpdateV6 = class(TTableUpdate)
    public
        constructor Create(aUpdateID: integer);
    end;

    TRacksTableUpdateV7 = class(TTableUpdate)
    public
        constructor Create(aUpdateID: integer);
    end;

    TRacksTableUpdateV8 = class(TTableUpdate)
    public
        constructor Create(aUpdateID: integer);
    end;


implementation


{ TRacksTableStructDefV0 }

procedure TRacksTableStructDefV0.DoDefineStruct;
begin
    inherited;
    fName := 'RACKS';
end;

{ TRacksTableStructDefV1 }

procedure TRacksTableStructDefV1.DoDefineStruct();
begin
    inherited;
    self.AddField('NAME', tftString, 20);
    self.AddField('TYP', tftInteger, 0);
    self.AddField('X_MM', tftFloat, 0);
    self.AddField('Y_MM', tftFloat, 0);
    self.AddField('Z_MM', tftFloat, 0);
    self.AddField('ROWS', tftInteger, 0);
    self.AddField('COLS', tftInteger, 0);
    self.AddField('TUBETYP', tftInteger, 0);
    self.AddField('TUBEX_MM', tftFloat, 0);
    self.AddField('TUBEY_MM', tftFloat, 0);
    self.AddField('TUBEZ_MM', tftFloat, 0);
    self.AddField('BORDERX_MM', tftFloat, 0);
    self.AddField('BORDERY_MM', tftFloat, 0);
    self.AddField('POSX_FIRST_MM', tftFloat, 0);
    self.AddField('POSY_FIRST_MM', tftFloat, 0);
    self.AddField('POSX_LAST_MM', tftFloat, 0);
    self.AddField('POSY_LAST_MM', tftFloat, 0);
    self.AddField('POSX_OFFSET_MM', tftFloat, 0);
    self.AddField('POSY_OFFSET_MM', tftFloat, 0);
    self.AddField('ZTRAVEL_MM', tftFloat, 0);
    self.AddField('ZSCAN_MM', tftFloat, 0);
    self.AddField('ZDISP_MM', tftFloat, 0);
    self.AddField('ZMAX_MM', tftFloat, 0);
    self.AddField('H_XTAKE_MM', tftFloat, 0);
    self.AddField('H_YTAKE_MM', tftFloat, 0);
    self.AddField('H_ZTAKE_MM', tftFloat, 0);
    self.AddField('H_ROTATION', tftInteger, 0);
    self.AddField('H_VOPEN', tftInteger, 0);
    self.AddField('H_VCLOSE', tftInteger, 0);
    self.AddField('ADDOFFSETX_AFTERPOS', tftInteger, 0);
    self.AddField('ADDOFFSETX_MM', tftFloat, 0);
    self.AddField('ADDOFFSETY_AFTERPOS', tftInteger, 0);
    self.AddField('ADDOFFSETY_MM', tftFloat, 0);
    self.AddField('MOFFSETZ_MM', tftFloat, 0);
    self.AddField('MOFFSETUPX_MM', tftFloat, 0);
    self.AddField('MOFFSETUPY_MM', tftFloat, 0);
    self.AddField('SHIFT_RADIUS_MM', tftFloat, 0);
    self.AddField('SHIFT_NOOFSTEPS', tftInteger, 0);
    self.AddField('TUBEY2_MM', tftFloat, 0);
    self.AddField('ZPOSX_FIRST_MM', tftFloat, 0);
    self.AddField('ZPOSX_LAST_MM', tftFloat, 0);
    self.AddField('ZPOSY_FIRST_MM', tftFloat, 0);
    self.AddField('ZPOSY_LAST_MM', tftFloat, 0);
    self.AddField('CAPTYPE', tftString, 10);
    self.AddField('CAPDIAMETER', tftFloat, 0);
    self.AddField('CAP_GRIPZ_MM', tftFloat, 0);
    self.AddField('COLOR', tftInteger, 0);
    self.AddField('Z_LASTPOS_MM', tftFloat, 0);
    self.AddField('SLOPETYPE', tftInteger, 0);
    self.AddField('H_DIRECTION', tftInteger, 0);
    self.AddField('TUBEGETOPEN_STEPS', tftInteger, 0);
    self.AddField('TUBEGETCLOSE_STEPS', tftInteger, 0);
    self.AddField('TUBEPUTOPEN_STEPS', tftInteger, 0);
    self.AddField('H_RTAKE_DEGREE', tftFloat, 0);
    self.AddField('BLOCKBALANCEDOOR', tftBoolean, 0);
    self.AddField('STACKHEIGHT_MM', tftFloat, 0);

    self.AddIndex('NAME');
end;

{ TRacksTableStructDefV2 }

procedure TRacksTableStructDefV2.DoDefineStruct();
begin
    inherited;
    DelField('H_VOPEN');
    DelField('H_VCLOSE');
    DelField('TUBEGETOPEN_STEPS');
    DelField('TUBEGETCLOSE_STEPS');
    DelField('TUBEPUTOPEN_STEPS');
    DelField('H_ROTATION');

    AddFieldAt('H_VOPEN_MM', tftFloat, 0, 26);
    AddFieldAt('H_VCLOSE_MM', tftFloat, 0, 27);
    AddFieldAt('TUBEGETOPEN_MM', tftFloat, 0, 49);
    AddFieldAt('TUBEGETCLOSE_MM', tftFloat, 0, 50);
    AddFieldAt('TUBEPUTOPEN_MM', tftFloat, 0, 51);
end;

{ TRacksTableStructDefV3 }

procedure TRacksTableStructDefV3.DoDefineStruct();
begin
    inherited;

    AddFieldAt('TUBEPUTOFFSET_MM', tftFloat, 0, 52);
end;

{ TRacksTableStructDefV4 }

procedure TRacksTableStructDefV4.DoDefineStruct();
begin
    inherited;

    AddField('WellsAroundCenter', tftBoolean, 0);
    AddField('CenterX_mm', tftFloat, 0);
    AddField('CenterY_mm', tftFloat, 0);
end;

{ TRacksTableStructDefV5 }

procedure TRacksTableStructDefV5.DoDefineStruct;
begin
    inherited;

    AddFieldAt('TUBEGRIPOFFSET_MM', tftFloat, 0, 52);
end;

{ TRacksTableStructDefV6 }

procedure TRacksTableStructDefV6.DoDefineStruct;
begin
    inherited;

    AddField('DONOTPAINTTUBES', tftBoolean, 0);
end;

{ TRacksTableStructDefV7 }

procedure TRacksTableStructDefV7.DoDefineStruct;
begin
    inherited;

    AddField('DISCRETEPOSITIONS', tftBoolean, 0);
end;

{ TRacksTableStructDefV8 }

procedure TRacksTableStructDefV8.DoDefineStruct;
begin
    inherited;

    AddField('RACKMOVEZTRAVELOFFSET', tftFloat, 0);
end;

{ TRacksTableUpdateV1 }

constructor TRacksTableUpdateV1.Create(aUpdateID: integer);
begin
    inherited Create(aUpdateID, TRacksTableStructDefV0, INT_RACKS_MAJORREVISION_1);
    AlterStructure(TRacksTableStructDefV1);
    CopyMatchingFields([]);
end;

{ TRacksTableUpdateV3 }

constructor TRacksTableUpdateV3.Create(aUpdateID: integer);
begin
    inherited Create(aUpdateID, TRacksTableStructDefV2, INT_RACKS_MAJORREVISION_3);
    AlterStructure(TRacksTableStructDefV3);
    CopyMatchingFields([]);
end;

{ TRacksTableUpdateV4 }

constructor TRacksTableUpdateV4.Create(aUpdateID: integer);
begin
    inherited Create(aUpdateID, TRacksTableStructDefV3, INT_RACKS_MAJORREVISION_4);
    AlterStructure(TRacksTableStructDefV4);
    CopyMatchingFields([]);
end;

{ TRacksTableUpdateV5 }

constructor TRacksTableUpdateV5.Create(aUpdateID: integer);
begin
    inherited Create(aUpdateID, TRacksTableStructDefV4, INT_RACKS_MAJORREVISION_5);
    AlterStructure(TRacksTableStructDefV5);
    CopyMatchingFields([]);
end;

{ TRacksTableUpdateV6 }

constructor TRacksTableUpdateV6.Create(aUpdateID: integer);
begin
    inherited Create(aUpdateID, TRacksTableStructDefV5, INT_RACKS_MAJORREVISION_6);
    AlterStructure(TRacksTableStructDefV6);
    CopyMatchingFields([]);
end;

{ TRacksTableUpdateV7 }

constructor TRacksTableUpdateV7.Create(aUpdateID: integer);
begin
    inherited Create(aUpdateID, TRacksTableStructDefV6, INT_RACKS_MAJORREVISION_7);
    AlterStructure(TRacksTableStructDefV7);
    CopyMatchingFields([]);
end;

{ TRacksTableUpdateV8 }

constructor TRacksTableUpdateV8.Create(aUpdateID: integer);
begin
    inherited Create(aUpdateID, TRacksTableStructDefV7, INT_RACKS_MAJORREVISION_8);
    AlterStructure(TRacksTableStructDefV8);
    CopyMatchingFields([]);
end;


end.
