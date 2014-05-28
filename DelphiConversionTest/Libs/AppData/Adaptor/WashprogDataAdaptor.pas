{ --------------------------------------------------------------------------------------------------
  Copyright © 2005 Zinsser Analytic GmbH - All rights reserved.
  Author       : Wolfgang Lyncke (wl)
  Description  : Data Adaptor for WASHPROG.DB
  --------------------------------------------------------------------------------------------------
  Revision History:
  date     op  method                       track-no  improvement/change
  -------- --  ---------------------------  --------  ----------------------------------------------
  08.11.05 wl                               TN2745    initial version
  24.11.05 pk  ReadName,SQLAllDistinctNames TN2765    new functions needed for GetNames
  23.07.07 wl  STR_WASHPROG_TBL             TN3792    Tabelle von WASHPROG in WASHPROGRAM umbenannt
  23.07.07 wl  VerifyTable                  TN3792    Konvertierroutine für neue Felder, liest auch Daten aus Settings
  24.07.07 wl  ReadWashProgRec              TN3792    neu
  26.07.07 pk  UpdateVersion1               TN3792    Use aDestTableDir path
  26.07.07 wl  AppendWashProgram            TN3792    legt neuen Datensatz an
  26.07.07 wl  VerifyTable                  TN3792    Tabelle wird umbenannt, Abbruch wenn Tabelle nicht vorhanden
  08.08.07 wl                               TN3802    WashSt_Ch3Off wird wieder aus Settings gelesen
  08.08.07 wl                               TN3802    N2Time aufgeteilt in 2 Felder (vor und nach "Wash needle")
  20.09.07 wl  UpdateVersion1               TN3867    Bug entfernt, der zu Key Violation beim Update geführt hat
  02.10.07 wl  GetKeyFields                 TN3811.5 neu: damit GetNames benutzt werden können
  09.11.07 pk                               TN3921   Changes for updatemanager
  09.11.07 pk                               TN3922   Query, Dataset changed to DataProvider
  23.09.08 wl                               TN4236   Vereinigung mit ..Fieldnames
  16.01.09 wl                               TN4362   an Änderungen in TQueryDataAdaptor angepasst
  10.12.09 pk                               TN4928   New: ReadWashProg, WriteWashProg
  11.12.09 pk                               TN4167   New DispHeightPos: Choose from Z-Scan or Z-Disp height for dispensing
  07.01.13 ts                               TN6065   New: CleanHeight: Choose from Z-Scan or Z-Disp height for cleaning
  -------------------------------------------------------------------------------------------------- }

unit WashprogDataAdaptor;


interface


uses
    Classes,
    DataProvider,
    QueryDataAdaptor;

const
    INT_DISPHEIGHTPOS_ZSCAN = 0;
    INT_DISPHEIGHTPOS_ZDISP = 1;

type
    TWashprogRec = record
        WPName: string;
        Diluent: string;
        InertGas: boolean;

        Resin_Height: double;
        InsertSpeed: integer;
        InsertDelay: double;

        DispHeightPos: integer;
        CleanHeight: integer;

        AspDispVol: double;
        AspDispSpeed: integer;
        AspDispDelay: double;

        N2Time1: double;
        N2Time2: double;
        SingleRetract: boolean;

        Clean_Volume: double;
        CleanDispSpeed: integer;
        CleanMode: integer;
        CleanDelay: double;

        InnerWashVolume: double;
        OuterWashVolume: double;
        InnerWashN2Time: double;
    end;

    TWashprogDataAdaptor = class(TQueryDataAdaptor)
    protected
        function GetNameField(): string; override;
    public
        constructor Create();
        function ReadWashProg(const aWashProgName: string; out oWashProgRec: TWashprogRec): boolean;
        procedure WriteWashProg(const aRec: TWashprogRec);
        class procedure WriteRecAtCursor(aDataset: TDataProvider; const aRec: TWashprogRec; aAppend: boolean);
        class function ReadRecAtCursor(aDataset: TDataProvider; var vRec: TWashprogRec): boolean;
        class procedure AppendWashProgram(aDataset: TDataProvider; const aWPName: string);
        class function MakeDefaultRec(): TWashprogRec;

        class function InstReadWashProgRec(const aWashProgName: string;
            out oWashProgRec: TWashprogRec): boolean;
    end;


implementation


uses
    SysUtils,
    AppTypes,
    DataAdaptor;

const
    STR_WASHPROG_TBL = 'WASHPROGRAM';

    STR_WASHPROG_FLD_WPNAME = 'WPNAME';
    STR_WASHPROG_FLD_DILUENT = 'DILUENT';
    STR_WASHPROG_FLD_INERTGAS = 'INERTGAS';

    STR_WASHPROG_FLD_RESINHEIGHT = 'RESINHEIGHT';
    STR_WASHPROG_FLD_INSERTSPEED = 'INSERTSPEED';
    STR_WASHPROG_FLD_INSERTDELAY = 'INSERTDELAY'; // neu: übernimmt Wert aus 'ASPTIME'

    STR_WASHPROG_FLD_ASPDISPSPEED = 'ASPDISPSPEED';
    STR_WASHPROG_FLD_ASPDISPVOL = 'ASPDISPVOL';
    STR_WASHPROG_FLD_ASPDISPDELAY = 'ASPDISPDELAY'; // neu: übernimmt Wert aus 'ASPTIME'

    STR_WASHPROG_FLD_N2TIME1 = 'N2TIME1'; // neu: übernimmt Wert aus 'N2_TIME'
    STR_WASHPROG_FLD_N2TIME2 = 'N2TIME2'; // neu: übernimmt Wert aus 'N2_TIME'

    STR_WASHPROG_FLD_DISPHEIGHTPOS = 'DISPHEIGHTPOS';
    STR_WASHPROG_FLD_SINGLERETRACT = 'SINGLERETRACT';

    STR_WASHPROG_FLD_CLEANVOLUME = 'CLEANVOLUME';
    STR_WASHPROG_FLD_CLEANDISPSPEED = 'CLEANDISPSPEED';
    STR_WASHPROG_FLD_CLEANMODE = 'CLEANMODE'; // neu: Default = 0
    STR_WASHPROG_FLD_CLEANDELAY = 'CLEANDELAY';
    // neu: entspricht Setting: ( 'Washprogram', 'DelayBefore2ndBlowN2' )

    STR_WASHPROG_FLD_INNERWASHVOLUME = 'INNERWASHVOLUME';
    STR_WASHPROG_FLD_INNERWASHN2TIME = 'INNERWASHN2TIME'; // neu: vorher immer 2000 msec
    STR_WASHPROG_FLD_OUTERWASHVOLUME = 'OUTERWASHVOLUME';

    STR_WASHPROG_FLD_CLEANHEIGHT = 'CLEANHEIGHT';

    { TWashprogDataAdaptor }

function TWashProgDataAdaptor.GetNameField(): string;
begin
    result := STR_WASHPROG_FLD_WPNAME;
end;

constructor TWashProgDataAdaptor.Create;
begin
    inherited Create(STR_WASHPROG_TBL);
end;

class function TWashprogDataAdaptor.ReadRecAtCursor(aDataset: TDataProvider; var vRec: TWashprogRec): boolean;
begin
    result := not aDataset.Eof;

    vRec.WPName := aDataset.FieldByName(STR_WASHPROG_FLD_WPNAME).AsString;
    vRec.Diluent := aDataset.FieldByName(STR_WASHPROG_FLD_DILUENT).AsString;
    vRec.InertGas := aDataset.FieldByName(STR_WASHPROG_FLD_INERTGAS).AsBoolean;

    vRec.Resin_Height := aDataset.FieldByName(STR_WASHPROG_FLD_RESINHEIGHT).AsFloat;
    vRec.InsertSpeed := aDataset.FieldByName(STR_WASHPROG_FLD_INSERTSPEED).AsInteger;
    vRec.InsertDelay := aDataset.FieldByName(STR_WASHPROG_FLD_INSERTDELAY).AsFloat;

    vRec.AspDispVol := aDataset.FieldByName(STR_WASHPROG_FLD_ASPDISPVOL).AsFloat;
    vRec.AspDispSpeed := aDataset.FieldByName(STR_WASHPROG_FLD_ASPDISPSPEED).AsInteger;
    vRec.AspDispDelay := aDataset.FieldByName(STR_WASHPROG_FLD_ASPDISPDELAY).AsFloat;

    vRec.N2Time1 := aDataset.FieldByName(STR_WASHPROG_FLD_N2TIME1).AsFloat;
    vRec.N2Time2 := aDataset.FieldByName(STR_WASHPROG_FLD_N2TIME2).AsFloat;

    vRec.DispHeightPos := aDataset.FieldByName(STR_WASHPROG_FLD_DISPHEIGHTPOS).AsInteger;
    vRec.SingleRetract := aDataset.FieldByName(STR_WASHPROG_FLD_SINGLERETRACT).AsBoolean;

    vRec.Clean_Volume := aDataset.FieldByName(STR_WASHPROG_FLD_CLEANVOLUME).AsFloat;
    vRec.CleanDispSpeed := aDataset.FieldByName(STR_WASHPROG_FLD_CLEANDISPSPEED).AsInteger;
    vRec.CleanMode := aDataset.FieldByName(STR_WASHPROG_FLD_CLEANMODE).AsInteger;
    vRec.CleanDelay := aDataset.FieldByName(STR_WASHPROG_FLD_CLEANDELAY).AsFloat;

    vRec.InnerWashVolume := aDataset.FieldByName(STR_WASHPROG_FLD_INNERWASHVOLUME).AsFloat;
    vRec.InnerWashN2Time := aDataset.FieldByName(STR_WASHPROG_FLD_INNERWASHN2TIME).AsFloat;
    vRec.OuterWashVolume := aDataset.FieldByName(STR_WASHPROG_FLD_OUTERWASHVOLUME).AsFloat;
    vRec.CleanHeight := aDataset.FieldByName(STR_WASHPROG_FLD_CLEANHEIGHT).AsInteger;
end;

class function TWashprogDataAdaptor.MakeDefaultRec(): TWashprogRec;
begin
    result.WPName := '';
    result.Diluent := '';
    result.InertGas := true;

    result.Resin_Height := 0;
    result.InsertSpeed := 0;
    result.InsertDelay := 0;

    result.AspDispVol := 0;
    result.AspDispSpeed := 0;
    result.AspDispDelay := 0;

    result.N2Time1 := 0;
    result.N2Time2 := 0;

    result.DispHeightPos := INT_DISPHEIGHTPOS_ZSCAN;
    result.SingleRetract := true;

    result.Clean_Volume := 0;
    result.CleanDispSpeed := 0;
    result.CleanMode := 0;
    result.CleanDelay := 0;

    result.InnerWashVolume := 0;
    result.InnerWashN2Time := 2;
    result.OuterWashVolume := 0;
    result.CleanHeight := INT_DISPHEIGHTPOS_ZSCAN;
end;

class procedure TWashProgDataAdaptor.AppendWashProgram(aDataset: TDataProvider; const aWPName: string);
var
    xRec: TWashprogRec;
begin
    ASSERT(aDataSet.Active, 'Dataset not active');
    xRec := MakeDefaultRec();
    aDataset.Append;
    aDataset.FieldByName(STR_WASHPROG_FLD_WPNAME).AsString := aWPName;
    aDataset.FieldByName(STR_WASHPROG_FLD_INERTGAS).AsBoolean := xRec.InertGas;
    aDataset.FieldByName(STR_WASHPROG_FLD_CLEANMODE).AsInteger := xRec.CleanMode;
    aDataset.FieldByName(STR_WASHPROG_FLD_SINGLERETRACT).AsBoolean := xRec.SingleRetract;
    aDataset.FieldByName(STR_WASHPROG_FLD_INNERWASHN2TIME).AsFloat := xRec.InnerWashN2Time;

    aDataset.Post;
end;

class procedure TWashprogDataAdaptor.WriteRecAtCursor(aDataset: TDataProvider; const aRec: TWashprogRec;
    aAppend: boolean);
begin
    ASSERT(aDataSet.Active, 'Dataset not active');
    if aAppend then
        aDataset.Append
    else
        aDataset.Edit;

    aDataset.FieldByName(STR_WASHPROG_FLD_WPNAME).AsString := aRec.WPName;
    aDataset.FieldByName(STR_WASHPROG_FLD_DILUENT).AsString := aRec.Diluent;
    aDataset.FieldByName(STR_WASHPROG_FLD_INERTGAS).AsBoolean := aRec.InertGas;

    aDataset.FieldByName(STR_WASHPROG_FLD_RESINHEIGHT).AsFloat := aRec.Resin_Height;
    aDataset.FieldByName(STR_WASHPROG_FLD_INSERTSPEED).AsInteger := aRec.InsertSpeed;
    aDataset.FieldByName(STR_WASHPROG_FLD_INSERTDELAY).AsFloat := aRec.InsertDelay;

    aDataset.FieldByName(STR_WASHPROG_FLD_ASPDISPVOL).AsFloat := aRec.AspDispVol;
    aDataset.FieldByName(STR_WASHPROG_FLD_ASPDISPSPEED).AsInteger := aRec.AspDispSpeed;
    aDataset.FieldByName(STR_WASHPROG_FLD_ASPDISPDELAY).AsFloat := aRec.AspDispDelay;

    aDataset.FieldByName(STR_WASHPROG_FLD_N2TIME1).AsFloat := aRec.N2Time1;
    aDataset.FieldByName(STR_WASHPROG_FLD_N2TIME2).AsFloat := aRec.N2Time2;

    aDataset.FieldByName(STR_WASHPROG_FLD_DISPHEIGHTPOS).AsInteger := aRec.DispHeightPos;
    aDataset.FieldByName(STR_WASHPROG_FLD_SINGLERETRACT).AsBoolean := aRec.SingleRetract;

    aDataset.FieldByName(STR_WASHPROG_FLD_CLEANVOLUME).AsFloat := aRec.Clean_Volume;
    aDataset.FieldByName(STR_WASHPROG_FLD_CLEANDISPSPEED).AsInteger := aRec.CleanDispSpeed;
    aDataset.FieldByName(STR_WASHPROG_FLD_CLEANMODE).AsInteger := aRec.CleanMode;
    aDataset.FieldByName(STR_WASHPROG_FLD_CLEANDELAY).AsFloat := aRec.CleanDelay;

    aDataset.FieldByName(STR_WASHPROG_FLD_INNERWASHVOLUME).AsFloat := aRec.InnerWashVolume;
    aDataset.FieldByName(STR_WASHPROG_FLD_INNERWASHN2TIME).AsFloat := aRec.InnerWashN2Time;
    aDataset.FieldByName(STR_WASHPROG_FLD_OUTERWASHVOLUME).AsFloat := aRec.OuterWashVolume;
    aDataset.FieldByName(STR_WASHPROG_FLD_CLEANHEIGHT).AsInteger := aRec.CleanHeight;

    aDataset.Post;
end;

procedure TWashprogDataAdaptor.WriteWashProg(const aRec: TWashprogRec);
var
    xAppend: boolean;
begin
    self.SelectAndOpenByName(aRec.WPName, false);
    try
        xAppend := self.DataProvider.IsEmpty;
        self.WriteRecAtCursor(self.DataProvider, aRec, xAppend);
    finally
        self.Close();
    end;

end;

function TWashprogDataAdaptor.ReadWashProg(const aWashProgName: string;
    out oWashProgRec: TWashprogRec): boolean;
begin
    self.SelectAndOpenByName(aWashProgName, true);
    try
        result := not self.DataProvider.IsEmpty;
        if not result then
            EXIT;

        ReadRecAtCursor(self.DataProvider, oWashProgRec)
    finally
        self.Close;
    end;
end;

class function TWashprogDataAdaptor.InstReadWashProgRec(const aWashProgName: string;
    out oWashProgRec: TWashprogRec): boolean;
var
    xDataAdaptor: TWashProgDataAdaptor;
begin
    xDataAdaptor := TWashProgDataAdaptor.Create;
    try
        result := xDataAdaptor.ReadWashProg(aWashProgName, oWashProgRec);
    finally
        xDataAdaptor.Free;
    end;
end;


end.
