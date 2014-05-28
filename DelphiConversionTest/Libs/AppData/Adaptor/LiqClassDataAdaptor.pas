{ --------------------------------------------------------------------------------------------------
  Copyright © 2005 Zinsser Analytic GmbH - All rights reserved.
  Author       : tbh
  Description  : Data Adaptor for LiqParClasses table
  --------------------------------------------------------------------------------------------------
  Revision History:
  date     op   method                       track-no improvement/change
  -------- ---  ---------------------------  -------- ----------------------------------------------
  21.04.05 tbh                               TN2386   new
  14.07.05 wl                                TN2506   6 neue Felder für Spitback
  15.07.05 wl   ReadRecFromDataset           TN2506   Option "SPITBACKVOL" bestimmt das Volumen
  15.07.05 thr  PipGetSpitbackVolVolume      TN2506   LeerString abgefangen
  20.07.05 thr  ReadValueForVol              TN2510   Endlosschleife (eof) abgefangen
  27.07.05 pk   PipGetSpitbackVolVolume      TN2516   read option SPITBACKVOL even if it is not at beginnig of option string
  08.08.05 pk   SampleAspWasteVol            TN2523   New
  10.08.05 pk   ReadRecForVol                TN2551   Param aOption changed from var to const
  01.09.05 thr                               TN2587   wash record in liquid class
  11.01.06 pk  InstallTable                  TN2871.1 override
  12.09.06 wl  TLiqParWashRec                TN3285   Records für LiqH und LiqClass jetzt identisch
  12.09.06 wl  TLiqParWashRec                TN3285   enthält UseMacro als integer
  23.09.06 wl  TLiqParWashRec                TN3326   DropTipMethod wieder entfernt
  13.10.06 wl   ReadNonInterpolatedValueForVol  TN3334  Wenn Volume < der kleinste Wert in der Tabelle: Nimm kleinsten Tabellenwert statt 0
  13.10.06 wl   ReadBoolValueForVol             TN3334  Wenn Volume < der kleinste Wert in der Tabelle: Nimm kleinsten Tabellenwert statt false
  13.10.06 wl   ReadStringValueForVol           TN3334  Wenn Volume < der kleinste Wert in der Tabelle: Nimm kleinsten Tabellenwert statt ''
  05.02.07 pk                                TN3544     Changes for updatemanager
  09.11.07 pk                                TN3921     Changes for updatemanager
  09.11.07 pk                                TN3924     Carr_X/Y/Z Steps to mm
  09.11.07 pk                                TN3922     Dataset changed to DataProvider
  23.09.08 wl                                TN4236   Vereinigung mit ..Fieldnames
  25.09.08 wl                                TN4242   es gibt nur noch WashMethod, nicht mehr WashMacro
  16.01.09 wl                                TN4362   an Änderungen in TQueryDataAdaptor angepasst
  09.07.09 pk                                TN4585.4 DataProvider.Locate removed, functionality replaced by SelectAndOpen
  -----------------------------------------------------------------------------------------------------------
  27.09.11 wl                                TN5698   wieder aktiviert
  10.08.12 wl                                TN5947.1 nicht mehr verwendet: WASHAFTERDISPENSE, WASHISFORCED, USEWASHMACRO, WASHMACRONAME, DRYAFTERWASH
  13.11.12 wl  GetKeyFields                  TN6015   kann entfernt werden, wenn Key = NameField
  -------------------------------------------------------------------------------------------------- }

unit LiqClassDataAdaptor;


interface


uses
    DataProvider,
    QueryDataAdaptor;

type
    TLiqClassRecExtern = record // interpolierte Werte
        SysAirAspVol: double;
        SysAirDispVol: double;
        SysAirSpeed: double;
        SysAirDelay: double;

        SampleAspSpeed: double;
        SampleAspDelay: double;

        TransAirVol: double;
        TransAirSpeed: double;
        TransAirDelay: double;

        DilAspSpeed: double;
        DilAspDelay: double;

        DispenseSpeed: double;
        DispenseDelay: double;

        SampleAspSpitBackVol: double;
        SampleAspSpitBackCount: integer;
        SampleAspSpitBackSpeed: double;
        SampleAspSpitBackDelay: double;
        SampleAspCh2SpitBackVol: double;
        SampleAspCh2SpitBackSpeed: double;

        SampleAspWasteVol: double;
        SampleAspWastePerCent: double;

        WashVolMin: double;
        WashVolMax: double;
        WashVolFactor: double;
        WashVolChannel2: double;
    end;

    TLiqClassRecIntern = record // gibt die Datenstruktur wieder
        CurveName: string;
        Volume: double;

        SysAirAspVol: double;
        SysAirDispVol: double;
        SysAirSpeed: double;
        SysAirDelay: double;

        SampleAspSpeed: double;
        SampleAspDelay: double;

        TransAirVol: double;
        TransAirSpeed: double;
        TransAirDelay: double;

        DilAspSpeed: double;
        DilAspDelay: double;

        DispenseSpeed: double;
        DispenseDelay: double;

        SampleAspSpitBackVol: double;
        SampleAspSpitBackCount: double;
        SampleAspSpitBackSpeed: double;
        SampleAspSpitBackDelay: double;
        SampleAspCh2SpitBackVol: double;
        SampleAspCh2SpitBackSpeed: double;

        SampleAspWasteVol: double;
        SampleAspWastePerCent: double;

        WashVolMin: double;
        WashVolMax: double;
        WashVolFactor: double;
        WashVolChannel2: double;
    end;

    TLiqClassDataAdaptor = class(TQueryDataAdaptor)
    private const
        STR_LIQCLASS_TBL = 'LIQPARCLASSES';
        STR_LIQCLASS_FLD_CURVENAME = 'LIQCLASSCURVENAME';
        STR_LIQCLASS_FLD_VOLUME = 'VOLUME';
        STR_LIQCLASS_FLD_AIRGAPASPVOL = 'AIRGAPASPVOL';
        STR_LIQCLASS_FLD_AIRGAPDISPVOL = 'AIRGAPDISPVOL';
        STR_LIQCLASS_FLD_AIRGAPSPEED = 'AIRGAPSPEED';
        STR_LIQCLASS_FLD_AIRGAPDELAY = 'AIRGAPDELAY';
        STR_LIQCLASS_FLD_DILUENTASPSPEED = 'DILUENTSPEED';
        STR_LIQCLASS_FLD_DILUENTDELAY = 'DILUENTDELAY';
        STR_LIQCLASS_FLD_SAMPLEASPSPEED = 'SAMPLESPEED';
        STR_LIQCLASS_FLD_SAMPLEASPDELAY = 'SAMPLEDELAY';
        STR_LIQCLASS_FLD_TRANSPGAPVOL = 'TRANSPGAPVOL';
        STR_LIQCLASS_FLD_TRANSPGAPSPEED = 'TRANSPGAPSPEED';
        STR_LIQCLASS_FLD_TRANSPGAPDELAY = 'TRANSPGAPDELAY';
        STR_LIQCLASS_FLD_DISPENSESPEED = 'DISPENSESPEED';
        STR_LIQCLASS_FLD_DISPENSEDELAY = 'DISPENSEDELAY';
        STR_LIQCLASS_FLD_SAMPLESPITBACKVOL = 'SAMPLESPITBACKVOL';
        STR_LIQCLASS_FLD_SAMPLESPITBACKCOUNT = 'SAMPLESPITBACKCOUNT';
        STR_LIQCLASS_FLD_SAMPLESPITBACKSPEED = 'SAMPLESPITBACKSPEED';
        STR_LIQCLASS_FLD_SAMPLESPITBACKDELAY = 'SAMPLESPITBACKDELAY';
        STR_LIQCLASS_FLD_SAMPLECH2SPITBVOL = 'SAMPLECH2SPITBVOL';
        STR_LIQCLASS_FLD_SAMPLECH2SPITBSPEED = 'SAMPLECH2SPITBSPEED';
        STR_LIQCLASS_FLD_SAMPLEWASTEVOL = 'SAMPLEWASTEVOL';
        STR_LIQCLASS_FLD_SAMPLEWASTEPERCENT = 'SAMPLEWASTEPERCENT';
        STR_LIQCLASS_FLD_WASHVOLMIN = 'WASHVOLMIN';
        STR_LIQCLASS_FLD_WASHVOLMAX = 'WASHVOLMAX';
        STR_LIQCLASS_FLD_WASHVOLFACTOR = 'WASHVOLFACTOR';
        STR_LIQCLASS_FLD_WASHVOLCHANNEL2 = 'WASHVOLCHANNEL2';
    private
        function ReadRecForVol(aClassName: string; aAspVol, aDilVol, aDispVol, aSpitbackVol: double;
            out oRec: TLiqClassRecExtern): boolean;
        function ReadValueForVol(aClassName, aDataField: string; aVol: double): double;
        function ReadNonInterpolatedValueForVol(aClassName, aDataField: string; aVol: double): double;
    protected
        function GetNameField(): string; override;
    public
        constructor Create();

        class function ReadRecFromDataset(aDataset: TDataProvider; var vRec: TLiqClassRecIntern): boolean;
        function GetRecForVol(const aName: string; aAspVol, aDilVol, aDispVol, aSpitbackVol: double;
            out oRec: TLiqClassRecExtern): boolean;
    end;


implementation


uses
    SysUtils,
    Variants;

constructor TLiqClassDataAdaptor.Create;
begin
    inherited Create(STR_LIQCLASS_TBL);
end;

class function TLiqClassDataAdaptor.ReadRecFromDataset(aDataset: TDataProvider;
    var vRec: TLiqClassRecIntern): boolean;
begin
    // könnte man mal für Editor gebrauchen

    result := not aDataset.Eof;

    // Parameter-Übergabe
    vRec.CurveName := aDataset.FieldByName(STR_LIQCLASS_FLD_CURVENAME).AsString;
    vRec.Volume := aDataset.FieldByName(STR_LIQCLASS_FLD_VOLUME).AsFloat;

    vRec.SysAirAspVol := aDataset.FieldByName(STR_LIQCLASS_FLD_AIRGAPASPVOL).AsFloat;
    vRec.SysAirDispVol := aDataset.FieldByName(STR_LIQCLASS_FLD_AIRGAPDISPVOL).AsFloat;
    vRec.SysAirSpeed := aDataset.FieldByName(STR_LIQCLASS_FLD_AIRGAPSPEED).AsFloat;
    vRec.SysAirDelay := aDataset.FieldByName(STR_LIQCLASS_FLD_AIRGAPDELAY).AsFloat;

    vRec.SampleAspSpeed := aDataset.FieldByName(STR_LIQCLASS_FLD_SAMPLEASPSPEED).AsFloat;
    vRec.SampleAspDelay := aDataset.FieldByName(STR_LIQCLASS_FLD_SAMPLEASPDELAY).AsFloat;

    vRec.SampleAspWasteVol := aDataset.FieldByName(STR_LIQCLASS_FLD_SAMPLEWASTEVOL).AsFloat;
    vRec.SampleAspWastePerCent := aDataset.FieldByName(STR_LIQCLASS_FLD_SAMPLEWASTEPERCENT).AsFloat;

    vRec.TransAirVol := aDataset.FieldByName(STR_LIQCLASS_FLD_TRANSPGAPVOL).AsFloat;
    vRec.TransAirDelay := aDataset.FieldByName(STR_LIQCLASS_FLD_TRANSPGAPDELAY).AsFloat;
    vRec.TransAirSpeed := aDataset.FieldByName(STR_LIQCLASS_FLD_TRANSPGAPSPEED).AsFloat;

    vRec.DilAspSpeed := aDataset.FieldByName(STR_LIQCLASS_FLD_DILUENTASPSPEED).AsFloat;
    vRec.DilAspDelay := aDataset.FieldByName(STR_LIQCLASS_FLD_DILUENTDELAY).AsFloat;

    vRec.DispenseSpeed := aDataset.FieldByName(STR_LIQCLASS_FLD_DISPENSESPEED).AsFloat;
    vRec.DispenseDelay := aDataset.FieldByName(STR_LIQCLASS_FLD_DISPENSEDELAY).AsFloat;

    vRec.SampleAspSpitBackVol := aDataset.FieldByName(STR_LIQCLASS_FLD_SAMPLESPITBACKVOL).AsFloat;
    vRec.SampleAspSpitBackCount := aDataset.FieldByName(STR_LIQCLASS_FLD_SAMPLESPITBACKCOUNT).AsFloat;
    vRec.SampleAspSpitBackSpeed := aDataset.FieldByName(STR_LIQCLASS_FLD_SAMPLESPITBACKSPEED).AsFloat;
    vRec.SampleAspSpitBackDelay := aDataset.FieldByName(STR_LIQCLASS_FLD_SAMPLESPITBACKDELAY).AsFloat;
    vRec.SampleAspCh2SpitBackVol := aDataset.FieldByName(STR_LIQCLASS_FLD_SAMPLECH2SPITBVOL).AsFloat;
    vRec.SampleAspCh2SpitBackSpeed := aDataset.FieldByName(STR_LIQCLASS_FLD_SAMPLECH2SPITBSPEED).AsFloat;

    vRec.WashVolMin := aDataset.FieldByName(STR_LIQCLASS_FLD_WASHVOLMIN).AsFloat;
    vRec.WashVolMax := aDataset.FieldByName(STR_LIQCLASS_FLD_WASHVOLMAX).AsFloat;
    vRec.WashVolFactor := aDataset.FieldByName(STR_LIQCLASS_FLD_WASHVOLFACTOR).AsFloat;
    vRec.WashVolChannel2 := aDataset.FieldByName(STR_LIQCLASS_FLD_WASHVOLCHANNEL2).AsFloat;
end;

function TLiqClassDataAdaptor.GetRecForVol(const aName: string;
    aAspVol, aDilVol, aDispVol, aSpitbackVol: double; out oRec: TLiqClassRecExtern): boolean;
begin
    SelectAndOpenByName(aName, true);
    try
        result := ReadRecForVol(aName, aAspVol, aDilVol, aDispVol, aSpitbackVol, oRec);
    finally
        Close();
    end;
end;

function TLiqClassDataAdaptor.ReadRecForVol(aClassName: string;
    aAspVol, aDilVol, aDispVol, aSpitbackVol: double; out oRec: TLiqClassRecExtern): boolean;
begin
    result := not self.DataProvider.Eof;

    // ---------------------------------------------------------------------------------------------- Asp Vol
    oRec.SysAirAspVol := ReadValueForVol(aClassName, STR_LIQCLASS_FLD_AIRGAPASPVOL, aAspVol);
    oRec.SysAirDispVol := ReadValueForVol(aClassName, STR_LIQCLASS_FLD_AIRGAPDISPVOL, aAspVol);
    oRec.SysAirSpeed := ReadValueForVol(aClassName, STR_LIQCLASS_FLD_AIRGAPSPEED, aAspVol);
    oRec.SysAirDelay := ReadValueForVol(aClassName, STR_LIQCLASS_FLD_AIRGAPDELAY, aAspVol);

    oRec.SampleAspSpeed := ReadValueForVol(aClassName, STR_LIQCLASS_FLD_SAMPLEASPSPEED, aAspVol);
    oRec.SampleAspDelay := ReadValueForVol(aClassName, STR_LIQCLASS_FLD_SAMPLEASPDELAY, aAspVol);

    oRec.SampleAspWasteVol := ReadValueForVol(aClassName, STR_LIQCLASS_FLD_SAMPLEWASTEVOL, aAspVol);
    oRec.SampleAspWastePerCent := ReadValueForVol(aClassNAme, STR_LIQCLASS_FLD_SAMPLEWASTEPERCENT, aAspVol);

    oRec.TransAirVol := ReadValueForVol(aClassName, STR_LIQCLASS_FLD_TRANSPGAPVOL, aAspVol);
    oRec.TransAirSpeed := ReadValueForVol(aClassName, STR_LIQCLASS_FLD_TRANSPGAPSPEED, aAspVol);
    oRec.TransAirDelay := ReadValueForVol(aClassName, STR_LIQCLASS_FLD_TRANSPGAPDELAY, aAspVol);

    // ---------------------------------------------------------------------------------------------- Dil Vol
    oRec.DilAspSpeed := ReadValueForVol(aClassName, STR_LIQCLASS_FLD_DILUENTASPSPEED, aDilVol);
    oRec.DilAspDelay := ReadValueForVol(aClassName, STR_LIQCLASS_FLD_DILUENTDELAY, aDilVol);

    // ---------------------------------------------------------------------------------------------- Disp Vol
    oRec.DispenseSpeed := ReadValueForVol(aClassName, STR_LIQCLASS_FLD_DISPENSESPEED, aDispVol);
    oRec.DispenseDelay := ReadValueForVol(aClassName, STR_LIQCLASS_FLD_DISPENSEDELAY, aDispVol);

    oRec.SampleAspSpitBackVol := ReadValueForVol(aClassName, STR_LIQCLASS_FLD_SAMPLESPITBACKVOL,
        aSpitBackVol);
    oRec.SampleAspSpitBackCount := Round(ReadValueForVol(aClassName, STR_LIQCLASS_FLD_SAMPLESPITBACKCOUNT,
        aSpitBackVol));
    oRec.SampleAspSpitBackSpeed := ReadValueForVol(aClassName, STR_LIQCLASS_FLD_SAMPLESPITBACKSPEED,
        aSpitBackVol);
    oRec.SampleAspSpitBackDelay := ReadValueForVol(aClassName, STR_LIQCLASS_FLD_SAMPLESPITBACKDELAY,
        aSpitBackVol);
    oRec.SampleAspCh2SpitBackVol := ReadValueForVol(aClassName, STR_LIQCLASS_FLD_SAMPLECH2SPITBVOL,
        aSpitBackVol);
    oRec.SampleAspCh2SpitBackSpeed := ReadValueForVol(aClassName, STR_LIQCLASS_FLD_SAMPLECH2SPITBSPEED,
        aSpitBackVol);

    // ---------------------------------------------------------------------------------------------- Wash
    oRec.WashVolMin := ReadNonInterpolatedValueForVol(aClassName, STR_LIQCLASS_FLD_WASHVOLMIN, aDispVol);
    oRec.WashVolMax := ReadNonInterpolatedValueForVol(aClassName, STR_LIQCLASS_FLD_WASHVOLMAX, aDispVol);
    oRec.WashVolFactor := ReadNonInterpolatedValueForVol(aClassName, STR_LIQCLASS_FLD_WASHVOLFACTOR,
        aDispVol);
    oRec.WashVolChannel2 := ReadNonInterpolatedValueForVol(aClassName, STR_LIQCLASS_FLD_WASHVOLCHANNEL2,
        aDispVol);
end;

function TLiqClassDataAdaptor.ReadNonInterpolatedValueForVol(aClassName, aDataField: string;
    aVol: double): double;
begin
    self.DataProvider.Last;
    while (self.DataProvider.FieldByName(STR_LIQCLASS_FLD_VOLUME).AsFloat > aVol) do
    begin
        self.DataProvider.Prior;
        if self.DataProvider.Bof then
            break;
    end;
    result := self.DataProvider.FieldByName(aDataField).AsFloat;
end;

function TLiqClassDataAdaptor.ReadValueForVol(aClassName: string; aDataField: string; aVol: double): double;
var
    xVol: array [0 .. 1] of extended;
    xValue: array [0 .. 1] of extended;
begin
    result := 0;
    self.DataProvider.First;
    if self.DataProvider.Eof then
        exit;
    while (self.DataProvider.FieldByName(STR_LIQCLASS_FLD_VOLUME).AsFloat < aVol) do
    begin
        self.DataProvider.Next;
        if self.DataProvider.Eof then
        begin
            self.DataProvider.last;
            break;
        end;
    end;
    xVol[1] := self.DataProvider.FieldByName(STR_LIQCLASS_FLD_VOLUME).AsFloat;
    xValue[1] := self.DataProvider.FieldByName(aDataField).AsFloat;
    // -------------------------- wenn aVol = Volume in Tabelle dann Wert zuweisen
    if (xVol[1] = aVol) then
    begin
        result := xValue[1]
    end
    else
    begin
        try
            // ------------------------- prüfen ob auch ein kleinerer Volume Wert existiert ---------
            self.DataProvider.Prior;
            if self.DataProvider.bof then
                self.DataProvider.first;
            if (self.DataProvider.FieldByName(STR_LIQCLASS_FLD_CURVENAME).AsString = aClassName) then
            begin
                xVol[0] := self.DataProvider.FieldByName(STR_LIQCLASS_FLD_VOLUME).AsFloat;
                xValue[0] := self.DataProvider.FieldByName(aDataField).AsFloat;
                if (xValue[0] <> xValue[1]) then
                    // ------------------- wenn ja - dann Faktor aus beiden Volumen ermitteln ----------
                    result := xValue[0] + (aVol - xVol[0]) / (xVol[1] - xVol[0]) * (xValue[1] - xValue[0])
                else
                    result := xValue[0];
            end;
        except
        end;
    end;
end;

function TLiqClassDataAdaptor.GetNameField: string;
begin
    EXIT(STR_LIQCLASS_FLD_CURVENAME);
end;


end.
