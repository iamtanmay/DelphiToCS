{ --------------------------------------------------------------------------------------------------
  Copyright © 2007 Zinsser Analytic GmbH - All rights reserved.
  Author       : Payman Kamali (pk)
  Description  : Special High-level Liquid Handling Data Adaptor functions
  --------------------------------------------------------------------------------------------------
  Revision History:
  date     op  method                       track-no improvement/change
  -------- --  ---------------------------  -------- -----------------------------------------------
  03.09.07 pk                               TN3847   Initial Revision. Functions from LiqHDataAdaptor
  09.11.07 pk                               TN3922   Dataset changed to DataProvider
  03.07.08 wl                                         TN4157
  23.09.08 wl                               TN4236   Vereinigung mit ..Fieldnames
  16.01.09 wl                                TN4362   an Änderungen in TQueryDataAdaptor angepasst
  06.07.09 pk                               TN4585.4 no longer uses PipDeviceManager
  31.07.09 wl  AddPipetteParameter          TN3950   3 neue Felder: SampleAspInsertMoveType, DilAspInsertMoveType, DispInsertMoveType
  02.10.09 pk  AddPipetteParameter          TN4585.4   SysAirDispVol is read as float instead of int
  04.11.09 pk                               TN4843     Replace TObjectList, TStringList, TStrings with Generics Lists and TStringArray
  22.07.11 wl  GetPipetteParamNames         TN5614.2   Im Designer werden Liq.H.Parameter jetzt alphabetisch angezeigt
  18.04.12 wl  AddPipetteParameter          TN5870   Statt Array wird eine Liste verwendet (das Array war zu klein dimensioniert)
  27.03.13 wl                               TN6045   uses geändert
  15.08.13 wl                               TN6217   Instance-Variablen gelöscht
  15.08.13 wl  GetDefaultTipTypeName        TN6217   Für Liquid wird nicht mehr STANDARD angeboten, sondern der erste Eintrag der in der Liste steht
  -------------------------------------------------------------------------------------------------- }

unit LiqHDataAdaptorExt;


interface


uses
    LiqHDataAdaptor;

type
    TLiqHDataAdaptorExt = class(TLiqHDataAdaptor)
    private
        function AddPipetteParameter(const aNewName: string; aPowderH: boolean): string;
        class function NewPipetteParameter(const aNewName: string; aPowderH: boolean): string;
    public
        procedure GetPipetteParamNames(out oLiqParNames, oPwdParNames: TArray<string>;
            const aGetLiqParNames, aGetPwdParNames: boolean);
        class function GetDefaultTipTypeName(aPowderH: boolean): string;
    end;

    TLiquidParDataAdaptor = class(TLiqHDataAdaptorExt)
    public
        function ReadAllNames(): TArray<string>;
        class function NewParameter(const aNewName: string): string;
    end;

    TPowderParDataAdaptor = class(TLiqHDataAdaptorExt)
    public
        function ReadAllNames(): TArray<string>;
        class function NewParameter(const aNewName: string): string;
    end;


implementation


uses
    SysUtils,
    Generics.Collections,
    AppTypes,
    TipTypeDataAdaptor,
    CommonTypes;

{ TLiqHDataAdaptorExt }

class function TLiqHDataAdaptorExt.GetDefaultTipTypeName(aPowderH: boolean): string;
var
    xNames: TArray<string>;
begin
    // find first defined tip type name
    if (aPowderH) then
        xNames := TTipTypeDataAdaptor.InstGetPowderTipTypeNames
    else
        xNames := TTipTypeDataAdaptor.InstGetLiquidTipTypeNames;

    if (Length(xNames) > 0) then
        EXIT(xNames[0])
    else
        EXIT('');
end;

function TLiqHDataAdaptorExt.AddPipetteParameter(const aNewName: string; aPowderH: boolean): string;
var
    xTipTypeName: string;
    xDefaultList: TObjectList<TLiqHDefault>;
    x: integer;
begin
    result := '';

    xTipTypeName := GetDefaultTipTypeName(aPowderH);
    if (xTipTypeName = '') then
        EXIT;

    self.SelectAndOpenAll(false);
    try
        self.DataProvider.Append;

        self.DataProvider.FieldByName(STR_LIQPARAM_FLD_PARAMNAME).AsString := aNewName;
        self.DataProvider.FieldByName('SysAirDispVol').AsFloat := 0;
        self.DataProvider.FieldByName('SysAirAspVol').AsFloat := 0;
        self.DataProvider.FieldByName('DispLiqDet').AsInteger := 4;
        self.DataProvider.FieldByName('WashFlag').AsInteger := 0;
        self.DataProvider.FieldByName('DispErrFlag').AsInteger := 0;
        self.DataProvider.FieldByName('SampleAspErrFlag').AsInteger := 0;
        self.DataProvider.FieldByName('DilAspErrFlag').AsInteger := 0;
        self.DataProvider.FieldByName('UsedTips').AsInteger := 0;
        self.DataProvider.FieldByName(STR_LIQPARAM_FLD_USEDTIPTYPE).AsString := xTipTypeName;
        self.DataProvider.FieldByName('LIQCLASS').AsString := '';
        self.DataProvider.FieldByName('WashVolFactor').AsFloat := 1;
        self.DataProvider.FieldByName('VolCorrCurve').AsString := '';

        self.DataProvider.FieldByName(STR_LIQPARAM_FLD_SAMPLEASPLIQDET).AsInteger := 0;
        self.DataProvider.FieldByName(STR_LIQPARAM_FLD_ASPSWITCHPOS).AsInteger := 0;
        self.DataProvider.FieldByName(STR_LIQPARAM_FLD_DILASPSWITCHPOS).AsInteger := 0;
        self.DataProvider.FieldByName(STR_LIQPARAM_FLD_DSPSWITCHPOS).AsInteger := 0;
        self.DataProvider.FieldByName(STR_LIQPARAM_FLD_SAMPLEASPMIXMETHOD).AsInteger := 0;
        self.DataProvider.FieldByName(STR_LIQPARAM_FLD_DISPMIXMETHOD).AsInteger := 0;
        self.DataProvider.FieldByName(STR_LIQPARAM_FLD_SAMPLEASPINSERTMOVETYPE).AsInteger := 0;
        self.DataProvider.FieldByName(STR_LIQPARAM_FLD_DILASPINSERTMOVETYPE).AsInteger := 0;
        self.DataProvider.FieldByName(STR_LIQPARAM_FLD_DISPINSERTMOVETYPE).AsInteger := 0;

        xDefaultList := CreateDefaultValList();
        try
            for x := 0 to xDefaultList.Count - 1 do
            begin
                if self.DataProvider.FieldByName(xDefaultList[x].Key).IsNull then
                    self.DataProvider.FieldByName(xDefaultList[x].Key).Value := xDefaultList[x].Value;
            end;
        finally
            FreeAndNil(xDefaultList);
        end;

        self.DataProvider.Post;

        result := aNewName;
    finally
        self.Close;
    end;
end;

class function TLiqHDataAdaptorExt.NewPipetteParameter(const aNewName: string; aPowderH: boolean): string;
var
    xDA: TLiqHDataAdaptorExt;
begin
    xDA := TLiqHDataAdaptorExt.Create;
    try
        result := xDA.AddPipetteParameter(aNewName, aPowderH);
    finally
        FreeAndNil(xDA);
    end;
end;

procedure TLiqHDataAdaptorExt.GetPipetteParamNames(out oLiqParNames, oPwdParNames: TArray<string>;
    const aGetLiqParNames, aGetPwdParNames: boolean);
var
    xUsedTipName: string;

    xPowderTipTypeNames: TList<string>;
    xLiqParNamesList, xPwdParNamesList: TList<string>;
begin
    SetLength(oLiqParNames, 0);
    SetLength(oPwdParNames, 0);
    xLiqParNamesList := nil;
    xPwdParNamesList := nil;

    xPowderTipTypeNames := TList<string>.Create();
    try
        xPowderTipTypeNames.AddRange(TTipTypeDataAdaptor.InstGetPowderTipTypeNames());
        xPowderTipTypeNames.Sort;

        if aGetLiqParNames then
            xLiqParNamesList := TList<string>.Create();
        if aGetPwdParNames then
            xPwdParNamesList := TList<string>.Create();
        try

            SelectAndOpenAllSorted(true);
            try
                while not self.DataProvider.Eof do
                begin

                    xUsedTipName := GetUsedTipType(self.DataProvider);

                    if xPowderTipTypeNames.IndexOf(xUsedTipName) >= 0 then
                    begin
                        if Assigned(xPwdParNamesList) then
                            xPwdParNamesList.Add(self.DataProvider.FieldByName(STR_LIQPARAM_FLD_PARAMNAME)
                                .AsString);
                    end
                    else
                    begin
                        if Assigned(xLiqParNamesList) then
                            xLiqParNamesList.Add(self.DataProvider.FieldByName(STR_LIQPARAM_FLD_PARAMNAME)
                                .AsString);
                    end;

                    self.DataProvider.Next;
                end;

            finally
                Close();
            end;

            if aGetLiqParNames then
                oLiqParNames := xLiqParNamesList.ToArray;
            if aGetPwdParNames then
                oPwdParNames := xPwdParNamesList.ToArray;

        finally
            FreeAndNil(xLiqParNamesList);
            FreeAndNil(xPwdParNamesList);
        end;
    finally
        xPowderTipTypeNames.Free;
    end;
end;

{ TLiquidParDataAdaptor }

function TLiquidParDataAdaptor.ReadAllNames(): TArray<string>;
var
    xDummy: TArray<string>;
begin
    GetPipetteParamNames(result, xDummy, true, false);
end;

class function TLiquidParDataAdaptor.NewParameter(const aNewName: string): string;
begin
    result := self.NewPipetteParameter(aNewName, false);
end;

{ TPowderParDataAdaptor }

function TPowderParDataAdaptor.ReadAllNames(): TArray<string>;
var
    xDummy: TArray<string>;
begin
    GetPipetteParamNames(xDummy, result, false, true);
end;

class function TPowderParDataAdaptor.NewParameter(const aNewName: string): string;
begin
    result := self.NewPipetteParameter(aNewName, true);
end;


end.
