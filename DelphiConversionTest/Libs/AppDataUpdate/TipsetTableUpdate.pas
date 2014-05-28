unit TipsetTableUpdate;
{ --------------------------------------------------------------------------------------------------
  Copyright © 2008 Zinsser Analytic GmbH - All rights reserved.
  Author       : Wolfgang Lyncke (wl)
  Description  :
  --------------------------------------------------------------------------------------------------
  Revision History:
  date     op  method                                 track-no improvement/change
  -------- --  -------------------------------------  -------- -------------------------------------
  10.07.08 wl  TTipsetTableStructDefV2                TN4157   new field 'PIPDEVICENAME'
  10.07.08 wl  TTipsetTableUpdateV2.ReplaceTotalTipMap  TN4157    reads Pip Devices and change Tips
  10.07.08 wl  TTipsetTableUpdateV2                   TN4157   TableUpdate avtivated
  15.02.11 pk                               	     	 TN4780   changes needed to make UpdateManager compatible with TurboDB
  -------------------------------------------------------------------------------------------------- }


interface


uses
    Update,
    TableStructDef,
    TableUpdate,
    SettingsTableUpdate;

type
    // table structure definitions
    TTipsetTableStructDefV0 = class(TTableStructDef)
    protected
        procedure DoDefineStruct(); override;
    end;

    TTipsetTableStructDefV1 = class(TTipsetTableStructDefV0)
    protected
        procedure DoDefineStruct(); override;
    end;

    TTipsetTableStructDefV2 = class(TTipsetTableStructDefV1)
    protected
        procedure DoDefineStruct(); override;
    end;

    TTipsetTableUpdateV1 = class(TTableUpdate)
    public
        constructor Create(aUpdateNumber: TUpdateID);
    end;

    TTipsetTableUpdateV2_Rec = record
        Layout: string;
        Tip: integer;
    end;

    TTipsetTableUpdateV2_RecArray = array of TTipsetTableUpdateV2_Rec;

    TTipsetTableUpdateV2 = class(TTableUpdate)
    private
        procedure TipsetUpdateV2CustomFunc(aSender: TObject);
        procedure ReplaceTotalTipMap(aPipDevLoader: TSettingsTablePipDeviceLoader);
    public
        constructor Create(aUpdateNumber: TUpdateID);
    end;


implementation


uses
    SysUtils,
    DataProvider;

const
    INT_REVISION_1 = 1;
    INT_REVISION_2 = 2;

    STR_TIPSET_V0_TBL = 'TIPSET';
    STR_TIPSET_V0_FLD_LAYOUT = 'LAYOUT';
    STR_TIPSET_V0_FLD_TIP = 'TIP';
    STR_TIPSET_V0_FLD_TIPTYPE = 'TIPTYPE';
    INT_TIPSET_V0_FLDLEN_LAYOUT = 20;
    INT_TIPSET_V0_FLDLEN_TIPTYPE = 20;
    STR_TIPSET_V0_INDEX_FIELDS = STR_TIPSET_V0_FLD_LAYOUT + ';' + STR_TIPSET_V0_FLD_TIP;

    STR_TIPSET_V2_FLD_PIPDEVNAME = 'PIPDEVICENAME';
    INT_TIPSET_V2_FLDLEN_PIPDEVNAME = 40;
    STR_TIPSET_V2_INDEX_FIELDS = STR_TIPSET_V0_FLD_LAYOUT + ';' + STR_TIPSET_V2_FLD_PIPDEVNAME + ';' +
        STR_TIPSET_V0_FLD_TIP;

    { TTipsetTableStructDefV0 }

procedure TTipsetTableStructDefV0.DoDefineStruct;
begin
    inherited;
    fName := STR_TIPSET_V0_TBL;
end;

procedure TTipsetTableStructDefV1.DoDefineStruct();
begin
    inherited;
    self.AddField(STR_TIPSET_V0_FLD_LAYOUT, tftString, INT_TIPSET_V0_FLDLEN_LAYOUT);
    self.AddField(STR_TIPSET_V0_FLD_TIP, tftSmallint, 0);
    self.AddField(STR_TIPSET_V0_FLD_TIPTYPE, tftString, INT_TIPSET_V0_FLDLEN_TIPTYPE);
    self.AddIndex(STR_TIPSET_V0_INDEX_FIELDS);
end;

{ TRunTableUpdateV1 }

constructor TTipsetTableUpdateV1.Create(aUpdateNumber: TUpdateID);
begin
    inherited Create(aUpdateNumber, TTipsetTableStructDefV0, INT_REVISION_1);
    AlterStructure(TTipsetTableStructDefV1);
    CopyMatchingFields([]);
end;

{ TTipsetTableStructDefV2 }

procedure TTipsetTableStructDefV2.DoDefineStruct();
begin
    inherited;
    self.AddFieldAt(STR_TIPSET_V2_FLD_PIPDEVNAME, tftString, INT_TIPSET_V2_FLDLEN_PIPDEVNAME, 1);
    self.AddIndex(STR_TIPSET_V2_INDEX_FIELDS);
end;

{ TTipsetTableUpdateV2 }

constructor TTipsetTableUpdateV2.Create(aUpdateNumber: TUpdateID);
begin
    inherited Create(aUpdateNumber, TTipsetTableStructDefV1, INT_REVISION_2);
    AlterStructure(TTipsetTableStructDefV2);
    CopyMatchingFields([]);

    self.CustomDataFunc(TipsetUpdateV2CustomFunc);
end;

procedure TTipsetTableUpdateV2.ReplaceTotalTipMap(aPipDevLoader: TSettingsTablePipDeviceLoader);
const
    cSQLUpdate = 'UPDATE ' + STR_TIPSET_V0_TBL + ' SET ' + STR_TIPSET_V0_FLD_TIP + ' = %d ,' +
        STR_TIPSET_V2_FLD_PIPDEVNAME + ' = ''%s'' WHERE ' + STR_TIPSET_V0_FLD_LAYOUT + ' = ''%s'' AND ' +
        STR_TIPSET_V0_FLD_TIP + ' = %d';
var
    xDP: TDataProvider;
    xPipDeviceName: string;
    xOrigRecs: TTipsetTableUpdateV2_RecArray;
    x: integer;
    xSQL: string;
    xOrigTip, xNewTip: integer;
begin
    xDP := fTableChangeAdaptor.CreateSourceDataProvider();
    try
        xDP.SelectAndOpen('SELECT * FROM ' + STR_TIPSET_V0_TBL, true);
        try
            SetLength(xOrigRecs, xDP.RecordCount);
            x := 0;
            while not xDP.Eof do
            begin
                xOrigRecs[x].Layout := xDP.FieldByName(STR_TIPSET_V0_FLD_LAYOUT).AsString;
                xOrigRecs[x].Tip := xDP.FieldByName(STR_TIPSET_V0_FLD_TIP).AsInteger;
                xDP.Next;
                Inc(x);
            end;
        finally
            xDP.Close();
        end;

    finally
        xDP.Free;
    end;

    xDP := fTableChangeAdaptor.CreateDestDataProvider();
    try
        for x := 0 to high(xOrigRecs) do
        begin
            xOrigTip := xOrigRecs[x].Tip;
            xNewTip := aPipDevLoader.GetArmTipFromTotalTip(xOrigTip, xPipDeviceName);
            xSQL := Format(cSQLUpdate, [xNewTip, xPipDeviceName, xOrigRecs[x].Layout, xOrigTip]);
            xDP.ExecSQL(xSQL);
        end;

    finally
        xDP.Free;
    end;
end;

procedure TTipsetTableUpdateV2.TipsetUpdateV2CustomFunc(aSender: TObject);
var
    xPipDevLoader: TSettingsTablePipDeviceLoader;
begin
    xPipDevLoader := TSettingsTablePipDeviceLoader.Create(fTableChangeAdaptor);
    try
        self.ReplaceTotalTipMap(xPipDevLoader);
    finally
        xPipDevLoader.Free;
    end;
end;


end.
