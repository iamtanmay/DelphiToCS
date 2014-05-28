unit VolcorrTableUpdate;
{ --------------------------------------------------------------------------------------------------
  Copyright © 2008 Zinsser Analytic GmbH - All rights reserved.
  Author       : Wolfgang Lyncke (wl)
  Description  :
  --------------------------------------------------------------------------------------------------
  Revision History:
  date     op  method                                 track-no improvement/change
  -------- --  -------------------------------------  -------- -----------------------------------------------
  10.07.08 wl  TVolcorrTableStructDefV2               TN4157   new field 'PIPDEVICENAME'
  10.07.08 wl  TVolcorrTableUpdateV2.ReplaceTotalTipMap  TN4157    reads Pip Devices and change Tips
  10.07.08 wl  TVolcorrTableUpdateV2                  TN4157   TableUpdate avtivated
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
    TVolcorrTableStructDefV0 = class(TTableStructDef)
    protected
        procedure DoDefineStruct(); override;
    end;

    TVolcorrTableStructDefV2 = class(TVolcorrTableStructDefV0)
    protected
        procedure DoDefineStruct(); override;
    end;

    TVolcorrTableUpdateV2 = class(TTableUpdate)
    private
        procedure VolcorrUpdateV2CustomFunc(aSender: TObject);
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

    STR_VOLCORRECT_V0_TBL = 'VOLCORR';

    STR_VOLCORRECT_V0_FLD_CURVENAME = 'NAME';
    STR_VOLCORRECT_V0_FLD_TIP = 'TIP';
    STR_VOLCORRECT_V0_FLD_VOLUME = 'VOLUME';
    STR_VOLCORRECT_V0_FLD_FACTOR = 'FACTOR';
    INT_VOLCORRECT_V0_FLDLEN_NAME = 20;
    STR_VOLCORRECT_V0_INDEX_FIELDS = STR_VOLCORRECT_V0_FLD_CURVENAME + ';' + STR_VOLCORRECT_V0_FLD_TIP + ';' +
        STR_VOLCORRECT_V0_FLD_VOLUME;

    STR_VOLCORRECT_V2_FLD_PIPDEVNAME = 'PIPDEVICENAME';
    INT_VOLCORRECT_V2_FLDLEN_PIPDEVNAME = 40;
    STR_VOLCORRECT_V2_INDEX_FIELDS = STR_VOLCORRECT_V0_FLD_CURVENAME + ';' + STR_VOLCORRECT_V2_FLD_PIPDEVNAME
        + ';' + STR_VOLCORRECT_V0_FLD_TIP + ';' + STR_VOLCORRECT_V0_FLD_VOLUME;

    { TVolcorrTableStructDefV0 }

procedure TVolcorrTableStructDefV0.DoDefineStruct;
begin
    inherited;
    fName := STR_VOLCORRECT_V0_TBL;
end;

{ TVolcorrTableStructDefV2 }

procedure TVolcorrTableStructDefV2.DoDefineStruct();
begin
    inherited;

    self.AddField(STR_VOLCORRECT_V0_FLD_CURVENAME, tftString, INT_VOLCORRECT_V0_FLDLEN_NAME);
    self.AddField(STR_VOLCORRECT_V2_FLD_PIPDEVNAME, tftString, INT_VOLCORRECT_V2_FLDLEN_PIPDEVNAME);
    self.AddField(STR_VOLCORRECT_V0_FLD_TIP, tftSmallInt, 0);
    self.AddField(STR_VOLCORRECT_V0_FLD_VOLUME, tftFloat, 0);
    self.AddField(STR_VOLCORRECT_V0_FLD_FACTOR, tftFloat, 0);

    self.AddIndex(STR_VOLCORRECT_V2_INDEX_FIELDS);
end;

{ TVolcorrTableUpdateV2 }

constructor TVolcorrTableUpdateV2.Create(aUpdateNumber: TUpdateID);
begin
    inherited Create(aUpdateNumber, TVolcorrTableStructDefV2, INT_REVISION_2);

    AlterStructure(TVolcorrTableStructDefV2);
    CopyMatchingFields([]);

    self.CustomDataFunc(VolcorrUpdateV2CustomFunc);
end;

procedure TVolcorrTableUpdateV2.ReplaceTotalTipMap(aPipDevLoader: TSettingsTablePipDeviceLoader);
var
    xDP: TDataProvider;
    xPipDeviceName: string;
begin
    xDP := fTableChangeAdaptor.CreateDestDataProvider();
    try
        xDP.SelectAndOpen('SELECT * FROM ' + STR_VOLCORRECT_V0_TBL, false);
        try
            while not xDP.Eof do
            begin

                xDP.Edit;
                xDP.FieldByName(STR_VOLCORRECT_V0_FLD_TIP).AsInteger :=
                    aPipDevLoader.GetArmTipFromTotalTip(xDP.FieldByName(STR_VOLCORRECT_V0_FLD_TIP).AsInteger,
                    xPipDeviceName);
                xDP.FieldByName(STR_VOLCORRECT_V2_FLD_PIPDEVNAME).AsString := xPipDeviceName;
                xDP.Post;

                xDP.Next;
            end;
        finally
            xDP.Close();
        end;
    finally
        xDP.Free;
    end;
end;

procedure TVolcorrTableUpdateV2.VolcorrUpdateV2CustomFunc(aSender: TObject);
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


{
  class procedure TVolcorrTableUpdater.UpdateCurveName(
  const aSourceVal: variant; var vDestVal: variant);
  begin
  if ( aSourceVal = null ) or ( aSourceVal = '' ) then
  vDestVal := STR_VOLCORRECT_V0_CURVENAME_DEFAULT
  else
  vDestVal := aSourceVal;
  end;


  procedure TVolcorrTableUpdater.DefineVersions();
  var
  xVersion : TTableUpdateVersionExt;
  xVersionNumber : integer;
  begin

  // V 1
  xVersionNumber := 1;
  xVersion := TTableUpdateVersionExt.Create( xVersionNumber );
  self.AddVersionUpdate( xVersion );


  xVersion.AddIndex( STR_VOLCORRECT_V0_INDEX_FIELDS );

  xVersion.CopyMatchingFields( [] );

  xVersion.AddDataItem( TTableUpdateExecuteDataMapItem.Create( TTableUpdateCopyFieldWithFuncDataMap.Create( StringArrayOf( [ STR_VOLCORRECT_V0_FLD_CURVENAME ] ), STR_VOLCORRECT_V0_FLD_CURVENAME, UpdateCurveName ) ) );
  end;
}
end.
