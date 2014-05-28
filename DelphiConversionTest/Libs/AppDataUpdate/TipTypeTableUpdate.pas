unit TipTypeTableUpdate;


{ --------------------------------------------------------------------------------------------------
  Copyright © 2011 Zinsser Analytic GmbH - All rights reserved.
  Author       : Payman Kamali (pk)
  Description  :
  --------------------------------------------------------------------------------------------------
  Revision History:
  date     op  method                       track-no improvement/change
  -------- --  ---------------------------  -------- -----------------------------------------------
  15.02.11 pk                               TN4780   updates finally added
  -------------------------------------------------------------------------------------------------- }
interface


uses
    Update,
    TableStructDef,
    TableUpdate;

type
    // table structure definitions
    TTipTypeTableStructDefV0 = class(TTableStructDef)
    protected
        procedure DoDefineStruct(); override;
    end;

    TTipTypeTableStructDefV1 = class(TTipTypeTableStructDefV0)
    protected
        procedure DoDefineStruct(); override;
    end;

    TTipTypeTableStructDefV2 = class(TTipTypeTableStructDefV1)
    protected
        procedure DoDefineStruct(); override;
    end;

    // table updates
    TTipTypeTableUpdateV1 = class(TTableUpdate)
    public
        constructor Create(aUpdateNumber: TUpdateID);
    end;

    TTipTypeTableUpdateV2 = class(TTableUpdate)
    public
        constructor Create(aUpdateNumber: TUpdateID);
    end;


implementation


uses
    SysUtils;

const
    INT_REVISION_1 = 1;
    INT_REVISION_2 = 2;

const
    STR_TIPTYPES_FLD_TYPENAME = 'TYPENAME';

const
    STR_TIPTYPES_FLD_BASICTIPTYPE = 'BASICTIPTYPE';

const
    STR_TIPTYPES_FLD_TOOLNAME = 'TOOLNAME';

const
    STR_TIPTYPES_FLD_REDIDEVICENAME = 'REDIDEVICENAME';

const
    STR_TIPTYPES_FLD_DILUTORNAME = 'DILUTORNAME';

const
    STR_TIPTYPES_FLD_MAXVOLUME = 'MAXVOLUME';

const
    STR_TIPTYPES_FLD_MINVOLUME = 'MINVOLUME';

const
    STR_TIPTYPES_FLD_XOFFSET = 'XOFFSET';

const
    STR_TIPTYPES_FLD_YOFFSET = 'YOFFSET';

const
    STR_TIPTYPES_FLD_ZOFFSET = 'ZOFFSET';

const
    STR_TIPTYPES_FLD_ZOFFSETREMOVABLE = 'ZOFFSETREMOVABLE';

const
    STR_TIPTYPES_FLD_ZOFFSETWASH = 'ZOFFSETWASH';

const
    STR_TIPTYPES_FLD_ZOFFSETWASTE = 'ZOFFSETWASTE';

const
    STR_TIPTYPES_FLD_ZOFFSETDRY = 'ZOFFSETDRY';

const
    STR_TIPTYPES_FLD_DRYAFTERFLUSH = 'DRYAFTERFLUSH';

const
    STR_TIPTYPES_FLD_METHODNAMEGETTIP = 'METHODNAMEGETTIP';

const
    STR_TIPTYPES_FLD_METHODNAMEPUTTIP = 'METHODNAMEPUTTIP';

const
    INT_TIPTYPES_FLDLEN_TYPENAME = 20;

const
    INT_TIPTYPES_FLDLEN_BASICTIPTYPE = 10;

const
    INT_TIPTYPES_FLDLEN_TOOLNAME = 30;

const
    INT_TIPTYPES_FLDLEN_REDIDEVICENAME = 30;

const
    INT_TIPTYPES_FLDLEN_DILUTORNAME = 30;

const
    INT_TIPTYPES_FLDLEN_METHODNAMEGETTIP = 20;

const
    INT_TIPTYPES_FLDLEN_METHODNAMEPUTTIP = 20;

const
    STR_TIPTYPES_INDEX_FIELDS = STR_TIPTYPES_FLD_TYPENAME;

const
    STR_TIPTYPES_FLD_DONOTINIT_V2 = 'DONOTINIT';

    { TTipTypeTableStructDefV0 }

procedure TTipTypeTableStructDefV0.DoDefineStruct;
begin
    inherited;
    fName := 'TIPTYPES';
end;

{ TTipTypeTableStructDefV1 }

procedure TTipTypeTableStructDefV1.DoDefineStruct();
begin
    inherited;
    AddField(STR_TIPTYPES_FLD_TYPENAME, tftString, INT_TIPTYPES_FLDLEN_TYPENAME);
    AddField(STR_TIPTYPES_FLD_BASICTIPTYPE, tftString, INT_TIPTYPES_FLDLEN_BASICTIPTYPE);
    AddField(STR_TIPTYPES_FLD_TOOLNAME, tftString, INT_TIPTYPES_FLDLEN_TOOLNAME);
    AddField(STR_TIPTYPES_FLD_REDIDEVICENAME, tftString, INT_TIPTYPES_FLDLEN_REDIDEVICENAME);
    AddField(STR_TIPTYPES_FLD_DILUTORNAME, tftString, INT_TIPTYPES_FLDLEN_DILUTORNAME);
    AddField(STR_TIPTYPES_FLD_MAXVOLUME, tftFloat, 0);
    AddField(STR_TIPTYPES_FLD_MINVOLUME, tftFloat, 0);
    AddField(STR_TIPTYPES_FLD_XOFFSET, tftFloat, 0);
    AddField(STR_TIPTYPES_FLD_YOFFSET, tftFloat, 0);
    AddField(STR_TIPTYPES_FLD_ZOFFSET, tftFloat, 0);
    AddField(STR_TIPTYPES_FLD_ZOFFSETREMOVABLE, tftFloat, 0);
    AddField(STR_TIPTYPES_FLD_ZOFFSETWASH, tftFloat, 0);
    AddField(STR_TIPTYPES_FLD_ZOFFSETWASTE, tftFloat, 0);
    AddField(STR_TIPTYPES_FLD_ZOFFSETDRY, tftFloat, 0);
    AddField(STR_TIPTYPES_FLD_DRYAFTERFLUSH, tftBoolean, 0);
    AddField(STR_TIPTYPES_FLD_METHODNAMEGETTIP, tftString, INT_TIPTYPES_FLDLEN_METHODNAMEGETTIP);
    AddField(STR_TIPTYPES_FLD_METHODNAMEPUTTIP, tftString, INT_TIPTYPES_FLDLEN_METHODNAMEPUTTIP);

    AddIndex(STR_TIPTYPES_INDEX_FIELDS);

end;

procedure TTipTypeTableStructDefV2.DoDefineStruct();
begin
    inherited;
    AddField(STR_TIPTYPES_FLD_DONOTINIT_V2, tftBoolean, 0);

end;

constructor TTipTypeTableUpdateV1.Create(aUpdateNumber: TUpdateID);
begin
    inherited Create(aUpdateNumber, TTipTypeTableStructDefV0, INT_REVISION_1);
    AlterStructure(TTipTypeTableStructDefV1);
    CopyMatchingFields([]);
end;

constructor TTipTypeTableUpdateV2.Create(aUpdateNumber: TUpdateID);
begin
    inherited Create(aUpdateNumber, TTipTypeTableStructDefV1, INT_REVISION_2);
    AlterStructure(TTipTypeTableStructDefV2);
    CopyMatchingFields([]);
end;


{
  class procedure TTipTypeTableUpdater.UpdateVersion1( aSender : TObject; const aDestTableDir : string );
  const
  STR_TIPTYPEENTRY_DELIMITER = ',';
  var
  xDataAdaptor : TTipTypeDataAdaptor;
  xSettingsQuery: TQuery;
  x, xCurrentChar: integer;
  xRec: TTipType;
  xValue, xSubValue: string;
  begin
  // brutal gehackt !!!
  xSettingsQuery := TQuery.Create( nil );
  xDataAdaptor := TTipTypeDataAdaptor.Create();
  try
  xDataAdaptor.SelectAndOpen( Format( 'SELECT * FROM "%s%s"', [ aDestTableDir, STR_TIPTYPES_TBL ] ), false );

  // Update only if table is empty
  if xDataAdaptor.Dataset.RecordCount > 0 then EXIT;

  // STANDARD tip eintragen!!
  xRec := TTipTypeDataAdaptor.GetEmptyRec();
  xRec.Name  := cStandardTipName;
  xRec.BasicType := DefaultTip;
  TTipTypeDataAdaptor.WriteRecAtCursor( xDataAdaptor.Dataset, xRec, true );

  xSettingsQuery.DatabaseName := xDataAdaptor.GetAlias();
  xSettingsQuery.SQL.Add( 'select * from SETTINGS where AREA = "ROBOT" and SECTION = "TipTypes"');
  xSettingsQuery.Open;

  while ( not xSettingsQuery.Eof ) do begin

  xValue := xSettingsQuery.Fields[3].AsString;

  xCurrentChar:=1;
  for x := 0 to 15 do begin
  xSubValue := gmReadSubString( xCurrentChar, xValue, STR_TIPTYPEENTRY_DELIMITER );

  case x of
  0: xRec.Name                 := xSubValue;
  1: xRec.RelLength_mm         := StrToIntDef( xSubValue, 0 );
  2: xRec.MaxVolume            := StrToIntDef( xSubValue, 0 );
  3: xRec.BasicType            := TTipTypeDataAdaptor.GetBasicTipTypeFromName( xSubValue, xRec.DitiType );
  4: xRec.DTOffset_mm          := StrToIntDef( xSubValue, 0 );
  5: xRec.DeviceName           := xSubValue;
  6: xRec.MinVolume            := StrToIntDef( xSubValue, 0 );
  7: xRec.ToolName             := xSubValue;
  8: xRec.XOffset_mm           := StrToIntDef( xSubValue, 0 );
  9: xRec.YOffset_mm           := StrToIntDef( xSubValue, 0 );
  10: xRec.DilutorName         := xSubValue;
  11: xRec.WashZOffset_mm      := StrToIntDef( xSubValue, 0 );
  12: xRec.WasteZOffset_mm     := StrToIntDef( xSubValue, 0 );
  13: xRec.DryZOffset_mm       := StrToIntDef( xSubValue, 0 );
  14: xRec.DoNotDryAfterFlush  := StrToBoolDef( xSubValue, false );
  15: xRec.DoNotInit           := StrToBoolDef( xSubValue, false );
  end;
  end;

  TTipTypeDataAdaptor.WriteRecAtCursor( xDataAdaptor.Dataset, xRec, true );

  xSettingsQuery.Next;
  end;
  xDataAdaptor.Close();
  finally
  xDataAdaptor.Free;
  xSettingsQuery.Free;
  end;
  end;

  procedure TTipTypeTableUpdater.DefineVersions();
  var
  xVersion : TTableUpdateVersionExt;
  xVersionNumber : integer;
  begin
  // V 1
  xVersionNumber := 1;
  xVersion := TTableUpdateVersionExt.Create( xVersionNumber );
  self.AddVersionUpdate( xVersion );
  xVersion.AddField( STR_TIPTYPES_FLD_TYPENAME            , ftString, INT_TIPTYPES_FLDLEN_TYPENAME         );
  xVersion.AddField( STR_TIPTYPES_FLD_BASICTIPTYPE        , ftString, INT_TIPTYPES_FLDLEN_BASICTIPTYPE     );
  xVersion.AddField( STR_TIPTYPES_FLD_TOOLNAME            , ftString, INT_TIPTYPES_FLDLEN_TOOLNAME         );
  xVersion.AddField( STR_TIPTYPES_FLD_REDIDEVICENAME      , ftString, INT_TIPTYPES_FLDLEN_REDIDEVICENAME   );
  xVersion.AddField( STR_TIPTYPES_FLD_DILUTORNAME         , ftString, INT_TIPTYPES_FLDLEN_DILUTORNAME      );
  xVersion.AddField( STR_TIPTYPES_FLD_MAXVOLUME           , ftFloat , 0                                    );
  xVersion.AddField( STR_TIPTYPES_FLD_MINVOLUME           , ftFloat , 0                                    );
  xVersion.AddField( STR_TIPTYPES_FLD_XOFFSET             , ftFloat , 0                                    );
  xVersion.AddField( STR_TIPTYPES_FLD_YOFFSET             , ftFloat , 0                                    );
  xVersion.AddField( STR_TIPTYPES_FLD_ZOFFSET             , ftFloat , 0                                    );
  xVersion.AddField( STR_TIPTYPES_FLD_ZOFFSETREMOVABLE    , ftFloat , 0                                    );
  xVersion.AddField( STR_TIPTYPES_FLD_ZOFFSETWASH         , ftFloat , 0                                    );
  xVersion.AddField( STR_TIPTYPES_FLD_ZOFFSETWASTE        , ftFloat , 0                                    );
  xVersion.AddField( STR_TIPTYPES_FLD_ZOFFSETDRY          , ftFloat , 0                                    );
  xVersion.AddField( STR_TIPTYPES_FLD_DRYAFTERFLUSH       , ftBoolean, 0                                   );
  xVersion.AddField( STR_TIPTYPES_FLD_METHODNAMEGETTIP    , ftString, INT_TIPTYPES_FLDLEN_METHODNAMEGETTIP );
  xVersion.AddField( STR_TIPTYPES_FLD_METHODNAMEPUTTIP    , ftString, INT_TIPTYPES_FLDLEN_METHODNAMEPUTTIP );
  xVersion.AddField( STR_TIPTYPES_FLD_DONOTINIT           , ftBoolean, 0                                   );

  xVersion.AddIndex( STR_TIPTYPES_INDEX_FIELDS );
  xVersion.CopyMatchingFields( [] );

  //02.02.07 pk Hack! please dont use TTableUpdateDataFuncItem in other dataadaptors
  xVersion.AddDataItem( TTableUpdateDataFuncItem.Create( UpdateVersion1 ) );
  end;
}
end.
