unit MethImportDefsTableUpdate;


interface


implementation


{
  procedure TMethImportDefsTableUpdater.DefineVersions();
  var
  xVersion : TTableUpdateVersionExt;
  xVersionNumber : integer;
  begin
  // V 1
  xVersionNumber := 1;
  xVersion := TTableUpdateVersionExt.Create( xVersionNumber );
  self.AddVersionUpdate( xVersion );
  xVersion.AddField( STR_METHIMPORTDEFS_FLD_KEYNAME   , ftString, INT_METHIMPORTDEFS_FLDLEN_KEYNAME );
  xVersion.AddField( STR_METHIMPORTDEFS_FLD_ACTION    , ftString, INT_METHIMPORTDEFS_FLDLEN_ACTION  );
  xVersion.AddField( STR_METHIMPORTDEFS_FLD_OPTIONS   , ftMemo,   0                                 );
  xVersion.AddField( STR_METHIMPORTDEFS_FLD_COMMENT   , ftMemo,   0                                 );
  xVersion.AddField( STR_METHIMPORTDEFS_FLD_SCHEDMIN  , ftString, INT_METHIMPORTDEFS_FLDLEN_SCHEDMIN );
  xVersion.AddField( STR_METHIMPORTDEFS_FLD_SCHEDMAX  , ftString, INT_METHIMPORTDEFS_FLDLEN_SCHEDMAX );
  xVersion.AddField( STR_METHIMPORTDEFS_FLD_RESID     , ftString, INT_METHIMPORTDEFS_FLDLEN_RESID    );

  xVersion.AddIndex( STR_METHIMPORTDEFS_INDEX_FIELDS );
  xVersion.CopyMatchingFields( [] );

  end;
}
end.
