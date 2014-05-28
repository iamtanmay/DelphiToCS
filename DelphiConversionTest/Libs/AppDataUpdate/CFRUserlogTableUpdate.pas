unit CFRUserlogTableUpdate;


interface


implementation


{
  procedure TCFR21UserlogTableUpdater.DefineVersions;
  var
  xVersion : TTableUpdateVersionExt;
  xVersionNumber : integer;
  begin
  // V 1
  xVersionNumber := 1;
  xVersion := TTableUpdateVersionExt.Create( xVersionNumber );
  self.AddVersionUpdate( xVersion );

  xVersion.AddField( STR_CFR21_USERLOG_FLD_USERLOG_PK, ftAutoInc, 0 );
  xVersion.AddField( STR_CFR21_USERLOG_FLD_USER_FK, ftInteger, 0 );
  xVersion.AddField( STR_CFR21_USERLOG_FLD_LOGTYPE, ftInteger, 0 );
  xVersion.AddField( STR_CFR21_USERLOG_FLD_ACTIONTYPE, ftInteger, 0 );
  xVersion.AddField( STR_CFR21_USERLOG_FLD_LOGTIME, ftDateTime, 0 );
  xVersion.AddField( STR_CFR21_USERLOG_FLD_TITLE,  ftString, INT_CFR21_USERLOG_FLDLEN_TITLE );
  xVersion.AddField( STR_CFR21_USERLOG_FLD_ACTION,  ftString, INT_CFR21_USERLOG_FLDLEN_ACTION );
  xVersion.AddField( STR_CFR21_USERLOG_FLD_REASON,  ftString, INT_CFR21_USERLOG_FLDLEN_REASON );

  xVersion.AddIndex( STR_CFR21_USERLOG_FLD_USERLOG_PK );

  xVersion.CopyMatchingFields( [] );
  end;
}
end.
