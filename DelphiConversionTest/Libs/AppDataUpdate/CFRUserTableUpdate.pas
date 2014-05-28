unit CFRUserTableUpdate;


interface


implementation


{
  procedure TCFR21UserTableUpdater.DefineVersions;
  var
  xVersion : TTableUpdateVersionExt;
  xVersionNumber : integer;
  begin
  // V 1
  xVersionNumber := 1;
  xVersion := TTableUpdateVersionExt.Create( xVersionNumber );
  self.AddVersionUpdate( xVersion );


  xVersion.AddField( STR_CFR21_USERS_FLD_USER_PK,    ftAutoInc, 0 );
  xVersion.AddField( STR_CFR21_USERS_FLD_NAME,       ftString,  INT_CFR21_USERS_FLDLEN_NAME  );
  xVersion.AddField( STR_CFR21_USERS_FLD_CODE,       ftString,  INT_CFR21_USERS_FLDLEN_CODE  );
  xVersion.AddField( STR_CFR21_USERS_FLD_LEVEL,      ftInteger, 0  );
  xVersion.AddField( STR_CFR21_USERS_FLD_DELETED    ,ftBoolean, 0  );
  xVersion.AddField( STR_CFR21_USERS_FLD_DESCRIPTION,ftString,  INT_CFR21_USERS_FLDLEN_DESCRIPTION  );

  xVersion.AddIndex( STR_CFR21_USERS_FLD_USER_PK );
  //    xVersion.AddIndex( STR_CFR21_USERS_INDEX_NAME +','+ STR_CFR21_USERS_FLD_NAME );

  xVersion.CopyMatchingFields( [] );
  end;
}
end.
