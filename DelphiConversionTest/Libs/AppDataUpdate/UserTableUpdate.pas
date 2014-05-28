unit UserTableUpdate;


interface


implementation


{
  procedure TUserTableUpdater.DefineVersions;
  var
  xVersion : TTableUpdateVersionExt;
  xVersionNumber : integer;
  begin
  // V 1
  xVersionNumber := 1;
  xVersion := TTableUpdateVersionExt.Create( xVersionNumber );
  self.AddVersionUpdate( xVersion );

  xVersion.AddField( STR_OLD_USERS_FLD_USER_PK          , ftAutoInc, 0 );
  xVersion.AddField( STR_OLD_USERS_FLD_NAME             , ftString, INT_OLD_USERS_FLDLEN_NAME  );
  xVersion.AddField( STR_OLD_USERS_FLD_PW               , ftString, INT_OLD_USERS_FLDLEN_PW  );
  xVersion.AddField( STR_OLD_USERS_FLD_ADMIN            , ftBoolean, 0  );

  xVersion.AddIndex( STR_OLD_USERS_FLD_USER_PK );

  xVersion.CopyMatchingFields( [] );

  //25.07.07 wl Hack! please dont use TTableUpdateDataFuncItem in other dataadaptors
  xVersion.AddDataItem( TTableUpdateDataFuncItem.Create( TUserTableUpdater.UpdateVersion1 ) );
  end;

  class procedure TUserTableUpdater.UpdateVersion1(aSender: TObject; const aDestTableDir: string);
  var
  xDA: TUserDataAdaptor;
  begin
  xDA := TUserDataAdaptor.Create;
  try
  xDA.SelectAndOpen( Format( 'SELECT * FROM "%s%s"', [ aDestTableDir, STR_OLD_USERS_NAME ] ), false );
  xDA.Dataset.Append;
  xDA.Dataset.FieldByName(STR_OLD_USERS_FLD_NAME).AsString := 'Administrator';
  xDA.Dataset.FieldByName(STR_OLD_USERS_FLD_PW).AsString := '';
  xDA.Dataset.FieldByName(STR_OLD_USERS_FLD_ADMIN).AsBoolean := true;
  xDA.Dataset.Post;
  xDA.Dataset.Close;
  finally
  xDA.Free;
  end;
  end;
}
end.
