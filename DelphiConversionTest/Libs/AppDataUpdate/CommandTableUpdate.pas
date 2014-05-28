unit CommandTableUpdate;


interface


implementation


{
  procedure TCommandTableUpdater.DefineVersions();
  var
  xVersion : TTableUpdateVersionExt;
  xVersionNumber : integer;
  begin
  // V 1
  xVersionNumber := 1;
  xVersion := TTableUpdateVersionExt.Create( xVersionNumber );
  self.AddVersionUpdate( xVersion );

  xVersion.AddField( STR_COMMAND_FLD_NAME, ftString, INT_COMMAND_FLDLEN_NAME );
  xVersion.AddField( STR_COMMAND_FLD_Step, ftInteger, 0 );
  xVersion.AddField( STR_COMMAND_FLD_Adr, ftInteger, 0 );
  xVersion.AddField( STR_COMMAND_FLD_Command, ftString, INT_COMMAND_FLDLEN_COMMAND );
  xVersion.AddField( STR_COMMAND_FLD_Execute, ftBoolean, 0 );
  xVersion.AddField( STR_COMMAND_FLD_Flag, ftString, INT_COMMAND_FLDLEN_FLAG );
  xVersion.AddField( STR_COMMAND_FLD_Remark, ftString, INT_COMMAND_FLDLEN_REMARK );
  xVersion.AddIndex( STR_COMMAND_INDEX_FIELDS );

  xVersion.CopyMatchingFields( [] );

  end;
}
end.
