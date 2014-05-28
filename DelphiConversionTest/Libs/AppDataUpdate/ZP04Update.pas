unit ZP04Update;
{ ------------------------------------------------------------------------------------------------------------
  Copyright  2009 Zinsser Analytic GmbH - All rights reserved.
  Author       : Wolfgang Lyncke (wl)
  Description  : Hilfe für die Trennung von ZP04 und LinearMotor01
  ------------------------------------------------------------------------------------------------------------
  Revision History:
  date     op  method                            track-no improvement/change
  -------- --  --------------------------------  -------- ----------------------------------------------------
  20.01.09 wl                                    TN4358   initial revision
  15.02.11 pk                             	     TN4780   changes needed to make UpdateManager compatible with TurboDB
  13.09.11 wl  MoveRobotDB                       TN5672   deaktiviert (greift direkt auf BDE zu)
  ------------------------------------------------------------------------------------------------------------ }


interface


uses
    Update,
    TableUpdate,
    DataProvider;

type
    TSettingsTableUpdateV1_3 = class(TTableUpdate)
    private
        fSettingDP: TDataProvider;
        procedure Convert(aSender: TObject);
        procedure ConvertZP04Settings();
//        procedure MoveRobotDB();
    public
        constructor Create(aUpdateID: TUpdateID);
        destructor Destroy(); override;
    end;


implementation


uses
    Windows,
    SysUtils,
    Classes,
    StrUtils,
    SettingsTableUpdate;

{ TSettingsTableUpdateV1_3 }

constructor TSettingsTableUpdateV1_3.Create(aUpdateID: TUpdateID);
begin
    inherited Create(aUpdateID, TSettingsTableStructDefV1, INT_SETTINGS_MAJORREVISION_1,
        INT_SETTINGS_MINORREVISION_3);
    self.AlterStructure(TSettingsTableStructDefV1);
    CopyMatchingFields([]);
    self.CustomDataFunc(Convert);
end;

destructor TSettingsTableUpdateV1_3.Destroy;
begin

    inherited;
end;

procedure TSettingsTableUpdateV1_3.Convert(aSender: TObject);
begin
    fSettingDP := fTableChangeAdaptor.CreateDestDataProvider();
    try
        self.ConvertZP04Settings();
    finally
        fSettingDP.Free;
    end;
end;

procedure TSettingsTableUpdateV1_3.ConvertZP04Settings;
var
    xConnectionName, xZP04DriverName, xNameStart, xTrackConnectionName, xTrackDriverName, xIdentName: string;
begin
    // ZP04Connection suchen, umbenennen
    fSettingDP.SelectAndOpen('select * from settings s where ' + TSettingsSQL.ValueEq('s',
        'LOCATIONBASEDALLAXESCONNECTION'), false);
    try
        // Wenn es diesen Typ nicht gibt, ist das Update überflüssig!
        if fSettingDP.Eof then
            EXIT;

        xConnectionName := fSettingDP.FieldByName('Section').AsString;
        fSettingDP.Edit;
        fSettingDP.FieldByName('Value').AsString := 'ZP04CONNECTION';
        fSettingDP.Post;

        xNameStart := StringReplace(xConnectionName, 'Connection', '', [rfIgnoreCase]);
        xTrackConnectionName := xNameStart + 'TrackConnection';
        xTrackDriverName := xNameStart + 'TrackDriver';

        // Hinzufügen der neuen Module
        TSettingsTableProviderUtilsV0.AppendRecord(fSettingDP, 'CONNECTION', xTrackConnectionName, 'Type',
            'LINEARMOTOR01CONNECTION');
        TSettingsTableProviderUtilsV0.AppendRecord(fSettingDP, 'DRIVER', xTrackDriverName, 'Type',
            'LINEARMOTOR01DRIVER');
        TSettingsTableProviderUtilsV0.AppendRecord(fSettingDP, 'DRIVER', xTrackDriverName, 'Connection',
            xTrackConnectionName);
    finally
        fSettingDP.Close;
    end;

    fSettingDP.SelectAndOpen('select * from settings s where ' + TSettingsSQL.ValueEq('s',
        'LOCATIONBASEDALLAXES'), false);
    try
        if not fSettingDP.Eof then
        begin
            xZP04DriverName := fSettingDP.FieldByName('Section').AsString;
            fSettingDP.Edit;
            fSettingDP.FieldByName('Value').AsString := 'ZP04ALLAXES';
            fSettingDP.Post;

            TSettingsTableProviderUtilsV0.AppendRecord(fSettingDP, 'DRIVER', xZP04DriverName, 'TrackDriver',
                xTrackDriverName);
        end;
    finally
        fSettingDP.Close;
    end;

    // Einstellungen neu zuordnen
    fSettingDP.SelectAndOpen('select * from settings s where ' + TSettingsSQL.SectionEq('s',
        xConnectionName), false);
    try
        while not fSettingDP.Eof do
        begin
            xIdentName := fSettingDP.FieldByName('IDENT').AsString;
            fSettingDP.Edit;
            if (xIdentName = 'Robot-ControllerIPAddress') then
            begin
                fSettingDP.FieldByName('IDENT').AsString := 'ControllerIPAddress';
                fSettingDP.Post;
                fSettingDP.First;
            end
            else if (xIdentName = 'Robot-ExternalSpeed') then
            begin
                fSettingDP.FieldByName('IDENT').AsString := 'Speed';
                fSettingDP.Post;
                fSettingDP.First;
            end
            else if (xIdentName = 'Robot-Acceleration') then
            begin
                fSettingDP.FieldByName('IDENT').AsString := 'Acceleration';
                fSettingDP.Post;
                fSettingDP.First;
            end
            else if (xIdentName = 'Robot-Deceleration') then
            begin
                fSettingDP.FieldByName('IDENT').AsString := 'Deceleration';
                fSettingDP.Post;
                fSettingDP.First;
            end
            else if Pos('Track-', xIdentName) > 0 then
            begin
                fSettingDP.FieldByName('IDENT').AsString := Copy(xIdentName, 7, Length(xIdentName));
                fSettingDP.FieldByName('Section').AsString := xTrackConnectionName;
                fSettingDP.Post;
                fSettingDP.First;
            end
            else
            begin
                fSettingDP.Next;
            end;
        end;
    finally
        fSettingDP.Close;
    end;

    // Namensänderungen anderer Typen
    fSettingDP.ExecSQL('Update settings Set ' + TSettingsSQL.ValueEq('settings', 'ZP04SWITCH') + ' where ' +
        TSettingsSQL.ValueEq('settings', 'LOCATIONBASEDALLAXESSWITCH'));
    fSettingDP.ExecSQL('Update settings Set ' + TSettingsSQL.ValueEq('settings', 'ZP04SENSOR') + ' where ' +
        TSettingsSQL.ValueEq('settings', 'LOCATIONBASEDALLAXESSENSOR'));

//    self.MoveRobotDB();
end;

{
function GetAliasPath(const aAliasName: string): string;
var
    xAliasPath: string;
    xParams: TStringList;
    x: integer;
begin
    xAliasPath := '';
    xParams := TStringlist.Create;
    Session.GetAliasParams(aAliasName, xParams);
    for x := 0 to xParams.Count - 1 do
        if (Pos('PATH=', xParams[x]) > 0) then
            xAliasPath := Copy(xParams[x], 6, 255);
    xParams.Free;
    result := IncludeTrailingPathDelimiter(xAliasPath);
end;

procedure TSettingsTableUpdateV1_3.MoveRobotDB;
var
    xDbPath, xRobotPath: string;
    x: integer;
    xSearchRec: TSearchRec;
begin
    xDbPath := GetAliasPath('SAMINTF');

    xRobotPath := Copy(xDbPath, 0, LastDelimiter('\', LeftStr(xDbPath, Length(xDbPath) - 1)));
    xRobotPath := xRobotPath + 'RobotDB\';

    x := FindFirst(xRobotPath + '*.*', 0, xSearchRec);
    while (x = 0) do
    begin
        if (xSearchRec.Attr and faAnyFile > 0) then
        begin

            // Ich hätte die Dateien gern umbenannt, aber das geht nicht

            CopyFile(PChar(xRobotPath + xSearchRec.Name), PChar(xDbPath + xSearchRec.Name), true);
        end;
        x := FindNext(xSearchRec);
    end;
    SysUtils.FindClose(xSearchRec);
end;
}

end.
