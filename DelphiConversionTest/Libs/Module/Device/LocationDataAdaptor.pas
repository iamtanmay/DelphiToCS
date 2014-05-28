unit LocationDataAdaptor;
{ ------------------------------------------------------------------------------------------------------------
  Copyright  2009 Zinsser Analytic GmbH - All rights reserved.
  Author       : Wolfgang Lyncke (wl)
  Description  : Data Adaptor used by TExternLocationBasedMotionDevice
  ------------------------------------------------------------------------------------------------------------
  Revision History:
  date     op  method                            track-no improvement/change
  -------- --  --------------------------------  -------- ----------------------------------------------------
  20.01.09 wl                                    TN4358   initial revision
  20.01.09 wl                                    TN4358   benutzt jetzt SAMINTF alias
  23.11.12 ts  ReadLocationRec                   TN6030   Anpassung an TurboDB
  ------------------------------------------------------------------------------------------------------------ }


interface


uses
    QueryDataAdaptor,
    IntfLocationBasedAllAxesDriver;

type
    TLocationDataAdaptor = class(TQueryDataAdaptor)
    protected
        function GetNameField(): string; override;
    public
        constructor Create();
        function ReadLocationRec(const aName: string): TLocationRec;
    end;


implementation


uses
    StrUtils,
    SysUtils,
    DataAdaptor;

const
    cRobotRailMasterTable = 'RobotRailMaster';

    cRRMasterFieldID = 'ID';
    cRRMasterFieldName = 'NAME';
    cRRMasterFieldSpeed = 'SPEED';
    cRRMasterFieldAcceleration = 'ACCELERATION';
    cRRMasterFieldDeceleration = 'DECELERATION';
    cRRMasterFieldSimultaneuos = 'SIMULTANEOUSARMTRACK';

    cRobotRailDetailTable = 'RobotRailJointsDetails';

    cRRDetailFieldID = 'ID';
    cRRDetailFieldIndex = 'INDEX';
    cRRDetailFieldMoveType = 'MOVEMENTTYPE';
    cRRDetailFieldInterpolationType = 'INTERPOLATIONTYPE';
    cRRDetailFieldJ1 = 'J1';
    cRRDetailFieldJ2 = 'J2';
    cRRDetailFieldJ3 = 'J3';
    cRRDetailFieldJ4 = 'J4';
    cRRDetailFieldJ5 = 'J5';
    cRRDetailFieldJ6 = 'J6';
    cRRDetailFieldJ7 = 'J7';
    cRRDetailFieldXOffset = 'XOFFSET';
    cRRDetailFieldZOffset = 'ZOFFSET';

    { TLocationDataAdaptor }

constructor TLocationDataAdaptor.Create;
begin
    inherited Create(cRobotRailMasterTable);
end;

function TLocationDataAdaptor.GetNameField: string;
begin
    result := cRRMasterFieldName;
end;

function TLocationDataAdaptor.ReadLocationRec(const aName: string): TLocationRec;
var
    x: integer;
begin
    self.SelectAndOpenFmt('select * from %s m,%s d where m.ID = d.ID and %s = ''%s'' order by d.''%s''',
        [cRobotRailMasterTable, cRobotRailDetailTable, cRRMasterFieldName, aName, cRRDetailFieldIndex], true);
    try
        if self.DataProvider.Eof then
        begin
            result.Valid := false;
            EXIT;
        end;

        result.Valid := true;
        result.Name := self.DataProvider.FieldByName(cRRMasterFieldName).AsString;
        result.Speed := self.DataProvider.FieldByName(cRRMasterFieldSpeed).AsFloat;
        result.Acceleration := self.DataProvider.FieldByName(cRRMasterFieldAcceleration).AsFloat;
        result.Deceleration := self.DataProvider.FieldByName(cRRMasterFieldDeceleration).AsFloat;
        result.Simultaneuos := self.DataProvider.FieldByName(cRRMasterFieldSimultaneuos).AsBoolean;

        SetLength(result.Details, self.DataProvider.RecordCount);
        for x := 0 to self.DataProvider.RecordCount - 1 do
        begin
            result.Details[x].Index := self.DataProvider.FieldByName(cRRDetailFieldIndex).AsInteger;
            result.Details[x].MoveType := self.DataProvider.FieldByName(cRRDetailFieldMoveType).AsString;
            result.Details[x].InterpolationType := self.DataProvider.FieldByName
                (cRRDetailFieldInterpolationType).AsInteger;
            result.Details[x].J1 := self.DataProvider.FieldByName(cRRDetailFieldJ1).AsFloat;
            result.Details[x].J2 := self.DataProvider.FieldByName(cRRDetailFieldJ2).AsFloat;
            result.Details[x].J3 := self.DataProvider.FieldByName(cRRDetailFieldJ3).AsFloat;
            result.Details[x].J4 := self.DataProvider.FieldByName(cRRDetailFieldJ4).AsFloat;
            result.Details[x].J5 := self.DataProvider.FieldByName(cRRDetailFieldJ5).AsFloat;
            result.Details[x].J6 := self.DataProvider.FieldByName(cRRDetailFieldJ6).AsFloat;
            result.Details[x].J7 := self.DataProvider.FieldByName(cRRDetailFieldJ7).AsFloat;
            result.Details[x].XOffset := self.DataProvider.FieldByName(cRRDetailFieldXOffset).AsFloat;
            result.Details[x].ZOffset := self.DataProvider.FieldByName(cRRDetailFieldZOffset).AsFloat;
            if self.DataProvider.Eof then
                EXIT;
            self.DataProvider.Next;
        end;
    finally
        self.Close;
    end;
end;


end.
