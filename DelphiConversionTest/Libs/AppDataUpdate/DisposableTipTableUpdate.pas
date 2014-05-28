{ -----------------------------------------------------------------------------------------------------------
  Copyright © 2011 Zinsser Analytic GmbH - All rights reserved.
  Author       : Wolfgang Lyncke (wl)
  Description  :
  -----------------------------------------------------------------------------------------------------------
  Revision History:
  date     op  method                              track-no improvement/change
  -------- --  ----------------------------------  -------- -------------------------------------------------
  28.10.11 wl                                      TN5728   Initial Revision
  ----------------------------------------------------------------------------------------------------------- }

unit DisposableTipTableUpdate;


interface


uses
    Update,
    TableStructDef,
    TableUpdate;

type
    // table structure definitions
    TDisposableTipTableStructDefV0 = class(TTableStructDef)
    protected
        procedure DoDefineStruct(); override;
    end;

    TDisposableTipTableStructDefV1 = class(TDisposableTipTableStructDefV0)
    protected
        procedure DoDefineStruct(); override;
    end;

    // table updates
    TDisposableTipTableUpdateV1 = class(TTableUpdate)
    strict private
        procedure MoveDataFromPosinfo(aSender: TObject);
    public
        constructor Create(aUpdateNumber: TUpdateID);
    end;


implementation


uses
    SysUtils,
    DataProvider;

const
    INT_REVISION_1 = 1;

    { TDisposableTipTableStructDefV0 }

procedure TDisposableTipTableStructDefV0.DoDefineStruct;
begin
    inherited;
    fName := 'DISPOSABLETIP';
end;

{ TDisposableTipTableStructDefV1 }

procedure TDisposableTipTableStructDefV1.DoDefineStruct();
begin
    inherited;
    AddField('RACKID', tftString, 20);
    AddField('POS', tftInteger, 0);
    AddIndex('RACKID;POS');
end;

constructor TDisposableTipTableUpdateV1.Create(aUpdateNumber: TUpdateID);
begin
    inherited Create(aUpdateNumber, TDisposableTipTableStructDefV0, INT_REVISION_1);
    AlterStructure(TDisposableTipTableStructDefV1);
    CopyMatchingFields([]);

    self.CustomDataFunc(MoveDataFromPosinfo);
end;

procedure TDisposableTipTableUpdateV1.MoveDataFromPosinfo(aSender: TObject);
var
    xPosinfoDP, xDitiDP: TDataProvider;
begin
    xPosinfoDP := fTableChangeAdaptor.CreateSourceDataProvider();
    try
        xPosinfoDP.SelectAndOpen('select * from POSINFO where posinfo.step is null', false);

        // Aus allen existierenden SQL-Dateien werden Datensätze generiert!
        xDitiDP := fTableChangeAdaptor.CreateDestDataProvider();
        try
            xDitiDP.SelectAndOpen('SELECT * FROM DISPOSABLETIP', false);

            while (not xPosinfoDP.Eof) do
            begin
                xDitiDP.Append;
                xDitiDP.FieldByName('RACKID').AsString := xPosinfoDP.FieldByName('RACKID').AsString;
                xDitiDP.FieldByName('POS').AsInteger := xPosinfoDP.FieldByName('POS').AsInteger;
                xDitiDP.Post;

                xPosinfoDP.Next;
            end;

            xDitiDP.Close;
        finally
            xDitiDP.Free;
        end;

        xPosinfoDP.Close;
    finally
        xPosinfoDP.Free;
    end;
end;


end.
