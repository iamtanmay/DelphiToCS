{ -----------------------------------------------------------------------------------------------------------
  Copyright © 2011 Zinsser Analytic GmbH - All rights reserved.
  Author       : Wolfgang Lyncke (wl)
  Description  :
  -----------------------------------------------------------------------------------------------------------
  Revision History:
  date     op  method                              track-no improvement/change
  -------- --  ----------------------------------  -------- -------------------------------------------------
  28.10.11 wl                                      TN5729   Initial Revision
  31.10.11 wl  TSubstanceSetTableStructDefV1       TN5725   SUBSTCOLOR -> SubstanceData
  17.11.11 wl                                      TN5725   UNIT und DESCRIPTION weg
  26.11.11 wl                                      TN5725   MinVolume,MaxVolume: neu, werden aber noch nicht verwendet
  10.09.12 wl  TSubstanceSetTableUpdateV2          TN5979   neu: MinVolume2
  ----------------------------------------------------------------------------------------------------------- }

unit SubstanceSetTableUpdate;


interface


uses
    Update,
    TableStructDef,
    TableUpdate;

type
    // table structure definitions
    TSubstanceSetTableStructDefV0 = class(TTableStructDef)
    protected
        procedure DoDefineStruct(); override;
    end;

    TSubstanceSetTableStructDefV1 = class(TSubstanceSetTableStructDefV0)
    protected
        procedure DoDefineStruct(); override;
    end;

    TSubstanceSetTableStructDefV2 = class(TSubstanceSetTableStructDefV1)
    protected
        procedure DoDefineStruct(); override;
    end;

    // table updates
    TSubstanceSetTableUpdateV1 = class(TTableUpdate)
    strict private
        procedure MoveDataFromPosinfo(aSender: TObject);
    public
        constructor Create(aUpdateNumber: TUpdateID);
    end;

    TSubstanceSetTableUpdateV2 = class(TTableUpdate)
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

    { TSubstanceSetTableStructDefV0 }

procedure TSubstanceSetTableStructDefV0.DoDefineStruct;
begin
    inherited;
    fName := 'SUBSTANCESET';
end;

{ TSubstanceSetTableStructDefV1 }

procedure TSubstanceSetTableStructDefV1.DoDefineStruct();
begin
    inherited;
    AddField('SETNAME', tftString, 20);
    AddField('RACKID', tftString, 20);
    AddField('POS', tftInteger, 0);
    AddField('SUBSTID', tftString, 40);
    AddField('AMOUNT', tftFloat, 0);
    AddField('LASTCHANGE', tftDateTime, 0);
    AddField('LASTAMOUNT', tftFloat, 0);
    AddField('MINVOLUME', tftFloat, 0);
    AddField('MAXVOLUME', tftFloat, 0);
    AddIndex('SETNAME;RACKID;POS');
end;

{ TSubstanceSetTableStructDefV2 }

procedure TSubstanceSetTableStructDefV2.DoDefineStruct();
begin
    inherited;
    AddField('MINVOLUME2', tftFloat, 0);
end;

{ TSubstanceSetTableUpdateV1 }

constructor TSubstanceSetTableUpdateV1.Create(aUpdateNumber: TUpdateID);
begin
    inherited Create(aUpdateNumber, TSubstanceSetTableStructDefV0, INT_REVISION_1);
    AlterStructure(TSubstanceSetTableStructDefV1);
    CopyMatchingFields([]);

    self.CustomDataFunc(MoveDataFromPosinfo);
end;

procedure TSubstanceSetTableUpdateV1.MoveDataFromPosinfo(aSender: TObject);
var
    xPosinfoDP, xSubstSetDP: TDataProvider;
begin
    xPosinfoDP := fTableChangeAdaptor.CreateSourceDataProvider();
    try
        xPosinfoDP.SelectAndOpen('select * from POSINFO where posinfo.step = 0', false);

        // Aus allen existierenden SQL-Dateien werden Datensätze generiert!
        xSubstSetDP := fTableChangeAdaptor.CreateDestDataProvider();
        try
            xSubstSetDP.SelectAndOpen('SELECT * FROM SubstanceSet', false);

            while (not xPosinfoDP.Eof) do
            begin
                xSubstSetDP.Append;
                xSubstSetDP.FieldByName('SETNAME').AsString := 'AUTOLOAD';
                // die Autoload-Reagenzien werden immer geladen
                xSubstSetDP.FieldByName('RACKID').AsString := xPosinfoDP.FieldByName('RACKID').AsString;
                xSubstSetDP.FieldByName('POS').AsInteger := xPosinfoDP.FieldByName('POS').AsInteger;
                xSubstSetDP.FieldByName('AMOUNT').AsFloat := xPosinfoDP.FieldByName('AMOUNT').AsFloat;
                xSubstSetDP.FieldByName('SUBSTID').AsString := xPosinfoDP.FieldByName('SUBSTID').AsString;
                xSubstSetDP.Post;

                xPosinfoDP.Next;
            end;

            xSubstSetDP.Close;
        finally
            xSubstSetDP.Free;
        end;

        xPosinfoDP.Close;
    finally
        xPosinfoDP.Free;
    end;
end;

{ TSubstanceSetTableUpdateV2 }

constructor TSubstanceSetTableUpdateV2.Create(aUpdateNumber: TUpdateID);
begin
    inherited Create(aUpdateNumber, TSubstanceSetTableStructDefV1, INT_REVISION_2);

    AlterStructure(TSubstanceSetTableStructDefV2);
    CopyMatchingFields([]);
end;


end.
