unit WashprogTableUpdate;
{ --------------------------------------------------------------------------------------------------
  Copyright © 2010 Zinsser Analytic GmbH - All rights reserved.
  Author       : Wolfgang Lyncke (wl)
  Description  : Updater für Tabelle WASHPROGRAM
  --------------------------------------------------------------------------------------------------
  Revision History:
  date     op  method                       track-no improvement/change
  -------- --  ---------------------------  -------- -----------------------------------------------
  20.01.11 wl                               TN5439   initial revision
  15.02.11 pk                               TN4780   changes needed to make UpdateManager compatible with TurboDB
  07.01.13 ts                               TN6065   TWashProgramTableUpdateV2: neues Feld: CleanHeight
  -------------------------------------------------------------------------------------------------- }


interface


uses
    Types,
    Update,
    TableStructDef,
    TableUpdate;

type
    // table structure definitions
    TWashprogTableStructDefV0 = class(TTableStructDef)
    protected
        procedure DoDefineStruct(); override;
    end;

    TWashprogTableStructDefV1 = class(TWashprogTableStructDefV0)
    protected
        procedure DoDefineStruct(); override;
    end;

    TWashprogTableStructDefV2 = class(TWashprogTableStructDefV1)
    protected
        procedure DoDefineStruct(); override;
    end;

    // table updates
    TWashprogTableUpdateV1 = class(TTableUpdate)
    strict private
        procedure Washprog_ConvertFromOldTable(aSender: TObject);
    public
        constructor Create(aUpdateNumber: TUpdateID);
    end;

    TWashprogTableUpdateV2 = class(TTableUpdate)
    public
        constructor Create(aUpdateNumber: TUpdateID);
    end;


implementation


uses
    SysUtils,
    Classes,
    IOUtils,
    Generics.Collections,
    DataProvider;

const
    INT_REVISION_1 = 1;
    INT_REVISION_2 = 2;

    { TWashprogTableStructDefV0 }

procedure TWashprogTableStructDefV0.DoDefineStruct;
begin
    inherited;

    fName := 'WASHPROGRAM';
end;

{ TWashprogTableStructDefV1 }

procedure TWashprogTableStructDefV1.DoDefineStruct();
begin
    inherited;
    self.AddField('WPNAME', tftString, 30);
    self.AddField('DILUENT', tftString, 20);
    self.AddField('INERTGAS', tftBoolean, 0);
    self.AddField('RESINHEIGHT', tftFloat, 0);
    self.AddField('INSERTSPEED', tftInteger, 0);
    self.AddField('INSERTDELAY', tftFloat, 0);
    self.AddField('ASPDISPVOL', tftFloat, 0);
    self.AddField('ASPDISPSPEED', tftInteger, 0);
    self.AddField('ASPDISPDELAY', tftFloat, 0);
    self.AddField('N2TIME1', tftFloat, 0);
    self.AddField('N2TIME2', tftFloat, 0);
    self.AddField('DISPHEIGHTPOS', tftInteger, 0);
    self.AddField('SINGLERETRACT', tftBoolean, 0);
    self.AddField('REPEATSTEP1TO3', tftInteger, 0);
    self.AddField('CLEANVOLUME', tftFloat, 0);
    self.AddField('CLEANDISPSPEED', tftInteger, 0);
    self.AddField('CLEANMODE', tftInteger, 0);
    self.AddField('CLEANDELAY', tftFloat, 0);
    self.AddField('INNERWASHVOLUME', tftFloat, 0);
    self.AddField('INNERWASHN2TIME', tftFloat, 0);
    self.AddField('OUTERWASHVOLUME', tftFloat, 0);

    self.AddIndex('WPNAME');
end;

{ TWashprogTableStructDefV2 }

procedure TWashprogTableStructDefV2.DoDefineStruct;
begin
    inherited;
    AddField('CLEANHEIGHT', tftInteger, 0);
end;

{ TWashprogTableUpdateV1 }

constructor TWashprogTableUpdateV1.Create(aUpdateNumber: TUpdateID);
begin
    inherited Create(aUpdateNumber, TWashprogTableStructDefV0, INT_REVISION_1);
    AlterStructure(TWashprogTableStructDefV1);
    CopyMatchingFields([]);

    self.CustomDataFunc(Washprog_ConvertFromOldTable);
end;

procedure TWashprogTableUpdateV1.Washprog_ConvertFromOldTable(aSender: TObject);
const
    STR_TIPTYPEENTRY_DELIMITER = ',';
var
    xDP, xOldDP: TDataProvider;
    xCleanDelay: integer;
    xWPName, xLastWPName: string;
begin
    xOldDP := fTableChangeAdaptor.CreateSourceDataProvider();
    try
        xOldDP.SelectAndOpen
            ('select * from SETTINGS s where s.AREA = ''APPLICATION'' and s.SECTION = ''Washprogram''' +
            ' and s.IDENT = ''DelayBefore2ndBlowN2''', true);
        if xOldDP.Eof then
            xCleanDelay := 0
        else
            xCleanDelay := xOldDP.FieldByName('VALUE').AsInteger;

        xOldDP.Close;
    finally
        xOldDP.Free;
    end;

    // Aus allen existierenden SQL-Dateien werden Datensätze generiert!
    xOldDP := fTableChangeAdaptor.CreateSourceDataProvider();
    try
        try
            xOldDP.SelectAndOpen('select * from WASHPROG', true);
        except
            EXIT; // Tabelle existiert nicht -> raus
        end;

        // Aus allen existierenden SQL-Dateien werden Datensätze generiert!
        xDP := fTableChangeAdaptor.CreateDestDataProvider();
        try
            xDP.SelectAndOpen('SELECT * FROM Washprogram', false);

            // Exit if the table is not empty.  It means that this update was already done in a previous version where versioning did not exist yet
            if xDP.RecordCount > 0 then
                EXIT;

            xLastWPName := '';
            while (not xOldDP.Eof) do
            begin

                xDP.Append;

                xWPName := xOldDP.FieldByName('WASHPROGRAM').AsString;
                if (xLastWPName = xWPName) then // Weitere Schritte müssen umbenannt werden, sonst Fehler
                    xWPName := xWPName + '_Step' + xOldDP.FieldByName('STEP').AsString;
                xLastWPName := xOldDP.FieldByName('WASHPROGRAM').AsString;
                xDP.FieldByName('WPNAME').AsString := xWPName;

                xDP.FieldByName('DILUENT').AsString := xOldDP.FieldByName('DILUENT').AsString;
                xDP.FieldByName('INERTGAS').AsBoolean := xOldDP.FieldByName('INERT_GAS').AsBoolean;

                xDP.FieldByName('RESINHEIGHT').AsFloat := xOldDP.FieldByName('RESINHEIGHT').AsFloat;
                xDP.FieldByName('INSERTSPEED').AsInteger := xOldDP.FieldByName('DIP_SPEED').AsInteger;
                xDP.FieldByName('INSERTDELAY').AsFloat := xOldDP.FieldByName('ASPTIME').AsFloat;

                xDP.FieldByName('ASPDISPVOL').AsFloat := xOldDP.FieldByName('ASPDISPVOL').AsFloat;
                xDP.FieldByName('ASPDISPSPEED').AsInteger := xOldDP.FieldByName('ASPDISPSPEED').AsInteger;
                xDP.FieldByName('ASPDISPDELAY').AsFloat := xOldDP.FieldByName('ASPTIME').AsFloat;

                xDP.FieldByName('N2TIME1').AsFloat := xOldDP.FieldByName('N2_TIME').AsFloat;
                xDP.FieldByName('N2TIME2').AsFloat := xOldDP.FieldByName('N2_TIME').AsFloat;
                // nicht erwähnt: 'DISPHEIGHTPOS'
                xDP.FieldByName('SINGLERETRACT').AsBoolean := xOldDP.FieldByName('SINGLERETRACT').AsBoolean;
                // nicht erwähnt: 'REPEATSTEP1TO3'

                xDP.FieldByName('CLEANVOLUME').AsFloat := xOldDP.FieldByName('CLEAN_VOLUME').AsFloat;
                xDP.FieldByName('CLEANDISPSPEED').AsInteger := xOldDP.FieldByName('CLEAN_DISPENSE_SPEED')
                    .AsInteger;
                xDP.FieldByName('CLEANMODE').AsInteger := 0;
                xDP.FieldByName('CLEANDELAY').AsFloat := xCleanDelay / 1000;

                xDP.FieldByName('INNERWASHVOLUME').AsFloat :=
                    xOldDP.FieldByName('WASHSTATION_INNER_VOLUME').AsFloat;
                xDP.FieldByName('INNERWASHN2TIME').AsFloat := 2;
                xDP.FieldByName('OUTERWASHVOLUME').AsFloat :=
                    xOldDP.FieldByName('WASHSTATION_OUTER_VOLUME').AsFloat;

                xDP.Post;

                xOldDP.Next;
            end;

            xDP.Close;
        finally
            xDP.Free;
        end;
    finally
        xOldDP.Free;
    end;
end;

{ TWashprogTableUpdateV2 }

constructor TWashprogTableUpdateV2.Create(aUpdateNumber: TUpdateID);
begin
    inherited Create(aUpdateNumber, TWashprogTableStructDefV0, INT_REVISION_2);
    AlterStructure(TWashprogTableStructDefV2);
    CopyMatchingFields([]);
end;


end.
