{ -----------------------------------------------------------------------------------------------------------
  Copyright © 2011 Zinsser Analytic GmbH - All rights reserved.
  Author       : Wolfgang Lyncke (wl)
  Description  :
  -----------------------------------------------------------------------------------------------------------
  Revision History:
  date     op  method                              track-no improvement/change
  -------- --  ----------------------------------  -------- -------------------------------------------------
  13.11.12 wl                                      TN6015   Initial Revision
  13.11.12 wl  TDisplayComponentsTableUpdateV1     TN6015   neu: DisplayComponents von Settings trennen
  ----------------------------------------------------------------------------------------------------------- }

unit DisplayComponentsTableUpdate;


interface


uses
    Update,
    TableStructDef,
    TableUpdate;

type
    // table structure definitions
    TDisplayComponentsTableStructDefV0 = class(TTableStructDef)
    protected
        procedure DoDefineStruct(); override;
    end;

    TDisplayComponentsTableStructDefV1 = class(TDisplayComponentsTableStructDefV0)
    protected
        procedure DoDefineStruct(); override;
    end;

    // table updates
    TDisplayComponentsTableUpdateV1 = class(TTableUpdate)
    strict private
        procedure MoveDataFromSettings(aSender: TObject);
    public
        constructor Create(aUpdateNumber: TUpdateID);
    end;


implementation


uses
    SysUtils,
    DataProvider;

const
    INT_REVISION_1 = 1;
    cFieldNameName = 'NAME';
    cFieldNameIdent = 'IDENT';
    cFieldNameValue = 'VALUE';

    { TDisplayComponentsTableStructDefV0 }

procedure TDisplayComponentsTableStructDefV0.DoDefineStruct;
begin
    inherited;
    fName := 'DISPLAYCOMPONENTS';
end;

{ TDisplayComponentsTableStructDefV1 }

procedure TDisplayComponentsTableStructDefV1.DoDefineStruct();
const
    cFieldLengthName = 80;
    cFieldLengthIdent = 60;
    cFieldLengthValue = 200;
begin
    inherited;
    AddField(cFieldNameName, tftString, cFieldLengthName);
    AddField(cFieldNameIdent, tftString, cFieldLengthIdent);
    AddField(cFieldNameValue, tftString, cFieldLengthValue);
    AddIndex(cFieldNameName + ';' + cFieldNameIdent);
end;

constructor TDisplayComponentsTableUpdateV1.Create(aUpdateNumber: TUpdateID);
begin
    inherited Create(aUpdateNumber, TDisplayComponentsTableStructDefV0, INT_REVISION_1);
    AlterStructure(TDisplayComponentsTableStructDefV1);
    CopyMatchingFields([]);

    self.CustomDataFunc(MoveDataFromSettings);
end;

procedure TDisplayComponentsTableUpdateV1.MoveDataFromSettings(aSender: TObject);
var
    xSourceDP, xDestDP: TDataProvider;
begin
    xSourceDP := fTableChangeAdaptor.CreateSourceDataProvider();
    try
        xSourceDP.SelectAndOpen('select * from SETTINGS where [AREA]=''DISPLAYCOMPONENTS''', false);

        // Aus allen existierenden SQL-Dateien werden Datensätze generiert!
        xDestDP := fTableChangeAdaptor.CreateDestDataProvider();
        try
            xDestDP.SelectAndOpen('SELECT * FROM DisplayComponents', false);

            while (not xSourceDP.Eof) do
            begin
                xDestDP.Append;
                xDestDP.FieldByName(cFieldNameName).AsString := xSourceDP.FieldByName('SECTION').AsString;
                xDestDP.FieldByName(cFieldNameIdent).AsString := xSourceDP.FieldByName('IDENT').AsString;
                xDestDP.FieldByName(cFieldNameValue).AsString := xSourceDP.FieldByName('VALUE').AsString;
                xDestDP.Post;

                xSourceDP.Next;
            end;

            xDestDP.Close;
        finally
            xDestDP.Free;
        end;

        xSourceDP.Close;
    finally
        xSourceDP.Free;
    end;
end;


end.
