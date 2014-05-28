{ --------------------------------------------------------------------------------------------------
  Copyright © 2008 Zinsser Analytic GmbH - All rights reserved.
  Author       : Payman Kamali (pk)
  Description  :
  --------------------------------------------------------------------------------------------------
  Revision History:
  date     op  method                       track-no improvement/change
  -------- --  ---------------------------  -------- -----------------------------------------------
  30.11.10 wl                               TN5373   initial revision
  15.02.11 pk                               TN4780   changes needed to make UpdateManager compatible with TurboDB
  24.08.11 wl  TSQLTermsTableStructDefV2    TN5663   Neu: AliasName  (wird noch nicht verwendet)
  -------------------------------------------------------------------------------------------------- }

unit SQLTermsTableUpdate;


interface


uses
    Types,
    Update,
    TableStructDef,
    TableUpdate;

type
    // table structure definitions
    TSQLTermsTableStructDefV0 = class(TTableStructDef)
    protected
        procedure DoDefineStruct(); override;
    end;

    TSQLTermsTableStructDefV1 = class(TSQLTermsTableStructDefV0)
    protected
        procedure DoDefineStruct(); override;
    end;

    TSQLTermsTableStructDefV2 = class(TSQLTermsTableStructDefV1)
    protected
        procedure DoDefineStruct(); override;
    end;

    // table updates
    TSQLTermsTableUpdateV1 = class(TTableUpdate)
    strict private
        procedure RemoveZinsserStyleRemarks(const aTerms: TStringDynArray; out oSQLTerms, oComments: string);
        procedure SQLTerms_ConvertDataFromSqlFiles(aSender: TObject);
    public
        constructor Create(aUpdateNumber: TUpdateID);
    end;

    // table updates
    TSQLTermsTableUpdateV2 = class(TTableUpdate)
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

    { TSQLTermsTableStructDefV0 }

procedure TSQLTermsTableStructDefV0.DoDefineStruct;
begin
    inherited;

    fName := 'SQLTERMS';
end;

{ TSQLTermsTableStructDefV1 }

procedure TSQLTermsTableStructDefV1.DoDefineStruct();
begin
    inherited;
    self.AddField('NAME', tftString, 50);
    self.AddField('TERM', tftMemo, 0);
    self.AddField('COMMENT', tftMemo, 0);
    self.AddField('PARAMSDEFAULT', tftMemo, 0);

    self.AddIndex('NAME');
end;

{ TSQLTermsTableStructDefV2 }

procedure TSQLTermsTableStructDefV2.DoDefineStruct();
begin
    inherited;
    self.AddField('ALIASNAME', tftString, 50);
end;

{ TSQLTermsTableUpdateV1 }

constructor TSQLTermsTableUpdateV1.Create(aUpdateNumber: TUpdateID);
begin
    inherited Create(aUpdateNumber, TSQLTermsTableStructDefV0, INT_REVISION_1);
    AlterStructure(TSQLTermsTableStructDefV1);
    CopyMatchingFields([]);

    self.CustomDataFunc(SQLTerms_ConvertDataFromSqlFiles);
end;

procedure TSQLTermsTableUpdateV1.RemoveZinsserStyleRemarks(const aTerms: TStringDynArray;
    out oSQLTerms, oComments: string);
var
    xTerm: string;
begin
    for xTerm in aTerms do
    begin

        // Zinsser-Style-Kommentare in Comment-Feld schreiben!
        // -> Diese Art der Kommentare wird nicht mehr verwendet
        if (POS('//', Trim(xTerm)) = 1) then
        begin

            if (oComments = '') then
                oComments := Trim(Copy(xTerm, 3, Length(xTerm)))
            else
                oComments := oComments + #13#10 + Trim(Copy(xTerm, 3, Length(xTerm)));
        end
        else
        begin
            if (oSQLTerms = '') then
                oSQLTerms := xTerm
            else
                oSQLTerms := oSQLTerms + #13#10 + xTerm;
        end;
    end;
end;

procedure TSQLTermsTableUpdateV1.SQLTerms_ConvertDataFromSqlFiles(aSender: TObject);
var
    xDP: TDataProvider;
    x: Integer;
    xSQLTerms, xFiles: TStringDynArray;
    xName: string;
    xTerm: string;
    xComment: string;
begin
    // Aus allen existierenden SQL-Dateien werden Datensätze generiert!
    xDP := fTableChangeAdaptor.CreateDestDataProvider();
    try
        xDP.SelectAndOpen('SELECT * FROM SQLTERMS', false);

        // Exit if the table is not empty.  It means that this update was already done in a previous version where versioning did not exist yet
        if xDP.RecordCount > 0 then
            EXIT;

        xFiles := TDirectory.GetFiles(TUpdatePaths.Instance.DataPath);
        for x := 0 to Length(xFiles) - 1 do
        begin
            if (UpperCase(ExtractFileExt(xFiles[x])) = '.SQL') then
            begin

                xName := TPath.GetFileNameWithoutExtension(xFiles[x]);
                xSQLTerms := TFile.ReadAllLines(xFiles[x]);

                RemoveZinsserStyleRemarks(xSQLTerms, xTerm, xComment);

                xDP.Append;
                xDP.FieldByName('NAME').AsString := xName;
                xDP.FieldByName('TERM').AsString := xTerm;
                xDP.FieldByName('COMMENT').AsString := xComment;
                xDP.Post;
            end;
        end;

        xDP.Close();
    finally
        xDP.Free;
    end;
end;

{ TSQLTermsTableUpdateV2 }

constructor TSQLTermsTableUpdateV2.Create(aUpdateNumber: TUpdateID);
begin
    inherited Create(aUpdateNumber, TSQLTermsTableStructDefV1, INT_REVISION_2);
    AlterStructure(TSQLTermsTableStructDefV2);
    CopyMatchingFields([]);
end;


end.
