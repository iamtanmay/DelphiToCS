{ --------------------------------------------------------------------------------------------------
  Copyright © 2008 Zinsser Analytic GmbH - All rights reserved.
  Author       : Payman Kamali (pk)
  Description  :
  --------------------------------------------------------------------------------------------------
  Revision History:
  date     op  method                       track-no improvement/change
  -------- --  ---------------------------  -------- -----------------------------------------------
  24.06.08 pk                               TN4148   FieldNames unit removed
  19.12.11 wl  TImportColDefTableUpdateV1   TN5771  ändert Variablen-Namen in Import-Definitionen in lokale Variablen
  -------------------------------------------------------------------------------------------------- }

unit ImportColDefTableUpdate;


interface


uses
    Update,
    TableStructDef,
    TableUpdate;

type
    // table structure definitions
    TImportColDefTableStructDefV0 = class(TTableStructDef)
    protected
        procedure DoDefineStruct(); override;
    end;

    TImportColDefTableStructDefV1 = class(TImportColDefTableStructDefV0)
    protected
        procedure DoDefineStruct(); override;
    end;

    // table updates
    TImportColDefTableUpdateV1 = class(TTableUpdate)
    private
        procedure CustomFunc(aSender: TObject);
    public
        constructor Create(aUpdateNumber: TUpdateID);
    end;


implementation


uses
    SysUtils,
    DataProvider,
    MethodGUIParserConverter;

const
    INT_REVISION_1 = 1;

    { TImportColDefTableStructDefV0 }

procedure TImportColDefTableStructDefV0.DoDefineStruct;
begin
    inherited;
    fName := 'IMPCOLS';
end;

{ TImportColDefTableStructDefV1 }

procedure TImportColDefTableStructDefV1.DoDefineStruct();
begin
    inherited;
    AddField('IMPORTDEFNAME', tftString, 50);
    AddField('TARGETCOL', tftString, 50);
    AddField('SOURCECOL', tftString, 50);
    AddField('SOURCENOTREQUIRED', tftBoolean, 0);
    AddField('SOURCEDEFAULT', tftString, 50);
    AddIndex('IMPORTDEFNAME;TARGETCOL');
end;

{ TImportColDefTableUpdateV1 }

constructor TImportColDefTableUpdateV1.Create(aUpdateNumber: TUpdateID);
begin
    inherited Create(aUpdateNumber, TImportColDefTableStructDefV1, INT_REVISION_1);
    AlterStructure(TImportColDefTableStructDefV1);
    CopyMatchingFields([]);

    self.CustomDataFunc(CustomFunc);
end;

procedure TImportColDefTableUpdateV1.CustomFunc(aSender: TObject);
var
    xDP: TDataProvider;
    xVarName, xNewVarName: string;
begin
    xDP := fTableChangeAdaptor.CreateDestDataProvider();
    try
        xDP.SelectAndOpen('SELECT * FROM ImpCols', false);
        try
            while not xDP.Eof do
            begin
                try
                    xVarName := xDP.FieldByName('TARGETCOL').AsString;
                    xNewVarName := TImportColDefUtils.ReplaceGlobalVarName(xVarName);
                    if (xNewVarName <> xVarName) then
                    begin
                        xDP.Edit;
                        xDP.FieldByName('TARGETCOL').AsString := xNewVarName;
                        xDP.Post;
                    end;
                finally
                    xDP.Next;
                end;
            end;
        finally
            xDP.Close();
        end;
    finally
        xDP.Free;
    end;
end;


end.
