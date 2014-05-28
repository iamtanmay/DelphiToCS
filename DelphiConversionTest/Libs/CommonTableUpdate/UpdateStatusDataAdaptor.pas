unit UpdateStatusDataAdaptor;


{ --------------------------------------------------------------------------------------------------
  Copyright © 2008 Zinsser Analytic GmbH - All rights reserved.
  Author       : Payman Kamali (pk)
  Description  :
  --------------------------------------------------------------------------------------------------
  Revision History:
  date     op  method                       track-no improvement/change
  -------- --  ---------------------------  -------- -----------------------------------------------
  24.06.08 pk                               TN4148   uses UpdateManagerDataProvider instead of dataprovider
  15.02.11 pk                               TN4780   changes needed to make UpdateManager compatible with TurboDB
  -------------------------------------------------------------------------------------------------- }
interface


uses
    DataProvider;

type
    TUpdateStatusType = (ustNone, ustUpdated);

    TUpdateStatusDataAdaptor = class
    private
        fDataProvider: TDataProvider;
        class function IntToStatus(aInt: integer): TUpdateStatusType;
        class function StatusToInt(aStatus: TUpdateStatusType): integer;

    public
        constructor Create(const aDBPath: string);
        procedure WriteStatus(aUpdateID: integer; const aUpdateDescription: string;
            aStatus: TUpdateStatusType);
        function ReadStatus(aUpdateID: integer): TUpdateStatusType;
    end;


implementation


uses
    SysUtils,
    UpdaterDataProviderFactory;

{ TUpdateStatusDataAdaptor }

constructor TUpdateStatusDataAdaptor.Create(const aDBPath: string);
begin
    inherited Create();
    fDataProvider := TUpdaterDataProviderFactory.CreateStandardDataProvider(aDBPath);
end;

class function TUpdateStatusDataAdaptor.IntToStatus(aInt: integer): TUpdateStatusType;
begin
    case aInt of
        1:
            result := ustUpdated;
        else
            result := ustNone;
    end;
end;

class function TUpdateStatusDataAdaptor.StatusToInt(aStatus: TUpdateStatusType): integer;
begin
    case aStatus of
        ustUpdated:
            result := 1;
        else
            result := 0;
    end;
end;

function TUpdateStatusDataAdaptor.ReadStatus(aUpdateID: integer): TUpdateStatusType;
begin
    result := ustNone;

    fDataProvider.SelectAndOpen(Format('SELECT * FROM "UPDATESTATUS" WHERE ID = %d', [aUpdateID]), true);
    try
        if fDataProvider.IsEmpty then
            EXIT;
        result := IntToStatus(fDataProvider.FieldByName('STATUS').AsInteger);
    finally
        fDataProvider.Close();
    end;
end;

procedure TUpdateStatusDataAdaptor.WriteStatus(aUpdateID: integer; const aUpdateDescription: string;
    aStatus: TUpdateStatusType);
begin
    fDataProvider.SelectAndOpen(Format('SELECT * FROM "UPDATESTATUS" WHERE ID = %d', [aUpdateID]), false);
    try
        if fDataProvider.IsEmpty then
        begin
            fDataProvider.Append;
            fDataProvider.FieldByName('ID').AsInteger := aUpdateID;
            fDataProvider.FieldByName('DESCRIPTION').AsString := aUpdateDescription;
        end
        else
            fDataProvider.Edit;

        fDataProvider.FieldByName('STATUS').AsInteger := StatusToInt(aStatus);

        fDataProvider.Post;
    finally
        fDataProvider.Close();
    end;
end;


end.
