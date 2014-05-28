{ -----------------------------------------------------------------------------------------------------------------------
  Copyright © 2010 Zinsser Analytic GmbH - All rights reserved.
  Author       : Payman Kamali (pk)
  Description  :
  -----------------------------------------------------------------------------------------------------------------------
  Revision History:
  date     op  method                                track-no  improvement/change
  -------- --  ------------------------------------  --------- ----------------------------------------------------------
  04.08.10 pk                                        TN5218     Initial revision
  13.09.11 wl  TDatabaseConfig.Destroy               TN5672   Memory leak fixed
  21.03.13 wl                                        TN6045   uses geändert
  ----------------------------------------------------------------------------------------------------------------------- }

unit DatabaseConfig;


interface


uses
    GeneralTypes,
    Streamable,
    DataConnectionParams;

type
    TDatabaseConfig = class(TStreamable)
    private
        fAlias: string;
        fConnectionParams: TDataConnectionParams;
    public
        destructor Destroy(); override;
    published
        property Alias: string read fAlias write fAlias;
        property ConnectionParams: TDataConnectionParams read fConnectionParams write fConnectionParams;
    end;

    TDatabaseConfigList = class(TStreamableObjectList)
    private
        function GetItemAt(aIndex: integer): TDatabaseConfig;
    public
        function FindByAlias(const aAlias: string): TDatabaseConfig;
        procedure DeleteByAlias(const aAlias: string);
        property this[aIndex: integer]: TDatabaseConfig read GetItemAt; default;
    end;

    TDatabaseConfigGroup = class(TStreamable)
    private
        fList: TDatabaseConfigList;
    public
        constructor Create(); override;
        destructor Destroy(); override;
    published
        function GetDatabaseAliasNames(): TStringArray;
        property List: TDatabaseConfigList read fList write fList;
    end;


implementation


uses
    SysUtils;

{ TDatabaseConfigList }

procedure TDatabaseConfigList.DeleteByAlias(const aAlias: string);
var
    xDatabaseConfig: TDatabaseConfig;
begin
    xDatabaseConfig := FindByAlias(aAlias);
    if not Assigned(xDatabaseConfig) then
        EXIT;

    self.Remove(xDatabaseConfig);
end;

function TDatabaseConfigList.FindByAlias(const aAlias: string): TDatabaseConfig;
var
    x: integer;
begin
    result := nil;
    for x := 0 to self.Count - 1 do
    begin
        if SameText(self[x].Alias, aAlias) then
        begin
            result := self[x];
            EXIT;
        end;
    end;

end;

function TDatabaseConfigList.GetItemAt(aIndex: integer): TDatabaseConfig;
begin
    result := inherited Items[aIndex] as TDatabaseConfig;
end;

{ TDatabaseConfigGroup }

constructor TDatabaseConfigGroup.Create;
begin
    inherited Create();
    fList := TDatabaseConfigList.Create();
end;

destructor TDatabaseConfigGroup.Destroy;
begin
    FreeAndNil(fList);
    inherited;
end;

function TDatabaseConfigGroup.GetDatabaseAliasNames(): TStringArray;
var

    x: integer;
begin
    SetLength(result, self.List.Count);
    for x := 0 to self.List.Count - 1 do
    begin
        result[x] := self.List[x].Alias;
    end;
end;

{ TDatabaseConfig }

destructor TDatabaseConfig.Destroy;
begin
    FreeAndNil(fConnectionParams);
    inherited;
end;


end.
