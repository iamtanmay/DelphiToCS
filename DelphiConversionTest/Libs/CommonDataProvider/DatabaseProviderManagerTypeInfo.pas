{ -----------------------------------------------------------------------------------------------------------------------
  Copyright © 2010 Zinsser Analytic GmbH - All rights reserved.
  Author       : Payman Kamali (pk)
  Description  :
  -----------------------------------------------------------------------------------------------------------------------
  Revision History:
  date     op  method                                track-no  improvement/change
  -------- --  ------------------------------------  --------- ----------------------------------------------------------
  04.08.10 pk                                        TN5218     Initial revision
  11.09.11 wl  TDatabaseProviderManagerTypeInfo.Destroy TN5672   zerstört jetzt auch fDatabaseProviderManagerCreator
  ----------------------------------------------------------------------------------------------------------------------- }

unit DatabaseProviderManagerTypeInfo;


interface


uses
    DataConnectionParams,
    DatabaseProviderManager,
    TypeInfo;

type
    TDatabaseProviderManagerCreator = class
    protected
        function GetInstance: TDatabaseProviderManager; virtual; abstract;
        procedure DoCreateInstance(); virtual; abstract;
        procedure DoDestroyInstance(); virtual; abstract;
    public
        procedure CreateInstance();
        procedure DestroyInstance();
        property Instance: TDatabaseProviderManager read GetInstance;
    end;

    TDatabaseProviderManagerTypeInfo = class(TTypeInfo)
    private
        fDatabaseProviderManagerCreator: TDatabaseProviderManagerCreator;
        procedure CreateDatabaseProviderManagerCreator();
    protected
        function DoCreateDatabaseProviderManagerCreator(): TDatabaseProviderManagerCreator; virtual; abstract;
    public
        constructor Create(const aTypeName, aTypeInfoVersion, aLibName, aLibVersion: string);
        destructor Destroy; override;
        property DatabaseProviderManagerCreator: TDatabaseProviderManagerCreator
            read fDatabaseProviderManagerCreator;
    end;


implementation


uses
    SysUtils,
    Classes;

{ TDatabaseProviderManagerCreator }

procedure TDatabaseProviderManagerCreator.CreateInstance;
begin
    DoCreateInstance();
end;

procedure TDatabaseProviderManagerCreator.DestroyInstance;
begin
    DoDestroyInstance();
end;

{ TDatabaseProviderManagerTypeInfo }

constructor TDatabaseProviderManagerTypeInfo.Create(const aTypeName, aTypeInfoVersion, aLibName,
    aLibVersion: string);
begin
    inherited;
    self.CreateDatabaseProviderManagerCreator();
end;

procedure TDatabaseProviderManagerTypeInfo.CreateDatabaseProviderManagerCreator;
begin
    fDatabaseProviderManagerCreator := DoCreateDatabaseProviderManagerCreator();
end;

destructor TDatabaseProviderManagerTypeInfo.Destroy;
begin
    FreeAndNil(fDatabaseProviderManagerCreator);
    inherited;
end;


end.
