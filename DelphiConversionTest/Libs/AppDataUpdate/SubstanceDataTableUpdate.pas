{ -----------------------------------------------------------------------------------------------------------
  Copyright © 2011 Zinsser Analytic GmbH - All rights reserved.
  Author       : Wolfgang Lyncke (wl)
  Description  :
  -----------------------------------------------------------------------------------------------------------
  Revision History:
  date     op  method                              track-no improvement/change
  -------- --  ----------------------------------  -------- -------------------------------------------------
  31.10.11 wl                                      TN5725   Initial Revision
  17.11.11 wl                                      TN5725   neu: FULLNAME
  21.11.11 wl                                      TN5730   SYSTEMASPSPEED entfernt
  27.11.11 wl                                      TN5730   ISMIX wieder entfernt
  ----------------------------------------------------------------------------------------------------------- }

unit SubstanceDataTableUpdate;


interface


uses
    Update,
    TableStructDef,
    TableUpdate;

type
    // table structure definitions
    TSubstanceDataTableStructDefV0 = class(TTableStructDef)
    protected
        procedure DoDefineStruct(); override;
    end;

    TSubstanceDataTableStructDefV1 = class(TSubstanceDataTableStructDefV0)
    protected
        procedure DoDefineStruct(); override;
    end;

    // table updates
    TSubstanceDataTableUpdateV1 = class(TTableUpdate)
    public
        constructor Create(aUpdateNumber: TUpdateID);
    end;


implementation


uses
    SysUtils,
    DataProvider;

const
    INT_REVISION_1 = 1;

    { TSubstanceDataTableStructDefV0 }

procedure TSubstanceDataTableStructDefV0.DoDefineStruct;
begin
    inherited;
    fName := 'SUBSTANCEDATA';
end;

{ TSubstanceDataTableStructDefV1 }

procedure TSubstanceDataTableStructDefV1.DoDefineStruct();
begin
    inherited;
    AddField('SUBSTID', tftString, 20);
    AddField('FULLNAME', tftString, 50);
    AddField('DESCRIPTION', tftString, 80);
    AddField('SUBSTCOLOR', tftInteger, 0);
    AddField('LIQPARAM', tftString, 20);
    AddIndex('SUBSTID');
end;

{ TSubstanceDataTableUpdateV1 }

constructor TSubstanceDataTableUpdateV1.Create(aUpdateNumber: TUpdateID);
begin
    inherited Create(aUpdateNumber, TSubstanceDataTableStructDefV0, INT_REVISION_1);
    AlterStructure(TSubstanceDataTableStructDefV1);
    CopyMatchingFields([]);
end;


end.
