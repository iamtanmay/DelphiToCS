unit ResourcesTableUpdate;


{ --------------------------------------------------------------------------------------------------
  Copyright © 2008 Zinsser Analytic GmbH - All rights reserved.
  Author       : Payman Kamali (pk)
  Description  :
  --------------------------------------------------------------------------------------------------
  Revision History:
  date     op  method                       track-no improvement/change
  -------- --  ---------------------------  -------- -----------------------------------------------
  24.06.08 pk                               TN4148   FieldNames unit removed
  -------------------------------------------------------------------------------------------------- }
interface


uses
    Update,
    TableStructDef,
    TableUpdate;

type
    // table structure definitions
    TResourcesTableStructDefV0 = class(TTableStructDef)
    protected
        procedure DoDefineStruct(); override;
    end;

    TResourcesTableStructDefV1 = class(TResourcesTableStructDefV0)
    protected
        procedure DoDefineStruct(); override;
    end;

    // table updates
    TResourcesTableUpdateV1 = class(TTableUpdate)
    public
        constructor Create(aUpdateNumber: TUpdateID);
    end;


implementation


uses
    SysUtils;

const
    INT_REVISION_1 = 1;

    { TResourcesTableStructDefV0 }

procedure TResourcesTableStructDefV0.DoDefineStruct;
begin
    inherited;
    fName := 'RESOURCES';
end;

{ TResourcesTableStructDefV1 }

procedure TResourcesTableStructDefV1.DoDefineStruct();
begin
    inherited;
    self.AddField('NAME', tftString, 20);
    self.AddField('RESNAME', tftString, 20);
    self.AddField('SHARELEVEL', tftInteger, 0);
    self.AddIndex('NAME;RESNAME');
end;

constructor TResourcesTableUpdateV1.Create(aUpdateNumber: TUpdateID);
begin
    inherited Create(aUpdateNumber, TResourcesTableStructDefV0, INT_REVISION_1);
    AlterStructure(TResourcesTableStructDefV1);
    CopyMatchingFields([]);
end;


{
  procedure TResourcesTableUpdater.DefineVersions();
  var
  xVersion : TTableUpdateVersionExt;
  xVersionNumber : integer;
  begin
  // V 1
  xVersionNumber := 1;
  xVersion := TTableUpdateVersionExt.Create( xVersionNumber );
  self.AddVersionUpdate( xVersion );
  xVersion.AddField( STR_RESOURCES_FLD_NAME,    ftString,   20 );
  xVersion.AddField( STR_RESOURCES_FLD_RESNAME, ftString,   20 );
  xVersion.AddField( STR_RESOURCES_FLD_SHARELEVEL,   ftInteger,   0 );
  xVersion.AddIndex( Format('%s;%s;', [STR_RESOURCES_FLD_NAME, STR_RESOURCES_FLD_RESNAME] ) );

  xVersion.CopyMatchingFields( [] );

  end;
}
end.
