{ --------------------------------------------------------------------------------------------------
  Copyright © 2008 Zinsser Analytic GmbH - All rights reserved.
  Author       : Payman Kamali (pk)
  Description  :
  --------------------------------------------------------------------------------------------------
  Revision History:
  date     op  method                       track-no improvement/change
  -------- --  ---------------------------  -------- -----------------------------------------------
  24.06.08 pk                               TN4148   FieldNames unit removed
  30.06.11 wl  TSequenceTableUpdateV2       TN5603   Neue Tabelle, Daten werden nicht kopiert
  -------------------------------------------------------------------------------------------------- }

unit SequenceTableUpdate;


interface


uses
    Update,
    TableStructDef,
    TableUpdate;

type
    // table structure definitions
    TSequenceTableStructDefV0 = class(TTableStructDef)
    protected
        procedure DoDefineStruct(); override;
    end;

    TSequenceTableStructDefV1 = class(TSequenceTableStructDefV0)
    protected
        procedure DoDefineStruct(); override;
    end;

    TSequenceTableStructDefV2 = class(TSequenceTableStructDefV0)
    protected
        procedure DoDefineStruct(); override;
    end;

    // table updates
    TSequenceTableUpdateV1 = class(TTableUpdate)
    public
        constructor Create(aUpdateNumber: TUpdateID);
    end;

    TSequenceTableUpdateV2 = class(TTableUpdate)
    public
        constructor Create(aUpdateNumber: TUpdateID);
    end;


implementation


uses
    SysUtils;

const
    INT_REVISION_1 = 1;
    INT_REVISION_2 = 2;

    { TSequenceTableStructDefV0 }

procedure TSequenceTableStructDefV0.DoDefineStruct;
begin
    inherited;
    fName := 'SEQUENCE';
end;

{ TSequenceTableStructDefV1 }

procedure TSequenceTableStructDefV1.DoDefineStruct();
begin
    inherited;
    self.AddField('SEQNAME', tftString, 20);
    self.AddField('PRODUCTNO', tftInteger, 0);
    self.AddField('LEVEL', tftInteger, 0);
    self.AddField('SUBSTID', tftString, 20);
    self.AddField('VOLUME', tftFloat, 0);

    self.AddIndex('SEQNAME;PRODUCTNO;LEVEL;SUBSTID');
end;

{ TSequenceTableUpdateV1}

constructor TSequenceTableUpdateV1.Create(aUpdateNumber: TUpdateID);
begin
    inherited Create(aUpdateNumber, TSequenceTableStructDefV0, INT_REVISION_1);
    AlterStructure(TSequenceTableStructDefV1);
    CopyMatchingFields([]);
end;

{ TSequenceTableStructDefV2 }

procedure TSequenceTableStructDefV2.DoDefineStruct();
begin
    inherited;

    // Um alles neu zu gestalten ist TSequenceTableStructDefV2 nicht von V1 abgeleitet, sondern von V0

    self.AddField('SEQNAME', tftString, 30);
    self.AddField('LEVEL', tftInteger, 0);
    self.AddField('RACKPOS', tftInteger, 0);
    self.AddField('SEQTAG', tftString, 40);
    self.AddField('SUBSTID', tftString, 20);
    self.AddField('AMOUNT', tftFloat, 0);
    self.AddField('AMOUNTUNIT', tftString, 10);
    self.AddField('VAL_FLOAT1', tftFloat, 0);
    self.AddField('VAL_FLOAT2', tftFloat, 0);
    self.AddField('VAL_FLOAT3', tftFloat, 0);
    self.AddField('VAL_BOOL1', tftBoolean, 0);
    self.AddField('VAL_INT1', tftInteger, 0);
    self.AddField('VAL_STRING1', tftString, 40);

    self.AddIndex('SEQNAME;LEVEL;RACKPOS');
end;

{ TSequenceTableUpdateV2}

constructor TSequenceTableUpdateV2.Create(aUpdateNumber: TUpdateID);
begin
    inherited Create(aUpdateNumber, TSequenceTableStructDefV1, INT_REVISION_2);
    AlterStructure(TSequenceTableStructDefV2);

    // Daten sind sowieso unbrauchbar, werden deshalb auch nicht kopiert
    //CopyMatchingFields([]);
end;


end.
