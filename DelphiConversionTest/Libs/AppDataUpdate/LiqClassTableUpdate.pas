unit LiqClassTableUpdate;
{ --------------------------------------------------------------------------------------------------
  Copyright © 2008 Zinsser Analytic GmbH - All rights reserved.
  Author       : Payman Kamali (pk)
  Description  :
  --------------------------------------------------------------------------------------------------
  Revision History:
  date     op  method                       track-no improvement/change
  -------- --  ---------------------------  -------- -----------------------------------------------
  24.06.08 pk                               TN4148   FieldNames unit removed
  31.07.09 wl  TLiqClassTableStructDefV2    TN4693   WashMacroName hat jetzt 50 chars
  -------------------------------------------------------------------------------------------------- }


interface


uses
    Update,
    TableStructDef,
    TableUpdate;

type
    // table structure definitions
    TLiqClassTableStructDefV0 = class(TTableStructDef)
    protected
        procedure DoDefineStruct(); override;
    end;

    TLiqClassTableStructDefV1 = class(TLiqClassTableStructDefV0)
    protected
        procedure DoDefineStruct(); override;
    end;

    TLiqClassTableStructDefV2 = class(TLiqClassTableStructDefV1)
    protected
        procedure DoDefineStruct(); override;
    end;

    // table updates
    TLiqClassTableUpdateV1 = class(TTableUpdate)
    public
        constructor Create(aUpdateNumber: TUpdateID);
    end;

    // table updates
    TLiqClassTableUpdateV2 = class(TTableUpdate)
    public
        constructor Create(aUpdateNumber: TUpdateID);
    end;


implementation


uses
    SysUtils;

const
    INT_REVISION_1 = 1;
    INT_REVISION_2 = 2;

    { TLiqClassTableStructDefV0 }

procedure TLiqClassTableStructDefV0.DoDefineStruct;
begin
    inherited;
    fName := 'LIQPARCLASSES';
end;

{ TLiqClassTableStructDefV1 }

procedure TLiqClassTableStructDefV1.DoDefineStruct();
begin
    inherited;
    self.AddField('LIQCLASSCURVENAME', tftString, 20);
    self.AddField('VOLUME', tftFloat, 0);

    self.AddField('AIRGAPASPVOL', tftFloat, 0);
    self.AddField('AIRGAPDISPVOL', tftFloat, 0);
    self.AddField('AIRGAPSPEED', tftFloat, 0);
    self.AddField('AIRGAPDELAY', tftFloat, 0);

    self.AddField('DILUENTSPEED', tftFloat, 0);
    self.AddField('DILUENTDELAY', tftFloat, 0);

    self.AddField('SAMPLESPEED', tftFloat, 0);
    self.AddField('SAMPLEDELAY', tftFloat, 0);

    self.AddField('TRANSPGAPVOL', tftFloat, 0);
    self.AddField('TRANSPGAPSPEED', tftFloat, 0);
    self.AddField('TRANSPGAPDELAY', tftFloat, 0);

    self.AddField('DISPENSESPEED', tftFloat, 0);
    self.AddField('DISPENSEDELAY', tftFloat, 0);

    self.AddField('SAMPLESPITBACKVOL', tftFloat, 0);
    self.AddField('SAMPLESPITBACKCOUNT', tftFloat, 0);
    self.AddField('SAMPLESPITBACKSPEED', tftFloat, 0);
    self.AddField('SAMPLESPITBACKDELAY', tftFloat, 0);
    self.AddField('SAMPLECH2SPITBVOL', tftFloat, 0);
    self.AddField('SAMPLECH2SPITBSPEED', tftFloat, 0);

    self.AddField('SAMPLEWASTEVOL', tftFloat, 0);
    self.AddField('SAMPLEWASTEPERCENT', tftFloat, 0);

    self.AddField('WASHAFTERDISPENSE', tftFloat, 0);
    self.AddField('WASHISFORCED', tftBoolean, 0);
    self.AddField('USEWASHMACRO', tftBoolean, 0);
    self.AddField('WASHMACRONAME', tftString, 20);
    self.AddField('WASHVOLMIN', tftFloat, 0);
    self.AddField('WASHVOLMAX', tftFloat, 0);
    self.AddField('WASHVOLFACTOR', tftFloat, 0);
    self.AddField('WASHVOLCHANNEL2', tftFloat, 0);
    self.AddField('DRYAFTERWASH', tftBoolean, 0);

    // Index
    self.AddIndex('LIQCLASSCURVENAME;VOLUME');
end;

{ TLiqClassTableStructDefV2 }

procedure TLiqClassTableStructDefV2.DoDefineStruct;
begin
    inherited;

    self.ResizeField('WASHMACRONAME', 50);
    { TODO -owl : Eigentlich müssten die Felder USEMACRO, WASHMACRONAME umbenannt werden }
end;

{ TLiqClassTableUpdateV1 }

constructor TLiqClassTableUpdateV1.Create(aUpdateNumber: TUpdateID);
begin
    inherited Create(aUpdateNumber, TLiqClassTableStructDefV0, INT_REVISION_1);
    AlterStructure(TLiqClassTableStructDefV1);
    CopyMatchingFields([]);
end;

{ TLiqClassTableUpdateV2 }

constructor TLiqClassTableUpdateV2.Create(aUpdateNumber: TUpdateID);
begin
    inherited Create(aUpdateNumber, TLiqClassTableStructDefV1, INT_REVISION_2);
    AlterStructure(TLiqClassTableStructDefV2);
    CopyMatchingFields([]);
end;


end.
