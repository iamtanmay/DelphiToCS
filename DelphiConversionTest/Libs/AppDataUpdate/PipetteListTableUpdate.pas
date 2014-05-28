{ -----------------------------------------------------------------------------------------------------------
  Copyright © 2011 Zinsser Analytic GmbH - All rights reserved.
  Author       : Wolfgang Lyncke (wl)
  Description  :
  -----------------------------------------------------------------------------------------------------------
  Revision History:
  date     op  method                              track-no improvement/change
  -------- --  ----------------------------------  -------- -------------------------------------------------
  03.11.11 wl                                      TN5730   Initial Revision
  21.11.11 wl                                      TN5730   erweitert für Volumenkontrolle
  22.11.11 wl                                      TN5730   PIPDEVICE statt USEDPIPDEVICE
  30.11.11 wl  TPipetteListTableStructDefV2        TN5755   neue Felder: RemarkText1, RemarkNumber1
  27.12.11 wl  TPipetteListTableStructDefV3        TN5773   neue Felder: DESTVOLWASTE, DILVOLWASTE
  05.07.12 wl  TPipetteListTableStructDefV4        TN5931   neue Felder: RunSourcePosX, ...
  05.07.12 wl  TPipetteListTableStructDefV5        TN5927   DILNAME ist jetzt ein string-Feld
  12.06.13 wl  TPipetteListTableUpdateV6           TN6172   neue Felder für den aktuellen Status
  02.09.13 wl  TPipetteListTableUpdateV7           TN6239   NAME hat jetzt 50 Character
  ----------------------------------------------------------------------------------------------------------- }

unit PipetteListTableUpdate;


interface


uses
    Update,
    TableStructDef,
    TableUpdate;

type
    // table structure definitions
    TPipetteListTableStructDefV0 = class(TTableStructDef)
    protected
        procedure DoDefineStruct(); override;
    end;

    TPipetteListTableStructDefV1 = class(TPipetteListTableStructDefV0)
    protected
        procedure DoDefineStruct(); override;
    end;

    TPipetteListTableStructDefV2 = class(TPipetteListTableStructDefV1)
    protected
        procedure DoDefineStruct(); override;
    end;

    TPipetteListTableStructDefV3 = class(TPipetteListTableStructDefV2)
    protected
        procedure DoDefineStruct(); override;
    end;

    TPipetteListTableStructDefV4 = class(TPipetteListTableStructDefV3)
    protected
        procedure DoDefineStruct(); override;
    end;

    TPipetteListTableStructDefV5 = class(TPipetteListTableStructDefV4)
    protected
        procedure DoDefineStruct(); override;
    end;

    TPipetteListTableStructDefV6 = class(TPipetteListTableStructDefV5)
    protected
        procedure DoDefineStruct(); override;
    end;

    TPipetteListTableStructDefV7 = class(TPipetteListTableStructDefV6)
    protected
        procedure DoDefineStruct(); override;
    end;

    // table updates
    TPipetteListTableUpdateV1 = class(TTableUpdate)
    public
        constructor Create(aUpdateNumber: TUpdateID);
    end;

    TPipetteListTableUpdateV2 = class(TTableUpdate)
    public
        constructor Create(aUpdateNumber: TUpdateID);
    end;

    TPipetteListTableUpdateV3 = class(TTableUpdate)
    public
        constructor Create(aUpdateNumber: TUpdateID);
    end;

    TPipetteListTableUpdateV4 = class(TTableUpdate)
    public
        constructor Create(aUpdateNumber: TUpdateID);
    end;

    TPipetteListTableUpdateV5 = class(TTableUpdate)
    public
        constructor Create(aUpdateNumber: TUpdateID);
    end;

    TPipetteListTableUpdateV6 = class(TTableUpdate)
    public
        constructor Create(aUpdateNumber: TUpdateID);
    end;

    TPipetteListTableUpdateV7 = class(TTableUpdate)
    public
        constructor Create(aUpdateNumber: TUpdateID);
    end;


implementation


uses
    SysUtils;

const
    INT_REVISION_1 = 1;
    INT_REVISION_2 = 2;
    INT_REVISION_3 = 3;
    INT_REVISION_4 = 4;
    INT_REVISION_5 = 5;
    INT_REVISION_6 = 6;
    INT_REVISION_7 = 7;

    { TPipetteListTableStructDefV0 }

procedure TPipetteListTableStructDefV0.DoDefineStruct;
begin
    inherited;
    fName := 'PIPETTELIST';
end;

{ TPipetteListTableStructDefV1 }

procedure TPipetteListTableStructDefV1.DoDefineStruct();
begin
    inherited;

    self.AddField('NAME', tftString, 20);
    self.AddField('SEQ', tftInteger, 0);
    self.AddField('PIPDEVICE', tftString, 40);
    self.AddField('USEDTIPS', tftInteger, 0);
    self.AddField('USEDTIPTYPE', tftString, 20);
    self.AddField('TIPBLOCK', tftInteger, 0);
    self.AddField('TIP', tftInteger, 0);
    self.AddField('LIQPARAM', tftString, 20);
    self.AddField('DILUENT', tftInteger, 0);

    self.AddField('SOURCERACK', tftString, 30);
    self.AddField('SOURCEPOS', tftInteger, 0);

    self.AddField('DESTRACK', tftString, 30);
    self.AddField('DESTPOS', tftInteger, 0);

    self.AddField('SOURCEVOL', tftFloat, 0);
    self.AddField('DESTVOL', tftFloat, 0);

    self.AddField('DILRACK', tftString, 30);
    self.AddField('DILPOS', tftInteger, 0);
    self.AddField('DILVOL', tftFloat, 0);

    self.AddField('RunMissingSourceVol', tftFloat, 0);
    self.AddField('RunMissingDilVol', tftFloat, 0);
    self.AddField('RunReplacedSourceID', tftString, 30);
    self.AddField('RunReplacedDestID', tftString, 30);
    self.AddField('RunReplacedDilID', tftString, 30);

    self.AddIndex('NAME;SEQ');
end;

{ TPipetteListTableStructDefV2 }

procedure TPipetteListTableStructDefV2.DoDefineStruct;
begin
    inherited;

    self.AddFieldAt('REMARKNUMBER1', tftInteger, 0, 2);
    self.AddFieldAt('REMARKTEXT1', tftString, 50, 3);
end;

{ TPipetteListTableStructDefV3 }

procedure TPipetteListTableStructDefV3.DoDefineStruct;
begin
    inherited;

    self.AddFieldAt('DESTVOLWASTE', tftFloat, 0, 17);
    self.AddFieldAt('DILVOLWASTE', tftFloat, 0, 21);
end;

{ TPipetteListTableStructDefV4 }

procedure TPipetteListTableStructDefV4.DoDefineStruct;
begin
    inherited;

    self.AddField('RunSourcePosX', tftFloat, 0);
    self.AddField('RunSourcePosY', tftFloat, 0);
    self.AddField('RunDestPosX', tftFloat, 0);
    self.AddField('RunDestPosY', tftFloat, 0);
    self.AddField('RunDilPosX', tftFloat, 0);
    self.AddField('RunDilPosY', tftFloat, 0);
end;

{ TPipetteListTableStructDefV5 }

procedure TPipetteListTableStructDefV5.DoDefineStruct;
begin
    inherited;

    self.DelField('DILUENT');
    self.AddFieldAt('DILNAME', tftString, 30, 17);
end;

{ TPipetteListTableStructDefV6 }

procedure TPipetteListTableStructDefV6.DoDefineStruct;
begin
    inherited;

    self.AddFieldAt('DilAspDone', tftBoolean, 0, 2);
    self.AddFieldAt('SampleAspDone', tftBoolean, 0, 3);
    self.AddFieldAt('DispDone', tftBoolean, 0, 4);
    self.AddField('StartTime', tftDateTime, 0);
    self.AddField('EndTime', tftDateTime, 0);
end;

{ TPipetteListTableStructDefV7 }

procedure TPipetteListTableStructDefV7.DoDefineStruct;
begin
    inherited;

    self.ResizeField('NAME', 50);
end;

{ TPipetteListTableUpdateV1 }

constructor TPipetteListTableUpdateV1.Create(aUpdateNumber: TUpdateID);
begin
    inherited Create(aUpdateNumber, TPipetteListTableStructDefV0, INT_REVISION_1);
    AlterStructure(TPipetteListTableStructDefV1);
    CopyMatchingFields([]);
end;

{ TPipetteListTableUpdateV2 }

constructor TPipetteListTableUpdateV2.Create(aUpdateNumber: TUpdateID);
begin
    inherited Create(aUpdateNumber, TPipetteListTableStructDefV1, INT_REVISION_2);
    AlterStructure(TPipetteListTableStructDefV2);
    CopyMatchingFields([]);
end;

{ TPipetteListTableUpdateV3 }

constructor TPipetteListTableUpdateV3.Create(aUpdateNumber: TUpdateID);
begin
    inherited Create(aUpdateNumber, TPipetteListTableStructDefV2, INT_REVISION_3);
    AlterStructure(TPipetteListTableStructDefV3);
    CopyMatchingFields([]);
end;

{ TPipetteListTableUpdateV4 }

constructor TPipetteListTableUpdateV4.Create(aUpdateNumber: TUpdateID);
begin
    inherited Create(aUpdateNumber, TPipetteListTableStructDefV3, INT_REVISION_4);
    AlterStructure(TPipetteListTableStructDefV4);
    CopyMatchingFields([]);
end;

{ TPipetteListTableUpdateV5 }

constructor TPipetteListTableUpdateV5.Create(aUpdateNumber: TUpdateID);
begin
    inherited Create(aUpdateNumber, TPipetteListTableStructDefV4, INT_REVISION_5);
    AlterStructure(TPipetteListTableStructDefV5);
    CopyMatchingFields([]);
end;

{ TPipetteListTableUpdateV6 }

constructor TPipetteListTableUpdateV6.Create(aUpdateNumber: TUpdateID);
begin
    inherited Create(aUpdateNumber, TPipetteListTableStructDefV5, INT_REVISION_6);
    AlterStructure(TPipetteListTableStructDefV6);
    CopyMatchingFields([]);
end;

{ TPipetteListTableUpdateV7 }

constructor TPipetteListTableUpdateV7.Create(aUpdateNumber: TUpdateID);
begin
    inherited Create(aUpdateNumber, TPipetteListTableStructDefV6, INT_REVISION_7);
    AlterStructure(TPipetteListTableStructDefV7);
    CopyMatchingFields([]);
end;


end.
