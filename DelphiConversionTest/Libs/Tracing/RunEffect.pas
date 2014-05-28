{ -----------------------------------------------------------------------------------------------------------------------
  Copyright © 2010 Zinsser Analytic GmbH - All rights reserved.
  Author       : Payman Kamali (pk)
  Description  :
  -----------------------------------------------------------------------------------------------------------------------
  Revision History:
  date     op  method                                track-no  improvement/change
  -------- --  ------------------------------------  --------- ----------------------------------------------------------
  07.06.10 pk                                        TN5077     initial revision
  10.04.13 wl                                        TN6045   uses Generics.Collections
  ----------------------------------------------------------------------------------------------------------------------- }

unit RunEffect;


interface


uses
    Generics.Collections,
    ActionData,
    TraceProcessDetails,
    TraceThreadDetails,
    RunEffectData;

type
    TRunEffect = class
    protected
        fRunEffectData: TRunEffectData;
        procedure DoUndo(const aProcessDetails: TProcessDetailsItem; const aThreadDetails: TThreadDetailsItem;
            const aActionData: TActionData); virtual; abstract;
    public
        constructor Create(const aRunEffectData: TRunEffectData); reintroduce;
        destructor Destroy(); override;
        procedure Undo(const aProcessDetails: TProcessDetailsItem; const aThreadDetails: TThreadDetailsItem;
            const aActionData: TActionData);
    end;

    TRunEffectCreatorTypeInfo = class
    protected
        function DoCreateRunEffect(const aRunEffectData: TRunEffectData): TRunEffect; virtual; abstract;
        function DoSupportsData(const aRunEffectData: TRunEffectData): boolean; virtual; abstract;
    public
        function CreateRunEffect(const aRunEffectData: TRunEffectData): TRunEffect;
        function SupportsData(const aRunEffectData: TRunEffectData): boolean;
    end;

    TRunEffectCreatorTypeInfoList = class(TObjectList<TRunEffectCreatorTypeInfo>)
    public
        function FindBySupportsData(const aRunEffectData: TRunEffectData): TRunEffectCreatorTypeInfo;
    end;


implementation


uses
    SysUtils;

{ TRunEffect }

constructor TRunEffect.Create(const aRunEffectData: TRunEffectData);
begin
    inherited Create();
    fRunEffectData := aRunEffectData;
end;

destructor TRunEffect.Destroy;
begin
    FreeAndNil(fRunEffectData);
    inherited;
end;

procedure TRunEffect.Undo(const aProcessDetails: TProcessDetailsItem;
    const aThreadDetails: TThreadDetailsItem; const aActionData: TActionData);
begin
    self.DoUndo(aProcessDetails, aThreadDetails, aActionData);
end;

{ TRunEffectCreatorTypeInfo }

function TRunEffectCreatorTypeInfo.CreateRunEffect(const aRunEffectData: TRunEffectData): TRunEffect;
begin
    result := DoCreateRunEffect(aRunEffectData);
end;

function TRunEffectCreatorTypeInfo.SupportsData(const aRunEffectData: TRunEffectData): boolean;
begin
    result := DoSupportsData(aRunEffectData);
end;
{ TRunEffectCreatorTypeInfoList }

function TRunEffectCreatorTypeInfoList.FindBySupportsData(const aRunEffectData: TRunEffectData)
    : TRunEffectCreatorTypeInfo;
var
    xTypeInfo: TRunEffectCreatorTypeInfo;
begin
    result := nil;
    for xTypeInfo in self do
    begin
        if not xTypeInfo.SupportsData(aRunEffectData) then
            CONTINUE;
        result := xTypeInfo;
        EXIT;
    end;

end;


end.
