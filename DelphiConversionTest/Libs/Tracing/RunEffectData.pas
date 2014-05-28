unit RunEffectData;


{ -----------------------------------------------------------------------------------------------------------------------
  Copyright © 2010 Zinsser Analytic GmbH - All rights reserved.
  Author       : Payman Kamali (pk)
  Description  :
  -----------------------------------------------------------------------------------------------------------------------
  Revision History:
  date     op  method                                track-no  improvement/change
  -------- --  ------------------------------------  --------- ----------------------------------------------------------
  07.06.10 pk                                        TN5077     initial revision
  26.10.10 pk  TRunEffectListData                    TN5297     New
  ----------------------------------------------------------------------------------------------------------------------- }
interface


uses
    Streamable;

type
    TRunEffectData = class(TStreamable)
    end;

    TRunEffectListData = class(TStreamable)
    private
        fList: TStreamableObjectList;
        function GetCount: integer;
        function GetValueAt(aIndex: integer): TRunEffectData;
    public
        constructor Create(); override;
        destructor Destroy(); override;
        procedure Clear();
        property this[aIndex: integer]: TRunEffectData read GetValueAt; default;
        property Count: integer read GetCount;
    published

        property List: TStreamableObjectList read fList write fList;
    end;


implementation


uses
    SysUtils;

{ TRunEffectListData }
procedure TRunEffectListData.Clear;
begin
    fList.Clear();
end;

constructor TRunEffectListData.Create;
begin
    inherited;
    fList := TStreamableObjectList.Create();
end;

destructor TRunEffectListData.Destroy;
begin
    FreeAndNil(fList);
    inherited;
end;

function TRunEffectListData.GetCount: integer;
begin
    result := fList.Count;
end;

function TRunEffectListData.GetValueAt(aIndex: integer): TRunEffectData;
begin
    result := fList[aIndex] as TRunEffectData;
end;


end.
