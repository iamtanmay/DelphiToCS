{ --------------------------------------------------------------------------------------------------
  Copyright © 2007 Zinsser Analytic GmbH - All rights reserved.
  Author       : Payman Kamali (pk)
  Description  : structures which are required by higher update manager classes
  --------------------------------------------------------------------------------------------------
  Revision History:
  date     op  method                       track-no improvement/change
  -------- --  ---------------------------  -------- -----------------------------------------------
  06.02.07 pk                                TN3544  Initial Revision
  16.02.07 pk  TTableUpdateDataDef           TN3581  DataFunc function also takes aDestTableDir as parameter
  22.02.07 pk  TTableUpdateStructDef.Destroy TN3583  Free fIndexDef
  21.09.07 wl  TTableUpdateStructDef.DbPath  TN3811.4 ersetzt Alias
  27.02.13 wl                                TN6045   uses geändert
  -------------------------------------------------------------------------------------------------- }

unit TableUpdateItem;


interface


uses
    Generics.Collections,
    TableStructDef,
    TableUpdateDataMap;

type
    TTableUpdateDataFunc = procedure(aSender: TObject) of object;

    TTableUpdateDataDef = class
    private
        fDataMaps: TObjectList<TTableUpdateDataMap>;
        fDataFunc: TTableUpdateDataFunc;
    public
        constructor Create();
        destructor Destroy(); override;
        property DataMaps: TObjectList<TTableUpdateDataMap>read fDataMaps;
        property DataFunc: TTableUpdateDataFunc read fDataFunc write fDataFunc;
    end;

    TTableUpdateItem = class
    protected
        fStructDef: TTableStructDef;
        procedure DoPrepareUpdate(); virtual;
    public
        procedure PrepareUpdate(aStructDef: TTableStructDef);
    end;

    TTableUpdateStructItem = class(TTableUpdateItem)
    end;

    TTableUpdateAlterStructItem = class(TTableUpdateStructItem)
    protected
        fAlteredStructDef: TTableStructDef;
        procedure DoPrepareUpdate(); override;
    public
        constructor Create(aTableStructDef: TTableStructDef);
    end;

    // Data updates
    TTableUpdateDataItem = class(TTableUpdateItem)
    protected
        fDataDef: TTableUpdateDataDef;
    public
        procedure SetDataDef(aDataDef: TTableUpdateDataDef);
    end;

    TTableUpdateExecuteDataMapItem = class(TTableUpdateDataItem)
    protected
        fDataMap: TTableUpdateDataMap;
        procedure DoPrepareUpdate(); override;
    public
        constructor Create(aDataMap: TTableUpdateDataMap);
        destructor Destroy(); override;
    end;

    TTableUpdateDataFuncItem = class(TTableUpdateDataItem)
    protected
        fFunc: TTableUpdateDataFunc;
        procedure DoPrepareUpdate(); override;
    public
        constructor Create(aFunc: TTableUpdateDataFunc);

    end;


implementation


uses
    SysUtils;

{ TTableUpdateItem }

procedure TTableUpdateItem.DoPrepareUpdate;
begin

end;

procedure TTableUpdateItem.PrepareUpdate(aStructDef: TTableStructDef);
begin
    fStructDef := aStructDef;
    DoPrepareUpdate();
end;

{ TTableUpdateExecuteDataMapItem }
constructor TTableUpdateExecuteDataMapItem.Create(aDataMap: TTableUpdateDataMap);
begin
    inherited Create();
    fDataMap := aDataMap;
end;

destructor TTableUpdateExecuteDataMapItem.Destroy();
begin
    fDataMap.Free;
    inherited Destroy();
end;

procedure TTableUpdateExecuteDataMapItem.DoPrepareUpdate;
begin
    fDataDef.DataMaps.Add(fDataMap);
end;

{ TTableUpdateSpecialDataFuncItem }

constructor TTableUpdateDataFuncItem.Create(aFunc: TTableUpdateDataFunc);
begin
    inherited Create();
    fFunc := aFunc;
end;

procedure TTableUpdateDataFuncItem.DoPrepareUpdate;
begin
    fDataDef.DataFunc := fFunc;
end;

{ TTableUpdateDataItem }

procedure TTableUpdateDataItem.SetDataDef(aDataDef: TTableUpdateDataDef);
begin
    fDataDef := aDataDef;
end;

{ TTableUpdateDataDef }

constructor TTableUpdateDataDef.Create;
const
    BOOL_OWNS_DATAMAP_OBJECTS = false;
begin
    inherited Create();
    fDataMaps := TObjectList<TTableUpdateDataMap>.Create(BOOL_OWNS_DATAMAP_OBJECTS);
    fDataFunc := nil;
end;

destructor TTableUpdateDataDef.Destroy();
begin
    fDataMaps.Free;
    inherited Destroy();
end;

constructor TTableUpdateAlterStructItem.Create(aTableStructDef: TTableStructDef);
begin
    inherited Create();
    fAlteredStructDef := aTableStructDef;
end;

procedure TTableUpdateAlterStructItem.DoPrepareUpdate;
begin
    fStructDef.Assign(fAlteredStructDef);
end;


end.
