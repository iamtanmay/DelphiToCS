unit DatasetUtils;
{--------------------------------------------------------------------------------------------------
 Copyright © 2007 Zinsser Analytic GmbH - All rights reserved.
 Author       : Wolfgang Lyncke (wl)
 Description  : Utility functions for TDataset
 --------------------------------------------------------------------------------------------------
 Revision History:
 date     op  method                       track-no improvement/change
 -------- --  ---------------------------  -------- -----------------------------------------------
 07.08.07 wl  CopyDatasetRecord            TN3811.3 von DataAdaptor hierher
 07.08.07 wl  CommonDelete,CommonSaveAs    TN3811.3 von DataAdaptor hierher
 02.10.07 wl  CommonDelete,CommonSaveAs    TN3811.5 entfernt
 --------------------------------------------------------------------------------------------------}

interface

uses
    DB;

type
    TDatasetUtils = class
      public
        class procedure CopyDatasetRecord(aSourceDataset, aTargetDataset: TDataset; aAppend: boolean;
            aReplaceFields: array of string; aReplaceValues: array of variant);
    end;


//##################################################################################################

implementation

uses
    SysUtils;


class procedure TDatasetUtils.CopyDatasetRecord( aSourceDataset, aTargetDataset : TDataset; aAppend : boolean;
                                            aReplaceFields : array of string; aReplaceValues : array of variant );
var
    i : integer;
    xSourceValue : variant;
    xReplaceIndex : integer;
    function IndexOfReplaceField( aFieldName : string ) : integer;
    var j : integer;
    begin
        result := -1;
        aFieldName := UpperCase( aFieldName );
        for j := 0 to High( aReplaceFields ) do
            if UpperCase( aReplaceFields[j] ) = aFieldName then begin result := j; Exit; end;
    end;
begin
    if aSourceDataset.Eof then Exit;


    if aAppend then begin
        aTargetDataset.Append;
    end
    else begin
        aTargetDataset.Edit;
        if aTargetDataset.Eof then Exit;
    end;

    for i:= 0 to aTargetDataset.FieldCount - 1 do begin
        if (aSourceDataset.FieldCount - 1) < i then Break;
        xReplaceIndex := IndexOfReplaceField( aTargetDataset.Fields[i].FieldName );

        if xReplaceIndex >= 0 then
            xSourceValue := aReplaceValues[xReplaceIndex]
        else
            xSourceValue := aSourceDataset.Fields[i].Value;

        aTargetDataset.Fields[i].Value := xSourceValue;
    end;

    aTargetDataset.Post;
end;

end.
