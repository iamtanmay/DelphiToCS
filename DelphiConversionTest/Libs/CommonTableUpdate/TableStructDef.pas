{ --------------------------------------------------------------------------------------------------
  Copyright © 2007 Zinsser Analytic GmbH - All rights reserved.
  Author       : Payman Kamali (pk)
  Description  :
  --------------------------------------------------------------------------------------------------
  Revision History:
  date     op  method                       track-no improvement/change
  -------- --  ---------------------------  -------- -----------------------------------------------
  25.09.08 wl  TTableStructDef.ResizeField  TN4242   neu
  11.01.11 wl                                TN5405   verbessertes Logging
  17.04.13 wl                                TN6106   uses geändert
  -------------------------------------------------------------------------------------------------- }

unit TableStructDef;


interface


uses
    Classes,
    TableVersionInfo;

const
    INT_TABLEFIELD_NO_INDEX = -1;

type
    TTableFieldType = (tftString, tftInteger, tftSmallint, tftFloat, tftBoolean, tftMemo, tftDateTime,
        tftAutoInc);

    TTableFieldDef = class
    protected
        fFieldName: string;
        fFieldType: TTableFieldType;
        fFieldLen: integer;
    public
        constructor Create(const aFieldName: string; aFieldType: TTableFieldType; aFieldLen: integer);
        property FieldName: string read fFieldName;
        property FieldType: TTableFieldType read fFieldType;
        property FieldLen: integer read fFieldLen write fFieldLen;
    end;

    TTableFieldDefList = class(TStringList)
    private
        function GetFieldDef(aIndex: integer): TTableFieldDef;
    public
        property FieldDefs[aIndex: integer]: TTableFieldDef read GetFieldDef; default;
    end;

    TTableIndexDef = class
    protected
        fIndexText: string;
    public
        constructor Create(const aIndexText: string);
        property IndexText: string read fIndexText write fIndexText;
    end;

    TTableStructDefClass = class of TTableStructDef;

    TTableStructDef = class
    protected
        fName: string;
        fFieldDefs: TTableFieldDefList;
        fIndexDef: TTableIndexDef;
        fVersion: TTableRevisionNumber;
        fOwnsObjects: boolean;
        procedure InsertFieldDef(aFieldDef: TTableFieldDef; aIndex: integer);
        procedure AddField(const aFieldName: string; aFieldType: TTableFieldType; aFieldLen: integer);
        procedure AddFieldAt(const aFieldName: string; aFieldType: TTableFieldType;
            aFieldLen, aFieldPosition: integer);
        procedure AddIndex(const aIndexText: string);
        procedure DelField(const aFieldName: string);
        procedure DoDefineStruct(); virtual;
        procedure ResizeField(const aFieldName: string; aSize: integer);
    public
        constructor Create(aOwnsObjects: boolean = true); virtual;
        destructor Destroy(); override;

        procedure Assign(aTableStructDef: TTableStructDef);
        procedure DefineStruct();

        property name: string read fName write fName;
        property Version: TTableRevisionNumber read fVersion write fVersion;
        property FieldDefs: TTableFieldDefList read fFieldDefs;
        property IndexDef: TTableIndexDef read fIndexDef;
    end;


implementation


uses
    SysUtils;

{ TTableFieldDef }

constructor TTableFieldDef.Create(const aFieldName: string; aFieldType: TTableFieldType; aFieldLen: integer);
begin
    inherited Create();
    fFieldName := aFieldName;
    fFieldType := aFieldType;
    fFieldLen := aFieldLen;
end;

{ TTableIndexDef }

constructor TTableIndexDef.Create(const aIndexText: string);
begin
    inherited Create();
    fIndexText := aIndexText;
end;

{ TTableStructDef }

constructor TTableStructDef.Create(aOwnsObjects: boolean = true);
begin
    inherited Create();
    fOwnsObjects := aOwnsObjects;
    fFieldDefs := TTableFieldDefList.Create; // cannot own fielddefs
    fIndexDef := TTableIndexDef.Create('');
    DefineStruct();
end;

destructor TTableStructDef.Destroy();
var
    x: integer;
begin
    fIndexDef.Free;

    if fOwnsObjects then
    begin
        for x := 0 to fFieldDefs.Count - 1 do
            fFieldDefs.Objects[x].Free;
    end;

    fFieldDefs.Free;

    inherited;
end;

procedure TTableStructDef.DefineStruct;
begin
    DoDefineStruct();
end;

procedure TTableStructDef.DoDefineStruct;
begin
    //
end;

procedure TTableStructDef.InsertFieldDef(aFieldDef: TTableFieldDef; aIndex: integer);
begin
    fFieldDefs.InsertObject(aIndex, aFieldDef.FieldName, aFieldDef);
end;

procedure TTableStructDef.AddField(const aFieldName: string; aFieldType: TTableFieldType; aFieldLen: integer);
begin
    AddFieldAt(aFieldName, aFieldType, aFieldLen, INT_TABLEFIELD_NO_INDEX);
end;

procedure TTableStructDef.AddFieldAt(const aFieldName: string; aFieldType: TTableFieldType;
    aFieldLen: integer; aFieldPosition: integer);
begin
    if aFieldPosition = INT_TABLEFIELD_NO_INDEX then
        aFieldPosition := fFieldDefs.Count;
    InsertFieldDef(TTableFieldDef.Create(aFieldName, aFieldType, aFieldLen), aFieldPosition);
end;

procedure TTableStructDef.AddIndex(const aIndexText: string);
begin
    fIndexDef.IndexText := aIndexText;
end;

procedure TTableStructDef.DelField(const aFieldName: string);
var
    xIndex: integer;
begin
    xIndex := fFieldDefs.IndexOf(aFieldName);
    if xIndex < 0 then
        EXIT;
    fFieldDefs.Objects[xIndex].Free;
    fFieldDefs.Delete(xIndex);
end;

procedure TTableStructDef.ResizeField(const aFieldName: string; aSize: integer);
var
    xIndex: integer;
begin
    xIndex := fFieldDefs.IndexOf(aFieldName);
    if xIndex < 0 then
        EXIT;

    fFieldDefs[xIndex].FieldLen := aSize;
end;

procedure TTableStructDef.Assign(aTableStructDef: TTableStructDef);
var
    x: integer;
begin
    fFieldDefs.Clear();
    for x := 0 to aTableStructDef.FieldDefs.Count - 1 do
    begin
        self.InsertFieldDef(aTableStructDef.FieldDefs[x], x);
    end;
    fIndexDef.IndexText := aTableStructDef.IndexDef.IndexText;
end;

{ TTableFieldDefList }

function TTableFieldDefList.GetFieldDef(aIndex: integer): TTableFieldDef;
begin
    result := self.Objects[aIndex] as TTableFieldDef;
end;


end.
