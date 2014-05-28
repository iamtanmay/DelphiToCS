unit DelayInfo;
{ --------------------------------------------------------------------------------------------------
  Copyright © 2007 Zinsser Analytic GmbH - All rights reserved.
  Author       : Payman Kamali (pk)
  Description  : A frame which displays delay/timer action information
  --------------------------------------------------------------------------------------------------
  Revision History:
  date     op  method                       track-no improvement/change
  -------- --  ---------------------------  -------- -----------------------------------------------
  03.01.07 pk                                TN3744  New
  24.01.08 pk                                TN3998  The grid is no longer automatically sorted by priority
  24.01.08 pk  FindInsertRowIndex            TN3998  New: returns the sorted row index at which the new line can be inserted
  25.09.08 pk  Instance                      TN4241  New
  10.11.08 pk                                TN4280  Priority changed to ID
  06.04.09 pk                                TN4503  Changed from Frame to Form
  30.06.09 wl                                TN4635   Spaltenbreite und -höhe passen sich jetzt automatisch an
  10.08.09 wl                                TN4702   Strings werden jetzt direkt geladen
  20.07.10 pk                                TN5201  "Time" String Column changed into "Status" Progressbar Column
  21.07.10 wl                                TN5202   Nur für Windows 7: neue Schriftart "Segoe UI", Schriftgröße 9
  -------------------------------------------------------------------------------------------------- }


interface


uses
    Windows,
    Messages,
    SysUtils,
    Variants,
    Classes,
    Graphics,
    Controls,
    Forms,
    Dialogs,
    cxStyles,
    cxCustomData,
    cxGraphics,
    cxFilter,
    cxData,
    cxDataStorage,
    cxEdit,
    cxButtonEdit,
    cxGridLevel,
    cxGridCustomTableView,
    cxGridTableView,
    cxClasses,
    cxControls,
    cxGridCustomView,
    cxGrid,
    ExtCtrls,
    cxEditRepositoryItems,
    cxExtEditRepositoryItems,
    cxProgressBar,
    cxTrackBar,
    cxLookAndFeels,
    cxLookAndFeelPainters;

type
    TfraDelayInfo = class(TForm)
        grdDelays: TcxGrid;
        grdDelaysTableView1: TcxGridTableView;
        grdDelaysTableView1ColCancel: TcxGridColumn;
        grdDelaysTableView1ColID: TcxGridColumn;
        grdDelaysTableView1ColTime: TcxGridColumn;
        grdDelaysTableView1ColDescription: TcxGridColumn;
        grdDelaysTableView1ColStatus: TcxGridColumn;
        grdDelaysLevel1: TcxGridLevel;
        cxEditRepository1: TcxEditRepository;
        procedure grdDelaysTableView1ColCancelPropertiesButtonClick(Sender: TObject; AButtonIndex: Integer);
        procedure grdDelaysTableView1ColTimeGetProperties(Sender: TcxCustomGridTableItem;
            ARecord: TcxCustomGridRecord; var AProperties: TcxCustomEditProperties);
    private const
        cColIndexButton = 0;

    const
        cColIndexID = 1;

    const
        cColIndexStatus = 2;

    const
        cColIndexDescription = 3;

    const
        cColIndexIsCancelled = 4;

        class var uInstance: TfraDelayInfo;

    const
        cProgressBarNumTimes = 10;
        function DelayFindRowIndex(const aID: string): integer;
        function FindInsertRowIndex(const aID: string): integer;
    public
        constructor Create(AOwner: TComponent); override;
        destructor Destroy(); override;
        procedure DelayChangeTime(const aID: string; aProgressText: string; const aProgress: integer);
        procedure DelayInsertInfo(const aID: string; aProgressText, aDescription: string);
        procedure DelayDeleteInfo(const aID: string);
        function DelayIsCancelled(const aID: string): boolean;
        class procedure SetInstance(const aValue: TfraDelayInfo);
        class function Instance(): TfraDelayInfo;
    end;


implementation


{$R *.dfm}

uses
    GeneralTypes,
    ControlUtils;

class procedure TfraDelayInfo.SetInstance(const aValue: TfraDelayInfo);
begin
    uInstance := aValue;
end;

class function TfraDelayInfo.Instance: TfraDelayInfo;
begin
    result := uInstance;
end;

constructor TfraDelayInfo.Create(AOwner: TComponent);
begin
    inherited Create(AOwner);

    TControlUtils.ResetFontForWinXP(self);

    grdDelaysTableView1ColCancel.Caption := TLanguageString.Read('Cancel', 'Abbrechen');
    grdDelaysTableView1ColID.Caption := TLanguageString.Read('ID', 'ID');
    grdDelaysTableView1ColTime.Caption := TLanguageString.Read('Status', 'Status');
    grdDelaysTableView1ColDescription.Caption := TLanguageString.Read('Description', 'Beschreibung');
end;

destructor TfraDelayInfo.Destroy();
begin
    inherited;
end;

function TfraDelayInfo.DelayFindRowIndex(const aID: string): integer;
var
    x: integer;
begin
    result := -1;
    for x := 0 to grdDelaysTableView1.DataController.RecordCount - 1 do
    begin
        if grdDelaysTableView1.DataController.Values[x, cColIndexID] = aID then
        begin
            result := x;
            EXIT;
        end;
    end;
end;

procedure TfraDelayInfo.DelayChangeTime(const aID: string; aProgressText: string; const aProgress: integer);
var
    xRow: integer;
    xProperties: TcxProgressBarProperties;
begin
    xRow := DelayFindRowIndex(aID);
    xProperties := (cxEditRepository1.Items[xRow].Properties as TcxProgressBarProperties);

    // progress tex could be any string. It does not necessarily have to be a time string.
    xProperties.Text := aProgressText;
    if aProgress = -1 then
    begin
        // In this mode we dont know how much progress there is so every time this function is called we just move the bar a bit, and at
        // the max value we go back to 0. We want to some kind of progress so the user doesn't think that nothing is happening
        grdDelaysTableView1.DataController.Values[xRow, cColIndexStatus] :=
            (grdDelaysTableView1.DataController.Values[xRow, 2] +
            Round(xProperties.Max / cProgressBarNumTimes)) mod xProperties.Max;
    end
    else
    begin
        grdDelaysTableView1.DataController.Values[xRow, cColIndexStatus] := aProgress;
    end;
end;

function TfraDelayInfo.FindInsertRowIndex(const aID: string): integer;
var
    x: integer;
begin
    result := grdDelaysTableView1.DataController.RecordCount;

    for x := 0 to grdDelaysTableView1.DataController.RecordCount - 1 do
    begin
        if grdDelaysTableView1.DataController.Values[x, cColIndexID] > aID then
        begin
            result := x;
            EXIT;
        end;
    end;
end;

procedure TfraDelayInfo.DelayInsertInfo(const aID: string; aProgressText: string; aDescription: string);
var
    xRow: integer;
    xItem: TcxEditRepositoryProgressBar;
begin
    xRow := DelayFindRowIndex(aID);
    if xRow < 0 then
    begin
        xRow := FindInsertRowIndex(aID);
        xRow := grdDelaysTableView1.DataController.InsertRecord(xRow);
    end;

    grdDelaysTableView1.DataController.Values[xRow, cColIndexID] := aID;
    grdDelaysTableView1.DataController.Values[xRow, cColIndexStatus] := 0;
    grdDelaysTableView1.DataController.Values[xRow, cColIndexDescription] := aDescription;
    grdDelaysTableView1.DataController.Values[xRow, cColIndexIsCancelled] := true;

    // set the ProgressBar props
    xItem := cxEditRepository1.CreateItem(TcxEditRepositoryProgressBar) as TcxEditRepositoryProgressBar;
    // xItem.Properties.Marquee := true  20.07.10 pk The Marquee Mode doesn't work properly, it doesnt animate automatically like it is supposed to
    xItem.Properties.Text := aProgressText;
    xItem.Properties.BarStyle := cxbsAnimation;
end;

procedure TfraDelayInfo.DelayDeleteInfo(const aID: string);
var
    xRow: integer;
begin
    xRow := DelayFindRowIndex(aID);
    grdDelaysTableView1.DataController.DeleteRecord(xRow);
end;

function TfraDelayInfo.DelayIsCancelled(const aID: string): boolean;
var
    xRow: integer;
begin
    xRow := DelayFindRowIndex(aID);
    result := not grdDelaysTableView1.DataController.Values[xRow, cColIndexIsCancelled];
end;

procedure TfraDelayInfo.grdDelaysTableView1ColCancelPropertiesButtonClick(Sender: TObject;
    AButtonIndex: Integer);
var
    xRow: integer;
begin
    xRow := grdDelaysTableView1.Controller.FocusedRow.Index;
    grdDelaysTableView1.DataController.Values[xRow, cColIndexIsCancelled] := false;
end;

procedure TfraDelayInfo.grdDelaysTableView1ColTimeGetProperties(Sender: TcxCustomGridTableItem;
    ARecord: TcxCustomGridRecord; var AProperties: TcxCustomEditProperties);
var
    xRepIndex: integer;
    xID: string;
    xRow: integer;
begin
    if VarIsNull(ARecord.Values[cColIndexID]) then
        EXIT;

    xID := ARecord.Values[cColIndexID];

    if xID = '' then
        EXIT;

    xRow := DelayFindRowIndex(xID);

    xRepIndex := xRow;

    if xRepIndex >= cxEditRepository1.Count then
        EXIT;

    AProperties := cxEditRepository1.Items[ARecord.RecordIndex].Properties;
end;


end.
