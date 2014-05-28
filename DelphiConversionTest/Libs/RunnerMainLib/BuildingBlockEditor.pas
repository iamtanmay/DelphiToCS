{ -----------------------------------------------------------------------------------------------------------
  Copyright © 2012 Zinsser Analytic GmbH - All rights reserved.
  Author       : Wolfgang Lyncke (wl)
  Description  :
  -----------------------------------------------------------------------------------------------------------
  Revision History:
  date     op  method                              track-no improvement/change
  -------- --  ----------------------------------  -------- -------------------------------------------------
  10.12.12 wl                                      TN6045   Initial Revision
  21.03.13 wl  ImageList1                          TN6045   neu: wird zur Laufzeit gefüllt
  21.03.13 wl                                      TN6045   Festlegung auf 64x64-Format
  30.08.13 wl  ReadAllBuildingBlocks               TN6236   an MethodDataAdaptor angepasst
  08.04.14 ts                                      TN6391   kein Fehler, wenn Icon nicht gefunden wird
  08.04.14 ts  ReadItems                           TN6391   ImageIndex ist auch 0 wenn kein Bild angegeben
  ----------------------------------------------------------------------------------------------------------- }

unit BuildingBlockEditor;


interface


uses
    Windows,
    Messages,
    SysUtils,
    Variants,
    Generics.Collections,
    Classes,
    Graphics,
    Controls,
    Forms,
    Dialogs,
    ExtCtrls,
    ComCtrls,
    StdCtrls,
    BuildingBlockMethodEditor,
    MethodSettingsDataAdaptor,
    ViewItem,
    ButtonGroup,
    ImgList;

type
    TfrmBuildingBlockEditor = class(TForm)
        lvBuildingBlocks: TListView;
        Panel3: TPanel;
        btnOK: TButton;
        Button2: TButton;
        ImageList1: TImageList;
        Splitter1: TSplitter;
        procedure FormCreate(Sender: TObject);
        procedure btnOKClick(Sender: TObject);
        procedure lvBuildingBlocksStartDrag(Sender: TObject; var DragObject: TDragObject);
        procedure lvBuildingBlocksEndDrag(Sender, Target: TObject; X, Y: Integer);
    private
        fEditor: TfrmBuildingBlockMethodEditor;
        function GetAddMethodIndexEvent(const aMethodName: string): integer;
    public
        procedure LoadMethod(const aItem: TViewItem);
        procedure ReadItems;
        class function ReadAllBuildingBlocks: TArray<TMethodSettingsRec>;
    end;


implementation


{$R *.dfm}

uses
    ZARunnerMain,
    DockableForm;

{ TfrmMethodSelection }

procedure TfrmBuildingBlockEditor.btnOKClick(Sender: TObject);
begin
    fEditor.Save;
    Close;
end;

procedure TfrmBuildingBlockEditor.FormCreate(Sender: TObject);
begin
    self.ReadItems();
end;

function TfrmBuildingBlockEditor.GetAddMethodIndexEvent(const aMethodName: string): integer;
var
    x: integer;
begin
    for x := 0 to lvBuildingBlocks.Items.Count - 1 do
    begin
        if (lvBuildingBlocks.Items[x].Caption = aMethodName) then
            EXIT(lvBuildingBlocks.Items[x].ImageIndex);
    end;
    EXIT(-1);
end;

procedure TfrmBuildingBlockEditor.LoadMethod(const aItem: TViewItem);
begin
    fEditor := TfrmBuildingBlockMethodEditor.Create(Application, aItem.Name, ImageList1,
        MainForm.AfterSaveStatusChanged, self.GetAddMethodIndexEvent);
    fEditor.BorderStyle := bsNone;
    fEditor.Align := alClient;
    fEditor.Parent := self;
    fEditor.Visible := true;
    fEditor.FirstLoad;
    self.Caption := 'Method: ' + aItem.name;
end;

procedure TfrmBuildingBlockEditor.lvBuildingBlocksEndDrag(Sender, Target: TObject; X, Y: Integer);
begin
    //
end;

procedure TfrmBuildingBlockEditor.lvBuildingBlocksStartDrag(Sender: TObject; var DragObject: TDragObject);
begin
    //
end;

class function TfrmBuildingBlockEditor.ReadAllBuildingBlocks: TArray<TMethodSettingsRec>;
var
    xDA: TMethodSettingsDataAdaptor;
begin
    xDA := TMethodSettingsDataAdaptor.Create;
    try
        EXIT(xDA.ReadBuildingBlocks());
    finally
        xDA.Free;
    end;
end;

procedure TfrmBuildingBlockEditor.ReadItems;
var
    x, xImageIndex: integer;
    xMethods: TArray<TMethodSettingsRec>;
    xItem: TListItem;
    xPicture: TPicture;
    xImageFileName: string;
begin
    // Füllen der Liste
    xMethods := self.ReadAllBuildingBlocks();

    lvBuildingBlocks.visible := false;
    lvBuildingBlocks.SortType := stNone;

    lvBuildingBlocks.Items.Clear;
    for x := 0 to high(xMethods) do
    begin
        xItem := lvBuildingBlocks.Items.Add;
        xItem.Caption := xMethods[x].MethodName;

        xImageFileName := xMethods[x].ImageFileName;
        if (xImageFileName <> '') then
        begin
            xPicture := TPicture.Create;
            try
                try
                    xPicture.Bitmap.LoadFromFile(xImageFileName);
                    xPicture.Bitmap.SetSize(64, 64);
                    xImageIndex := ImageList1.Add(xPicture.Bitmap, nil);
                except
                    xImageIndex := 0;
                end;
                // ImageList1.SetSize(xPicture.Bitmap.Width, xPicture.Bitmap.Height);
                xItem.ImageIndex := xImageIndex;
            finally
                FreeAndNil(xPicture);
            end;
        end
        else
            xItem.ImageIndex := 0;
    end;

    lvBuildingBlocks.SortType := stText;
    lvBuildingBlocks.visible := true;
end;


end.
