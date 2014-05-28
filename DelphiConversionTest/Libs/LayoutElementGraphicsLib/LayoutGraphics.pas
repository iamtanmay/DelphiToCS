unit LayoutGraphics;
{ --------------------------------------------------------------------------------------------------
  Copyright © 2008 Zinsser Analytic GmbH - All rights reserved.
  Author       : Payman Kamali (pk)
  Description  : Classes and math functions for Coordinate System Relation
  --------------------------------------------------------------------------------------------------
  Revision History:
  date     op  method                       track-no improvement/change
  -------- --  ---------------------------  -------- -----------------------------------------------
  23.06.08 pk                                TN4139  Initial Revision
  10.03.09 pk                                TN4457  Changes needed for reimplementing Stacker Buttons
  17.04.09 pk  DoInitGraphicsInfo            TN4532  IsSizeVirtual := true
  19.08.09 wl                                TN4702   Strings werden jetzt direkt geladen
  -------------------------------------------------------------------------------------------------- }


interface


uses
    LayoutElementGraphicsInfo,
    LayoutElementGraphics,
    WorkspaceGraphics,
    IntfLayoutElementGraphicsDriver;

type
    TLayoutGraphics = class(TLayoutElementGraphics)
    private
        // fOnZoom      : TNotifyEvent;
        // function GetShape() : TPanel;

        // procedure XBorderClick(Sender: TObject);
        // procedure YBorderClick(Sender: TObject);
    protected
        function GetGraphicsDriverID: TLayoutElementGraphicsDriverID; override;
        function GetGraphicsInfo: TLayoutGraphicsInfo;
        procedure DoInitGraphicsInfo; override;
        procedure DoUpdateGraphicsInfo; override;
    public
        constructor Create();
        destructor Destroy(); override;
        procedure AddWorkspace(aWorkspaceGraphics: TWorkspaceGraphics);
        procedure UpdateShapeBounds;
        // property OnZoom : TNotifyEvent read fOnZoom write fOnZoom;
        // property Shape : TPanel read GetShape;
        property GraphicsInfo: TLayoutGraphicsInfo read GetGraphicsInfo;
    end;


implementation


uses
    Utility2,
    LayoutElementCallBackTypes;

{ TLayoutGraphics }

constructor TLayoutGraphics.Create();
begin
    inherited Create();

    // Align := alClient;
    self.Color := $009E9E9E;
    self.BorderType := gbtNone;
    {
      UpdateShapeBounds();

      Top  := gArmBoundManager.AllArmBound.BoundRect.Top;
      Left := gArmBoundManager.AllArmBound.BoundRect.Left;
      Width := gArmBoundManager.AllArmBound.BoundWidth; // + WBBorders.Left + WBBorders.Right;
      Height:= gArmBoundManager.AllArmBound.BoundHeight; //+ WBBorders.Top + WBBorders.Bottom;
    }
    { TODO : no access to gmodules }
    // gModules.AddBCReaders(self);
    // gModules.BCReaderSetBounds;

end;

destructor TLayoutGraphics.Destroy();
begin
    inherited;
end;

function TLayoutGraphics.GetGraphicsDriverID: TLayoutElementGraphicsDriverID;
begin
    result := ILayoutGraphicsDriver;
end;

procedure TLayoutGraphics.UpdateShapeBounds();
begin
    // gArmBoundManager.SetParent( self );
    // gArmBoundManager.UpdateBounds( WBBorders, Zoom );
end;
{
  function TLayoutGraphics.GetShape() : TPanel;
  begin
  //result := gArmBoundManager.AllArmBound.Shape;
  end;
}
// procedure TLayoutGraphics.CreatePopup;
// begin
// //self.AddPopupMenuItem( TPopupMenuInfoItem.Create( 'Zoom', False{checked},True{Enabled},ZoomClick{OnClick},'Zoom') );
// //ItemArray[1] := NewItem('-',0,False{checked},True{Enabled},nil{OnClick}, 0{HelpContext},'N1');
// //ItemArray[2] := NewItem(TResLoader.GetResString(26340{Border Width Left}),0,False{checked},true{Enabled},XBorderClick{OnClick}, 0{HelpContext},'XBorder');
// //ItemArray[3] := NewItem(TResLoader.GetResString(26350{Border Width Top}),0,False{checked},True{Enabled},YBorderClick{OnClick}, 0{HelpContext},'YBorder');
// //self.PopupMenu := NewPopupMenu(self.CanvasHandle, 'WBPopupMenu',paLeft,True{AutoPopup},ItemArray );
// end;

{ TODO -oPK -cWB : }
// procedure TLayoutGraphics.XBorderClick(Sender: TObject);
// var NewXBorder:integer;
// begin
// if not gmStrToIntTry(NewXBorder,gmSyncInputBox( TResLoader.GetResString(26010{Insert X-Border (0-200):,X-Border} ),
// '',IntToStr(WBBorders.Left))) then EXIT;
//
// if (NewXBorder<>WBBorders.Left) and (NewXBorder<200) and (NewXBorder>=0) then begin
// WBBorders.Left:=NewXBorder;
// //fCallbacks.OnXBorderClick( self ); //Load(FRunName,FLayoutName);
// //fBackgroundGraphics.UpdateShapeBounds();
// { TODO : no access to gModules }
// //gModules.BCReaderSetBounds;
// FZoomChanged:=true;
// end;
// end;
//
// procedure TLayoutGraphics.YBorderClick(Sender: TObject);
// var NewYBorder:integer;
// begin
// if not gmStrToIntTry(NewYBorder,gmSyncInputBox( TResLoader.GetResString(26020{Insert Y-Border (0-200):,Y-Border} ),
// '', IntToStr(WBBorders.Top))) then EXIT;
//
// if (NewYBorder<>WBBorders.Top) and (NewYBorder<120) and (NewYBorder>=0) then begin
// WBBorders.Top:=NewYBorder;
// //fCallbacks.OnYBorderClick( self ); //Load(FRunName,FLayoutName);
// //fBackgroundGraphics.UpdateShapeBounds();
// { TODO : no access to gModules }
// //gModules.BCReaderSetBounds;
// FZoomChanged:=true;
// end;
// end;

procedure TLayoutGraphics.AddWorkspace(aWorkspaceGraphics: TWorkspaceGraphics);
begin
    // fWorkspaces.Add( aWorkspaceGraphics );
    aWorkspaceGraphics.Parent := self;
    // aWorkspaceGraphics.CoordSystem.ReflectY := true;
    // aWorkspaceGraphics.ScreenCoordSystemRelation := fScreenCoordSystemRelation;
end;

function TLayoutGraphics.GetGraphicsInfo: TLayoutGraphicsInfo;
begin
    result := fGraphicsInfo as TLayoutGraphicsInfo;
end;

procedure TLayoutGraphics.DoInitGraphicsInfo();
begin
    fGraphicsInfo := TLayoutGraphicsInfo.Create();
    fGraphicsInfo.SizeX := 10000;
    fGraphicsInfo.SizeY := 10000;
    fGraphicsInfo.SizeZ := 10;
    fGraphicsInfo.IsSizeVirtual := true;
end;

procedure TLayoutGraphics.DoUpdateGraphicsInfo;
begin
    inherited;
    {
      with self.GraphicsDriver.GraphicsInfo as TLayoutGraphicsInfo do begin
      self.CoordSystem.TranslateX := self.GraphicsInfo.PosX;
      self.CoordSystem.TranslateY := self.GraphicsInfo.PosY;
      end;
    }
end;


end.
