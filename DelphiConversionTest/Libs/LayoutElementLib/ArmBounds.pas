unit ArmBounds;
{--------------------------------------------------------------------------------------------------
 Copyright © 2007 Zinsser Analytic GmbH - All rights reserved.
 Author       : Payman Kamali (pk)
 Description  : devices for robotic arms (gripper and pipetting arm)
 --------------------------------------------------------------------------------------------------
 Revision History:
 date     op  method                       track-no improvement/change
 -------- --  ---------------------------  -------- -----------------------------------------------
 05.09.07 pk                                         Initial Revision
 09.11.07 pk  fZMax                         TN3924   Changed from steps to mm
 07.01.08 pk  fZMax                         TN3971   removed
 --------------------------------------------------------------------------------------------------}

interface
uses
    Controls, ExtCtrls, Graphics, Contnrs, Types,
    CommonTypes, AppTypes;
type

  TArmBound = class
    private
      function GetBoundHeight: integer;
      function GetBoundWidth: integer;
    protected
      fName : string;
      fXMin : TPosMM;
      fXMax : TPosMM;
      fYMin : TPosMM;
      fYMax : TPosMM;
      fBoundRect : TRect;
      fColor : TColor;
      fShape : TPanel;
    public
      constructor Create ( const aName : string; aXMin : TPosMM; aXMax : TPosMM; aYMin : TPosMM; aYMax : TPosMM; aColor : TColor );
      destructor Destroy(); override;
      class function CreateShape( aParent : TWinControl ) : TPanel;
      procedure UpdateBounds( aOffset : TWBBorder; aZoom: single);
      procedure SetParent(aParent: TWinControl);
      procedure SetMouseEvents( aOnDragOver : TDragOverEvent; aOnDragDrop : TDragDropEvent; aOnMouseDown : TMouseEvent );
      property XMin : TPosMM read fXMin write fXMin;
      property XMax : TPosMM read fXMax write fXMax;
      property YMin : TPosMM read fYMin write fYMin;
      property YMax : TPosMM read fYMax write fYMax;
      property BoundRect : TRect read fBoundRect write fBoundRect;
      property BoundWidth : integer read GetBoundWidth;
      property BoundHeight : integer read GetBoundHeight;
      property Shape : TPanel read fShape;
      property Name : string read fName; 
  end;

  TAllArmBound = class( TArmBound )
    public
      constructor Create ( aXMin : TPosMM; aXMax : TPosMM; aYMin : TPosMM; aYMax : TPosMM );
  end;

  TArmBoundList = class( TObjectList )
    private
      function GetArmBound(aIndex: integer): TArmBound;
    public
      property ArmBound[ aIndex : integer ] : TArmBound read GetArmBound; default;
  end;

  TArmBoundManager = class
    private
      fArmBounds : TArmBoundList;
      fAllArmBound : TAllArmBound;
      function FindArmBoundByShape(aShape: TObject): TArmBound;
    public
      constructor Create();
      procedure AddArmBound( aArmBound : TArmBound );
      procedure SelectArmBound( const aBoundName : string );
      procedure BringBoundToFront(aShape: TObject);
      procedure SendBoundToBack(aShape: TObject);
      procedure SetAllArmBound( aAllArmBound : TAllArmBound );
      procedure UpdateBounds( aOffset : TWBBorder; aZoom: single );
      procedure SetMouseEvents( aOnDragOver : TDragOverEvent; aOnDragDrop : TDragDropEvent; aOnMouseDown : TMouseEvent );
      procedure SetParent( aParent: TWinControl );
      property ArmBounds : TArmBoundList read fArmBounds;
      property AllArmBound : TAllArmBound read fAllArmBound;
  end;


var
    gArmBoundManager : TArmBoundManager;
    
implementation

{ TArmBound }

constructor TArmBound.Create( const aName : string; aXMin : TPosMM; aXMax : TPosMM; aYMin : TPosMM; aYMax : TPosMM; aColor : TColor );
begin
    inherited Create();
    fName := aName;
    fXMin := aXMin;
    fXMax := aXMax;
    fYMin := aYMin;
    fYMax := aYMax;
    fColor := aColor;
    fBoundRect := Rect( 0,0,0,0 );
    fShape := CreateShape( nil );
end;

destructor TArmBound.Destroy();
begin
    fShape.Free;
    inherited;
end;

class function TArmBound.CreateShape( aParent : TWinControl ) : TPanel;
begin
    result := TPanel.Create(aParent);
    result.ParentBackground := false;
    result.Parent := aParent;
    result.BevelInner := bvNone;
    result.Visible := false;
end;

procedure TArmBound.SetParent( aParent : TWinControl );
begin
    fShape.Parent := aParent;
    fShape.Color := fColor;
end;

procedure TArmBound.UpdateBounds( aOffset : TWBBorder; aZoom : single );
begin
    fBoundRect := Rect( aOffset.Left + Round( fXMin * aZoom ),
                        aOffset.Top  + Round( fYMin * aZoom ),
                        aOffset.Left + Round( fXMax * aZoom ),
                        aOffset.Top  + Round( fYMax * aZoom ) );

    fShape.BoundsRect := fBoundRect;
end;


function TArmBound.GetBoundHeight: integer;
begin
    result := fBoundRect.Bottom - fBoundRect.Top;
end;

function TArmBound.GetBoundWidth: integer;
begin
    result := fBoundRect.Right - fBoundRect.Left;
end;

procedure TArmBound.SetMouseEvents(aOnDragOver: TDragOverEvent;
  aOnDragDrop: TDragDropEvent; aOnMouseDown: TMouseEvent);
begin
    fShape.OnDragOver := aOnDragOver;
    fShape.OnDragDrop := aOnDragDrop;
    fShape.OnMouseDown := aOnMouseDown;
end;

{ TAllArmBound }

constructor TAllArmBound.Create( aXMin : TPosMM; aXMax : TPosMM; aYMin : TPosMM; aYMax : TPosMM );
begin
    inherited Create( 'ALL', aXMin, aXMax, aYMin, aYMax, clGray );
end;

{ TArmBoundList }

function TArmBoundList.GetArmBound(aIndex: integer): TArmBound;
begin
    result := inherited Items[ aIndex ] as TArmBound;
end;

{ TArmBoundManager }
constructor TArmBoundManager.Create;
begin
    inherited Create();
    fArmBounds := TArmBoundList.Create();
    fAllArmBound := nil;
end;

procedure TArmBoundManager.AddArmBound(aArmBound: TArmBound);
begin
    fArmBounds.Add( aArmBound );
end;


procedure TArmBoundManager.SetAllArmBound(aAllArmBound: TAllArmBound);
begin
    fAllArmBound := aAllArmBound;
end;

procedure TArmBoundManager.SetParent(aParent: TWinControl);
var
    x : integer;
begin
    fAllArmBound.SetParent( aParent );
    fAllArmBound.Shape.Visible := true;
    for x := 0 to fArmBounds.Count - 1 do
        fArmBounds[ x ].SetParent( aParent );
end;

procedure TArmBoundManager.SetMouseEvents(aOnDragOver: TDragOverEvent;
  aOnDragDrop: TDragDropEvent; aOnMouseDown: TMouseEvent);
var
    x : integer;
begin
    fAllArmBound.SetMouseEvents( aOnDragOver, aOnDragDrop, aOnMouseDown );
    for x := 0 to fArmBounds.Count - 1 do
        fArmBounds[ x ].SetMouseEvents( aOnDragOver, aOnDragDrop, aOnMouseDown );
end;

procedure TArmBoundManager.UpdateBounds(aOffset: TWBBorder; aZoom: single);
var
    x : integer;
begin
    fAllArmBound.UpdateBounds( aOffset, aZoom );
    for x := 0 to fArmBounds.Count - 1 do
        fArmBounds[ x ].UpdateBounds( aOffset, aZoom );
end;

procedure TArmBoundManager.SelectArmBound(const aBoundName: string);
var
    x : integer;
begin
    for x := 0 to fArmBounds.Count - 1 do begin
        fArmBounds[ x ].Shape.Visible :=  ( fArmBounds[ x ].Name = aBoundName );
    end;
end;

function TArmBoundManager.FindArmBoundByShape( aShape : TObject ) : TArmBound;
var
    x : integer;
begin
    result := nil;
    for x := 0 to fArmBounds.Count - 1 do begin
        if fArmBounds[ x ].Shape = aShape then begin
            result := fArmBounds[ x ];
            EXIT;
        end;
    end;
end;

procedure TArmBoundManager.BringBoundToFront( aShape : TObject );
var
    xArmBound : TArmBound;
begin
    xArmBound := FindArmBoundByShape( aShape );
    if not Assigned( xArmBound ) then EXIT;
    xArmBound.Shape.BringToFront;
end;

procedure TArmBoundManager.SendBoundToBack( aShape : TObject );
var
    xArmBound : TArmBound;
begin
    xArmBound := FindArmBoundByShape( aShape );
    if not Assigned( xArmBound ) then EXIT;
    xArmBound.Shape.SendToBack;
end;

end.
