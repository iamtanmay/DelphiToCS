unit BasicControlGraphics;


interface


uses
    LayoutElementGraphics,
    IntfLayoutElementGraphicsDriver,
    LayoutElementGraphicsInfo;

type
    TBasicControlGraphics = class(TLayoutElementGraphics)
    private
        function GetSizeX: double;
        function GetSizeY: double;
        function GetSizeZ: double;
        procedure SetSizeX(const aValue: double);
        procedure SetSizeY(const aValue: double);
        procedure SetSizeZ(const aValue: double);
        function GetGraphicsDriver: IBasicControlGraphicsDriver;
        function GetGraphicsInfo: TBasicControlGraphicsInfo;
    public
        procedure SetBounds(const aPosX, aPosY, aPosZ, aSizeX, aSizeY, aSizeZ: double);
        property GraphicsDriver: IBasicControlGraphicsDriver read GetGraphicsDriver;
        property GraphicsInfo: TBasicControlGraphicsInfo read GetGraphicsInfo;
        property SizeX: double read GetSizeX write SetSizeX;
        property SizeY: double read GetSizeY write SetSizeY;
        property SizeZ: double read GetSizeZ write SetSizeZ;
    end;


implementation


uses
    SysUtils;

{ TBasicControlGraphics }

function TBasicControlGraphics.GetGraphicsDriver: IBasicControlGraphicsDriver;
begin
    Supports(fGraphicsDriver, IBasicControlGraphicsDriver, result);
end;

function TBasicControlGraphics.GetGraphicsInfo: TBasicControlGraphicsInfo;
begin
    result := fGraphicsInfo as TBasicControlGraphicsInfo;
end;

function TBasicControlGraphics.GetSizeX: double;
begin
    result := self.GraphicsInfo.SizeX;
end;

function TBasicControlGraphics.GetSizeY: double;
begin
    result := self.GraphicsInfo.SizeY;
end;

function TBasicControlGraphics.GetSizeZ: double;
begin
    result := self.GraphicsInfo.SizeZ;
end;

procedure TBasicControlGraphics.SetSizeX(const aValue: double);
begin
    self.GraphicsInfo.SizeX := aValue;
end;

procedure TBasicControlGraphics.SetSizeY(const aValue: double);
begin
    self.GraphicsInfo.SizeY := aValue;
end;

procedure TBasicControlGraphics.SetSizeZ(const aValue: double);
begin
    self.GraphicsInfo.SizeZ := aValue;
end;

procedure TBasicControlGraphics.SetBounds(const aPosX, aPosY, aPosZ, aSizeX, aSizeY, aSizeZ: double);
begin
    self.PosX := aPosX;
    self.PosY := aPosY;
    self.PosZ := aPosZ;
    self.SizeX := aSizeX;
    self.SizeY := aSizeY;
    self.SizeZ := aSizeZ;
end;


end.
