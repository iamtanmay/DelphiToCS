{ --------------------------------------------------------------------------------------------------
  Copyright © 2008 Zinsser Analytic GmbH - All rights reserved.
  Author       : Payman Kamali (pk)
  Description  :
  --------------------------------------------------------------------------------------------------
  Revision History:
  date     op  method                       track-no improvement/change
  -------- --  ---------------------------  -------- -----------------------------------------------
  23.06.08 pk                                TN4139  Initial Revision
  07.07.08 pk  GetName                       TN4139  New
  16.07.08 pk  SceneChanged                  TN4139  New
  31.07.08 pk  Cursor                        TN4139  New
  10.03.09 pk                                TN4457  Changes needed for reimplementing Stacker Buttons
  13.03.09 pk                                TN4463  Changes needed for reimplementing carrier drag move
  13.03.09 pk                                TN4466  Changes need for implementing rack carrier slot change via drag and drop
  19.05.09 wl  GetLeft, GetTop               TN4466   entfernt
  12.08.09 pk  Finalize                      TN4713  New
  19.11.09 pk  TGraphicsInterfacedObject     TN4892  New
  29.10.10 pk  Invalidate                    TN5268  New
  10.04.13 wl                                TN6045   uses geändert
  -------------------------------------------------------------------------------------------------- }

unit IntfLayoutElementGraphicsDriver;


interface


uses
    MatrixMath,
    CoordSystemMath,
    LayoutElementGraphicsInfo,
    LayoutElementCallbackTypes,
    TypeInfo;

type
    TLayoutElementGraphicsDriverCallbacks = class
    public
        MouseClickCallback: TMouseClickCallback;
        MouseDblClickCallback: TMouseDblClickCallback;
        MouseMoveCallback: TMouseMoveCallback;
        MouseDownCallback: TMouseDownCallback;
        MouseUpCallback: TMouseUpCallback;
        MouseDragOverCallback: TMouseDragOverCallback;
        MouseDragDropCallback: TMouseDragDropCallback;
        IsDragableCallback: TIsDragableCallback;
        PopupCallback: TPopupCallback;
        ShowHintCallback: TShowHintCallback;
        PosChangedCallback: TPosChangedCallback;
    end;

    ILayoutElementGraphicsDriver = interface
        ['{CE5D58A0-7C70-4DF3-A9F1-A870A04CFEB4}']
        function GetName(): string;
        procedure SetName(const aName: string);
        function GetVisible(): boolean;
        procedure SetVisible(aVisible: boolean);
        function GetColor(): integer;
        procedure SetColor(aColor: integer);
        function GetBounds(): TBounds;
        function GetBorderType: TGraphicsBorderType;
        procedure SetBorderType(const aValue: TGraphicsBorderType);
        // function GetLeft: integer;
        // function GetTop: integer;
        function GetCoordCalculator(): TCoordCalculator;
        procedure SetCoordCalculator(aCoordCalculator: TCoordCalculator);
        function GetParent(): ILayoutElementGraphicsDriver;
        procedure SetParent(const aParent: ILayoutElementGraphicsDriver);

        function GetCallbacks: TLayoutElementGraphicsDriverCallbacks;
        procedure SetTag(aTag: TObject);
        function GetTag(): TObject;
        function GetCursor(): TGraphicsMouseCursor;
        procedure SetCursor(const aValue: TGraphicsMouseCursor);
        function GetCaption(): string;
        procedure SetCaption(const aValue: string);
        function SceneToClient(aX, aY, aZ: double): TPoint4d;
        function ClientToScreen(aX, aY, aZ: double): TPoint4d;
        function ClientToScene(aX, aY, aZ: double): TPoint4d;
        procedure Draw();
        function GetGraphicsInfo: TLayoutElementGraphicsInfo;
        procedure SetSettings(aSettings: TLayoutElementGraphicsInfo);
        procedure AddChild(aChild: ILayoutElementGraphicsDriver);
        procedure RemoveChild(aChild: ILayoutElementGraphicsDriver);
        procedure SceneChanged();
        procedure PosChangeDragStart(const aRefX, aRefY, aRefZ: double);
        procedure Finalize();
        procedure Invalidate(const aErase: boolean);

        property name: string read GetName write SetName;
        property Visible: boolean read GetVisible write SetVisible;
        property Color: integer read GetColor write SetColor;
        // property Left : integer read GetLeft;
        // property Top : integer read GetTop;
        property CoordCalculator: TCoordCalculator read GetCoordCalculator write SetCoordCalculator;
        property Parent: ILayoutElementGraphicsDriver read GetParent write SetParent;
        property Callbacks: TLayoutElementGraphicsDriverCallbacks read GetCallbacks;
        property Tag: TObject read GetTag write SetTag;
        property GraphicsInfo: TLayoutElementGraphicsInfo read GetGraphicsInfo;
        property Cursor: TGraphicsMouseCursor read GetCursor write SetCursor;
        property Bounds: TBounds read GetBounds;
        property BorderType: TGraphicsBorderType read GetBorderType write SetBorderType;
        property Caption: string read GetCaption write SetCaption;
    end;

    TGraphicsInterfacedObject = class(TObject, IInterface)
    protected
        FRefCount: Integer;
        function QueryInterface(const IID: TGUID; out Obj): HResult; stdcall;
        function _AddRef: Integer; virtual; stdcall;
        function _Release: Integer; virtual; stdcall;
    public
        procedure AfterConstruction; override;
        procedure BeforeDestruction; override;
        class function NewInstance: TObject; override;
        property RefCount: Integer read FRefCount;
    end;

    TLayoutElementGraphicsDriverID = TGUID;
    TLayoutElementGraphicsDriverClass = class of TGraphicsInterfacedObject;

    TLayoutElementGraphicsDriverTypeInfo = class(TTypeInfo)
    protected
        fDriverClass: TLayoutElementGraphicsDriverClass;
        fGraphicsType: string;
        function GetGraphicsType(): string;
    public
        constructor Create(const aTypeName, aTypeInfoVersion, aLibName, aLibVersion: string;
            aDriverClass: TLayoutElementGraphicsDriverClass; aGraphicsType: string);
        function CreateDriver(): ILayoutElementGraphicsDriver; virtual; abstract;
        function CreateDriverSettings(): TLayoutElementGraphicsInfo; virtual; abstract;
        function SupportsDriver(aDriverID: TLayoutElementGraphicsDriverID): boolean;
        property GraphicsType: string read GetGraphicsType;
    end;

    ILayoutGraphicsDriver = interface(ILayoutElementGraphicsDriver)
        ['{85084EC7-01BB-403F-A5CF-6E9473C90956}']
    end;

    IWorkspaceGraphicsDriver = interface(ILayoutElementGraphicsDriver)
        ['{FA395E3B-64CF-4703-B896-E217CE3549F0}']
    end;

    ICarrierGraphicsDriver = interface(ILayoutElementGraphicsDriver)
        ['{97E887CB-1EF9-4D5F-BC6C-9F93204ACD35}']
    end;

    ICarrierSlotGraphicsDriver = interface(ILayoutElementGraphicsDriver)
        ['{BB799ADE-B89C-4622-B41E-FBE2DBE65EC4}']
    end;

    IRackGraphicsDriver = interface(ILayoutElementGraphicsDriver)
        ['{2FF832C2-9E51-451C-B538-190CDB580CB5}']
    end;

    IRackWellGraphicsDriver = interface(ILayoutElementGraphicsDriver)
        ['{2C11D4AD-B366-472C-8877-CE73644F13A4}']
    end;

    IRubberBandGraphicsDriver = interface(ILayoutElementGraphicsDriver)
        ['{CFB7E23C-1612-4CF0-9273-5AF105849327}']
    end;

    IBasicControlGraphicsDriver = interface(ILayoutElementGraphicsDriver)
        ['{D5FA28A9-D2FC-40D2-84FA-3FBBCE61CD41}']
        function GetSizeX: double;
        function GetSizeY: double;
        function GetSizeZ: double;
        procedure SetSizeX(const aValue: double);
        procedure SetSizeY(const aValue: double);
        procedure SetSizeZ(const aValue: double);
        property SizeX: double read GetSizeX write SetSizeX;
        property SizeY: double read GetSizeY write SetSizeY;
        property SizeZ: double read GetSizeZ write SetSizeZ;

    end;

    IPanelControlGraphicsDriver = interface(IBasicControlGraphicsDriver)
        ['{2C306149-75EB-47F1-AA87-A9EDD555EE3F}']
    end;

    IButtonControlGraphicsDriver = interface(IBasicControlGraphicsDriver)
        ['{7A8B494E-8BA5-4803-8DD4-B963C6FDE2D8}']
    end;


implementation


uses
    SysUtils,
    Windows;

{ TLayoutElementGraphicsDriverTypeInfo }

constructor TLayoutElementGraphicsDriverTypeInfo.Create(const aTypeName, aTypeInfoVersion, aLibName,
    aLibVersion: string; aDriverClass: TLayoutElementGraphicsDriverClass; aGraphicsType: string);
begin
    inherited Create(aTypeName, aTypeInfoVersion, aLibName, aLibVersion);
    fDriverClass := aDriverClass;
    fGraphicsType := aGraphicsType;
end;

function TLayoutElementGraphicsDriverTypeInfo.GetGraphicsType: string;
begin
    result := fGraphicsType;
end;

function TLayoutElementGraphicsDriverTypeInfo.SupportsDriver
    (aDriverID: TLayoutElementGraphicsDriverID): boolean;
begin
    result := Supports(fDriverClass, aDriverID);
end;

{ TGraphicsInterfacedObject }

procedure TGraphicsInterfacedObject.AfterConstruction;
begin
    // Release the constructor's implicit refcount
    InterlockedDecrement(FRefCount);
end;

procedure TGraphicsInterfacedObject.BeforeDestruction;
begin
    // if RefCount <> 0 then
    // Error(reInvalidPtr);
end;

// Set an implicit refcount so that refcounting
// during construction won't destroy the object.
class function TGraphicsInterfacedObject.NewInstance: TObject;
begin
    Result := inherited NewInstance;
    TGraphicsInterfacedObject(Result).FRefCount := 1;
end;

function TGraphicsInterfacedObject.QueryInterface(const IID: TGUID; out Obj): HResult;
begin
    if GetInterface(IID, Obj) then
        Result := 0
    else
        Result := E_NOINTERFACE;
end;

function TGraphicsInterfacedObject._AddRef: Integer;
begin
    Result := InterlockedIncrement(FRefCount);
end;

function TGraphicsInterfacedObject._Release: Integer;
begin
    Result := InterlockedDecrement(FRefCount);
    if Result = 0 then
        Destroy;
end;


end.
