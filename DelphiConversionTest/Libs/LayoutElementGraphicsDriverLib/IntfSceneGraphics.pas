unit IntfSceneGraphics;


{ --------------------------------------------------------------------------------------------------
  Copyright © 2008 Zinsser Analytic GmbH - All rights reserved.
  Author       : Payman Kamali (pk)
  Description  :
  --------------------------------------------------------------------------------------------------
  Revision History:
  date     op  method                       track-no improvement/change
  -------- --  ---------------------------  -------- -----------------------------------------------
  23.06.08 pk                                TN4139  Initial Revision
  16.07.08 pk  SceneChanged                  TN4139  Removed
  04.08.08 pk  PanX, PanY                    TN4139  New
  17.04.09 pk  SetCameraMode, FitToScreen    TN4532  Now public property
  -------------------------------------------------------------------------------------------------- }
interface


uses
    IntfLayoutElementGraphicsDriver;

type
    TBasicCallBack = procedure(aSender: TObject) of object;
    TSceneGraphicsCameraMode = (scmNone, scmPan, scmZoom);

    ISceneGraphicsDriver = interface(ILayoutElementGraphicsDriver)
        ['{4C2A7C2B-F5D4-4AF6-A2F1-C11D2DDA4064}']
        procedure SetBackgroundParent(aBackgroundGraphicsParent: TObject);
        procedure SetZoom(aZoom: double);
        function GetZoom(): double;
        procedure SetPanX(const aValue: double);
        function GetPanX(): double;
        procedure SetPanY(const aValue: double);
        function GetPanY(): double;
        function GetCanvas: TObject;
        procedure SetCameraMode(const aMode: TSceneGraphicsCameraMode);
        procedure FitToScreen();
        property Zoom: double read GetZoom write SetZoom;
        property PanX: double read GetPanX write SetPanX;
        property PanY: double read GetPanY write SetPanY;
        property Canvas: TObject read GetCanvas;
    end;


implementation


end.
