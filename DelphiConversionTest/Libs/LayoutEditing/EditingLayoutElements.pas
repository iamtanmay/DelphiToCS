{ --------------------------------------------------------------------------------------------------
  LISSY LAYOUTER
  --------------------------------------------------------------------------------------------------
  Objekte, die zum Editieren von Layouts benötigt werden
  --------------------------------------------------------------------------------------------------
  Datum    op  function/procedure          Änderung / Neuerung
  -------- --  --------------------------  --------------------------------------------------------------
  14.02.00 wl  TSetupArm                   über Popup-Menü editierbar
  15.02.00 wl  TDragHandle                 Zeilen gekürzt
  22.02.00 wl  NewLayout/SaveLayoutAs      entfernt
  WBDragOver                  Accept:=false oder true
  10.03.00 wl  TSetupRack.CreatePopup      Bei Edit Stacker Content erscheint wieder das richtige Fenster
  20.03.00 wl  TSetupCarrier.CreatePopup   wird unabhängig von CreateS aufgerufen: Edit Stacker Content erscheint wieder
  20.03.00 wl  TSetupRack.InsertIDClick    '' wird jetzt auch wieder als ID akzeptiert
  21.03.00 mo  TSetupWorkbench.CreateWB    WBShape.OnMouseDown   := nil;  - kein Popup mit zoom Änderung
  22.03.00 wl  TSetupRack.CreatePopup      Menüpunkt "Edit Rack Type" entfernt
  22.03.00 mo  TSetupTip.MouseDown1        im EvaluateCarrier modus disabled
  15.05.00 wl                              neu: Main.pnLEdit
  16.05.00 wl  TSetupRack.MouseDown1       nach CheckTubePos werden die Motorpositionen geändert
  16.05.00 wl  TSetupCarrier.SetWB_X_Steps,SetWB_Y_Steps,SetWB_Z_Steps (von Carrier hierher)
  16.05.00 wl  TSetupRack.SetWB_X_Steps,SetWB_Y_Steps,SetWB_Z_Steps  ändert auch die Layoutdaten des Racks
  24.05.00 wl  TDragHandle.GetModifiedRect  Fehler beseitigt: Carrier wurde bei jedem Doppelklick (ohne Bewegen) 2 Pixel verschoben
  24.05.00 wl  TSetupRack                  Achtung !! Workbench ist jetzt Parent, Carrier bleibt Owner
  24.05.00 wl  TDragHandle.WMLButtonUp     da Carrier nicht mehr Parent der Racks ist, müssen alle Rackpositionen neu bestimmt werden
  24.05.00 wl  TSetupCarrier.DefineRack    zusätzliche Berechnung von (Rack.) Left & Top entfällt
  24.05.00 wl  TSetupWorkbench.WBDragDrop  X- und Y-Werte für InsertNewCarrier korrigiert
  24.05.00 wl  TSetupWorkbench.CreateWB    KV 5.1: Default: AutoPopup=false
  26.05.00 wl  TSetupRack.MouseDown1       ruft gmCheckTubePos auf
  05.07.00 wl                              TRackNames statt RackNameArray
  11.10.01 mo                              TN1067 TWorkbench => TWorkbenchExt
  17.10.01 mo                              TN1067 Änderungen für SIAS
  28.12.01 tbh diverse                     TN1051 Aufrufe und Deklarationen von SetUseTips angepasst
  16.01.02 tbh TSetupCarrier.EvaluationEnd TN1171 Next entfernt
  12.07.02 tbh TSetupCarrier.EvaluationEnd TN1254 Abbruchmöglichkeit für Update anderer Layouts hinzugefügt
  10.10.02 wl                               TN1293.2 Ini Access - uses geändert
  05.11.02 wl  TSetupRack.SetRName         TN1330 Bugfix: Verschwundene Racks aus Stacker mit 20 Buchstaben
  12.12.02 wl                               TN1345 TWorkbenchExt => TWorkbench
  12.12.02 wl                               TN1345 Robot. statt Sam.
  27.12.02 wl                               TN1293.5 uses und WinlissyIniAccess geändert
  07.02.03 wl  TSetupCarrier.EvaluationEnd       TN1334.3 Log für jedes geänderte Layout
  07.02.03 wl  TSetupWorkbench.SaveSetupLayout   TN1334.3 mit Logging (Änderungen werden nicht aufgeschlüsselt)
  07.02.03 wl  TSetupWorkbench.LayoutSaved       TN1334.3 ersetzt durch AskForSave
  07.02.03 wl  TSetupWorkbench.SaveBeforeClosing TN1334.3 ersetzt durch AskForSave
  12.03.03 wl                               TN1293.5 uses posTools
  14.04.03 tbh TSetupCarrier                TN1466 FSRacks jetzt bis 250 Racks
  30.05.03 wl  TSetupTip.MouseDown1         TN1493.2 Motor-Beschriftung automatisch beim Setzen von TipUsed
  30.05.03 wl  TSetupArm.SetUseTips         TN1493.2 Motor-Beschriftung automatisch beim Setzen von TipUsed
  02.06.03 wl                               TN1485.4 TAppSettings.IsSias ersetzt die ifdefs
  26.06.03 wl  TSetupRack.InsertIDClick     TN1501.0 Bei Frage nach RackID wird Rackname genannt
  02.09.03 wl                               TN1559   Anpassung an Änderungen in TRack und TCarrier
  12.09.03 wl  RackNArray,FSRacks,...       TN1581   Erntfernung von redundantem Ballast
  12.09.03 wl  TSetupWorkbench.LoadSL       TN1581   entfernt - Laden des Layouts ab sofort über TWorkbench.Load
  12.09.03 wl  TSetupWorkbench.SaveAll      TN1581   LoadAgain ist nicht mehr nötig
  12.09.03 wl  alle Funktionen              TN1581.4  keine Sonderbehandlung mehr für Stacker (EditStck.pas entfernt)
  12.09.03 wl  TSetupRack.CreatePopup       TN1581.7/13 neue Menüpunkte: Rack drehen, Rack-Position ändern
  12.09.03 wl  TSetupRack.InsertIDClick     TN1315    eine Rack-ID kann nicht mehr doppelt vergeben werden!
  12.09.03 wl  TSetupRack.ChangeCarrPosClick TN1581.13 Ändern der Carrier-Position (noch nicht implementiert)
  12.09.03 wl  TSetupRack.TurnRackClick     TN1581.7  Rack dreht sich bei jedem Aufruf um 90°
  17.09.03 wl  TSetupWorkbench.GetNewRackName  TN1581.14 neues Rack: kein Anhang bei "WASH", "RWASH", "WASTE", "DRY"
  02.10.03 wl  TSetupRack.CreatePopup       TN1581    unfertige Menüpunkte entfernt!
  06.10.03 wl  TSetupCarrier.MouseDown1     TN1581.15 Bei STRG + Mausklick: Rack einfügen, mit Shift: keine Namensabfrage
  06.10.03 wl  TSetupWorkbench.MouseDown1   TN1581.15 Bei STRG + Mausklick: Carrier einfügen, mit Shift: keine Namensabfrage
  07.10.03 wl  TSetupRack.SetTubeData       TN1581    Fehler beseitigt: Gelöschte Racks wurden wieder sichtbar
  17.10.03 wl  StackerGridMouseDown         TN1581.15 Rack einfügen mit STRG + Mausklick jetzt auch im Stacker-Fenster möglich
  12.11.03 wl  TSetupRack.ChangeCarrPosClick TN1581.13 Startet den Vorgang "Ändern der Carrier-Position"
  12.11.03 wl  TSetupCarrier.SlotChosen     TN1581.13 Ändert die Position des vorher gewählten Racks
  14.11.03 wl  TSetupRack.TurnRackClick     TN1664    wieder aktiviert
  17.02.04 pk  TSetupRack.ChangeVisible     TN1749    Dont set visible to true if rack is deleted
  17.02.04 pk  CreatePopup                  TN1750.0  New: Choose Rack Level option
  25.02.04 pk  TSetupCarrier.EvaluationEnd  TN1763    If The modalresult flag is mrCancel on the EvaluateForm then Exit
  25.02.04 pk  TDragHandle.WMLButtonUp      TN1749    Call Change visible for all racks
  03.03.04 wl  TDragHandle.WMLButtonUp      TN1783    line removed: if top < 0 then top := 0
  03.03.04 pk  TSetupCarrier.ChangeVisible  TN1749    Dont set visible to true if carrier is deleted
  18.03.04 wl  TSetupArm-Methoden,TSetupTip TN1765.1 --> LObjAll
  18.03.04 wl  TSetupCarrier.EvaluationStartTN1825   zu Beginn wird GlobalErr zurückgesetzt (gCommManager.LayouterResetGlobalErr)
  18.03.04 wl  TSetupRack.MouseDown1        TN1825   zu Beginn wird GlobalErr zurückgesetzt (gCommManager.LayouterResetGlobalErr)
  19.03.04 wl  TSetupWorkbench.CreateArm    TN1788   Create statt CreateArm (Anpassung an TArm)
  20.04.04 wl  TSetupArm                    TN1788   an Änderungen von TArmPanel und TTipPanel angepasst
  10.05.04 wl  TSetupRack.DrawRackPosition  TN1788   entspricht SetTubeData
  27.10.04 wl  TSetupRack.DrawRackPosition  TN2071   Menu "Turn Rack" wird nur angezeigt, wenn man das Rack auch drehen kann!
  06.01.05 wl  TSetupWorkbench.AddNewRack   TN2284   das dynamische Array FRacks wird jetzt immer an die Zahl der Racks angepasst
  15.06.05 pk  TSetupRack.MouseDown1        TN2464   use GetCurrentArm instead of using gPipArm
  15.06.05 pk  TSetupWorkbench.InsertNewCarrier TN2466 Increase Dynamic array size
  21.06.05 pk  TSetupWorkbench..CreateArm   TN2464.3 Do not call Arm.SetUseTips
  21.06.05 pk  TSetupArm.ShowUseTips        TN2464.3 replaced by UsedTipsChange
  26.09.05 pk  TSetupRack.ChangeCarrPosClick TN2618  changed german message to resource
  14.10.05 wl  TSetupWorkbench.GetNewRackName   TN2672   benutzt TRackDefinitions für Rack-Namenskonventionen
  27.04.06 pk  InsertNewCarrier             TN2958   Robot.PArm_GetZMax replaced by wb.MaxZ_Steps
  30.08.06 pk  TSetupWorkbench.SetMouseEvents TN3256 Bug fix: can now place carrier on workbench again
  11.12.06 wl  TSetupRack.MouseDown1          TN3402  Bei Evaluate wird jetzt die richtige Position ermittelt
  31.01.07 wl                                 TN3532    benutzt Main.CARRIER statt DBRack.CARRIER
  20.02.07 wl  TSetupWorkbench.GetNewCarrierName  TN3586  statt _001 wird für neue Carrier _1 angeboten
  20.02.07 wl  TSetupWorkbench.GetNewRackName     TN3586  statt _001 wird für neue Racks _1 angeboten
  20.02.07 wl  TNewRackPage                       TN1921  Eingabedialog für Rackname UND Rack-ID
  20.02.07 wl  TSetupWorkbench.RequestRackName    TN1921  zeigt TNewRackPage für Rackname UND Rack-ID
  01.03.07 wl  TSetupCarrier.EvaluationStart      TN2611   wenn kein Arm existiert, wird Evaluate-Dialog nicht gezeigt
  01.03.07 wl  TSetupWorkbench.SaveAll            TN2611   Abfrage, ob FArm existiert
  04.05.07 wl  TSetupWorkbench.SaveAll            TN3669    benutzt TAppSettings.ItemConfirmEdit
  30.08.07 pk                                     TN3840.1 uses TLayoutDataAdaptor instead of dmRack
  12.11.07 pk                                     TN3924   Steps changed to mm
  07.01.08 pk  TSetupWorkbench.AddNewRack         TN3864   Set OnFindCarrier event
  25.01.08 pk  TSetupCarrier.NewPosition          TN3924   divide by zoom instead of multiply by zoom
  25.01.08 pk  TSetupWorkbench.AskForSave         TN3840.1 oLayoutEmpty now set correctly
  20.06.08 pk                                     TN4139    Layout Elements no longer based on TPanel
  27.06.08 pk  SaveAll                            TN4139   calls DeleteAllLayoutRelated
  02.07.08 pk  fTurnRackItem                      TN4139   reimplemented
  03.07.08 wl                                         TN4157
  09.07.08 pk                                     TN4139   various changes
  16.07.08 wl                                     TN4164   using TMain directly instead of TMain.FraEvaluate1
  22.07.08 pk                                     TN4139   Bug fixes to avoid editing linked layouts
  22.07.08 pk  MovePos                            TN4139   New
  30.07.08 pk  AskForSave                         TN4139   call Load instead of reload
  17.09.08 wl  TSetupTipsetDevice                 TN4224   überarbeitet
  20.09.08 wl  GetCurrentArm                      TN4224   result muss nicht assigned sein
  23.09.08 wl                                     TN4237   nicht mehr von TTestAndSetupLayout abgeleitet
  13.03.09 pk                                     TN4463  Changes needed for reimplementing carrier drag move
  13.03.09 pk                                     TN4466  Changes need for implementing rack carrier slot change via drag and drop
  10.08.09 wl                                     TN4702   Strings werden jetzt direkt geladen
  12.08.09 pk  CalcNewWorkspacePos                TN4714   New
  21.08.09 wl  TSetupLayout.EditZHeights          TN4702   Instanz ist lokal definiert
  26.08.09 pk                                     TN4753   most instances of Main.TV removed
  04.11.09 pk                                	 TN4843  Replace TObjectList, TStringList, TStrings with Generics Lists and TStringArray
  13.11.09 pk  TNewRackPage.GetHeaderText         TN4843  result was not set correctly
  18.02.10 ts  EvaluationEnd                      TN4981  nach Evaluierung Änderungsabfrage nur einmal pro Layout (Run-Layouts werden nicht beachtet)
  13.04.10 wl                                     TN5044   uses geändert
  umbenannt von LObjEdit in EditingLayoutElements:
  23.04.10 wl  DragOver/DragDrop                  TN5070   alles Layouter-spezifische wurde gekapselt
  23.04.10 wl  TSetupCarrier.fEvaluation          TN5070   alles Layouter-spezifische wurde gekapselt
  30.04.10 wl                                     TN5070   Racks können jetzt direkt auf Workspace plaziert werden
  03.05.10 wl  TNewRackPage                       TN5070   Andere Beschriftung, wenn Rack direkt platziert wird
  06.05.10 ts  TNewLayoutPage                     TN5083   neu, Wizard für Workspace und Default Tiptype bei Erstellen von neuem Layout
  07.05.10 wl                                     TN5052    Display-Optionen aus TAppSettings statt IniAccess
  20.05.10 wl                                     TN5117   uses ControlUtils
  28.05.10 wl                                     TN5116   benutzt DesignModuleSettingFinder
  07.06.10 wl  TCarrierEvaluation                 TN5116   hierher verschoben
  07.06.10 wl                                     TN5116   Zugriff auf TLayoutEditingFunctionProvider statt auf TLayoutManager
  17.06.10 wl                                     TN5150   TSetupLayout ist von TLayoutExt abgeleitet
  03.09.10 wl  TSetupCarrierSlot.DoDragDrop       TN5253   Beim Verschieben von Racks wird jetzt geprüft, ob der Slot schon besetzt ist
  09.12.10 wl  TSetupStackerLevelSelectorGraphics.StackerGridKeyDown    TN5257   ENTF-Taste: Rack löschen
  09.12.10 wl  TSetupStackerLevelSelectorGraphics.StackerGridKeyDown    TN5257   STRG + RETURN: Rack in Stacker einfügen
  09.12.10 wl  TSetupStackerLevelSelectorGraphics.StackerGridMouseDown  TN5257   STRG + Left Mouseklick: Rack in Stacker einfügen
  09.12.10 wl  TSetupCarrierSlot.DoMouseDown      TN5257   STRG + Left Mouseklick: Rack auf Carrier platzieren
  09.12.10 wl  TSetupWorkspace.DoMouseDown        TN5257   STRG + Left Mouseklick: Carrier auf Workbech platzieren
  09.12.10 wl                                     TN5257.1 CTRL durch ALT ersetzt
  08.02.11 wl                                     TN5475   Zugriffe auf TLayoutDataAdaptor.Instance geändert
  10.02.11 wl                                     TN5475   Zugriffe auf ..DataAdaptor.Instance geändert
  03.11.11 wl  DoCreateRackWell                   TN5725   jetzt mit WellNr
  01.03.12 wl                                     TN5822   uses geändert
  13.03.13 wl                                     TN5960   uses geändert
  27.03.13 wl                                     TN6045   uses geändert
  -------------------------------------------------------------------------------------------------- }

unit EditingLayoutElements;


interface


uses
    Forms,
    Controls,
    Generics.Collections,
    Classes,
    SysUtils,
    Graphics,
    StdCtrls,
    Tipset,
    Rack,
    RackWell,
    Carrier,
    CarrierSlot,
    Workspace,
    Layout,
    CommonTypes,
    AppTypes,
    IntfArmDevice,
    Wizard,
    LayoutElementCallbackTypes,
    MatrixMath,
    LayoutElement,
    LayoutElementGraphics,
    LayoutElementGraphicsInfo,
    PopupMenuInfo,
    GeneralTypes,
    LayoutExt,
    StackerLevelSelectorGraphics;

type
    TCarrierEvaluation = class
    public
        procedure EvaluationStart(aCarrier: TCarrier; aRack: TRack); virtual; abstract;
        function EvaluationEndXYChanged(out oNewX, oNewY, oOldX, oOldY: TPosMM): boolean; virtual; abstract;
        procedure EvaluationSetTubeRef(aRack: TRack; aPosition: integer); virtual; abstract;
        procedure EvaluationReferenceChanged(); virtual; abstract;
    end;

    TNewRackPage = class(TWizardPage)
    private
        FActiveControl: TWinControl;
        FRackNameEdit: TEdit;
        FRackIDEdit: TEdit;
        function GetRackName(): string;
        function GetRackID(): string;
    public
        // constructor
        constructor Create(aOwner: TComponent; aRackTypeName, aNewRackName, aCarrierAndSlotName: string);
            reintroduce;
        // methods derived from TWizardPage
        function GetHeaderTitle: string; override;
        function GetHeaderText(): TStringArray; override;
        function CheckBeforeNext: boolean; override;
        function GetActiveControl: TWinControl; override;
        // properties
        property RackName: string read GetRackName;
        property RackID: string read GetRackID;
    end;

    TNewLayoutPage = class(TWizardPage)
    private
        FActiveControl: TWinControl;
        FLayoutNameEdit: TEdit;
        FWorkspaceNameEdit: TEdit;
        FTipTypeNameEdit: TEdit;
        cbWorkspace: TComboBox;
        cbTipType: TComboBox;
        procedure cbWorkspaceOnChange(Sender: TObject);
        procedure cbTipTypeOnChange(Sender: TObject);
        function GetLayoutName(): string;
        function GetWorkspaceName(): string;
        function GetTipTypeName(): string;
    public
        // constructor
        constructor Create(aOwner: TComponent; aLayoutName: string); reintroduce;
        // methods derived from TWizardPage
        function GetHeaderTitle: string; override;
        function GetHeaderText(): TStringArray; override;
        function CheckBeforeNext: boolean; override;
        function GetActiveControl: TWinControl; override;
        procedure SetUp(aPreviousPage: TWizardPage); override;
        // properties
        property LayoutName: string read GetLayoutName;
        property WorkspaceName: string read GetWorkspaceName;
        property TipType: string read GetTipTypeName;
    end;

    TSetupRackWell = class(TRackWell)
    private
        procedure DoMouseDown(aSender: TObject; aButton: TGraphicsMouseButton; aShift: TGraphicsShiftState;
            aX, aY: double);
    protected
        procedure DoInitGraphics; override;
    end;

    TSetupRack = class(TRack)
    private
        FDeleted: boolean;
        fTurnRackItem: TPopupMenuInfoItem;
        fGeneralPopupMenuItems: TObjectList<TPopupMenuInfoItem>;
        fMenuItemMoveCarrier: TPopupMenuInfoItem;
        fMenuItemEvalCarrier: TPopupMenuInfoItem;
        fMenuItemCarrierChooseLevel: TPopupMenuInfoItem;
        // --------------------------------------------------------- interne Methoden
        procedure CreatePopup;
        procedure DoPopup(aSender: TObject; aX, aY: double; var vPopupMenuInfos: TPopupMenuInfoList);
        procedure DoIsDragable(aSender: TObject; var vIsDragable: boolean);
        procedure RackMenuClick(Sender: TObject);
        procedure ChangeNameClick(Sender: TObject);
        procedure InsertIDClick(Sender: TObject);
        procedure TurnRackClick(Sender: TObject);
        procedure DeleteRack;
        function IsOnInvisibleCarrier(): boolean;
        function CarrierHasLevels: boolean;
        procedure CustomnizePopupMenu();
    protected
        procedure DoInitGraphics(); override;
        procedure SetVisible(aVisible: boolean); override;
        function DoCreateRackWell(aWellNr: integer): TRackWell; override;
        procedure DoAfterCreateRackWell(aRackWell: TRackWell); override;
    public
        property Deleted: boolean read FDeleted;
        procedure Save;
        procedure EvaluationStart;
        procedure EvaluationEnd;
        procedure DrawRackPosition; override;
        // -------------------------------------------------------------- CONSTRUCTOR
        constructor Create();
        destructor Destroy; override;

    end;

    TSetupCarrierSlot = class(TCarrierSlot)
    private
        procedure DoDragDrop(aSender, aSource: TObject; aX, aY: double);
        procedure DoDragOver(aSender, aSource: TObject; aX, aY: double; aState: TGraphicsDragState;
            var vAccept: boolean);
        procedure DoMouseDown(aSender: TObject; aButton: TGraphicsMouseButton; aShift: TGraphicsShiftState;
            aX, aY: double);
        procedure DoDblClick(aSender: TObject);
    protected
        procedure DoInitGraphics(); override;
    public
        function CheckIfEmpty(): boolean;
    end;

    TSetupStackerLevelSelectorGraphics = class(TStackerLevelSelectorGraphics)
    protected
        procedure StackerGridMouseDown(aSender: TObject; aButton: TMouseButton; aShift: TShiftState;
            aX, aY: Integer); override;
        procedure StackerGridKeyDown(aSender: TObject; var aKey: Word; aShift: TShiftState); override;
    end;

    TSetupCarrier = class(TCarrier)
    private
        FSaveColor: TColor;
        FDrag: boolean; // Enable Dragging
        { TODO -opk -cWB : }// FDragHandle: TDragHandle;
        FDeleted: boolean;
        fGeneralPopupMenuItems: TObjectList<TPopupMenuInfoItem>;
        fShowStackerOverviewMenuItem: TPopupMenuInfoItem;
        fEvaluation: TCarrierEvaluation;
        // ----------------------------------------- DragDrop
        // procedure WBDragOver(Sender, Source: TObject; X, Y: Integer;State: TDragState; var Accept: Boolean);
        // procedure WBDragDrop(Sender, Source: TObject; X, Y: Integer);
        // procedure DoDragDrop(aSender, aSource: TObject; aX, aY : double);
        // procedure DoDragOver(aSender, aSource: TObject; aX, aY : double; aState: TGraphicsDragState; var vAccept: boolean);

        // --------------------------------------------------------- interne Methoden
        procedure CarrierMenuClick(Sender: TObject);
        procedure DeleteClick(Sender: TObject);
        procedure ChangeNameClick(Sender: TObject);
        procedure MovePos();
        procedure DoDblClick(aSender: TObject);
        procedure EvaluationStart(aRack: TRack);
        procedure ShowStackerOverviewDialog(Sender: TObject); overload;
        procedure RedrawSlots;
        procedure DoPopup(aSender: TObject; aX, aY: double; var vPopupMenuInfos: TPopupMenuInfoList);
        procedure DoPosChanged(aSender: TObject; const aX, aY, aZ: double);

        procedure ChangeRackEvaluationState(aIsEvaluationStart: boolean);
        procedure SetRacksVisible(const aVisible: boolean);
        function CalcNewWorkspacePos(const aNewWorldPos: TPoint4d): TPoint4d;
    protected
        procedure DoAfterCreateCarrierSlot(aCarrierSlot: TCarrierSlot); override;
        procedure DoInitGraphics(); override;
        function DoCreateCarrierSlot(): TCarrierSlot; override;
        function DoCreateStackerLevelSelectorGraphics(): TStackerLevelSelectorGraphics; override;
        procedure SetVisible(aVisible: boolean); override;
    public
        // --------------------------------------------------------------- Properties
        property Dragg: boolean read FDrag write FDrag;
        property SaveColor: TColor read FSaveColor;
        property Evaluation: TCarrierEvaluation read fEvaluation;
        // ------------------------------------------------------ allgemeine Methoden
        procedure EvaluationEnd;
        procedure ChooseEvalPos(aRack: TRack; aPos: integer);
        procedure Save;
        procedure NewPosition(X, Y: double);
        procedure CreatePopUp;
        procedure SetWB_X(aValue: TPosMM);
        procedure SetWB_Y(aValue: TPosMM);
        procedure SetWB_Z(aValue: TPosMM);
        procedure SetWorldX(const aValue: TPosMM);
        procedure SetWorldY(const aValue: TPosMM);
        constructor Create();
        procedure Delete();
        destructor Destroy; override;
    end;

    TSetupTipsetDevice = class(TTipsetDevice)
    public
        procedure SaveTips(const aLayoutName: string);
        constructor Create();
    end;

    TSetupWorkspace = class(TWorkspace)
    private
        fDeleted: boolean;
        fGeneralPopupMenuItems: TObjectList<TPopupMenuInfoItem>;
        procedure CreatePopup;
        procedure EditViewRelation(aSender: TObject);
        procedure DeleteClick(aSender: TObject);
    protected
        procedure DoInitGraphics(); override;
        procedure DoDragOver(aSender, aSource: TObject; aX, aY: double; aState: TGraphicsDragState;
            var vAccept: boolean);
        procedure DoDragDrop(aSender, aSource: TObject; aX, aY: double);
        procedure DoPopup(aSender: TObject; aX, aY: double; var vPopupMenuInfos: TPopupMenuInfoList);
        procedure DoMouseDown(aSender: TObject; aButton: TGraphicsMouseButton; aShift: TGraphicsShiftState;
            aX, aY: double);
        procedure ChangeNameClick(Sender: TObject);
    public
        procedure Save();
        procedure Delete();
        constructor Create;
        destructor Destroy; override;
    end;

    TSetupLayout = class(TLayoutExt)
    private
        // procedure SetMouseEvents();
        function RequestRackName(aRackTypeName, aNewRackName, aCarrierAndSlotName: string;
            out oRackName, oRackID: string): boolean;
        function GetWorkspaceTypeIDByTypeName(const aTypeName: string): integer;
        procedure AddNewLinkedLayout(const aSourceName: string);
        function LayoutLinksIndexOfName(const aName: string): integer;
    protected
        FReason: string;
        procedure DoInitGraphics(); override;
        procedure DoDragOver(aSender, aSource: TObject; aX, aY: double; aState: TGraphicsDragState;
            var vAccept: boolean);
        procedure DoDragDrop(aSender, aSource: TObject; aX, aY: double);
        // //----------------------------------------- DragDrop
        // procedure WBDragOver(Sender, Source: TObject; X, Y: Integer;State: TDragState; var Accept: Boolean);
        // procedure WBDragDrop(Sender, Source: TObject; X, Y: Integer);
        // procedure MouseDown1(Sender:TObject;Button:TMouseButton;Shift:TShiftState; X,Y:Integer);
        // //------------------------------------------------------------------------- interne Methoden
        function AddNewCarrier(aWorkspaceID: integer; aX, aY: double; const aSourceName: string;
            aDontAskName: boolean): TCarrier;
        function GetNewCarrierName(const aSourceName: string): string;
        function GetNewWorkspaceName(const aSourceName: string): string;
        // //--------------------------------------------------- überschriebene Funktionen
        function DoCreateTipsetDevice: TTipsetDevice; override;
        function DoCreateWorkspace(): TWorkspace; override;
        function DoCreateRack(): TRack; override;
        function DoCreateCarrier(): TCarrier; override;
        function DoCreateLayoutLink: TLayoutLink; override;

        function GetNotSaved: boolean;
        procedure SetNotSaved(const Value: boolean);

    public
        constructor Create(const aLayoutName, aRunName: string);
        destructor Destroy; override;
        procedure RefreshScene;
        procedure UpdateEvalCarr;
        function GetNewRackName(const aSourceName: string): string;
        function SaveAll: boolean;
        procedure EditZHeights;
        procedure MoveLayout;
        procedure DeleteRack(aRack: TRack);
        procedure DeleteCarrier(aCarrier: TCarrier);
        procedure DeleteWorkspace(aWorkspace: TWorkspace);
        procedure AddNewWorkspace(const aSourceName: string; aDontAskName: boolean);
        function AskForSave(aLoadAgain: boolean; out oLayoutEmpty: boolean): boolean;
        procedure AddNewRack(const aRackTypeName, aCarrierName: string; aSlotNr: integer;
            aDontAskName: boolean);
        function AddNewLayout(aLayoutName: string; out oWorkspace: string; out oTipType: string): string;
        procedure SetDefaultTipset(aTipType: string);
        // //
        property NotSaved: boolean read GetNotSaved write SetNotSaved;
        property Reason: string read FReason;
    end;

    TSetupLayoutLink = class(TLayoutLink)
    protected
        fDeleted: boolean;
    public
        constructor Create;
        procedure Save;
    end;

    EControlInvalid = class(Exception);


implementation


uses
    Windows,
    ComCtrls,
    Menus,
    Dialogs,
    SamGlobe,
    AppSettings,
    RackTypes,
    ControlUtils,
    IntfPipDevice,
    EditArm,
    EditZPos,
    LayoutEditingFunctionProvider,
    LayoutDataAdaptor,
    LayoutDataAdaptorExt,
    TipsetDataAdaptor,
    WorkspaceDataAdaptor,
    LayoutWorkspaceDataAdaptor,
    RackDataAdaptor,
    CarrierDataAdaptor,
    LayoutDataCache,
    GUIManager,
    EditXY,
    DesignModuleSettingFinder,
    LayoutWorkspaceEditor,
    CoordSystemMath,
    DialogUtils,
    TipTypeDataAdaptor;

{ TNewRackPage }

constructor TNewRackPage.Create(aOwner: TComponent; aRackTypeName, aNewRackName, aCarrierAndSlotName: string);
begin
    inherited Create(aOwner);

    AddLabel(TLanguageString.Read('Rack type:', 'Racktyp:'), INT_WIZARD_LEFT_1);
    with self.AddStaticText(INT_WIZARD_LEFT_EDIT, INT_WIZARD_WIDTH_EDIT) do
        Caption := aRackTypeName;
    AddDistance(wzdMedium);

    if (aCarrierAndSlotName <> '') then
    begin
        AddLabel(TLanguageString.Read('Carrier position:', 'Zielposition:'), INT_WIZARD_LEFT_1);
        with self.AddStaticText(INT_WIZARD_LEFT_EDIT, INT_WIZARD_WIDTH_EDIT) do
            Caption := aCarrierAndSlotName;
    end
    else
    begin
        AddLabel(TLanguageString.Read('Rack will be placed onto layout.',
            'Das Rack wird im Layout platziert.'), INT_WIZARD_LEFT_1);
        if not TAppSettings.IsTraySy then
        begin
            AddDistance(wzdSmall);
            AddLabel(TLanguageString.Read('Rack can not be gripped and moved by a gripper device.',
                'Das Rack kann nicht von einem Greifer gegriffen und bewegt werden.'), INT_WIZARD_LEFT_1);
        end;
    end;

    AddDistance(wzdXLarge);

    AddLabel(TLanguageString.Read('Rack name:', 'Rackname:'), INT_WIZARD_LEFT_1);
    FRackNameEdit := self.AddEdit(INT_WIZARD_LEFT_EDIT, INT_WIZARD_WIDTH_EDIT);
    FRackNameEdit.Text := aNewRackName;
    FRackNameEdit.MaxLength := TLayoutDataAdaptor.MaxRackNameLen;
    AddDistance(wzdMedium);

    AddLabel(TLanguageString.Read('Rack ID:', 'Rack-ID:'), INT_WIZARD_LEFT_1);
    FRackIDEdit := self.AddEdit(INT_WIZARD_LEFT_EDIT, INT_WIZARD_WIDTH_EDIT);
    if not TRackDefinitions.RackIsGeneralSpecialRack(aNewRackName) then
        FRackIDEdit.Text := aNewRackName;
    FRackIDEdit.MaxLength := TLayoutDataAdaptor.MaxRackIDLen;
    AddDistance(wzdXLarge);

    FActiveControl := FRackNameEdit
end;

function TNewRackPage.GetHeaderTitle: string;
begin
    result := TLanguageString.Read('New Rack', 'Neues Rack');
end;

function TNewRackPage.GetHeaderText(): TStringArray;
begin
    result := self.GetSingleTextAsHeader(TLanguageString.Read('A new rack will be placed',
        'Ein neues Rack soll platziert werden'));
end;

function TNewRackPage.CheckBeforeNext: boolean;
begin
    result := false;

    // check if layout name exists
    if (FRackNameEdit.Text = '') then
    begin
        TDialogUtils.MessageBox(TLanguageString.Read('The rack name can not be empty.',
            'Der Rackname kann nicht leer sein.'), TLanguageString.Read('Rack name', 'Rackname'), 64);
        FActiveControl := FRackNameEdit;
        EXIT;
    end;

    if Assigned(TLayoutEditingFunctionProvider.Instance.CurrentLayout.FindRackByName(FRackNameEdit.Text)) then
    begin
        TDialogUtils.MessageBox(TLanguageString.Read('The rack name already exists in the layout.',
            'Der Rackname ist bereits vergeben.'), TLanguageString.Read('Rack name', 'Rackname'), 64);
        FActiveControl := FRackNameEdit;
        EXIT;
    end;

    if (FRackIDEdit.Text <> '') then
    begin
        if Assigned(TLayoutEditingFunctionProvider.Instance.CurrentLayout.FindRackByRackID
            (FRackIDEdit.Text)) then
        begin
            TDialogUtils.MessageBox(TLanguageString.Read('The rack ID already exists in the layout.',
                'Die Rack-ID ist bereits vergeben.'), TLanguageString.Read('Rack name', 'Rackname'), 64);
            FActiveControl := FRackIDEdit;
            EXIT;
        end;
    end;

    result := true;
end;

function TNewRackPage.GetActiveControl: TWinControl;
begin
    result := FActiveControl;
end;

function TNewRackPage.GetRackName(): string;
begin
    result := FRackNameEdit.Text;
end;

function TNewRackPage.GetRackID(): string;
begin
    result := FRackIDEdit.Text;
end;

{ TSetupRackWell }

procedure TSetupRackWell.DoInitGraphics;
begin
    inherited;
    self.Graphics.Callbacks.MouseDownCallback := DoMouseDown;
end;

procedure TSetupRackWell.DoMouseDown(aSender: TObject; aButton: TGraphicsMouseButton;
    aShift: TGraphicsShiftState; aX, aY: double);
var
    xRack: TSetupRack;
    xCarrier: TSetupCarrier;
begin
    xRack := self.Rack as TSetupRack;
    if (aButton = gmbRight) then
    begin
        xRack.CustomnizePopupMenu;
    end;

    xCarrier := TLayout.GetCarrierOfRack(xRack) as TSetupCarrier;
    if (aButton = gmbLeft) then
    begin
        if not Assigned(xCarrier.Evaluation) then
            EXIT;
        xCarrier.ChooseEvalPos(xRack, self.WellNr);
    end;
end;

{ TSetupRack }

constructor TSetupRack.Create();
begin
    inherited Create();
    fGeneralPopupMenuItems := TObjectList<TPopupMenuInfoItem>.Create(false);
end;

destructor TSetupRack.Destroy();
begin
    fGeneralPopupMenuItems.Free;
    inherited;
end;

procedure TSetupRack.DoInitGraphics;
begin
    inherited;
    CreatePopup();
    self.Graphics.Callbacks.PopupCallback := self.DoPopup;
    self.Graphics.Callbacks.IsDragableCallback := self.DoIsDragable;
end;

procedure TSetupRack.CreatePopup;
var
    x: integer;
begin
    self.Graphics.PopupMenuInfos.AddItem(TPopupMenuInfoItem.Create('-', False, True, nil, 'N0'));
    self.Graphics.PopupMenuInfos.AddItem(TPopupMenuInfoItem.Create(TLanguageString.Read('Edit Rack Name',
        'Rackname bearbeiten'), False { checked } , true, ChangeNameClick { OnClick } , 'ChangeName'));
    self.Graphics.PopupMenuInfos.AddItem(TPopupMenuInfoItem.Create(TLanguageString.Read('Edit Rack-ID',
        'Rack-ID bearbeiten'), False { checked } , true, InsertIDClick { OnClick } , 'InsertID'));

    fTurnRackItem := TPopupMenuInfoItem.Create('', False, True { Enabled } , TurnRackClick { OnClick } ,
        'mnuTurnRackDir');
    self.Graphics.PopupMenuInfos.AddItem(fTurnRackItem);
    self.Graphics.PopupMenuInfos.AddItem(TPopupMenuInfoItem.Create('-', False, true, nil, 'N1'));
    self.Graphics.PopupMenuInfos.AddItem(TPopupMenuInfoItem.Create(TLanguageString.Read('Delete Rack',
        'Rack löschen'), False, true, RackMenuClick, 'mnuDelete'));
    self.Graphics.PopupMenuInfos.AddItem(TPopupMenuInfoItem.Create('-', False, true, nil, 'N4'));

    fMenuItemEvalCarrier := TPopupMenuInfoItem.Create(TLanguageString.Read('Evaluate Carrier Position',
        'Carrier Position evaluieren'), False, true, RackMenuClick, 'mnuEvaluate');
    self.Graphics.PopupMenuInfos.AddItem(fMenuItemEvalCarrier);
    fMenuItemMoveCarrier := TPopupMenuInfoItem.Create(TLanguageString.Read('Move Carrier', 'Carrier bewegen'),
        False, true, RackMenuClick, 'mnuMoveCarrier');
    self.Graphics.PopupMenuInfos.AddItem(fMenuItemMoveCarrier);

    // Add items which can also be used when Layoutelement is linked
    fMenuItemCarrierChooseLevel := TPopupMenuInfoItem.Create(TLanguageString.Read('Choose Stacker Level',
        'Stackerebene wählen'), False, True, RackMenuClick, 'mnuShowStackerLevelDialog');
    fGeneralPopupMenuItems.Add(fMenuItemCarrierChooseLevel);

    for x := 0 to fGeneralPopupMenuItems.Count - 1 do
        self.Graphics.PopupMenuInfos.AddItem(fGeneralPopupMenuItems[x]);
end;

procedure TSetupRack.CustomnizePopupMenu();
var
    x: integer;
begin
    for x := 0 to self.Graphics.PopupMenuInfos.Count - 1 do
    begin
        self.Graphics.PopupMenuInfos[x].Enabled := (not self.IsLinked) or
            (fGeneralPopupMenuItems.IndexOf(self.Graphics.PopupMenuInfos[x]) >= 0);
    end;
    if (self.IsOnInvisibleCarrier) then
    begin
        fMenuItemEvalCarrier.Caption := TLanguageString.
            Read('Evaluate Rack Position', 'Rack-Position evaluieren');
        fMenuItemMoveCarrier.Caption := TLanguageString.Read('Move Rack', 'Rack bewegen');
    end
    else
    begin
        fMenuItemEvalCarrier.Caption := TLanguageString.
            Read('Evaluate Carrier Position', 'Carrier-Position evaluieren');
        fMenuItemMoveCarrier.Caption := TLanguageString.Read('Move Carrier', 'Carrier bewegen');
    end;

    fMenuItemCarrierChooseLevel.Visible := self.CarrierHasLevels;
end;

procedure TSetupRack.DoPopup(aSender: TObject; aX, aY: double; var vPopupMenuInfos: TPopupMenuInfoList);
begin
    CustomnizePopupMenu();
end;

procedure TSetupRack.RackMenuClick(Sender: TObject);
var
    MName: string;
    xCarrier: TCarrier;
begin
    MName := (Sender as TMenuItem).Name;
    xCarrier := TLayout.GetCarrierOfRack(self);
    if (MName = 'mnuEvaluate') then
        (xCarrier as TSetupCarrier).EvaluationStart(self);
    if (MName = 'mnuMoveCarrier') then
        (xCarrier as TSetupCarrier).MovePos();
    if (MName = 'mnuShowStackerLevelDialog') then
        (xCarrier as TSetupCarrier).ShowStackerOverviewDialog(xCarrier);
    if (MName = 'mnuDelete') then
        DeleteRack;
end;

procedure TSetupRack.ChangeNameClick(Sender: TObject);
var
    xNewName: string;
begin
    xNewName := FName;
    if not gGUIManager.InputQuery(TLanguageString.Read('Change Rack Name', 'Racknamen ändern'),
        TLanguageString.Read('Rack Name', 'Rackname'), xNewName) then
        exit;

    if (xNewName = '') then
        exit;

    xNewName := Copy(xNewName, 1, TLayoutDataAdaptor.MaxRackNameLen);
    if (xNewName <> FName) then
    begin
        if Assigned(TLayoutEditingFunctionProvider.Instance.CurrentLayout.FindRackByName(xNewName)) then
        begin
            TDialogUtils.MessageBox(TLanguageString.Read('Rackname already exists!',
                'Rackname existiert bereits!'), '', MB_OK);
            EXIT;
        end;
        FName := xNewName;
        TLayoutEditingFunctionProvider.Instance.CurrentSetupLayout.NotSaved := true;
    end;
end;

procedure TSetupRack.InsertIDClick(Sender: TObject);
var
    xRackID: string;
begin
    xRackID := FRackID;
    if not gGUIManager.InputQuery(TLanguageString.Read('Insert Rack ID for rack {0}:',
        'Geben Sie die Rack-ID for Rack {0} ein:', [FName]), 'Rack ID', xRackID) then
        exit;
    xRackID := Copy(xRackID, 1, TLayoutDataAdaptor.MaxRackIDLen);

    if (xRackID <> FRackID) then
    begin
        if (XRackID <> '') then
        begin

            if Assigned(TLayoutEditingFunctionProvider.Instance.CurrentLayout.FindRackByRackID(xRackID)) then
            begin;
                showMessage(TLanguageString.Read('Rack ID already exists!', 'Rack-ID existiert bereits!'));
                exit;
            end;
        end;
        FRackId := xRackID;
        { TODO -opk -cWB : }
        // CreateHint;
        TLayoutEditingFunctionProvider.Instance.CurrentSetupLayout.NotSaved := true;
    end;
end;

function TSetupRack.IsOnInvisibleCarrier: boolean;
var
    xCarrier: TCarrier;
begin
    result := false;
    xCarrier := TLayout.GetCarrierOfRack(self);
    if Assigned(xCarrier) then
        result := (xCarrier.TypeName = TCarrierDataAdaptor.InvisibleCarrierName);
end;

function TSetupRack.CarrierHasLevels: boolean;
var
    xCarrier: TCarrier;
begin
    result := false;
    xCarrier := TLayout.GetCarrierOfRack(self);
    if Assigned(xCarrier) then
        result := (xCarrier.SlotGroupFloors > 1);
end;

procedure TSetupRack.TurnRackClick(Sender: TObject);
var
    xCarrier: TCarrier;
begin
    xCarrier := TLayout.GetCarrierOfRack(self);
    if (not Assigned(xCarrier)) then
        EXIT;

    case (xCarrier.SlotTurnType) of
        stt180Degree:
            self.RackRotation := TCarrier.Turn180degree(self.RackRotation);
        stt90Degree:
            self.RackRotation := TCarrier.Turn90degree(self.RackRotation);
        else
            EXIT;
    end;
    DrawRackPosition;
    TLayoutEditingFunctionProvider.Instance.CurrentSetupLayout.RefreshScene();
    TLayoutEditingFunctionProvider.Instance.CurrentSetupLayout.NotSaved := true;
end;

procedure TSetupRack.DeleteRack;
var
    xCarrier: TCarrier;
begin
    xCarrier := TLayout.GetCarrierOfRack(self);

    // delete rack
    TLayoutEditingFunctionProvider.Instance.CurrentSetupLayout.DeleteRack(self);
    FDeleted := true;

    // delete carrier (if invisible)
    if (xCarrier.TypeName = TCarrierDataAdaptor.InvisibleCarrierName) then
    begin
        TLayoutEditingFunctionProvider.Instance.CurrentSetupLayout.DeleteCarrier(xCarrier);
    end;
end;

procedure TSetupRack.Save;
var
    xCarrier: TCarrier;
    xSlotNr: integer;
    xTypeExists: boolean;
    xDA: TLayoutDataAdaptor;
begin
    if (FDeleted) then
        EXIT;
    if self.IsLinked then
        EXIT;

    xCarrier := TLayout.GetCarrierOfRack(self);
    if (xCarrier = nil) or (xCarrier.Name = '') then
    begin
        ShowMessage(Format('Rack %s - No Carrier set!!', [FName]));
        exit;
    end;

    xTypeExists := TRackDataAdaptor.InstNameExists(FTypeName);
    if not xTypeExists then
    begin
        raise Exception.Create(TLanguageString.Read('Rack type [{0}] not found in table',
            'Racktyp [{0}] wurde in der Tabelle nicht gefunden.', [FTypeName]));
    end;

    xDA := TLayoutDataAdaptor.Create;
    try
        if not xDA.RackExists('', TLayoutEditingFunctionProvider.Instance.CurrentLayout.LayoutName,
            fName) then
        begin
            xSlotNr := 0;
            if Assigned(fSlot) then
                xSlotNr := (fSlot as TCarrierSlot).SlotNr;
            xDA.AddRack(TLayoutEditingFunctionProvider.Instance.CurrentLayout.LayoutName, fTypeName, fName,
                fRackID, xCarrier.Name, xSlotNr, TRack.GetRotationDegree(self.RackRotation));
        end;
    finally
        FreeAndNil(xDA);
    end;
end;

procedure TSetupRack.DrawRackPosition;
var
    xCarrier: TCarrier;
begin
    if (FDeleted) then
        EXIT;

    inherited DrawRackPosition;

    xCarrier := TLayout.GetCarrierOfRack(self);

    if (not(fTurnRackItem is TPopupMenuInfoItem)) or (not(xCarrier is TCarrier)) then
        EXIT;

    case (xCarrier.SlotTurnType) of

        // Rack kann um 180° verdreht sein
        stt180Degree:
            begin
                fTurnRackItem.Visible := true;
                fTurnRackItem.Caption := TLanguageString.
                    Read('Turn Rack (180 degrees)', 'Rack drehen (180 Grad)');
            end;

        // Rack kann um 90° verdreht sein -> alle Richtungen sind möglich
        stt90Degree:
            begin
                fTurnRackItem.Visible := true;
                fTurnRackItem.Caption := TLanguageString.
                    Read('Turn Rack (90 degrees)', 'Rack drehen (90 Grad)');
            end;

        // keine Rotation möglich
        else
            fTurnRackItem.Visible := false;
    end;
end;

procedure TSetupRack.SetVisible(aVisible: boolean);
var
    xVisible: boolean;
begin
    xVisible := aVisible and (not FDeleted);
    inherited SetVisible(xVisible);
end;

procedure TSetupRack.EvaluationStart;
begin
    { TODO -opk -cWB : }
    {
      PopupMenu.AutoPopup:=false;
      Matrix.PopupMenu.AutoPopup:=false;
      Matrix.Cursor:=crHandPoint;
    }
end;

procedure TSetupRack.EvaluationEnd;

begin
    { TODO -opk -cWB : }
    // PopupMenu.AutoPopup := true;
    // Matrix.PopupMenu.AutoPopup := true;
    // Matrix.Cursor := crDefault;
end;

procedure TSetupRack.DoAfterCreateRackWell(aRackWell: TRackWell);
var
    x: integer;
begin
    for x := 0 to self.Graphics.PopupMenuInfos.Count - 1 do
        aRackWell.Graphics.PopupMenuInfos.AddItem(self.Graphics.PopupMenuInfos[x]);
end;

function TSetupRack.DoCreateRackWell(aWellNr: integer): TRackWell;
begin
    result := TSetupRackWell.Create(aWellNr);
end;

procedure TSetupRack.DoIsDragable(aSender: TObject; var vIsDragable: boolean);
begin
    vIsDragable := not self.IsLinked;
end;

{ TSetupCarrierSlot }

procedure TSetupCarrierSlot.DoInitGraphics;
begin
    inherited;
    self.Graphics.Callbacks.MouseDragOverCallback := self.DoDragOver;
    self.Graphics.Callbacks.MouseDragDropCallback := self.DoDragDrop;
    self.Graphics.Callbacks.MouseDownCallback := self.DoMouseDown;
    self.Graphics.Callbacks.MouseDblClickCallback := self.DoDblClick;
end;

procedure TSetupCarrierSlot.DoDragDrop(aSender, aSource: TObject; aX, aY: double);
var
    xNewSlotNr: integer;
    xCarrierName, xRackName: string;
    xTag: TObject;
    xRack: TRack;
begin
    xNewSlotNr := self.SlotNr;
    xCarrierName := (self.Carrier as TCarrier).Name;

    if TLayoutEditingFunctionProvider.Instance.DragSourceIsNode(aSource) then
    begin
        if TLayoutEditingFunctionProvider.Instance.DragSourceIsRackNode(aSource, xRackName) and
            TRackDataAdaptor.InstNameExists(xRackName) then
        begin
            // add rack to workbench
            TLayoutEditingFunctionProvider.Instance.CurrentSetupLayout.AddNewRack
                ((aSource as TTreeView).Selected.Text, xCarrierName, xNewSlotNr, false);
        end
        else
            Showmessage(TLanguageString.Read('Select a rack in selection window!',
                'Bitte wählen Sie ein Rack aus dem Auswahl-Fenster!'));
    end
    else if (aSource is TLayoutElementGraphics) then
    begin
        xTag := (aSource as TLayoutElementGraphics).Tag;
        if xTag is TRack then
        begin
            if not CheckIfEmpty() then
                EXIT;
            xRack := xTag as TRack;
            TLayoutEditingFunctionProvider.Instance.CurrentLayout.MoveRack(xRack, xCarrierName, xNewSlotNr,
                rotation_0);
            TLayoutEditingFunctionProvider.Instance.CurrentSetupLayout.NotSaved := true;
        end;
    end;
end;

procedure TSetupCarrierSlot.DoDragOver(aSender, aSource: TObject; aX, aY: double; aState: TGraphicsDragState;
    var vAccept: boolean);
var
    xRackMoveAllowed: boolean;
    xRackAddAllowed: boolean;
    xCarrier: TCarrier;
    xRackName: string;
begin
    vAccept := false;
    xCarrier := self.Carrier as TCarrier;
    if xCarrier.IsLinked then
        EXIT;
    xRackMoveAllowed := (aSource is TLayoutElementGraphics) and
        ((aSource as TLayoutElementGraphics).Tag is TRack);
    xRackAddAllowed := TLayoutEditingFunctionProvider.Instance.DragSourceIsNode(aSource) and
        TLayoutEditingFunctionProvider.Instance.DragSourceIsRackNode(aSource, xRackName);
    vAccept := xRackMoveAllowed or xRackAddAllowed;
end;

procedure TSetupCarrierSlot.DoMouseDown(aSender: TObject; aButton: TGraphicsMouseButton;
    aShift: TGraphicsShiftState; aX, aY: double);
var
    xCarrier: TCarrier;
    xRackName: string;
begin
    xCarrier := self.Carrier as TCarrier;
    if xCarrier.IsLinked then
        EXIT;

    // Racks schnell platzieren
    if (gssAlt in aShift) and (aButton = gmbLeft) then
    begin

        if TLayoutEditingFunctionProvider.Instance.MainNodeIsRackNode(xRackName) and
            TRackDataAdaptor.InstNameExists(xRackName) then
        begin
            // add rack to workbench
            TLayoutEditingFunctionProvider.Instance.CurrentSetupLayout.AddNewRack(xRackName, xCarrier.Name,
                self.SlotNr, (gssShift in aShift));

        end
        else
            Showmessage(TLanguageString.Read('Select a rack in selection window!',
                'Bitte wählen Sie ein Rack aus dem Auswahl-Fenster!'));

    end;
end;

function TSetupCarrierSlot.CheckIfEmpty: boolean;
var
    xCarrier: TCarrier;
begin
    xCarrier := self.Carrier as TCarrier;
    if xCarrier.IsLinked then
        EXIT(false);

    if Assigned(self.Rack) then
    begin
        gGUIManager.MessageBox(TLanguageString.Read('Slot {0} on Carrier {1} is occupied by rack [{2}]!',
            'Slot {0} on Carrier {1} ist mit Rack [{2}] besetzt!', [self.SlotNr, xCarrier.Name,
            (self.Rack as TRack).Name]), TLanguageString.Read('Insert new rack',
            'Neues Rack hinzufügen'), 16);
        EXIT(false);
    end;
    EXIT(true);
end;

procedure TSetupCarrierSlot.DoDblClick(aSender: TObject);
var
    xCarrier: TSetupCarrier;
begin
    xCarrier := self.Carrier as TSetupCarrier;
    xCarrier.MovePos;
end;

{ TSetupCarrier }

constructor TSetupCarrier.Create();
begin
    inherited Create();
    fGeneralPopupMenuItems := TObjectList<TPopupMenuInfoItem>.Create(false);
    fDeleted := false;

    FDrag := true;
end;

destructor TSetupCarrier.Destroy();
begin
    fGeneralPopupMenuItems.Free;
    inherited;
end;

procedure TSetupCarrier.DoInitGraphics;
begin
    inherited;
    self.Graphics.Callbacks.PopupCallback := DoPopup;
    self.Graphics.Callbacks.MouseDblClickCallback := self.DoDblClick;
    self.Graphics.Callbacks.PosChangedCallback := self.DoPosChanged;
    self.CreatePopUp();
end;

procedure TSetupCarrier.CreatePopup;
begin
    self.Graphics.PopupMenuInfos.AddItem(TPopupMenuInfoItem.Create(TLanguageString.
        Read('Evaluate Carrier Position', 'Carrier Position evaluieren'), False { checked } , true,
        CarrierMenuClick, 'mnuEvaluate'));
    self.Graphics.PopupMenuInfos.AddItem(TPopupMenuInfoItem.Create('-', False, True, nil, 'N1'));
    self.Graphics.PopupMenuInfos.AddItem(TPopupMenuInfoItem.Create(TLanguageString.Read('Delete Carrier',
        'Carrier löschen'), False { checked } , true, DeleteClick, 'Delete1'));
    self.Graphics.PopupMenuInfos.AddItem(TPopupMenuInfoItem.Create('-', False, True, nil, 'N2'));
    self.Graphics.PopupMenuInfos.AddItem(TPopupMenuInfoItem.Create(TLanguageString.Read('Change Carrier Name',
        'Carriernamen ändern'), False { checked } , true, ChangeNameClick, 'ChangeName'));
    self.Graphics.PopupMenuInfos.AddItem(TPopupMenuInfoItem.Create('-', False, True, nil, 'N3'));
    self.Graphics.PopupMenuInfos.AddItem(TPopupMenuInfoItem.Create(TLanguageString.Read('Move Carrier',
        'Carrier bewegen'), False, true, DoDblClick, 'mnuMoveCarrier'));

    fShowStackerOverviewMenuItem := TPopupMenuInfoItem.Create(TLanguageString.Read('Choose Stacker Level',
        'Stackerebene wählen'), False { checked } , true, ShowStackerOverviewDialog,
        'mnuShowStackerLevelDialog');
    self.Graphics.PopupMenuInfos.AddItem(fShowStackerOverviewMenuItem);
    fGeneralPopupMenuItems.Add(fShowStackerOverviewMenuItem);

    // fCarrierGraphics. Matrix.PopupMenu:=PopupMenu;
end;

procedure TSetupCarrier.DoPopup(aSender: TObject; aX, aY: double; var vPopupMenuInfos: TPopupMenuInfoList);
var
    x: integer;
begin
    fShowStackerOverviewMenuItem.Visible := self.SlotGroupFloors > 1;
    for x := 0 to vPopupMenuInfos.Count - 1 do
    begin
        vPopupMenuInfos[x].Enabled := (not self.IsLinked) or
            (fGeneralPopupMenuItems.IndexOf(vPopupMenuInfos[x]) >= 0);
    end;
end;

procedure TSetupCarrier.DoAfterCreateCarrierSlot(aCarrierSlot: TCarrierSlot);
var
    x: integer;
begin
    for x := 0 to self.Graphics.PopupMenuInfos.Count - 1 do
    begin
        aCarrierSlot.Graphics.PopupMenuInfos.AddItem(self.Graphics.PopupMenuInfos[x]);
    end;
    // slot should use same dopopup function so that we can disable some menu items if the carrier islinked=true
    aCarrierSlot.Graphics.Callbacks.PopupCallback := DoPopup;
end;

function TSetupCarrier.DoCreateCarrierSlot: TCarrierSlot;
begin
    result := TSetupCarrierSlot.Create();
end;

function TSetupCarrier.DoCreateStackerLevelSelectorGraphics: TStackerLevelSelectorGraphics;
begin
    result := TSetupStackerLevelSelectorGraphics.Create;
end;

procedure TSetupCarrier.Save;
var
    xTypeExists: boolean;
    xCarrierDA: TCarrierDataAdaptor;
    xLayoutDA: TLayoutDataAdaptor;
begin
    if (fDeleted) then
        EXIT;
    if self.IsLinked then
        EXIT;

    xCarrierDA := TCarrierDataAdaptor.Create;
    try
        xTypeExists := xCarrierDA.NameExists(fTypeName);
    finally
        FreeAndNil(xCarrierDA);
    end;

    if not xTypeExists then
    begin
        raise Exception.Create(TLanguageString.Read('Carrier type [{0}] not found in table',
            'Carriertyp [{0}] wurde in der Tabelle nicht gefunden.', [FTypeName]));
        EXIT;
    end;

    xLayoutDA := TLayoutDataAdaptor.Create;
    try
        if not xLayoutDA.CarrierExists('', TLayoutEditingFunctionProvider.Instance.CurrentLayout.LayoutName,
            fName) then
        begin
            xLayoutDA.AddCarrier(TLayoutEditingFunctionProvider.Instance.CurrentLayout.LayoutName,
                fWorkspaceID, fTypeName, fName, self.PosX, self.PosY, self.PosZ);
        end;
    finally
        FreeAndNil(xLayoutDA);
    end;
end;

procedure TSetupCarrier.NewPosition(X, Y: double);

begin
    self.PosX := x; // round((x - WBBorders.Left) / zoom );
    self.PosY := y; // round((y - WBBorders.Top) / zoom );
    // SetCarrPosition;
end;

procedure TSetupCarrier.CarrierMenuClick(Sender: TObject);

begin
    if ((Sender as TMenuItem).Name = 'mnuEvaluate') then
        EvaluationStart(nil);
end;

procedure TSetupCarrier.Delete();
begin
    TLayoutEditingFunctionProvider.Instance.CurrentSetupLayout.DeleteCarrier(self);
end;

procedure TSetupCarrier.DeleteClick(Sender: TObject);

var
    MsgString: string;
    x: integer;
    xSlot: TCarrierSlot;
    xRack: TSetupRack;
begin
    MsgString := '';

    for x := 1 to self.SlotCount do
    begin
        xSlot := self.GetSlotBySlotNr(x);
        if not Assigned(xSlot.Rack) then
            CONTINUE;
        xRack := (xSlot.Rack as TSetupRack);
        if not xRack.Deleted then
        begin
            if MsgString <> '' then
                MsgString := MsgString + ', ';
            MsgString := MsgString + xRack.Name;
        end;
    end;

    if (MsgString <> '') then
    begin
        MsgString := Copy(MsgString, 1, Length(MsgString) - 2);
        if gGUIManager.MessageBox(TLanguageString.Read('Do you want to delete carrier {0} (with racks: {1})?',
            'Soll Carrier {0} (mit Racks: {1}) gelöscht werden?', [self.Name, MsgString]),
            TLanguageString.Read('Delete Carrier', 'Carrier löschen'), MB_ICONQUESTION + MB_OKCANCEL) <>
            mrOK then
            EXIT;
    end;

    Delete();

    if (TLayoutEditingFunctionProvider.Instance.CurrentSetupLayout) <> nil then
    begin
        TLayoutEditingFunctionProvider.Instance.CurrentSetupLayout.Notsaved := true;
        TLayoutEditingFunctionProvider.Instance.CurrentSetupLayout.RefreshScene();
    end;
end;

procedure TSetupCarrier.ChangeNameClick(Sender: TObject);
var
    xNewName: string;
begin
    xNewName := fName;
    if not gGUIManager.InputQuery(TLanguageString.Read('Change Carrier Name', 'Carriernamen ändern'),
        TLanguageString.Read('Carrier Name', 'Carriername'), xNewName) then
        exit;
    xNewName := Copy(xNewName, 1, TLayoutDataAdaptor.MaxCarrierNameLen);
    if (xNewName = '') then
        exit;
    if Assigned(TLayoutEditingFunctionProvider.Instance.CurrentLayout.FindCarrierByName(xNewName)) then
    begin
        showMessage(TLanguageString.Read('Carrier name already exists!', 'Carriername existiert bereits!'));
        exit;
    end;

    fName := xNewName;
    TLayoutEditingFunctionProvider.Instance.CurrentSetupLayout.NotSaved := true;
end;

procedure TSetupCarrier.MovePos();
var
    xAbsolutePos: TPoint4d;
begin
    if (self.IsLinked) then
        EXIT;

    SetRacksVisible(false);
    TLayoutEditingFunctionProvider.Instance.CurrentSetupLayout.RefreshScene();
    xAbsolutePos := (self.Workspace as TWorkspace).Graphics.ClientToScene(self.PosX, self.PosY, self.PosZ);
    self.Graphics.PosChangeDragStart(xAbsolutePos.x, xAbsolutePos.y, xAbsolutePos.z);
end;

{
  procedure TSetupCarrier.MovePosEntry();
  var
  xForm : TfrmEdXY;
  xChanged : boolean;
  begin
  xChanged := false;
  xForm := TfrmEdXY.Create(nil);
  try
  xForm.PosX := self.PosX;
  xForm.PosY := self.PosY;

  if xForm.ShowModal <> mrOK then
  EXIT;

  if ( xForm.PosX <> self.PosX ) then begin
  self.SetWB_X(  xForm.PosX );
  xChanged:= true;
  end;

  if ( xForm.PosY <> self.PosY ) then begin
  self.SetWB_Y( xForm.PosY );
  xChanged := true;
  end;

  if xChanged then begin
  TLayoutEditingFunctionProvider.Instance.CurrentLayout.NotSaved:=true;
  TLayoutEditingFunctionProvider.Instance.CurrentLayout.RefreshScene();
  end;
  finally
  xForm.Free;
  end;

  end;
}
procedure TSetupCarrier.SetRacksVisible(const aVisible: boolean);
var
    x: integer;
    xSlot: TCarrierSlot;
    xRack: TSetupRack;
begin

    // // alle Racks unsichtbar machen
    for x := 1 to self.SlotCount do
    begin
        xSlot := self.GetSlotBySlotNr(x);
        if not Assigned(xSlot.Rack) then
            CONTINUE;
        xRack := (xSlot.Rack as TSetupRack);
        xRack.Visible := aVisible;
    end;
end;

procedure TSetupCarrier.DoDblClick(aSender: TObject);

begin
    MovePos();

    { TODO -opk -cWB : }
    // gMoveRack := nil;
    // if (FDrag) and (EvaluateForm=nil) then begin
    // // alle Racks unsichtbar machen
    // for i:=0 to WB.NoOfRacks-1 do if (WB.RackA[i].Slot.CarrierName = FCarrierName) then
    // WB.RackA[i].Visible := false;
    //
    // // Drag-Action starten
    // FDragHandle.Attach(self);
    // end;
    // uMovingCarrier := self;

end;

procedure TSetupCarrier.DoPosChanged(aSender: TObject; const aX, aY, aZ: double);
var
    xChanged: boolean;
    xPoint: TPoint4d;
begin
    xChanged := false;
    xPoint := (self.Workspace as TWorkspace).Graphics.SceneToClient(aX, aY, aZ);
    // xPoint.x := aX;
    // xPoint.y := aY;

    if (xPoint.X <> self.PosX) then
    begin
        self.SetWB_X(xPoint.X);
        xChanged := true;
    end;

    if (xPoint.Y <> self.PosY) then
    begin
        self.SetWB_Y(xPoint.Y);
        xChanged := true;
    end;

    self.SetRacksVisible(true);
    TLayoutEditingFunctionProvider.Instance.CurrentSetupLayout.RefreshScene();

    if xChanged then
    begin
        TLayoutEditingFunctionProvider.Instance.CurrentSetupLayout.NotSaved := true;
    end;
end;

procedure TSetupCarrier.ShowStackerOverviewDialog(Sender: TObject);

begin
    self.ShowStackerOverviewDialog();
    TLayoutEditingFunctionProvider.Instance.CurrentSetupLayout.RefreshScene();
end;

procedure TSetupCarrier.EvaluationStart(aRack: TRack);
begin
    if Assigned(self.Evaluation) then
        EXIT; // es wird bereits evaluiert
    if not TDesignModuleSettingFinder.DeviceExists(IPipDevice) then
        EXIT; // es gibt keinen Arm zum evaluieren

    FSaveColor := fGraphics.Color;
    fGraphics.Color := clRed;

    { TODO -opk -cWB : }
    // PopupMenu.AutoPopup := false;
    // Matrix.PopupMenu.AutoPopup := false;

    fEvaluation := TLayoutEditingFunctionProvider.Instance.EvaluationStart(self, aRack);
    ChangeRackEvaluationState(true);
end;

procedure TSetupCarrier.ChangeRackEvaluationState(aIsEvaluationStart: boolean);
var
    x: integer;
    xSlot: TCarrierSlot;
    xRack: TSetupRack;
begin
    for x := 1 to self.SlotCount do
    begin
        xSlot := self.GetSlotBySlotNr(x);
        if not(xSlot.Rack is TSetupRack) then
            CONTINUE;
        xRack := xSlot.Rack as TSetupRack;
        if xRack.Deleted then
            CONTINUE;
        if aIsEvaluationStart then
            xRack.EvaluationStart()
        else
            xRack.EvaluationEnd();
    end;
end;

procedure TSetupCarrier.EvaluationEnd;
var
    x: integer;
    xRecs: TLayoutRecArray;
    xNewX, xNewY, xOldX, xOldY: TPosMM;
    xDA: TLayoutDataAdaptor;
begin
    try
        if not Assigned(fEvaluation) then
            Exit;
        if not self.Evaluation.EvaluationEndXYChanged(xNewX, xNewY, xOldX, xOldY) then
            EXIT;

        self.SetWorldX(xNewX);
        self.SetWorldY(xNewY);

        // ------ Layout komplett speichern
        if not TLayoutEditingFunctionProvider.Instance.CurrentSetupLayout.SaveAll then
            exit;

        // ---------------- Weitere Carrier gleichen Typs in anderen Layouts finden
        xDA := TLayoutDataAdaptor.Create;
        try
            xDA.ReadRecsByTypeAndCarrXY(self.TypeName, xOldX, xOldY, xRecs);

            for x := 0 to high(xRecs) do
            begin
                if (SameText(xRecs[x].Layout, TLayoutEditingFunctionProvider.Instance.CurrentLayout.Name) or
                    (xRecs[x].Run <> '')) then
                    CONTINUE;

                case gGUIManager.MessageBox(TLanguageString.
                    Read('Carrier {0} in Layout {1} has the same XY-values. Change also?',
                    'Carrier {0} im Layout {1} hat die gleichen XY-Werte. Ebenfalls ändern?',
                    [xRecs[x].CarrierName, xRecs[x].Layout]), TLanguageString.Read('XY-Values Changes',
                    'XY-Werte geändert'), MB_YESNOCANCEL + MB_ICONQUESTION) of
                    mrYES:
                        xDA.WriteCarrXYForCarrier(xRecs[x].Layout, xRecs[x].CarrierName, self.PosX,
                            self.PosY);
                    mrNO:
                        CONTINUE;
                    else
                        BREAK;
                end;
            end;
        finally
            FreeAndNil(xDA);
        end;

    finally
        self.Graphics.Color := SaveColor;
        // PopupMenu.AutoPopup := true;
        // Matrix.PopupMenu.AutoPopup := true;
        if Assigned(fEvaluation) then
            FreeAndNil(fEvaluation);
        ChangeRackEvaluationState(false);
        TLayoutEditingFunctionProvider.Instance.CurrentSetupLayout.RefreshScene();
    end;
end;

procedure TSetupCarrier.ChooseEvalPos(aRack: TRack; aPos: integer);

begin
    if Assigned(fEvaluation) then
        fEvaluation.EvaluationSetTubeRef(aRack, aPos);
end;

{ TODO -opk -cWB : }

procedure TSetupCarrier.RedrawSlots();
var
    x: integer;
    xSlot: TCarrierSlot;
begin
    for x := 1 to self.SlotCount do
    begin
        xSlot := self.GetSlotBySlotNr(x);
        if not Assigned(xSlot.Rack) then
            CONTINUE;
        (xSlot.Rack as TRack).DrawRackPosition;
    end;
end;

procedure TSetupCarrier.SetWB_X(aValue: TPosMM);
begin
    self.PosX := aValue;
    RedrawSlots();
end;

procedure TSetupCarrier.SetWB_Y(aValue: TPosMM);
begin
    self.PosY := aValue;
    RedrawSlots();
end;

procedure TSetupCarrier.SetWB_Z(aValue: TPosMM);
begin
    self.PosZ := aValue;
    RedrawSlots();
end;

function TSetupCarrier.CalcNewWorkspacePos(const aNewWorldPos: TPoint4d): TPoint4d;
var
    xDifVector: TVector4d;
    xOldPos: TPoint4d;
begin
    xOldPos := self.CalcCarrierPosition;
    xDifVector := GetDifVector4d(aNewWorldPos, xOldPos);
    result := InterpPointAlongVector4d(MakePoint4d(self.PosX, self.PosY, self.PosZ), xDifVector, 1);
end;

procedure TSetupCarrier.SetWorldX(const aValue: TPosMM);
var
    xNewPos: TPoint4d;
begin
    xNewPos := CalcNewWorkspacePos(MakePoint4d(aValue, 0, 0));
    SetWB_X(xNewPos.x);
end;

procedure TSetupCarrier.SetWorldY(const aValue: TPosMM);
var
    xNewPos: TPoint4d;
begin
    xNewPos := CalcNewWorkspacePos(MakePoint4d(0, aValue, 0));
    SetWB_Y(xNewPos.y);
end;

procedure TSetupCarrier.SetVisible(aVisible: boolean);
var
    xVisible: boolean;
begin
    xVisible := aVisible and (not FDeleted);
    inherited SetVisible(xVisible);
end;

{ TSetupTipsetDevice }

constructor TSetupTipsetDevice.Create();
begin
    inherited Create();
end;

procedure TSetupTipsetDevice.SaveTips(const aLayoutName: string);
var
    x: integer;
    xDA: TTipsetDataAdaptor;
begin
    if self.IsLinked then
        EXIT;
    // ----------- Tips in Datenbank eintragen
    xDA := TTipsetDataAdaptor.Create;
    try
        for x := 0 to self.Tips.Count - 1 do
        begin
            if (self.Tips[x] <> nil) then
                xDA.AddTipset(aLayoutName, self.Tips[x].PipDeviceName, self.Tips[x].TipIndex + 1,
                    self.Tips[x].TypeName);
        end;
    finally
        FreeAndNil(xDA);
    end;
end;

{ TSetupWorkspace }

constructor TSetupWorkspace.Create();
begin
    inherited;
    fGeneralPopupMenuItems := TObjectList<TPopupMenuInfoItem>.Create(false);
    fDeleted := false;
end;

destructor TSetupWorkspace.Destroy();
begin
    fGeneralPopupMenuItems.Free;
    inherited;
end;

procedure TSetupWorkspace.DoInitGraphics;
begin
    inherited;
    self.Graphics.Callbacks.MouseDragOverCallback := self.DoDragOver;
    self.Graphics.Callbacks.MouseDragDropCallback := self.DoDragDrop;
    self.Graphics.Callbacks.PopupCallback := self.DoPopup;
    self.Graphics.Callbacks.MouseDownCallback := self.DoMouseDown;
    CreatePopup();
end;

procedure TSetupWorkspace.CreatePopup();
var
    x: integer;
begin
    self.Graphics.PopupMenuInfos.AddItem(TPopupMenuInfoItem.Create(TLanguageString.
        Read('Change Workspace Name', 'Workspacenamen ändern'), false { checked } , true, ChangeNameClick,
        'ChangeName'));
    self.Graphics.PopupMenuInfos.AddItem(TPopupMenuInfoItem.Create(TLanguageString.Read('Delete Workspace',
        'Workspace löschen'), false, true, DeleteClick, 'DeleteClick1'));

    fGeneralPopupMenuItems.Add(TPopupMenuInfoItem.Create(TLanguageString.Read('Edit Workspace View Relation',
        'Workspace View-Relation bearbeiten'), False { checked } , true, EditViewRelation,
        'EditViewRelation'));
    for x := 0 to fGeneralPopupMenuItems.Count - 1 do
        self.Graphics.PopupMenuInfos.AddItem(fGeneralPopupMenuItems[x]);
end;

procedure TSetupWorkspace.DoPopup(aSender: TObject; aX, aY: double; var vPopupMenuInfos: TPopupMenuInfoList);
var
    x: integer;
begin
    for x := 0 to vPopupMenuInfos.Count - 1 do
    begin
        vPopupMenuInfos[x].Enabled := (not self.IsLinked) or
            (fGeneralPopupMenuItems.IndexOf(vPopupMenuInfos[x]) >= 0);
    end;
end;

procedure TSetupWorkspace.DeleteClick(aSender: TObject);
var
    xIsWorkspaceClear: boolean;
    x: integer;
begin
    xIsWorkspaceClear := true;
    for x := 0 to TLayoutEditingFunctionProvider.Instance.CurrentLayout.NoOfCarrier - 1 do
    begin
        if TLayoutEditingFunctionProvider.Instance.CurrentLayout.Carriers[x].Workspace = self then
        begin
            xIsWorkspaceClear := false;
            BREAK;
        end;
    end;

    if not xIsWorkspaceClear then
    begin
        if gGUIManager.MessageBox(TLanguageString.
            Read('Do you want to delete workspace {0} (with all carriers and racks)?',
            'Soll Workspace {0} (mit allen Carriers und Racks) gelöscht werden?', [self.Name]), '',
            MB_ICONQUESTION + MB_OKCANCEL) <> mrOK then
            EXIT;
    end;

    for x := 0 to TLayoutEditingFunctionProvider.Instance.CurrentLayout.NoOfCarrier - 1 do
    begin
        if TLayoutEditingFunctionProvider.Instance.CurrentLayout.Carriers[x].Workspace = self then
            (TLayoutEditingFunctionProvider.Instance.CurrentLayout.Carriers[x] as TSetupCarrier).Delete();
    end;

    self.Graphics.Visible := false;
    FDeleted := true;

    if (TLayoutEditingFunctionProvider.Instance.CurrentLayout) <> nil then
    begin
        TLayoutEditingFunctionProvider.Instance.CurrentSetupLayout.Notsaved := true;
        TLayoutEditingFunctionProvider.Instance.CurrentSetupLayout.RefreshScene();
    end;

    FName := '';
end;

procedure TSetupWorkspace.ChangeNameClick(Sender: TObject);
var
    xNewName: string;
begin
    xNewName := FName;
    if not gGUIManager.InputQuery(TLanguageString.Read('Change Workspace Name', 'Workspacenamen ändern'),
        TLanguageString.Read('Workspace Name', 'Workspace-Name'), xNewName) then
        exit;

    if (xNewName = '') then
        exit;

    xNewName := Copy(xNewName, 1, TLayoutWorkspaceDataAdaptor.GetNameMaxLen());
    if (xNewName <> FName) then
    begin
        if Assigned(TLayoutEditingFunctionProvider.Instance.CurrentLayout.FindWorkspaceByName(xNewName)) then
        begin
            TDialogUtils.MessageBox(TLanguageString.Read('Workspace name already exists',
                'Workspacename existiert bereits'), '', MB_OK);
            EXIT;
        end;
        FName := xNewName;
        TLayoutEditingFunctionProvider.Instance.CurrentSetupLayout.NotSaved := true;
    end;
end;

procedure TSetupWorkspace.Delete;
begin
    TLayoutEditingFunctionProvider.Instance.CurrentSetupLayout.DeleteWorkspace(self);
end;

procedure TSetupWorkspace.DoDragDrop(aSender, aSource: TObject; aX, aY: double);
var
    xTag: TObject;
    xName: string;
    xCarrier: TCarrier;
begin
    xTag := (aSender as TLayoutElementGraphics).Tag;
    if not(xTag is TSetupWorkspace) then
        EXIT;

    if TLayoutEditingFunctionProvider.Instance.DragSourceIsNode(aSource) then
    begin
        if TLayoutEditingFunctionProvider.Instance.DragSourceIsRackNode(aSource, xName) then
        begin
            // Carrier wird zusammen mit dem Rack platziert
            xCarrier := TLayoutEditingFunctionProvider.Instance.CurrentSetupLayout.AddNewCarrier(self.ID, aX,
                aY, TCarrierDataAdaptor.InvisibleCarrierName, true);
            if Assigned(xCarrier) then
                TLayoutEditingFunctionProvider.Instance.CurrentSetupLayout.AddNewRack(xName, xCarrier.Name,
                    1, false);
        end
        else if TLayoutEditingFunctionProvider.Instance.DragSourceIsCarrierNode(aSource, xName) then
        begin
            TLayoutEditingFunctionProvider.Instance.CurrentSetupLayout.AddNewCarrier(self.ID, aX, aY,
                xName, false);
        end
        else
        begin
            gGUIManager.MessageBox(TLanguageString.Read('Select a carrier!', 'Wählen Sie einen Carrier aus!'),
                TLanguageString.Read('Insert Carrier', 'Carrier einfügen'), MB_ICONSTOP);
        end;
    end;
end;

procedure TSetupWorkspace.DoDragOver(aSender, aSource: TObject; aX, aY: double; aState: TGraphicsDragState;
    var vAccept: boolean);
var
    xAddNewCarrierAllowed: boolean;
    xMoveCarrierAllowed: boolean;
    xName: string;
begin
    vAccept := false;
    if self.IsLinked then
        EXIT;

    xAddNewCarrierAllowed := TLayoutEditingFunctionProvider.Instance.DragSourceIsNode(aSource) and
        (TLayoutEditingFunctionProvider.Instance.DragSourceIsRackNode(aSource, xName) or
        TLayoutEditingFunctionProvider.Instance.DragSourceIsCarrierNode(aSource, xName));

    xMoveCarrierAllowed := (aSource is TSetupCarrier);
    vAccept := xAddNewCarrierAllowed or xMoveCarrierAllowed;
end;

procedure TSetupWorkspace.DoMouseDown(aSender: TObject; aButton: TGraphicsMouseButton;
    aShift: TGraphicsShiftState; aX, aY: double);
var
    xCarrierName: string;
begin
    if self.IsLinked then
        EXIT;

    if (gssAlt in aShift) and (aButton = gmbLeft) and
        TLayoutEditingFunctionProvider.Instance.MainNodeIsCarrierNode(xCarrierName) then
    begin
        TLayoutEditingFunctionProvider.Instance.CurrentSetupLayout.AddNewCarrier(self.ID, aX, aY,
            xCarrierName, (gssShift in aShift));
    end;
end;

procedure TSetupWorkspace.EditViewRelation(aSender: TObject);
var
    xUseCustomView: boolean;
    xCoordSystemRelation: TCoordSystemRelation;
begin
    xUseCustomView := self.UseCustomView;

    xCoordSystemRelation := TCoordSystemRelation.Create();
    try
        xCoordSystemRelation.Assign(self.CurrentViewCoordSystem);
        if TfrmLayoutWorkspaceEditor.InstanceShowModal(xCoordSystemRelation, xUseCustomView) = mrOK then
        begin
            self.CustomViewCoordSystem.Assign(xCoordSystemRelation);
            self.UseCustomView := xUseCustomView;
            // call view changed again because we may have changed a coordinate parameter
            self.ViewChanged();
            TLayoutEditingFunctionProvider.Instance.CurrentSetupLayout.NotSaved := true;
        end;
    finally
        xCoordSystemRelation.Free;
    end;

end;

procedure TSetupWorkspace.Save;
var
    xTypeExists: boolean;
    xViewRelationRec: TCoordSystemRelationRec;
    xWDA: TWorkspaceDataAdaptor;
    xLWDA: TLayoutWorkspaceDataAdaptor;
begin
    if (fDeleted) then
        EXIT;

    if self.IsLinked then
        EXIT;

    xWDA := TWorkspaceDataAdaptor.Create;
    try
        xTypeExists := xWDA.IDExists(self.WorkspaceTypeID);
    finally
        FreeAndNil(xWDA);
    end;

    if not xTypeExists then
    begin
        raise Exception.Create(TLanguageString.Read('Workspace type [{0}] not found in table',
            'Workspacetyp [{0}] wurde in der Tabelle nicht gefunden.', [self.WorkspaceTypeID]));
    end;

    xViewRelationRec.Valid := self.UseCustomView;
    if xViewRelationRec.Valid then
        self.AssignCoordSystemRelationToRec(self.CustomViewCoordSystem, xViewRelationRec);

    xLWDA := TLayoutWorkspaceDataAdaptor.Create;
    try
        if not xLWDA.WorkspaceNameExists(TLayoutEditingFunctionProvider.Instance.CurrentLayout.LayoutName,
            fName) then
        begin
            xLWDA.AddWorkspace(self.ID, TLayoutEditingFunctionProvider.Instance.CurrentLayout.LayoutName,
                fName, fWorkspaceTypeID, xViewRelationRec);
        end;
    finally
        FreeAndNil(xLWDA);
    end;
end;

{ TSetupLayoutLink }

constructor TSetupLayoutLink.Create();
begin
    inherited Create();
    fDeleted := false;
end;

procedure TSetupLayoutLink.Save;
var
    xDA: TLayoutDataAdaptor;
begin
    if (fDeleted) then
        EXIT;

    xDA := TLayoutDataAdaptor.Create;
    try
        if not xDA.LinkedLayoutExists('', TLayoutEditingFunctionProvider.Instance.CurrentLayout.LayoutName,
            fName) then
        begin
            xDA.AddLinkedLayout(TLayoutEditingFunctionProvider.Instance.CurrentLayout.LayoutName, fName);
        end;
    finally
        FreeAndNil(xDA);
    end;
end;

{ TSetupLayout }

constructor TSetupLayout.Create(const aLayoutName, aRunName: string);
begin
    inherited Create(aLayoutName, aRunName);

    SetNotSaved(false);
    FReason := '';

    { TODO -oPK -cWB : }
    // if (PopupMenu<>nil) then PopupMenu.AutoPopup:=false;
    // OnDragOver  := WBDragOver;
    // OnDragDrop  := WBDragDrop;
    // OnMouseDown := MouseDown1;
    // SetMouseEvents();
end;
//
// procedure TSetupLayout.SetMouseEvents();
// begin
// gArmBoundManager.SetMouseEvents( WBDragOver, WBDragDrop, MouseDown1 );
// end;

destructor TSetupLayout.Destroy;
begin
    inherited Destroy;
end;

procedure TSetupLayout.DoInitGraphics;
begin
    inherited;
    self.Graphics.Callbacks.MouseDragOverCallback := self.DoDragOver;
    self.Graphics.Callbacks.MouseDragDropCallback := self.DoDragDrop;
end;

procedure TSetupLayout.RefreshScene();
begin
    TLayoutEditingFunctionProvider.Instance.SceneChanged();
end;

function TSetupLayout.GetWorkspaceTypeIDByTypeName(const aTypeName: string): integer;
var
    xDA: TWorkspaceDataAdaptor;
begin
    xDA := TWorkspaceDataAdaptor.Create;
    try
        ASSERT(xDA.ReadIDByName(aTypeName, result), 'Workspace Type not found');
    finally
        FreeAndNil(xDA);
    end;
end;

function TSetupLayout.GetNewWorkspaceName(const aSourceName: string): string;
const
    cMaxWorkspaceNameLen = 20;
var
    xNewNr: integer;
begin
    xNewNr := 1;
    while true do
    begin
        result := Copy(aSourceName, 1, cMaxWorkspaceNameLen - 4) + '_' + IntToStr(xNewNr);
        // prüfen, ob Name bereits vorhanden
        if (not Assigned(self.FindWorkspaceByName(result))) then
            EXIT;
        Inc(xNewNr);
    end;
end;

procedure TSetupLayout.AddNewWorkspace(const aSourceName: string; aDontAskName: boolean);
var
    xNewName: string;
    xWorkspaceID: integer;
    xTypeID: integer;
    xViewRelation: TCoordSystemRelationRec;
    xWorkspace: TWorkspace;
begin
    xNewName := GetNewWorkspaceName(aSourceName);

    if (not aDontAskName) then
    begin
        if not gGUIManager.InputQuery(TLanguageString.Read('Please insert workspace name',
            'Bitte geben Sie einen Workspacenamen ein'), TLanguageString.Read('Insert Workspace Name',
            'Workspacenamen eingeben'), xNewName) then
            EXIT;
        if xNewName = '' then
            exit;
        xNewName := Copy(xNewName, 1, TLayoutDataAdaptor.MaxCarrierNameLen);
    end;

    if Assigned(self.FindWorkspaceByName(xNewName)) then
    begin
        gGUIManager.MessageBox(TLanguageString.Read('Workspace name already exists',
            'Workspacename existiert bereits'), '', MB_ICONSTOP);
        EXIT;
    end;

    xTypeID := GetWorkspaceTypeIDByTypeName(aSourceName);
    xViewRelation := TWorkspaceUtils.MakeDefCoordSystemRelationRec();

    xWorkspaceID := self.GenerateNewWorkspaceID();
    xWorkspace := self.AddWorkspace(false, xWorkspaceID, xNewName, xTypeID, xViewRelation);
    xWorkspace.Visible := true;

    NotSaved := true;

    RefreshScene();

end;

function TSetupLayout.GetNewCarrierName(const aSourceName: string): string;
var
    xNewNr: integer;
begin
    xNewNr := 1;
    while true do
    begin
        result := Copy(aSourceName, 1, TLayoutDataAdaptor.MaxCarrierNameLen - 4) + '_' + IntToStr(xNewNr);
        // prüfen, ob Name bereits vorhanden
        if (not Assigned(self.FindCarrierByName(result))) then
            EXIT;
        Inc(xNewNr);
    end;
end;

function TSetupLayout.AddNewCarrier(aWorkspaceID: integer; aX, aY: double; const aSourceName: string;
    aDontAskName: boolean): TCarrier;
var
    xNewName: string;
begin
    result := nil;
    xNewName := GetNewCarrierName(aSourceName);

    if (not aDontAskName) then
    begin
        if not gGUIManager.InputQuery(TLanguageString.Read('Please insert carrier name!',
            'Bitte geben Sie einen Carriernamen ein!'), TLanguageString.Read('Carrier Name', 'Carrier-Name'),
            xNewName) then
            exit;
        if xNewName = '' then
            exit;
        xNewName := Copy(xNewName, 1, TLayoutDataAdaptor.MaxCarrierNameLen);
    end;

    if Assigned(self.FindCarrierByName(xNewName)) then
    begin
        gGUIManager.MessageBox(TLanguageString.Read('Carrier name already exists!',
            'Carriername existiert bereits!'), '', MB_ICONSTOP);
        EXIT;
    end;

    result := self.AddCarrier(false, xNewName, aSourceName, aWorkspaceID, aX, aY, gWorkbenchZ);
    result.Visible := true;

    NotSaved := true;

    RefreshScene();
end;

function TSetupLayout.GetNewRackName(const aSourceName: string): string;
var
    xNewNr: integer;
begin
    // kein Anhang bei "WASH", "RWASH", "WASTE", "DRY"
    if TRackDefinitions.RackIsGeneralSpecialRack(aSourceName) then
    begin
        result := aSourceName;
        // prüfen, ob Name bereits vorhanden
        if (not Assigned(self.FindRackByName(result))) then
            EXIT;
    end;

    xNewNr := 1;
    while true do
    begin
        result := Copy(aSourceName, 1, TLayoutDataAdaptor.MaxRackNameLen - 4) + '_' + IntToStr(xNewNr);
        // prüfen, ob Name bereits vorhanden
        if not Assigned(self.FindRackByName(result)) then
            EXIT;
        Inc(xNewNr);
    end;
end;

function TSetupLayout.RequestRackName(aRackTypeName, aNewRackName, aCarrierAndSlotName: string;
    out oRackName, oRackID: string): boolean;
var
    xfrmWizard: TfrmWizard;
    xModalResult: TModalResult;
begin
    result := false;

    // Create a "Layout Name" wizard
    xfrmWizard := TfrmWizard.Create(Application);
    xfrmWizard.Caption := TLanguageString.Read('Select Layout', 'Layout auswählen');
    xfrmWizard.Add(TNewRackPage.Create(xfrmWizard, aRackTypeName, aNewRackName, aCarrierAndSlotName));

    xModalResult := xfrmWizard.ShowModal;
    if (xModalResult = mrOK) then
    begin
        result := true;
        oRackName := (xfrmWizard.Items[0] as TNewRackPage).RackName;
        oRackID := (xfrmWizard.Items[0] as TNewRackPage).RackID;
    end;
    xfrmWizard.Free;
end;

procedure TSetupLayout.AddNewRack(const aRackTypeName, aCarrierName: string; aSlotNr: integer;
    aDontAskName: boolean);
var
    xNewName, xNewRackID: string;
    xCarrier: TCarrier;
    xSlot: TCarrierSlot;
    xRack: TRack;
    xCarrierAndSlotName: string;
begin
    xCarrier := self.FindCarrierByName(aCarrierName);
    ASSERT(Assigned(xCarrier), 'Carrier not found');
    xSlot := xCarrier.GetSlotBySlotNr(aSlotNr);
    ASSERT(Assigned(xSlot), 'Slot not found');
    if not(xSlot as TSetupCarrierSlot).CheckIfEmpty() then
        EXIT;

    if xCarrier.TypeName = TCarrierDataAdaptor.InvisibleCarrierName then
        xCarrierAndSlotName := ''
    else
        xCarrierAndSlotName := aCarrierName + ', Slot:' + IntToStr(aSlotNr);

    if (not aDontAskName) then
    begin
        if not self.RequestRackName(aRackTypeName, self.GetNewRackName(aRackTypeName), xCarrierAndSlotName,
            xNewName, xNewRackID) then
            EXIT;
        if (xNewName = '') then
            exit;
    end
    else
    begin
        xNewName := self.GetNewRackName(aRackTypeName);
        xNewRackID := xNewName;
    end;

    xRack := self.AddRack(false, xNewName, xNewRackId, aRackTypeName, aCarrierName, aSlotNr, rotation_0);
    xRack.Visible := true;
    NotSaved := true;

    RefreshScene();
end;

function TSetupLayout.AskForSave(aLoadAgain: boolean; out oLayoutEmpty: boolean): boolean;
// Sollte immer beim Schließen aufgerufen werden
var
    x, xButton: integer;
    xLayoutDA: TLayoutDataAdaptor;
    xTipsetDA: TTipsetDataAdaptor;
begin
    oLayoutEmpty := false;
    result := false;
    // Alle Evaluierungs-Fenster schließen
    for x := 0 to self.NoOfCarrier - 1 do
        if ((self.Carriers[x] as TSetupCarrier).Evaluation <> nil) then
        begin
            gGUIManager.MessageBox(TLanguageString.Read('Please close all evaluation windows!',
                'Bitte Schießen Sie alle Evaluierungs-Fenster!'), '', MB_ICONSTOP);
            EXIT;
        end;

    result := true;
    if (NotSaved) then
    begin
        xButton := gGUIManager.MessageBox(TLanguageString.Read('Save changes in current layout?',
            'Sollen die Änderungen im aktuellen Layout gespeichert werden?'),
            TLanguageString.Read('Save Layout', 'Layout speichern'), MB_ICONQUESTION + MB_YESNOCANCEL);

        if (xButton = IDYES) then
            if not SaveAll then
                result := false;

        if (xButton = IDNO) then
            if (aLoadAgain) then
                TLayoutEditingFunctionProvider.Instance.Load();

        if (xButton = IDCANCEL) then
            result := false;
    end;

    self.NotSaved := false;

    xLayoutDA := TLayoutDataAdaptor.Create;
    try
        if (not xLayoutDA.NameExists(fLayoutName)) then
        begin

            xTipsetDA := TTipsetDataAdaptor.Create;
            try
                if (not xTipsetDA.NameExists(fLayoutName)) then
                    oLayoutEmpty := true;
            finally
                FreeAndNil(xTipsetDA);
            end;
        end;
    finally
        FreeAndNil(xLayoutDA);
    end;
end;

function TSetupLayout.SaveAll: boolean;
var
    x: integer;
begin
    result := false;
    if not TAppSettings.ItemConfirmEdit('Layout', FLayoutName) then
        EXIT;

    result := true;

    TLayoutDataAdaptorExt.Instance.DeleteAllLayoutRelated(fLayoutName);

    for x := 0 to self.fTipsets.Count - 1 do
        (fTipsets[x] as TSetupTipsetDevice).SaveTips(fLayoutName);

        for x := 0 to self.fLayoutLinks.Count - 1 do
            (fLayoutLinks[x] as TSetupLayoutLink).Save;

            // Save Workspaces, Carriers, and Racks
            for x := 0 to fWorkspaces.Count - 1 do
                (fWorkspaces[x] as TSetupWorkspace).Save;

                for x := 0 to self.NoOfCarrier - 1 do
                    (fCarriers[x] as TSetupCarrier).Save;

                    for x := 0 to self.NoOfRacks - 1 do
                        (fRacks[x] as TSetupRack).Save;

                        NotSaved := false;
end;

function TSetupLayout.AddNewLayout(aLayoutName: string; out oWorkspace: string; out oTipType: string): string;
var
    xfrmWizard: TfrmWizard;
    xModalResult: TModalResult;
    xIniAccess: IWinlissyIniAccess;
begin
    result := '';

    xIniAccess := gCommonDll.CreateAppIni;

    if (TAppSettings.IsOneWorkspaceMode and TAppSettings.IsOneTipTypeMode) then
    begin
        oWorkspace := TWorkspaceDataAdaptor.InstReadAllNames[0];
        oTipType := TTipTypeDataAdaptor.InstReadAllNames[0];
    end
    else
    begin
        // Create a "Layout Name" wizard
        xfrmWizard := TfrmWizard.Create(Application);
        xfrmWizard.Caption := TLanguageString.Read('Create new Layout', 'Neues Layout erstellen');
        xfrmWizard.Add(TNewLayoutPage.Create(xfrmWizard, aLayoutName));

        xModalResult := xfrmWizard.ShowModal;
        if (xModalResult = mrOK) then
        begin
            oWorkspace := (xfrmWizard.Items[0] as TNewLayoutPage).WorkspaceName;
            oTipType := (xfrmWizard.Items[0] as TNewLayoutPage).TipType;
        end;
        xfrmWizard.Free;
    end;
end;

function TSetupLayout.LayoutLinksIndexOfName(const aName: string): integer;
var
    x: integer;
begin
    result := -1;
    for x := 0 to fLayoutLinks.Count - 1 do
    begin
        if SameText(fLayoutLinks[x].Name, aName) then
        begin
            result := x;
            EXIT;
        end;
    end;
end;

procedure TSetupLayout.AddNewLinkedLayout(const aSourceName: string);
var
    xLinkedLayoutName: string;
begin
    xLinkedLayoutName := aSourceName;
    if LayoutLinksIndexOfName(xLinkedLayoutName) >= 0 then
    begin
        gGUIManager.MessageBox(TLanguageString.Read('A Link to the Layout {0} already exists',
            'Es besteht schon eine Verknüpfung zu dem Layout {0}', [xLinkedLayoutName]), '', MB_ICONSTOP);
        EXIT;
    end;
    self.AddLayoutLink(xLinkedLayoutName);

    // we need to set the whole layout to visible again so that the new elements are set to visible.
    self.Visible := true;

    self.NotSaved := true;
    RefreshScene();
end;

procedure TSetupLayout.DoDragDrop(aSender, aSource: TObject; aX, aY: double);
var
    xTag: TObject;
    xName: string;
begin
    xTag := (aSender as TLayoutElementGraphics).Tag;
    if not(xTag is TSetupLayout) then
        EXIT;

    if TLayoutEditingFunctionProvider.Instance.DragSourceIsNode(aSource) and
        TLayoutEditingFunctionProvider.Instance.DragSourceIsWorkspaceNode(aSource, xName) then
    begin
        AddNewWorkspace(xName, false);
    end
    else if TLayoutEditingFunctionProvider.Instance.DragSourceIsNode(aSource) and
        TLayoutEditingFunctionProvider.Instance.DragSourceIsLayoutNode(aSource, xName) then
    begin
        AddNewLinkedLayout(xName);
    end;
end;

procedure TSetupLayout.DoDragOver(aSender, aSource: TObject; aX, aY: double; aState: TGraphicsDragState;
    var vAccept: boolean);
var
    xLayoutNodeAllowed: boolean;
    xWorkspaceNodeAllowed: boolean;
    xName: string;
begin
    xWorkspaceNodeAllowed := TLayoutEditingFunctionProvider.Instance.DragSourceIsNode(aSource) and
        TLayoutEditingFunctionProvider.Instance.DragSourceIsWorkspaceNode(aSource, xName);
    xLayoutNodeAllowed := TLayoutEditingFunctionProvider.Instance.DragSourceIsNode(aSource) and
        (TLayoutEditingFunctionProvider.Instance.DragSourceIsLayoutNode(aSource, xName) and
        (not SameText((aSource as TTreeView).Selected.Text, self.LayoutName)));
    vAccept := xWorkspaceNodeAllowed or xLayoutNodeAllowed;
end;

procedure TSetupLayout.UpdateEvalCarr;
var
    i: integer;
begin
    for i := 0 to self.NoOfCarrier - 1 do
    begin
        if (self.Carriers[i] as TSetupCarrier).Evaluation <> nil then
            (self.Carriers[i] as TSetupCarrier).Evaluation.EvaluationReferenceChanged;
    end;
end;

procedure TSetupLayout.EditZHeights;
var
    xDummy: boolean;
    frmEdZPos: TfrmEdZPos;
begin
    if (self.NoOfCarrier <= 0) then
        EXIT;
    if not self.AskForSave(true, xDummy) then
        EXIT;

    frmEdZPos := TfrmEdZPos.Create(nil);
    try
        frmEdZPos.ShowModal;
    finally
        frmEdZPos.Free;
    end;
end;

procedure TSetupLayout.MoveLayout;
var
    xDummy: boolean;
    i: integer;
    xCarrier: TSetupCarrier;
    xForm: TfrmEdXY;
begin
    if (self.NoOfCarrier <= 0) then
        EXIT;
    if not self.AskForSave(true, xDummy) then
        exit;

    xForm := TfrmEdXY.Create(nil);
    try
        xForm.PosX := 0;
        xForm.PosY := 0;
        xForm.Caption := TLanguageString.Read('Change XY-Values Of Layout', 'XY-Werte des Layouts ändern');
        if xForm.ShowModal <> mrOK then
            EXIT;

        if (xForm.PosX <> 0) then
            for i := 0 to self.NoOfCarrier - 1 do
            begin
                xCarrier := (self.Carriers[i] as TSetupCarrier);
                xCarrier.SetWB_X(xCarrier.PosX + xForm.PosX);
                self.notsaved := true;
            end;

        if (xForm.PosY <> 0) then
            for i := 0 to self.NoOfCarrier - 1 do
            begin
                xCarrier := (self.Carriers[i] as TSetupCarrier);
                xCarrier.SetWB_Y(xCarrier.PosY + xForm.PosY);
                self.notsaved := true;
            end;
    finally
        xForm.Free;
    end;
end;

function TSetupLayout.DoCreateTipsetDevice: TTipsetDevice;
begin
    result := TSetupTipsetDevice.Create();
end;

function TSetupLayout.DoCreateWorkspace: TWorkspace;
begin
    result := TSetupWorkspace.Create();
end;

function TSetupLayout.DoCreateCarrier: TCarrier;
begin
    result := TSetupCarrier.Create();
end;

function TSetupLayout.DoCreateRack: TRack;
begin
    result := TSetupRack.Create();
end;

function TSetupLayout.DoCreateLayoutLink(): TLayoutLink;
begin
    result := TSetupLayoutLink.Create();
end;

function TSetupLayout.GetNotSaved: boolean;
begin
    result := TLayoutEditingFunctionProvider.Instance.NotSaved;
end;

procedure TSetupLayout.SetNotSaved(const Value: boolean);
begin
    TLayoutEditingFunctionProvider.Instance.NotSaved := Value;
end;

procedure TSetupLayout.DeleteCarrier(aCarrier: TCarrier);
var
    x: integer;
    xSlot: TCarrierSlot;
    xRack: TSetupRack;
begin
    for x := 1 to aCarrier.SlotCount do
    begin
        xSlot := aCarrier.GetSlotBySlotNr(x);
        if not Assigned(xSlot.Rack) then
            CONTINUE;
        xRack := (xSlot.Rack as TSetupRack);
        if not xRack.Deleted then
        begin
            xRack.DeleteRack;
        end;
    end;
    // aCarrier.Deleted := true;
    aCarrier.Visible := false;
    self.RemoveCarrier(aCarrier.Name);
    self.NotSaved := true;
    self.RefreshScene();
end;

procedure TSetupLayout.DeleteRack(aRack: TRack);
begin
    aRack.Visible := false;
    self.TakeRack(aRack);
    self.RemoveRack(aRack.Name);
    self.NotSaved := true;
    self.RefreshScene();
end;

procedure TSetupLayout.DeleteWorkspace(aWorkspace: TWorkspace);
var
    x: integer;
    xCarrier: TSetupCarrier;
begin
    for x := 0 to self.NoOfCarrier - 1 do
    begin
        xCarrier := self.Carriers[x] as TSetupCarrier;
        if xCarrier.Workspace = aWorkspace then
        begin
            xCarrier.Delete();
        end;
    end;
    aWorkspace.Visible := false;
    self.RemoveWorkspace(aWorkspace.Name);
    self.NotSaved := true;
    self.RefreshScene();
end;

{ TNewLayoutPage }

procedure TNewLayoutPage.cbTipTypeOnChange(Sender: TObject);
begin
    FTipTypeNameEdit.Text := cbTipType.Items[cbTipType.ItemIndex];
end;

procedure TNewLayoutPage.cbWorkspaceOnChange(Sender: TObject);
begin
    FWorkspaceNameEdit.Text := cbWorkspace.Items[cbWorkspace.ItemIndex];
    // cbSection.Items[cbSection.ItemIndex]
end;

function TNewLayoutPage.CheckBeforeNext: boolean;
var
    xTipType: TTipType;
begin
    result := false;

    // check if workspace name is empty
    if (FWorkspaceNameEdit.Text = '') then
    begin
        TDialogUtils.MessageBox(TLanguageString.Read('The workspace name can not be empty.',
            'Der Workspacename darf nicht leer sein.'), TLanguageString.Read('Workspace name',
            'Workspacename'), 64);
        FActiveControl := FWorkspaceNameEdit;
        EXIT;
    end;

    // check if workspace name exists
    if not(TWorkspaceDataAdaptor.InstNameExists(FWorkspaceNameEdit.Text)) then
    begin
        TDialogUtils.MessageBox(TLanguageString.Read('The workspace does not exist.',
            'Der Workspace existiert nicht.'), TLanguageString.Read('Workspace name', 'Workspacename'), 64);
        FActiveControl := FWorkspaceNameEdit;
        EXIT;
    end;

    // check if tiptype name is empty
    if (FTipTypeNameEdit.Text = '') then
    begin
        TDialogUtils.MessageBox(TLanguageString.Read('The tiptype name can not be empty.',
            'Der Tiptyp darf nicht leer sein.'), TLanguageString.Read('Tiptype name', 'Tiptypname'), 64);
        FActiveControl := FTipTypeNameEdit;
        EXIT;
    end;

    // check if tiptype name exists
    if not(TTipTypeDataAdaptor.TipTypeExists(FTipTypeNameEdit.Text, xTipType)) then
    begin
        TDialogUtils.MessageBox(TLanguageString.Read('The tiptype does not exist.',
            'Der Tiptyp existiert nicht.'), TLanguageString.Read('Workspace name', 'Workspacename'), 64);
        FActiveControl := FTipTypeNameEdit;
        EXIT;
    end;

    result := true;
end;

constructor TNewLayoutPage.Create(aOwner: TComponent; aLayoutName: string);
begin
    inherited Create(aOwner);

    AddLabel(TLanguageString.Read('Layout name:', 'Layoutname:'), INT_WIZARD_LEFT_1);
    FLayoutNameEdit := self.AddEdit(INT_WIZARD_LEFT_EDIT, INT_WIZARD_WIDTH_EDIT);
    FLayoutNameEdit.Text := aLayoutName;
    FLayoutNameEdit.Enabled := false;
    FLayoutNameEdit.MaxLength := TLayoutDataAdaptor.MaxRackNameLen;
    AddDistance(wzdXLarge);

    AddLabel(TLanguageString.Read('Workspace name:', 'Workspacename:'), INT_WIZARD_LEFT_1);
    cbWorkspace := AddComboBox(INT_WIZARD_LEFT_EDIT, INT_WIZARD_WIDTH_EDIT);
    cbWorkspace.Style := csDropDownList;
    cbWorkspace.OnChange := cbWorkspaceOnChange;
    FWorkspaceNameEdit := self.AddEdit(INT_WIZARD_LEFT_EDIT, INT_WIZARD_WIDTH_EDIT - 18);
    FWorkspaceNameEdit.MaxLength := TLayoutDataAdaptor.MaxRackNameLen;
    AddDistance(wzdMedium);

    AddLabel(TLanguageString.Read('Default tiptype:', 'Standard Tiptyp:'), INT_WIZARD_LEFT_1);
    cbTipType := AddComboBox(INT_WIZARD_LEFT_EDIT, INT_WIZARD_WIDTH_EDIT);
    cbTipType.Style := csDropDownList;
    cbTipType.OnChange := cbTipTypeOnChange;
    FTipTypeNameEdit := self.AddEdit(INT_WIZARD_LEFT_EDIT, INT_WIZARD_WIDTH_EDIT - 18);
    FTipTypeNameEdit.MaxLength := TLayoutDataAdaptor.MaxRackNameLen;
    AddDistance(wzdXLarge);

    FActiveControl := FWorkspaceNameEdit
end;

function TNewLayoutPage.GetActiveControl: TWinControl;
begin
    result := FActiveControl;
end;

function TNewLayoutPage.GetHeaderText: TStringArray;
begin
    result := self.GetSingleTextAsHeader(TLanguageString.
        Read('Declare used workspace and default tiptype for new layout.',
        'Workspace und Standard-Tiptypen für neues Layout deklarieren.'));
end;

function TNewLayoutPage.GetHeaderTitle: string;
begin
    result := TLanguageString.Read('New Layout', 'Neues Layout');
end;

function TNewLayoutPage.GetLayoutName: string;
begin
    result := self.FLayoutNameEdit.Text;
end;

function TNewLayoutPage.GetTipTypeName: string;
begin
    result := self.FTipTypeNameEdit.Text;
end;

function TNewLayoutPage.GetWorkspaceName: string;
begin
    result := self.FWorkspaceNameEdit.Text;
end;

procedure TNewLayoutPage.SetUp(aPreviousPage: TWizardPage);
var
    xWorkspaceNames, xTipTypeNames: TStringArray;
    xIniAccess: IWinlissyIniAccess;
begin
    inherited SetUp(aPreviousPage);

    xIniAccess := gCommonDll.CreateAppIni;

    xWorkspaceNames := TWorkspaceDataAdaptor.InstReadAllNames;
    FWorkspaceNameEdit.Text := xWorkspaceNames[0];
    FWorkspaceNameEdit.Enabled := not TAppSettings.IsOneWorkspaceMode;
    cbWorkspace.Enabled := FWorkspaceNameEdit.Enabled;

    xTipTypeNames := TTipTypeDataAdaptor.InstReadAllNames;
    FTipTypeNameEdit.Text := xTipTypeNames[0];
    FTipTypeNameEdit.Enabled := not TAppSettings.IsOneTipTypeMode;
    cbTipType.Enabled := FTipTypeNameEdit.Enabled;

    TControlUtils.AddValuesToComboBox(xWorkspaceNames, cbWorkspace, true);
    TControlUtils.AddValuesToComboBox(xTipTypeNames, cbTipType, true);

    if FWorkspaceNameEdit.Enabled then
        FActiveControl := FWorkspaceNameEdit
    else if FTipTypeNameEdit.Enabled then
        FActiveControl := FTipTypeNameEdit
    else
        FActiveControl := nil;
end;

procedure TSetupLayout.SetDefaultTipset(aTipType: string);
var
    x, y: integer;
    xTipset: TTipsetDevice;
begin

    for x := 0 to self.fTipsets.Count - 1 do
    begin
        xTipset := fTipsets[x];
        for y := 0 to xTipset.Tips.Count - 1 do
        begin
            if xTipSet.Tips[y].TypeName = '' then
            begin
                xTipSet.Tips[y].TypeName := aTipType;
            end;
        end;
        // fTipsets[ x ] := xTipset;
        (xTipset as TSetupTipsetDevice).SaveTips(fLayoutName);
    end;
end;

{ TSetupStackerLevelSelectorGraphics }

procedure TSetupStackerLevelSelectorGraphics.StackerGridKeyDown(aSender: TObject; var aKey: Word;
    aShift: TShiftState);
var
    xIndex: integer;
    xRackName: string;
    xRack: TSetupRack;
begin
    inherited;

    if (aKey = VK_DELETE) then
    begin
        if not ChooseSelected() then
            EXIT;

        xIndex := TLayoutEditingFunctionProvider.Instance.CurrentSetupLayout.R(self.ChosenRack);
        if (xIndex < 0) then
            EXIT;

        xRack := TLayoutEditingFunctionProvider.Instance.CurrentSetupLayout.Racks[xIndex] as TSetupRack;
        if Assigned(xRack) then
        begin
            xRack.DeleteRack;
            self.FillGrid();
        end;
    end;

    if (aKey = VK_RETURN) and (ssAlt in aShift) then
    begin
        if TLayoutEditingFunctionProvider.Instance.MainNodeIsRackNode(xRackName) and
            TRackDataAdaptor.InstNameExists(xRackName) then
        begin

            if not ChooseSelected() then
                EXIT;

            // add rack to workbench
            TLayoutEditingFunctionProvider.Instance.CurrentSetupLayout.AddNewRack(xRackName, self.StackerName,
                self.ChosenSlot, (ssShift in aShift));
            self.FillGrid();

        end
        else
            Showmessage(TLanguageString.Read('Select a rack in selection window!',
                'Bitte wählen Sie ein Rack aus dem Auswahl-Fenster!'));
    end;
end;

procedure TSetupStackerLevelSelectorGraphics.StackerGridMouseDown(aSender: TObject; aButton: TMouseButton;
    aShift: TShiftState; aX, aY: Integer);
var
    xRackName: string;
begin
    // Racks schnell platzieren
    if (ssAlt in aShift) and (aButton = mbLeft) then
    begin

        if TLayoutEditingFunctionProvider.Instance.MainNodeIsRackNode(xRackName) and
            TRackDataAdaptor.InstNameExists(xRackName) then
        begin

            if not ChooseByMousePos(aX, aY) then
                EXIT;

            // add rack to workbench
            TLayoutEditingFunctionProvider.Instance.CurrentSetupLayout.AddNewRack(xRackName, self.StackerName,
                self.ChosenSlot, (ssShift in aShift));
            self.FillGrid();

        end
        else
            Showmessage(TLanguageString.Read('Select a rack in selection window!',
                'Bitte wählen Sie ein Rack aus dem Auswahl-Fenster!'));
    end;
end;


end.
