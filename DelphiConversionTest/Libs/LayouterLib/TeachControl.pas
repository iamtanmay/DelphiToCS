unit TeachControl;

// noch nicht funktionstüchtig


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
    StdCtrls,
    ExtCtrls,
    IntfMotionDevice,
    Spin,
    Buttons,
    DBCtrls,
    Mask,
    Grids,
    DBGrids;

type
    TfrmTeachControl = class(TForm)
        Panel6: TPanel;
        DBNavigator: TDBNavigator;
        dbgNavRec: TDBGrid;
        Panel2: TPanel;
        ScrollBox: TScrollBox;
        gbDataFromMachine: TGroupBox;
        Label2: TLabel;
        Label3: TLabel;
        Label38: TLabel;
        Label39: TLabel;
        Label40: TLabel;
        Label41: TLabel;
        Label42: TLabel;
        Label43: TLabel;
        Label44: TLabel;
        lstJoints: TListBox;
        lstPositions: TListBox;
        btnReadPositionsFromMachine: TBitBtn;
        btnAssignToCurRecAsWorld: TBitBtn;
        btnAssignToNewRecAsWorld: TBitBtn;
        GroupBox22: TGroupBox;
        Label115: TLabel;
        Label116: TLabel;
        Label117: TLabel;
        dbeCurrentPosition: TDBText;
        dbeXPosition: TDBText;
        dbeZPosition: TDBText;
        Label118: TLabel;
        Label119: TLabel;
        btnGoForward: TBitBtn;
        btnGoBackward: TBitBtn;
        edtExtraX: TEdit;
        edtExtraZ: TEdit;
        btnGoForwardX: TBitBtn;
        btnGoForwardZ: TBitBtn;
        btnGoBackwardX: TBitBtn;
        btnGoBackwardZ: TBitBtn;
        GroupBox21: TGroupBox;
        fSensor1: TLabel;
        BitBtn7: TBitBtn;
        BitBtn8: TBitBtn;
        pnlSensor1State: TPanel;
        GroupBox23: TGroupBox;
        Label121: TLabel;
        Label122: TLabel;
        Label123: TLabel;
        edtXt: TEdit;
        StaticText1: TStaticText;
        edtYt: TEdit;
        StaticText2: TStaticText;
        edtZt: TEdit;
        StaticText3: TStaticText;
        BitBtn9: TBitBtn;
        rgValves: TGroupBox;
        fSensor2: TLabel;
        btnOpenValves: TBitBtn;
        btnCloseValves: TBitBtn;
        pnlSensor2State: TPanel;
        Panel5: TPanel;
        Label1: TLabel;
        Label98: TLabel;
        Label99: TLabel;
        Label100: TLabel;
        eEditNAME: TDBEdit;
        eEditSPEED: TDBEdit;
        eEditACCELERATION: TDBEdit;
        eEditDECELERATION: TDBEdit;
        DBCheckBox1: TDBCheckBox;
        Panel3: TPanel;
        spUndo: TSpeedButton;
        dbgPath: TDBGrid;
        btnMoveToCurrentRecordPosition: TBitBtn;
        btnMoveForwAlongTheWholePath: TBitBtn;
        GroupBox1: TGroupBox;
        Label12: TLabel;
        Label13: TLabel;
        btnMoveFromTo: TBitBtn;
        spFromPosition: TSpinEdit;
        spToPosition: TSpinEdit;
        btnMoveFromCurPosToEnd: TBitBtn;
        btnMoveFromCurPosToBegin: TBitBtn;
        btnMoveBackAlongTheWholePath: TBitBtn;
        cbMoveTrack: TCheckBox;
        BitBtn12: TBitBtn;
        BitBtn13: TBitBtn;
        BitBtn14: TBitBtn;
        BitBtn15: TBitBtn;
        BitBtn16: TBitBtn;
        BitBtn17: TBitBtn;
        DBNavigator1: TDBNavigator;
        dbeJ1: TDBEdit;
        procedure FormCreate(Sender: TObject);
    private
        fMotionDevice: ILocationBasedMotionDevice;
    public
        property MotionDevice: ILocationBasedMotionDevice read fMotionDevice write fMotionDevice;
    end;


implementation


{$R *.dfm}

uses
    ControlUtils;

procedure TfrmTeachControl.FormCreate(Sender: TObject);
begin
    TControlUtils.ResetFontForWinXP(self);
end;


end.
