{ --------------------------------------------------------------------------------------------------
  Ebene 2 (Sam-Interface)
  --------------------------------------------------------------------------------------------------
  Test-Fenster zum testen von DLL-Funktionen
  --------------------------------------------------------------------------------------------------
  Datum    op  function/procedure     Änderung / Neuerung
  -------- --  -------------------    --------------------------------------------------------------
  17.05.00 wl                         neue unit
  10.08.00 wl                         --> String in Ressourcen
  09.10.02 wl                               TN1293.1 divided into rcRobotics and rcCommon
  12.12.02 wl                         TN1345 uses geändert
  27.12.02 wl                         TN1293.5 uses und WinlissyIniAccess geändert
  12.03.03 wl                               TN1293.5 uses posTools
  28.03.03 wl  btnExeClick            TN1293.7 das Ergebnis wird jetzt wirklich angezeigt
  28.09.04 wl                         TN2156   The parameter edit control is bigger than before.
  28.09.04 wl  FormCreate,FormClose   TN2156   The window remembers the DLL name and function after restart
  08.11.04 wl  btnExeClick            TN2213   benutzen TDllCallExt.DirectExecute statt gmLoadEventFunction
  08.03.05 pk                         TN2337   TDLLCallExt changed to TDLLCall
  28.05.08 wl  TDllThread             TN4122   neuer Button, um Funktion im Thread zu testen!
  02.06.08 pk                         TN4122   uses DllCall instead of DLLLoading
  10.08.09 wl                         TN4702   Strings werden jetzt direkt geladen
  21.08.09 wl  fStringLoader          TN4702   fStringLoader lädt Strings für Dialog-Elemente
  26.10.09 wl                         TN4831   IConfigurationSet replaces ILocalIniFile
  09.02.10 pk                         TN4973   Thread.Resume changed to Thread.Start
  20.05.10 wl                         TN5117   neue Schriftart "Segoe UI", Schriftgröße 9
  21.06.10 wl                               TN5160   Position = poScreenCenter
  15.12.11 wl                               TN5767   Dll kann jetzt mit Interface V6 oder V8 verwendet werden
  28.09.12 wl  spMaxRetries                 TN5984   SpinEdit bestimmt AutoRetryCount
  -------------------------------------------------------------------------------------------------- }

unit DLLTest;


interface


uses
    Windows,
    Messages,
    SysUtils,
    Classes,
    Graphics,
    Controls,
    Forms,
    Dialogs,
    StdCtrls,
    StringLoader,
    ExtCtrls,
    DLLCall,
    Spin;

type
    TDLLTestStringLoader = class(TTextListStringLoader)
    protected
        procedure AddAllItems(); override;
    end;

    TfrmDLLTest = class(TForm)
        Label1: TLabel;
        Label2: TLabel;
        Label3: TLabel;
        Edit1: TEdit;
        Edit2: TEdit;
        Edit3: TEdit;
        btnExe: TButton;
        Label4: TLabel;
        Edit4: TEdit;
        btnThreadExe: TButton;
        rgVersion: TRadioGroup;
        spMaxRetries: TSpinEdit;
        Label5: TLabel;
        procedure btnExeClick(Sender: TObject);
        procedure FormCreate(Sender: TObject);
        procedure FormClose(Sender: TObject; var Action: TCloseAction);
        procedure btnThreadExeClick(Sender: TObject);
        procedure FormDestroy(Sender: TObject);
    strict private
        fStringLoader: TDLLTestStringLoader;
    private const
        STR_INISECTION_DLL = 'DllLoading';
    public
        function GetVersion: TDllInterfaceVersion;
    end;

    TDllThread = class(TThread)
    strict private
        fParam1: string;
        fParam3: string;
        fParam2: string;
        fVersion: TDllInterfaceVersion;
        fAutoRetryCount: integer;
    public
        procedure Execute; override;
        property Param1: string read fParam1 write fParam1;
        property Param2: string read fParam2 write fParam2;
        property Param3: string read fParam3 write fParam3;
        property Version: TDllInterfaceVersion read fVersion write fVersion;
        property AutoRetryCount: integer read fAutoRetryCount write fAutoRetryCount;
    end;


implementation


{$R *.DFM}

uses
    DialogUtils,
    GeneralTypes,
    ConfigurationFile,
    ErrorManager,
    AppTypes,
    AppSettings,
    CommonTypes,
    ControlUtils;

{ TDLLTestStringLoader }

procedure TDLLTestStringLoader.AddAllItems;
begin
    AddSingle(29700, 'DLL Name:', 'DLL-Name:');
    AddSingle(29710, 'Function:', 'Funktion:');
    AddSingle(29720, 'Parameter:', 'Parameter:');
    AddSingle(29730, 'Result:', 'Ergebnis:');
    AddSingle(29740, 'Execute', 'Ausführen');
    AddSingle(29760, 'Test DLL Functions', 'DLL-Funktionen testen');
end;

{ TfrmDLLTest }

procedure TfrmDLLTest.btnExeClick(Sender: TObject);
var
    xVersion: TDllInterfaceVersion;
begin
    btnExe.Enabled := false;
    Edit4.Text := '';
    gErrorManager.SetGlobalErr(ERR_NONE);

    xVersion := self.GetVersion();
    Edit4.Text := TDllCall.DirectExecute(Edit1.Text, Edit2.Text, Edit3.Text, xVersion,
        self.spMaxRetries.Value);

    if gErrorManager.IsGlobalErr() then
        TDialogUtils.MessageBox(TLanguageString.Read('Error at DLL function execution',
            'Fehler beim Ausführen der DLL-Funktion'), TLanguageString.Read('DLL Function Error',
            'DLL-Funktion Fehler'), 16);
    gErrorManager.SetGlobalErr(ERR_NONE);
    btnExe.Enabled := true;
end;

procedure TfrmDLLTest.FormCreate(Sender: TObject);
var
    xLocalIniFile: IConfigurationSet;
begin
    TControlUtils.ResetFontForWinXP(self);
    fStringLoader := TDLLTestStringLoader.Create;
    fStringLoader.LoadLanguage(self);

    xLocalIniFile := TConfigurationFile.Create(TAppsettings.LocalIniFileName);
    xLocalIniFile.Open(true);
    try
        Edit1.Text := xLocalIniFile.ReadString(STR_INISECTION_DLL, 'File', '');
        Edit2.Text := xLocalIniFile.ReadString(STR_INISECTION_DLL, 'Function', '');
        Edit3.Text := xLocalIniFile.ReadString(STR_INISECTION_DLL, 'Parameter', '');
    finally
        xLocalIniFile.Close;
    end;
end;

procedure TfrmDLLTest.FormDestroy(Sender: TObject);
begin
    fStringLoader.Free;
end;

function TfrmDLLTest.GetVersion: TDllInterfaceVersion;
begin
    if rgVersion.ItemIndex = 0 then
        EXIT(TDllInterfaceVersion.Version8)
    else
        EXIT(TDllInterfaceVersion.Version6);
end;

procedure TfrmDLLTest.FormClose(Sender: TObject; var Action: TCloseAction);
var
    xLocalIniFile: IConfigurationSet;
begin
    xLocalIniFile := TConfigurationFile.Create(TAppsettings.LocalIniFileName);
    xLocalIniFile.Open(false);
    try
        xLocalIniFile.WriteString(STR_INISECTION_DLL, 'File', Edit1.Text);
        xLocalIniFile.WriteString(STR_INISECTION_DLL, 'Function', Edit2.Text);
        xLocalIniFile.WriteString(STR_INISECTION_DLL, 'Parameter', Edit3.Text);
    finally
        xLocalIniFile.Close;
    end;
end;

procedure TfrmDLLTest.btnThreadExeClick(Sender: TObject);
var
    xThread: TDllThread;
begin
    btnExe.Enabled := false;
    Edit4.Text := '';
    gErrorManager.SetGlobalErr(ERR_NONE);

    xThread := TDllThread.Create(true);
    try
        xThread.Param1 := Edit1.Text;
        xThread.Param2 := Edit2.Text;
        xThread.Param3 := Edit3.Text;
        xThread.Version := self.GetVersion();
        xThread.AutoRetryCount := self.spMaxRetries.Value;

        xThread.Start;
        xThread.WaitFor();
        if gErrorManager.IsGlobalErr() then
            TDialogUtils.MessageBox(TLanguageString.Read('Error at DLL function execution',
                'Fehler beim Ausführen der DLL-Funktion'), TLanguageString.Read('DLL Function Error',
                'DLL-Funktion Fehler'), 16);
        gErrorManager.SetGlobalErr(ERR_NONE);
        Edit4.Text := xThread.Param3;
    finally
        xThread.Free;
    end;
    if gErrorManager.IsGlobalErr() then
        TDialogUtils.MessageBox(TLanguageString.Read('Error at DLL function execution',
            'Fehler beim Ausführen der DLL-Funktion'), TLanguageString.Read('DLL Function Error',
            'DLL-Funktion Fehler'), 16);
    gErrorManager.SetGlobalErr(ERR_NONE);
    btnExe.Enabled := true;
end;

{ TDllThread }

procedure TDllThread.Execute;
begin
    inherited;

    try
        Param3 := TDllCall.DirectExecute(fParam1, fParam2, fParam3, fVersion, fAutoRetryCount);
    except
        on E: Exception do
            Application.MessageBox('Dll Call is not thread-safe!', 'Error', 0);
    end;
end;


end.
