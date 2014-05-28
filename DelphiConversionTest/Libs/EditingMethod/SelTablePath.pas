{ --------------------------------------------------------------------------------------------------
  Copyright © 2004 Zinsser Analytic GmbH - All rights reserved.
  Author       : Payman Kamali (pk)
  Description  : A GUI for selecting the path of a table.
  --------------------------------------------------------------------------------------------------
  Revision History:
  date     op  method                       track-no improvement/change
  -------- --  ---------------------------  -------- -----------------------------------------------
  27.04.04 pk                               TN1880   initial version
  04.05.04 pk                               TN1880   resources
  19.12.06 wl                               TN3409   Kann nur noch von SystemAdmin editiert werden
  09.11.07 pk                               TN3922   uses ImportDataProvider
  09.01.08 wl                               TN3972   uses geändert
  22.01.08 wl                               TN3972   uses DatabaseProvider
  04.04.08 wl                               TN4058    uses geändert
  14.04.08 wl                               TN4060   uses DialogUtils
  19.08.09 wl  fStringLoader                TN4702   fStringLoader lädt Strings für Dialog-Elemente
  04.11.09 pk                               TN4843   Replace TObjectList, TStringList, TStrings with Generics Lists and TStringArray
  20.11.09 pk  TFrmSelTablePath.FormCreate  TN4894   add ofNoChangeDir to opdlgFilePath.Options so that system path does not get changed
  16.12.09 pk                               TN4933   TDatabaseProvider functions no longer class functions
  04.02.10 pk                               TN4972   Changes for Restart
  20.05.10 wl                                TN5117   neue Schriftart "Segoe UI", Schriftgröße 9
  17.06.10 pk                               TN5152.1  uses DataProviderFactory
  04.08.10 pk                               TN5218   Changes for Plugin database packages
  07.04.11 wl                               TN5541   komplett überarbeitet, denn reine Tabellenpfade machen keinen Sinn mehr
  -------------------------------------------------------------------------------------------------- }

unit SelTablePath;


interface


uses
    Windows,
    SysUtils,
    Classes,
    Controls,
    Forms,
    StdCtrls,
    ExtCtrls,
    Menus,
    StringLoader;

type
    TSelTablePathStringLoader = class(TTextListStringLoader)
    protected
        procedure AddAllItems(); override;
    end;

    TFrmSelTablePath = class(TForm)
        Panel1: TPanel;
        Button1: TButton;
        Button2: TButton;
        Bevel1: TBevel;
        cmbTableName: TComboBox;
        Label4: TLabel;
        cmbAlias: TComboBox;
        RefreshAliasNames1: TButton;
        Label2: TLabel;
        procedure FormCreate(Sender: TObject);
        procedure cmbAliasChange(Sender: TObject);
        procedure RefreshAliasNames1Click(Sender: TObject);
        procedure FormDestroy(Sender: TObject);
    private
        fStringLoader: TSelTablePathStringLoader;
        procedure SelectAlias(aAliasName: string);
        procedure SelectTable(aTableName: string);
        procedure GetAllTableNames();
        procedure GetAllAliasNames(aMustRefresh: boolean);
        procedure AliasChanged();
    public
        class function SelectTablePathModal(var vTablePath: string): TModalResult;
    end;


implementation


{$R *.DFM}

uses
    ControlUtils,
    AppSettings,
    GeneralTypes,
    ImportClasses,
    DataProviderFactory,
    DatabaseProvider,
    DataProvider,
    SQLParser;

{ TSelTablePathStringLoader }

procedure TSelTablePathStringLoader.AddAllItems;
begin
    AddSingle(510, '&OK', '&OK');
    AddSingle(520, '&Cancel', '&Abbrechen');
    AddSingle(9690, 'Define Table Path', 'Tabellepfad Definieren');
    AddSingle(14230, 'Table Name', 'Tabellename');
    AddSingle(14240, 'Alias Name', 'Aliasname');
end;

{ TFrmSelTablePath }

procedure TFrmSelTablePath.FormCreate(Sender: TObject);
begin
    TControlUtils.ResetFontForWinXP(self);
    fStringLoader := TSelTablePathStringLoader.Create;
    fStringLoader.LoadLanguage(self);

    self.GetAllAliasNames(true);
end;

procedure TFrmSelTablePath.FormDestroy(Sender: TObject);
begin
    fStringLoader.Free;
end;

class function TFrmSelTablePath.SelectTablePathModal(var vTablePath: string): TModalResult;
var
    xFrmSelTablePath: TFrmSelTablePath;
    xAlias, xTableName: string;
begin
    xFrmSelTablePath := TFrmSelTablePath.Create(nil);
    try
        if TAliasTablePath.ParseAliasTablePath(vTablePath, xAlias, xTableName) then
        begin
            xFrmSelTablePath.SelectAlias(xAlias);
            xFrmSelTablePath.SelectTable(xTableName);
        end
        else
        begin
            // select Default Alias
            xFrmSelTablePath.SelectAlias(TAppSettings.Alias);
            xFrmSelTablePath.SelectTable('');
        end;

        result := xFrmSelTablePath.ShowModal;
        if result = mrCancel then
            Exit;

        vTablePath := TAliasTablePath.MakeAliasTablePath(dbStandard, xFrmSelTablePath.cmbAlias.Text,
            xFrmSelTablePath.cmbTableName.Text, false);
    finally
        xFrmSelTablePath.Free;
    end;
end;

procedure TFrmSelTablePath.SelectAlias(aAliasName: string);
begin
    if (self.cmbAlias.Text = aAliasName) then
        Exit;
    TControlUtils.SelectComboBoxEntry(self.cmbAlias, aAliasName);
    AliasChanged();
end;

procedure TFrmSelTablePath.SelectTable(aTableName: string);
begin
    TControlUtils.SelectComboBoxEntry(self.cmbTableName, aTableName);
end;

procedure TFrmSelTablePath.AliasChanged();
begin
    GetAllTableNames();
end;

procedure TFrmSelTablePath.cmbAliasChange(Sender: TObject);
begin
    AliasChanged();
end;

procedure TFrmSelTablePath.GetAllAliasNames(aMustRefresh: boolean);
var
    xCurrentAlias, xCurrentTable: string;
    xNames: TStringArray;
begin
    if (not aMustRefresh) and (self.cmbAlias.Items.Count > 0) then
        Exit;
    xCurrentAlias := self.cmbAlias.Text;
    xCurrentTable := self.cmbTableName.Text;
    xNames := TDataProviderFactory.Instance.GetAllAliasNames();
    TControlUtils.AddValuesToComboBox(xNames, self.cmbAlias, true);
    SelectAlias(xCurrentAlias);
    SelectTable(xCurrentTable);
end;

procedure TFrmSelTablePath.GetAllTableNames();
var
    xCurrentTable: string;
    xNames: TStringArray;
    xDatabaseProvider: TDatabaseProvider;
    xAlias: string;
begin
    xAlias := cmbAlias.Text;
    if xAlias = '' then
        EXIT;

    xCurrentTable := self.cmbTableName.Text;

    xDatabaseProvider := TDataProviderFactory.Instance.CreateDatabaseProvider(xAlias);
    try
        xNames := xDatabaseProvider.GetAllTableNames();
    finally
        FreeAndNil(xDatabaseProvider);
    end;

    TControlUtils.AddValuesToComboBox(xNames, cmbTableName, true);
    SelectTable(xCurrentTable);
end;

procedure TFrmSelTablePath.RefreshAliasNames1Click(Sender: TObject);
begin
    GetAllAliasNames(true);
end;


end.
