{ ----------------------------------------------------------------------------------------------------------------------
  Copyright  2008 Zinsser Analytic GmbH - All rights reserved.
  Author       : Wolfgang Lyncke (wl)
  Description  :
  ----------------------------------------------------------------------------------------------------------------------
  Revision History:
  date     op  method                       track-no improvement/change
  -------- --  ---------------------------  -------- -------------------------------------------------------------------
  14.04.08 wl  gmInputBox,gmInputQuery      TN4060   from UtilityWin32
  15.04.08 wl  TDialogUtils.AppMessageBox   TN4060   from UtilityWin32
  15.04.08 wl  TDialogUtils.AskForNewName   TN4060   from UtilLib
  15.04.08 wl  TControlUtils                TN4060   all methods from UtilLib
  06.05.08 wl  TControlUtils                         new useful methods
  13.01.09 wl  TDialogUtils.InputQuery      TN4312   Inhalt wieder durch Dialogs.InputQuery ersetzt
  17.04.09 pk  uAppHInstance                TN4532   New: store the HInstance value of the main app
  17.04.09 pk  LoadScreenCursor             TN4532   New: Load a cursor from the resources
  06.08.09 wl  MessageBox                   TN4702   = AppMessageBox
  06.08.09 wl  AppMessageBox                TN4702   entfernt
  11.08.09 wl  SelectItemBox                TN4702   Aufruf von GetNames
  13.08.09 wl  TDialogUtils.SelectItemBox   TN4702   Rückgabewert ist jetzt definiert
  04.11.09 pk                               TN4843   Replace TObjectList, TStringList, TStrings with Generics Lists and TStringArray
  11.11.09 pk  AddTreeNodeListToTreeNodes   TN4856   New
  14.11.09 wl  MessageBox                   TN4869   3 getrennte Parameter für Buttons, Icon & DefaultButton
  14.11.09 wl  AskForNewName                TN4869   Bugfix: Benutzt jetzt TTypeSafeFormat.Format
  09.02.10 pk  SelectDirectory              TN4973   wrapper for Dialogs.SelectDirectory. Also added compiler directive WARN UNIT_PLATFORM OFF
  17.03.10 wl                               TN5031   uses GenericTree
  09.04.10 wl  InputQueryNew,InputBoxNew    TN5044   bentuzt neuen InputDialog
  09.04.10 wl  EnableBorderIcons,RemoveBorderIcons  TN5044  --> UfrmScheduleChartOptions
  20.05.10 wl  InputQuery,InputBox          TN5117   benutzt jetzt immer neue Box
  20.05.10 wl  TControlUtils                TN5117   --> ControlUtils
  17.08.10 wl  MessageBox                   TN5112   DefaultWert auch für Caption
  02.02.11 wl  SelectItemBox                TN5466   Eine Funktion mit Default-Wert (einfacher)
  26.11.11 wl  BitmapMessageBox             TN5750   neu: MessageBox mit Bitmap
  26.03.12 wl                               TN5844   WARN UNIT_PLATFORM OFF
  09.05.12 wl  SelectItemBox                TN5890   neuer Parameter aCheckIfItemExists
  27.03.13 wl                               TN6045   uses geändert
  ---------------------------------------------------------------------------------------------------------------------- }

unit DialogUtils;


interface


uses
    Forms,
    Classes,
    Grids,
    Controls,
    StdCtrls,
    ComCtrls,
    GeneralTypes,
    GenericTree;

type
    TDialogUtils = class
    private
        class var uAppHInstance: LongWord;
    public
        class function MessageBox(const aText: string; const aCaption: string = ''; aButtons: integer = 0;
            aIcon: integer = 0; aDefaultButton: Integer = 0): Integer;
        class function BitmapMessageBox(const aBitmapName: string; const aText: string;
            const aCaption: string = ''; aButtons: integer = 0): Integer;
        class function InputQuery(const ACaption, APrompt: string; var Value: string): Boolean; overload;
        class function InputQuery(const ACaption, APrompt: string; const aButtonText: string;
            var Value: string): Boolean; overload;
        class function InputBox(const ACaption, APrompt, ADefault: string): string;
        class function SelectItemBox(const aItems: TStringArray; const aText, aCaption: string;
            const aButtonText: string = ''; aCheckIfItemExists: boolean = true): string;
        class function AskForNewName(var aNewDefName: string; aCaption, aLabel, aDuplicateError: string;
            const aExistingItems: TStringArray): boolean;
        class procedure LoadScreenCursor(const aStoreIndex: integer; const aCursorName: string);
        class procedure SetAppHInstance(const aAppHInstance: LongWord);
        class function GetAppHInstance(): LongWord;
        class function SelectDirectory(const aCaption: string; const aRoot: string;
            var vDirectory: string): boolean;
    end;


implementation


uses
{$WARN UNIT_PLATFORM OFF}
    FileCtrl,
{$WARN UNIT_PLATFORM ON}
    Windows,
    Dialogs,
    Generics.Collections,
    Graphics,
    SysUtils,
    GetNames,
    BitmapMessageBox,
    InputDialog,
    UtilLib;

{ TDialogUtils }

class function TDialogUtils.MessageBox(const aText: string; const aCaption: string; aButtons: integer;
    aIcon: integer; aDefaultButton: Integer): Integer;
begin
    result := Application.MessageBox(PChar(aText), PChar(aCaption), aButtons + aIcon + aDefaultButton);
end;

class function TDialogUtils.BitmapMessageBox(const aBitmapName, aText, aCaption: string;
    aButtons: integer): Integer;
begin
    result := TfrmBitmapMessageBox.MessageBox(aBitmapName, aText, aCaption, aButtons);
end;

class function TDialogUtils.InputQuery(const ACaption, APrompt: string; var Value: string): Boolean;
begin
    result := TDialogUtils.InputQuery(APrompt, ACaption, TLanguageString.Read('Continue',
        'Fortsetzen'), Value);
end;

class function TDialogUtils.InputQuery(const ACaption, APrompt, aButtonText: string;
    var Value: string): Boolean;
begin
    result := TfrmInputDialog.ShowDialog(ACaption, APrompt, aButtonText, Value);
end;

class function TDialogUtils.InputBox(const ACaption, APrompt, ADefault: string): string;
begin
    Result := ADefault;
    TDialogUtils.InputQuery(ACaption, APrompt, Result);
end;

class function TDialogUtils.AskForNewName(var aNewDefName: string; aCaption, aLabel, aDuplicateError: string;
    const aExistingItems: TStringArray): boolean;
var
    xContinue: boolean;
    xIndex: integer;
    xList: TList<string>;
begin
    xList := TList<string>.Create();
    try
        xList.AddRange(aExistingItems);
        xList.Sort;

        result := false;
        while true do
        begin
            xContinue := self.InputQuery(aCaption, aLabel, aNewDefName);
            if not xContinue then
                Exit;
            if not Assigned(aExistingItems) then
                Break;

            xIndex := xList.IndexOf(aNewDefName);
            if xIndex = -1 then
                Break; // Name does not already exist, so proceed
            ShowMessage(TTypeSafeFormat.Format(aDuplicateError, [aNewDefName]));
        end;
    finally
        FreeAndNil(xList);

    end;
    result := true;
end;

class procedure TDialogUtils.LoadScreenCursor(const aStoreIndex: integer; const aCursorName: string);
begin
    Screen.Cursors[aStoreIndex] := LoadCursor(uAppHInstance, MAKEINTRESOURCE(aCursorName));
end;

class function TDialogUtils.GetAppHInstance: LongWord;
begin
    result := uAppHInstance;
end;

class procedure TDialogUtils.SetAppHInstance(const aAppHInstance: LongWord);
begin
    uAppHInstance := aAppHInstance;
end;

class function TDialogUtils.SelectDirectory(const aCaption, aRoot: string; var vDirectory: string): boolean;
begin
    result := FileCtrl.SelectDirectory(aCaption, aRoot, vDirectory);
end;

class function TDialogUtils.SelectItemBox(const aItems: TStringArray;
    const aText, aCaption, aButtonText: string; aCheckIfItemExists: boolean): string;
var
    xOKText: string;
begin
    if (xOKText = '') then
        xOKText := TLanguageString.Read('Continue', 'Fortsetzen')
    else
        xOKText := aButtonText;

    result := TGetNameDlg.ShowDialog(aItems, aText, aCaption, xOKText, aCheckIfItemExists);
end;


end.
