{ --------------------------------------------------------------------------------------------------
  Copyright © 2005 Zinsser Analytic GmbH - All rights reserved.
  Author       : Wolfgang Lyncke (wl)
  Description  : Advanced Data Adaptor for the Method Table
  General functions requiring non-common units should be programmed here
  --------------------------------------------------------------------------------------------------
  Revision History:
  date     op  method                           track-no improvement/change
  -------- --  ------------------------------   -------- -----------------------------------------------
  10.10.05 wl  ReplaceCommentsOfAddedMethods    TN2584    Ändert die Kommentare von ADDM-Actions, die eine bestimmte Methode aufrufen
  10.10.05 wl  ReadAllMethodWithThisAddedMethod TN2584    wird noch nicht benutzt
  07.07.06 wl  ReplaceCommentsOfAddedMethods    TN3189    statt REMARK direkt zu lesen, wird jetzt ReadRemark() benutzt
  07.08.07 wl                                   TN3811.3  benutzt SelectandOpenAll statt SelectAll & Open
  03.09.07 pk  CheckNewName,etc                 TN3847    Application-specific funcs moved here from TMethodDataAdaptor
  02.10.07 wl                                   TN3811.5 benutzt self.Query statt fQuery
  09.11.07 pk                                   TN3922    Dataset changed to DataProvider
  09.01.08 wl  GetAddedMethodNameFromCurrentRec TN3972    benutzt TRunStepBuilderTypeDictionary.GetAddedMethodNameFromAction
  23.09.08 wl                                   TN4236    Verweis auf MethodFieldnames entfernt
  06.10.08 pk  GetAddedMethodNameFromCurrentRec TN4258    GetAddedMethodNameFromAction no longer class function
  13.07.09 pk  NameAllowedAsFileName            TN4585.4  RunAliasPath changed to RunPath
  04.11.09 pk                                   TN4843    Replace TObjectList, TStringList, TStrings with Generics Lists and TStringArray
  19.11.09 pk                                   TN4795    NameAllowedAsFileName, use AliasPath instead of RunPath
  04.08.10 pk                                   TN5218    Changes for Plugin database packages
  07.08.12 wl  SaveTemporaryMethod              TN5946   erzeugt temporäre Methode, die sofort ausgeführt werden kann
  07.08.12 wl  IsSubMethodLayoutNameAllowed     TN5946   entfernt
  05.04.13 ts  SaveTemporaryMethod              TN6123   TMethodSettings.SaveDeleteRunDataOption = false, damit POSINFO nicht gelöscht wird
  10.04.13 wl                                   TN6045   uses geändert
  20.06.13 ts                                   TN6146   cTemporaryMethodName zu LayoutDataAdaptorExt verschoben
  30.08.13 wl  SaveTemporaryMethod              TN6236   verwendet TMethodSettingsDataAdaptor
  -------------------------------------------------------------------------------------------------- }

unit MethodDataAdaptorExt;


interface


uses
    Generics.Collections,
    DataProvider,
    MethodDataAdaptor;

type
    TMethodDataAdaptorExt = class(TMethodDataAdaptor)
    private
        class function NameReservedByUser(aNewName: string): boolean;
        class function NameIsTooLong(aNewName: string): boolean;
        class function NameAllowedAsFileName(aNewName: string): boolean;
        class function NameContainsForbiddenCharacters(aNewName: string; out oCharacter: string): boolean;
        class function NameFirstCharContainsForbiddenCharacters(aNewName: string;
            out oCharacter: string): boolean;
        class function GetAddedMethodNameFromCurrentRec(aDataset: TDataProvider): string;
    public
        procedure ReplaceCommentsOfAddedMethods(const aAddMethodName, aNewComments, aOldComments: string);
        procedure ReadAllMethodWithThisAddedMethod(aResultList: TList<string>; const aAddMethodName: string);
        class function CheckNewName(aName: string): boolean;
        class procedure SaveTemporaryMethod(const aMethodRecs: TArray<TMethodRec>; const aLayoutName: string);
    end;


implementation


uses
    Windows,
    SysUtils,
    Forms,
    AppSettings,
    CommonTypes,
    DataAdaptor,
    RunStepBuilderTypeDictionary,
    FileUtilities,
    MethodSettingsDataAdaptor,
    LayoutDataAdaptorExt;

{ TMethodDataAdaptorExt }

class function TMethodDataAdaptorExt.NameReservedByUser(aNewName: string): boolean;
var
    xIdentNames: TArray<string>;
    x: integer;
    NewSC: string;
    xIniAccess: IWinlissyIniAccess;
    xTempStr: string;
begin
    xIniAccess := gCommonDll.CreateAppIni;

    result := false;
    // Abfrage auf reservierte Namen, die in sampler.ini stehen
    xIdentNames := xIniAccess.ReadAllowedSection('ReservedNames', 'N');

    for x := 0 to Length(xIdentNames) - 1 do
    begin
        xTempStr := xIniAccess.ReadString('ReservedNames', xIdentNames[x]);
        if (Copy(xTempStr, Length(xTempStr), 1) = '*') then
        begin

            // nicht erlaubter Namensanfang
            NewSC := Copy(aNewName, 1, Length(xTempStr) - 1) + '*';
            if (Uppercase(NewSC) = Uppercase(xTempStr)) then
                result := true;
        end
        else
        begin
            if (Uppercase(aNewName) = Uppercase(xTempStr)) then
                result := true;
        end;
    end;
end;

class function TMethodDataAdaptorExt.NameIsTooLong(aNewName: string): boolean;
var
    xDataAdaptor: TMethodDataAdaptor;
    xDataSize: integer;
begin
    result := true;
    xDataAdaptor := TMethodDataAdaptor.Create;
    xDataAdaptor.SelectAndOpenAll(true);
    try
        xDataSize := xDataAdaptor.DataProvider.FieldByName(STR_METHOD_FLD_NAME).DataSize - 1;
        if (Length(aNewName) <= xDataSize) then
            result := false;
    finally
        xDataAdaptor.Close();
    end;
    xDataAdaptor.Free;
end;

class function TMethodDataAdaptorExt.NameAllowedAsFileName(aNewName: string): boolean;
var
    TmpFile: Textfile;
    xFileName: string;
begin
    result := true;
    if (result = false) then
        EXIT;
    try

        TFileUtilities.ForceDirectories(TAppSettings.RunPath);
        xFileName := TAppSettings.RunPath + aNewName + '.TMP';
        AssignFile(Tmpfile, xFileName);
        Rewrite(TmpFile);
        Closefile(TmpFile);
        result := FileExists(xFileName);
        if not Deletefile(xFileName) then
            result := false;
        if FileExists(xFileName) then
            result := false;
    except
        result := false;
    end;
end;

const
    CHRS_METHODNAME_ALLOWED_FIRSTCHAR = ['A' .. 'Z', 'a' .. 'z', '0' .. '9', 'ä', 'Ä', 'ö', 'Ö', 'ü', 'Ü'];
    CHRS_METHODNAME_ALLOWED = CHRS_METHODNAME_ALLOWED_FIRSTCHAR + ['_', '-', ' '];

class function TMethodDataAdaptorExt.NameFirstCharContainsForbiddenCharacters(aNewName: string;
    out oCharacter: string): boolean;
begin
    result := true;
    if not CharInSet(aNewName[1], CHRS_METHODNAME_ALLOWED_FIRSTCHAR) then
    begin
        result := false;
        oCharacter := aNewName[1];
        EXIT;
    end;
end;

class function TMethodDataAdaptorExt.NameContainsForbiddenCharacters(aNewName: string;
    out oCharacter: string): boolean;
var
    x: integer;
begin
    result := true;
    for x := 1 to Length(aNewName) do
    begin
        if not CharInSet(aNewName[x], CHRS_METHODNAME_ALLOWED) then
        begin
            result := false;
            oCharacter := aNewName[x];
            EXIT;
        end;
    end;
end;

// --------------------------------------------------------------------------------------------------
class function TMethodDataAdaptorExt.CheckNewName(aName: string): boolean;
// --------------------------------------------------------------------------------------------------
// Prüfen ob Methodenname zulässig ist
// --------------------------------------------------------------------------------------------------
var
    xCharacter: string;
begin
    result := true;
    try
        if (aName = '') then
            raise Exception.Create('Blank Name');

        if NameReservedByUser(aName) then
            raise Exception.Create('Reserved Name');

        if NameIsTooLong(aName) then
            raise Exception.Create('Name is too long');

        if not NameContainsForbiddenCharacters(aName, xCharacter) then
            raise Exception.Create('Character ' + xCharacter + ' is not allowed');

        if not NameFirstCharContainsForbiddenCharacters(aName, xCharacter) then
            raise Exception.Create('Character ' + xCharacter + ' is not allowed as first character');

        if not NameAllowedAsFileName(aName) then
            raise Exception.Create('Name is not allowed');

    except
        on E: Exception do
        begin
            Application.MessageBox(PChar('Name ' + aName + ': ' + E.Message), '', MB_ICONSTOP + MB_OK);
            result := false;
        end;
    end;
end;

class procedure TMethodDataAdaptorExt.SaveTemporaryMethod(const aMethodRecs: TArray<TMethodRec>;
    const aLayoutName: string);
var
    xMethodDA: TMethodDataAdaptor;
    xMethodSettingsDA: TMethodSettingsDataAdaptor;
    x: integer;
    xRec: TMethodSettingsRec;
begin
    xMethodDA := TMethodDataAdaptor.Create();
    try
        // delete temp. method
        xMethodDA.DeleteName(TLayoutDataAdaptorExt.cTemporaryMethodName);

        for x := 0 to high(aMethodRecs) do
            xMethodDA.WriteMethodRec(aMethodRecs[x], true { ForceAppend } );
    finally
        FreeAndNil(xMethodDA);
    end;

    xMethodSettingsDA := TMethodSettingsDataAdaptor.Create();
    try
        xRec := TMethodSettingsDataAdaptor.GetEmptyRec();
        xRec.MethodName := TLayoutDataAdaptorExt.cTemporaryMethodName;
        xRec.LayoutName := aLayoutName;
        xRec.Startable := true;
        xRec.DeleteRunDataAtStart := false;
        xMethodSettingsDA.WriteRec(xRec);
    finally
        FreeAndNil(xMethodSettingsDA);
    end;
end;

class function TMethodDataAdaptorExt.GetAddedMethodNameFromCurrentRec(aDataset: TDataProvider): string;
begin
    result := TRunStepBuilderTypeDictionary.Instance.GetAddedMethodNameFromAction
        (aDataset.FieldbyName(STR_METHOD_FLD_ACTION).AsString, aDataset.FieldbyName(STR_METHOD_FLD_AOPTIONS)
        .AsString);
end;

procedure TMethodDataAdaptorExt.ReplaceCommentsOfAddedMethods(const aAddMethodName, aNewComments,
    aOldComments: string);
var
    xAddedMethodName: string;
begin
    if (aAddMethodName = '') then
        EXIT;

    self.SelectAndOpenAll(false);
    try
        while not self.DataProvider.Eof do
        begin

            xAddedMethodName := self.GetAddedMethodNameFromCurrentRec(self.DataProvider);

            if (xAddedMethodName = aAddMethodName) then
            begin

                // wenn Kommentar = alter Methoden-Kommentar wird er ersetzt
                if (self.DataProvider.FieldbyName(STR_METHOD_FLD_COMMENT).AsString = aOldComments) then
                begin
                    self.DataProvider.Edit;
                    self.DataProvider.FieldbyName(STR_METHOD_FLD_COMMENT).AsString := aNewComments;
                    self.DataProvider.Post;
                end;
            end;

            self.DataProvider.Next;
        end;
    finally
        self.Close();
    end;
end;

procedure TMethodDataAdaptorExt.ReadAllMethodWithThisAddedMethod(aResultList: TList<string>;
    const aAddMethodName: string);
var
    xAddedMethodName, xLastMethodName: string;
begin
    ASSERT(Assigned(aResultList));
    self.SelectAndOpenAll(true);
    try
        xLastMethodName := '';

        while not self.DataProvider.Eof do
        begin

            xAddedMethodName := self.GetAddedMethodNameFromCurrentRec(self.DataProvider);

            // wenn Kommentar = alter Methoden-Kommentar wird er ersetzt
            if (xAddedMethodName = aAddMethodName) then
            begin

                xLastMethodName := self.DataProvider.FieldbyName(STR_METHOD_FLD_NAME).AsString;
                if (aResultList.Count <= 0) or (aResultList[aResultList.Count - 1] <> xLastMethodName) then
                    // keine doppelten Nennungen
                    aResultList.Add(xLastMethodName);
            end;

            self.DataProvider.Next;
        end;
    finally
        self.Close();
    end;
end;


end.
