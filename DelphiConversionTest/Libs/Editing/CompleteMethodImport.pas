unit CompleteMethodImport;
{ --------------------------------------------------------------------------------------------------
  Copyright © 2005 Zinsser Analytic GmbH - All rights reserved.
  Author       : Wolfgang Lyncke (wl)
  Description  : Classes to convert a text file containing method steps into a method
  --------------------------------------------------------------------------------------------------
  Revision History:
  date     op  method                       track-no  improvement/change
  -------- --  -------------------------------  --------- ----------------------------------------------
  18.08.05 wl                               TN2541.0  initial version
  31.08.05 wl                               TN2541.0  Import ist jetzt möglich
  01.11.05 wl                               TN2541.0  Neue Anfordrungen
  30.11.05 wl                               TN2541.0  mit first- und last-Methode
  21.12.05 wl                               TN2541.0  Layoutname kann unter "layout:" abgelegt werden
  05.01.06 wl  CreateMethodStepAndReplaceOptions      TN2541.0  'runtime:' kann anders definiert sein, muß aber nicht
  12.01.06 wl                                         TN2541.0  neu: _|ImportFile| ist der Name der importierten Datei
  17.01.06 wl                                         TN2541.0  neu: _|Step| ist der Name des aktuellen Schrittes
  20.01.06 pk  CreateMethodStepWithImportDefs         TN2891    call CreateMethodStepWithDefDataLink instead of CreateMethodStep
  26.01.06 wl                                         TN2541.0  Priority = 1
  10.02.06 wl                                         TN2541.0  neue Anpassungen
  18.04.06 wl  TMethodStep.AsMethodRec                TN3025    Inactive komplett entfernt
  04.05.06 wl                                         TN3080    neu: SchedMin, SchedMax, ResID
  14.11.06 wl  TCompleteMethodImportFile.AddNextStepOptions  TN3405   die Options des nächsten Schrittes werden mit dem Zusatz Next. hinzugefügt
  09.01.08 wl                                         TN3972    benutzt gRunStepBuilderTypeDictionary
  02.09.08 pk                                         TN4215    reference to gRunStepBuilderTypeDictionary changed
  26.10.09 wl  AddNextStepOptions                     TN4831   IConfigurationSet replaces TIniFile
  04.11.09 pk                               	     TN4843   Replace TObjectList, TStringList, TStrings with Generics Lists and TStringArray
  13.04.10 wl                                     TN5044   uses StringUtilities
  07.05.10 wl                                     TN5052   Scheduler-Optionen entfernt
  23.09.10 pk  CreateMethodStepWithImportDefs     TN5089   Changes in TRunStepBuilderTypeDictionary
  29.09.10 pk                                     TN5283   Short and Long comment combined, unneeded MethodEditor columns removed
  -------------------------------------------------------------------------------------------------- }


interface


uses
    SysUtils,
    ConfigurationFile,
    ListClasses,
    GeneralTypes,
    MethodDataAdaptor,
    MethodStep,
    MethodStepList,
    MethImportDefsDataAdaptor;

type
    ECompleteMethodImportException = class(Exception);

    TCompleteMethodImportFile = class
    private
        fFileName: string;
        procedure ReadIniSectionIntoOptions(const aIniFile: IConfigurationSet; const aIniSectionName: string;
            const aOptions: TStringKeyStringValueList);
        procedure AddNextStepOptions(const aNextStepSectionName: string; aIniFile: IConfigurationSet;
            aStepOptions: TStringKeyStringValueList);
        class function GetKeyValueFromOptions(xKeyName: string;
            aStepOptions: TStringKeyStringValueList): string;
        class procedure ReplacePlaceholders(var vText: string; aStepOptions: TStringKeyStringValueList);
        class function CreateMethodStepWithImportDefs(aSectionName, aKeyName: string;
            aDefinitions: TMethImportDefsRec; aStepOptions: TStringKeyStringValueList): TMethodStep;
        class function CreateMethodStepAndReplaceOptions(const aSectionName, aKeyName: string;
            aImportDefs: TMethImportDefsDataAdaptor; aStepOptions, aGeneralOptions: TStringKeyStringValueList)
            : TMethodStep;
        class function CreateSimpleMethodStep(const aSectionName, aKeyName: string;
            aImportDefs: TMethImportDefsDataAdaptor; aGeneralOptions: TStringKeyStringValueList): TMethodStep;
    public
        constructor Create(const aImportFileName: string);
        destructor Destroy(); override;
        //
        procedure ReadToMethodStepList(aImportDefs: TMethImportDefsDataAdaptor;
            aMethodStepList: TMethodStepList);
    end;

    TCompleteMethodImport = class
    private
        fImportDefs: TMethImportDefsDataAdaptor;
        //
        procedure ImportListToMethod(aStepList: TMethodStepList; const aMethodName, aLayoutName: string);
        function ReadLayoutName(): string;
    public
        constructor Create();
        destructor Destroy(); override;
        //
        procedure ImportMethod(const aImportFileName, aMethodName: string);
        class procedure CheckNewMethodName(const aMethodName: string);
    end;


implementation


uses
    StrUtils,
    RunStepBuilderTypeDictionary,
    MethodSettings,
    StringUtilities;

const
    // Symyx-Vereinbarungen
    STR_IMPORTFILE_SECTION_GENERAL = 'General';
    STR_IMPORTFILE_IDENT_STEPS = 'Steps';
    STR_IMPORTFILE_IDENT_ACTIONKEYNAME = 'Method Name';

    // eigene Definitionen:
    STR_IMPORTFILE_IDENT_FIRST_LINE = 'first:';
    STR_IMPORTFILE_IDENT_START_LINE = 'start:';
    STR_IMPORTFILE_IDENT_LAST_LINE = 'last:';
    STR_IMPORTFILE_IDENT_LAYOUT = 'layout:';
    STR_IMPORTFILE_IDENT_PRE_STEP = 'pre:';

    // Aufbau der Platzhalter: z.B. _|Input|  oder  _|General.Steps| oder _|General.File|
    STR_IMPORTFILE_STARTKEY = '_|';
    STR_IMPORTFILE_ENDKEY = '|';
    STR_IMPORTFILE_KEY_GENERAL = 'General.';
    STR_IMPORTFILE_KEY_NEXT = 'Next.';
    STR_IMPORTFILE_KEY_IMPORTFILE = 'ImportFile';
    STR_IMPORTFILE_KEY_STEP = 'Step';

    STR_IMPORTFILE_EMPTY = 'EMPTY';

    { TCompleteMethodImportFile }

constructor TCompleteMethodImportFile.Create(const aImportFileName: string);
begin
    inherited Create;

    fFileName := aImportFileName;
end;

destructor TCompleteMethodImportFile.Destroy;
begin
    inherited;

end;

class function TCompleteMethodImportFile.GetKeyValueFromOptions(xKeyName: string;
    aStepOptions: TStringKeyStringValueList): string;
var
    x: integer;
begin
    result := STR_IMPORTFILE_EMPTY; // wenn undefiniert!

    for x := 0 to aStepOptions.Count - 1 do
    begin
        if (aStepOptions[x] <> xKeyName) then
            CONTINUE;

        result := aStepOptions.Values[x];
    end;
end;

class procedure TCompleteMethodImportFile.ReplacePlaceholders(var vText: string;
    aStepOptions: TStringKeyStringValueList);
var
    xSearchTextFound: boolean;
    xPos, xLength, xPositionFound: integer;
    xKeyName, xSearchText, xReplaceText: string;
begin
    xSearchTextFound := true;

    while (xSearchTextFound) do
    begin

        xPositionFound := Pos(STR_IMPORTFILE_STARTKEY, vText);

        if (xPositionFound > 0) then
        begin

            xPos := xPositionFound + Length(STR_IMPORTFILE_STARTKEY);
            xLength := Length(vText);
            while (xPos <= xLength) and (vText[xPos] <> STR_IMPORTFILE_ENDKEY) do
            begin
                inc(xPos);
            end;

            xKeyName := Copy(vText, xPositionFound + 2, xPos - 2 - xPositionFound);
            xSearchText := Copy(vText, xPositionFound, xPos + 1 - xPositionFound);
            xReplaceText := GetKeyValueFromOptions(xKeyName, aStepOptions);

            vText := Copy(vText, 1, xPositionFound - 1) + xReplaceText +
                Copy(vText, xPositionFound + Length(xSearchText), Length(vText) - 1);
        end
        else
            xSearchTextFound := false;
    end;
end;

class function TCompleteMethodImportFile.CreateMethodStepAndReplaceOptions(const aSectionName,
    aKeyName: string; aImportDefs: TMethImportDefsDataAdaptor;
    aStepOptions, aGeneralOptions: TStringKeyStringValueList): TMethodStep;
var
    xDefinitions: TMethImportDefsRec;
    x: integer;
begin
    result := nil;
    xDefinitions := aImportDefs.ReadMethImportDefsByKey(aKeyName);
    if (xDefinitions.KeyName = '') then
        EXIT;

    // add general options
    for x := 0 to aGeneralOptions.Count - 1 do
        aStepOptions.Add(aGeneralOptions[x]);

    result := CreateMethodStepWithImportDefs(aSectionName, aKeyName, xDefinitions, aStepOptions);
end;

class function TCompleteMethodImportFile.CreateSimpleMethodStep(const aSectionName, aKeyName: string;
    aImportDefs: TMethImportDefsDataAdaptor; aGeneralOptions: TStringKeyStringValueList): TMethodStep;
var
    xDefinitions: TMethImportDefsRec;
begin
    result := nil;
    xDefinitions := aImportDefs.ReadMethImportDefsByKey(aKeyName);
    if (xDefinitions.KeyName = '') then
        EXIT;
    result := CreateMethodStepWithImportDefs(aSectionName, aKeyName, xDefinitions, aGeneralOptions);
end;

class function TCompleteMethodImportFile.CreateMethodStepWithImportDefs(aSectionName, aKeyName: string;
    aDefinitions: TMethImportDefsRec; aStepOptions: TStringKeyStringValueList): TMethodStep;
var
    xMethodStep: TMethodStep;
begin
    result := nil;
    if (aDefinitions.KeyName = '') then
        EXIT;

    ReplacePlaceholders(aDefinitions.Options, aStepOptions);
    ReplacePlaceholders(aDefinitions.Comment, aStepOptions);
    ReplacePlaceholders(aDefinitions.SchedMin, aStepOptions);
    ReplacePlaceholders(aDefinitions.SchedMax, aStepOptions);
    ReplacePlaceholders(aDefinitions.ResID, aStepOptions);

    xMethodStep := TRunStepBuilderTypeDictionary.Instance.CreateMethodStep(aDefinitions.Action);
    xMethodStep.M_Options := aDefinitions.Options;
    // xMethodStep.M_Priority := '1'; // sollte definieren Wert haben
    // xMethodStep.M_SchedMin := aDefinitions.SchedMin;
    // xMethodStep.M_SchedMax := aDefinitions.SchedMin;
    // xMethodStep.M_ResourceID := aDefinitions.ResID;
    TRunStepBuilderTypeDictionary.Instance.RefreshComments(xMethodStep);

    // Kommentare neu arrangieren:
    { TODO -oPK : Long Comment concept was removed }
    // xMethodStep.ShiftCommentShortToLong(); // 2.Kommentar kommt aus Sub-Methode

    if (aDefinitions.Comment <> '') then // 1.Kommentar enthält Import-Infos
        xMethodStep.M_Comments := Format('%s: %s', [aSectionName, aDefinitions.Comment])
    else
        xMethodStep.M_Comments := Format('%s: %s', [aSectionName, aDefinitions.KeyName]);

    result := xMethodStep;
end;

procedure TCompleteMethodImportFile.AddNextStepOptions(const aNextStepSectionName: string;
    aIniFile: IConfigurationSet; aStepOptions: TStringKeyStringValueList);
var
    x: integer;
    xNextStepOptions: TStringArray;
begin
    // Read next step from file
    xNextStepOptions := aIniFile.ReadSectionValues(aNextStepSectionName);
    for x := 0 to Length(xNextStepOptions) - 1 do
    begin

        // add "Next." key
        xNextStepOptions[x] := STR_IMPORTFILE_KEY_NEXT + xNextStepOptions[x];

        // add to step options
        aStepOptions.Add(xNextStepOptions[x]);
    end;
end;

procedure TCompleteMethodImportFile.ReadIniSectionIntoOptions(const aIniFile: IConfigurationSet;
    const aIniSectionName: string; const aOptions: TStringKeyStringValueList);
var
    xSectionValues: TStringArray;
    x: integer;
    xKey, xValue: string;
begin
    // Read general options
    xSectionValues := aIniFile.ReadSectionValues(aIniSectionName);
    for x := 0 to Length(xSectionValues) - 1 do
    begin
        TStringUtilities.SplitStr(xSectionValues[x], '=', xKey, xValue);
        aOptions.AddValue(xKey, xValue);
    end;
end;

procedure TCompleteMethodImportFile.ReadToMethodStepList(aImportDefs: TMethImportDefsDataAdaptor;
    aMethodStepList: TMethodStepList);
var
    xStepOptions, xGeneralOptions: TStringKeyStringValueList;
    xMethodStep: TMethodStep;
    xIniFile: IConfigurationSet;
    x, xOption, xStepCount: integer;
    xStepSectionName: string;

begin
    xStepOptions := TStringKeyStringValueList.Create();
    xGeneralOptions := TStringKeyStringValueList.Create();
    xIniFile := TConfigurationFile.Create(fFileName);
    xIniFile.Open(true);
    try
        xStepCount := xIniFile.ReadInteger(STR_IMPORTFILE_SECTION_GENERAL, STR_IMPORTFILE_IDENT_STEPS, 0);
        if (xStepCount <= 0) then
            EXIT;

        // Read general options
        ReadIniSectionIntoOptions(xIniFile, STR_IMPORTFILE_SECTION_GENERAL, xGeneralOptions);

        for xOption := 0 to xGeneralOptions.Count - 1 do
            xGeneralOptions[xOption] := STR_IMPORTFILE_KEY_GENERAL + xGeneralOptions[xOption];
        // add "General." key

        // add ini file name as parameter
        xGeneralOptions.AddValue(STR_IMPORTFILE_KEY_IMPORTFILE, fFileName);

        // first step
        xMethodStep := CreateSimpleMethodStep('First Step', STR_IMPORTFILE_IDENT_FIRST_LINE, aImportDefs,
            xGeneralOptions);
        if Assigned(xMethodStep) then
            aMethodStepList.Add(xMethodStep);

        // pre-steps ( before the prepare step)
        for x := 0 to xStepCount - 1 do
        begin
            xStepSectionName := Format('Step_%d', [x + 1]);

            // Read step from file
            xStepOptions.Clear;
            ReadIniSectionIntoOptions(xIniFile, xStepSectionName, xStepOptions);

            // add step number as parameter
            xStepOptions.AddValue(STR_IMPORTFILE_KEY_STEP, IntToStr(x + 1));

            // Read next step from file
            self.AddNextStepOptions(Format('Step_%d', [x + 2]), xIniFile, xStepOptions);

            xMethodStep := CreateMethodStepAndReplaceOptions('Prepare ' + xStepSectionName,
                STR_IMPORTFILE_IDENT_PRE_STEP + GetKeyValueFromOptions(STR_IMPORTFILE_IDENT_ACTIONKEYNAME,
                xStepOptions), aImportDefs, xStepOptions, xGeneralOptions);
            if Assigned(xMethodStep) then
                aMethodStepList.Add(xMethodStep)
        end;

        // start step
        xMethodStep := CreateSimpleMethodStep('Start Step', STR_IMPORTFILE_IDENT_START_LINE, aImportDefs,
            xGeneralOptions);
        if Assigned(xMethodStep) then
            aMethodStepList.Add(xMethodStep);

        for x := 0 to xStepCount - 1 do
        begin
            xStepSectionName := Format('Step_%d', [x + 1]);

            // Read step from file
            xStepOptions.Clear;
            ReadIniSectionIntoOptions(xIniFile, xStepSectionName, xStepOptions);

            // add step number as parameter
            xStepOptions.AddValue(STR_IMPORTFILE_KEY_STEP, IntToStr(x + 1));

            // Read next step from file
            self.AddNextStepOptions(Format('Step_%d', [x + 2]), xIniFile, xStepOptions);

            xMethodStep := CreateMethodStepAndReplaceOptions(xStepSectionName,
                GetKeyValueFromOptions(STR_IMPORTFILE_IDENT_ACTIONKEYNAME, xStepOptions), aImportDefs,
                xStepOptions, xGeneralOptions);

            if not Assigned(xMethodStep) then
                raise ECompleteMethodImportException.Create
                    (xStepSectionName + ': Method step definition not found');

            aMethodStepList.Add(xMethodStep)
        end;

        // last step
        xMethodStep := CreateSimpleMethodStep('Last Step', STR_IMPORTFILE_IDENT_LAST_LINE, aImportDefs,
            xGeneralOptions);
        if Assigned(xMethodStep) then
            aMethodStepList.Add(xMethodStep);

    finally
        xIniFile.Close;
        xGeneralOptions.Free;
        xStepOptions.Free;
    end;
end;

{ TCompleteMethodImport }

constructor TCompleteMethodImport.Create;
begin
    inherited;

    fImportDefs := TMethImportDefsDataAdaptor.Create();
end;

destructor TCompleteMethodImport.Destroy;
begin
    fImportDefs.Free;

    inherited;
end;

class procedure TCompleteMethodImport.CheckNewMethodName(const aMethodName: string);
var
    xExistingMethodName: string;
begin
    if (aMethodName = '') then
        raise ECompleteMethodImportException.Create('No method name!');

    xExistingMethodName := aMethodName;
    if TMethodDataAdaptor.Instance().MethodExists_NotCaseSensitive(xExistingMethodName) then
        raise ECompleteMethodImportException.CreateFmt('Method name %s exists!', [xExistingMethodName]);
end;

function TCompleteMethodImport.ReadLayoutName(): string;
var
    xLayoutDef: TMethImportDefsRec;
begin
    xLayoutDef := fImportDefs.ReadMethImportDefsByKey(STR_IMPORTFILE_IDENT_LAYOUT);
    result := xLayoutDef.Options;
end;

procedure TCompleteMethodImport.ImportMethod(const aImportFileName, aMethodName: string);
var
    xSourceFile: TCompleteMethodImportFile;
    xStepList: TMethodStepList;
    xLayoutName: string;
begin
    xLayoutName := '';
    CheckNewMethodName(aMethodName);

    xSourceFile := TCompleteMethodImportFile.Create(aImportFileName);
    xStepList := TMethodstepList.Create;
    try
        // Layout lesen
        xLayoutName := ReadLayoutName();

        // Daten aus Datei einlesen
        xSourceFile.ReadToMethodStepList(fImportDefs, xStepList);

        // MethodStepList in Method.db einlesen
        ImportListToMethod(xStepList, aMethodName, xLayoutName);
    finally
        xStepList.Free;
        xSourceFile.Free;
    end;
end;

procedure TCompleteMethodImport.ImportListToMethod(aStepList: TMethodStepList;
    const aMethodName, aLayoutName: string);
var
    xMethodDA: TMethodDataAdaptor;
    xMethodRec: TMethodRec;
    x: integer;
begin
    xMethodDA := TMethodDataAdaptor.Instance();

    for x := 0 to aStepList.Count - 1 do
    begin

        // create a record from method step class
        xMethodRec := aStepList.Items[x].AsMethodRec(aMethodName, x + 1);

        // Write record into method.db
        xMethodDA.WriteMethodRec(xMethodRec, true);
    end;

    TMethodSettings.SaveLayoutName(aMethodName, aLayoutName);
end;


end.
