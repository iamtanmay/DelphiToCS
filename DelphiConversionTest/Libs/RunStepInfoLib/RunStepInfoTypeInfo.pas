{ -----------------------------------------------------------------------------------------------------------------------
  Copyright © 2008 Zinsser Analytic GmbH - All rights reserved.
  Author       : Payman Kamali (pk)
  Description  :
  -----------------------------------------------------------------------------------------------------------------------
  Revision History:
  date     op  method                                track-no  improvement/change
  -------- --  ------------------------------------  --------- ----------------------------------------------------------
  17.02.09 pk  RegisterRunStepInfoClass              TN4232    New
  22.07.09 pk  GetIsHidden                           TN4668    New
  08.10.10 pk                                        TN5295    New DoGetCategoryName(s)
  15.05.12 ts                                        TN5896   CategoryNameDefault is Other instead of Unknown
  06.06.12 wl                                        TN5908   Categories StepByStep, Blocks neu
  06.06.12 wl                                        TN5908   Categories Time, Movement entfernt
  ----------------------------------------------------------------------------------------------------------------------- }

unit RunStepInfoTypeInfo;


interface


uses
    RunStepInfo,
    TypeInfo,
    GeneralTypes;

type

    TRunStepInfoCreator = class
    protected
        function DoCreateRunStepInfo(): TRunStepInfo; virtual; abstract;
        procedure RegClass(const aRunStepInfoClass: TRunStepInfoClass);
        procedure DoRegisterRunStepInfoClass(); virtual;
    public
        function CreateRunStepInfo(): TRunStepInfo;
        procedure RegisterRunStepInfoClass();
    end;

    TRunStepInfoTypeInfo = class(TTypeInfo)
    protected
        fRunStepInfoCreator: TRunStepInfoCreator;
        procedure DoCreateRunStepInfoCreator(); virtual; abstract;
        function GetIsHidden: boolean; virtual;
        function MakeCategoryNamesArray(const aArgs: array of string): TStringArray;
        function DoGetSingleCategoryName(): string; virtual;
        function DoGetCategoryNames: TStringArray; virtual;
    public
        constructor Create(const aTypeName, aTypeInfoVersion, aLibName, aLibVersion: string);
        property RunStepInfoCreator: TRunStepInfoCreator read fRunStepInfoCreator;
        property IsHidden: boolean read GetIsHidden;
        function GetCategoryNames: TStringArray;
        class function CategoryNameStandard(): string;
        class function CategoryNameWeighing(): string;
        class function CategoryNameDevice: string;
        class function CategoryNameLayout: string;
        class function CategoryNameThread: string;
        class function CategoryNamePipetting(): string;
        class function CategoryNameStepByStep: string;
        class function CategoryNameBlocks: string;
        class function CategoryNameFileAndDatabase: string;
        class function CategoryNameDisplay: string;
        class function CategoryNameControlFlow: string;
        class function CategoryNameBarcode: string;
        class function CategoryNameRack(): string;
        class function CategoryNameTube(): string;
    end;


implementation


uses
    Classes;

{ TRunStepInfoTypeInfo }

constructor TRunStepInfoTypeInfo.Create(const aTypeName, aTypeInfoVersion, aLibName, aLibVersion: string);
begin
    inherited Create(aTypeName, aTypeInfoVersion, aLibName, aLibVersion);
    DoCreateRunStepInfoCreator();
end;

function TRunStepInfoCreator.CreateRunStepInfo(): TRunStepInfo;
begin
    result := DoCreateRunStepInfo();
end;

procedure TRunStepInfoCreator.RegClass(const aRunStepInfoClass: TRunStepInfoClass);
begin
    RegisterClass(aRunStepInfoClass);
end;

procedure TRunStepInfoCreator.DoRegisterRunStepInfoClass;
var
    xRunStepInfo: TRunStepInfo;
begin
    xRunStepInfo := self.CreateRunStepInfo();
    try
        RegClass(TRunStepInfoClass(xRunStepInfo.ClassType));
    finally
        xRunStepInfo.Free;
    end;
end;

procedure TRunStepInfoCreator.RegisterRunStepInfoClass;
begin
    DoRegisterRunStepInfoClass();
end;

function TRunStepInfoTypeInfo.DoGetCategoryNames: TStringArray;
begin
    SetLength(result, 0);
end;

function TRunStepInfoTypeInfo.DoGetSingleCategoryName: string;
begin
    result := CategoryNameStandard;
end;

function TRunStepInfoTypeInfo.GetCategoryNames: TStringArray;
var
    xCategoryName: string;
begin
    result := DoGetCategoryNames();
    if Length(result) > 0 then
        EXIT;

    xCategoryName := DoGetSingleCategoryName();
    if xCategoryName = '' then
        EXIT;
    result := self.MakeCategoryNamesArray(xCategoryName);
end;

function TRunStepInfoTypeInfo.GetIsHidden: boolean;
begin
    result := false;
end;

function TRunStepInfoTypeInfo.MakeCategoryNamesArray(const aArgs: array of string): TStringArray;
var
    x: integer;
begin
    SetLength(result, Length(aArgs));
    for x := 0 to Length(aArgs) - 1 do
        result[x] := aArgs[x];

end;

class function TRunStepInfoTypeInfo.CategoryNameBlocks: string;
begin
    EXIT(TLanguageString.Read('Block Steps', 'Block-Schritte'));
end;

class function TRunStepInfoTypeInfo.CategoryNameControlFlow: string;
begin
    EXIT(TLanguageString.Read('Code', 'Code'));
end;

class function TRunStepInfoTypeInfo.CategoryNameBarcode: string;
begin
    result := TLanguageString.Read('Barcode', 'Barcode');
end;

class function TRunStepInfoTypeInfo.CategoryNamePipetting: string;
begin
    result := TLanguageString.Read('Pipetting', 'Pipettieren');
end;

class function TRunStepInfoTypeInfo.CategoryNameStepByStep: string;
begin
    result := TLanguageString.Read('Pipetting (Step by step)', 'Pipettieren (schrittweise)');
end;

class function TRunStepInfoTypeInfo.CategoryNameRack: string;
begin
    result := TLanguageString.Read('Rack', 'Rack');
end;

class function TRunStepInfoTypeInfo.CategoryNameStandard: string;
begin
    result := TLanguageString.Read('Other', 'Sonstiges');
end;

class function TRunStepInfoTypeInfo.CategoryNameThread: string;
begin
    result := TLanguageString.Read('Thread', 'Thread');
end;

class function TRunStepInfoTypeInfo.CategoryNameTube: string;
begin
    result := TLanguageString.Read('Tube', 'Tube');
end;

class function TRunStepInfoTypeInfo.CategoryNameWeighing: string;
begin
    result := TLanguageString.Read('Weighing', 'Wiegen');
end;

class function TRunStepInfoTypeInfo.CategoryNameDevice: string;
begin
    result := TLanguageString.Read('Device', 'Device');
end;

class function TRunStepInfoTypeInfo.CategoryNameDisplay: string;
begin
    result := TLanguageString.Read('Display', 'Anzeige');
end;

class function TRunStepInfoTypeInfo.CategoryNameLayout: string;
begin
    result := TLanguageString.Read('Layout', 'Layout');
end;

class function TRunStepInfoTypeInfo.CategoryNameFileAndDatabase: string;
begin
    result := TLanguageString.Read('File + Database', 'Datei + Datenbank');
end;


end.
