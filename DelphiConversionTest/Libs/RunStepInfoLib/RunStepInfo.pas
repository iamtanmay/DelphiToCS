unit RunStepInfo;
{ --------------------------------------------------------------------------------------------------
  Copyright © 2008 Zinsser Analytic GmbH - All rights reserved.
  Author       : Wolfgang Lyncke (wl)
  Description  : soll alle wichtigen Infos zu einem Method- bzw. Run-Step enthalten
  --------------------------------------------------------------------------------------------------
  Revision History:
  date     op  method                       track-no  improvement/change
  -------- --  ---------------------------  --------  ----------------------------------------------
  08.01.08 wl                               TN3972    initial version
  08.09.08 pk                               TN4215    uses
  20.09.08 pk  TUnknownRunStepInfo          TN4215    new
  19.02.09 pk  TRunStepInfo                 TN4232    inherits from TStremable
  19.02.09 pk  fResNo                        TN4232    ersetzt durch fCaption & fDescription
  08.05.09 wl  fDescription                 TN4555    Defaultwert ist '', nicht "Es gibt keine Beschreibung"
  04.02.10 pk                               TN4972   Changes for Restart
  21.09.10 pk  IsCodeStep                   TN5089    New
  29.09.10 pk  GetIsExecutable              TN5284    New
  -------------------------------------------------------------------------------------------------- }


interface


uses
    Streamable;

type
    TRunStepInfoOptions = (rsoNone, rsoIterate, rsoSchedulingAndIterate);

    TRunStepInfo = class(TStreamable)
    protected
        fCaption: string;
        fDescription: string;
        function GetDefaultName: string; virtual;
        function GetChangesRackPositionInLayout(): boolean; virtual;
        function GetAdditionalOptions: TRunStepInfoOptions; virtual;
        function GetIconIndex: integer; virtual;
        function GetPaintPositions: boolean; virtual;
        function GetIsCodeStep(): boolean; virtual;
        function GetIsExecutable(): boolean; virtual;
    public
        constructor Create(); override;

        property DefaultName: string read GetDefaultName;
        property Caption: string read fCaption;
        property Description: string read fDescription;
        property ChangesRackPositionInLayout: boolean read GetChangesRackPositionInLayout;
        property AdditionalOptions: TRunStepInfoOptions read GetAdditionalOptions;
        property IconIndex: integer read GetIconIndex;
        property PaintPositions: boolean read GetPaintPositions;
        property IsCodeStep: boolean read GetIsCodeStep;
        property IsExecutable: boolean read GetIsExecutable;
    end;

    TRunStepInfoClass = class of TRunStepInfo;

    TUnknownRunStepInfo = class(TRunStepInfo)
    protected
        function GetIconIndex(): integer; override;
        function GetDefaultName(): string; override;
    end;


implementation


uses
    SysUtils,
    MethodTypes,
    GeneralTypes,
    TypeInfo,
    RunStepInfoTypeInfo,
    RunStepInfoTypeDictionary;

{ TStepInfo }

constructor TRunStepInfo.Create();
begin
    inherited Create;

    fCaption := TLanguageString.Read('Undefined method step', 'Nicht definierter Methodenschritt');
    fDescription := '';
end;

function TRunStepInfo.GetDefaultName: string;
begin
    result := 'XXXXX'; // Diese Funktion soll noch weg
end;

function TRunStepInfo.GetChangesRackPositionInLayout(): boolean;
begin
    result := false;
end;

function TRunStepInfo.GetAdditionalOptions: TRunStepInfoOptions;
begin
    result := rsoIterate;
end;

function TRunStepInfo.GetIconIndex: integer;
begin
    result := INT_IM_INDEX_ACTION;
end;

function TRunStepInfo.GetIsCodeStep: boolean;
begin
    result := false;
end;

function TRunStepInfo.GetIsExecutable: boolean;
begin
    result := true;
end;

function TRunStepInfo.GetPaintPositions: boolean;
begin
    result := false;
end;

{ TUnknownRunStepInfo }

function TUnknownRunStepInfo.GetDefaultName: string;
begin
    result := 'Uknown';
end;

function TUnknownRunStepInfo.GetIconIndex: integer;
begin
    result := INT_IM_INDEX_ACTION_UNKNOWN
end;


end.
