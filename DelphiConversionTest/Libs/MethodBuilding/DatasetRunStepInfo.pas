unit DatasetRunStepInfo;
{ -----------------------------------------------------------------------------------------------------------------------
  Copyright © 2009 Zinsser Analytic GmbH - All rights reserved.
  Author       : Payman Kamali (pk)
  Description  :
  -----------------------------------------------------------------------------------------------------------------------
  Revision History:
  date     op  method                                track-no  improvement/change
  -------- --  ------------------------------------  --------- ----------------------------------------------------------
  17.02.09 pk                                        TN4232    Initial Revision
  20.02.09 wl  ..RunStepInfo.Create                  TN4438    bestimmt fCaption und fDescription (GetResNo entfernt)
  08.10.10 pk                                        TN5295    GetIsHidden implemented
  23.02.11 wl  GetIconIndex                          TN5486   neue Icons
  11.03.11 wl                                        TN5500   Caption für alle Actions verständlicher
  ----------------------------------------------------------------------------------------------------------------------- }


interface


uses
    RunStepInfo,
    MethodTypes,
    RunStepInfoTypeInfo;

const
    cActionNameDatasetOpen = 'DSOPN';

type
    TDatasetOpenRunStepInfo = class(TRunStepInfo)
    protected
        function GetIconIndex: integer; override;
        function GetDefaultName: string; override;
        function GetAdditionalOptions: TRunStepInfoOptions; override;
    public
        constructor Create(); override;
    end;

    TDatasetOpenRunStepInfoCreator = class(TRunStepInfoCreator)
    protected
        function DoCreateRunStepInfo: TRunStepInfo; override;
    end;

    TDatasetOpenRunStepInfoTypeInfo = class(TRunStepInfoTypeInfo)
    protected
        procedure DoCreateRunStepInfoCreator(); override;
        function GetIsHidden: boolean; override;
    public
        constructor Create(const aLibName, aLibVersion: string);
    end;

const
    cActionNameDatasetClose = 'DSCLS';

type
    TDatasetCloseRunStepInfo = class(TRunStepInfo)
    protected
        function GetIconIndex: integer; override;
        function GetDefaultName: string; override;
        function GetAdditionalOptions: TRunStepInfoOptions; override;
    public
        constructor Create(); override;
    end;

    TDatasetCloseRunStepInfoCreator = class(TRunStepInfoCreator)
    protected
        function DoCreateRunStepInfo: TRunStepInfo; override;
    end;

    TDatasetCloseRunStepInfoTypeInfo = class(TRunStepInfoTypeInfo)
    protected
        procedure DoCreateRunStepInfoCreator(); override;
        function GetIsHidden: boolean; override;
    public
        constructor Create(const aLibName, aLibVersion: string);
    end;

const
    cActionNameDatasetCursorMove = 'DSCUR';

type
    TDatasetCursorMoveRunStepInfo = class(TRunStepInfo)
    protected
        function GetIconIndex: integer; override;
        function GetDefaultName: string; override;
        function GetAdditionalOptions: TRunStepInfoOptions; override;
    public
        constructor Create(); override;
    end;

    TDatasetCursorMoveRunStepInfoCreator = class(TRunStepInfoCreator)
    protected
        function DoCreateRunStepInfo: TRunStepInfo; override;
    end;

    TDatasetCursorMoveRunStepInfoTypeInfo = class(TRunStepInfoTypeInfo)
    protected
        procedure DoCreateRunStepInfoCreator(); override;
        function GetIsHidden: boolean; override;
    public
        constructor Create(const aLibName, aLibVersion: string);
    end;

const
    cActionNameDatasetRead = 'DSREA';

type
    TDatasetReadRunStepInfo = class(TRunStepInfo)
    protected
        function GetIconIndex: integer; override;
        function GetDefaultName: string; override;
        function GetAdditionalOptions: TRunStepInfoOptions; override;
    public
        constructor Create(); override;
    end;

    TDatasetReadRunStepInfoCreator = class(TRunStepInfoCreator)
    protected
        function DoCreateRunStepInfo: TRunStepInfo; override;
    end;

    TDatasetReadRunStepInfoTypeInfo = class(TRunStepInfoTypeInfo)
    protected
        procedure DoCreateRunStepInfoCreator(); override;
        function GetIsHidden: boolean; override;
    public
        constructor Create(const aLibName, aLibVersion: string);
    end;


implementation


uses
    GeneralTypes;

{ TDatasetOpenRunStepInfo }

constructor TDatasetOpenRunStepInfo.Create;
begin
    inherited Create;

    fCaption := TLanguageString.Read('Import begin', 'Import beginnen');
    fDescription := ''
end;

function TDatasetOpenRunStepInfo.GetAdditionalOptions: TRunStepInfoOptions;
begin
    result := rsoNone;
end;

function TDatasetOpenRunStepInfo.GetDefaultName(): string;
begin
    result := cActionNameDatasetOpen;
end;

function TDatasetOpenRunStepInfo.GetIconIndex: integer;
begin
    EXIT(cImageIndexActionDataset);
end;

{ TDatasetOpenRunStepInfoCreator }

function TDatasetOpenRunStepInfoCreator.DoCreateRunStepInfo: TRunStepInfo;
begin
    result := TDatasetOpenRunStepInfo.Create();
end;

{ TDatasetOpenRunStepInfoTypeInfo }

constructor TDatasetOpenRunStepInfoTypeInfo.Create(const aLibName, aLibVersion: string);
const
    cStepTypeVersionDatasetOpen = '1.0.0';
    cStepTypeNameDatasetOpen = cActionNameDatasetOpen;
begin
    inherited Create(cStepTypeNameDatasetOpen, cStepTypeVersionDatasetOpen, aLibName, aLibVersion);
end;

procedure TDatasetOpenRunStepInfoTypeInfo.DoCreateRunStepInfoCreator;
begin
    fRunStepInfoCreator := TDatasetOpenRunStepInfoCreator.Create();
end;

function TDatasetOpenRunStepInfoTypeInfo.GetIsHidden: boolean;
begin
    result := true;
end;

{ TDatasetCloseRunStepInfo }

constructor TDatasetCloseRunStepInfo.Create;
begin
    inherited Create;

    fCaption := TLanguageString.Read('Import end', 'Import beenden');
    fDescription := ''
end;

function TDatasetCloseRunStepInfo.GetAdditionalOptions: TRunStepInfoOptions;
begin
    result := rsoNone;
end;

function TDatasetCloseRunStepInfo.GetDefaultName(): string;
begin
    result := cActionNameDatasetClose;
end;

function TDatasetCloseRunStepInfo.GetIconIndex: integer;
begin
    EXIT(cImageIndexActionDataset);
end;

{ TDatasetCloseRunStepInfoCreator }

function TDatasetCloseRunStepInfoCreator.DoCreateRunStepInfo: TRunStepInfo;
begin
    result := TDatasetCloseRunStepInfo.Create();
end;

{ TDatasetCloseRunStepInfoTypeInfo }

constructor TDatasetCloseRunStepInfoTypeInfo.Create(const aLibName, aLibVersion: string);
const
    cStepTypeVersionDatasetClose = '1.0.0';
    cStepTypeNameDatasetClose = cActionNameDatasetClose;
begin
    inherited Create(cStepTypeNameDatasetClose, cStepTypeVersionDatasetClose, aLibName, aLibVersion);
end;

procedure TDatasetCloseRunStepInfoTypeInfo.DoCreateRunStepInfoCreator;
begin
    fRunStepInfoCreator := TDatasetCloseRunStepInfoCreator.Create();
end;

function TDatasetCloseRunStepInfoTypeInfo.GetIsHidden: boolean;
begin
    result := true;
end;

{ TDatasetCursorMoveRunStepInfo }

constructor TDatasetCursorMoveRunStepInfo.Create;
begin
    inherited Create;

    fCaption := TLanguageString.Read('Import, go to next line', 'Import, gehe zu nächster Zeile');
    fDescription := ''
end;

function TDatasetCursorMoveRunStepInfo.GetAdditionalOptions: TRunStepInfoOptions;
begin
    result := rsoNone;
end;

function TDatasetCursorMoveRunStepInfo.GetDefaultName(): string;
begin
    result := cActionNameDatasetCursorMove;
end;

function TDatasetCursorMoveRunStepInfo.GetIconIndex: integer;
begin
    EXIT(cImageIndexActionDataset);
end;

{ TDatasetCursorMoveRunStepInfoCreator }

function TDatasetCursorMoveRunStepInfoCreator.DoCreateRunStepInfo: TRunStepInfo;
begin
    result := TDatasetCursorMoveRunStepInfo.Create();
end;

{ TDatasetCursorMoveRunStepInfoTypeInfo }

constructor TDatasetCursorMoveRunStepInfoTypeInfo.Create(const aLibName, aLibVersion: string);
const
    cStepTypeVersionDatasetCursorMove = '1.0.0';
    cStepTypeNameDatasetCursorMove = cActionNameDatasetCursorMove;
begin
    inherited Create(cStepTypeNameDatasetCursorMove, cStepTypeVersionDatasetCursorMove, aLibName,
        aLibVersion);
end;

procedure TDatasetCursorMoveRunStepInfoTypeInfo.DoCreateRunStepInfoCreator;
begin
    fRunStepInfoCreator := TDatasetCursorMoveRunStepInfoCreator.Create();
end;

function TDatasetCursorMoveRunStepInfoTypeInfo.GetIsHidden: boolean;
begin
    result := true;
end;

{ TDatasetReadRunStepInfo }

constructor TDatasetReadRunStepInfo.Create;
begin
    inherited Create;

    fCaption := TLanguageString.Read('Import, Read line', 'Import, Zeile lesen');
    fDescription := ''
end;

function TDatasetReadRunStepInfo.GetAdditionalOptions: TRunStepInfoOptions;
begin
    result := rsoNone;
end;

function TDatasetReadRunStepInfo.GetDefaultName(): string;
begin
    result := cActionNameDatasetRead;
end;

function TDatasetReadRunStepInfo.GetIconIndex: integer;
begin
    EXIT(cImageIndexActionDataset);
end;

{ TDatasetReadRunStepInfoCreator }

function TDatasetReadRunStepInfoCreator.DoCreateRunStepInfo: TRunStepInfo;
begin
    result := TDatasetReadRunStepInfo.Create();
end;

{ TDatasetReadRunStepInfoTypeInfo }

constructor TDatasetReadRunStepInfoTypeInfo.Create(const aLibName, aLibVersion: string);
const
    cStepTypeVersionDatasetRead = '1.0.0';
    cStepTypeNameDatasetRead = cActionNameDatasetRead;
begin
    inherited Create(cStepTypeNameDatasetRead, cStepTypeVersionDatasetRead, aLibName, aLibVersion);
end;

procedure TDatasetReadRunStepInfoTypeInfo.DoCreateRunStepInfoCreator;
begin
    fRunStepInfoCreator := TDatasetReadRunStepInfoCreator.Create();
end;

function TDatasetReadRunStepInfoTypeInfo.GetIsHidden: boolean;
begin
    result := true;
end;


end.
