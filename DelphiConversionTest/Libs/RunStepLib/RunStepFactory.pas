{ --------------------------------------------------------------------------------------------------
  Copyright © 2005 Zinsser Analytic GmbH - All rights reserved.
  Author       : Payman Kamali (pk)
  Description  : Creates TRunStep using a Run record
  --------------------------------------------------------------------------------------------------
  Revision History:
  date     op  method                       track-no improvement/change
  -------- --  ---------------------------  -------- -----------------------------------------------
  16.02.05 pk                               TN2315   New
  28.02.05 pk  CreateNextRunStep            TN2314.1 Calls Creates TPipetteRunStep instead of just TSilentRunStep
  06.04.05 pk  CreateNextRunStep            TN2373   call SetData to set args
  19.04.05 pk  CreateRunStepList            TN2390   Groups are now identified differently
  20.04.05 pk  CreateRunStepList            TN2390   Make first group ID using MethodGUIParser
  24.06.05 wl  TMessageWithBeeperRunStep    TN2459   entfernt
  15.08.05 pk                               TN2560   TMethodGUIParser changed to TRunGUIParser
  02.01.06 wl  TSQLWriteFileAction          TN2876   Parameter OutputFileName
  25.03.06 pk                               TN2999   All constructors other than TGroupStep removed
  18.04.06 pk  CreateDelayRunStep           TN3048   New
  02.05.06 pk  InitList                     TN3081   create TRunLoadRunStep
  02.05.06 pk  CreateCompositeStepList      TN3081   dont create sublist if live mode is selected
  04.05.06 pk  CreateCompositeStepList      TN3081   Sublist must always be created!
  08.05.06 pk  CreateNextRunStep            TN3087   call CreateMultiTipRunStep (previously called CreatePipetteRunStep)
  30.05.06 pk  InitList                     TN3120   TRunStartRunStep
  06.06.06 pk  CreateCompositeStepList      TN3133   Don no create substeps
  09.11.07 pk                               TN3922   DataProvider.MoveNext instead of DataAdaptor.MoveNext
  09.01.08 wl                               TN3972   TRunGUIParser ersetzt
  16.07.08 pk                               TN4157   New
  08.09.08 pk                               TN4215   Uses Changed
  09.12.08 pk  CreateNextRunStep            TN4279   reference to GroupRunStep removed
  04.02.10 pk                               TN4972   Changes for Restart
  27.02.13 wl                               TN6045   uses Generics.Collections
  -------------------------------------------------------------------------------------------------- }

unit RunStepFactory;


interface


uses
    RunStepInfo,
    RunStep;

type
    TRunStepFactory = class
    public
        constructor Create();
        destructor Destroy(); override;
        class function CreateRunStepByStepInfo(const aStepInfo: TRunStepInfo): TRunStep;
        class function CreateRunStepByName(const aStepName: string): TRunStep;
    end;


implementation


uses
    SysUtils,
    RunStepTypeInfo,
    TypeInfo,
    RunStepTypeDictionary,
    RunStepInfoFactory,
    RunStepInfoTypeDictionary;

constructor TRunStepFactory.Create();
begin
    inherited;
end;

destructor TRunStepFactory.Destroy();
begin
    inherited;
end;

class function TRunStepFactory.CreateRunStepByStepInfo(const aStepInfo: TRunStepInfo): TRunStep;
var
    xTypeInfo: TTypeInfo;
begin
    xTypeInfo := TRunStepTypeDictionary.Instance.GetTypeFromTypeName(aStepInfo.DefaultName);
    ASSERT((xTypeInfo is TRunStepTypeInfo), 'Typeinfo is not TRunStepTypeInfo');
    result := (xTypeInfo as TRunStepTypeInfo).RunStepCreator.CreateRunStep();
    result.RunStepInfo := aStepInfo;
end;

class function TRunStepFactory.CreateRunStepByName(const aStepName: string): TRunStep;
var
    xRunStepInfo: TRunStepInfo;
begin
    xRunStepInfo := TRunStepInfoFactory.CreateRunStepInfoByTypeName(aStepName);
    result := CreateRunStepByStepInfo(xRunStepInfo);
end;


end.
