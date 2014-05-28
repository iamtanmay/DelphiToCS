{ -----------------------------------------------------------------------------------------------------------------------
  Copyright © 2008 Zinsser Analytic GmbH - All rights reserved.
  Author       : Payman Kamali (pk)
  Description  :
  -----------------------------------------------------------------------------------------------------------------------
  Revision History:
  date     op  method                                track-no  improvement/change
  -------- --  ------------------------------------  --------- ----------------------------------------------------------
  08.09.08 pk                                        TN4215    Initial Revision
  20.09.08 pk                                        TN4215
  25.09.08 wl                                        TN4242    uses MethodStepSettingRunStart
  06.10.08 pk                                        TN4258    Various changes
  28.07.09 pk  PosStepArrayToStr                     TN4683    New
  28.07.09 pk  DoGetDescription                      TN4683    SourceRack was logged incorrectly instead of the DestRack
  25.08.09 pk                                        TN4745    Volumes now logged
  12.09.09 wl                                        TN4740    TipFloat-/TPosMM-/TipInteger-/MPOSArray durch TDoubleArray/TIntArray ersetzt
  22.10.09 pk  TBasicPipetRunStepBuilder             TN4759    New
  26.11.09 pk                                        TN4898    Register TCombinedPipetteRunStep class
  16.12.09 pk  TCombinedPipetteRunStep               TN4950    New UsedTips and UsedTipType
  04.02.10 pk                                        TN4972    Changes for Restart
  08.04.10 pk  DoGetDescription                      TN4996    now also includes source vol
  15.04.10 pk                                        TN5050    call DetermineUsableTips instead of SetUsedTips
  23.04.10 pk  GetUsedTips                           TN5050    now with oAllowedTips parameter
  07.05.10 pk                                        TN5092    TRunStartOptions now a class instead of record
  29.06.10 pk                                        TN5143    changes for using different LiqParams together
  18.08.10 wl  GetUsedTips                           TN5240    Wenn PipDevice nicht existiert, gibt es jetzt eine richtige Fehlermeldung
  23.08.10 wl  SetTipLiqDetErrorType                 TN5214    entfernt
  31.08.10 wl  StepContainsAnyVolume                 TN5251    wird überhaupt etwas transferiert
  26.10.10 pk  TCombinedPipetteRunStep               TN5297    new GetRestartAtStepAllowed
  06.04.11 wl  TBasicPipetteRunStep.LiqParam         TN5501    enthält alle Liquid-Handling-Daten
  19.07.11 wl  DTransAirRetake                       TN5630   DTransAirRetake boolean statt double
  27.09.11 wl                                        TN5698   verwendet TLiqHandlingData für TPipetteRunStep
  12.12.11 wl  TMultiPipStrategy                     TN5764   --> AppTypes
  14.12.11 wl                                        TN5765   uses geändert
  02.02.11 wl  TPipetteRackPositions                 TN5791   enthält TArray<TXRackPosition> statt TRack und TIntArray für Positionen
  15.08.12 wl  cSource/cDestSortCriteriaDefault      TN5956   Default-Werte für Source und Dest Criteria wieder wie in 8.0.3
  11.04.13 wl  TCriteriaArray                        TN6128   ersetzt durch TArray<string>
  21.10.13 wl  TMultiPipStrategy                     TN6276   entfernt
  21.10.13 wl  TStepOptimizerOptions                 TN6276   neu
  ----------------------------------------------------------------------------------------------------------------------- }

unit BasicPipetteTypes;


interface


uses
    AppTypes,
    RackTypes;

type
    TPipetteVolType = (pvtSource, pvtDest, pvtDil);

    TAspirateEvOptions = record
        BeforeAsp: string;
        BeforePickLq: string;
        AfterPickLq: string;
        AfterAsp: string;
    end;

    TDispenseEvOptions = record
        BeforeDispense: string;
        BeforeDispLq: string;
        AfterDispLq: string;
        AfterDispense: string;
        TransAir: string;
    end;

    TDilRackPositions = record
        Diluent: integer;
        DilRackType: TDilRackType;
        Positions: TArray<TXRackPosition>;
    end;

    TPipetteRackPositions = record
        Source: TArray<TXRackPosition>;
        Dest: TArray<TXRackPosition>;
        Dil: TDilRackPositions;
    end;

    TMultiPipStepShiftType = (ssoNoShift, ssoReduceSourceSteps); // ReduceDestSteps ist denkbar
    TMultiPipTipScatteringType = (stoNone, stoStandard); // Progressive denkbar

    TStepOptimizerOptions = record
        BasicSortCriteria: string;
        SourceSortCriteria: string;
        DestSortCriteria: string;
        TipScattering: TMultiPipTipScatteringType;
        ShiftType: TMultiPipStepShiftType;
        CheckSourcePos: boolean;
        CheckDestPos: boolean;
        SortDestByTips: boolean;
    end;

const
    cSourceSortCriteriaDefault = 'SOURCERACK;SOURCEPOS';
    cSourceSortCriteriaDefault2 = 'RunSourcePosX;RunSourcePosY';
    cSourceSortCriteriaNone = 'NONE';
    cDestSortCriteriaDefault = 'DESTRACK;DESTPOS';
    cDestSortCriteriaDefault2 = 'RunDestPosX;RunDestPosY';
    cDestSortCriteriaNone = 'NONE';


implementation


end.
