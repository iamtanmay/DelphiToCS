{ --------------------------------------------------------------------------------------------------
  Copyright © 2004 Zinsser Analytic GmbH - All rights reserved.
  Author       : Wolfgang Lyncke (wl)
  Description  : Class that represents an identifier variable (for parser10)
  --------------------------------------------------------------------------------------------------
  Revision History:
  date     op  method                       track-no improvement/change
  -------- --  ---------------------------  -------- -----------------------------------------------
  30.08.04 wl                               TN2097   initial version
  31.08.04 wl                               TN2097   Änderungen im Detail
  10.11.04 wl  WriteValue,DeleteValue       TN2213   neu: für TParamStore/TParamDeleteAction
  02.02.05 pk  GetValueOrDefault()          TN2304   new : if value exists returns value, otherwise return default
  28.04.05 wl  DeleteIdentifier             TN2248.8 neu: löscht alle Daten zu einem Identifier
  28.04.05 wl  GetAllExistingVariables      TN2248.8 soll alle existierenden Identifier finden (noch nicht perfekt)
  07.06.05 pk  ValueNecessary               TN2449   Now named NeedsInput() - Do NOT read value from database
  05.07.05 pk  IsNonInput                   TN1494   New property
  25.08.05 pk  TParserStoredIdent           TN2547   New : code extracted from TParserIdentifier
  25.08.05 pk  TParserStoredIdent           TN2547   Now uses ParserIdentReaderWriter to access data
  25.08.05 pk  TParserStoredIdent           TN2547   New : CreateWithReaderWriterSettings#
  18.11.05 pk  TParserStoredIdent           TN2779   New : CanUseValue
  21.03.06 wl  TParserStoredIdent.CreatePickList  TN2985   Bei einer Exception steht in der DropdownList "Error reading list items"
  20.02.07 wl                               TN3016   entfernt: uses ParserWrapper
  02.08.07 wl  TParserStoredIdent           TN3817   neue Funktionen für Darstellung in TreeList
  27.08.07 pk  TParserStoredIdent           TN3788   moved to New ParserStoredIdentifier.Pas unit
  04.09.07 pk                               TN       uses ArgClass
  09.07.08 pk  SetValue                     TN4160   changed so that no internal exceptions are raised
  06.11.08 pk  TParserIdentDataType         TN4279   moved to ParserIdentDataType unit
  06.11.08 pk  IParserSymbolTable           TN4279   move to ParserEvalTable
  04.11.09 pk                               TN4843   Replace TObjectList, TStringList, TStrings with Generics Lists and TStringArray
  04.02.10 pk                               TN4973   Compiler Warnings fixed
  07.05.10 pk  TParserIdentifier            TN5092   fValue is now an object instead of a simple string
  27.06.11 wl                               TN5609   neu: TAttrValueUtils
  01.03.12 wl                               TN5822   TArg statt TAttrValue
  05.07.12 wl  TParserIdentifier.StrToValue TN5917   an TArgUtils angepasst
  14.12.12 wl  IsRunIdent                   TN6054   entfernt
  14.12.12 wl  IsNonInputIdent              TN6054   _!-(Run-)Variablen sind jetzt globale non-input-Variablen
  27.03.13 wl  ValueToStr,StrToValue        TN6045   --> ParserStoredIdent
  10.04.13 wl                               TN6045   uses geändert
  10.06.13 wl  IsNonInputIdent,NeedsInput   TN6045   --> ParserSymbolTable
  10.06.13 wl  Destroy                      TN6045   neu: fValue wird gelöscht (fixed memory leak!)
  25.06.13 wl                               TN6178   ist jetzt von TIdentifier abgeleitet
  25.06.13 wl  IsNonInputIdent,NeedsInput   TN6178   --> TParserIdentifierUtils (ParserSymbolTable)
  -------------------------------------------------------------------------------------------------- }

unit ParserIdentifier;


interface


uses
    Identifier,
    ParserIdentDataType;

type
    TParserIdentType = (itNone, itGlobal, itLocal);

    TParserIdentifier = class(TIdentifier)
    private
        function GetName: string;
    public
        constructor Create(const aKey: string);
        class function GetIdentType(aIdentName: string): TParserIdentType; static;
        property name: string read GetName;
    end;


implementation


uses
    SysUtils;

{ TParserIdentifier }

constructor TParserIdentifier.Create(const aKey: string);
begin
    inherited Create(aKey, true);
end;

class function TParserIdentifier.GetIdentType(aIdentName: string): TParserIdentType;
const
    cChrIdentStart = '_';
    cChrLocalType = '$';
var
    xLen: integer;
begin
    xLen := Length(aIdentName);

    if (xLen >= 2) and (aIdentName[1] = cChrIdentStart) and (aIdentName[2] = cChrLocalType) then
        EXIT(itLocal);

    if (xLen >= 1) and (aIdentName[1] = cChrIdentStart) then
        EXIT(itGlobal);

    EXIT(itNone);
end;

function TParserIdentifier.GetName: string;
begin
    EXIT(self.Key);
end;


end.
