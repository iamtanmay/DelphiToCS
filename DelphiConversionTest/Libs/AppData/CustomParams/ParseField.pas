{ --------------------------------------------------------------------------------------------------
  Copyright © 2008 Zinsser Analytic GmbH - All rights reserved.
  Author       : Payman Kamali (pk)
  Description  :
  --------------------------------------------------------------------------------------------------
  Revision History:
  date     op  method                       track-no  improvement/change
  -------- --  ---------------------------  --------  ----------------------------------------------
  06.10.08 pk                               TN4258    New
  07.05.10 pk  Evaluate                     TN5092    New
  01.03.12 wl                               TN5822   uses geändert
  05.07.12 wl  Evaluate                     TN5917   ist jetzt abstract
  -------------------------------------------------------------------------------------------------- }

unit ParseField;


interface


uses
    ParserIdentDataType;

type
    TParseField = class abstract
    protected
        fFieldName: string;
        fUnparsedVal: string;
        function GetAsString(): string; virtual; abstract;
    public
        constructor Create(const aFieldName: string; const aUnparsedVal: string);
        function Evaluate: TArg; virtual; abstract;
        property UnparsedVal: string read fUnparsedVal;
        property FieldName: string read fFieldName;
        property AsString: string read GetAsString;

    end;


implementation


constructor TParseField.Create(const aFieldName: string; const aUnparsedVal: string);
begin
    inherited Create();
    fFieldName := aFieldName;
    fUnparsedVal := aUnparsedVal;
end;


end.
