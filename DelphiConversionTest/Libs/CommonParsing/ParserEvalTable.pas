{ -----------------------------------------------------------------------------------------------------------------------
  Copyright © 2008 Zinsser Analytic GmbH - All rights reserved.
  Author       : Payman Kamali (pk)
  Description  :
  -----------------------------------------------------------------------------------------------------------------------
  Revision History:
  date     op  method                                track-no  improvement/change
  -------- --  ------------------------------------- --------  ----------------------------------------------------------
  06.11.08 pk                                        TN4279    Initial Revision
  08.01.09 pk  TAllocateResourceCallback             TN4279    change to const
  17.02.09 pk                                        TN4232    Resource callbacks removed
  07.05.10 pk                                        TN5092    changes for arrays
  01.03.12 wl                                        TN5822   TArg statt TAttrValue
  01.03.12 wl  TSetIdentifierValueCallback           TN5822   ohne Index-Parameter
  ----------------------------------------------------------------------------------------------------------------------- }

unit ParserEvalTable;


interface


uses
    ParserIdentDataType;

type
    TSetIdentifierValueCallback = procedure(const aIdentName: string; const aValue: TArg) of object;
    TGetIdentifierValueCallback = procedure(const aIdentName: string; const aArrayIndex: integer;
        out oValue: TArg) of object;
    TSetArrayLengthCallback = procedure(const aIdentName: string; const aLength: integer) of object;

    TParserEvalTable = class
    private
        fSetIdentifierValueCallback: TSetIdentifierValueCallback;
        fGetIdentifierValueCallback: TGetIdentifierValueCallback;
        fSetArrayLengthCallback: TSetArrayLengthCallback;
    public
        constructor Create;
        property SetIdentifierValue: TSetIdentifierValueCallback read fSetIdentifierValueCallback
            write fSetIdentifierValueCallback;
        property GetIdentifierValue: TGetIdentifierValueCallback read fGetIdentifierValueCallback
            write fGetIdentifierValueCallback;
        property SetArrayLength: TSetArrayLengthCallback read fSetArrayLengthCallback
            write fSetArrayLengthCallback;
    end;


implementation


{ TParserEvalTable }

constructor TParserEvalTable.Create;
begin
    inherited Create();
end;


end.
