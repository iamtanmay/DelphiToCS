unit LayoutElementGraphicsDriverTypeDictionary;


{ --------------------------------------------------------------------------------------------------
  Copyright © 2008 Zinsser Analytic GmbH - All rights reserved.
  Author       : Payman Kamali (pk)
  Description  :
  --------------------------------------------------------------------------------------------------
  Revision History:
  date     op  method                       track-no improvement/change
  -------- --  ---------------------------  -------- -----------------------------------------------
  23.06.08 pk                                TN4139  Initial Revision
  -------------------------------------------------------------------------------------------------- }
interface


uses
    Classes,
    TypeInfo,
    TypeDictionary,
    IntfLayoutElementGraphicsDriver;

type
    TLayoutElementGraphicsDriverTypeDictionary = class(TTypeDictionary)
    protected
        function IsValidType(aTypeInfo: TTypeInfo): boolean; override;
    end;

var
    gLayoutElementGraphicsDriverTypeDictionary: TLayoutElementGraphicsDriverTypeDictionary;

    // ##################################################################################################


implementation


function TLayoutElementGraphicsDriverTypeDictionary.IsValidType(aTypeInfo: TTypeInfo): boolean;
begin
    result := aTypeInfo is TLayoutElementGraphicsDriverTypeInfo;
end;


initialization


gLayoutElementGraphicsDriverTypeDictionary := TLayoutElementGraphicsDriverTypeDictionary.Create
    ('LayoutElementGraphicsDriver Type Dictionary');


finalization


gLayoutElementGraphicsDriverTypeDictionary.Free;


end.
