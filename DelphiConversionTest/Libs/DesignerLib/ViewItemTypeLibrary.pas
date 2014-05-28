{ ------------------------------------------------------------------------------------------------------------
  Copyright  2009 Zinsser Analytic GmbH - All rights reserved.
  Author       : Wolfgang Lyncke (wl)
  Description  :
  ------------------------------------------------------------------------------------------------------------
  Revision History:
  date     op  method                            track-no improvement/change
  -------- --  --------------------------------  -------- ----------------------------------------------------
  16.01.09 wl                                    TN4362   not yet a real type library, but it should be
  06.04.09 pk                                    TN4503   New: ntDisplayComponent
  16.06.09 wl                                    TN4606   alle Bezüge auf ntRunTable und RunEditor entfernt
  03.05.10 wl                                    TN5052   uses LayoutEditingViewItem
  09.06.10 wl                                    TN5116   neu: ntFavFolder
  23.02.11 wl                                    TN5486   neu: ImportViewItems
  14.12.11 wl                                    TN5765   ohne Session
  03.02.12 wl                                    TN5792   neue Namen: SubstanceSet,Substance statt ReagentRack,Reagent
  10.12.12 wl                                    TN6045   verwendet DesignerMethodViewItem
  14.12.12 wl                                    TN6054   ntVariable entfernt
  13.03.13 wl                                    TN5960   class statt record
  03.04.14 ts                                    TN6387   new ntMethodEditableInRunner
  ------------------------------------------------------------------------------------------------------------ }

unit ViewItemTypeLibrary;


interface


uses
    ViewItem;

type
    // Type Library (noch nicht fertig)
    TViewItemTypeLibrary = class
    public
        function CreateAnyViewItem(aViewType: TViewItemType; const aName: string): TViewItem;
    end;


implementation


uses
    SpecialViewItems,
    LayoutEditingViewItem,
    ImportViewItems,
    ModuleViewItem,
    DesignerMethodViewItem;

{ TViewItemTypeLibrary }

function TViewItemTypeLibrary.CreateAnyViewItem(aViewType: TViewItemType; const aName: string): TViewItem;
begin
    result := nil;
    case (aViewType) of
        ntFolder:
            result := TFolderViewItem.Create(aName);
        ntAction:
            result := TActionViewItem.Create(aName);
        ntRack:
            result := TRackViewItem.Create(aName);
        ntCarrier:
            result := TCarrierViewItem.Create(aName);
        ntWorkspace:
            result := TWorkspaceViewItem.Create(aName);
        ntTipType:
            result := TTipTypeViewItem.Create(aName);
        ntMethod:
            result := TDesignerMethodViewItem.Create(aName);
        ntMethodEditableInRunner:
            result := TDesignerMethodViewItem.Create(aName);
        ntLayout:
            result := TLayoutViewItem.Create(aName);
        ntSequence:
            result := TSequenceViewItem.Create(aName);
        ntWashProg:
            result := TWashProgViewItem.Create(aName);
        ntSubstance:
            result := TReagentViewItem.Create(aName);
        ntSubstanceSet:
            result := TSubstanceSetViewItem.Create(aName);
        ntPowderPar:
            result := TPowderParViewItem.Create(aName);
        ntLiquidPar:
            result := TLiquidParViewItem.Create(aName);
        ntSQLTerm:
            result := TSQLTermViewItem.Create(aName);
        ntDevice:
            result := TDeviceViewItem.Create(aName);
        ntDriver:
            result := TDriverViewItem.Create(aName);
        ntConnection:
            result := TConnectionViewItem.Create(aName);
        ntDisplayComponent:
            result := TDisplayComponentViewItem.Create(aName);
        ntTableImportDef:
            result := TTableImportDefViewItem.Create(aName);
        ntVarImportDef:
            result := TVarImportDefViewItem.Create(aName);
        ntImportFileDef:
            result := TImportFileDefViewItem.Create(aName);
        ntFavFolder:
            result := TFolderViewItem.Create(aName);
        else
            ASSERT(false, 'Node does not have a corresponding ViewItem');
    end;
end;


end.
