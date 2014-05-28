{ --------------------------------------------------------------------------------------------------
  Copyright © 2002 Zinsser Analytic GmbH - All rights reserved.
  Author       : Michael Ott (mo)
  Description  : Manages all internal data and data structure updates
  --------------------------------------------------------------------------------------------------
  Revision History:
  date     op  method                            track-no    improvement/change
  -------- --  ---------------------------       --------    -----------------------------------------------
  18.11.02 mo                                    TN1242      Initial Revision
  18.12.02 mo  V573_UpdateSophasReactionBlocks   TN1242      korrigiert
  20.12.02 mo  V573_UpdateSophasReactionBlocks   TN1242      endgültig korrigiert
  27.12.02 wl                                    TN1293.5    uses geändert
  05.03.03 tbh V574_ChangeScriptTableSeparator   TN1306      automat. Änderung bei Update
  05.03.03 tbh gmCheckDataForUpdate              TN1306      V574_ChangeScriptTableSeparator wird aufgerufen
  12.03.03 wl                                    TN1293.5    uses posTools
  08.12.03 pk                                    TN1697.1    The update functions no longer return boolean values.
  Exceptions are caught in the gmCheckDataForUpdate function.
  08.12.03 pk  V605_UpdateScriptTables           TN1697.1    Update the all tables that have to do with scheduling
  13.05.04 pk  V611_UpdateImportTables           TN1916      New: Updates import tables
  21.07.04 pk  V612_UpdateRunTable               TN2049      New: Updates Run table
  21.07.04 pk  V612_UpdateMethodTable            TN2049      New: Updates Method table
  17.11.04 pk                                    TN2231      uses RunDataAdaptorExt
  02.02.05 pk  V620_UpdateResSchemeTable         TN2302.1    New
  02.02.05 pk  V620_UpdateRunTable               TN2302.2    New field : ResID
  02.02.05 pk  V620_UpdateMethodTable            TN2302.2    New field : ResID
  15.02.05 pk  V620_UpdateSessionTable           TN2315      New
  08.06.05 tbh V621_UpdateVolcorrTable           TN2385      New
  01.07.05 pk                                    TN2487      Install...Table function name changed to InstallTable
  14.07.05 wl  V630_UpdateLiqClassTable          TN2506      Update für neue Spitback-Felder
  08.08.05 pk  V630_UpdateLiqClassTable          TN2523      Update SampleAspWasteVol field
  25.08.05 pk  V632_UpdateRunVarTable            TN2547      New
  31.08.05 wl  V632_UpdateMethImportDefsTable    TN2541.0    new
  01.09.05 thr                                   TN2587      wash record in liquid class
  01.09.05 pk  V632_UpdateCommandTable           TN2590      New - Flag field
  07.11.05 pk  V620_UpdateSessionTable           TN2737      New - Joined field
  08.11.05 wl  V574_ChangeScriptTableSeparator   TN2745      entfernt
  08.11.05 wl  V605_VerifyResourcesTable         TN2745      entspricht V605_UpdateScriptTables (umbenannt)
  24.11.05 pk  CopyTable                         TN2805      references to dbrack removed
  11.01.06 pk  StandardUpdate                    TN2871.1    New - Executes an update for a data adaptor
  11.01.06 pk  gmCheckDataForUpdate              TN2871.1    Update table for Liquid Handling
  19.01.06 pk  gmCheckDataForUpdate              TN2889      Update TImportColDefsDataAdaptor
  06.04.06 pk  gmCheckDataForUpdate              TN3024      MethodDataAdaptor and RunDataAdaptor use StandardUpdate
  05.05.06 pk  gmCheckDataForUpdate              TN3084      RunVarDataAdaptor use StandardUpdate
  15.05.06 pk  gmCheckDataForUpdate              TN3097      SessionDataAdaptor use StandardUpdate
  17.05.06 wl  StandardUpdate                    TN3107      Die alte Datei bekommt jetzt den kompletten Versionsnamen
  23.09.06 wl  gmCheckDataForUpdate              TN3326      mit TipTypeDataAdaptor
  23.09.06 wl  StandardUpdate                    TN3326      bei VerifyTable neuer Parameter
  05.02.07 pk  TUpdateManager                    TN3544      New. Use Updater to update tables
  22.02.07 pk  TUpdateManager.Destroy            TN3583      New
  22.02.07 pk  gmCheckDataForUpdate              TN3583      Free all queries
  28.02.07 wl  V573_UpdateSophasReactionBlocks   TN3603      entfernt
  16.04.07 wl  gmCheckDataForUpdate              TN3547      jetzt mit SQLTermsDataAdaptor
  24.07.07 pk  gmCheckDataForUpdate              TN3785      Update Parser
  25.07.07 wl  gmCheckDataForUpdate              TN3792      mit WashProgDataAdaptor
  07.08.07 wl                                    TN3811.2 uses geändert
  31.08.07 wl                                    TN3811.4    jetzt Teil von DataAdaptorCommon
  31.08.07 wl  gmCheckDataForUpdate              TN3811.4    --> AppInterfaceDataAdaptorCommon
  21.09.07 wl  AddUpdater                        TN3811.4    erzeugt DA-Instanzen: Funktion müsste nicht mehr statisch sein
  12.11.07 pk                                    TN3921      various changes
  27.02.13 wl                                    TN6045   uses geändert
  -------------------------------------------------------------------------------------------------- }

unit UpdateManager;


interface


uses
    Generics.Collections,
    Update;

type
    TOnAfterUpdate = procedure(aUpdate: TUpdate) of object;

    TUpdateManager = class
    private
        fUpdates: TObjectList<TUpdate>;
        fOnAfterUpdate: TOnAfterUpdate;
        procedure Update(aUpdate: TUpdate);
    public
        constructor Create();
        destructor Destroy(); override;
        procedure AddUpdate(aUpdate: TUpdate);
        procedure CheckUpdates();
        procedure PerformUpdates();
        property Updates: TObjectList<TUpdate>read fUpdates;
        property OnAfterUpdate: TOnAfterUpdate read fOnAfterUpdate write fOnAfterUpdate;
    end;

var
    gUpdateManager: TUpdateManager;


implementation


uses
    Forms;

{ TUpdateManager }

constructor TUpdateManager.Create;
begin
    inherited Create();
    fUpdates := TObjectList<TUpdate>.Create();
end;

destructor TUpdateManager.Destroy();
begin
    fUpdates.Free;
    inherited;
end;

procedure TUpdateManager.AddUpdate(aUpdate: TUpdate);
begin
    fUpdates.Add(aUpdate);
end;

procedure TUpdateManager.CheckUpdates();
var
    x: integer;
    xUpdate: TUpdate;
begin
    for x := 0 to fUpdates.Count - 1 do
    begin
        xUpdate := fUpdates[x];
        xUpdate.VersionCheck();
    end;
end;

procedure TUpdateManager.Update(aUpdate: TUpdate);
begin
    aUpdate.Update();
    if Assigned(self.fOnAfterUpdate) then
        self.fOnAfterUpdate(aUpdate);
end;

procedure TUpdateManager.PerformUpdates();
var
    x: integer;
begin
    for x := 0 to fUpdates.Count - 1 do
    begin
        Update(fUpdates[x] as TUpdate);
    end;
end;


end.
