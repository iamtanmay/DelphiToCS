unit LocalIniFile;
{ --------------------------------------------------------------------------------------------------
  Copyright © 2002 Zinsser Analytic GmbH - All rights reserved.
  Author       : Wolfgang Lyncke (wl)
  Project      : ZACommon.dll
  Description  : This class represents the file 'APPDATA.TMP' that stores temporary local data
  --------------------------------------------------------------------------------------------------
  Revision History:
  Date     op  Method                       Track-no Improvement/Change
  -------- --  ---------------------------  -------- -----------------------------------------------
  12.02.03 wl                               TN1293.5 initial version
  29.07.04 wl  Read/WriteControlBounds      TN2007.1 Aufrufe ohne TControl, da nicht Versionskompatibel
  23.09.04 wl  ReadControlBounds            TN2148   Wenn ReadBounds = false, wird die Größe des Fensters nicht mehr geändert
  25.05.05 wl  Read/WriteInteger,Float,Boolean  TN2427 Neu
  31.08.07 wl                               TN3811.4 Items entfernt weil property = default
  06.08.09 wl                               TN4702   Strings werden direkt geladen
 26.10.09 wl                                TN4831   IConfigurationSet replaces TIniFile
 -------------------------------------------------------------------------------------------------- }

interface

uses Types, ConfigurationFile,
    CommonTypes, CompositeIniEntry;

type

    TRectEntry = class(TCompositeIniEntry)
      private
        const STR_RectENTRY_DELIMITER = ';'; // !!!
        function GetRectValue: TRect;
        procedure SetRectValue(aRect: TRect);
      public
        // Constructor/Destructor
        constructor Create(aSection, aIdent: string; aRect: TRect);
        constructor CreateWithoutDefault(aSection, aIdent: string);
        // Properties
        property RectValue: TRect read GetRectValue write SetRectValue;
    end;


    ILocalConfigurationSet = interface(IConfigurationSet)
    ['{69107941-8FC2-4CE3-BF1C-FF1AC5EABF5E}']
        procedure ReadControlBounds(var aBoundsRect: TRect; aName: string; aReadSize: boolean; aIsEditor: boolean);
        procedure WriteControlBounds(aBoundsRect: TRect; aName: string; aIsEditor: boolean);
    end;

    TLocalIniFile = class(TConfigurationFile, ILocalConfigurationSet)
      private
        const STR_ISEC_POS_EDIT = 'FormPosition_ED';
        const STR_ISEC_POS_RUN = 'FormPosition_SA';
      public
        procedure ReadControlBounds(var aBoundsRect: TRect; aName: string;
            aReadSize: boolean; aIsEditor: boolean);
        procedure WriteControlBounds(aBoundsRect: TRect; aName: string;
            aIsEditor: boolean);
    end;


implementation

uses
    IniEntry;

{ TRectEntry }

function TRectEntry.GetRectValue: TRect;
begin
    result.Left := (FSubEntries[0] as TIntegerEntry).IntegerValue;
    result.Top := (FSubEntries[1] as TIntegerEntry).IntegerValue;
    result.Right := (FSubEntries[2] as TIntegerEntry).IntegerValue;
    result.Bottom := (FSubEntries[3] as TIntegerEntry).IntegerValue;
end;

procedure TRectEntry.SetRectValue(aRect: TRect);
begin
    (FSubEntries[0] as TIntegerEntry).IntegerValue := aRect.Left;
    (FSubEntries[1] as TIntegerEntry).IntegerValue := aRect.Top;
    (FSubEntries[2] as TIntegerEntry).IntegerValue := aRect.Right;
    (FSubEntries[3] as TIntegerEntry).IntegerValue := aRect.Bottom;
end;

constructor TRectEntry.Create(aSection, aIdent: string; aRect: TRect);
begin
    inherited Create(aSection, aIdent, STR_RectENTRY_DELIMITER, '');
    AddInteger(aRect.Left, '', '');
    AddInteger(aRect.Top, '', '');
    AddInteger(aRect.Right, '', '');
    AddInteger(aRect.Bottom, '', '');
end;

constructor TRectEntry.CreateWithoutDefault(aSection, aIdent: string);
var
    x: integer;
begin
    inherited CreateWithoutDefault(aSection, aIdent, STR_RectENTRY_DELIMITER);
    for x := 0 to 3 do
        AddIntegerWithoutDefault;
end;

{ TLocalIniFile }

procedure TLocalIniFile.ReadControlBounds(var aBoundsRect: TRect;
    aName: string; aReadSize: boolean; aIsEditor: boolean);
var
    xIniEntry: TRectEntry;
    xBounds: TRect;
    xSection: string;
    xHeight, xWidth: integer;
begin
    if (aIsEditor) then
        xSection := STR_ISEC_POS_EDIT
    else
        xSection := STR_ISEC_POS_RUN;

    // read size and position from ini entry
    xIniEntry := TRectEntry.Create(xSection, aName, aBoundsRect);
    xIniEntry.Value := ReadString(xIniEntry.Section, xIniEntry.Ident,
        xIniEntry.Value);
    if (xIniEntry.DefaultValue = xIniEntry.Value) then
        exit; // exit if there is no stored value

    xBounds := xIniEntry.RectValue;
    xIniEntry.Free;

    // set control size and position
    if (aReadSize) then
        aBoundsRect := xBounds
    else
    begin
        xWidth := aBoundsRect.Right - aBoundsRect.Left;
        xHeight := aBoundsRect.Bottom - aBoundsRect.Top;
        aBoundsRect.Left := xBounds.Left;
        aBoundsRect.Top := xBounds.Top;
        aBoundsRect.Right := xBounds.Left + xWidth;
        aBoundsRect.Bottom := xBounds.Top + xHeight;
    end;
end;

procedure TLocalIniFile.WriteControlBounds(aBoundsRect: TRect; aName: string;
    aIsEditor: boolean);
var
    xIniEntry: TRectEntry;
    xSection: string;
begin
    if (aIsEditor) then
        xSection := STR_ISEC_POS_EDIT
    else
        xSection := STR_ISEC_POS_RUN;

    // write size and position to ini entry
    xIniEntry := TRectEntry.CreateWithoutDefault(xSection, aName);
    xIniEntry.RectValue := aBoundsRect;
    WriteString(xIniEntry.Section, xIniEntry.Ident, xIniEntry.Value);
    xIniEntry.Free;
end;

end.
