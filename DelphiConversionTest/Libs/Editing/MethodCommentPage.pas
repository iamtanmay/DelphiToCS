unit MethodCommentPage;
{ --------------------------------------------------------------------------------------------------
  Copyright © 2005 Zinsser Analytic GmbH - All rights reserved.
  Author       : Wolfgang Lyncke (wl)
  Description  : Wizard page for defining comments assigned to a method
  --------------------------------------------------------------------------------------------------
  Revision History:
  date     op  method                       track-no  improvement/change
  -------- --  ---------------------------  --------  ----------------------------------------------
  05.10.05 wl                               TN2575    initial version
  03.08.09 wl                               TN4702   Strings werden jetzt direkt geladen
  04.11.09 pk                               TN4843   Replace TObjectList, TStringList, TStrings with Generics Lists and TStringArray
  -------------------------------------------------------------------------------------------------- }


interface


uses
    StdCtrls,
    Classes,
    Controls,
    Wizard,
    GeneralTypes;

type
    TMethodCommentPage = class(TWizardPage)
    private
        FActiveControl: TWinControl;
        FStaticText1: TStaticText;
        FCmbCmShort: TEdit;
        FCmbCmLong: TEdit;
        function GetCommentLong: string;
        function GetCommentShort: string;
    public
        // constructor
        constructor Create(aOwner: TComponent; const aMethodName, aCommentShort, aCommentLong: string);
            reintroduce;
        // methods derived from TWizardPage
        function GetHeaderTitle: string; override;
        function GetHeaderText(): TStringArray; override;
        function CheckBeforeNext: boolean; override;
        function GetActiveControl: TWinControl; override;
        procedure SetUp(aPreviousPage: TWizardPage); override;
        // properties
        property CommentShort: string read GetCommentShort;
        property CommentLong: string read GetCommentLong;
    end;


implementation


constructor TMethodCommentPage.Create(aOwner: TComponent;
    const aMethodName, aCommentShort, aCommentLong: string);
begin
    inherited Create(aOwner);

    AddLabel(TLanguageString.Read('Method name:', 'Methodenname:'), INT_WIZARD_LEFT_1);
    FStaticText1 := self.AddStaticText(INT_WIZARD_LEFT_EDIT, INT_WIZARD_WIDTH_EDIT);
    FStaticText1.Caption := aMethodName;
    AddDistance(wzdLarge);

    AddLabel(TLanguageString.Read('Comment:', 'Kommentar:'), INT_WIZARD_LEFT_1);
    AddDistance(wzdSmall);
    FCmbCmShort := self.AddEdit(INT_WIZARD_LEFT_1, INT_WIZARD_WIDTH_1);
    FCmbCmShort.Text := aCommentShort;
    AddDistance(wzdMedium);

    AddLabel(TLanguageString.Read('Additional comment:', 'Zusätzlicher Kommentar:'), INT_WIZARD_LEFT_1);
    AddDistance(wzdSmall);
    FCmbCmLong := self.AddEdit(INT_WIZARD_LEFT_1, INT_WIZARD_WIDTH_1);
    FCmbCmLong.Text := aCommentLong;

    FActiveControl := FCmbCmShort;
end;

function TMethodCommentPage.GetHeaderTitle: string;
begin
    result := TLanguageString.Read('Change Method Comments', 'Methodenkommentare ändern');
end;

function TMethodCommentPage.GetHeaderText(): TStringArray;
begin
    self.GetSingleTextAsHeader(TLanguageString.Read('You can change the comments assigned to the method',
        'Sie können die der Methode zugeordneten Kommentare ändern'));
end;

function TMethodCommentPage.CheckBeforeNext: boolean;
begin
    result := true;
end;

function TMethodCommentPage.GetActiveControl: TWinControl;
begin
    result := FActiveControl;
end;

procedure TMethodCommentPage.SetUp(aPreviousPage: TWizardPage);
// var
// xDA: TLayoutDataAdaptor;
begin
    inherited;

    // xDA := TLayoutDataAdaptor.Instance();
    // xDA.GetLayoutNames( FNameCombo.Items );
end;

function TMethodCommentPage.GetCommentLong: string;
begin
    result := FCmbCmLong.Text;
end;

function TMethodCommentPage.GetCommentShort: string;
begin
    result := FCmbCmShort.Text;
end;


end.
