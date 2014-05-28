unit SeqenceManager;
{ --------------------------------------------------------------------------------------------------
  Copyright © 2004 Zinsser Analytic GmbH - All rights reserved.
  Author       : Wolfgang Lyncke (wl)
  Description  :
  --------------------------------------------------------------------------------------------------
  Revision History:
  date     op  method                       track-no improvement/change
  -------- --  ---------------------------  -------- -----------------------------------------------
  24.05.04 wl                               TN1945   initial version
  01.11.05 wl                               TN2541.0 Resource 2000 statt 56565
  10.08.09 wl                               TN4702   Strings werden jetzt direkt geladen
  -------------------------------------------------------------------------------------------------- }


interface


type
    TSequenceManager = class
    public
        class function ImportSequence: string;
        class function NewSequence: string;
    end;


implementation


uses
    Controls,
    Forms,
    Dialogs,
    GeneralTypes,
    Wizard,
    SeqenceImport1,
    SeqenceImport2,
    SeqenceNamePage,
    SeqenceDataAdaptor;

class function TSequenceManager.ImportSequence: string;
var
    xfrmWizard: TfrmWizard;
    xModalResult: TModalResult;
    xOpenDialog: TOpenDialog;
    xFileName: string;
begin
    result := '';

    // choose file
    xOpenDialog := TOpenDialog.Create(nil);
    xOpenDialog.Filter := TLanguageString.
        Read('Text files (*.TXT;*.ASC;*.CSV)|*.TXT;*.ASC;*.CSV|All files (*.*)|*.*|',
        'Textdateien (*.TXT;*.ASC;*.CSV)|*.TXT;*.ASC;*.CSV|Alle Dateien (*.*)|*.*|');
    if (not xOpenDialog.Execute) then
        exit;

    xFileName := xOpenDialog.FileName;
    xOpenDialog.Free;

    // Create a "Sequence Import" wizard
    xfrmWizard := TfrmWizard.Create(nil);
    xfrmWizard.Caption := TLanguageString.Read('Sequence import wizard', 'Sequenzimport-Assistent');
    xfrmWizard.Add(TSeqenceImportPage1.Create(xfrmWizard, xFileName));
    xfrmWizard.Add(TSeqenceImportPage2.Create(xfrmWizard));
    xfrmWizard.Add(TSeqenceNamePage.Create(xfrmWizard));

    xModalResult := xfrmWizard.ShowModal;
    if (xModalResult = mrOK) then
    begin
        result := (xfrmWizard.Items[2] as TSeqenceNamePage).SeqName;
        TSeqenceDataAdaptor.StoreGridData(result, (xfrmWizard.Items[1] as TSeqenceImportPage2).PreviewGrid,
            (xfrmWizard.Items[1] as TSeqenceImportPage2).Volume, (xfrmWizard.Items[2] as TSeqenceNamePage)
            .Rows, (xfrmWizard.Items[2] as TSeqenceNamePage).Cols);
    end;
    xfrmWizard.Free;
end;

class function TSequenceManager.NewSequence: string;
var
    xfrmWizard: TfrmWizard;
    xModalResult: TModalResult;
begin
    result := '';

    // Create a "Sequence Import" wizard
    xfrmWizard := TfrmWizard.Create(nil);
    xfrmWizard.Caption := TLanguageString.Read('Sequence import wizard', 'Sequenzimport-Assistent');
    xfrmWizard.Add(TSeqenceNamePage.Create(xfrmWizard));

    xModalResult := xfrmWizard.ShowModal;
    if (xModalResult = mrOK) then
    begin
        result := (xfrmWizard.Items[0] as TSeqenceNamePage).SeqName;
    end;
    xfrmWizard.Free;
end;


end.
