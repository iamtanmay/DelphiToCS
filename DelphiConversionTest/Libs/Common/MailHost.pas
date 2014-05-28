{ ------------------------------------------------------------------------------------------------------------
  Copyright © 2010 Zinsser Analytic GmbH - All rights reserved.
  Author       : Wolfgang Lyncke (wl)
  Description  :
  ------------------------------------------------------------------------------------------------------------
  Revision History:
  date     op  method                                track-no improvement/change
  -------- --  ------------------------------------  -------- ------------------------------------------------
  27.10.10 wl                                        TN5300   Initial Revision
  ------------------------------------------------------------------------------------------------------------ }

unit MailHost;


interface


uses
    GeneralTypes;

type
    TMailHost = class
    private
        fSendHostIP: string;
        fSendPort: integer;
        fMailAddress: string;
        fMailPassword: string;
        fActive: boolean;
    public
        constructor Create(const aSendHostIP: string; aSendPort: integer;
            const aMailAddress, aMailPassword: string);

        procedure SendMail(const aRecipients, aCCRecipients, aSubject: string; const aText: TStringArray);
        procedure SendMailWithJpeg(const aRecipients, aCCRecipients, aSubject: string;
            const aText: TStringArray; const aJpegFileName: string);

        property SendHostIP: string read fSendHostIP;
        property SendPort: integer read fSendPort;
        property MailAddress: string read fMailAddress;
        property MailPassword: string read fMailPassword;
        property Active: boolean read fActive write fActive;
    end;


implementation


uses
    SysUtils,
    IdText,
    IdMessage,
    IdSMTP,
    IdAttachment,
    IdAttachmentFile;

constructor TMailHost.Create(const aSendHostIP: string; aSendPort: integer;
    const aMailAddress, aMailPassword: string);
begin
    inherited Create;

    fActive := false;
    fSendHostIP := aSendHostIP;
    fSendPort := aSendPort;
    fMailAddress := aMailAddress;
    fMailPassword := aMailPassword;
end;

procedure TMailHost.SendMail(const aRecipients, aCCRecipients, aSubject: string; const aText: TStringArray);
var
    txtpart: TIdText;
    xMessage: TIdMessage;
    xIdSMTP: TIdSMTP;
    x: Integer;
begin
    if not fActive then
        EXIT;

    xIdSMTP := TIdSMTP.Create(nil);
    try
        xIdSMTP.Username := fMailAddress;
        xIdSMTP.Host := fSendHostIP;
        xIdSMTP.Password := fMailPassword;
        xIdSMTP.Port := fSendPort;
        xIdSMTP.AuthType := satDefault;

        xMessage := TIdMessage.Create(nil);
        xMessage.From.Address := fMailAddress;
        xMessage.Recipients.EMailAddresses := aRecipients;
        xMessage.CCList.EMailAddresses := aCCRecipients;
        xMessage.Subject := aSubject;

        txtpart := TIdText.Create(xMessage.MessageParts);
        // txtpart.ContentType := 'text/plain';
        for x := 0 to high(aText) do
        begin
            txtpart.Body.Add(aText[x]);
        end;

        try
            xIdSMTP.Connect();
            xIdSMTP.Send(xMessage);
        finally
            if xIdSMTP.Connected then
                try
                    xIdSMTP.Disconnect();
                except
                end;
        end;
    finally
        FreeAndNil(xMessage);
        FreeAndNil(xIdSMTP);
    end;
end;

procedure TMailHost.SendMailWithJpeg(const aRecipients, aCCRecipients, aSubject: string;
    const aText: TStringArray; const aJpegFileName: string);
var
    txtpart: TIdText;
    bmppart: TIdAttachment;
    xMessage: TIdMessage;
    xIdSMTP: TIdSMTP;
    x: Integer;
begin
    if not fActive then
        EXIT;

    xIdSMTP := TIdSMTP.Create(nil);
    try
        xIdSMTP.Username := fMailAddress;
        xIdSMTP.Host := fSendHostIP;
        xIdSMTP.Password := fMailPassword;
        xIdSMTP.Port := fSendPort;
        xIdSMTP.AuthType := satDefault;

        xMessage := TIdMessage.Create(nil);
        xMessage.From.Address := fMailAddress;
        xMessage.Recipients.EMailAddresses := aRecipients;
        xMessage.CCList.EMailAddresses := aCCRecipients;
        xMessage.ContentType := 'multipart/mixed';
        xMessage.Subject := aSubject;

        txtpart := TIdText.Create(xMessage.MessageParts);
        // txtpart.ContentType := 'text/plain';
        for x := 0 to high(aText) do
        begin
            txtpart.Body.Add(aText[x]);
        end;

        bmppart := TIdAttachmentfile.Create(xMessage.MessageParts, aJpegFileName);
        bmppart.ContentType := 'image/jpeg';
        bmppart.ContentDisposition := 'inline';
        bmppart.ExtraHeaders.Values['content-id'] := 'us.jpg';
        bmppart.DisplayName := 'us.jpg';

        try
            xIdSMTP.Connect();
            xIdSMTP.Send(xMessage);
        finally
            if xIdSMTP.Connected then
                try
                    xIdSMTP.Disconnect();
                except
                end;
        end;
    finally
        FreeAndNil(xMessage);
        FreeAndNil(xIdSMTP);
    end;
end;


end.
