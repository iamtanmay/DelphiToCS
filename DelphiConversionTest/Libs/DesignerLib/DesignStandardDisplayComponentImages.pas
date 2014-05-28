unit DesignStandardDisplayComponentImages;
{ -----------------------------------------------------------------------------------------------------------------------
  Copyright © 2009 Zinsser Analytic GmbH - All rights reserved.
  Author       : Payman Kamali (pk)
  Description  :
  -----------------------------------------------------------------------------------------------------------------------
  Revision History:
  date     op  method                       track-no improvement/change
  -------- --  ---------------------------  -------- --------------------------------------------------------------------
  06.04.09 pk                                TN4503  Initial Revision
  20.05.10 wl                                TN5117   neue Schriftart "Segoe UI", Schriftgröße 9
  ----------------------------------------------------------------------------------------------------------------------- }


interface


uses
    Windows,
    Messages,
    SysUtils,
    Variants,
    Classes,
    Graphics,
    Controls,
    Forms,
    Dialogs,
    ExtCtrls,
    ImgList;

type
    TFrmDesignStandardDisplayComponentImages = class(TForm)
        Image1: TImage;
        Image2: TImage;
        Image3: TImage;
        Image4: TImage;
        Image5: TImage;
        procedure FormCreate(Sender: TObject);
    private
        { Private declarations }
    public
        { Public declarations }
    end;


implementation


{$R *.dfm}

uses
    ControlUtils;

procedure TFrmDesignStandardDisplayComponentImages.FormCreate(Sender: TObject);
begin
    TControlUtils.ResetFontForWinXP(self);
end;


end.
