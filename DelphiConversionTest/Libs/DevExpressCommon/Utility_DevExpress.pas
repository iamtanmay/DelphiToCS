{ --------------------------------------------------------------------------------------------------
  Copyright © 2005 Zinsser Analytic GmbH - All rights reserved.
  Project      :
  Author       : Payman Kamali (pk)
  Description  : This is a utility unit for Developer Express.
  This code should only contain GENERAL code that can be used for ANY project
  Any classes which are inherited from a Developer Express class should be placed in here
  --------------------------------------------------------------------------------------------------
  Revision History:
  date     op  method                       track-no  improvement/change
  -------- --  ---------------------------  --------  ----------------------------------------------
  04.12.05 pk                                TN2828   initial version
  04.11.09 pk                                TN4843   Replace TObjectList, TStringList, TStrings with Generics Lists and TStringArray
  06.03.13 wl  AddValuesToComboBoxProps      TN6103   zunächst wird die Liste gelöscht
  -------------------------------------------------------------------------------------------------- }

unit Utility_DevExpress;


interface


uses
    Classes,
    cxEdit,
    cxEditRepositoryItems,
    cxDropDownEdit,
    cxContainer;

type

    // This is an editor which has a dropdown editor as well as an extra button
    // It is similar to the MRUEdit unit, WITHOUT the MRU (most recently used) behaviour
    TcxUtils = class
    public
        class procedure AddValuesToComboBoxProps(const aValues: array of string;
            const aProps: TcxCustomComboBoxProperties);
    end;

    TcxCustomComboButtonProperties = class(TcxCustomComboBoxProperties)
    protected
        FOnButtonClick: TNotifyEvent;
    public
        constructor Create(AOwner: TPersistent); override;
        procedure Assign(Source: TPersistent); override;
        procedure Update(AProperties: TcxCustomEditProperties); override;
        class function GetContainerClass: TcxContainerClass; override;
        property OnButtonClick: TNotifyEvent read FOnButtonClick write FOnButtonClick;
        property Items;
        property MaxLength;
    end;

    TcxCustomComboButton = class(TcxCustomComboBox)
    private
        function GetProperties: TcxCustomComboButtonProperties;
        procedure SetProperties(const Value: TcxCustomComboButtonProperties);
        function GetActiveProperties: TcxCustomComboButtonProperties;
    protected
        procedure DoButtonClick(AButtonVisibleIndex: Integer); override;

    public
        class function GetPropertiesClass: TcxCustomEditPropertiesClass; override;
        property ActiveProperties: TcxCustomComboButtonProperties read GetActiveProperties;
        property Properties: TcxCustomComboButtonProperties read GetProperties write SetProperties;

    end;

    TcxEditRepositoryComboButtonItem = class(TcxEditRepositoryItem)
    private
        function GetProperties: TcxCustomComboButtonProperties;
        procedure SetProperties(Value: TcxCustomComboButtonProperties);
    public
        class function GetEditPropertiesClass: TcxCustomEditPropertiesClass; override;
    published
        property Properties: TcxCustomComboButtonProperties read GetProperties write SetProperties;
    end;


implementation


{ TcxCustomComboButtonProperties }

constructor TcxCustomComboButtonProperties.Create(AOwner: TPersistent);
begin
    inherited;
    Buttons.Add;
    GlyphButtonIndex := 1;
    Buttons[1].Kind := bkEllipsis;
    Buttons[1].Default := True;
end;

class function TcxCustomComboButtonProperties.GetContainerClass: TcxContainerClass;
begin
    Result := TcxCustomComboButton;
end;

procedure TcxCustomComboButtonProperties.Update(AProperties: TcxCustomEditProperties);
begin
    if AProperties is TcxCustomComboButtonProperties then
        TcxCustomComboButtonProperties(AProperties).Items.Assign(Items);
end;

procedure TcxCustomComboButtonProperties.Assign(Source: TPersistent);
begin
    if Source is TcxCustomComboButtonProperties then
    begin
        BeginUpdate;
        try
            inherited Assign(Source);
            with TcxCustomComboButtonProperties(Source) do
            begin
                Self.OnButtonClick := OnButtonClick;
            end;
        finally
            EndUpdate
        end
    end
    else
        inherited Assign(Source);
end;
{ TcxEditRepositoryComboButtonItem }

class function TcxEditRepositoryComboButtonItem.GetEditPropertiesClass: TcxCustomEditPropertiesClass;
begin
    Result := TcxCustomComboButtonProperties;
end;

function TcxEditRepositoryComboButtonItem.GetProperties: TcxCustomComboButtonProperties;
begin
    Result := inherited Properties as TcxCustomComboButtonProperties;
end;

procedure TcxEditRepositoryComboButtonItem.SetProperties(Value: TcxCustomComboButtonProperties);
begin
    inherited Properties := Value;
end;

procedure TcxCustomComboButton.DoButtonClick(AButtonVisibleIndex: Integer);
begin
    if AButtonVisibleIndex = 1 then
    begin
        with Properties do
            if Assigned(OnButtonClick) then
            begin
                OnButtonClick(Self);
            end;
        if RepositoryItem <> nil then
            with ActiveProperties do
                if Assigned(OnButtonClick) then
                    OnButtonClick(Self);
    end;
end;

function TcxCustomComboButton.GetProperties: TcxCustomComboButtonProperties;
begin
    result := TcxCustomComboButtonProperties(fProperties);
end;

procedure TcxCustomComboButton.SetProperties(const Value: TcxCustomComboButtonProperties);
begin
    fProperties.Assign(Value);
end;

function TcxCustomComboButton.GetActiveProperties: TcxCustomComboButtonProperties;
begin
    result := TcxCustomComboButtonProperties(InternalGetActiveProperties);
end;

class function TcxCustomComboButton.GetPropertiesClass: TcxCustomEditPropertiesClass;
begin
    result := TcxCustomComboButtonProperties;
end;

{ TcxUtils }

class procedure TcxUtils.AddValuesToComboBoxProps(const aValues: array of string;
    const aProps: TcxCustomComboBoxProperties);
var
    x: integer;
begin
    aProps.Items.Clear;
    for x := 0 to Length(aValues) - 1 do
        aProps.Items.Add(aValues[x]);

end;


end.
