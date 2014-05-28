{ -----------------------------------------------------------------------------------------------------------------------
  Copyright © 2008 Zinsser Analytic GmbH - All rights reserved.
  Author       : Payman Kamali (pk)
  Description  :
  -----------------------------------------------------------------------------------------------------------------------
  Revision History:
  date     op  method                                track-no  improvement/change
  -------- --  ------------------------------------- --------  ----------------------------------------------------------
  06.11.08 pk  IExecutable                           TN4280    New: Terminate
  09.03.10 pk  Terminate                             TN5015    paramter removed
  ----------------------------------------------------------------------------------------------------------------------- }

unit Executable;


interface


uses
    Classes,
    SysUtils;

type
    IExecutable = interface
        ['{8984B33A-B6FC-43F9-B2BB-FE851125B792}']
        procedure Execute();
        procedure Terminate();
    end;

    IExceptionHandler = interface
        ['{FF670F2F-4D4F-4999-A840-EB3ED26C6423}']
        procedure HandleException(aException: Exception);
    end;

    TExecutable = class(TInterfacedObject, IExecutable)
    protected
        fTerminated: boolean;
    public
        constructor Create();
        procedure Execute(); virtual; abstract;
        procedure Terminate(); virtual;
        property Terminated: boolean read fTerminated;
    end;

    TExecutableWithException = class(TExecutable, IExceptionHandler)
    public
        procedure HandleException(aException: Exception); virtual;
    end;


implementation


{ TExecutableWithException }

procedure TExecutableWithException.HandleException(aException: Exception);
begin
end;

{ TExecutable }

constructor TExecutable.Create;
begin
    inherited Create();
    fTerminated := false;
end;

procedure TExecutable.Terminate();
begin
    fTerminated := true;
end;


end.
