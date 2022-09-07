program pas2jsthreadworker;

{$mode objfpc}

uses
  Classes, WasiWorkerThreadHost;

type
  { TApplication }

  TApplication = class(TWorkerWASIHostApplication)
  end;

{ TApplication }

var
  App: TApplication;

begin
  App:=TApplication.Create(nil);
  App.Run;
end.
