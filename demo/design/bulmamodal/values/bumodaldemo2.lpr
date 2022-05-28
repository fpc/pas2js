program bumodaldemo2;

{$mode objfpc}

uses
  browserapp, JS, Classes, SysUtils, Web, dmindex, p2jsres;

type
  TMyApplication = class(TBrowserApplication)
    procedure doRun; override;
  end;

procedure TMyApplication.doRun;

begin
  with ThfHello.Create(Self) do
    Show;
  Terminate;
end;

var
  Application : TMyApplication;

begin
  Application:=TMyApplication.Create(nil);
  Application.Initialize;
  Application.Run;
end.
