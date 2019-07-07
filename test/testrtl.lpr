program testrtl;

{$mode objfpc}

uses
  browserconsole, {browsertestrunner} consoletestrunner, JS, Classes, SysUtils, Web, frmrtlrun, tcstream, tccompstreaming, simplelinkedlist;

var
  Application : TTestRunner;

begin
  Application:=TTestRunner.Create(nil);
  Application.RunFormClass:=TConsoleRunner;
  Application.Initialize;
  Application.Run;
//  Application.Free;
end.
