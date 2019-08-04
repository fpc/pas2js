program testwidgets;

{$mode objfpc}

uses
  browserconsole, {browsertestrunner} consoletestrunner, JS, Classes, SysUtils, Web, btnrun, tcWidget, tchtmlwidgets;

var
  Application : TTestRunner;

begin
  Application:=TTestRunner.Create(nil);
  Application.RunFormClass:=TConsoleRunner;
  Application.Initialize;
  Application.Run;
end.
