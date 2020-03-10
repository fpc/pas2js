program testwidgets;

{$mode objfpc}

uses
  browserconsole, consoletestrunner, JS, Classes, SysUtils, Web, btnrun, tcWidget, tchtmlwidgets, tcdbhtmlwidgets, tcdbwidgets;

var
  Application : TTestRunner;

begin
  Application:=TTestRunner.Create(nil);
  Application.RunFormClass:=TConsoleRunner;
  Application.Initialize;
  Application.Run;
end.
