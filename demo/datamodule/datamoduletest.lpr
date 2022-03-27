program datamoduletest;

{$mode objfpc}

uses
  JS, Classes, SysUtils, Web, dmtest, htmlactions, htmleventnames;



begin
  With TTestModule.Create(Nil) do
    DoSomething;
end.
