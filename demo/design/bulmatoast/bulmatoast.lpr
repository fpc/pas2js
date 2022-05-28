program bulmatoast;

{$mode objfpc}

uses
  JS, Classes, SysUtils, Web, hfgIndex, p2jsres;

begin
  SetResourceSource(rsJS);
  With TfrgmIndex.Create(Nil) do
    Show;
end.
