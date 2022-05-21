program dbelementactions;

{$mode objfpc}

uses
  JS, Classes, SysUtils, Web, fgmIndex, localjsondataset;

begin
  With THFGMindex.Create(Nil) do
    begin
    UseProjectHTMLFile:=True;
    Show;
    end;
end.
