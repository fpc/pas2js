program WasiDomTest1;

{$mode objfpc}
{$h+}
{$codepage UTF8}

uses
  SysUtils, wadom_wasm;

var
  obj: TJSObject;
  d: Double;
  u: UnicodeString;
begin
  obj:=TJSObject.CreateFromID(-1);
  d:=obj.InvokeJSDoubleResult('fly',[]);
  writeln('AAA1 ',d);
  u:='äbc';
  d:=obj.InvokeJSDoubleResult('fly',[u,12345678901]);
  writeln('AAA2 ',d);
end.

