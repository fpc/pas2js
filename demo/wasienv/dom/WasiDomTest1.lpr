program WasiDomTest1;

{$mode objfpc}
{$h+}
{$codepage UTF8}

uses
  SysUtils, wadom_wasm, wadom_shared;

var
  obj: TJSObject;
  d: Double;
  u: UnicodeString;
begin
  obj:=TJSObject.CreateFromID(WasiObjIdBird);
  writeln('AAA1 ');
  obj.InvokeJSNoResult('Proc',[]);
  writeln('AAA2 ');
  exit;

  u:='äbc';
  d:=obj.InvokeJSDoubleResult('GetDouble',[u,12345678901]);
  writeln('AAA3 ',d);
end.

