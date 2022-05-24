program WasiDomTest1;

{$mode objfpc}
{$h+}
{$codepage UTF8}

uses
  SysUtils, JOB_WAsm, JOB_Shared;

type

  { TBird }

  TBird = class(TJSObject)
  public
    function GetDouble: double;
  end;

{ TBird }

function TBird.GetDouble: double;
begin
  Result:=InvokeJSDoubleResult('GetDouble',[]);
end;

var
  obj: TJSObject;
  d: Double;
  u: UnicodeString;
  Freddy: TBird;
begin
  obj:=TJSObject.CreateFromID(WasiObjIdBird);
  writeln('AAA1 ');
  u:='äbc';

  u:=obj.InvokeJSUnicodeStringResult('GetString',[u]);
  writeln('AAA2 u="',u,'"');

  exit;

  //obj.InvokeJSNoResult('Proc',[]);
  //d:=obj.InvokeJSDoubleResult('GetDouble',[u,12345678901]);
  Freddy:=obj.InvokeJSObjResult('CreateChick',TBird,['Freddy']) as TBird;
  writeln('AAA3 ');
  d:=Freddy.GetDouble;
  writeln('AAA4 ',d);
  Freddy.Free;
  writeln('AAA5 ');
end.

