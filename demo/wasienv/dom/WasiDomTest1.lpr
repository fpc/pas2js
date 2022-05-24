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
    function GetInteger: integer;
  end;

{ TBird }

function TBird.GetDouble: double;
begin
  Result:=InvokeJSDoubleResult('GetDouble',[]);
end;

function TBird.GetInteger: integer;
begin
  Result:=InvokeJSLongIntResult('GetInteger',[]);
end;

var
  obj: TJSObject;
  d: Double;
  u: UnicodeString;
  Freddy: TBird;
  i: Integer;
begin
  obj:=TJSObject.CreateFromID(WasiObjIdBird);
  writeln('AAA1 ');
  u:='äbc';

  u:=obj.InvokeJSUnicodeStringResult('GetString',[u]);
  writeln('AAA2 u="',u,'"');

  //obj.InvokeJSNoResult('Proc',[]);
  //d:=obj.InvokeJSDoubleResult('GetDouble',[u,12345678901]);
  Freddy:=obj.InvokeJSObjResult('CreateChick',TBird,['Freddy']) as TBird;
  writeln('AAA3 ');
  i:=Freddy.GetInteger;
  writeln('AAA4 ',i);
  Freddy.Free;
  writeln('AAA5 ');
end.

