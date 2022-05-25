program WasiDomTest1;

{$mode objfpc}
{$h+}
{$codepage UTF8}

uses
  SysUtils, JOB_WAsm, JOB_Shared;

type

  { TBird }

  TBird = class(TJSObject)
  private
    function GetSize: integer;
    procedure SetSize(const AValue: integer);
  public
    function GetDouble: double;
    function GetInteger: integer;
    property Size: integer read GetSize write SetSize;
  end;

{ TBird }

function TBird.GetSize: integer;
begin
  Result:=ReadJSPropertyLongInt('Size');
end;

procedure TBird.SetSize(const AValue: integer);
begin
  WriteJSPropertyLongInt('Size',AValue);
end;

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

  //obj.InvokeJSNoResult('Proc',[]);
  //d:=obj.InvokeJSDoubleResult('GetDouble',[u,12345678901]);
  Freddy:=obj.InvokeJSObjResult('CreateChick',['Freddy'],TBird) as TBird;
  writeln('AAA3 ');
  Freddy.Size:=81;
  writeln('AAA4 ');
  i:=Freddy.Size;
  writeln('AAA5 ',i);
  Freddy.Free;
  writeln('AAA6 ');
end.

