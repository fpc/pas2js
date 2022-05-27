program WasiDomTest1;

{$mode objfpc}
{$h+}
{$codepage UTF8}

uses
  SysUtils, JOB_WAsm, JOB_Shared, JOB_Web, JOB_JS;

type

  { TBird }

  TBird = class(TJSObject)
  private
    function GetName: string;
    function GetChild: TBird;
    function GetSize: integer;
    procedure SetName(const AValue: string);
    procedure SetChild(const AValue: TBird);
    procedure SetSize(const AValue: integer);
  public
    function GetDouble: double;
    function GetInteger: integer;
    property Name: string read GetName write SetName;
    property Size: integer read GetSize write SetSize;
    property Child: TBird read GetChild write SetChild;
  end;

{ TBird }

function TBird.GetName: string;
begin
  Result:=ReadJSPropertyUtf8String('Name');
end;

function TBird.GetChild: TBird;
begin
  Result:=ReadJSPropertyObject('Child',TBird) as TBird;
end;

function TBird.GetSize: integer;
begin
  Result:=ReadJSPropertyLongInt('Size');
end;

procedure TBird.SetName(const AValue: string);
begin
  WriteJSPropertyUtf8String('Name',AValue);
end;

procedure TBird.SetChild(const AValue: TBird);
begin
  WriteJSPropertyObject('Child',AValue);
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
  Freddy, Alice, aBird: TBird;
  i: Integer;
  JSValue: TJOB_JSValue;
  JSElem: IJSElement;
  aDate: IJSDate;
begin
  JSElem:=JSDocument.getElementById('playground');
  writeln('Class=',JSElem._ClassName);

  aDate:=JSDate.Create(2003,2,5,8,47,30,777);
  u:=aDate.toLocaleDateString;
  writeln('toLocaleDateString=',u);

  exit;

  obj:=TJSObject.CreateFromID(JObjIdBird);
  obj.WriteJSPropertyUnicodeString('Caption','Root');
  writeln('AAA1 ');
  u:='äbc';

  //obj.InvokeJSNoResult('Proc',[]);
  //d:=obj.InvokeJSDoubleResult('GetDouble',[u,12345678901]);
  writeln('Create Freddy...');
  Freddy:=obj.InvokeJSObjectResult('CreateChick',['Freddy'],TBird) as TBird;
  writeln('AAA5 ',Freddy.Name);

  writeln('Create Alice...');
  Alice:=obj.InvokeJSObjectResult('CreateChick',['Alice'],TBird) as TBird;
  writeln('Freddy.Child:=Alice...');
  Freddy.Child:=Alice;
  aBird:=Freddy.Child;
  writeln('Freddy.Child=',aBird.Name);

  //Freddy.Size:=123;
  //writeln('Freddy.Size=',Freddy.Size);
  JSValue:=Freddy.ReadJSPropertyValue('Child');
  writeln('JSValue: ',JSValue.Kind,' ',JSValue.AsString);

  writeln('Freeing Freddy...');
  Freddy.Free;
  writeln('Freeing Alice...');
  Alice.Free;
end.

