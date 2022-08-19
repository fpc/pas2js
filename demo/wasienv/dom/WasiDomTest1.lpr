library WasiDomTest1;

{$mode objfpc}
{$h+}
{$codepage UTF8}
{$WARN 5028 off : Local $1 "$2" is not used}

uses
  Math, SysUtils, JOB_Shared, JOB_Web, JOB_JS;

type
  EWasiTest = class(Exception);

  TJSBird = class;

  { IJSBird }

  IJSBird = interface(IJSObject)
    ['{BABEA093-411B-4E30-8C56-53C11060BF5D}']
    // functions
    procedure IncSize;
    function CreateBird(const aName: string): IJSBird;
    // properties
    function GetCaption: UnicodeString;
    function GetEnabled: boolean;
    function GetName: string;
    function GetChild: IJSBird;
    function GetScale: double;
    function GetSize: integer;
    procedure SetCaption(const AValue: UnicodeString);
    procedure SetEnabled(const AValue: boolean);
    procedure SetName(const AValue: string);
    procedure SetChild(const AValue: IJSBird);
    procedure SetScale(const AValue: double);
    procedure SetSize(const AValue: integer);
    property Enabled: boolean read GetEnabled write SetEnabled;
    property Name: string read GetName write SetName;
    property Caption: UnicodeString read GetCaption write SetCaption;
    property Size: integer read GetSize write SetSize;
    property Scale: double read GetScale write SetScale;
    property Child: IJSBird read GetChild write SetChild;
  end;

  { TJSBird }

  TJSBird = class(TJSObject,IJSBird)
  private
  public
    // functions
    procedure IncSize;
    function CreateBird(const aName: string): IJSBird;
    // properties
    function GetCaption: UnicodeString;
    function GetEnabled: boolean;
    function GetName: string;
    function GetChild: IJSBird;
    function GetScale: double;
    function GetSize: integer;
    procedure SetCaption(const AValue: UnicodeString);
    procedure SetEnabled(const AValue: boolean);
    procedure SetName(const AValue: string);
    procedure SetChild(const AValue: IJSBird);
    procedure SetScale(const AValue: double);
    procedure SetSize(const AValue: integer);
    property Enabled: boolean read GetEnabled write SetEnabled;
    property Name: string read GetName write SetName;
    property Caption: UnicodeString read GetCaption write SetCaption;
    property Size: integer read GetSize write SetSize;
    property Scale: double read GetScale write SetScale;
    property Child: IJSBird read GetChild write SetChild;
  end;

  { TWasmApp }

  TWasmApp = class
  private
    function OnPlaygroundClick(Event: IJSEvent): boolean;
  public
    Prefix: string;
    Bird: IJSBird;
    procedure Run;
    procedure Fail(const Msg: string);
    procedure AssertEqual(const Msg: string; const Expected, Actual: boolean);
    procedure AssertEqual(const Msg: string; const Expected, Actual: integer);
    procedure AssertEqual(const Msg: string; const Expected, Actual: double);
    procedure AssertEqual(const Msg: string; const Expected, Actual: String);
    procedure AssertEqual(const Msg: string; const Expected, Actual: UnicodeString);
    procedure AssertEqual(const Msg: string; const Expected, Actual: IJSBird);
  public
    // read/write properties
    procedure TestBooleanProperty;
    procedure TestIntegerProperty;
    procedure TestDoubleProperty;
    procedure TestStringProperty;
    procedure TestObjectProperty;
    // todo procedure TestVariantProperty;

    // function
    // todo procedure TestCallProcedure;
  end;


{ TApplication }

function TWasmApp.OnPlaygroundClick(Event: IJSEvent): boolean;
var
  w: TJOBResult;
begin
  writeln('TWasmApp.OnPlaygroundClick ');
  w:=Event.InvokeJSTypeOf('targetElement',[]);

  writeln('TWasmApp.OnPlaygroundClick typeof=',w);

  Result:=true;
end;

procedure TWasmApp.Run;
var
  JSElem: IJSElement;
begin
  Bird:=TJSBird.JOBCreateGlobal('Bird') as IJSBird;

  //TestBooleanProperty;
  //TestIntegerProperty;
  //TestDoubleProperty;
  //TestStringProperty;
  TestObjectProperty;

  exit;

  JSElem:=JSDocument.getElementById('playground');
  writeln('TWasmApp.Run playground classname=',JSElem.className_);

  writeln('TWasmApp.Run addEventListener click...');
  JSElem.addEventListener('click',@OnPlaygroundClick);
  writeln('TWasmApp.Run ');

  exit;

 { obj:=TJSObject.JOBCreateGlobal('Bird');
  obj.WriteJSPropertyUnicodeString('Caption','Root');
  writeln('AAA1 ');
  //u:='äbc';

  //obj.InvokeJSNoResult('Proc',[]);
  //d:=obj.InvokeJSDoubleResult('GetDouble',[u,12345678901]);
  writeln('Create Freddy...');
  Freddy:=obj.InvokeJSObjectResult('CreateChick',['Freddy'],TJSBird) as TJSBird;
  writeln('AAA5 ',Freddy.Name);

  writeln('Create Alice...');
  Alice:=obj.InvokeJSObjectResult('CreateChick',['Alice'],TJSBird) as TJSBird;
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
}
end;

procedure TWasmApp.TestBooleanProperty;
begin
  Prefix:='TWasmApp.TestBooleanProperty';
  Bird.Enabled:=true;
  AssertEqual('Bird.Enabled:=true',true,Bird.Enabled);
  Bird.Enabled:=false;
  AssertEqual('Bird.Enabled:=false',false,Bird.Enabled);
end;

procedure TWasmApp.TestIntegerProperty;
begin
  Prefix:='TWasmApp.TestIntegerProperty';
  Bird.Size:=3;
  AssertEqual('Bird.Size:=3',3,Bird.Size);
  Bird.Size:=-13;
  AssertEqual('Bird.Size:=-13',-13,Bird.Size);
  Bird.Size:=High(longint);
  AssertEqual('Bird.Size:=High(longint)',High(longint),Bird.Size);
  Bird.Size:=Low(longint);
  AssertEqual('Bird.Size:=Low(longint)',Low(longint),Bird.Size);
end;

procedure TWasmApp.TestDoubleProperty;
begin
  Prefix:='TWasmApp.TestDoubleProperty';
  Bird.Scale:=0.3;
  AssertEqual('Bird.Scale:=0.3',0.3,Bird.Scale);
  Bird.Scale:=-0.3;
  AssertEqual('Bird.Scale:=-0.3',-0.3,Bird.Scale);
  Bird.Scale:=NaN;
  if not IsNan(Bird.Scale) then
    Fail('Bird.Scale:=NaN');
  Bird.Scale:=Infinity;
  AssertEqual('Bird.Scale:=Infinity',Infinity,Bird.Scale);
  Bird.Scale:=NegInfinity;
  AssertEqual('Bird.Scale:=NegInfinity',NegInfinity,Bird.Scale);
  Bird.Scale:=0.12345678901234;
  AssertEqual('Bird.Scale:=0.12345678901234',0.12345678901234,Bird.Scale);
end;

procedure TWasmApp.TestStringProperty;
begin
  Prefix:='TWasmApp.TestStringProperty';
  Bird.Name:='';
  AssertEqual('Bird.Name:=''''','',Bird.Name);
  Bird.Name:='a';
  AssertEqual('Bird.Name:=''a''','a',Bird.Name);
  Bird.Name:='abc';
  AssertEqual('Bird.Name:=''abc''','abc',Bird.Name);
  Bird.Name:=#13;
  AssertEqual('Bird.Name:=#13',#13,Bird.Name);
  Bird.Name:='ä';
  AssertEqual('Bird.Name:=''ä''','ä',Bird.Name);
  Bird.Name:='🎉';
  AssertEqual('Bird.Name:=''🎉''','🎉',Bird.Name);
end;

procedure TWasmApp.TestObjectProperty;
var
  Lisa, Bart: IJSBird;
begin
  Prefix:='TWasmApp.TestObjectProperty';
  Bird.Child:=nil;
  AssertEqual('Bird.Child:=nil',nil,Bird.Child);
  Lisa:=Bird.CreateBird('Lisa');
  AssertEqual('Lisa','Root.Lisa',Lisa.Name);
  Bart:=Bird.CreateBird('Bart');
  AssertEqual('Bart','Root.Bart',Bart.Name);
  Bird.Child:=Lisa;
  AssertEqual('Bird.Child:=Lisa',Lisa,Bird.Child);
end;

procedure TWasmApp.Fail(const Msg: string);
begin
  writeln('TWasmApp.Fail ',Prefix+': '+Msg);
  raise EWasiTest.Create(Prefix+': '+Msg);
end;

procedure TWasmApp.AssertEqual(const Msg: string; const Expected,
  Actual: boolean);
begin
  if Expected=Actual then exit;
  Fail(Msg+'. Expected '+BoolToStr(Expected,'True','False')+', but got '+BoolToStr(Actual,'True','False'));
end;

procedure TWasmApp.AssertEqual(const Msg: string; const Expected,
  Actual: integer);
begin
  if Expected=Actual then exit;
  Fail(Msg+'. Expected '+IntToStr(Expected)+', but got '+IntToStr(Actual));
end;

procedure TWasmApp.AssertEqual(const Msg: string; const Expected, Actual: double
  );
begin
  if Expected=Actual then exit;
  Fail(Msg+'. Expected '+FloatToStr(Expected)+', but got '+FloatToStr(Actual));
end;

procedure TWasmApp.AssertEqual(const Msg: string; const Expected, Actual: String
  );
begin
  if Expected=Actual then exit;
  Fail(Msg+'. Expected "'+Expected+'", but got "'+Actual+'"');
end;

procedure TWasmApp.AssertEqual(const Msg: string; const Expected,
  Actual: UnicodeString);
begin
  if Expected=Actual then exit;
  Fail(Msg+'. Expected "'+string(Expected)+'", but got "'+string(Actual)+'"');
end;

procedure TWasmApp.AssertEqual(const Msg: string; const Expected,
  Actual: IJSBird);
var
  ExpName, ActName: String;
begin
  if Expected=Actual then exit;
  if Expected=nil then
    Fail(Msg+'. Expected nil, but got Name="'+string(Actual.Name)+'"');
  if Actual=nil then
    Fail(Msg+'. Expected Name="'+string(Expected.Name)+'", but got nil');
  ExpName:=Expected.Name;
  ActName:=Actual.Name;
  if ExpName=ActName then exit;
  Fail(Msg+'. Expected Name="'+string(ExpName)+'", but got Name="'+string(ActName)+'"');
end;

{ TBird }

procedure TJSBird.IncSize;
begin
  InvokeJSNoResult('IncSize',[]);
end;

function TJSBird.CreateBird(const aName: string): IJSBird;
begin
  Result:=InvokeJSObjectResult('CreateBird',[aName],TJSBird) as IJSBird;
end;

function TJSBird.GetCaption: UnicodeString;
begin
  Result:=ReadJSPropertyUnicodeString('Caption');
end;

procedure TJSBird.SetCaption(const AValue: UnicodeString);
begin
  WriteJSPropertyUnicodeString('Caption',AValue);
end;

function TJSBird.GetEnabled: boolean;
begin
  Result:=ReadJSPropertyBoolean('Enabled');
end;

procedure TJSBird.SetEnabled(const AValue: boolean);
begin
  WriteJSPropertyBoolean('Enabled',AValue);
end;

function TJSBird.GetName: string;
begin
  Result:=ReadJSPropertyUtf8String('Name');
end;

function TJSBird.GetChild: IJSBird;
begin
  Result:=ReadJSPropertyObject('Child',TJSBird) as IJSBird;
end;

function TJSBird.GetScale: double;
begin
  Result:=ReadJSPropertyDouble('Scale');
end;

function TJSBird.GetSize: integer;
begin
  Result:=ReadJSPropertyLongInt('Size');
end;

procedure TJSBird.SetName(const AValue: string);
begin
  WriteJSPropertyUtf8String('Name',AValue);
end;

procedure TJSBird.SetChild(const AValue: IJSBird);
begin
  WriteJSPropertyObject('Child',AValue);
end;

procedure TJSBird.SetScale(const AValue: double);
begin
  WriteJSPropertyDouble('Scale',AValue);
end;

procedure TJSBird.SetSize(const AValue: integer);
begin
  WriteJSPropertyLongInt('Size',AValue);
end;

// workaround: fpc wasm does not yet support exporting functions from units
function JOBCallback(const Func: TJOBCallback; Data, Code: Pointer; Args: PByte): PByte;
begin
  Result:=JOB_JS.JOBCallback(Func,Data,Code,Args);
end;

exports
  JOBCallback;

var
  Application: TWasmApp;
begin
  Application:=TWasmApp.Create;
  Application.Run;
end.

