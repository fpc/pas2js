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
    function GetBoolean: boolean;
    function GetInteger: Integer;
    function GetDouble: Double;
    function GetUnicodeString: UnicodeString;
    function GetUtf8String: String;
    function GetBird: IJSBird;
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
    function GetBoolean: boolean;
    function GetInteger: Integer;
    function GetDouble: Double;
    function GetUnicodeString: UnicodeString;
    function GetUtf8String: String;
    function GetBird: IJSBird;
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
    procedure AssertEqualUS(const Msg: string; const Expected, Actual: UnicodeString);
    procedure AssertEqual(const Msg: string; const Expected, Actual: IJSBird);
  public
    // read/write properties
    procedure TestBooleanProperty;
    procedure TestIntegerProperty;
    procedure TestDoubleProperty;
    procedure TestUnicodeStringProperty;
    procedure TestUTF8StringProperty;
    procedure TestObjectProperty;
    // todo procedure TestMethodProperty;
    // todo procedure TestVariantProperty;

    // function
    procedure TestCallProcedure;
    procedure TestFuncResultBoolean;
    procedure TestFuncResultInteger;
    procedure TestFuncResultDouble;
    procedure TestFuncResultUnicodeString;
    procedure TestFuncResultUTF8String;
    procedure TestFuncResultBird;
    // todo procedure TestFuncResultVariant;

    // function args
    // todo procedure TestFuncArgBoolean;
    // todo procedure TestFuncArgInteger;
    // todo procedure TestFuncArgDouble;
    // todo procedure TestFuncArgUnicodeString;
    // todo procedure TestFuncArgUTF8String;
    // todo procedure TestFuncArgBird;
    // todo procedure TestFuncArgMethod;
    // todo procedure TestFuncArgVariant;

    // dictionaries

    // arrays
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

  TestBooleanProperty;
  TestIntegerProperty;
  TestDoubleProperty;
  TestUnicodeStringProperty;
  TestUTF8StringProperty;
  TestObjectProperty;

  TestCallProcedure;
  TestFuncResultBoolean;
  TestFuncResultInteger;
  TestFuncResultDouble;
  TestFuncResultUnicodeString;
  TestFuncResultUTF8String;
  TestFuncResultBird;

  exit;

  JSElem:=JSDocument.getElementById('playground');
  writeln('TWasmApp.Run playground classname=',JSElem.className_);

  writeln('TWasmApp.Run addEventListener click...');
  JSElem.addEventListener('click',@OnPlaygroundClick);
  writeln('TWasmApp.Run ');
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

procedure TWasmApp.TestUnicodeStringProperty;
begin
  Prefix:='TWasmApp.TestUnicodeStringProperty';
  Bird.Caption:='';
  AssertEqualUS('Bird.Caption:=''''','',Bird.Caption);
  Bird.Caption:='a';
  AssertEqualUS('Bird.Caption:=''a''','a',Bird.Caption);
  Bird.Caption:='abc';
  AssertEqualUS('Bird.Caption:=''abc''','abc',Bird.Caption);
  Bird.Caption:=#13;
  AssertEqualUS('Bird.Caption:=#13',#13,Bird.Caption);
  Bird.Caption:='ä';
  AssertEqualUS('Bird.Caption:=''ä''','ä',Bird.Caption);
  Bird.Caption:='🎉';
  AssertEqualUS('Bird.Caption:=''🎉''','🎉',Bird.Caption);
end;

procedure TWasmApp.TestUTF8StringProperty;
begin
  Prefix:='TWasmApp.TestUTF8StringProperty';
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
  Bird.Name:='TestObjectProperty';
  Bird.Child:=nil;
  AssertEqual('Bird.Child:=nil',nil,Bird.Child);
  Lisa:=Bird.CreateBird('Lisa');
  AssertEqual('Lisa','TestObjectProperty.Lisa',Lisa.Name);
  Bart:=Bird.CreateBird('Bart');
  AssertEqual('Bart','TestObjectProperty.Bart',Bart.Name);
  Bird.Child:=Lisa;
  AssertEqual('Bird.Child:=Lisa',Lisa,Bird.Child);
end;

procedure TWasmApp.TestCallProcedure;
begin
  Prefix:='TWasmApp.TestCallProcedure';
  Bird.Size:=13;
  AssertEqual('Bird.Size:=13',13,Bird.Size);
  Bird.IncSize;
  AssertEqual('Bird.IncSize',14,Bird.Size);
end;

procedure TWasmApp.TestFuncResultBoolean;
begin
  Prefix:='TWasmApp.TestFuncResultBoolean';
  Bird.Enabled:=true;
  AssertEqual('Bird.Enabled:=true',true,Bird.Enabled);
  AssertEqual('Bird.GetBoolean',true,Bird.GetBoolean);
  Bird.Enabled:=false;
  AssertEqual('Bird.Enabled:=false',false,Bird.Enabled);
  AssertEqual('Bird.GetBoolean',false,Bird.GetBoolean);
end;

procedure TWasmApp.TestFuncResultInteger;
begin
  Prefix:='TWasmApp.TestFuncResultInteger';
  Bird.Size:=73;
  AssertEqual('Bird.Size:=73',73,Bird.Size);
  AssertEqual('Bird.GetInteger',73,Bird.GetInteger);
  Bird.Size:=low(integer);
  AssertEqual('Bird.Size:=low(integer)',low(integer),Bird.Size);
  AssertEqual('Bird.GetInteger',low(integer),Bird.GetInteger);
  Bird.Size:=high(integer);
  AssertEqual('Bird.Size:=high(integer)',high(integer),Bird.Size);
  AssertEqual('Bird.GetInteger',high(integer),Bird.GetInteger);
end;

procedure TWasmApp.TestFuncResultDouble;
begin
  Prefix:='TWasmApp.TestFuncResultDouble';
  Bird.Scale:=0.3;
  AssertEqual('Bird.GetDouble 0.3',0.3,Bird.GetDouble);
  Bird.Scale:=-0.3;
  AssertEqual('Bird.GetDouble -0.3',-0.3,Bird.GetDouble);
  Bird.Scale:=NaN;
  if not IsNan(Bird.GetDouble) then
    Fail('Bird.GetDouble NaN');
  Bird.Scale:=Infinity;
  AssertEqual('Bird.GetDouble Infinity',Infinity,Bird.GetDouble);
  Bird.Scale:=NegInfinity;
  AssertEqual('Bird.GetDouble NegInfinity',NegInfinity,Bird.GetDouble);
  Bird.Scale:=0.12345678901234;
  AssertEqual('Bird.GetDouble 0.12345678901234',0.12345678901234,Bird.GetDouble);
end;

procedure TWasmApp.TestFuncResultUnicodeString;
begin
  Prefix:='TWasmApp.TestFuncResultUnicodeString';
  Bird.Name:='';
  AssertEqualUS('Bird.GetUnicodeString ''''','',Bird.GetUnicodeString);
  Bird.Name:='a';
  AssertEqualUS('Bird.GetUnicodeString ''a''','a',Bird.GetUnicodeString);
  Bird.Name:='abc';
  AssertEqualUS('Bird.GetUnicodeString ''abc''','abc',Bird.GetUnicodeString);
  Bird.Name:=#13;
  AssertEqualUS('Bird.GetUnicodeString #13',#13,Bird.GetUnicodeString);
  Bird.Name:='ä';
  AssertEqualUS('Bird.GetUnicodeString ''ä''','ä',Bird.GetUnicodeString);
  Bird.Name:='🎉';
  AssertEqualUS('Bird.GetUnicodeString ''🎉''','🎉',Bird.GetUnicodeString);
end;

procedure TWasmApp.TestFuncResultUTF8String;
begin
  Prefix:='TWasmApp.TestFuncResultUTF8String';
  Bird.Name:='';
  AssertEqual('Bird.GetUTF8String ''''','',Bird.GetUTF8String);
  Bird.Name:='a';
  AssertEqual('Bird.GetUTF8String ''a''','a',Bird.GetUTF8String);
  Bird.Name:='abc';
  AssertEqual('Bird.GetUTF8String ''abc''','abc',Bird.GetUTF8String);
  Bird.Name:=#13;
  AssertEqual('Bird.GetUTF8String #13',#13,Bird.GetUTF8String);
  Bird.Name:='ä';
  AssertEqual('Bird.GetUTF8String ''ä''','ä',Bird.GetUTF8String);
  Bird.Name:='🎉';
  AssertEqual('Bird.GetUTF8String ''🎉''','🎉',Bird.GetUTF8String);
end;

procedure TWasmApp.TestFuncResultBird;
var
  Lisa: IJSBird;
begin
  Prefix:='TWasmApp.TestFuncResultBird';
  Bird.Name:='TestFuncResultBird';
  Bird.Child:=nil;
  AssertEqual('Bird.Child:=nil',nil,Bird.Child);
  AssertEqual('Bird.GetBird',nil,Bird.GetBird);

  Lisa:=Bird.CreateBird('Lisa');
  AssertEqual('Lisa','TestFuncResultBird.Lisa',Lisa.Name);
  Bird.Child:=Lisa;
  AssertEqual('Bird.Child:=Lisa',Lisa,Bird.Child);
  AssertEqual('Bird.GetBird',Lisa,Bird.GetBird);
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

procedure TWasmApp.AssertEqualUS(const Msg: string; const Expected,
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

function TJSBird.GetBoolean: boolean;
begin
  Result:=InvokeJSBooleanResult('GetBoolean',[]);
end;

function TJSBird.GetInteger: Integer;
begin
  Result:=InvokeJSLongIntResult('GetInteger',[]);
end;

function TJSBird.GetDouble: Double;
begin
  Result:=InvokeJSDoubleResult('GetDouble',[]);
end;

function TJSBird.GetUnicodeString: UnicodeString;
begin
  Result:=InvokeJSUnicodeStringResult('GetString',[]);
end;

function TJSBird.GetUtf8String: String;
begin
  Result:=InvokeJSUtf8StringResult('GetString',[]);
end;

function TJSBird.GetBird: IJSBird;
begin
  Result:=InvokeJSObjectResult('GetBird',[],TJSBird) as IJSBird;
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

