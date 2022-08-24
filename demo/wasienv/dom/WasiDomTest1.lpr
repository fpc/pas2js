library WasiDomTest1;

{$mode objfpc}
{$h+}
{$codepage UTF8}
{$WARN 5028 off : Local $1 "$2" is not used}

{off $DEFINE UseDucet}

uses
  {$IFDEF UseDucet}
  unicodeducet, unicodedata, fpwidestring,
  {$ENDIF}
  Math, SysUtils, Variants, JOB_Shared, JOB_Web, JOB_JS;

type
  EWasiTest = class(Exception);

  TJSBird = class;

  TBirdCallBoolean = function(const v: Boolean): Boolean of object;
  TBirdCallInteger = function(const v: integer): integer of object;
  TBirdCallVariant = function(const v: variant): variant of object;

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
    function Echo(const v: Variant): Variant;
    function EchoBoolean(const v: Boolean; const Call: TBirdCallBoolean): Boolean;
    function EchoInteger(const v: integer; const Call: TBirdCallInteger): integer;
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
    class function Cast(Intf: IJSObject): IJSBird; overload;
    // functions
    procedure IncSize;
    function CreateBird(const aName: string): IJSBird;
    function GetBoolean: boolean;
    function GetInteger: Integer;
    function GetDouble: Double;
    function GetUnicodeString: UnicodeString;
    function GetUtf8String: String;
    function GetBird: IJSBird;
    function Echo(const v: Variant): Variant;
    function EchoBoolean(const v: Boolean; const Call: TBirdCallBoolean): Boolean;
    function EchoInteger(const v: integer; const Call: TBirdCallInteger): integer;
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
    function OnBirdCallBoolean(const v: boolean): boolean;
    function OnBirdCallInteger(const v: integer): integer;
  public
    Prefix: string;
    Bird: IJSBird;
    procedure Run;
    procedure Fail(const Msg: string);
    procedure AssertEqual(const Msg: string; const Expected, Actual: boolean);
    procedure AssertEqual(const Msg: string; const Expected, Actual: integer);
    procedure AssertEqual(const Msg: string; const Expected, Actual: int64);
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
    procedure TestFuncResultObject;
    procedure TestFuncResultVariant;
    procedure TestFuncResultVariantNumber;
    procedure TestFuncResultVariantString;
    procedure TestFuncResultVariantObject;

    // callbacks
    procedure TestFuncArgMethod_Boolean;
    procedure TestFuncArgMethod_Integer;

    // dictionaries

    // arrays
    // todo: TestFuncResultVariantArray
    // todo: TestFuncResultDoubleArray
    // todo: TestFuncResultUnicodeStringArray
  end;

function JOBCallTBirdCallBoolean(const aMethod: TMethod; var H: TJOBCallbackHelper): PByte;
var
  v: Boolean;
begin
  v:=H.GetBoolean;
  Result:=H.AllocBool(TBirdCallBoolean(aMethod)(v));
end;

function JOBCallTBirdCallInteger(const aMethod: TMethod; var H: TJOBCallbackHelper): PByte;
var
  v: LongInt;
begin
  v:=H.GetLongInt;
  Result:=H.AllocLongint(TBirdCallInteger(aMethod)(v));
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

function TWasmApp.OnBirdCallBoolean(const v: boolean): boolean;
begin
  Result:=v;
end;

function TWasmApp.OnBirdCallInteger(const v: integer): integer;
begin
  Result:=v;
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
  TestFuncResultObject;
  TestFuncResultVariant;
  TestFuncResultVariantNumber;
  TestFuncResultVariantString;
  TestFuncResultVariantObject;

  TestFuncArgMethod_Boolean;
  TestFuncArgMethod_Integer;

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

procedure TWasmApp.TestFuncResultObject;
var
  Lisa: IJSBird;
begin
  Prefix:='TWasmApp.TestFuncResultObject';
  Bird.Name:='TestFuncResultObject';
  Bird.Child:=nil;
  AssertEqual('Bird.Child:=nil',nil,Bird.Child);
  AssertEqual('Bird.GetBird',nil,Bird.GetBird);

  Lisa:=Bird.CreateBird('Lisa');
  AssertEqual('Lisa','TestFuncResultObject.Lisa',Lisa.Name);
  Bird.Child:=Lisa;
  AssertEqual('Bird.Child:=Lisa',Lisa,Bird.Child);
  AssertEqual('Bird.GetBird',Lisa,Bird.GetBird);
end;

procedure TWasmApp.TestFuncResultVariant;
var
  Value: Variant;
begin
  Prefix:='TWasmApp.TestFuncResultVariant';
  Bird.Name:='TestFuncResultVariant';

  {$IFDEF UseDucet}
  Value:=nil;
  if Value<>nil then ;
  {$ENDIF}

  Value:=Bird.Echo(Variants.Null);
  AssertEqual('Bird.Echo(Variant.Null) VarType',varNull,VarType(Value));
  if Value<>Variants.Null then
    Fail('Bird.Echo(Variant.Null)');

  Value:=Bird.Echo(true);
  AssertEqual('Bird.Echo(true) VarType',varBoolean,VarType(Value));
  AssertEqual('Bird.Echo(true)',true,Value);

  Value:=Bird.Echo(false);
  AssertEqual('Bird.Echo(false) VarType',varBoolean,VarType(Value));
  AssertEqual('Bird.Echo(false)',false,Value);
end;

procedure TWasmApp.TestFuncResultVariantNumber;
var
  Value: Variant;
begin
  Prefix:='TWasmApp.TestFuncResultVariantNumber';
  Bird.Name:='TestFuncResultVariantNumber';

  Value:=Bird.Echo(0);
  AssertEqual('Bird.Echo(0) VarType',varDouble,VarType(Value));
  AssertEqual('Bird.Echo(0)',0,Value);

  Value:=Bird.Echo(127);
  AssertEqual('Bird.Echo(127) VarType',varDouble,VarType(Value));
  AssertEqual('Bird.Echo(127)',127,Value);

  Value:=Bird.Echo(-127);
  AssertEqual('Bird.Echo(-127) VarType',varDouble,VarType(Value));
  AssertEqual('Bird.Echo(-127)',-127,Value);

  Value:=Bird.Echo(128);
  AssertEqual('Bird.Echo(128) VarType',varDouble,VarType(Value));
  AssertEqual('Bird.Echo(128)',128,Value);

  Value:=Bird.Echo(high(longint));
  AssertEqual('Bird.Echo(high(longint)) VarType',varDouble,VarType(Value));
  AssertEqual('Bird.Echo(high(longint))',high(longint),Value);

  Value:=Bird.Echo(low(longint));
  AssertEqual('Bird.Echo(low(longint)) VarType',varDouble,VarType(Value));
  AssertEqual('Bird.Echo(low(longint))',low(longint),Value);

  Value:=Bird.Echo(high(longword));
  AssertEqual('Bird.Echo(high(longword)) VarType',varDouble,VarType(Value));
  AssertEqual('Bird.Echo(high(longword))',high(longword),Value);

  Value:=Bird.Echo(MaxSafeIntDouble);
  AssertEqual('Bird.Echo(MaxSafeIntDouble) VarType',varDouble,VarType(Value));
  AssertEqual('Bird.Echo(MaxSafeIntDouble)',double(MaxSafeIntDouble),Value);

  Value:=Bird.Echo(MinSafeIntDouble);
  AssertEqual('Bird.Echo(MinSafeIntDouble) VarType',varDouble,VarType(Value));
  AssertEqual('Bird.Echo(MinSafeIntDouble)',double(MinSafeIntDouble),Value);

  Value:=Bird.Echo(NaN);
  AssertEqual('Bird.Echo(NaN) VarType',varDouble,VarType(Value));
  if not IsNan(Value) then
    Fail('Bird.Echo(NaN)');

  Value:=Bird.Echo(Infinity);
  AssertEqual('Bird.Echo(Infinity) VarType',varDouble,VarType(Value));
  AssertEqual('Bird.Echo(Infinity)',double(Infinity),Value);

  Value:=Bird.Echo(NegInfinity);
  AssertEqual('Bird.Echo(NegInfinity) VarType',varDouble,VarType(Value));
  AssertEqual('Bird.Echo(NegInfinity)',double(NegInfinity),Value);

  Value:=Bird.Echo(0.3);
  AssertEqual('Bird.Echo(0.3) VarType',varDouble,VarType(Value));
  AssertEqual('Bird.Echo(0.3)',double(0.3),Value);

end;

procedure TWasmApp.TestFuncResultVariantString;
var
  Value: Variant;
  us: UnicodeString;
  s, h: string;
begin
  Prefix:='TWasmApp.TestFuncResultVariantString';
  Bird.Name:='TestFuncResultVariantString';

  // literals
  Value:=Bird.Echo('');
  AssertEqual('Bird.Echo('''') VarType',varOleStr,VarType(Value));
  AssertEqualUS('Bird.Echo('''')','',Value);

  Value:=Bird.Echo('a');
  AssertEqual('Bird.Echo(''a'') VarType',varOleStr,VarType(Value));
  AssertEqualUS('Bird.Echo(''a'')','a',Value);

  Value:=Bird.Echo('abc');
  AssertEqual('Bird.Echo(''abc'') VarType',varOleStr,VarType(Value));
  AssertEqualUS('Bird.Echo(''abc'')','abc',Value);

  Value:=Bird.Echo(#13);
  AssertEqual('Bird.Echo(#13) VarType',varOleStr,VarType(Value));
  AssertEqualUS('Bird.Echo(#13)',#13,Value);

  Value:=Bird.Echo('ä');
  AssertEqual('Bird.Echo(''ä'') VarType',varOleStr,VarType(Value));
  AssertEqualUS('Bird.Echo(''ä'')','ä',Value);

  Value:=Bird.Echo('🎉');
  AssertEqual('Bird.Echo(''🎉'') VarType',varOleStr,VarType(Value));
  AssertEqualUS('Bird.Echo(''🎉'')','🎉',Value);

  // unicodestring
  us:='';
  Value:=Bird.Echo(us);
  AssertEqual('Bird.Echo(us:='''') VarType',varOleStr,VarType(Value));
  AssertEqualUS('Bird.Echo(us:='''')','',Value);

  us:='a';
  Value:=Bird.Echo(us);
  AssertEqual('Bird.Echo(us:=''a'') VarType',varOleStr,VarType(Value));
  AssertEqualUS('Bird.Echo(us:=''a'')','a',Value);

  us:='abc';
  Value:=Bird.Echo(us);
  AssertEqual('Bird.Echo(us:=''abc'') VarType',varOleStr,VarType(Value));
  AssertEqualUS('Bird.Echo(us:=''abc'')','abc',Value);

  us:=#13;
  Value:=Bird.Echo(us);
  AssertEqual('Bird.Echo(us:=#13) VarType',varOleStr,VarType(Value));
  AssertEqualUS('Bird.Echo(us:=#13)',#13,Value);

  us:='ä';
  Value:=Bird.Echo(us);
  AssertEqual('Bird.Echo(us:=''ä'') VarType',varOleStr,VarType(Value));
  AssertEqualUS('Bird.Echo(us:=''ä'')','ä',Value);

  us:='🤯';
  Value:=Bird.Echo(us);
  AssertEqual('Bird.Echo(us:=''🤯'') VarType',varOleStr,VarType(Value));
  AssertEqualUS('Bird.Echo(us:=''🤯'')','🤯',Value);

  // ansistring
  s:='';
  Value:=Bird.Echo(s);
  AssertEqual('Bird.Echo(s:='''') VarType',varOleStr,VarType(Value));
  AssertEqual('Bird.Echo(s:='''')','',Value);

  s:='a';
  Value:=Bird.Echo(s);
  AssertEqual('Bird.Echo(s:=''a'') VarType',varOleStr,VarType(Value));
  AssertEqual('Bird.Echo(s:=''a'')','a',Value);

  s:='abc';
  Value:=Bird.Echo(s);
  AssertEqual('Bird.Echo(s:=''abc'') VarType',varOleStr,VarType(Value));
  AssertEqual('Bird.Echo(s:=''abc'')','abc',Value);

  s:=#13;
  Value:=Bird.Echo(s);
  AssertEqual('Bird.Echo(s:=#13) VarType',varOleStr,VarType(Value));
  AssertEqual('Bird.Echo(s:=#13)',#13,Value);

  s:='ä';
  Value:=Bird.Echo(UTF8Decode(s));
  AssertEqual('Bird.Echo(s:=''ä'') VarType',varOleStr,VarType(Value));
  h:=UTF8Encode(Value);
  AssertEqual('Bird.Echo(s:=''ä'')',s,h);

  s:='🤯';
  Value:=Bird.Echo(UTF8Decode(s));
  AssertEqual('Bird.Echo(s:=''🤯'') VarType',varOleStr,VarType(Value));
  h:=UTF8Encode(Value);
  AssertEqual('Bird.Echo(s:=''🤯'')',s,h);
end;

procedure TWasmApp.TestFuncResultVariantObject;
var
  Value: Variant;
  Lisa, Bart: IJSBird;
begin
  Prefix:='TWasmApp.TestFuncResultVariantObject';
  Bird.Name:='TestFuncResultVariantObject';

  Lisa:=nil;
  Value:=Bird.Echo(Lisa);
  AssertEqual('Bird.Echo(Lisa:=nil) VarType',varNull,VarType(Value));

  Lisa:=Bird.CreateBird('Lisa');
  AssertEqual('Lisa','TestFuncResultVariantObject.Lisa',Lisa.Name);
  Value:=Bird.Echo(Lisa);
  AssertEqual('Bird.Echo(Lisa) VarType',varUnknown,VarType(Value));
  Bart:=TJSBird.Cast(Value);
  AssertEqual('Bird.Echo(Lisa)',Lisa,Bart);
end;

procedure TWasmApp.TestFuncArgMethod_Boolean;
var
  v: Boolean;
begin
  Prefix:='TWasmApp.TestFuncArgMethod_Boolean';
  Bird.Name:='TestFuncArgMethod_Boolean';

  v:=Bird.EchoBoolean(true,@OnBirdCallBoolean);
  AssertEqual('Bird.EchoBoolean(true,...)',true,v);

  v:=Bird.EchoBoolean(false,@OnBirdCallBoolean);
  AssertEqual('Bird.EchoBoolean(false,...)',false,v);
end;

procedure TWasmApp.TestFuncArgMethod_Integer;
var
  v: Integer;
begin
  Prefix:='TWasmApp.TestFuncArgMethod_Integer';
  Bird.Name:='TestFuncArgMethod_Integer';

  v:=Bird.EchoInteger(13,@OnBirdCallInteger);
  AssertEqual('Bird.EchoInteger(13,...)',13,v);

  v:=Bird.EchoInteger(low(longint),@OnBirdCallInteger);
  AssertEqual('Bird.EchoInteger(low(longint),...)',low(longint),v);

  v:=Bird.EchoInteger(high(longint),@OnBirdCallInteger);
  AssertEqual('Bird.EchoInteger(high(longint),...)',high(longint),v);
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

procedure TWasmApp.AssertEqual(const Msg: string; const Expected, Actual: int64
  );
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

class function TJSBird.Cast(Intf: IJSObject): IJSBird;
begin
  Result:=TJSBird.JOBCast(Intf);
end;

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

function TJSBird.Echo(const v: Variant): Variant;
begin
  Result:=InvokeJSVariantResult('Echo',[v]);
end;

function TJSBird.EchoBoolean(const v: Boolean; const Call: TBirdCallBoolean
  ): Boolean;
var
  m: TJOB_Method;
begin
  m:=TJOB_Method.Create(TMethod(Call),@JOBCallTBirdCallBoolean);
  try
    Result:=InvokeJSBooleanResult('EchoCall',[v,m]);
  finally
    m.Free;
  end;
end;

function TJSBird.EchoInteger(const v: integer; const Call: TBirdCallInteger
  ): integer;
var
  m: TJOB_Method;
begin
  m:=TJOB_Method.Create(TMethod(Call),@JOBCallTBirdCallInteger);
  try
    Result:=InvokeJSLongIntResult('EchoCall',[v,m]);
  finally
    m.Free;
  end;
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
  {$IFDEF UseDucet}
  SetActiveCollation('DUCET');
  {$ENDIF}
  Application:=TWasmApp.Create;
  Application.Run;
end.

