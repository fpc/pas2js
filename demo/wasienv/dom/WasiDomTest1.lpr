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

  IJSBird = interface;
  TJSBird = class;

  TBirdCallBoolean = function(const v: Boolean): Boolean of object;
  TBirdCallInteger = function(const v: integer): integer of object;
  TBirdCallDouble = function(const v: double): double of object;
  TBirdCallUnicodeString = function(const v: UnicodeString): UnicodeString of object;
  TBirdCallBird = function(const v: IJSBird): IJSBird of object;
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
    function EchoDouble(const v: Double; const Call: TBirdCallDouble): Double;
    function EchoUnicodeString(const v: UnicodeString; const Call: TBirdCallUnicodeString): UnicodeString;
    function EchoBird(const v: IJSBird; const Call: TBirdCallBird): IJSBird;
    function EchoVariant(const v: Variant; const Call: TBirdCallVariant): Variant;
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
    function EchoDouble(const v: Double; const Call: TBirdCallDouble): Double;
    function EchoUnicodeString(const v: UnicodeString; const Call: TBirdCallUnicodeString): UnicodeString;
    function EchoBird(const v: IJSBird; const Call: TBirdCallBird): IJSBird;
    function EchoVariant(const v: Variant; const Call: TBirdCallVariant): Variant;
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
    function OnBirdCallDouble(const v: double): double;
    function OnBirdCallUnicodeString(const v: UnicodeString): UnicodeString;
    function OnBirdCallBird(const v: IJSBird): IJSBird;
    function OnBirdCallVariant(const v: Variant): Variant;
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
    procedure TestFuncArgMethod_Double;
    procedure TestFuncArgMethod_UnicodeString;
    procedure TestFuncArgMethod_Object;
    procedure TestFuncArgMethod_Variant;
    procedure TestFuncArgMethod_VariantNumber;
    procedure TestFuncArgMethod_VariantString;
    procedure TestFuncArgMethod_VariantObject;

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

function JOBCallTBirdCallDouble(const aMethod: TMethod; var H: TJOBCallbackHelper): PByte;
var
  v: Double;
begin
  v:=H.GetDouble;
  Result:=H.AllocDouble(TBirdCallDouble(aMethod)(v));
end;

function JOBCallTBirdCallUnicodeString(const aMethod: TMethod; var H: TJOBCallbackHelper): PByte;
var
  v: UnicodeString;
begin
  v:=H.GetString;
  Result:=H.AllocString(TBirdCallUnicodeString(aMethod)(v));
end;

function JOBCallTBirdCallBird(const aMethod: TMethod; var H: TJOBCallbackHelper): PByte;
var
  v: IJSBird;
begin
  //writeln('JOBCallTBirdCallBird START');
  v:=H.GetObject(TJSBird) as IJSBird;
  //writeln('JOBCallTBirdCallBird ',v<>nil);
  Result:=H.AllocIntf(TBirdCallBird(aMethod)(v));
  //writeln('JOBCallTBirdCallBird ',ptruint(Result));
end;

function JOBCallTBirdCallVariant(const aMethod: TMethod; var H: TJOBCallbackHelper): PByte;
var
  v: Variant;
begin
  writeln('JOBCallTBirdCallVariant START');
  v:=H.GetVariant;
  Result:=H.AllocVariant(TBirdCallVariant(aMethod)(v));
  writeln('JOBCallTBirdCallVariant END');
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

function TWasmApp.OnBirdCallDouble(const v: double): double;
begin
  Result:=v;
end;

function TWasmApp.OnBirdCallUnicodeString(const v: UnicodeString
  ): UnicodeString;
begin
  Result:=v;
end;

function TWasmApp.OnBirdCallBird(const v: IJSBird): IJSBird;
begin
  Result:=v;
end;

function TWasmApp.OnBirdCallVariant(const v: Variant): Variant;
begin
  Result:=v;
  case VarType(v) of
  varEmpty: writeln('TWasmApp.OnBirdCallVariant Result=unassigned');
  varNull: writeln('TWasmApp.OnBirdCallVariant Result=Null');
  else
    if VarIsBool(v) or VarIsNumeric(v) or VarIsStr(v) then
      writeln('TWasmApp.OnBirdCallVariant Result=',v)
    else
      writeln('TWasmApp.OnBirdCallVariant VarType(v)=',VarType(v));
  end;
end;

procedure TWasmApp.Run;
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
  TestFuncArgMethod_Double;
  TestFuncArgMethod_UnicodeString;
  TestFuncArgMethod_Object;
  TestFuncArgMethod_Variant;
  TestFuncArgMethod_VariantNumber;
  TestFuncArgMethod_VariantString;
  TestFuncArgMethod_VariantObject;
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

procedure TWasmApp.TestFuncArgMethod_Double;
var
  v: Double;
begin
  Prefix:='TWasmApp.TestFuncArgMethod_Double';
  Bird.Name:='TestFuncArgMethod_Double';

  v:=Bird.EchoDouble(0.5,@OnBirdCallDouble);
  AssertEqual('Bird.EchoDouble(0.5,...)',0.5,v);

  v:=Bird.EchoDouble(MaxSafeIntDouble,@OnBirdCallDouble);
  AssertEqual('Bird.EchoDouble(MaxSafeIntDouble,...)',MaxSafeIntDouble,v);

  v:=Bird.EchoDouble(MinSafeIntDouble,@OnBirdCallDouble);
  AssertEqual('Bird.EchoDouble(MinSafeIntDouble,...)',MinSafeIntDouble,v);

  v:=Bird.EchoDouble(NaN,@OnBirdCallDouble);
  if not IsNan(v) then
    Fail('Bird.EchoDouble(NaN,...) is not NaN');

  v:=Bird.EchoDouble(Infinity,@OnBirdCallDouble);
  AssertEqual('Bird.EchoDouble(Infinity,...)',Infinity,v);

  v:=Bird.EchoDouble(NegInfinity,@OnBirdCallDouble);
  AssertEqual('Bird.EchoDouble(NegInfinity,...)',NegInfinity,v);
end;

procedure TWasmApp.TestFuncArgMethod_UnicodeString;
var
  v: UnicodeString;
begin
  Prefix:='TWasmApp.TestFuncArgMethod_UnicodeString';
  Bird.Name:='TestFuncArgMethod_UnicodeString';

  v:=Bird.EchoUnicodeString('',@OnBirdCallUnicodeString);
  AssertEqualUS('Bird.EchoUnicodeString('''',...)','',v);

  v:=Bird.EchoUnicodeString('c',@OnBirdCallUnicodeString);
  AssertEqualUS('Bird.EchoUnicodeString(''c'',...)','c',v);

  v:=Bird.EchoUnicodeString('abc',@OnBirdCallUnicodeString);
  AssertEqualUS('Bird.EchoUnicodeString(''abc'',...)','abc',v);

  v:=Bird.EchoUnicodeString(#10,@OnBirdCallUnicodeString);
  AssertEqualUS('Bird.EchoUnicodeString(#10,...)',#10,v);

  v:=Bird.EchoUnicodeString('ä',@OnBirdCallUnicodeString);
  AssertEqualUS('Bird.EchoUnicodeString(''ä'',...)','ä',v);

  v:=Bird.EchoUnicodeString('😄',@OnBirdCallUnicodeString);
  AssertEqualUS('Bird.EchoUnicodeString(''😄'',...)','😄',v);
end;

procedure TWasmApp.TestFuncArgMethod_Object;
var
  v, Lisa: IJSBird;
begin
  Prefix:='TWasmApp.TestFuncArgMethod_Object';
  Bird.Name:='TestFuncArgMethod_Object';

  v:=Bird.EchoBird(nil,@OnBirdCallBird);
  AssertEqual('Bird.EchoBird(nil,...)',nil,v);

  v:=Bird.EchoBird(Bird,@OnBirdCallBird);
  AssertEqual('Bird.EchoBird(Bird,...)',Bird,v);

  Lisa:=Bird.CreateBird('Lisa');
  v:=Bird.EchoBird(Lisa,@OnBirdCallBird);
  AssertEqual('Bird.EchoBird(Lisa,...)',Lisa,v);
end;

procedure TWasmApp.TestFuncArgMethod_Variant;
var
  v: Variant;
begin
  Prefix:='TWasmApp.TestFuncArgMethod_Variant';
  Bird.Name:='TestFuncArgMethod_Variant';

  v:=Bird.EchoVariant(true,@OnBirdCallVariant);
  AssertEqual('Bird.EchoVariant(true,...) VarType',varBoolean,VarType(v));
  AssertEqual('Bird.EchoVariant(true,...)',true,v);

  v:=Bird.EchoVariant(false,@OnBirdCallVariant);
  AssertEqual('Bird.EchoVariant(false,...) VarType',varBoolean,VarType(v));
  AssertEqual('Bird.EchoVariant(false,...)',false,v);

  v:=Bird.EchoVariant(Variants.Null,@OnBirdCallVariant);
  AssertEqual('Bird.EchoVariant(Variants.Null,...) VarType',varNull,VarType(v));
end;

procedure TWasmApp.TestFuncArgMethod_VariantNumber;
var
  v: Variant;
begin
  Prefix:='TWasmApp.TestFuncArgMethod_VariantNumber';
  Bird.Name:='TestFuncArgMethod_VariantNumber';

  v:=Bird.EchoVariant(0.5,@OnBirdCallVariant);
  AssertEqual('Bird.EchoVariant(0.5,...) VarType',varDouble,VarType(v));
  AssertEqual('Bird.EchoVariant(0.5,...)',0.5,v);

  v:=Bird.EchoVariant(NaN,@OnBirdCallVariant);
  AssertEqual('Bird.EchoVariant(NaN,...) VarType',varDouble,VarType(v));
  if not IsNan(v) then
    Fail('Bird.EchoVariant(NaN,...)');

  v:=Bird.EchoVariant(Infinity,@OnBirdCallVariant);
  AssertEqual('Bird.EchoVariant(Infinity,...) VarType',varDouble,VarType(v));
  AssertEqual('Bird.EchoVariant(Infinity,...)',Infinity,v);

  v:=Bird.EchoVariant(NegInfinity,@OnBirdCallVariant);
  AssertEqual('Bird.EchoVariant(NegInfinity,...) VarType',varDouble,VarType(v));
  AssertEqual('Bird.EchoVariant(NegInfinity,...)',NegInfinity,v);
end;

procedure TWasmApp.TestFuncArgMethod_VariantString;
var
  Value: Variant;
  us: UnicodeString;
  s, h: AnsiString;
begin
  Prefix:='TWasmApp.TestFuncArgMethod_VariantString';
  Bird.Name:='TestFuncArgMethod_VariantString';

  // literals
  Value:=Bird.EchoVariant('',@OnBirdCallVariant);
  AssertEqual('Bird.EchoVariant('''') VarType',varOleStr,VarType(Value));
  AssertEqualUS('Bird.EchoVariant('''')','',Value);

  Value:=Bird.EchoVariant('a',@OnBirdCallVariant);
  AssertEqual('Bird.EchoVariant(''a'') VarType',varOleStr,VarType(Value));
  AssertEqualUS('Bird.EchoVariant(''a'')','a',Value);

  Value:=Bird.EchoVariant('abc',@OnBirdCallVariant);
  AssertEqual('Bird.EchoVariant(''abc'') VarType',varOleStr,VarType(Value));
  AssertEqualUS('Bird.EchoVariant(''abc'')','abc',Value);

  Value:=Bird.EchoVariant(#13,@OnBirdCallVariant);
  AssertEqual('Bird.EchoVariant(#13) VarType',varOleStr,VarType(Value));
  AssertEqualUS('Bird.EchoVariant(#13)',#13,Value);

  Value:=Bird.EchoVariant('ä',@OnBirdCallVariant);
  AssertEqual('Bird.EchoVariant(''ä'') VarType',varOleStr,VarType(Value));
  AssertEqualUS('Bird.EchoVariant(''ä'')','ä',Value);

  Value:=Bird.EchoVariant('🎉',@OnBirdCallVariant);
  AssertEqual('Bird.EchoVariant(''🎉'') VarType',varOleStr,VarType(Value));
  AssertEqualUS('Bird.EchoVariant(''🎉'')','🎉',Value);

  // unicodestring
  us:='';
  Value:=Bird.EchoVariant(us,@OnBirdCallVariant);
  AssertEqual('Bird.EchoVariant(us:='''') VarType',varOleStr,VarType(Value));
  AssertEqualUS('Bird.EchoVariant(us:='''')','',Value);

  us:='a';
  Value:=Bird.EchoVariant(us,@OnBirdCallVariant);
  AssertEqual('Bird.EchoVariant(us:=''a'') VarType',varOleStr,VarType(Value));
  AssertEqualUS('Bird.EchoVariant(us:=''a'')','a',Value);

  us:='abc';
  Value:=Bird.EchoVariant(us,@OnBirdCallVariant);
  AssertEqual('Bird.EchoVariant(us:=''abc'') VarType',varOleStr,VarType(Value));
  AssertEqualUS('Bird.EchoVariant(us:=''abc'')','abc',Value);

  us:=#13;
  Value:=Bird.EchoVariant(us,@OnBirdCallVariant);
  AssertEqual('Bird.EchoVariant(us:=#13) VarType',varOleStr,VarType(Value));
  AssertEqualUS('Bird.EchoVariant(us:=#13)',#13,Value);

  us:='ä';
  Value:=Bird.EchoVariant(us,@OnBirdCallVariant);
  AssertEqual('Bird.EchoVariant(us:=''ä'') VarType',varOleStr,VarType(Value));
  AssertEqualUS('Bird.EchoVariant(us:=''ä'')','ä',Value);

  us:='🤯';
  Value:=Bird.EchoVariant(us,@OnBirdCallVariant);
  AssertEqual('Bird.EchoVariant(us:=''🤯'') VarType',varOleStr,VarType(Value));
  AssertEqualUS('Bird.EchoVariant(us:=''🤯'')','🤯',Value);

  // ansistring
  s:='';
  Value:=Bird.EchoVariant(s,@OnBirdCallVariant);
  AssertEqual('Bird.EchoVariant(s:='''') VarType',varOleStr,VarType(Value));
  AssertEqual('Bird.EchoVariant(s:='''')','',Value);

  s:='a';
  Value:=Bird.EchoVariant(s,@OnBirdCallVariant);
  AssertEqual('Bird.EchoVariant(s:=''a'') VarType',varOleStr,VarType(Value));
  AssertEqual('Bird.EchoVariant(s:=''a'')','a',Value);

  s:='abc';
  Value:=Bird.EchoVariant(s,@OnBirdCallVariant);
  AssertEqual('Bird.EchoVariant(s:=''abc'') VarType',varOleStr,VarType(Value));
  AssertEqual('Bird.EchoVariant(s:=''abc'')','abc',Value);

  s:=#13;
  Value:=Bird.EchoVariant(s,@OnBirdCallVariant);
  AssertEqual('Bird.EchoVariant(s:=#13) VarType',varOleStr,VarType(Value));
  AssertEqual('Bird.EchoVariant(s:=#13)',#13,Value);

  s:='ä';
  Value:=Bird.EchoVariant(UTF8Decode(s),@OnBirdCallVariant);
  AssertEqual('Bird.EchoVariant(s:=''ä'') VarType',varOleStr,VarType(Value));
  h:=UTF8Encode(Value);
  AssertEqual('Bird.EchoVariant(s:=''ä'')',s,h);

  s:='🤯';
  Value:=Bird.EchoVariant(UTF8Decode(s),@OnBirdCallVariant);
  AssertEqual('Bird.EchoVariant(s:=''🤯'') VarType',varOleStr,VarType(Value));
  h:=UTF8Encode(Value);
  AssertEqual('Bird.EchoVariant(s:=''🤯'')',s,h);
end;

procedure TWasmApp.TestFuncArgMethod_VariantObject;
var
  v: Variant;
  Bart: IJSBird;
begin
  Prefix:='TWasmApp.TestFuncArgMethod_VariantObject';
  Bird.Name:='TestFuncArgMethod_VariantObject';

  v:=Bird.EchoVariant(Bird,@OnBirdCallVariant);
  AssertEqual('Bird.EchoVariant(Bird,...) VarType',varUnknown,VarType(v));

  Bart:=TJSBird.Cast(v);
  AssertEqual('Bird.EchoVariant(Bird)',Bird,Bart);
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

function TJSBird.EchoDouble(const v: Double; const Call: TBirdCallDouble
  ): Double;
var
  m: TJOB_Method;
begin
  m:=TJOB_Method.Create(TMethod(Call),@JOBCallTBirdCallDouble);
  try
    Result:=InvokeJSDoubleResult('EchoCall',[v,m]);
  finally
    m.Free;
  end;
end;

function TJSBird.EchoUnicodeString(const v: UnicodeString;
  const Call: TBirdCallUnicodeString): UnicodeString;
var
  m: TJOB_Method;
begin
  m:=TJOB_Method.Create(TMethod(Call),@JOBCallTBirdCallUnicodeString);
  try
    Result:=InvokeJSUnicodeStringResult('EchoCall',[v,m]);
  finally
    m.Free;
  end;
end;

function TJSBird.EchoBird(const v: IJSBird; const Call: TBirdCallBird): IJSBird;
var
  m: TJOB_Method;
begin
  m:=TJOB_Method.Create(TMethod(Call),@JOBCallTBirdCallBird);
  try
    Result:=InvokeJSObjectResult('EchoCall',[v,m],TJSBird) as IJSBird;
  finally
    m.Free;
  end;
end;

function TJSBird.EchoVariant(const v: Variant; const Call: TBirdCallVariant
  ): Variant;
var
  m: TJOB_Method;
begin
  m:=TJOB_Method.Create(TMethod(Call),@JOBCallTBirdCallVariant);
  try
    Result:=InvokeJSVariantResult('EchoCall',[v,m]);
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

