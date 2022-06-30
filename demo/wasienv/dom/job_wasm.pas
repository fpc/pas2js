{
  JOB - JS Object Bridge for Webassembly

  Webassembly unit giving access to the browser DOM.

  see https://wiki.freepascal.org/WebAssembly/DOM
}
unit JOB_WAsm;

{$mode ObjFPC}{$H+}
{$ModeSwitch advancedrecords}

{$define VerboseJOB}

interface

uses
  SysUtils, Types, Math, Classes, JOB_Shared;

const
  MinSafeIntDouble = -$1fffffffffffff; // -9007199254740991 54 bits (52 plus signed bit plus implicit highest bit)
  MaxSafeIntDouble =  $1fffffffffffff; //  9007199254740991

Type
  PJOBObjectID = ^TJOBObjectID;

  EJSObject = class(Exception);
  EJSInvoke = class(EJSObject)
  public
    ObjectID: TJOBObjectID;
    FuncName: string;
  end;
  EJSArgParse = class(EJSObject);

  TJOB_JSValueKind = (
    jjvkUndefined,
    jjvkBoolean,
    jjvkDouble,
    jjvkString,
    jjvkObject,
    jjvkMethod,
    jjvkDictionary
    );
  TJOB_JSValueKinds = set of TJOB_JSValueKind;

const
  JOB_JSValueKindNames: array[TJOB_JSValueKind] of string = (
    'Undefined',
    'Boolean',
    'Double',
    'String',
    'Object',
    'Method',
    'Dictionary'
    );

  JOB_Undefined = Pointer(1);

type

  { TJOB_JSValue }

  TJOB_JSValue = class
  public
    Kind: TJOB_JSValueKind;
    constructor Create(aKind: TJOB_JSValueKind);
    function AsString: string; virtual;
  end;

  { TJOB_Boolean }

  TJOB_Boolean = class(TJOB_JSValue)
  public
    Value: Boolean;
    constructor Create(aValue: Boolean);
    function AsString: string; override;
  end;

  { TJOB_Double }

  TJOB_Double = class(TJOB_JSValue)
  public
    Value: Double;
    constructor Create(const aValue: Double);
    function AsString: string; override;
  end;

  { TJOB_String }

  TJOB_String = class(TJOB_JSValue)
  public
    Value: UnicodeString;
    constructor Create(const aValue: UnicodeString);
    function AsString: string; override;
  end;

  IJSObject = interface;

  { TJOB_Object }

  TJOB_Object = class(TJOB_JSValue)
  public
    Value: IJSObject;
    constructor Create(aValue: IJSObject);
    function AsString: string; override;
  end;

  TJOBInvokeType = (
    jiCall,  // call function
    jiGet, // read property
    jiGetTypeOf, // read property and do typeof
    jiSet, // write property
    jiNew // new operator
    );
  TJOBInvokeTypes = set of TJOBInvokeType;

  TJSObject = class;
  TJSObjectClass = class of TJSObject;

  { TJOBCallbackHelper - parse callback arguments and create result }

  TJOBCallbackHelper = record
    p: PByte;
    Index: integer;
    Count: integer;
    procedure Init(Args: PByte);
    function GetType: byte; // see JOBArg* constants, keeps p
    procedure Skip;
    function GetBoolean: boolean;
    function GetDouble: double;
    function GetString: UnicodeString;
    function GetObject(aResultClass: TJSObjectClass): TJSObject;
    function GetValue: TJOB_JSValue;
    function GetLongInt: longint;
    function GetMaxInt: int64;

    function AllocUndefined: PByte;
    function AllocBool(b: boolean): PByte;
    function AllocLongint(i: longint): PByte;
    function AllocDouble(const d: double): PByte;
    function AllocString(const s: UnicodeString): PByte;
    function AllocNil: PByte;
    function AllocIntf(Intf: IJSObject): PByte;
    function AllocObject(Obj: TJSObject): PByte;
    function AllocObjId(ObjId: TJOBObjectID): PByte;
  end;

  TJOBCallback = function(const aMethod: TMethod; var H: TJOBCallbackHelper): PByte;

  { TJOB_Method }

  TJOB_Method = class(TJOB_JSValue)
  public
    Value: TMethod;
    Invoke: TJOBCallback;
    constructor Create(const aMethod: TMethod; const AnInvoke: TJOBCallback);
    function AsString: string; override;
  end;

  TJOB_Pair = record
    Name: UnicodeString;
    Value: TJOB_JSValue;
  end;
  TJOB_PairArray = array of TJOB_Pair;

  { TJOB_Dictionary }

  TJOB_Dictionary = class(TJOB_JSValue)
  public
    Values: TJOB_PairArray;
    procedure Add(const aName: UnicodeString; const aValue: TJOB_JSValue);
    constructor Create(const Pairs: array of const);
    destructor Destroy; override;
    procedure Clear;
  end;

  { IJSObject }

  IJSObject = interface
    ['{BE5CDE03-D471-4AB3-8F27-A5EA637416F7}']
    function GetJSObjectID: TJOBObjectID;
    function GetJSObjectCastSrc: IJSObject;
    function GetPascalClassName: string;
    procedure InvokeJSNoResult(const aName: string; Const Args: Array of const; Invoke: TJOBInvokeType = jiCall); virtual;
    function InvokeJSBooleanResult(const aName: string; Const Args: Array of const; Invoke: TJOBInvokeType = jiCall): Boolean; virtual;
    function InvokeJSDoubleResult(const aName: string; Const Args: Array of const; Invoke: TJOBInvokeType = jiCall): Double; virtual;
    function InvokeJSUnicodeStringResult(const aName: string; Const Args: Array of const; Invoke: TJOBInvokeType = jiCall): UnicodeString; virtual;
    function InvokeJSObjectResult(const aName: string; Const Args: Array of const; aResultClass: TJSObjectClass; Invoke: TJOBInvokeType = jiCall): TJSObject; virtual;
    function InvokeJSValueResult(const aName: string; Const Args: Array of const; Invoke: TJOBInvokeType = jiCall): TJOB_JSValue; virtual;
    function InvokeJSUtf8StringResult(const aName: string; Const args: Array of const; Invoke: TJOBInvokeType = jiCall): String; virtual;
    function InvokeJSLongIntResult(const aName: string; Const args: Array of const; Invoke: TJOBInvokeType = jiCall): LongInt; virtual;
    function InvokeJSTypeOf(const aName: string; Const Args: Array of const): TJOBResult; virtual;
    function ReadJSPropertyBoolean(const aName: string): boolean; virtual;
    function ReadJSPropertyDouble(const aName: string): double; virtual;
    function ReadJSPropertyUnicodeString(const aName: string): UnicodeString; virtual;
    function ReadJSPropertyObject(const aName: string; aResultClass: TJSObjectClass): TJSObject; virtual;
    function ReadJSPropertyUtf8String(const aName: string): string; virtual;
    function ReadJSPropertyLongInt(const aName: string): LongInt; virtual;
    function ReadJSPropertyValue(const aName: string): TJOB_JSValue; virtual;
    procedure WriteJSPropertyBoolean(const aName: string; Value: Boolean); virtual;
    procedure WriteJSPropertyDouble(const aName: string; Value: Double); virtual;
    procedure WriteJSPropertyUnicodeString(const aName: string; const Value: UnicodeString); virtual;
    procedure WriteJSPropertyUtf8String(const aName: string; const Value: String); virtual;
    procedure WriteJSPropertyObject(const aName: string; Value: IJSObject); virtual;
    procedure WriteJSPropertyLongInt(const aName: string; Value: LongInt); virtual;
    function NewJSObject(Const Args: Array of const; aResultClass: TJSObjectClass): TJSObject; virtual;
  end;

  { TJSObject }

  TJSObject = class(TInterfacedObject,IJSObject)
  private
    FObjectID: TJOBObjectID;
    FCastSrc: IJSObject;
  protected
    type
      TJOBInvokeNoResultFunc = function(
          ObjID: TJOBObjectID;
          NameP: PChar;
          NameLen: longint;
          Invoke: longint;
          ArgP: PByte
        ): TJOBResult;
      TJOBInvokeOneResultFunc = function(
          ObjID: TJOBObjectID;
          NameP: PChar;
          NameLen: longint;
          Invoke: longint;
          ArgP: PByte;
          ResultP: PByte
        ): TJOBResult;
    function GetJSObjectID: TJOBObjectID;
    function GetJSObjectCastSrc: IJSObject;
    function GetPascalClassName: string;
    function FetchString(Len: NativeInt): UnicodeString;
    function InvokeJSNoResultFunc(const aName: string; Const Args: Array of const;
      const InvokeFunc: TJOBInvokeNoResultFunc; Invoke: TJOBInvokeType): TJOBResult;
    function InvokeJSOneResult(const aName: string; Const Args: Array of const;
      const InvokeFunc: TJOBInvokeOneResultFunc; ResultP: PByte; Invoke: TJOBInvokeType): TJOBResult;
    procedure InvokeJS_Raise(const aName, Msg: string); virtual;
    procedure InvokeJS_RaiseResultMismatch(const aName: string; Expected, Actual: TJOBResult); virtual;
    procedure InvokeJS_RaiseResultMismatchStr(const aName: string; const Expected, Actual: string); virtual;
    function CreateInvokeJSArgs(const Args: array of const): PByte; virtual;
  public
    constructor Cast(Intf: IJSObject);
    constructor CreateFromID(aID: TJOBObjectID); virtual; // use this only for the owner (it will release it on free)
    destructor Destroy; override;
    property ObjectID: TJOBObjectID read FObjectID;
    property CastSrc: IJSObject read FCastSrc; // nil means it is the owner, otherwise it is a typecast
    // call a function
    procedure InvokeJSNoResult(const aName: string; Const Args: Array of const; Invoke: TJOBInvokeType = jiCall); virtual;
    function InvokeJSBooleanResult(const aName: string; Const Args: Array of const; Invoke: TJOBInvokeType = jiCall): Boolean; virtual;
    function InvokeJSDoubleResult(const aName: string; Const Args: Array of const; Invoke: TJOBInvokeType = jiCall): Double; virtual;
    function InvokeJSUnicodeStringResult(const aName: string; Const Args: Array of const; Invoke: TJOBInvokeType = jiCall): UnicodeString; virtual;
    function InvokeJSObjectResult(const aName: string; Const Args: Array of const; aResultClass: TJSObjectClass; Invoke: TJOBInvokeType = jiCall): TJSObject; virtual;
    function InvokeJSValueResult(const aName: string; Const Args: Array of const; Invoke: TJOBInvokeType = jiCall): TJOB_JSValue; virtual;
    function InvokeJSUtf8StringResult(const aName: string; Const args: Array of const; Invoke: TJOBInvokeType = jiCall): String; virtual;
    function InvokeJSLongIntResult(const aName: string; Const args: Array of const; Invoke: TJOBInvokeType = jiCall): LongInt; virtual;
    function InvokeJSMaxIntResult(const aName: string; Const args: Array of const; Invoke: TJOBInvokeType = jiCall): int64; virtual;
    function InvokeJSTypeOf(const aName: string; Const Args: Array of const): TJOBResult; virtual;
    // read a property
    function ReadJSPropertyBoolean(const aName: string): boolean; virtual;
    function ReadJSPropertyDouble(const aName: string): double; virtual;
    function ReadJSPropertyUnicodeString(const aName: string): UnicodeString; virtual;
    function ReadJSPropertyObject(const aName: string; aResultClass: TJSObjectClass): TJSObject; virtual;
    function ReadJSPropertyUtf8String(const aName: string): string; virtual;
    function ReadJSPropertyLongInt(const aName: string): LongInt; virtual;
    function ReadJSPropertyInt64(const aName: string): Int64; virtual;
    function ReadJSPropertyValue(const aName: string): TJOB_JSValue; virtual;
    // write a property
    procedure WriteJSPropertyBoolean(const aName: string; Value: Boolean); virtual;
    procedure WriteJSPropertyDouble(const aName: string; Value: Double); virtual;
    procedure WriteJSPropertyUnicodeString(const aName: string; const Value: UnicodeString); virtual;
    procedure WriteJSPropertyUtf8String(const aName: string; const Value: String); virtual;
    procedure WriteJSPropertyObject(const aName: string; Value: IJSObject); virtual;
    procedure WriteJSPropertyLongInt(const aName: string; Value: LongInt); virtual;
    procedure WriteJSPropertyValue(const aName: string; Value: TJOB_JSValue); virtual;
    // create a new object using the new-operator
    function NewJSObject(Const Args: Array of const; aResultClass: TJSObjectClass): TJSObject; virtual;
  end;

var
  JSObject: TJSObject;

// imported functions from browser
function __job_invoke_noresult(
  ObjID: TJOBObjectID;
  NameP: PChar;
  NameLen: longint;
  Invoke: longint;
  ArgP: PByte
): TJOBResult; external JOBExportName name JOBFn_InvokeNoResult;

function __job_invoke_boolresult(
  ObjID: TJOBObjectID;
  NameP: PChar;
  NameLen: longint;
  Invoke: longint;
  ArgP: PByte;
  ResultByteBoolP: PByte
): TJOBResult; external JOBExportName name JOBFn_InvokeBooleanResult;

function __job_invoke_doubleresult(
  ObjID: TJOBObjectID;
  NameP: PChar;
  NameLen: longint;
  Invoke: longint;
  ArgP: PByte;
  ResultDoubleP: PByte
): TJOBResult; external JOBExportName name JOBFn_InvokeDoubleResult;

function __job_invoke_stringresult(
  ObjID: TJOBObjectID;
  NameP: PChar;
  NameLen: longint;
  Invoke: longint;
  ArgP: PByte;
  ResultLenP: PByte // nativeint
): TJOBResult; external JOBExportName name JOBFn_InvokeStringResult;

function __job_getstringresult(
  ResultP: PByte
): TJOBResult; external JOBExportName name JOBFn_GetStringResult;

function __job_releasestringresult(
): TJOBResult; external JOBExportName name JOBFn_ReleaseStringResult;

function __job_invoke_objectresult(
  ObjID: TJOBObjectID;
  NameP: PChar;
  NameLen: longint;
  Invoke: longint;
  ArgP: PByte;
  ResultObjIDP: PByte // nativeint
): TJOBResult; external JOBExportName name JOBFn_InvokeObjectResult;

function __job_release_object(
  ObjID: TJOBObjectID
): TJOBResult; external JOBExportName name JOBFn_ReleaseObject;

function __job_invoke_jsvalueresult(
  ObjID: TJOBObjectID;
  NameP: PChar;
  NameLen: longint;
  Invoke: longint;
  ArgP: PByte;
  ResultP: PByte  // various
): TJOBResult; external JOBExportName name JOBFn_InvokeJSValueResult;

function JOBCallback(const Func: TJOBCallback; Data, Code: Pointer; Args: PByte): PByte;

implementation

const
  InvokeGetToInt: array[TJOBInvokeType] of integer = (
    JOBInvokeCall,
    JOBInvokeGet,
    JOBInvokeGetTypeOf,
    JOBInvokeSet,
    JOBInvokeNew
    );

{$IFDEF VerboseJOB}
function GetVarRecName(vt: word): string;
begin
  case vt of
    vtInteger: Result:='vtInteger';
    vtBoolean: Result:='vtBoolean';
    vtChar: Result:='vtChar';
    {$ifndef FPUNONE}
    vtExtended: Result:='vtExtended';
    {$endif}
    vtString: Result:='vtString';
    vtPointer: Result:='vtPointer';
    vtPChar: Result:='vtPChar';
    vtObject: Result:='vtObject';
    vtClass: Result:='vtClass';
    vtWideChar: Result:='vtWideChar';
    vtPWideChar: Result:='vtPWideChar';
    vtAnsiString: Result:='vtAnsiString';
    vtCurrency: Result:='vtCurrency';
    vtVariant: Result:='vtVariant';
    vtInterface: Result:='vtInterface';
    vtWideString: Result:='vtWideString';
    vtInt64: Result:='vtInt64';
    vtQWord: Result:='vtQWord';
    vtUnicodeString: Result:='vtUnicodeString';
  else
    Result:='vt?';
  end;
end;
{$ENDIF}

function __job_callback(w: NativeInt): boolean;
begin
  writeln('__job_callback w=',w);
  Result:=true;
end;

function JOBCallback(const Func: TJOBCallback; Data, Code: Pointer; Args: PByte
  ): PByte;
var
  m: TMethod;
  h: TJOBCallbackHelper;
begin
  Result:=nil;
  try
    //writeln('JOBCallback');
    m.Data:=Data;
    m.Code:=Code;
    h.Init(Args);
    Result:=Func(m,h);
  finally
    if Args<>nil then
      FreeMem(Args);
  end;
end;

{ TJOBCallbackHelper }

procedure TJOBCallbackHelper.Init(Args: PByte);
begin
  p:=Args;
  Index:=0;
  if p<>nil then
  begin
    Count:=p^;
    inc(p);
  end else
    Count:=0;
end;

function TJOBCallbackHelper.GetType: byte;
begin
  if Index=Count then
    Result:=JOBArgUndefined
  else
    Result:=p^;
end;

procedure TJOBCallbackHelper.Skip;
var
  Len: LongWord;
begin
  if Index=Count then exit;
  case p^ of
  JOBArgUndefined,
  JOBArgTrue,
  JOBArgFalse,
  JOBArgNil: inc(p);
  JOBArgDouble: inc(p,9);
  JOBArgUnicodeString:
    begin
      inc(p);
      Len:=PLongWord(p)^;
      inc(p,4+2*Len);
    end
  else
    raise EJSArgParse.Create(JOBArgNames[p^]);
  end;
  inc(Index);
end;

function TJOBCallbackHelper.GetBoolean: boolean;
begin
  Result:=false;
  if Index=Count then
    exit;
  case p^ of
  JOBArgUndefined: ;
  JOBArgTrue: Result:=true;
  JOBArgFalse: ;
  else
    raise EJSArgParse.Create(JOBArgNames[p^]);
  end;
  inc(p);
  inc(Index);
end;

function TJOBCallbackHelper.GetDouble: double;
begin
  Result:=NaN;
  if Index=Count then
    exit;
  case p^ of
  JOBArgUndefined:
    inc(p);
  JOBArgDouble:
    begin
      inc(p);
      Result:=PDouble(p)^;
      inc(p,8);
    end
  else
    raise EJSArgParse.Create(JOBArgNames[p^]);
  end;
  inc(Index);
end;

function TJOBCallbackHelper.GetString: UnicodeString;
var
  Len: LongWord;
begin
  Result:='';
  if Index=Count then
    exit;
  case p^ of
  JOBArgUndefined:
    inc(p);
  JOBArgUnicodeString:
    begin
      inc(p);
      Len:=PLongWord(p)^;
      inc(p,4);
      if Len>0 then
      begin
        SetLength(Result,Len);
        Move(p^,Result[1],2*Len);
        inc(p,2*Len);
      end;
    end
  else
    raise EJSArgParse.Create(JOBArgNames[p^]);
  end;
  inc(Index);
end;

function TJOBCallbackHelper.GetObject(aResultClass: TJSObjectClass): TJSObject;
var
  ObjId: LongWord;
begin
  //writeln('TJOBCallbackHelper.GetObject ',Index,' Count=',Count);
  Result:=nil;
  if Index=Count then
    exit;
  //writeln('TJOBCallbackHelper.GetObject type=',p^);
  case p^ of
  JOBArgUndefined,
  JOBArgNil:
    inc(p);
  JOBArgObject:
    begin
      inc(p);
      ObjId:=PLongWord(p)^;
      inc(p,4);
      Result:=aResultClass.CreateFromID(ObjId);
    end
  else
    raise EJSArgParse.Create(JOBArgNames[p^]);
  end;
  inc(Index);
end;

function TJOBCallbackHelper.GetValue: TJOB_JSValue;
var
  ObjId, Len: LongWord;
  Obj: TJSObject;
  S: UnicodeString;
begin
  Result:=nil;
  if (Index=Count) or (p^=JOBArgUndefined) then
  begin
    Result:=TJOB_JSValue.Create(jjvkUndefined);
    exit;
  end;
  case p^ of
  JOBArgTrue:
    begin
      Result:=TJOB_Boolean.Create(true);
      inc(p);
    end;
  JOBArgFalse:
    begin
      Result:=TJOB_Boolean.Create(false);
      inc(p);
    end;
  JOBArgDouble:
    begin
      inc(p);
      Result:=TJOB_Double.Create(PDouble(p)^);
      inc(p,8);
    end;
  JOBArgUnicodeString:
    begin
      inc(p);
      Len:=PLongWord(p)^;
      inc(p,4);
      S:='';
      if Len>0 then
      begin
        SetLength(S,Len);
        Move(p^,S[1],2*Len);
        inc(p,2*Len);
      end;
      Result:=TJOB_String.Create(S);
    end;
  JOBArgNil:
    begin
      Result:=TJOB_Object.Create(nil);
      inc(p);
    end;
  JOBArgObject:
    begin
      inc(p);
      ObjId:=PLongWord(p)^;
      inc(p,4);
      Obj:=TJSObject.CreateFromID(ObjId);
      Result:=TJOB_Object.Create(Obj);
    end;
  else
    raise EJSArgParse.Create(JOBArgNames[p^]);
  end;
  inc(Index);
end;

function TJOBCallbackHelper.GetLongInt: longint;
var
  d: Double;
begin
  d:=GetDouble;
  if (Frac(d)<>0) or (d<low(longint)) or (d>high(longint)) then
    raise EJSArgParse.Create('expected longint, but got double')
  else
    Result:=Trunc(d);
end;

function TJOBCallbackHelper.GetMaxInt: int64;
var
  d: Double;
begin
  d:=GetDouble;
  if (Frac(d)<>0) or (d<low(int64)) or (d>high(int64)) then
    raise EJSArgParse.Create('expected int64, but got double')
  else
    Result:=Trunc(d);
end;

function TJOBCallbackHelper.AllocUndefined: PByte;
begin
  GetMem(Result,1);
  Result^:=JOBArgUndefined;
end;

function TJOBCallbackHelper.AllocBool(b: boolean): PByte;
begin
  GetMem(Result,1);
  if b then
    Result^:=JOBArgTrue
  else
    Result^:=JOBArgFalse;
end;

function TJOBCallbackHelper.AllocLongint(i: longint): PByte;
begin
  GetMem(Result,5);
  Result^:=JOBArgLongint;
  PLongint(Result+1)^:=i;
end;

function TJOBCallbackHelper.AllocDouble(const d: double): PByte;
begin
  GetMem(Result,9);
  Result^:=JOBArgDouble;
  PDouble(Result+1)^:=d;
end;

function TJOBCallbackHelper.AllocString(const s: UnicodeString): PByte;
var
  l: SizeInt;
begin
  l:=length(s);
  GetMem(Result,5+l);
  Result^:=JOBArgUnicodeString;
  PLongWord(Result+1)^:=l;
  if l>0 then
    Move(s[1],Result[5],l);
end;

function TJOBCallbackHelper.AllocNil: PByte;
begin
  GetMem(Result,1);
  Result^:=JOBArgNil;
end;

function TJOBCallbackHelper.AllocIntf(Intf: IJSObject): PByte;
begin
  if Intf=nil then
    Result:=AllocNil
  else
    Result:=AllocObjId(Intf.GetJSObjectID);
end;

function TJOBCallbackHelper.AllocObject(Obj: TJSObject): PByte;
begin
  if Obj=nil then
    Result:=AllocNil
  else
    Result:=AllocObjId(Obj.ObjectID);
end;

function TJOBCallbackHelper.AllocObjId(ObjId: TJOBObjectID): PByte;
begin
  GetMem(Result,1+SizeOf(TJOBObjectID));
  Result^:=JOBArgObject;
  PJOBObjectID(Result+1)^:=ObjId;
end;

{ TJOB_JSValue }

constructor TJOB_JSValue.Create(aKind: TJOB_JSValueKind);
begin
  Kind:=aKind;
end;

function TJOB_JSValue.AsString: string;
begin
  if Kind=jjvkUndefined then
    Result:='undefined'
  else begin
    Result:='';
    str(Kind,Result);
  end;
end;

{ TJOB_Boolean }

constructor TJOB_Boolean.Create(aValue: Boolean);
begin
  Kind:=jjvkBoolean;
  Value:=aValue;
end;

function TJOB_Boolean.AsString: string;
begin
  str(Value,Result);
end;

{ TJOB_Double }

constructor TJOB_Double.Create(const aValue: Double);
begin
  Kind:=jjvkDouble;
  Value:=aValue;
end;

function TJOB_Double.AsString: string;
begin
  str(Value,Result);
end;

{ TJOB_String }

constructor TJOB_String.Create(const aValue: UnicodeString);
begin
  Kind:=jjvkString;
  Value:=aValue;
end;

function TJOB_String.AsString: string;
begin
  Result:=AnsiQuotedStr(String(Value),'"');
end;

{ TJOB_Object }

constructor TJOB_Object.Create(aValue: IJSObject);
begin
  Kind:=jjvkObject;
  Value:=aValue;
end;

function TJOB_Object.AsString: string;
begin
  if Value=nil then
    Result:='nil'
  else
    Result:='['+IntToStr(Value.GetJSObjectID)+']:'+Value.GetPascalClassName;
end;

{ TJOB_Method }

constructor TJOB_Method.Create(const aMethod: TMethod;
  const AnInvoke: TJOBCallback);
begin
  Kind:=jjvkMethod;
  Value:=aMethod;
  Invoke:=AnInvoke;
end;

function TJOB_Method.AsString: string;
begin
  Result:='Callback';
end;

{ TJOB_Dictionary }

procedure TJOB_Dictionary.Add(const aName: UnicodeString;
  const aValue: TJOB_JSValue);
var
  p: TJOB_Pair;
begin
  p.Name:=aName;
  p.Value:=aValue;
  Insert(p,Values,length(Values));
end;

constructor TJOB_Dictionary.Create(const Pairs: array of const);
var
  i: Integer;
  l, CurLen: SizeInt;
  CurName: UnicodeString;
  Value: TJOB_JSValue;
  p: Pointer;
  Obj: TObject;
  Intf: IJSObject;
begin
  inherited Create(jjvkDictionary);
  l:=length(Pairs);
  SetLength(Values,l div 2);
  for i:=0 to length(Values)-1 do
    Values[i].Value:=nil;
  i:=0;
  while i<l do
  begin
    case Pairs[i].VType of
    vtChar:
      CurName:=UnicodeString(Pairs[i].VChar);
    vtString:
      CurName:=UnicodeString(Pairs[i].VString^);
    vtPChar:
      begin
      CurLen:=strlen(Pairs[i].VPChar);
      SetString(CurName,Pairs[i].VPChar,CurLen);
      end;
    vtWideChar:
      CurName:=Pairs[i].VWideChar;
    vtAnsiString:
      CurName:=UnicodeString(PAnsiString(Pairs[i].VAnsiString)^);
    vtUnicodeString:
      CurName:=PUnicodeString(Pairs[i].VUnicodeString)^;
    else
      raise EJSArgParse.Create('TJOB_Dictionary.Create expected name at index '+IntToStr(i)+', but found '+IntToStr(Pairs[i].VType));
    end;
    Values[i div 2].Name:=CurName;
    inc(i);
    if i=l then
      raise EJSArgParse.Create('TJOB_Dictionary.Create name "'+String(CurName)+'" has no value');
    case Pairs[i].VType of
    vtInteger:
      Value:=TJOB_Double.Create(Pairs[i].VInteger);
    vtBoolean:
      Value:=TJOB_Boolean.Create(Pairs[i].VBoolean);
    vtChar:
      Value:=TJOB_String.Create(UnicodeString(Pairs[i].VChar));
    {$ifndef FPUNONE}
    vtExtended:
      Value:=TJOB_Double.Create(Pairs[i].VExtended^);
    {$endif}
    vtString:
      Value:=TJOB_String.Create(UnicodeString(Pairs[i].VString^));
    vtPointer:
      begin
      p:=Pairs[i].VPointer;
      if p=nil then
        Value:=TJOB_Object.Create(nil)
      else if p=JOB_Undefined then
        Value:=TJOB_JSValue.Create(jjvkUndefined)
      else
        raise EJSArgParse.Create('TJOB_Dictionary.Create pointer not supported, name='+String(CurName));
      end;
    vtPChar:
      begin
      CurLen:=strlen(Pairs[i].VPChar);
      SetString(CurName,Pairs[i].VPChar,CurLen);
      end;
    vtObject:
      begin
      Obj:=Pairs[i].VObject;
      if Obj=nil then
        Value:=TJOB_Object.Create(nil)
      else if Obj is TJOB_JSValue then
        Value:=TJOB_JSValue(Obj)
      else if Obj is TJSObject then
        Value:=TJOB_Object.Create(TJSObject(Obj) as IJSObject)
      else
        raise EJSArgParse.Create('TJOB_Dictionary.Create object '+Obj.ClassName+' not supported, name='+String(CurName));
      end;
    vtClass:
      raise EJSArgParse.Create('TJOB_Dictionary.Create class not supported, name='+String(CurName));
    vtWideChar:
      Value:=TJOB_String.Create(Pairs[i].VWideChar);
    vtPWideChar:
      raise EJSArgParse.Create('TJOB_Dictionary.Create vtPWideChar not supported, name='+String(CurName));
    vtAnsiString:
      Value:=TJOB_String.Create(UnicodeString(PAnsiString(Pairs[i].VAnsiString)^));
    vtCurrency:
      Value:=TJOB_Double.Create(Pairs[i].VCurrency^);
    vtVariant:
      raise EJSArgParse.Create('TJOB_Dictionary.Create vtVariant not supported, name='+String(CurName));
    vtInterface:
      begin
      Intf:=IJSObject(Pairs[i].VInterface);
      Value:=TJOB_Object.Create(Intf);
      end;
    vtWideString:
      raise EJSArgParse.Create('TJOB_Dictionary.Create vtWideString not supported, name='+String(CurName));
    vtInt64:
      Value:=TJOB_Double.Create(Pairs[i].VInt64^);
    vtQWord:
      Value:=TJOB_Double.Create(Pairs[i].VQWord^);
    vtUnicodeString:
      Value:=TJOB_String.Create(PUnicodeString(Pairs[i].VUnicodeString)^);
    else
      raise EJSArgParse.Create('TJOB_Dictionary.Create unsupported VType '+IntToStr(Pairs[i].VType)+' at index '+IntToStr(i));
    end;
    Values[i div 2].Value:=Value;
    inc(i);
  end;
end;

destructor TJOB_Dictionary.Destroy;
begin
  Clear;
  inherited Destroy;
end;

procedure TJOB_Dictionary.Clear;
var
  i: Integer;
begin
  for i:=0 to length(Values)-1 do
    FreeAndNil(Values[i].Value);
  Values:=nil;
end;

{ TJSObject }

function TJSObject.GetJSObjectID: TJOBObjectID;
begin
  Result:=FObjectID;
end;

function TJSObject.GetJSObjectCastSrc: IJSObject;
begin
  Result:=FCastSrc;
end;

function TJSObject.GetPascalClassName: string;
begin
  Result:=ClassName;
end;

function TJSObject.FetchString(Len: NativeInt): UnicodeString;
var
  ok: Boolean;
begin
  if Len=0 then exit('');
  ok:=false;
  try
    // try to allocate the memory
    SetLength(Result,Len);
    ok:=true;
  finally
    if not ok then
      __job_releasestringresult();
  end;
  __job_getstringresult(PByte(Result));
end;

function TJSObject.InvokeJSNoResultFunc(const aName: string;
  const Args: array of const; const InvokeFunc: TJOBInvokeNoResultFunc;
  Invoke: TJOBInvokeType): TJOBResult;
var
  InvokeArgs: PByte;
begin
  if length(Args)=0 then
    Result:=InvokeFunc(ObjectID,PChar(aName),length(aName),InvokeGetToInt[Invoke],nil)
  else begin
    InvokeArgs:=CreateInvokeJSArgs(Args);
    try
      Result:=InvokeFunc(ObjectID,PChar(aName),length(aName),InvokeGetToInt[Invoke],InvokeArgs);
    finally
      if InvokeArgs<>nil then
        FreeMem(InvokeArgs);
    end;
  end;
end;

function TJSObject.InvokeJSOneResult(const aName: string;
  const Args: array of const; const InvokeFunc: TJOBInvokeOneResultFunc;
  ResultP: PByte; Invoke: TJOBInvokeType): TJOBResult;
var
  InvokeArgs: PByte;
begin
  if length(Args)=0 then
    Result:=InvokeFunc(ObjectID,PChar(aName),length(aName),InvokeGetToInt[Invoke],nil,ResultP)
  else begin
    InvokeArgs:=CreateInvokeJSArgs(Args);
    try
      Result:=InvokeFunc(ObjectID,PChar(aName),length(aName),InvokeGetToInt[Invoke],InvokeArgs,ResultP);
    finally
      if InvokeArgs<>nil then
        FreeMem(InvokeArgs);
    end;
  end;
end;

procedure TJSObject.InvokeJS_Raise(const aName, Msg: string);
var
  E: EJSInvoke;
begin
  E:=EJSInvoke.Create(Msg);
  E.ObjectID:=ObjectID;
  E.FuncName:=aName;
  raise E;
end;

procedure TJSObject.InvokeJS_RaiseResultMismatch(const aName: string;
  Expected, Actual: TJOBResult);
begin
  case Actual of
  JOBResult_UnknownObjId: InvokeJS_Raise(aName,'unknown object id '+IntToStr(ObjectID));
  JOBResult_NotAFunction: InvokeJS_Raise(aName,'object '+IntToStr(ObjectID)+' does not have a function "'+aName+'"');
  else
    InvokeJS_RaiseResultMismatchStr(aName,JOBResult_Names[Expected],JOBResult_Names[Actual]);
  end;
end;

procedure TJSObject.InvokeJS_RaiseResultMismatchStr(const aName: string;
  const Expected, Actual: string);
begin
  InvokeJS_Raise(aName,'expected '+Expected+', but got '+Actual+' from object '+IntToStr(ObjectID)+' function "'+aName+'"');
end;

function TJSObject.CreateInvokeJSArgs(const Args: array of const): PByte;

  procedure RaiseNotSupported(const Msg: string);
  begin
    raise EJSInvoke.Create('Invoke js: type not supported '+Msg);
  end;

  procedure RaiseRange;
  begin
    raise ERangeError.Create('Invoke js: number out of bounds');
  end;

var
  p: PByte;

  function SizeOfTJOB_JSValue(JSValue: TJOB_JSValue): integer;
  var
    Dict: TJOB_PairArray;
    i: Integer;
  begin
    case JSValue.Kind of
      jjvkUndefined: Result:=1;
      jjvkBoolean: Result:=1;
      jjvkDouble: Result:=9;
      jjvkString: Result:=1+SizeOf(NativeInt)+SizeOf(PByte);
      jjvkObject:
        if TJOB_Object(JSValue).Value=nil then
          Result:=1
        else
          Result:=1+SizeOf(TJOBObjectID);
      jjvkMethod: Result:=1+3*SizeOf(PByte);
      jjvkDictionary:
        begin
          Result:=1+SizeOf(NativeInt);
          Dict:=TJOB_Dictionary(JSValue).Values;
          for i:=0 to length(Dict)-1 do
            begin
            inc(Result,1+SizeOf(NativeInt)+SizeOf(PByte));
            inc(Result,SizeOfTJOB_JSValue(Dict[i].Value));
            end;
        end;
      else
        RaiseNotSupported('20220630135718'){%H-};
    end;
  end;

  procedure AddBoolean(b: boolean);
  begin
    if b then
      p^:=JOBArgTrue
    else
      p^:=JOBArgFalse;
    inc(p);
  end;

  procedure AddLongInt(const i: LongInt);
  begin
    p^:=JOBArgLongint;
    inc(p);
    PLongint(p)^:=i;
    inc(p,4);
  end;

  procedure AddDouble(const d: double);
  begin
    p^:=JOBArgDouble;
    inc(p);
    PDouble(p)^:=d;
    inc(p,8);
  end;

  procedure AddChar(c: word);
  begin
    p^:=JOBArgChar;
    inc(p);
    PWord(p)^:=c;
    inc(p,2);
  end;

  procedure AddObjectID(const ObjId: TJOBObjectID);
  begin
    p^:=JOBArgObject;
    inc(p);
    PNativeInt(p)^:=ObjId;
    inc(p,sizeof(NativeInt));
  end;

  procedure AddIJSObject(const Intf: IJSObject);
  begin
    if Intf=nil then
    begin
      p^:=JOBArgNil;
      inc(p);
    end else
      AddObjectID(Intf.GetJSObjectID);
  end;

  procedure AddUTF8String(s: PByte; Len: NativeInt);
  begin
    p^:=JOBArgUTF8String;
    inc(p);
    PNativeInt(p)^:=Len;
    inc(p,sizeof(NativeInt));
    PPointer(p)^:=s;
    inc(p,sizeof(Pointer));
  end;

  procedure AddUnicodeString(s: PByte; Len: NativeInt); overload;
  begin
    p^:=JOBArgUnicodeString;
    inc(p);
    PNativeInt(p)^:=Len;
    inc(p,sizeof(NativeInt));
    PPointer(p)^:=s;
    inc(p,sizeof(Pointer));
  end;

  procedure AddUnicodeString(const us: UnicodeString); overload;
  begin
    if us='' then
      AddUnicodeString(nil,0)
    else
      AddUnicodeString(@us[1],length(us));
  end;

  procedure Add_TJOB_JSValue(aValue: TJOB_JSValue);
  var
    us: UnicodeString;
    h: PByte;
    aMethod: TJOB_Method;
    Dict: TJOB_PairArray;
    i: Integer;
  begin
    case aValue.Kind of
      jjvkUndefined:
        begin
          p^:=JOBArgUndefined;
          inc(p);
        end;
      jjvkBoolean:
        AddBoolean(TJOB_Boolean(aValue).Value);
      jjvkDouble:
        AddDouble(TJOB_Double(aValue).Value);
      jjvkString:
        begin
          us:=TJOB_String(aValue).Value;
          h:=PByte(PWideChar(us));
          AddUnicodeString(h,length(us));
        end;
      jjvkObject:
        AddIJSObject(TJOB_Object(aValue).Value);
      jjvkMethod:
        begin
          aMethod:=TJOB_Method(aValue);
          p^:=JOBArgMethod;
          inc(p);
          PPointer(p)^:=Pointer(aMethod.Invoke);
          inc(p,sizeof(Pointer));
          PPointer(p)^:=aMethod.Value.Data;
          inc(p,sizeof(Pointer));
          PPointer(p)^:=aMethod.Value.Code;
          inc(p,sizeof(Pointer));
        end;
      jjvkDictionary:
        begin
          Dict:=TJOB_Dictionary(aValue).Values;
          p^:=JOBArgDictionary;
          inc(p);
          PNativeInt(p)^:=length(Dict);
          for i:=0 to length(Dict)-1 do
          begin
            AddUnicodeString(Dict[i].Name);
            Add_TJOB_JSValue(Dict[i].Value);
          end;
        end;
    end;
  end;

var
  i, Len: Integer;
  qw: QWord;
  i64: Int64;
  h: PByte;
  s: String;
  ws: WideString;
  us: UnicodeString;
  d: Double;
  Obj: TObject;
  JSValue: TJOB_JSValue;
begin
  Result:=nil;
  if length(Args)>255 then
    raise EJSInvoke.Create('Invoke js: too many args');

  Len:=1;
  for i:=0 to high(Args) do
  begin
    {$IFDEF VerboseInvokeJSArgs}
    writeln('TJSObject.CreateInvokeJSArgs ',i,' VType=',Args[i].VType);
    {$ENDIF}
    case Args[i].VType of
    vtInteger       : inc(Len,5);
    vtBoolean       : inc(Len);
    vtChar,
    vtWideChar      : inc(Len,3);
    {$ifndef FPUNONE}
    vtExtended:
      begin
        d:=double(Args[i].VExtended^);
        if d=0 then ;
        inc(Len,9);
      end;
    {$endif}
    vtString        : inc(Len,1+SizeOf(NativeInt)+SizeOf(PByte));
    vtPointer:
      begin
        p:=Args[i].VPointer;
        if p=JOB_Undefined then
          inc(Len)
        else
          inc(Len,1+SizeOf(PByte));
      end;
    vtPChar:
      begin
        // check length
        strlen(Args[i].VPChar);
        inc(Len,1+SizeOf(NativeInt)+SizeOf(PByte));
      end;
    vtObject:
      begin
        Obj:=Args[i].VObject;
        if Obj=nil then
          inc(Len,1)
        else if Obj is TJSObject then
          inc(Len,1+sizeof(TJOBObjectID))
        else if Obj is TJOB_JSValue then
        begin
          JSValue:=TJOB_JSValue(Obj);
          inc(Len,SizeOfTJOB_JSValue(JSValue));
        end else
          RaiseNotSupported('object');
      end;
    vtClass         : RaiseNotSupported('class');
    vtPWideChar:
      begin
        // check length
        strlen(Args[i].VPWideChar);
        inc(Len,1+SizeOf(NativeInt)+SizeOf(PByte));
      end;
    vtAnsiString:
      inc(Len,1+SizeOf(NativeInt)+SizeOf(PByte));
    vtCurrency      : RaiseNotSupported('currency');
    {$ifdef FPC_HAS_FEATURE_VARIANTS}
    vtVariant       : RaiseNotSupported('variant');
    {$endif FPC_HAS_FEATURE_VARIANTS}
    vtInterface:
      begin
        p:=Args[i].VInterface;
        if p=nil then
          inc(Len,1)
        else if IInterface(p) is IJSObject then
          inc(Len,1+sizeof(TJOBObjectID))
        else
          RaiseNotSupported('interface');
      end;
    vtWideString:
      inc(Len,1+SizeOf(NativeInt)+SizeOf(PByte));
    vtInt64:
      begin
        i64:=Args[i].VInt64^;
        if (i64<MinSafeIntDouble) or (i64>MaxSafeIntDouble) then
          RaiseRange;
        if (i64>=low(longint)) and (i64<=high(longint)) then
          inc(Len,5)
        else
          inc(Len,9);
      end;
    vtUnicodeString:
      inc(Len,1+SizeOf(NativeInt)+SizeOf(PByte));
    vtQWord:
      begin
        qw:=Args[i].VQWord^;
        if (qw>MaxSafeIntDouble) then
          RaiseRange;
        if (qw<=high(longint)) then
          inc(Len,5)
        else
          inc(Len,9);
      end;
    else
      RaiseNotSupported(IntToStr(Args[i].VType));
    end;
  end;

  Result:=GetMem(Len);
  p:=Result;
  p^:=length(Args);
  inc(p);
  for i:=0 to high(Args) do
  begin
    case Args[i].VType of
    vtInteger:
      AddLongInt(Args[i].VInteger);
    vtBoolean:
      AddBoolean(Args[i].VBoolean);
    {$ifndef FPUNONE}
    vtExtended:
      AddDouble(double(Args[i].VExtended^));
    {$endif}
    vtChar:
      AddChar(ord(Args[i].VChar));
    vtWideChar:
      AddChar(ord(Args[i].VWideChar));
    vtString:
      begin
        // shortstring
        h:=PByte(Args[i].VString);
        AddUTF8String(h+1,h^);
      end;
    vtPointer:
      begin
        h:=Args[i].VPointer;
        if h=nil then
        begin
          p^:=JOBArgNil;
          inc(p);
        end else if h=JOB_Undefined then
        begin
          p^:=JOBArgUndefined;
          inc(p);
        end
        else begin
          p^:=JOBArgPointer;
          inc(p);
          PPointer(p)^:=h;
          inc(p,sizeof(Pointer));
        end;
      end;
    vtPChar:
      begin
        h:=PByte(Args[i].VPChar);
        AddUTF8String(h,strlen(PChar(h)));
      end;
    vtObject:
      begin
        Obj:=Args[i].VObject;
        if Obj=nil then
        begin
          p^:=JOBArgNil;
          inc(p);
        end else if Obj is TJSObject then
          AddObjectID(TJSObject(Obj).ObjectID)
        else if Obj is TJOB_JSValue then
        begin
          JSValue:=TJOB_JSValue(Obj);
          Add_TJOB_JSValue(JSValue);
        end else
          RaiseNotSupported(Obj.ClassName);
      end;
    vtClass: ;
    vtPWideChar:
      begin
        h:=PByte(Args[i].VPWideChar);
        AddUnicodeString(h,strlen(PWideChar(h)));
      end;
    vtAnsiString:
      begin
        h:=Args[i].VAnsiString;
        s:=AnsiString(h);
        AddUTF8String(h,length(s));
      end;
    vtCurrency      : ;
    {$ifdef FPC_HAS_FEATURE_VARIANTS}
    vtVariant       : ;
    {$endif FPC_HAS_FEATURE_VARIANTS}
    vtInterface:
      begin
        h:=Args[i].VInterface;
        AddIJSObject(IJSObject(h));
      end;
    vtWideString:
      begin
        h:=Args[i].VWideString;
        ws:=WideString(h);
        AddUnicodeString(h,length(ws));
      end;
    vtInt64:
      begin
        i64:=Args[i].VInt64^;
        if (i64>=low(longint)) and (i64<=high(longint)) then
          AddLongInt(i64)
        else
          AddDouble(i64);
      end;
    vtUnicodeString:
      begin
        h:=Args[i].VUnicodeString;
        us:=UnicodeString(h);
        AddUnicodeString(h,length(us));
      end;
    vtQWord:
      begin
        qw:=Args[i].VQWord^;
        if (qw<=high(longint)) then
          AddLongInt(qw)
        else
          AddDouble(qw);
      end;
    end;
  end;

  {$IFDEF VerboseInvokeJSArgs}
  s:='TJSObject.CreateInvokeJSArgs ArgCnt='+IntToStr(length(Args));
  for i:=0 to high(Args) do
    s:=s+' '+GetVarRecName(Args[i].VType);
  s:=s+' Len='+IntToStr(Len);
  s:=s+' Bytes=';
  for i:=0 to Len-1 do
    s:=s+HexStr(Result[i],2);
  writeln(s);
  {$ENDIF}
end;

constructor TJSObject.Cast(Intf: IJSObject);
begin
  FObjectID:=Intf.GetJSObjectID;
  FCastSrc:=Intf.GetJSObjectCastSrc;
  if FCastSrc=nil then
    FCastSrc:=Intf;
end;

constructor TJSObject.CreateFromID(aID: TJOBObjectID);
begin
  FObjectID:=aID;
end;

destructor TJSObject.Destroy;
begin
  if FCastSrc<>nil then
    FCastSrc:=nil
  else if ObjectID>=0 then
    __job_release_object(ObjectID);
  FObjectID:=0;
  inherited Destroy;
end;

procedure TJSObject.InvokeJSNoResult(const aName: string;
  const Args: array of const; Invoke: TJOBInvokeType);
var
  aError: TJOBResult;
begin
  aError:=InvokeJSNoResultFunc(aName,Args,@__job_invoke_noresult,Invoke);
  if aError<>JOBResult_Success then
    InvokeJS_RaiseResultMismatch(aName,JOBResult_Success,aError);
end;

function TJSObject.InvokeJSBooleanResult(const aName: string;
  const Args: array of const; Invoke: TJOBInvokeType): Boolean;
var
  aError: TJOBResult;
  b: bytebool;
begin
  b:=false;
  aError:=InvokeJSOneResult(aName,Args,@__job_invoke_boolresult,@b,Invoke);
  if aError=JOBResult_Boolean then
  else if aError=JOBResult_Undefined then
    b:=false
  else
    InvokeJS_RaiseResultMismatch(aName,JOBResult_Boolean,aError);
  Result:=b;
end;

function TJSObject.InvokeJSDoubleResult(const aName: string;
  const Args: array of const; Invoke: TJOBInvokeType): Double;
var
  aError: TJOBResult;
begin
  Result:=NaN;
  aError:=InvokeJSOneResult(aName,Args,@__job_invoke_doubleresult,@Result,Invoke);
  if aError=JOBResult_Double then
  else if aError=JOBResult_Undefined then
    Result:=NaN
  else
    InvokeJS_RaiseResultMismatch(aName,JOBResult_Double,aError);
end;

function TJSObject.InvokeJSUnicodeStringResult(const aName: string;
  const Args: array of const; Invoke: TJOBInvokeType): UnicodeString;
var
  ResultLen: NativeInt;
  aError: TJOBResult;
begin
  ResultLen:=0;
  aError:=InvokeJSOneResult(aName,Args,@__job_invoke_stringresult,@ResultLen,Invoke);
  if aError=JOBResult_String then
    Result:=FetchString(ResultLen)
  else begin
    Result:='';
    if aError<>JOBResult_Undefined then
      InvokeJS_RaiseResultMismatch(aName,JOBResult_String,aError);
  end;
  //writeln('TJSObject.InvokeJSUnicodeStringResult Result="',Result,'"');
end;

function TJSObject.InvokeJSObjectResult(const aName: string;
  const Args: array of const; aResultClass: TJSObjectClass;
  Invoke: TJOBInvokeType): TJSObject;
var
  aError: TJOBResult;
  NewObjId: TJOBObjectID;
begin
  Result:=nil;
  NewObjId:=-1;
  aError:=InvokeJSOneResult(aName,Args,@__job_invoke_objectresult,@NewObjId,Invoke);
  if (aError=JOBResult_Null) or (aError=JOBResult_Undefined) then
    exit;
  if aError<>JOBResult_Object then
    InvokeJS_RaiseResultMismatch(aName,JOBResult_Object,aError);

  Result:=aResultClass.CreateFromID(NewObjId);
end;

function TJSObject.InvokeJSValueResult(const aName: string;
  const Args: array of const; Invoke: TJOBInvokeType): TJOB_JSValue;
var
  Buf: array[0..7] of byte;
  p: PByte;
  aError: TJOBResult;
  Obj: TJSObject;
begin
  Result:=nil;
  FillByte(Buf[0],length(Buf),0);
  p:=@Buf[0];
  aError:=InvokeJSOneResult(aName,Args,@__job_invoke_jsvalueresult,p,Invoke);
  case aError of
  JOBResult_Undefined:
    Result:=TJOB_JSValue.Create(jjvkUndefined);
  JOBResult_Null:
    Result:=TJOB_Object.Create(nil);
  JOBResult_Boolean:
    Result:=TJOB_Boolean.Create(p^<>0);
  JOBResult_Double:
    Result:=TJOB_Double.Create(PDouble(p)^);
  JOBResult_String:
    Result:=TJOB_String.Create(FetchString(PNativeInt(p)^));
  JOBResult_Function,
  JOBResult_Object:
    begin
    Obj:=TJSObject.CreateFromID(PJOBObjectID(p)^);
    Result:=TJOB_Object.Create(Obj);
    end;
  else
    InvokeJS_RaiseResultMismatchStr(aName,'jsvalue',JOBResult_Names[aError]);
  end;
end;

function TJSObject.InvokeJSUtf8StringResult(const aName: string;
  const args: array of const; Invoke: TJOBInvokeType): String;
begin
  Result:=String(InvokeJSUnicodeStringResult(aName,Args,Invoke));
end;

function TJSObject.InvokeJSLongIntResult(const aName: string;
  const args: array of const; Invoke: TJOBInvokeType): LongInt;
var
  d: Double;
begin
  d:=InvokeJSDoubleResult(aName,Args,Invoke);
  if (Frac(d)<>0) or (d<low(longint)) or (d>high(longint)) then
    InvokeJS_RaiseResultMismatchStr(aName,'longint','double')
  else
    Result:=Trunc(d);
end;

function TJSObject.InvokeJSMaxIntResult(const aName: string;
  const args: array of const; Invoke: TJOBInvokeType): int64;
var
  d: Double;
begin
  d:=InvokeJSDoubleResult(aName,Args,Invoke);
  if (Frac(d)<>0) or (d<low(int64)) or (d>high(int64)) then
    InvokeJS_RaiseResultMismatchStr(aName,'int64','double')
  else
    Result:=Trunc(d);
end;

function TJSObject.InvokeJSTypeOf(const aName: string;
  const Args: array of const): TJOBResult;
begin
  Result:=InvokeJSNoResultFunc(aName,Args,@__job_invoke_noresult,jiGetTypeOf);
end;

function TJSObject.ReadJSPropertyBoolean(const aName: string): boolean;
begin
  Result:=InvokeJSBooleanResult(aName,[],jiGet);
end;

function TJSObject.ReadJSPropertyDouble(const aName: string): double;
begin
  Result:=InvokeJSDoubleResult(aName,[],jiGet);
end;

function TJSObject.ReadJSPropertyUnicodeString(const aName: string
  ): UnicodeString;
begin
  Result:=InvokeJSUnicodeStringResult(aName,[],jiGet);
end;

function TJSObject.ReadJSPropertyObject(const aName: string;
  aResultClass: TJSObjectClass): TJSObject;
begin
  Result:=InvokeJSObjectResult(aName,[],aResultClass,jiGet);
end;

function TJSObject.ReadJSPropertyUtf8String(const aName: string): string;
begin
  Result:=InvokeJSUtf8StringResult(aName,[],jiGet);
end;

function TJSObject.ReadJSPropertyLongInt(const aName: string): LongInt;
begin
  Result:=InvokeJSLongIntResult(aName,[],jiGet);
end;

function TJSObject.ReadJSPropertyInt64(const aName: string): Int64;
begin
  Result:=Trunc(InvokeJSDoubleResult(aName,[],jiGet));
end;

function TJSObject.ReadJSPropertyValue(const aName: string): TJOB_JSValue;
begin
  Result:=InvokeJSValueResult(aName,[],jiGet);
end;

procedure TJSObject.WriteJSPropertyBoolean(const aName: string; Value: Boolean);
begin
  InvokeJSNoResult(aName,[Value],jiSet);
end;

procedure TJSObject.WriteJSPropertyDouble(const aName: string; Value: Double);
begin
  InvokeJSNoResult(aName,[Value],jiSet);
end;

procedure TJSObject.WriteJSPropertyUnicodeString(const aName: string;
  const Value: UnicodeString);
begin
  InvokeJSNoResult(aName,[Value],jiSet);
end;

procedure TJSObject.WriteJSPropertyUtf8String(const aName: string;
  const Value: String);
begin
  InvokeJSNoResult(aName,[Value],jiSet);
end;

procedure TJSObject.WriteJSPropertyObject(const aName: string; Value: IJSObject
  );
begin
  InvokeJSNoResult(aName,[Value],jiSet);
end;

procedure TJSObject.WriteJSPropertyLongInt(const aName: string; Value: LongInt);
begin
  InvokeJSNoResult(aName,[Value],jiSet);
end;

procedure TJSObject.WriteJSPropertyValue(const aName: string;
  Value: TJOB_JSValue);
begin
  InvokeJSNoResult(aName,[Value],jiSet);
end;

function TJSObject.NewJSObject(const Args: array of const;
  aResultClass: TJSObjectClass): TJSObject;
begin
  Result:=InvokeJSObjectResult('',Args,aResultClass,jiNew);
end;

initialization
  JSObject:=TJSObject.CreateFromID(JOBObjIdObject);

end.

