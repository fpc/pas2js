{
  JOB - JS Object Bridge for Webassembly

  Webassembly unit giving access to the browser DOM.

  see https://wiki.freepascal.org/WebAssembly/DOM
}
unit JOB_WAsm;

{$mode ObjFPC}{$H+}

{$define VerboseJOB}

interface

uses
  SysUtils, Types, Math, Classes, JOB_Shared;

const
  MinSafeIntDouble = -$1fffffffffffff; // -9007199254740991 54 bits (52 plus signed bit plus implicit highest bit)
  MaxSafeIntDouble =  $1fffffffffffff; //  9007199254740991

Type
  EJSObject = class(Exception);
  EJSInvoke = class(EJSObject)
  public
    ObjectID: TJOBObjectID;
    FuncName: string;
  end;

  TJOB_JSValueKind = (
    jjvkUndefined,
    jjvkBoolean,
    jjvkDouble,
    jjvkString,
    jjvkObject
    );
  TJOB_JSValueKinds = set of TJOB_JSValueKind;

const
  JOB_JSValueKindNames: array[TJOB_JSValueKind] of string = (
    'Undefined',
    'Boolean',
    'Double',
    'String',
    'Object'
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

  { TJOB_JSValueBoolean }

  TJOB_JSValueBoolean = class(TJOB_JSValue)
  public
    Value: Boolean;
    constructor Create(aValue: Boolean);
    function AsString: string; override;
  end;

  { TJOB_JSValueDouble }

  TJOB_JSValueDouble = class(TJOB_JSValue)
  public
    Value: Double;
    constructor Create(const aValue: Double);
    function AsString: string; override;
  end;

  { TJOB_JSValueString }

  TJOB_JSValueString = class(TJOB_JSValue)
  public
    Value: UnicodeString;
    constructor Create(const aValue: UnicodeString);
    function AsString: string; override;
  end;

  PJOBObjectID = ^TJOBObjectID;
  TJOBInvokeOneResultFunc = function(
      ObjID: TJOBObjectID;
      NameP: PChar;
      NameLen: longint;
      Invoke: longint;
      ArgP: PByte;
      ResultP: PByte
    ): TJOBResult;

  TJOBInvokeGetType = (
    jigCall,  // call function
    jigGetter, // read property
    jigNew // new operator
    );
  TJOBInvokeSetType = (
    jisCall,  // call function
    jisSetter // write property
    );

  TJSObject = class;
  TJSObjectClass = class of TJSObject;

  { IJSObject }

  IJSObject = interface
    ['{BE5CDE03-D471-4AB3-8F27-A5EA637416F7}']
    function GetJSObjectID: TJOBObjectID;
    function GetPascalClassName: string;
    procedure InvokeJSNoResult(const aName: string; Const Args: Array of const; Invoke: TJOBInvokeSetType = jisCall); virtual;
    function InvokeJSBooleanResult(const aName: string; Const Args: Array of const; Invoke: TJOBInvokeGetType = jigCall): Boolean; virtual;
    function InvokeJSDoubleResult(const aName: string; Const Args: Array of const; Invoke: TJOBInvokeGetType = jigCall): Double; virtual;
    function InvokeJSUnicodeStringResult(const aName: string; Const Args: Array of const; Invoke: TJOBInvokeGetType = jigCall): UnicodeString; virtual;
    function InvokeJSObjectResult(const aName: string; Const Args: Array of const; aResultClass: TJSObjectClass; Invoke: TJOBInvokeGetType = jigCall): TJSObject; virtual;
    function InvokeJSValueResult(const aName: string; Const Args: Array of const; Invoke: TJOBInvokeGetType = jigCall): TJOB_JSValue; virtual;
    function InvokeJSUtf8StringResult(const aName: string; Const args: Array of const; Invoke: TJOBInvokeGetType = jigCall): String; virtual;
    function InvokeJSLongIntResult(const aName: string; Const args: Array of const; Invoke: TJOBInvokeGetType = jigCall): LongInt; virtual;
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
    procedure WriteJSPropertyObject(const aName: string; Value: TJSObject); virtual;
    procedure WriteJSPropertyLongInt(const aName: string; Value: LongInt); virtual;
    function NewJSObject(Const Args: Array of const; aResultClass: TJSObjectClass): TJSObject; virtual;
  end;

  { TJOB_JSValueObject }

  TJOB_JSValueObject = class(TJOB_JSValue)
  public
    Value: IJSObject;
    constructor Create(aValue: IJSObject);
    function AsString: string; override;
  end;

  { TJSObject }

  TJSObject = class(TInterfacedObject,IJSObject)
  private
    FObjectID: TJOBObjectID;
  protected
    function GetJSObjectID: TJOBObjectID;
    function GetPascalClassName: string;
    function FetchString(Len: NativeInt): UnicodeString;
    function InvokeJSOneResult(const aName: string; Const Args: Array of const;
      const InvokeFunc: TJOBInvokeOneResultFunc; ResultP: PByte; Invoke: TJOBInvokeGetType): TJOBResult;
    procedure InvokeJS_Raise(const aName, Msg: string); virtual;
    procedure InvokeJS_RaiseResultMismatch(const aName: string; Expected, Actual: TJOBResult); virtual;
    procedure InvokeJS_RaiseResultMismatchStr(const aName: string; const Expected, Actual: string); virtual;
    function CreateInvokeJSArgs(const Args: array of const): PByte; virtual;
  public
    constructor CreateFromID(aID: TJOBObjectID); virtual;
    destructor Destroy; override;
    property ObjectID: TJOBObjectID read FObjectID;
    procedure InvokeJSNoResult(const aName: string; Const Args: Array of const; Invoke: TJOBInvokeSetType = jisCall); virtual;
    function InvokeJSBooleanResult(const aName: string; Const Args: Array of const; Invoke: TJOBInvokeGetType = jigCall): Boolean; virtual;
    function InvokeJSDoubleResult(const aName: string; Const Args: Array of const; Invoke: TJOBInvokeGetType = jigCall): Double; virtual;
    function InvokeJSUnicodeStringResult(const aName: string; Const Args: Array of const; Invoke: TJOBInvokeGetType = jigCall): UnicodeString; virtual;
    function InvokeJSObjectResult(const aName: string; Const Args: Array of const; aResultClass: TJSObjectClass; Invoke: TJOBInvokeGetType = jigCall): TJSObject; virtual;
    function InvokeJSValueResult(const aName: string; Const Args: Array of const; Invoke: TJOBInvokeGetType = jigCall): TJOB_JSValue; virtual;
    function InvokeJSUtf8StringResult(const aName: string; Const args: Array of const; Invoke: TJOBInvokeGetType = jigCall): String; virtual;
    function InvokeJSLongIntResult(const aName: string; Const args: Array of const; Invoke: TJOBInvokeGetType = jigCall): LongInt; virtual;
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
    procedure WriteJSPropertyObject(const aName: string; Value: TJSObject); virtual;
    procedure WriteJSPropertyLongInt(const aName: string; Value: LongInt); virtual;
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

implementation

const
  InvokeGetToInt: array[TJOBInvokeGetType] of integer = (
    JOBInvokeCall,
    JOBInvokeGet,
    JOBInvokeNew
    );
  InvokeSetToInt: array[TJOBInvokeSetType] of integer = (
    JOBInvokeCall,
    JOBInvokeSet
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

function __job_callback(w: NativeInt): boolean;
begin
  writeln('__job_callback w=',w);
  Result:=true;
end;

{$ENDIF}

{ TJOB_JSValue }

constructor TJOB_JSValue.Create(aKind: TJOB_JSValueKind);
begin
  Kind:=aKind;
end;

function TJOB_JSValue.AsString: string;
begin
  case Kind of
    jjvkUndefined: Result:='undefined';
    jjvkBoolean: ;
    jjvkDouble: ;
    jjvkString: ;
    jjvkObject: ;
  end;
end;

{ TJOB_JSValueBoolean }

constructor TJOB_JSValueBoolean.Create(aValue: Boolean);
begin
  Kind:=jjvkBoolean;
  Value:=aValue;
end;

function TJOB_JSValueBoolean.AsString: string;
begin
  str(Value,Result);
end;

{ TJOB_JSValueDouble }

constructor TJOB_JSValueDouble.Create(const aValue: Double);
begin
  Kind:=jjvkDouble;
  Value:=aValue;
end;

function TJOB_JSValueDouble.AsString: string;
begin
  str(Value,Result);
end;

{ TJOB_JSValueString }

constructor TJOB_JSValueString.Create(const aValue: UnicodeString);
begin
  Kind:=jjvkString;
  Value:=aValue;
end;

function TJOB_JSValueString.AsString: string;
begin
  Result:=AnsiQuotedStr(String(Value),'"');
end;

{ TJOB_JSValueObject }

constructor TJOB_JSValueObject.Create(aValue: IJSObject);
begin
  Kind:=jjvkObject;
  Value:=aValue;
end;

function TJOB_JSValueObject.AsString: string;
begin
  if Value=nil then
    Result:='nil'
  else
    Result:='['+IntToStr(Value.GetJSObjectID)+']:'+Value.GetPascalClassName;
end;

{ TJSObject }

function TJSObject.GetJSObjectID: TJOBObjectID;
begin
  Result:=FObjectID;
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

function TJSObject.InvokeJSOneResult(const aName: string;
  const Args: array of const; const InvokeFunc: TJOBInvokeOneResultFunc;
  ResultP: PByte; Invoke: TJOBInvokeGetType): TJOBResult;
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
  i, Len: Integer;
  qw: QWord;
  i64: Int64;
  p, h: PByte;
  s: String;
  ws: WideString;
  us: UnicodeString;
  d: Double;
  Obj: TObject;
begin
  Result:=nil;
  if length(Args)>255 then
    raise EJSInvoke.Create('Invoke js: too many args');

  Len:=1;
  for i:=0 to high(Args) do
  begin
    case Args[i].VType of
    vtInteger       : inc(Len,5);
    vtBoolean       : inc(Len);
    vtChar,
    vtWideChar      : inc(Len,3);
    {$ifndef FPUNONE}
    vtExtended      :
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
    vtPChar         :
      begin
        strlen(Args[i].VPChar);
        inc(Len,1+SizeOf(NativeInt)+SizeOf(PByte));
      end;
    vtObject        :
      begin
        Obj:=Args[i].VObject;
        if Obj=nil then
          inc(Len,1)
        else if Obj is TJSObject then
          inc(Len,1+sizeof(TJOBObjectID))
        else
          RaiseNotSupported('object');
      end;
    vtClass         : RaiseNotSupported('class');
    vtPWideChar     : RaiseNotSupported('pwidechar');
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
    vtUnicodeString : inc(Len,1+SizeOf(NativeInt)+SizeOf(PByte));
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
      begin
        p^:=JOBArgLongint;
        inc(p);
        PLongint(p)^:=Args[i].VInteger;
        inc(p,4);
      end;
    vtBoolean:
      begin
        if Args[i].VBoolean then
          p^:=JOBArgTrue
        else
          p^:=JOBArgFalse;
        inc(p);
      end;
    {$ifndef FPUNONE}
    vtExtended:
      begin
        p^:=JOBArgDouble;
        inc(p);
        PDouble(p)^:=double(Args[i].VExtended^);
        inc(p,8);
      end;
    {$endif}
    vtChar:
      begin
        p^:=JOBArgChar;
        inc(p);
        PWord(p)^:=ord(Args[i].VChar);
        inc(p,2);
      end;
    vtWideChar:
      begin
        p^:=JOBArgChar;
        inc(p);
        PWord(p)^:=ord(Args[i].VWideChar);
        inc(p,2);
      end;
    vtString:
      begin
        // shortstring
        p^:=JOBArgUTF8String;
        inc(p);
        h:=PByte(Args[i].VString);
        PNativeInt(p)^:=h^;
        inc(h);
        inc(p,sizeof(NativeInt));
        PPointer(p)^:=h;
        inc(p,sizeof(Pointer));
      end;
    vtPointer:
      begin
        h:=Args[i].VPointer;
        if h=JOB_Undefined then
        begin
          p^:=JOBArgNone;
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
        p^:=JOBArgUTF8String;
        inc(p);
        h:=PByte(Args[i].VPChar);
        PNativeInt(p)^:=strlen(PChar(h));
        inc(p,sizeof(NativeInt));
        PPointer(p)^:=h;
        inc(p,sizeof(Pointer));
      end;
    vtObject:
      begin
        Obj:=Args[i].VObject;
        if Obj=nil then
        begin
          p^:=JOBArgNil;
          inc(p);
        end else begin
          p^:=JOBArgObject;
          inc(p);
          PNativeInt(p)^:=TJSObject(Obj).ObjectID;
          inc(p,sizeof(NativeInt));
        end;
      end;
    vtClass         : ;
    vtPWideChar     : ;
    vtAnsiString    :
      begin
        p^:=JOBArgUTF8String;
        inc(p);
        h:=Args[i].VAnsiString;
        s:=AnsiString(h);
        PNativeInt(p)^:=length(s);
        inc(p,sizeof(NativeInt));
        PPointer(p)^:=h;
        inc(p,sizeof(Pointer));
      end;
    vtCurrency      : ;
    {$ifdef FPC_HAS_FEATURE_VARIANTS}
    vtVariant       : ;
    {$endif FPC_HAS_FEATURE_VARIANTS}
    vtInterface:
      begin
        h:=Args[i].VInterface;
        if h=nil then
        begin
          p^:=JOBArgNil;
          inc(p);
        end else begin
          p^:=JOBArgObject;
          inc(p);
          PNativeInt(p)^:=IJSObject(h).GetJSObjectID;
          inc(p,sizeof(NativeInt));
        end;
      end;
    vtWideString:
      begin
        p^:=JOBArgUnicodeString;
        inc(p);
        h:=Args[i].VWideString;
        ws:=WideString(h);
        PNativeInt(p)^:=length(ws);
        inc(p,sizeof(NativeInt));
        PPointer(p)^:=h;
        inc(p,sizeof(Pointer));
      end;
    vtInt64:
      begin
        i64:=Args[i].VInt64^;
        if (i64>=low(longint)) and (i64<=high(longint)) then
        begin
          p^:=JOBArgLongint;
          inc(p);
          PLongint(p)^:=i64;
          inc(p,4);
        end else begin
          p^:=JOBArgDouble;
          inc(p);
          PDouble(p)^:=i64;
          inc(p,8);
        end;
      end;
    vtUnicodeString:
      begin
        p^:=JOBArgUnicodeString;
        inc(p);
        h:=Args[i].VUnicodeString;
        us:=UnicodeString(h);
        PNativeInt(p)^:=length(us);
        inc(p,sizeof(NativeInt));
        PPointer(p)^:=h;
        inc(p,sizeof(Pointer));
      end;
    vtQWord:
      begin
        qw:=Args[i].VQWord^;
        if (qw<=high(longint)) then
        begin
          p^:=JOBArgLongint;
          inc(p);
          PLongint(p)^:=qw;
          inc(p,4);
        end else begin
          p^:=JOBArgDouble;
          inc(p);
          PDouble(p)^:=qw;
          inc(p,8);
        end;
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

constructor TJSObject.CreateFromID(aID: TJOBObjectID);
begin
  FObjectID:=aID;
end;

destructor TJSObject.Destroy;
begin
  if ObjectID>=0 then
    __job_release_object(ObjectID);
  inherited Destroy;
end;

procedure TJSObject.InvokeJSNoResult(const aName: string;
  const Args: array of const; Invoke: TJOBInvokeSetType);
var
  aError: TJOBResult;
  InvokeArgs: PByte;
begin
  if length(Args)=0 then
    aError:=__job_invoke_noresult(ObjectID,PChar(aName),length(aName),InvokeSetToInt[Invoke],nil)
  else begin
    InvokeArgs:=CreateInvokeJSArgs(Args);
    try
      aError:=__job_invoke_noresult(ObjectID,PChar(aName),length(aName),InvokeSetToInt[Invoke],InvokeArgs);
    finally
      if InvokeArgs<>nil then
        FreeMem(InvokeArgs);
    end;
  end;
  if aError<>JOBResult_Success then
    InvokeJS_RaiseResultMismatch(aName,JOBResult_Success,aError);
end;

function TJSObject.InvokeJSBooleanResult(const aName: string;
  const Args: array of const; Invoke: TJOBInvokeGetType): Boolean;
var
  aError: TJOBResult;
  b: bytebool;
begin
  b:=false;
  aError:=InvokeJSOneResult(aName,Args,@__job_invoke_boolresult,@b,Invoke);
  if aError<>JOBResult_Boolean then
    InvokeJS_RaiseResultMismatch(aName,JOBResult_Boolean,aError);
  Result:=b;
end;

function TJSObject.InvokeJSDoubleResult(const aName: string;
  const Args: array of const; Invoke: TJOBInvokeGetType): Double;
var
  aError: TJOBResult;
begin
  Result:=NaN;
  aError:=InvokeJSOneResult(aName,Args,@__job_invoke_doubleresult,@Result,Invoke);
  if aError<>JOBResult_Double then
    InvokeJS_RaiseResultMismatch(aName,JOBResult_Double,aError);
end;

function TJSObject.InvokeJSUnicodeStringResult(const aName: string;
  const Args: array of const; Invoke: TJOBInvokeGetType): UnicodeString;
var
  ResultLen: NativeInt;
  aError: TJOBResult;
begin
  ResultLen:=0;
  aError:=InvokeJSOneResult(aName,Args,@__job_invoke_stringresult,@ResultLen,Invoke);
  if aError<>JOBResult_String then
    InvokeJS_RaiseResultMismatch(aName,JOBResult_String,aError);
  Result:=FetchString(ResultLen);
  //writeln('TJSObject.InvokeJSUnicodeStringResult Result="',Result,'"');
end;

function TJSObject.InvokeJSObjectResult(const aName: string;
  const Args: array of const; aResultClass: TJSObjectClass;
  Invoke: TJOBInvokeGetType): TJSObject;
var
  aError: TJOBResult;
  NewObjId: TJOBObjectID;
begin
  Result:=nil;
  NewObjId:=-1;
  aError:=InvokeJSOneResult(aName,Args,@__job_invoke_objectresult,@NewObjId,Invoke);
  if aError=JOBResult_Null then
    exit;
  if aError<>JOBResult_Object then
    InvokeJS_RaiseResultMismatch(aName,JOBResult_Object,aError);

  Result:=aResultClass.CreateFromID(NewObjId);
end;

function TJSObject.InvokeJSValueResult(const aName: string;
  const Args: array of const; Invoke: TJOBInvokeGetType): TJOB_JSValue;
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
    Result:=TJOB_JSValueObject.Create(nil);
  JOBResult_Boolean:
    Result:=TJOB_JSValueBoolean.Create(p^<>0);
  JOBResult_Double:
    Result:=TJOB_JSValueDouble.Create(PDouble(p)^);
  JOBResult_String:
    Result:=TJOB_JSValueString.Create(FetchString(PNativeInt(p)^));
  JOBResult_Function,
  JOBResult_Object:
    begin
    Obj:=TJSObject.CreateFromID(PJOBObjectID(p)^);
    Result:=TJOB_JSValueObject.Create(Obj);
    end;
  else
    InvokeJS_RaiseResultMismatchStr(aName,'jsvalue',JOBResult_Names[aError]);
  end;
end;

function TJSObject.InvokeJSUtf8StringResult(const aName: string;
  const args: array of const; Invoke: TJOBInvokeGetType): String;
begin
  Result:=String(InvokeJSUnicodeStringResult(aName,Args,Invoke));
end;

function TJSObject.InvokeJSLongIntResult(const aName: string;
  const args: array of const; Invoke: TJOBInvokeGetType): LongInt;
var
  d: Double;
begin
  d:=InvokeJSDoubleResult(aName,Args,Invoke);
  if (Frac(d)<>0) or (d<low(longint)) or (d>high(longint)) then
    InvokeJS_RaiseResultMismatchStr(aName,'longint','double')
  else
    Result:=Trunc(d);
end;

function TJSObject.ReadJSPropertyBoolean(const aName: string): boolean;
begin
  Result:=InvokeJSBooleanResult(aName,[],jigGetter);
end;

function TJSObject.ReadJSPropertyDouble(const aName: string): double;
begin
  Result:=InvokeJSDoubleResult(aName,[],jigGetter);
end;

function TJSObject.ReadJSPropertyUnicodeString(const aName: string
  ): UnicodeString;
begin
  Result:=InvokeJSUnicodeStringResult(aName,[],jigGetter);
end;

function TJSObject.ReadJSPropertyObject(const aName: string;
  aResultClass: TJSObjectClass): TJSObject;
begin
  Result:=InvokeJSObjectResult(aName,[],aResultClass,jigGetter);
end;

function TJSObject.ReadJSPropertyUtf8String(const aName: string): string;
begin
  Result:=InvokeJSUtf8StringResult(aName,[],jigGetter);
end;

function TJSObject.ReadJSPropertyLongInt(const aName: string): LongInt;
begin
  Result:=InvokeJSLongIntResult(aName,[],jigGetter);
end;

function TJSObject.ReadJSPropertyValue(const aName: string): TJOB_JSValue;
begin
  Result:=InvokeJSValueResult(aName,[],jigGetter);
end;

procedure TJSObject.WriteJSPropertyBoolean(const aName: string; Value: Boolean);
begin
  InvokeJSNoResult(aName,[Value],jisSetter);
end;

procedure TJSObject.WriteJSPropertyDouble(const aName: string; Value: Double);
begin
  InvokeJSNoResult(aName,[Value],jisSetter);
end;

procedure TJSObject.WriteJSPropertyUnicodeString(const aName: string;
  const Value: UnicodeString);
begin
  InvokeJSNoResult(aName,[Value],jisSetter);
end;

procedure TJSObject.WriteJSPropertyUtf8String(const aName: string;
  const Value: String);
begin
  InvokeJSNoResult(aName,[Value],jisSetter);
end;

procedure TJSObject.WriteJSPropertyObject(const aName: string; Value: TJSObject
  );
begin
  InvokeJSNoResult(aName,[Value],jisSetter);
end;

procedure TJSObject.WriteJSPropertyLongInt(const aName: string; Value: LongInt);
begin
  InvokeJSNoResult(aName,[Value],jisSetter);
end;

function TJSObject.NewJSObject(const Args: array of const;
  aResultClass: TJSObjectClass): TJSObject;
begin
  Result:=InvokeJSObjectResult('',Args,aResultClass,jigNew);
end;

initialization
  JSObject:=TJSObject.CreateFromID(JOBObjIdObject);

end.

