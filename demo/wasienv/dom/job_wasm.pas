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

  PJOBObjectID = ^TJOBObjectID;
  TJOBInvokeOneResultFunc = function(
      ObjID: TJOBObjectID;
      FuncNameP: PChar;
      FuncNameLen: longint;
      ArgP: PByte;
      ResultP: PByte
    ): TJOBResult;

  TJSObject = class;
  TJSObjectClass = class of TJSObject;

  { TJSObject }

  TJSObject = class(TInterfacedObject)
  private
    FObjectID: TJOBObjectID;
  protected
    function InvokeJSOneResult(const aName: string; Const Args: Array of const;
      const InvokeFunc: TJOBInvokeOneResultFunc; ResultP: PByte): TJOBResult;
    procedure InvokeRaise(const aName, Msg: string); virtual;
    procedure InvokeRaiseResultMismatch(const aName: string; Expected, Actual: TJOBResult); virtual;
    procedure InvokeRaiseResultMismatchStr(const aName: string; const Expected, Actual: string); virtual;
    function CreateInvokeJSArgs(const Args: array of const): PByte; virtual;
  public
    constructor CreateFromID(aID: TJOBObjectID); virtual;
    destructor Destroy; override;
    property ObjectID: TJOBObjectID read FObjectID;
    procedure InvokeJSNoResult(const aName: string; Const Args: Array of const); virtual;
    function InvokeJSBooleanResult(const aName: string; Const Args: Array of const): Boolean; virtual;
    function InvokeJSDoubleResult(const aName: string; Const Args: Array of const): Double; virtual;
    function InvokeJSUnicodeStringResult(const aName: string; Const Args: Array of const): UnicodeString; virtual;
    function InvokeJSObjResult(const aName: string; aResultClass: TJSObjectClass; Const Args: Array of const): TJSObject; virtual;
    // ToDo: InvokeJSVarRecResult
    function InvokeJSUtf8StringResult(const aName: string; Const args: Array of const): String; virtual;
    function InvokeJSLongIntResult(const aName: string; Const args: Array of const): LongInt; virtual;
  end;

var
  JSDocument: TJSObject; // ToDo

function __job_invoke_noresult(
  ObjID: TJOBObjectID;
  FuncNameP: PChar;
  FuncNameLen: longint;
  ArgP: PByte;
  Dummy: PByte
): TJOBResult; external JOBExportName name JOBFn_InvokeNoResult;

function __job_invoke_boolresult(
  ObjID: TJOBObjectID;
  FuncNameP: PChar;
  FuncNameLen: longint;
  ArgP: PByte;
  ResultP: PByte // bytebool
): TJOBResult; external JOBExportName name JOBFn_InvokeBooleanResult;

function __job_invoke_doubleresult(
  ObjID: TJOBObjectID;
  FuncNameP: PChar;
  FuncNameLen: longint;
  ArgP: PByte;
  ResultP: PByte // double
): TJOBResult; external JOBExportName name JOBFn_InvokeDoubleResult;

function __job_invoke_stringresult(
  ObjID: TJOBObjectID;
  FuncNameP: PChar;
  FuncNameLen: longint;
  ArgP: PByte;
  ResultLenP: PByte // length
): TJOBResult; external JOBExportName name JOBFn_InvokeStringResult;

function __job_getstringresult(
  ResultP: PByte
): TJOBResult; external JOBExportName name JOBFn_GetStringResult;

function __job_releasestringresult(
): TJOBResult; external JOBExportName name JOBFn_ReleaseStringResult;

function __job_invoke_objectresult(
  ObjID: TJOBObjectID;
  FuncNameP: PChar;
  FuncNameLen: longint;
  ArgP: PByte;
  ResultP: PByte // nativeint
): TJOBResult; external JOBExportName name JOBFn_InvokeObjectResult;

function __job_release_object(
  ObjID: TJOBObjectID
): TJOBResult; external JOBExportName name JOBFn_ReleaseObject;

implementation

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

{ TJSObject }

function TJSObject.InvokeJSOneResult(const aName: string; const Args: array of const;
  const InvokeFunc: TJOBInvokeOneResultFunc; ResultP: PByte): TJOBResult;
var
  InvokeArgs: PByte;
begin
  if length(Args)=0 then
    Result:=InvokeFunc(ObjectID,PChar(aName),length(aName),nil,ResultP)
  else begin
    InvokeArgs:=CreateInvokeJSArgs(Args);
    try
      Result:=InvokeFunc(ObjectID,PChar(aName),length(aName),InvokeArgs,ResultP);
    finally
      if InvokeArgs<>nil then
        FreeMem(InvokeArgs);
    end;
  end;
end;

procedure TJSObject.InvokeRaise(const aName, Msg: string);
var
  E: EJSInvoke;
begin
  E:=EJSInvoke.Create(Msg);
  E.ObjectID:=ObjectID;
  E.FuncName:=aName;
  raise E;
end;

procedure TJSObject.InvokeRaiseResultMismatch(const aName: string;
  Expected, Actual: TJOBResult);
begin
  case Actual of
  JOBResult_UnknownObjId: InvokeRaise(aName,'unknown object id '+IntToStr(ObjectID));
  JOBResult_NotAFunction: InvokeRaise(aName,'object '+IntToStr(ObjectID)+' does not have a function "'+aName+'"');
  else
    InvokeRaiseResultMismatchStr(aName,JOBResult_Names[Expected],JOBResult_Names[Actual]);
  end;
end;

procedure TJSObject.InvokeRaiseResultMismatchStr(const aName: string;
  const Expected, Actual: string);
begin
  InvokeRaise(aName,'expected '+Expected+', but got '+Actual+' from object '+IntToStr(ObjectID)+' function "'+aName+'"');
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
    vtPointer,
    vtPChar         :
      begin
        strlen(Args[i].VPChar);
        inc(Len,1+SizeOf(NativeInt)+SizeOf(PByte));
      end;
    vtObject        : RaiseNotSupported('object');
    vtClass         : RaiseNotSupported('class');
    vtPWideChar     : RaiseNotSupported('pwidechar');
    vtAnsiString    : inc(Len,1+SizeOf(NativeInt)+SizeOf(PByte));
    vtCurrency      : RaiseNotSupported('currency');
    {$ifdef FPC_HAS_FEATURE_VARIANTS}
    vtVariant       : RaiseNotSupported('variant');
    {$endif FPC_HAS_FEATURE_VARIANTS}
    vtInterface     : RaiseNotSupported('interface');
    vtWideString    : inc(Len,1+SizeOf(NativeInt)+SizeOf(PByte));
    vtInt64         :
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
    vtQWord         :
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
    vtInteger       :
      begin
        p^:=JOBArgLongint;
        inc(p);
        PLongint(p)^:=Args[i].VInteger;
        inc(p,4);
      end;
    vtBoolean       :
      begin
        if Args[i].VBoolean then
          p^:=JOBArgTrue
        else
          p^:=JOBArgFalse;
        inc(p);
      end;
    {$ifndef FPUNONE}
    vtExtended      :
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
    vtWideChar      :
      begin
        p^:=JOBArgChar;
        inc(p);
        PWord(p)^:=ord(Args[i].VWideChar);
        inc(p,2);
      end;
    vtString        :
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
        p^:=JOBArgPointer;
        inc(p);
        PPointer(p)^:=Args[i].VPointer;
        inc(p,sizeof(Pointer));
      end;
    vtPChar         :
      begin
        p^:=JOBArgUTF8String;
        inc(p);
        h:=PByte(Args[i].VPChar);
        PNativeInt(p)^:=strlen(PChar(h));
        inc(p,sizeof(NativeInt));
        PPointer(p)^:=h;
        inc(p,sizeof(Pointer));
      end;
    vtObject        : ;
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
    vtInterface     : ;
    vtWideString    :
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
    vtInt64         :
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
    vtUnicodeString :
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
    vtQWord         :
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
  const Args: array of const);
var
  aError: TJOBResult;
  InvokeArgs: PByte;
begin
  if length(Args)=0 then
    aError:=__job_invoke_noresult(ObjectID,PChar(aName),length(aName),nil,nil)
  else begin
    InvokeArgs:=CreateInvokeJSArgs(Args);
    try
      aError:=__job_invoke_noresult(ObjectID,PChar(aName),length(aName),InvokeArgs,nil);
    finally
      if InvokeArgs<>nil then
        FreeMem(InvokeArgs);
    end;
  end;
  if aError<>JOBResult_Success then
    InvokeRaiseResultMismatch(aName,JOBResult_Success,aError);
end;

function TJSObject.InvokeJSBooleanResult(const aName: string;
  const Args: array of const): Boolean;
var
  aError: TJOBResult;
  b: bytebool;
begin
  b:=false;
  aError:=InvokeJSOneResult(aName,Args,@__job_invoke_boolresult,@b);
  if aError<>JOBResult_Boolean then
    InvokeRaiseResultMismatch(aName,JOBResult_Boolean,aError);
  Result:=b;
end;

function TJSObject.InvokeJSDoubleResult(const aName: string;
  const Args: array of const): Double;
var
  aError: TJOBResult;
begin
  Result:=NaN;
  aError:=InvokeJSOneResult(aName,Args,@__job_invoke_doubleresult,@Result);
  if aError<>JOBResult_Double then
    InvokeRaiseResultMismatch(aName,JOBResult_Double,aError);
end;

function TJSObject.InvokeJSUnicodeStringResult(const aName: string;
  const Args: array of const): UnicodeString;
var
  ResultLen: NativeInt;
  aError: TJOBResult;
begin
  ResultLen:=0;
  aError:=InvokeJSOneResult(aName,Args,@__job_invoke_stringresult,@ResultLen);
  if aError<>JOBResult_String then
    InvokeRaiseResultMismatch(aName,JOBResult_String,aError);
  if ResultLen=0 then
    exit('');
  try
    // try to allocate the memory
    SetLength(Result,ResultLen);
    aError:=JOBResult_Success;
  finally
    if aError<>JOBResult_Success then
      __job_releasestringresult();
  end;
  __job_getstringresult(PByte(Result));
end;

function TJSObject.InvokeJSObjResult(const aName: string;
  aResultClass: TJSObjectClass; const Args: array of const): TJSObject;
var
  aError: TJOBResult;
  NewObjId: TJOBObjectID;
begin
  Result:=nil;
  NewObjId:=-1;
  aError:=InvokeJSOneResult(aName,Args,@__job_invoke_objectresult,@NewObjId);
  if aError=JOBResult_Null then
    exit;
  if aError<>JOBResult_Object then
    InvokeRaiseResultMismatch(aName,JOBResult_Object,aError);

  Result:=aResultClass.CreateFromID(NewObjId);
end;

function TJSObject.InvokeJSUtf8StringResult(const aName: string;
  const args: array of const): String;
begin
  Result:=String(InvokeJSUnicodeStringResult(aName,Args));
end;

function TJSObject.InvokeJSLongIntResult(const aName: string;
  const args: array of const): LongInt;
var
  d: Double;
begin
  d:=InvokeJSDoubleResult(aName,Args);
  if Frac(d)<>0 then
    InvokeRaiseResultMismatchStr(aName,'longint','double')
  else if (d<low(longint)) or (d>high(longint)) then
    InvokeRaiseResultMismatchStr(aName,'longint','double')
  else
    Result:=Trunc(d);
end;

initialization
  JSDocument:=TJSObject.CreateFromID(JOBObjIdDocument);

end.

