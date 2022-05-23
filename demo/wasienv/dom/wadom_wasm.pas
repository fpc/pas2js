{
  Webassembly unit giving access to the browser DOM.

  see https://wiki.freepascal.org/WebAssembly/DOM
}
unit wadom_wasm;

{$mode ObjFPC}{$H+}

{$define VerboseWasiDom}

interface

uses
  SysUtils, Types, Math, Classes, wadom_shared;

const
  MinSafeIntDouble = -$1fffffffffffff; // -9007199254740991 54 bits (52 plus signed bit plus implicit highest bit)
  MaxSafeIntDouble =  $1fffffffffffff; //  9007199254740991

Type
  EJSObject = class(Exception);
  EJSInvoke = class(EJSObject)
  public
    ObjectID: TWasiDomObjectID;
    FuncName: string;
  end;

  PWasiDomObjectID = ^TWasiDomObjectID;

  TJSObject = class;
  TJSObjectClass = class of TJSObject;

  { TJSObject }

  TJSObject = class(TInterfacedObject)
  private
    FObjectID: TWasiDomObjectID;
  protected
    procedure WasiInvokeRaise(const aName, Msg: string); virtual;
    procedure WasiInvokeRaiseResultMismatch(const aName: string; Expected, Actual: TWasiDomResult); virtual;
    function CreateInvokeJSArgs(const Args: array of const): PByte; virtual;
  public
    constructor CreateFromID(aID: TWasiDomObjectID); virtual;
    destructor Destroy; override;
    property ObjectID: TWasiDomObjectID read FObjectID;
    procedure InvokeJSNoResult(const aName: string; Const args: Array of const);
    function InvokeJSBooleanResult(const aName: string; Const args: Array of const): Boolean;
    function InvokeJSDoubleResult(const aName: string; Const Args: Array of const): Double;
    //function InvokeJSUnicodeStringResult(const aName: string; Const args: Array of const): UnicodeString;
    function InvokeJSObjResult(const aName: string; aResultClass: TJSObjectClass; Const args: Array of const): TJSObject;
    // ToDo: InvokeJSVarRecResult
    //function InvokeJSUtf8StringResult(const aName: string; Const args: Array of const): String;
  end;

function __wasidom_invoke_noresult(
  ObjID: TWasiDomObjectID;
  FuncNameP: PChar;
  FuncNameLen: longint;
  ArgP: PByte
): TWasiDomResult; external WasiDomExportName name WasiDomInvokeBooleanResult;

function __wasidom_invoke_boolresult(
  ObjID: TWasiDomObjectID;
  FuncNameP: PChar;
  FuncNameLen: longint;
  ArgP: PByte;
  ResultP: PByteBool
): TWasiDomResult; external WasiDomExportName name WasiDomInvokeBooleanResult;

function __wasidom_invoke_doubleresult(
  ObjID: TWasiDomObjectID;
  FuncNameP: PChar;
  FuncNameLen: longint;
  ArgP: PByte;
  ResultP: PDouble
): TWasiDomResult; external WasiDomExportName name WasiDomInvokeDoubleResult;

function __wasidom_invoke_objectresult(
  ObjID: TWasiDomObjectID;
  FuncNameP: PChar;
  FuncNameLen: longint;
  ArgP: PByte;
  ResultP: PWasiDomObjectID
): TWasiDomResult; external WasiDomExportName name WasiDomInvokeDoubleResult;

implementation

{$IFDEF VerboseWasiDom}
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

procedure TJSObject.WasiInvokeRaise(const aName, Msg: string);
var
  E: EJSInvoke;
begin
  E:=EJSInvoke.Create(Msg);
  E.ObjectID:=ObjectID;
  E.FuncName:=aName;
  raise E;
end;

procedure TJSObject.WasiInvokeRaiseResultMismatch(const aName: string;
  Expected, Actual: TWasiDomResult);
begin
  case Actual of
  WasiDomResult_UnknownObjId: WasiInvokeRaise(aName,'unknown object id '+IntToStr(ObjectID));
  WasiDomResult_NotAFunction: WasiInvokeRaise(aName,'object '+IntToStr(ObjectID)+' does not have a function "'+aName+'"');
  else
    WasiInvokeRaise(aName,'expected '+WasiDomResult_Names[Expected]+', but got '+WasiDomResult_Names[Actual]+' from object '+IntToStr(ObjectID)+' function "'+aName+'"');
  end;
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
        p^:=WasiArgLongint;
        inc(p);
        PLongint(p)^:=Args[i].VInteger;
        inc(p,4);
      end;
    vtBoolean       :
      begin
        if Args[i].VBoolean then
          p^:=WasiArgTrue
        else
          p^:=WasiArgFalse;
        inc(p);
      end;
    {$ifndef FPUNONE}
    vtExtended      :
      begin
        p^:=WasiArgDouble;
        inc(p);
        PDouble(p)^:=double(Args[i].VExtended^);
        inc(p,8);
      end;
    {$endif}
    vtChar:
      begin
        p^:=WasiArgChar;
        inc(p);
        PWord(p)^:=ord(Args[i].VChar);
        inc(p,2);
      end;
    vtWideChar      :
      begin
        p^:=WasiArgChar;
        inc(p);
        PWord(p)^:=ord(Args[i].VWideChar);
        inc(p,2);
      end;
    vtString        :
      begin
        // shortstring
        p^:=WasiArgUTF8String;
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
        p^:=WasiArgPointer;
        inc(p);
        PPointer(p)^:=Args[i].VPointer;
        inc(p,sizeof(Pointer));
      end;
    vtPChar         :
      begin
        p^:=WasiArgUTF8String;
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
        p^:=WasiArgUTF8String;
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
        p^:=WasiArgUnicodeString;
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
          p^:=WasiArgLongint;
          inc(p);
          PLongint(p)^:=i64;
          inc(p,4);
        end else begin
          p^:=WasiArgDouble;
          inc(p);
          PDouble(p)^:=i64;
          inc(p,8);
        end;
      end;
    vtUnicodeString :
      begin
        p^:=WasiArgUnicodeString;
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
          p^:=WasiArgLongint;
          inc(p);
          PLongint(p)^:=qw;
          inc(p,4);
        end else begin
          p^:=WasiArgDouble;
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

constructor TJSObject.CreateFromID(aID: TWasiDomObjectID);
begin
  FObjectID:=aID;
end;

destructor TJSObject.Destroy;
begin
  inherited Destroy;
end;

procedure TJSObject.InvokeJSNoResult(const aName: string;
  const args: array of const);
var
  aError: TWasiDomResult;
  InvokeArgs: PByte;
begin
  if length(Args)=0 then
    aError:=__wasidom_invoke_noresult(ObjectID,PChar(aName),length(aName),nil)
  else begin
    InvokeArgs:=CreateInvokeJSArgs(Args);
    try
      aError:=__wasidom_invoke_noresult(ObjectID,PChar(aName),length(aName),InvokeArgs);
    finally
      if InvokeArgs<>nil then
        FreeMem(InvokeArgs);
    end;
  end;
  if aError<>WasiDomResult_Success then
    WasiInvokeRaiseResultMismatch(aName,WasiDomResult_Boolean,aError);
end;

function TJSObject.InvokeJSBooleanResult(const aName: string;
  const args: array of const): Boolean;
var
  aError: TWasiDomResult;
  InvokeArgs: PByte;
  b: bytebool;
begin
  b:=false;
  if length(Args)=0 then
    aError:=__wasidom_invoke_boolresult(ObjectID,PChar(aName),length(aName),nil,@b)
  else begin
    InvokeArgs:=CreateInvokeJSArgs(Args);
    try
      aError:=__wasidom_invoke_boolresult(ObjectID,PChar(aName),length(aName),
                                               InvokeArgs,@b);
    finally
      if InvokeArgs<>nil then
        FreeMem(InvokeArgs);
    end;
  end;
  if aError<>WasiDomResult_Boolean then
    WasiInvokeRaiseResultMismatch(aName,WasiDomResult_Boolean,aError);
  Result:=b;
end;

function TJSObject.InvokeJSDoubleResult(const aName: string;
  const Args: array of const): Double;
var
  aError: TWasiDomResult;
  InvokeArgs: PByte;
begin
  Result:=NaN;
  if length(Args)=0 then
    aError:=__wasidom_invoke_doubleresult(ObjectID,PChar(aName),length(aName),nil,@Result)
  else begin
    InvokeArgs:=CreateInvokeJSArgs(Args);
    try
      aError:=__wasidom_invoke_doubleresult(ObjectID,PChar(aName),length(aName),
                                               InvokeArgs,@Result);
    finally
      if InvokeArgs<>nil then
        FreeMem(InvokeArgs);
    end;
  end;
  if aError<>WasiDomResult_Double then
    WasiInvokeRaiseResultMismatch(aName,WasiDomResult_Double,aError);
end;

function TJSObject.InvokeJSObjResult(const aName: string;
  aResultClass: TJSObjectClass; const args: array of const): TJSObject;
var
  aError: TWasiDomResult;
  InvokeArgs: PByte;
  NewObjId: TWasiDomObjectID;
begin
  Result:=nil;
  NewObjId:=-1;
  if length(Args)=0 then
    aError:=__wasidom_invoke_objectresult(ObjectID,PChar(aName),length(aName),nil,@NewObjId)
  else begin
    InvokeArgs:=CreateInvokeJSArgs(Args);
    try
      aError:=__wasidom_invoke_objectresult(ObjectID,PChar(aName),length(aName),
                                               InvokeArgs,@NewObjId);
    finally
      if InvokeArgs<>nil then
        FreeMem(InvokeArgs);
    end;
  end;
  if aError=WasiDomResult_Null then
    exit;
  if aError<>WasiDomResult_Object then
    WasiInvokeRaiseResultMismatch(aName,WasiDomResult_Object,aError);

  Result:=aResultClass.CreateFromID(NewObjId);
end;

end.

