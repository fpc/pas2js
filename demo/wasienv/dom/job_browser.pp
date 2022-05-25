{
  JOB - JS Object Bridge for Webassembly

  Browser side.
}
unit JOB_Browser;

{$mode objfpc}

interface

uses sysutils, types, js, web, wasienv, JOB_Shared;

Type
  EJOBBridge = class(Exception);

  { TJOBBridge }

  TJOBBridge = class(TImportExtension)
  Private
    FGlobalObjects: TJSArray;
    FLocalObjects: TJSArray;
    FFreeLocalIds: TJSArray; // free positions in FLocalObjects
    FStringResult: string;
  Protected
    function FindObject(ObjId: TJOBObjectID): TJSObject; virtual;
    function Invoke_JSResult(ObjId: TJOBObjectID; NameP, NameLen, Invoke, ArgsP: NativeInt; out JSResult: JSValue): TJOBResult; virtual;
    function Invoke_NoResult(ObjId: TJOBObjectID; NameP, NameLen, Invoke, ArgsP: NativeInt): TJOBResult; virtual;
    function Invoke_BooleanResult(ObjId: TJOBObjectID; NameP, NameLen, Invoke, ArgsP, ResultP: NativeInt): TJOBResult; virtual;
    function Invoke_DoubleResult(ObjId: TJOBObjectID; NameP, NameLen, Invoke, ArgsP, ResultP: NativeInt): TJOBResult; virtual;
    function Invoke_StringResult(ObjId: TJOBObjectID; NameP, NameLen, Invoke, ArgsP, ResultP: NativeInt): TJOBResult; virtual;
    function Invoke_ObjectResult(ObjId: TJOBObjectID; NameP, NameLen, Invoke, ArgsP, ResultP: NativeInt): TJOBResult; virtual;
    function ReleaseObject(ObjId: TJOBObjectID): TJOBResult; virtual;
    function GetStringResult(ResultP: NativeInt): TJOBResult; virtual;
    function ReleaseStringResult: TJOBResult; virtual;
    function GetInvokeArguments(View: TJSDataView; ArgsP: NativeInt): TJSValueDynArray; virtual;
    function GetJOBResult(v: jsvalue): TJOBResult;
  Public
    Constructor Create(aEnv: TPas2JSWASIEnvironment); override;
    Procedure FillImportObject(aObject: TJSObject); override;
    Function ImportName: String; override;
    Function RegisterGlobalObject(Obj: TJSObject): TJOBObjectID; virtual;
  end;

Implementation

function TypedArrayToString(const a: TJSTypedArray): string; assembler;
asm
  return String.fromCharCode.apply(null,a);
end;

constructor TJOBBridge.Create(aEnv: TPas2JSWASIEnvironment);
begin
  Inherited Create(aEnv);
  FGlobalObjects:=TJSArray.new;
  FGlobalObjects[-JOBObjIdDocument]:=document;
  FGlobalObjects[-JOBObjIdWindow]:=window;
  FGlobalObjects[-JOBObjIdConsole]:=console;
  FGlobalObjects[-JOBObjIdCaches]:=caches;
  FLocalObjects:=TJSArray.new;
  FFreeLocalIds:=TJSArray.new;
end;

function TJOBBridge.ImportName: String;
begin
  Result:=JOBExportName;
end;

function TJOBBridge.RegisterGlobalObject(Obj: TJSObject): TJOBObjectID;
begin
  Result:=-(FGlobalObjects.push(Obj)-1);
end;

procedure TJOBBridge.FillImportObject(aObject: TJSObject);
begin
  aObject[JOBFn_InvokeNoResult]:=@Invoke_NoResult;
  aObject[JOBFn_InvokeBooleanResult]:=@Invoke_BooleanResult;
  aObject[JOBFn_InvokeDoubleResult]:=@Invoke_DoubleResult;
  aObject[JOBFn_InvokeStringResult]:=@Invoke_StringResult;
  aObject[JOBFn_GetStringResult]:=@GetStringResult;
  aObject[JOBFn_ReleaseStringResult]:=@ReleaseStringResult;
  aObject[JOBFn_InvokeObjectResult]:=@Invoke_ObjectResult;
  aObject[JOBFn_ReleaseObject]:=@ReleaseObject;
end;

function TJOBBridge.FindObject(ObjId: TJOBObjectID): TJSObject;
begin
  if ObjId<0 then
    Result:=TJSObject(FGlobalObjects[-ObjId])
  else
    Result:=TJSObject(FLocalObjects[ObjId]);
  if isUndefined(Result) then
    Result:=nil;
end;

function TJOBBridge.Invoke_JSResult(ObjId: TJOBObjectID; NameP, NameLen,
  Invoke, ArgsP: NativeInt; out JSResult: JSValue): TJOBResult;
var
  View: TJSDataView;
  aBytes: TJSUint8Array;
  PropName: String;
  Args: TJSValueDynArray;
  Obj: TJSObject;
  fn: JSValue;
begin
  writeln('TJOBBridge.Invoke_JSResult ObjId=',ObjId,' FuncNameP=',NameP,' FuncNameLen=',NameLen,' ArgsP=',ArgsP,' Invoke=',Invoke);

  Obj:=FindObject(ObjId);
  if Obj=nil then
    exit(JOBResult_UnknownObjId);

  View:=getModuleMemoryDataView();
  aBytes:=TJSUint8Array.New(View.buffer, NameP, NameLen);
  //writeln('TJOBBridge.Invoke_JSResult aBytes=',aBytes);
  PropName:=TypedArrayToString(aBytes);
  writeln('TJOBBridge.Invoke_JSResult PropName="',PropName,'"');

  case Invoke of
  JOBInvokeCall:
    begin
      fn:=Obj[PropName];
      if jstypeof(fn)<>'function' then
        exit(JOBResult_NotAFunction);

      if ArgsP=0 then
        JSResult:=TJSFunction(fn).call(Obj)
      else begin
        Args:=GetInvokeArguments(View,ArgsP);
        JSResult:=TJSFunction(fn).apply(Obj,Args);
      end;
    end;
  JOBInvokeGetter:
    begin
      if ArgsP>0 then
        exit(JOBResult_WrongArgs);
      JSResult:=Obj[PropName];
    end;
  JOBInvokeSetter:
    begin
      JSResult:=Undefined;
      if ArgsP=0 then
        exit(JOBResult_WrongArgs);
      Args:=GetInvokeArguments(View,ArgsP);
      if length(Args)<>1 then
        exit(JOBResult_WrongArgs);
      Obj[PropName]:=Args[0];
    end
  else
    exit(JOBResult_NotAFunction);
  end;

  Result:=JOBResult_Success;
end;

function TJOBBridge.Invoke_NoResult(ObjId: TJOBObjectID; NameP, NameLen,
  Invoke, ArgsP: NativeInt): TJOBResult;
var
  JSResult: JSValue;
begin
  // invoke
  Result:=Invoke_JSResult(ObjId,NameP,NameLen,Invoke,ArgsP,JSResult);
end;

function TJOBBridge.Invoke_BooleanResult(ObjId: TJOBObjectID; NameP, NameLen,
  Invoke, ArgsP, ResultP: NativeInt): TJOBResult;
var
  JSResult: JSValue;
  b: byte;
begin
  // invoke
  Result:=Invoke_JSResult(ObjId,NameP,NameLen,Invoke,ArgsP,JSResult);
  if Result<>JOBResult_Success then
    exit;
  // check result type
  if jstypeof(JSResult)<>'boolean' then
    exit(GetJOBResult(JSResult));
  if JSResult then
    b:=1
  else
    b:=0;
  // set result
  getModuleMemoryDataView().setUint8(ResultP, b);
  Result:=JOBResult_Boolean;
end;

function TJOBBridge.Invoke_DoubleResult(ObjId: TJOBObjectID; NameP, NameLen,
  Invoke, ArgsP, ResultP: NativeInt): TJOBResult;
var
  JSResult: JSValue;
begin
  // invoke
  Result:=Invoke_JSResult(ObjId,NameP,NameLen,Invoke,ArgsP,JSResult);
  if Result<>JOBResult_Success then
    exit;
  // check result type
  if jstypeof(JSResult)<>'number' then
    exit(GetJOBResult(JSResult));
  // set result
  getModuleMemoryDataView().setFloat64(ResultP, double(JSResult), env.IsLittleEndian);
  Result:=JOBResult_Double;
end;

function TJOBBridge.Invoke_StringResult(ObjId: TJOBObjectID; NameP, NameLen,
  Invoke, ArgsP, ResultP: NativeInt): TJOBResult;
var
  JSResult: JSValue;
begin
  // invoke
  Result:=Invoke_JSResult(ObjId,NameP,NameLen,Invoke,ArgsP,JSResult);
  if Result<>JOBResult_Success then
    exit;
  // check result type
  if jstypeof(JSResult)<>'string' then
    exit(GetJOBResult(JSResult));
  Result:=JOBResult_String;
  FStringResult:=String(JSResult);

  // set result length
  getModuleMemoryDataView().setInt32(ResultP, length(FStringResult), env.IsLittleEndian);
end;

function TJOBBridge.Invoke_ObjectResult(ObjId: TJOBObjectID; NameP, NameLen,
  Invoke, ArgsP, ResultP: NativeInt): TJOBResult;
var
  t: String;
  JSResult, NewId: JSValue;
begin
  // invoke
  Result:=Invoke_JSResult(ObjId,NameP,NameLen,Invoke,ArgsP,JSResult);
  writeln('BBB1 TJOBBridge.Invoke_ObjectResult ',Result,' JSResult=',JSResult);
  if Result<>JOBResult_Success then
    exit;
  // check result type
  t:=jstypeof(JSResult);
  if (t<>'object') and (t<>'function') then
    exit(GetJOBResult(JSResult));
  if JSResult=nil then
    exit(JOBResult_Null);

  // create Id
  NewId:=FFreeLocalIds.pop;
  if isUndefined(NewId) then
    NewId:=FLocalObjects.push(JSResult)-1
  else
    FLocalObjects[longword(NewId)]:=JSResult;

  // set result
  getModuleMemoryDataView().setUint32(ResultP, longword(NewId), env.IsLittleEndian);
  Result:=JOBResult_Object;
end;

function TJOBBridge.ReleaseObject(ObjId: TJOBObjectID): TJOBResult;
begin
  //writeln('TJOBBridge.ReleaseObject ',ObjId);
  if ObjId<0 then
    raise EJOBBridge.Create('cannot release a global object');
  if ObjId>=FLocalObjects.Length then
    raise EJOBBridge.Create('cannot release unknown object');
  if FLocalObjects[ObjId]=nil then
    raise EJOBBridge.Create('object already released');
  FLocalObjects[ObjId]:=nil;
  FFreeLocalIds.push(ObjId);
  Result:=JOBResult_Success;
end;

function TJOBBridge.GetStringResult(ResultP: NativeInt): TJOBResult;
var
  View: TJSDataView;
  l, i: SizeInt;
begin
  Result:=JOBResult_Success;
  l:=length(FStringResult);
  if l=0 then exit;
  View:=getModuleMemoryDataView();
  for i:=0 to l-1 do
    View.setUint16(ResultP+2*i,ord(FStringResult[i]),env.IsLittleEndian);
  FStringResult:='';
end;

function TJOBBridge.ReleaseStringResult: TJOBResult;
begin
  Result:=JOBResult_Success;
  FStringResult:='';
end;

function TJOBBridge.GetInvokeArguments(View: TJSDataView; ArgsP: NativeInt
  ): TJSValueDynArray;
var
  Cnt, aType: Byte;
  i: Integer;
  p: NativeInt;
  Len, Ptr: LongWord;
  aBytes: TJSUint8Array;
  aWords: TJSUint16Array;
begin
  p:=ArgsP;
  Cnt:=View.getUInt8(p);
  inc(p);
  for i:=0 to Cnt-1 do
  begin
    aType:=View.getUInt8(p);
    inc(p);
    case aType of
    JOBArgLongint:
      begin
        Result[i]:=View.getInt32(p,env.IsLittleEndian);
        inc(p,4);
      end;
    JOBArgDouble:
      begin
        Result[i]:=View.getFloat64(p,env.IsLittleEndian);
        inc(p,8);
      end;
    JOBArgTrue:
      Result[i]:=true;
    JOBArgFalse:
      Result[i]:=false;
    JOBArgChar:
      begin
        Result[i]:=chr(View.getUint16(p,env.IsLittleEndian));
        inc(p,2);
      end;
    JOBArgUTF8String:
      begin
        Len:=View.getUint32(p,env.IsLittleEndian);
        inc(p,4);
        Ptr:=View.getUint32(p,env.IsLittleEndian);
        inc(p,4);
        aBytes:=TJSUint8Array.New(View.buffer, Ptr,Len);
        Result[i]:=TypedArrayToString(aBytes);
      end;
    JOBArgUnicodeString:
      begin
        Len:=View.getUint32(p,env.IsLittleEndian);
        inc(p,4);
        Ptr:=View.getUint32(p,env.IsLittleEndian);
        inc(p,4);
        aWords:=TJSUint16Array.New(View.buffer, Ptr,Len);
        Result[i]:=TypedArrayToString(aWords);
      end;
    JOBArgPointer:
      begin
        Result[i]:=View.getUint32(p,env.IsLittleEndian);
        inc(p,4);
      end
    else
      raise Exception.Create('unknown arg type '+IntToStr(aType));
    end;
    //writeln('TJOBBridge.GetInvokeArguments ',i,'/',Cnt,' = ',Result[i]);
  end;
end;

function TJOBBridge.GetJOBResult(v: jsvalue): TJOBResult;
begin
  case jstypeof(v) of
  'undefined': Result:=JOBResult_Undefined;
  'boolean': Result:=JOBResult_Boolean;
  'number': Result:=JOBResult_Number;
  'string': Result:=JOBResult_String;
  'symbol': Result:=JOBResult_Symbol;
  'bigint': Result:=JOBResult_BigInt;
  'function': Result:=JOBResult_Function;
  'object': if v=nil then Result:=JOBResult_Null else Result:=JOBResult_Object;
  else Result:=JOBResult_None;
  end;
end;

end.  
