unit wadom_browser;

{$mode objfpc}
{$h+}

interface

uses sysutils, types, js, web, wasienv, wadom_shared;

Type

  { TWADomBridge }

  TWADomBridge = class(TImportExtension)
  Private
    FGlobalObjects: TJSArray;
    FLocalObjects: TJSArray;
  Protected
    function FindObject(ObjId: TWasiDomObjectID): TJSObject; virtual;
    function Invoke_JSResult(ObjId: TWasiDomObjectID; FuncNameP, FuncNameLen, ArgsP: NativeInt; out JSResult: JSValue): TWasiDomResult; virtual;
    function Invoke_BooleanResult(ObjId: TWasiDomObjectID; FuncNameP, FuncNameLen, ArgsP, ResultP: NativeInt): TWasiDomResult; virtual;
    function Invoke_DoubleResult(ObjId: TWasiDomObjectID; FuncNameP, FuncNameLen, ArgsP, ResultP: NativeInt): TWasiDomResult; virtual;
    function GetInvokeArguments(View: TJSDataView; ArgsP: NativeInt): TJSValueDynArray; virtual;
    function GetWasiDomResult(const v: jsvalue): TWasiDomResult;
  Public
    Constructor Create(aEnv: TPas2JSWASIEnvironment); override;
    Procedure FillImportObject(aObject: TJSObject); override;
    Function ImportName: String; override;
  end;

Implementation

function TypedArrayToString(const a: TJSTypedArray): string; assembler;
asm
  return String.fromCharCode.apply(null,a);
end;

constructor TWADomBridge.Create(aEnv: TPas2JSWASIEnvironment);
begin
  Inherited Create(aEnv);
  FGlobalObjects:=TJSArray.new;
  FGlobalObjects[-WasiObjIdDocument]:=document;
  FGlobalObjects[-WasiObjIdWindow]:=window;
  FGlobalObjects[-WasiObjIdConsole]:=console;
  FGlobalObjects[-WasiObjIdCaches]:=caches;
  FLocalObjects:=TJSArray.new;
end;

function TWADomBridge.ImportName: String;
begin
  Result:=WasiDomExtName;
end;

procedure TWADomBridge.FillImportObject(aObject: TJSObject);
begin
  aObject[WasiDomInvokeBooleanResult]:=@invoke_booleanresult;
  aObject[WasiDomInvokeDoubleResult]:=@invoke_doubleresult;
end;

function TWADomBridge.FindObject(ObjId: TWasiDomObjectID): TJSObject;
begin
  if ObjId<0 then
    Result:=TJSObject(FGlobalObjects[-ObjId])
  else
    Result:=TJSObject(FLocalObjects[ObjId]);
  if isUndefined(Result) then
    Result:=nil;
end;

function TWADomBridge.Invoke_JSResult(ObjId: TWasiDomObjectID; FuncNameP,
  FuncNameLen, ArgsP: NativeInt; out JSResult: JSValue): TWasiDomResult;
var
  View: TJSDataView;
  aBytes: TJSUint8Array;
  FuncName: String;
  Args: TJSValueDynArray;
  Obj: TJSObject;
  fn: JSValue;
begin
  writeln('TWADomBridge.Invoke_JSResult ObjId=',ObjId,' FuncNameP=',FuncNameP,' FuncNameLen=',FuncNameLen,' ArgsP=',ArgsP);

  Obj:=FindObject(ObjId);
  if Obj=nil then
    exit(WasiDomResult_UnknownObjId);

  View:=getModuleMemoryDataView();
  aBytes:=TJSUint8Array.New(View.buffer, FuncNameP, FuncNameLen);
  writeln('TWADomBridge.Invoke_JSResult aBytes=',aBytes);
  FuncName:=TypedArrayToString(aBytes);
  writeln('TWADomBridge.Invoke_JSResult FuncName="',FuncName,'"');

  fn:=Obj[FuncName];
  if jstypeof(fn)<>'function' then
    exit(WasiDomResult_NotAFunction);

  if ArgsP=0 then
    JSResult:=TJSFunction(fn).call(Obj)
  else begin
    Args:=GetInvokeArguments(View,ArgsP);
    JSResult:=TJSFunction(fn).apply(Obj,Args);
  end;

  exit(WasiDomResult_Success);
end;

function TWADomBridge.Invoke_BooleanResult(ObjId: TWasiDomObjectID; FuncNameP,
  FuncNameLen, ArgsP, ResultP: NativeInt): TWasiDomResult;
var
  JSResult: JSValue;
  b: byte;
begin
  // invoke
  Result:=Invoke_JSResult(ObjId,FuncNameP,FuncNameLen,ArgsP,JSResult);
  if Result<>WasiDomResult_Success then
    exit;
  // check result type
  if jstypeof(JSResult)<>'boolean' then
    exit(GetWasiDomResult(JSResult));
  if JSResult then
    b:=1
  else
    b:=0;
  // set result
  getModuleMemoryDataView().setUint8(ResultP, b);
  Result:=WasiDomResult_Success;
end;

function TWADomBridge.Invoke_DoubleResult(ObjId: TWasiDomObjectID; FuncNameP,
  FuncNameLen, ArgsP, ResultP: NativeInt): TWasiDomResult;
var
  JSResult: JSValue;
begin
  // invoke
  Result:=Invoke_JSResult(ObjId,FuncNameP,FuncNameLen,ArgsP,JSResult);
  if Result<>WasiDomResult_Success then
    exit;
  // check result type
  if jstypeof(JSResult)<>'number' then
    exit(GetWasiDomResult(JSResult));
  // set result
  getModuleMemoryDataView().setFloat64(ResultP, double(JSResult), env.IsLittleEndian);
  Result:=WasiDomResult_Success;
end;

function TWADomBridge.GetInvokeArguments(View: TJSDataView; ArgsP: NativeInt
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
    WasiArgLongint:
      begin
        Result[i]:=View.getInt32(p,env.IsLittleEndian);
        inc(p,4);
      end;
    WasiArgDouble:
      begin
        Result[i]:=View.getFloat64(p,env.IsLittleEndian);
        inc(p,8);
      end;
    WasiArgTrue:
      Result[i]:=true;
    WasiArgFalse:
      Result[i]:=false;
    WasiArgChar:
      begin
        Result[i]:=chr(View.getUint16(p,env.IsLittleEndian));
        inc(p,2);
      end;
    WasiArgUTF8String:
      begin
        Len:=View.getUint32(p,env.IsLittleEndian);
        inc(p,4);
        Ptr:=View.getUint32(p,env.IsLittleEndian);
        inc(p,4);
        aBytes:=TJSUint8Array.New(View.buffer, Ptr,Len);
        Result[i]:=TypedArrayToString(aBytes);
      end;
    WasiArgUnicodeString:
      begin
        Len:=View.getUint32(p,env.IsLittleEndian);
        inc(p,4);
        Ptr:=View.getUint32(p,env.IsLittleEndian);
        inc(p,4);
        aWords:=TJSUint16Array.New(View.buffer, Ptr,Len);
        Result[i]:=TypedArrayToString(aWords);
      end;
    WasiArgPointer:
      begin
        Result[i]:=View.getUint32(p,env.IsLittleEndian);
        inc(p,4);
      end
    else
      raise Exception.Create('unknown arg type '+IntToStr(aType));
    end;
    writeln('TWADomBridge.GetInvokeArguments ',i,'/',Cnt,' = ',Result[i]);
  end;
end;

function TWADomBridge.GetWasiDomResult(const v: jsvalue): TWasiDomResult;
begin
  case jstypeof(v) of
  'undefined': Result:=WasiDomResult_Undefined;
  'boolean': Result:=WasiDomResult_Boolean;
  'number': Result:=WasiDomResult_Number;
  'string': Result:=WasiDomResult_String;
  'symbol': Result:=WasiDomResult_Symbol;
  'bigint': Result:=WasiDomResult_BigInt;
  'function': Result:=WasiDomResult_Function;
  'object': Result:=WasiDomResult_Object;
  else Result:=WasiDomResult_None;
  end;
end;

end.  
