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
  TWasmNativeInt = Longword;
  TJOBCallback = function(aCall, aData, aCode, Args: TWasmNativeInt): TWasmNativeInt;

  { TJSObjectBridge }

  TJSObjectBridge = class(TImportExtension)
  Private
    FCallbackHandler: TJOBCallback;
    FGlobalObjects: TJSArray; // id to TJSObject
    FGlobalNames: TJSObject; // name to id
    FLocalObjects: TJSArray;
    FFreeLocalIds: TJSArray; // free positions in FLocalObjects
    FStringResult: string;
    FWasiExports: TWASIExports;
    procedure SetWasiExports(const AValue: TWASIExports);
  Protected
    function Invoke_JSResult(ObjId: TJOBObjectID; NameP, NameLen, Invoke, ArgsP: NativeInt; out JSResult: JSValue): TJOBResult; virtual;
    function GetInvokeArguments(View: TJSDataView; ArgsP: NativeInt): TJSValueDynArray; virtual;
    function CreateCallbackArgs(View: TJSDataView; const Args: TJSFunctionArguments; TempObjIds: TJOBObjectIDArray): TWasmNativeInt; virtual;
    function EatCallbackResult(View: TJSDataView; ResultP: TWasmNativeInt): jsvalue; virtual;
    // exports
    function Get_GlobalID(NameP, NameLen: NativeInt): TJOBObjectID; virtual;
    function Invoke_NoResult(ObjId: TJOBObjectID; NameP, NameLen, Invoke, ArgsP: NativeInt): TJOBResult; virtual;
    function Invoke_BooleanResult(ObjId: TJOBObjectID; NameP, NameLen, Invoke, ArgsP, ResultP: NativeInt): TJOBResult; virtual;
    function Invoke_DoubleResult(ObjId: TJOBObjectID; NameP, NameLen, Invoke, ArgsP, ResultP: NativeInt): TJOBResult; virtual;
    function Invoke_StringResult(ObjId: TJOBObjectID; NameP, NameLen, Invoke, ArgsP, ResultP: NativeInt): TJOBResult; virtual;
    function Invoke_ObjectResult(ObjId: TJOBObjectID; NameP, NameLen, Invoke, ArgsP, ResultP: NativeInt): TJOBResult; virtual;
    function Invoke_JSValueResult(ObjId: TJOBObjectID; NameP, NameLen, Invoke, ArgsP, ResultP: NativeInt): TJOBResult; virtual;
    function Invoke_ArrayStringResult(ObjId: TJOBObjectID; NameP, NameLen, Invoke, ArgsP, ResultP: NativeInt): TJOBResult; virtual;
    function ReleaseObject(ObjId: TJOBObjectID): TJOBResult; virtual;
    function GetStringResult(ResultP: NativeInt): TJOBResult; virtual;
    function ReleaseStringResult: TJOBResult; virtual;
  Public
    Constructor Create(aEnv: TPas2JSWASIEnvironment); override;
    Procedure FillImportObject(aObject: TJSObject); override;
    Function ImportName: String; override;
    function FindObject(ObjId: TJOBObjectID): TJSObject; virtual;
    function FindGlobalObject(const aName: string): TJOBObjectID; virtual; // 0=not found
    function RegisterLocalObject(Obj: TJSObject): TJOBObjectID; virtual;
    Function RegisterGlobalObject(Obj: JSValue; const aName: string): TJOBObjectID; virtual;
    Function GetJOBResult(v: jsvalue): TJOBResult;
    property CallbackHandler: TJOBCallback read FCallbackHandler write FCallbackHandler;
    property WasiExports: TWASIExports read FWasiExports write SetWasiExports;
  end;

Implementation

function TypedArrayToString(const a: TJSTypedArray): string; assembler;
asm
  return String.fromCharCode.apply(null,a);
end;

function NewObj(const fn: TJSFunction; const Args: TJSValueDynArray): TJSFunction; assembler;
asm
  if (Args == null){
    return new fn();
  }
  var l = Args.length;
  if (l==0){
    return new fn();
  } else if (l==1){
    return new fn(Args[0]);
  } else if (l==2){
    return new fn(Args[0],Args[1]);
  } else if (l==3){
    return new fn(Args[0],Args[1],Args[2]);
  } else if (l==4){
    return new fn(Args[0],Args[1],Args[2],Args[3]);
  } else if (l==5){
    return new fn(Args[0],Args[1],Args[2],Args[3],Args[4]);
  } else if (l==6){
    return new fn(Args[0],Args[1],Args[2],Args[3],Args[4],Args[5]);
  } else if (l==7){
    return new fn(Args[0],Args[1],Args[2],Args[3],Args[4],Args[5],Args[6]);
  } else if (l==8){
    return new fn(Args[0],Args[1],Args[2],Args[3],Args[4],Args[5],Args[6],Args[7]);
  } else if (l==9){
    return new fn(Args[0],Args[1],Args[2],Args[3],Args[4],Args[5],Args[6],Args[7],Args[8]);
  } else if (l==10){
    return new fn(Args[0],Args[1],Args[2],Args[3],Args[4],Args[5],Args[6],Args[7],Args[8],Args[9]);
  } else {
    return null;
  }
end;

constructor TJSObjectBridge.Create(aEnv: TPas2JSWASIEnvironment);
begin
  Inherited Create(aEnv);
  FGlobalObjects:=TJSArray.new;
  FGlobalObjects.push(nil); // allocate FGlobalObjects[0]
  FGlobalNames:=TJSObject.new;
  RegisterGlobalObject(document,'document');
  RegisterGlobalObject(window,'window');
  RegisterGlobalObject(console,'console');
  RegisterGlobalObject(caches,'caches');
  RegisterGlobalObject(TJSObject,'Object');
  RegisterGlobalObject(TJSFunction,'Function');
  RegisterGlobalObject(TJSDate,'Date');
  RegisterGlobalObject(TJSString,'String');
  RegisterGlobalObject(TJSArray,'Array');
  RegisterGlobalObject(TJSArrayBuffer,'ArrayBuffer');
  RegisterGlobalObject(TJSInt8Array,'Int8Array');
  RegisterGlobalObject(TJSUint8Array,'Uint8Array');
  RegisterGlobalObject(TJSUint8ClampedArray,'Uint8ClampedArray');
  RegisterGlobalObject(TJSInt16Array,'Int16Array');
  RegisterGlobalObject(TJSUint16Array,'Uint16Array');
  RegisterGlobalObject(TJSUint32Array,'Uint32Array');
  RegisterGlobalObject(TJSFloat32Array,'Float32Array');
  RegisterGlobalObject(TJSFloat64Array,'Float64Array');
  RegisterGlobalObject(TJSJSON,'JSON');
  RegisterGlobalObject(TJSPromise,'Promise');
  FLocalObjects:=TJSArray.new;
  FLocalObjects.push(nil); // allocate FLocalObjects[0]
  FFreeLocalIds:=TJSArray.new;
end;

function TJSObjectBridge.ImportName: String;
begin
  Result:=JOBExportName;
end;

function TJSObjectBridge.RegisterGlobalObject(Obj: JSValue; const aName: string
  ): TJOBObjectID;
begin
  if FGlobalNames.hasOwnProperty(aName) then
    raise EJOBBridge.Create('duplicate "'+aName+'"');
  Result:=-(FGlobalObjects.push(Obj)-1);
  FGlobalNames[aName]:=Result;
end;

procedure TJSObjectBridge.FillImportObject(aObject: TJSObject);
begin
  aObject[JOBFn_GetGlobal]:=@Get_GlobalID;
  aObject[JOBFn_InvokeNoResult]:=@Invoke_NoResult;
  aObject[JOBFn_InvokeBooleanResult]:=@Invoke_BooleanResult;
  aObject[JOBFn_InvokeDoubleResult]:=@Invoke_DoubleResult;
  aObject[JOBFn_InvokeStringResult]:=@Invoke_StringResult;
  aObject[JOBFn_GetStringResult]:=@GetStringResult;
  aObject[JOBFn_ReleaseStringResult]:=@ReleaseStringResult;
  aObject[JOBFn_InvokeObjectResult]:=@Invoke_ObjectResult;
  aObject[JOBFn_ReleaseObject]:=@ReleaseObject;
  aObject[JOBFn_InvokeJSValueResult]:=@Invoke_JSValueResult;
  aObject[JOBFn_InvokeArrayStringResult]:=@Invoke_ArrayStringResult;
end;

function TJSObjectBridge.FindObject(ObjId: TJOBObjectID): TJSObject;
begin
  if ObjId<0 then
    Result:=TJSObject(FGlobalObjects[-ObjId])
  else
    Result:=TJSObject(FLocalObjects[ObjId]);
  if isUndefined(Result) then
    Result:=nil;
end;

function TJSObjectBridge.FindGlobalObject(const aName: string): TJOBObjectID;
begin
  if not FGlobalNames.hasOwnProperty(aName) then
    exit(0);
  Result:=NativeInt(FGlobalNames[aName]);
end;

function TJSObjectBridge.RegisterLocalObject(Obj: TJSObject): TJOBObjectID;
var
  NewId: JSValue;
begin
  NewId:=FFreeLocalIds.pop;
  if isUndefined(NewId) then
  begin
    NewId:=FLocalObjects.push(Obj)-1;
    Result:=TJOBObjectID(NewId);
  end
  else begin
    Result:=TJOBObjectID(NewId);
    FLocalObjects[Result]:=Obj;
  end;
  {$IFDEF VerboseJOB}
  writeln('TJSObjectBridge.RegisterLocalObject ',Result);
  {$ENDIF}
end;

procedure TJSObjectBridge.SetWasiExports(const AValue: TWASIExports);
begin
  if FWasiExports=AValue then Exit;
  FWasiExports:=AValue;
  if FWasiExports<>nil then
    CallbackHandler:=TJOBCallback(FWasiExports.functions[JOBFn_CallbackHandler])
  else
    CallbackHandler:=nil;
end;

function TJSObjectBridge.Invoke_JSResult(ObjId: TJOBObjectID; NameP, NameLen,
  Invoke, ArgsP: NativeInt; out JSResult: JSValue): TJOBResult;
var
  View: TJSDataView;
  aBytes: TJSUint8Array;
  PropName: String;
  Args: TJSValueDynArray;
  Obj: TJSObject;
  fn: JSValue;
begin
  {$IFDEF VerboseJOB}
  writeln('TJSObjectBridge.Invoke_JSResult ObjId=',ObjId,' FuncNameP=',NameP,' FuncNameLen=',NameLen,' ArgsP=',ArgsP,' Invoke=',Invoke);
  {$ENDIF}

  Obj:=FindObject(ObjId);
  if Obj=nil then
    exit(JOBResult_UnknownObjId);

  View:=getModuleMemoryDataView();
  aBytes:=TJSUint8Array.New(View.buffer, NameP, NameLen);
  //writeln('TJSObjectBridge.Invoke_JSResult aBytes=',aBytes);
  PropName:=TypedArrayToString(aBytes);
  {$IFDEF VerboseJOB}
  writeln('TJSObjectBridge.Invoke_JSResult PropName="',PropName,'"');
  {$ENDIF}

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
  JOBInvokeNew:
    begin
      if PropName<>'' then
        fn:=Obj[PropName]
      else
        fn:=Obj;
      if jstypeof(fn)<>'function' then
        exit(JOBResult_NotAFunction);

      if ArgsP=0 then
        JSResult:=NewObj(TJSFunction(fn),nil)
      else begin
        Args:=GetInvokeArguments(View,ArgsP);
        JSResult:=NewObj(TJSFunction(fn),Args)
      end;
    end;
  JOBInvokeGet,JOBInvokeGetTypeOf:
    begin
      if ArgsP>0 then
        exit(JOBResult_WrongArgs);
      JSResult:=Obj[PropName];
      if Invoke=JOBInvokeGetTypeOf then
      begin
        Result:=GetJOBResult(jsTypeOf(JSResult));
        exit;
      end;
    end;
  JOBInvokeSet:
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

function TJSObjectBridge.Invoke_NoResult(ObjId: TJOBObjectID; NameP, NameLen,
  Invoke, ArgsP: NativeInt): TJOBResult;
var
  JSResult: JSValue;
begin
  // invoke
  Result:=Invoke_JSResult(ObjId,NameP,NameLen,Invoke,ArgsP,JSResult);
end;

function TJSObjectBridge.Invoke_BooleanResult(ObjId: TJOBObjectID; NameP, NameLen,
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

function TJSObjectBridge.Invoke_DoubleResult(ObjId: TJOBObjectID; NameP, NameLen,
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

function TJSObjectBridge.Invoke_StringResult(ObjId: TJOBObjectID; NameP, NameLen,
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
  //writeln('TJSObjectBridge.Invoke_StringResult FStringResult="',FStringResult,'"');

  // set result length
  getModuleMemoryDataView().setInt32(ResultP, length(FStringResult), env.IsLittleEndian);
end;

function TJSObjectBridge.Invoke_ObjectResult(ObjId: TJOBObjectID; NameP, NameLen,
  Invoke, ArgsP, ResultP: NativeInt): TJOBResult;
var
  t: String;
  JSResult: JSValue;
  NewId: TJOBObjectID;
begin
  // invoke
  Result:=Invoke_JSResult(ObjId,NameP,NameLen,Invoke,ArgsP,JSResult);
  if Result<>JOBResult_Success then
    exit;
  // check result type
  t:=jstypeof(JSResult);
  if (t<>'object') and (t<>'function') then
    exit(GetJOBResult(JSResult));
  if JSResult=nil then
    exit(JOBResult_Null);

  // set result
  NewId:=RegisterLocalObject(TJSObject(JSResult));
  getModuleMemoryDataView().setUint32(ResultP, longword(NewId), env.IsLittleEndian);
  Result:=JOBResult_Object;
end;

function TJSObjectBridge.Invoke_JSValueResult(ObjId: TJOBObjectID; NameP, NameLen,
  Invoke, ArgsP, ResultP: NativeInt): TJOBResult;
var
  JSResult: JSValue;
  b: byte;
  NewId: TJOBObjectID;
begin
  {$IFDEF VerboseJOB}
  writeln('TJSObjectBridge.Invoke_JSValueResult START');
  {$ENDIF}
  // invoke
  Result:=Invoke_JSResult(ObjId,NameP,NameLen,Invoke,ArgsP,JSResult);
  {$IFDEF VerboseJOB}
  writeln('TJSObjectBridge.Invoke_JSValueResult JSResult=',JSResult);
  {$ENDIF}
  if Result<>JOBResult_Success then
    exit;
  Result:=GetJOBResult(JSResult);
  {$IFDEF VerboseJOB}
  writeln('TJSObjectBridge.Invoke_JSValueResult Type=',Result);
  {$ENDIF}
  // set result
  case Result of
  JOBResult_Boolean:
    begin
      if JSResult then
        b:=1
      else
        b:=0;
      getModuleMemoryDataView().setUint8(ResultP, b);
    end;
  JOBResult_Double:
    getModuleMemoryDataView().setFloat64(ResultP, double(JSResult), env.IsLittleEndian);
  JOBResult_String:
    begin
    FStringResult:=String(JSResult);
    getModuleMemoryDataView().setInt32(ResultP, length(FStringResult), env.IsLittleEndian);
    end;
  JOBResult_Function,
  JOBResult_Object:
    begin
      NewId:=RegisterLocalObject(TJSObject(JSResult));
      getModuleMemoryDataView().setUint32(ResultP, longword(NewId), env.IsLittleEndian);
    end;
  else
    // no args
  end;
end;

function TJSObjectBridge.Invoke_ArrayStringResult(ObjId: TJOBObjectID; NameP,
  NameLen, Invoke, ArgsP, ResultP: NativeInt): TJOBResult;
var
  JSResult: JSValue;
begin
  // invoke
  Result:=Invoke_JSResult(ObjId,NameP,NameLen,Invoke,ArgsP,JSResult);
  if Result<>JOBResult_Success then
    exit;
  raise EJOBBridge.Create('TJSObjectBridge.Invoke_ArrayStringResult not yet implemented');
  // check result type
  //exit(GetJOBResult(JSResult));
  Result:=JOBResult_String;
  if ResultP=0 then ;
end;

function TJSObjectBridge.ReleaseObject(ObjId: TJOBObjectID): TJOBResult;
begin
  {$IFDEF VerboseJOB}
  writeln('TJSObjectBridge.ReleaseObject ',ObjId);
  {$ENDIF}
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

function TJSObjectBridge.GetStringResult(ResultP: NativeInt): TJOBResult;
var
  View: TJSDataView;
  l, i: SizeInt;
begin
  Result:=JOBResult_Success;
  l:=length(FStringResult);
  if l=0 then exit;
  View:=getModuleMemoryDataView();
  for i:=0 to l-1 do
    View.setUint16(ResultP+2*i,ord(FStringResult[i+1]),env.IsLittleEndian);
  FStringResult:='';
end;

function TJSObjectBridge.ReleaseStringResult: TJOBResult;
begin
  Result:=JOBResult_Success;
  FStringResult:='';
end;

function TJSObjectBridge.GetInvokeArguments(View: TJSDataView; ArgsP: NativeInt
  ): TJSValueDynArray;
type
  TProxyFunc = reference to function: jsvalue;
var
  p: NativeInt;

  function ReadWasmNativeInt: TWasmNativeInt;
  begin
    Result:=View.getInt32(p,env.IsLittleEndian);
    inc(p,4);
  end;

  function ReadArgMethod: TProxyFunc;
  var
    aCall, aData, aCode: TWasmNativeInt;
    i: Integer;
  begin
    aCall:=ReadWasmNativeInt;
    aData:=ReadWasmNativeInt;
    aCode:=ReadWasmNativeInt;

    Result:=function: jsvalue
      var
        Args, ResultP: TWasmNativeInt;
        TempObjIds: TJOBObjectIDArray;
      begin
        {$IFDEF VerboseJOBCallback}
        writeln('TJSObjectBridge Callback: JS Method Call=',aCall,' Data=',aData,' Code=',aCode,' Args=',JSArguments.length,' converting args for wasm...');
        {$ENDIF}
        Args:=CreateCallbackArgs(View,JSArguments,TempObjIds);
        try
          {$IFDEF VerboseJOBCallback}
          writeln('TJSObjectBridge Callback: calling Wasm...');
          {$ENDIF}
          ResultP:=CallbackHandler(aCall,aData,aCode,Args); // this frees Args, and may detach View
          View:=getModuleMemoryDataView();
          {$IFDEF VerboseJOBCallback}
          writeln('TJSObjectBridge Callback: called Wasm Call=',aCall,' Data=',aData,' Code=',aCode,' ResultP=',ResultP,' getting Result...');
          {$ENDIF}
          Result:=EatCallbackResult(View,ResultP); // this frees ResultP
          {$IFDEF VerboseJOBCallback}
          writeln('TJSObjectBridge Callback: Result=',Result);
          {$ENDIF}
        finally
          {$IFDEF VerboseJOBCallback}
          writeln('TJSObjectBridge Callback: cleaning up TempObjIds=',length(TempObjIds),' ',TempObjIds);
          {$ENDIF}
          for i:=0 to length(TempObjIds)-1 do
            ReleaseObject(TempObjIds[i]);
        end;
      end;
  end;

  function ReadString: String;
  var
    Len: TWasmNativeInt;
    aWords: TJSUint16Array;
  begin
    Len:=ReadWasmNativeInt;
    aWords:=TJSUint16Array.New(View.buffer, p,Len);
    inc(p,Len*2);
    Result:=TypedArrayToString(aWords);
  end;

  function ReadUnicodeString: String;
  var
    Len, Ptr: TWasmNativeInt;
    aWords: TJSUint16Array;
  begin
    Len:=ReadWasmNativeInt;
    Ptr:=ReadWasmNativeInt;
    aWords:=TJSUint16Array.New(View.buffer, Ptr,Len);
    Result:=TypedArrayToString(aWords);
  end;

  function ReadValue: JSValue; forward;

  function ReadArgDictionary: JSValue;
  var
    Cnt: TWasmNativeInt;
    CurName: String;
    i: Integer;
    aType: Byte;
  begin
    Cnt:=ReadWasmNativeInt;
    Result:=TJSObject.new;
    for i:=0 to Cnt-1 do
    begin
      aType:=View.getUInt8(p);
      inc(p);
      if aType<>JOBArgUnicodeString then
        raise EJOBBridge.Create('20220825000909: dictionary name must be unicodestring, but was '+IntToStr(aType));
      CurName:=ReadUnicodeString;
      TJSObject(Result)[CurName]:=ReadValue;
    end;
  end;

  function ReadArgArrayOfJSValue: JSValue;
  var
    Cnt: TWasmNativeInt;
    i: Integer;
  begin
    Cnt:=ReadWasmNativeInt;
    Result:=TJSArray.new;
    for i:=0 to Cnt-1 do
      TJSArray(Result)[i]:=ReadValue;
  end;

  function ReadArgArrayOfDouble: JSValue;
  var
    Cnt, El: TWasmNativeInt;
    i: Integer;
  begin
    Cnt:=ReadWasmNativeInt;
    El:=ReadWasmNativeInt;
    Result:=TJSArray.new;
    for i:=0 to Cnt-1 do
      TJSArray(Result)[i]:=View.getFloat64(El+i*8,env.IsLittleEndian);
  end;

  function ReadValue: JSValue;
  var
    aType: Byte;
    ObjID: LongInt;
    Obj: TJSObject;
  begin
    aType:=View.getUInt8(p);
    //writeln('TJSObjectBridge.GetInvokeArguments.ReadValue aType=',aType,' p=',p);
    inc(p);
    case aType of
    JOBArgUndefined:
      Result:=Undefined;
    JOBArgLongint:
      begin
        Result:=View.getInt32(p,env.IsLittleEndian);
        inc(p,4);
      end;
    JOBArgDouble:
      begin
        Result:=View.getFloat64(p,env.IsLittleEndian);
        inc(p,8);
      end;
    JOBArgTrue:
      Result:=true;
    JOBArgFalse:
      Result:=false;
    JOBArgChar:
      begin
        Result:=chr(View.getUint16(p,env.IsLittleEndian));
        inc(p,2);
      end;
    JOBArgString:
      Result:=ReadString;
    JOBArgUnicodeString:
      Result:=ReadUnicodeString;
    JOBArgNil:
      Result:=nil;
    JOBArgPointer:
      Result:=ReadWasmNativeInt;
    JOBArgObject:
      begin
        ObjID:=ReadWasmNativeInt;
        Obj:=FindObject(ObjID);
        if Obj=nil then
          raise EJOBBridge.Create('20220825000904: invalid JSObject '+IntToStr(ObjID));
        Result:=Obj;
      end;
    JOBArgMethod:
      Result:=ReadArgMethod;
    JOBArgDictionary:
      Result:=ReadArgDictionary;
    JOBArgArrayOfJSValue:
      Result:=ReadArgArrayOfJSValue;
    JOBArgArrayOfDouble:
      Result:=ReadArgArrayOfDouble;
    else
      raise EJOBBridge.Create('20220825000852: unknown arg type '+IntToStr(aType));
    end;
  end;

var
  Cnt: Byte;
  i: Integer;
begin
  p:=ArgsP;
  Cnt:=View.getUInt8(p);
  //writeln('TJSObjectBridge.GetInvokeArguments Cnt=',Cnt);
  inc(p);
  for i:=0 to Cnt-1 do
  begin
    Result[i]:=ReadValue;
    //writeln('TJSObjectBridge.GetInvokeArguments ',i,'/',Cnt,' = ',Result[i]);
  end;
end;

function TJSObjectBridge.CreateCallbackArgs(View: TJSDataView;
  const Args: TJSFunctionArguments; TempObjIds: TJOBObjectIDArray
  ): TWasmNativeInt;
var
  i, Len, j: Integer;
  Arg: JSValue;
  r: TJOBResult;
  s: String;
  NewId: TJOBObjectID;
  p: LongWord;
begin
  Result:=0;
  if Args.Length=0 then exit;
  if Args.Length>255 then
    raise EJOBBridge.Create('too many arguments');

  // compute needed wasm memory
  Len:=1;
  for i:=0 to Args.Length-1 do
  begin
    Arg:=Args[i];
    r:=GetJOBResult(Arg);
    inc(Len);
    case r of
    JOBResult_Boolean: ;
    JOBResult_Double: inc(Len,8);
    JOBResult_String: inc(Len,4+2*TJSString(Arg).length);
    JOBResult_Function,
    JOBResult_Object: inc(Len,4);
    end;
  end;

  // allocate wasm memory
  Result:=WasiExports.AllocMem(Len);

  // write
  p:=Result;
  View.setUint8(p,Args.Length);
  inc(p);
  for i:=0 to Args.Length-1 do
  begin
    Arg:=Args[i];
    r:=GetJOBResult(Arg);
    writeln('TJSObjectBridge.CreateCallbackArgs ',i,'/',Args.Length,' r=',r);
    case r of
    JOBResult_Null:
      begin
        View.setUint8(p,JOBArgNil);
        inc(p);
      end;
    JOBResult_Boolean:
      begin
      if Arg then
        View.setUint8(p,JOBArgTrue)
      else
        View.setUint8(p,JOBArgFalse);
      inc(p);
      end;
    JOBResult_Double:
      begin
        View.setUint8(p,JOBArgDouble);
        inc(p);
        View.setFloat64(p,double(Arg),env.IsLittleEndian);
        inc(p,8);
      end;
    JOBResult_String:
      begin
        View.setUint8(p,JOBArgUnicodeString);
        inc(p);
        s:=String(Arg);
        View.setUint32(p,length(s),env.IsLittleEndian);
        inc(p,4);
        for j:=0 to length(s)-1 do
        begin
          View.setUint16(p,ord(s[j+1]),env.IsLittleEndian);
          inc(p,2);
        end;
      end;
    JOBResult_Function,
    JOBResult_Object:
      begin
        View.setUint8(p,JOBArgObject);
        inc(p);
        NewId:=RegisterLocalObject(TJSObject(Arg));
        TJSArray(TempObjIds).push(NewId);
        writeln('TJSObjectBridge.CreateCallbackArgs Object ID=',NewID);
        View.setInt32(p, NewId, env.IsLittleEndian);
        inc(p,4);
      end;
    else
      View.setUint8(p,JOBArgUndefined);
      inc(p);
    end;
  end;
end;

function TJSObjectBridge.EatCallbackResult(View: TJSDataView;
  ResultP: TWasmNativeInt): jsvalue;
var
  p: TWasmNativeInt;

  function EatString: JSValue;
  var
    Len: LongWord;
    i: Integer;
    a: TWordDynArray;
  begin
    Len:=View.getUInt32(p,env.IsLittleEndian);
    inc(p,4);
    SetLength(a,Len);
    for i:=0 to Len-1 do begin
      a[i]:=View.getUint16(p,env.IsLittleEndian);
      inc(p,2);
    end;
    Result:=TJSFunction(@TJSString.fromCharCode).apply(nil,a);
  end;

var
  aType: Byte;
  ObjId: LongInt;
begin
  if ResultP=0 then
    exit(Undefined);
  p:=ResultP;
  try
    aType:=View.getUint8(p);
    //writeln('TJSObjectBridge.EatCallbackResult aType=',aType);
    inc(p);
    case aType of
    JOBArgTrue: Result:=true;
    JOBArgFalse: Result:=false;
    JOBArgLongint: Result:=View.getInt32(p,env.IsLittleEndian);
    JOBArgDouble: Result:=View.getFloat64(p,env.IsLittleEndian);
    JOBArgUnicodeString: Result:=EatString;
    JOBArgNil: Result:=nil;
    JOBArgObject:
      begin
        ObjId:=View.getInt32(p,env.IsLittleEndian);
        Result:=FindObject(ObjId);
        writeln('TJSObjectBridge.EatCallbackResult ObjID=',ObjId,' Result=',Result<>nil);
      end;
    else
      Result:=Undefined;
    end;
  finally
    //writeln('TJSObjectBridge.EatCallbackResult freeing result...');
    WasiExports.freeMem(ResultP);
  end;
end;

function TJSObjectBridge.Get_GlobalID(NameP, NameLen: NativeInt
  ): TJOBObjectID;
var
  View: TJSDataView;
  aWords: TJSUint16Array;
  aName: String;
begin
  View:=getModuleMemoryDataView();
  aWords:=TJSUint16Array.New(View.buffer, NameP, NameLen);
  aName:=TypedArrayToString(aWords);
  Result:=FindGlobalObject(aName);
end;

function TJSObjectBridge.GetJOBResult(v: jsvalue): TJOBResult;
begin
  case jstypeof(v) of
  'undefined': Result:=JOBResult_Undefined;
  'boolean': Result:=JOBResult_Boolean;
  'number': Result:=JOBResult_Double;
  'string': Result:=JOBResult_String;
  'symbol': Result:=JOBResult_Symbol;
  'bigint': Result:=JOBResult_BigInt;
  'function': Result:=JOBResult_Function;
  'object': if v=nil then Result:=JOBResult_Null else Result:=JOBResult_Object;
  else Result:=JOBResult_None;
  end;
end;

end.  
