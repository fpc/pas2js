{
    This file is part of the Pas2JS run time library.
    Copyright (c) 2018 by Mattias Gaertner

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
unit RTTI;

{$mode objfpc}
{$ModeSwitch advancedrecords}

interface

uses
  JS, RTLConsts, Types, SysUtils, TypInfo;

resourcestring
  SErrInvokeInvalidCodeAddr = 'CodeAddress is not a function';

type

  { TValue }

  TValue = record
  private
    FTypeInfo: TTypeInfo;
    FData: JSValue;
    function GetIsEmpty: boolean;
    function GetTypeKind: TTypeKind;
  public
    class function FromJSValue(v: JSValue): TValue; static;

    property Kind: TTypeKind read GetTypeKind;
    property TypeInfo: TTypeInfo read FTypeInfo;

    property IsEmpty: boolean read GetIsEmpty; // check if nil or undefined
    function IsObject: boolean;
    function AsObject: TObject;
    function IsObjectInstance: boolean;
    function IsArray: boolean;
    function IsClass: boolean;
    function AsClass: TClass;
    function IsOrdinal: boolean;
    function AsOrdinal: NativeInt;
    function AsBoolean: boolean;
    //ToDo: function AsCurrency: Currency;
    function AsInteger: Integer;
    function AsNativeInt: NativeInt;
    function AsInterface: IInterface;
    function AsString: string;
    function AsUnicodeString: UnicodeString;
    function AsExtended: Extended;
    function ToString: String;
    function GetArrayLength: SizeInt;
    function GetArrayElement(aIndex: SizeInt): TValue;
    //ToDo: procedure SetArrayElement(aIndex: SizeInt; constref AValue: TValue);
    function IsType(ATypeInfo: PTypeInfo): boolean;
  end;

  TRttiType = class;

  { TRTTIContext }

  TRTTIContext = record
  private
    FPool: TJSObject; // maps 'modulename.typename' to TRTTIType
    class constructor Init;
  public
    class function Create: TRTTIContext; static;
    procedure Free;

    function GetType(aTypeInfo: PTypeInfo): TRTTIType; overload;
    function GetType(aClass: TClass): TRTTIType; overload;
  end;

  { TRttiObject }

  TRttiObject = class abstract
  public
    //property Handle: Pointer read GetHandle;  not supported in pas2js
    function GetAttributes: TCustomAttributeArray; virtual;
  end;

  { TRttiNamedObject }

  TRttiNamedObject = class(TRttiObject)
  protected
    function GetName: string; virtual;
  public
    property Name: string read GetName;
  end;

  { TRttiMember }

  TMemberVisibility=(
    mvPrivate,
    mvProtected,
    mvPublic,
    mvPublished);

  TRttiMember = class(TRttiNamedObject)
  private
    FTypeInfo: TTypeMember;
    FParent: TRttiType;
  protected
    function GetName: string; override;
    function GetVisibility: TMemberVisibility; virtual;
  public
    constructor Create(AParent: TRttiType; ATypeInfo: TTypeMember);
    function GetAttributes: TCustomAttributeArray; override;
    property Visibility: TMemberVisibility read GetVisibility;
    property Parent: TRttiType read FParent;
  end;

  { TRttiField }

  TRttiField = class(TRttiMember)
  private
    function GetFieldType: TRttiType;
  public
    property FieldType: TRttiType read GetFieldType;
    //function GetValue(Instance: Pointer): TValue;
    //procedure SetValue(Instance: Pointer; const AValue: TValue);
    //function ToString: string; override;
  end;
  TRttiFieldArray = array of TRttiField;

  { TRttiMethod }

  TRttiMethod = class(TRttiMember)
  private
    function GetIsClassMethod: boolean;
    function GetIsConstructor: boolean;
    function GetIsDestructor: boolean;
    function GetIsExternal: boolean;
    function GetIsStatic: boolean;
    function GetIsVarArgs: boolean;
    function GetMethodKind: TMethodKind;
    function GetReturnType: TRttiType;
  public
    property ReturnType: TRttiType read GetReturnType;
    property MethodKind: TMethodKind read GetMethodKind;
    property IsConstructor: boolean read GetIsConstructor;
    property IsDestructor: boolean read GetIsDestructor;
    property IsClassMethod: boolean read GetIsClassMethod;
    property IsExternal: boolean read GetIsExternal;
    property IsStatic: boolean read GetIsStatic;// true = has Self argument
    property IsVarArgs: boolean read GetIsVarArgs;
    //function GetParameters:
  end;
  TRttiMethodArray = array of TRttiMethod;

  { TRttiProperty }

  TRttiProperty = class(TRttiMember)
  private
    function GetPropertyType: TRttiType;
    function GetIsWritable: boolean;
    function GetIsReadable: boolean;
  protected
    function GetVisibility: TMemberVisibility; override;
  public
    //function GetValue(Instance: Pointer): TValue;
    //procedure SetValue(Instance: Pointer; const AValue: TValue);
    property PropertyType: TRttiType read GetPropertyType;
    property IsReadable: boolean read GetIsReadable;
    property IsWritable: boolean read GetIsWritable;
    property Visibility: TMemberVisibility read GetVisibility;
  end;
  TRttiPropertyArray = array of TRttiProperty;

  { TRttiType }

  TRttiType = class(TRttiNamedObject)
  private
    FAttributes: TCustomAttributeArray;
    FTypeInfo: TTypeInfo;
    //FMethods: specialize TArray<TRttiMethod>;
    //function GetAsInstance: TRttiInstanceType;
  protected
    function GetName: string; override;
    //function GetHandle: Pointer; override;
    function GetIsInstance: boolean; virtual;
    //function GetIsManaged: boolean; virtual;
    function GetIsOrdinal: boolean; virtual;
    function GetIsRecord: boolean; virtual;
    function GetIsSet: boolean; virtual;
    function GetTypeKind: TTypeKind; virtual;
    //function GetTypeSize: integer; virtual;
    //function GetBaseType: TRttiType; virtual;
  public
    constructor Create(ATypeInfo : PTypeInfo);
    destructor Destroy; override;
    function GetAttributes: TCustomAttributeArray; override;
    function GetField(const AName: string): TRttiField; virtual;
    function GetMethods(const aName: String): TRttiMethodArray; virtual;
    function GetMethod(const aName: String): TRttiMethod; virtual;
    function GetProperty(const AName: string): TRttiProperty; virtual;
    //function GetIndexedProperty(const AName: string): TRttiIndexedProperty; virtual;

    function GetDeclaredProperties: TRttiPropertyArray; virtual;
    //function GetDeclaredIndexedProperties: TRttiIndexedPropertyArray; virtual;
    function GetDeclaredMethods: TRttiMethodArray; virtual;
    function GetDeclaredFields: TRttiFieldArray; virtual;

    property IsInstance: boolean read GetIsInstance;
    //property isManaged: boolean read GetIsManaged;
    property IsOrdinal: boolean read GetIsOrdinal;
    property IsRecord: boolean read GetIsRecord;
    property IsSet: boolean read GetIsSet;
    //property BaseType: TRttiType read GetBaseType;
    //property AsInstance: TRttiInstanceType read GetAsInstance;
    property TypeKind: TTypeKind read GetTypeKind;
    //property TypeSize: integer read GetTypeSize;
  end;

  { TRttiStructuredType }

  TRttiStructuredType = class abstract(TRttiType)
  end;

  { TRttiInstanceType }

  TRttiInstanceType = class(TRttiStructuredType)
  private
    function GetClassTypeInfo: TTypeInfoClass;
    function GetMetaClassType: TClass;
  public
    constructor Create(ATypeInfo: PTypeInfo);
    property ClassTypeInfo: TTypeInfoClass read GetClassTypeInfo;
    property MetaClassType: TClass read GetMetaClassType;
    //function GetDeclaredProperties: TRttiPropertyArray;
  end;

  EInvoke = EJS;

  TVirtualInterfaceInvokeEvent = function(const aMethodName: string;
    const Args: TJSValueDynArray): JSValue of object;

  { TVirtualInterface: A class that can implement any IInterface. Any method
    call is handled by the OnInvoke event. }
  TVirtualInterface = class(TInterfacedObject, IInterface)
  private
    FOnInvoke: TVirtualInterfaceInvokeEvent;
  public
    constructor Create(InterfaceTypeInfo: Pointer); overload; assembler;
    constructor Create(InterfaceTypeInfo: Pointer;
      const InvokeEvent: TVirtualInterfaceInvokeEvent); overload;
    property OnInvoke: TVirtualInterfaceInvokeEvent read FOnInvoke write FOnInvoke;
  end;

procedure CreateVirtualCorbaInterface(InterfaceTypeInfo: Pointer;
  const MethodImplementation: TVirtualInterfaceInvokeEvent; out IntfVar); assembler;

function Invoke(ACodeAddress: Pointer; const AArgs: TJSValueDynArray;
  ACallConv: TCallConv; AResultType: PTypeInfo; AIsStatic: Boolean;
  AIsConstructor: Boolean): TValue;

implementation

var
  GRttiContext: TRTTIContext;

procedure CreateVirtualCorbaInterface(InterfaceTypeInfo: Pointer;
  const MethodImplementation: TVirtualInterfaceInvokeEvent; out IntfVar); assembler;
asm
  var IntfType = InterfaceTypeInfo.interface;
  var i = Object.create(IntfType);
  var o = { $name: "virtual", $fullname: "virtual" };
  i.$o = o;
  do {
    var names = IntfType.$names;
    if (!names) break;
    for (var j=0; j<names.length; j++){
      let fnname = names[j];
      i[fnname] = function(){ return MethodImplementation(fnname,arguments); };
    }
    IntfType = Object.getPrototypeOf(IntfType);
  } while(IntfType!=null);
  IntfVar.set(i);
end;

{ TValue }

function TValue.GetTypeKind: TTypeKind;
begin
  if TypeInfo=nil then
    Result:=tkUnknown
  else
    Result:=FTypeInfo.Kind;
end;

class function TValue.FromJSValue(v: JSValue): TValue;
var
  i: NativeInt;
begin
  Result.FData:=v;
  case jsTypeOf(v) of
  'number':
    if JS.isInteger(v) then
      begin
      i:=NativeInt(v);
      if (i>=low(integer)) and (i<=high(integer)) then
        Result.FTypeInfo:=system.TypeInfo(Integer)
      else
        Result.FTypeInfo:=system.TypeInfo(NativeInt);
      end
    else
      Result.FTypeInfo:=system.TypeInfo(Double);
  'string':  Result.FTypeInfo:=system.TypeInfo(String);
  'boolean': Result.FTypeInfo:=system.TypeInfo(Boolean);
  'object':
    begin
    if v=nil then
      Result.FTypeInfo:=system.TypeInfo(Pointer)
    else if JS.isClass(v) and JS.isExt(v,TObject) then
      Result.FTypeInfo:=system.TypeInfo(TClass(v))
    else if JS.isObject(v) and JS.isExt(v,TObject) then
      Result.FTypeInfo:=system.TypeInfo(TObject(v))
    else
      Result.FTypeInfo:=system.TypeInfo(Pointer);
    if (Result.FTypeInfo=JS.Undefined) or (Result.FTypeInfo=nil) then
      Result.FTypeInfo:=system.TypeInfo(Pointer);
    end
  else
    Result.FTypeInfo:=system.TypeInfo(JSValue);
  end;
end;

function TValue.IsObject: boolean;
begin
  Result:=IsEmpty or (TypeInfo.Kind=tkClass);
end;

function TValue.AsObject: TObject;
begin
  if IsObject or (IsClass and not js.isObject(FData)) then
    Result := TObject(FData)
  else
    raise EInvalidCast.Create(SErrInvalidTypecast);
end;

function TValue.IsObjectInstance: boolean;
begin
  Result:=(TypeInfo<>nil) and (TypeInfo.Kind=tkClass);
end;

function TValue.IsArray: boolean;
begin
  Result := Kind in [tkArray, tkDynArray];
end;

function TValue.IsClass: boolean;
var
  k: TTypeKind;
begin
  k:=Kind;
  Result :=  (k = tkClassRef)
         or ((k in [tkClass,tkUnknown]) and not JS.IsObject(FData));
end;

function TValue.AsClass: TClass;
begin
  if IsClass then
    Result := TClass(FData)
  else
    raise EInvalidCast.Create(SErrInvalidTypecast);
end;

function TValue.IsOrdinal: boolean;
var
  k: TTypeKind;
begin
  k:=Kind;
  Result := (k in [tkInteger, tkBool]) or
            ((k in [tkClass, tkClassRef, tkUnknown]) and not JS.isObject(FData));
end;

function TValue.AsOrdinal: NativeInt;
begin
  if IsOrdinal then
    Result:=NativeInt(FData)
  else
    raise EInvalidCast.Create(SErrInvalidTypecast);
end;

function TValue.AsBoolean: boolean;
begin
  if (Kind = tkBool) then
    Result:=boolean(FData)
  else
    raise EInvalidCast.Create(SErrInvalidTypecast);
end;

function TValue.AsInteger: Integer;
begin
  if JS.isInteger(FData) then
    Result:=NativeInt(FData)
  else
    raise EInvalidCast.Create(SErrInvalidTypecast);
end;

function TValue.AsNativeInt: NativeInt;
begin
  if JS.isInteger(FData) then
    Result:=NativeInt(FData)
  else
    raise EInvalidCast.Create(SErrInvalidTypecast);
end;

function TValue.AsInterface: IInterface;
var
  k: TTypeKind;
begin
  k:=Kind;
  if k = tkInterface then
    Result := IInterface(FData)// ToDo
  else if (k in [tkClass, tkClassRef, tkUnknown]) and not JS.isObject(FData) then
    Result := Nil
  else
    raise EInvalidCast.Create(SErrInvalidTypecast);
end;

function TValue.AsString: string;
begin
  if js.isString(FData) then
    Result:=String(FData)
  else
    raise EInvalidCast.Create(SErrInvalidTypecast);
end;

function TValue.AsUnicodeString: UnicodeString;
begin
  Result:=AsString;
end;

function TValue.AsExtended: Extended;
begin
  if js.isNumber(FData) then
    Result:=Double(FData)
  else
    raise EInvalidCast.Create(SErrInvalidTypecast);
end;

function TValue.ToString: String;
begin
  case Kind of
    tkString: Result := AsString;
    tkInteger: Result := IntToStr(AsNativeInt);
    tkBool: Result := BoolToStr(AsBoolean, True);
  else
    Result := '';
  end;
end;

function TValue.GetArrayLength: SizeInt;
begin
  if not IsArray then
    raise EInvalidCast.Create(SErrInvalidTypecast);
  Result:=length(TJSValueDynArray(FData));
end;

function TValue.GetArrayElement(aIndex: SizeInt): TValue;
var
  StaticTI: TTypeInfoStaticArray;
  DynIT: TTypeInfoDynArray;
begin
  case Kind of
  tkDynArray:
    begin
    DynIT:=TTypeInfoDynArray(FTypeInfo);
    Result.FTypeInfo:=DynIT.ElType;
    if DynIT.DimCount<>1 then
      raise EInvalidCast.Create(SErrInvalidTypecast);
    end;
  tkArray:
    begin
    StaticTI:=TTypeInfoStaticArray(FTypeInfo);
    if length(StaticTI.Dims)<>1 then
      raise EInvalidCast.Create(SErrInvalidTypecast);
    Result.FTypeInfo:=StaticTI.ElType;
    end;
  else
    raise EInvalidCast.Create(SErrInvalidTypecast);
  end;
  Result.FData:=TJSValueDynArray(FData)[aIndex];
end;

function TValue.IsType(ATypeInfo: PTypeInfo): boolean;
begin
  Result := ATypeInfo = TypeInfo;
end;

function TValue.GetIsEmpty: boolean;
begin
  if (TypeInfo=nil) or (FData=Undefined) or (FData=nil) then
    exit(true);
  case TypeInfo.Kind of
  tkDynArray:
    Result:=TJSArray(FData).Length=0;
  else
    Result:=false;
  end;
end;

{ TRttiInstanceType }

function TRttiInstanceType.GetClassTypeInfo: TTypeInfoClass;
begin
  Result:=TTypeInfoClass(FTypeInfo);
end;

function TRttiInstanceType.GetMetaClassType: TClass;
begin
  Result:=TTypeInfoClass(FTypeInfo).ClassType;
end;

constructor TRttiInstanceType.Create(ATypeInfo: PTypeInfo);
begin
  if not (TTypeInfo(ATypeInfo) is TTypeInfoClass) then
    raise EInvalidCast.Create('');
  inherited Create(ATypeInfo);
end;

{ TRTTIContext }

class constructor TRTTIContext.Init;
begin
  GRttiContext:=TRTTIContext.Create;
end;

class function TRTTIContext.Create: TRTTIContext;
begin
  Result.FPool:=TJSObject.new;
end;

procedure TRTTIContext.Free;
var
  key: string;
  o: TRttiType;
begin
  for key in FPool do
    if FPool.hasOwnProperty(key) then begin
      o:=TRTTIType(FPool[key]);
      o.Free;
      end;
  FPool:=nil;
end;

function TRTTIContext.GetType(aTypeInfo: PTypeInfo): TRTTIType;
var
  t: TTypeinfo absolute aTypeInfo;
  Name: String;
begin
  if aTypeInfo=nil then exit(nil);
  Name:=t.Name;
  if isModule(t.Module) then
    Name:=t.Module.Name+'.'+Name;
  if FPool.hasOwnProperty(Name) then
    Result:=TRttiType(FPool[Name])
  else
    begin
    Result:=TRttiType.Create(aTypeInfo);
    FPool[Name]:=Result;
    end;
end;

function TRTTIContext.GetType(aClass: TClass): TRTTIType;
begin
  if aClass=nil then exit(nil);
  Result:=GetType(TypeInfo(aClass));
end;

{ TRttiObject }

function TRttiObject.GetAttributes: TCustomAttributeArray;
begin
  Result:=nil;
end;

{ TRttiNamedObject }

function TRttiNamedObject.GetName: string;
begin
  Result:='';
end;

{ TRttiMember }

function TRttiMember.GetName: string;
begin
  Result:=FTypeInfo.Name;
end;

function TRttiMember.GetVisibility: TMemberVisibility;
begin
  Result:=mvPublished;
end;

constructor TRttiMember.Create(AParent: TRttiType; ATypeInfo: TTypeMember);
begin
  inherited Create();
  FParent := AParent;
  FTypeInfo:=ATypeInfo;
end;

function TRttiMember.GetAttributes: TCustomAttributeArray;
begin
  Result:=inherited GetAttributes;
end;

{ TRttiField }

function TRttiField.GetFieldType: TRttiType;
begin
  Result := GRttiContext.GetType(FTypeInfo);
end;

{ TRttiMethod }

function TRttiMethod.GetIsClassMethod: boolean;
begin
  Result:=TTypeMemberMethod(FTypeInfo).MethodKind in [mkClassFunction,mkClassProcedure];
end;

function TRttiMethod.GetIsConstructor: boolean;
begin
  Result:=TTypeMemberMethod(FTypeInfo).MethodKind=mkConstructor;
end;

function TRttiMethod.GetIsDestructor: boolean;
begin
  Result:=TTypeMemberMethod(FTypeInfo).MethodKind=mkDestructor;
end;

function TRttiMethod.GetIsExternal: boolean;
begin
  Result:=(TTypeMemberMethod(FTypeInfo).ProcSig.Flags and 4)>0; // pfExternal
end;

function TRttiMethod.GetIsStatic: boolean;
begin
  Result:=(TTypeMemberMethod(FTypeInfo).ProcSig.Flags and 1)>0; // pfStatic
end;

function TRttiMethod.GetIsVarArgs: boolean;
begin
  Result:=(TTypeMemberMethod(FTypeInfo).ProcSig.Flags and 2)>0; // pfVarargs
end;

function TRttiMethod.GetMethodKind: TMethodKind;
begin
  Result:=TTypeMemberMethod(FTypeInfo).MethodKind;;
end;

function TRttiMethod.GetReturnType: TRttiType;
begin
  Result := GRttiContext.GetType(TTypeMemberMethod(FTypeInfo).ProcSig.ResultType);
end;

{ TRttiProperty }

function TRttiProperty.GetPropertyType: TRttiType;
begin
  Result := GRttiContext.GetType(FTypeInfo);
end;

function TRttiProperty.GetIsWritable: boolean;
begin
  Result := TTypeMemberProperty(FTypeInfo).Setter<>'';
end;

function TRttiProperty.GetIsReadable: boolean;
begin
  Result := TTypeMemberProperty(FTypeInfo).Getter<>'';
end;

function TRttiProperty.GetVisibility: TMemberVisibility;
begin
  // At this moment only pulished rtti-property-info is supported by pas2js
  Result := mvPublished;
end;

{ TRttiType }

function TRttiType.GetName: string;
begin
  Result:=FTypeInfo.Name;
end;

function TRttiType.GetIsInstance: boolean;
begin
  Result:=false;
end;

function TRttiType.GetIsOrdinal: boolean;
begin
  Result:=false;
end;

function TRttiType.GetIsRecord: boolean;
begin
  Result:=false;
end;

function TRttiType.GetIsSet: boolean;
begin
  Result:=false;
end;

function TRttiType.GetTypeKind: TTypeKind;
begin
  Result:=FTypeInfo.Kind;
end;

constructor TRttiType.Create(ATypeInfo: PTypeInfo);
begin
  inherited Create();
  FTypeInfo:=TTypeInfo(ATypeInfo);
end;

destructor TRttiType.Destroy;
var
  o: TCustomAttribute;
begin
  for o in FAttributes do
    o.Free;
  FAttributes:=nil;
  inherited Destroy;
end;

function TRttiType.GetAttributes: TCustomAttributeArray;
begin
  FAttributes:=GetRTTIAttributes(FTypeInfo.Attributes);
  Result:=FAttributes;
end;

function TRttiType.GetDeclaredProperties: TRttiPropertyArray;
begin
  Result:=nil;
end;

function TRttiType.GetProperty(const AName: string): TRttiProperty;
begin
  Result:=nil;
  if AName='' then ;
end;

function TRttiType.GetMethods(const aName: String): TRttiMethodArray;
begin
  Result:=nil;
end;

function TRttiType.GetMethod(const aName: String): TRttiMethod;
begin
  Result:=nil;
  if aName='' then ;
end;

function TRttiType.GetDeclaredMethods: TRttiMethodArray;
begin
  Result:=nil;
end;

function TRttiType.GetDeclaredFields: TRttiFieldArray;
begin
  Result:=nil;
end;

function TRttiType.GetField(const AName: string): TRttiField;
begin
  Result:=nil;
  if AName='' then ;
end;

{ TVirtualInterface }

constructor TVirtualInterface.Create(InterfaceTypeInfo: Pointer); assembler;
asm
  var IntfType = InterfaceTypeInfo.interface;
  if (IntfType.$kind !== 'com') rtl.raiseE('EInvalidCast');
  var guid = IntfType.$guid;
  var i = Object.create(IntfType); // needed by IntfVar is IntfType
  i.$o = this;
  // copy IInterface methods: _AddRef, _Release, QueryInterface
  var iinterfaceguid = '{00000000-0000-0000-C000-000000000046}';
  var map = this.$intfmaps[iinterfaceguid];
  for (var key in map){
    var v = map[key];
    if (typeof(v)!=='function') continue;
    i[key] = map[key];
  }
  // all other methods call OnInvoke
  do {
    var names = IntfType.$names;
    if (!names) break;
    for (var j=0; j<names.length; j++){
      let fnname = names[j];
      if (i[fnname]) continue;
      i[fnname] = function(){ return this.$o.FOnInvoke(fnname,arguments); };
    }
    IntfType = Object.getPrototypeOf(IntfType);
  } while(IntfType!=null);
  // create a new list of interface map, supporting IInterface and IntfType
  this.$intfmaps = {};
  this.$intfmaps[iinterfaceguid] = map;
  this.$intfmaps[guid] = {};
  // store the implementation of IntfType (used by the as-operator)
  this.$interfaces = {};
  this.$interfaces[guid] = i;
end;

constructor TVirtualInterface.Create(InterfaceTypeInfo: Pointer;
  const InvokeEvent: TVirtualInterfaceInvokeEvent);
begin
  Create(InterfaceTypeInfo);
  OnInvoke:=InvokeEvent;
end;

function Invoke(ACodeAddress: Pointer; const AArgs: TJSValueDynArray;
  ACallConv: TCallConv; AResultType: PTypeInfo; AIsStatic: Boolean;
  AIsConstructor: Boolean): TValue;
begin
  if ACallConv=ccReg then ;
  if AIsStatic then ;
  if AIsConstructor then
    raise EInvoke.Create('not supported');
  if isFunction(ACodeAddress) then
    begin
    Result.FData := TJSFunction(ACodeAddress).apply(nil, AArgs);
    if AResultType<>nil then
      Result.FTypeInfo:=AResultType
    else
      Result.FTypeInfo:=TypeInfo(JSValue);
    end
  else
    raise EInvoke.Create(SErrInvokeInvalidCodeAddr);
end;

end.

