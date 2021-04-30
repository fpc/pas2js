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
  SErrTypeIsNotEnumerated  = 'Type %s is not an enumerated type';

type
  { TValue }

  TValue = record
  private
    FTypeInfo: TTypeInfo;
    FData: JSValue;
    function GetIsEmpty: boolean;
    function GetTypeKind: TTypeKind;
  public
    generic class function From<T>(const Value: T): TValue; static;
    class function FromJSValue(v: JSValue): TValue; static;

    property Kind: TTypeKind read GetTypeKind;
    property TypeInfo: TTypeInfo read FTypeInfo;

    property IsEmpty: boolean read GetIsEmpty; // check if nil or undefined
    generic function AsType<T>: T;
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
    procedure SetArrayElement(aIndex: SizeInt; const AValue: TValue);
    procedure SetArrayLength(const Size: SizeInt);
    function IsType(ATypeInfo: PTypeInfo): boolean;
    function AsJSValue: JSValue;
    class function Empty: TValue; static;
    class function Make(TypeInfo: TTypeInfo; const Value: JSValue): TValue; static;
    class function Make(const Value: TValue): TValue; static;
  end;

  TRttiType = class;
  TRttiInstanceType = class;

  { TRTTIContext }

  TRTTIContext = record
  private
    FPool: TJSObject; // maps 'modulename.typename' to TRTTIType
    class constructor Init;
  public
    class function Create: TRTTIContext; static;
    procedure Free;

    function FindType(const AQualifiedName: String): TRttiType;
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
    function GetMemberTypeInfo: TTypeMember;
    function GetName: string; override;
    function GetVisibility: TMemberVisibility; virtual;
  public
    constructor Create(AParent: TRttiType; ATypeInfo: TTypeMember);
    function GetAttributes: TCustomAttributeArray; override;

    property MemberTypeInfo: TTypeMember read GetMemberTypeInfo;
    property Visibility: TMemberVisibility read GetVisibility;
    property Parent: TRttiType read FParent;
  end;

  { TRttiField }

  TRttiField = class(TRttiMember)
  private
    function GetFieldType: TRttiType;
    function GetFieldTypeInfo: TTypeMemberField;
  public
    constructor Create(AParent: TRttiType; ATypeInfo: TTypeMember);
    function GetValue(Instance: JSValue): TValue;
    procedure SetValue(Instance: JSValue; const AValue: TValue);
    property FieldType: TRttiType read GetFieldType;
    property FieldTypeInfo: TTypeMemberField read GetFieldTypeInfo;
  end;

  TRttiFieldArray = specialize TArray<TRttiField>;

  { TRttiParameter }

  TRttiParameter = class(TRttiNamedObject)
  private
    FParamType: TRttiType;
    FFlags: TParamFlags;
    FName: String;
  protected
    function GetName: string; override;
  public
    property Flags: TParamFlags read FFlags;
    property ParamType: TRttiType read FParamType;
  end;

  TRttiParameterArray = specialize TArray<TRttiParameter>;

  { TRttiMethod }

  TRttiMethod = class(TRttiMember)
  private
    FParameters: TRttiParameterArray;
    FParametersLoaded: Boolean;

    function GetIsAsyncCall: Boolean;
    function GetIsClassMethod: Boolean;
    function GetIsConstructor: Boolean;
    function GetIsDestructor: Boolean;
    function GetIsExternal: Boolean;
    function GetIsSafeCall: Boolean;
    function GetIsStatic: Boolean;
    function GetIsVarArgs: Boolean;
    function GetMethodKind: TMethodKind;
    function GetMethodTypeInfo: TTypeMemberMethod;
    function GetProcedureFlags: TProcedureFlags;
    function GetReturnType: TRttiType;

    procedure LoadParameters;
  public
    function GetParameters: TRttiParameterArray;

    property IsAsyncCall: Boolean read GetIsAsyncCall;
    property IsClassMethod: Boolean read GetIsClassMethod;
    property IsConstructor: Boolean read GetIsConstructor;
    property IsDestructor: Boolean read GetIsDestructor;
    property IsExternal: Boolean read GetIsExternal;
    property IsSafeCall: Boolean read GetIsSafeCall;
    property IsStatic: Boolean read GetIsStatic;
    property MethodKind: TMethodKind read GetMethodKind;
    property MethodTypeInfo: TTypeMemberMethod read GetMethodTypeInfo;
    property ReturnType: TRttiType read GetReturnType;
  end;

  TRttiMethodArray = specialize TArray<TRttiMethod>;

  { TRttiProperty }

  TRttiProperty = class(TRttiMember)
  private
    function GetPropertyTypeInfo: TTypeMemberProperty;
    function GetPropertyType: TRttiType;
    function GetIsWritable: boolean;
    function GetIsReadable: boolean;
  protected
    function GetVisibility: TMemberVisibility; override;
  public
    constructor Create(AParent: TRttiType; ATypeInfo: TTypeMember);
    function GetValue(Instance: JSValue): TValue;

    procedure SetValue(Instance: JSValue; const AValue: JSValue); overload;
    procedure SetValue(Instance: JSValue; const AValue: TValue); overload;
    procedure SetValue(Instance: TObject; const AValue: TValue); overload;

    property PropertyTypeInfo: TTypeMemberProperty read GetPropertyTypeInfo;
    property PropertyType: TRttiType read GetPropertyType;
    property IsReadable: boolean read GetIsReadable;
    property IsWritable: boolean read GetIsWritable;
    property Visibility: TMemberVisibility read GetVisibility;
  end;

  TRttiPropertyArray = specialize TArray<TRttiProperty>;

  { TRttiType }

  TRttiType = class(TRttiNamedObject)
  private
    FAttributes: TCustomAttributeArray;
    FTypeInfo: TTypeInfo;
    //FMethods: specialize TArray<TRttiMethod>;
    function GetAsInstance: TRttiInstanceType;
  protected
    function GetName: string; override;
    //function GetHandle: Pointer; override;
    function GetIsInstance: boolean;
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
    function GetFields: TRttiFieldArray; virtual;
    function GetMethods: TRttiMethodArray; virtual;
    function GetMethods(const aName: String): TRttiMethodArray; virtual;
    function GetMethod(const aName: String): TRttiMethod; virtual;
    function GetProperty(const AName: string): TRttiProperty; virtual;
    //function GetIndexedProperty(const AName: string): TRttiIndexedProperty; virtual;

    function GetDeclaredProperties: TRttiPropertyArray; virtual;
    //function GetDeclaredIndexedProperties: TRttiIndexedPropertyArray; virtual;
    function GetDeclaredMethods: TRttiMethodArray; virtual;
    function GetDeclaredFields: TRttiFieldArray; virtual;

    property Handle: TTypeInfo read FTypeInfo;
    property IsInstance: boolean read GetIsInstance;
    //property isManaged: boolean read GetIsManaged;
    property IsOrdinal: boolean read GetIsOrdinal;
    property IsRecord: boolean read GetIsRecord;
    property IsSet: boolean read GetIsSet;
    //property BaseType: TRttiType read GetBaseType;
    property AsInstance: TRttiInstanceType read GetAsInstance;
    property TypeKind: TTypeKind read GetTypeKind;
    //property TypeSize: integer read GetTypeSize;
  end;

  TRttiTypeClass = class of TRttiType;

  { TRttiStructuredType }

  TRttiStructuredType = class abstract(TRttiType)
  private
    FMethods: TRttiMethodArray;
    FProperties: TRttiPropertyArray;
  protected
    function GetAncestor: TRttiStructuredType; virtual;
    function GetStructTypeInfo: TTypeInfoStruct;
  public
    constructor Create(ATypeInfo: PTypeInfo);

    destructor Destroy; override;

    function GetDeclaredMethods: TRttiMethodArray; override;
    function GetDeclaredProperties: TRttiPropertyArray; override;
    function GetMethod(const aName: String): TRttiMethod; override;
    function GetMethods: TRttiMethodArray; override;
    function GetMethods(const aName: String): TRttiMethodArray; override;
    function GetProperties: TRttiPropertyArray;
    function GetProperty(const AName: string): TRttiProperty; override;

    property StructTypeInfo: TTypeInfoStruct read GetStructTypeInfo;
  end;

  { TRttiInstanceType }

  TRttiInstanceType = class(TRttiStructuredType)
  private
    FFields: TRttiFieldArray;

    function GetClassTypeInfo: TTypeInfoClass;
    function GetMetaClassType: TClass;
  protected
    function GetAncestor: TRttiStructuredType; override;
  public
    constructor Create(ATypeInfo: PTypeInfo);

    function GetFields: TRttiFieldArray; override;
    function GetDeclaredFields: TRttiFieldArray; override;

    property ClassTypeInfo: TTypeInfoClass read GetClassTypeInfo;
    property MetaClassType: TClass read GetMetaClassType;
  end;

  { TRttiInterfaceType }

  TRttiInterfaceType = class(TRttiStructuredType)
  private
    function GetGUID: TGUID;
    function GetInterfaceTypeInfo: TTypeInfoInterface;
  protected
    function GetAncestor: TRttiStructuredType; override;
  public
    constructor Create(ATypeInfo: PTypeInfo);

    property GUID: TGUID read GetGUID;
    property InterfaceTypeInfo: TTypeInfoInterface read GetInterfaceTypeInfo;
  end;

  { TRttiRecordType }

  TRttiRecordType = class(TRttiStructuredType)
  private
    function GetRecordTypeInfo: TTypeInfoRecord;
  protected
    function GetIsRecord: Boolean; override;
  public
    constructor Create(ATypeInfo: PTypeInfo);

    property RecordTypeInfo: TTypeInfoRecord read GetRecordTypeInfo;
  end;

  { TRttiClassRefType }
  TRttiClassRefType = class(TRttiType)
  private
    function GetClassRefTypeInfo: TTypeInfoClassRef;
    function GetInstanceType: TRttiInstanceType;
    function GetMetaclassType: TClass;
  public
    constructor Create(ATypeInfo: PTypeInfo);

    property ClassRefTypeInfo: TTypeInfoClassRef read GetClassRefTypeInfo;
    property InstanceType: TRttiInstanceType read GetInstanceType;
    property MetaclassType: TClass read GetMetaclassType;
  end;

  { TRttiOrdinalType }

  TRttiOrdinalType = class(TRttiType)
  private
    function GetMaxValue: Integer; virtual;
    function GetMinValue: Integer; virtual;
    function GetOrdType: TOrdType;
    function GetOrdinalTypeInfo: TTypeInfoInteger;
  public
    constructor Create(ATypeInfo: PTypeInfo);

    property OrdType: TOrdType read GetOrdType;
    property MinValue: Integer read GetMinValue;
    property MaxValue: Integer read GetMaxValue;
    property OrdinalTypeInfo: TTypeInfoInteger read GetOrdinalTypeInfo;
  end;

  { TRttiEnumerationType }

  TRttiEnumerationType = class(TRttiOrdinalType)
  private
    function GetEnumerationTypeInfo: TTypeInfoEnum;
  public
    constructor Create(ATypeInfo: PTypeInfo);

    property EnumerationTypeInfo: TTypeInfoEnum read GetEnumerationTypeInfo;

    function GetNames: TStringArray;
    generic class function GetName<T>(AValue: T): String; reintroduce;
    generic class function GetValue<T>(const AValue: String): T;
  end;

  { TRttiDynamicArrayType }

  TRttiDynamicArrayType = class(TRttiType)
  private
    function GetDynArrayTypeInfo: TTypeInfoDynArray;
    function GetElementType: TRttiType;
  public
    constructor Create(ATypeInfo: PTypeInfo);

    property DynArrayTypeInfo: TTypeInfoDynArray read GetDynArrayTypeInfo;
    property ElementType: TRttiType read GetElementType;
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
    function QueryInterface(const iid: TGuid; out obj): Integer; override;
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
  pas: TJSObject; external name 'pas';

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

{ TRttiDynamicArrayType }

function TRttiDynamicArrayType.GetDynArrayTypeInfo: TTypeInfoDynArray;
begin
  Result := TTypeInfoDynArray(FTypeInfo);
end;

function TRttiDynamicArrayType.GetElementType: TRttiType;
begin
  Result := GRttiContext.GetType(DynArrayTypeInfo.ElType);
end;

constructor TRttiDynamicArrayType.Create(ATypeInfo: PTypeInfo);
begin
  if not (TTypeInfo(ATypeInfo) is TTypeInfoDynArray) then
    raise EInvalidCast.Create('');

  inherited Create(ATypeInfo);
end;

{ TRttiOrdinalType }

function TRttiOrdinalType.GetMaxValue: Integer;
begin
  Result := OrdinalTypeInfo.MaxValue;
end;

function TRttiOrdinalType.GetMinValue: Integer;
begin
  Result := OrdinalTypeInfo.MinValue;
end;

function TRttiOrdinalType.GetOrdType: TOrdType;
begin
  Result := OrdinalTypeInfo.OrdType;
end;

function TRttiOrdinalType.GetOrdinalTypeInfo: TTypeInfoInteger;
begin
  Result := TTypeInfoInteger(FTypeInfo);
end;

constructor TRttiOrdinalType.Create(ATypeInfo: PTypeInfo);
begin
  if not (TTypeInfo(ATypeInfo) is TTypeInfoInteger) then
    raise EInvalidCast.Create('');

  inherited Create(ATypeInfo);
end;

{ TRttiEnumerationType }

function TRttiEnumerationType.GetEnumerationTypeInfo: TTypeInfoEnum;
begin
  Result := TTypeInfoEnum(FTypeInfo);
end;

function TRttiEnumerationType.GetNames: TStringArray;
var
  A, NamesSize: Integer;

begin
  NamesSize := GetEnumNameCount(EnumerationTypeInfo);

  SetLength(Result, NamesSize);

  for A := 0 to Pred(NamesSize) do
    Result[A] := EnumerationTypeInfo.EnumType.IntToName[A + MinValue];
end;

generic class function TRttiEnumerationType.GetName<T>(AValue: T): String;

Var
  P : PTypeInfo;

begin
  P:=TypeInfo(T);
  if not (TTypeInfo(P).kind=tkEnumeration) then
    raise EInvalidCast.CreateFmt(SErrTypeIsNotEnumerated,[TTypeInfo(P).Name]);
  Result := GetEnumName(TTypeInfoEnum(P), Integer(JSValue(AValue)));
end;

generic class function TRttiEnumerationType.GetValue<T>(const AValue: String): T;

Var
  P : PTypeInfo;

begin
  P:=TypeInfo(T);
  if not (TTypeInfo(P).kind=tkEnumeration) then
    raise EInvalidCast.CreateFmt(SErrTypeIsNotEnumerated,[TTypeInfo(P).Name]);
  Result := T(JSValue(GetEnumValue(TTypeInfoEnum(TypeInfo(T)), AValue)));
end;

constructor TRttiEnumerationType.Create(ATypeInfo: PTypeInfo);
begin
  if not (TTypeInfo(ATypeInfo) is TTypeInfoEnum) then
    raise EInvalidCast.Create('');

  inherited Create(ATypeInfo);
end;

{ TValue }

function TValue.GetTypeKind: TTypeKind;
begin
  if TypeInfo=nil then
    Result:=tkUnknown
  else
    Result:=FTypeInfo.Kind;
end;

generic function TValue.AsType<T>: T;
begin
  Result := T(AsJSValue)
end;

generic class function TValue.From<T>(const Value: T): TValue;
begin
  Result := Make(System.TypeInfo(T), Value);
end;

class function TValue.Make(TypeInfo: TTypeInfo; const Value: JSValue): TValue;
begin
  Result.FData := Value;
  Result.FTypeInfo := TypeInfo;
end;

class function TValue.Make(const Value: TValue): TValue;
begin
  Result := TValue.Make(Value.TypeInfo, Value.AsJSValue);
end;

class function TValue.FromJSValue(v: JSValue): TValue;
var
  i: NativeInt;
  TypeOfValue: TTypeInfo;

begin
  case jsTypeOf(v) of
  'number':
    if JS.isInteger(v) then
      begin
      i:=NativeInt(v);
      if (i>=low(integer)) and (i<=high(integer)) then
        TypeOfValue:=system.TypeInfo(Integer)
      else
        TypeOfValue:=system.TypeInfo(NativeInt);
      end
    else
      TypeOfValue:=system.TypeInfo(Double);
  'string':  TypeOfValue:=system.TypeInfo(String);
  'boolean': TypeOfValue:=system.TypeInfo(Boolean);
  'object':
    begin
    if v=nil then
      TypeOfValue:=system.TypeInfo(Pointer)
    else if JS.isClass(v) and JS.isExt(v,TObject) then
      TypeOfValue:=system.TypeInfo(TClass(v))
    else if JS.isObject(v) and JS.isExt(v,TObject) then
      TypeOfValue:=system.TypeInfo(TObject(v))
    else
      TypeOfValue:=system.TypeInfo(Pointer);
    if (TypeOfValue=JS.Undefined) or (TypeOfValue=nil) then
      TypeOfValue:=system.TypeInfo(Pointer);
    end
  else
    TypeOfValue:=system.TypeInfo(JSValue);
  end;

  Result := Make(TypeOfValue, v);
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
  case Kind of
    tkDynArray: Exit(True);
    tkArray: Exit(Length(TTypeInfoStaticArray(FTypeInfo).Dims) = 1);
    else Result := False;
  end;
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
  if IsArray then
    Exit(Length(TJSValueDynArray(FData)));

  raise EInvalidCast.Create(SErrInvalidTypecast);
end;

function TValue.GetArrayElement(aIndex: SizeInt): TValue;
begin
  if IsArray then
  begin
    case Kind of
      tkArray: Result.FTypeInfo:=TTypeInfoStaticArray(FTypeInfo).ElType;
      tkDynArray: Result.FTypeInfo:=TTypeInfoDynArray(FTypeInfo).ElType;
    end;

    Result.FData:=TJSValueDynArray(FData)[aIndex];
  end
  else
    raise EInvalidCast.Create(SErrInvalidTypecast);
end;

procedure TValue.SetArrayLength(const Size: SizeInt);
var
  NewArray: TJSValueDynArray;

begin
  NewArray := TJSValueDynArray(FData);

  SetLength(NewArray, Size);

  FData := NewArray;
end;

procedure TValue.SetArrayElement(aIndex: SizeInt; const AValue: TValue);

begin
  if IsArray then
    TJSValueDynArray(FData)[aIndex] := AValue.AsJSValue
  else
    raise EInvalidCast.Create(SErrInvalidTypecast);
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

function TValue.AsJSValue: JSValue;
begin
  Result := FData;
end;

class function TValue.Empty: TValue;
begin
  Result.FData := nil;
  Result.FTypeInfo := nil;
end;

{ TRttiStructuredType }

function TRttiStructuredType.GetMethods: TRttiMethodArray;
var
  A, Start: Integer;

  BaseClass: TRttiStructuredType;

  Declared: TRttiMethodArray;

begin
  BaseClass := Self;
  Result := nil;
  while Assigned(BaseClass) do
  begin
    Declared := BaseClass.GetDeclaredMethods;
    Start := Length(Result);
    SetLength(Result, Start + Length(Declared));
    for A := Low(Declared) to High(Declared) do
      Result[Start + A] := Declared[A];
    BaseClass := BaseClass.GetAncestor;
  end;
end;

function TRttiStructuredType.GetMethods(const aName: String): TRttiMethodArray;
var
  Method: TRttiMethod;
  MethodCount: Integer;

begin
  MethodCount := 0;
  for Method in GetMethods do
    if aName = Method.Name then
      Inc(MethodCount);
  SetLength(Result, MethodCount);
  for Method in GetMethods do
    if aName = Method.Name then
    begin
      Dec(MethodCount);
      Result[MethodCount] := Method;
    end;
end;

function TRttiStructuredType.GetProperties: TRttiPropertyArray;
var
  A, Start: Integer;

  BaseClass: TRttiStructuredType;

  Declared: TRttiPropertyArray;

begin
  BaseClass := Self;
  Result := nil;

  while Assigned(BaseClass) do
  begin
    Declared := BaseClass.GetDeclaredProperties;
    Start := Length(Result);

    SetLength(Result, Start + Length(Declared));

    for A := Low(Declared) to High(Declared) do
      Result[Start + A] := Declared[A];

    BaseClass := BaseClass.GetAncestor;
  end;
end;

function TRttiStructuredType.GetMethod(const aName: String): TRttiMethod;
var
  Method: TRttiMethod;

begin
  for Method in GetMethods do
    if aName = Method.Name then
      Exit(Method);
end;

function TRttiStructuredType.GetProperty(const AName: string): TRttiProperty;
var
  Prop: TRttiProperty;

begin
  for Prop in GetProperties do
    if Prop.Name = AName then
      Exit(Prop);
end;

function TRttiStructuredType.GetDeclaredProperties: TRttiPropertyArray;
var
  A, PropCount: Integer;

begin
  if not Assigned(FProperties) then
  begin
    PropCount := StructTypeInfo.PropCount;

    SetLength(FProperties, PropCount);

    for A := 0 to Pred(PropCount) do
      FProperties[A] := TRttiProperty.Create(Self, StructTypeInfo.GetProp(A));
  end;

  Result := FProperties;
end;

function TRttiStructuredType.GetAncestor: TRttiStructuredType;
begin
  Result := nil;
end;

function TRttiStructuredType.GetStructTypeInfo: TTypeInfoStruct;
begin
  Result:=TTypeInfoStruct(FTypeInfo);
end;

constructor TRttiStructuredType.Create(ATypeInfo: PTypeInfo);
begin
  if not (TTypeInfo(ATypeInfo) is TTypeInfoStruct) then
    raise EInvalidCast.Create('');

  inherited Create(ATypeInfo);
end;

destructor TRttiStructuredType.Destroy;
var
  Method: TRttiMethod;

  Prop: TRttiProperty;

begin
  for Method in FMethods do
    Method.Free;

  for Prop in FProperties do
    Prop.Free;

  inherited Destroy;
end;

function TRttiStructuredType.GetDeclaredMethods: TRttiMethodArray;
var
  A, MethodCount: Integer;

begin
  if not Assigned(FMethods) then
  begin
    MethodCount := StructTypeInfo.MethodCount;
    SetLength(FMethods, MethodCount);

    for A := 0 to Pred(MethodCount) do
      FMethods[A] := TRttiMethod.Create(Self, StructTypeInfo.GetMethod(A));
  end;

  Result := FMethods;
end;

{ TRttiInstanceType }

function TRttiInstanceType.GetClassTypeInfo: TTypeInfoClass;
begin
  Result:=TTypeInfoClass(FTypeInfo);
end;

function TRttiInstanceType.GetMetaClassType: TClass;
begin
  Result:=ClassTypeInfo.ClassType;
end;

function TRttiInstanceType.GetAncestor: TRttiStructuredType;
begin
  Result := GRttiContext.GetType(ClassTypeInfo.Ancestor) as TRttiStructuredType;
end;

constructor TRttiInstanceType.Create(ATypeInfo: PTypeInfo);
begin
  if not (TTypeInfo(ATypeInfo) is TTypeInfoClass) then
    raise EInvalidCast.Create('');
  inherited Create(ATypeInfo);
end;

function TRttiInstanceType.GetDeclaredFields: TRttiFieldArray;
var
  A, FieldCount: Integer;

begin
  if not Assigned(FFields) then
  begin
    FieldCount := StructTypeInfo.FieldCount;

    SetLength(FFields, FieldCount);

    for A := 0 to Pred(FieldCount) do
      FFields[A] := TRttiField.Create(Self, StructTypeInfo.GetField(A));
  end;

  Result := FFields;
end;

function TRttiInstanceType.GetFields: TRttiFieldArray;
var
  A, Start: Integer;

  BaseClass: TRttiStructuredType;

  Declared: TRttiFieldArray;

begin
  BaseClass := Self;
  Result := nil;

  while Assigned(BaseClass) do
  begin
    Declared := BaseClass.GetDeclaredFields;
    Start := Length(Result);

    SetLength(Result, Start + Length(Declared));

    for A := Low(Declared) to High(Declared) do
      Result[Start + A] := Declared[A];

    BaseClass := BaseClass.GetAncestor;
  end;
end;

{ TRttiInterfaceType }

constructor TRttiInterfaceType.Create(ATypeInfo: PTypeInfo);
begin
  if not (TTypeInfo(ATypeInfo) is TTypeInfoInterface) then
    raise EInvalidCast.Create('');
  inherited Create(ATypeInfo);
end;

function TRttiInterfaceType.GetGUID: TGUID;
var
  Guid: String;

begin
  Guid := String(InterfaceTypeInfo.InterfaceType['$guid']);

  TryStringToGUID(Guid, Result);
end;

function TRttiInterfaceType.GetInterfaceTypeInfo: TTypeInfoInterface;
begin
  Result := TTypeInfoInterface(FTypeInfo);
end;

function TRttiInterfaceType.GetAncestor: TRttiStructuredType;
begin
  Result := GRttiContext.GetType(InterfaceTypeInfo.Ancestor) as TRttiStructuredType;
end;

{ TRttiRecordType }

function TRttiRecordType.GetRecordTypeInfo: TTypeInfoRecord;
begin
  Result := TTypeInfoRecord(FTypeInfo);
end;

function TRttiRecordType.GetIsRecord: Boolean;
begin
  Result := True;
end;

constructor TRttiRecordType.Create(ATypeInfo: PTypeInfo);
begin
  if not (TTypeInfo(ATypeInfo) is TTypeInfoClass) then
    raise EInvalidCast.Create('');
  inherited Create(ATypeInfo);
end;

{ TRttiClassRefType }

constructor TRttiClassRefType.Create(ATypeInfo: PTypeInfo);
begin
  if not (TTypeInfo(ATypeInfo) is TTypeInfoClassRef) then
    raise EInvalidCast.Create('');

  inherited Create(ATypeInfo);
end;

function TRttiClassRefType.GetClassRefTypeInfo: TTypeInfoClassRef;
begin
  Result := TTypeInfoClassRef(FTypeInfo);
end;

function TRttiClassRefType.GetInstanceType: TRttiInstanceType;
begin
  Result := GRttiContext.GetType(ClassRefTypeInfo.InstanceType) as TRttiInstanceType;
end;

function TRttiClassRefType.GetMetaclassType: TClass;
begin
  Result := InstanceType.MetaClassType;
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
  RttiTypeClass: array[TTypeKind] of TRttiTypeClass = (
    nil, // tkUnknown
    TRttiOrdinalType, // tkInteger
    TRttiOrdinalType, // tkChar
    TRttiType, // tkString
    TRttiEnumerationType, // tkEnumeration
    TRttiType, // tkSet
    TRttiOrdinalType, // tkDouble
    TRttiEnumerationType, // tkBool
    TRttiType, // tkProcVar
    nil, // tkMethod
    TRttiType, // tkArray
    TRttiDynamicArrayType, // tkDynArray
    TRttiRecordType, // tkRecord
    TRttiInstanceType, // tkClass
    TRttiClassRefType, // tkClassRef
    TRttiType, // tkPointer
    TRttiType, // tkJSValue
    TRttiType, // tkRefToProcVar
    TRttiInterfaceType, // tkInterface
    TRttiType, // tkHelper
    TRttiInstanceType // tkExtClass
  );
  t: TTypeinfo absolute aTypeInfo;
  Name: String;
begin
  if aTypeInfo=nil then exit(nil);
  Name:=t.Name;
  if isModule(t.Module) then
    Name:=t.Module.Name+'.'+Name;
  if GRttiContext.FPool.hasOwnProperty(Name) then
    Result:=TRttiType(GRttiContext.FPool[Name])
  else
    begin
    Result := RttiTypeClass[T.Kind].Create(aTypeInfo);

    GRttiContext.FPool[Name]:=Result;
    end;
end;

function TRTTIContext.GetType(aClass: TClass): TRTTIType;
begin
  if aClass=nil then exit(nil);
  Result:=GetType(TypeInfo(aClass));
end;

function TRTTIContext.FindType(const AQualifiedName: String): TRttiType;
var
  ModuleName, TypeName: String;

  Module: TTypeInfoModule;

  TypeFound: PTypeInfo;

begin
  Result := nil;

  for ModuleName in TJSObject.Keys(pas) do
    if AQualifiedName.StartsWith(ModuleName + '.') then
    begin
      Module := TTypeInfoModule(pas[ModuleName]);
      TypeName := Copy(AQualifiedName, Length(ModuleName) + 2, Length(AQualifiedName));

      if Module.RTTI.HasOwnProperty(TypeName) then
      begin
        TypeFound := PTypeInfo(Module.RTTI[TypeName]);

        Exit(GetType(TypeFound));
      end;
    end;
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
  if not (ATypeInfo is TTypeMember) then
    raise EInvalidCast.Create('');

  inherited Create();

  FParent := AParent;
  FTypeInfo:=ATypeInfo;
end;

function TRttiMember.GetAttributes: TCustomAttributeArray;
begin
  Result:=inherited GetAttributes;
end;

function TRttiMember.GetMemberTypeInfo: TTypeMember;
begin
  Result := TTypeMember(FTypeInfo);
end;

{ TRttiField }

constructor TRttiField.Create(AParent: TRttiType; ATypeInfo: TTypeMember);
begin
  if not (ATypeInfo is TTypeMemberField) then
    raise EInvalidCast.Create('');

  inherited;
end;

function TRttiField.GetFieldType: TRttiType;
begin
  Result := GRttiContext.GetType(FTypeInfo);
end;

function TRttiField.GetFieldTypeInfo: TTypeMemberField;
begin
  Result := TTypeMemberField(FTypeInfo);
end;

function TRttiField.GetValue(Instance: JSValue): TValue;
var
  JSInstance: TJSObject absolute Instance;

begin
  Result := TValue.FromJSValue(JSInstance[Name]);
end;

procedure TRttiField.SetValue(Instance: JSValue; const AValue: TValue);
var
  JSInstance: TJSObject absolute Instance;

begin
  JSInstance[Name] := AValue.AsJSValue;
end;

{ TRttiParameter }

function TRttiParameter.GetName: String;
begin
  Result := FName;
end;

{ TRttiMethod }

function TRttiMethod.GetMethodTypeInfo: TTypeMemberMethod;
begin
  Result := TTypeMemberMethod(FTypeInfo);
end;

function TRttiMethod.GetIsClassMethod: Boolean;
begin
  Result:=MethodTypeInfo.MethodKind in [mkClassFunction,mkClassProcedure];
end;

function TRttiMethod.GetIsConstructor: Boolean;
begin
  Result:=MethodTypeInfo.MethodKind=mkConstructor;
end;

function TRttiMethod.GetIsDestructor: Boolean;
begin
  Result:=MethodTypeInfo.MethodKind=mkDestructor;
end;

function TRttiMethod.GetIsExternal: Boolean;
begin
  Result := pfExternal in GetProcedureFlags;
end;

function TRttiMethod.GetIsStatic: Boolean;
begin
  Result := pfStatic in GetProcedureFlags;
end;

function TRttiMethod.GetIsVarArgs: Boolean;
begin
  Result := pfVarargs in GetProcedureFlags;
end;

function TRttiMethod.GetIsAsyncCall: Boolean;
begin
  Result := pfAsync in GetProcedureFlags;
end;

function TRttiMethod.GetIsSafeCall: Boolean;
begin
  Result := pfSafeCall in GetProcedureFlags;
end;

function TRttiMethod.GetMethodKind: TMethodKind;
begin
  Result:=MethodTypeInfo.MethodKind;;
end;

function TRttiMethod.GetProcedureFlags: TProcedureFlags;
const
  PROCEDURE_FLAGS: array[TProcedureFlag] of NativeInt = (1, 2, 4, 8, 16);

var
  Flag: TProcedureFlag;

  ProcedureFlags: NativeInt;

begin
  ProcedureFlags := MethodTypeInfo.ProcSig.Flags;
  Result := [];

  for Flag := Low(PROCEDURE_FLAGS) to High(PROCEDURE_FLAGS) do
    if PROCEDURE_FLAGS[Flag] and ProcedureFlags > 0 then
      Result := Result + [Flag];
end;

function TRttiMethod.GetReturnType: TRttiType;
begin
  Result := GRttiContext.GetType(MethodTypeInfo.ProcSig.ResultType);
end;

procedure TRttiMethod.LoadParameters;
const
  FLAGS_CONVERSION: array[TParamFlag] of NativeInt = (1, 2, 4, 8, 16, 32);

var
  A: Integer;

  Flag: TParamFlag;

  Param: TProcedureParam;

  RttiParam: TRttiParameter;

  MethodParams: TProcedureParams;

begin
  FParametersLoaded := True;
  MethodParams := MethodTypeInfo.ProcSig.Params;

  SetLength(FParameters, Length(MethodParams));

  for A := Low(FParameters) to High(FParameters) do
  begin
    Param := MethodParams[A];
    RttiParam := TRttiParameter.Create;
    RttiParam.FName := Param.Name;
    RttiParam.FParamType := GRttiContext.GetType(Param.TypeInfo);

    for Flag := Low(FLAGS_CONVERSION) to High(FLAGS_CONVERSION) do
      if FLAGS_CONVERSION[Flag] and Param.Flags > 0 then
        RttiParam.FFlags := RttiParam.FFlags + [Flag];

    FParameters[A] := RttiParam;
  end;
end;

function TRttiMethod.GetParameters: TRttiParameterArray;
begin
  if not FParametersLoaded then
    LoadParameters;

  Result := FParameters;
end;

{ TRttiProperty }

constructor TRttiProperty.Create(AParent: TRttiType; ATypeInfo: TTypeMember);
begin
  if not (ATypeInfo is TTypeMemberProperty) then
    raise EInvalidCast.Create('');

  inherited;
end;

function TRttiProperty.GetPropertyTypeInfo: TTypeMemberProperty;
begin
  Result := TTypeMemberProperty(FTypeInfo);
end;

function TRttiProperty.GetValue(Instance: JSValue): TValue;
var
  JSObject: TJSObject absolute Instance;

begin
  Result := TValue.Make(PropertyType.Handle, GetJSValueProp(JSObject, PropertyTypeInfo));
end;

procedure TRttiProperty.SetValue(Instance: JSValue; const AValue: TValue);
var
  JSObject: TJSObject absolute Instance;

begin
  SetJSValueProp(JSObject, PropertyTypeInfo, AValue.AsJSValue);
end;

procedure TRttiProperty.SetValue(Instance: JSValue; const AValue: JSValue);
var
  JSObject: TJSObject absolute Instance;

begin
  SetJSValueProp(JSObject, PropertyTypeInfo, AValue);
end;

procedure TRttiProperty.SetValue(Instance: TObject; const AValue: TValue);
begin
  SetValue(JSValue(Instance), AValue);
end;

function TRttiProperty.GetPropertyType: TRttiType;
begin
  Result := GRttiContext.GetType(PropertyTypeInfo.TypeInfo);
end;

function TRttiProperty.GetIsWritable: boolean;
begin
  Result := PropertyTypeInfo.Setter<>'';
end;

function TRttiProperty.GetIsReadable: boolean;
begin
  Result := PropertyTypeInfo.Getter<>'';
end;

function TRttiProperty.GetVisibility: TMemberVisibility;
begin
  // At this moment only published rtti-property-info is supported by pas2js
  Result := mvPublished;
end;

{ TRttiType }

function TRttiType.GetName: string;
begin
  Result:=FTypeInfo.Name;
end;

function TRttiType.GetIsInstance: boolean;
begin
  Result:=Self is TRttiInstanceType;
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

function TRttiType.GetAsInstance: TRttiInstanceType;
begin
  Result := Self as TRttiInstanceType;
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

function TRttiType.GetMethods: TRttiMethodArray;
begin
  Result:=nil;
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
var
  AField: TRttiField;

begin
  Result:=nil;
  for AField in GetFields do
    if AField.Name = AName then
      Exit(AField);
end;

function TRttiType.GetFields: TRttiFieldArray;
begin
  Result := nil;
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

function TVirtualInterface.QueryInterface(const iid: TGuid; out obj): Integer;
begin
  Result := inherited QueryInterface(iid, obj);
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

