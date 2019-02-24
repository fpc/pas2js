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
  Types, TypInfo, JS;

resourcestring
  SErrInvokeInvalidCodeAddr = 'CodeAddress is not a function';

type
  // will be changed to 'record' and improved as soon as the
  // operator overloading is implemented
  TValue = JSValue;

  TRttiType = class;

  { TRTTIContext }

  TRTTIContext = record
  private
    FPool: TJSObject; // maps 'modulename.typename' to TRTTIType
  public
    class function Create: TRTTIContext; static;
    procedure Free;

    function GetType(aTypeInfo: PTypeInfo): TRTTIType; overload;
    function GetType(aClass: TClass): TRTTIType; overload;
  end;

  { TRttiObject }

  TRttiObject = class abstract
  protected
    //function GetHandle: Pointer; virtual; abstract;
  public
    //property Handle: Pointer read GetHandle;
    function GetAttributes: TCustomAttributeArray; virtual;
  end;

  { TRttiNamedObject }

  TRttiNamedObject = class(TRttiObject)
  protected
    function GetName: string; virtual;
  public
    property Name: string read GetName;
  end;

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
    //function GetProperties: specialize TArray<TRttiProperty>; virtual;
    //function GetProperty(const AName: string): TRttiProperty; virtual;
    //function GetMethods: specialize TArray<TRttiMethod>; virtual;
    //function GetMethod(const aName: String): TRttiMethod; virtual;
    //function GetDeclaredMethods: specialize TArray<TRttiMethod>; virtual;
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

{ TRTTIContext }

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

function TRTTIContext.GetType(aTypeInfo: Pointer): TRTTIType;
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
  if AResultType=nil then ;
  if AIsStatic then ;
  if AIsConstructor then
    raise EInvoke.Create('not supported');
  if isFunction(ACodeAddress) then
    Result := TJSFunction(ACodeAddress).apply(nil, AArgs)
  else
    raise EInvoke.Create(SErrInvokeInvalidCodeAddr);
end;

end.

