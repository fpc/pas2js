{
    This file is part of the Pas2JS run time library.
    Copyright (c) 2017 by Mattias Gaertner

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
unit TypInfo;

{$mode objfpc}
{$modeswitch externalclass}

interface

uses
  SysUtils, Types, RTLConsts, JS;

type
  // if you change the following enumeration type in any way
  // you also have to change the rtl.js in an appropriate way !
  TTypeKind = (
    tkUnknown,  // 0
    tkInteger,  // 1
    tkChar,     // 2
    tkString,   // 3 in Delphi/FPC tkSString, tkWString or tkUString
    tkEnumeration, // 4
    tkSet,      // 5
    tkDouble,   // 6
    tkBool,     // 7
    tkProcVar,  // 8
    tkMethod,   // 9  proc var of object
    tkArray,    // 10 static array
    tkDynArray, // 11
    tkRecord,   // 12
    tkClass,    // 13
    tkClassRef, // 14
    tkPointer,  // 15
    tkJSValue,  // 16
    tkRefToProcVar  // 17
    //tkInterface,
    //tkObject,
    //tkSString,tkLString,tkAString,tkWString,
    //tkVariant,
    //tkWChar,
    //tkInt64,
    //tkQWord,
    //tkInterfaceRaw,
    //tkUString,tkUChar,
    //tkHelper,
    //tkFile,
    );
  TTypeKinds = set of TTypeKind;

const
  tkFloat = tkDouble; // for compatibility with Delphi/FPC
  tkProcedure = tkProcVar; // for compatibility with Delphi
  tkAny = [Low(TTypeKind)..High(TTypeKind)];
  tkMethods = [tkMethod];
  tkProperties = tkAny-tkMethods-[tkUnknown];

type
  { TTypeInfo }

  TTypeInfo = class external name 'rtl.tTypeInfo'
  public
    Name: String external name 'name';
    Kind: TTypeKind external name 'kind';
  end;
  TTypeInfoClassOf = class of TTypeInfo;

  TOrdType  = (
    otSByte,      // 0
    otUByte,      // 1
    otSWord,      // 2
    otUWord,      // 3
    otSLong,      // 4
    otULong,      // 5
    otSIntDouble, // 6 NativeInt
    otUIntDouble  // 7 NativeUInt
    );

  { TTypeInfoInteger - Kind = tkInteger }

  TTypeInfoInteger = class external name 'rtl.tTypeInfoInteger'(TTypeInfo)
  public
    MinValue: NativeInt external name 'minvalue';
    MaxValue: NativeInt external name 'maxvalue';
    OrdType : TOrdType external name 'ordtype';
  end;

  { TEnumType }

  TEnumType = class external name 'anonymous'
  private
    function GetIntToName(Index: NativeInt): String; external name '[]';
    function GetNameToInt(Name: String): NativeInt; external name '[]';
  public
    property IntToName[Index: NativeInt]: String read GetIntToName;
    property NameToInt[Name: String]: NativeInt read GetNameToInt;
  end;

  { TTypeInfoEnum - Kind = tkEnumeration }

  TTypeInfoEnum = class external name 'rtl.tTypeInfoEnum'(TTypeInfoInteger)
  public
    // not supported: OrdType : TOrdType, BaseType: TTypeInfo
    EnumType: TEnumType external name 'enumtype';
  end;

  { TTypeInfoSet - Kind = tkSet }

  TTypeInfoSet = class external name 'rtl.tTypeInfoSet'(TTypeInfo)
  public
    // not supported: OrdType : TOrdType, BaseType: TTypeInfo
    CompType: TTypeInfo external name 'comptype';
  end;

  { TTypeInfoStaticArray - Kind = tkArray }

  TTypeInfoStaticArray = class external name 'rtl.tTypeInfoStaticArray'(TTypeInfo)
  public
    Dims: TIntegerDynArray;
    ElType: TTypeInfo external name 'eltype';
  end;

  { TTypeInfoDynArray - Kind = tkDynArray }

  TTypeInfoDynArray = class external name 'rtl.tTypeInfoDynArray'(TTypeInfo)
  public
    DimCount: NativeInt external name 'dimcount';
    ElType: TTypeInfo external name 'eltype';
  end;

  TParamFlag     = (
    pfVar,     // 2^0 = 1
    pfConst,   // 2^1 = 2
    pfOut,     // 2^2 = 4
    pfArray    // 2^3 = 8
    //pfAddress,pfReference,
    );
  TParamFlags = set of TParamFlag;

  { TProcedureParam }

  TProcedureParam = class external name 'anonymous'
  public
    Name: String external name 'name';
    TypeInfo: TTypeInfo external name 'typeinfo';
    Flags: NativeInt external name 'flags'; // TParamFlags as bit vector
  end;

  TProcedureParams = array of TProcedureParam;

  TProcedureFlag = (
    pfStatic,   // 2^0 = 1
    pfVarargs,  // 2^1 = 2
    pfExternal  // 2^2 = 4  name may be an expression
    );
  TProcedureFlags = set of TProcedureFlag;

  { TProcedureSignature }

  TProcedureSignature = class external name 'anonymous'
  public
    Params: TProcedureParams external name 'params'; // can be null
    ResultType: TTypeInfo external name 'resulttype'; // can be null
    Flags: NativeInt external name 'flags'; // TProcedureFlags as bit vector
  end;

  { TTypeInfoProcVar - Kind = tkProcVar }

  TTypeInfoProcVar = class external name 'rtl.tTypeInfoProcVar'(TTypeInfo)
  public
    ProcSig: TProcedureSignature external name 'procsig';
  end;

  { TTypeInfoRefToProcVar - Kind = tkRefToProcVar }

  TTypeInfoRefToProcVar = class external name 'rtl.tTypeInfoRefToProcVar'(TTypeInfoProcVar)
  end;

  TMethodKind = (
    mkProcedure,     // 0  default
    mkFunction,      // 1
    mkConstructor,   // 2
    mkDestructor,    // 3
    mkClassProcedure,// 4
    mkClassFunction  // 5
    //mkClassConstructor,mkClassDestructor,mkOperatorOverload
    );
  TMethodKinds = set of TMethodKind;

  { TTypeInfoMethodVar - Kind = tkMethod }

  TTypeInfoMethodVar = class external name 'rtl.tTypeInfoMethodVar'(TTypeInfoProcVar)
  public
    MethodKind: TMethodKind external name 'methodkind';
  end;

  TTypeMemberKind = (
    tmkUnknown,  // 0
    tmkField,    // 1
    tmkMethod,   // 2
    tmkProperty  // 3
    );
  TTypeMemberKinds = set of TTypeMemberKind;

  { TTypeMember }

  TTypeMember = class external name 'rtl.tTypeMember'
  public
    Name: String external name 'name';
    Kind: TTypeMemberKind external name 'kind';
  end;
  TTypeMemberDynArray = array of TTypeMember;

  { TTypeMemberField - Kind = tmkField }

  TTypeMemberField = class external name 'rtl.tTypeMemberField'(TTypeMember)
  public
    TypeInfo: TTypeInfo external name 'typeinfo';
  end;

  { TTypeMemberMethod - Kind = tmkMethod }

  TTypeMemberMethod = class external name 'rtl.tTypeMemberMethod'(TTypeMember)
  public
    MethodKind: TMethodKind external name 'methodkind';
    ProcSig: TProcedureSignature external name 'procsig';
  end;
  TTypeMemberMethodDynArray = array of TTypeMemberMethod;

const
  pfGetFunction = 1; // getter is a function
  pfSetProcedure = 2; // setter is a procedure
  // stored is a 2-bit vector:
  pfStoredFalse = 4; // stored false, never
  pfStoredField = 8; // stored field, field name is in Stored
  pfStoredFunction = 12; // stored function, function name is in Stored
  pfHasIndex = 16; { if getter is function, append Index as last param
                     if setter is function, append Index as second last param }
type
  { TTypeMemberProperty - Kind = tmkProperty }

  TTypeMemberProperty = class external name 'rtl.tTypeMemberProperty'(TTypeMember)
  public
    TypeInfo: TTypeInfo external name 'typeinfo';
    Flags: NativeInt external name 'flags'; // bit vector, see pf constants above
    Params: TProcedureParams external name 'params'; // can be null
    Index: JSValue external name 'index'; // can be undefined
    Getter: String external name 'getter'; // name of field or function
    Setter: String external name 'setter'; // name of field or function
    Stored: String external name 'stored'; // name of field or function, can be undefined
    Default: JSValue external name 'Default'; // can be undefined
  end;
  TTypeMemberPropertyDynArray = array of TTypeMemberProperty;

  { TTypeMembers }

  TTypeMembers = class external name 'rtl.tTypeMembers'
  private
    function GetItems(Name: String): TTypeMember; external name '[]';
    procedure SetItems(Name: String; const AValue: TTypeMember); external name '[]';
  public
    property Members[Name: String]: TTypeMember read GetItems write SetItems; default;
  end;

  { TTypeInfoStruct }

  TTypeInfoStruct = class external name 'rtl.tTypeInfoStruct'(TTypeInfo)
  private
    FFieldCount: NativeInt external name 'fields.length';
    FMethodCount: NativeInt external name 'methods.length';
    FPropCount: NativeInt external name 'properties.length';
  public
    Members: TTypeMembers external name 'members';
    Names: TStringDynArray external name 'names'; // all member names with TTypeInfo
    Fields: TStringDynArray external name 'fields';
    Methods: TStringDynArray external name 'methods';
    Properties: TStringDynArray external name 'properties';
    property FieldCount: NativeInt read FFieldCount;
    function GetField(Index: NativeInt): TTypeMemberField; external name 'getField';
    function AddField(aName: String; aType: TTypeInfo; Options: TJSObject = nil
      ): TTypeMemberField; external name 'addField';
    property MethodCount: NativeInt read FMethodCount;
    function GetMethod(Index: NativeInt): TTypeMemberMethod; external name 'getMethod';
    function AddMethod(aName: String; MethodKind: TMethodKind = mkProcedure;
      Params: TJSArray = nil; ResultType: TTypeInfo = nil;
      Options: TJSObject = nil): TTypeMemberMethod; external name 'addMethod';
    property PropCount: NativeInt read FPropCount;
    function GetProp(Index: NativeInt): TTypeMemberProperty; external name 'getProperty';
    function AddProperty(aName: String; Flags: NativeInt; ResultType: TTypeInfo;
      Getter, Setter: String; Options: TJSObject = nil): TTypeMemberProperty; external name 'addProperty';
  end;

  { TTypeInfoRecord - Kind = tkRecord }

  TTypeInfoRecord = class external name 'rtl.tTypeInfoRecord'(TTypeInfoStruct)
  public
    RecordType: TJSObject external name 'record';
  end;

  { TTypeInfoClass - Kind = tkClass }

  TTypeInfoClass = class external name 'rtl.tTypeInfoClass'(TTypeInfoStruct)
  public
    ClassType: TClass external name 'class';
    Ancestor: TTypeInfoClass external name 'ancestor';
  end;

  { TTypeInfoClassRef - class-of, Kind = tkClassRef }

  TTypeInfoClassRef = class external name 'rtl.tTypeInfoClassRef'(TTypeInfo)
  public
    InstanceType: TTypeInfo external name 'instancetype';
  end;

  { TTypeInfoPointer - Kind = tkPointer }

  TTypeInfoPointer = class external name 'rtl.tTypeInfoPointer'(TTypeInfo)
  public
    RefType: TTypeInfo external name 'reftype'; // can be null
  end;

  EPropertyError  = class(Exception);

function GetClassMembers(aTIClass: TTypeInfoClass): TTypeMemberDynArray;
function GetClassMember(aTIClass: TTypeInfoClass; const aName: String): TTypeMember;
function GetInstanceMethod(Instance: TObject; const aName: String): Pointer;
function GetClassMethods(aTIClass: TTypeInfoClass): TTypeMemberMethodDynArray;
function CreateMethod(Instance: TObject; FuncName: String): Pointer; external name 'rtl.createCallback';

function GetPropInfos(aTIClass: TTypeInfoClass): TTypeMemberPropertyDynArray;
function GetPropInfo(TI: TTypeInfoClass; const PropName: String): TTypeMemberProperty;
function GetPropInfo(TI: TTypeInfoClass; const PropName: String; const Kinds: TTypeKinds): TTypeMemberProperty;
function GetPropInfo(Instance: TObject; const PropName: String): TTypeMemberProperty;
function GetPropInfo(Instance: TObject; const PropName: String; const Kinds: TTypeKinds): TTypeMemberProperty;
function GetPropInfo(aClass: TClass; const PropName: String): TTypeMemberProperty;
function GetPropInfo(aClass: TClass; const PropName: String; const Kinds: TTypeKinds): TTypeMemberProperty;

function FindPropInfo(Instance: TObject; const PropName: String): TTypeMemberProperty;
function FindPropInfo(Instance: TObject; const PropName: String; const Kinds: TTypeKinds): TTypeMemberProperty;
function FindPropInfo(aClass: TClass; const PropName: String): TTypeMemberProperty;
function FindPropInfo(aClass: TClass; const PropName: String; const Kinds: TTypeKinds): TTypeMemberProperty;

// Property information routines.
Function IsStoredProp(Instance: TObject; const PropInfo: TTypeMemberProperty): Boolean;
Function IsStoredProp(Instance: TObject; const PropName: string): Boolean;
function IsPublishedProp(Instance: TObject; const PropName: String): Boolean;
function IsPublishedProp(aClass: TClass; const PropName: String): Boolean;
function PropType(Instance: TObject; const PropName: string): TTypeKind;
function PropType(aClass: TClass; const PropName: string): TTypeKind;
function PropIsType(Instance: TObject; const PropName: string; const TypeKind: TTypeKind): Boolean;
function PropIsType(aClass: TClass; const PropName: string; const TypeKind: TTypeKind): Boolean;

function GetJSValueProp(Instance: TObject; const PropName: String): JSValue;
function GetJSValueProp(Instance: TObject; const PropInfo: TTypeMemberProperty): JSValue;
procedure SetJSValueProp(Instance: TObject; const PropName: String; Value: JSValue);
procedure SetJSValueProp(Instance: TObject; const PropInfo: TTypeMemberProperty; Value: JSValue);

function GetNativeIntProp(Instance: TObject; const PropName: String): NativeInt;
function GetNativeIntProp(Instance: TObject; const PropInfo: TTypeMemberProperty): NativeInt;
procedure SetNativeIntProp(Instance: TObject; const PropName: String; Value: NativeInt);
procedure SetNativeIntProp(Instance: TObject; const PropInfo: TTypeMemberProperty; Value: NativeInt);

function GetStringProp(Instance: TObject; const PropName: String): String;
function GetStringProp(Instance: TObject; const PropInfo: TTypeMemberProperty): String;
procedure SetStringProp(Instance: TObject; const PropName: String; Value: String);
procedure SetStringProp(Instance: TObject; const PropInfo: TTypeMemberProperty; Value: String);

function GetBoolProp(Instance: TObject; const PropName: String): boolean;
function GetBoolProp(Instance: TObject; const PropInfo: TTypeMemberProperty): boolean;
procedure SetBoolProp(Instance: TObject; const PropName: String; Value: boolean);
procedure SetBoolProp(Instance: TObject; const PropInfo: TTypeMemberProperty; Value: boolean);

function GetObjectProp(Instance: TObject; const PropName: String): TObject;
function GetObjectProp(Instance: TObject; const PropName: String; MinClass : TClass): TObject;
function GetObjectProp(Instance: TObject; const PropInfo: TTypeMemberProperty):  TObject;
function GetObjectProp(Instance: TObject; const PropInfo: TTypeMemberProperty; MinClass : TClass):  TObject;
procedure SetObjectProp(Instance: TObject; const PropName: String; Value: TObject) ;
procedure SetObjectProp(Instance: TObject; const PropInfo: TTypeMemberProperty; Value: TObject);

Function  GetFloatProp(Instance: TObject; const PropName: string): Double;
Function  GetFloatProp(Instance: TObject; PropInfo : TTypeMemberProperty) : Double;
Procedure SetFloatProp(Instance: TObject; const PropName: string; Value: Double);
Procedure SetFloatProp(Instance: TObject; PropInfo : TTypeMemberProperty;  Value : Double);

implementation

function GetClassMembers(aTIClass: TTypeInfoClass): TTypeMemberDynArray;
var
  C: TTypeInfoClass;
  i, Cnt, j: Integer;
begin
  Cnt:=0;
  C:=aTIClass;
  while C<>nil do
  begin
    inc(Cnt,length(C.Names));
    C:=C.Ancestor;
  end;
  SetLength(Result,Cnt);
  C:=aTIClass;
  i:=0;
  while C<>nil do
  begin
    for j:=0 to length(C.Names)-1 do
    begin
      Result[i]:=C.Members[C.Names[j]];
      inc(i);
    end;
    C:=C.Ancestor;
  end;
end;

function GetClassMember(aTIClass: TTypeInfoClass; const aName: String): TTypeMember;
var
  C: TTypeInfoClass;
  i: Integer;
begin
  // quick search: case sensitive
  C:=aTIClass;
  while C<>nil do
  begin
    if TJSObject(C.Members).hasOwnProperty(aName) then
      exit(C.Members[aName]);
    C:=C.Ancestor;
  end;
  // slow search: case insensitive
  C:=aTIClass;
  while C<>nil do
  begin
    for i:=0 to length(C.Names)-1 do
      if CompareText(C.Names[i],aName)=0 then
        exit(C.Members[C.Names[i]]);
    C:=C.Ancestor;
  end;
  Result:=nil;
end;

function GetInstanceMethod(Instance: TObject; const aName: String): Pointer;
var
  TI: TTypeMember;
begin
  if Instance=nil then exit(nil);
  TI:=GetClassMember(TypeInfo(Instance),aName);
  if not (TI is TTypeMemberMethod) then exit(nil);
  Result:=CreateMethod(Instance,TI.Name); // Note: use TI.Name for the correct case!
end;

function GetClassMethods(aTIClass: TTypeInfoClass): TTypeMemberMethodDynArray;
var
  C: TTypeInfoClass;
  i, Cnt, j: Integer;
begin
  Cnt:=0;
  C:=aTIClass;
  while C<>nil do
  begin
    inc(Cnt,C.MethodCount);
    C:=C.Ancestor;
  end;
  SetLength(Result,Cnt);
  C:=aTIClass;
  i:=0;
  while C<>nil do
  begin
    for j:=0 to C.MethodCount-1 do
    begin
      Result[i]:=TTypeMemberMethod(C.Members[C.Methods[j]]);
      inc(i);
    end;
    C:=C.Ancestor;
  end;
end;

function GetPropInfos(aTIClass: TTypeInfoClass): TTypeMemberPropertyDynArray;
var
  C: TTypeInfoClass;
  i, Cnt, j: Integer;
begin
  Cnt:=0;
  C:=aTIClass;
  while C<>nil do
  begin
    inc(Cnt,C.PropCount);
    C:=C.Ancestor;
  end;
  SetLength(Result,Cnt);
  C:=aTIClass;
  i:=0;
  while C<>nil do
  begin
    for j:=0 to C.PropCount-1 do
    begin
      Result[i]:=TTypeMemberProperty(C.Members[C.Properties[j]]);
      inc(i);
    end;
    C:=C.Ancestor;
  end;
end;

function GetPropInfo(TI: TTypeInfoClass; const PropName: String
  ): TTypeMemberProperty;
var
  m: TTypeMember;
  i: Integer;
  C: TTypeInfoClass;
begin
  // quick search case sensitive
  C:=TI;
  while C<>nil do
  begin
    m:=C.Members[PropName];
    if m is TTypeMemberProperty then
      exit(TTypeMemberProperty(m));
    C:=C.Ancestor;
  end;

  // slow search case insensitive
  Result:=nil;
  repeat
    for i:=0 to TI.PropCount-1 do
      if CompareText(PropName,TI.Properties[i])=0 then
      begin
        m:=TI.Members[TI.Properties[i]];
        if m is TTypeMemberProperty then
          Result:=TTypeMemberProperty(m);
        exit;
      end;
    TI:=TI.Ancestor;
  until TI=nil;
end;

function GetPropInfo(TI: TTypeInfoClass; const PropName: String;
  const Kinds: TTypeKinds): TTypeMemberProperty;
begin
  Result:=GetPropInfo(TI,PropName);
  if (Kinds<>[]) and (Result<>nil) and not (Result.TypeInfo.Kind in Kinds) then
    Result:=nil;
end;

function GetPropInfo(Instance: TObject; const PropName: String
  ): TTypeMemberProperty;
begin
  Result:=GetPropInfo(TypeInfo(Instance),PropName,[]);
end;

function GetPropInfo(Instance: TObject; const PropName: String;
  const Kinds: TTypeKinds): TTypeMemberProperty;
begin
  Result:=GetPropInfo(TypeInfo(Instance),PropName,Kinds);
end;

function GetPropInfo(aClass: TClass; const PropName: String
  ): TTypeMemberProperty;
begin
  Result:=GetPropInfo(TypeInfo(AClass),PropName,[]);
end;

function GetPropInfo(aClass: TClass; const PropName: String;
  const Kinds: TTypeKinds): TTypeMemberProperty;
begin
  Result:=GetPropInfo(TypeInfo(AClass),PropName,Kinds);
end;

function FindPropInfo(Instance: TObject; const PropName: String
  ): TTypeMemberProperty;
begin
  Result:=GetPropInfo(TypeInfo(Instance), PropName);
  if Result=nil then
    raise EPropertyError.CreateFmt(SErrPropertyNotFound, [PropName]);
end;

function FindPropInfo(Instance: TObject; const PropName: String;
  const Kinds: TTypeKinds): TTypeMemberProperty;
begin
  Result:=GetPropInfo(TypeInfo(Instance), PropName, Kinds);
  if Result=nil then
    raise EPropertyError.CreateFmt(SErrPropertyNotFound, [PropName]);
end;

function FindPropInfo(aClass: TClass; const PropName: String
  ): TTypeMemberProperty;
begin
  Result:=GetPropInfo(TypeInfo(aClass), PropName);
  if Result=nil then
    raise EPropertyError.CreateFmt(SErrPropertyNotFound, [PropName]);
end;

function FindPropInfo(aClass: TClass; const PropName: String;
  const Kinds: TTypeKinds): TTypeMemberProperty;
begin
  Result:=GetPropInfo(TypeInfo(aClass), PropName, Kinds);
  if Result=nil then
    raise EPropertyError.CreateFmt(SErrPropertyNotFound, [PropName]);
end;

function IsStoredProp(Instance: TObject; const PropInfo: TTypeMemberProperty
  ): Boolean;
type
  TIsStored = function: Boolean of object;
begin
  case PropInfo.Flags and 12 of
  0: Result:=true;
  4: Result:=false;
  8: Result:=Boolean(TJSObject(Instance)[PropInfo.Stored]);
  else Result:=TIsStored(TJSObject(Instance)[PropInfo.Stored])();
  end;
end;

function IsStoredProp(Instance: TObject; const PropName: string): Boolean;
begin
  Result:=IsStoredProp(Instance,FindPropInfo(Instance,PropName));
end;

function IsPublishedProp(Instance: TObject; const PropName: String): Boolean;
begin
  Result:=GetPropInfo(Instance,PropName)<>nil;
end;

function IsPublishedProp(aClass: TClass; const PropName: String): Boolean;
begin
  Result:=GetPropInfo(aClass,PropName)<>nil;
end;

function PropType(Instance: TObject; const PropName: string): TTypeKind;
begin
  Result:=FindPropInfo(Instance,PropName).TypeInfo.Kind;
end;

function PropType(aClass: TClass; const PropName: string): TTypeKind;
begin
  Result:=FindPropInfo(aClass,PropName).TypeInfo.Kind;
end;

function PropIsType(Instance: TObject; const PropName: string;
  const TypeKind: TTypeKind): Boolean;
begin
  Result:=PropType(Instance,PropName)=TypeKind;
end;

function PropIsType(aClass: TClass; const PropName: string;
  const TypeKind: TTypeKind): Boolean;
begin
  Result:=PropType(aClass,PropName)=TypeKind;
end;

type
  TGetterKind = (
    gkNone,
    gkField,
    gkFunction,
    gkFunctionWithParams
  );

function GetPropGetterKind(const PropInfo: TTypeMemberProperty): TGetterKind;
begin
  if PropInfo.Getter='' then
    Result:=gkNone
  else if (pfGetFunction and PropInfo.Flags)>0 then
    begin
    if length(PropInfo.Params)>0 then
      Result:=gkFunctionWithParams
    else
      Result:=gkFunction;
    end
  else
    Result:=gkField;
end;

type
  TSetterKind = (
    skNone,
    skField,
    skProcedure,
    skProcedureWithParams
  );

function GetPropSetterKind(const PropInfo: TTypeMemberProperty): TSetterKind;
begin
  if PropInfo.Setter='' then
    Result:=skNone
  else if (pfSetProcedure and PropInfo.Flags)>0 then
    begin
    if length(PropInfo.Params)>0 then
      Result:=skProcedureWithParams
    else
      Result:=skProcedure;
    end
  else
    Result:=skField;
end;

function GetJSValueProp(Instance: TObject; const PropName: String): JSValue;
begin
  Result:=GetJSValueProp(Instance,FindPropInfo(Instance,PropName));
end;

function GetJSValueProp(Instance: TObject; const PropInfo: TTypeMemberProperty
  ): JSValue;
type
  TGetter = function: JSValue of object;
  TGetterWithIndex = function(Index: JSValue): JSValue of object;
var
  gk: TGetterKind;
begin
  gk:=GetPropGetterKind(PropInfo);
  case gk of
    gkNone:
      raise EPropertyError.CreateFmt(SCantReadPropertyS, [PropInfo.Name]);
    gkField:
      Result:=TJSObject(Instance)[PropInfo.Getter];
    gkFunction:
      if (pfHasIndex and PropInfo.Flags)>0 then
        Result:=TGetterWithIndex(TJSObject(Instance)[PropInfo.Getter])(PropInfo.Index)
      else
        Result:=TGetter(TJSObject(Instance)[PropInfo.Getter])();
    gkFunctionWithParams:
      raise EPropertyError.CreateFmt(SIndexedPropertyNeedsParams, [PropInfo.Name]);
  end;
end;

procedure SetJSValueProp(Instance: TObject; const PropName: String;
  Value: JSValue);
begin
  SetJSValueProp(Instance,FindPropInfo(Instance,PropName),Value);
end;

procedure SetJSValueProp(Instance: TObject;
  const PropInfo: TTypeMemberProperty; Value: JSValue);
type
  TSetter = procedure(Value: JSValue) of object;
  TSetterWithIndex = procedure(Index, Value: JSValue) of object;
var
  sk: TSetterKind;
begin
  sk:=GetPropSetterKind(PropInfo);
  case sk of
    skNone:
      raise EPropertyError.CreateFmt(SCantWritePropertyS, [PropInfo.Name]);
    skField:
      TJSObject(Instance)[PropInfo.Setter]:=Value;
    skProcedure:
      if (pfHasIndex and PropInfo.Flags)>0 then
        TSetterWithIndex(TJSObject(Instance)[PropInfo.Setter])(PropInfo.Index,Value)
      else
        TSetter(TJSObject(Instance)[PropInfo.Setter])(Value);
    skProcedureWithParams:
      raise EPropertyError.CreateFmt(SIndexedPropertyNeedsParams, [PropInfo.Name]);
  end;
end;

function GetNativeIntProp(Instance: TObject; const PropName: String): NativeInt;
begin
  Result:=GetNativeIntProp(Instance,FindPropInfo(Instance,PropName));
end;

function GetNativeIntProp(Instance: TObject; const PropInfo: TTypeMemberProperty
  ): NativeInt;
begin
  Result:=NativeInt(GetJSValueProp(Instance,PropInfo));
end;

procedure SetNativeIntProp(Instance: TObject; const PropName: String;
  Value: NativeInt);
begin
  SetJSValueProp(Instance,FindPropInfo(Instance,PropName),Value);
end;

procedure SetNativeIntProp(Instance: TObject;
  const PropInfo: TTypeMemberProperty; Value: NativeInt);
begin
  SetJSValueProp(Instance,PropInfo,Value);
end;

function GetStringProp(Instance: TObject; const PropName: String): String;
begin
  Result:=GetStringProp(Instance,FindPropInfo(Instance,PropName));
end;

function GetStringProp(Instance: TObject; const PropInfo: TTypeMemberProperty
  ): String;
begin
  Result:=String(GetJSValueProp(Instance,PropInfo));
end;

procedure SetStringProp(Instance: TObject; const PropName: String; Value: String
  );
begin
  SetStringProp(Instance,FindPropInfo(Instance,PropName),Value);
end;

procedure SetStringProp(Instance: TObject; const PropInfo: TTypeMemberProperty;
  Value: String);
begin
  SetJSValueProp(Instance,PropInfo,Value);
end;

function GetBoolProp(Instance: TObject; const PropName: String): boolean;
begin
  Result:=GetBoolProp(Instance,FindPropInfo(Instance,PropName));
end;

function GetBoolProp(Instance: TObject; const PropInfo: TTypeMemberProperty
  ): boolean;
begin
  Result:=Boolean(GetJSValueProp(Instance,PropInfo));
end;

procedure SetBoolProp(Instance: TObject; const PropName: String; Value: boolean
  );
begin
  SetBoolProp(Instance,FindPropInfo(Instance,PropName),Value);
end;

procedure SetBoolProp(Instance: TObject; const PropInfo: TTypeMemberProperty;
  Value: boolean);
begin
  SetJSValueProp(Instance,PropInfo,Value);
end;

function GetObjectProp(Instance: TObject; const PropName: String): TObject;
begin
  Result:=GetObjectProp(Instance,FindPropInfo(Instance,PropName));
end;

function GetObjectProp(Instance: TObject; const PropName: String; MinClass : TClass): TObject;
begin
  Result:=GetObjectProp(Instance,FindPropInfo(Instance,PropName));
  if (MinClass<>Nil) and (Result<>Nil) Then
    if not Result.InheritsFrom(MinClass) then
      Result:=Nil;
end;

function GetObjectProp(Instance: TObject; const PropInfo: TTypeMemberProperty):  TObject;

begin
  Result:=GetObjectProp(Instance,PropInfo,Nil);
end;

function GetObjectProp(Instance: TObject; const PropInfo: TTypeMemberProperty; MinClass : TClass):  TObject;

Var
  O : TObject;

begin
  O:=TObject(GetJSValueProp(Instance,PropInfo));
  if (MinClass<>Nil) and not O.InheritsFrom(MinClass) then
    Result:=Nil
  else
    Result:=O;
end;

procedure SetObjectProp(Instance: TObject; const PropName: String; Value: TObject) ;

begin
  SetObjectProp(Instance,FindPropInfo(Instance,PropName),Value);
end;

procedure SetObjectProp(Instance: TObject; const PropInfo: TTypeMemberProperty; Value: TObject);

begin
  SetJSValueProp(Instance,PropInfo,Value);
end;

Function  GetFloatProp(Instance: TObject; PropInfo : TTypeMemberProperty) : Double;
begin
  Result:=Double(GetJSValueProp(Instance,PropInfo));
end;

Function  GetFloatProp(Instance: TObject; const PropName: string): Double;

begin
  Result:=GetFloatProp(Instance,FindPropInfo(Instance,PropName));
end;

Procedure SetFloatProp(Instance: TObject; const PropName: string; Value: Double);

begin
  SetFloatProp(Instance,FindPropInfo(Instance,PropName),Value);
end;

Procedure SetFloatProp(Instance: TObject; PropInfo : TTypeMemberProperty; Value : Double);

begin
  SetJSValueProp(Instance,PropInfo,Value);
end;

end.

