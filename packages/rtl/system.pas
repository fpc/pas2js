{
    This file is part of the Pas2JS run time library.
    Copyright (c) 2017 by Mattias Gaertner

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
unit System;

{$mode objfpc}
{$modeswitch externalclass}

interface

const
  LineEnding = #10;
  sLineBreak = LineEnding;

  MaxSmallint = 32767;
  MinSmallint = -32768;
  MaxShortInt = 127;
  MinShortInt = -128;
  MaxByte = $FF;
  MaxWord = $FFFF;
  MaxLongint  = $7fffffff;
  MaxCardinal = LongWord($ffffffff);

  Maxint = MaxLongint;
  IsMultiThread = false;
{*****************************************************************************
                               Base types
*****************************************************************************}
type
  Integer = LongInt;
  Cardinal = LongWord;
  DWord = LongWord;
  SizeInt = NativeInt;
  SizeUInt = NativeUInt;
  PtrInt = NativeInt;
  PtrUInt = NativeUInt;
  ValSInt = NativeInt;
  ValUInt = NativeUInt;
  ValReal = Double;
  Real = Double;
  Extended = Double;

  Int64 = NativeInt unimplemented; // only 53 bits at runtime
  UInt64 = NativeUInt unimplemented; // only 52 bits at runtime
  QWord = NativeUInt unimplemented; // only 52 bits at runtime
  Single = Double unimplemented;
  Comp = NativeInt unimplemented;
  NativeLargeInt = NativeInt;
  NativeLargeUInt = NativeUInt;

  UnicodeString = String;
  WideString = String;
  WideChar = char;

  TDynArrayIndex = NativeInt;
  TTextLineBreakStyle = (tlbsLF,tlbsCRLF,tlbsCR);

{*****************************************************************************
                            TObject, TClass
*****************************************************************************}
type
  TClass = class of TObject;

  { TObject }

  TObject = class
  private
    class var FClassName: String; external name '$classname';
    class var FClassParent: TClass; external name '$ancestor';
    class var FUnitName: String; external name '$module.$name';
  public
    constructor Create;
    destructor Destroy; virtual;

    // Free is using compiler magic.
    // Reasons:
    // 1. In JS calling obj.Free when obj=nil crashes.
    // 2. In JS freeing memory requires to set all references to nil.
    // Therefore any obj.free call is replaced by the compiler with some rtl magic.
    procedure Free;

    class function ClassType: TClass; assembler;
    class property ClassName: String read FClassName;
    class function ClassNameIs(const Name: string): boolean;
    class property ClassParent: TClass read FClassParent;
    class function InheritsFrom(aClass: TClass): boolean; assembler;
    class property UnitName: String read FUnitName;

    procedure AfterConstruction; virtual;
    procedure BeforeDestruction; virtual;

    function Equals(Obj: TObject): boolean; virtual;
    function ToString: String; virtual;
  end;

Const
  DefaultTextLineBreakStyle : TTextLineBreakStyle = tlbsLF;

{*****************************************************************************
                            Init / Exit / ExitProc
*****************************************************************************}
var
  ExitCode: Integer; external name 'rtl.exitcode';
  IsConsole: Boolean = {$IFDEF NodeJS}true{$ELSE}false{$ENDIF};

type
  TOnParamCount = function: Longint;
  TOnParamStr = function(Index: Longint): String;
var
  OnParamCount: TOnParamCount;
  OnParamStr: TOnParamStr;

function ParamCount: Longint;
function ParamStr(Index: Longint): String;

{*****************************************************************************
                                 Math
*****************************************************************************}
var // ToDo: make these const
  PI: Double; external name 'Math.PI';
  MathE: Double; external name 'Math.E'; // Euler's number
  MathLN10: Double; external name 'Math.LN10'; // ln(10)
  MathLN2: Double; external name 'Math.LN2'; // ln(2)
  MathLog10E: Double; external name 'Math.Log10E'; // log10(e)
  MathLog2E: Double; external name 'Math.LOG2E'; // log2(e)
  MathSQRT1_2: Double; external name 'Math.SQRT1_2'; // sqrt(0.5)
  MathSQRT2: Double; external name 'Math.SQRT2'; // sqrt(2)

function Abs(const A: integer): integer; overload; external name 'Math.abs';
function Abs(const A: NativeInt): integer; overload; external name 'Math.abs';
function Abs(const A: Double): Double; overload; external name 'Math.abs';
function ArcTan(const A, B: Double): Double; external name 'Math.atan';
function Cos(const A: Double): Double; external name 'Math.cos';
function Exp(const A: Double): Double; external name 'Math.exp';
function Frac(const A: Double): Double; assembler;
function Ln(const A: Double): Double; external name 'Math.log';
function Odd(const A: Integer): Boolean; assembler;
function Random(const Range: Integer): Integer; overload; assembler;
function Random: Double; overload; external name 'Math.random';
function Round(const A: Double): NativeInt; external name 'Math.round';
function Sin(const A: Double): Double; external name 'Math.sin';
function Sqr(const A: Integer): Integer; assembler; overload;
function Sqr(const A: Double): Double; assembler; overload;
function sqrt(const A: Double): Double; external name 'Math.sqrt';
function Trunc(const A: Double): NativeInt; external name 'Math.trunc'; // not on IE

{*****************************************************************************
                          String functions
*****************************************************************************}
function Int(const A: Double): double;
function Copy(const S: string; Index, Size: Integer): String; assembler; overload;
function Copy(const S: string; Index: Integer): String; assembler; overload;
procedure Delete(var S: String; Index, Size: Integer); assembler; overload;
function Pos(const Search, InString: String): Integer; assembler; overload;
procedure Insert(const Insertion: String; var Target: String; Index: Integer); overload;
function upcase(c : char) : char; assembler;

procedure val(const S: String; out NI : NativeInt; out Code: Integer); overload;
procedure val(const S: String; out SI : ShortInt; out Code: Integer); overload;
procedure val(const S: String; out B : Byte; out Code: Integer); overload;
procedure val(const S: String; out SI : smallint; out Code: Integer); overload;
procedure val(const S: String; out W : word; out Code : Integer); overload;
procedure val(const S: String; out I : integer; out Code : Integer); overload;
procedure val(const S: String; out C : Cardinal; out Code: Integer); overload;
procedure val(const S: String; out d : double; out Code : Integer); overload;
function StringOfChar(c: Char; l: NativeInt): String;

{*****************************************************************************
                          Other functions
*****************************************************************************}
procedure Write; varargs; // ToDo: should be compiler built-in function
procedure Writeln; varargs; // ToDo: should be compiler built-in function

Type
  TConsoleHandler = Procedure (S : JSValue; NewLine : Boolean);
Function SetWriteCallBack(H : TConsoleHandler) : TConsoleHandler;

function Assigned(const V: JSValue): boolean; assembler; overload;
function StrictEqual(const A: JSValue; const B): boolean; assembler;
function StrictInequal(const A: JSValue; const B): boolean; assembler;

implementation

// function parseInt(s: String; Radix: NativeInt): NativeInt; external name 'parseInt'; // may result NaN
function isNaN(i: JSValue): boolean; external name 'isNaN'; // may result NaN

// needed by ClassNameIs, the real SameText is in SysUtils
function SameText(const s1, s2: String): Boolean; assembler;
asm
  return s1.toLowerCase() == s2.toLowerCase();
end;

function ParamCount: Longint;
begin
  if Assigned(OnParamCount) then
    Result:=OnParamCount()
  else
    Result:=0;
end;

function ParamStr(Index: Longint): String;
begin
  if Assigned(OnParamStr) then
    Result:=OnParamStr(Index)
  else if Index=0 then
    Result:='js'
  else
    Result:='';
end;


function Frac(const A: Double): Double; assembler;
asm
  return A % 1;
end;

function Odd(const A: Integer): Boolean; assembler;
asm
  return A&1 != 0;
end;

function Random(const Range: Integer): Integer; assembler;
asm
  return Math.floor(Math.random()*Range);
end;

function Sqr(const A: Integer): Integer; assembler;
asm
  return A*A;
end;

function Sqr(const A: Double): Double; assembler;
asm
  return A*A;
end;

function Copy(const S: string; Index, Size: Integer): String; assembler;
asm
  if (Index<1) Index = 1;
  return (Size>0) ? S.substring(Index-1,Index+Size-1) : "";
end;

function Copy(const S: string; Index: Integer): String; assembler;
asm
  if (Index<1) Index = 1;
  return S.substr(Index-1);
end;

procedure Delete(var S: String; Index, Size: Integer);
var
  h: String;
begin
  if (Index<1) or (Index>length(S)) or (Size<=0) then exit;
  h:=S;
  S:=copy(h,1,Index-1)+copy(h,Index+Size);
end;

function Pos(const Search, InString: String): Integer; assembler;
asm
  return InString.indexOf(Search)+1;
end;

procedure Insert(const Insertion: String; var Target: String; Index: Integer);
var
  t: String;
begin
  if Insertion='' then exit;
  t:=Target;
  if Index<1 then
    Target:=Insertion+t
  else if Index>length(t) then
    Target:=t+Insertion
  else
    Target:=copy(t,1,Index-1)+Insertion+copy(t,Index,length(t));
end;

var
  WriteBuf: String;
  JSArguments: array of JSValue; external name 'arguments';
  WriteCallBack : TConsoleHandler;

Function SetWriteCallBack(H : TConsoleHandler) : TConsoleHandler;

begin
  Result:=WriteCallBack;
  WriteCallBack:=H;
end;

procedure Write;
var
  i: Integer;
begin
  for i:=0 to length(JSArguments)-1 do
    if Assigned(WriteCallBack) then
      WriteCallBack(JSArguments[i],False)
    else
      WriteBuf:=WriteBuf+String(JSArguments[i]);
end;

procedure Writeln;

var
  i,l: Integer;
  s: String;

begin
  L:=length(JSArguments)-1;
  if Assigned(WriteCallBack) then
    begin
    for i:=0 to L do
      WriteCallBack(JSArguments[i],I=L);
    end
  else
    begin
    s:=WriteBuf;
    for i:=0 to L do
      s:=s+String(JSArguments[i]);
    asm
      console.log(s);
    end;
    WriteBuf:='';
    end;
end;

function Int(const A: Double): double;

  function FTrunc(const A: Double): double; overload; external name 'Math.trunc';

begin
  Result:=FTrunc(A);
end;

function Number(S: String): Double; external name 'Number';

procedure val(const S: String; out NI : NativeInt; out Code: Integer);

var
  x : double;

begin
  Code:=0;
  x:=Number(S);
  if isNaN(x) or (X<>Int(X)) then
    Code:=1
  else
    NI:=Trunc(x);
end;

procedure val(const S: String; out SI : ShortInt; out Code: Integer);

var
  X:Double;
begin
  Code:=0;
  x:=Number(S);
  if isNaN(x) or (X<>Int(X)) then
    Code:=1
  else if (x<MinShortInt) or (x>MaxShortInt) then
    Code:=2
  else
    SI:=Trunc(x);
end;

procedure val(const S: String; out SI: smallint; out Code: Integer);

var
  x: double;
begin
  Code:=0;
  x:=Number(S);
  if isNaN(x) or (X<>Int(X)) then
    Code:=1
  else if (x<MinSmallint) or (x>MaxSmallint) then
    Code:=2
  else
    SI:=Trunc(x);
end;

procedure val(const S: String; out C: Cardinal; out Code: Integer);

var
  x: double;
begin
  Code:=0;
  x:=Number(S);
  if isNaN(x) or (X<>Int(X)) then
    Code:=1
  else if (x<0) or (x>MaxCardinal) then
    Code:=2
  else
    C:=trunc(x);
end;

procedure val(const S: String; out B: Byte; out Code: Integer);

var
  x: double;
begin
  Code:=0;
  x:=Number(S);
  if isNaN(x) or (X<>Int(X)) then
    Code:=1
  else if (x<0) or (x>MaxByte) then
    Code:=2
  else
    B:=Trunc(x);
end;


procedure val(const S: String; out W: word; out Code: Integer);

var
  x: double;
begin
  Code:=0;
  x:=Number(S);
  if isNaN(x) then
    Code:=1
  else if (x<0) or (x>MaxWord) then
    Code:=2
  else
    W:=Trunc(x);
end;

procedure val(const S : String; out I : integer; out Code : Integer);
var
  x: double;
begin
  Code:=0;
  x:=Number(S);
  if isNaN(x) then
    Code:=1
  else if x>MaxInt then
    Code:=2
  else
    I:=Trunc(x);
end;

procedure val(const S : String; out d : double; out Code : Integer);

Var
  x: double;
begin
  x:=Number(S);
  if isNaN(x) then
    Code:=1
  else
    begin
    Code:=0;
    d:=x;
    end;
end;

function upcase(c : char) : char; assembler;

asm
  return c.toUpperCase();
end;

function StringOfChar(c: Char; l: NativeInt): String;
var
  i: Integer;
begin
  Result:='';
  for i:=1 to l do Result:=Result+c;
end;

function Assigned(const V: JSValue): boolean; assembler;
asm
  return (V!=undefined) && (V!=null) && (!rtl.isArray(V) || (V.length > 0));
end;

function StrictEqual(const A: JSValue; const B): boolean; assembler;
asm
  return A === B;
end;

function StrictInequal(const A: JSValue; const B): boolean; assembler;
asm
  return A !== B;
end;

{ TObject }

constructor TObject.Create;
begin

end;

destructor TObject.Destroy;
begin

end;

procedure TObject.Free;
begin
  Destroy;
end;

class function TObject.ClassType: TClass; assembler;
asm
  return this;
end;

class function TObject.ClassNameIs(const Name: string): boolean;
begin
  Result:=SameText(Name,ClassName);
end;

class function TObject.InheritsFrom(aClass: TClass): boolean; assembler;
asm
  return (aClass!=null) && ((this==aClass) || aClass.isPrototypeOf(this));
end;

procedure TObject.AfterConstruction;
begin

end;

procedure TObject.BeforeDestruction;
begin

end;

function TObject.Equals(Obj: TObject): boolean;
begin
  Result:=Obj=Self;
end;

function TObject.ToString: String;
begin
  Result:=ClassName;
end;


initialization
  ExitCode:=0; // set it here, so that WPO does not remove it


end.

