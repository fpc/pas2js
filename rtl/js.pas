{
    This file is part of the Pas2JS run time library.
    Copyright (c) 2017 by Mattias Gaertner

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
unit JS;

{$mode objfpc}
{$modeswitch externalclass}

interface

uses
  Types;

type
  // We cannot use EConvertError or Exception, this would result in a circular dependency.

  { EJS }

  EJS = class(TObject)
  private
    FMessage: string;
  Public
    constructor Create(const Msg: String); reintroduce;
    property Message : string Read FMessage Write FMessage;
  end;

  TJSObjectPropertyDescriptor = JSValue;
  Float32 = Double;
  Float64 = Double;

  { TJSObject }

  TJSObject = class external name 'Object'
  private
    function GetProperties(Name: String): JSValue; external name '[]';
    procedure SetProperties(Name: String; const AValue: JSValue); external name '[]';
  public
    constructor new;
    class function create(const proto: TJSObject): TJSObject;
    class function create(const proto, propertiesObject: TJSObject): TJSObject;
    class function assign(const Target, Source1: TJSObject): TJSObject; varargs;
    class procedure defineProperty(const obj: TJSObject; propname: String; const descriptor: TJSObjectPropertyDescriptor);
    //class procedure defineProperties
    class function freeze(const obj: TJSObject): TJSObject;
    class function getOwnPropertyDescriptor(const obj: TJSObject; propname: String): TJSObjectPropertyDescriptor;
    //class function getOwnPropertyDescriptors
    class function getOwnPropertyNames(const obj: TJSObject): TStringDynArray;
    {$IFDEF FIREFOX}
    class function getOwnPropertySymbols(const obj: TJSObject): TJSValueDynArray;
    {$ENDIF}
    class function getPrototypeOf(const obj: TJSObject): TJSObject;
    {$IFDEF FIREFOX}
    class function _is(const value1, value2: JSValue): boolean;
    {$ENDIF}
    class function isExtensible(const obj: TJSObject): boolean;
    class function isFrozen(const obj: TJSObject): boolean;
    class function isSealed(const obj: TJSObject): boolean;
    class function keys(const obj: TJSObject): TStringDynArray;
    class function preventExtensions(const obj: TJSObject): TJSObject;
    class function seal(const obj: TJSObject): TJSObject;
    class function setPrototypeOf(const obj, prototype: TJSObject): TJSObject;
    function hasOwnProperty(prop: String): boolean;
    function isPrototypeOf(const obj: TJSObject): boolean;
    function propertyIsEnumerable(propname: String): boolean;
    function toLocaleString: String;
    function toString: String;
    function valueOf: JSValue;
    property Properties[Name: String]: JSValue read GetProperties write SetProperties; default;
  end;

  TJSObjectDynArray = Array of TJSObject;
  TJSObjectDynArrayArray = Array of TJSObjectDynArray;
  TJSStringDynArray = Array of String;

  { TJSFunction }

  TJSFunction = class external name 'Function'(TJSObject)
  private
    Flength: NativeInt external name 'length';
    Fprototyp: TJSFunction external name 'prototyp';
  public
    name: String;
    property prototyp: TJSFunction read Fprototyp;
    property length: NativeInt read Flength;
    function apply(thisArg: TJSObject; const ArgArray: TJSValueDynArray): JSValue; varargs;
    function bind(thisArg: TJSObject): JSValue; varargs;
    function call(thisArg: TJSObject): JSValue; varargs;
  end;

  { TJSDate - wrapper for JavaScript Date }

  TJSDate = class external name 'Date'(TJSFunction)
  private
    function getDate: NativeInt;
    function getFullYear: NativeInt;
    function getHours: NativeInt;
    function getMilliseconds: NativeInt;
    function getMinutes: NativeInt;
    function getMonth: NativeInt;
    function getSeconds: NativeInt;
    function getYear: NativeInt;
    function getTime: NativeInt;
    function getUTCDate: NativeInt;
    function getUTCFullYear: NativeInt;
    function getUTCHours: NativeInt;
    function getUTCMilliseconds: NativeInt;
    function getUTCMinutes: NativeInt;
    function getUTCMonth: NativeInt;
    function getUTCSeconds: NativeInt;
    procedure setDate(const AValue: NativeInt);
    procedure setFullYear(const AValue: NativeInt);
    procedure setHours(const AValue: NativeInt);
    procedure setMilliseconds(const AValue: NativeInt);
    procedure setMinutes(const AValue: NativeInt);
    procedure setMonth(const AValue: NativeInt);
    procedure setSeconds(const AValue: NativeInt);
    procedure setYear(const AValue: NativeInt);
    procedure setTime(const AValue: NativeInt);
    procedure setUTCDate(const AValue: NativeInt);
    procedure setUTCFullYear(const AValue: NativeInt);
    procedure setUTCHours(const AValue: NativeInt);
    procedure setUTCMilliseconds(const AValue: NativeInt);
    procedure setUTCMinutes(const AValue: NativeInt);
    procedure setUTCMonth(const AValue: NativeInt);
    procedure setUTCSeconds(const AValue: NativeInt);
  public
    constructor New; reintroduce;
    constructor New(const MilliSecsSince1970: NativeInt); // milliseconds since 1 January 1970 00:00:00 UTC, with leap seconds ignored
    constructor New(const aDateString: String); // RFC 2822, ISO8601
    constructor New(aYear: NativeInt; aMonth: NativeInt; aDayOfMonth: NativeInt = 1;
      TheHours: NativeInt = 0; TheMinutes: NativeInt = 0; TheSeconds: NativeInt = 0;
      TheMilliseconds: NativeInt = 0);
    class function now: NativeInt; // current date and time in milliseconds since 1 January 1970 00:00:00 UTC, with leap seconds ignored
    class function parse(const aDateString: string): NativeInt; // format depends on browser
    class function UTC(aYear: NativeInt; aMonth: NativeInt = 0; aDayOfMonth: NativeInt = 1;
      TheHours: NativeInt = 0; TheMinutes: NativeInt = 0; TheSeconds: NativeInt = 0;
      TheMilliseconds: NativeInt = 0): NativeInt;
    function getDay: NativeInt;
    function getTimezoneOffset: NativeInt;
    function getUTCDay: NativeInt; // day of the week
    function toDateString: string; // human readable date, without time
    function toISOString: string; // ISO 8601 Extended Format
    function toJSON: string;
    function toGMTString: string; // in GMT timezone
    function toLocaleDateString: string; // date in locale timezone, no time
    function toLocaleString: string; reintroduce; // date and time in locale timezone
    function toLocaleTimeString: string; // time in locale timezone, no date
    function toTimeString: string; // time human readable, no date
    function toUTCString: string; // date and time using UTC timezone
    property Year: NativeInt read getYear write setYear;
    property Time: NativeInt read getTime write setTime; // milliseconds since 1 January 1970 00:00:00 UTC, with leap seconds ignored
    property FullYear: NativeInt read getFullYear write setFullYear;
    property UTCDate: NativeInt read getUTCDate write setUTCDate; // day of month
    property UTCFullYear: NativeInt read getUTCFullYear write setUTCFullYear;
    property UTCHours: NativeInt read getUTCHours write setUTCHours;
    property UTCMilliseconds: NativeInt read getUTCMilliseconds write setUTCMilliseconds;
    property UTCMinutes: NativeInt read getUTCMinutes write setUTCMinutes;
    property UTCMonth: NativeInt read getUTCMonth write setUTCMonth;
    property UTCSeconds: NativeInt read getUTCSeconds write setUTCSeconds;
    property Month: NativeInt read getMonth write setMonth;
    property Date: NativeInt read getDate write setDate; // day of the month, starting at 1
    property Hours: NativeInt read getHours write setHours;
    property Minutes: NativeInt read getMinutes write setMinutes;
    property Seconds: NativeInt read getSeconds write setSeconds;
    property Milliseconds: NativeInt read getMilliseconds write setMilliseconds;
  end;

  TLocaleCompareOptions = record
    localematched : string;
    usage: string;
    sensitivity : string;
    ignorePunctuation : Boolean;
    numeric : boolean;
    caseFirst : string;
  end;

  TJSRegexp = class external name 'RegExp'
  private
  {$IFDEF FIREFOX}
    // not on all browsers:
    FFlags : string; external name 'flags';
    FSticky : boolean; external name 'sticky';
  {$endif}
    fglobal: boolean; external name 'global';
    fignoreCase : boolean; external name 'ignoreCase';
    fmultiline : boolean; external name 'multiline';
    fsource : string; external name 'source';
    funicode : boolean; external name 'unicode';
  public
    Constructor New(Pattern : string);
    Constructor New(Pattern, Flags : string);
    lastIndex: NativeInt;
    function exec(aString : string): TStringDynArray;
    function test(aString : string) : boolean;
    function toString : String;
    property Global : boolean read fglobal;
    property IgnoreCase : Boolean read FIgnoreCase;
    property Multiline : Boolean Read FMultiLine;
    Property Source : string Read FSource;
    Property Unicode : boolean Read FUnicode;
    {$IFDEF FIREFOX}
    // not on all browsers:
    property Flags : string read FFlags;
    property Sticky : boolean read FSticky;
    {$endif}
  end;


  TReplaceCallBack = Function () : string; varargs;

  TJSString = class external name 'String'
  private
    flength : NativeInt; external name 'length';
  public 
    constructor New(Const S : String);
    constructor New(Const I : NativeInt);
    constructor New(Const D : double);
    property length : NativeInt read flength; 
    class function fromCharCode() : string; varargs;
    class function fromCodePoint() : string; varargs;
    function anchor(const aName : string) : string;
    function charAt(aIndex : NativeInt) : string;
    function charCodeAt(aIndex : NativeInt) : NativeInt;
    function codePointAt(aIndex : NativeInt) : NativeInt;
    function concat(s : string) : string; varargs;
    function endsWith(aSearchString : string; Pos : NativeInt = 0) : boolean;
    function includes(aSearchString : string; Pos : NativeInt = 0) : boolean;
    function indexOf(aSearchString : String; Pos : NativeInt = 0) : Integer;
    function lastIndexOf(aSearchString : String) : NativeInt;overload;
    function lastIndexOf(aSearchString : String; Pos : NativeInt) : Integer;overload;
    function link(aUrl : string) : String;
    function localeCompare(aCompareString : string) : NativeInt; overload;
    function localeCompare(aCompareString : string; aLocales: string) : integer; overload;
    function localeCompare(compareString : string; locales: string; Options : TlocaleCompareOptions) : integer; overload;
    function match(aRegexp : TJSRegexp) : TStringDynArray; overload;
    function match(aRegexp : String) : TStringDynArray;overload;
    {$IFDEF ECMAScript6}
    function normalize(aForm : string) : string;
    {$ENDIF}
    function _repeat(aCount : NativeInt) : Integer; external name 'repeat';
    function replace(aRegexp : String; NewString : String) : String; overload;
    function replace(aRegexp : TJSRegexp; NewString : String) : String; overload;
    function replace(Regexp : String; aCallback : TReplaceCallBack) : String; overload;
    function replace(Regexp : TJSRegexp; aCallback : TReplaceCallBack) : String; overload;
    function search(Regexp : TJSRegexp) : NativeInt; overload;
    function search(Regexp : JSValue) : NativeInt; overload;
    function slice(aBeginIndex : NativeInt) : String; overload;
    function slice(aBeginIndex, aEndIndex : NativeInt) : String; overload;
    function split : TStringDynArray; overload;
    function split(aSeparator : string) : TStringDynArray; overload;
    function split(aSeparator : string; aLimit : NativeInt) : TStringDynArray; overload;
    function startsWith(aSearchString : String) : Boolean; overload;
    function startsWith(aSearchString : String; aPosition : NativeInt) : Boolean; overload;
    function substr(aStartIndex : NativeInt) : String; overload;
    function substr(aStartIndex,aLength : NativeInt) : String; overload;
    function subString(aStartIndex : NativeInt) : String; overload;
    function subString(aStartIndex,aEndIndex : NativeInt) : String; overload;
    function toLocaleLowerCase : String;
    function toLocaleUpperCase : String;
    function toLowerCase : String;
    function toString : string;
    function toUpperCase : String;
    function trim : string;
    function valueOf : string;
  end;

  TJSArray = Class;
  
  TJSArrayCallBack = function (element : JSValue; index: NativeInt; anArray : TJSArray) : Boolean;
  TJSArrayEvent = function (element : JSValue; index: NativeInt; anArray : TJSArray) : Boolean of object;
  TJSArrayMapCallBack = function (element : JSValue; index: NativeInt; anArray : TJSArray) : JSValue;
  TJSArrayMapEvent = function (element : JSValue; index: NativeInt; anArray : TJSArray) : JSValue of object;
  TJSArrayReduceCallBack = function (accumulator, currentValue : JSValue; currentIndex : NativeInt; anArray : TJSArray) : JSValue;
  TJSArrayCompareCallBack = function (a,b : JSValue) : NativeInt; 
    
  { TJSArray }

  TJSArray = Class external name 'Array'
  private
    function GetElements(Index: NativeInt): JSValue; external name '[]';
    procedure SetElements(Index: NativeInt; const AValue: JSValue); external name '[]';
  public
    flength : integer; external name 'length';
    constructor new; overload;
    constructor new(aLength : NativeInt); overload;
    constructor new(aElement1 : JSValue); varargs; overload;
    class function _of() : TJSArray; varargs; external name 'of'; 
    class function isArray(a: JSValue) : Boolean;
{$IFDEF JAVASCRIPT2015}    
    class function from(a : JSValue) : TJSArray;
{$ENDIF}    
    function concat(el : JSValue) : TJSArray; varargs;
    function copyWithin(aTarget : NativeInt) : TJSarray;overload; // not in IE
    function copyWithin(aTarget, aStart : NativeInt) : TJSarray;overload; // not in IE
    function copyWithin(aTarget, aStart, aEnd : NativeInt) : TJSarray;overload; // not in IE
    Function every(const aCallback : TJSArrayCallBack) : boolean;overload;
    Function every(const aCallback : TJSArrayEvent; aThis : TObject) : boolean;overload;
    Function filter(const aCallBack : TJSArrayCallBack) : TJSArray; overload;
    Function filter(const aCallBack : TJSArrayEvent; aThis : TObject) : TJSArray;overload;
    Function fill(aValue : JSValue) : TJSArray; overload;
    Function fill(aValue : JSValue; aStartIndex : NativeInt) : TJSArray; overload;
    Function fill(aValue : JSValue; aStartIndex,aEndIndex : NativeInt) : TJSArray; overload;
    Function find(const aCallBack : TJSArrayCallBack) : JSValue; overload;
    Function find(const aCallBack : TJSArrayEvent; aThis : TObject) : JSValue; overload;
    Function findIndex(const aCallBack : TJSArrayCallBack) : NativeInt; overload;
    Function findIndex(const aCallBack : TJSArrayEvent; aThis : TObject) : NativeInt; overload;
    procedure forEach(const aCallBack : TJSArrayCallBack); overload;
    procedure forEach(const aCallBack : TJSArrayEvent; aThis : TObject); overload;
    function includes(aElement : JSValue) : Boolean; overload;
    function includes(aElement : JSValue; FromIndex : NativeInt) : Boolean; overload;
    function indexOf(aElement : JSValue) : NativeInt; overload;
    function indexOf(aElement : JSValue; FromIndex : NativeInt) : NativeInt; overload;
    function join : String; overload;
    function join (aSeparator : string) : String; overload;
    function lastIndexOf(aElement : JSValue) : NativeInt; overload;
    function lastIndexOf(aElement : JSValue; FromIndex : NativeInt) : NativeInt; overload;
    Function map(const aCallBack : TJSArrayCallBack) : TJSArray; overload;
    Function map(const aCallBack : TJSArrayEvent; aThis : TObject) : TJSArray; overload;
    function pop : JSValue; 
    function push(aElement : JSValue) : NativeInt; varargs;
    function reduce(const aCallBack : TJSArrayReduceCallBack) : JSValue; overload;
    function reduce(const aCallBack : TJSArrayReduceCallBack; initialValue : JSValue) : JSValue; overload;
    function reduceRight(const aCallBack : TJSArrayReduceCallBack) : JSValue; overload;
    function reduceRight(const aCallBack : TJSArrayReduceCallBack; initialValue : JSValue) : JSValue; overload;
    Function reverse : TJSArray;
    Function shift : JSValue;
    Function slice : TJSArray; overload;
    function slice(aBegin : NativeInt) : TJSArray; overload;
    function slice(aBegin,aEnd : NativeInt) : TJSArray; overload;
    Function some(const aCallback : TJSArrayCallBack) : boolean; overload;
    Function some(const aCallback : TJSArrayEvent; aThis : TObject) : boolean; overload;
    Function sort(const aCallback : TJSArrayCompareCallBack) : TJSArray; overload;
    Function sort() : TJSArray; overload;
    function splice(aStart : NativeInt) : TJSArray; overload;
    function splice(aStart,aDeleteCount : NativeInt) : TJSArray; varargs; overload;
    function toLocaleString: String; overload;
    function toLocaleString(locales : string) : String; overload;
    function toLocaleString(locales : string; const Options : TLocaleCompareOptions) : String; overload;
    function toString : String;
    function unshift : NativeInt; varargs; 
    Property Length : Integer Read FLength;
    property Elements[Index: NativeInt]: JSValue read GetElements write SetElements; default;
  end;

  TJSArrayBuffer = Class external name 'ArrayBuffer'
  private
    fLength : NativeInt; external name 'byteLength';
  public
    constructor new(aByteLength : NativeInt);
    class function isView(aValue : JSValue) : Boolean;   
    function slice(aBegin : NativeInt) : TJSArrayBuffer; overload;
    function slice(aBegin,aEnd : NativeInt) : TJSArrayBuffer; overload;
    Property byteLength : NativeInt Read fLength;
  end;

  TJSBufferSource = class external name 'BufferSource'
  end;

  { TJSTypedArray }
  TJSTypedArray = Class;

  TJSTypedArrayCallBack = function (element : JSValue; index: NativeInt; anArray : TJSTypedArray) : Boolean;
  TJSTypedArrayEvent = function (element : JSValue; index: NativeInt; anArray : TJSTypedArray) : Boolean of object;
  TJSTypedArrayMapCallBack = function (element : JSValue; index: NativeInt; anArray : TJSTypedArray) : JSValue;
  TJSTypedArrayMapEvent = function (element : JSValue; index: NativeInt; anArray : TJSTypedArray) : JSValue of object;
  TJSTypedArrayReduceCallBack = function (accumulator, currentValue : JSValue; currentIndex : NativeInt; anArray : TJSTypedArray) : JSValue;
  TJSTypedArrayCompareCallBack = function (a,b : JSValue) : NativeInt;

  TJSTypedArray = class external name 'TypedArray' (TJSBufferSource)
  Private
    FBuffer: TJSArrayBuffer; external name 'buffer';
    FByteLength: NativeInt; external name 'byteLength';
    FLength: NativeInt; external name 'length';
    FByteOffset: NativeInt; external name 'byteOffset';
    function getValue(Index : NativeInt) : JSValue; external name '[]';
    procedure setValue(Index : NativeInt;AValue : JSValue); external name '[]';
  Public
    class var BYTES_PER_ELEMENT : NativeInt;
    class var name : string;
    class function from(aValue : jsValue) : TJSTypedArray;
    class function from(aValue : jsValue; Map : TJSTypedArrayMapCallBack) : TJSTypedArray;
    class function from(aValue : jsValue; aMap : TJSTypedArrayMapEvent) : TJSTypedArray;
    class function _of(aValue : jsValue) : TJSTypedArray; varargs;
    function copyWithin(aTarget : NativeInt) : TJSTypedArray;overload;
    function copyWithin(aTarget, aStart : NativeInt) : TJSTypedArray;overload;
    function copyWithin(aTarget, aStart, aEnd : NativeInt) : TJSTypedArray;overload;
    Function every(const aCallback : TJSTypedArrayCallBack) : boolean;overload;
    Function every(const aCallback : TJSTypedArrayEvent; aThis : TObject) : boolean;overload;
    Function fill(aValue : JSValue) : TJSTypedArray; overload;
    Function fill(aValue : JSValue; aStartIndex : NativeInt) : TJSTypedArray; overload;
    Function fill(aValue : JSValue; aStartIndex,aEndIndex : NativeInt) : TJSTypedArray; overload;
    Function filter(const aCallBack : TJSTypedArrayCallBack) : TJSTypedArray; overload;
    Function filter(const aCallBack : TJSTypedArrayEvent; aThis : TObject) : TJSTypedArray;overload;
    Function find(const aCallBack : TJSTypedArrayCallBack) : JSValue; overload;
    Function find(const aCallBack : TJSTypedArrayEvent; aThis : TObject) : JSValue; overload;
    Function findIndex(const aCallBack : TJSTypedArrayCallBack) : NativeInt; overload;
    Function findIndex(const aCallBack : TJSTypedArrayEvent; aThis : TObject) : NativeInt; overload;
    procedure forEach(const aCallBack : TJSTypedArrayCallBack); overload;
    procedure forEach(const aCallBack : TJSTypedArrayEvent; aThis : TObject); overload;
    function includes(aElement : JSValue) : Boolean; overload;
    function includes(aElement : JSValue; FromIndex : NativeInt) : Boolean; overload;
    function indexOf(aElement : JSValue) : NativeInt; overload;
    function indexOf(aElement : JSValue; FromIndex : NativeInt) : NativeInt; overload;
    function join : String; overload;
    function join (aSeparator : string) : String; overload;
    function lastIndexOf(aElement : JSValue) : NativeInt; overload;
    function lastIndexOf(aElement : JSValue; FromIndex : NativeInt) : NativeInt; overload;
    Function map(const aCallBack : TJSTypedArrayCallBack) : TJSTypedArray; overload;
    Function map(const aCallBack : TJSTypedArrayEvent; aThis : TObject) : TJSTypedArray; overload;
    function reduce(const aCallBack : TJSTypedArrayReduceCallBack) : JSValue; overload;
    function reduce(const aCallBack : TJSTypedArrayReduceCallBack; initialValue : JSValue) : JSValue; overload;
    function reduceRight(const aCallBack : TJSTypedArrayReduceCallBack) : JSValue; overload;
    function reduceRight(const aCallBack : TJSTypedArrayReduceCallBack; initialValue : JSValue) : JSValue; overload;
    Function reverse : TJSTypedArray;
    procedure _set(anArray : TJSTypedArray);
    procedure _set(anArray : TJSTypedArray; anOffset : NativeInt);
    Function slice : TJSTypedArray; overload;
    function slice(aBegin : NativeInt) : TJSTypedArray; overload;
    function slice(aBegin,aEnd : NativeInt) : TJSTypedArray; overload;
    Function some(const aCallback : TJSTypedArrayCallBack) : boolean; overload;
    Function some(const aCallback : TJSTypedArrayEvent; aThis : TObject) : boolean; overload;
    Function sort(const aCallback : TJSTypedArrayCompareCallBack) : TJSTypedArray; overload;
    Function sort() : TJSTypedArray; overload;
    function splice(aStart : NativeInt) : TJSTypedArray; overload;
    function splice(aStart,aDeleteCount : NativeInt) : TJSTypedArray; varargs; overload;
    function toLocaleString: String; overload;
    function toLocaleString(locales : string) : String; overload;
    function toLocaleString(locales : string; const Options : TLocaleCompareOptions) : String; overload;
    function toString : String;
    function unshift : NativeInt; varargs;
    property buffer : TJSArrayBuffer read FBuffer;
    property byteLength : NativeInt Read FByteLength;
    property byteOffset : NativeInt Read FByteOffset;
    property length : NativeInt Read FLength;
    Property values[Index : NativeInt] : JSValue Read getValue Write SetValue; default;
  end;

  { TJSInt8Array }

  TJSInt8Array = class external name 'Int8Array' (TJSTypedArray)
  private
    function getTypedValue(Index : Integer): Shortint; external name '[]';
    procedure setTypedValue(Index : Integer; AValue: Shortint);external name '[]';
  public
    Property values[Index : Integer] : Shortint Read getTypedValue Write setTypedValue; default;
  end;

  TJSUint8Array  = class external name 'UInt8Array' (TJSTypedArray)
  private
    function getTypedValue(Index : Integer): Byte; external name '[]';
    procedure setTypedValue(Index : Integer; AValue: Byte);external name '[]';
  public
    Property values[Index : Integer] : Byte Read getTypedValue Write setTypedValue; default;
  end;

  TJSUint8ClampedArray  = class external name 'UInt8ClampedArray' (TJSTypedArray)
  private
    function getTypedValue(Index : Integer): Byte; external name '[]';
    procedure setTypedValue(Index : Integer; AValue: Byte);external name '[]';
  public
    Property values[Index : Integer] : Byte Read getTypedValue Write setTypedValue; default;
  end;

  TJSInt16Array = class external name 'Int16Array' (TJSTypedArray)
  private
    function getTypedValue(Index : Integer): smallint; external name '[]';
    procedure setTypedValue(Index : Integer; AValue: Smallint);external name '[]';
  public
    Property values[Index : Integer] : SmallInt Read getTypedValue Write setTypedValue; default;
  end;

  TJSUint16Array = class external name 'UInt16Array' (TJSTypedArray)
  private
    function getTypedValue(Index : Integer): Word; external name '[]';
    procedure setTypedValue(Index : Integer; AValue: Word);external name '[]';
  public
    Property values[Index : Integer] : Word Read getTypedValue Write setTypedValue; default;
  end;

  TJSInt32Array = class external name 'Int32Array' (TJSTypedArray)
  private
    function getTypedValue(Index : Integer): longint; external name '[]';
    procedure setTypedValue(Index : Integer; AValue: longint);external name '[]';
  public
    Property values[Index : Integer] : longint Read getTypedValue Write setTypedValue; default;
  end;

  TJSUint32Array = class external name 'UInt32Array' (TJSTypedArray)
  private
    function getTypedValue(Index : Integer): LongWord; external name '[]';
    procedure setTypedValue(Index : Integer; AValue: LongWord);external name '[]';
  public
    Property values[Index : Integer] : LongWord Read getTypedValue Write setTypedValue; default;
  end;

  TJSFloat32Array = class external name 'Float32Array' (TJSTypedArray)
  private
    function getTypedValue(Index : Integer): Float32; external name '[]';
    procedure setTypedValue(Index : Integer; AValue: Float32);external name '[]';
  public
    Property values[Index : Integer] : Float32 Read getTypedValue Write setTypedValue; default;
  end;

  TJSFloat64Array = class external name 'Float64Array' (TJSTypedArray)
  private
    function getTypedValue(Index : Integer): Float64; external name '[]';
    procedure setTypedValue(Index : Integer; AValue: Float64);external name '[]';
  public
    Property values[Index : Integer] : Float64 Read getTypedValue Write setTypedValue; default;
  end;

  TJSDataView = Class external name 'DataView' (TJSBufferSource)
  private
    fBuffer : TJSArrayBuffer; external name 'buffer';
    fLength : NativeInt; external name 'byteLength';
    fOffset : NativeInt; external name 'byteOffset';

  public
    constructor new(aBuffer : TJSArrayBuffer); overload;
    constructor new(aBuffer : TJSArrayBuffer; aOffset : NativeInt); overload;
    constructor new(aBuffer : TJSArrayBuffer; aOffset,aByteLength : NativeInt); overload;
    function getFloat32(aByteOffset : NativeInt) : double; overload;
    function getFloat32(aByteOffset : NativeInt; aLittleEndian: Boolean) : double; overload;
    function getFloat64(aByteOffset : NativeInt) : double; overload;
    function getFloat64(aByteOffset : NativeInt; aLittleEndian: Boolean) : double; overload;
    function getInt8(aByteOffset : NativeInt) : ShortInt; 
    function getInt16(aByteOffset : NativeInt) : SmallInt; overload;
    function getInt16(aByteOffset : NativeInt; aLittleEndian : Boolean) : SmallInt; overload;
    function getInt32(aByteOffset : NativeInt) : Longint; overload;
    function getInt32(aByteOffset : NativeInt; aLittleEndian : Boolean) : Longint; overload;
    function getUint8(aByteOffset : NativeInt) : Byte; overload;
    function getUint16(aByteOffset : NativeInt) : Word; overload;
    function getUint16(aByteOffset : NativeInt; aLittleEndian : Boolean) : Word; overload;
    function getUint32(aByteOffset : NativeInt) : LongWord; overload;
    function getUint32(aByteOffset : NativeInt; aLittleEndian : Boolean) : LongWord; overload;

    procedure setFloat32(aByteOffset : NativeInt; aValue : double); overload;
    procedure setFloat32(aByteOffset : NativeInt; aValue : double; aLittleEndian: Boolean); overload;
    procedure setFloat64(aByteOffset : NativeInt; aValue : double); overload;
    procedure setFloat64(aByteOffset : NativeInt; aValue : double; aLittleEndian: Boolean); overload;
    procedure setInt8(aByteOffset : NativeInt; aValue : ShortInt); 
    procedure setInt16(aByteOffset : NativeInt; aValue : SmallInt); overload;
    procedure setInt16(aByteOffset : NativeInt; aValue : SmallInt; aLittleEndian : Boolean); overload;
    procedure setInt32(aByteOffset : NativeInt; aValue : Longint); overload;
    procedure setInt32(aByteOffset : NativeInt; aValue : Longint; aLittleEndian : Boolean); overload;
    procedure setUint8(aByteOffset : NativeInt; aValue : Byte); overload;
    procedure setUint16(aByteOffset : NativeInt; aValue : Word); overload;
    procedure setUint16(aByteOffset : NativeInt; aValue : Word; aLittleEndian : Boolean); overload;
    procedure setUint32(aByteOffset : NativeInt; aValue : LongWord); overload;
    procedure setUint32(aByteOffset : NativeInt; aValue: LongWord; aLittleEndian : Boolean); overload;
 
    Property byteLength : NativeInt Read fLength;
    Property byteOffset : NativeInt read fOffset;
    property buffer : TJSArrayBuffer Read fBuffer;
  end;

  TJSJSON = class external name 'JSON' (TJSObject)
  Public
    class function parse(aJSON : String) : TJSObject;
    class function stringify(aValue : JSValue) : string;
    class function stringify(aValue,aReplacer : JSValue) : string;
    class function stringify(aValue,aReplacer : JSValue; space:  NativeInt) : string;
    class function stringify(aValue,aReplacer : JSValue; space:  String) : string;
  end;

var
  // This can be used in procedures/functions to provide access to the 'arguments' array.
  JSArguments: TJSValueDynArray; external name 'arguments';
  // This can be used in all code to access the javascript 'this' object.
  JSThis: TJSObject; external name 'this';
  // This can be used in catch blocks to access the JS throw value
  JSExceptValue: JSValue; external name '$e';

Function new(aElements: TJSValueDynArray) : TJSObject;

function decodeURIComponent(encodedURI : String) : String; external name 'decodeURIComponent';
function encodeURIComponent(str : String) : String; external name 'encodeURIComponent';

function parseInt(s: String; Radix: NativeInt): NativeInt; overload; external name 'parseInt'; // may result NaN
function parseInt(s: String): NativeInt; overload; external name 'parseInt'; // may result NaN
function parseFloat(s: String): double; overload; external name 'parseFloat'; // may result NaN

function hasString(const v: JSValue): boolean; external name 'rtl.hasString';// isString(v) and v<>''
function hasValue(const v: JSValue): boolean; assembler; // returns the JS definition of if(v): v is not false, undefined, null, 0, NaN, or the empty string. Note: JS if(new Boolean(false)) returns true.
function isArray(const v: JSValue): boolean; external name 'rtl.isArray';
function isBoolean(const v: JSValue): boolean; assembler;
function isCallback(const v: JSValue): boolean; assembler;
function isChar(const v: JSValue): boolean; assembler;
function isClass(const v: JSValue): boolean; assembler; // is a Pascal class, e.g. a TClass
function isClassInstance(const v: JSValue): boolean; assembler;// is a Pascal class instance, e.g. a TObject
function isFunction(const v: JSValue): boolean; external name 'rtl.isFunction';
function isInteger(const v: JSValue): boolean; assembler;
function isModule(const v: JSValue): boolean; external name 'rtl.isModule';
function isNull(const v: JSValue): boolean; assembler;
function isNumber(const v: JSValue): boolean; external name 'rtl.isNumber';
function isObject(const v: JSValue): boolean; external name 'rtl.isObject'; // true if not null and a JS Object
function isRecord(const v: JSValue): boolean; assembler;
function isString(const v: JSValue): boolean; external name 'rtl.isString';
function isUndefined(const v: JSValue): boolean; assembler;
function isDefined(const v: JSValue): boolean; assembler;
function isUTF16Char(const v: JSValue): boolean; assembler;
function isExt(const InstanceOrClass, aClass: JSValue): boolean; external name 'rtl.isExt'; // aClass can be a JS object or function
function jsInstanceOf(const aFunction, aFunctionWithPrototype: JSValue): String; assembler;
function jsTypeOf(const v: JSValue): String; external name 'typeof';
function jsIsNaN(const v: JSValue): boolean; external name 'isNaN';// true if value cannot be converted to a number. e.g. True on NaN, undefined, {}, '123'. False on true, null, '', ' ', '1A'
function toNumber(const v: JSValue): double; assembler; // if not possible, returns NaN

Type
  TJSValueType = (jvtNull,jvtBoolean,jvtInteger,jvtFloat,jvtString,jvtObject,jvtArray);

Function GetValueType(JS : JSValue) : TJSValueType;

Var
  Null : JSValue; external name 'null';
  Undefined : JSValue; external name 'undefined';

implementation

Function new(aElements: TJSValueDynArray) : TJSObject;

  function toString(I : Integer): string; external name 'String';

Var
  L,I : integer;
  S : String;

begin
  L:=length(aElements);
  if (L mod 2)=1 then
    raise EJS.Create('Number of arguments must be even');
  I:=0;
  // Check all arguments;
  While (i<L) do
    begin
    if Not isString(aElements[i]) then
      begin
      S:=ToString(I);
      raise EJS.Create('Argument '+S+' must be a string.');
      end;
    inc(I,2);
    end;
  I:=0;
  Result:=TJSObject.New;
  While (i<L) do
    begin
    S:=String(aElements[i]);
    Result.Properties[S]:=aElements[i+1];
    inc(I,2);
    end;
end;

function hasValue(const v: JSValue): boolean; assembler;
asm
  if(v){ return true; } else { return false; };
end;

function isBoolean(const v: JSValue): boolean; assembler;
asm
  return typeof(v) == 'boolean';
end;

function isCallback(const v: JSValue): boolean; assembler;
asm
  return rtl.isObject(v) && rtl.isObject(v.scope) && (rtl.isString(v.fn) || rtl.isFunction(v.fn));
end;

function isChar(const v: JSValue): boolean; assembler;
asm
  return (typeof(v)!="string") && (v.length==1);
end;

function isClass(const v: JSValue): boolean; assembler;
asm
  return (typeof(v)=="object") && (v!=null) && (v.$class == v);
end;

function isClassInstance(const v: JSValue): boolean; assembler;
asm
  return (typeof(v)=="object") && (v!=null) && (v.$class == Object.getPrototypeOf(v));
end;

function isInteger(const v: JSValue): boolean; assembler;
asm
  return Math.floor(v)===v;
end;

function isNull(const v: JSValue): boolean; assembler;
// Note: use identity, "==" would fit undefined
asm
  return v === null;
end;

function isRecord(const v: JSValue): boolean; assembler;
asm
  return (typeof(v)=="function") && (typeof(v.$create) == "function");
end;

function isUndefined(const v: JSValue): boolean; assembler;
asm
  return v == undefined;
end;

function isDefined(const v: JSValue): boolean; assembler;
asm
  return !(v == undefined);
end;

function isUTF16Char(const v: JSValue): boolean; assembler;
asm
  if (typeof(v)!="string") return false;
  if ((v.length==0) || (v.length>2)) return false;
  var code = v.charCodeAt(0);
  if (code < 0xD800){
    if (v.length == 1) return true;
  } else if (code <= 0xDBFF){
    if (v.length==2){
      code = v.charCodeAt(1);
      if (code >= 0xDC00 && code <= 0xDFFF) return true;
    };
  };
  return false;
end;

function jsInstanceOf(const aFunction, aFunctionWithPrototype: JSValue
  ): String; assembler;
asm
  return aFunction instanceof aFunctionWithPrototype;
end;

function toNumber(const v: JSValue): double; assembler;
asm
  return v-0;
end;

{ EJS }

constructor EJS.Create(const Msg: String);
begin
  FMessage:=Msg;
end;


Function GetValueType(JS : JSValue) : TJSValueType;

Var
  t : string;

begin
  if isNull(js) then   // null reported as object
    result:=jvtNull
  else
    begin
    t:=jsTypeOf(js);
    if (t='string') then
      Result:=jvtString
    else if (t='boolean') then
      Result:=jvtBoolean
    else if (t='object') then
      begin
      if IsArray(JS) then
        Result:=jvtArray
      else
        Result:=jvtObject;
      end
    else if (t='number') then
      if isInteger(JS) then
        result:=jvtInteger
      else
        result:=jvtFloat
    end;
end;

end.

