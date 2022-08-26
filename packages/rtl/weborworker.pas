unit weborworker;

{$mode objfpc}
{$modeswitch externalclass}

interface

uses
  JS, types;

type
  // Forward declarations

  TJSCryptoKey = Class;
  TJSSubtleCrypto = Class;
  TJSEventTarget = class;
  TIDBDatabase = class;
  TJSIDBObjectStore = class;
  TJSIDBRequest = class;
  TJSServiceWorker = class;
  TJSReadableStream = class;
  TJSClient = class;

{ ----------------------------------------------------------------------
  Console
  ----------------------------------------------------------------------}


  TJSConsole = class external name 'Console'  (TJSObject)
  Public
    procedure assert(anAssertion : string; Obj1 : JSValue); varargs;
    Procedure clear;
    procedure count; overload;
    procedure count(aCounter : String);
    procedure debug(Obj1 : JSValue); varargs of JSValue;
    procedure error(Obj1 : JSValue); varargs of JSValue;
    procedure group; overload;
    procedure group(aLabel : String); overload;
    procedure groupCollapsed; overload;
    procedure groupCollapsed(aLabel : String);overload;
    procedure groupEnd;
    procedure info(Obj1 : JSValue); varargs of JSValue;
    procedure log(Obj1 : JSValue); varargs of JSValue;
    procedure table(args: array of JSValue); overload;
    procedure table(args: array of JSValue; Columns : Array of string);
    procedure table(args: TJSObject); overload;
    procedure table(args: TJSObject; Columns : Array of string); overload;
    procedure time(aName : string);
    procedure timeEnd(aName : string);
    procedure trace;
    procedure warn(Obj1 : JSValue); varargs of JSValue;
  end;

{ ----------------------------------------------------------------------
  Events
  ----------------------------------------------------------------------}


  TJSTimerCallBack = reference to procedure; safecall;

  TJSEventInit = record
    bubbles : boolean;
    cancelable : boolean;
    scoped : boolean;
    composed : boolean;
  end;

  TJSEvent = class external name 'Event'  (TJSObject)
  Private
    FBubbles : Boolean; external name 'bubbles';
    FCancelable : Boolean; external name 'cancelable';
    FComposed : Boolean; external name 'composed';
    FCurrentTarget : TJSEventTarget; external name 'currentTarget';
    FdefaultPrevented : Boolean; external name 'defaultPrevented';
    FEventPhase : NativeInt; external name 'eventPhase';
    FTarget : TJSEventTarget; external name 'target';
    FTimeStamp : NativeInt; external name 'timestamp';
    FType : String; external name 'type';
    FIsTrusted : Boolean; external name 'isTrusted';
  Public
    Const
      NONE = 0;
      CAPTURING_PHASE = 1;
      AT_TARGET  = 2;
      BUBBLING_PHASE = 3;
  public
    cancelBubble : Boolean;
    constructor new (aType : String; const aInit : TJSEventInit); overload;
    constructor new (aType : String); overload;
    procedure preventDefault;
    procedure stopImmediatePropagation;
    procedure stopPropagation;
    Property bubbles : Boolean Read FBubbles;
    Property cancelable : Boolean Read FCancelable;
    Property composed : Boolean Read FComposed;
    property currentTarget : TJSEventTarget Read FCurrentTarget;
    property defaultPrevented : Boolean Read FdefaultPrevented;
    property eventPhase : NativeInt Read FEventPhase;
    property target : TJSEventTarget Read FTarget;
    Property timestamp : NativeInt Read FTimeStamp;
    property _type : string read FType;
    property isTrusted : Boolean Read FIsTrusted;
  end;

  TJSExtendableEvent = class external name 'ExtendableEvent' (TJSEvent)
    Procedure waitUntil(aPromise : TJSPromise);
  end;


  TJSEventHandler = reference to function(Event: TJSEvent): boolean; safecall;
  TJSRawEventHandler = reference to Procedure(Event: TJSEvent); safecall;

  TJSEventTarget = class external name 'EventTarget' (TJSObject)
  public
    procedure addEventListener(aname : string; aListener : TJSEventHandler);
    procedure addEventListener(aname : string; aListener : TJSRawEventHandler);
    procedure addEventListener(aname : string; aListener : JSValue);
    function dispatchEvent(event : JSValue) : Boolean;
    procedure removeEventListener(aname : string; aListener : TJSEventHandler);
    procedure removeEventListener(aname : string; aListener : TJSRawEventHandler);
    procedure removeEventListener(aname : string; aListener : JSValue);
  end;


  TJSMessagePort = class external name 'MessagePort' (TJSEventTarget)
  Public
    procedure close;
    procedure postMessage(aValue : JSValue);
    procedure postMessage(aValue : JSValue; aList : TJSValueDynArray);
    procedure start;
  end;
  TJSMessagePortDynArray = Array of TJSMessagePort;

  { TJSMessageEvent }

  TJSMessageEvent = class external name 'MessageEvent' (TJSEvent)
  private
    FData: JSValue; external name 'data';
    FLastEventID: String; external name 'lastEventID';
    FOrigin: String;  external name 'origin';
    FPorts: TJSMessagePortDynArray; external name 'ports';
  Public
    Property Data : JSValue Read FData;
    Property LastEventID : String Read FLastEventID;
    Property Origin : String Read FOrigin;
    Property Ports : TJSMessagePortDynArray Read FPorts;
  end;

  { TJSExtendableMessageEvent }

  TJSExtendableMessageEvent = class external name 'ExtendableMessageEvent' (TJSExtendableEvent)
  private
    FData: JSValue; external name 'data';
    FLastEventID: String; external name 'lastEventId';
    FOrigin: String; external name 'origin';
    FPorts: TJSMessagePortDynArray; external name 'ports';
    FSource: TJSObject; external name 'source';
    FSourceClient: TJSClient; external name 'source';
    FSourcePort: TJSMessagePort; external name 'source';
    FSourceServiceWorker: TJSServiceWorker; external name 'source';
  Public
    Property Data : JSValue Read FData;
    Property LastEventID : String Read FLastEventID;
    Property Origin : String Read FOrigin;
    Property Ports : TJSMessagePortDynArray Read FPorts;
    Property Source : TJSObject Read FSource;
    // Possible types for Source
    Property SourceServiceWorker : TJSServiceWorker Read FSourceServiceWorker;
    Property SourcePort : TJSMessagePort Read FSourcePort;
    Property SourceClient : TJSClient Read FSourceClient;
  end;


  { TJSClient }

  TJSClient = class external name 'Client' (TJSObject)
  private
    FFrameType: String; external name 'frameType';
    FID: String; external name 'id';
    FType: String; external name 'type';
    FURL: String; external name 'url';
  Public
    procedure postMessage(aValue : JSValue);
    procedure postMessage(aValue : JSValue; aList : TJSValueDynArray);
    Property Id : String Read FID;
    Property Type_ : String Read FType;
    Property FrameType : String Read FFrameType;
    Property URL : String Read FURL;
  end;


{ ----------------------------------------------------------------------
  Fetch & Streams
  ----------------------------------------------------------------------}


  TJSStructuredSerializeOptions = class external name 'Object' (TJSObject)
    transfer : TJSValueDynArray;
  end;

  TJSReadableStreamDefaultReader = class external name 'ReadableStreamDefaultReader' (TJSObject)
   private
     fclosed: TJSPromise; external name 'closed';
   public
     property closed: TJSPromise read fclosed;
     constructor new(stream: TJSReadableStream);
     function cancel(): TJSPromise; overload;
     function cancel(reason: string): TJSPromise; overload;
     function read(): TJSPromise;
     function releaseLock(): TJSPromise;
   end;


  TJSReadableStream = class external name 'ReadableStream' (TJSObject)
  private
    flocked: Boolean; external name 'locked';
  public
    property locked: Boolean read flocked;
    constructor new(underlyingSource: TJSObject);
    constructor new(underlyingSource, queueingStrategy: TJSObject);
    function cancel(reason: String): TJSPromise;
    function getReader(): TJSReadableStreamDefaultReader; overload;
    function getReader(mode: TJSObject): TJSReadableStreamDefaultReader; overload;
    function pipeThrough(transformStream: TJSObject): TJSReadableStream; overload;
    function pipeThrough(transformStream, options: TJSObject): TJSReadableStream; overload;
    function pipeTo(destination: TJSObject): TJSPromise; overload;
    function pipeTo(destination, options: TJSObject): TJSPromise; overload;
    function tee(): TJSArray; // array containing two TJSReadableStream instances
  end;

  TJSWritableStream = class external name 'WritableStream' (TJSObject)
  private
    FLocked: Boolean; external name 'locked';
  public
    function abort(reason: String): TJSPromise;
    function close: TJSPromise;
    function getWriter: TJSObject;
    property locked: Boolean read FLocked;
  end;


  TJSBlob = class external name 'Blob' (TJSEventTarget)
  private
    FSize: NativeInt; external name 'size';
    FType: string; external name  'type';
  Public
    procedure close;
    function slice : TJSBlob; overload;
    function slice(aStart : NativeInt) : TJSBlob; overload;
    function slice(aStart,aEnd : NativeInt) : TJSBlob; overload;
    function slice(aStart,aEnd : NativeInt; AContentType : String) : TJSBlob; overload;
    function arrayBuffer : TJSPromise;
    property size : NativeInt read FSize;
    property _type : string read FType; deprecated;
    property type_ : string read FType;
  end;

  TJSBody = class external name 'Body' (TJSObject)
  private
    fbody: TJSReadableStream; external name 'body';
    fbodyUsed: Boolean; external name 'bodyUsed';
  public
    property body: TJSReadableStream read fbody;
    property bodyUsed: Boolean read fbodyUsed;
    function arrayBuffer(): TJSPromise; // resolves to TJSArrayBuffer
    function blobPromise(): TJSPromise; // resolves to TJSBlob
    function blob: TJSBlob; {$IFNDEF SkipAsync}async;{$ENDIF}
    function json(): TJSPromise; // resolves to JSON / TJSValue
    //function text(): TJSPromise; // resolves to USVString, always decoded using UTF-8
    function text(): string; {$IFNDEF SkipAsync}async;{$ENDIF}
  end;

  Theader = Array [0..1] of String;
  THeaderArray = Array of Theader;

  TJSHTMLHeaders = Class external name 'Headers' (TJSObject)
  Public
    constructor new(values : THeaderArray); overload;
    procedure append(aName, aValue : String);
    procedure delete(aName : String);
    function entries : TJSIterator;
    Function get(aName: String): string;  // string, but can be Null. Only use after Has returned true.
    function getRaw(const aName : string): JSValue; external name 'get'; // can return null
    Function has(aName: String): Boolean;
    function keys : TJSIterator; reintroduce;
    function values : TJSIterator; reintroduce;
    procedure set_(aName, aValue : String);
    Property Headers[aName : string] : string Read Get Write Set_;
  end;

  TJSResponseInit = class external name 'Object'
    status : Integer;
    statusText : String;
    headersObj : TJSObject;
    headers : TJSHTMLHeaders;
  end;

  TJSResponse = class external name 'Response' (TJSBody)
  private
    fheaders: TJSHTMLHeaders; external name 'headers';
    fok: Boolean; external name 'ok';
    fredirected: Boolean; external name 'redirected';
    fstatus: NativeInt; external name 'status';
    fstatusText: String; external name 'statusText';
    ftype: String; external name 'type';
    furl: String; external name 'url';
    fuseFinalUrl: Boolean; external name 'useFinalUrl';
  public
    constructor new(body: TJSObject); overload; external name 'new';
    constructor new(body: TJSObject; init: TJSObject); overload; external name 'new'; deprecated;
    constructor new(body: TJSObject; init: TJSResponseInit); overload; external name 'new';
    constructor new(Msg: string); overload; external name 'new';
    constructor new(Msg: string; init: TJSObject); overload; external name 'new';   deprecated;
    constructor new(Msg: string; init: TJSResponseInit); overload; external name 'new';

    function clone(): TJSResponse;
    function error(): TJSResponse;
    function redirect(url: String; Status: NativeInt): TJSResponse;
    property headers: TJSHTMLHeaders read fheaders; //
    property ok: Boolean read fok;
    property redirected: Boolean read fredirected;
    property status: NativeInt read fstatus;
    property statusText: String read fstatusText; //
    property type_: String read ftype; //
    property url: String read furl; //
    property useFinalUrl: Boolean read fuseFinalUrl write fuseFinalUrl;
  end;

  TJSFormData = class external name 'FormData' (TJSObject)
  public
    procedure append(const aName, aValue: string); overload;
    procedure append(const aName: string; const aValue: TJSBlob); overload;
    procedure append(const aName: string; const aValue: TJSBlob; const aFileName: string); overload;
    procedure delete(const aName: string);
    function entries : TJSIterator;
    function get(const aName : string): JSValue; //string or TJSFile
    function getAll: TJSArray; // of FormDataEntryValue(string or TJSFile)
    function has(const aName : string): Boolean;
    function keys: TJSIterator; reintroduce;
    procedure set_(const aName, aValue: string); overload; external name 'set';
    procedure set_(const aName, aValue, aFileName: string); overload; external name 'set';
    procedure set_(const aName: string; const aValue: TJSBlob); overload; external name 'set';
    procedure set_(const aName: string; const aValue: TJSBlob; const aFileName: string); overload; external name 'set';
    function values: TJSIterator; reintroduce;
  end;

  { TJSRequest }

  TJSRequest = class external name 'Request' (TJSObject)
  private
    FBody: TJSReadableStream; external name 'body';
    FBodyUsed: Boolean; external name 'bodyUsed';
    FCache: String; external name 'cache';
    FCredentials: TJSObject; external name 'credentials';
    FDestination: String; external name 'destination';
    FHeaders: TJSHTMLHeaders; external name 'headers';
    FIntegrity: String; external name 'integrity';
    FMethod: String; external name 'method';
    FMode: String; external name 'mode';
    FReferrer: string; external name 'referrer';
    FReferrerPolicy: string; external name 'referrerPolicy';
    FURL: String;external name 'url';
  Public
    constructor new(aInput: string); overload;
    constructor new(aInput: string; aOptions: TJSObject); overload;
    function arrayBuffer: TJSPromise; // TJSArrayBuffer
    function blob: TJSPromise; // TJSBlob
    function clone: TJSRequest;
    function formData: TJSPromise; // TJSFormData
    function json: TJSPromise; // TJSJSON
    function text: TJSPromise; // string
    Property body : TJSReadableStream Read FBody;
    property bodyUsed : Boolean Read FBodyUsed;
    Property Cache : String Read FCache;
    Property Credentials : TJSObject Read FCredentials;
    Property Destination : String Read FDestination;
    Property Headers : TJSHTMLHeaders Read FHeaders;
    Property Integrity : String Read FIntegrity;
    Property Method : String Read FMethod;
    Property Mode : String Read FMode;
    Property Referrer : string Read FReferrer;
    Property ReferrerPolicy : string Read FReferrerPolicy;
    Property URL : String Read FURL;
  end;
  TJSRequestDynArray = array of TJSRequest;

  { TJSFetchEvent }

  TJSFetchEvent = class external name 'FetchEvent' (TJSExtendableEvent)
  private
    FClientID: String; external name 'clientId';
    FReplacesClientID: String; external name 'replacesClientId';
    FRequest: TJSRequest; external name 'request';
    FResultingClientID: String; external name 'resultingClientId';
    FPreloadResponse: TJSPromise; external name 'preloadResponse';
  Public
    Procedure respondWith(aPromise : TJSPromise);
    Procedure respondWith(aResponse : TJSResponse);
    Property ClientId : String Read FClientID;
    Property PreloadResponse : TJSPromise Read FPreloadResponse;
    Property ReplacesClientID : String Read FReplacesClientID;
    Property ResultingClientID : String Read FResultingClientID;
    Property request : TJSRequest Read FRequest;
  end;

{ ----------------------------------------------------------------------
  IndexedDB
  ----------------------------------------------------------------------}


  TJSIDBTransactionMode = class
  const
    readonly = 'readonly';
    readwrite = 'readwrite';
    versionchange = 'versionchange';
  end;


  { TJSIDBTransaction }

  TJSIDBTransaction = class external name 'IDBTransaction'  (TJSEventTarget)
  private
    FDB : TIDBDatabase; external name 'db';
    FError: JSValue; external name 'error';
    FMode: String; external name 'mode';
    FObjectStoreNames: TStringDynArray; external name 'objectStoreNames';
  public
    procedure abort;
    function objectStore(aName : String) : TJSIDBObjectStore;
    property db : TIDBDatabase read FDB;
    property mode : String read FMode;
    property objectStoreNames : TStringDynArray read FObjectStoreNames;
    property error : JSValue read FError;
  end;


  { TJSIDBKeyRange }

  TJSIDBKeyRange = class external name 'IDBKeyRange'  (TJSObject)
  private
    FLower: JSValue;
    FLowerOpen: Boolean;
    FUpper: JSValue;
    FUpperOpen: Boolean;
  Public
    Class Function bound(aLower,aUpper : JSValue) : TJSIDBKeyRange; overload;
    Class Function bound(aLower,aUpper : JSValue; aLowerOpen : Boolean) : TJSIDBKeyRange; overload;
    Class Function bound(aLower,aUpper : JSValue; aLowerOpen,aUpperOpen : Boolean) : TJSIDBKeyRange; overload;
    Class Function lowerBound(aLower : JSValue) : TJSIDBKeyRange; overload;
    Class Function lowerBound(aLower : JSValue; aOpen: Boolean) : TJSIDBKeyRange; overload;
    Class Function only(aValue : JSValue) : TJSIDBKeyRange;
    Class Function upperBound(aUpper : JSValue) : TJSIDBKeyRange; overload;
    Class Function upperBound(aUpper : JSValue; aOpen: Boolean) : TJSIDBKeyRange; overload;
    function includes (aValue : JSValue) : Boolean;
    property lower : JSValue read FLower;
    property lowerOpen : Boolean read FLowerOpen;
    property upper : JSValue read FUpper;
    property upperOpen : Boolean read FUpperOpen;
  end;

  TJSIDBIndexParameters = record
    unique : boolean;
    multiEntry : boolean;
    locale : string;
  end;


  { TJSIDBIndex }

  TJSIDBIndex = class external name 'IDBIndex'  (TJSObject)
  private
    FKeyPath: JSValue; external name 'keyPath';
    FMultiEntry: Boolean; external name 'multiEntry';
    FObjectStore: TJSIDBObjectStore; external name 'objectStore';
    FUnique: boolean; external name 'unique';
  public
    name : string;
    function count : TJSIDBRequest;
    function get(aKey : jsValue) : TJSIDBRequest; overload;
    function get(aKey : TJSIDBKeyRange) : TJSIDBRequest; overload;
    function getAll(aKey : jsValue) : TJSIDBRequest; overload;
    function getAll(aKey : TJSIDBKeyRange) : TJSIDBRequest; overload;
    function getAll(aKey : jsValue; ACount : NativeInt) : TJSIDBRequest; overload;
    function getAll(aKey : TJSIDBKeyRange; ACount : NativeInt) : TJSIDBRequest; overload;
    function getAllKeys(aKey : jsValue) : TJSIDBRequest; overload;
    function getAllKeys(aKey : TJSIDBKeyRange) : TJSIDBRequest; overload;
    function getAllKeys(aKey : jsValue; ACount : NativeInt) : TJSIDBRequest; overload;
    function getAllKeys(aKey : TJSIDBKeyRange; ACount : NativeInt) : TJSIDBRequest; overload;
    function getKey(aKey : jsValue) : TJSIDBRequest;
    function openCursor : TJSIDBRequest; overload;
    function openCursor(aKeyRange : TJSIDBKeyRange) : TJSIDBRequest; overload;
    function openCursor(aKeyRange : TJSIDBKeyRange; ADirection : String) : TJSIDBRequest;overload;
    function openKeyCursor : TJSIDBRequest;overload;
    function openKeyCursor(aKeyRange : TJSIDBKeyRange) : TJSIDBRequest;overload;
    function openKeyCursor(aKeyRange : TJSIDBKeyRange; ADirection : String) : TJSIDBRequest;overload;
    Property keyPath : JSValue Read FKeyPath;
    property multiEntry : Boolean read FMultiEntry;
    property objectStore : TJSIDBObjectStore read FObjectStore;
    property unique : boolean read FUnique;
  end;

  TJSIDBCursorDirection = class external name 'IDBCursorDirection'  (TJSObject)
  Const
    next = 'next';
    nextUnique = 'nextUnique';
    prev = 'prev';
    prevUnique = 'prevUnique';
  end;


  { TJSIDBCursor }

  TJSIDBCursor = class external name 'IDBCursor'  (TJSObject)
  private
    FDirection: string; external name 'direction';
    FKey: JSValue; external name 'key';
    FValue : JSValue; external name 'value';
    FPrimaryKey: JSValue; external name 'primaryKey';
    FSource: JSValue; external name 'source';
    FSourceAsIndex: TJSIDBIndex; external name 'source';
    FSourceAsStore: TJSIDBObjectStore; external name 'source';
  Public
    procedure advance(aCount : NativeInt); overload;
    procedure advance(aKey : JSValue); overload;
    procedure continue(aKey : JSValue); overload;
    procedure continue; overload;
    procedure continuePrimaryKey(aKey : JSValue); overload;
    procedure continuePrimaryKey(aKey,aPrimaryKey : JSValue); overload;
    procedure delete;
    procedure update(aValue : JSValue);
    property source : JSValue read FSource;
    property sourceAsStore : TJSIDBObjectStore read FSourceAsStore;
    property sourceAsIndex : TJSIDBIndex read FSourceAsIndex;
    property key : JSValue read FKey;
    Property Value : JSValue Read FValue;
    property primaryKey : JSValue read FPrimaryKey;
    property direction : string read FDirection;
  end;

  TJSIDBObjectStore = class external name 'IDBObjectStore'  (TJSEventTarget)
  public
    function add(aValue : JSValue; aKey : String) : TJSIDBRequest;
    function add(aValue : JSValue) : TJSIDBRequest;
    function clear : TJSIDBRequest;
    function delete(aKey : string) : TJSIDBRequest;
    function delete(aKeyRange : TJSIDBKeyRange) : TJSIDBRequest;
    function get(aKey : string) : TJSIDBRequest; overload;
    function get(aKeyRange : TJSIDBKeyRange) : TJSIDBRequest; overload;
    function getKey(aKey : string) : TJSIDBRequest; overload;
    function getKey(aKeyRange : TJSIDBKeyRange) : TJSIDBRequest; overload;
    function getAll : TJSIDBRequest; overload;
    function getAll(aKey : String) : TJSIDBRequest; overload;
    function getAll(aKeyRange : TJSIDBKeyRange) : TJSIDBRequest; overload;
    function getAll(aKey : String; aCount: NativeInt) : TJSIDBRequest; overload;
    function getAll(aKeyRange : TJSIDBKeyRange; aCount: NativeInt) : TJSIDBRequest; overload;
    function getAllKeys(aKey : String) : TJSIDBRequest; overload;
    function getAllKeys(aKeyRange : TJSIDBKeyRange) : TJSIDBRequest; overload;
    function getAllKeys(aKey : String; aCount: NativeInt) : TJSIDBRequest; overload;
    function getAllKeys(aKeyRange : TJSIDBKeyRange; aCount: NativeInt) : TJSIDBRequest; overload;
    function createIndex (aIndexName : String; KeyPath : String)  : TJSIDBIndex; overload;
    function createIndex (aIndexName : String; KeyPath : String; Options : TJSIDBIndexParameters)  : TJSIDBIndex; overload;
    function createIndex (aIndexName : String; KeyPath : Array of String)  : TJSIDBIndex; overload;
    function createIndex (aIndexName : String; KeyPath : Array of String; Options : TJSIDBIndexParameters)  : TJSIDBIndex; overload;
    Procedure deleteIndex (aIndexName : String);
    function index (aIndexName : String)  : TJSIDBIndex;
    function put(aValue : JSValue; aKey : String) : TJSIDBRequest; overload;
    function put(aValue : JSValue) : TJSIDBRequest; overload;
    function openCursor : TJSIDBRequest; overload;
    function openCursor(aKey : String) : TJSIDBRequest; overload;
    function openCursor(aKeyRange : TJSIDBKeyRange) : TJSIDBRequest; overload;
    function openCursor(aKey : String; aDirection : string) : TJSIDBRequest; overload;
    function openCursor(aKeyRange : TJSIDBKeyRange; aDirection : string) : TJSIDBRequest; overload;
    function openKeyCursor : TJSIDBRequest; overload;
    function openKeyCursor(aKey : String) : TJSIDBRequest; overload;
    function openKeyCursor(aKeyRange : TJSIDBKeyRange) : TJSIDBRequest; overload;
    function openKeyCursor(aKey : String; aDirection : string) : TJSIDBRequest; overload;
    function openKeyCursor(aKeyRange : TJSIDBKeyRange; aDirection : string) : TJSIDBRequest; overload;
    function count : TJSIDBRequest; overload;
    function count(aKey : String) : TJSIDBRequest; overload;
    function count(aKeyRange : TJSIDBKeyRange) : TJSIDBRequest; overload;
    property Indexes [aIndexName : String] : TJSIDBIndex read index;
  end;

  { TJSIDBRequest }

  TJSIDBRequest = class external name 'IDBRequest'  (TJSEventTarget)
  private
    Ferror : JSValue; external name 'error'; // standards are not quite clear on this one
    FReadyState: string; external name 'readyState';
    FResult: JSValue; external name 'result';
    FResultDatabase: TIDBDatabase; external name 'result';
    FResultIndex: TJSIDBIndex; external name 'result';
    FResultObjectStore : TJSIDBObjectStore; external name 'result';
    FResultCursor : TJSIDBCursor; external name 'result';
    FSourceDatabase: TIDBDatabase; external name 'source';
    FSourceIndex: TJSIDBIndex; external name 'source';
    FSourceObjectStore : TJSIDBObjectStore; external name 'source';
    FSourceCursor : TJSIDBCursor; external name 'source';
    FSource: JSValue; external name 'source';
    FTransaction: TJSIDBTransaction; external name 'transaction';
  Public
    onerror : TJSEventHandler;
    onsuccess : TJSEventHandler;
    Property error : JSValue read FError;
    property readyState : string read FReadyState;

    property result : JSValue read FResult;
    property resultAsObjectStore : TJSIDBObjectStore read FResultObjectStore;
    property resultAsCursor : TJSIDBCursor read FResultCursor;
    property resultAsIndex : TJSIDBIndex read FResultIndex;
    property resultAsDatabase : TIDBDatabase read FResultDatabase;

    property source : JSValue read FSource;
    property sourceAsObjectStore : TJSIDBObjectStore read FSourceObjectStore;
    property sourceAsCursor : TJSIDBCursor read FSourceCursor;
    property sourceAsIndex : TJSIDBIndex read FSourceIndex;
    property sourceAsDatabase : TIDBDatabase read FSourceDatabase;

    property transaction : TJSIDBTransaction read FTransaction;
  end;

  TJSIDBOpenDBRequest = class external name 'IDBOpenDBRequest' (TJSIDBRequest)
  Public
    onblocked : TJSEventHandler;
    onupgradeneeded : TJSEventHandler;
  end;

  TJSCreateObjectStoreOptions = record
    keyPath : jsValue;
    autoIncrement : boolean;
  end;

  { TIDBDatabase }

  TIDBDatabase = class external name 'IDBDatabase' (TJSEventTarget)
  private
    FName: string; external name 'name';
    FobjectStoreNames: TStringDynArray; external name 'objectStoreNames';
    FVersion: integer; external name 'version';
  public
    procedure close;
    function createObjectStore(aName : string) : TJSIDBObjectStore; overload;
    function createObjectStore(aName : string; Options: TJSCreateObjectStoreOptions) : TJSIDBObjectStore; overload;
    procedure deleteObjectStore(aName : string);
    function transaction(aStoreNames : array of string) : TJSIDBTransaction; overload;
    function transaction(aStoreNames : array of string; aMode : string) : TJSIDBTransaction; overload;
    property name : string read FName;
    property version : integer read FVersion;
    property objectStoreNames : TStringDynArray read FobjectStoreNames;
  end;

  TJSIDBFactory = class external name 'IDBFactory' (TJSEventTarget)
  public
    function open(aName : string) : TJSIDBOpenDBRequest;
    function open(aName : string; aVersion : Integer) : TJSIDBOpenDBRequest;
    function deleteDatabase(aName : string) : TJSIDBOpenDBRequest;
    function cmp (a,b : jsValue) : NativeInt;
  end;

{ ----------------------------------------------------------------------
  Cache
  ----------------------------------------------------------------------}


  TJSCacheDeleteOptions = class external name 'Object' (TJSObject)
    ignoreSearch : Boolean;
    ignoreMethod : Boolean;
    ignoreVary : Boolean;
    cacheName : string;
  end;

  TJSParamEnumCallBack = reference to procedure (const aKey,aValue : string);

  TJSURLSearchParams = class external name 'URLSearchParams' (TJSObject)
  Public
    constructor new(aQuery : String);
    Procedure append(const aName,aValue : string);
    Procedure delete(const aName : string);
    Function entries : TJSIterator;
    Procedure foreach(aEnumCallBack : TJSParamEnumCallBack);
    function get(const aName : string) : JSValue;
    // If you're sure the value exists...
    function getString(const aName : string) : string; external name 'get';
    function getAll(const aName : string) : TStringDynArray;
    function has(const aName : string) : Boolean;
    Function keys : TJSIterator; reintroduce;
    Procedure set_(const aName,aValue : string); external name 'set';
    Procedure sort;
    Function values : TJSIterator; reintroduce;
  end;

  TJSURL = class external name 'URL' (TJSObject)
  Private
    FOrigin : String; external name 'origin';
    FSearchParams : TJSURLSearchParams; external name 'searchParams';
  public
    hash : string;
    host : string;
    hostname : string;
    href : string;
    password : string;
    pathname : string;
    port : string;
    protocol : string;
    search : string;
    username : string;
    constructor new(aURL : String);
    constructor new(aURL,aBase : String);
    class function createObjectURL(const v: JSValue): string;
    class function revokeObjectURL(const S : String): string;
    function toJSON : String;
    Property Origin : String Read FOrigin;
    property SearchParams : TJSURLSearchParams read FSearchParams;
  end;
  TJSURLDynArray = array of TJSURL;


  { TJSNavigationPreloadState }

  TJSNavigationPreloadState = class external name 'navigationPreloadState'
  public
    enabled: boolean;
    headerValue: string;
  end;


  { TJSCache }

  TJSCache = class external name 'Cache' (TJSObject)
  Public
    Function add(aRequest : String) : TJSPromise;
    Function add(aRequest : TJSURL) : TJSPromise;
    Function addAll(aRequests : TJSStringDynArray) : TJSPromise;
    Function addAll(aRequests : TJSURLDynArray) : TJSPromise;
    Function addAll(aRequests : TJSValueDynArray) : TJSPromise;
    Function put(aRequest : String; aResponse : TJSResponse) : TJSPromise;
    Function put(aRequest : TJSRequest; aResponse : TJSResponse) : TJSPromise;
    Function delete(aRequest : String) : TJSPromise;
    Function delete(aRequest : TJSRequest) : TJSPromise;
    Function delete(aRequest : String; aOptions : TJSObject) : TJSPromise;
    Function delete(aRequest : TJSRequest; aOptions : TJSObject) : TJSPromise;
    Function delete(aRequest : String; aOptions : TJSCacheDeleteOptions) : TJSPromise;
    Function delete(aRequest : TJSRequest; aOptions : TJSCacheDeleteOptions) : TJSPromise;
    Function keys : TJSPromise; reintroduce;
    Function match(aRequest : String): TJSPromise;
    Function match(aRequest : TJSRequest): TJSPromise;
    Function matchAll(aRequest : TJSStringDynArray): TJSPromise;
    Function matchAll(aRequest : TJSRequestDynArray): TJSPromise;
    Function matchAll(aRequests : TJSValueDynArray) : TJSPromise;
  end;

  TJSCacheStorage = class external name 'CacheStorage' (TJSObject)
  Public
    function delete(aName : string) : TJSPromise; // resolves to boolean
    function has(aName : string) : TJSPromise;
    Function keys : TJSPromise; reintroduce;
    Function match(aRequest : String): TJSPromise;
    Function match(aRequest : TJSRequest): TJSPromise;
    function open(aName : string) : TJSPromise;
  end;


{ ----------------------------------------------------------------------
  Crypto
  ----------------------------------------------------------------------}

  { Basic types }

  TJSCryptoAlgorithmIdentifier = JSValue;
  TJSCryptoNamedCurve = JSValue;
  TJSCryptoBigInteger = TJSUint8Array;
  TJSCryptoKeyUsage = string;
  TJSCryptoKeyType = string;
  TJSCryptoKeyFormat = string;

  { Algorithm }

  TJSCryptoAlgorithm = record
    name : String;
  end;

  { AesCbcParams }

  TJSCryptoAesCbcParams = record
    iv : TJSBufferSource;
  end;

  { AesCtrParams }

  TJSCryptoAesCtrParams = record
    counter : TJSBufferSource;
    length_ : Byte;external name 'length';
  end;

  { AesGcmParams }

  TJSCryptoAesGcmParams = record
    iv : TJSBufferSource;
    additionalData : TJSBufferSource;
    tagLength : Byte;
  end;

  { HmacImportParams }

  TJSCryptoHmacImportParams = record
    hash : TJSCryptoAlgorithmIdentifier;
  end;

  { Pbkdf2Params }

  TJSCryptoPbkdf2Params = record
    salt : TJSBufferSource;
    iterations : NativeInt;
    hash : TJSCryptoAlgorithmIdentifier;
  end;

  { RsaHashedImportParams }

  TJSCryptoRsaHashedImportParams = record
    hash : TJSCryptoAlgorithmIdentifier;
  end;

  { AesKeyGenParams }

  TJSCryptoAesKeyGenParams = record
    length_ : Integer;external name 'length';
  end;

  { HmacKeyGenParams }

  TJSCryptoHmacKeyGenParams = record
    hash : TJSCryptoAlgorithmIdentifier;
    length_ : Integer;external name 'length';
  end;

  { RsaHashedKeyGenParams }

  TJSCryptoRsaHashedKeyGenParams = record
    modulusLength : Integer;
    publicExponent : TJSCryptoBigInteger;
    hash : TJSCryptoAlgorithmIdentifier;
  end;

  { RsaOaepParams }

  TJSCryptoRsaOaepParams = record
    label_ : TJSBufferSource;external name 'label';
  end;

  { RsaPssParams }

  TJSCryptoRsaPssParams = record
    saltLength : Integer;
  end;

  { DhKeyGenParams }

  TJSCryptoDhKeyGenParams = record
    prime : TJSCryptoBigInteger;
    generator : TJSCryptoBigInteger;
  end;

  { EcKeyGenParams }

  TJSCryptoEcKeyGenParams = record
    _namedCurve : TJSCryptoNamedCurve;external name 'namedCurve';
  end;

  { AesDerivedKeyParams }

  TJSCryptoAesDerivedKeyParams = record
    length_ : Integer;external name 'length';
  end;

  { HmacDerivedKeyParams }

  TJSCryptoHmacDerivedKeyParams = record
    length_ : Integer;external name 'length';
  end;

  { EcdhKeyDeriveParams }

  TJSCryptoEcdhKeyDeriveParams = record
    public_ : TJSCryptoKey; external name 'public';
  end;

  { DhKeyDeriveParams }

  TJSCryptoDhKeyDeriveParams = record
    public_ : TJSCryptoKey;  external name 'public';
  end;

  { DhImportKeyParams }

  TJSCryptoDhImportKeyParams = record
    prime : TJSCryptoBigInteger;
    generator : TJSCryptoBigInteger;
  end;

  { EcdsaParams }

  TJSCryptoEcdsaParams = record
    hash : TJSCryptoAlgorithmIdentifier;
  end;

  { EcKeyImportParams }

  TJSCryptoEcKeyImportParams = record
    _namedCurve : TJSCryptoNamedCurve;external name 'namedCurve';
  end;

  { HkdfParams  }

  TJSCryptoHkdfParams = record
    hash : TJSCryptoAlgorithmIdentifier;
    salt : TJSBufferSource;
    info : TJSBufferSource;
  end;

  { RsaOtherPrimesInfo }

  TJSCryptoRsaOtherPrimesInfo = record
    r : String;
    d : String;
    t : String;
  end;

  { JsonWebKey }

  TJSCryptoRsaOtherPrimesInfoDynArray = Array of TJSCryptoRsaOtherPrimesInfo;
  TJSCryptoJsonWebKey = record
    kty : String;
    use : String;
    key_ops : TStringDynArray;
    alg : String;
    ext : boolean;
    crv : String;
    x : String;
    y : String;
    d : String;
    n : String;
    e : String;
    p : String;
    q : String;
    dp : String;
    dq : String;
    qi : String;
    oth : TJSCryptoRsaOtherPrimesInfoDynArray;
    k : String;
  end;

  { CryptoKeyPair }

  TJSCryptoKeyPair = record
    publicKey : TJSCryptoKey;
    privateKey : TJSCryptoKey;
  end;

  { TJSCryptoKey }

  TJSCryptoKeyUsageDynArray = Array of TJSCryptoKeyUsage;
  TJSCryptoKey = class external name 'CryptoKey'
  Private
    Ftype_ : TJSCryptoKeyType; external name 'type';
    Fextractable : boolean; external name 'extractable';
    Falgorithm : TJSObject; external name 'algorithm';
    Fusages : TJSCryptoKeyUsageDynArray; external name 'usages';
  Public
    Property type_ : TJSCryptoKeyType Read Ftype_;
    Property extractable : boolean Read Fextractable;
    Property algorithm : TJSObject Read Falgorithm;
    Property usages : TJSCryptoKeyUsageDynArray Read Fusages;
  end;

  { TJSSubtleCrypto }

  TJSSubtleCrypto = class external name 'SubtleCrypto'
  Private
  Public
    function encrypt(algorithm :  TJSCryptoAlgorithmIdentifier; key : TJSCryptoKey; data : TJSBufferSource): TJSPromise;
    function decrypt(algorithm : TJSCryptoAlgorithmIdentifier; key : TJSCryptoKey; data : TJSBufferSource): TJSPromise;
    function sign(algorithm : TJSCryptoAlgorithmIdentifier; key : TJSCryptoKey; data : TJSBufferSource): TJSPromise;
    function verify(algorithm : TJSCryptoAlgorithmIdentifier; key : TJSCryptoKey; signature : TJSBufferSource; data : TJSBufferSource): TJSPromise;
    function digest(algorithm : TJSCryptoAlgorithmIdentifier; data : TJSBufferSource): TJSPromise;
    function generateKey(algorithm : TJSCryptoAlgorithmIdentifier; extractable : boolean; keyUsages : TJSCryptoKeyUsageDynArray): TJSPromise;
    function deriveKey(algorithm : TJSCryptoAlgorithmIdentifier; baseKey : TJSCryptoKey; derivedKeyType : TJSCryptoAlgorithmIdentifier; extractable : boolean; keyUsages : TJSCryptoKeyUsageDynArray): TJSPromise;
    function deriveBits(algorithm : TJSCryptoAlgorithmIdentifier; baseKey : TJSCryptoKey; length_ : NativeInt): TJSPromise;
    function importKey(format : TJSCryptoKeyFormat; keyData : TJSObject; algorithm : TJSCryptoAlgorithmIdentifier; extractable : boolean; keyUsages : TJSCryptoKeyUsageDynArray): TJSPromise;
    function exportKey(format : TJSCryptoKeyFormat; key : TJSCryptoKey): TJSPromise;
    function wrapKey(format : TJSCryptoKeyFormat; key : TJSCryptoKey; wrappingKey : TJSCryptoKey; wrapAlgorithm : TJSCryptoAlgorithmIdentifier): TJSPromise;
    function unwrapKey(format : TJSCryptoKeyFormat; wrappedKey : TJSBufferSource; unwrappingKey : TJSCryptoKey; unwrapAlgorithm : TJSCryptoAlgorithmIdentifier; unwrappedKeyAlgorithm : TJSCryptoAlgorithmIdentifier; extractable : boolean; keyUsages : TJSCryptoKeyUsageDynArray): TJSPromise;
  end;

  { TJSCrypto }

  TJSCrypto = class external name 'Crypto'  (TJSObject)
  private
    Fsubtle: TJSSubtleCrypto; external name 'subtle';
  Public
    procedure getRandomValues (anArray : TJSTypedArray);
    property subtle : TJSSubtleCrypto Read Fsubtle;
  end;

  TJSEventSourceOptions = class external name 'Object' (TJSObject)
    withCredentials: boolean;
  end;

  TJSEventSource = class external name 'EventSource' (TJSEventTarget)
  Private
    FReadyState : Integer; external name 'readyState';
    fURL : String; external name 'url';
    fwithCredentials : Boolean; external name 'withCredentials';
  Public
    constructor new(aURL : String);
    constructor new(aURL : String; options: TJSEventSourceOptions);
    procedure close;
    property readyState : Integer Read FReadyState;
    property url : String Read fURL;
    property withCredentials: boolean Read FwithCredentials;
  end;


  { ----------------------------------------------------------------------
    Service Worker
    ----------------------------------------------------------------------}

  { TJSNavigationPreload }

  TJSNavigationPreload = class external name 'navigationPreload' (TJSObject)
  public
    function enable: boolean; async;
    function disable: boolean; async;
    function setHeaderValue(Value: string): TJSPromise;
    function getState: TJSNavigationPreloadState; async;
  end;


  TJSWorker = class external name 'Worker' (TJSEventTarget)
  public
    constructor new(aURL : string);
    procedure postMessage(aValue : JSValue);
    procedure postMessage(aValue : JSValue; aList : TJSValueDynArray);
  end;


  { TJSServiceWorkerRegistration }

  TJSServiceWorkerRegistration = class external name 'ServiceWorkerRegistration'  (TJSObject)
  private
    FActive: TJSServiceWorker; external name 'active';
    FInstalling: TJSServiceWorker; external name 'installing';
    FScope: string; external name 'scope';
    FWaiting: TJSServiceWorker; external name 'waiting';
    FNavigationPreload: TJSNavigationPreload; external name 'navigationPreload';
  public
    function unregister : TJSPromise;
    procedure update;
    property Active : TJSServiceWorker read FActive;
    property Scope : string read FScope;
    property Waiting : TJSServiceWorker read FWaiting;
    property Installing : TJSServiceWorker read FInstalling;
    property NavigationPreload: TJSNavigationPreload read FNavigationPreload;
  end;

  { TJSServiceWorker }

  TJSServiceWorker = class external name 'ServiceWorker' (TJSWorker)
  private
    FRegistration: TJSServiceWorkerRegistration; external name 'registration';
    FScriptURL: String;  external name 'scriptURL';
    FState: string;  external name 'state';
  Public
    property State : string read FState;
    property ScriptURL : String Read FscriptURL;
    property Registration: TJSServiceWorkerRegistration read FRegistration;
  end;

  TJSStorageManager = class external name 'StorageManager' (TJSObject)
    function estimate : TJSPromise;
    function persist : TJSPromise;
    function persisted : TJSPromise;
  end;

  TJSMicrotaskProcedure = reference to Procedure;

  TJSImageBitmapOptions = class external name 'Object' (TJSObject)
    imageOrientation : string;
    premultiplyAlpha : string;
    colorSpaceConversion : String;
    resizeWidth : NativeInt;
    resizeHeight : NativeInt;
    resizeQuality : String;
  end;

  { TWindowOrWorkerGlobalScope }

  TWindowOrWorkerGlobalScope = Class external name 'Object' (TJSEventTarget)
  Private
    FCrypto: TJSCrypto; external name 'crypto';
    FisSecureContext : boolean; external name 'isSecureContext';
    FIDBFactory : TJSIDBFactory; external name 'indexedDB';
    fcaches : TJSCacheStorage; external name 'caches';
  Public
    Function setInterval(ahandler : TJSTimerCallBack; aInterval : NativeUInt) : NativeInt; varargs;
    Function setTimeout(ahandler : TJSTimerCallBack; aTimeout : NativeUInt) : NativeInt; varargs;
    Function setTimeout(ahandler : TJSTimerCallBack) : NativeInt;
    Procedure clearInterval(aID: NativeInt);
    Procedure clearTimeout(aID: NativeInt);
    procedure queueMicrotask(callback : TJSMicrotaskProcedure);
    Function createImageBitmap(Source : JSValue) : TJSPromise;
    Function createImageBitmap(Source : JSValue; aOptions : TJSImageBitmapOptions) : TJSPromise;
    Function createImageBitmap(Source : JSValue; sx,sy,sw,sh : NativeInt; aOptions : TJSImageBitmapOptions) : TJSPromise;
    Function structuredClone(value : JSValue) : JSValue;
    Function structuredClone(value : JSValue; aOptions : TJSStructuredSerializeOptions) : JSValue;
    function fetch(resource: String; init: TJSObject): TJSPromise; overload; external name 'fetch';
    //function fetch(resource: String): TJSPromise; overload; external name 'fetch';
    function fetch(resource: String): TJSResponse; {$IFNDEF SkipAsync}async;{$ENDIF} overload; external name 'fetch';
    function fetch(resource: TJSObject; init: TJSObject): TJSPromise; overload; external name 'fetch';
    function fetch(resource: TJSObject): TJSPromise; overload; external name 'fetch';
    property isSecureContext : Boolean Read FisSecureContext;
    property IDBFactory : TJSIDBFactory Read FIDBFactory;
    property caches : TJSCacheStorage read fcaches;
    property crypto : TJSCrypto Read FCrypto;
  end;


var
  Console : TJSConsole; external name 'console';
  Crypto: TJSCrypto; external name 'crypto';
  indexedDB : TJSIDBFactory; external name 'indexedDB';

  function fetch(resource: String; init: TJSObject): TJSPromise; overload; external name 'fetch';
  //function fetch(resource: String): TJSPromise; overload; external name 'fetch';
  function fetch(resource: String): TJSResponse; {$IFNDEF SkipAsync}async;{$ENDIF} overload; external name 'fetch';
  function fetch(resource: TJSObject; init: TJSObject): TJSPromise; overload; external name 'fetch';
  function fetch(resource: TJSObject): TJSPromise; overload; external name 'fetch';

implementation

end.

