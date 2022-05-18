unit weborworker;

{$mode objfpc}
{$modeswitch externalclass}

interface

uses
  JS, types;

type
  TJSEventTarget = class;
  TIDBDatabase = class;
  TJSIDBObjectStore = class;
  TJSIDBRequest = class;
  TJSServiceWorker = class;

  TJSStorageManager = class external name 'StorageManager' (TJSObject)
    function estimate : TJSPromise;
    function persist : TJSPromise;
    function persisted : TJSPromise;
  end;


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
//    property currentTargetElement : TJSElement Read FCurrentTargetElement;
    property defaultPrevented : Boolean Read FdefaultPrevented;
    property eventPhase : NativeInt Read FEventPhase;
    property target : TJSEventTarget Read FTarget;
//    property targetElement : TJSElement Read FTargetElement;
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


  TJSStructuredSerializeOptions = class external name 'Object' (TJSObject)
    transfer : TJSValueDynArray;
  end;

  TJSReadableStream = class external name 'ReadableStream' (TJSObject)
  private
    flocked: Boolean; external name 'locked';
  public
    property locked: Boolean read flocked;
    constructor new(underlyingSource: TJSObject);
    constructor new(underlyingSource, queueingStrategy: TJSObject);
    function cancel(reason: String): TJSPromise;
    function getReader(): TJSObject; overload;
    function getReader(mode: TJSObject): TJSObject; overload;
    function pipeThrough(transformStream: TJSObject): TJSReadableStream; overload;
    function pipeThrough(transformStream, options: TJSObject): TJSReadableStream; overload;
    function pipeTo(destination: TJSObject): TJSPromise; overload;
    function pipeTo(destination, options: TJSObject): TJSPromise; overload;
    function tee(): TJSArray; // array containing two TJSReadableStream instances
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
    //function blob(): TJSPromise; // resolves to TJSBlob
    function blob: TJSBlob; {$IFNDEF SkipAsync}async;{$ENDIF}
    function json(): TJSPromise; // resolves to JSON / TJSValue
    //function text(): TJSPromise; // resolves to USVString, always decoded using UTF-8
    function text(): string; {$IFNDEF SkipAsync}async;{$ENDIF}
  end;


  TJSResponse = class external name 'Response' (TJSBody)
  private
    fheaders: TJSObject;external name 'headers';
    fok: Boolean; external name 'ok';
    fredirected: Boolean; external name 'redirected';
    fstatus: NativeInt; external name 'status';
    fstatusText: String; external name 'statusText';
    ftype: String; external name 'type';
    furl: String; external name 'url';
    fuseFinalUrl: Boolean; external name 'useFinalUrl';
  public
    property headers: TJSObject read fheaders; //
    property ok: Boolean read fok;
    property redirected: Boolean read fredirected;
    property status: NativeInt read fstatus;
    property statusText: String read fstatusText; //
    property type_: String read ftype; //
    property url: String read furl; //
    property useFinalUrl: Boolean read fuseFinalUrl write fuseFinalUrl;
    constructor new(body: TJSObject; init: TJSObject); overload; varargs; external name 'new';
    constructor new(Msg: string; init: TJSObject); overload; varargs; external name 'new';
    function clone(): TJSResponse;
    function error(): TJSResponse;
    function redirect(url: String; Status: NativeInt): TJSResponse;
  end;




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

  { TJSRequest }

  TJSRequest = class external name 'Request' (TJSObject)
  private
    FBody: TJSReadableStream; external name 'body';
    FBodyUsed: Boolean; external name 'bodyUsed';
    FCache: String; external name 'cache';
    FCredentials: TJSObject; external name 'credentials';
    FDestination: String; external name 'destination';
    FHeaders: TJSObject; external name 'headers';
    FIntegrity: String; external name 'integrity';
    FMethod: String; external name 'method';
    FMode: String; external name 'mode';
    FReferrer: string; external name 'referrer';
    FReferrerPolicy: string; external name 'referrerPolicy';
    FURL: String;external name 'url';
  Public
    Property body : TJSReadableStream Read FBody;
    property bodyUsed : Boolean Read FBodyUsed;
    Property Cache : String Read FCache;
    Property Credentials : TJSObject Read FCredentials;
    Property Destination : String Read FDestination;
    // TODO : actually Headers object
    Property Headers : TJSObject Read FHeaders;
    Property Integrity : String Read FIntegrity;
    Property Method : String Read FMethod;
    Property Mode : String Read FMode;
    Property Referrer : string Read FReferrer;
    Property ReferrerPolicy : string Read FReferrerPolicy;
    Property URL : String Read FURL;
  end;
  TJSRequestDynArray = array of TJSRequest;

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


  { TJSRequest }

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


  TJSMicrotaskProcedure = reference to Procedure;

  TJSImageBitmapOptions = class external name 'Object' (TJSObject)
    imageOrientation : string;
    premultiplyAlpha : string;
    colorSpaceConversion : String;
    resizeWidth : NativeInt;
    resizeHeight : NativeInt;
    resizeQuality : String;
  end;



  TWindowOrWorkerGlobalScope = Class external name 'Object' (TJSObject)
  Private
    FisSecureContext : boolean; external name 'isSecureContext';
    FIDBFactory : TJSIDBFactory; external name 'IDBFactory';
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
  end;




(*


// https://wicg.github.io/scheduling-apis/#ref-for-windoworworkerglobalscope-scheduler
partial interface mixin WindowOrWorkerGlobalScope {
  [Replaceable, Pref="dom.enable_web_task_scheduling", SameObject]
  readonly attribute Scheduler scheduler;
};
*)

implementation

end.

