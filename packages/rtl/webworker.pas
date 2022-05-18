unit webworker;

{$mode ObjFPC}
{$modeswitch externalclass}

interface

uses
  JS, types, weborworker;

Type

  { TJSWorkerNavigator }

  TJSWorkerNavigator = class external name 'WorkerNavigator' (TJSObject)
  private
    FhardwareConcurrency: Integer; external name 'hardwareConcurrency';
    FLanguage: String; external name 'language';
    FLanguages: TJSStringDynArray; external name 'languages';
    FOnline: boolean; external name 'onLine';
    FPlatform: string; external name 'platform';
    FUserAgent: string; external name 'userAgent';
  public
    property language : String read FLanguage;
    property languages : TJSStringDynArray read FLanguages;
    property onLine : boolean read FOnline;
    property platform : string read FPlatform;
    property userAgent : string read FUserAgent;
    property hardwareConcurrency : Integer Read FhardwareConcurrency;
  end;

  { TJSWorkerLocation }

  TJSWorkerLocation = class external name 'WorkerLocation' (TJSObject)
  Private
    FHash: string;external name 'hash';
    FHost: string;external name 'host';
    FHostName: string;external name 'hostname';
    FHRef: string; external name 'href';
    FOrigin : string; external name 'origin';
    FPathName: string;external name 'pathname';
    FPort: string;external name 'port';
    FProtocol: string;external name 'protocol';
    FSearch: string;external name 'search';
  Public
    Property hash : string Read FHash;
    Property host : string read FHost;
    Property hostname : string read FHostName;
    Property href : string read FHRef;
    Property pathname : string Read FPathName;
    Property port : string Read FPort;
    Property protocol : string Read FProtocol;
    Property search : string read FSearch;
    property origin : string read FOrigin;
  end;

  { TJSWorkerGlobalScope }

  TJSWorkerGlobalScope = class external name 'WorkerGlobalScope' (TWindowOrWorkerGlobalScope)
  private
    FConsole: TJSConsole; external name 'console';
    FLocation: TJSWorkerLocation; external name 'location';
    FNavigator: TJSWorkerNavigator; external name 'navigator';
    FSelf : TJSWorkerGlobalScope external name 'self';
  Public
    procedure importScripts(path : string); varargs;
    property Navigator: TJSWorkerNavigator read FNavigator;
    property console : TJSConsole Read FConsole;
    property location : TJSWorkerLocation Read FLocation;
    Property Self_ : TJSWorkerGlobalScope Read FSelf;
  end;

Var
  Self_ : TJSWorkerGlobalScope; external name 'self';
  location : TJSWorkerLocation;
  console : TJSConsole;
  navigator : TJSWorkerNavigator;
  serviceWorker : TJSServiceWorker;
  caches : TJSCacheStorage;

function fetch(resource: String; init: TJSObject): TJSPromise; overload; external name 'fetch';
//function fetch(resource: String): TJSPromise; overload; external name 'fetch';
function fetch(resource: String): TJSResponse; {$IFNDEF SkipAsync}async;{$ENDIF} overload; external name 'fetch';
function fetch(resource: TJSObject; init: TJSObject): TJSPromise; overload; external name 'fetch';
function fetch(resource: TJSObject): TJSPromise; overload; external name 'fetch';

implementation

end.

