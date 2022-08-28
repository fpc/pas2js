unit webworker;

{$mode ObjFPC}
{$modeswitch externalclass}

interface

uses
  JS, weborworker;

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

  { TJSDedicatedWorkerGlobalScope }

  TJSDedicatedWorkerGlobalScope = class external name 'DedicatedWorkerGlobalScope' (TJSWorkerGlobalScope)
  private
    FName: String; external name 'name';
  Public
    Procedure close;
    Procedure postMessage(aMessage : JSValue); overload;
    Procedure postMessage(aMessage : JSValue; TransferableObjects : Array of JSValue); overload;
    Property name : String Read FName;
  end;

  TJSClientsMatchAllOptions = class external name 'Object'
    includeUncontrolled : Boolean;
    type_ : string; external name 'type';
  end;

  TJSClients = class external name 'Clients' (TJSObject)
    function claim : TJSPromise;
    function get(ID : String) : TJSPromise;
    function matchAll : TJSPromise;
    function matchAll(Options : TJSClientsMatchAllOptions) : TJSPromise;
    function matchAll(Options : TJSObject) : TJSPromise;
    function openWindow(url : string) : TJSPromise;
  end;

  { TJSServiceworkerGlobalScope }

  TJSServiceworkerGlobalScope = class external name 'ServiceWorkerGlobalScope' (TJSWorkerGlobalScope)
  private
    FClients: TJSClients; external name 'clients';
    FRegistration: TJSServiceWorkerRegistration; external name 'registration';
  Public
    Function SkipWaiting : TJSPromise; external name 'skipWaiting';
    property registration : TJSServiceWorkerRegistration Read FRegistration;
    property clients : TJSClients Read FClients;
  end;

Var
  Self_ : TJSServiceWorkerGlobalScope; external name 'self';
  location : TJSWorkerLocation;
  console : TJSConsole;
  navigator : TJSWorkerNavigator;
  caches : TJSCacheStorage;

function fetch(resource: String; init: TJSObject): TJSPromise; overload; external name 'fetch';
//function fetch(resource: String): TJSPromise; overload; external name 'fetch';
function fetch(resource: String): TJSResponse; {$IFNDEF SkipAsync}async;{$ENDIF} overload; external name 'fetch';
function fetch(resource: TJSObject; init: TJSObject): TJSPromise; overload; external name 'fetch';
function fetch(resource: TJSObject): TJSPromise; overload; external name 'fetch';

implementation

end.

