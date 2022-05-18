{
  A service worker functions like a proxy server, allowing to modify requests
  and responses, and replace them with items from its own cache, and more.
}
unit ServiceWorkerApp;

{$mode objfpc}

interface

uses
  Classes, SysUtils, Types, JS, weborworker, webworker, CustApp;

type

  { TServiceWorkerApplication }

  TServiceWorkerApplication = class(TCustomApplication)
  private
    FFallbackURL: string;
  protected
    FCacheName: string;
    FResources: TStringDynArray;
    procedure PutInCache(Request: TJSRequest; Response: TJSResponse); async; virtual;
    function CacheFirst(Request: TJSRequest; PreloadResponsePromise: TJSPromise;
      FallbackUrl: string): jsvalue; async; virtual;
    function EnableNavigationPreload: jsvalue; async; virtual;
    procedure DeleteCache(key: string); async; virtual;
    function DeleteOldCaches: jsvalue; async; virtual;
    procedure SetFallbackURL(const AValue: string); virtual;
    procedure DoRun; override;
    procedure Activate(Event: TJSExtendableEvent); virtual;
    procedure Install(Event: TJSExtendableEvent); virtual;
    procedure Fetch(FetchEvent: TJSFetchEvent); virtual;

    function GetConsoleApplication: boolean; override;
    function GetLocation: String; override;
  public
    procedure GetEnvironmentList(List{%H-}: TStrings; NamesOnly{%H-}: Boolean); override;
    procedure ShowException(E: Exception); override;
    procedure HandleException(Sender: TObject); override;

    property CacheName: string read FCacheName;
    property FallbackURL: string read FFallbackURL write SetFallbackURL;
    property Resources: TStringDynArray read FResources;
  end;

implementation

var
  EnvNames: TJSObject;

procedure ReloadEnvironmentStrings;

var
  I : Integer;
  S,N : String;
  A,P : TStringDynArray;
begin
  if Assigned(EnvNames) then
    FreeAndNil(EnvNames);
  EnvNames:=TJSObject.new;
  S:=self_.Location.search;
  S:=Copy(S,2,Length(S)-1);
  A:=TJSString(S).split('&');
  for I:=0 to Length(A)-1 do
    begin
    P:=TJSString(A[i]).split('=');
    N:=LowerCase(decodeURIComponent(P[0]));
    if Length(P)=2 then
      EnvNames[N]:=decodeURIComponent(P[1])
    else if Length(P)=1 then
      EnvNames[N]:=''
    end;
end;

function MyGetEnvironmentVariable(Const EnvVar: String): String;

Var
  aName : String;

begin
  aName:=Lowercase(EnvVar);
  if EnvNames.hasOwnProperty(aName) then
    Result:=String(EnvNames[aName])
  else
    Result:='';
end;

function MyGetEnvironmentVariableCount: Integer;
begin
  Result:=length(TJSOBject.getOwnPropertyNames(envNames));
end;

function MyGetEnvironmentString(Index: Integer): String;
begin
  Result:=String(EnvNames[TJSOBject.getOwnPropertyNames(envNames)[Index]]);
end;

{ TServiceWorkerApplication }

procedure TServiceWorkerApplication.SetFallbackURL(const AValue: string);
begin
  if FFallbackURL=AValue then Exit;
  FFallbackURL:=AValue;
end;

procedure TServiceWorkerApplication.PutInCache(Request: TJSRequest;
  Response: TJSResponse);
var
  Cache: TJSCache;
begin
  Cache := await(TJSCache,self_.Caches.open(CacheName));
  await(TJSCache,Cache.put(Request, Response));
end;

function TServiceWorkerApplication.CacheFirst(Request: TJSRequest;
  PreloadResponsePromise: TJSPromise; FallbackUrl: string): jsvalue;
var
  ResponseFromCache, PreloadResponse, ResponseFromNetwork, FallbackResponse: TJSResponse;
begin
  Result:=nil;

  // First try to get the resource from the cache
  ResponseFromCache := await(TJSResponse,self_.caches.match(Request));
  if Assigned(ResponseFromCache) then
    exit(ResponseFromCache);

  // Next try to use (and cache) the preloaded response, if it's there
  PreloadResponse := await(TJSResponse,PreloadResponsePromise);
  if Assigned(PreloadResponse) then
  begin
    //console.info('using preload response: '+String(JSValue(PreloadResponse)));
    putInCache(Request, PreloadResponse.clone());
    exit(PreloadResponse);
  end;

  // Next try to get the resource from the network
  try
    ResponseFromNetwork := await(TJSResponse,self_.fetch(Request));
    // response may be used only once
    // we need to save clone to put one copy in cache
    // and serve second one
    PutInCache(Request, ResponseFromNetwork.clone());
    exit(ResponseFromNetwork);
  except
    FallbackResponse := await(TJSResponse,self_.caches.match(FallbackUrl));
    if Assigned(FallbackResponse) then
      exit(FallbackResponse);

    // when even the fallback response is not available,
    // there is nothing we can do, but we must always
    // return a Response object
    Result:=TJSResponse.new('Network error happened', js.new([
      'status', 408,
      'headers',
        js.new(['Content-Type', 'text/plain' ])
      ]) );
  end;
end;

function TServiceWorkerApplication.EnableNavigationPreload: jsvalue;
begin
  Result:=nil;
  if jsvalue(self_.registration.navigationPreload) then
    // Enable navigation preloads!
    await(self_.registration.navigationPreload.enable());
end;

procedure TServiceWorkerApplication.DeleteCache(key: string);
begin
  await(boolean,Self_.caches.delete(key));
end;

function TServiceWorkerApplication.DeleteOldCaches: jsvalue;
var
  CacheKeepList: TStringDynArray;
  CachesToDelete, KeyList: TJSArray;
begin
  CacheKeepList := [CacheName];
  KeyList := await(TJSArray,self_.caches.keys());
  CachesToDelete := keyList.filter(
    function (key: JSValue; index: NativeInt; anArray : TJSArray) : Boolean
    begin
      Result:=not TJSArray(CacheKeepList).includes(key);
    end);
  Result:=await(jsvalue,TJSPromise.all(CachesToDelete.map(TJSArrayMapEvent(@DeleteCache))));
end;

procedure TServiceWorkerApplication.DoRun;
begin
  self_.addEventListener('activate', @Activate);
  self_.addEventListener('install', @Install);
  self_.addEventListener('fetch', @Fetch);
end;

procedure TServiceWorkerApplication.Activate(Event: TJSExtendableEvent);
begin
  Event.waitUntil(EnableNavigationPreload());
  Event.waitUntil(DeleteOldCaches());
end;

procedure TServiceWorkerApplication.Install(Event: TJSExtendableEvent);
begin
  Event.waitUntil(
    self_.Caches.Open(CacheName)._then(
      TJSPromiseResolver(procedure(Cache: TJSCache)
      begin
        Cache.addAll(Resources);
      end))
  );
end;

procedure TServiceWorkerApplication.Fetch(FetchEvent: TJSFetchEvent);
begin
  FetchEvent.RespondWith(CacheFirst(FetchEvent.request,
                         FetchEvent.PreloadResponse,FallbackURL) );
end;

function TServiceWorkerApplication.GetConsoleApplication: boolean;
begin
  Result:=true;
end;

function TServiceWorkerApplication.GetLocation: String;
begin
  Result:='';
end;

procedure TServiceWorkerApplication.GetEnvironmentList(List: TStrings;
  NamesOnly: Boolean);
begin

end;

procedure TServiceWorkerApplication.ShowException(E: Exception);

Var
  S : String;

begin
  if (E<>nil) then
    S:=E.ClassName+': '+E.Message
  else if ExceptObjectJS then
    s:=TJSObject(ExceptObjectJS).toString;
  console.log('Unhandled exception caught:'+S);
end;

procedure TServiceWorkerApplication.HandleException(Sender: TObject);
begin
  if ExceptObject is Exception then
    ShowException(ExceptObject);
  inherited HandleException(Sender);
end;

initialization
  
  IsConsole:=true;
  ReloadEnvironmentStrings;
  OnGetEnvironmentVariable:=@MyGetEnvironmentVariable;
  OnGetEnvironmentVariableCount:=@MyGetEnvironmentVariableCount;
  OnGetEnvironmentString:=@MyGetEnvironmentString;
  
end.

