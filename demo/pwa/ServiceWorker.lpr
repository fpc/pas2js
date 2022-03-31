program ServiceWorker;

{$mode objfpc}

uses
  JS, Web, Types;

const
  CacheName = 'v5';

  FallbackURL = '/images/error.png';

  Resources: array[0..12] of string = (
    '/index.html',
    '/css/style.css',
    '/SimplePWA1.js',
    '/images/Alpha.png',
    '/images/Beta.png',
    '/images/Gamma.png',
    '/images/Delta.png',
    '/images/Epsilon.png',
    '/images/Zeta.png',
    '/images/Eta.png',
    '/images/Theta.png',
    '/images/Iota.png',
    '/images/error.png'
    );

procedure PutInCache(Request: TJSRequest; Response: TJSResponse); async;
var
  Cache: TJSCache;
begin
  Cache := await(TJSCache,Caches.open(CacheName));
  await(TJSCache,Cache.put(Request, Response));
end;

function CacheFirst(Request: TJSRequest; PreloadResponsePromise: TJSPromise;
  FallbackUrl: string): jsvalue; async;
var
  ResponseFromCache, PreloadResponse, ResponseFromNetwork, FallbackResponse: TJSResponse;
begin
  Result:=nil;

  // First try to get the resource from the cache
  ResponseFromCache := await(TJSResponse,caches.match(Request));
  if Assigned(ResponseFromCache) then
    exit(ResponseFromCache);

  // Next try to use (and cache) the preloaded response, if it's there
  PreloadResponse := await(TJSResponse,PreloadResponsePromise);
  if Assigned(PreloadResponse) then
  begin
    console.info('using preload response: '+String(JSValue(PreloadResponse)));
    putInCache(Request, PreloadResponse.clone());
    exit(PreloadResponse);
  end;

  // Next try to get the resource from the network
  try
    ResponseFromNetwork := await(TJSResponse,window.fetch(Request));
    // response may be used only once
    // we need to save clone to put one copy in cache
    // and serve second one
    PutInCache(Request, ResponseFromNetwork.clone());
    exit(ResponseFromNetwork);
  except
    FallbackResponse := await(TJSResponse,caches.match(FallbackUrl));
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

// Enable navigation preload
function EnableNavigationPreload: jsvalue; async;
begin
  Result:=nil;
  if jsvalue(serviceWorker.registration.navigationPreload) then
    // Enable navigation preloads!
    await(serviceWorker.registration.navigationPreload.enable());
end;

procedure DeleteCache(key: string); async;
begin
  await(boolean,caches.delete(key));
end;

function DeleteOldCaches: jsvalue; async;
var
  CacheKeepList: TStringDynArray;
  CachesToDelete, KeyList: TJSArray;
begin
  CacheKeepList := [CacheName];
  KeyList := await(TJSArray,caches.keys());
  CachesToDelete := keyList.filter(
    function (key: JSValue; index: NativeInt; anArray : TJSArray) : Boolean
    begin
      Result:=not TJSArray(CacheKeepList).includes(key);
    end);
  Result:=await(jsvalue,TJSPromise.all(CachesToDelete.map(TJSArrayMapEvent(@DeleteCache))));
end;

begin
  ServiceWorker.addEventListener('activate', procedure(Event: TJSExtendableEvent)
    begin
      Event.waitUntil(EnableNavigationPreload());
      event.waitUntil(DeleteOldCaches());
    end);

  ServiceWorker.addEventListener('install', procedure(Event: TJSExtendableEvent)
    begin
      Event.waitUntil(
        Caches.Open(CacheName)._then(
          TJSPromiseResolver(procedure(Cache: TJSCache)
          begin
            Cache.addAll(Resources);
          end))
      );
    end);

  ServiceWorker.addEventListener('fetch', procedure(FetchEvent: TJSFetchEvent)
    begin
      FetchEvent.RespondWith(CacheFirst(FetchEvent.request,
                             FetchEvent.PreloadResponse,FallbackURL) );
    end);
end.
