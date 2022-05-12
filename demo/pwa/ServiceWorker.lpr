program ServiceWorker;

{$mode objfpc}

uses
  SysUtils, Classes, ServiceWorkerApp;

const
  YourCacheName = 'v7'; // usually increased with every version
   // The cache is specific to your domain, so no need to include your app name.

type

  { TApplication }

  TApplication = class(TServiceWorkerApplication)
  public
    constructor Create(AOwner: TComponent); override;
  end;

{ TApplication }

constructor TApplication.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  FCacheName:=YourCacheName;
  FResources:=[
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
    '/images/error.png' ];
  FallbackURL := '/images/error.png';
end;

var
  App: TApplication;
begin
  App:=TApplication.Create(nil);
  App.Run;
end.
