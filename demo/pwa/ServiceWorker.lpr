program ServiceWorker;

{$mode objfpc}

uses
  JS, Web, Types, ServiceWorkerApp;

const
  YourCacheName = 'v5';

type

  { TApplication }

  TApplication = class(TServiceWorkerApplication)
  protected
    procedure DoRun; override;
  public
  end;

var
  App: TApplication;

{ TApplication }

procedure TApplication.DoRun;
begin
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
  inherited DoRun;
end;

begin
  App:=TApplication.Create(nil);
  App.Run;
end.
