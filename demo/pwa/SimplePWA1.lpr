program SimplePWA1;

{$mode objfpc}

uses
  Web, BrowserApp;

const
  GreekLetters: array[1..9] of string = (
   'Alpha', 'Beta', 'Gamma', 'Delta', 'Epsilon', 'Zeta', 'Eta', 'Theta', 'Iota'
    );

type

  { TWebApp }

  TWebApp = class(TBrowserApplication)
  protected
    procedure DoRun; override;
  public
    procedure ShowLetters;
  end;

procedure TWebApp.DoRun;
begin
  inherited DoRun;
  document.addEventListener('DOMContentLoaded', @ShowLetters);
  RegisterServiceWorker('/ServiceWorker.js');
end;

procedure TWebApp.ShowLetters;
var
  h, Letter: String;
  container: TJSElement;
begin
  h:='';
  for Letter in GreekLetters do
  begin
    h:=h+'<div class="card">'#10
        +'  <img class="card--image" src="/images/'+Letter+'.png"/>'#10
        +'  <h1 class="card--title">'+Letter+'</h1>'#10
        +'  <a class="card--link" href="#">Click</a>'#10
        +'</div>'#10;
  end;
  container:=document.querySelector('.container');
  container.innerHTML := h;
end;

var
  App: TWebApp;
begin
  App:=TWebApp.Create(nil);
  App.Run;
end.
