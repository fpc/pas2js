program SimplePWA1;

{$mode objfpc}

uses
  JS, Classes, SysUtils, Web;

const
  GreekLetters: array[1..9] of string = (
   'Alpha', 'Beta', 'Gamma', 'Delta', 'Epsilon', 'Zeta', 'Eta', 'Theta', 'Iota'
    );

procedure ShowLetters;
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

begin
  // Your code here
  document.addEventListener('DOMContentLoaded', @ShowLetters);

  // register service worker
  if IsServiceWorker then
    Window.addEventListener('load',
      procedure()
      begin
        Window.navigator.serviceWorker
          .register('/ServiceWorker.js')
          ._then(TJSPromiseResolver(procedure(Registration: TJSServiceWorkerRegistration)
            begin
              console.log('service worker registered');
              if IsDefined(Registration.installing) then ;
            end))
          .catch(TJSPromiseResolver(procedure(err: JSValue)
            begin
              console.log('service worker not registered: '+String(err));
            end));
      end);
end.
