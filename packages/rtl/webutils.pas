unit webutils deprecated 'Use rtl.HTMLUtils';

{$mode objfpc}

interface

uses
  web, js;

function AsyncSleep(ms: NativeInt): TJSPromise deprecated 'Use rtl.HTMLUtils.AsyncSleep';

implementation

function AsyncSleep(ms: NativeInt): TJSPromise;

begin
  Result := TJSPromise.New(
  procedure(resolve,reject : TJSPromiseResolver)
  begin
    window.setTimeout(
    procedure()
    begin
      resolve(ms);
    end,
    ms);
  end);
end;

end.

