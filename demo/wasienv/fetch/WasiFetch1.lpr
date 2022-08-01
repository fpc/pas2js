library WasiFetch1;

{$mode objfpc}
{$h+}
{$codepage UTF8}

uses
  SysUtils, JOB_Shared, JOB_Web, JOB_JS;

type

  { TWasmApp }

  TWasmApp = class
  private
    function OnAccepted(const aValue: TJOB_JSValue): TJOB_JSValue;
    function OnButtonClick(Event: IJSEvent): boolean;
    function OnJSONFailed(const aValue: TJOB_JSValue): TJOB_JSValue;
    function OnJSONReceived(const aValue: TJOB_JSValue): TJOB_JSValue;
    function OnRejected(const aValue: TJOB_JSValue): TJOB_JSValue;
  public
    procedure Run;
  end;

{ TApplication }

function TWasmApp.OnAccepted(const aValue: TJOB_JSValue): TJOB_JSValue;
var
  Obj: IJSObject;
  Response: IJSResponse;
  p: IJSPromise;
begin
  Result:=TJOB_Boolean.Create(false);
  writeln('TWasmApp.OnAccepted ',aValue.AsString);
  if aValue.Kind<>jjvkObject then
  begin
    writeln('TWasmApp.OnAccepted Expected object, but got '+JOB_JSValueKindNames[aValue.Kind]);
    exit;
  end;
  Obj:=TJOB_Object(aValue).Value;
  if Obj=nil then
  begin
    writeln('TWasmApp.OnAccepted Expected object, but got nil');
    exit;
  end;

  Response:=TJSResponse.Cast(Obj);
  writeln('TWasmApp.OnAccepted Response: ok=',Response.ok);
  writeln('TWasmApp.OnAccepted Response: status=',Response.status);
  writeln('TWasmApp.OnAccepted Response: statusText="',Response.statusText,'"');
  writeln('TWasmApp.OnAccepted Response: redirected=',Response.redirected);
  writeln('TWasmApp.OnAccepted Response: URL="',Response.url,'"');

  p:=Response.InvokeJSObjectResult('json',[],TJSPromise) as IJSPromise;
  p._then(@OnJSONReceived,@OnJSONFailed);

  TJOB_Boolean(Result).Value:=true;
end;

function TWasmApp.OnButtonClick(Event: IJSEvent): boolean;
var
  p: IJSPromise;
begin
  writeln('TWasmApp.OnButtonClick ');
  if Event=nil then ;

  JSWindow.Alert('You triggered TWasmApp.OnButtonClick');

  p:=JSWindow.InvokeJSObjectResult('fetch',['Example.json'],TJSPromise) as IJSPromise;
  p._then(@OnAccepted,@OnRejected);
  //JSWindow.fetch('Example.json')._then(@OnAccepted,@OnRejected);
  Result:=true;
end;

function TWasmApp.OnJSONFailed(const aValue: TJOB_JSValue): TJOB_JSValue;
begin
  Result:=TJOB_Boolean.Create(true);
end;

function TWasmApp.OnJSONReceived(const aValue: TJOB_JSValue): TJOB_JSValue;
var
  Obj: IJSObject;
begin
  Result:=TJOB_Boolean.Create(true);

  if aValue.Kind<>jjvkObject then
  begin
    writeln('TWasmApp.OnJSONReceived Expected object, but got '+JOB_JSValueKindNames[aValue.Kind]);
    exit;
  end;
  Obj:=TJOB_Object(aValue).Value;
  if Obj=nil then
  begin
    writeln('TWasmApp.OnJSONReceived Expected object, but got nil');
    exit;
  end;

  writeln('TWasmApp.OnJSONReceived Obj.name=',Obj.Properties['name'].AsString);
  writeln('TWasmApp.OnJSONReceived Obj.value=',Obj.Properties['value'].AsString);
end;

function TWasmApp.OnRejected(const aValue: TJOB_JSValue): TJOB_JSValue;
begin
  writeln('TWasmApp.OnRejected ',aValue.AsString);
  Result:=TJOB_Boolean.Create(true);
end;

procedure TWasmApp.Run;
var
  JSDiv: IJSHTMLDivElement;
  JSButton: IJSHTMLButtonElement;
begin
  writeln('TWasmApp.Run getElementById "playground" ...');
  // get reference of HTML element "playground" and type cast it to Div
  JSDiv:=TJSHTMLDivElement.Cast(JSDocument.getElementById('playground'));

  // create button
  writeln('TWasmApp.Run create button ...');
  JSButton:=TJSHTMLButtonElement.Cast(JSDocument.createElement('button'));
  writeln('TWasmApp.Run set button caption ...');
  JSButton.InnerHTML:='Click me!';

  // add button to div
  writeln('TWasmApp.Run add button to div ...');
  JSDiv.append(JSButton);

  // add event listener OnButtonClick
  writeln('TWasmApp.Run addEventListener OnButtonClick ...');
  JSButton.addEventListener('click',@OnButtonClick);

  writeln('TWasmApp.Run END');
end;

// workaround: fpc wasm does not yet support exporting functions from units
function JOBCallback(const Func: TJOBCallback; Data, Code: Pointer; Args: PByte): PByte;
begin
  Result:=JOB_JS.JOBCallback(Func,Data,Code,Args);
end;

exports
  JOBCallback;

var
  Application: TWasmApp;
begin
  Application:=TWasmApp.Create;
  Application.Run;
end.

