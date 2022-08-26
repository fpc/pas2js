library WasiFetch1;

{$mode objfpc}
{$h+}
{$codepage UTF8}

uses
  SysUtils, JOB_Shared, JOB_Web, JOB_JS, Variants;

type

  { TWasmApp }

  TWasmApp = class
  private
    function OnAccepted(const aValue: Variant): Variant;
    function OnButtonClick(Event: IJSEvent): boolean;
    function OnJSONFailed(const aValue: Variant): Variant;
    function OnJSONReceived(const aValue: Variant): Variant;
    function OnRejected(const aValue: Variant): Variant;
  public
    procedure Run;
  end;

{ TApplication }

function TWasmApp.OnAccepted(const aValue: Variant): Variant;
var
  Obj: IJSObject;
  Response: IJSResponse;
begin
  Result:=false;
  writeln('TWasmApp.OnAccepted START');

  if not VarSupports(aValue,IJSObject,Obj) then
  begin
    writeln('TWasmApp.OnAccepted Expected object, but got '+VarTypeAsText(aValue));
    exit;
  end;
  Response:=TJSResponse.Cast(Obj);
  writeln('TWasmApp.OnAccepted Response: ok=',Response.ok);
  writeln('TWasmApp.OnAccepted Response: status=',Response.status);
  writeln('TWasmApp.OnAccepted Response: statusText="',Response.statusText,'"');
  writeln('TWasmApp.OnAccepted Response: redirected=',Response.redirected);
  writeln('TWasmApp.OnAccepted Response: URL="',Response.url,'"');

  Response.json._then(@OnJSONReceived,@OnJSONFailed);

  Result:=true;
end;

function TWasmApp.OnButtonClick(Event: IJSEvent): boolean;
begin
  writeln('TWasmApp.OnButtonClick ');
  if Event=nil then ;

  // JSWindow.Alert('You triggered TWasmApp.OnButtonClick');

  JSWindow.fetch('Example.json')._then(@OnAccepted,@OnRejected);
  Result:=true;
end;

function TWasmApp.OnJSONFailed(const aValue: Variant): Variant;
begin
  writeln('TWasmApp.OnJSONFailed');
  Result:=true;
end;

function TWasmApp.OnJSONReceived(const aValue: Variant): Variant;
var
  Obj: IJSObject;
begin
  writeln('TWasmApp.OnJSONReceived START');
  Result:=true;
  if not VarSupports(aValue,IJSObject,Obj) then
  begin
    writeln('TWasmApp.OnJSONReceived not an IJSObject');
    writeln('TWasmApp.OnJSONReceived Expected object, but got '+VarTypeAsText(aValue));
    exit;
  end;
  writeln('TWasmApp.OnJSONReceived Obj.name=',Obj.Properties['name']);
  writeln('TWasmApp.OnJSONReceived Obj.value=',Obj.Properties['value']);
end;

function TWasmApp.OnRejected(const aValue: Variant): Variant;
begin
  writeln('TWasmApp.OnRejected START');
  if VarIsStr(aValue) then
    writeln('TWasmApp.OnRejected ',aValue)
  else
    writeln('TWasmApp.OnRejected ',VarTypeAsText(aValue));
  Result:=true;
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

