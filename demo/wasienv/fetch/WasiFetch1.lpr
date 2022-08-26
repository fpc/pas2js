library WasiFetch1;

{$mode objfpc}
{$h+}
{$codepage UTF8}

uses
  SysUtils, Classes, JOB_Shared, JOB_Web, JOB_JS, Variants;

type

  { TFetchJSONHelper }

  TFetchJSONHelper = class
  protected
    function OnAccepted(const aValue: Variant): Variant; virtual;
    function OnRejected(const aValue: Variant): Variant; virtual;
    function OnJSONFailed(const aValue: Variant): Variant; virtual;
    function OnJSONReceived(const aValue: Variant): Variant; virtual;
    procedure DoError(const Msg: string); virtual;
  public
    URL: UnicodeString;
    Received: IJSObject; // result parsed from received JSON
    ErrorMsg: string;
    OnError: TNotifyEvent;
    OnReceived: TNotifyEvent;
    constructor Create(const TheURL: UnicodeString; const OnReceivedEvent: TNotifyEvent; const OnErrorEvent: TNotifyEvent);
  end;

  { TWasmApp }

  TWasmApp = class
  private
    FExampleFetch: TFetchJSONHelper;
    function OnButtonClick(Event: IJSEvent): boolean;
    procedure OnExampleFetchError(Sender: TObject);
    procedure OnExampleReceived(Sender: TObject);
  public
    procedure Run;
  end;

{ TFetchJSONHelper }

function TFetchJSONHelper.OnAccepted(const aValue: Variant): Variant;
var
  Obj: IJSObject;
  Response: IJSResponse;
begin
  Result:=false;
  writeln('TFetchJSONHelper.OnAccepted START');

  if not VarSupports(aValue,IJSObject,Obj) then
  begin
    DoError('TFetchJSONHelper.OnAccepted Expected object, but got '+VarTypeAsText(aValue));
    exit;
  end;
  Response:=TJSResponse.Cast(Obj);
  if not Response.ok then
  begin
    DoError('TFetchJSONHelper.OnAccepted Response not ok, status='+string(Response.statusText));
    exit;
  end;

  Response.json._then(@OnJSONReceived,@OnJSONFailed);

  Result:=true;
end;

function TFetchJSONHelper.OnRejected(const aValue: Variant): Variant;
begin
  Result:=true;
  DoError('TFetchJSONHelper.OnRejected '+VarTypeAsText(aValue));
end;

function TFetchJSONHelper.OnJSONFailed(const aValue: Variant): Variant;
begin
  Result:=true;
  DoError('TFetchJSONHelper.OnJSONFailed '+VarTypeAsText(aValue));
end;

function TFetchJSONHelper.OnJSONReceived(const aValue: Variant): Variant;
begin
  Result:=true;
  if not VarSupports(aValue,IJSObject,Received) then
  begin
    DoError('TWasmApp.OnJSONReceived Expected object, but got '+VarTypeAsText(aValue));
    exit;
  end;

  if Assigned(OnReceived) then
    OnReceived(Self);
end;

procedure TFetchJSONHelper.DoError(const Msg: string);
begin
  writeln('TFetchJSONHelper.DoError ',Msg);
  ErrorMsg:=Msg;
  if Assigned(OnError) then
    OnError(Self);
end;

constructor TFetchJSONHelper.Create(const TheURL: UnicodeString;
  const OnReceivedEvent: TNotifyEvent; const OnErrorEvent: TNotifyEvent);
begin
  URL:=TheURL;
  OnError:=OnErrorEvent;
  OnReceived:=OnReceivedEvent;
  JSWindow.fetch(URL)._then(@OnAccepted,@OnRejected);
end;

{ TApplication }

function TWasmApp.OnButtonClick(Event: IJSEvent): boolean;
begin
  Result:=true;
  writeln('TWasmApp.OnButtonClick ');
  if Event=nil then ;

  // JSWindow.Alert('You triggered TWasmApp.OnButtonClick');

  FExampleFetch:=TFetchJSONHelper.Create('Example.json',@OnExampleReceived,@OnExampleFetchError);
end;

procedure TWasmApp.OnExampleFetchError(Sender: TObject);
begin
  FExampleFetch.Free;
end;

procedure TWasmApp.OnExampleReceived(Sender: TObject);
var
  Example: IJSObject;
  aName, aValue: unicodestring;
begin
  Example:=FExampleFetch.Received;
  aName:=Example.Properties['name'];
  aValue:=Example.Properties['value'];
  JSWindow.Alert('Example Name="'+aName+'" Value="'+aValue+'"');
  FExampleFetch.free;
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

