library WasiButton1;

{$mode objfpc}
{$h+}
{$codepage UTF8}

uses
  SysUtils, JOB_Shared, JOB_Web, JOB_JS;

type

  { TWasmApp }

  TWasmApp = class
  private
    function OnButtonClick(Event: IJSEvent): boolean;
  public
    procedure Run;
  end;

{ TApplication }

function TWasmApp.OnButtonClick(Event: IJSEvent): boolean;
begin
  writeln('TWasmApp.OnButtonClick ');
  if Event=nil then ;

  JSWindow.Alert('You triggered TWasmApp.OnButtonClick');
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

