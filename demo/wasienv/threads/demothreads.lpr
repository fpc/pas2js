program demowasithreads;

{$mode objfpc}

uses
  browserconsole, browserapp, JS, Classes, SysUtils, Web, WebAssembly, types,
  wasienv, Rtl.WebThreads, wasihostapp, wasithreadedapp ;

Type

  { TMyApplication }

  TMyApplication = class(TBrowserWASIThreadedHostApplication)
  Private
    BtnStart : TJSHTMLButtonElement;
    procedure DoBeforeWasmInstantiate(Sender: TObject);
    function DoStartClick(aEvent: TJSMouseEvent): boolean;
    procedure DoWasmLoaded(Sender: TObject);
    procedure DoWrite(Sender: TObject; aOutput: String);
  Public
    procedure doRun; override;
  end;

procedure TMyApplication.DoWrite(Sender: TObject; aOutput: String);

begin
  Writeln('Wasm: '+aOutput);
end;

function TMyApplication.DoStartClick(aEvent: TJSMouseEvent): boolean;
begin
  Result:=false;
  Writeln('Host: Starting program');
  Host.Exported.start;
end;

procedure TMyApplication.DoBeforeWasmInstantiate(Sender: TObject);
begin
  Writeln('Host: Webassembly downloaded, instantiating VM');
end;

procedure TMyApplication.DoWasmLoaded(Sender: TObject);
begin
  Writeln('Host: wasm loaded, ready to run');
  BtnStart.Disabled:=False;
end;

procedure TMyApplication.doRun;

begin
  // Your code here
  Terminate;
  btnStart:=TJSHTMLButtonElement(GetHTMLElement('btnStart'));
  btnStart.onclick:=@DoStartClick;
  BtnStart.Disabled:=True;
  Host.MemoryDescriptor.initial:=256;
  Host.MemoryDescriptor.maximum:=512;
  Host.OnConsoleWrite:=@DoWrite;
  Host.AfterInstantation:=@DoWasmLoaded;
  Host.BeforeInstantation:=@DoBeforeWasmInstantiate;
  Writeln('Host: Loading wasm...');
  StartWebAssembly('threadapp.wasm',False);
end;

var
  Application : TMyApplication;

begin
  MaxConsoleLines:=250;
  Application:=TMyApplication.Create(nil);
  Application.Initialize;
  Application.Run;
end.
