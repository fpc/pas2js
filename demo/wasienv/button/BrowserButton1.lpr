program BrowserButton1;

{$mode objfpc}

uses
  BrowserConsole, BrowserApp, JS, Classes, SysUtils, Web, WebAssembly, Types,
  WasiEnv, WasiHostApp, JOB_Shared, JOB_Browser;

Type

  { TMyApplication }

  TMyApplication = class(TBrowserWASIHostApplication)
  Private
    FWADomBridge : TJSObjectBridge;
    function OnBeforeStart(Sender: TObject;
      aDescriptor: TWebAssemblyStartDescriptor): Boolean;
  Public
    Constructor Create(aOwner : TComponent); override;
    procedure DoRun; override;
  end;

function TMyApplication.OnBeforeStart(Sender: TObject;
  aDescriptor: TWebAssemblyStartDescriptor): Boolean;
begin
  FWADomBridge.WasiExports:=aDescriptor.Exported;
  Result:=true;
end;

constructor TMyApplication.Create(aOwner: TComponent);
begin
  inherited Create(aOwner);
  FWADomBridge:=TJSObjectBridge.Create(WasiEnvironment);
  RunEntryFunction:='_initialize';
end;

procedure TMyApplication.DoRun;

begin
  // Your code here
  StartWebAssembly('WasiButton1.wasm',true,@OnBeforeStart);
end;

var
  Application : TMyApplication;

begin
  Application:=TMyApplication.Create(nil);
  Application.Initialize;
  Application.Run;
end.

