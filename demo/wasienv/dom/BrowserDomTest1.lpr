program BrowserDomTest1;

{$mode objfpc}

uses
  BrowserConsole, JS, Classes, SysUtils, Web, WasiEnv, WasiHostApp, JOB_Browser, JOB_Shared;

Type
  TBirdCallback = function(const v: JSValue): JSValue;

  { TBird }

  TBird = class
  public
    constructor Create(const aName: string); reintroduce;
    procedure Proc;
    function ArgsToStr(Args: TJSFunctionArguments): string;
  published
    Enabled: boolean;
    Scale: double;
    Size: integer;
    Name: string;
    Child: TBird;
    function GetBoolean: boolean;
    function GetDouble: double;
    function GetInteger: integer;
    function GetString: string;
    function GetBird: TBird;
    function Echo(const a: JSValue): JSValue;
    function EchoCall(const a: JSValue; const CB: TBirdCallback): JSValue;
    function CreateBird(const aName: string): TBird;
    procedure IncSize;
  end;

  { TMyApplication }

  TMyApplication = class(TBrowserWASIHostApplication)
  Private
    FWADomBridge : TJSObjectBridge;
    function OnBeforeStart(Sender: TObject;
      aDescriptor: TWebAssemblyStartDescriptor): Boolean;
  Public
    constructor Create(aOwner : TComponent); override;
    procedure DoRun; override;
  end;

{ TBird }

constructor TBird.Create(const aName: string);
begin
  Name:=aName;

  if false then begin
    // use, so pas2js includes them
    Proc;
    GetBoolean;
    GetDouble;
    GetInteger;
    GetString;
    GetBird;
    CreateBird('');
  end;
end;

procedure TBird.Proc;
begin
  writeln('TBird.Proc [',Name,'] ',ArgsToStr(JSArguments));
end;

function TBird.GetBoolean: boolean;
begin
  writeln('TBird.GetBoolean [',Name,'] ',ArgsToStr(JSArguments));
  Result:=Enabled;
end;

function TBird.GetDouble: double;
begin
  writeln('TBird.GetDouble [',Name,'] ',ArgsToStr(JSArguments));
  Result:=Scale;
end;

function TBird.GetString: string;
begin
  writeln('TBird.GetString [',Name,'] ',ArgsToStr(JSArguments));
  Result:=Name;
end;

function TBird.GetBird: TBird;
begin
  writeln('TBird.GetBird [',Name,'] ',ArgsToStr(JSArguments));
  Result:=Child;
end;

function TBird.Echo(const a: JSValue): JSValue;
begin
  Result:=a;
end;

function TBird.EchoCall(const a: JSValue; const CB: TBirdCallback): JSValue;
begin
  writeln('TBird.EchoCall argument=',a);
  Result:=CB(a);
  writeln('TBird.EchoCall Result=',Result);
end;

function TBird.GetInteger: integer;
begin
  writeln('TBird.GetInteger [',Name,'] ',ArgsToStr(JSArguments));
  Result:=Size;
end;

function TBird.CreateBird(const aName: string): TBird;
begin
  writeln('TBird.CreateBird [',Name,'] ',ArgsToStr(JSArguments));
  Result:=TBird.Create(Name+'.'+aName);
end;

procedure TBird.IncSize;
begin
  inc(Size);
end;

function TBird.ArgsToStr(Args: TJSFunctionArguments): string;
var
  i: Integer;
  t: String;
  a: JSValue;
begin
  Result:='[';
  for i:=0 to Args.Length-1 do
  begin
    if i>0 then Result:=Result+',';
    a:=Args[i];
    t:=jstypeof(a);
    if t='string' then
      Result:=Result+'"'+String(a)+'"'
    else
      Result:=Result+String(a);
  end;
  Result:=Result+']';
end;

{ TMyApplication }

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

  if FWADomBridge.RegisterGlobalObject(TBird.Create('Root'),'Bird')=0 then
    raise Exception.Create('failed to register TBird');
end;

procedure TMyApplication.DoRun;

begin
  // Your code here
  Terminate;
  StartWebAssembly('WasiDomTest1.wasm',true,@OnBeforeStart);
end;

var
  Application : TMyApplication;

begin
  Application:=TMyApplication.Create(nil);
  Application.Initialize;
  Application.Run;
end.
