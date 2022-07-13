program BrowserDomTest1;

{$mode objfpc}

uses
  BrowserConsole, JS, Classes, SysUtils, Web, WasiEnv, WasiHostApp, JOB_Browser, JOB_Shared;

Type

  { TBird }

  TBird = class
  public
    constructor Create(const aName: string); reintroduce;
    procedure Proc;
    function ArgsToStr(Args: TJSFunctionArguments): string;
  published
    Size: integer;
    Name: string;
    Child: TBird;
    function GetBoolean: boolean;
    function GetDouble: double;
    function GetString: string;
    function GetInteger: integer;
    function CreateChick(const aName: string): TBird;
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
    CreateChick('');
  end;
end;

procedure TBird.Proc;
begin
  writeln('TBird.Proc [',Name,'] ',ArgsToStr(JSArguments));
end;

function TBird.GetBoolean: boolean;
begin
  writeln('TBird.GetBoolean [',Name,'] ',ArgsToStr(JSArguments));
  Result:=JSArguments.Length mod 1 = 0;
end;

function TBird.GetDouble: double;
begin
  writeln('TBird.GetDouble [',Name,'] ',ArgsToStr(JSArguments));
  Result:=0.3+JSArguments.Length;
end;

function TBird.GetString: string;
begin
  writeln('TBird.GetString [',Name,'] ',ArgsToStr(JSArguments));
  Result:='TBird.GetString:'+str(JSArguments.Length);
  if JSArguments.Length>0 then
    Result:=Result+String(JSArguments[0]);
end;

function TBird.GetInteger: integer;
begin
  writeln('TBird.GetInteger [',Name,'] ',ArgsToStr(JSArguments));
  Result:=3000+JSArguments.Length;
end;

function TBird.CreateChick(const aName: string): TBird;
begin
  writeln('TBird.CreateChick [',Name,'] ',ArgsToStr(JSArguments));
  Result:=TBird.Create(Name+'.'+aName);
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
