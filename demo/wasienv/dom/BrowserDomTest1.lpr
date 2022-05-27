program BrowserDomTest1;

{$mode objfpc}

uses
  BrowserConsole, BrowserApp, JS, Classes, SysUtils, Web, WebAssembly, Types,
  wasienv, job_browser, job_shared;

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

  TMyApplication = class(TBrowserApplication)
  Private
    FWasiEnv: TPas2JSWASIEnvironment;
    FMemory : TJSWebAssemblyMemory; // Memory of webassembly
    FTable : TJSWebAssemblyTable; // Table of exported functions
    FWADomBridge : TJOBBridge;
    function CreateWebAssembly(Path: string; ImportObject: TJSObject
      ): TJSPromise;
    procedure DoWrite(Sender: TObject; const aOutput: String);
    function initEnv(aValue: JSValue): JSValue;
    procedure InitWebAssembly;
  Public
    Constructor Create(aOwner : TComponent); override;
    Destructor Destroy; override;
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

function TMyApplication.InitEnv(aValue: JSValue): JSValue;
Var
  Module : TJSInstantiateResult absolute aValue;
  Exps : TWASIExports;
begin
  Result:=True;
  Exps := TWASIExports(TJSObject(Module.Instance.exports_));
  FWasiEnv.Instance:=Module.Instance;
  //  console.info('got exports', exps);
  Exps.Start;
end;

{ TMyApplication }

procedure TMyApplication.DoWrite(Sender: TObject; const aOutput: String);
begin
  Writeln(aOutput);
end;

constructor TMyApplication.Create(aOwner: TComponent);
begin
  inherited Create(aOwner);
  FWasiEnv:=TPas2JSWASIEnvironment.Create;
  FWasiEnv.OnStdErrorWrite:=@DoWrite;
  FWasiEnv.OnStdOutputWrite:=@DoWrite;
  FWADomBridge:=TJOBBridge.Create(FWasiEnv);

  if FWADomBridge.RegisterGlobalObject(TJSObject(TBird.Create('Root')))<>JObjIdBird then
    raise Exception.Create('Root TBird wrong number');
end;

function TMyApplication.CreateWebAssembly(Path: string; ImportObject: TJSObject): TJSPromise;
begin
  Result:=window.fetch(Path)._then(Function (res : jsValue) : JSValue
    begin
      Result:=TJSResponse(Res).arrayBuffer._then(Function (res2 : jsValue) : JSValue
        begin
          Result:=TJSWebAssembly.instantiate(TJSArrayBuffer(res2),ImportObject);
        end,Nil)
    end,Nil
  );
end;

procedure TMyApplication.InitWebAssembly;

Var
  mDesc : TJSWebAssemblyMemoryDescriptor;
  tDesc: TJSWebAssemblyTableDescriptor;
  ImportObj : TJSObject;

begin
  //  Setup memory
  mDesc.initial:=256;
  mDesc.maximum:=256;
  FMemory:=TJSWebAssemblyMemory.New(mDesc);
  // Setup table
  tDesc.initial:=0;
  tDesc.maximum:=0;
  tDesc.element:='anyfunc';
  FTable:=TJSWebAssemblyTable.New(tDesc);
  // Setup ImportObject
  ImportObj:=new([
    'js', new([
      'mem', FMemory,
      'tbl', FTable
    ])
 ]);
  FWasiEnv.AddImports(ImportObj);
  CreateWebAssembly('WasiDomTest1.wasm',ImportObj)._then(@initEnv);
end;

destructor TMyApplication.Destroy;
begin
  FreeAndNil(FWasiEnv);
  inherited Destroy;
end;

procedure TMyApplication.DoRun;

begin
  // Your code here
  Terminate;
  InitWebAssembly;
end;

var
  Application : TMyApplication;

begin
  Application:=TMyApplication.Create(nil);
  Application.Initialize;
  Application.Run;
end.
