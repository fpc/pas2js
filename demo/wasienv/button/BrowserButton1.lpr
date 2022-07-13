program BrowserButton1;

{$mode objfpc}

uses
  BrowserConsole, BrowserApp, JS, Classes, SysUtils, Web, WebAssembly, Types,
  WasiEnv, JOB_Shared, JOB_Browser;

Type

  { TMyApplication }

  TMyApplication = class(TBrowserApplication)
  // todo: most of this code should be moved a TBrowserJOBApplication
  Private
    FWasiEnv: TPas2JSWASIEnvironment;
    FMemory : TJSWebAssemblyMemory; // Memory of webassembly
    FTable : TJSWebAssemblyTable; // Table of exported functions
    FWADomBridge : TJSObjectBridge;
    function CreateWebAssembly(Path: string; ImportObject: TJSObject
      ): TJSPromise;
    procedure DoWrite(Sender: TObject; const aOutput: String);
    function InitEnv(aValue: JSValue): JSValue;
    procedure InitWebAssembly;
  Public
    Constructor Create(aOwner : TComponent); override;
    Destructor Destroy; override;
    procedure DoRun; override;
  end;

function TMyApplication.InitEnv(aValue: JSValue): JSValue;
Var
  Module : TJSInstantiateResult absolute aValue;
  Exps : TWASIExports;
  InitFunc: TProc;
begin
  Result:=True;
  FWasiEnv.Instance:=Module.Instance;
  Exps := TWASIExports(TJSObject(Module.Instance.exports_));
  //writeln('TMyApplication.InitEnv wasm exports=',TJSObject.keys(Exps));
  FWADomBridge.WasiExports:=Exps;

  // init the library
  InitFunc:=TProc(Exps.functions['_initialize']);
  writeln('TMyApplication.InitEnv ');
  InitFunc();
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
  FWADomBridge:=TJSObjectBridge.Create(FWasiEnv);
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
  mDesc: TJSWebAssemblyMemoryDescriptor;
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
  CreateWebAssembly('WasiButton1.wasm',ImportObj)._then(@InitEnv);
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

