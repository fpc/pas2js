unit wasihostapp;

{$mode ObjFPC}

interface

uses
  Classes, SysUtils, JS, browserapp, web, webassembly, wasienv;

Type
  TStartDescriptor = record
    Memory : TJSWebAssemblyMemory;
    Table : TJSWebAssemblyTable;
    Exported : TWASIExports;
    Instance : TJSWebAssemblyInstance;
  end;

  { TWASIHostApplication }
  TBeforeStartCallBack = Reference to Function (Sender : TObject; aDescriptor : TStartDescriptor) : Boolean;
  TAfterStartCallBack = Reference to Procedure (Sender : TObject; aDescriptor : TStartDescriptor);

  TBeforeStartEvent = Procedure (Sender : TObject; aDescriptor : TStartDescriptor; var aAllowRun : Boolean) of object;
  TAfterStartEvent = Procedure (Sender : TObject; aDescriptor : TStartDescriptor) of object;


  TWASIHostApplication = class(TBrowserApplication)
  private
    FAfterStart: TAfterStartEvent;
    FBeforeStart: TBeforeStartEvent;
    FEnv: TPas2JSWASIEnvironment;
    FExported: TWASIExports;
    FMemoryDescriptor : TJSWebAssemblyMemoryDescriptor;
    FRunEntryFunction: String;
    FTableDescriptor : TJSWebAssemblyTableDescriptor;
    procedure DoStdRead(Sender: TObject; var AInput: string);
  protected
    procedure DoStdWrite(Sender: TObject; const aOutput: String); virtual;
    function CreateWebAssembly(aPath: string; aImportObject: TJSObject): TJSPromise; virtual;
    Function CreateWasiEnvironment : TPas2JSWASIEnvironment; virtual;
    function GetTable: TJSWebAssemblyTable; virtual;
    function GetMemory: TJSWebAssemblyMemory; virtual;
 public
    Constructor Create(aOwner : TComponent); override;
    Destructor Destroy; override;
    // Load and start webassembly. If DoRun is true, then Webassembly entry point is called.
    // If aBeforeStart is specified, then it is called prior to calling run, and can disable running.
    // If aAfterStart is specified, then it is called after calling run. It is not called is running was disabled.
    Procedure StartWebAssembly(aPath: string; DoRun : Boolean = True;  aBeforeStart : TBeforeStartCallback = Nil; aAfterStart : TAfterStartCallback = Nil);
    // Initial memory descriptor
    Property MemoryDescriptor : TJSWebAssemblyMemoryDescriptor Read FMemoryDescriptor Write FMemoryDescriptor;
    // Import/export table descriptor
    Property TableDescriptor : TJSWebAssemblyTableDescriptor Read FTableDescriptor Write FTableDescriptor;
    // Environment to be used
    Property WasiEnvironment : TPas2JSWASIEnvironment Read FEnv;
    // Exported functions. Also available in start descriptor.
    Property Exported : TWASIExports Read FExported;
    Property RunEntryFunction : String Read FRunEntryFunction Write FRunEntryFunction;
    // Called after webassembly start was run. Not called if webassembly was not run.
    Property AfterStart : TAfterStartEvent Read FAfterStart Write FAfterStart;
    // Called before running webassembly. If aAllowRun is false, running is disabled
    Property BeforeStart : TBeforeStartEvent Read FBeforeStart Write FBeforeStart;
  end;

implementation

{ TWASIHostApplication }

function TWASIHostApplication.CreateWasiEnvironment: TPas2JSWASIEnvironment;
begin
  Result:=TPas2JSWASIEnvironment.Create;
end;

constructor TWASIHostApplication.Create(aOwner: TComponent);
begin
  inherited Create(aOwner);
  FEnv:=CreateWasiEnvironment;
  FEnv.OnStdErrorWrite:=@DoStdWrite;
  FEnv.OnStdOutputWrite:=@DoStdWrite;
  Fenv.OnGetConsoleInputString:=@DoStdRead;
  FMemoryDescriptor.initial:=256;
  FMemoryDescriptor.maximum:=256;
  FTableDescriptor.initial:=0;
  FTableDescriptor.maximum:=0;
  FTableDescriptor.element:='anyfunc';
end;

destructor TWASIHostApplication.Destroy;
begin
  FreeAndNil(FEnv);
  inherited Destroy;
end;

function TWASIHostApplication.GetTable : TJSWebAssemblyTable;

begin
  Result:=TJSWebAssemblyTable.New(FTableDescriptor);
end;

function TWASIHostApplication.GetMemory : TJSWebAssemblyMemory;

begin
  Result:=TJSWebAssemblyMemory.New(FMemoryDescriptor);
end;


procedure TWASIHostApplication.StartWebAssembly(aPath: string; DoRun: Boolean;
  aBeforeStart: TBeforeStartCallback = nil; aAfterStart: TAfterStartCallback = nil);

Var
  ImportObj : TJSObject;
  Res : TStartDescriptor;

  function InitEnv(aValue: JSValue): JSValue;

  Var
    Module : TJSInstantiateResult absolute aValue;

  begin
    Result:=True;
    Res.Instance:=Module.Instance;
    Res.Exported:=TWASIExports(TJSObject(Module.Instance.exports_));
    // These 2 prevent running different instances simultaneously.
    FExported:=Res.Exported;
    WasiEnvironment.Instance:=Module.Instance;
    if Assigned(aBeforeStart) then
      DoRun:=aBeforeStart(Self,Res) and DoRun;
    if Assigned(FBeforeStart) then
      FBeforeStart(Self,Res,DoRun);
    if DoRun then
      begin
      if FRunEntryFunction='' then
        Res.Exported.Start
      else
        TProcedure(Res.Exported[RunEntryFunction])();
      if Assigned(aAfterStart) then
        aAfterStart(Self,Res);
      if Assigned(FAfterStart) then
        FAfterStart(Self,Res)
      end;
  end;


begin
  Res.Memory:=GetMemory;
  Res.Table:=GetTable;
  ImportObj:=new([
    'js', new([
      'mem', Res.Memory,
      'tbl', Res.Table
    ])
  ]);
  FEnv.AddImports(ImportObj);
  CreateWebAssembly(aPath,ImportObj)._then(@initEnv)
end;

procedure TWASIHostApplication.DoStdRead(Sender: TObject; var AInput: string);
begin
  aInput:=Window.prompt('Please enter the input for the running webassembly program.');
end;

procedure TWASIHostApplication.DoStdWrite(Sender: TObject; const aOutput: String
  );
begin
  Console.log('Webassembly output: ', aOutput);
end;

function TWASIHostApplication.CreateWebAssembly(aPath: string; aImportObject: TJSObject): TJSPromise;

  Function ArrayOK(res2 : jsValue) : JSValue;

  begin
     Result:=TJSWebAssembly.instantiate(TJSArrayBuffer(res2),aImportObject);
  end;

  function fetchOK(res : jsValue) : JSValue;
  begin
    Result:=TJSResponse(Res).arrayBuffer._then(@ArrayOK,Nil)
  end;

begin
  Result:=window.fetch(aPath)._then(@fetchOK,Nil);
end;


end.

