unit wasiserviceapp;

{$mode ObjFPC}

interface

uses
  Classes, SysUtils, serviceworkerapp,  webassembly, wasienv;

Type
  { TWASIServiceApplication }

  TWASIServiceApplication = class(TServiceWorkerApplication)
  private
    FHost : TWASIHost;
    FOnConsoleRead: TConsoleReadEvent;
    FOnConsoleWrite: TConsoleWriteEvent;
    FPredefinedConsoleInput: TStrings;
    function GetAfterStart: TAfterStartEvent;
    function GetBeforeStart: TBeforeStartEvent;
    function GetEnv: TPas2JSWASIEnvironment;
    function GetExported: TWASIExports;
    function GetMemoryDescriptor: TJSWebAssemblyMemoryDescriptor;
    function GetRunEntryFunction: String;
    function GetTableDescriptor: TJSWebAssemblyTableDescriptor;
    procedure SetAfterStart(AValue: TAfterStartEvent);
    procedure SetBeforeStart(AValue: TBeforeStartEvent);
    procedure SetMemoryDescriptor(AValue: TJSWebAssemblyMemoryDescriptor);
    procedure SetPredefinedConsoleInput(AValue: TStrings);
    procedure SetRunEntryFunction(AValue: String);
    procedure SetTableDescriptor(AValue: TJSWebAssemblyTableDescriptor);
  protected
    function CreateHost: TWASIHost; virtual;
  public
    Constructor Create(aOwner : TComponent); override;
    Destructor Destroy; override;
    // Load and start webassembly. If DoRun is true, then Webassembly entry point is called.
    // If aBeforeStart is specified, then it is called prior to calling run, and can disable running.
    // If aAfterStart is specified, then it is called after calling run. It is not called is running was disabled.
    Procedure StartWebAssembly(aPath: string; DoRun : Boolean = True;  aBeforeStart : TBeforeStartCallback = Nil; aAfterStart : TAfterStartCallback = Nil);
    // Initial memory descriptor
    Property MemoryDescriptor : TJSWebAssemblyMemoryDescriptor Read GetMemoryDescriptor Write SetMemoryDescriptor;
    // Import/export table descriptor
    Property TableDescriptor : TJSWebAssemblyTableDescriptor Read GetTableDescriptor Write SetTableDescriptor;
    // Environment to be used
    Property WasiEnvironment : TPas2JSWASIEnvironment Read GetEnv;
    // Exported functions. Also available in start descriptor.
    Property Exported : TWASIExports Read GetExported;
    // Name of function to run, if empty default _start symbol is used.
    Property RunEntryFunction : String Read GetRunEntryFunction Write SetRunEntryFunction;
    // Called after webassembly start was run. Not called if webassembly was not run.
    Property AfterStart : TAfterStartEvent Read GetAfterStart Write SetAfterStart;
    // Called before running webassembly. If aAllowRun is false, running is disabled
    Property BeforeStart : TBeforeStartEvent Read GetBeforeStart Write SetBeforeStart;
    // Default console input
    Property PredefinedConsoleInput : TStrings Read FPredefinedConsoleInput Write SetPredefinedConsoleInput;
    // Called when reading from console (stdin). If not set, PredefinedConsoleinput is used.
    property OnConsoleRead : TConsoleReadEvent Read FOnConsoleRead Write FOnConsoleRead;
    // Called when writing to console (stdout). If not set, console.log is used.
    property OnConsoleWrite : TConsoleWriteEvent Read FOnConsoleWrite Write FOnConsoleWrite;
  end;

implementation

{ TWASIServiceApplication }

function TWASIServiceApplication.GetAfterStart: TAfterStartEvent;
begin
  Result:=FHost.AfterStart;
end;

function TWASIServiceApplication.GetBeforeStart: TBeforeStartEvent;
begin
  Result:=FHost.BeforeStart;
end;

function TWASIServiceApplication.GetEnv: TPas2JSWASIEnvironment;
begin
  Result:=FHost.WasiEnvironment;
end;

function TWASIServiceApplication.GetExported: TWASIExports;
begin
  Result:=FHost.Exported;
end;

function TWASIServiceApplication.GetMemoryDescriptor: TJSWebAssemblyMemoryDescriptor;
begin
  Result:=FHost.MemoryDescriptor;
end;

function TWASIServiceApplication.GetRunEntryFunction: String;
begin
  Result:=FHost.RunEntryFunction;
end;

function TWASIServiceApplication.GetTableDescriptor: TJSWebAssemblyTableDescriptor;
begin
  Result:=FHost.TableDescriptor;
end;

procedure TWASIServiceApplication.SetAfterStart(AValue: TAfterStartEvent);
begin
  FHost.AfterStart:=aValue;
end;

procedure TWASIServiceApplication.SetBeforeStart(AValue: TBeforeStartEvent);
begin
  FHost.BeforeStart:=aValue;
end;

procedure TWASIServiceApplication.SetMemoryDescriptor(
  AValue: TJSWebAssemblyMemoryDescriptor);
begin
  FHost.MemoryDescriptor:=aValue;
end;

procedure TWASIServiceApplication.SetPredefinedConsoleInput(AValue: TStrings);
begin
  FHost.PredefinedConsoleInput:=aValue;
end;

procedure TWASIServiceApplication.SetRunEntryFunction(AValue: String);
begin
  FHost.RunEntryFunction:=aValue;
end;

procedure TWASIServiceApplication.SetTableDescriptor(
  AValue: TJSWebAssemblyTableDescriptor);
begin
  FHost.TableDescriptor:=aValue;
end;

function TWASIServiceApplication.CreateHost : TWASIHost;

begin
  Result:=TWASIHost.Create(Nil);
end;

constructor TWASIServiceApplication.Create(aOwner: TComponent);
begin
  inherited Create(aOwner);
  FHost:=CreateHost;
end;

destructor TWASIServiceApplication.Destroy;
begin
  FreeAndNil(FHost);
  inherited Destroy;
end;

procedure TWASIServiceApplication.StartWebAssembly(aPath: string; DoRun: Boolean;
  aBeforeStart: TBeforeStartCallback = nil; aAfterStart: TAfterStartCallback = nil);

begin
  FHost.StartWebAssembly(aPath,DoRun,aBeforeStart,aAfterStart);
end;

end.

