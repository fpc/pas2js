unit wasihostapp;

{$mode ObjFPC}

interface

uses
  Classes, SysUtils, browserapp,  webassembly, wasienv;

Type

  { TBrowserWASIHostApplication }

  TBrowserWASIHostApplication = class(TBrowserApplication)
  private
    FHost : TWASIHost;
    FPredefinedConsoleInput: TStrings;
    function GetAfterStart: TAfterStartEvent;
    function GetBeforeStart: TBeforeStartEvent;
    function GetEnv: TPas2JSWASIEnvironment;
    function GetExported: TWASIExports;
    function GetMemoryDescriptor: TJSWebAssemblyMemoryDescriptor;
    function GetOnConsoleRead: TConsoleReadEvent;
    function GetOnConsoleWrite: TConsoleWriteEvent;
    function GetRunEntryFunction: String;
    function GetTableDescriptor: TJSWebAssemblyTableDescriptor;
    procedure SetAfterStart(AValue: TAfterStartEvent);
    procedure SetBeforeStart(AValue: TBeforeStartEvent);
    procedure SetMemoryDescriptor(AValue: TJSWebAssemblyMemoryDescriptor);
    procedure SetOnConsoleRead(AValue: TConsoleReadEvent);
    procedure SetOnConsoleWrite(AValue: TConsoleWriteEvent);
    procedure SetPredefinedConsoleInput(AValue: TStrings);
    procedure SetRunEntryFunction(AValue: String);
    procedure SetTableDescriptor(AValue: TJSWebAssemblyTableDescriptor);
  protected
    function CreateHost: TWASIHost; virtual;
    Property Host : TWASIHost Read FHost;
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
    property OnConsoleRead : TConsoleReadEvent Read GetOnConsoleRead Write SetOnConsoleRead;
    // Called when writing to console (stdout). If not set, console.log is used.
    property OnConsoleWrite : TConsoleWriteEvent Read GetOnConsoleWrite Write SetOnConsoleWrite;
  end;

  // For backwards compatibility

  TWASIHostApplication = TBrowserWASIHostApplication;

implementation

{ TBrowserWASIHostApplication }

function TBrowserWASIHostApplication.GetAfterStart: TAfterStartEvent;
begin
  Result:=FHost.AfterStart;
end;

function TBrowserWASIHostApplication.GetBeforeStart: TBeforeStartEvent;
begin
  Result:=FHost.BeforeStart;
end;

function TBrowserWASIHostApplication.GetEnv: TPas2JSWASIEnvironment;
begin
  Result:=FHost.WasiEnvironment;
end;

function TBrowserWASIHostApplication.GetExported: TWASIExports;
begin
  Result:=FHost.Exported;
end;

function TBrowserWASIHostApplication.GetMemoryDescriptor: TJSWebAssemblyMemoryDescriptor;
begin
  Result:=FHost.MemoryDescriptor;
end;

function TBrowserWASIHostApplication.GetOnConsoleRead: TConsoleReadEvent;
begin
  Result:=FHost.OnConsoleRead;
end;

function TBrowserWASIHostApplication.GetOnConsoleWrite: TConsoleWriteEvent;
begin
  Result:=FHost.OnConsoleWrite;
end;

function TBrowserWASIHostApplication.GetRunEntryFunction: String;
begin
  Result:=FHost.RunEntryFunction;
end;

function TBrowserWASIHostApplication.GetTableDescriptor: TJSWebAssemblyTableDescriptor;
begin
  Result:=FHost.TableDescriptor;
end;

procedure TBrowserWASIHostApplication.SetAfterStart(AValue: TAfterStartEvent);
begin
  FHost.AfterStart:=aValue;
end;

procedure TBrowserWASIHostApplication.SetBeforeStart(AValue: TBeforeStartEvent);
begin
  FHost.BeforeStart:=aValue;
end;

procedure TBrowserWASIHostApplication.SetMemoryDescriptor(
  AValue: TJSWebAssemblyMemoryDescriptor);
begin
  FHost.MemoryDescriptor:=aValue;
end;

procedure TBrowserWASIHostApplication.SetOnConsoleRead(AValue: TConsoleReadEvent
  );
begin
  FHost.OnConsoleRead:=aValue
end;

procedure TBrowserWASIHostApplication.SetOnConsoleWrite(
  AValue: TConsoleWriteEvent);
begin
  FHost.OnConsoleWrite:=aValue;
end;

procedure TBrowserWASIHostApplication.SetPredefinedConsoleInput(AValue: TStrings);
begin
  FHost.PredefinedConsoleInput:=aValue;
end;

procedure TBrowserWASIHostApplication.SetRunEntryFunction(AValue: String);
begin
  FHost.RunEntryFunction:=aValue;
end;

procedure TBrowserWASIHostApplication.SetTableDescriptor(
  AValue: TJSWebAssemblyTableDescriptor);
begin
  FHost.TableDescriptor:=aValue;
end;

function TBrowserWASIHostApplication.CreateHost : TWASIHost;

begin
  Result:=TWASIHost.Create(Self);
end;

constructor TBrowserWASIHostApplication.Create(aOwner: TComponent);
begin
  inherited Create(aOwner);
  FHost:=CreateHost;
end;

destructor TBrowserWASIHostApplication.Destroy;
begin
  FreeAndNil(FHost);
  inherited Destroy;
end;

procedure TBrowserWASIHostApplication.StartWebAssembly(aPath: string; DoRun: Boolean;
  aBeforeStart: TBeforeStartCallback = nil; aAfterStart: TAfterStartCallback = nil);

begin
  FHost.StartWebAssembly(aPath,DoRun,aBeforeStart,aAfterStart);
end;

end.

