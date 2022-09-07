unit wasiworkerthreadhost;

{$mode ObjFPC}
{$modeswitch externalclass}

interface

uses
  Classes, SysUtils, JS, custapp, weborworker, webworker, webassembly, wasienv, Rtl.WebThreads;

Type
  TWorkerThreadSupport = Class;

  { TWASIThreadHost }

  TWASIThreadHost = class(TWASIHost)
  private
    FSendOutputToBrowserWindow: Boolean;
    FThreadEntryPoint: String;
    FThreadInitInstanceEntry : String;
    FThreadSupport: TWorkerThreadSupport;
    procedure SetThreadSupport(AValue: TWorkerThreadSupport);
  Protected
    Procedure RunWebAssemblyThread(aProc : TRunWebassemblyProc); virtual;
    Procedure PrepareWebAssemblyThread(aDescr : TWebAssemblyStartDescriptor); virtual;
    procedure DoStdWrite(Sender: TObject; const aOutput: String); override;
  Public
    constructor Create(aOwner: TComponent); override;
    // Thread entry point name for the WASI Host.
    Property ThreadEntryPoint : String Read FThreadEntryPoint Write FThreadEntryPoint;
    // Thread instance Init point name for the WASI Host.
    Property ThreadInitInstanceEntry : String Read FThreadInitInstanceEntry Write FThreadInitInstanceEntry;
    // Send output to main window
    Property SendOutputToBrowserWindow : Boolean Read FSendOutputToBrowserWindow Write FSendOutputToBrowserWindow;
    // our thread
    Property ThreadSupport : TWorkerThreadSupport Read FThreadSupport Write SetThreadSupport;
  end;


  // This object has the thread support that is needed by the worker that runs a thread.

  { TWorkerThreadSupport }

  TWorkerThreadSupport = class(TWasmThreadSupport)
  Private
    FStartThreadID : Integer;
    FNextThreadID : Integer;
    FCurrentThreadInfo : TThreadinfo;
    FModule : TJSWebAssemblyModule;
    FMemory : TJSWebAssemblyMemory;
    FWasiHost: TWASIThreadHost;
  Protected
    // Set new thread range
    procedure InitThreadRange(aRange: Integer);
    // allocate new thread ID.
    Function AllocateNewThreadID : NativeInt;
    // Incoming messages
    procedure LoadWasmModule(aCommand: TWorkerLoadCommand); virtual;
    procedure RunWasmModule(aCommand: TWorkerRunCommand); virtual;
    procedure CancelWasmModule(aCommand: TWorkerCancelCommand); virtual;
    procedure SetThreadRange(aCommand: TWorkerThreadIDRangeCommand); virtual;
    // outgoing messages
    procedure RequestNewThreadBlock; virtual;
    procedure SendLoaded; virtual;
    Procedure SendConsoleMessage(aMessage : String); overload;
    Procedure SendConsoleMessage(aFmt : String; const aArgs : array of const); overload;
    Procedure SendConsoleMessage(const aArgs : array of JSValue); overload;
    procedure SendException(aError: Exception); overload;
    procedure SendException(aError: TJSError); overload;
  Protected
    Function thread_spawn(thread_id : Integer; attrs: Integer; thread_start_func : Integer; args : Integer) : Integer;  override;
    Function thread_detach(thread_id : Integer) : Integer; override;
    Function thread_cancel(thread_id : Integer) : Integer; override;
    Function thread_self() : Integer; override;
  Public
    // Handle incoming command
    Procedure HandleCommand(aCommand : TWorkerCommand); override;
    // Current thread info.
    Property CurrentThreadInfo : TThreadInfo Read FCurrentThreadInfo;
    // The WASI host, used to run routines.
    Property Host : TWASIThreadHost Read FWasiHost Write FWasiHost;
  end;


  { TWorkerWASIHostApplication }

  TWorkerWASIHostApplication = class(TCustomApplication)
  private
    FHost : TWASIHost;
    FThreadSupport : TWorkerThreadSupport;
    FSendOutputToBrowser: Boolean;
    function GetAfterStart: TAfterStartEvent;
    function GetBeforeStart: TBeforeStartEvent;
    function GetcPredefinedConsoleInput: TStrings;
    function GetEnv: TPas2JSWASIEnvironment;
    function GetExported: TWASIExports;
    function GetOnConsoleRead: TConsoleReadEvent;
    function GetOnConsoleWrite: TConsoleWriteEvent;
    function GetRunEntryFunction: String;
    procedure SetAfterStart(AValue: TAfterStartEvent);
    procedure SetBeforeStart(AValue: TBeforeStartEvent);
    procedure SetOnConsoleRead(AValue: TConsoleReadEvent);
    procedure SetOnConsoleWrite(AValue: TConsoleWriteEvent);
    procedure SetPredefinedConsoleInput(AValue: TStrings);
    procedure SetRunEntryFunction(AValue: String);
  protected
    procedure HandleMessage(aEvent: TJSEvent); virtual;
    procedure DoOnSendCommand(Sender : TObject; aCommand : TWorkerCommand);
    function CreateHost: TWASIHost; virtual;
    procedure DoRun; override;
    function GetConsoleApplication: boolean; override;
    function GetLocation: String; override;
  public
    Constructor Create(aOwner : TComponent); override;
    Destructor Destroy; override;
    procedure SendCommand(aCommand: TWorkerCommand); virtual;
    procedure GetEnvironmentList(List: TStrings; NamesOnly: Boolean); override;
    procedure ShowException(E: Exception); override;
    // Load and start webassembly. If DoRun is true, then Webassembly entry point is called.
    // If aBeforeStart is specified, then it is called prior to calling run, and can disable running.
    // If aAfterStart is specified, then it is called after calling run. It is not called is running was disabled.
    Procedure StartWebAssembly(aPath: string; DoRun : Boolean = True;  aBeforeStart : TBeforeStartCallback = Nil; aAfterStart : TAfterStartCallback = Nil);
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
    // Send output to browser window process?
    Property SendOutputToBrowser : Boolean Read FSendOutputToBrowser Write FSendOutputToBrowser;
    // Default console input
    Property PredefinedConsoleInput : TStrings Read GetcPredefinedConsoleInput Write SetPredefinedConsoleInput;
    // Called when reading from console (stdin). If not set, PredefinedConsoleinput is used.
    property OnConsoleRead : TConsoleReadEvent Read GetOnConsoleRead Write SetOnConsoleRead;
    // Called when writing to console (stdout). If not set, console.log is used.
    property OnConsoleWrite : TConsoleWriteEvent Read GetOnConsoleWrite Write SetOnConsoleWrite;
    // Our thread support object
    Property ThreadSupport : TWorkerThreadSupport Read FThreadSupport Write FThreadSupport;
  end;

implementation

uses Types;

var
  Self_ : TJSDedicatedWorkerGlobalScope; external name 'self';
  EnvNames: TJSObject;

procedure ReloadEnvironmentStrings;

var
  I : Integer;
  S,N : String;
  A,P : TStringDynArray;

begin
  if Assigned(EnvNames) then
    FreeAndNil(EnvNames);
  EnvNames:=TJSObject.new;
  S:=self_.Location.search;
  S:=Copy(S,2,Length(S)-1);
  A:=TJSString(S).split('&');
  for I:=0 to Length(A)-1 do
    begin
    P:=TJSString(A[i]).split('=');
    N:=LowerCase(decodeURIComponent(P[0]));
    if Length(P)=2 then
      EnvNames[N]:=decodeURIComponent(P[1])
    else if Length(P)=1 then
      EnvNames[N]:=''
    end;
end;

function MyGetEnvironmentVariable(Const EnvVar: String): String;

Var
  aName : String;

begin
  aName:=Lowercase(EnvVar);
  if EnvNames.hasOwnProperty(aName) then
    Result:=String(EnvNames[aName])
  else
    Result:='';
end;

function MyGetEnvironmentVariableCount: Integer;
begin
  Result:=length(TJSOBject.getOwnPropertyNames(envNames));
end;

function MyGetEnvironmentString(Index: Integer): String;
begin
  Result:=String(EnvNames[TJSOBject.getOwnPropertyNames(envNames)[Index]]);
end;


{ TWASIThreadHost }

procedure TWASIThreadHost.SetThreadSupport(AValue: TWorkerThreadSupport);
begin
  if FThreadSupport=AValue then Exit;
  if Assigned(FThreadSupport) then
    FThreadSupport.Host:=Nil;
  FThreadSupport:=AValue;
  if Assigned(FThreadSupport) then
    FThreadSupport.Host:=Self;
end;

procedure TWASIThreadHost.RunWebAssemblyThread(aProc : TRunWebassemblyProc);
begin
//  Writeln('TWASIThreadHost.Entering RunWebAssemblyThread ');
  RunWebAssemblyInstance(Nil,Nil,aProc);
end;

procedure TWASIThreadHost.PrepareWebAssemblyThread( aDescr: TWebAssemblyStartDescriptor);

Var
  func : JSValue;
  InitFunc : TThreadInitInstanceFunction absolute func;
  res : Integer;

begin
  PrepareWebAssemblyInstance(aDescr);
  func:=aDescr.Exported[ThreadInitInstanceEntry];
  if Assigned(func) then
    begin
    res:=InitFunc(1,0,1);
    if Res<>0 then
      if Assigned(ThreadSupport) then
        ThreadSupport.SendConsoleMessage('Could not init assembly thread: %d', [Res])
      else
        Writeln('Could not init assembly thread: ',Res);
    end;
end;

procedure TWASIThreadHost.DoStdWrite(Sender: TObject; const aOutput: String);
begin
  inherited DoStdWrite(Sender, aOutput);
  if FSendOutputToBrowserWindow and assigned(FThreadSupport) then
    FThreadSupport.SendConsoleMessage(aOutput);
end;

constructor TWASIThreadHost.Create(aOwner: TComponent);
begin
  inherited Create(aOwner);
  FThreadEntryPoint:=DefaultThreadEntryPoint;
  FThreadInitInstanceEntry:=DefaultThreadInstanceInitPoint;
  FSendOutputToBrowserWindow:=True;
end;

{ TWorkerThreadSupport }

function TWorkerThreadSupport.thread_spawn(thread_id: Integer; attrs: Integer;
  thread_start_func: Integer; args: Integer): Integer;

Var
  P : TWorkerSpawnThreadCommand;

begin
  P:=TWorkerSpawnThreadCommand.Create(AllocateNewThreadID,Attrs,Args,thread_start_func,Thread_id);
  SendCommand(P);
  Env.SetMemInfoInt32(thread_id,P.ThreadID);
  Result:=0;
end;

function TWorkerThreadSupport.thread_detach(thread_id: Integer): Integer;
begin
  Result:=0;
end;

function TWorkerThreadSupport.thread_cancel(thread_id: Integer): Integer;
begin
  Result:=0;
end;

function TWorkerThreadSupport.thread_self: Integer;
begin
  Result:=0;
end;

function TWorkerThreadSupport.AllocateNewThreadID: NativeInt;

begin
  if (FNextThreadID-FStartThreadID)>=ThreadIDInterval then
    FNextThreadID:=FStartThreadID;
  Inc(FNextThreadID);
  if (FNextThreadID-FStartThreadID)=ThreadIDInterval-ThreadIDMargin then
    RequestNewThreadBlock;
  Result:=FNextThreadID;
end;

procedure TWorkerThreadSupport.SendLoaded;

Var
  L : TWorkerLoadedCommand;

begin
  L:=TWorkerLoadedCommand.Create();
  SendCommand(L);
end;

procedure TWorkerThreadSupport.SendConsoleMessage(aMessage: String);

Var
  L : TWorkerConsoleCommand;

begin
  L:=TWorkerConsoleCommand.Create(aMessage,FCurrentThreadInfo.ThreadId);
  SendCommand(L);
end;

procedure TWorkerThreadSupport.SendConsoleMessage(aFmt: String;
  const aArgs: array of const);
begin
  SendConsoleMessage(Format(aFmt,aArgs));
end;

procedure TWorkerThreadSupport.SendConsoleMessage(const aArgs: array of JSValue);

Var
  L : TWorkerConsoleCommand;

begin
  L:=TWorkerConsoleCommand.Create(aArgs,FCurrentThreadInfo.ThreadId);
  SendCommand(L);
end;

procedure TWorkerThreadSupport.CancelWasmModule(aCommand : TWorkerCancelCommand);

begin
  // todo
end;


procedure TWorkerThreadSupport.SendException(aError : Exception);

Var
  E : TWorkerExceptionCommand;

begin
  E:=TWorkerExceptionCommand.CreateNew(aError.ClassName,aError.Message,FCurrentThreadInfo.ThreadId);
  SendCommand(E);
end;

procedure TWorkerThreadSupport.SendException(aError: TJSError);

Var
  aMessage,aClass : String;
  E : TWorkerExceptionCommand;

begin
  aClass:='Error';
  aMessage:=aError.Message;
  E:=TWorkerExceptionCommand.CreateNew(aClass,aMessage,FCurrentThreadInfo.ThreadId);
  SendCommand(E);
end;


procedure TWorkerThreadSupport.RunWasmModule(aCommand : TWorkerRunCommand);

  Procedure DoRun (aExports : TWASIExports);

  Var
    aResult : Integer;

  begin
    try
      // Writeln('About to run webassembly entry point (',Host.ThreadEntryPoint,') for thread ID ',aCommand.ThreadID);
      aResult:=TThreadEntryPointFunction(aExports[Host.ThreadEntryPoint])(aCommand.ThreadInfo,aCommand.RunThreadProc, aCommand.args);
      if aResult>0 then
        Writeln('Thread run function result ',aResult);
    except
      on E : Exception do
        SendException(E);
      on JE : TJSError do
        SendException(JE);
      on JE : TJSError do
        SendException(JE)
    end;

  end;

begin
  // Writeln('Entering TWorkerThreadSupport.RunWasmModule '+TJSJSON.Stringify(aCommand));
  // initialize current thread info
  FCurrentThreadInfo.ThreadID:=aCommand.ThreadID;
  FCurrentThreadInfo.Arguments:=aCommand.Args;
  FCurrentThreadInfo.ThreadInfoLocation:=aCommand.ThreadInfo;
  FCurrentThreadInfo.Attributes:=aCommand.Attrs;
  FCurrentThreadInfo.RunFunction:=aCommand.RunThreadProc;
  Host.RunWebAssemblyThread(@DoRun);
end;

procedure TWorkerThreadSupport.LoadWasmModule(aCommand: TWorkerLoadCommand);


Var
  WASD : TWebAssemblyStartDescriptor;
  aTable : TJSWebAssemblyTable;

  function doOK(aValue: JSValue): JSValue;
  // We are using the overload that takes a compiled module.
  // In that case the promise resolves to a WebAssembly.Instance, not to a InstantiateResult !
  Var
    aInstance : TJSWebAssemblyInstance absolute aValue;

  begin
    Result:=True;
    WASD.Instance:=aInstance;
    WASD.Exported:=TWASIExports(TJSObject(aInstance.exports_));
    WASD.CallRun:=Nil;
    Host.PrepareWebAssemblyThread(WASD);
    SendLoaded;
    // These 2 prevent running different instances simultaneously.
  end;

  function DoFail(aValue: JSValue): JSValue;

  var
    E: Exception;

  begin
    Result:=True;
    E:=Exception.Create('Failed to create webassembly. Reason: '+TJSJSON.Stringify(aValue));
    SendException(E);
    E.Free;
  end;


begin
  FMemory:=aCommand.Memory;
  FModule:=aCommand.Module;
  InitThreadRange(aCommand.ThreadRangeStart);
  try
    aTable:=TJSWebAssemblyTable.New(Host.TableDescriptor);
    WASD:=Host.InitStartDescriptor(FMemory,aTable,Nil);
    TJSWebAssembly.Instantiate(FModule,WASD.Imports)._then(@DoOK,@DoFail).Catch(@DoFail);
  except
    on E : Exception do
      SendException(E);
    on JE : TJSError do
      SendException(JE);
  end;
end;


procedure TWorkerThreadSupport.InitThreadRange(aRange: Integer);

begin
  FStartThreadID:=aRange;
  FNextThreadID:=FStartThreadID;
end;

procedure TWorkerThreadSupport.RequestNewThreadBlock;

begin
  SendCommand(TWorkerNeedIdBlockCommand.Create(FNextThreadID));
end;

procedure TWorkerThreadSupport.SetThreadRange(
  aCommand: TWorkerThreadIDRangeCommand);

begin
  InitThreadRange(aCommand.RangeStart);
end;

procedure TWorkerThreadSupport.HandleCommand(aCommand: TWorkerCommand);

begin
  case aCommand.Command of
    cmdload : LoadWasmModule(TWorkerLoadCommand(aCommand));
    cmdRun : RunWasmModule(TWorkerRunCommand(aCommand));
    cmdCancel : CancelWasmModule(TWorkerCancelCommand(aCommand));
    cmdThreadIdRange : SetThreadRange(TWorkerThreadIDRangeCommand(aCommand));
  end;
end;



{ TWorkerWASIHostApplication }

function TWorkerWASIHostApplication.GetAfterStart: TAfterStartEvent;
begin
  Result:=FHost.AfterStart;
end;

function TWorkerWASIHostApplication.GetBeforeStart: TBeforeStartEvent;
begin
  Result:=FHost.BeforeStart;
end;

function TWorkerWASIHostApplication.GetcPredefinedConsoleInput: TStrings;
begin
  Result:=FHost.PredefinedConsoleInput;
end;

function TWorkerWASIHostApplication.GetEnv: TPas2JSWASIEnvironment;
begin
  Result:=FHost.WasiEnvironment;
end;

function TWorkerWASIHostApplication.GetExported: TWASIExports;
begin
  Result:=FHost.Exported;
end;


function TWorkerWASIHostApplication.GetOnConsoleRead: TConsoleReadEvent;
begin
  Result:=FHost.OnConsoleRead;
end;

function TWorkerWASIHostApplication.GetOnConsoleWrite: TConsoleWriteEvent;
begin
  Result:=FHost.OnConsoleWrite;
end;

function TWorkerWASIHostApplication.GetRunEntryFunction: String;
begin
  Result:=FHost.RunEntryFunction;
end;


procedure TWorkerWASIHostApplication.SetAfterStart(AValue: TAfterStartEvent);
begin
  FHost.AfterStart:=aValue;
end;

procedure TWorkerWASIHostApplication.SetBeforeStart(AValue: TBeforeStartEvent);
begin
  FHost.BeforeStart:=aValue;
end;

procedure TWorkerWASIHostApplication.SetOnConsoleRead(AValue: TConsoleReadEvent
  );
begin
  FHost.OnConsoleRead:=aValue;
end;

procedure TWorkerWASIHostApplication.SetOnConsoleWrite(
  AValue: TConsoleWriteEvent);
begin
    FHost.OnConsoleWrite:=aValue;
end;

procedure TWorkerWASIHostApplication.SetPredefinedConsoleInput(AValue: TStrings);
begin
  FHost.PredefinedConsoleInput:=aValue;
end;

procedure TWorkerWASIHostApplication.SetRunEntryFunction(AValue: String);
begin
  FHost.RunEntryFunction:=aValue;
end;

function TWorkerWASIHostApplication.CreateHost : TWASIHost;

Var
  TH : TWasiThreadHost;

begin
  TH:=TWASIThreadHost.Create(Self);
  FThreadSupport:=TWorkerThreadSupport.Create(TH.WasiEnvironment);
  FThreadSupport.OnSendCommand:=@DoOnSendCommand;
  TH.ThreadSupport:=FThreadSupport; // Sets FThreadSupport.host
  Result:=TH;
end;

procedure TWorkerWASIHostApplication.DoRun;
begin
  Self_.addEventListener('message',@HandleMessage);
end;

procedure TWorkerWASIHostApplication.HandleMessage(aEvent: TJSEvent);

Var
  aMessageEvent : TJSMessageEvent absolute aEvent;
  aData : TWorkerCommand;

begin
  if IsObject(aMessageEvent.Data) and TJSObject(aMessageEvent.Data).hasOwnProperty('Command') then
    begin
    aData:=TWorkerCommand(aMessageEvent.Data);
    FThreadSupport.HandleCommand(aData);
    end
  else
    FThreadSupport.SendConsoleMessage('Unknown message received: '+TJSJSON.Stringify(aMessageEvent.Data));
end;

procedure TWorkerWASIHostApplication.DoOnSendCommand(Sender: TObject;
  aCommand: TWorkerCommand);
begin
  SendCommand(aCommand);
end;

procedure TWorkerWASIHostApplication.SendCommand(aCommand: TWorkerCommand);
begin
  Self_.PostMessage(aCommand);
end;

function TWorkerWASIHostApplication.GetConsoleApplication: boolean;
begin
  Result:=true;
end;

function TWorkerWASIHostApplication.GetLocation: String;
begin
  Result:=webworker.Location.pathname;
end;

constructor TWorkerWASIHostApplication.Create(aOwner: TComponent);
begin
  inherited Create(aOwner);
  FHost:=CreateHost;
end;

destructor TWorkerWASIHostApplication.Destroy;
begin
  FreeAndNil(FHost);
  inherited Destroy;
end;

procedure TWorkerWASIHostApplication.GetEnvironmentList(List: TStrings;
  NamesOnly: Boolean);
var
  Names: TStringDynArray;
  i: Integer;
begin
  Names:=TJSObject.getOwnPropertyNames(EnvNames);
  for i:=0 to length(Names)-1 do
  begin
    if NamesOnly then
      List.Add(Names[i])
    else
      List.Add(Names[i]+'='+String(EnvNames[Names[i]]));
  end;
end;

procedure TWorkerWASIHostApplication.ShowException(E: Exception);

begin
  ThreadSupport.SendException(E);
end;

procedure TWorkerWASIHostApplication.StartWebAssembly(aPath: string; DoRun: Boolean;
  aBeforeStart: TBeforeStartCallback = nil; aAfterStart: TAfterStartCallback = nil);

begin
  FHost.StartWebAssembly(aPath,DoRun,aBeforeStart,aAfterStart);
end;

Initialization
  ReloadEnvironmentStrings;
  OnGetEnvironmentVariable:=@MyGetEnvironmentVariable;
  OnGetEnvironmentVariableCount:=@MyGetEnvironmentVariableCount;
  OnGetEnvironmentString:=@MyGetEnvironmentString;
end.

