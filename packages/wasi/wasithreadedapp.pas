unit wasithreadedapp;

{$mode ObjFPC}
{$modeswitch externalclass}
{$modeswitch typehelpers}

interface

uses
  JS, Classes, SysUtils, Rtl.WebThreads, wasienv, wasihostapp, weborworker;

Type
  { TWasmThread }
  TWasmThread = TJSWorker;

  { TWasmThreadHelper }

  TWasmThreadHelper = Class helper for TWasmThread
  private
    function GetLoaded: Boolean;
    function GetLoadSent: Boolean;
    function GetThreadID: Integer;
    function GetThreadIDRange: Integer;
    function GetThreadInfo: TThreadinfo;
    function GetThreadLocation: Integer;
    procedure SetLoaded(AValue: Boolean);
    procedure SetLoadSent(AValue: Boolean);
    procedure SetThreadID(AValue: Integer);
    procedure SetThreadIDRange(AValue: Integer);
    procedure SetThreadInfo(AValue: TThreadinfo);
    procedure SetThreadLocation(AValue: Integer);
  Public
    Class function Create(aScript : String) : TWasmThread; reintroduce; static;
    Procedure SendCommand(aCommand : TWorkerCommand);
    Property LoadSent : Boolean Read GetLoadSent Write SetLoadSent;
    Property Loaded : Boolean Read GetLoaded Write SetLoaded;
    Property ThreadInfo : TThreadinfo Read GetThreadInfo Write SetThreadInfo;
    Property ThreadID : Integer Read GetThreadID Write SetThreadID;
    Property ThreadIDRange : Integer Read GetThreadIDRange Write SetThreadIDRange;
    Property ThreadLocation : Integer Read GetThreadLocation Write SetThreadLocation;
  end;



  TThreadHash = class external name 'Object' (TJSObject)
  Private
    function GetThreadData(aIndex: NativeInt): TWasmThread; external name '[]';
    procedure SetThreadData(aIndex: NativeInt; const AValue: TWasmThread); external name '[]';
  Public
    Property ThreadData[aIndex : NativeInt] : TWasmThread Read GetThreadData Write SetThreadData; default;
  end;


  // This object has the thread support that is needed  by the 'main' program

  { TMainThreadSupport }

  TMainThreadSupport = class(TWasmThreadSupport)
  private
    FInitialWorkerCount: Integer;
    FMaxWorkerCount: Integer;
    FOnUnknownMessage: TJSRawEventHandler;
    FHost: TWASIHost;
    FWorkerScript: String;
    FNextIDRange : Integer;
    FNextThreadID : Integer;
    procedure SetWasiHost(AValue: TWASIHost);
  Protected
    Function thread_spawn(thread_id : Integer; attrs: Integer; thread_start_func : Integer; args : Integer) : Integer;  override;
    Function thread_detach(thread_id : Integer) : Integer; override;
    Function thread_cancel(thread_id : Integer) : Integer; override;
    Function thread_self() : Integer; override;
    function AllocateThreadID : Integer;
  Protected
    FIdleWorkers : Array of TWasmThread;
    FBusyWorkers : Array of TWasmThread;
    FThreads : TThreadHash; // ThreadID is key,
    // Send load commands to all workers that still need it.
    procedure SendLoadCommands;
    // Allocate new thread ID range
    function GetNextThreadIDRange: Integer;
    // Handle worker messages. If it is a command, it is set to handlecommand.
    procedure DoWorkerMessage(aEvent: TJSEvent);
    // Create & set up new worker
    Function AllocateNewWorker(Const aWorkerScript : string) : TWasmThread;
    // Send a load command
    procedure SendLoadCommand(aThreadWorker: TWasmThread); virtual;
    // Get new worker from pool, create new if needed.
    Function GetNewWorker : TWasmThread;
    // Spawn & prepare to run a new thread.
    Function SpawnThread(aInfo : TThreadInfo) : Integer;
    // Actually send run command.
    Procedure SendRunCommand(aThreadWorker: TWasmThread);
    //
    // Handle Various commands sent from worker threads.
    //
    // Allocate a new worker for a thread and run the thread if the worker is loaded.
    procedure HandleSpawnCommand(aWorker: TWasmThread; aCommand: TWorkerSpawnThreadCommand); virtual;
    // Cancel command: stop the thread
    procedure HandleCancelCommand(aWorker: TWasmThread; aCommand: TWorkerCancelCommand); virtual;
    // Cleanup thread : after join (or stopped if detached), free worker.
    procedure HandleCleanupCommand(aWorker: TWasmThread; aCommand: TWorkerCleanupCommand); virtual;
    // forward KILL signal to thread.
    procedure HandleKillCommand(aWorker: TWasmThread; aCommand: TWorkerKillCommand); virtual;
    // Worker script is loaded, has loaded webassembly and is ready to run.
    procedure HandleLoadedCommand(aWorker: TWasmThread; aCommand: TWorkerLoadedCommand); overload;
    // Console output from worker.
    procedure HandleConsoleCommand(aWorker: TWasmThread;  aCommand: TWorkerConsoleCommand);
  Public
    Constructor Create(aEnv : TPas2JSWASIEnvironment); override;
    Constructor Create(aEnv : TPas2JSWASIEnvironment; aWorkerScript : String; aSpawnWorkerCount : integer); virtual; overload;
    Procedure HandleCommand(aWorker : TWasmThread; aCommand : TWorkerCommand); overload; virtual;
    Property WorkerScript : String Read FWorkerScript;
    // Initial number of threads, set by constructor
    Property InitialWorkerCount : Integer Read FInitialWorkerCount;
    // Maximum number of workers. If more workers are requested, the GetNewWorker will return Nil.
    Property MaxWorkerCount : Integer Read FMaxWorkerCount Write FMaxWorkerCount;
    Property OnUnknownMessage : TJSRawEventHandler Read FOnUnknownMessage Write FOnUnknownMessage;
    // The WASI host, used to run routines.
    Property Host : TWASIHost Read FHost Write SetWasiHost;
  end;

  { TBrowserWASIThreadedHostApplication }

  TBrowserWASIThreadedHostApplication = class(TBrowserWASIHostApplication)
  private
    FThreadSupport: TMainThreadSupport;
  protected
    Function CreateThreadSupport(aEnv : TPas2JSWASIEnvironment) : TMainThreadSupport; virtual;
    Function CreateHost: TWASIHost; override;
  Public
    Destructor Destroy; override;
    Property ThreadSupport : TMainThreadSupport Read FThreadSupport;
  end;

  { ThreadAppWASIHost }

  ThreadAppWASIHost = class(TWASIHost)
  private
    FThreadInitInstanceEntry: String;
    FThreadSupport: TMainThreadSupport;

    procedure SetThreadSupport(AValue: TMainThreadSupport);
  Protected
    Procedure PrepareWebAssemblyInstance(aDescr: TWebAssemblyStartDescriptor); override;
    Procedure DoAfterInstantiate; override;
  Public
     constructor Create(aOwner: TComponent); override;

    Property ThreadSupport : TMainThreadSupport Read FThreadSupport Write SetThreadSupport;
    // Thread instance Init point name for the WASI Host.
    Property ThreadInitInstanceEntry : String Read FThreadInitInstanceEntry Write FThreadInitInstanceEntry;
  end;


implementation

Resourcestring
  SErrMaxWorkersReached = 'Cannot create thread worker, Maximum number of workers (%d) reached.';

{ ThreadAppWASIHost }

procedure ThreadAppWASIHost.SetThreadSupport(AValue: TMainThreadSupport);
begin
  if FThreadSupport=AValue then Exit;
  FThreadSupport:=AValue;
  FThreadSupport.Host:=Self;
end;

procedure ThreadAppWASIHost.PrepareWebAssemblyInstance(
  aDescr: TWebAssemblyStartDescriptor);
Var
  func : JSValue;
  InitFunc : TThreadInitInstanceFunction absolute func;
  Res : Integer;

begin
  inherited;
  Writeln('PrepareWebAssemblyInstance: check init thread');
  func:=aDescr.Exported[ThreadInitInstanceEntry];
  if Assigned(func) then
    begin
    Writeln('Initializing main thread instance');
    res:=InitFunc(0,1,0);
    if Res<>0 then
      Writeln('Failed to initialize thread');
    end;
end;

procedure ThreadAppWASIHost.DoAfterInstantiate;
begin
  inherited DoAfterInstantiate;
  If Assigned(FThreadSupport) then
    FThreadSupport.SendLoadCommands;
end;

constructor ThreadAppWASIHost.Create(aOwner: TComponent);
begin
  inherited Create(aOwner);
  ThreadInitInstanceEntry:=DefaultThreadInstanceInitPoint;
end;


{ TBrowserWASIThreadedHostApplication }

function TBrowserWASIThreadedHostApplication.CreateThreadSupport(
  aEnv: TPas2JSWASIEnvironment): TMainThreadSupport;
begin
  Result:=TMainThreadSupport.Create(aEnv);
end;

function TBrowserWASIThreadedHostApplication.CreateHost: TWASIHost;

Var
  Res : ThreadAppWASIHost;

begin
  Res:=ThreadAppWASIHost.Create(Self);
  Res.UseSharedMemory:=True;
  Res.ThreadSupport:=CreateThreadSupport(Res.WasiEnvironment);
  Result:=Res;
end;


destructor TBrowserWASIThreadedHostApplication.Destroy;
begin
  FreeAndNil(FThreadSupport);
  inherited Destroy;
end;


{ TWasmThread }


class function TWasmThreadHelper.Create(aScript: String): TWasmThread;
begin
  Result:=TJSWorker.new(aScript);
  Result.ThreadID:=-1;
  Result.Loaded:=False;
  Result.LoadSent:=False;
  Result.ThreadIDRange:=-1;
  Result.ThreadInfo:=Default(TThreadInfo);
end;

function TWasmThreadHelper.GetLoaded: Boolean;
Var
  S : JSValue;
begin
  S:=Properties['FLoaded'];
  if isBoolean(S) then
    Result:=Boolean(S)
  else
    Result:=False;
end;

function TWasmThreadHelper.GetLoadSent: Boolean;

Var
  S : JSValue;
begin
  S:=Properties['FLoadSent'];
  if isBoolean(S) then
    Result:=Boolean(S)
  else
    Result:=False;
end;

function TWasmThreadHelper.GetThreadID: Integer;
begin
  Result:=ThreadInfo.ThreadID;
end;

function TWasmThreadHelper.GetThreadIDRange: Integer;
Var
  S : JSValue;
begin
  S:=Properties['FThreadIDRange'];
  if isNumber(S) then
    Result:=Integer(S)
  else
    Result:=0;
end;

function TWasmThreadHelper.GetThreadInfo: TThreadinfo;
Var
  S : JSValue;
begin
  S:=Properties['FThreadInfo'];
  if isObject(S) then
    Result:=TThreadinfo(S)
  else
    Result:=Default(TThreadInfo);
end;

function TWasmThreadHelper.GetThreadLocation: Integer;
begin
  Result:=ThreadInfo.ThreadInfoLocation;
end;

procedure TWasmThreadHelper.SetLoaded(AValue: Boolean);
begin
  Properties['FLoaded']:=aValue
end;

procedure TWasmThreadHelper.SetLoadSent(AValue: Boolean);
begin
  Properties['FLoadSent']:=aValue;
end;



procedure TWasmThreadHelper.SetThreadID(AValue: Integer);
begin
  ThreadInfo.ThreadID:=aValue;
end;

procedure TWasmThreadHelper.SetThreadIDRange(AValue: Integer);
begin
  Properties['FThreadIDRange']:=aValue
end;

procedure TWasmThreadHelper.SetThreadInfo(AValue: TThreadinfo);
begin
  Properties['FThreadInfo']:=aValue
end;

procedure TWasmThreadHelper.SetThreadLocation(AValue: Integer);
begin
  ThreadInfo.ThreadInfoLocation:=aValue
end;


procedure TWasmThreadHelper.SendCommand(aCommand: TWorkerCommand);
begin
  // Writeln('Sending command '+TJSJSON.Stringify(aCommand));
  PostMessage(aCommand);
end;

procedure TMainThreadSupport.DoWorkerMessage(aEvent: TJSEvent);

Var
  aMessageEvent : TJSMessageEvent absolute aEvent;
  aData : TWorkerCommand;
  aWorker : TWasmThread;

begin
  // Writeln('Received worker message '+TJSJSON.Stringify(aMessageEvent.Data));
  if IsObject(aMessageEvent.Data) and TJSObject(aMessageEvent.Data).hasOwnProperty('Command') then
    begin
    aData:=TWorkerCommand(aMessageEvent.Data);
    aWorker:=TWasmThread(aMessageEvent.Target);
    HandleCommand(aWorker,aData);
    end
  else if Assigned(FOnUnknownMessage) then
    FOnUnknownMessage(aEvent)
  else
    Writeln('Unknown worker message : ',TJSJSON.stringify(aEvent));
end;

function TMainThreadSupport.GetNextThreadIDRange : Integer;

begin
  Inc(FNextIDRange,ThreadIDInterval);
  Result:=FNextIDRange;
end;

function TMainThreadSupport.AllocateNewWorker(const aWorkerScript: string): TWasmThread;

begin
  // Writeln('Allocating new worker for: '+aWorkerScript);
  Result:=TWasmThread.Create(aWorkerScript);
  Result.ThreadIDRange:=GetNextThreadIDRange;
  Result.addEventListener('message',@DoWorkerMessage);
  if Assigned(Host) and Host.StartDescriptorReady then
    SendLoadCommand(Result)
  else
    Writeln('Host not set, delaying sending load command.'+aWorkerScript);
end;

procedure TMainThreadSupport.SendLoadCommand(aThreadWorker: TWasmThread);

Var
  WLC: TWorkerLoadCommand;

begin
  WLC:=TWorkerLoadCommand.Create(aThreadWorker.ThreadIDRange, Host.PreparedStartDescriptor.Module, Host.PreparedStartDescriptor.Memory);
  aThreadWorker.SendCommand(WLC);
  aThreadWorker.LoadSent:=True;
end;

function TMainThreadSupport.GetNewWorker: TWasmThread;

Var
  WT : TWasmThread;

begin
  if Length(FIdleWorkers)=0 then
    begin
    // Writeln('No idle workers, creating new one');
    if Length(FBusyWorkers)<MaxWorkerCount then
      WT:=AllocateNewWorker(FWorkerScript)
    else
      Raise EWasmThreads.Create(SErrMaxWorkersReached);
    end
  else
    begin
    WT:=TWasmThread(TJSArray(FIdleWorkers).pop);
    end;
  TJSArray(FBusyWorkers).Push(WT);
  Result:=WT;
end;


procedure TMainThreadSupport.SendRunCommand(aThreadWorker: TWasmThread);

Var
  WRC : TWorkerRunCommand;

begin
  With aThreadWorker.ThreadInfo do
    WRC:=TWorkerRunCommand.Create(ThreadID,RunFunction,Attributes,Arguments,ThreadInfoLocation);
  aThreadWorker.SendCommand(Wrc);
end;

procedure TMainThreadSupport.SetWasiHost(AValue: TWASIHost);


begin
  // Writeln('Setting wasi host');
  if FHost=AValue then
    Exit;
  FHost:=AValue;
  If Assigned(FHost) and Host.StartDescriptorReady then
    SendLoadCommands;
end;

function TMainThreadSupport.thread_spawn(thread_id: Integer; attrs: Integer;
  thread_start_func: Integer; args: Integer): Integer;

var
  aInfo : TThreadInfo;

begin
  // Writeln('In host thread_spawn');
  aInfo.ThreadID:=AllocateThreadID;
  aInfo.RunFunction:=thread_start_func;
  aInfo.Arguments:=Args;
  aInfo.Attributes:=Attrs;
  aInfo.OriginThreadID:=0;
  aInfo.ThreadInfoLocation:=thread_id;
  Env.SetMemInfoInt32(thread_id,aInfo.ThreadID);
  Result:=SpawnThread(aInfo);
end;

function TMainThreadSupport.thread_detach(thread_id: Integer): Integer;
begin
  Result:=0;
end;

function TMainThreadSupport.thread_cancel(thread_id: Integer): Integer;
begin
  Result:=0;
end;

function TMainThreadSupport.thread_self: Integer;
begin
  Result:=0;
end;

function TMainThreadSupport.AllocateThreadID: Integer;
begin
  Inc(FNextThreadID);
  Result:=FNextThreadID;
end;

procedure TMainThreadSupport.SendLoadCommands;

Var
  WT : TWasmThread;

begin
  // Writeln('Sending load command to all workers');
  For WT in FIdleWorkers do
    if not WT.LoadSent then
      SendLoadCommand(WT);
end;

function TMainThreadSupport.SpawnThread(aInfo: TThreadInfo): Integer;

Var
  WT : TWasmThread;

begin
  // Writeln('Enter TMainThreadSupport.SpawnThread for ID ',aInfo.ThreadID);
  WT:=GetNewWorker;
  if WT=nil then
    begin
    Writeln('Error: no worker !');
    exit(-1)
    end;
  WT.ThreadInfo:=aInfo;
  FThreads[aInfo.ThreadID]:=WT;
  if WT.Loaded then
    begin
    // Writeln('Worker is loaded. Sending run command to worker');
    SendRunCommand(WT);
    end;
  // Writeln('Exit: TMainThreadSupport.SpawnThread for ID ',WT.ThreadID);
end;


constructor TMainThreadSupport.Create(aEnv: TPas2JSWASIEnvironment);
begin
  Create(aEnv,DefaultThreadWorker,DefaultThreadCount)
end;

constructor TMainThreadSupport.Create(aEnv: TPas2JSWASIEnvironment;
  aWorkerScript: String; aSpawnWorkerCount: integer);

Var
  I : Integer;

begin
  Inherited Create(aEnv);
  FThreads:=TThreadHash.new;
  FWorkerScript:=aWorkerScript;
  FInitialWorkerCount:=aSpawnWorkerCount;
  FMaxWorkerCount:=DefaultMaxWorkerCount;
  For I:=1 to aSpawnWorkerCount do
    TJSArray(FIdleWorkers).Push(AllocateNewWorker(aWorkerScript));
end;

procedure TMainThreadSupport.HandleSpawnCommand(aWorker : TWasmThread; aCommand: TWorkerSpawnThreadCommand);

Var
  aInfo: TThreadInfo;

begin
  aInfo.OriginThreadID:=aWorker.ThreadID;
  aInfo.RunFunction:=aCommand.RunFunction;
  aInfo.ThreadID:=aCommand.ThreadID;
  aInfo.Arguments:=aCommand.Arguments;
  aInfo.Attributes:=aCommand.Attributes;
  SpawnThread(aInfo);
end;

procedure TMainThreadSupport.HandleKillCommand(aWorker : TWasmThread; aCommand: TWorkerKillCommand);

begin

end;

procedure TMainThreadSupport.HandleCancelCommand(aWorker : TWasmThread; aCommand: TWorkerCancelCommand);

begin

end;

procedure TMainThreadSupport.HandleLoadedCommand(aWorker : TWasmThread; aCommand: TWorkerLoadedCommand);

begin
  // Writeln('Host: Entering TMainThreadSupport.HandleLoadedCommand');
  aWorker.Loaded:=True;
  // if a thread is scheduled to run in this thread, run it.
  if aWorker.ThreadID>0 then
    SendRunCommand(aWorker);
  // Writeln('Host: exiting TMainThreadSupport.HandleLoadedCommand');
end;

procedure TMainThreadSupport.HandleCleanupCommand(aWorker : TWasmThread; aCommand: TWorkerCleanupCommand);

Var
  Idx : Integer;

begin
  aWorker.ThreadInfo:=Default(TThreadInfo);
  Idx:=TJSarray(FBusyWorkers).indexOf(aWorker);
  if Idx<>-1 then
    Delete(FBusyWorkers,Idx,1);
  Idx:=TJSarray(FIdleWorkers).indexOf(aWorker);
  if Idx=-1 then
    FIdleWorkers:=Concat(FIdleWorkers,[aWorker]);
end;

procedure TMainThreadSupport.HandleConsoleCommand(aWorker : TWasmThread; aCommand: TWorkerConsoleCommand);

Var
  Prefix : string;

begin
  Prefix:=Format('Wasm thread %d: ',[aWorker.ThreadID]);
  if Assigned(Host.OnConsoleWrite) then
    Host.OnConsoleWrite(Host,Prefix+aCommand.ConsoleMessage)
  else
    Writeln(Prefix+aCommand.ConsoleMessage);
end;

procedure TMainThreadSupport.HandleCommand(aWorker : TWasmThread; aCommand: TWorkerCommand);
begin
  Case aCommand.Command of
    cmdSpawn : HandleSpawnCommand(aWorker, TWorkerSpawnThreadCommand(aCommand));
    cmdCleanup : HandleCleanupCommand(aWorker, TWorkerCleanupCommand(aCommand));
    cmdKill : HandleKillCommand(aWorker, TWorkerKillCommand(aCommand));
    cmdCancel : HandleCancelCommand(aWorker, TWorkerCancelCommand(aCommand));
    cmdLoaded : HandleLoadedCommand(aWorker, TWorkerLoadedCommand(aCommand));
    cmdConsole : HandleConsoleCommand(aWorker, TWorkerConsoleCommand(aCommand));
  else
    HandleCommand(aCommand);
  end;
end;

end.

