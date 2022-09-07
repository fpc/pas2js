unit Rtl.WebThreads;

{$mode ObjFPC}
{$modeswitch externalclass}

interface

uses
  JS, SysUtils, wasienv, webassembly;

Const
  // Each thread starts spawning at 1000*IndexOfWorker
  ThreadIDInterval = 1000;
  // When the thread ID reaches this limit, then it requests a new block
  ThreadIDMargin = 2;

  // lowercase !!
  cmdConsole = 'console';
  cmdException = 'exception';
  cmdCleanup = 'cleanup';
  cmdCancel = 'cancel';
  cmdLoaded = 'loaded';
  cmdKill = 'kill';
  cmdNeedIdBlock = 'needidblock';
  cmdThreadIdRange = 'threadidrange';
  cmdSpawn = 'spawn';
  cmdLoad = 'load';
  cmdRun = 'run';

  DefaultThreadWorker = 'pas2jsthreadworker.js';
  DefaultThreadCount = 2;
  DefaultMaxWorkerCount = 100;

  // Default exported thread entry point. Must have signature TThreadEntryPointFunction
  DefaultThreadEntryPoint = 'FPC_WASM_THREAD_ENTRY';
  // Default exported thread instance point. Must have signature TThreadInitInstanceFunction
  DefaultThreadInstanceInitPoint = 'FPC_WASM_THREAD_INIT';

  // Imports to wasi env.
  sThreadSpawn = 'thread_spawn';
  sThreadDetach = 'thread_detach';
  sThreadCancel = 'thread_cancel';
  sThreadSelf = 'thread_self';



Type
  // aRunProc and aArgs are pointers inside wasm.
  TThreadEntryPointFunction = Function(ThreadId: Integer; aRunProc : Integer; aArgs: Integer) : Integer;
  TThreadInitInstanceFunction = Function(IsWorkerThread : Longint; IsMainThread : Integer; CanBlock : Integer) : Integer;

  EWasmThreads = class(Exception);

  // Commands sent between thread workers and main program.

  { Basic TWorkerCommand. Command is the actual command }

  { we do not use Pascal classes for this, to avoid transferring unnecessary metadata present in the pascal class }

  TWorkerCommand = Class external name 'Object' (TJSObject)
    Command : String;
    ThreadID : Integer; // Meaning depends on actual command.
    TargetID : Integer; // Forward to thread ID
  end;
  TCommandNotifyEvent = Procedure (Sender : TObject; aCommand : TWorkerCommand) of object;

  { TWorkerCommandHelper }

  TWorkerCommandHelper = class helper for TWorkerCommand
    Class function NewWorker(const aCommand : string; aThreadID : Integer = -1) : TWorkerCommand; static;
  end;

  { TWorkerExceptionCommand }

  // When an unexpected error occurred.
  TWorkerExceptionCommand = class external name 'Object' (TWorkerCommand)
  public
    ExceptionClass: String;
    ExceptionMessage: String;
  end;

  { TWorkerExceptionCommandHelper }

  TWorkerExceptionCommandHelper = class helper for TWorkerExceptionCommand
    Class function CommandName : string; static;
    Class function CreateNew(const aExceptionClass,aExceptionMessage : string; aThreadID : Integer = -1) : TWorkerExceptionCommand; static;
  end;

  { TWorkerConsoleCommand }

  // Sent by worker to main: write message to console
  // Thread ID : sending console ID
  TWorkerConsoleCommand = class external name 'Object' (TWorkerCommand)
  public
    ConsoleMessage : String;
  end;

  { TWorkerConsoleCommandHelper }

  TWorkerConsoleCommandHelper = class helper for TWorkerConsoleCommand
    Class function CommandName : string; static;
    Class function Create(const aMessage : string; aThreadID : Integer = -1) : TWorkerConsoleCommand; static; reintroduce;
    Class function Create(const aMessage : array of JSValue; aThreadID : Integer = -1) : TWorkerConsoleCommand; static; reintroduce;
  end;

  // Cleanup thread info: put this worker into unusued workers
  TWorkerCleanupCommand = class external name 'Object' (TWorkerCommand)
  end;

  { TWorkerCleanupCommandHelper }

  TWorkerCleanupCommandHelper = class helper for TWorkerCleanupCommand
    Class function CommandName : string; static;
    Class function Create(aThreadID : Integer): TWorkerCleanupCommand; static;  reintroduce;
  end;


  { TWorkerKillCommand }
  // Kill thread (thread ID in ThreadID)
  TWorkerKillCommand = class external name 'Object' (TWorkerCommand)
  end;

  { TWorkerCleanupCommandHelper }

  TWorkerKillCommandHelper = class helper for TWorkerKillCommand
    Class function CommandName : string; static;
    Class function Create(aThreadID : Integer): TWorkerKillCommand; static;reintroduce;
  end;

  // Cancel thread (thread ID in ThreadID)
  TWorkerCancelCommand = class external name 'Object' (TWorkerCommand)
  end;

  { TWorkerCancelCommandHelper }

  TWorkerCancelCommandHelper = class helper for TWorkerCancelCommand
    Class function CommandName : string; static;
    Class function Create(aThreadID : Integer): TWorkerCancelCommand; static; reintroduce;
  end;

  // sent to notify main thread that the wasm module is loaded.
  TWorkerLoadedCommand = class external name 'Object' (TWorkerCommand)
  end;

  { TWorkerLoadedCommandHelper }

  TWorkerLoadedCommandHelper = class helper for TWorkerLoadedCommand
    Class function CommandName : string; static;
    Class function Create: TWorkerLoadedCommand; static; reintroduce;
  end;

  // Sent to notify main thread that a new range of IDs is needed.
  TWorkerNeedIdBlockCommand = class external name 'Object' (TWorkerCommand)
    Current : NativeInt;
  end;

  { TWorkerNeedIdBlockCommandHelper }

  TWorkerNeedIdBlockCommandHelper = class helper for TWorkerNeedIdBlockCommand
    Class function CommandName : string; static;
    Class function Create(aCurrent : NativeInt): TWorkerNeedIdBlockCommand; static; reintroduce;
  end;


  // Sent to notify main thread that a new thread must be started.
  // Worker cannot start new thread. It allocates the ID (threadId)
  // It sends RunFunction, Attributes and Arguments received by thread_spawn call.
  TWorkerSpawnThreadCommand = class external name 'Object' (TWorkerCommand)
    Attributes : Integer;
    Arguments : Integer;
    RunFunction : Integer;
    ThreadInfo : integer;
  end;

  { TWorkerSpawnThreadCommandHelper }

  TWorkerSpawnThreadCommandHelper = class helper for TWorkerSpawnThreadCommand
    Class function CommandName : string; static;
    Class function Create(aThreadID : integer; aAttrs,aArgs,aRun,aThreadInfo : Integer): TWorkerSpawnThreadCommand; static;reintroduce;
  end;



  // Sent by main to worker: load wasm module
  TWorkerLoadCommand = class external name 'Object' (TWorkerCommand)
  public
    Memory : TJSWebAssemblyMemory;
    Module : TJSWebAssemblyModule;
    ThreadRangeStart : NativeInt;
  end;

  { TWorkerLoadCommandHelper }

  TWorkerLoadCommandHelper = class helper for TWorkerLoadCommand
    Class function CommandName : string; static;
    Class function Create(aStartThreadIdRange : integer; aModule : TJSWebAssemblyModule; aMemory : TJSWebAssemblyMemory): TWorkerLoadCommand; static;reintroduce;
  end;


  // Sent by main to worker: run thread procedure
  TWorkerRunCommand = class external name 'Object' (TWorkerCommand)
  public
    ThreadInfo : Integer;
    RunThreadProc : Integer;
    Attrs : Integer;
    Args : Integer;
  end;

  { TWorkerRunCommandHelper }

  TWorkerRunCommandHelper = class helper for TWorkerRunCommand
    Class function CommandName : string; static;
    Class function Create(aThreadID, aRunProc, aAttrs, aArgs, aThreadInfoLocation : integer): TWorkerRunCommand; static; reintroduce;
  end;


  // Sent to worker with new range of thread IDs.
  TWorkerThreadIDRangeCommand = class external name 'Object' (TWorkerCommand)
    RangeStart : NativeInt;
  end;

  { TWorkerThreadIDRangeCommandHelper }

  TWorkerThreadIDRangeCommandHelper = class helper for TWorkerThreadIDRangeCommand
    Class function CommandName : string; static;
    class function Create(aRangeStart: NativeInt): TWorkerThreadIDRangeCommand;  static; reintroduce;
  end;



  TThreadinfo = record
    OriginThreadID : Integer; // Numerical thread ID
    ThreadID : Integer; // Numerical thread ID
    ThreadInfoLocation : Integer; // Location of thread block (pointer)
    RunFunction : Integer; // Location of thread function (pointer)
    Attributes : Integer;  // Unused for the moment
    Arguments : Integer;  // Arguments (pointer)
  end;

  // This basis object has the thread support that is needed by the WASM module.
  // It relies on descendents to implement the actual calls.

  { TWasmThreadSupport }

  TWasmThreadSupport = Class (TImportExtension)
  private
    FOnSendCommand: TCommandNotifyEvent;
  Protected
    // Proposed WASI standard, modeled after POSIX pthreads.
    Function thread_spawn(thread_id : Integer; attrs: Integer; thread_start_func : Integer; args : Integer) : Integer;  virtual; abstract;
    Function thread_detach(thread_id : Integer) : Integer; virtual; abstract;
    Function thread_cancel(thread_id : Integer) : Integer; virtual; abstract;
    Function thread_self() : Integer; virtual; abstract;
  Public
    Function ImportName : String; override;
    procedure FillImportObject(aObject: TJSObject); override;
    Procedure HandleCommand(aCommand : TWorkerCommand); virtual;
    Procedure SendCommand(aCommand : TWorkerCommand); virtual;
    // Set this to actually send commands. Normally set by TWorkerWASIHostApplication
    Property OnSendCommand : TCommandNotifyEvent Read FOnSendCommand Write FOnSendCommand;
  end;


implementation

{ TWorkerRunCommandHelper }

class function TWorkerRunCommandHelper.CommandName: string;
begin
  Result:=cmdRun;
end;

class function TWorkerRunCommandHelper.Create(aThreadID, aRunProc, aAttrs,
  aArgs, aThreadInfoLocation: integer): TWorkerRunCommand;
begin
  Result:=TWorkerRunCommand(TWorkerCommand.NewWorker(CommandName));
  Result.ThreadID:=aThreadID;
  Result.ThreadInfo:=aThreadInfoLocation;
  Result.RunThreadProc:=aRunProc;
  Result.Attrs:=aAttrs;
  Result.Args:=aArgs;
end;

{ TWorkerLoadCommandHelper }

class function TWorkerLoadCommandHelper.CommandName: string;
begin
  Result:=cmdLoad;
end;

class function TWorkerLoadCommandHelper.Create(aStartThreadIdRange: integer;
  aModule: TJSWebAssemblyModule; aMemory: TJSWebAssemblyMemory
  ): TWorkerLoadCommand;
begin
  Result:=TWorkerLoadCommand(TWorkerCommand.NewWorker(CommandName));
  Result.ThreadRangeStart:=aStartThreadIdRange;
  Result.Memory:=aMemory;
  Result.Module:=aModule;
end;

{ TWorkerSpawnThreadCommandHelper }

class function TWorkerSpawnThreadCommandHelper.CommandName: string;
begin
  Result:=cmdSpawn
end;

class function TWorkerSpawnThreadCommandHelper.Create(aThreadID: integer;
  aAttrs, aArgs, aRun, aThreadInfo: Integer): TWorkerSpawnThreadCommand;
begin
  Result:=TWorkerSpawnThreadCommand(TWorkerCommand.NewWorker(CommandName,aThreadID));
  Result.Arguments:=aArgs;
  Result.Attributes:=aAttrs;
  Result.RunFunction:=aRun;
  Result.ThreadInfo:=aThreadInfo;
end;

{ TWorkerThreadIDRangeCommandHelper }

class function TWorkerThreadIDRangeCommandHelper.CommandName: string;
begin
  Result:=cmdThreadIdRange;
end;

class function TWorkerThreadIDRangeCommandHelper.Create(aRangeStart: NativeInt
  ): TWorkerThreadIDRangeCommand;
begin
  Result:=TWorkerThreadIDRangeCommand(TWorkerCommand.NewWorker(CommandName));
  Result.RangeStart:=aRangeStart;
end;

{ TWorkerNeedIdBlockCommandHelper }

class function TWorkerNeedIdBlockCommandHelper.CommandName: string;
begin
  Result:=cmdNeedIdBlock;
end;

class function TWorkerNeedIdBlockCommandHelper.Create(aCurrent: NativeInt
  ): TWorkerNeedIdBlockCommand;
begin
  Result:=TWorkerNeedIdBlockCommand(TWorkerCommand.NewWorker(CommandName));
  Result.Current:=aCurrent;
end;


{ TWorkerLoadedCommandHelper }

class function TWorkerLoadedCommandHelper.CommandName: string;
begin
  Result:=cmdLoaded;
end;

class function TWorkerLoadedCommandHelper.Create: TWorkerLoadedCommand;
begin
  Result:=TWorkerLoadedCommand(TWorkerCommand.NewWorker(CommandName));
end;

{ TWorkerCancelCommandHelper }

class function TWorkerCancelCommandHelper.CommandName: string;
begin
  result:=cmdCancel;
end;

class function TWorkerCancelCommandHelper.Create(aThreadID: Integer
  ): TWorkerCancelCommand;
begin
  Result:=TWorkerCancelCommand(TWorkerCommand.NewWorker(CommandName,aThreadID));
end;

{ TWorkerKillCommandHelper }

class function TWorkerKillCommandHelper.CommandName: string;
begin
  Result:=cmdKill
end;

class function TWorkerKillCommandHelper.Create(aThreadID : Integer): TWorkerKillCommand;
begin
  Result:=TWorkerKillCommand(TWorkerCommand.NewWorker(CommandName,aThreadID));
end;

{ TWorkerCleanupCommandHelper }

class function TWorkerCleanupCommandHelper.CommandName: string;
begin
  Result:=cmdCleanup
end;

class function TWorkerCleanupCommandHelper.Create(aThreadID: Integer): TWorkerCleanupCommand;
begin
  Result:=TWorkerCleanupCommand(TWorkerCommand.NewWorker(CommandName,aThreadID));
end;

{ TWorkerConsoleCommandHelper }

class function TWorkerConsoleCommandHelper.CommandName: string;
begin
  Result:=cmdConsole;
end;

class function TWorkerConsoleCommandHelper.Create(
  const aMessage: string; aThreadID : Integer = -1): TWorkerConsoleCommand;
begin
  Result:=TWorkerConsoleCommand(TWorkerCommand.NewWorker(CommandName,aThreadID));
  Result.ConsoleMessage:=aMessage;
end;

class function TWorkerConsoleCommandHelper.Create(
  const aMessage: array of JSValue; aThreadID : Integer = -1): TWorkerConsoleCommand;
begin
  Result:=Create(TJSArray(aMessage).join(' '),aThreadID);
end;

{ TWorkerExceptionCommandHelper }

class function TWorkerExceptionCommandHelper.CommandName: string;
begin
  Result:=cmdException;
end;

class function TWorkerExceptionCommandHelper.CreateNew(const aExceptionClass,aExceptionMessage: string; aThreadID : Integer = -1  ): TWorkerExceptionCommand;
begin
  Result:=TWorkerExceptionCommand(TWorkerCommand.NewWorker(CommandName,aThreadID));
  Result.ExceptionClass:=aExceptionClass;
  Result.ExceptionMessage:=aExceptionMessage;
end;

{ TWorkerCommandHelper }

class function TWorkerCommandHelper.NewWorker(const aCommand : string; aThreadID : Integer = -1): TWorkerCommand;
begin
  Result:=TWorkerCommand.New;
  Result.Command:=LowerCase(aCommand);
  if aThreadID<>-1 then
    Result.ThreadID:=aThreadID;
end;


{ TWasmThreadSupport }

function TWasmThreadSupport.ImportName: String;
begin
  Result:='FPCThreading';
end;

procedure TWasmThreadSupport.FillImportObject(aObject: TJSObject);
begin
  aObject[sThreadSpawn]:=@Thread_Spawn;
  aObject[sThreadDetach]:=@Thread_Detach;
  aObject[sThreadCancel]:=@Thread_Cancel;
  aObject[sThreadSelf]:=@Thread_Self;
end;


procedure TWasmThreadSupport.HandleCommand(aCommand: TWorkerCommand);

Var
  P : TWorkerExceptionCommand;

begin
  P:=TWorkerExceptionCommand.New;
  P.ExceptionClass:='ENotSupportedException';
  P.ExceptionMessage:='Unsupported command : '+TJSJSON.Stringify(aCommand);
  SendCommand(aCommand);
end;

procedure TWasmThreadSupport.SendCommand(aCommand: TWorkerCommand);
begin
  if Assigned(FOnSendCommand) then
    FOnSendCommand(Self,aCommand);
end;


end.

