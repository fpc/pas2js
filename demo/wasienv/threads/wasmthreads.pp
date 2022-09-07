{
    This file is part of the Free Pascal run time library.
    Copyright (c) 2022 by Michael Van Canneyt,
    member of the Free Pascal development team.

    wasm threading support implementation

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
{$mode objfpc}
{$modeswitch advancedrecords}

{$DEFINE DEBUG_MT}

unit wasmthreads;

interface

Procedure SetWasmThreadManager;

implementation

Uses
  WebAssembly, wasiapi;

{*****************************************************************************
                             System unit import
*****************************************************************************}

procedure fpc_threaderror; [external name 'FPC_THREADERROR'];

Type
  TTimeLockResult = (tlrOK,tlrTimeout,tlrError);

  TFPWasmMutex = record
    _lock : Longint;
    _owner : Pointer;
    function TryLock : Boolean;
    function Lock : Boolean;
    function TimedLock(aTimeOut : Longint) : TTimeLockResult;
    function Unlock : Boolean;
  end;

  TFPWasmEvent = record
    _mutex : TFPWasmMutex;
    _isset : Boolean;
  end;

  PFPWasmThread = ^TFPWasmThread;
  TFPWasmThread = record
    ThreadID : Integer;
    Next : PFPWasmThread;
    Previous : PFPWasmThread;
  end;

Var
  MainThread : TFPWasmThread;
  threadvarblocksize : dword = 0;
  TLSInitialized : Integer = 0;

{$IFDEF DEBUG_MT}
Type
  TSmallString = string[100];


Procedure SetTLSMemory(aValue : Pointer);

begin
  fpc_wasm32_init_tls(aValue);
end;

Function GetTLSMemory : Pointer;

begin
  Result:=fpc_wasm32_tls_base;
end;


Procedure RawWrite(var S : TSmallString);

begin
  // ToDo
end;
{$ENDIF DEBUG_MT}

procedure WasmInitThreadvar(var offset : dword;size : dword);

begin
  threadvarblocksize:=align(threadvarblocksize, fpc_wasm32_tls_align);
  offset:=threadvarblocksize;
  inc(threadvarblocksize,size);
end;



procedure WasmAllocateThreadVars;

var
  tlsMemBlock : pointer;
  tlsBlockSize : Integer;

begin
  tlsBlockSize:=fpc_wasm32_tls_size;
  if threadvarblocksize<>tlsBlocksize then
    Writeln('Warning : block sizes differ: ',tlsBlocksize,'<>',threadvarblocksize,'(calculated) !');
//  DataIndex:=Pointer(Fpmmap(nil,threadvarblocksize,3,MAP_PRIVATE+MAP_ANONYMOUS,-1,0));
  FillChar(DataIndex^,threadvarblocksize,0);
//  pthread_setspecific(tlskey,dataindex);
end;

procedure WasmThreadCleanup(p: pointer); cdecl;

{$ifdef DEBUG_MT}
var
  s: TSmallString; // not an ansistring
{$endif DEBUG_MT}

begin
{$ifdef DEBUG_MT}
  s := 'finishing externally started thread'#10;
  RawWrite(s);
{$endif DEBUG_MT}
  { Restore tlskey value as it may already have been set to null,
    in which case
      a) DoneThread can't release the memory
      b) accesses to threadvars from DoneThread or anything it
         calls would allocate new threadvar memory
  }
  { clean up }
  DoneThread;
pthread_setspecific(CleanupKey,nil);
end;




procedure HookThread;
{ Set up externally created thread }
begin
  WasmAllocateThreadVars;
  InitThread(1000000000);
  pthread_setspecific(CleanupKey,getTlsMemory);
end;



function WasmRelocateThreadvar(offset : dword) : pointer;

var
  P : Pointer;

begin
  P:=GetTLSMemory;
  if (P=Nil) then
    begin
    HookThread;
    P:=GetTLSMemory;
    end;
  WasmRelocateThreadvar:=P+Offset;
end;


procedure WasmReleaseThreadVars;
begin
  Fpmunmap(pointer(pthread_getspecific(tlskey)),threadvarblocksize);
end;


function WasmThreadMain(param : pointer) : pointer;

var
{$ifdef DEBUG_MT}
  s: TSmallString; // not an ansistring
{$endif DEBUG_MT}

begin
{$ifdef DEBUG_MT}
  s := 'New thread started, initing threadvars'#10;
  RawWrite(s);
{$endif DEBUG_MT}
  { Must be first, many system unit things depend on threadvars}
  WasmAllocateThreadVars;
  { Copy parameter to local data }
{$ifdef DEBUG_MT}
  s := 'New thread started, initialising ...'#10;
  RawWrite(s);
{$endif DEBUG_MT}
  ti:=pthreadinfo(param)^;

  { Initialize thread }
  InitThread(ti.stklen);

  dispose(pthreadinfo(param));
  { Start thread function }
{$ifdef DEBUG_MT}
  writeln('Jumping to thread function');
{$endif DEBUG_MT}
  WasmThreadMain:=pointer(ti.f(ti.p));
  DoneThread;
  pthread_exit(WasmThreadMain);
end;



Procedure InitWasmTLS;

begin
  if (InterLockedExchange(longint(TLSInitialized),1) = 0) then
    begin
      { We're still running in single thread mode, setup the TLS }
      pthread_key_create(@TLSKey,nil);
      InitThreadVars(@WasmRelocateThreadvar);
      { used to clean up threads that we did not create ourselves:
         a) the default value for a key (and hence also this one) in
            new threads is NULL, and if it's still like that when the
            thread terminates, nothing will happen
         b) if it's non-NULL, the destructor routine will be called
            when the thread terminates
       -> we will set it to 1 if the threadvar relocation routine is
          called from a thread we did not create, so that we can
          clean up everything at the end }
      pthread_key_create(@CleanupKey,@WasmthreadCleanup);
    end
end;

function WasmBeginThread(sa : Pointer;stacksize : PtrUInt;
                   ThreadFunction : tthreadfunc;p : pointer;
                   creationFlags : dword; var ThreadId : TThreadId) : TThreadID;
var
  ti : pthreadinfo;
  thread_attr : pthread_attr_t;
{$ifdef DEBUG_MT}
  S : TSmallString;
{$endif DEBUG_MT}
begin
{$ifdef DEBUG_MT}
  S:='Creating new thread';
  RawWrite(S);
{$endif DEBUG_MT}
  { Initialize multithreading if not done }
  if not TLSInitialized then
    InitWasmTLS;
  if not IsMultiThread then
    begin
      { We're still running in single thread mode, lazy initialize thread support }
       LazyInitThreading;
       IsMultiThread:=true;
    end;

  { the only way to pass data to the newly created thread
    in a MT safe way, is to use the heap }
  new(ti);
  ti^.f:=ThreadFunction;
  ti^.p:=p;
  ti^.stklen:=stacksize;
  { call pthread_create }
{$ifdef DEBUG_MT}
  S:='Starting new thread';
  RawWrite(S);
{$endif DEBUG_MT}
  pthread_attr_init(@thread_attr);
  {$if not defined(HAIKU)and not defined(BEOS) and not defined(ANDROID)}
  {$if defined (solaris) or defined (netbsd) }
  pthread_attr_setinheritsched(@thread_attr, PTHREAD_INHERIT_SCHED);
  {$else not solaris}
  pthread_attr_setinheritsched(@thread_attr, PTHREAD_EXPLICIT_SCHED);
  {$endif not solaris}
  {$ifend}

  // will fail under linux -- apparently unimplemented
  pthread_attr_setscope(@thread_attr, PTHREAD_SCOPE_PROCESS);

  // don't create detached, we need to be able to join (waitfor) on
  // the newly created thread!
  //pthread_attr_setdetachstate(@thread_attr, PTHREAD_CREATE_DETACHED);

  // set the stack size
  if (pthread_attr_setstacksize(@thread_attr, stacksize)<>0) or
     // and create the thread
     (pthread_create(ppthread_t(@threadid), @thread_attr, @ThreadMain,ti) <> 0) then

    begin
      dispose(ti);
      threadid := TThreadID(0);
    end;
  CBeginThread:=threadid;
  pthread_attr_destroy(@thread_attr);
{$ifdef DEBUG_MT}
  Str(ptrint(CBeginThread),S);
  S:= 'BeginThread returning '+S;
  RawWrite(S);
{$endif DEBUG_MT}
end;


procedure WasmEndThread(ExitCode : DWord);

begin
  DoneThread;
  pthread_detach(pthread_t(pthread_self()));
  pthread_exit(pointer(ptrint(ExitCode)));
end;



function  WasmSuspendThread (threadHandle : TThreadID) : dword;
// Not supported
begin
  result:=dword(-1);
end;


function  WasmResumeThread  (threadHandle : TThreadID) : dword;
// Not supported
begin
  result:=dword(-1);
end;



procedure WasmThreadSwitch;  {give time to other threads}

begin
  // Not supported
end;


function  WasmKillThread (threadHandle : TThreadID) : dword;
begin
  pthread_detach(pthread_t(threadHandle));
  WasmKillThread := pthread_cancel(pthread_t(threadHandle));
end;

function WasmCloseThread (threadHandle : TThreadID) : dword;

begin
  result:=0;
end;

function  WasmWaitForThreadTerminate (threadHandle : TThreadID; TimeoutMs : longint) : dword;  {0=no timeout}

var
  LResultP: Pointer;

begin
  pthread_join(pthread_t(threadHandle), @LResultP);
  WasmWaitForThreadTerminate := dword(LResultP);
end;

function  WasmThreadSetPriority (threadHandle : TThreadID; Prio: longint): boolean; {-15..+15, 0=normal}
begin
  result:=false;
end;

function  WasmThreadGetPriority (threadHandle : TThreadID): Integer;
begin
  result:=0;
end;


  function  CGetCurrentThreadId : TThreadID;
    begin
      CGetCurrentThreadId := TThreadID (pthread_self());
    end;


  procedure CSetThreadDebugNameA(threadHandle: TThreadID; const ThreadName: AnsiString);
{$if defined(Linux) or defined(Android)}
    var
      CuttedName: AnsiString;
{$endif}
    begin
{$if defined(Linux) or defined(Android)}
      if ThreadName = '' then
        Exit;
  {$ifdef dynpthreads}
      if Assigned(pthread_setname_np) then
  {$endif dynpthreads}
      begin
        // length restricted to 16 characters including terminating null byte
        CuttedName:=Copy(ThreadName, 1, 15);
        if threadHandle=TThreadID(-1) then
        begin
          pthread_setname_np(pthread_self(), @CuttedName[1]);
        end
        else
        begin
          pthread_setname_np(pthread_t(threadHandle), @CuttedName[1]);
        end;
      end;
{$elseif defined(Darwin) or defined(iphonesim)}
  {$ifdef dynpthreads}
      if Assigned(pthread_setname_np) then
  {$endif dynpthreads}
      begin
        // only allowed to set from within the thread
        if threadHandle=TThreadID(-1) then
          pthread_setname_np(@ThreadName[1]);
      end;
{$else}
       {$Warning SetThreadDebugName needs to be implemented}
{$endif}
    end;


  procedure CSetThreadDebugNameU(threadHandle: TThreadID; const ThreadName: UnicodeString);
    begin
{$if defined(Linux) or defined(Android)}
  {$ifdef dynpthreads}
      if Assigned(pthread_setname_np) then
  {$endif dynpthreads}
      begin
        CSetThreadDebugNameA(threadHandle, AnsiString(ThreadName));
      end;
{$elseif defined(Darwin) or defined(iphonesim)}
  {$ifdef dynpthreads}
      if Assigned(pthread_setname_np) then
  {$endif dynpthreads}
      begin
        CSetThreadDebugNameA(threadHandle, AnsiString(ThreadName));
      end;
{$else}
       {$Warning SetThreadDebugName needs to be implemented}
{$endif}
    end;


{*****************************************************************************
                          Delphi/Win32 compatibility
*****************************************************************************}

    procedure CInitCriticalSection(var CS);

    var
      MAttr : pthread_mutexattr_t;
      res: longint;
    begin
      res:=pthread_mutexattr_init(@MAttr);
      if res=0 then
        begin
          res:=pthread_mutexattr_settype(@MAttr,longint(_PTHREAD_MUTEX_RECURSIVE));
          if res=0 then
            res := pthread_mutex_init(@CS,@MAttr)
          else
            { No recursive mutex support :/ }
            fpc_threaderror
        end
      else
        res:= pthread_mutex_init(@CS,NIL);
      pthread_mutexattr_destroy(@MAttr);
      if res <> 0 then
        fpc_threaderror;
    end;

    procedure CEnterCriticalSection(var CS);
      begin
         if pthread_mutex_lock(@CS) <> 0 then
           fpc_threaderror
      end;

    function CTryEnterCriticalSection(var CS):longint;
      begin
         if pthread_mutex_Trylock(@CS)=0 then
           result:=1  // succes
         else
           result:=0; // failure
      end;

    procedure CLeaveCriticalSection(var CS);
      begin
         if pthread_mutex_unlock(@CS) <> 0 then
           fpc_threaderror
      end;

    procedure CDoneCriticalSection(var CS);
      begin
         { unlock as long as unlocking works to unlock it if it is recursive
           some Delphi code might call this function with a locked mutex      }
         while pthread_mutex_unlock(@CS)=0 do
           ;

         if pthread_mutex_destroy(@CS) <> 0 then
           fpc_threaderror;
      end;


{*****************************************************************************
                           Semaphore routines
*****************************************************************************}


type
     TPthreadCondition = pthread_cond_t;
     TPthreadMutex = pthread_mutex_t;
     Tbasiceventstate=record
         FCondVar: TPthreadCondition;
{$if defined(Linux) and not defined(Android)}         
         FAttr: pthread_condattr_t;
         FClockID: longint;
{$ifend}        
         FEventSection: TPthreadMutex;
         FWaiters: longint;
         FIsSet,
         FManualReset,
         FDestroying : Boolean;
        end;
     plocaleventstate = ^tbasiceventstate;
//     peventstate=pointer;

Const
        wrSignaled = 0;
        wrTimeout  = 1;
        wrAbandoned= 2;
        wrError    = 3;

function IntBasicEventCreate(EventAttributes : Pointer; AManualReset,InitialState : Boolean;const Name : ansistring):pEventState;
var
  MAttr : pthread_mutexattr_t;
  res   : cint;  
{$if defined(Linux) and not defined(Android)}  
  timespec: ttimespec;
{$ifend}  
begin
  new(plocaleventstate(result));
  plocaleventstate(result)^.FManualReset:=AManualReset;
  plocaleventstate(result)^.FWaiters:=0;
  plocaleventstate(result)^.FDestroying:=False;
  plocaleventstate(result)^.FIsSet:=InitialState;
{$if defined(Linux) and not defined(Android)}  
  res := pthread_condattr_init(@plocaleventstate(result)^.FAttr);
  if (res <> 0) then
  begin
    FreeMem(result);
    fpc_threaderror;  
  end;
  
  if clock_gettime(CLOCK_MONOTONIC_RAW, @timespec) = 0 then
  begin
    res := pthread_condattr_setclock(@plocaleventstate(result)^.FAttr, CLOCK_MONOTONIC_RAW);
  end
  else
  begin
    res := -1; // No support for CLOCK_MONOTONIC_RAW   
  end;
  
  if (res = 0) then
  begin
    plocaleventstate(result)^.FClockID := CLOCK_MONOTONIC_RAW;
  end
  else
  begin
    res := pthread_condattr_setclock(@plocaleventstate(result)^.FAttr, CLOCK_MONOTONIC);
    if (res = 0) then
    begin
      plocaleventstate(result)^.FClockID := CLOCK_MONOTONIC;
    end
    else
    begin
      pthread_condattr_destroy(@plocaleventstate(result)^.FAttr);
      FreeMem(result);
      fpc_threaderror;  
    end;    
  end;  

  res := pthread_cond_init(@plocaleventstate(result)^.FCondVar, @plocaleventstate(result)^.FAttr);
  if (res <> 0) then
  begin
    pthread_condattr_destroy(@plocaleventstate(result)^.FAttr);  
    FreeMem(result);
    fpc_threaderror;
  end;
{$else}
  res := pthread_cond_init(@plocaleventstate(result)^.FCondVar, nil);
  if (res <> 0) then
  begin
    FreeMem(result);
    fpc_threaderror;
  end; 
{$ifend} 

  res:=pthread_mutexattr_init(@MAttr);
  if res=0 then
    begin
      res:=pthread_mutexattr_settype(@MAttr,longint(_PTHREAD_MUTEX_RECURSIVE));
      if Res=0 then
        Res:=pthread_mutex_init(@plocaleventstate(result)^.feventsection,@MAttr)
      else
        res:=pthread_mutex_init(@plocaleventstate(result)^.feventsection,nil);
    end
  else
    res:=pthread_mutex_init(@plocaleventstate(result)^.feventsection,nil);

  pthread_mutexattr_destroy(@MAttr);
  if res <> 0 then
    begin
      pthread_cond_destroy(@plocaleventstate(result)^.FCondVar);
{$if defined(Linux) and not defined(Android)}  
      pthread_condattr_destroy(@plocaleventstate(result)^.FAttr);	
{$ifend}      
      FreeMem(result);
      fpc_threaderror;
    end;
end;

procedure Intbasiceventdestroy(state:peventstate);
begin
  { safely mark that we are destroying this event }
  pthread_mutex_lock(@plocaleventstate(state)^.feventsection);
  plocaleventstate(state)^.FDestroying:=true;

  { send a signal to all threads that are waiting }
  pthread_cond_broadcast(@plocaleventstate(state)^.FCondVar);
  pthread_mutex_unlock(@plocaleventstate(state)^.feventsection);

  { now wait until they've finished their business }
  while (plocaleventstate(state)^.FWaiters <> 0) do
    cThreadSwitch;

  { and clean up }
  pthread_cond_destroy(@plocaleventstate(state)^.Fcondvar);
{$if defined(Linux) and not defined(Android)}  
  pthread_condattr_destroy(@plocaleventstate(state)^.FAttr);	
{$ifend}  
  pthread_mutex_destroy(@plocaleventstate(state)^.FEventSection);
  dispose(plocaleventstate(state));
end;


procedure IntbasiceventResetEvent(state:peventstate);
begin
  pthread_mutex_lock(@plocaleventstate(state)^.feventsection);
  plocaleventstate(state)^.fisset:=false;
  pthread_mutex_unlock(@plocaleventstate(state)^.feventsection);
end;

procedure IntbasiceventSetEvent(state:peventstate);
begin
  pthread_mutex_lock(@plocaleventstate(state)^.feventsection);
  plocaleventstate(state)^.Fisset:=true;
  if not(plocaleventstate(state)^.FManualReset) then
    pthread_cond_signal(@plocaleventstate(state)^.Fcondvar)
  else
    pthread_cond_broadcast(@plocaleventstate(state)^.Fcondvar);
  pthread_mutex_unlock(@plocaleventstate(state)^.feventsection);
end;

function IntbasiceventWaitFor(Timeout : Cardinal;state:peventstate) : longint;
var
  timespec: ttimespec;
  errres: cint;
  isset: boolean;
  tnow : timeval;
begin

  { safely check whether we are being destroyed, if so immediately return. }
  { otherwise (under the same mutex) increase the number of waiters        }
  pthread_mutex_lock(@plocaleventstate(state)^.feventsection);
  if (plocaleventstate(state)^.FDestroying) then
    begin
      pthread_mutex_unlock(@plocaleventstate(state)^.feventsection);
      result := wrAbandoned;
      exit;
    end;
  { not a regular inc() because it may happen simulatneously with the }
  { interlockeddecrement() at the end                                 }
  interlockedincrement(plocaleventstate(state)^.FWaiters);

  //Wait without timeout using pthread_cond_wait
  if Timeout = $FFFFFFFF then
    begin
      while (not plocaleventstate(state)^.FIsSet) and (not plocaleventstate(state)^.FDestroying) do
        pthread_cond_wait(@plocaleventstate(state)^.Fcondvar, @plocaleventstate(state)^.feventsection);
    end
  else
    begin
      //Wait with timeout using pthread_cond_timedwait
{$if defined(Linux) and not defined(Android)}
      if clock_gettime(plocaleventstate(state)^.FClockID, @timespec) <> 0 then
      begin
        Result := Ord(wrError);
        Exit;
      end;
      timespec.tv_sec  := timespec.tv_sec + (clong(timeout) div 1000);
      timespec.tv_nsec := ((clong(timeout) mod 1000) * 1000000) + (timespec.tv_nsec);
{$else}
      // TODO: FIX-ME: Also use monotonic clock for other *nix targets
      fpgettimeofday(@tnow, nil);
      timespec.tv_sec  := tnow.tv_sec + (clong(timeout) div 1000);
      timespec.tv_nsec := ((clong(timeout) mod 1000) * 1000000) + (tnow.tv_usec * 1000);
{$ifend}
      if timespec.tv_nsec >= 1000000000 then
        begin
          inc(timespec.tv_sec);
          dec(timespec.tv_nsec, 1000000000);
        end;
      errres := 0;
      while (not plocaleventstate(state)^.FDestroying) and
            (not plocaleventstate(state)^.FIsSet) and 
            (errres<>ESysETIMEDOUT) do
        errres := pthread_cond_timedwait(@plocaleventstate(state)^.Fcondvar,
                                         @plocaleventstate(state)^.feventsection, 
                                         @timespec);
    end;

  isset := plocaleventstate(state)^.FIsSet;

  { if ManualReset=false, reset the event immediately. }
  if (plocaleventstate(state)^.FManualReset=false) then
    plocaleventstate(state)^.FIsSet := false;

  //check the results...
  if plocaleventstate(state)^.FDestroying then
    Result := wrAbandoned
  else
    if IsSet then
      Result := wrSignaled
    else
      begin
        if errres=ESysETIMEDOUT then
          Result := wrTimeout
        else
          Result := wrError;
      end;

  pthread_mutex_unlock(@plocaleventstate(state)^.feventsection);

  { don't put this above the previous pthread_mutex_unlock, because    }
  { otherwise we can get errors in case an object is destroyed between }
  { end of the wait/sleep loop and the signalling above.               }
  { The pthread_mutex_unlock above takes care of the memory barrier    }
  interlockeddecrement(plocaleventstate(state)^.FWaiters);
end;

function intRTLEventCreate: PRTLEvent;

var p:pintrtlevent;

begin
  new(p);
  if not assigned(p) then
    fpc_threaderror;
  if pthread_cond_init(@p^.condvar, nil)<>0 then
    begin
      dispose(p);
      fpc_threaderror;
    end;
  if pthread_mutex_init(@p^.mutex, nil)<>0 then
    begin
      pthread_cond_destroy(@p^.condvar);
      dispose(p);
      fpc_threaderror;
    end;
  p^.isset:=false;
  result:=PRTLEVENT(p);
end;

procedure intRTLEventDestroy(AEvent: PRTLEvent);

var p:pintrtlevent;

begin
  p:=pintrtlevent(aevent);
  pthread_cond_destroy(@p^.condvar);
  pthread_mutex_destroy(@p^.mutex);
  dispose(p);
end;

procedure intRTLEventSetEvent(AEvent: PRTLEvent);
var p:pintrtlevent;

begin
  p:=pintrtlevent(aevent);
  pthread_mutex_lock(@p^.mutex);
  p^.isset:=true;
  pthread_cond_signal(@p^.condvar);
  pthread_mutex_unlock(@p^.mutex);
end;


procedure intRTLEventResetEvent(AEvent: PRTLEvent);
var p:pintrtlevent;

begin
  p:=pintrtlevent(aevent);
  pthread_mutex_lock(@p^.mutex);
  p^.isset:=false;
  pthread_mutex_unlock(@p^.mutex);
end;


procedure intRTLEventWaitFor(AEvent: PRTLEvent);
var p:pintrtlevent;

begin
  p:=pintrtlevent(aevent);
  pthread_mutex_lock(@p^.mutex);
  while not p^.isset do pthread_cond_wait(@p^.condvar, @p^.mutex);
  p^.isset:=false;
  pthread_mutex_unlock(@p^.mutex);
end;

procedure intRTLEventWaitForTimeout(AEvent: PRTLEvent;timeout : longint);
  var
    p : pintrtlevent;
    errres : cint;
    timespec : ttimespec;
    tnow : timeval;

  begin
    p:=pintrtlevent(aevent);
    fpgettimeofday(@tnow,nil);
    timespec.tv_sec:=tnow.tv_sec+(timeout div 1000);
    timespec.tv_nsec:=(timeout mod 1000)*1000000 + tnow.tv_usec*1000;
    if timespec.tv_nsec >= 1000000000 then
    begin
      inc(timespec.tv_sec);
      dec(timespec.tv_nsec, 1000000000);
    end;
    errres:=0;
    pthread_mutex_lock(@p^.mutex);
    while (not p^.isset) and
          (errres <> ESysETIMEDOUT) do
      begin
        errres:=pthread_cond_timedwait(@p^.condvar, @p^.mutex, @timespec);
      end;
    p^.isset:=false;
    pthread_mutex_unlock(@p^.mutex);
  end;


type
  threadmethod = procedure of object;


Function CInitThreads : Boolean;

begin
{$ifdef DEBUG_MT}
  Writeln('Entering InitThreads.');
{$endif}
{$ifndef dynpthreads}
  Result:=True;
{$else}
  Result:=LoadPthreads;
{$endif}
  ThreadID := TThreadID (pthread_self());
{$ifdef DEBUG_MT}
  Writeln('InitThreads : ',Result);
{$endif DEBUG_MT}
  // We assume that if you set the thread manager, the application is multithreading.
  InitCTLS;
end;

Function CDoneThreads : Boolean;

begin
{$ifndef dynpthreads}
  Result:=True;
{$else}
  Result:=UnloadPthreads;
{$endif}
end;


Var
  CThreadManager : TThreadManager;

Procedure SetCThreadManager;

begin
  With CThreadManager do begin
    InitManager            :=@WasmInitThreads;
    DoneManager            :=@WasmDoneThreads;
    BeginThread            :=@WasmBeginThread;
    EndThread              :=@WasmEndThread;
    SuspendThread          :=@WasmSuspendThread;
    ResumeThread           :=@WasmResumeThread;
    KillThread             :=@WasmKillThread;
    ThreadSwitch           :=@WasmThreadSwitch;
    CloseThread	           :=@WasmCloseThread;
    WaitForThreadTerminate :=@WasmWaitForThreadTerminate;
    ThreadSetPriority      :=@WasmThreadSetPriority;
    ThreadGetPriority      :=@WasmThreadGetPriority;
    GetCurrentThreadId     :=@WasmGetCurrentThreadId;
    SetThreadDebugNameA    :=@WasmSetThreadDebugNameA;
    SetThreadDebugNameU    :=@WasmSetThreadDebugNameU;
    InitCriticalSection    :=@WasmInitCriticalSection;
    DoneCriticalSection    :=@WasmDoneCriticalSection;
    EnterCriticalSection   :=@WasmEnterCriticalSection;
    TryEnterCriticalSection:=@WasmTryEnterCriticalSection;
    LeaveCriticalSection   :=@WasmLeaveCriticalSection;
    InitThreadVar          :=@WasmInitThreadVar;
    RelocateThreadVar      :=@WasmRelocateThreadVar;
    AllocateThreadVars     :=@WasmAllocateThreadVars;
    ReleaseThreadVars      :=@WasmReleaseThreadVars;
    BasicEventCreate       :=@intBasicEventCreate;
    BasicEventDestroy      :=@intBasicEventDestroy;
    BasicEventResetEvent   :=@intBasicEventResetEvent;
    BasicEventSetEvent     :=@intBasicEventSetEvent;
    BasiceventWaitFor      :=@intBasiceventWaitFor;
    rtlEventCreate         :=@intrtlEventCreate;
    rtlEventDestroy        :=@intrtlEventDestroy;
    rtlEventSetEvent       :=@intrtlEventSetEvent;
    rtlEventResetEvent     :=@intrtlEventResetEvent;
    rtleventWaitForTimeout :=@intrtleventWaitForTimeout;
    rtleventWaitFor        :=@intrtleventWaitFor;
  end;
  SetThreadManager(CThreadManager);
end;


initialization
  if ThreadingAlreadyUsed then
    begin
      writeln('Threading has been used before cthreads was initialized.');
      writeln('Make wasmthreads one of the first units in your uses clause.');
      runerror(211);
    end;
  SetWasmThreadManager;
finalization
end.
