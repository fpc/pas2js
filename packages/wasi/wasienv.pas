
unit wasienv;

{$mode ObjFPC}
{$modeswitch externalclass}
{$INTERFACES CORBA}
{$WARN 5024 off}
{$WARN 4501 off}
interface

uses
  SysUtils, Classes, JS, WebAssembly, types;


Const
  WASI_ESUCCESS = 0;
  WASI_E2BIG = 1;
  WASI_EACCES = 2;
  WASI_EADDRINUSE = 3;
  WASI_EADDRNOTAVAIL = 4;
  WASI_EAFNOSUPPORT = 5;
  WASI_EAGAIN = 6;
  WASI_EALREADY = 7;
  WASI_EBADF = 8;
  WASI_EBADMSG = 9;
  WASI_EBUSY = 10;
  WASI_ECANCELED = 11;
  WASI_ECHILD = 12;
  WASI_ECONNABORTED = 13;
  WASI_ECONNREFUSED = 14;
  WASI_ECONNRESET = 15;
  WASI_EDEADLK = 16;
  WASI_EDESTADDRREQ = 17;
  WASI_EDOM = 18;
  WASI_EDQUOT = 19;
  WASI_EEXIST = 20;
  WASI_EFAULT = 21;
  WASI_EFBIG = 22;
  WASI_EHOSTUNREACH = 23;
  WASI_EIDRM = 24;
  WASI_EILSEQ = 25;
  WASI_EINPROGRESS = 26;
  WASI_EINTR = 27;
  WASI_EINVAL = 28;
  WASI_EIO = 29;
  WASI_EISCONN = 30;
  WASI_EISDIR = 31;
  WASI_ELOOP = 32;
  WASI_EMFILE = 33;
  WASI_EMLINK = 34;
  WASI_EMSGSIZE = 35;
  WASI_EMULTIHOP = 36;
  WASI_ENAMETOOLONG = 37;
  WASI_ENETDOWN = 38;
  WASI_ENETRESET = 39;
  WASI_ENETUNREACH = 40;
  WASI_ENFILE = 41;
  WASI_ENOBUFS = 42;
  WASI_ENODEV = 43;
  WASI_ENOENT = 44;
  WASI_ENOEXEC = 45;
  WASI_ENOLCK = 46;
  WASI_ENOLINK = 47;
  WASI_ENOMEM = 48;
  WASI_ENOMSG = 49;
  WASI_ENOPROTOOPT = 50;
  WASI_ENOSPC = 51;
  WASI_ENOSYS = 52;
  WASI_ENOTCONN = 53;
  WASI_ENOTDIR = 54;
  WASI_ENOTEMPTY = 55;
  WASI_ENOTRECOVERABLE = 56;
  WASI_ENOTSOCK = 57;
  WASI_ENOTSUP = 58;
  WASI_ENOTTY = 59;
  WASI_ENXIO = 60;
  WASI_EOVERFLOW = 61;
  WASI_EOWNERDEAD = 62;
  WASI_EPERM = 63;
  WASI_EPIPE = 64;
  WASI_EPROTO = 65;
  WASI_EPROTONOSUPPORT = 66;
  WASI_EPROTOTYPE = 67;
  WASI_ERANGE = 68;
  WASI_EROFS = 69;
  WASI_ESPIPE = 70;
  WASI_ESRCH = 71;
  WASI_ESTALE = 72;
  WASI_ETIMEDOUT = 73;
  WASI_ETXTBSY = 74;
  WASI_EXDEV = 75;
  WASI_ENOTCAPABLE = 76;

  WASI_SIGABRT = 0;
  WASI_SIGALRM = 1;
  WASI_SIGBUS = 2;
  WASI_SIGCHLD = 3;
  WASI_SIGCONT = 4;
  WASI_SIGFPE = 5;
  WASI_SIGHUP = 6;
  WASI_SIGILL = 7;
  WASI_SIGINT = 8;
  WASI_SIGKILL = 9;
  WASI_SIGPIPE = 10;
  WASI_SIGQUIT = 11;
  WASI_SIGSEGV = 12;
  WASI_SIGSTOP = 13;
  WASI_SIGTERM = 14;
  WASI_SIGTRAP = 15;
  WASI_SIGTSTP = 16;
  WASI_SIGTTIN = 17;
  WASI_SIGTTOU = 18;
  WASI_SIGURG = 19;
  WASI_SIGUSR1 = 20;
  WASI_SIGUSR2 = 21;
  WASI_SIGVTALRM = 22;
  WASI_SIGXCPU = 23;
  WASI_SIGXFSZ = 24;

  WASI_FILETYPE_UNKNOWN = 0;
  WASI_FILETYPE_BLOCK_DEVICE = 1;
  WASI_FILETYPE_CHARACTER_DEVICE = 2;
  WASI_FILETYPE_DIRECTORY = 3;
  WASI_FILETYPE_REGULAR_FILE = 4;
  WASI_FILETYPE_SOCKET_DGRAM = 5;
  WASI_FILETYPE_SOCKET_STREAM = 6;
  WASI_FILETYPE_SYMBOLIC_LINK = 7;

  WASI_FDFLAG_APPEND = $0001;
  WASI_FDFLAG_DSYNC = $0002;
  WASI_FDFLAG_NONBLOCK = $0004;
  WASI_FDFLAG_RSYNC = $0008;
  WASI_FDFLAG_SYNC = $0010;

  WASI_RIGHT_FD_DATASYNC             = $0000000000000001;
  WASI_RIGHT_FD_READ                 = $0000000000000002;
  WASI_RIGHT_FD_SEEK                 = $0000000000000004;
  WASI_RIGHT_FD_FDSTAT_SET_FLAGS     = $0000000000000008;
  WASI_RIGHT_FD_SYNC                 = $0000000000000010;
  WASI_RIGHT_FD_TELL                 = $0000000000000020;
  WASI_RIGHT_FD_WRITE                = $0000000000000040;
  WASI_RIGHT_FD_ADVISE               = $0000000000000080;
  WASI_RIGHT_FD_ALLOCATE             = $0000000000000100;
  WASI_RIGHT_PATH_CREATE_DIRECTORY   = $0000000000000200;
  WASI_RIGHT_PATH_CREATE_FILE        = $0000000000000400;
  WASI_RIGHT_PATH_LINK_SOURCE        = $0000000000000800;
  WASI_RIGHT_PATH_LINK_TARGET        = $0000000000001000;
  WASI_RIGHT_PATH_OPEN               = $0000000000002000;
  WASI_RIGHT_FD_READDIR              = $0000000000004000;
  WASI_RIGHT_PATH_READLINK           = $0000000000008000;
  WASI_RIGHT_PATH_RENAME_SOURCE      = $0000000000010000;
  WASI_RIGHT_PATH_RENAME_TARGET      = $0000000000020000;
  WASI_RIGHT_PATH_FILESTAT_GET       = $0000000000040000;
  WASI_RIGHT_PATH_FILESTAT_SET_SIZE  = $0000000000080000;
  WASI_RIGHT_PATH_FILESTAT_SET_TIMES = $0000000000100000;
  WASI_RIGHT_FD_FILESTAT_GET         = $0000000000200000;
  WASI_RIGHT_FD_FILESTAT_SET_SIZE    = $0000000000400000;
  WASI_RIGHT_FD_FILESTAT_SET_TIMES   = $0000000000800000;
  WASI_RIGHT_PATH_SYMLINK            = $0000000001000000;
  WASI_RIGHT_PATH_REMOVE_DIRECTORY   = $0000000002000000;
  WASI_RIGHT_PATH_UNLINK_FILE        = $0000000004000000;
  WASI_RIGHT_POLL_FD_READWRITE       = $0000000008000000;
  WASI_RIGHT_SOCK_SHUTDOWN           = $0000000010000000;

  RIGHTS_ALL = WASI_RIGHT_FD_DATASYNC or WASI_RIGHT_FD_READ
  or WASI_RIGHT_FD_SEEK or WASI_RIGHT_FD_FDSTAT_SET_FLAGS or WASI_RIGHT_FD_SYNC
  or WASI_RIGHT_FD_TELL or WASI_RIGHT_FD_WRITE or WASI_RIGHT_FD_ADVISE
  or WASI_RIGHT_FD_ALLOCATE or WASI_RIGHT_PATH_CREATE_DIRECTORY
  or WASI_RIGHT_PATH_CREATE_FILE or WASI_RIGHT_PATH_LINK_SOURCE
  or WASI_RIGHT_PATH_LINK_TARGET or WASI_RIGHT_PATH_OPEN or WASI_RIGHT_FD_READDIR
  or WASI_RIGHT_PATH_READLINK or WASI_RIGHT_PATH_RENAME_SOURCE
  or WASI_RIGHT_PATH_RENAME_TARGET or WASI_RIGHT_PATH_FILESTAT_GET
  or WASI_RIGHT_PATH_FILESTAT_SET_SIZE or WASI_RIGHT_PATH_FILESTAT_SET_TIMES
  or WASI_RIGHT_FD_FILESTAT_GET or WASI_RIGHT_FD_FILESTAT_SET_TIMES
  or WASI_RIGHT_FD_FILESTAT_SET_SIZE or WASI_RIGHT_PATH_SYMLINK
  or WASI_RIGHT_PATH_UNLINK_FILE or WASI_RIGHT_PATH_REMOVE_DIRECTORY
  or WASI_RIGHT_POLL_FD_READWRITE or WASI_RIGHT_SOCK_SHUTDOWN;

  RIGHTS_BLOCK_DEVICE_BASE = RIGHTS_ALL;
  RIGHTS_BLOCK_DEVICE_INHERITING = RIGHTS_ALL;

  RIGHTS_CHARACTER_DEVICE_BASE = RIGHTS_ALL;
  RIGHTS_CHARACTER_DEVICE_INHERITING = RIGHTS_ALL;

  RIGHTS_REGULAR_FILE_BASE = WASI_RIGHT_FD_DATASYNC or WASI_RIGHT_FD_READ
                           or WASI_RIGHT_FD_SEEK or WASI_RIGHT_FD_FDSTAT_SET_FLAGS or WASI_RIGHT_FD_SYNC
                           or WASI_RIGHT_FD_TELL or WASI_RIGHT_FD_WRITE or WASI_RIGHT_FD_ADVISE
                           or WASI_RIGHT_FD_ALLOCATE or WASI_RIGHT_FD_FILESTAT_GET
                           or WASI_RIGHT_FD_FILESTAT_SET_SIZE or WASI_RIGHT_FD_FILESTAT_SET_TIMES
                           or WASI_RIGHT_POLL_FD_READWRITE;
  RIGHTS_REGULAR_FILE_INHERITING = 00;

  RIGHTS_DIRECTORY_BASE = WASI_RIGHT_FD_FDSTAT_SET_FLAGS
                        or WASI_RIGHT_FD_SYNC or WASI_RIGHT_FD_ADVISE or WASI_RIGHT_PATH_CREATE_DIRECTORY
                        or WASI_RIGHT_PATH_CREATE_FILE or WASI_RIGHT_PATH_LINK_SOURCE
                        or WASI_RIGHT_PATH_LINK_TARGET or WASI_RIGHT_PATH_OPEN or WASI_RIGHT_FD_READDIR
                        or WASI_RIGHT_PATH_READLINK or WASI_RIGHT_PATH_RENAME_SOURCE
                        or WASI_RIGHT_PATH_RENAME_TARGET or WASI_RIGHT_PATH_FILESTAT_GET
                        or WASI_RIGHT_PATH_FILESTAT_SET_SIZE or WASI_RIGHT_PATH_FILESTAT_SET_TIMES
                        or WASI_RIGHT_FD_FILESTAT_GET or WASI_RIGHT_FD_FILESTAT_SET_TIMES
                        or WASI_RIGHT_PATH_SYMLINK or WASI_RIGHT_PATH_UNLINK_FILE
                        or WASI_RIGHT_PATH_REMOVE_DIRECTORY or WASI_RIGHT_POLL_FD_READWRITE;
  RIGHTS_DIRECTORY_INHERITING = RIGHTS_DIRECTORY_BASE
                        or RIGHTS_REGULAR_FILE_BASE;

  RIGHTS_SOCKET_BASE = WASI_RIGHT_FD_READ or WASI_RIGHT_FD_FDSTAT_SET_FLAGS
                     or WASI_RIGHT_FD_WRITE or WASI_RIGHT_FD_FILESTAT_GET
                     or WASI_RIGHT_POLL_FD_READWRITE or WASI_RIGHT_SOCK_SHUTDOWN;

  RIGHTS_SOCKET_INHERITING = RIGHTS_ALL;

  RIGHTS_TTY_BASE = WASI_RIGHT_FD_READ or WASI_RIGHT_FD_FDSTAT_SET_FLAGS
                  or WASI_RIGHT_FD_WRITE or WASI_RIGHT_FD_FILESTAT_GET
                  or WASI_RIGHT_POLL_FD_READWRITE;

  RIGHTS_TTY_INHERITING = 0;

  WASI_CLOCK_MONOTONIC = 0;
  WASI_CLOCK_PROCESS_CPUTIME_ID = 1;
  WASI_CLOCK_REALTIME = 2;
  WASI_CLOCK_THREAD_CPUTIME_ID = 3;

  WASI_EVENTTYPE_CLOCK = 0;
  WASI_EVENTTYPE_FD_READ = 1;
  WASI_EVENTTYPE_FD_WRITE = 2;

  WASI_FILESTAT_SET_ATIM = 1 << 0;
  WASI_FILESTAT_SET_ATIM_NOW = 1 << 1;
  WASI_FILESTAT_SET_MTIM = 1 << 2;
  WASI_FILESTAT_SET_MTIM_NOW = 1 << 3;

  WASI_O_CREAT = 1 << 0;
  WASI_O_DIRECTORY = 1 << 1;
  WASI_O_EXCL = 1 << 2;
  WASI_O_TRUNC = 1 << 3;

  WASI_PREOPENTYPE_DIR = 0;

  WASI_DIRCOOKIE_START = 0;

  WASI_STDIN_FILENO = 0;
  WASI_STDOUT_FILENO = 1;
  WASI_STDERR_FILENO = 2;

  WASI_WHENCE_CUR = 0;
  WASI_WHENCE_END = 1;
  WASI_WHENCE_SET = 2;


type
  // The imports as expected by WASI
  IWASI = interface ['{A03AC61B-3C68-4DA8-AC4F-53ED01814673}']
    // Please keep these sorted !!
    function args_get(argc, argvBufSize : NativeInt) : NativeInt;
    function args_sizes_get(argc, argvBufSize : NativeInt) : NativeInt;
    function clock_res_get(clockId, resolution: NativeInt): NativeInt;
    function clock_time_get(clockId, precision, time: NativeInt): NativeInt;
    function environ_get(environ, environBuf : NativeInt) : NativeInt;
    function environ_sizes_get(environCount, environBufSize : NativeInt) : NativeInt;
    function fd_advise (fd, offset, len, advice : NativeInt) : NativeInt;
    function fd_allocate (fd, offset, len : NativeInt) : NativeInt;
    function fd_close(fd : NativeInt) : NativeInt;
    function fd_datasync (fd : NativeInt) : NativeInt;
    function fd_fdstat_get(fd,bufPtr : NativeInt) : NativeInt;
    function fd_fdstat_set_flags (fd, flags: NativeInt) : NativeInt;
    function fd_fdstat_set_rights (fd, fsRightsBase, fsRightsInheriting: NativeInt) : NativeInt;
    function fd_filestat_get(fd, bufPtr : NativeInt) : NativeInt;
    function fd_filestat_set_size (fd, stSize: NativeInt) : NativeInt;
    function fd_filestat_set_times (fd, stAtim, stMtim, fstflags: NativeInt) : NativeInt;
    function fd_pread(fd, iovs, iovsLen, offset, nread : NativeInt) : NativeInt;
    function fd_prestat_dir_name(fd, pathPtr, pathLen : NativeInt) : NativeInt;
    function fd_prestat_get(fd, bufPtr: NativeInt) : NativeInt;
    function fd_pwrite(fd, iovs, iovsLen, offset, nwritten : NativeInt) : NativeInt;
    function fd_read(fd, iovs, iovsLen, nread : NativeInt) : NativeInt;
    function fd_readdir(fd, bufPtr, bufLen, cookie, bufusedPtr : NativeInt) : NativeInt;
    function fd_renumber(afrom,ato : NativeInt) : NativeInt;
    function fd_seek(fd, offset, whence, newOffsetPtr : NativeInt) : NativeInt;
    function fd_sync(fd : NativeInt) : NativeInt;
    function fd_tell(fd, offsetPtr: NativeInt) : NativeInt;
    function fd_write(fd,iovs,iovsLen,nwritten : NativeInt) : NativeInt;
    function path_create_directory (fd, pathPtr, pathLen : NativeInt) : NativeInt;
    function path_filestat_get (fd, flags, pathPtr, pathLen, bufPtr : NativeInt) : NativeInt;
    function path_filestat_set_times(fd, fstflags, pathPtr, pathLen, stAtim, stMtim : NativeInt) : NativeInt;
    function path_link (oldFd, oldFlags, oldPath, oldPathLen, newFd, newPath, newPathLen: NativeInt) : NativeInt;
    function path_open (dirfd, dirflags, pathPtr, pathLen, oflags, fsRightsBase, fsRightsInheriting, fsFlags, fd : NativeInt) : NativeInt;
    function path_readlink (fd, pathPtr, pathLen, buf, bufLen, bufused : NativeInt) : NativeInt;
    function path_remove_directory (fd, pathPtr, pathLen : NativeInt) : NativeInt;
    function path_rename (oldFd, oldPath, oldPathLen, newFd, newPath, newPathLen : NativeInt) : NativeInt;
    function path_symlink (oldPath, oldPathLen, fd, newPath, newPathLen : NativeInt) : NativeInt;
    function path_unlink_file (fd, pathPtr, pathLen : NativeInt) : NativeInt;
    function poll_oneoff(sin, sout, nsubscriptions, nevents : NativeInt) : NativeInt;
    function proc_exit(rval : NativeInt) : NativeInt;
    function proc_raise (sig : NativeInt) : NativeInt;
    function random_get (bufPtr, bufLen: NativeInt) : NativeInt;
    function sched_yield() : NativeInt;
    function sock_recv() : NativeInt;
    function sock_send() : NativeInt;
    function sock_shutdown() : NativeInt;
  end;

  TWASIWriteEvent = Reference to Procedure(Sender : TObject; Const aOutput : String);

  // Standard FPC exports.
  TWASIExports = Class External name 'Object' (TJSModulesExports)
  Public
    Procedure start; external name '_start';
    function AllocMem(aSize : Integer) : Integer; external name 'wasiAlloc';
    function freeMem(aLocation : Integer) : Integer; external name 'wasiFree';
  end;

  TGetConsoleInputBufferEvent = Reference to Procedure(Sender : TObject; Var AInput : TJSUint8Array);
  TGetConsoleInputStringEvent =Reference to Procedure (Sender : TObject; Var AInput : string);

  TImportExtension = Class;

  { TPas2JSWASIEnvironment }

  TPas2JSWASIEnvironment = class (TObject,IWASI)
  Private
    FExitCode: Nativeint;
    FImportObject : TJSObject;
    Finstance: TJSWebAssemblyInstance;
    FIsLittleEndian: Boolean;
    FModuleInstanceExports : TJSModulesExports;
    FOnGetConsoleInputBuffer: TGetConsoleInputBufferEvent;
    FOnGetConsoleInputString: TGetConsoleInputStringEvent;
    FOnStdErrorWrite: TWASIWriteEvent;
    FOnStdOutputWrite: TWASIWriteEvent;
    FImportExtensions : TFPList;
    FWASIImportName : string;
    FMemory : TJSWebAssemblyMemory;
    function GetConsoleInputBuffer: TJSUint8Array;
    function GetFileBuffer(FD: NativeInt): TJSUint8Array;
    function GetImportObject: TJSObject;
    function getiovs(view: TJSDataView; iovs, iovsLen: NativeInt): TJSArray;
    function GetMemory: TJSWebassemblyMemory;
    procedure SetInstance(AValue: TJSWebAssemblyInstance);
  Protected
    Class Var UTF8TextDecoder: TJSTextDecoder;
  Protected
    class procedure setBigUint64(View: TJSDataView; byteOffset, value: NativeInt; littleEndian: Boolean);
    class procedure setBigInt64(View: TJSDataView; byteOffset, value: NativeInt; littleEndian: Boolean);
    procedure DoConsoleWrite(IsStdErr: Boolean; aBytes: TJSArray); virtual;
    procedure GetImports(aImports: TJSObject); virtual;
    Function GetTime(aClockID : NativeInt): NativeInt; virtual;
    function getModuleMemoryDataView : TJSDataView;
    procedure AddExtension(aExtension : TImportExtension); virtual;
    procedure RemoveExtension(aExtension : TImportExtension); virtual;

    // IWASI calls
    // !! Please keep these sorted !!

    function args_get(argv, argvBuf : NativeInt) : NativeInt; virtual;
    function args_sizes_get(argc, argvBufSize : NativeInt) : NativeInt; virtual;
    function clock_res_get(clockId, resolution: NativeInt): NativeInt; virtual;
    function clock_time_get(clockId, precision, time: NativeInt): NativeInt; virtual;
    function environ_get(environ, environBuf : NativeInt) : NativeInt; virtual;
    function environ_sizes_get(environCount, environBufSize : NativeInt) : NativeInt; virtual;
    function fd_advise (fd, offset, len, advice : NativeInt) : NativeInt; virtual;
    function fd_allocate (fd, offset, len : NativeInt) : NativeInt; virtual;
    function fd_close(fd : NativeInt) : NativeInt; virtual;
    function fd_datasync (fd : NativeInt) : NativeInt; virtual;
    function fd_fdstat_get(fd,bufPtr : NativeInt) : NativeInt; virtual;
    function fd_fdstat_set_flags (fd, flags: NativeInt) : NativeInt; virtual;
    function fd_fdstat_set_rights (fd, fsRightsBase, fsRightsInheriting: NativeInt) : NativeInt; virtual;
    function fd_filestat_get (fd, bufPtr: NativeInt) : NativeInt; virtual;
    function fd_filestat_set_size (fd, stSize: NativeInt) : NativeInt; virtual;
    function fd_filestat_set_times (fd, stAtim, stMtim, fstflags: NativeInt) : NativeInt; virtual;
    function fd_pread(fd, iovs, iovsLen, offset, nread : NativeInt) : NativeInt; virtual;
    function fd_prestat_dir_name(fd, pathPtr, pathLen : NativeInt) : NativeInt; virtual;
    function fd_prestat_get(fd, bufPtr: NativeInt) : NativeInt; virtual;
    function fd_pwrite(fd, iovs, iovsLen, offset, nwritten : NativeInt) : NativeInt;virtual;
    function fd_read(fd, iovs, iovsLen, nread : NativeInt) : NativeInt; virtual;
    function fd_readdir(fd, bufPtr, bufLen, cookie, bufusedPtr : NativeInt) : NativeInt; virtual;
    function fd_renumber(afrom,ato : NativeInt) : NativeInt; virtual;
    function fd_seek(fd, offset, whence, newOffsetPtr : NativeInt) : NativeInt; virtual;
    function fd_sync(fd : NativeInt) : NativeInt; virtual;
    function fd_tell(fd, offsetPtr: NativeInt) : NativeInt; virtual;
    function fd_write(fd,iovs,iovsLen,nwritten : NativeInt) : NativeInt; virtual;
    function path_create_directory (fd, pathPtr, pathLen : NativeInt) : NativeInt;
    function path_filestat_get (fd, flags, pathPtr, pathLen, bufPtr : NativeInt) : NativeInt;
    function path_filestat_set_times(fd, fstflags, pathPtr, pathLen, stAtim, stMtim : NativeInt) : NativeInt;
    function path_link (oldFd, oldFlags, oldPath, oldPathLen, newFd, newPath, newPathLen: NativeInt) : NativeInt;
    function path_open (dirfd, dirflags, pathPtr, pathLen, oflags, fsRightsBase, fsRightsInheriting, fsFlags, fd : NativeInt) : NativeInt; virtual;
    function path_readlink (fd, pathPtr, pathLen, buf, bufLen, bufused : NativeInt) : NativeInt; virtual;
    function path_remove_directory (fd, pathPtr, pathLen : NativeInt) : NativeInt;
    function path_rename (oldFd, oldPath, oldPathLen, newFd, newPath, newPathLen : NativeInt) : NativeInt;
    function path_symlink (oldPath, oldPathLen, fd, newPath, newPathLen : NativeInt) : NativeInt;
    function path_unlink_file (fd, pathPtr, pathLen : NativeInt) : NativeInt;
    function poll_oneoff(sin, sout, nsubscriptions, nevents : NativeInt) : NativeInt; virtual;
    function proc_exit(rval : NativeInt) : NativeInt; virtual;
    function proc_raise (sig : NativeInt) : NativeInt; virtual;
    function random_get (bufPtr, bufLen: NativeInt) : NativeInt; virtual;
    function sched_yield() : NativeInt; virtual;
    function sock_recv() : NativeInt; virtual;
    function sock_send() : NativeInt; virtual;
    function sock_shutdown() : NativeInt; virtual;
  Protected
    Procedure SetMemory(aMemory : TJSWebAssemblyMemory);
  Public
    class constructor init;
    Constructor Create;
    Destructor Destroy; override;
    Function GetUTF8StringFromMem(aLoc, aLen : Longint) : String;
    Procedure SetMemInfoInt8(aLoc : Integer; aValue : ShortInt);
    Procedure SetMemInfoInt16(aLoc : Integer; aValue : SmallInt);
    Procedure SetMemInfoInt32(aLoc : Integer; aValue : Longint);
    Procedure SetMemInfoInt64(aLoc : Integer; aValue : NativeInt);
    Procedure SetMemInfoUInt8(aLoc : Integer; aValue : Byte);
    Procedure SetMemInfoUInt16(aLoc : Integer; aValue : Word);
    Procedure SetMemInfoUInt32(aLoc : Integer; aValue : Cardinal);
    Procedure SetMemInfoUInt64(aLoc : Integer; aValue : NativeUint);
    // Add imports
    Procedure AddImports(aObject: TJSObject); 
    Property ImportObject : TJSObject Read GetImportObject;
    Property IsLittleEndian : Boolean Read FIsLittleEndian Write FIsLittleEndian;
    Property OnStdOutputWrite : TWASIWriteEvent Read FOnStdOutputWrite Write FOnStdOutputWrite;
    Property OnStdErrorWrite : TWASIWriteEvent Read FOnStdErrorWrite Write FOnStdErrorWrite;
    Property OnGetConsoleInputBuffer : TGetConsoleInputBufferEvent Read FOnGetConsoleInputBuffer Write FOnGetConsoleInputBuffer;
    Property OnGetConsoleInputString : TGetConsoleInputStringEvent Read FOnGetConsoleInputString Write FOnGetConsoleInputString;
    Property Instance : TJSWebAssemblyInstance Read Finstance Write SetInstance;
    Property Memory : TJSWebassemblyMemory Read GetMemory;
    Property Exitcode : Nativeint Read FExitCode;
    // Default is set to the one expected by FPC runtime: wasi_snapshot_preview1
    Property WASIImportName : String Read FWASIImportName Write FWASIImportName;
  end;

  { TImportExtension }

  TImportExtension = class (TObject)
  Private
    FEnv : TPas2JSWASIEnvironment;
  Protected
    function getModuleMemoryDataView : TJSDataView;  
  Public
    Constructor Create(aEnv : TPas2JSWASIEnvironment); virtual;
    Destructor Destroy; override;
    Procedure FillImportObject(aObject : TJSObject); virtual; abstract;
    Function ImportName : String; virtual; abstract;
    Property Env : TPas2JSWASIEnvironment Read FEnv;
  end;

  TRunWebassemblyProc = reference to Procedure(aExports : TWASIExports);
  TWebAssemblyStartDescriptor = record
    // Module
    Module : TJSWebAssemblyModule;
    // memory to use
    Memory : TJSWebAssemblyMemory;
    // Table to use
    Table : TJSWebAssemblyTable;
    // Exports of module
    Exported : TWASIExports;
    // Imports of module
    Imports : TJSOBject;
    // Instance
    Instance : TJSWebAssemblyInstance;
    // Procedure to actually run a function.
    CallRun : TRunWebassemblyProc;
    // After run, if an exception occurred, this is filled with error class/message.
    RunExceptionClass : String;
    RunExceptionMessage : String;
  end;


  TBeforeStartCallBack = Reference to Function (Sender : TObject; aDescriptor : TWebAssemblyStartDescriptor) : Boolean;
  TAfterStartCallBack = Reference to Procedure (Sender : TObject; aDescriptor : TWebAssemblyStartDescriptor);

  TBeforeStartEvent = Procedure (Sender : TObject; aDescriptor : TWebAssemblyStartDescriptor; var aAllowRun : Boolean) of object;
  TAfterStartEvent = Procedure (Sender : TObject; aDescriptor : TWebAssemblyStartDescriptor) of object;

  TFailEvent =  Procedure (Sender : TObject; aFail : JSValue) of object;

  TConsoleReadEvent = Procedure(Sender : TObject; Var AInput : String) of object;
  TConsoleWriteEvent = Procedure (Sender : TObject; aOutput : string) of object;

  { TWASIHost }

  TWASIHost = Class(TComponent)
  Private
    FAfterInstantation: TNotifyEvent;
    FAfterStart: TAfterStartEvent;
    FBeforeInstantation: TNotifyEvent;
    FBeforeStart: TBeforeStartEvent;
    FEnv: TPas2JSWASIEnvironment;
    FExported: TWASIExports;
    FOnInstantiateFail: TFailEvent;
    FOnLoadFail: TFailEvent;
    FPreparedStartDescriptor: TWebAssemblyStartDescriptor;
    FMemoryDescriptor : TJSWebAssemblyMemoryDescriptor;
    FOnConsoleRead: TConsoleReadEvent;
    FOnConsoleWrite: TConsoleWriteEvent;
    FPredefinedConsoleInput: TStrings;
    FReadLineCount : Integer;
    FRunEntryFunction: String;
    FTableDescriptor : TJSWebAssemblyTableDescriptor;
    function GetStartDescriptorReady: Boolean;
    function GetUseSharedMemory: Boolean;
    procedure SetPredefinedConsoleInput(AValue: TStrings);
    procedure SetUseSharedMemory(AValue: Boolean);
  protected
    // Called after instantiation was OK.
    Procedure DoAfterInstantiate; virtual;
    // Called before instantiation starts.
    Procedure DoBeforeInstantiate; virtual;
    // Called when loading fails
    Procedure DoLoadFail(aError : JSValue); virtual;
    // Called when instantiating fails
    Procedure DoInstantiateFail(aError : JSValue); virtual;
    // Prepare start descriptor
    Procedure PrepareWebAssemblyInstance(aDescr: TWebAssemblyStartDescriptor); virtual;
    // Call the run function on an instantiated webassembly
    function RunWebAssemblyInstance(aBeforeStart: TBeforeStartCallback; aAfterStart: TAfterStartCallback; aRun : TRunWebassemblyProc): Boolean; virtual; overload;
    // Prepare and run web assembly instance.
    function RunWebAssemblyInstance(aDescr: TWebAssemblyStartDescriptor; aBeforeStart: TBeforeStartCallback; aAfterStart: TAfterStartCallback): Boolean; overload;
    // Standard Input/Output reads
    procedure DoStdRead(Sender: TObject; var AInput: string); virtual;
    procedure DoStdWrite(Sender: TObject; const aOutput: String); virtual;
    // Load file from path ans instantiate a webassembly from it.
    function CreateWebAssembly(aPath: string; aImportObject: TJSObject): TJSPromise; virtual;
    // Create a WASI environment. Called during constructor, override to customize.
    Function CreateWasiEnvironment : TPas2JSWASIEnvironment; virtual;
    // Create Standard webassembly table description
    function GetTable: TJSWebAssemblyTable; virtual;
    // Create tandard webassembly memory.
    function GetMemory: TJSWebAssemblyMemory; virtual;
  public
    Constructor Create(aOwner : TComponent); override;
    Destructor Destroy; override;
    // Will call OnConsoleWrite or write to console
    procedure WriteOutput(const aOutput: String); virtual;
    // Get prepared descriptor
    Property PreparedStartDescriptor : TWebAssemblyStartDescriptor Read FPreparedStartDescriptor;
    // Initialize a start descriptor.
    function InitStartDescriptor(aMemory: TJSWebAssemblyMemory; aTable: TJSWebAssemblyTable; aImportObj: TJSObject): TWebAssemblyStartDescriptor;
    // Load and start webassembly. If DoRun is true, then Webassembly entry point is called.
    // If aBeforeStart is specified, then it is called prior to calling run, and can disable running.
    // If aAfterStart is specified, then it is called after calling run. It is not called if running was disabled.
    Procedure StartWebAssembly(aPath: string; DoRun: Boolean;  aBeforeStart: TBeforeStartCallback; aAfterStart: TAfterStartCallback);
    // Run the prepared descriptor
    Procedure RunPreparedDescriptor;
    // Initial memory descriptor
    Property MemoryDescriptor : TJSWebAssemblyMemoryDescriptor Read FMemoryDescriptor Write FMemoryDescriptor;
    // Import/export table descriptor
    Property TableDescriptor : TJSWebAssemblyTableDescriptor Read FTableDescriptor Write FTableDescriptor;
    // Environment to be used
    Property WasiEnvironment : TPas2JSWASIEnvironment Read FEnv;
    // Exported functions. Also available in start descriptor.
    Property Exported : TWASIExports Read FExported;
    // Is the descriptor prepared ?
    Property StartDescriptorReady : Boolean Read GetStartDescriptorReady;
    // Default console input
    Property PredefinedConsoleInput : TStrings Read FPredefinedConsoleInput Write SetPredefinedConsoleInput;

    // Name of function to run. If empty, the FPC default _start is used.
    Property RunEntryFunction : String Read FRunEntryFunction Write FRunEntryFunction;
    // Called after webassembly start was run. Not called if webassembly was not run.
    Property AfterStart : TAfterStartEvent Read FAfterStart Write FAfterStart;
    // Called before running webassembly. If aAllowRun is false, running is disabled
    Property BeforeStart : TBeforeStartEvent Read FBeforeStart Write FBeforeStart;
    // Called when reading from console (stdin). If not set, PredefinedConsoleinput is used.
    property OnConsoleRead : TConsoleReadEvent Read FOnConsoleRead Write FOnConsoleRead;
    // Called when writing to console (stdout). If not set, console.log is used.
    property OnConsoleWrite : TConsoleWriteEvent Read FOnConsoleWrite Write FOnConsoleWrite;
    // Called when fetch of the wasm module fails.
    Property OnLoadFail : TFailEvent Read FOnLoadFail Write FOnLoadFail;
    // Called when instantiation of the wasm module fails.
    Property OnInstantiateFail : TFailEvent Read FOnInstantiateFail Write FOnInstantiateFail;
    // Use Shared memory for webassembly instances ?
    Property UseSharedMemory : Boolean Read GetUseSharedMemory Write SetUseSharedMemory;
    // Executed after instantiation
    Property AfterInstantation : TNotifyEvent Read FAfterInstantation Write FAfterInstantation;
    // Executed before instantiation
    Property BeforeInstantation : TNotifyEvent Read FBeforeInstantation Write FBeforeInstantation;

  end;

implementation

uses weborworker;

{ TWASIHost }

procedure TWASIHost.DoStdRead(Sender: TObject; var AInput: string);

Var
  S : String;
begin
  S:='';
  if Assigned(FOnConsoleRead) then
    FOnConsoleRead(Self,S)
  else
    begin
    if (FReadLineCount<FPredefinedConsoleInput.Count) then
      begin
      S:=FPredefinedConsoleInput[FReadLineCount];
      Inc(FReadLineCount);
      end;
    end;
  aInput:=S;
end;

procedure TWASIHost.SetPredefinedConsoleInput(AValue: TStrings);
begin
  if FPredefinedConsoleInput=AValue then Exit;
  FPredefinedConsoleInput.Assign(AValue);
end;

function TWASIHost.GetUseSharedMemory: Boolean;
begin
  Result:=FMemoryDescriptor.shared;
  if isUndefined(Result) then
    Result:=False;
end;

function TWASIHost.GetStartDescriptorReady: Boolean;
begin
  With FPreparedStartDescriptor do
    Result:=Assigned(Memory) and Assigned(Module);
end;

procedure TWASIHost.SetUseSharedMemory(AValue: Boolean);
begin
  FMemoryDescriptor.shared:=aValue;
end;

procedure TWASIHost.DoAfterInstantiate;
begin
  If Assigned(FAfterInstantation) then
    FAfterInstantation(Self);
end;

procedure TWASIHost.DoBeforeInstantiate;
begin
  If Assigned(FBeforeInstantation) then
    FBeforeInstantation(Self);
end;

procedure TWASIHost.DoLoadFail(aError: JSValue);
begin
  If Assigned(FOnLoadFail) then
    FOnLoadFail(Self,aError);
end;

procedure TWASIHost.DoInstantiateFail(aError: JSValue);
begin
  If Assigned(FOnInstantiateFail) then
    FOnInstantiateFail(Self,aError);
end;

procedure TWASIHost.PrepareWebAssemblyInstance(
  aDescr: TWebAssemblyStartDescriptor);
begin
  FPreparedStartDescriptor:=aDescr;
  FExported:=aDescr.Exported;
  WasiEnvironment.Instance:=aDescr.Instance;
  WasiEnvironment.SetMemory(aDescr.Memory);
  // We do this here, so in the event, the FPreparedStartDescriptor Is ready.
  DoAfterInstantiate;
end;

function TWASIHost.RunWebAssemblyInstance(aBeforeStart: TBeforeStartCallback; aAfterStart: TAfterStartCallback; aRun : TRunWebassemblyProc): Boolean;

begin
  Result:=True;
  // Writeln('Entering RunWebAssemblyInstance');
  if Assigned(aBeforeStart) then
    Result:=aBeforeStart(Self,FPreparedStartDescriptor);
  if Assigned(FBeforeStart) then
    FBeforeStart(Self,FPreparedStartDescriptor,Result);
  if not Result then
    exit;
  try
    if aRun=Nil then
      aRun:=FPreparedStartDescriptor.CallRun;
    aRun(FPreparedStartDescriptor.Exported);
    if Assigned(aAfterStart) then
      aAfterStart(Self,FPreparedStartDescriptor);
    if Assigned(FAfterStart) then
      FAfterStart(Self,FPreparedStartDescriptor)
  except
    On E : exception do
      begin
      FPreparedStartDescriptor.RunExceptionClass:=E.ClassName;
      FPreparedStartDescriptor.RunExceptionMessage:=E.Message;
      end;
    On JE : TJSError do
      begin
      FPreparedStartDescriptor.RunExceptionClass:=jsTypeOf(JE);
      FPreparedStartDescriptor.RunExceptionMessage:=JE.Message;
      end;
    On OE : TJSObject do
      begin
      FPreparedStartDescriptor.RunExceptionClass:=jsTypeOf(OE);
      FPreparedStartDescriptor.RunExceptionMessage:=TJSJSON.Stringify(OE);
      end;
  end;
end;

procedure TWASIHost.DoStdWrite(Sender: TObject; const aOutput: String);
begin
  WriteOutput(aOutput);
end;

function TWASIHost.CreateWebAssembly(aPath: string; aImportObject: TJSObject
  ): TJSPromise;

  Function InstantiateOK(Res : JSValue) : JSValue;

  begin
    Result:=res;
  end;

  Function InstantiateFail(Res : JSValue) : JSValue;

  begin
    Result:=False;
    DoInstantiateFail(res);
  end;


  Function ArrayOK(res2 : jsValue) : JSValue;

  begin
    DoBeforeInstantiate;
    Result:=TJSWebAssembly.instantiate(TJSArrayBuffer(res2),aImportObject)._then(@InstantiateOK,@InstantiateFail);
  end;

  function fetchOK(res : jsValue) : JSValue;
  begin
    Result:=TJSResponse(Res).arrayBuffer._then(@ArrayOK,Nil);
  end;

  function DoFail(res : jsValue) : JSValue;
  begin
    Result:=False;
    DoLoadFail(res);
  end;

begin
  Result:=fetch(aPath)._then(@fetchOK,@DoFail).Catch(@DoFail);
end;

function TWASIHost.CreateWasiEnvironment: TPas2JSWASIEnvironment;
begin
  Result:=TPas2JSWASIEnvironment.Create;
end;

function TWASIHost.GetTable: TJSWebAssemblyTable;
begin
  Result:=TJSWebAssemblyTable.New(FTableDescriptor);
end;

function TWASIHost.GetMemory: TJSWebAssemblyMemory;
begin
  Result:=TJSWebAssemblyMemory.New(FMemoryDescriptor);
end;

constructor TWASIHost.Create(aOwner: TComponent);
begin
  inherited Create(aOwner);
  FEnv:=CreateWasiEnvironment;
  FEnv.OnStdErrorWrite:=@DoStdWrite;
  FEnv.OnStdOutputWrite:=@DoStdWrite;
  Fenv.OnGetConsoleInputString:=@DoStdRead;
  FMemoryDescriptor.initial:=256;
  FMemoryDescriptor.maximum:=256;
  FMemoryDescriptor.shared:=False;
  FTableDescriptor.initial:=0;
  FTableDescriptor.maximum:=0;
  FTableDescriptor.element:='anyfunc';
  FPredefinedConsoleInput:=TStringList.Create;
end;

destructor TWASIHost.Destroy;
begin
  FreeAndNil(FPredefinedConsoleInput);
  FreeAndNil(FEnv);
  inherited Destroy;
end;

procedure TWASIHost.WriteOutput(const aOutput: String);
begin
  if assigned(FOnConsoleWrite) then
    FOnConsoleWrite(Self,aOutput)
  else
    Writeln(aOutput);
end;


function TWASIHost.RunWebAssemblyInstance(aDescr: TWebAssemblyStartDescriptor;
  aBeforeStart: TBeforeStartCallback;
  aAfterStart: TAfterStartCallback): Boolean;

begin
  Result:=RunWebAssemblyInstance(aBeforeStart,aAfterStart,Nil);
end;

procedure TWASIHost.StartWebAssembly(aPath: string; DoRun: Boolean; aBeforeStart: TBeforeStartCallback; aAfterStart: TAfterStartCallback);

Var
  WASD : TWebAssemblyStartDescriptor;

  function InitEnv(aValue: JSValue): JSValue;

  Var
    InstResult : TJSInstantiateResult absolute aValue;

  begin
    Result:=True;
    WASD.Instance:=InstResult.Instance;
    WASD.Module:=InstResult.Module;
    WASD.Exported:=TWASIExports(TJSObject(WASD.Instance.exports_));
    WASD.CallRun:=Procedure(aExports : TWASIExports)
      begin
      if FRunEntryFunction='' then
        aExports.Start
      else
        TProcedure(aExports[RunEntryFunction])();
      end;
    PrepareWebAssemblyInstance(WASD);
    if DoRun then
      RunWebAssemblyInstance(aBeforeStart,aAfterStart,Nil);
  end;

  function DoFail(aValue: JSValue): JSValue;

  begin
    Result:=True;
    Console.Log('Failed to create webassembly. Reason:');
    Console.Debug(aValue);
  end;

begin
  FReadLineCount:=0;
  // Clear current descriptor.
  FPreparedStartDescriptor:=Default(TWebAssemblyStartDescriptor);
  WASD:=InitStartDescriptor(GetMemory,GetTable,Nil);
  CreateWebAssembly(aPath,WASD.Imports)._then(@initEnv,@DoFail).catch(@DoFail);
end;

procedure TWASIHost.RunPreparedDescriptor;
begin
  RunWebAssemblyInstance(Nil,Nil,Nil)
end;

function TWASIHost.InitStartDescriptor(aMemory: TJSWebAssemblyMemory;
  aTable: TJSWebAssemblyTable; aImportObj: TJSObject
  ): TWebAssemblyStartDescriptor;

begin
  Result.Memory:=aMemory;
  Result.Table:=aTable;
  if Not assigned(aImportObj) then
    aImportObj:=TJSObject.New;
  aImportObj['env']:=new([
    'memory', Result.Memory,
    'tbl', Result.Table
  ]);
  FEnv.AddImports(aImportObj);
  Result.Imports:=aImportObj;
end;

function TImportExtension.getModuleMemoryDataView : TJSDataView;  

begin
  Result:=FEnv.getModuleMemoryDataView;
end;

constructor TImportExtension.Create(aEnv: TPas2JSWASIEnvironment);

begin
  FEnv:=aEnv;
  if Assigned(Fenv) then
    Fenv.AddExtension(Self);
end;

destructor TImportExtension.Destroy;
begin
  if Assigned(Fenv) then
    Fenv.RemoveExtension(Self);
  inherited Destroy;
end;

procedure TPas2JSWASIEnvironment.AddImports(aObject: TJSObject);

Var
  Ext : TImportExtension;
  I : Integer;
  O : TJSObject;
  
begin
  aObject[WASIImportName]:=ImportObject;
  if Assigned(FImportExtensions) then
    For I:=0 to FImportExtensions.Count-1 do
      begin
      Ext:=TImportExtension(FImportExtensions[i]);
      O:=TJSObject.New;
      Ext.FillImportObject(O);
      aObject[Ext.ImportName]:=O;
      end;
end;

procedure TPas2JSWASIEnvironment.AddExtension(aExtension : TImportExtension); 
begin
  if Not Assigned(FImportExtensions) then
    FImportExtensions:=TFPList.Create;
  FImportExtensions.Add(aExtension);
end;

procedure TPas2JSWASIEnvironment.RemoveExtension(aExtension: TImportExtension);

begin
 if Assigned(FImportExtensions) then
   FImportExtensions.Remove(aExtension);
end;

function TPas2JSWASIEnvironment.getModuleMemoryDataView: TJSDataView;
begin
  Result:=TJSDataView.New(Memory.buffer);
end;

function TPas2JSWASIEnvironment.fd_prestat_get(fd, bufPtr: NativeInt
  ): NativeInt;
begin
  console.log('Unimplemented: TPas2JSWASIEnvironment.fd_prestat_get');
  Result:=WASI_EBADF;
end;

function TPas2JSWASIEnvironment.fd_prestat_dir_name(fd, pathPtr,
  pathLen: NativeInt): NativeInt;
begin
  console.log('Unimplemented: TPas2JSWASIEnvironment.fd_prestat_dir_name');
  Result:=WASI_ENOSYS;
end;

function TPas2JSWASIEnvironment.environ_sizes_get(environCount,
  environBufSize: NativeInt): NativeInt;

Var
  View : TJSDataView;

begin
  view:=getModuleMemoryDataView();
  view.setUint32(environCount, 0, IsLittleEndian);
  view.setUint32(environBufSize, 0, IsLittleEndian);
  Result:= WASI_ESUCCESS;
end;

function TPas2JSWASIEnvironment.environ_get(environ, environBuf: NativeInt
  ): NativeInt;
begin
  Result:= WASI_ESUCCESS;
end;

function TPas2JSWASIEnvironment.args_sizes_get(argc, argvBufSize: NativeInt
  ): NativeInt;

Var
  View : TJSDataView;

begin
  view:=getModuleMemoryDataView();
  view.setUint32(argc, 0, IsLittleEndian);
  view.setUint32(argvBufSize, 0, IsLittleEndian);
  Result:=WASI_ESUCCESS;
end;

function TPas2JSWASIEnvironment.args_get(argv, argvBuf: NativeInt): NativeInt;
begin
  Result:=WASI_ESUCCESS;
end;

class procedure TPas2JSWASIEnvironment.setBigUint64(View: TJSDataView;
  byteOffset, value: NativeInt; littleEndian: Boolean);

Var
  LowWord,HighWord : Integer;

begin
  lowWord:=value;
  highWord:=value shr 32;
  if LittleEndian then
    begin
    view.setUint32(ByteOffset+0, lowWord, littleEndian);
    view.setUint32(ByteOffset+4, highWord, littleEndian);
    end
  else
    begin
    view.setUint32(ByteOffset+4, lowWord, littleEndian);
    view.setUint32(ByteOffset+0, highWord, littleEndian);
    end;
end;

class procedure TPas2JSWASIEnvironment.setBigInt64(View: TJSDataView;
  byteOffset, value: NativeInt; littleEndian: Boolean);

Var
  LowWord,HighWord : Integer;

begin
  lowWord:=value;
  highWord:=value shr 32;
  if LittleEndian then
    begin
    view.setint32(ByteOffset+0, lowWord, littleEndian);
    view.setint32(ByteOffset+4, highWord, littleEndian);
    end
  else
    begin
    view.setint32(ByteOffset+4, lowWord, littleEndian);
    view.setint32(ByteOffset+0, highWord, littleEndian);
    end;
end;


procedure TPas2JSWASIEnvironment.SetInstance(AValue: TJSWebAssemblyInstance);
begin
  if Finstance=AValue then Exit;
  Finstance:=AValue;
  FModuleInstanceExports:=Finstance.exports_;
  if Not Assigned(FMemory) and Assigned(FModuleInstanceExports.Memory) then
    FMemory:=FModuleInstanceExports.Memory;
end;

function TPas2JSWASIEnvironment.GetTime(aClockID: NativeInt): NativeInt;
begin
  Result:=-1;
  Case aClockId of
  WASI_CLOCK_MONOTONIC:
    Result:=TJSDate.Now;
  WASI_CLOCK_REALTIME:
    Result:=TJSDate.Now;
  WASI_CLOCK_PROCESS_CPUTIME_ID,
  WASI_CLOCK_THREAD_CPUTIME_ID:
    Result:=TJSDate.Now;
  end;
  Result:=Result*1000000
end;


function TPas2JSWASIEnvironment.fd_fdstat_get(fd, bufPtr: NativeInt): NativeInt;

Var
  View : TJSDataView;

begin
  view:=getModuleMemoryDataView();
  view.setUint8(bufPtr, fd);
  view.setUint16(bufPtr + 2, 0, IsLittleEndian);
  view.setUint16(bufPtr + 4, 0, IsLittleEndian);
  setBigUint64(View, bufPtr + 8, 0, IsLittleEndian);
  setBigUint64(View, bufPtr + 8 + 8, 0, IsLittleEndian);
  Result:= WASI_ESUCCESS;
end;

function TPas2JSWASIEnvironment.fd_fdstat_set_flags(fd, flags: NativeInt): NativeInt;
begin
  console.log('Unimplemented: TPas2JSWASIEnvironment.fd_fdstat_set_flags');
  Result:= WASI_ENOSYS;
end;

function TPas2JSWASIEnvironment.fd_fdstat_set_rights(fd, fsRightsBase, fsRightsInheriting: NativeInt): NativeInt;
begin
  console.log('Unimplemented: TPas2JSWASIEnvironment.fd_fdstat_set_rights');
  Result:= WASI_ENOSYS;
end;

function TPas2JSWASIEnvironment.getiovs(view: TJSDataView; iovs,
  iovsLen: NativeInt): TJSArray;

Var
  I : integer;
  ArrayBuf : TJSUint8Array;
  Ptr,Buf,BufLen : Integer;

begin
  Result:=TJSArray.New;
  For I:=0 to iovsLen-1 do
    begin
    ptr:=iovs + i * 8;
    buf:=view.getUint32(ptr, IsLittleEndian);
    bufLen:=view.getUint32(ptr + 4, IsLittleEndian);
    ArrayBuf:=TJSUint8Array.New(Memory.buffer, buf, bufLen);
    Result.Push(ArrayBuf);
    end;
end;

function TPas2JSWASIEnvironment.GetMemory: TJSWebassemblyMemory;
begin
  if Assigned(FMemory) then
    Result:=FMemory
  else
    Result:= FModuleInstanceExports.Memory;
end;

function TPas2JSWASIEnvironment.fd_write(fd, iovs, iovsLen, nwritten: NativeInt): NativeInt;

var
  view : TJSDataView;
  written : NativeInt;
  bufferBytes : TJSArray;
  Buffers : TJSArray;

  function writev(element : JSValue; index: NativeInt; anArray : TJSArray) : Boolean;

  var
    b : NativeInt;
    iov: TJSUint8Array absolute Element;

  begin
    For b:=0 to iov.byteLength-1 do
       bufferBytes.push(iov[b]);
    Inc(Written,iov.byteLength);
    Result:=true;
  end;

begin
  BufferBytes:=TJSArray.New;
  view:=getModuleMemoryDataView();
  written:=0;
  buffers:=getiovs(view, iovs, iovsLen);
  buffers.forEach(@writev);
  if (fd = WASI_STDOUT_FILENO) or (fd = WASI_STDERR_FILENO) then
    DoConsoleWrite((fd=WASI_STDERR_FILENO),BufferBytes);
  view.setUint32(nwritten, written, IsLittleEndian);
  Result:=WASI_ESUCCESS;
end;

function TPas2JSWASIEnvironment.fd_pwrite(fd, iovs, iovsLen, offset,
  nwritten: NativeInt): NativeInt;
begin
  console.log('Unimplemented: TPas2JSWASIEnvironment.fd_pwrite');
  Result:=WASI_ENOSYS;
end;

procedure TPas2JSWASIEnvironment.DoConsoleWrite(IsStdErr: Boolean;
  aBytes: TJSArray);

Var
  S : String;

begin
  asm
    // Result=String.fromCharCode.apply(null, new Uint16Array(a));
    S=String.fromCharCode.apply(null, aBytes);
  end;
  if IsStdErr then
    begin
    if Assigned(FOnStdErrorWrite) then
      FOnStdErrorWrite(Self,S)
    end
  else
    begin
    if Assigned(FOnStdOutputWrite) then
      FOnStdOutputWrite(Self,S)
    end
end;

function TPas2JSWASIEnvironment.clock_res_get (clockId, resolution : NativeInt) : NativeInt;
Var
  view: TJSDataView;

begin
  view:=getModuleMemoryDataView;
  setBigUint64(view,resolution, 0,IsLittleEndian);
  Result:=WASI_ESUCCESS;
end;

function TPas2JSWASIEnvironment.clock_time_get(clockId, precision, time: NativeInt) : NativeInt;

Var
  view: TJSDataView;
  n : NativeInt;
begin
  view:=getModuleMemoryDataView;
  n:=GetTime(clockId);
  if N=-1 then
    Result:=WASI_EINVAL
  else
    begin
    setBigUint64(view,time,n,IsLittleEndian);
    Result:=WASI_ESUCCESS;
    end;
end;

function TPas2JSWASIEnvironment.GetImportObject: TJSObject;

begin
  // We need this trick to be able to access self or this.
  // The webassembly callbacks get called without a this.
  if Not Assigned(FImportObject) then
    begin
    FImportObject:=TJSObject.New;
    GetImports(FImportObject);
    end;
  Result:=FImportObject;
end;

procedure TPas2JSWASIEnvironment.GetImports(aImports: TJSObject);

begin
  aImports['args_get']:=@args_get;
  aImports['args_sizes_get']:=@args_sizes_get;
  aImports['clock_res_get']:=@clock_res_get;
  aImports['clock_time_get']:=@clock_time_get;
  aImports['environ_get']:=@environ_get;
  aImports['environ_sizes_get']:=@environ_sizes_get;
  aImports['fd_advise']:=@fd_advise;
  aImports['fd_allocate']:=@fd_allocate;
  aImports['fd_close']:=@fd_close;
  aImports['fd_datasync']:=@fd_datasync;
  aImports['fd_fdstat_get']:=@fd_fdstat_get;
  aImports['fd_fdstat_set_flags']:=@fd_fdstat_set_flags;
  aImports['fd_fdstat_set_rights']:=@fd_fdstat_set_rights;
  aImports['fd_filestat_get']:=@fd_filestat_get;
  aImports['fd_filestat_set_size']:=@fd_filestat_set_size;
  aImports['fd_filestat_set_times']:=@fd_filestat_set_times;
  aImports['fd_pread']:=@fd_pread;
  aImports['fd_prestat_dir_name']:=@fd_prestat_dir_name;
  aImports['fd_prestat_get']:=@fd_prestat_get;
  aImports['fd_pwrite']:=@fd_pwrite;
  aImports['fd_read']:=@fd_read;
  aImports['fd_readdir']:=@fd_readdir;
  aImports['fd_renumber']:=@fd_renumber;
  aImports['fd_seek']:=@fd_seek;
  aImports['fd_sync']:=@fd_sync;
  aImports['fd_tell']:=@fd_tell;
  aImports['fd_write']:=@fd_write;
  aImports['path_create_directory']:=@path_create_directory;
  aImports['path_filestat_get']:=@path_filestat_get;
  aImports['path_filestat_set_times']:=@path_filestat_set_times;
  aImports['path_link']:=@path_link;
  aImports['path_open']:=@path_open;
  aImports['path_readlink']:=@path_readlink;
  aImports['path_remove_directory']:=@path_remove_directory;
  aImports['path_rename']:=@path_rename;
  aImports['path_symlink']:=@path_symlink;
  aImports['path_unlink_file']:=@path_unlink_file;
  aImports['poll_oneoff']:=@poll_oneoff;
  aImports['proc_exit']:=@proc_exit;
  aImports['proc_raise']:=@proc_raise;
  aImports['random_get']:=@random_get;
  aImports['sched_yield']:=@sched_yield;
  aImports['sock_recv']:=@sock_recv;
  aImports['sock_send']:=@sock_send;
  aImports['sock_shutdown']:=@sock_shutdown;
end;

function TPas2JSWASIEnvironment.poll_oneoff(sin, sout, nsubscriptions,
  nevents: NativeInt): NativeInt;
begin
  console.log('Unimplemented: TPas2JSWASIEnvironment.poll_oneoff');
  Result:= WASI_ENOSYS;
end;

function TPas2JSWASIEnvironment.proc_exit(rval: NativeInt): NativeInt;
begin
  FExitCode:=rval;
  Result:=WASI_ESUCCESS;
end;

function TPas2JSWASIEnvironment.proc_raise(sig: NativeInt): NativeInt;
begin
  console.log('Unimplemented: TPas2JSWASIEnvironment.proc_raise');
  Result:=WASI_ENOSYS;
end;

function TPas2JSWASIEnvironment.random_get(bufPtr, bufLen: NativeInt ): NativeInt;
var
  arr: TJSUint8Array;
  I : integer;
  View : TJSDataView;
begin
  arr:=TJSUint8Array.new(BufLen);

  crypto.getRandomValues(arr);

  view:=getModuleMemoryDataView;
  For I:=0 to arr.length-1 do
    view.setInt8(bufptr+i,arr[i]);
  Result:=WASI_ESUCCESS;
end;

function TPas2JSWASIEnvironment.sched_yield: NativeInt;
begin
  Result:=WASI_ESUCCESS;
end;

function TPas2JSWASIEnvironment.sock_recv: NativeInt;
begin
  console.log('Unimplemented: TPas2JSWASIEnvironment.sock_recv');
  Result:=WASI_ENOSYS;
end;

function TPas2JSWASIEnvironment.sock_send: NativeInt;
begin
  console.log('Unimplemented: TPas2JSWASIEnvironment.sock_recv');
  Result:=WASI_ENOSYS;
end;

function TPas2JSWASIEnvironment.sock_shutdown: NativeInt;
begin
  console.log('Unimplemented: TPas2JSWASIEnvironment.sock_shutdown');
  Result:=WASI_ENOSYS;
end;

procedure TPas2JSWASIEnvironment.SetMemory(aMemory: TJSWebAssemblyMemory);
begin
  FMemory:=aMemory;
end;

class constructor TPas2JSWASIEnvironment.init;
Var
  Opts : TJSTextDecoderOptions;

begin
  Opts:=TJSTextDecoderOptions.New;
  Opts.ignoreBOM:=true;
  Opts.fatal:=True;
  UTF8TextDecoder:=TJSTextDecoder.new ('utf-8',Opts);
end;

function TPas2JSWASIEnvironment.fd_advise(fd, offset, len, advice: NativeInt
  ): NativeInt;
begin
  console.log('Unimplemented: TPas2JSWASIEnvironment.fd_advise');
  Result:= WASI_ENOSYS;
end;

function TPas2JSWASIEnvironment.fd_allocate(fd, offset, len: NativeInt
  ): NativeInt;
begin
  console.log('Unimplemented: TPas2JSWASIEnvironment.fd_allocate');
  Result:= WASI_ENOSYS;
end;

function TPas2JSWASIEnvironment.fd_close(fd: NativeInt): NativeInt;
begin
  console.log('Unimplemented: TPas2JSWASIEnvironment.fd_close');
  Result:= WASI_ENOSYS;
end;

function TPas2JSWASIEnvironment.fd_datasync(fd: NativeInt): NativeInt;
begin
  console.log('Unimplemented: TPas2JSWASIEnvironment.fd_datasync');
  Result:= WASI_ENOSYS;
end;

function TPas2JSWASIEnvironment.fd_seek(fd, offset, whence,
  newOffsetPtr: NativeInt): NativeInt;
begin
  console.log('Unimplemented: TPas2JSWASIEnvironment.fd_seek');
  Result:= WASI_ENOSYS;
end;

function TPas2JSWASIEnvironment.fd_sync(fd: NativeInt): NativeInt;
begin
  console.log('Unimplemented: TPas2JSWASIEnvironment.fd_sync');
  Result:= WASI_ENOSYS;
end;

function TPas2JSWASIEnvironment.fd_pread(fd, iovs, iovsLen, offset,
  nread: NativeInt): NativeInt;
begin
  console.log('Unimplemented: TPas2JSWASIEnvironment.fd_pread');
  Result:= WASI_ENOSYS;
end;


function toUTF8Array(str : string) : TJSUint8Array;

Var
  Len,I,P : integer;
  charCode : NativeInt;

  procedure push (abyte : Byte);

  begin
    Result[P]:=aByte;
    inc(P);
  end;

begin
  Result:=TJSUint8Array.new(Length(str)*4);
  P:=0;
  Len:=Length(str);
  I:=1;
  While i<=Len do
    begin
    charcode:=Ord(Str[i]);
    if (charcode < $80) then
      push(charcode)
    else if (charcode < $800) then
      begin
      push($c0 or (charcode shr 6));
      push($80 or (charcode and $3f));
      end
    else if (charcode < $d800) or (charcode >= $e000) then
      begin
      push($e0 or (charcode shr 12));
      push($80 or ((charcode shr 6) and $3f));
      push($80 or (charcode and $3f));
      end
    else
      begin
      Inc(I);
      // UTF-16 encodes 0x10000-0x10FFFF by
      // subtracting 0x10000 and splitting the
      // 20 bits of 0x0-0xFFFFF into two halves
      charcode := $10000 + (((charcode and $3ff) shl 10) or (Ord(Str[i]) and $3ff));
      push($f0 or (charcode shr 18));
      push($80 or ((charcode shr 12) and $3f));
      push($80 or ((charcode shr 6) and $3f));
      push($80 or (charcode and $3f));
      end;
    inc(I);
    end;
  Result:=TJSUint8Array(Result.slice(0,p));
end;

function TPas2JSWASIEnvironment.fd_read(fd, iovs, iovsLen, nread: NativeInt
  ): NativeInt;
var
  view : TJSDataView;
  bytesRead : NativeInt;
  bufferBytes : TJSUint8Array;
  Buffers : TJSArray;

  function readv(element : JSValue; index: NativeInt; anArray : TJSArray) : Boolean;

  var
    b : NativeInt;
    iov: TJSUint8Array absolute Element;

  begin
    b:=0;
    While (B<iov.byteLength) and (BytesRead<BufferBytes.Length) do
      begin
      iov[b]:=BufferBytes[BytesRead];
      inc(b);
      inc(BytesRead);
      end;
    Result:=true;
  end;

begin
  bytesRead:=0;
  view:=getModuleMemoryDataView();
  if (fd = WASI_STDIN_FILENO) then
    begin
    BufferBytes:=GetConsoleInputBuffer
    end
  else
    BufferBytes:=GetFileBuffer(FD);
  if BufferBytes.length>0 then
    begin
    buffers:=getiovs(view, iovs, iovsLen);
    buffers.forEach(@readv);
    end;
  view.setUint32(nread, bytesRead, IsLittleEndian);
  Result:=WASI_ESUCCESS;
end;

function TPas2JSWASIEnvironment.GetFileBuffer(FD : NativeInt): TJSUint8Array;

begin
  Result:=TJSUint8Array.new(0);
end;

function TPas2JSWASIEnvironment.GetConsoleInputBuffer : TJSUint8Array;

Var
  S : String;

begin
  Result:=Nil;
  If Assigned(OnGetConsoleInputBuffer) then
    OnGetConsoleInputBuffer(Self,Result)
  else If Assigned(OnGetConsoleInputString) then
    begin
    S:='';
    OnGetConsoleInputString(Self,S);
    Result:=toUTF8Array(S);
    end
  else
    Result:=TJSUint8Array.New(0);
end;

function TPas2JSWASIEnvironment.fd_readdir(fd, bufPtr, bufLen, cookie,
  bufusedPtr: NativeInt): NativeInt;
begin
  console.log('Unimplemented: TPas2JSWASIEnvironment.fd_readdir');
  Result:= WASI_ENOSYS;
end;

function TPas2JSWASIEnvironment.fd_renumber(afrom, ato: NativeInt): NativeInt;
begin
  console.log('Unimplemented: TPas2JSWASIEnvironment.fd_renumber');
  Result:= WASI_ENOSYS;
end;

function TPas2JSWASIEnvironment.fd_tell(fd, offsetPtr: NativeInt): NativeInt;
begin
  console.log('Unimplemented: TPas2JSWASIEnvironment.fd_tell');
  Result:= WASI_ENOSYS;
end;

function TPas2JSWASIEnvironment.fd_filestat_get(fd, bufPtr: NativeInt
  ): NativeInt;
begin
  console.log('Unimplemented: TPas2JSWASIEnvironment.fd_filestat_get');
  Result:= WASI_ENOSYS;
end;

function TPas2JSWASIEnvironment.fd_filestat_set_size(fd, stSize: NativeInt
  ): NativeInt;
begin
  console.log('Unimplemented: TPas2JSWASIEnvironment.fd_filestat_set_size');
  Result:= WASI_ENOSYS;
end;

function TPas2JSWASIEnvironment.fd_filestat_set_times(fd, stAtim, stMtim,
  fstflags: NativeInt): NativeInt;
begin
  console.log('Unimplemented: TPas2JSWASIEnvironment.fd_filestat_set_times');
  Result:= WASI_ENOSYS;
end;

function TPas2JSWASIEnvironment.path_readlink(fd, pathPtr, pathLen, buf,
  bufLen, bufused: NativeInt): NativeInt;
begin
  console.log('Unimplemented: TPas2JSWASIEnvironment.path_readlink');
  Result:= WASI_ENOSYS;
end;

function TPas2JSWASIEnvironment.path_create_directory(fd, pathPtr,
  pathLen: NativeInt): NativeInt;
begin
  console.log('Unimplemented: TPas2JSWASIEnvironment.path_create_directory');
  Result:= WASI_ENOSYS;
end;

function TPas2JSWASIEnvironment.path_filestat_get(fd, flags, pathPtr, pathLen,
  bufPtr: NativeInt): NativeInt;
begin
  console.log('Unimplemented: TPas2JSWASIEnvironment.path_filestat_get');
  Result:= WASI_ENOSYS;
end;

function TPas2JSWASIEnvironment.path_link(oldFd, oldFlags, oldPath, oldPathLen,
  newFd, newPath, newPathLen: NativeInt): NativeInt;
begin
  console.log('Unimplemented: TPas2JSWASIEnvironment.path_link');
  Result:= WASI_ENOSYS;
end;

function TPas2JSWASIEnvironment.path_remove_directory(fd, pathPtr,
  pathLen: NativeInt): NativeInt;
begin
  console.log('Unimplemented: TPas2JSWASIEnvironment.path_remove_directory');
  Result:= WASI_ENOSYS;
end;

function TPas2JSWASIEnvironment.path_rename(oldFd, oldPath, oldPathLen, newFd,
  newPath, newPathLen: NativeInt): NativeInt;
begin
  console.log('Unimplemented: TPas2JSWASIEnvironment.path_rename');
  Result:= WASI_ENOSYS;
end;

function TPas2JSWASIEnvironment.path_symlink(oldPath, oldPathLen, fd, newPath,
  newPathLen: NativeInt): NativeInt;
begin
  console.log('Unimplemented: TPas2JSWASIEnvironment.path_symlink');
  Result:= WASI_ENOSYS;
end;

function TPas2JSWASIEnvironment.path_unlink_file(fd, pathPtr, pathLen: NativeInt
  ): NativeInt;
begin
  console.log('Unimplemented: TPas2JSWASIEnvironment.path_unlink_file');
  Result:= WASI_ENOSYS;
end;

function TPas2JSWASIEnvironment.path_open(dirfd, dirflags, pathPtr, pathLen,
  oflags, fsRightsBase, fsRightsInheriting, fsFlags, fd: NativeInt): NativeInt;
begin
  console.log('Unimplemented: TPas2JSWASIEnvironment.path_open');
  Result:= WASI_ENOSYS;
end;

function TPas2JSWASIEnvironment.path_filestat_set_times(fd, fstflags, pathPtr,
  pathLen, stAtim, stMtim: NativeInt): NativeInt;
begin
  console.log('Unimplemented: TPas2JSWASIEnvironment.path_filestat_set_times');
  Result:= WASI_ENOSYS;
end;

constructor TPas2JSWASIEnvironment.Create;
begin
  FIsLittleEndian:=True;
  // Default expected by FPC runtime
  WASIImportName:='wasi_snapshot_preview1';
end;

destructor TPas2JSWASIEnvironment.Destroy;
begin
  FreeAndNil(FImportExtensions);
  inherited Destroy;
end;


function TPas2JSWASIEnvironment.GetUTF8StringFromMem(aLoc, aLen: Longint): String;
begin
  Result:=UTF8TextDecoder.Decode(getModuleMemoryDataView.buffer.slice(aLoc,aLoc+alen));
end;

procedure TPas2JSWASIEnvironment.SetMemInfoInt8(aLoc: Integer; aValue: ShortInt
  );

Var
  View : TJSDataView;

begin
  view:=getModuleMemoryDataView();
  view.setint8(aLoc,aValue);
end;

procedure TPas2JSWASIEnvironment.SetMemInfoInt16(aLoc: Integer; aValue: SmallInt);

Var
  View : TJSDataView;

begin
  view:=getModuleMemoryDataView();
  view.setint16(aLoc,aValue, IsLittleEndian);
end;

procedure TPas2JSWASIEnvironment.SetMemInfoInt32(aLoc: Integer; aValue: Longint);

Var
  View : TJSDataView;

begin
  view:=getModuleMemoryDataView();
  view.setInt32(aLoc,aValue,IsLittleEndian);
end;

procedure TPas2JSWASIEnvironment.SetMemInfoInt64(aLoc: Integer; aValue: NativeInt);

Var
  View : TJSDataView;

begin
  view:=getModuleMemoryDataView();
  setBigInt64(View,aLoc,aValue,IsLittleEndian);
end;

procedure TPas2JSWASIEnvironment.SetMemInfoUInt8(aLoc: Integer; aValue: Byte);
Var
  View : TJSDataView;

begin
  view:=getModuleMemoryDataView();
  view.setUInt8(aLoc,aValue);
end;

procedure TPas2JSWASIEnvironment.SetMemInfoUInt16(aLoc: Integer; aValue: Word);
Var
  View : TJSDataView;

begin
  view:=getModuleMemoryDataView();
  view.setUint16(aLoc,aValue,IsLittleEndian);
end;

procedure TPas2JSWASIEnvironment.SetMemInfoUInt32(aLoc: Integer;
  aValue: Cardinal);
Var
  View : TJSDataView;

begin
  view:=getModuleMemoryDataView();
  view.setUint32(aLoc,aValue,IsLittleEndian);
end;

procedure TPas2JSWASIEnvironment.SetMemInfoUInt64(aLoc: Integer;
  aValue: NativeUint);
Var
  View : TJSDataView;

begin
  view:=getModuleMemoryDataView();
  setBigUint64(View,aLoc,aValue,IsLittleEndian);
end;

initialization

end.

