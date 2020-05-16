program errordemo;

{$mode objfpc}

uses
  BrowserConsole, JS, Classes, SysUtils, Web;

function DoRaise(aEvent : TJSMouseEvent) : boolean;

begin
  Result:=False;
  raise exception.Create('A exception');
end;

function DoHook(aEvent : TJSMouseEvent) : boolean;

begin
  Result:=False;
  HookUncaughtExceptions;
end;

Procedure DoPascalException(O : TObject);

begin
  Writeln('O :',O.ClassName);
  if O is Exception then
    Writeln('Exception class message : ',Exception(O).Message);
end;

Procedure DoJSException(O : TJSObject);
begin
  writeln('Javascript exception: ',O.toString);
  if O is TJSError then
    Writeln('Error message : ',TJSError(O).Message);
end;

Procedure DoRaiseJS; assembler;
asm
  throw new Object();
end;

Procedure DoRaiseJSError; assembler;
asm
  var e = new Error();
  e.message="My error message";
  throw e;
end;

begin
  // This will only work for the main program if you have set showUncaughtExceptions before rtl.run();
  TJSHtmlButtonElement(Document.getElementById('btnhook')).OnClick:=@DoHook;
  // These will not be caught (yet)
  TJSHtmlButtonElement(Document.getElementById('btn')).OnClick:=@DoRaise;
  // Uncomment this to set default exception handlers
  // HookUncaughtExceptions;

  //  Uncomment these to set special exception handlers
  //  SetOnUnCaughtExceptionHandler(@DoPascalException);
  //  SetOnUnCaughtExceptionHandler(@DoJSException);

  // Various ways to raise an exception.
  // DoRaiseJS;
  // DoRaiseJSError;

  DoRaise(Nil);
end.
