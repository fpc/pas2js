program threadapp;
{$mode objfpc}
{$h+}
{$i-}

uses SysUtils, Classes;

Function Fibonacci(N : Integer) : Int64;

Var
  Next,Last : Int64;
  I : Integer;

begin
  if N=0 then
    exit(0);
  Result:=1;
  Last:=0;
  for I:=1 to N-1 do
    begin
    Next:=Result+last;
    Last:=Result;
    Result:=Next;
    end;
end;

Type
  { TCalcThread }
  TCalcThread = Class(TThread)
    Procedure Execute; override;
  end;

{ TCalcThread }

procedure TCalcThread.Execute;
begin
  FreeOnTerminate:=True;
  DebugWriteln('Fibonacci(10) = '+IntToStr(Fibonacci(10)));
end;

begin
  DebugWriteln('Starting thread');
  With TCalcThread.Create(False) do
    begin
    DebugWriteln('Thread created');
    WaitFor;
    DebugWriteln('thread ended');
    end;
end.

