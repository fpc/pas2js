program browsertest;

uses
  SysUtils, TypInfo,BrowserTestRunner, demotests, frmrunform;

Var
  Application : TTestRunner;

Type

  { TMyMethod }
{$M+}
  TMyMethod = Class(TObject)
  Public
    Class Function MyMethodAddress (aName : String) : Pointer;
  Published
    Procedure DoIt;
  end;

  { TMyMethod2 }

  TMyMethod2 = Class(TMyMethod)
  Published
    Procedure DoIt2;
  end;

{ TMyMethod2 }

procedure TMyMethod2.DoIt2;
begin
  Writeln('OK 2');
end;

{ TMyMethod }

class function TMyMethod.MyMethodAddress(aName: String): Pointer;

Var
  i : integer;
  TI : TTypeInfoClass;
  N,MN : String;

begin
  Result:=nil;
  N:=LowerCase(aName);
  TI:=TypeInfo(Self);
  MN:='';
  While (MN='') and Assigned(TI) do
    begin
    I:=0;
    While (MN='') and (I<TI.MethodCount) do
      begin
      If TI.GetMethod(i).Name=aName then
        MN:=TI.GetMethod(i).Name;
      Inc(I);
      end;
    if MN='' then
      TI:=TI.Ancestor;
    end;
  if MN<>'' then
    asm
    return this[MN];
    end;
end;

procedure TMyMethod.DoIt;
begin
  Writeln('Doit 1');
end;

Var
  A : TMyMethod;
  B : TMyMethod2;

begin
  A:=TMyMethod.Create();
  A.Doit;
  B:=TMyMethod2.Create();
  B.Doit2;

  Writeln('Doit A',A.MyMethodAddress('doit')<>Nil);
  Writeln('Doit B',B.MyMethodAddress('doit')<>Nil);
  Writeln('Doit2 A',A.MyMethodAddress('doit2')<>Nil);
  Writeln('Doit2 B',B.MyMethodAddress('doit2')<>Nil);
{  Application:=TTestRunner.Create(Nil);
  Application.RunFormClass:=TTestRunForm;
  Application.Initialize;
  Application.Run;
  Application.Free;}
end.

