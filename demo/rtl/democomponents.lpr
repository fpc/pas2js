program democomponents;

uses browserconsole, Classes;

Type
   TMyGeneration = (first,second,third);

   { TMyParentComponent }

   TMyParentComponent = Class(TComponent)
   private
     FMyProperty: TMyGeneration;
   Published
     Property MyProperty : TMyGeneration Read FMyProperty Write FMyProperty;
   end;

   { TMyChildComponent }

   TMyChildComponent = Class(TMyParentComponent)
   Public
     destructor Destroy; override;
   end;

Var
  DestroyCount : Integer;

{ TMyChildComponent }

destructor TMyChildComponent.Destroy;
begin
  DestroyCount:=DestroyCount+1;
  Writeln('Destroying child "',Name,'", current count : ',DestroyCount);
  inherited Destroy;
end;

Var
  P : TMyParentComponent;
  C : TMyChildComponent;

begin
  P:=TMyParentComponent.Create(Nil);
  try
    P.Name:='Parent1';
    P.MyProperty:=First;
    C:=TMyChildComponent.Create(P);
    C.Name:='Child1';
    C.MyProperty:=Second;
    C:=TMyChildComponent.Create(C);
    C.Name:='Child2';
    C.MyProperty:=Third;
    C:=TMyChildComponent.Create(P);
    C.Name:='Child3';
    C.MyProperty:=Second;
  finally
    P.Destroy;
  end;
end.

