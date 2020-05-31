unit tcgenericdictionary;

{$mode objfpc}

interface

uses
  fpcunit, testregistry, Classes, SysUtils, Generics.Defaults, Generics.Collections;

Type
  TMySimpleDict = Class(Specialize TDictionary<Integer,String>);

  { TTestSimpleDictionary }

  TTestSimpleDictionary = Class(TTestCase)
  Private
    FDict : TMySimpleDict;
    procedure DoAdd(aCount: Integer; aOffset: Integer=0);
    procedure DoAdd2;
    procedure DoGetValue(aKey: Integer; Match: String; ExceptionClass: TClass=nil);
  Public
    Procedure SetUp; override;
    Procedure TearDown; override;
    Property Dict : TMySimpleDict Read FDict;
  Published
    Procedure TestEmpty;
    Procedure TestAdd;
    Procedure TestClear;
    Procedure TestTryGetValue;
    Procedure TestGetValue;
    Procedure TestSetValue;
    Procedure TestAddDuplicate;
    Procedure TestAddOrSet;
    Procedure TestContainsKey;
    Procedure TestDelete;
  end;

implementation

{ TTestSimpleDictionary }

procedure TTestSimpleDictionary.SetUp;
begin
  inherited SetUp;
  FDict:=TMySimpleDict.Create;
end;

procedure TTestSimpleDictionary.TearDown;
begin
  FreeAndNil(FDict);
  inherited TearDown;
end;

procedure TTestSimpleDictionary.TestEmpty;
begin
  AssertNotNull('Have dictionary',Dict);
  AssertEquals('empty dictionary',0,Dict.Count);
end;

procedure TTestSimpleDictionary.DoAdd(aCount : Integer; aOffset : Integer=0);

Var
  I : Integer;

begin
  if aOffset=-1 then
    aOffset:=Dict.Count;
  For I:=aOffset+1 to aOffset+aCount do
    Dict.Add(I,IntToStr(i));
end;

procedure TTestSimpleDictionary.TestAdd;

begin
  DoAdd(1);
  AssertEquals('Count OK',1,Dict.Count);
  AssertTrue('Has added value',Dict.ContainsKey(1));
  DoAdd(1,1);
  AssertEquals('Count OK',2,Dict.Count);
  AssertTrue('Has added value',Dict.ContainsKey(2));
end;

procedure TTestSimpleDictionary.TestClear;
begin
  DoAdd(3);
  AssertEquals('Count OK',3,Dict.Count);
  Dict.Clear;
  AssertEquals('Count after clear OK',0,Dict.Count);
end;

procedure TTestSimpleDictionary.TestTryGetValue;

Var
  I : integer;
  SI,A : string;

begin
  DoAdd(3);
  For I:=1 to 3 do
    begin
    SI:=IntToStr(I);
    AssertTrue('Have value '+SI,Dict.TryGetValue(I,A));
    AssertEquals('Value is correct '+SI,SI,A);
    end;
  AssertFalse('Have no value 4',Dict.TryGetValue(4,A));
end;

procedure TTestSimpleDictionary.DoGetValue(aKey: Integer; Match: String; ExceptionClass: TClass);

Var
  EC : TClass;
  A,EM : String;

begin
  EC:=Nil;
  try
    A:=Dict.Items[aKey];
  except
    On E : Exception do
      begin
      EC:=E.ClassType;
      EM:=E.Message;
      end
  end;
  if ExceptionClass=Nil then
    begin
    if EC<>Nil then
      Fail('Got exception '+EC.ClassName+' with message: '+EM);
    AssertEquals('Value is correct for '+IntToStr(aKey),Match,A)
    end
  else
    begin
    if EC=Nil then
      Fail('Expected exception '+ExceptionClass.ClassName+' but got none');
    if EC<>ExceptionClass then
      Fail('Expected exception class '+ExceptionClass.ClassName+' but got '+EC.ClassName+' with message '+EM);
    end;
end;

procedure TTestSimpleDictionary.TestGetValue;

Var
  I : integer;

begin
  DoAdd(3);
  For I:=1 to 3 do
    DoGetValue(I,IntToStr(I));
  DoGetValue(4,'4',EDictionary);
end;

procedure TTestSimpleDictionary.TestSetValue;
begin
  TestGetValue;
  Dict.Items[3]:='Six';
  DoGetValue(3,'Six');
end;

procedure TTestSimpleDictionary.DoAdd2;

begin
  Dict.Add(2,'A new 2');
end;

procedure TTestSimpleDictionary.TestAddDuplicate;
begin
  DoAdd(3);
  AssertException('Cannot add duplicate',EDictionary,@DoAdd2);
end;

procedure TTestSimpleDictionary.TestAddOrSet;

begin
  DoAdd(3);
  Dict.AddOrSetValue(2,'a new 2');
  DoGetValue(2,'a new 2');
end;

procedure TTestSimpleDictionary.TestContainsKey;

Var
  I : Integer;

begin
  DoAdd(3);
  For I:=1 to 3 do
    AssertTrue('Has '+IntToStr(i),Dict.ContainsKey(I));
  AssertFalse('Has not 4',Dict.ContainsKey(4));
end;

procedure TTestSimpleDictionary.TestDelete;
begin
  DoAdd(3);
  Dict.Remove(2);
  AssertEquals('Count',2,Dict.Count);
  AssertFalse('Has not 2',Dict.ContainsKey(2));
end;

begin
  RegisterTest(TTestSimpleDictionary);
end.

