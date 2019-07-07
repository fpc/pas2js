Unit tccompstreaming;

interface

Uses
  SysUtils, Classes, tcstreaming, fpcunit, testregistry, testcomps;

Type

  { TTestComponentStream }

  TTestComponentStream = Class(TTestStreaming)
  Published
      Procedure TestTEmptyComponent;
      Procedure TestTIntegerComponent;
      Procedure TestTIntegerComponent2;
      Procedure TestTIntegerComponent3;
      Procedure TestTIntegerComponent4;
      Procedure TestTIntegerComponent5;
      Procedure TestTInt64Component;
      Procedure TestTInt64Component2;
      Procedure TestTInt64Component3;
      Procedure TestTInt64Component4;
      Procedure TestTInt64Component5;
      Procedure TestTInt64Component6;
      Procedure TestTStringComponent;
      Procedure TestTStringComponent2;
      Procedure TestTWideStringComponent;
      Procedure TestTWideStringComponent2;
      Procedure TestTSingleComponent;
      Procedure TestTDoubleComponent;
      Procedure TestTExtendedComponent;
//      Procedure TestTCompComponent;
      Procedure TestTCurrencyComponent;
      Procedure TestTDateTimeComponent;
      Procedure TestTDateTimeComponent2;
      Procedure TestTDateTimeComponent3;
      Procedure TestTEnumComponent;
      Procedure TestTEnumComponent2;
      Procedure TestTEnumComponent3;
      Procedure TestTEnumComponent4;
      Procedure TestTEnumComponent5;
      Procedure TestTSetComponent;
      Procedure TestTSetComponent2;
      Procedure TestTSetComponent3;
      Procedure TestTSetComponent4;
      Procedure TestTMultipleComponent;
      Procedure TestTPersistentComponent;
      Procedure TestTCollectionComponent;
      Procedure TestTCollectionComponent2;
      Procedure TestTCollectionComponent3;
      Procedure TestTCollectionComponent4;
      Procedure TestTCollectionComponent5;
      Procedure TestTOwnedComponent;
      Procedure TestTStreamedOwnedComponent;
      Procedure TestTStreamedOwnedComponents;
      Procedure TestTMethodComponent;
      Procedure TestTMethodComponent2;
      // Read
      Procedure TestTEmptyComponentRead;
      Procedure TestTIntegerComponentRead;
      Procedure TestTIntegerComponent2Read;
      Procedure TestTIntegerComponent3Read;
      Procedure TestTIntegerComponent4Read;
      Procedure TestTIntegerComponent5Read;
      Procedure TestTInt64ComponentRead;
      Procedure TestTInt64Component2Read;
      Procedure TestTInt64Component3Read;
      Procedure TestTInt64Component4Read;
      Procedure TestTInt64Component5Read;
      Procedure TestTInt64Component6Read;
      Procedure TestTStringComponentRead;
      Procedure TestTStringComponent2Read;
      Procedure TestTWideStringComponentRead;
      Procedure TestTWideStringComponent2Read;
      Procedure TestTSingleComponentRead;
      Procedure TestTDoubleComponentRead;
      Procedure TestTExtendedComponentRead;
//      Procedure TestTCompComponent;
      Procedure TestTCurrencyComponentRead;
      Procedure TestTDateTimeComponentRead;
      Procedure TestTDateTimeComponent2Read;
      Procedure TestTDateTimeComponent3Read;
      Procedure TestTEnumComponentRead;
      Procedure TestTEnumComponent2Read;
      Procedure TestTEnumComponent3Read;
      Procedure TestTEnumComponent4Read;
      Procedure TestTEnumComponent5Read;
      Procedure TestTSetComponentRead;
      Procedure TestTSetComponent2Read;
      Procedure TestTSetComponent3Read;
      Procedure TestTSetComponent4Read;
      Procedure TestTMultipleComponentRead;
      Procedure TestTPersistentComponentRead;
      Procedure TestTCollectionComponentRead;
      Procedure TestTCollectionComponent2Read;
      Procedure TestTCollectionComponent3Read;
      Procedure TestTCollectionComponent4Read;
      Procedure TestTCollectionComponent5Read;
      Procedure TestTOwnedComponentRead;
      Procedure TestTStreamedOwnedComponentRead;
      Procedure TestTStreamedOwnedComponentsRead;
    end;


  { TTestCollectionStream }

  TTestCollectionStream = Class(TTestCase)
  private
    procedure CompareColl(CA, CB: TMyColl);
    function CreateColl(Anr: Integer): TCollComp;
    function EmptyComp: TCollComp;
    procedure TestNr(ACount: Integer);
  Published
    procedure Test1;
    procedure Test2;
    procedure Test3;
    procedure TestClear;
    procedure TestEmpty;
  end;

Implementation

Procedure TTestComponentStream.TestTEmptyComponent;

Var
  C : TComponent;

begin
  C:=TEmptyComponent.Create(Nil);
  Try
    SaveToStream(C);
    ExpectSignature;
    ExpectFlags([],0);
    ExpectBareString('TEmptyComponent');
    ExpectBareString('TestTEmptyComponent');
    ExpectEndOfList;
    ExpectEndOfList;
  Finally
    C.Free;
  end;
end;

procedure TTestComponentStream.TestTEmptyComponentRead;

Var
  C : TEmptyComponent;
begin
  TestTEmptyComponent;
  C:=TEmptyComponent.Create(Nil);
  try
    LoadFromStream(C);
    AssertEquals('Name','TestTEmptyComponent',C.Name);
  finally
    C.Free;
  end;
end;

Procedure TTestComponentStream.TestTIntegerComponent;

Var
  C : TComponent;

begin
  C:=TIntegerComponent.Create(Nil);
  Try
    SaveToStream(C);
    ExpectSignature;
    ExpectFlags([],0);
    ExpectBareString('TIntegerComponent');
    ExpectBareString('TestTIntegerComponent');
    ExpectBareString('IntProp');
    ExpectInteger(3);
    ExpectEndOfList;
    ExpectEndOfList;
  Finally
    C.Free;
  end;
end;

procedure TTestComponentStream.TestTIntegerComponentRead;

Var
  C : TIntegerComponent;

begin
  TestTIntegerComponent;
  C:=TIntegerComponent.Create(Nil);
  Try
    LoadFromStream(C);
    AssertEquals('Name','TestTIntegerComponent',C.Name);
    AssertEquals('IntProp',3,C.IntProp);
  Finally
    C.Free;
  end;
end;

procedure TTestComponentStream.TestTIntegerComponent2Read;

Var
  C : TIntegerComponent2;

begin
  TestTIntegerComponent2;
  C:=TIntegerComponent2.Create(Nil);
  Try
    LoadFromStream(C);
    AssertEquals('Name','TestTIntegerComponent2',C.Name);
    AssertEquals('IntProp',1024,C.IntProp);
  Finally
    C.Free;
  end;
end;

procedure TTestComponentStream.TestTIntegerComponent3Read;
Var
  C : TIntegerComponent3;

begin
  TestTIntegerComponent3;
  C:=TIntegerComponent3.Create(Nil);
  Try
    LoadFromStream(C);
    AssertEquals('Name','TestTIntegerComponent3',C.Name);
    AssertEquals('IntProp',262144,C.IntProp);
  Finally
    C.Free;
  end;
end;

procedure TTestComponentStream.TestTIntegerComponent4Read;

Var
  C : TIntegerComponent4;

begin
  TestTIntegerComponent4;
  C:=TIntegerComponent4.Create(Nil);
  Try
    LoadFromStream(C);
    AssertEquals('Name','TestTIntegerComponent4',C.Name);
    AssertEquals('IntProp',6,C.IntProp);
  Finally
    C.Free;
  end;
end;

procedure TTestComponentStream.TestTIntegerComponent5Read;

Var
  C : TIntegerComponent5;

begin
  TestTIntegerComponent5;
  C:=TIntegerComponent5.Create(Nil);
  Try
    LoadFromStream(C);
    AssertEquals('Name','TestTIntegerComponent5',C.Name);
    AssertEquals('IntProp',5,C.IntProp);
  Finally
    C.Free;
  end;
end;

procedure TTestComponentStream.TestTInt64ComponentRead;

Var
  C : TInt64Component;

begin
  TestTInt64Component;
  C:=TInt64Component.Create(Nil);
  Try
    C.Int64Prop:=0;
    LoadFromStream(C);
    AssertEquals('Name','TestTInt64Component',C.Name);
    AssertEquals('Int64Prop',4,C.Int64Prop);
  Finally
    C.Free;
  end;
end;

procedure TTestComponentStream.TestTInt64Component2Read;

Var
  C : TInt64Component2;

begin
  TestTInt64Component2;
  C:=TInt64Component2.Create(Nil);
  Try
    C.Int64Prop:=0;
    LoadFromStream(C);
    AssertEquals('Name','TestTInt64Component2',C.Name);
    AssertEquals('Int64Prop',2 shl 9,C.Int64Prop);
  Finally
    C.Free;
  end;
end;

procedure TTestComponentStream.TestTInt64Component3Read;

Var
  C : TInt64Component3;

begin
  TestTInt64Component3;
  C:=TInt64Component3.Create(Nil);
  Try
    C.Int64Prop:=0;
    LoadFromStream(C);
    AssertEquals('Name','TestTInt64Component3',C.Name);
    AssertEquals('Int64Prop',2 shl 17,C.Int64Prop);
  Finally
    C.Free;
  end;
end;

procedure TTestComponentStream.TestTInt64Component4Read;

Var
  C : TInt64Component4;

begin
  TestTInt64Component4;
  C:=TInt64Component4.Create(Nil);
  Try
    C.Int64Prop:=0;
    LoadFromStream(C);
    AssertEquals('Name','TestTInt64Component4',C.Name);
    AssertEquals('Int64Prop',NativeInt(MaxInt)+NativeInt(2 shl 14),C.Int64Prop);
  Finally
    C.Free;
  end;
end;

procedure TTestComponentStream.TestTInt64Component5Read;

Var
  C : TInt64Component5;

begin
  TestTInt64Component5;
  C:=TInt64Component5.Create(Nil);
  Try
    C.Int64Prop:=0;
    LoadFromStream(C);
    AssertEquals('Name','TestTInt64Component5',C.Name);
    // Not written, so zero remains
    AssertEquals('Int64Prop',0,C.Int64Prop);
  Finally
    C.Free;
  end;
end;

procedure TTestComponentStream.TestTInt64Component6Read;
Var
  C : TInt64Component6;

begin
  TestTInt64Component6;
  C:=TInt64Component6.Create(Nil);
  Try
    C.Int64Prop:=0;
    LoadFromStream(C);
    AssertEquals('Name','TestTInt64Component6',C.Name);
    AssertEquals('Int64Prop',8,C.Int64Prop);
  Finally
    C.Free;
  end;
end;

procedure TTestComponentStream.TestTStringComponentRead;

Var
  C : TStringComponent;

begin
  TestTStringComponent;
  C:=TStringComponent.Create(Nil);
  Try
    C.StringProp:='';
    LoadFromStream(C);
    AssertEquals('Name','TestTStringComponent',C.Name);
    AssertEquals('StringProp','A string',C.StringProp);
  Finally
    C.Free;
  end;
end;

procedure TTestComponentStream.TestTStringComponent2Read;

Var
  C : TStringComponent2;

begin
  TestTStringComponent2;
  C:=TStringComponent2.Create(Nil);
  Try
    C.StringProp:='abc';
    LoadFromStream(C);
    AssertEquals('Name','TestTStringComponent2',C.Name);
    AssertEquals('StringProp','abc',C.StringProp);
  Finally
    C.Free;
  end;
end;

procedure TTestComponentStream.TestTWideStringComponentRead;

Var
  C : TWideStringComponent;

begin
  TestTWideStringComponent;
  C:=TWideStringComponent.Create(Nil);
  Try
    C.WideStringProp:='abc';
    LoadFromStream(C);
    AssertEquals('Name','TestTWideStringComponent',C.Name);
    AssertEquals('WideStringProp','Some WideString',C.WideStringProp);
  Finally
    C.Free;
  end;
end;

procedure TTestComponentStream.TestTWideStringComponent2Read;
Var
  C : TWideStringComponent2;

begin
  TestTWideStringComponent2;
  C:=TWideStringComponent2.Create(Nil);
  Try
    C.WideStringProp:='abc';
    LoadFromStream(C);
    AssertEquals('Name','TestTWideStringComponent2',C.Name);
    AssertEquals('WideStringProp','abc',C.WideStringProp);
  Finally
    C.Free;
  end;
end;

procedure TTestComponentStream.TestTSingleComponentRead;

Var
  C : TSingleComponent;

begin
  TestTSingleComponent;
  C:=TSingleComponent.Create(Nil);
  Try
    C.SingleProp:=0;
    LoadFromStream(C);
    AssertEquals('Name','TestTSingleComponent',C.Name);
    AssertEquals('SingleProp',1.23,C.SingleProp,0.01);
  Finally
    C.Free;
  end;
end;

procedure TTestComponentStream.TestTDoubleComponentRead;

Var
  C : TDoubleComponent;

begin
  TestTDoubleComponent;
  C:=TDoubleComponent.Create(Nil);
  Try
    C.DoubleProp:=0;
    LoadFromStream(C);
    AssertEquals('Name','TestTDoubleComponent',C.Name);
    AssertEquals('DoubleProp',2.34,C.DoubleProp,0.01);
  Finally
    C.Free;
  end;
end;

procedure TTestComponentStream.TestTExtendedComponentRead;

Var
  C : TExtendedComponent;

begin
  TestTExtendedComponent;
  C:=TExtendedComponent.Create(Nil);
  Try
    C.ExtendedProp:=0;
    LoadFromStream(C);
    AssertEquals('Name','TestTExtendedComponent',C.Name);
    AssertEquals('ExtendedProp',3.45,C.ExtendedProp,0.01);
  Finally
    C.Free;
  end;
end;

procedure TTestComponentStream.TestTCurrencyComponentRead;

Var
  C : TCurrencyComponent;

begin
  TestTCurrencyComponent;
  C:=TCurrencyComponent.Create(Nil);
  Try
    C.CurrencyProp:=0;
    LoadFromStream(C);
    AssertEquals('Name','TestTCurrencyComponent',C.Name);
    AssertEquals('CurrencyProp',5.67,C.CurrencyProp,0.01);
  Finally
    C.Free;
  end;
end;

procedure TTestComponentStream.TestTDateTimeComponentRead;

Var
  C : TDateTimeComponent;

begin
  TestTDateTimeComponent;
  C:=TDateTimeComponent.Create(Nil);
  Try
    C.DateTimeProp:=0;
    LoadFromStream(C);
    AssertEquals('Name','TestTDateTimeComponent',C.Name);
    AssertEquals('DateTimeProp',35278.00,C.DateTimeProp,0.01);
  Finally
    C.Free;
  end;
end;

procedure TTestComponentStream.TestTDateTimeComponent2Read;

Var
  C : TDateTimeComponent2;

begin
  TestTDateTimeComponent2;
  C:=TDateTimeComponent2.Create(Nil);
  Try
    C.DateTimeProp:=0;
    LoadFromStream(C);
    AssertEquals('Name','TestTDateTimeComponent2',C.Name);
    AssertEquals('DateTimeProp',0.97,C.DateTimeProp,0.01);
  Finally
    C.Free;
  end;
end;

procedure TTestComponentStream.TestTDateTimeComponent3Read;
Var
  C : TDateTimeComponent3;

begin
  TestTDateTimeComponent3;
  C:=TDateTimeComponent3.Create(Nil);
  Try
    C.DateTimeProp:=0;
    LoadFromStream(C);
    AssertEquals('Name','TestTDateTimeComponent3',C.Name);
    AssertEquals('DateTimeProp',35278.97,C.DateTimeProp,0.01);
  Finally
    C.Free;
  end;
end;

procedure TTestComponentStream.TestTEnumComponentRead;

Var
  C : TEnumComponent;

begin
  TestTEnumComponent;
  C:=TEnumComponent.Create(Nil);
  Try
    C.Dice:=One;
    LoadFromStream(C);
    AssertEquals('Name','TestTEnumComponent',C.Name);
    AssertTrue('Dice',four=C.Dice);
  Finally
    C.Free;
  end;
end;

procedure TTestComponentStream.TestTEnumComponent2Read;

Var
  C : TEnumComponent2;

begin
  TestTEnumComponent2;
  C:=TEnumComponent2.Create(Nil);
  Try
    C.Dice:=Three;
    LoadFromStream(C);
    AssertEquals('Name','TestTEnumComponent2',C.Name);
    // Stream does  a value
    AssertTrue('Dice',One=C.Dice);
  Finally
    C.Free;
  end;
end;

procedure TTestComponentStream.TestTEnumComponent3Read;

Var
  C : TEnumComponent3;

begin
  TestTEnumComponent3;
  C:=TEnumComponent3.Create(Nil);
  Try
    LoadFromStream(C);
    AssertEquals('Name','TestTEnumComponent3',C.Name);
    // Stream does not contain a value
    AssertTrue('Dice',Three=C.Dice);
  Finally
    C.Free;
  end;
end;

procedure TTestComponentStream.TestTEnumComponent4Read;

Var
  C : TEnumComponent4;

begin
  TestTEnumComponent4;
  C:=TEnumComponent4.Create(Nil);
  Try
    C.Dice:=six;
    LoadFromStream(C);
    AssertEquals('Name','TestTEnumComponent4',C.Name);
    // Stream does not contain a value
    AssertTrue('Dice',Six=C.Dice);
  Finally
    C.Free;
  end;
end;

procedure TTestComponentStream.TestTEnumComponent5Read;
Var
  C : TEnumComponent5;

begin
  TestTEnumComponent5;
  C:=TEnumComponent5.Create(Nil);
  Try
    C.Dice:=six;
    LoadFromStream(C);
    AssertEquals('Name','TestTEnumComponent5',C.Name);
    // Stream does not contain a value
    AssertTrue('Dice',Six=C.Dice);
  Finally
    C.Free;
  end;
end;

procedure TTestComponentStream.TestTSetComponentRead;

Var
  C : TSetComponent;

begin
  TestTSetComponent;
  C:=TSetComponent.Create(Nil);
  Try
    C.Throw:=[];
    LoadFromStream(C);
    AssertEquals('Name','TestTSetComponent',C.Name);
    AssertTrue('Throw',[two,five]=C.Throw);
  Finally
    C.Free;
  end;
end;

procedure TTestComponentStream.TestTSetComponent2Read;

Var
  C : TSetComponent2;

begin
  TestTSetComponent2;
  C:=TSetComponent2.Create(Nil);
  Try
    C.Throw:=[one,six];
    LoadFromStream(C);
    AssertEquals('Name','TestTSetComponent2',C.Name);
    // Nothing was streamed
    AssertTrue('Throw',[one,six]=C.Throw);
  Finally
    C.Free;
  end;
end;

procedure TTestComponentStream.TestTSetComponent3Read;

Var
  C : TSetComponent3;

begin
  TestTSetComponent3;
  C:=TSetComponent3.Create(Nil);
  Try
    C.Throw:=[two,six];
    LoadFromStream(C);
    AssertEquals('Name','TestTSetComponent3',C.Name);
    // Nothing was streamed
    AssertTrue('Throw',[one,four]=C.Throw);
  Finally
    C.Free;
  end;
end;

procedure TTestComponentStream.TestTSetComponent4Read;

Var
  C : TSetComponent4;

begin
  TestTSetComponent4;
  C:=TSetComponent4.Create(Nil);
  Try
    C.Throw:=[two,six];
    LoadFromStream(C);
    AssertEquals('Name','TestTSetComponent4',C.Name);
    // Nothing was streamed
    AssertTrue('Throw',[two,six]=C.Throw);
  Finally
    C.Free;
  end;
end;

procedure TTestComponentStream.TestTMultipleComponentRead;
Var
  C : TMultipleComponent;

begin
  TestTMultipleComponent;
  C:=TMultipleComponent.Create(Nil);
  Try
    c.IntProp:=23;
    C.Dice:=six;
    C.CurrencyProp:=12.3;
    C.StringProp:='abc';
    LoadFromStream(C);
    AssertEquals('Name','TestTMultipleComponent',C.Name);
    AssertEquals('IntProp',1,C.IntProp);
    AssertEquals('StringProp','A String',C.StringProp);
    AssertEquals('CurrencyProp',2.3,C.CurrencyProp,0.1);
    AssertTrue('Dice',two=C.Dice);
    AssertTrue('Throw',[three,four]=C.Throw);
  Finally
    C.Free;
  end;
end;

procedure TTestComponentStream.TestTPersistentComponentRead;

Var
  C : TPersistentComponent;

begin
  TestTPersistentComponent;
  C:=TPersistentComponent.Create(Nil);
  Try
    C.Persist.AInteger:=36;
    C.Persist.AString:='nono';
    LoadFromStream(C);
    AssertEquals('Name','TestTPersistentComponent',C.Name);
    AssertEquals('Persist.AInteger',3,C.Persist.AInteger);
    AssertEquals('Persist.AString','A persistent string',C.Persist.AString);
  Finally
    C.Free;
  end;
end;

procedure TTestComponentStream.TestTCollectionComponentRead;

Var
  C : TCollectionComponent;

begin
  TestTCollectionComponent;
  C:=TCollectionComponent.Create(Nil);
  Try
    C.Coll.Add;
    LoadFromStream(C);
    AssertEquals('Name','TestTCollectionComponent',C.Name);
    // If the stream does not have a collection, it does not get cleared
    AssertEquals('Coll count',1,C.Coll.Count);
  Finally
    C.Free;
  end;
end;

procedure TTestComponentStream.TestTCollectionComponent2Read;

Var
  C : TCollectionComponent2;

begin
  TestTCollectionComponent2;
  C:=TCollectionComponent2.Create(Nil);
  Try
    C.Coll.Add;
    LoadFromStream(C);
    AssertEquals('Name','TestTCollectionComponent2',C.Name);
    AssertEquals('Coll count',3,C.Coll.Count);
    AssertEquals('Correct class type',TTestItem,C.Coll.Items[0].ClassType);
    AssertEquals('Coll 0 Property','First',TTestItem(C.Coll.items[0]).StrProp);
    AssertEquals('Coll 1 Property','Second',TTestItem(C.Coll.Items[1]).StrProp);
    AssertEquals('Coll 2 Property','Third',TTestItem(C.Coll.Items[2]).StrProp);
  Finally
    C.Free;
  end;
end;

procedure TTestComponentStream.TestTCollectionComponent3Read;

Var
  C : TCollectionComponent3;

begin
  TestTCollectionComponent3;
  C:=TCollectionComponent3.Create(Nil);
  Try
    C.Coll.Add;
    LoadFromStream(C);
    AssertEquals('Name','TestTCollectionComponent3',C.Name);
    AssertEquals('Coll count',3,C.Coll.Count);
    AssertEquals('Correct class type',TTestItem,C.Coll.Items[0].ClassType);
    AssertEquals('Coll 0 Property','First',TTestItem(C.Coll.items[0]).StrProp);
    AssertEquals('Coll 1 Property','',TTestItem(C.Coll.Items[1]).StrProp);
    AssertEquals('Coll 2 Property','Third',TTestItem(C.Coll.Items[2]).StrProp);
  Finally
    C.Free;
  end;
end;

procedure TTestComponentStream.TestTCollectionComponent4Read;

Var
  C : TCollectionComponent4;

begin
  TestTCollectionComponent4;
  C:=TCollectionComponent4.Create(Nil);
  Try
    C.Coll.Add;
    LoadFromStream(C);
    AssertEquals('Name','TestTCollectionComponent4',C.Name);
    AssertEquals('Coll count',1,C.Coll.Count);
    AssertEquals('Correct class type',TTestItem,C.Coll.Items[0].ClassType);
    AssertEquals('Coll 0 Property','Something',TTestItem(C.Coll.items[0]).StrProp);
  Finally
    C.Free;
  end;
end;

procedure TTestComponentStream.TestTCollectionComponent5Read;

Var
  C : TCollectionComponent5;

begin
  TestTCollectionComponent5;
  C:=TCollectionComponent5.Create(Nil);
  Try
    C.Coll.Add;
    LoadFromStream(C);
    AssertEquals('Name','TestTCollectionComponent5',C.Name);
    AssertEquals('Coll count',2,C.Coll.Count);
    AssertEquals('Correct class type',TTest2Item,C.Coll.Items[0].ClassType);
    AssertEquals('Coll 0 Property 1','Something',TTest2Item(C.Coll.items[0]).StrProp1);
    AssertEquals('Coll 0 Property 2','Otherthing',TTest2Item(C.Coll.items[0]).StrProp2);
    AssertEquals('Coll 1 property 1','Something 2',TTest2Item(C.Coll.items[1]).StrProp1);
    AssertEquals('Coll 1 property 2','Otherthing 2',TTest2Item(C.Coll.items[1]).StrProp2);
  Finally
    C.Free;
  end;
end;

procedure TTestComponentStream.TestTOwnedComponentRead;

Var
  C : TOwnedComponent;
  C2 : TComponent;
begin
  TestTOwnedComponent;
  C:=TOwnedComponent.Create(Nil);
  try
    C2:=C.CompProp;
    C.CompProp:=nil;
    LoadFromStream(C);
    AssertEquals('Name','TestTOwnedComponent',C.Name);
    AssertEquals('ComponentCount',1,C.ComponentCount);
    AssertSame('ComponentCount',C2,C.CompProp);
  finally
    C.Free;
  end;
end;

procedure TTestComponentStream.TestTStreamedOwnedComponentRead;
Var
  C : TStreamedOwnedComponent;

begin
  TestTStreamedOwnedComponent;
  C:=TStreamedOwnedComponent.Create(Nil);
  Try
    C.Sub.Free;
    C.Sub:=Nil;
    LoadFromStream(C);
    AssertEquals('Name','TestTStreamedOwnedComponent',C.Name);
    AssertNotNull('Have sub',C.Sub);
    AssertEquals('Correct class',TIntegerComponent,C.Sub.ClassType);
    AssertEquals('Name','Sub',C.Sub.Name);
    AssertEquals('Name',3,C.Sub.IntProp);
  Finally
    C.Free;
  end;
end;

procedure TTestComponentStream.TestTStreamedOwnedComponentsRead;
Var
  C : TStreamedOwnedComponents;

begin
  TestTStreamedOwnedComponents;
  C:=TStreamedOwnedComponents.Create(Nil);
  Try
    C.SubA.Free;
    C.SubA:=Nil;
    C.SubB.Free;
    C.SubB:=Nil;
    LoadFromStream(C);
    AssertEquals('Name','TestTStreamedOwnedComponents',C.Name);
    AssertNotNull('Have sub A',C.SubA);
    AssertEquals('Correct sub A class',TIntegerComponent,C.SubA.ClassType);
    AssertEquals('Name','SubA',C.SubA.Name);
    AssertEquals('Name',3,C.SubA.IntProp);
    AssertNotNull('Have sub B',C.SubB);
    AssertEquals('Correct sub B class',TStringComponent,C.SubB.ClassType);
    AssertEquals('Name','SubB',C.SubB.Name);
    AssertEquals('Name','A string',C.SubB.StringProp);
  Finally
    C.Free;
  end;
end;

Procedure TTestComponentStream.TestTIntegerComponent2;

Var
  C : TComponent;

begin
  C:=TIntegerComponent2.Create(Nil);
  Try
    SaveToStream(C);
    ExpectSignature;
    ExpectFlags([],0);
    ExpectBareString('TIntegerComponent2');
    ExpectBareString('TestTIntegerComponent2');
    ExpectBareString('IntProp');
    ExpectInteger(1024);
    ExpectEndOfList;
    ExpectEndOfList;
  Finally
    C.Free;
  end;
end;


Procedure TTestComponentStream.TestTIntegerComponent3;

Var
  C : TComponent;

begin
  C:=TIntegerComponent3.Create(Nil);
  Try
    SaveToStream(C);
    ExpectSignature;
    ExpectFlags([],0);
    ExpectBareString('TIntegerComponent3');
    ExpectBareString('TestTIntegerComponent3');
    ExpectBareString('IntProp');
    ExpectInteger(262144);
    ExpectEndOfList;
    ExpectEndOfList;
  Finally
    C.Free;
  end;
end;


Procedure TTestComponentStream.TestTIntegerComponent4;

Var
  C : TComponent;

begin
  C:=TIntegerComponent4.Create(Nil);
  Try
    SaveToStream(C);
    ExpectSignature;
    ExpectFlags([],0);
    ExpectBareString('TIntegerComponent4');
    ExpectBareString('TestTIntegerComponent4');
    ExpectEndOfList;
    ExpectEndOfList;
  Finally
    C.Free;
  end;
end;


Procedure TTestComponentStream.TestTIntegerComponent5;

Var
  C : TComponent;

begin
  C:=TIntegerComponent5.Create(Nil);
  Try
    SaveToStream(C);
    ExpectSignature;
    ExpectFlags([],0);
    ExpectBareString('TIntegerComponent5');
    ExpectBareString('TestTIntegerComponent5');
    ExpectBareString('IntProp');
    ExpectInteger(5);
    ExpectEndOfList;
    ExpectEndOfList;
  Finally
    C.Free;
  end;
end;


Procedure TTestComponentStream.TestTInt64Component;

Var
  C : TComponent;

begin
  C:=TInt64Component.Create(Nil);
  Try
    SaveToStream(C);
    ExpectSignature;
    ExpectFlags([],0);
    ExpectBareString('TInt64Component');
    ExpectBareString('TestTInt64Component');
    ExpectBareString('Int64Prop');
    ExpectInteger(4);
    ExpectEndOfList;
    ExpectEndOfList;
  Finally
    C.Free;
  end;
end;


Procedure TTestComponentStream.TestTInt64Component2;

Var
  C : TComponent;

begin
  C:=TInt64Component2.Create(Nil);
  Try
    SaveToStream(C);
    ExpectSignature;
    ExpectFlags([],0);
    ExpectBareString('TInt64Component2');
    ExpectBareString('TestTInt64Component2');
    ExpectBareString('Int64Prop');
    ExpectInteger(1024);
    ExpectEndOfList;
    ExpectEndOfList;
  Finally
    C.Free;
  end;
end;


Procedure TTestComponentStream.TestTInt64Component3;

Var
  C : TComponent;

begin
  C:=TInt64Component3.Create(Nil);
  Try
    SaveToStream(C);
    ExpectSignature;
    ExpectFlags([],0);
    ExpectBareString('TInt64Component3');
    ExpectBareString('TestTInt64Component3');
    ExpectBareString('Int64Prop');
    ExpectInteger(262144);
    ExpectEndOfList;
    ExpectEndOfList;
  Finally
    C.Free;
  end;
end;


Procedure TTestComponentStream.TestTInt64Component4;

Var
  C : TComponent;

begin
  C:=TInt64Component4.Create(Nil);
  Try
    SaveToStream(C);
    ExpectSignature;
    ExpectFlags([],0);
    ExpectBareString('TInt64Component4');
    ExpectBareString('TestTInt64Component4');
    ExpectBareString('Int64Prop');
    ExpectInt64(2147516415{     2147745791});
    ExpectEndOfList;
    ExpectEndOfList;
  Finally
    C.Free;
  end;
end;


Procedure TTestComponentStream.TestTInt64Component5;

Var
  C : TComponent;

begin
  C:=TInt64Component5.Create(Nil);
  Try
    SaveToStream(C);
    ExpectSignature;
    ExpectFlags([],0);
    ExpectBareString('TInt64Component5');
    ExpectBareString('TestTInt64Component5');
//    ExpectBareString('Int64Prop');
//    ExpectInteger(7);
    ExpectEndOfList;
    ExpectEndOfList;
  Finally
    C.Free;
  end;
end;


Procedure TTestComponentStream.TestTInt64Component6;

Var
  C : TComponent;

begin
  C:=TInt64Component6.Create(Nil);
  Try
    SaveToStream(C);
    ExpectSignature;
    ExpectFlags([],0);
    ExpectBareString('TInt64Component6');
    ExpectBareString('TestTInt64Component6');
    ExpectBareString('Int64Prop');
    ExpectInteger(8);
    ExpectEndOfList;
    ExpectEndOfList;
  Finally
    C.Free;
  end;
end;


Procedure TTestComponentStream.TestTStringComponent;

Var
  C : TComponent;

begin
  C:=TStringComponent.Create(Nil);
  Try
    SaveToStream(C);
    ExpectSignature;
    ExpectFlags([],0);
    ExpectBareString('TStringComponent');
    ExpectBareString('TestTStringComponent');
    ExpectBareString('StringProp');
    ExpectString('A string');
    ExpectEndOfList;
    ExpectEndOfList;
  Finally
    C.Free;
  end;
end;


Procedure TTestComponentStream.TestTStringComponent2;

Var
  C : TComponent;

begin
  C:=TStringComponent2.Create(Nil);
  Try
    SaveToStream(C);
    ExpectSignature;
    ExpectFlags([],0);
    ExpectBareString('TStringComponent2');
    ExpectBareString('TestTStringComponent2');
    ExpectEndOfList;
    ExpectEndOfList;
  Finally
    C.Free;
  end;
end;


Procedure TTestComponentStream.TestTWideStringComponent;

Var
  C : TComponent;

begin
  C:=TWideStringComponent.Create(Nil);
  Try
    SaveToStream(C);
    ExpectSignature;
    ExpectFlags([],0);
    ExpectBareString('TWideStringComponent');
    ExpectBareString('TestTWideStringComponent');
    ExpectBareString('WideStringProp');
    ExpectString('Some WideString');
    ExpectEndOfList;
    ExpectEndOfList;
  Finally
    C.Free;
  end;
end;


Procedure TTestComponentStream.TestTWideStringComponent2;

Var
  C : TComponent;

begin
  C:=TWideStringComponent2.Create(Nil);
  Try
    SaveToStream(C);
    ExpectSignature;
    ExpectFlags([],0);
    ExpectBareString('TWideStringComponent2');
    ExpectBareString('TestTWideStringComponent2');
    ExpectEndOfList;
    ExpectEndOfList;
  Finally
    C.Free;
  end;
end;


Procedure TTestComponentStream.TestTSingleComponent;

Var
  C : TComponent;

begin
  C:=TSingleComponent.Create(Nil);
  Try
    SaveToStream(C);
    ExpectSignature;
    ExpectFlags([],0);
    ExpectBareString('TSingleComponent');
    ExpectBareString('TestTSingleComponent');
    ExpectBareString('SingleProp');
    ExpectExtended(1.23);
    ExpectEndOfList;
    ExpectEndOfList;
  Finally
    C.Free;
  end;
end;


Procedure TTestComponentStream.TestTDoubleComponent;

Var
  C : TComponent;

begin
  C:=TDoubleComponent.Create(Nil);
  Try
    SaveToStream(C);
    ExpectSignature;
    ExpectFlags([],0);
    ExpectBareString('TDoubleComponent');
    ExpectBareString('TestTDoubleComponent');
    ExpectBareString('DoubleProp');
    ExpectExtended(2.34);
    ExpectEndOfList;
    ExpectEndOfList;
  Finally
    C.Free;
  end;
end;


Procedure TTestComponentStream.TestTExtendedComponent;

Var
  C : TComponent;

begin
  C:=TExtendedComponent.Create(Nil);
  Try
    SaveToStream(C);
    ExpectSignature;
    ExpectFlags([],0);
    ExpectBareString('TExtendedComponent');
    ExpectBareString('TestTExtendedComponent');
    ExpectBareString('ExtendedProp');
    ExpectExtended(3.45);
    ExpectEndOfList;
    ExpectEndOfList;
  Finally
    C.Free;
  end;
end;


(*
Procedure TTestComponentStream.TestTCompComponent;

Var
  C : TComponent;

begin
  C:=TCompComponent.Create(Nil);
  Try
    SaveToStream(C);
    ExpectSignature;
    ExpectFlags([],0);
    ExpectBareString('TCompComponent');
    ExpectBareString('TestTCompComponent');
    ExpectBareString('ExtendedProp');
    ExpectExtended(5.00);
    ExpectEndOfList;
    ExpectEndOfList;
  Finally
    C.Free;
    end;
end;
*)

Procedure TTestComponentStream.TestTCurrencyComponent;

Var
  C : TComponent;

begin
  C:=TCurrencyComponent.Create(Nil);
  Try
    SaveToStream(C);
    ExpectSignature;
    ExpectFlags([],0);
    ExpectBareString('TCurrencyComponent');
    ExpectBareString('TestTCurrencyComponent');
    ExpectBareString('CurrencyProp');
    ExpectInteger(56700);
// Natively, this is:
//    ExpectExtended(5.67);
    ExpectEndOfList;
    ExpectEndOfList;
  Finally
    C.Free;
  end;
end;


Procedure TTestComponentStream.TestTDateTimeComponent;

Var
  C : TComponent;

begin
  C:=TDateTimeComponent.Create(Nil);
  Try
    SaveToStream(C);
    ExpectSignature;
    ExpectFlags([],0);
    ExpectBareString('TDateTimeComponent');
    ExpectBareString('TestTDateTimeComponent');
    ExpectBareString('DateTimeProp');
    ExpectExtended(35278.00);
    ExpectEndOfList;
    ExpectEndOfList;
  Finally
    C.Free;
  end;
end;


Procedure TTestComponentStream.TestTDateTimeComponent2;

Var
  C : TComponent;

begin
  C:=TDateTimeComponent2.Create(Nil);
  Try
    SaveToStream(C);
    ExpectSignature;
    ExpectFlags([],0);
    ExpectBareString('TDateTimeComponent2');
    ExpectBareString('TestTDateTimeComponent2');
    ExpectBareString('DateTimeProp');
    ExpectExtended(0.97);
    ExpectEndOfList;
    ExpectEndOfList;
  Finally
    C.Free;
  end;
end;


Procedure TTestComponentStream.TestTDateTimeComponent3;

Var
  C : TComponent;

begin
  C:=TDateTimeComponent3.Create(Nil);
  Try
    SaveToStream(C);
    ExpectSignature;
    ExpectFlags([],0);
    ExpectBareString('TDateTimeComponent3');
    ExpectBareString('TestTDateTimeComponent3');
    ExpectBareString('DateTimeProp');
    ExpectExtended(35278.97);
    ExpectEndOfList;
    ExpectEndOfList;
  Finally
    C.Free;
  end;
end;


Procedure TTestComponentStream.TestTEnumComponent;

Var
  C : TComponent;

begin
  C:=TEnumComponent.Create(Nil);
  Try
    SaveToStream(C);
    ExpectSignature;
    ExpectFlags([],0);
    ExpectBareString('TEnumComponent');
    ExpectBareString('TestTEnumComponent');
    ExpectBareString('Dice');
    ExpectIdent('four');
    ExpectEndOfList;
    ExpectEndOfList;
  Finally
    C.Free;
  end;
end;


Procedure TTestComponentStream.TestTEnumComponent2;

Var
  C : TComponent;

begin
  C:=TEnumComponent2.Create(Nil);
  Try
    SaveToStream(C);
    ExpectSignature;
    ExpectFlags([],0);
    ExpectBareString('TEnumComponent2');
    ExpectBareString('TestTEnumComponent2');
{$ifndef FPC}
    // FPC does not stream an undeclared default value, it assumes the
    // 0-the value is the default.
    ExpectBareString('Dice');
    ExpectIdent('one');
{$endif FPC}
    ExpectEndOfList;
    ExpectEndOfList;
  Finally
    C.Free;
  end;
end;


Procedure TTestComponentStream.TestTEnumComponent3;

Var
  C : TComponent;

begin
  C:=TEnumComponent3.Create(Nil);
  Try
    SaveToStream(C);
    ExpectSignature;
    ExpectFlags([],0);
    ExpectBareString('TEnumComponent3');
    ExpectBareString('TestTEnumComponent3');
    ExpectBareString('Dice');
    ExpectIdent('three');
    ExpectEndOfList;
    ExpectEndOfList;
  Finally
    C.Free;
  end;
end;


Procedure TTestComponentStream.TestTEnumComponent4;

Var
  C : TComponent;

begin
  C:=TEnumComponent4.Create(Nil);
  Try
    SaveToStream(C);
    ExpectSignature;
    ExpectFlags([],0);
    ExpectBareString('TEnumComponent4');
    ExpectBareString('TestTEnumComponent4');
    ExpectEndOfList;
    ExpectEndOfList;
  Finally
    C.Free;
  end;
end;

Procedure TTestComponentStream.TestTEnumComponent5;

Var
  C : TComponent;

begin
  C:=TEnumComponent5.Create(Nil);
  Try
    SaveToStream(C);
    ExpectSignature;
    ExpectFlags([],0);
    ExpectBareString('TEnumComponent5');
    ExpectBareString('TestTEnumComponent5');
    ExpectEndOfList;
    ExpectEndOfList;
  Finally
    C.Free;
  end;
end;


Procedure TTestComponentStream.TestTSetComponent;

Var
  C : TComponent;

begin
  C:=TSetComponent.Create(Nil);
  Try
    SaveToStream(C);
    ExpectSignature;
    ExpectFlags([],0);
    ExpectBareString('TSetComponent');
    ExpectBareString('TestTSetComponent');
    ExpectBareString('Throw');
    ExpectValue(vaSet);
    ExpectBareString('two');
    ExpectBareString('five');
    ExpectBareString('');
    ExpectEndOfList;
    ExpectEndOfList;
  Finally
    C.Free;
  end;
end;


Procedure TTestComponentStream.TestTSetComponent2;

Var
  C : TComponent;

begin
  C:=TSetComponent2.Create(Nil);
  Try
    SaveToStream(C);
    ExpectSignature;
    ExpectFlags([],0);
    ExpectBareString('TSetComponent2');
    ExpectBareString('TestTSetComponent2');
{$ifdef delphi}
    // Same as for sets: a set with undeclared default is regarded as
    // A set with default [], and is not streamed if it is empty.
    ExpectBareString('Throw');
    ExpectValue(vaSet);
    ExpectBareString('');
{$endif delphi}
    ExpectEndOfList;
    ExpectEndOfList;
  Finally
    C.Free;
  end;
end;


Procedure TTestComponentStream.TestTSetComponent3;

Var
  C : TComponent;

begin
  C:=TSetComponent3.Create(Nil);
  Try
    SaveToStream(C);
    ExpectSignature;
    ExpectFlags([],0);
    ExpectBareString('TSetComponent3');
    ExpectBareString('TestTSetComponent3');
    ExpectBareString('Throw');
    ExpectValue(vaSet);
    ExpectBareString('one');
    ExpectBareString('four');
    ExpectBareString('');
    ExpectEndOfList;
    ExpectEndOfList;
  Finally
    C.Free;
  end;
end;


Procedure TTestComponentStream.TestTSetComponent4;

Var
  C : TComponent;

begin
  // Writeln('Start test');
  C:=TSetComponent4.Create(Nil);
  Try
    SaveToStream(C);
    ExpectSignature;
    ExpectFlags([],0);
    ExpectBareString('TSetComponent4');
    ExpectBareString('TestTSetComponent4');
    ExpectEndOfList;
    ExpectEndOfList;
  Finally
    C.Free;
  end;
end;


Procedure TTestComponentStream.TestTMultipleComponent;

Var
  C : TComponent;

begin
  C:=TMultipleComponent.Create(Nil);
  Try
    SaveToStream(C);
    ExpectSignature;
    ExpectFlags([],0);
    ExpectBareString('TMultipleComponent');
    ExpectBareString('TestTMultipleComponent');
    ExpectBareString('IntProp');
    ExpectInteger(1);
    ExpectBareString('StringProp');
    ExpectString('A String');
    ExpectBareString('CurrencyProp');
    ExpectInteger(23000);
//    ExpectExtended(2.30);
    ExpectBareString('Dice');
    ExpectIdent('two');
    ExpectBareString('Throw');
    ExpectValue(vaSet);
    ExpectBareString('three');
    ExpectBareString('four');
    ExpectBareString('');
    ExpectEndOfList;
    ExpectEndOfList;
  Finally
    C.Free;
  end;
end;


Procedure TTestComponentStream.TestTPersistentComponent;

Var
  C : TComponent;

begin
  C:=TPersistentComponent.Create(Nil);
  Try
    SaveToStream(C);
    ExpectSignature;
    ExpectFlags([],0);
    ExpectBareString('TPersistentComponent');
    ExpectBareString('TestTPersistentComponent');
    ExpectBareString('Persist.AInteger');
    ExpectInteger(3);
    ExpectBareString('Persist.AString');
    ExpectString('A persistent string');
    ExpectEndOfList;
    ExpectEndOfList;
  Finally
    C.Free;
  end;
end;


Procedure TTestComponentStream.TestTCollectionComponent;

Var
  C : TComponent;

begin
  C:=TCollectionComponent.Create(Nil);
  Try
    SaveToStream(C);
    ExpectSignature;
    ExpectFlags([],0);
    ExpectBareString('TCollectionComponent');
    ExpectBareString('TestTCollectionComponent');
    ExpectBareString('Coll');
    ExpectValue(vaCollection);
    ExpectEndOfList;
    ExpectEndOfList;
    ExpectEndOfList;
  Finally
    C.Free;
  end;
end;


Procedure TTestComponentStream.TestTCollectionComponent2;

Var
  C : TComponent;

begin
  C:=TCollectionComponent2.Create(Nil);
  Try
    SaveToStream(C);
    ExpectSignature;
    ExpectFlags([],0);
    ExpectBareString('TCollectionComponent2');
    ExpectBareString('TestTCollectionComponent2');
    ExpectBareString('Coll');
    ExpectValue(vaCollection);
    ExpectValue(vaList);
    ExpectBareString('StrProp');
    ExpectString('First');
    ExpectEndOfList;
    ExpectValue(vaList);
    ExpectBareString('StrProp');
    ExpectString('Second');
    ExpectEndOfList;
    ExpectValue(vaList);
    ExpectBareString('StrProp');
    ExpectString('Third');
    ExpectEndOfList;
    ExpectEndOfList;
    ExpectEndOfList;
    ExpectEndOfList;
  Finally
    C.Free;
  end;
end;


Procedure TTestComponentStream.TestTCollectionComponent3;

Var
  C : TComponent;

begin
  C:=TCollectionComponent3.Create(Nil);
  Try
    SaveToStream(C);
    ExpectSignature;
    ExpectFlags([],0);
    ExpectBareString('TCollectionComponent3');
    ExpectBareString('TestTCollectionComponent3');
    ExpectBareString('Coll');
    ExpectValue(vaCollection);
    ExpectValue(vaList);
    ExpectBareString('StrProp');
    ExpectString('First');
    ExpectEndOfList;
    ExpectValue(vaList);
    ExpectEndOfList;
    ExpectValue(vaList);
    ExpectBareString('StrProp');
    ExpectString('Third');
    ExpectEndOfList;
    ExpectEndOfList;
    ExpectEndOfList;
    ExpectEndOfList;
  Finally
    C.Free;
  end;
end;


Procedure TTestComponentStream.TestTCollectionComponent4;

Var
  C : TComponent;

begin
  C:=TCollectionComponent4.Create(Nil);
  Try
    SaveToStream(C);
    ExpectSignature;
    ExpectFlags([],0);
    ExpectBareString('TCollectionComponent4');
    ExpectBareString('TestTCollectionComponent4');
    ExpectBareString('Coll');
    ExpectValue(vaCollection);
    ExpectValue(vaList);
    ExpectBareString('StrProp');
    ExpectString('Something');
    ExpectEndOfList;
    ExpectEndOfList;
    ExpectEndOfList;
    ExpectEndOfList;
  Finally
    C.Free;
  end;
end;

Procedure TTestComponentStream.TestTCollectionComponent5;

Var
  C : TComponent;

begin
  C:=TCollectionComponent5.Create(Nil);
  Try
    SaveToStream(C);
    ExpectSignature;
    ExpectFlags([],0);
    ExpectBareString('TCollectionComponent5');
    ExpectBareString('TestTCollectionComponent5');
    ExpectBareString('Coll');
    ExpectValue(vaCollection);
    ExpectValue(vaList);
    ExpectBareString('StrProp1');
    ExpectString('Something');
    ExpectBareString('StrProp2');
    ExpectString('Otherthing');
    ExpectEndOfList;
    ExpectValue(vaList);
    ExpectBareString('StrProp1');
    ExpectString('Something 2');
    ExpectBareString('StrProp2');
    ExpectString('Otherthing 2');
    ExpectEndOfList;
    ExpectEndOfList;
    ExpectEndOfList;
  Finally
    C.Free;
  end;
end;


Procedure TTestComponentStream.TestTOwnedComponent;

Var
  C : TComponent;

begin
  C:=TOwnedComponent.Create(Nil);
  Try
    SaveToStream(C);
    ExpectSignature;
    ExpectFlags([],0);
    ExpectBareString('TOwnedComponent');
    ExpectBareString('TestTOwnedComponent');
    ExpectBareString('CompProp');
    ExpectIdent('SubComponent');
    ExpectEndOfList;
    ExpectEndOfList;
  Finally
    C.Free;
  end;
end;


Procedure TTestComponentStream.TestTStreamedOwnedComponent;

Var
  C : TComponent;

begin
  C:=TStreamedOwnedComponent.Create(Nil);
  Try
    SaveToStream(C);
    ExpectSignature;
    ExpectFlags([],0);
    ExpectBareString('TStreamedOwnedComponent');
    ExpectBareString('TestTStreamedOwnedComponent');
    ExpectEndOfList;
    ExpectFlags([],0);
    ExpectBareString('TIntegerComponent');
    ExpectBareString('Sub');
    ExpectBareString('IntProp');
    ExpectInteger(3);
    ExpectEndOfList;
    ExpectEndOfList;
    ExpectEndOfList;
    ExpectEndOfStream;
  Finally
    C.Free;
  end;
end;

Procedure TTestComponentStream.TestTStreamedOwnedComponents;

Var
  C : TComponent;

begin
  C:=TStreamedOwnedComponents.Create(Nil);
  Try
    SaveToStream(C);
    ExpectSignature;
    ExpectFlags([],0);
    ExpectBareString('TStreamedOwnedComponents');
    ExpectBareString('TestTStreamedOwnedComponents');
    ExpectEndOfList;
    ExpectFlags([],0);
    ExpectBareString('TIntegerComponent');
    ExpectBareString('SubA');
    ExpectBareString('IntProp');
    ExpectInteger(3);
    ExpectEndOfList;
    ExpectEndOfList;
    ExpectFlags([],0);
    ExpectBareString('TStringComponent');
    ExpectBareString('SubB');
    ExpectBareString('StringProp');
    ExpectString('A string');
    ExpectEndOfList;
    ExpectEndOfList;
    ExpectEndOfList;
    ExpectEndOfStream;
  Finally
    C.Free;
  end;
end;


Procedure TTestComponentStream.TestTMethodComponent;

Var
  C : TComponent;

begin
  C:=TMethodComponent.Create(Nil);
  Try
    SaveToStream(C);
    ExpectSignature;
    ExpectFlags([],0);
    ExpectBareString('TMethodComponent');
    ExpectBareString('TestTMethodComponent');
    ExpectBareString('MethodProp');
    ExpectIdent('MyMethod');
    ExpectEndOfList;
    ExpectEndOfList;
  Finally
    C.Free;
  end;
end;


Procedure TTestComponentStream.TestTMethodComponent2;

Var
  C : TComponent;

begin
  C:=TMethodComponent2.Create(Nil);
  Try
    SaveToStream(C);
    ExpectSignature;
    ExpectFlags([],0);
    ExpectBareString('TMethodComponent2');
    ExpectBareString('TestTMethodComponent2');
    ExpectEndOfList;
    ExpectFlags([],0);
    ExpectBareString('TMethodComponent');
    ExpectBareString('AComponent');
    ExpectBareString('MethodProp');
    ExpectIdent('MyMethod2');
    ExpectEndOfList;
    ExpectEndOfList;
    ExpectEndOfList;
  Finally
    C.Free;
  end;
end;



Procedure TTestCollectionStream.CompareColl(CA,CB : TMyColl);

Var
  I : Integer;

begin
  AssertEquals('Counts differ: %d %d',CA.Count,CB.Count);
  For I:=0 to CA.Count-1 do
    begin
    AssertEquals(Format('Nr property of element %d equals',[I]),CA[i].Nr,CB[i].Nr);
    AssertEquals(Format('Str property of element %d equals',[I]),CA[i].Str,CB[i].Str);
    end;
end;

Function TTestCollectionStream.EmptyComp : TCollComp;

begin
  Result:=TCollComp.Create(Nil);
end;

Function TTestCollectionStream.CreateColl(Anr : Integer) : TCollComp;

Var
  I : Integer;
  T : TMyItem;

begin
  Result:=EmptyComp;
  Result.Name:='C'+IntToStr(Anr);
  For I:=0 to ANr-1 do
    begin
    T:=Result.MyColl.Add as TMyItem;
    T.Nr:=I; // not I+1, so the default value gets tested too
    T.Str:=IntToStr(I+1);
    end;
end;

Procedure TTestCollectionStream.TestEmpty;

Var
 CA,CB : TCollComp;

begin
  CA:=CreateColl(0);
  try
    CB:=EmptyComp;
    Try
      CB.FromStream(CA.ToStream);
      CompareColl(CA.MyColl,CB.MyColl);
    Finally
      CB.Free;
    end;
  Finally
    CA.Free;
  end;
end;

Procedure TTestCollectionStream.TestNr(ACount : Integer);

Var
 CA,CB : TCollComp;

begin
  CA:=CreateColl(ACount);
  try
    CB:=EmptyComp;
    Try
      CB.FromStream(CA.ToStream);
      CompareColl(CA.MyColl,CB.MyColl);
    Finally
      CB.Free;
    end;
  Finally
    CA.Free;
  end;
end;

Procedure TTestCollectionStream.TestClear;

Var
 CA,CB : TCollComp;

begin
  CA:=CreateColl(3);
  try
    CB:=CreateColl(1);
    CB.Name:='';
    Try
      // CB collection should be cleared before loading.
      CB.FromStream(CA.ToStream);
      CompareColl(CA.MyColl,CB.MyColl);
    Finally
      CB.Free;
    end;
  Finally
    CA.Free;
  end;
end;

Procedure TTestCollectionStream.Test1;

begin
  TestNr(1);
end;

Procedure TTestCollectionStream.Test2;

begin
  TestNr(2);
end;

Procedure TTestCollectionStream.Test3;

begin
  TestNr(3);
end;

begin
  RegisterTests([TTestComponentStream,TTestCollectionStream]);
end.
