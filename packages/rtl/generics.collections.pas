unit Generics.Collections;

{$Mode Delphi}
{$COperators On}

interface

uses
  Classes, SysUtils,
  {$IFDEF Pas2js}JS,{$ENDIF}
  Generics.Strings, Generics.Defaults;

type
  TCollectionNotification = (cnAdded, cnRemoved, cnExtracted);
  TCollectionNotifyEvent<T> = procedure(ASender: TObject; const AItem: T;
    AAction: TCollectionNotification) of object;

  { TBinarySearchResult }

  TBinarySearchResult = record
    FoundIndex, CandidateIndex: SizeInt;
    CompareResult: SizeInt;
  end;

  { TCustomArrayHelper }

  TCustomArrayHelper<T> = class abstract
  protected
    class procedure QuickSort(var AValues: array of T; ALeft, ARight: SizeInt;
      const AComparer: IComparer<T>); virtual; abstract;
  public
    //class procedure Sort(var AValues: array of T); overload;
    class procedure Sort(var AValues: array of T;
      const AComparer: IComparer<T>); overload;
    class procedure Sort(var AValues: array of T;
      const AComparer: IComparer<T>; AIndex, ACount: SizeInt); overload;

    class function BinarySearch(const AValues: array of T; const AItem: T;
      out ASearchResult: TBinarySearchResult; const AComparer: IComparer<T>;
      AIndex, ACount: SizeInt): Boolean; virtual; abstract; overload;
    class function BinarySearch(const AValues: array of T; const AItem: T;
      out AFoundIndex: SizeInt; const AComparer: IComparer<T>;
      AIndex, ACount: SizeInt): Boolean; virtual; abstract; overload;
    class function BinarySearch(const AValues: array of T; const AItem: T;
      out AFoundIndex: SizeInt; const AComparer: IComparer<T>): Boolean; overload;
    class function BinarySearch(const AValues: array of T; const AItem: T;
      out ASearchResult: TBinarySearchResult; const AComparer: IComparer<T>): Boolean; overload;
    // No support for automatically creating a comparer.
    //  class function BinarySearch(const AValues: array of T; const AItem: T;
    //  out AFoundIndex: SizeInt): Boolean; overload;
    //  class function BinarySearch(const AValues: array of T; const AItem: T;
    //  out ASearchResult: TBinarySearchResult): Boolean; overload;
  end;

  { TArrayHelper }

  TArrayHelper<T> = class(TCustomArrayHelper<T>)
  protected
    class procedure QuickSort(var AValues: array of T; ALeft, ARight: SizeInt;
      const AComparer: IComparer<T>); override;
  public
    class function BinarySearch(const AValues: array of T; const AItem: T;
      out ASearchResult: TBinarySearchResult; const AComparer: IComparer<T>;
      AIndex, ACount: SizeInt): Boolean; override; overload;
    class function BinarySearch(const AValues: array of T; const AItem: T;
      out AFoundIndex: SizeInt; const AComparer: IComparer<T>;
      AIndex, ACount: SizeInt): Boolean; override; overload;
  end;

  { TEnumerator }

  TEnumerator<T> = class abstract
  protected
    function DoGetCurrent: T; virtual; abstract;
    function DoMoveNext: boolean; virtual; abstract;
  public
    property Current: T read DoGetCurrent;
    function MoveNext: boolean;
  end;

  { TEnumerable }

  TEnumerable<T> = class abstract
  protected
    type
      TMyEnumerator = TEnumerator<T>;
      TMyArray = TArray<T>;
    function DoGetEnumerator: TMyEnumerator; virtual; abstract;
  public
    function GetEnumerator: TMyEnumerator; inline;
    function ToArray: TMyArray; virtual; overload;
  end;

  { TCustomList }

  TCustomList<T> = class abstract(TEnumerable<T>)
  private
    FOnNotify: TCollectionNotifyEvent<T>;
    function GetCapacity: SizeInt; inline;
  protected
    type TMyArrayHelper = TArrayHelper<T>;
  protected
    FLength: SizeInt;
    FItems: array of T;
    function PrepareAddingItem: SizeInt; virtual;
    function PrepareAddingRange(ACount: SizeInt): SizeInt; virtual;
    procedure Notify(const AValue: T; ACollectionNotification: TCollectionNotification); virtual;
    function DoRemove(AIndex: SizeInt; ACollectionNotification: TCollectionNotification): T; virtual;
    procedure SetCapacity(AValue: SizeInt); virtual; abstract;
    function GetCount: SizeInt; virtual;
  public
    function ToArray: TArray<T>; override;

    property Count: SizeInt read GetCount;
    property Capacity: SizeInt read GetCapacity write SetCapacity;
    property OnNotify: TCollectionNotifyEvent<T> read FOnNotify write FOnNotify;

    procedure TrimExcess; virtual; abstract;
  end;

  { TCustomListEnumerator }

  TCustomListEnumerator<T> = class abstract(TEnumerator<T>)
  private
    FList: TCustomList<T>;
    FIndex: SizeInt;
  protected
    function DoMoveNext: boolean; override;
    function DoGetCurrent: T; override;
    function GetCurrent: T; virtual;
  public
    constructor Create(AList: TCustomList<T>);
  end;

  { TList }

  TList<T> = class(TCustomList<T>)
  private
    FComparer: IComparer<T>;
  protected
    procedure SetCapacity(AValue: SizeInt); override;
    procedure SetCount(AValue: SizeInt);
    procedure InitializeList; virtual;
    procedure InternalInsert(AIndex: SizeInt; const AValue: T);
    function DoGetEnumerator: TEnumerator<T>; override;
  private
    function GetItem(AIndex: SizeInt): T;
    procedure SetItem(AIndex: SizeInt; const AValue: T);
  public
    type
      TEnumerator = class(TCustomListEnumerator<T>);
    function GetEnumerator: TEnumerator; reintroduce;
  public
    constructor Create; overload;
    constructor Create(const AComparer: IComparer<T>); overload;
    constructor Create(ACollection: TEnumerable<T>); overload;

    destructor Destroy; override;

    function Add(const AValue: T): SizeInt; virtual;
    procedure AddRange(const AValues: array of T); virtual; overload;
    procedure AddRange(const AEnumerable: IEnumerable<T>); overload;
    procedure AddRange(AEnumerable: TEnumerable<T>); overload;

    procedure Insert(AIndex: SizeInt; const AValue: T); virtual;
    procedure InsertRange(AIndex: SizeInt; const AValues: array of T); virtual; overload;
    procedure InsertRange(AIndex: SizeInt; const AEnumerable: IEnumerable<T>); overload;
    procedure InsertRange(AIndex: SizeInt; const AEnumerable: TEnumerable<T>); overload;

    function Remove(const AValue: T): SizeInt;
    procedure Delete(AIndex: SizeInt); inline;
    procedure DeleteRange(AIndex, ACount: SizeInt);
    function ExtractIndex(const AIndex: SizeInt): T; overload;
    function Extract(const AValue: T): T; overload;

    procedure Exchange(AIndex1, AIndex2: SizeInt); virtual;
    procedure Move(AIndex, ANewIndex: SizeInt); virtual;

    function First: T; inline;
    function Last: T; inline;

    procedure Clear;

    function Contains(const AValue: T): Boolean; inline;
    function IndexOf(const AValue: T): SizeInt; virtual;
    function LastIndexOf(const AValue: T): SizeInt; virtual;

    procedure Reverse;

    procedure TrimExcess; override;

    procedure Sort; overload;
    procedure Sort(const AComparer: IComparer<T>); overload;
    function BinarySearch(const AItem: T; out AIndex: SizeInt): Boolean; overload;
    function BinarySearch(const AItem: T; out AIndex: SizeInt; const AComparer: IComparer<T>): Boolean; overload;

    property Count: SizeInt read FLength write SetCount;
    property Items[Index: SizeInt]: T read GetItem write SetItem; default;
  end;

implementation

{ TCustomArrayHelper }

class procedure TCustomArrayHelper<T>.Sort(var AValues: array of T;
  const AComparer: IComparer<T>);
begin
  QuickSort(AValues, 0, Length(AValues), AComparer);
end;

class procedure TCustomArrayHelper<T>.Sort(var AValues: array of T;
  const AComparer: IComparer<T>; AIndex, ACount: SizeInt);
begin
  if ACount <= 1 then
    Exit;
  QuickSort(AValues, AIndex, Pred(AIndex + ACount), AComparer);
end;

class function TCustomArrayHelper<T>.BinarySearch(const AValues: array of T;
  const AItem: T; out AFoundIndex: SizeInt; const AComparer: IComparer<T>
  ): Boolean;
begin
  Writeln('Here too',Length(aValues));
  Result := BinarySearch(AValues, AItem, AFoundIndex, AComparer,
                         0, Length(AValues));
end;

class function TCustomArrayHelper<T>.BinarySearch(const AValues: array of T;
  const AItem: T; out ASearchResult: TBinarySearchResult;
  const AComparer: IComparer<T>): Boolean;
begin
  Writeln('Here',Length(aValues));
  Result := BinarySearch(AValues, AItem, ASearchResult, AComparer,
                         0, Length(AValues));
end;

{ TArrayHelper }

class procedure TArrayHelper<T>.QuickSort(var AValues: array of T; ALeft,
  ARight: SizeInt; const AComparer: IComparer<T>);
var
  I, J: SizeInt;
  P, Q: T;
begin
  if ((ARight - ALeft) <= 0) or (Length(AValues) = 0) then
    Exit;
  repeat
    I := ALeft;
    J := ARight;
    P := AValues[ALeft + (ARight - ALeft) shr 1];
    repeat
      while AComparer.Compare(AValues[I], P) < 0 do
        Inc(I);
      while AComparer.Compare(AValues[J], P) > 0 do
        Dec(J);
      if I <= J then
      begin
        if I <> J then
        begin
          Q := AValues[I];
          AValues[I] := AValues[J];
          AValues[J] := Q;
        end;
        Inc(I);
        Dec(J);
      end;
    until I > J;
    // sort the smaller range recursively
    // sort the bigger range via the loop
    // Reasons: memory usage is O(log(n)) instead of O(n) and loop is faster than recursion
    if J - ALeft < ARight - I then
    begin
      if ALeft < J then
        QuickSort(AValues, ALeft, J, AComparer);
      ALeft := I;
    end
    else
    begin
      if I < ARight then
        QuickSort(AValues, I, ARight, AComparer);
      ARight := J;
    end;
  until ALeft >= ARight;
end;

class function TArrayHelper<T>.BinarySearch(const AValues: array of T;
  const AItem: T; out ASearchResult: TBinarySearchResult;
  const AComparer: IComparer<T>; AIndex, ACount: SizeInt): Boolean;
var
  imin, imax, imid, ilo: Int32;

begin
  Writeln('Enter ',Length(aValues),' Idx ',aIndex,' acount: ',aCount);
  // continually narrow search until just one element remains
  imin := AIndex;
  imax := Pred(AIndex + ACount);
  Writeln('Start Imax, imin : ',Imax, ' - ', imin);
  ilo:=imid * imid;
  imid:=ilo*imid;
  while (imin < imax) do
  begin
    imid := (imax+imin) div 2;
    writeln('imid',imid);

    ASearchResult.CompareResult := AComparer.Compare(AValues[imid], AItem);
    // reduce the search
    if (ASearchResult.CompareResult < 0) then
      imin := imid + 1
    else
    begin
      if ASearchResult.CompareResult = 0 then
      begin
        ASearchResult.FoundIndex := imid;
        ASearchResult.CandidateIndex := imid;
        Exit(True);
      end;
      imax := imid;
    end;
  end;
  // At exit of while:
  //   if A[] is empty, then imax < imin
  //   otherwise imax == imin

  // deferred test for equality
  Writeln('End Imax, imin : ',Imax, ' - ', imin);
  if (imax = imin) then
  begin
    ASearchResult.CompareResult := AComparer.Compare(AValues[imin], AItem);
    ASearchResult.CandidateIndex := imin;
    if (ASearchResult.CompareResult = 0) then
    begin
      ASearchResult.FoundIndex := imin;
      Exit(True);
    end else
    begin
      ASearchResult.FoundIndex := -1;
      Exit(False);
    end;
  end
  else
  begin
    ASearchResult.CompareResult := 0;
    ASearchResult.FoundIndex := -1;
    ASearchResult.CandidateIndex := -1;
    Exit(False);
  end;
end;

class function TArrayHelper<T>.BinarySearch(const AValues: array of T;
  const AItem: T; out AFoundIndex: SizeInt; const AComparer: IComparer<T>;
  AIndex, ACount: SizeInt): Boolean;
var
  imin, imax, imid: Int32;
  LCompare: SizeInt;
begin
  // continually narrow search until just one element remains
  imin := AIndex;
  imax := Pred(AIndex + ACount);

  // http://en.wikipedia.org/wiki/Binary_search_algorithm
  while (imin < imax) do
  begin
    imid := (imin + imax) div 2;

    // code must guarantee the interval is reduced at each iteration
    // assert(imid < imax);
    // note: 0 <= imin < imax implies imid will always be less than imax

    LCompare := AComparer.Compare(AValues[imid], AItem);
    // reduce the search
    if (LCompare < 0) then
      imin := imid + 1
    else
    begin
      if LCompare = 0 then
      begin
        AFoundIndex := imid;
        Exit(True);
      end;
      imax := imid;
    end;
  end;
  // At exit of while:
  //   if A[] is empty, then imax < imin
  //   otherwise imax == imin

  // deferred test for equality
  LCompare := AComparer.Compare(AValues[imin], AItem);
  if (imax = imin) and (LCompare = 0) then
  begin
    AFoundIndex := imin;
    Exit(True);
  end
  else
  begin
    AFoundIndex := -1;
    Exit(False);
  end;
end;

{ TEnumerator }

function TEnumerator<T>.MoveNext: boolean;
begin
  Exit(DoMoveNext);
end;

{ TEnumerable }

function TEnumerable<T>.GetEnumerator: TMyEnumerator;
begin
  Exit(DoGetEnumerator);
end;

function TEnumerable<T>.ToArray: TMyArray;
var
  LEnumerator: TMyEnumerator;
begin
  Result:=[];
  LEnumerator := GetEnumerator;
  try
    while LEnumerator.MoveNext do
      TJSArray(Result).push(LEnumerator.Current);
  finally
    LEnumerator.Free;
  end;
end;

{ TCustomList }

function TCustomList<T>.GetCapacity: SizeInt;
begin
  Result:=length(FItems);
end;

function TCustomList<T>.PrepareAddingItem: SizeInt;
begin
  if FLength=length(FItems) then
    TJSArray(FItems).push(Default(T));

  Result := FLength;
  Inc(FLength);
end;

function TCustomList<T>.PrepareAddingRange(ACount: SizeInt): SizeInt;
var
  l: SizeInt;
begin
  if ACount < 0 then
    raise EArgumentOutOfRangeException.Create(SArgumentOutOfRange);
  if ACount = 0 then
    Exit(FLength - 1);

  for l:=length(FItems)+1 to FLength+ACount do
    TJSArray(FItems).push(Default(T));

  Result := FLength;
  Inc(FLength, ACount);
end;

procedure TCustomList<T>.Notify(const AValue: T;
  ACollectionNotification: TCollectionNotification);
begin
  if Assigned(FOnNotify) then
    FOnNotify(Self, AValue, ACollectionNotification);
end;

function TCustomList<T>.DoRemove(AIndex: SizeInt;
  ACollectionNotification: TCollectionNotification): T;
begin
  if (AIndex < 0) or (AIndex >= FLength) then
    raise EArgumentOutOfRangeException.Create(SArgumentOutOfRange);

  Result := FItems[AIndex];
  Dec(FLength);

  FItems[AIndex] := Default(T); // needed for refcounted types
  TJSArray(FItems).splice(AIndex,1);

  Notify(Result, ACollectionNotification);
end;

function TCustomList<T>.GetCount: SizeInt;
begin
  Result := FLength;
end;

function TCustomList<T>.ToArray: TArray<T>;
begin
  Result := ToArray;
end;

{ TCustomListEnumerator }

function TCustomListEnumerator<T>.DoMoveNext: boolean;
begin
  Inc(FIndex);
  Result := (FList.FLength > 0) and (FIndex < FList.FLength)
end;

function TCustomListEnumerator<T>.DoGetCurrent: T;
begin
  Result := GetCurrent;
end;

function TCustomListEnumerator<T>.GetCurrent: T;
begin
  Result := FList.FItems[FIndex];
end;

constructor TCustomListEnumerator<T>.Create(AList: TCustomList<T>);
begin
  inherited Create;
  FIndex := -1;
  FList := AList;
end;

{ TList }

procedure TList<T>.SetCapacity(AValue: SizeInt);
begin
  if AValue < Count then
    Count := AValue;

  SetLength(FItems, AValue);
end;

procedure TList<T>.SetCount(AValue: SizeInt);
begin
  if AValue < 0 then
    raise EArgumentOutOfRangeException.Create(SArgumentOutOfRange);

  if AValue > Capacity then
    Capacity := AValue
  else if AValue < Count then
    DeleteRange(AValue, Count - AValue);

  FLength := AValue;
end;

procedure TList<T>.InitializeList;
begin
end;

procedure TList<T>.InternalInsert(AIndex: SizeInt; const AValue: T);
begin
  if (AIndex < 0) or (AIndex > Count) then
    raise EArgumentOutOfRangeException.Create(SArgumentOutOfRange);
  TJSArray(FItems).splice(AIndex,0,AValue);
  inc(FLength);

  FItems[AIndex] := AValue;
  Notify(AValue, cnAdded);
end;

function TList<T>.DoGetEnumerator: TEnumerator<T>;
begin
  Result := GetEnumerator;
end;

function TList<T>.GetItem(AIndex: SizeInt): T;
begin
  if (AIndex < 0) or (AIndex >= Count) then
    raise EArgumentOutOfRangeException.Create(SArgumentOutOfRange);

  Result := FItems[AIndex];
end;

procedure TList<T>.SetItem(AIndex: SizeInt; const AValue: T);
begin
  if (AIndex < 0) or (AIndex >= Count) then
    raise EArgumentOutOfRangeException.Create(SArgumentOutOfRange);
  Notify(FItems[AIndex], cnRemoved);
  FItems[AIndex] := AValue;
  Notify(AValue, cnAdded);
end;

function TList<T>.GetEnumerator: TEnumerator;
begin
  Result := TEnumerator.Create(Self);
end;

constructor TList<T>.Create;
begin
  InitializeList;
  FComparer := TComparer<T>.Default;
end;

constructor TList<T>.Create(const AComparer: IComparer<T>);
begin
  InitializeList;
  FComparer := AComparer;
end;

constructor TList<T>.Create(ACollection: TEnumerable<T>);
var
  LItem: T;
begin
  Create;
  for LItem in ACollection do
    Add(LItem);
end;

destructor TList<T>.Destroy;
begin
  SetCapacity(0);
end;

function TList<T>.Add(const AValue: T): SizeInt;
begin
  Result := PrepareAddingItem;
  FItems[Result] := AValue;
  Notify(AValue, cnAdded);
end;

procedure TList<T>.AddRange(const AValues: array of T);
begin
  InsertRange(Count, AValues);
end;

procedure TList<T>.AddRange(const AEnumerable: IEnumerable<T>);
var
  LValue: T;
begin
  for LValue in AEnumerable do
    Add(LValue);
end;

procedure TList<T>.AddRange(AEnumerable: TEnumerable<T>);
var
  LValue: T;
begin
  for LValue in AEnumerable do
    Add(LValue);
end;

procedure TList<T>.Insert(AIndex: SizeInt; const AValue: T);
begin
  if (AIndex < 0) or (AIndex > Count) then
    raise EArgumentOutOfRangeException.Create(SArgumentOutOfRange);

  InternalInsert(AIndex, AValue);
end;

procedure TList<T>.InsertRange(AIndex: SizeInt; const AValues: array of T);
var
  LLength, i: sizeint;
  LValue: T;
begin
  if (AIndex < 0) or (AIndex > Count) then
    raise EArgumentOutOfRangeException.Create(SArgumentOutOfRange);

  LLength := Length(AValues);
  if LLength = 0 then
    Exit;

  if AIndex <> PrepareAddingRange(LLength) then
  begin
    for i := AIndex to Count - LLength -1 do
      FItems[i+LLength] := FItems[i];
    for i := 0 to LLength -1 do
      FItems[AIndex+i] := Default(T);
  end;

  for i := 0 to Pred(LLength) do
  begin
    LValue:=AValues[i];
    FItems[i+AIndex] := LValue;
    Notify(LValue, cnAdded);
  end;
end;

procedure TList<T>.InsertRange(AIndex: SizeInt; const AEnumerable: IEnumerable<T>);
var
  LValue: T;
  i: SizeInt;
begin
  if (AIndex < 0) or (AIndex > Count) then
    raise EArgumentOutOfRangeException.Create(SArgumentOutOfRange);

  i := 0;
  for LValue in AEnumerable do
  begin
    InternalInsert(AIndex + i, LValue);
    Inc(i);
  end;
end;

procedure TList<T>.InsertRange(AIndex: SizeInt; const AEnumerable: TEnumerable<T>);
var
  LValue: T;
  i:  SizeInt;
begin
  if (AIndex < 0) or (AIndex > Count) then
    raise EArgumentOutOfRangeException.Create(SArgumentOutOfRange);

  i := 0;
  for LValue in AEnumerable do
  begin
    InternalInsert(Aindex + i, LValue);
    Inc(i);
  end;
end;

function TList<T>.Remove(const AValue: T): SizeInt;
begin
  Result := IndexOf(AValue);
  if Result >= 0 then
    DoRemove(Result, cnRemoved);
end;

procedure TList<T>.Delete(AIndex: SizeInt);
begin
  DoRemove(AIndex, cnRemoved);
end;

procedure TList<T>.DeleteRange(AIndex, ACount: SizeInt);
var
  LDeleted: TMyArray;
  i: SizeInt;
begin
  if ACount = 0 then
    Exit;

  if (ACount < 0) or (AIndex < 0) or (AIndex + ACount > Count) then
    raise EArgumentOutOfRangeException.Create(SArgumentOutOfRange);

  LDeleted:=TMyArray(TJSArray(FItems).splice(AIndex,Count));
  Dec(FLength, ACount);

  for i := 0 to High(LDeleted) do
    Notify(LDeleted[i], cnRemoved);
end;

function TList<T>.ExtractIndex(const AIndex: SizeInt): T;
begin
  Result := DoRemove(AIndex, cnExtracted);
end;

function TList<T>.Extract(const AValue: T): T;
var
  LIndex: SizeInt;
begin
  LIndex := IndexOf(AValue);
  if LIndex < 0 then
    Exit(Default(T));

  Result := DoRemove(LIndex, cnExtracted);
end;

procedure TList<T>.Exchange(AIndex1, AIndex2: SizeInt);
var
  LTemp: T;
begin
  LTemp := FItems[AIndex1];
  FItems[AIndex1] := FItems[AIndex2];
  FItems[AIndex2] := LTemp;
end;

procedure TList<T>.Move(AIndex, ANewIndex: SizeInt);
var
  Arr: TJSArray;
  LTemp: JSValue;
  i: SizeInt;
begin
  if ANewIndex = AIndex then
    Exit;

  if (ANewIndex < 0) or (ANewIndex >= Count) then
    raise EArgumentOutOfRangeException.Create(SArgumentOutOfRange);

  Arr := TJSArray(FItems);
  LTemp := Arr[AIndex];
  if AIndex < ANewIndex then
    for i := AIndex to ANewIndex-1 do
      Arr[i] := Arr[i+1]
  else
    for i := ANewIndex downto AIndex+1 do
      Arr[i] := Arr[i-1];
  Arr[ANewIndex] := LTemp;
end;

function TList<T>.First: T;
begin
  Result := Items[0];
end;

function TList<T>.Last: T;
begin
  Result := Items[Pred(Count)];
end;

procedure TList<T>.Clear;
begin
  SetCount(0);
  SetCapacity(0);
end;

function TList<T>.Contains(const AValue: T): Boolean;
begin
  Result := IndexOf(AValue) >= 0;
end;

function TList<T>.IndexOf(const AValue: T): SizeInt;
var
  i: SizeInt;
begin
  for i := 0 to Count - 1 do
    if FComparer.Compare(AValue, FItems[i]) = 0 then
      Exit(i);
  Result := -1;
end;

function TList<T>.LastIndexOf(const AValue: T): SizeInt;
var
  i: SizeInt;
begin
  for i := Count - 1 downto 0 do
    if FComparer.Compare(AValue, FItems[i]) = 0 then
      Exit(i);
  Result := -1;
end;

procedure TList<T>.Reverse;
var
  a, b: SizeInt;
  LTemp: T;
begin
  a := 0;
  b := Count - 1;
  while a < b do
  begin
    LTemp := FItems[a];
    FItems[a] := FItems[b];
    FItems[b] := LTemp;
    Inc(a);
    Dec(b);
  end;
end;

procedure TList<T>.TrimExcess;
begin
  SetCapacity(Count);
end;

procedure TList<T>.Sort;
begin
  TMyArrayHelper.Sort(FItems, FComparer, 0, Count);
end;

procedure TList<T>.Sort(const AComparer: IComparer<T>);
begin
  TMyArrayHelper.Sort(FItems, AComparer, 0, Count);
end;

function TList<T>.BinarySearch(const AItem: T; out AIndex: SizeInt): Boolean;
begin
  Result := TMyArrayHelper.BinarySearch(FItems, AItem, AIndex, FComparer, 0, Count);
end;

function TList<T>.BinarySearch(const AItem: T; out AIndex: SizeInt;
  const AComparer: IComparer<T>): Boolean;
begin
  Result := TMyArrayHelper.BinarySearch(FItems, AItem, AIndex, AComparer, 0, Count);
end;

end.
