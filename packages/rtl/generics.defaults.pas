unit Generics.Defaults;

{$MODE DELPHI}

interface

//uses Classes;

type
  TArray<T> = array of T;

  { IComparer }

  IComparer<T> = interface
    function Compare(const Left, Right: T): Integer; overload;
  end;

  TOnComparison<T> = function(const Left, Right: T): Integer of object;
  TComparisonFunc<T> = function(const Left, Right: T): Integer;

  { TComparer }

  TComparer<T> = class(TInterfacedObject, IComparer<T>)
  private
    type TMyComparer = TComparer<T>;
    class var DefaultComparer: TMyComparer;
  public
    class function Default: IComparer<T>; static;
    function Compare(const ALeft, ARight: T): Integer; virtual; abstract; overload;

    class function Construct(const AComparison: TOnComparison<T>): IComparer<T>; overload;
    class function Construct(const AComparison: TComparisonFunc<T>): IComparer<T>; overload;
  end;

  { TDefaultComparer }

  TDefaultComparer<T> = class(TComparer<T>)
  public
    function Compare(const ALeft, ARight: T): Integer; override; overload;
  end;

  { TDelegatedComparerEvents }

  TDelegatedComparerEvents<T> = class(TComparer<T>)
  private
    FComparison: TOnComparison<T>;
  public
    function Compare(const ALeft, ARight: T): Integer; override;
    constructor Create(AComparison: TOnComparison<T>);
  end;

  { TDelegatedComparerFunc }

  TDelegatedComparerFunc<T> = class(TComparer<T>)
  private
    FComparison: TComparisonFunc<T>;
  public
    function Compare(const ALeft, ARight: T): Integer; override;
    constructor Create(AComparison: TComparisonFunc<T>);
  end;

  IEnumerator<T> = interface
    function GetCurrent: T;
    function MoveNext: Boolean;
    procedure Reset;
    property Current: T read GetCurrent;
  end;

  IEnumerable<T> = interface
    function GetEnumerator: IEnumerator<T>;
  end;

implementation

{ TComparer }

class function TComparer<T>.Default: IComparer<T>;
begin
  if DefaultComparer=nil then
    DefaultComparer:=TDefaultComparer<T>.Create;
  Result:=DefaultComparer;
end;

class function TComparer<T>.Construct(const AComparison: TOnComparison<T>
  ): IComparer<T>;
begin
  Result := TDelegatedComparerEvents<T>.Create(AComparison);
end;

class function TComparer<T>.Construct(const AComparison: TComparisonFunc<T>
  ): IComparer<T>;
begin
  Result := TDelegatedComparerFunc<T>.Create(AComparison);
end;

{ TDefaultComparer }

function TDefaultComparer<T>.Compare(const ALeft, ARight: T): Integer;
begin
  asm
    if (ALeft < ARight) return -1;
    if (ALeft > ARight) return 1;
  end;
  Result:=0;
  if ALeft = ARight then exit;
end;

{ TDelegatedComparerEvents }

function TDelegatedComparerEvents<T>.Compare(const ALeft, ARight: T
  ): Integer;
begin
  Result := FComparison(ALeft, ARight);
end;

constructor TDelegatedComparerEvents<T>.Create(AComparison: TOnComparison<T>);
begin
  FComparison := AComparison;
end;

{ TDelegatedComparerFunc }

function TDelegatedComparerFunc<T>.Compare(const ALeft, ARight: T): Integer;
begin
  Result := FComparison(ALeft, ARight);
end;

constructor TDelegatedComparerFunc<T>.Create(AComparison: TComparisonFunc<T>);
begin
  FComparison := AComparison;
end;

end.

