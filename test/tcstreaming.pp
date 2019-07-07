{$mode objfpc}
{$h+}
unit tcstreaming;

interface

Uses
  SysUtils,Classes, fpcunit, testregistry;

Type

  { TTestStreaming }

  TTestStreaming = Class(TTestCase)
  Private
    FStream : TMemoryStream;
    Function ReadByte : byte;
    Function ReadWord : Word;
    Function ReadInteger : LongInt;
    Function ReadNativeInt : NativeInt;
    function ReadBareStr: string;
    function ReadString(V : TValueType): string;
    function ReadWideString(V : TValueType): WideString;
    Procedure Fail(Fmt : String; Args : Array of JSValue); overload;
  Public
    Procedure Setup; override;
    Procedure TearDown; override;
    Procedure ResetStream;
    Procedure SaveToStream(C : TComponent);
    Procedure LoadFromStream(C : TComponent);
    Function ReadValue : TValueType;
    Procedure ExpectValue(AValue : TValueType);
    Procedure ExpectFlags(Flags : TFilerFlags; APosition : Integer);
    Procedure ExpectInteger(AValue : Integer);
    Procedure ExpectByte(AValue : Byte);
    Procedure ExpectInt64(AValue : Int64);
    Procedure ExpectBareString(AValue : String);
    Procedure ExpectString(AValue : String);
    Procedure ExpectSingle(AValue : Single);
    Procedure ExpectExtended(AValue : Extended);
    Procedure ExpectCurrency(AValue : Currency);
    Procedure ExpectIdent(AValue : String);
    Procedure ExpectDate(AValue : TDateTime);
    Procedure ExpectWideString(AValue : WideString);
    Procedure ExpectEndofList;
    Procedure ExpectSignature;
    Procedure ExpectEndOfStream;
  end;

implementation

uses typinfo;

Function ValName(V : TValueType) : String;

begin
  Result:=GetEnumName(TypeInfo(TValueType),Ord(v));
end;

{ TTestStreaming }


procedure TTestStreaming.ExpectByte(AValue: Byte);

Var
  B : Byte;

begin
  B:=ReadByte;
  If (B<>AValue) then
    Fail('Expected byte %d, got %d',[AValue,B]);
end;

procedure TTestStreaming.ExpectCurrency(AValue: Currency);

Var
  C : Double;

begin
  ExpectValue(vaCurrency);
  FStream.ReadBufferData(C);
  If (C<>AValue) then
    Fail('Expected currency %f, got %f',[AValue,C]);
end;

procedure TTestStreaming.ExpectDate(AValue: TDateTime);

Var
  C : TDateTime;

begin
  ExpectValue(vaDate);
  FStream.ReadBufferData(C);
  If (C<>AValue) then
    Fail('Expected datetime %f, got %f',[AValue,C]);
end;

procedure TTestStreaming.ExpectEndofList;
begin
  ExpectValue(vaNull);
end;

procedure TTestStreaming.ExpectExtended(AValue: Extended);

Var
  E : Extended;

begin
  ExpectValue(vaExtended);
  FStream.ReadBufferData(E);
  If Abs(E-AValue)>0.01 then
    Fail('Expected extended %f, got %f',[AValue,E]);
end;

procedure TTestStreaming.ExpectFlags(Flags: TFilerFlags;
  APosition: Integer);

var
  FF : TFilerFlag;
  F : TFilerFlags;
  B : Byte;
  I : Integer;

begin
  F := [];
  I:=0;
  B:=ReadByte;
  if (B and $F0) = $F0 then
    begin
    F:=[];
    for FF in TFilerFlag do
      if (B and (1 shl ord(FF)))<>0 then
         Include(F,FF);
    if ffChildPos in Flags then
      I:=ReadInteger;
    end
  else
    FStream.Position:=FStream.Position-1;
  If (FLags<>F) then
    Fail('Wrong Flags');
  If I<>APosition then
    Fail('Wrong position, expected %d, got %d',[APosition,I]);
end;

procedure TTestStreaming.ExpectIdent(AValue: String);

var
  I,L : Byte;
  V : TValueType;
  S : String;
  C : Char;
begin
  V:=ReadValue;
  case V of
    vaIdent:
      begin
      L:=ReadByte;
      SetLength(S,L);
      for I:=1 to L do
        begin
        FStream.ReadBufferData(C);
        S[i]:=C;
        end;
      end;
    vaFalse:
      S := 'False';
    vaTrue:
      S := 'True';
    vaNil:
      S := 'nil';
    vaNull:
      S := 'Null';
  else
    Fail(Format('Expected identifier property type, got %s',[valName(V)]));
  end;
  If (S<>AValue) then
    Fail(Format('Wrong identifier %s, expected %s',[S,AValue]));
end;

procedure TTestStreaming.ExpectInt64(AValue: Int64);

Var
  V : TValueType;
  I : Int64;

begin
  V:=ReadValue;
  Case V of
    vaInt8  : I:=ReadByte;
    vaInt16 : I:=ReadWord;
    vaInt32 : I:=ReadInteger;
    vaInt64 : I:=ReadNativeInt;
  else
    Fail(Format('Expected integer property type, got %s',[valName(V)]));
  end;
  If (AValue<>I) then
    Fail(Format('Expected integer %d, but got %d',[AValue,I]));
end;

procedure TTestStreaming.ExpectInteger(AValue: Integer);

Var
  V : TValueType;
  I : Integer;

begin
  V:=ReadValue;
  Case V of
    vaInt8  : I:=ReadByte;
    vaInt16 : I:=ReadWord;
    vaInt32 : I:=ReadInteger;
  else
    Fail('Expected integer  property type, got %s',[valName(V)]);
  end;
  If (AValue<>I) then
    Fail('Expected integer %d, but got %d',[AValue,I]);
end;



procedure TTestStreaming.ExpectSignature;

const
  // Sig : array[1..4] of Char = 'TPF0';
  // Integer version of 4 chars 'TPF0'
  FilerSignatureInt = 809914452;

var
  E,L : Longint;

begin
  L:=ReadInteger;
  E:=FilerSignatureInt;
  if L<>E then
    Fail('Invalid signature %d, expected %d',[L,E]);
end;

procedure TTestStreaming.ExpectSingle(AValue: Single);

Var
  S : Double;

begin
  ExpectValue(vaSingle);
  FStream.ReadBufferData(S);
  If Abs(AValue-S)>0.0001 then
    Fail('Expected single %f, but got %s',[AValue,S]);
end;

function TTestStreaming.ReadString(V : TValueType): string;

var
  L,I : Integer;
  C : Char;

begin
  // There is only 1 string type
  if V<>vaString then
    Fail('Wrong type %s, expected string type.',[ValName(V)]);
  L := 0;
  FStream.ReadBufferData(L);
  SetLength(Result, L);
  For I:=1 to L do
    begin
    FStream.ReadBufferData(C);
    Result[i]:=C;
    end;
end;

function TTestStreaming.ReadWideString(V : TValueType): WideString;

begin
  Result := ReadString(V)
end;

procedure TTestStreaming.ExpectString(AValue: String);

Var
  V : TValueType;
  S : String;
begin
  V:=ReadValue;
  If v in [vaString,vaLstring,vaWString,vaUTF8String] then
    S:=ReadString(V)
  else
    Fail('Expected string type, but got : %s',[ValName(V)]);
  If (S<>AValue) then
    Fail('Expected string "%s", but got "%s"',[AVAlue,S]);
end;

procedure TTestStreaming.ExpectValue(AValue: TValueType);

Var
  V : TValueType;

begin
  V:=ReadValue;
  If (V<>AValue) then
    Fail('Expecting value %s, but read %s',[ValName(AValue),ValName(V)]);
end;

procedure TTestStreaming.ExpectWideString(AValue: WideString);

Var
  W : WideString;
  V : TValueType;

begin
  V:=ReadValue;
  If v in [vaString,vaLstring,vaWString,vaUTF8String] then
    W:=ReadWideString(V)
  else
    Fail('Expected string type, but got : %s',[ValName(V)]);
  If (W<>AValue) then
    Fail('Expected string "%s", but got "%s"',[AVAlue,W]);
end;


procedure TTestStreaming.Fail(Fmt: String; Args: array of jsvalue);
begin
  Fail(Format(Fmt,Args));
end;

function TTestStreaming.ReadValue: TValueType;

var b : byte;

begin
  FStream.ReadBufferData(b);
  result := TValueType(b);
end;

procedure TTestStreaming.Setup;
begin
  FStream:=TMemoryStream.Create;
end;

procedure TTestStreaming.SaveToStream(C: TComponent);
begin
  C.Name:='Test'+C.ClassName;
  FStream.Clear;
  FStream.WriteComponent(C);
  FStream.Position:=0;
end;

procedure TTestStreaming.LoadFromStream(C: TComponent);
begin
  ResetStream;
  FStream.ReadComponent(C);
end;

procedure TTestStreaming.TearDown;
begin
  FreeAndNil(FStream);
end;

procedure TTestStreaming.ResetStream;
begin
  FStream.Position:=0;
end;

function TTestStreaming.ReadByte: byte;
begin
  FStream.ReadBufferData(Result);
end;

function TTestStreaming.ReadNativeInt: NativeInt;

begin
  FStream.ReadBufferData(Result);
end;

function TTestStreaming.ReadInteger: LongInt;
begin
  FStream.ReadBufferData(Result);
end;

function TTestStreaming.ReadWord: Word;
begin
  FStream.ReadBufferData(Result);
end;

function TTestStreaming.ReadBareStr: string;

var
  L,I : Integer;
  C : Char;

begin
  L:=ReadByte;
  SetLength(Result,L);
  for I:=1 to L do
    begin
    FStream.ReadBufferData(C);
    Result[I]:=C;
    end;
end;

procedure TTestStreaming.ExpectBareString(AValue: String);

Var
  S : String;

begin
  S:=ReadBareStr;
  If (S<>AValue) then
    Fail('Expected bare string %s, got :%s',[AValue,S]);
end;

procedure TTestStreaming.ExpectEndOfStream;
begin
  If (FStream.Position<>FStream.Size) then
    Fail('Expected at end of stream, current position=%d, size=%d',
          [FStream.Position,FStream.Size]);
end;

end.
