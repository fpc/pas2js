unit localjsondataset;

{$mode ObjFPC}

interface

uses
  Classes, SysUtils, db, jsondataset, js, web;

Type
  { TCustomLocalJSONDataset }
  TStorageKind = (skLocal,skSession);
  TStorageOption = (soAutoLoad,soAutoSave);
  TStorageOptions = Set of TStorageOption;

  TCustomLocalJSONDataset = class(TJSONDataset)
  private
    FDataElement: String;
    FStorageKey: String;
    FStorageKind: TStorageKind;
    FStorageOptions: TStorageOptions;
    function GetAutoLoad: Boolean;
    function GetAutoSave: Boolean;
    function GetStorage: TJSStorage;
    function GetStorageKey: String;
    function IsKeyStored: Boolean;
    procedure SetDataElement(AValue: String);
    procedure SetStorageKey(AValue: String);
    procedure SetStorageKind(AValue: TStorageKind);
    procedure SetStorageOptions(AValue: TStorageOptions);
  Protected
    Procedure InternalOpen; override;
    Procedure InternalClose; override;
    Property Storage : TJSStorage Read GetStorage;
    Property AutoLoad : Boolean Read GetAutoLoad;
    Property AutoSave : Boolean Read GetAutoSave;
  Public
    Procedure LoadDataFromStorage;
    Procedure SaveDataToStorage;
    Property Rows;
    Property DataElement : String Read FDataElement Write SetDataElement;
    Property StorageKey : String Read GetStorageKey Write SetStorageKey stored IsKeyStored;
    Property StorageKind : TStorageKind Read FStorageKind Write SetStorageKind;
    Property Options : TStorageOptions Read FStorageOptions Write SetStorageOptions;
  end;

  TLocalJSONDataset = Class(TCustomLocalJSONDataset)
  Published
    Property DataElement;
    Property AutoLoad;
    Property StorageKey;
    Property StorageKind;
    Property Options;
  end;


implementation

resourcestring
  SErrStorageNotArray = 'Storage data %s is not an array';
  SErrElementNotArray = 'Storage object property "%s" is not an array';

{ TCustomLocalJSONDataset }

procedure TCustomLocalJSONDataset.SetStorageKey(AValue: String);
begin
  if FStorageKey=AValue then Exit;
  FStorageKey:=AValue;
end;

procedure TCustomLocalJSONDataset.SetStorageKind(AValue: TStorageKind);
begin
  if FStorageKind=AValue then Exit;
  CheckInactive;
  FStorageKind:=AValue;
end;

procedure TCustomLocalJSONDataset.SetStorageOptions(AValue: TStorageOptions);
begin
  if FStorageOptions=AValue then Exit;
  CheckInactive;
  FStorageOptions:=AValue;
end;

procedure TCustomLocalJSONDataset.InternalOpen;
begin
  if AutoLoad then
    LoadDataFromStorage;
  inherited InternalOpen;
end;

procedure TCustomLocalJSONDataset.InternalClose;
begin
  if AutoSave then
    SaveDataToStorage;
  inherited InternalClose;
end;


function TCustomLocalJSONDataset.GetStorage: TJSStorage;
begin
  if FStorageKind=skLocal then
    Result:=Window.localStorage
  else
    Result:=Window.SessionStorage;
end;

function TCustomLocalJSONDataset.GetAutoLoad: Boolean;
begin
  Result:=soAutoLoad in FStorageOptions;
end;

function TCustomLocalJSONDataset.GetAutoSave: Boolean;
begin
  Result:=soAutoSave in FStorageOptions;
end;

function TCustomLocalJSONDataset.GetStorageKey: String;
begin
  Result:=FStorageKey;
  if Result='' then
    Result:=Name;
  if Result='' then
    begin
    // Allocate one
    FStorageKey:=ClassName+IntToStr(TJSDate.New.Time);
    Result:=FStorageKey;
    end;
end;

function TCustomLocalJSONDataset.IsKeyStored: Boolean;
begin
  Result:=FStorageKey<>Name;
end;


procedure TCustomLocalJSONDataset.SetDataElement(AValue: String);
begin
  if FDataElement=AValue then Exit;
  CheckInactive;
  FDataElement:=AValue;
end;


procedure TCustomLocalJSONDataset.LoadDataFromStorage;

Var
  S,P : String;
  value : JSValue;

begin
  Rows:=TJSArray.New;
  S:=Storage.getItem(StorageKey);
  if (S<>'') then
    begin
    value:=TJSJSON.parse(S);
    if isArray(Value) then
      Rows:=TJSArray(value)
    else if not isObject(Value) then
      DatabaseErrorFmt(SErrStorageNotArray,[StorageKey],Self)
    else
      begin
      P:=DataElement;
      if P='' then
        P:='data';
      Value:=TJSObject(Value)[p];
      if not IsArray(Value) then
        DatabaseErrorFmt(SErrElementNotArray,[P],Self);
      Rows:=TJSArray(Value);
      end;
    end;
end;

procedure TCustomLocalJSONDataset.SaveDataToStorage;

Var
  S : String;
  J : TJSObject;
begin
  if (DataElement<>'') then
    begin
    J:=TJSObject.New;
    J[DataElement]:=Rows;
    S:=TJSJSON.Stringify(J);
    end
  else
    S:=TJSJSON.Stringify(Rows);
  Storage.setItem(StorageKey,S);
end;


end.

