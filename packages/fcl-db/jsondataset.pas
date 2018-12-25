{$mode objfpc}

unit JSONDataset;

interface

uses
  Types, JS, DB, Classes, SysUtils, typinfo;

type
  TBaseJSONDataset = Class;

  { TJSONFieldMapper }
  // This class is responsible for mapping the field objects of the records.
  TJSONFieldMapper = Class(TObject)
  Public
    // Return row TJSONData instance with data for field 'FieldName' or 'FieldIndex'.
    Function GetJSONDataForField(Const FieldName : String; FieldIndex : Integer; Row : JSValue) : JSValue; virtual; abstract;
    // Same, but now based on TField.
    Function GetJSONDataForField(F : TField; Row : JSValue) : JSValue; virtual;
    // Set data for field 'FieldName' or 'FieldIndex' to supplied TJSONData instance in row
    procedure SetJSONDataForField(Const FieldName : String; FieldIndex : Integer; Row,Data : JSValue); virtual; abstract;
    // Set data for field TField to supplied TJSONData instance
    procedure SetJSONDataForField(F : TField; Row,Data : JSValue); virtual;
    // Create a new row.
    Function CreateRow : JSValue; virtual; abstract;
  end;

  // JSON has no date/time type, so we use a string field.
  // ExtJS provides the date/time  format in it's field config: 'dateFormat'
  // The below field classes store this in the NNNFormat field.
  { TJSONDateField }

  TJSONDateField = Class(TDateField)
  private
    FDateFormat: String;
  Published
    Property DateFormat : String Read FDateFormat Write FDateFormat;
  end;

  { TJSONTimeField }

  TJSONTimeField = Class(TTimeField)
  private
    FTimeFormat: String;
  Published
    Property TimeFormat : String Read FTimeFormat Write FTimeFormat;
  end;

  { TJSONDateTimeField }

  TJSONDateTimeField = Class(TDateTimeField)
  private
    FDateTimeFormat: String;
  Published
    Property DateTimeFormat : String Read FDateTimeFormat Write FDateTimeFormat;
  end;

  { TFieldComparer }

  TFieldComparer = Class
  Private
    FValue : JSValue;
    FField : TField;
    FOptions : TLocateOptions;
    FDataset : TBaseJSONDataset;
  Public
    Constructor Create(aDataset : TBaseJSONDataset; aField : TField; aValue : JSValue; aOptions : TLocateOptions);
    Function GetFieldValue(RowIndex : integer) : JSValue;
    // First value is always dataset value.
    Function Compare (RowIndex : Integer; aValue : JSValue) : Integer; virtual; abstract;
    Function Compare (RowIndex : Integer) : Integer; virtual;
    Property Value : JSValue read FValue Write FValue;
    Property Options : TLocateOptions Read FOptions;
    Property Dataset : TBaseJSONDataset Read FDataset;
    Property Field : TField Read FField;
  end;
  TFieldComparerClass = Class of TFieldComparer;

  { TStringFieldComparer }

  TStringFieldComparer = Class (TFieldComparer)
    Function Compare (RowIndex : Integer; aValue : JSValue) : Integer; override;
  end;

  { TNativeIntFieldComparer }

  TNativeIntFieldComparer = Class (TFieldComparer)
    Function Compare (RowIndex : Integer; aValue : JSValue) : Integer; override;
  end;

  { TBooleanFieldComparer }

  TBooleanFieldComparer = Class (TFieldComparer)
    Function Compare (RowIndex : Integer; aValue : JSValue) : Integer; override;
  end;

  { TDateTimeFieldComparer }

  TDateTimeFieldComparer = Class (TFieldComparer)
    Function Compare (RowIndex : Integer; aValue : JSValue) : Integer; override;
  end;

  { TFloatFieldComparer }

  TFloatFieldComparer = Class (TFieldComparer)
    Function Compare (RowIndex : Integer; aValue : JSValue) : Integer; override;
  end;

  { TRecordComparer }

  TRecordComparer = class
  private
    FDataset: TBaseJSONDataset;
    FItems : Array of TFieldComparer;
    FOptions: TLocateOptions;
    FValues: TJSValueDynArray;
    function GetFieldComparer(Index : Integer): TFieldComparer;
  Protected
    procedure ConstructItems(aFields: String); virtual;
    function DataTypeToComparerClass(aFieldType: TFieldType): TFieldComparerClass;
    Function Compare(aRowindex : integer) : Integer;
  Public
    Constructor Create(aDataset : TBaseJSONDataset; aFields : String; aValues : JSValue; aOptions : TLocateOptions);
    Property Dataset : TBaseJSONDataset Read FDataset;
    property Items [Index : Integer] : TFieldComparer Read GetFieldComparer;
    Property Options : TLocateOptions Read FOptions Write FOptions;
    Property Values : TJSValueDynArray Read FValues;
  end;

  { TBaseJSONDataSet }

  { TJSONIndex }

  TJSONIndex = Class
    FList : TJSArray; // Indexes of elements in FRows.
    FRows : TJSArray;
    FDataset : TDataset;
  private
    function GetRecordIndex(aListIndex : Integer): NativeInt;
  protected
    Function GetCount: Integer; virtual;
    Procedure CreateIndex; Virtual; abstract;
    Property List : TJSArray Read FList;
    Property Rows : TJSArray Read FRows;
    Property Dataset : TDataset Read FDataset;
  Public
    Constructor Create(aDataset: TDataset; aRows : TJSArray); reintroduce;
    // Append remainder of FRows to FList.
    Procedure AppendToIndex; virtual; abstract;
    // Delete aListIndex from list, not from row. Return Recordindex of deleted record.
    Function Delete(aListIndex : Integer) : Integer; virtual;
    // Append aRecordIndex to list. Return ListIndex of appended record.
    Function Append(aRecordIndex : Integer) : Integer; virtual; abstract;
    // Insert record into list. By default, this does an append. Return ListIndex of inserted record
    Function Insert(aCurrentIndex{%H-}, aRecordIndex : Integer) : Integer; virtual;
    // Record at index aCurrentIndex has changed. Update index and return new listindex.
    Function Update(aCurrentIndex, aRecordIndex : Integer) : Integer; virtual; abstract;
    // Find list index for Record at index aCurrentIndex. Return -1 if not found.
    Function FindRecord(aRecordIndex : Integer) : Integer; virtual; abstract;
    // index of record in FRows based on aListIndex in List.
    Property RecordIndex[aListIndex : Integer] : NativeInt Read GetRecordIndex;
    // Number of records in index. This can differ from FRows, e.g. when filtering.
    Property Count : Integer Read GetCount;
  end;

  { TDefaultJSONIndex }

  TDefaultJSONIndex = Class(TJSONIndex)
  public
    Procedure CreateIndex; override;
    Procedure AppendToIndex; override;
    Function Append(aRecordIndex : Integer) : Integer; override;
    Function Insert(aCurrentIndex, aRecordIndex : Integer) : Integer; override;
    Function FindRecord(aRecordIndex : Integer) : Integer; override;
    Function Update(aCurrentIndex, aRecordIndex : Integer) : Integer; override;
  end;

  // basic JSON dataset. Does nothing ExtJS specific.
  TBaseJSONDataSet = class (TDataSet)
  private
    FMUS: Boolean;
    FOwnsData : Boolean;
    FDefaultIndex : TJSONIndex; // Default index, built from array
    FCurrentIndex : TJSONIndex; // Currently active index.
    FCurrent: Integer; // Record Index in the current IndexList
    // Possible metadata to configure fields from.
    FMetaData : TJSObject;
    // This will contain the rows.
    FRows : TJSArray;
    // Deleted rows
    FDeletedRows : TJSArray;
    FFieldMapper : TJSONFieldMapper;
    // When editing, this object is edited.
    FEditIdx : Integer;
    FEditRow : JSValue;
    FUseDateTimeFormatFields: Boolean;
    procedure SetMetaData(AValue: TJSObject);
    procedure SetRows(AValue: TJSArray);
  protected
    // Return index of Row in FRows matching keyfields/values. If not found, -1 is returned.
    function LocateRecordIndex(const KeyFields: string; const KeyValues: JSValue; Options: TLocateOptions): Integer;
    // dataset virtual methods
    function AllocRecordBuffer: TDataRecord; override;
    procedure FreeRecordBuffer(var Buffer: TDataRecord); override;
    procedure InternalInitRecord(var Buffer: TDataRecord); override;
    function GetRecord(Var Buffer: TDataRecord; GetMode: TGetMode; DoCheck{%H-}: Boolean): TGetResult; override;
    function GetRecordSize: Word; override;
    procedure AddToRows(AValue: TJSArray);
    procedure InternalClose; override;
    procedure InternalDelete; override;
    procedure InternalFirst; override;
    procedure InternalLast; override;
    procedure InternalOpen; override;
    procedure InternalPost; override;
    procedure InternalInsert; override;
    procedure InternalEdit; override;
    procedure InternalCancel; override;
    procedure InternalInitFieldDefs; override;
    procedure InternalSetToRecord(Buffer: TDataRecord); override;
    function  GetFieldClass(FieldType: TFieldType): TFieldClass; override;
    function IsCursorOpen: Boolean; override;
    // Bookmark operations
    procedure GetBookmarkData(Buffer: TDataRecord; var Data: TBookmark); override;
    function GetBookmarkFlag(Buffer: TDataRecord): TBookmarkFlag; override;
    procedure InternalGotoBookmark(ABookmark: TBookmark); override;
    procedure SetBookmarkFlag(Var Buffer: TDataRecord; Value: TBookmarkFlag); override;
    procedure SetBookmarkData(Var Buffer: TDataRecord; Data: TBookmark); override;
    function GetRecordCount: Integer; override;
    procedure SetRecNo(Value: Integer); override;
    function GetRecNo: Integer; override;
  Protected
    // New methods.
    // Called when dataset is closed. If OwnsData is true, metadata and rows are freed.
    Procedure FreeData; virtual;
    // Fill default list.
    procedure AppendToIndexes; virtual;
    Procedure CreateIndexes; virtual;
    // Convert MetaData object to FieldDefs.
    Procedure MetaDataToFieldDefs; virtual; abstract;
    // Initialize Date/Time info in all date/time fields. Called during InternalOpen
    procedure InitDateTimeFields; virtual;
    // Convert JSON date S to DateTime for Field F
    function ConvertDateTimeField(S: String; F: TField): TDateTime; virtual;
    // Format JSON date to from DT for Field F
    function FormatDateTimeField(DT : TDateTime; F: TField): String; virtual;
    // Create fieldmapper. A descendent MUST implement this.
    Function CreateFieldMapper : TJSONFieldMapper; virtual; abstract;
    // If True, then the dataset will free MetaData and FRows when it is closed.
    Property OwnsData : Boolean Read FownsData Write FOwnsData;
    // set to true if unknown field types should be handled as string fields.
    Property MapUnknownToStringType : Boolean Read FMUS Write FMUS;
    // Metadata
    Property MetaData : TJSObject Read FMetaData Write SetMetaData;
    // Rows
    Property Rows : TJSArray Read FRows Write SetRows;
    // Fieldmapper
    Property FieldMapper : TJSONFieldMapper Read FFieldMapper;
    // FieldClass
    Property UseDateTimeFormatFields : Boolean Read FUseDateTimeFormatFields Write FUseDateTimeFormatFields;
  public
    constructor Create (AOwner: TComponent); override;
    destructor Destroy; override;
    function Locate(const KeyFields: string; const KeyValues: JSValue; Options: TLocateOptions): boolean; override;
    function Lookup(const KeyFields: string; const KeyValues: JSValue; const ResultFields: string): JSValue; override;
    function GetFieldData(Field: TField; Buffer: TDatarecord): JSValue;  override;
    procedure SetFieldData(Field: TField; var Buffer{%H-}: TDatarecord; AValue : JSValue);  override;
    function BookmarkValid(ABookmark: TBookmark): Boolean; override;
    function CompareBookmarks(Bookmark1, Bookmark2: TBookmark): Longint; override;
  end;

  TJSONDataset = Class(TBaseJSONDataset)
  published
    Property FieldDefs;
    // redeclared data set properties
    property Active;
    property BeforeOpen;
    property AfterOpen;
    property BeforeClose;
    property AfterClose;
    property BeforeInsert;
    property AfterInsert;
    property BeforeEdit;
    property AfterEdit;
    property BeforePost;
    property AfterPost;
    property BeforeCancel;
    property AfterCancel;
    property BeforeDelete;
    property AfterDelete;
    property BeforeScroll;
    property AfterScroll;
    property OnCalcFields;
    property OnDeleteError;
    property OnEditError;
    property OnFilterRecord;
    property OnNewRecord;
    property OnPostError;
  end;

  { TJSONObjectFieldMapper }
  // Fieldmapper to be used when the data is in an object
  TJSONObjectFieldMapper = Class(TJSONFieldMapper)
  Public
    procedure SetJSONDataForField(Const FieldName : String; FieldIndex{%H-} : Integer; Row,Data : JSValue); override;
    Function GetJSONDataForField(Const FieldName : String; FieldIndex{%H-} : Integer; Row : JSValue) : JSValue; override;
    Function CreateRow : JSValue; override;
  end;

  { TJSONArrayFieldMapper }
  // Fieldmapper to be used when the data is in an array
  TJSONArrayFieldMapper = Class(TJSONFieldMapper)
  Public
    procedure SetJSONDataForField(Const FieldName{%H-} : String; FieldIndex : Integer; Row,Data : JSValue); override;
    Function GetJSONDataForField(Const FieldName{%H-} : String; FieldIndex : Integer; Row : JSValue) : JSValue; override;
    Function CreateRow : JSValue; override;
  end;

  EJSONDataset = Class(EDatabaseError);
  
implementation

uses DateUtils;

{ TFloatFieldComparer }

function TFloatFieldComparer.Compare(RowIndex: Integer; aValue: JSValue): Integer;
var
  D1,D2 : Double;

begin
  D1:=Double(GetFieldValue(Rowindex));
  D2:=Double(aValue);
  Result:=Round(D1-D2);
end;

{ TDateTimeFieldComparer }

function TDateTimeFieldComparer.Compare(RowIndex: Integer; aValue: JSValue): Integer;

var
  D1,D2 : TDateTime;

begin
  D1:=Dataset.ConvertDateTimeField(String(GetFieldValue(Rowindex)),Self.Field);
  D2:=TDateTime(aValue);
  Result:=Round(D1-D2);
end;

{ TBooleanFieldComparer }

function TBooleanFieldComparer.Compare(RowIndex: Integer; aValue: JSValue): Integer;

var
  B1,B2 : Boolean;

begin
  B1:=Boolean(GetFieldValue(Rowindex));
  B2:=Boolean(aValue);
  Result:=Ord(B1)-Ord(B2);
end;

{ TNativeIntFieldComparer }

function TNativeIntFieldComparer.Compare(RowIndex: Integer; aValue: JSValue): Integer;

var
  I1,I2 : NativeInt;

begin
  I1:=NativeInt(GetFieldValue(Rowindex));
  I2:=NativeInt(aValue);
  Result:=I1-I2;
end;

{ TStringFieldComparer }

function TStringFieldComparer.Compare(RowIndex: Integer; aValue: JSValue): Integer;

var
  S1,S2 : String;

begin
  S1:=String(GetFieldValue(Rowindex));
  S2:=String(aValue);
  if loPartialKey in Options then
    S1:=Copy(S1,1,Length(S2));
  if loCaseInsensitive in options then
    Result := CompareText(S1,S2)
  else
    Result := CompareStr(S1,S2);
end;

{ TFieldComparer }

constructor TFieldComparer.Create(aDataset: TBaseJSONDataset; aField: TField; aValue: JSValue; aOptions: TLocateOptions);

begin
  FField:=AField;
  FValue:=aValue;
  FOptions:=aOptions;
  FDataset:=aDataset;
end;

function TFieldComparer.GetFieldValue(RowIndex: integer): JSValue;
begin
  Result:=FDataset.FieldMapper.GetJSONDataForField(FField,FDataset.FRows[Rowindex]);

end;

function TFieldComparer.Compare(RowIndex: Integer): Integer;
begin
  Result:=Compare(RowIndex,FValue);
end;

{ TRecordComparer }

function TRecordComparer.GetFieldComparer(Index: Integer): TFieldComparer;
begin
  if (Index<0) or (Index>=Length(Fitems)) then
    Raise EListError.CreateFmt('Index out of bounds: %d not in [%d,%d]',[Index,0,Length(Fitems)-1]);
  Result:=Items[Index];
end;

procedure TRecordComparer.ConstructItems(aFields : String);

Var
  L : TFPlist;
  FCC : TFieldComparerClass;
  F : TField;
  I : Integer;


begin
  L:=TFPList.Create;
  try
    Dataset.GetFieldList(L,aFields);
    if L.Count<>Length(FValues) then
       Raise EDatabaseError.CreateFmt('Array of values has different length (%d) from array of fields (%d)',[Length(FValues), L.Count]);
    SetLength(FItems,L.Count);
    For I:=0 to L.Count-1 do
      begin
      F:=TField(L[i]);
      FCC:=DataTypeToComparerClass(F.DataType);
      If FCC=Nil then
        Raise EDatabaseError.CreateFmt('Cannot locate on field %s of type %s)',[F.FieldName,GetEnumName(TypeInfo(TFieldType),Ord(F.DataType))]);
      Fitems[i]:=FCC.Create(FDataset,F,FValues[i],FOptions);
      end;
  finally
    L.Free;
  end;
end;

function TRecordComparer.DataTypeToComparerClass(aFieldType: TFieldType): TFieldComparerClass;

begin
  Case aFieldType of
    ftMemo, ftFixedChar,ftString :
      Result:=TStringFieldComparer;
    ftAutoInc, ftInteger, ftLargeInt:
      Result:=TNativeIntFieldComparer;
    ftBoolean:
      Result:=TBooleanFieldComparer;
    ftFloat:
      Result:=TFloatFieldComparer;
    ftDate, ftTime, ftDateTime:
      Result:=TDateTimeFieldComparer;
  else
   result:=Nil;
  end;
end;

function TRecordComparer.Compare(aRowindex: integer): Integer;

Var
  I,L : Integer;

begin
  Result:=0;
  I:=0;
  L:=Length(FItems);
  While (Result=0) and (I<L) do
    begin
    Result:=Fitems[i].Compare(aRowindex);
    Inc(I);
    end;
end;

constructor TRecordComparer.Create(aDataset: TBaseJSONDataset; aFields: String; aValues: JSValue; aOptions: TLocateOptions);
begin
  FDataset:=aDataset;
  if isArray(aValues) then
    FValues:=TJSValueDynArray(aValues)
  else
    begin
    SetLength(FValues,1);
    FValues[0]:=Avalues;
    end;
  Foptions:=aOptions;
  ConstructItems(aFields);
end;

{ TDefaultJSONIndex }

procedure TDefaultJSONIndex.CreateIndex;

Var
  I : Integer;

begin
  For I:=0 to FRows.length-1 do
    FList[i]:=I;
end;

procedure TDefaultJSONIndex.AppendToIndex;

Var
  I,L : Integer;

begin
  L:=FList.Length;
  FList.Length:=FRows.Length;
  For I:=L to FRows.Length-1 do
    FList[i]:=I;
end;

function TDefaultJSONIndex.Append(aRecordIndex: Integer): Integer;
begin
  Result:=FList.Push(aRecordIndex)-1;
end;

function TDefaultJSONIndex.Insert(aCurrentIndex, aRecordIndex: Integer
  ): Integer;
begin
  FList.splice(aCurrentIndex, 0, aRecordIndex);
  Result:=aCurrentIndex;
end;

function TDefaultJSONIndex.FindRecord(aRecordIndex: Integer): Integer;
begin
  Result:=FList.indexOf(aRecordIndex);
end;

function TDefaultJSONIndex.Update(aCurrentIndex, aRecordIndex: Integer
  ): Integer;
begin
  Result:=0;
  If RecordIndex[aCurrentIndex]<>aRecordIndex then
    DatabaseErrorFmt('Inconsistent record index in default index, expected %d, got %d.',[aCurrentIndex,RecordIndex[aCurrentIndex]],Dataset);
end;

{ TJSONIndex }

constructor TJSONIndex.Create(aDataset: TDataset; aRows: TJSArray);
begin
  FRows:=aRows;
  FList:=TJSArray.New(FRows.length);
  FDataset:=aDataset;
  CreateIndex;
end;

function TJSONIndex.Delete(aListIndex: Integer): Integer;

Var
  a : TJSArray;

begin
  A:=FList.Splice(aListIndex,1);
  If a.Length>0 then
    Result:=Integer(A[0])
  else
    Result:=-1;
end;

function TJSONIndex.Insert(aCurrentIndex, aRecordIndex: Integer): Integer;
begin
  Result:=Append(aRecordIndex);
end;

function TJSONIndex.GetCount: Integer;
begin
  Result:=FList.Length;
end;

function TJSONIndex.GetRecordIndex(aListIndex : Integer): NativeInt;
begin
  if isUndefined(FList[aListIndex]) then
    Result:=-1
  else
    Result:=NativeInt(FList[aListIndex]);
end;



{ TJSONFieldMapper }

function TJSONFieldMapper.GetJSONDataForField(F: TField; Row: JSValue  ): JSValue;
begin
  // This supposes that Index is correct, i.e. the field positions have not been changed.
  Result:=GetJSONDataForField(F.FieldName,F.Index,Row);
end;

procedure TJSONFieldMapper.SetJSONDataForField(F: TField; Row,Data: JSValue);
begin
  SetJSONDataForField(F.FieldName,F.Index,Row,Data);
end;


{ TJSONArrayFieldMapper }

procedure TJSONArrayFieldMapper.SetJSONDataForField(const FieldName: String;
  FieldIndex: Integer; Row, Data: JSValue);
begin
  TJSValueDynArray(Row)[FieldIndex]:=Data;
end;

function TJSONArrayFieldMapper.GetJSONDataForField(Const FieldName: String;
  FieldIndex: Integer; Row: JSValue): JSValue;
begin
  Result:=TJSValueDynArray(Row)[FieldIndex];
end;

function TJSONArrayFieldMapper.CreateRow: JSValue;

begin
  Result:=TJSArray.New;
end;

{ TJSONObjectFieldMapper }

procedure TJSONObjectFieldMapper.SetJSONDataForField(const FieldName: String;
  FieldIndex: Integer; Row, Data: JSValue);
begin
  TJSObject(Row).Properties[FieldName]:=Data;
end;

function TJSONObjectFieldMapper.GetJSONDataForField(const FieldName: String;
  FieldIndex: Integer; Row: JSValue): JSValue;
begin
  Result:=TJSObject(Row).Properties[FieldName];
end;

function TJSONObjectFieldMapper.CreateRow: JSValue;
begin
  Result:=TJSObject.New;
end;

procedure TBaseJSONDataSet.SetMetaData(AValue: TJSObject);
begin
  CheckInActive;
  FMetaData:=AValue;
end;

procedure TBaseJSONDataSet.AddToRows(AValue: TJSArray);

begin
  if FRows=Nil then
    FRows:=AValue
  else
    begin
    FRows:=FRows.Concat(AValue);
    AppendToIndexes;
    end;
end;

procedure TBaseJSONDataSet.SetRows(AValue: TJSArray);
begin
  if AValue=FRows then exit;
  CheckInActive;
  FRows:=Nil;
  AddToRows(AValue);
end;


function TBaseJSONDataSet.AllocRecordBuffer: TDataRecord;
begin
  Result.data:=TJSObject.New;
  Result.bookmark:=null;
  Result.state:=rsNew;
end;

// the next two are particularly ugly.
procedure TBaseJSONDataSet.InternalInitRecord(var Buffer: TDataRecord);
begin
//  Writeln('TBaseJSONDataSet.InternalInitRecord');
  Buffer.Data:=FFieldMapper.CreateRow;
  Buffer.bookmark:=null;
  Buffer.state:=rsNew;
end;

procedure TBaseJSONDataSet.FreeRecordBuffer (var Buffer: TDataRecord);
begin
  Buffer.Data:=Null;
  Buffer.bookmark:=null;
  Buffer.state:=rsNew;
end;

procedure TBaseJSONDataSet.GetBookmarkData(Buffer: TDataRecord; var Data: TBookmark);
begin
   Data.Data:=Buffer.bookmark;
end;

function TBaseJSONDataSet.GetBookmarkFlag(Buffer: TDataRecord): TBookmarkFlag;
begin
  Result :=Buffer.BookmarkFlag;
end;

function TBaseJSONDataSet.GetRecNo: Integer;

Var
  bkmIdx : Integer;

begin
  bkmIdx:=Integer(ActiveBuffer.bookmark);
  Result:=FCurrentIndex.FindRecord(bkmIdx)+1;
end;

procedure TBaseJSONDataSet.InternalInitFieldDefs;
begin
  If Assigned(FMetaData) then
    MetaDataToFieldDefs;
  if (FieldDefs.Count=0) then
    Raise EJSONDataset.Create('No fields found');
end;

procedure TBaseJSONDataSet.FreeData;
begin
  If FOwnsData then
    begin
    FRows:=Nil;
    FMetaData:=Nil;
    end;
  if (FCurrentIndex<>FDefaultIndex) then
    FreeAndNil(FCurrentIndex)
  else
    FCurrentIndex:=Nil;
  FreeAndNil(FDefaultindex);
  FreeAndNil(FFieldMapper);
  FCurrentIndex:=Nil;
  FDeletedRows:=Nil;
end;

procedure TBaseJSONDataSet.AppendToIndexes;

begin
  FDefaultIndex.AppendToIndex;
end;

procedure TBaseJSONDataSet.CreateIndexes;

begin
  FDefaultIndex:=TDefaultJSONIndex.Create(Self,FRows);
  AppendToIndexes;
  FCurrentIndex:=FDefaultIndex;
end;

function TBaseJSONDataSet.GetRecord(Var Buffer: TDataRecord; GetMode: TGetMode; DoCheck: Boolean): TGetResult;

Var
  BkmIdx : Integer;

begin
  Result := grOK; // default
  case GetMode of
    gmNext: // move on
      if fCurrent < fCurrentIndex.Count - 1 then
        Inc (fCurrent)
      else
        Result := grEOF; // end of file
    gmPrior: // move back
      if fCurrent > 0 then
        Dec (fCurrent)
      else
        Result := grBOF; // begin of file
    gmCurrent: // check if empty
      if fCurrent >= fCurrentIndex.Count then
        Result := grEOF;
  end;
  if Result = grOK then // read the data
    begin
    BkmIdx:=FCurrentIndex.RecordIndex[FCurrent];
    Buffer.Data:=FRows[bkmIdx];
    Buffer.BookmarkFlag := bfCurrent;
    Buffer.Bookmark:=BkmIdx;
    CalculateFields(Buffer);
    end;
end;

function TBaseJSONDataSet.GetRecordCount: Integer;
begin
  Result:=FCurrentIndex.Count;
end;

function TBaseJSONDataSet.GetRecordSize: Word;
begin
  Result := 0; // actual data without house-keeping
end;


procedure TBaseJSONDataSet.InternalClose;
begin
  // disconnet and destroy field objects
  BindFields (False);
  if DefaultFields then
    DestroyFields;
  FreeData;
end;

procedure TBaseJSONDataSet.InternalDelete;

Var
  Idx : Integer;

begin
  Idx:=FCurrentIndex.Delete(FCurrent);
  if (Idx<>-1) then
    begin
    // Add code here to Delete from other indexes as well.
    // ...
    // Add to array of deleted records.
    if Not Assigned(FDeletedRows) then
      FDeletedRows:=TJSArray.New(FRows[idx])
    else
      FDeletedRows.Push(FRows[Idx]);
    FRows[Idx]:=Undefined;
    end;
end;

procedure TBaseJSONDataSet.InternalFirst;
begin
  FCurrent := -1;
end;

procedure TBaseJSONDataSet.InternalGotoBookmark(ABookmark: TBookmark);
begin
  if isNumber(ABookmark.Data) then
    FCurrent:=FCurrentIndex.FindRecord(Integer(ABookmark.Data));
//  Writeln('Fcurrent', FCurrent,' from ',ABookmark.Data);
end;


procedure TBaseJSONDataSet.InternalInsert;

Var
  I : Integer;
  D : TFieldDef;

begin
//  Writeln('TBaseJSONDataSet.InternalInsert');
  FEditRow:=ActiveBuffer.Data;
  For I:=0 to FieldDefs.Count-1 do
    begin
    D:=FieldDefs[i];
    FFieldMapper.SetJSONDataForField(D.Name,D.Index,FEditRow,Null);
    end;
end;

procedure TBaseJSONDataSet.InternalEdit;

begin
//  Writeln('TBaseJSONDataSet.InternalEdit:  ');
  FEditIdx:=FCurrentIndex.RecordIndex[FCurrent];
  if not isUndefined(Rows[FEditIdx]) then
    FEditRow:=TJSJSON.parse(TJSJSON.stringify(Rows[FEditIdx]))
  else
    FEditRow:=TJSObject.new;
//  Writeln('TBaseJSONDataSet.InternalEdit: ',FEditRow);
end;

procedure TBaseJSONDataSet.InternalCancel;
begin
  FEditIdx:=-1;
  FEditRow:=Nil;
end;

procedure TBaseJSONDataSet.InternalLast;
begin
  // The first thing that will happen is a GetPrior Record.
  FCurrent:=FCurrentIndex.Count;
end;

procedure TBaseJSONDataSet.InitDateTimeFields;

begin
  // Do nothing
end;

procedure TBaseJSONDataSet.InternalOpen;
begin
  FreeAndNil(FFieldMapper);
  FFieldMapper:=CreateFieldMapper;
  IF (FRows=Nil) then // opening from fielddefs ?
    begin
    FRows:=TJSArray.New;
    OwnsData:=True;
    end;
  CreateIndexes;
  InternalInitFieldDefs;
  if DefaultFields then
    CreateFields;
  BindFields (True);
  InitDateTimeFields;
  FCurrent := -1;
end;

procedure TBaseJSONDataSet.InternalPost;

Var
  Idx : integer;
  B : TBookmark;

begin
  GetBookMarkData(ActiveBuffer,B);
  if (State=dsInsert) then
    begin // Insert or Append
    Idx:=FRows.push(FEditRow)-1;
    if GetBookMarkFlag(ActiveBuffer)=bfEOF then
      begin // Append
      FDefaultIndex.Append(Idx);
      // Must replace this by updating all indexes
      if (FCurrentIndex<>FDefaultIndex) then
        FCurrentIndex.Append(Idx);
      end
    else  // insert
      begin
      FCurrent:=FDefaultIndex.Insert(FCurrent,Idx);
      // Must replace this by updating all indexes.
      // Note that this will change current index.
      if (FCurrentIndex<>FDefaultIndex) then
        FCurrent:=FCurrentIndex.Insert(FCurrent,Idx);
      end;
    end
  else
    begin // Edit
    if (FEditIdx=-1) then
      DatabaseErrorFmt('Failed to retrieve record index for record %d',[FCurrent]);
    // Update source record
    Idx:=FEditIdx;
    FRows[Idx]:=FEditRow;
    FDefaultIndex.Update(FCurrent,Idx);
    // Must replace this by updating all indexes.
    // Note that this will change current index.
    if (FCurrentIndex<>FDefaultIndex) then
      FCurrentIndex.Update(FCurrent,Idx);
    end;
  FEditIdx:=-1;
  FEditRow:=Nil;
end;

procedure TBaseJSONDataSet.InternalSetToRecord(Buffer: TDataRecord);
begin
  FCurrent:=FCurrentIndex.FindRecord(Integer(Buffer.Bookmark));
end;

function TBaseJSONDataSet.GetFieldClass(FieldType: TFieldType): TFieldClass;
begin
  If UseDateTimeFormatFields and (FieldType in [ftDate,ftDateTime,ftTime]) then
    case FieldType of
      ftDate : Result:=TJSONDateField;
      ftDateTime : Result:=TJSONDateTimeField;
      ftTime : Result:=TJSONTimeField;
    end
  else
    Result:=inherited GetFieldClass(FieldType);
end;

function TBaseJSONDataSet.IsCursorOpen: Boolean;
begin
  Result := Assigned(FDefaultIndex);
end;

function TBaseJSONDataSet.BookmarkValid(ABookmark: TBookmark): Boolean;
begin
  Result:=isNumber(ABookmark.Data);
end;

procedure TBaseJSONDataSet.SetBookmarkData(var Buffer: TDataRecord;  Data: TBookmark);
begin
  Buffer.Bookmark:=Data.Data;
//  Writeln('Set Bookmark from: ',Data.Data);
end;

function TBaseJSONDataSet.ConvertDateTimeField(S : String; F : TField) : TDateTime;

Var
  Ptrn : string;

begin
  Result:=0;
  Ptrn:='';
  Case F.DataType of
    ftDate : if F is TJSONDateField then
               Ptrn:=(F as TJSONDateField).DateFormat;
    ftTime : if F is TJSONTimeField then
               Ptrn:=(F as TJSONTimeField).TimeFormat;
    ftDateTime : if F is TJSONDateTimeField then
               Ptrn:=(F as TJSONDateTimeField).DateTimeFormat;
  end;
  If (Ptrn='') then
    Case F.DataType of
      ftDate : Result:=StrToDate(S);
      ftTime : Result:=StrToTime(S);
      ftDateTime : Result:=StrToDateTime(S);
    end
  else
    begin
    Result:=ScanDateTime(ptrn,S,1);
    end;
end;

function TBaseJSONDataSet.FormatDateTimeField(DT: TDateTime; F: TField
  ): String;

Var
  Ptrn : string;
begin
  Result:='';
  Ptrn:='';
  Case F.DataType of
    ftDate : if F is TJSONDateField then
                Ptrn:=TJSONDateField(F).DateFormat;
    ftTime : if F is TJSONTimeField then
                Ptrn:=TJSONTimeField(F).TimeFormat;
    ftDateTime : if F is TJSONDateTimeField then
                Ptrn:=TJSONDateTimeField(F).DateTimeFormat;
  end;
  If (Ptrn='') then
    Case F.DataType of
      ftDate : Result:=DateToStr(DT);
      ftTime : Result:=TimeToStr(DT);
      ftDateTime : Result:=DateTimeToStr(DT);
    end
  else
    Result:=FormatDateTime(ptrn,DT);
end;

function TBaseJSONDataSet.GetFieldData(Field: TField; Buffer: TDatarecord): JSValue;

var
  R : JSValue;

begin
  if State in [dsCalcFields,dsInternalCalc] then
    R:=CalcBuffer.data
  else if (FEditIdx=Buffer.Bookmark) then
    begin
    if State=dsOldValue then
      R:=Buffer.data
    else
      R:=FEditRow
    end
  else
    begin
    if State=dsOldValue then
      Exit(Null)
    else
      R:=Buffer.data;
    end;
  Result:=FFieldMapper.GetJSONDataForField(Field,R);
end;

procedure TBaseJSONDataSet.SetFieldData(Field: TField; var Buffer: TDatarecord; AValue : JSValue);

var
  R : JSValue;

begin
  if State in [dsCalcFields,dsInternalCalc] then
    R:=CalcBuffer.Data
  else
    R:=FEditRow;
  FFieldMapper.SetJSONDataForField(Field,R,AValue);
  
  if not(State in [dsCalcFields, dsInternalCalc, dsFilter, dsNewValue]) then
    DataEvent(deFieldChange, Field);

  SetModified(True);
//  FFieldMapper.SetJSONDataForField(Field,Buffer.Data,AValue);
end;

procedure TBaseJSONDataSet.SetBookmarkFlag(var Buffer: TDataRecord; Value: TBookmarkFlag);

begin
  Buffer.BookmarkFlag := Value;
end;

function TBaseJSONDataSet.CompareBookmarks(Bookmark1, Bookmark2: TBookmark): Longint;

begin
  if isNumber(Bookmark1.Data) and isNumber(Bookmark2.Data) then
    Result := Integer(Bookmark2.Data) - Integer(Bookmark1.Data)
  else
  begin
    if isNumber(Bookmark1.Data) then
      Result := -1
    else
    if isNumber(Bookmark2.Data) then
      Result := 1
    else
      Result := 0;
  end;
end;


procedure TBaseJSONDataSet.SetRecNo(Value: Integer);
begin
  if (Value < 1) or (Value > FCurrentIndex.Count) then
    raise EJSONDataset.CreateFmt('%s: SetRecNo: index %d out of range',[Name,Value]);
  FCurrent := Value - 1;
  Resync([]); 
  DoAfterScroll;
end;

constructor TBaseJSONDataSet.Create(AOwner: TComponent);
begin
  inherited;
  FownsData:=True;
  UseDateTimeFormatFields:=False;
  FEditIdx:=-1;
end;

destructor TBaseJSONDataSet.Destroy;
begin
  FEditIdx:=-1;
  FreeData;
  inherited;
end;

function TBaseJSONDataSet.LocateRecordIndex(const KeyFields: string; const KeyValues: JSValue; Options: TLocateOptions): Integer;

Var
  Comp : TRecordComparer;
  RI,I : Integer;

begin
  Result:=-1;
  Comp:=TRecordComparer.Create(Self,KeyFields,KeyValues,Options);
  try
    I:=FCurrent;
    RI:=FCurrentIndex.GetRecordIndex(I);
    While (Result=-1) and (RI<>-1) do
      begin
      if Comp.Compare(RI)=0 then
        Result:=RI;
      inc(I);
      RI:=FCurrentIndex.GetRecordIndex(I);
      end;
  finally
    Comp.Free;
  end;
end;

function TBaseJSONDataSet.Locate(const KeyFields: string; const KeyValues: JSValue; Options: TLocateOptions): boolean;

Var
  I : Integer;
  BM : TBookMark;

begin
  Result:=Inherited;
  I:=LocateRecordIndex(KeyFields,KeyValues,Options);
  Result:=I<>-1;
  if Result then
    begin
    // Construct bookmark.
    // Bookmark is always the index in the FRows array.
    BM.Data:=I;
    BM.Flag:=bfCurrent;
    GotoBookMark(BM);
    end;
end;

function TBaseJSONDataSet.Lookup(const KeyFields: string; const KeyValues: JSValue; const ResultFields: string): JSValue;

Var
  RI,I : Integer;
  BM : TBookMark;
  l : TFPList;
  Vals : TJSValueDynArray;

begin
  Result:=Null;
  l:=TFPList.Create;
  try
    GetFieldList(L,ResultFields);
    Result:=inherited Lookup(KeyFields, KeyValues, ResultFields);
    RI:=LocateRecordIndex(KeyFields,KeyValues,[]);
    Result:=RI<>-1;
    if Result then
      begin
      SetLength(Vals,L.Count);
      For I:=0 to L.Count-1 do
        Vals[i]:=FFieldMapper.GetJSONDataForField(TField(L[I]),FRows[RI]);
      if L.Count=1 then
        Result:=Vals[i]
      else
        Result:=Vals;
      end;
  finally
    L.Free;
  end;
end;

end.
