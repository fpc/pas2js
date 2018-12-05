{$mode objfpc}

unit JSONDataset;

interface

uses
  Types, JS, DB, Classes, SysUtils;

type

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
  Case F.DataType of
    ftDate : Ptrn:=TJSONDateField(F).DateFormat;
    ftTime : Ptrn:=TJSONTimeField(F).TimeFormat;
    ftDateTime : Ptrn:=TJSONDateTimeField(F).DateTimeFormat;
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
  Case F.DataType of
    ftDate : Ptrn:=TJSONDateField(F).DateFormat;
    ftTime : Ptrn:=TJSONTimeField(F).TimeFormat;
    ftDateTime : Ptrn:=TJSONDateTimeField(F).DateTimeFormat;
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

end.
