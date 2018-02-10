{$mode objfpc}
{$h+}
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

  // basic JSON dataset. Does nothing ExtJS specific.
  TBaseJSONDataSet = class (TDataSet)
  private
    FMUS: Boolean;
    FOwnsData : Boolean;
    FDefaultList : TFPList;
    FCurrentList: TFPList;
    FCurrent: Integer;
    // Possible metadata to configure fields from.
    FMetaData : TJSObject;
    // This will contain the rows.
    FRows : TJSArray;
    FFieldMapper : TJSONFieldMapper;
    // When editing, this object is edited.
    FEditRow : JSValue;
    procedure SetMetaData(AValue: TJSObject);
    procedure SetRows(AValue: TJSArray);
  protected
    // dataset virtual methods
    function AllocRecordBuffer: TDataRecord; override;
    procedure FreeRecordBuffer(var Buffer: TDataRecord); override;
    procedure InternalInitRecord(var Buffer: TDataRecord); override;
    procedure GetBookmarkData(Buffer: TDataRecord; var Data: TBookmark); override;
    function GetBookmarkFlag(Buffer: TDataRecord): TBookmarkFlag; override;
    function GetRecord(Var Buffer: TDataRecord; GetMode: TGetMode; DoCheck: Boolean): TGetResult; override;
    function GetRecordSize: Word; override;
    procedure AddToRows(AValue: TJSArray);
    procedure InternalClose; override;
    procedure InternalDelete; override;
    procedure InternalFirst; override;
    procedure InternalGotoBookmark(ABookmark: TBookmark); override;
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
    procedure AddToList; virtual;
    Procedure FillList; virtual;
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
  public
    constructor Create (AOwner: TComponent); override;
    destructor Destroy; override;
    function GetFieldData(Field: TField; Buffer: TDatarecord): JSValue;  override;
    procedure SetFieldData(Field: TField; var Buffer: TDatarecord; AValue : JSValue);  override;
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
    procedure SetJSONDataForField(Const FieldName : String; FieldIndex : Integer; Row,Data : JSValue); override;
    Function GetJSONDataForField(Const FieldName : String; FieldIndex : Integer; Row : JSValue) : JSValue; override;
    Function CreateRow : JSValue; override;
  end;

  { TJSONArrayFieldMapper }
  // Fieldmapper to be used when the data is in an array
  TJSONArrayFieldMapper = Class(TJSONFieldMapper)
  Public
    procedure SetJSONDataForField(Const FieldName : String; FieldIndex : Integer; Row,Data : JSValue); override;
    Function GetJSONDataForField(Const FieldName : String; FieldIndex : Integer; Row : JSValue) : JSValue; override;
    Function CreateRow : JSValue; override;
  end;

  EJSONDataset = Class(EDatabaseError);
  
implementation

uses DateUtils;


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
    AddToList;
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
//  writeln('Bookmark :',Buffer.bookmark);
  Data.Data:=Buffer.bookmark;
end;

function TBaseJSONDataSet.GetBookmarkFlag(Buffer: TDataRecord): TBookmarkFlag;
begin
  Result :=Buffer.BookmarkFlag;
end;

function TBaseJSONDataSet.GetRecNo: Integer;
begin
  Result := FCurrent + 1;
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
  if (FCurrentList<>FDefaultList) then
    FreeAndNil(FCurrentList)
  else
    FCurrentList:=Nil;
  FreeAndNil(FDefaultList);
  FreeAndNil(FFieldMapper);
  FCurrentList:=Nil;
end;

procedure TBaseJSONDataSet.AddToList;

Var
  I : Integer;

begin
  For I:=FDefaultList.Count to FRows.Length-1 do
    FDefaultList.Add(FRows[i]);
end;

procedure TBaseJSONDataSet.FillList;


begin
  FDefaultList:=TFPList.Create;
  AddToList;
  FCurrentList:=FDefaultList;
end;



function TBaseJSONDataSet.GetRecord(Var Buffer: TDataRecord; GetMode: TGetMode; DoCheck: Boolean): TGetResult;
begin
  Result := grOK; // default
  case GetMode of
    gmNext: // move on
      if fCurrent < fCurrentList.Count - 1 then
        Inc (fCurrent)
      else
        Result := grEOF; // end of file
    gmPrior: // move back
      if fCurrent > 0 then
        Dec (fCurrent)
      else
        Result := grBOF; // begin of file
    gmCurrent: // check if empty
      if fCurrent >= fCurrentList.Count then
        Result := grEOF;
  end;
  if Result = grOK then // read the data
    begin
    Buffer.Data:=FRows[FCurrent];
    Buffer.BookmarkFlag := bfCurrent;
    Buffer.Bookmark:=fCurrent;
    end;
end;

function TBaseJSONDataSet.GetRecordCount: Integer;
begin
  Result := FCurrentList.Count;
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
  R : JSValue;

begin
  R:=JSValue(FCurrentList[FCurrent]);
  FCurrentList.Delete(FCurrent);
  if (FCurrent>=FCurrentList.Count) then
    Dec(FCurrent);
  FRows:=FRows.Splice(FCurrent,1);
end;

procedure TBaseJSONDataSet.InternalFirst;
begin
  FCurrent := -1;
end;

procedure TBaseJSONDataSet.InternalGotoBookmark(ABookmark: TBookmark);
begin
  if isNumber(ABookmark.Data) then
    FCurrent:=Integer(ABookmark.Data);
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
  FEditRow:=TJSJSON.parse(TJSJSON.stringify(FRows[FCurrent]));
//  Writeln('TBaseJSONDataSet.InternalEdit: ',FEditRow);
end;

procedure TBaseJSONDataSet.InternalCancel;
begin
  FEditRow:=Nil;
end;

procedure TBaseJSONDataSet.InternalLast;
begin
  // The first thing that will happen is a GetPrior Record.
  FCurrent:=FCurrentList.Count;
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
  FillList;
  InternalInitFieldDefs;
  if DefaultFields then
    CreateFields;
  BindFields (True);
  InitDateTimeFields;
  FCurrent := -1;
end;

procedure TBaseJSONDataSet.InternalPost;

Var
  RI,I : integer;
  B : TBookmark;

begin
  GetBookMarkData(ActiveBuffer,B);
  if (State=dsInsert) then
    begin // Insert or Append
    FRows.push(FEditRow);
    if GetBookMarkFlag(ActiveBuffer)=bfEOF then
      begin // Append
      FDefaultList.Add(FEditRow);
      if (FCurrentList<>FDefaultList) then
        FCurrentList.Add(FEditRow);
      end
    else  // insert
      begin
      FCurrentList.Insert(FCurrent,FEditRow);
      if (FCurrentList<>FDefaultList) then
        FDefaultList.Add(FEditRow);
      end;
    end
  else
    begin // Edit
    RI:=FRows.IndexOf(JSValue(FCurrentList[FCurrent]));
    if (RI<>-1) then
      FRows[RI]:=FEditRow
    else
      FRows.push(FEditRow);
    FCurrentList[FCurrent]:=FEditRow;
    if (FCurrentList<>FDefaultList) then
      FDefaultList[FCurrent]:=FEditRow;
    end;
  FEditRow:=Nil;
end;

procedure TBaseJSONDataSet.InternalSetToRecord(Buffer: TDataRecord);
begin
  FCurrent := Integer(Bookmark.Data);
end;

function TBaseJSONDataSet.GetFieldClass(FieldType: TFieldType): TFieldClass;
begin
  case FieldType of
    ftDate : Result:=TJSONDateField;
    ftDateTime : Result:=TJSONDateTimeField;
    ftTime : Result:=TJSONTimeField;
  else
    Result:=inherited GetFieldClass(FieldType);
  end;
end;

function TBaseJSONDataSet.IsCursorOpen: Boolean;
begin
  Result := Assigned(FDefaultList);
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
//  Writeln('Getting data for field ',Field.FieldName,'Buffer  ',Buffer);
  if (FEditRow<>Nil) then
    R:=FEditRow
  else
    R:=Buffer.data;
  Result:=FFieldMapper.GetJSONDataForField(Field,R);
end;

procedure TBaseJSONDataSet.SetFieldData(Field: TField; var Buffer: TDatarecord; AValue : JSValue);

begin
  FFieldMapper.SetJSONDataForField(Field,FEditRow,AValue);
//  FFieldMapper.SetJSONDataForField(Field,Buffer.Data,AValue);
end;

procedure TBaseJSONDataSet.SetBookmarkFlag(var Buffer: TDataRecord; Value: TBookmarkFlag);

begin
  Buffer.BookmarkFlag := Value;
end;

procedure TBaseJSONDataSet.SetRecNo(Value: Integer);
begin
  if (Value < 0) or (Value > FCurrentList.Count) then
    raise EJSONDataset.CreateFmt('SetRecNo: index %d out of range',[Value]);
  FCurrent := Value - 1;
  Resync([]); 
  DoAfterScroll;
end;

constructor TBaseJSONDataSet.Create(AOwner: TComponent);
begin
  inherited;
  FownsData:=True;
end;

destructor TBaseJSONDataSet.Destroy;
begin
  FreeData;
  inherited;
end;

end.
