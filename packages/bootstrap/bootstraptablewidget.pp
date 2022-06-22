unit bootstraptablewidget;

interface

uses
  libjquery, libbootstraptable, WebWidget, SysUtils, Classes, DB, JS, Web, strutils;

type
  EBootstrapTable = class(EWidgets);

  TCustomDBBootstrapTableWidget = Class;

  TColumnRenderMode = (crmText, crmNumeric, crmDateTime, crmTransformedValue, crmCheckBox, crmButton, crmCustom);
  TColumnButtonType = (cbtInfo, cbtEdit, cbtDelete, cbtCustom);

  TDataTablesFieldMap = Class(TObject)
  Private
    FRow: TJSArray;
    FFieldDefs: TFieldDefs;
  Public
    Function GetValueByName(S: String): JSValue;
  end;

  TOnCustomValueEvent = procedure(Sender: TObject; Fields: TDataTablesFieldMap; out aOutput: String) of object;
  TOnFormatEvent = procedure(Sender: TObject; Data: JSValue; row: TJSObject; RowIndex : Integer; Field : string; out aOutput: JSValue);
  TSortOrderMethod = function(Sender: TObject; Data: JSValue): Integer;

  { TStylingClasses }

  TStylingClasses = Class(TPersistent)
  private
    FButtonClass: String;
    FCheckBoxClass: String;
    FDeleteClass: String;
    FEditClass: String;
    FInfoClass: String;
    FReadonlyEditClass: String;
    FWidget : TCustomDBBootstrapTableWidget;
    function GetReadonlyEditClass: String;
    function IsReadonlyStored: Boolean;
  Public
    Constructor Create(aWidget: TCustomDBBootstrapTableWidget); virtual;
    Procedure Assign(Source : TPersistent); override;
  Published
    Property CheckBoxClass : String Read FCheckBoxClass Write FCheckBoxClass;
    Property ButtonClass : String Read FButtonClass Write FButtonClass;
    Property InfoClass : String Read FInfoClass Write FInfoClass;
    Property EditClass : String Read FEditClass Write FEditClass;
    Property ReadonlyEditClass : String Read GetReadonlyEditClass Write FReadonlyEditClass stored IsReadonlyStored;
    Property DeleteClass : String Read FDeleteClass Write FDeleteClass;
  end;

  { TBSTableColumn }

  TBSTableColumn = class(TCollectionItem)
  private
    FElementID: string;
    FFieldName: string;
    FFormatting: string;
    FSelectable: Boolean;
    FTitle: string;
    FRenderMode: TColumnRenderMode;
    FButtonType: TColumnButtonType;
    FOnCustomFormat: TOnFormatEvent;
    FWidth: Integer;
    FCSSClassName: string;
    FCheckBoxClassName: string;
    FVisible: Boolean;
    FSearchable: Boolean;
    FSortable: Boolean;
    FButtonURL: string;
    FButtonURLTarget: string;
    FButtonIconClass: String;
    FDtType: String;
    FOnGetSortValue: TSortOrderMethod;
    FOnGetValue: TOnCustomValueEvent;
    FExtraAttributes: String;
    FWidthUnits: String;
    function GetTitle: string;
  protected
    function GetDisplayName: String; override;
    { private declarations }
  public
    procedure Assign(Source: TPersistent); override;
    constructor Create(aOwner: TCollection); override;
  published
    // Fieldname for this column
    property FieldName: string read FFieldName write FFieldName;
    // Title for this column
    property Title: string read GetTitle write FTitle;
    // Render mode: text, numer, checkbox, button custom render
    property RenderMode: TColumnRenderMode read FRenderMode write FRenderMode;
    // When rendermode is rmButton, what button ?
    property ButtonType: TColumnButtonType read FButtonType write FButtonType;
    // When buttontype is btCustom, use the following class (in <i class="">)
    Property ButtonIconClass: String Read FButtonIconClass Write FButtonIconClass;
    // Called when rendermode is rmTransformValue
    Property OnTransformValue: TOnCustomValueEvent Read FOnGetValue Write FOnGetValue;
    // Called when rendermode is rmCustom
    property OnCustomFormat: TOnFormatEvent read FOnCustomFormat write FOnCustomFormat;
    // Called when column is sorted; Return a sortable value for the data
    property OnGetSortValue: TSortOrderMethod read FOnGetSortValue write FOnGetSortValue;
    // Width (in WidthUnits units)
    property Width: Integer Read FWidth Write FWidth;
    // Width (in CSS units)
    property WidthUnits: String Read FWidthUnits Write FWidthUnits;
    // CSS Class name for this column
    property CSSClassName: string read FCSSClassName write FCSSClassName;
    // CSS Class name for this column if there is a check box.
    property CheckBoxClassName: string read FCheckBoxClassName write FCheckBoxClassName;
    // Visible column ?
    property Visible: Boolean Read FVisible Write FVisible;
    // Indication if column is searchable
    property Searchable: Boolean read FSearchable write FSearchable;
    // Indication if column is sortable
    property Sortable: Boolean read FSortable write FSortable;
    // URL to use when the button is clicked
    property ButtonURL: string read FButtonURL write FButtonURL;
    // Indication if column is sortable
    property ButtonURLTarget: string read FButtonURLTarget write FButtonURLTarget;
    // Formatting string:
    // for rmDateTime, specify format as for FormatDateTime
    // for rmNumeric, specify format as for FormatFloat (sysutils)
    property Formatting : string read FFormatting write FFormatting;
    // Set the type (for custom sorting)
    property DtType: String read FDtType write FDtType;
    // Add extra attributes to the contents of the column if needed
    property ExtraAttributes: String read FExtraAttributes write FExtraAttributes;
    // Selectable ? This is a native bootstrap-table select column
    Property Selectable : Boolean Read FSelectable Write FSelectable;

  end;

  TBSTableColumns = class(TCollection)
  private
    function DoGetColumn(const aIndex: Integer): TBSTableColumn;
  public
    // Needed for TMS Web core streaming.
    function Add: TBSTableColumn; reintroduce; overload;
    function Add(const aName: string): TBSTableColumn; overload;
    function IndexOfColumn(const aName: string): Integer;
    function ColumnByName(const aName: string): TBSTableColumn;
    property DatatablesColumn[const aIndex: Integer]: TBSTableColumn read DoGetColumn; default;
    { public declarations }
  end;

  TOnCreatedRowEvent = reference to function(row: TJSNode; Data: TJSArray; dataIndex: Integer): JSValue;

  { TTableDataLink }

  TTableDataLink = class(TDatalink)

  private
    FTable: TCustomDBBootstrapTableWidget;
  Protected
    Constructor Create(aTable : TCustomDBBootstrapTableWidget);
    Procedure ActiveChanged; override;
    Property Table : TCustomDBBootstrapTableWidget Read FTable;
  end;

  TBSTableOption = (btoClickToSelect,btoEscapeHTML,btoSingleSelect, btoMultipleSelectRow,btoRememberOrder,
                    btoResizable,btoDetailViewByClick);
  TBSTableOptions = set of TBSTableOption;

  TBSTableViewOption = (bvoCardview,bvoCheckboxHeader,bvoDetailView,bvoDetailViewIcon,
                        bvoShowButtonIcons,bvoShowButtonText, bvoShowColumns,
                        bvoShowColumnsToggleAll,bvoShowToggle,bvoShowHeader,bvoSmartDisplay,
                        bvoShowRefresh,bvoShowFooter,bvoShowFullscreen,bvoVirtualScroll,
                        bvoShowSearchButton,bvoShowClearButton);
  TBSTableViewOptions = set of TBSTableViewOption;

  TBSTablePaginationOption = (bpoPagination,bpoLoop,bpoUseIntermediate,bpoExtended,bpoShowSwitch);
  TBSTablePaginationOptions = set of TBSTablePaginationOption;

  TBSTableSearchOption = (bsoSearch,bsoSearchOnEnterKey,bsoSearchTimeOut,
                          bsoStrictSearch, bsoTrimOnSearch,bsoVisibleSearch);
  TBSTableSearchOptions = set of TBSTableSearchOption;

  TBSTableSortOption = (booSortStable,booSortable,booSilentSort,booResetSort);
  TBSTableSortOptions = set of TBSTableSortOption;

  { TCustomDBBootstrapTableWidget }

  TCustomDBBootstrapTableWidget = class(TWebWidget)
  private
    FAfterBodyDraw: TNotifyEvent;
    FStylingClasses: TStylingClasses;
    FLink : TTableDataLink;
    FColumns: TBSTableColumns;
    FGridID: String;
    FData: TJSArray;
    FDataSet: TDataSet;
    FTableOptions: TBSTableOptions;
    FTablePaginationOptions: TBSTablePaginationOptions;
    FTableSearchOptions: TBSTableSearchOptions;
    FTableSortOptions: TBSTableSortOptions;
    FTableViewOptions: TBSTableViewOptions;
    FShowSearch: Boolean;
    FReadOnly: Boolean;
    function GetData: TJSArray;
    function GetDataFromDataset: TJSArray;
    function GetDatasource: TDataSource;
    function IsOptionsStored: Boolean;
    function IsPaginationOptionsStored: Boolean;
    function IsSearchOptionsStored: Boolean;
    function IsSortOPtionsStored: Boolean;
    function IsViewOptionsStored: Boolean;
    procedure SetData(const aData: TJSArray);
    procedure SetDatasource(AValue: TDataSource);
    procedure SetStylingClasses(AValue: TStylingClasses);
    function ReplaceMoustache(row: TJSObject; aMoustacheString: String): String;
    procedure SetReadOnly(const Value: Boolean);
    procedure SetTableOptions(AValue: TBSTableOptions);
    procedure SetTablePaginationOptions(AValue: TBSTablePaginationOptions);
    procedure SetTableSearchOptions(AValue: TBSTableSearchOptions);
    procedure SetTableSortOptions(AValue: TBSTableSortOptions);
    procedure SetTableViewOptions(AValue: TBSTableViewOptions);
  Protected
    // Called from datalink
    Procedure ActiveChanged; virtual;
    procedure DoAfterBodyDraw(aData : JSValue);
    procedure ApplyPaginationOptions(aOptions: TBootstrapTableOptions); virtual;
    procedure ApplySearchOptions(aOptions: TBootstrapTableOptions); virtual;
    procedure ApplyViewOptions(aOptions: TBootstrapTableOptions); virtual;
    // Apply all properties.
    function FinishColumn(aColumn: TBootstrapTableColumn; aTitle,aWidthUnits : String; aWidth: Integer; aVisible: Boolean; aClassName: String;
      aSearchable: Boolean; aSortable: Boolean): TBootstrapTableColumn;
    // Create column based on fieldName;
    function CreateCol(aFieldName, aTitle: string; aWidthUnits: String=''; aWidth : Integer = 0; aVisible: Boolean= True; aClassName: String ='';
      aSearchable: Boolean = True; aSortable: Boolean = True): TBootstrapTableColumn; overload;
    // Convert column to Button column
    function MakeButtonCol(aCol: TBootstrapTableColumn; aTableCol: TBSTableColumn): TBootstrapTableColumn;
    // Convert column to checkbox column
    function MakeCheckBoxCol(aCol: TBootstrapTableColumn; aTableCol: TBSTableColumn): TBootstrapTableColumn;
    // Convert column to custom render column
    function MakeCustomFormatCol(aCol: TBootstrapTableColumn; aTableCol: TBSTableColumn): TBootstrapTableColumn;
    // Convert column to custom render column
    function MakeTransformValueCol(aCol: TBootstrapTableColumn; aTableCol: TBSTableColumn): TBootstrapTableColumn;
    // Convert column to datetime column
    function MakeDateTimeRenderCol(aCol: TBootstrapTableColumn; aTableCol: TBSTableColumn): TBootstrapTableColumn;
    // Convert column to Numeric column
    function MakeNumericRenderCol(aCol: TBootstrapTableColumn; aTableCol: TBSTableColumn): TBootstrapTableColumn;
    // Create the column array for the options
    function GetColumnArray: TBootstrapTableColumnArray; virtual;
    // Configure options based on properties
    procedure ConfigureOptions(aOptions: TBootstrapTableOptions); virtual;
    // Defines the sorting functions (TODO: Refactoring => Move the function definitions to the table instead of the column)
    procedure DefineSortingFunctions;
    { private declarations }
  public
    class var DefaultLanguage : TJSObject;
  public

    constructor Create(aOwner: TComponent); override;
    Destructor Destroy; override;
    // Create default columns based on the fields in Dataset.
    Procedure CreateDefaultColumns(DoClear: Boolean = True);
    // Unrender table. If table was not yet rendered, this is a No-op
    Procedure UnRender;
    // Render the table. If the table was already rendered, it is first unrendered.
    procedure ApplyWidgetSettings(aElement: TJSHTMLElement); override;
    // Show a spinner
    procedure ShowLoading;
    // Data to be rendered
    property Data: TJSArray read GetData write SetData;
    // Below protected section can be published
  protected
    // Classes used in styling
    Property StylingClasses : TStylingClasses Read FStylingClasses Write SetStylingClasses;
    // Columns to create
    property Columns: TBSTableColumns read FColumns write FColumns;
    // Dataset to get fields from.
    property DataSource: TDataSource read GetDatasource write SetDatasource;
    // General behaviour options
    Property Options : TBSTableOptions Read FTableOptions Write SetTableOptions;
    // General View options
    Property ViewOptions : TBSTableViewOptions Read FTableViewOptions Write SetTableViewOptions Stored IsViewOptionsStored;
    // Pagination  options
    Property PaginationOptions : TBSTablePaginationOptions Read FTablePaginationOptions Write SetTablePaginationOptions Stored IsPaginationOptionsStored;
    // Search options
    Property SearchOptions : TBSTableSearchOptions Read FTableSearchOptions Write SetTableSearchOptions Stored IsSearchOptionsStored;
    // Sort options
    Property SortOptions : TBSTableSortOptions Read FTableSortOptions Write SetTableSortOptions Stored IsSortOPtionsStored;
    // Read-only table: disable delete, change edit button
    property DisplayReadOnly : Boolean read FReadOnly write SetReadOnly;
      // Called after the table has completed drawing
    Property AfterBodyDraw : TNotifyEvent Read FAfterBodyDraw Write FAfterBodyDraw;
   end;

  TDBBootstrapTableWidget = class(TCustomDBBootstrapTableWidget)
  Published
    Property StylingClasses;
    property Columns;
    property DataSource;
    Property Options;
    Property ViewOptions;
    Property PaginationOptions;
    Property SearchOptions;
    Property SortOptions;
    property DisplayReadOnly;
    Property AfterBodyDraw;
  end;

Const
  DefaultViewOptions = [bvoDetailView,bvoDetailViewIcon,
                        bvoShowButtonIcons,bvoShowColumns,
                        bvoShowColumnsToggleAll,bvoShowToggle,bvoShowHeader,
                        bvoShowRefresh,bvoShowFooter,bvoShowFullscreen,
                        bvoShowSearchButton,bvoShowClearButton];
  DefaultPaginationOptions = [bpoPagination];
  DefaultSortOptions = [booSortable];
  DefaultSearchOptions = [bsoSearch,bsoSearchOnEnterKey,bsoSearchTimeOut];
  DefaultTableOptions = [btoClickToSelect,btoSingleSelect,btoDetailViewByClick];


implementation

uses jsondataset;

{ TTableDataLink }

constructor TTableDataLink.Create(aTable: TCustomDBBootstrapTableWidget);
begin
  FTable:=aTable;
end;

procedure TTableDataLink.ActiveChanged;
begin
  inherited ActiveChanged;
  FTable.ActiveChanged;
end;

{ TStylingClasses }

function TStylingClasses.GetReadonlyEditClass: String;
begin
  Result:=FReadonlyEditClass;
  if Result='' then
    Result:=InfoClass;
end;

function TStylingClasses.IsReadonlyStored: Boolean;
begin
  Result:=(FReadonlyEditClass<>'');
end;

constructor TStylingClasses.Create(aWidget: TCustomDBBootstrapTableWidget);
begin
  FWidget:=aWidget;
  ButtonClass:='btn btn-secondary btn-sm btn-outline';
  EditClass:='bi bi-pencil';
  DeleteClass:='bi bi-trash';
  InfoClass:='bi bi-info-circle';
  CheckBoxClass:='form-check-input';
end;

procedure TStylingClasses.Assign(Source: TPersistent);

Var
  IC : TStylingClasses absolute Source;

begin
  if Source is TStylingClasses then
    begin
    FCheckBoxClass:=IC.FCheckBoxClass;
    FButtonClass:=IC.FButtonClass;
    FDeleteClass:=IC.FDeleteClass;
    FEditClass:=IC.FEditClass;
    FInfoClass:=IC.FInfoClass;
    FReadonlyEditClass:=IC.FReadonlyEditClass;
    end
  else
    inherited Assign(Source);
end;


  { TBSTableColumn }

procedure TBSTableColumn.Assign(Source: TPersistent);
var
  Src: TBSTableColumn absolute source;
begin
  if Source is TBSTableColumn then
  begin
    FSelectable:=Src.Selectable;
    FieldName:=Src.FieldName;
    Title:=Src.Title;
    RenderMode:=Src.RenderMode;
    ButtonType:=Src.ButtonType;
    Width:=Src.Width;
    CSSClassName:=Src.CSSClassName;
    CheckBoxClassName:=Src.CheckBoxClassName;
    OnCustomFormat:=Src.OnCustomFormat;
    Searchable := Src.Searchable;
    Sortable := Src.Sortable;
    ButtonURL := Src.ButtonURL;
    ButtonURLTarget := Src.ButtonURLTarget;
    ButtonIconClass := Src.ButtonIconClass;
    Formatting := Src.Formatting;
    DtType := Src.DtType;
    OnGetSortValue := Src.OnGetSortValue;
    ExtraAttributes := Src.ExtraAttributes;
  end
  else
    inherited;
end;

constructor TBSTableColumn.Create(aOwner: TCollection);
begin
  inherited;
  FRenderMode := crmText;
  FVisible := True;
  FSortable := True;
  FSearchable := True;
end;

function TBSTableColumn.GetDisplayName: String;
begin
  Result:=FieldName;
  if Result='' then
    Result:=Title;
  if Result='' then
    Result:=Inherited GetDisplayName;
end;

function TBSTableColumn.GetTitle: string;
begin
  Result := FTitle;
  if Result='' then
    Result:=FieldName;
end;

{ TBSTableColumns }

function TBSTableColumns.Add(const aName: string): TBSTableColumn;
begin
  if IndexOfColumn(aName) <> -1 then
    raise EBootstrapTable.CreateFmt('Column "%s" already exists!', [aName]);
  Result := Add;
  Result.FieldName := aName;
end;

function TBSTableColumns.Add: TBSTableColumn;
begin
  Result:=TBSTableColumn(inherited Add);
end;

function TBSTableColumns.ColumnByName(const aName: string): TBSTableColumn;
var
  iIdx: Integer;
begin
  iIdx := IndexOfColumn(aName);
  if iIdx > -1 then
    Result := DoGetColumn(iIdx)
  else
    Result := nil;
end;

function TBSTableColumns.DoGetColumn(const aIndex: Integer): TBSTableColumn;
begin
  Result := Items[aIndex] as TBSTableColumn;
end;

function TBSTableColumns.IndexOfColumn(const aName: string): Integer;
begin
  Result := Pred(Count);
  while (Result >= 0) and (not SameText(DoGetColumn(Result).FieldName, aName)) do
    Dec(Result);
end;

{ TCustomDBBootstrapTableWidget }

Type
 TRowsDataset = Class(TBaseJSONDataset)
 Public
   Property Rows;
 end;

function TCustomDBBootstrapTableWidget.GetDataFromDataset : TJSArray;

Var
  Rec : TJSObject;
  I : Integer;
  F : TField;
  v : JSValue;

begin
  if Datasource.Dataset is TBaseJSONDataset then
    Result:=TRowsDataset(Datasource.Dataset).Rows
  else
    With Datasource.Dataset do
      begin
      DisableControls;
      Result:=TJSArray.New;
      While not EOF do
        begin
        Rec:=TJSObject.new;
        For I:=0 to Fields.Count-1 do
          begin
          F:=Fields[i];
          if F.IsNull then
            V:=Null
          else
            Case F.DataType of
              ftUnknown,
              ftMemo,
              ftFixedChar,
              ftString : V:=F.AsString;
              ftInteger : V:=F.AsInteger;
              ftAutoInc,
              ftLargeInt : V:=F.AsLargeInt;
              ftBoolean : V:=F.AsBoolean;
              ftFloat : V:=F.AsFloat;
              ftDate : V:=FormatDateTime('yyyy-mm-dd',F.AsDateTime);
              ftTime : V:=FormatDateTime('hh:nn:ss.zzz',F.AsDateTime);
              ftBlob : V:=F.AsString;
              ftVariant : V:=F.AsJSValue
            end;
          Rec[F.FieldName]:=V;
          end;
        Next;
        end;
      end;
end;

function TCustomDBBootstrapTableWidget.GetData: TJSArray;
begin
  if FData=Nil then
    FData:=GetDataFromDataset;
  Result:=FData;
end;

function TCustomDBBootstrapTableWidget.GetDatasource : TDataSource;
begin
  Result:=FLink.DataSource;
end;


procedure TCustomDBBootstrapTableWidget.SetDatasource(AValue: TDataSource);
begin
  FLink.DataSource:=aValue
end;

procedure TCustomDBBootstrapTableWidget.SeTStylingClasses(AValue: TStylingClasses);
begin
  if FStylingClasses=AValue then Exit;
  FStylingClasses.Assign(AValue);
end;


function TCustomDBBootstrapTableWidget.FinishColumn(aColumn: TBootstrapTableColumn; aTitle, aWidthUnits: String; aWidth : Integer; aVisible: Boolean; aClassName: String;
  aSearchable: Boolean; aSortable: Boolean): TBootstrapTableColumn;
begin
  if aTitle<>'' then
    aColumn.Title:=aTitle;
  if aWidthUnits<>'' then
    aColumn.widthUnit:=aWidthUnits;
  if aWidth>0 then
    aColumn.width:=aWidth;
  if not aVisible then
    aColumn.Visible:=False;
  if aClassName <> '' then
    aColumn.class_ := aClassName;
  aColumn.Searchable := aSearchable;
  aColumn.Sortable := aSortable;
  Result:=aColumn;
end;

function TCustomDBBootstrapTableWidget.CreateCol(aFieldName, aTitle: string;
  aWidthUnits: String; aWidth: Integer; aVisible: Boolean; aClassName: String;
  aSearchable: Boolean; aSortable: Boolean): TBootstrapTableColumn;
begin
  Result := TBootstrapTableColumn.New;
  Result.Field:=aFieldName;
  FinishColumn(Result, aTitle, aWidthUnits, aWidth, aVisible, aClassName, aSearchable, aSortable);
end;

function TCustomDBBootstrapTableWidget.MakeCustomFormatCol(aCol: TBootstrapTableColumn; aTableCol: TBSTableColumn): TBootstrapTableColumn;

  function renderCallBack(Data: JSValue; row: TJSObject; RowIndex : Integer; Field : string): JSValue;

  begin
    Result:='';
    if Assigned(aTableCol.OnCustomFormat) then
      aTableCol.OnCustomFormat(aTableCol, Data, row, rowindex, field, Result)
    else
      Result:='';
  end;

begin
  Result:=aCol;
  Result.Formatter := @renderCallBack;
end;

function TCustomDBBootstrapTableWidget.MakeTransformValueCol(aCol: TBootstrapTableColumn; aTableCol: TBSTableColumn): TBootstrapTableColumn;

var
   F:TDataTablesFieldMap;

  function renderCallBack(Data: JSValue; row: TJSObject; RowIndex : Integer; Field : string): JSValue;

  Var
    Res : String;

  begin
    Res:='';
    if Assigned(aTableCol.OnTransformValue) then
      aTableCol.OnTransformValue(aTableCol, F, Res);
    Result:=Res;
  end;

begin
  Result:=aCol;
  F:=TDataTablesFieldMap.Create;
  F.FFieldDefs:=Datasource.DataSet.FieldDefs;
  Result.Formatter := @renderCallBack;
end;

function TCustomDBBootstrapTableWidget.MakeDateTimeRenderCol(aCol: TBootstrapTableColumn; aTableCol: TBSTableColumn): TBootstrapTableColumn;

  function renderCallBack(Data: JSValue; row: TJSObject; RowIndex : Integer; Field : string): JSValue;

  var
    DT : TDateTime;

  begin
    if aTableCol.Formatting = '' then
      Result := String(Data)
    else
      begin
      if IsNull(Data) then
        Result:=''
      else
        begin
        Dt:=JSDateToDateTime(TJSDate(Data));
        if Dt<=100 then
          Result:=''
        else  
          Result := FormatDateTime(aTableCol.Formatting, DT)
        end;
      end;
    if aTableCol.ExtraAttributes <> '' then
      Result := Format('<span %s>%s</span>', [ReplaceMoustache(row, aTableCol.ExtraAttributes), Result]);
  end;

begin
  Result:=aCol;
  Result.Formatter := @renderCallBack;
end;

function TCustomDBBootstrapTableWidget.MakeNumericRenderCol(aCol: TBootstrapTableColumn; aTableCol: TBSTableColumn): TBootstrapTableColumn;

  function renderCallBack(Data: JSValue; row: TJSObject; RowIndex : Integer; Field : string): JSValue;

  begin
    if aTableCol.Formatting <> '' then
      if IsNull(Data) then
        Result:=''
      else
        Result := FormatFloat(aTableCol.Formatting, Double(Data))
    else
      Result := Data;
    if aTableCol.ExtraAttributes <> '' then
      Result := Format('<span %s>%s</span>', [ReplaceMoustache(row, aTableCol.ExtraAttributes), String(Result)]);
  end;

begin
  Result:=aCol;
  Result.Formatter := @renderCallBack;
end;



function TCustomDBBootstrapTableWidget.MakeButtonCol(aCol: TBootstrapTableColumn; aTableCol: TBSTableColumn): TBootstrapTableColumn;

  function renderCallBack(Data: JSValue; row: TJSObject; RowIndex : Integer; Field : string): JSValue;

  var
    classnames, tagName,sIcon: string;
    sUrl, sExtraAttributes: string;

  begin
    classnames:= StylingClasses.ButtonClass;
    Case aTableCol.ButtonType of
      cbtInfo:
        sIcon := StylingClasses.InfoClass;
      cbtEdit:
        sIcon := IfThen(DisplayReadOnly,StylingClasses.EditClass,StylingClasses.ReadonlyEditClass);
      cbtDelete:
        sIcon := StylingClasses.DeleteClass;
      cbtCustom:
        sIcon := aTableCol.ButtonIconClass;
    End;
    sUrl := ReplaceMoustache(row, aTableCol.ButtonURL);

    sExtraAttributes := ReplaceMoustache(row, aTableCol.ExtraAttributes);

    if (aTableCol.ButtonType=cbtDelete) and DisplayReadOnly then
      begin
      tagName:='div';
      Surl:='';
      classnames:=classnames+' disabled';
      end
    else
      begin
      tagName:='a';
      if aTableCol.ButtonURLTarget<>'' then
        sExtraAttributes:=sExtraAttributes+' target="'+aTableCol.ButtonURLTarget+'"';
      sUrl:=Format('href="%s"',[sUrl]);
      end;
    Result := Format('<%s class="%s" %s %s><i class="%s"></i></%s> ',
        [tagName, classnames, sUrl, sExtraAttributes, sIcon, tagName])
  end;

begin
  Result:=aCol;
  Result.Formatter := @renderCallBack;
end;

function TCustomDBBootstrapTableWidget.MakeCheckBoxCol(aCol: TBootstrapTableColumn; aTableCol: TBSTableColumn): TBootstrapTableColumn;

  function renderCallBack(Data: JSValue; row: TJSObject; RowIndex : Integer; Field : string): JSValue;

  var
    aClassProp, aIsCheckedProp, sExtraAttributes: String;


  begin
    aClassProp:=aTableCol.CheckBoxClassName;
    if aClassProp='' then
      aClassProp:=StylingClasses.CheckBoxClass;
    if aClassProp<>'' then
      aClassProp:=' class="' + aClassProp + '"';
    if (row.Properties[aCol.Field] <> '0') then
      aIsCheckedProp := ' checked';
    sExtraAttributes := ReplaceMoustache(row, aTableCol.ExtraAttributes);
    Result := Format('<input type="checkbox" %s %s %s>', [aClassProp, aIsCheckedProp, sExtraAttributes])
  end;

begin
  Result:=aCol;
  aCol.Formatter:=@renderCallBack;
end;

constructor TCustomDBBootstrapTableWidget.Create(aOwner: TComponent);
begin
  inherited;
  FColumns := TBSTableColumns.Create(TBSTableColumn);
  FStylingClasses:=TStylingClasses.Create;
  FLink:=TTableDataLink.Create(Self);
end;

procedure TCustomDBBootstrapTableWidget.CreateDefaultColumns(DoClear: Boolean = True);
var
  I: Integer;
  F: TField;
  col: TBSTableColumn;
begin
  if DoClear then
    Columns.Clear;
  if not Assigned(FDataSet) then
    exit;
  for I := 0 to FDataSet.Fields.Count - 1 do
  begin
    F:=FDataSet.Fields[I];
    if DoClear or (Columns.IndexOfColumn(F.FieldName)=-1) then
    begin
      col:=Columns.Add(F.FieldName);
      With col do
      begin
        Title := F.DisplayName;
        Case F.DataType of
          ftFloat, ftInteger:
            RenderMode := TColumnRenderMode.crmNumeric;
          ftBoolean:
            RenderMode := TColumnRenderMode.crmCheckBox;
        end;
      end;
    end;
  end;
end;

procedure TCustomDBBootstrapTableWidget.DefineSortingFunctions;
var
  I: Integer;
  aCol: TBSTableColumn;
  Data: JSValue;
  aSortValue: Integer;
  aSortFunction: TSortOrderMethod;
  aDefinedTypes: Array of String;
  aTypeName: String; // used in asm.

begin
  for I := 0 to FColumns.Count - 1 do
  begin
    aCol := FColumns[I];
    if (aCol.DtType <> '') and (not MatchStr(aCol.DtType, aDefinedTypes)) then
    begin
      SetLength(aDefinedTypes, Length(aDefinedTypes)+1);
      aDefinedTypes[High(aDefinedTypes)] := aCol.DtType;
      aTypeName := aCol.DtType + '-pre';
      aSortFunction := aCol.OnGetSortValue;
      asm
        $.fn.dataTable.ext.type.order[aTypeName] = function ( Data ) {
        end;
        aSortValue := aSortFunction(Self, Data);
        asm
        return aSortValue;
         };
      end;
      // Silence compiler warning
      if aSortValue<>0 then;
      if aTypeName<>'' then;
    end;
  end;
end;

destructor TCustomDBBootstrapTableWidget.Destroy;
begin
  FreeAndNil(FLink);
  FreeAndNil(FStylingClasses);
  FreeAndNil(FColumns);
  inherited;
end;

function TCustomDBBootstrapTableWidget.GetColumnArray: TBootstrapTableColumnArray;
var
  I : Integer;
  aCol: TBSTableColumn;
  aColumn: TBootstrapTableColumn;
begin
  SetLength(Result, FColumns.Count);

  for I := 0 to FColumns.Count - 1 do
  begin
    aCol := FColumns[I];
    aColumn := CreateCol(aCol.FieldName, aCol.Title, aCol.WidthUnits, aCol.Width, aCol.Visible, aCol.CSSClassName, aCol.Searchable, aCol.Sortable);
    case aCol.RenderMode of
      crmCheckBox:
        MakeCheckBoxCol(aColumn, aCol);
      crmButton:
        MakeButtonCol(aColumn, aCol);
      crmCustom:
        MakeCustomFormatCol(aColumn, aCol);
      crmTransformedValue:
        MakeTransformValueCol(aColumn, aCol);
      crmDateTime:
        MakeDateTimeRenderCol(aColumn, aCol);
      crmNumeric:
        MakeNumericRenderCol(aColumn, aCol);
    end;
    Result[I] := aColumn;
  end;
end;

procedure TCustomDBBootstrapTableWidget.ApplyPaginationOptions(aOptions: TBootstrapTableOptions);

begin
  With aOptions do
    begin
    pagination:=bpoPagination in PaginationOptions;
    paginationLoop:=bpoLoop in PaginationOptions;
    paginationUseIntermediate:=bpoUseIntermediate in PaginationOptions;
    showExtendedPagination:=bpoExtended in PaginationOptions;
    showPaginationSwitch:=bpoShowSwitch in PaginationOptions;
    end;
end;

procedure TCustomDBBootstrapTableWidget.ApplyViewOptions(aOptions: TBootstrapTableOptions);

begin
  With aOptions do
    begin
    cardView:=bvoCardview in ViewOptions;
    checkboxHeader:=bvoCheckboxHeader in ViewOptions;
    detailView:=bvoDetailView in ViewOptions;
    detailViewIcon:=bvoDetailViewIcon in ViewOptions;
    showButtonIcons:=bvoShowButtonIcons in ViewOptions;
    showButtonText:=bvoShowButtonText in ViewOptions;
    showColumns:=bvoShowColumns in ViewOptions;
    showColumnsToggleAll:=bvoShowColumnsToggleAll in ViewOptions;
    showToggle:=bvoShowToggle in ViewOptions;
    showHeader:=bvoShowHeader in Viewoptions;
    smartDisplay:=bvoSmartDisplay in ViewOptions;
    showRefresh:=bvoShowRefresh in ViewOptions;
    showFooter:=bvoShowFooter in ViewOptions;
    showFullscreen:=bvoShowFullscreen in ViewOptions;
    virtualScroll:=bvoVirtualScroll in ViewOptions;
    showSearchButton:=bvoShowSearchButton in ViewOptions;
    showSearchClearButton:=bvoShowClearButton in ViewOptions;
    end;
end;

procedure TCustomDBBootstrapTableWidget.ApplySearchOptions(aOptions: TBootstrapTableOptions);

begin
  With aOptions do
    begin
    search:=bsoSearch in SearchOptions;
    searchOnEnterKey:=bsoSearchOnEnterKey in SearchOptions;
    searchTimeOut:=bsoSearchTimeOut in SearchOptions;
    strictSearch:=bsoStrictSearch in SearchOptions;
    trimOnSearch:=bsoTrimOnSearch in SearchOptions;
    visibleSearch:=bsoVisibleSearch in SearchOptions;
    end;
end;

procedure TCustomDBBootstrapTableWidget.ConfigureOptions(
  aOptions: TBootstrapTableOptions);
Var
  URL: String;
begin
  aOptions.Columns := GetColumnArray;
  aOptions.search := FShowSearch;
  ApplySearchOptions(aOptions);
  ApplyPaginationOptions(aOptions);
  ApplyViewOptions(aOptions);
  aOptions.Data := Data;
  aOptions.onPostBody:=@DoAfterBodyDraw;
end;


function TCustomDBBootstrapTableWidget.ReplaceMoustache(row: TJSObject;
  aMoustacheString: String): String;
var
  E: TJSRegexp;

  function Moustache(const match, pattern: string; offset: Integer; AString: string): string;
  begin
    Result:=String(Row[pattern]);
  end;

begin
  E := TJSRegexp.New('{{([_\w]*)}}', 'g');
  Result := TJSString(aMoustacheString).Replace(E, @Moustache);
end;


procedure TCustomDBBootstrapTableWidget.SetData(const aData: TJSArray);
begin
  if FData=aData then
    exit;
  FData:=aData;
  if IsRendered then
    Refresh;
end;


procedure TCustomDBBootstrapTableWidget.SetReadOnly(const Value: Boolean);
begin
  FReadOnly := Value;
  if IsRendered then
    Refresh;
end;

procedure TCustomDBBootstrapTableWidget.SetTableOptions(AValue: TBSTableOptions
  );
begin
  if FTableOptions=AValue then Exit;
  FTableOptions:=AValue;
  if IsRendered then
    Refresh;
end;

procedure TCustomDBBootstrapTableWidget.SetTablePaginationOptions(
  AValue: TBSTablePaginationOptions);
begin
  if FTablePaginationOptions=AValue then Exit;
  FTablePaginationOptions:=AValue;
  if IsRendered then
    Refresh;
end;

procedure TCustomDBBootstrapTableWidget.SetTableSearchOptions(
  AValue: TBSTableSearchOptions);
begin
  if FTableSearchOptions=AValue then Exit;
  FTableSearchOptions:=AValue;
  if IsRendered then
    Refresh;
end;

procedure TCustomDBBootstrapTableWidget.SetTableSortOptions(
  AValue: TBSTableSortOptions);
begin
  if FTableSortOptions=AValue then Exit;
  FTableSortOptions:=AValue;
  if IsRendered then
    Refresh;
end;

procedure TCustomDBBootstrapTableWidget.SetTableViewOptions(
  AValue: TBSTableViewOptions);
begin
  if FTableViewOptions=AValue then Exit;
  FTableViewOptions:=AValue;
  if IsRendered then
    Refresh;
end;

procedure TCustomDBBootstrapTableWidget.ActiveChanged;
begin
  Refresh
end;

procedure TCustomDBBootstrapTableWidget.DoAfterBodyDraw(aData: JSValue);
begin
  if Assigned(FAfterBodyDraw) then
    FAfterBodyDraw(Self);
end;


procedure TCustomDBBootstrapTableWidget.ShowLoading;
var
  aTable: TJSElement;
begin
  // First rudimentary implementation, only changes HTML
  aTable := document.GetElementByID(FGridID);
  if aTable <> nil then
  begin
    if aTable.lastElementChild <> nil then
    begin
      if aTable.lastElementChild.localName = 'tbody' then
        aTable.lastElementChild.innerHTML :=
          '<tr><td colspan="100%" style="text-align: center;"> <i class="fa fa-spinner fa-spin fa-3x fa-fw"></i><span class="sr-only">Loading...</span></td></tr>';
      if aTable.lastElementChild.localName = 'thead' then
        aTable.append
          ('<tbody><tr><td colspan="100%" style="text-align: center;"> <i class="fa fa-spinner fa-spin fa-3x fa-fw"></i><span class="sr-only">Bezig met laden..</span></td></tr></tbody>');
    end;
  end;
end;


procedure TCustomDBBootstrapTableWidget.UnRender;

begin
  JQuery('#'+ElementID).BootstrapTable('destroy');
end;

procedure TCustomDBBootstrapTableWidget.ApplyWidgetSettings(aElement: TJSHTMLElement);

var
  aOptions : TBootstrapTableOptions;

begin
  aOptions:=TBootstrapTableOptions.New;
  ConfigureOptions(aOptions);
  JQuery(aElement).BootstrapTable(aOptions);
  // JQuery('#'+ElementID).BootstrapTable('refresh');
end;

function TCustomDBBootstrapTableWidget.IsOptionsStored: Boolean;
begin
  Result:=Options<>DefaultTableOptions;
end;

function TCustomDBBootstrapTableWidget.IsPaginationOptionsStored: Boolean;
begin
  Result:=PaginationOptions<>DefaultPaginationOptions;
end;

function TCustomDBBootstrapTableWidget.IsSearchOptionsStored: Boolean;
begin
  Result:=SearchOptions<>DefaultSearchOptions;
end;

function TCustomDBBootstrapTableWidget.IsSortOPtionsStored: Boolean;
begin
  Result:=SortOptions<>DefaultSortOptions;
end;

function TCustomDBBootstrapTableWidget.IsViewOptionsStored: Boolean;
begin
  Result:=ViewOptions<>DefaultViewOptions;
end;

{ TDataTablesFieldMap }

function TDataTablesFieldMap.GetValueByName(S: String): JSValue;
Var
  fld: TFieldDef;
begin
  fld:=FFieldDefs.Find(S);
  if Assigned(fld) then
    Result:=FRow[fld.Index]
  else
    Result:=Null;
end;

end.
