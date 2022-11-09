{
    This file is part of the Pas2JS run time library.
    Copyright (c) 2020 by the Pas2JS development team.

    Import classes for datatables (https://www.datatables.net)

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}


unit libdatatables;

{$mode objfpc}
{$modeswitch externalclass}

interface

uses types, js, web, libjquery;


Const
  OrderAsc = 'asc';
  OrderDesc = 'desc';

  PagingNumbers = 'numbers';
  PagingSimple = 'simple';
  PagingSimpleNumbers = 'simple_numbers';
  PagingFull = 'full';
  PagingFullNumbers = 'full_numbers';
  PagingFirstLastNumbers = 'first_last_numbers';

  AutoFillFocusClick = 'click';
  AutoFillFocusFocus = 'focus';
  AutoFillFocushover = 'hover';

  FixedColumnsHeightMatchNone = 'none';
  FixedColumnsHeightMatchSemiAuto = 'semiauto';
  FixedColumnsHeightMatchAuto = 'auto';

  SelectOptionsItemsRow = 'row';
  SelectOptionsItemsColumn = 'column';
  SelectOptionsItemsCell = 'cell';

  SelectOptionsStyleAPI = 'api';
  SelectOptionsStyleSingle = 'single';
  SelectOptionsStyleMulti = 'multi';
  SelectOptionsStyleMultiShitf = 'multi+shift';






Type
  // Forward definitions
  TJSDataTableAPI = class;
  TJSDTOButton = Class;
  TJSDataTableEditor = TJSObject;

  TJSDTOSearchOptions = Class external name 'Object' (TJSObject)
    search : string;
    caseInsensitive : Boolean;
    regex : boolean;
    smart : boolean;
  end;
  TJSDTOSearchOptionsDynArray = Array of TJSDTOSearchOptions;

  TJSDTOLanguagePaginateOptions = Class external name 'Object' (TJSObject)
    first : string;
    previous : string;
    next : string;
    last : string;
  end;

  TJSDTOLanguageAriaOptions = Class external name 'Object' (TJSObject)
    paginate : TJSDTOLanguagePaginateOptions;
    sortAscending : string;
  end;

  TJSDTOLanguageAutoFillOptions = Class external name 'Object' (TJSObject)
    button : string;
    cancel : string;
    fill : string;
    fillHorizontal : string;
    fillVertical : string;
    increment : string;
    info : string;
  end;


  TJSDTOLanguageSelectOptions = Class external name 'Object' (TJSObject)
    cells : string;
    cellsObj : TJSObject; external name 'cells';
    columns : string;
    columnsObj : TJSObject; external name 'columns';
    rows : string;
    rowsObj : TJSObject; external name 'rows';
  end;


  TJSDTOLanguageOptions = Class external name 'Object' (TJSObject)
    aria : TJSDTOLanguageAriaOptions;
    autoFill : TJSDTOLanguageAutoFillOptions;
    decimal : string;
    emptyTable : String;
    info : String;
    infoEmpty : String;
    infoFiltered : String;
    infoPostFix : string;
    lengthMenu : string;
    loadingRecords : String;
    paginate : TJSDTOLanguagePaginateOptions;
    processing : string;
    search : string;
    searchPlaceholder : string;
    thousands : string;
    url : string;
    zeroRecords : string;
    select : TJSDTOLanguageSelectOptions;
  end;

  TJSDTOAutoFillOptions = Class external name 'Object' (TJSObject)
    alwaysAsk : Boolean;
    columns : string;
    columnsIndex : TIntegerDynArray; external name 'columns';
    editor : TJSDataTableEditor;
    enable : boolean;
    focus : string;
    focusNull : jsValue external name 'focus';
    horizontal : boolean;
    update : boolean;
    vertical : boolean;
  end;

  TJSDTOButtonActionCallBack = reference to procedure (e : TJSEvent; dt : TJSDataTableAPI; node : TJQuery; Config : TJSDTOButton);
  TJSDTOButtonAvailableCallBack = reference to function (dt : TJSDataTableAPI; Config : TJSDTOButton) : boolean;
  TJSDTOButtonDestroyCallBack = reference to procedure (dt : TJSDataTableAPI; node : TJQuery; Config : TJSDTOButton);
  TJSDTOButtonInitCallBack = reference to procedure (dt : TJSDataTableAPI; node : TJQuery; Config : TJSDTOButton);
  TJSDTOButtonTitleCallBack = reference to function (dt : TJSDataTableAPI; node : TJQuery; Config : TJSDTOButton) : string;

  TJSDataTableOptionsButtonAttr = Class external name 'Object' (TJSObject)
    title : String;
    id : string;
  end;

  TJSDTOButtonKeyOptions = Class external name 'Object' (TJSObject)
    key : string;
    shift : Boolean;
    altkey : Boolean;
    ctrlKey : Boolean;
    metaKey : Boolean;
  end;

  TJSDataTableButtonDomBase = Class external name 'Object' (TJSObject)
    tag : string;
    classname : string;
  end;

  TJSDataTableButtonDomButtonOptions = Class external name 'Object' (TJSDataTableButtonDomBase)
    disabled : string;
    active : string;
  end;

  TJSDataTableButtonDomOption = Class external name 'Object' (TJSObject)
    button : TJSDataTableButtonDomButtonOptions;
    buttonObj : TJSObject; external name 'button';
    buttonLiner : TJSDataTableButtonDomBase;
    buttonLinerObj : TJSObject; external name 'buttonLiner';
    collection : TJSDataTableButtonDomBase;
    collectionObj : TJSObject; external name 'collection';
    container : TJSDataTableButtonDomBase;
    containerObj : TJSObject; external name 'container';
  end;

  TJSDTOButton = Class external name 'Object' (TJSObject)
    action : TJSDTOButtonActionCallBack;
    attr : TJSDataTableOptionsButtonAttr;
    available : TJSDTOButtonAvailableCallBack;
    className : string;
    destroy : TJSDTOButtonDestroyCallBack;
    enabled : boolean;
    extend : string;
    init : TJSDTOButtonInitCallBack;
    key : string;
    keyObj : TJSDTOButtonKeyOptions;  external name 'key';
    name : string;
    namespace : string;
    tag : string;
    text : string;
    titleAttr : string;
    titleAttrCallBack : TJSDTOButtonTitleCallBack; external name 'titleAttr';
    dom : TJSDataTableButtonDomOption;
  end;

  TJSDTOButtonDynArray = Array of TJSDTOButton;

  TJSDTOButtons = Class external name 'Object' (TJSObject)
    buttonsStringArray : TStringDynArray; external name 'buttons';
    buttonsMixedArray : TJSValueDynArray; external name 'buttons';
    ButtonsObjArray : TJSDTOButtonDynArray; external name 'buttons';
    bame : string;
  end;

  TJSDTOColReorderOptions = Class external name 'Object' (TJSObject)
    enable : boolean;
    fixedColumnsLeft : Integer;
    fixedColumnsRight : Integer;
    order : TIntegerDynArray;
    realtime : Boolean;
  end;

  TJSDTOFixedColumnsOptions = Class external name 'Object' (TJSObject)
    heightMatch : string;
    leftColumns : Integer;
    rightColumns : Integer;
  end;

  TJSDTOFixedHeaderOptions =  Class external name 'Object' (TJSObject)
    footer : Boolean;
    footerOffset : Integer;
    header : Boolean;
    headerOffset : Integer;
  end;

  TJSDTOKeyTableOptions =  Class external name 'Object' (TJSObject)
    blurable : boolean;
    className : string;
    clipboardOrthogonal : String;
    columns : string;
    editOnFocus : Boolean;
    editor : TJSDataTableEditor;
    focus : string;
    keysArray : TStringDynArray; external name 'keys';
    keysNull : jsValue; external name 'keys';
    tabIndex : integer;
  end;

  TJSDTOResponsiveBreakPoint = record
    name : string;
    width : integer;
  end;
  TJSDTOResponsiveBreakPointDynArray = Array of TJSDTOResponsiveBreakPoint;

  TJSRenderColumns = record
    title : string;
    data : string;
    hidden : boolean;
    columnIndex : integer;
    rowIndex : Integer;
  end;
  TJSRenderColumnsDynArray = Array of TJSRenderColumns;

  TJSDTOResponsiveDetailsDisplayCallbackRender = Function : String;
  TJSDTOResponsiveDetailsDisplayCallback = reference to function (row : TJSDataTableAPI; upate : boolean; Render :  TJSDTOResponsiveDetailsDisplayCallbackRender) : Boolean;


  TJSDTOResponsiveDetailsRenderBoolCallback = reference to function (api : TJSDataTableAPI; RowIndex : Integer; Columns :  TJSRenderColumnsDynArray) : Boolean;
  TJSDTOResponsiveDetailsRenderStringCallback = reference to function (api : TJSDataTableAPI; RowIndex : Integer; Columns :  TJSRenderColumnsDynArray) : String;

  TJSDTOResponsiveDetails = Class external name 'Object' (TJSObject)
    display : TJSDTOResponsiveDetailsDisplayCallback;
    renderBool : TJSDTOResponsiveDetailsRenderBoolCallback; external name 'render';
    renderString : TJSDTOResponsiveDetailsRenderBoolCallback; external name 'render';
    target : integer;
    targetStr : String; external name 'target';
    type_ : String; external name 'type';
    orthogonal : string;
  end;

  TJSDTOResponsiveOptions = Class external name 'Object' (TJSObject)
    breakpoints : TJSDTOResponsiveBreakPointDynArray;
    details : boolean;
    detailsObj : TJSDTOResponsiveDetails; external name 'details';
  end;


  TJSDTORowReorderOptions = Class external name 'Object' (TJSObject)
    dataSrc : Integer;
    dataSrcStr : String; external name 'dataSrc';
    editor : TJSDataTableEditor;
    selector : String;
    snapX : Boolean;
    snapXint : integer; external name 'snapX';
    update : boolean;
  end;


  TJSDTOScrollerOptions = Class external name 'Object' (TJSObject)
    boundaryScale : Double;
    displayBuffer : Integer;
    loadingIndicator : Boolean;
    rowHeight : Integer;
    rowHeightStr : String; external name 'rowHeight';
    serverWait : Integer;
  end;

  TJSDTOSelectOptions = Class external name 'Object' (TJSObject)
    blurable : boolean;
    className : string;
    info : Boolean;
    items : string;
    selector : string;
    style : string;
    toggleable : boolean;
  end;

  TJSDTOColumnCreatedCellCallBack = reference to procedure(Cell : TJSNode; cellData,rowData : JSValue; rowIndex,ColIndex : integer);
  TJSDTOColumnObjectDataCallBack = reference to function (Row : TJSObject; aType : string; aSet : JSValue; meta : TJSObject) : jsValue;
  TJSDTOColumnArrayDataCallBack = reference to function (Row : TJSArray; aType : string; aSet : JSValue; meta : TJSObject) : jsValue;
  TJSDTOColumnObjectRenderCallBack = reference to function (data: JSValue; aType : string; row : TJSObject; meta : TJSObject) : jsValue;
  TJSDTOColumnArrayRenderCallBack = reference to function (data: JSValue; aType : string; row : TJSArray; meta : TJSObject) : jsValue;

  TJSDTOColumn = Class external name 'Object' (TJSObject)
    cellType : string;
    className : string;
    contentPadding : string;
    createdCell : TJSDTOColumnCreatedCellCallBack;
    data : String;
    dataInt : Integer; external name 'data';
    dataNull : jsValue ; external name 'data';
    dataObj : TJSObject ; external name 'data';
    dataObjectFunction : TJSDTOColumnObjectDataCallBack ; external name 'data';
    dataArrayFunction : TJSDTOColumnArrayDataCallBack ; external name 'data';
    defaultContent : string;
    name : string;
    orderData : Integer;
    orderDataArray : TIntegerDynArray; external name 'orderData';
    orderDataType : String;
    orderSequence : TStringDynArray;
    orderable : boolean;
    render : Integer;
    renderStr : string; external name 'render';
    renderObj : string; external name 'render';
    renderObjectFunction : TJSDTOColumnObjectRenderCallBack ; external name 'render';
    renderArrayFunction : TJSDTOColumnArrayRenderCallBack ; external name 'render';
    searchable : boolean;
    title : string;
    type_ : string; external name 'type';
    visible : boolean;
    width : string;
    targets : Integer;
    targetsStr : String; external name 'targets';
    targetsIntArray : TIntegerDynArray; external name 'targets';
    targetsStringArray : TStringDynArray; external name 'targets';
  end;
  TJSDTOColumnDynArray = array of TJSDTOColumn;

  TJSDTORowGroupEndRenderCallBack = reference to Function(rows : TJSDataTableAPI; Group : string; level : Integer) : JSValue;

  TJSDTORowGroup = Class external name 'Object' (TJSObject)
    className : string;
    dataSrc : Integer;
    dataSrcStr : String; external name 'dataSrc';
    dataSrcStrArray : TStringDynArray; external name 'dataSrc';
    dataSrcIntegerArray : TIntegerDynArray; external name 'dataSrc';
    emptyDataGroup : String;
    emptyDataGroupNull : JSValue; external name 'emptyDataGroup';
    enable : boolean;
    endClass : String;
    endRenderNull : jsValue; external name 'endRender';
    endRender : TJSDTORowGroupEndRenderCallBack;
    startClassName : string;
    startRenderNull : jsValue; external name 'sraerRender';
    startRender : TJSDTORowGroupEndRenderCallBack;
  end;

  TJSDTOAjaxCallBack = reference to Function (data : TJSObject) : TJSArray;

  TJSDTOAjax = Class external name 'Object' (TJSAjaxSettings)
    dataSrc : string;
    dataSrcCallBack : TJSDTOAjaxCallBack; external name 'dataSrc';
  end;

  TJSDataTableOptions = Class external name 'Object' (TJSObject)
    ajax : string;
    ajaxObj : TJSDTOAjax; external name 'ajax';
    data : TJSArray;
    autoFill : Boolean;
    autoFillObj : TJSDTOAutoFillOptions; external name 'autoFill';
    autoWidth : Boolean;
    deferRender : Boolean;
    info : Boolean;
    lengthChange : Boolean;
    ordering : Boolean;
    paging : Boolean;
    processing : Boolean;
    scrollX : Boolean;
    scrollY : string;
    searching : Boolean;
    serverSide : Boolean;
    stateSave : Boolean;
    deferLoading : Integer;
    deferLoadingArray : TIntegerDynArray; external name 'deferLoading';
    destroy : Boolean;
    displayStart : Integer;
    dom : String;
    lengthMenu : TIntegerDynArray;
    lengthMenuJSValue : TJSValueDynArray; external name 'lengthMenu';
    order : array of TJSValueDynArray;
    orderCellsTop : boolean;
    orderClasses : Boolean;
    orderFixed : TJSValueDynArray;
    orderFixedObj : TJSObject; external name 'orderFixed';
    orderMulti : Boolean;
    pageLength : Integer;
    pagingType : string;
    renderer : String;
    rendererObj : TJSObject; external name 'renderer';
    retrieve : Boolean;
    rowId : String;
    scrollCollapse : Boolean;
    search  : TJSDTOSearchOptions;
    searchCols : TJSDTOSearchOptionsDynArray;
    searchDelay : Boolean;
    stateDuration : Boolean;
    stripeClasses : TStringDynArray;
    tabIndex : Integer;
    language : TJSDTOLanguageOptions;
    languageObj : TJSObject; external name 'language';
    buttons : Boolean;
    buttonsArray : TStringDynArray; external name 'buttons';
    buttonsMixedArray : TJSValueDynArray; external name 'buttons';
    ButtonsObj : TJSDTOButtons; external name 'buttons';
    ButtonsObjArray : TJSDTOButtonDynArray; external name 'buttons';
    colReorder : Boolean;
    colReorderObj : TJSDTOColReorderOptions; external name 'colReorder';
    fixedColumns : Boolean;
    fixedColumnsObj : TJSDTOFixedColumnsOptions; external name 'fixedColumns';
    fixedHeader : Boolean;
    fixedHeaderObj : TJSDTOFixedHeaderOptions; external name 'fixedHeader';
    keyTable : Boolean;
    keyTableObj : TJSDTOKeyTableOptions; external name 'keyTable';
    responsive : Boolean;
    responsiveObj : TJSDTOResponsiveOptions; external name 'responsive';
    rowReorder : Boolean;
    rowReorderObj : TJSDTORowReorderOptions; external name 'rowReorder';
    scroller : Boolean;
    scrollerObj : TJSDTOScrollerOptions; external name 'scroller';
    select : Boolean;
    selectObj : TJSDTOSelectOptions; external name 'select';
    columnDefs : TJSDTOColumnDynArray;
    columns : TJSDTOColumnDynArray;
    rowGroup : Boolean;
    rowGroupObj : TJSDTORowGroup; external name 'rowGroup';
  end;

  TDTAjaxReloadCallBack = reference to procedure (aJSON : TJSObject);

  TDTAjax = Class external name 'Object' (TJSObject)
    function json: TJSObject;
    function params : TJSObject;
    function url : string;
    function url (aURL : String) :  TJSDataTableAPI;
    function reload () : TJSDataTableAPI;
    function reload (callback : TDTAjaxReloadCallBack) :  TJSDataTableAPI;
    function reload (callback : TDTAjaxReloadCallBack; resetPaging : Boolean) :  TJSDataTableAPI;
  end;

  TDTOrderListener = reference to procedure;
  TDTOrderOptions = Class external name 'Object' (TJSObject)
    function fixed : TJSObject;
    function fixed (aOrder : TJSObject) : TJSDataTableAPI;
    function listener (node: TJSNode; Index : Integer; Listener : TDTOrderListener = Nil) : TJSDataTableAPI;
    function listener (node: TJQuery; Index : Integer; Listener : TDTOrderListener = Nil) : TJSDataTableAPI;
    function listener (node: String; Index : Integer; Listener : TDTOrderListener = Nil) : TJSDataTableAPI;
  end;

  TDTPageInfo =  Class external name 'Object' (TJSObject)
    page : integer;
    pages : integer;
    start : integer;
    end_ : integer; external name 'end';
    length : integer;
    recordsTotal : integer;
    recordsDisplay : integer;
    serverSide: Boolean;
  end;

  TDTPageOptions = Class external name 'Object' (TJSObject)
    Function info : TDTPageInfo;
    function len : Integer;
    function len(aLength: Integer) : TJSDataTableAPI;
  end;

  TDTStateOptions = Class external name 'Object' (TJSObject)
    function clear : TJSDataTableAPI;
    function loaded : TJSObject;
    function save : TJSDataTableAPI;
  end;

  TDTSelectorModifier  = Class external name 'Object' (TJSObject)
    order : string;
    page : string;
    search : String;
  end;

  TJSCellSelectorCallback = reference to function(aIndex: Integer; aData : JSValue; aCell: TJSNode) : Boolean;
  TDTCellIndexData = Class external name 'Object' (TJSObject)
    row : integer;
    column : integer;
    columnVisible : Integer;
  end;

  TJSDataTableCellAPI  = Class external name 'Object' (TJSObject)
    function cache() :  TJSDataTableAPI;
    function cache(aType : string) : TJSDataTableAPI ;
    function data : JSValue;
    function data(aValue : JSValue) : TJSDataTableAPI;
    function index : TDTCellIndexData;
    function invalidate : TJSDataTableAPI;
    function invalidate(aSource : string) : TJSDataTableAPI;
    function node : TJSHTMLelement;
    function render : JSValue;
    function render(aType : string) : JSValue;
  end;

  TJSDataTableAPI = Class external name 'DataTable' (TJSObject)
  Public
    context : TJSObject;
    selector : TJSObject;
    ajax : TDTAjax;
    pageAPI : TDTPageOptions; external name 'page';
    orderAPI : TDTOrderOptions ; external name 'order';
    stateAPI : TDTStateOptions; external name 'state';
    function tables() : JSValue;
    function table() : JSValue;
    function draw(paging : boolean) : TJSDataTableAPI;
    function draw(paging : string) : TJSDataTableAPI;
    function page() : integer;
    function page(aPage : integer) : TJSDataTableAPI;
    function rows() : JSValue;
    function row() : JSValue;
    function columns() : JSValue;
    function column() : JSValue;
    function cells() : JSValue;
    function cell() : TJSDataTableCellAPI;
    function cell(Modifier : TDTSelectorModifier) : TJSDataTableCellAPI;
    function cell(Selector : string) : TJSDataTableCellAPI;
    function cell(Selector : string; Modifier : TDTSelectorModifier) : TJSDataTableCellAPI;
    function cell(Selector : TJSNode) : TJSDataTableCellAPI;
    function cell(Selector : TJSNode; Modifier : TDTSelectorModifier) : TJSDataTableCellAPI;
    function cell(Selector : TJQuery) : TJSDataTableCellAPI;
    function cell(Selector : TJQuery; Modifier : TDTSelectorModifier) : TJSDataTableCellAPI;
    function cell(Selector : TJSObject) : TJSDataTableCellAPI;
    function cell(Selector : TJSObject; Modifier : TDTSelectorModifier) : TJSDataTableCellAPI;
    function cell(Selector : TJSValueDynArray) : TJSDataTableCellAPI;
    function cell(Selector : TJSValueDynArray; Modifier : TDTSelectorModifier) : TJSDataTableCellAPI;
    function cell(Selector : TJSCellSelectorCallback) : TJSDataTableCellAPI;
    function cell(Selector : TJSCellSelectorCallback; Modifier : TDTSelectorModifier) : TJSDataTableCellAPI;
    // Defining all combinations would result in 36 function definitions...
    function cell(CellSelector,RowSelector : jsValue; Modifier: TDTSelectorModifier) : TJSDataTableCellAPI;

    function order() : TJSValueDynArray;
    function order(aOrder : TJSValueDynArray) : TJSDataTableAPI; varargs;
    function search() : string;
    function search(aValue : string; aRegexp : Boolean) : TJSDataTableAPI;
    function search(aValue : string; aRegexp : Boolean; Smart : Boolean) : TJSDataTableAPI;
    function search(aValue : string; aRegexp : Boolean; Smart : Boolean; caseIns : Boolean) : TJSDataTableAPI;

    function state() : TJSObject;
    function jquery() : TJQuery; external name '$';
    function on(aEvent : string; aHandler : TJSRawEventHandler) : TJSDataTableAPI;
    function one(aEvent : string; aHandler : TJSRawEventHandler) : TJSDataTableAPI;
    function off(aEvent : string) : TJSDataTableAPI;
    function off(aEvent : string; aHandler : TJSRawEventHandler) : TJSDataTableAPI;
    function clear() : TJSDataTableAPI;
    function settings() : TJSDataTableAPI;
    function init() : TJSObject;
    function data() : TJSDataTableAPI;
    function destroy(remove : Boolean) : TJSDataTableAPI;
    function destroy() : TJSDataTableAPI;
    function i18n(token : string; def : string) : string;
    function i18n(token : string; def : TJSObject) : string;
    function i18n(token : string; def : string; Numeric : Double) : string;
    function i18n(token : string; def : TJSObject; Numeric : Double) : string;
    function i18n(token : string; def : string; Numeric : NativeInt) : string;
    function i18n(token : string; def : TJSObject; Numeric : NativeInt) : string;
  // next getPrototypeOf ...
    function r(arg1: JSValue;arg2: JSValue) : JSValue;
    function any() : JSValue;
    function concat(arg1: JSValue) : JSValue;
    function count() : JSValue;
    function each(arg1: JSValue) : JSValue;
    function eq(arg1: JSValue) : JSValue;
    function filter(arg1: JSValue) : JSValue;
    function flatten() : JSValue;
    function join(arg1: JSValue) : JSValue;
    function indexOf(arg1: JSValue) : JSValue;
    function iterator(arg1: JSValue;arg2: JSValue;arg3: JSValue;arg4: JSValue) : JSValue;
    function lastIndexOf(arg1: JSValue) : JSValue;
    length : double;
    function map(arg1: JSValue) : JSValue;
    function pluck(arg1: JSValue) : JSValue;
    function pop() : JSValue;
    function push(arg1: JSValue) : JSValue;
    function reduce(arg1: JSValue) : JSValue;
    function reduceRight(arg1: JSValue) : JSValue;
    function reverse() : JSValue;
    function shift() : JSValue;
    function slice() : JSValue;
    function sort(arg1: JSValue) : JSValue;
    function splice(arg1: JSValue;arg2: JSValue) : JSValue;
    function toArray() : JSValue;
    function toDollar() : JSValue; external name 'to$';
    function toJQuery() : JSValue;
    function unique() : JSValue;
    function unshift(arg1: JSValue) : JSValue;
  // next getPrototypeOf ...
  end;

  TJSDataTableJQuery = class external name 'JQuery' (TJQuery)
    function api : TJSDataTableAPI;
  end;

  TDataTablesHelper = Class helper for TJQuery
    Function dataTableAPI : TJSDataTableAPI; external name 'DataTable'; overload;
    Function dataTableAPI(options : TJSObject) : TJSDataTableAPI; external name 'DataTable'; overload;
    Function dataTable : TJSDataTableJQuery; external name 'dataTable';
  end;

Implementation

end.
