unit tcHTMLWidgets;

{$mode objfpc}

interface

uses
  Classes, SysUtils, fpcunit, testregistry, web, webwidget, htmlwidgets, tcwidget, js;

Type
  { TTestButtonWidget }

  TTestButtonWidget = Class(TBaseTestWidget)
  private
    FButton: TButtonWidget;
  Protected
    Procedure SetUp; override;
    Procedure TearDown; override;
    Property Button : TButtonWidget Read FButton;
  Published
    Procedure TestTextBeforeRender;
    Procedure TestTextAfterRender;
    Procedure TestTextElementID;
    Procedure TestClick;
  end;

  { TTestLabelWidget }
  TMyLabelWidget = Class(TLabelWidget)
  Public
    Property LabelElement;
  end;

  TTestLabelWidget = Class(TBaseTestWidget)
  private
    FEdit: TTextInputWidget;
    FMy: TMyLabelWidget;
  Protected
    Procedure SetUp; override;
    Procedure TearDown; override;
    Property My : TMyLabelWidget Read FMy;
    Property Edit : TTextInputWidget Read FEdit;
  Published
    Procedure TestPropsBeforeRender;
    Procedure TestPropsAfterRender;
  end;

  { TTestViewPort }

  { TMyViewPort }

  TMyViewPort = Class(TViewPort)
  Public
    Procedure SetParentId;
    Procedure SetParent;
    Procedure SetElementID;
  end;

  TTestViewPort = Class(TBaseTestWidget)
  private
    FMy: TMyViewPort;
  Protected
    Procedure Setup; override;
    Procedure TearDown; override;
    Property My : TMyViewPort Read FMy;
  Published
    Procedure TestInstance;
    Procedure TestHTMLTag;
    Procedure TestElement;
    Procedure TestUnrender;
    Procedure TestNoParent;
    Procedure TestNoElementID;
    Procedure TestNoParentID;
  end;

  { TTestPage }

  { TMyWebPage }

  TMyWebPage = Class(TWebPage)
  Public
    Procedure SetParentId;
    Procedure SetParent;
    Procedure SetElementID;
  end;

  TTestPage = Class(TBaseTestWidget)
  private
    FMy: TMyWebPage;
  Protected
    Function CreateElement(aID : String) : TJSHTMLElement;
    Procedure Setup; override;
    Procedure TearDown; override;
    Property My : TMyWebPage Read FMy;
  Published
    Procedure TestEmpty;
    Procedure TestAsWindow;
    Procedure TestNoParentOK;
    Procedure TestDefaultTag;
  end;

  { TBaseTestInputElement }

  TInputHack = class(TCustomInputWidget)
  Public
    Property Element;
    Property InputElement;
  end;

  TBaseTestInputElement = Class(TBaseTestWidget)
  private
    FMy: TCustomInputWidget;
    function GetInputElement: TJSHTMLInputElement;
  Protected
    // Must be handled in descendent. Called during setup to populate My.
    Function CreateInput : TCustomInputWidget; virtual; abstract;
    // (Re)create my. Calls createinput
    Procedure CreateMy; virtual;
    Procedure Setup; override;
    Procedure TearDown; override;
    // Assert basic properties are correct on the element.
    procedure AssertBaseProps(aType, aValueName, aValue: String; aText: String='');
    Property My : TCustomInputWidget Read FMy;
    Property InputElement : TJSHTMLInputElement Read GetInputElement;
  Published
    Procedure TestEmpty;
    Procedure TestRequiredOnRender;
    Procedure TestReadOnlyOnRender;
    Procedure TestRequiredAfterRender;
    Procedure TestReadOnlyAfterRender;
  end;

  { TTestTextInputElement }

  TTestTextInputElement = Class(TBaseTestInputElement)
  Protected
    FITT: TInputTextType;
    Procedure setup; override;
    Function CreateInput : TCustomInputWidget; override;
    Function MyText : TTextInputWidget;
  Published
    Procedure TestDefaultTextType;
    Procedure TestRender;
    Procedure TestChangeValue;
    Procedure TestChangeName;
    Procedure TestChangeTextType;
    Procedure TestTypePassword;
    Procedure TestTypeNumber;
    Procedure TestAsNumber;
    Procedure TestTypeEmail;
    Procedure TestTypeSearch;
    Procedure TestTypeTel;
    Procedure TestTypeURL;
    Procedure TestTypeColor;
  end;

  { TTestRadioInputElement }

  TTestRadioInputElement = Class(TBaseTestInputElement)
  Protected
    Function CreateInput : TCustomInputWidget; override;
    Function MyRadio : TRadioInputWidget;
  Published
    Procedure TestPropsOnRender;
    Procedure TestPropsAfterRender;
  end;

  TTestCheckboxInputElement = Class(TBaseTestInputElement)
  Protected
    Function CreateInput : TCustomInputWidget; override;
    Function MyCheckbox : TCheckboxInputWidget;
  Published
    Procedure TestPropsOnRender;
    Procedure TestPropsAfterRender;
  end;

  TMyDateInputWidget = Class(TDateInputWidget)
  end;

  { TTestDateInputElement }

  TTestDateInputElement = Class(TBaseTestInputElement)
  Protected
    Function CreateInput : TCustomInputWidget; override;
    Procedure CreateMy; override;
    Function MyDate : TMyDateInputWidget;
  Published
    Procedure TestPropsOnRender;
    Procedure TestPropsAfterRender;
  end;

  TMyFileInputWidget = Class(TFileInputWidget)
  end;

  { TTestFileInputElement }

  TTestFileInputElement = Class(TBaseTestInputElement)
  Protected
    Function CreateInput : TCustomInputWidget; override;
    Procedure CreateMy; override;
    Function MyFile : TMyFileInputWidget;
  Published
    Procedure TestPropsOnRender;
    Procedure TestPropsAfterRender;
  end;

  TMyHiddenInputWidget = Class(THiddenInputWidget)
  end;

  { TTestHiddenInputElement }

  TTestHiddenInputElement = Class(TBaseTestInputElement)
  Protected
    Function CreateInput : TCustomInputWidget; override;
    Function MyHidden : TMyHiddenInputWidget;
  Published
    Procedure TestPropsOnRender;
    Procedure TestPropsAfterRender;
  end;

  { TTestTextAreaElement }
  TMyTextAreaWidget = Class(TTextAreaWidget)
  Public
    Property TextArea;
  end;

  TTestTextAreaElement = Class(TBaseTestWidget)
  private
    FMy: TMyTextAreaWidget;
    function GetArea: TJSHTMLTextAreaElement;
  Protected
    Procedure Setup; override;
    Procedure TearDown; override;
    Property My : TMyTextAreaWidget Read FMy;
    Property Area : TJSHTMLTextAreaElement Read GetArea;
  Published
    Procedure TestEmpty;
    Procedure TestPropsOnRender;
    Procedure TestPropsAfterRender;
  end;

  TMyImageWidget = Class(TImageWidget)
  Public
    Property Element;
  end;

  { TTestImageElement }

  TTestImageElement  = Class(TBaseTestWidget)
  private
    FMy: TMyImageWidget;
    function GetImg: TJSHTMLImageElement;
  Protected
    Procedure Setup; override;
    Procedure TearDown; override;
    Function ThisURL : String;
    Property My : TMyImageWidget Read FMy;
    Property Image : TJSHTMLImageElement Read GetImg;
  Published
    Procedure TestEmpty;
    Procedure TestPropsOnRender;
    Procedure TestPropsAfterRender;
  end;

  TMySelectWidget = Class(TSelectWidget)
  Public
    Property Element;
    Property SelectElement;
    Property Options;
  end;

  { TTestSelectElement }

  TTestSelectElement  = Class(TBaseTestWidget)
  private
    FMy: TMySelectWidget;
    procedure AssertOption(Idx: Integer; aText, aValue: String; Selected: Boolean=False);
    function GetOptions: TJSHTMLOPtionElementArray;
    function GetSelect: TJSHTMLSelectElement;
  Protected
    Procedure Setup; override;
    Procedure TearDown; override;
    Property My : TMySelectWidget Read FMy;
    Property Select : TJSHTMLSelectElement Read GetSelect;
    Property Options : TJSHTMLOPtionElementArray Read GetOptions;
  Published
    Procedure TestEmpty;
    Procedure TestPropsOnRender;
    Procedure TestPropsAfterRender;
    Procedure TestMultiSelect;
    Procedure TestNoSelectedIndex;
  end;

  TMyTextWidget = Class(TTextWidget)
  Public
    Property Element;
    Property ParentElement;
  end;

  { TTestTextWidget }

  TTestTextWidget = Class(TBaseTestWidget)
  private
    FMy: TMyTextWidget;
  Protected
    Procedure Setup; override;
    Procedure TearDown; override;
    Property My : TMyTextWidget Read FMy;
  Published
    Procedure TestEmpty;
    Procedure TestRenderText;
    Procedure TestRenderedTextChange;
    Procedure TestRenderHTML;
    Procedure TestRenderedHTMLChange;
    procedure TestTextModeChangeRenders;
    procedure TestEnvelopeChangeRenders;
  end;

  TMyTextLinesWidget = Class(TTextLinesWidget)
  Public
    Property Element;
    Property ParentElement;
  end;

  { TTestTextLinesWidget }

  TTestTextLinesWidget = Class(TBaseTestWidget)
  private
    FMy: TMyTextLinesWidget;
  Protected
    Procedure Setup; override;
    Procedure TearDown; override;
    Property My : TMyTextLinesWidget Read FMy;
  Published
    Procedure TestEmpty;
    Procedure TestRenderText;
    Procedure TestRenderedTextChange;
    Procedure TestRenderTextLineBreaks;
    Procedure TestRenderHTML;
    Procedure TestRenderHTMLLineBreaks;
    Procedure TestRenderedHTMLChange;
    procedure TestTextModeChangeRenders;
    procedure TestEnvelopeChangeRenders;
  end;

  { TMyTableWidget }

  TMyTableWidget = Class(TCustomTableWidget)
  private
    FRowCount: Integer;
  Protected
    Type
      TMyTableRowCountEnumerator = Class(TTableRowCountEnumerator)
        procedure GetCellData(aData: TTableWidgetCellData); override;
      end;
  Public
    Constructor create(aOwner : TComponent); override;
    Function GetBodyRowEnumerator : TTableRowEnumerator; override;
    Function GetRowEnumerator(aKind: TRowKind): TTableRowEnumerator; override;
    Property RowCount : Integer Read FRowCount Write FRowCount;
    Property CustomColumns;
    Property Caption;
    Property TableOptions;
    Property OnGetCellData;
    Property OnCellClick;
    Property OnHeaderCellClick;
    Property OnFooterCellClick;
    Property OnRowClick;
    Property OnHeaderRowClick;
    Property OnFooterRowClick;
  end;

  { TTestTableWidget }

  TTestTableWidget = Class(TBaseTestWidget)
  private
    FMy: TMyTableWidget;
    FClickCount : Integer;
    FClickEvent: TJSEvent;
    procedure AssertTableCaption(El: TJSHTMLElement);
    procedure CheckBodyCells(aParent: TJSHTMLELement);
    procedure CheckBodyRow(aParent: TJSHTMLELement; aIndex: Integer);
    procedure CheckCellData(el: TJSHTMLElement; aRow, aCol: Integer; rk: TRowKind; RowOption: TTableOption; ColOption: TTableOption);
    procedure CheckHeaderCells(aParent: TJSHTMLELement);
    procedure CheckFooterCells(aParent: TJSHTMLELement);
    procedure CheckRowData(aRow: TJSHTMLELement; aRowKind: TRowKind; aRowKindOption: TTableOption; aIndex: integer);
    procedure DoClickCount(Sender: TObject; Event: TJSEvent);
  Protected
    Procedure Setup; override;
    Procedure TearDown; override;
    Property My : TMyTableWidget Read FMy;
  Published
    Procedure TestEmpty;
    Procedure TestRender;
    Procedure TestRenderNoCaption;
    Procedure TestRenderNoCaptionNoHeaders;
    Procedure TestRenderFooters;
    Procedure TestRenderNoheaderFooterBody;
    Procedure TestRenderRowId;
    Procedure TestRenderCellID;
    Procedure TestRenderHeaderRowData;
    Procedure TestRenderHeaderCellDataRow;
    Procedure TestRenderHeaderCellDataCol;
    Procedure TestRenderRowData;
    Procedure TestRenderBodyCellDataRow;
    Procedure TestRenderBodyCellDataCol;
    Procedure TestRenderFooterRowData;
    Procedure TestRenderFooterCellDataRow;
    Procedure TestRenderFooterCellDataCol;
    Procedure TestClickHeaderCell;
    Procedure TestClickFooterCell;
    Procedure TestClickCell;
    Procedure TestClickRow;
    Procedure TestClickRowFromCell;
    Procedure TestClickHeaderRowFromHeaderCell;
    Procedure TestClickFooterRowFromFooterCell;
  end;

implementation

{ TTestTableWidget }

procedure TTestTableWidget.Setup;
begin
  inherited Setup;
  FMy:=TMyTableWidget.Create(Nil);
  FMy.ParentID:=SBaseWindowID;
end;

procedure TTestTableWidget.TearDown;
begin
  FreeAndNil(FMy);
  inherited TearDown;
end;

procedure TTestTableWidget.TestEmpty;
begin
  AssertNotNull('Have table',My);
  AssertEquals('Have parentid',SBaseWindowID,My.ParentID);
  AssertNotNull('Have table cols',My.CustomColumns);
  AssertEquals('Have table col count',2,My.CustomColumns.Count);
end;

procedure TTestTableWidget.CheckHeaderCells(aParent : TJSHTMLELement);

Var
  El : TJSHTMLElement;
  I : integer;
  Col : TCustomTableColumn;

begin

  AssertEquals('Header row count',1, aParent.childElementCount);
  aParent:=TJSHTMLElement(aParent.firstElementChild);
  AssertnotNull('Have row',aParent);
  AssertEquals('Have row tag','tr',LowerCase(aParent.tagname));
  CheckRowData(aParent,rkHeader,toHeaderRowData,0);
  AssertEquals('Header cell count',My.CustomColumns.Count, aParent.childElementCount);
  I:=0;
  el:=TJSHTMLElement(aParent.firstElementChild);
  While el<>Nil do
    begin
    AssertTrue('have col avail',I<My.CustomColumns.Count);
    Col:=My.CustomColumns[i];
    AssertNotNull('have col instance',Col);
    AssertEquals('Have head element','th',LowerCase(el.tagName));
    AssertEquals('Have head content col caption',Col.Caption,el.innerText);
    CheckCellData(el,0,i,rkHeader,toHeaderCellDataRow,toHeaderCellDataCol);
    El:=TJSHTMLElement(El.nextElementSibling);
    Inc(i);
    end;
end;

procedure TTestTableWidget.CheckRowData(aRow: TJSHTMLELement; aRowKind : TRowKind; aRowKindOption : TTableOption; aIndex : integer);

Var
  S : String;
begin
  S:=RowKindNames[aRowKind];
  if (toRowID in My.TableOptions)  then
    AssertEquals(S+' row ID',My.ElementID+'-'+S+'-'+IntToStr(aIndex),String(aRow.ID))
  else
    AssertEquals(S+' Row ID empty','',aRow.ID);
  if (aRowKindOption in My.TableOptions) then
    begin
    AssertEquals(S+' row data',IntToStr(aIndex),String(aRow.Dataset['row']));
    AssertEquals(S+' row kind data',S,String(aRow.Dataset['kind']));
    end
  else
    begin
    AssertTrue(S+' empty row data',isUndefined(aRow.Dataset['kind']));
    AssertTrue(S+' empty row data',isUndefined(aRow.Dataset['row']));
    end;
end;

procedure TTestTableWidget.CheckFooterCells(aParent: TJSHTMLELement);
Var
  El : TJSHTMLElement;
  I : integer;
  Col : TCustomTableColumn;

begin
  AssertEquals('Footer row count',1, aParent.childElementCount);
  aParent:=TJSHTMLElement(aParent.firstElementChild);
  AssertnotNull('Have row',aParent);
  AssertEquals('Have row tag','tr',LowerCase(aParent.tagname));
  CheckRowData(aParent,rkFooter,tofooterRowData,0);
  AssertEquals('Footer cell count',My.CustomColumns.Count, aParent.childElementCount);
  I:=0;
  el:=TJSHTMLElement(aParent.firstElementChild);
  While el<>Nil do
    begin
    AssertTrue('have col avail',I<My.CustomColumns.Count);
    Col:=My.CustomColumns[i];
    AssertNotNull('have col instance',Col);
    AssertEquals('Have footer element','td',LowerCase(el.tagName));
    AssertEquals('Have footer content',Format('Footer[%d]',[I]),el.innerText);
    CheckCellData(el,0,i,rkFooter,toFooterCellDataRow,toFooterCellDataCol);
    El:=TJSHTMLElement(El.nextElementSibling);
    Inc(i);
    end;
end;

procedure TTestTableWidget.DoClickCount(Sender: TObject; Event: TJSEvent);
begin
  Inc(FClickCount);
  AssertSame('Table',My,Sender);
  FClickEvent:=Event;
end;

procedure TTestTableWidget.CheckCellData(el : TJSHTMLElement; aRow,aCol : Integer; rk : TRowKind; RowOption : TTableOption; ColOption : TTableOption) ;

Var
  S : String;

begin
  S:=RowKindNames[RK];
  if toCellID in My.TableOptions then
    AssertEquals('Cell ID',My.ElementID+'-'+S+'-'+IntToStr(aRow)+'-'+IntToStr(aCol),el.ID)
  else
    AssertEquals('Cell ID','',el.ID);
  if ([rowoption,coloption] * My.TableOptions) <> [] then
    AssertEquals(S+'row kind data',S,String(el.Dataset['kind']))
  else
    AssertTrue(S+'cell empty row data',isUndefined(el.Dataset['kind']));
  if (rowOption in My.TableOptions) then
    AssertEquals(S+'cell row data',IntToStr(aRow),String(el.Dataset['row']))
  else
    AssertTrue(S+' cell empty row data',isUndefined(el.Dataset['row']));
  if (ColOption in My.TableOptions) then
    AssertEquals(S+' cell col data',IntToStr(aCol),String(el.Dataset['col']))
  else
    AssertTrue(S+' cell empty col data',isUndefined(el.Dataset['col']));
end;

procedure TTestTableWidget.CheckBodyRow(aParent : TJSHTMLELement; aIndex : Integer);

Var
  El : TJSHTMLElement;
  I : integer;
  Col : TCustomTableColumn;
begin
  CheckRowData(aParent,rkBody,toBodyRowData,aIndex);
  AssertEquals('row cell count',My.CustomColumns.Count, aParent.childElementCount);
  I:=0;
  el:=TJSHTMLElement(aParent.firstElementChild);
  While el<>Nil do
    begin
    AssertTrue('have col avail',I<My.CustomColumns.Count);
    Col:=My.CustomColumns[i];
    AssertNotNull('have col instance',Col);
    AssertEquals('Have cell element','td',LowerCase(el.tagName));
    AssertEquals('Have cell content ',Format('cell[%d,%d]',[I,aIndex]),el.innerText);
    CheckCellData(el,aIndex,i,rkBody,toBodyCellDataRow,toBodyCellDataCol);
    El:=TJSHTMLElement(El.nextElementSibling);
    Inc(i);
    end;
end;
procedure TTestTableWidget.CheckBodyCells(aParent : TJSHTMLELement);

Var
  aRow : integer;

begin
  AssertEquals('Body row count',My.RowCount, aParent.childElementCount);
  aParent:=TJSHTMLElement(aParent.firstElementChild);
  aRow:=0;
  While aParent<>nil do
    begin
    AssertNotNull('Have row',aParent);
    AssertEquals('Have row tag','tr',LowerCase(aParent.tagname));
    CheckBodyRow(aParent,aRow);
    aParent:=TJSHTMLElement(aParent.nextElementSibling);
    inc(aRow);
    end;
end;

procedure TTestTableWidget.AssertTableCaption(El : TJSHTMLElement);

begin
  AssertTrue('Caption element',SameText('caption',el.tagName));
  AssertEquals('Caption',My.Caption,El.InnerHTML)
end;

procedure TTestTableWidget.TestRender;

Var
  El : TJSHTMLElement;

begin
  My.Refresh;
  AssertNotNull('Have element',My.Element);
  AssertEquals('Have element','table',Lowercase(My.Element.tagName));
  AssertEquals('Sub elements',3,My.Element.childElementCount);
  El:=TJSHTMLElement(My.Element.firstElementChild);
  AssertTableCaption(El);
  El:=TJSHTMLElement(El.nextElementSibling);
  AssertEquals('Have head element','thead',LowerCase(el.tagName));
  CheckHeaderCells(el);
  El:=TJSHTMLElement(El.nextElementSibling);
  AssertEquals('Have body element','tbody',LowerCase(el.tagName));
  CheckBodyCells(el);
end;

procedure TTestTableWidget.TestRenderNoCaption;

Var
  El : TJSHTMLElement;

begin
  My.Caption:='';
  My.Refresh;
  AssertNotNull('Have element',My.Element);
  AssertEquals('Have element','table',Lowercase(My.Element.tagName));
  AssertEquals('Sub elements',2,My.Element.childElementCount);
  El:=TJSHTMLElement(My.Element.firstElementChild);
  AssertEquals('Have head element','thead',LowerCase(el.tagName));
  CheckHeaderCells(el);
  El:=TJSHTMLElement(El.nextElementSibling);
  AssertEquals('Have body element','tbody',LowerCase(el.tagName));
  CheckBodyCells(el);
end;

procedure TTestTableWidget.TestRenderNoCaptionNoHeaders;

Var
  El : TJSHTMLElement;

begin
  My.Caption:='';
  My.TableOptions:=My.TableOptions-[toHeaderRow];
  My.Refresh;
  AssertNotNull('Have element',My.Element);
  AssertEquals('Have element','table',Lowercase(My.Element.tagName));
  AssertEquals('Sub elements',1,My.Element.childElementCount);
  El:=TJSHTMLElement(My.Element.firstElementChild);
  AssertEquals('Have body element','tbody',LowerCase(el.tagName));
  CheckBodyCells(el);
end;

procedure TTestTableWidget.TestRenderFooters;
Var
  El : TJSHTMLElement;

begin
  My.TableOptions:=My.TableOptions+[toFooterRow];
  My.Refresh;
  AssertNotNull('Have element',My.Element);
  AssertEquals('Have element','table',Lowercase(My.Element.tagName));
  AssertEquals('Sub elements',4,My.Element.childElementCount);
  El:=TJSHTMLElement(My.Element.firstElementChild);
  AssertTableCaption(El);
  El:=TJSHTMLElement(El.nextElementSibling);
  AssertEquals('Have head element','thead',LowerCase(el.tagName));
  CheckHeaderCells(el);
  El:=TJSHTMLElement(El.nextElementSibling);
  AssertEquals('Have body element','tbody',LowerCase(el.tagName));
  CheckBodyCells(el);
  El:=TJSHTMLElement(El.nextElementSibling);
  AssertEquals('Have footer element','tfoot',LowerCase(el.tagName));
  CheckFooterCells(el);
end;

procedure TTestTableWidget.TestRenderNoheaderFooterBody;

Var
  El : TJSHTMLElement;

begin
  My.TableOptions:=My.TableOptions-[toFooter,toBody,toHeader]+[toFooterRow];
  My.Refresh;
  AssertNotNull('Have element',My.Element);
  AssertEquals('Have element','table',Lowercase(My.Element.tagName));
  AssertEquals('Sub elements',5,My.Element.childElementCount);
  El:=TJSHTMLElement(My.Element.firstElementChild);
  AssertTableCaption(El);
  El:=TJSHTMLElement(El.nextElementSibling);
  AssertEquals('Have head element','tr',LowerCase(el.tagName));
  El:=TJSHTMLElement(El.nextElementSibling);
  AssertEquals('Have body element 1','tr',LowerCase(el.tagName));
  CheckBodyRow(El,0);
  El:=TJSHTMLElement(El.nextElementSibling);
  AssertEquals('Have body element 2','tr',LowerCase(el.tagName));
  CheckBodyRow(El,1);
  El:=TJSHTMLElement(El.nextElementSibling);
  AssertEquals('Have footer element','tr',LowerCase(el.tagName));
end;

procedure TTestTableWidget.TestRenderRowId;
begin
  My.TableOptions:=My.TableOptions+[toRowID];
  TestRender;// Check functions will do additional check.
end;

procedure TTestTableWidget.TestRenderCellID;
begin
  My.TableOptions:=My.TableOptions+[toCellID];
  TestRender;// Check functions will do additional check.
end;

procedure TTestTableWidget.TestRenderHeaderRowData;
begin
  My.TableOptions:=My.TableOptions+[toHeaderRowData];
  TestRender;// Check functions will do additional check.
end;

procedure TTestTableWidget.TestRenderHeaderCellDataRow;
begin
  My.TableOptions:=My.TableOptions+[toHeaderCellDataRow];
  TestRender;// Check functions will do additional check.
end;

procedure TTestTableWidget.TestRenderHeaderCellDataCol;
begin
  My.TableOptions:=My.TableOptions+[toHeaderCellDataCol];
  TestRender;// Check functions will do additional check.
end;

procedure TTestTableWidget.TestRenderRowData;
begin
  My.TableOptions:=My.TableOptions+[toBodyRowData];
  TestRender;// Check functions will do additional check.
end;

procedure TTestTableWidget.TestRenderBodyCellDataRow;
begin
  My.TableOptions:=My.TableOptions+[toBodyCellDataRow];
  TestRender;// Check functions will do additional check.
end;

procedure TTestTableWidget.TestRenderBodyCellDataCol;
begin
  My.TableOptions:=My.TableOptions+[toBodyCellDataCol];
  TestRender;// Check functions will do additional check.
end;

procedure TTestTableWidget.TestRenderFooterRowData;
begin
  My.TableOptions:=My.TableOptions+[tofooterRowData];
  TestRender;// Check functions will do additional check.
end;

procedure TTestTableWidget.TestRenderFooterCellDataRow;
begin
  My.TableOptions:=My.TableOptions+[tofooterCellDataRow];
  TestRender;// Check functions will do additional check.
end;

procedure TTestTableWidget.TestRenderFooterCellDataCol;
begin
  My.TableOptions:=My.TableOptions+[tofooterCellDataCol];
  TestRender;// Check functions will do additional check.
end;

procedure TTestTableWidget.TestClickHeaderCell;

Var
  ev : TJSEvent;
  el : TJSHTMLElement;
begin
  My.Caption:='';
  My.OnHeaderCellClick:=@DoClickCount;
  My.Refresh;
  AssertNotNull('Have element',My.Element);
  ev:=TJSEvent.New('click');
  el:=TJSHTMLElement(My.Element.firstElementChild.firstElementChild.firstElementChild);
  AssertEquals('TH el','th',LowerCase(el.TagName));
  el.dispatchEvent(ev);
  AssertEquals('One click',1,FClickCount);
  AssertSame('Event',ev,FClickEvent);
end;

procedure TTestTableWidget.TestClickFooterCell;

Var
  ev : TJSEvent;
  el : TJSHTMLElement;
begin
  My.Caption:='';
  My.TableOptions:=My.TableOptions-[toHeaderRow]+[toFooterRow];
  My.OnFooterCellClick:=@DoClickCount;
  My.Refresh;
  AssertNotNull('Have element',My.Element);
  ev:=TJSEvent.New('click');
  el:=TJSHTMLElement(My.Element.firstElementChild.nextElementSibling.firstElementChild.firstElementChild);
  AssertEquals('TD el','td',LowerCase(el.TagName));
  el.dispatchEvent(ev);
  AssertEquals('One click',1,FClickCount);
  AssertSame('Event',ev,FClickEvent);
end;

procedure TTestTableWidget.TestClickCell;

Var
  ev : TJSEvent;
  el : TJSHTMLElement;
begin
  My.Caption:='';
  My.TableOptions:=My.TableOptions-[toHeaderRow];
  My.OnCellClick:=@DoClickCount;
  My.Refresh;
  AssertNotNull('Have element',My.Element);
  ev:=TJSEvent.New('click');
  el:=TJSHTMLElement(My.Element.firstElementChild.firstElementChild.firstElementChild);
  AssertEquals('TD el','td',LowerCase(el.TagName));
  el.dispatchEvent(ev);
  AssertEquals('One click',1,FClickCount);
  AssertSame('Event',ev,FClickEvent);
end;

procedure TTestTableWidget.TestClickRow;
Var
  ev : TJSEvent;
  el : TJSHTMLElement;
begin
  My.Caption:='';
  My.TableOptions:=My.TableOptions-[toHeaderRow];
  My.OnRowClick:=@DoClickCount;
  My.Refresh;
  AssertNotNull('Have element',My.Element);
  ev:=TJSEvent.New('click');
  el:=TJSHTMLElement(My.Element.firstElementChild.firstElementChild);
  AssertEquals('TD el','tr',LowerCase(el.TagName));
  el.dispatchEvent(ev);
  AssertEquals('One click',1,FClickCount);
  AssertSame('Event',ev,FClickEvent);
end;

procedure TTestTableWidget.TestClickRowFromCell;
Var
  ev : TJSEvent;
  el : TJSHTMLElement;
begin
  My.Caption:='';
  My.TableOptions:=My.TableOptions-[toHeaderRow];
  My.OnRowClick:=@DoClickCount;
  My.Refresh;
  AssertNotNull('Have element',My.Element);
  ev:=TJSEvent.New('click');
  el:=TJSHTMLElement(My.Element.firstElementChild.firstElementChild.FirstElementChild);
  AssertEquals('TD el','td',LowerCase(el.TagName));
  el.dispatchEvent(ev);
  AssertEquals('One click',1,FClickCount);
  AssertSame('Event',ev,FClickEvent);
end;

procedure TTestTableWidget.TestClickHeaderRowFromHeaderCell;
Var
  ev : TJSEvent;
  el : TJSHTMLElement;
begin
  My.Caption:='';
//  My.TableOptions:=My.TableOptions;
  My.OnHeaderRowClick:=@DoClickCount;
  My.Refresh;
  AssertNotNull('Have element',My.Element);
  ev:=TJSEvent.New('click');
  el:=TJSHTMLElement(My.Element.firstElementChild.firstElementChild.FirstElementChild);
  AssertEquals('TD el','th',LowerCase(el.TagName));
  el.dispatchEvent(ev);
  AssertEquals('One click',1,FClickCount);
  AssertSame('Event',ev,FClickEvent);
end;

procedure TTestTableWidget.TestClickFooterRowFromFooterCell;
Var
  ev : TJSEvent;
  el : TJSHTMLElement;
begin
  My.Caption:='';
  My.TableOptions:=My.TableOptions-[toHeaderRow]+[toFooterRow];
  My.OnFooterRowClick:=@DoClickCount;
  My.Refresh;
  AssertNotNull('Have element',My.Element);
  ev:=TJSEvent.New('click');
  el:=TJSHTMLElement(My.Element.firstElementChild.nextElementSibling.firstElementChild.firstElementChild);
  AssertEquals('TD el','td',LowerCase(el.TagName));
  el.dispatchEvent(ev);
  AssertEquals('One click',1,FClickCount);
  AssertSame('Event',ev,FClickEvent);
end;

{ TMyTableWidget }

constructor TMyTableWidget.create(aOwner: TComponent);

begin
  inherited create(aOwner);
  CustomColumns.Add ('Col1');
  CustomColumns.Add ('Col2');
  Caption:='Our caption';
  RowCount:=2;
end;

function TMyTableWidget.GetBodyRowEnumerator: TTableRowEnumerator;
begin
  Result:=TMyTableRowCountEnumerator.Create(Self,RowCount);
end;

function TMyTableWidget.GetRowEnumerator(aKind: TRowKind): TTableRowEnumerator;
begin
  if AKind=rkFooter then
    Result:=TMyTableRowCountEnumerator.Create(Self,1)
  else
    Result:=Inherited GetRowEnumerator(aKind);
end;

procedure TMyTableWidget.TMyTableRowCountEnumerator.GetCellData(aData: TTableWidgetCellData);
begin
  inherited GetCellData(aData);
  Case aData.Kind of
    rkBody :
      aData.Text:=Format('cell[%d,%d]',[aData.Col,aData.Row]);
    rkFooter :
      begin
      aData.Text:=Format('Footer[%d]',[aData.Col]);
      end;
  end;
end;

{ TTestTextLinesWidget }

procedure TTestTextLinesWidget.Setup;
begin
  inherited Setup;
  FMy:=TMyTextLinesWidget.Create(Nil);
  FMy.ParentID:=SBaseWindowID;
  FMy.Lines.Add('0&lt;1');
  FMy.Lines.Add('two');
end;

procedure TTestTextLinesWidget.TearDown;
begin
  FreeAndNil(FMy);
  inherited TearDown;
end;

procedure TTestTextLinesWidget.TestEmpty;
begin
  AssertNotNull('Have widget',My);
  AssertNull('widget not rendered',My.Element);
  AssertTrue('Text mode default text',tmText=My.TextMode);
  AssertTrue('Envelope tag default paragraph',ttParagraph=My.EnvelopeTag);
end;

procedure TTestTextLinesWidget.TestRenderText;
begin
  My.Refresh;
  AssertNotNull('Have element',My.Element);
  AssertEquals('Have element','P',My.Element.tagName);
  AssertEquals('Have text','0&lt;1'+slineBreak+'two'+slineBreak,My.Element.InnerText);
end;

procedure TTestTextLinesWidget.TestRenderedTextChange;
begin
  My.Refresh;
  My.Lines[1]:='Three';
  AssertNotNull('Have element',My.Element);
  AssertEquals('Have element','P',My.Element.tagName);
  AssertEquals('Have text','0&lt;1'+slineBreak+'Three'+slineBreak,My.Element.InnerText);
end;

procedure TTestTextLinesWidget.TestRenderTextLineBreaks;
begin
  My.ForceLineBreaks:=True;
  My.Refresh;
  AssertNotNull('Have element',My.Element);
  AssertEquals('Have text','0&lt;1'+slineBreak+'two'+slineBreak,My.Element.InnerText);
  AssertEquals('Have HTML','0&amp;lt;1<br>two<br>',My.Element.InnerHtml);
end;

procedure TTestTextLinesWidget.TestRenderHTML;
begin
  My.TextMode:=tmHTML;
  My.Refresh;
  AssertNotNull('Have element',My.Element);
  AssertEquals('Have element','P',My.Element.tagName);
  AssertEquals('Have text','0<1 two',My.Element.InnerText);
  AssertEquals('Have HTML','0&lt;1'+sLineBreak+'two'+sLineBreak,My.Element.InnerHtml);
end;

procedure TTestTextLinesWidget.TestRenderHTMLLineBreaks;

begin
  My.TextMode:=tmHTML;
  My.ForceLineBreaks:=True;
  My.Refresh;
  AssertNotNull('Have element',My.Element);
  AssertEquals('Have element','P',My.Element.tagName);
  AssertEquals('Have text','0<1'+slineBreak+'two'+slineBreak,My.Element.InnerText);
  AssertEquals('Have HTML','0&lt;1<br>two<br>',My.Element.InnerHtml);
end;

procedure TTestTextLinesWidget.TestRenderedHTMLChange;
begin
  TestRenderHTML;
  My.Lines[1]:='three';
  AssertNotNull('Have element',My.Element);
  AssertEquals('Have element','P',My.Element.tagName);
  AssertEquals('Have text','0<1 three',My.Element.InnerText);
  AssertEquals('Have HTML','0&lt;1'+sLineBreak+'three'+sLineBreak,My.Element.InnerHtml);
end;

procedure TTestTextLinesWidget.TestTextModeChangeRenders;
begin
  TestRenderText;
  My.TextMode:=tmHTML;
  AssertNotNull('Have element',My.Element);
  AssertEquals('Have element','P',My.Element.tagName);
  AssertEquals('Have text','0<1 two',My.Element.InnerText);
  AssertEquals('Have HTML','0&lt;1'+sLineBreak+'two'+sLineBreak,My.Element.InnerHtml);
end;

procedure TTestTextLinesWidget.TestEnvelopeChangeRenders;
begin
  TestRenderText;
  My.EnvelopeTag:=ttSpan;
  AssertNotNull('Have element',My.Element);
  AssertEquals('Have element','SPAN',My.Element.tagName);
  AssertEquals('Have text','0&lt;1'+slineBreak+'two'+slineBreak,My.Element.InnerText);

end;

{ TTestTextWidget }

procedure TTestTextWidget.Setup;
begin
  inherited Setup;
  FMy:=TMyTextWidget.Create(Nil);
  FMy.ParentID:=SBaseWindowID;
  FMy.Text:='0&lt;1';
end;

procedure TTestTextWidget.TearDown;
begin
  FreeAndNil(FMy);
  inherited TearDown;
end;

procedure TTestTextWidget.TestEmpty;
begin
  AssertNotNull('Have widget',My);
  AssertNull('widget not rendered',My.Element);
  AssertTrue('Text mode default text',tmText=My.TextMode);
  AssertTrue('Envelope tag default paragraph',ttParagraph=My.EnvelopeTag);
end;

procedure TTestTextWidget.TestRenderText;
begin
  My.Refresh;
  AssertNotNull('Have element',My.Element);
  AssertEquals('Have element','P',My.Element.tagName);
  AssertEquals('Have text','0&lt;1',My.Element.InnerText);
end;

procedure TTestTextWidget.TestRenderedTextChange;
begin
  TestRenderText;
  My.Text:='Something else';
  AssertEquals('Have text','Something else',My.Element.InnerText);
end;

procedure TTestTextWidget.TestRenderHTML;
begin
  My.TextMode:=tmHTML;
  My.Refresh;
  AssertNotNull('Have element',My.Element);
  AssertEquals('Have element','P',My.Element.tagName);
  AssertEquals('Have text','0<1',My.Element.InnerText);
  AssertEquals('Have HTML','0&lt;1',My.Element.InnerHtml);
end;

procedure TTestTextWidget.TestRenderedHTMLChange;
begin
  TestRenderHtml;
  My.Text:='2&gt;1';
  AssertEquals('Have text','2>1',My.Element.InnerText);
  AssertEquals('Have HTML','2&gt;1',My.Element.InnerHtml);
end;

procedure TTestTextWidget.TestTextModeChangeRenders;
begin
  TestRenderText;
  My.TextMode:=tmHTML;
  AssertEquals('Have text','0<1',My.Element.InnerText);
  AssertEquals('Have HTML','0&lt;1',My.Element.InnerHtml);
end;

procedure TTestTextWidget.TestEnvelopeChangeRenders;
begin
  TestRenderText;
  My.EnvelopeTag:=ttSpan;
  AssertEquals('Have element','SPAN',My.Element.tagName);
  AssertEquals('Have text','0&lt;1',My.Element.InnerText);
  AssertEquals('Have HTML','0&amp;lt;1',My.Element.InnerHtml);
end;

{ TTestLabelWidget }

procedure TTestLabelWidget.SetUp;
begin
  inherited SetUp;
  FMy:=TMyLabelWidget.Create(Nil);
  My.Text:='Your name';
  My.ParentID:=SBaseWindowID;
  FEdit:=TTextInputWidget.Create(Nil);
  FEdit.ParentID:=SBaseWindowID;
  FMy.LabelFor:=Edit;
end;

procedure TTestLabelWidget.TearDown;
begin
  FreeAndNil(Fmy);
  FreeAndNil(FEdit);
  inherited TearDown;
end;

procedure TTestLabelWidget.TestPropsBeforeRender;
begin
  Edit.Refresh;
  My.Refresh;
  AssertEquals('text','Your name',My.LabelElement.innerText);
  AssertEquals('for',Edit.ElementID,My.LabelElement.For_);
end;

procedure TTestLabelWidget.TestPropsAfterRender;
begin
  My.LabelFor:=Nil;
  My.Refresh;
  AssertEquals('text','Your name',My.LabelElement.innerText);
  AssertEquals('for','',My.LabelElement.For_);
  // Will render Edit!
  My.LabelFor:=Edit;
  AssertTrue('Have edit id',Edit.ElementID<>'');
  My.Text:='My Name';
  My.Refresh;
  AssertEquals('text','My Name',My.LabelElement.innerText);
  AssertEquals('for',Edit.ElementID,My.LabelElement.For_);
end;


{ TTestSelectElement }

function TTestSelectElement.GetOptions: TJSHTMLOPtionElementArray;
begin
  Result:=My.Options;
end;

function TTestSelectElement.GetSelect: TJSHTMLSelectElement;
begin
  Result:=My.SelectElement;
end;

procedure TTestSelectElement.Setup;
begin
  inherited Setup;
  FMy:=TMySelectWidget.Create(Nil);
  FMy.ParentID:=SBaseWindowID;
  FMy.Items.Add('One');
  FMy.Items.Add('Two');
  FMy.Items.Add('Three');
  FMy.Values.Add('1');
  FMy.Values.Add('2');
  FMy.Values.Add('3');
  FMy.SelectedIndex:=0;
end;

procedure TTestSelectElement.TearDown;
begin
  FreeAndNil(FMy);
  inherited TearDown;
end;


procedure TTestSelectElement.TestEmpty;
begin
  AssertNotNull('Have widget',My);
  AssertNull('Not rendered',My.Element);
end;

procedure TTestSelectElement.AssertOption(Idx : Integer; aText,aValue : String; Selected : Boolean= False);

Var
  O : TJSHTMLOptionElement;

begin
  AssertTrue('Correct index',Idx<Select.childElementCount);
  O:=Select.children[Idx] as TJSHTMLOptionElement;
  AssertEquals('Text',aText,O.InnerText);
  if aValue='' then aValue:=aText;
  AssertEquals('Value',aValue,O.Value);
  AssertEquals('Selected',Selected,O.selected);
end;

procedure TTestSelectElement.TestPropsOnRender;


begin
  My.Refresh;
  AssertTree('select/option');
  AssertEquals('Multi',False,Select.multiple);
  AssertEquals('SelectedIndex',0,Select.selectedIndex);
  AssertEquals('Amount of options',3,Length(Options));
  AssertEquals('Amount of option values',3,Select.childElementCount);
  AssertOption(0,'One','1',True);
  AssertOption(1,'Two','2');
  AssertOption(2,'Three','3');
end;

procedure TTestSelectElement.TestPropsAfterRender;

Var
  L1,L2 : TStrings;

begin
  TestPropsOnRender;
  My.Multiple:=True;
  L1:=My.Items;
  l2:=My.Values;
  L1.BeginUpdate;
  L2.BeginUpdate;
  L1.Clear;
  L1.Add('Alpha');
  L1.Add('Beta');
  L1.Add('Gamma');
  L2.Clear;
  L2.Add('a');
  L2.Add('b');
  L1.EndUpdate;
  L2.EndUpdate;
  My.SelectedIndex:=2;
  AssertEquals('Multi',True,Select.multiple);
  AssertEquals('SelectedIndex',2,Select.selectedIndex);
  AssertEquals('Amount of options',3,Length(Options));
  AssertEquals('Amount of option values',3,Select.childElementCount);
  AssertOption(0,'Alpha','a');
  AssertOption(1,'Beta','b');
  AssertOption(2,'Gamma','Gamma',True);
end;

procedure TTestSelectElement.TestMultiSelect;

Var
  I : Integer;

begin
  TestPropsOnRender;
  My.Multiple:=True;
  For I:=0 to My.Items.Count-1 do
    begin
    AssertEquals(IntToStr(I)+' selected',I=My.SelectedIndex,My.Selected[I]);
    AssertEquals(IntToStr(I)+' option selected',I=My.SelectedIndex,Options[i].Selected);
    end;
  My.Selected[2]:=True;
  AssertEquals('First selected index',0,My.SelectedIndex);
  AssertEquals('Additional selected',True,Options[2].Selected);
  AssertEquals('Additional option selected',True,My.Selected[2]);
  AssertEquals('SelectionCount',2,My.selectionCount);
  AssertEquals('SelectionValue[0]','1',My.selectionValue[0]);
  AssertEquals('SelectionItem[0]','One',My.SelectionItem[0]);
  AssertEquals('SelectionValue[1]','3',My.selectionValue[1]);
  AssertEquals('SelectionItem[1]','Three',My.selectionItem[1]);
end;

procedure TTestSelectElement.TestNoSelectedIndex;
begin
  My.SelectedIndex:=-1;
  My.Refresh;
  AssertTree('select/option');
  AssertEquals('Multi',False,Select.multiple);
  AssertEquals('SelectedIndex',-1,Select.selectedIndex);
  AssertEquals('Amount of options',3,Length(Options));
  AssertEquals('Amount of option values',3,Select.childElementCount);
  AssertOption(0,'One','1');
  AssertOption(1,'Two','2');
  AssertOption(2,'Three','3');
end;


{ TTestImageElement }

function TTestImageElement.GetImg: TJSHTMLImageElement;
begin
  Result:=TJSHTMLImageElement(My.Element);
end;

procedure TTestImageElement.Setup;
begin
  inherited Setup;
  FMy:=TMyImageWidget.Create(Nil);
  FMy.ParentID:=SBaseWindowID;
  FMy.Src:='img.png';
  FMy.Width:=64;
  FMy.Height:=128;
end;

procedure TTestImageElement.TearDown;
begin
  FreeAndNil(FMy);
  inherited TearDown;
end;

function TTestImageElement.ThisURL: String;
begin
  Result:=ExtractFilePath(Window.Location.href);
end;

procedure TTestImageElement.TestEmpty;
begin
  AssertNotNull('have image',My);
  AssertNull('Not rendered',My.Element);
end;

procedure TTestImageElement.TestPropsOnRender;
begin
  My.Refresh;
  AssertNotNull('have element',My.Element);
  AssertEquals('URL',ThisURL+'img.png',Image.src);
  AssertEquals('Width',64,Image.width);
  AssertEquals('Height',128,Image.Height);
end;

procedure TTestImageElement.TestPropsAfterRender;
begin
  My.Refresh;
  My.Src:='img2.png';
  My.Width:=88;
  My.Height:=166;
  AssertEquals('URL',ThisURL+'img2.png',Image.src);
  AssertEquals('Width',88,Image.width);
  AssertEquals('Height',166,Image.Height);
end;

{ TTestHiddenInputElement }

function TTestHiddenInputElement.CreateInput: TCustomInputWidget;
begin
  Result:=THiddenInputWidget.Create(Nil);
end;

function TTestHiddenInputElement.MyHidden: TMyHiddenInputWidget;
begin
  Result:=My as TMyHiddenInputWidget;
end;


procedure TTestHiddenInputElement.TestPropsOnRender;
begin
  My.Refresh;
  AssertBaseProps('','','');
end;

procedure TTestHiddenInputElement.TestPropsAfterRender;
begin
  My.Refresh;
  My.ValueName:='a';
  My.Value:='b';
  AssertBaseProps('hidden','a','b');
end;

{ TTestDateInputElement }

function TTestDateInputElement.CreateInput: TCustomInputWidget;
begin
  Result:=TMyDateInputWidget.Create(Nil);
end;

procedure TTestDateInputElement.CreateMy;
begin
  inherited CreateMy;
  MyDate.Date:=Date;
end;

function TTestDateInputElement.MyDate: TMyDateInputWidget;
begin
  Result:=My as TMyDateInputWidget;
end;

procedure TTestDateInputElement.TestPropsOnRender;
begin
  My.Refresh;
  AssertBaseProps('','',FormatDateTime('yyyy-mm-dd',Date));
end;

procedure TTestDateInputElement.TestPropsAfterRender;
begin
  My.Refresh;
  MyDate.Date:=Date-1;
  AssertBaseProps('','',FormatDateTime('yyyy-mm-dd',Date-1));
end;

{ TTestFileInputElement }

function TTestFileInputElement.CreateInput: TCustomInputWidget;
begin
  Result:=TMyFileInputWidget.Create(Nil);
end;

procedure TTestFileInputElement.CreateMy;
begin
  inherited CreateMy;
  My.Value:='';
end;

function TTestFileInputElement.MyFile: TMyFileInputWidget;
begin
  Result:=My as TMyFileInputWidget;
end;

procedure TTestFileInputElement.TestPropsOnRender;
begin
  My.Refresh;
  // We cannot use assertbaseprops
  AssertTree('input('+My.ElementID+')');
  AssertEquals('Type','file',InputElement._Type);
  AssertEquals('Value name','Test',InputElement.name);
  AssertEquals('Value','',InputElement.value);
  AssertEquals('Text (inner text)','',InputElement.innerText);
end;

procedure TTestFileInputElement.TestPropsAfterRender;
begin
  My.Refresh;
  // We cannot use assertbaseprops
  AssertTree('input('+My.ElementID+')');
  AssertEquals('Type','file',InputElement._Type);
  AssertEquals('Value name','Test',InputElement.name);
  AssertEquals('Value','',InputElement.value);
  AssertEquals('Text (inner text)','',InputElement.innerText);
end;


{ TTestRadioInputElement }

function TTestRadioInputElement.CreateInput: TCustomInputWidget;
begin
  Result:=TRadioInputWidget.Create(Nil);
end;

function TTestRadioInputElement.MyRadio: TRadioInputWidget;
begin
  Result:=My as TRadioInputWidget;
end;

procedure TTestRadioInputElement.TestPropsOnRender;
begin
  MyRadio.Checked:=true;
  My.Refresh;
  AssertBaseProps('','','');
  AssertEquals('Checked',true,InputElement.Checked);
end;

procedure TTestRadioInputElement.TestPropsAfterRender;
begin
  My.Refresh;
  AssertEquals('Checked before',False,InputElement.Checked);
  MyRadio.Checked:=true;
  AssertBaseProps('','','');
  AssertEquals('Checked after',true,InputElement.Checked);
end;

{ TTestCheckBoxInputElement }

function TTestCheckBoxInputElement.CreateInput: TCustomInputWidget;
begin
  Result:=TCheckBoxInputWidget.Create(Nil);
end;

function TTestCheckBoxInputElement.MyCheckBox: TCheckBoxInputWidget;
begin
  Result:=My as TCheckBoxInputWidget;
end;

procedure TTestCheckBoxInputElement.TestPropsOnRender;
begin
  MyCheckBox.Checked:=true;
  My.Refresh;
  AssertBaseProps('','','');
  AssertEquals('Checked',true,InputElement.Checked);
end;

procedure TTestCheckBoxInputElement.TestPropsAfterRender;
begin
  My.Refresh;
  AssertEquals('Checked before',False,InputElement.Checked);
  MyCheckBox.Checked:=true;
  AssertBaseProps('','','');
  AssertEquals('Checked after',true,InputElement.Checked);
end;

{ TTestTextAreaElement }

function TTestTextAreaElement.GetArea: TJSHTMLTextAreaElement;
begin
  Result:=FMy.TextArea
end;

procedure TTestTextAreaElement.Setup;
begin
  inherited Setup;
  FMy:=TMyTextAreaWidget.Create(Nil);
  FMy.Lines.Add('a');
  FMy.Lines.Add('b');
end;

procedure TTestTextAreaElement.TearDown;
begin
  FreeAndNil(FMy);
  inherited TearDown;
end;

procedure TTestTextAreaElement.TestEmpty;
begin
  AssertNotNull(My);
end;

procedure TTestTextAreaElement.TestPropsOnRender;
begin
  My.ParentID:=BaseID;
  My.ValueName:='test';
  My.Columns:=25;
  My.Rows:=35;
  My.MaxLength:=500;
  My.Wrap:=tawHard;
  My.Required:=True;
  My.ReadOnly:=True;
  My.Refresh;
  AssertEquals('ValueName','test',area.Name);
  AssertEquals('Wrap','hard',area.Wrap);
  AssertEquals('Rows',35,area.Rows);
  AssertEquals('Cols',25,area.Cols);
  AssertEquals('MaxLength',500,area.MaxLength);
  AssertEquals('Text','a'+sLineBreak+'b'+sLineBreak,area.innerHtml);
  AssertEquals('Required',true,Area.Required);
  AssertEquals('ReadOnly',true,Area.ReadOnly);
end;

procedure TTestTextAreaElement.TestPropsAfterRender;

begin
  My.ParentID:=BaseID;
  My.Refresh;
  My.ValueName:='test';
  My.Columns:=25;
  My.Rows:=35;
  My.MaxLength:=500;
  My.Required:=True;
  My.ReadOnly:=True;
  My.Wrap:=tawHard;
  With My.Lines do
     begin
     BeginUpdate;
     Clear;
     Add('d');
     Add('e');
     EndUpdate;
     end;
  AssertEquals('ValueName','test',area.Name);
  AssertEquals('Wrap','hard',area.Wrap);
  AssertEquals('Rows',35,area.Rows);
  AssertEquals('Cols',25,area.Cols);
  AssertEquals('MaxLength',500,area.MaxLength);
  AssertEquals('Text','d'+sLineBreak+'e'+sLineBreak,area.innerHTML);
  AssertEquals('Required',true,Area.Required);
  AssertEquals('ReadOnly',true,Area.ReadOnly);
end;

{ TTestTextInputElement }

procedure TTestTextInputElement.setup;
begin
  inherited setup;
  FITT:=ittText;
end;

function TTestTextInputElement.CreateInput: TCustomInputWidget;
begin
  Result:=TTextInputWidget.Create(Nil);
  TTextInputWidget(Result).TextType:=FITT;
end;

function TTestTextInputElement.MyText: TTextInputWidget;
begin
  Result:=My as TTextInputWidget;
end;

procedure TTestTextInputElement.TestDefaultTextType;
begin
  AssertTrue('Correct type',ittText=MyText.TextType);
end;

procedure TTestTextInputElement.TestRender;
begin
  My.Refresh;
  AssertBaseProps('','','','');
end;

procedure TTestTextInputElement.TestChangeValue;
begin
  My.Refresh;
  AssertBaseProps('','','','');
  My.Value:='soso';
  AssertEquals('Value propagates','soso',InputElement.value);
end;

procedure TTestTextInputElement.TestChangeName;
begin
  My.Refresh;
  AssertBaseProps('','','','');
  My.ValueName:='soso';
  AssertEquals('ValueName propagates','soso',InputElement.name);
end;

procedure TTestTextInputElement.TestChangeTextType;
begin
  My.Refresh;
  AssertBaseProps('','','','');
  MyText.TextType:=ittPassword;
  AssertEquals('TextType propagates to type','password',InputElement._type);
end;

procedure TTestTextInputElement.TestTypePassword;
begin
  FItt:=ittPassword;
  CreateMy;
  My.Refresh;
  AssertBaseProps('password','','','');
end;

procedure TTestTextInputElement.TestTypeNumber;
begin
  FItt:=ittNumber;
  CreateMy;
  My.Refresh;
  AssertBaseProps('number','','','');
end;

procedure TTestTextInputElement.TestAsNumber;
begin
  TestTypeNumber;
  AssertBaseProps('number','','','');
  AssertEquals('Correct read',1,MyText.AsNumber);
  MyText.AsNumber:=123;
  AssertEquals('Correctly set','123',InputElement.Value);
  AssertEquals('Correctly set 2','123',Mytext.Value);
end;

procedure TTestTextInputElement.TestTypeEmail;
begin
  FItt:=ittEmail;
  CreateMy;
  My.Refresh;
  AssertBaseProps('email','','','');
end;

procedure TTestTextInputElement.TestTypeSearch;
begin
  FItt:=ittSearch;
  CreateMy;
  My.Refresh;
  AssertBaseProps('search','','','');
end;

procedure TTestTextInputElement.TestTypeTel;
begin
  FItt:=ittTelephone;
  CreateMy;
  My.Refresh;
  AssertBaseProps('tel','','','');
end;

procedure TTestTextInputElement.TestTypeURL;
begin
  FItt:=ittURL;
  CreateMy;
  My.Refresh;
  AssertBaseProps('url','','','');
end;

procedure TTestTextInputElement.TestTypeColor;
begin
  FItt:=ittColor;
  CreateMy;
  My.Refresh;
  AssertBaseProps('color','','#000000','');
end;

{ TBaseTestInputElement }

function TBaseTestInputElement.GetInputElement: TJSHTMLInputElement;
begin
  Result:= TInputHack(My).InputElement;
  AssertNotNull('Have input element',Result);
end;

procedure TBaseTestInputElement.CreateMy;
begin
  FreeAndNil(FMy);
  FMy:=CreateInput;
  FMy.ParentID:=BaseID;
  FMy.ValueName:='Test';
  FMy.Value:='1';
end;

procedure TBaseTestInputElement.Setup;
begin
  inherited Setup;
  CreateMy;
end;

procedure TBaseTestInputElement.TearDown;
begin
  FreeAndNil(FMy);
  inherited TearDown;
end;


procedure TBaseTestInputElement.AssertBaseProps(aType, aValueName, aValue : String; aText : String = '');
Var
  El : TJSHTMLInputElement;

begin
  if AType='' then
    aType:=My.InputType;
  if aValueName='' then
    aValueName:='Test'; // Same as in CreateMy
  if aValue='' then
    aValue:='1'; // Same as in CreateMy
  el:=InputElement;
  AssertTree('input('+el.ID+')');
  AssertEquals('Type',aType,el._Type);
  AssertEquals('Value name',aValueName,el.name);
  AssertEquals('Value',aValue,el.value);
  AssertEquals('Text (inner text)',aText,el.innerText);
end;

procedure TBaseTestInputElement.TestEmpty;
begin
  AssertNotNull('Have element',My);
end;

procedure TBaseTestInputElement.TestRequiredOnRender;
begin
  My.Required:=True;
  My.Refresh;
  AssertEquals('required',True,InputElement.required);
end;

procedure TBaseTestInputElement.TestReadOnlyOnRender;
begin
  My.ReadOnly:=True;
  My.Refresh;
  AssertEquals('ReadOnly',True,InputElement.ReadOnly);
end;

procedure TBaseTestInputElement.TestRequiredAfterRender;
begin
  My.Refresh;
  My.Required:=True;
  AssertEquals('required',True,InputElement.required);
end;

procedure TBaseTestInputElement.TestReadOnlyAfterRender;
begin
  My.Refresh;
  My.ReadOnly:=True;
  AssertEquals('ReadOnly',True,InputElement.ReadOnly);
end;

{ TMyWebPage }

procedure TMyWebPage.SetParentId;
begin
  ParentID:='A';
end;

procedure TMyWebPage.SetParent;
begin
  Parent:=TViewPort.Create(Nil);
end;

procedure TMyWebPage.SetElementID;
begin
  ElementID:=BaseID;
end;

{ TTestPage }

function TTestPage.CreateElement(aID: String): TJSHTMLElement;
begin
  Result:=TJSHTMLElement(Document.CreateElement('div'));
  Result.ID:=aID;
  BaseWindow.AppendChild(Result);
end;

procedure TTestPage.Setup;
begin
  inherited Setup;
  FMy:=TMyWebPage.Create(Nil);
end;

procedure TTestPage.TearDown;
begin
  FreeAndNil(FMy);
  inherited TearDown;
end;

procedure TTestPage.TestEmpty;
begin
  AssertNotNull('Have element');
end;

procedure TTestPage.TestAsWindow;
begin
  // Set element to base-window
  My.SetElementID;
  AssertSame('Correct',BaseWindow,My.Element);
end;

procedure TTestPage.TestNoParentOK;
begin
  My.Refresh;
  AssertSame('Correct parent',ViewPort.Element,My.ParentElement);
end;

procedure TTestPage.TestDefaultTag;
begin
  AssertEquals('Correct tag','div',My.HTMLTag);
end;

{ TMyViewPort }

procedure TMyViewPort.SetParentId;
begin
  ParentID:='SomeThing';
end;

procedure TMyViewPort.SetParent;
begin
  Parent:=Instance
end;

procedure TMyViewPort.SetElementID;
begin
  ElementID:='Something';
end;

{ TTestViewPort }

procedure TTestViewPort.Setup;
begin
  inherited Setup;
  FMy:=TMyViewPort.Create(Nil);
end;

procedure TTestViewPort.TearDown;
begin
  FreeAndNil(FMy);
  inherited TearDown;
end;

procedure TTestViewPort.TestInstance;

begin
  AssertNotNull('Have viewport',ViewPort);
  AssertSame('Have viewport',TViewPort.Instance,ViewPort);
end;

procedure TTestViewPort.TestHTMLTag;
begin
  AssertEquals('Correct tag','body',ViewPort.HTMLTag);
end;

procedure TTestViewPort.TestElement;
begin
  AssertSame('Correct Element',Document.Body,ViewPort.Element);
end;

procedure TTestViewPort.TestUnrender;
begin
  AssertSame('Element retained',Document.Body,ViewPort.Element);
end;

procedure TTestViewPort.TestNoParent;
begin
  AssertException('No parent can be set',EWidgets,@My.SetParent);
end;

procedure TTestViewPort.TestNoElementID;
begin
  AssertException('No elementID can be set',EWidgets,@My.SetElementID);
end;

procedure TTestViewPort.TestNoParentID;
begin
  AssertException('No ParentID can be set',EWidgets,@My.SetParentID);
end;

{ TTestButtonWidget }

procedure TTestButtonWidget.SetUp;
begin
  inherited SetUp;
  FButton:=TButtonWidget.Create(Nil);
end;

procedure TTestButtonWidget.TearDown;
begin
  FreeAndNil(FButton);
  inherited TearDown;
end;

procedure TTestButtonWidget.TestTextBeforeRender;

Var
  El : TJSHTMLElement;

begin
  Button.ParentID:=BaseID;
  Button.Text:='Click me';
  Button.Refresh;
  El:=AssertTree('button('+Button.ElementID+')');
  AssertEquals('Text set','Click me',el.innerText);
end;

procedure TTestButtonWidget.TestTextAfterRender;
Var
  El : TJSHTMLElement;

begin
  Button.ParentID:=BaseID;
  Button.Refresh;
  El:=AssertTree('button('+Button.ElementID+')');
  Button.Text:='Click me';
  AssertEquals('Text set','Click me',el.innerText);
end;

procedure TTestButtonWidget.TestTextElementID;
Var
  El : TJSHTMLElement;

begin
  el:=TJSHTMLElement(Document.createElement('button'));
  el.id:='b1';
  BaseWindow.appendChild(el);
  El:=AssertTree('button(b1)');
  Button.elementID:='b1';
  Button.Refresh;
  Button.Text:='Click me';
  AssertEquals('Text set','Click me',el.innerText);
end;

procedure TTestButtonWidget.TestClick;

begin
  Button.ParentID:=BaseID;
  Button.Refresh;
  Button.OnClick:=@MyTestEventHandler;
  Button.Click;
  AssertEvent('click',Button);
end;

initialization
  RegisterTests([TTestViewPort,TTestButtonWidget,TTestPage,
                 TTestTextInputElement,TTestTextAreaElement,
                 TTestRadioInputElement,TTestCheckBoxInputElement,
                 TTestDateInputElement,TTestFileInputElement,
                 TTestHiddenInputElement, TTestImageElement,
                 TTestImageElement,
                 TTestLabelWidget,TTestTextWidget,TTestTextLinesWidget,
                 TTestSelectElement,
                 TTestTableWidget]);
end.

