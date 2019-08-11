unit tcWidget;

{$mode objfpc}

interface

uses
  Classes, SysUtils, fpcunit, testregistry, js, web, webwidget;

Const
   SBaseWindowID = 'widget-window'; // Must match what is in HTML File
   BaseID = SBaseWindowID;

   SMyChildID = 'mychild';
   SMyParentID = 'myparent';

Type
  TJSEventClass = Class of TJSEvent;

  { TMyWebWidget }

  TMyWebWidget = Class(TWebWidget)
  private
    FAdd: String;
  Protected
    Function WidgetClasses: String; override;
    Function DoRenderHTML(aParent,aElement : TJSHTMLElement) :TJSHTMLElement; override;
  Public
    Function HTMLTag : String; override;
    Function MyElement : TJSHTMLElement;
    Function MyParent : TJSHTMLElement;
    Function MyContent : TJSHTMLElement;
    Function MyTop : TJSHTMLElement;
    Property AddedClasses : String Read FAdd Write FAdd;
  end;

  { TMyChildWidget }

  TMyChildWidget = Class(TMyWebWidget);

  TMySimpleChildWidget = Class(TMyChildWidget)
  Public
    Class Function AllowChildren : Boolean; override;
  end;
  TMyParentWidget = Class(TMyWebWidget);

  { TMySubContentWidget }

  TMySubContentWidget = Class(TMyParentWidget)
  Private
    FSub: TJSHTMLElement;
  Public
    Function DoRenderHTML(aParent,aElement : TJSHTMLElement) :TJSHTMLElement; override;
    Procedure DoUnRender(aParent: TJSHTMLElement); override;
    function GetContentElement: TJSHTMLELement; override;
  end;

  { TMyPrefixSubContentWidget }

  TMyPrefixSubContentWidget = Class(TMySubContentWidget)
  Private
    FTop : TJSHTMLELement;
  Public
    Function DoRenderHTML(aParent,aElement : TJSHTMLElement) :TJSHTMLElement; override;
    Procedure DoUnRender(aParent: TJSHTMLElement); override;
    function GetTopElement: TJSHTMLELement; override;
  end;

  { TMyRefWidget }

  TMyRefWidget = Class(TMyPrefixSubContentWidget)
  Protected
    FUL : TJSHTMLElement;
    FSubs : TJSHTMLElementArray;
    Function DoRenderHTML(aParent,aElement : TJSHTMLElement) :TJSHTMLElement; override;
  Public
    Property References;
    Property Subs : TJSHTMLElementArray read FSubs;
    Property UL : TJSHTMLElement read FUL;
    Property Element;
  end;


  { TBaseTestWidget }

  TBaseTestWidget = Class(TTestCase)
  private
    FBaseWindow: TJSHTMLElement;
    FEventCount : Integer;
    FEventSender : TObject;
    FEventName : String;
    FDefaultPrevented : Boolean;
  Protected
    Procedure SetUp; override;
    Procedure TearDown; override;
    procedure MyTestEventHandler(Sender: TObject; Event: TJSEvent);
    Procedure AssertEvent(aName : String; aElement : TCustomWebWidget = Nil);
    Function FindElement(aID : String) : TJSHTMLElement;
    Function GetElement(aID : String) : TJSHTMLElement;
    // Checks for /tag(id)/tag(id)
    Function AssertTree(aParent : TJSHTmlElement; aTree : String) :TJSHTMLElement;
    Function AssertTree(aTree : String) : TJSHTMLElement;
  Public
    Property BaseWindow : TJSHTMLElement read FBaseWindow;
    Property EventName : String Read FEventName;
    // Set in MyTestEventHandler;
    Property EventSender : TObject Read FEventSender;
    Property EventCount : Integer Read FEventCount;
    Property DefaultPrevented : Boolean read FDefaultPrevented;
  end;

  { TTestWidgetBasicOperations }

  TTestWidgetBasicOperations = Class(TBaseTestWidget)
  private
    FMy: TMyChildWidget;
    FMyParent: TMyParentWidget;
    FMySub: TMySubContentWidget;
    FMyTop: TMyPrefixSubContentWidget;
    function GetMySub: TMySubContentWidget;
    function GetTop: TMyPrefixSubContentWidget;
  Protected
    Procedure SetUp; override;
    Procedure TearDown; override;
    Function SetupElement : TJSHTMLElement;
    procedure SetupParentElement;
    // Check for event trigger
    procedure TriggerEvent(aName: String; aClass: TJSEventClass=nil);
    // Calls triggerevent and then testevent
    procedure TestEvent(aName: String);
    // Create parent element below windowbase
    function CreateParentElement : TJSHTMLElement;
    // Create child element below windowbase or below parent
    function CreateMyElement(asChild : Boolean = False) : TJSHTMLELement;
    // Set parent of child
    Procedure SetParentElement;
    // Set element ID of child
    Procedure DoSetElementID;
    // Set Parent ID of child
    Procedure DoSetParentID;
    // Set dataset element
    Procedure DoSetDataset;
    // Create parent element and bind parent widget to it.
    function SetupParent: TJSHTMLElement;
    Property MyWidget : TMyChildWidget Read FMy;
    Property MyParentWidget : TMyParentWidget Read FMyParent;
    Property MySubWidget : TMySubContentWidget Read GetMySub;
    Property MyTopWidget : TMyPrefixSubContentWidget Read GetTop;
  Published
//  Public
    Procedure TestEmpty;
    Procedure TestNoElementIDAndParentElementID;
    procedure TestNoParentElementIDAndElementID;
    Procedure TestParentIDToElement;
    Procedure TestElementIDToElement;
    Procedure TestSetParent;
    Procedure TestRenderParent;
    Procedure TestRenderParentID;
    Procedure TestUnRenderParent;
    Procedure TestUnRenderParentID;
    Procedure TestUnRenderElementID;
    Procedure TestSubContent;
    Procedure TestTopContent;
    Procedure TestAddClasses;
    Procedure TestAddClassesNormalized;
    Procedure TestRemoveClasses;
    Procedure TestRemoveClassesNormalized;
    Procedure TestClassesBeforeRender;
    Procedure TestClassesAfterRender;
    Procedure TestWidgetClassesBeforeRender;
    Procedure TestWidgetClassesAfterRender;
    Procedure TestClassesOnElementID;
    Procedure TestStylesBeforeRender;
    Procedure TestStylesAfterRender;
    Procedure TestStylesRefreshOnRender;
    Procedure TestVisibleBeforeRender;
    Procedure TestVisibleAfterRender;
    Procedure TestVisibleBeforeRenderPreserves;
    Procedure TestVisibleAfterRenderPreserves;
    Procedure TestGetData;
    Procedure TestSetData;
    Procedure TestGetDataNotRendered;
    Procedure TestSetDataNotRendered;
    Procedure TestEventClick;
    Procedure TestEventOnAbort;
    Procedure TestEventOnAnimationCancel;
    Procedure TestEventOnAnimationEnd;
    Procedure TestEventOnAnimationIteration;
    Procedure TestEventOnAnimationStart;
    Procedure TestEventOnAuxClick;
    Procedure TestEventOnBlur;
    Procedure TestEventOnCancel;
    Procedure TestEventOnCanPlay;
    Procedure TestEventOnCanPlayThrough;
    Procedure TestEventOnChange;
    Procedure TestEventOnClick;
    Procedure TestEventOnCompositionEnd;
    Procedure TestEventOnCompositionStart;
    Procedure TestEventOnCompositionUpdate;
    Procedure TestEventOnContextMenu;
    Procedure TestEventOnCopy;
    Procedure TestEventOnCut;
    Procedure TestEventOnCueChange;
    Procedure TestEventOnDblClick;
    Procedure TestEventOnDurationChange;
    Procedure TestEventOnEnded ;
    Procedure TestEventOnError ;
    Procedure TestEventOnFocus;
    Procedure TestEventOnFocusIn ;
    Procedure TestEventOnFocusOut ;
    Procedure TestEventOnGotPointerCapture;
    Procedure TestEventOnInput;
    Procedure TestEventOnInvalid;
    Procedure TestEventOnKeyDown;
    Procedure TestEventOnKeyPress;
    Procedure TestEventOnKeyUp;
    Procedure TestEventOnLoad;
    Procedure TestEventOnLoadedData;
    Procedure TestEventOnLoadedMetaData;
    Procedure TestEventOnLoadend;
    Procedure TestEventOnLoadStart;
    Procedure TestEventOnLostPointerCapture;
    Procedure TestEventOnMouseDown;
    Procedure TestEventOnMouseEnter;
    Procedure TestEventOnMouseLeave;
    Procedure TestEventOnMouseMove;
    Procedure TestEventOnMouseOut;
    Procedure TestEventOnMouseUp;
    Procedure TestEventOnOverFlow;
    Procedure TestEventOnPaste;
    Procedure TestEventOnPause;
    Procedure TestEventOnPlay;
    Procedure TestEventOnPointerCancel;
    Procedure TestEventOnPointerDown;
    Procedure TestEventOnPointerEnter;
    Procedure TestEventOnPointerLeave;
    Procedure TestEventOnPointerMove;
    Procedure TestEventOnPointerOut;
    Procedure TestEventOnPointerOver;
    Procedure TestEventOnPointerUp;
    Procedure TestEventOnReset;
    Procedure TestEventOnResize;
    Procedure TestEventOnScroll;
    Procedure TestEventOnSelect;
    Procedure TestEventOnSubmit;
    Procedure TestEventOnTouchStart;
    Procedure TestEventOnTransitionCancel;
    Procedure TestEventOnTransitionEnd;
    Procedure TestEventOnTransitionRun;
    Procedure TestEventOnTransitionStart;
    Procedure TestEventOnWheel;
  end;

  { TTestWebWidgetStyles }

  TTestWebWidgetStyles = Class(TBaseTestWidget)
  private
    FMy: TMyChildWidget;
    procedure AddDefaults;
    procedure AssertStyle(aIdx: integer; const AName, aValue: String; CheckElement: Boolean=True);
    function GetItems: TWebWidgetStyles;
    procedure GetNonExist;
  Public
    Procedure Setup; override;
    Procedure TearDown; override;
    Property MyWidget : TMyChildWidget Read FMy;
    Property Styles : TWebWidgetStyles Read GetItems;
  Published
    Procedure TestEmpty;
    Procedure TestAdd;
    Procedure TestAddOnRender;
    Procedure TestAddAfterRender;
    Procedure TestIndexOf;
    Procedure TestFind;
    Procedure TestGet;
    Procedure TestEnsure;
    Procedure TestEnsureRendered;
    Procedure TestApplyToDOM;
    Procedure TestRefreshFromDOM;
    Procedure TestDirty;
    Procedure TestClearImported;
  end;

  { TTestWebWidgetReferences }

  TTestWebWidgetReferences = Class(TBaseTestWidget)
  private
    FMy: TMyRefWidget;
    F : TReferenceItem;
    function GetItems: TWebWidgetReferences;
  Public
    Procedure Setup; override;
    Procedure TearDown; override;
    Procedure AddSet;
    Procedure GetDRef;
    Property MyWidget : TMyRefWidget Read FMy;
    Property References : TWebWidgetReferences Read GetItems;
  Published
    Procedure TestEmpty;
    Procedure TestAdd;
    Procedure TestIndexOf;
    Procedure TestIndexOfCaseInsensitive;
    Procedure TestFind;
    Procedure TestGet;
    Procedure SelectSingleBeforeRefresh;
    Procedure GetSingleByName;
    Procedure GetSingleNonExist;
    Procedure GetSingleByNameCaseInsensitive;
    Procedure SelectMultiBeforeRefresh;
    procedure GetMultiByName;
    procedure GetMultiNonExist;
    Procedure SelectSingleAfterRefresh;
    Procedure SelectMultiAfterRefresh;
  end;

implementation

{ TMyChildWidget }

class function TMySimpleChildWidget.AllowChildren: Boolean;
begin
  Result:=FAlse;
end;

{ TMyRefWidget }

function TMyRefWidget.DoRenderHTML(aParent, aElement: TJSHTMLElement): TJSHTMLElement;
{
  Final tree should be
  <div> <!-- top -->
    <div> <!-- element -->
      <ul>
        <li>item 0</li>
        <!-- ... -->
        <li>item 9</li>
      </ul>
      <div> <!-- 'sub' content element -->
      </div>
    </div>
  </div>
}
Var
  I : Integer;
begin
  Result:=inherited DoRenderHTML(aParent, aElement);
  FUL:=TJSHTMLElement(Document.CreateElement('ul'));
  Result.insertBefore(Ful,FSub);
  SetLength(FSubs,10);
  For I:=0 to 9 do
    begin
    FSubs[i]:=TJSHTMLElement(Document.CreateElement('li'));
    FSubs[i].InnerText:='item '+IntToStr(I);
    FUL.AppendChild(FSubs[i]);
    end;
end;

{ TTestWebWidgetReferences }

function TTestWebWidgetReferences.GetItems: TWebWidgetReferences;
begin
  Result:=FMy.References;
end;

procedure TTestWebWidgetReferences.Setup;
begin
  inherited Setup;
  FMy:=TMyRefWidget.Create(Nil);
  FMy.ParentID:=SBaseWindowID;
end;

procedure TTestWebWidgetReferences.TearDown;
begin
  FreeAndNil(FMy);
  inherited TearDown;
end;

procedure TTestWebWidgetReferences.AddSet;
begin
  MyWidget.References.Add('a','div');
  MyWidget.References.Add('b','ul');
  MyWidget.References.Add('c','ul>li');
end;

procedure TTestWebWidgetReferences.GetDRef;
begin
  F:=References.GetReference('D');
end;

procedure TTestWebWidgetReferences.TestEmpty;
begin
  AssertNotNull('Have widget',MyWidget);
  AssertNull('widget not rendered',MyWidget.Element);
  AssertEquals('No references',0,MyWidget.References.Count);
  AssertSame('Correct references prop',MyWidget.References,References);
end;

procedure TTestWebWidgetReferences.TestAdd;
begin
  AddSet;
  AssertEquals('Count',3,references.Count);
  AssertEquals('0 : Name','a',references[0].name);
  AssertEquals('0 : Selector','div',references[0].Selector);
  AssertEquals('1 : Name','b',references[1].name);
  AssertEquals('1 : Selector','ul',references[1].selector);
  AssertEquals('2 : Name','c',references[2].name);
  AssertEquals('2 : Selector','ul>li',references[2].selector);
end;

procedure TTestWebWidgetReferences.TestIndexOf;
begin
  AddSet;
  AssertEquals('a',0,References.IndexOfReference('a'));
  AssertEquals('b',1,References.IndexOfReference('b'));
  AssertEquals('c',2,References.IndexOfReference('c'));
  AssertEquals('d',-1,References.IndexOfReference('d'));
end;

procedure TTestWebWidgetReferences.TestIndexOfCaseInsensitive;
begin
  AddSet;
  AssertEquals('a',0,References.IndexOfReference('A'));
  AssertEquals('b',1,References.IndexOfReference('B'));
  AssertEquals('c',2,References.IndexOfReference('C'));
  AssertEquals('d',-1,References.IndexOfReference('D'));
end;

procedure TTestWebWidgetReferences.TestFind;
begin
  AddSet;
  AssertSame('A',References[0],References.FindReference('A'));
  AssertSame('a',References[0],References.FindReference('a'));
  AssertNull('e',References.FindReference('E'));
end;

procedure TTestWebWidgetReferences.TestGet;
begin
  AddSet;
  AssertSame('A',References[0],References.GetReference('A'));
  AssertSame('a',References[0],References.GetReference('a'));
  AssertException('Get unknown',EWidgets,@GetDRef);
end;

procedure TTestWebWidgetReferences.SelectSingleBeforeRefresh;
begin
  MyWidget.References.Add('me','ul');
  AssertEquals('Ref count',1,MyWidget.References.Count);
  MyWidget.Refresh;
  AssertEquals('count of References found',1,Length(References[0].Elements));
  AssertSame('first array Reference filled',MyWidget.UL,References[0].Elements[0]);
  AssertSame('Reference filled',MyWidget.UL,References[0].Element);
end;

procedure TTestWebWidgetReferences.GetSingleByName;
begin
  MyWidget.References.Add('me','ul');
  AssertEquals('Ref count',1,MyWidget.References.Count);
  MyWidget.Refresh;
  AssertEquals('count of References found',1,Length(References[0].Elements));
  AssertSame('first array Reference filled',MyWidget.UL,References[0].Elements[0]);
  AssertSame('Reference filled',MyWidget.UL,MyWidget.references.GetElementByName('me'));
  AssertEquals('Reference filled',1,Length(MyWidget.references.GetElementsByName('me')));
  AssertSame('Reference filled',MyWidget.UL,MyWidget.references.GetElementsByName('me')[0]);
end;

procedure TTestWebWidgetReferences.GetSingleNonExist;
begin
  MyWidget.References.Add('me','ul');
  AssertEquals('Ref count',1,MyWidget.References.Count);
  MyWidget.Refresh;
  AssertNull('Reference filled',MyWidget.references.GetElementByName('a'));
end;

procedure TTestWebWidgetReferences.GetSingleByNameCaseInsensitive;
begin
  MyWidget.References.Add('ME','ul');
  AssertEquals('Ref count',1,MyWidget.References.Count);
  MyWidget.Refresh;
  AssertEquals('count of References found',1,Length(References[0].Elements));
  AssertSame('first array Reference filled',MyWidget.UL,References[0].Elements[0]);
  AssertSame('Reference filled',MyWidget.UL,MyWidget.references.GetElementByName('me'));
  AssertEquals('Reference filled',1,Length(MyWidget.references.GetElementsByName('me')));
  AssertSame('Reference filled',MyWidget.UL,MyWidget.references.GetElementsByName('me')[0]);
end;

procedure TTestWebWidgetReferences.SelectMultiBeforeRefresh;

Var
  I : integer;

begin
  MyWidget.References.Add('me','li');
  AssertEquals('Ref count',1,MyWidget.References.Count);
  MyWidget.Refresh;
  AssertEquals('Count of References found',10,Length(References[0].Elements));
  for I:=0 to Length(MyWidget.FSubs)-1 do
    AssertSame('1 array Reference filled',MyWidget.Subs[I],References[0].Elements[i]);
  AssertSame('first Reference filled',MyWidget.Subs[0],References[0].Element);
end;

procedure TTestWebWidgetReferences.GetMultiByName;

Var
  I : integer;
  a : TJSHTMLElementArray;

begin
  MyWidget.References.Add('me','li');
  AssertEquals('Ref count',1,MyWidget.References.Count);
  MyWidget.Refresh;
  a:=References.GetElementsByname('me');
  AssertEquals('Count of References found',10,Length(a));
  for I:=0 to Length(MyWidget.FSubs)-1 do
    AssertSame('1 array Reference filled',MyWidget.Subs[I],a[i]);
end;

procedure TTestWebWidgetReferences.GetMultiNonExist;
Var
  a : TJSHTMLElementArray;
begin
  MyWidget.References.Add('me','li');
  AssertEquals('Ref count',1,MyWidget.References.Count);
  MyWidget.Refresh;
  A:=References.GetElementsByName('no');
  AssertNotNull('Empty array 1',A);
  AssertEquals('Empty array 2',0, Length(A));
end;

procedure TTestWebWidgetReferences.SelectSingleAfterRefresh;
begin
  MyWidget.Refresh;
  MyWidget.References.Add('me','ul');
  AssertEquals('Ref count',1,MyWidget.References.Count);
  AssertEquals('count of References found',1,Length(References[0].Elements));
  AssertSame('first array Reference filled',MyWidget.UL,References[0].Elements[0]);
  AssertSame('Reference filled',MyWidget.UL,References[0].Element);
end;

procedure TTestWebWidgetReferences.SelectMultiAfterRefresh;
Var
  I : integer;

begin
  MyWidget.Refresh;
  MyWidget.References.Add('me','li');
  AssertEquals('Ref count',1,MyWidget.References.Count);
  AssertEquals('Count of References found',10,Length(References[0].Elements));
  for I:=0 to Length(MyWidget.FSubs)-1 do
    AssertSame('1 array Reference filled',MyWidget.Subs[I],References[0].Elements[i]);
  AssertSame('first Reference filled',MyWidget.Subs[0],References[0].Element);
end;

{ TTestWebWidgetStyles }

function TTestWebWidgetStyles.GetItems: TWebWidgetStyles;
begin
  Result:=MyWidget.Styles;
end;

procedure TTestWebWidgetStyles.Setup;
begin
  inherited Setup;
  FMy:=TMyChildWidget.Create(Nil);
  FMy.ParentID:=SBaseWindowID;
end;

procedure TTestWebWidgetStyles.TearDown;
begin
  FreeAndNil(FMy);
  inherited TearDown;
end;

procedure TTestWebWidgetStyles.TestEmpty;
begin
  AssertNotNull('Have widget',MyWidget);
  AssertNotNull('Have widget styles',MyWidget.Styles);
  AssertNull('Not yet rendered',MyWidget.MyElement);
end;

procedure TTestWebWidgetStyles.AssertStyle(aIdx : integer; const AName,aValue : String; CheckElement : Boolean = True);

begin
  AssertTrue('Correct index',(aIdx>=0) and (aIdx<Styles.Count));
  AssertEquals('Correct name at index',aName,Styles[aIdx].name);
  AssertEquals('Correct value at index',aValue,Styles[aIdx].Value);
  if CheckElement then
    begin
    AssertNotNull('Must check element, have element',MyWidget.MyElement);
    AssertEquals('Correct style applied',aValue,MyWidget.MyElement.style.getPropertyValue(aName));
    end;
end;


procedure TTestWebWidgetStyles.TestAdd;
begin
  Styles.Add('display','none');
  AssertEquals('Count',1,Styles.Count);
  AssertStyle(0,'display','none',False);
end;

procedure TTestWebWidgetStyles.TestAddOnRender;
begin
  TestAdd;
  MyWidget.Refresh;
  AssertStyle(0,'display','none',True);
end;

procedure TTestWebWidgetStyles.TestAddAfterRender;

begin
  MyWidget.Refresh;
  Styles.Add('display','none');
  AssertStyle(0,'display','none',True);
end;

procedure TTestWebWidgetStyles.AddDefaults;

begin
  Styles.Add('display','none');
  Styles.Add('width','10px');
  Styles.Add('height','20px');
end;

procedure TTestWebWidgetStyles.TestIndexOf;
begin
  AddDefaults;
  AssertEquals('Non-existing',-1,Styles.IndexOfStyle('wo'));
  AssertEquals('display',0,Styles.IndexOfStyle('display'));
  AssertEquals('width',1,Styles.IndexOfStyle('width'));
  AssertEquals('height',2,Styles.IndexOfStyle('height'));
end;

procedure TTestWebWidgetStyles.TestFind;
begin
  AddDefaults;
  AssertSame('Non-existing',nil,Styles.FindStyle('wo'));
  AssertSame('display',Styles[0],Styles.FindStyle('display'));
  AssertSame('width',Styles[1],Styles.FindStyle('width'));
  AssertSame('height',Styles[2],Styles.FindStyle('height'));
end;

procedure TTestWebWidgetStyles.GetNonExist;

begin
  Styles.GetStyle('Wo');
end;

procedure TTestWebWidgetStyles.TestGet;
begin
  AddDefaults;
  AssertException('Non-existing',EWidgets,@GetNonExist);
  AssertSame('display',Styles[0],Styles.FindStyle('display'));
  AssertSame('width',Styles[1],Styles.FindStyle('width'));
  AssertSame('height',Styles[2],Styles.FindStyle('height'));
end;

procedure TTestWebWidgetStyles.TestEnsure;

Var
  El : TStyleItem;

begin
  AddDefaults;
  El:=Styles.EnsureStyle('background-color','red');
  AssertSame('In coll',El,Styles.FindStyle('background-color'));
end;

procedure TTestWebWidgetStyles.TestEnsureRendered;

begin
  MyWidget.Refresh;
  AddDefaults;
  Styles.EnsureStyle('background-color','red');
  AssertStyle(3,'background-color','red',True);
end;

procedure TTestWebWidgetStyles.TestApplyToDOM;

Var
  El : TJSHTmlElement;

begin
  AddDefaults;
  El:=TJSHTmlElement(Document.CreateElement('div'));
  Styles.ApplyToDOM(El);
  AssertEquals('Set 1','none',EL.Style.getPropertyValue('display'));
  AssertEquals('Set 2','10px',EL.Style.getPropertyValue('width'));
  AssertEquals('Set 3','20px',EL.Style.getPropertyValue('height'));
end;

procedure TTestWebWidgetStyles.TestRefreshFromDOM;

Var
  El : TJSHTmlElement;
  Idx : integer;

begin
  AddDefaults;
  El:=TJSHTmlElement(Document.CreateElement('div'));
  El.Style.setProperty('display','block');
  El.Style.setProperty('min-width','40px');
  El.Style.setProperty('min-height','50px');
  Styles.RefreshFromDOM(el,True);
  idx:=Styles.IndexOfStyle('display');
  AssertTrue('Have display',idx<>-1);
  AssertStyle(idx,'display','block',False);
  AssertTrue('display imported',Styles[idx].Imported);
  idx:=Styles.IndexOfStyle('min-width');
  AssertTrue('Have min-width',idx<>-1);
  AssertStyle(idx,'min-width','40px',False);
  AssertTrue('min-width imported',Styles[idx].Imported);
  idx:=Styles.IndexOfStyle('min-height');
  AssertTrue('Have min-height',idx<>-1);
  AssertStyle(idx,'min-height','50px',False);
  AssertTrue('min-height imported',Styles[idx].Imported);
end;

procedure TTestWebWidgetStyles.TestDirty;
begin
  MyWidget.Refresh;
  Styles.Add('display','none');
  AssertStyle(0,'display','none',True);
  Styles.EnsureStyle('display','block');
  AssertStyle(0,'display','block',True);
end;

procedure TTestWebWidgetStyles.TestClearImported;
begin
  TestRefreshFromDOM;
  Styles.ClearImported;
  AssertEquals('None left',0,Styles.Count);
end;

{ TMyPrefixSubContentWidget }

function TMyPrefixSubContentWidget.DoRenderHTML(aParent, aElement: TJSHTMLElement): TJSHTMLElement;
begin
  Result:=inherited DoRenderHTML(aParent, aElement);
  FTop:=CreateElement('span',aElement.ID+'-span');
  aParent.removeChild(aElement);
  FTop.AppendChild(aElement);
  aParent.appendChild(FTop);
end;

procedure TMyPrefixSubContentWidget.DoUnRender(aParent: TJSHTMLElement);
begin
  inherited DoUnRender(aParent);
  FTop:=Nil;
end;

function TMyPrefixSubContentWidget.GetTopElement: TJSHTMLELement;
begin
  Result:=FTop;
end;


{ TMySubContentWidget }

function TMySubContentWidget.DoRenderHTML(aParent, aElement: TJSHTMLElement): TJSHTMLElement;
begin
  Result:=inherited DoRenderHTML(aParent, aElement);
  FSub:=CreateElement('div',aElement.ID+'-sub');
  Result.AppendChild(FSub);
end;

procedure TMySubContentWidget.DoUnRender(aParent: TJSHTMLElement);
begin
  inherited DoUnRender(aParent);
  FSub:=nil;
end;

function TMySubContentWidget.GetContentElement: TJSHTMLELement;
begin
  Result:=FSub;
end;

{ TMyWebWidget }

function TMyWebWidget.WidgetClasses: String;
begin
  Result:=FAdd;
end;

function TMyWebWidget.DoRenderHTML(aParent,aElement: TJSHTMLElement): TJSHTMLElement;
begin
  // Do nothing
  if aParent<>Nil then;
  Result:=aElement;
end;

function TMyWebWidget.HTMLTag: String;
begin
  Result:='div'
end;

function TMyWebWidget.MyElement: TJSHTMLElement;
begin
  Result:=Element;
end;

function TMyWebWidget.MyParent: TJSHTMLElement;
begin
  Result:=ParentElement;
end;

function TMyWebWidget.MyContent: TJSHTMLElement;
begin
  Result:=ContentElement;
end;

function TMyWebWidget.MyTop: TJSHTMLElement;

begin
  Result:=TopElement;
end;

{ TTestWidgetBasicOperations }

function TTestWidgetBasicOperations.GetTop: TMyPrefixSubContentWidget;
begin
  if FMyTop=Nil then
    FMyTop:=TMyPrefixSubContentWidget.Create(Nil);
  Result:=FMyTop;
end;

function TTestWidgetBasicOperations.GetMySub: TMySubContentWidget;
begin
  If FMySub=Nil then
    FMySub:=TMySubContentWidget.Create(Nil);
  Result:=FMySub;
end;

procedure TTestWidgetBasicOperations.SetUp;

begin
  inherited SetUp;
  FMy:=TMyChildWidget.Create(Nil);
  AssertEquals('Correct tag','div',FMy.HTMLTag);
  FMyParent:=TMyParentWidget.Create(Nil);
  AssertEquals('Correct parent tag','div',FMyParent.HTMLTag);
  FEventCount:=0;
  FEventSender:=Nil;
  FEventName:='';
  FDefaultPrevented:=False;
  FMySub:=Nil;
  FMyTop:=Nil;
end;

procedure TTestWidgetBasicOperations.TearDown;
begin
  FreeAndNil(FMySub);
  FreeAndNil(FMyTop);
  FreeAndNil(FMy);
  FreeAndNil(FMyParent);
  inherited TearDown;
end;

procedure TTestWidgetBasicOperations.TriggerEvent(aName: String; aClass : TJSEventClass = Nil);

Var
  ev : TJSEvent;

begin
  if aClass=Nil then
    ev:=TJSEvent.New(aName)
  else
    ev:=aClass.New(aName);
  FDefaultPrevented:=FMy.EnsureElement.dispatchEvent(ev);
end;



function TTestWidgetBasicOperations.CreateParentElement: TJSHTMLElement;


begin
  Result:=TJSHTMLElement(Document.CreateElement('div'));
  Result.Id:=SMyParentID;
  BaseWindow.AppendChild(Result);
end;

function TTestWidgetBasicOperations.CreateMyElement(asChild: Boolean = False): TJSHTMLELement;
Var
  El : TJSHTMLElement;
begin
  if AsChild then
    El:=CreateParentElement
  else
    El:=BaseWindow;
  Result:=TJSHTMLElement(Document.CreateElement('div'));
  Result.Id:=SMyChildID;
  El.AppendChild(Result);
end;

procedure TTestWidgetBasicOperations.SetParentElement;

begin
  FMy.Parent:=FMyParent;
end;

procedure TTestWidgetBasicOperations.DoSetElementID;
begin
  FMy.ElementID:=SMyChildID;
end;

procedure TTestWidgetBasicOperations.DoSetParentID;
begin
  FMy.ParentID:=SMyParentID;;
end;

procedure TTestWidgetBasicOperations.DoSetDataset;
begin
  MyWidget.Data['name']:='me';
end;


function TTestWidgetBasicOperations.SetupParent: TJSHTMLElement;
begin
  Result:=CreateParentElement;
  FMyParent.ElementID:=SMyParentID;
end;

procedure TTestWidgetBasicOperations.TestEmpty;
begin
  AssertNotNull(MyWidget);
  AssertNotNull(MyParentWidget);
end;

procedure TTestWidgetBasicOperations.TestNoElementIDAndParentElementID;
begin
  DoSetElementID;
  AssertException('Cannot set both',EWidgets,@DoSetParentID);
end;

procedure TTestWidgetBasicOperations.TestNoParentElementIDAndElementID;
begin
  DoSetParentID;
  AssertException('Cannot set both',EWidgets,@DoSetElementID);
end;

procedure TTestWidgetBasicOperations.TestParentIDToElement;

Var
  El : TJSHTMLElement;

begin
  El:=CreateParentElement;
  DoSetParentID;
  AssertSame('Correct parent element',El,MyWidget.MyParent);
  AssertSame('Correct content element',MyWidget.MyElement,MyWidget.MyContent);
  AssertTree('div('+SMyParentID+')');
end;

procedure TTestWidgetBasicOperations.TestElementIDToElement;
Var
  El : TJSHTMLElement;

begin
  El:=CreateMyElement;
  DoSetElementID;
  AssertSame('Correct element',El,MyWidget.MyElement);
  AssertSame('Correct content element',El,MyWidget.MyContent);
  AssertTree('div('+SMyChildID+')');
  AssertEquals('Have element data',el.ID,String(el.dataset['wwElement']));
  AssertEquals('Have element top data',el.ID,String(el.dataset['wwElementTop']));
  AssertEquals('Have element content data',el.ID,String(el.dataset['wwElementContent']));
end;

procedure TTestWidgetBasicOperations.TestSetParent;

Var
  El : TJSHTMLElement;

begin
  El:=SetupParent;
  AssertSame('Correct parent element',El,MyParentWidget.MyElement);
  MyWidget.Parent:=MyParentWidget;
  AssertEquals('Child count correct',1,MyParentWidget.ChildCount);
  AssertSame('Correct parent element',El,MyWidget.MyParent);
  AssertNull('No element yet',MyWidget.Element);
  MyWidget.Refresh;
  AssertTrue('Have element ID',MyWidget.ElementID<>'');
  AssertTree('div('+SMyParentID+')/div('+MyWidget.ElementID+')'); // No ID assigned !
  MyWidget.Parent:=Nil;
  AssertEquals('Child count correct',0,MyParentWidget.ChildCount);
  AssertNull('No more parent element ',MyWidget.MyParent);
end;

procedure TTestWidgetBasicOperations.TestRenderParent;

Var
  El,El2 : TJSHTMLElement;

begin
  El:=SetupParent;
  MyWidget.Parent:=MyParentWidget;
  MyWidget.Refresh;
  El2:=MyWidget.MyElement;
  AssertNotNull('Have element',El2);
  AssertSame('Have content element',El2,MyWidget.MyContent);
  AssertSame('Have correct parent element',El,El2.parentElement);
  AssertSame('Have correct parent',El,MyWidget.MyParent);
  AssertEquals('Correct ID',el2.ID,MyWidget.ElementiD);
  AssertEquals('Have element data',el2.ID,String(el2.dataset['wwElement']));
  AssertEquals('Have element top data',el2.ID,String(el2.dataset['wwElementTop']));
  AssertEquals('Have element content data',el2.ID,String(el2.dataset['wwElementContent']));
end;

procedure TTestWidgetBasicOperations.TestRenderParentID;

Var
  El,El2 : TJSHTMLElement;

begin
  El:=CreateParentElement;
  MyWidget.ParentID:=el.ID;
  MyWidget.Refresh;
  El2:=MyWidget.MyElement;
  AssertNotNull('Have element',El2);
  AssertSame('Have content element',El2,MyWidget.MyContent);
  AssertSame('Have correct parent element',El,El2.parentElement);
  AssertSame('Have correct parent',El,MyWidget.MyParent);
  AssertEquals('Correct ID',el2.ID,MyWidget.ElementiD);
  AssertEquals('Have element data',el2.ID,String(el2.dataset['wwElement']));
  AssertEquals('Have element top data',el2.ID,String(el2.dataset['wwElementTop']));
  AssertEquals('Have element content data',el2.ID,String(el2.dataset['wwElementContent']));
end;

procedure TTestWidgetBasicOperations.TestUnRenderParent;
Var
  El,El2 : TJSHTMLElement;

begin
  El:=SetupParent;
  MyWidget.Parent:=MyParentWidget;
  MyWidget.Refresh;
  El2:=MyWidget.MyElement;
  AssertNotNull('Have element',El2);
  MyWidget.Parent:=Nil;
  AssertEquals('Not rendered any more',0,el.childElementCount);
  AssertNull('Have no more element',MyWidget.MyElement);
  AssertNull('Have no more parent element',MyWidget.MyParent);
end;

procedure TTestWidgetBasicOperations.TestUnRenderParentID;
Var
  El,El2 : TJSHTMLElement;

begin
  El:=CreateParentElement;
  MyWidget.ParentID:=el.ID;
  MyWidget.Refresh;
  El2:=MyWidget.MyElement;
  AssertNotNull('Have element',El2);
  MyWidget.ParentID:='';
  AssertEquals('Not rendered any more',0,el.childElementCount);
  AssertNull('Have no more element',MyWidget.MyElement);
  AssertNull('Have no more parent element',MyWidget.MyParent);
end;

procedure TTestWidgetBasicOperations.TestUnRenderElementID;

Var
  El : TJSHTMLElement;

begin
  TestElementIDToElement;
  El:=MyWidget.MyElement;
  // This will unrender
  MyWidget.ElementID:='';
  AssertTrue('Have removed element data', IsUndefined(el.dataset['WwElement']));
  AssertTrue('Have removed element top data',isUndefined(el.dataset['WwElementTop']));
  AssertTrue('Have removed element content data',isUndefined(el.dataset['WwElementContent']));

end;

procedure TTestWidgetBasicOperations.TestSubContent;


begin
  SetupParentElement;
  MySubWidget.Parent:=MyParentWidget;
  MySubWidget.Refresh;
  AssertTree('div('+SMyParentID+')/div('+MySubWidget.ElementID+')/div('+MySubWidget.ElementID+'-sub)');
  AssertNotNull('have content',MySubWidget.MyContent);
  AssertSame('content element parent is element',MySubWidget.MyElement,MySubWidget.MyContent.parentElement);
end;

procedure TTestWidgetBasicOperations.TestTopContent;
begin
{  SetupParentElement;
  MySubWidget.Parent:=MyParentWidget;
  MySubWidget.Refresh;
  AssertTree('div('+SMyParentID+')/span('+MySubWidget.ElementID+'-top)/div('+MySubWidget.ElementID+')/div('+MySubWidget.ElementID+'-sub)');
  AssertNotNull('have content',MySubWidget.MyContent);
  AssertNotNull('have top content',MySubWidget.MyTop);
  AssertSame('top element parent is element',MyParentWidget.MyContent,MySubWidget.MyTop.parentElement);
  AssertSame('element parent is top element',MySubWidget.MyTop,MySubWidget.MyElement.parentElement);
  AssertSame('content element parent is element',MySubWidget.MyElement,MySubWidget.MyContent.parentElement);}
end;

procedure TTestWidgetBasicOperations.TestAddClasses;

Var
  S : String;

begin
  S:=TWebWidget.AddClasses('a   b   c','d c e');
  AssertEquals('Correctly added, no duplicates','a   b   c d e',S);
end;

procedure TTestWidgetBasicOperations.TestAddClassesNormalized;
Var
  S : String;

begin
  S:=TWebWidget.AddClasses('a   b   c','d c e',true);
  AssertEquals('Correctly added, no duplicates','a b c d e',S);
end;

procedure TTestWidgetBasicOperations.TestRemoveClasses;

Var
  S : String;

begin
  S:=TWebWidget.RemoveClasses('a   b   c','d c e');
  AssertEquals('Correctly removed','a   b  ',S);
end;

procedure TTestWidgetBasicOperations.TestRemoveClassesNormalized;
Var
  S : String;

begin
  S:=TWebWidget.RemoveClasses('a   b   c','d c e',True);
  AssertEquals('Correctly removed','a b',S);
end;

procedure TTestWidgetBasicOperations.TestClassesBeforeRender;
begin
  SetupParent;
  MyWidget.Parent:=MyParentWidget;
  MyWidget.Classes:='a b c';
  MyWidget.Refresh;
  AssertNotNull('Have element',MyWidget.Element);
  AssertEquals('Have element classes ','a b c',MyWidget.Element.className);
end;

procedure TTestWidgetBasicOperations.TestClassesAfterRender;
begin
  SetupParent;
  MyWidget.Parent:=MyParentWidget;
  MyWidget.Refresh;
  AssertNotNull('Have element',MyWidget.Element);
  MyWidget.Classes:='a b c';
  AssertEquals('Have element classes ','a b c',MyWidget.Element.className);
end;

procedure TTestWidgetBasicOperations.TestWidgetClassesBeforeRender;
begin
  SetupParent;
  MyWidget.Parent:=MyParentWidget;
  MyWidget.AddedClasses:='d e c';
  MyWidget.Refresh;
  AssertNotNull('Have element',MyWidget.Element);
  MyWidget.Classes:='a b c';
  AssertEquals('Have element classes ','a b c d e',MyWidget.Element.className);
end;

procedure TTestWidgetBasicOperations.TestWidgetClassesAfterRender;
begin
  SetupParent;
  MyWidget.Parent:=MyParentWidget;
  MyWidget.Refresh;
  AssertNotNull('Have element',MyWidget.Element);
  MyWidget.AddedClasses:='d e c';
  // They are not yet added
  AssertEquals('Have element classes before setting','',MyWidget.Element.className);
  // but now they are added
  MyWidget.Classes:='a b c';
  AssertEquals('Have element classes ','a b c d e',MyWidget.Element.className);
end;

procedure TTestWidgetBasicOperations.TestClassesOnElementID;

var
  el : TJSHTMLElement;

begin
  el:=CreateMyElement();
  MyWidget.Classes:='a b c';
  MyWidget.ElementID:=SMyChildID;
  AssertNotNull('Have element',MyWidget.MyElement);
  AssertSame('Have element',El,MyWidget.MyElement);
  AssertEquals('Have element classes ','a b c',MyWidget.Element.className);
end;

procedure TTestWidgetBasicOperations.TestStylesBeforeRender;
begin
  MyWidget.ParentID:=BaseID;
  MyWidget.Styles.Add('display').Value:='none';
  MyWidget.Refresh;
  AssertTree('div('+MyWidget.ElementID+')');
  AssertEquals('have style applied','none',MyWidget.MyElement.style.getPropertyValue('display'));
end;

procedure TTestWidgetBasicOperations.TestStylesAfterRender;
begin
  MyWidget.ParentID:=BaseID;
  MyWidget.Refresh;
  MyWidget.Styles.Add('display').Value:='none';
  AssertTree('div('+MyWidget.ElementID+')');
  AssertEquals('have style applied','none',MyWidget.MyElement.style.getPropertyValue('display'));
end;

procedure TTestWidgetBasicOperations.TestStylesRefreshOnRender;

var
  Idx : Integer;

begin
  CreateMyElement(False).Style.setProperty('width','30px');
  AssertTree('div('+MyWidget.ElementID+')');
  MyWidget.ElementID:=SMyChildID;
  AssertTree('div('+MyWidget.ElementID+')');
  MyWidget.Refresh;
  MyWidget.Styles.Add('display').Value:='none';
  AssertEquals('have style applied','none',MyWidget.MyElement.style.getPropertyValue('display'));
  AssertEquals('have pre-existing imported: count',2,MyWidget.Styles.Count);
  Idx:=MyWidget.Styles.IndexOfStyle('width');
  AssertTrue('have pre-existing imported: find name',Idx<>-1);
  AssertEquals('have pre-existing imported: have value','30px',MyWidget.Styles[idx].Value);
  AssertEquals('have pre-existing imported: marked as imported',True,MyWidget.Styles[idx].IMported);
end;

procedure TTestWidgetBasicOperations.TestVisibleBeforeRender;
begin
  MyWidget.Visible:=False;
  SetupElement;
  AssertFalse('Visible false',MyWidget.Visible);
  AssertEquals('Display none','none',MyWidget.MyElement.Style.getPropertyValue('display'));
end;

procedure TTestWidgetBasicOperations.TestVisibleAfterRender;
begin
  SetupElement;
  MyWidget.Refresh;
  MyWidget.Visible:=False;
  AssertFalse('Visible false',MyWidget.Visible);
  AssertEquals('Display none','none',MyWidget.MyElement.Style.getPropertyValue('display'));
end;

procedure TTestWidgetBasicOperations.TestVisibleBeforeRenderPreserves;
begin
  MyWidget.Visible:=False;
  SetupElement.style.setProperty('display','inline');
  MyWidget.Refresh;
  AssertFalse('Visible false',MyWidget.Visible);
  AssertEquals('Display none','none',MyWidget.MyElement.Style.getPropertyValue('display'));
  MyWidget.Visible:=True;
  AssertTrue('Visible true',MyWidget.Visible);
  AssertEquals('Display none','inline',MyWidget.MyElement.Style.getPropertyValue('display'));
end;

procedure TTestWidgetBasicOperations.TestVisibleAfterRenderPreserves;
begin
  SetupElement.style.setProperty('display','inline');
  MyWidget.Refresh;
  MyWidget.Visible:=False;
  AssertFalse('Visible false',MyWidget.Visible);
  AssertEquals('Display none','none',MyWidget.MyElement.Style.getPropertyValue('display'));
  MyWidget.Visible:=True;
  AssertTrue('Visible true',MyWidget.Visible);
  AssertEquals('Display none','inline',MyWidget.MyElement.Style.getPropertyValue('display'));
end;

procedure TTestWidgetBasicOperations.TestGetData;
begin
  SetupElement.Dataset['name']:='me';
  AssertEquals('Correctly read','me',MyWidget.Data['name']);
end;

procedure TTestWidgetBasicOperations.TestSetData;

var
  el : TJSHTMLElement;
begin
  El:=SetupElement;
  MyWidget.Data['name']:='me';
  AssertEquals('Correctly set','me',String(el.Dataset['name']));
end;

procedure TTestWidgetBasicOperations.TestGetDataNotRendered;
begin
  AssertEquals('Correctly read','',MyWidget.Data['name']);
end;

procedure TTestWidgetBasicOperations.TestSetDataNotRendered;
begin
  AssertException('Cannot write',EWidgets,@DoSetDataset);
end;

function TTestWidgetBasicOperations.SetupElement: TJSHTMLElement;

begin
  Result:=CreateMyElement(False);
  MyWidget.ElementID:=SMyChildID;
end;

procedure TTestWidgetBasicOperations.SetupParentElement;
begin
  CreateParentElement;
  MyParentWidget.ElementID:=SMyParentID;
end;

procedure TTestWidgetBasicOperations.TestEvent(aName : String);

begin
  TriggerEvent(aName);
  AssertEvent(aName,Fmy);
end;


procedure TTestWidgetBasicOperations.TestEventClick;
begin
  SetupElement;
  MyWidget.OnClick:=@MyTestEventHandler;
  TestEvent('click');
end;

{ ---------------------------------------------------------------------
  TBaseTestWidget
  ---------------------------------------------------------------------}
procedure TBaseTestWidget.SetUp;
begin
  inherited SetUp;
  FBaseWindow:=GetElement(BaseID);
end;

procedure TBaseTestWidget.TearDown;
begin
  if Assigned(FBaseWindow) then
    FBaseWindow.InnerHTML:='';
  FBaseWindow:=Nil;
  inherited TearDown;
end;

procedure TBaseTestWidget.AssertEvent(aName: String; aElement : TCustomWebWidget = Nil);
begin
  AssertEquals('Event handler called',1,FEventCount);
  AssertSame('Event handler sender',aElement,FEventSender);
  AssertEquals('Event name',aName,FEventName);
end;

procedure TBaseTestWidget.MyTestEventHandler(Sender: TObject; Event: TJSEvent);
begin
  Inc(FEventCount);
  FEventSender:=Sender;
  FEventName:=Event._type;
end;

function TBaseTestWidget.FindElement(aID: String): TJSHTMLElement;
begin
  Result:=TJSHTMLElement(Document.GetElementById(aID));
end;

function TBaseTestWidget.GetElement(aID: String): TJSHTMLElement;
begin
  Result:=FindElement(aID);
  if Result=Nil then
    Fail('No such element : '+aID);
end;

Function TBaseTestWidget.AssertTree(aParent: TJSHTmlElement; aTree: String) : TJSHTMLElement;

Var
  S,aTag,aID : String;
  P : integer;
  T : TJSHTMLElement;

begin
  P:=Pos('/',aTree);
  if P=0 then
    P:=Length(aTree)+1;
  aTag:=Copy(aTree,1,P-1);
  aTree:=Copy(aTree,P+1,Length(aTree)-P);
  P:=Pos('(',aTag);
  if P=0 then
    P:=Length(aTag)+1;
  aID:=Copy(aTag,P+1,Length(aTag)-P);
  aTag:=Copy(aTag,1,P-1);
  P:=Pos(')',aID);
  if P>0 then
    aID:=Copy(aID,1,P-1);
  // Writeln('Look for <',aTag,'>(',aID,')');
  T:=TJSHTMLElement(aParent.firstElementChild);
  While (T<>Nil) and Not (SameText(T.tagName,aTag) and ((aID='') or SameText(T.ID,aID))) do
    T:=TJSHTMLElement(T.nextElementSibling);
  if (T=Nil) then
    begin
    S:='Could not find <'+aTag+'>';
    if (aID<>'') then
      S:=S+'(ID='+aID+')';
    S:=S+'below element '+aParent.TagName;
    if (aParent.ID<>'') then
      S:=S+'(ID='+aParent.ID+')';
    Fail(S);
    end;
  // Writeln('Found: <',T.TagName,'>(iD=',T.ID+')');
  if aTree<>'' then
    Result:=AssertTree(T,aTree)
  else
    Result:=T;
end;

Function TBaseTestWidget.AssertTree(aTree: String) : TJSHTMLElement;
begin
  Result:=AssertTree(BaseWindow,aTree)
end;

{ ---------------------------------------------------------------------
  TTestWidgetBasicOperations
  ---------------------------------------------------------------------}


procedure TTestWidgetBasicOperations.TestEventOnAbort;

begin
  SetupElement;
  MyWidget.OnAbort:=@MyTestEventHandler;
  TestEvent('abort');
end;

procedure TTestWidgetBasicOperations.TestEventOnAnimationCancel;

begin
  SetupElement;
  MyWidget.OnAnimationCancel:=@MyTestEventHandler;
  TestEvent('animationcancel');
end;

procedure TTestWidgetBasicOperations.TestEventOnAnimationEnd;

begin
  SetupElement;
  MyWidget.OnAnimationEnd:=@MyTestEventHandler;
  TestEvent('animationend');
end;

procedure TTestWidgetBasicOperations.TestEventOnAnimationIteration;

begin
  SetupElement;
  MyWidget.OnAnimationIteration:=@MyTestEventHandler;
  TestEvent('animationiteration');
end;

procedure TTestWidgetBasicOperations.TestEventOnAnimationStart;

begin
  SetupElement;
  MyWidget.OnAnimationStart:=@MyTestEventHandler;
  TestEvent('animationstart');
end;

procedure TTestWidgetBasicOperations.TestEventOnAuxClick;

begin
  SetupElement;
  MyWidget.OnAuxClick:=@MyTestEventHandler;
  TestEvent('auxclick');
end;

procedure TTestWidgetBasicOperations.TestEventOnBlur;

begin
  SetupElement;
  MyWidget.OnBlur:=@MyTestEventHandler;
  TestEvent('blur');
end;

procedure TTestWidgetBasicOperations.TestEventOnCancel;

begin
  SetupElement;
  MyWidget.OnCancel:=@MyTestEventHandler;
  TestEvent('cancel');
end;

procedure TTestWidgetBasicOperations.TestEventOnCanPlay;

begin
  SetupElement;
  MyWidget.OnCanPlay:=@MyTestEventHandler;
  TestEvent('canplay');
end;

procedure TTestWidgetBasicOperations.TestEventOnCanPlayThrough;

begin
  SetupElement;
  MyWidget.OnCanPlayThrough:=@MyTestEventHandler;
  TestEvent('canplaythrough');
end;

procedure TTestWidgetBasicOperations.TestEventOnChange;

begin
  SetupElement;
  MyWidget.OnChange:=@MyTestEventHandler;
  TestEvent('change');
end;

procedure TTestWidgetBasicOperations.TestEventOnClick;

begin
  SetupElement;
  MyWidget.OnClick:=@MyTestEventHandler;
  TestEvent('click');
end;

procedure TTestWidgetBasicOperations.TestEventOnCompositionEnd;

begin
  SetupElement;
  MyWidget.OnCompositionEnd:=@MyTestEventHandler;
  TestEvent('compositionend');
end;

procedure TTestWidgetBasicOperations.TestEventOnCompositionStart;

begin
  SetupElement;
  MyWidget.OnCompositionStart:=@MyTestEventHandler;
  TestEvent('compositionstart');
end;

procedure TTestWidgetBasicOperations.TestEventOnCompositionUpdate;

begin
  SetupElement;
  MyWidget.OnCompositionUpdate:=@MyTestEventHandler;
  TestEvent('compositionupdate');
end;

procedure TTestWidgetBasicOperations.TestEventOnContextMenu;

begin
  SetupElement;
  MyWidget.OnContextMenu:=@MyTestEventHandler;
  TestEvent('contextmenu');
end;

procedure TTestWidgetBasicOperations.TestEventOnCopy;

begin
  SetupElement;
  MyWidget.OnCopy:=@MyTestEventHandler;
  TestEvent('copy');
end;

procedure TTestWidgetBasicOperations.TestEventOnCut;

begin
  SetupElement;
  MyWidget.OnCut:=@MyTestEventHandler;
  TestEvent('cut');
end;

procedure TTestWidgetBasicOperations.TestEventOnCueChange;

begin
  SetupElement;
  MyWidget.OnCueChange:=@MyTestEventHandler;
  TestEvent('cuechange');
end;

procedure TTestWidgetBasicOperations.TestEventOnDblClick;

begin
  SetupElement;
  MyWidget.OnDblClick:=@MyTestEventHandler;
  TestEvent('dblclick');
end;

procedure TTestWidgetBasicOperations.TestEventOnDurationChange;

begin
  SetupElement;
  MyWidget.OnDurationChange:=@MyTestEventHandler;
  TestEvent('durationchange');
end;

procedure TTestWidgetBasicOperations.TestEventOnEnded;

begin
  SetupElement;
  MyWidget.OnEnded :=@MyTestEventHandler;
  TestEvent('ended');
end;

procedure TTestWidgetBasicOperations.TestEventOnError;

begin
  SetupElement;
  MyWidget.OnError :=@MyTestEventHandler;
  TestEvent('error');
end;

procedure TTestWidgetBasicOperations.TestEventOnFocus;

begin
  SetupElement;
  MyWidget.OnFocus:=@MyTestEventHandler;
  TestEvent('focus');
end;

procedure TTestWidgetBasicOperations.TestEventOnFocusIn;

begin
  SetupElement;
  MyWidget.OnFocusIn :=@MyTestEventHandler;
  TestEvent('focusin');
end;

procedure TTestWidgetBasicOperations.TestEventOnFocusOut;

begin
  SetupElement;
  MyWidget.OnFocusOut :=@MyTestEventHandler;
  TestEvent('focusout');
end;

procedure TTestWidgetBasicOperations.TestEventOnGotPointerCapture;

begin
  SetupElement;
  MyWidget.OnGotPointerCapture:=@MyTestEventHandler;
  TestEvent('gotpointercapture');
end;

procedure TTestWidgetBasicOperations.TestEventOnInput;

begin
  SetupElement;
  MyWidget.OnInput:=@MyTestEventHandler;
  TestEvent('input');
end;

procedure TTestWidgetBasicOperations.TestEventOnInvalid;

begin
  SetupElement;
  MyWidget.OnInvalid:=@MyTestEventHandler;
  TestEvent('invalid');
end;

procedure TTestWidgetBasicOperations.TestEventOnKeyDown;

begin
  SetupElement;
  MyWidget.OnKeyDown:=@MyTestEventHandler;
  TestEvent('keydown');
end;

procedure TTestWidgetBasicOperations.TestEventOnKeyPress;

begin
  SetupElement;
  MyWidget.OnKeyPress:=@MyTestEventHandler;
  TestEvent('keypress');
end;

procedure TTestWidgetBasicOperations.TestEventOnKeyUp;

begin
  SetupElement;
  MyWidget.OnKeyUp:=@MyTestEventHandler;
  TestEvent('keyup');
end;

procedure TTestWidgetBasicOperations.TestEventOnLoad;

begin
  SetupElement;
  MyWidget.OnLoad:=@MyTestEventHandler;
  TestEvent('load');
end;

procedure TTestWidgetBasicOperations.TestEventOnLoadedData;

begin
  SetupElement;
  MyWidget.OnLoadedData:=@MyTestEventHandler;
  TestEvent('loadeddata');
end;

procedure TTestWidgetBasicOperations.TestEventOnLoadedMetaData;

begin
  SetupElement;
  MyWidget.OnLoadedMetaData:=@MyTestEventHandler;
  TestEvent('loadedmetadata');
end;

procedure TTestWidgetBasicOperations.TestEventOnLoadend;

begin
  SetupElement;
  MyWidget.OnLoadend:=@MyTestEventHandler;
  TestEvent('loadend');
end;

procedure TTestWidgetBasicOperations.TestEventOnLoadStart;

begin
  SetupElement;
  MyWidget.OnLoadStart:=@MyTestEventHandler;
  TestEvent('loadstart');
end;

procedure TTestWidgetBasicOperations.TestEventOnLostPointerCapture;

begin
  SetupElement;
  MyWidget.OnLostPointerCapture:=@MyTestEventHandler;
  TestEvent('lostpointercapture');
end;

procedure TTestWidgetBasicOperations.TestEventOnMouseDown;

begin
  SetupElement;
  MyWidget.OnMouseDown:=@MyTestEventHandler;
  TestEvent('mousedown');
end;

procedure TTestWidgetBasicOperations.TestEventOnMouseEnter;

begin
  SetupElement;
  MyWidget.OnMouseEnter:=@MyTestEventHandler;
  TestEvent('mouseenter');
end;

procedure TTestWidgetBasicOperations.TestEventOnMouseLeave;

begin
  SetupElement;
  MyWidget.OnMouseLeave:=@MyTestEventHandler;
  TestEvent('mouseleave');
end;

procedure TTestWidgetBasicOperations.TestEventOnMouseMove;

begin
  SetupElement;
  MyWidget.OnMouseMove:=@MyTestEventHandler;
  TestEvent('mousemove');
end;

procedure TTestWidgetBasicOperations.TestEventOnMouseOut;

begin
  SetupElement;
  MyWidget.OnMouseOut:=@MyTestEventHandler;
  TestEvent('mouseout');
end;

procedure TTestWidgetBasicOperations.TestEventOnMouseUp;

begin
  SetupElement;
  MyWidget.OnMouseUp:=@MyTestEventHandler;
  TestEvent('mouseup');
end;

procedure TTestWidgetBasicOperations.TestEventOnOverFlow;

begin
  SetupElement;
  MyWidget.OnOverFlow:=@MyTestEventHandler;
  TestEvent('overflow');
end;

procedure TTestWidgetBasicOperations.TestEventOnPaste;

begin
  SetupElement;
  MyWidget.OnPaste:=@MyTestEventHandler;
  TestEvent('paste');
end;

procedure TTestWidgetBasicOperations.TestEventOnPause;

begin
  SetupElement;
  MyWidget.OnPause:=@MyTestEventHandler;
  TestEvent('pause');
end;

procedure TTestWidgetBasicOperations.TestEventOnPlay;

begin
  SetupElement;
  MyWidget.OnPlay:=@MyTestEventHandler;
  TestEvent('play');
end;

procedure TTestWidgetBasicOperations.TestEventOnPointerCancel;

begin
  SetupElement;
  MyWidget.OnPointerCancel:=@MyTestEventHandler;
  TestEvent('pointercancel');
end;

procedure TTestWidgetBasicOperations.TestEventOnPointerDown;

begin
  SetupElement;
  MyWidget.OnPointerDown:=@MyTestEventHandler;
  TestEvent('pointerdown');
end;

procedure TTestWidgetBasicOperations.TestEventOnPointerEnter;

begin
  SetupElement;
  MyWidget.OnPointerEnter:=@MyTestEventHandler;
  TestEvent('pointerenter');
end;

procedure TTestWidgetBasicOperations.TestEventOnPointerLeave;

begin
  SetupElement;
  MyWidget.OnPointerLeave:=@MyTestEventHandler;
  TestEvent('pointerleave');
end;

procedure TTestWidgetBasicOperations.TestEventOnPointerMove;

begin
  SetupElement;
  MyWidget.OnPointerMove:=@MyTestEventHandler;
  TestEvent('pointermove');
end;

procedure TTestWidgetBasicOperations.TestEventOnPointerOut;

begin
  SetupElement;
  MyWidget.OnPointerOut:=@MyTestEventHandler;
  TestEvent('pointerout');
end;

procedure TTestWidgetBasicOperations.TestEventOnPointerOver;

begin
  SetupElement;
  MyWidget.OnPointerOver:=@MyTestEventHandler;
  TestEvent('pointerover');
end;

procedure TTestWidgetBasicOperations.TestEventOnPointerUp;

begin
  SetupElement;
  MyWidget.OnPointerUp:=@MyTestEventHandler;
  TestEvent('pointerup');
end;

procedure TTestWidgetBasicOperations.TestEventOnReset;

begin
  SetupElement;
  MyWidget.OnReset:=@MyTestEventHandler;
  TestEvent('reset');
end;

procedure TTestWidgetBasicOperations.TestEventOnResize;

begin
  SetupElement;
  MyWidget.OnResize:=@MyTestEventHandler;
  TestEvent('resize');
end;

procedure TTestWidgetBasicOperations.TestEventOnScroll;

begin
  SetupElement;
  MyWidget.OnScroll:=@MyTestEventHandler;
  TestEvent('scroll');
end;

procedure TTestWidgetBasicOperations.TestEventOnSelect;

begin
  SetupElement;
  MyWidget.OnSelect:=@MyTestEventHandler;
  TestEvent('select');
end;

procedure TTestWidgetBasicOperations.TestEventOnSubmit;

begin
  SetupElement;
  MyWidget.OnSubmit:=@MyTestEventHandler;
  TestEvent('submit');
end;

procedure TTestWidgetBasicOperations.TestEventOnTouchStart;

begin
  SetupElement;
  MyWidget.OnTouchStart:=@MyTestEventHandler;
  TestEvent('touchstart');
end;

procedure TTestWidgetBasicOperations.TestEventOnTransitionCancel;

begin
  SetupElement;
  MyWidget.OnTransitionCancel:=@MyTestEventHandler;
  TestEvent('transitioncancel');
end;

procedure TTestWidgetBasicOperations.TestEventOnTransitionEnd;

begin
  SetupElement;
  MyWidget.OnTransitionEnd:=@MyTestEventHandler;
  TestEvent('transitionend');
end;

procedure TTestWidgetBasicOperations.TestEventOnTransitionRun;

begin
  SetupElement;
  MyWidget.OnTransitionRun:=@MyTestEventHandler;
  TestEvent('transitionrun');
end;

procedure TTestWidgetBasicOperations.TestEventOnTransitionStart;

begin
  SetupElement;
  MyWidget.OnTransitionStart:=@MyTestEventHandler;
  TestEvent('transitionstart');
end;

procedure TTestWidgetBasicOperations.TestEventOnWheel;

begin
  SetupElement;
  MyWidget.OnWheel:=@MyTestEventHandler;
  TestEvent('wheel');
end;

initialization
//  RegisterTests([TTestWidgetBasicOperations,TTestWebWidgetStyles,TTestWebWidgetReferences]);
end.

