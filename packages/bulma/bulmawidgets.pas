unit bulmawidgets;

{$mode objfpc}
{$h+}
{$modeswitch externalclass}

Interface

uses
  Js,
  web,
  SysUtils,
  Classes,
  webwidget,
  rtl.TemplateLoader;

Const
  DefaultClose = 'DefaultCloseClicks';

Type

  { TBulmaModal }
  TOnModalHideEvent = Procedure (Sender : TObject; CloseEl : TJSHTMLElement; Values : TStrings) of object;

  TModalItemKind = (mikValue,mikClose);

  { TModalReferenceItem }

  TModalReferenceItem = Class(TReferenceItem)
  private
    FInitialValue: String;
    FKind: TModalItemKind;
  Protected
    Function GetValue: String;
    Procedure SetValue(aValue: String);
  Public
    Procedure Assign(Source : TPersistent); override;
    Property Value : String Read GetValue Write SetValue;
  Published
    Property Kind : TModalItemKind Read FKind Write FKind;
    Property InitialValue : String Read FInitialValue Write FInitialValue;
  end;

  { TModalReferences }

  TModalReferences = Class(TWebWidgetReferences)
  private
    function GetMR(aIndex : Integer): TModalReferenceItem;
    procedure SetMR(aIndex : Integer; AValue: TModalReferenceItem);
  Public
    Function Add(Const aName : String; aSelector : String; aKind : TModalItemKind) : TModalReferenceItem; overload;
    Property ModalRefs[aIndex : Integer] : TModalReferenceItem Read GetMR Write SetMR; default;
  end;

  TBulmaModal = Class(TCustomTemplateWidget)
  private
    FHideEl : TJSHTMLElement;
    FBackDrop: Boolean;
    FFocus: Boolean;
    FKeyBoard: Boolean;
    FOnHide: TOnModalHideEvent;
    FShowOnRender: Boolean;
    FTemplate: String;
    FTemplateLoader: TCustomTemplateLoader;
    FTemplateName: String;
    FRemoveOnHide: Boolean;
    FOKButtonName: String;
    FCancelButtonName: String;
    FCloseButtonName: String;
    FOnRender: TNotifyEvent;
    FOnShow: TNotifyEvent;
    FShowing : Boolean;
    procedure DoShow;
    function GetModalReferences: TModalReferences;
    procedure HideClick(Event: TJSEvent);
    procedure SetModalReferences(AValue: TModalReferences);
    procedure CheckRemove;
    procedure SetTemplateLoader(AValue: TCustomTemplateLoader);
  protected
    function GetTemplateManager: TCustomTemplateLoader; virtual;
    Function CreateReferences: TWebWidgetReferences; override;
    Function ModalHide(Event : TJSEvent) : Boolean;
    Function DoRenderHTML(aParent, aElement: TJSHTMLElement): TJSHTMLElement; override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    Function GetTemplateHTML: String; override;
    Procedure RefreshReferences; override;
    procedure Loaded; override;
  Public
    Constructor Create(aOwner : TComponent); override;
    Destructor Destroy; override;
    procedure GetValues(aList: TStrings);
    Procedure Show;
    Procedure Hide;
    Function HTMLTag : String; override;
    Property Showing : Boolean Read FShowing;
    // Id of the modal toplevel element. If not set, the first child is used.
  Published
    Property ParentID;
    Property Styles;
    Property StyleRefresh;
    Property ElementID;
    Property References : TModalReferences Read GetModalReferences Write SetModalReferences;
    Property ShowOnRender: Boolean Read FShowOnRender Write FShowOnrender default False;
    Property RemoveOnHide : Boolean Read FRemoveOnHide Write FRemoveOnHide default True;
    Property BackDrop : Boolean Read FBackDrop Write FBackDrop default false;
    Property KeyBoard : Boolean Read FKeyBoard Write FKeyBoard default false;
    Property Focus : Boolean Read FFocus Write FFocus default false;
    Property OKButtonName : String Read FOKButtonName write FOKButtonName;
    Property CancelButtonName : String Read FCancelButtonName Write FCancelButtonName;
    Property CloseButtonName : String Read FCloseButtonName Write FCloseButtonName;
    // Template gets precedence over templatename;
    Property Template : String Read FTemplate Write FTemplate;
    Property TemplateName : String Read FTemplateName Write FTemplateName;
    Property TemplateLoader : TCustomTemplateLoader Read FTemplateLoader Write SetTemplateLoader;
    Property OnHide : TOnModalHideEvent Read FOnHide Write FOnHide;
    Property OnRender : TNotifyEvent Read FOnRender Write FOnRender;
    Property OnShow : TNotifyEvent Read FOnShow Write FOnShow;
  end;

  { TSimpleToastWidget }
  TContextual = (cNone,
                 cPrimary,cLink,cInfo,cSuccess,cWarning,cDanger,
                 cWhite,cLight,cDark,cBlack,cText,cGhost);
  TToastPosition = (tpDefault,
               tpTopRight,tpTopLeft,tpTopCenter,
               tpBottomRight,tpBottomLeft,tpBottomCenter,
               tpCenter);

  // Single toast message

  { TBaseBulmaToastWidget }

  TBaseBulmaToastWidget = Class(TCustomWebWidget)
  private
    FAnimate: Boolean;
    FAutoHide: Boolean;
    FBody: String;
    FBoolean: Boolean;
    FContextual: TContextual;
    FHeader: String;
    FHeaderImage: String;
    FHideDelay: Integer;
    FMinWidth: Integer;
    FSingle: Boolean;
    FPosition: TToastPosition;
    procedure SetAnimate(AValue: Boolean);
    procedure SetAutoHide(AValue: Boolean);
    procedure SetBody(AValue: String);
    procedure SetBoolean(AValue: Boolean);
    procedure SetContextual(AValue: TContextual);
    procedure SetHeader(AValue: String);
    procedure SetHeaderImage(AValue: String);
    procedure SetHideDelay(AValue: Integer);
    procedure SetMinWidth(AValue: Integer);
  Protected
    FElement : TJSHTMLElement;
    function BodyHTML: String; virtual;
    function HeaderHTML: String; virtual;
  Public
    Class Function DefaultParentElement : TJSHTMLElement; override;
    Constructor Create(aOwner : TComponent); override;
    Function DoRenderHTML(aParent, aElement: TJSHTMLElement): TJSHTMLElement; override;
    procedure Hide;
    function HtmlTag : String; override;
  Protected
    Property Header : String Read FHeader Write SetHeader;
    Property Body : String Read FBody Write SetBody;
    Property HeaderImage : String Read FHeaderImage Write SetHeaderImage;
    Property CloseButton : Boolean Read FBoolean Write SetBoolean;
    Property Contextual : TContextual Read FContextual write SetContextual;
    Property HideDelay : Integer Read FHideDelay Write SetHideDelay default 2000;
    Property AutoHide : Boolean Read FAutoHide Write SetAutoHide default True;
    Property Animate : Boolean Read FAnimate Write SetAnimate default False;
    Property MinWidth : Integer Read FMinWidth Write SetMinWidth default 200;
    Property Single : Boolean Read FSingle Write FSingle;
    Property Position : TToastPosition Read FPosition Write FPosition default tpTopRight;
  end;
  
  TBulmaToastWidget = class(TBaseBulmaToastWidget)
  Published
    Property Styles;
    Property StyleRefresh;
    Property References;
    Property ParentID;
    Property ElementID;
    Property Header;
    Property Body;
    Property HeaderImage;
    Property CloseButton;
    Property Contextual;
    Property HideDelay;
    Property AutoHide;
    Property Animate;
    Property MinWidth;
    Property Single;
    Property Position;
  end;

  // Encapsulates the global tag where the toasts are shown.

  { TToastManager }

  TToastManager = Class(TComponent)
  Private
    class var
      _instance : TToastManager;
      _ToastID : NativeInt;
  Private
    FAnimate: Boolean;
    FAutoHide: Boolean;
    FHideDelay: Integer;
    FMinheight: Integer;
    FMinWidth: Integer;
    FMultiToast: Boolean;
    FToastIcon: String;
    FParentID: String;
    FToastPosition: TToastPosition;
    procedure SetMinHeight(AValue: Integer);
    procedure SetMultiToast(AValue: Boolean);
    procedure SetParentID(const Value: String);
    class function CreateElement(aTag : String; aID: String = ''): TJSHTMLElement; static;
  Public
    Constructor Create(aOwner : TComponent); override;
    class function Instance : TToastManager;
    Class Function getToastID : String;
    Procedure clear;
    function ShowToast(const aHeader, aBody: String; aContext: TContextual=cNone; Closable: Boolean=True; aDelay : Integer = 0): TBaseBulmaToastWidget;
  Published
    Property ParentID : String Read FParentID Write SetParentID;
    Property MultiToast : Boolean Read FMultiToast Write SetMultiToast;
    Property MinHeight : Integer Read FMinheight Write SetMinHeight default 250;
    Property ToastHideDelay : Integer Read FHideDelay Write FHideDelay default 2000;
    Property ToastAutoHide : Boolean Read FAutoHide Write FAutoHide default True;
    Property ToastAnimate : Boolean Read FAnimate Write FAnimate default False;
    Property ToastMinWidth : Integer Read FMinWidth Write FMinWidth default 200;
    Property ToastIcon : String Read FToastIcon Write FToastIcon;
    Property ToastPosition : TToastPosition Read FToastPosition Write FToastPosition;
  end;

Const
  ContextualNames : Array[TContextual] of string
                  = ('','primary','link','info','success','warning','danger',
                     'white','light','dark','black','text','ghost');
  ToastPositionNames : Array[TToastPosition] of string =
               ('top-right','top-right','top-left','top-center',
                'bottom-right','bottom-left','bottom-center',
                'center');

Function Toasts : TToastManager;

// Bulma-toast
Type
  TBulmaToast = Class {$IFDEF PAS2JS} external name 'bulmaToast' {$ENDIF} (TJSObject)
  Public
    class Procedure toast(aObject : TJSObject);
    class Procedure setDoc(aEl : TJSObject);
    class Procedure setDefaults(aObject : TJSObject);
    class Procedure resetDefaults;
  end;



Implementation

Resourcestring
  SErrNoTemplateSet = '%s: No template set';
//  SErrCannotUnrenderFixedElementID = 'Cannot unrender when ElementID (%s) is set';


{ TModalReferenceItem }

procedure TModalReferenceItem.Assign(Source: TPersistent);

Var
  MRI : TModalReferenceItem absolute Source;

begin
  if Source is TModalReferenceItem then
    begin
    Self.Kind:=MRI.Kind;
    Self.InitialValue:=MRI.InitialValue;
    end;
  inherited Assign(Source);
end;

function TModalReferenceItem.GetValue: String;
begin
  if (Kind=mikValue) and (Element<>Nil) then
    Result:=TJSHTMLInputElement(Element).value;
end;

procedure TModalReferenceItem.SetValue(aValue: String);
begin
  if (Kind=mikValue) and (Element<>Nil) then
    TJSHTMLInputElement(Element).value:=aValue;
end;

{ TModalReferences }

function TModalReferences.GetMR(aIndex : Integer): TModalReferenceItem;
begin
  Result:=TModalReferenceItem(Items[aIndex])
end;

procedure TModalReferences.SetMR(aIndex : Integer; AValue: TModalReferenceItem);
begin
  Items[aIndex]:=aValue;
end;

function TModalReferences.Add(const aName: String; aSelector: String; aKind: TModalItemKind): TModalReferenceItem;
begin
  Result:=TModalReferenceItem(Inherited Add(aName,aselector));
  Result.Kind:=aKind;
end;

{ TBulmaModal }


procedure TBulmaModal.Hide;

begin
  if Assigned(Element) then
    Element.ClassList.remove('is-active');
  FShowing:=False;
end;

procedure TBulmaModal.HideClick(Event: TJSEvent);

begin
  // Writeln('In hide click');
  FHideEl:=TJSHtmlElement(Event.currentTarget);
  ModalHide(Event);
  Hide;
end;

procedure TBulmaModal.SetModalReferences(AValue: TModalReferences);
begin
  References.Assign(aValue);
end;

procedure TBulmaModal.Loaded;
begin
  inherited;
end;

function TBulmaModal.HTMLTag: String;
begin
  Result:='div'
end;

procedure TBulmaModal.GetValues(aList: TStrings);

Var
  I : integer;
  Itm : TModalReferenceItem;

begin
  For I:=0 to references.Count-1 do
    begin
    Itm:=references[i];
    if (itm.Kind=mikValue) and assigned(itm.Element) then
      aList.Values[Itm.Name]:=Itm.Value
    end;
end;

procedure TBulmaModal.CheckRemove;

begin
  if FRemoveOnHide then
    UnRender;
end;

procedure TBulmaModal.SetTemplateLoader(AValue: TCustomTemplateLoader);
begin
  if FTemplateLoader=AValue then Exit;
  if assigned(FTemplateLoader) then
    FTemplateLoader.RemoveFreeNotification(Self);
  FTemplateLoader:=AValue;
  if assigned(FTemplateLoader) then
    FTemplateLoader.FreeNotification(Self);
end;

function TBulmaModal.CreateReferences: TWebWidgetReferences;
begin
  Result:=TModalReferences.Create(Self,TModalReferenceItem);
end;

function TBulmaModal.ModalHide(Event: TJSEvent): Boolean;

Var
  L : Tstrings;

begin
  // Writeln('In bootstraphide callback ', assigned(onhide));
  // Note that this can still be called after the bootstrap modal is destroyed.
  Result:=False;
  If Assigned(OnHide) then
    begin
    L:=TStringList.Create;
    GetValues(L);
    if L.Count=0 then
      FreeAndNil(L);
    Try
      OnHide(Self,FHideEl,L);
    finally
      L.Free;
    end;
    end;
  CheckRemove;
  // Sometimes it gets stuck so we force remove it because it messes up the scrollbars...

end;

destructor TBulmaModal.Destroy;
begin
  CheckRemove;
  inherited;
end;

function TBulmaModal.DoRenderHTML(aParent, aElement: TJSHTMLElement): TJSHTMLElement;

begin
  FHideEl:=Nil;
  Result:=inherited DoRenderHTML(aParent, aElement);
end;

procedure TBulmaModal.Notification(AComponent: TComponent; Operation: TOperation
  );
begin
  inherited Notification(AComponent, Operation);
  if (Operation=opRemove) and (aComponent=TemplateLoader) then
    FTemplateLoader:=Nil;
end;


function TBulmaModal.GetTemplateHTML: String;
begin
  Result:=FTemplate;
  if Result='' then
    if Assigned(TemplateLoader) then
      Result:=TemplateLoader.Templates[TemplateName]
    else
      Result:=globalTemplates.Templates[TemplateName];
end;

procedure TBulmaModal.RefreshReferences;

Var
  I : Integer;
  E : TJSHTMLElement;
  MR : TModalReferenceItem;
  NList : TJSNodeList;
  N : TJSNode;
begin
  inherited RefreshReferences;
  // Specials from Bulma itself
  NList:=Element.querySelectorAll('.modal-background, .modal-close, .modal-card-head .delete');
  for I:=0 to Nlist.length-1 do
    TJSHTMLElement(NList.Nodes[i]).addEventListener('click',@HideClick);
  E:=References.FindElementByName('OK');
  if (E<>Nil) then
    E.addEventListener('click',@HideClick);
  for I:=0 to References.Count-1 do
    begin
    MR:=References[i];
    if (MR.Kind=mikClose) then
      begin
      if MR.Exists then
        if Not MR.IsArray then
          MR.Element.AddEventListener('click',@HideClick)
        else
          For E in MR.Elements do
            E.AddEventListener('click',@HideClick);
        end
    else if (MR.Kind=mikValue) then
      begin
      if (MR.element<>Nil) and (MR.InitialValue<>'') then
        MR.Value:=MR.InitialValue;
      end;
    end;
end;

constructor TBulmaModal.Create(aOwner: TComponent);
begin
  inherited;
  FRemoveOnHide:=True;
end;

function TBulmaModal.GetModalReferences: TModalReferences;
begin
  Result:=TModalReferences(Inherited References);
end;


procedure TBulmaModal.DoShow;

begin
  FHideEl:=Nil;
  if not IsRendered then
    Refresh;
  Element.ClassList.Add('is-active');
  FShowing:=True;
  if Assigned(FOnShow) then
    FOnShow(Self);
end;

Function TBulmaModal.GetTemplateManager : TCustomTemplateLoader;

begin
  if Assigned(TemplateLoader) then
    Result:=TemplateLoader
  else
    Result:=GlobalTemplates;
end;

procedure TBulmaModal.Show;

  procedure DoShowTemplate(Sender: TObject; const aTemplate: String);
  begin
    Template:=GetTemplateManager.Templates[aTemplate];
    DoShow;
  end;

begin
  if (Template<>'') or (ElementID<>'') Then
    DoShow
  else if TemplateName<>'' then
    GetTemplateManager.IfTemplate(TemplateName,@DoShowTemplate)
  else
    Raise EWidgets.CreateFmt(SErrNoTemplateSet,[Name]);
end;

{ TBulmaToast }

function Toasts: TToastManager;
begin
  Result:=TToastManager.Instance;
end;

{ TToastManager }

class function TToastManager.Instance: TToastManager;
begin
  if _instance=Nil then
   _instance:=TToastManager.Create(Nil);
  Result:=_instance;
end;


procedure TToastManager.SetMinHeight(AValue: Integer);
begin
  if FMinheight=AValue then Exit;
  FMinheight:=AValue;
end;

procedure TToastManager.SetMultiToast(AValue: Boolean);
begin
  if FMultiToast=AValue then Exit;
  FMultiToast:=AValue;
end;

procedure TToastManager.SetParentID(const Value: String);
begin
  FParentID := Value;
  if ParentID<>'' then
    TBulmaToast.SetDoc(document.getElementById(ParentID));
end;



class function TToastManager.getToastID: String;
begin
  Inc(_ToastID);
  Result:='toast-'+intToStr(_ToastID);
end;

Class Function TToastManager.CreateElement(aTag : String; aID : String = '') : TJSHTMLElement;

begin
  Result:=TJSHTMLElement(document.CreateElement(aTag));
  if aID='' then
    aID:=GetToastID;
  Result.ID:=aID;
end;


procedure TToastManager.clear;
begin
  if (ParentID<>'') then
    document.getElementById(ParentID).innerHTML:='';
end;

constructor TToastManager.Create(aOwner: TComponent);
begin
  inherited Create(aOwner);
  FMinHeight:=250;
  FMinWidth:=200;
  FMultiToast:=True;
  FHideDelay:=2000;
  FAutoHide:=True;
  FAnimate:=False;
end;

function TToastManager.ShowToast(const aHeader, aBody: String; aContext : TContextual = cNone; Closable: Boolean = True; aDelay : Integer = 0): TBaseBulmaToastWidget;

Var
  MsgDelay : Integer;
  aHide : Boolean;

begin
  MsgDelay:=aDelay;
  if MsgDelay=0 then
    begin
    MsgDelay:=ToastHideDelay;
    aHide:=ToastAutoHide;
    end
  else if MsgDelay=-1 then
    begin
    MsgDelay:=0;
    aHide:=False;
    end;
  Result:=TBaseBulmaToastWidget.Create(Self) ;
  With Result do
    begin
    Header:=aHeader;
    Body:=aBody;
    HeaderImage:=ToastIcon;
    CloseButton:=Closable;
    Contextual:=aContext;
    AutoHide:=aHide;
    HideDelay:=MsgDelay;
    Animate:=ToastAnimate;
    MinWidth:=ToastMinWidth;
    Single:=Not Multitoast;
    if ToastPosition<>tpDefault then
      Position:=ToastPosition;
    Refresh;
    end;
end;

{ TBaseBulmaToastWidget }


procedure TBaseBulmaToastWidget.SetBody(AValue: String);
begin
  if FBody=AValue then Exit;
  FBody:=AValue;
  if isRendered then Refresh;
end;

procedure TBaseBulmaToastWidget.SetAnimate(AValue: Boolean);
begin
  if FAnimate=AValue then Exit;
  FAnimate:=AValue;
  if isRendered then Refresh;
end;

procedure TBaseBulmaToastWidget.SetAutoHide(AValue: Boolean);
begin
  if FAutoHide=AValue then Exit;
  FAutoHide:=AValue;
  if isRendered then Refresh;
end;

procedure TBaseBulmaToastWidget.SetBoolean(AValue: Boolean);
begin
  if FBoolean=AValue then Exit;
  FBoolean:=AValue;
  if isRendered then Refresh;
end;

procedure TBaseBulmaToastWidget.SetContextual(AValue: TContextual);
begin
  if FContextual=AValue then Exit;
  FContextual:=AValue;
  if isRendered then Refresh;
end;

procedure TBaseBulmaToastWidget.SetHeader(AValue: String);
begin
  if FHeader=AValue then Exit;
  FHeader:=AValue;
  if isRendered then Refresh;
end;

procedure TBaseBulmaToastWidget.SetHeaderImage(AValue: String);
begin
  if FHeaderImage=AValue then Exit;
  FHeaderImage:=AValue;
  if isRendered then Refresh;
end;

procedure TBaseBulmaToastWidget.SetHideDelay(AValue: Integer);
begin
  if FHideDelay=AValue then Exit;
  FHideDelay:=AValue;
  if isRendered then Refresh;
end;

procedure TBaseBulmaToastWidget.SetMinWidth(AValue: Integer);
begin
  if FMinWidth=AValue then Exit;
  FMinWidth:=AValue;
  if isRendered then Refresh;
end;


function TBaseBulmaToastWidget.HeaderHTML: String;

Var
  S : String;

begin
  Result:='';
  if (Header='') and (HeaderImage='') then
    exit;
  S:=ContextualNames[Contextual];
  if S<>'' then
    S:=' is-'+S;
  Result:='<p class="title is-6 is-small">';
  if HeaderImage<>'' then
    Result:=Result+'<img src="'+HeaderImage+'" class="icon is-small">';
  Result:=Result+Header;
  Result:=Result+'</p>'
end;

class function TBaseBulmaToastWidget.DefaultParentElement : TJSHTMLElement;
begin
  Result:=Nil;
  if Toasts.ParentID<>'' then
    Result:=TJSHTMLElement(Document.getElementById(Toasts.ParentID))
  else
    Result:=TJSHTMLElement(Document.Body);
end;


function TBaseBulmaToastWidget.BodyHTML: String;

begin
  Result:='<div class="message-body is-light">';
  Result:=Result+HeaderHTML;
  Result:=Result+'<p>'+Body+'</p>';
  Result:=Result+'</div>';
end;

function TBaseBulmaToastWidget.DoRenderHTML(aParent, aElement: TJSHTMLElement): TJSHTMLElement;

Var
  S : String;
  Opts : TJSObject;
  aDelay : NativeInt;
Begin
  S:=ContextualNames[Contextual];
  if S<>'' then
    S:='is-'+S;
  Result:=aElement;
  Result.ClassName:='message is-small is-light '+S;
  Result.Style.CSSText:='min-width: '+IntToStr(MinWidth)+'px;';
  Result.InnerHTML:={HeaderHTML+}BodyHTML;
  aDelay:=FHideDelay;
  if Not AutoHide then // we let it display for 1 day
    aDelay:=24*3600*1000;
  Opts:=New([
    'message', aElement,
      'type', S,
      'single', single,
      'animate',FAnimate,
      'dismissible', CloseButton ,
      'position',ToastPositionNames[Position],
      'duration',aDelay,
      'extraClasses','is-light'
    ]);
  TBulmaToast.toast(Opts);
end;

constructor TBaseBulmaToastWidget.Create(aOwner: TComponent);
begin
  inherited Create(aOwner);
  FMinWidth:=200;
  FAutoHide:=True;
  FHideDelay:=2000;
  FPosition:=tpTopRight;
end;

procedure TBaseBulmaToastWidget.Hide;
begin

end;

function TBaseBulmaToastWidget.HtmlTag: String;
begin
  Result:='article';
end;



end.
