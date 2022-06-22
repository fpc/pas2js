unit htmlfragment;

{$mode ObjFPC}
{$INTERFACES CORBA}

interface

uses
  {$ifdef pas2js} web,{$endif} Classes, SysUtils, Rtl.HTMLUtils;

Type
  TCustomHTMLFragment = class;

  { TFragmentHandler }
  IFragmentClient = Rtl.HTMLUtils.IHTMLClient;


  TFragmentHandler = Class(TObject)
  Private
    class var _instance : TFragmentHandler;
  Private
    Type

      { TElementFragment }

      TElementFragment = Class
      private
        FElement: TJSHTMLElement;
        FFragment: TCustomHTMLFragment;
      public
        Constructor Create(aElement : TJSHTMLElement; aFragment : TCustomHTMLFragment);
        property Element : TJSHTMLElement Read FElement;
        property Fragment : TCustomHTMLFragment Read FFragment;
      end;
  Private
    FFragments : TFPList;
  Protected
    Function IndexOfElement(aElement : TJSHTMLElement) : Integer;
    Function FindElementFragment(aElement : TJSHTMLElement) : TElementFragment;
  Public
    Constructor Create; virtual;
    Destructor Destroy; override;
    Function RemoveParentFragment(aParent : TJSHTMLElement; aFragment : TCustomHTMLFragment) : Boolean;
    Function ClearParentFragment(aParent : TJSHTMLElement; aFragment : TCustomHTMLFragment) : Boolean;
    Procedure SetParentFragment(aParent : TJSHTMLElement; aFragment : TCustomHTMLFragment);
    Class Function Instance : TFragmentHandler;
  end;

  { TCustomHTMLFragment }
  TAllowUnrenderEvent = Procedure (Sender : TObject; var allowClose : Boolean);
  TCustomHTMLFragment = class(TDatamodule)
  private
    // FAppendToParent: Boolean;
    FHTMLFile: String;
    FOnAllowUnrender: TAllowUnrenderEvent;
    FOnHTMLLoaded: TNotifyEvent;
    FOnRendered: TNotifyEvent;
    FOnRenderFail: TNotifyEvent;
    FOnUnRendered: TNotifyEvent;
    FParentID: String;
    FTemplateName: String;
    FTemplate : String;
    FParent : TJSHTMLElement;
    FUseProjectHTML: Boolean;
    function GetIsRendered: Boolean;
    procedure SetHTMLFile(AValue: String);
    procedure SetParentID(AValue: String);
    procedure SetTemplateName(AValue: String);
    procedure SetUseProjectHTML(AValue: Boolean);
  Protected
    procedure CheckRendered(const aOperation: string);
    procedure DoOnHTMLLoaded; virtual;
    procedure DoOnRender; virtual;
    procedure DoOnUnRender; virtual;
    procedure DoRender; virtual;
    procedure DoUnRender; virtual;
    function AllowUnrender : Boolean;
    Property UseProjectHTMLFile : Boolean Read FUseProjectHTML Write SetUseProjectHTML;
    Property ParentID : String Read FParentID Write SetParentID;
    Property TemplateName : String Read FTemplateName Write SetTemplateName;
    Property HTMLFileName : String Read FHTMLFile Write SetHTMLFile;
    Property OnRendered : TNotifyEvent Read FOnRendered Write FOnrendered;
    Property OnUnRendered : TNotifyEvent Read FOnUnRendered Write FOnUnrendered;
    Property OnRenderFail : TNotifyEvent Read FOnRenderFail Write FOnrenderFail;
    Property OnHTMLLoaded : TNotifyEvent Read FOnHTMLLoaded Write FOnHTMLLoaded;
//    Property AppendToParent : Boolean Read FAppendToParent Write FAppendToParent;
    Property OnAllowUnrender : TAllowUnrenderEvent Read FOnAllowUnrender Write FOnAllowUnrender;
  Public
    Destructor destroy; override;
    Procedure Render; virtual;
    Procedure UnRender;
    Procedure Show;
    Procedure Hide;
    Property IsRendered : Boolean Read GetIsRendered;
  end;
  TCustomHTMLFragmentClass = class of TCustomHTMLFragment;

  THTMLFragment = class(TCustomHTMLFragment)
  Published
    Property UseProjectHTMLFile;
    Property ParentID;
    Property TemplateName;
    Property HTMLFileName;
    Property OnRendered;
    Property OnHTMLLoaded;
//    Property AppendToParent;
    Property OnAllowUnrender;
    Property OnUnrendered;

  end;
  THTMLFragmentClass = class of THTMLFragment;

implementation

uses rtl.TemplateLoader;

Resourcestring
  SErrCannotPerformOperation = 'Cannot perform operation "%s" when HTML is rendered';

{ TFragmentHandler }

function TFragmentHandler.IndexOfElement(aElement: TJSHTMLElement): Integer;
begin
  Result:=FFragments.Count-1;
  While (Result>=0) and (TElementFragment(FFragments[Result]).Element<>aElement) do
    Dec(Result);
end;

function TFragmentHandler.FindElementFragment(aElement: TJSHTMLElement
  ): TElementFragment;

Var
  Idx : Integer;

begin
  Idx:=IndexOfElement(aElement);
  if (Idx>=0) then
    Result:=TElementFragment(FFragments[Idx])
  else
    Result:=Nil;
end;

constructor TFragmentHandler.Create;
begin
  FFragments:=TFPList.Create;;
end;

destructor TFragmentHandler.Destroy;
begin
  FreeAndNil(FFragments);
  inherited Destroy;
end;

function TFragmentHandler.RemoveParentFragment(aParent: TJSHTMLElement;
  aFragment: TCustomHTMLFragment): Boolean;
Var
  Idx : Integer;
  ef : TElementFragment;

begin
  Idx:=IndexOfElement(aParent);
  if (Idx<0) then
    Result:=True
  else
    begin
    ef:=TElementFragment(FFragments[Idx]);
    Result:=ef.Fragment=aFragment;
    if Result then
      begin
      FFragments.Delete(Idx);
      ef.Free;
      end;
    end;
end;

function TFragmentHandler.ClearParentFragment(aParent: TJSHTMLElement; aFragment: TCustomHTMLFragment): Boolean;

Var
  Idx : Integer;
  ef : TElementFragment;
  F : TCustomHTMLFragment;

begin
  Idx:=IndexOfElement(aParent);
  if (Idx<0) then
    Result:=True
  else
    begin
    ef:=TElementFragment(FFragments[Idx]);
    if Ef.FFragment=aFragment then
      Result:=True
    else
      Result:=Ef.FFragment.AllowUnrender;
    if Result then
      begin
      F:=ef.Fragment;
      // This will remove the fragment from the list.
      ef.Fragment.DoUnrender;
      F.Free;
      end;
    end;
end;

procedure TFragmentHandler.SetParentFragment(aParent: TJSHTMLElement; aFragment: TCustomHTMLFragment);

Var
  ef : TElementFragment;

begin
  ef:=FindElementFragment(aParent);
  if Assigned(Ef) then
    Ef.FFragment:=aFragment
  else
    FFragments.Add(TElementFragment.Create(aParent,aFragment));
end;

class function TFragmentHandler.Instance: TFragmentHandler;
begin
  if (_Instance=Nil) then
    _Instance:=TFragmentHandler.Create;
  Result:=_Instance;
end;

{ TFragmentHandler.TElementFragment }

constructor TFragmentHandler.TElementFragment.Create(aElement: TJSHTMLElement;
  aFragment: TCustomHTMLFragment);
begin
  FElement:=aElement;
  FFragment:=aFragment;
end;

{ TCustomHTMLFragment }

procedure TCustomHTMLFragment.SetHTMLFile(AValue: String);
begin
  if (FHTMLFile=AValue) then Exit;
  FHTMLFile:=AValue;
end;

function TCustomHTMLFragment.GetIsRendered: Boolean;
begin
  Result:=Assigned(FParent);
end;

procedure TCustomHTMLFragment.SetParentID(AValue: String);
begin
  if FParentID=AValue then Exit;
  FParentID:=AValue;
end;

procedure TCustomHTMLFragment.SetTemplateName(AValue: String);
begin
  if FTemplateName=AValue then Exit;
  FTemplateName:=AValue;
end;

procedure TCustomHTMLFragment.SetUseProjectHTML(AValue: Boolean);
begin
  if FUseProjectHTML=AValue then Exit;
  FUseProjectHTML:=AValue;
  if FUseProjectHTML then
    begin
    TemplateName:='';
    HTMLFileName:='';
    end;
end;

procedure TCustomHTMLFragment.CheckRendered(const aOperation: string);
begin
  If IsRendered then
    Raise ENotSupportedException.CreateFmt(SErrCannotPerformOperation,[aOperation]);
end;

procedure TCustomHTMLFragment.DoOnRender;

Var
  I : Integer;
  aClient : IFragmentClient;

begin
  if Assigned(FOnRendered) then
    FOnRendered(Self);
  For I:=0 to ComponentCount-1 do
    if Supports(Components[i],IFRagmentClient,aClient) then
      aClient.HTMLRendered;
end;

procedure TCustomHTMLFragment.DoOnUnRender;
begin
  If Assigned(FOnUnRendered) then
    FOnUnRendered(Self);
end;

procedure TCustomHTMLFragment.DoRender;
begin
  Fparent.InnerHtml:=FTemplate;
end;

procedure TCustomHTMLFragment.DoUnRender;
begin
  if Assigned(FParent) then
    begin
    FParent.innerHTML:='';
    TFragmentHandler.Instance.RemoveParentFragment(FParent,Self);
    FParent:=Nil;
    end;
end;

function TCustomHTMLFragment.AllowUnrender: Boolean;
begin
  Result:=True;
  if Assigned(FOnAllowUnrender) then
    FOnAllowUnrender(Self,Result);
end;

destructor TCustomHTMLFragment.destroy;
begin
  if IsRendered then
    UnRender;
  inherited destroy;
end;

procedure TCustomHTMLFragment.Render;
begin
  if ParentID<>'' then
    FParent:=TJSHTMLElement(Document.getElementById(ParentID));
  if FParent=Nil then
    FParent:=TJSHTMLElement(Document.body);
  if TFragmentHandler.Instance.ClearParentFragment(FParent,Self) then
    begin
    if not UseProjectHTMLFile then
      DoRender;
    TFragmentHandler.Instance.SetParentFragment(FParent,Self);
    DoOnRender;
    end
  else if Assigned(FOnRenderFail) then
    FOnrenderFail(Self);
end;

procedure TCustomHTMLFragment.UnRender;
begin
  DoUnRender;
  DoOnUnRender;
end;

procedure TCustomHTMLFragment.DoOnHTMLLoaded;

Var
  I : Integer;
  aClient : IFragmentClient;

begin
  if Assigned(FOnHTMLLoaded) then
    FOnHTMLLoaded(Self);
  For I:=0 to ComponentCount-1 do
    if Supports(Components[i],IFRagmentClient,aClient) then
      aClient.HTMLLoaded;
end;

procedure TCustomHTMLFragment.Show;

  Procedure DoStartRender(Sender : TObject; const aName : string);

  begin
    if not UseProjectHTMLFile then
      FTemplate:=GlobalTemplates[aName];
    DoOnHTMLLoaded;
    Render;
  end;

Var
  N,HTML : String;

begin
  if UseProjectHTMLFile then
    DoStartRender(GlobalTemplates,N)
  else
    begin
    N:=TemplateName;
    if N='' then
      N:=Name;
    if GlobalTemplates[N]='' then
      begin
      HTML:=HTMLFileName;
      if (HTML='') then
        HTML:=LowerCase(N)+'.html';
      GlobalTemplates.LoadTemplate(N,Html);
      GlobalTemplates.IfTemplate(N,@DoStartRender);
      end
    else
      DoStartRender(GlobalTemplates,N);
    end;
end;

procedure TCustomHTMLFragment.Hide;
begin
  if IsRendered then
    Unrender;
end;

end.

