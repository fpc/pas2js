{
    This file is part of the Pas2JS run time library.
    Copyright (C) 2019 Michael Van Canneyt

    extra Bootstrap widgets

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

unit bootstrapwidgets;

{$mode objfpc}

interface

uses
  Classes, SysUtils, js, libjquery, libbootstrap, web, webwidget, htmlwidgets;

Type

  { TSimpleToastWidget }
  TContextual = (cNone,cPrimary,cSecondary,cSuccess,cDanger,cWarning,cInfo,cLight,cDark);

  TSimpleToastWidget = Class(TCustomTemplateWidget)
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
    FSmallHeader: String;
    procedure SetAnimate(AValue: Boolean);
    procedure SetAutoHide(AValue: Boolean);
    procedure SetBody(AValue: String);
    procedure SetBoolean(AValue: Boolean);
    procedure SetContextual(AValue: TContextual);
    procedure SetHeader(AValue: String);
    procedure SetHeaderImage(AValue: String);
    procedure SetHideDelay(AValue: Integer);
    procedure SetMinWidth(AValue: Integer);
    procedure SetSmallHeader(AValue: String);
  Protected
    function BodyHTML: String; virtual;
    function CloseButtonHTML: String; virtual;
    function HeaderHTML: String; virtual;
    Function GetTemplateHTML: String; override;
    Function DoRenderHTML(aParent, aElement: TJSHTMLElement): TJSHTMLElement; override;
  Public
    Constructor Create(aOwner : TComponent); override;
    Procedure Hide;
  Published
    Property Header : String Read FHeader Write SetHeader;
    Property SmallHeader : String Read FSmallHeader Write SetSmallHeader;
    Property Body : String Read FBody Write SetBody;
    Property HeaderImage : String Read FHeaderImage Write SetHeaderImage;
    Property CloseButton : Boolean Read FBoolean Write SetBoolean;
    Property Contextual : TContextual Read FContextual write SetContextual;
    Property HideDelay : Integer Read FHideDelay Write SetHideDelay default 2000;
    Property AutoHide : Boolean Read FAutoHide Write SetAutoHide default True;
    Property Animate : Boolean Read FAnimate Write SetAnimate default False;
    Property MinWidth : Integer Read FMinWidth Write SetMinWidth default 200;
  end;

  // Encapsulates the global tag where the toasts are shown.

  { TToastManager }

  TToastManager = Class(TWebWidget)
  Private
    FAnimate: Boolean;
    FAutoHide: Boolean;
    FHideDelay: Integer;
    FMinheight: Integer;
    FMinWidth: Integer;
    FMultiToast: Boolean;
    FContentElement : TJSHTMLElement;
    FToastIcon: String;
    class var
      _instance : TToastManager;
    procedure CheckInit;
    procedure SetMinHeight(AValue: Integer);
    procedure SetMultiToast(AValue: Boolean);
   Protected
    Class Function DefaultParentElement : TJSHTMLElement; override;
    Function DoRenderHTML(aParent, aElement: TJSHTMLElement): TJSHTMLElement; override;
    Function GetContentElement: TJSHTMLELement; override;
    Procedure InvalidateElement; override;
    Function HTMLTag: String; override;
  Public
    Constructor Create(aOwner : TComponent); override;
    class function Instance : TToastManager;
    function ShowToast(const aHeader, aBody: String; aContext: TContextual=cNone; Closable: Boolean=True): TSimpleToastWidget;
  Published
    Property MultiToast : Boolean Read FMultiToast Write SetMultiToast;
    Property MinHeight : Integer Read FMinheight Write SetMinHeight default 250;
    Property ToastHideDelay : Integer Read FHideDelay Write FHideDelay default 2000;
    Property ToastAutoHide : Boolean Read FAutoHide Write FAutoHide default True;
    Property ToastAnimate : Boolean Read FAnimate Write FAnimate default False;
    Property ToastMinWidth : Integer Read FMinWidth Write FMinWidth default 200;
    Property ToastIcon : String Read FToastIcon Write FToastIcon;
  end;

  { TBootstrapButton }

  TBootstrapButton = class (TButtonWidget)
  private
    FContextual: TContextual;
    FOutLine: Boolean;
    procedure SetContextual(AValue: TContextual);
    procedure SetOutLine(AValue: Boolean);
  Protected
    Function RecalcClasses(aOldContextual : TContextual; aOldOutline : Boolean) : String;
  Public
    Constructor Create(aOwner : TComponent); override;
  Published
    Property Contextual : TContextual Read FContextual Write SetContextual default cPrimary;
    Property Outline : Boolean Read FOutLine Write SetOutLine;
  end;

Const
  ContextualNames : Array[TContextual] of string = ('','primary','secondary','success','danger','warning','info','light','dark');

Function Toasts : TToastManager;

Implementation

function Toasts: TToastManager;
begin
  Result:=TToastManager.Instance;
end;

{ TBootstrapButton }

procedure TBootstrapButton.SetContextual(AValue: TContextual);

Var
  Old : TContextual;

begin
  if FContextual=AValue then Exit;
  old:=FContextual;
  FContextual:=AValue;
  RecalcClasses(Old,FOutline);
end;

procedure TBootstrapButton.SetOutLine(AValue: Boolean);

Var
  Old : Boolean;

begin
  if FOutLine=AValue then Exit;
  old:=FoutLine;
  FOutLine:=AValue;
  RecalcClasses(FContextual,Old);
end;

function TBootstrapButton.RecalcClasses(aOldContextual: TContextual; aOldOutline: Boolean): String;

Const
  OL : Array[Boolean] of string = ('','outline-');
Var
  c : String;

begin
  Result:='btn btn-'+OL[FOutLine]+ContextualNames[FContextual];
  C:=RemoveClasses(Classes,'btn-'+OL[aOldOutLine]+ContextualNames[aOldContextual]);
  Classes:=AddClasses(C,Result);
end;

constructor TBootstrapButton.Create(aOwner: TComponent);
begin
  inherited Create(aOwner);
  Contextual:=cPrimary;
end;


{ TToastManager }

class function TToastManager.Instance: TToastManager;
begin
  if _instance=Nil then
   _instance:=TToastManager.Create(Nil);
  Result:=_instance;
end;

procedure TToastManager.CheckInit;

begin
  if not IsRendered then
    Refresh;
end;

procedure TToastManager.SetMinHeight(AValue: Integer);
begin
  if FMinheight=AValue then Exit;
  FMinheight:=AValue;
  if IsRendered then
    Refresh;
end;

procedure TToastManager.SetMultiToast(AValue: Boolean);
begin
  if FMultiToast=AValue then Exit;
  FMultiToast:=AValue;
  if IsRendered then refresh;
end;

class function TToastManager.DefaultParentElement: TJSHTMLElement;
begin
  Result:=TJSHTMLElement(Document.body);
end;

function TToastManager.DoRenderHTML(aParent, aElement: TJSHTMLElement): TJSHTMLElement;

Var
  El : TJSHTMLElement;

begin
  Result:=AElement;
  Result['aria-live']:='polite';
  Result['aria-atomic']:='true';
  Result['style']:='position: relative; min-height: '+IntToStr(MinHeight)+'px;';
  if not MultiToast then
    FContentElement:=Result
  else
    begin
    El:=CreateElement('div',ElementID+'-multi');
    El['style']:='position: absolute; top: 0; right: 0;';
    FContentElement:=El;
    Result.AppendChild(El);
    end;
end;

function TToastManager.GetContentElement: TJSHTMLELement;
begin
  Result:=FContentElement;
  if (Result=Nil) then
    Result:=Element;
end;

procedure TToastManager.InvalidateElement;
begin
  inherited;
  FContentElement:=nil;
end;


function TToastManager.HTMLTag: String;
begin
  Result:='div';
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

function TToastManager.ShowToast(const aHeader, aBody: String; aContext : TContextual = cNone; Closable: Boolean = True): TSimpleToastWidget;
begin
  CheckInit;
  Result:=TSimpleToastWidget.Create(Self) ;
  With Result do
    begin
    Parent:=Self;
    Header:=aHeader;
    Body:=aBody;
    HeaderImage:=ToastIcon;
    CloseButton:=Closable;
    Contextual:=aContext;
    AutoHide:=ToastAutoHide;
    HideDelay:=ToastHideDelay;
    Animate:=ToastAnimate;
    MinWidth:=ToastMinWidth;
    Refresh;
    end;
end;

{ TSimpleToastWidget }

function TSimpleToastWidget.CloseButtonHTML: String;

Var
  S : String;

begin
  S:=ContextualNames[Contextual];
  if S<>'' then
    S:='text-'+S;
  Result:=Result+ '<button type="button" class="ml-2 mb-1 close '+S+'" data-dismiss="toast" aria-label="Close">';
  Result:=Result+ '   <span aria-hidden="true">&times;</span>';
  Result:=Result+ '</button>';
end;

function TSimpleToastWidget.HeaderHTML: String;

Var
  S : String;

begin
  S:=ContextualNames[Contextual];
  if S<>'' then
    S:='text-'+S;
  Result:='<div class="toast-header '+S+'">';
  if HeaderImage<>'' then
    Result:=Result+'<img src="'+HeaderImage+'" class="rounded mr-2">';
  Result:=Result+'<div class="mr-auto">'+Header+'</div>';
  if (SmallHeader<>'') then
    Result:=Result+'<small>'+SmallHeader+'</small>';
  if CloseButton then
    Result:=Result+CloseButtonHTML;
  Result:=Result+'</div>';
end;

procedure TSimpleToastWidget.SetBody(AValue: String);
begin
  if FBody=AValue then Exit;
  FBody:=AValue;
  if isRendered then Refresh;
end;

procedure TSimpleToastWidget.SetAnimate(AValue: Boolean);
begin
  if FAnimate=AValue then Exit;
  FAnimate:=AValue;
  if isRendered then Refresh;
end;

procedure TSimpleToastWidget.SetAutoHide(AValue: Boolean);
begin
  if FAutoHide=AValue then Exit;
  FAutoHide:=AValue;
  if isRendered then Refresh;
end;

procedure TSimpleToastWidget.SetBoolean(AValue: Boolean);
begin
  if FBoolean=AValue then Exit;
  FBoolean:=AValue;
  if isRendered then Refresh;
end;

procedure TSimpleToastWidget.SetContextual(AValue: TContextual);
begin
  if FContextual=AValue then Exit;
  FContextual:=AValue;
  if isRendered then Refresh;
end;

procedure TSimpleToastWidget.SetHeader(AValue: String);
begin
  if FHeader=AValue then Exit;
  FHeader:=AValue;
  if isRendered then Refresh;
end;

procedure TSimpleToastWidget.SetHeaderImage(AValue: String);
begin
  if FHeaderImage=AValue then Exit;
  FHeaderImage:=AValue;
  if isRendered then Refresh;
end;

procedure TSimpleToastWidget.SetHideDelay(AValue: Integer);
begin
  if FHideDelay=AValue then Exit;
  FHideDelay:=AValue;
  if isRendered then Refresh;
end;

procedure TSimpleToastWidget.SetMinWidth(AValue: Integer);
begin
  if FMinWidth=AValue then Exit;
  FMinWidth:=AValue;
  if isRendered then Refresh;
end;

procedure TSimpleToastWidget.SetSmallHeader(AValue: String);
begin
  if FSmallHeader=AValue then Exit;
  FSmallHeader:=AValue;
  if isRendered then Refresh;
end;

function TSimpleToastWidget.BodyHTML: String;

Var
  S : String;

begin
  S:=ContextualNames[Contextual];
  if S<>'' then
    S:='alert-'+S;
  Result:='<div class="toast-body '+S+'">';
  Result:=Result+Body;
  Result:=Result+'</div>';
end;

function TSimpleToastWidget.GetTemplateHTML: String;

Var
  Head : String;

begin
  Result:='<div class="toast" aria-live="assertive" aria atomic="true" style="min-width: '+IntToStr(MinWidth)+'px;">';
  Result:=Result+HeaderHTML;
  Result:=Result+BodyHTML;
  Result:=Result+'</div>';
end;

function TSimpleToastWidget.DoRenderHTML(aParent, aElement: TJSHTMLElement): TJSHTMLElement;
begin
  Result:=inherited DoRenderHTML(aParent, aElement);
  JQuery(Result).toast(New(['animation',FAnimate,'autohide',autohide,'delay',FHideDelay]));
  JQuery(Result).ToastShow;
end;

constructor TSimpleToastWidget.Create(aOwner: TComponent);
begin
  inherited Create(aOwner);
  FMinWidth:=200;
  FAutoHide:=True;
  FHideDelay:=2000;
end;

procedure TSimpleToastWidget.Hide;
begin
  JQuery(Element).Toast('hide');
end;

end.
