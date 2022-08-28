{
    This file is part of the Pas2JS run time library.
    Copyright (c) 2019 by Michael Van Canneyt

    This unit implements a HTML template loader.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
unit Rtl.TemplateLoader;

{$mode objfpc}

interface

uses
  Classes, SysUtils, JS, web;

Type
   TFailData = Record
     message : string;
     code : Integer;
   end;

  TTemplateNotifyEvent = Reference to Procedure(Sender : TObject; Const aTemplate : String);
  TTemplateErrorNotifyEvent = Reference to Procedure(Sender : TObject; Const aTemplate,aError : String; aErrorcode : Integer);

  TTemplateNotification = Record
    Name : string;
    Event  : TTemplateNotifyEvent;
  end;
  TTemplateNotificationDynArray = Array of TTemplateNotification;

  TPreLoadTemplateItem = Class(TCollectionItem)
  private
    FHTMLFile: String;
    FName: string;
    function GetHTMLFile: String;
  protected
    function GetDisplayName: string; override;
  Public
    Procedure Assign(aSource : TPersistent); override;
  Published
    Property Name: string Read FName Write FName;
    Property HTMLFile : String Read GetHTMLFile Write FHTMLFile;
  end;

  { TPreLoadTemplateItemList }

  TPreLoadTemplateItemList = Class(TCollection)
  private
    function GetT(aIndex : Integer): TPreLoadTemplateItem;
  Public
    Property Template[aIndex : Integer] : TPreLoadTemplateItem  Read GetT;default;
  end;

  { TCustomTemplateLoader }

  TCustomTemplateLoader = Class(TComponent)
  Private
    FBaseURL: String;
    FCheckResources: Boolean;
    FOnLoad: TTemplateNotifyEvent;
    FOnLoadFail: TTemplateErrorNotifyEvent;
    FTemplates : TJSObject;
    FNotifications : TTemplateNotificationDynArray;
    FPreload: TPreLoadTemplateItemList;
    function IndexOfTemplateEvent(aName: String): Integer;
    function GetTemplate(const aName : String): String;
    procedure SetTemplate(const aName : String; const AValue: String);
    procedure SetPreload(AValue: TPreLoadTemplateItemList);
  Protected
    procedure NotifyEvents(aName: string);
    function CheckTemplateResource(const aName: string): string; virtual;
    Procedure TemplateLoaded(const aName,aTemplate : String); virtual;
    Procedure Loaded; override;
    // Process an url before it is used to fetch data
    Function ProcessURL(const aURL : String) : String; virtual;
  Public
    Constructor Create (aOwner : TComponent); override;
    Destructor Destroy; override;
    // Load whatever is in PreloadTemplates;
    Procedure Preload; virtual;
    // call aEvent when template aName is loaded.
    Procedure IfTemplate(const aName : String; aEvent : TTemplateNotifyEvent);
    // Remove a template
    Procedure RemoveRemplate(const aName : String);
    // fetch a template using promise. Promise resolves to template name. On fail a TFailData record is passed on.
    // Note that the global OnLoad/OnLoadFail a
    Function FetchTemplate(Const aName,aURL : String) : TJSPromise;
    // procedural API.
    // If the aOnSuccess aOnFail event handlers are specified, they're called as well in addition to global handlers.
    Procedure LoadTemplate(Const aName,aURL : String; aOnSuccess : TTemplateNotifyEvent = Nil; AOnFail : TTemplateErrorNotifyEvent= Nil);
    // procedural API for multiple templates at once.
    // Form = name, URL, name URL.
    // If the aOnSuccess aOnFail event handlers are specified, they're called as well in addition to global handlers.
    Procedure LoadTemplates(Const Templates : Array of String; aOnSuccess : TTemplateNotifyEvent = Nil; AOnFail : TTemplateErrorNotifyEvent= nil);
    // URLs will be relative to this. Take care that you add a / at the end if needed !
    Property BaseURL : String Read FBaseURL Write FBaseURl;
    // Check resources for templates when accessing Templates.
    Property CheckResources : Boolean Read FCheckResources Write FCheckResources;
    // Access to templates based on name
    Property Templates[aName : String] : String Read GetTemplate Write SetTemplate; default;
    // Called when a template was loaded.
    Property OnLoad : TTemplateNotifyEvent Read FOnLoad Write FOnLoad;
    // Called when a template failed to load.
    Property OnLoadFail : TTemplateErrorNotifyEvent Read FOnLoadFail Write FOnLoadFail;
    // templates to load when the component is loaded.
    Property PreloadTemplates : TPreLoadTemplateItemList Read FPreload Write SetPreload;
  end;

  TTemplateLoader = Class(TCustomTemplateLoader)
  Published
    Property BaseURL;
    Property CheckResources;
    Property PreloadTemplates;
    Property OnLoad;
    Property OnLoadFail;
  end;

// Global instance, for ease of use.
Function GlobalTemplates : TCustomTemplateLoader;

implementation

uses p2jsres, rtlconsts;

{ TCustomTemplateLoader }

Var
  _loader : TCustomTemplateLoader;

Function GlobalTemplates : TCustomTemplateLoader;

begin
  if _loader=Nil then
    begin
    _loader:=TCustomTemplateLoader.Create(Nil);
    _loader.Name:='GlobalTemplates';
    end;
  Result:=_Loader;
end;

Type
   { TURLLoader }

   TURLLoader = Class(TObject)
   private
     FLoader: TCustomTemplateLoader;
     FName: String;
     FURL: String;
     procedure dofetch(resolve, reject: TJSPromiseResolver);
   Public
     Constructor Create(aLoader : TCustomTemplateLoader; aName,aURL : String);
     Function fetch : TJSPromise;
     Property Name : String Read FName;
     Property URL : String Read FURL;
     Property Loader : TCustomTemplateLoader Read FLoader;
   end;

{ TPreLoadTemplateItemList }

function TPreLoadTemplateItemList.GetT(aIndex: Integer): TPreLoadTemplateItem;
begin
  Result:=Items[aIndex] as TPreLoadTemplateItem;
end;


{ TURLLoader }

constructor TURLLoader.Create(aLoader: TCustomTemplateLoader; aName, aURL: String);
begin
  FLoader:=aLoader;
  FURL:=aURL;
  FName:=aName;
end;

procedure TURLLoader.dofetch(resolve,reject : TJSPromiseResolver);

  function doOK(response : JSValue) : JSValue;

  var
    Res : TJSResponse absolute response;
    F : TFailData;

  begin
    If (Res.status<>200) then
      begin
      F.Message:=res.StatusText;
      F.Code:=Res.Status;
      Result:=Reject(F);
      end
    else
      Res.text._then(
        function (value : JSValue) : JSValue
          begin
          Loader.TemplateLoaded(Name,String(Value));
          Result:=Resolve(Name);
          end
      );
  end;

  function doErr(Err : JSValue) : JSValue;

  Var
    F : TFailData;
  begin
    F.message:='Unknown error: '+TJSJSON.Stringify(Err);
    F.code:=999;
    Result:=Reject(F);
  end;

  function doFail(respo : JSValue) : JSValue;

  Var
    F : TFailData;

  begin
    F.message:='Unknown error';
    F.code:=999;
    Result:=Reject(F);
  end;

begin
  Window.Fetch(URl)._then(@DoOK,@DoErr).catch(@DoFail);
end;

function TURLLoader.fetch : TJSPromise;

begin
  Result:=TJSPromise.New(@Dofetch)
end;

function TCustomTemplateLoader.GetTemplate(const aName : String): String;

Var
  V : jsValue;

begin
  V:=FTemplates[LowerCase(aName)];
  if isString(V) then
    Result:=String(V)
  else if CheckResources then
    Result:=CheckTemplateResource(aName)
  else
    Result:='';
end;

// We need a polyfill for node.js
Function atob (s : String) : string; external name 'atob';

function TCustomTemplateLoader.CheckTemplateResource(const aName: string
  ): string;

Var
  aInfo : TResourceInfo;

begin
  Result:='';
  If GetResourceInfo(aName,aInfo) and (aInfo.format='application/octet-stream') then
    if aInfo.Encoding='base64' then
      Result:=atob(aInfo.Data)
    else if (aInfo.Encoding='text') then
      Result:=aInfo.Data
    else
      Raise EConvertError.CreateFmt(SErrUnknownResourceEncoding,[aInfo.Encoding]);
end;

procedure TCustomTemplateLoader.SetTemplate(const aName : String; const AValue: String);
begin
  FTemplates[LowerCase(aName)]:=AValue;
end;

procedure TCustomTemplateLoader.SetPreload(AValue: TPreLoadTemplateItemList);
begin
  if FPreload=AValue then Exit;
  FPreload.Assign(AValue);
end;

function TCustomTemplateLoader.IndexOfTemplateEvent(aName: String): Integer;

begin
  Result:=Length(FNotifications)-1;
  While (Result>=0) and not SameText(FNotifications[Result].Name,aName) do
    Dec(Result);
end;

procedure TCustomTemplateLoader.TemplateLoaded(const aName, aTemplate: String);


begin
  FTemplates[LowerCase(aName)]:=aTemplate;
  if Assigned(FOnLoad) then
    FOnLoad(Self,aName);
  NotifyEvents(aName);
end;

procedure TCustomTemplateLoader.NotifyEvents(aName : string);

Var
  Idx : Integer;

begin
  Idx:=IndexOfTemplateEvent(aName);
  While Idx<>-1 do
    begin
    FNotifications[Idx].Event(Self,aName);
    Delete(FNotifications,Idx,1);
    Idx:=IndexOfTemplateEvent(aName);
    end;
end;

procedure TCustomTemplateLoader.Loaded;
begin
  inherited Loaded;
  Preload;
end;

function TCustomTemplateLoader.ProcessURL(const aURL: String): String;

Var
  R : TJSRegexp;

begin
  R:=TJSRegexp.New('^https?://|^/','i');
  if R.Test(aURL) then
    Result:=aURL
  else
    Result:=BaseURL+aURL;
end;

constructor TCustomTemplateLoader.Create(aOwner: TComponent);
begin
  inherited Create(aOwner);
  FTemplates:=TJSObject.New;
  FPreload:=TPreLoadTemplateItemList.Create(TPreLoadTemplateItem);
end;

destructor TCustomTemplateLoader.Destroy;
begin
  FTemplates:=nil;
  FreeAndNil(FPreload);
  inherited Destroy;
end;

procedure TCustomTemplateLoader.Preload;

Var
  I : integer;

begin
  For I:=0 to PreloadTemplates.Count-1 do
    With PreloadTemplates[I] do
      LoadTemplate(Name,HTMLFile);
end;

procedure TCustomTemplateLoader.IfTemplate(const aName: String; aEvent: TTemplateNotifyEvent);
Var
  N : TTemplateNotification;

begin
  if Templates[aName]<>'' then
    aEvent(Self,aName)
  else
   begin
   N.Name:=aName;
   N.Event:=aEvent;
   FNotifications:=Concat(FNotifications,[N]);
   // Can happen
   if Templates[aName]<>'' then
     NotifyEvents(aName)
   end;
end;

procedure TCustomTemplateLoader.RemoveRemplate(const aName: String);
begin
  jsDelete(FTemplates,Lowercase(aName));
end;

function TCustomTemplateLoader.FetchTemplate(const aName, aURL: String): TJSPromise;

begin
  Result:=TURLLoader.Create(Self,aName,ProcessURL(aURL)).fetch;
end;

procedure TCustomTemplateLoader.LoadTemplate(const aName, aURL: String; aOnSuccess: TTemplateNotifyEvent;
  AOnFail: TTemplateErrorNotifyEvent);

  function doOK(aValue{%H-} : JSValue) : JSValue;

  begin
    if Assigned(aOnSuccess) then
      aOnSuccess(Self,aName);
    Result:=nil;
  end;

  function doFail(aValue : JSValue) : JSValue;

  Var
    F : TFailData absolute aValue;
    S : String;
    C : Integer;

  begin
    S:=F.message;
    C:=F.Code;
    if Assigned(FonLoadFail) then
      FOnLoadFail(Self,aName,S,C);
    if Assigned(aOnFail) then
      aOnFail(Self,aName,S,C);
    Result:=nil;
  end;

begin
  FetchTemplate(aName,aURL)._then(@DoOK).catch(@doFail);
end;

procedure TCustomTemplateLoader.LoadTemplates(const Templates: array of String; aOnSuccess: TTemplateNotifyEvent;
  AOnFail: TTemplateErrorNotifyEvent);

Var
  I,L : Integer;

begin
  L:=Length(Templates);
  if (L mod 2)<>0 then
    Raise Exception.CreateFmt('Number of arguments (%d) must be even',[L]);
  I:=0;
  While I<L do
   begin
   LoadTemplate(Templates[I],Templates[I+1],aOnsuccess,aOnFail);
   Inc(I,2);
   end;
end;


{ TPreLoadTemplateItem }

function TPreLoadTemplateItem.GetHTMLFile: String;
begin
  Result:=FHTMLFile;
  If (Result='') and (Name<>'') then
    Result:=LowerCase(Name)+'.html';
end;

function TPreLoadTemplateItem.GetDisplayName: string;
begin
  Result:=Name;
  if Result='' then
    Result:=inherited GetDisplayName;
end;

procedure TPreLoadTemplateItem.Assign(aSource: TPersistent);

Var
  PLI : TPreLoadTemplateItem absolute aSource;

begin
  if aSource is TPreLoadTemplateItem then
    begin
    FName:=PLI.Name;
    FHTMLFile:=PLI.FHTMLFile;
    end
  else
    inherited Assign(aSource);
end;



end.

