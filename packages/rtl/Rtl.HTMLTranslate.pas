{
    This file is part of the Pas2JS run time library.
    Copyright (c) 2019 by Michael Van Canneyt

    RTL HTML tag translator class

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
unit Rtl.HTMLTranslate;

interface

Uses SysUtils, JS, Web, Classes, Rtl.HTMLUtils;

(* Language data is a JSON object structure. 

  2 file-based formats are supported (see LanguageSource):

  lsSingle: Single Language file in the following form
  {
    "name" : "value"
  }
  or
  {
    scope1 : {
      "name' : "value"
    },
    scope1 : {
      "name' : "value"
    }
  }
  lsMulti: Multiple languages are in the file in the following form. 
  Languages are lowercase !
  (
    "lang1" : {
          "name" : "value"
    },
    "lang2" : {
      scope1 : {
        "name' : "value"
      },
      scope1 : {
        "name' : "value"
      }
    }
  }
  
  there is an Javascript-only mode as well:
  
  for language mode lsScoped: a JS global variable is assumed to exist
  with a JS object using the same format as the lsMultiple format.
  
*)

Const
  DefaultDataTagName = 'translate';

Type
  TLanguageLoadedEvent = reference to Procedure (Sender : TObject; aLanguage : String);
  TLanguageLoadErrorEvent = reference to Procedure (Sender : TObject; aLanguague,aErrorMessage : String);

  // Are all languages in a single file or in multiple files or in global JS scope.
  
  TLanguageSource = (lsSingle, lsMulti, lsScoped);
  
  EHTMLTagTranslator = Class(Exception);

  { THTMLTagTranslator }

  THTMLTagTranslator = class(TComponent)
  private
    FDataTagName: String;
    FLanguage: String;
    FOnLanguageLoaded: TLanguageLoadedEvent;
    FCurrLanguageStrings: TJSObject;
    FDefaultScopeObj : TJSObject;
    FLanguages: TJSObject;
    FLanguageFileURL: String;
    FLanguageVarName: String;
    FFileMode: TLanguageSource;
    FOnLanguageLoadError: TLanguageLoadErrorEvent;
    FDefaultScopeName: String;
    FLoadingLanguage: String;
    FContinueKey: String;
    FTextMode: TTextMode;
    procedure LoadLanguageFile(aLanguage: String);
  Protected
    procedure DoSetLanguageData(aData : TJSObject; aLanguage : String);
    function GetDataTagName : String;
    function GetSeeAlsoScope(aScope: TJSObject): TJSObject;
    procedure SetLanguage(const Value: String); virtual;
    Procedure DoLanguageLoaded; virtual;
    Property LoadingLanguage : String Read FloadingLanguage;
    Procedure TranslateHTMLTag(aEl : TJSHTMLElement; aScope,aSeealsoScope : TJSObject); overload;
    Procedure TranslateBelowHTMLTag(aEl : TJSHTMLElement; aScope,aSeealsoScope : TJSObject); overload;
  Public
    // Directly set the language data instead of using an URL. This can be used with lsSingle or lsMulti
    procedure SetLanguageData(aData : TJSObject);
    // Translate a single HTML tag, using indicated scope.
    Procedure TranslateHTMLTag(aEl : TJSHTMLElement; aScope : String = ''); overload;
    // Translate everything below the indicated HTML tag, using indicated scope.
    Procedure TranslateBelowHTMLTag(aEl : TJSHTMLElement; aScope : String = ''); overload;
    // Search message name in current root, scope
    function GetMessageByName(const aScope, aName: string): String; overload;
    // Search scope in provided root, use to search message in found scope
    Function GetMessageByName(aRoot : TJSObject; const aScope,aName: string): String; overload;
    // Search in aScope for message aName
    Function GetMessageByName(aScope : TJSObject; aName : string) : String; overload;
    // Find in aScope and aSeeAlsoScope message named aName
    Function GetMessageByName(aScope,aSeealsoScope : TJSObject; aName : string) : String; overload;
    // Check if language is available
    Function HasLanguage(aLanguage : String) : Boolean;
    // Find a scope in the current languages
    Function GetScope(const aScope : String) : TJSObject; overload;
    Function GetScope(aRoot : TJSObject; const aScope : String) : TJSObject; overload;
    Property CurrLanguageStrings : TJSObject Read FCurrLanguageStrings;
  Published
    // Data Tag name. Default is 'translate'
    Property DataTagName : String Read GetDataTagName Write FDataTagName;
    // Default scope to use when looking for translations. First the local scope is checked, then default scope, then ContinueKey
    Property DefaultScope : String Read FDefaultScopeName Write FDefaultScopeName;
    // Continue key name;
    // When set, indicates the name of a scope in which to continue searching for a translation term.
    // This can be used for form inheritance; 
    // ContinueKey can be set to continue searching in the inherited scope.
    Property ContinueKey : String Read FContinueKey Write FContinueKey;
    // Language name (will be lowercased)
    Property Language : String Read FLanguage Write SetLanguage;
    // Language source: single file, multiple files or in Javascript Scope 
    Property LanguageSource : TLanguageSource Read FFileMode Write FFileMode;
    // File to load language strings from. 
    // If LanguageSource=lsMulti, a format with the language code is done (use %s)
    // Example: /js/languages/lang-%s.json
    Property LanguageFileURL : String Read FLanguageFileURL Write FLanguageFileURL;
    // if LanguageSource=lsScoped then LanguageVarName is the name of the global Window property that contains the translations.
    property LanguageVarName: String read FLanguageVarName write FLanguageVarName;
    // Textmode determines whether we use InnerText or InnerHTML to set the text.
    Property TextMode : TTextMode Read FTextMode Write FTextMode;
    // Called when the language file is succesfully loaded.
    Property OnLanguageLoaded : TLanguageLoadedEvent Read FOnLanguageLoaded Write FOnLanguageLoaded;
    // Called when an error occurs during loading of a language file.
    Property OnLanguageLoadError : TLanguageLoadErrorEvent Read FOnLanguageLoadError Write FOnLanguageLoadError;
  end;

implementation

uses Types;

Resourcestring
  SErrNoSuchLanguage = 'No such language available: %s.';
  SErrFailedToLoadURL = 'Failed to load language file from URL: "%s".';
  SErrUnknownError = 'Unknown error loading language from URL "%s": %s.';
  SErrNoLanguageSelected = 'Cannot TranslateHTML, no language selected.';


procedure THTMLTagTranslator.DoLanguageLoaded;
begin
  FloadingLanguage:='';
  if Assigned(FOnLanguageLoaded) then
    FOnLanguageLoaded(Self,FLanguage);
end;

function THTMLTagTranslator.GetDataTagName : String;

begin
  Result:=FDataTagName;
  if Result='' then
    Result:=DefaultDataTagName;
end;


function THTMLTagTranslator.GetScope(const aScope: String): TJSObject;

begin
  Result:=GetScope(FCurrLanguageStrings,aScope);
end;

function THTMLTagTranslator.GetScope(aRoot: TJSObject; const aScope: String
  ): TJSObject;

begin
  if (aScope='') then
    Result:=aRoot
  else if Assigned(aRoot) and isObject(aRoot[aScope]) and Assigned(TJSObject(aRoot[aScope])) then
    Result:=TJSObject(aRoot[aScope])
  else
    Result:=Nil;
end;

function THTMLTagTranslator.GetMessageByName(const aScope,aName: string): String;

begin
  Result:=GetMessageByName(FCurrLanguageStrings,aScope,aName);
end;

function THTMLTagTranslator.GetMessageByName(aRoot : TJSObject; const aScope,aName: string): String;

begin
  Result:=GetMessageByname(GetScope(aRoot,aScope),aName)
end;

function THTMLTagTranslator.GetSeeAlsoScope(aScope : TJSObject) : TJSObject;

begin
  if Assigned(aScope) and (ContinueKey<>'') and IsString(aScope[ContinueKey]) then
    Result:=GetScope(FCurrLanguageStrings,String(aScope[ContinueKey]))
  else
    Result:=Nil;
end;

function THTMLTagTranslator.GetMessageByName(aScope : TJSObject; aName: string): String;

Var
  aScope2 : TJSObject;

begin
  aScope2:=GetSeeAlsoScope(aScope);
  Result:=GetMessageByName(aScope,aScope2,aName);
end;

function THTMLTagTranslator.GetMessageByName(aScope, aSeealsoScope: TJSObject;  aName: string): String;

Var
  DoDefault : Boolean;

begin
  Result:='';
  DoDefault:=Not Assigned(aScope);
  if Assigned(aScope) then
    begin
    if IsString(aScope[aName]) then
      Result:=String(aScope[aName])
    else if Assigned(aSeeAlsoScope) then
      begin
      if IsString(aSeeAlsoScope[aName]) then
        Result:=String(aSeeAlsoScope[aName])
      else
        DoDefault:=True;
      end
    else
      DoDefault:=True;  
    end;
  if DoDefault and Assigned(FDefaultScopeObj) and IsString(FDefaultScopeObj[aName]) then
    Result:=String(FDefaultScopeObj[aName]);
end;

function THTMLTagTranslator.HasLanguage(aLanguage: String): Boolean;
begin
  aLanguage:=LowerCase(aLanguage);
  Result:=False;
  if FLanguages=Nil then exit;
  if (LanguageSource=lsMulti) or (LanguageSource=lsScoped) then
    Result:=isObject(FLanguages[aLanguage])
  else
    Result:=(sameText(aLanguage,FLanguage))
end;

procedure THTMLTagTranslator.LoadLanguageFile(aLanguage: String);

var
  URL : String;

  Procedure DoLoadError(aMsg : String);

  begin
    If Assigned(FOnLanguageLoadError) then
      FOnLanguageLoadError(Self,aLanguage,aMsg);
  end;

  function LoadLanguageJson(value : JSValue) : JSValue;
  begin
    Result:=True;
    try
      DoSetLanguageData(TJSJSON.parseObject(String(Value)),aLanguage);
    except
      on er : TJSError do
        DoLoadError(Format(SErrUnknownError,[URL,er.message]));
      on e : TJSObject do
        DoLoadError(Format(SErrUnknownError,[URL,TJSJSON.stringify(E)]));
    end;
    DoLanguageLoaded;
  end;

  function doOK(response : JSValue) : JSValue;

  var
    Res : TJSResponse absolute response;

  begin
    Result:=Null;
    If (Res.status<>200) then
      begin
      DoLoadError(Format(SErrUnknownError,[URL,res.StatusText]));
      end
    else
      Res.text._then( @LoadLanguageJson );
  end;

  function doFail(response{%H-} : JSValue) : JSValue;

  begin
    Result:=Null;
    DoLoadError(Format(SErrFailedToLoadURL,[URL]));
  end;

begin
  if LanguageSource = lsScoped then
    DoSetLanguageData(TJSObject(window[LanguageVarName]),aLanguage)
  else
  begin
    FloadingLanguage:=aLanguage;
    if LanguageSource=lsMulti then
      URL:=LanguageFileURL
    else
      URL:=Format(LanguageFileURL,[aLanguage]);
    Window.Fetch(URl)._then(@DoOK).catch(@DoFail);
  end;
end;

procedure THTMLTagTranslator.DoSetLanguageData(aData: TJSObject;
  aLanguage: String);

  Procedure DoLoadError(aMsg : String);

  begin
    If Assigned(FOnLanguageLoadError) then
      FOnLanguageLoadError(Self,aLanguage,aMsg);
  end;

var
  aVal: JSValue;

begin
  FLanguages:=aData;
  if FLanguages=Nil then
    exit;
  if (LanguageSource=lsMulti) or (LanguageSource=lsScoped) then
    FCurrLanguageStrings:=TJSObject(FLanguages[aLanguage])
  else
    FCurrLanguageStrings:=TJSObject(FLanguages);
  if Not Assigned(FCurrLanguageStrings) then
    DoLoadError(Format(SErrNoSuchLanguage,[aLanguage]));
  FDefaultScopeObj:=Nil;
  if DefaultScope<>'' then
    begin
    aVal:=FCurrLanguageStrings[DefaultScope];
    if IsObject(aVal) then
      FDefaultScopeObj:=TJSObject(aVal)
    end;
  FLanguage:=aLanguage;
end;

procedure THTMLTagTranslator.SetLanguage(const Value: String);

Var
  NewLanguage : String;

begin
  NewLanguage:=LowerCase(Value);
  if NewLanguage=FLanguage then Exit;
  if ((LanguageSource=lsSingle) or (LanguageSource=lsScoped)) and Assigned(FLanguages) then
    begin
    if Not Assigned(TJSObject(FLanguages[NewLanguage])) then
      Raise EHTMLTagTranslator.CreateFmt(SErrNoSuchLanguage,[Value]);
    FCurrLanguageStrings:=TJSObject(FLanguages[NewLanguage]);
    FLanguage := NewLanguage;
    end
  else
    LoadLanguageFile(NewLanguage);
end;

procedure THTMLTagTranslator.TranslateHTMLTag(aEl: TJSHTMLElement;
  aScope: String);

Var
  aScope1,aScope2 : TJSObject;

begin
  aScope1:=GetScope(aScope);
  aScope2:=GetSeeAlsoScope(aScope1);
  TranslateHTMLTag(aEl,aScope1,aScope2);
end;

procedure THTMLTagTranslator.TranslateHTMLTag(aEl: TJSHTMLElement; aScope,
  aSeealsoScope: TJSObject);

Var
  Terms : TStringDynArray;
  T,S,aAttr,aValue : String;
  P : Integer;

begin
  T:=String(aEl.Dataset.Properties[DataTagName]);
  If T='' then
    exit;
  Terms:=T.Split(';');
  For S in Terms do
    begin
    P:=Pos('-',S);
    if P=0 then
      P:=Length(S)+1;
    aAttr:=Copy(S,P+1,Length(S)-P);
    aValue:=GetMessageByName(aScope,aSeeAlsoScope,S);
    if (aValue<>'') then
      if aAttr='' then
        begin
        if TextMode=tmText then
          aEl.InnerText:=aValue
        else
          aEl.InnerHTML:=aValue
        end
      else
        aEl[aAttr]:=aValue;
    end;
end;

procedure THTMLTagTranslator.TranslateBelowHTMLTag(aEl: TJSHTMLElement;
  aScope: String);

Var
  aScope1,aScope2 : TJSObject;

begin
  aScope1:=GetScope(aScope);
  aScope2:=GetSeeAlsoScope(aScope1);
  TranslateBelowHTMLTag(aEl,aScope1,aScope2);
end;

procedure THTMLTagTranslator.TranslateBelowHTMLTag(aEl: TJSHTMLElement; aScope,
  aSeealsoScope: TJSObject);

Var
  NL : TJSNodeList;
  TN : String;
  E: TJSHTMLElement;
  I : Integer;

begin
  TN:=DataTagName;
  if Not assigned(FCurrLanguageStrings) then
    Raise EHTMLTagTranslator.Create(SErrNoLanguageSelected);
  nl:=aEl.querySelectorAll('[data-'+TN+']');
  for I:=0 to nl.length-1 do
    begin
    E:=TJSHTMLElement(nl.Nodes[i]);
    TranslateHTMLTag(E,aScope,aSeealsoScope);
    end;
end;

procedure THTMLTagTranslator.SetLanguageData(aData: TJSObject);

begin
  DoSetLanguageData(aData,Language);
  DoLanguageLoaded;
end;


end.
