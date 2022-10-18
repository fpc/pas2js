{
    This file is part of the Pas2JS run time library.
    Copyright (c) 2019 by Michael Van Canneyt
    
    Unit to extract data-translate tags from a HTML file and create a JSON file from it.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
Unit langextractor;

{$mode objfpc}{$H+}
interface

uses
  Classes, Contnrs, SysUtils, StrUtils, sax, sax_html, fpjson;

Type
  TFileMode = (fmSingle,fmMultiple);

  TLogEvent = Procedure(Sender : TObject; Const Msg : String) of object;
  ETranslate = Class(Exception);

  TTranslations = Class(TObject)
    Strings : Array of string;
    Used : Boolean;
  end;

  { THTMLLangExtractor }

  THTMLLangExtractor = Class(TComponent)
  private
    // Used in CollectFileNamesAndTexts...
    FCurrent,
    // texts in language used in HTML
    FLangObjects : TJSONObject;
    FFileMode: TFileMode;
    FOutputFileName: String;
    FCleanOutput: Boolean;
    FMiniFied: Boolean;
    FRecurse: Boolean;
    FSingleScope: String;
    FTagName: String;
    // Map of language - JSON object
    FTranslations : TFPObjectList;
    FHTMLDir: String;
    FCurrentName:String;
    FCurrentCount: Integer;
    FOnLog: TLogEvent;
    FLanguages: String;
    FTrash: Boolean;
    procedure DoEndElement({%H-}Sender: TObject; const {%H-}NamespaceURI, {%H-}LocalName, {%H-}QName: SAXString);
    procedure DoStartElement(Sender: TObject; const {%H-}NamespaceURI, {%H-}LocalName, {%H-}QName: SAXString; Atts: TSAXAttributes);
    procedure DoTextElement({%H-}Sender: TObject; const ch: PSAXChar; {%H-}AStart, ALength: Integer);
    function GetLanguageFile(aLang: String): String;
    function GetTagName: String;
    procedure LoadExistingFiles;
    procedure CreateLanguageNodes;
    function LoadFile(const aFileName: string): TJSONObject;
  Protected
    
    procedure AddString(const aName, aValue: String);
    procedure CollectHTMLFileNamesAndTexts(const aFileName: String);
    procedure CopyMissingWords;
    procedure CopyWords(SrcScope, DestScope: TJSONObject; aList: TStrings);
    Procedure Log(Const Msg : String); overload;
    Procedure Log(Const Fmt : String; Const Args : Array of const); overload;
    Procedure CollectHTMLNamesAndTexts(Const aDir : string);
    Procedure CreateLanguageFiles;
  Public
    Constructor Create(aOwner : TComponent); override;
    Destructor Destroy; override;
    Procedure Execute;
    // ClearOutput 
    Property CleanOutput : Boolean Read FCleanOutput Write FCleanOutput;
    // HTML Files that need translation
    Property HTMLDir : String Read FHTMLDir Write FHTMLDir;
    // File for JSON file(s) with translations
    Property OutputFileName : String Read FOutputFileName Write FOutputFileName;
    // Emit Log messages
    Property OnLog : TLogEvent Read FOnLog Write FOnlog;
    // Minified language constants
    Property Minified : Boolean Read FMiniFied Write FMinified;
    // TagName (data-tag)
    Property TagName : String Read GetTagName Write FTagName;
    // Trash new values in translations.
    Property TrashNewValues : Boolean Read FTrash Write FTrash;
    // Single/Multiple files
    Property OutputFileMode : TFileMode Read FFileMode Write FFileMode;
    // Languages: comma-separated list. First is the input language (en)
    Property Languages: String Read FLanguages Write FLanguages;
    // Recurse : Boolean;
    Property Recurse: Boolean Read FRecurse Write FRecurse;
    // SingleScope : If this is set, all identifiers are set in a single scope.
    Property SingleScope : String Read FSingleScope Write FSingleScope;
  end;

implementation

{ THTMLLangExtractor }

procedure THTMLLangExtractor.Log(const Msg: String);
begin
  if Assigned(FOnLog) then
    FOnLog(Self,Msg);
end;

procedure THTMLLangExtractor.Log(const Fmt: String; const Args: array of const);
begin
  Log(Format(Fmt,Args));
end;




procedure THTMLLangExtractor.DoStartElement(Sender: TObject; const {%H-}NamespaceURI, LocalName, {%H-}QName: SAXString; Atts: TSAXAttributes);

Var
  aID,aTerm,aAttr : String;
  I,P,aCount : Integer;

begin
  if Not Assigned(atts) then exit;
  aID:=UTF8Encode(Atts.GetValue('','data-'+Utf8Decode(tagname)));
  if (aID='') then
    exit;
  aCount:=WordCount(aID,[';']);
  FcurrentName:='';
  for I:=1 to aCount do
    begin
    aTerm:=ExtractWord(I,aID,[';']);
    P:=Pos('-',aTerm);
    if (P=0) then
      begin
      if FCurrentName='' then
        FCurrentName:=aID
      else
        Log('Translate element "%s" contains 2 IDs: "%s" "%s". Ignoring 2nd ',[aID,FCurrentName,aTerm]);
      end
    else
      begin
      aAttr:=Copy(aTerm,P+1);
      AddString(aTerm,UTF8Encode(Atts.GetValue('',UTF8Decode(aAttr))));
      end;
    end;
end;

procedure THTMLLangExtractor.DoTextElement(Sender: TObject; const ch: PSAXChar; AStart, ALength: Integer);

Var
  S : String;
  W : UnicodeString;

begin
  if FCurrentName='' then exit;
  W:='';
  SetLength(W,aLength);
  Move(ch^,W[1],aLength*SizeOf(WideChar));
  S:=Trim(UTF8Encode(W));
  AddString(FCurrentName,S);
end;

procedure THTMLLangExtractor.AddString(const aName, aValue: String);

Var
  Idx : Integer;
  Old : String;

begin
  Idx:=FCurrent.IndexOfName(aName,True);
  If Idx<>-1 then
    begin
    Old:=FCurrent.Items[idx].AsString;
    if (Old<>aValue) then
      Log('Ignoring duplicate name %s. Old text = "%s", new = "%s"',[aName, Old, aValue]);
    end
  else
    begin
    FCurrent.Strings[aName]:=aValue;
    FCurrentName:='';
    Inc(FCurrentCount);
    end;
end;

procedure THTMLLangExtractor.CollectHTMLFileNamesAndTexts(const aFileName : String);

Var
  MyReader : THTMLReader;
  F : TFileStream;
  aScope : string;


begin
  if SingleScope<>'' then
    aScope:=SingleScope
  else
    aScope:=LowerCase(ChangeFileExt(ExtractFileName(aFileName),''));
  Log('Searching %s for translatable terms, adding to scope : %s',[aFileName,aScope]);
  if (FLangObjects.Items[0] as TJSONObject).IndexOfName(aScope)<>-1 then
    FCurrent:=(FLangObjects.Items[0] as TJSONObject).Objects[aScope]
  else
    begin
    FCurrent:=TJSONObject.Create;
    // Add scope to default language
    (FLangObjects.Items[0] as TJSONObject).Add(aScope,FCurrent);
    end;
  FCurrentCount:=0;
  MyReader:=nil;
  F:=TFileStream.Create(aFileName,fmOpenRead or fmShareDenyNone);
  Try
    MyReader:=THTMLReader.Create;
    MyReader.OnStartElement:=@DoStartElement;
    MyReader.OnCharacters:=@DoTextElement;
    MyReader.OnEndElement:=@DoEndElement;
    MyReader.ParseStream(F);
    Log('Found %d translatable terms',[FCurrentCount]);
  finally
    FreeAndNil(MyReader);
    FreeAndNil(F);
  end;
end;

procedure THTMLLangExtractor.DoEndElement(Sender: TObject; const NamespaceURI, LocalName, QName: SAXString);
begin
  FCurrentName:='';
end;


procedure THTMLLangExtractor.CollectHTMLNamesAndTexts(const aDir: string);

Var
  Info : TSearchRec;

begin
  // HTML files
  If FindFirst(aDir+'*.html',0,Info)=0 then
    try
      Repeat
        CollectHTMLFileNamesAndTexts(aDir+Info.Name);
      Until FindNext(Info)<>0;
    finally
      FindClose(Info);
    end;
  // Subdirs
  if Recurse then
    If FindFirst(aDir+'*',faDirectory,Info)=0 then
      try
        Repeat
          With Info do
            if ((Attr and faDirectory)<>0) and (Name<>'.') and (Name<>'..') then
              CollectHTMLNamesAndTexts(IncludeTrailingPathDelimiter(aDir+Name));
        Until FindNext(Info)<>0;
      finally
        FindClose(Info);
      end;
end;


function THTMLLangExtractor.GetLanguageFile(aLang: String): String;

Var
  Ext : String;

begin
  Ext:=ExtractFileExt(OutputFileName);
  Result:=ChangeFileExt(OutputFileName,'-'+aLang+Ext);
end;

function THTMLLangExtractor.GetTagName: String;
begin
  Result:=FTagName;
  if Result='' then
    Result:='translate';
end;

procedure THTMLLangExtractor.CreateLanguageFiles;

  Function GetAsJSON(aObject : TJSONObject) : string;

  begin
    if FMinified then
      Result:=aObject.AsJSON
    else
      Result:=aObject.FormatJSON
  end;

Var
  I : Integer;
  S : TStringStream;

begin
  if FFileMode=fmSingle then
    begin
    S:=TstringStream.Create(GetAsJSON(FLangObjects),TEncoding.UTF8);
    try
      S.SaveToFile(OutputFileName);
    finally
      S.Free;
    end;
    end
  else
    begin
    For I:=0 to FLangObjects.Count-1 do
      begin
      S:=TstringStream.Create(GetAsJSON(FLangObjects.Items[i] as TJSONObject),TEncoding.UTF8);
      try
        S.SaveToFile(GetLanguageFile(FLangObjects.Names[i]));
      finally
        S.Free;
      end;
      end;
    end;
end;


constructor THTMLLangExtractor.Create(aOwner: TComponent);
begin
  inherited Create(aOwner);
  FLangObjects:=TJSONObject.Create;
  FTranslations:=TFPObjectList.Create(True);
end;

destructor THTMLLangExtractor.Destroy;
begin
  FreeAndNil(FTranslations);
  FreeAndNil(FLangObjects);
  inherited Destroy;
end;

procedure THTMLLangExtractor.CopyWords(SrcScope,DestScope : TJSONObject; aList : TStrings);

Var
  I : Integer;
  aName,aValue : String;

begin
  For I:=0 to SrcScope.Count-1 do
    begin
    aName:=SrcScope.Names[I];
    if DestScope.IndexOfName(aName)=-1 then
      begin
      if TrashNewValues then
        aValue:='生词'+IntToStr(i)
      else
        aValue:=SrcScope.Items[I].AsString;
      DestScope.Add(aName,aValue);
      if Assigned(aList) then
        aList.Add(aName);
      end;
    end;
end;

procedure THTMLLangExtractor.CopyMissingWords;

Var
  I,J,aSectionWordCount,aSectionCount : Integer;
  NewWords : TStringList;
  Src,Dest,SrcScope,DestScope : TJSONObject;
  NewSection : Boolean;
  aScope : String;

begin
  aSectionCount:=0;
  aSectionWordCount:=0;
  NewWords:=TstringList.Create;
  Try
    NewWords.Sorted:=True;
    NewWords.Duplicates:=dupIgnore;
    Src:=FLangObjects.Items[0] as TJSONObject;
    // Copy all scopes
    For I:=0 to Src.Count-1 do
      begin
      aScope:=Src.Names[I];
      SrcScope:=Src.Items[i] as TJSONObject;
      NewSection:=False;
      For J:=1 to FLangObjects.Count-1 do
        begin
        Dest:=FLangObjects.Items[J] as TJSONObject;
        If (Dest.IndexOfName(aScope)=-1) then
          begin
          NewSection:=true;
          if TrashNewValues then
            begin
            DestScope:=TJSONObject.Create;
            Dest.Add(aScope,DestScope);
            CopyWords(SrcScope,DestScope,Nil);
            end
          else
            Dest.Add(aScope,Src.Items[I].Clone);
          end
        else
          begin
          DestScope:=Dest.Objects[aScope] as TJSONObject;
          CopyWords(SrcScope,DestScope,NewWords);
          end;
        end;
      If NewSection then
        begin
        Inc(aSectionCount);
        Inc(aSectionWordCount,SrcScope.Count);
        end;
      end;
    Log('Copied %d new scopes with %d words, added %d new words in existing scopes.',[aSectionCount,aSectionWordCount,NewWords.Count])
  finally
    NewWords.Free;
  end;
end;

function THTMLLangExtractor.LoadFile(const aFileName: string): TJSONObject;

Var
  F : TFileStream;
  D : TJSONData;

begin
  Log('Loading existing file "%s"',[aFileName]);
  F:=TFileStream.Create(aFileName,fmOpenRead or fmShareDenyWrite);
  try
    D:=GetJSON(F);
    if D is TJSONObject then
      begin
      Result:=D as TJSONObject;
      D:=Nil;
      end
    else
      begin
      Log('File "%s" does not contain valid JSON',[aFileName]);
      Result:=TJSONObject.Create;
      end;
  finally
    D.Free;
    F.Free;
  end;
end;


procedure THTMLLangExtractor.LoadExistingFiles;

Var
  I : Integer;
  Obj : TJSONObject;
  aLang : String;

begin
  // Load global file, if any
  if (OutputFileMode=fmSingle) and FileExists(OutputFileName) then
    begin
    Obj:=LoadFile(OutputFileName);
    FreeAndNil(FLangObjects);
    FLangObjects:=Obj;
    end;
  // Add all languages
  for I:=1 to WordCount(Languages,[',']) do
    begin
    aLang:=ExtractWord(I,Languages,[',']);
    if (OutputFileMode=fmMultiple) and FileExists(GetLanguageFile(aLang)) then
      FLangObjects.Add(aLang,LoadFile(GetLanguageFile(aLang)))
    else if FLangObjects.IndexOfName(aLang)=-1 then
      FLangObjects.Add(aLang,TJSONObject.Create)
    end;
end;

Procedure THTMLLangExtractor.CreateLanguageNodes;

var
  I : Integer;
  aLang : String;
  
begin
  FreeAndNil(FLangObjects);
  FLangObjects:=TJSONObject.Create;
  // Add all languages
  for I:=1 to WordCount(Languages,[',']) do
    begin
    aLang:=ExtractWord(I,Languages,[',']);
    if FLangObjects.IndexOfName(aLang)=-1 then
      FLangObjects.Add(aLang,TJSONObject.Create)
    end;
end;

procedure THTMLLangExtractor.Execute;

Var
  aCount : Integer;

begin
  if Languages='' then
    Languages:='en';
  if not CleanOutput then
    LoadExistingFiles
  else
    CreateLanguageNodes;  
  if (HTMLDir<>'') then
    CollectHTMLNamesAndTexts(IncludeTrailingPathDelimiter(HTMLDir));
  aCount:=FLangObjects.Items[0].Count;
  Log('Collected %d message scopes',[aCount]);
  CopyMissingWords;
  CreateLanguageFiles;
end;

end.

