{
    This file is part of the Pas2JS run time library.
    Copyright (c) 2019 by Michael Van Canneyt

    RTL HTML utility routines

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
unit Rtl.HTMLUtils;

interface

uses JS, Web, SysUtils, DateUtils, Types;

Type
  IHTMLClient = Interface ['{984EDD58-4F93-40BA-A757-06AC293D11F9}']
    Procedure HTMLLoaded;
    Procedure HTMLRendered;
  end;

  TTextMode = (tmText,tmHTML);
{ ----------------------------------------------------------------------
  HTML tag manipulation
  ----------------------------------------------------------------------}

// Check/Add/remove CSS classes
procedure AddClass(EM: TJSHTMLElement; Const aClass: String); overload;
procedure AddClass(aList: TJSNodeList; Const aClass: String); overload;
procedure RemoveClass(EM: TJSHTMLElement; Const aClass: String); overload;
procedure RemoveClass(aList: TJSNodeList; Const aClass: String); overload;
procedure AddRemoveClass(EM: TJSHTMLElement; Const aAddClass, aRemoveClass: String); overload;
procedure AddRemoveClass(aList: TJSNodeList; Const aAddClass, aRemoveClass: String); overload;
function HasClass(aElement: TJSHTMLElement;  const aClass: String): Boolean;

// replace CR/LF with <BR>, < and > with appropriate entities
Function HtmlIze(aString : String) : String;

{
  Get/Set the value of an element:
  For Input - value
  For Select - Value
  For TextArea - Value
  For Img : Src
  For Other : InnerText.
}
Function GetElementValue(el: TJSHTMLElement): string;
Procedure SetElementValue(el: TJSHTMLElement; aValue: String);

function getRadioGroupValue(const aGroupName: String): string;
procedure setRadiogroupSelectedElement(const aGroupName, aValue: string);


{ ----------------------------------------------------------------------
  Date/Time handling
  ----------------------------------------------------------------------}


Function UTCToLocalTime(aDateTime: TDateTime): TDateTime;
Function LocalTimeToUTC(aDateTime: TDateTime): TDateTime;

Function ExtractDate(S: String): TDateTime;
Function ExtractTime(S: String): TDateTime;

Function FormatHTMLDate(aDateTime : TDateTime; ZeroAsEmpty : Boolean = True) : string;
Function FormatHTMLTime(aDateTime : TDateTime; ZeroAsEmpty : Boolean = True) : string;



Function AbsoluteURL(aBaseURL,aURL : String) : string;

Type
  TJSHTMLElementMatcher = Reference to Function(aElement : TJSHTMLElement) : Boolean;

  { TJSHTMLElementHelper }

  TJSHTMLElementHelper = Class helper for TJSHTMLElement
  private
    Function GetData(aName: String): String;
    function GetInputValue: String;
    procedure SetData(Index: String; AValue: String);
    procedure SetInputValue(const aValue: String);
  Public
    Function ParentHTMLElement : TJSHTMLElement;
    Function FindParent(aMatch : TJSHTMLElementMatcher) : TJSHTMLElement;
    function FindParentWithClass(const aClass: String): TJSHTMLElement;
    function FindParentWithOutClass(const aClass: String): TJSHTMLElement;
    procedure AddClass(Const aClass: String); overload;
    procedure RemoveClass(Const aClass: String); overload;
    procedure AddRemoveClass(Const aAddClass, aRemoveClass: String); overload;
    function HasClass(const aClass: String): Boolean;
    Property InputValue: String Read GetInputValue Write SetInputValue;
    Property Data[Index: String]: String Read GetData Write SetData;
  end;

Function JSValueToInt(a: JSValue) : Integer;
function AsyncSleep(ms: NativeInt): TJSPromise;

implementation


Function JSValueToInt(a: JSValue) : Integer;

begin
  if isNumber(a) then
    Result:=Trunc(Double(a))
  else if isString(a) then
    Result:=StrToIntDef(String(a),0)
  else
    Result:=0;
end;


Function FormatHTMLDate(aDateTime : TDateTime; ZeroAsEmpty : Boolean = True) : string;

begin
  if (Trunc(aDateTime)=0) and (ZeroAsEmpty) then
    Result:=''
  else
    Result:=FormatDateTime('yyyy"-"mm"-"dd',aDateTime);
end;

Function FormatHTMLTime(aDateTime : TDateTime; ZeroAsEmpty : Boolean = True) : string;

begin
  if (Trunc(aDateTime)=0) and (ZeroAsEmpty) then
    Result:=''
  else
    Result:=FormatDateTime('hh":"nn"',aDateTime);
end;

Function ExtractDate(S: String): TDateTime;

Var
  Y, M, D: Word;

begin
  Y:=StrToIntDef(Copy(S, 1, 4), 0);
  M:=StrToIntDef(Copy(S, 6, 2), 0);
  D:=StrToIntDef(Copy(S, 9, 2), 0);
  if (Y<>0) and (M>0) and (M<13) and (D>0) and (D<32) then
    Result:=EncodeDate(Y, M, D)
  else
    Result:=0;
end;

Function ExtractTime(S: String): TDateTime;

Var
  H, M, Sec: Word;

begin
  H:=StrToIntDef(Copy(S, 1, 2), 0);
  M:=StrToIntDef(Copy(S, 4, 2), 0);
  if Length(S)>6 then
    Sec:=StrToIntDef(Copy(S, 7, 2), 0)
  else
    Sec:=0;
  if (H>=0) and (H<25) and (M>=0) and (M<60) and (Sec>=0) and (Sec<60) then
    Result:=EncodeTime(H, M, Sec, 0)
  else
    Result:=0;
end;

Function UTCToLocalTime(aDateTime: TDateTime): TDateTime;

Var
  Y, Mo, D, H, Mi, S, MS: Word;
  JD: TJSDate;

begin
  DecodeDateTime(aDateTime, Y, Mo, D, H, Mi, S, MS); // will be In UTC
  // Returns local time
  JD:=TJSDate.New(TJSDate.UTC(Y, Mo-1, D, H, Mi, S));
  Result:=JSDateToDateTime(JD);
end;

Function LocalTimeToUTC(aDateTime: TDateTime): TDateTime;

Var
  JD: TJSDate;

begin
  // Local time
  JD:=DateTimeToJSDate(aDateTime);
  Result:=EncodeDateTime(JD.UTCFullYear, JD.UTCMonth+1, JD.UTCDate, JD.UTCHours, JD.UTCMinutes, JD.UTCSeconds, 0);
end;

procedure RemoveClass(EM: TJSHTMLElement; Const aClass: String);

begin
  AddRemoveClass(EM, '', aClass);
end;

procedure RemoveClass(aList: TJSNodeList; Const aClass: String);

Var
  I: Integer;

begin
  For I:=0 to aList.Length-1 do
    if (aList[I] is TJSHTMLElement) then
      AddRemoveClass(TJSHTMLElement(aList[I]), '', aClass);
end;

procedure AddClass(aList: TJSNodeList; Const aClass: String);

Var
  I: Integer;

begin
  For I:=0 to aList.Length-1 do
    if (aList[I] is TJSHTMLElement) then
      AddRemoveClass(TJSHTMLElement(aList[I]), aClass, '');
end;

procedure AddClass(EM: TJSHTMLElement; Const aClass: String);

begin
  AddRemoveClass(EM, aClass, '');
end;

procedure AddRemoveClass(aList: TJSNodeList; Const aAddClass, aRemoveClass: String); overload;

Var
  I: Integer;

begin
  For I:=0 to aList.Length-1 do
    if (aList[I] is TJSHTMLElement) then
      AddRemoveClass(TJSHTMLElement(aList[I]), aAddClass, aRemoveClass);
end;

procedure AddRemoveClass(EM: TJSHTMLElement; Const aAddClass, aRemoveClass: String);

Var
  S: String;
  List: TStringDynArray;
  idx: Integer;

begin
  if Not Assigned(EM) then
    Exit;
  List:=TJSString(EM.ClassName).split(' ');
  if (aRemoveClass<>'') then
  begin
    idx:=TJSArray(List).indexOf(aRemoveClass);
    if idx<>-1 then
      Delete(List, idx, 1);
  end;
  if (aAddClass<>'') then
  begin
    idx:=TJSArray(List).indexOf(aAddClass);
    if idx=-1 then
      TJSArray(List).Push(aAddClass);
  end;
  S:=TJSArray(List).join(' ');
  EM.ClassName:=S;
end;

function HasClass(aElement: TJSHTMLElement; const aClass: String): Boolean;
begin
  if (aClass = '') or Not Assigned(aElement) then
    Exit(False);
  Result:=aElement.ClassList.contains(aClass);
end;


function getRadioGroupValue(const aGroupName: String): string;

Var
  N : TJSElement;

begin
  Result:='';
  N:=Document.querySelector(Format('input[name="%s"]:checked',[aGroupName]));
  if Assigned(N) then
    Result:=TJSHTMLInputElement(N).value;
end;


procedure setRadiogroupSelectedElement(const aGroupName, aValue: string);

  procedure DoSet(currentValue: TJSNode; currentIndex{%H-}: NativeInt; list{%H-}: TJSNodeList);

  Var
    El : TJSHTMLInputElement absolute currentValue;

  begin
    El.Checked:=(El.Value=aValue);
  end;


Var
  N : TJSNodeList;

begin
  N:=Document.querySelectorAll(Format('input[name="%s"]',[aGroupName]));
  N.forEach(@DoSet);
end;

Function GetElementValue(el: TJSHTMLElement): string;

Var
  S: String;

begin
  S:='';
  if Not Assigned(El) then
    Console.Debug('Attempting to get value from empty element')
  else if El is TJSHTMLInputElement then
    S:=TJSHTMLTextAreaElement(El).Value
  else if El is TJSHTMLSelectElement then
    S:=TJSHTMLInputElement(El).Value
  else if el is TJSHTMLTextAreaElement then
    S:=TJSHTMLSelectElement(El).Value
  else if el is TJSHTMLImageElement then
    S:=TJSHTMLImageElement(El).Src
  else
    S:=El.InnerText;
  Result:=S;
end;

Procedure SetElementValue(el: TJSHTMLElement; aValue: String);

begin
  if Not Assigned(El) then
    Console.Debug('Attempting to set value "'+aValue+'" on empty element')
  else if El is TJSHTMLInputElement then
    TJSHTMLInputElement(El).Value:=aValue
  else if El is TJSHTMLSelectElement then
    TJSHTMLSelectElement(El).Value:=aValue
  else if El is TJSHTMLTextAreaElement then
    TJSHTMLTextAreaElement(El).Value:=aValue
  else if El is TJSHTMLImageElement then
    TJSHTMLImageElement(El).Src:=aValue
  else
    El.InnerText:=aValue
end;

{ ---------------------------------------------------------------------
  TJSHTMLElementHelper
  --------------------------------------------------------------------- }

procedure TJSHTMLElementHelper.AddClass(const aClass: String);
begin
  If Assigned(Self) then
    ClassList.add(aClass);
end;

procedure TJSHTMLElementHelper.AddRemoveClass(const aAddClass, aRemoveClass: String);
begin
  If Assigned(Self) then
    begin
    ClassList.add(aAddClass);
    ClassList.Remove(aRemoveClass);
    end;
end;

function TJSHTMLElementHelper.GetData(aName: String): String;

begin
  if Assigned(Self) and IsString(Self.Dataset[aName]) then
    Result:=String(Self.Dataset[aName])
  else
    Result:=''
end;

function TJSHTMLElementHelper.GetInputValue: String;
begin
  Result:=GetElementValue(Self)
end;

procedure TJSHTMLElementHelper.SetData(Index: String; AValue: String);
begin
  Dataset.Map[Index]:=aValue;
end;

function TJSHTMLElementHelper.HasClass(const aClass: String): Boolean;
begin
  Result:=Assigned(Self);
  if Result then
    Result := Self.ClassList.contains(aClass);
end;

procedure TJSHTMLElementHelper.RemoveClass(const aClass: String);
begin
  if Assigned(Self) then
    Self.ClassList.remove(aClass);
end;


procedure TJSHTMLElementHelper.SetInputValue(const aValue: String);
begin
  SetElementValue(Self,aValue)
end;

function TJSHTMLElementHelper.ParentHTMLElement: TJSHTMLElement;

Var
  Par : TJSELement;

begin
  Par:=Nil;
  if Assigned(Self) then
    Par:=Self.ParentElement;
  While Assigned(Par) and not (Par is TJSHTMLELement) do
    Par:=Par.ParentElement;
  Result:=TJSHTMLElement(Par);
end;

function TJSHTMLElementHelper.FindParent(aMatch: TJSHTMLElementMatcher): TJSHTMLElement;

begin
  Result:=ParentHTMLElement;
  While (Result<>Nil) and Not aMatch(Result) do
    Result:=Result.ParentHTMLElement;
end;

function TJSHTMLElementHelper.FindParentWithClass(const aClass : String): TJSHTMLElement;

  Function IsMatch(aEl : TJSHTMLElement) : Boolean;

  begin
    Result:=aEL.ClassList.Contains(aClass);
  end;

begin
  Result:=FindParent(@IsMatch);
end;

function TJSHTMLElementHelper.FindParentWithOutClass(const aClass: String
  ): TJSHTMLElement;

    Function IsMatch(aEl : TJSHTMLElement) : Boolean;

    begin
      Result:=not aEL.ClassList.Contains(aClass);
    end;

begin
  Result:=FindParent(@IsMatch);
end;

Function HtmlIze(aString : String) : String;

begin
  Result:=StringReplace(aString,#13#10,'<br>',[rfReplaceAll]);
  Result:=StringReplace(Result,#13,'<br>',[rfReplaceAll]);
  Result:=StringReplace(Result,#10,'<br>',[rfReplaceAll]);
  Result:=StringReplace(Result,'<','&lt;',[rfReplaceAll]);
  Result:=StringReplace(Result,'>','&gt;',[rfReplaceAll]);
end;

Function AbsoluteURL(aBaseURL,aURL : String) : string;

Var
  R : TJSRegexp;

begin
  R:=TJSRegexp.New('^https?://|^/','i');
  if R.Test(aURL) then
    Result:=aURL
  else
    begin
    if (aBaseURL<>'') and (Copy(aBaseURL,Length(aBaseURL),1)<>'/') then
      aBaseURL:=aBaseURL+'/';
    Result:=aBaseURL+aURL;
    end;
end;


function AsyncSleep(ms: NativeInt): TJSPromise;

begin
  Result := TJSPromise.New(
  procedure(resolve,reject : TJSPromiseResolver)
  begin
    window.setTimeout(
    procedure()
    begin
      resolve(ms);
    end,
    ms);
  end);
end;

end.
