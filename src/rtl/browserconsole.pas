unit browserconsole;

{$mode objfpc}{$H+}

interface

uses
  js,web;

Const
  DefaultMaxConsoleLines = 25;
  DefaultConsoleStyle = '.pasconsole { '+sLineBreak+
  'font-family: courier;'+sLineBreak+
  'font-size: 14px;'+sLineBreak+
  'background: #FFFFFF;'+sLineBreak+
  'color: #000000;'+sLineBreak+
  'display: block;'+sLineBreak+
  '}';
  DefaultCRTConsoleStyle = '.pasconsole { '+sLineBreak+
  'font-family: courier;'+sLineBreak+
  'font-size: 14px;'+sLineBreak+
  'background: #000;'+sLineBreak+
  'color: #14fdce;'+sLineBreak+
  'display: block;'+sLineBreak+
  '}';

Var
  // Element ID for console output. If this is not set, body is used.
  // If you cange this, call HookConsole.
  ConsoleElementID : String;
  // Style to use for console lines. If you change this, call initconsole
  ConsoleStyle : String;
  // Style to use for console lines. You can change this at any time.
  MaxConsoleLines : Integer;
  // Copy console lines on newline to browser console log
  ConsoleLinesToBrowserLog : Boolean;

// Clear console content
Procedure ResetConsole;
// Re-initialize console (style)
Procedure InitConsole;
// Re-hook console
Procedure HookConsole;

implementation


Var
  LastLine,
  StyleElement,
  LinesParent,
  ConsoleElement : TJSElement;


Procedure AppendLine;

Var
  CurrentCount : Integer;
  S : TJSNode;

begin
  CurrentCount:=0;
  S:=LinesParent.firstChild;
  While Assigned(S) do
    begin
    Inc(CurrentCount);
    S:=S.nextSibling;
    end;
  While CurrentCount>MaxConsoleLines do
    begin
    Dec(CurrentCount);
    LinesParent.removeChild(LinesParent.firstChild);
    end;
  LastLine:=Document.createElement('div');
  LastLine.className:='pasconsole';
  LinesParent.AppendChild(LastLine);
end;


Procedure WriteConsole(S : JSValue; NewLine : Boolean);

Var
  CL: String;

begin
  // Maybe add some way to limit line length
  CL:=LastLine.InnerText;
  CL:=CL+String(S);
  LastLine.InnerText:=CL;
  if NewLine then
    begin
    if ConsoleLinesToBrowserLog then
      console.log(CL);
    AppendLine;
    end;
end;

Procedure ResetConsole;


begin
  if LinesParent=Nil then exit;
  While LinesParent.firstElementChild<>Nil do
    LinesParent.removeChild(LinesParent.firstElementChild);
  AppendLine;
end;

Procedure InitConsole;

begin
  if ConsoleElement=Nil then
     exit;
  if (TJSString(ConsoleElement.nodeName).toLowerCase<>'body') then
     begin
     While ConsoleElement.firstElementChild<>Nil do
       ConsoleElement.removeChild(ConsoleElement.firstElementChild);
     end;
  StyleElement:=Document.createElement('style');
  StyleElement.innerText:=ConsoleStyle;
  ConsoleElement.appendChild(StyleElement);
  LinesParent:=Document.createElement('div');
  ConsoleElement.appendChild(LinesParent);
end;

Procedure HookConsole;

begin
  ConsoleElement:=Nil;
  if (ConsoleElementID<>'') then
    ConsoleElement:=document.getElementById(ConsoleElementID);
  if (ConsoleElement=Nil) then
    ConsoleElement:=document.body;
  if ConsoleElement=Nil then
    exit;
  InitConsole;
  ResetConsole;
  SetWriteCallBack(@WriteConsole);
end;

initialization
  ConsoleLinesToBrowserLog:=True;
  ConsoleElementID:='pasjsconsole';
  ConsoleStyle:=DefaultConsoleStyle;
  MaxConsoleLines:=DefaultMaxConsoleLines;
  HookConsole;
end.

