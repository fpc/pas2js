{
    This file is part of the Pas2JS run time library.
    Copyright (c) 2022- by the Pas2JS development team.

    Electron renderer-preload process application.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
program preload;

{$mode objfpc}

uses
  JS, Classes, SysUtils, Web, libelectron, nodejs;

procedure DoRun(event : TJSEvent);

  Procedure ReplaceText(const aSelector, aText : String);

  Var
    el : TJSHTMLElement;

  begin
    el:=TJSHTMLElement(Document.getElementById(aSelector));
    if Assigned(el) then
      el.InnerText:=aText;
  end;

begin
  ReplaceText('pas2js-version',{$i %PAS2JSVERSION%});
  ReplaceText('chrome-version',String(TNJSProcess.versions['chrome']));
  ReplaceText('electron-version',String(TNJSProcess.versions['electron']));
  ReplaceText('node-version',String(TNJSProcess.versions['node']));
end;

begin
  console.log('preload environment start');
  console.log(electron);
  console.log('preload environment done');
  window.addEventListener('DOMContentLoaded',@DoRun);
end.
