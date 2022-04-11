{
    This file is part of the Pas2JS run time library.
    Copyright (c) 2022- by the Pas2JS development team.

    Electron renderer process application, runs inside a browser context.
    Electron API is not available in this process

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

program render;

{$mode objfpc}

uses
  JS, Web;

Var
  el : TJSHTMLElement;

begin
  el:=Document.getHTMLElementById('renderertext');
  el.innerHTML:='This text was produced in the Electron Renderer process using Pas2JS version <b>'+{$i %PAS2JSVERSION%}+'</b>';
end.
