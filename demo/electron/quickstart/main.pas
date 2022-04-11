{
    This file is part of the Pas2JS run time library.
    Copyright (c) 2022- by the Pas2JS development team.

    Electron main process application.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

program main;

uses js, nodejs, node.fs, libelectron;


Procedure createWindow(event : TEvent; accessibilitySupportEnabled : boolean);

Var
  opts : TBrowserWindowConstructorOptions;
  win : TBrowserWindow;

begin
  opts:=TBrowserWindowConstructorOptions.new;
  opts.width:=800;
  opts.height:=600;
  opts.webPreferences:=TWebPreferences.New;
  opts.webPreferences.preload:=NJS_Path.join(__dirname,'preload.js');
  win:=Electron.TBrowserWindow.new(opts);
  win.loadFile('index.html');  
end;
  
begin
  electron.app.addListener('ready',@CreateWindow);
end.

