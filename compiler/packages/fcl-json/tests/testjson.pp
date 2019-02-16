{
    This file is part of the Free Component Library

    JSON fpcunit tester program
    Copyright (c) 2007 by Michael Van Canneyt michael@freepascal.org

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
{$mode objfpc}
{$h+}
program testjson;

uses
  {$ifdef unix}
  cwstring,
  {$endif}
  Classes, testjsondata, testjsonparser, testjsonrtti, consoletestrunner, testjsonreader;

type
  { TLazTestRunner }
   TMyTestRunner = class(TTestRunner)
   protected
     // override the protected methods of TTestRunner to customize its behavior
   end;
      
var
  Application: TMyTestRunner;
begin
  DefaultFormat := fPlain;
  DefaultRunAllTests := True;
  Application := TMyTestRunner.Create(nil);
  Application.Initialize;
  Application.Run;  
  Application.Free;
end.
