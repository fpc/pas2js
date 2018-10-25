{
    This file is part of the Pas2JS run time library.
    Copyright (c) 2018 by Mattias Gaertner

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
unit NodeJSFS;

{$mode objfpc}
{$ModeSwitch externalclass}

interface

uses
  JS, SysUtils;

function FileExists(Filename: string): boolean;
function ExtractFilePath(Filename: string): string;
function ExtractFileName(Filename: string): string;
function SetDirSeparators(Filename: string): string;
function ExpandFileName(Filename: string): string;
function IncludeTrailingPathDelimiter(Filename: string): string;
function ChangeFileExt(Filename, NewExt: string): string;

implementation

function FileExists(Filename: string): boolean;
begin
  Result:=false; // ToDo
  if Filename='' then ;
end;

function ExtractFilePath(Filename: string): string;
begin
  Result:=''; // ToDo
  if Filename='' then ;
end;

function ExtractFileName(Filename: string): string;
begin
  Result:=''; // ToDo
  if Filename='' then ;
end;

function SetDirSeparators(Filename: string): string;
begin
  Result:=''; // ToDo
  if Filename='' then ;
end;

function ExpandFileName(Filename: string): string;
begin
  Result:=''; // ToDo
  if Filename='' then ;
end;

function IncludeTrailingPathDelimiter(Filename: string): string;
begin
  Result:=''; // ToDo
  if Filename='' then ;
end;

function ChangeFileExt(Filename, NewExt: string): string;
begin
  Result:=''; // ToDo
  if Filename='' then ;
  if NewExt='' then ;
end;

end.

