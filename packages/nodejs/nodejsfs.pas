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

var
  DirectorySeparator: char = '/';
  DriveSeparator: string = '';
  ExtensionSeparator: char = '.';
  PathSeparator: char = ':';
  AllowDirectorySeparators: set of char = ['\','/'];
  AllowDriveSeparators: set of char = [];
  AllFilesMask: string = '*';
  //MaxPathLen: integer = 4096;

  PathDelim: char = '/'; // = DirectorySeparator;
  DriveDelim: string = ''; // = DriveSeparator;
  PathSep: char = ':'; // = PathSeparator;
  //MAX_PATH: integer = 4096; // = MaxPathLen;

const
  //faReadOnly      =  1;
  //faHidden        =  2;
  //faSystem        =  4;
  //faReserve       =  8;
  faDirectory     = 16;
  //faArchive       = 32;

function FileExists(Filename: string): boolean;
function DirectoryExists(Filename: string): boolean;
function ExtractFilePath(Filename: string): string;
function ExtractFileName(Filename: string): string;
function ExtractFileExt(Filename: string): string;
function SetDirSeparators(Filename: string): string;
function ExpandFileName(Filename: string): string;
function IncludeTrailingPathDelimiter(Filename: string): string;
function ChangeFileExt(Filename, NewExt: string): string;

implementation

function FileExists(Filename: string): boolean;
begin
  writeln('FileExists TODO ',Filename);
  Result:=false; // ToDo
  if Filename='' then ;
  raise Exception.Create('FileExists TODO');
end;

function DirectoryExists(Filename: string): boolean;
begin
  writeln('DirectoryExists TODO ',Filename);
  Result:=false; // ToDo
  if Filename='' then ;
  raise Exception.Create('DirectoryExists TODO');
end;

function ExtractFilePath(Filename: string): string;
begin
  writeln('ExtractFilePath TODO ',Filename);
  Result:=''; // ToDo
  if Filename='' then ;
  raise Exception.Create('ExtractFilePath TODO');
end;

function ExtractFileName(Filename: string): string;
begin
  writeln('ExtractFileName TODO ',Filename);
  Result:=''; // ToDo
  if Filename='' then ;
  raise Exception.Create('ExtractFileName TODO');
end;

function ExtractFileExt(Filename: string): string;
begin
  writeln('ExtractFileExt TODO ',Filename);
  Result:=''; // ToDo
  if Filename='' then ;
  raise Exception.Create('ExtractFileExt TODO');
end;

function SetDirSeparators(Filename: string): string;
begin
  writeln('SetDirSeparators  TODO ',Filename);
  Result:=''; // ToDo
  if Filename='' then ;
  raise Exception.Create('SetDirSeparators TODO');
end;

function ExpandFileName(Filename: string): string;
begin
  writeln('ExpandFileName TODO ',Filename);
  Result:=''; // ToDo
  if Filename='' then ;
  raise Exception.Create('ExpandFileName TODO');
end;

function IncludeTrailingPathDelimiter(Filename: string): string;
begin
  writeln('IncludeTrailingPathDelimiter TODO ',Filename);
  Result:=''; // ToDo
  if Filename='' then ;
  raise Exception.Create('IncludeTrailingPathDelimiter TODO');
end;

function ChangeFileExt(Filename, NewExt: string): string;
begin
  writeln('ChangeFileExt TODO ',Filename,' NewExt=',NewExt);
  Result:=''; // ToDo
  if Filename='' then ;
  if NewExt='' then ;
  raise Exception.Create('ChangeFileExt TODO');
end;

end.

