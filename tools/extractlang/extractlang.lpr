{
    This file is part of the Pas2JS run time library.
    Copyright (c) 2019 by Michael Van Canneyt
    
    Program to extract data-translate tags from a HTML file.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
program extractlang;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}
  cwstring,
  {$ENDIF}
  Classes, SysUtils, CustApp, jsonparser, langextractor;

type

  { TExtractLangApplication }

  TExtractLangApplication = class(TCustomApplication)
  private
    procedure Logger({%H-}Sender: TObject; const Msg: String);
  protected
    FExtractor : THTMLLangExtractor;
    procedure DoRun; override;
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    procedure Usage(Const Msg : String); virtual;
  end;

{ TExtractLangApplication }

procedure TExtractLangApplication.Logger(Sender: TObject; const Msg: String);
begin
  Writeln(Msg);
end;

procedure TExtractLangApplication.DoRun;
var
  ErrorMsg: String;
begin
  Terminate;
  ErrorMsg:=CheckOptions('cd:f:hl:mn:o:ts:r', ['clear','file-mode','help','html-dir','languages','minify','name','output','recurse','single-scope','trash-values']);
  if (ErrorMsg<>'') or HasOption('h','help') then
    begin
    Usage(ErrorMsg);
    exit;
    end;
  With FExtractor do
    begin
    OnLog:=@Logger;
    HTMLDir:=GetOptionValue('d','html-dir');
    OutputFileName:=GetOptionValue('o','output');
    Languages:=GetOptionValue('l','languages');
    Minified:=HasOption('m','minify');
    TrashNewValues:=HasOption('t','trash-values');
    SingleScope:=GetOptionValue('s','single-scope');
    CleanOutput:=HasOption('c','clear');
    Recurse:=HasOption('r','recurse');
    TagName:=GetOptionValue('n','name');
    if (HTMLDir='') or (OutputFileName='') then
      Usage('Need input dir and output filename');
    if HasOption('f','file-mode') then
      Case LowerCase(GetOptionValue('f','file-mode')) of
       'single':
          OutputFileMode:=fmSingle;
       'multiple',
       'multi':
          OutputFileMode:=fmMultiple;
      else
        OutputFileMode:=fmSingle;
      end;
    TrashNewValues:=HasOption('t','trash-values');
    Execute;
    end;
end;

constructor TExtractLangApplication.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  StopOnException:=True;
  FExtractor:=THTMLLangExtractor.Create(Self);
end;

destructor TExtractLangApplication.Destroy;
begin
  FreeAndNil(FExtractor);
  inherited Destroy;
end;

procedure TExtractLangApplication.Usage(const Msg: String);
begin
  if Msg<>'' then
     Writeln('Error : ',Msg);
  Writeln('Usage: ', ExeName, ' [options]');
  Writeln('Where options is one or more of:');
  Writeln('-h --help                   This help text');
  Writeln('-c --clear                  Clear output JSON file (Default is to update existing output file).');
  Writeln('-d --html-dir=DIR           Directory with HTML files to scan (recursively)');
  Writeln('-f --file-mode=MODE         Set file mode: one of single or multiple');
  Writeln('-o --output=FILE            File to write JSON translations (may get suffix depending on file mode)');
  Writeln('-l --languages=LIST         Comma-separated list of languages to create');
  Writeln('-m --minify                 Minify output');
  Writeln('-n --name=NAME              Set name of data-tag to NAME (data-NAME)');
  Writeln('-r --recurse                Recurse into subdirectories of the HTML directory');
  Writeln('-s --single-scope=SCOPE     Put all translation names in a single scope');
  Writeln('-t --trash-values           Trash values for other languages');
  ExitCode:=Ord(Msg<>'');
  Halt;
end;

var
  Application: TExtractLangApplication;

begin
  Application:=TExtractLangApplication.Create(nil);
  Application.Title:='Extract data-translate tag application';
  Application.Run;
  Application.Free;
end.

