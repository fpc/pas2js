program demodatatables;

{$mode objfpc}

uses
  browserapp, JS, Classes, SysUtils, Web, libjquery, libdatatables;

type
  TMyApplication = class(TBrowserApplication)
    procedure doRun; override;
  end;

procedure TMyApplication.doRun;

Var
  Opts : TJSDataTableOptions;
  Lang : TJSDTOLanguageOptions;
  LangPag : TJSDTOLanguagePaginateOptions;
  checkCol,cCol : TJSDTOColumn;

begin
  Opts:=TJSDataTableOptions.New;
  opts.Select:=True;
  Lang:=TJSDTOLanguageOptions.new;
  LangPag:=TJSDTOLanguagePaginateOptions.New;
  // These are ignored because of the lang.url ?
  LangPag.previous:='<i class="fas fa-lg fa-angle-left"></i>';
  LangPag.Next:='<i class="fas fa-lg fa-angle-right"></i>';
  Lang.paginate:=LangPag;
  opts.Language:=Lang;
  cCol:=TJSDTOColumn.New;
  cCol.DataInt:=-1;
  checkCol:=TJSDTOColumn.New;
  checkCol.orderable:=false;
  checkCol.className:='select-checkbox';
  checkCol.DefaultContent:='';
  checkCol.targets:=0;
 { Opts.columns:=[
    cCol, // Fake, data = -1
    Nil,
    Nil,
    Nil,
    Nil,
    Nil,
    Nil
  ];}
  Opts.columnDefs:=[CheckCol];
  opts.SelectObj:=TJSDTOSelectOptions.new;
  opts.SelectObj.style:='os';
  opts.SelectObj.selector:='td:first-child';
  opts.order:=[[1,'asc']];
  opts.responsive:=False;
  JQuery('#example').dataTableAPi(opts);
  Terminate;
end;

var
  Application : TMyApplication;

begin
  Application:=TMyApplication.Create(nil);
  Application.Initialize;
  Application.Run;
  Application.Free;
end.
