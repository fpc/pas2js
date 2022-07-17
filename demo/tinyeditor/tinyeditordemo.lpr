program tinyeditordemo;

{$mode objfpc}

uses
  BrowserApp, JS, Classes, SysUtils, Web, libtinyeditor;

type

  { TMyApplication }

  TMyApplication = class(TBrowserApplication)
  private
    function DoShowHTML(aEvent: TJSEvent): boolean;
  protected
    divHTMLContent : TJSHTMLElement;
    divEditor : TJSHTMLElement;
    btnShowHTML : TJSHTMLButtonElement;
    procedure DoRun; override;
  public
  end;

function TMyApplication.DoShowHTML(aEvent: TJSEvent): boolean;
begin
  divHTMLContent.InnerHtml:=divEditor.InnerHTML;
end;

procedure TMyApplication.DoRun;
begin
  divHTMLContent:=GetHTMLElement('divHTMLContent');
  divEditor:=GetHTMLElement('editor');
  btnShowHTML:=TJSHTMLButtonElement(GetHTMLElement('btnShowHTML'));
  btnShowHTML.addEventListener('click',@DoShowHTML);
  tinyEditor.transformToEditor(divEditor);
end;

var
  Application : TMyApplication;

begin
  Application:=TMyApplication.Create(nil);
  Application.Initialize;
  Application.Run;
end.
