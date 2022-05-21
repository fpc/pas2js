program demohtmlfragment;

{$mode objfpc}

uses
  browserapp, JS, Classes, SysUtils, Web, htmlfragment;

type
  TMyHTMLFragment = Class(THTMLFragment)
  end;

  { TMyApplication }

  TMyApplication = class(TBrowserApplication)
    FFragment : TMyHTMLFragment;
    FBtnShow : TJSHTMLButtonElement;
    FBtnHide : TJSHTMLButtonElement;
    procedure doRun; override;
  private
    procedure BindElements;
    procedure CreateFragment;
    function DoHideClick(aEvent: TJSMouseEvent): boolean;
    procedure DoHTMLLoaded(Sender: TObject);
    procedure DoRendered(Sender: TObject);
    function DoShowClick(aEvent: TJSMouseEvent): boolean;
    procedure DoUnRendered(Sender: TObject);
  end;

procedure TMyApplication.BindElements;

begin
  FBtnShow:=TJSHTMLButtonElement(GetHTMLElement('btnShow'));
  FBtnShow.OnClick:=@DoShowClick;
  FBtnHide:=TJSHTMLButtonElement(GetHTMLElement('btnHide'));
  FBtnHide.OnClick:=@DoHideClick;
end;

Procedure TMyApplication.CreateFragment;

begin
  FFragment:=TMyHTMLFragment.CreateNew(Self);
  FFragment.Name:='login';
  FFragment.ParentID:='form-parent';
  FFragment.HTMLFileName:='login.html';
  FFragment.OnHTMLLoaded:=@DoHTMLLoaded;
  FFragment.OnRendered:=@DoRendered;
  FFragment.OnUnRendered:=@DoUnRendered;
end;

function TMyApplication.DoHideClick(aEvent: TJSMouseEvent): boolean;
begin
  FFragment.Hide;
end;

procedure TMyApplication.DoHTMLLoaded(Sender: TObject);
begin
  FBtnShow.classList.Remove('is-primary');
  FBtnShow.classList.Add('is-info');
end;

procedure TMyApplication.DoRendered(Sender: TObject);
begin
  FbtnShow.disabled:=True;
  FbtnHide.disabled:=False;
end;

function TMyApplication.DoShowClick(aEvent: TJSMouseEvent): boolean;
begin
  if not Assigned(FFragment) then
    CreateFragment;
  FFragment.Show;
end;

procedure TMyApplication.DoUnRendered(Sender: TObject);
begin
  FbtnShow.disabled:=False;
  FbtnHide.disabled:=True;
end;

procedure TMyApplication.doRun;


begin
  Terminate;
  BindElements;

end;

var
  Application : TMyApplication;

begin
  Application:=TMyApplication.Create(nil);
  Application.Initialize;
  Application.Run;
end.
