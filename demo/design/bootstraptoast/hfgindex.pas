unit hfgIndex;

{$mode ObjFPC}

interface

uses
  SysUtils, Classes, libbootstrap, Rtl.HTMLActions, htmlfragment, bootstrapwidgets, Web;

type

  { TfrgmIndex }

  TfrgmIndex = class(THTMLFragment)
    btwDemo: TBootstrapToastWidget;
    actShowToast: THTMLElementAction;
    alIndex: THTMLElementActionList;
    procedure actShowToastExecute(Sender: TObject; Event: TJSEvent);
  private

  public

  end;

var
  frgmIndex: TfrgmIndex;

implementation

{$R *.lfm}

{ TfrgmIndex }

procedure TfrgmIndex.actShowToastExecute(Sender: TObject;
  Event: TJSEvent);
begin
  btwdemo.UnrenderOnHide:=True;
  btwdemo.AutoHide:=True;
  btwdemo.HideDelay:=1500;
  btwDemo.Refresh;
end;

initialization
  Bootstrapversion:=bv5;
end.

