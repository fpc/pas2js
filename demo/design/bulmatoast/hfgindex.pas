unit hfgIndex;

{$mode ObjFPC}

interface

uses
  SysUtils, Classes, Rtl.HTMLActions, htmlfragment, bulmawidgets, Web;

type

  { TfrgmIndex }

  TfrgmIndex = class(THTMLFragment)
    btwDemo: TBulmaToastWidget;
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
  btwDemo.Refresh;
end;

end.

