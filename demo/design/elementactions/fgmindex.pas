unit fgmIndex;

{$mode ObjFPC}

interface

uses
  SysUtils, Rtl.HTMLActions, htmlfragment, Web;

type

  { TIndexFragment }

  TIndexFragment = class(THTMLFragment)
    actbtnHello: THTMLElementAction;
    actedtFirstName: THTMLElementAction;
    actedtLastName: THTMLElementAction;
    alIndex: THTMLElementActionList;
    procedure actbtnHelloExecute(Sender: TObject; Event: TJSEvent);
    procedure DataModuleRendered(Sender: TObject);
  private

  public

  end;

var
  IndexFragment: TIndexFragment;

implementation

{$R *.lfm}

{ TIndexFragment }

procedure TIndexFragment.actbtnHelloExecute(Sender: TObject; Event: TJSEvent);

Var
  Msg : String;

begin
  Msg:='Hello, '+actedtFirstName.Value+' '+actedtLastName.Value+'!';
  window.alert(Msg);
end;

procedure TIndexFragment.DataModuleRendered(Sender: TObject);
begin
  alIndex.Bind;
end;

end.

