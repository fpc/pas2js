unit dmindex;

{$mode ObjFPC}

interface

uses
  SysUtils, Classes, web, htmlfragment, bulmawidgets, Rtl.TemplateLoader,
  Rtl.HTMLActions;

type

  { ThfHello }

  ThfHello = class(THTMLFragment)
    bmHello: TBulmaModal;
    alMain: THTMLElementActionList;
    actShowModal: THTMLElementAction;
    tlDialogs: TTemplateLoader;
    procedure actShowModalExecute(Sender: TObject; Event: TJSEvent);
    procedure alMainExecute(Sender: TObject; Event: TJSEvent;
      var Handled: Boolean);
    procedure bmHelloHide(Sender: TObject; El: TJSHTMLElement; Values: TStrings
      );
    procedure DataModuleHTMLLoaded(Sender: TObject);
  private
  public

  end;

var
  hfHello: ThfHello;

implementation

{$R *.lfm}

{ ThfHello }


procedure ThfHello.actShowModalExecute(Sender: TObject; Event: TJSEvent);
begin
  bmHello.show;
end;

procedure ThfHello.alMainExecute(Sender: TObject; Event: TJSEvent;
  var Handled: Boolean);
begin

end;

procedure ThfHello.bmHelloHide(Sender: TObject; El: TJSHTMLElement;
  Values: TStrings);
begin
  if Assigned(el) and SameText(el.id,'btnSave') then
    window.alert('You confirmed the dialog with the save button')
  else
    window.alert('You canceled the dialog');

end;

procedure ThfHello.DataModuleHTMLLoaded(Sender: TObject);
begin
  alMain.Bind;
end;

end.

