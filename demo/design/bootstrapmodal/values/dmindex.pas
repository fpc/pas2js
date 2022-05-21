unit dmindex;

{$mode ObjFPC}

interface

uses
  SysUtils, Classes, web, htmlfragment, bootstrapwidgets, Rtl.TemplateLoader,
  Rtl.HTMLActions;

type

  { ThfHello }

  ThfHello = class(THTMLFragment)
    bmHello: TBootstrapModal;
    alMain: THTMLElementActionList;
    actShowModal: THTMLElementAction;
    tlDialogs: TTemplateLoader;
    procedure actShowModalExecute(Sender: TObject; Event: TJSEvent);
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

procedure ThfHello.bmHelloHide(Sender: TObject; El: TJSHTMLElement;
  Values: TStrings);

Var
  aName,aFirstname,aLastName : String;

begin
  if Assigned(el) and SameText(el.id,'btnSave') then
    begin
    aFirstName:=Values.Values['FirstName'];
    aLastName:=Values.Values['LastName'];
    if (aFirstName='') and (aLastName='') then
      aName:='Anonymous person'
    else
      aName:=aFirstName+' '+aLastName;
    window.alert(aName+', you confirmed the dialog with the save button')
    end
  else
    window.alert('You canceled the dialog');

end;

procedure ThfHello.DataModuleHTMLLoaded(Sender: TObject);
begin
  alMain.Bind;
end;

end.

