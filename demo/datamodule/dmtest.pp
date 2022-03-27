unit dmtest;

{$mode ObjFPC}

interface

uses
  Classes, SysUtils, htmlactions, Web;



type
  { TTestModule }

  TTestModule = class(TDataModule)
    actbtnSetTitle_2: THTMLElementAction;
    acthdrTitle_2: THTMLElementAction;
    actSetTitle: THTMLElementAction;
    alHTML: THTMLElementActionList;
    actTitle: THTMLElementAction;
    procedure actSetTitleExecute(Sender: TObject; Event: TJSEvent);
  private

  public
    Procedure DoSomething;
  end;

var
  TestModule: TTestModule;

implementation

{$R *.lfm}

{ TTestModule }

procedure TTestModule.actSetTitleExecute(Sender: TObject; Event: TJSEvent);
begin
  ActTitle.Element.innerHTML:='Title is now set';
end;

procedure TTestModule.DoSomething;
begin
  Writeln('DoSomething');
end;

end.

