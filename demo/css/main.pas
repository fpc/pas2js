unit main;
{$MODE ObjFPC}
{$H+}

interface

uses js, web,Classes;

Type
  
  { --------------------------------------------------------------------
    TBaseMainForm
    --------------------------------------------------------------------}
  
  TBaseMainForm = class(TComponent) 
  Published
    navbarNav : TJSHTMLElement;
    navbarScrollingDropdown : TJSHTMLElement;
    userCss : TJSHTMLTextAreaElement;
    btnMinimize : TJSHTMLButtonElement;
    btnClassNames : TJSHTMLButtonElement;
    processedCss : TJSHTMLTextAreaElement;
    Procedure doMinimize(Event : TJSEvent); async; virtual; abstract;
    Procedure doExtract(Event : TJSEvent); async; virtual; abstract;
  Public
    Constructor Create(aOwner : TComponent); override;
    Procedure BindElements; virtual;
    Procedure BindElementEvents; virtual;
  end;

implementation


{ --------------------------------------------------------------------
  TBaseMainForm
  --------------------------------------------------------------------}


Constructor TBaseMainForm.create(aOwner : TComponent);

begin
  Inherited;
  BindElements;
  BindElementEvents;
end;



Procedure TBaseMainForm.BindElements;

begin
  navbarNav:=TJSHTMLElement(document.getelementByID('navbarNav'));
  navbarScrollingDropdown:=TJSHTMLElement(document.getelementByID('navbarScrollingDropdown'));
  userCss:=TJSHTMLTextAreaElement(document.getelementByID('userCss'));
  btnMinimize:=TJSHTMLButtonElement(document.getelementByID('btnMinimize'));
  btnClassNames:=TJSHTMLButtonElement(document.getelementByID('btnClassNames'));
  processedCss:=TJSHTMLTextAreaElement(document.getelementByID('processedCss'));
end;

Procedure TBaseMainForm.BindElementEvents;

begin
  btnMinimize.AddEventListener('click',@doMinimize);
  btnClassNames.AddEventListener('click',@doExtract);
end;

end.
