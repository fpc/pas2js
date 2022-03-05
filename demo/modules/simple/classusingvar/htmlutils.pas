library htmlutils;

uses
  web;

Type
   THTMLUtils = class(TObject)
   Public
     DefaultClearID : String;
     Procedure SetPageTitle(aTitle : String);
     Procedure ClearPage(aBelowID : String);
   end;

Procedure THTMLUtils.SetPageTitle(aTitle : String);

begin
  Document.Title:=aTitle;
end;

Procedure THTMLUtils.ClearPage(aBelowID : String);

Var
  EL : TJSElement;

begin
  if (aBelowID='') then
    aBelowID:=DefaultClearID;
  if (aBelowID='') then
    el:=Document.body
  else
    el:=Document.getElementById(aBelowID);
  if Assigned(El) then
    El.innerHTML:='';
end;

var
  Utils : THTMLUtils;


exports
  Utils;

initialization
  Utils:=THTMLUtils.Create;

end.
