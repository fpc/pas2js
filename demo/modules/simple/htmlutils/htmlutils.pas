library htmlutils;

uses
  web;

Var
  DefaultClearID : String;

Procedure SetPageTitle(aTitle : String);

begin
  Document.Title:=aTitle;
end;

Procedure ClearPage(aBelowID : String);

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

exports
  DefaultClearID,
  SetPageTitle,
  ClearPage;

begin
  // Your library initialization code here
end.
