program htmlutilsdemo;

{$mode objfpc}
{$linklib ./htmlutils.js utils}

uses
  Web;

Procedure SetPageTitle(aTitle : String); external name 'utils.SetPageTitle';
Procedure ClearPage(aBelowID : String); external name 'utils.ClearPage';

var DefaultClearID : string; external name 'utils.vars.DefaultClearID';

Var
  BtnSetTitle,BtnClear : TJSHTMLButtonElement;
  edtTitle,edtBelowID,cbUseDefaultClearID : TJSHTMLInputElement;


function DoSetTitle(aEvent: TJSMouseEvent): boolean;
begin
  Result:=False;
  SetPageTitle(edtTitle.Value);
end;

function DoClear(aEvent: TJSMouseEvent): boolean;
begin
  Result:=False;
  if cbUseDefaultClearID.Checked then
    begin
    DefaultClearID:=edtBelowID.value;
    ClearPage('');
    end
  else
    begin
    DefaultClearID:='';
    ClearPage(edtBelowID.value);
    end;
end;

Procedure BindElements;

begin
  TJSElement(BtnSetTitle):=Document.getElementById('btnSetTitle');
  BtnSetTitle.OnClick:=@DoSetTitle;
  TJSElement(BtnClear):=Document.getElementById('btnClear');
  BtnClear.onclick:=@DoClear;
  TJSElement(edtTitle):=Document.getElementById('edtTitle');
  TJSElement(edtBelowID):=Document.getElementById('edtBelowID');
  TJSElement(cbUseDefaultClearID):=Document.getElementById('cbUseDefaultClearID');
end;

begin
  BindElements;
end.
