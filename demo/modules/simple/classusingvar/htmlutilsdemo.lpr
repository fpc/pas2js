program htmlutilsdemo;

{$mode objfpc}
{$linklib ./htmlutils.js utils}
{$modeswitch externalclass}

uses
  JS, Web;

type
  THTMLUtils = class external name 'Object' (TJSObject)
  Public
    DefaultClearID : String;
    Procedure SetPageTitle(aTitle : String);
    Procedure ClearPage(aBelowID : String);
  end;


Var
  BtnSetTitle,BtnClear : TJSHTMLButtonElement;
  edtTitle,edtBelowID,cbUseDefaultClearID : TJSHTMLInputElement;
  UtilsObj : THTMLUtils; external name 'utils.vars.Utils';

function DoSetTitle(aEvent: TJSMouseEvent): boolean;
begin
  Result:=False;
  UtilsObj.SetPageTitle(edtTitle.Value);
end;

function DoClear(aEvent: TJSMouseEvent): boolean;
begin
  Result:=False;
  if cbUseDefaultClearID.Checked then
    begin
    UtilsObj.DefaultClearID:=edtBelowID.value;
    UtilsObj.ClearPage('');
    end
  else
    begin
    UtilsObj.DefaultClearID:='';
    UtilsObj.ClearPage(edtBelowID.value);
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
