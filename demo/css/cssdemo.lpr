program cssdemo;

uses js,web,main;

Type 
  TMainForm = Class(TBaseMainForm)
    Procedure doMinimize(Event : TJSEvent); override; async;
    Procedure doExtract(Event : TJSEvent); override; async;
    Function CreateURL(aPath : string) : string;
  end;
  
  
Function TMainForm.CreateURL(aPath : string) : string;

begin
  result:='fpcss.cgi/'+aPath;
end;
  
Procedure TMainForm.doMinimize(Event : TJSEvent); 

var
  Init : TJSObject;
  Resp: TJSResponse;
  CSS : String;
  
begin
  Init:=New(['method','POST','cache','no-cache','body',userCss.Value]);
  Resp:=await(TJSResponse,window.fetch(createurl('minimize'),Init));
  CSS:=await(string,Resp.text());
  processedCss.Value:=CSS;
end;


Procedure TMainForm.doExtract(Event : TJSEvent); 
var
  Init : TJSObject;
  Resp: TJSResponse;
  CSS : String;
  
begin
  Init:=New(['method','POST','cache','no-cache','body',userCss.Value]);
  Resp:=await(TJSResponse,window.fetch(createurl('classnames'),Init));
  CSS:=await(string,Resp.text());
  processedCss.Value:=CSS;
end;


begin
  TMainForm.Create(Nil);
end.  