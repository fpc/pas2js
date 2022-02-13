unit uCanvas;

{$mode ObjFPC}

interface

uses
  web;

Type
  TCreateCanvasResult = record
    ctx : TJSCanvasRenderingContext2D;
    id : string;
  end;

Function create(aID : String; aParent : TJSElement; aWidth,aHeight : integer) : TCreateCanvasResult;
Function createReportList(aID : String) : string ;


Implementation

Function create(aID : String; aParent : TJSElement; aWidth,aHeight : integer) : TCreateCanvasResult;

Var
  divWrapper : TJSHTMLElement;
  canvasElem : TJSHTMLCanvasElement;

begin
  divWrapper:=TJSHTMLElement(document.createElement('div'));
  canvasElem:=TJSHTMLCanvasElement(document.createElement('canvas'));
  aParent.appendChild(divWrapper);
  divWrapper.appendChild(canvasElem);

  divWrapper.id:=aid;
  canvasElem.width := awidth;
  canvasElem.height := aheight;
  Result.ctx:=TJSCanvasRenderingContext2D(canvasElem.getContext('2d'));
  Result.ID:=aID;
end;

Function createReportList(aID : String) : string ;

Var
  aWrapper,aList : TJSHTMLElement;

begin
   alist:=TJSHTMLElement(document.createElement('ul'));
   alist.id:=aId + '-reporter';
   aWrapper:=TJSHTMLElement(document.getElementById(aId));
   aWrapper.appendChild(aList);
   Result:=aList.id;
end;

end.

