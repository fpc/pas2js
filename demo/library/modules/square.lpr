library square;

{$mode objfpc}

uses
  SysUtils,JS, Web;

Type
  TDrawSquare = record
    length,x,y : NativeInt;
    color : string;
  end;

function draw(aCTX : TJSCanvasRenderingContext2D; aLength,aX,aY : NativeInt; aColor : String) : TDrawSquare;

begin
  aCtx.fillStyle:=aColor;
  aCtx.fillRect(aX, aY, aLength, aLength);
  Result.length:=alength;
  Result.x:=aX;
  Result.y:=aY;
  Result.color:=aColor;
end;

Function randomSquare (aCTX : TJSCanvasRenderingContext2D) : TDrawSquare;

var
  x,y,l : Integer;
  col : string;

begin
  Col:=format('rgb(%d,%d,%d)',[Random(256),Random(256),Random(256)]);
  X:=Random(481);
  Y:=Random(320);
  L:=10+Random(9);
  Result:=Draw(aCtx,l,x,y,col);
end;

procedure reportArea (aLength : NativeInt; aListID : string);

Var
  aItem,aList : TJSHTMLElement;

begin
  aItem:=TJSHTMLElement(document.createElement('li'));
  aItem.textContent:=Format('Square area is %dpx squared.',[aLength*aLength]);
  aList:=TJSHTMLElement(document.getElementById(aListID));
  alist.appendChild(aItem);
end;

procedure reportPerimeter (aLength : NativeInt; aListID : string);

Var
  aItem,aList : TJSHTMLElement;

begin
  aItem:=TJSHTMLElement(document.createElement('li'));
  aItem.textContent:=Format('Square perimeter is %dpx.',[aLength*4]);
  aList:=TJSHTMLElement(document.getElementById(aListID));
  alist.appendChild(aItem);
end;

exports
  draw,
  randomSquare,
  reportArea,
  reportPerimeter;

begin
  // Your code here
end.
