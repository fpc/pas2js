program main;

{$mode objfpc}

uses js, web;

{$linklib ./modules/canvas.js canvas}
{$linklib ./modules/square.js square}

Type
  TCreateCanvasResult = record
    ctx : TJSCanvasRenderingContext2D;
    id : string;
  end;

Function create(aID : String; aParent : TJSElement; aWidth,aHeight : integer) : TCreateCanvasResult; external name 'canvas.create';
Function createReportList(aID : String) : string ; external name 'canvas.createReportList';

Type
  TDrawSquare = record
    length,x,y : NativeInt;
    color : string;
  end;

function draw(aCTX : TJSCanvasRenderingContext2D; aLength,aX,aY : NativeInt; aColor : String) : TDrawSquare; external name 'square.draw';
Function randomSquare (aCTX : TJSCanvasRenderingContext2D) : TDrawSquare; external name 'square.randomSquare';
procedure reportArea (aLength : NativeInt; aListID : string); external name 'square.reportArea';
procedure reportPerimeter (aLength : NativeInt; aListID : string); external name 'square.reportPerimeter';

var
  myCanvas : TCreateCanvasResult;
  reportList : String;
  square1,square2 : TDrawSquare;

begin
  myCanvas:=create('myCanvas', document.body, 480, 320);
  ReportList:= createReportList(myCanvas.id);
  square1:=draw(myCanvas.ctx, 50, 50, 100, 'blue');
  reportArea(square1.length, reportList);
  reportPerimeter(square1.length, reportList);
  square2:=randomSquare(myCanvas.ctx);
  reportArea(square2.length, reportList);
  reportPerimeter(square2.length, reportList);
end.
