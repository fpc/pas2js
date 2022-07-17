unit libtinyeditor;

{$mode objfpc}
{$modeswitch externalclass}

interface

Uses JS, Web;

Type
  TTinyEditor = class external name 'Object' (TJSObject)
  Public
    procedure transformToEditor(aElement : TJSHTMLElement);
  end;  

var
  tinyEditor : TTinyEditor; external name '__tinyEditor';

Implementation
 
end.
