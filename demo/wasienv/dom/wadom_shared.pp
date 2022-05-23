{
  These types and constants are shared between pas2js and webassembly.
}
unit wadom_shared;

interface

type
  TWasiDomObjectID = NativeInt;

// invoke results
type
  TWasiDomResult = longint;
const
  WasiDomResult_None = 0;
  WasiDomResult_Success = 1;
  WasiDomResult_UnknownObjId = 2;
  WasiDomResult_NotAFunction = 3;
  WasiDomResult_Undefined = 4;
  WasiDomResult_Boolean = 5;
  WasiDomResult_Number = 6;
  WasiDomResult_Double = 7;
  WasiDomResult_String = 8;
  WasiDomResult_Function = 9;
  WasiDomResult_Object = 10;
  WasiDomResult_BigInt = 11;
  WasiDomResult_Symbol = 12;

  WasiDomResultLast = 12;

  WasiDomResult_Names: array[0..WasiDomResultLast] of string = (
    'None',
    'Success',
    'UnknownObjId',
    'NotAFunction',
    'Undefined',
    'Null',
    'Boolean',
    'Double',
    'String',
    'Function',
    'Object',
    'BigInt',
    'Symbol'
    );

  WasiDomExtName = 'wasi_dom';
  WasiDomInvokeBooleanResult = 'invoke_boolresult';
  WasiDomInvokeDoubleResult = 'invoke_doubleresult';

  WasiArgNone = 0;
  WasiArgLongint = 1;
  WasiArgDouble = 2;
  WasiArgTrue = 3;
  WasiArgFalse = 4;
  WasiArgChar = 5; // followed by a word
  WasiArgUTF8String = 6; // followed by length and pointer
  WasiArgUnicodeString = 7; // followed by length and pointer
  WasiArgPointer = 8;

  WasiObjIdDocument = -1;
  WasiObjIdWindow = -2;
  WasiObjIdConsole = -3;
  WasiObjIdCaches = -4;

implementation

end.
