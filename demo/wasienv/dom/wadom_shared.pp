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
  WasiDomResult_Null = 5;
  WasiDomResult_Boolean = 6;
  WasiDomResult_Number = 7;
  WasiDomResult_Double = 8;
  WasiDomResult_String = 9;
  WasiDomResult_Function = 10;
  WasiDomResult_Object = 11;
  WasiDomResult_BigInt = 12;
  WasiDomResult_Symbol = 13;

  WasiDomResultLast = 13;

  WasiDomResult_Names: array[0..WasiDomResultLast] of string = (
    'None',
    'Success',
    'UnknownObjId',
    'NotAFunction',
    'Undefined',
    'Null',
    'Boolean',
    'Number',
    'Double',
    'String',
    'Function',
    'Object',
    'BigInt',
    'Symbol'
    );

  WasiDomExportName = 'wasi_dom';
  WasiDomInvokeNoResult = 'invoke_noresult';
  WasiDomInvokeBooleanResult = 'invoke_boolresult';
  WasiDomInvokeDoubleResult = 'invoke_doubleresult';
  WasiDomInvokeObjectResult = 'invoke_objectresult';

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

  WasiObjIdBird = -5;

implementation

end.
