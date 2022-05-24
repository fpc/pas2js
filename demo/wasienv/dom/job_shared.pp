{
  JOB - JS Object Bridge for Webassembly

  These types and constants are shared between pas2js and webassembly.
}
unit JOB_Shared;

interface

type
  TJOBObjectID = NativeInt;

// invoke results
type
  TJOBResult = longint;
const
  JOBResult_None = 0;
  JOBResult_Success = 1;
  JOBResult_UnknownObjId = 2;
  JOBResult_NotAFunction = 3;
  JOBResult_Undefined = 4;
  JOBResult_Null = 5;
  JOBResult_Boolean = 6;
  JOBResult_Number = 7;
  JOBResult_Double = 8;
  JOBResult_String = 9;
  JOBResult_Function = 10;
  JOBResult_Object = 11;
  JOBResult_BigInt = 12;
  JOBResult_Symbol = 13;

  JOBResultLast = 13;

  JOBResult_Names: array[0..JOBResultLast] of string = (
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

  JOBExportName = 'job';
  JOBFn_InvokeNoResult = 'invoke_noresult';
  JOBFn_InvokeBooleanResult = 'invoke_boolresult';
  JOBFn_InvokeDoubleResult = 'invoke_doubleresult';
  JOBFn_InvokeStringResult = 'invoke_stringresult';
  JOBFn_GetStringResult = 'get_stringresult';
  JOBFn_ReleaseStringResult = 'release_stringresult';
  JOBFn_InvokeObjectResult = 'invoke_objectresult';
  JOBFn_ReleaseObject = 'release_object';

  JOBArgNone = 0;
  JOBArgLongint = 1;
  JOBArgDouble = 2;
  JOBArgTrue = 3;
  JOBArgFalse = 4;
  JOBArgChar = 5; // followed by a word
  JOBArgUTF8String = 6; // followed by length and pointer
  JOBArgUnicodeString = 7; // followed by length and pointer
  JOBArgPointer = 8;

  JOBObjIdDocument = -1;
  JOBObjIdWindow = -2;
  JOBObjIdConsole = -3;
  JOBObjIdCaches = -4;

  WasiObjIdBird = -5;

implementation

end.
