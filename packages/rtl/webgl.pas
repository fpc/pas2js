unit webgl;

{$mode objfpc}
{$modeswitch externalclass}

interface

uses JS, Web;

Type
  GLenum = Cardinal;
  GLboolean = Boolean;
  GLbitfield = Cardinal;
  GLbyte = ShortInt;         (* 'byte' should be a signed 8 bit type. *)
  GLshort = Integer;
  GLint = Integer;
  GLsizei = Integer;
  GLintptr = NativeInt;
  GLsizeiptr = NativeInt;
// Ideally the typedef below would use 'unsigned byte', but that doesn't currently exist in Web IDL.
  GLubyte = Byte;        (* 'octet' should be an unsigned 8 bit type. *)
  GLushort = Word;
  GLuint = Cardinal;
  GLfloat = Double;
  GLclampf = Double;

  TJSWebGLPowerPreference = String;
// "default", "low-power", "high-performance"


  TJSWebGLContextAttributes  = Class external name 'WebGLContextAttributes' (TJSObject)
    alpha :GLboolean;
    depth : GLboolean;
    stencil :  GLboolean;
    antialias : GLboolean;
    premultipliedAlpha : GLboolean;
    preserveDrawingBuffer : GLboolean;
    powerPreference  : string;
    failIfMajorPerformanceCaveat : GLBoolean;
  end;

  TJSWebGLObject = class external name 'WebGLObject' (TJSObject);
  TJSWebGLBuffer = Class external name 'WebGLBuffer' (TJSWebGLObject);
  TJSWebGLFramebuffer = Class external name 'WebGLFramebuffer' (TJSWebGLObject);
  TJSWebGLProgram = Class external name 'WebGLProgram' (TJSWebGLObject);
  TJSWebGLRenderbuffer = Class external name 'WebGLRenderbuffer' (TJSWebGLObject);
  TJSWebGLShader = Class external name 'WebGLShader' (TJSWebGLObject);
  TJSWebGLTexture = Class external name 'WebGLTexture' (TJSWebGLObject);
  TJSWebGLUniformLocation = class external name 'WebGLUniformLocation' (TJSWebGLObject);

  TJSWebGLActiveInfo = Class external name 'WebGLActiveInfo' (TJSOBject)
  Private
    Fsize : GLint; external name 'size';
    Ftype : GLenum; external name 'type';
    Fname : String; external name 'name';
  Public
    Property size : GLint read Fsize;
    Property &type : GLenum read Ftype;
    Property name : String read Fname;
  end;

  TJSWebGLShaderPrecisionFormat = Class external name 'WebGLShaderPrecisionFormat' (TJSObject)
  Private
    FrangeMin : GLint; external name 'rangeMin';
    FrangeMax : GLint; external name 'rangeMax';
    Fprecision : GLint; external name 'precision';
  Public
    Property rangeMin : GLint read FrangeMin;
    Property rangeMax : GLint read FrangeMax;
    Property precision : GLint read Fprecision;
  end;

{
 typedef (ImageBitmap or
         ImageData or
         HTMLImageElement or
         HTMLCanvasElement or
         HTMLVideoElement) 
}

  TJSArrayBufferView = TJSTypedArray;
  TJSTexImageSource = JSValue;
  TJSFloat32List = Array of GLFloat;
  TJSInt32List = Array of integer;

  TJSWebGLShaderArray = array of TJSWebGLShader;
  TJSWebGLRenderingContextBase = Class External name 'WebGLRenderingContextBase' (TJSObject)

  Public

  (* ClearBufferMask *)
  const
    DEPTH_BUFFER_BIT : Nativeint;
    STENCIL_BUFFER_BIT : NativeInt;
    COLOR_BUFFER_BIT : NativeInt;

   (* BeginMode *)
    POINTS      : NativeInt;
    LINES       : NativeInt;
    LINE_LOOP      : NativeInt;
    LINE_STRIP      : NativeInt;
    TRIANGLES      : NativeInt;
    TRIANGLE_STRIP     : NativeInt;
    TRIANGLE_FAN     : NativeInt;

   (* AlphaFunction (not supported in ES20) *)
   (*  NEVER *)
   (*  LESS *)
   (*  EQUAL *)
   (*  LEQUAL *)
   (*  GREATER *)
   (*  NOTEQUAL *)
   (*  GEQUAL *)
   (*  ALWAYS *)

   (* BlendingFactorDest *)
    ZERO       : NativeInt;
    ONE       : NativeInt;
    SRC_COLOR      : NativeInt;
    ONE_MINUS_SRC_COLOR   : NativeInt;
    SRC_ALPHA      : NativeInt;
    ONE_MINUS_SRC_ALPHA   : NativeInt;
    DST_ALPHA      : NativeInt;
    ONE_MINUS_DST_ALPHA   : NativeInt;

   (* BlendingFactorSrc *)
   (*  ZERO *)
   (*  ONE *)
    DST_COLOR      : NativeInt;
    ONE_MINUS_DST_COLOR   : NativeInt;
    SRC_ALPHA_SATURATE    : NativeInt;
   (*  SRC_ALPHA *)
   (*  ONE_MINUS_SRC_ALPHA *)
   (*  DST_ALPHA *)
   (*  ONE_MINUS_DST_ALPHA *)

   (* BlendEquationSeparate *)
    FUNC_ADD      : NativeInt;
    BLEND_EQUATION     : NativeInt;
    BLEND_EQUATION_RGB   : Nativeint; (* same as BLEND_EQUATION *)
    BLEND_EQUATION_ALPHA   : NativeInt;

   (* BlendSubtract *)
    FUNC_SUBTRACT     : NativeInt;
    FUNC_REVERSE_SUBTRACT   : NativeInt;

   (* Separate Blend Functions *)
    BLEND_DST_RGB     : NativeInt;
    BLEND_SRC_RGB     : NativeInt;
    BLEND_DST_ALPHA    : NativeInt;
    BLEND_SRC_ALPHA    : NativeInt;
    CONSTANT_COLOR     : NativeInt;
    ONE_MINUS_CONSTANT_COLOR  : NativeInt;
    CONSTANT_ALPHA     : NativeInt;
    ONE_MINUS_CONSTANT_ALPHA  : NativeInt;
    BLEND_COLOR     : NativeInt;

   (* Buffer Objects *)
    ARRAY_BUFFER     : NativeInt;
    ELEMENT_ARRAY_BUFFER   : NativeInt;
    ARRAY_BUFFER_BINDING   : NativeInt;
    ELEMENT_ARRAY_BUFFER_BINDING : NativeInt;

    STREAM_DRAW     : NativeInt;
    STATIC_DRAW     : NativeInt;
    DYNAMIC_DRAW     : NativeInt;

    BUFFER_SIZE     : NativeInt;
    BUFFER_USAGE     : NativeInt;

    CURRENT_VERTEX_ATTRIB   : NativeInt;

   (* CullFaceMode *)
    FRONT       : NativeInt;
    BACK       : NativeInt;
    FRONT_AND_BACK     : NativeInt;

   (* DepthFunction *)
   (*  NEVER *)
   (*  LESS *)
   (*  EQUAL *)
   (*  LEQUAL *)
   (*  GREATER *)
   (*  NOTEQUAL *)
   (*  GEQUAL *)
   (*  ALWAYS *)

   (* EnableCap *)
   (* TEXTURE_2D *)
    CULL_FACE      : NativeInt;
    BLEND       : NativeInt;
    DITHER       : NativeInt;
    STENCIL_TEST     : NativeInt;
    DEPTH_TEST      : NativeInt;
    SCISSOR_TEST     : NativeInt;
    POLYGON_OFFSET_FILL   : NativeInt;
    SAMPLE_ALPHA_TO_COVERAGE  : NativeInt;
    SAMPLE_COVERAGE    : NativeInt;

   (* ErrorCode *)
    NO_ERROR      : NativeInt;
    INVALID_ENUM     : NativeInt;
    INVALID_VALUE     : NativeInt;
    INVALID_OPERATION    : NativeInt;
    OUT_OF_MEMORY     : NativeInt;

   (* FrontFaceDirection *)
    CW        : NativeInt;
    CCW       : NativeInt;

   (* GetPName *)
    LINE_WIDTH      : NativeInt;
    ALIASED_POINT_SIZE_RANGE  : NativeInt;
    ALIASED_LINE_WIDTH_RANGE  : NativeInt;
    CULL_FACE_MODE     : NativeInt;
    FRONT_FACE      : NativeInt;
    DEPTH_RANGE     : NativeInt;
    DEPTH_WRITEMASK    : NativeInt;
    DEPTH_CLEAR_VALUE    : NativeInt;
    DEPTH_FUNC      : NativeInt;
    STENCIL_CLEAR_VALUE   : NativeInt;
    STENCIL_FUNC     : NativeInt;
    STENCIL_FAIL     : NativeInt;
    STENCIL_PASS_DEPTH_FAIL  : NativeInt;
    STENCIL_PASS_DEPTH_PASS  : NativeInt;
    STENCIL_REF     : NativeInt;
    STENCIL_VALUE_MASK    : NativeInt;
    STENCIL_WRITEMASK    : NativeInt;
    STENCIL_BACK_FUNC    : NativeInt;
    STENCIL_BACK_FAIL    : NativeInt;
    STENCIL_BACK_PASS_DEPTH_FAIL : NativeInt;
    STENCIL_BACK_PASS_DEPTH_PASS : NativeInt;
    STENCIL_BACK_REF    : NativeInt;
    STENCIL_BACK_VALUE_MASK  : NativeInt;
    STENCIL_BACK_WRITEMASK   : NativeInt;
    _VIEWPORT      : NativeInt;
    SCISSOR_BOX     : NativeInt;
   (*  SCISSOR_TEST *)
    COLOR_CLEAR_VALUE    : NativeInt;
    COLOR_WRITEMASK    : NativeInt;
    UNPACK_ALIGNMENT    : NativeInt;
    PACK_ALIGNMENT     : NativeInt;
    MAX_TEXTURE_SIZE    : NativeInt;
    MAX_VIEWPORT_DIMS    : NativeInt;
    SUBPIXEL_BITS     : NativeInt;
    RED_BITS      : NativeInt;
    GREEN_BITS      : NativeInt;
    BLUE_BITS      : NativeInt;
    ALPHA_BITS      : NativeInt;
    DEPTH_BITS      : NativeInt;
    STENCIL_BITS     : NativeInt;
    POLYGON_OFFSET_UNITS   : NativeInt;
   (*  POLYGON_OFFSET_FILL *)
    POLYGON_OFFSET_FACTOR   : NativeInt;
    TEXTURE_BINDING_2D    : NativeInt;
    SAMPLE_BUFFERS     : NativeInt;
    SAMPLES      : NativeInt;
    SAMPLE_COVERAGE_VALUE   : NativeInt;
    SAMPLE_COVERAGE_INVERT   : NativeInt;

   (* GetTextureParameter *)
   (*  TEXTURE_MAG_FILTER *)
   (*  TEXTURE_MIN_FILTER *)
   (*  TEXTURE_WRAP_S *)
   (*  TEXTURE_WRAP_T *)

    COMPRESSED_TEXTURE_FORMATS  : NativeInt;

   (* HintMode *)
    DONT_CARE      : NativeInt;
    FASTEST      : NativeInt;
    NICEST       : NativeInt;

   (* HintTarget *)
    GENERATE_MIPMAP_HINT   : NativeInt;

   (* DataType *)
    BYTE       : NativeInt;
    UNSIGNED_BYTE     : NativeInt;
    SHORT       : NativeInt;
    UNSIGNED_SHORT     : NativeInt;
    INT       : NativeInt;
    UNSIGNED_INT     : NativeInt;
    FLOAT       : NativeInt;

   (* PixelFormat *)
    DEPTH_COMPONENT    : NativeInt;
    ALPHA       : NativeInt;
    RGB       : NativeInt;
    RGBA       : NativeInt;
    LUMINANCE      : NativeInt;
    LUMINANCE_ALPHA    : NativeInt;

   (* PixelType *)
   (*  UNSIGNED_BYTE *)
    UNSIGNED_SHORT_4_4_4_4   : NativeInt;
    UNSIGNED_SHORT_5_5_5_1   : NativeInt;
    UNSIGNED_SHORT_5_6_5   : NativeInt;

   (* Shaders *)
    FRAGMENT_SHADER     : NativeInt;
    VERTEX_SHADER     : NativeInt;
    MAX_VERTEX_ATTRIBS    : NativeInt;
    MAX_VERTEX_UNIFORM_VECTORS  : NativeInt;
    MAX_VARYING_VECTORS    : NativeInt;
    MAX_COMBINED_TEXTURE_IMAGE_UNITS : NativeInt;
    MAX_VERTEX_TEXTURE_IMAGE_UNITS : NativeInt;
    MAX_TEXTURE_IMAGE_UNITS   : NativeInt;
    MAX_FRAGMENT_UNIFORM_VECTORS  : NativeInt;
    SHADER_TYPE      : NativeInt;
    DELETE_STATUS     : NativeInt;
    LINK_STATUS      : NativeInt;
    VALIDATE_STATUS     : NativeInt;
    ATTACHED_SHADERS     : NativeInt;
    ACTIVE_UNIFORMS     : NativeInt;
    ACTIVE_ATTRIBUTES    : NativeInt;
    SHADING_LANGUAGE_VERSION   : NativeInt;
    CURRENT_PROGRAM     : NativeInt;

   (* StencilFunction *)
    NEVER       : NativeInt;
    LESS       : NativeInt;
    EQUAL       : NativeInt;
    LEQUAL       : NativeInt;
    GREATER      : NativeInt;
    NOTEQUAL      : NativeInt;
    GEQUAL       : NativeInt;
    ALWAYS       : NativeInt;

   (* StencilOp *)
   (*  ZERO *)
    KEEP       : NativeInt;
    REPLACE      : NativeInt;
    INCR       : NativeInt;
    DECR       : NativeInt;
    INVERT       : NativeInt;
    INCR_WRAP      : NativeInt;
    DECR_WRAP      : NativeInt;

   (* StringName *)
    VENDOR       : NativeInt;
    RENDERER      : NativeInt;
    VERSION      : NativeInt;

   (* TextureMagFilter *)
    NEAREST      : NativeInt;
    LINEAR       : NativeInt;

   (* TextureMinFilter *)
   (*  NEAREST *)
   (*  LINEAR *)
    NEAREST_MIPMAP_NEAREST   : NativeInt;
    LINEAR_MIPMAP_NEAREST   : NativeInt;
    NEAREST_MIPMAP_LINEAR   : NativeInt;
    LINEAR_MIPMAP_LINEAR   : NativeInt;

   (* TextureParameterName *)
    TEXTURE_MAG_FILTER    : NativeInt;
    TEXTURE_MIN_FILTER    : NativeInt;
    TEXTURE_WRAP_S     : NativeInt;
    TEXTURE_WRAP_T     : NativeInt;

   (* TextureTarget *)
    TEXTURE_2D      : NativeInt;
    TEXTURE      : NativeInt;

    TEXTURE_CUBE_MAP    : NativeInt;
    TEXTURE_BINDING_CUBE_MAP  : NativeInt;
    TEXTURE_CUBE_MAP_POSITIVE_X : NativeInt;
    TEXTURE_CUBE_MAP_NEGATIVE_X : NativeInt;
    TEXTURE_CUBE_MAP_POSITIVE_Y : NativeInt;
    TEXTURE_CUBE_MAP_NEGATIVE_Y : NativeInt;
    TEXTURE_CUBE_MAP_POSITIVE_Z : NativeInt;
    TEXTURE_CUBE_MAP_NEGATIVE_Z : NativeInt;
    MAX_CUBE_MAP_TEXTURE_SIZE  : NativeInt;

   (* TextureUnit *)
    TEXTURE0      : NativeInt;
    TEXTURE1      : NativeInt;
    TEXTURE2      : NativeInt;
    TEXTURE3      : NativeInt;
    TEXTURE4      : NativeInt;
    TEXTURE5      : NativeInt;
    TEXTURE6      : NativeInt;
    TEXTURE7      : NativeInt;
    TEXTURE8      : NativeInt;
    TEXTURE9      : NativeInt;
    TEXTURE10      : NativeInt;
    TEXTURE11      : NativeInt;
    TEXTURE12      : NativeInt;
    TEXTURE13      : NativeInt;
    TEXTURE14      : NativeInt;
    TEXTURE15      : NativeInt;
    TEXTURE16      : NativeInt;
    TEXTURE17      : NativeInt;
    TEXTURE18      : NativeInt;
    TEXTURE19      : NativeInt;
    TEXTURE20      : NativeInt;
    TEXTURE21      : NativeInt;
    TEXTURE22      : NativeInt;
    TEXTURE23      : NativeInt;
    TEXTURE24      : NativeInt;
    TEXTURE25      : NativeInt;
    TEXTURE26      : NativeInt;
    TEXTURE27      : NativeInt;
    TEXTURE28      : NativeInt;
    TEXTURE29      : NativeInt;
    TEXTURE30      : NativeInt;
    TEXTURE31      : NativeInt;
    ACTIVE_TEXTURE     : NativeInt;

   (* TextureWrapMode *)
    _REPEAT       : NativeInt;
    CLAMP_TO_EDGE     : NativeInt;
    MIRRORED_REPEAT    : NativeInt;

   (* Uniform Types *)
    FLOAT_VEC2      : NativeInt;
    FLOAT_VEC3      : NativeInt;
    FLOAT_VEC4      : NativeInt;
    INT_VEC2      : NativeInt;
    INT_VEC3      : NativeInt;
    INT_VEC4      : NativeInt;
    BOOL       : NativeInt;
    BOOL_VEC2      : NativeInt;
    BOOL_VEC3      : NativeInt;
    BOOL_VEC4      : NativeInt;
    FLOAT_MAT2      : NativeInt;
    FLOAT_MAT3      : NativeInt;
    FLOAT_MAT4      : NativeInt;
    SAMPLER_2D      : NativeInt;
    SAMPLER_CUBE     : NativeInt;

   (* Vertex Arrays *)
    VERTEX_ATTRIB_ARRAY_ENABLED  : NativeInt;
    VERTEX_ATTRIB_ARRAY_SIZE   : NativeInt;
    VERTEX_ATTRIB_ARRAY_STRIDE   : NativeInt;
    VERTEX_ATTRIB_ARRAY_TYPE   : NativeInt;
    VERTEX_ATTRIB_ARRAY_NORMALIZED  : NativeInt;
    VERTEX_ATTRIB_ARRAY_POINTER  : NativeInt;
    VERTEX_ATTRIB_ARRAY_BUFFER_BINDING : NativeInt;

   (* Read Format *)
    IMPLEMENTATION_COLOR_READ_TYPE : NativeInt;
    IMPLEMENTATION_COLOR_READ_FORMAT : NativeInt;

   (* Shader Source *)
    COMPILE_STATUS     : NativeInt;

   (* Shader Precision-Specified Types *)
    LOW_FLOAT      : NativeInt;
    MEDIUM_FLOAT     : NativeInt;
    HIGH_FLOAT      : NativeInt;
    LOW_INT      : NativeInt;
    MEDIUM_INT      : NativeInt;
    HIGH_INT      : NativeInt;

   (* Framebuffer Object. *)
    FRAMEBUFFER     : NativeInt;
    RENDERBUFFER     : NativeInt;

    RGBA4       : NativeInt;
    RGB5_A1      : NativeInt;
    RGB565       : NativeInt;
    DEPTH_COMPONENT16    : NativeInt;
    STENCIL_INDEX8     : NativeInt;
    DEPTH_STENCIL     : NativeInt;

    RENDERBUFFER_WIDTH    : NativeInt;
    RENDERBUFFER_HEIGHT   : NativeInt;
    RENDERBUFFER_INTERNAL_FORMAT : NativeInt;
    RENDERBUFFER_RED_SIZE   : NativeInt;
    RENDERBUFFER_GREEN_SIZE  : NativeInt;
    RENDERBUFFER_BLUE_SIZE   : NativeInt;
    RENDERBUFFER_ALPHA_SIZE  : NativeInt;
    RENDERBUFFER_DEPTH_SIZE  : NativeInt;
    RENDERBUFFER_STENCIL_SIZE  : NativeInt;

    FRAMEBUFFER_ATTACHMENT_OBJECT_TYPE   : NativeInt;
    FRAMEBUFFER_ATTACHMENT_OBJECT_NAME   : NativeInt;
    FRAMEBUFFER_ATTACHMENT_TEXTURE_LEVEL   : NativeInt;
    FRAMEBUFFER_ATTACHMENT_TEXTURE_CUBE_MAP_FACE : NativeInt;

    COLOR_ATTACHMENT0    : NativeInt;
    DEPTH_ATTACHMENT    : NativeInt;
    STENCIL_ATTACHMENT    : NativeInt;
    DEPTH_STENCIL_ATTACHMENT  : NativeInt;

    NONE       : NativeInt;

    FRAMEBUFFER_COMPLETE      : NativeInt;
    FRAMEBUFFER_INCOMPLETE_ATTACHMENT   : NativeInt;
    FRAMEBUFFER_INCOMPLETE_MISSING_ATTACHMENT : NativeInt;
    FRAMEBUFFER_INCOMPLETE_DIMENSIONS   : NativeInt;
    FRAMEBUFFER_UNSUPPORTED     : NativeInt;

    FRAMEBUFFER_BINDING   : NativeInt;
    RENDERBUFFER_BINDING   : NativeInt;
    MAX_RENDERBUFFER_SIZE   : NativeInt;

    INVALID_FRAMEBUFFER_OPERATION : NativeInt;

   (* WebGL-specific enums *)
    UNPACK_FLIP_Y_WEBGL   : NativeInt;
    UNPACK_PREMULTIPLY_ALPHA_WEBGL : NativeInt;
    CONTEXT_LOST_WEBGL    : NativeInt;
    UNPACK_COLORSPACE_CONVERSION_WEBGL : NativeInt;
    BROWSER_DEFAULT_WEBGL   : NativeInt;

  Private
    Fcanvas : TJSHTMLCanvasElement; external name 'canvas';
    FdrawingBufferWidth : GLsizei; external name 'drawingBufferWidth';
    FdrawingBufferHeight : GLsizei; external name 'drawingBufferHeight';
  Public
    function getContextAttributes() : TJSWebGLContextAttributes;
    function isContextLost() : boolean ;
    function getSupportedExtensions() : TJSStringDynArray;
    function getExtension(name : String) : TJSobject;
    procedure activeTexture(texture : GLenum);
    procedure attachShader(aprogram : TJSWebGLProgram ; shader : TJSWebGLShader);
    procedure bindAttribLocation(WebGLProgram : TJSWebGLProgram ; index : GLuint; name : String);
    procedure bindBuffer(target : GLenum; buffer : TJSWebGLBuffer );
    procedure bindFramebuffer(target : GLenum; framebuffer : TJSWebGLFramebuffer );
    procedure bindRenderbuffer(target : GLenum; renderbuffer : TJSWebGLRenderbuffer);
    procedure bindTexture(target : GLenum; texture : TJSWebGLTexture);
    procedure blendColor(red : GLclampf; green : GLclampf; blue : GLclampf; alpha : GLclampf);
    procedure blendEquation(RGB : GLenum : mode : GLenum);
    procedure blendEquationSeparate(Alpha : GLenum; mode : GLenum);
    procedure blendFunc(sfactor : GLenum; dfactor : GLenum);
    procedure blendFuncSeparate(srcRGB : GLenum; dstRGB : GLenum ; dstAlpha : GLenum: srcAlpha : GLenum);

    procedure bufferData(target : GLenum; size : GLsizeiptr ; usage : GLenum);
    procedure bufferData(target : GLenum;  data : TJSBufferSource ; usage : GLenum);
    procedure bufferSubData(target : GLenum; offset : GLintptr; data : TJSBufferSource);

    function checkFramebufferStatus(target : GLenum) :String;
    procedure clear(mask : GLbitfield);
    procedure clearColor(red: GLclampf ; green : GLclampf; blue: GLclampf ; alpha: GLclampf );
    procedure clearDepth(depth : GLclampf);
    procedure clearStencil(s : GLint);
    procedure colorMask(red : GLboolean ; green : GLboolean ; blue : GLboolean; alpha: GLboolean );
    procedure compileShader(shader : TJSWebGLShader);
    procedure compressedTexImage2D(target : GLenum ; level : GLint; internalformat : GLenum ; width : GLsizei; height : GLsizei ; border : GLint; data : TJSArrayBufferView);
    procedure compressedTexSubImage2D(target : GLenum; level: GLint ; xoffset : GLint; yoffset : GLint ; width : GLsizei; height: GLsizei ; format : GLenum; data: TJSArrayBufferView );
    procedure copyTexImage2D(target : GLenum; level : GLint ; internalformat : GLenum; x: GLint ; y : GLint; width : GLsizei ; height : GLsizei;border: GLint );
    procedure copyTexSubImage2D(target : GLenum; level: GLint ; xoffset : GLint; yoffset : GLint ; x : GLint; y: GLint ; width : GLsizei; height: GLsizei );

    function createBuffer() : TJSWebGLBuffer;
    function createFramebuffer() : TJSWebGLFramebuffer;
    function createProgram() : TJSWebGLProgram;
    function createRenderbuffer() : TJSWebGLRenderbuffer;
    function createShader(atype : GLenum) : TJSWebGLShader;
    function createTexture() : TJSWebGLTexture;
    procedure cullFace(mode : GLenum);

    procedure deleteBuffer(buffer : TJSWebGLBuffer);
    procedure deleteFramebuffer(framebuffer : TJSWebGLFramebuffer);
    procedure deleteProgram(aprogram : TJSWebGLProgram);
    procedure deleteRenderbuffer(renderbuffer : TJSWebGLRenderbuffer);
    procedure deleteShader(shader : TJSWebGLShader);
    procedure deleteTexture(texture : TJSWebGLTexture);

    procedure depthFunc(func : GLenum);
    procedure depthMask(flag : GLboolean);
    procedure depthRange(zNear: GLclampf ; zFar : GLclampf);
    procedure detachShader(aprogram : TJSWebGLProgram; shader: TJSWebGLShader );
    procedure disable(cap : GLenum);
    procedure disableVertexAttribArray(index : GLuint);
    procedure drawArrays(mode: GLenum ; first : GLint; count: GLsizei );
    procedure drawElements(mode : GLenum; count: GLsizei ; atype : GLenum; offset: GLintptr );

    procedure enable(cap : GLenum);
    procedure enableVertexAttribArray(index : GLuint);
    procedure finish();
    procedure flush();
    procedure framebufferRenderbuffer(target: GLenum ; attachment : GLenum; renderbuffertarget: GLenum , renderbuffer: TJSWebGLRenderbuffer );
    procedure framebufferTexture2D(target : GLenum; attachment:GLenum ;textarget: GLenum ; texture : TJSWebGLTexture; level: GLint );
    procedure frontFace(mode : GLenum);
    procedure generateMipmap(target : GLenum);
    function getActiveAttrib(aprogram: TJSWebGLProgram ; index : GLuint) : TJSWebGLActiveInfo;
    function getActiveUniform(aprogram : TJSWebGLProgram; index: GLuint ) : TJSWebGLActiveInfo;
    function getAttachedShaders(aprogram: TJSWebGLProgram) : TJSWebGLShaderArray ;
    function getAttribLocation(aprogram :TJSWebGLProgram ; name : String) : GLint ;
    function getBufferParameter(target : GLenum; pname: GLenum ) : JSValue;
    function getParameter(pname : GLenum) : JSValue;
    function getError() : Integer;
    function getFramebufferAttachmentParameter(target: GLenum ; attachment : GLenum; pname : GLenum ) : JSValue;
    function getProgramParameter(aprogram : TJSWebGLProgram; pname : GLenum) : JSValue;
    Function getProgramInfoLog(aprogram : TJSWebGLProgram) : String;
    function getRenderbufferParameter(target: GLenum ; pname : GLenum) : JSValue;
    function getShaderParameter(shader : TJSWebGLShader; pname: GLenum ) : JSValue;
    Function getShaderPrecisionFormat(shadertype : GLenum; precisiontype:GLenum ) : TJSWebGLShaderPrecisionFormat;
    function getShaderInfoLog(shader : TJSWebGLShader) : TJSString;
    function getShaderSource(shader : TJSWebGLShader) : TJSString;
    function getTexParameter(target: GLenum ; pname : GLenum) : JSValue;
    function getUniform(aprogram : TJSWebGLProgram; location: TJSWebGLUniformLocation ) : JSValue;
    function getUniformLocation(aprogram : TJSWebGLProgram; name :String )  : TJSWebGLUniformLocation;
    Function getVertexAttrib(index : GLuint; pname: GLenum ) : JSValue;
    Function getVertexAttribOffset(index : GLuint; pname: GLenum ) : GLintptr ;
    procedure hint(target : GLenum; mode: GLenum );
    function isBuffer(buffer : TJSWebGLBuffer) : GLBoolean;
    function isEnabled(cap : GLenum) : GLBoolean;
    function isFramebuffer(framebuffer : TJSWebGLFramebuffer) : GLBoolean;
    function isProgram(aprogram : TJSWebGLProgram) : GLBoolean;
    function isRenderbuffer(renderbuffer : TJSWebGLRenderbuffer) : GLBoolean;
    function isShader(shader : TJSWebGLShader) : GLBoolean;
    function isTexture(texture : TJSWebGLTexture) : GLBoolean;
    procedure lineWidth(width : GLfloat);
    procedure linkProgram(aprogram : TJSWebGLProgram);
    procedure pixelStorei(pname: GLenum ; param : GLint);
    procedure polygonOffset(factor : GLfloat; units: GLfloat );
    procedure readPixels(x : GLint; y : GLint ; width : GLsizei; height : GLsizei ; format : GLenum; atype: GLenum ,  pixels: TJSArrayBufferView );
    procedure renderbufferStorage(target : GLenum; internalformat :GLenum ; width : GLsizei; height: GLsizei );
    procedure sampleCoverage(value : GLclampf; invert: GLboolean );
    procedure scissor(x : GLint; y : GLint ; width : GLsizei; height : GLsizei);
    procedure shaderSource(shader : TJSWebGLShader; source: String);
    procedure stencilFunc(func : GLenum; ref: GLint ; mask : GLuint);
    procedure stencilFuncSeparate(face : GLenum; func: GLenum ; ref : GLint; mask: GLuint );
    procedure stencilMask(mask : GLuint);
    procedure stencilMaskSeparate(face : GLenum ; mask : GLuint);
    procedure stencilOp(fail : GLenum; zfail : GLEnum; zpass : GLenum);
    procedure stencilOpSeparate(face : GLenum; fail : GLEnum; zfail : GLenum; zpass: GLenum );
    procedure texImage2D(target : GLenum; level : GLint; internalformat : GLint; width: GLsizei ; height : GLsizei; border: GLint ; format : GLenum; atype : GLEnum; pixels : TJSArrayBufferView ) ;
    procedure texImage2D(target : GLenum; level : GLint; internalformat : GLint; format : GLEnum; atype : GLenum; source : TJSTexImageSource ); // May throw DOMException

    procedure texParameterf(target : GLenum; pname : GLEnum; param : GLfloat);
    procedure texParameteri(target : GLenum; pname : GLEnum; param : GLint);

    procedure texSubImage2D(target : GLenum; level : GLint; xoffset : GLint; yoffset : GLint; width : GLsizei; height : GLsizei ; format : GLenum; atype : GLenum ;  pixels : TJSArrayBufferView);
    procedure texSubImage2D(target : GLenum; level : GLint; xoffset : GLint; yoffset : GLint; format : GLenum; atype : GLenum ; source: TJSTexImageSource ); // May throw DOMException

    procedure uniform1f(location : TJSWebGLUniformLocation; x : GLFLoat);
    procedure uniform2f(location : TJSWebGLUniformLocation; x : GLFLoat; y : GLfloat);
    procedure uniform3f(location : TJSWebGLUniformLocation; x : GLFLoat, y : GLFLoat, z : GLFLoat);
    procedure uniform4f(location : TJSWebGLUniformLocation; x : GLFLoat, y : GLFLoat; z : GLfloat; w : GLFLoat);

    procedure uniform1i(location : TJSWebGLUniformLocation; x : GLint);
    procedure uniform2i(location : TJSWebGLUniformLocation; x : GLint, y : GLint);
    procedure uniform3i(location : TJSWebGLUniformLocation; x : GLint, y : GLint, z : GLint);
    procedure uniform4i(location : TJSWebGLUniformLocation; x : GLint; y : GLint; z : GLint; w : GLint);

    procedure uniform1fv(location : TJSWebGLUniformLocation; v : TJSFloat32List );
    procedure uniform2fv(location : TJSWebGLUniformLocation; v : TJSFloat32List );
    procedure uniform3fv(location : TJSWebGLUniformLocation; v : TJSFloat32List );
    procedure uniform4fv(location : TJSWebGLUniformLocation; v : TJSFloat32List );

    procedure uniform1iv(location : TJSWebGLUniformLocation; v: TJSInt32List );
    procedure uniform2iv(location : TJSWebGLUniformLocation; v: TJSInt32List );
    procedure uniform3iv(location : TJSWebGLUniformLocation; v: TJSInt32List );
    procedure uniform4iv(location : TJSWebGLUniformLocation; v: TJSInt32List );

    procedure uniformMatrix2fv(location : TJSWebGLUniformLocation; transpose: GLboolean ; value : TJSFloat32List);
    procedure uniformMatrix3fv(location : TJSWebGLUniformLocation; transpose: GLboolean ; value : TJSFloat32List);
    procedure uniformMatrix4fv(location : TJSWebGLUniformLocation; transpose: GLboolean ; value : TJSFloat32List);

    procedure useProgram(aprogram : TJSWebGLProgram);
    procedure validateProgram(aprogram : TJSWebGLProgram);

    procedure vertexAttrib1f(index: GLuint; x : GLfloat);
    procedure vertexAttrib2f(index : GLuint; x : GLFLoat; y : GLfloat);
    procedure vertexAttrib3f(index : GLuint; x : GLFLoat; y : GLfloat; z : GLFLoat);
    procedure vertexAttrib4f(index : GLuint; x : GLFLoat; y : GLfloat; z : GLFLoat, w : GLFLoat);

    procedure vertexAttrib1fv(index : GLuint; values : TJSFloat32List );
    procedure vertexAttrib2fv(index : GLuint; values : TJSFloat32List );
    procedure vertexAttrib3fv(index : GLuint; values : TJSFloat32List );
    procedure vertexAttrib4fv(index : GLuint; values : TJSFloat32List );

    procedure vertexAttribPointer(index : GLuint; size : GLInt, atype : GLEnum; normalized : GLboolean; stride: GLsizei; aoffset: GLintptr );
    procedure viewport(x : GLint; y : GLint; width: GLsizei , height: GLsizei );
    Property canvas : TJSHTMLCanvasElement read Fcanvas;
    Property drawingBufferWidth : GLsizei read FdrawingBufferWidth;
    Property drawingBufferHeight : GLsizei read FdrawingBufferHeight;
  end;

  TJSWebGLRenderingContext = Class external name 'WebGLRenderingContext' (TJSWebGLRenderingContextBase);


  TJSWebGLContextEvent = class external name 'WebGLContextEvent'  (TJSEvent)
  Private
    FstatusMessage : String; external name 'statusMessage';
  public
    Property statusMessage : String read FstatusMessage;
  end;

  // EventInit is defined in the DOM4 specification.
  TJSWebGLContext = class external name 'WebGLContext' (TJSEvent)
    statusMessage : String;
  end;

implementation

end.
