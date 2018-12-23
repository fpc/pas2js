unit WebGL2;

{$mode objfpc}
{$modeswitch externalclass}

interface
uses
 WebGL, JS, Web;

type
  GLenumDynArray = array of GLenum;
  GLuintDynArray = array of GLuint;

type
  GLint64 = nativeint; // long long
  GLuint64 = longword; // unsigned long long
type
  TJSWebGLQuery = class external name 'WebGLQuery' (TJSWebGLObject);
  TJSWebGLSampler = class external name 'WebGLSampler' (TJSWebGLObject);
  TJSWebGLSync = class external name 'WebGLSync' (TJSWebGLObject);
  TJSWebGLTransformFeedback = class external name 'WebGLTransformFeedback' (TJSWebGLObject);
  TJSWebGLVertexArrayObject = class external name 'WebGLVertexArrayObject' (TJSWebGLObject);
type  
  TJSWebGL2RenderingContextBase = class external name 'WebGL2RenderingContextBase' (TJSWebGLRenderingContextBase)
  public
  const
    READ_BUFFER: nativeint;
    UNPACK_ROW_LENGTH: nativeint;
    UNPACK_SKIP_ROWS: nativeint;
    UNPACK_SKIP_PIXELS: nativeint;
    PACK_ROW_LENGTH: nativeint;
    PACK_SKIP_ROWS: nativeint;
    PACK_SKIP_PIXELS: nativeint;
    COLOR: nativeint;
    DEPTH: nativeint;
    STENCIL: nativeint;
    RED: nativeint;
    RGB8: nativeint;
    RGBA8: nativeint;
    RGB10_A2: nativeint;
    TEXTURE_BINDING_3D: nativeint;
    UNPACK_SKIP_IMAGES: nativeint;
    UNPACK_IMAGE_HEIGHT: nativeint;
    TEXTURE_3D: nativeint;
    TEXTURE_WRAP_R: nativeint;
    MAX_3D_TEXTURE_SIZE: nativeint;
    UNSIGNED_INT_2_10_10_10_REV: nativeint;
    MAX_ELEMENTS_VERTICES: nativeint;
    MAX_ELEMENTS_INDICES: nativeint;
    TEXTURE_MIN_LOD: nativeint;
    TEXTURE_MAX_LOD: nativeint;
    TEXTURE_BASE_LEVEL: nativeint;
    TEXTURE_MAX_LEVEL: nativeint;
    MIN: nativeint;
    MAX: nativeint;
    DEPTH_COMPONENT24: nativeint;
    MAX_TEXTURE_LOD_BIAS: nativeint;
    TEXTURE_COMPARE_MODE: nativeint;
    TEXTURE_COMPARE_FUNC: nativeint;
    CURRENT_QUERY: nativeint;
    QUERY_RESULT: nativeint;
    QUERY_RESULT_AVAILABLE: nativeint;
    STREAM_READ: nativeint;
    STREAM_COPY: nativeint;
    STATIC_READ: nativeint;
    STATIC_COPY: nativeint;
    DYNAMIC_READ: nativeint;
    DYNAMIC_COPY: nativeint;
    MAX_DRAW_BUFFERS: nativeint;
    DRAW_BUFFER0: nativeint;
    DRAW_BUFFER1: nativeint;
    DRAW_BUFFER2: nativeint;
    DRAW_BUFFER3: nativeint;
    DRAW_BUFFER4: nativeint;
    DRAW_BUFFER5: nativeint;
    DRAW_BUFFER6: nativeint;
    DRAW_BUFFER7: nativeint;
    DRAW_BUFFER8: nativeint;
    DRAW_BUFFER9: nativeint;
    DRAW_BUFFER10: nativeint;
    DRAW_BUFFER11: nativeint;
    DRAW_BUFFER12: nativeint;
    DRAW_BUFFER13: nativeint;
    DRAW_BUFFER14: nativeint;
    DRAW_BUFFER15: nativeint;
    MAX_FRAGMENT_UNIFORM_COMPONENTS: nativeint;
    MAX_VERTEX_UNIFORM_COMPONENTS: nativeint;
    SAMPLER_3D: nativeint;
    SAMPLER_2D_SHADOW: nativeint;
    FRAGMENT_SHADER_DERIVATIVE_HINT: nativeint;
    PIXEL_PACK_BUFFER: nativeint;
    PIXEL_UNPACK_BUFFER: nativeint;
    PIXEL_PACK_BUFFER_BINDING: nativeint;
    PIXEL_UNPACK_BUFFER_BINDING: nativeint;
    FLOAT_MAT2x3: nativeint;
    FLOAT_MAT2x4: nativeint;
    FLOAT_MAT3x2: nativeint;
    FLOAT_MAT3x4: nativeint;
    FLOAT_MAT4x2: nativeint;
    FLOAT_MAT4x3: nativeint;
    SRGB: nativeint;
    SRGB8: nativeint;
    SRGB8_ALPHA8: nativeint;
    COMPARE_REF_TO_TEXTURE: nativeint;
    RGBA32F: nativeint;
    RGB32F: nativeint;
    RGBA16F: nativeint;
    RGB16F: nativeint;
    VERTEX_ATTRIB_ARRAY_INTEGER: nativeint;
    MAX_ARRAY_TEXTURE_LAYERS: nativeint;
    MIN_PROGRAM_TEXEL_OFFSET: nativeint;
    MAX_PROGRAM_TEXEL_OFFSET: nativeint;
    MAX_VARYING_COMPONENTS: nativeint;
    TEXTURE_2D_ARRAY: nativeint;
    TEXTURE_BINDING_2D_ARRAY: nativeint;
    R11F_G11F_B10F: nativeint;
    UNSIGNED_INT_10F_11F_11F_REV: nativeint;
    RGB9_E5: nativeint;
    UNSIGNED_INT_5_9_9_9_REV: nativeint;
    TRANSFORM_FEEDBACK_BUFFER_MODE: nativeint;
    MAX_TRANSFORM_FEEDBACK_SEPARATE_COMPONENTS: nativeint;
    TRANSFORM_FEEDBACK_VARYINGS: nativeint;
    TRANSFORM_FEEDBACK_BUFFER_START: nativeint;
    TRANSFORM_FEEDBACK_BUFFER_SIZE: nativeint;
    TRANSFORM_FEEDBACK_PRIMITIVES_WRITTEN: nativeint;
    RASTERIZER_DISCARD: nativeint;
    MAX_TRANSFORM_FEEDBACK_INTERLEAVED_COMPONENTS: nativeint;
    MAX_TRANSFORM_FEEDBACK_SEPARATE_ATTRIBS: nativeint;
    INTERLEAVED_ATTRIBS: nativeint;
    SEPARATE_ATTRIBS: nativeint;
    TRANSFORM_FEEDBACK_BUFFER: nativeint;
    TRANSFORM_FEEDBACK_BUFFER_BINDING: nativeint;
    RGBA32UI: nativeint;
    RGB32UI: nativeint;
    RGBA16UI: nativeint;
    RGB16UI: nativeint;
    RGBA8UI: nativeint;
    RGB8UI: nativeint;
    RGBA32I: nativeint;
    RGB32I: nativeint;
    RGBA16I: nativeint;
    RGB16I: nativeint;
    RGBA8I: nativeint;
    RGB8I: nativeint;
    RED_INTEGER: nativeint;
    RGB_INTEGER: nativeint;
    RGBA_INTEGER: nativeint;
    SAMPLER_2D_ARRAY: nativeint;
    SAMPLER_2D_ARRAY_SHADOW: nativeint;
    SAMPLER_CUBE_SHADOW: nativeint;
    UNSIGNED_INT_VEC2: nativeint;
    UNSIGNED_INT_VEC3: nativeint;
    UNSIGNED_INT_VEC4: nativeint;
    INT_SAMPLER_2D: nativeint;
    INT_SAMPLER_3D: nativeint;
    INT_SAMPLER_CUBE: nativeint;
    INT_SAMPLER_2D_ARRAY: nativeint;
    UNSIGNED_INT_SAMPLER_2D: nativeint;
    UNSIGNED_INT_SAMPLER_3D: nativeint;
    UNSIGNED_INT_SAMPLER_CUBE: nativeint;
    UNSIGNED_INT_SAMPLER_2D_ARRAY: nativeint;
    DEPTH_COMPONENT32F: nativeint;
    DEPTH32F_STENCIL8: nativeint;
    FLOAT_32_UNSIGNED_INT_24_8_REV: nativeint;
    FRAMEBUFFER_ATTACHMENT_COLOR_ENCODING: nativeint;
    FRAMEBUFFER_ATTACHMENT_COMPONENT_TYPE: nativeint;
    FRAMEBUFFER_ATTACHMENT_RED_SIZE: nativeint;
    FRAMEBUFFER_ATTACHMENT_GREEN_SIZE: nativeint;
    FRAMEBUFFER_ATTACHMENT_BLUE_SIZE: nativeint;
    FRAMEBUFFER_ATTACHMENT_ALPHA_SIZE: nativeint;
    FRAMEBUFFER_ATTACHMENT_DEPTH_SIZE: nativeint;
    FRAMEBUFFER_ATTACHMENT_STENCIL_SIZE: nativeint;
    FRAMEBUFFER_DEFAULT: nativeint;
    //DEPTH_STENCIL_ATTACHMENT: nativeint;
    //DEPTH_STENCIL: nativeint;
    UNSIGNED_INT_24_8: nativeint;
    DEPTH24_STENCIL8: nativeint;
    UNSIGNED_NORMALIZED: nativeint;
    DRAW_FRAMEBUFFER_BINDING: nativeint;
    READ_FRAMEBUFFER: nativeint;
    DRAW_FRAMEBUFFER: nativeint;
    READ_FRAMEBUFFER_BINDING: nativeint;
    RENDERBUFFER_SAMPLES: nativeint;
    FRAMEBUFFER_ATTACHMENT_TEXTURE_LAYER: nativeint;
    MAX_COLOR_ATTACHMENTS: nativeint;
    COLOR_ATTACHMENT1: nativeint;
    COLOR_ATTACHMENT2: nativeint;
    COLOR_ATTACHMENT3: nativeint;
    COLOR_ATTACHMENT4: nativeint;
    COLOR_ATTACHMENT5: nativeint;
    COLOR_ATTACHMENT6: nativeint;
    COLOR_ATTACHMENT7: nativeint;
    COLOR_ATTACHMENT8: nativeint;
    COLOR_ATTACHMENT9: nativeint;
    COLOR_ATTACHMENT10: nativeint;
    COLOR_ATTACHMENT11: nativeint;
    COLOR_ATTACHMENT12: nativeint;
    COLOR_ATTACHMENT13: nativeint;
    COLOR_ATTACHMENT14: nativeint;
    COLOR_ATTACHMENT15: nativeint;
    FRAMEBUFFER_INCOMPLETE_MULTISAMPLE: nativeint;
    MAX_SAMPLES: nativeint;
    HALF_FLOAT: nativeint;
    RG: nativeint;
    RG_INTEGER: nativeint;
    R8: nativeint;
    RG8: nativeint;
    R16F: nativeint;
    R32F: nativeint;
    RG16F: nativeint;
    RG32F: nativeint;
    R8I: nativeint;
    R8UI: nativeint;
    R16I: nativeint;
    R16UI: nativeint;
    R32I: nativeint;
    R32UI: nativeint;
    RG8I: nativeint;
    RG8UI: nativeint;
    RG16I: nativeint;
    RG16UI: nativeint;
    RG32I: nativeint;
    RG32UI: nativeint;
    VERTEX_ARRAY_BINDING: nativeint;
    R8_SNORM: nativeint;
    RG8_SNORM: nativeint;
    RGB8_SNORM: nativeint;
    RGBA8_SNORM: nativeint;
    SIGNED_NORMALIZED: nativeint;
    COPY_READ_BUFFER: nativeint;
    COPY_WRITE_BUFFER: nativeint;
    COPY_READ_BUFFER_BINDING: nativeint;
    COPY_WRITE_BUFFER_BINDING: nativeint;
    UNIFORM_BUFFER: nativeint;
    UNIFORM_BUFFER_BINDING: nativeint;
    UNIFORM_BUFFER_START: nativeint;
    UNIFORM_BUFFER_SIZE: nativeint;
    MAX_VERTEX_UNIFORM_BLOCKS: nativeint;
    MAX_FRAGMENT_UNIFORM_BLOCKS: nativeint;
    MAX_COMBINED_UNIFORM_BLOCKS: nativeint;
    MAX_UNIFORM_BUFFER_BINDINGS: nativeint;
    MAX_UNIFORM_BLOCK_SIZE: nativeint;
    MAX_COMBINED_VERTEX_UNIFORM_COMPONENTS: nativeint;
    MAX_COMBINED_FRAGMENT_UNIFORM_COMPONENTS: nativeint;
    UNIFORM_BUFFER_OFFSET_ALIGNMENT: nativeint;
    ACTIVE_UNIFORM_BLOCKS: nativeint;
    UNIFORM_TYPE: nativeint;
    UNIFORM_SIZE: nativeint;
    UNIFORM_BLOCK_INDEX: nativeint;
    UNIFORM_OFFSET: nativeint;
    UNIFORM_ARRAY_STRIDE: nativeint;
    UNIFORM_MATRIX_STRIDE: nativeint;
    UNIFORM_IS_ROW_MAJOR: nativeint;
    UNIFORM_BLOCK_BINDING: nativeint;
    UNIFORM_BLOCK_DATA_SIZE: nativeint;
    UNIFORM_BLOCK_ACTIVE_UNIFORMS: nativeint;
    UNIFORM_BLOCK_ACTIVE_UNIFORM_INDICES: nativeint;
    UNIFORM_BLOCK_REFERENCED_BY_VERTEX_SHADER: nativeint;
    UNIFORM_BLOCK_REFERENCED_BY_FRAGMENT_SHADER: nativeint;
    INVALID_INDEX: nativeint;
    MAX_VERTEX_OUTPUT_COMPONENTS: nativeint;
    MAX_FRAGMENT_INPUT_COMPONENTS: nativeint;
    MAX_SERVER_WAIT_TIMEOUT: nativeint;
    OBJECT_TYPE: nativeint;
    SYNC_CONDITION: nativeint;
    SYNC_STATUS: nativeint;
    SYNC_FLAGS: nativeint;
    SYNC_FENCE: nativeint;
    SYNC_GPU_COMMANDS_COMPLETE: nativeint;
    UNSIGNALED: nativeint;
    SIGNALED: nativeint;
    ALREADY_SIGNALED: nativeint;
    TIMEOUT_EXPIRED: nativeint;
    CONDITION_SATISFIED: nativeint;
    WAIT_FAILED: nativeint;
    SYNC_FLUSH_COMMANDS_BIT: nativeint;
    VERTEX_ATTRIB_ARRAY_DIVISOR: nativeint;
    ANY_SAMPLES_PASSED: nativeint;
    ANY_SAMPLES_PASSED_CONSERVATIVE: nativeint;
    SAMPLER_BINDING: nativeint;
    RGB10_A2UI: nativeint;
    INT_2_10_10_10_REV: nativeint;
    TRANSFORM_FEEDBACK: nativeint;
    TRANSFORM_FEEDBACK_PAUSED: nativeint;
    TRANSFORM_FEEDBACK_ACTIVE: nativeint;
    TRANSFORM_FEEDBACK_BINDING: nativeint;
    TEXTURE_IMMUTABLE_FORMAT: nativeint;
    MAX_ELEMENT_INDEX: nativeint;
    TEXTURE_IMMUTABLE_LEVELS: nativeint;
    MAX_CLIENT_WAIT_TIMEOUT_WEBGL: nativeint;
  public

    // Buffer objects
    procedure bufferData (target: GLenum; size: GLsizeiptr; usage: GLenum);
    procedure bufferData (target: GLenum; srcData: TJSBufferSource; usage: GLenum);
    procedure bufferSubData (target: GLenum; dstByteOffset: GLintptr; srcData: TJSBufferSource);

    // WebGL2
    procedure bufferData (target: GLenum; srcData: TJSArrayBufferView; usage: GLenum; srcOffset: GLuint; length: GLuint);
    procedure bufferSubData (target: GLenum; dstByteOffset: GLintptr; srcData: TJSArrayBufferView; length: GLuint);
    procedure copyBufferSubData (readTarget: GLenum; writeTarget: GLenum; readOffset: GLintptr; size: GLsizeiptr);

    // MapBufferRange, in particular its read-only and write-only modes,
    // can not be exposed safely to JavaScript. GetBufferSubData
    // replaces it for the purpose of fetching data back from the GPU.
    procedure getBufferSubData (target: GLenum; srcByteOffset: GLintptr; dstBuffer: TJSArrayBufferView; dstOffset: GLuint; length: GLuint);
    
    // Framebuffer objects
    procedure blitFramebuffer (srcX0: GLint; srcY0: GLint; srcX1: GLint; srcY1: GLint; dstX0: GLint; dstY0: GLint; dstY1: GLint; mask: GLbitfield; filter: GLenum);
    procedure framebufferTextureLayer (target: GLenum; attachment: GLenum; texture: TJSWebGLTexture; level: GLint);
    procedure invalidateFramebuffer (target: GLenum; attachments: GLenumDynArray);
    procedure invalidateSubFramebuffer (target: GLenum; attachments: GLenumDynArray; y: GLint; width: GLsizei; height: GLsizei);
    procedure readBuffer (src: GLenum);

    // Renderbuffer objects
    function getInternalformatParameter (target: GLenum; internalformat: GLenum; pname: GLenum): JSValue;
    procedure renderbufferStorageMultisample (target: GLenum; samples: GLsizei; internalformat: GLenum; height: GLsizei);

    // Texture objects
    procedure texStorage2D (target: GLenum; levels: GLsizei; internalformat: GLenum; width: GLsizei);
    procedure texStorage3D (target: GLenum; levels: GLsizei; internalformat: GLenum; width: GLsizei; depth: GLsizei);

    // WebGL1 legacy entrypoints:
    procedure texImage2D (target: GLenum; level: GLint; internalformat: GLint; height: GLsizei; border: GLint; format: GLenum; pixels: TJSArrayBufferView);
    procedure texImage2D (target: GLenum; level: GLint; internalformat: GLint; _type: GLenum; source: TJSTexImageSource);
    procedure texSubImage2D (target: GLenum; level: GLint; xoffset: GLint; yoffset: GLint; height: GLsizei; _type: GLenum; pixels: TJSArrayBufferView); overload;
    procedure texSubImage2D (target: GLenum; level: GLint; xoffset: GLint; yoffset: GLint; _type: GLenum; source: TJSTexImageSource);

    // WebGL2 entrypoints:
    procedure texImage2D (target: GLenum; level: GLint; internalformat: GLint; width: GLsizei; height: GLsizei; format: GLenum; _type: GLenum; pboOffset: GLintptr);
    procedure texImage2D (target: GLenum; level: GLint; internalformat: GLint; width: GLsizei; height: GLsizei; format: GLenum; _type: GLenum);
    procedure texImage2D (target: GLenum; level: GLint; internalformat: GLint; width: GLsizei; height: GLsizei; format: GLenum; _type: GLenum; srcData: TJSArrayBufferView);
    procedure texImage3D (target: GLenum; level: GLint; internalformat: GLint; width: GLsizei; height: GLsizei; border: GLint; format: GLenum; _type: GLenum; pboOffset: GLintptr);
    procedure texImage3D (target: GLenum; level: GLint; internalformat: GLint; width: GLsizei; height: GLsizei; border: GLint; format: GLenum; _type: GLenum);
    procedure texImage3D (target: GLenum; level: GLint; internalformat: GLint; width: GLsizei; height: GLsizei; border: GLint; format: GLenum; _type: GLenum; srcData: TJSArrayBufferView);
    //procedure texImage3D (target: GLenum; level: GLint; internalformat: GLint; width: GLsizei; height: GLsizei; border: GLint; format: GLenum; _type: GLenum; srcData: TJSArrayBufferView);
    procedure texSubImage2D (target: GLenum; level: GLint; xoffset: GLint; yoffset: GLint; width: GLsizei; format: GLenum; _type: GLenum; pboOffset: GLintptr);
    procedure texSubImage2D (target: GLenum; level: GLint; xoffset: GLint; yoffset: GLint; width: GLsizei; format: GLenum; _type: GLenum);
    procedure texSubImage2D (target: GLenum; level: GLint; xoffset: GLint; yoffset: GLint; width: GLsizei; format: GLenum; _type: GLenum; srcData: TJSArrayBufferView);
    procedure texSubImage3D (target: GLenum; level: GLint; xoffset: GLint; yoffset: GLint; zoffset: GLint; height: GLsizei; depth: GLsizei; format: GLenum; _type: GLenum);
    //procedure texSubImage3D (target: GLenum; level: GLint; xoffset: GLint; yoffset: GLint; zoffset: GLint; height: GLsizei; depth: GLsizei; format: GLenum; _type: GLenum);
    procedure texSubImage3D (target: GLenum; level: GLint; xoffset: GLint; yoffset: GLint; zoffset: GLint; height: GLsizei; depth: GLsizei; format: GLenum; _type: GLenum; srcOffset: GLuint);
    procedure copyTexSubImage3D (target: GLenum; level: GLint; xoffset: GLint; yoffset: GLint; zoffset: GLint; y: GLint; width: GLsizei; height: GLsizei);
    procedure compressedTexImage2D (target: GLenum; level: GLint; internalformat: GLenum; width: GLsizei; border: GLint; imageSize: GLsizei; offset: GLintptr);
    procedure compressedTexImage2D (target: GLenum; level: GLint; internalformat: GLenum; width: GLsizei; border: GLint; srcData: TJSArrayBufferView; srcOffset: GLuint; srcLengthOverride: GLuint);
    procedure compressedTexImage3D (target: GLenum; level: GLint; internalformat: GLenum; width: GLsizei; depth: GLsizei; border: GLint; imageSize: GLsizei; offset: GLintptr);
    procedure compressedTexImage3D (target: GLenum; level: GLint; internalformat: GLenum; width: GLsizei; depth: GLsizei; border: GLint; srcData: TJSArrayBufferView; srcOffset: GLuint; srcLengthOverride: GLuint);
    procedure compressedTexSubImage2D (target: GLenum; level: GLint; xoffset: GLint; yoffset: GLint; height: GLsizei; format: GLenum; imageSize: GLsizei; offset: GLintptr);
    procedure compressedTexSubImage2D (target: GLenum; level: GLint; xoffset: GLint; yoffset: GLint; height: GLsizei; format: GLenum; srcOffset: GLuint; srcLengthOverride: GLuint);
    procedure compressedTexSubImage3D (target: GLenum; level: GLint; xoffset: GLint; yoffset: GLint; width: GLsizei; height: GLsizei; depth: GLsizei; imageSize: GLsizei; offset: GLintptr);
    procedure compressedTexSubImage3D (target: GLenum; level: GLint; xoffset: GLint; yoffset: GLint; width: GLsizei; height: GLsizei; depth: GLsizei; srcData: TJSArrayBufferView; srcOffset: GLuint; srcLengthOverride: GLuint);

    // Programs and shaders
    function getFragDataLocation (_program: TJSWebGLProgram; name: TJSDOMString): GLint;

    // Uniforms
    procedure uniform1ui (location: TJSWebGLUniformLocation; v0: GLuint);
    procedure uniform2ui (location: TJSWebGLUniformLocation; v0: GLuint; v1: GLuint);
    procedure uniform3ui (location: TJSWebGLUniformLocation; v0: GLuint; v1: GLuint; v2: GLuint);
    procedure uniform4ui (location: TJSWebGLUniformLocation; v0: GLuint; v1: GLuint; v2: GLuint; v3: GLuint);
    procedure uniform1fv (location: TJSWebGLUniformLocation; data: TJSFloat32List; srcOffset: GLuint; srcLength: GLuint);
    procedure uniform2fv (location: TJSWebGLUniformLocation; data: TJSFloat32List; srcOffset: GLuint; srcLength: GLuint);
    procedure uniform3fv (location: TJSWebGLUniformLocation; data: TJSFloat32List; srcOffset: GLuint; srcLength: GLuint);
    procedure uniform4fv (location: TJSWebGLUniformLocation; data: TJSFloat32List; srcOffset: GLuint; srcLength: GLuint);
    procedure uniform1iv (location: TJSWebGLUniformLocation; data: TJSInt32List; srcOffset: GLuint; srcLength: GLuint);
    procedure uniform2iv (location: TJSWebGLUniformLocation; data: TJSInt32List; srcOffset: GLuint; srcLength: GLuint);
    procedure uniform3iv (location: TJSWebGLUniformLocation; data: TJSInt32List; srcOffset: GLuint; srcLength: GLuint);
    procedure uniform4iv (location: TJSWebGLUniformLocation; data: TJSInt32List; srcOffset: GLuint; srcLength: GLuint);
    procedure uniform1uiv (location: TJSWebGLUniformLocation; data: TJSUint32List; srcOffset: GLuint; srcLength: GLuint);
    procedure uniform2uiv (location: TJSWebGLUniformLocation; data: TJSUint32List; srcOffset: GLuint; srcLength: GLuint);
    procedure uniform3uiv (location: TJSWebGLUniformLocation; data: TJSUint32List; srcOffset: GLuint; srcLength: GLuint);
    procedure uniform4uiv (location: TJSWebGLUniformLocation; data: TJSUint32List; srcOffset: GLuint; srcLength: GLuint);
    procedure uniformMatrix2fv (location: TJSWebGLUniformLocation; transpose: GLboolean; data: TJSFloat32List; srcOffset: GLuint; srcLength: GLuint);
    procedure uniformMatrix3x2fv (location: TJSWebGLUniformLocation; transpose: GLboolean; data: TJSFloat32List; srcOffset: GLuint; srcLength: GLuint);
    procedure uniformMatrix4x2fv (location: TJSWebGLUniformLocation; transpose: GLboolean; data: TJSFloat32List; srcOffset: GLuint; srcLength: GLuint);
    procedure uniformMatrix2x3fv (location: TJSWebGLUniformLocation; transpose: GLboolean; data: TJSFloat32List; srcOffset: GLuint; srcLength: GLuint);
    procedure uniformMatrix3fv (location: TJSWebGLUniformLocation; transpose: GLboolean; data: TJSFloat32List; srcOffset: GLuint; srcLength: GLuint);
    procedure uniformMatrix4x3fv (location: TJSWebGLUniformLocation; transpose: GLboolean; data: TJSFloat32List; srcOffset: GLuint; srcLength: GLuint);
    procedure uniformMatrix2x4fv (location: TJSWebGLUniformLocation; transpose: GLboolean; data: TJSFloat32List; srcOffset: GLuint; srcLength: GLuint);
    procedure uniformMatrix3x4fv (location: TJSWebGLUniformLocation; transpose: GLboolean; data: TJSFloat32List; srcOffset: GLuint; srcLength: GLuint);
    procedure uniformMatrix4fv (location: TJSWebGLUniformLocation; transpose: GLboolean; data: TJSFloat32List; srcOffset: GLuint; srcLength: GLuint);

    // Vertex attribs
    procedure vertexAttribI4i (index: GLuint; x: GLint; y: GLint; z: GLint; w: GLint);
    procedure vertexAttribI4iv (index: GLuint; values: TJSInt32List);
    procedure vertexAttribI4ui (index: GLuint; x: GLuint; y: GLuint; z: GLuint; w: GLuint);
    procedure vertexAttribI4uiv (index: GLuint; values: TJSUint32List);
    procedure vertexAttribIPointer (index: GLuint; size: GLint; _type: GLenum; stride: GLsizei; offset: GLintptr);

    // Writing to the drawing buffer
    procedure vertexAttribDivisor (index: GLuint; divisor: GLuint);
    procedure drawArraysInstanced (mode: GLenum; first: GLint; count: GLsizei; instanceCount: GLsizei);
    procedure drawElementsInstanced (mode: GLenum; count: GLsizei; _type: GLenum; offset: GLintptr; instanceCount: GLsizei);
    procedure drawRangeElements (mode: GLenum; start: GLuint; _end: GLuint; count: GLsizei; _type: GLenum; offset: GLintptr);

    // Reading back pixels

    // WebGL1:
    procedure readPixels (x: GLint; y: GLint; width: GLsizei; height: GLsizei; format: GLenum; _type: GLenum);
    // WebGL2:
    //procedure readPixels (x: GLint; y: GLint; width: GLsizei; height: GLsizei; format: GLenum; _type: GLenum);
    procedure readPixels (x: GLint; y: GLint; width: GLsizei; height: GLsizei; format: GLenum; _type: GLenum; dstOffset: GLuint);

    // Multiple Render Targets */
    procedure drawBuffers (buffers: GLenumDynArray);
    procedure clearBufferfv (buffer: GLenum; drawbuffer: GLint; values: TJSFloat32List; srcOffset: GLuint);
    procedure clearBufferiv (buffer: GLenum; drawbuffer: GLint; values: TJSInt32List; srcOffset: GLuint);
    procedure clearBufferuiv (buffer: GLenum; drawbuffer: GLint; values: TJSUint32List; srcOffset: GLuint);
    procedure clearBufferfi (buffer: GLenum; drawbuffer: GLint; depth: GLfloat; stencil: GLint);

    // Query Objects
    function createQuery: TJSWebGLQuery;
    procedure deleteQuery (query: TJSWebGLQuery);
    function isQuery (query: TJSWebGLQuery): GLboolean;
    procedure beginQuery (target: GLenum; query: TJSWebGLQuery);
    procedure endQuery (target: GLenum);
    function getQuery (target: GLenum; pname: GLenum): TJSWebGLQuery;
    function getQueryParameter (query: TJSWebGLQuery; pname: GLenum): JSValue;

    // Sampler Objects
    function createSampler: TJSWebGLSampler;
    procedure deleteSampler (sampler: TJSWebGLSampler);
    function isSampler (sampler: TJSWebGLSampler): GLboolean;
    procedure bindSampler (_unit: GLuint; sampler: TJSWebGLSampler);
    procedure samplerParameteri (sampler: TJSWebGLSampler; pname: GLenum; param: GLint);
    procedure samplerParameterf (sampler: TJSWebGLSampler; pname: GLenum; param: GLfloat);
    function getSamplerParameter (sampler: TJSWebGLSampler; pname: GLenum): JSValue;

    // Sync objects
    function fenceSync (condition: GLenum; flags: GLbitfield): TJSWebGLSync;
    function isSync (sync: TJSWebGLSync): GLboolean;
    procedure deleteSync (sync: TJSWebGLSync);
    function clientWaitSync (sync: TJSWebGLSync; flags: GLbitfield; timeout: GLuint64): GLenum;
    procedure waitSync (sync: TJSWebGLSync; flags: GLbitfield; timeout: GLint64);
    function getSyncParameter (sync: TJSWebGLSync; pname: GLenum): JSValue;

    // Transform Feedback
    function createTransformFeedback: TJSWebGLTransformFeedback;
    procedure deleteTransformFeedback (tf: TJSWebGLTransformFeedback);
    function isTransformFeedback (tf: TJSWebGLTransformFeedback): GLboolean;
    procedure bindTransformFeedback (target: GLenum; tf: TJSWebGLTransformFeedback);
    procedure beginTransformFeedback (primitiveMode: GLenum);
    procedure endTransformFeedback;
    procedure transformFeedbackVaryings (_program: TJSWebGLProgram; varyings: array of TJSDOMString; bufferMode: GLenum);
    function getTransformFeedbackVarying (_program: TJSWebGLProgram; index: GLuint): TJSWebGLActiveInfo;
    procedure pauseTransformFeedback;
    procedure resumeTransformFeedback;

    // Uniform Buffer Objects and Transform Feedback Buffers
    procedure bindBufferBase (target: GLenum; index: GLuint; buffer: TJSWebGLBuffer);
    procedure bindBufferRange (target: GLenum; index: GLuint; buffer: TJSWebGLBuffer; offset: GLintptr; size: GLsizeiptr);
    function getIndexedParameter (target: GLenum; index: GLuint): JSValue;
    function getUniformIndices (_program: TJSWebGLProgram; uniformNames: array of TJSDOMString): GLuintDynArray;
    function getActiveUniforms (_program: TJSWebGLProgram; uniformIndices: GLuintDynArray; pname: GLenum): JSValue;
    function getUniformBlockIndex (_program: TJSWebGLProgram; uniformBlockName: TJSDOMString): GLuint;
    function getActiveUniformBlockParameter (_program: TJSWebGLProgram; uniformBlockIndex: GLuint; pname: GLenum): JSValue;
    function getActiveUniformBlockName (_program: TJSWebGLProgram; uniformBlockIndex: GLuint): TJSDOMString;
    procedure uniformBlockBinding (_program: TJSWebGLProgram; uniformBlockIndex: GLuint; uniformBlockBinding: GLuint);

    // Vertex Array Objects
    function createVertexArray: TJSWebGLVertexArrayObject;
    procedure deleteVertexArray (vertexArray: TJSWebGLVertexArrayObject);
    function isVertexArray (vertexArray: TJSWebGLVertexArrayObject): GLboolean;
    procedure bindVertexArray (_array: TJSWebGLVertexArrayObject);
end;

TJSWebGL2RenderingContext = class external name 'WebGL2RenderingContext' (TJSWebGL2RenderingContextBase);

implementation
end.
