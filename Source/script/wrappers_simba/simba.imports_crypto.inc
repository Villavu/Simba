pushSection('Crypto');

addGlobalFunc('function BlowFish_Encrypt(const Data, Password: String): String', @_LapeBlowFish_Encrypt);
addGlobalFunc('function BlowFish_Decrypt(const Data, Password: String): String', @_LapeBlowFish_Decrypt);
addGlobalFunc('function MD5String(const Data: String): String', @_LapeMD5String);
addGlobalFunc('function MD5File(const FileName: String): String', @_LapeMD5File);
addGlobalFunc('function SHA1String(const Data: String): String', @_LapeSHA1String);
addGlobalFunc('function SHA1File(const FileName: String): String', @_LapeSHA1File);
addGlobalFunc('function HMACMD5(const Key, Message: String): String', @_LapeHMACMD5);
addGlobalFunc('function HMACSHA1(const Key, Message: String): String', @_LapeHMACSHA1);

addGlobalFunc('function CompressString(S: String): String', @_LapeCompressString);
addGlobalFunc('function DecompressString(S: String): String', @_LapeDecompressString);
addGlobalFunc('function Base64Encode(S: String): String', @_LapeBase64Encode);
addGlobalFunc('function Base64Decode(S: String): String', @_LapeBase64Decode);

addGlobalFunc('function SynLZCompressDestLen(in_len: Integer): Integer', @_LapeSynLZCompressDestLen);
addGlobalFunc('function SynLZDecompressDestLen(in_p: Pointer): Integer', @_LapeSynLZDecompressDestLen);
addGlobalFunc('function SynLZCompress(src: Pointer; size: integer; dst: Pointer): Integer', @_LapeSynLZCompress);
addGlobalFunc('function SynLZDecompress(src: Pointer; size: integer; dst: Pointer): Integer', @_LapeSynLZDecompress);

popSection();

