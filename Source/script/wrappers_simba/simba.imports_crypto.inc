pushSection('Crypto');

addGlobalFunc('function BlowFish_Encrypt(const Data, Password: String): String', @_LapeBlowFish_Encrypt);
addGlobalFunc('function BlowFish_Decrypt(const Data, Password: String): String', @_LapeBlowFish_Decrypt);
addGlobalFunc('function MD5String(const Data: String): String', @_LapeMD5String);
addGlobalFunc('function MD5File(const FileName: String): String', @_LapeMD5File);
addGlobalFunc('function SHA1String(const Data: String): String', @_LapeSHA1String);
addGlobalFunc('function SHA1File(const FileName: String): String', @_LapeSHA1File);
addGlobalFunc('function HMACMD5(const Key, Message: String): String', @_LapeHMACMD5);
addGlobalFunc('function HMACSHA1(const Key, Message: String): String', @_LapeHMACSHA1);

popSection();

