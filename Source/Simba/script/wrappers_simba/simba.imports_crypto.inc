FSection := 'Crypto';
addGlobalFunc('function BlowFish_Encrypt(constref Data, Password: String): String', @_LapeBlowFish_Encrypt);
addGlobalFunc('function BlowFish_Decrypt(constref Data, Password: String): String', @_LapeBlowFish_Decrypt);
addGlobalFunc('function MD5String(constref Data: String): String', @_LapeMD5String);
addGlobalFunc('function MD5File(constref FileName: String): String', @_LapeMD5File);
addGlobalFunc('function SHA1String(constref Data: String): String', @_LapeSHA1String);
addGlobalFunc('function SHA1File(constref FileName: String): String', @_LapeSHA1File);
addGlobalFunc('function HMACMD5(constref Key, Message: String): String', @_LapeHMACMD5);
addGlobalFunc('function HMACSHA1(constref Key, Message: String): String', @_LapeHMACSHA1);

