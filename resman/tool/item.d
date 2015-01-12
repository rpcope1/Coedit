module item;

import core.exception;

import std.file, std.path;
import std.string: format;

import utils;

enum ResFormat {bytes, utf8, base16, base64, z85, e7F}

alias ResourceItems = ResourceItem * [];

/**
 * Resource item.
 * The resource properties and the encoded form are generated in the constructor.
 */
struct ResourceItem{
    private:
        static const resFileMsg = "resource file '%s' ";
        ResFormat _resFormat;
        string _resIdentifier;
        string _resMetaData;
        char[] _resTxtData;
        ubyte[] _resRawData;
        uint _initialSum;
        uint _encodedSum;

        bool encodeDispatcher(){
            final switch(this._resFormat){
                case ResFormat.bytes:
                    return encodeUtf8(); // should add to a ubyte[][] import(filename)
                case ResFormat.utf8:
                    return encodeUtf8();
                case ResFormat.base16:
                    return encodeb16();
                case ResFormat.base64:
                    return encodeb64();
                case ResFormat.z85:
                    return encodez85();
                case ResFormat.e7F:
                    return encodee7F();
            }
        }

        /// encodes _resRawData to an UTF8 string
        bool encodeUtf8(){
            scope(failure) return false;
            _resTxtData = (cast(char[])_resRawData).dup;
            return true;
        }

        /// encodes _resRawData to a hex string
        bool encodeb16(){
            scope(failure) return false;
            foreach(b; _resRawData)
                this._resTxtData ~= ubyte2Hex(b);
            assert(_resTxtData.length == _resRawData.length * 2,
                "b16 representation length mismatches");
            return true;
        }

        /// encodes _resRawData to a base64 string
        bool encodeb64(){
            import std.base64: Base64;
            scope(failure) return false;
            _resTxtData = Base64.encode(_resRawData);
            return true;
        }

        /// encodes _resRawData to a Z85 string
        bool encodez85(){
            import z85: Z85_encode;
            scope(failure) return false;
            _resTxtData = Z85_encode(_resRawData);
            assert(_resTxtData.length == _resRawData.length * 5 / 4,
                "z85 representation length mismatches");
            return true;
        }
        
        /// encodes _resRawData to an e7F string
        bool encodee7F()
        {
            import e7F: encode_7F;
            scope(failure) return false;
            _resTxtData = encode_7F(_resRawData);
            return true;
        }

    public:

        /** 
         * Creates and encodes a new resource item.
         *
         * Params:
         *
         * resFile = the file whose content is to encode.
         * resEnc = the encoding format.
         * resIdent = optional string used as identifier. When not set, the filename is used.
         * resMeta = optional string used as metadata.
         *
         * Examples:
         * --------
         * auto item = new Item("\folder\strings_en.txt", ResEncoding.e7F, "string_english");
         * auto item = new Item("\folder\strings_fr.txt", ResEncoding.e7F, "string_frecnh");
         * --------
         */
        this(string resFile, ResFormat resFormat, string resIdent = "", string resMeta = "")
        {
            this._resFormat = resFormat;

            // load the raw content
            if (!resFile.exists)
                throw new Exception(format(resFileMsg ~ "does not exist", resFile));
            else
                _resRawData = cast(ubyte[])std.file.read(resFile);
            if (!_resRawData.length)
                throw new Exception(format(resFileMsg ~ "is empty", resFile));

            import std.digest.crc: CRC32;
            CRC32 ihash;
            ihash.put(_resRawData);
            _initialSum = crc322uint(ihash.finish);

            // sets the resource identifier to the res filename if param is empty
            this._resIdentifier = resIdent;
            if (this._resIdentifier == "")
                this._resIdentifier = resFile.baseName.stripExtension;
                
            this._resMetaData = resMeta;

            if (!encodeDispatcher)
                throw new Exception(format(resFileMsg ~ "encoding failed", resFile));
            this._resRawData.length = 0;
            CRC32 ehash;
            ehash.put(cast(ubyte[])_resTxtData);
            _encodedSum = crc322uint(ehash.finish);

            writeMessage(true, format("encoded resource file '%s'", resFile));
        }

        /// returns the resource encoded as a string.
        char[] resText(){return _resTxtData;}

        /// returns the resource identifier.
        string resIdentifier(){return _resIdentifier;}
        
        /// returns the resource metadata.
        string resMetaData(){return _resMetaData;}

        /// returns the resource encoding kind.
        ResFormat resFormat(){return _resFormat;}

        /// returns the signature of the original data.
        uint initialSum(){return _initialSum;}

        /// returns the signature of the encoded data.
        uint encodedSum(){return _encodedSum;}
}
