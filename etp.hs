import Codec.MIME.Parse
import Codec.MIME.Type
import Data.Text (Text, pack)
import Data.Tree
import GHC.IO.Encoding
import System.Environment
import System.Exit
import qualified Data.Text.IO as T

extractTextPlain :: MIMEValue -> [Text]
extractTextPlain MIMEValue
	{ mime_val_disp = Just Disposition
		{ dispType = DispAttachment
		}
	} = []
extractTextPlain MIMEValue
	{ mime_val_type = Type
		{ mimeType = Multipart Alternative
		}
	, mime_val_content = Multi vs
	} = take 1 . reverse $ vs >>= extractTextPlain
	-- TODO: Is that really right? Suppose the last alternative is a
	-- multipart/mixed with several text/plain pieces. What should
	-- happen? What does happen?
extractTextPlain MIMEValue
	{ mime_val_type = Type
		{ mimeType = Multipart Mixed
		}
	, mime_val_content = Multi vs
	} = vs >>= extractTextPlain
extractTextPlain MIMEValue
	{ mime_val_type = Type
		{ mimeType = Text ty
		-- TODO: what the heck are we supposed to do with the charset param?
		-- (Does the parser already handle this?)
		}
	, mime_val_content = Single txt
	} = [txt | ty == pack "plain"]
extractTextPlain _ = []

main = do
	setLocaleEncoding latin1
	args <- getArgs
	eml  <- case args of
		[filename] -> T.readFile filename
		_ -> do
			me <- getProgName
			die $ "USAGE: " ++ me ++ " FILENAME\n" ++
			      "Extracts the text/plain part of a MIME message, if there is a unique one."
	case extractTextPlain (parseMIMEMessage eml) of
		[t] -> T.putStr t
		[]  -> die "Could not find a text/plain part"
		_   -> die "Found multiple text/plain parts"
