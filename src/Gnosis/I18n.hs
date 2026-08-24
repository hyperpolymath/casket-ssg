{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE OverloadedStrings #-}

module Gnosis.I18n (
    LolHandle,
    initLol,
    freeLol,
    resolveLang,
    translate,
    translatePlural,
    getLastError
) where

import Foreign
import Foreign.C.Types
import Foreign.C.String
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Foreign as TF
import System.IO.Unsafe (unsafePerformIO)
import Foreign.Ptr (nullPtr)

data LolHandleStruct
type LolHandle = Ptr LolHandleStruct

data LolTranslationResult
data LolLocale

foreign import ccall "lol_init" c_lol_init :: CString -> IO LolHandle
foreign import ccall "lol_free" c_lol_free :: LolHandle -> IO ()
foreign import ccall "lol_resolve_locale" c_lol_resolve_locale :: LolHandle -> CString -> IO (Ptr LolLocale)
foreign import ccall "lol_free_locale" c_lol_free_locale :: Ptr LolLocale -> IO ()
foreign import ccall "lol_translate" c_lol_translate :: LolHandle -> CString -> CString -> IO (Ptr LolTranslationResult)
foreign import ccall "lol_translate_plural" c_lol_translate_plural :: LolHandle -> CString -> CString -> Word64 -> IO (Ptr LolTranslationResult)
foreign import ccall "lol_translation_text" c_lol_translation_text :: Ptr LolTranslationResult -> IO CString
foreign import ccall "lol_free_translation" c_lol_free_translation :: Ptr LolTranslationResult -> IO ()
foreign import ccall "lol_last_error" c_lol_last_error :: IO CString

initLol :: FilePath -> IO LolHandle
initLol path = withCString path c_lol_init

freeLol :: LolHandle -> IO ()
freeLol = c_lol_free

resolveLang :: LolHandle -> Text -> IO (Maybe Text)
resolveLang handle tag = TF.withCStringLen tag $ \(ptr, len) -> do
    -- Using withCString since lol_resolve_locale expects null-terminated
    tagStr <- peekCStringLen (ptr, len)
    withCString tagStr $ \cTag -> do
        locPtr <- c_lol_resolve_locale handle cTag
        if locPtr == nullPtr
            then return Nothing
            else do
                -- In a real implementation we would extract the lang/dir fields from the struct
                -- For now just return the tag to mirror missing
                c_lol_free_locale locPtr
                return (Just tag)

translate :: LolHandle -> Text -> Text -> IO Text
translate handle locale key = 
    withTextCString locale $ \cLoc ->
    withTextCString key $ \cKey -> do
        resPtr <- c_lol_translate handle cLoc cKey
        if resPtr == nullPtr
            then return $ "(:MISSING:" <> key <> ")"
            else do
                cStr <- c_lol_translation_text resPtr
                res <- if cStr == nullPtr
                        then return $ "(:MISSING:" <> key <> ")"
                        else peekCString cStr >>= return . T.pack
                c_lol_free_translation resPtr
                return res

translatePlural :: LolHandle -> Text -> Text -> Word64 -> IO Text
translatePlural handle locale key quantity = 
    withTextCString locale $ \cLoc ->
    withTextCString key $ \cKey -> do
        resPtr <- c_lol_translate_plural handle cLoc cKey quantity
        if resPtr == nullPtr
            then return $ "(:MISSING:" <> key <> ")"
            else do
                cStr <- c_lol_translation_text resPtr
                res <- if cStr == nullPtr
                        then return $ "(:MISSING:" <> key <> ")"
                        else peekCString cStr >>= return . T.pack
                c_lol_free_translation resPtr
                return res

getLastError :: IO (Maybe Text)
getLastError = do
    cStr <- c_lol_last_error
    if cStr == nullPtr
        then return Nothing
        else do
            str <- peekCString cStr
            return $ Just (T.pack str)

withTextCString :: Text -> (CString -> IO a) -> IO a
withTextCString t action = do
    let str = T.unpack t
    withCString str action
