{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Config             as C
import qualified Data.Map.Strict    as M
import qualified Data.Text          as T
import qualified Data.Text.IO       as IT
import qualified System.Environment as Env

main :: IO ()
main = do
    args <- Env.getArgs
    cfg <- case args
            of []  -> C.config
               [f] -> loadOrError f
               _   -> error "Script takes none or one argument"
    IT.putStr $ configToShellVariables cfg

loadOrError :: FilePath -> IO C.Config
loadOrError f = do
    eitherC <- C.loadConfig f
    return $ case eitherC of
        Left err -> error err
        Right c  -> c

configToShellVariables :: C.Config -> T.Text
configToShellVariables (C.Config c) = M.foldrWithKey kvToLine T.empty c
    where kvToLine k v s = s `T.append` k `T.append` "=\"" `T.append` v `T.append` "\"\n"

