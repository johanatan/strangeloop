{-
Copyright (c)2012, Evan Czaplicki

All rights reserved.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are met:

    * Redistributions of source code must retain the above copyright
      notice, this list of conditions and the following disclaimer.

    * Redistributions in binary form must reproduce the above
      copyright notice, this list of conditions and the following
      disclaimer in the documentation and/or other materials provided
      with the distribution.

    * Neither the name of Evan Czaplicki nor the names of other
      contributors may be used to endorse or promote products derived
      from this software without specific prior written permission.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
"AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
(INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
-}

{-# LANGUAGE OverloadedStrings #-}
module Main where

import Prelude hiding (head,span,id)
import Control.Monad (msum,when)
import Happstack.Server hiding (body)
import Happstack.Server.Compression

import Text.Blaze.Html (Html)
import Control.Monad.Trans (MonadIO(liftIO))

import ElmToHtml
import Editor
import Utils

-- | Set up the server.
main :: IO ()
main = simpleHTTP nullConf $ do
         compressedResponseFilter
         msum [ nullDir >> compileFile "StrangeLoop.elm"
              , serveDirectory DisableBrowsing [] "resources"
              , dir "compile" $ compilePart
              , dir "edit" . uriRest $ withFile ide
              , dir "code" . uriRest $ withFile editor
              , dir "login" sayHi
              , uriRest compileFile
              ]

-- | Compile an Elm program that has been POST'd to the server.
compilePart :: ServerPart Response
compilePart = do
  decodeBody $ defaultBodyPolicy "/tmp/" 0 10000 1000
  code <- look "input"
  ok $ toResponse $ elmToHtml "Compiled Elm" code

-- | Do something with the contents of a File.
withFile :: (FilePath -> String -> Html) -> FilePath -> ServerPart Response
withFile handler fp = do
  content <- liftIO $ readFile ("public/" ++ fp)
  ok . toResponse $ handler fp content

-- | Compile an arbitrary Elm program from the public/ directory.
compileFile :: FilePath -> ServerPart Response
compileFile fp =
    do content <- liftIO $ readFile ("public/" ++ fp)
       ok $ toResponse $ elmToHtml (pageTitle fp) content

-- | Simple response for form-validation demo.
sayHi :: ServerPart Response
sayHi = do
  first <- look "first"
  last  <- look "last"
  email <- look "email"
  ok . toResponse $
     concat [ "Hello, ", first, " ", last
            , "! Welcome to the fake login-confirmation page.\n\n"
            , "We will not attempt to contact you at ", email
            , ".\nIn fact, your (fake?) email has not even been recorded." ]
