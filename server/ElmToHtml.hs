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
module ElmToHtml (elmToHtml) where

import Text.Blaze (preEscapedToMarkup)
import Text.Blaze.Html5 ((!))
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import Network.HTTP.Base (urlEncode)

import Language.Elm (toParts)
import Utils

-- | Using a page title and the full source of an Elm program, compile down to
--   a valid HTML document.
elmToHtml :: String -> String -> H.Html
elmToHtml name src =
  let (body, css, js) = toParts src in
  H.docTypeHtml $ do
      H.head $ do
        H.meta ! A.charset "UTF-8"
        H.title . H.toHtml $ name
        css
        H.style ! A.type_ "text/css" $ preEscapedToMarkup
         ("a:link {text-decoration: none; color: rgb(15,102,230);}\
          \a:visited {text-decoration: none}\
          \a:active {text-decoration: none}\
          \a:hover {text-decoration: underline; color: rgb(234,21,122);}" :: String)
      H.body $ do
        body
        H.script ! A.type_ "text/javascript" ! A.src (H.toValue ("/elm-runtime.js" :: String)) $ ""
        H.script ! A.type_ "text/javascript" ! A.src (H.toValue ("/FrameRate.js" :: String)) $ ""
        H.script ! A.type_ "text/javascript" ! A.src (H.toValue ("/Config.js" :: String)) $ ""
        H.script ! A.type_ "text/javascript" $ preEscapedToMarkup js
        H.script ! A.type_ "text/javascript" $ "Dispatcher.initialize()"
