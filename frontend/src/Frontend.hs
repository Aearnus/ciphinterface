{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE RankNTypes #-}


module Frontend where

import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.Read as T

import Obelisk.Frontend
import Obelisk.Configs
import Obelisk.Route
import Obelisk.Generated.Static

import Reflex.Dom.Core

import Common.Api
import Common.Route
import Common.Cipherland


encodeWidget :: (DomBuilder t m, PostBuild t m) => Stringrep -> Cipher k -> Dynamic t k -> m ()
encodeWidget strep cipher key = mdo
  toEnc <- inputElement def
  el "p" $ dynText . fmap T.pack $ (encCipher strep cipher <$> key) <*> (T.unpack <$> value toEnc) 

frontend :: Frontend (R FrontendRoute)
frontend = Frontend
  { _frontend_head = do 
      el "title" $ text "Ciphers"
      elAttr "link" ("href" =: static @"main.css" <> "type" =: "text/css" <> "rel" =: "stylesheet") blank
      elAttr "link" ("href" =: static @"pure-min.css" <> "type" =: "text/css" <> "rel" =: "stylesheet") blank
  , _frontend_body = mdo
      rotNum <- inputElement $ def 
        & inputElementConfig_elementConfig . elementConfig_initialAttributes .~ ("type" =: "number")
      encodeWidget alphabetic' rot $ (either (const 0) fst . T.decimal) <$> value rotNum
      el "br" $ blank
      mNum <- inputElement $ def 
        & inputElementConfig_elementConfig . elementConfig_initialAttributes .~ ("type" =: "number")
      bNum <- inputElement $ def 
        & inputElementConfig_elementConfig . elementConfig_initialAttributes .~ ("type" =: "number")
      encodeWidget alphabetic' affine $ ((either (const 0) fst . T.decimal) <$> value mNum, (either (const 0) fst . T.decimal) <$> value bNum)
      return ()
  }
