{-# LANGUAGE FlexibleContexts #-}

{- |
Module                  : Iris.Colour.Formatting
Copyright               : (c) 2022 Dmitrii Kovanikov
SPDX-License-Identifier : MPL-2.0
Maintainer              : Dmitrii Kovanikov <kovanikov@gmail.com>
Stability               : Experimental
Portability             : Portable

Helper functions to print with colouring.

@since 0.0.0.0
-}

module Iris.Colour.Formatting
    ( putStdoutColouredLn
    , putStderrColouredLn
    , putStdoutColoured
    , putStderrColoured
    ) where

import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.Reader (MonadReader)
import qualified Data.Text.IO as T
import Data.Text (Text)
import System.IO (stderr)

import Iris.Colour.Mode (ColourMode (..))
import Iris.Env (CliEnv (..), asksCliEnv)

import qualified Data.Text.IO as TIO

{- | Print 'Text' to 'System.IO.stdout' by providing a custom
formatting function.

This works especially well with the @colourista@ package:

@
'putStdoutColouredLn'
    (Colourista.formatWith [Colourista.bold, Colourista.green])
    "my message"
@

@since 0.0.0.0
-}
putStdoutColouredLn
    :: ( MonadReader (CliEnv cmd appEnv) m
       , MonadIO m
       )
    => (Text -> Text)
    -> Text
    -> m ()
putStdoutColouredLn formatWithColour str = do
    colourMode <- asksCliEnv cliEnvStdoutColourMode
    liftIO $ T.putStrLn $ case colourMode of
        DisableColour -> str
        EnableColour  -> formatWithColour str

{- | Print 'Text' to 'System.IO.stderr' by providing a custom
formatting function.

This works especially well with the @colourista@ package:

@
'putStderrColouredLn'
    (Colourista.formatWith [Colourista.bold, Colourista.green])
    "my message"
@

@since 0.0.0.0
-}
putStderrColouredLn
    :: ( MonadReader (CliEnv cmd appEnv) m
       , MonadIO m
       )
    => (Text -> Text)
    -> Text
    -> m ()
putStderrColouredLn formatWithColour str = do
    colourMode <- asksCliEnv cliEnvStderrColourMode
    liftIO $ T.hPutStrLn stderr $ case colourMode of
        DisableColour -> str
        EnableColour  -> formatWithColour str

{- | Print 'Text' to 'System.IO.stdout' by providing a custom
formatting function. Doesn't breaks output line that differs from
`putStdoutColouredLn`

This works especially well with the @colourista@ package:

@
'putStdoutColoured'
    (Colourista.formatWith [Colourista.bold, Colourista.green])
    "my message"
@

@since x.x.x.x
-}
putStdoutColoured
    :: ( MonadReader (CliEnv cmd appEnv) m
       , MonadIO m
       )
    => (Text -> Text)
    -> Text
    -> m ()
putStdoutColoured formatWithColour str = do
    colourMode <- asksCliEnv cliEnvStdoutColourMode
    liftIO $ TIO.putStr $ case colourMode of
        DisableColour -> str
        EnableColour  -> formatWithColour str

{- | Print 'Text' to 'System.IO.stderr' by providing a custom
formatting function. Doesn't breaks output line that differs from
`putStderrColouredLn`

This works especially well with the @colourista@ package:

@
'putStderrColoured'
    (Colourista.formatWith [Colourista.bold, Colourista.green])
    "my message"
@

@since x.x.x.x
-}
putStderrColoured
    :: ( MonadReader (CliEnv cmd appEnv) m
       , MonadIO m
       )
    => (Text -> Text)
    -> Text
    -> m ()
putStderrColoured formatWithColour str = do
    colourMode <- asksCliEnv cliEnvStderrColourMode
    liftIO $ TIO.hPutStr stderr $ case colourMode of
        DisableColour -> str
        EnableColour  -> formatWithColour str
