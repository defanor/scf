-- | Common functions for SCF programs.

module SCF (
  -- * Formats
  module SCF.JSON,
  -- * General
  PBS.stdin
  ) where

import SCF.JSON
import qualified Pipes.ByteString as PBS
