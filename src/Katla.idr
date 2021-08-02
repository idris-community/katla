module Katla

import Katla.CLI

main : IO ()
main = do
  katlaCmd.handleWith katlaExec
