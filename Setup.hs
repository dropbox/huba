import Distribution.Simple
import System.Process (callCommand)

main = defaultMainWithHooks simpleUserHooks { preBuild = tackOn runThrift (preBuild simpleUserHooks) }
  where tackOn f hook args bf = f >> hook args bf
        runThrift = callCommand "command -v thrift > /dev/null && thrift -r --gen hs huba.thrift"
