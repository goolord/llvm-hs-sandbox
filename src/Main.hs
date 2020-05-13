{-# LANGUAGE 
    OverloadedStrings 
  , RecursiveDo
  , ScopedTypeVariables
#-}

import JIT
import LLVM.IRBuilder
import Data.ByteString.Short (ShortByteString)
import Data.Word
import qualified LLVM.AST as AST
import qualified LLVM.AST.Type as AST
import qualified LLVM.AST.IntegerPredicate as AST
import qualified LLVM.AST.Constant as C
import qualified LLVM.AST.Float as F

var :: MonadIRBuilder m => ShortByteString -> m r -> m r
var name ir = ir `named` name

equalsVal :: Word32 -> Word32 -> ModuleBuilder ()
equalsVal bitSz size = do
  function "equalsVal" 
    [ (AST.ptr $ AST.VectorType size (AST.IntegerType bitSz),"src0")
    , (AST.ptr AST.i8, "dst0")
    , (AST.i64, "len0")
    , ((AST.IntegerType bitSz),"val0")
    ] AST.void 
    $ \[src0, dst0, len0, val0] -> mdo
      ---
      entry <- freshName "entry"
      emitBlockStart entry
      len <- var "len" $ emitInstr AST.i64 $ AST.UDiv True len0 (AST.ConstantOperand $ C.Int 64 8) []
      br cond
      ---
      cond <- freshName "cond"
      emitBlockStart cond
      i <- var "i" $ phi [(AST.ConstantOperand $ C.Int 64 0, entry), (isucc, loop)]
      src <- var "src" $ phi [(src0, entry), (srcsucc, loop)]
      dst <- var "dst" $ phi [(dst0, entry), (dstsucc, loop)]
      cmp <- var "cmp" $ icmp AST.SLT i len
      condBr cmp loop end
      ---
      loop <- freshName "loop"
      emitBlockStart loop
      isucc <- var "isucc" $ add i (AST.ConstantOperand $ C.Int 64 1)
      srcsucc <- var "srcsucc" $ gep src [AST.ConstantOperand $ C.Int 64 1]
      dstsucc <- var "dstsucc" $ gep dst [AST.ConstantOperand $ C.Int 64 1]
      val <- var "val" $ load src 1024
      bits <- var "bits" $ icmp AST.EQ val =<< do
        vec0 <- alloca (AST.VectorType 1 $ AST.IntegerType bitSz) Nothing 1024
        vec1 <- insertElement vec0 val0 (AST.ConstantOperand $ C.Int 32 0)
        vec2 <- load vec1 1024
        shuffleVector 
          vec2
          (AST.ConstantOperand $ C.Vector [C.Int bitSz 0]) 
          (C.Vector $ replicate (fromIntegral size) (C.Int 32 0))
      res <- var "res" $ bitcast bits AST.i8
      store res 1024 dst
      br cond
      ---
      end <- freshName "end"
      emitBlockStart end
      retVoid
      ---
  pure ()

main :: IO AST.Module
main = do
  let ast = buildModule "ex" (equalsVal 32 16)
  rc <- runJIT ast
  pure ast
