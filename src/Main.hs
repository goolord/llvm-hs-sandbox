{-# LANGUAGE 
    OverloadedStrings 
  , RecursiveDo
#-}

import JIT
import LLVM.IRBuilder
import Data.ByteString.Short (ShortByteString)
import qualified LLVM.AST as AST
import qualified LLVM.AST.Type as AST
import qualified LLVM.AST.IntegerPredicate as AST
import qualified LLVM.AST.Constant as C
import qualified LLVM.AST.Float as F

{-

; ModuleID = 'ex'

define void @equals42(<8 x i16>* %src0,i8* %dst0,i64 %len0) { ; i32()*
entry:
    %len = udiv exact i64 %len0, 8
    br label %cond
cond:
    %i = phi i64 [ 0, %entry ], [ %isucc, %loop ]
    %src = phi <8 x i16>* [ %src0, %entry ], [ %srcsucc, %loop ]
    %dst = phi i8* [ %dst0, %entry ], [ %dstsucc, %loop ]
    %cmp = icmp slt i64 %i, %len
    br i1 %cmp, label %loop, label %end
loop:
    %isucc = add i64 %i, 1
    %srcsucc = getelementptr <8 x i16>, <8 x i16>* %src, i64 1
    %dstsucc = getelementptr i8, i8* %dst, i64 1
    %val = load <8 x i16>, <8 x i16>* %src
    %bits = icmp eq <8 x i16> %val, <i16 42,i16 42,i16 42,i16 42,i16 42,i16 42,i16 42,i16 42>
    %res = bitcast <8 x i1> %bits to i8
    store i8 %res, i8* %dst
    br label %cond
end:
    ret void;
}

-}

var :: MonadIRBuilder m => ShortByteString -> m r -> m r
var name ir = ir `named` name

equals42 :: ModuleBuilder ()
equals42 = do
  function "equals42" 
    [ (AST.ptr $ AST.VectorType 8 AST.i16,"src0")
    , (AST.ptr AST.i8, "dst0")
    , (AST.i64, "len0")
    ] AST.void 
    $ \[src0, dst0, len0] -> mdo
      ---
      entry <- freshName "entry"
      emitBlockStart entry
      len <- var "len" $ emitInstr AST.i64 $ AST.UDiv True len0 (AST.ConstantOperand $ C.Int 64 8) []
      br cond
      ---
      cond <- freshName "cond"
      emitBlockStart cond
      i <- var "i" $ phi [(AST.ConstantOperand $ C.Int 64 0, entry), (isucc, loop)]
      src <- var "src" $ phi [(src0, entry), (srcsucc, loop) ]
      dst <- var "dst" $ phi [(dst0, entry), (dstsucc, loop)]
      cmp <- var "cmp" $ icmp AST.SLT i len
      condBr cmp loop end
      ---
      loop <- freshName "loop"
      emitBlockStart loop
      isucc <- var "isucc" $ add i (AST.ConstantOperand $ C.Int 64 1)
      srcsucc <- var "srcsucc" $ gep src [AST.ConstantOperand $ C.Int 64 1]
      dstsucc <- var "dstsucc" $ gep dst [AST.ConstantOperand $ C.Int 64 1]
      val <- var "val" $ load src 128
      bits <- var "bits" $ icmp AST.EQ val $ AST.ConstantOperand $ C.Vector $ fmap (C.Int 16) [42,42,42,42,42,42,42,42]
      res <- var "res" $ bitcast bits AST.i8
      store res 1 dst
      br cond
      ---
      end <- freshName "end"
      emitBlockStart end
      retVoid
      ---
  pure ()

main :: IO AST.Module
main = do
  let ast = buildModule "ex" equals42
  rc <- runJIT ast
  pure ast
