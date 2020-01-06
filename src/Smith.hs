-- TODO : find all undefined's

module Smith (
    smith
  ) where

import Prelude ()
import Prelude'

import System.Exit

import Tokens

import Data.List (null, head, iterate, foldl', genericIndex, zip)
import Data.Char
import Data.Map hiding (null)

import Control.Applicative hiding (empty)
import Control.Monad.State

-----------------------------------------------------------

type Index = Integer

data SmithState = SS {
    regs :: Map Index Integer
  , code :: Map Index Operator
  , cp :: Integer -- current position
  }
  deriving (Show)

newSmithState cd = SS {
    regs = empty
  , code = cd
  , cp = 0
  }

type Smith = StateT SmithState IO

-----------------------------------------------------------

smith :: [Operator] -> IO ()
smith = evalStateT (forever runInstruction) . newSmithState . fromList . zip [0..]
  where
    runInstruction = do
      st <- get
      let code' = code st
          cp' = cp st
      execute $ findWithDefault STOP cp' code'
      modify $ \st -> st { cp = 1 + cp st }

-----------------------------------------------------------

regIndex :: Register -> Smith Index
regIndex (REG n) = return n
regIndex (IREG r) = value $ Reg r
regIndex r = error $ "regIndex : " ++ show r

modifyReg :: Register -> (Integer -> Integer) -> Smith ()
modifyReg r f = regIndex r >>= \n -> value (Reg r) >>= \x -> modify $ \st -> st { regs = insert n (f x) $ regs st }

-----------------------------------------------------------

execute :: Operator -> Smith ()

execute (MOV TTY (Left o)) = value o >>= liftIO . putChar . chr . fromInteger
execute (MOV TTY (Right str)) = liftIO $ putStr str
execute (MOV r (Left o)) = value o >>= modifyReg r . const
execute (MOV r (Right str)) = regIndex r >>= \n -> zipWithM_ (
  \n -> modifyReg (REG n) . const . fromIntegral . ord
  ) (iterate succ n) str

execute (SUB r o) = value o >>= modifyReg r . flip (-)

execute (MUL r o) = value o >>= modifyReg r . (*)

execute (NOT r) = modifyReg r $ \x -> if x == 0 then 1 else 0

execute (COR rDoff rSoff rLen) = do
  [doff, soff, len] <- mapM value [rDoff, rSoff, rLen]
  st <- get
  let code' = code st
      code'' = fst $ flip genericIndex len $ iterate (
        \(cd, i) -> let
          di = i + doff
          si = i + soff
          cd' = insert di (code' ! si) cd
          in (cd', i + 1)
        ) (code', cp st)
  put $ st { code = code'' }

execute (BLA rDoff opr rLen) = do
  [doff, len] <- mapM value [rDoff, rLen]
  st <- get
  let code' = fst $ flip genericIndex len $ iterate (
        \(cd, i) -> (insert i opr cd, i + 1)
        ) (code st, cp st + doff)
  put $ st { code = code' }

execute NOP = return ()

execute STOP = liftIO $ exitWith ExitSuccess

-----------------------------------------------------------

value :: Operand -> Smith Integer
value (Immed x) = return x
value (Reg r) = value' r
  where
    lookupReg n = findWithDefault 0 n . regs . get
    value' (REG n) = lookupReg n
    value' (IREG r) = lookupReg =<< value' r
    value' TTY = fromIntegral . ord . (\xs -> if null xs then '\n' else head xs) . liftIO getLine
    value' PC = cp . get -- is this right ???
