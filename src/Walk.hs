
module Walk (walkZork) where

import Addr (Addr)
import Control.Monad (ap,liftM)
import Data.Bits -- (testBit,(.&.),shiftR)
import Data.Int (Int16)
import Data.Word (Word8)
import Decode (fetchInstruction,fetchRoutineHeader)
import Dis (runFetch)
import Instruction (Instruction,RoutineHeader,Func(..),Args(..),Arg(..),Variable(..),Label(..),Dest(..))
import Story (Story,loadStory,readStoryByte)
import Text.Printf (printf)
import qualified Instruction as I
import Data.Map (Map)
import qualified Data.Map as Map

type Byte = Word8
type Target = Variable -- TODO: actually do the rename


walkZork :: IO ()
walkZork = do
  print "*walkZork*"
  let filename = "story/zork1.88-840726.z3"
  story <- loadStory filename
  let e = theEffect
  let s :: State = initState story
  let i :: Inter = runEff s e
  runInter i

--[run interaction as IO]---------------------------------------------

runInter :: Inter -> IO ()
runInter = \case
  I_Trace n a instruction next -> do
    printf "(Decode %d %s %s)\n" n (show a) (I.pretty instruction)
    runInter next
  I_Output s next -> do
    putStrLn s
    runInter next
  I_Stop -> do
    print (show ("I_Stop"))

--[interaction type]--------------------------------------------------

data Inter
  = I_Trace Int Addr Instruction Inter
  | I_Output String Inter
  | I_Stop

--[interpreter for execution effect]----------------------------------

runEff :: State -> Eff () -> Inter
runEff s0 e0 = loop s0 e0 $ \_ () -> I_Stop
  where
    loop :: State -> Eff a -> (State -> a -> Inter) -> Inter
    loop s e k = case e of
      Ret x -> k s x
      Bind e f -> loop s e $ \s a -> loop s (f a) k
      Print mes -> I_Output mes (k s ())

      FetchI -> do
       --I_Output (show s) $ do
        let State{story,pc,count} = s
        let (i,pc') = fetchI pc story
        I_Trace count pc i (k s { pc = pc', count = count + 1 } i)

      FetchHeader{} -> do
        let State{story,pc} = s
        let (rh,pc') = fetchH pc story
        k s { pc = pc' } rh

      PushFrame addr target -> do
        let State{pc,stack,locals,frames} = s
        k s { pc = addr, frames = Frame { pc, target, stack, locals } : frames } ()

      PopFrame -> do
        let State{frames} = s
        case frames of
          [] -> error "PopFrame, frames=[]"
          Frame{pc,target,stack,locals}:frames -> do
            k s { pc, stack, locals, frames } target

      SetPC pc -> k s { pc } ()

      GetLocal n -> do
        let State{locals} = s
        let v = maybe (error (show ("GetLocal",n))) id $ Map.lookup n locals
        k s v

      SetLocal n v -> do
        let State{locals} = s
        --let _debug = I_Output (show ("SetLocal",n,v))
        k s { locals = Map.insert n v locals } ()

      AllEqual vs -> do
        let res = (case vs of [v1,v2] -> v1==v2; _ -> undefined)
        --let _debug = I_Output (show ("AllEqual",vs,res))
        k s res
      IsZero value -> do
        let res = (value == 0)
        --let _debug = I_Output (show ("IsZero",value,res))
        k s res
      BinOp bin v1 v2 -> do
        let res = case bin of BAdd -> v1+v2; BSub -> v1-v2
        --let _debug = I_Output (show ("BinOp",bin,v1,v2,res))
        k s res

      GetByte a -> do
        let State{story,overrides} = s
        case Map.lookup a overrides of
          Just b -> k s b
          Nothing -> k s (readStoryByte story a)

      SetByte a b -> do
        let State{overrides} = s
        k s { overrides = Map.insert a b overrides } ()

      PushStack v -> do
        let State{stack} = s
        k s { stack = v : stack } ()

      PopStack -> do
        let State{stack} = s
        case stack of
          [] -> error "PopStack: []"
          v:stack -> do
            k s { stack } v

fetchI :: Addr -> Story -> (Instruction,Addr) -- TODO: inline
fetchI a story =
  case (runFetch a story fetchInstruction) of
    Left e -> error (show ("fetchI",e))
    Right x -> x

fetchH :: Addr -> Story -> (RoutineHeader,Addr) -- TODO: inline
fetchH a story =
  case (runFetch a story fetchRoutineHeader) of
    Left e -> error (show ("fetchH",e))
    Right x -> x


--[interpreter state]-------------------------------------------------

data State = State
  { story :: Story
  , pc :: Addr
  , count :: Int
  , stack :: [Value]
  , locals :: Map Byte Value
  , frames :: [Frame]
  , overrides :: Map Addr Byte
  }

instance Show State where
  show State{pc,stack,locals,frames} = show (pc,stack,locals,frames)

initState :: Story -> State
initState story = do
  let pc :: Addr = fromIntegral (readStoryWord story 0x6)
  State { story
        , pc
        , count = 0
        , stack = []
        , locals = Map.empty
        , frames = []
        , overrides = Map.empty
        }

readStoryWord :: Story -> Addr -> Word
readStoryWord story a = do
  let hi = readStoryByte story a
  let lo = readStoryByte story (a+1)
  256 * fromIntegral hi + fromIntegral lo

data Frame = Frame
  { pc :: Addr
  , target :: Target
  , stack :: [Value]
  , locals :: Map Byte Value
  }
  deriving Show

--[evaluation]--------------------------------------------------------

theEffect :: Eff ()
theEffect = loop
  where
    loop = do
      i <- FetchI
      eval i
      loop

eval :: Instruction -> Eff ()
eval = \case

  I.Add arg1 arg2 target -> do evalBin BAdd arg1 arg2 target

  I.And_ arg1 arg2 target -> do undefined arg1 arg2 target

  I.Call func (Args args) target -> do
    funcAddress <- evalFunc func
    PushFrame funcAddress target
    rh <- FetchHeader
    actuals <- mapM evalArg args
    setLocals rh actuals

  I.Clear_attr arg1 arg2 -> do undefined arg1 arg2
  I.Dec arg -> do undefined arg
  I.Dec_check arg1 arg2 label -> do undefined arg1 arg2 label
  I.Div arg1 arg2 target -> do undefined arg1 arg2 target
  I.Get_child arg target label -> do undefined arg target label
  I.Get_parent arg target -> do undefined arg target
  I.Get_prop arg1 arg2 target -> do undefined arg1 arg2 target
  I.Get_prop_addr arg1 arg2 target -> do undefined arg1 arg2 target
  I.Get_prop_len arg target -> do undefined arg target
  I.Get_sibling arg target label -> do undefined arg target label
  I.Inc arg -> do undefined arg
  I.Inc_check arg1 arg2 label -> do undefined arg1 arg2 label
  I.Insert_obj arg1 arg2 -> do undefined arg1 arg2
  I.Je (Args args) label -> do
    mapM evalArg args >>= AllEqual >>= evalLabel label

  I.Jg arg1 arg2 label -> do undefined arg1 arg2 label
  I.Jin arg1 arg2 label -> do undefined arg1 arg2 label
  I.Jl arg1 arg2 label -> do undefined arg1 arg2 label
  I.Jump addr -> do undefined addr
  I.Jz arg label -> do
    evalArg arg >>= IsZero >>= evalLabel label

  I.Load_byte arg1 arg2 target -> do undefined arg1 arg2 target
  I.Load_word arg1 arg2 target -> do
    v1 <- evalArg arg1
    v2 <- evalArg arg2
    w <- getWord (fromIntegral (v1 + 2*v2))
    setTarget target (valueOfWord w)

  I.Mul arg1 arg2 target -> do undefined arg1 arg2 target
  I.New_line -> do undefined
  I.Print string -> do undefined string
  I.Print_addr arg -> do undefined arg
  I.Print_char arg -> do undefined arg
  I.Print_num arg -> do undefined arg
  I.Print_obj arg -> do undefined arg
  I.Print_paddr arg -> do undefined arg
  I.Print_ret string -> do undefined string
  I.Pull arg -> do undefined arg
  I.Push arg -> do undefined arg
  I.Put_prop arg1 arg2 arg -> do undefined arg1 arg2 arg
  I.Random arg target -> do undefined arg target
  I.Ret_popped -> do undefined
  I.Return arg -> do
    v <- evalArg arg
    target <- PopFrame
    setTarget target v

  I.Rfalse -> do undefined
  I.Rtrue -> do undefined
  I.Set_attr arg1 arg2 -> do undefined arg1 arg2
  I.Sread arg1 arg2 -> do undefined arg1 arg2
  I.Store arg1 arg2 -> do undefined arg1 arg2
  I.Storeb arg1 arg2 arg -> do undefined arg1 arg2 arg

  I.Storew arg1 arg2 arg3 -> do
    v1 <- evalArg arg1
    v2 <- evalArg arg2
    v3 <- evalArg arg3
    let w1 = valueToAddr v1
    let w2 = valueToAddr v2
    let w3 = valueToUnsigned v3
    let a = w1 + 2*w2
    setWord a w3

  I.Sub arg1 arg2 target -> do evalBin BSub arg1 arg2 target

  I.Test arg1 arg2 label -> do undefined arg1 arg2 label
  I.Test_attr arg1 arg2 label -> do undefined arg1 arg2 label

  _ -> do error "eval"


evalBin :: Bin -> Arg -> Arg -> Target -> Eff ()
evalBin bin arg1 arg2 target = do
  v1 <- evalArg arg1
  v2 <- evalArg arg2
  v <- BinOp bin v1 v2
  setTarget target v

evalLabel :: Label -> Bool -> Eff ()
evalLabel (Branch sense dest) b = case (sense,b) of
  (I.T,False) -> pure ()
  (I.F,True) -> pure ()
  (I.T,True) -> gotoDest dest
  (I.F,False) -> gotoDest dest

gotoDest :: Dest -> Eff ()
gotoDest = \case
  Dfalse -> undefined
  Dtrue -> undefined
  Dloc a -> SetPC a

evalFunc :: Func -> Eff Addr
evalFunc = \case
  Floc a -> pure a
  Fvar{} -> undefined

evalArg :: Arg -> Eff Value
evalArg = \case
  Con x -> pure $ valueOfInt x
  Var v -> evalTarget v

evalTarget :: Target -> Eff Value
evalTarget = \case
  Sp -> PopStack
  Local n -> GetLocal n
  Global b -> do
    a <- globalAddr b
    w <- getWord a
    let v = valueOfWord w
    --Print (show ("valueOfWord:",w,v))
    pure v

setTarget :: Target -> Value -> Eff ()
setTarget var v = case var of
  Sp -> PushStack v
  Local n -> SetLocal n v
  Global b -> do
    a <- globalAddr b
    let w = valueToUnsigned v
    setWord a w

globalAddr :: Byte -> Eff Addr
globalAddr b = do
  globalBase :: Word <- getWord 0xC
  pure $ fromIntegral (globalBase + 2 * fromIntegral b)

setLocals :: RoutineHeader -> [Value] -> Eff ()
setLocals (I.RoutineHeader defs) actuals = do
  sequence_ [ SetLocal n (fromIntegral v) | (n,v) <- zip [1..] defs ]
  sequence_ [ SetLocal n v | (n,v) <- zip [1..] actuals ]

getWord :: Addr -> Eff Word
getWord a = do
  hi <- GetByte a
  lo <- GetByte (a+1)
  pure (256 * fromIntegral hi + fromIntegral lo)

setWord :: Addr -> Word -> Eff ()
setWord a w = do
  let hi = fromIntegral (w `shiftR` 8) --TODO: should be an effect?
  let lo = fromIntegral (w .&. 0xff)
  SetByte a hi
  SetByte (a+1) lo

--[exection effect]---------------------------------------------------

instance Functor Eff where fmap = liftM
instance Applicative Eff where pure = return; (<*>) = ap
instance Monad Eff where return = Ret; (>>=) = Bind

data Eff a where
  Ret :: a -> Eff a
  Bind :: Eff a -> (a -> Eff b) -> Eff b
  Print :: String -> Eff ()

  FetchI :: Eff Instruction
  FetchHeader :: Eff RoutineHeader

  PushFrame :: Addr -> Target -> Eff ()
  PopFrame :: Eff Target
  SetPC :: Addr -> Eff ()

  GetLocal :: Byte -> Eff Value
  SetLocal :: Byte -> Value -> Eff ()

  AllEqual :: [Value] -> Eff Bool
  IsZero :: Value -> Eff Bool
  BinOp :: Bin -> Value -> Value -> Eff Value

  GetByte :: Addr -> Eff Byte
  SetByte :: Addr -> Byte -> Eff ()

  PushStack :: Value -> Eff ()
  PopStack :: Eff Value

--[z-value]-----------------------------------------------------------

type Value = Int16

data Bin = BAdd | BSub
  deriving Show

valueToAddr :: Value -> Addr
valueToAddr v = fromIntegral (valueToUnsigned v)

valueToUnsigned :: Value -> Word
valueToUnsigned v =
  if v < 0 then fromIntegral v else -- error (show ("valueToUnsigned",v)) else
    fromIntegral v

valueOfWord :: Word -> Value
valueOfWord w =
  --if w `testBit` 15 then undefined else
    fromIntegral w

valueOfInt :: Int -> Value
valueOfInt = fromIntegral
