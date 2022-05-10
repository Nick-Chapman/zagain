
module Walk (walkZork) where

import Data.Bits ((.&.),shiftR)
import Data.Map (Map)
import Decode (fetchInstruction,fetchRoutineHeader,makeVariable)
import Dis (runFetch)
import Eff (Eff(..),Bin(..))
import Instruction (Instruction,RoutineHeader,Func(..),Args(..),Arg(..),Variable(..),Label(..),Dest(..))
import Numbers (Addr,addrOfPackedWord,Value,Byte,valueToByte,valueOfByte,valueOfWord,valueToAddr,valueToWord,valueOfInt)
import Story (Story,loadStory,readStoryByte)
import Text.Printf (printf)
import qualified Data.Char as Char
import qualified Data.Map as Map
import qualified Instruction as I

type Target = Variable --TODO: actually do the rename

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
runInter = loop []
  where
    loop :: [String] -> Inter -> IO ()
    loop buf = \case
      I_Trace _n a instruction next -> do
        printf "(Decode %d %s %s)\n" _n (show a) (I.pretty instruction)
        --printf "(Decode %s %s)\n" (show a) (I.pretty instruction)
        loop buf next
      I_Output text next -> do
        --let _ = putStrLn ("OUTPUT: " ++ s) --TODO: buffer and print
        loop (text:buf) next
      I_Debug s next -> do
        let _ = putStrLn ("DEBUG:" ++ s) --hide debug for now
        loop buf next
      I_Input f -> do
        let s :: String = undefined "get text from user"
        loop buf (f s)
      I_Stop -> do
        print "**Stop**(buffered output...)"
        mapM_ putStr (reverse buf)

--[interaction type]--------------------------------------------------

data Inter
  = I_Trace Int Addr Instruction Inter
  | I_Output String Inter
  | I_Debug String Inter
  | I_Input (String -> Inter) --TODO: make use of this!
  | I_Stop

--[interpreter for execution effect]----------------------------------

runEff :: State -> Eff () -> Inter
runEff s0 e0 = loop s0 e0 $ \_ () -> I_Stop
  where
    loop :: State -> Eff a -> (State -> a -> Inter) -> Inter
    loop s e k = case e of
      Ret x -> k s x
      Bind e f -> loop s e $ \s a -> loop s (f a) k
      GamePrint mes -> I_Output mes (k s ())
      Debug mes -> I_Debug mes (k s ())

      --ReadInputFromUser -> do I_Input $ \response -> k s response
      ReadInputFromUser -> I_Stop

      FetchI -> do
       --I_Output (show s) $ do
        let State{story,pc,count} = s
        let (ins',pc') = fetchI pc story
        let ins = if count > 225 then error "too many!" else ins'
        I_Trace count pc ins (k s { pc = pc', count = count + 1 } ins)

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
        let _debug = id --I_Debug (show ("SetLocal",n,v))
        _debug $ k s { locals = Map.insert n v locals } ()

      EqualAny vs -> do
        let
          res =
            case vs of
              [v1,v2] -> v1==v2
              [v1,v2,v3] -> v1==v2 || v1==v3
              vs -> error (show ("EqualAny",vs))
        let _debug = id --I_Debug (show ("EqualAny",vs,res))
        _debug $ k s res
      IsZero value -> do
        let res = (value == 0)
        let _debug = id --I_Debug (show ("IsZero",value,res))
        _debug $ k s res
      BinOp bin v1 v2 -> do
        let res = case bin of
              BAdd -> v1 + v2
              BSub -> v1 - v2
              BAnd -> v1 .&. v2
        let _debug = id --I_Debug (show ("BinOp",bin,v1,v2,res))
        _debug $ k s res

      GetByte a -> do
        let State{story,overrides} = s
        let (_over,b) =
              case Map.lookup a overrides of
                Just b -> (True,b)
                Nothing -> (False,readStoryByte story a)
        let _debug = id --I_Debug (show ("GetByte",a,_over,b))
        _debug $ k s b

      SetByte a b -> do
        let State{overrides} = s
        let _debug = id --I_Debug (show ("SetByte",a,b))
        _debug $ k s { overrides = Map.insert a b overrides } ()

      PushStack v -> do
        let State{stack} = s
        k s { stack = v : stack } ()

      PopStack -> do
        let State{stack} = s
        case stack of
          [] -> error "PopStack: []"
          v:stack -> do
            k s { stack } v

fetchI :: Addr -> Story -> (Instruction,Addr) --TODO: inline
fetchI a story =
  case (runFetch a story fetchInstruction) of
    Left e -> error (show ("fetchI",e))
    Right x -> x

fetchH :: Addr -> Story -> (RoutineHeader,Addr) --TODO: inline
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
  I.Bad s -> do error (show ("Bad",s))

  I.Add arg1 arg2 target -> do evalBin BAdd arg1 arg2 target
  I.And_ arg1 arg2 target -> do evalBin BAnd arg1 arg2 target

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

  I.Get_child arg target label -> do
    let _ = undefined arg target label --TODO
    Debug (show ("TODO:Get_child",arg,target,label))
    pure ()

  I.Get_parent arg target -> do
    let _ = undefined arg target --TODO
    let res = 999
    Debug (show ("TODO:Get_parent(HACK res)",arg,target,res))
    setTarget target res
    pure ()

  I.Get_prop arg1 arg2 target -> do
    let _ = undefined arg1 arg2 target --TODO
    let res = 19102
    Debug (show ("TODO:Get_prop(HACK fixed res)",arg1,arg2,target,res))
    setTarget target res
    pure ()

  I.Get_prop_addr arg1 arg2 target -> do
    let _ = undefined arg1 arg2 target --TODO
    Debug (show ("TODO:Get_prop_addr",arg1,arg2,target))
    pure ()

  I.Get_prop_len arg target -> do undefined arg target

  I.Get_sibling arg target label -> do
    let _ = undefined arg target label --TODO
    Debug (show ("TODO:Get_sibling",arg,target,label))
    pure ()

  I.Inc arg -> do undefined arg

  I.Inc_check arg1 arg2 label -> do
    v0 <- evalArg arg1
    let target :: Target = makeVariable (valueToByte v0)
    --let v1 = v0
    v1 <- evalTarget target
    v2 <- evalArg arg2
    --Debug (show ("inc-check",v1,v2,(v1 >= v2)))
    setTarget target (v1 + 1)
    branchMaybe label (v1 >= v2)

  I.Insert_obj arg1 arg2 -> do
    let _ = undefined arg1 arg2 --TODO
    Debug (show ("TODO:Insert_obj",arg1,arg2))
    pure ()

  I.Je (Args args) label -> do
    mapM evalArg args >>= EqualAny >>= branchMaybe label

  I.Jg arg1 arg2 label -> do
    v1 <- evalArg arg1
    v2 <- evalArg arg2
    branchMaybe label (v1 > v2)

  I.Jin arg1 arg2 label -> do
    let _ = undefined arg1 arg2 label --TODO
    Debug (show ("TODO: Jin",arg1,arg2,label))
    pure ()

  I.Jl arg1 arg2 label -> do undefined arg1 arg2 label

  I.Jump addr -> do SetPC addr
  I.Jz arg label -> do evalArg arg >>= IsZero >>= branchMaybe label

  I.Load_byte arg1 arg2 target -> do
    v1 <- evalArg arg1
    v2 <- evalArg arg2
    b <- GetByte (fromIntegral (v1 + 2*v2))
    setTarget target (valueOfByte b)

  I.Load_word arg1 arg2 target -> do
    v1 <- evalArg arg1
    v2 <- evalArg arg2
    w <- getWord (fromIntegral (v1 + 2*v2))
    setTarget target (valueOfWord w)

  I.Mul arg1 arg2 target -> do undefined arg1 arg2 target

  I.New_line -> do GamePrint "\n"
  I.Print string -> do GamePrint string

  I.Print_addr arg -> do undefined arg

  I.Print_char arg -> do
    v <- evalArg arg
    let c :: Char = Char.chr (fromIntegral v)
    -- todo -- check char in bound!
    GamePrint [c]

  I.Print_num arg -> do evalArg arg >>= GamePrint . show

  I.Print_obj arg -> do
    v <- evalArg arg
    shortName <- getObjShortName v
    GamePrint shortName

  I.Print_paddr arg -> do undefined arg
  I.Print_ret string -> do undefined string

  I.Pull arg -> do
    v0 <- evalArg arg
    let target :: Target = makeVariable (valueToByte v0)
    v1 <- PopStack
    --Debug (show ("pull",v0,target,v1))
    setTarget target v1


  I.Push arg -> do evalArg arg >>= PushStack

  I.Put_prop arg1 arg2 arg3 -> do
    let _ = undefined arg1 arg2 arg3 --TODO
    Debug (show ("TODO: Put_prop",arg1,arg2,arg3))
    pure ()

  I.Random arg target -> do undefined arg target
  I.Ret_popped -> do PopStack >>= returnValue
  I.Return arg -> do
    v <- evalArg arg
    target <- PopFrame
    setTarget target v

  I.Rfalse -> do returnValue 0
  I.Rtrue -> do returnValue 1

  I.Set_attr arg1 arg2 -> do
    let _ = undefined arg1 arg2 --TODO
    Debug (show ("TODO: Set_attr",arg1,arg2))
    pure ()

  I.Sread arg1 arg2 -> do
    v1 <- evalArg arg1
    v2 <- evalArg arg2
    typed <- ReadInputFromUser
    Debug (show ("Sread",(arg1,v1),(arg2,v2),"-->",typed))

  I.Store arg1 arg2 -> do
    v1 <- evalArg arg1
    let target :: Target = makeVariable (valueToByte v1)
    v2 <- evalArg arg2
    --Debug (show ("Store",(arg1,v1,target),(arg2,v2)))
    case target of
      Sp{} -> undefined (do _ <- PopStack; pure ()) -- from niz
      _ -> pure ()
    setTarget target v2
    pure ()

  I.Storeb arg1 arg2 arg3 -> do undefined arg1 arg2 arg3

  I.Storew arg1 arg2 arg3 -> do
    v1 <- evalArg arg1
    v2 <- evalArg arg2
    v3 <- evalArg arg3
    let w1 = valueToAddr v1
    let w2 = valueToAddr v2
    let w3 = valueToWord v3
    let a = w1 + 2*w2
    setWord a w3

  I.Sub arg1 arg2 target -> do evalBin BSub arg1 arg2 target

  I.Test arg1 arg2 label -> do
    v1 <- evalArg arg1
    v2 <- evalArg arg2
    let res = v1 .&. v2 == v2
    --Debug (show ("Test",(arg1,v1),(arg2,v2),label))
    branchMaybe label res

  I.Test_attr arg1 arg2 label -> do
    let _ = undefined arg1 arg2 label --TODO
    let res = False
    Debug (show ("TODO:Test_attr(hack res=FALSE)",arg1,arg2,label))
    branchMaybe label res

getObjShortName :: Value -> Eff String
getObjShortName v = do
  pure $ printf "object<%s>" (show v) --TODO


evalBin :: Bin -> Arg -> Arg -> Target -> Eff ()
evalBin bin arg1 arg2 target = do
  v1 <- evalArg arg1
  v2 <- evalArg arg2
  v <- BinOp bin v1 v2
  setTarget target v

branchMaybe :: Label -> Bool -> Eff ()
branchMaybe (Branch sense dest) b = case (sense,b) of
  (I.T,False) -> pure ()
  (I.F,True) -> pure ()
  (I.T,True) -> gotoDest dest
  (I.F,False) -> gotoDest dest

gotoDest :: Dest -> Eff ()
gotoDest = \case
  Dfalse -> returnValue 0
  Dtrue -> returnValue 1
  Dloc a -> SetPC a

evalFunc :: Func -> Eff Addr
evalFunc = \case
  Floc a -> pure a
  Fvar var -> do
    v <- evalTarget var
    let a = addrOfPackedWord (valueToWord v)
    --Debug(show("evalFunc(Fvar)",var,v,a))
    pure a

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

returnValue :: Value -> Eff ()
returnValue v = do
  target <- PopFrame
  setTarget target v


setTarget :: Target -> Value -> Eff ()
setTarget var v = do
  --Debug (show ("setTarget",var,v))
  setTarget' var v

setTarget' :: Target -> Value -> Eff ()
setTarget' var v = case var of
  Sp -> PushStack v
  Local n -> SetLocal n v
  Global b -> do
    a <- globalAddr b
    let w = valueToWord v
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
