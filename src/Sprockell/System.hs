{-# LANGUAGE RecordWildCards #-}
module Sprockell.System (module Sprockell.System, module X) where
import Sprockell.Components as X
import Sprockell.TypesEtc as X
import Sprockell.Sprockell as X
import System.IO
import Data.Bits

data SystemConfig = SysConf
        { bufferDelay :: Int  -- bufferDelay > 0, also impacts maximum number of outstanding read requests
        , dataMemSize :: Int  -- local data memory size
        , coreCount   :: Int  -- number of sprockells in the system
        , multiplier  :: Int  -- multiplier > 0, factor of 'clockspeed' between system and sprockells
        }

defaultConfig :: SystemConfig
defaultConfig = SysConf
        { bufferDelay = 4
        , dataMemSize = 128
        , coreCount = 4
        , multiplier = 2
        }

type SharedMem = Memory Value

data SystemState = SysState
        { instrs     :: !InstructionMem
        , sprs       :: ![SprockellState]
        , sReqFifos  :: ![Fifo Request]
        , repBuffers :: ![Buffer (Maybe Reply)]
        , sharedMem  :: !SharedMem
        , rngState   :: !RngState
        , cycleCount :: !Int
        }

-- ===========================================================================================
-- IO Devices
-- ===========================================================================================
stdio :: MemAddr
stdio = Addr stdioAddr
stdioAddr :: Address
stdioAddr = 0x1000000

type IODevice = SharedMem -> Request -> IO (SharedMem, Maybe Reply)

memDevice :: IODevice
memDevice mem (addr, ReadReq)        = return (mem, Just (mem !!! addr))
memDevice mem (addr, WriteReq value) = return (mem <~= (addr, value), Nothing)
memDevice mem (addr, TestReq)        = return (mem <~= (addr, 1),     Just test)
    where test = intBool $ not $ testBit (mem !!! addr) 0

stdDevice :: IODevice
stdDevice mem (_, WriteReq value) = putChar (chr value) >> return (mem, Nothing)
stdDevice _   (a, TestReq) = error ("TestAndSet not supported on address: " ++ show a)
stdDevice mem (_, ReadReq) = fmap ((,) mem . Just) $ do
    rdy <- hReady stdin
    if rdy
        then fmap ord getChar
        else return (-1)

-- ===========================================================================================
-- ===========================================================================================
withDevice :: Address -> IODevice
withDevice addr | addr < stdioAddr = memDevice
                | otherwise        = stdDevice

processRequest :: Maybe Request -> SharedMem -> IO (SharedMem, Maybe Reply)
processRequest Nothing    mem = return (mem, Nothing)
processRequest (Just out) mem = withDevice (fst out) mem out

updateElemWith :: (a -> (a,b)) -> Int -> [a] -> ([a],b)
updateElemWith f n xs = (take n xs ++ x' : drop (n + 1) xs, y)
   where (x', y) = f (xs !! n)

system :: SystemConfig -> SystemState -> IO SystemState
system SysConf{..} SysState{..} = do
        let (sprs', sprReqs)  = unzip $ zipWith (sprockell instrs) sprs $ map peek repBuffers  -- let all sprockells run a step

        let (rnd, rngState')  = nextRandom rngState
        let rsid              = rnd `mod` length sprs'

        let (sReqFifos', req) = if (cycleCount `mod` multiplier) == 0       -- once every multiplier cycles pick a request
                                then updateElemWith deQueue rsid sReqFifos  -- from a random fifo to be processed by system
                                else (sReqFifos, Nothing)

        (mem', reply)        <- processRequest req sharedMem
        let replies           = map (\i -> if i == rsid then reply else Nothing) [0..]

        let sReqFifos''       = zipWith (maybe id enQueue) sprReqs sReqFifos'
        let repBuffers'       = zipWith (<+) repBuffers replies

        length repBuffers' `seq` length sReqFifos'' `seq` return () -- workaround to prevents space leaks

        return (SysState instrs sprs' sReqFifos'' repBuffers' mem' rngState' (succ cycleCount))

-- ===========================================================================================
-- ===========================================================================================
-- "Simulates" sprockells by recursively calling them over and over again
simulate :: SystemConfig -> (SystemState -> IO()) -> SystemState -> IO SystemState
simulate sysConf debugFunc sysState@SysState{..}
    | all halted sprs && all isEmptyQueue sReqFifos  = return sysState
    | otherwise = do
        sysState' <- system sysConf sysState
        --putStr (debugFunc sysState')
        debugFunc sysState'
        simulate sysConf debugFunc sysState'

-- ===========================================================================================
-- ===========================================================================================
-- Initialise SystemState for N sprockells
initSystemState :: SystemConfig -> [Instruction] -> Seed -> SystemState
initSystemState SysConf{..} is seed = SysState
        { instrs     = initLookupTable "InstructionMemory" is
        , sprs       = map (initSprockell dataMemSize) $ take coreCount [0..]
        , sReqFifos  = replicate coreCount initFifo
        , repBuffers = replicate coreCount (initBuffer bufferDelay Nothing)
        , sharedMem  = initMemory
        , rngState   = initRng seed
        , cycleCount = 0
        }

run :: Int -> [Instruction] -> IO SystemState
run = runDebug (\_ -> return ())

runDebug :: (SystemState -> IO()) -> Int -> [Instruction] -> IO SystemState
runDebug debugFunc n instrs = do
    seed <- pickSeed
    runDebugWithSeed seed debugFunc n instrs

runWithSeed :: Seed -> Int -> [Instruction] -> IO SystemState
runWithSeed seed = runDebugWithSeed seed (\_ -> return ())

runDebugWithSeed :: Seed -> (SystemState -> IO()) -> Int -> [Instruction] -> IO SystemState
runDebugWithSeed seed debugFunc n instrs = do
    let sysConf = defaultConfig {coreCount = n}
    hPutStrLn stderr ("Starting with random seed: " ++ show seed)
    simulate sysConf debugFunc (initSystemState sysConf instrs seed)
