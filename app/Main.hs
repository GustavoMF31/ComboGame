{-# OPTIONS_GHC -Werror=incomplete-patterns #-}
{-# OPTIONS_GHC -Werror=missing-fields #-}

{-# LANGUAGE OverloadedStrings, TupleSections #-}

import Control.Monad (when, (<=<), guard, join)
import Data.Foldable (traverse_)
import Data.Maybe (mapMaybe, listToMaybe, isNothing, isJust)
import Data.Bool (bool)
import Data.List.NonEmpty (NonEmpty(..), nonEmpty)
import qualified Data.List.NonEmpty as NonEmpty
import Data.Functor (($>))

import Foreign.C.Types (CInt, CDouble(CDouble))
import Data.Word (Word32, Word8)
import Numeric.Natural (Natural)

import Nat (Nat, nat, natMinus, natAsDouble, natAdd, monus)

import Data.Default.Class (def)
import SDL hiding (angle)
import SDL.Image (loadTexture)
import qualified SDL.Mixer as Mix

getScancode :: KeyboardEventData -> Scancode
getScancode = keysymScancode . keyboardEventKeysym

getKeycode :: KeyboardEventData -> Keycode
getKeycode = keysymKeycode . keyboardEventKeysym

getKeyboardEvent :: EventPayload -> Maybe KeyboardEventData
getKeyboardEvent (KeyboardEvent event) = Just event
getKeyboardEvent _ = Nothing 

shouldQuit :: [Event] -> Bool
shouldQuit = any $ (QuitEvent ==) . eventPayload

type Combo = NonEmpty ComboKey
data Enemy = Rat | BlueRat | GreenRat | Spider | PurpleSpider | RedSpider | Clone | GrassTileEnemy
    deriving (Show)
data EnemyEncounter = MkEnemyEncounter { encounterEnemy :: Enemy, hitsTakenBeforeShowingHint :: Nat }
    deriving (Show)

data TextureData = MkTextureData
    { fighterIdle :: Texture
    , grassTile :: Texture
    , keyboardKeys :: Texture
    , ratSprite :: Texture
    , blueRatSprite :: Texture
    , spiderSprite :: Texture
    , sadFighter :: Texture
    , attackPose :: Texture
    , attackKick :: Texture -- high kick
    , lowKick :: Texture
    , hitEffect :: Texture
    , headbutt :: Texture
    , leftPunch :: Texture
    , crouchPunch :: Texture
    , kneeStrike :: Texture
    , jumpKick :: Texture
    , evilClone :: Texture
    , greenRat :: Texture
    , purpleSpider :: Texture
    , redSpider :: Texture
    }

data AudioData = MkAudioData
    { hit0 :: Mix.Chunk
    , hit1 :: Mix.Chunk
    , hit2 :: Mix.Chunk
    , hit3 :: Mix.Chunk
    , hit4 :: Mix.Chunk
    , hit5 :: Mix.Chunk
    , hit6 :: Mix.Chunk
    , hit7 :: Mix.Chunk
    , hit8 :: Mix.Chunk
    }

type Sound = AudioData -> Mix.Chunk

loadAudioData :: IO AudioData
loadAudioData = MkAudioData
    <$> Mix.load (mp3 "hit0")
    <*> Mix.load (mp3 "hit1")
    <*> Mix.load (mp3 "hit2")
    <*> Mix.load (mp3 "hit3")
    <*> Mix.load (mp3 "hit4")
    <*> Mix.load (mp3 "hit5")
    <*> Mix.load (mp3 "hit6")
    <*> Mix.load (mp3 "hit7")
    <*> Mix.load (mp3 "hit8")
  where
    mp3 :: String -> String
    mp3 x = "assets/" ++ x ++ ".mp3"
 
freeAudioData :: AudioData -> IO ()
freeAudioData (MkAudioData a b c d e f g h i) = do
    Mix.free a
    Mix.free b
    Mix.free c
    Mix.free d
    Mix.free e
    Mix.free f
    Mix.free g
    Mix.free h
    Mix.free i

data ComboKey
    = KeyJ
    | KeyK
    | KeyL
    | KeyU
    | KeyI
    | KeyO
    | KeyM
    | KeyComma
    | KeyPeriod
  deriving (Show, Eq)

comboKeyAttackPose :: ComboKey -> TextureData -> Texture
comboKeyAttackPose KeyK = attackKick -- high kick
comboKeyAttackPose KeyM = lowKick
comboKeyAttackPose KeyL = headbutt
comboKeyAttackPose KeyJ = attackPose -- Right hand punch
comboKeyAttackPose KeyO = leftPunch -- Left hand punch
comboKeyAttackPose KeyI = kneeStrike
comboKeyAttackPose KeyU = jumpKick
comboKeyAttackPose KeyComma = crouchPunch
-- comboKeyAttackPose KeyPeriod = leftPunch
comboKeyAttackPose _ = attackPose

attackSoundForKey :: ComboKey -> Sound
attackSoundForKey KeyJ = hit0
attackSoundForKey KeyK = hit1
attackSoundForKey KeyL = hit2
attackSoundForKey KeyO = hit3
attackSoundForKey KeyM = hit4
attackSoundForKey KeyU = hit5
attackSoundForKey KeyI = hit6
attackSoundForKey KeyComma = hit7
attackSoundForKey KeyPeriod = hit8

data FlyingEnemyEffect = MkFlyingEnemyEffect
    { effectEnemy :: Enemy
    , angle :: Double
    , angularVelocity :: Double
    , timeUntilDisappearing :: Nat
    } deriving (Show)

updateEffect :: Nat -> FlyingEnemyEffect -> Maybe FlyingEnemyEffect
updateEffect delta effect = fmap (\newTimeUntilDisappearing ->
    effect { timeUntilDisappearing = newTimeUntilDisappearing })
    $ natMinus (timeUntilDisappearing effect) delta

hitEffectDuration :: Nat
hitEffectDuration = nat 200

type Level = NonEmpty EnemyEncounter

data LevelZipper = MkLevelZipper [Level] Level [Level]
    deriving (Show)

currentLevel :: LevelZipper -> Level
currentLevel (MkLevelZipper _ current _) = current

data Scene = TitleScreen | InGame LevelZipper GameState | LevelWonScreen LevelZipper | GameWonScreen
    deriving (Show)

data GameState = MkGameState
    { comboKeysLeft :: Combo
    , currentEncounter :: EnemyEncounter
    , nextEncounters :: [EnemyEncounter]
    , playerHealth :: Nat
    -- Time until the next enemy attack
    , enemyAttackTimer :: Nat
    -- (Nothing means it's in Idle instead)
    -- (Just holds how long until the effect ends)
    , sadnessEffect :: Maybe Nat
    , timeSinceStart :: Nat -- In miliseconds
    , timeUntilComboTimeout :: Maybe Nat
    , flyingEnemyEffect :: Maybe FlyingEnemyEffect
    , currentAttack :: Maybe ComboKey
    , hitsTakenDueToCurrentEnemy :: Nat
    , hitEffectInProgress :: Maybe Nat
    , enemyWalkInTimer :: Maybe Nat -- Nothing means the enemy is not walking in
    , levelRestartEffect :: Maybe Nat -- Counts down to the effect's disappearance
    } deriving (Show)

enemyTexture :: Enemy -> TextureData -> Texture
enemyTexture Rat = ratSprite
enemyTexture BlueRat = blueRatSprite
enemyTexture GreenRat = greenRat
enemyTexture Spider = spiderSprite
enemyTexture PurpleSpider = purpleSpider
enemyTexture RedSpider = redSpider
enemyTexture GrassTileEnemy = grassTile
enemyTexture Clone = evilClone

enemySize :: Enemy -> V2 CInt
enemySize Rat = ratSize
enemySize BlueRat = ratSize
enemySize GreenRat = ratSize
enemySize Spider = 12 * V2 25 10
enemySize PurpleSpider = 12 * V2 25 10
enemySize RedSpider = 12 * V2 25 10
enemySize GrassTileEnemy = V2 200 200
enemySize Clone = 8 * V2 64 64

-- Can be used to adjust the position to play well with the sprite and size defined above
enemyY :: Enemy -> CInt
enemyY Rat = ratY
enemyY BlueRat = ratY
enemyY GreenRat = ratY
enemyY Spider = 530
enemyY PurpleSpider = 530
enemyY RedSpider = 530
enemyY GrassTileEnemy = 500
enemyY Clone = 200

ratSize :: V2 CInt
ratSize = 12 * V2 34 12

ratY :: CInt
ratY = 530

forward, blueRatCombo :: Combo

forward = KeyL :| [KeyK, KeyJ]
blueRatCombo = KeyO :| [KeyK, KeyM]

-- TODO: An enemy that requires OJOJ combo
comboToDefeat :: Enemy -> Combo
comboToDefeat Rat = KeyJ :| []
-- comboToDefeat Rat = KeyO :| concat (replicate 4 [KeyJ, KeyO])
-- comboToDefeat Rat = KeyU :| [KeyI, KeyO, KeyJ, KeyK, KeyL, KeyM, KeyComma, KeyPeriod]
comboToDefeat BlueRat = KeyO :| [KeyK, KeyM]
comboToDefeat GreenRat = KeyU :| [KeyK, KeyPeriod]
comboToDefeat Spider = blueRatCombo <> blueRatCombo <> forward -- KeyPeriod :| [KeyK, KeyU]
comboToDefeat PurpleSpider = KeyK :| [KeyM, KeyU]
comboToDefeat RedSpider = KeyL :| [KeyL, KeyJ]
comboToDefeat GrassTileEnemy = KeyK :| [KeyL, KeyJ]
comboToDefeat Clone = KeyK :| [KeyK, KeyJ]

maxHealth :: Nat
maxHealth = nat 10

timeUntilSadnessEnd :: Nat
timeUntilSadnessEnd = nat 300

-- In ticks (miliseconds)
timeBetweenEnemyAttacks :: Nat
timeBetweenEnemyAttacks = nat 2000

maxTimeBetweenComboKeys :: Nat
maxTimeBetweenComboKeys = nat 500

mkLevel :: Functor f => f (Enemy, Natural) -> f EnemyEncounter
mkLevel = fmap (uncurry $ flip $ flip MkEnemyEncounter . nat)

allLevels :: LevelZipper
allLevels = MkLevelZipper [] testLevel [firstLevel]

firstLevel :: NonEmpty EnemyEncounter
firstLevel = mkLevel $
      (Rat, 0) :|
    [ (Rat, 1)
    , (Rat, 2)
    , (BlueRat, 0)
    , (BlueRat, 2)
    , (Rat, 3)
    , (BlueRat, 3)
    , (Rat, 3)
    , (Rat, 3)
    , (BlueRat, 3)
    , (BlueRat, 3)
    ]

initialScene :: Scene
initialScene = TitleScreen

testLevel :: Level
testLevel = mkLevel $
      (Clone, 0) :|
    [ (RedSpider, 0)
    , (GreenRat, 0)
    , (PurpleSpider, 0)
    , (Rat, 0)
    , (Spider, 0)
    -- , GrassTileEnemy
    ]

loadLevel :: Level -> GameState
loadLevel level = MkGameState
    { comboKeysLeft = comboToDefeat initialEnemy
    , currentEncounter = initialEncounter
    , nextEncounters = NonEmpty.tail level
    , playerHealth = maxHealth
    , enemyAttackTimer = timeBetweenEnemyAttacks
    , timeSinceStart = nat 0
    , timeUntilComboTimeout = Nothing
    , sadnessEffect = Nothing
    , flyingEnemyEffect = Nothing
    , currentAttack = Nothing
    , hitsTakenDueToCurrentEnemy = nat 0
    , hitEffectInProgress = Nothing
    , enemyWalkInTimer = Just enemyWalkInDuration
    , levelRestartEffect = Nothing
    }
  where
    initialEnemy = encounterEnemy initialEncounter
    initialEncounter = NonEmpty.head $ level

ensure :: (a -> Bool) -> a -> Maybe a
ensure p a = guard (p a) $> a

isPressed :: KeyboardEventData -> Bool
isPressed = (== Pressed) . keyboardEventKeyMotion

isReleased :: KeyboardEventData -> Bool
isReleased = (== Released) . keyboardEventKeyMotion

asComboKey :: Scancode -> Maybe ComboKey
asComboKey ScancodeK = Just KeyK
asComboKey ScancodeJ = Just KeyJ
asComboKey ScancodeL = Just KeyL
asComboKey ScancodeU = Just KeyU
asComboKey ScancodeI = Just KeyI
asComboKey ScancodeO = Just KeyO
asComboKey ScancodeM = Just KeyM
asComboKey ScancodeComma = Just KeyComma
asComboKey ScancodePeriod = Just KeyPeriod
asComboKey _ = Nothing

comboKeyAsScancode :: ComboKey -> Scancode
comboKeyAsScancode KeyK = ScancodeK
comboKeyAsScancode KeyJ = ScancodeJ
comboKeyAsScancode KeyL = ScancodeL
comboKeyAsScancode KeyU = ScancodeU
comboKeyAsScancode KeyI = ScancodeI
comboKeyAsScancode KeyO = ScancodeO
comboKeyAsScancode KeyM = ScancodeM
comboKeyAsScancode KeyComma = ScancodeComma
comboKeyAsScancode KeyPeriod = ScancodePeriod

isEnterPressed :: [Event] -> Bool
isEnterPressed = not . null . (mapMaybe $ ensure (== KeycodeReturn) <=< fmap getKeycode . ensure isPressed <=< getKeyboardEvent . eventPayload)

getComboInputs :: [Event] -> [ComboKey]
getComboInputs = mapMaybe $ asComboKey <=< fmap getScancode . ensure isPressed <=< getKeyboardEvent . eventPayload

wasReleased :: ComboKey -> [Event] -> Bool
wasReleased key = not . null . mapMaybe
    (ensure (== comboKeyAsScancode key) <=< fmap getScancode . ensure isReleased <=< getKeyboardEvent . eventPayload)

-- Process one key press from the player
processComboKey :: Combo -> ComboKey -> Maybe [ComboKey]
processComboKey keysLeft key = if key == NonEmpty.head keysLeft
    then Just $ NonEmpty.tail keysLeft 
    else Nothing

flyingEnemyEffectTime :: Nat
flyingEnemyEffectTime = nat 500

enemyWalkInDuration :: Nat
enemyWalkInDuration = nat 300

nextLevel :: LevelZipper -> Maybe LevelZipper
nextLevel (MkLevelZipper past current (next : nexts)) = Just $ MkLevelZipper (current : past) next nexts
nextLevel _ = Nothing

updateScene :: [Event] -> Nat -> Scene -> (Scene, [Sound])
updateScene events delta scene = case scene of
    InGame gameState levelZipper -> update events delta gameState levelZipper
    TitleScreen -> if isEnterPressed events
        then (InGame allLevels (loadLevel $ currentLevel allLevels), [])
        else (TitleScreen, [])
    LevelWonScreen levelZipper -> (, []) $ if isEnterPressed events
        then InGame levelZipper (loadLevel $ currentLevel levelZipper)
        else LevelWonScreen levelZipper
    GameWonScreen -> (, []) $ if isEnterPressed events
        then TitleScreen
        else GameWonScreen

update :: [Event] -> Nat -> LevelZipper -> GameState -> (Scene, [Sound])
update events delta levelZipper gameState = maybe ((, []) $ maybe GameWonScreen LevelWonScreen $ nextLevel levelZipper) (\(keysOfTheComboLeft, currentKeySucceeded, newEnemies) ->
    let killedEnemy :: Bool
        killedEnemy = isJust newEnemies
    in (InGame levelZipper $ flip (maybe ((loadLevel $ currentLevel levelZipper) { levelRestartEffect = Just levelRestartEffectDuration })) newPlayerHealth (\playerAliveHealth -> MkGameState
    { comboKeysLeft = if comboTimeOut then comboToDefeat (currentEnemy gameState) else keysOfTheComboLeft
    , nextEncounters = maybe (nextEncounters gameState) snd newEnemies
    , timeSinceStart = natAdd delta $ timeSinceStart gameState
    , currentEncounter = maybe (currentEncounter gameState) fst newEnemies
    , playerHealth = playerAliveHealth
    -- Reset the enemy attack timer whenever the enemy attacks or dies
    , enemyAttackTimer = case updatedEnemyTimer of
                             Nothing -> timeBetweenEnemyAttacks
                             Just t -> if isJust newEnemies
                                          then timeBetweenEnemyAttacks -- If the player killed an enemy, the timer resets
                                          else if isJust (timeUntilComboTimeout gameState) -- If the player is attacking
                                                  then enemyAttackTimer gameState -- freeze the enemyAttackTimer
                                                  else t -- otherwise keep it going
                            -- fromMaybe timeBetweenEnemyAttacks (guard (isNothing newEnemies && isNothing (timeUntilComboTimeout gameState)) *> updatedEnemyTimer)
    , timeUntilComboTimeout = if killedEnemy then Nothing else
                                if currentKeySucceeded
                                    then Just maxTimeBetweenComboKeys
                                    else newTimeUntilComboTimeout
    , sadnessEffect = if comboTimeOut || (not currentKeySucceeded && not (isNothing keyPressed))
                          then Just timeUntilSadnessEnd
                          else updateTimer $ sadnessEffect gameState
    , flyingEnemyEffect = if killedEnemy -- If the enemy was killed
                              then Just $ MkFlyingEnemyEffect -- Send it flying
                                  { effectEnemy = currentEnemy gameState
                                  , angle = 135 -- _angle
                                  , angularVelocity = 1 -- _velocity
                                  , timeUntilDisappearing = flyingEnemyEffectTime
                                  }  
                              else flyingEnemyEffect gameState >>= updateEffect delta
    , currentAttack = case keyPressed of
        Nothing -> resetCurrentAttackIfNecessary
        Just key -> if currentKeySucceeded then Just key else resetCurrentAttackIfNecessary
    , hitsTakenDueToCurrentEnemy = if killedEnemy
                                    then nat 0
                                    else natAdd (hitsTakenDueToCurrentEnemy gameState) (nat $ if enemyAttacked then 1 else 0)
    , hitEffectInProgress = if currentKeySucceeded then Just hitEffectDuration else updateTimer $ hitEffectInProgress gameState
    , enemyWalkInTimer = if isJust newEnemies then Just enemyWalkInDuration else updateTimer $ enemyWalkInTimer gameState
    , levelRestartEffect = updateTimer $ levelRestartEffect gameState
    }),
        maybe [] (\key -> if currentKeySucceeded then [attackSoundForKey key] else []) keyPressed
     ++ maybe [hit0] (const []) updatedEnemyTimer
    )) keyPressResult
        
  where
    keysPressed :: [ComboKey]
    keysPressed = getComboInputs events

    newPlayerHealth :: Maybe Nat
    newPlayerHealth = natMinus (playerHealth gameState) $ bool (nat 0) (nat 1) enemyAttacked

    keyPressed :: Maybe ComboKey
    keyPressed = listToMaybe keysPressed

    -- Retuns:
    --   the new comboKeysLeft
    --   A bool indicating if the key press succeeded (If there wasn't one, it's False)
    --   possibly a pair of new currentEnemy and new nextEnemies
    keyPressResult :: Maybe (Combo, Bool, Maybe (EnemyEncounter, [EnemyEncounter]))
    keyPressResult = case keyPressed of
        Nothing  -> Just (comboKeysLeft gameState, False, Nothing)
        Just key -> case processComboKey (comboKeysLeft gameState) key of
            Nothing -> Just (comboToDefeat $ currentEnemy gameState, False, Nothing)
            Just newComboKeysLeft -> case nonEmpty newComboKeysLeft of
                -- If there are no combo keys left, the player has beaten the enemy
                Nothing -> case nextEncounters gameState of
                    [] -> Nothing -- No new enemies left! Let's return Nothing to signal the level won screen should start
                    (x:xs) -> Just (comboToDefeat $ encounterEnemy x, True, Just (x, xs))
                Just xs -> Just (xs, True, Nothing)

    resetCurrentAttackIfNecessary :: Maybe ComboKey
    resetCurrentAttackIfNecessary = wasReleased <$> currentAttack gameState <*> pure events
        >>= bool (currentAttack gameState) Nothing

    -- Nothing means that the enemy attacked
    -- Just holds the new time until the enemy attack
    updatedEnemyTimer :: Maybe Nat
    updatedEnemyTimer = natMinus (enemyAttackTimer gameState) delta

    enemyAttacked :: Bool
    enemyAttacked = isNothing updatedEnemyTimer

    updateTimer :: Maybe Nat -> Maybe Nat
    updateTimer timer = join $ natMinus <$> timer <*> pure delta

    newTimeUntilComboTimeout :: Maybe Nat
    newTimeUntilComboTimeout = updateTimer (timeUntilComboTimeout gameState)

    comboTimeOut :: Bool
    comboTimeOut = isJust (timeUntilComboTimeout gameState) && isNothing newTimeUntilComboTimeout

-- Gets the combo keys that were already pressed in order to reach a certain GameState
alreadyPressed :: GameState -> [ComboKey]
alreadyPressed g = reverse $ drop (length $ comboKeysLeft g) $ reverse $ NonEmpty.toList $ comboToDefeat (currentEnemy g)

gameLoop :: Renderer -> TextureData -> AudioData -> V2 CInt -> Word32 -> Scene -> IO ()
gameLoop renderer textures audioData windowDimensions ticksSinceLastUpdate scene = do
    let loop = gameLoop renderer textures audioData windowDimensions

    events <- pollEvents

    renderScene textures renderer windowDimensions scene
    present renderer

    now <- ticks
    let deltaTime = now - ticksSinceLastUpdate
        (newScene, soundEffects) = updateScene events (nat $ fromIntegral deltaTime) scene

    if not $ shouldQuit events
      -- This use of fromIntegral can crash if deltaTime is negative
      then playAll soundEffects >> loop now newScene
      else pure ()
  where
    playAll :: [Sound] -> IO ()
    playAll = traverse_ (Mix.play . ($ audioData))

grassTileSize :: CInt
grassTileSize = 256

square :: CInt -> Point V2 CInt -> Rectangle CInt
square x = flip Rectangle (V2 x x)

spriteRect :: Int -> Rectangle CInt
spriteRect frame = Rectangle (P $ V2 (64 * fromIntegral (mod frame 4)) 0) (V2 64 64)

playerSpriteSize :: V2 CInt
playerSpriteSize = let size = 10 * 64 in V2 size size

followedBy :: (Double -> Double) -> (Double -> Double) -> Double -> Double
followedBy f g x
    | x > 0.5 = (f 1 + g (2 * (x - 0.5))) / 2
    | otherwise = f (2 * x) / 2

easeQuadratic :: Double -> Double
easeQuadratic x = x * x

easeInOutSine :: Double -> Double
easeInOutSine x = -(cos (pi * x) - 1) / 2

windupThenAttackEase :: Double -> Double
-- windupThenAttackEase x = (if x > 0.5 then 0.5 else 1) * negate (sin (2 * pi * x))
windupThenAttackEase = ((/ 2) . negate . easeInOutSine) `followedBy` ((* (5 / 2)) . easeQuadratic)
-- windupThenAttackEase = easeInOutSine

getKeyIndex :: ComboKey -> CInt
getKeyIndex KeyU = 3
getKeyIndex KeyI = 4
getKeyIndex KeyO = 5
getKeyIndex KeyJ = 1
getKeyIndex KeyK = 0
getKeyIndex KeyL = 2
getKeyIndex KeyM = 6
getKeyIndex KeyComma = 7
getKeyIndex KeyPeriod = 8

getKeyRect :: Bool -> ComboKey -> Rectangle CInt
getKeyRect pressed = (square 16 . P . flip V2 y . (*16)) . getKeyIndex
  where
    y = if pressed then 16 else 0

moveRectangle :: Point V2 CInt -> Rectangle CInt -> Rectangle CInt
moveRectangle p (Rectangle p' size) = Rectangle (p' + p) size

currentEnemy :: GameState -> Enemy
currentEnemy = encounterEnemy . currentEncounter

shouldDisplayHint :: GameState -> Bool
shouldDisplayHint gameState = hitsTakenBeforeShowingHint (currentEncounter gameState) <= hitsTakenDueToCurrentEnemy gameState

renderScene :: TextureData -> Renderer -> V2 CInt -> Scene -> IO ()
renderScene textures renderer windowDimensions scene = case scene of
    InGame _ gameState -> render textures renderer windowDimensions gameState
    TitleScreen -> do
        renderBackground textures renderer

        -- Test square
        rendererDrawColor renderer $= V4 0 200 20 255 
        fillRect renderer $ Just $ Rectangle (P $ V2 100 100) (V2 200 20)

    LevelWonScreen _ -> do
        renderBackground textures renderer

        -- Test white square
        rendererDrawColor renderer $= white
        fillRect renderer $ Just $ Rectangle (P $ V2 100 100) (V2 200 20)

    GameWonScreen -> do
        renderBackground textures renderer

        -- Test red square
        rendererDrawColor renderer $= red
        fillRect renderer $ Just $ Rectangle (P $ V2 100 100) (V2 200 20)

renderBackground :: TextureData -> Renderer -> IO ()
renderBackground textures renderer = do
    -- Blue sky
    rendererDrawColor renderer $= (V4 0 153 255 255)
    fillRect renderer Nothing

    -- Grass tiles
    traverse_ (copy renderer (grassTile textures) Nothing) $ map (Just . square grassTileSize . P . flip V2 600) $
        map (* grassTileSize) [0..5]

white :: V4 Word8
white = V4 255 255 255 255

red :: V4 Word8
red = V4 255 0 0 255

levelRestartEffectDuration :: Nat
levelRestartEffectDuration = nat 1000

render :: TextureData -> Renderer -> V2 CInt -> GameState -> IO ()
render textures renderer windowDimensions gameState = do
    
    renderBackground textures renderer

    -- Enemy
    let
        enemyAttackAnimationFraction :: Double
        enemyAttackAnimationFraction = natAsDouble (enemyAttackTimer gameState) / natAsDouble timeBetweenEnemyAttacks

        animationTimeSpeedup :: Double
        animationTimeSpeedup = 3

        enemyXOffset' :: Double -- Does not take into account the walking in animation
        enemyXOffset'
            | isJust (timeUntilComboTimeout gameState) = 0
            | enemyAttackAnimationFraction < recip animationTimeSpeedup = negate $ windupThenAttackEase $ 1 - animationTimeSpeedup * enemyAttackAnimationFraction
            | otherwise = 0

        enemyXOffset :: Double
        enemyXOffset = case enemyWalkInTimer gameState of
            Nothing -> 900 * enemyXOffset' -- The distance from the enemy to the player is of about 900 pixels
            Just t -> let walkInAnimationRatio = natAsDouble t / natAsDouble enemyWalkInDuration
                in 500 * walkInAnimationRatio

    copy renderer (enemyTexture (currentEnemy gameState) textures) Nothing $ Just $
        -- Rectangle (P $ V2 (1000 + round (enemyXOffsetRatio * 900)) (enemyY $ currentEnemy gameState)) (enemySize $ currentEnemy gameState)
        Rectangle (P $ V2 (round $ 1000 + enemyXOffset) (enemyY $ currentEnemy gameState)) (enemySize $ currentEnemy gameState)

    -- Fighter
    let fighterTopLeft = P $ V2 (-150) 100
        fighterRect = Rectangle fighterTopLeft playerSpriteSize
        attackingFighterRect = moveRectangle (P $ V2 550 0) fighterRect
        inCombo = comboToDefeat (currentEnemy gameState) /= comboKeysLeft gameState
        currentIdleSprite = Just $ spriteRect $ round $ natAsDouble (timeSinceStart gameState) / (300 :: Double)
        drawFighter
            -- Sadness lines maybe?
            | isJust (sadnessEffect gameState) = copy renderer (sadFighter textures) Nothing (Just fighterRect)
            | inCombo = copy renderer (fighterIdle textures) currentIdleSprite (Just $ attackingFighterRect)
            | otherwise = copy renderer (fighterIdle textures) currentIdleSprite (Just fighterRect)

    case currentAttack gameState of
        Nothing -> drawFighter
        Just attackKey -> copy renderer (comboKeyAttackPose attackKey textures) Nothing
            (Just $ moveRectangle (P $ V2 150 0) attackingFighterRect)

    -- Health bar:

    -- White background
    rendererDrawColor renderer $= white
    fillRect renderer $ Just $ Rectangle (P $ V2 100 700) (V2 200 20)

    when (shouldDisplayHint gameState) $ drawKeysToPressHint renderer textures gameState windowDimensions
    -- drawKeysToPressHint renderer textures gameState windowDimensions

    -- Red indicator
    rendererDrawColor renderer $= red
    fillRect renderer $ Just $ Rectangle (P $ V2 100 700) (V2 (round $ 200 * (natAsDouble $ playerHealth gameState) / natAsDouble maxHealth) 20)

    -- Flying enemy effect

    whenJust (flyingEnemyEffect gameState) $ \effect ->
        let defaultEnemyPositionOnScreen = P $ V2 1000 (enemyY $ effectEnemy effect)
        in copyEx renderer
            (enemyTexture (effectEnemy effect) textures) -- Texture to copy
            Nothing -- Copy the whole texture
            (Just $ Rectangle (defaultEnemyPositionOnScreen + currentEffectOffset effect) (enemySize $ effectEnemy effect)) -- Where to copy to
            (currentEffectAngle effect) -- The angle of rotation
            Nothing -- Rotate aroung the center
            (V2 False False) -- Don't flip it

    -- Hit Effect
    whenJust (hitEffectInProgress gameState) $ \timeLeft ->
        let ratioCompleted = 1 - (natAsDouble timeLeft / natAsDouble hitEffectDuration)
            frameCount = 7
            animationFrame = round $ ratioCompleted * frameCount
            frameWidth = 64
            frameHeight = 64
            size = V2 frameWidth frameHeight
            frameRect = Just $ Rectangle (P $ V2 (frameWidth * animationFrame) 0) size
        in copy renderer (hitEffect textures) frameRect (Just $ Rectangle (P $ V2 700 0) (15 * size))

    -- Level restart graphic
    whenJust (levelRestartEffect gameState) $ \timeLeft ->
        let ratioCompleted = 1 - (natAsDouble timeLeft / natAsDouble levelRestartEffectDuration)
        in do
          rendererDrawColor renderer $= white
          fillRect renderer $ Just $ Rectangle (P $ V2 500 500) (V2 300 300)

-- In miliseconds
effectTimeAlive :: FlyingEnemyEffect -> Nat
effectTimeAlive effect = monus flyingEnemyEffectTime $ timeUntilDisappearing effect 

currentEffectAngle :: FlyingEnemyEffect -> CDouble
currentEffectAngle effect = CDouble $ (natAsDouble $ effectTimeAlive effect) * angularVelocity effect

currentEffectOffset :: FlyingEnemyEffect -> Point V2 CInt
currentEffectOffset effect = P $ V2 (round $ flyAwaySpeed * sin radians * timeAlive) (round $ flyAwaySpeed * cos radians * timeAlive)
  where
    flyAwaySpeed :: Double
    flyAwaySpeed = 2

    radians :: Double
    radians = (angle effect / 360) * 2 * pi

    timeAlive :: Double
    timeAlive = natAsDouble $ effectTimeAlive effect

whenJust :: Applicative m => Maybe a -> (a -> m ()) -> m ()
whenJust x f = maybe (pure ()) f x

drawKeysToPressHint :: Renderer -> TextureData -> GameState -> V2 CInt -> IO ()
drawKeysToPressHint renderer textures gameState windowDimensions = do

    let pressedKeys = alreadyPressed gameState
        keysYetToBePressed = comboKeysLeft gameState
        totalKeyDisplayWidth = fromIntegral keyIconSize * (length pressedKeys + length keysYetToBePressed)
        (V2 screenWidth _) = windowDimensions
        keyDisplayXCoord :: Double
        keyDisplayXCoord = fromIntegral screenWidth / 2 - fromIntegral totalKeyDisplayWidth / 2

    -- Draw the UI elements that represent the keys the user has pressed and has yet to press
    sequence_ $ zipWith (drawKeyIcon renderer textures True)
        (map (P . flip V2 100) $ map ((+ round keyDisplayXCoord) . (*keyIconSize)) [0..])
        pressedKeys

    sequence_ $ zipWith (drawKeyIcon renderer textures False)
        (map (P . flip V2 100) $ map ((+ round keyDisplayXCoord) . (*keyIconSize)) [fromIntegral $ length pressedKeys..])
        (NonEmpty.toList keysYetToBePressed)

keyIconSize :: CInt
keyIconSize = 128

drawKeyIcon :: Renderer -> TextureData -> Bool -> Point V2 CInt -> ComboKey -> IO ()
drawKeyIcon renderer textureData pressed whereToDraw comboKey = copy renderer (keyboardKeys textureData)
    (Just $ getKeyRect pressed comboKey) (Just $ Rectangle whereToDraw (V2 keyIconSize keyIconSize))

loadGameTextures :: Renderer -> IO TextureData
loadGameTextures renderer = MkTextureData
    <$> loadTexture renderer (png "fighter-idle")
    <*> loadTexture renderer (png "grass")
    <*> loadTexture renderer (png "keyboard-key-icons")
    <*> loadTexture renderer (png "rat")
    <*> loadTexture renderer (png "blue-rat")
    <*> loadTexture renderer (png "spider")
    <*> loadTexture renderer (png "fighter-sad")
    <*> loadTexture renderer (png "attack-pose")
    <*> loadTexture renderer (png "attack-kick")
    <*> loadTexture renderer (png "low-kick")
    <*> loadTexture renderer (png "hit-effect")
    <*> loadTexture renderer (png "headbutt")
    <*> loadTexture renderer (png "left-punch")
    <*> loadTexture renderer (png "crouch-punch")
    <*> loadTexture renderer (png "knee-strike")
    <*> loadTexture renderer (png "jump-kick")
    <*> loadTexture renderer (png "evil-clone")
    <*> loadTexture renderer (png "green-rat")
    <*> loadTexture renderer (png "purple-spider")
    <*> loadTexture renderer (png "red-spider")
  where
    png :: String -> String
    png x = "assets/" ++ x ++ ".png"

main :: IO ()
main = do
    initializeAll

    window <- createWindow "Combo!" defaultWindow
    setWindowMode window FullscreenDesktop

    renderer <- createRenderer window (-1) defaultRenderer { rendererType = AcceleratedVSyncRenderer }
    rendererLogicalSize renderer $= Just (V2 1366 768)

    initialTicks <- ticks
    textures <- loadGameTextures renderer
    windowDimensions <- get $ windowSize window
    -- firstMousePos <- getAbsoluteMouseLocation

    Mix.initialize [Mix.InitMP3]
    Mix.openAudio def 256

    audioData <- loadAudioData
    gameLoop renderer textures audioData windowDimensions initialTicks initialScene

    freeAudioData audioData
    Mix.closeAudio
    Mix.quit

    -- quit SDL
    quit

{-
TODO

    Attack pose: 1 missing
        - Dragon ball like hit with two hands

    Goal: 5 Levels
        (Estimating 10 to 12 encounters each)
    Goal: 10 enemies

    Draw:
      Title screen
      You beat the level! screen
      Level restart graphic

Optional:
    Keys per second bonus - Fast = Good (give health back)
    Game mode with random enemies
    Player dead drawing and effect
-}

{-
Enemies can vary in:
    - Size
    - Color
    - "Species"
-}
