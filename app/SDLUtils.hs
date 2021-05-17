module SDLUtils
    ( sdlLoaderFullscreenMain
    , sdlLoaderFullscreenMainNoSound
    , screenWidth
    , screenHeight
    , getKeyboardEvent
    , getScancode
    , getKeycode
    , ensure
    , isPressed
    , isReleased
    , isEnterPressed
    , shouldQuit
    , wasScancodeReleased
    , moveRectangle
    ) where

import Control.Monad ((<=<), guard)
import Data.Functor (($>))
import Data.Maybe (mapMaybe)
import Data.Text (Text)
import Data.Word (Word32)
import Foreign.C.Types (CInt)

import Data.Default.Class (def)
import SDL
import qualified SDL.Mixer as Mix
import qualified SDL.Image (quit)

screenWidth :: CInt
screenWidth = 1366

screenHeight :: CInt
screenHeight = 768

sdlLoaderFullscreenMain
    :: Text -- Window name
    -> (Renderer -> IO textures) -- How to load the textures
    -> IO audio -- How to load the audio
    -> (audio -> IO ()) -- How to free the audio
    -> (Renderer -> textures -> audio -> Word32 -> IO ()) -- The gameloop
    -> IO ()
sdlLoaderFullscreenMain windowName loadGameTextures loadAudioData freeAudioData gameLoop = do
    initializeAll

    window <- createWindow windowName defaultWindow
    setWindowMode window FullscreenDesktop

    renderer <- createRenderer window (-1) defaultRenderer { rendererType = AcceleratedVSyncRenderer }
    rendererLogicalSize renderer $= Just (V2 screenWidth screenHeight)

    initialTicks <- ticks
    textures <- loadGameTextures renderer

    Mix.initialize [Mix.InitMP3]
    Mix.openAudio def 256

    audioData <- loadAudioData
    gameLoop renderer textures audioData initialTicks

    freeAudioData audioData
    Mix.closeAudio
    Mix.quit

    -- quit SDL
    SDL.Image.quit
    quit

sdlLoaderFullscreenMainNoSound :: Text -> (Renderer -> IO textures) -> (Renderer -> textures -> Word32 -> IO ()) -> IO ()
sdlLoaderFullscreenMainNoSound name loadGameTextures gameLoop = sdlLoaderFullscreenMain name loadGameTextures (pure ()) (const $ pure ()) $
    \renderer textures () -> gameLoop renderer textures

getScancode :: KeyboardEventData -> Scancode
getScancode = keysymScancode . keyboardEventKeysym

getKeycode :: KeyboardEventData -> Keycode
getKeycode = keysymKeycode . keyboardEventKeysym

getKeyboardEvent :: EventPayload -> Maybe KeyboardEventData
getKeyboardEvent (KeyboardEvent event) = Just event
getKeyboardEvent _ = Nothing

-- Works for alt-f4 or the window's X button
shouldQuit :: [Event] -> Bool
shouldQuit = any $ (QuitEvent ==) . eventPayload

ensure :: (a -> Bool) -> a -> Maybe a
ensure p a = guard (p a) $> a

isPressed :: KeyboardEventData -> Bool
isPressed = (== Pressed) . keyboardEventKeyMotion

isReleased :: KeyboardEventData -> Bool
isReleased = (== Released) . keyboardEventKeyMotion

hasKeyStateChanged :: Eq a => (KeyboardEventData -> Bool) -> (KeyboardEventData -> a) -> a -> [Event] -> Bool
hasKeyStateChanged pressedness getKeyIdentifier goalKeyIdentifier =
    not . null . (mapMaybe $ ensure (== goalKeyIdentifier) <=< fmap getKeyIdentifier  . ensure pressedness <=< getKeyboardEvent . eventPayload)

isKeyPressed :: Keycode -> [Event] -> Bool
isKeyPressed = hasKeyStateChanged isPressed getKeycode

wasScancodeReleased :: Scancode -> [Event] -> Bool
wasScancodeReleased = hasKeyStateChanged isReleased getScancode

isEnterPressed :: [Event] -> Bool
isEnterPressed = isKeyPressed KeycodeReturn

moveRectangle :: Point V2 CInt -> Rectangle CInt -> Rectangle CInt
moveRectangle p (Rectangle p' size) = Rectangle (p' + p) size

