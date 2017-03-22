#!/usr/bin/env stack
-- stack --install-ghc runghc
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

import Data.List (isInfixOf)
import System.IO.Unsafe (unsafePerformIO)
import System.Environment
import System.Process
import Control.Monad
import System.Directory
import System.FilePath
import Prelude hiding (FilePath)

supportedMCVersions :: [String]
supportedMCVersions =
  ["1.10.2"
  ,"1.8.8"
  ,"1.8.7"
  ]

devotedDir :: FilePath
devotedDir = unsafePerformIO $ makeAbsolute "devoted"

spigotDir :: FilePath
spigotDir = unsafePerformIO $ makeAbsolute "devoted-spigot"

spigotBuild :: FilePath
spigotBuild = unsafePerformIO $ makeAbsolute "spigot-build"

-- print is a horrible hack to prevent spigotBuild and spigotDir from being forced while current directory is devotedDir
main :: IO ()
main = print [devotedDir,spigotDir,spigotBuild] >> getArgs >>= \case
  -- If they didn't give any arguments, tell them to
  [] -> putStrLn "Please specify a command. Try: `./setup.hs clean` or `./setup.hs install 1.10.2`"
  ("clean":_) -> cleanup
  ("stop":_) -> stopServer
  ("attach":_) -> attachServer
  ("start":_) -> startServer
  ("install":mcVer:_) ->
    -- If the are giving us a minecraft version, check it, warning them if it is unsupported
    ensure (return $ mcVer `elem` supportedMCVersions) unsupportedVer $
    -- Make sure they didn't already run the script and create the files
    ensure (not <$> doesDirectoryExist devotedDir) alreadyExists $ do
      -- If all is well, proceed with the setup
      -- Download Mojangcraft and set it up
      installVanilla mcVer
      -- Replace Mojangcraft with Spigot
      buildSpigot mcVer
      putStrLn "Done!"
    where
    -- If they gave us an unsupported (or non-sense) version, tell them so
      unsupportedVer = "Unsupported Minecraft version: \"" ++ show mcVer ++ "\". Try one of: \n" ++ unlines supportedMCVersions
      -- If they already set it up, warn and exit
      alreadyExists = "Warning: `devoted` folder already exists. Please run `./setup.hs clean` or remove it manually before reinstalling"
  -- If the argument is non-sense, warn and exit
  _ -> putStrLn "Invalid arguments. Try `./setup.hs clean` or `./setup.hs install <Minecraft Version>`"

cleanup :: IO ()
cleanup = do
  -- Delete all directories, making sure they exist so that we don't get errors
  forM_ [devotedDir,spigotDir,spigotBuild] $ \x -> doesDirectoryExist x >>= flip when (removeDirectoryRecursive x)
  putStrLn "All clean!"

installVanilla :: String -> IO ()
installVanilla mcVer = do
  -- Create the directory we will install into
  createDirectoryIfMissing True devotedDir
  -- Grab Mojangcraft jar from the offical download link
  -- Injection here is impossible because `elem mcVer supportedMCVersions`
  callProcess "wget" ["https://s3.amazonaws.com/Minecraft.Download/versions/" ++ mcVer ++ "/minecraft_server." ++ mcVer ++ ".jar","-O",devotedDir </> "minecraft_server.jar"]
  -- Premptively agree to the EULA. Please no murder me M$ :3
  writeFile (devotedDir </> "eula.txt") "eula=true"
  -- Let the server initialize. The important thing is that it accepts our eula agreement
  startServer
  stopServer

buildSpigot :: String -> IO ()
buildSpigot mcVer = do
  -- Create and enter the build directory
  createDirectoryIfMissing True spigotBuild
  -- Get the spigot build tools ready
  callCommand $ "wget https://hub.spigotmc.org/jenkins/job/BuildTools/lastSuccessfulBuild/artifact/target/BuildTools.jar -O " ++ spigotBuild </> "BuildTools.jar"
  -- Spigot build tools like to have this unset for whatever reason; discard the exit code cause it can be 5 for some reason /shrug
  _ <- waitForProcess =<< spawnCommand "git config --global --unset core.autocrlf"
  -- Go into the build directory so BuildTools.jar puts its files in the right place
  setCurrentDirectory spigotBuild
  -- Actually build Spigot
  putStrLn "Building spigot, might take a while"
  callProcess "java" ["-Xmx1500M","-jar","BuildTools.jar","--rev",mcVer]
  -- Make sure that it compiled properly and actually generated an artifact
  let spigotJar = "spigot-" ++ mcVer ++ ".jar"
  let cbJar = "craftbukkit-" ++ mcVer ++ ".jar"
  -- Ensure the build worked
  ensure (doesFileExist (spigotBuild </> spigotJar)) "Failed to compile Spigot" $ do
    -- Create the spigot artifact storage folder
    createDirectoryIfMissing True spigotDir
    -- Install the Spigot jar into the minecraft server
    copyFile (spigotBuild </> spigotJar) (devotedDir </> "minecraft_server.jar")
    -- Archive the Spigot and Craftbukkit jars
    copyFile (spigotBuild </> spigotJar) (spigotDir </> spigotJar)
    copyFile (spigotBuild </> cbJar) (spigotDir </> cbJar)
    -- Install Spigot and CB pom files
    copyFile (spigotBuild </> "Spigot/Spigot-Server/pom.xml") (spigotDir </> "spigot-pom.xml")
    copyFile (spigotBuild </> "CraftBukkit/pom.xml") (spigotDir </> "craftbukkit-pom.xml")
    -- Install Spigot jars into Maven
    callProcess "mvn" ["install:install-file","-Dfile=" ++ spigotBuild </> spigotJar,"-Dpackaging=jar","-DpomFile=" ++ spigotBuild </> "Spigot/Spigot-Server/pom.xml"]
    callProcess "mvn" ["install:install-file","-Dfile=" ++ spigotBuild </> cbJar,"-Dpackaging=jar","-DpomFile=" ++ spigotBuild </> "CraftBukkit/pom.xml"]

-- Server directory must exist in order to start the server
startServer :: IO ()
startServer = ensureServerExists $ do
  -- Go into server directory so screen can find `minecraft_server.jar`
  setCurrentDirectory devotedDir
  putStrLn "Starting Server..."
  -- `-dmS` is dameon mode; screen will run in the background. To control it manually, see `attachServer`
  callCommand $ "screen -dmS mc_screen -p 0 bash -c 'java -Xmx1500M -Xms500M -XX:MaxPermSize=256M -jar minecraft_server.jar nogui'"

-- The server must be up in order to attach to it
attachServer :: IO ()
attachServer = ensureServerUp $ do
  putStrLn "Attaching to Server..."
  -- `-r` will attach the console to the server screen
  callCommand $ "screen -S mc_screen -p 0 -r"

-- Stop the server (the server must be up to do this)
stopServer :: IO ()
stopServer = ensureServerUp $ do
  putStrLn "Stopping Server..."
  -- `-X stuff 'stop\n'` will push the text "stop" and newline into screen, issuing the /stop command to the server
  callCommand $ "screen -S mc_screen -p 0 -X stuff 'stop\n'"

-- The server being up ~= screen lists "mc_screen" as one of the present screens
ensureServerUp :: IO () -> IO ()
ensureServerUp = ensureServerExists . ensure (isInfixOf "mc_screen" . (\(_,a,_) -> a) <$> readProcessWithExitCode "screen" ["-list"] "") "Server is not running!"

-- The server existing ~= the server directory exists
ensureServerExists :: IO () -> IO ()
ensureServerExists = ensure (doesDirectoryExist devotedDir) $ "\"" ++ devotedDir ++ "\" does not exist! Try running `./setup.hs install <Minecraft Version>` to setup the server."

-- Ensure some invariant, on threat of printing an error message and not calling the continuation
ensure :: IO Bool -> String -> IO () -> IO ()
ensure p f x = p >>= \b -> if b then x else putStrLn f
