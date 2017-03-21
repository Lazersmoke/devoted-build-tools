#!/usr/bin/env stack
-- stack --install-ghc runghc
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

import System.IO
import System.Environment
import System.Process
import Control.Monad
import System.Directory
import Prelude hiding (FilePath)

supportedMCVersions :: [String]
supportedMCVersions =
  ["1.10.2"
  ,"1.8.8"
  ,"1.8.7"
  ]

main :: IO ()
main = doesDirectoryExist "devoted" >>= \serverSetup -> getArgs >>= \case
  -- If they didn't give any arguments, tell them to
  [] -> putStrLn "Please specify a Minecraft version to build with. Try one of: " >> mapM_ print supportedMCVersions
  -- If they say to cleanup, cleanup
  ("clean":_) -> cleanup
  -- If they say to stop, stop
  ("stop":_) -> if not serverSetup
    then putStrLn "You need to setup the server with `./setup.hs version <Minecraft Version>` before stopping it"
    else setCurrentDirectory "devoted" >> stopServer >> return ()
  -- If they say to start, start
  ("start":_) -> if not serverSetup
    then putStrLn "You need to setup the server with `./setup.hs version <Minecraft Version>` before starting it"
    else setCurrentDirectory "devoted" >> startServer >> return ()
  -- If the are giving us a minecraft version, check it
  ("version":mcVer:_) -> if not $ mcVer `elem` supportedMCVersions
    -- If they gave us an unsupported (or non-sense) version, tell them so
    then putStrLn $ "Unsupported Minecraft version: " ++ show mcVer
    -- Make sure they didn't already run the script and create the files
    else if serverSetup
      -- If they already set it up, warn and exit
      then putStrLn "Warning: `devoted` folder already exists. Please run `./setup.hs clean` or remove it manually before reinstalling"
      -- Otherwise, proceed with the installation
      else do
        -- Download Mojangcraft and set it up
        installVanilla mcVer
        -- Replace Mojangcraft with Spigot
        buildSpigot mcVer
        putStrLn "Done!"
  -- If the argument is non-sense, warn and exit
  _ -> putStrLn "Invalid arguments. Try `./setup.hs clean` or `./setup.hs version <Minecraft Version>`"

cleanup :: IO ()
cleanup = do
  -- Delete both folders, making sure the exist first
  forM_ ["devoted","devoted-spigot","spigot-build"] $ \x -> doesDirectoryExist x >>= flip when (removeDirectoryRecursive x)
  putStrLn $ "All clean!"

installVanilla :: String -> IO ()
installVanilla mcVer = do
  -- Create the directory we will install into
  createDirectoryIfMissing True "devoted"
  -- Move into the new install directory
  setCurrentDirectory "devoted"
  -- Grab Mojangcraft jar from the offical download link
  -- Injection here is impossible because `elem mcVer supportedMCVersions`
  callProcess "wget" ["https://s3.amazonaws.com/Minecraft.Download/versions/" ++ mcVer ++ "/minecraft_server." ++ mcVer ++ ".jar","-O","minecraft_server.jar"]
  -- Premptively agree to the EULA. Please no murder me M$ :3
  writeFile "eula.txt" "eula=true"
  -- Let the server initialize. The important thing is that it accepts our eula agreement
  startServer
  stopServer
  -- Back out to main directory for spigot build
  setCurrentDirectory ".."

buildSpigot :: String -> IO ()
buildSpigot mcVer = do
  -- Create and enter the build directory
  createDirectoryIfMissing True "spigot-build"
  setCurrentDirectory "spigot-build"
  -- Get the spigot build tools ready
  callProcess "wget" ["https://hub.spigotmc.org/jenkins/job/BuildTools/lastSuccessfulBuild/artifact/target/BuildTools.jar","-O","BuildTools.jar"]
  -- Spigot build tools like to have this unset for whatever reason; discard the exit code cause it can be 5 for some reason /shrug
  _ <- waitForProcess =<< spawnCommand ("git config --global --unset core.autocrlf")
  -- Actually build Spigot
  putStrLn "Building spigot, might take a while"
  callProcess "java" ["-Xmx1500M","-jar","BuildTools.jar","--rev",mcVer]
  -- We are done building, so back out of the directory
  setCurrentDirectory ".."
  -- Make sure that it compiled properly and actually generated an artifact
  let spigotJar = "spigot-" ++ mcVer ++ ".jar"
  let cbJar = "craftbukkit-" ++ mcVer ++ ".jar"
  let spigotBuild = "spigot-build/"
  compilationWorked <- doesFileExist (spigotBuild ++ spigotJar)
  if not compilationWorked
    then putStrLn "Failed to compile Spigot"
    else do
      -- Create the spigot artifact storage folder
      createDirectoryIfMissing True "devoted-spigot"
      -- Install the Spigot jar into the minecraft server
      copyFile (spigotBuild ++ spigotJar) "devoted/minecraft_server.jar"
      -- Archive the Spigot and Craftbukkit jars
      copyFile (spigotBuild ++ spigotJar) ("devoted-spigot/" ++ spigotJar)
      copyFile (spigotBuild ++ cbJar) ("devoted-spigot/" ++ cbJar)
      -- Install Spigot and CB pom files
      copyFile (spigotBuild ++ "Spigot/Spigot-Server/pom.xml") "devoted-spigot/spigot-pom.xml"
      copyFile (spigotBuild ++ "CraftBukkit/pom.xml") "devoted-spigot/craftbukkit-pom.xml"
      -- Install Spigot jars into Maven
      callProcess "mvn" ["install:install-file","-Dfile=" ++ spigotBuild ++ spigotJar,"-Dpackaging=jar","-DpomFile=" ++ spigotBuild ++ "Spigot/Spigot-Server/pom.xml"]
      callProcess "mvn" ["install:install-file","-Dfile=" ++ spigotBuild ++ cbJar,"-Dpackaging=jar","-DpomFile=" ++ spigotBuild ++ "CraftBukkit/pom.xml"]

-- Start the server (must be inside "devoted" upon call)
startServer :: IO ()
startServer = do
  putStrLn "Starting Server..."
  -- 'screen -d -R' will attach to an existing session if possible, or start the server if it isn't already
  callCommand $ devotedScreen ++ "-d -R bash -c 'java -Xmx1500M -Xms500M -XX:MaxPermSize=256M -jar minecraft_server.jar nogui'"

-- Stop the server (must be inside "devoted" upon call)
stopServer :: IO ()
stopServer = do
  putStrLn "Stopping Server..."
  callCommand $ devotedScreen ++ "-X stuff 'stop\n'"

devotedScreen :: String
devotedScreen = "screen -S mc_screen -p 0 "
