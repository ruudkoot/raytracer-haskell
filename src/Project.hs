module Main where 

import Base.CLI                             -- Command line options and usage
import Renderer.Renderer      (renderFile)  -- Rendering the scene 
import System.Console.CmdArgs (cmdArgs)


-- | Renders the files given via command line 
-- arguments. See the --help switch for more 
-- information on specifying options.
--
main :: IO()
main = do cargs <- cmdArgs usage [standard]
          mapM_ (renderFile cargs) (files cargs)
 

