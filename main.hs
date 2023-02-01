import Discord
import Discord.Types
import Control.Monad (void)
import Data.Text (Text)
import qualified Data.Text as T

main :: IO ()
main = do
  -- Load the configuration
  (token, prefix) <- loadConfig

  -- Connect to Discord API
  withManager $ \m -> do
    -- Start the bot
    void $ runDiscord m $ def
      { discordToken = token
      , discordOnMessageCreate = onMessageCreate prefix
      }

loadConfig :: IO (Token, Text)
loadConfig = do
  -- Load the Discord API token
  token <- T.strip <$> readFile "config/token.txt"

  -- Load the command prefix
  prefix <- T.strip <$> readFile "config/prefix.txt"

  return (token, prefix)

onMessageCreate :: Text -> DiscordHandle -> Message -> IO ()
onMessageCreate prefix dh message = do
  -- Check if the message was sent by a user and not a webhook
  if not (messageAuthorIsWebhook message) && messageAuthorIsUser message
    then do
      -- Check if the message starts with the command prefix
      let content = messageContent message
      if T.isPrefixOf prefix content
        then do
          -- Check if the command is "ping"
          let command = T.stripStart (T.drop (T.length prefix) content)
          if command == "ping"
            then do
              -- Send the latency
              void $ discordCreateMessage dh (messageChannel message) (T.pack (show (discordShardLatency dh)))
            -- Do nothing otherwise
            else return ()
        -- Do nothing if the message does not start with the command prefix
        else return ()
    -- Do nothing if the message was sent by a webhook
    else return ()
