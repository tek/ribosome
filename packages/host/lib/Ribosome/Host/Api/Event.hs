module Ribosome.Host.Api.Event where

import Ribosome.Host.Api.Data (Buffer)
import Ribosome.Host.Class.Msgpack.Array (msgpackArray)
import Ribosome.Host.Class.Msgpack.Decode (pattern Msgpack)
import Ribosome.Host.Data.Event (Event (Event))

pattern BufLinesEvent :: Buffer -> Maybe Int -> Int -> Int -> [Text] -> Bool -> Event
pattern BufLinesEvent {buffer, changedtick, firstline, lastline, linedata, more} <- Event "nvim_buf_lines_event" [
  Msgpack buffer,
  Msgpack changedtick,
  Msgpack firstline,
  Msgpack lastline,
  Msgpack linedata,
  Msgpack more
  ] where
    BufLinesEvent b c f l ld m =
      Event "nvim_buf_lines_event" (msgpackArray b c f l ld m)
