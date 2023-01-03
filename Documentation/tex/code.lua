-- code.lua
--
-- This file is part of LilyPond, the GNU music typesetter.
--
-- Copyright (C) 2021--2023 Werner Lemberg <wl@gnu.org>
--
-- LilyPond is free software: you can redistribute it and/or modify
-- it under the terms of the GNU General Public License as published by
-- the Free Software Foundation, either version 3 of the License, or
-- (at your option) any later version.
--
-- LilyPond is distributed in the hope that it will be useful,
-- but WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
-- GNU General Public License for more details.
--
-- You should have received a copy of the GNU General Public License
-- along with LilyPond.  If not, see <http://www.gnu.org/licenses/>.


-- If the LilyPond documentation is compiled with LuaTeX, Texinfo's
-- `@code` macro gets enhanced by this Lua code to do the following
-- things.
--
-- (0) For further analysis, break the argument to `@code` into words.
--     Words that already contain discretionaries and penalties (for
--     example, by inserting `@/` or `@-`) are ignored.
-- (1) Add possible line breaks after `-` and `_`.
-- (2) Avoid that single-character words at the start or the end of
--     the `@code` argument are positioned at the end or start of an
--     output line, respectively.  A typical case with LilyPond code
--     is `@code{@{ ... @}}`.
--
-- There will be at least two characters at the start or the end of a
-- word before inserting a hyphenation point or a possible line break.
--
-- Note that this code only works if `@allowcodebreaks false` is set.


-- Some shorthands.
local char_hyphen = 0x2d
local char_underscore = 0x5f

local umatch = unicode.utf8.match
local uchar = unicode.utf8.char

local DISC = node.id("disc")
local GLUE = node.id("glue")
local GLYPH = node.id("glyph")
local HLIST = node.id("hlist")
local PENALTY = node.id("penalty")
local WHAT = node.id("whatsit")


-- This value must be the same as set in `common-macros.itexi` (using
-- `\attribute`).
local code_attribute = 200


-- This is the function to be additionally registered to the
-- 'hyphenate' callback.  In the chain of callbacks, this one should
-- be executed first.
code_hyphenate = function(head)
  local words = {}
  local idx = 1
  local word_start = 0
  local word_len = 0
  local prev_font = -1
  local only_characters = true

  -- Loop over all nodes to find start and length of words, to be
  -- stored in array `words`.
  for n in node.traverse(head) do
    local in_word = false

    -- Handle only stuff in `@code`.
    if node.has_attribute(n, code_attribute) then
      -- Only characters typeset with the same font are considered to
      -- be in the same word.
      if n.id == GLYPH and (prev_font == -1 or prev_font == n.font) then
        in_word = true
      end

      -- We only take care of discretionaries and penalties inside of
      -- words.
      if word_len > 0 then
        if n.id == DISC then
          in_word = true
          only_characters = false
        elseif n.id == PENALTY then
          in_word = true
          -- If `@allowcodebreaks false` is set (which we expect), a
          -- hyphen also inserts `\penalty 10000`.
          if n.penalty ~= 10000 then
            only_characters = false
          end
        end
      end
    end

    if in_word then
      if word_len == 0 then
        word_start = n
      end

      word_len = word_len + 1

      if n.id == GLYPH then
        prev_font = n.font
      end
    else
      -- Ignore words that already contain discretionaries or
      -- penalties with a value different from 10000.
      if word_len > 0 and only_characters then
        words[idx] = { word_start, word_len }
        idx = idx + 1
      end

      only_characters = true

      -- The current node might still start a new word.
      if node.has_attribute(n, code_attribute) and n.id == GLYPH then
        word_start = n
        word_len = 1
        prev_font = n.font
      else
        word_len = 0
        prev_font = -1
      end
    end
  end

  -- Now loop over all collected words.
  for _, word in ipairs(words) do
    word_start, word_len = table.unpack(word)

    -- Check for
    --
    -- ```
    -- [<letter>_] [<letter>_] - <penalty> <letter> [<letter>_]
    -- ```
    --
    -- and adjust the penalty after `-` if we have a hit.
    if word_len >= 6 then
      local start = word_start
      local len = word_len

      while len >= 6 do
        local n1 = start
        local n2 = n1.next
        local n3 = n2.next
        local n4 = n3.next
        local n5 = n4.next
        local n6 = n5.next

        if n1.id == GLYPH and umatch(uchar(n1.char), "[%l%u_]")
             and n2.id == GLYPH and umatch(uchar(n2.char), "[%l%u_]")
             and n3.id == GLYPH and n3.char == char_hyphen
             and n4.id == PENALTY
             and n5.id == GLYPH and umatch(uchar(n5.char), "[%l%u]")
             and n6.id == GLYPH and umatch(uchar(n6.char), "[%l%u_]") then
          n4.penalty = tex.hyphenpenalty

          len = len - 4
          start = n5
        else
          len = len - 1
          start = n2
        end
      end
    end

    -- Check for
    --
    -- ```
    -- [<letter>_] [<letter>_] _ <letter> [<letter>_]
    -- ```
    --
    -- and insert a penalty after `_` if we have a hit.
    if word_len >= 5 then
      local start = word_start
      local len = word_len
      local num_added_nodes = 0

      while len >= 5 do
        local n1 = start
        local n2 = n1.next
        local n3 = n2.next
        local n4 = n3.next
        local n5 = n4.next

        if n1.id == GLYPH and umatch(uchar(n1.char), "[%l%u_]")
             and n2.id == GLYPH and umatch(uchar(n2.char), "[%l%u_]")
             and n3.id == GLYPH and n3.char == char_underscore
             and n4.id == GLYPH and umatch(uchar(n4.char), "[%l%u]")
             and n5.id == GLYPH and umatch(uchar(n5.char), "[%l%u_]") then
          local pen = node.new(PENALTY)
          pen.penalty = tex.hyphenpenalty
          node.set_attribute(pen, code_attribute, 1)
          node.insert_after(head, n3, pen)

          num_added_nodes = num_added_nodes + 1

          len = len - 3
          start = n4
        else
          len = len - 1
          start = n2
        end
      end

      word_len = word_len + num_added_nodes
    end
  end -- end of for-loop

  for n in node.traverse(head) do
    -- Check whether there is a single character at the beginning of
    -- `@code`, followed by a space.  If we have a hit, insert a
    -- penalty after the character.
    do
      local non_code = n
      if not node.has_attribute(non_code, code_attribute) then
        local char = non_code.next
        if char
             and node.has_attribute(char, code_attribute)
             and char.id == GLYPH then
          local space = char.next
          if space
               and node.has_attribute(space, code_attribute)
               and space.id == GLUE then
            local pen = node.new(PENALTY)
            pen.penalty = 10000
            node.set_attribute(pen, code_attribute, 1)
            node.insert_after(head, char, pen)
          end
        end
      end
    end

    -- Check whether there is a single character at the end of
    -- `@code`, preceded by a space.  If we have a hit, insert a
    -- penalty before the space.
    do
      local space = n
      if node.has_attribute(space, code_attribute)
           and space.id == GLUE then
        local char = space.next
        if char
             and node.has_attribute(char, code_attribute)
             and char.id == GLYPH then
          -- We actually have to check for one more node because
          -- `@code` ends with a call to `\null`, which creates an
          -- empty hbox.
          local hbox = char.next
          if hbox
               and node.has_attribute(hbox, code_attribute)
               and hbox.id == HLIST then
            local non_code = hbox.next
            if non_code
                 and not node.has_attribute(non_code, code_attribute) then
              local pen = node.new(PENALTY)
              pen.penalty = 10000
              node.set_attribute(pen, code_attribute, 1)
              node.insert_before(head, space, pen)
            end
          end
        end
      end
    end
  end -- end of for-loop
end

-- eof
