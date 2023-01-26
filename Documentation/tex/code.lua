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
-- There will be at least one character before the hyphen at the start
-- and two characters after the hyphen at the end of a word before
-- inserting a possible line break.
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

  -- Item (1)
  --
  -- In the following, we assume that underscores are a substitute for
  -- dashes because dashes are often not allowed in identifiers (this
  -- is the case for most computer languages); as a consequence, we
  -- treat them identically.
  --
  -- Here are all handled use-cases.  `|` marks a possible breakpoint,
  -- `x` is a character not equal to `-` or `_`, and `-` means either
  -- `-` or `_`.  `^` and `$` indicate the start and the end of a
  -- word, respectively.
  --
  --   allowed pattern  example
  --   ---------------- ------------------
  --   x-|xx            x-axis
  --   x-|x-            type-p-name-alist
  --   x--|xx           fo2--bar
  --   x--|x-           foo--2-bar
  --
  --   disallowed pattern  example
  --   ------------------- -----------------
  --   ^-|x                -foo
  --   ^--|x               --verbose
  --   -|x$                self-alignment-X
  --   --|x$               fo2--x
  --
  -- Putting the above together we get the following two regular
  -- expressions (with inserted spaces for better legibility).
  -- `(?=...)` is a positive, zero-width look-ahead assertion.
  --
  -- ```
  -- ([^_-]) ([_-]) ([_-]) (?=[^_-].)  ->  \1 \2 \3 <break>
  -- ([^_-])        ([_-]) (?=[^_-].)  ->  \1 \2 <break>
  -- ```
  --
  -- These regular expressions map to the cases below, which we are
  -- going to implement.  We are a bit more strict and define a
  -- 'character' as being either a letter or digit.
  --
  -- Note that in `texinfo.tex` a dash is actually already represented
  -- as a `-` glyph node followed by a penalty; we just have to adjust
  -- the penalty value.  On the other hand, we must insert a penalty
  -- node after `_` if necessary.
  --
  -- Cases with more than two subsequent dashes or hyphens are not
  -- handled and must be resolved manually.

  -- Now loop over all collected words.
  for _, word in ipairs(words) do
    word_start, word_len = table.unpack(word)

    -- Check for
    --
    -- ```
    -- <character> - <penalty> - <penalty> <character> <any character>
    -- <character> _           - <penalty> <character> <any character>
    -- <character>             - <penalty> <character> <any character>
    -- ```
    --
    -- and adjust the penalty after `-` if we have a hit.
    if word_len >= 5 then
      local start = word_start
      local len = word_len

      while len >= 5 do
        local cur = start
        local pen
        local skip = 0

        if not (cur.id == GLYPH
                and umatch(uchar(cur.char), "[%a%d]")) then
          goto no_match
        end

        cur = cur.next
        if cur.id ~= GLYPH then
          goto no_match
        end

        if cur.char == char_underscore
            and len >= 6 then
          cur = cur.next
          skip = 1
        elseif cur.char == char_hyphen
            and cur.next.id == PENALTY
            and cur.next.next.id == GLYPH
              and cur.next.next.char == char_hyphen
            and cur.next.next.next.id == PENALTY
            and len >= 7 then
          cur = cur.next.next
          skip = 2
        end

        if not (cur.id == GLYPH and cur.char == char_hyphen
                and cur.next.id == PENALTY) then
          goto no_match
        end

        pen = cur.next
        cur = cur.next.next

        if cur.id == GLYPH and umatch(uchar(cur.char), "[%a%d]")
            and cur.next.id == GLYPH then
          pen.penalty = tex.hyphenpenalty
          len = len - 3 - skip
          start = cur
          goto continue
        end

        ::no_match::
        len = len - 1
        start = start.next

        ::continue::
      end -- end of while-loop
    end

    -- Check for
    --
    -- ```
    -- <character> - <penalty> _ <character> <any character>
    -- <character> _           _ <character> <any character>
    -- <character>             _ <character> <any character>
    -- ```
    --
    -- and insert a penalty after `_` if we have a hit.
    if word_len >= 4 then
      local start = word_start
      local len = word_len
      local num_added_nodes = 0

      while len >= 4 do
        local cur = start
        local skip = 0

        if not (cur.id == GLYPH
                and umatch(uchar(cur.char), "[%a%d]")) then
          goto no_match
        end

        cur = cur.next
        if cur.id ~= GLYPH then
          goto no_match
        end

        if cur.char == char_underscore
            and cur.next.id == GLYPH
              and cur.next.char == char_underscore
            and len >= 5 then
          cur = cur.next
          skip = 1
        elseif cur.char == char_hyphen
            and cur.next.id == PENALTY
            and len >= 6 then
          cur = cur.next.next
          skip = 2
        end

        if not (cur.id == GLYPH and cur.char == char_underscore) then
          goto no_match
        end

        cur = cur.next

        if cur.id == GLYPH and umatch(uchar(cur.char), "[%a%d]")
            and cur.next.id == GLYPH then
          local pen = node.new(PENALTY)
          pen.penalty = tex.hyphenpenalty
          node.set_attribute(pen, code_attribute, 1)
          node.insert_before(head, cur, pen)

          num_added_nodes = num_added_nodes + 1

          len = len - 2 - skip
          start = cur
          goto continue
        end

        ::no_match::
        len = len - 1
        start = start.next

        ::continue::
      end -- end of while-loop

      word_len = word_len + num_added_nodes
    end
  end -- end of for-loop

  -- Item (2)
  --
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
