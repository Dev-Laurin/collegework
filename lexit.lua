-- lexer.lua
-- Glenn G. Chappell
-- 13 Feb 2015
--
-- For CS 331 Spring 2015
-- Lexer Module

-- Usage:
--
--    program = "return a+b;"  -- program to lex
--    for lexstr, cat in lexer.lex(program) do
--        -- lexstr is the string form of a lexeme
--        -- cat is the kind of lexeme
--           This can be used as an index for array lexer.catnames
--    end

local lexit = {}  -- Our module


-- Lexeme Category Names

lexit.catnames = {
    "Identifier",
    "Keyword",
    "Operator",
    "NumericLiteral",
    "Punctuation",
    [99]="ILLEGAL"
}


-- Kind-of-Character Functions

-- isLetter
-- Returns true if c is a letter character, false otherwise.
local function isLetter(c)
    if c:len() ~= 1 then
        return false
    elseif c >= "A" and c <= "Z" then
        return true
    elseif c >= "a" and c <= "z" then
        return true
    else
        return false
    end
end


-- isDigit
-- Returns true if c is a digit character, false otherwise.
local function isDigit(c)
    if c:len() ~= 1 then
        return false
    elseif c >= "0" and c <= "9" then
        return true
    else
        return false
    end
end


-- isSpace
-- Returns true if c is a space character, false otherwise.
local function isSpace(c)
    if c:len() ~= 1 then
        return false
    elseif c == " " or c == "\t" or c == "\n" or c == "\r"
      or c == "\f" then
        return true
    else
        return false
    end
end


-- The Lexer Itself

-- lex
-- Our lexer
-- Intended for use in a for-in loop:
--     for lexstr, cat in lexer.lex(prog) do
function lexit.lex(prog)
    -- ***** Variables (like class data members) *****

    local pos       -- Index of next character in prog
    local state     -- Current state for our state machine
    local ch        -- Current character
    local lexstr    -- The lexeme, so far
    local category  -- Category of lexeme, set when state set to DONE
    local handlers  -- Dispatch table; value created later

    -- ***** Lexeme Categories *****

    local ID = 1
    local KEY = 2
    local OP = 3
    local NUMLIT = 4
    local PUNCT = 5
    local ILLEGAL = 99

    -- ***** States *****

    local START = 1
    local LETTER = 2
    local DIGIT = 3
    local DIGDOT = 4
    local PLUS = 5
    local MINUS = 6
    local DOT = 7
    local DONE = 0

    -- ***** Character-Related Functions *****

    -- currChar
    -- Return the current character, at index pos in prog. Return value
    -- is a single-character string, or the empty string if pos is past
    -- the end.
    local function currChar()
        return prog:sub(pos, pos)
    end

    -- nextChar
    -- Return the next character, at index pos+1 in prog. Return value
    -- is a single-character string, or the empty string if pos+1 is
    -- past the end.
    local function nextChar()
        return prog:sub(pos+1, pos+1)
    end

    -- drop1
    -- Move pos to the next character.
    local function drop1()
        pos = pos+1
    end

    -- add1
    -- Add the current character to the lexeme, moving pos to the next
    -- character.
    local function add1()
        lexstr = lexstr .. currChar()
        drop1()
    end

    -- skipSpace
    -- Skip whitespace and comments, moving pos to the beginning of
    -- the next lexeme, or to prog:len()+1.
    local function skipSpace()
        while true do
            while isSpace(currChar()) do
                drop1()
            end
            if currChar() ~= "#" then  -- Comment?
                break
            end
            drop1()  -- Skip the "#"
            while true do
                if currChar() == "\n" then
                    drop1()
                    break
                elseif
                    currChar() == "" then  -- End of input
                    return
                else
                    drop1()
                end
            end
        end
    end

    -- ***** State-Handler Functions *****

    local function handle_START()
        if isLetter(ch) then
            add1()
            state = LETTER
        elseif ch == "_" then
            add1()
            state = LETTER
        elseif isDigit(ch) then
            add1()
            state = DIGIT
        elseif ch == "+" then
            add1()
            state = PLUS
        elseif ch == "-" then
            add1()
            state = MINUS
        elseif ch == "." then
            add1()
            state = DOT
        elseif ch == "=" then
            add1()
            state = DONE
            category = OP
        elseif ch < " " or ch > "~" then
            add1()
            state = DONE
            category = ILLEGAL
        else
            add1()
            state = DONE
            category = PUNCT
        end
    end

    local function handle_LETTER()
        if isLetter(ch) then
            add1()
        elseif ch == "_" then
            add1()
        elseif isDigit(ch) then
            add1()
        else
            state = DONE
            category = ID
            if lexstr == "func" or lexstr == "return" 
                or lexstr == "set" then
                category = KEY
            end
        end
    end

    local function handle_DIGIT()
        if isDigit(ch) then
            add1()
        elseif ch == "." then
            add1()
            state = DIGDOT
        else
            state = DONE
            category = NUMLIT
        end
    end

    local function handle_DIGDOT()
        if isDigit(ch) then
            add1()
        else
            state = DONE
            category = NUMLIT
        end
    end

    local function handle_PLUS()
        if ch == "+" then
            add1()
            state = DONE
            category = OP
        elseif ch == "=" then
            add1()
            state = DONE
            category = OP
        elseif isDigit(ch) then
            add1()
            state = DIGIT
        elseif ch == '.' then
            if isDigit(nextChar()) then
                add1()
                state = DIGDOT
            else
                state = DONE
                category = OP
            end
        else
            state = DONE
            category = OP
        end
    end

    local function handle_MINUS()
        if ch == "-" then
            add1()
            state = DONE
            category = OP
        elseif ch == "=" then
            add1()
            state = DONE
            category = OP
        elseif isDigit(ch) then
            add1()
            state = DIGIT
        elseif ch == '.' then
            if isDigit(nextChar()) then
                add1()
                state = DIGDOT
            else
                state = DONE
                category = OP
            end
        else
            state = DONE
            category = OP
        end
    end

    local function handle_DOT()
        if isDigit(ch) then
            add1()
            state = DIGDOT
        else
            state = DONE
            category = OP
        end
    end

    local function handle_DONE()
        print("ERROR: 'DONE' state should not be handled")
        assert(0)
    end

    -- ***** Table of State-Handler Functions *****

    handlers = {
        [START]=handle_START,
        [LETTER]=handle_LETTER,
        [DIGIT]=handle_DIGIT,
        [DIGDOT]=handle_DIGDOT,
        [PLUS]=handle_PLUS,
        [MINUS]=handle_MINUS,
        [DOT]=handle_DOT,
        [DONE]=handle_DONE
    }

    -- ***** Iterator Function *****

    -- getLexeme
    -- Called each time through the for-in loop.
    -- Returns a pair: lexeme-string (string) and category (int), or
    -- nil, nil if no more lexemes.
    local function getLexeme(d1, d2)
        if pos > prog:len() then
            return nil, nil
        end
        lexstr = ""
        state = START
        while state ~= DONE do
            ch = currChar()
            handlers[state]()
        end

        skipSpace()
        return lexstr, category
    end

    -- ***** Body of Function lex *****

    -- Initialize & return the iterator function
    pos = 1
    skipSpace()
    return getLexeme, nil, nil
end


-- Module Export

return lexit

