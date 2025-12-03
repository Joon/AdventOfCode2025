  -- see if the file exists
  function file_exists(file)
    local f = io.open(file, "rb")
    if f then f:close() end
    return f ~= nil
  end
  
  -- get all lines from a file, returns an empty 
  -- list/table if the file does not exist
  function lines_from(file)
    if not file_exists(file) then return {} end
    local lines = {}
    for line in io.lines(file) do 
      lines[#lines + 1] = line
    end
    return lines
  end
  
  local file = 'inputs/day2.txt'
  local lines = lines_from(file)
  
  total_part1 = 0
  total_part2 = 0
  
  -- print all line numbers and their contents
  for k,v in string.gmatch(lines[1], "(%w+)-(%w+)") do
    -- print(' ---------- ')
    for i = tonumber(k),tonumber(v) do
      strVal = tostring(i)
      -- print('strVal: ' .. strVal)
      for l =1,strVal:len() - 1 do
        checkChar = strVal:sub(1, l)

        matchPattern_part1 = checkChar .. checkChar
        if string.match(strVal, '^' .. matchPattern_part1 .. '$') then
            total_part1 = total_part1 + i
            print('MATCH Part 1. Current total: ' .. total_part1 )
            break            
        end
      end
    end
  end

  -- print all line numbers and their contents
  for k,v in string.gmatch(lines[1], "(%w+)-(%w+)") do
    -- print(' ---------- ')
    for i = tonumber(k),tonumber(v) do
      strVal = tostring(i)
      -- print('strVal: ' .. strVal)
      for l =1,strVal:len() - 1 do
        checkChar = strVal:sub(1, l)
        matchPattern = ''
        while matchPattern:len() < strVal:len() do
            matchPattern = matchPattern .. checkChar
        end
        if string.match(strVal, '^' .. matchPattern .. '$') then
            total_part2 = total_part2 + i
            print('MATCH part 2. Current total: ' .. total_part2 )
            break            
        end
      end
    end
  end

  
  print('Part 1: ' .. total_part1)
  print('Part 2: ' .. total_part2)
