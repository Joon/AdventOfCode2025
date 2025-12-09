program AOC2025Day6;
uses
 Classes, SysUtils;

type
    stringArr = array of string;
    intIntArr = array of array of longInt;

function stringSlots(fileLine: string): stringArr;
var i: Integer;
    slotCount: Integer;
    inSlot: Boolean;
    currentSlot: String;
    allSlots: stringArr;
begin
    i := 1;
    slotCount := 0;
    inslot := False;
    while i <= Length(fileLine) do
    begin
        if (Ord(fileLine[i]) < 32) or (Ord(fileLine[i]) > 64) then
        begin
            i := i + 1;
            continue;
        end;
        if (fileLine[i] <> ' ') and not inSlot then
        begin
            inSlot := True;
            slotCount := slotCount + 1;
            currentSlot := fileLine[i];
        end else if (fileLine[i] <> ' ') then
        begin
          currentSlot := currentSlot + fileLine[i];
        end;

        if (fileLine[i] = ' ') and inSlot then
        begin
            inSlot := False;
            SetLength(allSlots, slotCount);
            allSlots[slotCount - 1] := currentSlot;
        end;
        i := i + 1;
    end;
    if inSlot then
    begin
        SetLength(allSlots, slotCount);
        allSlots[slotCount - 1] := currentSlot;
    end;

    stringSlots := allSlots;
end;

procedure Part1Processing(testInput: Boolean);
var F: TextFile;
    line: string;
    lines: array of string;
    inputsArr: array of array of longInt;
    inputArr: array of longInt;
    operatorArr: array of char;
    operatorLine: string;
    currentIndex: integer;
    secondIndex: integer;
    slots: array of string;
    tempResult: Int64;
    totalResult: Int64;
    loadFileName: string;
begin
    if (testInput) then
    begin
        loadFileName := 'inputs\\day6_test.txt';
    end else
    begin
        loadFileName := 'inputs\\day6.txt';
    end;

    Assign(F, loadFileName);
    Reset(F);
    currentIndex := 1;
    while not Eof(F) do
    begin
        setLength(lines, currentIndex);
        ReadLn(F, line);
        lines[currentIndex - 1] := line;
        currentIndex := currentIndex + 1;
    end;

    for currentIndex := 0 to Length(lines) - 2 do
    begin
        slots := stringSlots(lines[currentIndex]);
        setLength(inputsArr, currentIndex + 1);
        setLength(inputsArr[currentIndex], Length(slots));
        for secondIndex := 0 to Length(slots) -1 do
        begin
            inputsArr[currentIndex][secondIndex] := StrToInt64(slots[secondIndex]);
        end;
    end;

    operatorLine := lines[Length(lines) - 1];
    WriteLn(operatorLine);
    slots := stringSlots(operatorLine);
    totalResult := 0;
    for currentIndex := 0 to Length(inputsArr[0]) - 1 do
    begin
        tempResult := inputsArr[0][currentIndex];
        WriteLn('Starting with ' + IntToStr(inputsArr[0][currentIndex]));
        for secondIndex := 1 to Length(inputsArr) - 1 do
        begin
            WriteLn('Will now ' + slots[currentIndex] + ' the value of ' + IntToStr(inputsArr[secondIndex][currentIndex]));
            case slots[currentIndex] of
                '+' : tempResult := tempResult + inputsArr[secondIndex][currentIndex];
                '*' : tempResult := tempResult * inputsArr[secondIndex][currentIndex];
            otherwise
               WriteLn('ERRORROORERRORERRORERRORERROR');
            end;
        end;
        totalResult := totalResult + tempResult;
    end;
    WriteLn('End result: ' + IntToStr(totalResult));
end;

function ParsePart2Numbers(lines: array of string): intIntArr;
var
    resultArr: intIntArr;
    resultSize: Integer;
    counter: Integer;
    counter2: Integer;
    tempConstruct: String;
    tempArr: array of longInt;
    inSegment: Boolean;
    segmentSize: Integer;
begin
    inSegment := False;
    resultSize := 0;
    SetLength(tempArr, 0);
    segmentSize := 0;
    for counter := 1 to Length(lines[0]) do
    begin
        if (lines[0][counter] = ' ') and (lines[1][counter] = ' ') and
           (lines[2][counter] = ' ') and (lines[3][counter] = ' ') then
        begin
            if inSegment then
            begin
                //WriteLn('saving a segment yo');
                //for counter2 := 0 to Length(tempArr) - 1 do
                //begin
                //    Write(tempArr[counter2]);
                //    Write(' ');
                //    WriteLn();
                //end;
                inSegment := False;
                resultSize := resultSize + 1;
                SetLength(resultArr, resultSize);
                resultArr[resultSize - 1] := tempArr;
                segmentSize := 0;
            end;
        end else
        begin
            inSegment := True;
            tempConstruct := '';
            for counter2 := 0 to 3 do
            begin
                if lines[counter2][counter] <> ' ' then
                begin
                    tempConstruct := tempConstruct + lines[counter2][counter];
                end;
            end;
            segmentSize := segmentSize + 1;
            SetLength(tempArr, segmentSize);
            tempArr[segmentSize - 1] := StrToInt(tempConstruct);
        end;
    end;
    if inSegment then
    begin
        resultSize := resultSize + 1;
        SetLength(resultArr, resultSize);
        resultArr[resultSize - 1] := tempArr;
    end;
    ParsePart2Numbers := resultArr;
end;

procedure Part2Processing(testInput: Boolean);
var F: TextFile;
    line: string;
    lines: array of string;
    inputsArr: array of array of longInt;
    inputArr: array of longInt;
    operatorArr: array of char;
    operatorLine: string;
    currentIndex: integer;
    secondIndex: integer;
    tempResult: Int64;
    totalResult: Int64;
    loadFileName: string;
    operators: array of string;
begin
    if (testInput) then
    begin
        loadFileName := 'inputs\\day6_test.txt';
    end else
    begin
        loadFileName := 'inputs\\day6.txt';
    end;

    Assign(F, loadFileName);
    Reset(F);
    currentIndex := 1;
    while not Eof(F) do
    begin
        setLength(lines, currentIndex);
        ReadLn(F, line);
        lines[currentIndex - 1] := line;
        currentIndex := currentIndex + 1;
    end;

    inputsArr := ParsePart2Numbers(lines);
    operators := stringSlots(operatorLine);
    for currentIndex := 0 to Length(inputsArr) - 1 do
    begin
        WriteLn('------');
        //WriteLn(Length(inputsArr));
        //WriteLn(Length(inputsArr[currentIndex]));
        for secondIndex := 0 to Length(inputsArr[currentIndex]) - 1 do
        begin
            WriteLn(inputsArr[currentIndex][secondIndex]);
        end;
        WriteLn('yoop');
        WriteLn(operators[currentIndex]);
    end;

end;

begin
   //Part1Processing(False);
   Part2Processing(False);
   Readln();
end.
