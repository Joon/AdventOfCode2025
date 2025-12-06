$processFile = "inputs/day5.txt"
$rangeLineRegEx = "\d+-\d+"
$ingredientLineRegEx = "^\d+$"

$ranges = (Get-Content $processFile | Where-Object {$_ -match $rangeLineRegEx})
$ingredients = (Get-Content $processFile | Where-Object {$_ -match $ingredientLineRegEx})

echo "PART 1 PROCESSING" 
$goodIngredientCount = 0

foreach ($ingredient in $ingredients) {
    foreach($range in $ranges) {
        [int64]$lower,[int64]$higher = $range.split('-')
        if ([int64]$ingredient -ge $lower -and [int64]$ingredient -le $higher) {
            $goodIngredientCount++
            break
        }
    }  
}

echo "Part1: Good ingredient count " 
echo $goodIngredientCount

echo "Part 2 Processing"
$rangesToProcess = $ranges
$moreWorkToDo = $True

while ($moreWorkToDo) {
    echo "=============================="
    #echo "Processing ranges: $rangesToProcess"
    $dedupedRanges = @() 
    $processedRanges = @()
    $moreWorkToDo = $False
    foreach($range in $rangesToProcess) {
        echo "Processing range $range"
        if ($range -in $processedRanges) {
            echo "Range $range already processed through expansion. Skipping"
            continue
        }
        $processedRanges = $processedRanges + @($range)

        [int64]$low, [int64]$high = $range.split('-')
        foreach($checkRange in $rangesToProcess) {
            if ($range -eq $checkRange) {
                continue
            }
            if ($checkRange -in $processedRanges) {
                continue
            }

            [int64]$checkLow, [int64]$checkHigh = $checkRange.split('-')

            if ($low -ge $checkLow -and $low -le $checkHigh) {
                echo "$range overlaps with $checkRange. Expanding low to $checkLow"
                echo "Adding $checkRange to processedRanges"
                $processedRanges = $processedRanges + @($checkRange)
                $moreWorkToDo = $low -ne $checkLow
                $low = $checkLow
            }    
            if ($high -ge $checkLow -and $high -le $checkHigh) {
                echo "$range overlaps with $checkRange. Expanding high to $checkHigh"
                echo "Adding $checkRange to processedRanges"
                $processedRanges = $processedRanges + @($checkRange)
                $moreWorkToDo = $high -ne $checkHigh
                $high = $checkHigh
            }          
        }
        $newRange="$low-$high"
        $contained = $False
        foreach($checkRange in $rangesToProcess) {
            [int64]$checkLow, [int64]$checkHigh = $checkRange.split('-')
            if ($low -gt $checkLow -and $high -lt $checkHigh) {
                $contained = $True
                break
            }
        }
        if (-not $contained) {
            $dedupedRanges = $dedupedRanges + @($newRange)
        }
    }
    $rangesToProcess = $dedupedRanges
}
echo $dedupedRanges

[int64]$total = 0 
foreach ($processRange in $dedupedRanges) {
    [int64]$checkLow, [int64]$checkHigh = $processRange.split('-')
    $total += ($checkHigh - $checkLow) + 1
}
echo "Part2 Answer: $total"