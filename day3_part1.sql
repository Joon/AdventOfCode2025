WITH zero_to_nine AS (SELECT 0 as seq UNION SELECT 1 UNION SELECT 2 UNION SELECT 3 UNION SELECT 4 UNION SELECT 5 UNION SELECT 6 UNION SELECT 7 UNION SELECT 8 UNION SELECT 9), 
      one_to_ninety_nine AS (SELECT ones.seq + (tens.seq * 10) + (hundreds.seq * 100) AS num 
                              FROM zero_to_nine as ones 
                              CROSS JOIN zero_to_nine as tens 
                              CROSS JOIN zero_to_nine as hundreds 
                              WHERE  ones.seq + (tens.seq * 10) + (hundreds.seq * 100) > 0
                              AND    ones.seq + (tens.seq * 10) + (hundreds.seq * 100) < 110
                              ORDER BY num), 
      bank_batteries AS (SELECT bat.ROWID as bank_id, counter.num AS battery_seq, 
                                substr(banks, counter.num, 1) AS battery, bat.banks 
                         FROM batteries bat 
                         INNER JOIN one_to_ninety_nine counter 
                         ON counter.num <= length(bat.banks)), 
      bank_batteries_combined AS (SELECT bb1.bank_id, bb1.battery battery1, bb2.battery battery2 
                                  FROM bank_batteries bb1 
                                  CROSS JOIN bank_batteries bb2 
                                  ON bb2.bank_id = bb1.bank_id 
                                  AND bb2.battery_seq > bb1.battery_seq), 
      largest_jolt_value AS (SELECT bank_id, MAX(battery1 || battery2) jolts 
                             FROM   bank_batteries_combined 
                             GROUP BY bank_id) 
 SELECT SUM(jolts) 
 FROM largest_jolt_value